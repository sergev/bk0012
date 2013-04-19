library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_ARITH.all;  

entity ps2_contr is
  port
  (
		clk : in std_logic;
		reset : in std_logic;

    ps2_data: inout std_logic;
    ps2_clock: inout std_logic;

		-- Avalon bus interface
		address: in std_logic_vector(0 downto 0);
		writedata: in std_logic_vector(15 downto 0); 
		chipselect: in std_logic;
		read: in std_logic;
		write: in std_logic;
		readdata: out std_logic_vector(15 downto 0);		

    virq1: out std_logic;
		virq2: out std_logic;
		
		palette: out std_logic_vector(3 downto 0);
		screen_buf: out std_logic;
		timer_intr_en: out std_logic
  );
end ps2_contr;


architecture behaviour of ps2_contr is
	component code_convert is
		port
		(
		  shift: in std_logic; 
			lat_rus: in std_logic;
			e0: in std_logic;
			scan_code: in std_logic_vector(7 downto 0);
			
			out_code: out std_logic_vector(6 downto 0)
		);
	end component;

  type states is (idle, write_request, start, data, parity, stop);
  type debounce_states is (stable, rise, fall, wait_stable);

  constant DEBOUNCE_BITS: integer:= 6;
  constant WATCHDOG_BITS: integer:= 8;
  
  signal state             : states;
  signal debounce_state    : debounce_states;  
  signal debounce_cnt      : std_logic_vector(DEBOUNCE_BITS-1 downto 0);
  signal debounce_cao      : std_logic;
  signal ps2_clk_syn       : std_logic;  -- PS2 clock input syncronized
  signal ps2_clk_clean     : std_logic;  -- PS2 clock debounced and clean
  signal ps2_clk_fall      : std_logic;  -- PS2 clock fall edge
  signal ps2_clk_rise      : std_logic;  -- PS2 clock rise edge
  signal ps2_data_syn      : std_logic;  -- PS2 data  input syncronized
  signal ps2_clk_out       : std_logic;  -- PS2 clock output
  signal ps2_data_out      : std_logic;  -- PS2 clock output
  signal writing           : std_logic;  -- read / write cycle flag
  signal shift_cnt         : std_logic_vector(2 downto 0);
  signal shift_cao         : std_logic;  -- Shift counter carry out
  signal shift_reg         : std_logic_vector(8 downto 0);
  signal shift_in          : std_logic;  -- Shift register to right
  signal shift_load        : std_logic;  -- Shift register parallel load
  signal shift_calc_parity : std_logic;  -- Shift register set parity
  signal wdt_cnt           : std_logic_vector(WATCHDOG_BITS-1 downto 0);
  signal wdt_rst           : std_logic;  -- watchdog reset
  signal wdt_cao           : std_logic;  -- watchdog carry out
  signal shift_parity      : std_logic;  -- Current parity of shift_reg
  signal ibf               : std_logic;  -- IBF, In Buffer Full
  signal obf               : std_logic;  -- OBF, Out Buffer Full
  signal parity_err        : std_logic;  -- Parity error
  signal frame_err         : std_logic;  -- Frame error

  signal ibf_clr_i         : std_logic;  -- Ifb flag clear input
  signal obf_set_i         : std_logic;  -- Obf flag set input
  signal err_clr_i         : std_logic;  -- Clear error flags  

	signal ctrl, next_ctrl: std_logic;
	signal alt, next_alt: std_logic;
	signal shift, next_shift: std_logic;
	signal lat_rus, next_lat_rus: std_logic; 
	signal ar2, next_ar2: std_logic;
	signal e0: std_logic;
	signal conv_code: std_logic_vector(6 downto 0);
	signal ex_conv_code: std_logic_vector(6 downto 0);
	
	type control_state_type is (st_start, st_e0, st_break, st_break_e0);
	signal control_state, next_control_state: control_state_type;
	signal data_buffer: std_logic_vector(6 downto 0);
	signal data_ready: std_logic;
	signal data_rd: std_logic;
	signal data_wr: std_logic;
	
  constant STATE_REG_ADDRESS: std_logic_vector(0 downto 0) := "0";  -- (177660 - 177660)/ 2
  constant DATA_REG_ADDRESS:  std_logic_vector(0 downto 0) := "1";  -- (177662	- 177660)/ 2	
	-- bits of register 177660
	signal intr_en_reg: std_logic;
	-- bits of register 177662
	signal palette_reg: std_logic_vector(3 downto 0);
	signal timer_intr_en_reg: std_logic;
	signal screen_buf_reg: std_logic;
begin
	convert: code_convert
	port map
	(
	  shift => shift,
		lat_rus => lat_rus,
	  e0 => e0,
		scan_code => shift_reg(7 downto 0),
		out_code => conv_code
	);
	
  -- Sincronize input signals
  syn_ps2 : process (reset, clk)
  begin
    if reset = '1' then    
      ps2_clk_syn  <= '0';
      ps2_data_syn <= '0';
    elsif rising_edge(clk) then  
      ps2_clk_syn  <= TO_X01(ps2_clock);
      ps2_data_syn <= TO_X01(ps2_data);
    end if;
  end process syn_ps2;     
  
  -- clk debounce timer
  debounce_count : process (reset, clk)
  begin
    if reset = '1' then 
      debounce_cnt <= (others => '0');
    elsif rising_edge(clk) then 
      if (ps2_clk_fall or ps2_clk_rise or debounce_cao) = '1' then
        debounce_cnt <= (others => '0');
      else
        debounce_cnt <= debounce_cnt + 1;
      end if;
    end if;
  end process;
  debounce_cao <= debounce_cnt(DEBOUNCE_BITS-1);  

  -- PS2 clock debounce and edge detector
  debounce_stm : process (reset, clk)
  begin
    if reset = '1' then
      debounce_state <= stable;
      ps2_clk_clean  <= '0';
    elsif rising_edge(clk) then
      case debounce_state is
        when stable =>
          if ps2_clk_clean /= ps2_clk_syn then
            if ps2_clk_syn = '1' then
              debounce_state <= rise;
            else
              debounce_state <= fall;
            end if;
          end if;
        when wait_stable =>
          if debounce_cao = '1' then
            debounce_state <= stable;
          end if;
        when rise => 
          debounce_state <= wait_stable;
          ps2_clk_clean <= '1';
        when fall => 
          debounce_state <= wait_stable;
          ps2_clk_clean <= '0';
        when others => null;
      end case;
    end if;
  end process;
  ps2_clk_fall <= '1' when debounce_state = fall else '0';
  ps2_clk_rise <= '1' when debounce_state = rise else '0';  
    
  -- PS2 watchdog
  wdt_proc : process(reset, clk)
  begin
    if reset= '1' then   
      wdt_cnt <= (others => '0');
    elsif rising_edge(clk) then
      if (wdt_rst or wdt_cao) = '1' then
        wdt_cnt <= (others => '0');
      elsif debounce_cao = '1' then
        wdt_cnt <= wdt_cnt + 1;
      end if;
    end if;
  end process wdt_proc;
  wdt_cao <= wdt_cnt(WATCHDOG_BITS-1);
  wdt_rst <= ps2_clk_fall;   
  
  -- Shift register
  shift_reg_proc : process (reset, clk)
  begin
    if reset = '1' then
      shift_reg <= (others => '0');
    elsif rising_edge(clk) then  
      if shift_load = '1' then
        shift_reg(7 downto 0) <= writedata(7 downto 0);
        shift_reg(8) <= '0';
      elsif shift_calc_parity = '1' then
        shift_reg(8) <= not shift_parity;
      elsif shift_in = '1' then
        shift_reg(7 downto 0) <= shift_reg(8 downto 1);
        shift_reg(8) <= ps2_data_syn;
      end if;
    end if;
  end process shift_reg_proc;  
  
  -- Shift counter
  shift_cnt_proc : process(reset, clk)
  begin
    if reset = '1' then
      shift_cnt <= (others => '0');
    elsif rising_edge(clk) then 
      if state = start then
        shift_cnt <= (others => '0');
      elsif state = data and ps2_clk_fall = '1' then
        shift_cnt <= shift_cnt + 1;
      end if;
    end if;
  end process shift_cnt_proc;
  shift_cao <= '1' when shift_cnt = "111" else '0';  
    
  -- Odd Parity generator
  shift_parity <= (shift_reg(0) xor shift_reg(1) xor shift_reg(2) xor shift_reg(3) xor
    shift_reg(4) xor shift_reg(5) xor shift_reg(6) xor shift_reg(7));    
    
  -- Main State Machine
  stm : process (reset, clk)
  begin
    if reset = '1' then 
      state   <= idle;
      writing <= '0';
    elsif rising_edge(clk) then 
      case state is
        -- Waiting for clk
        when idle => 
          if obf_set_i = '1' and writing = '0' then
            state <= write_request;
            writing <= '1';
          elsif ps2_clk_fall = '1' then
            state <= start;
          end if;

        -- Write request, clk low
        when write_request => 
          if wdt_cao = '1' then
            state <= idle;
          end if;

        -- Clock 1, start bit
        when start => 
          if wdt_cao = '1' then
            state <= idle;
          elsif ps2_clk_fall = '1' then
            state <= data;
          end if;

        -- Clocks 2-9, Data bits (LSB first)
        when data => 
          if wdt_cao = '1' then
            state <= idle;
          elsif ps2_clk_fall = '1' and
            shift_cao = '1' then
            state <= parity;
          end if;

        -- Clock 10, Parity bit
        when parity => 
          if wdt_cao = '1' then
            state <= idle;
          elsif ps2_clk_fall = '1' then
            state <= stop;
          end if;

        -- Clock 11, Stop bit
        when stop  => 
          writing <= '0';
          state <= idle;
        when others => null;
      end case;
    end if;
  end process;

  -- State flags
  flags_proc : process (reset, clk, state, writing)
  begin  -- process stm_out
    -- Input Buffer write flag
    if reset = '1' then  
      --obf <= '0';
      ibf <= '0';
      parity_err <= '0';
      frame_err  <= '0';
    elsif rising_edge(clk) then
      -- Parity error flag
      if err_clr_i = '1' then
        parity_err <= '0';
      elsif writing = '0' and state = stop then
        if shift_reg(8) /= not shift_parity then
          parity_err <= '1';
        end if;
      end if;

      -- Frame error flag
      if err_clr_i = '1' then
        frame_err <= '0';
      elsif (state = start or
             state = data or state = parity) and wdt_cao = '1' then
        frame_err <= '1';
      end if;
      -- Input Buffer full flag
      if ibf_clr_i = '1' then
        ibf <= '0';
      elsif writing = '0' and state = stop then
        if shift_reg(8) = not shift_parity then
          ibf <= '1';
        end if;
      end if;

      -- Output buffer full flag
      --if state = stop and writing = '1' then
      --        obf <= '0';
      --elsif obf_set_i = '1' then
      --        obf <= '1';
      --end if;
    end if;
  end process;

  -- PS2 Registered outputs
  syn_ps2_out : process (reset, clk)
  begin
    if reset = '1' then
      ps2_data_out <= '1';
      ps2_clk_out  <= '1';
    elsif rising_edge(clk) then
      -- PS2 Data out
      if writing = '1' then
        if state = idle then
          ps2_data_out <= '0';
        elsif state = data or state = start then
          ps2_data_out <= shift_reg(0);
        else
          ps2_data_out <= '1';
        end if;
      end if;

      -- PS2 Clk out
      if state = write_request then
        ps2_clk_out <= '0';
      else
        ps2_clk_out <= '1';
      end if;
    end if;
  end process;  

	next_control_state_proc: process (control_state, ibf, shift_reg)
	  variable scan_code: std_logic_vector(7 downto 0);
		variable v_conv_code: std_logic_vector(6 downto 0);
		variable v_wr: std_logic;
		variable next_state: control_state_type;
		variable v_ctrl: std_logic;
		variable v_alt: std_logic;
		variable v_shift: std_logic;
		variable v_lat_rus: std_logic;
		variable v_ar2: std_logic;
	begin	 
		v_wr := '0';
		scan_code := shift_reg(7 downto 0);
		v_conv_code := conv_code;
		next_state := control_state;
		v_ctrl := ctrl;	
		v_alt := alt;
		v_shift := shift;
		v_lat_rus := lat_rus;
		v_ar2 := ar2;
		if ibf = '1' then					 
			case control_state is
				when st_start =>
				  case scan_code is
				    when x"E0" =>
							next_state := st_e0;	
					  when x"F0" =>
						  next_state := st_break;
						when x"14" =>          -- Left Ctrl
						  v_ctrl := '1';
						when x"11" =>          -- Left Alt
						  v_alt := '1';
						when x"12" | x"59" =>  -- Left & Right Shift
						  v_shift := '1';
						when others =>
						  v_wr := '1';  -- valid ASCII code , write it to data buffer
					end case;
				when st_e0 =>
				  next_state := st_start;
				  case scan_code is	
					  when x"14" =>        -- Right Ctrl
  						v_ctrl := '1';					
						when x"11" =>        -- Right Alt
						  v_alt := '1';					
						when x"1F" =>        -- Left Windows -> AR2
						  v_ar2 := '1';
						when x"F0" =>
						  next_state := st_break_e0;
						when others =>
				      v_wr := '1';       -- valid ASCII code , write it to data buffer
					end case;
				when st_break =>						
			   	case scan_code is							 
					  when x"14" =>          -- Left Ctrl
						  v_ctrl := '0';
						when x"11" =>          -- Left Alt
						  v_alt := '0';
						when x"12" | x"59" =>  -- Left & Right Shift
						  v_shift := '0';	
						when others => null;
					end case;
					next_state := st_start;
				when st_break_e0 =>
				  case scan_code is	 
					  when x"14" =>          -- Right Ctrl
						  v_ctrl := '0';
						when x"11" =>          -- Right Alt
						  v_alt := '0';
						when x"1F" =>          -- Windows -> AR2
						  v_ar2 := '0';					    
				    when others => null;
					end case;
					next_state := st_start;
				when others => null;
			end case;
		end if;	
		if ((v_shift and v_ctrl) = '1') and ((shift and ctrl) = '0') then
			if lat_rus = '0' then
				v_lat_rus := '1';
				v_conv_code := "0001110";  -- x"0E"
			else
				v_lat_rus := '0';
				v_conv_code := "0001111";  -- x"0F"
			end if;	 
			v_wr := '1';
		elsif ctrl = '1' then
			v_conv_code := "00" & conv_code(4 downto 0);
		end if;	
		
		data_wr <= v_wr;
		next_control_state <= next_state;
		next_ctrl <= v_ctrl;
		next_alt <= v_alt;
		next_shift <= v_shift;
		next_lat_rus <= v_lat_rus;
		next_ar2 <= v_ar2;
		ex_conv_code <= v_conv_code;
	end process next_control_state_proc;	
	
	control_state_proc: process (reset, clk)
	begin
		if reset = '1' then
			control_state <= st_start;
			ctrl <= '0';
			alt <= '0';
			shift <= '0';
			lat_rus <= '0';
			ar2 <= '0';
		elsif rising_edge(clk) then	
			control_state <= next_control_state;
			ctrl <= next_ctrl;
			alt <= next_alt;
			shift <= next_shift; 
			lat_rus <= next_lat_rus;
			ar2 <= next_ar2;
		end if;
	end process control_state_proc;	

	ibf_clr_i <= ibf;
	e0 <= '1' when control_state = st_e0 else '0';
		
	data_buffer_proc: process (reset, clk)
	begin
		if reset = '1' then
			data_buffer <= (others => '0');
			data_ready <= '0';
		elsif rising_edge(clk) then	
			if data_wr = '1' then
				data_buffer <= ex_conv_code;
				data_ready <= '1';
			elsif data_rd = '1' then
				data_ready <= '0';
			end if;	
		end if;	
	end process data_buffer_proc;	
		
	external_bus: process(reset, clk, chipselect, read, address)
	begin
		if reset = '1' then
			intr_en_reg <= '0';
			timer_intr_en_reg <= '1';
			screen_buf_reg <= '0';
			palette_reg <= "1111";
		elsif rising_edge(clk) then	
		  if chipselect= '1' and write = '1' then
			  if address = STATE_REG_ADDRESS then
					intr_en_reg <= writedata(6);
				end if;
			  if address = DATA_REG_ADDRESS then
					screen_buf_reg <= writedata(15); 
					palette_reg <= writedata(12 downto 9);
					timer_intr_en_reg <= writedata(14);
				end if;
			end if;			
		end if;
	  if chipselect= '1' and read = '1' and address = DATA_REG_ADDRESS then	
			readdata <= screen_buf_reg & timer_intr_en_reg & "0000000" & data_buffer;	
			data_rd <= '1';
		elsif chipselect= '1' and read = '1' and address = STATE_REG_ADDRESS then
			readdata <= "00000000" & data_ready & intr_en_reg & "000000";
			data_rd <= '0';
		else	
			readdata <= (others => '0');
			data_rd <= '0';
		end if;		
	end process external_bus;	
	
  obf <= writing;
  -- Shift register control
  shift_load <= '0'; -- '1' when obf_set_i = '1' else '0';
  shift_calc_parity <= '1' when state = idle and writing = '1' else '0';
  shift_in <= ps2_clk_fall when state = data or state = start else '0';    

  ps2_clock <= '0' when ps2_clk_out = '0'  else 'Z';
  ps2_data <= '0' when ps2_data_out = '0' else 'Z';    
  virq1 <= (not ar2) and (not intr_en_reg) and data_ready; 
	virq2 <= ar2 and (not intr_en_reg) and data_ready;
	
	palette <= palette_reg;
	timer_intr_en <= timer_intr_en_reg;
	screen_buf <= screen_buf_reg;
end behaviour;
