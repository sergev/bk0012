library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_ARITH.all;
--library altera_mf;
--use altera_mf.altera_mf_components.all;
use work.def.all;
use work.iface.all;

entity hw_debugger is
  generic
  (
	  INSTRUCTION_BREAKPOINT_NUM: natural range 1 to 8;
		DATA_BREAKPOINT_NUM: natural range 1 to 6;
		TRACE_BUFFER_SIZE: natural
  );
  port
  (
	  clk : in std_logic;
		reset : in std_logic;

		-- Avalon bus slave
		avs_dbg_address: in std_logic_vector(8 downto 0):= (others => '0');
		avs_dbg_writedata: in std_logic_vector(15 downto 0):= (others => '0');
		avs_dbg_chipselect: in std_logic:= '0';
		avs_dbg_read: in std_logic:= '0';
		avs_dbg_write: in std_logic:= '0';
		avs_dbg_readdata: out std_logic_vector(WORD_SIZE-1 downto 0);

		din: in hw_debugger_in_type;
		dout: out hw_debugger_out_type;

		reset_out: out std_logic;
		int_reset_out: out std_logic
  );
end hw_debugger;

architecture behaviour of hw_debugger is
  constant CMD_RESET: std_logic_vector(3 downto 0):= conv_std_logic_vector(1, 4);
  constant CMD_RUN:   std_logic_vector(3 downto 0):= conv_std_logic_vector(2, 4);
	constant CMD_HALT:  std_logic_vector(3 downto 0):= conv_std_logic_vector(3, 4);
	constant CMD_STEP:  std_logic_vector(3 downto 0):= conv_std_logic_vector(4, 4);

  type hw_insn_breakpoint_control is
	record
		enable: std_logic;
	end record;

  type hw_insn_breakpoint_info is
	record
		address : std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 1);
		address_mask: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 1);
		control: hw_insn_breakpoint_control;
	end record;

	type hw_data_breakpoint_control is
	record
		load: std_logic;
		store: std_logic;
		byte_en: std_logic_vector(1 downto 0);
	end record;

	type hw_data_breakpoint_info is
	record
		address : std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		address_mask: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		control: hw_data_breakpoint_control;
	end record;

	type hw_breakpoint_array is array (0 to INSTRUCTION_BREAKPOINT_NUM-1) of hw_insn_breakpoint_info;
	signal hw_breakpoints: hw_breakpoint_array;

	type hw_data_breakpoint_array is array (0 to DATA_BREAKPOINT_NUM-1) of hw_data_breakpoint_info;
	signal hw_data_breakpoints: hw_data_breakpoint_array;

	signal prev_pc: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 1);
	signal pc_match: std_logic_vector(INSTRUCTION_BREAKPOINT_NUM-1 downto 0);
	signal instr_hit: std_logic;
	signal data_match: std_logic_vector(DATA_BREAKPOINT_NUM-1 downto 0);
	signal data_hit: std_logic;
	signal instr_halt_disable: std_logic;
	signal halt_reg: std_logic;
	signal reset_cnt: std_logic_vector(9 downto 0):= (others => '0'); -- conv_std_logic_vector(1020, 10);
	signal int_reset: std_logic;
	signal reset_out_reg: std_logic;
	signal step_mode: std_logic;
	signal step_cnt: std_logic_vector(7 downto 0);

	signal trace_mask: std_logic_vector(15 downto 0);
	alias  MASK_STATE: std_logic is trace_mask(6);
	alias  MASK_STORE: std_logic is trace_mask(5);
	alias  MASK_PC: std_logic is trace_mask(4);
	alias  MASK_RTI: std_logic is trace_mask(3);
	alias  MASK_TRAP: std_logic is trace_mask(2);
	alias  MASK_RTS: std_logic is trace_mask(1);
	alias  MASK_JSR: std_logic is trace_mask(0);
	signal trace_data: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
	signal trace_q: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
	signal trace_size: std_logic_vector(log2(TRACE_BUFFER_SIZE)-1 downto 0) := (others => '0');
	signal trace_empty: std_logic;
	signal trace_full: std_logic;
	signal trace_wr: std_logic;
	signal trace_rd: std_logic;
begin
--  trace_buffer : dcfifo
--	generic map
--	(
--		intended_device_family => DEVICE_FAMILY,
--		lpm_width => PHYSICAL_ADDRESS_WIDTH,
--		lpm_numwords => TRACE_BUFFER_SIZE,
--		lpm_widthu => log2(TRACE_BUFFER_SIZE),
--		lpm_showahead => "ON",
--		clocks_are_synchronized => "TRUE",
--		lpm_type => "dcfifo",
--		overflow_checking => "OFF",
--		underflow_checking => "ON",
----		rdsync_delaypipe => 3,
----		wrsync_delaypipe => 3,
--		use_eab => "ON",
--		add_ram_output_register => "OFF"
--	)
--	port map
--	(
--		wrclk => clk,
--		rdreq => trace_rd,
--		aclr => reset,
--		rdclk => clk,
--		wrreq => trace_wr,
--		data => trace_data,
--		rdempty => trace_empty,
--		wrusedw => trace_size,
--		wrfull => trace_full,
--		q => trace_q
--	);

	trace_data <= din.pc & '0';
	trace_wr <= '1' when halt_reg = '0' and din.pc_valid = '1'
    else '0';

	trace_read_proc: process (reset, clk, trace_size,
	     avs_dbg_chipselect, avs_dbg_read, avs_dbg_address)
	  variable vrd_del: std_logic;
		variable vrd: std_logic;
	begin
		if avs_dbg_chipselect = '1' and avs_dbg_read = '1' and avs_dbg_address = "00000101" then
			vrd := '1';
		else
			vrd := '0';
		end if;
		if reset = '1' then
			vrd_del := '0';
		elsif rising_edge(clk) then
			vrd_del := vrd;
		end if;
		if (vrd_del = '1' and vrd = '0') or (trace_size > TRACE_BUFFER_SIZE- 3) then
		  trace_rd <= '1';
		else
			trace_rd <= '0';
		end if;
	end process trace_read_proc;

	pc_match_proc: process (din, hw_breakpoints)
	  variable vmatch: std_logic_vector(INSTRUCTION_BREAKPOINT_NUM-1 downto 0);
	  variable vhit: std_logic;
	begin
		vhit := '0';
		vmatch := (others => '0');
		if din.pc_valid = '1' then
	    for i in 0 to INSTRUCTION_BREAKPOINT_NUM-1 loop
				if hw_breakpoints(i).control.enable = '1' then
					if (hw_breakpoints(i).address and hw_breakpoints(i).address_mask) =
						 (din.pc and hw_breakpoints(i).address_mask)
					then
					  vmatch(i) := '1';
						vhit := '1';
					end if;
				end if;
	    end loop;
		end if;
		pc_match <= vmatch;
		instr_hit <= vhit;
	end process pc_match_proc;

	data_match_proc: process (din, hw_data_breakpoints)
	  variable vmatch: std_logic_vector(DATA_BREAKPOINT_NUM-1 downto 0);
	  variable vhit: std_logic;
	begin
		vhit := '0';
		vmatch := (others => '0');
		for i in 0 to DATA_BREAKPOINT_NUM-1 loop
			if (din.byte_en and hw_data_breakpoints(i).control.byte_en) /= 0 then
				if (((din.load and hw_data_breakpoints(i).control.load) = '1') and
					   (hw_data_breakpoints(i).address and hw_data_breakpoints(i).address_mask) =
					   (din.mem_rd_address and hw_data_breakpoints(i).address_mask)) or
					 (((din.store and hw_data_breakpoints(i).control.store) = '1') and
					   (hw_data_breakpoints(i).address and hw_data_breakpoints(i).address_mask) =
					   (din.mem_wr_address and hw_data_breakpoints(i).address_mask))
				then
					  vmatch(i) := '1';
						vhit := '1';
				end if;
			end if;
		end loop;
		data_match <= vmatch;
		data_hit <= vhit;
	end process data_match_proc;

	-- internal reset generator. Must be standalone because external reset
	--   dependent on reset_out output from HW debugger
	int_reset_proc: process(clk, reset_cnt)
	begin
		if rising_edge(clk) then
			if reset_cnt /= 1023 then
				reset_cnt <= reset_cnt + 1;
		  end if;
		end if;
		if reset_cnt = 1023 then
			int_reset <= '0';
		else
			int_reset <= '1';
		end if;
	end process;

  external_bus: process (din, int_reset, clk, instr_hit, halt_reg, instr_halt_disable,
	    pc_match, trace_size, trace_full, trace_q, data_match,
	    hw_breakpoints, avs_dbg_chipselect, avs_dbg_read, avs_dbg_address)
	  variable vdata: std_logic_vector(15 downto 0);
		variable vinstr_halt_disable: std_logic;
		variable vreset_out: std_logic;
		variable vhalt_reg: std_logic;
		variable vstep_mode: std_logic;
		variable num: natural range 0 to 7;
	begin
		if int_reset = '1' then
			for i in 0 to INSTRUCTION_BREAKPOINT_NUM-1 loop
				hw_breakpoints(i).control.enable <= '0';
				hw_breakpoints(i).address <= (others => '0');
				hw_breakpoints(i).address_mask <= (others => '1');
			end loop;
			for i in 0 to DATA_BREAKPOINT_NUM-1 loop
				hw_data_breakpoints(i).control.load <= '0';
				hw_data_breakpoints(i).control.store <= '0';
				hw_data_breakpoints(i).control.byte_en <= "00";
				hw_data_breakpoints(i).address <= (others => '0');
				hw_data_breakpoints(i).address_mask <= conv_std_logic_vector(-2, PHYSICAL_ADDRESS_WIDTH);
			end loop;
			instr_halt_disable <= '0';
			reset_out_reg <= '0';
			halt_reg <= '0';
			step_mode <= '0';
			step_cnt <= (others => '0');
			trace_mask <= conv_std_logic_vector(64, 16);
			prev_pc <= (others => '1');
		elsif rising_edge(clk) then
			if din.pc_valid = '1' then
			  prev_pc <= din.pc;
			end if;
	    vinstr_halt_disable := '0';
		  vreset_out := '0';
			vstep_mode := step_mode;
		  vhalt_reg := (halt_reg or instr_hit or data_hit or (step_mode and din.pc_valid)) and not instr_halt_disable;

--			if step_cnt = x"0F" then
--				vinstr_halt_disable := '1';
--				vhalt_reg := '0';
--				step_cnt <= (others => '0');
--			else
--				step_cnt <= step_cnt + 1;
--			end if;

			if avs_dbg_chipselect = '1' and avs_dbg_write = '1' then
				case avs_dbg_address(8 downto 6) is
					when "000" =>  -- control registers
					  case avs_dbg_address(5 downto 0) is
							when "000000" =>  -- INSN_COMMAND_REG
							  if avs_dbg_writedata(3 downto 0) = CMD_RUN then
							    vinstr_halt_disable := '1';
									vhalt_reg := '0';
									vstep_mode := '0';
								end if;
							  if avs_dbg_writedata(3 downto 0) = CMD_RESET then
									vreset_out := '1';
									vhalt_reg := '0';
								end if;
							  if avs_dbg_writedata(3 downto 0) = CMD_HALT then
									vhalt_reg := '1';
								end if;
							  if avs_dbg_writedata(3 downto 0) = CMD_STEP then
							    vinstr_halt_disable := '1';
									vhalt_reg := '0';
									vstep_mode := '1';
								end if;
							when "000001" =>  -- TRACE_MASK_REG
							  trace_mask <= avs_dbg_writedata;
							when others => null;
						end case;
					when "001" =>  -- processor registers
					  case avs_dbg_address(5 downto 4) is

							when others => null;
						end case;
					when "010" =>  -- instruction breakpoint registers
					  num := conv_integer(avs_dbg_address(5 downto 3));
						case avs_dbg_address(2 downto 0) is
							when "000" => -- low address
								hw_breakpoints(num).address(15 downto 1) <= avs_dbg_writedata(15 downto 1);
							when "001" => -- high address
--				      hw_breakpoints(num).address(PHYSICAL_ADDRESS_WIDTH-1 downto 16) <= avs_dbg_writedata(PHYSICAL_ADDRESS_WIDTH-17 downto 0);
							when "010" => -- low address mask
								hw_breakpoints(num).address_mask(15 downto 1) <= avs_dbg_writedata(15 downto 1);
							when "011" => -- high address mask
--						  hw_breakpoints(num).address_mask(PHYSICAL_ADDRESS_WIDTH-1 downto 16) <= avs_dbg_writedata(PHYSICAL_ADDRESS_WIDTH-17 downto 0);
							when "100" => -- control
								hw_breakpoints(num).control.enable <= avs_dbg_writedata(0);
							when others => null;
						end case;
					when "011" =>  -- data breakpoint registers
					  num := conv_integer(avs_dbg_address(5 downto 3));
						case avs_dbg_address(2 downto 0) is
							when "000" => -- low address
								hw_data_breakpoints(num).address <= avs_dbg_writedata;
							when "001" => -- high address
--				      hw_data_breakpoints(num).address(PHYSICAL_ADDRESS_WIDTH-1 downto 16) <= avs_dbg_writedata(PHYSICAL_ADDRESS_WIDTH-17 downto 0);
							when "010" => -- low address mask
								hw_data_breakpoints(num).address_mask <= avs_dbg_writedata;
							when "011" => -- high address mask
--						  hw_data_breakpoints(num).address_mask(PHYSICAL_ADDRESS_WIDTH-1 downto 16) <= avs_dbg_writedata(PHYSICAL_ADDRESS_WIDTH-17 downto 0);
							when "100" => -- control
							  hw_data_breakpoints(num).control.store <= avs_dbg_writedata(0);
								hw_data_breakpoints(num).control.load <= avs_dbg_writedata(1);
								hw_data_breakpoints(num).control.byte_en <= avs_dbg_writedata(3 downto 2);
							when others => null;
						end case;
					when others => null;
				end case;
			end if;
			instr_halt_disable <= vinstr_halt_disable;
			reset_out_reg <= vreset_out;
			halt_reg <= vhalt_reg;
			step_mode <= vstep_mode;
		end if;
		vdata := (others => '0');
		if avs_dbg_chipselect = '1' and avs_dbg_read = '1' then
			case avs_dbg_address(8 downto 6) is
				when "000" =>  -- control registers
				  case avs_dbg_address(5 downto 0) is
						when "000000" =>
							vdata := x"5555";
						when "000001" =>  -- INSN_STATUS_REG
						  vdata(15) := halt_reg;
							vdata(INSTRUCTION_BREAKPOINT_NUM-1 downto 0) := pc_match;
							vdata(8+DATA_BREAKPOINT_NUM-1 downto 8) := data_match;
						when "000010" =>  -- TRACE_STATUS_REG
						  vdata(log2(TRACE_BUFFER_SIZE)-1 downto 0) := trace_size;
							vdata(log2(TRACE_BUFFER_SIZE)) := trace_full;
						when "000100" =>  -- TRACE_LOW_WORD
						  vdata := trace_q;
						when "000101" =>  -- TRACE_HIGH_WORD
						  null;
						when others => null;
					end case;
				when "001" =>  -- processor registers
				  case avs_dbg_address(5 downto 4) is
						when "00" =>
						  vdata := din.reg_value;
						when "01" =>
						  case avs_dbg_address(3 downto 0) is
								when "0000" => vdata := din.pc(15 downto 1) & '0';
								when "0001" => vdata := din.pipe_state;
								when "0010" => vdata := din.mem_rd_address;
								when "0011" => vdata := din.insn;
								when "0100" => vdata := din.immed1;
						    when "0101" => vdata := din.immed2;
								when "0110" => vdata := din.mem_wr_address;
--								when "0111" => vdata(15 downto log2(DCACHE_SIZE/ DCACHE_ASSOCIATIVITY)) := dcache_state.tag_address_ar(0);
--								when "1000" => vdata(15 downto log2(DCACHE_SIZE/ DCACHE_ASSOCIATIVITY)) := dcache_state.tag_address_ar(1);
--								when "1001" => vdata := icache_state.data;
--								when "1010" => vdata := icache_state.next_data;
								when others => null;
							end case;
						when others => null;
					end case;
				when "010" =>  -- instruction breakpoint registers
				  num := conv_integer(avs_dbg_address(5 downto 3));
					case avs_dbg_address(2 downto 0) is
						when "000" => -- low address
						  vdata := hw_breakpoints(num).address(15 downto 1) & '0';
						when "001" => -- high address
						  vdata := x"ABDE";
--					  vdata(PHYSICAL_ADDRESS_WIDTH-17 downto 0) := hw_breakpoints(num).address(PHYSICAL_ADDRESS_WIDTH-1 downto 16);
						when "010" => -- low address mask
						  vdata := hw_breakpoints(num).address_mask(15 downto 1) & '0';
						when "011" => -- high address mask
						  vdata := x"EBDA";
--            vdata(PHYSICAL_ADDRESS_WIDTH-17 downto 0) := hw_breakpoints(num).address_mask(PHYSICAL_ADDRESS_WIDTH-1 downto 16);
            when "100" => -- control
							vdata(0) := hw_breakpoints(num).control.enable;
						when others => null;
					end case;
				when "011" =>  -- data breakpoint registers

				when others => null;
			end case;
		end if;
		avs_dbg_readdata <= vdata;
	end process external_bus;

	dout.reg_address <= avs_dbg_address(3 downto 0);
	dout.halt <= '1' when (instr_hit = '1' or (step_mode and din.pc_valid) = '1' or halt_reg = '1') and instr_halt_disable = '0' else '0';
	reset_out <= reset_out_reg;
	int_reset_out <= int_reset;
end behaviour;
