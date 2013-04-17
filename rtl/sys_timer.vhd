library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity sys_timer is
	port
	(
		reset: in std_logic;
		clk:   in std_logic;
		
		-- Avalon bus interface	 
		-- Register 177706	
		slv1_writedata: in std_logic_vector(15 downto 0); 
		slv1_chipselect: in std_logic;
		slv1_read: in std_logic;
		slv1_write: in std_logic;
		slv1_readdata: out std_logic_vector(15 downto 0);		
		
		-- Avalon bus interface
		-- Registers 177710 - 177712
		slv2_address: in std_logic_vector(0 downto 0);
		slv2_writedata: in std_logic_vector(15 downto 0); 
		slv2_chipselect: in std_logic;
		slv2_read: in std_logic;
		slv2_write: in std_logic;
		slv2_readdata: out std_logic_vector(15 downto 0);		
		
		aux_out: out std_logic;  -- for external frequency control
		
		irq_en: in std_logic;
    irq:  out std_logic		
	);
end sys_timer;

architecture behaviour of sys_timer is
  signal counter: std_logic_vector(15 downto 0);
	signal period: std_logic_vector(15 downto 0);
	
	-- Control register 177712 bits
	signal stop: std_logic;
	signal init_set: std_logic;
	signal count_end_enable: std_logic;
	signal single_shot: std_logic;
	signal count_enable: std_logic;
	signal divider4: std_logic;
	signal divider16: std_logic;
	signal count_end: std_logic;
	
	signal clk_prescaler: std_logic_vector(10 downto 0);
	signal tmr_prescaler: std_logic_vector(5 downto 0);
	signal tick: std_logic;	
	signal aux_reg: std_logic;
	
	signal counter_wr: std_logic;
	signal control_wr: std_logic;

	signal counter_50hz: std_logic_vector(21 downto 0);
	signal tick_50hz: std_logic;
begin											 
  prescaler_proc: process (reset, clk, divider4, divider16)
	  variable vtick: std_logic;
	begin
		if reset = '1' then
			clk_prescaler <= (others => '0');
			tmr_prescaler <= (others => '0');
		elsif rising_edge(clk) then	
			if clk_prescaler = (1600-1) then				
				tmr_prescaler <= tmr_prescaler + 1;
				clk_prescaler <= (others => '0');
			else	
				clk_prescaler <= clk_prescaler + 1;
			end if;	
		end if;
		vtick := '0';
		if clk_prescaler = (1600-1) then
		  if divider4 = '1' then
			  if divider16 = '1' then
					if tmr_prescaler = "111111" then
						vtick := '1';
					end if;
				else
					if tmr_prescaler(1 downto 0) = "11" then
						vtick := '1';
					end if;	
			  end if;	
		  elsif divider16 = '1' then
				if tmr_prescaler(3 downto 0) = "1111" then
					vtick := '1';
				end if;	
		  end if;	
		end if;
		tick <= vtick;
	end process prescaler_proc;	
	
	counter_proc: process (reset, clk)
	begin
	  if reset = '1' then
			counter <= (others => '0');			
			stop <= '0';
			init_set <= '0';
			count_end_enable <= '0';
			single_shot <= '0';
			count_enable <= '0';
			divider4 <= '0';
			divider16 <= '0';
			count_end <= '0';
			aux_reg <= '0';
		elsif rising_edge(clk) then	
			if control_wr = '1' then
				stop <= slv2_writedata(0);
				init_set <= slv2_writedata(1);
				count_end_enable <= slv2_writedata(2);
				single_shot <= slv2_writedata(3);
				count_enable <= slv2_writedata(4);
				divider4 <= slv2_writedata(5);
				divider16 <= slv2_writedata(6);
				count_end <= slv2_writedata(7);
			elsif counter_wr = '1' then
				counter <= slv2_writedata;
			elsif init_set = '1' then
				counter <= period;
			elsif stop = '0' and count_enable = '1' and tick = '1' then
				if counter = 0 then
					if period /= 0 then
						counter <= period;
					else
						counter <= counter - 1;
					end if;	
					count_enable <= not single_shot;	
					count_end <= count_end_enable;
					aux_reg <= not aux_reg;
				else	
					counter <= counter - 1;
				end if;	 
			end if;	
		end if;
	end process counter_proc;	
	
  tick_50hz <= '1' when counter_50hz = 999999 else '0';	
	
	timer50hz_proc: process (reset, clk)
	begin
		if reset = '1' then
			counter_50hz <= (others => '0');			
		elsif rising_edge(clk) then
			if tick_50hz = '1' then
				counter_50hz <= (others => '0');
			else	
				counter_50hz <= counter_50hz + 1;
			end if;
		end if;
	end process timer50hz_proc;	
	
	external_bus: process (reset, clk, slv1_chipselect, slv1_read, slv2_read)
	begin	 
		if reset = '1' then
			period <= (others => '0');
		elsif rising_edge(clk) then	
			if slv1_chipselect = '1' and slv1_write = '1' then  -- register 177706
				period <= slv1_writedata;
			end if;	
		end if;	
		if slv1_chipselect = '1' and slv1_read = '1' then
			slv1_readdata <= period;					 -- register 177706
		else 	
			slv1_readdata <= (others => '0');
		end if;
		if slv2_chipselect = '1' and slv2_write = '1' then
			if slv2_address = "0" then
				counter_wr <= '1';	 	-- register 177710
				control_wr <= '0';
			else	
				counter_wr <= '0';	  -- register 177712
				control_wr <= '1';
			end if;
		else	
			counter_wr <= '0';
			control_wr <= '0';
		end if;	
		if slv2_chipselect = '1' and slv2_read = '1' then
			if slv2_address = "0" then
				slv2_readdata <= counter;        -- register 177710
			else															 -- register 177712
				slv2_readdata <= "11111111" & count_end & divider16 & divider4 & count_enable & 
				                 single_shot & count_end_enable & init_set & stop;	
			end if;	
		else	
			slv2_readdata <= (others => '0');
		end if;
	end process external_bus;	
	
	aux_out <= aux_reg;
  irq <= not irq_en and tick_50hz;
end behaviour;
