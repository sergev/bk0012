library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity sys_reg is
	port
	(
		reset: in std_logic;
		clk:   in std_logic;
		
		-- Avalon bus interface	 
		-- Register 177716	
		slv1_writedata: in std_logic_vector(15 downto 0); 
		slv1_chipselect: in std_logic;
		slv1_read: in std_logic;
		slv1_write: in std_logic;
		slv1_readdata: out std_logic_vector(15 downto 0);
		
		stop_intr_en: out std_logic; 
		RAM_window0: out std_logic_vector(2 downto 0);
		RAM_window1: out std_logic_vector(2 downto 0);
		ROM_page: out std_logic_vector(6 downto 0)
	);	
end sys_reg;

architecture behaviour of sys_reg is
  signal write_flag: std_logic;						 
	signal key_pressed: std_logic;
	signal stop_intr_en_reg: std_logic;
  signal RAM_window0_reg: std_logic_vector(2 downto 0);
	signal RAM_window1_reg: std_logic_vector(2 downto 0);
	signal ROM_page_reg: std_logic_vector(6 downto 0);
	signal rd_del: std_logic;
begin
	external_bus: process (reset, clk, slv1_chipselect, slv1_read)
	begin
		if reset = '1' then								
			write_flag <= '0';
			key_pressed <= '1';
			RAM_window0_reg <= (others => '0');
			RAM_window1_reg <= (others => '0');
			ROM_page_reg <= (others => '0');
			rd_del <= '0';
		elsif rising_edge(clk) then	
			if slv1_chipselect = '1' and slv1_write = '1' then
				if slv1_writedata(11) = '0' then 
					stop_intr_en_reg <= slv1_writedata(12);
					key_pressed <= slv1_writedata(6);
				else	
					RAM_window0_reg <= slv1_writedata(14 downto 12);
					RAM_window1_reg <= slv1_writedata(10 downto 8);
					ROM_page_reg <= slv1_writedata(6 downto 0);
				end if;
				write_flag <= '1';	 
			else	
				if slv1_chipselect = '1' and slv1_read = '1' then
					rd_del <= '1';
				else
					if rd_del = '1' then	
				    write_flag <= '0';
					end if;
					rd_del <= '0';
				end if;
			end if;	
		end if;
		if slv1_chipselect = '1' and slv1_read = '1' then
			slv1_readdata <= x"C0" & "1" & key_pressed & "000" & write_flag & "00";
		else	
			slv1_readdata <= (others => '0');
		end if;	
	end process external_bus;	

	stop_intr_en <= stop_intr_en_reg;
	RAM_window0 <= RAM_window0_reg;
	RAM_window1 <= RAM_window1_reg;
	ROM_page <= ROM_page_reg;
end behaviour;
