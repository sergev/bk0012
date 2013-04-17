library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_unsigned.all;

entity resetgen is
  port
  (
    clk: in std_logic;
--	  reset_in: in std_logic;
	  reset: out std_logic := '1';
	  reset_n: out std_logic
  );
 end resetgen;

architecture behaviour of resetgen is
  constant MAX_RESET_CNT: integer := 23;
  signal prescale_cnt: std_logic_vector(2 downto 0);
  signal reset_cnt: std_logic_vector(9 downto 0) := (others => '0');
  signal reset_in: std_logic := '0';
begin
  process (clk)
  begin
	  if rising_edge(clk) then
      if reset_cnt /= MAX_RESET_CNT then
	      reset_cnt <= reset_cnt + 1;
		  end if;
	  end if;
	  if reset_in = '1' then
		  reset <= '1';
		  reset_n <= '0';
	  elsif reset_cnt = MAX_RESET_CNT then
	    reset <= '0';
		  reset_n <= '1';
	  else
		  reset <= '1';
		  reset_n <= '0';
	  end if;
  end process;
end behaviour;
