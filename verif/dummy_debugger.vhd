library IEEE;
use IEEE.std_logic_1164.all;
use work.def.all;
use work.iface.all;

entity hw_debugger is
	generic (
	  INSTRUCTION_BREAKPOINT_NUM: natural range 1 to 8;
		DATA_BREAKPOINT_NUM: natural range 1 to 6;
		TRACE_BUFFER_SIZE: natural
	);
  port (
	  clk : in std_logic;
		reset : in std_logic;

		-- Avalon bus slave
		avs_dbg_address: in std_logic_vector(8 downto 0) := (others => '0');
		avs_dbg_writedata: in std_logic_vector(15 downto 0) := (others => '0');
		avs_dbg_chipselect: in std_logic := '0';
		avs_dbg_read: in std_logic := '0';
		avs_dbg_write: in std_logic := '0';
		avs_dbg_readdata: out std_logic_vector(WORD_SIZE-1 downto 0);

		din: in hw_debugger_in_type;
		dout: out hw_debugger_out_type;

		reset_out: out std_logic;
		int_reset_out: out std_logic
  );
end hw_debugger;

architecture behaviour of hw_debugger is
begin
  avs_dbg_readdata <= x"0000";
  dout.reg_address <= "0000";
  dout.halt <= '0';
  reset_out <= '0';
  int_reset_out <= '0';
end behaviour;
