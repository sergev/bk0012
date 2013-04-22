library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

entity sysreg is

  port (
    clk              : in  std_logic;
    io_address       : in  std_logic_vector(12 downto 0);
    io_waitrequest   : out std_logic;
    io_read          : in  std_logic;
    io_readdata      : out std_logic_vector(15 downto 0);
    io_readdatavalid : out std_logic;
    io_write         : in  std_logic;
    io_writedata     : in  std_logic_vector(15 downto 0);
    io_byteenable    : in  std_logic_vector(1 downto 0)
  );

end entity sysreg;

architecture behavior of sysreg is
begin

  sreg : process is
    variable io_index: natural;
  begin
    -- Initialize outputs.
    io_readdata      <= x"0000";
    io_waitrequest   <= '0';
    io_readdatavalid <= '0';

    -- Process sysreg cycles.
    loop

      -- Wait for a command, valid on leading edge of clk
      wait until clk = '1';

      -- decode address and perform command if selected
      io_index := to_integer(unsigned(io_address));

      --
      -- Write cycle.
      --
      if io_write = '1' then
        case io_index is
          when 8#17566# =>              -- Output port
            report "Output " & character'image(character'val(to_integer(unsigned(io_writedata(7 downto 0)))));
          when others =>
            report "I/O write unknown port " & integer'image(io_index);
        end case;

      --
      -- Read cycle.
      --
      elsif io_read = '1' then
        case io_index is
          when 8#17564# =>              -- Status of output port
            io_readdata <= std_logic_vector(to_unsigned(8#200#, 16));
--report "Poll output";
--        when 8#1376# / 2 =>           -- Address of output port
--          io_readdata <= std_logic_vector(to_unsigned(8#177566#, 16));
          when others =>
            report "I/O read unknown port " & integer'image(io_index);
            io_readdata <= x"0000";
        end case;

        io_readdatavalid <= '1';
        wait until clk = '1';
        io_readdatavalid <= '0';
      end if;
    end loop;
  end process sreg;

end architecture behavior;
