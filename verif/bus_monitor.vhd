--
-- Bus monitor
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.iface.all;
use work.textio.all;
use work.pdp11_disasm.all;

entity bus_monitor is
  generic (
    enable  : boolean := TRUE                 -- enable monitoring
  );
  port (
    clk                 : in std_logic;       -- clock
    reset               : in std_logic;       -- synchronous reset
    irq                 : in std_logic;
    ivec                : in std_logic_vector(7 downto 0);
    iack                : in std_logic;
    instr_address       : in std_logic_vector(31 downto 0);
    instr_read          : in std_logic;
    instr_readdata      : in std_logic_vector(31 downto 0);
    instr_readdatavalid : in std_logic;
    instr_waitrequest   : in std_logic;
    data_address        : in std_logic_vector(31 downto 0);
    data_read           : in std_logic;
    data_readdata       : in std_logic_vector(15 downto 0);
    data_readdatavalid  : in std_logic;
    data_write          : in std_logic;
    data_writedata      : in std_logic_vector(15 downto 0);
    data_byteenable     : in std_logic_vector(1 downto 0);
    data_waitrequest    : in std_logic;
    io_address          : in std_logic_vector(31 downto 0);
    io_read             : in std_logic;
    io_readdata         : in std_logic_vector(15 downto 0);
    io_readdatavalid    : in std_logic;
    io_write            : in std_logic;
    io_writedata        : in std_logic_vector(15 downto 0);
    io_byteenable       : in std_logic_vector(1 downto 0);
    io_waitrequest      : in std_logic;
    trace               : in trace_type
  );
  constant CLOCK_PERIOD : Time := 1000 ns;      -- Use 1us as a base cycle
end bus_monitor;

architecture behaviour of bus_monitor is
begin

  monitor : if enable generate

    enabled_monitor : process (clk)

      variable L : line;
      variable under_reset: boolean := FALSE;
      variable latch_instr_address: std_logic_vector(31 downto 0);
      variable latch_data_address: std_logic_vector(31 downto 0);
      variable latch_io_address: std_logic_vector(31 downto 0);

      procedure print_insn (insn: insn_trace_type; tag: string) is
      begin
        if (insn.rdy = '1' and
            (insn.len = "01" or
             (insn.len = "10" and insn.immed1_rdy = '1') or
             (insn.len = "11" and insn.immed1_rdy = '1' and insn.immed2_rdy = '1'))) or
           (trace.halt = '1' and insn.opcode = x"0000" and
            trace.ID_insn.rdy = '0' and trace.ID2_insn.rdy = '0' and
            trace.AG_insn.rdy = '0' and trace.MEM_insn.rdy = '0' and
            trace.EX_insn.rdy = '0' and trace.WB_insn.rdy = '0')
        then
          write(L, '(');
          write(L, integer'image(now / CLOCK_PERIOD));
          write(L, string'(") "));
          write(L, tag);
          owrite(L, "00" & insn.pc);
          write(L, string'(": "));
          owrite(L, "00" & insn.opcode);
          if insn.len(1) = '1' then
            write(L, string'(" "));
            owrite(L, "00" & insn.immed1);
          else
            write(L, string'("       "));
          end if;
          if insn.len = "11" then
            write(L, string'(" "));
            owrite(L, "00" & insn.immed2);
          else
            write(L, string'("       "));
          end if;
          write(L, string'(" "));
          write(L, disassemble(insn.opcode, insn.immed1, insn.immed2,
            '1', insn.immed1_rdy, insn.immed2_rdy));
          writeline(output, L);
        end if;
      end print_insn;

    begin
      --
      -- wait for a command, valid on falling edge of clk
      --
      if clk'event and clk = '1' then

        if reset = '1' then
          if not under_reset then                   -- Reset
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(") Reset"));
            writeline(output, L);
            under_reset := TRUE;
          end if;
        else
          if under_reset then                         -- Reset clear
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(") End Reset"));
            writeline(output, L);
            under_reset := FALSE;
          end if;

          --if trace.ID_stall = '0' or trace.ID2_stall = '0' or
          --   trace.AG_stall = '0' or trace.MEM_stall = '0' or
          --   trace.EX_stall = '0' or trace.ID_stall = '0'
          --then
          --  write(L, '(');
          --  write(L, integer'image(now / CLOCK_PERIOD));
          --  write(L, string'(") ------"));
          --  writeline(output, L);
          --end if;
          --if trace.ID_stall = '0' then
          --  print_insn (trace.ID_insn, "id ");
          --end if;
          --if trace.ID2_stall = '0' then
          --  print_insn (trace.ID2_insn, "id+");
          --end if;
          --if trace.AG_stall = '0' then
          --  print_insn (trace.AG_insn, "ag ");
          --end if;
          --if trace.MEM_stall = '0' then
          --  print_insn (trace.MEM_insn, "mem");
          --end if;
          --if trace.EX_stall = '0' then
          print_insn (trace.EX_insn, "");         -- Print instruction
          --end if;
          --if trace.WB_stall = '0' then
          --  print_insn (trace.WB_insn, "wb ");
          --end if;

          if trace.gpr_mask(0) = '1' then             -- Print registers
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")    r0 = "));
            owrite(L, "00" & trace.gpr(0));
            writeline(output, L);
          end if;
          if trace.gpr_mask(1) = '1' then
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")    r1 = "));
            owrite(L, "00" & trace.gpr(1));
            writeline(output, L);
          end if;
          if trace.gpr_mask(2) = '1' then
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")    r2 = "));
            owrite(L, "00" & trace.gpr(2));
            writeline(output, L);
          end if;
          if trace.gpr_mask(3) = '1' then
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")    r3 = "));
            owrite(L, "00" & trace.gpr(3));
            writeline(output, L);
          end if;
          if trace.gpr_mask(4) = '1' then
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")    r4 = "));
            owrite(L, "00" & trace.gpr(4));
            writeline(output, L);
          end if;
          if trace.gpr_mask(5) = '1' then
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")    r5 = "));
            owrite(L, "00" & trace.gpr(5));
            writeline(output, L);
          end if;
          if trace.gpr_mask(6) = '1' then
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")    sp = "));
            owrite(L, "00" & trace.gpr(6));
            writeline(output, L);
          end if;

          if instr_read = '1' then                    -- Fetch address
            latch_instr_address := instr_address;
          end if;

          if instr_readdatavalid = '1' then           -- Fetch data
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")                                           Fetch ["));
            --owrite(L, "00" & latch_instr_address(31 downto 16));
            --write(L, string'(" "));
            owrite(L, "00" & latch_instr_address(15 downto 0));
            write(L, string'("] -> "));
            owrite(L, "00" & instr_readdata(31 downto 16));
            write(L, string'(" "));
            owrite(L, "00" & instr_readdata(15 downto 0));
            writeline(output, L);
          end if;

          if data_read = '1' or data_write = '1' then -- Read/write address
            latch_data_address := data_address;
          end if;

          if data_readdatavalid = '1' then            -- Read data
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")                                           Read  ["));
            --owrite(L, "00" & latch_data_address(31 downto 16));
            --write(L, string'(" "));
            owrite(L, "00" & latch_data_address(15 downto 0));
            write(L, string'("] -> "));
            owrite(L, "00" & data_readdata(15 downto 0));
            writeline(output, L);
          end if;

          if data_write = '1' then                    -- Write data
            write(L, '(');
            write(L, integer'image(now / CLOCK_PERIOD));
            write(L, string'(")                                           Write ["));
            --owrite(L, "00" & data_address(31 downto 16));
            --write(L, string'(" "));
            owrite(L, "00" & data_address(15 downto 0));
            write(L, string'("] <- "));
            if data_byteenable(0) = '1' then
              if data_byteenable(1) = '1' then
                owrite(L, "00" & data_writedata(15 downto 0));
              else
                write(L, string'("xxx"));
                owrite(L, "0" & data_writedata(7 downto 0));
              end if;
            else
              if data_byteenable(1) = '1' then
                owrite(L, "00" & data_writedata(15 downto 8) & "00");
                write(L, string'("xx"));
              else
                write(L, string'("xxxxxx"));
              end if;
            end if;
            writeline(output, L);
          end if;
        end if;
      end if;
    end process enabled_monitor;

  end generate;

end behaviour;
