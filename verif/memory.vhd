library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

entity memory is

  generic (
    MEM_SIZE       : positive;
	  LOAD_FILE_NAME : string
  );

  port (
    clk              : in  std_logic;

    -- Port 1: width 16 bits, read and write
    p1_address       : in  std_logic_vector(31 downto 0);
    p1_waitrequest   : out std_logic;
    p1_read          : in  std_logic;
    p1_readdata      : out std_logic_vector(15 downto 0);
    p1_readdatavalid : out std_logic;
    p1_write         : in  std_logic;
    p1_writedata     : in  std_logic_vector(15 downto 0);
    p1_byteenable    : in  std_logic_vector(1 downto 0);

    -- Port 2: width 32 bits, read only
    p2_address       : in  std_logic_vector(31 downto 0);
    p2_waitrequest   : out std_logic;
    p2_read          : in  std_logic;
    p2_readdata      : out std_logic_vector(31 downto 0);
    p2_readdatavalid : out std_logic
  );

end entity memory;

architecture preloaded of memory is

  constant HIGH_INDEX : natural := (MEM_SIZE - 1) / 2;

  type memory_array is array (natural range 0 to HIGH_INDEX)
                    of bit_vector(15 downto 0);

  shared variable mem : memory_array;

  --
  -- Load memory contents from file.
  -- Valid lines start with octal digit and have format:
  --  <address> <word> ...
  -- Line can contain an address and several octal words.
  -- All other lines, and trailing characters are ignored.
  --
  procedure load is

    file fd : text is LOAD_FILE_NAME;
    variable L : line;
    variable ch : character;
    variable addr, word : natural;
    variable succeed : boolean;

    --
    -- Read octal number from the line.
    -- Return ch=NUL when no valid number found.
    --
    procedure read_oct(
      L    : inout line;
      ch   : inout character;
      word : out natural
    )
    is
      variable result  : natural;
      variable succeed : boolean;
    begin
      loop
        if ch >= '0' and ch <= '7' then         -- Find first digit
          exit;
        end if;
        if ch /= ' ' and ch /= HT then          -- Not a space - ignore line
          ch := NUL;
          return;
        end if;
        read(L, ch, succeed);                   -- Get first symbol
        if not succeed then
          ch := NUL;                            -- End of line
          return;
        end if;
      end loop;
      result := 0;
      while (ch >= '0' and ch <= '7') loop      -- Read octal number
        result := result*8 + character'pos(ch) - character'pos('0');
        read(L, ch, succeed);
        if not succeed then                     -- Premature EOF - ignore line
          ch := NUL;
          exit;
        end if;
      end loop;
      word := result;
    end read_oct;

  begin
    write(L, string'("Load file """ & LOAD_FILE_NAME) & """");
    writeline(output, L);
    while not endfile(fd) loop
      readline(fd, L);
      read(L, ch, succeed);                     -- Get first symbol
      if succeed and ch >= '0' and ch <= '7' then
        read_oct(L, ch, addr);                  -- Get octal address
        while ch /= NUL loop
          read_oct(L, ch, word);                -- Get octal word, ignore the rest
          mem(addr/2) := to_bitvector(std_logic_vector(to_unsigned(word, 16)));
          addr := addr + 2;
        end loop;
      end if;
    end loop;
  end load;

begin

  mem_port1 : process is
    variable p1_index: natural;
  begin
    load;

    -- Initialize outputs.
    p1_readdata      <= x"0000";
    p1_waitrequest   <= '0';
    p1_readdatavalid <= '0';

    -- Process memory cycles.
    loop

      -- Wait for a command, valid on leading edge of clk
      wait until clk = '1';

      -- decode address and perform command if selected
      p1_index := to_integer(unsigned(p1_address(27 downto 1)));
      if p1_index <= HIGH_INDEX then
        --
        -- Write cycle on port 1
        --
        if p1_write = '1' then
          if p1_byteenable(0) = '1' then
            mem(p1_index)(7 downto 0) := to_bitvector(p1_writedata(7 downto 0));
          end if;
          if p1_byteenable(1) = '1' then
            mem(p1_index)(15 downto 8) := to_bitvector(p1_writedata(15 downto 8));
          end if;
          if p1_index = 8#000406# / 2 then
            report "Pass " & integer'image(to_integer(unsigned(p1_writedata)));
          end if;
          if p1_index = 8#000404# / 2 then
            report "Test " & integer'image(to_integer(unsigned(p1_writedata)));
          end if;

        --
        -- Read cycle on port 1
        --
        elsif p1_read = '1' then
          p1_readdata <= To_X01(mem(p1_index));
          p1_readdatavalid <= '1';
          wait until clk = '1';
          p1_readdatavalid <= '0';
        end if;
      end if;
    end loop;
  end process mem_port1;

  mem_port2 : process is
    variable p2_index: natural;
  begin
    -- Initialize outputs.
    p2_readdata      <= x"00000000";
    p2_waitrequest   <= '0';
    p2_readdatavalid <= '0';

    -- Process memory cycles.
    loop
      -- Wait for a command, valid on leading edge of clk
      wait until clk = '1';

      -- decode address and perform command if selected
      p2_index := to_integer(unsigned(p2_address(27 downto 1)));
      if p2_index <= HIGH_INDEX then
        --
        -- Read cycle on port 2
        --
        if p2_read = '1' then
          p2_readdata(15 downto 0)  <= To_X01(mem(p2_index));
          p2_readdata(31 downto 16) <= To_X01(mem(p2_index + 1));
          p2_readdatavalid <= '1';
          wait until clk = '1';
          p2_readdatavalid <= '0';
        end if;
      end if;
    end loop;
  end process mem_port2;

end architecture preloaded;
