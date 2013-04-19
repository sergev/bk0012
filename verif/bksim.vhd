library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.iface.all;
--use work.textio.all;

entity bksim is
end bksim;

architecture bench of bksim is

  constant CLOCK_PERIOD : Time := 1000 ns;      -- Use 1us as a base cycle

  component resetgen is                         -- Reset generator
    port (
      clk     : in  std_logic := 'X';
      reset   : out std_logic;
      reset_n : out std_logic
    );
  end component resetgen;

  component moonshiner11 is                     -- PDP-11 processor
    port (
      clk                      : in  std_logic                     := 'X';
      reset                    : in  std_logic                     := 'X';
      reset_out                : out std_logic;
      int_reset_out            : out std_logic;
      irq                      : in  std_logic                     := 'X';
      ivec                     : in  std_logic_vector(7 downto 0)  := (others => 'X');
      iack                     : out std_logic;
      avm_icache_address       : out std_logic_vector(31 downto 0);
      avm_icache_burstcount    : out std_logic_vector(3 downto 0);
      avm_icache_read          : out std_logic;
      avm_icache_readdata      : in  std_logic_vector(31 downto 0) := (others => 'X');
      avm_icache_readdatavalid : in  std_logic                     := 'X';
      avm_icache_waitrequest   : in  std_logic                     := 'X';
      avm_icache_flush         : out std_logic;
      avm_dcache_address       : out std_logic_vector(31 downto 0);
      avm_dcache_burstcount    : out std_logic_vector(3 downto 0);
      avm_dcache_read          : out std_logic;
      avm_dcache_readdata      : in  std_logic_vector(15 downto 0) := (others => 'X');
      avm_dcache_readdatavalid : in  std_logic                     := 'X';
      avm_dcache_write         : out std_logic;
      avm_dcache_writedata     : out std_logic_vector(15 downto 0);
      avm_dcache_byteenable    : out std_logic_vector(1 downto 0);
      avm_dcache_waitrequest   : in  std_logic                     := 'X';
      avm_io_address           : out std_logic_vector(31 downto 0);
      avm_io_read              : out std_logic;
      avm_io_readdata          : in  std_logic_vector(15 downto 0) := (others => 'X');
      avm_io_readdatavalid     : in  std_logic                     := 'X';
      avm_io_write             : out std_logic;
      avm_io_writedata         : out std_logic_vector(15 downto 0);
      avm_io_byteenable        : out std_logic_vector(1 downto 0);
      avm_io_waitrequest       : in  std_logic                     := 'X';
      avs_dbg_address          : in  std_logic_vector(8 downto 0)  := (others => 'X');
      avs_dbg_writedata        : in  std_logic_vector(15 downto 0) := (others => 'X');
      avs_dbg_chipselect       : in  std_logic                     := 'X';
      avs_dbg_read             : in  std_logic                     := 'X';
      avs_dbg_write            : in  std_logic                     := 'X';
      avs_dbg_readdata         : out std_logic_vector(15 downto 0);
      trace                    : out trace_type
    );
  end component moonshiner11;

  component bus_monitor is
    port (
      clk                 : in std_logic;
    	reset               : in std_logic;
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
  end component;

  component memory is
    port (
      clk              : in  std_logic;
      p1_address       : in  std_logic_vector(31 downto 0);
      p1_waitrequest   : out std_logic;
      p1_read          : in  std_logic;
      p1_readdata      : out std_logic_vector(15 downto 0);
      p1_readdatavalid : out std_logic;
      p1_write         : in  std_logic;
      p1_writedata     : in  std_logic_vector(15 downto 0);
      p1_byteenable    : in  std_logic_vector(1 downto 0);
      p2_address       : in  std_logic_vector(31 downto 0);
      p2_waitrequest   : out std_logic;
      p2_read          : in  std_logic;
      p2_readdata      : out std_logic_vector(31 downto 0);
      p2_readdatavalid : out std_logic
    );
  end component;

  signal clk           : std_logic;
  signal reset         : std_logic;

  -- Instruction bus.
  signal instr_address       : std_logic_vector(31 downto 0);
  signal instr_read          : std_logic;
  signal instr_readdata      : std_logic_vector(31 downto 0);
  signal instr_readdatavalid : std_logic;
  signal instr_waitrequest   : std_logic;

  -- Data bus.
  signal data_address        : std_logic_vector(31 downto 0);
  signal data_read           : std_logic;
  signal data_readdata       : std_logic_vector(15 downto 0);
  signal data_readdatavalid  : std_logic;
  signal data_waitrequest    : std_logic;
  signal data_write          : std_logic;
  signal data_writedata      : std_logic_vector(15 downto 0);
  signal data_byteenable     : std_logic_vector(1 downto 0);

  -- I/o bus.
  signal io_address          : std_logic_vector(31 downto 0);
  signal io_read             : std_logic;
  signal io_readdata         : std_logic_vector(15 downto 0);
  signal io_readdatavalid    : std_logic;
  signal io_waitrequest      : std_logic;
  signal io_write            : std_logic;
  signal io_writedata        : std_logic_vector(15 downto 0);
  signal io_byteenable       : std_logic_vector(1 downto 0);

  -- Interrupts.
  signal irq  : std_logic;
  signal ivec : std_logic_vector(7 downto 0);
  signal iack : std_logic;

  -- Trace port.
  signal trace : trace_type;

begin

  rg : component resetgen               -- Generate reset at startup
    port map (
      clk     => clk,
      reset   => reset,
      reset_n => open
    );

  cpu : component moonshiner11
    port map (
      clk                      => clk,                  -- in
      reset                    => reset,                -- in
      reset_out                => open,                 -- out
      int_reset_out            => open,                 -- out
      irq                      => irq,                  -- in        irq.export
      ivec                     => ivec,                 -- in       ivec.export
      iack                     => iack,                 -- out      iack.export
      avm_icache_address       => instr_address,        -- out    icache.address
      avm_icache_burstcount    => open,                 -- out
      avm_icache_read          => instr_read,           -- out          .read
      avm_icache_readdata      => instr_readdata,       -- in           .readdata
      avm_icache_readdatavalid => instr_readdatavalid,  -- in           .readdatavalid
      avm_icache_waitrequest   => instr_waitrequest,    -- in           .waitrequest
      avm_icache_flush         => open,                 -- out          .flush
      avm_dcache_address       => data_address,         -- out    dcache.address
      avm_dcache_burstcount    => open,                 -- out
      avm_dcache_read          => data_read,            -- out          .read
      avm_dcache_readdata      => data_readdata,        -- in           .readdata
      avm_dcache_readdatavalid => data_readdatavalid,   -- in           .readdatavalid
      avm_dcache_write         => data_write,           -- out          .write
      avm_dcache_writedata     => data_writedata,       -- out          .writedata
      avm_dcache_byteenable    => data_byteenable,      -- out          .byteenable
      avm_dcache_waitrequest   => data_waitrequest,     -- in           .waitrequest
      avm_io_address           => io_address,           -- out        io.address
      avm_io_read              => io_read,              -- out          .read
      avm_io_readdata          => io_readdata,          -- in           .readdata
      avm_io_readdatavalid     => io_readdatavalid,     -- in           .readdatavalid
      avm_io_write             => io_write,             -- out          .write
      avm_io_writedata         => io_writedata,         -- out          .writedata
      avm_io_byteenable        => io_byteenable,        -- out          .byteenable
      avm_io_waitrequest       => io_waitrequest,       -- in           .waitrequest
      avs_dbg_address          => o"000",               -- in
      avs_dbg_writedata        => x"0000",              -- in
      avs_dbg_chipselect       => '0',                  -- in
      avs_dbg_read             => '0',                  -- in
      avs_dbg_write            => '0',                  -- in
      avs_dbg_readdata         => open,                 -- out
      trace                    => trace                 -- out
    );

  bm : component bus_monitor            -- Print trace information
    port map (
      clk                 => clk,
      reset               => reset,
      irq                 => irq,
      ivec                => ivec,
      iack                => iack,
      instr_address       => instr_address,
      instr_read          => instr_read,
      instr_readdata      => instr_readdata,
      instr_readdatavalid => instr_readdatavalid,
      instr_waitrequest   => instr_waitrequest,
      data_address        => data_address,
      data_read           => data_read,
      data_readdata       => data_readdata,
      data_readdatavalid  => data_readdatavalid,
      data_write          => data_write,
      data_writedata      => data_writedata,
      data_byteenable     => data_byteenable,
      data_waitrequest    => data_waitrequest,
      io_address          => io_address,
      io_read             => io_read,
      io_readdata         => io_readdata,
      io_readdatavalid    => io_readdatavalid,
      io_write            => io_write,
      io_writedata        => io_writedata,
      io_byteenable       => io_byteenable,
      io_waitrequest      => io_waitrequest,
      trace               => trace
    );

  mem : component memory                -- RAM
    port map (
      clk              => clk,
      p1_address       => data_address,       -- in
      p1_waitrequest   => data_waitrequest,   -- out
      p1_read          => data_read,          -- in
      p1_readdata      => data_readdata,      -- out
      p1_readdatavalid => data_readdatavalid, -- out
      p1_write         => data_write,         -- in
      p1_writedata     => data_writedata,     -- in
      p1_byteenable    => data_byteenable,    -- in
      p2_address       => instr_address,      -- in
      p2_waitrequest   => instr_waitrequest,  -- out
      p2_read          => instr_read,         -- in
      p2_readdata      => instr_readdata,     -- out
      p2_readdatavalid => instr_readdatavalid -- out
    );

  clock_gen : process                   -- Generate main clock
  begin
    if reset = '1' then                 -- Initial state
      irq               <= '0';
      ivec              <= x"00";
      io_readdata       <= x"0000";
      io_readdatavalid  <= '0';
      io_waitrequest    <= '0';
    end if;

    if trace.halt = '1' and trace.ID_insn.rdy = '0' and trace.ID2_insn.rdy = '0' and
       trace.AG_insn.rdy = '0' and trace.MEM_insn.rdy = '0' and
       trace.EX_insn.rdy = '0' and trace.WB_insn.rdy = '0'
    then                                -- CPU halted
      report "Processor halted";
      wait;
    end if;

    clk <= '1', '0' after CLOCK_PERIOD/2;
    wait for CLOCK_PERIOD;

  end process clock_gen;

end bench;
