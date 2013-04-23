library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.def.all;
use work.config.all;
use work.iface.all;

entity dcache is
  generic
  (
    CACHE_SIZE: natural:= 2048;
    ASSOCIATIVITY: natural:= 2;
    LINE_SIZE: natural:= 8;
		DATA_WIDTH: natural:= 16
  );
  port
  (
	  clk : in std_logic;
		reset : in std_logic;

		din: in dcache_in_type;
		dout: out dcache_out_type;
--		dbg_state: out dcache_debug_state;

		-- Avalon bus Master interface
		avm_address: out std_logic_vector(31 downto 0);
		avm_burstcount: out std_logic_vector(log2(LINE_SIZE) downto 0);
		avm_read: out std_logic;
		avm_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_readdatavalid: in std_logic;
		avm_write: out std_logic;
		avm_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_byteenable: out std_logic_vector(DATA_WIDTH/8 -1 downto 0);
		avm_waitrequest: in std_logic;

		-- Avalon bus Master interface
		avm_io_address: out std_logic_vector(31 downto 0);
		avm_io_read: out std_logic;
		avm_io_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_io_readdatavalid: in std_logic;
		avm_io_write: out std_logic;
		avm_io_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_io_byteenable: out std_logic_vector(DATA_WIDTH/8 -1 downto 0);
		avm_io_waitrequest: in std_logic
	);
end dcache;

architecture behaviour_stub of dcache is
  signal rd_address: std_logic_vector(15 downto 0);
	signal wr_address: std_logic_vector(15 downto 0);
	signal wr_data: std_logic_vector(15 downto 0);
	signal wr: std_logic;
	signal byte_en: std_logic_vector(1 downto 0);
	signal map_in: address_map_in;
	signal map_out: address_map_out;

	type state_type is (st_idle, st_rd_address, st_io_rd_address, st_rd_data, st_io_rd_data, st_wr_address, st_io_wr_address);
	signal state, next_state: state_type;
begin
  process (reset, clk)
  begin
		if reset = '1' then
			rd_address <= (others => '0');
			wr_address <= (others => '0');
			wr_data <= (others => '0');
			wr <= '0';
			byte_en <= (others => '0');
		elsif rising_edge(clk) then
		  if din.rd_address_stall = '0' then
			  rd_address <= din.rd_address;
			end if;
			if wr = '0' or (state = st_wr_address and next_state = st_idle)
				 or (wr = '1' and state = st_idle and next_state = st_idle)
			then
				wr_address <= din.wr_address;
			  wr_data <= din.wr_data;
			  wr <= din.wr;
			  byte_en <= din.byte_en;
			end if;
		end if;
	end process;

	process (reset, clk)
	begin
		if reset = '1' then
			state <= st_idle;
		elsif rising_edge(clk) then
			state <= next_state;
		end if;
	end process;

	next_state_proc: process (state, din, rd_address, wr_address, wr, byte_en, map_in, map_out,
	  avm_waitrequest, avm_io_waitrequest, avm_readdata, avm_readdatavalid,
	  avm_io_readdata, avm_io_readdatavalid)

		procedure EMPTY is
		begin
			map_in.address <= rd_address;
			map_out <= address_map(map_in);
			avm_address <= (others => '0');
		  avm_read <= '0';
		  avm_write <= '0';

			avm_io_address <= (others => '0');
			avm_io_read <= '0';
			avm_io_write <= '0';

			dout.data <= (others => '0');
			dout.data_rdy <= '0';
			dout.wr_rdy <= '1';
		end procedure EMPTY;

		procedure DO_WRITE is
		begin
			map_in.address <= wr_address;
			map_out <= address_map(map_in);
			avm_address <= map_out.address(31 downto 1) & '0';
		  avm_read <= '0';
		  avm_write <= map_out.ram_cs;

			avm_io_address <= map_out.address(31 downto 1) & '0';
			avm_io_read <= '0';
			avm_io_write <= map_out.io_cs;

			dout.data <= (others => '0');
			dout.data_rdy <= '0';
			if map_out.ram_cs = '1' then
				dout.wr_rdy <= not avm_waitrequest;
			elsif	map_out.io_cs = '1' then
				dout.wr_rdy <= not avm_io_waitrequest;
			else
				dout.wr_rdy <= '0';
			end if;
		end procedure	DO_WRITE;

		procedure DO_READ is
		begin
			map_in.address <= rd_address;
			map_out <= address_map(map_in);
			avm_address <= map_out.address(31 downto 1) & '0';
		  avm_read <= (map_out.ram_cs or map_out.rom_cs);
		  avm_write <= '0';

			avm_io_address <= map_out.address(31 downto 1) & '0';
			avm_io_read <= map_out.io_cs;
			avm_io_write <= '0';

			dout.data <= (others => '0');
			dout.data_rdy <= '0';
			dout.wr_rdy <= '0';
		end procedure DO_READ;

		procedure DO_READ_DATA is
		begin
			map_in.address <= rd_address;
			map_out <= address_map(map_in);
			avm_address <= map_out.address(31 downto 1) & '0';
		  avm_read <= '0';
			avm_write <= '0';

			avm_io_address <= map_out.address(31 downto 1) & '0';
			avm_io_read <= '0';
			avm_io_write <= '0';

			if map_out.ram_cs = '1' or map_out.rom_cs = '1' then
				dout.data <= avm_readdata;
				dout.data_rdy <= avm_readdatavalid;
			elsif map_out.io_cs = '1' then
				dout.data <= avm_io_readdata;
				dout.data_rdy <= avm_io_readdatavalid;
			else
				dout.data <= (others => '0');
				dout.data_rdy <= '0';
			end if;
			dout.wr_rdy <= '0';
		end procedure DO_READ_DATA;

	begin
		case state is
			when st_idle =>
			  if wr = '1' then
					DO_WRITE;
					if (map_out.ram_cs = '1' or map_out.rom_cs = '1') and avm_waitrequest = '1' then
						next_state <= st_wr_address;
					elsif map_out.io_cs = '1' and avm_io_waitrequest = '1' then
						next_state <= st_io_wr_address;
					else
						next_state <= st_idle;
					end if;
				elsif din.rd = '1' then
					DO_READ;
					if (map_out.ram_cs = '1' or map_out.rom_cs = '1') and avm_waitrequest = '1' then
						next_state <= st_rd_address;
					elsif map_out.io_cs = '1' and  avm_io_waitrequest = '1' then
						next_state <= st_io_rd_address;
					else
						if map_out.io_cs = '0' then
						  next_state <= st_rd_data;
						else
							next_state <= st_io_rd_data;
						end if;
					end if;
				else
					EMPTY;
					next_state <= st_idle;
				end if;
			when st_rd_address =>
			  DO_READ;
				if avm_waitrequest = '1' then
					next_state <= st_rd_address;
				else
					next_state <= st_rd_data;
				end if;
			when st_io_rd_address =>
			  DO_READ;
				if avm_io_waitrequest = '1' then
					next_state <= st_io_rd_address;
				else
					next_state <= st_io_rd_data;
				end if;
			when st_rd_data =>
			  DO_READ_DATA;
			  if avm_readdatavalid = '1' then
					next_state <= st_idle;
				else
					next_state <= st_rd_data;
				end if;
			when st_io_rd_data =>
			  DO_READ_DATA;
			  if avm_io_readdatavalid = '1' then
					next_state <= st_idle;
				else
					next_state <= st_io_rd_data;
				end if;
			when st_wr_address =>
			  DO_WRITE;
				if avm_waitrequest = '1' then
					next_state <= st_wr_address;
				else
					next_state <= st_idle;
				end if;
			when st_io_wr_address =>
			  DO_WRITE;
				if avm_io_waitrequest = '1' then
					next_state <= st_io_wr_address;
				else
					next_state <= st_idle;
				end if;
		end case;
	end process next_state_proc;

	avm_burstcount <= "0001";
	avm_writedata <= wr_data;
	avm_byteenable <= byte_en;

	avm_io_writedata <= wr_data;
	avm_io_byteenable <= byte_en;

end behaviour_stub;
