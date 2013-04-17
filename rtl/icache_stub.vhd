library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.def.all;
use work.iface.all;
use work.config.all;

entity icache is
  generic
  (
    CACHE_SIZE: natural:= 2048;
    ASSOCIATIVITY: natural:= 2;
    LINE_SIZE: natural:= 8;
		DATA_WIDTH: natural:= 32
  );
  port
  (
	  clk : in std_logic;
		reset : in std_logic;

		din: in icache_in_type;
		dout: out icache_out_type;

		-- Avalon bus Master interface
		avm_address: out std_logic_vector(31 downto 0);
		avm_burstcount: out std_logic_vector(log2(LINE_SIZE) downto 0);
		avm_read: out std_logic;
		avm_flush: out std_logic;
		avm_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_readdatavalid: in std_logic;
		avm_waitrequest: in std_logic
	);
end icache;

architecture behaviour_stub of icache is
	signal map_in: address_map_in;
	signal map_out: address_map_out;
	signal flush: std_logic;

	type state_type is (st_idle, st_rd_address, st_rd_data);
	signal state, next_state: state_type;
begin
	process (reset, clk)
	begin
		if reset = '1' then
			state <= st_idle;
			flush <= '0';
		elsif rising_edge(clk) then
			state <= next_state;
			if next_state = st_idle then
				flush <= '0';
			elsif din.flush = '1' then
				flush <= '1';
			end if;
		end if;
	end process;

	next_state_proc: process (state, din, map_out, avm_waitrequest, avm_readdatavalid)
	begin
		case state is
			when st_idle =>
			  if din.en = '1' then
				  if (map_out.rom_cs or map_out.ram_cs) = '1' then
						avm_read <= '1';
						if avm_waitrequest = '1' then
							next_state <= st_rd_address;
						else
							next_state <= st_rd_data;
						end if;
					else
					  avm_read <= '0';
					  next_state <= st_idle;
					end if;
				else
					avm_read <= '0';
					next_state <= st_idle;
				end if;
			when st_rd_address =>
			  avm_read <= '1';
				if avm_waitrequest = '1' then
					next_state <= st_rd_address;
				else
					next_state <= st_rd_data;
				end if;
			when st_rd_data =>
			  avm_read <= '0';
			  if avm_readdatavalid = '1' then
					next_state <= st_idle;
				else
					next_state <= st_rd_data;
				end if;
		end case;
	end process next_state_proc;

	map_in.address <= din.pc & '0';
	map_out <= address_map(map_in);

	avm_burstcount <= "0001";
	avm_address <= map_out.address(31 downto 2) & "00";
	avm_flush <= '0';

	dout.data0 <= avm_readdata(15 downto 0) when din.pc(1) = '0'
	  else avm_readdata(31 downto 16);
	dout.data0_rdy <= '1' when avm_readdatavalid = '1' and flush = '0' and din.flush = '0' else '0';
	dout.data1 <= avm_readdata(31 downto 16);
	dout.data1_rdy <= '1' when avm_readdatavalid = '1' and din.pc(1) = '0' and flush = '0' and din.flush = '0' else '0';
end behaviour_stub;
