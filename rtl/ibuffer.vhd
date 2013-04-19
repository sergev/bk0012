library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_unsigned.all;
use work.def.all;
use work.iface.all;

entity ibuffer is
	port
	(
		clk : in std_logic;
		reset : in std_logic;
	  din: in ibuffer_in_type;
		dout: out ibuffer_out_type
	);
end ibuffer;

architecture behaviour of ibuffer is
  type slice_type is record
		data: std_logic_vector(WORD_SIZE-1 downto 0);
		pc: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		len:  std_logic_vector(1 downto 0);
		rdy:  std_logic;
	end record;

	type slice_array_type is array(0 to 3) of slice_type;

	signal r, next_r: slice_array_type;
	signal word_num, next_word_num: std_logic_vector(2 downto 0);
	signal read_num: std_logic_vector(1 downto 0);

	function slice_init return slice_type is
	 variable v: slice_type;
	begin
		v.data := (others => '0');
		v.pc := (others => '0');
		v.len := (others => '0');
		v.rdy := '0';
		return v;
	end slice_init;
begin
	next_state: process (din, r, word_num)
	  variable consumed_word_num: std_logic_vector(1 downto 0);
	  variable remain_word_num: std_logic_vector(2 downto 0);
		variable in_word_num: std_logic_vector(1 downto 0);
		variable v: slice_array_type;
		variable in_slice0: slice_type;
		variable in_slice1: slice_type;
	begin
		v := r;
		in_slice0.data := din.icache_data0;
		in_slice0.pc := din.pc;
		in_slice0.rdy := din.icache_data0_rdy;
		if din.icache_data0_rdy = '1' then
		  in_slice0.len := instruction_len(din.icache_data0);
		else
			in_slice0.len := "00";
		end if;
		in_slice1.data := din.icache_data1;
		in_slice1.pc := din.pc + 1;
		in_slice1.rdy := din.icache_data1_rdy;
		if din.icache_data1_rdy = '1' then
		  in_slice1.len := instruction_len(din.icache_data1);
		else
			in_slice1.len := "00";
		end if;
		in_word_num := ("0" & din.icache_data0_rdy) + ("0" & din.icache_data1_rdy);
		if din.insn_rd = '1' then
		  consumed_word_num := r(0).len;
		else
			consumed_word_num := (others => '0');
		end if;
	  remain_word_num := word_num - ("0" & consumed_word_num);

		case consumed_word_num is
			when "01" =>
			  v(0) := r(1);
				v(1) := r(2);
				v(2) := r(3);
				v(3) := slice_init;
			when "10"	=>
			  v(0) := r(2);
				v(1) := r(3);
				v(2) := slice_init;
				v(3) := slice_init;
			when "11" =>
			  v(0) := r(3);
				v(1) := slice_init;
				v(2) := slice_init;
				v(3) := slice_init;
			when others => null;
		end case;

		case remain_word_num is
			when "000" =>
			  v(0) := in_slice0;
				v(1) := in_slice1;
				read_num <= in_word_num;
			when "001" =>
			  v(1) := in_slice0;
				v(2) := in_slice1;
				read_num <= in_word_num;
			when "010" =>
			  v(2) := in_slice0;
				v(3) := in_slice1;
				read_num <= in_word_num;
			when "011" =>
			  v(3) := in_slice0;
				if in_word_num(1) = '1' then
				  read_num <= "01";
					in_word_num := "01";
				else
					read_num <= in_word_num;
				end if;
			when others =>
			  read_num <= "00";
				in_word_num := "00";
		end case;

		if din.flush = '1' then
			for i in 0 to r'high loop
				v(i).rdy := '0';
				v(i).len := (others => '0');
			end loop;
			next_word_num <= (others => '0');
		else
      next_word_num <= remain_word_num + in_word_num;
		end if;
		next_r <= v;
	end process next_state;

	process(reset, clk)
	begin
		if reset = '1' then
			for i in r'low to r'high loop
				r(i) <= slice_init;
			end loop;
			word_num <= (others => '0');
		elsif rising_edge(clk) then
		  r <= next_r;
		  word_num <= next_word_num;
		end if;
	end process;

	dout.pc <= r(0).pc;
	dout.insn <= r(0).data;
	dout.insn_len <= r(0).len;
	dout.insn_rdy <= r(0).rdy;
	dout.immed1 <= r(1).data;
	dout.immed1_pc <= r(1).pc;
	dout.immed1_rdy <= r(1).rdy;
	dout.immed2 <= r(2).data;
	dout.immed2_pc <= r(2).pc;
	dout.immed2_rdy <= r(2).rdy;
	dout.read_num <= read_num;
end behaviour;
