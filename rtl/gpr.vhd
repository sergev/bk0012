library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.def.all;
use work.iface.all;

entity gpr is
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		din: in gpr_in_type;
		dout: out gpr_out_type;
		trace_value: out word_vector(0 to 6);
		trace_mask: out std_logic_vector(0 to 6)
	);
end gpr;

architecture behaviour of gpr is
  signal r0, r1, r2, r3, r4, r5: std_logic_vector(WORD_SIZE-1 downto 0);
	signal sp: std_logic_vector(WORD_SIZE-1 downto 0);
	signal tmp1, tmp2: std_logic_vector(WORD_SIZE-1 downto 0);
begin
	write_regs: process (reset, clk)
	  variable v_r0, v_r1, v_r2, v_r3, v_r4, v_r5: std_logic_vector(15 downto 0);
	  variable v_sp: std_logic_vector(15 downto 0);
	  variable v_tmp1, v_tmp2: std_logic_vector(15 downto 0);
		variable t_value:  word_vector(0 to 6);
		variable t_mask: std_logic_vector(0 to 6);
	begin
		if reset = '1' then
			r0 <= (others => '0');
			r1 <= (others => '0');
			r2 <= (others => '0');
			r3 <= (others => '0');
			r4 <= (others => '0');
			r5 <= (others => '0');
			sp <= (others => '0');
			tmp1 <= (others => '0');
			tmp2 <= (others => '0');
			t_value := (others => x"0000");
		elsif rising_edge(clk) then
			v_r0 := r0;
			v_r1 := r1;
			v_r2 := r2;
			v_r3 := r3;
			v_r4 := r4;
			v_r5 := r5;
			v_sp := sp;
			v_tmp1 := tmp1;
			v_tmp2 := tmp2;
      t_mask := "0000000";
			if din.we1 = '1' then
				case din.wr_address1 is
					when "0000" =>
					  v_r0(7 downto 0) := din.wr_data1(7 downto 0);
						if (din.wr_byte1 = '0') then
							v_r0(15 downto 8) := din.wr_data1(15 downto 8);
						end if;
						t_value(0) := v_r0;
						t_mask(0) := '1';
					when "0001" =>
					  v_r1(7 downto 0) := din.wr_data1(7 downto 0);
						if (din.wr_byte1 = '0') then
							v_r1(15 downto 8) := din.wr_data1(15 downto 8);
						end if;
						t_value(1) := v_r1;
						t_mask(1) := '1';
					when "0010" =>
					  v_r2(7 downto 0) := din.wr_data1(7 downto 0);
						if (din.wr_byte1 = '0') then
							v_r2(15 downto 8) := din.wr_data1(15 downto 8);
						end if;
						t_value(2) := v_r2;
						t_mask(2) := '1';
					when "0011" =>
					  v_r3(7 downto 0) := din.wr_data1(7 downto 0);
						if (din.wr_byte1 = '0') then
							v_r3(15 downto 8) := din.wr_data1(15 downto 8);
						end if;
						t_value(3) := v_r3;
						t_mask(3) := '1';
					when "0100" =>
					  v_r4(7 downto 0) := din.wr_data1(7 downto 0);
						if (din.wr_byte1 = '0') then
							v_r4(15 downto 8) := din.wr_data1(15 downto 8);
						end if;
						t_value(4) := v_r4;
						t_mask(4) := '1';
					when "0101" =>
					  v_r5(7 downto 0) := din.wr_data1(7 downto 0);
						if (din.wr_byte1 = '0') then
							v_r5(15 downto 8) := din.wr_data1(15 downto 8);
						end if;
						t_value(5) := v_r5;
						t_mask(5) := '1';
					when "0110" =>
            v_sp := din.wr_data1;
						t_value(6) := v_sp;
						t_mask(6) := '1';
					when TEMP1 =>  v_tmp1 := din.wr_data1;
					when TEMP2 =>  v_tmp2 := din.wr_data1;
					when others => null;
				end case;
			end if;
			if din.we2 = '1' then
				case din.wr_address2 is
					when "0000" =>
					  v_r0 := din.wr_data2;
						t_value(0) := v_r0;
						t_mask(0) := '1';
					when "0001" =>
					  v_r1 := din.wr_data2;
						t_value(1) := v_r1;
						t_mask(1) := '1';
					when "0010" =>
					  v_r2 := din.wr_data2;
						t_value(2) := v_r2;
						t_mask(2) := '1';
					when "0011" =>
					  v_r3 := din.wr_data2;
						t_value(3) := v_r3;
						t_mask(3) := '1';
					when "0100" =>
					  v_r4 := din.wr_data2;
						t_value(4) := v_r4;
						t_mask(4) := '1';
					when "0101" =>
					  v_r5 := din.wr_data2;
						t_value(5) := v_r5;
						t_mask(5) := '1';
					when "0110" =>
					  v_sp := din.wr_data2;
						t_value(6) := v_sp;
						t_mask(6) := '1';
					when TEMP1 =>
					  v_tmp1 := din.wr_data2;
					when TEMP2 =>
					  v_tmp2 := din.wr_data2;
					when others => null;
				end case;
			end if;
			r0 <= v_r0;
			r1 <= v_r1;
			r2 <= v_r2;
			r3 <= v_r3;
			r4 <= v_r4;
			r5 <= v_r5;
			sp <= v_sp;
			tmp1 <= v_tmp1;
			tmp2 <= v_tmp2;
			trace_value <= t_value;
      trace_mask <= t_mask;
		end if;
	end process write_regs;

	read_regs: process (din, r0, r1, r2, r3, r4, r5, sp, tmp1, tmp2)
	begin
		case din.rd_address1 is
			when "0000" =>  dout.rd_data1 <= r0;
			when "0001" =>  dout.rd_data1 <= r1;
			when "0010" =>  dout.rd_data1 <= r2;
			when "0011" =>  dout.rd_data1 <= r3;
			when "0100" =>  dout.rd_data1 <= r4;
			when "0101" =>  dout.rd_data1 <= r5;
			when "0110" =>  dout.rd_data1 <= sp;
			when "0111" =>  dout.rd_data1 <= din.pc1;
			when TEMP1 =>   dout.rd_data1 <= tmp1;
			when TEMP2 =>   dout.rd_data1 <= tmp2;
			when PSW_REG => dout.rd_data1 <= din.psw;
			when IMM_REG => dout.rd_data1 <= din.immed1;
			when MEM_REG => dout.rd_data1 <= din.mem_data;
			when others =>  dout.rd_data1 <= (others => '0');
		end case;

		case din.rd_address2 is
			when "0000" =>  dout.rd_data2 <= r0;
			when "0001" =>  dout.rd_data2 <= r1;
			when "0010" =>  dout.rd_data2 <= r2;
			when "0011" =>  dout.rd_data2 <= r3;
			when "0100" =>  dout.rd_data2 <= r4;
			when "0101" =>  dout.rd_data2 <= r5;
			when "0110" =>  dout.rd_data2 <= sp;
			when "0111" =>  dout.rd_data2 <= din.pc2;
			when TEMP1 =>   dout.rd_data2 <= tmp1;
			when TEMP2 =>   dout.rd_data2 <= tmp2;
			when IMM_REG => dout.rd_data2 <= din.immed2;
			when MEM_REG => dout.rd_data2 <= din.mem_data;
			when others =>  dout.rd_data2 <= (others => '0');
		end case;

		case din.rd_address3 is
			when "0000" =>  dout.rd_data3 <= r0;
			when "0001" =>  dout.rd_data3 <= r1;
			when "0010" =>  dout.rd_data3 <= r2;
			when "0011" =>  dout.rd_data3 <= r3;
			when "0100" =>  dout.rd_data3 <= r4;
			when "0101" =>  dout.rd_data3 <= r5;
			when "0110" =>  dout.rd_data3 <= sp;
			when "0111" =>  dout.rd_data3 <= din.pc3;
			when TEMP1 =>   dout.rd_data3 <= tmp1;
			when TEMP2 =>   dout.rd_data3 <= tmp2;
			when IMM_REG => dout.rd_data3 <= din.immed3;
			when others =>  dout.rd_data3 <= (others => '0');
		end case;

		case din.dbg_address is
			when "0000" =>  dout.dbg_data <= r0;
			when "0001" =>  dout.dbg_data <= r1;
			when "0010" =>  dout.dbg_data <= r2;
			when "0011" =>  dout.dbg_data <= r3;
			when "0100" =>  dout.dbg_data <= r4;
			when "0101" =>  dout.dbg_data <= r5;
			when "0110" =>  dout.dbg_data <= sp;
			when TEMP1 =>   dout.dbg_data <= tmp1;
			when TEMP2 =>   dout.dbg_data <= tmp2;
			when PSW_REG => dout.dbg_data <= din.psw;
			when others =>  dout.dbg_data <= (others => '0');
		end case;

	end process read_regs;

end behaviour;
