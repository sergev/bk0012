library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_unsigned.all;
use work.iface.all;

entity alu is
	port
	(
	  din: in alu_in_type;
		dout: out alu_out_type
	);
end alu;

architecture behaviour of alu is
begin
  process (din)
	  variable res: std_logic_vector(15 downto 0);
		variable sum: std_logic_vector(16 downto 0);
		variable vv, vc, vzl, vzh: std_logic;
	begin
		vv := '0';
		vc := '0';

	  case din.oper is
			when alu_add =>
				sum := conv_std_logic_vector((conv_integer(din.dst) + conv_integer(din.src)), 17);
				res := sum(15 downto 0);
			  if din.fbyte = '1' then
					vc := sum(8);
					vv := (din.dst(7) and din.src(7) and not sum(7)) or (not din.dst(7) and not din.src(7) and sum(7));
				else
			    vc := sum(16);
				  vv := (din.dst(15) and din.src(15) and not sum(15)) or (not din.dst(15) and not din.src(15) and sum(15));
				end if;
			when alu_adc =>
			  if din.fbyte = '1' then
				  sum := x"00" & conv_std_logic_vector((conv_integer(din.dst(7 downto 0)) + conv_integer(din.c)), 9);
					res := x"00" & sum(7 downto 0);
					vc := sum(8);
					vv := (din.dst(7) and din.src(7) and not sum(7)) or (not din.dst(7) and not din.src(7) and sum(7));
				else
			    sum := conv_std_logic_vector((conv_integer(din.dst) + conv_integer(din.c)), 17);
				  res := sum(15 downto 0);
					vc := sum(16);
					vv := (din.dst(15) and din.src(15) and not sum(15)) or (not din.dst(15) and not din.src(15) and sum(15));
				end if;
			when alu_sub =>
		    sum := conv_std_logic_vector((conv_integer(din.dst) - conv_integer(din.src)), 17);
			  res := sum(15 downto 0);
			  if din.fbyte = '1' then
					vc := sum(8);
					vv := (din.dst(7) and not din.src(7) and not sum(7)) or (not din.dst(7) and din.src(7) and sum(7));
				else
				  vc := sum(16);
				  vv := (din.dst(15) and not din.src(15) and not sum(15)) or (not din.dst(15) and din.src(15) and sum(15));
				end if;
			when alu_sbc =>
			  if din.fbyte = '1' then
				  sum := x"00" & conv_std_logic_vector((conv_integer(din.dst(7 downto 0)) - conv_integer(din.c)), 9);
					res := x"00" & sum(7 downto 0);
					vc := sum(8);
					vv := (din.dst(7) and not din.src(7) and not sum(7)) or (not din.dst(7) and din.src(7) and sum(7));
				else
			    sum := conv_std_logic_vector((conv_integer(din.dst) - conv_integer(din.c)), 17);
				  res := sum(15 downto 0);
					vc := sum(16);
					vv := (din.dst(15) and not din.src(15) and not sum(15)) or (not din.dst(15) and din.src(15) and sum(15));
				end if;
			when alu_rsb =>
			  sum := conv_std_logic_vector((conv_integer(din.src) - conv_integer(din.dst)), 17);
				res := sum(15 downto 0);
				if din.fbyte = '1' then
					vc := sum(8);
					vv := (not din.dst(7) and din.src(7) and not sum(7)) or (din.dst(7) and not din.src(7) and sum(7));
				else
					vc := sum(16);
					vv := (not din.dst(15) and din.src(15) and not sum(15)) or (din.dst(15) and not din.src(15) and sum(15));
				end if;
			when alu_ror =>
			  if din.fbyte = '1' then
					res := din.dst(15 downto 8) & din.c & din.dst(7 downto 1);
					vv := res(7) xor din.dst(0);
				else
					res := din.c & din.dst(15 downto 1);
					vv := res(15) xor din.dst(0);
				end if;
				vc := din.dst(0);
		  when alu_rol =>
			  if din.fbyte = '1' then
					res := din.dst(15 downto 8) & din.dst(6 downto 0) & din.c;
					vc := din.dst(7);
					vv := res(7) xor din.dst(7);
				else
					res := din.dst(14 downto 0) & din.c;
					vc := din.dst(15);
					vv := res(15) xor din.dst(15);
				end if;
			when alu_asr =>
			  if din.fbyte = '1' then
					res := din.dst(15 downto 8) & din.dst(7) & din.dst(7 downto 1);
					vv := res(7) xor din.dst(0);
				else
					res := din.dst(15) & din.dst(15 downto 1);
					vv := res(15) xor din.dst(0);
				end if;
				vc := din.dst(0);
			when alu_asl =>
			  res := din.dst(14 downto 0) & '0';
				if din.fbyte = '1' then
					vc := din.dst(7);
					vv := res(7) xor din.dst(7);
				else
				  vc := din.dst(15);
					vv := res(15) xor din.dst(15);
				end if;
			when alu_sxt =>
			  if din.n = '1' then
					res := (others => '1');
				else
					res := (others => '0');
				end if;
			when alu_bit =>
			  res := din.src and din.dst;
			when alu_bic =>
			  res := (not din.src) and din.dst;
			when alu_bis =>
			  res := din.src or din.dst;
			when alu_xor =>
				res := din.src xor din.dst;
			when alu_swab =>
			  res := din.dst(7 downto 0) & din.dst(15 downto 8);
				vc := '0';
			when alu_movb =>
			  res(7 downto 0) := din.src(7 downto 0);
				if din.src(7) = '1' then
					res(15 downto 8) := (others => '1');
				else
					res(15 downto 8) := (others => '0');
				end if;
		end case;

		if res(7 downto 0) = 0 then
			vzl := '1';
		else
			vzl := '0';
		end if;
		if res(15 downto 8) = 0 then
			vzh := '1';
		else
			vzh := '0';
		end if;
		if din.fbyte = '1' then
			dout.n <= res(7);
			dout.z <= vzl;
		else
			dout.n <= res(15);
			dout.z <= vzh and vzl;
		end if;

		dout.result(7 downto 0) <= res(7 downto 0);
		if din.oper = alu_movb or din.fbyte = '0' then
			dout.result(15 downto 8) <= res(15 downto 8);
		else
			dout.result(15 downto 8) <= (others => '0');
		end if;

		case din.n_oper is
			when fl_clear =>
			  dout.n <= '0';
			when fl_set =>
			  dout.n <= '1';
			when fl_alu =>
				if din.fbyte = '1' or din.oper = alu_swab then
					dout.n <= res(7);
				else
					dout.n <= res(15);
				end if;
			when others =>
				dout.n <= din.n;
		end case;

		case din.z_oper is
			when fl_clear =>
			  dout.z <= '0';
			when fl_set =>
			  dout.z <= '1';
			when fl_alu =>
				if din.fbyte = '1' or din.oper = alu_swab then
					dout.z <= vzl;
				else
					dout.z <= vzh and vzl;
				end if;
			when others =>
				dout.z <= din.z;
		end case;

		case din.v_oper is
			when fl_clear =>
			  dout.v <= '0';
			when fl_set =>
			  dout.v <= '1';
			when fl_alu =>
			  dout.v <= vv;
			when others =>
				dout.v <= din.v;
		end case;

		case din.c_oper is
			when fl_clear =>
			  dout.c <= '0';
			when fl_set =>
			  dout.c <= '1';
			when fl_alu =>
			  dout.c <= vc;
			when others =>
				dout.c <= din.c;
		end case;
	end process;

end behaviour;
