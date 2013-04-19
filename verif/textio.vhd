library IEEE;
use IEEE.std_logic_1164.all;
use std.textio.all;

package textio is

	procedure hwrite(L: inout line; value: in std_logic_vector;
			justified: in side := RIGHT; field: in width := 0);

	procedure owrite(L: inout line; value: in std_logic_vector;
			justified: in side := RIGHT; field: in width := 0);

	procedure print(message: string);
	procedure print(message: string; value: std_logic_vector);

end textio;

package body textio is

	procedure hwrite(
    L         : inout line;
    value     : in    std_logic_vector;
		justified : in    side := RIGHT;
    field     : in    width := 0
  ) is
		variable quad : std_logic_vector(0 to 3);
		constant ne   : integer := value'length/4;
		variable bv   : std_logic_vector(0 to value'length-1) := value;
		variable s    : string(1 to ne);
	begin
		if value'length mod 4 /= 0 then
			assert FALSE report "HWRITE Error: vector length not multiple of 4";
			return;
		end if;

		for i in 0 to ne-1 loop
			quad := bv(4*i to 4*i+3);
			case quad is
				when x"0"   => s(i+1) := '0';
				when x"1"   => s(i+1) := '1';
				when x"2"   => s(i+1) := '2';
				when x"3"   => s(i+1) := '3';
				when x"4"   => s(i+1) := '4';
				when x"5"   => s(i+1) := '5';
				when x"6"   => s(i+1) := '6';
				when x"7"   => s(i+1) := '7';
				when x"8"   => s(i+1) := '8';
				when x"9"   => s(i+1) := '9';
				when x"A"   => s(i+1) := 'A';
				when x"B"   => s(i+1) := 'B';
				when x"C"   => s(i+1) := 'C';
				when x"D"   => s(i+1) := 'D';
				when x"E"   => s(i+1) := 'E';
				when x"F"   => s(i+1) := 'F';
				when "---U" => s(i+1) := 'u';
				when "--U-" => s(i+1) := 'u';
				when "-U--" => s(i+1) := 'u';
				when "U---" => s(i+1) := 'u';
				when "---X" => s(i+1) := 'x';
				when "--X-" => s(i+1) := 'x';
				when "-X--" => s(i+1) := 'x';
				when "X---" => s(i+1) := 'x';
				when others => s(i+1) := '?';
			end case;
		end loop;
		write(L, s, justified, field);
	end hwrite;

	procedure owrite(
    L         : inout line;
    value     : in    std_logic_vector;
		justified : in    side := RIGHT;
    field     : in    width := 0
  ) is
		variable tri : std_logic_vector(0 to 2);
		constant ne  : integer := value'length/3;
		variable bv  : std_logic_vector(0 to value'length-1) := value;
		variable s   : string(1 to ne);
	begin
		if value'length mod 3 /= 0 then
			assert FALSE report "OWRITE Error: vector length not multiple of 3";
			return;
		end if;

		for i in 0 to ne-1 loop
			tri := bv(3*i to 3*i+2);
			case tri is
				when o"0"   => s(i+1) := '0';
				when o"1"   => s(i+1) := '1';
				when o"2"   => s(i+1) := '2';
				when o"3"   => s(i+1) := '3';
				when o"4"   => s(i+1) := '4';
				when o"5"   => s(i+1) := '5';
				when o"6"   => s(i+1) := '6';
				when o"7"   => s(i+1) := '7';
				when "--U"  => s(i+1) := 'u';
				when "-U-"  => s(i+1) := 'u';
				when "U--"  => s(i+1) := 'u';
				when "--X"  => s(i+1) := 'x';
				when "-X-"  => s(i+1) := 'x';
				when "X--"  => s(i+1) := 'x';
				when others => s(i+1) := '?';
			end case;
		end loop;
		write(L, s, justified, field);
	end owrite;

	procedure print(message: string) is
    variable L : line;
  begin
    write(L, message);
    writeline(output, L);
  end print;

	procedure print(
    message : string;
    value   : std_logic_vector
  ) is
		constant ne : integer := value'length;
		variable bv : std_logic_vector(0 to ne-1) := value;
		variable s  : string(1 to ne);
    variable L : line;
	begin
		for i in 0 to ne-1 loop
			case bv(i) is
				when '0' => s(i+1) := '0';
				when '1' => s(i+1) := '1';
				when 'U' => s(i+1) := 'u';
				when 'X' => s(i+1) := 'x';
				when others => s(i+1) := '?';
			end case;
		end loop;
    write(L, message);
		write(L, s);
    writeline(output, L);
	end print;

end textio;
