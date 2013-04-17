library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity code_convert is
	port
	(
	  shift: in std_logic;
		lat_rus: in std_logic;
		e0: in std_logic;
		scan_code: in std_logic_vector(7 downto 0);
		
		out_code: out std_logic_vector(6 downto 0)
	);
end code_convert;

architecture behaviour of code_convert is
begin
	convert: process (shift, lat_rus, e0, scan_code)
	  variable vcode: std_logic_vector(7 downto 0);
	begin						 
		vcode := x"00";		 
		if e0 = '1' then
			case scan_code is
				when x"70" => vcode := x"17";      -- Insert
				when x"71" => vcode := x"7F";      -- Delete
				when x"6B" => vcode := x"08";      -- Left Arrow
				when x"6C" => vcode := x"13";      -- Home
			  when x"69" => vcode := x"00";      -- End
				when x"75" => vcode := x"1A";      -- Up Arrow
				when x"72" => vcode := x"1B";      -- Down Arrow
				when x"7D" => vcode := x"00";      -- Page Up
				when x"7A" => vcode := x"00";      -- Page Down
				when x"74" => vcode := x"19";      -- Right Arrow
				when others => null;
			end case;	
		else	
	    case scan_code is
				when x"76" =>  vcode := x"1B";     -- ESC
				when x"05" =>  vcode := x"00";     -- F1
				when x"06" =>  vcode := x"00";     -- F2
				when x"04" =>  vcode := x"00";     -- F3
				when x"0C" =>  vcode := x"00";     -- F4
				when x"03" =>  vcode := x"00";     -- F5
				when x"0B" =>  vcode := x"00";     -- F6
				when x"83" =>  vcode := x"00";     -- F7
				when x"0A" =>	 vcode := x"00";		 -- F8
				when x"01" =>  vcode := x"00";     -- F9
				when x"09" =>	 vcode := x"00";		 -- F10
				when x"78" =>  vcode := x"00";     -- F11
				when x"07" =>  vcode := x"00";     -- F12	
				when x"66" =>  vcode := x"18";     -- Backspace
				when x"0D" =>  vcode := x"09";     -- Tab			
				when x"58" =>  vcode := x"00";     -- Caps Lock
				when x"5A" =>  vcode := x"0A";     -- Enter
				when x"12" =>  vcode := x"00";     -- Left Shift			
				when x"59" =>  vcode := x"00";     -- Right Shift
				when x"14" =>  vcode := x"00";     -- Left Ctrl
				when x"11" =>  vcode := x"00";     -- Left Alt
				when x"29" =>  vcode := x"20";     -- Space Bar
				when others => null;
			end case;
			
			if shift = '1' then
				case scan_code is
					when x"0E" =>  vcode := x"7E";   -- '~'
					when x"16" =>  vcode := x"21";   -- '!'
					when x"1E" =>  vcode := x"40";   -- '@'
					when x"26" =>  vcode := x"23";   -- '#'
					when x"25" =>  vcode := x"24";   -- '$'
					when x"2E" =>  vcode := x"25";   -- '%'
					when x"36" =>  vcode := x"5E";   -- '^'
					when x"3D" =>  vcode := x"26";   -- '&'
					when x"3E" =>  vcode := x"2A";   -- '*'
					when x"46" =>  vcode := x"28";   -- '('
					when x"45" =>  vcode := x"29";   -- ')'
					when x"4E" =>  vcode := x"5F";   -- '_'
					when x"55" =>  vcode := x"2B";   -- '+'
		
					when x"15" =>  vcode := x"71";   -- 'q'
					when x"1D" =>  vcode := x"77";   -- 'w'
					when x"24" =>  vcode := x"65";   -- 'e'
					when x"2D" =>  vcode := x"72";   -- 'r'
					when x"2C" =>  vcode := x"74";   -- 't'
					when x"35" =>  vcode := x"79";   -- 'y'
					when x"3C" =>  vcode := x"75";   -- 'u'
					when x"43" =>  vcode := x"69";   -- 'i'
					when x"44" =>  vcode := x"6F";   -- 'o'
					when x"4D" =>  vcode := x"70";   -- 'p'
					when x"54" =>  vcode := x"7B";   -- '{'
					when x"5B" =>  vcode := x"7D";   -- '}'
					when x"5D" =>  vcode := x"7C";   -- '|'
		
					when x"1C" =>  vcode := x"61";   -- 'a'
					when x"1B" =>  vcode := x"73";   -- 's'
					when x"23" =>  vcode := x"64";   -- 'd'
					when x"2B" =>  vcode := x"66";   -- 'f'
					when x"34" =>  vcode := x"67";   -- 'g'
					when x"33" =>  vcode := x"68";   -- 'h'
					when x"3B" =>  vcode := x"6A";   -- 'j'
					when x"42" =>  vcode := x"6B";   -- 'k'
					when x"4B" =>  vcode := x"6C";   -- 'l'
					when x"4C" =>  vcode := x"3A";   -- ':'
					when x"52" =>  vcode := x"22";   -- '"'
					when x"1A" =>  vcode := x"7A";   -- 'z'
					when x"22" =>  vcode := x"78";   -- 'x'
					when x"21" =>  vcode := x"63";   -- 'c'
					when x"2A" =>  vcode := x"76";   -- 'v'
					when x"32" =>  vcode := x"62";   -- 'b'
					when x"31" =>  vcode := x"6E";   -- 'n'
					when x"3A" =>  vcode := x"6D";   -- 'm'
					when x"41" =>  vcode := x"3C";   -- '<'
					when x"49" =>  vcode := x"3E";   -- '>'
					when x"4A" =>  vcode := x"3F";   -- '?'
																		 
				  when others => null;
		    end case;				
			else	
				case scan_code is
					when x"0E" =>  vcode := x"60";   -- '`'
					when x"16" =>  vcode := x"31";   -- '1'
					when x"1E" =>  vcode := x"32";   -- '2'
					when x"26" =>  vcode := x"33";   -- '3'
					when x"25" =>  vcode := x"34";   -- '4'
					when x"2E" =>  vcode := x"35";   -- '5'
					when x"36" =>  vcode := x"36";   -- '6'
					when x"3D" =>  vcode := x"37";   -- '7'
					when x"3E" =>  vcode := x"38";   -- '8'
					when x"46" =>  vcode := x"39";   -- '9'
					when x"45" =>  vcode := x"30";   -- '0'
					when x"4E" =>  vcode := x"2D";   -- '-'
					when x"55" =>  vcode := x"3D";   -- '='
		
					when x"15" =>  vcode := x"51";   -- 'Q'
					when x"1D" =>  vcode := x"57";   -- 'W'
					when x"24" =>  vcode := x"45";   -- 'E'
					when x"2D" =>  vcode := x"52";   -- 'R'
					when x"2C" =>  vcode := x"54";   -- 'T'
					when x"35" =>  vcode := x"59";   -- 'Y'
					when x"3C" =>  vcode := x"55";   -- 'U'
					when x"43" =>  vcode := x"49";   -- 'I'
					when x"44" =>  vcode := x"4F";   -- 'O'
					when x"4D" =>  vcode := x"50";   -- 'P'
					when x"54" =>  vcode := x"5B";   -- '['
					when x"5B" =>  vcode := x"5D";   -- ']'
					when x"5D" =>  vcode := x"5C";   -- '\'
		
					when x"1C" =>  vcode := x"41";   -- 'A'
					when x"1B" =>  vcode := x"53";   -- 'S'
					when x"23" =>  vcode := x"44";   -- 'D'
					when x"2B" =>  vcode := x"46";   -- 'F'
					when x"34" =>  vcode := x"47";   -- 'G'
					when x"33" =>  vcode := x"48";   -- 'H'
					when x"3B" =>  vcode := x"4A";   -- 'J'
					when x"42" =>  vcode := x"4B";   -- 'K'
					when x"4B" =>  vcode := x"4C";   -- 'L'
					when x"4C" =>  vcode := x"3B";   -- ';'
					when x"52" =>  vcode := x"27";   -- '''
					when x"1A" =>  vcode := x"5A";   -- 'Z'
					when x"22" =>  vcode := x"58";   -- 'X'
					when x"21" =>  vcode := x"43";   -- 'C'
					when x"2A" =>  vcode := x"56";   -- 'V'
					when x"32" =>  vcode := x"42";   -- 'B'
					when x"31" =>  vcode := x"4E";   -- 'N'
					when x"3A" =>  vcode := x"4D";   -- 'M'
					when x"41" =>  vcode := x"2C";   -- ','
					when x"49" =>  vcode := x"2E";   -- '.'
					when x"4A" =>  vcode := x"2F";   -- '/'
					
				  when others => null;
		    end case;	 														 
		  end if;
		end if;	
	  out_code <= vcode(6 downto 0);
	end process convert;
end behaviour;	

	