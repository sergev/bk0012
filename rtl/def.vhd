library IEEE;
use IEEE.STD_LOGIC_1164.all;

package def is
	constant DEVICE_FAMILY: string := "Cyclone III";

	constant WORD_SIZE: natural := 16;
	constant PHYSICAL_ADDRESS_WIDTH: natural := 16;
	constant PC_LOW: natural := 1;

	constant SP_REG:   std_logic_vector(3 downto 0) := "0110";	-- SP register number
	constant PC_REG:   std_logic_vector(3 downto 0) := "0111";	-- PC register number
	constant TEMP1:    std_logic_vector(3 downto 0) := "1000";	-- temporary register 1 number
	constant TEMP2:    std_logic_vector(3 downto 0) := "1001";	-- temporary register 2 number
	constant PSW_REG:  std_logic_vector(3 downto 0) := "1010";	-- immediate virtual register number
	constant PSW_REG2: std_logic_vector(3 downto 0) := "1011";	-- immediate virtual register number
	constant IMM_REG:  std_logic_vector(3 downto 0) := "1101";	-- immediate virtual register number
	constant MEM_REG:  std_logic_vector(3 downto 0) := "1110";	-- memory load virtual register number
	constant ZERO_REG: std_logic_vector(3 downto 0) := "1111";	-- ZERO virtual register number

	constant ROM_START_ADDRESS: std_logic_vector(31 downto 0) := x"00000000";
	constant RAM_START_ADDRESS: std_logic_vector(31 downto 0) := x"10000000";
	constant IO_START_ADDRESS:  std_logic_vector(31 downto 0) := x"80000000";

  function log2(v: in natural) return natural;

	function instruction_len(insn: std_logic_vector(15 downto 0)) return std_logic_vector;
	function has_immediate(op: std_logic_vector(5 downto 0)) return std_logic;

	type address_map_in is record
		address: std_logic_vector(WORD_SIZE-1 downto 0);
	end record;

	type address_map_out is record
		address: std_logic_vector(31 downto 0);
		rom_cs: std_logic;
		ram_cs: std_logic;
		io_cs:  std_logic;
	end record;

	function address_map(ain: address_map_in) return address_map_out;
end package def;

package body def is
  function log2(v: in natural) return natural is
  	variable n: natural;
  	variable logn: natural;
  begin
  	n := 1;
  	for i in 0 to 128 loop
  		logn := i;
  		exit when (n>=v);
  		n := n * 2;
  	end loop;
  	return logn;
  end function log2;

	function has_immediate(op: std_logic_vector(5 downto 0)) return std_logic is
	  alias mode: std_logic_vector(2 downto 0) is op(5 downto 3);
		alias reg:  std_logic_vector(2 downto 0) is op(2 downto 0);
	begin
		if (mode(2 downto 1) = "01" and reg = "111") or
			   mode(2 downto 1) = "11" then
			 return '1';
		else
			return '0';
		end if;
	end function has_immediate;

	function instruction_len(insn: std_logic_vector(15 downto 0)) return std_logic_vector	is
	  variable imm1: std_logic;
		variable imm2: std_logic;
	begin
		imm1 := '0';
		imm2 := '0';
		case insn(14 downto 12) is
			when "000" =>
			  case insn(11 downto 9) is
				  when "000" =>
					  case insn(8 downto 6) is
							when "011" =>
							  if insn(15) = '0' then
							    -- SWAB
								  imm1 := has_immediate(insn(5 downto 0));
								end if;
							when "001" =>
							  if insn(15) = '0' then
							    -- JMP
								  imm1 := has_immediate(insn(5 downto 0));
								end if;
							when others => null;
						end case;
					when "100" =>
					  if insn(15) = '0' then
					    -- JSR
						  imm1 := has_immediate(insn(5 downto 0));
						end if;
					when "101" | "110" =>
					  -- One-operand instructions
						imm1 := has_immediate(insn(5 downto 0));
					when others => null;
				end case;
			when "111" =>
			  if insn(15) = '0' then
					-- One-and-a-half-operand instructions
					imm1 := has_immediate(insn(5 downto 0));
				end if;
			when others =>
			  -- Two-operand instructions
				imm1 := has_immediate(insn(5 downto 0));
				imm2 := has_immediate(insn(11 downto 6));
		end case;
		if imm1 = '1' then
			if imm2 = '1' then
				return "11";
			else
				return "10";
			end if;
		elsif imm2 = '1' then
			return "10";
		else
			return "01";
		end if;
	end function instruction_len;

  function address_map(ain: address_map_in) return address_map_out is
	  variable v: address_map_out;
	begin
		v.rom_cs := '0';
		v.ram_cs := '0';
		v.io_cs := '0';

    -- PDP-11 address map, compatible with ELectronika-60.
    if ain.address(15 downto 13) = "111" then
      -- 160000-177777: system registers
      v.io_cs := '1';
			v.address := IO_START_ADDRESS or (x"0000" & ain.address);
    else
      -- 000000-157777: RAM space
			v.ram_cs := '1';
			v.address := RAM_START_ADDRESS or (x"0000" & ain.address);
		end if;
		return v;
	end function address_map;
end package body def;
