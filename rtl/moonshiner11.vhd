library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_ARITH.all;
use work.config.all;
use work.def.all;
use work.iface.all;

entity moonshiner11 is
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		reset_out: out std_logic;
		int_reset_out: out std_logic;

		-- interrupt controller interface
    irq: in std_logic := '0';
    ivec: in std_logic_vector(7 downto 0):= x"48";
		iack: out std_logic;

		-- Instruction cache Avalon Master
		avm_icache_address: out std_logic_vector(31 downto 0);
		avm_icache_burstcount: out std_logic_vector(log2(ICACHE_LINE_SIZE) downto 0);
		avm_icache_read: out std_logic;
		avm_icache_flush: out std_logic;
		avm_icache_readdata: in std_logic_vector(ICACHE_DATA_WIDTH-1 downto 0);
		avm_icache_readdatavalid: in std_logic;
		avm_icache_waitrequest: in std_logic;

		-- Data cache Avalon Master
		avm_dcache_address: out std_logic_vector(31 downto 0);
		avm_dcache_burstcount: out std_logic_vector(log2(DCACHE_LINE_SIZE) downto 0);
		avm_dcache_read: out std_logic;
		avm_dcache_readdata: in std_logic_vector(DCACHE_DATA_WIDTH-1 downto 0);
		avm_dcache_readdatavalid: in std_logic;
		avm_dcache_write: out std_logic;
		avm_dcache_writedata: out std_logic_vector(DCACHE_DATA_WIDTH-1 downto 0);
		avm_dcache_byteenable: out std_logic_vector(DCACHE_DATA_WIDTH/8 -1 downto 0);
		avm_dcache_waitrequest: in std_logic;

		-- I/O Avalon Master
		avm_io_address: out std_logic_vector(31 downto 0);
		avm_io_read: out std_logic;
		avm_io_readdata: in std_logic_vector(DCACHE_DATA_WIDTH-1 downto 0);
		avm_io_readdatavalid: in std_logic;
		avm_io_write: out std_logic;
		avm_io_writedata: out std_logic_vector(DCACHE_DATA_WIDTH-1 downto 0);
		avm_io_byteenable: out std_logic_vector(DCACHE_DATA_WIDTH/8 -1 downto 0);
		avm_io_waitrequest: in std_logic;

		-- Hardware Debugger Avalon Slave
		avs_dbg_address: in std_logic_vector(8 downto 0):= (others => '0');
		avs_dbg_writedata: in std_logic_vector(15 downto 0):= (others => '0');
		avs_dbg_chipselect: in std_logic:= '0';
		avs_dbg_read: in std_logic:= '0';
		avs_dbg_write: in std_logic:= '0';
		avs_dbg_readdata: out std_logic_vector(15 downto 0);

		-- trace interface, for verification only
		trace: out trace_type
	);
end moonshiner11;

architecture behaviour of moonshiner11 is
--  constant START_ADDRESS: std_logic_vector(15 downto 0) := "0" & o"00530"; --  x"8000";  -- "0" & o"00502"; -- & o"00530";
--  constant START_ADDRESS: std_logic_vector(15 downto 0) := x"8000";
  constant START_ADDRESS: std_logic_vector(15 downto 0) := x"0080";

  type IF_stage_in_type is record
		pc_incr: std_logic_vector(1 downto 0);
		branch: std_logic;
		branch_address: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		jump: std_logic;
		uncond_branch_address: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		uncond_branch: std_logic;
	end record;

	type IF_stage_type is record
		pc: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		rdy: std_logic;
		jump: std_logic;
	end record;

	type ID_stage_type is record
	  insn: insn_trace_type;
	end record;

	type alu_cmd_type is record
		src_reg: std_logic_vector(3 downto 0);
		src_immed: std_logic_vector(WORD_SIZE-1 downto 0);
		dst_reg: std_logic_vector(3 downto 0);
		dst_immed: std_logic_vector(WORD_SIZE-1 downto 0);
		alu_oper: alu_oper_type;
		n_oper: flag_oper_type;
		z_oper: flag_oper_type;
		v_oper: flag_oper_type;
		c_oper: flag_oper_type;
		fbyte: std_logic;
	end record;

	function alu_cmd_empty return alu_cmd_type is
	  variable v: alu_cmd_type;
	begin
		v.src_reg := (others => '0');
		v.src_immed := (others => '0');
		v.dst_reg := (others => '0');
		v.dst_immed := (others => '0');
		v.alu_oper := alu_add;
		v.n_oper := fl_disable;
		v.z_oper := fl_disable;
		v.v_oper := fl_disable;
		v.c_oper := fl_disable;
		v.fbyte := '0';
		return v;
	end function alu_cmd_empty;

--	function get_extended_reg_name(reg: std_logic_vector(3 downto 0);
--	  imm_str: string; mem_str: string) return string is
--	begin
--	  if reg(3) = '0' then
--			return getRegisterName(reg(2 downto 0));
--		else
--			case reg is
--				when TEMP1 => return "TEMP1";
--				when TEMP2 => return "TEMP2";
--				when PSW_REG => return "PSW";
--				when PSW_REG2 => return "PSW2";
--				when MEM_REG => return mem_str;
--				when IMM_REG => return imm_str;
--				when ZERO_REG => return "ZERO";
--				when others => return "???";
--			end case;
--		end if;
--	end function get_extended_reg_name;

--	function get_extended_reg_name(reg: std_logic_vector(3 downto 0)) return string is
--	begin
--		return get_extended_reg_name(reg, "IMM", "MEM");
--	end function get_extended_reg_name;

--	function size_toString(fbyte: std_logic) return string is
--	begin
--		if fbyte = '1' then
--			return ".B";
--		else
--			return "";
--		end if;
--	end function size_toString;

--	function alu_cmd_toString(alu_cmd: alu_cmd_type; mem_str: string) return string is
--	begin
--	  return alu_oper_toString(alu_cmd.alu_oper) & "(" &
--		  get_extended_reg_name(alu_cmd.src_reg, toHexString(alu_cmd.src_immed), mem_str) & "," &
--		  get_extended_reg_name(alu_cmd.dst_reg, toHexString(alu_cmd.dst_immed), mem_str) &
--		")";
--	end function alu_cmd_toString;

	type agu_cmd_type is record
		base: std_logic_vector(WORD_SIZE-1 downto 0);
		offset: std_logic_vector(WORD_SIZE-1 downto 0);
		load_areg: std_logic;
		fbyte: std_logic;
		rd: std_logic;
		base_reg: std_logic_vector(3 downto 0);
		reg_post_modify: std_logic;
		reg_we: std_logic;
	end record;

	function agu_cmd_empty return agu_cmd_type is
		variable v: agu_cmd_type;
	begin
		v.base := (others => '0');
		v.offset := (others => '0');
		v.load_areg := '0';
		v.fbyte := '0';
		v.rd := '0';
		v.base_reg := (others => '0');
		v.reg_post_modify := '0';
		v.reg_we := '0';
		return v;
	end function agu_cmd_empty;

--	function agu_cmd_toOperandString(agu_cmd: agu_cmd_type) return string is
--	begin
--		if agu_cmd.reg_post_modify = '1' then
--			if (agu_cmd.fbyte = '1' and agu_cmd.offset = x"0001") or
--				 (agu_cmd.fbyte = '0' and agu_cmd.offset = x"0002") then
--				return "-(" & get_extended_reg_name(agu_cmd.base_reg) & ")+";
--			else
--				return "???";
--			end if;
--		else
--			if agu_cmd.base_reg = ZERO_REG then
--				return "@" & toHexString(agu_cmd.offset);
--			else
--				if agu_cmd.offset = x"FFFF" then
--					return "-(" & get_extended_reg_name(agu_cmd.base_reg) & ")";
--				elsif agu_cmd.offset = x"0000" then
--					return "(" & get_extended_reg_name(agu_cmd.base_reg) & ")";
--				else
--					return toDecString(CONV_INTEGER(agu_cmd.offset)) & "(" & get_extended_reg_name(agu_cmd.base_reg) & ")";
--				end if;
--			end if;
--		end if;
--	end function agu_cmd_toOperandString;

	type mem_cmd_type is record
		address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		fbyte: std_logic;
		rd: std_logic;
	end record;

	function mem_cmd_empty return mem_cmd_type is
	  variable v: mem_cmd_type;
	begin
		v.address := (others => '0');
		v.fbyte := '0';
		v.rd := '0';
		return v;
	end function mem_cmd_empty;

	type alu_wb_cmd_type is record
		fbyte: std_logic;
		reg: std_logic_vector(3 downto 0);
		reg_we: std_logic;
		mem_we: std_logic;
	end record;

	function alu_wb_cmd_empty return alu_wb_cmd_type is
	  variable v: alu_wb_cmd_type;
	begin
		v.fbyte := '0';
		v.reg := (others => '0');
		v.reg_we := '0';
		v.mem_we := '0';
		return v;
	end function alu_wb_cmd_empty;

--	function alu_wb_cmd_toString(wb: alu_wb_cmd_type) return string is
--	begin
--		if wb.reg_we = '1' then
--			return get_extended_reg_name(wb.reg) & size_toString(wb.fbyte) & "=";
--		elsif wb.mem_we = '1' then
--			return "MEM" & size_ToString(wb.fbyte) & "=";
--		else
--			return "";
--		end if;
--	end function alu_wb_cmd_toString;

	type agu_wb_cmd_type is record
		value: std_logic_vector(WORD_SIZE-1 downto 0);
		base_reg: std_logic_vector(3 downto 0);
		reg_we: std_logic;
	end record;

	function agu_wb_cmd_empty return agu_wb_cmd_type is
		variable v: agu_wb_cmd_type;
	begin
		v.value := (others => '0');
		v.base_reg := (others => '0');
		v.reg_we := '0';
		return v;
	end function agu_wb_cmd_empty;

	subtype branch_cond_type is std_logic_vector(3 downto 0);
	type jump_cmd_type is record
		jump: std_logic;
		branch: std_logic;
		sob: std_logic;
		branch_cond: branch_cond_type;
		branch_address: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
	end record;

	function jump_cmd_empty return jump_cmd_type is
	  variable v: jump_cmd_type;
	begin
		v.jump := '0';
		v.branch := '0';
		v.sob := '0';
		v.branch_cond := (others => '0');
		v.branch_address := (others => '0');
		return v;
	end function jump_cmd_empty;

--	function branch_cond_toString(cond: branch_cond_type) return string is
--	begin
--		if cond(3) = '1' then
--			case cond(2 downto 0) is
--				when "000" =>  -- BPL
--				  return "BPL";
--				when "001" =>  -- BMI
--				  return "BMI";
--				when "010" =>	 -- BHI
--				  return "BHI";
--				when "011" =>	 -- BLOS
--				  return "BLOS";
--				when "100" =>	 --	BVC
--				  return "BVC";
--				when "101" =>	 -- BVS
--				  return "BVS";
--				when "110" =>	 -- BHIS/BCC
--				  return "BCC";
--				when "111" =>		-- BLO/ BCS
--				  return "BCS";
--				when others => return "???";
--			end case;
--		else
--			case cond(2 downto 0) is
--				when "001" =>  -- BR
--				  return "BR";
--				when "010" =>  -- BNE
--				  return "BNE";
--				when "011" =>	 -- BEQ
--				  return "BEQ";
--				when "100" =>	 -- BGE
--				  return "BGE";
--				when "101" =>	 -- BLT
--				  return "BLT";
--				when "110" =>	 -- BGT
--				  return "BGT";
--				when "111" =>	 -- BLE
--				  return "BLE";
--				when others => return "???";
--			end case;
--		end if;
--	end function branch_cond_toString;

--	function jump_cmd_toString(jump_cmd: jump_cmd_type; agu_cmd: agu_cmd_type) return string is
--	begin
--		if jump_cmd.jump = '1' then
--			return "JUMP " & agu_cmd_toOperandString(agu_cmd);
--		elsif jump_cmd.branch = '1' then
--			if jump_cmd.sob = '1' then
--				return "SOB " & toHexString(jump_cmd.branch_address & '0');
--			else
--			  return branch_cond_toString(jump_cmd.branch_cond) & " " & toHexString(jump_cmd.branch_address & '0');
--			end if;
--		else
--			return "";
--		end if;
--	end jump_cmd_toString;

	type ID2_stage_type is record
	  insn: insn_trace_type;
		bubble: std_logic;
		alu_cmd: alu_cmd_type;
		agu_cmd: agu_cmd_type;
		alu_wb_cmd: alu_wb_cmd_type;
		jump_cmd: jump_cmd_type;
	end record;

	function ID2_empty return ID2_stage_type is
	  variable v: ID2_stage_type;
	begin
		v.insn.pc := (others => '0');
		v.insn.rdy := '0';
		v.bubble := '0';
		v.alu_cmd := alu_cmd_empty;
		v.agu_cmd := agu_cmd_empty;
		v.alu_wb_cmd := alu_wb_cmd_empty;
		v.jump_cmd := jump_cmd_empty;
		return v;
	end function ID2_empty;

--	function ID2_toString(id2: ID2_stage_type) return string is
--	begin
--		if id2.jump_cmd.sob = '1' then
--			return jump_cmd_toString(id2.jump_cmd, id2.agu_cmd) & " || " &
--			  alu_wb_cmd_toString(id2.alu_wb_cmd) &
--			  alu_cmd_toString(id2.alu_cmd, agu_cmd_toOperandString(id2.agu_cmd));
--		elsif id2.jump_cmd.jump = '1' or id2.jump_cmd.branch = '1' then
--			return jump_cmd_toString(id2.jump_cmd, id2.agu_cmd);
--		else
--		  return alu_wb_cmd_toString(id2.alu_wb_cmd) &
--			  alu_cmd_toString(id2.alu_cmd, agu_cmd_toOperandString(id2.agu_cmd));
--		end if;
--	end function ID2_toString;

	type AG_stage_type is record
	  insn: insn_trace_type;
		alu_cmd: alu_cmd_type;
		agu_cmd: agu_cmd_type;
		alu_wb_cmd: alu_wb_cmd_type;
		jump_cmd: jump_cmd_type;
	end record;

	function AG_empty return AG_stage_type is
	  variable v: AG_stage_type;
	begin
		v.insn.pc := (others => '0');
		v.insn.rdy := '0';
		v.alu_cmd := alu_cmd_empty;
		v.agu_cmd := agu_cmd_empty;
		v.alu_wb_cmd := alu_wb_cmd_empty;
		v.jump_cmd := jump_cmd_empty;
		return v;
	end function AG_empty;

--	function AG_toString(ag: AG_stage_type) return string is
--	begin
--		if ag.jump_cmd.sob = '1' then
--			return jump_cmd_toString(ag.jump_cmd, ag.agu_cmd) & " || " &
--			  alu_wb_cmd_toString(ag.alu_wb_cmd) &
--			  alu_cmd_toString(ag.alu_cmd, agu_cmd_toOperandString(ag.agu_cmd));
--		elsif ag.jump_cmd.jump = '1' or ag.jump_cmd.branch = '1' then
--			return jump_cmd_toString(ag.jump_cmd, ag.agu_cmd);
--		else
--		  return alu_wb_cmd_toString(ag.alu_wb_cmd) &
--			  alu_cmd_toString(ag.alu_cmd, agu_cmd_toOperandString(ag.agu_cmd));
--		end if;
--	end function AG_toString;

	type MEM_stage_type is record
	  insn: insn_trace_type;
		alu_cmd: alu_cmd_type;
		mem_cmd: mem_cmd_type;
		alu_wb_cmd: alu_wb_cmd_type;
		agu_wb_cmd: agu_wb_cmd_type;
		jump_cmd: jump_cmd_type;
	end record;

	function MEM_empty return MEM_stage_type is
	  variable v: MEM_stage_type;
	begin
		v.insn.pc := (others => '0');
		v.insn.rdy := '0';
		v.alu_cmd := alu_cmd_empty;
		v.mem_cmd := mem_cmd_empty;
		v.alu_wb_cmd := alu_wb_cmd_empty;
		v.agu_wb_cmd := agu_wb_cmd_empty;
		v.jump_cmd := jump_cmd_empty;
		return v;
	end function MEM_empty;

	type EX_stage_type is record
	  insn: insn_trace_type;
		mem_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		alu_cmd: alu_cmd_type;
		alu_wb_cmd: alu_wb_cmd_type;
		agu_wb_cmd: agu_wb_cmd_type;
		src: std_logic_vector(WORD_SIZE-1 downto 0);
		dst: std_logic_vector(WORD_SIZE-1 downto 0);
		jump_cmd: jump_cmd_type;
	end record;

	function EX_empty return EX_stage_type is
	  variable v: EX_stage_type;
	begin
		v.insn.pc := (others => '0');
		v.insn.rdy := '0';
		v.mem_address := (others => '0');
		v.alu_cmd := alu_cmd_empty;
		v.alu_wb_cmd := alu_wb_cmd_empty;
		v.agu_wb_cmd := agu_wb_cmd_empty;
		v.jump_cmd := jump_cmd_empty;
		v.src := (others => '0');
		v.dst := (others => '0');
		return v;
	end function EX_empty;

	type WB_stage_type is record
	  insn: insn_trace_type;
    mem_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		mem_data: std_logic_vector(WORD_SIZE-1 downto 0);
		mem_byteen: std_logic_vector(1 downto 0);
		alu_wb_cmd: alu_wb_cmd_type;
		agu_wb_cmd: agu_wb_cmd_type;
	end record;

	function WB_empty return WB_stage_type is
	  variable v: WB_stage_type;
	begin
		v.insn.pc := (others => '0');
		v.insn.rdy := '0';
		v.mem_address := (others => '0');
		v.mem_data := (others => '0');
		v.mem_byteen := (others => '0');
		v.alu_wb_cmd := alu_wb_cmd_empty;
		v.agu_wb_cmd := agu_wb_cmd_empty;
		return v;
	end function WB_empty;

	function is_branch_taken(cond: branch_cond_type;
	    N: std_logic; Z: std_logic; V: std_logic; C: std_logic) return std_logic is
	begin
		if cond(3) = '1' then
			case cond(2 downto 0) is
				when "000" =>  -- BPL
				  return not N;
				when "001" =>  -- BMI
				  return N;
				when "010" =>	 -- BHI
				  return not (C or Z);
				when "011" =>	 -- BLOS
				  return C or Z;
				when "100" =>	 --	BVC
				  return not V;
				when "101" =>	 -- BVS
				  return V;
				when "110" =>	 -- BHIS/BCC
				  return not C;
				when "111" =>		-- BLO/ BCS
				  return C;
				when others => return '0';
			end case;
		else
			case cond(2 downto 0) is
				when "001" =>  -- BR
				  return '1';
				when "010" =>  -- BNE
				  return not Z;
				when "011" =>	 -- BEQ
				  return Z;
				when "100" =>	 -- BGE
				  return not(N xor V);
				when "101" =>	 -- BLT
				  return N xor V;
				when "110" =>	 -- BGT
				  return not(Z or (N xor V));
				when "111" =>	 -- BLE
				  return Z or (N xor V);
				when others => return '0';
			end case;
		end if;
	end function is_branch_taken;

	type ID_state_type is
	(
	  st_start,
		st_load_dst,
		st_load_src_deferred,
		st_alu,
		st_branch,
		st_jsr1, st_jsr2,
		st_rts,
		st_rti,
		st_mark1, st_mark2,
		st_trap1, st_trap2, st_trap3,
		st_halt,
		st_irq1, st_irq2, st_irq3,
		st_tirq1, st_tirq2, st_tirq3,
		st_branch_psw, st_psw
	);

	signal state, next_state: ID_state_type;
	signal IF_in: IF_stage_in_type;
	signal IFe, next_IF: IF_stage_type;
	signal ID: ID_stage_type;
	signal ID2, next_ID2: ID2_stage_type;
	signal AG, next_AG: AG_stage_type;
	signal MEM, next_MEM: MEM_stage_type;
	signal EX, next_EX: EX_stage_type;
	signal WB, next_WB: WB_stage_type;

	signal IF_stall: std_logic;
	signal ID_stall: std_logic;
	signal ID2_stall: std_logic;
	signal AG_stall: std_logic;
	signal MEM_stall: std_logic;
	signal EX_stall: std_logic;
	signal WB_stall: std_logic;

	signal ibuffer_rd: std_logic;
	signal icache_in: icache_in_type;
	signal icache_out: icache_out_type;
	signal ibuffer_in: ibuffer_in_type;
	signal ibuffer_out: ibuffer_out_type;
	signal dcache_in: dcache_in_type;
	signal dcache_out: dcache_out_type;
	signal gpr_out: gpr_out_type;
	signal alu_out: alu_out_type;
	signal mem_data: std_logic_vector(WORD_SIZE-1 downto 0);
	signal buf_flush: std_logic;
	signal mem_raw_hazard: std_logic;
	signal dbg_in: hw_debugger_in_type;
	signal dbg_out: hw_debugger_out_type;

	signal psw: std_logic_vector(15 downto 0);
	alias HALT_USER: std_logic is psw(8);
	alias P: std_logic is psw(7);  -- interrupt mask
	alias T: std_logic is psw(4);
	alias N: std_logic is psw(3);
	alias Z: std_logic is psw(2);
	alias V: std_logic is psw(1);
	alias C: std_logic is psw(0);
	signal psw_ext: std_logic_vector(15 downto 0);

	subtype instr_string is string(1 to 32);
--	signal ID_str: instr_string;
--	signal ID2_str: instr_string;
--	signal AG_str: instr_string;
	signal PC: std_logic_vector(15 downto 0);

	signal t_req, next_t_req: std_logic;

	for all: icache use entity work.icache(behaviour_stub);
	for all: dcache use entity work.dcache(behaviour_stub);
begin
	PC <= ibuffer_out.pc & "0";
	iack <= '1' when state = st_irq3 else '0';

	IF_in.pc_incr <= ibuffer_out.read_num;

-- pragma translate_off
--	instr_disasm: process (ibuffer_out)
--	  variable v: instr_string;
--	begin
--	  strcpy(v, disassemble(ibuffer_out.insn, ibuffer_out.immed1, ibuffer_out.immed2,
--		       ibuffer_out.insn_rdy, ibuffer_out.immed1_rdy, ibuffer_out.immed2_rdy));
--		ID_str <= v;
--	end process instr_disasm;

--	id2_disasm: process(ID2)
--	  variable v: instr_string;
--	begin
--		strcpy(v, ID2_toString(ID2));
--		ID2_str <= v;
--	end process id2_disasm;

--	ag_disasm: process (AG)
--	  variable v: instr_string;
--	begin
--	  strcpy(v, AG_toString(AG));
--		AG_str <= v;
--	end process ag_disasm;
-- pragma translate_on

	instr_cache: icache
	generic map
	(
    CACHE_SIZE => ICACHE_SIZE,
    ASSOCIATIVITY => ICACHE_ASSOCIATIVITY,
    LINE_SIZE	=> ICACHE_LINE_SIZE,
		DATA_WIDTH => ICACHE_DATA_WIDTH
	)
	port map
	(
	  clk => clk,
		reset => reset,
		din => icache_in,
		dout => icache_out,
		avm_address => avm_icache_address,
		avm_burstcount =>	avm_icache_burstcount,
		avm_read =>	avm_icache_read,
		avm_flush => avm_icache_flush,
		avm_readdata =>	avm_icache_readdata,
		avm_readdatavalid => avm_icache_readdatavalid,
		avm_waitrequest => avm_icache_waitrequest
	);

	buf_flush <= EX.jump_cmd.jump or IF_in.uncond_branch or IF_in.branch;
	icache_in.next_pc <= next_IF.pc;
	icache_in.pc <= IFe.pc;
	icache_in.en <= IFe.rdy and not dbg_out.halt;
	icache_in.flush <= buf_flush;
	icache_in.address_stall <= IF_stall;
	icache_in.wr_address <= next_WB.mem_address;
	icache_in.wr_data <= next_WB.mem_data;
	icache_in.wr <= next_WB.alu_wb_cmd.mem_we;
  icache_in.byte_en <= next_WB.mem_byteen;
	ibuffer_in.pc <= IFe.pc;
	ibuffer_in.icache_data0 <= icache_out.data0;
	ibuffer_in.icache_data0_rdy <= icache_out.data0_rdy;
	ibuffer_in.icache_data1 <= icache_out.data1;
	ibuffer_in.icache_data1_rdy <= icache_out.data1_rdy;
	ibuffer_in.insn_rd <= not ID_stall and not IF_in.uncond_branch and not dbg_out.halt;
	ibuffer_in.flush	<= buf_flush;

	instr_buffer: ibuffer
	port map
	(
	  clk => clk,
	  reset => reset,
		din => ibuffer_in,
		dout => ibuffer_out
	);

	dcache_in.rd_address <= next_MEM.mem_cmd.address;
	dcache_in.rd <= MEM.mem_cmd.rd and not mem_raw_hazard;
	dcache_in.rd_address_stall <= MEM_stall;
	dcache_in.wr_address <= next_WB.mem_address;
	dcache_in.wr_data <= next_WB.mem_data;
	dcache_in.wr <= next_WB.alu_wb_cmd.mem_we;
	dcache_in.byte_en <= next_WB.mem_byteen;

	data_cache: dcache
	generic map
	(
    CACHE_SIZE => DCACHE_SIZE,
    ASSOCIATIVITY => DCACHE_ASSOCIATIVITY,
    LINE_SIZE => DCACHE_LINE_SIZE,
		DATA_WIDTH => WORD_SIZE
	)
	port map
	(
	  clk => clk,
		reset => reset,

		din => dcache_in,
		dout => dcache_out,
--		dbg_state: out dcache_debug_state;
		avm_address => avm_dcache_address,
		avm_burstcount => avm_dcache_burstcount,
		avm_read => avm_dcache_read,
		avm_readdata => avm_dcache_readdata,
		avm_readdatavalid => avm_dcache_readdatavalid,
		avm_write => avm_dcache_write,
		avm_writedata => avm_dcache_writedata,
		avm_byteenable => avm_dcache_byteenable,
		avm_waitrequest => avm_dcache_waitrequest,

		avm_io_address => avm_io_address,
		avm_io_read => avm_io_read,
		avm_io_readdata => avm_io_readdata,
		avm_io_readdatavalid => avm_io_readdatavalid,
		avm_io_write => avm_io_write,
		avm_io_writedata => avm_io_writedata,
		avm_io_byteenable => avm_io_byteenable,
		avm_io_waitrequest => avm_io_waitrequest
	);

	psw_ext <= x"00" & psw(7 downto 0);

	regs: gpr
	port map
	(
	  clk => clk,
		reset => reset,

		din.rd_address1 => MEM.alu_cmd.src_reg,
		din.pc1 => "0000000000000000",
		din.immed1 => MEM.alu_cmd.src_immed,
		din.rd_address2 => MEM.alu_cmd.dst_reg,
		din.pc2 => "0000000000000000",
		din.immed2 =>	MEM.alu_cmd.dst_immed,
		din.rd_address3 => ID2.agu_cmd.base_reg,
		din.pc3 => "0000000000000000",
		din.immed3 => ID2.agu_cmd.base,
		din.dbg_address => dbg_out.reg_address,
		din.wr_address1 => next_WB.alu_wb_cmd.reg,
		din.wr_data1 =>	alu_out.result,
		din.wr_byte1 =>	 next_WB.alu_wb_cmd.fbyte,
		din.we1 => next_WB.alu_wb_cmd.reg_we,
		din.wr_address2 => next_WB.agu_wb_cmd.base_reg,
		din.wr_data2 =>	next_WB.agu_wb_cmd.value,
		din.we2 =>	next_WB.agu_wb_cmd.reg_we,
		din.psw => psw_ext,
		din.mem_data => mem_data,
		dout => gpr_out,
		trace_value => trace.gpr,
		trace_mask => trace.gpr_mask
	);

	c_alu: alu
	port map
	(
    din.src => EX.src,
    din.dst => EX.dst,
    din.oper => EX.alu_cmd.alu_oper,
		din.n_oper => EX.alu_cmd.n_oper,
		din.z_oper => EX.alu_cmd.z_oper,
		din.v_oper => EX.alu_cmd.v_oper,
		din.c_oper => EX.alu_cmd.c_oper,
		din.fbyte => EX.alu_cmd.fbyte,
    din.n => N,
		din.z => Z,
		din.v => V,
		din.c => C,
		dout => alu_out
	);

	dbg_in.pc <= ibuffer_out.pc;
	dbg_in.pc_valid <= '1' when ibuffer_out.insn_rdy = '1' and state = st_start else  '0';
	dbg_in.jump <= next_IF.jump;
	dbg_in.jump_address <= next_IF.pc;
	dbg_in.insn <= ibuffer_out.insn;
	dbg_in.immed1 <= ibuffer_out.immed1;
	dbg_in.immed2 <= ibuffer_out.immed2;
	dbg_in.mem_rd_address <= MEM.mem_cmd.address;
	dbg_in.mem_wr_address <= WB.mem_address;
	dbg_in.mem_wr_data <= WB.mem_data;
	dbg_in.load <= MEM.mem_cmd.rd;
	dbg_in.store <= WB.alu_wb_cmd.mem_we;
	dbg_in.byte_en <= WB.mem_byteen;
  dbg_in.reg_value <= gpr_out.dbg_data;
	dbg_in.pipe_state	<= "00000" & ibuffer_out.insn_len & ibuffer_out.immed2_rdy & ibuffer_out.immed1_rdy & IF_stall & ID_stall & ID2_stall & AG_stall & MEM_stall & EX_stall & WB_stall;

  ID.insn.pc <= ibuffer_out.pc & "0";
  ID.insn.len <= ibuffer_out.insn_len;
  ID.insn.opcode <= ibuffer_out.insn;
  ID.insn.rdy <= '1' when (ibuffer_out.insn_rdy = '1' and state = st_start) else '0';
  ID.insn.immed1 <= ibuffer_out.immed1;
  ID.insn.immed2 <= ibuffer_out.immed2;
  ID.insn.immed1_rdy <= ibuffer_out.immed1_rdy;
  ID.insn.immed2_rdy <= ibuffer_out.immed2_rdy;

	trace.ID_insn <= ID.insn;
	trace.ID2_insn <= ID2.insn;
	trace.AG_insn  <= AG.insn;
	trace.MEM_insn <= MEM.insn;
	trace.EX_insn  <= EX.insn;
	trace.WB_insn  <= WB.insn;

	trace.IF_stall  <= IF_stall;
	trace.ID_stall  <= ID_stall;
	trace.ID2_stall <= ID2_stall;
	trace.AG_stall  <= AG_stall;
	trace.MEM_stall <= MEM_stall;
	trace.EX_stall  <= EX_stall;
	trace.WB_stall  <= WB_stall;

  trace.halt <= '1' when (state = st_halt and ID.insn.rdy = '0' and
                    ID2.insn.rdy = '0' and AG.insn.rdy = '0' and
                    MEM.insn.rdy = '0' and EX.insn.rdy = '0' and
                    WB.insn.rdy = '0')
                else '0';

	debugger: hw_debugger
	generic map
	(
	  INSTRUCTION_BREAKPOINT_NUM => 3,
		DATA_BREAKPOINT_NUM => 1,
		TRACE_BUFFER_SIZE	=> 128
	)
	port map
	(
	  clk => clk,
		reset => reset,

		avs_dbg_address => avs_dbg_address,
		avs_dbg_writedata => avs_dbg_writedata,
		avs_dbg_chipselect => avs_dbg_chipselect,
		avs_dbg_read => avs_dbg_read,
		avs_dbg_write => avs_dbg_write,
		avs_dbg_readdata => avs_dbg_readdata,

		din => dbg_in,
		dout => dbg_out,

		reset_out => reset_out,
		int_reset_out => int_reset_out
	);

--	IF_stall <= '1' when state = st_start and ibuffer_out.immed2_rdy = '1' else '0';
  IF_stall <= '1' when ibuffer_out.immed2_rdy = '1' else '0';

	IF_stage: process(IFe, IF_in, next_state, alu_out, IF_stall, ID_stall)
	  variable v: IF_stage_type;
	begin
    if reset = '1' then
      v.pc := IFe.pc;
      v.jump := '0';
      v.rdy := '0';
		elsif IF_in.jump = '1' then
			v.pc := alu_out.result(WORD_SIZE-1 downto PC_LOW);
			v.jump := '1';
			v.rdy := '1';
		elsif IF_in.branch = '1' then
			v.pc := IF_in.branch_address;
			v.jump := '1';
			v.rdy := '1';
		elsif IF_in.uncond_branch = '1' then
			v.pc := IF_in.uncond_branch_address;
			v.jump := '1';
			v.rdy := '1';
		else
      v.pc := IFe.pc + ("0000000000000" & IF_in.pc_incr);
			v.jump := '0';
			if ibuffer_out.read_num /= 0 then
			  v.rdy := '0';
			else
				v.rdy := '1';
			end if;
		  v.rdy := v.rdy or not IF_stall;
		  if next_state = st_branch then
			  v.rdy := '0';
		  end if;
		end if;
		next_IF <= v;
	end process IF_stage;

	ID_stage: process(ID, ID2_stall, state, ibuffer_out, EX, gpr_out, alu_out,
                    irq, ivec, t_req, dbg_out.halt)
		alias ireg: std_logic_vector(15 downto 0) is ibuffer_out.insn;
	  alias op_code: std_logic_vector(3 downto 0) is ireg(15 downto 12);
	  alias op_prim: std_logic_vector(2 downto 0) is ireg(14 downto 12);
	  alias op_byte: std_logic is ireg(15);
	  alias op_ext1: std_logic_vector(2 downto 0) is ireg(11 downto 9);
	  alias op_ext2: std_logic_vector(2 downto 0) is ireg(8 downto 6);
	  alias op_ext3: std_logic_vector(2 downto 0) is ireg(5 downto 3);
	  alias op_ext4: std_logic_vector(2 downto 0) is ireg(2 downto 0);

	  alias src_mode: std_logic_vector(2 downto 0) is ireg(11 downto 9);
	  alias dst_mode: std_logic_vector(2 downto 0) is ireg(5 downto 3);

	  alias src_reg: std_logic_vector(2 downto 0) is ireg(8 downto 6);
	  alias dst_reg: std_logic_vector(2 downto 0) is ireg(2 downto 0);

		variable vpc_plus2: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		variable vpc_plus4: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		variable vpc_plus6: std_logic_vector(WORD_SIZE-1 downto PC_LOW);

		variable vnext_state: ID_state_type;
		variable vnext_pc: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		variable vv: ID2_stage_type;
		variable vcmd: alu_cmd_type;
		variable vsrc_agu_cmd: agu_cmd_type;
		variable vdst_agu_cmd: agu_cmd_type;

		variable vtrap_addr: std_logic_vector(7 downto 0);
		variable vtrap_addr_p2: std_logic_vector(7 downto 0);

		variable vsrc_reg: std_logic_vector(3 downto 0);
		variable vdst_reg: std_logic_vector(3 downto 0);
		variable vsrc_mode: std_logic_vector(2 downto 0);
		variable vload_src: std_logic;
		variable vload_dst: std_logic;
		variable vwrite_dst: std_logic;
		variable vsrc_use_immed: std_logic;
	  variable vdst_use_immed: std_logic;
		variable vsrc_use_mem: std_logic;
		variable vdst_use_mem: std_logic;

		variable vid_stall: std_logic;
		variable vpsw_we: std_logic;
		variable vbranch_offset: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		variable vbranch_address: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		variable vbranch_flag: std_logic;
		variable vuncond : std_logic;
		variable vjump_flag: std_logic;
		variable vsob_flag: std_logic;
		variable vjsr_flag: std_logic;
		variable vrts_flag: std_logic;
		variable vtrap_flag: std_logic;
		variable vrti_flag: std_logic;
		variable vrtt_flag: std_logic;
		variable vmark_flag: std_logic;
		variable vhalt_flag: std_logic;
		variable virq_trap: std_logic;
		variable vdecode_flag: std_logic;
		variable vnext_t_req: std_logic;

		procedure init is
		begin
			vv := ID2_empty;
		end procedure init;

		procedure bubble is
		begin
			vv := ID2_empty;
			vv.bubble := '1';
		end procedure bubble;

		function is_deferred_mode(mode: std_logic_vector(2 downto 0);
		  reg: std_logic_vector(2 downto 0)) return boolean is
		begin
			if mode(0) = '1' and mode(2 downto 1) /= "00" and not (mode(2 downto 1) = "01" and reg = "111") then
				return true;
			else
				return false;
			end if;
		end function is_deferred_mode;

		-- src = memory[base_reg + offset]
		procedure LOAD_SRC(fbyte: std_logic; base_reg: std_logic_vector(3 downto 0);
		  offset: std_logic_vector(WORD_SIZE-1 downto 0)) is
		begin
			vsrc_agu_cmd.load_areg := '1';
			vsrc_agu_cmd.fbyte := fbyte;
			vsrc_agu_cmd.base_reg := base_reg;
			vsrc_agu_cmd.offset := offset;
			vsrc_agu_cmd.rd := '1';
		end procedure LOAD_SRC;

		procedure LOAD_SRC(fbyte: std_logic; base_reg: std_logic_vector(3 downto 0); offset: integer) is
		begin
			LOAD_SRC(fbyte, base_reg, conv_std_logic_vector(offset, 16));
		end procedure LOAD_SRC;

		-- dst = memory[base_reg + offset]
		procedure LOAD_DST(fbyte: std_logic; base_reg: std_logic_vector(3 downto 0);
		  offset: std_logic_vector(WORD_SIZE-1 downto 0)) is
		begin
			vdst_agu_cmd.load_areg := '1';
			vdst_agu_cmd.fbyte := fbyte;
			vdst_agu_cmd.base_reg := base_reg;
			vdst_agu_cmd.offset := offset;
			vdst_agu_cmd.rd := '1';
		end procedure LOAD_DST;

		procedure LOAD_DST(fbyte: std_logic; base_reg: std_logic_vector(3 downto 0); offset: integer) is
		begin
			LOAD_DST(fbyte, base_reg, conv_std_logic_vector(offset, 16));
--			vv.alu_cmd.dst_reg := MEM_REG;
		end procedure LOAD_DST;

		procedure LOAD_ALU(temp_reg: std_logic_vector(3 downto 0)) is
		begin
			vv.alu_cmd.alu_oper := alu_add;
			vv.alu_cmd.src_reg := MEM_REG;
			vv.alu_cmd.dst_reg := ZERO_REG;
			vv.alu_wb_cmd.reg_we := '1';
			vv.alu_wb_cmd.reg := temp_reg;
		end procedure LOAD_ALU;

		-- TEMPn = (base_reg)
		procedure LOAD_TEMP(base_reg: std_logic_vector(3 downto 0); temp_reg: std_logic_vector(3 downto 0);
		  pre_dec: std_logic; post_inc: std_logic) is
		begin
			vv.agu_cmd.load_areg := '1';
			vv.agu_cmd.fbyte := '0';
			vv.agu_cmd.base_reg := base_reg;
		  if pre_dec = '1' then
				vv.agu_cmd.offset := x"FFFE";
				vv.agu_cmd.reg_we := '1';
			elsif post_inc = '1' then
				vv.agu_cmd.offset := x"0002";
				vv.agu_cmd.reg_post_modify := '1';
				vv.agu_cmd.reg_we := '1';
			else
				vv.agu_cmd.offset := x"0000";
			end if;
			vv.agu_cmd.rd := '1';
			LOAD_ALU(temp_reg);
		end procedure LOAD_TEMP;

		procedure STORE_ALU(reg: std_logic_vector(3 downto 0)) is
		begin
			vv.alu_cmd.alu_oper := alu_add;
			vv.alu_cmd.src_reg := reg;
			vv.alu_cmd.dst_reg := ZERO_REG;
			vv.alu_wb_cmd.mem_we := '1';
		end procedure STORE_ALU;

		-- [--SP] = reg
		procedure PUSH_REG(reg: std_logic_vector(3 downto 0)) is
		begin
			vv.agu_cmd.load_areg := '1';
			vv.agu_cmd.fbyte := '0';
			vv.agu_cmd.base_reg := SP_REG;
			vv.agu_cmd.offset := x"FFFE";
			vv.agu_cmd.reg_we := '1';
			STORE_ALU(reg);
		end procedure PUSH_REG;

		-- [--SP] = PC
		procedure PUSH_PC is
		begin
			vv.agu_cmd.load_areg := '1';
			vv.agu_cmd.fbyte := '0';
			vv.agu_cmd.base_reg := SP_REG;
			vv.agu_cmd.offset := x"FFFE";
			vv.agu_cmd.reg_we := '1';
			vv.alu_cmd.src_immed := ibuffer_out.pc & '0';
			STORE_ALU(IMM_REG);
		end procedure PUSH_PC;

		-- [--SP] = next PC
		procedure PUSH_NEXT_PC is
		begin
			vv.agu_cmd.load_areg := '1';
			vv.agu_cmd.fbyte := '0';
			vv.agu_cmd.base_reg := SP_REG;
			vv.agu_cmd.offset := x"FFFE";
			vv.agu_cmd.reg_we := '1';
			vv.alu_cmd.src_immed := vnext_pc & '0';
			STORE_ALU(IMM_REG);
		end procedure PUSH_NEXT_PC;

		-- reg = [SP++]
		procedure POP_REG(reg: std_logic_vector(3 downto 0)) is
		begin
			LOAD_TEMP(SP_REG, reg, '0', '1');
		end procedure POP_REG;

		-- if PC is the destination register, instruction converts to jump on calculated address
		procedure TEST_PC_DEST is
		begin
			if vv.alu_wb_cmd.reg_we = '1' and vv.alu_wb_cmd.reg = PC_REG then
				vv.jump_cmd.jump := '1';
				vnext_state := st_branch;
			end if;
		end procedure TEST_PC_DEST;

		procedure decode_src is
		begin
		  case vsrc_mode is
			  when "000" =>	             -- register Ri
				  if src_reg = "111" then
						vcmd.src_reg := IMM_REG;
						vcmd.src_immed := vnext_pc & '0';
					end if;
				when "001" =>		           -- register deferrred (Ri)
				  if src_reg = "111" then
						LOAD_SRC(vcmd.fbyte, IMM_REG, 0);
						vsrc_agu_cmd.base := vpc_plus2 & '0';
					else
				    LOAD_SRC(vcmd.fbyte, vsrc_reg, 0);
					end if;
				when "010" =>
				  if src_reg = "111" then  -- immediate #n
						vcmd.src_reg := IMM_REG;
					else	                   -- autoincrement (Ri)+
						if vsrc_reg(3 downto 1) = "011" then  -- SP or PC
							LOAD_SRC(vcmd.fbyte, vsrc_reg, 2);
						else
						  if vcmd.fbyte = '1' then
							  LOAD_SRC(vcmd.fbyte, vsrc_reg, 1);
						  else
							  LOAD_SRC(vcmd.fbyte, vsrc_reg, 2);
							end if;
						end if;
						vsrc_agu_cmd.reg_post_modify := '1';
						vsrc_agu_cmd.reg_we := '1';
					end if;
				when "011" =>
				  if src_reg = "111" then   -- absolute @#n
					  LOAD_SRC('0', ZERO_REG, ibuffer_out.immed1);
					else	                    -- autoincrement deferred @(Ri)+
						LOAD_SRC('0', vsrc_reg, 2);
						vsrc_agu_cmd.reg_post_modify := '1';
						vsrc_agu_cmd.reg_we := '1';
					end if;
				when "100" =>							  -- autodecrement -(Ri)
				  if vsrc_reg(3 downto 1) = "011" then  -- SP or PC
						LOAD_SRC(vcmd.fbyte, vsrc_reg, -2);
					else
				    if vcmd.fbyte = '1' then
					    LOAD_SRC(vcmd.fbyte, vsrc_reg, -1);
					  else
						  LOAD_SRC(vcmd.fbyte, vsrc_reg, -2);
					  end if;
					end if;
					vsrc_agu_cmd.reg_we := '1';
				when "101" =>							  -- autodecrement deferred @-(Ri)
				  LOAD_SRC('0', vsrc_reg, -2);
					vsrc_agu_cmd.reg_we := '1';
				when "110" =>
          if src_reg = "111" then	-- PC relative
            LOAD_SRC(vcmd.fbyte, IMM_REG, ibuffer_out.immed1);
            vsrc_agu_cmd.base := vpc_plus4 & '0';
          else								    -- index n(Ri)
            LOAD_SRC(vcmd.fbyte, vsrc_reg, ibuffer_out.immed1);
          end if;
        when "111" =>
          if src_reg = "111" then -- PC relative deferred
            LOAD_SRC('0', IMM_REG, ibuffer_out.immed1);
            vsrc_agu_cmd.base := vpc_plus4 & '0';
          else										-- index deferred @n(Ri)
            LOAD_SRC('0', vsrc_reg, ibuffer_out.immed1);
          end if;
				when others =>
				  null;
			end case;
			if vsrc_use_immed = '1' and ibuffer_out.immed1_rdy = '0' then
				vid_stall := '1';
			end if;
		end procedure decode_src;

		procedure decode_dst is
		  variable vimmed: std_logic_vector(15 downto 0);
		  variable vimmed_rdy: std_logic;
			variable vimmed_pc: std_logic_vector(15 downto 1);
		begin
			if vsrc_use_immed = '0' then
				vimmed := ibuffer_out.immed1;
				vimmed_rdy := ibuffer_out.immed1_rdy;
				vimmed_pc := vpc_plus4;
			else
				vimmed := ibuffer_out.immed2;
				vimmed_rdy := ibuffer_out.immed2_rdy;
				vimmed_pc := vpc_plus6;
			end if;
		  case dst_mode is
			  when "000" =>	             -- register Ri
				  if vload_dst = '1' and dst_reg = "111" then
						vcmd.dst_reg := IMM_REG;
						vcmd.dst_immed := vnext_pc & '0';
					end if;
				when "001" =>		           -- register deferrred (Ri)
				  LOAD_DST(vcmd.fbyte, vdst_reg, 0);
				when "010" =>
				  if dst_reg = "111" and vwrite_dst = '0' then  -- immediate #n
						vcmd.dst_reg := IMM_REG;
					else	                   -- autoincrement (Ri)+
						if vdst_reg(3 downto 1) = "011" then  -- SP or PC
							if vdst_reg(0) = '0' then
							  LOAD_DST(vcmd.fbyte, vdst_reg, 2);
							else
						    LOAD_DST(vcmd.fbyte, IMM_REG, 0);
								if vsrc_use_immed = '1' then
						      vdst_agu_cmd.base := vpc_plus4 & '0';
								else
									vdst_agu_cmd.base := vpc_plus2 & '0';
								end if;
							end if;
						else
						  if vcmd.fbyte = '1' then
							  LOAD_DST(vcmd.fbyte, vdst_reg, 1);
						  else
							  LOAD_DST(vcmd.fbyte, vdst_reg, 2);
						  end if;
						end if;
						vdst_agu_cmd.reg_post_modify := '1';
						vdst_agu_cmd.reg_we := '1';
					end if;
				when "011" =>
				  if dst_reg = "111" then   -- absolute @#n
					  LOAD_DST('0', ZERO_REG, vimmed);
					else	                    -- autoincrement deferred @(Ri)+
						LOAD_DST('0', vdst_reg, 2);
						vdst_agu_cmd.reg_post_modify := '1';
						vdst_agu_cmd.reg_we := '1';
					end if;
				when "100" =>							  -- autodecrement -(Ri)
				  if vdst_reg(3 downto 1) = "011" then  -- SP or PC
						LOAD_DST(vcmd.fbyte, vdst_reg, -2);
					else
				    if vcmd.fbyte = '1' then
					    LOAD_DST(vcmd.fbyte, vdst_reg, -1);
					  else
						  LOAD_DST(vcmd.fbyte, vdst_reg, -2);
					  end if;
					end if;
					vdst_agu_cmd.reg_we := '1';
				when "101" =>							  -- autodecrement deferred @-(Ri)
				  LOAD_DST('0', vdst_reg, -2);
					vdst_agu_cmd.reg_we := '1';
				when "110" =>
          if dst_reg = "111" then	-- PC relative
            LOAD_DST(vcmd.fbyte, IMM_REG, vimmed);
            vdst_agu_cmd.base := vimmed_pc & '0';
          else								    -- index n(Ri)
            LOAD_DST(vcmd.fbyte, vdst_reg, vimmed);
          end if;
        when "111" =>
          if dst_reg = "111" then -- PC relative deferred
            LOAD_DST('0', IMM_REG, vimmed);
            vdst_agu_cmd.base := vimmed_pc & '0';
          else										-- index deferred @n(Ri)
            LOAD_DST('0', vdst_reg, vimmed);
          end if;
				when others =>
				  null;
      end case;
			if vdst_use_immed = '1' and vimmed_rdy = '0' then
				vid_stall := '1';
			end if;
		end procedure decode_dst;

		procedure TRAP(trap_addr: std_logic_vector(7 downto 0)) is
		begin
			vv.jump_cmd.jump := '1';
			vv.alu_cmd.alu_oper := alu_add;
			vv.alu_cmd.src_reg := MEM_REG;
			vv.alu_cmd.dst_reg := ZERO_REG;
			vv.agu_cmd.base_reg := ZERO_REG;
			vv.agu_cmd.offset := x"00" & trap_addr;
			vv.agu_cmd.rd := '1';
			vv.alu_wb_cmd.reg := PC_REG;
			vv.alu_wb_cmd.reg_we := '1';
			vnext_state := st_trap1;
		end procedure TRAP;

	begin
		init;
		vnext_state := state;
		vnext_pc := ibuffer_out.pc + ("0000000000000" & ibuffer_out.insn_len);
		vpc_plus2 := ibuffer_out.pc + 1;
		vpc_plus4 := ibuffer_out.pc + 2;
		vpc_plus6 := ibuffer_out.pc + 3;
		vcmd := alu_cmd_empty;
		vsrc_agu_cmd := agu_cmd_empty;
		vdst_agu_cmd := agu_cmd_empty;
		vsrc_reg := '0' & src_reg;
		vdst_reg := '0' & dst_reg;
		vsrc_mode := src_mode;
		vcmd.src_reg := vsrc_reg;
		vcmd.dst_reg := vdst_reg;
		vload_src := '0';
		vload_dst := '0';
		vwrite_dst := '0';
		vsrc_use_immed := '0';
		vdst_use_immed := '0';
		vsrc_use_mem := '0';
		vdst_use_mem := '0';
		vpsw_we := '0';
		vbranch_flag := '0';
		vuncond := '0';
		vjump_flag := '0';
		vsob_flag := '0';
		vjsr_flag := '0';
		vrts_flag := '0';
		vtrap_flag := '0';
		vtrap_addr := (others => '0');
		vtrap_addr_p2 := (others => '0');
		vrti_flag := '0';
		vrtt_flag := '0';
		vmark_flag := '0';
		vhalt_flag := '0';
		vid_stall := '0';
		vdecode_flag := '0';
		vnext_t_req := t_req;

		if ireg(7) = '1' then
			vbranch_offset := "1111111" & ireg(7 downto 0);
		else
		  vbranch_offset := "0000000" & ireg(7 downto 0);
		end if;
		vbranch_address := ibuffer_out.pc + vbranch_offset + 1;

		if ibuffer_out.insn_rdy = '1' then
			vcmd.fbyte := op_byte;

	    if op_prim = "000" then

	      if op_byte = '0' and op_ext1 = "000" then

	        if op_ext2 = "000" and op_ext3 = "000" then -- HALT,...,RTT

						case op_ext4 is

	            when "000" =>               -- HALT
							  vhalt_flag := '1';
								vdecode_flag := '1';
	            when "001" =>               -- WAIT

							  vdecode_flag := '1';
	            when "010" =>               -- RTI
							  vrti_flag := '1';
								vdecode_flag := '1';
	            when "011" =>               -- BPT (trap to 14)
							  vtrap_flag := '1';
								vtrap_addr := "00001100";     -- 014
								vtrap_addr_p2 := "00001110";  -- 016
								vdecode_flag := '1';
	            when "100" =>               -- IOT (trap to 20)
							  vtrap_flag := '1';
								vtrap_addr := "00010000";     -- 020
								vtrap_addr_p2 := "00010010";  -- 022
								vdecode_flag := '1';
	            when "101" =>               -- RESET

							  vdecode_flag := '1';
	            when "110" =>               -- RTT
							  vrtt_flag := '1';
								vdecode_flag := '1';
	            when others => null;

	          end case;
	        end if;

	        if op_ext2 = "001" then          -- JMP
						vload_dst := '1';
						vcmd.src_reg := ZERO_REG;
						vcmd.alu_oper := alu_add;
						vjump_flag := '1';
						vdecode_flag := '1';
					end if;

	        if op_ext2 = "010" then
	          if op_ext3 = "000" then        -- RTS
							vrts_flag := '1';
							vdecode_flag := '1';
						end if;
	          if op_ext3 = "011" then        -- SPL

						end if;
	        end if;

	        if op_ext2 = "010" then
	          if op_ext3(2) = '1' then       -- SEx/CLx
							vdecode_flag := '1';
							if ireg(4) = '0' then
								if ireg(0) = '1' then  -- CLC
									vcmd.c_oper := fl_clear;
								end if;
								if ireg(1) = '1' then  -- CLV
									vcmd.v_oper := fl_clear;
								end if;
								if ireg(2) = '1' then  -- CLZ
									vcmd.z_oper := fl_clear;
								end if;
								if ireg(3) = '1' then  -- CLN
									vcmd.n_oper := fl_clear;
								end if;
							else
								if ireg(0) = '1' then  -- SEC
									vcmd.c_oper := fl_set;
								end if;
								if ireg(1) = '1' then  -- SEV
									vcmd.v_oper := fl_set;
								end if;
								if ireg(2) = '1' then  -- SEZ
									vcmd.z_oper := fl_set;
								end if;
								if ireg(3) = '1' then  -- SEN
									vcmd.n_oper := fl_set;
								end if;
							end if;
						end if;
	        end if;

	        if op_ext2 = "011" then          -- SWAB
						vload_dst := '1';
						vwrite_dst := '1';
						vcmd.src_reg := ZERO_REG;
						vcmd.alu_oper := alu_swab;
						vcmd.fbyte := '0';
						vcmd.n_oper := fl_alu;
						vcmd.z_oper := fl_alu;
						vcmd.v_oper := fl_clear;
						vcmd.c_oper := fl_clear;
						vdecode_flag := '1';
					end if;

	      end if;

	      if ireg(14 downto 11) = "0000" then              -- BR class instructions
					if ireg(15) /= '0' or ireg(10 downto 8) /= "000" then
					  vbranch_flag := '1';
						vdecode_flag := '1';
					end if;
					if ireg(15) = '0' and ireg(10 downto 8) = "001" then  -- BR
						vuncond := '1';
						vdecode_flag := '1';
					end if;
				end if;

	      if op_byte ='0' and op_ext1 = "100" then -- JSR
					vcmd.src_reg := ZERO_REG;
					vjsr_flag := '1';
					vload_dst := '1';
					vdecode_flag := '1';
				end if;

	      if op_byte = '1' and op_ext1 = "100" then -- EMT, TRAP
	        if op_ext2(2) = '0' then         -- EMT (trap tp 30)
						vtrap_flag := '1';
						vtrap_addr := "00011000";     -- 030
						vtrap_addr_p2 := "00011010";  -- 032
					else                            -- TRAP (trap to 34)
					  vtrap_flag := '1';
						vtrap_addr := "00011100";     -- 034
						vtrap_addr_p2 := "00011110";  -- 036
					end if;
					vdecode_flag := '1';
	      end if;

	      if op_ext1 = "101" then            -- CLR(B),...,TST(B)
					vload_dst := '1';
					vwrite_dst := '1';
					vcmd.n_oper := fl_alu;
					vcmd.z_oper := fl_alu;
					vcmd.fbyte := op_byte;
	        case op_ext2 is
	          when "000" =>                 -- CLR:    0 +    0 + 0   (0)
						  vload_dst := '0';
						  vcmd.src_reg := ZERO_REG;
							vcmd.dst_reg := ZERO_REG;
							vcmd.alu_oper := alu_add;
							vcmd.v_oper := fl_clear;
					    vcmd.c_oper := fl_clear;
							vdecode_flag := '1';
						when "001" =>                 -- COM:    0 + ~DST + 0   (~dst)
						  vcmd.src_reg := IMM_REG;
							vcmd.src_immed := x"FFFF";
							vcmd.alu_oper := alu_xor;
							vcmd.v_oper := fl_clear;
					    vcmd.c_oper := fl_set;
							vdecode_flag := '1';
						when "010" =>                 -- INC:    0 +  DST + 1   (dst+1)
						  vcmd.src_reg := IMM_REG;
							vcmd.src_immed := x"0001";
							vcmd.alu_oper := alu_add;
							vcmd.v_oper := fl_alu;
							vdecode_flag := '1';
						when "011" =>                 -- DEC:   ~0 +  DST + 0   (dst-1)
						  vcmd.src_reg := IMM_REG;
							vcmd.src_immed := x"0001";
							vcmd.alu_oper := alu_sub;
							vcmd.v_oper := fl_alu;
							vdecode_flag := '1';
						when "100" =>                 -- NEG:    0 + ~DST + 1   (-dst)
						  vcmd.src_reg := ZERO_REG;
							vcmd.alu_oper := alu_rsb;
							vcmd.v_oper := fl_alu;
							vcmd.c_oper := fl_alu;
							vdecode_flag := '1';
						when "101" =>                 -- ADC:    0 +  DST + CI  (dst+ci)
						  vcmd.alu_oper := alu_adc;
							vcmd.v_oper := fl_alu;
					    vcmd.c_oper := fl_alu;
							vdecode_flag := '1';
						when "110" =>                 -- SBC:   ~0 +  DST + ~CI (dst-ci)
						  vcmd.alu_oper := alu_sbc;
							vcmd.v_oper := fl_alu;
					    vcmd.c_oper := fl_alu;
							vdecode_flag := '1';
						when "111" =>                 -- TST:    0 +  DST + 0   (dst)
						  vwrite_dst := '0';
						  vcmd.src_reg := ZERO_REG;
							vcmd.alu_oper := alu_add;
							vcmd.v_oper := fl_clear;
					    vcmd.c_oper := fl_clear;
							vdecode_flag := '1';
						when others => null;
	        end case;
	      end if;

	      if op_ext1 = "110" then
	        if op_ext2(2) = '0' then         -- ROR(B),...,ASL(B)
						vload_dst := '1';
						vwrite_dst := '1';
						vcmd.fbyte := op_byte;
					  vcmd.n_oper := fl_alu;
					  vcmd.z_oper := fl_alu;
					  vcmd.v_oper := fl_alu;
					  vcmd.c_oper := fl_alu;
	          case op_ext2(1 downto 0) is
	            when "00" =>                -- ROR
								vcmd.alu_oper := alu_ror;
							when "01" =>                -- ROL
								vcmd.alu_oper := alu_rol;
							when "10" =>                -- ASR
								vcmd.alu_oper := alu_asr;
							when "11" =>                -- ASL
								vcmd.alu_oper := alu_asl;
							when others => null;
	          end case;
						vdecode_flag := '1';
	        end if;

	        if op_byte = '0' and op_ext2 = "100" then -- MARK
						vmark_flag := '1';
					end if;

	        if op_ext2 = "101" then          -- MFP(I/D)

					end if;

	        if op_ext2 = "110" then          -- MTP(I/D)

					end if;

					if op_byte = '1' and op_ext2 = "111" then  -- MFPS
					  vwrite_dst := '1';
						vcmd.fbyte := '1';
						vcmd.src_reg := PSW_REG;
						vcmd.dst_reg := ZERO_REG;
						vcmd.alu_oper := alu_movb;
						vcmd.n_oper := fl_alu;
						vcmd.z_oper := fl_alu;
						vcmd.v_oper := fl_clear;
						vdecode_flag := '1';
					end if;

				  if op_byte = '1' and op_ext2 = "100" then  -- MTPS
						vload_dst := '1';
						vpsw_we := '1';
						vcmd.src_reg := ZERO_REG;
						vcmd.alu_oper := alu_add;
						vcmd.fbyte := '1';
						vdecode_flag := '1';
					end if;

	        if op_byte = '0' and op_ext2 = "111" then -- SXT
						vload_dst := '1';
						vwrite_dst := '1';
						vcmd.alu_oper := alu_sxt;
						vcmd.z_oper := fl_alu;
						vcmd.v_oper := fl_clear;
						vdecode_flag := '1';
					end if;
	      end if;

	    end if;

	    if op_prim /= "000" and op_prim /= "111" then
				vload_src := '1';
				vcmd.fbyte := op_byte;
				vcmd.n_oper := fl_alu;
				vcmd.z_oper := fl_alu;
	      case op_prim is
	        when "001" =>                   -- MOV
					  vwrite_dst := '1';
						vcmd.dst_reg := ZERO_REG;
						if op_byte = '1' then
							-- MOVB to register extends the most significant bit of low byte
							-- (unique among byte operations!)
							vcmd.alu_oper := alu_movb;
						else
						  vcmd.alu_oper := alu_add;
						end if;
						vcmd.v_oper := fl_clear;
						vdecode_flag := '1';
					when "010" =>                   -- CMP
					  vload_dst := '1';
						vcmd.alu_oper := alu_rsb;
						vcmd.c_oper := fl_alu;
						vcmd.v_oper := fl_alu;
						vdecode_flag := '1';
					when "011" =>                   -- BIT
					  vload_dst := '1';
						vcmd.alu_oper := alu_bit;
						vcmd.v_oper := fl_clear;
						vdecode_flag := '1';
					when "100" =>                   -- BIC
					  vload_dst := '1';
						vwrite_dst := '1';
						vcmd.alu_oper := alu_bic;
						vcmd.v_oper := fl_clear;
						vdecode_flag := '1';
					when "101" =>                   -- BIS
					  vload_dst := '1';
						vwrite_dst := '1';
						vcmd.alu_oper := alu_bis;
						vcmd.v_oper := fl_clear;
						vdecode_flag := '1';
					when "110" =>
					  vcmd.fbyte := '0';
					  vload_dst := '1';
						vwrite_dst := '1';
	          if op_byte = '0' then          -- ADD
						  vcmd.alu_oper := alu_add;
						else                          -- SUB
							vcmd.alu_oper := alu_sub;
						end if;
						vcmd.c_oper := fl_alu;
						vcmd.v_oper := fl_alu;
						vdecode_flag := '1';
	        when others => null;
	      end case;

	    end if;

	    if op_byte = '0' and op_prim = "111" then
				vsrc_mode := "000";
				vcmd.fbyte := '0';
	      case op_ext1 is
	        when "000" =>                   -- MUL

					when "001" =>                   -- DIV

					when "010" =>                   -- ASH

					when "011" =>                   -- ASHC

					when "100" =>                   -- XOR
					  vload_src := '1';
						vload_dst := '1';
						vwrite_dst := '1';
						vcmd.alu_oper := alu_xor;
						vcmd.n_oper := fl_alu;
						vcmd.z_oper := fl_alu;
						vcmd.v_oper := fl_clear;
						vdecode_flag := '1';
					when "111" =>                   -- SOB:  SRC +   ~0 + 0   (src-1)
						vload_src := '1';
--						vcmd.dst_reg := R_ONE;
					  vcmd.alu_oper := alu_rsb;
--						vcmd.wr_addr := '0' & src_reg;
--						vcmd.we := '1';
						vsob_flag := '1';
						vdecode_flag := '1';
					when others => null;
	      end case;
	    end if;

			if (vload_src = '1') and not (vsrc_mode = "000" or (vsrc_mode = "010" and src_reg = "111")) -- not register or immediate modes
			then
				vsrc_use_mem := '1';
			end if;
			if (vload_dst = '1' or vwrite_dst = '1') and not (dst_mode = "000" or (dst_mode = "010" and dst_reg = "111" and vwrite_dst = '0')) then
				vdst_use_mem := '1';
			end if;

			if vload_src = '1' and (vsrc_mode(2 downto 1) = "11" or (vsrc_mode(2 downto 1) = "01" and src_reg = "111")) then
				vsrc_use_immed := '1';
				vcmd.src_immed := ibuffer_out.immed1;
			else
				vsrc_use_immed := '0';
			end if;
			if (vload_dst = '1' or vwrite_dst = '1') and (dst_mode(2 downto 1) = "11" or (dst_mode(2 downto 1) = "01" and dst_reg = "111")) then
				vdst_use_immed := '1';
				if vsrc_use_immed = '1' then
					vcmd.dst_immed := ibuffer_out.immed2;
				else
					vcmd.dst_immed := ibuffer_out.immed1;
				end if;
			else
				vdst_use_immed := '0';
			end if;

			if vdecode_flag = '0' then   -- Illegal instruction
				vtrap_flag := '1';
				vtrap_addr := "00001000";     -- 010
				vtrap_addr_p2 := "00001010";  -- 012
			end if;
			if (vjump_flag = '1' or vjsr_flag = '1') and dst_mode = "000" then
				-- Illegal JUMP Rd or JSR Rs, Rd  command
				vjump_flag := '0';
				vjsr_flag := '0';
				vtrap_flag := '1';
				vtrap_addr := "00000100";     -- 004
				vtrap_addr_p2 := "00000110";  -- 006
			end if;
			case state is
				when st_start =>
					decode_src;
					decode_dst;
					if irq = '1'  and P = '0'
					then
					  TRAP(ivec);
						vnext_state := st_irq1;
					elsif t_req = '1' then
						TRAP("00001100");     -- 014
						vnext_state := st_tirq1;
					else
						if vbranch_flag = '1' then
						  if vuncond = '1' then
								null;
							else
								vv.jump_cmd.branch_address := vbranch_address;
								vv.jump_cmd.branch_cond := ireg(15) & ireg(10 downto 8);
								vv.jump_cmd.branch := '1';
								vnext_state := st_branch;
							end if;
						end if;
						if vsob_flag = '1' then
							vv.alu_cmd.src_reg := vsrc_reg;
							vv.alu_cmd.dst_reg := IMM_REG;
							vv.alu_cmd.dst_immed := x"FFFF";
							vv.alu_wb_cmd.reg := vsrc_reg;
							vv.alu_wb_cmd.reg_we := '1';
							vv.jump_cmd.branch_address := ibuffer_out.pc -(("000000000" & ireg(5 downto 0))- 1);
							vv.jump_cmd.branch := '1';
							vv.jump_cmd.sob := '1';
							vnext_state := st_branch;
						end if;
						if vrts_flag = '1' then
							vv.jump_cmd.jump := '1';
							if dst_reg = "111" then
								POP_REG(PC_REG);
								vnext_state := st_branch;
							else
								vv.alu_cmd.alu_oper := alu_add;
								vv.alu_cmd.src_reg := vdst_reg;
								vv.alu_cmd.dst_reg := ZERO_REG;
								vv.alu_wb_cmd.reg := PC_REG;
								vv.alu_wb_cmd.reg_we := '1';
								vnext_state := st_rts;
							end if;
						end if;
						if vtrap_flag = '1' then
							TRAP(vtrap_addr);
						end if;
						if vrti_flag = '1' or vrtt_flag = '1' then
							vv.jump_cmd.jump := '1';
							POP_REG(PC_REG);
							vnext_state := st_rti;
	   				end if;
						if vmark_flag = '1' then
							vv.alu_cmd.alu_oper := alu_add;
							vv.alu_cmd.src_reg := "0101";
							vv.alu_cmd.dst_reg := ZERO_REG;
							vv.alu_wb_cmd.reg := PC_REG;
							vv.alu_wb_cmd.reg_we := '1';
							vv.jump_cmd.jump := '1';
							vnext_state := st_mark1;
						end if;
						if vhalt_flag = '1' then
							vnext_state := st_halt;
						end if;
						if vnext_state = st_start then
							if vsrc_use_mem = '1' then
								vv.agu_cmd := vsrc_agu_cmd;
								if is_deferred_mode(vsrc_mode, src_reg) then
									LOAD_ALU(TEMP1);
									if vdst_use_mem = '1' then
										if is_deferred_mode(dst_mode, dst_reg) then
										  vnext_state := st_load_dst;
									  else
									    vnext_state := st_load_src_deferred;
										end if;
									else
										vnext_state := st_alu;
									end if;
								elsif vdst_use_mem = '1' then
									LOAD_ALU(TEMP1);
									if is_deferred_mode(dst_mode, dst_reg) then
										vnext_state := st_load_dst;
									else
										vnext_state := st_alu;
									end if;
								else
									vv.alu_cmd := vcmd;
									if vwrite_dst = '1' then
										vv.alu_wb_cmd.reg := vdst_reg;
										vv.alu_wb_cmd.reg_we := '1';
									end if;
								end if;
								vv.alu_cmd.src_reg := MEM_REG;
							else
			  				if vdst_use_mem = '1' then
									vv.agu_cmd := vdst_agu_cmd;
									if is_deferred_mode(dst_mode, dst_reg) then
										LOAD_ALU(TEMP2);
										vnext_state := st_alu;
									else
										vv.alu_cmd := vcmd;
										if vwrite_dst = '1' then
											vv.alu_wb_cmd.mem_we := '1';
										end if;
										if vload_dst = '0' then
										  vv.agu_cmd.rd := '0';
										end if;
		                if vload_dst = '1'then
									    vv.alu_cmd.dst_reg := MEM_REG;
									  end if;
									end if;
								else
									vv.alu_cmd := vcmd;
									if vwrite_dst = '1' then
										vv.alu_wb_cmd.reg := vdst_reg;
										vv.alu_wb_cmd.reg_we := '1';
									end if;
								end if;
							end if;
							if vnext_state = st_start then
								TEST_PC_DEST;
							end if;
						end if;
					end if;
			  when st_load_src_deferred =>
				  LOAD_TEMP(TEMP1, TEMP1, '0', '0');
					vv.agu_cmd.fbyte := vcmd.fbyte;
					vnext_state :=  st_alu;
				when st_load_dst =>
				  decode_dst;
					vv.agu_cmd := vdst_agu_cmd;
					LOAD_ALU(TEMP2);
					vv.alu_cmd.src_reg := MEM_REG;
					if vload_src = '1' and is_deferred_mode(vsrc_mode, src_reg) then
						vnext_state := st_load_src_deferred;
					else
						vnext_state := st_alu;
					end if;
		    when st_alu	=>
					decode_src;
					decode_dst;
				  vv.alu_cmd := vcmd;
					if vsrc_use_mem = '1' then
						if vdst_use_mem = '1' then
				      vv.alu_cmd.src_reg := TEMP1;
						else
							if is_deferred_mode(vsrc_mode, src_reg) then
							  vv.agu_cmd.base_reg := TEMP1;
							  vv.agu_cmd.load_areg := '1';
							  vv.agu_cmd.fbyte := vv.alu_cmd.fbyte;
								vv.agu_cmd.rd := '1';
							else
							  vv.agu_cmd := vsrc_agu_cmd;
							end if;
							vv.alu_cmd.src_reg := MEM_REG;
						end if;
					else
						if vsrc_use_immed = '1' then
							vv.alu_cmd.src_reg := IMM_REG;
						end if;
					end if;
					if vdst_use_mem = '1' then
					  if is_deferred_mode(dst_mode, dst_reg) then
							vv.agu_cmd.base_reg := TEMP2;
							vv.agu_cmd.load_areg := '1';
							vv.agu_cmd.fbyte := vv.alu_cmd.fbyte;
							if vload_dst = '1' then
								vv.agu_cmd.rd := '1';
					      vv.alu_cmd.dst_reg := MEM_REG;
							else
								vv.agu_cmd.rd := '0';
								vv.alu_cmd.dst_reg := ZERO_REG;
							end if;
						else
							vv.agu_cmd := vdst_agu_cmd;
							if vload_dst = '1' then
  						  vv.alu_cmd.dst_reg := MEM_REG;
							end if;
							if vload_dst = '0' then
								vv.agu_cmd.rd := '0';
							end if;
						end if;
					else
						if vdst_use_immed = '1' then
							vv.alu_cmd.dst_reg := IMM_REG;
						end if;
					end if;
					if vwrite_dst = '1' then
					  if vdst_use_mem = '1' then
							vv.alu_wb_cmd.mem_we := '1';
						else
							vv.alu_wb_cmd.reg := vdst_reg;
							vv.alu_wb_cmd.reg_we := '1';
						end if;
					end if;
					TEST_PC_DEST;
					vnext_state := st_start;
				when st_branch =>
				  bubble;
				  if (EX.jump_cmd.branch = '1' or EX.jump_cmd.jump = '1') then
						vnext_state := st_start;
					end if;
				when st_jsr1 =>
				  PUSH_REG(vsrc_reg);
				  if vsrc_reg = "111" then
						vv.alu_cmd.src_immed := vnext_pc & '0';
						vv.alu_cmd.src_reg := IMM_REG;
						vnext_state := st_branch;
					else
					  vnext_state := st_jsr2;
					end if;
				when st_jsr2 =>
					vv.alu_cmd.alu_oper := alu_add;
					vv.alu_cmd.src_reg := IMM_REG;
					vv.alu_cmd.src_immed := vnext_pc & '0';
					vv.alu_cmd.dst_reg := ZERO_REG;
					vv.alu_wb_cmd.reg_we := '1';
					vv.alu_wb_cmd.reg := vsrc_reg;
					vnext_state := st_branch;
				when st_rts =>
				  POP_REG(vdst_reg);
					vnext_state := st_branch;
				when st_mark1 =>
				  -- SP = PC + 2* NN
					vv.alu_cmd.alu_oper := alu_add;
					vv.alu_cmd.src_reg := IMM_REG;
					vv.alu_cmd.src_immed := vnext_pc & '0';
					vv.alu_cmd.dst_reg := IMM_REG;
					vv.alu_cmd.dst_immed := "000000000" & ireg(5 downto 0) & '0';
					vv.alu_wb_cmd.reg_we := '1';
					vv.alu_wb_cmd.reg := SP_REG;
				  vnext_state := st_mark2;
				when st_mark2 =>
					POP_REG("0101");
				  vnext_state := st_branch;
				when st_trap1 =>
				  PUSH_REG(PSW_REG);
					vnext_state := st_trap2;
				when st_trap2 =>
				  PUSH_NEXT_PC;
					vnext_state := st_trap3;
				when st_trap3 =>
				  -- PSW = (vtrap_addr_p2)
					vv.alu_cmd.alu_oper := alu_add;
					vv.alu_cmd.src_reg := MEM_REG;
					vv.alu_cmd.dst_reg := ZERO_REG;
					vv.agu_cmd.base_reg := ZERO_REG;
				  vv.agu_cmd.offset := x"00" & vtrap_addr_p2;
					vv.agu_cmd.rd := '1';
					vv.alu_wb_cmd.reg_we := '1';
					vv.alu_wb_cmd.reg := PSW_REG2;
					vnext_state := st_psw;
				when st_tirq1 =>
				  PUSH_REG(PSW_REG);
					vnext_state := st_tirq2;
				when st_tirq2 =>
					PUSH_PC;
					vnext_state := st_tirq3;
				when st_tirq3 =>
				  -- PSW = (016)
					vv.alu_wb_cmd.reg := PSW_REG2;
					vv.alu_cmd.alu_oper := alu_add;
					vv.alu_cmd.src_reg := MEM_REG;
					vv.alu_cmd.dst_reg := ZERO_REG;
					vv.agu_cmd.base_reg := ZERO_REG;
				  vv.agu_cmd.offset := x"00" & (ivec + 2);
					vv.agu_cmd.rd := '1';
					vv.alu_wb_cmd.reg_we := '1';
					vv.alu_wb_cmd.reg := PSW_REG2;
					vnext_state := st_psw;
				when st_irq1 =>
				  PUSH_REG(PSW_REG);
					vnext_state := st_irq2;
				when st_irq2 =>
					PUSH_PC;
					vnext_state := st_irq3;
				when st_irq3 =>
				  -- PSW = (016)
					vv.alu_wb_cmd.reg := PSW_REG2;
					vv.alu_cmd.alu_oper := alu_add;
					vv.alu_cmd.src_reg := MEM_REG;
					vv.alu_cmd.dst_reg := ZERO_REG;
					vv.agu_cmd.base_reg := ZERO_REG;
				  vv.agu_cmd.offset := x"00" & "00001110";  -- 016
					vv.agu_cmd.rd := '1';
					vv.alu_wb_cmd.reg_we := '1';
					vv.alu_wb_cmd.reg := PSW_REG2;
					vnext_state := st_psw;
				when st_rti =>
				  POP_REG(PSW_REG2);
					if vrtt_flag = '1' then
						vnext_t_req := '0';
						vnext_state := st_branch;
					else
				    vnext_state := st_branch_psw;
					end if;
				when st_branch_psw =>
				  if EX.jump_cmd.branch = '1' or EX.jump_cmd.jump = '1' then
						vnext_state := st_psw;
					end if;
				when st_psw =>
					null;
				when st_halt =>
				  vnext_state := st_halt;
			end case;

			if (vjump_flag = '1' or vjsr_flag = '1') and vnext_state = st_start and EX.jump_cmd.jump = '0' then
				vv.jump_cmd.jump := '1';
				vv.agu_cmd.rd	:= '0';
				vv.alu_wb_cmd.reg := PC_REG;
				vv.alu_wb_cmd.reg_we := '1';
				if vjsr_flag = '1' then
					vnext_state := st_jsr1;
				else
				  vnext_state := st_branch;
				end if;
			end if;

			if vpsw_we = '1' and vnext_state = st_start then
				vv.alu_wb_cmd.reg := PSW_REG;
				vv.alu_wb_cmd.reg_we := '1';
			end if;

      vv.insn := ID.insn;
			if not (state = st_branch or state = st_branch_psw) and (vid_stall = '1' or ID2_stall = '1') then
				vnext_state := state;
				bubble;
			end if;
			if vnext_state /= st_start then
				vid_stall := '1';
			end if;

			if vv.alu_cmd.alu_oper = alu_movb and vv.alu_wb_cmd.reg_we = '1' then
				vv.alu_cmd.fbyte := '0';
				vv.alu_wb_cmd.fbyte := '0';
			else
			  vv.alu_wb_cmd.fbyte := vv.alu_cmd.fbyte;
			end if;
		else
			vnext_state := state;
			bubble;
		end if;
		if dbg_out.halt = '1' then
			vnext_state := state;
			bubble;
		end if;

		if state = st_psw then
			if EX.alu_wb_cmd.reg_we = '1' and EX.alu_wb_cmd.reg = PSW_REG2 then
				vnext_t_req := alu_out.result(4);
				vnext_state := st_start;
			end if;
		end if;
		if state /= st_psw and vnext_state = st_start then
			vnext_t_req := T;
		end if;

		next_t_req <= vnext_t_req;
		next_state <= vnext_state;
		ID_stall <= vid_stall or ID2_stall;
		next_ID2 <= vv;
		IF_in.uncond_branch_address <= vbranch_address;
		IF_in.uncond_branch <= vbranch_flag and vuncond;
	end process ID_stage;

	ID2_stage: process (ID2, AG_stall, AG, next_MEM, MEM, EX, alu_out, gpr_out)
	  variable vv: AG_stage_type;
		variable vid2_stall: std_logic;

		procedure bubble is
		begin
			vv := AG_empty;
		end procedure bubble;
	begin
		vv.alu_cmd := ID2.alu_cmd;
		vv.agu_cmd := ID2.agu_cmd;
		vv.alu_wb_cmd := ID2.alu_wb_cmd;
		vv.jump_cmd := ID2.jump_cmd;
		vid2_stall := '0';

		-- address register bypass
		if next_MEM.agu_wb_cmd.reg_we = '1' and next_MEM.agu_wb_cmd.base_reg = vv.agu_cmd.base_reg then
			vv.agu_cmd.base := next_MEM.agu_wb_cmd.value;
		elsif MEM.agu_wb_cmd.reg_we = '1' and MEM.agu_wb_cmd.base_reg = vv.agu_cmd.base_reg then
			vv.agu_cmd.base := MEM.agu_wb_cmd.value;
		elsif EX.agu_wb_cmd.reg_we = '1' and EX.agu_wb_cmd.base_reg = vv.agu_cmd.base_reg then
			vv.agu_cmd.base := EX.agu_wb_cmd.value;
		elsif EX.alu_wb_cmd.reg_we = '1' and EX.alu_wb_cmd.reg = vv.agu_cmd.base_reg then
			vv.agu_cmd.base(7 downto 0) := alu_out.result(7 downto 0);
			if EX.alu_wb_cmd.fbyte = '0' then
				vv.agu_cmd.base(15 downto 8) := alu_out.result(15 downto 8);
			else
				vv.agu_cmd.base(15 downto 8) := gpr_out.rd_data3(15 downto 8);
			end if;
		else
			vv.agu_cmd.base := gpr_out.rd_data3;
		end if;

		-- stall if address register computation in progress on stages AG or MEM
		if ID2.agu_cmd.load_areg = '1' and
			 ((AG.alu_wb_cmd.reg_we = '1' and vv.agu_cmd.base_reg = AG.alu_wb_cmd.reg) or
			 (MEM.alu_wb_cmd.reg_we = '1' and vv.agu_cmd.base_reg = MEM.alu_wb_cmd.reg)) then
			vid2_stall := '1';
			bubble;
		end if;
		vv.insn := ID2.insn;
		next_AG <= vv;
		ID2_stall <= vid2_stall or (not ID2.bubble and AG_stall);
	end process ID2_stage;

	AG_stage: process (AG, MEM, EX, WB, MEM_stall)
	  variable vv: mem_stage_type;
		variable address: std_logic_vector(WORD_SIZE-1 downto 0);
		variable mem_address: std_logic_vector(WORD_SIZE-1 downto 0);
		variable vag_stall: std_logic;

		procedure bubble is
		begin
			vv := MEM_empty;
		end procedure bubble;
	begin
		vag_stall := '0';
		address := AG.agu_cmd.base + AG.agu_cmd.offset;

		if AG.agu_cmd.reg_post_modify = '1' then
			mem_address := AG.agu_cmd.base;
		else
			mem_address := address;
		end if;
		vv.mem_cmd.address := mem_address;
		vv.mem_cmd.rd := AG.agu_cmd.rd;
		vv.mem_cmd.fbyte := AG.agu_cmd.fbyte;
		vv.agu_wb_cmd.value := address;
		vv.agu_wb_cmd.base_reg := AG.agu_cmd.base_reg;
		-- Suppress address register write if destination register is same as address register
		if AG.alu_wb_cmd.reg_we = '1' and AG.agu_cmd.base_reg = AG.alu_wb_cmd.reg then
			vv.agu_wb_cmd.reg_we := '0';
		else
		  vv.agu_wb_cmd.reg_we := AG.agu_cmd.reg_we;
		end if;
		vv.alu_cmd := AG.alu_cmd;
		vv.alu_wb_cmd := AG.alu_wb_cmd;
		vv.jump_cmd := AG.jump_cmd;
		vv.insn := AG.insn;
		if AG.agu_cmd.rd = '1' and
		   (MEM.alu_wb_cmd.mem_we = '1' or EX.alu_wb_cmd.mem_we = '1' or WB.alu_wb_cmd.mem_we = '1') then
		  bubble;
			vag_stall := '1';
		end if;

		next_MEM <= vv;
		AG_stall <= vag_stall or MEM_stall;
	end process AG_stage;

	MEM_stage: process (MEM, EX, EX_stall, WB, gpr_out, dcache_out, alu_out)
	  variable vv: ex_stage_type;
		variable vmem_data: std_logic_vector(WORD_SIZE-1 downto 0);
		variable vmem_stall: std_logic;

		procedure bubble is
		begin
			vv := EX_empty;
		end procedure bubble;
	begin
		vmem_stall := '0';
		if MEM.mem_cmd.rd = '1' then
			if MEM.mem_cmd.fbyte = '1' then
				if MEM.mem_cmd.address(0) = '1' then
					vmem_data := "00000000" & dcache_out.data(15 downto 8);
				else
					vmem_data := "00000000" & dcache_out.data(7 downto 0);
				end if;
			else
				vmem_data := dcache_out.data;
			end if;
		else
			vmem_data := MEM.mem_cmd.address;
		end if;
		vv.alu_cmd := MEM.alu_cmd;

		-- source & destination bypass
		if EX.agu_wb_cmd.reg_we = '1' and EX.agu_wb_cmd.base_reg = MEM.alu_cmd.src_reg then
			vv.src := EX.agu_wb_cmd.value;
		elsif EX.alu_wb_cmd.reg_we = '1' and EX.alu_wb_cmd.reg = MEM.alu_cmd.src_reg then
			vv.src(7 downto 0) := alu_out.result(7 downto 0);
			if EX.alu_wb_cmd.fbyte = '0' then
			  vv.src(15 downto 8) := alu_out.result(15 downto 8);
			else
				vv.src(15 downto 8) := gpr_out.rd_data1(15 downto 8);
			end if;
		else
		  vv.src := gpr_out.rd_data1;
		end if;
		if EX.agu_wb_cmd.reg_we = '1' and EX.agu_wb_cmd.base_reg = MEM.alu_cmd.dst_reg then
			vv.dst := EX.agu_wb_cmd.value;
		elsif EX.alu_wb_cmd.reg_we = '1' and EX.alu_wb_cmd.reg = MEM.alu_cmd.dst_reg then
			vv.dst(7 downto 0) := alu_out.result(7 downto 0);
			if EX.alu_wb_cmd.fbyte = '0' then
			  vv.dst(15 downto 8) := alu_out.result(15 downto 8);
			else
				vv.dst(15 downto 8) := gpr_out.rd_data2(15 downto 8);
			end if;
		else
		  vv.dst := gpr_out.rd_data2;
		end if;
		vv.mem_address := MEM.mem_cmd.address;
		vv.alu_wb_cmd := MEM.alu_wb_cmd;
		vv.agu_wb_cmd := MEM.agu_wb_cmd;
		vv.jump_cmd := MEM.jump_cmd;
		vv.insn := MEM.insn;
		-- stall if EX or WB stage contains memory write to same address as read on this stage
		if (MEM.mem_cmd.rd = '1' and
			 ((EX.alu_wb_cmd.mem_we = '1' and EX.mem_address(15 downto 1) = MEM.mem_cmd.address(15 downto 1)) or
			 (WB.alu_wb_cmd.mem_we = '1' and WB.mem_address(15 downto 1) = MEM.mem_cmd.address(15 downto 1)))) then
			bubble;
			vmem_stall := '1';
			mem_raw_hazard <= '1';
		else
			mem_raw_hazard <= '0';
		end if;
		if MEM.mem_cmd.rd = '1' and dcache_out.data_rdy = '0' then
			bubble;
			vmem_stall := '1';
		end if;
		vv.insn := MEM.insn;
		next_EX <= vv;
		mem_data <= vmem_data;
		MEM_stall <= vmem_stall or EX_stall;
	end process MEM_stage;

	EX_stage: process (EX, WB, WB_stall, alu_out)
	  variable vv: wb_stage_type;
		variable v_data: std_logic_vector(WORD_SIZE-1 downto 0);
		variable vex_stall: std_logic;

--		procedure bubble is
--		begin
--		  vv := WB_empty;
--		end procedure bubble;
	begin
		vex_stall := '0';
		vv.mem_address := EX.mem_address;
		v_data := alu_out.result;
		vv.alu_wb_cmd := EX.alu_wb_cmd;
		vv.agu_wb_cmd := EX.agu_wb_cmd;
		vv.insn := EX.insn;

		if EX.alu_wb_cmd.fbyte = '1' then
			if EX.mem_address(0) = '0' then
				vv.mem_byteen := "01";
				vv.mem_data := v_data;
			else
				vv.mem_byteen := "10";
				vv.mem_data := v_data(7 downto 0) & v_data(7 downto 0);
			end if;
		else
		  vv.mem_byteen := "11";
			vv.mem_data := v_data;
		end if;

    IF_in.branch_address <= EX.jump_cmd.branch_address;
		if EX.jump_cmd.jump = '1' then
			IF_in.branch <= '0';
			IF_in.jump <= '1';
		else
			IF_in.jump <= '0';
			if EX.jump_cmd.sob = '1' then
				if EX.src /= 1 then
					IF_in.branch <= '1';
				else
					IF_in.branch <= '0';
				end if;
			else
			  IF_in.branch <= EX.jump_cmd.branch and is_branch_taken(EX.jump_cmd.branch_cond, N, Z, V, C);
			end if;
		end if;
		vv.insn := EX.insn;
		EX_stall <= vex_stall or WB_stall;
		next_WB <= vv;
	end process EX_stage;

	WB_stall <= '1' when WB.alu_wb_cmd.mem_we = '1' and dcache_out.wr_rdy = '0' else '0';

	stage_clock: process(reset, clk)
	begin
		if reset = '1' then
			state <= st_start;
			t_req <= '0';
			IFe.pc <= START_ADDRESS(15 downto 1);
			IFe.jump <= '0';
			IFe.rdy <= '0';
			ID2 <= ID2_empty;
			AG <= AG_empty;
			MEM <= MEM_empty;
			EX <= EX_empty;
			WB <= WB_empty;
		elsif rising_edge(clk) then
			state <= next_state;
			t_req <= next_t_req;
			IFe <= next_IF;

			if ID2_stall = '0' then
			  ID2 <= next_ID2;
			end if;

			if AG_stall = '0' then
			  AG <= next_AG;
        if ID2_stall = '1' then
          ID2.insn.rdy <= '0';
        end if;
			end if;

			if MEM_stall = '0' then
			  MEM <= next_MEM;
        if AG_stall = '1' then
          AG.insn.rdy <= '0';
        end if;
			end if;

			if EX_stall = '0' then
			  EX <= next_EX;
        if MEM_stall = '1' then
          MEM.insn.rdy <= '0';
        end if;
			end if;

			if WB_stall = '0' then
			  WB <= next_WB;
        if EX_stall = '1' then
          EX.insn.rdy <= '0';
        end if;
			end if;
		end if;
	end process stage_clock;

	PSW_proc: process (reset, clk)
	begin
		if reset = '1' then
			psw(15 downto 8) <= (others => '0');
			psw(7 downto 0) <= "10000000";
		elsif	rising_edge(clk) then
			if WB_stall = '0' then
				if EX.alu_wb_cmd.reg_we = '1' and EX.alu_wb_cmd.reg = PSW_REG then  -- MTPS command
			    psw(15 downto 9) <= (others => '0');
					if HALT_USER = '1' then
						psw(8 downto 0) <= HALT_USER & alu_out.result(7 downto 0);
					else	 -- preserve T bit
					   psw(8 downto 0) <= HALT_USER & alu_out.result(7 downto 5) & T & alu_out.result(3 downto 0);
					end if;
				elsif EX.alu_wb_cmd.reg_we = '1' and EX.alu_wb_cmd.reg = PSW_REG2 then  -- RTI command
					if EX.insn.pc(15 downto 13) = "111" then
						psw(8 downto 0) <= alu_out.result(8 downto 0);
					else	 -- PC < 0160000
						psw(8 downto 0) <= HALT_USER & alu_out.result(7 downto 0);
					end if;
				else
			     psw(15 downto 8) <= (others => '0');
		       psw(7 downto 0) <= P & psw(6) & psw(5) & T & alu_out.n & alu_out.z & alu_out.v & alu_out.c;
				end if;
			end if;
  	end if;
	end process PSW_proc;
end behaviour;
