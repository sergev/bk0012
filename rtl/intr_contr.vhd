library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all; 
use IEEE.STD_LOGIC_ARITH.all;

entity intr_contr is
	port
	(
		reset: in std_logic;
		clk:   in std_logic;
		
		virq1: in std_logic;
		virq2: in std_logic;
		irq2: in std_logic;
		irq3: in std_logic := '0';
		
    irq: out std_logic;
    ivec: out std_logic_vector(7 downto 0);
		
		iack: in std_logic := '0';
		
		-- Auxialry Avalon bus interface	 
		slv1_chipselect: in std_logic;
		slv1_read: in std_logic;
		slv1_readdata: out std_logic_vector(15 downto 0)
	);	
end intr_contr;

architecture behaviour of intr_contr is
  function priority(v: std_logic_vector(7 downto 0)) return std_logic_vector is
  begin
    if v(7) = '1' then return ("111");
    elsif v(6) = '1' then return ("110"); 
    elsif v(5) = '1' then return ("101");
    elsif v(4) = '1' then return ("100");
    elsif v(3) = '1' then return ("011");
    elsif v(2) = '1' then return ("010");
    elsif v(1) = '1' then return ("001");
    else return ("000");  
    end if;  
  end; 	
	
  function decode(v : std_logic_vector) return std_logic_vector is
    variable res : std_logic_vector((2**v'length)-1 downto 0); --'
    variable i : natural;
  begin
    res := (others => '0');
    i := conv_integer(unsigned(v));
    res(i) := '1';
    return(res);
  end;	
	
	signal irq_inputs: std_logic_vector(7 downto 0);
  signal irq_del: std_logic_vector(7 downto 0);
  signal irq_pend: std_logic_vector(7 downto 0); 
  signal cur_level, next_cur_level: std_logic_vector(2 downto 0);
  signal irq_end: std_logic;
	signal int_vec: std_logic_vector(7 downto 0);
	signal vect_rd: std_logic;
begin													 
--	irq_inputs <= "0000" & irq3 & irq2 & virq2 & virq1;	
  irq_inputs <= "0000" & "0" & irq2 & virq2 & virq1;
	irq_end <= iack or vect_rd;

  irq_del_proc: process (reset, clk)
  begin
    if reset = '1' then
      irq_del <= (others => '0');
    elsif rising_edge(clk) then
      irq_del <= irq_inputs;
    end if;
  end process irq_del_proc;	
	
  irq_pend_proc: process (reset, clk, irq_inputs, irq_pend, irq_del, cur_level, irq_end)
    variable tmp: std_logic_vector(7 downto 0); 
  begin
    tmp:= irq_pend or (irq_inputs and not irq_del); 
    if irq_end = '1' then
      tmp:= tmp and not decode(cur_level);        
    end if;
		next_cur_level <= priority(tmp);
    if reset = '1' then
      irq_pend <= (others => '0');
    elsif rising_edge(clk) then  
      irq_pend <= tmp;
    end if;
  end process irq_pend_proc;

  cur_level_proc: process (reset, clk)
  begin       
    if reset = '1' then
      cur_level <= (others => '0');
    elsif rising_edge(clk) then  
      cur_level <= next_cur_level;
    end if;
  end process cur_level_proc;  
  
	vect_proc: process (irq_pend, cur_level)
	begin
		if irq_pend /= 0 then
	    case cur_level is
		    when "000" => 				
			    int_vec <= x"30";  -- VIRQ from keyboard 0060 
		    when "001" =>
			    int_vec <= x"BC";  -- VIRQ from keyboard 0274
		    when "010" =>
			    int_vec <= x"40";  -- VIRQ from irq2 0100
		    when "011" =>
			    int_vec <= x"B8";  -- VIRQ from irq2 0270
		    when others =>
		      int_vec <= (others => '0');
	    end case;	
		else
	    int_vec <= (others => '0');
		end if;	
	end process vect_proc;	

	external_bus: process(slv1_chipselect, slv1_read, int_vec)
	begin
		if slv1_chipselect = '1' and slv1_read = '1' then
			slv1_readdata <= "00000000" & int_vec;
			vect_rd <= '1';
		else
			slv1_readdata <= (others => '0');
			vect_rd <= '0';
		end if;	
	end process external_bus;	

--	irq <= '1' when irq_pend /= 0 else '0';
	irq <= '0';
	ivec <= int_vec;
end behaviour;
