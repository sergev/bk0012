library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_ARITH.all;			 
use IEEE.NUMERIC_STD.all;

entity display is
  port
  (
		reset: in std_logic;
		clk:   in std_logic;
		
		mem_clk: in std_logic;
		mem_address: out std_logic_vector(31 downto 0);
		mem_burstcount: out std_logic_vector(3 downto 0);
		mem_read: out std_logic;
		mem_readdata: in std_logic_vector(31 downto 0);
		mem_readdatavalid: in std_logic;
		mem_waitrequest: in std_logic;		
		
		-- Slave on address 177664
		slv1_clk: in std_logic;
		slv1_chipselect: in std_logic;
		slv1_read: in std_logic;
		slv1_write: in std_logic;
		slv1_writedata: in std_logic_vector(15 downto 0);
		slv1_readdata: out std_logic_vector(15 downto 0);
		
		palette: in std_logic_vector(3 downto 0):= (others => '0');
		screen_buf: in std_logic:= '0';		
		
		RED: out std_logic_vector(7 downto 0);
		GREEN: out std_logic_vector(7 downto 0);
		BLUE:  out std_logic_vector(7 downto 0);
		nRAMDAC_BLANK: out std_logic;
		nRAMDAC_SYNC: out std_logic;
		RAMDAC_CLK: out std_logic;
		HSYNC: out std_logic;
		VSYNC: out std_logic;
	
    DVI_D: out std_logic_vector(11 downto 0);
	  DVI_DE: out std_logic;
	  DVI_CLK: out std_logic	
  );	
end display;

architecture behaviour of display is
  constant FIFO_DEPTH: natural:= 128;

	constant RGB16_RED_WIDTH: natural:=	5;
	constant RGB16_GREEN_WIDTH: natural:= 6;
	constant RGB16_BLUE_WIDTH: natural:= 5;
	constant RGB16_BLUE_OFFSET: natural:=	0;
	constant RGB16_GREEN_OFFSET: natural:=	RGB16_BLUE_WIDTH;
	constant RGB16_RED_OFFSET: natural:= (RGB16_BLUE_WIDTH+ RGB16_GREEN_WIDTH);	
	
	type VGA_timings is 
	record
		h_polarity: boolean;
	  h_visible_area: natural;
		h_front_porch: natural;
		h_sync_pulse: natural;
		h_back_porch: natural;
		whole_line: natural;	
		v_polarity: boolean;
	  v_visible_area: natural;
		v_front_porch: natural;
		v_sync_pulse: natural;
		v_back_porch: natural;
		whole_frame: natural;
	end record;
	
	-- 640 x 350 x 70 Hz , f = 25.175 Mhz
	constant VGA_640x350x70: VGA_timings :=
	(			
	  h_polarity => true,     -- positive
	  h_visible_area => 640,	-- 25.422045680238 us
		h_front_porch => 16,		-- 0.63555114200596 us
		h_sync_pulse => 96,			-- 3.8133068520357 us 
		h_back_porch => 48,	  	-- 1.9066534260179 us
		whole_line => 800,	 		-- 31.777557100298 us
		v_polarity => false,    -- negative
		v_visible_area => 350,	-- 11.122144985104 ms
		v_front_porch => 37,		-- 1.175769612711 ms
		v_sync_pulse => 2,			-- 0.063555114200596 ms
		v_back_porch => 60,			-- 1.9066534260179 ms
		whole_frame => 449			-- 14.268123138034 ms
	);

	-- 512 x 256 x 70 Hz , f = 25.175 Mhz
	constant BK_512x256x70: VGA_timings :=
	(			
	  h_polarity => true,     -- positive
	  h_visible_area => 512,	-- 
		h_front_porch => 80,		-- 
		h_sync_pulse => 96,			-- 
		h_back_porch => 112,	  -- 
		whole_line => 800,	 		-- 
		v_polarity => false,    -- negative
		v_visible_area => 256,	-- 
		v_front_porch => 84,		-- 
		v_sync_pulse => 2,			-- 
		v_back_porch => 107,			-- 
		whole_frame => 449			-- 
	);	
	
	-- 640 x 350 x 85 Hz , f = 31.5 Mhz
	constant VESA_640x350x85: VGA_timings :=
	(			
	  h_polarity => true,     -- positive
	  h_visible_area => 640,	-- 20.31746031746 us
		h_front_porch => 32,		-- 1.015873015873 us
		h_sync_pulse => 64,			-- 2.031746031746 us 
		h_back_porch => 96,	  	-- 3.047619047619 us
		whole_line => 832,	 		-- 26.412698412698 us
		v_polarity => false,    -- negative
		v_visible_area => 350,	-- 9.2444444444444 ms
		v_front_porch => 32,		-- 0.84520634920635 ms
		v_sync_pulse => 3,			-- 0.079238095238095 ms
		v_back_porch => 60,			-- 1.5847619047619 ms
		whole_frame => 445			-- 11.753650793651 ms
	);	
	
		-- 640 x 480 x 60 Hz , f = 25.175 Mhz	Industry standard timing
	constant VGA_640x480x60: VGA_timings :=
	(			
	  h_polarity => false,    -- negative
	  h_visible_area => 640,	-- 25.422045680238 us
		h_front_porch => 16,		-- 0.63555114200596 us
		h_sync_pulse => 96,			-- 3.8133068520357 us 
		h_back_porch => 48,		  -- 1.9066534260179 us
		whole_line => 800,	 		-- 31.777557100298 us
		v_polarity => false,    -- negative
		v_visible_area => 480,	-- 15.253227408143 ms
		v_front_porch => 10,		-- 0.31777557100298 ms
		v_sync_pulse => 2,			-- 0.063555114200596 ms
		v_back_porch => 33,			-- 1.0486593843098 ms
		whole_frame => 525			-- 16.683217477656 ms
	);
	
	-- 640 x 480 x 73 Hz , f = 31.5 Mhz
	constant VGA_640x480x73: VGA_timings :=
	(			
	  h_polarity => false,    -- negative
	  h_visible_area => 640,	-- 20.31746031746 us
		h_front_porch => 24,		-- 0.76190476190476 us
		h_sync_pulse => 40,			-- 1.2698412698413 us 
		h_back_porch => 128,		-- 4.0634920634921 us
		whole_line => 832,	 		-- 26.412698412698 us
		v_polarity => false,    -- negative
		v_visible_area => 480,	-- 12.678095238095 ms
		v_front_porch => 9,			-- 0.23771428571429 ms
		v_sync_pulse => 2,			-- 0.052825396825397 ms
		v_back_porch => 29,			-- 0.76596825396825 ms
		whole_frame => 520			-- 13.734603174603 ms
	);		
	
	-- 640 x 480 x 75 Hz , f = 31.5 Mhz
	constant VESA_640x480x75: VGA_timings :=
	(			
	  h_polarity => false,    -- negative
	  h_visible_area => 640,	-- 20.31746031746 us
		h_front_porch => 16,		-- 0.50793650793651 us
		h_sync_pulse => 64,			-- 2.031746031746 us 
		h_back_porch => 120,		-- 3.8095238095238 us
		whole_line => 840,	 		-- 26.666666666667 us
		v_polarity => false,    -- negative
		v_visible_area => 480,	-- 12.8 ms
		v_front_porch => 1,			-- 0.026666666666667 ms
		v_sync_pulse => 3,			-- 0.08 ms
		v_back_porch => 16,			-- 0.42666666666667 ms
		whole_frame => 500			-- 13.333333333333 ms
	);	
	
	-- 768 x 576 x 75 Hz , f = 45.51 Mhz
	constant VESA_768x576x75: VGA_timings :=
	(			
	  h_polarity => false,    -- negative
	  h_visible_area => 768,	-- 16.875411997363 us
		h_front_porch => 40,		-- 0.878927708196 us
		h_sync_pulse => 80,			-- 1.757855416392 us 
		h_back_porch => 120,		-- 2.636783124588 us
		whole_line => 1008,	 		-- 22.148978246539 us
		v_polarity => true,     -- positive
		v_visible_area => 576,	-- 12.757811470007 ms
		v_front_porch => 1,			-- 0.022148978246539 ms
		v_sync_pulse => 3,			-- 0.066446934739618 ms
		v_back_porch => 22,			-- 0.48727752142386 ms
		whole_frame => 602			-- 13.333684904417 ms
	);	
	
	-- 800 x 600 x 72 Hz , f = 50.0 Mhz
	constant VGA_640x480x72: VGA_timings :=
	(										 
	  h_polarity => true,		  -- pisitive
	  h_visible_area => 800,	-- 16 us
		h_front_porch => 56,		-- 1.12 us
		h_sync_pulse => 120,		-- 2.4 us
		h_back_porch => 64,			-- 1.28 us
		whole_line => 1040,			-- 20.8 us
		v_polarity => true,		  -- positive
		v_visible_area => 600,	-- 12.48 ms
		v_front_porch => 37,		-- 0.7696 ms
		v_sync_pulse => 6,			-- 0.1248 ms
		v_back_porch => 23,			-- 0.4784 ms
		whole_frame => 666      -- 13.8528 ms
	);	
	
	component display_fifo is
	  port
	  (
			data		: in std_logic_vector(31 downto 0);
			wrreq		: in std_logic;
			rdreq		: in std_logic;
			rdclk		: in std_logic;
			wrclk		: in std_logic;
			aclr		: in std_logic;
			q		: out std_logic_vector(31 downto 0);
			rdempty		: out std_logic;
			wrusedw		: out std_logic_vector(6 downto 0)
		);
	end component;

	-- Register ob address 177664
	signal roll_offset: std_logic_vector(7 downto 0);
	signal full_screen: std_logic;
	
	signal display_clk: std_logic;
	
	constant BW_MODE: std_logic_vector(2 downto 0) := "000";
	constant COLOR2_MODE: std_logic_vector(2 downto 0) := "001";
	constant COLOR16_MODE: std_logic_vector(2 downto 0) := "100";
	signal color_mode: std_logic_vector(2 downto 0) := BW_MODE;
	
--  signal hsync_end: std_logic_vector(10 downto 0) := conv_std_logic_vector(93, 11);  -- 3.77 us
--	signal hblank_start: std_logic_vector(10 downto 0) := conv_std_logic_vector(770- 59, 11);  -- 31.77 us - 0.94 us 
--	signal hblank_end: std_logic_vector(10 downto 0) := conv_std_logic_vector(141+ 59, 11);  -- 3.77 us + 1.89 us;
--	signal hsize: std_logic_vector(10 downto 0) := conv_std_logic_vector(793, 11);  -- 31.77 us
--  signal vsync_end: std_logic_vector(10 downto 0) := conv_std_logic_vector(1, 11);  -- 0.06 ms
--	signal vblank_start: std_logic_vector(10 downto 0) := conv_std_logic_vector(410- 47, 11);  -- 14.27 ms - 1.2 ms
--	signal vblank_end: std_logic_vector(10 downto 0) := conv_std_logic_vector(60+ 48, 11);  -- 1.88 ms + 0.06 ms	
--	signal vsize: std_logic_vector(10 downto 0) := conv_std_logic_vector(449, 11);  -- 14.27 ms

  type internal_timings is 
  record
		h_sync_inv: std_logic;
  	h_sync_end: std_logic_vector(10 downto 0);
	  h_blank_start: std_logic_vector(10 downto 0);
	  h_blank_end: std_logic_vector(10 downto 0);
	  h_size: std_logic_vector(10 downto 0);
		v_sync_inv: std_logic;
    v_sync_end: std_logic_vector(10 downto 0);
	  v_blank_start: std_logic_vector(10 downto 0);
	  v_blank_end: std_logic_vector(10 downto 0);
	  v_size: std_logic_vector(10 downto 0);	
	end record;

	function convert_timings(t: VGA_timings) return internal_timings is
	  variable v: internal_timings;
	begin																	
		assert t.h_visible_area + t.h_front_porch+ t.h_sync_pulse+ t.h_back_porch = t.whole_line 
		  report "Invalid horizontal timings" severity error;	
		assert t.v_visible_area + t.v_front_porch+ t.v_sync_pulse+ t.v_back_porch = t.whole_frame 
		  report "Invalid vertical timings" severity error;		
		if t.h_polarity then
			v.h_sync_inv := '0';
		else
			v.h_sync_inv := '1';
		end if;	
		v.h_sync_end := conv_std_logic_vector(t.h_sync_pulse- 1, 11);
		v.h_blank_start := conv_std_logic_vector(t.whole_line- t.h_back_porch- 1, 11);
		v.h_blank_end := conv_std_logic_vector(t.h_sync_pulse+ t.h_front_porch- 1, 11);
	  v.h_size := conv_std_logic_vector(t.whole_line- 1, 11);
		if t.v_polarity then
			v.v_sync_inv := '0';
		else
			v.v_sync_inv := '1';
		end if;		
		v.v_sync_end := conv_std_logic_vector(t.v_sync_pulse- 1, 11);
		v.v_blank_start := conv_std_logic_vector(t.whole_frame- t.v_back_porch- 1, 11);
		v.v_blank_end := conv_std_logic_vector(t.v_sync_pulse+ t.v_front_porch- 1, 11);
		v.v_size := conv_std_logic_vector(t.whole_frame- 1, 11);
		return v;
	end function;	

  signal display_clk_cnt: std_logic_vector(0 downto 0);
	
	signal tms: internal_timings;
	signal shift_cnt: std_logic_vector(4 downto 0);
	signal hcnt: std_logic_vector(10 downto 0);
	signal vcnt: std_logic_vector(10 downto 0);
	signal hblank: std_logic;
	signal hsyncb: std_logic;
	signal vblank: std_logic;
	signal vsyncb: std_logic;	

	signal start_address: std_logic_vector(21 downto 0) := (others => '0'); -- conv_std_logic_vector(16384/4 , 22);
	signal address_cnt: std_logic_vector(21 downto 0);
	
	signal fifo_data: std_logic_vector(31 downto 0);
	signal fifo_wrreq: std_logic;
	signal fifo_rdreq: std_logic;
	signal fifo_rdclk: std_logic;
	signal fifo_aclr: std_logic;
	signal fifo_q: std_logic_vector(31 downto 0);
	signal fifo_rdempty: std_logic;
	signal fifo_wrusedw: std_logic_vector(6 downto 0);	

	type state_type is (st_idle, st_request, st_read);
	signal state: state_type;
	signal burst_cnt: std_logic_vector(2 downto 0);
	
	signal blank: std_logic;
	signal blank_del, blank_del2, blank_del3: std_logic;
	signal rcolor: std_logic_vector(7 downto 0);
	signal gcolor: std_logic_vector(7 downto 0);
	signal bcolor: std_logic_vector(7 downto 0);	
begin
--	tms <= convert_timings(VGA_640x480x60);	
	 tms <= convert_timings(BK_512x256x70);
	
	clk_proc: process (reset, clk)
	begin
	  if reset = '1' then
	    display_clk_cnt <= (others => '0');
	  elsif rising_edge(clk) then 
	    display_clk_cnt <= display_clk_cnt + 1; 
	  end if;
	end process clk_proc;
  display_clk <= display_clk_cnt(display_clk_cnt'high);
	fifo_rdclk <= display_clk;
	fifo_data <= mem_readdata;
	
  id_fifo: display_fifo
	  port map
		(
		  data => fifo_data,
		  wrreq	=> fifo_wrreq,
		  rdreq	=> fifo_rdreq,
		  rdclk	=> fifo_rdclk,
		  wrclk	=> mem_clk,
		  aclr	=> fifo_aclr,
		  q	=> fifo_q,
		  rdempty => fifo_rdempty,
		  wrusedw	=> fifo_wrusedw		
		);	

  horizontal: process(reset, display_clk)	
	  variable next_sync: std_logic;
  begin
    if reset = '1' then
	    hcnt <= (others => '0'); 
			hblank <= '1';
			hsyncb <= '1';
	  elsif rising_edge(display_clk) then	
			next_sync := hsyncb;
			if hcnt = tms.h_sync_end then
				next_sync := '0';	
			end if;	
			if hcnt = tms.h_blank_start then
				hblank <= '1';
			elsif hcnt = tms.h_blank_end then
				hblank <= '0';
			end if;	
	    if hcnt = tms.h_size then
				hcnt <= (others => '0');
				next_sync :='1';
			else	
	      hcnt <= hcnt + 1;
	    end if;				
			hsyncb <= next_sync;
	  end if;
  end process horizontal;
	
	vertical: process (reset, hsyncb)
	  variable next_sync: std_logic;
  begin
		if reset = '1' then
			vcnt <= (others => '0');
			vblank <= '1';
			vsyncb <= '1';
		elsif falling_edge(hsyncb) then	 
			next_sync := vsyncb;
			if vcnt = tms.v_sync_end then
				next_sync := '0';
			end if;	
			if vcnt = tms.v_blank_start then
				vblank <= '1';
			elsif vcnt = tms.v_blank_end then
				vblank <= '0';
			end if;	
			if vcnt = tms.v_size then	
				vcnt <= (others => '0');
				next_sync := '1';
			else
				vcnt <= vcnt + 1;
			end if;						 
			vsyncb <= next_sync;
		end if;	
	end process vertical;	
	
  fifo_flush: process (reset, mem_clk, vsyncb)
	  variable v_blank_prev1, v_blank_prev2: std_logic;
  begin
		if reset = '1' then
			v_blank_prev1:= '0';
			v_blank_prev2:= '0';
    elsif rising_edge(mem_clk) then
			v_blank_prev2:= v_blank_prev1;
      v_blank_prev1:= vsyncb;
    end if;
    if (v_blank_prev1 = '0') and (v_blank_prev2 = '1') then
      fifo_aclr <= '1';
    else
      fifo_aclr <= '0';
    end if;  
  end process fifo_flush;	
				  
	address_cnt_proc: process (reset, mem_clk)
	begin
		if reset = '1' then
			address_cnt <= start_address;
		elsif rising_edge(mem_clk) then
			if fifo_aclr = '1' then
				address_cnt <= start_address;
			elsif fifo_wrreq = '1' then 
			  address_cnt <= address_cnt + 1;
			end if;	
		end if;
	end process address_cnt_proc;		
	
	read_memory: process (reset, mem_clk)
	begin
		if reset = '1' then
			state <= st_idle;			
			burst_cnt <= (others => '0');
		elsif rising_edge(mem_clk) then	
			case state is
				when st_idle =>
				  if fifo_wrusedw(fifo_wrusedw'high) = '0' and (vcnt > tms.v_sync_end)  then
						state <= st_request;
					end if;	
				when st_request =>
				  if mem_waitrequest = '0' then
						state <= st_read;
					end if;	
				when st_read =>
					if mem_readdatavalid = '1' then
						if burst_cnt = "111" then
							state <= st_idle;
						end if;	
						burst_cnt <= burst_cnt + 1;
					end if;	
				when others => null;
			end case;
		end if;
	end process read_memory;	
	
  pixel_shifter: process (reset, display_clk)
	  variable data: std_logic_vector(31 downto 0);
		variable cnt: std_logic_vector(4 downto 0);
		variable vshift_cnt: std_logic_vector(4 downto 0);
	  variable color1: std_logic;
		variable color2: std_logic_vector(1 downto 0); 
		variable color16: std_logic_vector(15 downto 0);
  begin
    if reset = '1' then
	    rcolor <= (others => '0'); 
			gcolor <= (others => '0');
			bcolor <= (others => '0');
			shift_cnt <= (others => '0');
	  elsif rising_edge(display_clk) then	      
			data := fifo_q;
			vshift_cnt := shift_cnt + 1;
			cnt := shift_cnt - 1;
			case color_mode is
				when BW_MODE =>
				  color1 := data(conv_integer(cnt(4) & (cnt(3 downto 0))));
					if color1 = '1' then
						rcolor <= x"FF";
						gcolor <= x"FF";
						bcolor <= x"FF";
					else
						rcolor <= x"00";
						gcolor <= x"00";
						bcolor <= x"00";						
					end if;	
				when COLOR2_MODE =>
				  if cnt(0) = '0' then
				    color2(0) := data(conv_integer(cnt(4) & (cnt(3 downto 1)) & "0"));
					  color2(1) := data(conv_integer(cnt(4) & (cnt(3 downto 1)) & "1"));	
						if color2 = "00" then
							rcolor <= x"00";
							gcolor <= x"00";
							bcolor <= x"00";							
						else	
							case palette is	
								when x"0" =>							
									case color2 is
										when "01" =>
											rcolor <= x"00";
											gcolor <= x"00";
											bcolor <= x"FF";
										when "10" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"00";							  
										when others => null;
									end case;	
								when x"1" =>							
									case color2 is
										when "01" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "10" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"FF";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"00";							  
										when others => null;
									end case;	
								when x"2" =>							
									case color2 is
										when "01" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "10" =>
											rcolor <= x"00";
											gcolor <= x"00";
											bcolor <= x"FF";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"FF";							  
										when others => null;
									end case;	
								when x"3" =>							
									case color2 is
										when "01" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "10" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"00";							  
										when others => null;
									end case;	
								when x"4" =>							
									case color2 is
										when "01" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"FF";
										when "10" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"FF";							  
										when others => null;
									end case;
								when x"5" =>							
									case color2 is
										when "01" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "10" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"FF";							  
										when others => null;
									end case;
								when x"6" =>							
									case color2 is
										when "01" =>
											rcolor <= x"80";
											gcolor <= x"00";
											bcolor <= x"00";
										when "10" =>
											rcolor <= x"80";
											gcolor <= x"80";
											bcolor <= x"00";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"00";							  
										when others => null;
									end case;
								when x"7" =>							
									case color2 is
										when "01" =>
											rcolor <= x"80";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "10" =>
											rcolor <= x"80";
											gcolor <= x"80";
											bcolor <= x"80";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"00";							  
										when others => null;
									end case;
								when x"8" =>							
									case color2 is
										when "01" =>
											rcolor <= x"80";
											gcolor <= x"00";
											bcolor <= x"80";
										when "10" =>
											rcolor <= x"80";
											gcolor <= x"00";
											bcolor <= x"FF";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"FF";							  
										when others => null;
									end case;
								when x"9" =>							
									case color2 is
										when "01" =>
											rcolor <= x"80";
											gcolor <= x"FF";
											bcolor <= x"80";
										when "10" =>
											rcolor <= x"80";
											gcolor <= x"00";
											bcolor <= x"FF";
										when "11" =>
											rcolor <= x"80";
											gcolor <= x"80";
											bcolor <= x"00";							  
										when others => null;
									end case;
								when x"A" =>							
									case color2 is
										when "01" =>
											rcolor <= x"80";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "10" =>
											rcolor <= x"80";
											gcolor <= x"00";
											bcolor <= x"80";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"00";							  
										when others => null;
									end case;
								when x"B" =>							
									case color2 is
										when "01" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "10" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"00";							  
										when others => null;
									end case;
								when x"C" =>							
									case color2 is
										when "01" =>
											rcolor <= x"FF";
											gcolor <= x"00";
											bcolor <= x"00";
										when "10" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "11" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"FF";							  
										when others => null;
									end case;
								when x"D" =>							
									case color2 is
										when "01" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "10" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"FF";							  
										when others => null;
									end case;
								when x"E" =>							
									case color2 is
										when "01" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "10" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"FF";							  
										when others => null;
									end case;									
								when x"F" =>
									case color2 is
										when "01" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"FF";
										when "10" =>
											rcolor <= x"00";
											gcolor <= x"FF";
											bcolor <= x"00";
										when "11" =>
											rcolor <= x"FF";
											gcolor <= x"FF";
											bcolor <= x"FF";							  
										when others => null;
									end case;	
								when others => null;	
							end case;	
						end if;	
					end if;
				when COLOR16_MODE =>
				  if shift_cnt(0) = '0' then
						color16 := data(31 downto 16);
					else	
						color16 := data(15 downto 0);
					end if;
				  rcolor(7 downto 8- RGB16_RED_WIDTH) <= 
					  color16(RGB16_RED_OFFSET + RGB16_RED_WIDTH - 1 downto RGB16_RED_OFFSET);
				  gcolor(7 downto 8- RGB16_GREEN_WIDTH) <= 
					  color16(RGB16_GREEN_OFFSET + RGB16_GREEN_WIDTH - 1 downto RGB16_GREEN_OFFSET);					
				  bcolor(7 downto 8- RGB16_BLUE_WIDTH) <= 
					  color16(RGB16_BLUE_OFFSET + RGB16_BLUE_WIDTH - 1 downto RGB16_BLUE_OFFSET);					
			    rcolor(7- RGB16_RED_WIDTH downto 0) <= (others => '0');
			    gcolor(7- RGB16_GREEN_WIDTH downto 0) <= (others => '0');
			    bcolor(7- RGB16_BLUE_WIDTH downto 0) <= (others => '0');					
					if shift_cnt(0) = '1' then
						vshift_cnt := (others => '0');
					end if;	
				when others => null;
			end case;	
			if blank = '1' then
				shift_cnt <= (others => '0');
			else
				shift_cnt <= vshift_cnt;
			end if;	
    end if;
  end process pixel_shifter;	

  process (reset, clk)
  begin
    if reset = '1' then
	    blank_del <= '0';
			blank_del2 <= '0';
			blank_del3 <= '0';
	  elsif rising_edge(clk) then
		  blank_del3 <= blank_del2; 
			blank_del2 <= blank_del;
			blank_del <= blank;
	  end if;	
  end process;	

	-- Avalon slave on address 177664
	external_bus: process(reset, slv1_clk, slv1_read, slv1_chipselect)
	begin	
		if reset = '1' then
			roll_offset <= x"D8";
			full_screen <= '1';
		elsif	rising_edge(slv1_clk) then
			if slv1_chipselect = '1' and slv1_write = '1' then
				roll_offset <= slv1_writedata(7 downto 0);
				full_screen <= slv1_writedata(9);
			end if;	
		end if;
		if slv1_chipselect = '1' and slv1_read = '1' then
			slv1_readdata <= "000000" & full_screen & "0" & roll_offset;
		else
			slv1_readdata <= (others => '0');
		end if;	
	end process external_bus; 	

	mem_address_proc: process (address_cnt, roll_offset)
	  variable roll_addr: std_logic_vector(13 downto 1);
	begin
		roll_addr := (address_cnt(11 downto 0) & "0") + ((roll_offset - x"D8") & "00000");
		mem_address <= x"1000" & "01" & roll_addr & "0";	
--    mem_address <= "00000001" & "00000000" & "00" & roll_addr & "0";
--    mem_address <= "00000001" & address_cnt & "00";

--    roll_addr := address_cnt(11 downto 0) & "0";
--		mem_address <= "00000000" & "00000000" & "01" & roll_addr & "0";
	end process mem_address_proc;	
	
	mem_burstcount<= "1000";
--	mem_read <= '0';
	mem_read <= '1' when state = st_request else '0';
	fifo_wrreq <= '1' when state = st_read and mem_readdatavalid = '1' else '0';	
	
	blank <= hblank or vblank;
	fifo_rdreq <= '1' when (blank = '0') and (shift_cnt = "00000") else '0';
	
  nRAMDAC_BLANK <= not blank_del2; 
  nRAMDAC_SYNC <= '0';
  RAMDAC_CLK <= not clk;	
	
	HSYNC <= hsyncb xor tms.h_sync_inv;
	VSYNC <= vsyncb xor tms.v_sync_inv; 
  RED <= rcolor; 
  GREEN <= gcolor;
  BLUE <= bcolor;	
	
	dvi_out: process(reset, clk)
	begin
	  if falling_edge(clk) then
		  DVI_DE <= not blank_del2;			
			if display_clk_cnt(0) = '0' then
			  DVI_D <= gcolor(3 downto 0) & bcolor;
			else
			  DVI_D <= rcolor & gcolor(7 downto 4);
			end if;
	  end if;
	end process dvi_out;
	
	DVI_CLK <= display_clk;
end behaviour;
