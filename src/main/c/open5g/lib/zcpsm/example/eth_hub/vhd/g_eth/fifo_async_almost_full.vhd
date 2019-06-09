-- modified 2006-05-13 (Line 404)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity fifo_async_almost_full is
	generic(
		DEPTH		: natural;
		AWIDTH		: natural;
		DWIDTH		: natural;
		RAM_TYPE	: string	  			-- "BLOCK_RAM" or "DIS_RAM"
		);
	port(
		reset	: in std_logic;
		clr		: in std_logic;
		clka	: in std_logic;
		wea		: in std_logic;
		dia		: in std_logic_vector(DWIDTH - 1 downto 0);
		clkb	: in std_logic;
		rdb		: in std_logic;
		dob		: out std_logic_vector(DWIDTH - 1 downto 0);  -- dob delay = 2 clk compared with rdb 
		empty	: out std_logic;
		full	: out std_logic;  
		almost_full	: out std_logic;
		dn		: out std_logic_vector(AWIDTH -1 downto 0)
		);
end fifo_async_almost_full;

architecture fast_read of fifo_async_almost_full is
	
	component blockdram
		generic( 
			depth:	integer;
			Dwidth: integer;
			Awidth:	integer
			);
		port(
			addra: IN std_logic_VECTOR(Awidth-1 downto 0);
			clka: IN std_logic;
			addrb: IN std_logic_VECTOR(Awidth-1 downto 0);
			clkb: IN std_logic;
			dia: IN std_logic_VECTOR(Dwidth-1 downto 0);
			wea: IN std_logic;
			dob: OUT std_logic_VECTOR(Dwidth-1 downto 0)	:= (others => '0')
			);
	end component;
	
	component disdram
		generic( 
			depth:	integer;
			Dwidth: integer;
			Awidth:	integer
			);
		port(
			A: IN std_logic_VECTOR(Awidth-1 downto 0);
			CLK: IN std_logic;
			D: IN std_logic_VECTOR(Dwidth-1 downto 0);
			WE: IN std_logic;
			DPRA: IN std_logic_VECTOR(Awidth-1 downto 0);
			DPO: OUT std_logic_VECTOR(Dwidth-1 downto 0);
			QDPO: OUT std_logic_VECTOR(Dwidth-1 downto 0)
			);
	end component; 
	
	signal DPO	: std_logic_vector(DWIDTH-1 downto 0)	:= (others => '0');
									   
	component ASYNCWRITE
		port(
			reset: in std_logic;
			async_clk: in std_logic;
			sync_clk: in std_logic;
			async_wren: in std_logic;
			trigger: in std_logic;
			sync_wren: out std_logic;
			over: out std_logic;
			flag: out std_logic
			);
	end component;
	
	signal wea_sync	: std_logic	:= '0';	 
	signal wp_sync	: std_logic_vector(AWIDTH-1 downto 0)	:= (others => '0');
	
	signal wp		: std_logic_vector(AWIDTH - 1 downto 0)	:= (others => '0');
	signal rp		: std_logic_vector(AWIDTH - 1 downto 0)	:= (others => '0');
	signal ram_we	: std_logic	:= '0';
	signal empty_flag	: std_logic	:= '1';
	signal full_flag	: std_logic	:= '0';
	
begin
	
	use_block_ram	: if RAM_TYPE = "BLOCK_RAM" generate
		ram		: blockdram
		generic map( 
			depth	=> DEPTH,
			Dwidth	=> DWIDTH,
			Awidth	=> AWIDTH
			)
		port map(
			addra	=> wp,
			clka	=> clka,
			addrb	=> rp,
			clkb	=> clkb,
			dia		=> dia,
			wea		=> ram_we,
			dob		=> dob
			);
	end generate use_block_ram;
	
	use_dis_ram		: if RAM_TYPE = "DIS_RAM" generate
		ram	: disdram
		generic map( 
			depth	=> DEPTH,
			Dwidth	=> DWIDTH,
			Awidth	=> AWIDTH
			)
		port map(
			A		=> wp,
			CLK		=> clka,
			D		=> dia,
			WE		=> ram_we,
			DPRA	=> rp,
			DPO		=> DPO,
			QDPO	=> open
			);
		RegDout	: process(reset,clkb)
		begin
			if reset = '1' then
				dob <= (others => '0');
			elsif rising_edge(clkb) then
				dob <= DPO;
			end if;
		end process;
	end generate use_dis_ram;
	
	WritePointorCtrl	: process(reset,clr,clka)
	begin
		if reset = '1' or clr = '1' then
			wp <= (others => '0');
		elsif rising_edge(clka) then
--			if clr = '1' then
--				wp <= (others => '0');
--			elsif full_flag = '0' and wea = '1' then
			if full_flag = '0' and wea = '1' then
				wp <= wp + 1;
			end if;
		end if;
	end process;
	
	ram_we <= wea when full_flag = '0' else '0';
	
	ASYNCWRITE_wea_ins	: ASYNCWRITE
		port map(
			reset		=> reset,
			async_clk	=> clka,
			sync_clk	=> clkb,
			async_wren	=> wea,
			trigger		=> '1',
			sync_wren	=> wea_sync,
			over		=> open,
			flag		=> open
			); 
	
	WritePointorCtrl_sync	: process(reset,clr,clkb)
	begin
		if reset = '1' or clr = '1' then
			wp_sync <= (others => '0');
		elsif rising_edge(clkb) then
--			if clr = '1' then
--				wp_sync <= (others => '0');
--			elsif full_flag = '0' and wea_sync = '1' then
			if full_flag = '0' and wea_sync = '1' then
				wp_sync <= wp_sync + 1;
			end if;
		end if;
	end process;
	
	ReadPointorCtrl		: process(reset,clr,clkb)
	begin
		if reset = '1' or clr = '1' then
			rp <= (others => '0');
		elsif rising_edge(clkb) then
--			if clr = '1' then
--				rp <= (others => '0');
--			elsif empty_flag = '0' and rdb = '1' then
			if empty_flag = '0' and rdb = '1' then
				rp <= rp + 1;
			end if;
		end if;
	end process;
	
	GetEmptyFlag	: process(reset,clr,clkb)
	begin
		if reset = '1' or clr = '1' then
			empty_flag <= '1';
		elsif rising_edge(clkb) then
--			if clr = '1' then
--				empty_flag <= '1';
--			elsif (wp_sync = rp) and (wea_sync = '1') then
			if (wp_sync = rp) and (wea_sync = '1') then
				empty_flag <= '0';
			elsif (wp_sync = rp + 1) and (rdb = '1'and wea_sync = '0') then
				empty_flag <= '1';
			end if;
		end if;
	end process;
	empty <= empty_flag;
	
	GetFullFlag		: process(reset,clr,clkb)
	begin
		if reset = '1' or clr = '1' then
			full_flag <= '0';
		elsif rising_edge(clkb) then
--			if clr = '1' then
--				full_flag <= '0';
--			elsif (wp_sync = rp - 1) and (wea_sync = '1' and rdb = '0') then
			if (wp_sync = rp - 1) and (wea_sync = '1' and rdb = '0') then
				full_flag <= '1';
			elsif (wp_sync = rp) and (rdb = '1') then
				full_flag <= '0';
			end if;
		end if;
	end process;
	full <= full_flag;
	
	dn <= wp_sync - rp;
	
end fast_read;

---------------------------------------------------------------------------------

architecture fast_write of fifo_async_almost_full is
	
	component blockdram
		generic( 
			depth:	integer;
			Dwidth: integer;
			Awidth:	integer
			);
		port(
			addra: IN std_logic_VECTOR(Awidth-1 downto 0);
			clka: IN std_logic;
			addrb: IN std_logic_VECTOR(Awidth-1 downto 0);
			clkb: IN std_logic;
			dia: IN std_logic_VECTOR(Dwidth-1 downto 0);
			wea: IN std_logic;
			dob: OUT std_logic_VECTOR(Dwidth-1 downto 0)	:= (others => '0')
			);
	end component;
	
	component disdram
		generic( 
			depth:	integer;
			Dwidth: integer;
			Awidth:	integer
			);
		port(
			A: IN std_logic_VECTOR(Awidth-1 downto 0);
			CLK: IN std_logic;
			D: IN std_logic_VECTOR(Dwidth-1 downto 0);
			WE: IN std_logic;
			DPRA: IN std_logic_VECTOR(Awidth-1 downto 0);
			DPO: OUT std_logic_VECTOR(Dwidth-1 downto 0);
			QDPO: OUT std_logic_VECTOR(Dwidth-1 downto 0)
			);
	end component; 
	
	signal DPO	: std_logic_vector(DWIDTH-1 downto 0)	:= (others => '0');
	
	component ASYNCWRITE
		port(
			reset: in std_logic;
			async_clk: in std_logic;
			sync_clk: in std_logic;
			async_wren: in std_logic;
			trigger: in std_logic;
			sync_wren: out std_logic;
			over: out std_logic;
			flag: out std_logic
			);
	end component;
	
	signal rdb_sync	: std_logic	:= '0';	 
	signal rp_sync	: std_logic_vector(AWIDTH-1 downto 0)	:= (others => '0');
	
	signal wp		: std_logic_vector(AWIDTH - 1 downto 0)	:= (others => '0');
	signal rp		: std_logic_vector(AWIDTH - 1 downto 0)	:= (others => '0');
	signal ram_we	: std_logic	:= '0';
	signal empty_flag	: std_logic	:= '1';
	signal full_flag	: std_logic	:= '0';
	
begin
	
	use_block_ram	: if RAM_TYPE = "BLOCK_RAM" generate
		ram		: blockdram
		generic map( 
			depth	=> DEPTH,
			Dwidth	=> DWIDTH,
			Awidth	=> AWIDTH
			)
		port map(
			addra	=> wp,
			clka	=> clka,
			addrb	=> rp,
			clkb	=> clkb,
			dia		=> dia,
			wea		=> ram_we,
			dob		=> dob
			);
	end generate use_block_ram;
	
	use_dis_ram		: if RAM_TYPE = "DIS_RAM" generate
		ram	: disdram
		generic map( 
			depth	=> DEPTH,
			Dwidth	=> DWIDTH,
			Awidth	=> AWIDTH
			)
		port map(
			A		=> wp,
			CLK		=> clka,
			D		=> dia,
			WE		=> ram_we,
			DPRA	=> rp,
			DPO		=> DPO,
			QDPO	=> open
			);
		RegDout	: process(reset,clkb)
		begin
			if reset = '1' then
				dob <= (others => '0');
			elsif rising_edge(clkb) then
				dob <= DPO;
			end if;
		end process;
	end generate use_dis_ram;
	
	WritePointorCtrl	: process(reset,clr,clka)
	begin
		if reset = '1' or clr = '1' then
			wp <= (others => '0');
		elsif rising_edge(clka) then
--			if clr = '1' then
--				wp <= (others => '0');
--			elsif full_flag = '0' and wea = '1' then
			if full_flag = '0' and wea = '1' then
				wp <= wp + 1;
			end if;
		end if;
	end process;
	
	ram_we <= wea when full_flag = '0' else '0';
	
	ReadPointorCtrl		: process(reset,clr,clkb)
	begin
		if reset = '1' or clr = '1' then
			rp <= (others => '0');
		elsif rising_edge(clkb) then
--			if clr = '1' then
--				rp <= (others => '0');
--			elsif empty_flag = '0' and rdb = '1' then
			if empty_flag = '0' and rdb = '1' then
				rp <= rp + 1;
			end if;
		end if;
	end process;
	
	ASYNCWRITE_rdb_ins	: ASYNCWRITE
		port map(
			reset		=> reset,
			async_clk	=> clkb,
			sync_clk	=> clka,
			async_wren	=> rdb,
			trigger		=> '1',
			sync_wren	=> rdb_sync,
			over		=> open,
			flag		=> open
			);
			
	ReadPointorCtrl_sync	: process(reset,clr,clka)
	begin
		if reset = '1' or clr = '1' then
			rp_sync <= (others => '0');
		elsif rising_edge(clka) then
--			if clr = '1' then
--				rp_sync <= (others => '0');
--			elsif empty_flag = '0' and rdb_sync = '1' then
			if empty_flag = '0' and rdb_sync = '1' then
				rp_sync <= rp_sync + 1;
			end if;
		end if;
	end process;	
			
	GetEmptyFlag	: process(reset,clr,clka)
	begin
		if reset = '1' or clr = '1' then
			empty_flag <= '1';
		elsif rising_edge(clka) then
--			if clr = '1' then
--				empty_flag <= '1';
--			elsif (wp = rp_sync) and (wea = '1') then
			if (wp = rp_sync) and (wea = '1') then
				empty_flag <= '0';
			elsif (wp = rp_sync + 1) and (rdb_sync = '1'and wea = '0') then
				empty_flag <= '1';
			end if;
		end if;
	end process;
	empty <= empty_flag;
	
	GetFullFlag		: process(reset,clr,clka)
	begin
		if reset = '1' or clr = '1' then
			full_flag <= '0';  -- modified 2006-05-13
		elsif rising_edge(clka) then
--			if clr = '1' then
--				full_flag <= '0';
--			elsif (wp = rp_sync - 1) and (wea = '1' and rdb_sync = '0') then
			if (wp = rp_sync - 1) and (wea = '1' and rdb_sync = '0') then
				full_flag <= '1';
			elsif (wp = rp_sync) and (rdb_sync = '1') then
				full_flag <= '0';
			end if;
		end if;
	end process;
	full <= full_flag;
	
	GetAlmostFull		: process(reset,clr,clka)
	begin
		if reset = '1' or clr = '1' then
			almost_full <= '0';  -- modified 2006-05-13
		elsif rising_edge(clka) then
--			if clr = '1' then
--				almost_full <= '0';
--			elsif (wp = rp_sync - 2) and (wea = '1' and rdb_sync = '0') then
			if (wp = rp_sync - 2) and (wea = '1' and rdb_sync = '0') then
				almost_full <= '1';
			elsif (wp = rp_sync - 1) and (wea = '0' and rdb_sync = '1') then
				almost_full <= '0';
			end if;
		end if;
	end process;	 
	
	dn <= wp - rp_sync;
	
end fast_write;
