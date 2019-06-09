library ieee;
use ieee.std_logic_1164.all;

entity g_ethrx is
	generic(
		HEAD_AWIDTH			:	natural		:=	5;
		BUFF_AWIDTH			:	natural		:=	12;
		FIFO_AWIDTH			:	natural		:=	2;
		WR_CYCLE			:	natural		:=	3;
		RAM_AWIDTH			:	natural		:=  32
		);
	port(
		clk					:	in	std_logic;
		zcpsm_clk			:	in	std_logic;
		reset				:	in	std_logic;
		
		rxclk				:	in	std_logic;
		rxd					:	in	std_logic_vector(7 downto 0);
		rxdv				:	in	std_logic;
		
		db_ce				:	in	std_logic;
		db_port_id			:	in	std_logic_vector(3 downto 0);
		db_write_strobe		:	in	std_logic;
		db_out_port			:	in	std_logic_vector(7 downto 0);
		db_read_strobe		:	in	std_logic;
		db_in_port			:	out	std_logic_vector(7 downto 0);
		
		eth_ce				:	in	std_logic;
		eth_port_id			:	in	std_logic_vector(3 downto 0);
		eth_write_strobe	:	in	std_logic;
		eth_out_port		:	in	std_logic_vector(7 downto 0);
		eth_read_strobe		:	in	std_logic;
		eth_in_port			:	out	std_logic_vector(7 downto 0);
		eth_dma_ce			:	in	std_logic;
		
		ethrx_busy			:	out	std_logic;
		recvtime 			:	out std_logic_vector(31 downto 0);
		recvtime_valid		:	out	std_logic;	
		localtime_locked	:	out	std_logic;	
		lastframe_flag		:	out std_logic;
		
		ram_wren			:	out	std_logic;
		ram_waddr			:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
		
--		test			:	out	std_logic_vector(3 downto 0); 
		ram_wdata			:	out	std_logic_vector(31 downto 0)
		);
end entity;

architecture arch_ethrx of g_ethrx is
	
	component g_ethrx_input
	generic(
		HEAD_AWIDTH : NATURAL;
		BUFF_AWIDTH : NATURAL);
	port(  
--	test_crc	:	out std_logic_vector(3 downto 0);
	
		clk : in std_logic;
		reset : in std_logic;
		rxclk : in std_logic;
		rxd : in std_logic_vector(7 downto 0);
		rxdv : in std_logic;   
		
		recvtime 		:	out std_logic_vector(31 downto 0);
		recvtime_valid	:	out	std_logic;		
		localtime_locked :  out	std_logic;	
		
		head_wren : out std_logic;
		head_waddr : out std_logic_vector((HEAD_AWIDTH-1) downto 0);
		head_wdata : out std_logic_vector(7 downto 0);
		head_wr_block : out std_logic;
		buff_wren : out std_logic;
		buff_waddr : out std_logic_vector((BUFF_AWIDTH-1) downto 0);
		buff_wdata : out std_logic_vector(31 downto 0));
	end component;
	
	component Rx_queue
	generic(
		HEAD_AWIDTH : NATURAL;
		FIFO_AWIDTH : NATURAL;
		RAM_TYPE : STRING);
	port(
		clk : in std_logic;
		reset : in std_logic;
		head_wren : in std_logic;
		head_waddr : in std_logic_vector((HEAD_AWIDTH-1) downto 0);
		head_wdata : in std_logic_vector(7 downto 0);
		head_wr_block : in std_logic;
		zcpsm_clk : in std_logic;
		zcpsm_ce : in std_logic;
		zcpsm_port_id : in std_logic_vector(3 downto 0);
		zcpsm_write_strobe : in std_logic;
		zcpsm_out_port : in std_logic_vector(7 downto 0);
		zcpsm_read_strobe : in std_logic;
		zcpsm_in_port : out std_logic_vector(7 downto 0));
	end component;
	
	component blockdram
	generic(
		depth : INTEGER;
		Dwidth : INTEGER;
		Awidth : INTEGER);
	port(
		addra : in std_logic_vector((Awidth-1) downto 0);
		clka : in std_logic;
		addrb : in std_logic_vector((Awidth-1) downto 0);
		clkb : in std_logic;
		dia : in std_logic_vector((Dwidth-1) downto 0);
		wea : in std_logic;
		dob : out std_logic_vector((Dwidth-1) downto 0));
	end component;
	
	component zcpsm2dma
	generic(
	 	RAM_AWIDTH : NATURAL
	);
	port(
		clk : in std_logic;
		reset : in std_logic;
		zcpsm_clk : in std_logic;
		zcpsm_ce : in std_logic;
		zcpsm_port_id : in std_logic_vector(3 downto 0);
		zcpsm_write_strobe : in std_logic;
		zcpsm_out_port : in std_logic_vector(7 downto 0);
		zcpsm_read_strobe : in std_logic;
		zcpsm_in_port : out std_logic_vector(7 downto 0);  
		
		lastframe_flag		:	out std_logic; 
		
		start : out std_logic;
		length : out std_logic_vector(15 downto 0);
		start_waddr : out std_logic_vector(RAM_AWIDTH - 1 downto 0);
		start_raddr : out std_logic_vector(RAM_AWIDTH - 1  downto 0);
		wstep : out std_logic_vector(7 downto 0);
		rstep : out std_logic_vector(7 downto 0);
		busy : in std_logic);
	end component;
	
	component dma_ctrl
	generic(
		DWIDTH : NATURAL;
		RD_CYCLE : NATURAL;
		RD_DELAY : NATURAL;
		RAM_AWIDTH : NATURAL
		);
	port(
		clk : in std_logic;
		reset : in std_logic;
		ena : in std_logic;
		start : in std_logic;
		length : in std_logic_vector(15 downto 0);
		start_waddr : in std_logic_vector(RAM_AWIDTH - 1 downto 0);
		start_raddr : in std_logic_vector(RAM_AWIDTH - 1 downto 0);
		wstep : in std_logic_vector(7 downto 0);
		rstep : in std_logic_vector(7 downto 0);
		busy : out std_logic;
		raddr : out std_logic_vector(RAM_AWIDTH - 1 downto 0);
		rdata : in std_logic_vector((DWIDTH-1) downto 0);
		wren : out std_logic;
		waddr : out std_logic_vector(RAM_AWIDTH - 1 downto 0);
		wdata : out std_logic_vector((DWIDTH-1) downto 0));
	end component;
	
	component shiftreg
	generic(
		width : INTEGER;
		depth : INTEGER);
	port(
		clk : in std_logic;
		ce : in std_logic;
		D : in std_logic_vector((width-1) downto 0);
		Q : out std_logic_vector((width-1) downto 0);
		S : out std_logic_vector((width-1) downto 0));
	end component;
	
	signal head_wren		:	std_logic;
	signal head_waddr		:	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
	signal head_wdata		:	std_logic_vector(7 downto 0);
	signal head_wr_block	:	std_logic;
	
	signal buff_wren		:	std_logic;
	signal buff_waddr		:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	signal buff_wdata		:	std_logic_vector(31 downto 0);
	signal buff_raddr		:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	signal buff_rdata		:	std_logic_vector(31 downto 0);
	
	signal dma_length_byte	:	std_logic_vector(15 downto 0);
	signal dma_length_dword	:	std_logic_vector(15 downto 0);
	signal dma_start_waddr_word	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal dma_start_waddr_dword	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal dma_start_raddr_byte	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal dma_start_raddr_dword	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal dma_wstep		:	std_logic_vector(7 downto 0);
	signal dma_rstep		:	std_logic_vector(7 downto 0);
	signal dma_start		:	std_logic;
	signal dma_busy			:	std_logic;
	
	signal dma_raddr		:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal dma_rdata		:	std_logic_vector(31 downto 0);
	signal dma_wren			:	std_logic;
	signal dma_waddr		:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal dma_wdata		:	std_logic_vector(31 downto 0);
	
	signal ram_wren_d1		:	std_logic;
	signal ram_wren_d2		:	std_logic;
	signal v0				:	std_logic_vector(0 downto 0);
	signal v1				:	std_logic_vector(0 downto 0);
	signal dma_wren_dly		:	std_logic;	 


begin
	
--	test <= head_wr_block;
	
	ethrx_busy	<= dma_busy;
	
	u_input : g_ethrx_input
	generic map(
		HEAD_AWIDTH => HEAD_AWIDTH,
		BUFF_AWIDTH => BUFF_AWIDTH
		)
	port map(  
--		test_crc	=> 	test,
		clk => clk,
		reset => reset,
		rxclk => rxclk,
		rxd => rxd,
		rxdv => rxdv, 
		
		recvtime 		=>	recvtime,
		recvtime_valid	=>	recvtime_valid,		
		localtime_locked =>  localtime_locked,
		
		head_wren => head_wren,
		head_waddr => head_waddr,
		head_wdata => head_wdata,
		head_wr_block => head_wr_block,
		buff_wren => buff_wren,
		buff_waddr => buff_waddr,
		buff_wdata => buff_wdata
		);
	
	u_db_queue : Rx_queue
	generic map(
		HEAD_AWIDTH => HEAD_AWIDTH,
		FIFO_AWIDTH => 1,
		RAM_TYPE => "DIS_RAM"
		)
	port map(
		clk => clk,
		reset => reset,
		head_wren => head_wren,
		head_waddr => head_waddr,
		head_wdata => head_wdata,
		head_wr_block => head_wr_block,
		zcpsm_clk => zcpsm_clk,
		zcpsm_ce => db_ce,
		zcpsm_port_id => db_port_id,
		zcpsm_write_strobe => db_write_strobe,
		zcpsm_out_port => db_out_port,
		zcpsm_read_strobe => db_read_strobe,
		zcpsm_in_port => db_in_port
		);
	
	u_rx_queue : Rx_queue
	generic map(
		HEAD_AWIDTH => HEAD_AWIDTH,
		FIFO_AWIDTH => FIFO_AWIDTH,
		RAM_TYPE => "DIS_RAM"
		)
	port map(
		clk => clk,
		reset => reset,
		head_wren => head_wren,
		head_waddr => head_waddr,
		head_wdata => head_wdata,
		head_wr_block => head_wr_block,
		zcpsm_clk => zcpsm_clk,
		zcpsm_ce => eth_ce,
		zcpsm_port_id => eth_port_id,
		zcpsm_write_strobe => eth_write_strobe,
		zcpsm_out_port => eth_out_port,
		zcpsm_read_strobe => eth_read_strobe,
		zcpsm_in_port => eth_in_port
		);
	
	u_rx_buffer : blockdram
	generic map(
		DEPTH => 2 ** BUFF_AWIDTH,
		AWIDTH => BUFF_AWIDTH,
		DWIDTH => 32
		)
	port map(
		addra => buff_waddr,
		clka => clk,
		addrb => buff_raddr,
		clkb => clk,
		dia => buff_wdata,
		wea => buff_wren,
		dob => buff_rdata
		);
	
	u_dma : dma_ctrl
	generic map(
		DWIDTH => 32,
		RD_CYCLE => WR_CYCLE, 
		RD_DELAY => 0,
		RAM_AWIDTH => RAM_AWIDTH
		)
	port map(
		clk => clk,
		reset => reset,
		ena => '1',
		start => dma_start,
		length => dma_length_dword,
		start_waddr => dma_start_waddr_dword,
		start_raddr => dma_start_raddr_dword,			-- 32位，截取后12位作为rx_buffer的地址
		wstep => dma_wstep,
		rstep => dma_rstep,
		busy => dma_busy,
		raddr => dma_raddr,
		rdata => dma_rdata,
		wren => dma_wren,
		waddr => dma_waddr,
		wdata => dma_wdata
		);
	dma_length_dword <= "00" & dma_length_byte(15 downto 2); 
	dma_start_raddr_dword <= "00" & dma_start_raddr_byte(RAM_AWIDTH-1 downto 2);   
	dma_start_waddr_dword <= '0' & dma_start_waddr_word(RAM_AWIDTH-1 downto 1); 
	
	u_zcpsm2dma : zcpsm2dma
	generic map (
		RAM_AWIDTH => RAM_AWIDTH
	)
	port map(
		clk => clk,
		reset => reset,
		zcpsm_clk => zcpsm_clk,
		zcpsm_ce => eth_dma_ce,
		zcpsm_port_id => eth_port_id,
		zcpsm_write_strobe => eth_write_strobe,
		zcpsm_out_port => eth_out_port,
		zcpsm_read_strobe => eth_read_strobe,
		zcpsm_in_port => eth_in_port, 
		
		lastframe_flag		=>	lastframe_flag,
		
		start => dma_start,
		length => dma_length_byte,
		start_waddr => dma_start_waddr_word,
		start_raddr => dma_start_raddr_byte,
		wstep => dma_wstep,
		rstep => dma_rstep,
		busy => dma_busy
		);
	
	buff_raddr <= dma_raddr(BUFF_AWIDTH - 1 downto 0);
	dma_rdata <= buff_rdata;
	
--	p_wr: process(clk, reset)
--	begin
--		if reset = '1' then
---			ram_wren_d1 <= '0';
--			ram_wren_d2 <= '0';
--			ram_waddr <= (others => '0');
--			ram_wdata <= (others => '0');		
--		elsif rising_edge(clk) then
--			ram_wren_d1 <= dma_wren;
--			ram_wren_d2	<= ram_wren_d1;
--			if dma_wren = '1' then
--				ram_waddr <= dma_waddr;
--				ram_wdata <= dma_wdata;	
--			end if;	
--		end if;
--	end process;
	
--	ram_wren <= ram_wren_d1 or ram_wren_d2;
	u_wren_dly : ShiftReg
	generic map(width => 1, depth => WR_CYCLE)
	port map(clk => clk, ce => '1', d => v0, q => v1, s => open);
	
	v0(0) <= dma_wren;
	dma_wren_dly <= v1(0);
	
	p_wr : process(clk, reset)
	begin
		if reset = '1' then
			ram_wren <= '0';
			ram_waddr <= (others => '0');
			ram_wdata <= (others => '0');
		elsif rising_edge(clk) then
			if dma_wren = '1' then
				ram_waddr <= dma_waddr;
				ram_wdata <= dma_wdata;
			end if;
			if dma_wren = '1' then
				ram_wren <= '1';
			elsif dma_wren_dly = '1' then
				ram_wren <= '0';
			end if;
		end if;
	end process;

	
	
end arch_ethrx;
