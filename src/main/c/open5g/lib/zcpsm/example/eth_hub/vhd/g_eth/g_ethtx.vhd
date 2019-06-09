library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity g_ethtx is
	generic(
		HEAD_AWIDTH			:	natural		:=	5;
		BUFF_AWIDTH			:	natural		:=	5;
		FIFO_AWIDTH			:	natural		:=	2;
		RD_CYCLE			:	natural		:=	1;
		RD_DELAY			:	natural		:=	1;
		RAM_AWIDTH			:	natural		:=  32
		);
	port (
		clk					:	in	std_logic;
		zcpsm_clk			:	in	std_logic;
		reset				:	in	std_logic;

		txclk				:	in	std_logic;
		txd					:	out	std_logic_vector(7 downto 0);
		txen				:	out	std_logic;
		
		eth_ce				:	in	std_logic;
		eth_port_id			:	in	std_logic_vector(3 downto 0);
		eth_write_strobe	:	in	std_logic;
		eth_out_port		:	in	std_logic_vector(7 downto 0);
		eth_read_strobe		:	in	std_logic;
		eth_in_port			:	out	std_logic_vector(7 downto 0);
		
		db_ce				:	in	std_logic;
		db_port_id			:	in	std_logic_vector(3 downto 0);
		db_write_strobe		:	in	std_logic;
		db_out_port			:	in	std_logic_vector(7 downto 0);
		db_read_strobe		:	in	std_logic;
		db_in_port			:	out	std_logic_vector(7 downto 0);
		
--		ram_raddr			:	out	std_logic_vector(23 downto 0);
		ram_raddr			:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);	   

--		ram_rdata			:	in	std_logic_vector(7 downto 0);
		ram_rdata			:	in	std_logic_vector(31 downto 0);
		-- local time --
		localtime 			:	in 	std_logic_vector(31 downto 0)
		);
end entity;

architecture arch_ethtx of g_ethtx is
	
	component g_ethtx_output
	generic(
		HEAD_AWIDTH : NATURAL := 5;
		BUFF_AWIDTH : NATURAL := 5;
		RAM_AWIDTH  : NATURAL := 32
		);
	port(
		clk : in std_logic;
		reset : in std_logic;
		txclk : in std_logic;
		txd : out std_logic_vector(7 downto 0);
		txen : out std_logic;
		tx_queue_empty : in std_logic;
		tx_head_raddr : out std_logic_vector((HEAD_AWIDTH-1) downto 0);
		tx_head_rdata : in std_logic_vector(7 downto 0);
		tx_head_rd_block : out std_logic;
		db_queue_empty : in std_logic;
		db_head_raddr : out std_logic_vector((HEAD_AWIDTH-1) downto 0);
		db_head_rdata : in std_logic_vector(7 downto 0);
		db_head_rd_block : out std_logic;
		buff_raddr : out std_logic_vector((BUFF_AWIDTH-1) downto 0);
		buff_rdata : in std_logic_vector(31 downto 0);
		dma_start : out std_logic;
--		dma_start_addr : out std_logic_vector(23 downto 0);	
		dma_start_addr : out std_logic_vector(RAM_AWIDTH - 1 downto 0);

		dma_length : out std_logic_vector(15 downto 0);
		dma_step : out std_logic_vector(7 downto 0); 
		-- local time --
		localtime: in std_logic_vector(31 downto 0)
		);
	end component;
	
	component Tx_queue
	generic(
		HEAD_AWIDTH : NATURAL := 5;
		FIFO_AWIDTH : NATURAL := 2;
		RAM_TYPE : STRING := "DIS_RAM");
	port(
		clk : in std_logic;
		reset : in std_logic;
		queue_empty : out std_logic;
		head_raddr : in std_logic_vector((HEAD_AWIDTH-1) downto 0);
		head_rdata : out std_logic_vector(7 downto 0);
		head_rd_block : in std_logic;
		zcpsm_clk : in std_logic;
		zcpsm_ce : in std_logic;
		zcpsm_port_id : in std_logic_vector(3 downto 0);
		zcpsm_write_strobe : in std_logic;
		zcpsm_out_port : in std_logic_vector(7 downto 0);
		zcpsm_read_strobe : in std_logic;
		zcpsm_in_port : out std_logic_vector(7 downto 0));
	end component;
	
	component disdram
	generic(
		depth : INTEGER;
		Dwidth : INTEGER;
		Awidth : INTEGER);
	port(
		A : in std_logic_vector((Awidth-1) downto 0);
		CLK : in std_logic;
		D : in std_logic_vector((Dwidth-1) downto 0);
		WE : in std_logic;
		DPRA : in std_logic_vector((Awidth-1) downto 0);
		DPO : out std_logic_vector((Dwidth-1) downto 0);
		QDPO : out std_logic_vector((Dwidth-1) downto 0));
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
		
--		start_raddr : in std_logic_vector(23 downto 0);
		start_raddr : in std_logic_vector(RAM_AWIDTH - 1 downto 0);

		wstep : in std_logic_vector(7 downto 0);
		rstep : in std_logic_vector(7 downto 0);
		busy : out std_logic;
--		raddr : out std_logic_vector(23 downto 0); 
		raddr : out std_logic_vector(RAM_AWIDTH - 1 downto 0);

		rdata : in std_logic_vector((DWIDTH-1) downto 0);
		wren : out std_logic;
		waddr : out std_logic_vector(RAM_AWIDTH - 1 downto 0);
		wdata : out std_logic_vector((DWIDTH-1) downto 0));
	end component;
	
	signal tx_queue_empty	:	std_logic;
	signal tx_head_raddr	:	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
	signal tx_head_rdata	:	std_logic_vector(7 downto 0);
	signal tx_head_rd_block	:	std_logic;
	
	signal db_queue_empty	:	std_logic;
	signal db_head_raddr	:	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
	signal db_head_rdata	:	std_logic_vector(7 downto 0);
	signal db_head_rd_block	:	std_logic;
	
	signal buff_raddr 		:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	signal buff_rdata		:	std_logic_vector(31 downto 0);
	signal buff_wren		:	std_logic;
	signal buff_waddr 		:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	signal buff_wdata		:	std_logic_vector(31 downto 0);
	
	signal dma_length_byte	:	std_logic_vector(15 downto 0);
	signal dma_length_dword	:	std_logic_vector(15 downto 0);
--	signal dma_start_raddr	:	std_logic_vector(23 downto 0);
	signal dma_start_raddr_word	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal dma_start_raddr_dword	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	
	signal dma_rstep		:	std_logic_vector(7 downto 0);
	signal dma_start		:	std_logic;
	signal dma_busy			:	std_logic;
	
--	signal dma_raddr		:	std_logic_vector(23 downto 0); 
	signal dma_raddr		:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	
--	signal dma_rdata		:	std_logic_vector(7 downto 0);	 
	signal dma_rdata		:	std_logic_vector(31 downto 0);

	signal dma_wdata_dword	:	std_logic_vector(31 downto 0);
	signal dma_waddr_dword	:	std_logic_vector(RAM_AWIDTH - 1 downto 0); 
	signal dma_wren_dword	:	std_logic; 
	signal flag				:	std_logic; 
	
	signal buff_waddr_reg	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal buff_wdata_reg	:	std_logic_vector(31 downto 0);
	signal dma_wren_reg		:	std_logic;
--	signal dma_wren			:	std_logic;
--	signal dma_waddr		:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
--	signal dma_wdata		:	std_logic_vector(7 downto 0);

	signal dma_ena			:	std_logic;
	
	signal buff_wr_diff		:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	
begin
	
	u_output : g_ethtx_output
	generic map(
		HEAD_AWIDTH => HEAD_AWIDTH,
		BUFF_AWIDTH => BUFF_AWIDTH,
		RAM_AWIDTH  => RAM_AWIDTH
		)
	port map(
		clk => clk,
		reset => reset,
		txclk => txclk,
		txd => txd,
		txen => txen,
		tx_queue_empty => tx_queue_empty,
		tx_head_raddr => tx_head_raddr,
		tx_head_rdata => tx_head_rdata,
		tx_head_rd_block => tx_head_rd_block,
		db_queue_empty => db_queue_empty,
		db_head_raddr => db_head_raddr,
		db_head_rdata => db_head_rdata,
		db_head_rd_block => db_head_rd_block,
		buff_raddr => buff_raddr,
		buff_rdata => buff_rdata,
		dma_start => dma_start,
		dma_start_addr => dma_start_raddr_word,
		dma_length => dma_length_byte,
		dma_step => dma_rstep,
		-- local time 
		localtime => localtime
		);
	
	u_db_queue : Tx_queue
	generic map(
		HEAD_AWIDTH => HEAD_AWIDTH,
		FIFO_AWIDTH => 0,
		RAM_TYPE => "DIS_RAM"
		)
	port map(
		clk => clk,
		reset => reset,
		queue_empty => db_queue_empty,
		head_raddr => db_head_raddr,
		head_rdata => db_head_rdata,
		head_rd_block => db_head_rd_block,
		zcpsm_clk => zcpsm_clk,
		zcpsm_ce => db_ce,
		zcpsm_port_id => db_port_id,
		zcpsm_write_strobe => db_write_strobe,
		zcpsm_out_port => db_out_port,
		zcpsm_read_strobe => db_read_strobe,
		zcpsm_in_port => db_in_port
		);
	
	u_tx_queue : Tx_queue
	generic map(
		HEAD_AWIDTH => HEAD_AWIDTH,
		FIFO_AWIDTH => FIFO_AWIDTH,
		RAM_TYPE => "DIS_RAM"
		)
	port map(
		clk => clk,
		reset => reset,
		queue_empty => tx_queue_empty,
		head_raddr => tx_head_raddr,
		head_rdata => tx_head_rdata,
		head_rd_block => tx_head_rd_block,
		zcpsm_clk => zcpsm_clk,
		zcpsm_ce => eth_ce,
		zcpsm_port_id => eth_port_id,
		zcpsm_write_strobe => eth_write_strobe,
		zcpsm_out_port => eth_out_port,
		zcpsm_read_strobe => eth_read_strobe,
		zcpsm_in_port => eth_in_port
		);
	
	u_tx_buffer : disdram
	generic map(
		DEPTH => 2 ** BUFF_AWIDTH,
		AWIDTH => BUFF_AWIDTH,
		DWIDTH => 32
		)
	port map(
		A => buff_waddr(BUFF_AWIDTH - 1 downto 0),
		CLK => clk,
		D => buff_wdata,
		WE => buff_wren,
		DPRA => buff_raddr(BUFF_AWIDTH - 1 downto 0),
		DPO => buff_rdata,
		QDPO => open
		);
	
	u_dma : dma_ctrl
	generic map(
		DWIDTH => 32,
		RD_CYCLE => RD_CYCLE,
		RD_DELAY => RD_DELAY,
		RAM_AWIDTH => RAM_AWIDTH
		)
	port map(
		clk => clk,
		reset => reset,
		ena => dma_ena,
		start => dma_start,
		length => dma_length_dword,
		start_waddr => (others => '0'),
		start_raddr => dma_start_raddr_dword,
		wstep => X"01",
		rstep => dma_rstep,
		busy => dma_busy,
		raddr => dma_raddr,
		rdata => dma_rdata,
		wren => dma_wren_dword,
		waddr => dma_waddr_dword,
		wdata => dma_wdata_dword
		);
	
	----------
	dma_length_dword <= "00" & dma_length_byte(15 downto 2);  
	dma_start_raddr_dword <= '0' & dma_start_raddr_word(RAM_AWIDTH-1 downto 1);

---	process(reset, clk)
----	begin
--		if reset = '1' then 
---			wren_byte <= '0'; 
--		elsif rising_edge(clk) then
--			if dma_wren_byte = '1' then	
--				wren_byte <= dma_wren_byte;
--				buff_waddr <= dma_waddr_byte(BUFF_AWIDTH - 1 downto 1)&'0';	
--				buff_wdata <= dma_wdata_byte(7 downto 0);
--			elsif flag = '1' then
--				buff_waddr <= dma_waddr_byte(BUFF_AWIDTH - 1 downto 1)&'1';
--				buff_wdata <= dma_wdata_byte(15 downto 8); 
--			end if;
--		end if;
--	end process;  

	process(reset, clk)
	begin
		if reset = '1' then
			buff_waddr_reg <= (others => '0');
			buff_wdata_reg <= (others => '0');	
			dma_wren_reg <= '0';
		elsif rising_edge(clk) then
			if dma_wren_dword = '1' then
				buff_waddr_reg <= dma_waddr_dword;
				buff_wdata_reg <= dma_wdata_dword;
			end if;	 
			dma_wren_reg <= dma_wren_dword;
		end if;
	end process;
	
	
--	buff_waddr <= buff_waddr_reg(BUFF_AWIDTH - 2 downto 0)&'0' when dma_wren_reg = '1' else
--				  buff_waddr_reg(BUFF_AWIDTH - 2 downto 0)&'1' ;
--		
--	buff_wdata <= buff_wdata_reg(7 downto 0) when dma_wren_reg = '1' else
--				  buff_wdata_reg(15 downto 8); 

	buff_waddr <= buff_waddr_reg(BUFF_AWIDTH - 1 downto 0);
	buff_wdata <= buff_wdata_reg;
	
	process(reset, clk)
	begin
		if reset = '1' then 
			flag <= '0'; 
		elsif rising_edge(clk) then
--			if wren_byte = '1' then	
				flag <= dma_wren_reg; 
--			end if;
		end if;
	end process; 	

	buff_wren <= flag or dma_wren_reg;
	
	ram_raddr <= dma_raddr;
	dma_rdata <= ram_rdata;
	
--	buff_wren <= dma_wren;
--	buff_waddr <= dma_waddr(BUFF_AWIDTH - 1 downto 0);
--	buff_wdata <= dma_wdata;
	
	buff_wr_diff <= buff_waddr - buff_raddr;
	
	p_dma_ena : process(clk, reset)
	begin
		if reset = '1' then
			dma_ena <= '1';
		elsif rising_edge(clk) then
			if buff_wr_diff >= 2 ** BUFF_AWIDTH - RD_CYCLE - RD_DELAY - 4 then
				dma_ena <= '0';
			elsif buff_wr_diff <= RD_CYCLE + RD_DELAY + 2 then
				dma_ena <= '1';
			end if;
		end if;
	end process;
	
end arch_ethtx;
