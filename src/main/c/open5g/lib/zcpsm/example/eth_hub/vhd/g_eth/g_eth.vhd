library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.eth_config.all;

entity g_eth is
	generic(
		RAM_RD_CYCLE						:	natural		:=	1;
		RAM_WR_CYCLE						:	natural		:=	1;	
		RAM_RD_DELAY						:	natural		:=	1 ; --1
		RAM_AWIDTH							:   natural 		:=  32							
		);
	port(	
		txclk			:	in	std_logic;
		txd				:	out	std_logic_vector(7 downto 0);
		txen			:	out	std_logic;
		
		rxclk			:	in	std_logic;
		rxd				:	in	std_logic_vector(7 downto 0);
		rxdv			:	in	std_logic;
		
		clk				:	in	std_logic;
		reset			:	in	std_logic;
		zcpsm_clk		:	in	std_logic;	
		
		TxFIFO_W_Clk	: 	in std_logic;
		TxFIFO_Clr		: 	in std_logic;
		TxFIFO_W_Block	: 	in std_logic;		
		TxFIFO_WE		: 	in std_logic;
		TxFIFO_WAddr	: 	in std_logic_vector( TX_TASKFIFO_BLOCK_AWIDTH - 1 downto 0 );
		TxFIFO_WData	: 	in std_logic_vector( TASKFIFO_DWIDTH - 1 downto 0 );
		TxFIFO_Full		: 	out	std_logic;

		RxFIFO_R_Clk	: 	in std_logic;
		RxFIFO_R_Block	: 	in std_logic;		
		RxFIFO_RAddr	: 	in std_logic_vector( RX_TASKFIFO_BLOCK_AWIDTH - 1 downto 0 );
		RxFIFO_RData	: 	out std_logic_vector( TASKFIFO_DWIDTH - 1 downto 0 );
		RxFIFO_Empty	: 	out	std_logic;	
		
		localtime		: 	in  std_logic_vector(31 downto 0);
		recvtime		:	out std_logic_vector(31 downto 0);
		recvtime_valid	:	out std_logic;	
		localtime_locked:   out std_logic;
 
		
		debugIO_port_id		:	out std_logic_vector(15 downto 0);
		debugIO_write_strobe:	out std_logic;
		debugIO_out_port	:	out std_logic_vector(15 downto 0);
		debugIO_read_strobe	:	out std_logic;
		debugIO_in_port		:	in  std_logic_vector(15 downto 0);	
		
		
 ------------------------------------------------------------------------
		ram_wren			:	out	std_logic;
		ram_waddr			:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
		ram_wdata			: 	out	std_logic_vector(31 downto 0);
		ram_raddr			:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
		ram_rdata			:	in	std_logic_vector(31 downto 0); 
		
		--
		test				:	out std_logic_vector(1 downto 0);
		
		s_HighPri_Tx_Req			: 	in std_logic;
		m48_HighPri_Tx_Req_DesMac	: 	in std_logic_vector( 47 downto 0 );
		m16_HighPri_Tx_Req_Addr		: 	in std_logic_vector( 15 downto 0 );
		m16_HighPri_Tx_Req_Data		: 	in std_logic_vector( 15 downto 0 );

		local_id_MAC0_Req	:	in	std_logic_vector(7 downto 0);
		local_id_MAC0_A		:	in	std_logic_vector(7 downto 0);	
		local_id_MAC0_B		:	in	std_logic_vector(7 downto 0);	
		local_id			:	in	std_logic_vector(39 downto 0)	
		);
end entity;

architecture arch_eth of g_eth is
	
	component g_ethrx
	generic(
		HEAD_AWIDTH : NATURAL := 5;
		BUFF_AWIDTH : NATURAL := 12;
		FIFO_AWIDTH : NATURAL := 2;
		WR_CYCLE : NATURAL := 1;
		RAM_AWIDTH : NATURAL :=32 
		);
	port(
		clk : in std_logic;
		zcpsm_clk : in std_logic;
		reset : in std_logic;
		rxclk : in std_logic;
		rxd : in std_logic_vector(7 downto 0);
		rxdv : in std_logic;
		db_ce : in std_logic;
		db_port_id : in std_logic_vector(3 downto 0);
		db_write_strobe : in std_logic;
		db_out_port : in std_logic_vector(7 downto 0);
		db_read_strobe : in std_logic;
		db_in_port : out std_logic_vector(7 downto 0);
		eth_ce : in std_logic;
		eth_port_id : in std_logic_vector(3 downto 0);
		eth_write_strobe : in std_logic;
		eth_out_port : in std_logic_vector(7 downto 0);
		eth_read_strobe : in std_logic;
		eth_in_port : out std_logic_vector(7 downto 0);
		eth_dma_ce : in std_logic; 
		
		ethrx_busy	: out std_logic;
		recvtime 			:	out std_logic_vector(31 downto 0);
		recvtime_valid		:	out	std_logic;	
		localtime_locked	:	out	std_logic;
		lastframe_flag		:	out	std_logic;
		
		ram_wren : out std_logic;
		ram_waddr : out std_logic_vector(RAM_AWIDTH - 1 downto 0);	
		---------------
--		test : out std_logic_vector(3 downto 0); 				   
		
		ram_wdata : out std_logic_vector(31 downto 0));
	end component;
	
	component ethrx_zcpsm
		port(
			reset				:	in	std_logic;
			clk			:	in	std_logic;
			
			port_id				:	out	std_logic_vector(7 downto 0);
			write_strobe		:	out	std_logic;
			out_port			:	out	std_logic_vector(7 downto 0);
			read_strobe			:	out	std_logic;
			in_port				:	in	std_logic_vector(7 downto 0)
			);
	end component;	
	
	component ethrx_task 
		generic(
			TASKFIFO_DWIDTH				: natural := 8;
			TASKFIFO_BLOCK_DEPTH		: natural := 8;
			TASKFIFO_BLOCK_AWIDTH		: natural := 3;
			TASKFIFO_DEPTH				: natural := 16;
			TASKFIFO_AWIDTH				: natural := 4;
			TASKFIFO_RAM_TYPE			: string  := "DIS_RAM"
			);
		port(
			reset						:	in	std_logic;
			--	Task Input 
			RxFIFO_R_Clk				: 	in std_logic;
			RxFIFO_R_Block				: 	in std_logic;		
			RxFIFO_RAddr				: 	in std_logic_vector( TASKFIFO_BLOCK_AWIDTH - 1 downto 0 );
			RxFIFO_RData				: 	out std_logic_vector( TASKFIFO_DWIDTH - 1 downto 0 );
			RxFIFO_Full					: 	out	std_logic;
			RxFIFO_Empty				: 	out	std_logic;
			
			fifo_wr_block				:	in	std_logic;
			--	zcpsm
			zcpsm_clk					:	in	std_logic;
			zcpsm_ce					:	in	std_logic;
			zcpsm_port_id				:	in	std_logic_vector(3 downto 0);
			zcpsm_write_strobe			:	in	std_logic;
			zcpsm_out_port				:	in	std_logic_vector(7 downto 0);
			zcpsm_read_strobe			:	in	std_logic;
			zcpsm_in_port				:	out	std_logic_vector(7 downto 0)
			);
	end component;	 
	
	component dma2rxtask 
		port(			
			reset			:	in	std_logic;
			zcpsm_clk		:	in	std_logic;
			busy			:	in std_logic;
			lastframe		:	in	std_logic;
			rxtask_wr_block	:	out	std_logic
			
		);
	end component;	
		
	component asyncwrite
	port(
		reset : in std_logic;
		async_clk : in std_logic;
		sync_clk : in std_logic;
		async_wren : in std_logic;
		trigger : in std_logic;
		sync_wren : out std_logic;
		over : out std_logic;
		flag : out std_logic);
	end component;
	
	component g_ethtx
	generic(
		HEAD_AWIDTH 		: NATURAL := 5;
		BUFF_AWIDTH 		: NATURAL := 5;
		FIFO_AWIDTH 		: NATURAL := 2;
		RD_CYCLE 			: NATURAL := 1;
		RD_DELAY 			: NATURAL := 1;
		RAM_AWIDTH			: NATURAL := 32
		);
	port(
		clk 				: in std_logic;
		zcpsm_clk 			: in std_logic;
		reset 				: in std_logic;
		txclk 				: in std_logic;
		txd 				: out std_logic_vector(7 downto 0);
		txen 				: out std_logic;
		
		eth_ce 				: in std_logic;
		eth_port_id 		: in std_logic_vector(3 downto 0);
		eth_write_strobe 	: in std_logic;
		eth_out_port 		: in std_logic_vector(7 downto 0);
		eth_read_strobe 	: in std_logic;	
		eth_in_port 		: out std_logic_vector(7 downto 0);	 
		
		db_ce 				: in std_logic;
		db_port_id 			: in std_logic_vector(3 downto 0);
		db_write_strobe 	: in std_logic;
		db_out_port 		: in std_logic_vector(7 downto 0);
		db_read_strobe 		: in std_logic;
		db_in_port 			: out std_logic_vector(7 downto 0);	
		
		ram_raddr 			: out std_logic_vector(RAM_AWIDTH - 1 downto 0);
		ram_rdata 			: in std_logic_vector(31 downto 0);
		-- localtime --
		localtime			: in std_logic_vector(31 downto 0)
		);
	end component;
	
	component ethtx_zcpsm
	port(
		reset 				: in std_logic;
		clk 			: in std_logic;
		port_id 			: out std_logic_vector(7 downto 0);
		write_strobe 		: out std_logic;
		out_port 			: out std_logic_vector(7 downto 0);
		read_strobe 		: out std_logic;
		in_port 			: in std_logic_vector(7 downto 0)
		);
	end component;	
	
	component ethtx_task 
	generic(
		TASKFIFO_DWIDTH				: natural := 8;
		TASKFIFO_BLOCK_DEPTH		: natural := 16;
		TASKFIFO_BLOCK_AWIDTH		: natural := 4;
		TASKFIFO_DEPTH				: natural := 16;
		TASKFIFO_AWIDTH				: natural := 4;
		TASKFIFO_RAM_TYPE			: string  := "DIS_RAM"
		);
	port(
		reset						:	in	std_logic;
		--	Task Input 
		TxFIFO_W_Clk				: 	in std_logic;
		TxFIFO_Clr					: 	in std_logic;
		TxFIFO_W_Block				: 	in std_logic;		
		TxFIFO_WE					: 	in std_logic;
		TxFIFO_WAddr				: 	in std_logic_vector( TASKFIFO_BLOCK_AWIDTH - 1 downto 0 );
		TxFIFO_WData				: 	in std_logic_vector( TASKFIFO_DWIDTH - 1 downto 0 );
		TxFIFO_Full					: 	out	std_logic;
		TxFIFO_Empty				:	out std_logic;
		--	zcpsm
		zcpsm_clk					:	in	std_logic;
		zcpsm_ce					:	in	std_logic;
		zcpsm_port_id				:	in	std_logic_vector(3 downto 0);
		zcpsm_write_strobe			:	in	std_logic;
		zcpsm_out_port				:	in	std_logic_vector(7 downto 0);
		zcpsm_read_strobe			:	in	std_logic;
		zcpsm_in_port				:	out	std_logic_vector(7 downto 0)
		);
	end component;
	
	component Eth_Tx_HighPriority
		port(
		reset				: in std_logic;
		clk					: in std_logic;
		clk_zcpsm			: in std_logic;
		
		s_Tx_Req			: in std_logic;
		m48_Tx_Req_DesMac	: in std_logic_vector( 47 downto 0 );
		m16_Tx_Req_Addr		: in std_logic_vector( 15 downto 0 );
		m16_Tx_Req_Data		: in std_logic_vector( 15 downto 0 );
		
		port_id 			: in std_logic_vector(7 downto 0);
		write_strobe 		: in std_logic;
		out_port 			: in std_logic_vector(7 downto 0);
		read_strobe 		: in std_logic;
		in_port 			: out std_logic_vector(7 downto 0)
		);
	end component;
		
	component db_zcpsm
	port(
		reset : in std_logic;
		clk : in std_logic;
		port_id : out std_logic_vector(7 downto 0);
		write_strobe : out std_logic;
		out_port : out std_logic_vector(7 downto 0);
		read_strobe : out std_logic;
		in_port : in std_logic_vector(7 downto 0));
	end component;
 	
	component zcpsmIO2bus16
	port(
		reset : in std_logic;
		debug_port_id : out std_logic_vector(15 downto 0);
		debug_write_strobe : out std_logic;
		debug_out_port : out std_logic_vector(15 downto 0);
		debug_read_strobe : out std_logic;
		debug_in_port : in std_logic_vector(15 downto 0);
		zcpsm_clk : in std_logic;
		zcpsm_ce : in std_logic;
		zcpsm_port_id : in std_logic_vector(3 downto 0);
		zcpsm_write_strobe : in std_logic;
		zcpsm_out_port : in std_logic_vector(7 downto 0);
		zcpsm_read_strobe : in std_logic;
		zcpsm_in_port : out std_logic_vector(7 downto 0));
	end component;

	
	signal ethrx_port_id			:	std_logic_vector(7 downto 0);
	signal ethrx_write_strobe		:	std_logic;
	signal ethrx_out_port			:	std_logic_vector(7 downto 0);
	signal ethrx_read_strobe		:	std_logic;
	signal ethrx_in_port			:	std_logic_vector(7 downto 0);

	signal ethtx_port_id		:	std_logic_vector(7 downto 0);
	signal ethtx_write_strobe	:	std_logic;
	signal ethtx_out_port		:	std_logic_vector(7 downto 0);
	signal ethtx_read_strobe	:	std_logic;
	signal ethtx_in_port		:	std_logic_vector(7 downto 0);

	
	signal db_port_id			:	std_logic_vector(7 downto 0);
	signal db_write_strobe		:	std_logic;
	signal db_out_port			:	std_logic_vector(7 downto 0);
	signal db_read_strobe		:	std_logic;
	signal db_in_port			:	std_logic_vector(7 downto 0);
	
	signal debug_port_id		:	std_logic_vector(15 downto 0);
	signal debug_write_strobe	:	std_logic;
	signal debug_out_port		:	std_logic_vector(15 downto 0);
	signal debug_read_strobe	:	std_logic;
	signal debug_in_port		:	std_logic_vector(15 downto 0);
	
	signal debug_in_port_pro	:	std_logic_vector(15 downto 0);
	
	signal lastframe_flag		:	std_logic;
	signal ethrx_busy			:	std_logic;
	signal rxtask_wr_block		:	std_logic;
	signal rxtask_wr_block_Reg	:	std_logic;
	
	signal ethtx_task_ce		:	std_logic;
	signal eth_tx_ce			:	std_logic;
	
	signal eth_rx_ce			:	std_logic;
	signal eth_rxdma_ce			:	std_logic;
	signal ethrx_task_ce		:	std_logic;
	
	signal db_rx_ce				:	std_logic;
	signal db_tx_ce				:	std_logic;
	signal db_debug_ce			:	std_logic;
	signal debug_prog_ce		:	std_logic; 
	
--	signal test_0				:	std_logic_vector(3 downto 0);
	signal txen_buf				:	std_logic;
	
begin

	test(0)				<=  not rxdv;
	test(1)				<=	not txen_buf;  
--	test(2)				<=	test_0(2);
--	test(3)				<=	test_0(3);	   


	------------------------------------------------------------------------------
	--	RX
	------------------------------------------------------------------------------
	
	u_rx : g_ethrx
	generic map(
		HEAD_AWIDTH => ETHRX_HEAD_AWIDTH,
		BUFF_AWIDTH => ETHRX_BUFF_AWIDTH,
		FIFO_AWIDTH => ETHRX_FIFO_AWIDTH,
		WR_CYCLE => RAM_WR_CYCLE,
		RAM_AWIDTH => RAM_AWIDTH
		)
	port map(
		clk => clk,
		zcpsm_clk => zcpsm_clk,
		reset => reset,
		rxclk => rxclk,
		rxd => rxd,
		rxdv => rxdv,
		db_ce => db_rx_ce,
		db_port_id => db_port_id(3 downto 0),
		db_write_strobe => db_write_strobe,
		db_out_port => db_out_port,
		db_read_strobe => db_read_strobe,
		db_in_port => db_in_port,
		eth_ce => eth_rx_ce,
		eth_port_id => ethrx_port_id(3 downto 0),
		eth_write_strobe => ethrx_write_strobe,
		eth_out_port => ethrx_out_port,
		eth_read_strobe => ethrx_read_strobe,
		eth_in_port => ethrx_in_port,
		eth_dma_ce => eth_rxdma_ce,	 
		
		ethrx_busy	=>	ethrx_busy,
		recvtime 			=>	recvtime,
		recvtime_valid		=>	recvtime_valid,	
		localtime_locked	=>  localtime_locked,
		lastframe_flag		=>	lastframe_flag,
		
		ram_wren => ram_wren,
		ram_waddr => ram_waddr,	
		-----
--		test	=> test_0,
		ram_wdata => ram_wdata
		);
	
	db_rx_ce <= '1' when db_port_id(7 downto 4) = PORTS_DB_RX else '0';
	eth_rx_ce <= '1' when ethrx_port_id(7 downto 4) = PORTS_ETH_RX else '0';
	eth_rxdma_ce <= '1' when ethrx_port_id(7 downto 4) = PORTS_ETH_RXDMA else '0';

	u_ethrx_zcpsm : ethrx_zcpsm
	port map(
		reset 					=> reset,
		clk 				=> zcpsm_clk,
		port_id 				=> ethrx_port_id,
		write_strobe 			=> ethrx_write_strobe,
		out_port 				=> ethrx_out_port,
		read_strobe 			=> ethrx_read_strobe,
		in_port 				=> ethrx_in_port
		); 
		
		
	u_ethrx_task : ethrx_task  
	generic map (
		TASKFIFO_DWIDTH			=> TASKFIFO_DWIDTH,
		TASKFIFO_BLOCK_DEPTH	=> RX_TASKFIFO_BLOCK_DEPTH,
		TASKFIFO_BLOCK_AWIDTH	=> RX_TASKFIFO_BLOCK_AWIDTH,
		TASKFIFO_DEPTH			=> RX_TASKFIFO_DEPTH,
		TASKFIFO_AWIDTH			=> RX_TASKFIFO_AWIDTH,
		TASKFIFO_RAM_TYPE		=> RX_TASKFIFO_RAM_TYPE
		)
	port map(
		reset						=>	reset,
		--	Task Input 
		RxFIFO_R_Clk				=> 	RxFIFO_R_Clk,
		RxFIFO_R_Block				=> 	RxFIFO_R_Block,		
		RxFIFO_RAddr				=> 	RxFIFO_RAddr,
		RxFIFO_RData				=> 	RxFIFO_RData,
		RxFIFO_Full					=>	open,  
--		RxFIFO_Full					=>	RxFIFO_Full,		
		RxFIFO_Empty				=> 	RxFIFO_Empty,
		
		fifo_wr_block				=>	rxtask_wr_block,
		--	zcpsm
		zcpsm_clk					=>	zcpsm_clk,
		zcpsm_ce					=>	ethrx_task_ce,
		zcpsm_port_id				=>	ethrx_port_id(3 downto 0),
		zcpsm_write_strobe			=>	ethrx_write_strobe,
		zcpsm_out_port				=>	ethrx_out_port,
		zcpsm_read_strobe			=>	ethrx_read_strobe,
		zcpsm_in_port				=>	ethrx_in_port
		);

	ethrx_task_ce <= '1' when ethrx_port_id(7 downto 4) = PORTS_ETH_RX_TASK else '0'; 
		
	
	u_dma2rxtask: dma2rxtask  
		port map(			
			reset					=>	reset,
			zcpsm_clk				=>	zcpsm_clk,
			busy					=>	ethrx_busy,
			lastframe				=>	lastframe_flag,
			rxtask_wr_block			=>	rxtask_wr_block_Reg
			
		);
		
	ethrx_in_port <= 	local_id_MAC0_A when ethrx_port_id = PORT_ETH_LOCAL_ID_0_A else
						local_id_MAC0_B when ethrx_port_id = PORT_ETH_LOCAL_ID_0_B else
						local_id( 39 downto 32 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_1 else
						local_id( 31 downto 24 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_2 else
						local_id( 23 downto 16 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_3 else
						local_id( 15 downto 8 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_4 else
						local_id( 7 downto 0 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_5 else
						(others => 'Z');		

	u_wr_block : asyncwrite		 -- rxtask_wr_block must be synchronized with clk
	port map(
		reset => reset,
		async_clk => zcpsm_clk,
		sync_clk => clk,
		async_wren => rxtask_wr_block_Reg,
		trigger => '1',
		sync_wren => rxtask_wr_block,
		over => open,
		flag => open
		);
	------------------------------------------------------------------------------
	--	TX
	------------------------------------------------------------------------------
	
	u_tx : g_ethtx
	generic map(
		HEAD_AWIDTH => ETHTX_HEAD_AWIDTH,
		BUFF_AWIDTH => ETHTX_BUFF_AWIDTH,
		FIFO_AWIDTH => ETHTX_FIFO_AWIDTH,
		RD_CYCLE => RAM_RD_CYCLE,
		RD_DELAY => RAM_RD_DELAY,
		RAM_AWIDTH => RAM_AWIDTH
		)
	port map(
		clk => clk,
		zcpsm_clk => zcpsm_clk,
		reset => reset,
		txclk => txclk,
		txd => txd,
		txen => txen_buf,
		db_ce => db_tx_ce,
		db_port_id => db_port_id(3 downto 0),
		db_write_strobe => db_write_strobe,
		db_out_port => db_out_port,
		db_read_strobe => db_read_strobe,
		db_in_port => db_in_port,
		eth_ce => eth_tx_ce,
		eth_port_id => ethtx_port_id(3 downto 0),
		eth_write_strobe => ethtx_write_strobe,
		eth_out_port => ethtx_out_port,
		eth_read_strobe => ethtx_read_strobe,
		eth_in_port => ethtx_in_port,
		ram_raddr => ram_raddr,
		ram_rdata => ram_rdata,
		-- local time--
		localtime => localtime
		);
	
	txen <= txen_buf;
	
	db_tx_ce <= '1' when db_port_id(7 downto 4) = PORTS_DB_TX else '0';
	eth_tx_ce <= '1' when ethtx_port_id(7 downto 4) = PORTS_ETH_TX else '0';

	-- eth tx zcpsm
	
	u_ethtx_zcpsm : ethtx_zcpsm
	port map(
		reset 					=> reset,
		clk 				=> zcpsm_clk,
		port_id 				=> ethtx_port_id,
		write_strobe 			=> ethtx_write_strobe,
		out_port 				=> ethtx_out_port,
		read_strobe 			=> ethtx_read_strobe,
		in_port 				=> ethtx_in_port
		);
		
	mo_Eth_Tx_HighPriority : Eth_Tx_HighPriority
		port map(
		reset				=> reset,
		clk					=> clk,
		clk_zcpsm			=> zcpsm_clk,
		
		s_Tx_Req			=> s_HighPri_Tx_Req,
		m48_Tx_Req_DesMac	=> m48_HighPri_Tx_Req_DesMac,
		m16_Tx_Req_Addr		=> m16_HighPri_Tx_Req_Addr,
		m16_Tx_Req_Data		=> m16_HighPri_Tx_Req_Data,
		
		port_id 			=> ethtx_port_id,     
		write_strobe 		=> ethtx_write_strobe,
		out_port 			=> ethtx_out_port,    
		read_strobe 		=> ethtx_read_strobe, 
		in_port 			=> ethtx_in_port    
		);

	u_ethtx_task : 	ethtx_task 
	generic map(
		TASKFIFO_DWIDTH			=> TASKFIFO_DWIDTH,
		TASKFIFO_BLOCK_DEPTH	=> TX_TASKFIFO_BLOCK_DEPTH,
		TASKFIFO_BLOCK_AWIDTH	=> TX_TASKFIFO_BLOCK_AWIDTH,
		TASKFIFO_DEPTH			=> TX_TASKFIFO_DEPTH,
		TASKFIFO_AWIDTH			=> TX_TASKFIFO_AWIDTH,
		TASKFIFO_RAM_TYPE		=> TX_TASKFIFO_RAM_TYPE
		)
	port map(
		reset					=>	reset,
		--	Task Input 
		TxFIFO_W_Clk			=> 	TxFIFO_W_Clk,
		TxFIFO_Clr				=> 	TxFIFO_Clr,
		TxFIFO_W_Block			=> 	TxFIFO_W_Block,		
		TxFIFO_WE				=> 	TxFIFO_WE,
		TxFIFO_WAddr			=> 	TxFIFO_WAddr,
		TxFIFO_WData			=> 	TxFIFO_WData,
		TxFIFO_Full				=> 	TxFIFO_Full, 
--		TxFIFO_Empty			=>	TxFIFO_Empty,
		TxFIFO_Empty			=>	open,

		--	zcpsm
		zcpsm_clk				=>	zcpsm_clk,
		zcpsm_ce				=>	ethtx_task_ce,
		zcpsm_port_id			=>	ethtx_port_id(3 downto 0),
		zcpsm_write_strobe		=>	ethtx_write_strobe,
		zcpsm_out_port			=>	ethtx_out_port,
		zcpsm_read_strobe		=>	ethtx_read_strobe,
		zcpsm_in_port			=>	ethtx_in_port
		);


	ethtx_task_ce <= '1' when ethtx_port_id(7 downto 4) = PORTS_ETH_TX_TASK else '0';					
	ethtx_in_port <= 	local_id_MAC0_Req when ethtx_port_id = PORT_ETH_LOCAL_ID_0_REQ else
						local_id_MAC0_A when ethtx_port_id = PORT_ETH_LOCAL_ID_0_A else
						local_id_MAC0_B when ethtx_port_id = PORT_ETH_LOCAL_ID_0_B else
						local_id( 39 downto 32 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_1 else
						local_id( 31 downto 24 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_2 else
						local_id( 23 downto 16 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_3 else
						local_id( 15 downto 8 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_4 else
						local_id( 7 downto 0 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_5 else
						(others => 'Z');
			
	------------------------------------------------------------------------------
	--	DB zcpsm
	------------------------------------------------------------------------------
	
	u_db_zcpsm : db_zcpsm
	port map(
		reset => reset,
		clk => zcpsm_clk,
		port_id => db_port_id,
		write_strobe => db_write_strobe,
		out_port => db_out_port,
		read_strobe => db_read_strobe,
		in_port => db_in_port
		);
	
	------------------------------------------------------------------------------
	--	DEBUG & PROG
	------------------------------------------------------------------------------
	
	u_zcpsmIO2bus16 : zcpsmIO2bus16
	port map(
		reset 					=> reset,
		zcpsm_clk 				=> zcpsm_clk,
		debug_port_id 			=> debug_port_id,
		debug_write_strobe 		=> debug_write_strobe,
		debug_out_port 			=> debug_out_port,
		debug_read_strobe 		=> debug_read_strobe,
		debug_in_port 			=> debug_in_port,
		zcpsm_ce 				=> db_debug_ce,
		zcpsm_port_id 			=> db_port_id(3 downto 0),
		zcpsm_write_strobe 		=> db_write_strobe,
		zcpsm_out_port	 		=> db_out_port,
		zcpsm_read_strobe 		=> db_read_strobe,
		zcpsm_in_port 			=> db_in_port
		);
	
	db_debug_ce <= '1' when db_port_id(7 downto 4) = PORTS_DB_DEBUG else '0';
	
	------------------------------------------------------------------------------
	-- IO
	------------------------------------------------------------------------------
	
	debugIO_port_id		<= debug_port_id;	
	debugIO_write_strobe<= debug_write_strobe;	
	debugIO_out_port	<= debug_out_port;	
	debugIO_read_strobe	<= debug_read_strobe;	
	debug_in_port		<= debug_in_port_pro when debug_port_id(15 downto 12) = PORTS_DEBUG_PROG else
						   debugIO_in_port;	 
	
	------------------------------------------------------------------------------
	--	LOCAL ID
	------------------------------------------------------------------------------
	
	db_in_port <= 	local_id_MAC0_A when db_port_id = PORT_DB_LOCAL_ID_0_A else
					local_id_MAC0_B when db_port_id = PORT_DB_LOCAL_ID_0_B else
					local_id( 39 downto 32 ) when db_port_id = PORT_DB_LOCAL_ID_1 else
					local_id( 31 downto 24 ) when db_port_id = PORT_DB_LOCAL_ID_2 else
					local_id( 23 downto 16 ) when db_port_id = PORT_DB_LOCAL_ID_3 else
					local_id( 15 downto 8 ) when db_port_id = PORT_DB_LOCAL_ID_4 else
					local_id( 7 downto 0 ) when db_port_id = PORT_DB_LOCAL_ID_5 else
					(others => 'Z');
	
end arch_eth;
