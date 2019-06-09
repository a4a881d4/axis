---------------------------------------------------------------------------------------------------
--
-- Title       : ethrx_task
-- Design      : eth_new
-- Author      : dove
-- Company     : google
--
---------------------------------------------------------------------------------------------------
--
-- File        : ethrx_task.vhd
-- Generated   : Sun Sep  3 10:52:10 2006
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.20
--
---------------------------------------------------------------------------------------------------
--
-- Description : 
--
---------------------------------------------------------------------------------------------------

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {ethrx_task} architecture {arch_ethrx_task}}

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity ethrx_task is 
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
		RxFIFO_R_Clk				: 	in std_logic;
		RxFIFO_R_Block				: 	in std_logic;		
		RxFIFO_RAddr				: 	in std_logic_vector( TASKFIFO_BLOCK_AWIDTH - 1 downto 0 );
		RxFIFO_RData				: 	out std_logic_vector( TASKFIFO_DWIDTH - 1 downto 0 );
		RxFIFO_Full					:	out std_logic;
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
end ethrx_task;

--}} End of automatically maintained section

architecture arch_ethrx_task of ethrx_task is
	component fifo_block
		generic(
			DWIDTH 					: INTEGER; 			
			BLOCK_AWIDTH 			: INTEGER;	
			FIFO_AWIDTH 			: INTEGER;	    
			RAM_TYPE 				: STRING
			);
		port(
			clk 					: in std_logic;
			reset 					: in std_logic;
			clr 					: in std_logic;	  
			
			wr_block 				: in std_logic;
			wr_clk 					: in std_logic;
			wren 					: in std_logic;
			waddr 					: in std_logic_vector((BLOCK_AWIDTH-1) downto 0);
			wdata 					: in std_logic_vector((DWIDTH-1) downto 0); 
			
			rd_block 				: in std_logic;
			rd_clk 					: in std_logic;
			raddr 					: in std_logic_vector((BLOCK_AWIDTH-1) downto 0);
			rdata 					: out std_logic_vector((DWIDTH-1) downto 0);
			full 					: out std_logic;
			empty 					: out std_logic
			);
	end component;
	
	component zcpsm2fifo
		generic(
			BLOCK_AWIDTH 			: INTEGER;
			DWIDTH 					: INTEGER
			);
		port(
			clk 					: in std_logic;
			reset 					: in std_logic;
			zcpsm_clk 				: in std_logic;
			zcpsm_ce 				: in std_logic;
			zcpsm_port_id 			: in std_logic_vector(3 downto 0);
			zcpsm_write_strobe 		: in std_logic;
			zcpsm_out_port 			: in std_logic_vector(7 downto 0);
			zcpsm_read_strobe 		: in std_logic;
			zcpsm_in_port 			: out std_logic_vector(7 downto 0);
			fifo_wr_block 			: out std_logic;
			fifo_wren 				: out std_logic;
			fifo_waddr 				: out std_logic_vector((BLOCK_AWIDTH-1) downto 0);
			fifo_wdata 				: out std_logic_vector((DWIDTH-1) downto 0);
			fifo_full 				: in std_logic;
			fifo_empty 				: in std_logic
			);
	end component;

	signal fifo_full				:	std_logic;
	signal fifo_empty				:	std_logic;
	signal fifo_wren				:	std_logic;
	signal fifo_waddr				:	std_logic_vector(TASKFIFO_BLOCK_AWIDTH - 1 downto 0);
	signal fifo_wdata				:	std_logic_vector(TASKFIFO_DWIDTH - 1 downto 0);	
	
	signal fifo_db_wr_block			:	std_logic;	
	signal fifo_rxtask_wr_block		:	std_logic;
--	signal fifo_wr_block			:	std_logic;	
	
begin
	
	fifo_rxtask_wr_block <=  fifo_db_wr_block or fifo_wr_block;
	
	u_rx_task_fifo : fifo_block
	generic map(
		DWIDTH 						=> TASKFIFO_DWIDTH,
		BLOCK_AWIDTH 				=> TASKFIFO_BLOCK_AWIDTH,
		FIFO_AWIDTH 				=> TASKFIFO_AWIDTH,
		RAM_TYPE					=> TASKFIFO_RAM_TYPE
		)
	port map(
		clk 						=> RxFIFO_R_Clk,
		reset 						=> reset,
		clr	 						=> '0',
		wr_block 					=> fifo_rxtask_wr_block,
		wr_clk 						=> zcpsm_clk,
		wren 						=> fifo_wren,
		waddr 						=> fifo_waddr,
		wdata 						=> fifo_wdata,
		rd_block 					=> RxFIFO_R_Block,
		rd_clk 						=> RxFIFO_R_Clk,
		raddr 						=> RxFIFO_RAddr,
		rdata 						=> RxFIFO_RData,
		empty 						=> fifo_empty,
		full 						=> fifo_full
		);
	
	u_rx_zcpsm_task : zcpsm2fifo
	generic map(
		BLOCK_AWIDTH 				=> TASKFIFO_BLOCK_AWIDTH,
		DWIDTH 						=> TASKFIFO_DWIDTH
		)
	port map(
		clk 						=> RxFIFO_R_Clk,
		reset 						=> reset,
		zcpsm_clk 					=> zcpsm_clk,
		zcpsm_ce 					=> zcpsm_ce,
		zcpsm_port_id 				=> zcpsm_port_id,
		zcpsm_write_strobe 			=> zcpsm_write_strobe,
		zcpsm_out_port 				=> zcpsm_out_port,
		zcpsm_read_strobe 			=> zcpsm_read_strobe,
		zcpsm_in_port 				=> zcpsm_in_port,
--		fifo_wr_block 				=> open,		
		fifo_wr_block 				=> fifo_db_wr_block,
		fifo_wren 					=> fifo_wren,
		fifo_waddr 					=> fifo_waddr,
		fifo_wdata 					=> fifo_wdata,
		fifo_full 					=> fifo_full,
		fifo_empty 					=> fifo_empty
		);
	
	RxFIFO_Empty <= fifo_empty;	 
	RxFIFO_Full  <= fifo_full;	
	 -- enter your statements here --

end arch_ethrx_task;
