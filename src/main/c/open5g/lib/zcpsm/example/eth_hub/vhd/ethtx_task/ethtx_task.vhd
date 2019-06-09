---------------------------------------------------------------------------------------------------
--
-- Title       : ethtx_task
-- Design      : eth_new
-- Author      : lihf
-- Company     : wireless
--
---------------------------------------------------------------------------------------------------
--
-- File        : ethtx_task.vhd
-- Generated   : Tue Aug 29 10:23:40 2006
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
--{entity {ethtx_task} architecture {arch_ethtx_task}}

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity ethtx_task is
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
end ethtx_task;

--}} End of automatically maintained section

architecture arch_ethtx_task of ethtx_task is  

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
	
	component fifo2zcpsm
		generic(
			BLOCK_AWIDTH 			: INTEGER;
			DWIDTH 					: INTEGER);
		port(
			clk 					: in std_logic;
			reset 					: in std_logic;
			zcpsm_clk 				: in std_logic;
			zcpsm_ce 				: in std_logic;
			zcpsm_port_id 			: in std_logic_vector(3 downto 0);
			zcpsm_write_strobe 		: in std_logic;
			zcpsm_out_port			: in std_logic_vector(7 downto 0);
			zcpsm_read_strobe 		: in std_logic;
			zcpsm_in_port 			: out std_logic_vector(7 downto 0);
			fifo_rd_block 			: out std_logic;
			fifo_raddr 				: out std_logic_vector((BLOCK_AWIDTH-1) downto 0);
			fifo_rdata 				: in std_logic_vector((DWIDTH-1) downto 0);
			fifo_full 				: in std_logic;
			fifo_empty 				: in std_logic);
	end component;

	signal fifo_full				:	std_logic;
	signal fifo_empty				:	std_logic;
	signal fifo_raddr				:	std_logic_vector(TASKFIFO_BLOCK_AWIDTH - 1 downto 0);
	signal fifo_rdata				:	std_logic_vector(TASKFIFO_DWIDTH - 1 downto 0);
	signal fifo_rd_block			:	std_logic;
	
begin
	
	u_task_fifo : fifo_block
	generic map(
		DWIDTH 						=> TASKFIFO_DWIDTH,
		BLOCK_AWIDTH 				=> TASKFIFO_BLOCK_AWIDTH,
		FIFO_AWIDTH 				=> TASKFIFO_AWIDTH,
		RAM_TYPE					=> TASKFIFO_RAM_TYPE
		)
	port map(
		clk 						=> TxFIFO_W_Clk,
		reset 						=> reset,
		clr	 						=> TxFIFO_Clr,
		wr_block 					=> TxFIFO_W_Block,
		wr_clk 						=> TxFIFO_W_Clk,
		wren 						=> TxFIFO_WE,
		waddr 						=> TxFIFO_WAddr,
		wdata 						=> TxFIFO_WData,
		rd_block 					=> fifo_rd_block,
		rd_clk 						=> zcpsm_clk,
		raddr 						=> fifo_raddr,
		rdata 						=> fifo_rdata,
		empty 						=> fifo_empty,
		full 						=> fifo_full
		);
	
	u_zcpsm_task : fifo2zcpsm
	generic map(
		BLOCK_AWIDTH 				=> TASKFIFO_BLOCK_AWIDTH,
		DWIDTH 						=> TASKFIFO_DWIDTH
		)
	port map(
		clk 						=> TxFIFO_W_Clk,
		reset 						=> reset,
		zcpsm_clk 					=> zcpsm_clk,
		zcpsm_ce 					=> zcpsm_ce,
		zcpsm_port_id 				=> zcpsm_port_id,
		zcpsm_write_strobe 			=> zcpsm_write_strobe,
		zcpsm_out_port 				=> zcpsm_out_port,
		zcpsm_read_strobe 			=> zcpsm_read_strobe,
		zcpsm_in_port 				=> zcpsm_in_port,
		fifo_rd_block 				=> fifo_rd_block,
		fifo_raddr 					=> fifo_raddr,
		fifo_rdata 					=> fifo_rdata,
		fifo_full 					=> fifo_full,
		fifo_empty 					=> fifo_empty
		);	 
		
	TxFIFO_Full <= fifo_full;
	TxFIFO_Empty <= fifo_empty;
	 -- enter your statements here --

end arch_ethtx_task;
