library ieee;
use ieee.std_logic_1164.all;

package eth_config is
	
	constant ETHRX_HEAD_AWIDTH		:	natural		:=	6;
	constant ETHRX_BUFF_AWIDTH		:	natural		:=	12;
	constant ETHRX_FIFO_AWIDTH		:	natural		:=	2;
		
	constant ETHTX_HEAD_AWIDTH		:	natural		:=	6;
	constant ETHTX_BUFF_AWIDTH		:	natural		:=	5;
	constant ETHTX_FIFO_AWIDTH		:	natural		:=	0;

	constant TASKFIFO_DWIDTH				: natural := 8;
	constant TX_TASKFIFO_BLOCK_DEPTH		: natural := 32;
	constant TX_TASKFIFO_BLOCK_AWIDTH		: natural := 5;	
	constant RX_TASKFIFO_BLOCK_DEPTH		: natural := 32;
	constant RX_TASKFIFO_BLOCK_AWIDTH		: natural := 5;	
		
	constant TX_TASKFIFO_DEPTH				: natural := 64;--16;
	constant TX_TASKFIFO_AWIDTH				: natural := 6;--4;
	constant RX_TASKFIFO_DEPTH				: natural := 64;--32;
	constant RX_TASKFIFO_AWIDTH				: natural := 6;--5;	
		
	constant TX_TASKFIFO_RAM_TYPE			: string  := "BLK_RAM"; -- "DIS_RAM" or "BLK_RAM"
	constant RX_TASKFIFO_RAM_TYPE			: string  := "BLK_RAM";	-- "DIS_RAM" or "BLK_RAM"
	
--	constant RAM_WR_CYCLE			:	natural		:=	8;
--	constant RAM_RD_CYCLE			:	natural		:=	8;
--	constant RAM_RD_DELAY			:	natural		:=	9;
	
	constant ETHRX_ZCPSM_ID			:	std_logic_vector(3 downto 0)	:=	X"0";  
	constant ETHTX_ZCPSM_ID		   	:	std_logic_vector(3 downto 0)  :=  X"1";
	
	constant PORTS_ETH_TX			:	std_logic_vector(3 downto 0)	:=	X"0"; 
	constant PORTS_ETH_TX_TASK		:	std_logic_vector(3 downto 0)  :=  X"1";
--	constant PORTS_ETH_TX_TIMING	:	std_logic_vector(3 downto 0)  :=  X"2";
	
	constant PORTS_ETH_RX			:	std_logic_vector(3 downto 0)	:=	X"1";
	constant PORTS_ETH_RXDMA		:	std_logic_vector(3 downto 0)	:=	X"2";
	constant PORTS_ETH_RX_TASK		:	std_logic_vector(3 downto 0)  :=  X"3";
--	constant PORTS_ETH_RX_TIMING	:	std_logic_vector(3 downto 0)	:=  X"4";
	
	constant PORTS_DB_TX			:	std_logic_vector(3 downto 0)	:=	X"0";
	constant PORTS_DB_RX			:	std_logic_vector(3 downto 0)	:=	X"1";
	constant PORTS_DB_DEBUG			:	std_logic_vector(3 downto 0)	:=	X"2";	
	
	constant PORTS_DB_TX_TASK		:	std_logic_vector(3 downto 0)	:=	X"4"; 
	constant PORTS_DB_RX_TASK		:	std_logic_vector(3 downto 0)	:=	X"5";  
	
	constant PORTS_DEBUG_PROG		:	std_logic_vector(3 downto 0)	:=	X"0";
	
	
	constant PORT_ETH_TX_HIGHPRI_CE			:	std_logic_vector(3 downto 0)	:=	X"E";
	constant PORT_ETH_TX_HIGHPRI_REQ		:	std_logic_vector(3 downto 0)	:=	X"0";
	constant PORT_ETH_TX_HIGHPRI_ADDR_L		:	std_logic_vector(3 downto 0)	:=	X"1";
	constant PORT_ETH_TX_HIGHPRI_ADDR_H		:	std_logic_vector(3 downto 0)	:=	X"2";
	constant PORT_ETH_TX_HIGHPRI_DATA_L		:	std_logic_vector(3 downto 0)	:=	X"3";
	constant PORT_ETH_TX_HIGHPRI_DATA_H		:	std_logic_vector(3 downto 0)	:=	X"4";
	constant PORT_ETH_TX_HIGHPRI_DESMAC_0	:	std_logic_vector(3 downto 0)	:=	X"5";
	constant PORT_ETH_TX_HIGHPRI_DESMAC_1	:	std_logic_vector(3 downto 0)	:=	X"6";
	constant PORT_ETH_TX_HIGHPRI_DESMAC_2	:	std_logic_vector(3 downto 0)	:=	X"7";
	constant PORT_ETH_TX_HIGHPRI_DESMAC_3	:	std_logic_vector(3 downto 0)	:=	X"8";
	constant PORT_ETH_TX_HIGHPRI_DESMAC_4	:	std_logic_vector(3 downto 0)	:=	X"9";
	constant PORT_ETH_TX_HIGHPRI_DESMAC_5	:	std_logic_vector(3 downto 0)	:=	X"A";
	

	constant PORT_ETH_LOCAL_ID_0_REQ	:	std_logic_vector(7 downto 0)	:=	X"F8";
	constant PORT_ETH_LOCAL_ID_0_A	:	std_logic_vector(7 downto 0)	:=	X"F9";
	constant PORT_ETH_LOCAL_ID_0_B	:	std_logic_vector(7 downto 0)	:=	X"FA";
	constant PORT_ETH_LOCAL_ID_1	:	std_logic_vector(7 downto 0)	:=	X"FB";
	constant PORT_ETH_LOCAL_ID_2	:	std_logic_vector(7 downto 0)	:=	X"FC";
	constant PORT_ETH_LOCAL_ID_3	:	std_logic_vector(7 downto 0)	:=	X"FD";
	constant PORT_ETH_LOCAL_ID_4	:	std_logic_vector(7 downto 0)	:=	X"FE";
	constant PORT_ETH_LOCAL_ID_5	:	std_logic_vector(7 downto 0)	:=	X"FF";
	
	constant PORT_DB_LOCAL_ID_0_A	:	std_logic_vector(7 downto 0)	:=	X"F9";
	constant PORT_DB_LOCAL_ID_0_B	:	std_logic_vector(7 downto 0)	:=	X"FA";
	constant PORT_DB_LOCAL_ID_1		:	std_logic_vector(7 downto 0)	:=	X"FB";
	constant PORT_DB_LOCAL_ID_2		:	std_logic_vector(7 downto 0)	:=	X"FC";
	constant PORT_DB_LOCAL_ID_3		:	std_logic_vector(7 downto 0)	:=	X"FD";
	constant PORT_DB_LOCAL_ID_4		:	std_logic_vector(7 downto 0)	:=	X"FE";
	constant PORT_DB_LOCAL_ID_5		:	std_logic_vector(7 downto 0)	:=	X"FF";
	
end package;
