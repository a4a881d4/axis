#ifndef _ETH_TX_H
#define _ETH_TX_H


/* Ports Definition */
#define PORT_TX_IO_ADDR				0x0000
#define PORT_TX_IO_DATA				0x0002
#define PORT_TX_QUEUE_STATUS		0x0004
#define PORT_TX_WR_BLOCK			0x0008
#define PORT_TX_LAST_FRAME		0x0006

#define PORT_TX_TASK_IO_ADDR				0x0010
#define PORT_TX_TASK_IO_DATA				0x0012
#define PORT_TX_TASK_STATUS		0x0014
#define PORT_TX_TASK_RD_BLOCK			0x001A

#define PORT_TX_IO_LOCALTIME_0  0x0020
#define PORT_TX_IO_LOCALTIME_1  0x0021
#define PORT_TX_IO_LOCALTIME_2  0x0022
#define PORT_TX_IO_LOCALTIME_3  0x0023

#define PORT_HIGHPRI_REQ			0x00E0
#define PORT_HIGHPRI_ADDR_L		0x00E1
#define PORT_HIGHPRI_ADDR_H		0x00E2
#define PORT_HIGHPRI_DATA_L		0x00E3
#define PORT_HIGHPRI_DATA_H		0x00E4
#define PORT_HIGHPRI_DESMAC_0	0x00E5
#define PORT_HIGHPRI_DESMAC_1	0x00E6
#define PORT_HIGHPRI_DESMAC_2	0x00E7
#define PORT_HIGHPRI_DESMAC_3	0x00E8
#define PORT_HIGHPRI_DESMAC_4	0x00E9
#define PORT_HIGHPRI_DESMAC_5	0x00EA

#define PORT_LOCAL_ID_0_REQ		0x00F8
#define PORT_LOCAL_ID_0_A			0x00F9
#define PORT_LOCAL_ID_0_B			0x00FA
#define PORT_LOCAL_ID_1				0x00FB
#define PORT_LOCAL_ID_2				0x00FC
#define PORT_LOCAL_ID_3				0x00FD
#define PORT_LOCAL_ID_4				0x00FE
#define PORT_LOCAL_ID_5				0x00FF

/* Temp Registers */
unsigned char uTmpReg0;
unsigned char uTmpReg1;
unsigned char uTmpReg2;
unsigned char uTmpReg3;
unsigned char uTmpReg4;
unsigned char uTmpReg5;
unsigned char uTmpReg6;
unsigned char uTmpReg7;
unsigned char uTmpReg8;
unsigned char uTmpReg9;
unsigned char uTmpRegA;
unsigned char uTmpRegB;
unsigned char uTmpRegC;
unsigned char uTmpRegD;
unsigned char uTmpRegE;
unsigned char uTmpRegF;

/* Local Variables */
#define uReadDataL					uTmpReg0
#define uReadDataH					uTmpReg1

#define uDataLengthL				uTmpReg0
#define uDataLengthH				uTmpReg1

#define uLocalMAC						uTmpReg0


#define uTxDmaRaddr0				uTmpReg2
#define uTxDmaRaddr1				uTmpReg3
#define uTxDmaRaddr2				uTmpReg4
#define uTxDmaRaddr3				uTmpReg5

#define uIOAddr_L				uTmpReg5
#define uIOAddr_H				uTmpReg4
#define uIOData_L				uTmpReg3
#define uIOData_H				uTmpReg2

#define uRxDmaWaddr0				uTmpReg6
#define uRxDmaWaddr1				uTmpReg7
#define uRxDmaWaddr2				uTmpReg8
#define uRxDmaWaddr3				uTmpReg9

#define uRxIniWaddr0				uTmpReg0
#define uRxIniWaddr1				uTmpReg0
#define uRxIniWaddr2				uTmpReg0
#define uRxIniWaddr3				uTmpReg0

#define uEstRxRevTime3				uTmpReg0
#define uEstRxRevTime2				uTmpReg0
#define uEstRxRevTime1				uTmpReg0
#define uEstRxRevTime0				uTmpReg0

#define uDstMac						uTmpReg0

#define uTxLength0          uTmpRegA
#define uTxLength1          uTmpRegB
#define uTxLength2          uTmpRegC
#define uTxLength3          uTmpRegD

#define uTxTotalLength0          uTmpReg0
#define uTxTotalLength1          uTmpReg0
#define uTxTotalLength2          uTmpReg0
#define uTxTotalLength3          uTmpReg0

#define uTxTaskStatus				uTmpReg0		/* sF */
#define uTxQueueStatus			uTmpReg0
#define uTxLength0Tmp          uTmpReg0
/*#define uFragmentType				uTmpReg6*/

#define uFlag								uTmpRegE	

/*  uFlag = 00000 FirstFrame_flag LastFrame_flag */ 
	
#define uFlagTmp						uTmpReg0

#define uMcuStatus					uTmpReg1	

#define uHighPriReq					uTmpRegD			

/* Fragment Ctrl */
#define NoFragment				1
#define Fragment          0	

#define FirstFrame				1
#define NotFirstFrame  		0
	
/* MCU status */
#define ReadTask					1
#define TransData					0		

/* when datalength = 1024 + ETHTX_HEAD_LENGTH */
#define MaxDataLenL			ETHTX_HEAD_LENGTH
#define MaxDataLenH			2	
#define MaxDataLenHx2   4	

#define TxQueueFlagAddr	ETHTX_INFO_LENGTH + 18	

#endif


/* HEADLENGTH = DSTADDR (6) + SOUADDR (6) + PROTYPE (2) + RXDMAWADDR (4) + FLAG (1) + TIME (4) = 23 BYTES */