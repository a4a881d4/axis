#ifndef _ETHDB_H
#define _ETHDB_H

/* Ports Definition */
#define PORT_TX_IO_ADDR				0x0000
#define PORT_TX_IO_DATA				0x0002
#define PORT_TX_QUEUE_STATUS		0x0004
#define PORT_TX_WR_BLOCK			0x0008

#define PORT_RX_IO_ADDR				0x0010
#define PORT_RX_IO_DATA				0x0012
#define PORT_RX_QUEUE_STATUS		0x0014
#define PORT_RX_RD_BLOCK			0x001A

#define	PORT_DEBUG_ADDR_L			0x0020
#define	PORT_DEBUG_ADDR_H			0x0021
#define	PORT_DEBUG_DATA_L			0x0022
#define	PORT_DEBUG_DATA_H			0x0023

#define PORT_RX_TASK_IO_ADDR	0x0050
#define PORT_RX_TASK_IO_DATA	0x0052
#define PORT_RX_TASK_STATUS		0x0054
#define PORT_RX_TASK_RD_BLOCK	0x005A

#define PORT_TX_TASK_IO_ADDR	0x0040
#define PORT_TX_TASK_IO_DATA	0x0042
#define PORT_TX_TASK_STATUS		0x0044
#define PORT_TX_TASK_WR_BLOCK	0x0048


#define PORT_LOCAL_ID_0_A			0x00F9
#define PORT_LOCAL_ID_0_B			0x00FA
#define PORT_LOCAL_ID_1				0x00FB
#define PORT_LOCAL_ID_2				0x00FC
#define PORT_LOCAL_ID_3				0x00FD
#define PORT_LOCAL_ID_4				0x00FE
#define PORT_LOCAL_ID_5				0x00FF

#define PORT_PARA_CE					0x10
#define PORT_PARA_DESMAC_0		0xB2
#define PORT_PARA_SRCMACADDR_0	0xAB
#define PORT_PARA_SRCMACADDR_1	0xAC
#define PORT_PARA_SRCMACADDR_2	0xAD
#define PORT_PARA_SRCMACADDR_3	0xAE
#define PORT_PARA_SRCMACADDR_4	0xAF
#define PORT_PARA_SRCMACADDR_5	0xB0

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

unsigned char uTmpReg10;
unsigned char uTmpReg11;
unsigned char uTmpReg12;
unsigned char uTmpReg13;
unsigned char uTmpReg14;
unsigned char uTmpReg15;
unsigned char uTmpReg16;

unsigned char uTmpReg17;

/* Local Variables */

#define uDesMac0						uTmpReg10
#define uSrcMac0						uTmpReg11
#define uSrcMac1						uTmpReg12
#define uSrcMac2						uTmpReg13
#define uSrcMac3						uTmpReg14
#define uSrcMac4						uTmpReg15
#define uSrcMac5						uTmpReg16

#define uBroadCast					uTmpReg17

#define uProTypeL					uTmpReg0
#define uProTypeH					uTmpReg1
#define uMsgTypeL					uTmpReg2
#define uMsgTypeH					uTmpReg3

#define uMsgSNL						uTmpReg4
#define uMsgSNH						uTmpReg5

#define	uCmdAddrL					uTmpReg6
#define	uCmdAddrH					uTmpReg7
#define	uCmdDataL					uTmpReg8
#define	uCmdDataH					uTmpReg9

#define uTxRamRdAddr0			uTmpReg6
#define uTxRamRdAddr1			uTmpReg7
#define uTxRamRdAddr2			uTmpReg8
#define uTxRamRdAddr3			uTmpReg9

#define uRxRamWrAddr0			uTmpReg6
#define uRxRamWrAddr1			uTmpReg7
#define uRxRamWrAddr2			uTmpReg8
#define uRxRamWrAddr3			uTmpReg9

#define uTxLength0        uTmpReg6
#define uTxLength1        uTmpReg7
#define uTxLength2        uTmpReg8
#define uTxLength3        uTmpReg9

#define uEstRxRevTime3				uTmpReg6
#define uEstRxRevTime2				uTmpReg7
#define uEstRxRevTime1				uTmpReg8
#define uEstRxRevTime0				uTmpReg9

#define uDstMac								uTmpReg9							
#define uSouMac								uTmpReg9	

#define uRxQueueStatus				uTmpReg0
#define	uTxQueueStatus				uTmpReg0
#define uTxTaskStatus		uTmpReg0
#define uRxTaskStatus		uTmpReg0
#define uMac						uTmpReg0
#define uLocalMAC					uTmpReg1

#endif
