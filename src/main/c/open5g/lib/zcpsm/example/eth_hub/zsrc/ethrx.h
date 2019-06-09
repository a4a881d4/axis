#ifndef _ETH_RX_H
#define _ETH_RX_H

/* Ports Definition */

#define PORT_RX_IO_ADDR				0x0010
#define PORT_RX_IO_DATA				0x0012
#define PORT_RX_QUEUE_STATUS		0x0014
#define PORT_RX_RD_BLOCK			0x001A

#define PORT_RX_DMA_START			0x0020
#define PORT_RX_DMA_LENGTH_0		0x0021
#define PORT_RX_DMA_LENGTH_1		0x0022
#define PORT_RX_DMA_RADDR_0			0x0023
#define PORT_RX_DMA_RADDR_1			0x0024
#define PORT_RX_DMA_RADDR_2			0x0025
#define PORT_RX_DMA_RADDR_3			0x0026
#define PORT_RX_DMA_WADDR_0			0x0027
#define PORT_RX_DMA_WADDR_1			0x0028
#define PORT_RX_DMA_WADDR_2			0x0029
#define PORT_RX_DMA_WADDR_3			0x002A
#define PORT_RX_DMA_RSTEP			0x002B
#define PORT_RX_DMA_WSTEP			0x002C
#define PORT_RX_DMA_BUSY			0x002D
#define PORT_RX_LAST_FRAME		0x002E

#define PORT_RX_TASK_IO_ADDR				0x0030
#define PORT_RX_TASK_IO_DATA				0x0032
#define PORT_RX_TASK_STATUS		0x0034
#define PORT_RX_TASK_WR_BLOCK		0x0038

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

unsigned char uTmpReg10;

/* Local Variables */

#define uDesMac0						uTmpReg10

#define uReadDataL					uTmpReg0
#define uReadDataH					uTmpReg1

#define uIOAddr_L						uTmpReg0
#define uIOAddr_H						uTmpReg1
#define uIOData_L						uTmpReg0
#define uIOData_H						uTmpReg1

#define uMsgTypeL						uTmpReg0

#define uRxDmaBusy					uTmpReg1
#define uRxQueueStatus			uTmpReg0
#define uRxTaskStatus       uTmpReg0
#define uRxLenEn						uTmpReg0

#define uFlag								uTmpReg2


#define uRxDmaWaddr0				uTmpReg5
#define uRxDmaWaddr1				uTmpReg6
#define uRxDmaWaddr2				uTmpReg7
#define uRxDmaWaddr3				uTmpReg8

#define uEstRxRevTime3				uTmpReg9
#define uEstRxRevTime2				uTmpReg9
#define uEstRxRevTime1				uTmpReg9
#define uEstRxRevTime0				uTmpReg9

#define uSouMac						uTmpReg9

#define uMcuStatus					uTmpRegA

#define uRxTotalLength0					uTmpRegB
#define uRxTotalLength1					uTmpRegC
#define uRxTotalLength2					uTmpRegD
#define uRxTotalLength3					uTmpRegE

#define uRxLengthL					uTmpReg3
#define uRxLengthH					uTmpReg4

#define uLocalMAC					uTmpReg1

/* when datalength = 1024 + ETHTX_HEAD_LENGTH */
#define MaxDataLenL			ETHTX_HEAD_LENGTH
#define MaxDataLenH			2	
#define MaxDataLenHx2   4

/* mcu status */
#define WrTask					1
#define RxData					0

#define RxQueueFlagAddr	ETHRX_INFO_LENGTH + 18
#define RxQueueLastFrameLen  ETHRX_INFO_LENGTH + 27

#endif
