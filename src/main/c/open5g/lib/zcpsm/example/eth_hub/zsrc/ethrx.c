#include <public.h>
#include "ethconf.h"
#include "ethrx.h"

void init();
void rxDataOnce();
void wrTaskOnce();

main()
{
	INIT;
	
	init();
		
	while(1)
	{
		rxDataOnce();
/*		wrTaskOnce();*/
	}
	
	QUIT;
}


void init()
{
	WRITEIO(PORT_RX_DMA_RADDR_0, 0x00);
	WRITEIO(PORT_RX_DMA_RADDR_1, 0x00);
	WRITEIO(PORT_RX_DMA_RADDR_2, 0x00);
	WRITEIO(PORT_RX_DMA_WADDR_0, 0x00);
	WRITEIO(PORT_RX_DMA_WADDR_1, 0x00);
	WRITEIO(PORT_RX_DMA_WADDR_2, 0x00);
	WRITEIO(PORT_RX_DMA_RSTEP, 0x01);
	WRITEIO(PORT_RX_DMA_WSTEP, 0x01);
	uMcuStatus = RxData;
}


void rxDataOnce()
{
	/* check queue status */
	READIO(PORT_RX_QUEUE_STATUS, uRxQueueStatus);
	/* if not empty */
	if ( !(testbit(uRxQueueStatus, 1)) )
	{
		/* check dst addr */
		WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 0);
		READIO(PORT_RX_IO_DATA, uDesMac0);
		READIO(PORT_LOCAL_ID_0_A, uLocalMAC);
		if ( uDesMac0 != uLocalMAC )
			goto L_RX_MAC_B;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_1, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_2, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_3, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_4, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_5, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
			
		goto L_RX_LOCAL;

L_RX_MAC_B:
		READIO(PORT_LOCAL_ID_0_B, uLocalMAC);
		if ( uDesMac0 != uLocalMAC )
			goto L_RX_BROADCAST;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_1, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_2, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_3, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_4, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		READIO(PORT_LOCAL_ID_5, uLocalMAC);
		if ( uReadDataL != uLocalMAC )
			goto L_RX_BLOCK_END;
			
		goto L_RX_LOCAL;
		
L_RX_BROADCAST:
		if ( uDesMac0 != 0xff )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		if ( uReadDataL != 0xff )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		if ( uReadDataL != 0xff )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		if ( uReadDataL != 0xff )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		if ( uReadDataL != 0xff )
			goto L_RX_BLOCK_END;
		READIO(PORT_RX_IO_DATA, uReadDataL);
		if ( uReadDataL != 0xff )
			goto L_RX_BLOCK_END;
		
L_RX_LOCAL:		
		/* check type */
		WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 12);
		READIO(PORT_RX_IO_DATA, uReadDataH);
		READIO(PORT_RX_IO_DATA, uReadDataL);
		/* 0x080A */
		if ( (uReadDataH == DATA_TYPE_0) && (uReadDataL == DATA_TYPE_1) )
		{
			/* read first frame flag */
			WRITEIO(PORT_RX_IO_ADDR, RxQueueFlagAddr);
			READIO(PORT_RX_IO_DATA, uFlag);
			/*
			if ( testbit(uFlag, 1) )
			{
				uRxLength0 = 0;
				uRxLength1 = 0;
				uRxLength2 = 0;
				uRxLength3 = 0;	
			}			
			if ( uFlag == 1 )
			{
				uRxDmaWaddr1 += MaxDataLenH;
				if ( uRxDmaWaddr1 < MaxDataLenH )
				{ 
					uRxDmaWaddr2 += 1;
					if ( uRxDmaWaddr2 < 1 )
						uRxDmaWaddr3 += 1;
				}	
			}
			else
			{
				READIO(PORT_RX_IO_DATA, uRxDmaWaddr0);
				READIO(PORT_RX_IO_DATA, uRxDmaWaddr1);
				READIO(PORT_RX_IO_DATA, uRxDmaWaddr2);
				READIO(PORT_RX_IO_DATA, uRxDmaWaddr3);				
			}		*/
			READIO(PORT_RX_IO_DATA, uRxDmaWaddr0);
			READIO(PORT_RX_IO_DATA, uRxDmaWaddr1);
			READIO(PORT_RX_IO_DATA, uRxDmaWaddr2);
			READIO(PORT_RX_IO_DATA, uRxDmaWaddr3);				
			
			WRITEIO(PORT_RX_DMA_WADDR_0, uRxDmaWaddr0);			
			WRITEIO(PORT_RX_DMA_WADDR_1, uRxDmaWaddr1);
			WRITEIO(PORT_RX_DMA_WADDR_2, uRxDmaWaddr2);
			WRITEIO(PORT_RX_DMA_WADDR_3, uRxDmaWaddr3);				
				
			/* calc length in byte */
			if ( testbit(uFlag, 0) )       /*最后一帧*/
			{ 
				WRITEIO(PORT_RX_IO_ADDR, RxQueueLastFrameLen);
				READIO(PORT_RX_IO_DATA, uRxLenEn);
				if (uRxLenEn > 0) 
				{
					uRxLengthL = uRxLenEn;
					uRxLengthH = 0;
				}
				else
				{
					WRITEIO(PORT_RX_IO_ADDR, 0);
					READIO(PORT_RX_IO_DATA, uRxLengthL);
					READIO(PORT_RX_IO_DATA, uRxLengthH);
					if ( uRxLengthL < ETHTX_HEAD_LENGTH_of_LASTFRAME )
						uRxLengthH -= 1;
					uRxLengthL -= ETHTX_HEAD_LENGTH_of_LASTFRAME; 									
				}
			}
			else
			{
				WRITEIO(PORT_RX_IO_ADDR, 0);          /*读取以太包的长度*/
				READIO(PORT_RX_IO_DATA, uRxLengthL);
				READIO(PORT_RX_IO_DATA, uRxLengthH);
				if ( uRxLengthL < ETHTX_HEAD_LENGTH )
					uRxLengthH -= 1;
				uRxLengthL -= ETHTX_HEAD_LENGTH;      /*扣除包头开销长度*/
			}       						 
			WRITEIO(PORT_RX_DMA_LENGTH_0, uRxLengthL);
			WRITEIO(PORT_RX_DMA_LENGTH_1, uRxLengthH);
			/* calc start addr in int buffer */
			WRITEIO(PORT_RX_IO_ADDR, 2); 						/*读取以太包在ethrx中u_rx_buffer的起始地址*/
			READIO(PORT_RX_IO_DATA, uReadDataL);
			READIO(PORT_RX_IO_DATA, uReadDataH);
			if ( testbit(uFlag, 0) )								/*最后一帧*/
			{
				uReadDataL += ETHTX_HEAD_LENGTH_of_LASTFRAME;
				if ( uReadDataL < ETHTX_HEAD_LENGTH_of_LASTFRAME )
					uReadDataH += 1;				
			}
			else
			{
				uReadDataL += ETHTX_HEAD_LENGTH;      /*对应数据首地址*/
				if ( uReadDataL < ETHTX_HEAD_LENGTH )
					uReadDataH += 1;
			}
			WRITEIO(PORT_RX_DMA_RADDR_0, uReadDataL);   /*kcpsm2dma，DMA读首地址*/
			WRITEIO(PORT_RX_DMA_RADDR_1, uReadDataH);
			/* calc new dma write ext ram addr 
			uRxDmaWaddr1 += MaxDataLenH;
			if ( uRxDmaWaddr1 < MaxDataLenH )
			{ 
				uRxDmaWaddr2 += 1;
				if ( uRxDmaWaddr2 < 1 )
					uRxDmaWaddr3 += 1;
			}	
			uRxLength0 += uRxLengthL;
			if ( uRxLength0 < uRxLengthL )
			{
				uRxLength1 += 1;
				if ( uRxLength1 < 1 )
				{
					uRxLength2 += 1;
					if ( uRxLength2 < 1 )
						uRxLength3 += 1;		
				}					
			}	
			uRxLength1 += uRxLengthH;
			if ( uRxLength1 < uRxLengthH )
			{
				uRxLength2 += 1;
				if ( uRxLength2 < 1 )
					uRxLength3 += 1;							
			}			

			/* wait for DMA idle, then start DMA */
			do
			{
				READIO(PORT_RX_DMA_BUSY, uRxDmaBusy);
			}
			while ( testbit(uRxDmaBusy, 0) );

			WRITEIO(PORT_RX_DMA_START, 0); /*DMA开始工作，从ethrx中u_rx_buffer中读数据，输出到Tx_DBuf*/
			
			/* if last frame */
			if ( testbit(uFlag, 0) )
				wrTaskOnce();		
						
							
		}
		/* 0x080B */
		else if ( (uReadDataH == IO_TYPE_0) && (uReadDataL == IO_TYPE_1) )
		{
			do
			{
				READIO(PORT_RX_TASK_STATUS, uRxTaskStatus);
			}
			while ( testbit(uRxTaskStatus, 0) );			
			WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 24);/* IO地址 */
			READIO(PORT_RX_IO_DATA, uIOAddr_L);
			READIO(PORT_RX_IO_DATA, uIOAddr_H);
			WRITEIO(PORT_RX_TASK_IO_ADDR, 0);								/* RX_TASK：1～2 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uIOAddr_L);			
			WRITEIO(PORT_RX_TASK_IO_DATA, uIOAddr_H);		
			READIO(PORT_RX_IO_DATA, uIOData_L);						/* IO数据 */
			READIO(PORT_RX_IO_DATA, uIOData_H);				
			WRITEIO(PORT_RX_TASK_IO_DATA, uIOData_L);      /* RX_TASK：3～4 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uIOData_H);
			WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 18);  
			READIO(PORT_RX_IO_DATA, uMsgTypeL);						/* 调试命令返回类型 */
			WRITEIO(PORT_RX_TASK_IO_DATA, 0);							 /* RX_TASK：5～7 */
			WRITEIO(PORT_RX_TASK_IO_DATA, 0);	
			WRITEIO(PORT_RX_TASK_IO_DATA, 0);									
			if ( uMsgTypeL == 0x06 )											/* RX_TASK：8 */
				WRITEIO(PORT_RX_TASK_IO_DATA, 0);			/* 写I/O */
			else if	( uMsgTypeL == 0x07 )	
				WRITEIO(PORT_RX_TASK_IO_DATA, 1);			/* 读I/O */
					
			/* write msg source mac addr */ 
			WRITEIO(PORT_RX_TASK_IO_ADDR, 12);	
			WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 6);
			READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_0 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
			READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_1 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
			READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_2 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
			READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_3 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
			READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_4 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
			READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_5 */
			WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
			WRITEIO(PORT_RX_TASK_IO_DATA, uDesMac0); /* Des Mac 0 */
			
			WRITEIO(PORT_RX_TASK_WR_BLOCK, 0);		
		}

L_RX_BLOCK_END:
		/* step by one block in rx queue */
		WRITEIO(PORT_RX_RD_BLOCK, 0);
	}
}


void wrTaskOnce()
{


		do
		{
			READIO(PORT_RX_TASK_STATUS, uRxTaskStatus);
		}
		while ( testbit(uRxTaskStatus, 0) );
		
		WRITEIO(PORT_RX_IO_ADDR, RxQueueFlagAddr + 10);
		READIO(PORT_RX_IO_DATA, uRxDmaWaddr0);
		READIO(PORT_RX_IO_DATA, uRxDmaWaddr1);
		READIO(PORT_RX_IO_DATA, uRxDmaWaddr2);
		READIO(PORT_RX_IO_DATA, uRxDmaWaddr3);
		WRITEIO(PORT_RX_TASK_IO_ADDR, 0);
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxDmaWaddr3);			
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxDmaWaddr2);
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxDmaWaddr1);
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxDmaWaddr0);
		/* write total length in word 
		uRxLength0 >>= 1;
		if ( (testbit(uRxLength1, 0)) )
			uRxLength0 += 0x80;
		uRxLength1 >>= 1;
		if ( (testbit(uRxLength2, 0)) )
			uRxLength1 += 0x80;	
		uRxLength2 >>= 1;
		if ( (testbit(uRxLength3, 0)) )
			uRxLength2 += 0x80;	
		uRxLength3 >>= 1;		*/
		READIO(PORT_RX_IO_DATA, uRxTotalLength0);
		READIO(PORT_RX_IO_DATA, uRxTotalLength1);
		READIO(PORT_RX_IO_DATA, uRxTotalLength2);
		READIO(PORT_RX_IO_DATA, uRxTotalLength3);		
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxTotalLength3);	
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxTotalLength2);
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxTotalLength1);
		WRITEIO(PORT_RX_TASK_IO_DATA, uRxTotalLength0);
		
		WRITEIO(PORT_RX_IO_ADDR, RxQueueFlagAddr + 5);
		READIO(PORT_RX_IO_DATA, uEstRxRevTime3);
		WRITEIO(PORT_RX_TASK_IO_DATA, uEstRxRevTime3);
		READIO(PORT_RX_IO_DATA, uEstRxRevTime2);			
		WRITEIO(PORT_RX_TASK_IO_DATA, uEstRxRevTime2);
		READIO(PORT_RX_IO_DATA, uEstRxRevTime1);			
		WRITEIO(PORT_RX_TASK_IO_DATA, uEstRxRevTime1);
		READIO(PORT_RX_IO_DATA, uEstRxRevTime0);			
		WRITEIO(PORT_RX_TASK_IO_DATA, uEstRxRevTime0);	
		
		WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 6);
		READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_0 */
		WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
		READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_1 */
		WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
		READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_2 */
		WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
		READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_3 */
		WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
		READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_4 */
		WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
		READIO(PORT_RX_IO_DATA, uSouMac); /* Addr_5 */
		WRITEIO(PORT_RX_TASK_IO_DATA, uSouMac);
		WRITEIO(PORT_RX_TASK_IO_DATA, uDesMac0); /* Des Mac 0 */

		WRITEIO(PORT_RX_LAST_FRAME, 0);	
			
		uMcuStatus = RxData;

}
