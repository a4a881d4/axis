#include <public.h>
#include "ethconf.h"
#include "ethtx.h"

void init();
void rdTaskOnce();
void txDataOnce();
void HighPriorityTx(  );

main()
{
	INIT;

	init();
			
	while(1)
	{
/*		HighPriorityTx(  ); */
		rdTaskOnce();
		txDataOnce();
	}
	
	QUIT;
}


void init()
{
	uMcuStatus = ReadTask;
}

void rdTaskOnce()
{	
	if ( uMcuStatus == ReadTask )
	{
		/* check tx queue status */
		READIO(PORT_TX_QUEUE_STATUS, uTxQueueStatus);
		/* if empty */
		if ( testbit(uTxQueueStatus, 1) )
		{			
			/* check task status */
			READIO(PORT_TX_TASK_STATUS, uTxTaskStatus);	
			/* if not empty */
			if ( !(testbit(uTxTaskStatus, 1)) )
			{
				WRITEIO(PORT_TX_TASK_IO_ADDR, 0);
				/* read tx dma start addr = 0x00000040 */
				READIO(PORT_TX_TASK_IO_DATA, uTxDmaRaddr3);
				READIO(PORT_TX_TASK_IO_DATA, uTxDmaRaddr2);
				READIO(PORT_TX_TASK_IO_DATA, uTxDmaRaddr1);
				READIO(PORT_TX_TASK_IO_DATA, uTxDmaRaddr0);
				/* read rx dma start addr = 0xFE0000F1 */
				READIO(PORT_TX_TASK_IO_DATA, uRxDmaWaddr3);
				READIO(PORT_TX_TASK_IO_DATA, uRxDmaWaddr2);
				READIO(PORT_TX_TASK_IO_DATA, uRxDmaWaddr1);
				READIO(PORT_TX_TASK_IO_DATA, uRxDmaWaddr0);
				/* read length = 0x00000040 */
				READIO(PORT_TX_TASK_IO_DATA, uTxLength3);
				READIO(PORT_TX_TASK_IO_DATA, uTxLength2);
				READIO(PORT_TX_TASK_IO_DATA, uTxLength1);		
				READIO(PORT_TX_TASK_IO_DATA, uTxLength0);
/*				
				if ( !((uTxLength3 == 0) && (uTxLength2 == 0) && (uTxLength1 == 0) && (uTxLength0 <= 1)) )
				{
					WRITEIO(PORT_TX_IO_ADDR, TxQueueFlagAddr + 5);
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime3);
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime3);	
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime2);
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime2);
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime1);
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime1);
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime0);	
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime0);
				}							
				/* set flag to MCU trans data status */ 	 
				uMcuStatus = TransData; 
				/*uFragmentType = Fragment;*/
				/* first frame but not last frame */
				uFlag = 2;  
			}	
		}			
	}	
}

void txDataOnce()
{
	if ( uMcuStatus == TransData ) 
	{
		/* check tx queue status */
		READIO(PORT_TX_QUEUE_STATUS, uTxQueueStatus);
		/* if empty */
		if ( testbit(uTxQueueStatus, 1) )
		{
			/* task type = debug */
			if ( (uTxLength3 == 0) && (uTxLength2 == 0) && (uTxLength1 == 0) && (uTxLength0 <= 1) && (testbit(uFlag, 1)) )	
			{
				WRITEIO(PORT_TX_IO_ADDR, 0);
				/* write head length */
				WRITEIO(PORT_TX_IO_DATA, 60);			
				/* write data length (data + head)*/	
				WRITEIO(PORT_TX_IO_DATA, 60);
				WRITEIO(PORT_TX_IO_DATA, 0);
				/* write dma start addr */
				WRITEIO(PORT_TX_IO_DATA, 0);
				WRITEIO(PORT_TX_IO_DATA, 0);
				WRITEIO(PORT_TX_IO_DATA, 0);	
				WRITEIO(PORT_TX_IO_DATA, 0);								
				/* write dma step */
				WRITEIO(PORT_TX_IO_DATA, 0);
				/* write dst mac */
				WRITEIO(PORT_TX_TASK_IO_ADDR, 12);
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_0);                     
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_1);                     
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_2);                     
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_3);                     
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_4);                     
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_5);                     
				/* write src mac */                                      
				/* READIO(PORT_LOCAL_ID_0, uLocalMAC); */		/*Addr_0*/   
				                                                         
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_0);	                 
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_1);	                 
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_2);	                 
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_3);	                 
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_4);	                 
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_5);	                 
				
				/* wrtie pro type */
				WRITEIO(PORT_TX_IO_DATA, DEBUG_TYPE_0);
				WRITEIO(PORT_TX_IO_DATA, DEBUG_TYPE_1);	
				WRITEIO(PORT_TX_IO_ADDR, ETHTX_INFO_LENGTH + 18);
				/* debug type = write IO */
				if ( uTxLength0 == 0 )
					WRITEIO(PORT_TX_IO_DATA, 0x06);
				/* debug type = read IO */	
				else
					WRITEIO(PORT_TX_IO_DATA, 0x07);
				WRITEIO(PORT_TX_IO_DATA, 0x04);	
				WRITEIO(PORT_TX_IO_ADDR, ETHTX_INFO_LENGTH + 24);	
				/* write debug addr */
				WRITEIO(PORT_TX_IO_DATA, uIOAddr_L);
				WRITEIO(PORT_TX_IO_DATA, uIOAddr_H);
				/* write debug data */
				WRITEIO(PORT_TX_IO_DATA, uIOData_L);													
				WRITEIO(PORT_TX_IO_DATA, uIOData_H);	
				WRITEIO(PORT_TX_WR_BLOCK, 0);	
				WRITEIO(PORT_TX_TASK_RD_BLOCK, 0);
				uMcuStatus = ReadTask;						
			}
			else  /* task type = data */
			{			
				/* if dma tx length <= 1024 bytes */
				if ( (uTxLength3 == 0) && (uTxLength2 == 0) )
				{
					if ( uTxLength1 < MaxDataLenH ) 
						/*uFragmentType = NoFragment;*/
						setbit(uFlag, 0);
					else if ( (uTxLength1 == MaxDataLenH )&&(uTxLength0 == 0) )
						/*uFragmentType = NoFragment;*/ 
						setbit(uFlag, 0);					
				}
										
				WRITEIO(PORT_TX_IO_ADDR, 0);

				/* decide tx data length per frame */
				/*if ( uFragmentType == NoFragment )*/
				if ( testbit(uFlag, 0) )
				{
					/* write head length */
					WRITEIO(PORT_TX_IO_DATA, ETHTX_HEAD_LENGTH_of_LASTFRAME);	
									
					if ( (uTxLength3 == 0) && (uTxLength2 == 0) && (uTxLength1 == 0) && (uTxLength0 <= 11) )
					{
						uDataLengthH = 0;
						uDataLengthL = 60;	
					}
					else
					{
						uDataLengthH = uTxLength1;
						uDataLengthH <<= 1;
						if ( testbit(uTxLength0, 7) )
							uDataLengthH += 1;
						uDataLengthL = uTxLength0;
						uDataLengthL <<= 1;	
						
						uRxDmaWaddr0 = uDataLengthL;/* luo: length of last frame : 1 byte -> 2 byte */
						uRxDmaWaddr1 = uDataLengthH;/* Rx DMA Start Addr 暂时不使用，故暂时借用 */
						
						uDataLengthL += ETHTX_HEAD_LENGTH_of_LASTFRAME;
						if ( uDataLengthL < ETHTX_HEAD_LENGTH_of_LASTFRAME )	
							uDataLengthH += 1;	
					}
					/* write data length (data + head) = 0x0057 */	
					WRITEIO(PORT_TX_IO_DATA, uDataLengthL);
					WRITEIO(PORT_TX_IO_DATA, uDataLengthH);		

					WRITEIO(PORT_TX_IO_ADDR, TxQueueFlagAddr + 5);
					WRITEIO(PORT_TX_TASK_IO_ADDR, 12);
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime3);
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime3);	
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime2);
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime2);
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime1);
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime1);
					READIO(PORT_TX_TASK_IO_DATA, uEstRxRevTime0);	
					WRITEIO(PORT_TX_IO_DATA, uEstRxRevTime0);
										
					if ( (uTxLength3 == 0) && (uTxLength2 == 0) && (uTxLength1 == 0) && (uTxLength0 <= 11) )
					{
						uTxLength0Tmp = uTxLength0;
						uTxLength0Tmp <<= 1;
						WRITEIO(PORT_TX_IO_DATA, uTxLength0Tmp);
					}	
					else
					{	
/*						WRITEIO(PORT_TX_IO_DATA, 0);		*/
						WRITEIO(PORT_TX_IO_DATA, uRxDmaWaddr0);
						WRITEIO(PORT_TX_IO_DATA, uRxDmaWaddr1);
					}
					
/*					WRITEIO(PORT_TX_TASK_IO_ADDR, 7);	
					READIO(PORT_TX_TASK_IO_DATA, uRxIniWaddr0);
					WRITEIO(PORT_TX_IO_DATA, uRxIniWaddr0);	*/
					WRITEIO(PORT_TX_TASK_IO_ADDR, 6);		
					READIO(PORT_TX_TASK_IO_DATA, uRxIniWaddr1);
					WRITEIO(PORT_TX_IO_DATA, uRxIniWaddr1);
					WRITEIO(PORT_TX_TASK_IO_ADDR, 5);	
					READIO(PORT_TX_TASK_IO_DATA, uRxIniWaddr2);
					WRITEIO(PORT_TX_IO_DATA, uRxIniWaddr2);
					WRITEIO(PORT_TX_TASK_IO_ADDR, 4);																			
					READIO(PORT_TX_TASK_IO_DATA, uRxIniWaddr3);
					WRITEIO(PORT_TX_IO_DATA, uRxIniWaddr3);		

					WRITEIO(PORT_TX_TASK_IO_ADDR, 11);		
					READIO(PORT_TX_TASK_IO_DATA, uTxTotalLength0);
					WRITEIO(PORT_TX_IO_DATA, uTxTotalLength0);
					WRITEIO(PORT_TX_TASK_IO_ADDR, 10);
					READIO(PORT_TX_TASK_IO_DATA, uTxTotalLength1);
					WRITEIO(PORT_TX_IO_DATA, uTxTotalLength1);
					WRITEIO(PORT_TX_TASK_IO_ADDR, 9);
					READIO(PORT_TX_TASK_IO_DATA, uTxTotalLength2);
					WRITEIO(PORT_TX_IO_DATA, uTxTotalLength2);	
					WRITEIO(PORT_TX_TASK_IO_ADDR, 8);										
					READIO(PORT_TX_TASK_IO_DATA, uTxTotalLength3);
					WRITEIO(PORT_TX_IO_DATA, uTxTotalLength3);																																														
				}
				else
				{ 
					/* write head length */
					WRITEIO(PORT_TX_IO_DATA, ETHTX_HEAD_LENGTH);					
					uDataLengthL = MaxDataLenL; 
					uDataLengthH = MaxDataLenHx2; 
					/* write data length (data + head) = 0x0057 */	
					WRITEIO(PORT_TX_IO_DATA, uDataLengthL);
					WRITEIO(PORT_TX_IO_DATA, uDataLengthH);					
				}						
				WRITEIO(PORT_TX_IO_ADDR, 3);
				/* write dma start addr */
				WRITEIO(PORT_TX_IO_DATA, uTxDmaRaddr0); /*无效 */
				WRITEIO(PORT_TX_IO_DATA, uTxDmaRaddr1);
				WRITEIO(PORT_TX_IO_DATA, uTxDmaRaddr2);			
				WRITEIO(PORT_TX_IO_DATA, uTxDmaRaddr3);	
				/* write dma step */
				WRITEIO(PORT_TX_IO_DATA, 1);		
				/* write dst mac */
				WRITEIO(PORT_TX_TASK_IO_ADDR, 16);
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_0);
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_1);
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_2);
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_3);
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_4);
				WRITEIO(PORT_TX_IO_DATA, DST_MAC_5);
				/* write src mac */
				/* READIO(PORT_LOCAL_ID_0, uLocalMAC); */		/*Addr_0*/

				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_0);	
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_1);	
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_2);	
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_3);	
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_4);	
				WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_5);	
				/* wrtie pro type */
				WRITEIO(PORT_TX_IO_DATA, DATA_TYPE_0);
				WRITEIO(PORT_TX_IO_DATA, DATA_TYPE_1);			
				/* write Last&FirstFrame flag */
				WRITEIO(PORT_TX_IO_ADDR, TxQueueFlagAddr);
				WRITEIO(PORT_TX_IO_DATA, uFlag);

				WRITEIO(PORT_TX_IO_DATA, uRxDmaWaddr0);	
				WRITEIO(PORT_TX_IO_DATA, uRxDmaWaddr1);	
				WRITEIO(PORT_TX_IO_DATA, uRxDmaWaddr2);
				WRITEIO(PORT_TX_IO_DATA, uRxDmaWaddr3);
					
				/* start tx */				
				WRITEIO(PORT_TX_WR_BLOCK, 0);	
				
				/* not first frame */
				clrbit(uFlag, 1);
				/* if not LastFrame */
				/*if ( uFragmentType == Fragment )*/
				if ( !testbit(uFlag, 0) )
				{
					/* calc new tx dma read addr */	
					uTxDmaRaddr1 += MaxDataLenH;
					if ( uTxDmaRaddr1 < MaxDataLenH )
					{ 
						uTxDmaRaddr2 += 1;
						if ( uTxDmaRaddr2 < 1 )
							uTxDmaRaddr3 += 1;
					}
					
					uRxDmaWaddr1 += MaxDataLenH;
					if ( uRxDmaWaddr1 < MaxDataLenH )
					{ 
						uRxDmaWaddr2 += 1;
						if ( uRxDmaWaddr2 < 1 )
							uRxDmaWaddr3 += 1;
					}					
									
					/* ? length word ? */	
					if ( uTxLength1 < MaxDataLenH )
					{ 
						if ( uTxLength2 < 1 )
							uTxLength3 -= 1;
						uTxLength2 -= 1;
					}	
					uTxLength1 -= MaxDataLenH;
					/* set status to task reading */
					uMcuStatus = TransData;
				}	
				/* if last frame */
				else
					/* set status to data trans */
				{
					WRITEIO(PORT_TX_TASK_RD_BLOCK, 0);
					uMcuStatus = ReadTask;
				} 	
			}		 																								
		}							
	} 
}               