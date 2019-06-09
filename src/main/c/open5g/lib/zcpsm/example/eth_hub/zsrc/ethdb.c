#include <public.h>
#include "ethconf.h"
#include "ethdb.h"

main()
{
	INIT;
	
	while(1)
	{		
		READIO(PORT_RX_QUEUE_STATUS, uRxQueueStatus);
		/* check rx queue status */
		if ( testbit(uRxQueueStatus, 1) == 0 )
		{
			/* check dst addr */
			WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 0);
			READIO(PORT_RX_IO_DATA, uDesMac0);
			if ( uDesMac0 != LOCAL_MAC_0 )
				goto L_RX_MAC_B;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != LOCAL_MAC_1 )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != LOCAL_MAC_2 )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != LOCAL_MAC_3 )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != LOCAL_MAC_4 )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != LOCAL_MAC_5 )
				goto L_RX_BLOCK_END;
			
			uBroadCast = 0;	
			goto L_RX_LOCAL;

	L_RX_MAC_B:
			READIO(PORT_LOCAL_ID_0_B, uLocalMAC);
			if ( uDesMac0 != uLocalMAC )
				goto L_RX_BROADCAST;
			READIO(PORT_RX_IO_DATA, uMac);
			READIO(PORT_LOCAL_ID_1, uLocalMAC);
			if ( uMac != uLocalMAC )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			READIO(PORT_LOCAL_ID_2, uLocalMAC);
			if ( uMac != uLocalMAC )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			READIO(PORT_LOCAL_ID_3, uLocalMAC);
			if ( uMac != uLocalMAC )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			READIO(PORT_LOCAL_ID_4, uLocalMAC);
			if ( uMac != uLocalMAC )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			READIO(PORT_LOCAL_ID_5, uLocalMAC);
			if ( uMac != uLocalMAC )
				goto L_RX_BLOCK_END;
			
			uBroadCast = 0;	
			goto L_RX_LOCAL;
			
	L_RX_BROADCAST:
			if ( uDesMac0 != 0xff )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != 0xff )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != 0xff )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != 0xff )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != 0xff )
				goto L_RX_BLOCK_END;
			READIO(PORT_RX_IO_DATA, uMac);
			if ( uMac != 0xff )
				goto L_RX_BLOCK_END;
			
L_RX_LOCAL:
			/* check type */
			WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 12);
			READIO(PORT_RX_IO_DATA, uProTypeL);
			READIO(PORT_RX_IO_DATA, uProTypeH);
			if ( (uProTypeL == DEBUG_TYPE_0) && (uProTypeH == DEBUG_TYPE_1) ) /* 0x080f */
			{
				/* new */
				WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 18);
				/* check message type */
				READIO(PORT_RX_IO_DATA, uMsgTypeL);
				READIO(PORT_RX_IO_DATA, uMsgTypeH);

				if ( uMsgTypeH == 0x04 )
				{
					READIO(PORT_RX_IO_DATA, uMsgSNL);
					READIO(PORT_RX_IO_DATA, uMsgSNH);					
					
					/* proc addr */
					WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 24);
					READIO(PORT_RX_IO_DATA, uCmdAddrL);
					READIO(PORT_RX_IO_DATA, uCmdAddrH);
					WRITEIO(PORT_DEBUG_ADDR_L, uCmdAddrL);
					WRITEIO(PORT_DEBUG_ADDR_H, uCmdAddrH);

					/* IO */
					if ( uMsgTypeL == 0x06 )
					{
						READIO(PORT_RX_IO_DATA, uCmdDataL);
						READIO(PORT_RX_IO_DATA, uCmdDataH);
						WRITEIO(PORT_DEBUG_DATA_L, uCmdDataL);
						WRITEIO(PORT_DEBUG_DATA_H, uCmdDataH);
					}
					else if ( ( uMsgTypeL == 0x07 ) || ( uMsgTypeL == 0x08 ) )
					{
						if ( uMsgTypeL == 0x07 )
						{
							READIO(PORT_DEBUG_DATA_L, uCmdDataL);
							READIO(PORT_DEBUG_DATA_H, uCmdDataH);
						}
						else
						{
							READIO(PORT_RX_IO_DATA, uCmdDataL);
							READIO(PORT_RX_IO_DATA, uCmdDataH);
							WRITEIO(PORT_DEBUG_DATA_L, uCmdDataL);
							WRITEIO(PORT_DEBUG_DATA_H, uCmdDataH);
						}
						/* send return msg */
						/* gen tx head */
						WRITEIO(PORT_TX_IO_ADDR, 0);
						/* head length = 22 */
						WRITEIO(PORT_TX_IO_DATA, 60);
						/* data length = 60 */
						WRITEIO(PORT_TX_IO_DATA, 60);
						WRITEIO(PORT_TX_IO_DATA, 0);
						/* dma start addr = 0 */
						WRITEIO(PORT_TX_IO_DATA, 0);
						WRITEIO(PORT_TX_IO_DATA, 0);
						WRITEIO(PORT_TX_IO_DATA, 0);
						WRITEIO(PORT_TX_IO_DATA, 0);					
						/* dma_step = 0 */
						WRITEIO(PORT_TX_IO_DATA, 0);												
						/* Tx Dst MAC = Rx Src MAC */
						WRITEIO(PORT_RX_IO_ADDR, ETHRX_INFO_LENGTH + 6);
						READIO(PORT_RX_IO_DATA, uMac);
						WRITEIO(PORT_TX_IO_DATA, uMac);
						READIO(PORT_RX_IO_DATA, uMac);
						WRITEIO(PORT_TX_IO_DATA, uMac);
						READIO(PORT_RX_IO_DATA, uMac);
						WRITEIO(PORT_TX_IO_DATA, uMac);
						READIO(PORT_RX_IO_DATA, uMac);
						WRITEIO(PORT_TX_IO_DATA, uMac);
						READIO(PORT_RX_IO_DATA, uMac);
						WRITEIO(PORT_TX_IO_DATA, uMac);
						READIO(PORT_RX_IO_DATA, uMac);
						WRITEIO(PORT_TX_IO_DATA, uMac);
						/* Tx Src Addr = Local Mac */

						WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_0);  /* Addr_0 */
						WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_1);
						WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_2);
						WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_3);
						WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_4);
						WRITEIO(PORT_TX_IO_DATA, LOCAL_MAC_5);
						/* frame type = debug */
						WRITEIO(PORT_TX_IO_DATA, IO_TYPE_0);
						WRITEIO(PORT_TX_IO_DATA, IO_TYPE_1);
						WRITEIO(PORT_TX_IO_ADDR, ETHTX_INFO_LENGTH + 18);
						/* message type */
						WRITEIO(PORT_TX_IO_DATA, uMsgTypeL);
						WRITEIO(PORT_TX_IO_DATA, 0x84);
						/* message sn */
						WRITEIO(PORT_TX_IO_DATA, uMsgSNL);
						WRITEIO(PORT_TX_IO_DATA, uMsgSNH);
						/* message len */
						WRITEIO(PORT_TX_IO_DATA, 4);
						WRITEIO(PORT_TX_IO_DATA, 0);
						/* message content */
						WRITEIO(PORT_TX_IO_DATA, uCmdAddrL);
						WRITEIO(PORT_TX_IO_DATA, uCmdAddrH);						
						WRITEIO(PORT_TX_IO_DATA, uCmdDataL);
						WRITEIO(PORT_TX_IO_DATA, uCmdDataH);
						/* complete one tx block */
						WRITEIO(PORT_TX_WR_BLOCK, 0);							
					}
				}
			}
			/* step by one rx block*/
L_RX_BLOCK_END:
			WRITEIO(PORT_RX_RD_BLOCK, 0);
		}
	}
	
	QUIT;
}
