#include <public.h>
#define PORT_LED				0x01

unsigned char uTmpReg0;
unsigned char uTmpReg1;

unsigned short uhDelay;
unsigned short uhTemp;


void Delay()
{
	for( uhTemp = 0; uhTemp < uhDelay; uhTemp ++ )
	{
		uTmpReg0++;
	}
}
main()
{
	INIT;
	
	uTmpReg1 = 0;
	
	while(1)
	{		
		uhDelay = 0xffff;
		Delay();
		WRITEIO( PORT_LED, uTmpReg1 );
		uTmpReg1++;
	}
	
	QUIT;
}
