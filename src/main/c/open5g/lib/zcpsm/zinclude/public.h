#ifndef _PUBLIC_H
#define _PUBLIC_H

#define setbit( a, n ) (a|=1<<n)
#define clrbit( a, n ) (a&=~(1<<n))
#define testbit( a, n ) (a&(1<<n))
#define notbit( a, n ) (a^=(1<<n))

unsigned char XBYTE[1];
unsigned char XWORD[1];

#ifdef PC

void setup();
void shut_down();
int write_byte(unsigned short port, unsigned char data);
int read_byte(unsigned short port);
int write_word(unsigned short port, unsigned short data);
int read_word(unsigned short port);
#define READIO(a,b) (b=read_byte(a))
#define WRITEIO(a,b) (write_byte(a,b))
#define READWORD(a,b) (b=read_word(a))
#define WRITEWORD(a,b) (write_word(a,b))
#define INIT	setup()
#define QUIT	shut_down()

#else

#define READIO(a,b) (b=XBYTE[a])
#define WRITEIO(a,b) (XBYTE[a]=b)
#define READWORD(a,b) (b=XWORD[a])
#define WRITEWORD(a,b) (XWORD[a]=b)
#define INIT	
#define QUIT	

#endif

#endif
