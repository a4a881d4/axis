#include"zas.h"
#include<stdio.h>
#include<string.h>
#undef DEBUG
int codeline;
extern int yydebug;

int isDirect;
int twoArgOp;
int shiftOp;
int opGrp;
int jumpGrp;
int jumpFlag;
int arg1,arg2;
int code[4096];
int srcline[4096];
int ins;

label_buf memlabel;
label_buf needlabel;

extern char *yytext;
extern FILE *yyin;
int lineno;
char initstr[4][16][65];
void genLog( char *psmFile );
char strBitString[256];
char *genBitString(int x,int l);
void genCoeFile();

processLabel()
{
	int i,j,k;
#ifdef DEBUG
	for( i=0;i<needlabel.length;i++ )
	{
		printf("need: %s at %d \n",needlabel.label[i].name,needlabel.label[i].pos);
	}
	for( i=0;i<memlabel.length;i++ )
	{
		printf("mem: %s at %d \n",memlabel.label[i].name,memlabel.label[i].pos);
	}
#endif
	for( i=0;i<needlabel.length;i++ )
	{
		for( j=0;j<memlabel.length;j++ )
		{
			if( strcmp(needlabel.label[i].name,memlabel.label[j].name)==0 )
			{
				code[needlabel.label[i].pos] |= memlabel.label[j].pos&0x3ff;
				code[needlabel.label[i].pos] |= ((memlabel.label[j].pos&0xc00)>>10)<<16;
			}
		}
	}
}
FILE *fp;
void genHex()
{
	int i;
	for( i=0;i<256;i++ )
	{
		fprintf(fp,"%04X\n",code[i]);
	}
	fprintf(fp,"\n");
	
}
void genVhdlRom(char *romName)
{
	int i;
	fprintf(fp,"library IEEE;\n");
	fprintf(fp,"use IEEE.STD_LOGIC_1164.ALL;\n");
	fprintf(fp,"ENTITY %s IS\n",romName);
	fprintf(fp,"\tport (");
	fprintf(fp,"\t\taddrb: IN std_logic_VECTOR(11 downto 0);\n");
	fprintf(fp,"\t\tclkb: IN std_logic;\n");
	fprintf(fp,"\t\tdob: OUT std_logic_VECTOR(17 downto 0)	:= (others => '0')\n");
	fprintf(fp,"\t);\n");
	fprintf(fp,"end %s;\n",romName);

	fprintf(fp,"architecture behavior of %s is\n",romName);
	fprintf(fp,"signal addr : std_logic_vector(11 downto 0):=(others=>'0');\n");
	fprintf(fp,"begin\n");
	fprintf(fp,"\taddr<=addrb;\n");
	fprintf(fp,"process(clkb)\n");
	fprintf(fp,"begin\n");
	fprintf(fp,"\tif clkb'event and clkb='1' then\n");
	fprintf(fp,"\t\tcase addr is\n");
	for( i=0;i<codeline;i++ ) {
		fprintf(fp,"\t\t\twhen \"%s\" => ",genBitString(i,12));
		fprintf(fp,"dob<=\"%s\";\n",genBitString(code[i],18));
	}
	fprintf(fp,"\t\t\twhen others => dob<=\"%s\";\n",genBitString(0,18));
	fprintf(fp,"\t\tend case;\n");
	fprintf(fp,"\tend if;\n");
	fprintf(fp,"end process;\n");
	fprintf(fp,"end behavior;\n");
}
void genInitStr( int RamNum )
{
	int pos;
	int ram;
	int i,j,count;
	char c;
	for( i=0;i<4;i++ )
		for( j=0;j<4;j++ )
			initstr[i][j][64]='\0';
	count=0;
	for( j=0;j<16;j++ )
	if( RamNum!=2 )
	{
		for( pos=63;pos>=0;pos-- )
			for( ram=0;ram<RamNum;ram++ )
			{
				i=code[count/4]>>(count%4)*4;
				i&=0xf;
				c=(char)i+'0';
				if( c>'9' )
					c=c+'\7';
				initstr[ram][j][pos]=c;
				count++;
			}
	}
	else
	{
		
		for( pos=63;pos>=0;pos-=2 )
			for( ram=0;ram<RamNum;ram++ )
			{
				i=code[count/4]>>(count%4)*4;
				i&=0xf;
				c=(char)i+'0';
				if( c>'9' )
					c=c+'\7';
				initstr[ram][j][pos]=c;
				count++;
				i=code[count/4]>>(count%4)*4;
				i&=0xf;
				c=(char)i+'0';
				if( c>'9' )
					c=c+'\7';
				initstr[ram][j][pos-1]=c;
				count++;
			}
	}	
}

char ramstyle[5][32]={ "invaild","s16_s16","s8_s8","","s4_s4" };
char addrsize[5][32]={ "invaild","( 7 downto 0)","( 8 downto 0)","","" };
char zeros[5][32]={ "invailid","0000000000000000","00000000","","0000" };
char bus[32];

void genInstance( int i, int RamNum )
{
	int j;
	
	if( RamNum==1 )
	{
		bus[0]='\0';	
	}
	if( RamNum==2 )
	{
		if( i== 0 )
			strcpy(bus,"(7 downto 0)");
		if( i== 1 )
			strcpy(bus,"(15 downto 8)");	
	}
	if( RamNum==4 )
	{
		if( i== 0 )
			strcpy(bus,"(3 downto 0)");
		if( i== 1 )
			strcpy(bus,"(7 downto 4)");	
		if( i== 2 )
			strcpy(bus,"(11 downto 8)");
		if( i== 3 )
			strcpy(bus,"(15 downto 12)");	
	}	
	
	fprintf(fp,"  ram_%d: RAMB4_%s\n",i,ramstyle[RamNum]);
	fprintf(fp,"  generic map (\n");
	for( j=0;j<16;j++ )
	{
		fprintf(fp,"               INIT_0%1X => X\"%s\"",j,initstr[i][j]);
		if( j!=15 )
			fprintf(fp,",\n");
		else
			fprintf(fp,"\n");
	}
	fprintf(fp,"               )\n");
	fprintf(fp,"  port map(\n");    
	fprintf(fp,"  	DIA    => dina%s,\n",bus);
	fprintf(fp,"        DIB    => \"%s\",\n",zeros[RamNum]);
	fprintf(fp,"        ENA    => '1',\n");
	fprintf(fp,"        ENB    => '1',\n");
	fprintf(fp,"        WEA    => wea,\n");
	fprintf(fp,"        WEB    => '0',\n");
	fprintf(fp,"        RSTA   => '0',\n");
	fprintf(fp,"        RSTB   => '0',\n");
	fprintf(fp,"        CLKA   => clka,\n");
	fprintf(fp,"        CLKB   => clkb,\n");
	fprintf(fp,"        ADDRA  => addra%s,\n",addrsize[RamNum]);
	fprintf(fp,"        ADDRB  => addrb%s,\n",addrsize[RamNum]);
	fprintf(fp,"        DOA    => douta%s,\n",bus);
	fprintf(fp,"        DOB    => doutb%s\n",bus);
	fprintf(fp,"        );\n");

}

void genVhdlRam(char *name, int RamNum )
{
	int i,j;
	genInitStr(RamNum);
	fprintf(fp,"library IEEE;\n");
	fprintf(fp,"use IEEE.STD_LOGIC_1164.ALL;\n");
	fprintf(fp,"library unisim;\n");
	fprintf(fp,"use unisim.vcomponents.all;\n");
	fprintf(fp,"entity %s is\n",name);
	fprintf(fp,"    Port (\n");      
	fprintf(fp,"			addra: IN std_logic_VECTOR(11 downto 0);\n");
	fprintf(fp,"			addrb: IN std_logic_VECTOR(11 downto 0);\n");
	fprintf(fp,"			clka: IN std_logic;\n");
	fprintf(fp,"			clkb: IN std_logic;\n");
	fprintf(fp,"			dina: IN std_logic_VECTOR(18 downto 0);\n");
	fprintf(fp,"			douta: OUT std_logic_VECTOR(18 downto 0);\n");
	fprintf(fp,"			doutb: OUT std_logic_VECTOR(18 downto 0);\n");
	fprintf(fp,"			wea: IN std_logic);\n");
	fprintf(fp,"end %s;\n",name);
	fprintf(fp,"architecture low_level_definition of %s is\n",name);
	for( i=0;i<16;i++ )
		fprintf(fp,"attribute INIT_0%1X : string;\n",i); 
	for( i=0;i<16;i++ )
	{
		for( j=0;j<RamNum;j++ )
			fprintf(fp,"attribute INIT_0%1X of ram_%d : label is  \"%s\";\n",i,j,initstr[j][i]);
	}
	fprintf(fp,"begin\n");
	for( i=0;i<RamNum;i++ )
		genInstance(i,RamNum);
	fprintf(fp,"end low_level_definition;\n");

}

main(int argc, char *argv[])
{
	int i;
	char fn[32];
	codeline=0;
	memlabel.length=0;
	needlabel.length=0;
	ins=0;
	lineno=0;
	for( i=0;i<1024;i++ )
		code[i]=0;
#ifdef DEBUG
	yydebug=1;
#endif
	yyin=fopen(argv[2],"rt");
	yyparse();
	close(yyin);
	processLabel();
	strcpy(fn,argv[1]);
	
	strcat(fn,".coe");
	fp=fopen(fn,"wt");
	genCoeFile();
	fclose(fp);
	strcpy(fn,argv[1]);
	strcat(fn,"_romonly.vhd");
	fp=fopen(fn,"wt");
	strcpy(fn,argv[1]);
	strcat(fn,"_romonly");
	genVhdlRom(fn);
	fclose(fp);
	strcpy(fn,argv[1]);
	strcat(fn,".log");
	fp=fopen(fn,"wt");
	genLog(argv[2]);
	fclose(fp);
	strcpy(fn,argv[1]);
	strcat(fn,".bit");
	fp=fopen(fn,"wt");
	for( i=0;i<codeline;i++ )
		fprintf(fp,"%s\n",genBitString(code[i],18));
	fclose(fp);
}

void getLine( FILE *fp, char *str )
{
	int i;
	for( i=0;i<255;i++ )
	{
		if( (str[i]=getc(fp))=='\n' )
			break;
	}
	str[i]='\0';
}
void genLog( char *psmFile )
{
	FILE *psm;
	int i,line;
	char strLine[256];
	psm=fopen(psmFile,"rt");
	fprintf(fp,"addr  code\n");
	line=0;
	for( i=0;i<codeline;i++ )
	{
		for(;line<srcline[i];line++ )
		{
			fprintf(fp,"%04X  ",i);
			getLine(psm,strLine);
			fprintf(fp,"          %s\n",strLine);
		}
		fprintf(fp,"%04X  ",i);
		fprintf(fp,"%05X  ",code[i]);
		getLine(psm,strLine);
		fprintf(fp,"    %s\n",strLine);
		line++;
	}	
		
}

int yywrap()
{
	return(1);
}

void yyerror(const char* str )
{
	fprintf(stderr,"zas: %s %s %d\n",str,yytext,codeline);
}

void buildins()
{
	code[codeline]=ins;
	srcline[codeline]=lineno;	
}

void need_label(char *str)
{
	strcpy(needlabel.label[needlabel.length].name,str);
	needlabel.label[needlabel.length].pos=codeline;
	needlabel.length++;
}
int ctox(char *p)
{
	char c=*p;
	c=c-'0';
	if( (int)c>9 )
		c=c+'0'-'A'+10;
	return ((int)c)&0xf;
}
int atox(char *p)
{
	int a = ctox(p);
	a<<=4;
	a|=ctox(p+1);
	return a&0xff;
}

char *genBitString(int x,int l)
{
	int i;
	strBitString[0]='z';
	for(i=0;i<l;i++)
	{
		strBitString[i]='0'+((x>>(l-1-i))&1);
	}
	strBitString[l]='\0';
	return strBitString;
}

void genCoeFile()
{
	int i;
	fprintf(fp,"memory_initialization_radix = 16;\n");
	fprintf(fp,"memory_initialization_vector =\n"); 
	for( i=0;i<codeline-1;i++ ) {
		fprintf(fp,"%05X,\n",code[i]);
	}
	fprintf(fp,"%05X;\n",code[i]);
}