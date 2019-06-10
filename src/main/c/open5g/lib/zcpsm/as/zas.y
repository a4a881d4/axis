%{
#include"zas.h"
extern int lineno;

%}
%start instructions

%token LOAD_TOKEN AND_TOKEN OR_TOKEN XOR_TOKEN 
%token ADD_TOKEN ADDCY_TOKEN SUB_TOKEN SUBCY_TOKEN
%token SR0_TOKEN SR1_TOKEN SRX_TOKEN SRA_TOKEN RR_TOKEN
%token SL0_TOKEN SL1_TOKEN SLX_TOKEN SLA_TOKEN RL_TOKEN
%token INPUT_TOKEN OUTPUT_TOKEN
%token Z_TOKEN NZ_TOKEN C_TOKEN NC_TOKEN
%token JUMP_TOKEN CALL_TOKEN RETURN_TOKEN REG 
%token COMMENT LABEL_END 
%token DIRECT LABEL 
%token NEWLINE COMMA

%%

instructions: 
	| instructions line
	;
	
line: instruction
	{	buildins();codeline++; ins=0; lineno++; }
	| label_dec
	;

instruction: program_control	{opGrp=0x8; ins|=0x8000;}
	| twoarg		{opGrp=0xc;}
	| shift			{opGrp=0xd;}
	| inout			{opGrp=0xe;}
	;
	
program_control: jumps	{jumpGrp=0; ins|=0x0000;}
	| call		{jumpGrp=3; ins|=0x0c00;}
	| return	{jumpGrp=2; ins|=0x0800;}
	;
jumps: JUMP_TOKEN  LABEL { need_label($2); }
	| JUMP_TOKEN condition COMMA LABEL { jumpGrp=4; ins|=0x1000; need_label($4);  }
	;

condition: Z_TOKEN { jumpFlag=0;ins|=0x0000; }
	| NZ_TOKEN { jumpFlag=1;ins|=0x0400; }
	| C_TOKEN  { jumpFlag=2;ins|=0x0800; }
	| NC_TOKEN { jumpFlag=3;ins|=0x0C00; }
	;
	
call: CALL_TOKEN LABEL { need_label($2);  }
	;

return: RETURN_TOKEN 
	;
		
twoAop: logicalop 
	| arithmeticop {twoArgOp|=4;}
	;

logicalop: LOAD_TOKEN {twoArgOp=0;}
	| AND_TOKEN   {twoArgOp=1;}
	| OR_TOKEN    {twoArgOp=2;}	
	| XOR_TOKEN   {twoArgOp=3;}
	;
	
arithmeticop: ADD_TOKEN {twoArgOp=0;}
	| ADDCY_TOKEN   {twoArgOp=1;}
	| SUB_TOKEN     {twoArgOp=2;}
	| SUBCY_TOKEN   {twoArgOp=3;}
	;
secondarg: REG {isDirect=0;arg2=$1;}
	| DIRECT {isDirect=1;arg2=atox((char *)$1); }
	;
twoarg: twoAop REG COMMA secondarg 
	{	arg1=$2;
		ins|=(arg1&0xf)<<8;
		ins|=((arg1&0x10)>>4)<<17;
		if( !isDirect )
		{
			ins|=twoArgOp&0x7;
			ins|=(arg2&0xf)<<4;
			ins|=((arg2&0x10)>>4)<<16;
			ins|=0xc000;
			
		}
		else
		{
			ins|=(twoArgOp&0x7)<<12;
			ins|=arg2&0xff;
		}
	}
	;
shiftop: SR0_TOKEN	{shiftOp=0xe;}
	| SR1_TOKEN	{shiftOp=0xf;}
	| SRX_TOKEN	{shiftOp=0xa;}
	| SRA_TOKEN	{shiftOp=0x8;}
	| RR_TOKEN	{shiftOp=0xc;}
	| SL0_TOKEN	{shiftOp=0x6;}
	| SL1_TOKEN	{shiftOp=0x7;}
	| SLX_TOKEN	{shiftOp=0x4;}
	| SLA_TOKEN	{shiftOp=0x0;}
	| RL_TOKEN	{shiftOp=0x2;}
	;
shift: shiftop REG 
	{
		arg1=$2;
		ins|=0xd000;
		ins|=(arg1&0xf)<<8;
		ins|=((arg1&0x10)>>4)<<17;
		ins|=(shiftOp&0xf);
	}
	;
inout: INPUT_TOKEN REG COMMA secondarg
	{
		arg1=$2;
		ins|=(arg1&0xf)<<8;
		ins|=((arg1&0x10)>>4)<<17;
		if( isDirect )
		{
			ins|=(arg2&0xff);
			opGrp=0xa;
			ins|=0xa000;
		}
		else
		{
			ins|=(arg2&0xf)<<4;
			ins|=((arg2&0x10)>>4)<<16;
			opGrp=0xb;
			ins|=0xb000;
		}
		
	}	
	| OUTPUT_TOKEN REG COMMA secondarg
	{
		arg1=$2;
		ins|=(arg1&0xf)<<8;
		ins|=((arg1&0x10)>>4)<<17;
		if( isDirect )
		{
			ins|=(arg2&0xff);
			opGrp=0xe;
			ins|=0xe000;
		}
		else
		{
			ins|=(arg2&0xf)<<4;
			ins|=((arg2&0x10)>>4)<<16;
			opGrp=0xf;
			ins|=0xf000;
		}
		
	}	
	;

label_dec: LABEL LABEL_END
	{	strcpy(memlabel.label[memlabel.length].name,(char *)$1);
		memlabel.label[memlabel.length].pos=codeline;
		memlabel.length++;
		lineno++;
	}
	;	
	
