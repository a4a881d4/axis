/*	zas.lex	*/
%{
#include<string.h>
#include "y.tab.h"
extern long yylval;
extern char *yytext;
extern int lineno;
%}

%%
";".*	{ lineno++; }
LOAD	{ return(LOAD_TOKEN); }
AND	{ return(AND_TOKEN); }
OR	{ return(OR_TOKEN); }
XOR	{ return(XOR_TOKEN); }
ADD	{ return(ADD_TOKEN); }
ADDCY	{ return(ADDCY_TOKEN); }
SUB	{ return(SUB_TOKEN); }
SUBCY	{ return(SUBCY_TOKEN); }
SR0	{ return(SR0_TOKEN); }
SR1	{ return(SR1_TOKEN); }
SRX	{ return(SRX_TOKEN); }
SRA	{ return(SRA_TOKEN); }
RR	{ return(RR_TOKEN); }
SL0	{ return(SL0_TOKEN); }
SL1	{ return(SL1_TOKEN); }
SLX	{ return(SLX_TOKEN); }
SLA	{ return(SLA_TOKEN); }
RL	{ return(RL_TOKEN); }
INPUT	{ return(INPUT_TOKEN); }
OUTPUT	{ return(OUTPUT_TOKEN); }
Z	{ return(Z_TOKEN); }
NZ	{ return(NZ_TOKEN); }
C	{ return(C_TOKEN); }
NC	{ return(NC_TOKEN); }
JUMP	{ return(JUMP_TOKEN); }
CALL	{ return(CALL_TOKEN); }
RETURN	{ return(RETURN_TOKEN); }
s[0-1][0-9A-Z]	{ yylval = ctox(yytext+1)*16+ctox(yytext+2); return(REG); }
\:	{ return(LABEL_END); }
\,	{ return(COMMA); }
[0-9A-F][0-9A-F]	{ yylval = (long)strdup(yytext); return(DIRECT); }
[_L][a-zA-Z0-9]*	{ yylval = (long)strdup(yytext); return(LABEL); }
[ \t\n]+	
%%

