/* hardop.c - hardware operations for bcc */

/* Copyright (C) 1992 Bruce Evans */

#include "const.h"
#include "types.h"
#include "byteord.h"
#include "condcode.h"
#include "gencode.h"
#include "reg.h"
#include "sc.h"
#include "scan.h"
#include "sizes.h"
#include "type.h"
#include "parse.h"
#include<stdio.h>

extern offset_t autoVarPos;		/* auto Number */
extern offset_t globalVarPos;		/* global Number */



PUBLIC void _zcc_addab(source, target)
struct symstruct *source;
struct symstruct *target;
{
    
    if ( target->storage == LOCAL )
    {
	if( source->storage == CONSTANT && target->type->typesize==1)
	{
		printf("ADD\ts%02X,\t%02X\n",target->offset.offi, source->offset.offv&0xff); 
		return;
	}
	if( source->storage == CONSTANT && target->type->typesize==2 )
	{
		printf("ADD\ts%02X,\t%02X\n",target->offset.offi, source->offset.offv&0xff);
		printf("ADDCY\ts%02X,\t%02X\n",target->offset.offi+1, (source->offset.offv>>8)&0xff);
		return;
	}
	if( source->storage == LOCAL && target->type->typesize==1)
	{
		printf("ADD\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi); 
		
		return;
	}
	if( source->storage == LOCAL && target->type->typesize==2)
	{
		if( source->type->typesize==2 )
		{
			printf("ADD\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi); 
			printf("ADDCY\ts%02X,\ts%02X\n",target->offset.offi+1, source->offset.offi+1); 
		}
		if( source->type->typesize==1 )
		{
			printf("ADD\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi); 
			printf("ADDCY\ts%02X,\t00\n",target->offset.offi+1); 
		}
		
		return;
	}
    }
}

PUBLIC void _zcc_opab(source, target, opstr)
struct symstruct *source;
struct symstruct *target;
char *opstr;
{
    
    if ( target->storage == LOCAL )
    {
	if( source->storage == CONSTANT && target->type->typesize==1)
	{
		printf("%s\ts%02X,\t%02X\n",opstr,target->offset.offi, source->offset.offv&0xff); 
		return;
	}
	if( source->storage == CONSTANT && target->type->typesize==2 )
	{
		printf("%s\ts%02X,\t%02X\n",opstr,target->offset.offi, source->offset.offv&0xff);
		printf("%s\ts%02X,\t%02X\n",opstr,target->offset.offi+1, (source->offset.offv>>8)&0xff);
		return;
	}
	if( source->storage == LOCAL && target->type->typesize==1)
	{
		printf("%s\ts%02X,\ts%02X\n",opstr,target->offset.offi, source->offset.offi); 
		
		return;
	}
	if( source->storage == LOCAL && target->type->typesize==2)
	{
		if( source->type->typesize==2 )
		{
			printf("%s\ts%02X,\ts%02X\n",opstr,target->offset.offi, source->offset.offi); 
			printf("%s\ts%02X,\ts%02X\n",opstr,target->offset.offi+1, source->offset.offi+1); 
		}
		if( source->type->typesize==1 )
		{
			printf("%s\ts%02X,\ts%02X\n",opstr,target->offset.offi, source->offset.offi); 
			printf("%s\ts%02X,\t00\n",opstr,target->offset.offi+1); 
		}
		
		return;
	}
    }
}

static void _int_op1(source, target, opstr)
struct symstruct *source;
struct symstruct *target;
char *opstr;
{
    if (source->storage == CONSTANT)
    {
	if( target->storage == LOCAL )
	{
		if( target->type->typesize==1 )
		{
			printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
			printf("%s\ts00,\t%02X\n",opstr,source->offset.offv&0xff);
		}
		if( target->type->typesize==2 )
		{
			printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
			printf("LOAD\ts01,\ts%02X\n",target->offset.offi+1);
			printf("%s\ts00,\t%02X\n",opstr,source->offset.offv&0xff);
			printf("%s\ts01,\t%02X\n",opstr,(source->offset.offv>>8)&0xff);
		}
	}
	if( target->type->typesize==1 )
		target->storage=DREG8;
	if( target->type->typesize==2 )
		target->storage=DREG16;
	target->offset.offi = 0;
	return;	
    }
    if( source->storage == LOCAL )
    {
	if ( target->storage == LOCAL )
	{
		if( target->storage == LOCAL )
		{
			if( target->type->typesize==1 )
			{
				printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
				printf("%s\ts00,\ts%02X\n",opstr,source->offset.offi); 
			}
			if( target->type->typesize==2 )
			{
				printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
				printf("LOAD\ts01,\ts%02X\n",target->offset.offi+1);
				printf("%s\ts00,\ts%02X\n",opstr,source->offset.offi);
				printf("%s\ts01,\ts%02X\n",opstr,source->offset.offi+1);
			}

			if( target->type->typesize==1 )
				target->storage=DREG8;
			if( target->type->typesize==2 )
				target->storage=DREG16;
		}
	}
    }
    
	if( target->storage==DREG8 && source->storage==LOCAL )
		printf("%s\ts00,\ts%02X\n",opstr,source->offset.offi); 
	if( target->storage==DREG16 && source->storage==LOCAL )
	{
		printf("%s\ts00,\ts%02X\n",opstr,source->offset.offi); 
		printf("%s\ts01,\ts%02X\n",opstr,source->offset.offi+1);
	}
	
	if( target->storage==DREG8 && source->storage==CONSTANT )
		printf("%s\ts00,\t%02X\n",opstr,source->offset.offv&0xff); 
	if( target->storage==DREG16 && source->storage==LOCAL )
	{
		printf("%s\ts00,\t%02X\n",opstr,source->offset.offv&0xff); 
		printf("%s\ts01,\t%02X\n",opstr,(source->offset.offv>>8)&0xff);
	}
	
	return;
    

}


PUBLIC void _zcc_op1(op, source, target)
struct symstruct *source;
struct symstruct *target;
op_t op;
{
	switch( (op_t) op ) {
		case ANDOP:
			_int_op1( source, target, "AND" );
			break;
		case OROP:
			_int_op1( source, target, "OR" );
			break;
		case EOROP:
			_int_op1( source, target, "XOR" );
			break;
		default:
			break;
	}
	   
}



PUBLIC void _zcc_subab(source, target)
struct symstruct *source;
struct symstruct *target;
{
    
    if ( target->storage == LOCAL )
    {
	if( source->storage == CONSTANT && target->type->typesize==1)
	{
		printf("SUB\ts%02X,\t%02X\n",target->offset.offi, source->offset.offv&0xff); 
		return;
	}
	if( source->storage == CONSTANT && target->type->typesize==2 )
	{
		printf("SUB\ts%02X,\t%02X\n",target->offset.offi, source->offset.offv&0xff);
		printf("SUBCY\ts%02X,\t%02X\n",target->offset.offi+1, (source->offset.offv>>8)&0xff);
		return;
	}
	if( source->storage == LOCAL && target->type->typesize==1)
	{
		printf("SUB\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi); 
		
		return;
	}
	if( source->storage == LOCAL && target->type->typesize==2)
	{
		if( source->type->typesize==2 )
		{
			printf("SUB\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi); 
			printf("SUBCY\ts%02X,\ts%02X\n",target->offset.offi+1, source->offset.offi+1); 
		}
		if( source->type->typesize==1 )
		{
			printf("SUB\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi); 
			printf("SUBCY\ts%02X,\t00\n",target->offset.offi+1); 
		}
		return;
	}
    }
}

PUBLIC void _zcc_add(source, target)
struct symstruct *source;
struct symstruct *target;
{
    if (source->storage == CONSTANT)
    {
	if( target->storage == LOCAL )
	{
		if( target->type->typesize==1 )
		{
			printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
		}
		if( target->type->typesize==2 )
		{
			printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
			printf("LOAD\ts01,\ts%02X\n",target->offset.offi+1);
		}
	}
	if( target->type->typesize==1 )
		target->storage=CONSTANT|DREG8;
	if( target->type->typesize==2 )
		target->storage=CONSTANT|DREG16;
	target->offset.offi = (offset_t) source->offset.offv;
	return;	
    }
    if( source->storage == LOCAL )
    {
	if ( target->storage == LOCAL )
	{
		if( target->storage == LOCAL )
		{
			if( target->type->typesize==1 )
			{
				printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
				printf("ADD\ts00,\ts%02X\n",source->offset.offi); 
			}
			if( target->type->typesize==2 )
			{
				printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
				printf("LOAD\ts01,\ts%02X\n",target->offset.offi+1);
				printf("ADD\ts00,\ts%02X\n",source->offset.offi);
				printf("ADDCY\ts01,\ts%02X\n",source->offset.offi+1);
			}

			if( target->type->typesize==1 )
				target->storage=DREG8;
			if( target->type->typesize==2 )
				target->storage=DREG16;
		}
	}
	return;
	
    }	
	if( target->storage==DREG8 && source->storage==LOCAL )
		printf("ADD\ts00,\ts%02X\n",source->offset.offi); 
	if( target->storage==DREG16 && source->storage==LOCAL )
	{
		printf("ADD\ts00,\ts%02X\n",source->offset.offi); 
		printf("ADDCY\ts01,\ts%02X\n",source->offset.offi+1);
	}
	
	if( target->storage==DREG8 && source->storage==CONSTANT )
		printf("ADD\ts00,\t%02X\n",source->offset.offv&0xff); 
	if( target->storage==DREG16 && source->storage==LOCAL )
	{
		printf("ADDCY\ts00,\t%02X\n",source->offset.offv&0xff); 
		printf("ADDCY\ts01,\t%02X\n",(source->offset.offv>>8)&0xff);
	}

	return;
    
	
}


PUBLIC void _zcc_assign(source, target)
struct symstruct *source;
struct symstruct *target;
{
    store_pt regpushed;
    store_pt sourcereg;
    scalar_t tscalar;
	

    if ( target->storage == LOCAL )
    {
	if( source->storage == CONSTANT && target->type->typesize==1 )
	{
		printf("LOAD\ts%02X,\t%02X\n",target->offset.offi, source->offset.offv&0xff); 
		return;
	}
	if( source->storage == CONSTANT && target->type->typesize==2 )
	{
		if( source->type->typesize==2 )
		{
			printf("LOAD\ts%02X,\t%02X\n",target->offset.offi, source->offset.offv&0xff);
			printf("LOAD\ts%02X,\t%02X\n",target->offset.offi+1, (source->offset.offv>>8)&0xff);
		 }
		if( source->type->typesize==1 )
		{
			printf("LOAD\ts%02X,\t%02X\n",target->offset.offi, source->offset.offv&0xff);
			printf("LOAD\ts%02X,\t00\n",target->offset.offi+1);
		}
		 
		return;
	}
    
    	if ( source->storage == (DREG8|CONSTANT) )
    	{
	
		if(target->type->typesize==1)
		{
			printf("ADD\ts00,\t%02X\n",source->offset.offi&0xff);
			printf("LOAD\ts%02X,\ts00\n",target->offset.offi);
		}	
		if(target->type->typesize==2)
		{
			printf("LOAD\ts01,\t00\n");
			printf("ADD\ts00,\t%02X\n",source->offset.offi&0xff);
			printf("ADDCY\ts01,\t00\n");
			printf("LOAD\ts%02X,\ts00\n",target->offset.offi);
			printf("LOAD\ts%02X,\ts01\n",target->offset.offi+1);
		}	
		return;
    	}
    	if ( source->storage == (DREG16|CONSTANT) )
    	{
    		if(target->type->typesize==2)
    		{ 
			printf("ADD\ts00,\t%02X\n",source->offset.offi&0xff);
			printf("ADDCY\ts01,\t%02X\n",(source->offset.offi>>8)&0xff);
			printf("LOAD\ts%02X,\ts00\n",target->offset.offi);
			printf("LOAD\ts%02X,\ts01\n",target->offset.offi+1);
		}
		if(target->type->typesize==1)
    		{ 
			printf("ADD\ts00,\t%02X\n",source->offset.offi&0xff);
			printf("LOAD\ts%02X,\ts00\n",target->offset.offi);
		}
		return;
    	}

    	if ( source->storage == DREG8 )
    	{
	
		
		printf("LOAD\ts%02X,\ts00\n",target->offset.offi);
		return;
    	}
    	if ( source->storage == DREG16 )
    	{
		printf("LOAD\ts%02X,\ts00\n",target->offset.offi);
		printf("LOAD\ts%02X,\ts01\n",target->offset.offi+1);
		
		return;
    	}
	if( source->storage == LOCAL )
	{
		if(  target->type->typesize==1 )
		{
			printf("LOAD\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi); 
			return;
		}
		if( target->type->typesize==2 )
		{
			if(  source->type->typesize==2 )
			{
				printf("LOAD\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi);
				printf("LOAD\ts%02X,\ts%02X\n",target->offset.offi+1, source->offset.offi+1);
		 	}
			if(  source->type->typesize==1 )
			{
				printf("LOAD\ts%02X,\ts%02X\n",target->offset.offi, source->offset.offi);
				printf("LOAD\ts%02X,\t00\n",target->offset.offi+1);
		 	}
		 	return;
		}
	}    
	
    }
    if( target->storage == GLOBAL && strncmp(target->name.namep,"XBYTE",5)==0 )
    {
	if( source->storage == CONSTANT )
        {
		printf("LOAD\ts00,\t%02X\n", source->offset.offv&0xff);
		printf("OUTPUT\ts00,\t%02X\n",target->offset.offv&0xff); 
 		return;
	}
	if( source->storage == LOCAL )
        {
		printf("OUTPUT\ts%02X,\t%02X\n",source->offset.offi,target->offset.offv&0xff); 
 		return;
	}
    }
    if( source->storage == GLOBAL && strncmp(source->name.namep,"XBYTE",5)==0 )
    {
	
	if( target->storage == LOCAL )
        {
		printf("INPUT\ts%02X,\t%02X\n",target->offset.offi,source->offset.offv&0xff); 
 		return;
	}
    }

    if( target->storage == GLOBAL && strncmp(target->name.namep,"XWORD",5)==0 )
    {
	if( source->storage == CONSTANT )
        {
		printf("LOAD\ts00,\t%02X\n", source->offset.offv&0xff);
		printf("OUTPUT\ts00,\t%02X\n",target->offset.offv&0xff); 
 		printf("LOAD\ts01,\t%02X\n", (source->offset.offv>>8)&0xff);
		printf("OUTPUT\ts01,\t%02X\n",(target->offset.offv+1)&0xff); 
 		return;
	}
	if( source->storage == LOCAL )
        {
		printf("OUTPUT\ts%02X,\t%02X\n",source->offset.offi,target->offset.offv&0xff); 
 		printf("OUTPUT\ts%02X,\t%02X\n",source->offset.offi+1,(target->offset.offv+1)&0xff); 
 		return;
	}
    }
    if( source->storage == GLOBAL && strncmp(source->name.namep,"XWORD",5)==0 )
    {
	
	if( target->storage == LOCAL )
        {
		printf("INPUT\ts%02X,\t%02X\n",target->offset.offi,source->offset.offv&0xff); 
 		printf("INPUT\ts%02X,\t%02X\n",target->offset.offi+1,(source->offset.offv+1)&0xff); 
 		return;
	}
    }		
}

PUBLIC void _zcc_cmporsub(source, target, test)
struct symstruct *source;
struct symstruct *target;
int test;
{
	if( target->type->typesize == 1 )
	{	
		if( target->storage==LOCAL )
		{
			printf("LOAD\ts00,\ts%02X\n", target->offset.offi);
		}
		if( target->storage==CONSTANT )
		{
			printf("LOAD\ts00,\t%02X\n", target->offset.offv&0xff);
		}
		if( source->storage==LOCAL )
			printf("SUB\ts00,\ts%02X\n",source->offset.offi);
		if( source->storage==CONSTANT )
			printf("SUB\ts00,\t%02X\n",source->offset.offv&0xff);
	}
	if( target->type->typesize == 2 )
	{	
		if( target->storage==LOCAL )
		{
			printf("LOAD\ts00,\ts%02X\n", target->offset.offi);
			printf("LOAD\ts01,\ts%02X\n", target->offset.offi+1);
		}
		if( target->storage==CONSTANT )
		{
			printf("LOAD\ts00,\t%02X\n", target->offset.offv&0xff);
			printf("LOAD\ts01,\t%02X\n", (target->offset.offv>>8)&0xff);
		}
		if( source->storage==LOCAL )
		{
			printf("SUB\ts00,\ts%02X\n",source->offset.offi);
			printf("SUBCY\ts01,\ts%02X\n",source->offset.offi+1);
		}
		if( source->storage==CONSTANT )
		{
			printf("SUB\ts00,\t%02X\n",source->offset.offv&0xff);
			printf("SUBCY\ts01,\t%02X\n",(source->offset.offv>>8)&0xff);
		}
		if( test==1 )
			printf("OR\ts00,\ts01\n");
	}
	
}

PUBLIC void _zcc_incdec(op, source)
op_pt op;
struct symstruct *source;
{
    if ( source->storage == LOCAL )
    {
	if(source->type->typesize==1)
	{
		if(op==POSTINCOP)
			printf("ADD\ts%02X,\t01\n",source->offset.offi);	 	
		if(op==POSTDECOP)
			printf("SUB\ts%02X,\t01\n",source->offset.offi);	 	
	}
	
	if(source->type->typesize==2)
	{
		if(op==POSTINCOP)
		{
			printf("ADD\ts%02X,\t01\n",source->offset.offi);	 	
			printf("ADDCY\ts%02X,\t00\n",source->offset.offi+1);	 	
		}
		if(op==POSTDECOP)
		{
			printf("SUB\ts%02X,\t01\n",source->offset.offi);	
 			printf("SUBCY\ts%02X,\t00\n",source->offset.offi+1);	
		}	
	}
    }
}

PUBLIC struct symstruct *_zcc_addglb(name, type)
char *name;
struct typestruct *type;
{
    struct symstruct **hashptr;
    struct symstruct *oldsymptr;
    register struct symstruct *symptr;

    hashptr = gethashptr(name);
    symptr = *hashptr;
    while (symptr != NULL)
    {
	oldsymptr = symptr;
	symptr = symptr->next;
    }
    symptr = qmalloc(sizeof (struct symstruct) + strlen(name)+2);

    addsym(name, type, symptr);
    symptr->storage = GLOBAL;
    symptr->level = GLBLEVEL;
    if( strncmp(symptr->name.namea,"XBYTE",5)!= 0 && strncmp(symptr->name.namea,"XWORD",5)!= 0)
    {
	if( globalVarPos-type->typesize < autoVarPos )
		printf(";; too many global values auto value pos = %d global value pos = %d new auto value size is  %d\n",autoVarPos,globalVarPos,type->typesize);	
	symptr->offset.offi = globalVarPos-type->typesize;
	globalVarPos = globalVarPos-type->typesize;
	symptr->storage=LOCAL;
    }
    if (*hashptr == NULL)
    {
	*hashptr = symptr;
	symptr->prev = hashptr;
    }
    else
    {
	oldsymptr->next = symptr;
	symptr->prev = &oldsymptr->next;
    }
		


    return symptr;
}

PUBLIC void _zcc_not(target)
struct symstruct *target;
{
	if( target->storage==LOCAL )
	{
		printf("XOR\ts%02X,\tFF\n",target->offset.offi);
	}
}

PUBLIC void _zcc_sub(source, target)
struct symstruct *source;
struct symstruct *target;
{
    if (source->storage == CONSTANT)
    {
	if( target->storage == LOCAL )
	{
		if( target->type->typesize==1 )
		{
			printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
		}
		if( target->type->typesize==2 )
		{
			printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
			printf("LOAD\ts01,\ts%02X\n",target->offset.offi+1);
		}
	}
	if( target->type->typesize==1 )
		target->storage=CONSTANT|DREG8;
	if( target->type->typesize==2 )
		target->storage=CONSTANT|DREG16;
	target->offset.offi = -(offset_t) source->offset.offv;
	return;	
    }
    if( source->storage == LOCAL )
    {
	if ( target->storage == LOCAL )
	{
		if( target->storage == LOCAL )
		{
			if( target->type->typesize==1 )
			{
				printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
				printf("SUB\ts00,\ts%02X\n",source->offset.offi); 
			}
			if( target->type->typesize==2 )
			{
				printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
				printf("LOAD\ts01,\ts%02X\n",target->offset.offi+1);
				printf("SUB\ts00,\ts%02X\n",source->offset.offi);
				printf("SUBCY\ts01,\ts%02X\n",source->offset.offi+1);
			}

			if( target->type->typesize==1 )
				target->storage=DREG8;
			if( target->type->typesize==2 )
				target->storage=DREG16;
		}
	}
	return;
    }	
	if( target->storage==DREG8 && source->storage==LOCAL )
		printf("SUB\ts00,\ts%02X\n",source->offset.offi); 
	if( target->storage==DREG16 && source->storage==LOCAL )
	{
		printf("SUB\ts00,\ts%02X\n",source->offset.offi); 
		printf("SUBCY\ts01,\ts%02X\n",source->offset.offi+1);
	}
	
	if( target->storage==DREG8 && source->storage==CONSTANT )
		printf("SUB\ts00,\t%02X\n",source->offset.offv&0xff); 
	if( target->storage==DREG16 && source->storage==LOCAL )
	{
		printf("SUB\ts00,\t%02X\n",source->offset.offv&0xff); 
		printf("SUBCY\ts01,\t%02X\n",(source->offset.offv>>8)&0xff);
	}

	return;
    
	
}

PUBLIC void _zcc_sl(source, target)
struct symstruct *source;
struct symstruct *target;
{
    int i;
    if ( source->storage == CONSTANT )
    {
	if( target->storage == LOCAL && target->type->typesize==1)
	{
		for( i=0;i<source->offset.offv;i++)
			printf("SL0\ts%02X\n",target->offset.offi); 
		return;
	}
	if( target->storage == LOCAL && target->type->typesize==2 )
	{
		for( i=0;i<source->offset.offv;i++)
		{
			printf("SL0\ts%02X\n" ,target->offset.offi); 
			printf("SLA\ts%02X\n" ,target->offset.offi+1); 
		}
		return;
	}
    }
}

PUBLIC void _zcc_sr(source, target)
struct symstruct *source;
struct symstruct *target;
{
    int i;
    if ( source->storage == CONSTANT )
    {
	if( target->storage == LOCAL && target->type->typesize==1)
	{
		for( i=0;i<source->offset.offv;i++)
			printf("SR0\ts%02X\n",target->offset.offi); 
		return;
	}
	if( target->storage == LOCAL && target->type->typesize==2 )
	{
		for( i=0;i<source->offset.offv;i++)
		{
			printf("SR0\ts%02X\n" ,target->offset.offi+1); 
			printf("SRA\ts%02X\n" ,target->offset.offi); 
		}
		return;
	}
    }
}


PUBLIC void _zcc_test(target, pcondtrue)
struct symstruct *target;
ccode_t *pcondtrue;
{
    if( target->storage==DREG8 )
    {
    	printf("SUB\ts00,\t00\n");
    }
    if( target->storage==DREG16 )
    {
    	printf("OR\ts00,\ts01\n");
    }
    if( target->storage==LOCAL && target->type->typesize==1 )
    {
    	printf("SUB\ts%02X,\t00\n",target->offset.offi);
    }
    if( target->storage==LOCAL && target->type->typesize==2 )
    {
    	printf("LOAD\ts00,\ts%02X\n",target->offset.offi);
    	printf("OR\ts00,\ts%02X\n",target->offset.offi+1);
    }
}
