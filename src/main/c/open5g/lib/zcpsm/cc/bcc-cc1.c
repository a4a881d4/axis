/* bcc-cc1.c - "pass 1" for bcc */

/* Copyright (C) 1992 Bruce Evans */

#include "const.h"
#include "types.h"
extern bool_t debugon;
extern bool_t i386_32;
PUBLIC int main(argc, argv)
int argc;
char **argv;
{
    
    growheap(0);		/* init order is important */
    syminit();
    etreeinit();
    ifinit();
    predefine();
    openio(argc, argv);
#ifndef MC6809
    i386_32 = 0;
#endif
    codeinit();
    typeinit();
     program();
    finishup();

    /* NOTREACHED */
    return 0;
}
