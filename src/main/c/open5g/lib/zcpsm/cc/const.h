/* const.h - constants for bcc */

/* Copyright (C) 1992 Bruce Evans */

/* switches for code generation */

#define DEBUG			/* generate compiler-debugging code */
#define KCPSM	/* target processor is KCPSM */

#define MC6809 KCPSM
#undef SELFTYPECHECK		/* check calculated type = runtime type */


#ifdef MC6809
# define DYNAMIC_LONG_ORDER 0	/* have to define it so it works in #if's */
# define OP1			/* logical operators only use 1 byte */
# define POSINDEPENDENT		/* position indep code can (also) be gen */
#endif

#define TOS_EDOS		/* target O/S is EDOS */

/* switches for source machine dependencies */

#ifndef SOS_EDOS
# define S_ALIGNMENT (sizeof(int))  /* source memory alignment, power of 2 */
#endif

#ifndef SOS_MSDOS /* need portable alignment for large model */
# define UNPORTABLE_ALIGNMENT
#endif

/* local style */

#define FALSE 0
#ifndef NULL
#define NULL 0
#endif
#define TRUE 1

#define EXTERN extern
#define FORWARD static
#define PRIVATE static
#define PUBLIC
