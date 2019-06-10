#ifndef __ZAS_H
#define __ZAS_H	
extern int codeline;
extern int isDirect;
extern int twoArgOp;
extern int shiftOp;
extern int opGrp;
extern int jumpGrp;
extern int jumpFlag;
extern int code[];
extern int ins;
extern int arg1;
extern int arg2;

typedef struct strulabel {
	char name[20];
	int pos;
} label_t;

typedef struct strulabelBuf {
	label_t label[4096];
	int length;
} label_buf;

extern label_buf memlabel;
extern label_buf needlabel;
void buildins();
void need_label(); 
int ctox(char *);
int atox(char *);

#define YYSTYPE long

#endif
