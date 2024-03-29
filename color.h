//
// C++ Interface: colour
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef COLOR_H
#define COLOR_H

#define CNUL  ""

/*             Plain Colours            has the CNRM before it to reset color!*/
#define CNRM  "\x1B[0;0m"                /* &n */
#define CRED  "\x1B[0;0m\x1B[38;5;160m"  /* &r */
#define CGRN  "\x1B[0;0m\x1B[32m"        /* &g */
#define CYEL  "\x1B[0;0m\x1B[33m"        /* &y */
#define CBLU  "\x1B[0;0m\x1B[38;5;21m"   /* &b */
#define CMAG  "\x1B[0;0m\x1B[38;5;128m"  /* &m */
#define CCYN  "\x1B[0;0m\x1B[36m"        /* &c */
#define CWHT  "\x1B[0;0m\x1B[37m"        /* &w */
#define CBLK  "\x1B[0;0m\x1B[30m"        /* &l */
#define CORA  "\x1B[0;0m\x1B[38;5;172m"  /* &o */

/*              Bold Colors            */
#define BRED  "\x1B[1;31m"        /* &R */
#define BGRN  "\x1B[1;32m"        /* &G */
#define BYEL  "\x1B[1;33m"        /* &Y */
#define BBLU  "\x1B[1;38;5;33m"   /* &B */
#define BMAG  "\x1B[1;38;5;165m"  /* &M */
#define BCYN  "\x1B[1;36m"        /* &C */
#define BWHT  "\x1B[1;37m"        /* &W */
#define BBLK  "\x1B[1;30m"        /* &L */
#define BORA  "\x1B[1;38;5;214m"  /* &O */

/*             Backgrounds             */
#define BKRED  "\x1B[41m"     /* &e */
#define BKGRN  "\x1B[42m"     /* &k */
#define BKYEL  "\x1B[43m"     /* &p */
#define BKBLU  "\x1B[44m"     /* &u */
#define BKMAG  "\x1B[45m"     /* &a */
#define BKCYN  "\x1B[46m"     /* &f */
#define BKWHT  "\x1B[47m"     /* &h */
#define BKBLK  "\x1B[40m"     /* &i */
#define BKORA  "\x1B[48;5;172m"

/*         Underline, Flashing         */
#define UNDER  "\x1B[4m" /* &U */
#define FLASH  "\x1B[5m" /* &F */

//const char *COLOURLIST[] = {CNRM, CRED, CGRN, CYEL, CBLU, CMAG, CCYN, CWHT, CBLK,
//                BRED, BGRN, BYEL, BBLU, BMAG, BCYN, BWHT, BBLK,
//                BKRED,BKGRN,BKYEL,BKBLU,BKMAG,BKCYN,BKWHT,BKBLK,
//                UNDER,FLASH};
//we'll use a switch so we can use letters
#define MAX_COLOURS 26

#endif
