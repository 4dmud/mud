/* ************************************************************************
*   File: screen.h                                      Part of CircleMUD *
*  Usage: header file with ANSI color codes for online color              *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KGRN  "\x1B[32m"
#define KYEL  "\x1B[33m"
#define KBLU  "\x1B[34m"
#define KMAG  "\x1B[35m"
#define KCYN  "\x1B[36m"
#define KWHT  "\x1B[37m"
#define KNUL  ""

#define BNRM  "\x1B[1m"
#define BRED  "\x1B[1;31m"
#define BGRN  "\x1B[1;32m"
#define BYEL  "\x1B[1;33m"
#define BBLU  "\x1B[1;34m"
#define BMAG  "\x1B[1;35m"
#define BCYN  "\x1B[1;36m"
#define BWHT  "\x1B[1;37m"

/* conditional color.  pass it a pointer to a Character and a color level. */
#define C_OFF	0
#define C_SPR	1
#define C_NRM	2
#define C_CMP	3
#define _clrlevel(ch) (IS_NPC((ch)) ? 0 : (PRF_FLAGGED((ch), PRF_COLOR_1) ? 1 : 0) + (PRF_FLAGGED((ch), PRF_COLOR_2) ? 2 : 0))
#define clr(ch,lvl) (_clrlevel(ch) >= (lvl))
#define CCNRM(ch,lvl)  (clr((ch),(lvl))?KNRM:KNUL)
#define CCRED(ch,lvl)  (clr((ch),(lvl))?KRED:KNUL)
#define CCGRN(ch,lvl)  (clr((ch),(lvl))?KGRN:KNUL)
#define CCYEL(ch,lvl)  (clr((ch),(lvl))?KYEL:KNUL)
#define CCBLU(ch,lvl)  (clr((ch),(lvl))?KBLU:KNUL)
#define CCMAG(ch,lvl)  (clr((ch),(lvl))?KMAG:KNUL)
#define CCCYN(ch,lvl)  (clr((ch),(lvl))?KCYN:KNUL)
#define CCWHT(ch,lvl)  (clr((ch),(lvl))?KWHT:KNUL)

#define CBNRM(ch,lvl)  (clr((ch),(lvl))?BNRM:KNUL)
#define CBRED(ch,lvl)  (clr((ch),(lvl))?BRED:KNUL)
#define CBGRN(ch,lvl)  (clr((ch),(lvl))?BGRN:KNUL)
#define CBYEL(ch,lvl)  (clr((ch),(lvl))?BYEL:KNUL)
#define CBBLU(ch,lvl)  (clr((ch),(lvl))?BBLU:KNUL)
#define CBMAG(ch,lvl)  (clr((ch),(lvl))?BMAG:KNUL)
#define CBCYN(ch,lvl)  (clr((ch),(lvl))?BCYN:KNUL)
#define CBWHT(ch,lvl)  (clr((ch),(lvl))?BWHT:KNUL)

#define COLOR_LEV(ch) (_clrlevel(ch))

#define QNRM CCNRM(ch,C_SPR)
#define QRED CCRED(ch,C_SPR)
#define QGRN CCGRN(ch,C_SPR)
#define QYEL CCYEL(ch,C_SPR)
#define QBLU CCBLU(ch,C_SPR)
#define QMAG CCMAG(ch,C_SPR)
#define QCYN CCCYN(ch,C_SPR)
#define QWHT CCWHT(ch,C_SPR)

#define C_CLR "\x1B[H\x1B[J"
