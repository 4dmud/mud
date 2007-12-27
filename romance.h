//
// C++ Interface: romance
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
/* Romance Module Defines */
#define SAME_SEX_ALLOWED FALSE     /* True/False */

/* MatingMod Defines */
#define OUT_OF_WEDLOCK TRUE   /* True/False */
#define REWARD_ALLOWED TRUE   /* T/F */
#define HP_REWARD      10     /* All rewards are NUMERICAL */
#define MP_REWARD      10
#define STR_REWARD     0
#define CON_REWARD     1
#define INT_REWARD     0
#define WIS_REWARD     0
#define DEX_REWARD     0
#define ABORT_ALLOWED  TRUE   /* T/F */
#define NINE_MONTHS    6000   /* 6000 realtime minutes TO GO */
#define MONTHS_8        5333
#define MONTHS_7        4666  /* Note: These are MONTHS REMAINING */
#define MONTHS_6        4000
#define MONTHS_5        3333
#define MONTHS_4        2666
#define MONTHS_3        2000
#define MONTHS_2        1333
#define MONTH_1         666

char * romance_status(Character *ch);
char * baby_status(Character *i, char * buf, size_t len);
