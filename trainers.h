//
// C++ Interface: trainers
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef TRAINERS_H
#define TRAINERS_H

void improveallsubs ( Character *ch );
int mag_manacost ( Character *ch, int spellnum );

int   parse_class(char arg);
Character * GetMobProto ( mob_vnum vn );


int assign_group_trains ( Character *mob, int group, int filter );

const char *unlearnedsub = "Undiscovered SubSkill";
const char *prac_types[] =
{
	"spell",
	"skill",
	"subskill"
};
/* actual prac_params are in class.c */
extern int prac_params[4][NUM_CLASSES];

#define LEARNED_LEVEL	0	/* % known which is considered "learned" */
#define MAX_PER_PRAC	1	/* max percent gain in skill per practice */
#define MIN_PER_PRAC	2	/* min percent gain in skill per practice */
#define PRAC_TYPE	3	/* should it say 'spell' or 'skill'?     */

#define LEARNED(ch) (prac_params[LEARNED_LEVEL][(int)GET_CLASS(ch)])
#define MINGAIN(ch) (prac_params[MIN_PER_PRAC][(int)GET_CLASS(ch)])
#define MAXGAIN(ch) (prac_params[MAX_PER_PRAC][(int)GET_CLASS(ch)])
#define SPLSKL(ch) (prac_types[prac_params[PRAC_TYPE][(int)GET_CLASS(ch)]])

#define ALL_SKILLS 0
#define ALL_TIER_ONE 1
#define ALL_TIER_TWO 2
#define ALL_TIER_THREE 3
#define ALL_TIER_FOUR 4

#endif
