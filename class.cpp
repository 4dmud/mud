/* ************************************************************************
*   File: class.c                                       Part of CircleMUD *
*  Usage: Source file for class-specific code                             *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
/*
 * $Log: class.c,v $
 * Revision 1.19  2007/11/22 01:51:20  w4dimenscor
 * Updated the house expand value to be 200 instead of 125, to encourage people to spend their tokens on expanding existing houses. Also in class.c updated the gold gain values, and added in bonus practice sessions too.
 *
 * Revision 1.18  2007/11/14 21:39:41  w4dimenscor
 * Added the Gladiator race for the gladiatorpits.
 * --Matthijs
 *
 * Revision 1.17  2007/06/10 02:18:39  w4dimenscor
 * changed all entries in the code of 'color' to 'colour', but i now regret it.
 *
 * Revision 1.16  2007/06/08 10:19:05  w4dimenscor
 * Added a way to check for real time, and time passed in dgscripts, REALDAY and REALHOUR and NOW are all variables, and also made it so tha the newbie channel doesnt sound drunk, also, practicing spells and skills need more practices
 *
 * Revision 1.15  2006/12/07 18:49:13  w4dimenscor
 * Rangers now have kick.
 *
 * Revision 1.14  2006/08/13 06:26:50  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.13  2006/06/19 06:25:39  w4dimenscor
 * Changed the player saved mount feature so that all players can load mounts from houses
 *
 * Revision 1.12  2006/06/16 10:54:51  w4dimenscor
 * Moved several functions in fight.c into the Character object. Also removed all occurances of send_to_char from skills.c
 *
 * Revision 1.11  2006/06/07 06:36:57  w4dimenscor
 * Color code purple is back in now, and i have removed the FTOI macro from the exp_needed function
 *
 * Revision 1.10  2006/06/07 06:29:31  w4dimenscor
 * Color code purple is back in now, and i have removed the FTOI macro from the exp_needed function
 *
 * Revision 1.9  2006/05/21 11:02:26  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.8  2006/05/01 11:29:26  w4dimenscor
 * I wrote a typo checker that automaticly corrects typos in the comm channels. I have also been fixing shadowed variables. There may be residual issues with it.
 *
 * Revision 1.7  2006/01/23 05:23:19  w4dimenscor
 * sorry self. another. _can't remember the changes_ entry
 *
 * Revision 1.6  2005/11/30 18:47:12  w4dimenscor
 * changed slightly some gains you get from remorts
 *
 * Revision 1.5  2005/11/19 06:18:38  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.4  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.3  2005/03/17 12:42:13  w4dimenscor
 * Added skill smash
 *
 * Revision 1.2  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.1.1.1  2004/11/12 02:16:40  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.43  2004/09/22 09:40:41  molly
 * automeld added so that corpses arent so easily lost, and also made pk corpses lootable
 *
 * Revision 1.42  2004/09/14 10:09:44  molly
 * added better optimisations
 *
 * Revision 1.39  2004/08/15 01:12:25  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
/*
 * This file attempts to concentrate most of the code which must be changed
 * in order for new classes to be added.  If you're adding a new class,
 * you should go through this entire file from beginning to end and add
 * the appropriate new special cases for your new class.
 */



#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "db.h"
#include "utils.h"
#include "spells.h"
#include "comm.h"
#include "interpreter.h"
#include "constants.h"
#include "genmob.h"



int num_casting ( Character *ch );
/* local functions */
int   parse_class ( char arg );
int   thaco ( int chclass_num, int level );

int   invalid_class ( Character *ch, struct obj_data *obj );
long find_class_bitvector ( char arg );
sbyte  saving_throws ( int chclass_num, int type, int level );
void  roll_real_abils ( Character * ch );
void  do_start ( Character * ch );
const char *title_male ( int chclass, int level );
const char *title_female ( int chclass, int level );
const char *simple_class_name ( Character *ch );
ACMD ( do_skilllist );
void assign_class ( int spell,
                    int chclass );
void set_mastery ( Character *ch,char buf );


/* Names first */

const char *class_abbrevs[] =
{
	"Mag",
	"Pri",
	"Thi",
	"War",
	"Hun",
	"Ran",
	"Gyp",
	"Esp",
	"\n"
};


const char *get_class_abbrev ( Character *ch )
{
	if ( PLR_FLAGGED ( ch, PLR_NEEDS_CLASS ) )
	{
		switch ( GET_CLASS ( ch ) )
		{
			case CLASS_MAGE:
			case CLASS_PRIEST:
			case CLASS_ESPER:
				return "Cst";
				break;
			case CLASS_HUNTER:
			case CLASS_WARRIOR:
				return "Ftr";
				break;
			case CLASS_GYPSY:
			case CLASS_RANGER:
			case CLASS_THIEF:
				return "Rge";
				break;
			default:  return "---";
		}
	}
	else
	{
		return class_abbrevs[ ( int ) GET_CLASS ( ch ) ];
	}
}


const char *pc_class_types[] =
{
	"Mage",
	"Priest",
	"Thief",
	"Warrior",
	"Hunter",
	"Ranger",
	"Gypsy",
	"Esper",
	"\n"
};
const char *pc_class_group_types[] =
{
	"Caster",
	"",
	"Rogue",
	"Fighter",
	"",
	"",
	"",
	"",
	"\n"
};

void set_mastery ( Character *ch, char buf )
{

	int cl = parse_class ( buf );
	if ( cl != CLASS_UNDEFINED )
		GET_MASTERY ( ch, cl ) = TRUE;

}
const char *class_group_name ( Character *ch )
{
	switch ( GET_CLASS ( ch ) )
	{
		case CLASS_MAGE:
		case CLASS_PRIEST:
		case CLASS_ESPER:
			return "Caster";
			break;
		case CLASS_HUNTER:
		case CLASS_WARRIOR:
			return "Fighter";
			break;
		case CLASS_GYPSY:
		case CLASS_RANGER:
		case CLASS_THIEF:
			return "Rogue";
			break;
		default:  return "Broken";
	}
}

const char *simple_class_name ( Character *ch )
{
	if ( IS_NPC ( ch ) )
	{
		return ( npc_class_types[ ( int ) GET_CLASS ( ch ) ] );
	}
	else
	{
		if ( !PLR_FLAGGED ( ch, PLR_NEEDS_CLASS ) )
			return ( pc_class_types[ ( int ) GET_CLASS ( ch ) ] );
		else
			return ( class_group_name ( ch ) );

	}
}

/* The menu for choosing a class in interpreter.c: */
#if 0
const char *class_menu =
    "\r\n"
    "Select a class:\r\n"
    "---CLASS-----------STRENGTHS----------WEAKNESSES--------\r\n"
    "  [P]riest   ::   +Light  +Spirit    -Dark   -Death \r\n"
    "  [T]hief    ::   +Dark   +Death     -Spirit -Earth \r\n"
    "  [W]arrior  ::   +Death  +Air       -Dark   -Fire \r\n"
    "  [M]age     ::   +Fire   +Ice       -Mind   -Spirit\r\n"
    "  [H]unter   ::   +Earth  +Ice       -Fire   -Electric \r\n"
    "  [R]anger   ::   +Earth  +Air       -Mind   -Water \r\n"
    "  [G]ypsy    ::   +Death  +Fire      -Water  -Light \r\n"
    "  [E]sper    ::   +Mind   +Spirit    -Earth  -Ice \r\n";
#else
const char *class_menu =
    "\r\n"
    "{cySelect a class (more info given):\r\n"
    "---CLASS-----{c0\r\n"
    "  {cg[{cGP{cg]riest   \r\n"
    "  {cg[{cGT{cg]hief    \r\n"
    "  {cg[{cGW{cg]arrior  \r\n"
    "  {cg[{cGM{cg]age     \r\n"
    "  {cg[{cGH{cg]unter   \r\n"
    "  {cg[{cGR{cg]anger   \r\n"
    "  {cg[{cGG{cg]ypsy    \r\n"
    "  {cg[{cGE{cg]sper    \r\n{c0";
#endif
const char *class_group_menu =
    "\r\n"
    "{cySelect a class (more info given):\r\n"
    "---CLASS-----{c0\r\n"
    "  {cg[{cGC{cg]aster   \r\n"
    "  {cg[{cGF{cg]ighter    \r\n"
    "  {cg[{cGR{cg]ogue  \r\n{c0";

int class_elem_strength ( int chcl )
{
	switch ( chcl )
	{
		case CLASS_MAGE:    return E_FIRE  | E_ICE;
		case CLASS_PRIEST:  return E_ELEC  | E_SPIRIT;
		case CLASS_WARRIOR: return E_DEATH | E_AIR;
		case CLASS_THIEF:   return E_DARK  | E_DEATH;
		case CLASS_HUNTER:  return E_EARTH | E_ICE;
		case CLASS_RANGER:  return E_EARTH | E_AIR;
		case CLASS_GYPSY:   return E_DEATH | E_ELEC;
		case CLASS_ESPER:   return E_MIND  | E_SPIRIT;
		default:  return 0;

	}
}
int class_elem_weakness ( int chcl )
{
	switch ( chcl )
	{
		case CLASS_MAGE:    return E_MIND   | E_SPIRIT;
		case CLASS_PRIEST:  return E_DARK   | E_DEATH;
		case CLASS_WARRIOR: return E_DARK   | E_FIRE;
		case CLASS_THIEF:   return E_SPIRIT | E_EARTH;
		case CLASS_HUNTER:  return E_FIRE   | E_ELEC;
		case CLASS_RANGER:  return E_MIND   | E_WATER;
		case CLASS_GYPSY:   return E_WATER  | E_LIGHT;
		case CLASS_ESPER:   return E_EARTH  | E_ICE;
		default:  return 0;

	}
}

/*
 * The code to interpret a class letter -- used in interpreter.c when a
 * new character is selecting a class and by 'set class' in act.wizard.c.
 */

int parse_class ( char arg )
{
	arg = LOWER ( arg );

	switch ( arg )
	{
		case 'm': return CLASS_MAGE;
		case 'p': return CLASS_PRIEST;
		case 'w': return CLASS_WARRIOR;
		case 't': return CLASS_THIEF;
		case 'h': return CLASS_HUNTER;
		case 'r': return CLASS_RANGER;
		case 'g': return CLASS_GYPSY;
		case 'e': return CLASS_ESPER;
		default:  return CLASS_UNDEFINED;

	}
}

int parse_class_group ( char arg )
{
	arg = LOWER ( arg );

	switch ( arg )
	{
		case 'c': return CLASS_MAGE;
		case 'f': return CLASS_WARRIOR;
		case 'r': return CLASS_THIEF;
		default:  return CLASS_UNDEFINED;

	}
}

/*
 * bitvectors (i.e., powers of two) for each class, mainly for use in
 * do_who and do_users.  Add new classes at the end so that all classes
 * use sequential powers of two (1 << 0, 1 << 1, 1 << 2, 1 << 3, 1 << 4,
 * 1 << 5, etc.
 */

long find_class_bitvector ( char arg )
{
	arg = LOWER ( arg );

	switch ( arg )
	{
		case 'm': return ( 1 << CLASS_MAGE );
		case 'p': return ( 1 << CLASS_PRIEST );
		case 't': return ( 1 << CLASS_THIEF );
		case 'w': return ( 1 << CLASS_WARRIOR );
		case 'h': return ( 1 << CLASS_HUNTER );
		case 'r': return ( 1 << CLASS_RANGER );
		case 'g': return ( 1 << CLASS_GYPSY );
		case 'e': return ( 1 << CLASS_ESPER );
		default:  return ( 0 );
	}
}


/*
 * These are definitions which control the guildmasters for each class.
 *
 * The first field (top line) controls the highest percentage skill level
 * a character of the class is allowed to attain in any skill.  (After
 * this level, attempts to practice will say "You are already learned in
 * this area."
 *
 * The second line controls the maximum percent gain in learnedness a
 * character is allowed per practice -- in other words, if the random
 * die throw comes out higher than this number, the gain will only be
 * this number instead.
 *
 * The third line controls the minimu percent gain in learnedness a
 * character is allowed per practice -- in other words, if the random
 * die throw comes out below this number, the gain will be set up to
 * this number.
 *
 * The fourth line simply sets whether the character knows 'spells'
 * or 'skills'.  This does not affect anything except the message given
 * to the character when trying to practice (i.e. "You know of the
 * following spells" vs. "You know of the following skills"
 */

#define SPELL  0
#define SKILL  1

/* #define LEARNED_LEVEL 0  % known which is considered "learned" */
/* #define MAX_PER_PRAC       1  max percent gain in skill per practice */
/* #define MIN_PER_PRAC       2  min percent gain in skill per practice */
/* #define PRAC_TYPE          3  should it say 'spell' or 'skill'?    */

int prac_params[4][NUM_CLASSES] =
{
	/*   MAG    CLE    THE     WAR   HUN    RAN    GYP    ESP             */
	{   50,    50,    50,    50,    50,    50,    50,    50}, /*learned level */
	{   4,    4,     2,     2,     2,     2,    4,    4}, /* max per prac */
	{    2,     2,     1,     1,     1,     1,     2,    2 }, /* min per pac  */
	{SPELL, SPELL, SKILL, SKILL, SKILL, SKILL, SKILL, SPELL}  /* prac name    */
};


/*
 * ...And the appropriate rooms for each guildmaster/guildguard; controls
 * which types of people the various guildguards let through.  i.e., the
 * first line shows that from room 3017, only MAGIC_USERS are allowed
 * to go south.
 *
 * Don't forget to visit spec_assign.c if you create any new mobiles that
 * should be a guild master or guard so they can act appropriately. If you
 * "recycle" the existing mobs that are used in other guilds for your new
 * guild, then you don't have to change that file, only here.
 */
int guild_info[][3] =
{

	/* Midgaard */
	{CLASS_MAGE,          3017,   SCMD_SOUTH},
	{CLASS_PRIEST,        3147,   SCMD_SOUTH},
	{CLASS_PRIEST,        3147,   SCMD_NORTH},
	{CLASS_THIEF,         3027,   SCMD_EAST },
	{CLASS_WARRIOR,       3021,   SCMD_EAST },
	{CLASS_GYPSY,         3152,   SCMD_NORTH},
	{CLASS_HUNTER,         897,   SCMD_EAST },
	{CLASS_RANGER,        6498,   SCMD_EAST },
	{CLASS_ESPER,        10423,   SCMD_WEST },

	/* Brass Dragon */
	{-999 /* all */ ,    5065,     SCMD_WEST},
	{-999,                3006,   SCMD_WEST},

	/* this must go last -- add new guards above! */
	{-1, -1, -1}
};



/*
 * Saving throws for:
 * MCTW
 *   PARA, ROD, PETRI, BREATH, SPELL
 *     Levels 0-40
 *
 * Do not forget to change extern declaration in magic.c if you add to this.
 */

sbyte saving_throws ( int chclass_num, int type, int level )
{
	switch ( chclass_num )
	{
		case CLASS_MAGE:
		case CLASS_GYPSY:
			switch ( type )
			{
				case SAVING_PARA:   /* Paralyzation */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 70 - ( ( 70 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_ROD:    /* Rods */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 55 - ( ( 55 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_PETRI:  /* Petrification */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 65 - ( ( 65 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_BREATH: /* Breath weapons */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 75 - ( ( 75 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_SPELL:  /* Generic spells */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 60 - ( ( 60 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				default:
					log ( "SYSERR: Invalid saving throw type." );
					break;
			}
			break;
		case CLASS_PRIEST:
		case CLASS_ESPER:
			switch ( type )
			{
				case SAVING_PARA:   /* Paralyzation */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 80 - ( ( 80 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_ROD:    /* Rods */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 80 - ( ( 80 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_PETRI:  /* Petrification */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 75 - ( ( 75 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_BREATH: /* Breath weapons */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 80 - ( ( 80 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_SPELL:   /* Generic spells */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 75 - ( ( 75 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				default:
					log ( "SYSERR: Invalid saving throw type." );
					break;
			}
			break;
		case CLASS_THIEF:
		case CLASS_RANGER:
			switch ( type )
			{
				case SAVING_PARA:    /* Paralyzation */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 65 - ( ( 65 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_ROD:     /* Rods */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 70 - ( ( 70 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_PETRI:   /* Petrification */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 60 - ( ( 60 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_BREATH:  /* Breath weapons */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 80 - ( ( 80 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_SPELL:   /* Generic spells */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 75 - ( ( 75 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				default:
					log ( "SYSERR: Invalid saving throw type." );
					break;
			}
			break;
		case CLASS_WARRIOR:
		case CLASS_HUNTER:
			switch ( type )
			{
				case SAVING_PARA:    /* Paralyzation */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 70 - ( ( 70 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_ROD:     /* Rods */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 75 - ( ( 75 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_PETRI:   /* Petrification */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 75 - ( ( 75 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_BREATH:  /* Breath weapons */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 85 - ( ( 85 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				case SAVING_SPELL:   /* Generic spells */
					if ( level == 0 )
						return 100;
					if ( level >= LVL_IMMORT )
					{
						return 0;
					}
					else
					{
						return ( 85 - ( ( 85 * level ) / ( LVL_IMMORT - 1 ) ) );
					}
				default:
					log ( "SYSERR: Invalid saving throw type." );
					break;
			}
		default:
			log ( "SYSERR: Invalid class saving throw." );
			break;
	}

	/* Should not get here unless something is wrong. */
	return 100;
}

/* THAC0 for classes and levels.  (To Hit Armor Class 0)

  This truely is the dumbest unneeded function ever...

*/
int thaco ( int chclass_num, int level )
{
	if (level >= LVL_IMMORT) return 100;
	if (level == 0) return 0;
	return ((20*level) / (LVL_IMMORT - 1));

	//What follows is kept cause Horus says he'll need it. (2009 - Lets hope we don't find this comment back years from now)
	switch ( chclass_num )
	{
		case CLASS_MAGE:
		case CLASS_GYPSY:
			if ( level == 0 )
				return 100;
			if ( level >= LVL_IMMORT )
			{
				return 0;
			}
			else
			{
				return ( 20 - ( ( 20 * level ) / ( LVL_IMMORT - 1 ) ) );
			}
			break;
		case CLASS_PRIEST:
		case CLASS_ESPER:
			if ( level == 0 )
				return 100;
			if ( level >= LVL_IMMORT )
			{
				return 0;
			}
			else
			{
				return ( 20 - ( ( 20 * level ) / ( LVL_IMMORT - 1 ) ) );
			}
			break;
		case CLASS_THIEF:
		case CLASS_RANGER:
			if ( level == 0 )
				return 100;
			if ( level >= LVL_IMMORT )
			{
				return 0;
			}
			else
			{
				return ( 20 - ( ( 20 * level ) / ( LVL_IMMORT - 1 ) ) );
			}
			break;
		case CLASS_WARRIOR:
		case CLASS_HUNTER:
			if ( level == 0 )
				return 100;
			if ( level >= LVL_IMMORT )
			{
				return 0;
			}
			else
			{
				return ( 20 - ( ( 20 * level ) / ( LVL_IMMORT - 1 ) ) );
			}
			break;
		default:
			log ( "SYSERR: Unknown class in thac0 chart." );
	}

	/* Will not get there unless something is wrong. */
	return 100;
}

int add_fib ( int current, int to_add )
{
	int i, ret = current, pos = ( to_add > 0 ? 1 : -1 );
	int cost = 0;
	to_add *= pos;
	for ( i = 0; i < to_add; i++ )
	{
		if ( pos == -1 )
			ret += pos;

		cost += 1 + ( ret > 5 ) + ( ret > 9 ) + ( ret > 13 ) + ( ret > 15 ) + ( ret > 17 ) + ( ret > 19 ) + ( ret > 20 );

		if ( pos == 1 )
			ret += pos;
	}

	return ( cost * pos );
}

/* 1 if valid, 0 if invalid */
int choose_real_abils ( Character *ch, char selected, int amount )
{

	if ( ( CREATE_POINTS ( ch ) - amount ) < 0 )
		return 0;

	switch ( LOWER ( selected ) )
	{
		case 's':
			if ( ( ch->real_abils.str + amount ) < 0 )
				return ( 0 );
			if ( ( ch->real_abils.str + amount ) > MAX_MORTAL_BASE )
				return ( 0 );
			ch->real_abils.str += amount;
			break;
		case 'i':
			if ( ( ch->real_abils.intel + amount ) < 0 )
				return ( 0 );
			if ( ( ch->real_abils.intel + amount ) > MAX_MORTAL_BASE )
				return ( 0 );
			ch->real_abils.intel += amount;
			break;
		case 'w':
			if ( ( ch->real_abils.wis + amount ) < 0 )
				return ( 0 );
			if ( ( ch->real_abils.wis + amount ) > MAX_MORTAL_BASE )
				return ( 0 );
			ch->real_abils.wis += amount;
			break;
		case 'd':
			if ( ( ch->real_abils.dex + amount ) < 0 )
				return ( 0 );
			if ( ( ch->real_abils.dex + amount ) > MAX_MORTAL_BASE )
				return ( 0 );
			ch->real_abils.dex += amount;
			break;
		case 'o':
			if ( ( ch->real_abils.con + amount ) < 0 )
				return ( 0 );
			if ( ( ch->real_abils.con + amount ) > MAX_MORTAL_BASE )
				return ( 0 );
			ch->real_abils.con += amount;
			break;
		case 'c':
			if ( ( ch->real_abils.cha + amount ) < 0 )
				return ( 0 );
			if ( ( ch->real_abils.cha + amount ) > MAX_MORTAL_BASE )
				return ( 0 );
			ch->real_abils.cha += amount;
			break;
		default:
			return ( 0 );
	}
	CREATE_POINTS ( ch ) -= amount;
	return ( 1 );
}

/*
 * Roll the 6 stats for a character... each stat is made of the sum of
 * the best 3 out of 4 rolls of a 6-sided die.  Each class then decides
 * which priority will be given for the best to worst stats.
 */
void roll_real_abils ( Character * ch )
{
	int i, j, k, temp;
	ubyte table[6];
	ubyte rolls[4];

	for ( i = 0; i < 6; i++ )
		table[i] = 0;

	for ( i = 0; i < 6; i++ )
	{

		for ( j = 0; j < 4; j++ )
			rolls[j] = number ( 1, 6 );

		temp = rolls[0] + rolls[1] + rolls[2] + rolls[3] -
		       MIN ( rolls[0], MIN ( rolls[1], MIN ( rolls[2], rolls[3] ) ) );

		for ( k = 0; k < 6; k++ )
			if ( table[k] < temp )
			{
				temp ^= table[k];
				table[k] ^= temp;
				temp ^= table[k];
			}
	}

	ch->real_abils.str_add = 0;

	switch ( GET_CLASS ( ch ) )
	{
		case CLASS_MAGE:
			ch->real_abils.intel = table[0];
			ch->real_abils.wis = table[1];
			ch->real_abils.dex = table[2];
			ch->real_abils.str = table[3];
			ch->real_abils.con = table[4];
			ch->real_abils.cha = table[5];
			break;
		case CLASS_PRIEST:
			ch->real_abils.wis = table[0];
			ch->real_abils.intel = table[1];
			ch->real_abils.str = table[2];
			ch->real_abils.dex = table[3];
			ch->real_abils.con = table[4];
			ch->real_abils.cha = table[5];
			break;
		case CLASS_THIEF:
			ch->real_abils.dex = table[0];
			ch->real_abils.str = table[1];
			ch->real_abils.con = table[2];
			ch->real_abils.intel = table[3];
			ch->real_abils.wis = table[4];
			ch->real_abils.cha = table[5];
			break;
		case CLASS_WARRIOR:
			ch->real_abils.str = table[0];
			ch->real_abils.dex = table[1];
			ch->real_abils.con = table[2];
			ch->real_abils.wis = table[3];
			ch->real_abils.intel = table[4];
			ch->real_abils.cha = table[5];
			if ( ch->real_abils.str == 18 )
				ch->real_abils.str_add = number ( 0, 100 );
			break;
		case CLASS_HUNTER:
			ch->real_abils.str = table[0];
			ch->real_abils.wis = table[1];
			ch->real_abils.dex = table[2];
			ch->real_abils.intel = table[3];
			ch->real_abils.con = table[4];
			ch->real_abils.cha = table[5];
			break;
		case CLASS_RANGER:
			ch->real_abils.dex   = table[0];
			ch->real_abils.wis   = table[1];
			ch->real_abils.intel = table[2];
			ch->real_abils.str   = table[3];
			ch->real_abils.con   = table[4];
			ch->real_abils.cha   = table[5];
		case CLASS_GYPSY:
			ch->real_abils.intel = table[0];
			ch->real_abils.dex = table[1];
			ch->real_abils.wis = table[2];
			ch->real_abils.str = table[3];
			ch->real_abils.con = table[4];
			ch->real_abils.cha = table[5];
			break;
		case CLASS_ESPER:
			ch->real_abils.wis = table[0];
			ch->real_abils.intel =  table[1];
			ch->real_abils.str = table[2];
			ch->real_abils.dex = table[3];
			ch->real_abils.con = table[4];
			ch->real_abils.cha = table[5];
			break;
	}

	ch->aff_abils = ch->real_abils;

}

void race_abils ( Character *ch )
{
	switch ( GET_RACE ( ch ) )
	{
		case RACE_INDIAN:
		case RACE_GRINGO:
		case RACE_GLADIATOR:
			break;
		case RACE_FAUN:
			ch->real_abils.intel -= 1;
			ch->real_abils.str -= 1;
			ch->real_abils.cha += 1;
			ch->real_abils.dex += 1;
			break;
		case RACE_CENTAUR:
			ch->real_abils.intel -= 1;
			ch->real_abils.wis -= 1;
			ch->real_abils.str += 1;
			ch->real_abils.con += 1;
			break;
		case RACE_ELF:
			ch->real_abils.str -= 1;
			ch->real_abils.con -= 1;
			ch->real_abils.wis += 1;
			ch->real_abils.intel += 1;
			break;
		case RACE_DWARF:
			ch->real_abils.intel -= 1;
			ch->real_abils.str += 1;
			ch->real_abils.dex -= 1;
			ch->real_abils.con += 1;
			break;
		case RACE_MARTIAN:
			ch->real_abils.intel += 1;
			ch->real_abils.str -= 1;
			ch->real_abils.con -= 1;
			ch->real_abils.dex += 1;
			break;
		case RACE_SPACE_WOLF:
			ch->real_abils.wis -= 1;
			ch->real_abils.str += 1;
			ch->real_abils.dex -= 1;
			ch->real_abils.con += 1;
			break;
	}
}

/* Some initializations for characters, including initial skills */
void do_start ( Character * ch )
{
	GET_LEVEL ( ch ) = 1;
	GET_EXP ( ch ) = 1;

	set_title ( ch, NULL );
	ch->points.max_hit = 10;

	//Another test -> Prom
	GET_MAX_MOVE(ch) = 250;

	advance_level ( ch );

	GET_HIT ( ch ) = ( GET_MAX_HIT ( ch ) );
	GET_MANA ( ch ) = ( GET_MAX_MANA ( ch ) );
	GET_MOVE ( ch ) = ( GET_MAX_MOVE ( ch ) );
	GET_STAMINA ( ch ) = ( GET_MAX_STAMINA ( ch ) );

	GET_COND ( ch, THIRST ) = 24;
	GET_COND ( ch, FULL ) = 24;
	GET_COND ( ch, DRUNK ) = -1;

	ch->check_regen_rates();

	ch->player.time.played = 0;
	ch->player.time.logon = time ( 0 );

	SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_AUTOEXIT );
	SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_COLOUR_1 );
	SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_COLOUR_2 );

	SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_DISPHP );
	SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_DISPMANA );
	SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_DISPMOVE );

	if ( CONFIG_SITEOK_ALL )
		SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_SITEOK );
	ch->player_specials->saved.olc_zone = NOWHERE;
}


#define TIERS num_melee_tier(ch)+1
/*
 * This function controls the change to maxmove, maxmana, and maxhp for
 * each class every time they gain a level.
 */
void advance_level ( Character * ch , bool silent )
{
	float add_hp = 0, add_mana = 0, add_move = 0;
	unsigned i;
	int num_melee_tier ( Character *ch );
	int level = GET_LEVEL ( ch ), casttier = num_casting ( ch ), fightier = TIERS;
	int remorts = MIN(REMORTS(ch), 50);


	add_hp += ( con_app[GET_CON ( ch ) ].hitp * current_class_is_tier_num ( ch ) );
	//add_hp += MIN ( REMORTS ( ch ), 25 );
	if ( remorts > 5 )
		add_hp += ( remorts / 6 );
	add_mana = MIN ( remorts, 25 );
	add_move = ( MIN ( remorts, 25 ) * 0.5 );

	switch ( GET_CLASS ( ch ) )
	{

		case CLASS_MAGE:
		case CLASS_PRIEST:
		case CLASS_ESPER:
			add_hp += number ( FTOI ( 2 + ( GET_CHA ( ch ) * 0.25 ) ), FTOI ( ( 8 * fightier ) + ( GET_CHA ( ch ) * 0.35 ) + 4 ) );
			add_mana += number ( level, ( ( casttier * level ) ) ) + 2;
			add_move += number ( 1, ( 2 * fightier ) );
			break;

		case CLASS_THIEF:
			// Tweaking this formula from 11 * TIERS to 13 * tiers.
			// This is a WIP -> Prometheus
			add_hp += number ( ( 13 * TIERS ), ( 15 * TIERS ) ) + ( GET_CHA ( ch ) /10 );
			add_mana += num_casting ( ch );
			add_move += number ( 2 * TIERS, 4 * TIERS );
			break;

		case CLASS_GYPSY:
			add_hp += number ( ( 11 * TIERS ), ( 14 * TIERS ) ) + ( GET_CHA ( ch ) /10 );
			add_mana += number ( FTOI ( GET_LEVEL ( ch ) * 0.34 ), FTOI ( ( 1.5 * GET_LEVEL ( ch ) ) ) ) + num_casting ( ch );
			add_move += number ( 2 * TIERS, 4 * TIERS );
			break;

		case CLASS_RANGER:
			add_hp += number ( ( 14 * TIERS ), ( 17 * TIERS ) ) + ( GET_CHA ( ch ) /10 );
			add_mana += num_casting ( ch );
			add_move += number ( 2 * TIERS, 4 * TIERS );
			break;

		case CLASS_WARRIOR:
			add_hp += number ( ( 15 * TIERS ), ( 20 * TIERS ) ) + ( GET_CHA ( ch ) /10 );
                        add_mana +=  num_casting ( ch );
                        add_move += number ( 2 * TIERS, 4 * TIERS );
                        break;

		case CLASS_HUNTER:
			add_hp += number ( ( 15 * TIERS ), ( 18 * TIERS ) ) + ( GET_CHA ( ch ) /10 );
			add_mana +=  num_casting ( ch );
			add_move += number ( 2 * TIERS, 4 * TIERS );
			break;

	}
	if ( !silent )
	{
		switch ( GET_LEVEL ( ch ) )
		{
			case 5:
				if ( remorts == 0 )
					ch->Send ( "{cRYou will now start to feel hungry and thirsty.\r\n"
					           "So remember to eat and drink.\r\n{c0" );
				if ( remorts < 2 )
				{
					ch->Send ( "{cYYou gain 50000 coins to your bank!{c0\r\n" );
					ch->Gold ( 50000, GOLD_BANK );
				}
				break;
			case 11:
			case 18:
			case 6:
				if ( remorts == 0 && ( PLR_FLAGGED ( ch, PLR_NEEDS_CLASS ) || PLR_FLAGGED ( ch, PLR_NEEDS_STATS ) ) )
					ch->Send ( "{cRYou can choose a class to be at any time till level 20.{c0\r\nTo choose your class, and then your stats, type: {cGchoose class{c0 then {cGchoose stats{c0" );

				break;
			case 10:
				if ( remorts < 3 )
				{
					if ( remorts == 0 )
						ch->Send ( "{cRYou will no longer be immune to death.\r\n"
						           "When you die from now on you will \r\n"
						           "need to retrieve your corpse.\r\n{c0" );
					ch->Send ( "{cYYou gain 100000 coins to your bank!{c0\r\n" );
					ch->Send ( "{cYYou gain 10 bonus practice sessions!{c0\r\n" );
					GET_PRACTICES ( ch ) += 10;
					ch->Gold ( 100000, GOLD_BANK );

				}
				break;
			case 15:
				if ( remorts < 4 )
				{
					ch->Send ( "{cYYou gain 1000000 coins to your bank!{c0\r\n" );
					ch->Send ( "{cYYou gain 10 bonus practice sessions!{c0\r\n" );
					GET_PRACTICES ( ch ) += 10;
					ch->Gold ( 1000000, GOLD_BANK );
				}
				break;
			case 20:
				if ( remorts < 5 )
				{
					ch->Send ( "{cYYou gain 5000000 coins to your bank!{c0\r\n" );
					ch->Send ( "{cYYou gain 10 bonus practice sessions!{c0\r\n" );
					GET_PRACTICES ( ch ) += 10;
					ch->Gold ( 5000000, GOLD_BANK );
				}
				break;
			case 30:
				if ( remorts < 6 )
				{
					ch->Send ( "{cYYou gain 10000000 coins to your bank!{c0\r\n" );
					ch->Send ( "{cYYou gain 10 bonus practice sessions!{c0\r\n" );
					GET_PRACTICES ( ch ) += 10;
					ch->Gold ( 10000000, GOLD_BANK );
				}
				break;
			case 45:
				if ( remorts < 7 )
				{
					ch->Send ( "{cYYou gain 25000000 coins to your bank!{c0\r\n" );
					ch->Send ( "{cYYou gain 10 bonus practice sessions!{c0\r\n" );
					GET_PRACTICES ( ch ) += 20;
					ch->Gold ( 25000000, GOLD_BANK );
				}
				break;
		}
	}

	GET_MAX_HIT ( ch ) += MAX ( 1, FTOI ( add_hp ) );
	GET_MAX_MOVE ( ch ) += MAX ( 1, FTOI ( add_move ) );

	if ( GET_LEVEL ( ch ) > 1 )
		GET_MAX_MANA ( ch ) += FTOI ( add_mana );

	GET_MAX_STAMINA ( ch ) += current_class_is_tier_num ( ch ) + ( GET_DEX ( ch ) > 20 );

        if (remorts > 35) {
            if ((GET_LEVEL(ch) % 5) == 0)
                GET_PRACTICES(ch) += 1;
        }
        else {
	    if ( IS_MAGE ( ch ) || IS_PRIEST ( ch ) || IS_ESPER ( ch ) )
		GET_PRACTICES ( ch ) += MAX ( 2, ( int ) wis_app[GET_WIS ( ch ) ].bonus );
	    else
		GET_PRACTICES ( ch ) += MIN ( 2, MAX ( 1, ( int ) wis_app[GET_WIS ( ch ) ].bonus ) );
        }

	if ( GET_LEVEL ( ch ) >= LVL_GOD )
	{
		for ( i = 0; i < 3; i++ )
			GET_COND ( ch, i ) = ( char )-1;
		SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_HOLYLIGHT );
	}
	else
		REMOVE_BIT_AR ( PRF_FLAGS ( ch ), PRF_HOLYLIGHT );

	GET_HIT ( ch ) = ( GET_MAX_HIT ( ch ) );
	GET_MANA ( ch ) = ( GET_MAX_MANA ( ch ) );
	GET_MOVE ( ch ) = ( GET_MAX_MOVE ( ch ) );
	GET_STAMINA ( ch ) = ( GET_MAX_STAMINA ( ch ) );

	ch->check_regen_rates();

}


/*
 * This simply calculates the backstab multiplier based on a character's
 * level.  This used to be an array, but was changed to be a function so
 * that it would be easier to add more levels to your MUD.  This doesn't
 * really create a big performance hit because it's not used very often.
 */
float backstab_mult ( int level, int tier )
{
	float mt = 1.75f;
	if ( level > 50 )
		mt += 0.5f;
	switch ( tier )
	{
		case 1:
			mt += 0.35f;
			break;
		case 2:
			mt += 0.85f;
			break;
		case 3:
			mt += 1.35f;
			break;
		case 4:
			mt += 1.85f;
			break;
	}
	return mt;

}

float slit_mult ( int level, int tier )
{
        float mt = 1.50f;
        if ( level > 50 )
                mt += 0.5f;
        switch ( tier )
        {
                case 1:
                        mt += 0.35f;
                        break;
                case 2:
                        mt += 0.85f;
                        break;
                case 3:
                        mt += 1.35f;
                        break;
                case 4:
                        mt += 1.85f;
                        break;
        }
        return mt;

}

float cleave_mult ( int level, int tier )
{

	switch ( tier )
	{
		default:
			return 1.1f + ( level < 10 );
		case 1:
			return 1.55f + ( level < 10 );
		case 2:
			return 1.7f + ( level < 10 );
		case 3:
			return 1.9f + ( level < 10 );
		case 4:
			return 2.0f + ( level < 10 );
	}
}


ACMD ( do_dam_dice )
{
	int i, j;
	char num1[10], num2[10];
	skip_spaces ( &argument );
	if ( *argument || argument != NULL )
	{
		two_arguments ( argument, num1, num2 );
		i = atoi ( num1 );
		j = atoi ( num2 );
		ch->Send ( "%2dD%-2d avg %.1f max %3d   ", i, j,
		           ( ( ( j + 1 ) / 2.0 ) * i ), /*avg dam*/ ( i + ( i * j ) ) /*max damn*/ );
		return;
	}
	ch->Send ( "Dam Dice:\r\n" );
	for ( i = 0; i< 10; i++ )
		for ( j = 0; j < 10; j++ )
			ch->Send ( "%2dD%-2d avg %.1f max %3d   %s", i, j,
			           ( ( ( j + 1 ) / 2.0 ) * i ), //avg dam
			           ( i + ( i * j ) ), //max damn
			           ( i%2?"  ":" \r\n" ) );

}

/*
 * invalid_class is used by handler.c to determine if a piece of equipment is
 * usable by a particular class, based on the ITEM_ANTI_{class} bitvectors.
 */
int invalid_class ( Character *ch, struct obj_data *obj )
{
	if ( ( IS_OBJ_STAT ( obj, ITEM_ANTI_MAGE )    && IS_MAGE ( ch ) )    ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_PRIEST )  && IS_PRIEST ( ch ) )  ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_WARRIOR ) && IS_WARRIOR ( ch ) ) ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_THIEF )   && IS_THIEF ( ch ) )   ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_GYPSY )   && IS_GYPSY ( ch ) )   ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_HUNTER )  && IS_HUNTER ( ch ) )  ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_RANGER )  && IS_RANGER ( ch ) )  ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_ESPER )   && IS_ESPER ( ch ) )   ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_MALE )    && GET_SEX ( ch ) == SEX_MALE ) ||
	        ( IS_OBJ_STAT ( obj, ITEM_ANTI_FEMALE )  && GET_SEX ( ch ) == SEX_FEMALE ) )
		return ( 1 );
	else
		return ( 0 );
}

/*
 * SPELLS AND SKILLS.  This area defines which spells are assigned to
 * which classes, and the minimum level the character must be to use
 * the spell or skill.
 */
#define ALL_CLASSES (MAG | PRI | WAR | ESP | RAN | HUN | GYP | THI )
#define ALL_ROGUE   (THI | RAN | GYP)
#define ALL_CASTERS  (MAG | PRI | ESP)
#define ALL_FIGHTER (WAR | HUN)

void init_spell_levels ( void )
{
	assign_class ( SKILL_RIDING, ALL_CLASSES );
	assign_class ( SKILL_MOUNT, ALL_CLASSES );
	assign_class ( SKILL_DRUNK, ALL_CLASSES );


	assign_class ( SPELL_ANIMATE_DEAD,      PRI );
	assign_class ( SPELL_MAGIC_MISSILE,    ALL_CASTERS );
	assign_class ( SKILL_MANIFEST,         ALL_CASTERS);
	assign_class ( SPELL_DETECT_INVIS,     MAG | ESP | RAN );
	assign_class ( SPELL_DETECT_MAGIC,     ALL_CASTERS );
	assign_class ( SPELL_MIND_WATER,       ALL_CASTERS );
	assign_class ( SPELL_MIND_ICE,         PRI | MAG );
	assign_class ( SPELL_MIND_ELEC,        PRI | ESP );
	assign_class ( SPELL_MIND_FIRE,        MAG );
	assign_class ( SPELL_CHILL_TOUCH,      ALL_CASTERS );
	assign_class ( SPELL_INVISIBLE,        ALL_CASTERS );
	assign_class ( SPELL_SHIELD,           ESP | PRI );
	assign_class ( SPELL_ACID_ARROW,       MAG );
	assign_class ( SPELL_GATE,           ESP | MAG );
	assign_class ( SPELL_BURNING_HANDS,    MAG | PRI );
	assign_class ( SPELL_LOCATE_OBJECT,    ESP | GYP | RAN );
	assign_class ( SPELL_STRENGTH,         PRI | ESP );
	assign_class ( SPELL_FLAME_ARROW,      MAG | ESP );
	assign_class ( SPELL_METEOR_SHOWER,    MAG );
	assign_class ( SPELL_SLEEP,            ESP | GYP );
	assign_class ( SPELL_BLINDNESS,        MAG );
	assign_class ( SPELL_FACEMELT,         MAG );
	assign_class ( SPELL_DETECT_POISON,    ALL_CASTERS );
	assign_class ( SPELL_WORD_OF_RECALL,   ALL_CASTERS );
	assign_class ( SPELL_SHOCKING_GRASP,   ESP | PRI );
	assign_class ( SPELL_LIGHTNING_BOLT,   PRI );
	assign_class ( SPELL_POISON,           ESP | HUN | RAN | GYP );
	assign_class ( SPELL_CURSE,            GYP | PRI );
	assign_class ( SPELL_FORTIFY_BODY,     PRI | ESP | GYP );
	assign_class ( SPELL_BURNINGSKULL,     ESP );
	assign_class ( SPELL_COLOUR_SPRAY,      MAG );
	assign_class ( SPELL_ENERGY_DRAIN,     ESP );
	assign_class ( SPELL_FIREBALL,         MAG );
	assign_class ( SPELL_CHARM,            ESP | GYP );
	assign_class ( SPELL_GROUP_RECALL,     ALL_CASTERS );
	assign_class ( SPELL_WATERWALK,           PRI | HUN );
	assign_class ( SPELL_MAGIC_BUBBLE,     ESP | GYP );
	assign_class ( SPELL_PROT_FIRE,           PRI | MAG | GYP );
	assign_class ( SPELL_PROT_COLD,           MAG | HUN | RAN | ESP );
	assign_class ( SPELL_SHIELD_MIRROR,    MAG | PRI );
	assign_class ( SPELL_BATTLE_RAGE,      MAG );
	assign_class ( SPELL_ENCHANT_WEAPON,   MAG | GYP );
	assign_class ( SPELL_FORTIFY_MIND,     MAG | ESP );
	assign_class ( SPELL_HASTE,          MAG | ESP );
	assign_class ( SPELL_KNOCK,          PRI );
	assign_class ( SPELL_DEMONSHRIEK,      ESP );
	assign_class ( SPELL_SHIELD_ICE,      MAG | PRI | ESP | RAN );
	assign_class ( SPELL_CLONE,            ESP | PRI );
	//assign_class ( SPELL_MINOR_IDENTIFY,   ESP | GYP | PRI );
	assign_class ( SPELL_AIR_ELEMENTAL,    MAG );
	assign_class ( SPELL_STEELSKIN,           PRI | ESP );
	assign_class ( SPELL_FLIGHT,           MAG );
	assign_class ( SKILL_SCRIBE,         ALL_CASTERS );
	assign_class ( SPELL_SLOW,             MAG | ESP );
	assign_class ( SPELL_FIRE_ELEMENTAL,   MAG );
	assign_class ( SPELL_SHIELD_MANA,      MAG );
	assign_class ( SPELL_MANA_TRANSFER,    ALL_CASTERS );
	assign_class ( SPELL_TELEPORT,            ALL_CASTERS );
	assign_class ( SPELL_RECHARGE,            ALL_CASTERS );
	assign_class ( SPELL_HOLD_PERSON,      ESP );
	assign_class ( SPELL_WATER_ELEMENTAL,  PRI );
	assign_class ( SPELL_CHAIN_LIGHTNING,  PRI );
	assign_class ( SPELL_MANA_BLAST,       ESP | PRI );
	assign_class ( SPELL_GROUP_SHIELD,     PRI | ESP );
	assign_class ( SPELL_EARTH_ELEMENTAL,  ESP );
	assign_class ( SPELL_INFERNO,          MAG );
	assign_class ( SPELL_CURE_LIGHT,        PRI );
	assign_class ( SPELL_ARMOR,             PRI | ESP );
	assign_class ( SPELL_CREATE_FOOD,       PRI | ESP );
	assign_class ( SPELL_CREATE_WATER,      PRI | MAG );
	assign_class ( SPELL_SOULSMASH,         PRI );
	assign_class ( SPELL_PROT_FROM_GOOD,    PRI );
	assign_class ( SPELL_DETECT_ALIGN,      PRI | ESP );
	assign_class ( SPELL_CURE_BLIND,        PRI | HUN );
	assign_class ( SPELL_WEAKEN,       PRI );
	assign_class ( SPELL_BLESS,             PRI );
	assign_class ( SPELL_WATER_TO_WINE,     PRI );
	assign_class ( SPELL_INFRAVISION,       ALL_CASTERS | HUN );
	assign_class ( SPELL_PROT_FROM_EVIL,    PRI );
	assign_class ( SPELL_GROUP_ARMOR,       PRI );
	assign_class ( SPELL_CURE_CRITIC,       PRI | HUN );
	assign_class ( SPELL_SUMMON,            PRI );
	assign_class ( SPELL_ANTIDOTE_1,        PRI | HUN );
	assign_class ( SPELL_SHIELD_THORN,      PRI );
	assign_class ( SPELL_EARTHQUAKE,        PRI );
	assign_class ( SPELL_CONE_OF_COLD, PRI | ESP );
	assign_class ( SPELL_DISPEL_EVIL,       PRI );
	assign_class ( SPELL_DISPEL_GOOD,       ESP );
	assign_class ( SPELL_SANCTUARY,         PRI );
	assign_class ( SPELL_HOLY_WORD,         PRI );
	assign_class ( SPELL_HOLY_SHOUT,   PRI );
	assign_class ( SPELL_CALL_LIGHTNING,    PRI | GYP );
	assign_class ( SPELL_HEAL,              PRI );
	assign_class ( SPELL_CONTROL_WEATHER,   ALL_CASTERS );
	assign_class ( SPELL_SENSE_LIFE,        PRI | HUN | GYP );
	assign_class ( SPELL_HARM,              PRI );
	assign_class ( SPELL_GROUP_HEAL,        PRI );
	assign_class ( SPELL_VITALIZE,     PRI | ESP | GYP );
	assign_class ( SPELL_REMOVE_CURSE,      PRI );
	assign_class ( SPELL_DISPELL_SANCTURY,  PRI | ESP );
	assign_class ( SPELL_SUFFOCATE,         ESP );
	assign_class ( SPELL_ENCHANT_ARMOR,     PRI | GYP );
	assign_class ( SPELL_PARALYZE,          ESP );
	assign_class ( SKILL_BREW,         PRI );
	assign_class ( SPELL_ABSOLVE,      PRI );
	assign_class ( SPELL_DIVINE_MIND,       PRI | ESP );
	assign_class ( SPELL_LIFE_TRANSFER,     ESP );
	assign_class ( SPELL_ELECTRIC_BLAST,    PRI );
	assign_class ( SPELL_SHIELD_HOLY,       PRI );
	assign_class ( SKILL_HANDTOHAND,   ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_FLANK,        THI );
	assign_class ( SKILL_SLIP,         THI );
	assign_class ( SKILL_MANIPULATE,   MAG | GYP );
	assign_class ( SKILL_MEDITATE,          ALL_FIGHTER );
	assign_class ( SKILL_HOLY_STRENGTH,     MAG | PRI );
	assign_class ( SPELL_LIFESUCK,          MAG );
	assign_class ( SPELL_HEARTSQUEEZE,      MAG );
	assign_class ( SPELL_FIRE_SHIELD,  MAG );
	assign_class ( SPELL_CORRUPT_ARMOR,     MAG );
	assign_class ( SPELL_REMOVE_ALIGNMENT,  MAG );
	assign_class ( SPELL_STONESKIN,    MAG );
	assign_class ( SKILL_SNEAK,        ALL_ROGUE );
	assign_class ( SKILL_THROW,        ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_PICK_LOCK,    THI | RAN );
	assign_class ( SKILL_BACKSTAB,     THI | RAN );
	assign_class ( SKILL_TAME,         ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_STEAL,        ALL_ROGUE );
	assign_class ( SKILL_FIREARM,      ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_HIDE,         THI | RAN | HUN );
	assign_class ( SKILL_SLING,        ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_TRACK,        ALL_ROGUE | HUN );
	assign_class ( SKILL_SNARE,        GYP | HUN | THI );
	assign_class ( SKILL_TRAP_AWARE,   THI | HUN );
	assign_class ( SKILL_CROSSBOW,          ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_DISARM,       THI | ALL_FIGHTER );
	assign_class ( SKILL_SMASH,        ALL_FIGHTER );
	assign_class ( SKILL_CIRCLE,       THI );
	assign_class ( SKILL_BLACKJACK,    THI | GYP );
	assign_class ( SKILL_PUSH,         ALL_FIGHTER );
	assign_class ( SKILL_POISON_WEAPON,     THI );
	assign_class ( SKILL_MOUNTED_COMBAT,     ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_TRAMPLE,            ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_JOUST,              ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_GRAPPLE,            ALL_ROGUE | ALL_FIGHTER );
	assign_class ( SKILL_MELEE,        ALL_FIGHTER | ALL_ROGUE );
	assign_class ( SKILL_KICK,              ALL_FIGHTER | ALL_ROGUE ); // Ran -> all thief classes - Prom
	assign_class ( SKILL_RESCUE,            WAR );
	assign_class ( SKILL_BASH,              ALL_FIGHTER );
	assign_class ( SKILL_SECOND_ATTACK,     ALL_FIGHTER | ALL_ROGUE );
	assign_class ( SKILL_BOW,          RAN | HUN );
	assign_class ( SKILL_DUAL,              ALL_FIGHTER | ALL_ROGUE );
	assign_class ( SKILL_RETREAT,      WAR | RAN );
	assign_class ( SKILL_PARRY,        ALL_FIGHTER );
	assign_class ( SKILL_THIRD_ATTACK,      ALL_FIGHTER );
	assign_class ( SKILL_HAMSTRING,     WAR | HUN );
	assign_class ( SKILL_SHORT_BLADE,      ALL_ROGUE );
	assign_class ( SKILL_DODGE,        THI | RAN );
	assign_class ( SKILL_PHASE,        ESP | ALL_ROGUE );
	assign_class ( SKILL_CHARGE,       WAR );
	assign_class ( SKILL_GRIP,         WAR );
	assign_class ( SKILL_FACE,         THI | HUN );
	assign_class ( SKILL_STRANGLE,          THI );
	assign_class ( SKILL_FOCUS,        GYP | RAN );
	assign_class ( SKILL_MARTIAL_ARTS, ALL_ROGUE );
	assign_class ( SKILL_BESERK,       ALL_FIGHTER );
	assign_class ( SKILL_TRUE_STRIKE,  WAR | RAN );
	assign_class ( SKILL_FORTIFY,      ALL_FIGHTER );
	assign_class ( SKILL_SCALP,        GYP | RAN | HUN );
	assign_class ( SKILL_BLADE_DANCE,  GYP | THI | HUN );
	assign_class ( SKILL_LONGARM,       ALL_FIGHTER | RAN );
	assign_class ( SKILL_CLEAVE,       WAR );
	assign_class ( SKILL_BEHEAD,       HUN );
	assign_class ( SKILL_BRACE,        ALL_FIGHTER );
	assign_class ( SKILL_FILET,        HUN | GYP | RAN | THI );
	assign_class ( SKILL_FORAGE,       HUN | RAN | THI | GYP );
	assign_class ( SKILL_HYPERACTIVITY,     HUN | GYP );
	assign_class ( SKILL_SCAN,              RAN | HUN | THI );
	assign_class ( SKILL_SING_WOOD,          GYP );
	assign_class ( SKILL_TINKER,          GYP );
	assign_class ( SPELL_MIDAS_TOUCH,     GYP );
	assign_class ( SPELL_SWEET_DREAMS,    ESP );
	assign_class ( SPELL_NUMB_MIND,       ESP );
	assign_class ( SPELL_PSI_PANIC,       ESP );
	assign_class ( SPELL_FORSEE,          ESP | GYP );
	assign_class ( SPELL_SHIELD_STATIC,   GYP | ESP );
	assign_class ( SPELL_NIGHTMARE,       ESP );
	assign_class ( SPELL_CONFUSE,         ESP | MAG );
	assign_class ( SPELL_EVIL_EYE,        ESP );
	assign_class ( SKILL_THRUST,	      RAN );
	assign_class ( SKILL_SLIT,	      GYP );
	assign_class ( SPELL_ACID_HOLD,	      ESP | MAG );
	assign_class ( SPELL_ACIDBURST,      ESP );
	assign_class ( SPELL_FROST_ARROW,      MAG | PRI );
	assign_class ( SPELL_HAIL_STORM,      MAG );
}


gold_int group_exp_needed ( Character *ch )
{
	int remorts = MIN ( REMORTS ( ch ), 50 );
	if ( IS_NPC ( ch ) )
		return 0;
	else
		return ( gold_int ) ( ( level_exp ( GET_CLASS ( ch ), GET_LEVEL ( ch ) + 1, current_class_is_tier_num ( ch ), remorts ) * 0.2 ) ) - GET_GROUP_EXP ( ch );
}

gold_int exp_needed ( Character *ch )
{
	int remorts = MIN ( REMORTS ( ch ), 50 );
	if ( IS_NPC ( ch ) )
		return 0;
	else
		return ( level_exp ( GET_CLASS ( ch ), GET_LEVEL ( ch ) + 1, current_class_is_tier_num ( ch ), remorts ) - GET_EXP ( ch ) );
}

/* Function to return the exp required for each class/level/tier */
gold_int level_exp ( int chclass, int level, int tier, int remorts )
{
	gold_int multi, amount;
	if ( remorts > 50 )
		remorts = 50;
#define mod 17
	if ( level > MAX_MOB_LEVELS-1 || level < 0 )
	{
		log ( "SYSERR: Requesting exp for invalid level %d!", level );
		return 0;
	}
	multi = 1;
	if ( tier>0 ) multi = mult_2 ( tier );
	if ( tier>2 ) multi = mult_2 ( ( tier+1 ) );


	if ( level==1 )
	{
		return ( level );
	}
	else
	{
		float div_by = 0.5;
		amount = ( gold_int ) ( multi* ( ( mod+1 ) * ( mod+1 ) ) );

		amount = ( gold_int ) ( ( multi * ( ( ( ( ( level+mod ) * level ) * ( ( level+mod ) * level * level ) )-amount ) *0.5 ) ) );
		amount += ( gold_int ) ( ( ( remorts ) * ( amount*0.05 ) ) );
		if ( remorts == 0 )
			amount = ( gold_int ) ( amount * div_by );
		else if ( remorts < 5 )
			amount = ( gold_int ) ( amount * ( div_by + ( ( float ) remorts * 0.1 ) ) );
		return amount;
	}
#undef mod
}
/*

amount = (10 * (((3350) * (167500) - 3240)/2) = 2805608800
amount += 100 * (amount * 0.05)

*/

/*
 * Default titles of male characters.
 */
const char *title_male ( int chclass, int level )
{
	if ( level <= 0 || level > LVL_IMPL )
		return "the Man";
	if ( level == LVL_IMPL )
		return "the Implementor";

	switch ( chclass )
	{

		case CLASS_MAGE:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Warlock"; break;
				default: return "the Magic User"; break;
			}
			break;

		case CLASS_PRIEST:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Priest"; break;
				default: return "the Healer"; break;
			}
			break;

		case CLASS_THIEF:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Assassin"; break;
				default: return "the Thief"; break;
			}
			break;

		case CLASS_WARRIOR:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Warlord"; break;
				default: return "the Warrior"; break;
			}
			break;

		case CLASS_HUNTER:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Hunter"; break;
				default: return "the Hunter"; break;
			}
			break;

		case CLASS_GYPSY:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Gypsy"; break;
				default: return "the Gypsy"; break;
			}
			break;

		case CLASS_ESPER:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Esper"; break;
				default: return "the Esper"; break;
			}
			break;

		case CLASS_RANGER:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Ranger"; break;
				default: return "the Ranger"; break;
			}
			break;

	}

	/* Default title for classes which do not have titles defined */
	return "the Classless";
}


/*
 * Default titles of female characters.
 */
const char *title_female ( int chclass, int level )
{
	if ( level <= 0 || level > LVL_IMPL )
		return "the Woman";
	if ( level == LVL_IMPL )
		return "the Implementress";

	switch ( chclass )
	{

		case CLASS_MAGE:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Witch"; break;
				default: return "the Magic User"; break;
			}
			break;

		case CLASS_PRIEST:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Priestess"; break;
				default: return "the Healer"; break;
			}
			break;

		case CLASS_THIEF:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Assassin"; break;
				default: return "the Thief"; break;
			}
			break;

		case CLASS_WARRIOR:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Warlord"; break;
				default: return "the Warrior"; break;
			}
			break;

		case CLASS_HUNTER:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Hunteress"; break;
				default: return "the Hunteress"; break;
			}
			break;

		case CLASS_GYPSY:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Gypsy"; break;
				default: return "the Gypsy"; break;
			}
			break;

		case CLASS_ESPER:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Esper"; break;
				default: return "the Esper"; break;
			}
			break;

		case CLASS_RANGER:
			switch ( level )
			{
				case 51:
				case 52:
				case 53:
				case 54:
				case 55: return "the Immortal Ranger"; break;
				default: return "the Ranger"; break;
			}
			break;

	}

	/* Default title for classes which do not have titles defined */
	return "the Classless";
}

ACMD ( do_skilllist )
{
	int the_class;
	int i, j , k;
	char buf[MAX_INPUT_LENGTH];
	DYN_DEFINE;
	*buf = 0;

	skip_spaces ( &argument );

#define DISP "{c0Tier: {cy%d{c0 - Lev: {cG%2d{c0 - Skill or Spell: '{cC%s{c0'\r\n"

	if ( !*argument && GET_LEVEL ( ch ) < LVL_IMMORT )
	{
		ch->Send ( "Which class?\r\n" );
		return;
	}
	else if ( !*argument )
	{
		DYN_CREATE;
		*dynbuf = 0;
		for ( i = 0; i < MAX_SKILLS ; i++ )
		{
			if ( spell_info[i].classes == 0 )
			{
				snprintf ( buf, sizeof ( buf ), DISP ,spell_info[i].tier, spell_info[i].min_level, skill_name ( i ) );
				DYN_RESIZE ( buf );
			}
		}
	}
	else
	{

		the_class = parse_class ( *argument );
		if ( the_class == CLASS_UNDEFINED )
		{
			ch->Send ( "That isn't a proper class!\r\n" );
			return;
		}
		DYN_CREATE;
		*dynbuf = 0;

		for ( k = 0; k <= 4; k++ )
		{
			for ( j = 0; j <= 50; j++ )
			{
				for ( i = 0; i < MAX_SKILLS; i++ )
				{
					if ( !IS_SET ( spell_info[i].classes, ( 1 << the_class ) ) )
						continue;
					if ( spell_info[i].tier != k )
						continue;
					if ( spell_info[i].min_level != j )
						continue;

					snprintf ( buf, sizeof ( buf ), DISP ,spell_info[i].tier, spell_info[i].min_level, skill_name ( i ) );
					DYN_RESIZE ( buf );
				}
			}
		}
	}
	page_string ( ch->desc, dynbuf, DYN_BUFFER );

}

