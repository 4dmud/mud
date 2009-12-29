/* ************************************************************************
*   File: spell_parser.c                                Part of CircleMUD *
*  Usage: top-level magic routines; outside points of entry to magic sys. *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
/*
 * $Log: spell_parser.c,v $
 * Revision 1.42  2007/11/28 06:41:52  w4dimenscor
 * Changed Midas touch to be castable within combat
 *
 * Revision 1.41  2007/06/26 10:48:05  w4dimenscor
 * Fixed context in scripts so that it works again, changed mounted combat so that it is about 2/3rds player one third mount damage, updated the way skills get read using total_chance, stopped things with a PERC of 0 assisting, made it so that the ungroup command disbanded charmies
 *
 * Revision 1.40  2007/06/10 02:18:39  w4dimenscor
 * changed all entries in the code of 'color' to 'colour', but i now regret it.
 *
 * Revision 1.39  2007/06/07 22:28:53  w4dimenscor
 * fixed a crash bug in the code where it checked for if a target of a directional was able to hunt the victim
 *
 * Revision 1.38  2007/05/24 20:29:16  w4dimenscor
 * Last few fixes to hunting and directionals.
 * --Thotter
 *
 * Revision 1.37  2007/05/24 20:19:50  w4dimenscor
 * Directional spells don't hit everything in their path anymore.
 *
 * Also, the code will check whether or not you can see a target
 * before attacking it.
 * --Thotter
 *
 * Revision 1.36  2007/05/02 19:08:48  w4dimenscor
 * Spells won't think that they are directional when they are not
 * anymore.
 * -- Thotter
 *
 * Revision 1.35  2007/02/05 21:42:25  w4dimenscor
 * Fixed typo in magic fizzling to the room saying Catis instead of
 * Cati's. Hal. 02/05/07
 *
 * Revision 1.34  2006/09/19 10:56:17  w4dimenscor
 * fixed crash bug on extracting mob links
 *
 * Revision 1.33  2006/09/19 09:32:37  w4dimenscor
 * Have changed the movement code, and made some new functions for it.
 * Made it so that if your mount is sitting you can't move.
 * Fixed the problem with directional spells
 *
 * Revision 1.32  2006/09/15 08:01:12  w4dimenscor
 * Changed a large amount of send_to_char's to ch->Send and d->Output. fixed namechange command
 *
 * Revision 1.31  2006/08/31 10:39:17  w4dimenscor
 * Fixe dthe crash bug in medit. and also changed the mob proto list. there is still a memory leak in medit, which is being fixed now
 *
 * Revision 1.30  2006/08/25 10:22:44  w4dimenscor
 * added command to fix peoples skills back to the practiced amount they were at
 *
 * Revision 1.29  2006/08/23 11:37:33  w4dimenscor
 * Adjustments to the SkillsSpells Functions in char specials
 *
 * Revision 1.28  2006/08/23 09:01:27  w4dimenscor
 * Changed some of the std::vectors to std::map, killlist, and the lookup tables for id nums
 *
 * Revision 1.27  2006/08/20 12:12:33  w4dimenscor
 * Changed the lookup table buckets to use sorted vectors. exciting. Also changed ignore list to use vectors, and fixed the valgrind error with the sort algorithm. Also sped up top gold command
 *
 * Revision 1.26  2006/08/18 11:09:59  w4dimenscor
 * updated some clan functions to use vectors instead of malloccing memory, and also sorted clan lists and updated their layout
 *
 * Revision 1.25  2006/08/17 10:53:49  w4dimenscor
 * moved the subs and skills from the char class to the player specials struct, converted them to vectors, and made them sorted.
 *
 * Revision 1.24  2006/08/13 06:26:55  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.23  2006/06/16 10:54:51  w4dimenscor
 * Moved several functions in fight.c into the Character object. Also removed all occurances of send_to_char from skills.c
 *
 * Revision 1.22  2006/05/30 09:14:20  w4dimenscor
 * rewrote the color code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.21  2006/05/22 10:50:49  w4dimenscor
 * Created 3 new files, mxp.cpp, mxp.h and descriptor.cpp
 * struct descriptor_data has been converted to class Descriptor
 *
 * Revision 1.20  2006/05/21 11:02:27  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.19  2006/05/08 19:36:27  w4dimenscor
 * Commiting some files that were out of the cvs because of the last backup fiasco, and there is also a bugfix for teleport in
 * scripts.
 *
 * Revision 1.19  2006/05/01 15:42:36  w4dimenscor
 * Corrected a typo in hurdle jumping with mounts, fixed the bug which would put you in fighting mode, but without opponent if you used an offensive wand on yourself
 * or a non-pk pc victim, changed movement so that you can now drive a vehicle by sitting down and using the ordinary movement commands and fixed socials. I think it
 * now works completely as intended.
 *
 * Revision 1.18  2006/05/01 11:29:26  w4dimenscor
 * I wrote a typo checker that automaticly corrects typos in the comm channels. I have also been fixing shadowed variables. There may be residual issues with it.
 *
 * Revision 1.17  2006/04/21 12:46:45  w4dimenscor
 * Fixed gcc 4.1 compile time errors. Game will now compile in GCC4
 *
 * Revision 1.16  2006/04/03 23:31:35  w4dimenscor
 * Added new commands called pclean, it removes the files of anyone who is not in the player index from the lib directory.
 *
 * Revision 1.15  2006/02/25 23:42:45  w4dimenscor
 * Directional spells work now, BUT mana blast doesn't
 *
 * Revision 1.14  2006/02/24 20:51:14  w4dimenscor
 * Added a message to quaff for empty potions
 *
 * Revision 1.13  2006/01/23 05:23:19  w4dimenscor
 * sorry self. another. _can't remember the changes_ entry
 *
 * Revision 1.12  2005/11/30 18:47:12  w4dimenscor
 * changed slightly some gains you get from remorts
 *
 * Revision 1.11  2005/11/20 06:10:00  w4dimenscor
 * Fixed Directional spells, and exp
 *
 * Revision 1.10  2005/11/19 06:18:39  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.9  2005/10/30 08:37:05  w4dimenscor
 * Updated compare command and fixed mining
 *
 * Revision 1.8  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.7  2005/05/28 05:52:14  w4dimenscor
 * Fixed some errors in copyover, added MXP
 *
 * Revision 1.6  2005/05/01 11:42:13  w4dimenscor
 * started a change in the server so that multiple arguments can be used when referencing items: have done this for locate object, look, goto, at and a few other things, havent done it for: get, put, drink, wear and a few others
 *
 * Revision 1.5  2005/03/19 15:02:55  w4dimenscor
 * gave centaurs the innate skill mount and riding at 100 % also adjusted
 * damage and speed a little.
 *
 * Revision 1.4  2005/02/25 05:02:45  w4dimenscor
 * added new commands and a few little changes - i forget what eek
 *
 * Revision 1.3  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.2  2004/12/17 07:13:20  w4dimenscor
 * A few little updates.
 *
 * Revision 1.1.1.1  2004/11/12 02:16:15  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.48  2004/09/04 03:46:51  molly
 * made it so only one cost for recovering corpses, and skillist is sorted
 *
 * Revision 1.45  2004/08/15 01:12:30  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
/**************************************************************************
 * This spell parser can be greatly improved, by combining it with the
 * subskills, seperate the spells into spell headers, then make the
 * subs as the spell names, with the perent of that subskill as the spell
 * so you would remove the spell "fireball", and add a spell called
 * "fire control", then make a subskill whos perent is "fire control"
 * and whos name is "fireball". That way you can have unlimited subskills
 * pointing to the limited amount of current spells.
 * two ways i can think of making this work off the top of my head.
 *
 * 1: when you Activate a subskill, like: 'activate fireball'
 *    then when you cast "fire control" its default IS 'fireball'
 *    and then in the magic.c functions, it checks what the active
 *    spell is for that group.
 *
 * 2: Rearange the spell parser below to search for the subskill name,
 *    and call that function as if it was a spell.
 *
 * I personally like the idea of the first one better, it can be used
 * in more places, like having default counter attacks, and starting and
 * finishing moves, depending on your defaults. :)
 * Mordecai@xtra.co.nz
 ***************************************************************************/


#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "spells.h"
#include "handler.h"
#include "comm.h"
#include "db.h"
#include "dg_scripts.h"
#include "constants.h"
#include "fight.h"
#include "action.h"
#include "descriptor.h"

vector<spell_info_type> spell_info ( MAX_SKILLS + 1 );

extern int spell_sorted_info[];

/* external functions */
int check_potion_weight ( struct obj_data *obj );
void improve_skill ( Character *ch, int skill );

/* local functions */
void say_spell ( Character *ch, int spellnum, Character *tch,
                 struct obj_data *tobj );
void spello ( int spl, const char *name, int max_mana, int min_mana,
              int mana_change, int minpos, int targets, int violent,
              int routines, int wait, int first_prereq, int second_prereq, int tier, int level, int gmi, const char *wear_off_msg );

void spello_system ( int spl, const char *name, int max_mana, int min_mana,
              int mana_change, int minpos, int targets, int violent,
              int routines, int wait, int first_prereq, int second_prereq, int tier, int level, int gmi, const char *wear_off_msg );

int mag_manacost ( Character *ch, int spellnum );
char * print_elemental ( int chcl, int weak, char * buf, size_t len );
ACMD ( do_cast );
void unused_spell ( int spl );
void mag_assign_spells ( void );
void default_message ( void );
int do_magic_direction ( int level, int dir, int dist, Character *ch, Character *vict, int spellnum );
int sp_dist = -1;

int tier_level ( Character *ch, int chclass );
int has_class ( Character *ch, int chclass );
/*
 * This arrangement is pretty stupid, but the number of skills is limited by
 * the playerfile.  We can arbitrarily increase the number of skills by
 * increasing the space in the playerfile. Meanwhile, 200 should provide
 * ample slots for skills.
 */






struct syllable syls[] =
{
	{" ", " "
	},
	{"ar", "alma"},
	{"ate", "mi"},
	{"cau", "tau"},
	{"blind", "clos"},
	{"bur", "shai"},
	{"cu", "kue"},
	{"de", "viso"},
	{"dis", "rel"},
	{"ect", "che"},
	{"en", "ng"},
	{"gro", "cra"},
	{"light", "dies"},
	{"lo", "hi"},
	{"magi", "kari"},
	{"mon", "bar"},
	{"mor", "zak"},
	{"move", "sido"},
	{"ness", "lacri"},
	{"ning", "illa"},
	{"per", "duda"},
	{"ra", "gru"},
	{"re", "candus"},
	{"son", "nk"},
	{"tect", "infra"},
	{"tri", "cula"},
	{"ven", "nofo"},
	{"word of", "inset"},
	{"slee", "nef"},
	{"sl", "gat"},
	{"ol", "nela"},
	{"al", "pyr"},
	{"a", "i"}, {"b", "v"}, {"c", "q"}, {"d", "m"}, {"e", "o"}, {"f", "y"},
	{"g", "t"},
	{"h", "p"}, {"i", "u"}, {"j", "y"}, {"k", "t"}, {"l", "r"}, {"m", "w"},
	{"n", "b"},
	{"o", "a"}, {"p", "s"}, {"q", "d"}, {"r", "f"}, {"s", "g"}, {"t", "h"},
	{"u", "e"},
	{"v", "z"}, {"w", "x"}, {"x", "n"}, {"y", "l"}, {"z", "k"}, {"", ""}
};





int mag_manacost ( Character *ch, int spellnum )
{
	return FTOI ( ( MAX ( SINFO.mana_max - ( SINFO.mana_change *
	                      ( GET_LEVEL ( ch ) -
	                        SINFO.min_level ) ),
	                      SINFO.mana_min ) ) * ( 1.4 + ( -resist_elem ( ch, elemental_type ( spellnum ) ) *0.01 ) ) );
}


/* say_spell erodes buf, buf1, buf2 */

void say_spell ( Character *ch, int spellnum, Character *tch,
                 struct obj_data *tobj )
{
	struct obj_data *focus = GET_EQ ( ch, WEAR_FOCUS );
	char lbuf[256], buf[256], buf1[256], buf2[256]; /* FIXME */
	const char *format;

	Character *i;
	int j, ofs = 0;

	*buf = '\0';
	strlcpy ( lbuf, skill_name ( spellnum ), sizeof ( lbuf ) );

	while ( lbuf[ofs] )
	{
		for ( j = 0; * ( syls[j].org ); j++ )
		{
			if ( !strncmp ( syls[j].org, lbuf + ofs, strlen ( syls[j].org ) ) )
			{
				strcat ( buf, syls[j].news ); /* strcat: BAD */
				ofs += strlen ( syls[j].org );
				break;
			}
		}
		/* i.e., we didn't find a match in syls[] */
		if ( !*syls[j].org )
		{
			log ( "No entry in syllable table for substring of '%s'", lbuf );
			ofs++;
		}
	}


	if ( tch != NULL && IN_ROOM ( tch ) == IN_ROOM ( ch ) )
	{

		if ( tch == ch )
		{
			if ( focus )
				format = "$n draws the runes of %s on $mself with $s focus.";
			else
				format = "$n draws the runes of %s on $mself.";
		}
		else
		{
			if ( focus )
				format =
				    "$n draws the runes of %s in the air with $s focus and sends them at $N.";
			else
				format =
				    "$n draws the runes of %s in the air and sends them at $N.";
		}

	}
	else if ( tobj != NULL
	          && ( ( IN_ROOM ( tobj ) == IN_ROOM ( ch ) )
	               || ( tobj->carried_by == ch ) ) )
	{
		if ( focus )
			format = "$n draws the runes of %s on $p with $s focus.";
		else
			format = "$n draws the runes of %s on $p.";
	}
	else
	{
		if ( focus )
			format = "$n draws the runes of %s in the air with $s focus.";
		else
			format = "$n draws the runes of %s in the air.";
	}

	if ( GET_SPELL_DIR ( ch ) != NOWHERE )
	{
		format = "$n focuses $s energy and sends the runes of %s %s.";
	}


	if ( GET_SPELL_DIR ( ch ) != NOWHERE )
	{
		snprintf ( buf1, sizeof ( buf1 ), format, skill_name ( spellnum ),
		           dirs[GET_SPELL_DIR ( ch ) ] );
		snprintf ( buf2, sizeof ( buf2 ), format, buf, dirs[GET_SPELL_DIR ( ch ) ] );
	}
	else
	{

		snprintf ( buf1, sizeof ( buf1 ), format, skill_name ( spellnum ) );
		snprintf ( buf2, sizeof ( buf2 ), format, buf );

	}

	for ( i = IN_ROOM ( ch )->people; i; i = i->next_in_room )
	{
		if ( i == ch || i == tch || !i->desc || !AWAKE ( i ) )
			continue;
		if ( GET_CLASS ( ch ) == GET_CLASS ( i ) )
			perform_act ( buf1, ch, tobj, tch, i );
		else
			perform_act ( buf2, ch, tobj, tch, i );
	}

	if ( tch != NULL && tch != ch && IN_ROOM ( tch ) == IN_ROOM ( ch ) )
	{
		if ( focus )
		{
			snprintf ( buf1, sizeof ( buf1 ),
			           "$n draws the runes of %s in the air with %s and sends them at you.",
			           GET_CLASS ( ch ) ==
			           GET_CLASS ( tch ) ? skill_name ( spellnum ) : buf,
			           focus->short_description );
		}
		else
		{
			snprintf ( buf1, sizeof ( buf1 ),
			           "$n draws the runes of %s in the air and sends them at you.",
			           GET_CLASS ( ch ) ==
			           GET_CLASS ( tch ) ? skill_name ( spellnum ) : buf );
		}
		act ( buf1, FALSE, ch, NULL, tch, TO_VICT );
	}
}


/*
 * This function should be used anytime you are not 100% sure that you have
 * a valid spell/skill number.  A typical for() loop would not need to use
 * this because you can guarantee > 0 and <= MAX_SKILLS.
 */
const char *skill_name ( int num )
{
	if ( num > 0 && num <= MAX_SKILLS )
		return ( spell_info[num].name );
	else if ( num == -1 )
		return ( "UNUSED" );
	else
		return ( "UNDEFINED" );
}

int Character::get_skill ( int i )
{
	if ( GET_RACE ( this ) == RACE_CENTAUR &&
	        ( i == 141 || i == 142 ) )
		return 100;
	if ( !IS_NPC ( this ) && player_specials != NULL )
		return player_specials->saved.GetSkillLearn ( i );
	else
		return 0;
}
void set_skill ( Character *ch, int skill, int amount )
{
	SAVED ( ch ).SetSkillLearn ( skill, amount );
}

int get_skill_wait ( Character *ch, int skill )
{
	return SAVED ( ch ).GetSkillWait ( skill );
}

void set_skill_wait ( Character *ch, int skill, int w )
{
	SAVED ( ch ).SetSkillWait ( skill, w );
}


int is_abbrev2 ( const char *arg1, const char *arg2 )
{
	if ( !*arg1 )
		return ( 0 );

	for ( ; *arg1 && *arg2; arg1++, arg2++ )
		if ( LOWER ( *arg1 ) != LOWER ( *arg2 ) )
			return ( *arg1-*arg2 );

	if ( !*arg1 )
		return ( 0 );
	else
		return ( *arg1-*arg2 );
}

int spell_num ( const char *name )
{
	int bot, top, mid;

	bot = 0;
	top = MAX_SKILLS;

	for ( ;; )
	{
		mid = ( bot + top ) / 2;

		if ( str_cmp ( name, spell_info[spell_sorted_info[mid]].name ) == 0 )
			return ( spell_sorted_info[mid] );
		if ( bot >= top )
			break;
		if ( str_cmp ( name, spell_info[spell_sorted_info[mid]].name ) < 0 )
			top = mid - 1;
		else
			bot = mid + 1;
	}
	bot = 0;
	top = MAX_SKILLS;

	/* perform binary search on spells */
	for ( ;; )
	{
		mid = ( bot + top ) / 2;

		if ( is_abbrev2 ( name, spell_info[spell_sorted_info[mid]].name ) == 0 )
			return ( spell_sorted_info[mid] );
		if ( bot >= top )
			return ( TYPE_UNDEFINED );
		if ( is_abbrev2 ( name, spell_info[spell_sorted_info[mid]].name ) < 0 )
			top = mid - 1;
		else
			bot = mid + 1;
	}
}

int find_skill_num ( char *name )
{
	/*int skindex, ok;
	char *temp, *temp2;
	char first[256], first2[256], tempbuf[256];*/

	return ( spell_num ( ( const char * ) name ) );
	/*

	for (skindex = 1; skindex <= MAX_SKILLS; skindex++)
	if (*spell_info[spell_sorted_info[skindex]].name == *name)
	break;

	for (; skindex <= MAX_SKILLS; skindex++) {
	if (is_abbrev(name, spell_info[spell_sorted_info[skindex]].name))
	 return (spell_sorted_info[skindex]);

	ok = TRUE;
	strlcpy(tempbuf, spell_info[spell_sorted_info[skindex]].name, sizeof(tempbuf));
	temp = any_one_arg(tempbuf, first);
	temp2 = any_one_arg(name, first2);
	while (*first && *first2 && ok) {
	 if (!is_abbrev(first2, first))
	ok = FALSE;
	 temp = any_one_arg(temp, first);
	 temp2 = any_one_arg(temp2, first2);
	}

	if (ok && !*first2)
	 return (spell_sorted_info[skindex]);
	 */


	return ( -1 );
}

/*
 * This function is the very heart of the entire magic system.  All
 * invocations of all types of magic -- objects, spoken and unspoken PC
 * and NPC spells, the works -- all come through this function eventually.
 * This is also the entry point for non-spoken or unrestricted spells.
 * Spellnum 0 is legal but silently ignored here, to make callers simpler.
 */
int call_magic ( Character *caster, Character *cvict,
                 struct obj_data *ovict, char *tar_str, int spellnum,
                 int level, int casttype )
{
	int savetype;

	if ( spellnum < 1 || spellnum > MAX_SKILLS )
		return ( 0 );

	if ( !cast_wtrigger ( caster, cvict, ovict, spellnum ) )
		return 0;
	else if ( ovict && !cast_otrigger ( caster, ovict, spellnum ) )
		return 0;
	else if ( cvict && !cast_mtrigger ( caster, cvict, spellnum ) )
		return 0;

	if ( cvict && AFF_FLAGGED ( cvict, AFF_MAGIC_BUBBLE ) )
	{
		act ( "Your magic washes over $N without touching $M.", FALSE, caster, 0, cvict, TO_CHAR );
		act ( "$n's magic washes over you without touching you.", FALSE, caster, 0, cvict, TO_VICT );
		act ( "$n's magic washes over $N without touching $M.", FALSE, caster, 0, cvict, TO_NOTVICT );
		return 0;
	}


	if ( caster->vnum != DG_CASTER_PROXY ||
	        ( casttype != CAST_BREATH
	          || ( casttype == CAST_BREATH && !IS_NPC ( caster ) ) ) )
	{
		if ( ROOM_FLAGGED ( IN_ROOM ( caster ), ROOM_NOMAGIC ) && GET_NAME ( caster ) )
		{
			int n = 0;
			const char *name = GET_NAME ( caster );
			if ( GET_NAME ( caster ) )
				n = strlen ( GET_NAME ( caster ) );
			if ( n > 0 && name[n-1] == 's' )
				act ( "$n' magic fizzles out and dies.", FALSE, caster, 0, 0, TO_ROOM );
			else
				act ( "$n's magic fizzles out and dies.", FALSE, caster, 0, 0, TO_ROOM );

			caster->Send ( "Your magic fizzles out and dies.\r\n" );
			GET_WAIT_STATE ( caster ) += ( 2 RL_SEC );
			return ( 0 );
		}
		if ( ROOM_FLAGGED ( IN_ROOM ( caster ), ROOM_PEACEFUL ) &&
		        ( SINFO.violent || IS_SET ( SINFO.routines, MAG_DAMAGE ) ) )
		{
			send_to_char ( "A flash of white light fills the room, dispelling your violent magic!\r\n", caster );
			act ( "White light from no particular source suddenly fills the room, then vanishes.", FALSE, caster, 0, 0, TO_ROOM );
			GET_WAIT_STATE ( caster ) += ( 2 RL_SEC );
			return ( 0 );
		}
	}

	/* determine the type of saving throw */
	switch ( casttype )
	{
		case CAST_STAFF:
		case CAST_SCROLL:
		case CAST_POTION:
		case CAST_WAND:
			savetype = SAVING_ROD;
			break;
		case CAST_SPELL:
			savetype = SAVING_SPELL;
			break;
		default:
			savetype = SAVING_BREATH;
			break;
	}

	if ( GET_SPELL_DIR ( caster ) !=NOWHERE && sp_dist != NOWHERE )
		return do_magic_direction ( level, GET_SPELL_DIR ( caster ), sp_dist, caster,cvict, spellnum );

	if ( IS_SET ( SINFO.routines, MAG_DAMAGE ) )
		if ( mag_damage ( level, caster, cvict, spellnum, savetype ) == -1 )
			return ( -1 );  /* Successful and target died, don't cast again. */

	if ( IS_SET ( SINFO.routines, MAG_AFFECTS ) )
		mag_affects ( level, caster, cvict, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_UNAFFECTS ) )
		mag_unaffects ( level, caster, cvict, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_POINTS ) )
		mag_points ( level, caster, cvict, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_ALTER_OBJS ) )
		mag_alter_objs ( level, caster, ovict, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_GROUPS ) )
		mag_groups ( level, caster, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_MASSES ) )
		mag_masses ( level, caster, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_AREAS ) )
		mag_areas ( level, caster, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_SUMMONS ) )
		mag_summons ( level, caster, ovict, spellnum, savetype );

	if ( IS_SET ( SINFO.routines, MAG_CREATIONS ) )
		mag_creations ( level, caster, spellnum, tar_str );

        if (IS_SET (SINFO.routines, MAG_ROOM_AFFECTS))
                mag_room_affects(level, caster, spellnum);

	if ( IS_SET ( SINFO.routines, MAG_MANUAL ) )
		switch ( spellnum )
		{
			case SPELL_CHARM:
				MANUAL_SPELL ( spell_charm );
				break;
			case SPELL_CREATE_WATER:
				MANUAL_SPELL ( spell_create_water );
				break;
			case SPELL_DETECT_POISON:
				MANUAL_SPELL ( spell_detect_poison );
				break;
			case SPELL_ENCHANT_WEAPON:
				MANUAL_SPELL ( spell_enchant_weapon );
				break;
			case SPELL_MINOR_IDENTIFY:
				MANUAL_SPELL ( spell_minor_identify );
				break;
			case SPELL_LOCATE_OBJECT:
				MANUAL_SPELL ( spell_locate_object );
				break;
			case SPELL_SUMMON:
				MANUAL_SPELL ( spell_summon );
				break;
			case SPELL_WORD_OF_RECALL:
				MANUAL_SPELL ( spell_recall );
				break;
			case SPELL_TELEPORT:
				MANUAL_SPELL ( spell_teleport );
				break;
			case SPELL_GATE:
				MANUAL_SPELL ( spell_gate );
				break;
			case SPELL_CONTROL_WEATHER:
				MANUAL_SPELL ( spell_control_weather );
				break;
			case SPELL_CHAIN_LIGHTNING:
				MANUAL_SPELL ( spell_chain_lightning );
				break;
			case SPELL_RECHARGE:
				MANUAL_SPELL ( spell_recharge );
				break;
			case SPELL_IDENTIFY:
				MANUAL_SPELL ( spell_identify );
				break;
			case SPELL_POLYMORPH:
				MANUAL_SPELL ( spell_polymorph );
				break;
			case SPELL_MIDAS_TOUCH:
				MANUAL_SPELL ( spell_midas_touch );
				break;
			case SPELL_WATER_TO_WINE:
				MANUAL_SPELL ( spell_water_to_wine );
				break;
			case SPELL_REMOVE_ALIGNMENT:
				MANUAL_SPELL ( spell_remove_alignment );
				break;
			case SPELL_ENCHANT_ARMOR:
				MANUAL_SPELL ( spell_enchant_armor );
				break;
			case SPELL_PSI_PANIC:
				MANUAL_SPELL ( spell_psi_panic );
				break;
			case SPELL_KNOCK:
				MANUAL_SPELL ( spell_knock );
				break;
			case SPELL_CALL_LIGHTNING:
				MANUAL_SPELL ( spell_call_lightning );
				break;
		}

	return ( 1 );
}

/*
 * mag_objectmagic: This is the entry-point for all magic items.  This should
 * only be called by the 'quaff', 'use', 'recite', etc. routines.
 *
 * For reference, object values 0-3:
 * staff  - [0]     level     [1] max charges     [2] num charges     [3] spell num
 * wand   - [0]     level     [1] max charges     [2] num charges     [3] spell num
 * scroll - [0]     level     [1] spell num  [2] spell num  [3] spell num
 * potion - [0] level    [1] spell num  [2] spell num  [3] spell num
 *
 * Staves and wands will default to level 14 if the level is not specified;
 * the DikuMUD format did not specify staff and wand levels in the world
 * files (this is a CircleMUD enhancement).
 */

void mag_objectmagic ( Character *ch, struct obj_data *obj,
                       char *argument )
{
	int i, k;
	Character *tch = NULL;//, *next_tch;
	struct obj_data *tobj = NULL;
	char arg[MAX_INPUT_LENGTH];

	//Make sure that there is no spell direction set
	GET_SPELL_DIR ( ch ) =NOWHERE;

	one_argument ( argument, arg );

	k = generic_find ( arg, FIND_CHAR_ROOM | FIND_OBJ_INV | FIND_OBJ_ROOM |
	                   FIND_OBJ_EQUIP, ch, &tch, &tobj );

	if ( use_stamina ( ch, 3 ) < 0 )
	{
		ch->Send ( "You are far too exhausted!" );
		return;
	}


	switch ( GET_OBJ_TYPE ( obj ) )
	{
#if 1
		case ITEM_STAFF:
			act ( "You tap $p three times on the ground.", FALSE, ch, obj, 0,
			      TO_CHAR );
			if ( obj->action_description )
				act ( obj->action_description, FALSE, ch, obj, 0, TO_ROOM );
			else
				act ( "$n taps $p three times on the ground.", FALSE, ch, obj, 0,
				      TO_ROOM );

			if ( GET_OBJ_VAL ( obj, 2 ) <= 0 )
			{
				send_to_char ( "It seems powerless.", ch );
				act ( "Nothing seems to happen.", FALSE, ch, obj, 0, TO_ROOM );
			}
			else
			{
				GET_OBJ_VAL ( obj, 2 )--;
				WAIT_STATE ( ch, PULSE_VIOLENCE );
				/* Level to cast spell at. */
				k = GET_OBJ_VAL ( obj, 0 ) ? GET_OBJ_VAL ( obj,
				        0 ) : DEFAULT_STAFF_LVL;

				/*
				 * Problem : Area/mass spells on staves can cause crashes.
				 * Solution: Remove the special nature of area/mass spells on staves.
				 * Problem : People like that behavior.
				 * Solution: We special case the area/mass spells here.
				 */
				if ( HAS_SPELL_ROUTINE
				        ( GET_OBJ_VAL ( obj, 3 ), MAG_MASSES | MAG_AREAS ) )
				{
					for ( i = 0, tch = IN_ROOM ( ch )->people; tch;
					        tch = tch->next_in_room )
						i++;
					while ( i-- > 0 )
						call_magic ( ch, NULL, NULL, 0, GET_OBJ_VAL ( obj, 3 ), k,
						             CAST_STAFF );
				}
				else
				{
					Character *next_tch;
					for ( tch = IN_ROOM ( ch )->people; tch; tch = next_tch )
					{
						next_tch = tch->next_in_room;
						if ( ch != tch )
							call_magic ( ch, tch, NULL, 0, GET_OBJ_VAL ( obj, 3 ),
							             k, CAST_STAFF );
					}
				}
			}
			break;
#else

		case ITEM_STAFF:
#endif

		case ITEM_WAND:
			if ( k == FIND_CHAR_ROOM )
			{
				if ( tch == ch )
				{
					act ( "You point $p at yourself.", FALSE, ch, obj, 0,
					      TO_CHAR );
					act ( "$n points $p at $mself.", FALSE, ch, obj, 0, TO_ROOM );
				}
				else
				{
					act ( "You point $p at $N.", FALSE, ch, obj, tch, TO_CHAR );
					if ( obj->action_description )
						act ( obj->action_description, FALSE, ch, obj, tch,
						      TO_ROOM );
					else
						act ( "$n points $p at $N.", TRUE, ch, obj, tch,
						      TO_ROOM );
				}
			}
			else if ( tobj != NULL )
			{
				act ( "You point $p at $P.", FALSE, ch, obj, tobj, TO_CHAR );
				if ( obj->action_description )
					act ( obj->action_description, FALSE, ch, obj, tobj,
					      TO_ROOM );
				else
					act ( "$n points $p at $P.", TRUE, ch, obj, tobj, TO_ROOM );
			}
			else
				if ( IS_SET
				        ( spell_info[GET_OBJ_VAL ( obj, 3 ) ].routines,
				          MAG_AREAS | MAG_MASSES ) )
				{
					/* Wands with area spells don't need to be pointed. */
					act ( "You point $p outward.", FALSE, ch, obj, NULL, TO_CHAR );
					act ( "$n points $p outward.", TRUE, ch, obj, NULL, TO_ROOM );
				}
				else
				{
					act ( "At what should $p be pointed?", FALSE, ch, obj, NULL,
					      TO_CHAR );
					return;
				}

			if ( GET_OBJ_VAL ( obj, 2 ) <= 0 )
			{
				send_to_char ( "It seems powerless.\r\n", ch );
				act ( "Nothing seems to happen.", FALSE, ch, obj, 0, TO_ROOM );
				return;
			}
			GET_OBJ_VAL ( obj, 2 )--;
			WAIT_STATE ( ch, 1 RL_SEC );
			if ( GET_OBJ_VAL ( obj, 0 ) )
				call_magic ( ch, tch, tobj, 0, GET_OBJ_VAL ( obj, 3 ),
				             GET_OBJ_VAL ( obj, 0 ), CAST_WAND );
			else
				call_magic ( ch, tch, tobj, 0, GET_OBJ_VAL ( obj, 3 ),
				             DEFAULT_WAND_LVL, CAST_WAND );
			if ( FIGHTING ( ch ) && tch && ( !FIGHTING ( tch ) || tch==ch ) )
				stop_fighting ( ch );
			check_potion_weight ( obj );
			break;
		case ITEM_SCROLL:
			if ( *arg )
			{
				if ( !k )
				{
					act ( "There is nothing to here to affect with $p.", FALSE,
					      ch, obj, NULL, TO_CHAR );
					return;
				}
			}
			else
				tch = ch;

			act ( "You recite $p which dissolves.", TRUE, ch, obj, 0, TO_CHAR );
			if ( obj->action_description )
				act ( obj->action_description, FALSE, ch, obj, NULL, TO_ROOM );
			else
				act ( "$n recites $p.", FALSE, ch, obj, NULL, TO_ROOM );



			WAIT_STATE ( ch, 1 RL_SEC );
			for ( i = 1; i <= 3; i++ )
				if ( call_magic ( ch, tch, tobj, 0, GET_OBJ_VAL ( obj, i ),
				                  GET_OBJ_VAL ( obj, 0 ), CAST_SCROLL ) <= 0 )
					break;
			if ( obj != NULL )
			{
				extract_obj ( obj );
				obj = NULL;
			}
			break;
		case ITEM_POTION:
		case ITEM_ANTIDOTE_1:
		case ITEM_ANTIDOTE_2:
		case ITEM_ANTIDOTE_3:
			tch = ch;

			if ( !consume_otrigger ( obj, ch, OCMD_QUAFF ) )  /* check trigger */
				return;
			if ( GET_OBJ_VAL ( obj, 1 ) < 0 && GET_OBJ_VAL ( obj, 2 ) < 0 && GET_OBJ_VAL ( obj, 3 ) < 0 )
			{
				act ( "You quaff a little bit of air from $p then burp.", FALSE, ch, obj, NULL, TO_CHAR );
				act ( "$n quaffs a little bit of air from $p then burps.", FALSE, ch, obj, NULL, TO_ROOM );
				extract_obj ( obj );
				return;
			}
			act ( "You quaff $p.", FALSE, ch, obj, NULL, TO_CHAR );
			if ( obj->action_description )
				act ( obj->action_description, FALSE, ch, obj, NULL, TO_ROOM );
			else
				act ( "$n quaffs $p.", TRUE, ch, obj, NULL, TO_ROOM );

			WAIT_STATE ( ch, 1 RL_SEC );
			if ( GET_OBJ_TYPE ( obj ) == ITEM_POTION )
			{
				for ( i = 1; i <= 3; i++ )
					if ( call_magic ( ch, ch, NULL, 0, GET_OBJ_VAL ( obj, i ),
					                  GET_OBJ_VAL ( obj, 0 ), CAST_POTION ) <= 0 )
						;
			}
			else
			{
				i = TYPE_UNDEFINED;
				if ( GET_OBJ_TYPE ( obj ) == ITEM_ANTIDOTE_1 )
					i = SPELL_ANTIDOTE_1;
				if ( GET_OBJ_TYPE ( obj ) == ITEM_ANTIDOTE_2 )
					i = SPELL_ANTIDOTE_2;
				if ( GET_OBJ_TYPE ( obj ) == ITEM_ANTIDOTE_3 )
					i = SPELL_ANTIDOTE_3;

				call_magic ( ch, ch, NULL, 0, i,  50, CAST_POTION );
			}
			if ( obj != NULL )
			{
				extract_obj ( obj );
				obj = NULL;
			}
			break;
		default:
			log ( "SYSERR: Unknown object_type %d in mag_objectmagic.",
			      GET_OBJ_TYPE ( obj ) );
			break;
	}
}


/*
 * cast_spell is used generically to cast any spoken spell, assuming we
 * already have the target char/obj and spell number.  It checks all
 * restrictions, etc., prints the words, etc.
 *
 * Entry point for NPC casts.  Recommended entry point for spells cast
 * by NPCs via specprocs.
 */

int cast_spell ( Character *ch, Character *tch,
                 struct obj_data *tobj, char *tar_str, int spellnum )
{
	int result;
	struct obj_data *focus = GET_EQ ( ch, WEAR_FOCUS );



	if ( spellnum < 0 || spellnum >= MAX_SKILLS)
	{
		log ( "SYSERR: cast_spell trying to call spellnum %d/%d.\n",
		      spellnum, MAX_SKILLS );
		return ( 0 );
	}

	if ( GET_POS ( ch ) < SINFO.min_position )
	{
		switch ( GET_POS ( ch ) )
		{
			case POS_SLEEPING:
				ch->Send ( "You dream about great magical powers.\r\n" );
				break;
			case POS_RESTING:
				ch->Send ( "You can not concentrate while resting.\r\n" );
				break;
			case POS_SITTING:
				ch->Send ( "You can not do this sitting!\r\n" );
				break;
			case POS_FIGHTING:
				ch->Send ( "Impossible!  You can not concentrate enough!\r\n" );
				break;
			default:
				ch->Send ( "You can not do much of anything like this!\r\n" );
				break;
		}
		return ( 0 );
	}
	if ( AFF_FLAGGED ( ch, AFF_CHARM ) && ( ch->master == tch ) )
	{
		ch->Send ( "You are afraid you might hurt your master!\r\n" );
		return ( 0 );
	}
	if ( ( tch && tch != ch ) && IS_SET ( SINFO.targets, TAR_SELF_ONLY ) )
	{
		ch->Send ( "You can only cast this spell upon yourself!\r\n" );
		return ( 0 );
	}
	if ( ( tch == ch ) && IS_SET ( SINFO.targets, TAR_NOT_SELF ) )
	{
		ch->Send ( "You can not cast this spell upon yourself!\r\n" );
		return ( 0 );
	}
	if ( IS_SET ( SINFO.routines, MAG_GROUPS ) && ( !ch->master && !ch->followers ) )
	{
		ch->Send ( "You can not cast this spell if you are not in a group!\r\n" );
		return ( 0 );
	}
	if ( GET_SPELL_DIR ( ch ) != NOWHERE && SINFO.violent && tch!=NULL && CAN_HUNT ( tch ) && !tch->canHuntChar ( ch ) )
	{
		ch->Send ( "You can't get a clear shot.\r\n" );
		return ( 0 );
	}

	if ( !focus )
	{
		if ( PRF_FLAGGED ( ch, PRF_NOREPEAT ) )
		{
			ch->Send ( "%s", CONFIG_OK );
		}
		else if ( GET_SPELL_DIR ( ch ) != NOWHERE )
		{
			ch->Send ( "You concentrate your energy and send the runes of %s %s.\r\n",
			           skill_name ( spellnum ),
			           dirs[GET_SPELL_DIR ( ch ) ] );
		}
		else if ( IS_SET ( SINFO.routines, MAG_DAMAGE ) )
		{
			ch->Send ( "You power up %s %s attack.\r\n", LANA ( skill_name ( spellnum ) ), skill_name ( spellnum ) );
		}
		else
		{
			ch->Send (
			    "You concentrate and draw the runes for %s in the air.\r\n",
			    skill_name ( spellnum ) );
		}
	}
	else
	{
		if ( PRF_FLAGGED ( ch, PRF_NOREPEAT ) )
		{
			ch->Send ( "%s", CONFIG_OK );
		}
		else if ( IS_SET ( SINFO.routines, MAG_DAMAGE ) )
		{
			ch->Send ( "You power up %s %s attack.\r\n", LANA ( skill_name ( spellnum ) ), skill_name ( spellnum ) );
		}
		else if ( GET_SPELL_DIR ( ch ) != NOWHERE )
		{
			ch->Send (
			    "You focus your energy and send the runes of %s %s.\r\n",
			    skill_name ( spellnum ),
			    dirs[GET_SPELL_DIR ( ch ) ] );
		}
		else
		{
			ch->Send (
			    "You focus and draw the runes for %s in the air.\r\n",
			    skill_name ( spellnum ) );
		}
	}


	say_spell ( ch, spellnum, tch, tobj );

	result =
	    call_magic ( ch, tch, tobj, tar_str, spellnum, GET_LEVEL ( ch ),
	                 CAST_SPELL );
	if ( result != 0 )
		improve_skill ( ch, spellnum );
	return ( result );
}


/*
 * do_cast is the entry point for PC-casted spells.  It parses the arguments,
 * determines the spell number and finds a target, throws the die to see if
 * the spell can be cast, checks for sufficient mana and subtracts it, and
 * passes control to cast_spell().
 */

ACMD ( do_cast )
{

	Character *tch = NULL;//, *victim = NULL;
	struct obj_data *tobj = NULL;
	char *s, *t, abuf[MAX_INPUT_LENGTH], *a;
	int mana, spellnum, i, target = 0, dir = NOWHERE;

	a = abuf;




	if ( IS_NPC ( ch ) && IS_AFFECTED ( ch, AFF_CHARM ) )
		return;
	GET_SPELL_DIR ( ch ) = NOWHERE;

	/* get: blank, spell name, target name */
	s = strtok ( argument, "'" );

	if ( s == NULL )
	{
		ch->Send ( "Cast what where?\r\n" );
		return;
	}
	s = strtok ( NULL, "'" );
	if ( s == NULL )
	{
		ch->Send ( "Spell names must be enclosed in the Holy Magic Symbols: '\r\n" );
		return;
	}
	t = strtok ( NULL, "\0" );

	/* spellnum = search_block(s, spells, 0); */
	spellnum = find_skill_num ( s );

	if ( !IS_SPELL_CAST(spellnum))
	{
		ch->Send ( "Cast what?!?\r\n" );
		return;
	}




	if ( !IS_NPC ( ch ) && total_chance ( ch, spellnum ) == 0 )
	{
		ch->Send ( "You are unfamiliar with that spell.\r\n" );
		return;
	}

	if ( !IS_NPC ( ch ) && GET_LEVEL ( ch ) <= LVL_GOD )
		if ( GET_SPELL_WAIT ( ch, spellnum ) > 0 )
		{
			ch->Send (
			    "Your ability to cast this spell has been worn out for \r\n"
			    "at least another %d second%s\r\n",
			    GET_SPELL_WAIT ( ch, spellnum ),
			    GET_SPELL_WAIT ( ch,
			                     spellnum ) > 1 ? "s." : "." );
			return;
		}

	GET_WAIT_STATE ( ch ) += ( 1 RL_SEC );

	/* Find the target */
	if ( t != NULL )
	{
		char arg[MAX_INPUT_LENGTH];

		strlcpy ( arg, t, sizeof ( arg ) );
		a = one_argument ( t, arg );
		skip_spaces ( &t );
		skip_spaces ( &a );
		if ( *arg )
			dir = search_block ( arg, dirs, FALSE );
	}
	//ch->Send( "a is %s, dir is %s, t is %s\r\n", a, dirs[dir], t);

	/*Special case Locate Object*/
	if ( spellnum == SPELL_LOCATE_OBJECT )
	{
		/* use the CREATE macro instead of the create_obj func so we don't
		 * add the temp obj to the global list, avoiding the overhead of
		 * adding and removing it.
		 */
		/* could get fancy here and support multiple arguments, but the code in
		 * spells.c would have to be updated too.  Anyone want to write it? :-)
		 */
		/**mord - done this **/
		target = TRUE;
	}
	else

		/* the start of finding the target
		 * If they typed a target, see if we can find them*/

		if ( IS_SET ( SINFO.targets, TAR_IGNORE ) && !IS_SET ( SINFO.targets, TAR_AREA_DIR ) )
		{
			/* if no target wanted */
			target = TRUE;
		}
		else if ( IS_SET ( SINFO.targets, TAR_SELF_ONLY ) )
		{
			tch = ch;
			target = TRUE;

		}
		else if ( t != NULL && *t )
		{


			/* if target needed and a target was surplied */

			if ( !target && ( IS_SET ( SINFO.targets, TAR_CHAR_ROOM ) ) )
				if ( ( tch = get_char_vis ( ch, t, NULL, FIND_CHAR_ROOM ) ) != NULL )
					target = TRUE;

			if ( !target && IS_SET ( SINFO.targets, TAR_CHAR_WORLD ) )
				if ( ( tch = get_char_vis ( ch, t, NULL, FIND_CHAR_WORLD ) ) != NULL )
					target = TRUE;

			if ( !target && IS_SET ( SINFO.targets, TAR_OBJ_INV ) )
				if ( ( tobj =
				            get_obj_in_list_vis ( ch, t, NULL, ch->carrying ) ) != NULL )
					target = TRUE;

			if ( !target && IS_SET ( SINFO.targets, TAR_OBJ_EQUIP ) )
			{
				for ( i = 0; !target && i < NUM_WEARS; i++ )
					if ( HAS_BODY ( ch, i ) && GET_EQ ( ch, i )
					        && isname_full ( t, GET_EQ ( ch, i )->name ) )
					{
						tobj = GET_EQ ( ch, i );
						target = TRUE;
					}
			}

			if ( !target && IS_SET ( SINFO.targets, TAR_OBJ_ROOM ) )
				if ( ( tobj =
				            get_obj_in_list_vis ( ch, t, NULL,
				                                  IN_ROOM ( ch )->contents ) ) != NULL )
					target = TRUE;

			if ( !target && IS_SET ( SINFO.targets, TAR_OBJ_WORLD ) )
				if ( ( tobj = get_obj_vis ( ch, t, NULL ) ) != NULL )
					target = TRUE;

			/*New case: TAR_AREA_DIR targets someone, in a direction, or just a direction.
			   can be used for door spells. Can be used for far sight spells.
			   can be used for missile spells. Or even movement. */
			//ch->Send( "target is %d\r\n", target);
			if ( !target && IS_SET ( SINFO.targets, TAR_AREA_DIR ) && dir != NOWHERE && EXIT2 ( IN_ROOM ( ch ),dir ) != NULL )
			{
				//start find_in_dir one room away
				tch = find_in_dir ( EXIT2 ( IN_ROOM ( ch ),dir )->to_room, a, dir,ch );
				if ( tch && ( ( sp_dist = magic_distance ( ch, spellnum, dir, tch ) ) != NOWHERE ) )
				{
					target = TRUE;
					GET_SPELL_DIR ( ch ) = dir;
				}
				else if ( ( ( sp_dist = magic_distance ( ch, spellnum, dir, NULL ) ) != NOWHERE ) &&
				          IS_SET ( SINFO.targets, TAR_IGNORE ) && dir != NOWHERE )
				{
					tch = NULL;
					GET_SPELL_DIR ( ch ) = dir;
					target = TRUE;

				}
			}
			//ch->Send( "target is %d\r\n", target);
			/*New case: TAR_AREA_ROOM targets the casters room but cycles through everyone in the room */

			if ( !target && IS_SET ( SINFO.targets, TAR_AREA_ROOM ) )
			{
				//tch = IN_ROOM(ch)->people;
				tch = ch;
				target = TRUE;
			}

		}
		else                 /* if target string is empty */
		{
			if ( !target && IS_SET ( SINFO.targets, TAR_FIGHT_SELF ) )
				if ( FIGHTING ( ch ) != NULL )
				{
					tch = ch;
					target = TRUE;
				}
			if ( !target && IS_SET ( SINFO.targets, TAR_FIGHT_VICT ) )
				if ( FIGHTING ( ch ) != NULL )
				{
					tch = FIGHTING ( ch );
					target = TRUE;
				}
			/* if no target specified, and the spell isn't violent, default to self */
			if ( !target && IS_SET ( SINFO.targets, TAR_CHAR_ROOM ) &&
			        !SINFO.violent )
			{
				tch = ch;
				target = TRUE;
			}

			if ( !target )
			{
				ch->Send ( "Upon %s should the spell be cast?\r\n",
				           IS_SET ( SINFO.targets,
				                    TAR_OBJ_ROOM | TAR_OBJ_INV |
				                    TAR_OBJ_WORLD | TAR_OBJ_EQUIP ) ? "what"
				           : "who" );
				return;
			}
		}

	if ( target && ( tch == ch ) && SINFO.violent )
	{
		ch->Send ( "You shouldn't cast that on yourself -- could be bad for your health!\r\n" );
		return;
	}

	/*if (dir == NOWHERE && (t != NULL && *t)
	&& IS_SET(SINFO.targets, TAR_AREA_DIR) && sp_dist)
	if ((dir = search_block(t, dirs, FALSE)) != NOWHERE) {
	 tch = NULL;
	 GET_SPELL_DIR(ch) = dir;
	 target = TRUE;
	}*/
	//ch->Send( "target is %d\r\n", target);
	if ( !target )
	{
		send_to_char ( "Cannot find the target of your spell!\r\n", ch );
		return;
	}

	if ( use_stamina ( ch, 3 ) < 0 )
	{
		ch->Send ( "You haven't the energy to cast %s!\r\n", skill_name ( spellnum ) );
		GET_SPELL_DIR ( ch ) = NOWHERE;
		return;
	}
	mana = mag_manacost ( ch, spellnum );
	if ( ( mana > 0 ) && ( GET_MANA ( ch ) < mana ) && ( GET_LEVEL ( ch ) < LVL_HERO ) )
	{
		ch->Send ( "You haven't the energy to cast %s!\r\n", skill_name ( spellnum ) );
		GET_SPELL_DIR ( ch ) = NOWHERE;
		return;
	}

	/* You throw the dice and take your chances.. 101% is total failure */
	if ( number ( 0, 101 ) > ( total_chance ( ch, spellnum ) ) )
	{

		if ( !tch || !skill_message ( 0, ch, tch, spellnum ) )
			GET_SPELL_DIR ( ch ) = NOWHERE;

		if ( RIDING ( ch ) )
			ch->Send ( "Your mount knocks you about too much!\r\n" );
		else
			ch->Send ( "You lost your concentration!\r\n" );

		if ( mana > 0 )
			alter_mana ( ch, mana / 2 );

		if ( SINFO.violent && tch && IS_NPC ( tch ) && HERE ( tch, ch ) && !ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_PEACEFUL ) && !FIGHTING(tch) )
			start_fighting_delay ( tch, ch );

	}
	else              /* cast spell returns 1 on success; subtract mana & set waitstate */
	{
		if ( cast_spell ( ch, tch, tobj, t, spellnum ) )
		{
			GET_SPELL_DIR ( ch ) = NOWHERE;
			if ( !IS_NPC ( ch ) && GET_LEVEL ( ch ) < LVL_IMMORT )
			{
				if ( !SINFO.wait )
					WAIT_STATE ( ch, 1 RL_SEC );
				else
					SET_SPELL_WAIT ( ch, spellnum, SINFO.wait );

				if ( mana > 0 )
					alter_mana ( ch, mana );
			}
		}
		else if ( SINFO.violent && tch && IS_NPC ( tch ) && HERE ( tch, ch ) && !ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_PEACEFUL ) && !FIGHTING(tch))
			start_fighting_delay ( tch, ch );
	}
}
int grand_master ( Character *ch )
{
	int i, m = 0;
	for ( i = 0; i < NUM_CLASSES; i++ )
		if ( GET_MASTERY ( ch, i ) )
			m++;
	return ( m == NUM_CLASSES );
}

/* This works for skills too -- Mord */
int knows_spell ( Character *ch, int spell )
{
	int i, gm, t;
	int ret_val = 0;
	if ( IS_NPC ( ch ) )
	{
		if ( ch->vnum == DG_CASTER_PROXY )
			return TRUE;
		else if ( spell >= SPELL_FIRE_BREATH && spell <= SPELL_LIGHTNING_BREATH )
			return TRUE;
	}


	if ( GET_LEVEL ( ch ) >= LVL_IMMORT )
		return 1;
	if ( spell < 0 || spell >= MAX_SKILLS )
		return 0;
        /* only allow players to have access to skills and spells */
        /* system TYPE_HIT == 0 and system spells == 3 */
        if (spell_info[spell].type == 0)
               return 0;
        if (spell_info[spell].type > 2)
               return 0;
	if ( spell_info[spell].min_level >= LVL_IMMORT )
		return 0;

	gm = grand_master ( ch );

	for ( i = 0; i < NUM_CLASSES ; i++ )
	{
		if ( gm )
		{
			if ( spell_info[spell].gm > 0 )
				ret_val++;
		}

		if ( IS_SET ( spell_info[spell].classes, ( 1 << i ) ) )
		{
			if ( !IS_NPC ( ch ) )
			{
				if ( !has_class ( ch, i ) )
					continue;

				if ( GET_CLASS ( ch ) == i )
					t = tier_level ( ch, i );
				else
					t = MIN ( tier_level ( ch, i ), 2 );
			}
			else
				t = MOB_TIER ( ch );


			if ( spell_info[spell].tier < t )
			{
				ret_val++;
			}
			else if ( spell_info[spell].tier == t )
			{
				if ( spell_info[spell].min_level <= GET_LEVEL ( ch ) )
					ret_val++;
			}
		}

	}
	if ( ret_val && spell_info[spell].first_prereq != TYPE_UNDEFINED )
		if ( !knows_spell ( ch, spell_info[spell].first_prereq ) || total_chance ( ch, spell_info[spell].first_prereq ) == 0 )
			return 0;

	if ( ret_val && spell_info[spell].second_prereq != TYPE_UNDEFINED )
		if ( !knows_spell ( ch, spell_info[spell].second_prereq ) || total_chance ( ch, spell_info[spell].second_prereq ) == 0 )
			return 0;

	switch ( spell )
	{
		case SKILL_TINKER:
			if ( GET_CLASS ( ch ) != CLASS_GYPSY )
				return 0;
			break;
		case SPELL_POISON_4:
		case SPELL_POISON_3:
		case SPELL_POISON_2:
		case SPELL_LOCATE_PERSON:
			if ( GET_LEVEL ( ch ) <= LVL_IMMORT )
				return 0;
			break;
	}
	return ret_val;
}


void assign_class ( int spell, int chclass )
{
	if ( spell  < 0 || spell > MAX_SKILLS )
	{
		log ( "SYSERR: attempting assign to illegal spellnum %d/%d", spell, MAX_SKILLS );
		return;
	}

	if ( spell_info[spell].classes == 0 )
	{
		spell_info[spell].classes = chclass;
	}
	else
	{
		log ( "Spell %s already defined as %d but trying to be defined as %d.",
		      spell_info[spell].name, spell_info[spell].classes, chclass );
	}

}


/* Assign the spells on boot up */
void spello ( int spl, const char *name, int max_mana, int min_mana,
              int mana_change, int minpos, int targets, int violent,
              int routines, int w, int first_prereq, int second_prereq, int tier, int level, int gm , const char *wear_off_msg)
{
	spell_info[spl].mana_max = max_mana;
	spell_info[spl].mana_min = min_mana;
	spell_info[spl].mana_change = mana_change;
	spell_info[spl].min_position = minpos;
	spell_info[spl].targets = targets;
	spell_info[spl].violent = violent;
	spell_info[spl].routines = routines;
	spell_info[spl].name = name;
	spell_info[spl].wait = w;
	spell_info[spl].first_prereq = first_prereq;
	spell_info[spl].second_prereq = second_prereq;
	spell_info[spl].tier = tier;
	spell_info[spl].min_level = level;
	spell_info[spl].gm = gm;
        spell_info[spl].wear_off_msg = wear_off_msg;
        spell_info[spl].type = 1;   //1 for spells
}

void spello_system ( int spl, const char *name, int max_mana, int min_mana,
              int mana_change, int minpos, int targets, int violent,
              int routines, int w, int first_prereq, int second_prereq, int tier, int level, int gm , const char *wear_off_msg)
{
	spell_info[spl].mana_max = max_mana;
	spell_info[spl].mana_min = min_mana;
	spell_info[spl].mana_change = mana_change;
	spell_info[spl].min_position = minpos;
	spell_info[spl].targets = targets;
	spell_info[spl].violent = violent;
	spell_info[spl].routines = routines;
	spell_info[spl].name = name;
	spell_info[spl].wait = w;
	spell_info[spl].first_prereq = first_prereq;
	spell_info[spl].second_prereq = second_prereq;
	spell_info[spl].tier = tier;
	spell_info[spl].min_level = level;
	spell_info[spl].gm = gm;
        spell_info[spl].wear_off_msg = wear_off_msg;
        spell_info[spl].type = 3;   //0 for non-castable spells
}


void unused_spell ( int spl )
{

	spell_info[spl].mana_max = 0;
	spell_info[spl].mana_min = 0;
	spell_info[spl].mana_change = 0;
	spell_info[spl].min_position = 0;
	spell_info[spl].targets = 0;
	spell_info[spl].violent = 0;
	spell_info[spl].routines = 0;
	spell_info[spl].name = unused_spellname;
	spell_info[spl].first_prereq = TYPE_UNDEFINED;
	spell_info[spl].second_prereq = TYPE_UNDEFINED;
	spell_info[spl].tier = 0;
	spell_info[spl].wait = 0;
	spell_info[spl].classes = 0;

}

//#define skillo(skill, name) spello(skill, name, 0, 0, 0, 0, 0, 0, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0);


/*
 * Arguments for spello calls:
 *
 * spellnum, maxmana, minmana, manachng, minpos, targets, violent?, routines, wait (in seconds).
 *
 * spellnum:  Number of the spell.  Usually the symbolic name as defined in
 * spells.h (such as SPELL_HEAL).
 *
 * spellname: The name of the spell.
 *
 * maxmana :  The maximum mana this spell will take (i.e., the mana it
 * will take when the player first gets the spell).
 *
 * minmana :  The minimum mana this spell will take, no matter how high
 * level the caster is.
 *
 * manachng:  The change in mana for the spell from level to level.  This
 * number should be positive, but represents the reduction in mana cost as
 * the caster's level increases.
 *
 * minpos  :  Minimum position the caster must be in for the spell to work
 * (usually fighting or standing). targets :  A "list" of the valid targets
 * for the spell, joined with bitwise OR ('|').
 *
 * violent :  TRUE or FALSE, depending on if this is considered a violent
 * spell and should not be cast in PEACEFUL rooms or on yourself.  Should be
 * set on any spell that inflicts damage, is considered aggressive (i.e.
 * charm, curse), or is otherwise nasty.
 *
 * routines:  A list of magic routines which are associated with this spell
 * if the spell uses spell templates.  Also joined with bitwise OR ('|').
 * 
 * gm : GMs can get these skills or not?
 *
 * wear_off_msg : Moved to here for easy declaration.
 *
 * See the CircleMUD documentation for a more detailed description of these
 * fields.
 */


void mag_assign_spells ( void )
{
	int i;

	/* Do not change the loop below */
	for ( i = 0; i <= MAX_SKILLS; i++ )
		spell_info[i] = spell_info_type();
	/* Do not change the loop above */

	spello ( SPELL_VITALIZE, "vitalize", 60 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_AREA_DIR, FALSE, MAG_POINTS, 1,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 30, 0, "" );

	spello ( SPELL_ABSOLVE, "absolve", 60 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 100,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 30, 1, "" );

	spello ( SPELL_ANIMATE_DEAD, "animate dead", 35 , 10 , 3,
	         POS_STANDING, TAR_OBJ_ROOM, FALSE, MAG_SUMMONS, 120,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 40, 0, "" );

	spello ( SPELL_ARMOR, "armor", 30 , 15 , 3, POS_FIGHTING,
	         TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 30,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 25, 1,
                 "Your magical armor dissipates." );

	spello ( SPELL_BLESS, "bless", 35 , 5 , 3, POS_STANDING,
	         TAR_CHAR_ROOM | TAR_OBJ_INV, FALSE,
	         MAG_AFFECTS | MAG_ALTER_OBJS, 20, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 15, 1,
                 "You feel less righteous!" );

	spello ( SPELL_SUFFOCATE, "suffocate", 35 , 5 , 3, POS_STANDING,
	         TAR_CHAR_ROOM, TRUE,
	         MAG_AFFECTS , 30, TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 41, 0,
                 "You gasp and feel fresh air flood into your lungs!" );

	spello ( SPELL_BLINDNESS, "blindness", 35 , 25 , 1,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_NOT_SELF, FALSE, MAG_AFFECTS,
	         30, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 50, 1,
                 "Your vision returns." );

	spello ( SPELL_BURNING_HANDS, "burning hands", 30 , 10 , 3,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_DAMAGE | MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 30, 1,
                 "Magical flames stop burning your skin." );

	spello ( SPELL_CALL_LIGHTNING, "call lightning", 40 , 25 ,
	         3, POS_FIGHTING, TAR_IGNORE, TRUE,
	         MAG_MANUAL, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 32, 1, "" );

	spello ( SPELL_CHAIN_LIGHTNING, "chain lightning", 60 ,
	         30 , 3, POS_FIGHTING,
	         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT, TRUE, MAG_MANUAL,
	         30, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 34, 0, "" );

	spello ( SPELL_CHARM, "charm", 75 , 50 , 2, POS_FIGHTING,
	         TAR_CHAR_ROOM | TAR_NOT_SELF, TRUE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 30, 0,
                 "You feel more self-confident." );

	spello ( SPELL_CHILL_TOUCH, "chill touch", 30 , 10 , 3,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_DAMAGE | MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 2, 1, "" );

	spello ( SPELL_CLONE, "clone", 80 , 65 , 5, POS_STANDING,
	         TAR_CHAR_ROOM | TAR_IGNORE, FALSE, MAG_SUMMONS, 180,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 5, 1 , "");

	spello ( SPELL_COLOUR_SPRAY, "colour spray", 30 , 15 , 3,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE | MAG_AFFECTS,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 20, 1 , "");

	spello ( SPELL_CONTROL_WEATHER, "control weather", 75 ,
	         25 , 5, POS_STANDING, TAR_IGNORE, FALSE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 20, 0, "" );

	spello ( SPELL_CREATE_FOOD, "create food", 30 , 5 , 4,
	         POS_STANDING, TAR_IGNORE, FALSE, MAG_CREATIONS, 1,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 21, 1, "" );

	spello ( SPELL_WALL_FORCE, "wall of force", 30 , 5 , 4,
	         POS_STANDING, TAR_IGNORE, TRUE, MAG_CREATIONS, 1,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 21, 0, "" );

	spello ( SPELL_WALL_FIRE, "wall of fire", 30 , 5 , 4,
	         POS_STANDING, TAR_IGNORE, TRUE, MAG_CREATIONS, 1,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 21, 0, "" );

	spello ( SPELL_DARKNESS, "darkness", 30 , 5 , 4,
	         POS_STANDING, TAR_IGNORE, TRUE, MAG_ROOM_AFFECTS, 1,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 21, 0, "" );

	spello ( SPELL_CREATE_WATER, "create water", 30 , 5 , 4,
	         POS_STANDING, TAR_OBJ_INV | TAR_OBJ_EQUIP, FALSE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 28, 1, "" );

	spello ( SPELL_CURE_BLIND, "cure blind", 30 , 5 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_UNAFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 5, 1, "" );

	spello ( SPELL_CURE_CRITIC, "cure critic", 50 , 20 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM, FALSE, MAG_POINTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 10, 1, "" );

	spello ( SPELL_CURE_LIGHT, "cure light", 30 , 10 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM, FALSE, MAG_POINTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 5, 1, "" );

	spello ( SPELL_CURSE, "curse", 80 , 50 , 2, POS_STANDING,
	         TAR_CHAR_ROOM | TAR_OBJ_INV, TRUE, MAG_AFFECTS | MAG_ALTER_OBJS,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 9, 0, 
                 "You feel more optimistic." );

	spello ( SPELL_DETECT_ALIGN, "detect alignment", 20 , 10 ,
	         2, POS_STANDING, TAR_CHAR_ROOM | TAR_SELF_ONLY, FALSE,
	         MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 25, 1,
                 "You no longer are attuned to the alignment of others." );

	spello ( SPELL_DETECT_INVIS, "detect invisibility", 20 ,
	         10 , 2, POS_STANDING, TAR_CHAR_ROOM | TAR_SELF_ONLY,
	       FALSE, MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 15, 1,
                 "Your eyes stop tingling." );

	spello ( SPELL_DETECT_MAGIC, "detect magic", 20 , 10 , 2,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_SELF_ONLY, FALSE, MAG_AFFECTS,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 35, 1,
                 "You have lost your sensitivity to magic." );

	spello ( SPELL_DETECT_POISON, "detect poison", 15 , 5 , 1,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_OBJ_INV | TAR_OBJ_ROOM, FALSE,
	         MAG_MANUAL, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 3, 1,
                 "You no longer can detect poison." );

	spello ( SPELL_DISPEL_EVIL, "dispel evil", 40 , 25 , 3,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 30, 1, "" );

	spello ( SPELL_DISPEL_GOOD, "dispel good", 40 , 25 , 3,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 30, 0, "" );

	spello ( SPELL_EARTHQUAKE, "earthquake", 40 , 25 , 3,
	         POS_FIGHTING, TAR_IGNORE, TRUE, MAG_AREAS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 24, 1, "" );

	spello ( SPELL_ENCHANT_WEAPON, "enchant weapon", 150 ,
	         100 , 10, POS_STANDING, TAR_OBJ_INV | TAR_OBJ_EQUIP,
	         FALSE, MAG_MANUAL, 30, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 40, 0, "" );

	spello ( SPELL_MINOR_IDENTIFY, "minor identify", 75 , 30 ,
	         5, POS_RESTING, TAR_OBJ_INV | TAR_OBJ_ROOM, FALSE, MAG_MANUAL,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 4, 1, "" );

	spello ( SPELL_ENERGY_DRAIN, "energy drain", 40 , 25 , 1,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 30, 0, "" );

	spello ( SPELL_EVIL_EYE, "evil eye", 60 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 200,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 40, 0, "" );

	spello ( SPELL_GROUP_ARMOR, "group armor", 50 , 30 , 2,
	         POS_STANDING, TAR_IGNORE, FALSE, MAG_GROUPS, 20,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 20, 0, "" );

	spello ( SPELL_FIREBALL, "fireball", 30 , 20 , 5,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, 
                 MAG_DAMAGE | MAG_ROOM_AFFECTS,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 40, 1, "" );

	spello ( SPELL_SOULSMASH, "soulsmash", 70 , 60 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 31, 0, "" );

	spello ( SPELL_DEMONSHRIEK, "demonshriek", 70 , 60 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 32, 0, "" );

	spello ( SPELL_LIFESUCK, "lifesuck", 70 , 60 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 48, 1, "" );

	spello ( SPELL_BURNINGSKULL, "burning skull", 70 , 60 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         5, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 24, 0, "" );

	spello ( SPELL_HEARTSQUEEZE, "heart squeeze", 70 , 60 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         10, TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 7, 0, "" );

	spello ( SPELL_FACEMELT, "facemelt", 70 , 60 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         10, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 42, 0, "" );

	spello ( SPELL_ELECTRIC_BLAST, "electric blast", 140 , 125 , 3,
	         POS_FIGHTING, TAR_IGNORE | TAR_AREA_DIR, TRUE, MAG_AREAS, 100,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 15, 0, "" );

	// Changing the delay in Inferno to a shorter delay - Prom
	spello ( SPELL_INFERNO, "inferno", 140 , 125 , 3,
	         POS_FIGHTING, TAR_IGNORE | TAR_AREA_DIR, TRUE, MAG_AREAS, 30,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 15, 0, "" );


	spello ( SPELL_WATER_TO_WINE, "water to wine", 150 ,
	         100 , 10, POS_STANDING, TAR_OBJ_INV | TAR_OBJ_EQUIP,
	         FALSE, MAG_MANUAL, 30, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 28, 1, "" );

	spello ( SPELL_MIDAS_TOUCH, "midas touch", 150 ,
	         100 , 10, POS_FIGHTING, TAR_CHAR_ROOM| TAR_FIGHT_VICT| TAR_NOT_SELF,
	         FALSE, MAG_MANUAL, 30, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 4, 0, "" );


	spello ( SPELL_GROUP_HEAL, "group heal", 80 , 40 , 5,
	         POS_FIGHTING, TAR_IGNORE, FALSE, MAG_GROUPS, 0,
	         SPELL_HEAL, TYPE_UNDEFINED, 4, 3, 0, "" );

	spello ( SPELL_GROUP_RECALL, "group recall", 90 , 30 , 2,
	         POS_STANDING, TAR_IGNORE, FALSE, MAG_GROUPS, 20,
	         SPELL_WORD_OF_RECALL, TYPE_UNDEFINED, 3, 24, 0, "" );

	spello ( SPELL_HARM, "harm", 75 , 45 , 3, POS_FIGHTING,
	         TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 30, 1, "" );

	spello ( SPELL_HEAL, "heal", 70 , 35 , 3, POS_FIGHTING,
	         TAR_CHAR_ROOM, FALSE, MAG_POINTS | MAG_UNAFFECTS, 1,
	         SPELL_CURE_CRITIC, TYPE_UNDEFINED, 3, 32, 0, "" );

	spello ( SPELL_INFRAVISION, "infravision", 25 , 10 , 1,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_SELF_ONLY, FALSE, MAG_AFFECTS,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 3, 1, 
                 "Your night vision seems to fade." );

	spello ( SPELL_INVISIBLE, "invisibility", 35 , 25 , 1,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_OBJ_INV | TAR_OBJ_ROOM, FALSE,
	         MAG_AFFECTS | MAG_ALTER_OBJS, 20, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 32, 1,
                 "You feel yourself exposed." );

	spello ( SPELL_LIGHTNING_BOLT, "lightning bolt", 40 , 15 ,
	         1, POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_DAMAGE, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 37, 0, "" );

	spello ( SPELL_LOCATE_OBJECT, "locate object", 25 , 20 , 1,
	         POS_STANDING, TAR_OBJ_WORLD, FALSE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 6, 0, "" );

	spello ( SPELL_MAGIC_MISSILE, "magic missile", 70 , 30 , 4,
	         POS_FIGHTING, TAR_AREA_DIR | TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 20, 1, "" );

	spello ( SPELL_POISON, "poison", 50 , 20 , 3, POS_STANDING,
	         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_OBJ_INV, TRUE,
	         MAG_AFFECTS | MAG_ALTER_OBJS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 40, 1,
                 "You feel less sick." );

	spello ( SPELL_POISON_2, "poison 2", 50 , 20 , 3,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_OBJ_INV, TRUE,
	         MAG_AFFECTS | MAG_ALTER_OBJS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 45, 0,
                 "You feel less sick!!" );

	spello ( SPELL_POISON_3, "poison 3", 50 , 20 , 3,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_OBJ_INV, TRUE,
	         MAG_AFFECTS | MAG_ALTER_OBJS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 48, 0, 
                 "You feel less sick!!!" );

	spello ( SPELL_POISON_4, "poison 4", 50 , 20 , 3,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_OBJ_INV, TRUE,
	         MAG_AFFECTS | MAG_ALTER_OBJS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 51, 0,
                 "You feel less sick!!!!" );

	spello ( SPELL_LOCATE_PERSON, "locate person", 100 , 50 , 3,
	         POS_STANDING, TAR_CHAR_WORLD, FALSE, MAG_MANUAL,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 4, 1, "" );

	spello ( SPELL_PROT_FROM_EVIL, "protection from evil", 40 ,
	         10 , 3, POS_STANDING, TAR_CHAR_ROOM | TAR_SELF_ONLY,
	         FALSE, MAG_AFFECTS, 200, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 2, 1,
                 "Your ward against evil dissipates." );

	spello ( SPELL_PROT_FROM_GOOD, "protection from good", 40 ,
	         10 , 3, POS_STANDING, TAR_CHAR_ROOM | TAR_SELF_ONLY,
	         FALSE, MAG_AFFECTS, 200, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 2, 0,
                 "You feel your shell of warmth dissipate." );

	spello ( SPELL_REMOVE_CURSE, "remove curse", 45 , 25 , 5,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_OBJ_INV | TAR_OBJ_EQUIP,
	         FALSE, MAG_UNAFFECTS | MAG_ALTER_OBJS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 5, 1, "" );

	spello ( SPELL_SANCTUARY, "sanctuary", 110 , 50 , 5,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 20,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 10, 0,
                 "The glowing white aura around your body fades." );

	spello ( SPELL_SHOCKING_GRASP, "shocking grasp", 30 , 15 ,
	         3, POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_DAMAGE, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 31, 1,"" );

	spello ( SPELL_SLEEP, "sleep", 40 , 25 , 5, POS_STANDING,
	         TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 180, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 12, 1,
                 "You feel less tired." );

	spello ( SPELL_STRENGTH, "strength", 60 , 40 , 1,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 25, 1,
                 "You lose your magical strength boost." );

	spello ( SPELL_SUMMON, "summon", 75 , 50 , 3, POS_STANDING,
	         TAR_CHAR_WORLD | TAR_NOT_SELF, FALSE, MAG_MANUAL, 30,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 5, 0, "" );

	spello ( SPELL_TELEPORT, "teleport", 75 , 50 , 3,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 10, 1, "" );

	spello ( SPELL_WATERWALK, "waterwalk", 80 , 20 , 2,
	         POS_FIGHTING, TAR_SELF_ONLY, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 15, 0,
                 "Your feet seem less buoyant." );

	spello ( SPELL_MIND_FIRE, "mind fire", 80 , 20 , 2,
	         POS_FIGHTING, TAR_SELF_ONLY, FALSE, MAG_AFFECTS, 400,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 4, 0,
                 "The burning sensation in your skull fades away." );

	spello ( SPELL_MIND_ELEC, "mind electricity", 80 , 20 , 2,
	         POS_FIGHTING, TAR_SELF_ONLY, FALSE, MAG_AFFECTS, 500,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 8, 0,
                 "Your mind stops crackling." );

	spello ( SPELL_MIND_WATER, "mind water", 80 , 20 , 2,
	         POS_FIGHTING, TAR_SELF_ONLY, FALSE, MAG_AFFECTS, 300,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 8, 0, 
                 "Your mind dries out." );

	spello ( SPELL_MIND_ICE, "mind ice", 80 , 20 , 2,
	         POS_FIGHTING, TAR_SELF_ONLY, FALSE, MAG_AFFECTS, 500,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 12, 0 ,
                 "Your mind thaws out.");

	spello ( SPELL_WORD_OF_RECALL, "word of recall", 20 , 10 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM, FALSE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 3, 1, "" );

	spello ( SPELL_ANTIDOTE_1, "antidote 1", 40 , 8 , 4,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_OBJ_INV | TAR_OBJ_ROOM, FALSE,
	         MAG_UNAFFECTS | MAG_ALTER_OBJS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 34, 1, "" );

	spello ( SPELL_ANTIDOTE_2, "antidote 2", 80 , 16 , 8,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_UNAFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 34, 1, "" );

	spello ( SPELL_ANTIDOTE_3, "antidote 3", 160 , 32 , 16,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_UNAFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 34, 0, "" );

	spello ( SPELL_SENSE_LIFE, "sense life", 20 , 10 , 2,
	         POS_STANDING, TAR_CHAR_ROOM | TAR_SELF_ONLY, FALSE, MAG_AFFECTS,
	         140, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 41, 1,
                 "You feel less aware of your surroundings." );

	spello ( SPELL_GATE, "gate", 50 , 50 , 0, POS_STANDING,
	         TAR_CHAR_WORLD | TAR_NOT_SELF, FALSE, MAG_MANUAL, 30,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 40, 0, "" );

	spello ( SPELL_REMOVE_ALIGNMENT, "remove alignment", 75 ,
	         25 , 5, POS_STANDING, TAR_OBJ_INV, FALSE,
	         MAG_MANUAL, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 46, 1, "" );

	spello ( SPELL_EARTH_ELEMENTAL, "earth elemental", 120 ,
	         60 , 2, POS_STANDING, TAR_IGNORE, FALSE, MAG_SUMMONS, 600,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 25, 0 ,
                 "The earth elemental disappears.");

	spello ( SPELL_WATER_ELEMENTAL, "water elemental", 100 ,
	         60 , 2, POS_STANDING, TAR_IGNORE, FALSE, MAG_SUMMONS, 600,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 22, 1,
                 "The water elemental disappears." );

	spello ( SPELL_AIR_ELEMENTAL, "air elemental", 90 , 60 , 2,
	         POS_STANDING, TAR_IGNORE, FALSE, MAG_SUMMONS, 600,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 25, 1,
                 "The air elemental disappears." );

	spello ( SPELL_FIRE_ELEMENTAL, "fire elemental", 130 , 60 ,
	         2, POS_STANDING, TAR_IGNORE, FALSE, MAG_SUMMONS, 600,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 25, 0,
                 "The fire elemental disappears." );

	spello ( SPELL_RECHARGE, "recharge", 100 , 80 , 2,
	         POS_RESTING, TAR_OBJ_INV | TAR_OBJ_ROOM, FALSE, MAG_MANUAL, 600,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 43, 1, "" );

	spello ( SPELL_METEOR_SHOWER, "meteor shower", 60 , 40 , 2,
	         POS_STANDING, TAR_IGNORE, TRUE, MAG_AREAS, 40,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 42, 0, "" );

	spello ( SPELL_STONESKIN, "stoneskin", 35 , 15 , 3,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 47, 1,
                 "Your skin of rock crumbles." );

	spello ( SPELL_STEELSKIN, "steelskin", 45 , 20 , 3,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 48, 0, 
                 "Your skin of steel becomes more supple." );

	spello ( SPELL_HOLD_PERSON, "hold person", 70 , 40 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_NOT_SELF, TRUE, MAG_AFFECTS,
	         120, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 16, 0,
                 "You are freed of your bindings!" );

	spello ( SPELL_PARALYZE, "paralyze", 100 , 65 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_NOT_SELF, TRUE, MAG_AFFECTS,
	         300, TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 29, 0,
                 "You regain control of your muscles!" );

	spello ( SPELL_HOLY_WORD, "holy word", 25 , 15 , 3,
	         POS_FIGHTING, TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE, MAG_DAMAGE,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 13, 1, "" );

	spello ( SPELL_HOLY_SHOUT, "holy shout", 65 , 45 , 2,
	         POS_FIGHTING, TAR_IGNORE, TRUE, MAG_AREAS, 0,
	         SPELL_HOLY_WORD, TYPE_UNDEFINED, 3, 13, 0, "" );

	spello ( SPELL_HASTE, "haste", 50 , 35 , 1, POS_STANDING,
	         TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 70,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 16, 1, 
                 "You feel sluggish again." );

	spello ( SPELL_SHIELD, "shield", 30 , 20 , 2, POS_STANDING,
	         TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 7, 1,
                 "Your shield of force crumbles to dust." );

	spello ( SPELL_GROUP_SHIELD, "group shield", 50 , 35 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_GROUPS, 0,
	         SPELL_SHIELD, TYPE_UNDEFINED, 3, 12, 0, "" );

	spello ( SPELL_ACID_ARROW, "acid arrow", 30 , 20 , 3,
	         POS_FIGHTING, TAR_AREA_DIR | TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_DAMAGE | MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 50, 1, "" );

	spello ( SPELL_FLAME_ARROW, "flame arrow", 35 , 20 , 3,
	         POS_FIGHTING, TAR_AREA_DIR | TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_DAMAGE | MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 43, 0, "" );

	spello ( SPELL_CONE_OF_COLD, "cone of cold", 30 , 10 , 3,
	         POS_FIGHTING, TAR_AREA_DIR | TAR_CHAR_ROOM | TAR_FIGHT_VICT, TRUE,
	         MAG_DAMAGE | MAG_AFFECTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 35, 1, "" );

	spello ( SPELL_KNOCK, "knock", 30 , 10 , 3, POS_FIGHTING,
	         TAR_IGNORE | TAR_OBJ_INV | TAR_OBJ_ROOM, TRUE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 43, 1, "" );

	spello ( SPELL_PROT_FIRE, "protection from fire", 20 , 10 ,
	         2, POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 4, 1, 
                 "You feel less insulated." );

	spello ( SPELL_PROT_COLD, "protection from cold", 20 , 10 ,
	         2, POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 16, 1, 
                 "You feel your shell of warmth dissipate." );

	spello ( SPELL_FIRE_SHIELD, "fire shield", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 37, 0,
                 "Your protective shield of fire fades." );

	spello ( SPELL_LIFE_TRANSFER, "life transfer", 30 , 15 , 3,
	         POS_STANDING, TAR_AREA_DIR|TAR_CHAR_ROOM | TAR_NOT_SELF, FALSE, MAG_POINTS,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 1, 1, "" );

	spello ( SPELL_MANA_TRANSFER, "mana transfer", 20 , 10 , 2,
	         POS_STANDING, TAR_AREA_DIR | TAR_CHAR_ROOM | TAR_NOT_SELF, FALSE, MAG_POINTS,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 30, 1, "" );


	spello ( SPELL_PROT_FROM_GOOD, "protection from good", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 4, 0,
                 "Your unholy aura dissipates." );

	spello ( SPELL_SHIELD_ICE, "ice shield", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 16, 0,
                 "Your shield of ice melts." );

	spello ( SPELL_SHIELD_THORN, "thorn shield", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 13, 1,
                 "The thorns around you wither and die." );

	spello ( SPELL_SHIELD_MANA, "mana shield", 50 , 30 , 2,
	         POS_STANDING,  TAR_SELF_ONLY, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 10, 0,
                 "Your pulsating mana shield dissipates." );

	spello ( SPELL_SHIELD_MIRROR, "mirror shield", 50 , 30 , 2,
	         POS_STANDING,  TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 3, 1,
                 "Your mirror shield shatters." );

	spello ( SPELL_SHIELD_HOLY, "holy shield", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 22, 1,
                 "The geas is lifted." );

	spello ( SPELL_SHIELD_STATIC, "static shield", 50 , 30 , 2,
	         POS_STANDING,  TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 20, 1,
                 "Your static shield fizzles out." );

	spello ( SPELL_FORTIFY_MIND, "fortify mind", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 2, 1,
                 "Your mind's protection crumbles away." );

	spello ( SPELL_FORTIFY_BODY, "fortify body", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 12, 1,
                 "Your body's protection crumbles away." );

	spello ( SPELL_SWEET_DREAMS, "sweet dreams", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 240,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 23, 1,
                 "You wake up from the most refreshing dream." );

	spello ( SPELL_DEVINE_MIND, "devine mind", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 5, 1,
                 "Your devine guidance leaves you." );

	spello ( SPELL_NUMB_MIND, "numb mind", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 1, 0,
                 "Your mind tingles as feelings returns to it." );

	spello ( SPELL_SLOW, "slowness", 50 , 30 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 48, 0,
                 "The feelings of lethargy is lifted." );

	spello ( SPELL_FLIGHT, "flight", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 3, 0,
                 "Your magical wings shrivel and fall off." );

	spello ( SPELL_BATTLE_RAGE, "battle rage", 50 , 30 , 2,
	         POS_FIGHTING, TAR_CHAR_ROOM, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 40, 1,
                 "You calm down from your battle rage." );

	spello ( SPELL_ENCHANT_ARMOR, "embue armor", 100 , 80 , 2,
	         POS_RESTING, TAR_OBJ_INV | TAR_OBJ_ROOM, FALSE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 30, 1, "" );

	spello ( SPELL_MAGIC_BUBBLE, "magic bubble", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 14, 0,
                 "The magic bubble bursts!" );


	spello ( SPELL_PSI_PANIC, "psi panic", 100 , 80 , 2,
	         POS_RESTING, TAR_CHAR_ROOM, FALSE, MAG_MANUAL, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 19, 0,
                 "You stop panicking." );

	spello ( SPELL_NIGHTMARE, "nightmare", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_DAMAGE, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 26, 0,
                 "Reality chases your nightmares away." );

	spello ( SPELL_DISPELL_SANCTURY, "dispel sanctuary", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_UNAFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 3, 21, 0, "" );


	spello ( SPELL_FORSEE, "forsee", 50 , 30 , 2,
	         POS_STANDING, TAR_SELF_ONLY, FALSE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 3, 1,
                 "You lose your sense of anticipation." );

	spello ( SPELL_MANA_BLAST, "mana blast", 900 , 450 , 30,
	         POS_FIGHTING, TAR_IGNORE, TRUE, MAG_AREAS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 4, 50, 0 , "");

	spello ( SPELL_CONFUSE, "confuse", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 10, 1,
                 "You feel less confused." );

	spello ( SPELL_CORRUPT_ARMOR, "corrupt armor", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 1, 12, 1,
                 "The aura of corruption fades." );

	spello ( SPELL_WEAKEN, "weaken", 50 , 30 , 2,
	         POS_STANDING, TAR_CHAR_ROOM, TRUE, MAG_AFFECTS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 2, 17, 1,
                 "You feel your strength return." );


	/* NON-castable spells should appear here */
	spello_system ( SPELL_IDENTIFY, "identify", 0, 0, 0, POS_RESTING,
	         TAR_CHAR_ROOM | TAR_OBJ_INV | TAR_OBJ_ROOM, FALSE, MAG_MANUAL,
	         0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_FIRE_BREATH, "fire breath", 0, 0, 0, POS_FIGHTING,
	         TAR_IGNORE, TRUE, MAG_AREAS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );
	spello_system ( SPELL_GAS_BREATH, "gas breath", 0, 0, 0, POS_FIGHTING,
	         TAR_IGNORE, TRUE, MAG_AREAS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_FROST_BREATH, "frost breath", 0, 0, 0, POS_FIGHTING,
	         TAR_IGNORE, TRUE, MAG_AREAS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_ACID_BREATH, "acid breath", 0, 0, 0, POS_FIGHTING,
	         TAR_IGNORE, TRUE, MAG_AREAS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_LIGHTNING_BREATH, "lightning breath", 0, 0, 0,
	         POS_FIGHTING, TAR_IGNORE, TRUE, MAG_AREAS, 0,
	         TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0 , "");

	spello_system ( SPELL_ACID, "acid", 0, 0, 0, 0,
	         TAR_SELF_ONLY, FALSE, MAG_POINTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_BURN, "burn", 0, 0, 0, 0,
	         TAR_SELF_ONLY, FALSE, MAG_POINTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_FREEZE, "freeze", 0, 0, 0, 0,
	         TAR_SELF_ONLY, FALSE, MAG_POINTS, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0 , "");

	spello_system ( SPELL_POLYMORPH, "polymorph", 0, 0, 0, 0,
	         TAR_CHAR_ROOM, TRUE, MAG_MANUAL, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_DG_AFFECT, "Malactation", 0, 0, 0, POS_SITTING,
	         TAR_IGNORE, TRUE, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );
	spello_system ( SPELL_IMMFREEZE, "Imm Freeze", 0, 0, 0, POS_SITTING,
	         TAR_IGNORE, TRUE, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );
	spello_system ( SPELL_SILENCED, "Silenced", 0, 0, 0, POS_SITTING,
	         TAR_IGNORE, TRUE, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_OUTCAST, "OUTCAST", 0, 0, 0, POS_SITTING,
	         TAR_IGNORE, TRUE, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "" );

	spello_system ( SPELL_RESIST_FIRE, "resist fire", 0, 0, 0, POS_SITTING,
	         TAR_IGNORE, TRUE, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "The flow of water around your body evaporates into the air." );

	spello_system ( SPELL_RESIST_COLD, "resist cold", 0, 0, 0, POS_SITTING,
	         TAR_IGNORE, TRUE, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "The flow of flames around your body flicker out." );

	spello_system ( SPELL_RESIST_ELEC, "resist electricity", 0, 0, 0, POS_SITTING,
	         TAR_IGNORE, TRUE, 0, 0, TYPE_UNDEFINED, TYPE_UNDEFINED, 0, 0, 0, "Your glass-coated skin shatters into a million pieces." );

	/*
	 * Declaration of skills - this actually doesn't do anything except
	 * set it up so that immortals can use these skills by default.  The
	 * min level to use the skill for other classes is set up in class.c.
	 */



}

int elemental_type ( int spell )
{
	int retval = ELEM_NONE;
	switch ( spell )
	{
		case SPELL_ARMOR:
			retval = ELEM_EARTH;
			break;
		case SPELL_TELEPORT:
			retval = ELEM_MIND;
			break;
		case SPELL_BLINDNESS:
			retval = ELEM_LIGHT;
			break;
		case SPELL_CLONE:
			retval = ELEM_MIND;
			break;
		case SPELL_BLESS:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_BURNING_HANDS:
			retval = ELEM_FIRE;
			break;
		case SPELL_CALL_LIGHTNING:
			retval = ELEM_ELEC;
			break;
		case SPELL_CHARM:
			retval = ELEM_MIND;
			break;
		case SPELL_CHILL_TOUCH:
			retval = ELEM_ICE;
			break;
		case SPELL_COLOUR_SPRAY:
			retval = ELEM_LIGHT;
			break;
		case SPELL_CONTROL_WEATHER:
			retval = ELEM_AIR;
			break;
		case SPELL_CREATE_FOOD:
			retval = ELEM_EARTH;
			break;
		case SPELL_CREATE_WATER:
			retval = ELEM_WATER;
			break;
		case SPELL_CURE_BLIND:
			retval = ELEM_LIGHT;
			break;
		case SPELL_CURE_CRITIC:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_CURE_LIGHT:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_CURSE:
			retval = ELEM_MIND;
			break;
		case SPELL_DETECT_ALIGN:
			retval = ELEM_DARK;
			break;
		case SPELL_DETECT_INVIS:
			retval = ELEM_DARK;
			break;
		case SPELL_DETECT_MAGIC:
			retval = ELEM_DARK;
			break;
		case SPELL_DETECT_POISON:
			retval = ELEM_DARK;
			break;
		case SPELL_DISPEL_EVIL:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_EARTHQUAKE:
			retval = ELEM_EARTH;
			break;
		case SPELL_ENCHANT_WEAPON:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_ENERGY_DRAIN:
			retval = ELEM_DEATH;
			break;
		case SPELL_FIREBALL:
			retval = ELEM_FIRE;
			break;
		case SPELL_HARM:
			retval = ELEM_DEATH;
			break;
		case SPELL_HEAL:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_INVISIBLE:
			retval = ELEM_LIGHT;
			break;
		case SPELL_LIGHTNING_BOLT:
			retval = ELEM_ELEC;
			break;
		case SPELL_LOCATE_OBJECT:
			retval = ELEM_MIND;
			break;
		case SPELL_MAGIC_MISSILE:
			retval = ELEM_AIR;
			break;
		case SPELL_POISON:
			retval = ELEM_WATER;
			break;
		case SPELL_PROT_FROM_EVIL:
			retval = ELEM_LIGHT;
			break;
		case SPELL_REMOVE_CURSE:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_SANCTUARY:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_SHOCKING_GRASP:
			retval = ELEM_ELEC;
			break;
		case SPELL_SLEEP:
			retval = ELEM_MIND;
			break;
		case SPELL_STRENGTH:
			retval = ELEM_EARTH;
			break;
		case SPELL_SUMMON:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_SUFFOCATE:
			retval = ELEM_AIR;
			break;
		case SPELL_WORD_OF_RECALL:
			retval = ELEM_MIND;
			break;
		case SPELL_ANTIDOTE_1:
			retval = ELEM_FIRE;
			break;
		case SPELL_SENSE_LIFE:
			retval = ELEM_LIGHT;
			break;
		case SPELL_ANIMATE_DEAD:
			retval = ELEM_DEATH;
			break;
		case SPELL_DISPEL_GOOD:
			retval = ELEM_FIRE;
			break;
		case SPELL_GROUP_ARMOR:
			retval = ELEM_EARTH;
			break;
		case SPELL_GROUP_HEAL:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_GROUP_RECALL:
			retval = ELEM_MIND;
			break;
		case SPELL_INFRAVISION:
			retval = ELEM_LIGHT;
			break;
		case SPELL_WATERWALK:
			retval = ELEM_WATER;
			break;
		case SPELL_GATE:
			retval = ELEM_MIND;
			break;
		case SPELL_MINOR_IDENTIFY:
			retval = ELEM_MIND;
			break;
		case SPELL_REMOVE_ALIGNMENT:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_LOCATE_PERSON:
			retval = ELEM_MIND;
			break;
		case SPELL_POISON_2:
			retval = ELEM_ICE;
			break;
		case SPELL_POISON_3:
			retval = ELEM_AIR;
			break;
		case SPELL_POISON_4:
			retval = ELEM_DEATH;
			break;
		case SPELL_ANTIDOTE_2:
			retval = ELEM_FIRE;
			break;
		case SPELL_ANTIDOTE_3:
			retval = ELEM_EARTH;
			break;
		case SPELL_EVIL_EYE:
			retval = ELEM_DEATH;
			break;
		case SPELL_ABSOLVE:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_CHAIN_LIGHTNING:
			retval = ELEM_ELEC;
			break;
		case SPELL_RECHARGE:
			retval = ELEM_ELEC;
			break;
		case SPELL_METEOR_SHOWER:
			retval = ELEM_AIR;
			break;
		case SPELL_STONESKIN:
			retval = ELEM_EARTH;
			break;
		case SPELL_STEELSKIN:
			retval = ELEM_EARTH;
			break;
		case SPELL_HOLD_PERSON:
			retval = ELEM_ICE;
			break;
		case SPELL_PARALYZE:
			retval = ELEM_ICE;
			break;
		case SPELL_HOLY_WORD:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_HOLY_SHOUT:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_HASTE:
			retval = ELEM_AIR;
			break;
		case SPELL_SHIELD:
			retval = ELEM_AIR;
			break;
		case SPELL_GROUP_SHIELD:
			retval = ELEM_AIR;
			break;
		case SPELL_ACID_ARROW:
			retval = ELEM_WATER;
			break;
		case SPELL_FLAME_ARROW:
			retval = ELEM_FIRE;
			break;
		case SPELL_CONE_OF_COLD:
			retval = ELEM_ICE;
			break;
		case SPELL_KNOCK:
			retval = ELEM_AIR;
			break;
		case SPELL_PROT_FIRE:
			retval = ELEM_WATER;
			break;
		case SPELL_PROT_COLD:
			retval = ELEM_FIRE;
			break;
		case SPELL_EARTH_ELEMENTAL:
			retval = ELEM_EARTH;
			break;
		case SPELL_WATER_ELEMENTAL:
			retval = ELEM_WATER;
			break;
		case SPELL_AIR_ELEMENTAL:
			retval = ELEM_AIR;
			break;
		case SPELL_FIRE_ELEMENTAL:
			retval = ELEM_FIRE;
			break;
		case SPELL_FIRE_SHIELD:
			retval = ELEM_FIRE;
			break;
		case SPELL_LIFE_TRANSFER:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_MANA_TRANSFER:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_PROT_FROM_GOOD:
			retval = ELEM_DEATH;
			break;
		case SPELL_MIND_FIRE:
			retval = ELEM_FIRE;
			break;
		case SPELL_MIND_ELEC:
			retval = ELEM_ELEC;
			break;
		case SPELL_MIND_WATER:
			retval = ELEM_WATER;
			break;
		case SPELL_MIND_ICE:
			retval = ELEM_ICE;
			break;
		case SPELL_SHIELD_ICE:
			retval = ELEM_ICE;
			break;
		case SPELL_SHIELD_THORN:
			retval = ELEM_EARTH;
			break;
		case SPELL_SHIELD_MANA:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_SHIELD_MIRROR:
			retval = ELEM_LIGHT;
			break;
		case SPELL_SHIELD_HOLY:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_SHIELD_STATIC:
			retval = ELEM_ELEC;
			break;
		case SPELL_FORTIFY_MIND:
			retval = ELEM_MIND;
			break;
		case SPELL_FORTIFY_BODY:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_SWEET_DREAMS:
			retval = ELEM_MIND;
			break;
		case SPELL_DEVINE_MIND:
			retval = ELEM_MIND;
			break;
		case SPELL_NUMB_MIND:
			retval = ELEM_ICE;
			break;
		case SPELL_SLOW:
			retval = ELEM_ICE;
			break;
		case SPELL_FLIGHT:
			retval = ELEM_AIR;
			break;
		case SPELL_BATTLE_RAGE:
			retval = ELEM_FIRE;
			break;
		case SPELL_ENCHANT_ARMOR:
			retval = ELEM_FIRE;
			break;
		case SPELL_MAGIC_BUBBLE:
			retval = ELEM_DEATH;
			break;
		case SPELL_PSI_PANIC:
			retval = ELEM_MIND;
			break;
		case SPELL_NIGHTMARE:
			retval = ELEM_MIND;
			break;
		case SPELL_VITALIZE:
			retval = ELEM_AIR;
			break;
		case SPELL_DISPELL_SANCTURY:
			retval = ELEM_DEATH;
			break;
		case SPELL_FORSEE:
			retval = ELEM_LIGHT;
			break;
		case SPELL_MANA_BLAST:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_CONFUSE:
			retval = ELEM_MIND;
			break;
		case SPELL_CORRUPT_ARMOR:
			retval = ELEM_EARTH;
			break;
		case SPELL_WEAKEN:
			retval = ELEM_EARTH;
			break;
		case SPELL_SOULSMASH:
			retval = ELEM_DEATH;
			break;
		case SPELL_DEMONSHRIEK:
			retval = ELEM_DEATH;
			break;
		case SPELL_LIFESUCK:
			retval = ELEM_DEATH;
			break;
		case SPELL_BURNINGSKULL:
			retval = ELEM_FIRE;
			break;
		case SPELL_HEARTSQUEEZE:
			retval = ELEM_AIR;
			break;
		case SPELL_FACEMELT:
			retval = ELEM_FIRE;
			break;
		case SPELL_ELECTRIC_BLAST:
			retval = ELEM_ELEC;
			break;
		case SPELL_INFERNO:
			retval = ELEM_FIRE;
			break;
		case SPELL_WATER_TO_WINE:
			retval = ELEM_WATER;
			break;
		case SPELL_MIDAS_TOUCH:
			retval = ELEM_EARTH;
			break;
		case SPELL_POLYMORPH:
			retval = ELEM_SPIRIT;
			break;
		case SPELL_IDENTIFY:
			retval = ELEM_MIND;
			break;
		case SPELL_FIRE_BREATH:
			retval = ELEM_FIRE;
			break;
		case SPELL_GAS_BREATH:
			retval = ELEM_AIR;
			break;
		case SPELL_FROST_BREATH:
			retval = ELEM_ICE;
			break;
		case SPELL_ACID_BREATH:
			retval = ELEM_WATER;
			break;
		case SPELL_LIGHTNING_BREATH:
			retval = ELEM_ELEC;
			break;
		case SPELL_BURN:
			retval = ELEM_FIRE;
			break;
		case SPELL_FREEZE:
			retval = ELEM_ICE;
			break;
		case SPELL_ACID:
			retval = ELEM_WATER;
			break;
		default:
			retval = ELEM_NONE;
	}
	return retval;
}

int anti_elem ( int elem )
{
	switch ( elem )
	{
		case ELEM_NONE:
			return ELEM_NONE;
		case ELEM_FIRE:
			return ELEM_WATER;
		case ELEM_ICE:
			return ELEM_FIRE;
		case ELEM_EARTH:
			return ELEM_WATER;
		case ELEM_AIR:
			return ELEM_EARTH;
		case ELEM_ELEC:
			return ELEM_EARTH;
		case ELEM_WATER:
			return ELEM_AIR;
		case ELEM_LIGHT:
			return ELEM_DARK;
		case ELEM_DARK:
			return ELEM_LIGHT;
		case ELEM_SPIRIT:
			return ELEM_DEATH;
		case ELEM_MIND:
			return ELEM_MIND;
		case ELEM_DEATH:
			return ELEM_SPIRIT;
		default:
			return ELEM_NONE;
	}
}


int immune_to ( Character *ch, int elem )
{
	return 0;

	if ( IS_SET ( IMMUNE ( ch ), ( 1 << elem ) ) )
		return 1;
	else
		return 0;
}

float resist_elem ( Character *ch, int elem )
{
	float retval = 0;
	if ( IS_SET ( class_elem_weakness ( GET_CLASS ( ch ) ), ( 1 << elem ) ) )
		retval += 25.0;
	else if ( IS_SET ( class_elem_strength ( GET_CLASS ( ch ) ), ( 1 << elem ) ) )
		retval -= 25.0;

	return retval;
}

char * print_elemental ( int chcl, int weak, char * buf, size_t len )
{
	int i;
	size_t k=0;
	for ( i = 0; i < NUM_ELEM_TYPES;i++ )
	{
		if ( IS_SET ( ( weak ? class_elem_weakness ( chcl ) : class_elem_strength ( chcl ) ), ( 1 << i ) ) )
			k += snprintf ( buf + k, len - k,  "%s ", elemental_types[i] );
	}

	return buf;
}

#define PLR i->character
void update_spell_wait ( void )
{
	Descriptor *i;

	for ( i = descriptor_list; i; i = i->next )
	{
		if ( !IS_PLAYING ( i ) )
			continue;
		SAVED ( i->character ).SkillWaitTick();
	}

}
