//
// C++ Implementation: trainers
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "dg_scripts.h"
#include "constants.h"
#include "trainers.h"
#include "genmob.h"
#include "genzon.h"
#include "oasis.h"

/*
    ASSIGNMOB(500, guild);	// guild guard in Mud School
    ASSIGNMOB(564, guild);	// Gym master in school
    ASSIGNMOB(899, guild);	// Hunter Guildmaster
    ASSIGNMOB(1394, guild);	// sicilian gm
    ASSIGNMOB(2159, guild);	// Actor trainer in Crete
    ASSIGNMOB(2905, guild);	// Viking trainer
    ASSIGNMOB(2933, guild);	// new clan
    ASSIGNMOB(3007, guild);	// guild guard in Olde Yorke
    ASSIGNMOB(3020, guild);	// Mage guildmaster in Olde Yorke
    ASSIGNMOB(3022, guild);	// Thief gm in OY
    ASSIGNMOB(3023, guild);	// Warrior gm in OY
    ASSIGNMOB(3021, guild);	// Priest gm in OY
    ASSIGNMOB(3150, guild);	// Gypsy gm in OY
    ASSIGNMOB(6494, guild);	// Ranger gm
    ASSIGNMOB(9404, guild);	// Chaos gm
    ASSIGNMOB(10000, guild);	// Puff
    ASSIGNMOB(10401, guild);	// trainer in Palp's zone
    ASSIGNMOB(10498, guild);	// dark jedi trainer
    ASSIGNMOB(12403, guild);
    ASSIGNMOB(12904, guild);
    ASSIGNMOB(27801, guild);
     ASSIGNMOB(10204, guild);

    // guildmasters that teach special skills only
    ASSIGNMOB(7043, guild);	// scribe skill
    ASSIGNMOB(21819, guild);	// tinker skill
    ASSIGNMOB(26060, guild);	// brew skill
*/
int spell_num ( const char *name );
int spell_sorted_info[TOP_SPELL_DEFINE + 2];
int spell_sort_info[MAX_SKILLS + TOP_SUB_DEFINE + 1];
int spell_sort_data[MAX_SPELLS + 1];
int skill_sort_data[MAX_SKILLS - MAX_SPELLS + 2];
int sub_sort_data[TOP_SUB_DEFINE +1];

int compare_spells ( const void *x, const void *y )
{
	int a = * ( const int * ) x, b = * ( const int * ) y;

	return strcmp ( skill_name ( a ), skill_name ( b ) );
}
int compare_sub ( const void *x, const void *y )
{
	int a = * ( const int * ) x, b = * ( const int * ) y;

	return strcmp ( sub_name ( a ), sub_name ( b ) );
}

void sort_all_spell_data ( void )
{
	int a;

	/* initialize array, avoiding reserved. */
	for ( a = 0; a < TOP_SPELL_DEFINE; a++ )
		spell_sorted_info[a] = a;

	qsort ( spell_sorted_info, TOP_SPELL_DEFINE, sizeof ( int ), compare_spells );
}

void sort_sub_data ( void )
{
	int a;

	/* initialize array, avoiding reserved. */
	for ( a = 1; a < TOP_SUB_DEFINE; a++ )
		sub_sort_data[a] = a;

	qsort ( &sub_sort_data[0], TOP_SUB_DEFINE, sizeof ( int ), compare_sub );
}
void sort_spell_data ( void )
{
	int a;

	/* initialize array, avoiding reserved. */
	for ( a = 1; a <= MAX_SPELLS; a++ )
		spell_sort_data[a] = a;

	qsort ( &spell_sort_data[0], MAX_SPELLS, sizeof ( int ), compare_spells );
}
void sort_skill_data ( void )
{
	int a, b = 0;

	/* initialize array, avoiding reserved. */
	for ( a = MAX_SPELLS + 1; a <= MAX_SKILLS; a++ )
		skill_sort_data[++b] = a;

	qsort ( &skill_sort_data[0], b, sizeof ( int ), compare_spells );
}


void sort_spells ( void )
{
	int a, b = 1;

	sort_skill_data();
	sort_spell_data();
	sort_sub_data();
	sort_all_spell_data();

	/* initialize array, avoiding reserved. */
	for ( a = 1; a <= MAX_SPELLS; a++ )
		spell_sort_info[a] = spell_sort_data[a];

	for ( ; a <= MAX_SKILLS; a++ )
		spell_sort_info[a] = skill_sort_data[b++];

	for ( b = 0; a <= ( TOP_SUB_DEFINE + MAX_SKILLS ); a++ )
		spell_sort_info[a] = sub_sort_data[b++];


}

const char *how_good ( int percent )
{
	if ( percent < 0 )
		return " (error)";
	if ( percent == 0 )
		return "\x1B[0m----------\x1B[0m";
	if ( percent <= 10 )
		return "\x1B[0m#---------\x1B[0m";
	if ( percent <= 20 )
		return "\x1B[32m##\x1B[0m--------\x1B[0m";
	if ( percent <= 30 )
		return "\x1B[32m###\x1B[0m-------\x1B[0m";
	if ( percent <= 40 )
		return "\x1B[33m####\x1B[0m------\x1B[0m";
	if ( percent <= 50 )
		return "\x1B[33m#####\x1B[0m-----\x1B[0m";
	if ( percent <= 60 )
		return "\x1B[34m######\x1B[0m----\x1B[0m";
	if ( percent <= 70 )
		return "\x1B[34m#######\x1B[0m---\x1B[0m";
	if ( percent <= 80 )
		return "\x1B[35m########\x1B[0m--\x1B[0m";
	if ( percent <= 90 )
		return "\x1B[35m#########\x1B[0m-\x1B[0m";

	return "\x1B[36m##########\x1B[0m";
}

char * how_good_perc ( Character *ch, int perc )
{
	static char  perc_buf[80];
	if ( GET_INT ( ch ) < 22 )
		return ( char * ) how_good ( perc );
	sprintf ( perc_buf, "%-3d%%", perc );
	return perc_buf;

}

bool can_teach_skill ( Character *mob, int i )
{
	if ( IS_NPC ( mob ) && !mob->mob_specials.teaches_skills.empty() )
	{
		for ( int h = 0; h < mob->mob_specials.teaches_skills.size();h++ )
		{
			if ( mob->mob_specials.teaches_skills.at ( h ) == i )
				return TRUE;
		}
	}
	return FALSE;
}

void list_skills ( Character *ch, int skillspell, Character *mob )
{
	int i, sortpos, h = 0, imm, ending, sub;
	int count= 0;
	char buf[MAX_INPUT_LENGTH];
	DYN_DEFINE;
	*buf = 0;

	if ( skillspell == 0 )
	{
		sortpos = MAX_SPELLS + 1;
		ending = MAX_SKILLS;
	}
	else if ( skillspell == 1 )
	{
		sortpos = 1;
		ending = MAX_SPELLS;
	}
	else
	{
		sortpos = MAX_SKILLS + 1;
		ending = MAX_SKILLS + TOP_SUB_DEFINE;
	}
	DYN_CREATE;
	*dynbuf = 0;
	imm = -1;

	if ( mob == NULL )
	{
		if ( GET_PRACTICES ( ch ) <= 0 )
			strcpy ( buf, "You have no practice sessions remaining.\r\n" );
		else
			sprintf ( buf, "You have %d practice session%s remaining.\r\n",
			          GET_PRACTICES ( ch ), ( GET_PRACTICES ( ch ) == 1 ? "" : "s" ) );
		if ( skillspell < 2 )
			sprintf ( buf + strlen ( buf ), "You have the following %s:\r\n",skillspell == 1 ? "spells" : "skills" );
		else
			sprintf ( buf + strlen ( buf ), "You have the following extra abilities:\r\n" );
		DYN_RESIZE ( buf );
	}
	else
	{
		snprintf ( buf, sizeof ( buf ), "%s can teach you the following %s:\r\n", GET_NAME ( mob ), skillspell == 1 ? "spells" : "skills" );
		DYN_RESIZE ( buf );
	}

	for ( ; sortpos <= ending; sortpos++ )
	{
		i = spell_sort_info[sortpos];
		if ( skillspell < 2 && mob && !mob->mob_specials.teaches_skills.empty() )
		{
			if ( !can_teach_skill ( mob, i ) )
				continue;
		}

		if ( skillspell > 1 )
		{
			if ( i==0 )
				continue;
			if ( i==TOP_SUB_DEFINE )
				continue;
			sub = GET_SUB ( ch, i );

			/*if (sub < 1)
			continue;*/

			if ( GET_LEVEL ( ch ) > LVL_IMMORT )
			{
				improveallsubs ( ch );
				sub = 100;
			}
			if ( sub == 0 )
				continue;
			count++;
			if ( GET_LEVEL ( ch ) > LVL_IMMORT )
			{
				sprintf ( buf, "%-3d)", i );
				DYN_RESIZE ( buf );
			}
			sprintf ( buf, " \x1B[1;31m%-28s  \x1B[0m[%s] ", sub_name ( i ), ( ( sub > 0 ) ? how_good_perc ( ch, sub ) : unlearnedsub ) );
			DYN_RESIZE ( buf );




			if ( !IS_SET ( sub_info[i].flags, SUB_TYPE_PROF ) && sub_info[i].perent!=TYPE_UNDEFINED )
			{
				sprintf ( buf, " Parent Skill: %s", skill_name ( sub_info[i].perent ) );
				DYN_RESIZE ( buf );
			}
			if ( IS_SET ( sub_info[i].flags, SUB_TYPE_PROF ) && sub_info[i].perent!=TYPE_UNDEFINED )
			{
				sprintf ( buf, " Profession: %s", profession_names[sub_info[i].perent] );
				DYN_RESIZE ( buf );
			}
			sprintf ( buf, "\r\n" );
			DYN_RESIZE ( buf );
		}
		else
		{


			if ( knows_spell ( ch, i ) )
			{

				if ( ( FIRST_PRE ( i ) != TYPE_UNDEFINED )
				        && ( SECOND_PRE ( i ) != TYPE_UNDEFINED ) )
					h = 2;
				else if ( ( ( FIRST_PRE ( i ) != TYPE_UNDEFINED )
				            && ( SECOND_PRE ( i ) == TYPE_UNDEFINED ) ) )
					h = 1;
				else
					h = 0;
				if ( GET_LEVEL ( ch ) == LVL_IMPL )
				{
					sprintf ( buf, "%-3d)", i );
					DYN_RESIZE ( buf );
				}


				sprintf ( buf, " %s%-20s  \x1B[0m[%s] ",
				          ( ! ( i < MAX_SPELLS ) ? "\x1B[32m" : "\x1B[33m" ),
				          skill_name ( i ), how_good_perc ( ch, total_chance ( ch, i ) ) );
				DYN_RESIZE ( buf );

				sprintf ( buf, "[L:%2d T:%d]",spell_info[i].min_level, spell_info[i].tier );
				DYN_RESIZE ( buf );



				if ( skillspell == 1 )
				{
					sprintf ( buf, "[Mana:%-4d]\x1B[0m",
					          mag_manacost ( ch, i ) );
					DYN_RESIZE ( buf );
				}
				if ( skillspell == 1 && elemental_type ( i ) != ELEM_NONE )
				{
					snprintf ( buf, sizeof ( buf ), "[{cp%-11s{c0]", elemental_types[elemental_type ( i ) ] );
					DYN_RESIZE ( buf );
				}

				if ( spell_info[i].wait )
				{
					sprintf ( buf, "[Delay of %3d sec]",spell_info[i].wait );
					DYN_RESIZE ( buf );
				}

				if ( GET_SPELL_WAIT ( ch, i ) )
				{
					sprintf ( buf, "[%d seconds till recast]",
					          GET_SPELL_WAIT ( ch, i ) );
					DYN_RESIZE ( buf );
				}

				if ( h == 1 )
				{
					sprintf ( buf, "\x1B[1;34m  Requires: %s \x1B[0;0m",
					          skill_name ( FIRST_PRE ( i ) ) );
					DYN_RESIZE ( buf );
				}
				else if ( h == 2 )
				{
					sprintf ( buf,
					          "\x1B[1;34m  Requires: %s and %s \x1B[0;0m",
					          skill_name ( FIRST_PRE ( i ) ),
					          skill_name ( SECOND_PRE ( i ) ) );
					DYN_RESIZE ( buf );
				}

				sprintf ( buf, "\r\n" );
				DYN_RESIZE ( buf );
				count++;

				/*if (!(GET_LEVEL(ch) >= LVL_IMMORT))
				break;*/
			}


		}
	}
	if ( count == 0 )
	{
		sprintf ( buf, "You don't know any!\r\n" );
		DYN_RESIZE ( buf );
	}

	page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

ACMD ( do_practice )
{
	int skill_num, percent, learned, pp;
	gold_int cig  = 0;
	vector<Character *> mob_trainers;
	vector<Character *>::iterator mti;
	Character *mob;
	int train_list = -1;

	if ( IS_NPC ( ch ) || !IN_ROOM ( ch ) )
		return;

	skip_spaces ( &argument );

	if ( !*argument )
	{
		ch->Send ( "You have {cW%d{c0 practice sessions remaining.\r\n\r\n", GET_PRACTICES ( ch ) );
		ch->Send ( "To View Skills Or Spells Either Type:\r\n" );
		ch->Send ( "{cCpractice skills{c0 or type {cCpractice spells{c0\r\n\r\n" );
		ch->Send ( "To view any other abilities you may have (subskills, clan skills, etc) type:\r\n" );
		ch->Send ( "{cCpractice subs{c0\r\n\r\n" );
		ch->Send ( "Otherwise to get better in that skill or spell try:\r\n" );
		ch->Send ( "{ccpractice <skill/spell name>{c0\r\n" );
		ch->Send ( "{ccFor example: {cCpractice magic missile{c0\r\n" );
		ch->Send ( "{cG[NOTE: You can practice with your skills and spells at a guildmaster]{c0\r\n" );

		return;
	}

	for ( Character *p = IN_ROOM ( ch )->people; p != NULL;p = p->next_in_room )
		if ( IS_NPC ( p ) && !p->mob_specials.teaches_skills.empty() )
			mob_trainers.push_back ( p );

	if ( is_abbrev ( "skills", argument ) )
		train_list = 0;
	else if ( is_abbrev ( "spells", argument ) )
		train_list = 1;
	else if ( is_abbrev ( "subskills", argument ) )
		train_list = 2;

	if ( train_list != -1 )
	{
		if ( mob_trainers.empty() )
		{
			list_skills ( ch, train_list, NULL );
			return;
		}
		else
		{
			for ( mti = mob_trainers.begin();mti != mob_trainers.end();mti++ )
				list_skills ( ch, train_list, *mti );
		}
		return;
	}

	learned = IRANGE ( 30, ( 20* ( TIERNUM ) ), 80 );
	skill_num = find_skill_num ( argument );

	if ( mob_trainers.empty() )
	{
		ch->Send ( "You can't train that here, there are no trainers available." );
		return;
	}
	else if ( mob_trainers.size() == 1 )
	{
		mob = mob_trainers[0];
		if ( !can_teach_skill ( mob, skill_num ) )
		{
			act ( "$N says 'I'm not skilled in that.  You must find someone else to teach you it.'", FALSE, ch, 0, mob, TO_CHAR );
			return;
		}
	}
	else
	{
		bool can = false;
		for ( mti = mob_trainers.begin();can == false && mti != mob_trainers.end();mti++ )
			if ( can_teach_skill ( *mti, skill_num ) )
				can = true;
		if ( can == false )
		{
			act ( "Nobody here is skilled in that.  You must find someone else to teach you it.", FALSE, ch, 0, NULL, TO_CHAR );
			return;
		}
	}
	if ( skill_num < 1 ||
	        !knows_spell ( ch, skill_num ) )
	{
		ch->Send ( "You do not know of that %s.\r\n", SPLSKL ( ch ) );
		return;
	}
	else if ( GET_SKILL ( ch, skill_num ) >= learned )
	{
		*ch << "You can't train that skill any further for now, come back when you remort.\r\n";
		return;
	}
	else if ( GET_PRACTICES ( ch ) <= 0 )
	{
		*ch << "You do not seem to be able to practice now.\r\n";
		return ;
	}

	//pp = MIN(MAXGAIN(ch), MAX(MINGAIN(ch), int_app[GET_INT(ch)].learn));
	pp = MAX ( 2, ( ( GET_INT ( ch ) + GET_WIS ( ch ) - 20 ) *40+5 ) /240 + 2 );

	percent = GET_SKILL ( ch, skill_num );
	/* Cost In Gold */
	cig = ( ( percent*4000 ) + ( REMORTS ( ch ) * 1000 ) + ( GET_LEVEL ( ch ) * 1000 * current_class_is_tier_num ( ch ) ) + ( spell_info[skill_num].min_level * 1000 ) ) * pp;
	if ( REMORTS ( ch ) > 2 && ch->Gold ( 0, GOLD_HAND ) < cig )
	{
		ch->Send ( "You need at least %lld gold coins to pay for practicing that.\r\n", cig );
		return;
	}
	percent += pp;


	SET_SKILL ( ch, skill_num, MIN ( learned, percent ) );
	if ( REMORTS ( ch ) > 2 )
	{
		ch->Gold ( -cig, GOLD_HAND );
		ch->Send ( "You pay %lld gold and a practice point to train your skill to %d%%.\r\n", cig, GET_SKILL ( ch,skill_num ) );
	}
	else
	{
		ch->Send ( "You pay a practice point and train your skill up to %d%%.\r\n", GET_SKILL ( ch,skill_num ) );
	}
	GET_PRACTICES ( ch )--;

	if ( GET_SKILL ( ch, skill_num ) >= learned )
		*ch << "You cannot train that any further for now. \r\nAlthough it may improve through use.\r\n";

	return;
}

void parse_train_group ( Character *ch, Character *vict,char * val_arg )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	int t = 0, c = 0, res = 0;
	two_arguments ( val_arg, arg1, arg2 );
	if ( !*arg1 || !*arg2 )
	{
		ch->Send ( "You must provide two values, the TIER NUMBER (or 0 for all), and the CLASS name.\r\neg: set <trainer name> 1 priest\r\nThis would give the trainer you specified all of the tier 1 priest skills and spells to train.\r\n" );
		return;
	}
	t = atoi ( arg1 );
	c = parse_class ( *arg2 );
	res = assign_group_trains ( vict, t, c );
	ch->Send ( "You assign %d skills to %s.\r\n", res, GET_NAME ( vict ) );
}

/*
This function lets you assign groups of skills and spells to mobs to train.
the group value defined the TIER, and the filter defines the CLASS of the skills
This only ADDS skills, not sets them.
*/
int assign_group_trains ( Character *mob, int group, int filter )
{
	int assigned = 0;
	int ss = TYPE_UNDEFINED;
	Character *proto;
	mob_vnum mvn;
	if ( mob == NULL )
		return 0;
	mvn = GET_MOB_VNUM ( mob );
	proto = GetMobProto ( mvn );
	if ( proto == NULL )
		return 0;

	for ( int a = 1; a <= MAX_SKILLS; a++ )
	{
		ss = spell_sort_info[a];
		if ( IS_SET ( spell_info[ss].classes, ( 1 << filter ) ) )
		{
			switch ( group )
			{
				default:
					break;
				case ALL_TIER_ONE:
					if ( spell_info[ss].tier > 1 )
						continue;
					break;
				case ALL_TIER_TWO:
					if ( spell_info[ss].tier != 2 )
						continue;
					break;
				case ALL_TIER_THREE:
					if ( spell_info[ss].tier != 3 )
						continue;
					break;
				case ALL_TIER_FOUR:
					if ( spell_info[ss].tier != 4 )
						continue;
					break;
			}
			if ( ss != TYPE_UNDEFINED && !can_teach_skill(proto, ss))
			{
				proto->mob_specials.teaches_skills.push_back ( ss );
				assigned++;
			}
		}
	}
	if ( assigned > 0 )
	{
		medit_save_to_disk ( zone_table[real_zone_by_thing ( mvn ) ].number );
		for ( mob = character_list; mob; mob = mob->next )
		{
			if ( mvn != GET_MOB_VNUM(mob) )
				continue;
			mob->mob_specials.teaches_skills = proto->mob_specials.teaches_skills;
		}
	}
	return assigned;
}
