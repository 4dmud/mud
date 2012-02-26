/* ************************************************************************
*   File: handler.c                                     Part of CircleMUD *
*  Usage: internal funcs: moving and finding chars/objs                   *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "handler.h"
#include "interpreter.h"
#include "spells.h"
#include "dg_scripts.h"
#include "dg_event.h"
#include "trees.h"
#include "fight.h"
#include "damage.h"
#include "descriptor.h"
/* local vars */
int extractions_pending = 0;
int obj_extractions_pending = 0;
bool LS_REMOVE = 0;

/* external vars */
extern const char *MENU;
struct hunter_data *hunter_list = NULL;

/* local functions */
void affect_modify_ar ( Character *ch, byte loc, sbyte mod,
                        int bitv[], bool add
                      );
int apply_ac ( Character *ch, int eq_pos );
//void update_object(struct obj_data *obj, int use);
void update_object ( Character *ch, struct obj_data *obj, int use, time_t timenow );
void update_char_objects ( Character *ch );


/* external functions */
void unhitch_item ( struct obj_data *obj );
void unhitch_mob ( Character *ch );
void check_timer ( obj_data *obj );
OBJ_DATA *item_from_locker ( Character *ch, OBJ_DATA *obj );
void purge_qic ( obj_rnum rnum );
void die_link ( Character *mob );
void extract_all_in_list ( OBJ_DATA *obj );
void stop_fusion ( Character *ch );
void remove_corpse_from_list ( OBJ_DATA *corpse );
int stop_task ( Character *ch ) ;
extern Character *ch_selling;
void stop_auction ( int type, Character *ch );
int invalid_class ( Character *ch, struct obj_data *obj );
int invalid_race ( Character *ch, struct obj_data *obj );
void remove_follower ( Character *ch );
void clearMemory ( Character *ch );
void die_follower ( Character *ch );
void set_race ( Character *ch, int race );
ACMD ( do_return );
void save_corpses ( void );
void reset_zone ( int zone );
void zap_char ( Character *victim );
void clean_events2 ( void *pointer );
int speed_update ( Character *ch );
void halt_fighting ( Character *ch );
int OBJ_INNATE;
void extract_linked_mob ( Character *mob );
void dismount_char ( Character *ch );
int move_link_room ( Character *mob, room_rnum room );
void eq_to_room ( Character *ch );
//void extract_obj_final(struct obj_data *obj);

void unhitch_mob ( Character *ch );
void delete_vehicle(struct obj_data *obj);
void save_artifacts(Room* room);

char *fname ( const char *namelist )
{
	static char holder[READ_SIZE];
	register char *point;

	for ( point = holder; isalpha ( *namelist ); namelist++, point++ )
		*point = *namelist;

	*point = '\0';

	return ( holder );
}

/* allow abbreviations */


int isname ( const char *str, const char *namelist )
{
	char *newlist;
	register char *curtok;
	//register char *lp;
	static char newlistbuf[MAX_INPUT_LENGTH];

	if ( !str || !*str || !namelist || !*namelist )
		return 0;
	if ( !strcasecmp ( str, namelist ) ) /* the easy way */
		return 1;

	//lp = newlist = strdup(namelist); /* make a copy since strtok 'modifies' strings */
	strlcpy ( newlistbuf, namelist, sizeof ( newlistbuf ) );
	newlist = newlistbuf;
	for ( curtok = strsep ( &newlist, WHITESPACE ); curtok; curtok = strsep ( &newlist, WHITESPACE ) )
		if ( curtok && is_abbrev ( str, curtok ) )
		{
			//free(lp);
			return 1;
		}
	//free(lp);
	return 0;
}
int isname ( const char *str, string &namelist )
{
	char *newlist;
	register char *curtok;
	static char newlistbuf[MAX_INPUT_LENGTH];

	if ( !str || !*str )
		return 0;
	if ( !namelist.compare ( str ) ) /* the easy way */
		return 1;

	strlcpy ( newlistbuf, namelist.c_str(), sizeof ( newlistbuf ) );
	newlist = newlistbuf;
	for ( curtok = strsep ( &newlist, WHITESPACE ); curtok; curtok = strsep ( &newlist, WHITESPACE ) )
		if ( curtok && is_abbrev ( str, curtok ) )
			return 1;

	return 0;
}
/* Hard, because it doesn't check for abbreviations */
int isname_hard ( const char *str, const char *namelist )
{
	char *newlist;
	register char *curtok;
	//register char *lp;
	static char newlistbuf[MAX_INPUT_LENGTH];

	if ( !str || !*str || !namelist || !*namelist )
		return 0;
	if ( !strcasecmp ( str, namelist ) ) /* the easy way */
		return 1;

	//lp = newlist = strdup(namelist); /* make a copy since strtok 'modifies' strings */
	strlcpy ( newlistbuf, namelist, sizeof ( newlistbuf ) );
	newlist = newlistbuf;
	for ( curtok = strsep ( &newlist, WHITESPACE ); curtok; curtok = strsep ( &newlist, WHITESPACE ) )
		if ( curtok && !strcasecmp ( str, curtok ) )
		{
			//free(lp);
			return 1;
		}
	//free(lp);
	return 0;
}

/* search through strlist for matches on namelist
Return 1 for complete match of abbrev words
return 0 if not.
note: consider making it check for quote marks and exact phrases.
*/
int isname_full ( const char *strlist, const char *namelist )
{
	char *newlist, *lp;
	char *curtok;
	int cnt;

	if ( !strlist || !*strlist || !namelist || !*namelist )
		return 0;
	if ( !str_cmp ( strlist, namelist ) ) /* the easy way */
		return 1;

	/* make a copy since strtok 'modifies' strings */

	lp = newlist = strdup ( strlist );
	cnt = 0;
	for ( curtok = strsep ( &newlist, WHITESPACE ); curtok; curtok = strsep ( &newlist, WHITESPACE ) )
		if ( curtok )
		{
			if ( !isname ( curtok, namelist ) )
			{
				free ( lp );
				return 0;
			}
			else
			{
				cnt ++;
			}
		}

	free ( lp );

	return cnt;
}

char * str_until ( char *strlist, const char *key, char *newstr, size_t len )
{
	char *p = strlist;
	char arg[MAX_INPUT_LENGTH];
	int loop = 0;
	if ( !strlist || !*strlist || len <= 0 )
		return 0;
	*newstr = '\0';
	for ( p = any_one_arg ( p, arg );p && *arg;p = any_one_arg ( p, arg ) )
	{
		if ( !str_cmp ( key, arg ) )
			break;
		loop++;
		if ( loop != 1 )
			strlcat ( newstr, " ", len );
		strlcat ( newstr, arg, len );
	}


	return p;
}

int begins_with_number ( char * str )
{
	char num[MAX_INPUT_LENGTH];
	any_one_arg ( str, num );
	if ( is_number ( num ) )
		return 1;
	else
		return 0;
}

/*
int isname(const char *str, const char *namelist)
{
  const char *curname, *curstr;

  curname = namelist;
  for (;;) {
    for (curstr = str;; curstr++, curname++) {
      if (!*curstr && !isalpha(*curname))
     return (1);

      if (!*curname)
     return (0);

      if (!*curstr || *curname == ' ')
     break;

      if (LOWER(*curstr) != LOWER(*curname))
     break;
    }


    for (; isalpha(*curname); curname++);
    if (!*curname)
      return (0);
    curname++;
  }
}
*/

/* Stock isname().  Leave this here even if you put in a newer  *
 * isname().  Used for OasisOLC.                                */
int is_name ( const char *str, const char *namelist )
{
	const char *curname, *curstr;

	if ( !*str || !*namelist || !str || !namelist )
		return ( 0 );

	curname = namelist;
	for ( ;; )
	{
		for ( curstr = str;; curstr++, curname++ )
		{
			if ( !*curstr && !isalpha ( *curname ) )
				return ( 1 );

			if ( !*curname )
				return ( 0 );

			if ( !*curstr || *curname == ' ' )
				break;

			if ( LOWER ( *curstr ) != LOWER ( *curname ) )
				break;
		}

		/* skip to next name */

		for ( ; isalpha ( *curname ); curname++ )
			;
		if ( !*curname )
			return ( 0 );
		curname++;      /* first char of new name */
	}
}



void affect_modify ( Character *ch, byte loc, int mod,
                     bitvector_t bitv, bool add
                   )
{

	if ( loc < 0 )
		return ;
	if ( add
	        == TRUE )
		SET_BIT_AR ( AFF_FLAGS ( ch ), bitv );
	else
	{
		REMOVE_BIT_AR ( AFF_FLAGS ( ch ), bitv );
		mod = -mod;
	}

	switch ( loc )
	{
		case APPLY_NONE:
			break;

		case APPLY_STR:
			GET_STR ( ch ) += mod;
			break;
		case APPLY_DEX:
			GET_DEX ( ch ) += mod;
			break;
		case APPLY_INT:
			GET_INT ( ch ) += mod;
			break;
		case APPLY_WIS:
			GET_WIS ( ch ) += mod;
			break;
		case APPLY_CON:
			GET_CON ( ch ) += mod;
			break;
		case APPLY_CHA:
			GET_CHA ( ch ) += mod;
			break;
		case APPLY_MINE_SPEED:
			if ( !IS_NPC ( ch ) )
				MINE_SPEED ( ch ) += mod;
			break;
		case APPLY_MINE_BONUS:
			if ( !IS_NPC ( ch ) )
				MINE_BONUS ( ch ) += mod;
			break;
		case APPLY_MINE_DAMAGE:
			if ( !IS_NPC ( ch ) )
				MINE_DAMAGE ( ch ) += mod;
			break;
		case APPLY_MINE_STEALTH:
			if ( !IS_NPC ( ch ) )
				MINE_STEALTH ( ch ) += mod;
			break;

		case APPLY_REGEN_MOVE://class
			GET_REGEN_MOVE ( ch ) += mod;
			break;

		case APPLY_REGEN_HIT://lev
			GET_REGEN_HIT ( ch ) += mod;
			break;

		case APPLY_AGE:
			ch->player.time.birth -= ( mod * SECS_PER_MUD_YEAR );
			break;

		case APPLY_CHAR_WEIGHT:
			GET_WEIGHT ( ch ) += mod;
			break;

		case APPLY_CHAR_HEIGHT:
			GET_HEIGHT ( ch ) += mod;
			break;

		case APPLY_MANA:
			GET_MAX_MANA ( ch ) += mod;
			break;

		case APPLY_HIT:
			GET_MAX_HIT ( ch ) += mod;
			break;

		case APPLY_MOVE:
			GET_MAX_MOVE ( ch ) += mod;
			break;

		case APPLY_REGEN_MANA://gold
			GET_REGEN_MANA ( ch ) += mod;
			break;

		case APPLY_EXP:
			break;

		case APPLY_AC:
			GET_AC ( ch ) += mod;
			break;

		case APPLY_HITROLL:
			GET_HITROLL ( ch ) += mod;
			break;

		case APPLY_DAMROLL:
			GET_DAMROLL ( ch ) += mod;
			break;

		case APPLY_SAVING_PARA:
			GET_SAVE ( ch, SAVING_PARA ) += mod;
			break;

		case APPLY_SAVING_ROD:
			GET_SAVE ( ch, SAVING_ROD ) += mod;
			break;

		case APPLY_SAVING_PETRI:
			GET_SAVE ( ch, SAVING_PETRI ) += mod;
			break;

		case APPLY_SAVING_BREATH:
			GET_SAVE ( ch, SAVING_BREATH ) += mod;
			break;

		case APPLY_SAVING_SPELL:
			GET_SAVE ( ch, SAVING_SPELL ) += mod;
			break;

		case APPLY_SPEED:
			AFF_SPEED ( ch ) += mod;
			break;

		default:
			log ( "SYSERR: Unknown apply adjust %d attempt (%s, affect_modify).",
			      loc, __FILE__ );
			break;

	}                 /* switch */
}


void aff_apply_modify ( Character *ch, byte loc, int mod, char *msg )
{


	switch ( loc )
	{
		case APPLY_NONE:
			break;

		case APPLY_STR:
			GET_STR ( ch ) += mod;
			break;
		case APPLY_DEX:
			GET_DEX ( ch ) += mod;
			break;
		case APPLY_INT:
			GET_INT ( ch ) += mod;
			break;
		case APPLY_WIS:
			GET_WIS ( ch ) += mod;
			break;
		case APPLY_CON:
			GET_CON ( ch ) += mod;
			break;
		case APPLY_CHA:
			GET_CHA ( ch ) += mod;
			break;

		case APPLY_REGEN_MOVE:
			GET_REGEN_MOVE ( ch ) += mod;
			break;

			/*
			 * My personal thoughts on these two would be to set the person to the
			 * value of the apply.  That way you won't have to worry about people
			 * making +1 level things to be imp (you restrict anything that gives
			 * immortal level of course).  It also makes more sense to set someone
			 * to a class rather than adding to the class number. -gg
			 */

		case APPLY_REGEN_HIT:
			GET_REGEN_HIT ( ch ) += mod;
			break;

		case APPLY_AGE:
			ch->player.time.birth -= ( mod * SECS_PER_MUD_YEAR );
			break;

		case APPLY_CHAR_WEIGHT:
			GET_WEIGHT ( ch ) += mod;
			break;

		case APPLY_CHAR_HEIGHT:
			GET_HEIGHT ( ch ) += mod;
			break;

		case APPLY_MANA:
			GET_MAX_MANA ( ch ) += mod;
			break;

		case APPLY_HIT:
			GET_MAX_HIT ( ch ) += mod;
			break;

		case APPLY_MOVE:
			GET_MAX_MOVE ( ch ) += mod;
			break;

		case APPLY_REGEN_MANA:
			GET_REGEN_MANA ( ch ) += mod;
			break;

		case APPLY_EXP:
			break;

		case APPLY_AC:
			GET_AC ( ch ) += mod;
			break;

		case APPLY_HITROLL:
			GET_HITROLL ( ch ) += mod;
			break;

		case APPLY_DAMROLL:
			GET_DAMROLL ( ch ) += mod;
			break;

		case APPLY_SAVING_PARA:
			GET_SAVE ( ch, SAVING_PARA ) += mod;
			break;

		case APPLY_SAVING_ROD:
			GET_SAVE ( ch, SAVING_ROD ) += mod;
			break;

		case APPLY_SAVING_PETRI:
			GET_SAVE ( ch, SAVING_PETRI ) += mod;
			break;

		case APPLY_SAVING_BREATH:
			GET_SAVE ( ch, SAVING_BREATH ) += mod;
			break;

		case APPLY_SAVING_SPELL:
			GET_SAVE ( ch, SAVING_SPELL ) += mod;
			break;

		case APPLY_RACE:
			// GET_RACE(ch) += mod; */
			break;

		case APPLY_SPEED:
			AFF_SPEED ( ch ) += mod;
			break;

		case APPLY_COOLNESS:
			GET_COOLNESS ( ch ) += mod;
			break;
		case APPLY_MINE_SPEED:
			if ( !IS_NPC ( ch ) )
				MINE_SPEED ( ch ) += mod;
			break;
		case APPLY_MINE_BONUS:
			if ( !IS_NPC ( ch ) )
				MINE_BONUS ( ch ) += mod;
			break;
		case APPLY_MINE_DAMAGE:
			if ( !IS_NPC ( ch ) )
				MINE_DAMAGE ( ch ) += mod;
			break;
		case APPLY_MINE_STEALTH:
			if ( !IS_NPC ( ch ) )
				MINE_STEALTH ( ch ) += mod;
			break;

		default:
			log ( "SYSERR: Unknown apply adjust %d attempt (%s, affect_modify).",
			      loc, __FILE__ );
			break;

	}                 /* switch */
}

/*
void affect_modify(Character * ch, byte loc, sbyte mod,
             bitvector_t bitv, bool add)
{
  if (add)
    SET_BIT_AR(AFF_FLAGS(ch), bitv);
  else {
    REMOVE_BIT_AR(AFF_FLAGS(ch), bitv);
    mod = -mod;
  }

  aff_apply_modify(ch, loc, mod, "affect_modify");
}
*/



void affect_modify_ar ( Character *ch, byte loc, int mod,
                        int bitv[], bool add
                      )
{
	int i, j;

	if ( add
	   )
	{
		for ( i = 0; i < AF_ARRAY_MAX; i++ )
			for ( j = 0; j < 32; j++ )
				if ( IS_SET_AR ( bitv, ( i * 32 ) + j ) )
					SET_BIT_AR ( AFF_FLAGS ( ch ), ( i * 32 ) + j );
	}
	else
	{
		for ( i = 0; i < AF_ARRAY_MAX; i++ )
			for ( j = 0; j < 32; j++ )
				if ( IS_SET_AR ( bitv, ( i * 32 ) + j ) )
					REMOVE_BIT_AR ( AFF_FLAGS ( ch ), ( i * 32 ) + j );
		mod = -mod;
	}

	aff_apply_modify ( ch, loc, mod, ( char * ) "affect_modify_ar" );
}



/* Insert an affect_type in a Character structure
   Automatically sets apropriate bits and apply's */
void affect_to_char ( Character *ch, struct affected_type *af )
{
	struct affected_type *affected_alloc;

	if ( GET_CLASS ( ch ) > NUM_CLASSES || GET_RACE ( ch ) > NUM_RACES || ( af->expire != -2 && ( time_to_sec ( af->expire ) < 0 ) ) )
		return;

	CREATE ( affected_alloc, struct affected_type, 1 );

	*affected_alloc = *af;
	affected_alloc->next = ch->affected;
	ch->affected = affected_alloc;

	affect_modify ( ch, af->location, af->modifier, af->bitvector, TRUE );
	ch->affect_total();
	if ( !IS_NPC ( ch ) )
		SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );
}

/* Call affect_remove with every spell of spelltype "skill" */
void affect_from_char ( Character *ch, int type )
{
	struct affected_type *hjp = NULL, *next = NULL;

	for ( hjp = ch->affected; hjp; hjp = next )
	{
		next = hjp->next;
		if ( hjp->type == type )
		{
			if ( type == SPELL_IMMFREEZE )
			{
				REMOVE_BIT_AR ( PLR_FLAGS ( ch ), PLR_FROZEN );
			}
			if ( type == SPELL_SILENCED )
			{
				REMOVE_BIT_AR ( PLR_FLAGS ( ch ), PLR_COVENTRY );
			}
			ch->affect_remove ( hjp );
		}
	}
}



/*
 * Return TRUE if a char is affected by a spell (SPELL_XXX),
 * FALSE indicates not affected.
 */
bool affected_by_spell ( Character *ch, int type )
{
	struct affected_type *hjp;

	for ( hjp = ch->affected; hjp; hjp = hjp->next )
		if ( hjp->type == type )
			return ( TRUE );

	return ( FALSE );
}



void affect_join ( Character *ch, struct affected_type *af,
                   bool add_dur, bool avg_dur, bool add_mod, bool avg_mod )
{
	struct affected_type *hjp, *a_next;
	bool found = FALSE;

	for ( hjp = ch->affected; !found && hjp; hjp = a_next )
	{
		a_next = hjp->next;

		if ( ( hjp->type == af->type ) && ( hjp->location == af->location ) )
		{
			if ( add_dur && af->expire != -2 )
				af->expire += time_to_sec ( hjp->expire );
			if ( avg_dur && af->expire != -2 )
				af->expire -= time_to_sec ( af->expire ) /2;

			if ( add_mod )
				af->modifier += hjp->modifier;
			if ( avg_mod )
				af->modifier /= 2;

			ch->affect_remove ( hjp );
			affect_to_char ( ch, af );
			found = TRUE;
		}
	}
	if ( !found )
		affect_to_char ( ch, af );
}

int move_char_to ( Character *ch, room_rnum room )
{
	room_rnum cur;
	if ( !ch )
		return 0;

	if ( room == NULL )
		return 0;

	cur = IN_ROOM ( ch );

	if ( cur != NULL )
		char_from_room ( ch );
	if ( SITTING ( ch ) )
		char_from_chair ( ch );

	unhitch_mob ( ch );


	if ( cur == IN_ROOM ( ch ) )
		return 0;


	char_to_room ( ch, room );

	if ( IN_ROOM ( ch ) == NULL )
	{
		char_to_room ( ch, cur );
		IN_ROOM ( ch ) = cur;
		return 0;
	}
	else if ( IN_ROOM ( ch ) == cur )
	{
		return 0;
	}

	return 1;



}
void char_from_chair ( Character *ch )
{
	struct obj_data *chair;
	Character *tempch, *firstch, *nextch;
	int found = 0;

	if ( !ch )
		return;

	if ( ! ( chair = SITTING ( ch ) ) )
	{
		//log("SYSERR: ACK, no chair for char in char from chair");
		SITTING ( ch ) = NULL;
		NEXT_SITTING ( ch ) = NULL;
		return;
	}

	if ( ! ( tempch = OBJ_SAT_IN_BY ( chair ) ) )
	{
		log ( "SYSERR: Char from chair, but no chair!" );
		SITTING ( ch ) = NULL;
		NEXT_SITTING ( ch ) = NULL;
		return;
	}

	if ( tempch == ch )
	{
		if ( !NEXT_SITTING ( ch ) )
			OBJ_SAT_IN_BY ( chair ) = NULL;
		else
			OBJ_SAT_IN_BY ( chair ) = NEXT_SITTING ( ch );
		GET_OBJ_VAL ( chair, 1 ) -= 1;
		SITTING ( ch ) = NULL;
		NEXT_SITTING ( ch ) = NULL;

		return;
	}

        for (firstch = tempch; firstch; firstch = nextch)
	{
            nextch = NEXT_SITTING(firstch);
            if (nextch && nextch == ch)
	    {
                NEXT_SITTING(firstch) = NEXT_SITTING(ch);
                GET_OBJ_VAL(chair, 1) -= 1;
		found = 1;
                break;
            }
	}
	if ( found == 0 )
		log ( "SYSERR: Char flagged as sitting, but not in chair" );

	SITTING ( ch ) = NULL;
	NEXT_SITTING ( ch ) = NULL;

	return;
}

/* move a player out of a room */
void char_from_room ( Character *ch )
{
	Character *temp;


	if ( ch == NULL )
	{
		log ( "SYSERR: NULL character in %s, char_from_room",   __FILE__ );
		ALERT_1;
		exit ( 1 );
	}
	else if ( IN_ROOM ( ch ) == NULL )
	{
		return;
	}

	if ( SITTING ( ch ) )
		char_from_chair ( ch );

	if ( FIGHTING ( ch ) != NULL )
		halt_fighting ( ch );
	else if ( GET_FIGHT_EVENT ( ch ) != NULL )
		stop_fighting ( ch );


	if ( GET_EQ ( ch, WEAR_LIGHT ) != NULL )
		if ( GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_LIGHT ) ) == ITEM_LIGHT )
			if ( GET_OBJ_VAL ( GET_EQ ( ch, WEAR_LIGHT ), 2 ) ) /* Light is ON */
				IN_ROOM ( ch )->light--;

	if ( !IS_NPC ( ch ) )
		zone_table[IN_ROOM ( ch )->zone].num_players--;

	REMOVE_FROM_LIST ( ch, IN_ROOM ( ch )->people, next_in_room );
	IN_ROOM ( ch ) = NULL;
	ch->next_in_room = NULL;

}

bool is_same_zone(int dv, int cv)
{

    if (dv == cv)
        return TRUE;
/*
    if (cv == 18 || cv == 20)
        if (dv == 18 || dv == 20)
            return TRUE;
    if (cv == 24 || cv == 34)
        if (dv == 24 || dv == 34)
            return TRUE;
    if (cv == 27 || cv == 167)
        if (dv == 27 || dv == 167)
            return TRUE;
    if (cv == 63 || cv == 82)
        if (dv == 63 || dv == 82)
            return TRUE;
    if (cv == 96 || cv == 97)
        if (dv == 96 || dv == 97)
            return TRUE;
    if (cv == 110 || cv == 112)
        if (dv == 110 || dv == 112)
            return TRUE;
    if (cv == 118 || cv == 119)
        if (dv == 118 || dv == 119)
            return TRUE;
    if (cv == 145 || cv == 232)
        if (dv == 145 || dv == 232)
            return TRUE;
    if (cv == 172 || cv == 199)
        if (dv == 172 || dv == 199)
            return TRUE;
    if (cv == 180 || cv == 314)
        if (dv == 180 || dv == 314)
            return TRUE;
    if (cv == 187 || cv == 237)
        if (dv == 187 || dv == 237)
            return TRUE;
    if (cv == 189 || cv == 203)
        if (dv == 189 || dv == 203)
            return TRUE;
    if (cv == 219 || cv == 270)
        if (dv == 219 || dv == 270)
            return TRUE;
    if (cv == 40 || cv == 500 || cv == 550)
        if (dv == 40 || dv == 500 || dv == 550)
            return TRUE;
    if (cv == 43 || cv == 115 || cv == 116)
        if (dv == 43 || dv == 115 || dv == 116)
            return TRUE;
    if (cv == 58 || cv == 220 || cv == 383)
        if (dv == 58 || dv == 220 || dv == 383)
            return TRUE;
    if (cv == 70 || cv == 71 || cv == 72)
        if (dv == 70 || dv == 71 || dv == 72)
            return TRUE;
    if (cv == 74 || cv == 126 || cv == 160)
        if (dv == 74 || dv == 126 || dv == 160)
            return TRUE;
    if (cv == 62 || cv == 520 || cv == 521 || cv == 522)
        if (dv == 62 || dv == 520 || dv == 521 || dv == 522)
            return TRUE;
    if (cv == 123 || cv == 142 || cv == 146 || cv == 233)
        if (dv == 123 || dv == 142 || dv == 146 || dv == 233)
            return TRUE;
    if (cv == 132 || cv == 133 || cv == 134 || cv == 143)
        if (dv == 132 || dv == 133 || dv == 134 || dv == 143)
            return TRUE;
    if (cv == 560 || cv == 561 || cv == 562 || cv == 563 || cv == 600)
        if (dv == 560 || dv == 561 || dv == 562 || dv == 563 || dv == 600)
            return TRUE;
*/
    return FALSE;

}

/* place a character in a room */
void char_to_room ( Character *ch, room_rnum room )
{
  int zone_num;
	if ( IN_ROOM ( ch ) != NULL )
	{
		log ( "char_to_room called when current room doesnt equal null" );
		char_from_room ( ch );
	}

	if ( IS_NPC ( ch ) )
		move_link_room ( ch, room );

	if ( ch == NULL || room == NULL || room->number < 0 || room->number > top_of_world )
	{
		log ( "SYSERR: Illegal value(s) passed to char_to_room. (Room: %d/%d Ch: %p)", room != NULL ? room->number : -1, top_of_world, ch );
		//send_to_all("{cRALERT: Stability issue!\r\n");
		room = real_room ( 3001 );
	}
	if ( room == NULL )
	{
		log ( "SYSERR: Illegal value(s) passed to char_to_room. Ch: %p)", ch );
		exit ( 0 );
	}
	ch->next_in_room = room->people;
	room->people = ch;
	IN_ROOM ( ch ) = room;

	if ( GET_EQ ( ch, WEAR_LIGHT ) )
		if ( GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_LIGHT ) ) == ITEM_LIGHT )
			if ( GET_OBJ_VAL ( GET_EQ ( ch, WEAR_LIGHT ), 2 ) ) /* Light ON */
				room->light++;

	if ( !IS_NPC ( ch ) )
		zone_table[IN_ROOM ( ch )->zone].num_players--;

	/* Stop fighting now, if we left. */
	if ( FIGHTING ( ch ) && IN_ROOM ( ch ) != IN_ROOM ( FIGHTING ( ch ) ) )
	{
		stop_fighting ( FIGHTING ( ch ) );
		stop_fighting ( ch );
	}

    /* HORUS - clan deeds
         - For any given zone, they have to be in that zone for at least
         - five minutes before their time_in starts.
         - If they leave any zone for more than five minutes, their
         - current zone gets saved. They then cant leave this zone for
         - more than fives minutes or it gets overwritten by the next zone
    ************************************************************/
  if (!IS_NPC(ch)) {
    zone_num = zone_table[room->zone].number;
    if (ch->player.deeds.zone == 0) {
        ch->player.deeds.zone = zone_num;
        ch->player.deeds.time_in = time(0);
        ch->player.deeds.time_out = 0;
        ch->player.deeds.kills = 0;
    }
    else if (!is_same_zone(ch->player.deeds.zone, zone_num)) {
	ch->player.deeds.zone = zone_num;
	ch->player.deeds.kills = 0;
	ch->player.deeds.time_in = time(0);
        if (ch->player.deeds.time_out == 0)  {
            ch->player.deeds.time_out = time(0);
        }
        else if (time(0) - ch->player.deeds.time_out > 0) {
            ch->player.deeds.zone = zone_num;
            ch->player.deeds.time_in = time(0);
            ch->player.deeds.time_out = 0;
            ch->player.deeds.kills = 0;
        }
    }
    else if (is_same_zone(ch->player.deeds.zone, zone_num))
        ch->player.deeds.time_out = 0;
  }            

}


/* give an object to a char   */
void obj_to_char ( struct obj_data *object, Character *ch )
{


	if ( object && ch )
	{
		object->next_content = ch->carrying;
		ch->carrying = object;
		object->carried_by = ch;
		IN_ROOM ( object ) = NULL;
		IS_CARRYING_W ( ch ) += GET_OBJ_WEIGHT ( object );
		IS_CARRYING_N ( ch ) ++;

		/* set flag for crash-save system, but not on mobs! */
		if ( !IS_NPC ( ch ) )
		{
			SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );
			check_timer ( object );
		}

#if defined(ARTIS_ARE_OWNED)

		if ( GET_OBJ_RNUM ( object ) != NOTHING && obj_index[GET_OBJ_RNUM ( object ) ].qic )
		{
			if ( !IS_NPC ( ch ) && object->owner == 0 && GET_LEVEL ( ch ) < LVL_HERO )
			{
				ch->Send ( "%s binds itself to you.\r\n", object->short_description );
				object->owner = GET_IDNUM ( ch );
			}

			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "%s to character %s (Owner: %s)",
			             object->short_description, GET_NAME ( ch ), object->owner == 0 ? "nobody" : pi.NameById ( object->owner ) );

		}
#endif


	}
	else
		log ( "SYSERR: NULL or dead obj (%p) or char (%p) passed to obj_to_char.",
		      object, ch );
}


/* give an object to a char -- don't add the weight back */
void obj_to_char_no_weight ( struct obj_data *object, Character *ch )
{
	if ( object && ch )
	{
		object->next_content = ch->carrying;
		ch->carrying = object;
		object->carried_by = ch;
		IN_ROOM ( object ) = NULL;
		IS_CARRYING_N ( ch ) ++;

		/* set flag for crash-save system, but not on mobs! */
		if ( !IS_NPC ( ch ) )
		{
			SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );
			check_timer ( object );
		}
	}
	else
		log ( "SYSERR: NULL or dead obj (%p) or char (%p) passed to obj_to_char.",
		      object, ch );
}

/* take an object from a char */
int obj_from_char ( struct obj_data *object )
{
	struct obj_data *temp;

	if ( object == NULL )
	{
		log ( "SYSERR: NULL object passed to obj_from_char." );
		return 0;
	}

	if ( object->carried_by == NULL )
	{
		log ( "SYSERR: Trying to remove obj from char that isn't held." );
		return 0;
	}

	REMOVE_FROM_LIST ( object, object->carried_by->carrying, next_content );

	/* set flag for crash-save system, but not on mobs! */
	if ( !IS_NPC ( object->carried_by ) )
		SET_BIT_AR ( PLR_FLAGS ( object->carried_by ), PLR_CRASH );

	IS_CARRYING_W ( object->carried_by ) -= GET_OBJ_WEIGHT ( object );
	IS_CARRYING_N ( object->carried_by )--;
	object->carried_by = NULL;
	object->next_content = NULL;
	return 1;
}



/* Return the effect of a piece of armor in position eq_pos */
int apply_ac ( Character *ch, int eq_pos )
{
	int factor;

	if ( GET_EQ ( ch, eq_pos ) == NULL )
	{
		return ( 0 );
	}

	if ( ! ( GET_OBJ_TYPE ( GET_EQ ( ch, eq_pos ) ) == ITEM_ARMOR ) )
	{
		if ( IS_OBJ_STAT ( GET_EQ ( ch, eq_pos ), ITEM_NODROP ) )
			return -5;
		else
			return ( 0 );
	}

	switch ( eq_pos )
	{
		case WEAR_SHIELD:
			factor = 4;
			break;
		case WEAR_BODY:
			factor = 3;
			break;               /* 30% */
		case WEAR_HEAD:
		case WEAR_LEGS:
			factor = 2;
			break;               /* 20% */
		default:
			factor = 1;
			break;               /* all others 10% */
	}

	if ( IS_OBJ_STAT ( GET_EQ ( ch, eq_pos ), ITEM_NODROP ) )
		return - ( abs ( factor * GET_OBJ_VAL ( GET_EQ ( ch, eq_pos ), 0 ) ) );
	else
		return ( factor * GET_OBJ_VAL ( GET_EQ ( ch, eq_pos ), 0 ) );
}

int invalid_align ( Character *ch, struct obj_data *obj )
{
	if ( IS_OBJ_STAT ( obj, ITEM_ANTI_EVIL ) && IS_EVIL ( ch ) )
		return TRUE;
	if ( IS_OBJ_STAT ( obj, ITEM_ANTI_GOOD ) && IS_GOOD ( ch ) )
		return TRUE;
	if ( IS_OBJ_STAT ( obj, ITEM_ANTI_NEUTRAL ) && IS_NEUTRAL ( ch ) )
		return TRUE;
	if ( IS_OBJ_STAT ( obj, ITEM_ANTI_FEMALE ) && GET_SEX ( ch ) == SEX_FEMALE )
		return TRUE;
	if ( IS_OBJ_STAT ( obj, ITEM_ANTI_MALE ) && GET_SEX ( ch ) == SEX_MALE )
		return TRUE;
	return FALSE;
}

int equip_char ( Character *ch, struct obj_data *obj, int pos )
{
	int j;
	struct affected_type *aff;

	if ( pos < 0 || pos >= NUM_WEARS )
	{
		core_dump();
		return 0;
	}
	if ( !obj )
	{
		log ( "SYSERR: NULL obj passed to equip_char." );
		return 0;
	}

	if ( GET_EQ ( ch, pos ) )
	{
		log ( "SYSERR: Char is already equipped: %s, %s", GET_NAME ( ch ),
		      obj->short_description );
		return 0;
	}
	if ( obj->carried_by )
	{
		log ( "SYSERR: EQUIP: Obj is carried_by when equip." );
		return 0;
	}
	if ( IN_ROOM ( obj ) != NULL )
	{
		log ( "SYSERR: EQUIP: Obj is in_room when equip." );
		return 0;
	}

	if ( invalid_align ( ch, obj ) || invalid_class ( ch, obj )
	        || invalid_race ( ch, obj ) )
	{
		act ( "You are zapped by $p and instantly let go of it.", FALSE, ch, obj, 0, TO_CHAR );
		act ( "$n is zapped by $p and instantly lets go of it.", FALSE, ch, obj, 0, TO_ROOM );
		obj_to_char ( obj, ch );  // changed to drop in inventory instead of the ground
		zap_char ( ch );
		return 0;
	}

	IS_CARRYING_W ( ch ) += GET_OBJ_WEIGHT ( obj );
	GET_EQ ( ch, pos ) = obj;
	obj->worn_by = ch;
	obj->worn_on = pos;

	if ( GET_OBJ_TYPE ( obj ) == ITEM_ARMOR )
		GET_AC ( ch ) -= apply_ac ( ch, pos );

	if ( IN_ROOM ( ch ) != NULL )
	{
		if ( pos == WEAR_LIGHT && GET_OBJ_TYPE ( obj ) == ITEM_LIGHT )
			if ( GET_OBJ_VAL ( obj, 2 ) )     /* if light is ON */
				IN_ROOM ( ch )->light++;
	}
	else if ( ch->loader == NOBODY )
		log ( "SYSERR: IN_ROOM(ch) = NOWHERE when equipping char %s.",
		      GET_NAME ( ch ) );

	for ( j = 0; j < MAX_OBJ_AFFECT; j++ )
		affect_modify_ar ( ch, obj->affected[j].location,
		                   obj->affected[j].modifier,
		                   obj->obj_flags.bitvector, TRUE );

	if ( obj->obj_flags.obj_innate )
	{
		for ( aff = ch->affected; aff; aff = aff->next )   /* run through list */
			if ( aff->type == obj->obj_flags.obj_innate ) /* if already affected */
				continue;
		OBJ_INNATE = TRUE;
		mag_affects ( 90, NULL, ch, obj->obj_flags.obj_innate, NOBODY );
		OBJ_INNATE = FALSE;
	}

	ch->affect_total();
	return 1;

}

struct obj_data *unequip_char ( Character *ch, int pos )
{
	int j;
	struct obj_data *obj = NULL;
	struct affected_type *aff = NULL, *anext;

	if ( ch == NULL )
		log ( "Null ch passed to unequip_char" );


	if ( ( pos < 0 || pos >= NUM_WEARS ) )
	{
		log ( "Pos %d outside position range of %d - %d passed to unequip_char", pos, 0, NUM_WEARS );
		core_dump();
		return ( NULL );
	}
	if ( GET_EQ ( ch, pos ) == NULL )
	{
		log ( "unequip_char asked to remove a position already unequipped!" );
		return NULL;
	}
	obj = GET_EQ ( ch, pos );


	obj->worn_by = NULL;
	obj->worn_on = -1;


	if ( GET_OBJ_TYPE ( obj ) == ITEM_ARMOR )
		GET_AC ( ch ) += apply_ac ( ch, pos );
	GET_EQ ( ch, pos ) = NULL;

	if ( IN_ROOM ( ch ) != NULL )
	{
		if ( pos == WEAR_LIGHT && GET_OBJ_TYPE ( obj ) == ITEM_LIGHT )
			if ( GET_OBJ_VAL ( obj, 2 ) )     /* if light is ON */
				IN_ROOM ( ch )->light--;
	}
	else
		log ( "SYSERR: IN_ROOM(ch) = NOWHERE when unequipping char %s.", GET_NAME ( ch ) );

	IS_CARRYING_W ( ch ) -= GET_OBJ_WEIGHT ( obj );

	for ( j = 0; j < MAX_OBJ_AFFECT; j++ )
		affect_modify_ar ( ch, obj->affected[j].location,
		                   obj->affected[j].modifier,
		                   obj->obj_flags.bitvector, FALSE );

	if ( obj->obj_flags.obj_innate > 0 )
	{
		for ( aff = ch->affected; aff; aff = anext )
		{
			anext = aff->next;
			if ( aff->type == obj->obj_flags.obj_innate )
				ch->affect_remove ( aff );
		}
	}
	ch->affect_total();

	return ( LS_REMOVE ? revert_object ( obj ) : obj );
}

void remove_all_normal_affects ( Character *ch )
{
	struct affected_type *aff = NULL, *anext;
	for ( aff = ch->affected;aff;aff=anext )
	{
		anext = aff->next;
		if ( ch->affected->bitvector == AFF_SILENCED )
			continue;
		if ( ch->affected->bitvector == AFF_IMMFREEZE )
			continue;
		ch->affect_remove ( aff );
	}
}

int get_number ( char **name )
{
	int i = 0, plen = 0;
	char *ppos;
	char number[MAX_INPUT_LENGTH];


	*number = '\0';

	if ( !*name || !**name )
		return 1;

	if ( ( ppos = strchr ( *name, '.' ) ) != NULL )
	{
		*ppos++ = '\0';
		strlcpy ( number, *name, sizeof ( number ) );
		//strcpy(*name, ppos);    /* strcpy: OK (always smaller) */
		memmove ( *name, ppos, ( plen = strlen ( ppos ) ) );
		* ( ( *name + ( plen ) ) ) = '\0';

		for ( i = 0; * ( number + i ); i++ )
			if ( !isdigit ( * ( number + i ) ) )
				return ( 0 );

		return ( atoi ( number ) );
	}
	return ( 1 );
}

int get_number ( const char **name )
{
	int i = 0, plen = 0;
	char *ppos;
	char number[MAX_INPUT_LENGTH] = "";

	if ( !*name || !**name )
		return 1;

	char nname[strlen ( *name ) + 1];
	strcpy ( nname, *name );

	if ( ( ppos = strchr ( nname, '.' ) ) != NULL )
	{
		*ppos++ = '\0';
		strlcpy ( number, nname, sizeof ( number ) );
		//strcpy(*name, ppos);    /* strcpy: OK (always smaller) */
		memmove ( nname, ppos, ( plen = strlen ( ppos ) ) );
		* ( ( ( char * ) *name + ( plen ) ) ) = '\0';

		for ( i = 0; * ( number + i ); i++ )
			if ( !isdigit ( * ( number + i ) ) )
				return ( 0 );

		return ( atoi ( number ) );
	}
	return ( 1 );
}



/* Search a given list for an object number, and return a ptr to that obj */
struct obj_data *get_obj_in_list_num ( obj_rnum num, struct obj_data *list )
{
	struct obj_data *i;

	for ( i = list; i; i = i->next_content )
		if ( GET_OBJ_RNUM ( i ) == num )
			return ( i );

	return ( NULL );
}



/* search the entire world for an object number, and return a pointer  */
struct obj_data *get_obj_num ( obj_rnum nr )
{
	for ( olt_it i = object_list.begin(); i != object_list.end(); i++ )
		if ( GET_OBJ_RNUM ( ( i->second ) ) == nr )
			return ( i->second );

	return ( NULL );
}



/* search a room for a char, and return a pointer if found..  */
Character *get_char_room ( const char *name, int *number, room_rnum room )
{
	Character *i;
	int num;

	if ( !name || !*name )
		return NULL;

	if ( !number )
	{
		num = get_number ( &name );
		number = &num;
	}

	if ( *number == 0 || !room )
		return ( NULL );

	for ( i = room->people; i && *number; i = i->next_in_room )
		if ( isname_full ( name, i->player.name ) )
			if ( -- ( *number ) == 0 )
				return ( i );

	return ( NULL );
}




/* search all over the world for a char num, and return a pointer if found */
Character *get_char_num ( mob_vnum nr )
{
	Character *i;

	for ( i = character_list; i; i = i->next )
		if ( GET_MOB_VNUM ( i ) == nr )
			return ( i );

	return ( NULL );
}


void pause_timer(struct obj_data* object) {
  if (GET_OBJ_EXPIRE(object)) {
    GET_OBJ_SAVED_REMAINING_EXPIRE(object) = GET_OBJ_EXPIRE(object) - time(0);
    GET_OBJ_EXPIRE(object) = 0;
  }
}

void resume_timer(struct obj_data* object) {
  if (GET_OBJ_SAVED_REMAINING_EXPIRE(object)) {
    GET_OBJ_EXPIRE(object) = time(0) + GET_OBJ_SAVED_REMAINING_EXPIRE(object);
    GET_OBJ_SAVED_REMAINING_EXPIRE(object) = 0;
  }
}

/* put an object in a room */
void obj_to_room ( struct obj_data *object, room_rnum room )
{
	if ( !object || !room || room->number < 0 || room->number > top_of_world )
		log ( "SYSERR: Illegal value(s) passed to obj_to_room.\r\n (Room #%d/%d, obj %p)", room ? room->number : -1, top_of_world, object );
	else
	{
		object->next_content = room->contents;
		room->contents = object;
		IN_ROOM ( object ) = room;
		object->carried_by = NULL;
		object->worn_by = NULL;
		if ( ROOM_FLAGGED ( room, ROOM_HOUSE ) )
			SET_BIT_AR ( ROOM_FLAGS ( room ), ROOM_HOUSE_CRASH );
		if ( GET_OBJ_RNUM ( object ) != NOTHING && obj_index[ ( int ) GET_OBJ_RNUM ( object ) ].qic )
		{
			new_mudlog ( CMP, LVL_SEN, TRUE, "%s, vnum: %d, to room %s.",
			             object->short_description, GET_OBJ_VNUM ( object ),
			             room->name );
		}

		if ( IS_OBJ_STAT ( object, ITEM_ARTIFACT ) && ROOM_FLAGGED(IN_ROOM(object), ROOM_ARTISAVE)) {
		  // save that artifact and make sure the timer doesn't run
		  pause_timer(object);
		  save_artifacts(IN_ROOM(object));
		}

	}
}


/* Take an object from a room */
void obj_from_room ( struct obj_data *object )
{
	struct obj_data *temp = NULL;

	if ( !object || IN_ROOM ( object ) == NULL )
	{
		log ( "SYSERR: NULL object (%p) or obj not in a room (%p) passed to obj_from_room", object, IN_ROOM ( object ) );
		return;
	}

	if ( IS_OBJ_STAT ( object, ITEM_PC_CORPSE ) )
	{
		remove_corpse_from_list ( object );
		REMOVE_BIT_AR ( GET_OBJ_EXTRA ( object ), ITEM_PC_CORPSE );
		SET_BIT_AR ( GET_OBJ_EXTRA ( object ), ITEM_NPC_CORPSE );
		save_corpses();
	}

	REMOVE_FROM_LIST ( object, IN_ROOM ( object )->contents,
	                   next_content );

	if ( ROOM_FLAGGED ( IN_ROOM ( object ), ROOM_HOUSE ) )
		SET_BIT_AR ( ROOM_FLAGS ( IN_ROOM ( object ) ), ROOM_HOUSE_CRASH );

	/*can remove this section to clean up the code*/
	if ( GET_OBJ_RNUM ( object ) != NOTHING && obj_index[GET_OBJ_RNUM ( object ) ].qic )
		new_mudlog ( CMP, LVL_SEN, TRUE, "%s from room %s.", object->short_description, IN_ROOM ( object )->name );

	if ( IS_OBJ_STAT (object, ITEM_ARTIFACT) && ROOM_FLAGGED(IN_ROOM(object), ROOM_ARTISAVE)) {
	  //remove from save list and make timer run again
	  resume_timer(object);
	  save_artifacts(IN_ROOM(object));
	}

	IN_ROOM ( object ) = NULL;
	object->next_content = NULL;
}


/* put an object in an object (quaint)  */
void obj_to_obj ( struct obj_data *obj, struct obj_data *obj_to )
{
	struct obj_data *tmp_obj = NULL;

	if ( !obj || !obj_to || obj == obj_to )
	{
		log ( "SYSERR: NULL object (%s)(%p) or same source (%p) and target (%p) obj passed to obj_to_obj.", obj->short_description, obj, obj, obj_to );
		return;
	}
	if ( IN_ROOM ( obj ) != NULL && ROOM_FLAGGED ( IN_ROOM ( obj ), ROOM_HOUSE ) )
		SET_BIT_AR ( ROOM_FLAGS ( IN_ROOM ( obj ) ), ROOM_HOUSE_CRASH );

	obj->next_content = obj_to->contains;
	obj_to->contains = obj;
	obj->in_obj = obj_to;

	for ( tmp_obj = obj->in_obj; tmp_obj->in_obj; tmp_obj = tmp_obj->in_obj )
		GET_OBJ_WEIGHT ( tmp_obj ) += GET_OBJ_WEIGHT ( obj );

	/* top level object.  Subtract weight from inventory if necessary. */
	GET_OBJ_WEIGHT ( tmp_obj ) += GET_OBJ_WEIGHT ( obj );
	if ( tmp_obj->carried_by )
		IS_CARRYING_W ( tmp_obj->carried_by ) += GET_OBJ_WEIGHT ( obj );
}


/* remove an object from an object */
int obj_from_obj ( struct obj_data *obj )
{
	struct obj_data *temp, *obj_from = NULL;

	if ( !obj || !obj->in_obj )
	{
		log ( "SYSERR: (%s): trying to illegally extract obj from obj.", __FILE__ );
		core_dump();
		return 0;
	}

	obj_from = obj->in_obj;
	REMOVE_FROM_LIST ( obj, obj_from->contains, next_content );

	/* Subtract weight from containers container */
	for ( temp = obj->in_obj; temp->in_obj; temp = temp->in_obj )
		GET_OBJ_WEIGHT ( temp ) -= GET_OBJ_WEIGHT ( obj );

	/* Subtract weight from char that carries the object */
	GET_OBJ_WEIGHT ( temp ) -= GET_OBJ_WEIGHT ( obj );
	if ( temp->carried_by )
		IS_CARRYING_W ( temp->carried_by ) -= GET_OBJ_WEIGHT ( obj );

	if ( IN_ROOM ( obj ) != NULL && ROOM_FLAGGED ( IN_ROOM ( obj ), ROOM_HOUSE ) )
		SET_BIT_AR ( ROOM_FLAGS ( IN_ROOM ( obj ) ), ROOM_HOUSE_CRASH );

	obj->in_obj = NULL;
	obj->next_content = NULL;
	return 1;
}


/* Set all carried_by to point to new owner */
void object_list_new_owner ( struct obj_data *list, Character *ch )
{
	if ( list )
	{
		object_list_new_owner ( list->contains, ch );
		object_list_new_owner ( list->next_content, ch );
		list->carried_by = ch;
	}
}

/* Extract an object from the world */
void extract_obj ( struct obj_data *obj )
{

	Character *ch, *tch, *next = NULL;
	struct obj_data *tobj, *onext, *tnext;
	int chance = 20;
	room_rnum room = NULL, from, target;

	if ( !obj )
	{
		log ( "Null pointer given to extract object." );
		return;
	}

	if ( GET_ID ( obj ) == 0 )
	{
		log ( "extracting object that hasn't been initilised" );
	}
	if ( dead_obj.find ( GET_ID ( obj ) ) != dead_obj.end() )
	{
		log ( "Object %s atempted to be added to dead list twice!", obj->short_description );
		return;
	}
	if ( IS_OBJ_STAT ( obj, ITEM_PC_CORPSE ) )
	{
		save_corpses();
	}
	if ( obj->in_locker )
		item_from_locker ( obj->in_locker, obj );

        /* vehicle2 code by Horus */
        if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE2) 
            delete_vehicle(obj);

	unhitch_item ( obj );

	/* Normal extract_obj code */
	if ( obj->worn_by != NULL )
		if ( unequip_char ( obj->worn_by, obj->worn_on ) != obj )
		{
			log ( "SYSERR: Inconsistent worn_by and worn_on pointers!!" );
			return;
		}
	if ( ( room = IN_ROOM ( obj ) ) != NULL )
		obj_from_room ( obj );
	if ( obj->carried_by )
		obj_from_char ( obj );
	if ( obj->in_obj )
		obj_from_obj ( obj );

	if ( GET_OBJ_RNUM ( obj ) != NOTHING )
		( obj_index[GET_OBJ_RNUM ( obj ) ].number )--;


	/* Get rid of the contents of the object, as well. */
	for ( tobj = obj->contains; tobj; tobj = onext )
	{
		onext = tobj->next_content;
		//obj_from_obj(tobj);
		extract_obj ( tobj );
		//tobj->in_obj = NULL;
	}

	/* Purge a vehicle and leave the contents in the room -- Kalten */
	if ( GET_OBJ_TYPE ( obj ) == ITEM_VEHICLE && room != NULL )
	{
		if ( room->number != NOWHERE )
		{
			if ( GET_OBJ_VAL ( obj, 0 ) == 0 )
			{
				new_mudlog ( NRM, LVL_SEN, FALSE, "Vehicle %d has no room set.", GET_OBJ_VNUM ( obj ) );

			}
			else
			{
				target = room;   // the room the vehicle is in
				from = real_room ( GET_OBJ_VAL ( obj, 0 ) );    // the room where the mobs/objs are in

				// First, get all the people out of the room
				if ( from != NULL )
				{
					if ( target == from )
						new_mudlog ( NRM, LVL_GOD, FALSE, "Vehicle %d's inside room is the same as its outside room!", GET_OBJ_VNUM ( obj ) );
					else
					{
						while ( from->people != NULL )
						{
							tch = from->people;
							if ( tch != NULL )
							{
								if ( damage ( tch, tch, dice ( 10, 20 ), TYPE_UNDEFINED ) >= 0 )
								{
									move_char_to ( tch, target );
								}
							}
						}

						// Now get all the objects from the room
						for ( tobj = from->contents; tobj; tobj = tnext )
						{
							tnext = tobj->next_content;
							obj_from_room ( tobj );
							if ( GET_OBJ_TYPE ( tobj ) != ITEM_V_CONTROLS &&
							        GET_OBJ_TYPE ( tobj ) != ITEM_V_HATCH &&
							        GET_OBJ_TYPE ( tobj ) != ITEM_V_WINDOW &&
								number ( 1, 100) < chance)
							{
								obj_to_room ( tobj, target );
							}
							else
							{
								extract_obj ( tobj );
							}
						}
					}
				}
			}
		}
	}

	/* cancel message updates */
	if ( GET_TIMER_EVENT ( obj ) )
	{
		event_cancel ( GET_TIMER_EVENT ( obj ) );
		GET_TIMER_EVENT ( obj ) = NULL;
	}
	if ( TRAVEL_LIST ( obj ) != NULL )
	{
		free_travel_points ( TRAVEL_LIST ( obj ) );
		TRAVEL_LIST ( obj ) = NULL;
	}

	for ( ch = OBJ_SAT_IN_BY ( obj ); ch; ch = next )
	{

		if ( ch )
		{
			next = NEXT_SITTING ( ch );
			SITTING ( ch ) = NULL;
			NEXT_SITTING ( ch ) = NULL;
		}
		else
			next = NULL;
	}
	OBJ_SAT_IN_BY ( obj ) = NULL;

	if ( SCRIPT ( obj ) )
		extract_script ( obj, OBJ_TRIGGER );

	if ( GET_OBJ_RNUM ( obj ) == NOTHING || obj->proto_script != obj_proto[GET_OBJ_RNUM ( obj ) ].proto_script )
		free_proto_script ( obj, OBJ_TRIGGER );

	if ( ( GET_OBJ_TYPE ( obj ) == ITEM_TREE ) && ( GET_OBJ_VNUM ( obj ) == NOTHING ) )
	{
		tree_total--;
	}

	if ( GET_OBJ_RNUM ( obj ) != NOTHING && obj_index[GET_OBJ_RNUM ( obj ) ].qic )
	{
		new_mudlog ( CMP, LVL_SEN, FALSE, "%s purged", obj->short_description );
		purge_qic ( GET_OBJ_RNUM ( obj ) );
	}
	obj->carried_by = NULL;
	obj->next_content = NULL;
	obj_data_to_pool ( obj );

}

void crumble_obj ( Character *ch, struct obj_data *obj )
{
	struct obj_data *loop;
	int index;

	if ( IN_ROOM ( obj ) != NULL )
		if ( ROOM_FLAGGED ( IN_ROOM ( obj ), ROOM_HOUSE ) )
			SET_BIT_AR ( IN_ROOM ( obj )->room_flags, ROOM_HOUSE_CRASH );

	if ( GET_OBJ_TYPE ( obj ) == ITEM_PORTAL )    /* If it is a portal */
	{
		if ( obj->carried_by == NULL && IN_ROOM ( obj ) != NULL )
		{
			act ( "A glowing portal fades from existence.",
			      TRUE, IN_ROOM ( obj )->people, obj, 0, TO_ROOM );
			act ( "A glowing portal fades from existence.",
			      TRUE, IN_ROOM ( obj )->people, obj, 0, TO_CHAR );
		}
		else if ( obj->carried_by )
		{
			obj_from_char ( obj );
		}
		extract_obj ( obj );

	}
	else if ( IN_ROOM ( obj ) != NULL )    /* In a room */
	{
		if ( IN_ROOM ( obj )->people )
		{
			act ( "A quivering horde of maggots consumes $p.",
			      TRUE, IN_ROOM ( obj )->people, obj, 0, TO_ROOM );
			act ( "A quivering horde of maggots consumes $p.",
			      TRUE, IN_ROOM ( obj )->people, obj, 0, TO_CHAR );
		}
		for ( loop = obj->contains; loop; loop = obj->contains )
		{
			obj_from_obj ( loop );
			obj_to_room ( loop, IN_ROOM ( obj ) );
		}

		obj_from_room ( obj );

	}
	else if ( obj->in_locker )
	{
		item_from_locker ( obj->in_locker, obj );
	}
	else if ( !obj->in_obj && obj->carried_by )  /* Worn or inventory */
	{

		for ( loop = obj->contains; loop; loop = obj->contains )
		{
			obj_from_obj ( loop );
			obj_to_char ( loop, ch );
		}
		if (!obj->carried_by)     /* Equipped */ 
		{
			for ( index = 0; index < NUM_WEARS; index++ )
				if ( GET_EQ ( ch, index ) == obj )
				{
					obj = unequip_char ( ch, index );
					act ( "$p decays in your hands.", FALSE, ch, obj, 0,              TO_CHAR );
					act ( "$p decays in $n's hands.", FALSE, ch, obj, 0,              TO_ROOM );
					obj_from_char(obj); // Once's fix for decaying items
				}
		}
		else  
		{
			act ( "$p crumbles into dust.", FALSE, ( obj->carried_by ), obj, 0, TO_CHAR );
			obj_from_char ( obj );
		}
	}
	else if ( obj->in_obj )            /* In an object */
	{
		for ( loop = obj->contains; loop; loop = obj->contains )
		{
			obj_from_obj ( loop );
			obj_to_obj ( loop, obj->in_obj );
		}
		obj_from_obj ( obj );
	}
	extract_obj ( obj );
}


void update_object ( Character *ch, struct obj_data *obj, int use, time_t timenow )
{

        if ( GET_OBJ_TIMER ( obj ) == -1 || GET_OBJ_SAVED_REMAINING_EXPIRE(obj) != 0)
		return;
	// log("(update_obj.1) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));
	/* don't update objects with a timer trigger */

	// log("(update_obj.2) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));
	if ( obj->next_content )
		update_object ( ch, obj->next_content, use, timenow );
	// log("(update_obj.3) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));
	if ( obj->contains )
		update_object ( ch, obj->contains, use, timenow );
	// log("(update_obj.4) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));

//	if ( timenow <= GET_OBJ_EXPIRE ( obj ) )
      if ( GET_OBJ_EXPIRE ( obj ) > time ( 0 ) )

	{
		if ( timer_otrigger ( obj ) == -1 )
			return;

		crumble_obj ( ch, obj );
	}
	// log("(update_obj.6) obj: %s, timer: %d", obj->name, GET_OBJ_TIMER(obj));
}


void update_char_objects ( Character *ch )
{
	int i;

	if ( IN_ROOM ( ch ) != NULL && GET_EQ ( ch, WEAR_LIGHT ) != NULL )
		if ( GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_LIGHT ) ) == ITEM_LIGHT )
			if ( GET_OBJ_VAL ( GET_EQ ( ch, WEAR_LIGHT ), 2 ) > 0 )
			{
				i = --GET_OBJ_VAL ( GET_EQ ( ch, WEAR_LIGHT ), 2 );
				if ( i == 1 )
				{
					send_to_char ( "Your light begins to flicker and fade.\r\n", ch );
					act ( "$n's light begins to flicker and fade.", FALSE,ch, 0, 0, TO_ROOM );
				}
				else if ( i == 0 )
				{
					send_to_char ( "Your light sputters out and dies.\r\n", ch );
					act ( "$n's light sputters out and dies.", FALSE, ch, 0, 0, TO_ROOM );
					IN_ROOM ( ch )->light--;
				}
			}
#if 0
	/* this is just doubling up the full object scan done in point update */
	for ( i = 0; i < NUM_WEARS; i++ ) {
		
			if (GET_EQ(ch, i) == NULL)
                           continue;

			if ( GET_EQ ( ch, i ) ) 
			update_object ( ch, GET_EQ ( ch, i ), 1 );
}                       
	if ( ch->carrying )
		update_object ( ch, ch->carrying, 0 );
#endif

}

void eq_to_room ( Character *ch )
{
	OBJ_DATA *obj;
	int i;
	/* transfer objects to room, if any */
	while ( ch->carrying )
	{
		obj = ch->carrying;
		if ( obj_from_char ( obj ) == 0 )
			extract_obj ( obj );
		if ( IN_ROOM ( ch ) == NULL )
			extract_obj ( obj );
		else
			obj_to_room ( obj, IN_ROOM ( ch ) );
	}

	/* transfer equipment to room, if any */
	for ( i = 0; i < NUM_WEARS; i++ )
		if ( GET_EQ ( ch, i ) )
		{
			obj = unequip_char ( ch, i );
			if ( IN_ROOM ( ch ) == NULL )
				extract_obj ( obj );
			else
				obj_to_room ( obj, IN_ROOM ( ch ) );
		}
}

void death_room ( Character *ch )
{
	struct hunter_data *hunt = NULL, *hnext;


	if ( IS_NPC ( ch ) )
	{
		extract_char ( ch );
		return;
	}

	ch->Send ( "{cYAs your last breath passes, time rolls back to just before you died\r\n"
	           "and you find yourself transfered to a temple of healing.{c0\r\n" );

	GET_HIT ( ch ) = 3;

	if ( RIDING ( ch ) || RIDDEN_BY ( ch ) )
		dismount_char ( ch );

	stop_fusion ( ch );

	/** leave people grouped when they die **/
	/*if (ch->followers || ch->master)
	 die_follower(ch);*/

	/*ends fight event*/
	if ( FIGHTING ( ch ) )
		halt_fighting ( ch );
	if ( SITTING ( ch ) )
		char_from_chair ( ch );

	if ( ch->hitched )
		unhitch_mob ( ch );

	/* cancel message updates */
	ch->ClearMessageEvents();
	/* cancel the task */
	stop_task ( ch );
	remove_hunter ( ch );


	/* remove any pending event for/from this character */
	clean_events2 ( ch );

	/* we can't forget the hunters either... */
	for ( hunt = hunter_list; hunt; hunt = hnext )
	{
		hnext = hunt->next;
		if ( !hunt->hunter )
			continue;
		if ( HUNTING ( hunt->hunter ) != ch )
			continue;

		forget ( hunt->hunter, ch );
		forget ( ch, hunt->hunter );
		remove_hunter ( hunt->hunter );

	}
	if ( GET_RACE ( ch ) == RACE_GLADIATOR )
		move_char_to ( ch, CONFIG_GLA_DEATH_ROOM );
	else
		move_char_to ( ch, real_room ( 1205 ) );

	GET_WAIT_STATE ( ch ) = 2 RL_SEC;
	ch->affect_total();
	ch->check_regen_rates();
	ch->save();
	LOOK ( ch );

}
void free_hunter_list ( void )
{

	struct hunter_data *hunt, *hnext;
	/* we can't forget the hunters either... */
	for ( hunt = hunter_list; hunt; hunt = hnext )
	{
		hnext = hunt->next;

		remove_hunter ( hunt->hunter );
	}
	hunter_list = NULL;
}

#if 1
/* Extract a ch completely from the world, and leave his stuff behind */
void extract_char_final ( Character *ch )
{

	Descriptor *d = NULL;
	struct hunter_data *hunt = NULL, *hnext;
	int i;
        Character *temp;
        struct obj_data *obj;
	if ( !ch )
		return;
	if ( IN_ROOM ( ch ) == NULL )
	{
		log ( "SYSERR: NOWHERE extracting char %s. (%s, extract_char_final)", GET_NAME ( ch ), __FILE__ );
		ALERT_1;
		abort();
	}

	/* Forget snooping, if applicable */
	if ( ch->desc )
	{
		if ( ch->desc->snooping )
		{
			ch->desc->snooping->snoop_by = NULL;
			ch->desc->snooping = NULL;
		}
		if ( ch->desc->snoop_by )
		{
			ch->desc->snoop_by->Output ( "Your victim is no longer among us.\r\n" );
			ch->desc->snoop_by->snooping = NULL;
			ch->desc->snoop_by = NULL;
		}
	}
	/*
	 * We're booting the character of someone who has switched so first we
	 * need to stuff them back into their own body.  This will set ch->desc
	 * we're checking below this loop to the proper value.
	 */
	if ( !IS_NPC ( ch ) && !ch->desc )
	{
		for ( d = descriptor_list; d; d = d->next )
			if ( d->original == ch )
			{
				do_return ( d->character, NULL, 0, 0 );
				break;
			}
	}

	if ( ch->desc )
	{
		/*
		 * This time we're extracting the body someone has switched into
		 * (not the body of someone switching as above) so we need to put
		 * the switcher back to their own body.
		 *
		 * If this body is not possessed, the owner won't have a
		 * body after the removal so dump them to the main menu.
		 */
		if ( ch->desc->original )
			do_return ( ch, NULL, 0, 0 );
		else
		{
			/*
			 * Now we boot anybody trying to log in with the same character, to
			 * help guard against duping.  CON_DISCONNECT is used to close a
			 * descriptor without extracting the d->character associated with it,
			 * for being link-dead, so we want CON_CLOSE to clean everything up.
			 * If we're here, we know it's a player so no IS_NPC check required.
			 */
			for ( d = descriptor_list; d; d = d->next )
			{
				if ( d == ch->desc )
					continue;
				if ( d->character  && GET_ACC ( ch ) == GET_ACC ( d->character ) )
					STATE ( d ) = CON_CLOSE;
			}
			STATE ( ch->desc ) = CON_MENU;
			ch->desc->Output ( "%s", MENU );
		}
	}

	/*ends fight event*/

	halt_fighting ( ch );
	if ( SITTING ( ch ) )
		char_from_chair ( ch );

	if ( RIDING ( ch ) || RIDDEN_BY ( ch ) )
		dismount_char ( ch );

	unhitch_mob ( ch );

	stop_fusion ( ch );

	die_link ( ch );

	if ( ch->followers || ch->master )
		die_follower ( ch );

        /* Horus bug fix - lets fix the list of players sitting on furniture */
        if ((obj = SITTING(ch))) {
            if (OBJ_SAT_IN_BY(obj) == ch) {
                if (NEXT_SITTING(ch))
                    OBJ_SAT_IN_BY(obj) = NEXT_SITTING(ch);
                else
                    OBJ_SAT_IN_BY(obj) = NULL;
            }
            else
                REMOVE_FROM_LIST(ch, OBJ_SAT_IN_BY(obj), char_specials.next_in_chair);
            SITTING(ch) = NULL;
        }
 
	eq_to_room ( ch );

	/* remove the locker memory*/
	extract_all_in_list ( LOCKER ( ch ) );
	LOCKER ( ch ) = NULL;
	if ( GET_FIGHT_EVENT ( ch ) )
	{
		event_cancel ( GET_FIGHT_EVENT ( ch ) );
		GET_FIGHT_EVENT ( ch ) = NULL;
	}
	/* cancel point updates */
	for ( i = 0; i < 4; i++ )
		if ( GET_POINTS_EVENT ( ch, i ) )
		{
			event_cancel ( GET_POINTS_EVENT ( ch, i ) );
			GET_POINTS_EVENT ( ch, i ) = NULL;
		}
	/* cancel message updates */
//HORUS	ch->ClearMessageEvents();
	/* cancel the task */
	stop_task ( ch );
	if ( SCRIPT ( ch ) )
		extract_script ( ch, MOB_TRIGGER );
	if ( !IS_NPC ( ch ) )
	{
		void free_alias ( struct alias_data *a );
		struct alias_data *a;
		while ( ( a = GET_ALIASES ( ch ) ) != NULL )
		{
			GET_ALIASES ( ch ) = ( GET_ALIASES ( ch ) )->next;
			free_alias ( a );
		}
	}
	remove_hunter ( ch );

	if ( ch->hitched )
		unhitch_mob ( ch );

	/* we can't forget the hunters either... */
	for ( hunt = hunter_list; hunt; hunt = hnext )
	{
		hnext = hunt->next;

		if ( !hunt->hunter )
			continue;
		if ( HUNTING ( hunt->hunter ) != ch )
			continue;

		//
		forget ( hunt->hunter, ch );
		forget ( ch, hunt->hunter );
		remove_hunter ( hunt->hunter );
		//    HUNTING(hunt->hunter) = NULL;

	}

	/* remove any pending event for/from this character */
	clean_events2 ( ch );
	char_from_room ( ch );

	if ( IS_NPC ( ch ) )
	{
		if ( GET_MOB_VNUM ( ch ) != NOBODY && !ch->proto )    /* prototyped */
			GetMobIndex ( GET_MOB_VNUM ( ch ) )->number--;
		clearMemory ( ch );
		if ( SCRIPT ( ch ) )
			extract_script ( ch, MOB_TRIGGER );
		//    if (ch->proto || !MobProtoExists(GET_MOB_VNUM(ch)) || ch->proto_script != GetMobProto(GET_MOB_VNUM(ch))->proto_script)
		//     free_proto_script(ch, MOB_TRIGGER);
		if ( SCRIPT_MEM ( ch ) )
			extract_script_mem ( SCRIPT_MEM ( ch ) );
	}
	else
	{
		ch->save();
	}

	if ( GET_ID ( ch ) > 0 )
		removeFromChLookupTable ( GET_ID ( ch ) );

	/* If there's a descriptor, they're in the menu now. */
	if ( IS_NPC ( ch ) || !ch->desc )
		delete ch;
}
#endif
/*
 * Q: Why do we do this?
 * A: Because trying to iterate over the character
 *    list with 'ch = ch->next' does bad things if
 *    the current character happens to die. The
 *    trivial workaround of 'vict = next_vict'
 *    doesn't work if the _next_ person in the list
 *    gets killed, for example, by an area spell.
 *
 * Q: Why do we leave them on the character_list?
 * A: Because code doing 'vict = vict->next' would
 *    get really confused otherwise.

 put all the files back
 except the files from the world directory
 exept files taht already exist that are older then the ones in the tar


 */
#if 1
void extract_char ( Character *ch, int e_now )
{
	if ( DEAD ( ch ) )
	{
		if ( IN_ROOM ( ch ) )
			log ( "Extracting char more then once (vnum:%d : name:%s : room:%d)", GET_MOB_VNUM ( ch ), GET_NAME ( ch ), GET_ROOM_VNUM ( IN_ROOM ( ch ) ) );
		else
			log ( "Extracting char more then once (vnum:%d : name:%s)", GET_MOB_VNUM ( ch ), GET_NAME ( ch ) );
		return;
	}


	if ( IS_NPC ( ch ) )
	{
		SET_BIT_AR ( MOB_FLAGS ( ch ), MOB_NOTDEADYET );
		extract_linked_mob ( ch );
	}
	else
		SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_NOTDEADYET );

	extractions_pending++;

	if ( e_now == 1 )
		extract_pending_chars ();
}
#endif

/*
 * I'm not particularly pleased with the MOB/PLR
 * hoops that have to be jumped through but it
 * hardly calls for a completely new variable.
 * Ideally it would be its own list, but that
 * would change the '->next' pointer, potentially
 * confusing some code. Ugh. -gg 3/15/2001
 *
 * NOTE: This doesn't handle recursive extractions.
 */
void extract_pending_chars ( void )
{
	Character *vict, *next_vict, *prev_vict;
	long idnum;

	if ( extractions_pending < 0 )
		log ( "SYSERR: Negative (%d) extractions pending.",
		      extractions_pending );

	for ( vict = character_list, prev_vict = NULL;
	        vict && extractions_pending; vict = next_vict )
	{
		next_vict = vict->next;

		if ( MOB_FLAGGED ( vict, MOB_NOTDEADYET ) )
			REMOVE_BIT_AR ( MOB_FLAGS ( vict ), MOB_NOTDEADYET );
		else if ( PLR_FLAGGED ( vict, PLR_NOTDEADYET ) )
			REMOVE_BIT_AR ( PLR_FLAGS ( vict ), PLR_NOTDEADYET );
		else
		{
			/* Last non-free'd character to continue chain from. */
			prev_vict = vict;
			continue;
		}
		idnum = GET_ID ( vict );
		extract_char_final ( vict );
		extractions_pending--;

		if ( idnum > 0 )
			removeFromChLookupTable ( idnum );

		if ( prev_vict )
			prev_vict->next = next_vict;
		else
			character_list = next_vict;
	}

	if ( extractions_pending > 0 )
		log ( "SYSERR: Couldn't find %d extractions as counted.",
		      extractions_pending );

	extractions_pending = 0;
}

/* ***********************************************************************
* Here follows high-level versions of some earlier routines, ie functions*
* which incorporate the actual player-data                               *.
*********************************************************************** */



Character *get_player_vis ( Character *ch, char *name,
                            int *number, int inroom )
{
	Character *i;
	Descriptor *d;
	int num;

	skip_spaces ( &name );

	if ( !number )
	{
		number = &num;
		num = get_number ( &name );
	}
	if ( *name == UID_CHAR )
	{
		i = find_char ( atoi ( name + 1 ) );

		if ( i && valid_dg_target ( i, TRUE ) )
			return i;
	}
	for ( d = descriptor_list; d; d = d->next )
	{
		if ( !IS_PLAYING ( d ) )
			continue;
		if ( inroom == FIND_CHAR_ROOM && !HERE ( d->character, ch ) )
			continue;
		if ( !isname ( name, d->character->player.name ) )
			continue;

		if ( !CAN_SEE ( ch, d->character ) )
			continue;
		if ( -- ( *number ) != 0 )
			continue;

		return ( d->character );

	}
	if ( inroom!=FIND_CHAR_NOTINROOM )
		for ( i = character_list; i; i = i->next )
		{
			if ( inroom == FIND_CHAR_ROOM && !HERE ( i, ch ) )
				continue;
			if ( IS_NPC ( i ) )
				continue;
			if ( inroom == FIND_CHAR_NOTINROOM && HERE ( i, ch ) )
				continue;
			if ( !isname_full ( name, i->player.name ) )
				continue;

			if ( !CAN_SEE ( ch, i ) )
				continue;
			if ( -- ( *number ) != 0 )
				continue;
			return ( i );
		}

	return ( NULL );
}


Character *get_player_room ( room_rnum room, char *name, int *number,
                             int inroom )
{
	Descriptor *i;
	Character *ch;
	int num;

	if ( !number )
	{
		number = &num;
		num = get_number ( &name );
	}
	if ( !name || !*name )
		return NULL;

	for ( i = descriptor_list; i; i = i->next )
	{
		ch = i->character;
		if ( !ch )
			continue;
		if ( !IS_PLAYING ( i ) )
			continue;
		if ( inroom == FIND_CHAR_ROOM && IN_ROOM ( ch ) != room )
			continue;
		if ( str_cmp ( ch->player.name,name ) ) /* If not same, continue */
			continue;
		if ( -- ( *number ) != 0 )
			continue;
		return ( ch );
	}

	return ( NULL );
}

//Default argument for ch is NULL, as defined in handler.h
Character *get_room_vis ( room_rnum room, char *name, int *number,Character *ch )
{
	Character *i;
	int num;

	if ( !number )
	{
		number = &num;
		num = get_number ( &name );
	}

	if ( room == NULL )
		return NULL;

	/* 0.<name> means PC with name */
	if ( *number == 0 )
		return ( get_player_room ( room, name, NULL, FIND_CHAR_ROOM ) );

	for ( i = room->people; i && *number; i = i->next_in_room )
		if ( isname_full ( name, GET_NAME ( i ) ) && ( !ch || CAN_SEE ( ch,i ) ) )
			if ( -- ( *number ) == 0 )
				return ( i );

	return ( NULL );
}

Character *get_char_room_vis ( Character *ch, char *name,
                               int *number )
{
	Character *i;
	int num = 1;

	if ( !number )
	{
		number = &num;
		if ( name && *name )
			num = get_number ( &name );
	}

	if ( !ch )
		return NULL;

	/* JE 7/18/94 :-) :-) */
	if ( !str_cmp ( name, "self" ) || !str_cmp ( name, "me" ) )
		return ( ch );

	/* 0.<name> means PC with name */
	if ( *number == 0 )
		return ( get_player_vis ( ch, name, NULL, FIND_CHAR_ROOM ) );

	for ( i = IN_ROOM ( ch )->people; i && *number; i = i->next_in_room )
		if ( isname_full ( name, i->player.name ) )
			if ( CAN_SEE ( ch, i ) )
				if ( -- ( *number ) == 0 )
					return ( i );

	return ( NULL );
}



Character *get_char_world_vis ( Character *ch, char *name,
                                int *number )
{
	Character *i;
	int num;

	if ( !ch )
		return NULL;

	if ( !number )
	{
		number = &num;
		num = get_number ( &name );
	}

	if ( ( i = get_char_room_vis ( ch, name, number ) ) != NULL )
		return ( i );

	if ( *number == 0 )
		return get_player_vis ( ch, name, NULL, 0 );

	for ( i = character_list; i && *number; i = i->next )
	{
		if ( HERE ( ch,i ) )
			continue;
		if ( !isname_full ( name, i->player.name ) )
			continue;
		if ( !CAN_SEE ( ch, i ) )
			continue;
		if ( -- ( *number ) != 0 )
			continue;

		return ( i );
	}
	return ( NULL );
}


Character *get_char_vis ( Character *ch, char *name,
                          int *number, int where )
{
	if ( where == FIND_CHAR_ROOM )
		return get_char_room_vis ( ch, name, number );
	else if ( where == FIND_CHAR_WORLD )
		return get_char_world_vis ( ch, name, number );
	else
		return ( NULL );
}



struct obj_data *get_obj_in_list_vis ( Character *ch, char *name,
			                                       int *number, struct obj_data *list )
{
	struct obj_data *i;
	int num;

	if ( !number )
	{
		number = &num;
		num = get_number ( &name );
	}

	if ( *number == 0 )
		return ( NULL );

	for ( i = list; i && *number; i = i->next_content )
		if ( isname_full ( name, i->name ) )
			if ( CAN_SEE_OBJ ( ch, i ) )
				if ( -- ( *number ) == 0 )
					return ( i );

	return ( NULL );
}




/* search the entire world for an object, and return a pointer  */
struct obj_data *get_obj_vis ( Character *ch, char *name, int *number )
{
	struct obj_data *i;
	int num;

	if ( !number )
	{
		number = &num;
		num = get_number ( &name );
	}

	if ( *number == 0 )
		return ( NULL );

	/* scan items carried */
	if ( ( i = get_obj_in_list_vis ( ch, name, number, ch->carrying ) ) != NULL )
		return ( i );

	/* scan room */
	if ( ( i =
	            get_obj_in_list_vis ( ch, name, number,
	                                  IN_ROOM ( ch )->contents ) ) != NULL )
		return ( i );

	/* ok.. no luck yet. scan the entire obj list   */
	for ( olt_it ob = object_list.begin(); ob != object_list.end() && *number; ob++ )
		if ( isname_full ( name, ( ob->second )->name ) )
			if ( CAN_SEE_OBJ ( ch, ( ob->second ) ) )
				if ( -- ( *number ) == 0 )
					return ( ( ob->second ) );

	return ( NULL );
}


struct obj_data *get_obj_in_equip_vis ( Character *ch, char *arg,
			                                        int *number,
			                                        struct obj_data *equipment[] )
{
	int j, num;

	if ( !number )
	{
		number = &num;
		num = get_number ( &arg );
	}

	if ( *number == 0 )
		return ( NULL );

	for ( j = 0; j < NUM_WEARS; j++ )
		if ( equipment[j] && CAN_SEE_OBJ ( ch, equipment[j] )
		        && isname_full ( arg, equipment[j]->name ) )
			if ( -- ( *number ) == 0 )
				return ( equipment[j] );

	return ( NULL );
}
int get_obj_pos_in_equip_vis ( Character *ch, char *arg, int *number,
                               struct obj_data *equipment[] )
{
	int j, num;

	if ( !number )
	{
		number = &num;
		num = get_number ( &arg );
	}

	if ( *number == 0 )
		return ( -1 );

	for ( j = 0; j < NUM_WEARS; j++ )
		if ( equipment[j] && CAN_SEE_OBJ ( ch, equipment[j] )
		        && isname_full ( arg, equipment[j]->name ) )
			if ( -- ( *number ) == 0 )
				return ( j );

	return ( -1 );
}


const char *money_desc ( gold_int amount )
{
	gold_int cnt;
	struct
	{
		gold_int limit;
		const char *description;
	}
	money_table[] =
	{
		{
			1, "a gold coin"}, {
			10, "a tiny pile of gold coins"}, {
			20, "a handful of gold coins"}, {
			75, "a little pile of gold coins"}, {
			200, "a small pile of gold coins"}, {
			1000, "a pile of gold coins"}, {
			5000, "a big pile of gold coins"}, {
			10000, "a large heap of gold coins"}, {
			20000, "a huge mound of gold coins"}, {
			75000, "an enormous mound of gold coins"}, {
			150000, "a small mountain of gold coins"}, {
			250000, "a mountain of gold coins"}, {
			500000, "a huge mountain of gold coins"}, {
			1000000, "an enormous mountain of gold coins"}, {
			0, NULL},
	};

	if ( amount <= 0 )
	{
		log ( "SYSERR: Try to create negative or 0 money (%lld).", amount );
		return ( NULL );
	}

	for ( cnt = 0; money_table[cnt].limit; cnt++ )
		if ( amount <= money_table[cnt].limit )
			return ( money_table[cnt].description );

	return ( "an absolutely colossal mountain of gold coins" );
}


struct obj_data *create_money ( gold_int gamount )
{
	int y;
	struct obj_data *obj;
	struct extra_descr_data *new_descr;
	char buf[200];
	int amount;

	if ( gamount > 2000000000 )
		amount = 2000000000;
	else
		amount = ( int ) gamount;

	if ( amount <= 0 )
	{
		log ( "SYSERR: Try to create negative or 0 money, changing to 1(%lld)", gamount );
		return ( NULL );
	}
	obj = create_obj ( NOTHING );
	CREATE ( new_descr, struct extra_descr_data, 1 );

	if ( amount == 1 )
	{
		obj->name = str_dup ( "coin gold" );
		obj->short_description = str_dup ( "a gold coin" );
		obj->description =
		    str_dup ( "One miserable gold coin is lying here." );
		new_descr->keyword = str_dup ( "coin gold" );
		new_descr->description =
		    str_dup ( "It's just one miserable little gold coin." );
	}
	else
	{
		obj->name = str_dup ( "coins gold" );
		obj->short_description = str_dup ( money_desc ( amount ) );
		sprintf ( buf, "%s is lying here.", money_desc ( amount ) );
		obj->description = str_dup ( CAP ( buf ) );

		new_descr->keyword = str_dup ( "coins gold" );
		if ( amount < 10 )
		{
			sprintf ( buf, "There are %d coins.", amount );
			new_descr->description = str_dup ( buf );
		}
		else if ( amount < 100 )
		{
			sprintf ( buf, "There are about %d coins.",
			          10 * ( amount / 10 ) );
			new_descr->description = str_dup ( buf );
		}
		else if ( amount < 1000 )
		{
			sprintf ( buf, "It looks to be about %d coins.",
			          100 * ( amount / 100 ) );
			new_descr->description = str_dup ( buf );
		}
		else if ( amount < 100000 )
		{
			sprintf ( buf, "You guess there are, maybe, %d coins.",
			          1000 * ( ( ( int ) amount / 1000 ) +
			                   number ( 0, ( ( int ) amount / 1000 ) ) ) );
			new_descr->description = str_dup ( buf );
		}
		else
			new_descr->description = str_dup ( "There are a LOT of coins." );
	}

	new_descr->next = NULL;
	obj->ex_description = new_descr;

	GET_OBJ_TYPE ( obj ) = ITEM_MONEY;
	for ( y = 0; y < TW_ARRAY_MAX; y++ )
		obj->obj_flags.wear_flags[y] = 0;
	SET_BIT_AR ( GET_OBJ_WEAR ( obj ), ITEM_WEAR_TAKE );
	GET_OBJ_VAL ( obj, 0 ) = ( int ) amount;
	GET_OBJ_COST ( obj ) = ( int ) amount;
	obj->item_number = NOTHING;
	if ( GET_OBJ_RNUM ( obj ) != NOTHING ) //this is because it has a negitive rnum.
		obj_index[GET_OBJ_RNUM ( obj ) ].qic = NULL;


	return ( obj );
}


/* Generic Find, designed to find any object/character
 *
 * Calling:
 *  *arg     is the pointer containing the string to be searched for.
 *           This string doesn't have to be a single word, the routine
 *           extracts the next word itself.
 *  bitv..   All those bits that you want to "search through".
 *           Bit found will be result of the function
 *  *ch      This is the person that is trying to "find"
 *  **tar_ch Will be NULL if no character was found, otherwise points
 * **tar_obj Will be NULL if no object was found, otherwise points
 *
 * The routine used to return a pointer to the next word in *arg (just
 * like the one_argument routine), but now it returns an integer that
 * describes what it filled in.
 */
int generic_find ( char *arg, bitvector_t bitvector, Character *ch,
                   Character **tar_ch, struct obj_data **tar_obj )
{
	int i, found, number;
	char name_val[MAX_INPUT_LENGTH];
	char *name = name_val;

	*tar_ch = NULL;
	*tar_obj = NULL;

	one_argument ( arg, name );

	if ( !*name )
		return ( 0 );
	if ( ! ( number = get_number ( &name ) ) )
		return ( 0 );

	if ( IS_SET ( bitvector, FIND_CHAR_ROOM ) )    /* Find person in room */
	{
		if ( ( *tar_ch = get_char_room_vis ( ch, name, &number ) ) != NULL )
			return ( FIND_CHAR_ROOM );
	}

	if ( IS_SET ( bitvector, FIND_CHAR_WORLD ) )
	{
		if ( ( *tar_ch = get_char_world_vis ( ch, name, &number ) ) != NULL )
			return ( FIND_CHAR_WORLD );
	}

	if ( IS_SET ( bitvector, FIND_OBJ_EQUIP ) )
	{
		for ( found = FALSE, i = 0; i < NUM_WEARS && !found; i++ )
			if ( GET_EQ ( ch, i ) && isname ( name, GET_EQ ( ch, i )->name )
			        && --number == 0 )
			{
				*tar_obj = GET_EQ ( ch, i );
				found = TRUE;
			}
		if ( found )
			return ( FIND_OBJ_EQUIP );
	}

	if ( IS_SET ( bitvector, FIND_OBJ_INV ) )
	{
		if ( ( *tar_obj =
		            get_obj_in_list_vis ( ch, name, &number, ch->carrying ) ) != NULL )
			return ( FIND_OBJ_INV );
	}

	if ( IS_SET ( bitvector, FIND_OBJ_ROOM ) )
	{
		if ( ( *tar_obj =
		            get_obj_in_list_vis ( ch, name, &number,
		                                  IN_ROOM ( ch )->contents ) ) != NULL )
			return ( FIND_OBJ_ROOM );
	}

	if ( IS_SET ( bitvector, FIND_OBJ_WORLD ) )
	{
		if ( ( *tar_obj = get_obj_vis ( ch, name, &number ) ) )
			return ( FIND_OBJ_WORLD );
	}

	return ( 0 );
}


/* a function to scan for "all" or "all.x" */
int find_all_dots ( char *arg )
{
	char *ppos;
	int plen;
	if ( !arg || !*arg )
		return ( FIND_INDIV );

	if ( !strcmp ( arg, "all" ) )
		return ( FIND_ALL );
	else if ( !strncmp ( arg, "all.", 4 ) )
	{
		ppos = arg+4;
		memmove ( arg, ppos, ( plen = strlen ( ppos ) ) );
		* ( arg + plen ) = '\0';
		/*strcpy(arg, arg + 4);*/
		return ( FIND_ALLDOT );
	}
	else
		return ( FIND_INDIV );
}


// dismount_char() / fr: Daniel Koepke (dkoepke@california.com)
//   If a character is mounted on something, we dismount them.  If
//   someone is mounting our character, then we dismount that someone.
//   This is used for cleaning up after a mount is cancelled by
//   something (either intentionally or by death, etc.)
void dismount_char ( Character *ch )
{
	if ( RIDING ( ch ) )
	{
		RIDDEN_BY ( RIDING ( ch ) ) = NULL;
		RIDING ( ch ) = NULL;
	}

	if ( RIDDEN_BY ( ch ) )
	{
		RIDING ( RIDDEN_BY ( ch ) ) = NULL;
		RIDDEN_BY ( ch ) = NULL;
	}

	if ( SITTING ( ch ) )
		char_from_chair ( ch );

}


// mount_char() / fr: Daniel Koepke (dkoepke@california.com)
//   Sets _ch_ to mounting _mount_.  This does not make any checks
//   what-so-ever to see if the _mount_ is mountable, etc.  That is
//   left up to the calling function.  This does not present any
//   messages, either.
void mount_char ( Character *ch, Character *mount )
{
	RIDING ( ch ) = mount;
	RIDDEN_BY ( mount ) = ch;
}

/* Search the given list for an object type, and return a ptr to that obj */
struct obj_data *get_obj_in_list_type ( int type, struct obj_data *list )
{
	struct obj_data *i;

	for ( i = list; i; i = i->next_content )
		if ( GET_OBJ_TYPE ( i ) == type )
			return i;
	return NULL;
}

void add_hunter ( Character *ch )
{
	struct hunter_data *temp, *hunt, *nhunt;
	if ( !ch )
		return;
	for ( hunt = hunter_list; hunt; hunt = nhunt )
	{
		nhunt = hunt->next;
		if ( hunt == NULL )
			continue;
		if ( hunt->hunter == ch )
			return;
	}
	CREATE ( temp, struct hunter_data, 1 );
	temp->next = hunter_list;
	hunter_list = temp;
	temp->hunter = ch;
	HUNT_COUNT ( ch ) = MAX_HUNTSTEPS ( ch );
}

void remove_hunter ( Character *ch )
{
	struct hunter_data *temp, *hunt, *nhunt = NULL;
	if ( !ch )
		return;

	for ( hunt = hunter_list; hunt; hunt = nhunt )
	{
		nhunt = hunt->next;
		if ( hunt == NULL )
			continue;
		if ( hunt->hunter == ch )
		{
			HUNTING ( hunt->hunter ) = NULL;
			HUNT_COUNT ( hunt->hunter ) = 0;
			REMOVE_FROM_LIST ( hunt, hunter_list, next );
			free ( hunt );
			hunt = NULL;
			//return;
		}
	}
}

Character *check_ch ( Character *ch )
{
	register Character *tch;

	if ( !ch )
		return NULL;

	for ( tch = character_list; tch; tch = tch->next )
		if ( ch == tch )
			return tch;

	return NULL;
}

bool is_same_command(char *arg1, char *arg2)
{
  int length = 0;

  length = strlen(arg1);

  if (!strncmp(arg1, arg2, length))
      return TRUE;
  return FALSE;

}
