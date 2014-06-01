/**************************************************************************
*  File: dg_variables.c                                                   *
*  Usage: contains the functions dealing with variable substitution.      *
*                                                                         *
*                                                                         *
*  $Author: w4dimenscor $         		                          *
*  $Date: 2007/11/23 04:54:20 $                                           *
*  $Revision: 1.59 $                                                      *
**************************************************************************/

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "dg_scripts.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "clan.h"
#include "dg_event.h"
#include "db.h"
#include "screen.h"
#include "constants.h"
#include "spells.h"
#include "descriptor.h"
#include "strutil.h"
#include "assemblies.h"
#include "genmob.h"
#include "fight.h"

/* External variables and functions */

long long gold_data ( int type, long long amount );
int genpreg ( void );
extern const char *pc_class_types[];
extern struct time_info_data time_info;
void die ( Character *ch, Character *killer );
int togglebody ( Character *ch, int flag );
int has_body ( Character *ch, int flag );
int bodypartname ( char *bpn );
void zap_char ( Character *victim );
/* Utility functions */

/*
 * Thanks to James Long for his assistance in plugging the memory leak
 * that used to be here.   -- Welcor
 */
/* adds a variable with given name and value to trigger */
void add_var ( struct trig_var_data **var_list,const char *name,const char *value, long id )
{
	struct trig_var_data *vd;

	if ( strchr ( name, '.' ) )
	{
		log ( "add_var() : Attempt to add illegal var: %s", name );
		return;
	}

	for ( vd = *var_list; vd && strcasecmp ( vd->name.c_str(), name ); vd = vd->next )
		;

	if ( vd && ( !vd->context || vd->context==id ) )
	{
		vd->value.erase();
	}
	else
	{
		vd = new trig_var_data();
		vd->name.assign ( name );
		vd->next = *var_list;
		if ( id != 0 )
			vd->context = id;
		*var_list = vd;
	}
	vd->value.assign ( value );
}

void add_var ( struct trig_var_data **var_list, string &name, string value, long id )
{
	struct trig_var_data *vd;

	if ( strchr ( name.c_str(), '.' ) )
	{
		log ( "add_var() : Attempt to add illegal var: %s", name.c_str() );
		return;
	}

	for ( vd = *var_list; vd && strcasecmp ( vd->name.c_str(), name.c_str() ); vd = vd->next )
		;

	if ( vd && ( !vd->context || vd->context==id ) )
	{
		vd->value.erase();
	}
	else if ( !vd )
	{
		vd = new trig_var_data();
		vd->name.assign ( name );
		vd->next = *var_list;
		vd->context = id;
		*var_list = vd;
	}
	vd->value.assign ( value );
}


/* perhaps not the best place for this, but I didn't want a new file
char *skill_percent(Character *ch, char *skill)
{
  static char retval[16];
  int skillnum;

  skillnum = find_skill_num(skill);
  if (skillnum<=0) return("unknown skill");

  snprintf(retval, sizeof(retval), "%d", GET_SKILL(ch, skillnum));
  return retval;
}*/

/*
   search through all the persons items, including containers
   and 0 if it doesnt exist, and greater then 0 if it does!
   Jamie Nelson (mordecai@timespace.co.nz)
   MUD -- 4dimensions.org:6000
   Now also searches by vnum -- Welcor
   Now returns the number of matching objects -- Welcor 02/04
*/

int item_in_list ( char *item, obj_data *list )
{
	obj_data *i;
	int count = 0;

	if ( *item == UID_CHAR )
	{
		long id = atol ( item + 1 );

		for ( i = list; i; i = i->next_content )
		{
			if ( id == GET_ID ( i ) )
				count ++;
			if ( GET_OBJ_TYPE ( i ) == ITEM_CONTAINER )
				count += item_in_list ( item, i->contains );
		}
	}
	else if ( is_number ( item ) )   /* check for vnum */
	{
		obj_vnum ovnum = atoi ( item );

		for ( i = list; i; i = i->next_content )
		{
			if ( GET_OBJ_VNUM ( i ) == ovnum )
				count++;
			if ( GET_OBJ_TYPE ( i ) == ITEM_CONTAINER )
				count += item_in_list ( item, i->contains );
		}
	}
	else
	{
		for ( i = list; i; i = i->next_content )
		{
			if ( isname ( item, i->name ) )
				count++;
			if ( GET_OBJ_TYPE ( i ) == ITEM_CONTAINER )
				count += item_in_list ( item, i->contains );
		}
	}
	return count;
}

/*
   BOOLEAN return, just check if a player or mob
   has an item of any sort, searched for by name
   or id.
   searching equipment as well as inventory,
   and containers.
   Jamie Nelson (mordecai@timespace.co.nz)
   MUD -- 4dimensions.org:6000
*/

int char_has_item ( char *item, Character *ch )
{

	/* If this works, no more searching needed */
	if ( get_object_in_equip ( ch, item ) != NULL )
		return 1;

	if ( item_in_list ( item, ch->carrying ) == 0 )
		return 0;
	else
		return 1;
}

/**
Function that puts the word number 'n' from a string in to outstr, where first word is 1
- Mordecai from http://4Dimensions.org
**/
const char * wordat ( const char *str, int n )
{
	char *newlist;
	char *curtok;
	static char newlistbuf[MAX_INPUT_LENGTH];

	if ( !str || !*str || n <= 0 )
		return "";

	strlcpy ( newlistbuf, str, sizeof ( newlistbuf ) );
	newlist = newlistbuf;
	for ( curtok = strsep ( &newlist, WHITESPACE ); curtok && n; n--, curtok = strsep ( &newlist, WHITESPACE ) )
		if ( curtok && n == 1 )
		{
			return curtok;
		}
	return "";
}

int text_processed ( char *field, char *subfield, struct trig_var_data *vd,
                     char *str, size_t slen )
{

	if ( !str_cmp ( field, "strlen" ) )                  /* strlen    */
	{
		snprintf ( str, slen, "%d", ( int ) ( vd->value.length() ) );
		return TRUE;
	}
	else if ( !str_cmp ( field, "trim" ) )             /* trim      */
	{
#if 0
		/* trim whitespace from ends */
		snprintf ( tmpvar, sizeof ( tmpvar )-1 , "%s", vd->value ); /* -1 to use later*/
		p = tmpvar;
		p2 = tmpvar + strlen ( tmpvar ) - 1;
		while ( *p && isspace ( *p ) )
			p++;
		while ( ( p<=p2 ) && isspace ( *p2 ) )
			p2--;
		if ( p>p2 ) /* nothing left */
		{
			*str = '\0';
			return TRUE;
		}
		* ( ++p2 ) = '\0';                                      /* +1 ok (see above) */
#endif

		snprintf ( str, slen, "%s", Trim ( vd->value ).c_str() );
		return TRUE;
	}
	else if ( !str_cmp ( field, "contains" ) )         /* contains  */
	{
		if ( str_str ( ( char * ) vd->value.c_str(), subfield ) )
			strcpy ( str, "1" );
		else
			strcpy ( str, "0" );
		return TRUE;
	}
	else if ( !str_cmp ( field, "car" ) )              /* car       */
	{
		int ve = 0;
		*str = '\0';
		while ( ve < vd->value.length() && !isspace ( vd->value[ve] ) )
			*str++ = vd->value[ve++];
		*str = '\0';
		return TRUE;

	}
	else if ( !str_cmp ( field, "cdr" ) )              /* cdr       */
	{
		int cdr = 0;
		while ( cdr < vd->value.length() && !isspace ( vd->value[cdr] ) )
			cdr++; /* skip 1st field */
		while ( cdr < vd->value.length() && isspace ( vd->value[cdr] ) )
			cdr++;  /* skip to next */

		snprintf ( str, slen, "%s", vd->value.substr ( cdr, vd->value.length() ).c_str() );
		return TRUE;
	}
	else if ( !str_cmp ( field, "charat" ) )              /* charat       */
	{
		int c = atoi ( subfield );
		if ( c > 0 && c <= vd->value.length() )
			c--;
		snprintf ( str, slen, "%c", vd->value.at ( c ) );
		return TRUE;
	}
	else if ( !str_cmp ( field, "wordat" ) )              /* wordat       */
	{
		int c = atoi ( subfield );
		snprintf ( str, slen, "%s", wordat ( vd->value.c_str(), c ) );
		return TRUE;
	}
	else if ( !str_cmp ( field, "mudcommand" ) )
	{

		int length, cmd;
		for ( length = vd->value.length(), cmd = 0;
		        *complete_cmd_info[cmd].command != '\n'; cmd++ )
			if ( !strncmp ( complete_cmd_info[cmd].command, vd->value.c_str(), length ) )
				break;

		if ( *complete_cmd_info[cmd].command == '\n' )
			*str = '\0';
		else
			snprintf ( str, slen, "%s", complete_cmd_info[cmd].command );
		return TRUE;
	}
	else if ( !str_cmp ( field, "abbrev" ) )              /* abbrev       */
	{
		if ( is_abbrev ( vd->value.c_str(),subfield ) )
			strcpy ( str, "1" );
		else
			strcpy ( str, "0" );
		return TRUE;
	}

	return FALSE;
}


void dg_varexists (struct script_data *script, char *subfield, char *str, size_t slen) {
  struct trig_var_data *vdt;
  snprintf ( str, slen, "0" );
  if ( script )
    {
      for ( vdt = script->global_vars; vdt; vdt = vdt->next )
	{
	  if ( !strcasecmp ( vdt->name.c_str(), subfield ) )
	    break;
	}
      if ( vdt )
	snprintf ( str, slen, "1" );
      else
	snprintf ( str, slen, "0" );
    }
}


void find_replacement ( void *go, struct script_data *sc, trig_data * trig,
                        int type, char *var, char *field, char *subfield, char *str, size_t slen )
{
	struct trig_var_data *vd = NULL;
	Character *ch = NULL, *c = NULL, *rndm;
	obj_data *obj, *o = NULL;
	Room *room, *r = NULL;
	const char *name;
	int num, count;

	const char *send_cmd[] = { "msend ", "osend ", "wsend " };
	const char *echo_cmd[] = { "mecho ", "oecho ", "wecho " };
	const char *echoaround_cmd[] = { "mechoaround ", "oechoaround ", "wechoaround " };
	const char *door[] = { "mdoor ", "odoor ", "wdoor " };
	const char *force[] = { "mforce ", "oforce ", "wforce " };
	const char *load[] = { "mload ", "oload ", "wload " };
	const char *purge[] = { "mpurge ", "opurge ", "wpurge " };
	const char *teleport[] = { "mteleport ", "oteleport ", "wteleport " };
	const char *xdamage[] = { "mdamage ", "odamage ", "wdamage " };
	/* the x kills a 'shadow' warning in gcc. */
	const char *zecho_cmd[] = { "mzecho ", "ozecho ", "wzecho " };
	const char *zrecho_cmd[] = { "mzrecho ", "ozrecho ", "wzrecho " };
	const char *asound[] = { "masound ", "oasound ", "wasound " };
	const char *at[] = { "mat ", "oat ", "wat " };
	/* there is no such thing as wtransform, thus the wecho below  */
	const char *transform[]      = {"mtransform ",  "otransform ",  "wecho"       };
	const char *lag[]      = {"mlag ",  "olag ",  "wlag"       };


	/* X.global() will have a NULL trig */
	if ( trig )
		for ( vd = GET_TRIG_VARS ( trig ); vd; vd = vd->next )
			if ( !strcasecmp ( vd->name.c_str(), var ) )
				break;

	/* some evil waitstates could crash the mud if sent here with sc==NULL*/
	if ( !vd && sc )
		for ( vd = sc->global_vars; vd; vd = vd->next )
			if ( !strcasecmp ( vd->name.c_str(), var ) && ( vd->context==0 || vd->context==sc->context ) )
				break;


	if ( !*field )
	{
		if ( vd )
			snprintf ( str, slen, "%s", vd->value.c_str() );
		else
		{
			if ( !strcasecmp ( var, "realday" ) )
			{
				snprintf ( str, slen, "%d", SECS_PER_REAL_DAY );
			}
			else if ( !strcasecmp ( var, "realhour" ) )
			{
				snprintf ( str, slen, "%d", SECS_PER_REAL_HOUR );
			}
			else if ( !strcasecmp ( var, "now" ) )
			{
				snprintf ( str, slen, "%ld", time ( 0 ) );
			}
			else if ( !strcasecmp ( var, "self" ) )
			{
				switch ( type )
				{
					case MOB_TRIGGER:
						snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( ( Character * ) go ) );
						break;
					case OBJ_TRIGGER:
						snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( ( obj_data * ) go ) );
						break;
					case WLD_TRIGGER:
						snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) ( ( Room * ) go )->number + ROOM_ID_BASE );
						break;
				}
				//        snprintf(str, slen, "self");
			}
			else if ( !strcasecmp ( var, "door" ) )
				snprintf ( str, slen, "%s", door[type] );
			else if ( !strcasecmp ( var, "force" ) )
				snprintf ( str, slen, "%s", force[type] );
			else if ( !strcasecmp ( var, "load" ) )
				snprintf ( str, slen, "%s", load[type] );
			else if ( !strcasecmp ( var, "purge" ) )
				snprintf ( str, slen, "%s", purge[type] );
			else if ( !strcasecmp ( var, "teleport" ) )
				snprintf ( str, slen, "%s", teleport[type] );
			else if ( !strcasecmp ( var, "damage" ) )
				snprintf ( str, slen, "%s", xdamage[type] );
			else if ( !strcasecmp ( var, "send" ) )
				snprintf ( str, slen, "%s", send_cmd[type] );
			else if ( !strcasecmp ( var, "echo" ) )
				snprintf ( str, slen, "%s", echo_cmd[type] );
			else if ( !strcasecmp ( var, "echoaround" ) )
				snprintf ( str, slen, "%s", echoaround_cmd[type] );
			else if ( !strcasecmp ( var, "zecho" ) )
				snprintf ( str, slen, "%s", zecho_cmd[type] );
			else if ( !strcasecmp ( var, "zrecho" ) )
				snprintf ( str, slen, "%s", zrecho_cmd[type] );
			else if ( !strcasecmp ( var, "asound" ) )
				snprintf ( str, slen, "%s", asound[type] );
			else if ( !strcasecmp ( var, "at" ) )
				snprintf ( str, slen, "%s", at[type] );
			else if ( !strcasecmp ( var, "transform" ) )
				snprintf ( str, slen, "%s", transform[type] );
			else if ( !strcasecmp ( var, "lag" ) )
				snprintf ( str, slen, "%s", lag[type] );
			else
				*str = '\0';
		}

		return;
	}
	else
	{

		if ( vd )
		{
			name = vd->value.c_str();

			switch ( type )
			{
				case MOB_TRIGGER:
					ch = ( Character * ) go;

					if ( ( o = get_object_in_equip ( ch, name ) ) )
						;
					else if ( ch->carrying && ( o = get_obj_in_list ( name, ch->carrying ) ) )
						;
					else if ( IN_ROOM ( ch ) && ( c = get_char_room ( name, NULL, IN_ROOM ( ch ) ) ) )
						;
					else if ( IN_ROOM ( ch )->contents && ( o = get_obj_in_list ( name, IN_ROOM ( ch )->contents ) ) )
						;
					else if ( ( c = get_char ( name ) ) )
						;
					else if ( ( o = get_obj ( name ) ) )
						;
					else if ( ( r = get_room ( name ) ) ) {}

					break;
				case OBJ_TRIGGER:
					obj = ( obj_data * ) go;


					if ( ( c = get_char_by_obj ( obj, name ) ) )
						;
					else if ( ( o = get_obj_by_obj ( obj, name ) ) )
						;
					else if ( ( r = get_room ( name ) ) ) {}

					break;
				case WLD_TRIGGER:
					room = ( Room * ) go;


					if ( ( c = get_char_by_room ( room, name ) ) )
						;
					else if ( ( o = get_obj_by_room ( room, name ) ) )
						;
					else if ( ( r = get_room ( name ) ) ) {}

					break;
			}
		}
		else
		{

			if ( !strcasecmp ( var, "self" ) )
			{
				switch ( type )
				{
					case MOB_TRIGGER:
						c = ( Character * ) go;
						r = NULL;
						o = NULL;	/* NULL assignments added to avoid self to always be    */
						break;	/* the room.  - Welcor        */
					case OBJ_TRIGGER:
						o = ( obj_data * ) go;
						c = NULL;
						r = NULL;
						break;
					case WLD_TRIGGER:
						r = ( Room * ) go;
						c = NULL;
						o = NULL;
						break;
				}
			}


			/**

			  %findobj.<room vnum X>(<object vnum/id/name>)%
			    - count number of objects in room X with this name/id/vnum
			  %findmob.<room vnum X>(<mob vnum Y>)%
			    - count number of mobs in room X with vnum Y

			for example you want to check how many PC's are in room with vnum 1204.
			as PC's have the vnum -1...
			you would type:
			in any script:
			%echo% players in room 1204: %findmob.1204(-1)%

			Or say you had a bank, and you want a script to check the number of
			bags
			of gold (vnum: 1234)
			in the vault (vnum: 453) now and then. you can just use
			%findobj.453(1234)% and it will return the number of bags of gold.

			**/

			/* addition inspired by Jamie Nelson - mordecai@xtra.co.nz */
			else if ( !strcasecmp ( var, "findmob" ) )
			{
				if ( !field || !*field || !subfield || !*subfield )
				{
					script_log ( "findmob.vnum(mvnum) - illegal syntax" );
					strcpy ( str, "0" );
				}
				else
				{
					room_rnum rrnum = real_room ( atoi ( field ) );
					mob_vnum mvnum = atoi ( subfield );

					if ( rrnum == NULL )
					{
						script_log ( "findmob.vnum(ovnum): No room with vnum %d", atoi ( field ) );
						strcpy ( str, "0" );
					}
					else
					{
						int i = 0;
						Character *tch;
						for ( tch = rrnum->people; tch; tch = tch->next_in_room )
							if ( GET_MOB_VNUM ( tch ) == mvnum )
								i++;

						snprintf ( str, slen, "%d", i );
					}
				}
			}
			/* addition inspired by Jamie Nelson - mordecai@xtra.co.nz  7426mob 7409room*/
			else if ( !strcasecmp ( var, "findobj" ) )
			{
				if ( !field || !*field || !subfield || !*subfield )
				{
					script_log ( "findobj.vnum(ovnum) - illegal syntax" );
					strcpy ( str, "0" );
				}
				else
				{
					room_rnum rrnum = real_room ( atoi ( field ) );

					if ( rrnum == NULL )
					{
						script_log ( "findobj.vnum(ovnum): No room with vnum %d", atoi ( field ) );
						strcpy ( str, "0" );
					}
					else
					{
						/* item_in_list looks within containers as well. */
						snprintf ( str, slen, "%d", item_in_list ( subfield, rrnum->contents ) );
					}
				}
			}
			else if ( !strcasecmp ( var, "people" ) )
			{
				snprintf ( str, slen, "%d", ( ( num = atoi ( field ) ) > 0 ) ? trgvar_in_room ( num ) : 0 );
				return;
			}
			else if ( !strcasecmp ( var, "time" ) )
			{
				if ( !strcasecmp ( field, "hour" ) )
					snprintf ( str, slen, "%d", time_info.hours );
				else if ( !strcasecmp ( field, "day" ) )
					snprintf ( str, slen, "%d", time_info.day + 1 );
				else if ( !strcasecmp ( field, "month" ) )
					snprintf ( str, slen, "%d", time_info.month + 1 );
				else if ( !strcasecmp ( field, "year" ) )
					snprintf ( str, slen, "%d", time_info.year );
				else if ( !strcasecmp ( field, "season" ) ) {
					if (time_info.month >= 0 && time_info.month < 4) {
						snprintf ( str, slen, "winter");
					}
					else if (time_info.month >= 4 && time_info.month < 8) {
						snprintf ( str, slen, "spring");
					}
					else if (time_info.month >= 8 && time_info.month < 12) {
						snprintf ( str, slen, "summer");
					}
					else if (time_info.month >= 12 && time_info.month <= 16) {
						snprintf ( str, slen, "fall");
					}
				}
				else if ( !strcasecmp ( field, "moon" ) ) {
					if (time_info.moon == MOON_FULL_MOON){
						snprintf ( str, slen, "full" );
					}
					else if (time_info.moon == MOON_WANING_GIBBOUS){
						snprintf ( str, slen, "waning gibbous");
					}
					else if (time_info.moon == MOON_LAST_QUARTER){
						snprintf ( str, slen, "last quarter");
					}
					else if (time_info.moon == MOON_WANING_CRESCENT){
                                                snprintf ( str, slen, "waning crescent");
					}
					else if (time_info.moon == MOON_NEW_MOON){
                                                snprintf ( str, slen, "new");
					}	
					else if (time_info.moon == MOON_WAXING_CRESCENT){
                                                snprintf ( str, slen, "waxing crescent");
                                        }
					else if (time_info.moon == MOON_FIRST_QUARTER){
                                                snprintf ( str, slen, "first quarter");
                                        }
					else if (time_info.moon == MOON_WAXING_GIBBOUS){
                                                snprintf ( str, slen, "waxing gibbous");
                                        }
				}
				else
					*str = '\0';
				return;
			}
			else if ( !strcasecmp ( var, "random" ) )
			{
				if ( !strcasecmp ( field, "char" ) )
				{
					rndm = NULL;
					count = 0;

					if ( type == MOB_TRIGGER )
					{
						ch = ( Character * ) go;
						for ( c = IN_ROOM ( ch )->people; c;
						        c = c->next_in_room )
							if ( ( c != ch ) && valid_dg_target ( c, TRUE )
							        && CAN_SEE ( ch, c ) )
							{
								if ( !number ( 0, count ) )
									rndm = c;
								count++;
							}
					}
					else if ( type == OBJ_TRIGGER )
					{
						room_rnum trm = obj_room ( ( obj_data * ) go );
						for ( c = trm->people; c; c = c->next_in_room )
							if ( valid_dg_target ( c, TRUE ) )
							{
								if ( !number ( 0, count ) )
									rndm = c;
								count++;
							}
					}
					else if ( type == WLD_TRIGGER )
					{
						for ( c = ( ( Room * ) go )->people; c; c = c->next_in_room )
							if ( valid_dg_target ( c, TRUE ) )
							{
								if ( !number ( 0, count ) )
									rndm = c;
								count++;
							}
					}

					if ( rndm )
						snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( rndm ) );
					else
						*str = '\0';
				}
				else if ( !strcasecmp ( field, "dir" ) )
				{
					room_rnum in_room = NULL;

					switch ( type )
					{
						case WLD_TRIGGER:
							in_room = ( ( Room * ) go );
							break;
						case OBJ_TRIGGER:
							in_room = obj_room ( ( obj_data * ) go );
							break;
						case MOB_TRIGGER:
							in_room = IN_ROOM ( ( Character * ) go );
							break;
					}
					if ( in_room == NULL )
					{
						*str = '\0';
					}
					else
					{
						int doors = 0;
						for ( int i = 0; i < NUM_OF_DIRS ; i++ )
							if ( R_EXIT ( in_room, i ) )
								doors++;

						if ( !doors )
						{
							*str = '\0';
						}
						else
						{
							for ( ; ; )
							{
								doors = number ( 0, NUM_OF_DIRS-1 );
								if ( R_EXIT ( in_room, doors ) )
									break;
							}
							snprintf ( str, slen, "%s", dirs[doors] );
						}
					}
				}
				else
					snprintf ( str, slen, "%d", ( ( num = atoi ( field ) ) > 0 ) ? number ( 1, num ) : 0 );

				return;
			}
			else if ( !strcasecmp ( var, "assembly" ) )
			{
				if ( !strcasecmp ( field, "exists" ) )
				{
					if ( subfield && *subfield )
					{
						int lVnum;
						if ( ( lVnum = assemblyFindAssembly ( subfield ) ) < 0 )
							snprintf ( str, slen, "0" );
						else
							snprintf ( str, slen, "1" );
					}
					else
					{
						script_log ( "Trigger: %s, VNum %d. assemble.exists called without a subfield.",GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ) );
						snprintf ( str, slen, "0" );
					}
				}
				else
				{
					if ( subfield && *subfield )
					{
						int lVnum;
						if ( ( lVnum = assemblyFindAssembly ( subfield ) ) < 0 )
							snprintf ( str, slen, "0" );
						else
						{
							if ( assemblyGetType ( lVnum ) != ( assemblyGetTypeByName ( field ) - 101 ) )
							{
								snprintf ( str, slen, "0" );
							}
							else
							{
								snprintf ( str, slen, "1" );
							}
						}
					}
				}
			}
		}

		if ( c )
		{
			if ( DEAD ( c ) )
				return; //dead - please don't screw anything up!
			else if ( text_processed ( field, subfield, vd, str, slen ) )
				return;



			else if ( !strcasecmp ( field, "global" ) )  	/* get global of something else */
			{
				if ( IS_NPC ( c ) && c->script )
				{
					find_replacement ( go, c->script, NULL, MOB_TRIGGER,
					                   subfield, NULL, NULL, str, slen );
				}
			}
			/* set str to some 'non-text' first */
			*str = '\x1';
			* ( str + 1 ) = '\0';

			switch ( LOWER ( *field ) )
			{
				case 'a':
					if ( !strcasecmp ( field, "alias" ) )
						snprintf ( str, slen, "%s", GET_PC_NAME ( c ) );
					else if ( !strcasecmp ( field, "align" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							GET_ALIGNMENT ( c ) = IRANGE ( -1000, addition, 1000 );
						}
						snprintf ( str, slen, "%d", GET_ALIGNMENT ( c ) );
					}
					else if ( !strcasecmp ( field, "affect" ) )
					{
						if ( subfield && *subfield )
						{
							int spell = find_skill_num ( subfield );
							if ( affected_by_spell ( c, spell ) )
								snprintf ( str, slen, "1" );
							else
								snprintf ( str, slen, "0" );
						}
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "assemblycheck" ) )
					{
						if ( subfield && *subfield )
						{
							int lVnum;
							if ( ( lVnum = assemblyFindAssembly ( subfield ) ) < 0 )
							{
								snprintf ( str, slen, "0" );
							}
							else if ( !assemblyCheckComponents ( lVnum, c, TRUE ) )
							{
								snprintf ( str, slen, "0" );
							}
							else
								snprintf ( str, slen, "1" );
						}
					}
					else if ( !strcasecmp ( field, "assemble" ) )
					{
						if ( subfield && *subfield )
						{
							int lVnum;
							int percent;
							struct obj_data *pObject = NULL;
							if ( ( lVnum = assemblyFindAssembly ( subfield ) ) < 0 )
							{
								snprintf ( str, slen, "0" );
							}
							else if ( !assemblyCheckComponents ( lVnum, c, FALSE ) )
							{
								snprintf ( str, slen, "0" );
							}
							else if ( ( pObject = read_object ( lVnum, VIRTUAL ) ) == NULL )
							{
								snprintf ( str, slen, "0" );
								script_log ( "Trigger: %s, VNum %d. assemble vnum '%d' for '%s' doesnt exist.",GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), lVnum, subfield );
								snprintf ( str, slen, "0" );
							}
							else
							{
								percent = number ( 1, 101 );

								if ( percent < 5 )
									improve_sub ( c, ( enum subskill_list ) assemblyGetType ( lVnum ),1 );

								/* Now give the object to the character. */
								obj_to_char ( pObject, c );
								snprintf ( str, slen, "%c%ld", UID_CHAR,                                     GET_ID ( pObject ) );
							}
						}
					}
					break;
				case 'b':
					if ( !strcasecmp ( field, "bodycheck" ) )
					{
						if ( subfield && *subfield )
						{
							int bpn = bodypartname ( subfield );
							if ( bpn != -1 )
								snprintf ( str, slen, "%d", has_body ( c, bpn ) );
							else
								snprintf ( str, slen, "0" );
						}
					}
					else if ( !strcasecmp ( field, "body" ) )
					{
						if ( subfield && *subfield )
						{
							int bpn = bodypartname ( subfield );
							if ( bpn != -1 )
								snprintf ( str, slen, "%d", togglebody ( c, bpn ) );
							else
								snprintf ( str, slen, "0" );
						}
						else
						{
							snprintf ( str, slen, "0" ); /*FIXME: Change this to be a list of the body parts */
						}
					}
					break;
				case 'c':
				        if ( !strcasecmp ( field, "carriedweight" ))
					  snprintf ( str, slen, "%u", IS_CARRYING_W(c));

					else if ( !strcasecmp ( field, "carriedamount" ))
					  snprintf ( str, slen, "%u", IS_CARRYING_N(c));
					 
				    
					else if ( !strcasecmp ( field, "class" ) )
						snprintf ( str, slen, "%s", simple_class_name ( c ) );

					else if ( !strcasecmp ( field, "canbeseen" ) )
					{
						if ( ( type == MOB_TRIGGER )
						        && !CAN_SEE ( ( ( Character * ) go ), c ) )
							snprintf ( str, slen, "0" );
						else
							snprintf ( str, slen,"1" );
					}
					else if ( !strcasecmp ( field, "class_tier" ) )
						snprintf ( str, slen, "%d", current_class_is_tier_num ( c ) );

					else if ( !strcasecmp ( field, "clan" ) ) 
                                        {
                                                if ( subfield && *subfield ) 
                                                {
                                                   GET_CLAN ( c ) = atoi ( subfield );
                                                }   
						snprintf ( str, slen, "%d", IS_NPC ( c ) ? -1 : GET_CLAN ( c ) );
                                        }
					else if ( !strcasecmp ( field, "clanrank" ) )
                                        {
                                                 if ( subfield && *subfield )
                                                 {
                                                   GET_CLAN_RANK ( c ) = atoi ( subfield );
                                                 }
						snprintf ( str, slen, "%d", IS_NPC ( c ) ? -1 : GET_CLAN_RANK ( c ) );
                                        }
					else if ( !strcasecmp ( field, "clanname" ) )
						snprintf ( str, slen, "%s", ( ( IS_NPC ( c ) || !GET_CLAN ( c ) ) ? "" : clan[find_clan_by_id ( GET_CLAN ( c ) ) ].name ) );

					else if ( !strcasecmp ( field, "con" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE;
							GET_CON ( c ) += addition;
							if ( GET_CON ( c ) > max )
								GET_CON ( c ) = max;
							if ( GET_CON ( c ) < 3 )
								GET_CON ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_CON ( c ) );
					}
					else if ( !strcasecmp ( field, "cha" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE;
							GET_CHA ( c ) += addition;
							if ( GET_CHA ( c ) > max )
								GET_CHA ( c ) = max;
							if ( GET_CHA ( c ) < 3 )
								GET_CHA ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_CHA ( c ) );
					}
					else if ( !strcasecmp ( field, "cool" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE;
							GET_COOLNESS ( c ) += addition;
							if ( GET_COOLNESS ( c ) > max )
								GET_COOLNESS ( c ) = max;
							if ( GET_COOLNESS ( c ) < 3 )
								GET_COOLNESS ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_COOLNESS ( c ) );
					}


					break;
				case 'd':

					if ( !strcasecmp ( field, "dex" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE;
							GET_DEX ( c ) += addition;
							if ( GET_DEX ( c ) > max )
								GET_DEX ( c ) = max;
							if ( GET_DEX ( c ) < 3 )
								GET_DEX ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_DEX ( c ) );
					}
					else if ( !strcasecmp ( field, "dest" ) )
					{
						struct travel_point_data *t;
						room_vnum dest = NOWHERE;
						for ( t = TRAVEL_LIST ( c ); t; t = t->next )
							if ( t->last_stop == TRUE )
							{
								dest = t->dest;
								break;
							}
						if ( subfield && *subfield )
						{
							dest = atoi ( subfield );
							if ( t && real_room ( dest ) != NULL )
								t->dest = dest;
						}

						snprintf ( str, slen, "%d", dest );
					}
					else if ( !strcasecmp ( field, "damroll" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = IS_NPC ( ch ) ? MAX_MOB_DAMROLL : MAX_PLAYER_DAMROLL;
							GET_DAMROLL ( c ) += addition;
							if ( GET_DAMROLL ( c ) > max )
								GET_DAMROLL ( c ) = max;
							if ( GET_DAMROLL ( c ) < 3 )
								GET_DAMROLL ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_DAMROLL ( c ) );
					}

					break;
				case 'e':

					if ( !strcasecmp ( field, "exp" ) )
					{
						if ( subfield && *subfield )
						{
							log ( "INFO: %s just gained %d exp from script vnum %d.", GET_NAME ( c ),atoi ( subfield ), GET_TRIG_VNUM ( trig ) );
							gold_int addition = MIN ( atoi ( subfield ), 100000000 );	/* level_exp(GET_CLASS(c), GET_LEVEL(c) + 1, current_class_is_tier_num(c)) - GET_EXP(c) -1); //removed by mord */
							gain_exp ( c, addition );
						}
						snprintf ( str, slen, "%lld", GET_EXP ( c ) );
					}
					else if ( !strcasecmp ( field, "exp_needed" ) )
					{
						snprintf ( str, slen, "%lld", exp_needed ( c ) );
					}
					else if ( !strcasecmp ( field, "eq" ) )
					{
						int pos;
						if ( !subfield || !*subfield )
							strcpy ( str,"" );
						else if ( *subfield == '*' )
						{
							int i, j = 0;
							for ( i = 0; i < NUM_WEARS; i++ )
								if ( GET_EQ ( c, i ) )
								{
									j++;
									break;
								}
							if ( j > 0 )
								strcpy ( str,"1" );
							else
								strcpy ( str,"" );
						}
						else if ( ( pos = find_eq_pos_script ( subfield ) ) < 0 || !GET_EQ ( c, pos ) )
							strcpy ( str, "" );
						else
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( GET_EQ ( c, pos ) ) );
					}


					break;
				case 'f':

					if ( !strcasecmp ( field, "follower" ) )
					{
						if ( !c->followers || !c->followers->follower )
							*str = '\0';
						else
						{
							struct follow_type *k;
							snprintf ( str, slen, "0" );
							if ( ! ( !subfield || !*subfield ) )
							{
								mob_vnum mvn = atoi ( subfield );
								for ( k = c->followers; k; k = k->next )
								{
									if ( mvn == GET_MOB_VNUM ( k->follower ) )
										break;
									else if ( !strcasecmp ( GET_NAME ( k->follower ), subfield ) )
										break;
								}
								if ( k )
									snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( k->follower ) );
							}
							else
								snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( c->followers->follower ) );
						}
					}
					else if ( !strcasecmp ( field, "follower_in_room" ) )
					{
						struct follow_type *k;
						snprintf ( str, slen, "0" );
						if ( c->followers )
						{
							mob_vnum mvn = atoi ( subfield );
							for ( k = c->followers; k; k = k->next )
							{
								if ( ( !strcasecmp ( GET_NAME ( k->follower ), subfield ) || ( mvn == GET_MOB_VNUM ( k->follower ) ) ) )
									if ( IN_ROOM ( c ) == IN_ROOM ( k->follower ) )
										break;
							}
							if ( k )
								snprintf ( str, slen, "1" );
						}
					}
					else if ( !strcasecmp ( field, "fighting" ) )
					{
						if ( FIGHTING ( c ) )
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( FIGHTING ( c ) ) );
						else
							*str = '\0';
					}

					break;
				case 'g':

					if ( !strcasecmp ( field, "gold" ) )
					{
						if ( subfield && *subfield )
						{
							gold_int addition = atoi ( subfield );
							c->Gold ( addition, GOLD_HAND );
							if ( addition > 0 )
								gold_data ( DG_GOLD_OUT, addition );
							else
								gold_data ( DG_GOLD_IN, -addition );
						}
						snprintf ( str, slen, "%lld", c->Gold ( 0, GOLD_HAND ) );
					}

					break;
				case 'h':
					if ( !strcasecmp ( field, "hisher" ) )
						snprintf ( str, slen, "%s", HSHR ( c ) );

					else if ( !strcasecmp ( field, "heshe" ) )
						snprintf ( str, slen, "%s", HSSH ( c ) );

					else if ( !strcasecmp ( field, "hunt" ) )
					{
						if ( hunt_location ( GET_ID ( c ), STRUCT_IS_MOB ) )
							snprintf ( str, slen, "1" );
						else if ( HUNTING ( c ) )
						{
							hunt_victim ( c );
							snprintf ( str, slen, "1" );
						}
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "himher" ) )
						snprintf ( str, slen, "%s", HMHR ( c ) );

					else if ( !strcasecmp ( field, "hitp" ) )
					{
						if ( subfield && *subfield )
						{
							int newhit = atoi ( subfield );
							int oldhit = GET_HIT ( c );
							GET_HIT ( c ) = newhit;
							alter_hit ( c,0 );
							update_pos ( c );
							c->send_char_pos ( oldhit-newhit );
							if ( GET_POS ( c ) ==POS_DEAD )
								die ( c,NULL );
						}
						snprintf ( str, slen, "%d", GET_HIT ( c ) );
					}
					else if ( !strcasecmp ( field, "has_item" ) )
					{
						if ( ! ( subfield && *subfield ) )
							*str = '\0';
						else
							snprintf ( str, slen, "%d", char_has_item ( subfield, c ) );
					}
					else if ( !strcasecmp ( field, "hitroll" ) )
						snprintf ( str, slen, "%d", GET_HITROLL ( c ) );
					else if ( !strcasecmp ( field, "hunting" ) )
					{
						if ( HUNTING ( c ) )
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( HUNTING ( c ) ) );
						else
							*str = '\0';
					}
					else if ( !strcasecmp (field, "hunger" ) )
					  {
					    if ( subfield && *subfield ) {
					      GET_COND(c, FULL) = (char) atoi(subfield);
					    }
					    snprintf ( str, slen, "%d", GET_COND(c, FULL));
					  }

					      


				       
				case 'i':

					if ( !strcasecmp ( field, "inventory" ) )
					{
						if ( subfield && *subfield )
						{
							for ( obj = c->carrying; obj; obj = obj->next_content )
							{
								if ( GET_OBJ_VNUM ( obj ) == atoi ( subfield ) )
								{
									snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( obj ) );	/* arg given, found */
									return;
								}
								else if ( isname ( subfield, obj->short_description ) )
								{
									snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( obj ) );	/* arg given, found */
									return;
								}
							}
							if ( !obj )
								strcpy ( str, "" );	/* arg given, not found */
						}
						else  	/* no arg given */
						{
							if ( c->carrying )
							{
								snprintf ( str, slen, "%c%ld", UID_CHAR,
								           GET_ID ( c->carrying ) );
							}
							else
							{
								strcpy ( str, "" );
							}
						}
					}
					else if ( !strcasecmp ( field, "is_pc" ) )
						snprintf ( str, slen, "%d", !IS_NPC ( c ) );
					else if ( !strcasecmp ( field, "is_pk" ) )
						snprintf ( str, slen, "%d", IS_PK ( c ) );

					else if ( !strcasecmp ( field, "is_npc" ) )
						snprintf ( str, slen, "%d", IS_NPC ( c ) );
					else if ( !strcasecmp ( field, "is_hero" ) )
						snprintf ( str, slen, "%d", PLR_FLAGGED ( c, PLR_HERO ) );
					else if ( !strcasecmp ( field, "is_roleplay" ) )
						snprintf ( str, slen, "%d", PLR_FLAGGED ( c, PLR_ROLEPLAYER ) );
					else if ( !strcasecmp ( field, "is_rpl" ) )
						snprintf ( str, slen, "%d", PLR_FLAGGED ( c, PLR_RP_LEADER ) );
					else if ( !strcasecmp ( field, "id" ) )
						snprintf ( str, slen, "%ld", GET_ID ( c ) );
					else if ( !strcasecmp ( field, "int" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE;
							GET_INT ( c ) += addition;
							if ( GET_INT ( c ) > max )
								GET_INT ( c ) = max;
							if ( GET_INT ( c ) < 3 )
								GET_INT ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_INT ( c ) );
					}
					else if ( !strcasecmp ( field, "is_flying" ) )
					{
						if ( c->Flying() )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "is_helper" ) )
					{
						if ( subfield && *subfield )
						{
							if ( !strcasecmp ( "on", subfield ) )
								SET_BIT_AR ( PLR_FLAGS ( c ), PLR_NEWBIE_HLPR );
							else if ( !strcasecmp ( "off", subfield ) )
								REMOVE_BIT_AR ( PLR_FLAGS ( c ), PLR_NEWBIE_HLPR );
						}
						if ( PLR_FLAGGED ( c, PLR_NEWBIE_HLPR ) )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "is_killer" ) )
					{
						if ( subfield && *subfield )
						{
							if ( !strcasecmp ( "on", subfield ) )
								SET_BIT_AR ( PLR_FLAGS ( c ), PLR_KILLER );
							else if ( !strcasecmp ( "off", subfield ) )
								REMOVE_BIT_AR ( PLR_FLAGS ( c ), PLR_KILLER );
						}
						if ( PLR_FLAGGED ( c, PLR_KILLER ) )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "is_cheater" ) )
					{
						if ( subfield && *subfield )
						{
							if ( !strcasecmp ( "on", subfield ) )
								SET_BIT_AR ( PLR_FLAGS ( c ), PLR_CHEATER );
							else if ( !strcasecmp ( "off", subfield ) )
								REMOVE_BIT_AR ( PLR_FLAGS ( c ), PLR_CHEATER );
						}
						if ( PLR_FLAGGED ( c, PLR_CHEATER ) )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "is_thief" ) )
					{
						if ( subfield && *subfield )
						{
							if ( !strcasecmp ( "on", subfield ) )
								SET_BIT_AR ( PLR_FLAGS ( c ), PLR_THIEF );
							else if ( !strcasecmp ( "off", subfield ) )
								REMOVE_BIT_AR ( PLR_FLAGS ( c ), PLR_THIEF );
						}
						if ( PLR_FLAGGED ( c, PLR_THIEF ) )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					// ==============
					else if ( !strcasecmp ( field, "is_pregnant" ) )
					{
						if ( subfield && *subfield )
						{
							if ( !strcasecmp ( "off", subfield ) )
							{
								if ( PREG ( c ) > 0 && GET_SEX ( c ) != SEX_MALE )
									PREG ( c ) = NOT_PREG;
							}
							else if ( !strcasecmp ( "on", subfield ) )
							{
								if ( PREG ( c ) < 0 && GET_SEX ( c ) != SEX_MALE )
									PREG ( c ) = genpreg();
							}
						}
						if ( PREG ( c ) > 0 )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					/*
						break;
					     	case 'j':
						break;
					     	case 'k':
					*/

					break;
				case 'l':

					if ( !strcasecmp ( field, "level" ) )
						snprintf ( str, slen, "%d", GET_LEVEL ( c ) );
					else if ( !strcasecmp ( field, "longdesc" ) )
						snprintf ( str, slen, "%s", GET_LDESC ( c ) );
					break;
			        case 'm':
					if ( !strcasecmp ( field, "maxcarriedweight" ))
					  snprintf ( str, slen, "%u", CAN_CARRY_W(c));
				
					else if ( !strcasecmp ( field, "maxcarriedamount" ))
					  snprintf ( str, slen, "%u", CAN_CARRY_N(c));

					else if ( !strcasecmp ( field, "maxhitp" ) )
						snprintf ( str, slen, "%d", GET_MAX_HIT ( c ) );
					else if ( !strcasecmp ( field, "mid" ) )
						snprintf ( str, slen, "%ld", GET_ID ( MASTER ( c ) ) );
					else if ( !strcasecmp ( field, "maxmana" ) )
						snprintf ( str, slen, "%d", GET_MAX_MANA ( c ) );
					else if ( !strcasecmp ( field, "maxmove" ) )
						snprintf ( str, slen, "%d", GET_MAX_MOVE ( c ) );
					else if ( !strcasecmp ( field, "maxstamina" ) )
						snprintf ( str, slen, "%d", GET_MAX_STAMINA ( c ) );
					else if ( !strcasecmp ( field, "mana" ) )
					{
						if ( subfield && *subfield )
						{
							int newmana = atoi ( subfield );
							if ( newmana >= 0 )
								GET_MANA ( c ) = newmana;
							else
							{
								GET_MANA ( c ) = 0;
								script_log ( "Trigger: %s, VNum %d. attempt to set mana to a negative number.",GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ) );
							}
							alter_mana ( c,0 );

						}
						snprintf ( str, slen, "%d", GET_MANA ( c ) );
					}
					else if ( !strcasecmp ( field, "move" ) )
					{
						if ( subfield && *subfield )
						{
							int newmove = atoi ( subfield );
							if ( newmove >= 0 )
								GET_MOVE ( c ) = newmove;
							else
							{
								GET_MOVE ( c ) =0;
								script_log ( "Trigger: %s, VNum %d. attempt to set movepoints to a negative number.",GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ) );
							}
							alter_move ( c, 0 );
						}
						snprintf ( str, slen, "%d", GET_MOVE ( c ) );
					}
					else if ( !strcasecmp ( field, "master" ) )
					{
						if ( !c->master )
							*str = '\0';
						else
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( c->master ) );
					}

					break;
				case 'n':
					if ( !strcasecmp ( field, "next_in_room" ) )
					{
						if ( c->next_in_room )
							snprintf ( str, slen, "%c%ld", UID_CHAR,
							           GET_ID ( c->next_in_room ) );
						else
							*str = '\0';
					}
					else if ( !strcasecmp ( field, "next_follower" ) )
					{
						struct follow_type *k;
						Character *h = NULL;
						snprintf ( str, slen, "0" );
						if ( c->master )
						{
							for ( k = c->master->followers; k; k = k->next )
							{
								if ( c == k->follower )
								{
									if ( k->next && k->next->follower )
										h = k->next->follower;
									break;
								}
							}
						}

						if ( h )
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( h ) );
					}
					else if ( !strcasecmp ( field, "name" ) )
					{
						if ( GET_SHORT ( c ) != NULL )
							snprintf ( str, slen, "%s", GET_SHORT ( c ) );
						else if ( GET_NAME ( c ) != NULL )
							snprintf ( str, slen, "%s", GET_NAME ( c ) );
						else
							*str = '\0';
					}
					break;
				case 'o':
					if ( !strcasecmp ( field, "omr" ) )
						snprintf ( str, slen, "%s","mobile" );
					break;
				case 'p':
                                        if ( !strcasecmp ( field, "prac" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							GET_PRACTICES ( c ) += addition;
						}
						snprintf ( str, slen, "%d", GET_PRACTICES ( c ) );
					}
					else if ( !strcasecmp ( field, "perc" ) )
						snprintf ( str, slen, "%.4f", GET_PERC ( c ) );

					else if ( !strcasecmp ( field, "pkdeaths" ) )
						snprintf ( str, slen, "%d", GET_PK_RIP ( c ) );
					else if ( !strcasecmp ( field, "pkpoints" ) )
						snprintf ( str, slen, "%d", GET_PK_POINTS ( c ) );
					else if ( !strcasecmp ( field, "pkkills" ) )
						snprintf ( str, slen, "%d", GET_PK_CNT ( c ) );


					else if ( !strcasecmp ( field, "position" ) || !strcasecmp ( field, "pos" ) )
					{
						if ( subfield && *subfield )
						{
							for (int i = 0; *position_types[i] != '\n'; i++ )
							{
								if ( is_name ( subfield, position_types[i] ) )
								{
									GET_POS ( c ) = i;
								}
							}
						}
						if ( GET_POS ( c ) >= 0 && GET_POS ( c ) <=POS_STANDING )
							snprintf ( str, slen, "%s", position_types[ ( int ) GET_POS ( c ) ] );
						else
							*str = '\0';
					}
					else if ( !strcasecmp ( field, "people" ) )
					{
						if ( IN_ROOM ( c ) == NULL || !IN_ROOM ( c )->people )
						{
							*str = '\0';
						}
						else
						{
							if ( ( num = atoi ( subfield ) ) !=0 )
							{
								count = 0;
								for ( c = IN_ROOM ( c )->people; c;
								        c = c->next_in_room )
									if ( valid_dg_target ( c, TRUE ) )
									{
										if ( GET_MOB_VNUM ( c ) == num )
										{
											snprintf ( str, slen,"%c%ld", UID_CHAR,GET_ID ( c ) );
											count++;
											break;
										}
									}
								if ( !count )
									*str = '\0';
							}
							else
							{
								if ( IN_ROOM ( c ) != NULL )
								{
									if ( IN_ROOM ( c )->people )
									{
										snprintf ( str, slen,"%c%ld", UID_CHAR,
										           GET_ID ( IN_ROOM ( c )->people ) );
									}
									else
									{
										*str = '\0';
									}
								}
							}
						}
					}

					break;
				case 'r':


					if ( !strcasecmp ( field, "race" ) )
						snprintf ( str, slen, "%s", race_name ( c ) );

					else if ( !strcmp ( field, "remorts" ) )
						snprintf ( str, slen, "%u", REMORTS ( c ) );

					else if ( !strcasecmp ( field, "riding" ) )
					{
						if ( RIDING ( c ) )
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( RIDING ( c ) ) );
						else
							*str = '\0';
					}
					else if ( !strcasecmp ( field, "ridden_by" ) )
					{
						if ( RIDDEN_BY ( c ) )
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( RIDDEN_BY ( c ) ) );
						else
							*str = '\0';
					}
					else if ( !strcasecmp ( field, "room" ) )
					{
						/* see note in dg_scripts.h */
#ifdef ACTOR_ROOM_IS_UID
						snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) IN_ROOM ( c ) + ROOM_ID_BASE );
#else

						snprintf ( str, slen, "%d", IN_ROOM ( c )->number );
#endif

					}
					else if ( !strcasecmp ( field, "rpgroup" ) )
					{
						switch ( GET_RP_GROUP ( c ) )
						{
							case 1:
								snprintf ( str, slen, "%s", "jesters" );
								break;
							case 2:
								snprintf ( str, slen, "%s", "bitches" );
								break;
							case 3:
								snprintf ( str, slen, "%s", "riddlers" );
								break;
							case 4:
								snprintf ( str, slen, "%s", "madmen" );
								break;
							case 5:
								snprintf ( str, slen, "%s", "orsinis" );
								break;
							case 6:
								snprintf ( str, slen, "%s", "lolthite" );
								break;
							case 7:
								snprintf ( str, slen, "%s", "cthulytes" );
								break;
							case 8:
								snprintf ( str, slen, "%s", "alderisio" );
								break;
							case 9:
								snprintf ( str, slen, "%s", "fearless" );
								break;
							case 10:
								snprintf ( str, slen, "%s", "galliano" );
								break;
							default:
								*str = '\0';
								break;
						}
					}

					break;
				case 's':
					if ( !strcasecmp ( field, "speed" ) )
						snprintf ( str, slen, "%d", GET_SPEED ( c ) );
					else if ( !strcasecmp ( field, "sex" ) )
					{
						if ( subfield && *subfield )
						{
							if ( !strcasecmp ( subfield, "male" ) )
								GET_SEX ( c ) = SEX_MALE;
							else if ( !strcasecmp ( subfield, "female" ) )
								GET_SEX ( c ) = SEX_FEMALE;
							else if ( !strcasecmp ( subfield, "neutral" ) )
								GET_SEX ( c ) = SEX_NEUTRAL;
						}
						snprintf ( str, slen, "%s", genders[ ( int ) GET_SEX ( c ) ] );
					}
					else if ( !strcasecmp ( field, "str" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE;
							GET_STR ( c ) += addition;
							if ( GET_STR ( c ) > max )
								GET_STR ( c ) = max;
							if ( GET_STR ( c ) < 3 )
								GET_STR ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_STR ( c ) );
					}
					else if ( !strcasecmp ( field, "stradd" ) )
					{
						if ( GET_STR ( c ) == ( ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE ) )
						{
							if ( subfield && *subfield )
							{
								int addition = atoi ( subfield );
								GET_ADD ( c ) += addition;
								if ( GET_ADD ( c ) > 100 )
									GET_ADD ( c ) = 100;
								if ( GET_ADD ( c ) < 0 )
									GET_ADD ( c ) = 0;
							}
							snprintf ( str, slen, "%d", GET_ADD ( c ) );
						}
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "skill" ) )
					  snprintf ( str, slen, "%s", skill_percent ( c, subfield ) );
					else if ( !strcasecmp ( field, "skillset" ) )
					{
						if ( !IS_NPC ( c ) && subfield && *subfield )
						{
							char skillname[MAX_INPUT_LENGTH], *amount;
							amount = one_word ( subfield, skillname );
							skip_spaces ( &amount );
							if ( amount && *amount && is_number ( amount ) )
							{
								int skillnum = find_skill_num ( skillname );
								if ( skillnum > 0 )
								{
									int new_value = MAX ( 0, MIN ( 100, atoi ( amount ) ) );
									SET_SKILL ( c, skillnum, new_value );
								}
							}
						}
						strcpy ( str, "" ); /* so the parser know we recognize 'skillset' as a field */
					}
					else if ( !strcasecmp ( field, "stamina" ) )
					{
						if ( subfield && *subfield )
						{
							int newstamina = atoi ( subfield );
							if ( newstamina >= 0 )
								GET_STAMINA ( c ) = newstamina;
							else
							{
								GET_STAMINA ( c ) =0;
								script_log ( "Trigger: %s, VNum %d. attempt to set movepoints to a negative number.",GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ) );
							}
							alter_stamina ( c,0 );
						}
						snprintf ( str, slen, "%d", GET_STAMINA ( c ) );
					}
					else if ( !strcasecmp ( field, "subskill" ) )
					  snprintf ( str, slen, "%s", sub_percent ( c, subfield ) );
					else if ( !strcasecmp ( field, "subincrease" ) )
					{
						if((count = sub_number(subfield)) > 0)
                                                //count = sub_number ( subfield );
						//if ( count > 0 && count < 98 )
							improve_sub ( c, ( enum subskill_list ) count, 2 );
						snprintf ( str, slen, "%d", count );
					}
					else if ( !strcasecmp ( field, "subdecrease" ) )
					{
						if ( ( count = sub_number ( subfield ) ) > 0 )
							improve_sub ( c, ( enum subskill_list ) count, -2 );
						snprintf ( str, slen, "%d", count );

					}
					else if ( !strcasecmp ( field, "saving_para" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							GET_SAVE ( c, SAVING_PARA ) += addition;
						}
						snprintf ( str, slen, "%d", GET_SAVE ( c, SAVING_PARA ) );
					}
					else if ( !strcasecmp ( field, "saving_rod" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							GET_SAVE ( c, SAVING_ROD ) += addition;
						}
						snprintf ( str, slen, "%d", GET_SAVE ( c, SAVING_ROD ) );
					}
					else if ( !strcasecmp ( field, "saving_petri" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							GET_SAVE ( c, SAVING_PETRI ) += addition;
						}
						snprintf ( str, slen, "%d", GET_SAVE ( c, SAVING_PETRI ) );
					}
					else if ( !strcasecmp ( field, "saving_breath" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							GET_SAVE ( c, SAVING_SPELL ) += addition;
						}
						snprintf ( str, slen, "%d", GET_SAVE ( c, SAVING_BREATH ) );
					}
					else if ( !strcasecmp ( field, "saving_spell" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							GET_SAVE ( c, SAVING_SPELL ) += addition;
						}
						snprintf ( str, slen, "%d", GET_SAVE ( c, SAVING_SPELL ) );
					}
					break;

					break;
				case 't':

					if ( !strcasecmp ( field, "title" ) )
					{
						if ( subfield && *subfield && valid_dg_target ( c, DG_ALLOW_GODS ) )
						{
							free_string ( &GET_TITLE ( c ) );
							GET_TITLE ( c ) = strdup ( subfield );
						}
						snprintf ( str, slen, "%s", GET_TITLE ( c ) );
					}
					else if ( !strcasecmp (field, "thirst" ) )
					  {
					    if ( subfield && *subfield ) {
					      GET_COND(c, THIRST) = (char) atoi(subfield);
					    }
					    snprintf ( str, slen, "%d", GET_COND(c, FULL));
					  }

					else if ( !strcasecmp ( field, "trade" ) )
					{
						if ( subfield && *subfield && ( is_number ( subfield ) || *subfield=='-' ) )
						{
							int addition = atoi ( subfield );
							TRADEPOINTS ( c ) += addition;
							new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( c ) ), TRUE, "[TRADEPOINTS] %s %s %d tradepoints from trigger %d, %s. (%d)",  GET_NAME (c), (addition>0?"gained":"lost"), (addition>0?addition:-addition), GET_TRIG_VNUM(trig), GET_TRIG_NAME(trig), TRADEPOINTS(c));
						}
						snprintf ( str, slen, "%d", TRADEPOINTS ( c ) );
					}
					break;
				case 'v':

					if ( !strcasecmp ( field, "varexists" ) )
					{
					  dg_varexists(SCRIPT(c),subfield, str, slen);
					}
					else if ( !strcasecmp ( field, "vnum" ) )
					{
						if ( subfield && *subfield )
						{
							snprintf ( str, slen, "%d", ( int ) ( GET_MOB_VNUM ( c ) == atoi ( subfield ) ) );
						}
						else
						{
							if ( IS_NPC ( c ) )
								snprintf ( str, slen, "%d", GET_MOB_VNUM ( c ) );
							else
								/*
								 * for compatibility with unsigned indexes
								 * - this is deprecated - use %actor.is_pc% to check
								 * instead of %actor.vnum% == -1  --Welcor 09/03
								 */
								strcpy ( str, "-1" );
						}
					}
					break;
				case 'w':

					if ( !strcasecmp ( field, "weight" ) )
						snprintf ( str, slen, "%d", GET_WEIGHT ( c ) );

					else if ( !strcasecmp ( field, "wis" ) )
					{
						if ( subfield && *subfield )
						{
							int addition = atoi ( subfield );
							int max = ( IS_NPC ( c ) || GET_LEVEL ( c ) >= LVL_GRGOD ) ? MAX_IMM_BASE : MAX_MORTAL_BASE;
							GET_WIS ( c ) += addition;
							if ( GET_WIS ( c ) > max )
								GET_WIS ( c ) = max;
							if ( GET_WIS ( c ) < 3 )
								GET_WIS ( c ) = 3;
						}
						snprintf ( str, slen, "%d", GET_WIS ( c ) );
					}
					break;
			} /* switch *field */

			if ( *str == '\x1' ) /* no match found in switch */
			{
				if ( SCRIPT ( c ) )
				{
					for ( vd = ( SCRIPT ( c ) )->global_vars; vd; vd = vd->next )
						if ( !strcasecmp ( vd->name.c_str(), field ) )
							break;
					if ( vd )
						snprintf ( str, slen, "%s", vd->value.c_str() );
					else
					{
						*str = '\0';
						script_log ( "Trigger: %s, VNum %d. unknown char field: '%s'",
						             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), field );
					}
				}
				else
				{
					*str = '\0';
					script_log ( "Trigger: %s, VNum %d. unknown char field: '%s'",
					             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), field );
				}
			}
		} /* if (c) ...*/

		else if ( o )
		{
			if ( text_processed ( field, subfield, vd, str, slen ) )
				return;

			*str = '\x1';
			* ( str +1 ) = '\0';
			switch ( LOWER ( *field ) )
			{
				case 'c':
					if ( !strcasecmp ( field, "carried_by" ) )
						if ( o->carried_by )
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( o->carried_by ) );
						else
							strcpy ( str, "" );
					else if ( !strcasecmp ( field, "cost" ) )
						snprintf ( str, slen, "%lld", GET_OBJ_COST ( o ) );

					else if ( !strcasecmp ( field, "cost_per_day" ) )
						snprintf ( str, slen, "%d", GET_OBJ_RENT ( o ) );

					else if ( !strcasecmp ( field, "contents" ) )
					{

						struct obj_data *next_obj = NULL, *item = NULL;
						bool found = FALSE;

						if ( o->contains == NULL )
						{
							strcpy ( str, "" );
						}
						else
						{
							if ( !subfield || !*subfield )
							{
								snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( o->contains ) );
								return;
							}

							for ( item = o->contains; item && !found; item = next_obj )
							{
								next_obj = item->next_content;

								if ( is_number ( subfield ) )
								{
									if ( atoi ( subfield ) == GET_OBJ_VNUM ( item ) )
									{

										snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( item ) );
										found = TRUE;
										break;
									}
									else if ( isname ( subfield, item->short_description ) )
									{
										snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( item ) );	/* arg given, found */
										return;
									}
								}
								else
								{
									if ( isname ( subfield, item->name ) )
									{

										snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( item ) );
										found = TRUE;
										break;
									}
								}
							}
							if ( !found )
								strcpy ( str, "" );
						}


					}
					else if ( !strcasecmp ( field, "count" ) )
					{
						int cnt = 0;
						struct obj_data *next_obj = NULL, *item = NULL;

						if ( GET_OBJ_TYPE ( o ) == ITEM_CONTAINER )
						{
							for ( item = o->contains; item; item = next_obj )
							{
								next_obj = item->next_content;
								if ( !subfield || !*subfield )
									++cnt;
								else if ( is_number ( subfield ) )
								{
									if ( atoi ( subfield ) == GET_OBJ_VNUM ( item ) )
										++cnt;
								}
								else
								{
									if ( is_name ( subfield, item->name ) )
										++cnt;
								}
							}
							snprintf ( str, slen, "%d", cnt );
						}
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "contained_by" ) )
					{
						if ( o->in_obj!=NULL )
						        snprintf ( str, slen,"%c%ld",UID_CHAR, GET_ID(o->in_obj) );
						else
							strcpy ( str, "" );
					}
					break;
				case 'd':
					if ( !strcasecmp ( field, "dest" ) )
					{
						/* lets you find out what the next destination point vnum
						   is, and change it if need be with a subfield, if subfield
						   is invalid it wont be added. Caution here that destination
						   isnt set to 0. I would check for it here, but 0 IS a valid vnum
						   and you may actually want it. But any non numbers passed as subfields
						   will come out as 0.
						*/
						struct travel_point_data *t;
						room_vnum dest = NOWHERE;
						for ( t = TRAVEL_LIST ( o ); t; t = t->next )
							if ( t->last_stop == TRUE )
							{
								dest = t->dest;
								break;
							}
						if ( subfield && *subfield )
						{
							dest = atoi ( subfield );
							if ( t && real_room ( dest ) != NULL )
								t->dest = dest;
							else
								dest = t->dest;
						}

						snprintf ( str, slen, "%d", dest );
					}
					break;
				case 'h':
					if ( !strcasecmp ( field, "has_pos" ) )
					{
						if ( subfield && *subfield )
						{
							int where_to_worn ( int where );
							int where = search_block ( subfield, body, FALSE );
							if ( !CAN_WEAR ( o, where_to_worn ( where ) ) )
								snprintf ( str, slen, "0" );
							else
								snprintf ( str, slen, "1" );
						}
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "has_in" ) )
					{
						struct obj_data *next_obj = NULL, *item = NULL;
						bool found = FALSE;
						snprintf ( str, slen, "0" );

						if ( GET_OBJ_TYPE ( o ) == ITEM_CONTAINER )
						{
							for ( item = o->contains; item && !found;
							        item = next_obj )
							{
								next_obj = item->next_content;
								if ( is_number ( subfield ) )
								{
									if ( atoi ( subfield ) == GET_OBJ_VNUM ( item ) )
									{
										snprintf ( str, slen, "1" );
										found = TRUE;
										break;
									}
								}
								else
								{
									if ( is_name ( subfield, item->name ) )
									{
										snprintf ( str, slen, "1" );
										found = TRUE;
										break;
									}
								}
							}
							if ( !found )
								snprintf ( str, slen, "0" );
						}
						else
							snprintf ( str, slen, "0" );
					}
					else if ( !strcasecmp ( field, "hunt" ) )
					{
						if ( hunt_location ( GET_ID ( o ), STRUCT_IS_OBJ ) )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}

					break;
				case 'i':

					if ( !strcasecmp ( field, "id" ) )
						snprintf ( str, slen, "%ld", GET_ID ( o ) );
					else if ( !strcasecmp ( field, "is_inroom" ) )
					{
						if ( IN_ROOM ( o ) != NULL )
							snprintf ( str, slen,"%c%ld",UID_CHAR, ( long ) IN_ROOM ( o )->number + ROOM_ID_BASE );
						else
							strcpy ( str, "" );
					}
					else if ( !strcasecmp ( field, "is_inobj" ) )
					{
						if ( o->in_obj!=NULL )
						        snprintf ( str, slen,"%c%ld",UID_CHAR, GET_ID(o->in_obj) );
						else
							strcpy ( str, "" );
					}

					break;
				case 'n':

					if ( !strcasecmp ( field, "name" ) )
						snprintf ( str, slen, "%s", o->name );

					else if ( !strcasecmp ( field, "next_in_list" ) )
					{
						if ( o->next_content )
							snprintf ( str, slen, "%c%ld", UID_CHAR,
							           GET_ID ( o->next_content ) );
						else
							strcpy ( str, "" );
					}

					break;
				case 'o':
					if ( !strcasecmp ( field, "omr" ) )
						snprintf ( str, slen, "%s","object" );
					break;

				case 'p':
					if ( !strcasecmp ( field, "people" ) )
					{
						if ( obj_room ( o ) != NULL )
						{
							if ( ( num =atoi ( subfield ) ) != 0 )
							{
								count = 0;
								for ( c = IN_ROOM ( o )->people; c;
								        c = c->next_in_room )
									if ( valid_dg_target ( c, TRUE ) )
									{
										if ( GET_MOB_VNUM ( c ) == num )
										{
											snprintf ( str, slen,"%c%ld", UID_CHAR,GET_ID ( c ) );
											count++;
											break;
										}
									}
								if ( !count )
									*str = '\0';
							}
							else
							{
								if ( IN_ROOM ( o ) && IN_ROOM ( o )->people )

									snprintf ( str, slen, "%c%ld", UID_CHAR,
									           GET_ID ( IN_ROOM ( o )->people ) );
								else
									*str = '\0';
							}
						}
					}

					break;
				case 'r':
					if ( !strcasecmp ( field, "room" ) )
					{
						room_rnum rm;
						if ( ( rm = obj_room ( o ) ) != NULL )
							snprintf ( str, slen, "%c%ld", UID_CHAR,
							           ( long ) rm->number + ROOM_ID_BASE );
						else
							strcpy ( str, "" );
					}

					break;
				case 's':
					if ( !strcasecmp ( field, "shortdesc" ) )
						snprintf ( str, slen,"%s", o->short_description );

					break;
				case 't':
					if ( !strcasecmp ( field, "type" ) )
						new_sprinttype ( GET_OBJ_TYPE ( o ), item_types, str, slen );

					else if ( !strcasecmp ( field, "timer" ) )
					{
						if ( subfield && *subfield && is_number ( subfield ) )
						{
							if ( atoi ( subfield ) >= -1 )
							{
								GET_OBJ_TIMER ( o ) = atoi ( subfield );
							}
						}
						snprintf ( str, slen, "%d", GET_OBJ_TIMER ( o ) );
					}
					break;
				case 'v':
					if ( !strcasecmp ( field, "varexists" ) )
					{
					  dg_varexists(SCRIPT(o),subfield, str, slen);
					}
					else if ( !strcasecmp ( field, "vnum" ) )
					{
						if ( subfield && *subfield )
						{
							snprintf ( str, slen, "%d", ( int ) ( GET_OBJ_VNUM ( o ) == atoi ( subfield ) ) );
						}
						else
						{
							snprintf ( str, slen, "%d", GET_OBJ_VNUM ( o ) );
						}
					}
					else if ( !strcasecmp ( field, "val0" ) )
					{
						if ( subfield && *subfield && is_number ( subfield ) )
						{
							GET_OBJ_VAL ( o, 0 ) = atoi ( subfield );
						}
						snprintf ( str, slen, "%d", GET_OBJ_VAL ( o, 0 ) );
					}
					else if ( !strcasecmp ( field, "val1" ) )
					{
						if ( subfield && *subfield && is_number ( subfield ) )
						{
							GET_OBJ_VAL ( o, 1 ) = atoi ( subfield );
						}
						snprintf ( str, slen, "%d", GET_OBJ_VAL ( o, 1 ) );
					}
					else if ( !strcasecmp ( field, "val2" ) )
					{
						if ( subfield && *subfield && is_number ( subfield ) )
						{
							GET_OBJ_VAL ( o, 2 ) = atoi ( subfield );
						}
						snprintf ( str, slen, "%d", GET_OBJ_VAL ( o, 2 ) );
					}
					else if ( !strcasecmp ( field, "val3" ) )
					{
						if ( subfield && *subfield && is_number ( subfield ) )
						{
							GET_OBJ_VAL ( o, 3 ) = atoi ( subfield );
						}
						snprintf ( str, slen, "%d", GET_OBJ_VAL ( o, 3 ) );
					}
					else if ( !strcasecmp ( field, "val4" ) )
					{
						if ( subfield && *subfield && is_number ( subfield ) )
						{
							GET_OBJ_VAL ( o, 4 ) = atoi ( subfield );
						}
						snprintf ( str, slen, "%d", GET_OBJ_VAL ( o, 4 ) );
					}
					break;
				case 'w':
					if ( !strcasecmp ( field, "weight" ) )
						snprintf ( str, slen, "%d", GET_OBJ_WEIGHT ( o ) );
					else if ( !strcasecmp ( field, "worn_by" ) )
					{
						if ( o->worn_by )
							snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( o->worn_by ) );
						else
							strcpy ( str, "" );
					}
					break;
			} /* switch *field */






			if ( *str == '\x1' ) /* no match in switch */
			{
				if ( SCRIPT ( o ) )   /* check for global var */
				{
					for ( vd = ( SCRIPT ( o ) )->global_vars; vd; vd = vd->next )
						if ( !strcasecmp ( vd->name.c_str(), field ) )
							break;
					if ( vd )
						snprintf ( str, slen, "%s", vd->value.c_str() );
					else
					{
						*str = '\0';
						script_log ( "Trigger: %s, VNum %d, type: %d. unknown object field: '%s'",
						             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), type, field );
					}
				}
				else
				{
					*str = '\0';
					script_log ( "Trigger: %s, VNum %d, type: %d. unknown object field: '%s'",
					             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), type, field );
				}
			}
		} /* if (o) ... */

		else if ( r )
		{
			if ( text_processed ( field, subfield, vd, str, slen ) )
				return;
			*str = '\x1';
			* ( str +1 ) = '\0';
			switch ( LOWER ( *field ) )
			{

				case 'c':
					if ( !strcasecmp ( field, "contents" ) )
					{
						if ( subfield && *subfield )
						{
							for ( obj = r->contents; obj; obj = obj->next_content )
							{
								if ( GET_OBJ_VNUM ( obj ) == atoi ( subfield ) )
								{
									snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( obj ) );	/* arg given, found */
									return;
								}
								else if ( isname ( subfield, obj->short_description ) )
								{
									snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( obj ) );	/* arg given, found */
								}

							}
							if ( !obj )
								strcpy ( str, "" );	/* arg given, not found */
						}
						else  	/* no arg given */
						{
							if ( r->contents )
							{
								snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( r->contents ) );
							}
							else
							{
								strcpy ( str, "" );
							}
						}

					}
					break;
				case 'd':
					if ( !strcasecmp ( field, "down" ) )
					{
						if ( R_EXIT ( r, DOWN ) )
						{
							if ( subfield && *subfield )
							{
								if ( !strcasecmp ( subfield, "vnum" ) )
									snprintf ( str, slen, "%d", GET_ROOM_VNUM ( R_EXIT ( r, DOWN )->to_room ) );
								else if ( !strcasecmp ( subfield, "key" ) )
									snprintf ( str, slen, "%d", R_EXIT ( r, DOWN )->key );
								else if ( !strcasecmp ( subfield, "bits" ) )
									new_sprintbit ( R_EXIT ( r, DOWN )->exit_info ,exit_bits, str, slen );
								else if ( !strcasecmp ( subfield, "room" ) )
								{
									if ( R_EXIT ( r, DOWN )->to_room != NULL )
										snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) R_EXIT ( r, DOWN )->to_room + ROOM_ID_BASE );
									else
										*str = '\0';
								}
							}
							else /* no subfield - default to bits */
								new_sprintbit ( R_EXIT ( r, DOWN )->exit_info ,exit_bits, str, slen );
						}
						else
							*str = '\0';
					}
					break;
				case 'e':

					if ( !strcasecmp ( field, "east" ) )
					{
						if ( R_EXIT ( r, EAST ) )
						{
							if ( subfield && *subfield )
							{
								if ( !strcasecmp ( subfield, "vnum" ) )
									snprintf ( str, slen, "%d", GET_ROOM_VNUM ( R_EXIT ( r, EAST )->to_room ) );
								else if ( !strcasecmp ( subfield, "key" ) )
									snprintf ( str, slen, "%d", R_EXIT ( r, EAST )->key );
								else if ( !strcasecmp ( subfield, "bits" ) )
									new_sprintbit ( R_EXIT ( r, EAST )->exit_info ,exit_bits, str, slen );
								else if ( !strcasecmp ( subfield, "room" ) )
								{
									if ( R_EXIT ( r, EAST )->to_room != NULL )
										snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) R_EXIT ( r, EAST )->to_room + ROOM_ID_BASE );
									else
										*str = '\0';
								}
							}
							else /* no subfield - default to bits */
								new_sprintbit ( R_EXIT ( r, EAST )->exit_info ,exit_bits, str, slen );

						}
						else
							*str = '\0';
					}
					break;
				case 'f':

					if ( !strcasecmp ( field, "flag" ) ) 
					{
						if ( !subfield || !*subfield )
						{
							*str = '\0';
							script_log ( "Trigger: %s, VNum %d. no room flag specified",
					             		GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ) );
						}
						else if ( !strcasecmp ( subfield, "death" ) )
						{
							if ( ROOM_FLAGGED ( r, ROOM_DEATH ) )
								snprintf ( str, slen, "1" );
							else
								snprintf ( str, slen, "0" );				
						} 
						else if ( !strcasecmp ( subfield, "godroom" ) )
						{
							if ( ROOM_FLAGGED ( r, ROOM_GODROOM ) )
								snprintf ( str, slen, "1" );
							else
								snprintf ( str, slen, "0" );				
						} 
						else if ( !strcasecmp ( subfield, "house" ) )
						{
							if ( ROOM_FLAGGED ( r, ROOM_HOUSE ) )
								snprintf ( str, slen, "1" );
							else
								snprintf ( str, slen, "0" );				
						} 
						else if ( !strcasecmp ( subfield, "nomob" ) )
						{
							if ( ROOM_FLAGGED ( r, ROOM_NOMOB ) )
								snprintf ( str, slen, "1" );
							else
								snprintf ( str, slen, "0" );				
						} 
						else if ( !strcasecmp ( subfield, "peaceful" ) )
						{
							if ( ROOM_FLAGGED ( r, ROOM_PEACEFUL ) )
								snprintf ( str, slen, "1" );
							else
								snprintf ( str, slen, "0" );				
						} 
						else if ( !strcasecmp ( subfield, "roleplay" ) )
						{
							if ( ROOM_FLAGGED ( r, ROOM_ROLEPLAY ) )
								snprintf ( str, slen, "1" );
							else
								snprintf ( str, slen, "0" );				
						} 
						else 
						{
							*str = '\0';
							script_log ( "Trigger: %s, VNum %d. unknown room flag: '%s'",
					             		GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), subfield );
						}
					}
					break;
				case 'h':

					if (!strcasecmp ( field, "has_script" ) )
					{
						if ( r->script != NULL )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					break;
				case 'i':

					if ( !strcasecmp ( field, "id" ) )
					{
						room_rnum room_id = real_room ( r->number );
						if ( room_id != NULL )
							snprintf ( str, slen, "%ld", ( long ) room_id + ROOM_ID_BASE );	/* added by welcor */
						else
							*str = '\0';

					}
					else if ( !strcasecmp ( field, "is_peaceful" ) )
					{
						if ( ROOM_FLAGGED ( r, ROOM_PEACEFUL ) )
							snprintf ( str, slen, "1" );
						else
							snprintf ( str, slen, "0" );
					}
					break;
				case 'n':
					if ( !strcasecmp ( field, "name" ) )
						snprintf ( str, slen, "%s",  r->name );
					else if ( !strcasecmp ( field, "north" ) )
					{
						if ( R_EXIT ( r, NORTH ) )
						{
							if ( subfield && *subfield )
							{
								if ( !strcasecmp ( subfield, "vnum" ) )
									snprintf ( str, slen, "%d", GET_ROOM_VNUM ( R_EXIT ( r, NORTH )->to_room ) );
								else if ( !strcasecmp ( subfield, "key" ) )
									snprintf ( str, slen, "%d", R_EXIT ( r, NORTH )->key );
								else if ( !strcasecmp ( subfield, "bits" ) )
									new_sprintbit ( R_EXIT ( r, NORTH )->exit_info ,exit_bits, str, slen );
								else if ( !strcasecmp ( subfield, "room" ) )
								{
									if ( R_EXIT ( r, NORTH )->to_room != NULL )
										snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) R_EXIT ( r, NORTH )->to_room + ROOM_ID_BASE );
									else
										*str = '\0';
								}
							}
							else /* no subfield - default to bits */
								new_sprintbit ( R_EXIT ( r, NORTH )->exit_info ,exit_bits, str, slen );
						}
						else
							*str = '\0';
					}
					else if ( !strcasecmp ( field, "no_teleport_in" )) {
					  snprintf(str, slen, ROOM_FLAGGED(r, ROOM_NOTELEPORT_IN) ? "1" : "0");
					}


					break;
				case 'o':
					if ( !strcasecmp ( field, "omr" ) )
						snprintf ( str, slen, "%s","room" );
					break;

				case 'p':
					if ( !strcasecmp ( field, "people" ) )
					{
						if ( ( num = atoi ( subfield ) ) !=0 )
						{
							count = 0;
							for ( c = r->people; c;
							        c = c->next_in_room )
								if ( valid_dg_target ( c, TRUE ) )
								{
									if ( GET_MOB_VNUM ( c ) == num )
									{
										snprintf ( str, slen,"%c%ld", UID_CHAR, GET_ID ( c ) );
										count++;
										break;
									}
								}
							if ( !count )
								*str = '\0';
						}
						else
						{
							if ( r->people )
								snprintf ( str, slen, "%c%ld", UID_CHAR, GET_ID ( r->people ) );
							else
								*str = '\0';
						}
					}
					break;
				case 's':
					if ( !strcasecmp ( field, "south" ) )
					{
						if ( R_EXIT ( r, SOUTH ) )
						{
							if ( subfield && *subfield )
							{
								if ( !strcasecmp ( subfield, "vnum" ) )
									snprintf ( str, slen, "%d", GET_ROOM_VNUM ( R_EXIT ( r, SOUTH )->to_room ) );
								else if ( !strcasecmp ( subfield, "key" ) )
									snprintf ( str, slen, "%d", R_EXIT ( r, SOUTH )->key );
								else if ( !strcasecmp ( subfield, "bits" ) )
									new_sprintbit ( R_EXIT ( r, SOUTH )->exit_info ,exit_bits, str, slen );
								else if ( !strcasecmp ( subfield, "room" ) )
								{
									if ( R_EXIT ( r, SOUTH )->to_room != NULL )
										snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) R_EXIT ( r, SOUTH )->to_room + ROOM_ID_BASE );
									else
										*str = '\0';
								}
							}
							else /* no subfield - default to bits */
								new_sprintbit ( R_EXIT ( r, SOUTH )->exit_info ,exit_bits, str, slen );

						}
						else
							*str = '\0';
					}
					else if ( !strcasecmp ( field, "sector" ) )
						new_sprinttype ( r->sector_type, sector_types, str, slen );

					break;
				case 'u':

					if ( !strcasecmp ( field, "up" ) )
					{
						if ( R_EXIT ( r, UP ) )
						{
							if ( subfield && *subfield )
							{
								if ( !strcasecmp ( subfield, "vnum" ) )
									snprintf ( str, slen, "%d", GET_ROOM_VNUM ( R_EXIT ( r, UP )->to_room ) );
								else if ( !strcasecmp ( subfield, "key" ) )
									snprintf ( str, slen, "%d", R_EXIT ( r, UP )->key );
								else if ( !strcasecmp ( subfield, "bits" ) )
									new_sprintbit ( R_EXIT ( r, UP )->exit_info ,exit_bits, str, slen );
								else if ( !strcasecmp ( subfield, "room" ) )
								{
									if ( R_EXIT ( r, UP )->to_room != NULL )
										snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) R_EXIT ( r, UP )->to_room + ROOM_ID_BASE );
									else
										*str = '\0';
								}
							}
							else /* no subfield - default to bits */
								new_sprintbit ( R_EXIT ( r, UP )->exit_info ,exit_bits, str, slen );
						}
						else
							*str = '\0';
					}

					break;
				case 'v':

					if ( !strcasecmp ( field, "varexists" ) )
					{
					  dg_varexists(SCRIPT(r),subfield, str, slen);
					}
					else if ( !strcasecmp ( field, "vnum" ) )
					{
						if ( subfield && *subfield )
							snprintf ( str, slen, "%d", ( int ) ( r->number == atoi ( subfield ) ) );
						else
							snprintf ( str, slen, "%d", r->number );
					}

					break;
				case 'w':
					if ( !strcasecmp ( field, "west" ) )
					{
						if ( R_EXIT ( r, WEST ) )
						{
							if ( subfield && *subfield )
							{
								if ( !strcasecmp ( subfield, "vnum" ) )
									snprintf ( str, slen, "%d", GET_ROOM_VNUM ( R_EXIT ( r, WEST )->to_room ) );
								else if ( !strcasecmp ( subfield, "key" ) )
									snprintf ( str, slen, "%d", R_EXIT ( r, WEST )->key );
								else if ( !strcasecmp ( subfield, "bits" ) )
									new_sprintbit ( R_EXIT ( r, WEST )->exit_info ,exit_bits, str, slen );
								else if ( !strcasecmp ( subfield, "room" ) )
								{
									if ( R_EXIT ( r, WEST )->to_room != NULL )
										snprintf ( str, slen, "%c%ld", UID_CHAR, ( long ) R_EXIT ( r, WEST )->to_room + ROOM_ID_BASE );
									else
										*str = '\0';
								}
							}
							else /* no subfield - default to bits */
								new_sprintbit ( R_EXIT ( r, WEST )->exit_info ,exit_bits, str, slen );

						}
						else
							*str = '\0';
					}
					else if ( !strcasecmp ( field, "weather" ) )
					{
						const char *sky_look[] =
						{
							"sunny",
							"cloudy",
							"rainy",
							"lightning"
						};

						if ( !ROOM_FLAGGED ( real_room ( ( r->number ) ), ROOM_INDOORS ) )
							snprintf ( str, slen, "%s", sky_look[zone_table[GET_ROOM_ZONE ( real_room ( ( r->number ) ) ) ].sky] );
						else
							*str = '\0';


					}/* end of switch*/
					break;
			}

			if ( *str == '\x1' ) /* no match in switch */
			{
				if ( SCRIPT ( r ) )   /* check for global var */
				{
					for ( vd = ( SCRIPT ( r ) )->global_vars; vd; vd = vd->next )
						if ( !strcasecmp ( vd->name.c_str(), field ) )
							break;
					if ( vd )
						snprintf ( str, slen, "%s", vd->value.c_str() );
					else
					{
						*str = '\0';
						script_log ( "Trigger: %s, VNum %d, type: %d. unknown room field: '%s'",
						             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), type, field );
					}
				}
				else
				{
					*str = '\0';
					script_log ( "Trigger: %s, VNum %d, type: %d. unknown room field: '%s'",
					             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), type, field );
				}
			}
		}
		else if ( vd != NULL && text_processed ( field, subfield, vd, str, slen ) ) return;
	}
}


/*
 * Now automatically checks if the variable has more then one field
 * in it. And if the field returns a name or a script UID or the like
 * it can recurse.
 * If you supply a value like, %actor.int.str% it wont blow up on you
 * either.
 * - Jamie Nelson 31st Oct 2003 01:03
 *
 * Now also lets subfields have variables parsed inside of them
 * so that:
 * %echo% %actor.gold(%actor.gold%)%
 * will double the actors gold every time its called.  etc...
 * - Jamie Nelson 31st Oct 2003 01:24
 */

/* substitutes any variables into line and returns it as buf */
void var_subst ( void *go, struct script_data *sc, trig_data *trig,
                 int type, char *line, char *buf, size_t b_len )
{
	char tmp[MAX_INPUT_LENGTH] = "", repl_str[MAX_INPUT_LENGTH] = "";
	char *var = NULL, *field = NULL, *p = NULL;
	char *subfield_p, subfield[MAX_INPUT_LENGTH] = "";
	int left, len = 0;
	int paren_count = 0;
	int dots = 0;

	/* skip out if no %'s */
	if ( !strchr ( line, '%' ) )
	{
		strlcpy ( buf, line, b_len );
		return;
	}
	/*lets just empty these to start with*/
	*repl_str = *tmp = '\0';
	left = b_len-1;

	strlcpy ( tmp, line, sizeof ( tmp ) );
	p = tmp;

	while ( *p && ( left > 0 ) )
	{

		subfield_p = subfield;

		/* copy until we find the first % */
		while ( *p && ( *p != '%' ) && ( left > 0 ) )
		{
			* ( buf++ ) = * ( p++ );
			left--;
		}

		*buf = '\0';

		/* double % */
		if ( *p && ( * ( ++p ) == '%' ) && ( left > 0 ) )
		{
			* ( buf++ ) = * ( p++ );
			*buf = '\0';
			left--;
			continue;
		}

		/* so it wasn't double %'s */
		else if ( *p && ( left > 0 ) )
		{

			/* search until end of var or beginning of field */
			for ( var = p; *p && ( *p != '%' ) && ( *p != '.' ); p++ )
				;

			field = p;
			if ( *p == '.' )
			{
				* ( p++ ) = '\0';
				dots = 0;
				for ( field = p; *p && ( ( *p != '%' ) || ( paren_count > 0 ) || ( dots ) ); p++ )
				{
					if ( dots > 0 )
					{
						*subfield_p = '\0';
						find_replacement ( go, sc, trig, type, var, field, subfield, repl_str, sizeof ( repl_str ) );
						if ( *repl_str )
						{
							char tmp2[MAX_INPUT_LENGTH] = "";
							snprintf ( tmp2, sizeof ( tmp2 ), "eval tmpvr %s", repl_str ); //temp var
							process_eval ( go, sc, trig, type, tmp2 );
							var = ( char * ) "tmpvr";
							field = p;
							dots = 0;
							continue;
						}
						dots = 0;
					}
					else if ( *p=='(' )
					{
						*p = '\0';
						paren_count++;
					}
					else if ( *p==')' )
					{
						*p = '\0';
						paren_count--;
					}
					else if ( paren_count > 0 )
					{
						*subfield_p++ = *p;
					}
					else if ( *p=='.' )
					{
						*p = '\0';
						dots++;
					}
				} /* for (field.. */
			} /* if *p == '.' */

			* ( p++ ) = '\0';
			*subfield_p = '\0';

			if ( *subfield )
			{
				char tmp2[MAX_INPUT_LENGTH] = "";
				var_subst ( go, sc, trig, type, subfield, tmp2, sizeof ( tmp2 ) );
				strlcpy ( subfield, tmp2, sizeof ( subfield ) );
			}

			find_replacement ( go, sc, trig, type, var, field, subfield, repl_str, sizeof ( repl_str ) );

			//strlcat(buf, repl_str, b_len);
			strncat ( buf, repl_str, left );
			len = strlen ( repl_str );
			buf += len;
			left -= len;
		} /* else if *p .. */
	} /* while *p .. */
}
