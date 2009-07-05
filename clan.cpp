/**************************************************************************
 * File: clan.c                       Intended to be used with CircleMUD  *
 * Usage: This is the code for clans                                      *
 * By Mehdi Keddache (Heritsun on Eclipse of Fate eclipse.argy.com 7777)  *
 * CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
 * CircleMUD (C) 1993, 94 by the Trustees of the Johns Hopkins University *
 **************************************************************************/

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "interpreter.h"
#include "spells.h"
#include "handler.h"
#include "clan.h"
#include "descriptor.h"

int num_of_clans;
vector <clan_rec> clan ( MAX_CLANS );
void tag_argument ( char *argument, char *tag );
extern Descriptor *descriptor_list;
void smash_tilde ( char *str );
void strip_cr ( char * );
void clan_to_store ( int i );
int store_to_clan ( int i );
char *clan_name ( int idnum );
void update_clan_member ( const char * name, int rank, int clan );
void remove_clan_member ( const char * name, int clan );
void add_clan_member ( const char * name, int rank, int clan );
void free_clan_lists ( void );

ACMD(do_ctell);

extern int TEMP_LOAD_CHAR;

vector<clan_list_data> clan_list[MAX_CLANS];


char clan_privileges[NUM_CP + 1][20] =
{
	"setplan", "enroll", "expel", "promote",
	"demote", "setfees","withdraw", "setapplev"
};

void send_clan_format ( Character *ch )
{
	int c, r;

	ch->Send ( "Clan commands available to you:\r\n"
	           "   clan who\r\n"
	           "   clan status\r\n"
	           "   clan list\r\n" 
                   "   clan info <clan>\r\n" 
                   "   clan leave\r\n");
	if ( GET_LEVEL ( ch ) >= LVL_CLAN_GOD )
		ch->Send ( "   clan create     <leader> <clan name>\r\n"
		           "   clan destroy    <clan>\r\n"
		           "   clan enroll     <player> <clan>\r\n"
		           "   clan expel      <player> <clan>\r\n"
		           "   clan promote    <player> <clan>\r\n"
		           "   clan demote     <player> <clan>\r\n"
		           "   clan withdraw   <amount> <clan>\r\n"
		           "   clan deposit    <amount> <clan>\r\n"
		           "   clan set ranks  <rank>   <clan>\r\n"
		           "   clan set appfee <amount> <clan>\r\n"
		           "   clan set dues   <amount> <clan>\r\n"
		           "   clan set applev <level>  <clan>\r\n"
		           "   clan set plan   <clan>\r\n"
		           "   clan set war    <clan> <clan>\r\n"
		           "   clan set peace  <clan> <clan>\r\n"
		           "   clan set privilege  <privilege>   <rank> <clan>\r\n"
		           "   clan set title  <clan number> <rank> <title>\r\n"
		           "   clan set board <board vnum> <clan>\r\n"
		           "   clan set recall <vnum of recall-to room> <clan>\r\n" );
	else
	{
		c = find_clan_by_id ( GET_CLAN ( ch ) );
		r = GET_CLAN_RANK ( ch );
		if ( r < 1 )
			ch->Send ( "   clan apply      <clan>\r\n" );
		if ( c >= 0 )
		{
			ch->Send ( "   clan deposit    <amount>\r\n" );
			if ( r >= clan[c].privilege[CP_WITHDRAW] )
				ch->Send ( "   clan withdraw   <amount>\r\n" );
			if ( r >= clan[c].privilege[CP_ENROLL] )
				ch->Send ( "   clan enroll     <player>\r\n" );
			if ( r >= clan[c].privilege[CP_EXPEL] )
				ch->Send ( "   clan expel      <player>\r\n" );
			if ( r >= clan[c].privilege[CP_PROMOTE] )
				ch->Send ( "   clan promote    <player>\r\n" );
			if ( r >= clan[c].privilege[CP_DEMOTE] )
				ch->Send ( "   clan demote     <player>\r\n" );
			if ( r >= clan[c].privilege[CP_SET_APPLEV] )
				ch->Send ( "   clan set applev <level>\r\n" );
			if ( r >= clan[c].privilege[CP_SET_FEES] )
				ch->Send ( "   clan set appfee <amount>\r\n"
				           "   clan set dues   <amount>\r\n" );
			if ( r >= clan[c].privilege[CP_SET_WAR] )
				ch->Send ( "   clan set peace <clan>\r\n"
				           "   clan set war   <clan>\r\n" );
			if ( r >= clan[c].privilege[CP_SET_PLAN] )
				ch->Send ( "   clan set plan\r\n" );
			if ( r == clan[c].ranks )
				ch->Send ( "   clan set ranks  <rank>\r\n"
				           "   clan set title  <rank> <title>\r\n"
				           "   clan set privilege  <privilege> <rank>\r\n" );

		}
	}
}

/* clan code */
Character *is_playing ( char *vict_name )
{
	extern Descriptor *descriptor_list;
	Descriptor *i, *next_i;

	for ( i = descriptor_list; i; i = next_i )
	{
		next_i = i->next;
		if ( IS_PLAYING ( i )
		        && !str_cmp ( i->character->player.name, vict_name ) )
			return i->character;
	}
	return NULL;
}

void do_clan_create ( Character *ch, char *arg )
{
	Character *leader = NULL;
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
	int new_id = 0, i;

	if ( !*arg )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
	{
		ch->Send ( "You are not mighty enough to create new clans!\r\n" );
		return;
	}

	if ( num_of_clans == MAX_CLANS )
	{
		ch->Send ( "Max clans reached. WOW!\r\n" );
		return;
	}

	half_chop ( arg, arg1, arg2 );

	if ( ! ( leader = get_char_vis ( ch, arg1, NULL, FIND_CHAR_WORLD ) ) )
	{
		ch->Send ( "The leader of the new clan must be present.\r\n" );
		return;
	}

	if ( strlen ( arg2 ) >= 32 )
	{
		ch->Send ( "Clan name too long! (32 characters max)\r\n" );
		return;
	}
	/*
	    if(GET_LEVEL(leader)>=LVL_GOD) {
	        send_to_char("You cannot set an immortal as the leader of a clan.\r\n",ch);
	        return; }
	*/
	if ( GET_CLAN ( leader ) != 0 && GET_CLAN_RANK ( leader ) != 0 )
	{
		ch->Send ( "The leader already belongs to a clan!\r\n" );
		return;
	}

	if ( find_clan ( arg2 ) != -1 )
	{
		ch->Send ( "That clan name alread exists!\r\n" );
		return;
	}

	strncpy ( clan[num_of_clans].name, CAP ( ( char * ) arg2 ), 32 );
	for ( i = 0; i < num_of_clans; i++ )
		if ( new_id < clan[i].id )
			new_id = clan[i].id;
	clan[num_of_clans].id = new_id + 1;
	clan[num_of_clans].ranks = 2;
	strcpy ( clan[num_of_clans].rank_name[0], "Member" );
	strcpy ( clan[num_of_clans].rank_name[1], "Leader" );
	clan[num_of_clans].treasure = 0;
	clan[num_of_clans].members = 1;
	clan[num_of_clans].power = GET_LEVEL ( leader );
	clan[num_of_clans].app_fee = 0;
	clan[num_of_clans].dues = 0;
	clan[num_of_clans].app_level = DEFAULT_APP_LVL;
	clan[num_of_clans].recall = NOWHERE;
	clan[num_of_clans].board = NOTHING;
	for ( i = 0; i < MAX_CLAN_SPELLS; i++ )
		clan[num_of_clans].spells[i] = TYPE_UNDEFINED;
	for ( i = 0; i < NUM_CLAN_PRIVILEGE; i++ )
		clan[num_of_clans].privilege[i] = clan[num_of_clans].ranks;
	for ( i = 0; i < NUM_AT_CLAN_WAR; i++ )
		clan[num_of_clans].at_war[i] = 0;
	for ( i = 0; i < NUM_CLAN_EQ; i++ )
		clan[num_of_clans].clan_eq[i] = NOTHING;
	num_of_clans++;
	save_clans();
	ch->Send ( "Clan created\r\n" );
	GET_CLAN ( leader ) = clan[num_of_clans - 1].id;
	GET_CLAN_RANK ( leader ) = clan[num_of_clans - 1].ranks;
	leader->save();

	return;
}
void do_clan_recall ( Character *ch, char *arg )
{
	obj_vnum brd;
	int clan_num = -1;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}


	if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
	{
		ch->Send ( "You do not have that clan privilege.\r\n" );
		return;
	}
	two_arguments ( arg, arg1, arg2 );
	if ( !*arg2 && !*arg2 && ( clan_num = find_clan ( arg2 ) ) < 0 )
	{
		ch->Send ( "Unknown clan.\r\n" );
		return;
	}


	if ( ! ( *arg1 ) )
	{
		ch->Send ( "clan set recall <VNUM> <clan>\r\n" );
		return;
	}

	if ( !is_number ( arg1 ) )
	{
		ch->Send ( "That isnt a vnum!\r\n" );
		return;
	}

	brd = atoi ( arg1 );

	if ( brd == clan[clan_num].recall )
	{
		ch->Send ( "The clan already has this as its recall point.\r\n" );
		return;
	}

	clan[clan_num].recall = brd;

	ch->Send ( "Done.\r\n" );

	save_clans();
	return;


}
void do_clan_board ( Character *ch, char *arg )
{
	obj_vnum brd;
	int clan_num = -1;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}


	if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
	{
		ch->Send ( "You do not have that clan privilege.\r\n" );
		return;
	}
	two_arguments ( arg, arg1, arg2 );
	if ( *arg2 )
	{
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}
	else
	{
		ch->Send ( "clan set board <VNUM> <clan>\r\n" );
		return;
	}


	if ( ! ( *arg1 ) )
	{
		ch->Send ( "clan set board <VNUM> <clan>\r\n" );
		return;
	}

	if ( !is_number ( arg1 ) )
	{
		ch->Send ( "That isnt a vnum!\r\n" );
		return;
	}

	brd = atoi ( arg1 );

	if ( brd == clan[clan_num].board )
	{
		ch->Send ( "The clan already has this as its board.\r\n" );
		return;
	}

	clan[clan_num].board = brd;

	ch->Send ( "Done.\r\n" );

	save_clans();
	return;


}

void do_clan_destroy ( Character *ch, char *arg )
{

	int i, j;
	Character *victim = NULL, *cbuf = NULL;

	if ( !*arg )
	{
		send_clan_format ( ch );
		return;
	}

	if ( ( i = find_clan ( arg ) ) < 0 )
	{
		ch->Send ( "Unknown clan.\r\n" );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
	{
		ch->Send ( "Your not mighty enough to destroy clans!\r\n" );
		return;
	}

	for ( j = 0; j < pi.Size(); j++ )
	{
		if ( ( victim = is_playing ( pi.NameByIndex ( j ) ) ) != NULL )
		{
			if ( GET_CLAN ( victim ) == clan[i].id )
			{
				GET_CLAN ( victim ) = 0;
				GET_CLAN_RANK ( victim ) = 0;
				victim->save();
			}
		}
		else
		{
			cbuf = new Character ( FALSE );
			if ( pi.LoadChar ( pi.NameByIndex ( j ), cbuf ) >= 0 )
			{
				if ( GET_CLAN ( cbuf ) == clan[i].id )
				{
					GET_CLAN ( cbuf ) = 0;
					GET_CLAN_RANK ( cbuf ) = 0;
					cbuf->save();
				}
			}
			delete ( victim );
		}
	}

	clan.erase ( ( clan.begin() +i ) );
	ch->Send ( "Clan deleted.\r\n" );
	save_clans();
	return;
}

void do_clan_enroll ( Character *ch, char *arg )
{
	Character *vict = NULL;
	int clan_num, immcom = 0;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_ENROLL] && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( vict = get_char_room_vis ( ch, arg, NULL ) ) )
	{
		ch->Send ( "Er, Who ??\r\n" );
		return;
	}
	else
	{
		if ( GET_CLAN ( vict ) != clan[clan_num].id )
		{
			if ( GET_CLAN_RANK ( vict ) > 0 )
			{
				ch->Send ( "They're already in a clan.\r\n" );
				return;
			}
			else
			{
				ch->Send ( "They didn't request to join your clan.\r\n" );
				return;
			}
		}
		else if ( GET_CLAN_RANK ( vict ) > 0 )
		{
			ch->Send ( "They're already in your clan.\r\n" );
			return;
		}
		if ( GET_LEVEL ( vict ) >= LVL_GOD )
		{
			ch->Send ( "You cannot enroll immortals in clans.\r\n" );
			return;
		}
	}

	GET_CLAN_RANK ( vict ) ++;
	vict->save();
	clan[clan_num].power += GET_LEVEL ( vict );
	clan[clan_num].members++;
	vict->Send ( "You've been enrolled in the clan you chose!\r\n" );
	ch->Send ( "Done.\r\n" );

	return;
}

void do_clan_retire (Character *ch, char *arg)
{
  int clan_num;
  char buf[MAX_STRING_LENGTH];
  
  if (!*arg || str_cmp(arg, "yes")) {
      ch->Send("You must type <clan retire yes> to confirm you want to retire.\r\n");
      return;
  }

  if ((clan_num = find_clan_by_id(GET_CLAN(ch)) < 0)) {
      ch->Send("You are not in any clan!\r\n");
      return;
  }

  if (PRF_FLAGGED(ch, PRF_RETIRED)) {
      ch->Send("You old fool, you are already retired!\r\n");
      return;
  }

  if ( GET_CLAN_RANK ( ch ) < clan[clan_num].ranks ) {
      ch->Send("Only clan leaders can retire.\r\n");
      return;
  }

  SET_BIT_AR(PRF_FLAGS(ch), PRF_RETIRED);
  GET_CLAN_RANK(ch)--;
  sprintf(buf, "{cMI have now officially retired as clan leader.{cx");
  do_ctell(ch, buf, 0, 0);

}

void do_clan_leave (Character *ch, char *arg)
{
  struct obj_data *obj;
  bool found = FALSE;
  int clan_num;
  struct affected_type af;

  if (!*arg || str_cmp(arg, "yes")) {
      ch->Send("You must type <clan leave yes> to confirm you want to leave.\r\n");
      return;
  }

  if ((clan_num = find_clan_by_id(GET_CLAN(ch)) < 0)) {
      ch->Send("You are not in any clan!\r\n");
      return;
  }
  
  for (obj = ch->carrying; obj; obj = obj->next_content)
      if (GET_OBJ_VNUM(obj) == 3302) {
          obj_from_char(obj);
          extract_obj(obj);
          found = TRUE;
      }

  if (!found) {
      ch->Send("You need to be carrying a silver token to leave your clan.\r\n");
      return;
  }

  clan[clan_num].members--;
  clan[clan_num].power -= GET_LEVEL ( ch );
  remove_clan_member ( GET_NAME ( ch ), clan_num );
  GET_CLAN ( ch ) = 0;
  GET_CLAN_RANK ( ch ) = 0;

  af.type = SPELL_RESERVE;
  af.expire = 60*24*14;  // two RL weeks 
  af.modifier = 0;
  af.location = APPLY_NONE;
  af.bitvector = AFF_OUTCAST;
  affect_to_char(ch, &af);
  ch->save();
  ch->Send("You have now left the clan!\r\n");

}
void do_clan_expel ( Character *ch, char *arg, int type )
{
  Character *vict = NULL;
  Descriptor *d = NULL;
  int clan_num = 0;
  char buf[MAX_INPUT_LENGTH];
  struct affected_type af;

  arg = one_argument(arg, buf);

  if ( buf[0] == '\0' )
  {
      send_clan_format ( ch );
      return;
  }

  if ( GET_LEVEL ( ch ) < LVL_GOD )
  {
      if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
      {
          ch->Send ( "You don't belong to any clan!\r\n" );
	  return;
      }
      if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[type]) {
          ch->Send("You are not high enough ranked to have that command!\r\n");
          return;
      }
  }

  /* Lets check if player is online */
  for (d = descriptor_list; d; d = d->next) 
    if (!str_cmp(d->character->player.name, buf)) {
        vict = d->character;
        break;
    }

  /* player isnt online, so lets check the file */
  if (!d) {
      vict = new Character(FALSE);
      if (store_to_char(buf, vict) == -1) {
        ch->Send("Player does not exist.\r\n");
        delete(vict);
        return;
      }
  }

  if ((clan_num = find_clan_by_id(GET_CLAN(vict))  < 0)) {
      ch->Send("That player does not belong in any clan.\r\n");
      if (!d)
          delete(vict);
      return;
  }

  if (GET_LEVEL(ch) < LVL_GOD) {
      if (GET_CLAN(vict) != GET_CLAN(ch)) {
          ch->Send("That player does not belong in your clan.\r\n");
          if (!d)
              delete(vict);
          return;
      }
      if (GET_CLAN_RANK(ch) < GET_CLAN_RANK(vict)) {
          act("Your rank is below $N!", FALSE, ch, 0, vict, TO_CHAR);
          if (!d)
              delete(vict);
          return;
      }
  }

  if (type == CP_EXPEL) {
      if (arg[0] == '\0' || str_cmp(arg, "outcast") || str_cmp(arg, "noflag")) {
          ch->Send("Format: clan expel <player name> <outcast/noflag>\r\n");
          return;
      }
      clan[clan_num].members--;
      clan[clan_num].power -= GET_LEVEL ( vict );
      remove_clan_member ( GET_NAME ( vict ), clan_num );
      GET_CLAN ( vict ) = 0;
      GET_CLAN_RANK ( vict ) = 0;
      if (!str_cmp(arg, "outcast")) {
          af.type = SPELL_RESERVE;
          af.expire = 60*24*14;  // two RL weeks 
          af.modifier = 0;
          af.location = APPLY_NONE;
          af.bitvector = AFF_OUTCAST;
          affect_to_char(ch, &af);
      }
      act("You have kicked $N out of your clan!", FALSE, ch, 0, vict, TO_CHAR);
      if (d)
          vict->Send("You've been kicked out of your clan!\r\n");
  }
  else if (type == CP_PROMOTE) {
      if (GET_CLAN_RANK(vict) + 1 > GET_CLAN_RANK(ch)) {
          act("You cannot promote $N above your rank!", FALSE, ch, 0, vict, TO_CHAR);
          if (!d)
              delete(vict);
          return;
      }
      if ( GET_CLAN_RANK ( vict ) == clan[clan_num].ranks ) {
          act("$n cannot be promoted above the highest rank!", FALSE, ch, 0, vict, TO_CHAR);
          if (!d)
              delete(vict);
          return;
      }
      GET_CLAN_RANK ( vict ) ++;
      update_clan_member ( GET_NAME ( vict ), GET_CLAN_RANK ( vict ), clan_num );
      if (d)
          vict->Send("You have been promoted in your clan!\r\n");
  }
  else if (type == CP_DEMOTE) {
      if (GET_CLAN_RANK(vict) == GET_CLAN_RANK(ch)) {
          ch->Send("You cannot demote someone of equal rank!\r\n");
          if (!d)
              delete(vict);
          return;
      }
      if (GET_CLAN_RANK(vict) == 1) {
         act("$N cannot be demoted any further.", FALSE, ch, 0, vict, TO_CHAR);
          if (!d)
              delete(vict);
          return;
      }  
      GET_CLAN_RANK ( vict )--;
      update_clan_member ( GET_NAME ( vict ), GET_CLAN_RANK ( vict ), clan_num );
  }
  vict->save();
  if (!d) 
      delete(vict);

  return;
}

int at_war ( int cln, int war_clan )
{

	return 1;
}

void declare_war ( int cln, int war_clan ) {}

void do_clan_war ( Character *ch, char *arg )
{
	//    Character *vict = NULL;
	int clan_num, immcom = 0, war_num = 0;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_SET_WAR] && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	war_num = find_clan ( arg );
	if ( at_war ( clan_num, war_num ) )
	{
		ch->Send ( "You're already AT WAR with %s!\r\n", clan[war_num].name );
		return;
	}

	declare_war ( clan_num, war_num );

	send_to_all ( "{cRThe %s have declared war on the %s!{c0",clan[clan_num].name,clan[war_num].name );
	ch->Send ( "Done.\r\n" );

	return;
}


void do_clan_demote ( Character *ch, char *arg )
{
	Character *vict = NULL;
	int clan_num, immcom = 0;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_DEMOTE] && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( vict = get_char_room_vis ( ch, arg, NULL ) ) )
	{
		ch->Send ( "Er, Who ??\r\n" );
		return;
	}
	else
	{
		if ( GET_CLAN ( vict ) != clan[clan_num].id )
		{
			ch->Send ( "They're not in your clan.\r\n" );
			return;
		}
		else
		{
			if ( GET_CLAN_RANK ( vict ) == 1 )
			{
				ch->Send ( "They can't be demoted any further, use expel now.\r\n" );
				return;
			}
			if ( GET_CLAN_RANK ( vict ) >= GET_CLAN_RANK ( ch ) && vict != ch && !immcom )
			{
				ch->Send ( "You cannot demote a person of this rank!\r\n" );
				return;
			}
		}
	}

	GET_CLAN_RANK ( vict )--;
	update_clan_member ( GET_NAME ( vict ), GET_CLAN_RANK ( vict ), clan_num );
	vict->save();
	vict->Send ( "You've been demoted within your clan!\r\n" );
	ch->Send ( "Done.\r\n" );
	return;
}

void do_clan_promote ( Character *ch, char *arg )
{
	Character *vict = NULL;
	int clan_num, immcom = 0;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_PROMOTE]
	        && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( vict = get_char_room_vis ( ch, arg, NULL ) ) )
	{
		ch->Send ( "Er, Who ??\r\n" );
		return;
	}
	else
	{
		if ( GET_CLAN ( vict ) != clan[clan_num].id )
		{
			ch->Send ( "They're not in your clan.\r\n" );
			return;
		}
		else
		{
			if ( GET_CLAN_RANK ( vict ) == 0 )
			{
				ch->Send ( "They're not enrolled yet.\r\n" );
				return;
			}
			if ( ( GET_CLAN_RANK ( vict ) + 1 ) > GET_CLAN_RANK ( ch ) && !immcom )
			{
				ch->Send ( "You cannot promote that person over your rank!\r\n" );
				return;
			}
			if ( GET_CLAN_RANK ( vict ) == clan[clan_num].ranks )
			{
				ch->Send ( "You cannot promote someone over the top rank!\r\n" );
				return;
			}
		}
	}

	GET_CLAN_RANK ( vict ) ++;
	update_clan_member ( GET_NAME ( vict ), GET_CLAN_RANK ( vict ), clan_num );
	vict->save();
	vict->Send ( "You've been promoted within your clan!\r\n" );
	ch->Send ( "Done.\r\n" );
	return;
}

void do_clan_who ( Character *ch )
{
	Descriptor *d;
	Character *tch;

	if ( GET_CLAN_RANK ( ch ) == 0 )
	{
		ch->Send ( "You do not belong to a clan!\r\n" );
		return;
	}

	ch->Send ( "\r\nList of your clan members\r\n" );
	ch->Send ( "-------------------------\r\n" );
	for ( d = descriptor_list; d; d = d->next )
	{
		if ( !IS_PLAYING ( d ) )
			continue;
		if ( ( tch = d->character ) )
		{
			if ( CAN_SEE ( ch, tch ) )
			{
				if ( GET_CLAN ( tch ) == GET_CLAN ( ch ) && GET_CLAN_RANK ( tch ) > 0 )
					ch->Send ( "%s\r\n", GET_NAME ( tch ) );
				if ( GET_CLAN ( tch ) == GET_CLAN ( ch ) && GET_CLAN_RANK ( tch ) == 0 )
					ch->Send ( "%s - Applicant\r\n", GET_NAME ( tch ) );
			}

		}

	}
	return;
}

void do_clan_status ( Character *ch )
{
	int clan_num;



	clan_num = find_clan_by_id ( GET_CLAN ( ch ) );

	if ( GET_CLAN_RANK ( ch ) == 0 )
	{
		if ( clan_num >= 0 )
		{
			ch->Send ( "You applied to %s\r\n",clan[clan_num].name );
			return;
		}
		else
		{
			ch->Send ( "You do not belong to a clan!\r\n" );
			return;
		}
	}

	ch->Send ( "You are %s (Rank %d) of %s (ID %d)\r\n",
	           clan[clan_num].rank_name[GET_CLAN_RANK ( ch ) - 1],
	           GET_CLAN_RANK ( ch ), clan[clan_num].name, clan[clan_num].id );

	return;
}

void do_clan_apply ( Character *ch, char *arg )
{
	int clan_num;

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}


	if ( GET_CLAN_RANK ( ch ) > 0 )
	{
		ch->Send ( "You already belong to a clan!\r\n" );
		return;
	}
	else
	{
		if ( ( clan_num = find_clan ( arg ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

        if (AFF_FLAGGED(ch, AFF_OUTCAST)) {
               ch->Send("Outcasts cannot join any clan!\r\n");
               return;
        }

	if ( GET_LEVEL ( ch ) < clan[clan_num].app_level )
	{
		ch->Send ( "You are not mighty enough to apply to this clan.\r\n" );
		return;
	}
	if ( clan[clan_num].app_fee < 0 )
	{
		ch->Send ( "That clan is far to exclusive for you!!\r\n" );
		return;
	}

	if ( ch->Gold ( 0, GOLD_ALL ) < ( clan[clan_num].app_fee ) )
	{
		ch->Send ( "You cannot afford the application fee!\r\n" );
		return;
	}

	ch->Gold ( -clan[clan_num].app_fee, GOLD_ALL ) ;
	clan[clan_num].treasure += clan[clan_num].app_fee;
	save_clans();
	GET_CLAN ( ch ) = clan[clan_num].id;
	ch->save();
	ch->Send ( "You've applied to the %s!\r\n", clan[clan_num].name );

	return;
}

void do_clan_info ( Character *ch, char *arg )
{
	int i = 0, j;

	if ( num_of_clans == 0 )
	{
		ch->Send ( "No clans have formed yet.\r\n" );
		return;
	}

	if ( ! ( *arg ) )
	{
		ch->Send ( "\r\n" );
		for ( i = 0; i < num_of_clans; i++ )
			ch->Send (
			    "[%-3d]  %-20s Id: %3d Members: %3d  Power: %5d  Appfee: %lld\r\n",
			    i, clan[i].name, clan[i].id, clan[i].members,
			    clan[i].power, clan[i].app_fee );
		return;
	}
	else if ( ( i = find_clan ( arg ) ) < 0 )
	{
		ch->Send ( "Unknown clan.\r\n" );
		return;
	}

	ch->Send (
	    "\r\n   <----------------[%-14s]--------------->\r\n"
	    "   O-----------------------------------------------O\r\n"
	    "   |    Ranks  : %-3d    |    Power   : %-5d       |\r\n"
	    "   |    Members: %-3d    |    Treasure: {cy%-10lld{c0  |\r\n"
	    "   O-----------------------------------------------O\r\n",
	    clan[i].name, clan[i].ranks, clan[i].power,
	    clan[i].members, clan[i].treasure );
	if ( 0 )
	{
		ch->Send (
		    "   |         Points          |      Tokens         |\r\n"
		    "   |   Quartz 0  Amethyst 0  |  Brass 0 Bronze 0   |\r\n"
		    "   | Sapphire 0      Ruby 0  | Silver 0   Gold 0   |\r\n"
		    "   O-----------------------------------------------O\r\n" );
	}

	ch->Send (
	    "   |    Enroll   : %-3d  |    Expel       : %-3d     |\r\n"
	    "   |    Promote  : %-3d  |    Demote      : %-3d     |\r\n"
	    "   |    Setplan  : %-3d  |    Setfees     : %-3d     |\r\n"
	    "   |    Withdraw : %-3d  |    Setapplev   : %-3d     |\r\n"
	    "   O-----------------------------------------------O\r\n",
	    clan[i].privilege[1], clan[i].privilege[2],
	    clan[i].privilege[3], clan[i].privilege[4],
	    clan[i].privilege[0], clan[i].privilege[5],
	    clan[i].privilege[6], clan[i].privilege[7] );

	ch->Send (
	    "   |    Application Fee  : {cc%-10lld {c0             |\r\n"
	    "   |    Monthly Dues     : {cc%-10lld{c0              |\r\n"
	    "   |    Application level: {cc%-3d{c0                     |\r\n"
	    "   O-----------------------------------------------O\r\n",
	    clan[i].app_fee, clan[i].dues, clan[i].app_level );
	if ( ( clan[i].at_war[0] == 0 ) && ( clan[i].at_war[1] == 0 )
	        && ( clan[i].at_war[2] == 0 ) && ( clan[i].at_war[3] == 0 ) )
		send_to_char
		( "   [     This clan is at peace with all others.    ]\r\n",
		  ch );
	else
		send_to_char
		( "   [             This clan is at war.              ]\r\n",
		  ch );
	ch->Send ( "   <-------------------[Titles]-------------------->\r\n" );

	for ( j = 0; j < clan[i].ranks; j++ )
	{
		ch->Send ( "({cg%3d{c0 - {cy%-20s{c0)%s", j + 1,
		           clan[i].rank_name[j], ( j % 2 ? "\r\n" : " " ) );
	};

	return;
}


sh_int find_clan_by_id ( int idnum )
{
	int i;
	for ( i = 0; i < num_of_clans; i++ )
		if ( idnum == clan[i].id )
			return i;
	return -1;
}

char *clan_name ( int idnum )
{
	if ( idnum > num_of_clans || idnum < 0 )
		return ( char * ) "none";

	return clan[idnum].name;
}

sh_int find_clan ( char *name )
{
	int i;
	for ( i = 0; i < num_of_clans; i++ )
		if ( is_abbrev ( name, clan[i].name ) )
			return i;
	return -1;
}

void init_clan_index ( void )
{


	FILE *index_file;
	num_of_clans = 0;
	if ( ( index_file = fopen ( CLAN_INDEX_FILE, "r" ) ) == NULL )
		log ( "SYSERR: Can't read from '%s' clan index file.", CLAN_INDEX_FILE );
	else
	{
		fscanf ( index_file, "%d\n", &num_of_clans );
		fclose ( index_file );
	}

}

void save_clan_index ( void )
{

	FILE *index_file;

	if ( ! ( index_file = fopen ( CLAN_INDEX_FILE, "w" ) ) )
	{
		log ( "SYSERR:  Could not write clan index file" );
		ALERT_2;
		return;
	}
	else
	{
		fprintf ( index_file, "%d\n", num_of_clans );
		fclose ( index_file );
	}
}

void save_clans ( void )
{
	int i;
	for ( i = 0; i < num_of_clans; i++ )
		clan_to_store ( i );

	save_clan_index();
}

void new_init_clans ( void )
{
	int i;
	for ( i = 0; i < num_of_clans; i++ )
		store_to_clan ( i );

}

void clan_to_store ( int i )
{
	FILE *fl;
	char outname[40], buf[MAX_STRING_LENGTH];
	int thing = 0;


	snprintf ( outname, sizeof ( outname ), "%s/clan.%d", CLAN_DIR, i );
	if ( ! ( fl = fopen ( outname, "w" ) ) )
	{
		new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: Couldn't open clan file %s for write",outname );
		return;
	}


	fprintf ( fl, "Name: %s\n", clan[i].name );
	fprintf ( fl, "Id  : %d\n", clan[i].id );
	fprintf ( fl, "Rnks: %d\n", clan[i].ranks );
	fprintf ( fl, "RnkN:\n" );
	for ( thing = 0; thing < clan[i].ranks; thing++ )
	{
		fprintf ( fl, "%d: %-.20s\n", thing, clan[i].rank_name[thing] );
	}
	fprintf ( fl, "-1: none\n" );

	fprintf ( fl, "Tres: %lld\n", clan[i].treasure );
	fprintf ( fl, "Memb: %d\n", clan[i].members );
	fprintf ( fl, "Powr: %d\n", clan[i].power );
	fprintf ( fl, "Appf: %lld\n", clan[i].app_fee );
	fprintf ( fl, "Dues: %lld\n", clan[i].dues );
	fprintf ( fl, "Reca: %d\n", clan[i].recall );
	fprintf ( fl, "Bord: %d\n", clan[i].board );
	fprintf ( fl, "Spel:\n" );
	for ( thing = 0; thing < MAX_CLAN_SPELLS; thing++ )
		fprintf ( fl, "%d %d\n", thing, clan[i].spells[thing] );
	fprintf ( fl, "-1 -1\n" );
	fprintf ( fl, "ALev: %d\n", clan[i].app_level );
	fprintf ( fl, "Priv:\n" );
	for ( thing = 0; thing < NUM_CLAN_PRIVILEGE; thing++ )
		fprintf ( fl, "%d %d\n", thing, clan[i].privilege[thing] );
	fprintf ( fl, "-1 -1\n" );

	fprintf ( fl, "War :\n" );
	for ( thing = 0; thing < NUM_AT_CLAN_WAR; thing++ )
		fprintf ( fl, "%d %d\n", thing, clan[i].at_war[thing] );
	fprintf ( fl, "-1 -1\n" );
	fprintf ( fl, "Eq  :\n" );
	for ( thing = 0; thing < NUM_CLAN_EQ; thing++ )
		fprintf ( fl, "%d %d\n", thing, clan[i].clan_eq[thing] );
	fprintf ( fl, "-1 -1\n" );
	if ( clan[i].description && *clan[i].description )
	{
		strcpy ( buf, clan[i].description );
		strip_cr ( buf );
		smash_tilde ( buf );
		fprintf ( fl, "Desc:\n%s~\n", buf );
	}

	fclose ( fl );
}

int store_to_clan ( int i )
{
	int cnt, num = 0, num2 = 0;
	gold_int num6 = 0;
	FILE *fl;
	char filename[40];
	char buffer[128], line[MAX_INPUT_LENGTH + 1], tag[6];
	char buf2[MAX_INPUT_LENGTH];

	sprintf ( filename, "%s/clan.%d", CLAN_DIR, i );
	if ( ! ( fl = fopen ( filename, "r" ) ) )
	{
		new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: Couldn't open clan file %s for read",filename );
		return -1;
	}


	clan[i].id = 0;
	clan[i].name[0] = 0;
	clan[i].ranks = 0;
	for ( cnt =0 ; cnt < 20; cnt++ )
		clan[i].rank_name[cnt][0] = 0;
	clan[i].treasure = 0;
	clan[i].members = 0;
	clan[i].power = 0;
	clan[i].app_fee = 0;
	clan[i].dues = 0;
	for ( cnt =0 ; cnt < MAX_CLAN_SPELLS; cnt++ )
		clan[i].spells[cnt] = TYPE_UNDEFINED;
	clan[i].app_level = LVL_IMPL;
	for ( cnt =0 ; cnt < NUM_CLAN_PRIVILEGE; cnt++ )
		clan[i].privilege[cnt] = 0;
	for ( cnt =0 ; cnt < NUM_AT_CLAN_WAR; cnt++ )
		clan[i].at_war[cnt] = 0;
	for ( cnt =0 ; cnt < NUM_CLAN_EQ; cnt++ )
		clan[i].clan_eq[cnt] = NOTHING;
	clan[i].description[0] = 0;
	clan[i].board = NOTHING;
	clan[i].recall = NOWHERE;

	while ( get_line ( fl, line ) )
	{
		tag_argument ( line, tag );
		num = atoi ( line );
		num6 = atoll ( line );


		switch ( *tag )
		{
			case 'A':
				if ( !strcmp ( tag, "Appf" ) )
					clan[i].app_fee = num6;
				else if ( !strcmp ( tag, "ALev" ) )
					clan[i].app_level = num;
				break;

			case 'B':
				if ( !strcmp ( tag, "Bord" ) )
					clan[i].board = num;
				break;

			case 'D':
				if ( !strcmp ( tag, "Desc" ) )
				{
					char *tmpd;

					if ( ( tmpd = fread_string ( fl, buf2 ) ) == NULL )
						tmpd = strdup ( "Undefined" );

					strcpy ( clan[i].description, tmpd );
					free_string ( &tmpd );
				}
				else if ( !strcmp ( tag, "Dues" ) )
					clan[i].dues = num;
				break;
			case 'E':
				if ( !strcmp ( tag, "Eq  " ) )
				{
					do
					{
						get_line ( fl, line );
						sscanf ( line, "%d %d", &num, &num2 );
						if ( num != -1 )
						{
							clan[i].clan_eq[num] = ( num2 );
						}
					}
					while ( num != -1 );
				}
				break;
			case 'I':
				if ( !strcmp ( tag, "Id  " ) )
					clan[i].id = num;
				break;

			case 'M':
				if ( !strcmp ( tag, "Memb" ) )
				{
					clan[i].members = num;
					break;
				case 'N':
					if ( !strcmp ( tag, "Name" ) )
						strcpy ( clan[i].name, line );
					break;

				case 'P':
					if ( !strcmp ( tag, "Powr" ) )
						clan[i].power = num;
					else if ( !strcmp ( tag, "Priv" ) )
					{
						do
						{
							get_line ( fl, line );
							sscanf ( line, "%d %d", &num, &num2 );
							if ( num != -1 )
							{
								clan[i].privilege[num] = num2;
							}
						}
						while ( num != -1 );
					}

					break;

				case 'R':


					if ( !strcmp ( tag, "Rnks" ) )
						clan[i].ranks = num;
					else if ( !strcmp ( tag, "Reca" ) )
						clan[i].recall = num;
					else if ( !strcmp ( tag, "RnkN" ) )
					{
						do
						{
							get_line ( fl, line );
							sscanf ( line, "%d: %20[^\f\n\r\t\v]", &num, ( char * ) &buffer );
							if ( num != -1 )
							{
								strcpy ( clan[i].rank_name[num],  buffer );
							}
						}
						while ( num != -1 );
					}
					break;

				case 'S':
					if ( !strcmp ( tag, "Spel" ) )
					{
						do
						{
							get_line ( fl, line );
							sscanf ( line, "%d %d", &num, &num2 );
							if ( num != -1 )
							{
								clan[i].spells[num]= ( num2 );
							}
						}
						while ( num != -1 );
					}
					break;

				case 'T':
					if ( !strcmp ( tag, "Tres" ) )
						clan[i].treasure = num6;
					break;

				case 'W':
					if ( !strcmp ( tag, "War " ) )
					{
						do
						{
							get_line ( fl, line );
							sscanf ( line, "%d %d", &num, &num2 );
							if ( num != -1 )
							{
								clan[i].at_war[num] = ( num2 );
							}
						}
						while ( num != -1 );
					}
					break;

				default:
					sprintf ( buffer, "SYSERR: Unknown tag %s in clan %d", tag, i );
				}
		}
	}

	fclose ( fl );
	return 1;
}



void init_clans()
{
	FILE *fl;
	int i, j;
	clan_rec tmp;

	init_clan_index();

	//memset(clan, 0, sizeof(struct clan_rec) * MAX_CLANS);
	if ( num_of_clans == 0 )
	{
		i = 0;

		if ( ! ( fl = fopen ( CLAN_FILE, "rb" ) ) )
		{
			log ( "   Clan file does not exist. Will create a new one" );
			save_clans();
			return;
		}

		fread ( &num_of_clans, sizeof ( int ), 1, fl );
		for ( int k = 0; k< num_of_clans;k++ )
		{
			fread ( &tmp, sizeof ( struct clan_rec ), 1, fl );
			clan.push_back ( tmp );
		}
		fclose ( fl );
	}
	else
	{
		new_init_clans();
	}
	save_clans();

	log ( "   Calculating powers and members" );
	for ( i = 0; i < num_of_clans; i++ )
	{
		clan[i].power = 0;
		clan[i].members = 0;
	}

	for ( j = 0; j <= pi.TopOfTable(); j++ )
	{
		if ( pi.DeletedByIndex ( j ) )
			continue;
		if ( pi.NameByIndex ( j ) && *pi.NameByIndex ( j ) &&
		        pi.RankByIndex ( j ) > 0 &&
		        ( i = find_clan_by_id ( pi.ClanByIndex ( j ) ) ) >= 0 )
		{
			add_clan_member ( pi.NameByIndex ( j ), pi.RankByIndex ( j ), i );
			clan[i].power += pi.LevelByIndex ( j );
			clan[i].members++;
		}

	}
	return;
}

/*
void do_clan_list(Character *ch, char *arg)
{
  send_to_char("This command has been temporarily disabled.\r\n", ch);
  return;
}



*/

void do_clan_list ( Character *ch, char *arg )
{
	int i;
	char buf[MAX_STRING_LENGTH];
	vector<clan_list_data>::iterator temp;

	DYN_DEFINE;
	*buf = '\0';
	DYN_CREATE;
	*dynbuf = 0;

	if ( *arg && GET_LEVEL ( ch ) >= LVL_SEN )
		i = find_clan ( arg );
	else
		i = find_clan_by_id ( GET_CLAN ( ch ) );

	ch->Send ( "Members of the %s clan\r\n", clan_name ( i ) );
	ch->Send ( "-------------------------------\r\n" );


	if ( i==NOTHING || i > ( MAX_CLANS-1 ) )
	{
		ch->Send ( "You are not in a clan.\r\n" );
		return;
	}

	temp = clan_list[i].begin();
	while ( temp != clan_list[i].end() )
	{
		if ( ( *temp ).rank > 0 )
		{
			if ( clan[i].ranks == ( *temp ).rank )
				snprintf ( buf, sizeof ( buf ), "{cW%-20s -- Rank: %d. (Leader){c0\r\n", ( *temp ).name, ( *temp ).rank );
			else
				snprintf ( buf, sizeof ( buf ), "{cw%-20s -- Rank: %d.{c0\r\n", ( *temp ).name, ( *temp ).rank );
			DYN_RESIZE ( buf );
		}

		temp++;
	}



	page_string ( ch->desc, dynbuf, DYN_BUFFER );
	return;

}

void do_clan_bank ( Character *ch, char *arg, int action )
{
	int clan_num, immcom = 0;
	gold_int amount = 0;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_WITHDRAW]
	        && !immcom && action == CB_WITHDRAW ) )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( *arg ) )
	{
		ch->Send ( "Deposit how much?\r\n" );
		return;
	}

	if ( !is_number ( arg ) )
	{
		ch->Send ( "Deposit what?\r\n" );
		return;
	}

	amount = atoll ( arg );
	if ( amount < 0 )
	{
		ch->Send ( "Don't be silly.\r\n" );
		return;
	}

	if ( !immcom && action == CB_DEPOSIT && ch->Gold ( 0, GOLD_ALL ) < amount )
	{
		ch->Send ( "You do not have that kind of money!\r\n" );
		return;
	}

	if ( amount > 2000000000 && action == CB_DEPOSIT )
	{
		ch->Send ( "You can't deposit more then 2 billion coins at a time!\r\n" );
		return;
	}


	if ( action == CB_WITHDRAW && clan[clan_num].treasure < amount )
	{
		ch->Send ( "The clan is not wealthy enough for your needs!\r\n" );
		return;
	}
	if ( clan[clan_num].treasure < 0 )
	{
		ch->Send ( "The clan's account has been frozen because of abuse!!\r\n" );
		return;
	}

	if ( action == CB_WITHDRAW && ( clan[clan_num].treasure - amount ) < 0 )
	{
		ch->Send ( "That transaction can't take place!!\r\n" );
		return;
	}

	switch ( action )
	{
		case CB_WITHDRAW:
			ch->Gold ( amount, GOLD_HAND );
			clan[clan_num].treasure -= amount;
			ch->Send ( "You withdraw from the clan's treasure.\r\n" );
			break;
		case CB_DEPOSIT:
			if ( !immcom )
				ch->Gold ( -amount, GOLD_ALL );
			clan[clan_num].treasure += amount;
			ch->Send ( "You add to the clan's treasure.\r\n" );
			break;
		default:
			ch->Send ( "Problem in command, please report.\r\n" );
			break;
	}
	ch->save();
	save_clans();
	return;
}

void do_clan_money ( Character *ch, char *arg, int action )
{
	int clan_num, immcom = 0;
	long amount = 0;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_SET_FEES]
	        && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( *arg ) )
	{
		ch->Send ( "Set it to how much?\r\n" );
		return;
	}

	if ( !is_number ( arg ) )
	{
		ch->Send ( "Set it to what?\r\n" );
		return;
	}

	amount = atoll ( arg );

	switch ( action )
	{
		case CM_APPFEE:
			clan[clan_num].app_fee = amount;
			ch->Send ( "You change the application fee.\r\n" );
			break;
		case CM_DUES:
			clan[clan_num].dues = amount;
			ch->Send ( "You change the monthly dues.\r\n" );
			break;
		default:
			ch->Send ( "Problem in command, please report.\r\n" );
			break;
	}

	save_clans();
	return;
}

void do_clan_ranks ( Character *ch, char *arg )
{
	int i, j;
	int clan_num, immcom = 0;
	int new_ranks;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	Character *victim = NULL;

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) != clan[clan_num].ranks && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( *arg ) )
	{
		ch->Send ( "Set how many ranks?\r\n" );
		return;
	}

	if ( !is_number ( arg ) )
	{
		ch->Send ( "Set the ranks to what?\r\n" );
		return;
	}

	new_ranks = atoi ( arg );

	if ( new_ranks == clan[clan_num].ranks )
	{
		ch->Send ( "The clan already has this number of ranks.\r\n" );
		return;
	}

	if ( new_ranks < 2 || new_ranks > 20 )
	{
		ch->Send ( "Clans must have from 2 to 20 ranks.\r\n" );
		return;
	}

	if ( ch->Gold ( 0, GOLD_ALL ) < 7500000 && !immcom )
	{
		send_to_char
		( "Changing the clan hierarchy requires 7,500,000 coins!\r\n",
		  ch );
		return;
	}

	if ( !immcom )
		ch->Gold ( -7500000, GOLD_ALL );

	for ( j = 0; j < pi.Size(); j++ )
	{
		if ( ( victim = is_playing ( pi.NameByIndex ( j ) ) ) )
		{
			if ( GET_CLAN ( victim ) == clan[clan_num].id )
			{
				if ( GET_CLAN_RANK ( victim ) < clan[clan_num].ranks
				        && GET_CLAN_RANK ( victim ) > 0 )
					GET_CLAN_RANK ( victim ) = 1;
				if ( GET_CLAN_RANK ( victim ) == clan[clan_num].ranks )
					GET_CLAN_RANK ( victim ) = new_ranks;
				victim->save();
			}
		}
		else
		{
			victim = new Character ( FALSE );
			if ( pi.LoadChar ( pi.NameByIndex ( j ), victim ) >= 0 )
			{
				if ( GET_CLAN ( victim ) == clan[clan_num].id )
				{
					if ( GET_CLAN_RANK ( victim ) < clan[clan_num].ranks
					        && GET_CLAN_RANK ( victim ) > 0 )
						GET_CLAN_RANK ( victim ) = 1;
					if ( GET_CLAN_RANK ( victim ) == clan[clan_num].ranks )
						GET_CLAN_RANK ( victim ) = new_ranks;
					victim->save();
				}
			}
			delete ( victim );
		}
	}

	clan[clan_num].ranks = new_ranks;
	for ( i = 0; i < clan[clan_num].ranks - 1; i++ )
		strcpy ( clan[clan_num].rank_name[i], "Member" );
	strcpy ( clan[clan_num].rank_name[clan[clan_num].ranks - 1], "Leader" );
	for ( i = 0; i < NUM_CP; i++ )
		clan[clan_num].privilege[i] = new_ranks;

	save_clans();
	return;
}

void do_clan_titles ( Character *ch, char *arg )
{
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
	int clan_num = 0, rank;

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
		if ( GET_CLAN_RANK ( ch ) != clan[clan_num].ranks )
		{
			ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg2 );
		if ( !is_number ( arg1 ) )
		{
			ch->Send ( "You need to specify a clan number.\r\n" );
			return;
		}
		if ( ( clan_num = atoi ( arg1 ) ) < 0 || clan_num >= num_of_clans )
		{
			ch->Send ( "There is no clan with that number.\r\n" );
			return;
		}
	}

	half_chop ( arg, arg1, arg2 );

	if ( !is_number ( arg1 ) )
	{
		ch->Send ( "You need to specify a rank number.\r\n" );
		return;
	}

	rank = atoi ( arg1 );

	if ( rank < 1 || rank > clan[clan_num].ranks )
	{
		ch->Send ( "This clan has no such rank number.\r\n" );
		return;
	}

	if ( strlen ( arg2 ) < 1 || strlen ( arg2 ) > 19 )
	{
		ch->Send ( "You need a clan title of under 20 characters.\r\n" );
		return;
	}

	strcpy ( clan[clan_num].rank_name[rank - 1], arg2 );
	save_clans();
	ch->Send ( "Done.\r\n" );
	return;
}

void do_clan_application ( Character *ch, char *arg )
{
	int clan_num, immcom = 0;
	int applevel;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg2 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_SET_APPLEV]
	        && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( *arg ) )
	{
		ch->Send ( "Set to which level?\r\n" );
		return;
	}

	if ( !is_number ( arg ) )
	{
		ch->Send ( "Set the application level to what?\r\n" );
		return;
	}

	applevel = atoi ( arg );

	if ( applevel < 1 || applevel > 999 )
	{
		ch->Send ( "The application level can go from 1 to 999.\r\n" );
		return;
	}

	clan[clan_num].app_level = applevel;
	save_clans();

	return;
}

void do_clan_sp ( Character *ch, char *arg, int priv )
{
	int clan_num, immcom = 0;
	int rank;
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];

	if ( ! ( *arg ) )
	{
		send_clan_format ( ch );
		return;
	}


	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		immcom = 1;
		half_chop ( arg, arg1, arg2 );
		strcpy ( arg, arg1 );
		if ( ( clan_num = find_clan ( arg1 ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( GET_CLAN_RANK ( ch ) != clan[clan_num].ranks && !immcom )
	{
		ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
		return;
	}

	if ( ! ( *arg ) )
	{
		ch->Send ( "Set the privilege to which rank?\r\n" );
		return;
	}

	if ( !is_number ( arg ) )
	{
		ch->Send ( "Set the privilege to what?\r\n" );
		return;
	}

	rank = atoi ( arg );

	if ( rank < 1 || rank > clan[clan_num].ranks )
	{
		ch->Send ( "There is no such rank in the clan.\r\n" );
		return;
	}

	clan[clan_num].privilege[priv] = rank;
	save_clans();

	return;
}

void do_clan_plan ( Character *ch, char *arg )
{
	int clan_num;

	ch->Send ( "Command not ready yet\r\n" );
	return;

	if ( GET_LEVEL ( ch ) < LVL_GOD )
	{
		if ( ( clan_num = find_clan_by_id ( GET_CLAN ( ch ) ) ) < 0 )
		{
			ch->Send ( "You don't belong to any clan!\r\n" );
			return;
		}
		if ( GET_CLAN_RANK ( ch ) < clan[clan_num].privilege[CP_SET_PLAN] )
		{
			ch->Send ( "You're not influent enough in the clan to do that!\r\n" );
			return;
		}
	}
	else
	{
		if ( GET_LEVEL ( ch ) < LVL_CLAN_GOD )
		{
			ch->Send ( "You do not have clan privileges.\r\n" );
			return;
		}
		if ( ! ( *arg ) )
		{
			send_clan_format ( ch );
			return;
		}
		if ( ( clan_num = find_clan ( arg ) ) < 0 )
		{
			ch->Send ( "Unknown clan.\r\n" );
			return;
		}
	}

	if ( strlen ( clan[clan_num].description ) == 0 )
	{
		ch->Send ( "Enter the description, or plan for clan <<%s>>.\r\n", clan[clan_num].name );
	}
	else
	{
		ch->Send ( "Old plan for clan <<%s>>:\r\n", clan[clan_num].name );
		ch->Send ( "%s", clan[clan_num].description );
		ch->Send ( "Enter new plan:\r\n" );
	}
	ch->Send ( "End with @ on a line by itself.\r\n" );
	ch->desc->str = ( char ** ) clan[clan_num].description;
	ch->desc->max_str = CLAN_PLAN_LENGTH;
	save_clans();
	return;
}

void do_clan_privilege ( Character *ch, char *arg )
{
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
	int i;

	half_chop ( arg, arg1, arg2 );

	if ( is_abbrev ( arg1, "setplan" ) )
	{
		do_clan_sp ( ch, arg2, CP_SET_PLAN );
		return;
	}
	if ( is_abbrev ( arg1, "enroll" ) )
	{
		do_clan_sp ( ch, arg2, CP_ENROLL );
		return;
	}
	if ( is_abbrev ( arg1, "expel" ) )
	{
		do_clan_sp ( ch, arg2, CP_EXPEL );
		return;
	}
	if ( is_abbrev ( arg1, "promote" ) )
	{
		do_clan_sp ( ch, arg2, CP_PROMOTE );
		return;
	}
	if ( is_abbrev ( arg1, "demote" ) )
	{
		do_clan_sp ( ch, arg2, CP_DEMOTE );
		return;
	}
	if ( is_abbrev ( arg1, "withdraw" ) )
	{
		do_clan_sp ( ch, arg2, CP_WITHDRAW );
		return;
	}
	if ( is_abbrev ( arg1, "setfees" ) )
	{
		do_clan_sp ( ch, arg2, CP_SET_FEES );
		return;
	}
	if ( is_abbrev ( arg1, "setapplev" ) )
	{
		do_clan_sp ( ch, arg2, CP_SET_APPLEV );
		return;
	}
	ch->Send ( "\r\nClan privileges:\r\n" );
	for ( i = 0; i < NUM_CP; i++ )
		ch->Send ( "\t%s\r\n", clan_privileges[i] );

}

void do_clan_set ( Character *ch, char *arg )
{
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

	half_chop ( arg, arg1, arg2 );

	if ( is_abbrev ( arg1, "plan" ) )
	{
		do_clan_plan ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "ranks" ) )
	{
		do_clan_ranks ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "title" ) )
	{
		do_clan_titles ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "privilege" ) )
	{
		do_clan_privilege ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "dues" ) )
	{
		do_clan_money ( ch, arg2, CM_DUES );
		return;
	}
	if ( is_abbrev ( arg1, "appfee" ) )
	{
		do_clan_money ( ch, arg2, CM_APPFEE );
		return;
	}
	if ( is_abbrev ( arg1, "applev" ) )
	{
		do_clan_application ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "recall" ) )
	{
		do_clan_recall ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "board" ) )
	{
		do_clan_board ( ch, arg2 );
		return;
	}

	send_clan_format ( ch );
}

ACMD ( do_clan )
{
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

	half_chop ( argument, arg1, arg2 );

	if ( is_abbrev ( arg1, "create" ) )
	{
		do_clan_create ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "destroy" ) )
	{
		do_clan_destroy ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "enroll" ) )
	{
		do_clan_enroll ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "expel" ) )
	{
		do_clan_expel ( ch, arg2, CP_EXPEL );
		return;
	}
	if ( is_abbrev ( arg1, "who" ) )
	{
		do_clan_who ( ch );
		return;
	}
	if ( is_abbrev ( arg1, "status" ) )
	{
		do_clan_status ( ch );
		return;
	}
	if ( is_abbrev ( arg1, "info" ) )
	{
		do_clan_info ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "apply" ) )
	{
		do_clan_apply ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "demote" ) )
	{
		do_clan_expel ( ch, arg2, CP_DEMOTE );
		return;
	}
	if ( is_abbrev ( arg1, "promote" ) )
	{
		do_clan_expel ( ch, arg2, CP_PROMOTE );
		return;
	}
	if ( is_abbrev ( arg1, "set" ) )
	{
		do_clan_set ( ch, arg2 );
		return;
	}
	if ( is_abbrev ( arg1, "withdraw" ) )
	{
		do_clan_bank ( ch, arg2, CB_WITHDRAW );
		return;
	}
	if ( is_abbrev ( arg1, "deposit" ) )
	{
		do_clan_bank ( ch, arg2, CB_DEPOSIT );
		return;
	}
	if ( is_abbrev ( arg1, "list" ) )
	{
		do_clan_list ( ch, arg2 );
		return;
	}
        if (!str_cmp(arg1, "leave"))
        {
                do_clan_leave(ch, arg2);
                return;
        }
        if (!str_cmp(arg1, "retire"))
        {
                do_clan_retire(ch, arg2);
                return;
        }
	send_clan_format ( ch );
}
bool operator< ( const clan_list_data &a, const clan_list_data &b )
{
	return ( b.rank<a.rank );
}

void add_clan_member ( const char * name, int rank, int cln )
{
	struct clan_list_data temp;

	if ( ( name == NULL ) || ! ( *name ) )
		return;

	temp.name = strdup ( name );
	temp.name[0] = toupper ( temp.name[0] );
	temp.rank = rank;
	clan_list[cln].push_back ( temp );
	sort ( clan_list[cln].begin(), clan_list[cln].end() );
}

void remove_clan_member ( const char * name, int cln )
{
	vector<clan_list_data>::iterator find;
	if ( clan_list[cln].size() == 0 || !name || !*name )
		return;
	find = clan_list[cln].begin();
	while ( find != clan_list[cln].end() )
	{
		if ( !str_cmp ( ( *find ).name, name ) )
		{
			free ( ( *find ).name );
			clan_list[cln].erase ( find );
			remove_clan_member ( name, cln ); /* recursive, to avoid missmatched iterators */
			return;
		}
		find++;
	}

}

void update_clan_member ( const char * name, int rank, int cln )
{
	vector<clan_list_data>::iterator find;
	if ( clan_list[cln].size() == 0 || !name || !*name )
		return;
	find = clan_list[cln].begin();
	while ( find != clan_list[cln].end() )
	{
		if ( !str_cmp ( ( *find ).name, name ) )
			( *find ).rank = rank;
		find++;
	}

}

void free_clan_list ( vector<clan_list_data> &find )
{
	for ( vector<clan_list_data>::iterator it = find.begin(); it != find.end(); it++ )
	{
		free ( ( *it ).name );
		( *it ).name = NULL;
	}
}

void free_clan_lists ( void )
{
	int i;
	for ( i = 0; i < num_of_clans; i++ )
		free_clan_list ( clan_list[i] );

}



