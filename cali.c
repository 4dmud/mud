#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "house.h"
#include "screen.h"
#include "constants.h"
#include "oasis.h"
#include "dg_scripts.h"

/* external variables */
/*
if %actor.clanname% != saints
%send% %actor% Only saints can use this item.
return 0
halt
end
if %self.vnum(12424)% || %self.vnum(12417)%
%send% %actor% A saintly figure rushes down holding a large sac.
%echoaround% %actor% A tiny Angel flutters down, beaming a ray of light onto %actor.name%.
wait 5s
%send% %actor% Darkness surrounds you as the sac is thrown over your head.
%echoaround% %actor% Hovering above %actor.name% , the Angel gently lifts %actor.himher% up
%echoaround% %actor% and takes %actor.himher% away to a safe refuge.
%teleport% %actor% 12445
%send% %actor% Suddenly, you find yourself in a place WORSE than the pits of Hell!
%force% %actor% look
end
%purge% %self%

*/
void convert_tokens ( Character *ch )
{
	div_t brass, bronze, silver;

	/*
	  log("%s before conversion.", GET_NAME(ch));
	  log("brass: %5d, bronze: %5d, silver: %5d, gold: %5d.",
	      GET_BRASS_TOKEN_COUNT(ch), GET_BRONZE_TOKEN_COUNT(ch),
	      GET_SILVER_TOKEN_COUNT(ch), GET_GOLD_TOKEN_COUNT(ch));
	*/

	/* convert brass to bronze */
	brass = div ( GET_BRASS_TOKEN_COUNT ( ch ), 5 );
	GET_BRASS_TOKEN_COUNT ( ch ) = brass.rem;
	GET_BRONZE_TOKEN_COUNT ( ch ) += brass.quot;

	/* convert bronze to silver */
	bronze = div ( GET_BRONZE_TOKEN_COUNT ( ch ), 10 );
	GET_BRONZE_TOKEN_COUNT ( ch ) = bronze.rem;
	GET_SILVER_TOKEN_COUNT ( ch ) += bronze.quot;

	/* convert silver to gold */
	silver = div ( GET_SILVER_TOKEN_COUNT ( ch ), 10 );
	GET_SILVER_TOKEN_COUNT ( ch ) = silver.rem;
	GET_GOLD_TOKEN_COUNT ( ch ) += silver.quot;

	/*
	  log("%s after conversion.", GET_NAME(ch));
	  log("brass: %5d, bronze: %5d, silver: %5d, gold: %5d.",
	      GET_BRASS_TOKEN_COUNT(ch), GET_BRONZE_TOKEN_COUNT(ch),
	      GET_SILVER_TOKEN_COUNT(ch), GET_GOLD_TOKEN_COUNT(ch));
	*/

	return;
}

SPECIAL ( token_machine )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	struct obj_data *obj, *cont;
	Character *tmp_char;
//  int obj_dotmode, cont_dotmode, found = 0;

	if ( !CMD_IS ( "put" ) )
		return ( 0 );
	if ( IS_NPC ( ch ) )
		return 0;

	two_arguments ( argument, arg1, arg2 );

	if ( !*arg1 )
	{
		ch->Send ( "What do you want to put where?\r\n" );
		return ( 1 );
	}

	if ( !*arg2 )
	{
		ch->Send ( "What do you want to put it in?\r\n" );
		return ( 1 );
	}

	generic_find ( arg2, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &tmp_char, &cont );

	if ( !cont )
		return ( 0 );

	if ( GET_OBJ_VNUM ( cont ) != 219 )
		return ( 0 );

	if ( ! ( obj = get_obj_in_list_vis ( ch, arg1, NULL, ch->carrying ) ) )
	{
		ch->Send ( "You aren't carrying %s %s.\r\n", AN ( arg1 ), arg1 );
		return ( 1 );
	}
	else if ( ( GET_OBJ_VNUM ( obj ) == 3301 ) || ( GET_OBJ_VNUM ( obj ) == 3304 )
	          || ( GET_OBJ_VNUM ( obj ) == 3307 )
	          || ( GET_OBJ_VNUM ( obj ) == 3310 ) )
	{
		ch->Send ( "You put %s %s into the machine.\r\n", AN ( arg1 ),
		           arg1 );
		obj_from_char ( obj );
		extract_obj ( obj );
		GET_BRONZE_TOKEN_COUNT ( ch ) += 1;
		convert_tokens ( ch );
		return ( 1 );
	}
	else if ( GET_OBJ_VNUM ( obj ) == 3300 )
	{
		ch->Send ( "You put %s %s into the machine.\r\n", AN ( arg1 ),
		           arg1 );
		obj_from_char ( obj );
		extract_obj ( obj );
		GET_BRASS_TOKEN_COUNT ( ch ) += 1;
		convert_tokens ( ch );
		return ( 1 );
	}
	else if ( ( GET_OBJ_VNUM ( obj ) == 3302 ) || ( GET_OBJ_VNUM ( obj ) == 3305 )
	          || ( GET_OBJ_VNUM ( obj ) == 3308 )
	          || ( GET_OBJ_VNUM ( obj ) == 3311 ) )
	{
		ch->Send ( "You put %s %s into the machine.\r\n", AN ( arg1 ),
		           arg1 );
		obj_from_char ( obj );
		extract_obj ( obj );
		GET_SILVER_TOKEN_COUNT ( ch ) += 1;
		convert_tokens ( ch );
		return ( 1 );
	}
	else if ( ( GET_OBJ_VNUM ( obj ) == 3303 ) || ( GET_OBJ_VNUM ( obj ) == 3306 )
	          || ( GET_OBJ_VNUM ( obj ) == 3309 )
	          || ( GET_OBJ_VNUM ( obj ) == 3312 ) )
	{
		ch->Send ( "You put %s %s into the machine.\r\n", AN ( arg1 ),
		           arg1 );
		obj_from_char ( obj );
		extract_obj ( obj );
		GET_GOLD_TOKEN_COUNT ( ch ) += 1;
		convert_tokens ( ch );
		return ( 1 );
	}
	else
	{
		ch->Send ( "The machine doesn't accept that.\r\n" );
		return ( 1 );
	}
}

ACMD ( do_deduct )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	int amount = 0, counter;
	struct obj_data *obj;

	if ( IS_NPC ( ch ) )
	{
		ch->Send ( "Silly mob, you don't collect tokens.\r\n" );
		return;
	}

	two_arguments ( argument, arg1, arg2 );

	if ( !*arg1 || !*arg2 )
	{
		ch->Send ( "Usage: deduct <amount> <type>.\r\n" );
		return;
	}

	amount = atoi ( arg1 );

	if ( amount < 0 )
	{
		ch->Send ( "Your attempt at cheating has been logged.  The Imms will be notified.\r\n" );
		return;
	}

	if ( isname ( "brass", arg2 ) )
	{
		if ( GET_BRASS_TOKEN_COUNT ( ch ) >= amount )
		{
			ch->Send ( "%d tokens were deducted from your account.\r\n", amount );
			GET_BRASS_TOKEN_COUNT ( ch ) -= amount;
			for ( counter = 0; counter < amount; counter++ )
			{
				obj = read_object ( 3300, VIRTUAL );
				if ( obj )
					obj_to_char ( obj, ch );
			}

			return;
		}
		else
		{
			ch->Send ( "You don't have that many tokens on account.\r\n" );
			return;
		}
	}
	else if ( isname ( "bronze", arg2 ) )
	{
		if ( GET_BRONZE_TOKEN_COUNT ( ch ) >= amount )
		{
			ch->Send ( "%d tokens were deducted from your account.\r\n", amount );
			GET_BRONZE_TOKEN_COUNT ( ch ) -= amount;
			for ( counter = 0; counter < amount; counter++ )
			{
				obj = read_object ( 3301, VIRTUAL );
				if ( obj )
					obj_to_char ( obj, ch );
			}

			return;
		}
		else
		{
			ch->Send ( "You don't have that many tokens on account.\r\n" );
			return;
		}
	}
	else if ( isname ( "silver", arg2 ) )
	{
		if ( GET_SILVER_TOKEN_COUNT ( ch ) >= amount )
		{
			ch->Send (
			    "%d tokens were deducted from your account.\r\n",
			    amount );
			GET_SILVER_TOKEN_COUNT ( ch ) -= amount;
			for ( counter = 0; counter < amount; counter++ )
			{
				obj = read_object ( 3302, VIRTUAL );
				if ( obj )
					obj_to_char ( obj, ch );
			}

			return;
		}
		else
		{
			ch->Send ( "You don't have that many tokens on account.\r\n" );
			return;
		}
	}
	else if ( isname ( "gold", arg2 ) )
	{
		if ( GET_GOLD_TOKEN_COUNT ( ch ) >= amount )
		{
			ch->Send (
			    "%d tokens were deducted from your account.\r\n",
			    amount );
			GET_GOLD_TOKEN_COUNT ( ch ) -= amount;
			for ( counter = 0; counter < amount; counter++ )
			{
				obj = read_object ( 3303, VIRTUAL );
				if ( obj )
					obj_to_char ( obj, ch );
			}

			return;
		}
		else
		{
			ch->Send ( "You don't have that many tokens on account.\r\n" );
			return;
		}
	}
	else
	{
		ch->Send ( "You can only deduct tokens from your account.\r\n" );
		return;
	}
	return;
}
