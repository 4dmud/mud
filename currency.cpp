/* these are some commands that we can start with now
   to give places for people to spend their tokens
   the first command lets people transfer tokens in their
   account into practice sessions.
   This is helpful for newbies, but most players
   that have a few remorts have practice sessions in the thousands.
   This will also allow them to use up their practice sessions.
   100 practice sessions can equal 1 bronze token.
   20  practice sessions can equal 1 brass token.
   whereas it will cost 1 brass token to buy a practice session.
   (5 brass = bronze)

   The command will be: transfer <token|sessions> <number>

   transfer token 1 - will transfer 1 brass token into a training session.
   transfer session 1 - will transfer 20 sessions into a brass token.
   ------------------------

   the command convert will be used for players to make 1 larger
   denomonation in their account into the next smaller denomonation.

   ie: 1 gold into 10 silver.
       1 silver into 10 bronze.
       1 bronze into 5 brass.

       convert <brass|bronze|silver|gold>
       it will just place that amount in your account for you to deduct.

   -------------------------mord-----------------------------

O======================================================================O
|Name: Soulstar            Class: [ Thief      ] |Level:        [ 255] |
|Race: Faun                Sex:   [ Male       ] |Age:          [  26] |
|-------------------------------|----------------+---------------------|
|Str: [17] [17] [100] [100]     |Weight:  [  4%] |Brass  Tokens:[ 100] |
|Int: [25] [17] Wis: [25] [17]  |Items:   [  4%] |Bronze Tokens:[ 100] |
|Con: [25] [17] Dex: [25] [17]  |Thirst:  [ -4%] |Silver Tokens:[ 100] |
|Cha: [25] [17]                 |Full:    [ -4%] |Gold   Tokens:[ 100] |
|Gold: [                      ] |Drunk:   [-10%] |                     |
|Bank: [                      ] |                |Fame         :[ 100] |
|-------------------------------+----------------+---------------------|
|Exp. Gained  :  [  999999000 ] |PKills:[    12] |Bank:  [         0 ] |
|Exp. to Level:  [ 3294968296 ] |PDeath:[    12] |Gold:  [    983897 ] |
|-------------------------------+----------------+---------------------|
|Hit   Points: [ 3050]  [ 3050] |SPEEDMIN: [  8] |Alignment:   [-1000] |
|Mana  Points: [ 2702]  [ 2702] |SPEEDMAX: [ 20] |Hit Bonus:   [   23] |
|Move  Points: [ 1747]  [ 1747] |                |Dam Bonus:   [   78] |
|Damage Percent Resist: [  90%] |Mail:     [yes] |Practices:   [12343] |
O======================================================================O

     equip_char(ch, obj, where);

*/

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
#include "spells.h"
#include "genmob.h"

/* external variables */

void convert_tokens ( Character *ch );
ACMD ( do_convert )
{
	char arg2[MAX_INPUT_LENGTH];
	int counter, amount = 1;
	struct obj_data *obj;

	if ( IS_NPC ( ch ) )
	{
		ch->Send ( "Silly mob, you don't collect tokens.\r\n" );
		return;
	}

	one_argument ( argument, arg2 );

	if ( !*arg2 )
	{
		ch->Send ( "Usage: convert <tokentype>.\r\n" );
		return;
	}


	if ( amount < 0 )
	{
		ch->Send ( "Your attempt at cheating has been logged.  The Imms will be notified.\r\n" );
		return;
	}

	if ( isname ( "brass", arg2 ) )
	{
		if ( GET_BRASS_TOKEN_COUNT ( ch ) >= amount )
		{
			ch->Send ( "You can't split a brass token. If you want to Deduct say so.\r\n" );
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
			ch->Send ( "%d tokens were deducted from your account and split to brass.\r\n",
			           amount );
			GET_BRONZE_TOKEN_COUNT ( ch ) -= amount;
			for ( counter = 0; counter < 5; counter++ )
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
	else if ( isname ( "silver", arg2 ) )
	{
		if ( GET_SILVER_TOKEN_COUNT ( ch ) >= amount )
		{
			ch->Send ( "%d tokens were deducted from your account and split to bronze.\r\n",
			           amount );
			GET_SILVER_TOKEN_COUNT ( ch ) -= amount;
			for ( counter = 0; counter < 10; counter++ )
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
	else if ( isname ( "gold", arg2 ) )
	{
		if ( GET_GOLD_TOKEN_COUNT ( ch ) >= amount )
		{
			ch->Send ( "%d tokens were deducted from your account and split.\r\n", amount );
			GET_GOLD_TOKEN_COUNT ( ch ) -= amount;
			for ( counter = 0; counter < 10; counter++ )
			{
				obj = read_object ( 3302, VIRTUAL );
				if ( obj )
					obj_to_char ( obj, ch );
			}

			return;
		}
		else
		{
			ch->Send ( "You don't have enough tokens on account.\r\n" );
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

ACMD ( do_convey )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	gold_int amount = 0;

	if ( IS_NPC ( ch ) )
	{
		ch->Send ( "Silly mob, you don't collect tokens.\r\n" );
		return;
	}

//	Removed Remort check per Molly. - Prometheus.
//
//	if ( GET_REMORT ( ch ) == -1 )
//	{
//		ch->Send ( "This command works for remorted players only.\r\n" );
//		return;
//	}

	two_arguments ( argument, arg2, arg1 );


	if ( !*arg1 || !*arg2 )
	{
		ch->Send ( "Usage: convey <award|token|gold|maxmove|tradepoints> <amount>.\r\n" );
		ch->Send ( "see HELP CONVEY for more info.\r\n"
		           "CONVEY gold <amount>, turns gold coins into exp points at 4 to 1 ratio\r\n"
		           "CONVEY maxmove 1, turns (num of times used x ) 10 mill into 100 maxmove\r\n" );
		return;
	}
	amount = atoll ( arg1 );

	if ( amount <= 0 && !isname ( arg1, "maxmove" ) && !isname ( arg2, "gold" ) )
	{
		*ch << "Your usage is wrong.\r\n";
		return;
	}

	if ( isname ( arg2, "tokens" ) )
	{
		if ( amount > 50 )
		{
			ch->Send ( "You can only convey 50 bronze tokens or less.\r\n" );
		}
		else
		{
			int silver_tokens = amount / 50;
			int bronze_tokens = ( amount - silver_tokens * 50 ) / 5;
			int brass_tokens  = amount - silver_tokens * 50 - bronze_tokens * 5;
			int new_gold_tokencount   = GET_GOLD_TOKEN_COUNT ( ch );
			int new_silver_tokencount = GET_SILVER_TOKEN_COUNT ( ch ) - silver_tokens;
			int new_bronze_tokencount = GET_BRONZE_TOKEN_COUNT ( ch ) - bronze_tokens;
			int new_brass_tokencount  = GET_BRASS_TOKEN_COUNT ( ch )  - brass_tokens;

			if ( new_brass_tokencount < 0 )
			{
				new_bronze_tokencount--;
				new_brass_tokencount = 5 + new_brass_tokencount;
			}
			if ( new_bronze_tokencount < 0 )
			{
				new_silver_tokencount--;
				new_bronze_tokencount = 10 + new_bronze_tokencount;
			}
			if ( new_silver_tokencount < 0 )
			{
				new_gold_tokencount--;
				new_silver_tokencount = 10 + new_silver_tokencount;
			}

			if ( new_gold_tokencount < 0 )
			{
				ch->Send ( "You don't have that many tokens on your account.\r\n" );
			}
			else
			{
				char buf[100];
				char *bufpointer=buf;
				int sessions=silver_tokens * 500 + bronze_tokens * 50 + brass_tokens * 5;
				bufpointer+=snprintf ( bufpointer, ( size_t ) ( 98 - ( bufpointer - buf ) ), "You convey " );
				if ( silver_tokens > 0 )
					bufpointer += snprintf ( bufpointer, ( size_t ) ( 98 - ( bufpointer - buf ) ), "%d silver token%s",
					                         silver_tokens,
					                         silver_tokens > 1 ? "s" : "" );
				if ( bronze_tokens > 0 )
					bufpointer += snprintf ( bufpointer, ( size_t ) ( 98 - ( bufpointer - buf ) ), "%s%s%d bronze token%s",
					                         silver_tokens > 0 && brass_tokens > 0  ? ", " : "",
					                         silver_tokens > 0 && brass_tokens == 0 ? " and " : "",
					                         bronze_tokens,
					                         bronze_tokens > 1 ? "s" : "" );
				if ( brass_tokens > 0 )
					bufpointer += snprintf ( bufpointer, ( size_t ) ( 98 - ( bufpointer - buf ) ), "%s%d brass token%s",
					                         silver_tokens > 0 || bronze_tokens > 0 ? " and " : "",
					                         brass_tokens,
					                         brass_tokens > 1 ? "s" : "" );
				bufpointer += snprintf ( bufpointer, ( size_t ) ( 98 - ( bufpointer - buf ) ), " to %d training session%s.\r\n",
				                         sessions,
				                         sessions > 1 ? "s":"" );
				ch->Send ( "%s", buf );
				GET_GOLD_TOKEN_COUNT ( ch )   = new_gold_tokencount;
				GET_SILVER_TOKEN_COUNT ( ch ) = new_silver_tokencount;
				GET_BRONZE_TOKEN_COUNT ( ch ) = new_bronze_tokencount;
				GET_BRASS_TOKEN_COUNT ( ch )  = new_brass_tokencount;
				GET_PRACTICES ( ch )         += sessions;
			}
		}
		/* no more session converting. It's EVIL!!!!
		    } else if (isname(arg2, "sessions")) {
		        if ((amount % 20)) {
		            ch->Send("The number must be devisable by 20.");
		            return;
		        }
		        if (amount < 20) {
		            ch->Send("The amount must be greater than 20.");
		            return;
		        }
		        if (!(amount <= 200)) {
		            ch->Send("The amount must be less than or equal to 200.");
		            return;
		        }
		        if (GET_PRACTICES(ch) >= amount) {
		            ch->Send("You convey %lld practice sessions to %lld bronze tokens.\r\n",
		                     amount, amount / 20);
		            GET_BRONZE_TOKEN_COUNT(ch) += amount / 20;
		            GET_PRACTICES(ch) -= amount;
		            convert_tokens(ch);

		            return;
		        } else {
		            ch->Send("You don't have that many sessions.\r\n");
		            return;
		        }
		*/
	}
	// This should be arg2 then gold not gold then arg2
	// Changed this from "gold" to "maxmove" since the rest of the
	// code does the next agruement. Prometheus.
//	else if ( isname ( arg2, "gold" ) )
//	{
	else if ( isname ( arg2, "maxmove" ) )
		{

			if ( ch->Gold ( 0, GOLD_ALL ) >= 10000000 * GET_CONVERSIONS (ch))
			{
				log ( "INFO: %s conveyed %lld gold into %d maxmove", GET_NAME ( ch ), ( gold_int ) ( 10000000 * GET_CONVERSIONS ( ch ) ), 100 );
				ch->Send ( "You convey %lld gold to %d maxmove.\r\n",
				           ( gold_int ) ( 10000000 * GET_CONVERSIONS(ch)), 100 );
				GET_MAX_MOVE ( ch ) += 100;
				ch->Gold ( -10000000 * GET_CONVERSIONS(ch), GOLD_ALL );
				GET_CONVERSIONS ( ch ) ++;
				ch->affect_total();

			}
			else
			{
				ch->Send ( "You can't afford to!\r\n" );
			}
			return;
		}

	else if (isname(arg2, "gold")) {

		if ( ! ( amount >= 4 ) )
		{
			ch->Send ( "The amount must be greater than 4.\r\n" );
			return;
		}
		if ( amount % 4 != 0 )
		{
			ch->Send ( "The amount must be a multiple of 4.\r\n" );
			return;
		}

		if ( ch->Gold ( 0, GOLD_ALL ) >= amount )
		{
			log ( "INFO: %s conveyed %lld gold into %lld exp", GET_NAME ( ch ), amount, amount/4 );
			ch->Send ( "You convey %lld gold to %lld exp points.\r\n", amount, amount / 4 );
			gain_exp ( ch, amount / 4 );
			ch->Gold ( -amount, GOLD_ALL );

			return;
		}
		else
		{
			ch->Send ( "You can't afford to!\r\n" );
		}

	}
	else if ( isname ( arg2, "award" ) )
	{


		if ( PLR_FLAGGED ( ch, PLR_HERO ) || PLR_FLAGGED ( ch, PLR_RP_LEADER ) )
		{
			ch->Send ( "Sorry, can't do that.\r\n" );
			return;
		}

		if ( ( amount % 10 ) )
		{
			ch->Send ( "The amount must be a multiple of 10." );
			return;
		}

		if ( amount < 10 )
		{
			ch->Send ( "You can only convey 10 or more.\r\n" );
			return;
		}

		if ( amount > GET_AWARD ( ch ) )
		{
			ch->Send ( "You can't afford that action.\r\n" );
			return;
		}

		log ( "INFO: %s conveyed %d award points into %d bronze tokens", GET_NAME ( ch ), ( int ) amount, ( int ) ( amount/10 ) );

		ch->Send ( "You convey %d award points to %d bronze tokens.\r\n", ( int ) amount, ( int ) ( amount/10 ) );
		GET_BRONZE_TOKEN_COUNT ( ch ) += ( int ) ( amount/10 );
		GET_AWARD ( ch ) -= amount;
		convert_tokens ( ch );

	}
	else if ( isname ( arg2, "tradepoints" ) )
	{
		if ( amount < 1 )
		{
			ch->Send ( "You have to convey at least 1\r\n" );
			return;
		}

		if ( amount > TRADEPOINTS ( ch ) )
		{
			ch->Send ( "You can't afford that action.\r\n" );
			return;
		}
		ch->Send ( "You convey tradepoints for exp \r\nat the rate of 1 tradepoint per level 40 mob's exp equivalent.\r\n" );
		// Changed this to fixed value
		// Since code isn't getting value.
		//gain_exp ( ch, amount * mob_stats[30].exp );
		gold_int gain = amount * 1100000;
		gain_exp (ch, gain);
		TRADEPOINTS ( ch ) -= amount;
		new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TRADEPOINTS] %s conveyed %lld tradepoints to %lld xp. (%d tradepoints remaining)",  GET_NAME ( ch ), amount, gain, TRADEPOINTS(ch));

	}
	else
	{
		ch->Send ( "You can only convey tokens, gold, maxmove, award points or trade points.\r\n" );
		return;
	}
	return;
}

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
