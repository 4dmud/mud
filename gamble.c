/* ***********************************************************************\
*  _____ _            ____                  _       _                     *
* |_   _| |__   ___  |  _ \  ___  _ __ ___ (_)_ __ (_) ___  _ __          *
*   | | | '_ \ / _ \ | | | |/ _ \| '_ ` _ \| | '_ \| |/ _ \| '_ \         *
*   | | | | | |  __/ | |_| | (_) | | | | | | | | | | | (_) | | | |        *
*   |_| |_| |_|\___| |____/ \___/|_| |_| |_|_|_| |_|_|\___/|_| |_|        *
*                                                                         *
*  File:  GAMBLE.C                                 Based on CircleMUD 3.0 *
*  Usage: Implementation of casino games                                  *
*  Programmer(s): Original code by the EnvyMUD Development Team           *
*                 All Modifications by Sean Mountcastle (Glasgian)        *
\*********************************************************************** */

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "screen.h"


/* New for TD 6/10/95 */
void play_slots(Character *ch)
{
    int num1, num2, num3, win = 0;
    const char *slot_msg[] = {
	"*YOU SHOULDN'T SEE THIS*",
	"a mithril bar",	/* 1 */
	"a golden dragon",
	"a Dwarven hammer",
	"a temple",
	"an Elven bow",		/* 5 */
	"a red brick",
	"a refuse pile",
	"a waybread",
	"a Gnomish bell",
	"a beggar",		/* 10 */
    };

    if (ch->Gold( 0, GOLD_HAND) < 1) {
	send_to_char("You do not have enough money to play the slots!\r\n",
		     ch);
	return;
    } else
	ch->Gold( -1, GOLD_HAND);

    send_to_char("You pull the handle on the slot machine.\r\n", ch);

    /* very simple roll 3 random numbers from 1 to 10 */
    num1 = number(1, 10);
    num2 = number(1, 10);
    num3 = number(1, 10);

    if (num1 == num2 && num2 == num3) {
	/* all 3 are equal! Woohoo! */
	if (num1 == 1)
	    win += 50;
	else if (num1 == 2)
	    win += 25;
	else if (num1 == 3)
	    win += 15;
	else if (num1 == 4)
	    win += 10;
	else if (num1 == 5)
	    win += 5;
	else			// any remaining 3 matching will pay 1 coin
	    win += 1;
    }

    ch->Send( "You got %s, %s, %s, ", slot_msg[num1],
	    slot_msg[num2], slot_msg[num3]);
    if (win > 1)
	ch->Send( "you win %d gold pieces!\r\n", win);
    else if (win == 1)
	ch->Send( "you win 1 gold piece!\r\n");
    else
	ch->Send( "you lose.\r\n");
    ch->Gold(win, GOLD_ALL);

    return;
}

void play_high_dice(Character *ch, Character *dealer,
		    int bet)
{
    int die1, die2, die3, die4;
    char buf[MAX_INPUT_LENGTH];

    if (ch->Gold( 0, GOLD_HAND) < bet) {
	act("$n says, 'I'm sorry sir but you don't have that much gold.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet > 5000) {
	act("$n says, 'I'm sorry sir but the limit at this table is 5000 gold pieces.'", FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet < 0) {
	act("$n says, 'Very funny asshole.'", FALSE, dealer, 0, ch,
	    TO_VICT);
	return;
    } else {
	ch->Gold( -bet, GOLD_HAND);
	act("$N places $S bet on the table.", FALSE, 0, 0, ch, TO_NOTVICT);
	act("You place your bet on the table.", FALSE, ch, 0, 0, TO_CHAR);
    }

    /* dealer rolls two dice */
    die1 = number(1, 6);
    die2 = number(1, 6);

    sprintf(buf,
	    "$n rolls the dice, $e gets %d, and %d, for a total of %d.",
	    die1, die2, (die1 + die2));
    act(buf, FALSE, dealer, 0, ch, TO_ROOM);
    /* now its the players turn */
    die3 = number(1, 6);
    die4 = number(1, 6);

    sprintf(buf,
	    "$N rolls the dice, $E gets %d, and %d, for a total of %d.",
	    die3, die4, (die3 + die4));
    act(buf, FALSE, dealer, 0, ch, TO_NOTVICT);
    ch->Send(
	    "You roll the dice, and get %d, and %d, for a total of %d.\r\n",
	    die3, die4, (die3 + die4));

    if ((die1 + die2) >= (die3 + die4)) {
	sprintf(buf, "The house wins %d coins from $N.", bet);
	act(buf, FALSE, dealer, 0, ch, TO_NOTVICT);
	ch->Send( "The house wins %d coins from you.\r\n", bet);
    } else {
	sprintf(buf, "$N wins %d gold coins!", bet * 2);
	act(buf, FALSE, dealer, 0, ch, TO_NOTVICT);
	ch->Send( "You win %d gold coins!\r\n", bet * 2);
	ch->Gold( (bet * 2), GOLD_ALL);
    }
    return;
}


void play_triples(Character *ch, Character *dealer,
		  char *guess, int bet)
{
    int die1, die2, die3, total = 0;
    
    char buf[MAX_INPUT_LENGTH];
    if (ch->Gold( 0, GOLD_HAND) < bet) {
	act("$n says, 'I'm sorry sir but you don't have that much gold.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet > 5000) {
	act("$n says, 'I'm sorry sir but the limit at this table is 5000 gold pieces.'", FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet < 0) {
	act("$n says, 'Very funny asshole.'", FALSE, dealer, 0, ch,
	    TO_VICT);
	return;
    } else {
	ch->Gold( -bet, GOLD_HAND);
	act("$N places $S bet on the table.", FALSE, 0, 0, ch, TO_NOTVICT);
	act("You place your bet on the table.", FALSE, ch, 0, 0, TO_CHAR);
    }

    if (!*guess) {
	act("$n tells you, 'You need to specify upper, lower, or triple.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	act("$n hands your bet back to you.", FALSE, dealer, 0, ch,
	    TO_VICT);
	ch->Gold(bet, GOLD_HAND);
	return;
    }

    if (!(!strcmp(guess, "upper") ||
	  !strcmp(guess, "lower") || !strcmp(guess, "triple"))) {
	act("$n tells you, 'You need to specify upper, lower, or triple.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	act("$n hands your bet back to you.", FALSE, dealer, 0, ch,
	    TO_VICT);
	ch->Gold( bet, GOLD_HAND);
	return;
    }

    die1 = number(1, 6);
    die2 = number(1, 6);
    die3 = number(1, 6);

    total = die1 + die2 + die3;

    sprintf(buf, "$N rolls %d, %d, and %d", die1, die2, die3);
    if (die1 == die2 && die2 == die3)
	strcat(buf, ", $E scored a triple!");
    else
	strcat(buf, ".");
    act(buf, FALSE, dealer, 0, ch, TO_NOTVICT);
    sprintf(buf, "You roll %d, %d, and %d, for a total of %d.\r\n", die1,
	    die2, die3, total);
    send_to_char(buf, ch);

    if ((die1 == die2 && die2 == die3) && !strcmp(guess, "triple")) {
	/* scored a triple! player wins 3x the bet */
	act("$n says, 'Congratulations $N, you win.'", FALSE, dealer, 0,
	    ch, TO_ROOM);
	sprintf(buf, "$n hands you %d gold pieces.", (bet * 3));
	act(buf, FALSE, dealer, 0, ch, TO_VICT);
	act("$n hands $N some gold pieces.", FALSE, dealer, 0, ch,
	    TO_NOTVICT);
	ch->Gold( (bet * 3), GOLD_ALL);
    } else if ((total <= 9 && !strcmp(guess, "lower")) ||
	       (total > 9 && !strcmp(guess, "upper"))) {
	act("$n says, 'Congratulations $N, you win.'", FALSE, dealer, 0,
	    ch, TO_ROOM);
	sprintf(buf, "$n hands you %d gold pieces.", (bet * 2));
	act(buf, FALSE, dealer, 0, ch, TO_VICT);
	act("$n hands $N some gold pieces.", FALSE, dealer, 0, ch,
	    TO_NOTVICT);
	ch->Gold(  (bet * 2), GOLD_ALL);
    } else {
	act("$n says, 'Sorry $N, better luck next time.'", FALSE, dealer,
	    0, ch, TO_ROOM);
	act("$n greedily counts $s new winnings.", FALSE, dealer, 0, ch,
	    TO_ROOM);
    }
    return;
}

void play_seven(Character *ch, Character *dealer,
		char *guess, int bet)
{
    int die1, die2, total = 0;
    
    char buf[MAX_INPUT_LENGTH];

    if (ch->Gold( 0, GOLD_HAND) < bet) {
	act("$n says, 'I'm sorry sir but you don't have that much gold.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet > 5000) {
	act("$n says, 'I'm sorry sir but the limit at this table is 5000 gold pieces.'", FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet < 0) {
	act("$n says, 'Very funny asshole.'", FALSE, dealer, 0, ch,
	    TO_VICT);
	return;
    } else {
	ch->Gold( -bet, GOLD_HAND);
	act("$N places $S bet on the table.", FALSE, 0, 0, ch, TO_NOTVICT);
	act("You place your bet on the table.", FALSE, ch, 0, 0, TO_CHAR);
    }

    if (!*guess) {
	act("$n tells you, 'You need to specify under, over, or seven.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	act("$n hands your bet back to you.", FALSE, dealer, 0, ch,
	    TO_VICT);
	ch->Gold( bet, GOLD_HAND);
	return;
    }
    if (!(!strcmp(guess, "under") ||
	  !strcmp(guess, "over") || !strcmp(guess, "seven"))) {
	act("$n tells you, 'You need to specify under, over, or seven.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	act("$n hands your bet back to you.", FALSE, dealer, 0, ch,
	    TO_VICT);
	ch->Gold( bet, GOLD_HAND);
	return;
    }

    act("$n says, 'Roll the dice $N and tempt lady luck.'",
	FALSE, dealer, 0, ch, TO_ROOM);

    die1 = number(1, 6);
    die2 = number(1, 6);
    total = die1 + die2;

    sprintf(buf, "$N rolls the dice, getting a %d and a %d.", die1, die2);
    act(buf, FALSE, dealer, 0, ch, TO_NOTVICT);
    sprintf(buf, "You roll the dice, they come up %d and %d.\r\n", die1,
	    die2);
    send_to_char(buf, ch);

    if ((total < 7 && !strcmp(guess, "under")) ||
	(total > 7 && !strcmp(guess, "over"))) {
	/* player wins 2x $s money */
	act("$n says, 'Congratulations $N, you win!'", FALSE, dealer, 0,
	    ch, TO_ROOM);
	act("$n hands $N some gold pieces.", FALSE, dealer, 0, ch,
	    TO_NOTVICT);
	sprintf(buf, "$n hands you %d gold pieces.", (bet * 2));
	act(buf, FALSE, dealer, 0, ch, TO_VICT);
	ch->Gold( (bet * 2), GOLD_ALL);
    } else if (total == 7 && !strcmp(guess, "seven")) {
	/* player wins 5x $s money */
	act("$n says, 'Congratulations $N, you win!'", FALSE, dealer, 0,
	    ch, TO_ROOM);
	act("$n hands $N some gold pieces.", FALSE, dealer, 0, ch,
	    TO_NOTVICT);
	sprintf(buf, "$n hands you %d gold pieces.", (bet * 5));
	act(buf, FALSE, dealer, 0, ch, TO_VICT);
	ch->Gold((bet * 5), GOLD_ALL);
    } else {
	/* player loses */
	act("$n says, 'Sorry $N, you lose.'", FALSE, dealer, 0, ch,
	    TO_ROOM);
	act("$n takes $N's bet from the table.", FALSE, dealer, 0, ch,
	    TO_NOTVICT);
	act("$n takes your bet from the table.", FALSE, dealer, 0, ch,
	    TO_VICT);
    }
    return;
}

void play_craps(Character *ch, Character *dealer, int bet)
{
    int die1, die2, mark = 0, last = 0;
    bool won = FALSE, firstime = TRUE;
    
    char buf[MAX_INPUT_LENGTH];

    if (ch->Gold( 0, GOLD_HAND) < bet) {
	act("$n says, 'I'm sorry sir but you don't have that much gold.'",
	    FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet > 10000) {
	act("$n says, 'I'm sorry sir but the limit at this table is 10000 gold pieces.'", FALSE, dealer, 0, ch, TO_VICT);
	return;
    } else if (bet < 0) {
	act("$n says, 'Very funny asshole.'", FALSE, dealer, 0, ch,
	    TO_VICT);
	return;
    } else {
	ch->Gold( -bet, GOLD_HAND);
	act("$N places $S bet on the table.", FALSE, 0, 0, ch, TO_NOTVICT);
	act("You place your bet on the table.", FALSE, ch, 0, 0, TO_CHAR);
    }

    act("$n hands $N the dice and says, 'roll 'em'", FALSE, dealer, 0, ch,
	TO_NOTVICT);
    act("$n hands you the dice and says, 'roll 'em'", FALSE, dealer, 0, ch,
	TO_VICT);

    while (won != TRUE) {
	die1 = number(1, 6);
	die2 = number(1, 6);
	mark = die1 + die2;

	sprintf(buf, "$n says, '$N rolls %d and %d, totalling %d.",
		die1, die2, mark);
	act(buf, FALSE, dealer, 0, ch, TO_ROOM);

	if ((mark == 7 || mark == 11) && firstime) {
	    /* win on first roll of the dice! 3x bet */
	    act("$n says, 'Congratulations $N, you win!'", FALSE, dealer,
		0, ch, TO_ROOM);
	    act("$n hands $N some gold pieces.", FALSE, dealer, 0, ch,
		TO_NOTVICT);
	    sprintf(buf, "$n hands you %d gold pieces.", (bet * 3));
	    act(buf, FALSE, dealer, 0, ch, TO_VICT);
	    ch->Gold( (bet * 3), GOLD_ALL);
	    won = TRUE;
	} else if (mark == 3 || mark == 12) {
	    /* player loses on first roll */
	    act("$n says, 'Sorry $N, you lose.'", FALSE, dealer, 0, ch,
		TO_ROOM);
	    act("$n takes $N's bet from the table.", FALSE, dealer, 0, ch,
		TO_NOTVICT);
	    act("$n takes your bet from the table.", FALSE, dealer, 0, ch,
		TO_VICT);
	    won = TRUE;
	} else if ((last == mark) && !firstime) {
	    /* player makes $s mark! 2x bet */
	    act("$n says, 'Congratulations $N, you win!'", FALSE, dealer,
		0, ch, TO_ROOM);
	    act("$n hands $N some gold pieces.", FALSE, dealer, 0, ch,
		TO_NOTVICT);
	    sprintf(buf, "$n hands you %d gold pieces.", (bet * 2));
	    act(buf, FALSE, dealer, 0, ch, TO_VICT);
	    ch->Gold( (bet * 2), GOLD_ALL);
	    won = TRUE;
	} else {
	    sprintf(buf, "$n says, '$N's mark is %d.  Roll 'em again $N!'",
		    mark);
	    act(buf, FALSE, dealer, 0, ch, TO_ROOM);
	    firstime = FALSE;
	    last = mark;
	    won = FALSE;
	}
    }
    return;
}

void play_blackjack(Character *ch, Character *dealer,
		    char *guess, int bet)
{
    send_to_char("This is currently unavailable\r\n", ch);
    return;
}
