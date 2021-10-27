/**
This file contains most functions to do with Player Killing
mordecai - dec - 11 - 05
 **/
#include "comm.h"
#include "config.h"
#include "db.h"
#include "fight.h"
#include "handler.h"
#include "interpreter.h"
#include "structs.h"
#include "sysdep.h"
#include "utils.h"

ACMD(do_register) {
    if (IS_NPC(ch)) return;

    if (subcmd == SCMD_UNREGISTER) {
        if (!PLR_FLAGGED(ch, PLR_PK)) {
            send_to_char(
                "You are not a player killer, you don't need to "
                "unregister.\r\n",
                ch);
            return;
        }
        if (GET_GOLD_TOKEN_COUNT(ch) >= 1) {
            GET_GOLD_TOKEN_COUNT(ch)--;
            REMOVE_BIT_AR(PLR_FLAGS(ch), PLR_PK);
            ch->Send(
                "You are charged 1 Gold Token, and have had your PK flag "
                "removed.\r\n");
        } else {
            ch->Send(
                "You cannot afford to unregister, the cost is 1 gold token on "
                "file.\r\n");
        }
        return;
    }

    if (PLR_FLAGGED(ch, PLR_PK)) {
        send_to_char("You are already a registered player killer.\r\n", ch);
        return;
    }

    if (GET_ROOM_VNUM(IN_ROOM(ch)) != 3138) {
        ch->Send(
            "You can only register at the Mayor's Secretary's office in Olde "
            "Yorke.\r\n");
        return;
    }

    if (REMORTS(ch) < 2) {
        ch->Send(
            "You must have at least 2 remorts to register for Player "
            "Killing.\r\n");
        return;
    }

    if (GET_BRONZE_TOKEN_COUNT(ch) >= 1) {
        GET_BRONZE_TOKEN_COUNT(ch)
        --;
        SET_BIT_AR(PLR_FLAGS(ch), PLR_PK);
        ch->Send(
            "That cost 1 Bronze token.\r\nWelcome to the World of Player "
            "Killing.\r\n");
    } else {
        ch->Send(
            "You need to have at least 1 Bronze token on file to afford "
            "this.\r\n");
    }
    return;
}

// Checks if both characters in a fight have the PK flag.
// TODO: why is this function not using a bool?
int both_pk(Character *a, Character *b) {
    // If either is a mob, it's not a PK fight.
    if (IS_NPC(a) || IS_NPC(b)) return 0;

    // Both players must have the PK flag.
    if (IS_PK(a) && IS_PK(b))
        return 1;
    else /* nope */
        return 0;
}

// interface
void kill_points(Character *ch, Character *vict);
// Calculates how many PK points to assigns to winner and loser.
// Also incremenets the PK kill counter and decrements the PK RIP counter.
void kill_points(Character *winner, Character *loser) {
    // General
    if (IS_NPC(winner) && IS_NPC(loser)) return;

    if (!IS_NPC(winner)) {
        GET_KILL_CNT(winner)++;
        kill_list(winner, loser);
    }
    if (!IS_NPC(loser)) GET_RIP_CNT(loser)++;

    // PK only
    if (both_pk(winner, loser) == 0) {
        return;
    }

    // increase PK KILL counter for winner
    GET_PK_CNT(winner)++;
    // increase PK RIP counter for loser
    GET_PK_RIP(loser)++;

    int points = 50;
    // if the KILL happened in arena reduce points awarded
    if (ROOM_FLAGGED(IN_ROOM(winner), ROOM_ARENA)) {
        points = 25;
    }

    LAST_PK = strdup(GET_NAME(winner));
    CHAMPION = GET_IDNUM(winner);

    player1 = current_class_is_tier_num(winner) * GET_LEVEL(winner);
    player2 = current_class_is_tier_num(loser) * GET_LEVEL(loser);

    // assign points
    if (player1 % player2 == 0) {  // it's a fair fight
        GET_PK_POINTS(winner) += points;
        GET_PK_POINTS(loser) -= points;
    } else {
        if (player1 > player2) {  // unfair fight | remove half points
            GET_PK_POINTS(winner) -= points / 2
        }
    }
}
