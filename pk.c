/**
This file contains most functions to do with Player Killing
mordecai - dec - 11 - 05
 **/
#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "fight.h"

ACMD(do_register)
{
  if (IS_NPC(ch))
    return;

  if (subcmd == SCMD_UNREGISTER)
  {
    if (!PLR_FLAGGED(ch, PLR_PK))
    {
      send_to_char("You are not a player killer, you don't need to unregister.\r\n", ch);
      return;
    }
    if (GET_GOLD_TOKEN_COUNT(ch) >= 1)
    {
      GET_GOLD_TOKEN_COUNT(ch)--;
      REMOVE_BIT_AR(PLR_FLAGS(ch), PLR_PK);
      ch->Send( "You are charged 1 Gold Token, and have had your PK flag removed.\r\n");
    }
    else
    {
      ch->Send( "You cannot afford to unregister, the cost is 1 gold token on file.\r\n");
    }
    return;
  }

  if (PLR_FLAGGED(ch, PLR_PK))
  {
    send_to_char("You are already a registered player killer.\r\n", ch);
    return;
  }

  if (GET_ROOM_VNUM(IN_ROOM(ch)) != 3138)
  {
    ch->Send( "You can only register at the Mayor's Secretary's office in Olde Yorke.\r\n");
    return;
  }

  if (REMORTS(ch) < 2)
  {
    ch->Send( "You must have at least 2 remorts to register for Player Killing.\r\n");
    return;
  }

  if (GET_BRONZE_TOKEN_COUNT(ch) >= 1)
  {
    GET_BRONZE_TOKEN_COUNT(ch)--;
    SET_BIT_AR(PLR_FLAGS(ch), PLR_PK);
    ch->Send( "That cost 1 Bronze token.\r\nWelcome to the World of Player Killing.\r\n");
  }
  else
  {
    ch->Send( "You need to have at least 1 Bronze token on file to afford this.\r\n");
  }
  return;
}

int both_pk(Character *a, Character *b)
{
  /* If either is a mob, they can't be PK */
  if (IS_NPC(a) || IS_NPC(b))
    return 0;
  /* Are they both registered? */
  if (IS_PK(a) && IS_PK(b))
    return 1;
  else /* nope */
    return 0;
}



void kill_points(Character *ch, Character *vict)
{

  int points = 50;
  int diff;

  if (!IS_NPC(vict))
    GET_RIP_CNT(vict) += 1;
  if (!IS_NPC(ch))
    GET_KILL_CNT(ch) += 1;

  if (both_pk(vict, ch))
  {
    if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_ARENA))
      points = 25;
    diff = ((current_class_is_tier_num(vict) * GET_LEVEL(vict)) - (current_class_is_tier_num(ch) * GET_LEVEL(ch)));

    if (diff > -10)
    {
      GET_PK_POINTS(ch) += IRANGE(-5, diff, points);
      GET_PK_POINTS(vict) -= IRANGE(1, diff, points);
    }
    else
    {
      GET_PK_POINTS(ch) -= IRANGE(1, abs(diff), points);
    }

    GET_PK_CNT(ch)++;
    GET_PK_RIP(vict)++;
  }
  if (!IS_NPC(ch))
    kill_list(ch, vict);

}


