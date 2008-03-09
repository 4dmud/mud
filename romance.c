/*
*  C Implementation: romance
*
* Description: 
*
*
* Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
*
* Copyright: See COPYING file that comes with this distribution
*
*/
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
#include "constants.h"
#include "damage.h"
#include "event.h"
#include "romance.h"

/* local functions */
int genpreg(void);
int crashcheck_alpha(Character *ch, Character *vict);


/* -^- *** YORU'S ROMANCE MODULE     *** -^-
          ** Romance For CircleMUD 3.0 **
           *  Special Thanks to:       *
      Rawther and Liran of AugMUD for their help! */
/* Extra Thanks to Philip Ames for help debugging V 0.92 */
/* Extra Thanks to Brian Finley for help debugging V 0.9/0.92.2b */

void namesave(Character *ch, Character *vict)
{
  /* Saves the names in PARTNER() */
  /* Now we're set! Save it! */
  
  if (!IS_NPC(ch)) SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
  if (!IS_NPC(vict)) SET_BIT_AR(PLR_FLAGS(vict), PLR_CRASH);
}

void changesave(Character *ch, Character *vict)
{
  /* Saves changes after rejection */
  namesave(ch, vict);
}

int check_samesex(Character *ch, Character *victim)
{
  /*Checks if it's a same-sex proposition/marriage */
  /* Then checks if SAME_SEX_ALLOWED is TRUE. */
  /* If the proposition is same-sex, and SAME_SEX_ALLOWED */
  /* is FALSE, then it returns 1, which will halt the procedure. */
  /* Otherwise, if SAME_SEX_ALLOWED is TRUE or the proposition is */
  /* NOT same-sex, it returns 0, and things continue as normal */
  if ((GET_SEX(ch) == GET_SEX(victim)) && (SAME_SEX_ALLOWED == FALSE))
  {
    return (1);
  }
  else
  {
    return (0);
  }
}

/* Romance Module -- Ask Someone Out */
ACMD(do_askout)
{
  Character *victim;
  char arg[MAX_INPUT_LENGTH];
  
  one_argument(argument, arg);
  if (!*arg)
  {       /* What, ask no one out? */
    send_to_char("Uh, whom do you wish to ask out?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Is that person here? Nope! */
    ch->Send( "%s", CONFIG_NOPERSON);
    return;
  }
  
  if (ROMANCE(ch) != 0)
  {  /* Are you already involved? */
    send_to_char("Sorry, you're already romantically involved!\r\n",
                 ch);
    return;
  }
  else if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  else if (ROMANCE(victim) != 0)
  {  /* Is the person you're propositioning to involved? */
    send_to_char
      ("Try propositioning someone who ISN'T already romantically involved!\r\n",
       ch);
    return;
  }
  else if (victim == ch)
  {
    /* Ask yourself out?!? */
    send_to_char("Sorry, but you cannot ask yourself out!\r\n", ch);
    return;
    //    } //else if (check_samesex(ch, victim) == 1) {
    /* Check if it's same-sex, and if same-sex is allowed. */
    //send_to_char("Sorry, same-sex relations are not allowed here.\r\n", ch);
    //return;
  }
  else if (ROMANCE(ch) == -3)
  {
    send_to_char
      ("Sorry, you have to turn romance on before asking someone out.\r\n",
       ch);
    return;
  }
  else if (ROMANCE(victim) == -3)
  {
    send_to_char
      ("Sorry, they've got romance turned off for the moment..\r\n",
       ch);
    return;
  }
  else
  {            /* Okay, now we do that actual asking out.. */
    act("You ask $N out...\r\n", TRUE, ch, 0, victim, TO_CHAR);
    act("$n is asking you out!\r\n", TRUE, ch, 0, victim, TO_VICT);
    act("$n asks $N out on a date!\r\n", TRUE, ch, 0, victim,
        TO_NOTVICT);
    ROMANCE(victim) = ASKED_OUT;   /* Now they're being asked out. */
    ROMANCE(ch) = ASKING;     /* Temporarily declare the person as being asked out.. */
    /* NOTE: PARTNER(ch) must be reset in DO_REJECT */
    /* At this point, the Victim can either ACCEPT or REJECT the proposition. */
  }
}

/* Romance Module -- Accept a Proposition */

ACMD(do_accept)
{
  Character *victim;
  char arg[MAX_INPUT_LENGTH];
  if (ROMANCE(ch) < ASKED_OUT)
  {  /* You're not being asked out.. */
    send_to_char("You're not being romantically propositioned..\r\n",
                 ch);
    return;
  }
  else
  {            /* Okay, you've been asked out or proposed to.. */
    one_argument(argument, arg);
    if (!*arg)
    {          /* Accept no one? */
      send_to_char("Who do you want to accept?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      ch->Send( "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {     /* Accept yourself? */
      send_to_char("You can't accept yourself as a partner!\r\n",
                   ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) < ASKING)
    {     /* Are they propositioning as well? */
      send_to_char("But they're not asking you out!\r\n", ch);
      return;
      //        } //else if(check_samesex(ch, victim) == 1) {
      /* Check for same-sex relations.. */
      //send_to_char("Sorry, same-sex relationships are not allowed here.\r\n", ch);
      /* Okay, they've been corrected. The tick-timer will expire the
         ROMANCE factor on it's own. */
      //return;
    }
    else
    {          /* Okay, all the tests pass.. */
      if ((ROMANCE(ch) == ASKED_OUT) && (ROMANCE(victim) == ASKING))
      {
        /* For dating.. */
        act("You agree to date $N!\r\n", TRUE, ch, 0, victim,
            TO_CHAR);
        act("$n has agreed to date you!\r\n", TRUE, ch, 0, victim,
            TO_VICT);
        act("$n agrees to date $N!\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        /* We've notified them, now change the variables. */
        
        PARTNER(ch) = GET_IDNUM(victim);
        PARTNER(victim) = GET_IDNUM(ch);
        ROMANCE(ch) = 1;
        ROMANCE(victim) = 1;
        /* Now they're set as partners, and they've been set at Dating. */
        namesave(ch, victim); // Reorient PARTNER()s
        return;
      }
      else if ((ROMANCE(ch) == PROPOSED_TO)
               && (ROMANCE(victim) == ASKING))
      {
        /* For engagement.. */
        act("You agree to marry $N!\r\n", TRUE, ch, 0, victim,
            TO_CHAR);
        act("$n has agreed to marry you!\r\n", TRUE, ch, 0, victim,
            TO_VICT);
        act("$n agrees to marry $N!\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        /* We've done the notification, now change the variables. */
        
        PARTNER(ch) = GET_IDNUM(victim);
        PARTNER(victim) = GET_IDNUM(ch);
        ROMANCE(ch) = 2;
        ROMANCE(victim) = 2;
        /* Now they're engaged and partnered.. */
        namesave(ch, victim); // Reorient PARTNER()s
        return;          /* Get the hell outta this command. */
      }
      else
      {        /* They're not propositioning you after all. */
        send_to_char("That person isn't propositioning you..\r\n",
                     ch);
        return;
      }
    }
  }
}

/* Romance Module -- Reject A Proposition */
ACMD(do_reject)
{
  Character *victim;
  char arg[MAX_INPUT_LENGTH+1];
  if (ROMANCE(ch) < ASKED_OUT)
  {  /* You're not being asked out.. */
    send_to_char("You're not being romantically propositioned..\r\n",
                 ch);
    return;
  }
  else
  {            /* Okay, you've been asked out or proposed to.. */
    one_argument(argument, arg);
    if (!*arg)
    {          /* Reject no one? */
      send_to_char("Whose heart do you wish to break?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      ch->Send( "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {     /* Reject yourself? */
      send_to_char("You can't reject yourself!\r\n", ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) < ASKING)
    {     /* Are they propositioning as well? */
      send_to_char("But they're not asking you out!\r\n", ch);
      return;
      //       } //else if(check_samesex(ch, victim) == 1) {
      /* Uh, they couldn't have asked you out. */
      //send_to_char("Sorry, but that person isn't propositioning you.\r\n", ch);
      //return;
    }
    else
    {          /* Okay, all the tests pass.. */
      if ((ROMANCE(ch) == ASKED_OUT) && (ROMANCE(victim) == ASKING))
      {
        /* For dating.. */
        act("You tell $N that you'd rather just be friends.\r\n",
            TRUE, ch, 0, victim, TO_CHAR);
        act("$n would rather just be 'friends', but you feel your heart shatter.\r\n", TRUE, ch, 0, victim, TO_VICT);
        act("$n rejects $N! How cold!\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        PARTNER(victim) = 0;
        PARTNER(ch) = 0;
        ROMANCE(victim) = 0;
        ROMANCE(ch) = 0;
        changesave(ch, victim);
        return;
      }
      else if ((ROMANCE(ch) == PROPOSED_TO)
               && (ROMANCE(victim) == ASKING))
      {
        /* For marriage proposals... */
        send_to_char
          ("You decide you'd rather not get married just yet.\r\n",
           ch);
        act("$n doesn't want to get married just yet..\r\n", TRUE,
            ch, 0, victim, TO_VICT);
        act("$n hands $N's ring back to $S.\r\n", TRUE, ch, 0,
            victim, TO_NOTVICT);
        PARTNER(ch) = GET_IDNUM(victim);
        PARTNER(victim) = GET_IDNUM(ch);
        /* Just in case. */
        ROMANCE(victim) = 1;
        ROMANCE(ch) = 1; /* Back to "Dating" status. */
        namesave(ch, victim);
        return;
      }
      else
      {        /* Oops! They're not asking you after all! */
        send_to_char("But they're not propositioning you!\r\n",
                     ch);
        return;
      }
    }
  }
}

/* Romance Module -- Propose Marriage */
ACMD(do_propose)
{
  /* To propose to someone. */
  Character *victim;
  char arg[MAX_INPUT_LENGTH];
  
  switch (ROMANCE(ch))
  {
  default:
    ch->Send( "Error!!\r\n");
    return;
  case 1:
    //success:
    break;
  case 2:
    ch->Send( "But you're already engaged!\r\n");
    return;
  case 3:
    ch->Send(
                     "But you're married already! %s wouldn't approve!\r\n", pi.NameById(PARTNER(ch)));
    return;
  case 4:
    ch->Send( "But you're being asked out! That would be rude!\r\n");
    return;
  case 5:
    ch->Send( "But you're already proposing!\r\n");
    return;
  }
  
  if (ROMANCE(ch) == ASKING)
  {  /*Asking someone out? */
    ch->Send( "But you're asking someone else!\r\n");
    return;
    
  }
  else
  {            /* Okay, YOU pass... */
    one_argument(argument, arg);
    if (!*arg)
    {          /* Propose to no one? */
      ch->Send("Whom do you want to propose to?\r\n");
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      ch->Send( "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {     /* Propose to yourself? */
      ch->Send("You can't propose to yourself!\r\n");
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) < 1)
    {     /* Are they already dating? */
      ch->Send( "But they're not dating anyone!\r\n");
      return;
    }
    else if (ROMANCE(victim) == 2)
    {     /* Are they already engaged? */
      ch->Send( "But they're already engaged to %s!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if (ROMANCE(victim) == 3)
    {     /* Are they already married? */
      ch->Send(
                       "But they're married already! %s wouldn't approve!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if (ROMANCE(victim) == 4)
    {     /* Are they being asked out? */
      ch->Send( "But they're being asked out! That would be rude!\r\n");
      return;
    }
    else if (ROMANCE(victim) == 5)
    {     /* Are they already proposing? */
      ch->Send("But they're already proposing!\r\n");
      return;
    }
    else if (ROMANCE(victim) == 6)
    {     /*Asking someone? */
      ch->Send("But they're already asking someone else!\r\n");
      return;
    }
    else if (ROMANCE(victim) > 6)
    {     /* Any errors in the module? */
    ch->Send( "ERROR IN ROMANCE MODULE: Romance Factor > 5!\r\n");
      return;
    }
    /* Okay, we've established you're both dating someone.. */
    else if (PARTNER(ch) != GET_IDNUM(victim))
    {
      /* But are you dating them? */
      ch->Send( "But you're not dating %s!\r\n",   GET_NAME(victim));
      return;
    }
    else if (PARTNER(victim) != GET_IDNUM(ch))
    {
      /* Are they dating you? We shouldn't need this, though.. */
      ch->Send( "But %s isn't dating you!\r\n", GET_NAME(victim));
      return;
    }
    /* Okay, you're dating each other, now we can get on with it! */
    else
    {
      act("You kneel in front of $N and ask for $S hand in marriage!\r\n", TRUE, ch, 0, victim, TO_CHAR);
      act("$n kneels before you and asks for your hand in marriage!\r\n", TRUE, ch, 0, victim, TO_VICT);
      act("$n kneels before $N, asking for $S hand in marriage!\r\n",
          TRUE, ch, 0, victim, TO_NOTVICT);
      /* We've informed the world, now change the vars.. */
      ROMANCE(ch) = ASKING;
      ROMANCE(victim) = PROPOSED_TO;
      /* Now all the partner has to do is accept or reject. */
      return;
    }
  }
}

/* Romance Module: Break up */

ACMD(do_breakup)
{
  Character *victim;
  char arg[MAX_INPUT_LENGTH];
  /* First, standard checks: Are you in a relationship? */
  if (ROMANCE(ch) == 0)
  {
    send_to_char("But you're not romantically involved!\r\n", ch);
    return;
  }
  else if (ROMANCE(ch) == 3)
  {
    ch->Send(
                     "But you're already married! You have to DIVORCE %s!\r\n",
                     pi.NameById(PARTNER(ch)));
    return;
  }
  else
  {
    /* Okay, you're involved.. */
    one_argument(argument, arg);
    if (!*arg)
    {          /* Break up with noone? */
      send_to_char("Whom do you want to break up with?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      ch->Send( "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {     /* Break up with yourself? */
      send_to_char("You can't break up with yourself!\r\n", ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) == 0)
    {
      send_to_char("But they're not romantically involved!\r\n", ch);
      return;
    }
    else if ((ROMANCE(victim) == 1)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      ch->Send( "But they're dating %s, not you!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 2)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      ch->Send( "But they're engaged to %s, not you!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 3)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      ch->Send( "But they're married to %s, not you!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 3)
             && (PARTNER(victim) == GET_IDNUM(ch)))
    {
      ch->Send(
                       "They're already married to you! You have to DIVORCE %s!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if (PARTNER(victim) != GET_IDNUM(ch))
    {
      ch->Send( "But %s isn't involved with you!\r\n",
                       GET_NAME(victim));
      return;
    }
    else
    {
      /* Okay, they're involved and with you... */
      /* Now we break them up! How FUN! */
      if ((ROMANCE(ch) == 1) && (ROMANCE(victim) == 1))
      {
        /* For dating */
        act("You inform $N that you will no longer date $M!\r\n",
            TRUE, ch, 0, victim, TO_CHAR);
        act("$n dumps you, tearing your heart out in the process!\r\n", TRUE, ch, 0, victim, TO_VICT);
        act("$n sends $s relationship with $N to Splitsville!\r\n",
            TRUE, ch, 0, victim, TO_NOTVICT);
        /* Now set the variables to 0 */
        PARTNER(ch) = 0;
        PARTNER(victim) = 0;
        ROMANCE(ch) = 0;
        ROMANCE(victim) = 0;
        namesave(ch, victim);
        /* Done. You've dumped them. */
        return;
      }
      else if ((ROMANCE(ch) == 2) && (ROMANCE(victim) == 2))
      {
        /* For engagements */
        act("You call off your wedding with $N.\r\n", TRUE, ch, 0,
            victim, TO_CHAR);
        act("$n doesn't want to marry you anymore! The wedding's off!\r\n", TRUE, ch, 0, victim, TO_VICT);
        act("$n nullifies $s engagement with $N! The wedding's off!\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
        /* Now set the variables to 0 */
        PARTNER(ch) = 0;
        PARTNER(victim) = 0;
        ROMANCE(ch) = 0;
        ROMANCE(victim) = 0;
        namesave(ch, victim);
        /* Done. You've dumped them. */
        return;
      }
      else if ((ROMANCE(ch) == 4) && (ROMANCE(victim) == 4))
      {
        /* For dating/askouts */
        act("You inform $N that you no longer wish to date $S!\r\n", TRUE, ch, 0, victim, TO_CHAR);
        act("$n doesn't feel like dating you anymore!\r\n", TRUE,
            ch, 0, victim, TO_VICT);
        act("$n decides not to date $N.\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        /* Now set the variables to 0 */
        PARTNER(ch) = 0;
        PARTNER(victim) = 0;
        ROMANCE(ch) = 0;
        ROMANCE(victim) = 0;
        namesave(ch, victim);
        /* Done. You've dumped them. */
        return;
      }
      else if ((ROMANCE(ch) == 5) && (ROMANCE(victim) == 5))
      {
        /* For engagements/proposals */
        act("You cancel your wedding proposal to $N.\r\n", TRUE,
            ch, 0, victim, TO_CHAR);
        act("$n doesn't want to marry you anymore!\r\n", TRUE, ch,
            0, victim, TO_VICT);
        act("$n nullifies $s engagement proposal with $N!\r\n",
            TRUE, ch, 0, victim, TO_NOTVICT);
        /* Now set the variables to back to Dating */
        /* This is the only exception. You can change it to cancel
           Dating status as well. */
        ROMANCE(ch) = 1;
        ROMANCE(victim) = 1;
        /* Done. You've cancelled your proposal them. */
        return;
      }
      else
      {        /* Guess you're not involved after all. */
        ch->Send( "But you're not involved with %s!",
                         GET_NAME(victim));
        return;
      }
    }
  }
}

/* Function for the actual marriage */
void marry_them(Character *ch, Character *victim,
                Character *imm)
{
  char buf[MAX_STRING_LENGTH];
  /* Do standard checks.. */
  if (crashcheck_alpha(victim, imm) == 1)
  {
    return;
  }
  else if (crashcheck_alpha(ch, imm) == 1)
  {
    return;
  }
  if (ROMANCE(ch) != 2)
  {
    /* Groom isn't engaged */
    imm->Send("But %s isn't engaged!\r\n", GET_NAME(ch));
    return;
  }
  else if (ROMANCE(victim) != 2)
  {
    /* Bride isn't engaged */
    imm->Send( "But %s isn't engaged!\r\n", GET_NAME(victim));
    return;
  }
  else if (PARTNER(ch) != GET_IDNUM(victim))
  {
    /* Not engaged to each other */
    send_to_char("But they're not engaged to each other!\r\n", imm);
    return;
  }
  else if (PARTNER(victim) != GET_IDNUM(ch))
  {
    /* Not engaged to each other */
    send_to_char("But they're not engaged to each other!\r\n", imm);
    return;
  }
  else if (check_samesex(ch, victim) == 1)
  {
    /* Same Sex Marriages? */
    send_to_char("Same-sex marriages are not allowed.\r\n", imm);
    return;
  }
  else
  {
    if (GET_SEX(ch) != GET_SEX(victim))
    {
      /* Regular Marriage */
      /* They're engaged to each other, now perform the marriage. */
      ch->Send( "%s declares you married to %s!\r\n",   GET_NAME(imm), pi.NameById(PARTNER(ch)));
      victim->Send( "%s declares you married to %s!\r\n",   GET_NAME(imm), pi.NameById(PARTNER(victim)));
      imm->Send( "You declare %s and %s man and wife!\r\n",   GET_NAME(ch),GET_NAME(victim));
      snprintf(buf, sizeof(buf), "%s declares %s and %s man and wife!\r\n",
               GET_NAME(imm), GET_NAME(ch), GET_NAME(victim));
      act(buf, TRUE, 0, 0, 0, TO_ROOM);
    }
    else
    {          /* Same-sex Marriage */
      ch->Send( "%s declares you married to %s!\r\n",
                       GET_NAME(imm), pi.NameById(PARTNER(ch)));
      victim->Send( "%s declares you married to %s!\r\n",
                       GET_NAME(imm), pi.NameById(PARTNER(victim)));
      imm->Send("You declare %s and %s married!\r\n",
                       GET_NAME(ch), GET_NAME(victim));
      snprintf(buf, sizeof(buf), "%s declares %s and %s married!\r\n",
               GET_NAME(imm), GET_NAME(ch), GET_NAME(victim));
      act(buf, TRUE, 0, 0, 0, TO_NOTVICT);
    }
    PARTNER(ch) = GET_IDNUM(victim);
    PARTNER(victim) = GET_IDNUM(ch);
    /* Just in case! */
    ROMANCE(ch) = 3;
    ROMANCE(victim) = 3;
    /* Now we're set! Save it! */
    namesave(ch, victim);
    return;
  }
}

/* Romance Module -- Marry Two People */
/* THIS SHOULD BE AN IMM-LEVEL COMMAND! */

ACMD(do_marry)
{
  Character *groom;
  Character *bride;
  char groom_name[MAX_INPUT_LENGTH];
  char bride_name[MAX_INPUT_LENGTH];
  argument = one_argument(argument, groom_name);
  one_argument(argument, bride_name);
  if (!(groom = get_char_room_vis(ch, groom_name, NULL)))
  {
    /* Are they here? No! */
    ch->Send( "%s", CONFIG_NOPERSON);
    return;
  }
  else if (!(bride = get_char_room_vis(ch, bride_name, NULL)))
  {
    /* Are they here? No! */
    ch->Send( "%s", CONFIG_NOPERSON);
    return;
  }
  if (groom_name == bride_name)
  {
    /* Groom is the Bride? */
    send_to_char("You can't marry someone to themself!\r\n", ch);
    return;
  }
  else if (groom == ch)
  {
    /* Can't perform a ceremony on yourself. */
    send_to_char
      ("Sorry, you have to get someone ELSE to perform the ceremony!\r\n",
       ch);
    return;
  }
  else if (bride == ch)
  {
    /* Can't perform a ceremony on yourself. */
    send_to_char
      ("Sorry, you have to get someone ELSE to perform the ceremony!\r\n",
       ch);
    return;
  }
  else if ((!groom) || (!bride))
  {
    send_to_char("Which couple do you wish to marry?\r\n", ch);
    return;
  }
  else
  {            /* Let the marry function check the rest. */
    marry_them(groom, bride, ch);
  }
}

/* Romance Module - Divorce */

ACMD(do_divorce)
{
  Character *victim;
  
  char arg[MAX_INPUT_LENGTH];
  /* First, standard checks: Are you in a relationship? */
  if (ROMANCE(ch) != 3)
  {
    send_to_char("But you're not married!\r\n", ch);
    return;
  }
  else
  {
    /* Okay, you're involved.. */
    one_argument(argument, arg);
    if (!*arg)
    {          /* Break up with noone? */
      send_to_char("Whom do you want to divorce?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      ch->Send( "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {     /* Break up with yourself? */
      send_to_char("You can't divorce yourself!\r\n", ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) == 0)
    {
      send_to_char("But they're not romantically involved!\r\n", ch);
      return;
    }
    else if ((ROMANCE(victim) == 1)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      ch->Send( "But they're dating %s, not you!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 2)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      ch->Send("But they're engaged to %s, not you!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 3)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      ch->Send( "But they're married to %s, not you!\r\n",
                       pi.NameById(PARTNER(victim)));
      return;
    }
    else if (PARTNER(victim) != GET_IDNUM(ch))
    {
      ch->Send( "But %s isn't involved with you!\r\n",
                       GET_NAME(victim));
      return;
    }
    else
    {
      /* Okay, they're involved and with you... */
      /* Now we break them up! How FUN! */
      act("You yank $N's wedding ring off and throw it on the floor!\r\n", TRUE, ch, 0, victim, TO_CHAR);
      act("$n yanks $s wedding ring off and throws it on the floor!\r\n", TRUE, ch, 0, victim, TO_VICT);
      act("$n yanks $s wedding ring from $N off and throws it on the floor! DIVORCE!\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
      PARTNER(ch) = 0;
      PARTNER(victim) = 0;
      ROMANCE(ch) = 0;
      ROMANCE(victim) = 0;
      /* It's all reset, save changes. */
      changesave(ch, victim);
    }
  }
}

int crashcheck_alpha(Character *ch, Character *vict)
{
  /* Just some crash-preventing checks. */
  /* Returns 1 to indicate a crash will occur. */
  /* Return 0 if everything's ok */
  if (IN_ROOM(ch) == NULL)
  {
    return 1;
  }
  else if (IS_NPC(ch))
  {
    send_to_char("You have to do that to a player!\r\n", vict);
    return 1;
  }
  else if (IS_NPC(vict))
  {
    send_to_char("You can't do that!\r\n", vict);
    return 1;
  }
  else if (GET_POS(ch) < POS_RESTING)
  {
    send_to_char("You can't do that now!\r\n", vict);
    return 1;
  }
  else
  {
    return 0;
  }
}

/* MatingMod ACMDs and functions */
/* MATINGMOD v0.93 by Yoru-Hikage */
/* Special Thanks to Rawther (formerly) of AugMUD for his help */

/* First command, to conceive */
ACMD(do_seduce)
{
  Character *victim;
  
  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);
  victim = get_char_room_vis(ch, arg, NULL);
  if (PREG(ch) < 0)
  {
    
    if (!*arg)
    {          /* Mate with noone? */
      send_to_char("Whom do you want to mate with?\r\n", ch);
      return;
    }
    else if (!(victim))
    {
      /* Are they here? No! */
      ch->Send( "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {     /* Mate with yourself? */
      send_to_char("You can't mate with yourself!\r\n", ch);
      return;
    }
    if (GET_SEX(ch) == SEX_MALE)
    {
      send_to_char("Hey, only women can have kids..\r\n", ch);
      return;
    }
    else if (GET_SEX(victim) == SEX_FEMALE)
    {
      send_to_char("You need a male partner to conceive.\r\n", ch);
      return;
    }
    if ((ROMANCE(ch) != 3) && (OUT_OF_WEDLOCK == FALSE))
    {
      send_to_char
        ("Sorry, children may not be conceived out of wedlock here.\r\n",
         ch);
      return;
    }
    if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    if (GET_POS(ch) == POS_FIGHTING)
    {
      send_to_char
        ("You can't do that while fighting! It ruins the mood!\r\n",
         ch);
      return;
    }
    if (GET_POS(victim) == POS_FIGHTING)
    {
      send_to_char
        ("You have to wait until your mate finishes fighting!\r\n",
         ch);
      return;
    }
    if (GET_POS(ch) < POS_RESTING)
    {
      send_to_char("You can't do that now!\r\n", ch);
      return;
    }
    if (PREG(ch) == CANT)
    {
      send_to_char
        ("Sorry, you have to turn mating on to be able to mate.\r\n",
         ch);
      return;
    }
    if (PREG(victim) == CANT)
    {
      send_to_char("Sorry, they have turned mating off.\r\n", ch);
      return;
    }
    /* Routine Checks Performed */
    act("You ask $N if $E wishes to have a child with you.\r\n", TRUE,
        ch, 0, victim, TO_CHAR);
    act("$n asks if you want to have a child with $m.\r\n", TRUE, ch,
        0, victim, TO_VICT);
    PREG(ch) = ASKING_P;
    /* Players involved are informed, female is set to ASKING */
  }
  else
  {
    send_to_char("You're already pregnant!!\r\n", ch);
    return;
  }
}

/* For Men - Agree */
ACMD(do_consent)
{
  Character *victim;
  
  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);
  
  if (!*arg)
  {       /* Break up with noone? */
    send_to_char("Whom do you want to mate with?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Are they here? No! */
    ch->Send( "%s", CONFIG_NOPERSON);
    return;
  }
  else if (victim == ch)
  {  /* Break up with yourself? */
    send_to_char("You can't agree to mate with yourself!\r\n", ch);
    return;
  }
  if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  if (GET_POS(ch) == POS_FIGHTING)
  {
    send_to_char
      ("You can't do that while fighting! It ruins the mood!\r\n",
       ch);
    return;
  }
  if (GET_POS(victim) == POS_FIGHTING)
  {
    send_to_char
      ("You have to wait until your mate finishes fighting!\r\n",
       ch);
    return;
  }
  
  if (GET_SEX(victim) == SEX_MALE)
  {
    send_to_char("You need a female partner to mate with.\r\n", ch);
    return;
  }
  else if (GET_SEX(ch) == SEX_FEMALE)
  {
    send_to_char
      ("Perhaps you should be asking to mate instead of agreeing?\r\n",
       ch);
    return;
  }
  if ((ROMANCE(ch) != 3) && (OUT_OF_WEDLOCK == FALSE))
  {
    send_to_char
      ("Sorry, children may not be conceived out of wedlock here.\r\n",
       ch);
    return;
  }
  if (GET_POS(ch) < POS_RESTING)
  {
    send_to_char("You can't do that now!\r\n", ch);
    return;
  }
  if (PREG(victim) != -3)
  {
    send_to_char("But she's not asking to mate with you!\r\n", ch);
    return;
  }
  /* Routine Checks Performed */
  act("You agree to have a child with $N!\r\n", TRUE, ch, 0, victim,
      TO_CHAR);
  act("$n agrees to have a kid with you!\r\n", TRUE, ch, 0, victim,
      TO_VICT);
  act("$N announces that she's having $n's baby!\r\n", TRUE, ch, 0,
      victim, TO_NOTVICT);
  PREG(victim) = genpreg();   /* Generate a pregnancy time */
  /* All set. */
  
}

/* Deny fatherhood */

ACMD(do_deny)
{
  Character *victim;
  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);
  if (!*arg)
  {       /* Break up with noone? */
    send_to_char("Whom do you want to deny to mate with?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Are they here? No! */
    ch->Send( "%s", CONFIG_NOPERSON);
    return;
  }
  else if (victim == ch)
  {  /* Break up with yourself? */
    send_to_char("You can't mate with yourself anyway!\r\n", ch);
    return;
  }
  if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  if (GET_POS(ch) == POS_FIGHTING)
  {
    send_to_char("You can't do that while fighting!\r\n", ch);
    return;
  }
  if (GET_POS(victim) == POS_FIGHTING)
  {
    send_to_char
      ("You have to wait until your mate finishes fighting!\r\n",
       ch);
    return;
  }
  
  else if (GET_SEX(victim) == SEX_MALE)
  {
    send_to_char("You can only mate with females...\r\n", ch);
    return;
  }
  else if (GET_SEX(ch) == SEX_FEMALE)
  {
    send_to_char
      ("Perhaps you should be asking to mate instead of denying?\r\n",
       ch);
    return;
  }
  if (PREG(victim) != -3)
  {
    send_to_char("But she's not asking to mate with you!\r\n", ch);
    return;
  }
  act("You inform $N that you don't wish to mate with her.\r\n", TRUE,
      ch, 0, victim, TO_CHAR);
  act("$n doesn't feel like mating just yet.\r\n", TRUE, ch, 0, victim,
      TO_VICT);
  PREG(victim) = -1;          /* Inform & Reset */
}

/* Generates a pregnancy time */
int genpreg(void)
{
  int t, rand_time;
  t = (NINE_MONTHS);
  rand_time = dice(6, 6);     /* Max = 36 (One more mudmonth) Min = 6 (Less than one MudWeek) */
  t = t + rand_time + 2;
  return t;
}

/* Certain late-pregnancy symptoms.. */
void symptoms(Character *ch)
{
  struct obj_data *obj;
  
  int contract = 0;
  if ((PREG(ch) < (MONTHS_5)) && (PREG(ch) > 0))
  {
    /* She's past month 5 */
    int check = 0;
    check = dice(1, 20); /* Random Number, 1 - 20) */
    if (check > 10)
    {
      return;
    }               /* No symptoms */
    if (check >= 10 && check < 13)
    {
      act("You feel the baby kick! OW!\r\n", TRUE, ch, 0, 0,
          TO_CHAR);
      act("$n flinches in pain. Baby is kicking again!\r\n", TRUE,
          ch, 0, 0, TO_ROOM);
      damage(ch,ch,2, TYPE_UNDEFINED);
      return;
    }
    if (check >= 13 && check < 16)
    {
      act("You feel the baby kick! OW!\r\n", TRUE, ch, 0, 0,
          TO_CHAR);
      act("$n flinches in pain. Baby is kicking again!\r\n", TRUE,
          ch, 0, 0, TO_ROOM);
      damage(ch,ch, 1, TYPE_UNDEFINED);
      return;
    }
    if (check >= 15)
    {
      act("A wave of nausea washes over you, and you vomit.\r\n",
          TRUE, ch, 0, 0, TO_CHAR);
      act("$n suddenly turns very green, then stumbles into a corner and vomits up her breakfast.\r\n", TRUE, ch, 0, 0, TO_CHAR);
      gain_condition(ch, FULL, -2);
      gain_condition(ch, THIRST, -3);   /* Stomach's slightly emptier */
      return;
    }
  }
  if ((PREG(ch) <= (MONTH_1) && (PREG(ch) > 6)))
  {
    /* Near Birth  */
    contract = dice(2, 3);
    if (contract >= 4)
    {
      act("You double over as a contraction causes a wave of sheer pain to wash over your body!\r\n", TRUE, ch, 0, 0, TO_CHAR);
      act("$n groans and doubles over as a contraction shoots through her.\r\n", TRUE, ch, 0, 0, TO_ROOM);
      damage(ch,ch, contract, TYPE_UNDEFINED);
    }
  }
  if ((PREG(ch) <= 6) && (PREG(ch) > 0))
  {
    act("You scream as the pain from birthing burns in your blood...\r\n", TRUE, ch, 0, 0, TO_CHAR);
    act("$n screams loudly and howls in pain!\r\n", TRUE, ch, 0, 0,
        TO_ROOM);
  }
  if (PREG(ch) == 0)
  {
    /* Birthing */
    PREG(ch) = -1;
    send_to_all("A large, white stork flies high above you!\r\n");
    act("You are whisked away to a nearby hospital, and return moments later, carrying your new child!\r\n", TRUE, ch, 0, 0, TO_CHAR);
    act("$n is whisked away in an ambulance, and quickly returns, baby in hand.\r\n", TRUE, ch, 0, 0, TO_ROOM);
    
    obj = read_object(number(5, 6), VIRTUAL);
    if (obj)
    obj_to_char(obj, ch);
    if (REWARD_ALLOWED == TRUE)
    {     /* Rewards? */
      GET_MAX_HIT(ch) += HP_REWARD;
      GET_MAX_MANA(ch) += MP_REWARD;
      GET_STR(ch) += STR_REWARD;
      GET_CON(ch) += CON_REWARD;
      GET_DEX(ch) += DEX_REWARD;
      GET_WIS(ch) += WIS_REWARD;
      GET_INT(ch) += INT_REWARD;
      ch->affect_total();
    }
    damage(ch,ch, 100, TYPE_UNDEFINED);
  }
}

/* ABORT Command - MatingMod -- IMMORTAL LEVEL */
ACMD(do_abort)
{
  Character *victim;
  char arg[MAX_INPUT_LENGTH];
  if (GET_LEVEL(ch) <= LVL_HERO)
  {
    send_to_char("Only Immortals can use this command!\r\n", ch);
    return;
  }
  if (ABORT_ALLOWED != TRUE)
  {
    send_to_char("Sorry, abortions are not allowed here!\r\n", ch);
    return;
  }
  one_argument(argument, arg);
  if (!*arg)
  {       /* Mate with noone? */
    send_to_char("Whose pregnancy do you want to abort?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Are they here? No! */
    ch->Send( "%s", CONFIG_NOPERSON);
    return;
  }
  if (PREG(victim) < 0)
  {
    ch->Send( "But %s isn't pregnant!\r\n", GET_NAME(victim));
    return;
  }
  
  if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  /* All checks pass.. */
  if (SELF(victim, ch))
  {
    act("You makes a mystical gesture towards your abdomen.\r\nAfter a moment of pain, it shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_CHAR);
    act("$n makes a mystical gesture towards $N's abdomen.\r\nIt shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
  }
  else
  {
    act("You make a mystical gesture towards $N's abdomen.\r\nIt shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_CHAR);
    act("$n makes a mystical gesture towards your abdomen.\r\nAfter a moment of pain, it shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_VICT);
    act("$n makes a mystical gesture towards $N's abdomen.\r\nIt shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
 
  }
  PREG(victim) = NOT_PREG;
}

ACMD(mate_toggle)
{
  /* Romance/Mating Toggle On/Off
   * Precondition: Person must be single + not pregnant
   * Postcondition: If the person wants to not be bothered with
   * M/R-mod thingies, then this toggle will prevent that.
   * If they did not and now want to be involved in the
   * social scene, then now they can.
   */
  if (ROMANCE(ch) > 0)
  {
    send_to_char("Sorry, you have to be single to turn mating off.\n",
                 ch);
  }
  if (PREG(ch) > -1)
  {
    send_to_char("Sorry, you cannot turn Mating off while pregnant.\n",
                 ch);
  }
  /* Both Conditions pass.. Check + Set! */
  if (ROMANCE(ch) == 0)
  {
    ROMANCE(ch) = -1;
  }                 // OFF
  else if (ROMANCE(ch) == -1)
  {
    ROMANCE(ch) = 0;
  }                 // Single
  if ((PREG(ch) == -1) || (PREG(ch) == -2))
  {
    PREG(ch) = CANT;     // OFF
  }
  else if (PREG(ch) == CANT)
  {
    if (GET_SEX(ch) == MALE)
    {
      PREG(ch) = -2;
    }
    else
    {
      PREG(ch) = -1;
    }
  }
}                   /* EO mate_toggle */

char * romance_status(Character *ch)
{
  switch (ROMANCE(ch))
  {
  case 2:
    return (char *)"Engaged";
    break;
  case 3:
    return (char *)"Married";
    break;
  case 1:
    return (char *)"Dating";
    break;
  case 0:
    return (char *)"Single";
    break;
  default:
    return (char *)"Broken!";
    break;
  }
}
char * baby_status(Character *i, char * buf, size_t len)
{
  /* MatingMod Addition - Essential! */
  if ((PREG(i) > NOT_PREG) && !IS_NPC(i))
  {
    
    if (PREG(i) < 7)
      snprintf(buf, len, "%s %s giving birth!\r\n", GET_NAME(i), "is");
    else if (PREG(i) < (MONTHS_2)) // Customize these messages, if you want.
      snprintf(buf, len, "%s look%s as if %s may give birth to a healthy, baby sometime soon.\r\n",
               GET_NAME(i), "s","she"); // They're pretty cheesy.
    else if (PREG(i) < (MONTHS_3)) // Month 8
      snprintf(buf, len,   "%s %s a large, cumbersome bulge in %s waist.\r\n", GET_NAME(i),  "has", "her");
    else if (PREG(i) < (MONTHS_4)) // Month 7
      snprintf(buf, len, "%s %s already had to let out %s armor.\r\n", GET_NAME(i),  "has",  "her");
    else if (PREG(i) < (MONTHS_5)) // Month 6
      snprintf(buf, len, "%s %s a small bulge in %s midsection.\r\n",  GET_NAME(i),  "has",  "her");
    else if (PREG(i) < (MONTHS_6)) // Month 5
      snprintf(buf, len, "%s %s just beginning to show.\r\n",  GET_NAME(i),  "is");
    else
    {
      snprintf(buf, len, "%s %s a motherly glow...\r\n", GET_NAME(i), "has");
    }
    return buf;
  }
  else
  {
    return (char *)"\r\n";
  }
}
