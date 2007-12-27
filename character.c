//
// C Implementation: character
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

#include "conf.h"
#include "sysdep.h"

#if CIRCLE_GNU_LIBC_MEMORY_TRACK
# include <mcheck.h>
#endif

#ifdef CIRCLE_MACINTOSH       /* Includes for the Macintosh */
# define SIGPIPE 13
# define SIGALRM 14
/* GUSI headers */
# include <sys/ioctl.h>
/* Codewarrior dependant */
# include <SIOUX.h>
# include <console.h>
#endif

#ifdef CIRCLE_WINDOWS         /* Includes for Win32 */
# ifdef __BORLANDC__
#  include <dir.h>
# else                   /* MSVC */
#  include <direct.h>
# endif
# include <mmsystem.h>
#endif                   /* CIRCLE_WINDOWS */

#ifdef CIRCLE_AMIGA      /* Includes for the Amiga */
# include <sys/ioctl.h>
# include <clib/socket_protos.h>
#endif                   /* CIRCLE_AMIGA */

#ifdef CIRCLE_ACORN      /* Includes for the Acorn (RiscOS) */
# include <socklib.h>
# include <inetlib.h>
# include <sys/ioctl.h>
#endif

/*
 * Note, most includes for all platforms are in sysdep.h.  The list of
 * files that is included is controlled by conf.h for that platform.
 */

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "house.h"
#include "dg_scripts.h"
#include "screen.h"
#include "arena.h"
#include "mail.h"
#include "dg_event.h"
#include "clan.h"
#include "oasis.h"
#include "genolc.h"
#include "constants.h"
#include "ident.h"
#include "auction.h"

#ifdef HAVE_ARPA_TELNET_H
#include <arpa/telnet.h>
#else
#include "telnet.h"
#endif

/* ================== Structure for player/non-player ===================== */
size_t Character::Send(const char *messg, ...) {
  if (this && this->desc && messg && *messg)
  {
    size_t left;
    va_list args;
    
    va_start(args, messg);
    left = this->desc->vwrite_to_output(messg, args);
    va_end(args);
    return left;
  }
  return 0;
}

void Character::send_char_pos(int dam)
{
  switch (GET_POS(this)) {
  case POS_MORTALLYW:
    act("$n is mortally wounded, and will die soon, if not aided.",
        TRUE, this, 0, 0, TO_ROOM);
    this->Send( "You are mortally wounded, and will die soon, if not aided.\r\n");
    break;
  case POS_INCAP:
    act("$n is incapacitated and will slowly die, if not aided.", TRUE,
        this, 0, 0, TO_ROOM);
    this->Send( "You are incapacitated and will slowly die, if not aided.\r\n");
    break;
  case POS_STUNNED:
    act("$n is stunned, but will probably regain consciousness again.",
        TRUE, this, 0, 0, TO_ROOM);
    this->Send( "You're stunned, but will probably regain consciousness again.\r\n");
    break;
  case POS_DEAD:
    act("$n is dead!  R.I.P.", FALSE, this, 0, 0, TO_ROOM);
    this->Send( "You are dead!  Sorry...\r\n");
    break;
  default:             /* >= POSITION SLEEPING */
    if (dam > (GET_MAX_HIT(this) >> 2))
      act("That really did HURT!", FALSE, this, 0, 0, TO_CHAR);
    if (GET_HIT(this) < (GET_MAX_HIT(this) >> 2))
      this->Send("%sYou wish that your wounds would stop BLEEDING so much!%s\r\n",
               CCRED(this, C_SPR), CCNRM(this, C_SPR));
  }
}

