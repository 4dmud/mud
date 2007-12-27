//
// C++ Implementation: descriptor
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
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


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
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
extern struct txt_block *bufpool;  /* pool of large output buffers */
extern int buf_largecount;       /* # of large buffers which exist */
extern int buf_overflows;        /* # of overflows of output */
extern int buf_switches;         /* # of switches from small to large buf */

/* Add a new string to a player's output queue. For outside use. */
size_t Descriptor::Output(const char *txt, ...)
{
  va_list args;
  size_t left;
  if (!this)
    return 0;
  
  va_start(args, txt);
  left = vwrite_to_output(txt, args);
  va_end(args);
  
  return left;
}

/* Add a new string to a player's output queue. */
size_t Descriptor::vwrite_to_output(const char *format, va_list args)
{
  const char *text_overflow = "\r\nOVERFLOW\r\n";
  static char txt[MAX_STRING_LENGTH];
  size_t size;
  int size_mxp;
  
  *txt = '\0';
  /* if we're in the overflow state already, ignore this new output */
  if (this->bufspace == 0)
    return (0);
  
  lock_desc(this);
  
  size = vsnprintf(txt, sizeof(txt), format, args);
  if (this->character)
    size = proc_color(txt, (clr(this->character, C_NRM)), sizeof(txt));
  
  
  /* work out how much we need to expand/contract it */
  size_mxp = count_mxp_tags(this->mxp, txt, strlen(txt));
  if (size_mxp < 0)
    size_mxp = 0;
  //size = size_mxp + strlen(txt);
  
  do
  {
    char dest[size + size_mxp + 20];
    int mxpon = this->mxp;
    dest[0] = '\0';
    if (size > sizeof(txt))
      mxpon = 0;
    convert_mxp_tags(mxpon, dest, txt, size);
    
    if (size + size_mxp > sizeof(txt))
      log("Mxp cut off");
    size = strlcpy(txt, dest, sizeof(txt)); /*this may cause overflows? */
  }
  while(0);
  
  
  /** don't wordwrap for folks who use mxp they can wrap at client side, or it may get fuzzled - mord**/
  if (this->character && !this->mxp && PRF_FLAGGED(this->character, PRF_PAGEWRAP) && PAGEWIDTH(this->character) > 0)
  {
    int len = strlen(txt);
    /* at max you will be adding 3 more characters per line
     so this is just to make sure you have the room for this function. */
    
    char dest[(((len / PAGEWIDTH(this->character)) * 3) + len + 3)];
    wordwrap(txt, dest, PAGEWIDTH(this->character), len); //size checked above
    
    size = strlcpy(txt, dest, sizeof(txt));
    
  }
  /* If exceeding the size of the buffer, truncate it for the overflow message */
  if (size >= sizeof(txt))
  {
    log("Output exceeding size of buffer");
    size = sizeof(txt) - 1;
    strcpy(txt + size - strlen(text_overflow), text_overflow);   /* strcpy: OK */
  }
  
  /*
   * If the text is too big to fit into even a large buffer, truncate
   * the new text to make it fit.  (This will switch to the overflow
   * state automatically because this->bufspace will end up 0.)
   */
  if (size + this->bufptr + 1 > LARGE_BUFSIZE)
  {
  log("Buffer Overflow: too big for buffer");
    size = LARGE_BUFSIZE - this->bufptr - 1;
    if (size > 0)
      txt[size] = '\0';
    else
      txt[0] = '\0';
    buf_overflows++;
  }
  
  /*
   * If we have enough space, just write to buffer and that's it! If the
   * text just barely fits, then it's switched to a large buffer instead.
   */
  if (this->bufspace > size)
  {
    
    strcpy(this->output + this->bufptr, txt); /* strcpy: OK (size checked above) */
    this->bufspace -= size;
    this->bufptr += size;
    unlock_desc(this);
    return (this->bufspace);
  }
  
  buf_switches++;
  
  /* if the pool has a buffer in it, grab it */
  if (bufpool != NULL) {
    this->large_outbuf = bufpool;
    bufpool = bufpool->next;
  } else {               /* else create a new one */
    CREATE(this->large_outbuf, struct txt_block, 1);
    CREATE(this->large_outbuf->text, char, LARGE_BUFSIZE);
    buf_largecount++;
  }
  *this->large_outbuf->text = '\0';
  strcpy(this->large_outbuf->text, this->output);  /* strcpy: OK (size checked previously) */
  this->output = this->large_outbuf->text;    /* make big buffer primary */
  strcat(this->output, txt);     /* strcat: OK (size checked) */
  
  /* set the pointer for the next write */
  this->bufptr = strlen(this->output);
  
  /* calculate how much space is left in the buffer */
  this->bufspace = LARGE_BUFSIZE - 1 - this->bufptr;
  unlock_desc(this);
  return (this->bufspace);
}

