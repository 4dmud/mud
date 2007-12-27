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
#include "col_string.h"
#include "descriptor.h"

extern struct txt_block *bufpool;  /* pool of large output buffers */
extern int buf_largecount;       /* # of large buffers which exist */
extern int buf_overflows;        /* # of overflows of output */
extern int buf_switches;         /* # of switches from small to large buf */

/* Add a new string to a player's output queue. For outside use. */
size_t Descriptor::Output(const char *txt, ...) {
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
size_t Descriptor::vwrite_to_output(const char *format, va_list args) {
    //   const char *text_overflow = "\r\nOVERFLOW\r\n";

    //  const int maxSize = 1024 * 64; /* max size 60 KB */
    char *txt;//, *tmp;
    size_t size, len = MAX_INPUT_LENGTH * 3, slen, wraplen;
//    int size_mxp;
    //   bool overf = FALSE;
    string stxt;
    /* if we're in the overflow state already, ignore this new output */
    if (bufspace == 0)
        return (0);

    lock_desc(this);

    if (strlen(format) > len)
        len = strlen(format) + 1;

    /* this call will slow it down for now */
    txt = (char *)malloc(len);

    size = vsnprintf(txt, len - 1, format, args);
    /** too big for buffer! Overflow! lets reallocate! - Mord **/
    while (size == -1 || size >= len) {
        // Try a bigger size
        len *= 2;
        txt = (char *)realloc(txt, len);
        //strlcpy(tmp, txt, len);
        //free( txt );
        //txt = tmp;
        size = vsnprintf(txt, len, format, args);
    }

stxt = string(txt);
free( txt );
    /* work out how much we need to expand/contract it */
    /*size_mxp = count_mxp_tags(mxp, txt, size);
    if (size_mxp < 0)
        size_mxp = 0;*/

    /* this should safely put all expanded MXP tags into the string */
    stxt = convert_mxp_tags(mxp, stxt);

    

    /** don't wordwrap for folks who use mxp they can wrap at client side, or it may get fuzzled - mord**/
    if (character && !mxp && PRF_FLAGGED(character, PRF_PAGEWRAP) && PAGEWIDTH(character) > 0) {
        slen = stxt.size();
        wraplen = (((slen / PAGEWIDTH(character)) * 3) + slen + 3);
        /* at max you will be adding 3 more characters per line
         so this is just to make sure you have the room for this function. */

        stxt = wordwrap(stxt.c_str(), PAGEWIDTH(character), wraplen); //size checked above

    }

#if 0
    /*
     * If the text is too big to fit into even a large buffer, truncate
     * the new text to make it fit.  (This will switch to the overflow
     * state automatically because bufspace will end up 0.)
     */
    if (size + bufptr + 1 > LARGE_BUFSIZE) {
        log("Buffer Overflow: too big for buffer");
        size = LARGE_BUFSIZE - bufptr - 1;
        if (size > 0)
            txt[size] = '\0';
        else
            txt[0] = '\0';
        buf_overflows++;
    }
#endif

    /*
     * If we have enough space, just write to buffer and that's it! If the
     * text just barely fits, then it's switched to a large buffer instead.
     */
    //  if (bufspace > size) {
    //output += string(cstring(stxt).c_str());
    output += cstring(stxt).c_str();
    //  strcpy(output + bufptr, txt); /* strcpy: OK (size checked above) */
    //bufspace -= size;
    //     bufptr += size;
    unlock_desc(this);
    return (LARGE_BUFSIZE -1);
    // }
#if 0

    buf_switches++;

    /* if the pool has a buffer in it, grab it */
    if (bufpool != NULL) {
        large_outbuf = bufpool;
        bufpool = bufpool->next;
    } else {               /* else create a new one */
        CREATE(large_outbuf, struct txt_block, 1);
        CREATE(large_outbuf->text, char, LARGE_BUFSIZE);
        buf_largecount++;
    }
    *large_outbuf->text = '\0';
    strcpy(large_outbuf->text, output);  /* strcpy: OK (size checked previously) */
    output = large_outbuf->text;    /* make big buffer primary */
    strcat(output, txt);     /* strcat: OK (size checked) */

    /* set the pointer for the next write */
    bufptr = strlen(output);

    /* calculate how much space is left in the buffer */
    bufspace = LARGE_BUFSIZE - 1 - bufptr;
    unlock_desc(this);
    free(txt);

    return (bufspace);
#endif
}

/* Initialize a descriptor */
void Descriptor::init_descriptor(int desc) {
    static int last_desc = 0;   /* last descriptor number */

    /* initialize descriptor data */

    *small_outbuf = 0;
    descriptor = desc;
    bufspace = SMALL_BUFSIZE - 1;
    login_time = time(0);
    output = string("");
    has_prompt = TRUE;  /* prompt is part of greetings */
    /*
     * This isn't exactly optimal but allows us to make a design choice.
     * Do we embed the history in descriptor_data or keep it dynamically
     * allocated and allow a user defined history size?
     */
    CREATE(history, char *, HISTORY_SIZE);

    if (++last_desc == 1000)
        last_desc = 1;
    desc_num = last_desc;


    CREATE(comp, struct compr, 1);
    comp->state = 0; /* we start in normal mode */
#ifdef HAVE_ZLIB_H

    comp->stream = NULL;
#endif /* HAVE_ZLIB_H */

}

bool Descriptor::pending_output() {
    return !(output.empty());
}

Descriptor::Descriptor() {
    showstr_count = 0;
    telnet_capable = 0;
    locked = 0;
    close_me = 0;
    str = NULL;
    snoop_by = NULL;
    input.head = NULL;
    input.tail = NULL;
    snooping = NULL;
    inbuf[0] = '\0';
    history_pos = 0;
    output.erase();
    /** --- **/
    bad_pws = 0;
  idle_tics = 0;
  connected = 0;
  orig_connected = 0;
  sub_state = 0;
  desc_num = 0;
  login_time = 0;
  showstr_head = NULL;
  showstr_vector = NULL;
  showstr_page = 0;
  pagebuf[0] = '\0';
  backstr = NULL;
  max_str = 0;
  mail_to = 0;
  has_prompt = 0;
  wait = 0;
  history = NULL;
  bufptr = 0;
  bufspace = 0;
  large_outbuf = NULL;
  character = NULL;     /* linked to char                       */
  original = NULL;
  next = NULL;
  olc = NULL;   /* OLC info                            */
  acc = NULL;
  callback_depth = 0;
  c_data = NULL;
  options = 0;
  comp = NULL;
  eor = FALSE;
  mxp = FALSE;
}

Descriptor::~Descriptor() {
    //  delete output;
}
