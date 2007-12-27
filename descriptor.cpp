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
    int size_mxp;
    //   bool overf = FALSE;
    string stxt;
    /* if we're in the overflow state already, ignore this new output */
    if (this->bufspace == 0)
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


    /* work out how much we need to expand/contract it */
    size_mxp = count_mxp_tags(this->mxp, txt, size);
    if (size_mxp < 0)
        size_mxp = 0;

    /* this should safely put all expanded MXP tags into the string */
    stxt = convert_mxp_tags(this->mxp, txt, size + size_mxp);

    free( txt );

    /** don't wordwrap for folks who use mxp they can wrap at client side, or it may get fuzzled - mord**/
    if (this->character && !this->mxp && PRF_FLAGGED(this->character, PRF_PAGEWRAP) && PAGEWIDTH(this->character) > 0) {
        slen = stxt.size();
        wraplen = (((slen / PAGEWIDTH(this->character)) * 3) + slen + 3);
        /* at max you will be adding 3 more characters per line
         so this is just to make sure you have the room for this function. */

        stxt = wordwrap(stxt.c_str(), PAGEWIDTH(this->character), wraplen); //size checked above

    }

#if 0
    /*
     * If the text is too big to fit into even a large buffer, truncate
     * the new text to make it fit.  (This will switch to the overflow
     * state automatically because this->bufspace will end up 0.)
     */
    if (size + this->bufptr + 1 > LARGE_BUFSIZE) {
        log("Buffer Overflow: too big for buffer");
        size = LARGE_BUFSIZE - this->bufptr - 1;
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
    //  if (this->bufspace > size) {
    //this->output += string(cstring(stxt).c_str());
    this->output += cstring(stxt).c_str();
    //  strcpy(this->output + this->bufptr, txt); /* strcpy: OK (size checked above) */
    //this->bufspace -= size;
    //     this->bufptr += size;
    unlock_desc(this);
    return (LARGE_BUFSIZE -1);
    // }
#if 0

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
    free(txt);

    return (this->bufspace);
#endif
}

/* Initialize a descriptor */
void Descriptor::init_descriptor(int desc) {
    static int last_desc = 0;   /* last descriptor number */

    /* initialize descriptor data */

    *this->small_outbuf = 0;
    this->large_outbuf = NULL;
    this->descriptor = desc;
    this->character = NULL;
    this->idle_tics = 0;
    this->bufspace = SMALL_BUFSIZE - 1;
    this->login_time = time(0);
    this->output = string("");
    this->bufptr = 0;
    this->has_prompt = TRUE;  /* prompt is part of greetings */
    /*
     * This isn't exactly optimal but allows us to make a design choice.
     * Do we embed the history in descriptor_data or keep it dynamically
     * allocated and allow a user defined history size?
     */
    CREATE(this->history, char *, HISTORY_SIZE);

    if (++last_desc == 1000)
        last_desc = 1;
    this->desc_num = last_desc;


    CREATE(this->comp, struct compr, 1);
    this->comp->state = 0; /* we start in normal mode */
#ifdef HAVE_ZLIB_H

    this->comp->stream = NULL;
#endif /* HAVE_ZLIB_H */

    this->eor = 0;
    this->mxp = FALSE;

}

bool Descriptor::pending_output() {
    return !(this->output.empty());
}

Descriptor::Descriptor() {
    this->showstr_count = 0;
    this->telnet_capable = 0;
    this->locked = 0;
    this->close_me = 0;
    this->str = NULL;
    this->snoop_by = NULL;
    this->input.head = NULL;
    this->input.tail = NULL;
    this->snooping = NULL;
    this->inbuf[0] = '\0';
    this->history_pos = 0;
    this->output.erase();
    /** --- **/
    this->bad_pws = 0;
  this->idle_tics = 0;
  this->connected = 0;
  this->orig_connected = 0;
  this->sub_state = 0;
  this->desc_num = 0;
  this->login_time = 0;
  this->showstr_head = NULL; 
  this->showstr_vector = NULL; 
  this->showstr_page = 0;
  this->pagebuf[0] = '\0';
  this->backstr = NULL;
  this->max_str = 0;
  this->mail_to = 0;
  this->has_prompt = 0;
  this->wait = 0;
  this->history = NULL;
  this->bufptr = 0;
  this->bufspace = 0;
  this->large_outbuf = NULL;
  this->character = NULL;     /* linked to char                       */
  this->original = NULL;
  this->next = NULL;  
  this->olc = NULL;   /* OLC info                            */
  this->acc = NULL;
  this->callback_depth = 0;  
  this->c_data = NULL; 
  this->options = 0;
  this->comp = NULL; 
  this->eor = 0;
  this->mxp = 0;
}

Descriptor::~Descriptor() {
    //  delete this->output;
}
