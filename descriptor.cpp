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
#include "db.h"
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
#include "descriptor.h"
#include "linkedlist.h"


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

    stxt = string( txt );
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

    for (unsigned int cnt = 0; cnt < HISTORY_SIZE; cnt++)
        history[cnt] = NULL;

}


//void Descriptor::close_socket()
Descriptor::~Descriptor() {
    Descriptor *temp;

    REMOVE_FROM_LIST(this, descriptor_list, next);
    CLOSE_SOCKET(this->descriptor);
    this->flush_queues();

    /* Forget snooping */
    if (this->snooping)
        this->snooping->snoop_by = NULL;

    if (this->snoop_by) {
        this->snoop_by->Output("Your victim is no longer among us.\r\n");
        this->snoop_by->snooping = NULL;
        this->snoop_by = NULL;
    }

    if (this->character) {
        /* If we're switched, this resets the mobile taken. */
        this->character->desc = NULL;

        /* Plug memory leak, from Eric Green. */
        if (!IS_NPC(this->character) && PLR_FLAGGED(this->character, PLR_MAILING) && this->str) {
            if (*(this->str))
                free(*(this->str));
            free(this->str);
            this->str = NULL;
        } else if (this->backstr && !IS_NPC(this->character) && !PLR_FLAGGED(this->character, PLR_WRITING)) {
            free(this->backstr);      /* editing description ... not olc */
            this->backstr = NULL;
        }
        if (IS_PLAYING(this) || STATE(this) == CON_DISCONNECT) {
            Character *link_challenged = this->original ? this->original : this->character;

            /* We are guaranteed to have a person. */
            act("$n has lost $s link.", TRUE, link_challenged, 0, 0, TO_ROOM);
            //save_char(link_challenged);
            new_mudlog(NRM, MAX(LVL_IMMORT, GET_INVIS_LEV(link_challenged)), TRUE, "Closing link to: %s.", GET_NAME(link_challenged));
        } else {
            new_mudlog(CMP, LVL_IMMORT, TRUE, "Losing player: %s.", GET_NAME(this->character) ? GET_NAME(this->character) : "<null>");
            free_char(this->character);
        }
    } else
        new_mudlog(CMP, LVL_IMMORT, TRUE, "Losing descriptor without char.");

    /* JE 2/22/95 -- part of my unending quest to make switch stable */
    if (this->original && this->original->desc)
        this->original->desc = NULL;

    /* Clear the command history. */
    if (this->history) {
        for (unsigned int cnt = 0; cnt < HISTORY_SIZE; cnt++)
            if (this->history[cnt])
                free(this->history[cnt]);
    }

    if (this->showstr_head)
        free(this->showstr_head);
    if (this->showstr_count)
        free(this->showstr_vector);

    /*. Kill any OLC stuff .*/
    switch (this->connected) {
    case CON_OEDIT:
    case CON_REDIT:
    case CON_ZEDIT:
    case CON_MEDIT:
    case CON_SEDIT:
    case CON_TEDIT:
    case CON_AEDIT:
    case CON_TRIGEDIT:
        cleanup_olc(this, CLEANUP_ALL);
        break;
    default:
        break;
    }

    /* free compression structures */
#ifdef HAVE_ZLIB_H
    if (this->comp->stream) {
        deflateEnd(this->comp->stream);
        free(this->comp->stream);
        free(this->comp->buff_out);
        free(this->comp->buff_in);

    }
#endif /* HAVE_ZLIB_H */
    /* d->comp was still created even if there is no zlib, for comp->state) */
    if (this->comp)
        free(this->comp);
}


/* Empty the queues before closing connection */
void Descriptor::flush_queues() {
    if (large_outbuf) {
        large_outbuf->next = bufpool;
        bufpool = large_outbuf;
    }
    while (input.head) {
        struct txt_block *tmp = input.head;
        input.head = input.head->next;
        free(tmp->text);
        free(tmp);
    }
}


int lock_desc(Descriptor *d) {
    if (!d)
        return -1;

    if (d->locked)
        log("Descriptor is being locked when it is already in a locked state.");

    d->locked = TRUE;

    return TRUE;
}

int unlock_desc(Descriptor *d) {
    if (!d)
        return -1;

    if (!d->locked)
        log("Descriptor is being unlocked when it is already in an unlocked state.");

    d->locked = FALSE;

    return TRUE;
}


int is_locked(Descriptor *d) {
    if (!d)
        return -1;

    if (d->locked)
        return TRUE;
    else
        return FALSE;
}

void delete_descriptor_list_node(Descriptor *d) {
    if (!d)
        return;

    if (d->next)
        delete_descriptor_list_node(d->next);

    delete d;
}
void delete_descriptor_list() {
    delete_descriptor_list_node(descriptor_list);
    descriptor_list = NULL;
}



size_t Descriptor::Output(string i) {

    /* this should safely put all expanded MXP tags into the string */
   stxt = convert_mxp_tags(mxp, i);

    /** don't wordwrap for folks who use mxp they can wrap at client side, or it may get fuzzled - mord**/
    if (character && !mxp && PRF_FLAGGED(character, PRF_PAGEWRAP) && PAGEWIDTH(character) > 0) {
        size_t slen = stxt.size();
        size_t wraplen = (((slen / PAGEWIDTH(character)) * 3) + slen + 3);
        /* at max you will be adding 3 more characters per line
         so this is just to make sure you have the room for this function. */

        stxt = wordwrap(stxt.c_str(), PAGEWIDTH(character), wraplen); //size checked above

    }
    output += cstring(stxt).c_str();
    return output.size();
}


