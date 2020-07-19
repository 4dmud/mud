/***************************************************************************
 *  Original Diku Mud copyright (C) 1990, 1991 by Sebastian Hammer,        *
 *  Michael Seifert, Hans Henrik St{rfeldt, Tom Madsen, and Katja Nyboe.   *
 *                                                                         *
 *  Merc Diku Mud improvments copyright (C) 1992, 1993 by Michael          *
 *  Chastain, Michael Quan, and Mitchell Tse.                              *
 *                                                                         *
 *  In order to use any part of this Merc Diku Mud, you must comply with   *
 *  both the original Diku license in 'license.doc' as well the Merc       *
 *  license in 'license.txt'.  In particular, you may not remove either of *
 *  these copyright notices.                                               *
 *                                                                         *
 *  Much time and thought has gone into this software and you are          *
 *  benefitting.  We hope that you share your changes too.  What goes      *
 *  around, comes around.                                                  *
 ***************************************************************************/

/***************************************************************************
*	ROM 2.4 is copyright 1993-1996 Russ Taylor			   *
*	ROM has been brought to you by the ROM consortium		   *
*	    Russ Taylor (rtaylor@efn.org)				   *
*	    Gabrielle Taylor						   *
*	    Brian Moore (zump@rom.org)					   *
*	By using this code, you have agreed to follow the terms of the	   *
*	ROM license, in the file Rom24/doc/rom.license			   *
***************************************************************************/

#if 0
#include <sys/types.h>
#include <sys/time.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "db.h"
#include "structs.h"
#endif
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
#include "improved-edit.h"
#include "dg_scripts.h"
#include "spells.h"
#include "clan.h"
#include "descriptor.h"

/* globals from db.c for load_notes */
#if !defined(macintosh)
extern  int     _filbuf ( FILE * );
#endif
//extern FILE *                  fpArea;
//extern char                    strArea[MAX_INPUT_LENGTH];

char log_buf[READ_SIZE];
void smash_tilde ( char *str );


#define fread_string(fp)  fread_string(fp, log_buf)

/* local procedures */
void load_thread ( const char *name, NOTE_DATA **list, int type, time_t free_time );
void parse_note ( Character *ch, char *argument, int type );
bool hide_note ( Character *ch, NOTE_DATA *pnote );
void free_note ( NOTE_DATA *note, int type );
int has_note ( Character *ch, int type );
void free_all_notes ( void );
void free_note_list ( NOTE_DATA *note, int type );
void free_char_note ( NOTE_DATA *note );


NOTE_DATA *new_note ( void );

NOTE_DATA *note_list;
NOTE_DATA *idea_list;
NOTE_DATA *penalty_list;
NOTE_DATA *news_list;
NOTE_DATA *changes_list;
NOTE_DATA *note_free;

#define IS_TRUSTED(ch, num) (GET_LEVEL(ch) > LVL_HERO)
#define IS_IMMORTAL(ch)     (GET_LEVEL(ch) >= LVL_IMMORT)
#define IS_ADMIN(ch)        (GET_LEVEL(ch) == LVL_IMPL)

int count_spool ( Character *ch, NOTE_DATA *spool )
{
    int count = 0;
    NOTE_DATA *pnote;

    for ( pnote = spool; pnote != NULL; pnote = pnote->next )
        if ( !hide_note ( ch,pnote ) )
            count++;

    return count;
}

void do_unread ( Character *ch )
{
    int count;
    bool found = FALSE;

    if ( IS_NPC ( ch ) )
        return;

    if ( ( count = count_spool ( ch,news_list ) ) > 0 )
    {
        found = TRUE;
        ch->Send ( "There %s %d new news article%s waiting.\r\n",
                   count > 1 ? "are" : "is",count, count > 1 ? "s" : "" );
    }
    if ( ( count = count_spool ( ch,changes_list ) ) > 0 )
    {
        found = TRUE;
        ch->Send ( "There %s %d change%s waiting to be read.\r\n",
                   count > 1 ? "are" : "is", count, count > 1 ? "s" : "" );
    }
    if ( ( count = count_spool ( ch,note_list ) ) > 0 )
    {
        found = TRUE;
        ch->Send ( "You have %d new note%s waiting.\r\n",
                   count, count > 1 ? "s" : "" );
    }
    if ( ( count = count_spool ( ch,idea_list ) ) > 0 )
    {
        found = TRUE;
        ch->Send ( "You have %d unread idea%s to peruse.\r\n",
                   count, count > 1 ? "s" : "" );
    }
    if ( IS_TRUSTED ( ch,ANGEL ) && ( count = count_spool ( ch,penalty_list ) ) > 0 )
    {
        found = TRUE;
        ch->Send ( "%d %s been added.\r\n",
                   count, count > 1 ? "penalties have" : "penalty has" );
    }

    if ( !found )
        ch->Send ( "You have no unread notes.\r\n" );
}

ACMD ( do_note )
{
    parse_note ( ch,argument,NOTE_NOTE );
}

ACMD ( do_idea )
{
    parse_note ( ch,argument,NOTE_IDEA );
}

ACMD ( do_penalty )
{
    parse_note ( ch,argument,NOTE_PENALTY );
}

ACMD ( do_news )
{
    parse_note ( ch,argument,NOTE_NEWS );
}

ACMD ( do_changes )
{
    parse_note ( ch,argument,NOTE_CHANGES );
}

void save_notes ( int type )
{
    FILE *fp;
    const char *name;
    char *pt;
    NOTE_DATA *pnote;

    switch ( type )
    {
        default:
            return;
        case NOTE_NOTE:
            name = NOTE_FILE;
            pnote = note_list;
            break;
        case NOTE_IDEA:
            name = IDEAS_FILE;
            pnote = idea_list;
            break;
        case NOTE_PENALTY:
            name = PENALTY_FILE;
            pnote = penalty_list;
            break;
        case NOTE_NEWS:
            name = NEWSNOTE_FILE;
            pnote = news_list;
            break;
        case NOTE_CHANGES:
            name = CHANGES_FILE;
            pnote = changes_list;
            break;
    }

    //    fclose( fpReserve );
    if ( ( fp = fopen ( name, "w" ) ) == NULL )
    {
        perror ( name );
    }
    else
    {
        for ( ; pnote != NULL; pnote = pnote->next )
        {
            pt = pnote->sender;
            skip_spaces ( &pt );
            fprintf ( fp, "Sender %s~\n", pt );
            pt = pnote->date;
            skip_spaces ( &pt );
            fprintf ( fp, "Date %s~\n", pt );
            fprintf ( fp, "Stamp %ld\n", pnote->date_stamp );
            pt = pnote->to_list;
            skip_spaces ( &pt );
            fprintf ( fp, "To %s~\n", pt );
            fprintf ( fp, "Item %d\n",  pnote->item_number );
            pt = pnote->subject;
            skip_spaces ( &pt );
            fprintf ( fp, "Subject %s~\n", pt );
            pt = pnote->text;
            skip_spaces ( &pt );
            fprintf ( fp, "Text\n%s~\n",   pt );
        }
        fclose ( fp );
        return;
    }
}
void load_notes ( void )
{
    load_thread ( NOTE_FILE,&note_list, NOTE_NOTE, 31*24*60*60 );
    load_thread ( IDEAS_FILE,&idea_list, NOTE_IDEA, 28*24*60*60 );
    load_thread ( PENALTY_FILE,&penalty_list, NOTE_PENALTY, 0 );
    load_thread ( NEWSNOTE_FILE,&news_list, NOTE_NEWS, 0 );
    load_thread ( CHANGES_FILE,&changes_list,NOTE_CHANGES, 0 );
}

void load_thread ( const char *name, NOTE_DATA **list, int type, time_t free_time )
{
    FILE *fp;
    NOTE_DATA *pnotelast;
    char *word = NULL;

    const char *keywords[] =
    {
        "Sender",
        "Date",
        "Stamp",
        "To",
        "Item",
        "Subject",
        "Text"
    };

    if ( ( fp = fopen ( name, "r" ) ) == NULL )
        return;

    pnotelast = NULL;
    for ( ; ; )
    {
        NOTE_DATA *pnote;
        char letter;

        do
        {
            letter = getc ( fp );
            if ( feof ( fp ) )
            {
                fclose ( fp );
                return;
            }
        }
        while ( isspace ( letter ) );
        ungetc ( letter, fp );

        CREATE ( pnote , NOTE_DATA, 1 );

        if ( !compares ( ( word = fread_word ( fp ) ), keywords[0] ) )
            break;
        pnote->sender   = fread_string ( fp );

        if ( !compares ( ( word = fread_word ( fp ) ), keywords[1] ) )
            break;
        pnote->date     = fread_string ( fp );

        if ( !compares ( ( word = fread_word ( fp ) ), keywords[2] ) )
            break;
        pnote->date_stamp = fread_number ( fp );


        if ( !compares ( ( word = fread_word ( fp ) ), keywords[3] ) )
            break;
        pnote->to_list  = fread_string ( fp );

        if ( !compares ( ( word = fread_word ( fp ) ), keywords[4] ) )
            break;
        pnote->item_number  = fread_number ( fp );


        if ( !compares ( ( word = fread_word ( fp ) ), keywords[5] ) )
            break;
        pnote->subject  = fread_string ( fp );

        if ( !compares ( ( word = fread_word ( fp ) ), keywords[6] ) )
            break;
        pnote->text     = fread_string ( fp );

        if ( free_time && pnote->date_stamp < time ( 0 ) - free_time )
        {
            free_note ( pnote, type );
            continue;
        }

        pnote->type = type;

        if ( *list == NULL )
            *list           = pnote;
        else
            pnotelast->next     = pnote;

        pnotelast       = pnote;
    }



    log ( "BUG: Load_notes: bad key word in file %s. [%s]" , name, word );
    exit ( 1 );
    return;
}

void append_note ( NOTE_DATA *pnote )
{
    FILE *fp;
    const char *name;
    char *pt;
    NOTE_DATA **list;
    NOTE_DATA *last;

    switch ( pnote->type )
    {
        default:
            return;
        case NOTE_NOTE:
            name = NOTE_FILE;
            list = &note_list;
            break;
        case NOTE_IDEA:
            name = IDEAS_FILE;
            list = &idea_list;
            break;
        case NOTE_PENALTY:
            name = PENALTY_FILE;
            list = &penalty_list;
            break;
        case NOTE_NEWS:
            name = NEWSNOTE_FILE;
            list = &news_list;
            break;
        case NOTE_CHANGES:
            name = CHANGES_FILE;
            list = &changes_list;
            break;
    }

    if ( *list == NULL )
        *list = pnote;
    else
    {
        for ( last = *list; last->next != NULL; last = last->next );
        last->next = pnote;
    }

    //fclose(fpReserve);
    if ( ( fp = fopen ( name, "a" ) ) == NULL )
    {
        perror ( name );
    }
    else
    {
        pt = pnote->sender;
        skip_spaces ( &pt );
        fprintf ( fp, "Sender %s~\n", pt );
        pt = pnote->date;
        skip_spaces ( &pt );
        fprintf ( fp, "Date %s~\n", pt );
        fprintf ( fp, "Stamp %ld\n", pnote->date_stamp );
        pt = pnote->to_list;
        skip_spaces ( &pt );
        fprintf ( fp, "To %s~\n", pt );
        fprintf ( fp, "Item %d\n",  pnote->item_number );
        pt = pnote->subject;
        skip_spaces ( &pt );
        fprintf ( fp, "Subject %s~\n", pt );
        pt = pnote->text;
        skip_spaces ( &pt );
        fprintf ( fp, "Text\n%s~\n",   pt );
        fclose ( fp );
    }
    //fpReserve = fopen( NULL_FILE, "r" );
}


string list_account_names ( Character *ch )
{
    int acc, pos;
    string nlist ( "" );


    try
    {
        pos = pi.TableIndexById ( GET_ID ( ch ) );
    }
    catch ( MudException &e )
    {
        return string ( GET_NAME ( ch ) );
    }

    acc = pi.AccByIndex ( pos );

    for ( pos = 0; pos <= pi.TopOfTable(); pos++ )
    {
        if ( !pi.DeletedByIndex ( pos ) && pi.AccByIndex ( pos ) == acc )
        {
            nlist += pi.NameByIndex ( pos );
            nlist += " ";
        }
    }
    return nlist;

}

bool is_note_to ( Character *ch, NOTE_DATA *pnote )
{
    if ( !str_cmp ( GET_NAME ( ch ), pnote->sender ) )
        return TRUE;

    if ( is_name ( "all", pnote->to_list ) )
        return TRUE;

    if ( IS_IMMORTAL ( ch ) &&
            ( is_name ( "immortal", pnote->to_list ) || ( is_name ( "imm", pnote->to_list ) ) ) )
        return TRUE;

    if ( IS_ADMIN ( ch )  && is_name ( "admin", pnote->to_list ) )
        return TRUE;

    if ( GET_CLAN ( ch ) && is_name ( clan_name ( find_clan_by_id ( GET_CLAN ( ch ) ) ), pnote->to_list ) )
        return TRUE;

    if ( PLR_FLAGGED ( ch, PLR_HERO ) && ( is_name ( "hero", pnote->to_list ) || is_name ( "heros", pnote->to_list ) ) )
        return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_RP_LEADER ) && is_name ( "rpl", pnote->to_list ) )
        return TRUE;

// Temp remove this. Prometheus
//	if ( PLR_FLAGGED ( ch, PLR_ROLEPLAYER ) && is_name ( "rp", pnote->to_list ) )
//		return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_PK ) && is_name ( "pk", pnote->to_list ) )
        return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_NEWBIE_HLPR ) && is_name ( "helper", pnote->to_list ) )
        return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_JESTER ) && is_name ( "jester", pnote->to_list ) )
                return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_CTHULYTE ) && is_name ( "cthulyte", pnote->to_list ) )
                return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_ALDERISIO ) && is_name ( "alderisio", pnote->to_list ) )
                return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_BITCH ) && is_name ( "bitch", pnote->to_list ) )
                return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_RIDDLER ) && is_name ( "riddler", pnote->to_list ) )
                return TRUE;
    if ( PLR_FLAGGED ( ch, PLR_LOLTHYTE ) && is_name ( "lolthyte", pnote->to_list ) )
                return TRUE;
        if ( PLR_FLAGGED ( ch, PLR_FEARLESS ) && is_name ( "fearless", pnote->to_list ) )
                return TRUE;
        if ( PLR_FLAGGED ( ch, PLR_GALLIANO ) && is_name ( "galliano", pnote->to_list ) )
                return TRUE;
    if ( is_name ( GET_NAME ( ch ), pnote->to_list ) )
        return TRUE;
    if ( is_name ( list_account_names ( ch ).c_str(), pnote->to_list ) )
        return TRUE;

    return FALSE;
}



void note_attach ( Character *ch, int type )
{
    NOTE_DATA *pnote;

    if ( ch->pnote != NULL )
        return;

    pnote = new_note();

    pnote->next		= NULL;
    pnote->sender	= str_dup ( GET_NAME ( ch ) );
    pnote->type		= type;
    ch->pnote		= pnote;
    return;
}



void note_remove ( Character *ch, NOTE_DATA *pnote, bool del )
{
    char to_new[MAX_INPUT_LENGTH];
    char to_one[MAX_INPUT_LENGTH];
    //    NOTE_DATA *prev;
    //NOTE_DATA **list;
    char *to_list;
    int type;

    if ( del == 0 )
    {
        /* make a new list */
        to_new[0]	= '\0';
        to_list	= pnote->to_list;
        while ( *to_list != '\0' )
        {
            to_list	= any_one_arg ( to_list, to_one );
            if ( to_one[0] != '\0' && str_cmp ( GET_NAME ( ch ), to_one ) )
            {
                strcat ( to_new, " " );
                strcat ( to_new, to_one );
            }
        }
        /* Just a simple recipient removal? */
        if ( str_cmp ( GET_NAME ( ch ), pnote->sender ) && to_new[0] != '\0' )
        {
            free_string ( &pnote->to_list );
            pnote->to_list = str_dup ( to_new + 1 );
            return;
        }
    }
    /* nuke the whole note */
/*
    switch ( pnote->type )
    {
        default:
            return;
        case NOTE_NOTE:
            list = &note_list;
            break;
        case NOTE_IDEA:
            list = &idea_list;
            break;
        case NOTE_PENALTY:
            list = &penalty_list;
            break;
        case NOTE_NEWS:
            list = &news_list;
            break;
        case NOTE_CHANGES:
            list = &changes_list;
            break;
    }
    */

#if 0
    /*
     * Remove note from linked list.
     */
    if ( pnote == *list )
    {
        *list = pnote->next;
    }
    else
    {
        for ( prev = *list; prev != NULL; prev = prev->next )
        {
            if ( prev->next == pnote )
                break;
        }

        if ( prev == NULL )
        {
            log ( "Note_remove: pnote not found." );
            return;
        }

        prev->next = pnote->next;
    }
#endif
    type = pnote->type;
    free_note ( pnote,type );
    save_notes ( type );
    return;
}

bool hide_note ( Character *ch, NOTE_DATA *pnote )
{
    time_t last_read;

    if ( IS_NPC ( ch ) )
        return TRUE;

    switch ( pnote->type )
    {
        default:
            return TRUE;
        case NOTE_NOTE:
            last_read = SPECIALS ( ch )->last_note;
            break;
        case NOTE_IDEA:
            last_read = SPECIALS ( ch )->last_idea;
            break;
        case NOTE_PENALTY:
            last_read = SPECIALS ( ch )->last_penalty;
            break;
        case NOTE_NEWS:
            last_read = SPECIALS ( ch )->last_news;
            break;
        case NOTE_CHANGES:
            last_read = SPECIALS ( ch )->last_changes;
            break;
    }

    if ( pnote->date_stamp <= last_read )
        return TRUE;

    if ( !str_cmp ( GET_NAME ( ch ),pnote->sender ) )
        return TRUE;

    if ( !is_note_to ( ch,pnote ) )
        return TRUE;

    return FALSE;
}

void update_read ( Character *ch, NOTE_DATA *pnote )
{
    time_t stamp;

    if ( IS_NPC ( ch ) )
        return;

    stamp = pnote->date_stamp;

    switch ( pnote->type )
    {
        default:
            return;
        case NOTE_NOTE:
            SPECIALS ( ch )->last_note = UMAX ( SPECIALS ( ch )->last_note,stamp );
            break;
        case NOTE_IDEA:
            SPECIALS ( ch )->last_idea = UMAX ( SPECIALS ( ch )->last_idea,stamp );
            break;
        case NOTE_PENALTY:
            SPECIALS ( ch )->last_penalty = UMAX ( SPECIALS ( ch )->last_penalty,stamp );
            break;
        case NOTE_NEWS:
            SPECIALS ( ch )->last_news = UMAX ( SPECIALS ( ch )->last_news,stamp );
            break;
        case NOTE_CHANGES:
            SPECIALS ( ch )->last_changes = UMAX ( SPECIALS ( ch )->last_changes,stamp );
            break;
    }
}

int has_note ( Character *ch, int type )
{
    NOTE_DATA **list;
    NOTE_DATA *pnote;

    if ( ch->has_note[type] != -1 )
        return ch->has_note[type];

    switch ( type )
    {
        default:
            return FALSE;
            break;
        case NOTE_NOTE:
            list = &note_list;
            break;
        case NOTE_IDEA:
            list = &idea_list;
            break;
        case NOTE_PENALTY:
            list = &penalty_list;
            break;
        case NOTE_NEWS:
            list = &news_list;
            break;
        case NOTE_CHANGES:
            list = &changes_list;
            break;
    }

    for ( pnote = *list; pnote != NULL; pnote = pnote->next )
        if ( is_note_to ( ch, pnote ) && ( !hide_note ( ch,pnote ) ) )
            return ( ch->has_note[type] = TRUE );

    return ( ch->has_note[type] = FALSE );


}


void parse_note ( Character *ch, char *argument, int type )
{
    Descriptor *d;
    char buffer[MAX_STRING_LENGTH];
    char buf[MAX_STRING_LENGTH];
    char arg[MAX_INPUT_LENGTH];
    char argument2[MAX_INPUT_LENGTH];
    NOTE_DATA *pnote;
    NOTE_DATA **list;
    const char *list_name;
    int vnum;
    int anum;
    time_t current_time = time ( 0 );

    if ( IS_NPC ( ch ) )
        return;

    switch ( type )
    {
        default:
            return;
        case NOTE_NOTE:
            list = &note_list;
            list_name = "note";
            break;
        case NOTE_IDEA:
            list = &idea_list;
            list_name = "idea";
            break;
        case NOTE_PENALTY:
            list = &penalty_list;
            list_name = "penaltie";
            break;
        case NOTE_NEWS:
            list = &news_list;
            list_name = "news";
            break;
        case NOTE_CHANGES:
            list = &changes_list;
            list_name = "change";
            break;
    }



    argument = any_one_arg ( argument, arg );
    skip_spaces ( &argument );
    smash_tilde ( argument );

    if ( arg[0] == '\0' || is_abbrev ( arg, "read" ) )
    {
        bool fAll;

        if ( !str_cmp ( argument, "all" ) )
        {
            fAll = TRUE;
            anum = 0;
        }

        else if ( argument[0] == '\0' || is_abbrev ( argument, "next" ) )
            /* read next unread note */
        {
            vnum = 0;
            for ( pnote = *list; pnote != NULL; pnote = pnote->next )
            {
                if ( !hide_note ( ch,pnote ) )
                {
                    ch->Send (
                        "{cyNote Number: {cc%3d{c0\r\n"
                        "{cy       From: {cc%-s{c0\r\n"
                        "{cy    Subject: {cg%-s{c0\r\n"
                        "{cy     Posted: {cc%-s{c0\r\n"
                        "{cy         To: {cw%-s{c0\r\n"
                        "=================================================\r\n",
                        vnum,
                        pnote->sender,
                        pnote->subject,
                        pnote->date,
                        pnote->to_list
                    );
                    page_string ( ch->desc, pnote->text, TRUE );
                    update_read ( ch,pnote );
                    ch->has_note[type] = -1;
                    has_note ( ch, type );
                    return;
                }
                else if ( is_note_to ( ch,pnote ) )
                    vnum++;
            }
            ch->Send ( "[%s] You have none unread.\r\n",list_name );
            ch->has_note[type] = ( FALSE );
            return;
        }

        else if ( is_number ( argument ) )
        {
            fAll = FALSE;
            anum = atoi ( argument );
        }
        else
        {
            send_to_char ( "Read which number?\r\n", ch );
            return;
        }

        vnum = 0;
        for ( pnote = *list; pnote != NULL; pnote = pnote->next )
        {
            if ( is_note_to ( ch, pnote ) && ( vnum++ == anum || fAll ) )
            {
                ch->Send (
                    "{cyNote Number: {cc%3d{c0\r\n"
                    "{cy       From: {cc%-s{c0\r\n"
                    "{cy    Subject: {cg%-s{c0\r\n"
                    "{cy     Posted: {cc%-s{c0\r\n"
                    "{cy         To: {cw%-s{c0\r\n"
                    "=================================================\r\n",
                    vnum - 1,
                    pnote->sender,
                    pnote->subject,
                    pnote->date,
                    pnote->to_list
                );
                page_string ( ch->desc, pnote->text, TRUE );
                update_read ( ch,pnote );
                ch->has_note[type] = -1;
                has_note ( ch, type );
                return;
            }
        }

        sprintf ( buf,"There aren't that many %ss.\r\n",list_name );
        send_to_char ( buf,ch );
        return;
    }

    if ( is_abbrev ( arg, "list" ) )
    {
        vnum = 0;
        DYN_DEFINE;
        DYN_CREATE;
        char nbuf[MAX_INPUT_LENGTH];
        for ( pnote = *list; pnote != NULL; pnote = pnote->next )
        {
            if ( is_note_to ( ch, pnote ) )
            {
                snprintf(nbuf, MAX_INPUT_LENGTH,  "{cw[{cy%3d{cG%s{cw] %-13s: {cc%s{c0\r\n",
                           vnum, hide_note ( ch,pnote ) ? " " : "N",
                           pnote->sender, pnote->subject );
                vnum++;
                DYN_RESIZE ( nbuf );
            }

        }
        page_string(ch->desc, dynbuf, DYN_BUFFER);
        if ( !vnum )
        {
            switch ( type )
            {
                case NOTE_NOTE:
                    send_to_char ( "There are no notes for you.\r\n",ch );
                    ch->has_note[NOTE_NOTE] = ( FALSE );
                    break;
                case NOTE_IDEA:
                    send_to_char ( "There are no ideas for you.\r\n",ch );
                    ch->has_note[NOTE_IDEA] = ( FALSE );
                    break;
                case NOTE_PENALTY:
                    send_to_char ( "There are no penalties for you.\r\n",ch );
                    ch->has_note[NOTE_PENALTY] = ( FALSE );
                    break;
                case NOTE_NEWS:
                    send_to_char ( "There is no news for you.\r\n",ch );
                    ch->has_note[NOTE_NEWS] = ( FALSE );
                    break;
                case NOTE_CHANGES:
                    send_to_char ( "There are no changes for you.\r\n",ch );
                    ch->has_note[NOTE_CHANGES] = ( FALSE );
                    break;
            }
        }
        return;
    }

    if ( is_abbrev ( arg, "remove" ) )
    {
        if ( !is_number ( argument ) )
        {
            send_to_char ( "Note remove which number?\r\n", ch );
            return;
        }

        anum = atoi ( argument );
        vnum = 0;
        for ( pnote = *list; pnote != NULL; pnote = pnote->next )
        {
            if ( is_note_to ( ch, pnote ) && vnum++ == anum )
            {
                note_remove ( ch, pnote, FALSE );
                send_to_char ( "Ok.\r\n", ch );
                return;
            }
        }

        ch->Send ( "There aren't that many %ss.",list_name );
        return;
    }

    if ( is_abbrev ( arg, "delete" ) && GET_LEVEL ( ch ) >= LVL_SEN )
    {
        if ( !is_number ( argument ) )
        {
            send_to_char ( "Note delete which number?\r\n", ch );
            return;
        }

        anum = atoi ( argument );
        vnum = 0;
        for ( pnote = *list; pnote != NULL; pnote = pnote->next )
        {
            if ( is_note_to ( ch, pnote ) && vnum++ == anum )
            {
                note_remove ( ch, pnote,TRUE );
                send_to_char ( "Ok.\r\n", ch );
                return;
            }
        }

        ch->Send ( "There aren't that many %ss.",list_name );
        return;
    }

    if ( is_abbrev ( arg,"catchup" ) )
    {
        switch ( type )
        {
            case NOTE_NOTE:
                SPECIALS ( ch )->last_note = time ( 0 );
                break;
            case NOTE_IDEA:
                SPECIALS ( ch )->last_idea = time ( 0 );
                break;
            case NOTE_PENALTY:
                SPECIALS ( ch )->last_penalty = time ( 0 );
                break;
            case NOTE_NEWS:
                SPECIALS ( ch )->last_news = time ( 0 );
                break;
            case NOTE_CHANGES:
                SPECIALS ( ch )->last_changes = time ( 0 );
                break;
        }
        ch->has_note[type] = ( FALSE );
        return;
    }

    /* below this point only certain people can edit notes */
    if ( ( type == NOTE_NEWS && !IS_TRUSTED ( ch,ANGEL ) )
            || (( type == NOTE_CHANGES && !IS_TRUSTED ( ch,CREATOR ) ) &&
           ( type == NOTE_CHANGES && !CMD_FLAGGED ( ch, WIZ_SEN_GRP))) )
    {
        ch->Send ( "You aren't high enough level to write %ss.",list_name );
        return;
    }

    if ( !str_cmp ( arg, "+" ) )
    {
        if ( ch->pnote == NULL )
        {
            ch->Send ( "You have no note in progress.\r\n" );
            return;
        }

        if ( ch->pnote->type != type )
        {
            ch->Send ( "You already have a different note in progress.\r\n" );
            return;
        }

        if ( strlen ( argument ) >= 4096 || ( ch->pnote->text && ( strlen ( ch->pnote->text ) + strlen ( argument ) >= 4096 ) ) )
        {
            ch->Send ( "The note will be too long.\r\n" );
            return;
        }

        if ( ch->pnote->text )
        {
            strcpy ( buffer, ch->pnote->text );
            free_string ( &ch->pnote->text );
        }
        else
            *buffer = '\0';

        strcat ( buffer, argument );
        strcat ( buffer, "\r\n" );
        ch->pnote->text = strdup ( buffer );
        ch->Send ( "Ok.\r\n" );
        return;
    }

    if ( !str_cmp ( arg, "-" ) )
    {
        if ( ch->pnote == NULL )
        {
            ch->Send ( "You have no note in progress.\r\n" );
            return;
        }

        if ( ch->pnote->type != type )
        {
            ch->Send ( "You already have a different note in progress.\r\n" );
            return;
        }

        if ( ch->pnote->text == NULL || ch->pnote->text[0] == '\0' )
        {
            ch->Send ( "No lines left to remove.\r\n" );
            return;
        }

        strcpy ( buf, ch->pnote->text );
        bool found = FALSE;

        for ( int len = strlen ( buf ); len > 0; len-- )
        {
            if ( buf[len] == '\n' )
            {
                if ( !found )  /* back it up */
                {
                    if ( len > 0 )
                        len--;
                    found = TRUE;
                }
                else /* found the second one */
                {
                    buf[len + 1] = '\0';
                    free_string ( &ch->pnote->text );
                    ch->pnote->text = str_dup ( buf );
                    ch->Send ( "Removed the last line of the note.\r\n" );
                    return;
                }
            }
        }
        buf[0] = '\0';
        free_string ( &ch->pnote->text );
        ch->pnote->text = str_dup ( buf );
        ch->Send ( "Removed the last line of the note.\r\n" );
        return;
    }

    if ( !str_cmp ( arg, "desc" ) || !str_cmp ( arg, "write" ) )
    {
        note_attach ( ch, type );
        string_append ( ch, &ch->pnote->text );
        return;
    }

    if ( is_abbrev ( arg, "subject" ) )
    {
        note_attach ( ch,type );
        if ( ch->pnote->type != type )
        {
            send_to_char (
                "You already have a different note in progress.\r\n",ch );
            return;
        }

        free_string ( &ch->pnote->subject );
        ch->pnote->subject = str_dup ( argument );
        send_to_char ( "Ok.\r\n", ch );
        return;
    }

    if ( is_abbrev ( arg, "to" ) )
    {
        if (( GET_LEVEL ( ch ) < LVL_IMMORT) && !CMD_FLAGGED(ch, WIZ_SEN_GRP))
        {
            if ( isname ( "all", argument ) )
            {
                ch->Send ( "You can't send to all.\r\nYou can send it to all the imms by sending it to IMM\r\nor to the Head Staff with ADMIN\r\n" );
                return;
            }
        }
        note_attach ( ch,type );
        if ( ch->pnote->type != type )
        {
            ch->Send ( "You already have a different note in progress.\r\n" );
            return;
        }
        free_string ( &ch->pnote->to_list );
        ch->pnote->to_list = str_dup ( argument );
        ch->Send ( "Ok.\r\n" );
        return;
    }

    if ( is_abbrev ( arg, "clear" ) )
    {
        if ( ch->pnote != NULL )
        {
            free_char_note ( ch->pnote );
            ch->pnote = NULL;
        }

        send_to_char ( "Ok.\r\n", ch );
        return;
    }

    if ( is_abbrev ( arg, "show" ) )
    {
        if ( ch->pnote == NULL )
        {
            send_to_char ( "You have no note in progress.\r\n", ch );
            return;
        }

        if ( ch->pnote->type != type )
        {
            send_to_char ( "You aren't working on that kind of note.\r\n",ch );
            return;
        }


        ch->Send (
            "\r\n{cy   From: {cc%-s{c0\r\n"
            "{cySubject: {cg%-s{c0\r\n"
            "{cy     To: {cw%-s{c0\r\n"
            "=================================================\r\n",
            ch->pnote->sender,
            ch->pnote->subject,
            ch->pnote->to_list
        );
        ch->Send ( "%s", ch->pnote->text );
        return;
    }

    if ( is_abbrev ( arg, "post" ) || is_abbrev ( arg, "send" ) )
    {
        char *strtime;

        if ( ch->pnote == NULL )
        {
            send_to_char ( "You have no note in progress.\r\n", ch );
            return;
        }

        if ( ch->pnote->type != type )
        {
            send_to_char ( "You aren't working on that kind of note.\r\n",ch );
            return;
        }

        if ( !ch->pnote->to_list || !str_cmp ( ch->pnote->to_list,"" ) )
        {
            send_to_char (
                "You need to provide a recipient (name, all, or immortal, clan name, admin, hero).\r\n",
                ch );
            return;
        }

        if ( !ch->pnote->subject || !str_cmp ( ch->pnote->subject,"" ) )
        {
            send_to_char ( "You need to provide a subject.\r\n",ch );
            return;
        }

        ch->pnote->next			= NULL;
        //strtime				= ctime( &time(0) );
        //strtime[strlen(strtime)-1]	= '\0';
        strtime = asctime ( localtime ( &current_time ) );
        * ( strtime + strlen ( strtime ) - 1 ) = '\0';
        ch->pnote->date			= str_dup ( strtime );
        ch->pnote->date_stamp	= current_time;

        append_note ( ch->pnote );

        ch->Send ( "You post the note.\r\n" );

        for ( d = descriptor_list; d != NULL; d = d->next )
        {

            if ( STATE ( d ) == CON_PLAYING         &&
                    is_note_to ( d->character, ch->pnote ) &&
                    d->character != ch                  &&
                    ch->pnote->type == NOTE_NOTE )
            {
                act ( "$N has just posted a new Note for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                d->character->has_note[ch->pnote->type] = ( TRUE );
            }

            if ( STATE ( d ) == CON_PLAYING         &&
                    is_note_to ( d->character, ch->pnote )  &&
                    d->character != ch                   &&
                    ch->pnote->type == NOTE_NEWS )
            {
                act ( "$N has just posted a new News Item for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                d->character->has_note[ch->pnote->type] = ( TRUE );
            }

            if ( STATE ( d ) == CON_PLAYING         &&
                    is_note_to ( d->character, ch->pnote )  &&
                    d->character != ch                   &&
                    ch->pnote->type == NOTE_CHANGES )
            {
                act ( "$N has just posted a new Change for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                d->character->has_note[ch->pnote->type] = ( TRUE );
            }

            if ( STATE ( d ) == CON_PLAYING         &&
                    is_note_to ( d->character, ch->pnote )  &&
                    d->character != ch                   &&
                    ch->pnote->type == NOTE_IDEA )
            {
                act ( "$N has just posted a new Idea for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                d->character->has_note[ch->pnote->type] = ( TRUE );
            }

            if ( STATE ( d ) == CON_PLAYING         &&
                    IS_TRUSTED ( d->character, ANGEL )      &&
                    is_note_to ( d->character, ch->pnote )  &&
                    d->character != ch                   &&
                    ch->pnote->type == NOTE_PENALTY )
            {
                act ( "$N has just posted a new Penalty for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                d->character->has_note[ch->pnote->type] = ( TRUE );
            }
        }

        ch->pnote = NULL;

        return;
    }

    if ( is_abbrev ( arg, "forward" ) )
    {
        argument = any_one_arg ( argument, argument2 );
        skip_spaces ( &argument );
        note_attach ( ch,type );
        if ( ch->pnote->type != type )
        {
            send_to_char ( "You already have a different note in progress.\r\n",ch );
            return;
        }

        if ( !is_number ( argument2 ) || *argument == '\0' )
        {
            send_to_char ( "note forward <note number> <recipient(s)>\r\n", ch );
            return;
        }

        anum = atoi ( argument2 );
        vnum = 0;
        for ( pnote = *list; pnote != NULL; pnote = pnote->next )
        {
            if ( is_note_to ( ch, pnote ) && ( vnum++ == anum ) )
            {
                char *strtime;

                ch->pnote->text = str_dup ( pnote->text );

                snprintf ( buf, sizeof ( buf ), "FWD from (%s): %s", pnote->sender, pnote->subject );

                free_string ( &ch->pnote->subject );
                ch->pnote->subject = str_dup ( buf );
                free_string ( &ch->pnote->to_list );
                ch->pnote->to_list = str_dup ( argument );

                if ( !str_cmp ( ch->pnote->to_list,"" ) )
                {
                    send_to_char (
                        "You need to provide a recipient (name(s), all, clan name, admin or immortal).\r\n",
                        ch );
                    return;
                }
                ch->pnote->next			= NULL;
                strtime = asctime ( localtime ( &current_time ) );
                * ( strtime + strlen ( strtime ) - 1 ) = '\0';
                ch->pnote->date			= str_dup ( strtime );
                ch->pnote->date_stamp		= current_time;

                append_note ( ch->pnote );
                ch->Send ( "You FWD the note.\r\n" );

                for ( d = descriptor_list; d != NULL; d = d->next )
                {

                    if ( STATE ( d ) == CON_PLAYING		&&
                            is_note_to ( d->character, ch->pnote ) &&
                            d->character != ch                  &&
                            ch->pnote->type == NOTE_NOTE )
                    {
                        act ( "$N has just posted a new Note for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                        d->character->has_note[ch->pnote->type] = ( TRUE );
                    }

                    if ( STATE ( d ) == CON_PLAYING         &&
                            is_note_to ( d->character, ch->pnote )  &&
                            d->character != ch                   &&
                            ch->pnote->type == NOTE_NEWS )
                    {
                        act ( "$N has just posted a new News Item for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                        d->character->has_note[ch->pnote->type] = ( TRUE );
                    }

                    if ( STATE ( d ) == CON_PLAYING         &&
                            is_note_to ( d->character, ch->pnote )  &&
                            d->character != ch                   &&
                            ch->pnote->type == NOTE_CHANGES )
                    {
                        act ( "$N has just posted a new Change for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                        d->character->has_note[ch->pnote->type] = ( TRUE );
                    }

                    if ( STATE ( d ) == CON_PLAYING         &&
                            is_note_to ( d->character, ch->pnote )  &&
                            d->character != ch                   &&
                            ch->pnote->type == NOTE_IDEA )
                    {
                        act ( "$N has just posted a new Idea for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                        d->character->has_note[ch->pnote->type] = ( TRUE );
                    }

                    if ( STATE ( d ) == CON_PLAYING         &&
                            IS_TRUSTED ( d->character, ANGEL )      &&
                            is_note_to ( d->character, ch->pnote )  &&
                            d->character != ch                   &&
                            ch->pnote->type == NOTE_PENALTY )
                    {
                        act ( "$N has just posted a new Penalty for you to read.\r\n", FALSE, d->character, NULL, ch, TO_CHAR );
                        d->character->has_note[ch->pnote->type] = ( TRUE );
                    }
                }
                ch->pnote = NULL;
                return;
            }
        }
        ch->Send ( "There aren't that many %s.\r\n",list_name );
        return;
    }

    ch->Send ( "Correct usage is:\r\n"
               "==============CREATING================\r\n"
               "%s TO <recipients>\r\n"
               "Gives the %s a list of people who can read the %s.\r\n"
               "%s SUBJECT <subject>\r\n"
               "Lets others see the subject without havingto read the %s\r\n"
               "%s WRITE\r\n"
               "Then enter the text body for the %s.\r\n"
               "When you finish editing you can review the %s by\r\n"
               "%s SHOW\r\n"
               "Then you can submit the note to the recipients with\r\n"
               "%s SEND\r\n"
               "Or alternatively you can,\r\n"
               "%s CLEAR\r\n"
               "To clear the %s you were working on.\r\n"
               "==============EDITING==================\r\n"
               "%s + <line>\r\n"
               "Appends the line.\r\n"
               "%s -\r\n"
               "Deletes the last line.\r\n"
               "==============READING==================\r\n"
               "%s LIST\r\n"
               "Will display all %s's available for you to read,\r\n"
               "The ones with an N beside the number are unread.\r\n"
               "%s READ\r\n"
               "Will let you read the latest unread message.\r\n"
               "%s READ ALL\r\n"
               "Will display all the messages that are unread\r\n"
               "%s READ <num>\r\n"
               "Will let you read that number message\r\n"
               "=============DELETING===================\r\n"
               "%s REMOVE\r\n"
               "Will let you delete any message that has been sent to you\r\n"
               "that has your name in the recipient list.\r\n"
               "=============FORWARDING=================\r\n"
               "%s FORWARD <num> <recipients>\r\n"
               "Will let you forward any message that has been sent to you.\r\n",
               list_name,list_name,list_name,list_name,list_name,list_name,list_name,list_name,
               list_name,list_name,list_name,list_name,list_name,list_name,list_name,list_name,
               list_name,list_name,list_name,list_name,list_name
             );
    return;
}



NOTE_DATA *new_note ( void )
{
    NOTE_DATA *note;

    if ( note_free == NULL )
        CREATE ( note, NOTE_DATA, 1 );
    else
    {
        note = note_free;
        note_free = note_free->next;
    }
    VALIDATE ( note );
    return note;
}

void free_all_notes ( void )
{
    /*notes*/
    free_note_list ( note_list, NOTE_NOTE );

    free_note_list ( idea_list, NOTE_IDEA );

    free_note_list ( penalty_list, NOTE_PENALTY );

    free_note_list ( news_list, NOTE_NEWS );

    free_note_list ( changes_list, NOTE_CHANGES );
}

void free_note_list ( NOTE_DATA *note, int type )
{

    if ( !note )
        return;

    if ( note->next )
        free_note_list ( note->next, type );

    free_note ( note, type );

}

void free_note ( NOTE_DATA *note, int type )
{
    NOTE_DATA *temp = NULL, **list = NULL;

    if ( !note )
        return;

    switch ( type )
    {
        case NOTE_NOTE:
            list = &note_list;
            break;
        case NOTE_IDEA:
            list = &idea_list;
            break;
        case NOTE_PENALTY:
            list = &penalty_list;
            break;
        case NOTE_CHANGES:
            list = &changes_list;
            break;
        case NOTE_NEWS:
            list = &news_list;
            break;
    }

    free_string ( &note->text );
    free_string ( &note->subject );
    free_string ( &note->to_list );
    free_string ( &note->date );
    free_string ( &note->sender );

    if ( list )
    {
        REMOVE_FROM_LIST ( note, *list, next );
    }
    free ( note );
    note = NULL;

}

void free_char_note ( NOTE_DATA *note )
{

    if ( !note )
        return;
    free_string ( &note->text );
    free_string ( &note->subject );
    free_string ( &note->to_list );
    free_string ( &note->date );
    free_string ( &note->sender );
    free ( note );
    note = NULL;


}

void free_string ( char **pt )
{
    if ( !pt )
    {
        log ( "null pointer to pointer passed to free string!" );
        return;
    }
    if ( *pt )
        free ( *pt );
    *pt = NULL;
}



