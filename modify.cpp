/* ************************************************************************
*   File: modify.c                                      Part of CircleMUD *
*  Usage: Run-time modification of game variables                         *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "comm.h"
#include "spells.h"
#include "mail.h"
#include "boards.h"
#include "improved-edit.h"
#include "oasis.h"
#include "tedit.h"
#include "descriptor.h"
#include "constants.h"

#define DEFAULT_WIDTH 80
#define DEFAULT_LENGTH 20
void show_string ( Descriptor *d, char *input );


/* local functions */
void smash_tilde ( char *str );
ACMD ( do_skillset );
const char *next_page ( const char *str, int length,int width );
int count_pages ( char *str, int length,int width );
void paginate_string ( const char *str, Descriptor *d );
void playing_string_cleanup ( Descriptor *d, int action );
void exdesc_string_cleanup ( Descriptor *d, int action );
void trigedit_string_cleanup ( Descriptor *d, int terminator );
void help_string_cleanup ( Descriptor *d, int action );
void note_string_cleanup ( Descriptor *d, int action );
void qedit_string_cleanup ( Descriptor *d, int action );
Character *find_char ( long n );


const char *string_fields[] =
{
    "name",
    "short",
    "long",
    "description",
    "title",
    "delete-description",
    "\n"
};


/* maximum length for text field x+1 */
const int txt_length[] =
{
    15,
    60,
    256,
    240,
    60
};

class PageSize
{
    public:
        int l;
        int w;
        PageSize()
        {
            l = DEFAULT_LENGTH;
            w = DEFAULT_WIDTH;
        }
        void Check()
        {
            if ( l <= 2 || w <= 20 )
            {
                l = DEFAULT_LENGTH;
                w = DEFAULT_WIDTH;
            }
        }
        const int W() { return ( w <= 20 ? 20 : w ); }
        const int L() { return ( l <= 2  ? 2  : l ); }
};

/* ************************************************************************
*  modification of malloc'ed strings                                      *
************************************************************************ */

/*
 * Put '#if 1' here to erase ~, or roll your own method.  A common idea
 * is smash/show tilde to convert the tilde to another innocuous character
 * to save and then back to display it. Whatever you do, at least keep the
 * function around because other MUD packages use it, like mudFTP.
 *   -gg 9/9/98
 */

void smash_tilde ( char *str )
{
    /*
     * Erase any _line ending_ tildes inserted in the editor.
     * The load mechanism can't handle those, yet.
     * -- Welcor 04/2003
     */

    char *p = str;
    for ( ; *p; p++ )
        if ( *p == '~' && ( * ( p+1 ) =='\r' || * ( p+1 ) =='\n' || * ( p+1 ) =='\0' ) )
            *p=' ';
#if 0
    /*
     * Erase any ~'s inserted by people in the editor.  This prevents anyone
     * using online creation from causing parse errors in the world files.
     * Derived from an idea by Sammy <samedi@dhc.net> (who happens to like
     * his tildes thank you very much.), -gg 2/20/98
     */
    while ( ( str = strchr ( str, '~' ) ) != NULL )
        *str = ' ';
#endif
}

/*
 * Basic API function to start writing somewhere.
 *
 * 'data' isn't used in stock CircleMUD but you can use it to pass whatever
 * else you may want through it.  The improved editor patch when updated
 * could use it to pass the old text buffer, for instance.
 */
void string_write ( Descriptor *d, char **writeto, size_t len, long mailto, void *data )
{
    if ( d->character && !IS_NPC ( d->character ) )
        SET_BIT_AR ( PLR_FLAGS ( d->character ), PLR_WRITING );

    if ( using_improved_editor )
        d->backstr = ( char * ) data;
    else if ( data )
        free ( data );
    d->str = writeto;
    d->max_str = len;
    d->mail_to = mailto;
    d->real_string = FALSE;
}
void string_write ( Descriptor *d, string *writeto, size_t len, long mailto, void *data )
{
    if ( d->character && !IS_NPC ( d->character ) )
        SET_BIT_AR ( PLR_FLAGS ( d->character ), PLR_WRITING );

    if ( using_improved_editor )
        d->backstr = ( char * ) data;
    else if ( data )
        free ( data );

    if ( writeto != NULL )
        *d->str = strdup ( ( writeto )->c_str() );
    else
    {
        log ( "Error in passing an empty real string to string_write" );
        return;
    }
    d->max_str = len;
    d->mail_to = mailto;
    d->real_string = TRUE;
}

/*
 * Add user input to the 'current' string (as defined b d->str).
 * This is still overly complex.
 */
void string_add ( Descriptor *d, char *str )
{
    int action;

    /* determine if this is the terminal string, and truncate if so */
    /* changed to only accept '@' at the beginning of line - J. Elson 1/17/94 */

    delete_doubledollar ( str );
    smash_tilde ( str );

    /* determine if this is the terminal string, and truncate if so */
    /* changed to only accept '@' at the beginning of line - J. Elson 1/17/94 */
    if ( ( action = ( *str == '@' ) ) )
        *str = '\0';
    else
        if ( ( action = improved_editor_execute ( d, str ) ) == STRINGADD_ACTION )
            return;
    lock_desc ( d );
    if ( action != STRINGADD_OK )
        /* Do nothing. */
        ;
    else if ( ! ( *d->str ) )
    {
        if ( strlen ( str ) + 3 > d->max_str )   /* \r\n\0 */
        {
            d->Output ( "String too long - Truncated.\r\n" );
            strcpy ( &str[d->max_str - 3], "\r\n" );	/* strcpy: OK (size checked) */
            CREATE ( *d->str, char, d->max_str );
            strcpy ( *d->str, str );	/* strcpy: OK (size checked) */
            if ( !using_improved_editor )
                action = STRINGADD_SAVE;
        }
        else
        {
            CREATE ( *d->str, char, strlen ( str ) + 3 );
            strcpy ( *d->str, str );	/* strcpy: OK (size checked) */
        }
    }
    else
    {
        if ( strlen ( str ) + strlen ( *d->str ) + 3 > d->max_str )   /* \r\n\0 */
        {
            d->Output ( "String too long.  Last line skipped.\r\n" );
            if ( !using_improved_editor )
                action = STRINGADD_SAVE;
            else if ( action == STRINGADD_OK )
                action = STRINGADD_ACTION;    /* No appending \r\n\0, but still let them save. */
        }
        else
        {
            RECREATE ( *d->str, char, strlen ( *d->str ) + strlen ( str ) + 3 ); /* \r\n\0 */
            strcat ( *d->str, str );	/* strcat: OK (size precalculated) */
        }
    }


    /*
     * Common cleanup code.
     */
    switch ( action )
    {
        case STRINGADD_ABORT:
            switch ( STATE ( d ) )
            {
                case CON_CEDIT:
                case CON_TEDIT:
                case CON_REDIT:
                case CON_MEDIT:
                case CON_OEDIT:
                case CON_QEDIT:
                case CON_EXDESC:
                case CON_TRIGEDIT:
                    free ( *d->str );
                    if ( d->backstr )
                        *d->str = strdup ( d->backstr );
                    else
                        *d->str = NULL;
                    free_string ( &d->backstr );
                    break;
                default:
                    log ( "SYSERR: string_add: Aborting write from unknown state %d of %s", STATE ( d ), d->character ? GET_NAME ( d->character ) : "?" );
                    break;
            }
            break;
        case STRINGADD_SAVE:
            if ( d->str && *d->str && **d->str == '\0' )
            {
                free ( *d->str );
                *d->str = strdup ( "Nothing.\r\n" );
            }
            free_string ( &d->backstr );
            break;
        case STRINGADD_ACTION:
            break;
    }

    /* Ok, now final cleanup. */

    if ( action == STRINGADD_SAVE || action == STRINGADD_ABORT )
    {
        int i;
        struct
        {
            int mode;
            void ( *func ) ( Descriptor *d, int action );
        }
        cleanup_modes[] =
        {
            { CON_CEDIT  , cedit_string_cleanup },
            { CON_MEDIT  , medit_string_cleanup },
            { CON_OEDIT  , oedit_string_cleanup },
            { CON_REDIT  , redit_string_cleanup },
            { CON_TEDIT  , tedit_string_cleanup },
            { CON_QEDIT  , qedit_string_cleanup },
            { CON_TRIGEDIT, trigedit_string_cleanup },
            { CON_EXDESC , exdesc_string_cleanup },
            { CON_PLAYING, playing_string_cleanup },
            { CON_HEDIT,  help_string_cleanup},
            { CON_NOTE_EDIT, note_string_cleanup},
            { -1, NULL }
        };

        for ( i = 0; cleanup_modes[i].func; i++ )
            if ( STATE ( d ) == cleanup_modes[i].mode )
                ( *cleanup_modes[i].func ) ( d, action );

        /* Common post cleanup code. */
        d->str = NULL;
        d->mail_to = 0;
        d->max_str = 0;
        if ( d->character && !IS_NPC ( d->character ) )
            REMOVE_BIT_AR ( PLR_FLAGS ( d->character ), PLR_MAILING );
        REMOVE_BIT_AR ( PLR_FLAGS ( d->character ), PLR_WRITING );
    }
    else if ( action != STRINGADD_ACTION && strlen ( *d->str ) + 3 <= d->max_str ) /* 3 = \r\n\0 */
        strcat ( *d->str, "\r\n" );
    unlock_desc ( d );
}

void playing_string_cleanup ( Descriptor *d, int action )
{
    if ( PLR_FLAGGED ( d->character, PLR_MAILING ) )
    {
        if ( action == STRINGADD_SAVE && *d->str )
        {
            Character *to;
            store_mail ( d->mail_to, GET_IDNUM ( d->character ), *d->str );
            d->Output ( "Message sent!\r\n" );
            if ( ( to = find_char ( d->mail_to ) ) != NULL )
            {
                HAS_MAIL ( to ) = -1;
                to->Send ( "You have a new mud-mail in your inbox.\r\n" );
            }

        }
        else
            d->Output ( "Mail aborted.\r\n" );
        if ( *d->str )
            free ( *d->str );
        if ( d->str )
            free ( d->str );
    }

    /*
     * We have no way of knowing which slot the post was sent to so we can only give the message...
     */
    if ( d->mail_to >= BOARD_MAGIC )
    {
        Board_save_board ( d->mail_to - BOARD_MAGIC );

        if ( action == STRINGADD_ABORT )
            d->Output ( "Post not aborted, use REMOVE <post #>.\r\n" );
        else
            d->Output ( "Posted!\r\n" );

    }

    return;
}

void help_string_cleanup ( Descriptor *d, int action )
{
    hedit_disp_menu ( d );
    return;
}
void note_string_cleanup ( Descriptor *d, int action )
{
    d->Output ( "Note Written, NOTE POST to submit it.\r\n" );
    return;
}

void exdesc_string_cleanup ( Descriptor *d, int action )
{
    if ( action == STRINGADD_ABORT )
        d->Output ( "Description aborted.\r\n" );

    d->Output ( "%s", CONFIG_MENU );
    STATE ( d ) = CON_MENU;
}


/* **********************************************************************
*  Modification of character skills                                     *
********************************************************************** */

ACMD ( do_skillset )
{
    Character *vict;
    char name[MAX_INPUT_LENGTH];
    char buf[MAX_INPUT_LENGTH], help[MAX_STRING_LENGTH];
    int skill, value, i, qend;

    argument = one_argument ( argument, name );

    if ( !*name )  			/* no arguments. print an informative text */
    {
        ch->Send ( "Syntax: skillset <name> '<skill>' <value>\r\n"
                   "Skill being one of the following:\r\n" );
        for ( qend = 0, i = 0; i <= MAX_SKILLS; i++ )
        {
            if ( spell_info[i].name == unused_spellname )	/* This is valid. */
                continue;
            ch->Send ( "%18s", spell_info[i].name );
            if ( qend++ % 4 == 3 )
                ch->Send ( "\r\n" );
        }
        if ( qend % 4 != 0 )
            ch->Send ( "\r\n" );
        return;
    }

    if ( ! ( vict = get_char_vis ( ch, name, NULL, FIND_CHAR_WORLD ) ) )
    {
        ch->Send ( "%s", CONFIG_NOPERSON );
        return;
    }
    skip_spaces ( &argument );

    /* If there is no chars in argument */
    if ( !*argument )
    {
        ch->Send ( "Skill name expected.\r\n" );
        return;
    }
    if ( *argument != '\'' )
    {
        ch->Send ( "Skill must be enclosed in: ''\r\n" );
        return;
    }
    /* Locate the last quote and lowercase the magic words (if any) */

    for ( qend = 1; argument[qend] && argument[qend] != '\''; qend++ )
        argument[qend] = LOWER ( argument[qend] );

    if ( argument[qend] != '\'' )
    {
        ch->Send ( "Skill must be enclosed in: ''\r\n" );
        return;
    }
    strcpy ( help, ( argument + 1 ) );	/* strcpy: OK (MAX_INPUT_LENGTH <= MAX_STRING_LENGTH) */
    help[qend - 1] = '\0';
    if ( ( skill = find_skill_num ( help ) ) <= 0 )
    {
        ch->Send ( "Unrecognized skill.\r\n" );
        return;
    }
    argument += qend + 1;		/* skip to next parameter */
    argument = one_argument ( argument, buf );

    if ( !*buf )
    {
        ch->Send ( "Learned value expected.\r\n" );
        return;
    }
    value = atoi ( buf );
    if ( value < 0 )
    {
        ch->Send ( "Minimum value for learned is 0.\r\n" );
        return;
    }
    if ( value > 100 )
    {
        ch->Send ( "Max value for learned is 100.\r\n" );
        return;
    }
    if ( IS_NPC ( vict ) )
    {
        ch->Send ( "You can't set NPC skills.\r\n" );
        return;
    }

    /*
     * find_skill_num() guarantees a valid spell_info[] index, or -1, and we
     * checked for the -1 above so we are safe here.
     */
    SET_SKILL ( vict, skill, value );
    new_mudlog ( BRF, LVL_IMMORT, TRUE, "%s changed %s's %s to %d.", GET_NAME ( ch ), GET_NAME ( vict ), spell_info[skill].name, value );
    ch->Send ( "You change %s's %s to %d.\r\n", GET_NAME ( vict ), spell_info[skill].name, value );
}


ACMD ( do_subskillset )
{
    Character *vict;
    char name[MAX_INPUT_LENGTH];
    char buf[MAX_INPUT_LENGTH], help[MAX_STRING_LENGTH];
    int skill, value, i, qend;

    argument = one_argument ( argument, name );

    /*
     * No arguments. print an informative text.
     */
    if ( !*name )
    {
        ch->Send (
            "Syntax: subskillset <name> '<skill>' <value>\r\n"
            "Skill being one of the following:\r\n" );
        for ( qend = 0, i = 0; i <= TOP_SUB_DEFINE; i++ )
        {
            ch->Send ( "%18s", sub_name ( i ) );
            if ( qend++ % 4 == 3 )
                ch->Send ( "\r\n" );
        }
        if ( qend % 4 != 0 )
            ch->Send ( "\r\n" );
        return;
    }

    if ( ! ( vict = get_char_vis ( ch, name, NULL, FIND_CHAR_WORLD ) ) )
    {
        ch->Send ( "%s", CONFIG_NOPERSON );
        return;
    }
    skip_spaces ( &argument );

    /* If there is no chars in argument */
    if ( !*argument )
    {
        ch->Send ( "SubSkill name expected.\r\n" );
        return;
    }
    if ( *argument != '\'' )
    {
        ch->Send ( "SubSkill must be enclosed in: ''\r\n" );
        return;
    }
    /* Locate the last quote and lowercase the magic words (if any) */

    for ( qend = 1; argument[qend] && argument[qend] != '\''; qend++ )
        argument[qend] = LOWER ( argument[qend] );

    if ( argument[qend] != '\'' )
    {
        ch->Send ( "Skill must be enclosed in: ''\r\n" );
        return;
    }
    strcpy ( help, ( argument + 1 ) );
    help[qend - 1] = '\0';
    if ( ( skill = sub_number ( help ) ) <= 0 )
    {
        ch->Send ( "Unrecognized subskill.\r\n" );
        return;
    }
    argument += qend + 1;	/* skip to next parameter */
    argument = one_argument ( argument, buf );

    if ( !*buf )
    {
        ch->Send ( "%s has learned %s to %d%%\r\n", GET_NAME ( vict ), sub_name ( skill ), GET_SUB ( vict, skill ) );
        return;
    }
    value = atoi ( buf );
    if ( value < 0 )
    {
        ch->Send ( "Minimum value for learned is 0.\r\n" );
        return;
    }
    if ( value > 100 )
    {
        ch->Send ( "Max value for learned is 100.\r\n" );
        return;
    }


    /*
     * find_skill_num() guarantees a valid spell_info[] index, or -1, and we
     * checked for the -1 above so we are safe here.
     */
    new_mudlog ( BRF, -1, TRUE, "%s changed %s's %s to %d.", GET_NAME ( ch ),
                 GET_NAME ( vict ), sub_name ( skill ), value );

//	improve_sub ( vict, ( enum subskill_list ) skill, value - GET_SUB ( vict, skill ) );
        SAVED(vict).SetSubLearn((enum subskill_list) skill, value);
        SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);

    ch->Send ( "You change %s's %s to %d.\r\n", GET_NAME ( vict ),
               sub_name ( skill ), value );
}


/*********************************************************************
* New Pagination Code
* Michael Buselli submitted the following code for an enhanced pager
* for CircleMUD.  All functions below are his.  --JE 8 Mar 96
*
*********************************************************************/

/* Traverse down the string until the begining of the next page has been
 * reached.  Return NULL if this is the last page of the string.
 */
const char *next_page ( const char *str, PageSize &ps )
{
    int col = 1, line = 1, spec_code = FALSE;

    for ( ;; str++ )
    {
        /* If end of string, return NULL. */
        if ( *str == '\0' )
            return ( NULL );

        /* If we're at the start of the next page, return this fact. */
        else if ( line > ps.L() )
            return ( str );

        /* Check for the begining of an ANSI colour code block. */
        else if ( *str == '\x1B' && !spec_code )
            spec_code = TRUE;

        /* Check for the end of an ANSI colour code block. */
        else if ( *str == 'm' && spec_code )
            spec_code = FALSE;

        /* Check for everything else. */
        else if ( !spec_code )
        {
            /* Carriage return puts us in column one. */
            if ( *str == '\r' )
                col = 1;
            /* Newline puts us on the next line. */
            else if ( *str == '\n' )
                line++;

            /* We need to check here and see if we are over the page width,
             * and if so, compensate by going to the begining of the next line.
             */
            else if ( col++ > ps.W() )
            {
                col = 1;
                line++;
            }
        }
    }
}


/* Function that returns the number of pages in the string. */
int count_pages ( const char *str, PageSize &ps )
{
    int pages;

    for ( pages = 1; ( str = next_page ( str, ps ) ); pages++ )
        ;
    return ( pages );
}


/* This function assigns all the pointers for showstr_vector for the
 * page_string function, after showstr_vector has been allocated and
 * showstr_count set.
 */
void paginate_string ( const char *str, Descriptor *d )
{
    int i;
    PageSize ps = PageSize();
    if ( !d )
    {
        log ( "Paginate_string passed null descriptor!" );
        return;
    }
    if ( d->character )
    {
        ps.l = PAGEHEIGHT ( d->character );
        ps.w = PAGEWIDTH ( d->character );
    }

    if ( d->showstr_count )
        * ( d->showstr_vector ) = str;

    for ( i = 1; i < d->showstr_count && str; i++ )
        str = d->showstr_vector[i] = next_page ( str, ps );

    d->showstr_page = 0;
}


/* The call that gets the paging ball rolling... */
void page_string ( Descriptor *d, char *str, int keep_internal )
{
    char actbuf[MAX_INPUT_LENGTH] = "";
    PageSize ps = PageSize();
    if ( !d )
    {
        if ( keep_internal == DYN_BUFFER )
        {
            free ( str );
        }
        return;
    }

    if ( !str || !*str )
    {
        if ( keep_internal == DYN_BUFFER )
            free ( str );
        return;
    }

    lock_desc ( d );

    if ( d->character )
    {
        ps.l = PAGEHEIGHT ( d->character );
        ps.w = PAGEWIDTH ( d->character );
    }

    d->showstr_count = count_pages ( str, ps );
    CREATE ( d->showstr_vector, const char *, d->showstr_count );

    if ( keep_internal )
    {
        if ( keep_internal == DYN_BUFFER )
            d->showstr_head = str;
        else
            d->showstr_head = strdup ( str );
        unlock_desc ( d );
        paginate_string ( d->showstr_head, d );
    }
    else
    {
        unlock_desc ( d );
        paginate_string ( str, d );
    }
    show_string ( d, actbuf );
}

/** Page string for c++ strings - mord */
void page_string ( Descriptor *d, std::stringstream &str )
{
    char actbuf[MAX_INPUT_LENGTH] = "";
    PageSize ps = PageSize();
    string sbuf = str.str();
    if ( !d )
    {
        return;
    }

    if ( sbuf.length()==0 )
        return;

    lock_desc ( d );

    if ( d->character )
    {
        ps.l = PAGEHEIGHT ( d->character );
        ps.w = PAGEWIDTH ( d->character );
    }

    d->showstr_count = count_pages ( sbuf.c_str(), ps );
    CREATE ( d->showstr_vector, const char *, d->showstr_count );
    unlock_desc ( d );
    paginate_string ( sbuf.c_str(), d );

    show_string ( d, actbuf );
}


/* The call that displays the next page. */
void show_string ( Descriptor *d, char *input )
{
    char buffer[MAX_STRING_LENGTH], buf[MAX_INPUT_LENGTH];
    int diff;

    any_one_arg ( input, buf );
    /* Q is for quit. :) */
    if ( LOWER ( *buf ) == 'q' )
    {

        free ( d->showstr_vector );
        d->showstr_vector = NULL;
        d->showstr_count = 0;
        if ( d->showstr_head )
        {
            free ( d->showstr_head );
            d->showstr_head = NULL;
        }
        return;
    }
    /* R is for refresh, so back up one page internally so we can display
     * it again.
     */
    else if ( LOWER ( *buf ) == 'r' )
        d->showstr_page = MAX ( 0, d->showstr_page - 1 );

    /* B is for back, so back up two pages internally so we can display the
     * correct page here.
     */
    else if ( LOWER ( *buf ) == 'b' )
        d->showstr_page = MAX ( 0, d->showstr_page - 2 );

    /* Feature to 'goto' a page.  Just type the number of the page and you
     * are there!
     */
    else if ( isdigit ( *buf ) )
        d->showstr_page = MAX ( 0, MIN ( atoi ( buf ) - 1, d->showstr_count - 1 ) );

    else if ( *buf )
    {
        d->Output ( "Valid commands while paging are RETURN, Q, R, B, or a numeric value.\r\n" );
        return;
    }
    /* If we're displaying the last page, just send it to the character, and
     * then free up the space we used.
     */
    if ( d->showstr_page + 1 >= d->showstr_count )
    {
        d->Output ( "%s", d->showstr_vector[d->showstr_page] );
        free ( d->showstr_vector );
        d->showstr_vector = NULL;
        d->showstr_count = 0;
        if ( d->showstr_head )
        {
            free ( d->showstr_head );
            d->showstr_head = NULL;
        }
    }
    /* Or if we have more to show.... */
    else
    {
        diff = d->showstr_vector[d->showstr_page + 1] - d->showstr_vector[d->showstr_page];
        if ( diff > MAX_STRING_LENGTH - 3 ) /* 3=\r\n\0 */
            diff = MAX_STRING_LENGTH - 3;
        strncpy ( buffer, d->showstr_vector[d->showstr_page], diff );	/* strncpy: OK (size truncated above) */
        /*
         * Fix for prompt overwriting last line in compact mode submitted by
         * Peter Ajamian <peter@pajamian.dhs.org> on 04/21/2001
         */
        if ( buffer[diff - 2] == '\r' && buffer[diff - 1]=='\n' )
            buffer[diff] = '\0';
        else if ( buffer[diff - 2] == '\n' && buffer[diff - 1] == '\r' )
            /* This is backwards.  Fix it. */
            strcpy ( buffer + diff - 2, "\r\n" );	/* strcpy: OK (size checked) */
        else if ( buffer[diff - 1] == '\r' || buffer[diff - 1] == '\n' )
            /* Just one of \r\n.  Overwrite it. */
            strcpy ( buffer + diff - 1, "\r\n" );	/* strcpy: OK (size checked) */
        else
            /* Tack \r\n onto the end to fix bug with prompt overwriting last line. */
            strcpy ( buffer + diff, "\r\n" );	/* strcpy: OK (size checked) */

        d->Output ( "%s", buffer );
        d->showstr_page++;
    }
}
