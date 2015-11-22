/***************************************************************************
                          4d_hedit.c  -  description
                             -------------------
    begin                : Sun Feb 22 2004
    copyright            : (C) 2004 by Jamie Nelson
    email                : mordecai@xtra.co.nz
 ***************************************************************************/
#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "interpreter.h"
#include "handler.h"
#include "comm.h"
#include "utils.h"
#include "db.h"
#include "oasis.h"
#include "screen.h"
#include "constants.h"
#include "genolc.h"
#include "descriptor.h"



/* function protos */


/*
 * External data structures.
 */
extern struct help_index_element *help_table;
extern Descriptor *descriptor_list;

/*------------------------------------------------------------------------*\
  Utils and exported functions.
\*------------------------------------------------------------------------*/

ACMD(do_oasis_hedit) {
    Descriptor *d;
    for (d = descriptor_list; d; d = d->next)
        if (STATE(d) == CON_HEDIT) {
            ch->Send( "Sorry, only one can edit help at a time.\r\n");
            return;
        }
    skip_spaces(&argument);
    if (!argument || !*argument) {
        ch->Send( "Please specify a help entry to edit.\r\n");
        return;
    }

    d = ch->desc;

    if (!str_cmp("save", argument)) {
        new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE, "OLC: %s saves help files.", GET_NAME(ch));
        ch->Send( "Writing help file..\r\n");
        //process_output(ch->desc);
        hedit_save_to_disk(d);
        ch->Send( "Done.\r\n");
        //process_output(ch->desc);
        return;
    }

    /*
     * Give descriptor an OLC structure.
     */
    if (d->olc) {
        new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: do_oasis: Player already had olc structure.");
        free(d->olc);
    }
    CREATE(d->olc, struct oasis_olc_data, 1);

    OLC_NUM(d) = 0;
    OLC_STORAGE(d) = strdup(argument);


    for (OLC_ZNUM(d) = 0;OLC_ZNUM(d) <= top_of_helpt;OLC_ZNUM(d)++)
        if (isname_full(OLC_STORAGE(d), help_table[OLC_ZNUM(d)].keywords))
            break;


    if (OLC_ZNUM(d) > top_of_helpt) {
        /**TODO: i want this to ask the player, and if they say NO then
                 to keep checking the list for the next one **/
        ch->Send( "Do you wish to add the '%s' help entry? ", OLC_STORAGE(d));
        OLC_MODE(d) = HEDIT_CONFIRM_ADD;
    } else {
        ch->Send( "Do you wish to edit the '%s' help entry? ", help_table[OLC_ZNUM(d)].keywords);
        OLC_MODE(d) = HEDIT_CONFIRM_EDIT;
    }
    STATE(d) = CON_HEDIT;
    act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
    SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);
    new_mudlog(CMP, LVL_IMMORT, TRUE, "OLC: %s starts editing help.", GET_NAME(ch));
}


void hedit_setup_new(Descriptor *d) {
    CREATE(OLC_HELP(d), struct help_index_element, 1);
    OLC_HELP(d)->keywords     = str_dup(OLC_STORAGE(d));
    OLC_HELP(d)->entry        = str_dup("This is an unfinished help entry.\r\n");
    OLC_HELP(d)->min_level = 0;
    hedit_disp_menu(d);
    OLC_VAL(d) = 0;
}

/*------------------------------------------------------------------------*/

void hedit_setup_existing(Descriptor *d, int real_num) {
    CREATE(OLC_HELP(d), struct help_index_element, 1);
    *OLC_HELP(d) = help_table[real_num];
    OLC_HELP(d)->entries = 0;
    /*
     * Allocate space for all strings.
     */


    OLC_HELP(d)->keywords = strdup(help_table[real_num].keywords ?
                                   help_table[real_num].keywords : "UNDEFINED");

    OLC_HELP(d)->entry = strdup(help_table[real_num].entry ?
                                help_table[real_num].entry : "undefined\r\n");

    OLC_VAL(d) = 0;
    hedit_disp_menu(d);
}



void hedit_save_internally(Descriptor *d) {
    unsigned int i;
    struct help_index_element *new_help_table;

    if (OLC_ZNUM(d) > top_of_helpt) {
        CREATE(new_help_table, struct help_index_element, top_of_helpt + 2);
        for (i = 0; i <= top_of_helpt; i++)
            new_help_table[i] = help_table[i];

        top_of_helpt = i;
        if (i != OLC_ZNUM(d))
            log("Znum is dif from rnum in help");
        free(help_table);
        help_table = new_help_table;
    } else {
        if (help_table[OLC_ZNUM(d)].entry && *help_table[OLC_ZNUM(d)].entry)
            free(help_table[OLC_ZNUM(d)].entry);
        if (help_table[OLC_ZNUM(d)].keywords && *help_table[OLC_ZNUM(d)].keywords)
            free(help_table[OLC_ZNUM(d)].keywords);
    }

    help_table[OLC_ZNUM(d)].min_level = OLC_HELP(d)->min_level;
    help_table[OLC_ZNUM(d)].duplicate = 0;
    help_table[OLC_ZNUM(d)].id = OLC_ZNUM(d);
    help_table[OLC_ZNUM(d)].entries = 0; /* How many key words there are */
    help_table[OLC_ZNUM(d)].entry = (OLC_HELP(d)->entry);
    help_table[OLC_ZNUM(d)].keywords = (OLC_HELP(d)->keywords);


    add_to_save_list(HEDIT_PERMISSION, SL_HELP);
    hedit_save_to_disk(d); /* autosave by Rumble */
}


/*------------------------------------------------------------------------*/

void hedit_save_to_disk(Descriptor *d) {
    FILE *fp;
    int i;
    struct help_index_element *help;
    char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];

    snprintf(buf, sizeof(buf), "%s/%s.tmp", HLP_PREFIX, HELP_FILE);
    if (!(fp = fopen(buf, "w+"))) {
        new_mudlog(NRM, GET_LEVEL(d->character), TRUE, "Can't open help file '%s'", buf);
        char error[MAX_STRING_LENGTH];
        snprintf(error, sizeof(error), "Can't open help file '%s'", buf);
        perror(error);
        exit(1);
    }
    log("Saving help to %s", buf);
    for (i = 0; i <= top_of_helpt; i++) {
		char entry[MAX_HELPENTRY_LENGTH];
		char *hptr,*carpointer;
		int lengthleft=MAX_HELPENTRY_LENGTH;
		help = (help_table + i);
		hptr=help->entry;
		*entry='\0';
		if(hptr && *hptr){
			while((carpointer=strchr(hptr,'\r'))!=NULL){
				strncat(entry,hptr,(carpointer-hptr<lengthleft-1)?carpointer-hptr:lengthleft);
				strcat(entry,"\n");
				lengthleft-=carpointer-hptr+1;
				hptr=carpointer;
				while(*hptr=='\r') hptr++;
				while(*hptr=='\n') hptr++;
			}
			strncat(entry,hptr,(strlen(hptr)<lengthleft)?strlen(hptr):lengthleft);
		}
        fprintf(fp, "%s\n%s\n#%d\n",
                help->keywords && *help->keywords ? help->keywords : "UNDEFINED",
                help->entry && *help->entry ? entry : "Empty",
                help->min_level);
        //log("%d - %s", i, help->keywords);
    }
    /*
     * Write final line and close.
     */
    fprintf(fp, "$~\n");
    fclose(fp);
    snprintf(buf2, sizeof(buf2), "%s/%s", HLP_PREFIX, HELP_FILE);
    /*
     * We're fubar'd if we crash between the two lines below.
     */
    remove(buf2);
    rename(buf, buf2);
    remove_from_save_list(HEDIT_PERMISSION, SL_HELP);
}

/*------------------------------------------------------------------------*/

/* Menu functions */
void hedit_disp_menu(Descriptor *d) {
    struct help_index_element *help = OLC_HELP(d);

    get_char_colours(d->character);

    d->Output(
#if defined(CLEAR_SCREEN)
        "[H[J"
#endif
        "%s1%s) Keywords    : %s%s\r\n"
        "%s2%s) Entry       :\r\n%s\r\n"
        "%s3%s) Min Level   : %s%d\r\n"
        "%sQ%s) Quit\r\n"
        "Enter choice : ",
        grn, nrm, yel, help->keywords ? help->keywords : "None",
        grn, nrm, help->entry ? help->entry : "Empty",
        grn, nrm, cyn, help->min_level, grn, nrm);

    OLC_MODE(d) = HEDIT_MAIN_MENU;
}




/*
 * The main loop
 */

/**************************************************************************
 The main loop
**************************************************************************/

void hedit_parse(Descriptor *d, char *arg) {
    int number;

    switch (OLC_MODE(d)) {
    case HEDIT_CONFIRM_SAVESTRING:
        switch (*arg) {
        case 'y':
        case 'Y':
            hedit_save_internally(d);
            new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(d->character)),
                       TRUE, "OLC: %s edits help for %s.",
                       GET_NAME(d->character), OLC_HELP(d)->keywords);
            /*
             * Do NOT free strings! Just the help structure.
             */
            cleanup_olc(d, CLEANUP_STRUCTS);
            d->Output( "Help entry saved to memory.\r\n");
            break;
        case 'n':
        case 'N':
            /*
             * Free everything up, including strings, etc.
             */
            cleanup_olc(d, CLEANUP_ALL);
            break;
        default:
            d->Output( "Invalid choice!\r\n"
                       "Do you wish to save this help entry internally? : ");
            break;
        }
        return;

    case HEDIT_MAIN_MENU:
        switch (*arg) {
        case 'q':
        case 'Q':
            if (OLC_VAL(d)) {	/* Something has been modified. */
                d->Output( "Do you wish to save this help entry internally? : ");
                OLC_MODE(d) = HEDIT_CONFIRM_SAVESTRING;
            } else
                cleanup_olc(d, CLEANUP_ALL);
            d->Output("\r\n");
            return;
        case '1':
            d->Output( "Enter keywords:-\r\n] ");
            OLC_MODE(d) = HEDIT_KEYWORDS;
            break;
        case '2':
            OLC_MODE(d) = HEDIT_ENTRY;
            d->Output("Enter help entry: (/s saves /h for help)\r\n\r\n");
            if (d->backstr)
                free(d->backstr);
            d->backstr = NULL;

            if (OLC_HELP(d)->entry) {
                d->Output( "%s", OLC_HELP(d)->entry);
                d->backstr = strdup(OLC_HELP(d)->entry);
            }
            d->str = &OLC_HELP(d)->entry;
            d->max_str = MAX_HELP_ENTRY;
            d->mail_to = 0;
            OLC_VAL(d) = 1;
            break;
        case '3':
            d->Output( "Enter min level:-\r\n] ");
            OLC_MODE(d) = HEDIT_MIN_LEVEL;
            break;
        default:
            d->Output( "Invalid choice!\r\n");
            hedit_disp_menu(d);
            break;
        }
        return;
    case HEDIT_CONFIRM_EDIT:
        switch (*arg) {
        case 'y':
        case 'Y':
            hedit_setup_existing(d, OLC_ZNUM(d));
            break;
        case 'n':
        case 'N':
        case 'q':
        case 'Q':
            cleanup_olc(d, CLEANUP_ALL);
            break;

            /*
            //OLC_ZNUM(d)++;
            //for (;(OLC_ZNUM(d) <= top_of_helpt); OLC_ZNUM(d)++)
             // if (isname(OLC_STORAGE(d), help_table[OLC_ZNUM(d)].keywords))
              //  break;

            if (OLC_ZNUM(d) > top_of_helpt)
            {
              if (hedit_find_entry(OLC_STORAGE(d)) != -1)
              {
                cleanup_olc(d, CLEANUP_ALL);
                break;
              }
              d->Output( "Do you wish to add the '%s' help entry? ",
                              OLC_STORAGE(d));
              OLC_MODE(d) = HEDIT_CONFIRM_ADD;
            }
            else
            {
              d->Output( "Do you wish to edit the '%s' help entry? ",
                              help_table[OLC_ZNUM(d)].keywords);
              OLC_MODE(d) = HEDIT_CONFIRM_EDIT;
            }
            break;
            */
        default:
            d->Output( "Invalid choice!\r\n"
                       "Do you wish to edit the '%s' help entry? ",
                       help_table[OLC_ZNUM(d)].keywords);
            break;
        }
        return;

    case HEDIT_CONFIRM_ADD:
        switch (*arg) {
        case 'y':
        case 'Y':
            hedit_setup_new(d);
            break;
        case 'n':
        case 'N':
        case 'q':
        case 'Q':
            cleanup_olc(d, CLEANUP_ALL);
            break;
        default:
            d->Output( "Invalid choice!\r\n"
                       "Do you wish to add the '%s' help entry? ",
                       OLC_STORAGE(d));
            break;
        }
        return;


    case HEDIT_KEYWORDS:
        free_string(&OLC_HELP(d)->keywords);
        if (strlen(arg) > MAX_HELP_KEYWORDS)
            arg[MAX_HELP_KEYWORDS - 1] = '\0';
        OLC_HELP(d)->keywords = str_dup((arg && *arg) ? arg : "UNDEFINED");
        break;

    case HEDIT_ENTRY:
        /*
         * We will NEVER get here, we hope.
         */
        new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached HEDIT_ENTRY case in parse_hedit");
        break;

    case HEDIT_MIN_LEVEL:
        number = atoi(arg);
        if ((number < 0) || (number > LVL_IMPL))
            d->Output( "That is not a valid choice!\r\n"
                       "Enter min level:-\r\n] ");
        else {
            OLC_HELP(d)->min_level = number;
            break;
        }
        return;

    default:
        /*
         * We should never get here.
         */
        new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached default case in parse_hedit");
        break;
    }
    /*
     * If we get this far, something has been changed.
     */
    OLC_VAL(d) = 1;
    hedit_disp_menu(d);
}

void free_help(struct help_index_element *help) {

    free_string(&help->keywords);
    free_string(&help->entry);
    free(help);

    //memset(help, 0, (sizeof(struct help_index_element)));

}

/* this function is unused since the list is always scanned */
int hsort(const void *a, const void *b) {
    const struct help_index_element *a1, *b1;

    a1 = (const struct help_index_element *) a;
    b1 = (const struct help_index_element *) b;

    return (str_cmp(a1->keywords, b1->keywords));
}

void sort_help(void) {
    return;
    //qsort(help_table, top_of_helpt, sizeof(struct help_index_element),        hsort);
    //top_of_helpt--;
}

int hedit_find_entry(char *keyword) {
    int i;

    for (i = 0; i < top_of_helpt; i++)
        if (isname(keyword, help_table[i].keywords))
            return i;

    return -1;
}

