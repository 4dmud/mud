/***************************************************************************
                          4d_hedit.c  -  description
                             -------------------
    begin                : Sun Feb 22 2004
    copyright            : (C) 2004 by Jamie Nelson
    email                : mordecai@xtra.co.nz
 ***************************************************************************/
#include "conf.h"
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
 /* function protos */


/*
 * External data structures.
 */
extern struct help_index_element *help_table;
extern int top_of_helpt;
extern struct descriptor_data *descriptor_list;

/*------------------------------------------------------------------------*\
  Utils and exported functions.
\*------------------------------------------------------------------------*/

ACMD(do_oasis_hedit)
{
  char arg[MAX_INPUT_LENGTH];
  struct descriptor_data *d;
  for (d = descriptor_list; d; d = d->next)
    if (STATE(d) == CON_HEDIT) {
      new_send_to_char(ch, "Sorry, only one can edit help at a time.\r\n");
      return;
    }

  one_argument(argument, arg);

  if (!*arg) {
    new_send_to_char(ch, "Please specify a help entry to edit.\r\n");
    return;
  }

  d = ch->desc;

  if (!str_cmp("save", arg)) {
    new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE, "OLC: %s saves help files.", GET_NAME(ch));
    new_send_to_char(ch, "Writing help file..\r\n");
    //process_output(ch->desc);
    hedit_save_to_disk(d);
    new_send_to_char(ch, "Done.\r\n");
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
  OLC_STORAGE(d) = strdup(arg);

  for (OLC_ZNUM(d) = 0; (OLC_ZNUM(d) <= top_of_helpt); OLC_ZNUM(d)++)
    if (isname(OLC_STORAGE(d), help_table[OLC_ZNUM(d)].keywords))
      break;

  if (OLC_ZNUM(d) > top_of_helpt)  {
    new_send_to_char(ch, "Do you wish to add the '%s' help entry? ", OLC_STORAGE(d));
    OLC_MODE(d) = HEDIT_CONFIRM_ADD;
  } else {
    new_send_to_char(ch, "Do you wish to edit the '%s' help entry? ", help_table[OLC_ZNUM(d)].keywords);
    OLC_MODE(d) = HEDIT_CONFIRM_EDIT;
  }
  STATE(d) = CON_HEDIT;
  act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);
  new_mudlog(CMP, LVL_IMMORT, TRUE, "OLC: %s starts editing help.", GET_NAME(ch));
}


void hedit_setup_new(struct descriptor_data *d) {
    CREATE(OLC_HELP(d), struct help_index_element, 1);
    OLC_HELP(d)->keywords     = strdup(OLC_STORAGE(d));
    OLC_HELP(d)->entry        = strdup("This is an unfinished help entry.");
    hedit_disp_menu(d);
    OLC_VAL(d) = 0;
}

/*------------------------------------------------------------------------*/

void hedit_setup_existing(struct descriptor_data *d, int real_num) {

    CREATE(OLC_HELP(d), struct help_index_element, 1);

    *OLC_HELP(d) = help_table[real_num];
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



void hedit_save_internally(struct descriptor_data *d) {
     int i;
    struct help_index_element *new_help_table;
    /*
     * Help entry exists exists: free and replace it.
     */
    if (OLC_ZNUM(d) > top_of_helpt) {
      CREATE(new_help_table, struct help_index_element, top_of_helpt + 2);
     	/*
	 * Count through help table.
	 */
   /* Entry doesn't exist, hafta add it. */
	for (i = 0; i <= top_of_helpt; i++)
	    new_help_table[i] = help_table[i];
       //add_to_help_index(OLC_HELP(d), OLC_HELP(d).keywords);
      new_help_table[++top_of_helpt] = *OLC_HELP(d);
      free(help_table);
      help_table = new_help_table;
    } else {	
      /* why did i do this..? hrm */
      free_help(help_table + OLC_ZNUM(d));
      help_table[OLC_ZNUM(d)] = *OLC_HELP(d);

   }

   
   
   top_of_helpt++;
   sort_help();
   
    add_to_save_list(HEDIT_PERMISSION, SL_HELP);
    hedit_save_to_disk(d); /* autosave by Rumble */
}


/*------------------------------------------------------------------------*/

void hedit_save_to_disk(struct descriptor_data *d) {
   FILE *fp;
   int i;
   struct help_index_element *help;
   char buf[MAX_INPUT_LENGTH];
   
   snprintf(buf, sizeof(buf), "%s/%s", HLP_PREFIX, HELP_FILE);
   if (!(fp = fopen(buf, "w+")))  {
     char error[MAX_STRING_LENGTH];
     snprintf(error, sizeof(error), "Can't open help file '%s'", buf);
     perror(error);
     exit(1);
   }

    for (i = 0; i <= top_of_helpt; i++) {
	help = (help_table + i);

	/*
	 * Forget making a buffer, lets just write the thing now.
	 */
	fprintf(fp, "%s\n%s\n#%d\n",
		help->keywords ? help->keywords : "UNDEFINED",
    help->entry ? help->entry : "Empty",
		help->min_level);
    }
  

/*
     * Write final line and close.
     */
    fprintf(fp, "$~\n");
    fclose(fp);

   remove_from_save_list(HEDIT_PERMISSION, SL_HELP);
}

/*------------------------------------------------------------------------*/

/* Menu functions */
void hedit_disp_menu(struct descriptor_data *d)
{
    struct help_index_element *help = OLC_HELP(d);

    get_char_colors(d->character);

    write_to_output(d,
#if defined(CLEAR_SCREEN)
	    "[H[J"
#endif
	    "%s1%s) Keywords    : %s%s\r\n"
	    "%s2%s) Entry       :\r\n%s%s"
	    "%s3%s) Min Level   : %s%d\r\n"
	    "%sQ%s) Quit\r\n"
	    "Enter choice : ",
	    grn, nrm, yel, help->keywords ? help->keywords : "None",
	    grn, nrm, yel, help->entry ? help->entry : "Empty",
	    grn, nrm, cyn, help->min_level, grn, nrm);

    OLC_MODE(d) = HEDIT_MAIN_MENU;
}




/*
 * The main loop
 */

 /**************************************************************************
  The main loop
 **************************************************************************/

void hedit_parse(struct descriptor_data *d, char *arg)
{
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
	    write_to_output(d, "Help entry saved to memory.\r\n");
	    break;
	case 'n':
	case 'N':
	    /*
	     * Free everything up, including strings, etc.
	     */
	    cleanup_olc(d, CLEANUP_ALL);
	    break;
	default:
	    write_to_output(d, "Invalid choice!\r\n"
                         "Do you wish to save this help entry internally? : ");
	    break;
	}
	return;

    case HEDIT_MAIN_MENU:
	switch (*arg) {
	case 'q':
	case 'Q':
	    if (OLC_VAL(d)) {	/* Something has been modified. */
		write_to_output(d, "Do you wish to save this help entry internally? : ");
		OLC_MODE(d) = HEDIT_CONFIRM_SAVESTRING;
	    } else
		cleanup_olc(d, CLEANUP_ALL);
	    write_to_output(d,"\r\n");
	    return;
	case '1':
	    write_to_output(d, "Enter keywords:-\r\n] ");
	    OLC_MODE(d) = HEDIT_KEYWORDS;
	    break;
	case '2':
	    OLC_MODE(d) = HEDIT_ENTRY;
	    write_to_output(d,"Enter help entry: (/s saves /h for help)\r\n\r\n");
	    d->backstr = NULL;
	    if (OLC_HELP(d)->entry) {
		write_to_output(d, "%s", OLC_HELP(d)->entry);
		d->backstr = str_dup(OLC_HELP(d)->entry);
	    }
	    d->str = &OLC_HELP(d)->entry;
	    d->max_str = MAX_HELP_ENTRY;
	    d->mail_to = 0;
	    OLC_VAL(d) = 1;
	    break;
	case '3':
	    write_to_output(d, "Enter min level:-\r\n] ");
	    OLC_MODE(d) = HEDIT_MIN_LEVEL;
	    break;
	default:
	    write_to_output(d, "Invalid choice!\r\n");
	    hedit_disp_menu(d);
	    break;
	}
	return;
  case HEDIT_CONFIRM_EDIT:
      switch (*arg)  {
       case 'y': case 'Y':
         hedit_setup_existing(d, OLC_ZNUM(d));
         break;
       case 'q': case 'Q':
         cleanup_olc(d, CLEANUP_ALL);
         break;
       case 'n': case 'N':
         OLC_ZNUM(d)++;
         for (;(OLC_ZNUM(d) <= top_of_helpt); OLC_ZNUM(d)++)
           if (isname(OLC_STORAGE(d), help_table[OLC_ZNUM(d)].keywords))
             break;

         if (OLC_ZNUM(d) > top_of_helpt) {
            if (hedit_find_entry(OLC_STORAGE(d)) != -1)  {
               cleanup_olc(d, CLEANUP_ALL);
               break;
            }
            write_to_output(d, "Do you wish to add the '%s' help entry? ",
                               OLC_STORAGE(d));
            OLC_MODE(d) = HEDIT_CONFIRM_ADD;
         } else  {
            write_to_output(d, "Do you wish to edit the '%s' help entry? ",
                            help_table[OLC_ZNUM(d)].keywords);
            OLC_MODE(d) = HEDIT_CONFIRM_EDIT;
         }
         break;
       default:
         write_to_output(d, "Invalid choice!\r\n"
                            "Do you wish to edit the '%s' help entry? ",
                            soc_mess_list[OLC_ZNUM(d)].command);
         break;
      }
      return;
      case HEDIT_CONFIRM_ADD:
      switch (*arg)  {
       case 'y': case 'Y':
         hedit_setup_new(d);
         break;
       case 'n': case 'N': case 'q': case 'Q':
         cleanup_olc(d, CLEANUP_ALL);
         break;
       default:
         write_to_output(d, "Invalid choice!\r\n"
                            "Do you wish to add the '%s' help entry? ",
                            OLC_STORAGE(d));
         break;
      }
      return;


    case HEDIT_KEYWORDS:
	if (OLC_HELP(d)->keywords)
	    free_string(OLC_HELP(d)->keywords);
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
	    write_to_output(d, "That is not a valid choice!\r\n"
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

void free_help(struct help_index_element *help)
{

	free_string(help->keywords);
  if (help->entry && !help->duplicate) {
	    free(help->entry);
      help->entry = NULL;
      }
      
  memset(help, 0, (sizeof(struct help_index_element)));

}

int hsort(const void *a, const void *b)
{
    const struct help_index_element *a1, *b1;

    a1 = (const struct help_index_element *) a;
    b1 = (const struct help_index_element *) b;

    return (str_cmp(a1->keywords, b1->keywords));
}

void sort_help(void) { 
	qsort(help_table, top_of_helpt, sizeof(struct help_index_element),
	      hsort);
	top_of_helpt--;
  }

int hedit_find_entry(char *keyword)
{
    extern int top_of_helpt;
    int i;

    for (i = 0; i < top_of_helpt; i++)
	if (isname(keyword, help_table[i].keywords))
	    return i;

    return -1;
}
