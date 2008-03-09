/*
 * Originally written by: Michael Scott -- Manx.
 * Last known e-mail address: scottm@workcomm.net
 *
 * XXX: This needs Oasis-ifying.
 */

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "comm.h"
#include "db.h"
#include "genolc.h"
#include "handler.h"
#include "oasis.h"
#include "improved-edit.h"
#include "tedit.h"
#include "descriptor.h"

extern const char *credits;
extern const char *news;
extern const char *motd;
extern const char *imotd;
extern const char *help;
extern const char *info;
extern const char *background;
extern const char *handbook;
extern const char *policies;

void tedit_string_cleanup(Descriptor *d, int terminator)
{
  FILE *fl;
  char *storage = OLC_STORAGE(d);

  if (!storage)
    terminator = STRINGADD_ABORT;

  switch (terminator) {
  case STRINGADD_SAVE:
    if (!(fl = fopen(storage, "w")))
      new_mudlog(CMP, LVL_IMPL, TRUE, "SYSERR: Can't write file '%s'.", storage);
    else {
      lock_desc(d);
      if (*d->str) {
        strip_cr(*d->str);
        fputs(*d->str, fl);
      }
      
      unlock_desc(d);
      fclose(fl);
      new_mudlog(CMP, LVL_GOD, TRUE, "OLC: %s saves '%s'.", GET_NAME(d->character), storage);
      d->Output( "Saved.\r\n");
    }
    break;
  case STRINGADD_ABORT:
    
    d->Output( "Edit aborted.\r\n");
    act("$n stops editing some scrolls.", TRUE, d->character, 0, 0, TO_ROOM);
    break;
  default:
    log("SYSERR: tedit_string_cleanup: Unknown terminator status.");
    break;
  }

  /* Common cleanup code. */
  cleanup_olc(d, CLEANUP_ALL);
  STATE(d) = CON_PLAYING;
}

ACMD(do_tedit)
{
  int l, i = 0;
  char field[MAX_INPUT_LENGTH];
  char *backstr = NULL;
   
  struct {
    const char *cmd;
    char level;
    const char **buffer;
    int  size;
    const char *filename;
  } fields[] = {
	/* edit the lvls to your own needs */
	{ (char *)"credits",	LVL_IMPL,	&credits,	2400,	CREDITS_FILE},
	{ (char *)"news",	LVL_GRGOD,	&news,		8192,	NEWS_FILE},
	{ (char *)"motd",	LVL_GRGOD,	&motd,		2400,	MOTD_FILE},
	{ (char *)"imotd",	LVL_IMPL,	&imotd,		2400,	IMOTD_FILE},
	{ (char *)"help",       LVL_GRGOD,	&help,		2400,	HELP_PAGE_FILE},
	{ (char *)"info",	LVL_GRGOD,	&info,		8192,	INFO_FILE},
	{ (char *)"background",	LVL_IMPL,	&background,	8192,	BACKGROUND_FILE},
	{ (char *)"handbook",   LVL_IMPL,	&handbook,	8192,   HANDBOOK_FILE},
	{ (char *)"policies",	LVL_IMPL,	&policies,	8192,	POLICIES_FILE},
	{ (char *)"\n",		0,		NULL,		0,	NULL }
  };

  if (ch->desc == NULL)
    return;
   
  one_argument(argument, field);

  if (!*field) {
    ch->Send( "Files available to be edited:\r\n");
    for (l = 0; *fields[l].cmd != '\n'; l++) {
      if (GET_LEVEL(ch) >= fields[l].level) {
	ch->Send( "%-11.11s ", fields[l].cmd);
	if (!(++i % 7))
	  ch->Send( "\r\n");
      }
    }
    if (i % 7)
      ch->Send( "\r\n");
    if (i == 0)
      ch->Send( "None.\r\n");
    return;
  }
  for (l = 0; *(fields[l].cmd) != '\n'; l++)
    if (!strncmp(field, fields[l].cmd, strlen(field)))
      break;
   
  if (*fields[l].cmd == '\n') {
    ch->Send( "Invalid text editor option.\r\n");
    return;
  }
   
  if (GET_LEVEL(ch) < fields[l].level) {
    ch->Send( "You are not godly enough for that!\r\n");
    return;
  }

  /* set up editor stats */
  clear_screen(ch->desc);
  send_editor_help(ch->desc);
  ch->Send( "Edit file below:\r\n\r\n");

  if (ch->desc->olc) {
    new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: do_tedit: Player already had olc structure.");
    free(ch->desc->olc);
  }
  CREATE(ch->desc->olc, struct oasis_olc_data, 1);
  
  if (*fields[l].buffer) {
    ch->Send( "%s", *fields[l].buffer);
    backstr = strdup(*fields[l].buffer);
  }

  OLC_STORAGE(ch->desc) = strdup(fields[l].filename);
  string_write(ch->desc, (char **)fields[l].buffer, fields[l].size, 0, backstr);

  act("$n begins editing a scroll.", TRUE, ch, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);
  STATE(ch->desc) = CON_TEDIT;
}
