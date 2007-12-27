/* ************************************************************************
*   File: ban.c                                         Part of CircleMUD *
*  Usage: banning/unbanning/checking sites and player names               *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"

struct ban_list_element *ban_list = NULL;

/* local functions */
void load_banned(void);
int isbanned(char *hostname);
void _write_one_node(FILE * fp, struct ban_list_element *node);
void write_ban_list(void);
ACMD(do_ban);
ACMD(do_unban);
int Valid_Name(char *newname);
void Read_Invalid_List(void);
void Free_Invalid_List(void);
void free_ban_list(void);
void free_all_ban(struct ban_list_element *ban_node);

const char *ban_types[] = {
    "no",
    "new",
    "select",
    "all",
    "name",
    "ERROR"
};


void load_banned(void)
{
    FILE *fl;
    int i, date;
    char site_name[BANNED_SITE_LENGTH + 1], ban_type[100];
    char name[MAX_NAME_LENGTH + 1];
    struct ban_list_element *next_node;

    ban_list = 0;

    if (!(fl = fopen(BAN_FILE, "r"))) {
	if (errno != ENOENT) {
	    log("SYSERR: Unable to open banfile '%s': %s", BAN_FILE,
		strerror(errno));
	} else
	    log("   Ban file '%s' doesn't exist.", BAN_FILE);
	return;
    }
    while (fscanf(fl, " %s %s %d %s ", ban_type, site_name, &date, name) ==
	   4) {
	CREATE(next_node, struct ban_list_element, 1);
	strncpy(next_node->site, site_name, BANNED_SITE_LENGTH);
	next_node->site[BANNED_SITE_LENGTH] = '\0';
	strncpy(next_node->name, name, MAX_NAME_LENGTH);
	next_node->name[MAX_NAME_LENGTH] = '\0';
	next_node->date = date;

	for (i = BAN_NOT; i <= BAN_NAME; i++)
	    if (!strcmp(ban_type, ban_types[i]))
		next_node->type = i;

	next_node->next = ban_list;
	ban_list = next_node;
    }

    fclose(fl);
}


int isbanned(char *hostname)
{
    int i;
    struct ban_list_element *banned_node;
    char *nextchar;

    if (!hostname || !*hostname)
	return (0);

    i = 0;
    for (nextchar = hostname; *nextchar; nextchar++)
	*nextchar = LOWER(*nextchar);

    for (banned_node = ban_list; banned_node;
	 banned_node = banned_node->next)
	if (strstr(hostname, banned_node->site))	/* if hostname is a substring */
	    i = MAX(i, banned_node->type);

    return (i);
}


void _write_one_node(FILE * fp, struct ban_list_element *node)
{
    if (node) {
	_write_one_node(fp, node->next);
	fprintf(fp, "%s %s %ld %s\n", ban_types[node->type],
		node->site, (long) node->date, node->name);
    }
}



void write_ban_list(void)
{
    FILE *fl;

    if (!(fl = fopen(BAN_FILE, "w"))) {
	perror("SYSERR: Unable to open '" BAN_FILE "' for writing");
	return;
    }
    _write_one_node(fl, ban_list);	/* recursively write from end to start */
    fclose(fl);
    return;
}

#define BAN_LIST_FORMAT "%-40.40s  %-8.8s  %-10.10s  %-16.16s\r\n"
ACMD(do_ban)
{
    char flag[MAX_INPUT_LENGTH], site[MAX_INPUT_LENGTH], *nextchar;
    char  timestr[16];
    int i;
    struct ban_list_element *ban_node;

 

    if (!*argument) {
	if (!ban_list) {
	    send_to_char("No sites are banned.\r\n", ch);
	    return;
	}
	ch->Send( BAN_LIST_FORMAT,
		"Banned Site Name", "Ban Type", "Banned On", "Banned By");
	ch->Send( BAN_LIST_FORMAT,
		"---------------------------------",
		"---------------------------------",
		"---------------------------------",
		"---------------------------------");


	for (ban_node = ban_list; ban_node; ban_node = ban_node->next) {
	    if (ban_node->date) {
	    strlcpy(timestr, asctime(localtime(&(ban_node->date))), 10);
		timestr[10] = '\0';
	    } else
		strcpy(timestr, "Unknown");
	    ch->Send( BAN_LIST_FORMAT , ban_node->site, ban_types[ban_node->type],
		    timestr, ban_node->name);
	    
	}
	return;
    }
    two_arguments(argument, flag, site);
    if (!*site || !*flag) {
	send_to_char("Usage: ban {{all | select | new | name} site_name/playername\r\n", ch);
	return;
    }
    if (!
	(!str_cmp(flag, "select") || !str_cmp(flag, "all")
	 || !str_cmp(flag, "new") || !str_cmp(flag, "name"))) {
	send_to_char("Flag must be ALL, SELECT, NEW, or NAME.\r\n", ch);
	return;
    }
    for (ban_node = ban_list; ban_node; ban_node = ban_node->next) {
	if (!str_cmp(ban_node->site, site)) {
	    send_to_char
		("That site has already been banned -- unban it to change the ban type.\r\n",
		 ch);
	    return;
	}
    }

    CREATE(ban_node, struct ban_list_element, 1);
    strncpy(ban_node->site, site, BANNED_SITE_LENGTH);
    for (nextchar = ban_node->site; *nextchar; nextchar++)
	*nextchar = LOWER(*nextchar);
    ban_node->site[BANNED_SITE_LENGTH] = '\0';
    strncpy(ban_node->name, GET_NAME(ch), MAX_NAME_LENGTH);
    ban_node->name[MAX_NAME_LENGTH] = '\0';
    ban_node->date = time(0);

    for (i = BAN_NEW; i <= BAN_NAME; i++)
	if (!str_cmp(flag, ban_types[i]))
	    ban_node->type = i;

    ban_node->next = ban_list;
    ban_list = ban_node;

    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s has banned %s for %s players.", GET_NAME(ch), site,
	    ban_types[ban_node->type]);
    send_to_char("Site banned.\r\n", ch);
    write_ban_list();
}


ACMD(do_unban)
{
    char site[MAX_INPUT_LENGTH];
    struct ban_list_element *ban_node, *temp;
    int found = 0;

    one_argument(argument, site);
    if (!*site) {
	send_to_char("A site to unban might help.\r\n", ch);
	return;
    }
    ban_node = ban_list;
    while (ban_node && !found) {
	if (!str_cmp(ban_node->site, site))
	    found = 1;
	else
	    ban_node = ban_node->next;
    }

    if (!found) {
	send_to_char("That site is not currently banned.\r\n", ch);
	return;
    }
    REMOVE_FROM_LIST(ban_node, ban_list, next);
    send_to_char("Site unbanned.\r\n", ch);
    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s removed the %s-player ban on %s.",
	    GET_NAME(ch), ban_types[ban_node->type], ban_node->site);

    free(ban_node);
    write_ban_list();
}

void free_ban_list(void) {

free_all_ban(ban_list);

}

void free_all_ban(struct ban_list_element *ban_node) {

if (!ban_node)
return;

if (ban_node->next)
    free_all_ban(ban_node->next);
    
free(ban_node);


}


/**************************************************************************
 *  Code to check for invalid names (i.e., profanity, etc.)		  *
 *  Written by Sharon P. Goza						  *
 **************************************************************************/

#define MAX_INVALID_NAMES	200

char *invalid_list[MAX_INVALID_NAMES];
int num_invalid = 0;

int Valid_Name(char *newname)
{
    int i, wovels = 0;
    struct descriptor_data *dt;
    char tempname[MAX_INPUT_LENGTH];

    /*
     * Make sure someone isn't trying to create this same name.  We want to
     * do a 'str_cmp' so people can't do 'Bob' and 'BoB'.  The creating login
     * will not have a character name yet and other people sitting at the
     * prompt won't have characters yet.
     *
     * New, unindexed characters (i.e., characters who are in the process of creating)
     * will have an idnum of -1, set by clear_char() in db.c.  If someone is creating a
     *character by the same name as the one we are checking, then the name is invalid,
     * to prevent character duping.
     * THIS SHOULD FIX THE 'invalid name' if disconnected from OLC-bug - WELCOR 9/00
     */
    for (dt = descriptor_list; dt; dt = dt->next)
	if (dt->character && GET_NAME(dt->character)
	    && !str_cmp(GET_NAME(dt->character), newname))
	    if (GET_IDNUM(dt->character) == -1)
		return (IS_PLAYING(dt));

  /* count wovels */
  for (i = 0; newname[i]; i++) {
    if (strchr("aeiouyAEIOUY", newname[i]))
      wovels++;
      }
  

  /* return invalid if no wovels */
  if (!wovels)
    return (0);

    /* return valid if list doesn't exist */
    if (!invalid_list || num_invalid < 1)
	return (1);

    /* change to lowercase */
    strcpy(tempname, newname);
    for (i = 0; tempname[i]; i++)
	tempname[i] = LOWER(tempname[i]);
	
	if (isbanned(tempname) == BAN_NAME)
	 return (0);

    /* Does the desired name contain a string in the invalid list? */
    for (i = 0; i < num_invalid; i++)
	if (strstr(tempname, invalid_list[i]))
	    return (0);
	    
/*  for (i = 0; i < top_of_mobt; i++) {
    if (isname(tempname, mob_proto[i].player.name))
      return 0;
  }*/

    return (1);
}
/* What's with the wacky capitalization in here? */
void Free_Invalid_List(void)
{
  int invl;

  for (invl = 0; invl < num_invalid; invl++)
    free_string(&invalid_list[invl]);

  num_invalid = 0;
}

void Read_Invalid_List(void)
{
    FILE *fp;
    char temp[256];

    if (!(fp = fopen(XNAME_FILE, "r"))) {
	perror("SYSERR: Unable to open '" XNAME_FILE "' for reading");
	return;
    }

    num_invalid = 0;
    while (get_line(fp, temp) && num_invalid < MAX_INVALID_NAMES)
	invalid_list[num_invalid++] = str_dup(temp);

    if (num_invalid >= MAX_INVALID_NAMES) {
	log("SYSERR: Too many invalid names; change MAX_INVALID_NAMES in ban.c");
	exit(1);
    }

    fclose(fp);
}
