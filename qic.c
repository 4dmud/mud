/* QIC system, written by Mattias Larsson 1995 ml@eniac.campus.luth.se */
/*
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <errno.h>
*/

#include "conf.h"
#include "sysdep.h"

#include "screen.h"
#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "interpreter.h"
#include "handler.h"

/* External Structures */
extern const int xap_objs;
extern int Crash_clean_qic(FILE * fl, char *name);
void scan_char_objects_qic(char *name, long id);

/* Global variables for qic system */

FILE *qic_fl = NULL;		/* File identification for qic file */
int qic_items;			/* Number of items in database */

int qic_vnums[500];

void save_record(int j)
{				/* save one record */
    struct qic_data q;

    q.vnum = obj_index[j].vnum;
    q.limit = obj_index[j].qic->limit;
    q.items = obj_index[j].qic->items;
    fseek(qic_fl, obj_index[j].qic->vnum * sizeof(struct qic_data), SEEK_SET);	/* search for the record number */
    if (fwrite(&q, sizeof(struct qic_data), 1, qic_fl) < 1) {
	perror("Error writing QIC record (full save)");
	return;
    }
}

void load_qic(void)
{
    int size, i, nr, j;
    struct qic_data q;

    if (!(qic_fl = fopen(QIC_FILE, "r+b"))) {
	if (errno != ENOENT) {
	    perror("fatal error opening qic database");
	    exit(1);
	} else {
	    log("No QIC database. Creating a new one.");
	    touch(QIC_FILE);
	    if (!(qic_fl = fopen(QIC_FILE, "r+b"))) {
		perror("fatal error opening qic database");
		exit(1);
	    }
	}
    }

    /* add code here to load database :) */

    fseek(qic_fl, 0L, SEEK_END);
    size = ftell(qic_fl);
    rewind(qic_fl);
    if (size % sizeof(struct qic_data))
	fprintf(stderr, "WARNING:  QIC DATABASE IS PROBABLY CORRUPT!\n");
    qic_items = size / sizeof(struct qic_data);
    if (qic_items) {
	log( "   %d records in QIC database.", qic_items);
    } else {
	qic_items = 0;
	return;
    }
    i = 0;
    for (; !feof(qic_fl);) {
	fread(&q, sizeof(struct qic_data), 1, qic_fl);
	if ((nr = real_object(q.vnum)) < 0) {
	    i++;
	    log("Invalid vnum [%5d] in QIC database!", q.vnum);
	} else {
	if (obj_index[nr].qic == NULL)	
	    CREATE(obj_index[nr].qic, struct qic_data, 1);
	    qic_vnums[i] = q.vnum;
	    obj_index[nr].qic->vnum = i;
	    i++;
	    obj_index[nr].qic->limit = q.limit;
	    obj_index[nr].qic->items = q.items;

	    /* make sure it's reset to 0... hmm */
	    for (j = 0; j < QIC_OWNERS; j++)
		obj_index[nr].qic->owners[j] = 0;
	}
    }

}				/* end of load thing */

void save_qic(void)
{				/* save the whole qic database */
    struct qic_data q;
    int j;

    fclose(qic_fl);

    if (!(qic_fl = fopen(QIC_FILE, "w+b"))) {
	if (errno != ENOENT) {
	    perror("fatal error opening qicfile");
	    exit(1);
	}
    }

    for (j = 0; j <= top_of_objt; j++) {
	if (obj_index[j].qic != NULL) {
	    q.vnum = obj_index[j].vnum;
	    q.limit = obj_index[j].qic->limit;
	    q.items = obj_index[j].qic->items;
	    if (fwrite(&q, sizeof(struct qic_data), 1, qic_fl) < 1) {
		perror("Error writing QIC record (full save)");
		return;
	    }
	}
    }
    fclose(qic_fl);		/* close QIC database file */
}				/* end of save rec database full blaha.. i'm so tired */


/* Check if it's okay to load a QIC. If yes, it increase the num loaded
 * QIC's and then returns TRUE to the caller. Otherwise it returns FALSE,
 * and doesnt touch any counter at all. */

int load_qic_check(obj_rnum rnum)
{
    // sanity check here, fix the bug in the calling routine instead..
    if (rnum < 0) {
	log("Negative RNUM passed to load_qic_check");
	return (0);
    }

    if (obj_index[rnum].qic == NULL)
	return (1);		/* okay to load, go ahead, no QIC record */

//    log("Checking object %s.", obj_proto[rnum].short_description);
//    log("Items: %d, Limit: %d", obj_index[rnum].qic->items,
//        obj_index[rnum].qic->limit);   

    /* Items that are ARTIFACTS are normal QIC */
    if ((obj_index[rnum].qic->items >= obj_index[rnum].qic->limit)) {
	new_mudlog( CMP, LVL_SEN, TRUE, "(QIC) Too many '%s' in game already.",
		obj_proto[rnum].short_description);
//      
	return (0);		/* too many items in the game already */
    }

    /* Otherwise, QIC number acts like %load */
    if (number(0, 199) > obj_index[rnum].qic->limit) {
      new_mudlog(CMP, LVL_SEN, TRUE , "(QIC) '%s' failed load check (limit %d, exist %d)",
		obj_proto[rnum].short_description, obj_index[rnum].qic->limit, obj_index[rnum].qic->items - 1);
	return (0);		/* didn't make the %load */
    }

    /*  if (number(0,100) > 1) *//* 2% item loads in any case... */
    /*  if (obj_index[rnum].qic->items >= obj_index[rnum].qic->limit) */
    /*                  return 0; *//* too many items in the game already */

    //obj_index[rnum].qic->items++;	/* okay.. increase items counter */
    new_mudlog(CMP, LVL_SEN, TRUE, "(QIC) %s loaded.", obj_proto[rnum].short_description);
    return (1);			/* and return true */
}

void purge_qic(int rnum)
{
if (rnum < 0)
return;
    if (obj_index[rnum].qic == NULL) {
	return;
    } else {
	if (obj_index[rnum].qic->items > 0)
	    obj_index[rnum].qic->items--;
    }
}

void qic_load(int rnum)
{
if (rnum < 0)
return;
    if (obj_index[rnum].qic == NULL) {
	return;
    } else {
	obj_index[rnum].qic->items++;
    }
}

void add_owner(int nr, long id)
{
    int i;

    if (id < 1)
	return;
    if (nr < 1)

    for (i = 0; i < QIC_OWNERS; i++)
	if (obj_index[nr].qic->owners[i] == id)
	    return;

    for (i = 0; i < QIC_OWNERS; i++)
	if (obj_index[nr].qic->owners[i] < 1) {
	    obj_index[nr].qic->owners[i] = id;
	    return;
	}

}

/* Scan a single rent database file for QIC's */
void qic_scan_file(char *name, long id)
{
    char fname[MAX_STRING_LENGTH];
    struct rent_info rent;
    int j, nr, objnum;
    FILE *fl;
    char line[MAX_INPUT_LENGTH];
    char buf1[MAX_INPUT_LENGTH];

    if (!get_filename(name, fname, NEW_OBJ_FILES))
	return;

    if (!(fl = fopen(fname, "r+b"))) {
	if (errno != ENOENT) {	/* if it fails, NOT because of no file */
	    sprintf(buf1, "SYSERR: OPENING OBJECT FILE %s (4)", fname);
	    perror(buf1);
	}
	return;
    }

    if (!feof(fl))
	fread(&rent, sizeof(struct rent_info), 1, fl);

    while (!feof(fl)) {
	get_line(fl, line);
	if (*line == '#') {
	    if (sscanf(line, "#%d", &nr) != 1) {
		continue;
	    }
	    if (nr != NOTHING) {
		for (j = 0; j < qic_items; j++) {
		    if (nr == qic_vnums[j]) {
			if ((objnum = real_object(nr)) >= 0) {
			    obj_index[objnum].qic->items++;
//            log("Vnum: %5d, Owner: %10s, Count: %d", nr, name, obj_index[objnum].qic->items);
			    add_owner(objnum, id);
			}
		    }
		}
		// free_obj(obj);
	    }
	}
    }
    if (ferror(fl)) {
	fclose(fl);
	return;
    }


    Crash_clean_qic(fl, name);

}

/* Scans through the whole rent database and sets the current QIC values */
void qic_scan_rent(void)
{
    int i;

    /* okay, first of all, clean out the item counters loaded by load_qic() */
    for (i = 0; i < top_of_objt; i++)
	if (obj_index[i].qic != NULL) {
	    obj_index[i].qic->items = 0;
//          log("%5d, %3d.", obj_index[i].vnum, obj_index[i].qic->items);
	}

    for (i = 0; i <= top_of_p_table; i++)
	scan_char_objects_qic((player_table + i)->name, (player_table + i)->id); //objsave.c
    
    return;
}

ACMD(do_setqic)
{
    struct qic_data *q;
    int i, j;
    char buf[MAX_INPUT_LENGTH];
    char arg[MAX_INPUT_LENGTH];

    if (!*argument) {
	send_to_char("Usage: setqic [vnum] [limit]\r\n", ch);
	return;
    }

    two_arguments(argument, buf, arg);

    if ((i = real_object(atoi(buf))) < 0) {
	send_to_char("There is no object with that vnum.\r\n", ch);
	return;
    }

    if ((j = atoi(arg)) == 0) {	/* remove QIC */
	if (obj_index[i].qic == NULL) {
	    send_to_char("No QIC record to remove.\r\n", ch);
	    return;
	}
	free(obj_index[i].qic);	/* set QIC record to NULL */
	obj_index[i].qic = NULL;
	save_qic();		/* rewrite the whole QIC database (yuck!) */
	load_qic();		/* and load it again .. pfffft */
	send_to_char("QIC record removed.\r\n", ch);
	new_mudlog( NRM, LVL_SEN, TRUE,"QIC record for item %d removed by %s.", i,
		GET_NAME(ch));
	return;
    }

    if (obj_index[i].qic == NULL) {
	CREATE(q, struct qic_data, 1);
	obj_index[i].qic = q;
	q->vnum = qic_items;	/* rec in database file */
	q->items = 0;		/* since it's a new QIC, no items loaded */
	q->limit = j;		/* the QIC limit */
	qic_items++;		/* increase max pointer */
    } else {
	q = obj_index[i].qic;	/* we already have a QIC record */
	q->limit = j;		/* set the new limit */
    }
    save_record(i);		/* save the QIC record */
    new_send_to_char(ch, "%s", CONFIG_OK);
 
    new_mudlog( NRM, LVL_SEN, TRUE,"QIC record for item %d changed to %d removed by %s.", i,
	    j, GET_NAME(ch));

    save_qic();			/* rewrite the whole QIC database (yuck!) */
    load_qic();			/* and load it again .. pfffft */
}

ACMD(do_qicsave)
{

    send_to_char("Forcing save and reload of QIC item database.\r\n", ch);
    save_qic();
    load_qic();

}

ACMD(do_qicinfo)
{
    int i;
    char buf[MAX_INPUT_LENGTH];
    char buf2[MAX_INPUT_LENGTH];
    extern void strip_color(char *inbuf);
DYN_DEFINE;
    *buf = '\0';

    DYN_CREATE;
	*dynbuf = 0;
    sprintf(buf, "Currently defined QICs:\r\n");
    DYN_RESIZE(buf);


    for (i = 0; i <= top_of_objt; i++) {
	if (obj_index[i].qic != NULL) {
	   strcpy(buf2, obj_proto[i].short_description);
	   strip_color(buf2);
	    sprintf(buf,
		    "%s[%s%5d%s]%s %-50s %-2sIn:%s %2d%s, Lim: %s%2d%s\r\n",
		    CBBLU(ch, C_NRM), CBWHT(ch, C_NRM), obj_index[i].vnum,
		    CBBLU(ch, C_NRM), CCCYN(ch, C_NRM),
		    buf2, CBBLU(ch, C_NRM),
		    CBWHT(ch, C_NRM), obj_index[i].qic->items, CBBLU(ch,
								     C_NRM),
		    CBWHT(ch, C_NRM), obj_index[i].qic->limit, CCNRM(ch,
								     C_NRM));
	    DYN_RESIZE(buf);
	}
    }

    page_string(ch->desc, dynbuf, DYN_BUFFER);

}

/* List owners of a QIC item */
ACMD(do_owners)
{
    int i, j;
    char arg[MAX_INPUT_LENGTH];

    one_argument(argument, arg);

    if (!*arg) {
	send_to_char("Usage: owners <vnum>\r\n", ch);
	return;
    }

    if ((i = real_object(atoi(arg))) < 0) {
	send_to_char("There is no object with that vnum.\r\n", ch);
	return;
    }

    if (obj_index[i].qic == NULL) {
	send_to_char("That item is not QIC.\r\n", ch);
	return;
    }

    new_send_to_char(ch, "Registered owners at boot (item #%d - %s):\r\n",
	    obj_index[i].vnum, obj_proto[i].short_description);

    for (j = 0; j < QIC_OWNERS; j += 2) {
	if (obj_index[i].qic->owners[j] < 1)
	    break;
	new_send_to_char(ch, "%20.20s  %20.20s\r\n",
		get_name_by_id(obj_index[i].qic->owners[j]),
		obj_index[i].qic->owners[j +
					 1] ? get_name_by_id(obj_index[i].
							     qic->
							     owners[j +
								    1]) :
		"");
    }
    new_send_to_char(ch, "\r\n");

}

ACMD(do_qload)
{
    struct obj_data *obj;
    int number, r_num;
    char buf[MAX_INPUT_LENGTH];

    one_argument(argument, buf);

    if (!*buf || !isdigit(*buf)) {
	send_to_char("Usage: qload <virt num>\r\n", ch);
	return;
    }
    if ((number = atoi(buf)) < 0) {
	send_to_char("A NEGATIVE number??\r\n", ch);
	return;
    }

    if ((r_num = real_object(number)) < 0) {
	send_to_char("There is no object with that number.\r\n", ch);
	return;
    }
    obj = read_object(r_num, REAL);
    if (obj == NULL) {
	send_to_char("Error loading QIC - report asap!\r\n", ch);
	return;
    }
    obj_to_char(obj, ch);
    qic_load(r_num);
    act("$n makes a strange powerful gesture.", TRUE, ch, 0, 0, TO_ROOM);
    act("$n has created $p!", FALSE, ch, obj, 0, TO_ROOM);
    act("You create $p.", FALSE, ch, obj, 0, TO_CHAR);
    new_mudlog(NRM, LVL_SEN, TRUE, "%s QIC created %s", GET_NAME(ch),
	    obj->short_description);
}
