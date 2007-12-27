/************************************************************************
 * Generic OLC Library - General / genolc.c			v1.0	*
 * Original author: Levork						*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#define __GENOLC_C__

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "db.h"
#include "handler.h"
#include "comm.h"
#include "shop.h"
#include "oasis.h"
#include "genolc.h"
#include "genwld.h"
#include "genmob.h"
#include "genshp.h"
#include "genzon.h"
#include "genobj.h"
#include "dg_olc.h"
#include "constants.h"

extern struct index_data **trig_index;
extern int top_shop;

int save_config( int nowhere );        /* Exported from cedit.c */

/* Adjustment for top_shop change between bpl15 and bpl16. */
#if _CIRCLEMUD < CIRCLEMUD_VERSION(3,0,16)
int top_shop_offset = 1;
#else
int top_shop_offset = 0;
#endif

/*
 * List of zones to be saved.
 */
struct save_list_data *save_list;

/*
 * Structure defining all known save types.
 */
struct known_save_types {
    int save_type;
    int (*func)(int rnum);
    const char *message;
}
save_types[] = {
                   { SL_MOB, save_mobiles , "mobile" },
                   { SL_OBJ, save_objects, "object" },
                   { SL_SHP, save_shops, "shop" },
                   { SL_WLD, save_rooms, "room" },
                   { SL_ZON, save_zone, "zone" },
                   { SL_CFG, save_config, "config" },
                   { SL_ACTION, NULL, "social" },
                   { SL_HELP, NULL, "help"},
                   { -1, NULL, NULL },
               };

/* -------------------------------------------------------------------------- */

int genolc_checkstring(Descriptor *d, char *arg) {
    smash_tilde(arg);
    return TRUE;
}

char *str_udup(const char *txt) {
    return strdup((txt && *txt) ? txt : "undefined");
}
string udup(const char *txt) {
    return string(((txt && *txt) ? txt : "undefined"));
}

/*
 * Original use: to be called at shutdown time.
 */
int save_all(void) {
    while (save_list) {
        if (save_list->type < 0 || save_list->type > SL_MAX) {
            if (save_list->type == SL_HELP) {
                log("Help not saved - can not autosave. Use 'hedit save'.");
                save_list = save_list->next;	/* Fatal error, skip this one. */
            } else if (save_list->type == SL_ACTION) {
                log("Actions not saved - can not autosave. Use 'aedit save'.");
                save_list = save_list->next;	/* Fatal error, skip this one. */
            } else
                log("SYSERR: GenOLC: Invalid save type %d in save list.\n", save_list->type);
        } else if ((*save_types[save_list->type].func)(real_zone(save_list->zone)) < 0)
            save_list = save_list->next;	/* Fatal error, skip this one. */
    }

    return TRUE;
}

/* -------------------------------------------------------------------------- */

/*
 * NOTE: This changes the buffer passed in.
 */
void strip_cr(char *buffer) {
    int rpos, wpos;

    if (buffer == NULL)
        return;

    for (rpos = 0, wpos = 0; buffer[rpos]; rpos++) {
        buffer[wpos] = buffer[rpos];
        wpos += (buffer[rpos] != '\r');
    }
    buffer[wpos] = '\0';
}

/* -------------------------------------------------------------------------- */

void copy_ex_descriptions(struct extra_descr_data **to, struct extra_descr_data *from) {
    struct extra_descr_data *wpos;

    CREATE(*to, struct extra_descr_data, 1);
    wpos = *to;

    for (; from; from = from->next, wpos = wpos->next) {
        wpos->keyword = str_udup(from->keyword);
        wpos->description = str_udup(from->description);
        if (from->next)
            CREATE(wpos->next, struct extra_descr_data, 1);
    }
}

/* -------------------------------------------------------------------------- */

void free_ex_descriptions(struct extra_descr_data *head) {
    struct extra_descr_data *thised, *next_one;

    if (!head) {
        log("free_ex_descriptions: NULL pointer or NULL data.");
        return;
    }

    for (thised = head; thised; thised = next_one) {
        next_one = thised->next;
        if (thised->keyword)
            free(thised->keyword);
        if (thised->description)
            free(thised->description);
        free(thised);
    }
}

/* -------------------------------------------------------------------------- */

int remove_from_save_list(zone_vnum zone, int type) {
    struct save_list_data *ritem, *temp;

    for (ritem = save_list; ritem; ritem = ritem->next)
        if (ritem->zone == zone && ritem->type == type)
            break;

    if (ritem == NULL) {
        log("SYSERR: remove_from_save_list: Saved item not found. (%d/%d)", zone, type);
        return FALSE;
    }
    REMOVE_FROM_LIST(ritem, save_list, next);
    free(ritem);
    return TRUE;
}

/* -------------------------------------------------------------------------- */

int add_to_save_list(zone_vnum zone, int type) {
    struct save_list_data *nitem;
    zone_rnum rznum;

    if (type == SL_CFG)
        return FALSE;

    rznum = real_zone(zone);
    if (rznum == NOWHERE || rznum > top_of_zone_table) {
        if (zone != AEDIT_PERMISSION && zone != HEDIT_PERMISSION) {
            log("SYSERR: add_to_save_list: Invalid zone number passed. (%d => %d, 0-%d)", zone, rznum, top_of_zone_table);
            return FALSE;
        }
    }

    for (nitem = save_list; nitem; nitem = nitem->next)
        if (nitem->zone == zone && nitem->type == type)
            return FALSE;

    CREATE(nitem, struct save_list_data, 1);
    nitem->zone = zone;
    nitem->type = type;
    nitem->next = save_list;
    save_list = nitem;
    return TRUE;
}

/* -------------------------------------------------------------------------- */

int in_save_list(zone_vnum zone, int type) {
    struct save_list_data *nitem;

    for (nitem = save_list; nitem; nitem = nitem->next)
        if (nitem->zone == zone && nitem->type == type)
            return TRUE;

    return FALSE;
}

/* -------------------------------------------------------------------------- */

/*
 * Used from do_show(), ideally.
 */
void do_show_save_list(Character *ch) {
    if (save_list == NULL)
        ch->Send( "All world files are up to date.\r\n");
    else {
        struct save_list_data *item;

        ch->Send( "The following files need saving:\r\n");
        for (item = save_list; item; item = item->next) {
            if (item->type != SL_CFG)
                ch->Send( " - %s data for zone %d.\r\n", save_types[item->type].message, item->zone);
            else
                ch->Send( " - Game configuration data.\r\n");
        }
    }
}

/* -------------------------------------------------------------------------- */

/*
 * TEST FUNCTION! Not meant to be pretty!
 *
 * edit q	- Query unsaved files.
 * edit a	- Save all.
 * edit r1204 c	- Copies current room to 1204.
 * edit r1204 d	- Deletes room 1204.
 * edit r12 s	- Saves rooms in zone 12.
 * edit m3000 c3001	- Copies mob 3000 to 3001.
 * edit m3000 d	- Deletes mob 3000.
 * edit m30 s	- Saves mobiles in zone 30.
 * edit o186 s	- Saves objects in zone 186.
 * edit s25 s	- Saves shops in zone 25.
 * edit z31 s	- Saves zone 31.
 */
#include "interpreter.h"
ACMD(do_edit);
ACMD(do_edit) {
    int idx, num, mun;
    char a[MAX_INPUT_LENGTH], b[MAX_INPUT_LENGTH];

    two_arguments(argument, a, b);
    num = atoi(a + 1);
    mun = atoi(b + 1);
    switch (a[0]) {
    case 'a':
        save_all();
        break;
    case 'm':
        switch (b[0]) {
        case 'd':
            if (MobProtoExists(num)) {
                delete_mobile(num);	/* Delete -> Real */
                ch->Send( "%s", CONFIG_OK);
            } else
                ch->Send( "What mobile?\r\n");
            break;
        case 's':
            save_mobiles(num);
            break;
        case 'c':
            if (!MobProtoExists(num))
                ch->Send( "What mobile?\r\n");
            else if (!MobProtoExists(mun))
                ch->Send( "Can only copy over an existing mob.\r\n");
            else {
                /* Otherwise the old ones have dangling string pointers. */
                extract_mobile_all(GetMobIndex(mun)->vnum);
                /* To <- From */
                copy_mobile(GetMobProto(mun), GetMobProto(num));
                ch->Send( "%s", CONFIG_OK);
            }
            break;
        }
        break;
    case 's':
        switch (b[0]) {
        case 's':
            save_objects(real_zone(num));
            break;
        }
        break;
    case 'z':
        switch (b[0]) {
        case 's':
            save_zone(real_zone(num));
            break;
        }
        break;
    case 'o':
        switch (b[0]) {
        case 's':
            save_objects(real_zone(num));
            break;
        }
        break;
    case 'r':
        switch (b[0]) {
        case 'd':
            if ((idx = num) != NOWHERE) {
                delete_room(world_vnum[idx]);
                ch->Send( "%s", CONFIG_OK);
            } else
                ch->Send( "What room?\r\n");
            break;
        case 's':
            save_rooms(real_zone(num));
            break;
        case 'c':
            duplicate_room(num, IN_ROOM(ch));	/* To -> Virtual, From -> Real */
            break;
        }
    case 'q':
        do_show_save_list(ch);
        break;
    default:
        ch->Send( "What?\r\n");
        break;
    }
}

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int sprintascii(char *out, bitvector_t bits) {
    int i, j = 0;
    /* 32 bits, don't just add letters to try to get more unless your bitvector_t is also as large. */
    char *flags = "abcdefghijklmnopqrstuvwxyzABCDEF";

    for (i = 0; flags[i] != '\0'; i++)
        if (bits & (1 << i))
            out[j++] = flags[i];

    if (j == 0) /* Didn't write anything. */
        out[j++] = '0';

    /* NUL terminate the output string. */
    out[j++] = '\0';
    return j;
}
