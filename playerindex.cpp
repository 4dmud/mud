//
// C++ Implementation: playerindex
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2007
//
// Copyright: See COPYING file that comes with this distribution
//
//

#include "config.h"
#include "sysdep.h"

#include <dirent.h>


#include "structs.h"
#include "utils.h"
#include "db.h"
#include "comm.h"
#include "handler.h"
#include "spells.h"
#include "mail.h"
#include "interpreter.h"
#include "house.h"
#include "constants.h"
#include "oasis.h"
#include "dg_scripts.h"
#include "dg_event.h"
#include "descriptor.h"
#include "strutil.h"
#include "clan.h"



void PlayerIndex::FreeSelf() {
    for (plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++)
        if ((*ptvi).name)
            delete[] (*ptvi).name;
    player_table.clear();
    top_of_p_table = -1;
}


/* new version to build the player index for the ascii pfiles
 * generate index table for the player file
 */
void PlayerIndex::Build() {
    int rec_count = 0, i, retval;
    FILE* plr_index;
    char index_name[80], line[READ_SIZE], bits[64];
    char arg2[80];
    int save_index = FALSE;

    snprintf(index_name, sizeof(index_name), "%s", PLR_INDEX_FILE);
    if ((plr_index = fopen(index_name, "r+")) == NULL) {
        top_of_p_table = -1;
        log("No player index file!  First new char will be IMP!");
        return;
    }

    /* count the number of players in the index */
    while (get_line(plr_index, line))
        if (*line != '~')
            rec_count++;
    rewind(plr_index);

    if (rec_count == 0) {
        top_of_p_file = top_of_p_table = -1;
        return;
    }
    log("   %d players in database.", rec_count);

    //CREATE(player_table, struct player_index_element, rec_count);
    player_index_element pte = player_index_element();
    int id_zero = 0;
    for (i = 0; i < rec_count; i++) {
        get_line(plr_index, line);
        if ((retval = sscanf(line, "%ld %s %d %s %ld %ld %hd %hd %lld %hd", &pte.id, arg2,
                             &pte.level, bits,  &pte.last, &pte.account,
                             &pte.clan, &pte.rank, &pte.gc_amount, &pte.gt_amount)) < 10) {
            if (pte.id <= 0)
                id_zero++;
            if (id_zero >= 1)
                pte.repair = TRUE;
            if (retval == 5) {
                pte.account = pte.id;
                save_index = TRUE;
            } else if (retval < 10)
                save_index = TRUE;
            else {
                log("Player Index Error! Line %d.", i);
                exit(1);
            }
        }
        pte.name = new char[strlen(arg2) + 1];
        //CREATE(pi.NameByIndex(i), char, strlen(arg2) + 1);
        strcpy(pte.name, arg2);
        *pte.name = LOWER(*pte.name);
        pte.flags = asciiflag_conv(bits);
        TopIdNum = MAX(TopIdNum, pte.id);
        player_table.push_back(pte);
    }
    fclose(plr_index);
    top_of_p_file = top_of_p_table = i - 1;

    if (save_index) {
        plrindex_it ptv;
        Character *victim;
        log("    fixing index fields: clans");

        TEMP_LOAD_CHAR = TRUE;
        for (ptv = player_table.begin(); ptv != player_table.end(); ptv++) {

            if ((*ptv).name && !IS_SET((*ptv).flags, PINDEX_DELETED) && !IS_SET((*ptv).flags, PINDEX_SELFDELETE)) {
                victim = new Character(FALSE);

                if (store_to_char((*ptv).name, victim) > -1) {
                    (*ptv).clan = GET_CLAN(victim);
                    (*ptv).rank = GET_CLAN_RANK(victim);
                    (*ptv).gc_amount = GET_GOLD(victim) + GET_BANK_GOLD(victim);
                    (*ptv).gt_amount = GET_GOLD_TOKEN_COUNT(victim);
                    delete victim;
                } else
                    delete victim;

            }
        }

        TEMP_LOAD_CHAR = FALSE;
        Save();
    }
}

void PlayerIndex::Save() {
    int cnt = 0;
    char bits[64];
    FILE *index_file;
    char tempname[MAX_INPUT_LENGTH];
    snprintf(tempname, sizeof(tempname), "%s%s", PLR_INDEX_FILE, ".tmp");
    if (!(index_file = fopen(tempname, "w"))) {
        log("SYSERR:  Could not write player index file");
        ALERT_2;
        return;
    }

    for (plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++) {
        cnt = 0;
        if (*(*ptvi).name &&
                !IS_SET((*ptvi).flags, PINDEX_DELETED) &&
                !IS_SET((*ptvi).flags, PINDEX_SELFDELETE)) {
            for (plrindex_it ptvj = player_table.begin(); ptvj != player_table.end(); ptvj++) {
                if (cnt == 0 &&
                        *(*ptvi).name == *(*ptvj).name &&
                        !strcmp((*ptvi).name, (*ptvj).name) &&
                        !IS_SET((*ptvi).flags, PINDEX_DELETED) &&
                        !IS_SET((*ptvi).flags, PINDEX_SELFDELETE)) {
                    cnt++;
                    sprintbits((*ptvi).flags, bits);
                    *(*ptvi).name = LOWER(*(*ptvi).name);
                    fprintf(index_file, "%ld %s %d %s %ld %ld %hd %hd %lld %hd\n",
                            (*ptvi).id, (*ptvi).name,
                            (*ptvi).level, *bits ? bits : "0",
                            (*ptvi).last, (*ptvi).account,
                            (*ptvi).clan, (*ptvi).rank, (*ptvi).gc_amount, (*ptvi).gt_amount);
                }
            }
        }
    }
    int i = fprintf(index_file, "~\n");
    fclose(index_file);
    if (i < 0)
        remove
            (tempname);
    else
        rename(tempname, PLR_INDEX_FILE);


}



void PlayerIndex::RemovePlayer(plrindex_it ptvi) {
    char backup_name[128], pfile_name[128];
    FILE *backup_file;

    if (!*(*ptvi).name)
        return;

    if ((*ptvi).level >= pfile_backup_minlevel
            && backup_wiped_pfiles &&
            (!selfdelete_fastwipe || !IS_SET((*ptvi).flags,PINDEX_SELFDELETE))) {
        snprintf(backup_name, sizeof(backup_name), "%s/%s.%d", BACKUP_PREFIX,
                 (*ptvi).name, (int) time(0));
        if (!(backup_file = fopen(backup_name, "w"))) {
            log( "PCLEAN: Unable to open backup file %s.",
                 backup_name);
            return;
        }

        fprintf(backup_file, "**\n** PFILE: %s\n**\n",
                (*ptvi).name);
        snprintf(pfile_name, sizeof(pfile_name), "%s/%c/%s%s", PLR_PREFIX,
                 *(*ptvi).name,
                 (*ptvi).name, PLR_SUFFIX);
        //      if(!fcat(pfile_name, backup_file))
        //              fprintf(backup_file, "** (NO FILE)\n");
        fclose(backup_file);

        remove
            (pfile_name);
    }
    log( "PCLEAN: %s Lev: %d Last: %s",
         (*ptvi).name, (*ptvi).level,
         asctime(localtime(&(*ptvi).last)));
    (*ptvi).name[0] = '\0';
    Save();
}

void PlayerIndex::CleanPFiles() {
    int timeout = 0;
    time_t tm = time(0);

    for (plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++) {
        if (IS_SET((*ptvi).flags, PINDEX_NODELETE))
            continue;
        timeout = -1;

        if ((IS_SET((*ptvi).flags, PINDEX_DELETED)) || ((*ptvi).level < 40 && (*ptvi).clan == 12)) {
            timeout = 90;

            timeout *= SECS_PER_REAL_DAY;
            if ((tm - (*ptvi).last) > timeout)
                RemovePlayer(ptvi);
        }
    }
}

long PlayerIndex::TableIndexByName(const char *name) {
    for (int i = 0; i < player_table.size(); i++)
        if (!str_cmp(player_table[i].name, name))
            return (i);
    stringstream ss;
    string s;
    ss << "Name " << name << " not found in player index.";
    s = ss.str();
    throw MudException(s);   
}

long PlayerIndex::TableIndexById(long &id) {
    for (int i = 0; i < player_table.size(); i++)
        if (player_table[i].id == id)
            return (i);

    stringstream ss;
    string s;
    ss << "Id " << id << " not found in player index.";
    s = ss.str();
    throw MudException(s);
}

long PlayerIndex::GetAccByName(const char *name) {
    for (plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++)
        if (!str_cmp((*ptvi).name, name))
            return ((*ptvi).account);

    return (-1);
}

long PlayerIndex::GetAccById(long id) {
    for (plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++)
        if ((*ptvi).id == id)
            return ((*ptvi).account);

    return -1;
}

//returns index of player num for account acc
int PlayerIndex::get_account_num(int num, long acc) {
    int i = 0, n = 0;
    for (plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++, i++)
        if (!DeletedByStruct(&(*ptvi)) && (*ptvi).account == acc && n++ == num)
            return (i);

    throw MudException("That is not a valid number!\r\n");
}


static int valid_file(const struct dirent *a1) {
    if (!a1)
        return 0;
    else if (!a1->d_name)
        return 0;
    else if (!*a1->d_name) //no blank filenames
        return 0;

    return 1;
}

int PlayerIndex::clean_dir(char *dirname) {
    struct dirent **eps = NULL;
    int n, tp;
    int found = FALSE;
    char filename[2048];
    n = scandir(dirname, &eps, (valid_file), alphasort);
    if (n >= 0) {
        int cnt;
        for (cnt = 0; cnt < n; ++cnt) {
            if (eps[cnt]->d_type == DT_REG ) {
                found = FALSE;
                for (tp = 0; tp < player_table.size(); tp++) {
                    if (*player_table[tp].name == *eps[cnt]->d_name) {
                        /*if (0 && str_str(eps[cnt]->d_name, player_table[tp].name))
                        log("Checking: %s, to see if it is a substring of %s, %d", player_table[tp].name, eps[cnt]->d_name, strncmp(player_table[tp].name, eps[cnt]->d_name, strlen(player_table[tp].name)));*/

                        if (strncmp(player_table[tp].name, eps[cnt]->d_name, strlen(player_table[tp].name)) == 0
                                /** && !isalpha(eps[cnt]->d_name[strlen(player_table[tp].name)+1])**/) {
                            found = TRUE;
                            break;
                        }
                    }
                }
                if (found == FALSE) {
                    snprintf(filename, sizeof(filename)-1, "%s%s", dirname, eps[cnt]->d_name);
                    log("REMOVING: %s - %s", filename, remove
                            (filename) == 0 ? "SUCCESS" : "FAILED");
                }

            }
            free(eps[cnt]);
        }
    }
    return 0;
}

/*************************************************************************
*  stuff related to the save/load player system                   *
*************************************************************************/
int PlayerIndex::LoadChar(const char *name, Character *ch) {
    Character *tch;
    int ret_val = 0;
    string chname;

    if (ch == NULL) {
        new_mudlog(NRM, LVL_GOD, TRUE,"SYSERR: load_char recieved null ch!");
        return -1;
    }

    if (player_table.size() == 0)
        return -1;

    for (tch = character_list;tch;tch = tch->next) {
        if (!IS_NPC(tch) && !strcmp(GET_NAME(tch), name)) {
            log("load_char loading a character (%s) that is already in the game!", name);
        }
    }
    //for (k = 0; (*(name + k) = LOWER(*(name + k))); k++);
    chname = tolower(string(name));

    for (plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++) {
        if (!IS_SET((*ptvi).flags, PINDEX_DELETED) &&
                !IS_SET((*ptvi).flags, PINDEX_SELFDELETE) &&
                (!(*ptvi).name || !*(*ptvi).name))
            continue;
        if (*(*ptvi).name == chname[0] && !strcmp((*ptvi).name, chname.c_str())) {
            ret_val = store_to_char(chname.c_str(), ch);
            if (ret_val != -1)
                return ((*ptvi).id);
            else
                return -2;
        }
    }
    log("NAME: '%s' not found in player index. Create new...", chname.c_str());
    return -1;
}

/*
 * Create a new entry in the in-memory index table for the player file.
 * If the name already exists, by overwriting a deleted character, then
 * we re-use the old position.
 */
int PlayerIndex::create_entry(const char *name) {
    int pos = 0;
    bool new_entry = FALSE;
    player_index_element pie = player_index_element();

    if (top_of_p_table == -1) {  /* no table */
        new_entry = TRUE;
    } else {
        try {
            pos = TableIndexByName(name);
	} catch (MudException &e) {  /* new name */
            pos = top_of_p_table;
            new_entry = TRUE;
        }
    }

pie.name = new char[strlen(name) + 1];

    if (!new_entry) {
        if (player_table[pos].name)
            delete[] player_table[pos].name;
        player_table[pos] = pie;
    } else {
        pos = player_table.size();
        top_of_p_table++;
        player_table.push_back(pie);
    }

    /* copy lowercase equivalent of name to table field */
    for (int i = 0; (player_table[pos].name[i] = LOWER(name[i])); i++)
        /* Nothing */
        ;


    return (pos);
}

void PlayerIndex::change_plrindex_name(long id, char *change) {
    int tp;

    if (!change)
        return;
    for (tp = 0; tp < Size(); tp++) {
        if (player_table[tp].id == id) {
            if (player_table[tp].name)
                delete[] player_table[tp].name;
            player_table[tp].name = new char[strlen(change) +1];
            strcpy(player_table[tp].name, change);
            Save();
            return;
        }
    }
}
