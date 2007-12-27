/**************************************************************************
*  File: ignore.c                                                         *
*  Usage: Ignoring Players                                                *
*                                                                         *
*  Written by Josh Brittenham                                             *
*                                                                         *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "comm.h"
#include "handler.h"
#include "strutil.h"


/**
If ch is ignoring vict then this function returns 1
otherwise it returns 0
*/
int is_ignoring(Character *ch, Character *vict) {
    if (!ch || !vict)
        return (0);

    if (IS_NPC(ch) || IS_NPC(vict))
        return (0);

    if (find(GET_IGNORELIST(ch).begin(), GET_IGNORELIST(ch).end(), tolower(string(GET_PC_NAME(vict)))) != GET_IGNORELIST(ch).end())
        return (1);

    return (0);
}
void print_ignorelist(Character *ch, Character *vict) {
    if (!ch || !vict)
        return;

    if (IS_NPC(ch) || IS_NPC(vict))
        return;

    for (vector<string>::iterator s = GET_IGNORELIST(ch).begin();s != GET_IGNORELIST(ch).end();s++)
        *vict << (*s) << "\r\n";

}

void write_ignorelist(Character *ch) {
    FILE *file;
    char ignoref[127];
    int valid_to_save(const char *name);

    get_filename(GET_NAME(ch), ignoref, IGNORE_FILE);
    unlink(ignoref);
    if (!GET_IGNORELIST(ch).size() == 0)
        return;
    if ((file = fopen(ignoref, "wt")) == NULL) {
        log("Can't open ignore file %s", ignoref);
        return;
    }
    for (vector<string>::iterator s = GET_IGNORELIST(ch).begin();s != GET_IGNORELIST(ch).end();s++) {
        if (valid_to_save((*s).c_str()))
            fprintf(file, "%d\n%s\n", (*s).length(), (*s).c_str());
    }
    fclose(file);
}

void read_ignorelist(Character *ch) {
    FILE *file;
    char ignoref[127];
    char buf[127];
    size_t ignorelength;
    get_filename(GET_NAME(ch), ignoref, IGNORE_FILE);
    file = fopen(ignoref, "r");
    if (!file)
        return;


    do {
        if (!fscanf(file, "%d\n", &ignorelength)) {
            fclose(file);
            return;
        }
        if (!fgets(buf, ignorelength + 1, file)) {
            fclose(file);
            return;
        }
        GET_IGNORELIST(ch).push_back(tolower(string(buf)));
        ;
    } while (!feof(file));
    fclose(file);

    sort(GET_IGNORELIST(ch).begin(), GET_IGNORELIST(ch).end());
}

