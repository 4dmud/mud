/* ******************************************************************** *
 * FILE        : assemblies.c                  Copyright (C) 1999 Geoff Davis
*
 * USAGE: Implementation for assembly engine.                          *
 * -------------------------------------------------------------------- *
 * 1999 MAY 07 gdavis/azrael@laker.net Initial implementation.         *
 * ******************************************************************** */

#define __ASSEMBLIES_C__

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"

#include "assemblies.h"
#include "comm.h"
#include "constants.h"
#include "db.h"
#include "handler.h"
#include "interpreter.h"
#include "descriptor.h"
/* inroom = 1, inventory = 0, hold = 2, wield = 3 */

/* Local global variables. */
long           g_lNumAssemblies = 0;
ASSEMBLY       *g_pAssemblyTable = NULL;

void assemblyBootAssemblies( void ) {
    char         szLine[ MAX_INPUT_LENGTH ] = { '\0' };
    char         szTag[ MAX_INPUT_LENGTH ] = { '\0' };
    char         szType[ MAX_INPUT_LENGTH ] = { '\0' };
    int          iExtract = 0;
    int          iInRoom = 0; /* inroom = 1, inventory = 0, hold = 2, wield = 3 */
    int          iType = 0;
    long         lLineCount = 0;
    long         lPartVnum = NOTHING;
    long         lVnum = NOTHING;
    FILE         *pFile = NULL;

    if( (pFile = fopen( ASSEMBLIES_FILE, "rt" )) == NULL ) {
        log( "SYSERR: assemblyBootAssemblies(): Couldn't open file '%s' for "
             "reading.", ASSEMBLIES_FILE );
        return;
    }

    while( !feof( pFile ) ) {
        lLineCount += get_line( pFile, szLine );
        half_chop( szLine, szTag, szLine );

        if( *szTag == '\0' )
            continue;

        if( str_cmp( szTag, "Component" ) == 0 ) {
            if( sscanf( szLine, "#%ld %d %d", &lPartVnum, &iExtract, &iInRoom ) != 3 ) {
                log( "SYSERR: bootAssemblies(): Invalid format in file %s, line %ld: "
                     "szTag=%s, szLine=%s.", ASSEMBLIES_FILE, lLineCount, szTag, szLine );
            } else if( !assemblyAddComponent( lVnum, lPartVnum, iExtract, iInRoom ) ) {
                log( "SYSERR: bootAssemblies(): Could not add component #%ld to "
                     "assembly #%ld.", lPartVnum, lVnum );
            }
        } else if( str_cmp( szTag, "Vnum" ) == 0 ) {
            if( sscanf( szLine, "#%ld %s", &lVnum, szType ) != 2 ) {
                log( "SYSERR: bootAssemblies(): Invalid format in file %s, "
                     "line %ld.", ASSEMBLIES_FILE, lLineCount );
                lVnum = NOTHING;
            } else if( (iType = search_block( szType, AssemblyTypes, TRUE )) < 0 ) {
                log( "SYSERR: bootAssemblies(): Invalid type '%s' for assembly "
                     "vnum #%ld at line %ld.", szType, lVnum, lLineCount );
                lVnum = NOTHING;
            } else if( !assemblyCreate( lVnum, iType ) ) {
                log( "SYSERR: bootAssemblies(): Could not create assembly for vnum "
                     "#%ld, type %s.", lVnum, szType );
                lVnum = NOTHING;
            }
        } else {
            log( "SYSERR: Invalid tag '%s' in file %s, line #%ld.", szTag,
                 ASSEMBLIES_FILE, lLineCount );
        }

        *szLine = '\0';
        *szTag = '\0';
    }

    fclose( pFile );
}

void assemblySaveAssemblies( void ) {
    char         szType[ MAX_STRING_LENGTH ] = { '\0' };
    long         i = 0;
    long         j = 0;
    ASSEMBLY     *pAssembly = NULL;
    FILE         *pFile = NULL;

    if( (pFile = fopen( ASSEMBLIES_FILE, "wt" )) == NULL ) {
        log( "SYSERR: assemblySaveAssemblies(): Couldn't open file '%s' for "
             "writing.", ASSEMBLIES_FILE );
        return;
    }

    for( i = 0; i < g_lNumAssemblies; i++) {
        pAssembly = (g_pAssemblyTable + i);
        sprinttype(pAssembly->uchAssemblyType,AssemblyTypes,szType, sizeof(szType));
        fprintf( pFile, "Vnum                #%ld %s\n", pAssembly->lVnum, szType );

        for( j = 0; j < pAssembly->lNumComponents; j++ ) {
            fprintf( pFile, "Component           #%ld %d %d\n",
                     pAssembly->pComponents[ j ].lVnum,
                     (pAssembly->pComponents[ j ].bExtract ? 1 : 0),
                     pAssembly->pComponents[ j ].bInRoom  );
        }

        if( i < g_lNumAssemblies - 1 )
            fprintf( pFile, "\n" );
    }

    fclose( pFile );
}
const char *compIn(int i) {
    switch (i) {
    case 0:
        return "Inventory";
        break;
    case 1:
        return "Room";
        break;
    case 2:
        return "Hold";
        break;
    case 3:
        return "Wield";
        break;
    }
    return (char *)"";
}

void assemblyListToChar( Character *pCharacter ) {
    char         szBuffer[ MAX_INPUT_LENGTH ] = { '\0' };
    char         szAssmType[ MAX_INPUT_LENGTH ] = { '\0' };
    long         i = 0;                  // Outer iterator.
    long         j = 0;                  // Inner iterator.
    long         lRnum = 0;              // Object rnum for obj_proto indexing.

    if( pCharacter == NULL ) {
        log( "SYSERR: assemblyListAssembliesToChar(): NULL 'pCharacter'." );
        return;
    } else if( g_pAssemblyTable == NULL ) {
        pCharacter->Send( "No assemblies exist.\r\n");
        return;
    }

    /* Send out a "header" of sorts. */
    pCharacter->Send( "The following assemblies exists:\r\n");

    for( i = 0; i < g_lNumAssemblies; i++ ) {
        if( (lRnum = real_object( g_pAssemblyTable[ i ].lVnum )) < 0 ) {
            pCharacter->Send( "[-----] ***RESERVED***\r\n");
            log( "SYSERR: assemblyListToChar(): Invalid vnum #%ld in assembly table.", g_pAssemblyTable[i].lVnum);
        } else {
            sprinttype(g_pAssemblyTable[ i ].uchAssemblyType, AssemblyTypes, szAssmType, sizeof(szAssmType));
            sprintf( szBuffer, "[%5ld] %s (%s)\r\n", g_pAssemblyTable[ i ].lVnum,
                     obj_proto[ lRnum ].short_description, szAssmType );
            pCharacter->Send( szBuffer);

            for( j = 0; j < g_pAssemblyTable[ i ].lNumComponents; j++ ) {
                if( (lRnum = real_object( g_pAssemblyTable[ i ].pComponents[ j ].lVnum )) < 0 ) {
                    pCharacter->Send( " -----: ***RESERVED***\r\n");
                    log( "SYSERR: assemblyListToChar(): Invalid component vnum #%ld in assembly for vnum #%ld.",
                         g_pAssemblyTable[ i ].pComponents[ j ].lVnum, g_pAssemblyTable[ i ].lVnum );
                } else {
                    pCharacter->Send( " %5ld: %-20.20s Extract=%-3.3s Item in %s\r\n",          g_pAssemblyTable[ i ].pComponents[ j ].lVnum,
                                      obj_proto[ lRnum ].short_description,
                                      (g_pAssemblyTable[ i ].pComponents[ j ].bExtract ? "Yes" : "No"),
                                      compIn(g_pAssemblyTable[ i ].pComponents[ j ].bInRoom) );
                }
            }
        }
    }
}

bool assemblyAddComponent( long lVnum, long lComponentVnum, bool bExtract, int bInRoom ) {
    ASSEMBLY     *pAssembly = NULL;
    COMPONENT    *pNewComponents = NULL;

    if( (pAssembly = assemblyGetAssemblyPtr( lVnum )) == NULL ) {
        log( "SYSERR: assemblyAddComponent(): Invalid 'lVnum' #%ld.", lVnum );
        return (FALSE);
    }
    #if CIRCLE_UNSIGNED_INDEX
    else if( real_object( lComponentVnum ) == NOTHING)
        #else

        else if( real_object( lComponentVnum ) <= NOTHING )
            #endif

            {
                log( "SYSERR: assemblyAddComponent(): Invalid 'lComponentVnum' #%ld.",
                     lComponentVnum );
                return (FALSE);
            }
            /* Removed as of 1.02.29 release */
            /* else if( assemblyHasComponent( lVnum, lComponentVnum ) )
            {
              log( "SYSERR: assemblyAddComponent(): Assembly for vnum #%ld already "
                "has component vnum #%ld.", lVnum, lComponentVnum );
              return (FALSE);
            } */

            /* Create a new component table with room for one more entry. */
            CREATE( pNewComponents, COMPONENT, pAssembly->lNumComponents + 1 );

    if( pAssembly->pComponents != NULL ) {
        /* Copy the old table over to the new. */
        memmove( pNewComponents, pAssembly->pComponents, pAssembly->lNumComponents * sizeof( COMPONENT ) );
        free( pAssembly->pComponents );
    }

    /*
     * Assign the new component table and setup the new component entry. Then
     * add increment the number of components.
     */

    pAssembly->pComponents = pNewComponents;
    pAssembly->pComponents[ pAssembly->lNumComponents ].lVnum = lComponentVnum;
    pAssembly->pComponents[ pAssembly->lNumComponents ].bExtract = bExtract;
    pAssembly->pComponents[ pAssembly->lNumComponents ].bInRoom = bInRoom;
    pAssembly->lNumComponents += 1;

    return (TRUE);
}

bool assemblyCheckComponents( long lVnum, Character *pCharacter , bool check_only) {
    bool         bOk = TRUE;
    long         i = 0;
    long         lRnum = 0;
    struct obj_data **ppComponentObjects = NULL;
    struct obj_data *tobj = NULL;
    ASSEMBLY     *pAssembly = NULL;

    if( pCharacter == NULL ) {
        log( "SYSERR: NULL assemblyCheckComponents(): 'pCharacter'." );
        return (FALSE);
    } else if( (pAssembly = assemblyGetAssemblyPtr( lVnum )) == NULL ) {
        log( "SYSERR: NULL assemblyCheckComponents(): Invalid 'lVnum' #%ld.", lVnum );
        return (FALSE);
    }

    if( pAssembly->pComponents == NULL )
        return (FALSE);
    else if( pAssembly->lNumComponents <= 0 )
        return (FALSE);

    CREATE( ppComponentObjects, struct obj_data*, pAssembly->lNumComponents );

    for( i = 0; i < pAssembly->lNumComponents && bOk; i++ ) {
        if( (lRnum = real_object( pAssembly->pComponents[ i ].lVnum )) < 0 )
            bOk = FALSE;
        else {
            switch ( pAssembly->pComponents[ i ].bInRoom ) {
            case 1:
                if( (ppComponentObjects[ i ] = get_obj_in_list_num( lRnum, IN_ROOM( pCharacter )->contents )) == NULL ) {
                new_mudlog(CMP, LVL_GOD, TRUE, "ASSEMBLY[%ld]: %s room doesn't contain needed item (%ld)",lVnum, GET_NAME(pCharacter), pAssembly->pComponents[ i ].lVnum);
                    bOk = FALSE;
                } else
                    obj_from_room( ppComponentObjects[ i ] );
                break;
            case 0: /*inventory*/
                if ( (ppComponentObjects[ i ] = get_obj_in_list_num( lRnum,
                                                pCharacter->carrying )) == NULL ) {
                new_mudlog(CMP, LVL_GOD, TRUE, "ASSEMBLY[%ld]: %s isn't carrying needed item (%ld)",lVnum, GET_NAME(pCharacter), pAssembly->pComponents[ i ].lVnum);
                    bOk = FALSE;
                } else
                    obj_from_char( ppComponentObjects[ i ] );
                break;
            case 2: /* HOLD */
                tobj = GET_EQ(pCharacter, WEAR_HOLD);
                if ( (ppComponentObjects[ i ] = (tobj && GET_OBJ_RNUM(tobj) == lRnum) ? tobj : NULL ) == NULL) {
                    new_mudlog(CMP, LVL_GOD, TRUE, "ASSEMBLY[%ld]: %s isn't holding needed item (%ld)",lVnum, GET_NAME(pCharacter), pAssembly->pComponents[ i ].lVnum);
                    bOk = FALSE;
                } else
                    ppComponentObjects[ i ] = unequip_char(pCharacter, WEAR_HOLD);
                break;
            case 3: /*wield*/
                tobj = GET_EQ(pCharacter, WEAR_WIELD);
                if ( (ppComponentObjects[ i ] = (tobj && GET_OBJ_RNUM(tobj) == lRnum) ? tobj : NULL ) == NULL ) {
                new_mudlog(CMP, LVL_GOD, TRUE, "ASSEMBLY[%ld]: %s isn't wielding needed item (%ld)",lVnum, GET_NAME(pCharacter), pAssembly->pComponents[ i ].lVnum);
                    bOk = FALSE;
                } else
                    ppComponentObjects[ i ] = unequip_char(pCharacter, WEAR_WIELD);
                break;
            }
        }
    }


    for( i = 0; i < pAssembly->lNumComponents; i++ ) {
        if( ppComponentObjects[ i ] == NULL )
            continue;

        if( pAssembly->pComponents[ i ].bExtract && bOk && !check_only)
            extract_obj( ppComponentObjects[ i ] );
        else {
            switch ( pAssembly->pComponents[ i ].bInRoom ) {
            case 1:
                obj_to_room( ppComponentObjects[ i ], IN_ROOM( pCharacter ) );
                break;
            case 0:
                obj_to_char( ppComponentObjects[ i ], pCharacter );
                break;
            case 2:
                equip_char( pCharacter,ppComponentObjects[ i ], WEAR_HOLD);
                break;
            case 3:
                equip_char( pCharacter, ppComponentObjects[ i ],WEAR_WIELD);
                break;
            }
        }
    }

    free( ppComponentObjects );

    return (bOk);
}

bool assemblyCreate( long lVnum, int iAssembledType ) {
    long         lBottom = 0;
    long         lMiddle = 0;
    long         lTop = 0;
    ASSEMBLY     *pNewAssemblyTable = NULL;

    if( lVnum < 0 )
        return (FALSE);
    else if( iAssembledType < 0 || iAssembledType >= MAX_ASSM )
        return (FALSE);

    if( g_pAssemblyTable == NULL ) {
        CREATE( g_pAssemblyTable, ASSEMBLY, 1 );
        g_lNumAssemblies = 1;
    } else {
        lTop = g_lNumAssemblies - 1;

        for( ;; ) {
            lMiddle = (lBottom + lTop) / 2;

            if( g_pAssemblyTable[ lMiddle ].lVnum == lVnum )
                return (FALSE);
            else if( lBottom >= lTop )
                break;
            else if( g_pAssemblyTable[ lMiddle ].lVnum > lVnum )
                lTop = lMiddle - 1;
            else
                lBottom = lMiddle + 1;
        }

        if( g_pAssemblyTable[ lMiddle ].lVnum <= lVnum )
            lMiddle += 1;

        CREATE( pNewAssemblyTable, ASSEMBLY, g_lNumAssemblies + 1 );

        if( lMiddle > 0 )
            memmove( pNewAssemblyTable, g_pAssemblyTable, lMiddle * sizeof( ASSEMBLY ) );

        if( lMiddle <= g_lNumAssemblies - 1 )
            memmove( pNewAssemblyTable + lMiddle + 1, g_pAssemblyTable + lMiddle, (g_lNumAssemblies - lMiddle) * sizeof( ASSEMBLY ) );

        free( g_pAssemblyTable );
        g_pAssemblyTable = pNewAssemblyTable;
        g_lNumAssemblies += 1;
    }

    g_pAssemblyTable[ lMiddle ].lNumComponents = 0;
    g_pAssemblyTable[ lMiddle ].lVnum = lVnum;
    g_pAssemblyTable[ lMiddle ].pComponents = NULL;
    g_pAssemblyTable[ lMiddle ].uchAssemblyType = (unsigned char) iAssembledType;
    return (TRUE);
}

bool assemblyDestroy( long lVnum ) {
    long         lIndex = 0;
    ASSEMBLY     *pNewAssemblyTable = NULL;

    /* Find the real number of the assembled vnum. */
    if( g_pAssemblyTable == NULL || (lIndex = assemblyGetAssemblyIndex( lVnum ))
            < 0 ) {
        log( "SYSERR: assemblyDestroy(): Invalid 'lVnum' #%ld.", lVnum );
        return (FALSE);
    }

    /* Deallocate component array. */
    if( g_pAssemblyTable[ lIndex ].pComponents != NULL )
        free( g_pAssemblyTable[ lIndex ].pComponents );

    if( g_lNumAssemblies > 1 ) {
        /* Create a new table, the same size as the old one less one item. */
        CREATE( pNewAssemblyTable, ASSEMBLY, g_lNumAssemblies - 1 );

        /* Copy all assemblies before the one removed into the new table. */
        if( lIndex > 0 )
            memmove( pNewAssemblyTable, g_pAssemblyTable, lIndex * sizeof( ASSEMBLY ) );

        /* Copy all assemblies after the one removed into the new table. */
        if( lIndex < g_lNumAssemblies - 1 ) {
            memmove( pNewAssemblyTable + lIndex, g_pAssemblyTable + lIndex + 1, (g_lNumAssemblies - lIndex - 1) *
                     sizeof( ASSEMBLY ) );
        }
    }

    /* Deallocate the old table. */
    free( g_pAssemblyTable );

    /* Decrement the assembly count and assign the new table. */
    g_lNumAssemblies -= 1;
    g_pAssemblyTable = pNewAssemblyTable;

    return (TRUE);
}

bool assemblyHasComponent( long lVnum, long lComponentVnum ) {
    ASSEMBLY     *pAssembly = NULL;

    if( (pAssembly = assemblyGetAssemblyPtr( lVnum )) == NULL ) {
        log( "SYSERR: assemblyHasComponent(): Invalid 'lVnum' #%ld.", lVnum );
        return (FALSE);
    }

    return (assemblyGetComponentIndex( pAssembly, lComponentVnum ) >= 0);
}

bool assemblyRemoveComponent( long lVnum, long lComponentVnum ) {
    long         lIndex = 0;
    ASSEMBLY     *pAssembly = NULL;
    COMPONENT    *pNewComponents = NULL;

    if( (pAssembly = assemblyGetAssemblyPtr( lVnum )) == NULL ) {
        log( "SYSERR: assemblyRemoveComponent(): Invalid 'lVnum' #%ld.", lVnum );
        return (FALSE);
    } else if( (lIndex = assemblyGetComponentIndex( pAssembly, lComponentVnum )) <
               0 ) {
        log( "SYSERR: assemblyRemoveComponent(): Vnum #%ld is not a "
             "component of assembled vnum #%ld.", lComponentVnum, lVnum );
        return (FALSE);
    }

    if( pAssembly->pComponents != NULL && pAssembly->lNumComponents > 1 ) {
        CREATE( pNewComponents, COMPONENT, pAssembly->lNumComponents - 1 );

        if( lIndex > 0 )
            memmove( pNewComponents, pAssembly->pComponents, lIndex * sizeof( COMPONENT ) );

        if( lIndex < pAssembly->lNumComponents - 1 ) {
            memmove( pNewComponents + lIndex, pAssembly->pComponents + lIndex + 1,
                     (pAssembly->lNumComponents - lIndex - 1) * sizeof( COMPONENT ) );
        }
    }

    free( pAssembly->pComponents );
    pAssembly->pComponents = pNewComponents;
    pAssembly->lNumComponents -= 1;

    return (TRUE);
}

int assemblyGetType( long lVnum ) {
    ASSEMBLY     *pAssembly = NULL;

    if( (pAssembly = assemblyGetAssemblyPtr( lVnum )) == NULL ) {
        log( "SYSERR: assemblyGetType(): Invalid 'lVnum' #%ld.", lVnum );
        return (-1);
    }

    return ((int) pAssembly->uchAssemblyType);
}

int assemblyGetTypeByName( char * tname ) {
    if (!tname || !*tname)
        return -1;

    switch (*tname) {
    case 'a':
        if (!strcasecmp("assemble", tname))
            return ASSM_ASSEMBLE;
        else
            return -1;
        break;
    case 'b':
        if (!strcasecmp("brew", tname))
            return ASSM_BREW;
        else if (!strcasecmp("bake", tname))
            return ASSM_BAKE;
        else
            return -1;
        break;
    case 'c':
        if (!strcasecmp("craft", tname))
            return ASSM_CRAFT;
        else
            return -1;
        break;
    case 'f':
        if (!strcasecmp("fletch", tname))
            return ASSM_FLETCH;
        else if (!strcasecmp("forge", tname))
            return ASSM_FORGE;
        else
            return -1;
        break;
    case 'k':
        if (!strcasecmp("knit", tname))
            return ASSM_KNIT;
        else
            return -1;
        break;
    case 'm':
        if (!strcasecmp("make", tname))
            return ASSM_MAKE;
        else if (!strcasecmp("mix", tname))
            return ASSM_MIX;
        else
            return -1;
        break;
    case 't':
        if (!strcasecmp("thatch", tname))
            return ASSM_THATCH;
        else
            return -1;
        break;
    case 'w':
        if (!strcasecmp("weave", tname))
            return ASSM_WEAVE;
        else
            return -1;
        break;
    }
    return -1;
}

long assemblyCountComponents( long lVnum ) {
    ASSEMBLY     *pAssembly = NULL;

    if( (pAssembly = assemblyGetAssemblyPtr( lVnum )) == NULL ) {
        log( "SYSERR: assemblyCountComponents(): Invalid 'lVnum' #%ld.", lVnum );
        return (0);
    }

    return (pAssembly->lNumComponents);
}

long assemblyFindAssembly( const char *pszAssemblyName ) {
    long         i = 0;
    long         lRnum = NOTHING;

    if( g_pAssemblyTable == NULL )
        return (-1);
    else if( pszAssemblyName == NULL || *pszAssemblyName == '\0' )
        return (-1);
    if (is_number(pszAssemblyName)) {
        long vn = atol(pszAssemblyName);
        for( i = 0; i < g_lNumAssemblies; i++ )
            if (vn == g_pAssemblyTable[ i ].lVnum)
                return (g_pAssemblyTable[ i ].lVnum);

    } else {
        for( i = 0; i < g_lNumAssemblies; i++ ) {
            if( (lRnum = real_object( g_pAssemblyTable[ i ].lVnum )) < 0 )
                log( "SYSERR: assemblyFindAssembly(): Invalid vnum #%ld in assembly table.", g_pAssemblyTable[i].lVnum );
            else if( isname_full( pszAssemblyName, obj_proto[ lRnum ].name ) )
                return (g_pAssemblyTable[ i ].lVnum);
        }
    }

    return (-1);
}

long assemblyGetAssemblyIndex( long lVnum ) {
    long         lBottom = 0;
    long         lMiddle = 0;
    long         lTop = 0;

    lTop = g_lNumAssemblies - 1;

    for( ;; ) {
        lMiddle = (lBottom + lTop) / 2;

        if( g_pAssemblyTable[ lMiddle ].lVnum == lVnum )
            return (lMiddle);
        else if( lBottom >= lTop )
            return (-1);
        else if( g_pAssemblyTable[ lMiddle ].lVnum > lVnum )
            lTop = lMiddle - 1;
        else
            lBottom = lMiddle + 1;
    }
}


long assemblyGetComponentIndex( ASSEMBLY *pAssembly, long lComponentVnum ) {
    long         i = 0;

    if( pAssembly == NULL )
        return (-1);

    for( i = 0; i < pAssembly->lNumComponents; i++ ) {
        if( pAssembly->pComponents[ i ].lVnum == lComponentVnum )
            return (i);
    }

    return (-1);
}

ASSEMBLY* assemblyGetAssemblyPtr( long lVnum ) {
    long         lIndex = 0;

    if( g_pAssemblyTable == NULL )
        return (NULL);

    if( (lIndex = assemblyGetAssemblyIndex( lVnum )) >= 0 )
        return (g_pAssemblyTable + lIndex);

    return (NULL);
}

void free_assemblies(void) {
    int i;
    if( g_pAssemblyTable == NULL )
        return;
    for( i = 0; i < g_lNumAssemblies; i++ ) {
        //for (j = 0; j < g_pAssemblyTable[i].lNumComponents;j++)
        if (g_pAssemblyTable[i].pComponents)
            free( g_pAssemblyTable[i].pComponents );

        g_pAssemblyTable[i].lNumComponents = 0;
    }

    free(g_pAssemblyTable);
}

#undef __ASSEMBLIES_C__

