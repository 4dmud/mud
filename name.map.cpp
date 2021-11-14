//
// C++ Implementation: name
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "name.map.h"
#include "db.h"

Character *find_char_by_uid_in_lookup_table ( long uid );

/**
The idea here is we create a kind of tree structure, that
allows object and mob names to have a smaller search distance.
**/

void NameIndexer::addNamelist ( const char * namelist, long id )
{
    char *newlist;
    char *curtok;

    if ( !namelist || id == -1 )
        return;

    /* make a copy since strtok 'modifies' strings */
    strlcpy ( newlistbuf, namelist, sizeof ( newlistbuf ) );
    newlist = newlistbuf;
    for ( curtok = strsep ( &newlist, WHITESPACE ); curtok; curtok = strsep ( &newlist, WHITESPACE ) )
        nlt.insert ( pair<string, long> ( string ( curtok ), id ) );
}

void NameIndexer::remNamelist ( long id )
{
    if ( id < 0 || nlt.empty() )
        return;

    if ( this == &objNames )
        strlcpy ( newlistbuf, GET_OBJ_NAME ( object_list[ id ] ), sizeof ( newlistbuf ) );
    else
        strlcpy ( newlistbuf, find_char_by_uid_in_lookup_table ( id )->player.name, sizeof ( newlistbuf ) );

    char *curtok, *newlist = newlistbuf;
    if ( !*newlist )
        return;

    for ( curtok = strsep ( &newlist, WHITESPACE ); curtok; curtok = strsep ( &newlist, WHITESPACE ) )
    {
        auto range = nlt.equal_range ( string ( curtok ) );
        for ( auto it = range.first; it != range.second; ++it )
            if ( it->second == id )
            {
                nlt.erase ( it );
                break;
            }
    }
}

long NameIndexer::nameLookup ( const char *n )
{
    if ( nlt.empty() )
        return -1;
    nlt_it = nlt.find ( string ( n ) );
    if ( nlt_it != nlt.end() )
        return nlt_it->second;
    return -1;
}

