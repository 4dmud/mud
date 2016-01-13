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
#include "name.map.h"
/**
The idea here is we create a kind of tree structure, that
allows object and mob names to have a smaller search distance.
**/

void NameIndexer::addNamelist ( const char * namelist, long id )
{
	char *newlist;
	register char *curtok;

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
	if ( nlt.empty() )
		return;
	nlt_it = nlt.begin();

	while ( nlt_it != nlt.end() )
	{
		if ( nlt_it->second == id )
			nlt_it = nlt.erase ( nlt_it );
		else
			++nlt_it;
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

