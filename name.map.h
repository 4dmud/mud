//
// C++ Interface: name
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef NAME_MAP_H
#define NAME_MAP_H

class NameIndexer
{
		multimap<string, long> nlt; /*name lookup table */
		multimap<string, long>::iterator nlt_it;
		char newlistbuf[MAX_INPUT_LENGTH];

	public:
		NameIndexer() {};
		~NameIndexer() {};
		void addNamelist ( const char * namelist, long id );
		void remNamelist ( long id );
		long nameLookup ( const char *n );
		void Clear() {nlt.clear();};
};
extern NameIndexer mobNames;
extern NameIndexer objNames;

#endif
