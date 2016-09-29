//
// C++ Implementation: find
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "find.h"

tPlayerList playerlist;
// returns a lower case version of the string
/*string tolower (const string & s)
  {
string d = s;
  transform (d.begin (), d.end (), d.begin (), (int(*)(int)) tolower);
  return d;
  }  // end of tolower*/
// case-independent (ci) string compare
// returns true if strings are EQUAL
struct ciEqualTo : binary_function <string, string, bool>
  {
  struct compare_equal
    : public binary_function <unsigned char, unsigned char,bool>
    {
    bool operator() (const unsigned char& c1, const unsigned char& c2) const
      { return tolower (c1) == tolower (c2); }
    };  // end of compare_equal

  bool operator() (const string & s1, const string & s2) const
    {
    pair <string::const_iterator, string::const_iterator> result =
      mismatch (s1.begin (), s1.end (), s2.begin (), compare_equal ());

    // match if both at end
    return result.first == s1.end () && result.second == s2.end ();
    }
  }; // end of ciEqualTo
// compare strings for equality using the binary function above
// returns true is s1 == s2
bool ciStringEqual (const string & s1, const string & s2)
  {
  return ciEqualTo () (s1, s2);
  }  // end of ciStringEqual
/* find a player by name */
// functor to help finding a player by name
struct findCharacterName
{
  const string name;
  // ctor
  findCharacterName (const string & n) : name ( n ) {}
  // check for player with correct name, and actually playing
  bool operator() (const Character * p) const
    {
    if (IS_NPC(p))
    return ciStringEqual (p->player.short_descr, name);
    else
    return ciStringEqual (p->player.name, name);
    } // end of operator()
};  // end of findPlayerName

Character * FindPlayer (const string & name)
{
  tPlayerListIterator i =
    find_if (playerlist.begin (), playerlist.end (), findCharacterName (name));

  if (i == playerlist.end ())
    return NULL;
  else
    return *i;

} /* end of FindPlayer */
