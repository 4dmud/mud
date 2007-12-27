//
// C++ Implementation: strutil
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//

#include "conf.h"
#include "sysdep.h"
#include "strutil.h"

// get rid of leading and trailing spaces from a string
string Trim (const string & s) {
    return Trim(s, SPACES);
}

string Trim (const string & s, const string & t) {
    string d = s;
    string::size_type i = d.find_last_not_of (t);
    if (i == string::npos)
        return "";
    else
        return d.erase (i + 1).erase (0, s.find_first_not_of (t)) ;
}

// returns a lower case version of the string
string tolower (const string & s) {
    string d = s;
    transform (d.begin (), d.end (), d.begin (), (int(*)(int)) tolower);
    return d;
}  // end of tolower

// transformation function for tocapitals that has a "state"
// so it can capitalise a sequence
class fCapitals : public unary_function<unsigned char,unsigned char> {
    bool bUpper;

public:

    // first letter in string will be in capitals
    fCapitals () : bUpper (true) {}
    ; // constructor

    unsigned char operator() (const unsigned char & c) {
        unsigned char c1;
        // capitalise depending on previous letter
        if (bUpper)
            c1 = toupper (c);
        else
            c1 = tolower (c);

        // work out whether next letter should be capitals
        bUpper = isalnum (c) == 0;
        return c1;
    }
}
;  // end of class fCapitals

// returns a capitalized version of the string
string tocapitals (const string & s) {
    string d = s;
    transform (d.begin (), d.end (), d.begin (), fCapitals ());
    return d;
}  // end of tocapitals


// string find-and-replace
string FindAndReplace
(const string& source, const string target, const string replacement) {
    string str = source;
    string::size_type pos = 0,   // where we are now
                            found;     // where the found data is
    if (target.size () > 0)   // searching for nothing will cause a loop
        while ((found = str.find (target, pos)) != string::npos) {
            str.replace (found, target.size (), replacement);
            pos = found + replacement.size ();
        }
    return str;
}   // end of FindAndReplace

char *ChToLower(char *c) {
	if (!c)
		return c;
	else {
	int n = strlen(c);
	for (int i = 0;i < n;i++)
	if( 'A'<=c[i] && c[i]<='Z' )
		c[i] = c[i]+'a'-'A';
	}
	return c;
}
