//
// C++ Interface: strutil
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
static const string SPACES = " \t\r\n";           // what gets removed when we trim

// get rid of leading and trailing spaces from a string
string Trim (const string & s, const string & t);
string Trim (const string & s);
string tolower (const string & s);
string FindAndReplace
  (const string& source, const string target, const string replacement);

