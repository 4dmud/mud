//
// C++ Interface: mxp
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef MXP_H
#define MXP_H

#define  TELOPT_MXP        '\x5B'
const unsigned char will_mxp_str  [] = { IAC, WILL, TELOPT_MXP, 0};
const unsigned char start_mxp_str [] = { IAC, SB,   TELOPT_MXP, IAC, SE, 0 };
const unsigned char do_mxp_str    [] = { IAC, DO,   TELOPT_MXP, 0 };
const unsigned char dont_mxp_str  [] = { IAC, DONT, TELOPT_MXP, 0 };
const unsigned char eor_offer     [] = { IAC, WILL, EOR,0};
const unsigned char ga_offer      [] = { IAC, WILL, GA,0};

#endif
