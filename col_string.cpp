/**
 **  File ......... cstring.cpp
 **  Published ....  2004-05-16
 **  Author ....... grymse@alhem.net
**/
/*
Copyright (C) 2004  Anders Hedstrom
 
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
 
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/
#undef cstring
#include "conf.h"
#include "sysdep.h"
#include "color.h"
#include "col_string.h"


// vs won't compile when I write out 'std::string' everywhere - blah


cstring::cstring()
        :string()
        ,m_cstr(NULL)
,m_ucstr(NULL) {
}


cstring::cstring(const std::string& p)
        :string(p)
        ,m_cstr(NULL)
,m_ucstr(NULL) {
}


cstring::~cstring() {
    if (m_cstr)
        delete[] m_cstr;
    if (m_ucstr)
        delete[] m_ucstr;

        
}


string cstring::c_str() {
    size_t n = size();
    string tmp;
    int incol = 0;

    for (size_t i = 0; i < n; i++) {
        if ((*this)[i] == '{') {
            i++;
            if (incol == 0) {
                switch ((*this)[i]) {
                case 'c':
                case 'C':
                    incol = 1;
                    break;
                case 'b':
                case 'B':
                    incol = 2;
                    break;
                }
                
                if (incol == 0)
                    tmp += (*this)[i];

                continue;
            }
            }

            switch (incol) {
            case 1:
                switch ((*this)[i]) {

                case 'x':
                case '0':
                case 'n':
                    tmp += CNRM;
                    break;
                case 'r':
                    tmp += CRED;
                    break;
                case 'g':
                    tmp += CGRN;
                    break;
                case 'y':
                    tmp += CYEL;
                    break;
                case 'b':
                    tmp += CBLU;
                    break;
                case 'm':
                    tmp += CMAG;
                    break;
                case 'c':
                    tmp += CCYN;
                    break;
                case 'w':
                    tmp += CWHT;
                    break;
                case 'l':
                    tmp += CBLK;
                    break;
                case 'R':
                    tmp += BRED;
                    break;
                case 'G':
                    tmp += BGRN;
                    break;
                case 'Y':
                    tmp += BYEL;
                    break;
                case 'B':
                    tmp += BBLU;
                    break;
                case 'M':
                    tmp += BMAG;
                    break;
                case 'C':
                    tmp += BCYN;
                    break;
                case 'W':
                    tmp += BWHT;
                    break;
                case 'L':
                    tmp += BBLK;
                    break;
                case 'u':
                case 'U':
                    tmp += UNDER;
                    break;
                case 'f':
                case 'F':
                    tmp += FLASH;
                    break;
                default:
                    tmp += (*this)[i];
                    break;
                }
                break;
            case 2:
                switch ((*this)[i]) {

                case 'r':
                    tmp += BKRED;
                    break;
                case 'g':
                    tmp += BKGRN;
                    break;
                case 'y':
                    tmp += BKYEL;
                    break;
                case 'b':
                    tmp += BKBLU;
                    break;
                case 'm':
                    tmp += BKMAG;
                    break;
                case 'c':
                    tmp += BKCYN;
                    break;
                case 'w':
                    tmp += BKWHT;
                    break;
                case 'l':
                    tmp += BKBLK;
                    break;
                case 'u':
                case 'U':
                    tmp += UNDER;
                    break;
                case 'f':
                case 'F':
                    tmp += FLASH;
                    break;
                default:
                    tmp += (*this)[i];
                    break;
                }
                break;

            default:
                tmp += (*this)[i];
                break;
            }
            incol = 0;
        /*} else {
            tmp += (*this)[i];
        }*/
    }
   /* if (m_cstr)
        delete[] m_cstr;
    m_cstr = new char[tmp.size() + 1];
    memcpy(m_cstr,tmp.c_str(),tmp.size());
    m_cstr[tmp.size()] = 0;

    return m_cstr;
    */
    return tmp;
}


const char *cstring::uc_str() {
    size_t n = size();
    std::string tmp;
    int incol = 0;

    for (size_t i = 0; i < n; i++) {
        if ((*this)[i] == '{') {
            i++;
            if (incol == 0) {
                switch ((*this)[i]) {
                case 'c':
                case 'C':
                    incol = 1;
                    break;
                case 'b':
                case 'B':
                    incol = 2;
                    break;
                }
                if (incol == 0)
                    tmp += (*this)[i];

                continue;
            }

            switch (incol) {
            case 1:
                switch ((*this)[i]) {
                case 'x':
                case '0':
                case 'n':
                    break;
                case 'r':
                    break;
                case 'g':
                    break;
                case 'y':
                    break;
                case 'b':
                    break;
                case 'm':
                    break;
                case 'c':
                    break;
                case 'w':
                    break;
                case 'l':
                    break;
                case 'R':
                    break;
                case 'G':
                    break;
                case 'Y':
                    break;
                case 'B':
                    break;
                case 'M':
                    break;
                case 'C':
                    break;
                case 'W':
                    break;
                case 'L':
                    break;
                case 'u':
                case 'U':
                    break;
                case 'f':
                case 'F':
                    break;
                default:
                    tmp += (*this)[i];
                    break;
                }
                break;
            case 2:
                switch ((*this)[i]) {

                case 'r':
                    break;
                case 'g':
                    break;
                case 'y':
                    break;
                case 'b':
                    break;
                case 'm':
                    break;
                case 'c':
                    break;
                case 'w':
                    break;
                case 'l':
                    break;
                case 'u':
                case 'U':
                    break;
                case 'f':
                case 'F':
                    break;
                default:
                    tmp += (*this)[i];
                    break;
                }
                break;

            default:
                tmp += (*this)[i];
                break;
            }
            incol = 0;
        } else {
            tmp += (*this)[i];
        }
    }
    if (m_cstr)
        delete m_cstr;
    m_cstr = new char[tmp.size() + 1];
    memcpy(m_cstr,tmp.c_str(),tmp.size());
    m_cstr[tmp.size()] = 0;

    return m_cstr;
}


bool cstring::operator==(const std::string& str) {
    return !strcmp(uc_str(),str.c_str());
}


bool cstring::operator==(cstring& str) {
    return !strcmp(uc_str(),str.uc_str());
}


bool cstring::operator==(const char *p) {
    return !strcmp(uc_str(),p);
}


//const cstring&
void cstring::operator=(const char *p) {
    //   return
    string::operator=(p);
}


void cstring::operator+=(const char *p) {
    string::operator+=(p);
}


void cstring::operator+=(const std::string& p) {
    string::operator+=(p);
}


