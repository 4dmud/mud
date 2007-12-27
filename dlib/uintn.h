// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#ifndef DLIB_UINtn_
#define DLIB_UINtn_

#include "assert.h"

namespace dlib
{

    /*!
        uint64 is a typedef for an unsigned integer that is exactly 64 bits wide.
        uint32 is a typedef for an unsigned integer that is exactly 32 bits wide.
        uint16 is a typedef for an unsigned integer that is exactly 16 bits wide.
    !*/


#ifdef __GNUC__
    typedef unsigned long long uint64;
#elif __BORLANDC__
    typedef unsigned __int64 uint64;
#elif _MSC_VER
    typedef unsigned __int64 uint64;
#else
    typedef unsigned long long uint64;
#endif

typedef unsigned short uint16;
typedef unsigned int   uint32;



// make sure these types have the right sizes on this platform
COMPILE_TIME_ASSERT(sizeof(uint16) == 2);
COMPILE_TIME_ASSERT(sizeof(uint32) == 4);
COMPILE_TIME_ASSERT(sizeof(uint64) == 8);

}

#endif // DLIB_UINtn_

