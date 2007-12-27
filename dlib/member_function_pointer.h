// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_MEMBER_FUNCTION_POINTEr_
#define DLIB_MEMBER_FUNCTION_POINTEr_

#include "member_function_pointer/member_function_pointer_kernel_1.h"
#include "member_function_pointer/member_function_pointer_kernel_c.h"

namespace dlib
{

    template <
        typename PARAM1 = void,
        typename PARAM2 = void,
        typename PARAM3 = void,
        typename PARAM4 = void
        >
    class member_function_pointer 
    {
        member_function_pointer() {}


    public:
        
        //----------- kernels ---------------

        // kernel_1a        
        typedef      member_function_pointer_kernel_1<PARAM1,PARAM2,PARAM3,PARAM4>
                     kernel_1a;
        typedef      member_function_pointer_kernel_c<kernel_1a>
                     kernel_1a_c;
           

    };
}

#endif // DLIB_MEMBER_FUNCTION_POINTEr_ 

