// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_VECTOr_
#define DLIB_VECTOr_

#include "vector/vector_kernel_1.h"


namespace dlib
{


    template <
        typename T
        >
    class vector
    {
        vector() {}
    public:
        
        //----------- kernels ---------------

        // kernel_1a        
        typedef     vector_kernel_1<T>    
                    kernel_1a;
     

    };
}

#endif // DLIB_VECTOr_

