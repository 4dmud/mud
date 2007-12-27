// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_RANd_
#define DLIB_RANd_

#include "rand/rand_kernel_1.h"
#include "rand/rand_kernel_2.h"

#include "algs.h"



namespace dlib
{


    class rand
    {
        rand() {}

    public:
        
        //----------- kernels ---------------

        // kernel_1a        
        typedef     rand_kernel_1  
                    kernel_1a;
 
        // kernel_2a        
        typedef     rand_kernel_2  
                    kernel_2a;
 
    };

}

#endif // DLIB_RANd_

