// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_RAND_KERNEL_2_CPp_
#define DLIB_RAND_KERNEL_2_CPp_
#include "../md5.h"
#include "rand_kernel_2.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    void rand_kernel_2::
    clear(
    )
    {
        seed.clear();
        reseed();
    }

// ----------------------------------------------------------------------------------------

    void rand_kernel_2::
    reseed (
    )
    {
        using namespace std;
        string::size_type i;
        seed1 = 1;
        seed2 = 1;
        for (i = 0; i < seed.size(); ++i)
        {
            if ((i&0x1) == 0)
            {
                seed1 += static_cast<int32>(i)*seed[i];
            }
            else
            {
                seed2 += static_cast<int32>(i)*seed[i];
            }
        }
    }

// ----------------------------------------------------------------------------------------

    const std::string& rand_kernel_2::
    get_seed (
    )
    {
        return seed;
    }

// ----------------------------------------------------------------------------------------

    void rand_kernel_2::
    set_seed (
        const std::string& value
    )
    {
        seed = value;
        reseed();
    }

// ----------------------------------------------------------------------------------------


    unsigned char rand_kernel_2::
    get_random_number (
    )
    {
        int32 z;
        modult(53668, 40014, 12211, 2147483563L, seed1);
        modult(52774, 40692, 3791,  2147483399L, seed1);
        z = (seed1 - seed2)&0xFFFFFFFF;
        if (z < 1)
            z += 2147483562;
        return static_cast<unsigned char>(z / 8421504);
    }

// ----------------------------------------------------------------------------------------

    void rand_kernel_2::
    swap (
        rand_kernel_2& item
    )
    {
        exchange(item.seed,seed);
        exchange(item.seed1,seed1);
        exchange(item.seed2,seed2);

    }
    
// ----------------------------------------------------------------------------------------



}
#endif // DLIB_RAND_KERNEL_2_CPp_

