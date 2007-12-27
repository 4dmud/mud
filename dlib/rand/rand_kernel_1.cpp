// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_RAND_KERNEL_1_CPp_
#define DLIB_RAND_KERNEL_1_CPp_
#include "../md5.h"
#include "rand_kernel_1.h"
#include <cstring>

namespace dlib
{

// ----------------------------------------------------------------------------------------

    void rand_kernel_1::
    clear(
    )  
    {
        seed.clear();
        current_byte = 16;
        md5 ( 
            reinterpret_cast<const unsigned char*>(seed.data()),
            static_cast<unsigned long>(seed.size()),
            bytes_and_seed+16 
            );
        memset(bytes,0,sizeof(bytes));
    }

// ----------------------------------------------------------------------------------------

    const std::string& rand_kernel_1::
    get_seed (
    )
    {
        return seed;
    }

// ----------------------------------------------------------------------------------------

    void rand_kernel_1::
    set_seed (
        const std::string& value
    )
    {
        seed = value;

        // make the algorithm restart using the new seed
        current_byte    = 16;

        md5 ( 
            reinterpret_cast<const unsigned char*>(seed.data()),
            static_cast<unsigned long>(seed.size()),
            bytes_and_seed+16 
            );
        memset(bytes,0,sizeof(bytes));
    }

// ----------------------------------------------------------------------------------------

    unsigned char rand_kernel_1::
    get_random_number (
    )
    {

        // if we need to refill the bytes array then do so
        if (current_byte == sizeof(bytes))
        {
            // copy bytes into bytes_and_seed
            memcpy(bytes_and_seed,bytes,sizeof(bytes));

            // fill the bytes array
            md5 (bytes_and_seed,sizeof(bytes_and_seed),bytes); 
            
            current_byte = 0;
        }

        return bytes[current_byte++];
    }

// ----------------------------------------------------------------------------------------

    void rand_kernel_1::
    swap (
        rand_kernel_1& item
    )
    {
        exchange(item.seed,seed);
        exchange(item.current_byte,current_byte);


        unsigned char temp;
        for (size_t i = 0; i < sizeof(bytes); ++i)
        {
            temp = bytes[i];
            bytes[i] = item.bytes[i];
            item.bytes[i] = temp;
        }

        for (size_t i = 0; i < sizeof(bytes_and_seed); ++i)
        {
            temp = bytes_and_seed[i];
            bytes_and_seed[i] = item.bytes_and_seed[i];
            item.bytes_and_seed[i] = temp;
        }
    }
    
// ----------------------------------------------------------------------------------------



}
#endif // DLIB_RAND_KERNEL_1_CPp_

