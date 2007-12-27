// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_RAND_KERNEl_1_
#define DLIB_RAND_KERNEl_1_

#include <string>
#include "../algs.h"
#include "rand_kernel_abstract.h"

namespace dlib
{


    class rand_kernel_1
    {

        /*!       
            INITIAL VALUE
                seed            == ""
                bytes           == an array of 16 unsigned chars and they are all set to 0
                current_byte    == 16
                bytes_and_seed[16..31] == md5(seed) 

            CONVENTION
                if (current_byte == 16)
                    then we need to refill bytes and set current_byte back to 0
                else
                    bytes[current_byte] == get_random_number().  
                                           i.e. the next random number


                seed            == get_seed()
                bytes_and_seed[16..31] == md5(seed) 
        !*/
        
        public:


            rand_kernel_1(
            ) 
            {clear();}

            virtual ~rand_kernel_1(
            )
            {}

            void clear(
            );

 
            const std::string& get_seed (
            );

            void set_seed (
                const std::string& value
            );

            unsigned char get_random_number (
            );

            void swap (
                rand_kernel_1& item
            );
    

        private:

            std::string seed;
            unsigned char bytes[16];
            int current_byte;
            unsigned char bytes_and_seed[32];


            // restricted functions
            rand_kernel_1(rand_kernel_1&);        // copy constructor
            rand_kernel_1& operator=(rand_kernel_1&);    // assignment operator

    };


    inline void swap (
        rand_kernel_1& a, 
        rand_kernel_1& b 
    ) { a.swap(b); }   


}

#ifdef NO_MAKEFILE
#include "rand_kernel_1.cpp"
#endif

#endif // DLIB_RAND_KERNEl_1_

