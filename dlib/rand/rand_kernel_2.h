// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_RAND_KERNEl_2_
#define DLIB_RAND_KERNEl_2_

#include <string>
#include "../algs.h"
#include "rand_kernel_abstract.h"

namespace dlib
{


    class rand_kernel_2
    {

        /*!       
            INITIAL VALUE
                seed            == ""
                seed1           == a value derived from seed
                seed2           == a value derived from seed

            CONVENTION
                seed            == get_seed()
                seed1           == a value derived from seed
                seed2           == a value derived from seed
                  
                This is the linear congruential generator from page 371 of
                Applied Cryptography by Bruce Schneier
        !*/

        // int32 should be a 32 bit signed integer
        typedef long int32;
        
        public:


            rand_kernel_2(
            ) 
            {clear();}

            virtual ~rand_kernel_2(
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
                rand_kernel_2& item
            );
    

        private:

            void reseed (
            );
            /*!
                ensures
                    - assigns values to seed1 and seed2 based on the value of seed
            !*/


            void modult(
                int32 a,
                int32 b,
                int32 c,
                int32 m,
                int32& s
            )
            {
                int32 q = s/a;
                s = b*(s-a*q) - c*q; 
                if (s < 0)
                    s += m;
            }

            std::string seed;
            int32 seed1;
            int32 seed2;

            // restricted functions
            rand_kernel_2(rand_kernel_2&);        // copy constructor
            rand_kernel_2& operator=(rand_kernel_2&);    // assignment operator

    };


    inline void swap (
        rand_kernel_2& a, 
        rand_kernel_2& b 
    ) { a.swap(b); }   


}

#ifdef NO_MAKEFILE
#include "rand_kernel_2.cpp"
#endif

#endif // DLIB_RAND_KERNEl_2_

