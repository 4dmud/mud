// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <ctime>
#include <cstdlib>

#include <dlib/conditioning_class.h>

#include "tester.h"
#include "conditioning_class.h"

namespace  
{


    class conditioning_class_tester : public tester
    {
    public:
        conditioning_class_tester (
        ) :
            tester ("test_conditioning_class",
                    "Runs tests on the conditioning_class component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += conditioning_class_kernel_test<
                conditioning_class<256>::kernel_1a,
                conditioning_class<2>::kernel_1a
                >    ("kernel_1a");
            print_spinner();

            n += conditioning_class_kernel_test<
                conditioning_class<256>::kernel_2a,
                conditioning_class<2>::kernel_2a
                >    ("kernel_2a");
            print_spinner();

            n += conditioning_class_kernel_test<
                conditioning_class<256>::kernel_3a,
                conditioning_class<2>::kernel_3a
                >    ("kernel_3a");
            print_spinner();

            n += conditioning_class_kernel_test<
                conditioning_class<256>::kernel_4a,
                conditioning_class<2>::kernel_4a
                >    ("kernel_4a");
            print_spinner();

            n += conditioning_class_kernel_test<
                conditioning_class<256>::kernel_4b,
                conditioning_class<2>::kernel_4b
                >    ("kernel_4b");
            print_spinner();

            n += conditioning_class_kernel_test<
                conditioning_class<256>::kernel_4c,
                conditioning_class<2>::kernel_4c
                >    ("kernel_4c");
            print_spinner();

            n += conditioning_class_kernel_test<
                conditioning_class<256>::kernel_4d,
                conditioning_class<2>::kernel_4d
                >    ("kernel_4d");
            print_spinner();

            return (n == 0);

        }
    } a;


}

