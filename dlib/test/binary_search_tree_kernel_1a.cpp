// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_BINARY_SEARCH_TREE_KERNEl_TEST_
#define DLIB_BINARY_SEARCH_TREE_KERNEl_TEST_


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/memory_manager_global.h>
#include <dlib/memory_manager_stateless.h>
#include <dlib/binary_search_tree.h>
#include "tester.h"
#include "binary_search_tree.h"

namespace  
{


    class binary_search_tree_tester : public tester
    {

    public:
        binary_search_tree_tester (
        ) :
            tester ("test_binary_search_tree_kernel_1a",
                    "Runs tests on the binary_search_tree_kernel_1a component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += binary_search_tree_kernel_test<binary_search_tree<int,int>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += binary_search_tree_kernel_test<binary_search_tree<int,int>::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

#endif
