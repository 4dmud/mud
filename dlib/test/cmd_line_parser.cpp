// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <string>
#include <dlib/string.h>

#include <dlib/cmd_line_parser.h>

#include "tester.h"

#include "cmd_line_parser.h"
namespace  
{

    class cmd_line_parser_tester : public tester
    {
    public:
        cmd_line_parser_tester (
        ) :
            tester ("test_cmd_line_parser_char",
                    "Runs tests on the cmd_line_parser component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += cmd_line_parser_kernel_test<cmd_line_parser<char>::kernel_1a>      ("kernel_1a with char");
            print_spinner();
            n += cmd_line_parser_kernel_test<cmd_line_parser<char>::kernel_1a_c>    ("kernel_1a_c with char");
            print_spinner();
            return (n == 0);
        }
    } a;

}


