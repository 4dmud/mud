// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_TESTEr_
#define DLIB_TESTEr_

#include <iostream>
#include <string>
#include <dlib/map.h>
#include <dlib/logger.h>
#include <dlib/assert.h>
#include <dlib/algs.h>
#include <typeinfo>

namespace test
{
    class tester;
    typedef dlib::map<std::string,tester*>::kernel_1a_c map_of_testers;

    map_of_testers& testers (
    );

// -----------------------------------------------------------------------------

    void print_spinner (
    );
    /*!
        ensures
            - reprints the spinner
    !*/

// -----------------------------------------------------------------------------

    class tester
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a generic regression test.
        !*/

    public:

        tester (
            const std::string& switch_name,
            const std::string& description_,
            unsigned long num_of_args_ = 0
        );
        /*!
            requires
                - testers().is_in_domain(switch_name) == false
            ensures
                - #cmd_line_switch() == switch_name
                - #description() == description_
                - #num_of_args() == num_of_args_
                - adds this tester to the testers() map.
        !*/

        virtual ~tester (
        ){}

        const std::string& cmd_line_switch (
        ) const;
        /*!
            ensures
                - returns the name of the command line switch for this tester.
        !*/

        const std::string& description (
        ) const;
        /*!
            ensures
                - returns the description of what this tester tests.
        !*/

        unsigned long num_of_args (
        ) const;
        /*!
            ensures
                - returns the number of arguments this test expects
        !*/

        virtual bool perform_test (
        );
        /*!
            ensures
                - if (num_of_args() == 0) then
                    - performs the test
                    - if (the test passed) then
                        - returns true
                    - else
                        - returns false
                - else
                    - returns true
        !*/

        virtual bool perform_test (
            const std::string& arg 
        );
        /*!
            ensures
                - if (num_of_args() == 1) then
                    - performs the test
                    - if (the test passed) then
                        - returns true
                    - else
                        - returns false
                - else
                    - returns true
        !*/

        virtual bool perform_test (
            const std::string& arg1, 
            const std::string& arg2 
        );
        /*!
            ensures
                - if (num_of_args() == 2) then
                    - performs the test
                    - if (the test passed) then
                        - returns true
                    - else
                        - returns false
                - else
                    - returns true
        !*/

    private:

    // ---------------------------------------------------------------------------
    //             Implementation Details
    // ---------------------------------------------------------------------------

        /*!
            CONVENTION
                - switch_name == cmd_line_switch()
                - description_ == description()
                - num_of_args_ == num_of_args()
                - test::tester[switch_name] == this
        !*/

        const std::string switch_name;
        const std::string description_;
        const unsigned long num_of_args_;
    };

}

#endif // DLIB_TESTEr_

