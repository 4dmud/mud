// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/reference_counter.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.reference_counter");

    template <
        typename ref_counter 
        >
    int reference_counter_test (
        const string& type
    )
    /*!
        requires
            - ref_counter is an implementation of reference_counter/reference_counter_kernel_abstract.h 
              and is instantiated to contain an int 
        ensures
            - runs tests on reference_counter for compliance with the specs 
            - returns 0 if no errors were detected, 1 otherwise.
    !*/
    {        

        try 
        {
            ref_counter a, b, c;
 
            for (long i = 0; i < 10; ++i)
            {
                print_spinner();
                for (long j = 0; j < 10000; ++j)
                {
                    a.modify() = j;
                    b.modify() = j+1;
                    c.modify() = j+2;
                    ASSERT(a.access() == j,"");
                    ASSERT(b.access() == j+1,"");
                    ASSERT(c.access() == j+2,"");
                    ASSERT(a.modify() == j,"");
                    ASSERT(b.modify() == j+1,"");
                    ASSERT(c.modify() == j+2,"");
                    ASSERT(a.access() == j,"");
                    ASSERT(b.access() == j+1,"");
                    ASSERT(c.access() == j+2,"");
                    ASSERT(a.modify() == j,"");
                    ASSERT(b.modify() == j+1,"");
                    ASSERT(c.modify() == j+2,"");
                    a = c;
                    ASSERT(a.access() == j+2,"");
                    ASSERT(b.access() == j+1,"");
                    ASSERT(c.access() == j+2,"");
                    ASSERT(a.modify() == j+2,"");
                    ASSERT(b.modify() == j+1,"");
                    ASSERT(c.modify() == j+2,"");
                    ASSERT(a.access() == j+2,"");
                    ASSERT(b.access() == j+1,"");
                    ASSERT(c.access() == j+2,"");
                    ASSERT(a.modify() == j+2,"");
                    ASSERT(b.modify() == j+1,"");
                    ASSERT(c.modify() == j+2,"");

                    a = b = c;
                    ASSERT(a.access() == b.access(),"");
                    ASSERT(a.access() == c.access(),"");
                    ASSERT(c.access() == b.access(),"");
                    a.modify() = j;
                    ASSERT(a.access() == j,"");
                    ASSERT(a.access() != b.access(),"");
                    ASSERT(a.access() != c.access(),"");
                    ASSERT(c.access() == b.access(),"");
                    ASSERT(c.access() == j+2,"");
                    ASSERT(b.access() == j+2,"");

                    ASSERT(a.access() == j,"");
                    a = a;
                    ASSERT(a.access() == j,"");
                    c = c;
                    ASSERT(c.access() == j+2,"");
                    ASSERT(b.access() == j+2,"");
                    swap(a,c);
                    ASSERT(a.access() == j+2,"");
                    ASSERT(c.access() == j,"");
                    ASSERT(b.access() == j+2,"");
                }
            }

            return 0;
        }
        catch(error& e)
        {
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.info;
            return 1;
        }        
        catch (bad_alloc)
        {
            cout << "\n\nRAN OUT OF MEMORY in " << type << endl;
            dlog << LWARN << "RAN OUT OF MEMORY in " << type;
            return 1;
        }
    }





    class reference_counter_tester : public tester
    {
    public:
        reference_counter_tester (
        ) :
            tester ("test_reference_counter",
                    "Runs tests on the reference_counter component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += reference_counter_test<reference_counter<int>::kernel_1a>    ("kernel_1a");
            print_spinner();
            return (n == 0);
        }
    } a;

}


