// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/queue.h>

#include "tester.h"

// This is called an unnamed-namespace and it has the effect of making everything inside this file "private"
// so that everything you declare will have static linkage.  Thus we won't have any multiply
// defined symbol errors coming out of the linker when we try to compile the test suite.
namespace  
{

    using namespace test;
    using namespace dlib;
    using namespace std;

    // Declare the logger we will use in this test.  The name of the tester 
    // should start with "test."
    logger dlog("test.queue");

    template <
        typename queue
        >
    int queue_sort_test (
        const string& type
    )
    /*!
        requires
            - queue is an implementation of queue/queue_sort_abstract.h 
              is instantiated with int
        ensures
            - runs tests on queue for compliance with the specs
            - returns 0 if there aren't any errors, 1 otherwise
    !*/
    {        

        try 
        {
            srand(static_cast<unsigned int>(time(0)));

            queue q,q2;

            enumerable<int>& e = q;

            // I will use these CASSERT macros to assert that conditions are true.  If they are
            // false then it means we have detected an error in the queue object.  CASSERT
            // will then throw an exception which we will catch at the end of this function and
            // report as an error/failed test.
            CASSERT(e.at_start() == true,"");

            int a;

            CASSERT(q.size() == 0,"");
            CASSERT(q.at_start() == true,"");
            CASSERT(q.current_element_valid() == false, "");

            q.sort();

            CASSERT(q.size() == 0,"");
            CASSERT(q.at_start() == true,"");
            CASSERT(q.current_element_valid() == false, "");

            CASSERT (q.move_next() == false,"");
            CASSERT (q.move_next() == false,"");
            CASSERT (q.move_next() == false,"");
            CASSERT (q.move_next() == false,"");
            CASSERT (q.move_next() == false,"");
            CASSERT (q.move_next() == false,"");


            CASSERT(q.size() == 0,"");
            CASSERT(q.at_start() == false,"");
            CASSERT(q.current_element_valid() == false, "");


            q.reset();

            CASSERT(q.size() == 0,"");
            CASSERT(q.at_start() == true,"");
            CASSERT(q.current_element_valid() == false, "");











            q.clear();
            q2.clear();
            CASSERT(q.size() == 0,"");
            CASSERT(q2.size() == 0,"");

            for (int i = 0; i < 10000; ++i)
            {
                int a = i;
                q.enqueue(a);
            }

            q2.cat(q);

            CASSERT(q.size() == 0,"");
            CASSERT(q2.size() == 10000,"");

            int g = 0;
            while (q2.move_next())
            {
                CASSERT(q2.element() == g,g);
                ++g;
            }

            for (int i = 0;i < 10000; ++i)
            {
                int a = 0;
                q2.dequeue(a);
                CASSERT(a == i,"");
            }

            CASSERT(q.size() == 0,"");
            CASSERT(q2.size() == 0,"");
            q.clear();
            q2.clear();















            q.clear();
            q2.clear();
            CASSERT(q.size() == 0,"");
            CASSERT(q2.size() == 0,"");

            for (int i = 0; i < 1; ++i)
            {
                int a = i;
                q.enqueue(a);
            }

            q2.cat(q);

            CASSERT(q.size() == 0,"");
            CASSERT(q2.size() == 1,"");



            g = 0;
            while (q2.move_next())
            {
                CASSERT(q2.element() == g,g);
                ++g;
            }

            for (int i = 0;i < 1; ++i)
            {
                int a = 0;
                q2.dequeue(a);
                CASSERT(a == i,"");
            }

            CASSERT(q.size() == 0,"");
            CASSERT(q2.size() == 0,"");
            q.clear();
            q2.clear();


















            for (int j = 0; j < 3; ++j)
            {
                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand();
                    q.enqueue(a);
                }

                while (q.move_next());

                CASSERT(q.at_start() == false,"");

                q.sort();

                CASSERT(q.at_start() == true,"");

                // serialize the state of q, then clear q, then
                // load the state back into q.
                ostringstream sout;
                serialize(q,sout);
                CASSERT(q.at_start() == true,"");
                istringstream sin(sout.str());
                q.clear();
                deserialize(q,sin);


                CASSERT(q.at_start() == true,"");

                a = 0;
                int last = 0;
                while (q.move_next())
                {
                    ++a;
                    CASSERT(last <= q.element(),"items weren't actually sorted");
                    last = q.element();
                    CASSERT(q.current_element_valid() == true,"");
                    CASSERT(q.at_start() == false,"");
                    CASSERT(q.current_element_valid() == true,"");

                    
                }
                CASSERT(a == 10000,"some items were lost between the sorting and iterating");


                CASSERT(q.size() == 10000,"");
                swap(q,q2);
                CASSERT(q2.at_start() == false,"");
                CASSERT(q2.current_element_valid() == false, "");

                CASSERT (q2.move_next() == false,"");
                CASSERT (q2.move_next() == false,"");
                CASSERT (q2.move_next() == false,"");
                CASSERT (q2.move_next() == false,"");
                CASSERT (q2.move_next() == false,"");
                CASSERT (q2.move_next() == false,"");


                CASSERT(q2.size() == 10000,"");
                CASSERT(q2.at_start() == false,"");
                CASSERT(q2.current_element_valid() == false, "");

                q2.clear();

                q.swap(q2);

                CASSERT(q.size() == 0,"");
                CASSERT(q.at_start() == true,"");
                CASSERT(q.current_element_valid() == false, "");
            }
            





            // try the above code but this time with just one element
            // in the queue
            for (int j = 0; j < 3; ++j)
            {
                for (int i = 0; i < 1; ++i)
                {
                    a = ::rand();
                    q.enqueue(a);
                }

                q.sort();

                a = 0;
                int last = 0;
                while (q.move_next())
                {
                    ++a;
                    CASSERT(last <= q.element(),"items weren't actually sorted");
                    CASSERT(q.current_element_valid() == true,"");
                    
                }
                CASSERT(a == 1,"some items were lost between the sorting and iterating");


                CASSERT(q.size() == 1,"");
                CASSERT(q.at_start() == false,"");
                CASSERT(q.current_element_valid() == false, "");

                q.clear();

                CASSERT(q.size() == 0,"");
                CASSERT(q.at_start() == true,"");
                CASSERT(q.current_element_valid() == false, "");
            }
            


            {
                q.clear();
                remover<int>& go = q;
                for (int i = 0; i < 100; ++i)
                {
                    int a = 3;
                    q.enqueue(a);
                }
                CASSERT(go.size() == 100,"");                
                for (int i = 0; i < 100; ++i)
                {
                    int a = 9;
                    go.remove_any(a);
                    CASSERT(a == 3,"");
                }
                CASSERT(go.size() == 0,"");
            }

            // the test completed without throwing any exceptions.  This means it passed.
            return 0;
        }
        catch(exception& e)
        {
            // an error occurred so report that.
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.what();
            return 1;
        }        
    }





    class queue_tester : public tester
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a test for the queue object.  When it is constructed
                it adds itself into the testing framework.  The command line switch is
                specified as test_queue by passing that string to the tester constructor.
        !*/
    public:
        queue_tester (
        ) :
            tester ("test_queue",
                    "Runs tests on the queue component.")
        {}

        bool perform_test (
        )
        {
            // There are multiple implementations of the queue object so use
            // the templated function defined above to test them all and report
            // a failed test if any of them don't pass.
            int n = 0;
            n += queue_sort_test<queue<int>::sort_1a>    ("sort_1a");
            print_spinner();
            n += queue_sort_test<queue<int>::sort_1a_c>  ("sort_1a_c");
            print_spinner();
            n += queue_sort_test<queue<int>::sort_1b>    ("sort_1b");
            print_spinner();
            n += queue_sort_test<queue<int>::sort_1b_c>  ("sort_1b_c");
            print_spinner();
            n += queue_sort_test<queue<int>::sort_1c>    ("sort_1c");
            print_spinner();
            n += queue_sort_test<queue<int>::sort_1c_c>  ("sort_1c_c");
            print_spinner();
            return (n == 0);
        }
    };

    // Create an instance of this object.  Doing this causes this test
    // to be automatically inserted into the testing framework whenever this cpp file
    // is linked into the project.  Note that since we are inside an unnamed-namespace 
    // we won't get any linker errors about the symbol a being defined multple times. 
    queue_tester a;

}

