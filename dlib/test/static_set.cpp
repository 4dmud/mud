// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/queue.h>
#include <dlib/static_set.h>
#include <dlib/set.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.static_set");

    template <
        typename set
        >
    int static_set_kernel_test (
        const string& type
    )
    /*!
        requires
            - set is an implementation of static_set/static_set_kernel_abstract.h and
              is instantiated to hold ints
        ensures
            - runs tests on set for compliance with the specs 
            - returns 0 if no error was detected, 1 otherwise.
    !*/
    {        

        try 
        {
 
            srand(static_cast<unsigned int>(time(0)));

            typedef queue<int>::kernel_2a_c queue_of_int;
            typedef dlib::set<int>::kernel_1a_c set_of_int;
            
            queue_of_int q, qb, qc;
            set_of_int ds;

            set S;
            S.load(ds);

            for (int k = 1; k < 1000; ++k)
            {
                q.clear();
                qb.clear();
                qc.clear();
                unsigned long num = k; 
                for (unsigned long i = 0; i < num; ++i)
                {
                    int a = ::rand()&0xFF;
                    int b = a;
                    int c = a;
                    q.enqueue(a);
                    qb.enqueue(b);
                    qc.enqueue(c);
                }

               
                
                set s;

                CASSERT(s.size() == 0,"");
                CASSERT(s.at_start(),"");
                CASSERT(s.current_element_valid() == false,"");
                CASSERT(s.move_next() == false,"");
                CASSERT(s.current_element_valid() == false,"");
                CASSERT(s.at_start() == false,"");

                s.load(q);
                CASSERT(s.at_start(),"");
                set se;
                se.load(q);

                CASSERT(se.size() == 0,"");
                CASSERT(se.at_start() == true,"");
                CASSERT(se.current_element_valid() == false,"");     
                CASSERT(se.move_next() == false,"");
                CASSERT(se.at_start() == false,"");
                CASSERT(se.current_element_valid() == false,"");


                CASSERT(s.size() == qb.size(),"");
                CASSERT(s.at_start() == true,"");
                CASSERT(s.current_element_valid() == false,"");     
                CASSERT(s.move_next() == true,"");
                CASSERT(s.at_start() == false,"");
                CASSERT(s.current_element_valid() == true,"");
                s.reset();
                se.reset();

                swap(se,s);

                CASSERT(s.size() == 0,"");
                CASSERT(s.at_start() == true,"");
                CASSERT(s.current_element_valid() == false,"");     
                CASSERT(s.move_next() == false,"");
                CASSERT(s.at_start() == false,"");
                CASSERT(s.current_element_valid() == false,"");

                CASSERT(se.size() == qb.size(),"");
                CASSERT(se.at_start() == true,"");
                CASSERT(se.current_element_valid() == false,"");     
                CASSERT(se.move_next() == true,"");
                CASSERT(se.at_start() == false,"");
                CASSERT(se.current_element_valid() == true,"");
                s.reset();
                se.reset();

                swap(se,s);



                int last = 0;
                while (s.move_next())
                {
                    CASSERT(last <= s.element(),"");
                    last = s.element();
                }



                while (qb.move_next())
                {
                    int a;
                    qb.dequeue(a);
                    CASSERT(s.is_member(a),"");
                    CASSERT(!se.is_member(a),"");

                    // make sure is_member() doesn't hang
                    for (int l = 0; l < 100; ++l)
                    {
                        int a = ::rand();
                        s.is_member(a);
                    }
                }

                swap(s,se);

                // serialize the state of se, then clear se, then
                // load the state back into se.
                ostringstream sout;
                serialize(se,sout);
                CASSERT(se.at_start() == true,"");
                istringstream sin(sout.str());
                se.clear();
                deserialize(se,sin);
                CASSERT(se.at_start() == true,"");


                last = 0;
                while (se.move_next())
                {
                    CASSERT(last <= se.element(),"");
                    last = se.element();
                }


                CASSERT(s.size() == 0,"");
                CASSERT(se.size() == qc.size(),"");

                while (qc.move_next())
                {
                    int a;
                    qc.dequeue(a);
                    CASSERT(se.is_member(a),"");
                    CASSERT(!s.is_member(a),"");
                }


            }





            return 0;
        }
        catch(error e)
        {
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.info;
            return 1;
        }     
        catch (bad_alloc)
        {
            cout << "\n\nOUT OF MEMORY in " << type << endl;
            dlog << LWARN << "OUT OF MEMORY in " << type;
            return 1;
        }
    }





    class static_set_tester : public tester
    {
    public:
        static_set_tester (
        ) :
            tester ("test_static_set",
                    "Runs tests on the static_set component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += static_set_kernel_test<static_set<int>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += static_set_kernel_test<static_set<int>::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

