// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/set.h>

#include "tester.h"

namespace  
{
    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.set");

    template <
        typename set
        >
    int set_compare_test (
        const string& type
    )
    /*!
        requires
            - set is an implementation of set/set_compare_abstract.h and
              is instantiated with int
        ensures
            - runs tests on set for compliance with the specs 
            - returns 0 if no errors were found, 1 otherwise.
    !*/
    {        

        try 
        {
 
            srand(static_cast<unsigned int>(time(0)));

 

            set test, test2;

            enumerable<const int>& e = test;
            CASSERT(e.at_start() == true,"");

            for (int j = 0; j < 4; ++j)
            {

                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");


                CASSERT(test.size() == 0,"");
                CASSERT(test.is_member(5) == false,"");
                CASSERT(test.is_member(0) == false,"");
                CASSERT(test.is_member(-999) == false,"");
                CASSERT(test.is_member(4999) == false,"");


                int a,b = 0;
                a = 8;
                test.add(a);
                CASSERT(test.size() == 1,"");
                CASSERT(test.is_member(8) == true,"");
                CASSERT(test.is_member(5) == false,"");
                CASSERT(test.is_member(0) == false,"");
                CASSERT(test.is_member(-999) == false,"");
                CASSERT(test.is_member(4999) == false,"");
                a = 53;
                test.add(a);
                CASSERT(test.size() == 2,"");
                CASSERT(test.is_member(53) == true,"");
                CASSERT(test.is_member(5) == false,"");
                CASSERT(test.is_member(0) == false,"");
                CASSERT(test.is_member(-999) == false,"");
                CASSERT(test.is_member(4999) == false,"");


                swap(test,test2);



                CASSERT(test2.is_member(8) == true,"");
                CASSERT(test2.is_member(5) == false,"");
                CASSERT(test2.is_member(0) == false,"");
                CASSERT(test2.is_member(-999) == false,"");
                CASSERT(test2.is_member(4999) == false,"");
                CASSERT(test2.size() == 2,"");
                CASSERT(test2.is_member(53) == true,"");
                CASSERT(test2.is_member(5) == false,"");
                CASSERT(test2.is_member(0) == false,"");
                CASSERT(test2.is_member(-999) == false,"");
                CASSERT(test2.is_member(4999) == false,"");


                CASSERT(test.size() == 0,"");
                CASSERT(test.is_member(8) == false,"");
                CASSERT(test.is_member(5) == false,"");
                CASSERT(test.is_member(0) == false,"");
                CASSERT(test.is_member(-999) == false,"");
                CASSERT(test.is_member(4999) == false,"");
                CASSERT(test.size() == 0,"");
                CASSERT(test.is_member(53) == false,"");
                CASSERT(test.is_member(5) == false,"");
                CASSERT(test.is_member(0) == false,"");
                CASSERT(test.is_member(-999) == false,"");
                CASSERT(test.is_member(4999) == false,"");

                
                test.clear();
                CASSERT(test.at_start() == true,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.at_start() == false,"");


                CASSERT(test.size() == 0,"");

                while (test.size() < 10000)
                {
                    a = ::rand();
                    if (!test.is_member(a))
                        test.add(a);
                }

                CASSERT(test.size() == 10000,"");
                test.clear();
                CASSERT(test.size() == 0,"");

                while (test.size() < 10000)
                {
                    a = ::rand();
                    if (!test.is_member(a))
                        test.add(a);
                }

                CASSERT(test.size() == 10000,"");

                int count = 0;
                a = 0;
                while (test.move_next())
                {
                    enumerable<const int>& gogo = test;
                    gogo.element();

                    CASSERT(test.element() == test.element(),"");
                    CASSERT(test.element() == test.element(),"");
                    CASSERT(test.element() == test.element(),"");

                    CASSERT(a <= test.element(),"");
                    a = test.element();
                    ++count;
                }
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.move_next() == false,"");

                CASSERT(count == 10000,"");

                test.swap(test2);

                CASSERT(test.size() == 2,"");
                CASSERT(test2.size() == 10000,"");
                count = 0;
                a = -1;
                test2.reset();
                while (test2.move_next())
                {
                    CASSERT(test2.element() == test2.element(),"");
                    CASSERT(test2.element() == test2.element(),"");
                    CASSERT(test2.element() == test2.element(),"");
                    CASSERT(a < test2.element(),"");
                    a = test2.element();                
                    ++count;
                }
                CASSERT(test2.size() == 10000,"");
                CASSERT(count == 10000,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.move_next() == false,"");



                test2.clear();
                CASSERT(test2.size() == 0,"");
                CASSERT(test2.at_start() == true,"");

                while (test.size() < 20000)
                {
                    a = ::rand();
                    if (!test.is_member(a))
                        test.add(a);
                }

                CASSERT(test.at_start() == true,"");

                {
                    int* array = new int[test.size()];
                    int* tmp = array;

                    count = 0;
                    while (test.move_next())
                    {
                        CASSERT(test.element() == test.element(),"");
                        CASSERT(test.element() == test.element(),"");
                        CASSERT(test.element() == test.element(),"");
                        *tmp = test.element();
                        ++tmp;
                        ++count;
                    }
                    CASSERT(count == 20000,"");

                    // serialize the state of test, then clear test, then
                    // load the state back into test.
                    ostringstream sout;
                    serialize(test,sout);
                    CASSERT(test.at_start() == true,"");
                    istringstream sin(sout.str());
                    test.clear();
                    deserialize(test,sin);



                    tmp = array;
                    for (int i = 0; i < 20000; ++i)
                    {
                        CASSERT(test.is_member(*tmp) == true,"");
                        ++tmp;
                    }

                    CASSERT(test.size() == 20000,"");

                    tmp = array;
                    count = 0;
                    while (test.size() > 10000)
                    {
                        test.remove(*tmp,a);
                        CASSERT(*tmp == a,"");
                        ++tmp;
                        ++count;
                    }
                    CASSERT(count == 10000,"");
                    CASSERT(test.size() == 10000,"");

                    while (test.move_next())
                    {
                        CASSERT(test.element() == *tmp,"");
                        CASSERT(test.element() == *tmp,"");
                        CASSERT(test.element() == *tmp,"");
                        ++tmp;
                        ++count;
                    }
                    CASSERT(count == 20000,"");
                    CASSERT(test.size() == 10000,"");

                    while (test.size() < 20000)
                    {
                        a = ::rand();
                        if (!test.is_member(a))
                            test.add(a);
                    }

                    test2.swap(test);

                    count = 0;
                    a = 0;
                    while (test2.move_next())
                    {
                        CASSERT(test2.element() == test2.element(),"");
                        CASSERT(test2.element() == test2.element(),"");
                        CASSERT(test2.element() == test2.element(),"");
                        CASSERT(a <= test2.element(),"");
                        a = test2.element();                
                        ++count;
                    }

                    CASSERT(count == 20000,"");
                    CASSERT(test2.size() == 20000,"");

                    a = -1;
                    while (test2.size()>0)
                    {
                        test2.remove_any(b);
                        CASSERT( a < b,"");
                        a = b;
                    }

                    CASSERT(test2.size() == 0,"");
                    delete [] array;
                }

                test.clear();
                test2.clear();
                while (test.size() < 10000)
                {
                    a = ::rand();
                    if (!test.is_member(a))
                        test.add(a);
                }

                count = 0; 
                a = -1;
                while (test.move_next())
                {
                    CASSERT(a < test.element(),"");
                    a = test.element();
                    ++count;
                    if (count == 5000)
                        break;
                    CASSERT(test.current_element_valid() == true,"");
                }

                test.reset();

                count = 0; 
                a = -1;
                while (test.move_next())
                {
                    CASSERT(a < test.element(),"");
                    a = test.element();
                    ++count;
                    CASSERT(test.current_element_valid() == true,"");
                }

                CASSERT(count == 10000,"");


                test.clear();
                test2.clear();
            }



            {
                CASSERT(test == test2,"");
                CASSERT(test < test2 == false,"");
                CASSERT(test2 < test == false,"");

                int a = 3, b = 3;
                test.add(a);
                test2.add(b);
                test.move_next();                
                CASSERT(test == test2,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test.move_next();
                CASSERT(test < test2 == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test.move_next();
                CASSERT(test2 < test == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");

                a = 2; b = 5;
                test.add(a);
                test2.add(b);
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();
                CASSERT((test == test2) == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();                
                CASSERT(test < test2 == true,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();                
                CASSERT(test2 < test == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");


                a = 8;
                test.add(a);
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();
                CASSERT((test == test2) == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();                
                CASSERT(test < test2 == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();                
                CASSERT(test2 < test == true,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                
                test.clear();

                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();
                CASSERT((test == test2) == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();                
                CASSERT(test < test2 == true,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                test2.move_next();                
                CASSERT(test2 < test == false,"");
                CASSERT(test.at_start() && test2.at_start(),"");
                

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
    }







    class set_tester : public tester
    {
    public:
        set_tester (
        ) :
            tester ("test_set",
                    "Runs tests on the set component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += set_compare_test<set<int>::compare_1a>    ("compare_1a");
            print_spinner();
            n += set_compare_test<set<int>::compare_1a_c>  ("compare_1a_c");
            print_spinner();
            n += set_compare_test<set<int>::compare_1b>    ("compare_1b");
            print_spinner();
            n += set_compare_test<set<int>::compare_1b_c>  ("compare_1b_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

