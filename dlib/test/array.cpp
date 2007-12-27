// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/interfaces/enumerable.h>
#include <dlib/array.h>


#include "tester.h"

namespace 
{
    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.array");

    template <
        typename arr
        >
    int array_sort_test (
        const std::string& type
    )
    /*!
        requires
            - array is an implementation of array/array_sort_abstract.h 
              array is instantiated with unsigned long
        ensures
            - runs tests on array for compliance with the specs
            - returns 0 if no errors were found, 1 otherwise
    !*/
    {        

        // extend the array we got by array_expand_1
        typedef array_expand_1<arr> array;

        try 
        {
            srand(static_cast<unsigned int>(time(0)));


            array a1, a2;
     
            {
                array a1, a2;

                for (int k = 1; k < 100000; k += 1000)
                {
                    for (int i = 0; i < 10; ++i)
                    {
                        a1.clear();
                        a1.set_max_size(500+k);
                        a1.set_size(500+k);
                        for (unsigned long j = 0; j < a1.size(); ++j)
                        {
                            a1[j] = j;
                            CASSERT(a1[j] == j,"");
                        }
                    }
                }
            }

            CASSERT(a1.max_size() == 0,"");
            CASSERT(a2.max_size() == 0,"");


            CASSERT(a1.size() == 0,"");
            CASSERT(a1.at_start(),"");
            CASSERT(a1.current_element_valid() == false,"");
            CASSERT(a1.move_next() == false,"");
            CASSERT(a1.size() == 0,"");
            CASSERT(a1.current_element_valid() == false,"");
            CASSERT(a1.at_start() == false,"");
            CASSERT(a1.move_next() == false,"");
            CASSERT(a1.current_element_valid() == false,"");
            CASSERT(a1.size() == 0,"");
            CASSERT(a1.at_start() == false,"");            
            CASSERT(a1.size() == 0,"");

            swap(a1,a2);
            CASSERT(a2.size() == 0,"");
            CASSERT(a2.current_element_valid() == false,"");
            CASSERT(a2.at_start() == false,"");
            CASSERT(a2.move_next() == false,"");
            CASSERT(a2.current_element_valid() == false,"");
            CASSERT(a2.size() == 0,"");
            CASSERT(a2.at_start() == false,"");            
            CASSERT(a2.size() == 0,"");



            CASSERT(a1.size() == 0,"");
            CASSERT(a1.at_start(),"");
            CASSERT(a1.current_element_valid() == false,"");
            CASSERT(a1.move_next() == false,"");
            CASSERT(a1.size() == 0,"");
            CASSERT(a1.current_element_valid() == false,"");
            CASSERT(a1.at_start() == false,"");
            CASSERT(a1.move_next() == false,"");
            CASSERT(a1.current_element_valid() == false,"");
            CASSERT(a1.size() == 0,"");
            CASSERT(a1.at_start() == false,"");            
            CASSERT(a1.size() == 0,"");

            a1.reset();
            a2.reset();

            for (unsigned long k = 0; k < 4; ++k)
            {

                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start(),"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.at_start() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start() == false,"");            
                CASSERT(a1.size() == 0,"");

                swap(a1,a2);
                CASSERT(a2.size() == 0,"");
                CASSERT(a2.current_element_valid() == false,"");
                CASSERT(a2.at_start() == false,"");
                CASSERT(a2.move_next() == false,"");
                CASSERT(a2.current_element_valid() == false,"");
                CASSERT(a2.size() == 0,"");
                CASSERT(a2.at_start() == false,"");            
                CASSERT(a2.size() == 0,"");



                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start(),"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.at_start() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start() == false,"");            
                CASSERT(a1.size() == 0,"");

                a1.clear();
                a2.clear();


                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start(),"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.at_start() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start() == false,"");            
                CASSERT(a1.size() == 0,"");

                swap(a1,a2);
                CASSERT(a2.size() == 0,"");
                CASSERT(a2.current_element_valid() == false,"");
                CASSERT(a2.at_start() == false,"");
                CASSERT(a2.move_next() == false,"");
                CASSERT(a2.current_element_valid() == false,"");
                CASSERT(a2.size() == 0,"");
                CASSERT(a2.at_start() == false,"");            
                CASSERT(a2.size() == 0,"");



                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start(),"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.at_start() == false,"");
                CASSERT(a1.move_next() == false,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.size() == 0,"");
                CASSERT(a1.at_start() == false,"");            
                CASSERT(a1.size() == 0,"");

                a1.clear();
                a2.clear();




                a1.set_max_size(100000);
                a2.set_max_size(100000);
                a1.set_size(10000);
                CASSERT(a1.size() == 10000,"");
                a2.set_size(10000);
                CASSERT(a2.size() == 10000,"");
                for (unsigned long i = 0; i < a1.size(); ++i)
                {
                    unsigned long a = static_cast<unsigned long>(::rand());
                    CASSERT(a >= 0,"");
                    a1[i] = a;
                    a2[i] = i;
                    CASSERT(a1[i] == a,"");
                    CASSERT(a2[i] == i,"");
                }

                CASSERT(a1.at_start(),"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.move_next(),"");
                CASSERT(a1.current_element_valid(),"");

                CASSERT(a1.at_start() == false,"");
                a1.sort();
                CASSERT(a1.at_start(),"");
                a2.sort();
                CASSERT(a1.size() == 10000,"");
                CASSERT(a2.size() == 10000,"");


                for (unsigned long i = 0; i < a1.size(); ++i)
                {
                    if (i+1 < a1.size())
                    {
                        CASSERT(a1[i] <= a1[i+1],
                            "a1[i]: " << a1[i] << "    a1[i+1]: " << a1[i+1]
                            << "    i: " << i);
                    }
                    CASSERT(a2[i] == i,"i: " << i << "   a2[i]: " << a2[i]);
                }

                unsigned long last = 0;
                unsigned long count = 0;
                while (a1.move_next())
                {
                    CASSERT(last <= a1.element(),"");
                    last = a1.element();
                    ++count;
                }
                CASSERT(count == a1.size(),"");

                last = 0;
                count = 0;
                while (a2.move_next())
                {
                    CASSERT(last <= a2.element(),"");
                    last = a2.element();
                    ++count;
                }
                CASSERT(count == a2.size(),"");

                a2.set_size(15000);

                for (unsigned long i = 0; i < a1.size(); ++i)
                {
                    if (i+1 < a1.size())
                    {
                        CASSERT(a1[i] <= a1[i+1],"");
                    }
                    CASSERT(a2[i] == i,"");
                }

                for (unsigned long i = 10000; i < a2.size(); ++i)
                {
                    a2[i] = i;
                    CASSERT(a2[i] == i,"");
                }

                for (unsigned long i = 0; i < a2.size(); ++i)
                {
                    CASSERT(a2[i] == i,"");
                }

                a2.reset();
                last = 0;
                while (a2.move_next())
                {
                    CASSERT(last <= a2.element(),"");
                    last = a2.element();
                }

                a1.reset();
                last = 0;
                while (a1.move_next())
                {
                    CASSERT(last <= a1.element(),"");
                    last = a1.element();
                }

                a1.sort();
                last = 0;
                while (a1.move_next())
                {
                    CASSERT(last <= a1.element(),"");
                    last = a1.element();
                }

                swap(a2,a1);

                for (unsigned long i = 0; i < 15000; ++i)
                {
                    CASSERT(a1[i] == i,"");
                }



                a1.clear();
                CASSERT(a1.max_size() == 0,"");




                a1.clear();
                a2.clear();


                CASSERT(a1.size() == 0,"");
                CASSERT(a2.size() == 0,"");
                a1.set_max_size(100000);
                a2.set_max_size(100000);

                a1.set_size(10000);
                CASSERT(a1.size() == 10000,"");
                a2.set_size(10000);
                CASSERT(a2.size() == 10000,"");
                for (unsigned long i = 0; i < a1.size(); ++i)
                {
                    unsigned long a = static_cast<unsigned long>(::rand());
                    CASSERT(a >= 0,"");
                    a1[i] = a;
                    a2[i] = i;
                    CASSERT(a1[i] == a,"");
                    CASSERT(a2[i] == i,"");
                }

                CASSERT(a1.at_start(),"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.move_next(),"");
                CASSERT(a1.current_element_valid(),"");

                CASSERT(a1.at_start() == false,"");
                a1.sort();
                CASSERT(a1.at_start(),"");
                a2.sort();
                CASSERT(a1.size() == 10000,"");
                CASSERT(a2.size() == 10000,"");


                for (unsigned long i = 0; i < a1.size(); ++i)
                {
                    if (i+1 < a1.size())
                    {
                        CASSERT(a1[i] <= a1[i+1],"");
                    }
                    CASSERT(a2[i] == i,"");
                }

                last = 0;
                while (a1.move_next())
                {
                    CASSERT(last <= a1.element(),"");
                    last = a1.element();
                }

                last = 0;
                while (a2.move_next())
                {
                    CASSERT(last <= a2.element(),"");
                    last = a2.element();
                }

                a2.set_size(15000);

                for (unsigned long i = 0; i < a1.size(); ++i)
                {
                    if (i+1 < a1.size())
                    {
                        CASSERT(a1[i] <= a1[i+1],"");
                    }
                    CASSERT(a2[i] == i,"");
                }

                for (unsigned long i = 10000; i < a2.size(); ++i)
                {
                    a2[i] = i;
                    CASSERT(a2[i] == i,"");
                }

                for (unsigned long i = 0; i < a2.size(); ++i)
                {
                    CASSERT(a2[i] == i,"");
                }

                a2.reset();
                last = 0;
                while (a2.move_next())
                {
                    CASSERT(last <= a2.element(),"");
                    last = a2.element();
                }

                a1.reset();
                last = 0;
                while (a1.move_next())
                {
                    CASSERT(last <= a1.element(),"");
                    last = a1.element();
                }

                a1.sort();
                last = 0;
                while (a1.move_next())
                {
                    CASSERT(last <= a1.element(),"");
                    last = a1.element();
                }

                swap(a2,a1);

                for (unsigned long i = 0; i < 15000; ++i)
                {
                    CASSERT(a1[i] == i,"");
                }



                a1.clear();
                CASSERT(a1.max_size() == 0,"");

                a2.clear();
                print_spinner();
            }



            a1.set_max_size(2000000);
            CASSERT(a1.max_size() == 2000000,"");
            CASSERT(a1.size() == 0,"");
            a1.set_size(2000000);
            CASSERT(a1.max_size() == 2000000,"");
            CASSERT(a1.size() == 2000000,"");
            
            for (unsigned long i = 0; i < a1.size(); ++i)
            {
                a1[i] = ::rand();
            }

            print_spinner();
            a1.sort();

            print_spinner();
            // serialize the state of a1, then clear a1, then
            // load the state back into a1.
            ostringstream sout;
            serialize(a1,sout);
            CASSERT(a1.at_start() == true,"");
            istringstream sin(sout.str());
            a1.clear();
            CASSERT(a1.max_size() == 0,"");
            deserialize(a1,sin);

            CASSERT(a1.size() == 2000000,"");

            for (unsigned long i = 0; i < a1.size()-1; ++i)
            {
                CASSERT(a1[i] <= a1[i+1],"");
            }

            CASSERT(a1.max_size() == 2000000,"");
            CASSERT(a1.size() == 2000000,"");


            swap(a1,a2);

            print_spinner();

            CASSERT(a2.size() == 2000000,"");

            for (unsigned long i = 0; i < a2.size()-1; ++i)
            {
                CASSERT(a2[i] <= a2[i+1],"");
            }

            CASSERT(a2.max_size() == 2000000,"");
            CASSERT(a2.size() == 2000000,"");

            swap(a1,a2);


            a1.clear();
            CASSERT(a1.size() == 0,"");
            CASSERT(a1.max_size() == 0,"");

            a1.expand(10);
            CASSERT(a1.size() == 10,"");
            CASSERT(a1.max_size() == 10,"");

            for (unsigned long i = 0; i < a1.size(); ++i)
            {
                a1[i] = i;
            }

            print_spinner();
            a1.expand(100);
            CASSERT(a1.size() == 100,"");
            CASSERT(a1.max_size() == 100,"");

            for (unsigned long i = 0; i < 10; ++i)
            {
                CASSERT(a1[i] == i,"");
            }

            a1.expand(50);
            CASSERT(a1.size() == 50,"");
            CASSERT(a1.max_size() == 100,"");

            for (unsigned long i = 0; i < 10; ++i)
            {
                CASSERT(a1[i] == i,"");
            }

            a1.expand(10);
            CASSERT(a1.size() == 10,"");
            CASSERT(a1.max_size() == 100,"");

            for (unsigned long i = 0; i < 10; ++i)
            {
                CASSERT(a1[i] == i,"");
            }

            a1.expand(20);
            CASSERT(a1.size() == 20,"");
            CASSERT(a1.max_size() == 100,"");

            for (unsigned long i = 0; i < 10; ++i)
            {
                CASSERT(a1[i] == i,"");
            }


            a1.expand(100);
            CASSERT(a1.size() == 100,"");
            CASSERT(a1.max_size() == 100,"");

            for (unsigned long i = 0; i < 10; ++i)
            {
                CASSERT(a1[i] == i,"");
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










    class array_tester : public tester
    {
    public:
        array_tester (
        ) :
            tester ("test_array",
                    "Runs tests on the array component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_1a>    ("sort_1a");
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_1a_c>  ("sort_1a_c");
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_1b>    ("sort_1b");
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_1b_c>  ("sort_1b_c");
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_2a>    ("sort_2a");
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_2a_c>  ("sort_2a_c");
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_2b>    ("sort_2b");
            print_spinner();
            n += array_sort_test<array<unsigned long>::sort_2b_c>  ("sort_2b_c");
            print_spinner();
            return (n == 0);
        }
    } a;




}

