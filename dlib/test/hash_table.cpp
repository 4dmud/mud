// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/hash_table.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.hash_table");

    template <
        typename hash_table
        >
    int hash_table_kernel_test (
        const string& type
    )
    /*!
        requires
            - hash_table is an implementation of hash_table/hash_table_kernel_abstract.h 
              and is instantiated to map ints to ints
        ensures
            - runs tests on hash_table for compliance with the specs 
            - returns 0 if no errors were detected, 1 otherwise.
    !*/
    {        

        try 
        {
 
            srand(static_cast<unsigned int>(time(0)));




            {
            hash_table test(16);

            CASSERT(test.count(3) == 0,"");

            enumerable<map_pair<int,int> >& e = test;
            CASSERT(e.at_start() == true,"");

            hash_table test2(16);

            hash_table test3(0);
            hash_table test4(0);


            print_spinner();

            int b;
            for (int j = 0; j < 4; ++j)
            {
                int a = 4;
                b = 5;
                test2.add(a,b);
                CASSERT(test2.size() == 1,"");
                CASSERT(*test2[4] == 5,"");
                CASSERT(test2[99] == 0,"");

                CASSERT(test2.move_next(),"");
                CASSERT(test2.element().key() == 4,"");
                CASSERT(test2.element().value() == 5,"");

                swap(test,test2);
                CASSERT(test.size() == 1,"");
                CASSERT(*test[4] == 5,"");
                CASSERT(test[99] == 0,"");

                test.swap(test2);

                a = 99; 
                b = 35;
                test2.add(a,b);
                CASSERT(test2.size() == 2,"");
                CASSERT(*test2[4] == 5,"");
                CASSERT(*test2[99] == 35,"");
                CASSERT(test2[99] != 0,"");
                CASSERT(test2[949] == 0,"");

                test2.destroy(4);
                CASSERT(test2.size() == 1,"");
                CASSERT(test2[4] == 0,"");
                CASSERT(*test2[99] == 35,"");
                CASSERT(test2[99] != 0,"");
                CASSERT(test2[949] == 0,"");



                test2.destroy(99);
                CASSERT(test2.size() == 0,"");
                CASSERT(test2[4] == 0,"");                
                CASSERT(test2[99] == 0,"");
                CASSERT(test2[949] == 0,"");



                test2.clear();
            }

            
            print_spinner();




            for (int j = 0; j < 4; ++j)
            {

                CASSERT(test.count(3) == 0,"");
                CASSERT(test.size() == 0,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");

                int a;

                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand()%1000;
                    int temp = a;
                    unsigned long count = test.count(a);
                    test.add(a,b);
                    CASSERT(test.count(temp) == count+1,"");
                }

                {
                    unsigned long count = test.count(3);

                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                }


                test.clear();


                for (int i = 0; i < 10000; ++i)
                {
                    a = b = i;
                    unsigned long count = test.count(a);
                    test.add(a,b);
                    CASSERT(test.count(i) == count+1,"");
                }

                CASSERT(test.size() == 10000,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.move_next() == true,"");            
                CASSERT(test.current_element_valid() == true,"");


                test.reset();

                CASSERT(test.size() == 10000,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                
                
                if (test.size() > 0)
                {
                    int* array = new int[test.size()];
                    int* tmp = array;

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        *tmp = test.element().key();
                        CASSERT(test[*tmp] != 0,"");                    
                        CASSERT(*tmp == test.element().key(),"");
                        CASSERT(*tmp == test.element().value(),"");
                        CASSERT(*tmp == test.element().key(),"");
                        CASSERT(test.current_element_valid() == true,"");
                        ++tmp;
                    }

                    CASSERT(count == 10000,"");
                    CASSERT(test.at_start() == false,"");
                    CASSERT(test.current_element_valid() == false,"");
                    CASSERT(test.move_next() == false,"");
                    CASSERT(test.current_element_valid() == false,"");
                    CASSERT(test.at_start() == false,"");
                    CASSERT(test.current_element_valid() == false,"");

                    CASSERT(test.size() == 10000,"");

                    swap(test,test2);




                    // serialize the state of test2, then clear test2, then
                    // load the state back into test2.
                    ostringstream sout;
                    serialize(test2,sout);
                    CASSERT(test2.at_start() == true,"");
                    istringstream sin(sout.str());
                    test2.clear();
                    deserialize(test2,sin);
                    CASSERT(test2.at_start() == true,"");




                    tmp = array;
                    for (int i = 0; i < 10000; ++i)
                    {
                        CASSERT(*test2[*tmp] == *tmp,"");
                        CASSERT(*test2[*tmp] == *tmp,"");
                        CASSERT(*test2[*tmp] == *tmp,"");
                        ++tmp;
                    }

                    test2.swap(test);
                    test.reset();

                    CASSERT(test.at_start() == true,"");
                    count = 0;
                    tmp = array;
                    while (test.size() > 0)
                    {
                        test.remove(*tmp,a,b);

                        ++tmp;
                        ++count;
                    }
                
                    CASSERT(count == 10000,"");
                    CASSERT(test.size() == 0,"");



                    CASSERT(count == 10000,"");







                    delete [] array;
                }

                test.move_next();

                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand();
                    test.add(a,b);
                }

                CASSERT(test.at_start() == true,"");
                CASSERT(test.move_next() == true,"");

                CASSERT(test.size() == 10000,"");
                
                for (int i = 0; i < 10000; ++i)
                {
                    test.remove_any(a,b);
                }

                CASSERT(test.at_start() == true,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.size() == 0,"");

                test.clear();









                int* dtmp = new int[10000];
                int* rtmp = new int[10000];
                
                int* d = dtmp;
                int* r = rtmp;
                for (unsigned long i = 0; i < 10000; ++i)
                {
                    a = ::rand();
                    b = ::rand();
                    *d = a;
                    *r = b;
                    if (test[a] != 0)
                    {
                        --i;
                        continue;
                    }
                    test.add(a,b);
                    ++d;
                    ++r;
                    CASSERT(test.size() == i+1,"");
                }

                CASSERT(test.size() == 10000,"");

                for (int i = 0; i < 10000; ++i)
                {
                    CASSERT(*test[dtmp[i]] == rtmp[i],"");
                }


                delete [] dtmp;
                delete [] rtmp;

                test.clear();
            }}


            print_spinner();
























            // now do the same thing as above but with a much smaller hash table
            {
            hash_table test(13);

            CASSERT(test.count(3) == 0,"");

            enumerable<map_pair<int,int> >& e = test;
            CASSERT(e.at_start() == true,"");

            hash_table test2(16);

            hash_table test3(0);
            hash_table test4(0);


            int b;
            for (int j = 0; j < 4; ++j)
            {
                int a = 4;
                b = 5;
                test2.add(a,b);
                CASSERT(test2.size() == 1,"");
                CASSERT(*test2[4] == 5,"");
                CASSERT(test2[99] == 0,"");


                CASSERT(test2.move_next(),"");
                CASSERT(test2.element().key() == 4,"");
                CASSERT(test2.element().value() == 5,"");

                swap(test,test2);
                CASSERT(test.size() == 1,"");
                CASSERT(*test[4] == 5,"");
                CASSERT(test[99] == 0,"");

                test.swap(test2);

                a = 99; 
                b = 35;
                test2.add(a,b);
                CASSERT(test2.size() == 2,"");
                CASSERT(*test2[4] == 5,"");
                CASSERT(*test2[99] == 35,"");
                CASSERT(test2[99] != 0,"");
                CASSERT(test2[949] == 0,"");

                test2.destroy(4);
                CASSERT(test2.size() == 1,"");
                CASSERT(test2[4] == 0,"");
                CASSERT(*test2[99] == 35,"");
                CASSERT(test2[99] != 0,"");
                CASSERT(test2[949] == 0,"");



                test2.destroy(99);
                CASSERT(test2.size() == 0,"");
                CASSERT(test2[4] == 0,"");                
                CASSERT(test2[99] == 0,"");
                CASSERT(test2[949] == 0,"");



                test2.clear();
            }

            
            print_spinner();




            for (int j = 0; j < 4; ++j)
            {

                CASSERT(test.count(3) == 0,"");
                CASSERT(test.size() == 0,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");

                int a;

                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand()%1000;
                    int temp = a;
                    unsigned long count = test.count(a);
                    test.add(a,b);
                    CASSERT(test.count(temp) == count+1,"");
                }

                {
                    unsigned long count = test.count(3);

                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                    a = 3; test.add(a,b); ++count;
                    CASSERT(test.count(3) == count,"");
                }


                test.clear();


                for (int i = 0; i < 10000; ++i)
                {
                    a = b = i;
                    unsigned long count = test.count(a);
                    test.add(a,b);
                    CASSERT(test.count(i) == count+1,"");
                }

                CASSERT(test.size() == 10000,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.move_next() == true,"");            
                CASSERT(test.current_element_valid() == true,"");


                test.reset();

                CASSERT(test.size() == 10000,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                
                
                if (test.size() > 0)
                {
                    int* array = new int[test.size()];
                    int* tmp = array;

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        *tmp = test.element().key();
                        CASSERT(test[*tmp] != 0,"");                    
                        CASSERT(*tmp == test.element().key(),"");
                        CASSERT(*tmp == test.element().value(),"");
                        CASSERT(*tmp == test.element().key(),"");
                        CASSERT(test.current_element_valid() == true,"");
                        ++tmp;
                    }

                    CASSERT(count == 10000,"");
                    CASSERT(test.at_start() == false,"");
                    CASSERT(test.current_element_valid() == false,"");
                    CASSERT(test.move_next() == false,"");
                    CASSERT(test.current_element_valid() == false,"");
                    CASSERT(test.at_start() == false,"");
                    CASSERT(test.current_element_valid() == false,"");

                    CASSERT(test.size() == 10000,"");

                    swap(test,test2);

                    tmp = array;
                    for (int i = 0; i < 10000; ++i)
                    {
                        CASSERT(*test2[*tmp] == *tmp,"");
                        CASSERT(*test2[*tmp] == *tmp,"");
                        CASSERT(*test2[*tmp] == *tmp,"");
                        ++tmp;
                    }

                    test2.swap(test);
                    test.reset();

                    CASSERT(test.at_start() == true,"");
                    count = 0;
                    tmp = array;
                    while (test.size() > 0)
                    {
                        test.remove(*tmp,a,b);

                        ++tmp;
                        ++count;
                    }
                
                    CASSERT(count == 10000,"");
                    CASSERT(test.size() == 0,"");



                    CASSERT(count == 10000,"");







                    delete [] array;
                }

                test.move_next();

                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand();
                    test.add(a,b);
                }

                CASSERT(test.at_start() == true,"");
                CASSERT(test.move_next() == true,"");

                CASSERT(test.size() == 10000,"");
                
                for (int i = 0; i < 10000; ++i)
                {
                    test.remove_any(a,b);
                }

                CASSERT(test.at_start() == true,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.size() == 0,"");

                test.clear();








                int* dtmp = new int[10000];
                int* rtmp = new int[10000];
                
                int* d = dtmp;
                int* r = rtmp;
                for (unsigned long i = 0; i < 10000; ++i)
                {
                    a = ::rand();
                    b = ::rand();
                    *d = a;
                    *r = b;
                    if (test[a] != 0)
                    {
                        --i;
                        continue;
                    }
                    test.add(a,b);
                    ++d;
                    ++r;
                    CASSERT(test.size() == i+1,"");
                }

                CASSERT(test.size() == 10000,"");

                for (int i = 0; i < 10000; ++i)
                {
                    CASSERT(*test[dtmp[i]] == rtmp[i],"");
                }


                delete [] dtmp;
                delete [] rtmp;

                test.clear();
            }}












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





    class hash_table_tester : public tester
    {
    public:
        hash_table_tester (
        ) :
            tester ("test_hash_table",
                    "Runs tests on the hash_table component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += hash_table_kernel_test<hash_table<int,int>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += hash_table_kernel_test<hash_table<int,int>::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            n += hash_table_kernel_test<hash_table<int,int>::kernel_2a>    ("kernel_2a");
            print_spinner();
            n += hash_table_kernel_test<hash_table<int,int>::kernel_2a_c>  ("kernel_2a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

