// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/hash_map.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.hash_map");

    template <
        typename hash_map
        >
    int hash_map_kernel_test (
        const string& type
    )
    /*!
        requires
            - hash_map is an implementation of hash_map/hash_map_kernel_abstract.h and 
              is instantiated to map int to int
        ensures
            - runs tests on hash_map for compliance with the specs 
            - returns 0 if no errors were found, 1 otherwise.
    !*/
    {        

        try 
        {
            srand(static_cast<unsigned int>(time(0)));

 

            hash_map test, test2;

            enumerable<map_pair<int,int> >& e = test;
            CASSERT(e.at_start() == true,"");

            for (int j = 0; j < 4; ++j)
            {
                print_spinner();

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
                CASSERT(test.is_in_domain(5) == false,"");
                CASSERT(test.is_in_domain(0) == false,"");
                CASSERT(test.is_in_domain(-999) == false,"");
                CASSERT(test.is_in_domain(4999) == false,"");


                int a,b;
                a = 8;
                b = 94;
                test.add(a,b);
                CASSERT(test.size() == 1,"");
                CASSERT(test.is_in_domain(8) == true,"");
                CASSERT(test.is_in_domain(5) == false,"");
                CASSERT(test.is_in_domain(0) == false,"");
                CASSERT(test.is_in_domain(-999) == false,"");
                CASSERT(test.is_in_domain(4999) == false,"");
                CASSERT(test[8] == 94,"");
                a = 53;
                b = 4;
                test.add(a,b);
                CASSERT(test.size() == 2,"");
                CASSERT(test.is_in_domain(53) == true,"");
                CASSERT(test.is_in_domain(5) == false,"");
                CASSERT(test.is_in_domain(0) == false,"");
                CASSERT(test.is_in_domain(-999) == false,"");
                CASSERT(test.is_in_domain(4999) == false,"");
                CASSERT(test[53] == 4,"");


                swap(test,test2);


                CASSERT(test2.size() == 2,test2.size());
                CASSERT(test2.is_in_domain(8) == true,"");
                CASSERT(test2.is_in_domain(5) == false,"");
                CASSERT(test2.is_in_domain(0) == false,"");
                CASSERT(test2.is_in_domain(-999) == false,"");
                CASSERT(test2.is_in_domain(4999) == false,"");
                CASSERT(test2[8] == 94,"");
                CASSERT(test2.size() == 2,"");
                CASSERT(test2.is_in_domain(53) == true,"");
                CASSERT(test2.is_in_domain(5) == false,"");
                CASSERT(test2.is_in_domain(0) == false,"");
                CASSERT(test2.is_in_domain(-999) == false,"");
                CASSERT(test2.is_in_domain(4999) == false,"");
                CASSERT(test2[53] == 4,"");


                CASSERT(test.size() == 0,"");
                CASSERT(test.is_in_domain(8) == false,"");
                CASSERT(test.is_in_domain(5) == false,"");
                CASSERT(test.is_in_domain(0) == false,"");
                CASSERT(test.is_in_domain(-999) == false,"");
                CASSERT(test.is_in_domain(4999) == false,"");
                CASSERT(test.size() == 0,"");
                CASSERT(test.is_in_domain(53) == false,"");
                CASSERT(test.is_in_domain(5) == false,"");
                CASSERT(test.is_in_domain(0) == false,"");
                CASSERT(test.is_in_domain(-999) == false,"");
                CASSERT(test.is_in_domain(4999) == false,"");

                
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
                    b = ::rand();
                    if (!test.is_in_domain(a))
                        test.add(a,b);
                }

                CASSERT(test.size() == 10000,"");
                test.clear();
                CASSERT(test.size() == 0,"");

                while (test.size() < 10000)
                {
                    a = ::rand();
                    b = ::rand();
                    if (!test.is_in_domain(a))
                        test.add(a,b);
                }

                CASSERT(test.size() == 10000,"");

                int count = 0;
                while (test.move_next())
                {
                    CASSERT(test.element().key() == test.element().key(),"");
                    CASSERT(test.element().value() == test.element().value(),"");
                    CASSERT(test.element().key() == test.element().key(),"");
                    CASSERT(test.element().value() == test.element().value(),"");
                    


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
                test2.reset();

                test2.move_next();
                test2.element().value() = 99;
                CASSERT(test2[test2.element().key()] == 99,"");
                CASSERT(test2.element().value() == 99,"");

                test2.reset();

                while (test2.move_next())
                {
                    CASSERT(test2[test2.element().key()] == test2.element().value(),"");
                    CASSERT(test2.element().key() == test2.element().key(),"");
                    CASSERT(test2.element().value() == test2.element().value(),"");
                    CASSERT(test2.element().key() == test2.element().key(),"");
                    CASSERT(test2.element().value() == test2.element().value(),"");
            
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
                    b = ::rand();
                    if (!test.is_in_domain(a))
                        test.add(a,b);
                }

                CASSERT(test.at_start() == true,"");

                {
                    int* array1 = new int[test.size()];
                    int* array2 = new int[test.size()];
    
                    int* tmp1 = array1;
                    int* tmp2 = array2;



                    // serialize the state of test, then clear test, then
                    // load the state back into test.
                    ostringstream sout;
                    serialize(test,sout);
                    CASSERT(test.at_start() == true,"");
                    istringstream sin(sout.str());
                    test.clear();
                    deserialize(test,sin);
                    CASSERT(test.at_start() == true,"");


                    count = 0;
                    while (test.move_next())
                    {
                        CASSERT(test.element().key() == test.element().key(),"");
                        CASSERT(test.element().value() == test.element().value(),"");
                        CASSERT(test.element().key() == test.element().key(),"");
                        CASSERT(test.current_element_valid() == true,"");
                        *tmp1 = test.element().key();
                        *tmp2 = test.element().value();
                        ++tmp1;
                        ++tmp2;
                        ++count;
                    }
                    CASSERT(count == 20000,"");

                    tmp1 = array1;
                    tmp2 = array2;
                    for (int i = 0; i < 20000; ++i)
                    {
                        CASSERT(test.is_in_domain(*tmp1) == true,"");
                        CASSERT(test[*tmp1] == *tmp2,"");
                        ++tmp1;
                        ++tmp2;
                    }

                    CASSERT(test.size() == 20000,"");

                    tmp1 = array1;
                    tmp2 = array2;
                    count = 0;
                    while (test.size() > 10000)
                    {
                        test.remove(*tmp1,a,b);
                        CASSERT(*tmp1 == a,"");
                        CASSERT(*tmp2 == b,"");
                        ++tmp1;
                        ++tmp2;
                        ++count;
                    }
                    CASSERT(count == 10000,"");
                    CASSERT(test.size() == 10000,"");

                    while (test.move_next())
                    {
                        CASSERT(test.element().key() == *tmp1,"");
                        CASSERT(test.element().key() == *tmp1,"");
                        CASSERT(test.element().key() == *tmp1,"");
                        CASSERT(test.element().value() == *tmp2,"");
                        CASSERT(test.element().value() == *tmp2,"");
                        CASSERT(test.element().value() == *tmp2,"");
                        ++tmp1;
                        ++tmp2;
                        ++count;
                    }
                    CASSERT(count == 20000,"");
                    CASSERT(test.size() == 10000,"");

                    while (test.size() < 20000)
                    {
                        a = ::rand();
                        b = ::rand();
                        if (!test.is_in_domain(a))
                            test.add(a,b);
                    }

                    test2.swap(test);

                    count = 0;
                    while (test2.move_next())
                    {
                        CASSERT(test2.element().key() == test2.element().key(),"");
                        CASSERT(test2.element().value() == test2.element().value(),"");
                        CASSERT(test2.element().key() == test2.element().key(),"");
             
                        ++count;
                    }

                    CASSERT(count == 20000,"");
                    CASSERT(test2.size() == 20000,"");

                    int c = 0;
                    while (test2.size()>0)
                    {
                        test2.remove_any(b,c);

                    }

                    CASSERT(test2.size() == 0,"");
                    delete [] array1;
                    delete [] array2;
                }

                test.clear();
                test2.clear();
                while (test.size() < 10000)
                {
                    a = ::rand();
                    b = ::rand();
                    if (!test.is_in_domain(a))
                        test.add(a,b);
                }

                count = 0; 
                while (test.move_next())
                {

                    CASSERT(test[test.element().key()] == test.element().value(),"");
   
                    ++count;
                    if (count == 5000)
                        break;
                    CASSERT(test.current_element_valid() == true,"");
                }

                test.reset();

                count = 0; 
  
                while (test.move_next())
                {

                    ++count;
                    CASSERT(test.current_element_valid() == true,"");
                }

                CASSERT(count == 10000,"");


                test.clear();
                test2.clear();
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





    class hash_map_tester : public tester
    {
    public:
        hash_map_tester (
        ) :
            tester ("test_hash_map",
                    "Runs tests on the hash_map component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += hash_map_kernel_test<hash_map<int,int,14>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += hash_map_kernel_test<hash_map<int,int,14>::kernel_1a_c>  ("kernel_1b_c");
            print_spinner();
            n += hash_map_kernel_test<hash_map<int,int,14>::kernel_1b>    ("kernel_1b");
            print_spinner();
            n += hash_map_kernel_test<hash_map<int,int,14>::kernel_1b_c>  ("kernel_1a_c");
            print_spinner();
            n += hash_map_kernel_test<hash_map<int,int,14>::kernel_1c>    ("kernel_1c");
            print_spinner();
            n += hash_map_kernel_test<hash_map<int,int,14>::kernel_1c_c>  ("kernel_1c_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

