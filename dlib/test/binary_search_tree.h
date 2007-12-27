// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_BINARY_SEARCH_TREE_KERNEl_TEST_H_
#define DLIB_BINARY_SEARCH_TREE_KERNEl_TEST_H_


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/memory_manager_global.h>
#include <dlib/memory_manager_stateless.h>
#include <dlib/binary_search_tree.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.binary_search_tree");

    template <
        typename bst
        >
    int binary_search_tree_kernel_test (
        const string& type
    )
    /*!
        requires
            - bst is an implementation of 
              binary_search_tree/binary_search_tree_kernel_abstract.h is instantiated 
              to map int to int
        ensures
            - runs tests on bst for compliance with the specs 
            - returns 0 if no errors were detected, 1 otherwise.
    !*/
    {        

            bst test, test2;
        try 
        {
 
            srand(static_cast<unsigned int>(time(0)));


            CASSERT(test.count(3) == 0,"");

            enumerable<map_pair<int,int> >& e = test;
            CASSERT(e.at_start() == true,"");

            CASSERT(test.count(3) == 0,"");

            for (int i = 0; i < 4; ++i)
            {
                CASSERT(test.size() == 0,"");
                CASSERT(test.count(3) == 0,"");
                CASSERT(test.height() == 0,"");
                CASSERT(test[5] == 0,"");
                CASSERT(test[0] == 0,"");
                CASSERT(test.at_start(),"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.count(3) == 0,"");

                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.count(3) == 0,"");

                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");

                test.clear();
                test.position_enumerator(5);
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false, "");
                test.position_enumerator(5);
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false, "");
                test.position_enumerator(9);
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false, "");
                test.clear();
                test.position_enumerator(5);
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false, "");
                test.position_enumerator(5);
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false, "");
                test.position_enumerator(9);
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false, "");
                test.clear();
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");


                CASSERT(test.count(3) == 0,"");

                CASSERT(test.size() == 0,"");
                CASSERT(test.height() == 0,"");
                CASSERT(test[5] == 0,"");
                CASSERT(test[0] == 0,"");
                CASSERT(const_cast<const bst&>(test)[5] == 0,"");
                CASSERT(const_cast<const bst&>(test)[0] == 0,"");
                CASSERT(test.at_start(),"");
                CASSERT(test.current_element_valid() == false,"");

                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");

                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");


                CASSERT(test.count(3) == 0,"");
                test.reset();
                CASSERT(test.count(3) == 0,"");

                CASSERT(test.at_start(),"");
                CASSERT(test.current_element_valid() == false,"");






                int a = 0, b = 0;

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
                    a = ::rand()&0x7FFF;
                    b = 0;
                    int temp = a;
                    unsigned long count = test.count(a);
                    test.add(a,b);
                    CASSERT(test.count(temp) == count+1,"");
                }

                // serialize the state of test, then clear test, then
                // load the state back into test.
                ostringstream sout;
                serialize(test,sout);
                istringstream sin(sout.str());
                test.clear();
                deserialize(test,sin);

                CASSERT(test.size() == 10000,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");


                CASSERT(test.height() > 13 && test.height() <= 26,"this is somewhat of an implementation dependent "
                    << "but really it should be in this range or the implementation is just crap");
                
                a = 0;
                unsigned long count = 0;
                while (test.move_next())
                {
                    CASSERT(a <= test.element().key(),"the numers are out of order but they should be in order");
                    a = test.element().key();
                    ++count;


                    CASSERT(test.at_start() == false,"");
                    CASSERT(test.current_element_valid() == true,"");
                }

                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.move_next() == false,"");

                CASSERT(count == 10000,"");




                CASSERT(test.height() > 13 && test.height() <= 26,"this is somewhat of an implementation dependent "
                    << "but really it should be in this range or the implementation is just crap");

                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.size() == 10000,"");


                swap(test,test2);


                test2.reset();
                count = 0;
                a = 0;
                while (test2.move_next())
                {
                    CASSERT(a <= test2.element().key(),"the numers are out of order but they should be in order");
                    a = test2.element().key();
                    ++count;


                    CASSERT(test2.at_start() == false,"");
                    CASSERT(test2.current_element_valid() == true,"");

                    if (count == 5000)
                    {
                        break;
                    }
                }

                CASSERT(test2.move_next() == true,"");
                CASSERT(test2.move_next() == true,"");
                CASSERT(test2.move_next() == true,"");


                test2.reset();

                count = 0;
                a = 0;
                while (test2.move_next())
                {
                    CASSERT(a <= test2.element().key(),"the numers are out of order but they should be in order");
                    a = test2.element().key();
                    ++count;


                    CASSERT(test2.at_start() == false,"");
                    CASSERT(test2.current_element_valid() == true,"");
                }

                CASSERT(count == 10000,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.move_next() == false,"");








                int last = 0;
                asc_pair_remover<int,int,typename bst::compare_type>& asdf = test2;
                CASSERT(asdf.size() > 0,"");
                while (asdf.size() > 0)
                {
                    asdf.remove_any(a,b);
                    CASSERT(last <= a,"");
                    last = a;
                    --count;
                    CASSERT(asdf.size() == count,"");
                }


                CASSERT(test2.size() == 0,"");
                CASSERT(test2.height() ==0,"");
                CASSERT(test2.at_start() == true,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.move_next() == false,"");




                for (int i = 0; i < 10000; ++i)
                {
                    a = i;
                    b = i;
                    test2.add(a,b);
                    CASSERT(test2.size() == (unsigned int)(i +1),"");
                    CASSERT(test2.count(i) == 1,"");
                }

                a = 0;
                test2.position_enumerator(a);
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.element().key() == a, "");
                CASSERT(test2.element().value() == a, "");
                a = 0;
                test2.position_enumerator(a);
                CASSERT(test2.element().key() == a, "");
                CASSERT(test2.element().value() == a, "");
                a = 8;
                test2.position_enumerator(a);
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.element().key() == a, "");
                CASSERT(test2.element().value() == a, "");
                a = 1;
                test2.position_enumerator(a);
                CASSERT(test2.element().key() == a, "");
                CASSERT(test2.element().value() == a, "");
                a = -29;
                test2.position_enumerator(a);
                CASSERT(test2.element().key() == 0, "");
                CASSERT(test2.element().value() == 0, "");
                a = 10000;
                test2.position_enumerator(a);
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == false,"");
                a = -29;
                test2.position_enumerator(a);
                CASSERT(test2.element().key() == 0, "");
                CASSERT(test2.element().value() == 0, "");
                a = 8;
                test2.position_enumerator(a);
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.element().key() == a, "");
                CASSERT(test2.element().value() == a, "");
                test2.reset();


                CASSERT(test2.height() > 13 && test2.height() <= 26,"this is somewhat of an implementation dependent "
                    << "but really it should be in this range or the implementation is just crap");

                CASSERT(test2.at_start() == true,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.size() == 10000,"");


                for (int i = 0; i < 10000; ++i)
                {
                    CASSERT(test2.move_next() == true,"");
                    CASSERT(test2.element().key() == i,"");
                }



                CASSERT(test2.height() > 13 && test2.height() <= 26,"this is somewhat of an implementation dependent "
                    << "but really it should be in this range or the implementation is just crap");

                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == true,"");
                CASSERT(test2.size() == 10000,"");


                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.current_element_valid() == false,"");

                a = 3;
                test2.add(a,b);
                CASSERT(test2.count(3) == 2,"");


                for (int i = 0; i < 10000; ++i)
                {
                    test2.remove(i,a,b);
                    CASSERT(i == a,"");
                }
                test2.remove(3,a,b);


                CASSERT(test2.size() == 0,"");
                CASSERT(test2.height() == 0,"");
                CASSERT(test2.at_start() == true,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == false,"");

                

                test2.clear();


                int m = 0;
                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand()&0x7FFF;
                    m = max(a,m);
                    test2.add(a,b);
                }

                CASSERT(test2.at_start() == true,"");
                CASSERT(test2.move_next() == true,"");
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == true,"");
                CASSERT(test2.move_next() == true,"");
                CASSERT(test2.current_element_valid() == true,"");
                CASSERT(test2.move_next() == true,"");
                CASSERT(test2.current_element_valid() == true,"");
                CASSERT(test2.move_next() == true,"");
                CASSERT(test2.current_element_valid() == true,"");
                CASSERT(test2.at_start() == false,"");

                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand()&0xFFFF;
                    test2.position_enumerator(a);
                    if (test2[a])
                    {
                        CASSERT(test2.element().key() == a,"");
                    }
                    else if (a <= m)
                    {
                        CASSERT(test2.element().key() > a,"");
                    }
                }

                test2.clear();

                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.at_start() == true,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.at_start() == false,"");


                CASSERT(test2.size() == 0,"");
                CASSERT(test2.height() == 0,"");
                

                for (int i = 0; i < 20000; ++i)
                {
                    a = ::rand()&0x7FFF;
                    b = a;
                    test2.add(a,b);
                }


                CASSERT(test2.size() == 20000,"");



                // remove a bunch of elements randomly
                int c;
                for (int i = 0; i < 50000; ++i)
                {
                    a = ::rand()&0x7FFF;
                    if (test2[a] != 0)
                    {
                        test2.remove(a,b,c);
                        CASSERT(a == b,"");
                    }
                }


                // now add a bunch more
                for (int i = 0; i < 10000; ++i)
                {
                    a = ::rand()&0x7FFF;
                    b = a;
                    test2.add(a,b);
                }


                // now iterate over it all and then remove all elements
                {
                    int* array = new int[test2.size()];
                    int* tmp = array;
                    CASSERT(test2.at_start() == true,"");
                    while (test2.move_next())
                    {
                        *tmp = test2.element().key();
                        ++tmp;
                    }

                    CASSERT(test2.at_start() == false,"");
                    CASSERT(test2.current_element_valid() == false,"");
                    CASSERT(test2.move_next() == false,"");

                    tmp = array;
                    for (int i = 0; i < 10000; ++i)
                    {
                        CASSERT(*test2[*tmp] == *tmp,"");
                        CASSERT(*test2[*tmp] == *tmp,"");
                        CASSERT(*test2[*tmp] == *tmp,"");
                        CASSERT(*const_cast<const bst&>(test2)[*tmp] == *tmp,"");
                        ++tmp;
                    }

                    tmp = array;
                    while (test2.size() > 0)
                    {
                        unsigned long count = test2.count(*tmp);
                        test2.destroy(*tmp);
                        CASSERT(test2.count(*tmp)+1 == count,"");
                        ++tmp;
                    }
                    
                    CASSERT(test2.at_start() == true,"");
                    CASSERT(test2.current_element_valid() == false,"");
                    CASSERT(test2.move_next() == false,"");
                    CASSERT(test2.at_start() == false,"");
                    test.swap(test2);
                    test.reset();

                    delete [] array;
                }


                CASSERT(test.size() == 0,"");
                CASSERT(test.height() == 0,"");

                for (unsigned long i = 1; i < 100; ++i)
                {
                    a = 1234;
                    test.add(a,b);
                    CASSERT(test.count(1234) == i,"");
                }

                test.clear();






                for (int m = 0; m < 3; ++m)
                {

                    test2.clear();

                    CASSERT(test2.current_element_valid() == false,"");
                    CASSERT(test2.at_start() == true,"");
                    CASSERT(test2.move_next() == false,"");
                    CASSERT(test2.at_start() == false,"");
                    CASSERT(test2.current_element_valid() == false,"");
                    CASSERT(test2.move_next() == false,"");
                    CASSERT(test2.current_element_valid() == false,"");
                    CASSERT(test2.move_next() == false,"");
                    CASSERT(test2.current_element_valid() == false,"");
                    CASSERT(test2.at_start() == false,"");


                    CASSERT(test2.size() == 0,"");
                    CASSERT(test2.height() == 0,"");
                    

                    int counter = 0;
                    while (counter < 10000)
                    {
                        a = ::rand()&0x7FFF;
                        b = ::rand()&0x7FFF;
                        if (test2[a] == 0)
                        {
                            test2.add(a,b);
                            ++counter;
                        }

                    }



                    CASSERT(test2.size() == 10000,"");



                    // remove a bunch of elements randomly                
                    for (int i = 0; i < 20000; ++i)
                    {
                        a = ::rand()&0x7FFF;
                        if (test2[a] != 0)
                        {
                            test2.remove(a,b,c);
                            CASSERT(a == b,"");
                        }
                    }


                    // now add a bunch more
                    for (int i = 0; i < 20000; ++i)
                    {
                        a = ::rand()&0x7FFF;
                        b = ::rand()&0x7FFF;
                        if (test2[a] == 0)
                            test2.add(a,b);
                    }


                    // now iterate over it all and then remove all elements
                    {
                        int* array = new int[test2.size()];
                        int* array_val = new int[test2.size()];
                        int* tmp = array;
                        int* tmp_val = array_val;
                        CASSERT(test2.at_start() == true,"");
                        int count = 0;
                        while (test2.move_next())
                        {
                            *tmp = test2.element().key();
                            ++tmp;
                            *tmp_val = test2.element().value();
                            ++tmp_val;

                            CASSERT(*test2[*(tmp-1)] == *(tmp_val-1),"");
                            ++count;
                        }

                        CASSERT(count == (int)test2.size(),"");
                        CASSERT(test2.at_start() == false,"");
                        CASSERT(test2.current_element_valid() == false,"");
                        CASSERT(test2.move_next() == false,"");

                        tmp = array;
                        tmp_val = array_val;
                        for (unsigned long i = 0; i < test2.size(); ++i)
                        {
                            CASSERT(*test2[*tmp] == *tmp_val,i);
                            CASSERT(*test2[*tmp] == *tmp_val,"");
                            CASSERT(*test2[*tmp] == *tmp_val,"");
                            CASSERT(*const_cast<const bst&>(test2)[*tmp] == *tmp_val,"");
                            ++tmp;
                            ++tmp_val;
                        }

                      //  out << "\nsize:   " << test2.size() << endl;
                      //  out << "height: " << test2.height() << endl;

                        tmp = array;
                        while (test2.size() > 0)
                        {
                            unsigned long count = test2.count(*tmp);
                            test2.destroy(*tmp);
                            CASSERT(test2.count(*tmp)+1 == count,"");
                            ++tmp;
                        }
                        
                        CASSERT(test2.at_start() == true,"");
                        CASSERT(test2.current_element_valid() == false,"");
                        CASSERT(test2.move_next() == false,"");
                        CASSERT(test2.at_start() == false,"");
                        test.swap(test2);
                        test.reset();

                        delete [] array;
                        delete [] array_val;
                    }


                    CASSERT(test.size() == 0,"");
                    CASSERT(test.height() == 0,"");

                    for (unsigned long i = 1; i < 100; ++i)
                    {
                        a = 1234;
                        test.add(a,b);
                        CASSERT(test.count(1234) == i,"");
                    }

                    test.clear();

                }


               
                a = 1;
                b = 2;

                test.add(a,b);

                test.position_enumerator(0);
                a = 0;
                b = 0;
                CASSERT(test.height() == 1,"");
                test.remove_current_element(a,b);
                CASSERT(a == 1, "");
                CASSERT(b == 2, "");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.height() == 0,"");
                CASSERT(test.size() == 0,"");

               
                a = 1;
                b = 2;
                test.add(a,b);
                a = 1;
                b = 2;
                test.add(a,b);

                test.position_enumerator(0);
                a = 0;
                b = 0;
                CASSERT(test.height() == 2,"");
                test.remove_current_element(a,b);
                CASSERT(a == 1, "");
                CASSERT(b == 2, "");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");
                CASSERT(test.height() == 1,"");
                CASSERT(test.size() == 1,"");

                test.remove_current_element(a,b);
                CASSERT(a == 1, "");
                CASSERT(b == 2, "");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.height() == 0,"");
                CASSERT(test.size() == 0,"");

                for (int i = 0; i < 100; ++i)
                {
                    a = i;
                    b = i;
                    test.add(a,b);
                }

                CASSERT(test.size() == 100,"");
                test.remove_last_in_order(a,b);
                CASSERT(a == 99, "");
                CASSERT(b == 99, "");
                CASSERT(test.size() == 99,"");
                test.remove_last_in_order(a,b);
                CASSERT(a == 98, "");
                CASSERT(b == 98, "");
                CASSERT(test.size() == 98,"");

                test.position_enumerator(-10);
                for (int i = 0; i < 97; ++i)
                {
                    CASSERT(test.element().key() == i, "");
                    CASSERT(test.element().value() == i, "");
                    CASSERT(test.move_next(), "");
                }
                CASSERT(test.move_next() == false,"");
                CASSERT(test.current_element_valid() == false,"");


                test.position_enumerator(10);
                for (int i = 10; i < 97; ++i)
                {
                    CASSERT(test.element().key() == i, "");
                    CASSERT(test.element().value() == i, "");
                    CASSERT(test.move_next(), "");
                }
                CASSERT(test.move_next() == false,"");
                CASSERT(test.current_element_valid() == false,"");

                test.reset();
                CASSERT(test.at_start(),"");
                CASSERT(test.current_element_valid() == false,"");
                for (int i = 0; i < 98; ++i)
                {
                    CASSERT(test.move_next(), "");
                    CASSERT(test.element().key() == i, "");
                    CASSERT(test.element().value() == i, "");
                }
                CASSERT(test.size() == 98, test.size());
                CASSERT(test.move_next() == false,"");

                test.position_enumerator(98);
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false,"");


                test.position_enumerator(50);
                CASSERT(test.element().key() == 50,"");
                CASSERT(test.element().value() == 50,"");
                CASSERT(test[50] != 0,"");
                test.remove_current_element(a,b);
                CASSERT(test[50] == 0,"");
                CASSERT(test.size() == 97, test.size());
                CASSERT(a == 50,"");
                CASSERT(b == 50,"");
                CASSERT(test.element().key() == 51,"");
                CASSERT(test.element().value() == 51,"");
                CASSERT(test.current_element_valid(),"");
                test.remove_current_element(a,b);
                CASSERT(test.size() == 96, test.size());
                CASSERT(a == 51,"");
                CASSERT(b == 51,"");
                CASSERT(test.element().key() == 52,test.element().key());
                CASSERT(test.element().value() == 52,test.element().value());
                CASSERT(test.current_element_valid(),"");
                test.remove_current_element(a,b);
                CASSERT(test.size() == 95, test.size());
                CASSERT(a == 52,"");
                CASSERT(b == 52,"");
                CASSERT(test.element().key() == 53,test.element().key());
                CASSERT(test.element().value() == 53,test.element().value());
                CASSERT(test.current_element_valid(),"");
                test.position_enumerator(50);
                CASSERT(test.element().key() == 53,test.element().key());
                CASSERT(test.element().value() == 53,test.element().value());
                CASSERT(test.current_element_valid(),"");
                test.position_enumerator(51);
                CASSERT(test.element().key() == 53,test.element().key());
                CASSERT(test.element().value() == 53,test.element().value());
                CASSERT(test.current_element_valid(),"");
                test.position_enumerator(52);
                CASSERT(test.element().key() == 53,test.element().key());
                CASSERT(test.element().value() == 53,test.element().value());
                CASSERT(test.current_element_valid(),"");
                test.position_enumerator(53);
                CASSERT(test.element().key() == 53,test.element().key());
                CASSERT(test.element().value() == 53,test.element().value());
                CASSERT(test.current_element_valid(),"");

                test.reset();
                test.move_next();
                int lasta = -1, lastb = -1;
                count = 0;
                while (test.current_element_valid() )
                {
                    ++count;
                    int c = test.element().key();
                    int d = test.element().value();
                    test.remove_current_element(a,b);
                    CASSERT(c == a,"");
                    CASSERT(d == a,"");
                    CASSERT(lasta < a,"");
                    CASSERT(lastb < b,"");
                    lasta = a;
                    lastb = b;
                }
                CASSERT(count == 95, count);
                CASSERT(test.size() == 0,"");
                CASSERT(test.height() == 0,"");

                test.clear();

                for (int i = 0; i < 1000; ++i)
                {
                    a = 1;
                    b = 1;
                    test.add(a,b);
                }

                for (int i = 0; i < 40; ++i)
                {
                    int num = ::rand()%800 + 1;
                    test.reset();
                    for (int j = 0; j < num; ++j)
                    {
                        CASSERT(test.move_next(),"");
                    }					             
					CASSERT(test.current_element_valid(),"size: " << test.size() << "   num: " << num);
                    test.remove_current_element(a,b);
					CASSERT(test.current_element_valid(),"size: " << test.size() << "   num: " << num);
                    test.remove_current_element(a,b);
                    test.position_enumerator(1);
                    if (test.current_element_valid())
                        test.remove_current_element(a,b);
                    CASSERT(a == 1,"");
                    CASSERT(b == 1,"");
                }

                test.clear();

            }


            test.clear();
            test2.clear();







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
        catch (std::bad_alloc e)
        {
            cout << "\n\nran out of memory in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ran out of memory in " << type;
            dlog << LWARN << e.what();
            return 1;
        }

    }

}

#endif // DLIB_BINARY_SEARCH_TREE_KERNEl_TEST_H_

