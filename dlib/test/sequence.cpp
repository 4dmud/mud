// Copyright (C) 2004  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/sequence.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.sequence");

    template <
        typename seq
        >
    int sequence_sort_test (
        const string& type 
    )
    /*!
        requires
            - seq is an implementation of sequence/sequence_sort_aseqract.h is instantiated 
              with int
        ensures
            - runs tests on seq for compliance with the specs 
            - returns 0 if no errors were detected, 1 otherwise.
    !*/
    {        

        try 
        {
 
            srand(static_cast<unsigned int>(time(0)));

            





            {
                // this test is to make sure that jumping around via
                // operator[] doesn't corrupt the object

                seq a;

                for (int i = 0; i < 100; ++i)
                {
                    int x = i;
                    a.add(a.size(),x);
                }

           
                int x;

                for (int i = 0; i < (int)a.size(); ++i)
                {
                    CASSERT(a[i] >= i,"1");
                // cout << a[i] << endl;
                }

                for (unsigned long i = 0; i < a.size(); ++i)
                {
                    for (unsigned long j = i+1; j < a.size(); ++j)
                    {
                        if ((a[j]+a[i])%3 ==0)
                        {                    
                            a.remove(j,x);
                            --j;
                        }
                    }
                }

            //cout << endl;

                for (int i = 0; i < (int)a.size(); ++i)
                {
                //   cout << a[i] << endl;
                CASSERT(a[i] >= i,"2");               
                }

            }







            seq test, test2;

            CASSERT(test.size() == 0,"");
            CASSERT(test.at_start() == true,"");
            CASSERT(test.current_element_valid() == false,"");

            enumerable<int>& e = test;
    
            CASSERT(e.at_start() == true,"");
            CASSERT(e.current_element_valid() == false,"");


            for (int g = 0; g < 5; ++g)
            {
                test.clear();
                test2.clear();
                CASSERT(test.size() == 0,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(e.at_start() == true,"");
                CASSERT(e.current_element_valid() == false,"");

                CASSERT(e.move_next() == false,"");
                CASSERT(e.current_element_valid() == false,"");
                CASSERT(e.at_start() == false,"");
                CASSERT(test.at_start() == false,"");
                swap(test,test2);
                CASSERT(test.at_start() == true,"");
                test.clear();
                test2.clear();

                int a;


                for (int i = 0; i < 100; ++i)
                {
                    a = i;
                    test.add(i,a);
                }

                CASSERT(test.size() == 100,"");

                for (int i = 0; i < static_cast<int>(test.size()); ++i)
                {       
                    CASSERT(test[i] == i,"");
                }   

                swap(test,test2);

                a = 0;
                CASSERT(test2.at_start() == true,"");
                while(test2.move_next())
                {
                    CASSERT(test2.at_start() == false,"");
                    CASSERT(test2.current_element_valid() == true,"");
                    CASSERT(test2.element() == a,"");
                    ++a;
                }
                
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == false,"");
                
                test2.reset();

                CASSERT(test2.at_start() == true,"");
                CASSERT(test2.current_element_valid() == false,"");

                a = 0;
                while(test2.move_next())
                {
                    CASSERT(test2.at_start() == false,"");
                    CASSERT(test2.current_element_valid() == true,"");
                    CASSERT(test2.element() == a,"");
                    ++a;
                }





                for (int i = 0; i < 1000; ++i)
                {
                    a = ::rand();
                    test.add(0,a);
                }
                CASSERT(test.size() == 1000,"");

                test.sort();

                
                for (unsigned long i = 0; i < test.size()-1; ++i)
                {
                    CASSERT(test[i] <= test[i+1],"");    
                }

                a = 0;
                while(test.move_next())
                {
                    CASSERT(a <= test.element(),"");
                    a = test.element();
                }


                test.clear();
                test2.clear();

                CASSERT(test.size() == 0,"");
                CASSERT(test2.size() == 0,"");

                for (int i = 0; i < 100; ++i)
                {
                    a = i;
                    test.add(i,a);
                }

                for (int i = 100; i < 200; ++i)
                {
                    a = i;
                    test.add(i,a);
                }

                test.cat(test2);
                CASSERT(test.size() == 200,"");
                CASSERT(test2.size() == 0,"");


                // serialize the state of test, then clear test, then
                // load the state back into test.
                ostringstream sout;
                serialize(test,sout);
                CASSERT(test.at_start() == true,"");
                istringstream sin(sout.str());
                test.clear();
                deserialize(test,sin);


                for (int i = 0; i < 200; ++i)
                {
                    CASSERT(test[i] == i,"");
                }

                a = 0;
                while (test.move_next())
                {
                    CASSERT(test.element() == a,"");
                    CASSERT(test[0]==0,"");
                    ++a;
                }

                CASSERT(a == 200,"");

                CASSERT(test[9] == 9,"");
                test.remove(9,a);
                CASSERT(a == 9,"");
                CASSERT(test[9] == 10,"");
                CASSERT(test.size() == 199,"");

                test.remove(0,a);
                CASSERT(test[0] == 1,"");
                CASSERT(test.size() == 198,"");
                CASSERT(a == 0,"");
                CASSERT(test[9] == 11,"");
                CASSERT(test[20] == 22,"");




            }

            {
                test.clear();
                for (int i = 0; i < 100; ++i)
                {
                    int a = 3;
                    test.add(0,a);
                }
                CASSERT(test.size() == 100,"");
                remover<int>& go = test;
                for (int i = 0; i < 100; ++i)
                {
                    int a = 9;
                    go.remove_any(a);
                    CASSERT(a == 3,"");
                }
                CASSERT(go.size() == 0,"");
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




    class sequence_tester : public tester
    {
    public:
        sequence_tester (
        ) :
            tester ("test_sequence",
                    "Runs tests on the sequence component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += sequence_sort_test<sequence<int>::sort_1a>    ("sort_1a");
            print_spinner();
            n += sequence_sort_test<sequence<int>::sort_1a_c>  ("sort_1a_c");
            print_spinner();
            n += sequence_sort_test<sequence<int>::sort_2a>    ("sort_2a");
            print_spinner();
            n += sequence_sort_test<sequence<int>::sort_2a_c>  ("sort_2a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

