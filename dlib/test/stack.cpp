// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/stack.h>

#include "tester.h"

namespace  
{

    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.stack");

    template <
        typename stack
        >
    int stack_kernel_test (
        const string& type
    )
    /*!
        requires
            - stack is an implementation of stack/stack_sort_abstract.h 
              stack is instantiated with int
        ensures
            - runs tests on stack for compliance with the specs
            - returns 0 if there weren't any errors, returns 1 otherwise.
    !*/
    {        

        try 
        {
 
            srand(static_cast<unsigned int>(time(0)));

     
            stack a1, a2;

          

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


                for (unsigned long i = 0; i < 100; ++i)
                {
                    int a = (int)i;
                    a1.push(a);
                }

                CASSERT(a1.size() == 100,"");

                int count = 99;
                while (a1.move_next())
                {
                    CASSERT(a1.element() == count,a1.element() << " : " << count);
                    --count;
                }

                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.at_start() == false,"");

                a1.swap(a2);

                count = 99;
                CASSERT(a2.current_element_valid() == false,"");
                CASSERT(a2.at_start() == false,"");
                CASSERT(a1.current_element_valid() == false,"");
                CASSERT(a1.at_start() == true,"");

                CASSERT(a1.size() == 0,"");
                CASSERT(a2.size() == 100,"");
                CASSERT(a2.current() == 99,"");

                a2.reset();
                while (a2.move_next())
                {
                    CASSERT(a2.element() == count--,"");
                }                

                CASSERT(a2.current_element_valid() == false,"");
                CASSERT(a2.at_start() == false,"");
                int b = 4;
                a2.push(b);
                CASSERT(a2.current_element_valid() == false,"");
                CASSERT(a2.at_start() == true,"");

                CASSERT(a2.current() == 4,"");
                int c = 0;
                a2.pop(c);
                CASSERT(c == 4,"");

                // serialize the state of a2, then clear a2, then
                // load the state back into a2.
                ostringstream sout;
                serialize(a2,sout);
                CASSERT(a2.at_start() == true,"");
                istringstream sin(sout.str());
                a2.clear();
                deserialize(a2,sin);


                count = 99;
                while (a2.size())
                {
                    int a = 0;
                    CASSERT(a2.current() == count,"");
                    CASSERT(const_cast<const stack&>(a2).current() == count,"");
                    a2.pop(a);
                    CASSERT(a == count--,"");
                }
 


    


                a1.clear();
                a2.clear();
            }


            {
                a1.clear();
                remover<int>& go = a1;
                for (int i = 0; i < 100; ++i)
                {
                    int a = 3;
                    a1.push(a);
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




    class stack_tester : public tester
    {
    public:
        stack_tester (
        ) :
            tester ("test_stack",
                    "Runs tests on the stack component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += stack_kernel_test<stack<int>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += stack_kernel_test<stack<int>::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

