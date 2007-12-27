// Copyright (C) 2004  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <ctime>
#include <cstdlib>

#include <dlib/sliding_buffer.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.sliding_buffer");

    template <
        typename buf
        >
    int sliding_buffer_kernel_test (
        const string& type
    )
    /*!
        requires
            - buf is an implementation of sliding_buffer/sliding_buffer_kernel_abstract.h 
            - buf is instantiated with T=unsigned char
        ensures
            - runs tests on buf for compliance with the specs 
            - returns 0 if no error was detected, 1 otherwise
    !*/
    {        

        try 
        {
         
            buf test;

            CASSERT(test.size() == 0,"");

            test.set_size(3);
            buf test2;

            CASSERT(test.size() == 8,"");

            for (int g = 0; g < 2; ++g)
            {

                test.clear();

                CASSERT(test.size() == 0,"");
                test.set_size(2);

                CASSERT(test.size() == 4,"");



                test[0] = 'a';
                test[1] = 's';
                test[2] = 'd';
                test[3] = 'f';

                unsigned long id = test.get_element_id(2);
                CASSERT(test[test.get_element_index(id)] == 'd',"");


                CASSERT(test[0] == 'a',"");
                CASSERT(test[1] == 's',"");
                CASSERT(test[2] == 'd',"");
                CASSERT(test[3] == 'f',"");

                CASSERT(test2.size() == 0,"");
                swap(test,test2);
                CASSERT(test2.size() == 4,"");

                CASSERT(test2[0] == 'a',"");
                CASSERT(test2[1] == 's',"");
                CASSERT(test2[2] == 'd',"");
                CASSERT(test2[3] == 'f',"");

                swap(test,test2);

                test.rotate_left(4);

                
                CASSERT(test[test.get_element_index(id)] == 'd',"");

                CASSERT(test[0] == 'a',"");
                CASSERT(test[1] == 's',"");
                CASSERT(test[2] == 'd',"");
                CASSERT(test[3] == 'f',"");

                test.rotate_right(1);

                CASSERT(test[test.get_element_index(id)] == 'd',"");

                CASSERT(test[0] == 's',"");
                CASSERT(test[1] == 'd',"");
                CASSERT(test[2] == 'f',"");
                CASSERT(test[3] == 'a',"");   


                test.rotate_left(1);

                CASSERT(test[test.get_element_index(id)] == 'd',"");
                CASSERT(test[0] == 'a',"");
                CASSERT(test[1] == 's',"");
                CASSERT(test[2] == 'd',"");
                CASSERT(test[3] == 'f',"");


                test.rotate_left(16);

                CASSERT(test[test.get_element_index(id)] == 'd',"");
                CASSERT(test[0] == 'a',"");
                CASSERT(test[1] == 's',"");
                CASSERT(test[2] == 'd',"");
                CASSERT(test[3] == 'f',"");


                test.rotate_left(2);

                CASSERT(test[test.get_element_index(id)] == 'd',"");

                CASSERT(test[0] == 'd',"");
                CASSERT(test[1] == 'f',"");
                CASSERT(test[2] == 'a',"");
                CASSERT(test[3] == 's',"");

                test.rotate_left(1);

                CASSERT(test[test.get_element_index(id)] == 'd',"");
                CASSERT(test[0] == 's',"");
                CASSERT(test[1] == 'd',"");
                CASSERT(test[2] == 'f',"");
                CASSERT(test[3] == 'a',"");

                test.rotate_left(1);

                CASSERT(test[test.get_element_index(id)] == 'd',"");
                CASSERT(test[0] == 'a',"");
                CASSERT(test[1] == 's',"");
                CASSERT(test[2] == 'd',"");
                CASSERT(test[3] == 'f',"");

                CASSERT(test.size() == 4,"");

                test[0] = 'x';

                CASSERT(test[0] == 'x',"");
                CASSERT(test[1] == 's',"");
                CASSERT(test[2] == 'd',"");
                CASSERT(test[3] == 'f',"");

                test.rotate_left(1);

                CASSERT(test[0] == 'f',test[0]);
                CASSERT(test[1] == 'x',"");
                CASSERT(test[2] == 's',"");
                CASSERT(test[3] == 'd',"");


                test[0] = 'x';
                
                CASSERT(test[0] == 'x',"");
                CASSERT(test[1] == 'x',"");
                CASSERT(test[2] == 's',"");
                CASSERT(test[3] == 'd',"");


                test.rotate_left(1);


                CASSERT(test[0] == 'd',"");
                CASSERT(test[1] == 'x',"");
                CASSERT(test[2] == 'x',"");
                CASSERT(test[3] == 's',"");



                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");

                test.clear();
                test2.clear();


                CASSERT(test.size() == 0,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == false,"");

                swap(test,test2);

                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.move_next() == false,"");
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.current_element_valid() == false,"");


                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.at_start() == false,"");

                test.set_size(3);
                CASSERT(test.size() == 8,"");

                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");
                test.reset();
                CASSERT(test.size() == 8,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");


                test.rotate_right(1);
                CASSERT(test.size() == 8,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");

                test.rotate_left(1);
                CASSERT(test.size() == 8,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.move_next() == true,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.current_element_valid() == true,"");
                test.reset();


                for (unsigned long i = 0; i < test.size(); ++i)
                {
                    test[i] = static_cast<unsigned char>(i);
                }

                unsigned long count = 0;
                while (test.move_next())
                {
                    CASSERT(test.element() == count,"");
                    ++count;
                }

                CASSERT(count == test.size(),"");


                test2.clear();
                ostringstream sout;
                istringstream sin;

                serialize(test,sout);
                sin.str(sout.str());
                deserialize(test2,sin);

                char ch;
                sin >> ch;
                CASSERT( !sin, "");

                CASSERT(test2.size() == test.size(),"");


                for (unsigned long i = 0; i < test.size(); ++i)
                {
                    CASSERT(test[i] == test2[i],
                           "\ni:        " << i <<
                           "\ntest[i]:  " << test[i] <<
                           "\ntest2[i]: " << test2[i]);
                }

                count = 0;
                while (test.move_next() && test2.move_next())
                {
                    CASSERT(test.element() == count,"");
                    CASSERT(test2.element() == count,"");
                    ++count;
                }

                CASSERT(test2.size() == count,"");
                CASSERT(test.size() == count,"");

                test2.clear();


            } // for (int g = 0; g < 2; ++g)





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
        catch (exception& e)
        {
            cout << "\n\nERROR OCCURRED (probably ran out of memory) in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERROR OCCURRED (probably ran out of memory) in " << type;
            dlog << LWARN << e.what();
            return 1;
        }
    }







    class sliding_buffer_tester : public tester
    {
    public:
        sliding_buffer_tester (
        ) :
            tester ("test_sliding_buffer",
                    "Runs tests on the sliding_buffer component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += sliding_buffer_kernel_test<sliding_buffer<unsigned char>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += sliding_buffer_kernel_test<sliding_buffer<unsigned char>::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

