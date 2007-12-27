// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/interfaces/enumerable.h>
#include <dlib/array2d.h>
#include "tester.h"

namespace  
{
    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.array2d");

    template <
        typename array2d
        >
    int array2d_kernel_test (
        const std::string& type
    )
    /*!
        requires
            - array2d is an implementation of array2d/array2d_kernel_abstract.h 
              is instantiated with unsigned long 
        ensures
            - runs tests on array2d for compliance with the specs
            - returns 0 if no errors were found, 1 otherwise 
    !*/
    {        

        try 
        {
 
            srand(static_cast<unsigned int>(time(0)));

            array2d test,test2;

            long width, height;



            enumerable<unsigned long>& e = test;
            CASSERT(e.at_start() == true,"");


            CASSERT(e.size() == 0,"");
            CASSERT(e.at_start() == true,"");
            CASSERT(e.current_element_valid() == false, "");

            CASSERT (e.move_next() == false,"");
            CASSERT (e.move_next() == false,"");
            CASSERT (e.move_next() == false,"");
            CASSERT (e.move_next() == false,"");
            CASSERT (e.move_next() == false,"");
            CASSERT (e.move_next() == false,"");


            CASSERT(e.size() == 0,"");
            CASSERT(e.at_start() == false,"");
            CASSERT(e.current_element_valid() == false, "");


            e.reset();

            CASSERT(e.size() == 0,"");
            CASSERT(e.at_start() == true,"");
            CASSERT(e.current_element_valid() == false, "");



            

            CASSERT(test.at_start() == true,"");


            CASSERT(test.size() == 0,"");
            CASSERT(test.at_start() == true,"");
            CASSERT(test.current_element_valid() == false, "");

            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");


            CASSERT(test.size() == 0,"");
            CASSERT(test.at_start() == false,"");
            CASSERT(test.current_element_valid() == false, "");


            test.reset();

            CASSERT(test.size() == 0,"");
            CASSERT(test.at_start() == true,"");
            CASSERT(test.current_element_valid() == false, "");

            test.clear();


            CASSERT(test.at_start() == true,"");


            CASSERT(test.size() == 0,"");
            CASSERT(test.at_start() == true,"");
            CASSERT(test.current_element_valid() == false, "");

            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");
            CASSERT (test.move_next() == false,"");

            swap(test,test2);
            CASSERT (test2.at_start() == false,"");
            CASSERT (test2.current_element_valid() == false,"");
            CASSERT(test.at_start() == true,"");
            CASSERT(test.current_element_valid() == false, "");
            swap(test,test2);
            CASSERT(test2.at_start() == true,"");
            CASSERT(test2.current_element_valid() == false, "");


            CASSERT(test.size() == 0,"");
            CASSERT(test.at_start() == false,"");
            CASSERT(test.current_element_valid() == false, "");


            test.reset();

            CASSERT(test.size() == 0,"");
            CASSERT(test.at_start() == true,"");
            CASSERT(test.current_element_valid() == false, "");




            for (int j = 0; j < 30; ++j)
            {
                test2.clear();
                switch (j)
                {
                    case 0:
                        width = 10;
                        height = 11;
                        break;
                    case 1:
                        width = 1;
                        height = 1;
                        break;
                    case 2:
                        width = 100;
                        height = 1;
                        break;
                    case 3:
                        width = 1;
                        height = 100;
                        break;
                    default:
                        width = ::rand()%100 + 1;
                        height = ::rand()%100 + 1;
                        break;
                }

                test.set_size(width,height);

                CASSERT(test.size() == static_cast<unsigned long>(width*height),"");
                CASSERT(test.height() == height,"");
                CASSERT(test.width() == width,"");
                CASSERT(test.at_start() == true,"");
                CASSERT(test.current_element_valid() == false,"");

                unsigned long i = 0;
                while (test.move_next())
                {
                    CASSERT(test.current_element_valid() == true,"");
                    CASSERT(test.at_start() == false,"");
                    test.element() = i;
                    CASSERT(const_cast<const array2d&>(test).element() == i,"");
                    ++i;
                }
                CASSERT(i == test.size(),"");
                CASSERT(test.current_element_valid() == false,"");

                CASSERT(test.height() == height,"");
                CASSERT(test.width() == width,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.size() == static_cast<unsigned long>(width*height),"");

                i = 0;
                for (long row = 0; row < test.height(); ++row)
                {
                    for (long col = 0; col < test.width(); ++col)
                    {
                        CASSERT(test[row][col] == i,
                               "\n\trow: " << row <<
                               "\n\tcol: " << col <<
                               "\n\ti:   " << i <<
                               "\n\ttest[row][col]: " << test[row][col]);
                        CASSERT(test[row].width() == test.width(),"");
                        CASSERT(test.current_element_valid() == false,"");

                        CASSERT(test.height() == height,"");
                        CASSERT(test.width() == width,"");
                        CASSERT(test.at_start() == false,"");
                        CASSERT(test.size() == static_cast<unsigned long>(width*height),"");
                        ++i;
                    }
                }

                test.reset();

                i = 0;
                while (test.move_next())
                {
                    CASSERT(test.element() == i,"");
                    ++i;
                    CASSERT(test.current_element_valid() == true,"");
                    CASSERT(test.at_start() == false,"");
                }
                CASSERT(i == test.size(),"");

                test.reset();




                swap(test,test2);

                CASSERT(test2.size() == static_cast<unsigned long>(width*height),"");
                CASSERT(test2.height() == height,"");
                CASSERT(test2.width() == width,"");
                CASSERT(test2.at_start() == true,"");
                CASSERT(test2.current_element_valid() == false,"");

                i = 0;
                while (test2.move_next())
                {
                    CASSERT(test2.current_element_valid() == true,"");
                    CASSERT(test2.at_start() == false,"");
                    test2.element() = i;
                    ++i;
                }
                CASSERT(i == test2.size(),"");
                CASSERT(test2.current_element_valid() == false,"");

                CASSERT(test2.height() == height,"");
                CASSERT(test2.height() == test2.nr(),"");
                CASSERT(test2.width() == width,"");
                CASSERT(test2.width() == test2.nc(),"");
                CASSERT(test2.at_start() == false,"");
                CASSERT(test2.size() == static_cast<unsigned long>(width*height),"");

                i = 0;
                for (long row = 0; row < test2.height(); ++row)
                {
                    for (long col = 0; col < test2.width(); ++col)
                    {
                        CASSERT(test2[row][col] == i,"");
                        CASSERT(const_cast<const array2d&>(test2)[row][col] == i,"");
                        CASSERT(test2[row].width() == test2.width(),"");
                        CASSERT(test2.current_element_valid() == false,"");

                        CASSERT(test2.height() == height,"");
                        CASSERT(test2.height() == test2.nr(),"");
                        CASSERT(test2.width() == width,"");
                        CASSERT(test2.width() == test2.nc(),"");
                        CASSERT(test2.at_start() == false,"");
                        CASSERT(test2.size() == static_cast<unsigned long>(width*height),"");
                        ++i;
                    }
                }

                test2.reset();

                i = 0;
                while (test2.move_next())
                {
                    CASSERT(test2.element() == i,"");
                    CASSERT(const_cast<const array2d&>(test2).element() == i,"");
                    ++i;
                    CASSERT(test2.current_element_valid() == true,"");
                    CASSERT(test2.at_start() == false,"");
                }
                CASSERT(i == test2.size(),"");


                test2.clear();
                CASSERT(test2.size() == 0,"");
                CASSERT(test2.height() == 0,"");
                CASSERT(test2.width() == 0,"");
                CASSERT(test2.current_element_valid() == false,"");
                CASSERT(test2.at_start() == true,"");

                CASSERT(test.size() == 0,"");
                CASSERT(test.width() == 0,"");
                CASSERT(test.height() == 0,"");

                test.set_size(width,height);
                CASSERT(test.size() == static_cast<unsigned long>(width*height),"");
                CASSERT(test.width() == width,"");
                CASSERT(test.height() == height,"");

                

            }





            // test the serialization
            istringstream sin;
            ostringstream sout;
            test.clear();
            test2.clear();

            CASSERT(test.size() == 0,"");
            CASSERT(test.width() == 0,"");
            CASSERT(test.height() == 0,"");
            CASSERT(test2.size() == 0,"");
            CASSERT(test2.width() == 0,"");
            CASSERT(test2.height() == 0,"");
            
            test.set_size(10,10);

            for (long row = 0; row < test.height(); ++row)
            {
                for (long col = 0; col < test.width(); ++col)
                {
                    test[row][col] = row*col;
                }
            }

            serialize(test,sout);
            sin.str(sout.str());
            deserialize(test2,sin);

            CASSERT(test2.size() == test.size(),"");
            CASSERT(test2.width() == test.width(),"");
            CASSERT(test2.height() == test.height(),"");
            CASSERT(test2.size() == 100,"");
            CASSERT(test2.width() == 10,"");
            CASSERT(test2.height() == 10,"");


            for (long row = 0; row < test.height(); ++row)
            {
                for (long col = 0; col < test.width(); ++col)
                {
                    CASSERT(test[row][col] == static_cast<unsigned long>(row*col),"");
                    CASSERT(test2[row][col] == static_cast<unsigned long>(row*col),"");
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
    }





    class array2d_tester : public tester
    {
    public:
        array2d_tester (
        ) :
            tester ("test_array2d",
                    "Runs tests on the array2d component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += array2d_kernel_test<array2d<unsigned long>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += array2d_kernel_test<array2d<unsigned long>::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}


