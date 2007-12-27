// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <dlib/matrix.h>
#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include "tester.h"

namespace  
{

    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.matrix");

    int matrix_test (
    )
    /*!
        ensures
            - runs tests on the matrix stuff compliance with the specs
            - returns 0 if there aren't any errors, 1 otherwise
    !*/
    {        

        try 
        {
            const double ident[] = {
                1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                0, 0, 0, 1 };

            const double uniform3[] = {
                3, 3, 3, 3,
                3, 3, 3, 3,
                3, 3, 3, 3,
                3, 3, 3, 3 
            };

            const double uniform1[] = {
                1, 1, 1, 1,
                1, 1, 1, 1,
                1, 1, 1, 1,
                1, 1, 1, 1 
            };

            const double uniform0[] = {
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0 
            };

            const int array[] = {
                42, 58, 9, 1,
                9, 5, 8, 2,
                98, 28, 4, 77, 
                9, 2, 44, 88 };

            const int array2[] = {
                1, 22, 3,
                4, 52, 6,
                7, 8, 9 };

            const int array2_r[] = {
                52, 6, 4,
                8, 9, 7,
                22, 3, 1
            };

            const int array3[] = { 1, 2, 3, 4 };

            matrix<double,3,3> m3(array2);

            matrix<int,3,3> m4, m5, m6;
            set_all_elements(m4, 4);
            set_all_elements(m5, 4);
            set_all_elements(m6, 1);
            CASSERT(m4 == m5,"");
            CASSERT(m6 != m5,"");
            m4.swap(m6);
            CASSERT(m6 == m5,"");
            CASSERT(m4 != m5,"");

            CASSERT(m3.nr() == 3,"");
            CASSERT(m3.nc() == 3,"");

            matrix<double,4,1> v(array3), v2;
            CASSERT(v.nr() == 4,"");
            CASSERT(v.nc() == 1,"");


            CASSERT(v(0) == 1,"");
            CASSERT(v(1) == 2,"");
            CASSERT(v(2) == 3,"");
            CASSERT(v(3) == 4,"");
            CASSERT((trans(v)*v).nr() == 1,"");
            CASSERT((trans(v)*v).nc() == 1,"");
            CASSERT((trans(v)*v)(0) == 1*1 + 2*2 + 3*3 + 4*4,"");

            CASSERT(floor(det(m3)+0.01) == -444,"");
            CASSERT(min(m3) == 1,"");
            CASSERT(max(m3) == 52,"");
            CASSERT(sum(m3) == 112,"");
            CASSERT(prod(m3) == 41513472,"");
            CASSERT(prod(diag(m3)) == 1*52*9,"");
            CASSERT(sum(diag(m3)) == 1+52+9,"");
            CASSERT((round(m3*inv(m3)) == identity_matrix<double,3>()),"");
            CASSERT(-1*m3 == -m3,"");

            matrix<double,4,4> mident(ident);
            matrix<double,4,4> muniform0(uniform0);
            matrix<double,4,4> muniform1(uniform1);
            matrix<double,4,4> muniform3(uniform3);
            matrix<double,4,4> m1(array), m2;
            CASSERT(m1.nr() == 4,"");
            CASSERT(m1.nc() == 4,"");

            CASSERT(muniform1 + muniform1 + muniform1 == muniform3,"");
            CASSERT(muniform1*2 + muniform1 + muniform1 - muniform1 == muniform3,"");
            CASSERT(2*muniform1 + muniform1 + muniform1 - muniform1 == muniform3,"");
            CASSERT(muniform1 + muniform1 + muniform1 - muniform3 == muniform0,"");
            CASSERT(muniform3/3 == muniform1,"");
            CASSERT(v != m1,"");
            CASSERT(v == v,"");
            CASSERT(m1 == m1,"");

            muniform0.swap(muniform1);
            CASSERT((muniform1 == matrix_cast<double>(uniform_matrix<long,4,4,0>())),"");
            CASSERT((muniform0 == matrix_cast<double>(uniform_matrix<long,4,4,1>())),"");
            swap(muniform0,muniform1);

            CASSERT((mident == identity_matrix<double,4>()),"");
            CASSERT((muniform0 == matrix_cast<double>(uniform_matrix<long,4,4,0>())),"");
            CASSERT((muniform1 == matrix_cast<double>(uniform_matrix<long,4,4,1>())),"");
            CASSERT((muniform3 == matrix_cast<double>(uniform_matrix<long,4,4,3>())),"");
            CASSERT((muniform1*8 == matrix_cast<double>(uniform_matrix<long,4,4,8>())),"");

            set_all_elements(m2,7);
            CASSERT(m2 == muniform1*7,"");
            m2 = array;
            CASSERT(m2 == m1,"");

            const double m1inv[] = {
                -0.00946427624, 0.0593272941,  0.00970564379,  -0.00973323731, 
                0.0249312057,   -0.0590122427, -0.00583102756, 0.00616002729, 
                -0.00575431149, 0.110081189,   -0.00806792253, 0.00462297692, 
                0.00327847478,  -0.0597669712, 0.00317386196,  0.00990759201 
            };

            m2 = m1inv;
            CASSERT((round(m2*m1) == identity_matrix<double,4>()),"");

            CASSERT(round(m2*10000) == round(inv(m1)*10000),"");
            CASSERT(m1 == abs(-1*m1),"");
            CASSERT(abs(m2) == abs(-1*m2),"");

            CASSERT(floor(det(m1)+0.01) == 3297875,"\nm1: \n" << m1 << "\ndet(m1): " << det(m1));


            ostringstream sout;
            m1 = m2;
            serialize(m1,sout);
            set_all_elements(m1,0);
            istringstream sin(sout.str());
            deserialize(m1,sin);
            CASSERT(round(100000*m1) == round(100000*m2),"m1: \n" << m1 << endl << "m2: \n" << m2);
            

            set_all_elements(v,2);
            v2 =  pointwise_multiply(v, v*2);
            set_all_elements(v,8);
            CASSERT(v == v2,"");
            CASSERT(v == tmp(v2),"");
            CASSERT((v == rotate<2,0>(v)),""); 

            m4 = array2;
            m5 = array2_r;
            CASSERT((m5 == rotate<1,1>(m4)),"");

            m5 = array2;
            CASSERT((m5*2 == pointwise_multiply(m5,uniform_matrix<int,3,3,2>())),"");
            CASSERT((tmp(m5*2) == tmp(pointwise_multiply(m5,uniform_matrix<int,3,3,2>()))),"");

            v = tmp(v);


            return 0;
        }
        catch(error e)
        {
            cout << "\n\nERRORS FOUND in matrix" << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in matrix";
            dlog << LWARN << e.info;
            return 1;
        }        
    }






    class matrix_tester : public tester
    {
    public:
        matrix_tester (
        ) :
            tester ("test_matrix",
                    "Runs tests on the matrix component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += matrix_test();
            print_spinner();
            return (n == 0);
        }
    } a;

}


