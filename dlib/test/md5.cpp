// Copyright (C) 2007  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <dlib/md5.h>
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

    logger dlog("test.md5");

    int md5_test (
    )
    /*!
        ensures
            - runs tests on the md5 stuff compliance with the specs
            - returns 0 if there aren't any errors, 1 otherwise
    !*/
    {        

        try 
        {
            CASSERT(md5 ("") == "d41d8cd98f00b204e9800998ecf8427e","");
            CASSERT(md5 ("a") == "0cc175b9c0f1b6a831c399e269772661","");
            CASSERT(md5 ("abc") == "900150983cd24fb0d6963f7d28e17f72","");
            CASSERT(md5 ("message digest") == "f96b697d7cb7938d525a2f31aaf161d0","");
            CASSERT(md5 ("abcdefghijklmnopqrstuvwxyz") == "c3fcd3d76192e4007dfb496cca67e13b","");
            CASSERT(md5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") == "d174ab98d277d9f5a5611c2c9f419d9f","");
            CASSERT(md5 ("12345678901234567890123456789012345678901234567890123456789012345678901234567890") == "57edf4a22be3c955ac49da2e2107b67a","");


            return 0;
        }
        catch(error e)
        {
            cout << "\n\nERRORS FOUND in md5" << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in md5";
            dlog << LWARN << e.info;
            return 1;
        }        
    }


    class md5_tester : public tester
    {
    public:
        md5_tester (
        ) :
            tester ("test_md5",
                    "Runs tests on the md5 component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += md5_test();
            print_spinner();
            return (n == 0);
        }
    } a;

}



