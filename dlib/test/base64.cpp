// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/base64.h>

#include "tester.h"

namespace  
{
    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.base64");

    template <
        typename base64
        >
    int base64_kernel_test (
        const string& type
    )
    /*!
        requires
            - base64 is an implementation of base64/base64_kernel_abstract.h 
        ensures
            - runs tests on base64 for compliance with the specs
            - returns 0 if no errors were found, 1 otherwise.
    !*/
    {        

        const unsigned int seed = static_cast<unsigned int>(time(0));
        try 
        {
 
            srand(seed);

            base64 test;

            const string wiki_normal = "\
Man is distinguished, not only by his reason, but by this singular passion from other \
animals, which is a lust of the mind, that by a perseverance of delight in the continued \
and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.";

            const string wiki_encoded = "\
TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0\n\
aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1\n\
c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0\n\
aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdl\n\
LCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=";



            string str;

            istringstream sin;
            ostringstream sout;

            sin.str(wiki_encoded);
            test.decode(sin,sout);
            CASSERT(sout.str() == wiki_normal,
                   "sout.str(): " << sout.str() <<
                   "\nwiki_normal: " << wiki_normal);


            sout.str("");
            sin.str(wiki_normal);
            sin.clear();
            test.encode(sin,sout);

            string a(sout.str()), b(wiki_encoded);
            // we want to strip all the whitespace from a and b now
            sin.str(a);
            a.clear();
            sin >> str;
            while (sin)
            {
                a += str;
                sin >> str;
            }

            sin.clear();
            sin.str(b);
            b.clear();
            sin >> str;
            while (sin)
            {
                b += str;
                sin >> str;
            }
            sin.clear();

            CASSERT(a == b,
                   "a: \n" << a <<
                   "\n\nb: \n" << b);



            sin.clear();
            sin.str("");
            sout.str("");
            test.encode(sin,sout);
            sin.str(sout.str());
            sout.str("");
            test.decode(sin,sout);
            CASSERT(sout.str() == "","");

            sin.clear();
            sin.str("a");
            sout.str("");
            test.encode(sin,sout);
            sin.str(sout.str());
            sout.str("");
            test.decode(sin,sout);
            CASSERT(sout.str() == "a","");

            sin.clear();
            sin.str("da");
            sout.str("");
            test.encode(sin,sout);
            sin.str(sout.str());
            sout.str("");
            test.decode(sin,sout);
            CASSERT(sout.str() == "da","");

            sin.clear();
            sin.str("dav");
            sout.str("");
            test.encode(sin,sout);
            sin.str(sout.str());
            sout.str("");
            test.decode(sin,sout);
            CASSERT(sout.str() == "dav","");

            sin.clear();
            sin.str("davi");
            sout.str("");
            test.encode(sin,sout);
            sin.str(sout.str());
            sout.str("");
            test.decode(sin,sout);
            CASSERT(sout.str() == "davi","");


            for (int i = 0; i < 1000; ++i)
            {
                str.clear();
                sin.clear();
                sout.str("");
                sin.str("");
                
                // fill str with random garbage
                const int size = rand()%2000;
                for (int j = 0; j < size; ++j)
                {
                    unsigned char ch = rand()&0xFF;
                    str += ch;
                }

                sin.str(str);
                test.encode(sin,sout);
                sin.clear();
                sin.str(sout.str());
                sout.str("");
                test.decode(sin,sout);

                CASSERT(str == sout.str(),"");


            }



            return 0;

        }
        catch (typename base64::decode_error& e)
        {
            cout << "\n\nERRORS FOUND (decode_error thrown when it shouldn't have been) (" << seed << ") in " 
                 << type << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND (decode_error thrown when it shouldn't have been) (" << seed << ") in " 
                 << type;
            dlog << LWARN << e.info;
            return 1;
        }
        catch(error e)
        {
            cout << "\n\nERRORS FOUND (" << seed << ") in " << type << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND (" << seed << ") in " << type;
            dlog << LWARN << e.info;
            return 1;
        }        
    }


    class base64_tester : public tester
    {
    public:
        base64_tester (
        ) :
            tester ("test_base64",
                    "Runs tests on the base64 component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            print_spinner();
            n += base64_kernel_test<base64::kernel_1a>    ("kernel_1a");
            return (n == 0);
        }
    } a;



}



