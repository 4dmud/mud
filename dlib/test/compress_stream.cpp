// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <ctime>
#include <cstdlib>

#include <dlib/compress_stream.h>

#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.compress_stream");

    template <
        typename cs
        >
    int compress_stream_kernel_test (
        const string& type,
        unsigned long seed
    )
    /*!
        requires
            - cs is an implementation of compress_stream/compress_stream_kernel_abstract.h            
              the alphabet_size for cc is 256
        ensures
            - runs tests on cs for compliance with the specs 
            - returns 0 if no errors were found, 1 otherwise.
    !*/
    {        

        try 
        {

            srand(seed);

            cs test;
           

    
            for (int i = 0; i < 3; ++i)
            {
                print_spinner();
                istringstream sin;
                ostringstream sout;
                string buffer;
                buffer.reserve(1000000);
                // fill sin with a bunch of random data in the range 0 to 63
                for (int i = 0; i < 1000000; ++i)
                {
                    char temp = static_cast<char>(::rand()&0x3f);
                    buffer.push_back(temp);
                }

                print_spinner();
                sin.str(buffer);

                test.compress(sin,sout);
                buffer = sout.str();

                print_spinner();
                // corrput the data in buffer
                buffer[buffer.size()/2]++;

                sin.str(buffer);
                sout.str("");

                bool detected_error = false;
                try {
                test.decompress(sin,sout);
                } catch ( typename cs::decompression_error e )
                {
                    detected_error = true;
                }

                CASSERT(detected_error,(unsigned int)sout.str().size());



            } /**/



            for (int j = 0; j < 3; ++j)
            {

                print_spinner();
                istringstream sin;
                ostringstream sout;

                string buffer;

                buffer.reserve(10);
                
                // make sure a single char can be compressed and decompressed
                for (int i = 0; i < 256; ++i)
                {
                    sin.str("");
                    sout.str("");
                    char ch = static_cast<char>(i);
                    buffer = ch;
                    sin.str(buffer);

                    test.compress(sin,sout);
                    
                    sin.str(sout.str());
                    sout.str("");
                    test.decompress(sin,sout);
                    CASSERT(sout.str() == buffer,"");                   
                }

                print_spinner();

                // make sure you can compress a single char, then append a new
                // compressed single char.  and make sure you can decode the
                // two streams.  Just to make sure the decoder doesn't leave 
                // extra bytes behind or eat more than it should.
                for (int i = 0; i < 2000; ++i)
                {
                    sin.str("");
                    sin.clear();
                    sout.str("");
                    sout.clear();
                    char ch = static_cast<char>(::rand()%256);
                    char ch2 = static_cast<char>(::rand()%256);

                    buffer = ch;
                    sin.str(buffer);
                    
                    

                    test.compress(sin,sout);

                    
                    

                    buffer = ch2;
                    sin.str(buffer);
                    test.compress(sin,sout);

                    sin.str(sout.str());

                    sout.str("");
                    test.decompress(sin,sout);
                    buffer = ch;
                    CASSERT(sout.str() == buffer,"");

                    


                    sout.str("");
                    test.decompress(sin,sout);
                    buffer = ch2;
                    CASSERT(sout.str() == buffer,"");

                    
                }
                print_spinner();


                // make sure you can compress and decompress the empty string
                sout.str("");
                sin.str("");
                test.compress(sin,sout);
                sin.str(sout.str());
                sout.str("");
                test.decompress(sin,sout);
                CASSERT(sout.str() == "",sout.str());





                print_spinner();

                sin.str("");
                sout.str("");
                buffer = "";

                buffer.reserve(2000000);
                // fill buffer with a bunch of random data in the range 0 to 63
                for (int i = 0; i < 2000000; ++i)
                {
                    char temp = static_cast<char>(::rand()&0x3f);
                    buffer.push_back(temp);
                }

                sin.str(buffer);

                print_spinner();
                test.compress(sin,sout);

                sin.str(sout.str());
                sout.str("");

                print_spinner();
                test.decompress(sin,sout);

                CASSERT(sout.str() == buffer,"");

                print_spinner();
            }


            // this block will try to compress a bunch of 'a' chars
            {

                istringstream sin;
                ostringstream sout;

                string buffer;


                print_spinner();

                sin.str("");
                sout.str("");
                buffer = "";

                buffer.reserve(2000000);
                // fill buffer with a bunch of 'a' chars
                for (int i = 0; i < 2000000; ++i)
                {
                    char temp = 'a';
                    buffer.push_back(temp);
                }

                sin.str(buffer);

                print_spinner();
                test.compress(sin,sout);

                sin.str(sout.str());
                sout.str("");

                print_spinner();
                test.decompress(sin,sout);

                CASSERT(sout.str() == buffer,"");

                print_spinner();

            }


            return 0;
        }
        catch(error& e)
        {
            cout << "\n\nERRORS FOUND in " << type << " (seed " << seed << ")" << endl;
            cout << e.what() << endl; 
            dlog << LWARN << "ERRORS FOUND in " << type << " (seed " << seed << ")";
            dlog << LWARN << e.what(); 
            return 1;
        }    
        catch (exception& e)
        {
            cout << "\n\nERROR OCCURRED (probably ran out of memory) in " << type 
                << " (seed " << seed << ")" << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERROR OCCURRED (probably ran out of memory) in " << type 
                << " (seed " << seed << ")";
            dlog << LWARN << e.what();
            return 1;
        }
    }






    class compress_stream_tester : public tester
    {
    public:
        compress_stream_tester (
        ) :
            tester ("test_compress_stream",
                    "Runs tests on the compress_stream component.")
        {}

        bool perform_test (
        )
        {
            const unsigned int seed = static_cast<unsigned int>(time(0));

            int n = 0;
            n += compress_stream_kernel_test<compress_stream::kernel_1a>    ("kernel_1a",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_1b>    ("kernel_1b",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_1c>    ("kernel_1c",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_1da>   ("kernel_1da",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_1db>   ("kernel_1db",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_1ea>   ("kernel_1ea",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_1eb>   ("kernel_1eb",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_1ec>   ("kernel_1ec",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_2a>    ("kernel_2a",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_3a>    ("kernel_3a",seed);
            n += compress_stream_kernel_test<compress_stream::kernel_3b>    ("kernel_3b",seed);
            return (n == 0);
        }
    } a;

}

