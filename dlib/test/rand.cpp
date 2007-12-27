// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <dlib/rand.h>
#include <dlib/compress_stream.h>

#include "tester.h"

namespace  
{

    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.rand");

    template <
        typename rand
        >
    int rand_test (
        const string& type
    )
    /*!
        requires
            - rand is an implementation of rand/rand_kernel_abstract.h 
              is instantiated with int
        ensures
            - runs tests on rand for compliance with the specs
            - returns 0 if there aren't any errors, 1 otherwise
    !*/
    {        

        try 
        {
            ostringstream seed;
            seed << (unsigned int)time(0);

            ostringstream sout;


            rand r, r2;
            CASSERT(r.get_seed() == "","");
            r.set_seed(seed.str());

            CASSERT(r.get_seed() == seed.str(),"");
            r.clear();
            CASSERT(r.get_seed() == "","");
            swap(r,r2);
            CASSERT(r.get_seed() == "","");
            r.set_seed(seed.str());
            CASSERT(r.get_seed() == seed.str(),"");
            swap(r,r2);
            CASSERT(r2.get_seed() == seed.str(),"");
            CASSERT(r.get_seed() == "","");
            swap(r,r2);
            CASSERT(r.get_seed() == seed.str(),"");
            CASSERT(r2.get_seed() == "","");

            const unsigned long size = 100000;
            srand((unsigned int)time(0));
            for (unsigned long i = 0; i < size; ++i) 
            {
                sout << r.get_random_number();
                if ((i&0xFFFFFF) == 0xFFFFFF)
                    print_spinner();
            }

            istringstream rdata;
            rdata.str(sout.str());
            double compressed_size;
            compress_stream::kernel_1a cs1;
            compress_stream::kernel_2a cs2;

            compress_stream_kernel_1<
                entropy_encoder_model_kernel_5<257,entropy_encoder::kernel_1a,4000000,4>,
                entropy_decoder_model_kernel_5<257,entropy_decoder::kernel_1a,4000000,4>
            > cs3;

            
            print_spinner();

            rdata.clear();
            rdata.seekg(0);
            sout.clear();
            sout.str("");
            cs1.compress(rdata,sout);
            compressed_size = sout.str().size();
            compressed_size *= 8;
            compressed_size /= size;
            CASSERT(compressed_size >= 8, "order 0 bps: " << compressed_size);
            dlog << LINFO << "order 0: " << compressed_size;

            print_spinner();

            rdata.clear();
            rdata.seekg(0);
            sout.clear();
            sout.str("");
            cs2.compress(rdata,sout);
            compressed_size = sout.str().size();
            compressed_size *= 8;
            compressed_size /= size;
            CASSERT(compressed_size >= 8, "order 1 bps: " << compressed_size);
            dlog << LINFO << "order 1: " << compressed_size;

            print_spinner();

            rdata.clear();
            rdata.seekg(0);
            sout.clear();
            sout.str("");
            cs3.compress(rdata,sout);
            compressed_size = sout.str().size();
            compressed_size *= 8;
            compressed_size /= size;
            CASSERT(compressed_size >= 8, "order 4 bps: " << compressed_size);
            dlog << LINFO << "order 4: " << compressed_size;


            return 0;
        }
        catch(error& e)
        {
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.info;
            return 1;
        }        
    }






    class rand_tester : public tester
    {
    public:
        rand_tester (
        ) :
            tester ("test_rand",
                    "Runs tests on the rand component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            print_spinner();
            n += rand_test<dlib::rand::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += rand_test<dlib::rand::kernel_2a>  ("kernel_2a");
            return (n == 0);
        }
    } a;

}


