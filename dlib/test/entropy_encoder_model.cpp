// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <ctime>
#include <cstdlib>

#include <dlib/entropy_encoder_model.h>
#include <dlib/entropy_decoder_model.h>
#include <dlib/entropy_encoder.h>
#include <dlib/entropy_decoder.h>
#include "tester.h"

namespace  
{
    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.entropy_coder_model");

    template <
        typename ee,
        typename ed
        >
    int entropy_encoder_model_kernel_test (
        const string& type
    )
    /*!
        requires
            - ee is an implementation of entropy_encoder_model/entropy_encoder_model_kernel_abstract.h            
              the alphabet_size for ee is 256
            - ed is an implementation of entropy_decoder_model/entropy_decoder_model_kernel_abstract.h            
              the alphabet_size for ed is 256
            - ee and ed must share the same kernel number
        ensures
            - runs tests on ee and ed for compliance with the specs 
            - returns 0 if no errors were found, 1 otherwise.
    !*/
    {        

        try 
        {

            srand(static_cast<unsigned int>(time(0)));

            typedef typename ee::entropy_encoder_type ee_type;
            typedef typename ed::entropy_decoder_type ed_type;

            

            {

                ee_type ecoder;
                ed_type dcoder;

                ee elen(ecoder);
                ed dlen(dcoder);
                ee elit(ecoder);
                ed dlit(dcoder);
               

                istringstream sin;
                ostringstream sout;

                ecoder.set_stream(sout);


                unsigned long temp;


                elen.encode(0);
                elit.encode(9);

                elen.encode(0);
                elit.encode(0);

                elen.encode(0);
                elit.encode(4);

                elen.encode(0);
                elit.encode(0);

                elen.encode(0);
                elit.encode(2);

                elen.encode(0);
                elit.encode(0);





    

                ecoder.clear();
                sin.str(sout.str());
                dcoder.set_stream(sin);
                

                dlen.decode(temp);
                CASSERT(temp == 0,"");
                dlit.decode(temp);
                CASSERT(temp == 9,"");
         
                dlen.decode(temp);
                CASSERT(temp == 0,"");
                dlit.decode(temp);
                CASSERT(temp == 0,"");
         
                dlen.decode(temp);
                CASSERT(temp == 0,"");
                dlit.decode(temp);
                CASSERT(temp == 4,"");
         
                dlen.decode(temp);
                CASSERT(temp == 0,"");
                dlit.decode(temp);
                CASSERT(temp == 0,"");
         
                dlen.decode(temp);
                CASSERT(temp == 0,"");
                dlit.decode(temp);
                CASSERT(temp == 2,"");
         
                dlen.decode(temp);
                CASSERT(temp == 0,"");
                dlit.decode(temp);
                CASSERT(temp == 0,"");
         


   
            }














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
        catch (exception e)
        {
            cout << "\n\nERROR OCCURRED (probably ran out of memory) in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERROR OCCURRED (probably ran out of memory) in " << type;
            dlog << LWARN << e.what();
            return 1;
        }
    }




    class entropy_encoder_model_tester : public tester
    {
    public:
        entropy_encoder_model_tester (
        ) :
            tester ("test_entropy_coder_model",
                    "Runs tests on the entropy_encoder_model and entropy_decoder_model components.")
        {}

        bool perform_test (
        )
        {
            typedef entropy_encoder::kernel_2a_c ee;
            typedef entropy_decoder::kernel_2a_c ed;
            int n = 0;
            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_1a,
                entropy_decoder_model<256,ed>::kernel_1a>    ("kernel_1a");
            print_spinner();

            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_2a,
                entropy_decoder_model<256,ed>::kernel_2a>    ("kernel_2a");
            print_spinner();

            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_3a,
                entropy_decoder_model<256,ed>::kernel_3a>    ("kernel_3a");
            print_spinner();

            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_4a,
                entropy_decoder_model<256,ed>::kernel_4a>    ("kernel_4a");
            print_spinner();

            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_4b,
                entropy_decoder_model<256,ed>::kernel_4b>    ("kernel_4b");
            print_spinner();

            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_5a,
                entropy_decoder_model<256,ed>::kernel_5a>    ("kernel_5a");
            print_spinner();

            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_5c,
                entropy_decoder_model<256,ed>::kernel_5c>    ("kernel_5c");
            print_spinner();

            n += entropy_encoder_model_kernel_test<
                entropy_encoder_model<256,ee>::kernel_6a,
                entropy_decoder_model<256,ed>::kernel_6a>    ("kernel_6a");
            print_spinner();

            return (n == 0);
        }
    } a;

}

