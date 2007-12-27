// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <ctime>
#include <cstdlib>

#include <dlib/entropy_encoder.h>
#include <dlib/entropy_decoder.h>

#include "tester.h"

namespace  
{

    using namespace test; 
    using namespace std;
    using namespace dlib;

    logger dlog("test.entropy_coder");

    namespace entropy_coder_kernel_test_helpers 
    {
        template <
            typename encoder,
            typename decoder
            >
        std::string test (
            const std::string& input
        )
        /*!
            ensures
                - encodes the data from in and then tries to decode it and returns
                  "" if it was successfully decoded else it returns the decoded string
        !*/
        {
            ostringstream sout;
            istringstream sin;
            istringstream in;


            in.str(input);

            const unsigned long max_total = 65535;




            unsigned long counts[256];
            for (int i = 0; i < 256; ++i)
            {
                counts[i] = 1;
            }


            encoder e;

         

            CASSERT(e.stream_is_set() == false,"");

            e.set_stream(sout);

            CASSERT(e.stream_is_set() == true,"");
            CASSERT(&e.get_stream() == &sout,"");

            unsigned char ch;
            
            unsigned long total = 256;

            while (in.read((char*)&ch,1))
            {
                if (total > max_total)
                {
                    total = 0;
                    for (int j = 0; j<256; ++j)
                    {
                        counts[j] >>= 1;
                        if (counts[j] == 0)
                            counts[j] = 1;
                        total += counts[j];

                    }
                }

                unsigned long low_count = 0;
                unsigned long high_count;
                for (int i = 0; i < ch; ++i)
                    low_count += counts[i];
                high_count = low_count + counts[ch];
               
                e.encode(low_count,high_count,total);
               

                ++total;
                counts[ch] += 1;
            }

            CASSERT(e.stream_is_set() == true,"");
            CASSERT(&e.get_stream() == &sout,"");
            


            e.clear();

            CASSERT(e.stream_is_set() == false,"");


            // *****************************************

     
            decoder d;


            CASSERT(d.stream_is_set() == false,"");
            CASSERT(d.get_target_called() == false,"");

            sin.str(sout.str());
            sout.str("");

            d.set_stream(sin);

            CASSERT(d.get_target_called() == false,"");

            CASSERT(d.stream_is_set() == true,"");
            CASSERT(&d.get_stream() == &sin,"");

            for (int i = 0; i < 256; ++i)
            {
                counts[i] = 1;
            }

            total = 256;

            for (string::size_type i = 0; i < input.size()  ; ++i)
            {
                if (total > max_total)
                {
                    total = 0;
                    for (int j = 0; j<256; ++j)
                    {
                        counts[j] >>= 1;
                        if (counts[j] == 0)
                            counts[j] = 1;
                        total += counts[j];

                    }
                }

                CASSERT(d.get_target_called() == false,"");

                unsigned long target = d.get_target(total);

                CASSERT(target < total,"");

                CASSERT(d.get_target_called() == true,"");


                unsigned long low_count;
                unsigned long high_count = 0;
                
                unsigned long j;
                for (j = 0; high_count <= target; ++j)
                {
                    high_count += counts[j];
                }
                --j;
                low_count = high_count - counts[j];


                ch = static_cast<unsigned char>(j);


                sout.rdbuf()->sputn((char*)&ch,1);
         


                d.decode(low_count,high_count);
                CASSERT(d.get_target_called() == false,"");
                ++total;
                counts[ch] += 1;

            }

            CASSERT(d.stream_is_set() == true,"");
            CASSERT(&d.get_stream() == &sin,"");

            d.clear();

            CASSERT(d.stream_is_set() == false,"");
            CASSERT(sout.str().size() == input.size(),"the test script is buggy");
            

            if (sout.str() == input)
                return "";
            else
                return sout.str();

        }

    }


    

    template <
        typename encoder,
        typename decoder
        >
    int entropy_coder_kernel_test (
        const string& type
    )
    /*!
        requires
            - encoder is an implementation of entropy_encoder/entropy_encoder_kernel_abstract.h
            - decoder is an implementation of entropy_decoder/entropy_decoder_kernel_abstract.h
        ensures
            - runs tests on encoder and decoder for compliance with the specs 
            - returns 0 if no errors are detected, 1 otherwise.
    !*/
    {        
        using namespace entropy_coder_kernel_test_helpers;

        try 
        {

            string temp, temp2;

            srand(static_cast<int>(time(0)));

            for (int k = 0; k < 100000; ++k)
            {
                string temp;
                istringstream sin;
                ostringstream sout;
                decoder d;
                encoder e;

                e.set_stream(sout);

                int num = ::rand() %200;
                unsigned long total[200];
                unsigned long high_count[200];
                unsigned long low_count[200];
                for (int i = 0; i < num; ++i)
                {
                    total[i] = ::rand()%256 + 20;
                    high_count[i] = ::rand()%total[i] + 1;
                    low_count[i] = ::rand()%high_count[i];

                    e.encode(low_count[i],high_count[i],total[i]);
                }

                e.clear();

                sout.rdbuf()->sputc('a');

                sin.str(sout.str());
                

                d.set_stream(sin);

                
                for (int i = 0; i < num; ++i)
                {
                    unsigned long N = d.get_target(total[i]);                
                    CASSERT(low_count[i] <= N && N < high_count[i],"");
                    d.decode(low_count[i],high_count[i]);
                }
                
                




                CASSERT(sin.rdbuf()->sgetc() != EOF,"num: " << num);
                CASSERT(sin.rdbuf()->sgetc() == 'a',
                    "sin.rdbuf()->sgetc() == " << (char)sin.rdbuf()->sgetc() <<
                    "\nnum: " << num
                    );
                CASSERT(sin.rdbuf()->sbumpc() == 'a',"");
                CASSERT(sin.rdbuf()->sgetc() == EOF,"");

            } // for (int k = 0; k < 100000; ++k)


            print_spinner();

            // the point of this block is to make sure that the return value
            // from decoder.get_target(total) is a always less than total
            for (int k = 0; k < 20; ++k)
            {
                string temp;
                temp.push_back(static_cast<char>(::rand()&0xff));
                istringstream sin(temp);
                decoder d;
                d.set_stream(sin);
                unsigned long total = ::rand()%256 + 20;
                unsigned long target = d.get_target(total);
                CASSERT(target<total,"");
                
                for (int i = 0; i < 30; ++i)
                {
                    unsigned long high_count = ::rand()%total + 1;
                    unsigned long low_count = ::rand()%high_count;
                    if (high_count <= target)
                        high_count = target+1;
                    if (low_count > target)
                        low_count = target;
                    
                    d.decode(low_count,high_count);
                    target = d.get_target(total);
                    CASSERT(target<total,"target: " << target << "    total: " << total);
                }
            }

            print_spinner();



            for (int k = 0; k < 10; ++k)
            {
                unsigned long seed1 = 1064644658, seed2 = 1064543921;
                //unsigned long seed1 = 1064682621, seed2 = 1064543921;

                // make array be an array with each element in the range 0 to 255
                // and have the probability of seeing each number in the array
                // not be the same
                seed1 = static_cast<unsigned long>(time(0));
                srand(seed1 );
                int array[65536];
                for (int i = 0; i < 65536; ++i)
                {
                    array[i] = ::rand()%256;
                }
                for (int i = 0; i < 60; ++i)
                {
                    int idx = ::rand()%65536;
                    int radius = 35;
                    if (idx > radius && idx <65536-radius)
                    {
                        for (int j = idx-radius; j < idx+radius; ++j)
                            array[j] = array[idx];
                    }
                }

                // test with 3 random strings of length 2000000 
                // but use the above array to bias the random numbers
                for (int j = 0; j < 3; ++j)
                {
                    print_spinner();
                    temp = "";
                    seed2 = static_cast<unsigned long>(time(0));
                    srand(seed2 );
                    for ( int i = 0; i < 3000000; ++i)
                    {
                        int a = array[::rand()%65536];
                        temp += (unsigned char)a;                
                    }               
                    string temp2;
                    temp2 = test<encoder,decoder>(temp);  
                    if (temp2 != "")
                    {

                        int k = 0;
                        CASSERT(temp != temp2,"");
                        while (temp[k] == temp2[k])++k;
                    }
                    

                    CASSERT(temp2 == "","")  
                }
            }

            print_spinner();






            // test with a large string which contains all the same character
            temp = "eeeeeeeeee";
            for (int i = 0; i < 22; ++i)
            {
                temp = temp + temp;
            }
            temp = test<encoder,decoder>(temp); 
            if (temp != "")
            {   
                // crop off all the e's until we find the part that is messed up
                string::size_type pos = temp.find_first_not_of("e");
                temp = temp.substr(pos);
            }
            CASSERT(temp == "","decoded string: \"" << temp << "\"") /**/



            print_spinner();

            temp = "davis";
            temp = test<encoder,decoder>(temp);  CASSERT(temp == "","decoded string: \"" << temp << "\"")

            temp = "";
            temp = test<encoder,decoder>(temp);  CASSERT(temp == "","decoded string: \"" << temp << "\"")

            // test for each single character
            for ( int i = 0; i <= 255; ++i)
            {
                temp = (unsigned char)i;
                temp = test<encoder,decoder>(temp);  CASSERT(temp == "","decoded string: \"" << temp << "\"")                  
            } 

            // test with a long string with the same thing repeated many times
            temp = "davis ";
            for (int i = 0; i < 20; ++i)
            {
                temp = temp + temp;
            }
            temp = test<encoder,decoder>(temp);  CASSERT(temp == "","decoded string: \"" << temp << "\"")


            // test with 10 random strings of length 1000 
            for (int j = 0; j < 10; ++j)
            {
                temp = "";
                srand(static_cast<unsigned int>(time(0)));
                for ( int i = 0; i < 1000; ++i)
                {
                    int a = ::rand()%256;
                    temp += (unsigned char)a;                
                } 
                temp = test<encoder,decoder>(temp);  CASSERT(temp == "","decoded string: \"" << temp << "\"")                    
            }


            print_spinner();

            // test with 15 random strings of length 500000 
            for (int j = 0; j < 15; ++j)
            {
                temp = "";
                unsigned long seed = static_cast<unsigned int>(time(0));
                srand(seed);
                for ( int i = 0; i < 500000; ++i)
                {
                    int a = ::rand()%256;
                    temp += (unsigned char)a;                
                }               
                temp = test<encoder,decoder>(temp);  CASSERT(temp == "","seed: " << seed)  
            }



            print_spinner();

            // test with a large string which contains all the same character
            temp = "         ";
            for (int i = 0; i < 22; ++i)
            {
                temp = temp + temp;
            }
            temp = test<encoder,decoder>(temp); 
            if (temp != "")
            {   
                // crop off all the spacess until we find the part that is messed up
                string::size_type pos = temp.find_first_not_of(" ");
                temp = temp.substr(pos);
            }
            CASSERT(temp == "","decoded string: \"" << temp << "\"")/**/












            // test with a large string which contains a bunch of a's followed by a
            // bunch of z's
            temp = "aaaaaaaa";
            temp2 = "zzzzzzzz";
            for (int i = 0; i < 17; ++i)
            {
                temp = temp + temp;
                temp2 = temp2 + temp2;
            }
            temp += temp2;
            print_spinner();
            temp = test<encoder,decoder>(temp); 
            CASSERT(temp == "","");






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
        catch (exception e)
        {
            cout << "\n\nERROR OCCURRED (probably ran out of memory) in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERROR OCCURRED (probably ran out of memory) in " << type;
            dlog << LWARN << e.what();
            return 1; 
        }
    }




    class entropy_coder_tester : public tester
    {
    public:
        entropy_coder_tester (
        ) :
            tester ("test_entropy_coder",
                    "Runs tests on the entropy_coder component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            print_spinner();
            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_1a,
                entropy_decoder::kernel_1a
                >    ("kernel_1a 1");
            print_spinner();

            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_1a_c,
                entropy_decoder::kernel_1a_c
                >    ("kernel_1a_c 2");
            print_spinner();

            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_2a,
                entropy_decoder::kernel_2a
                >    ("kernel_1a 3");
            print_spinner();

            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_2a_c,
                entropy_decoder::kernel_2a_c
                >    ("kernel_1a_c 4");
            print_spinner();

            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_1a,
                entropy_decoder::kernel_1a_c
                >    ("kernel_1a 5");
            print_spinner();

            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_1a_c,
                entropy_decoder::kernel_1a
                >    ("kernel_1a_c 6");
            print_spinner();

            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_2a,
                entropy_decoder::kernel_2a_c
                >    ("kernel_1a 7");
            print_spinner();

            n += entropy_coder_kernel_test<
                entropy_encoder::kernel_2a_c,
                entropy_decoder::kernel_2a
                >    ("kernel_1a_c 8");
            print_spinner();

            return (n == 0);
        }
    } a;




}

