// Copyright (C) 2004  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <ctime>
#include <cstdlib>
#include <dlib/sliding_buffer.h>

#include <dlib/lz77_buffer.h>

#include "tester.h"

namespace  
{
    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.lz77_buffer");

    template <
        typename buf
        >
    int lz77_buffer_kernel_test (
        const string& type
    )
    /*!
        requires
            - buf is an implementation of lz77_buffer/lz77_buffer_kernel_abstract.h 
        ensures
            - runs tests on buf for compliance with the specs 
            - returns 0 if no errors were detected, 1 otherwise.
    !*/
    {        
        typedef dlib::sliding_buffer<unsigned char>::kernel_1a sbuf;

        try 
        {
            buf test(8,20);
            srand(static_cast<unsigned int>(time(0)));

            CASSERT(test.get_lookahead_buffer_size() == 0,"");
            CASSERT(test.get_history_buffer_size() == 0,"");
            CASSERT(test.get_history_buffer_limit() == 256-20,test.get_history_buffer_limit());
            CASSERT(test.get_lookahead_buffer_limit() == 20,"");


            for (int g = 0; g < 2; ++g)
            {
                test.clear();

                for (int i = 0; i < 1000; ++i)
                {
                    test.add('a');
                }
                CASSERT(test.get_lookahead_buffer_size() == 20,"");
                

                test.shift_buffers(5);

                CASSERT(test.get_lookahead_buffer_size() == 15,"");
                


                unsigned long index, length, temp;
                temp = test.get_lookahead_buffer_size();
                test.find_match(index,length,5);


                CASSERT(length <= temp,
                    "length: " << length <<
                    "\ntemp: " << temp);
                CASSERT(test.get_lookahead_buffer_size() <= 15,"");
            

            }


            for (int g = 0; g < 2; ++g)
            {



                test.clear();



                CASSERT(test.get_lookahead_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_limit() == 256-20,"");
                CASSERT(test.get_lookahead_buffer_limit() == 20,"");

                unsigned long a,b, temp = test.get_lookahead_buffer_size();
                test.find_match(a,b,0);
                CASSERT(b <= temp,"");
                CASSERT(b == 0,"");

                test.find_match(a,b,5);
                CASSERT(b == 0,"");

                CASSERT(test.get_lookahead_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_limit() == 256-20,"");
                CASSERT(test.get_lookahead_buffer_limit() == 20,"");



                ostringstream sout;
                sout << "CASSERT(test.get_lookahead_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_limit() == 256-20,);\n";
                sout << "CASSERT(test.get_lookahead_buffer_limit() == 20,);\n";
                sout << "CASSERT(test.get_lookahead_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_limit() == 256-20,);\n";
                sout << "CASSERT(test.get_lookahead_buffer_limit() == 20,);\n";
                istringstream sin(sout.str());

                sout.str("");
                sout.clear();

                unsigned char ch;
                sbuf sbuffer;
                sbuffer.set_size(8);


                ch = sin.get();
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);
                CASSERT(test.lookahead_buffer(test.get_lookahead_buffer_size()-1) == ch,"");
                CASSERT(test.get_lookahead_buffer_size() == 1,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.get_lookahead_buffer_limit() == 20,"");



                ch = sin.get();
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);
                CASSERT(test.lookahead_buffer(test.get_lookahead_buffer_size()-1) == ch,"");
                CASSERT(test.get_lookahead_buffer_size() == 2,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.get_lookahead_buffer_limit() == 20,"");



                ch = sin.get();
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);
                CASSERT(test.lookahead_buffer(test.get_lookahead_buffer_size()-1) == ch,"");
                CASSERT(test.get_lookahead_buffer_size() == 3,"");
                CASSERT(test.get_history_buffer_size() == 0,"");

                // add 17 chars to test so that the lookahead buffer will be full
                for (int i = 0; i < 17; ++i)
                {
                    ch = sin.get();
                    sbuffer[0] = ch; sbuffer.rotate_left(1);
                    test.add(ch);
                    CASSERT(test.lookahead_buffer(test.get_lookahead_buffer_size()-1) == ch,"");
                }

                CASSERT(test.get_lookahead_buffer_size() == 20,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.lookahead_buffer(0) == sbuffer[20],"");


                ch = sin.get();
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);
                CASSERT(test.lookahead_buffer(test.get_lookahead_buffer_size()-1) == ch,"");
                CASSERT(test.get_lookahead_buffer_size() == 20,"");
                CASSERT(test.get_history_buffer_size() == 1,"");






                // add the above text to test and make sure it gives the correct results                
                ch = sin.get();
                while (sin)
                {
                    sbuffer[0] = ch; sbuffer.rotate_left(1);
                    test.add(ch);
                    CASSERT(test.lookahead_buffer(test.get_lookahead_buffer_size()-1) == ch,"");                    
                    CASSERT(test.history_buffer(0) == sbuffer[21],"");
                    CASSERT(test.history_buffer(1) == sbuffer[22],"");
                    
                    ch = sin.get();
                }



                // make sure the contents of lookahead_buffer and history_buffer 
                // match what is in sbuffer
                sbuffer.rotate_right(1);
                for (unsigned int i = 0; i < test.get_history_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()+i] == test.history_buffer(i),"");
                }
                for (unsigned int i = 0; i < test.get_lookahead_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()-1-i] == test.lookahead_buffer(i),"");
                }
                sbuffer.rotate_left(1);










                sbuffer.rotate_right(1);  // do this because we never put anything in sbuffer[0]

                unsigned long match_index, match_length;
                unsigned long ltemp = test.get_lookahead_buffer_size();
                test.find_match(match_index,match_length,0);
                CASSERT(match_length <= ltemp,"");


                // verify the match with sbuffer
                for (unsigned int i = 0; i < match_length; ++i)
                {
                    CASSERT(sbuffer[19-i] == sbuffer[match_index+20-i],i);
                }


                sin.str("");
                sin.clear();

            } // for (int g = 0; g < 2; ++g)


            for (int g = 0; g < 8; ++g)
            {
                test.clear();


                CASSERT(test.get_lookahead_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_limit() == 256-20,"");
                CASSERT(test.get_lookahead_buffer_limit() == 20,"");


                sbuf sbuffer;
                sbuffer.set_size(8);

                ostringstream sout;
                sout << "CASSERT(test.get_lookahead_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_limit() == 256-20,);\n";
                sout << "CASSERT(test.get_lookahead_buffer_limit() == 20,);\n";
                sout << "CASSERT(test.get_lookahead_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_limit() == 256-20,);\n";
                sout << "CASSERT(test.get_lookahead_buffer_limit() == 20,);\n";
                sout << "CASSERT(test.get_history_buffer_limit() == 256-20,);\n";
                sout << "CASSERT(test.get_lookahead_buffer_size() == 0,);\n";
                sout << "CASSERT(test.get_history_buffer_limit() == 256-20,);\n";
                sout << "CASSERT(test.get_history_buffer_limit() == 256-20,);\n";
                istringstream sin(sout.str());

                unsigned char ch;
                for (int i = 0; i < 100; ++i)
                {
                    ch = sin.get();
                    sbuffer[0] = ch; sbuffer.rotate_left(1);
                    test.add(ch);                    
                }
                
                // make sure the contents of lookahead_buffer and history_buffer 
                // match what is in sbuffer
                sbuffer.rotate_right(1);
                for (unsigned int i = 0; i < test.get_history_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()+i] == test.history_buffer(i),"");
                }
                for (unsigned int i = 0; i < test.get_lookahead_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()-1-i] == test.lookahead_buffer(i),"");
                }
                sbuffer.rotate_left(1);


               

                unsigned long match_index, match_length;
                unsigned long ltemp = test.get_lookahead_buffer_size();
                test.find_match(match_index,match_length,0);
                CASSERT(match_length <= ltemp,"");

                CASSERT(test.get_lookahead_buffer_size() == 20-match_length,"");

                sbuffer.rotate_right(1);  // do this because we never put anything in sbuffer[0]
                // verify the match with sbuffer
                for (unsigned int i = 0; i < match_length; ++i)
                {
                    CASSERT(sbuffer[i+20-match_length] == sbuffer[i+1+match_index+20-match_length],"");
                }
                sbuffer.rotate_left(1);  // free up sbuffer[0] for new data
                



                for (int i = 0; i < 7+g*2; ++i)
                {
                    ch = sin.get();
                    sbuffer[0] = ch; sbuffer.rotate_left(1);
                    test.add(ch);                    
                }

                ch = '?';
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);  
                ch = 'a';
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);  
                ch = 'v';
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);  
                ch = 'i';
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);  
                ch = 's';
                sbuffer[0] = ch; sbuffer.rotate_left(1);
                test.add(ch);  


                // adjust sbuffer due to the last call to test.find_match()
                // but only if we haven't already added enough (20 or more) chars
                // to fill the lookahead buffer already.  
                if (match_length > static_cast<unsigned int>(12+g*2))
                    sbuffer.rotate_left(match_length-(12+g*2)); 


 


                // make sure the contents of lookahead_buffer and history_buffer 
                // match what is in sbuffer
                sbuffer.rotate_right(1);
                for (unsigned int i = 0; i < test.get_history_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()+i] == test.history_buffer(i),"");
                }
                for (unsigned int i = 0; i < test.get_lookahead_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()-1-i] == test.lookahead_buffer(i),"");
                }
                sbuffer.rotate_left(1);




                test.find_match(match_index,match_length,10+g);

                if (match_length > 0)
                    CASSERT(match_length >= static_cast<unsigned int>(10+g) , "");


                sbuffer.rotate_right(1);  // do this because we never put anything in sbuffer[0]
                // verify the match with sbuffer
                for (unsigned int i = 0; i < match_length; ++i)
                {
                    CASSERT(sbuffer[i+20-match_length] == sbuffer[i+1+match_index+20-match_length],"");
                }
                sbuffer.rotate_left(1);  // free up sbuffer[0] for new data

            } // for (int g = 0; g < 8; ++g)







            srand(static_cast<unsigned int>(time(0)));

            for (int g = 0; g < 200; ++g)
            {
                test.clear();

                CASSERT(test.get_lookahead_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_limit() == 256-20,"");
                CASSERT(test.get_lookahead_buffer_limit() == 20,"");


                sbuf sbuffer;
                sbuffer.set_size(8);

                ostringstream sout;
                int l = ::rand()%500;
                for (int i = 0; i < l; ++i)
                {
                    char temp = static_cast<char>(::rand()%256);
                    sout << temp;
                }
                istringstream sin(sout.str());

                unsigned char ch;
                for (int i = 0; i < l; ++i)
                {
                    ch = sin.get();
                    sbuffer[0] = ch; sbuffer.rotate_left(1);
                    test.add(ch);                    
                }
                
                // make sure the contents of lookahead_buffer and history_buffer 
                // match what is in sbuffer
                sbuffer.rotate_right(1);
                
                // adjust so that sbuffer[19] is the same as lookahead_buffer[0]
                if (test.get_lookahead_buffer_size() < 20)
                    sbuffer.rotate_left(20-test.get_lookahead_buffer_size());

                for (unsigned int i = 0; i < test.get_history_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()+i] == test.history_buffer(i),"");
                }
                for (unsigned int i = 0; i < test.get_lookahead_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()-1-i] == test.lookahead_buffer(i),"");
                }
                sbuffer.rotate_left(1);



                unsigned long match_index, match_length;
                unsigned long lookahead_size_before = test.get_lookahead_buffer_size();
                test.find_match(match_index,match_length,0);
                CASSERT(match_length <= lookahead_size_before,"");


                CASSERT(test.get_lookahead_buffer_size() == lookahead_size_before-match_length,"");

                sbuffer.rotate_right(1);  // do this because we never put anything in sbuffer[0]
                // verify the match with sbuffer
                for (unsigned int i = 0; i < match_length; ++i)
                {
                    CASSERT(sbuffer[19-i] == sbuffer[match_index+20-i],i);
                }
                sbuffer.rotate_left(1);  // free up sbuffer[0] for new data
                
            } // for (int g = 0; g < 200; ++g)








            srand(static_cast<unsigned int>(time(0)));

            for (int g = 0; g < 300; ++g)
            {
                test.clear();

                CASSERT(test.get_lookahead_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_size() == 0,"");
                CASSERT(test.get_history_buffer_limit() == 256-20,"");
                CASSERT(test.get_lookahead_buffer_limit() == 20,"");


                sbuf sbuffer;
                sbuffer.set_size(8);

                ostringstream sout;
                int l = ::rand()%500;
                for (int i = 0; i < l; ++i)
                {
                    char temp = static_cast<char>(::rand()%20);
                    sout << temp;
                    sout << temp;
                    sout << temp;
                    sout << temp;
                    sout << temp;
                    sout << temp;
                }
                istringstream sin(sout.str());

                unsigned char ch;
                for (int i = 0; i < l; ++i)
                {
                    ch = sin.get();
                    sbuffer[0] = ch; sbuffer.rotate_left(1);
                    test.add(ch);                    
                }
                
                // make sure the contents of lookahead_buffer and history_buffer 
                // match what is in sbuffer
                sbuffer.rotate_right(1);
                
                // adjust so that sbuffer[19] is the same as lookahead_buffer[0]
                if (test.get_lookahead_buffer_size() < 20)
                    sbuffer.rotate_left(20-test.get_lookahead_buffer_size());

                for (unsigned int i = 0; i < test.get_history_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()+i] == test.history_buffer(i),"");
                }
                for (unsigned int i = 0; i < test.get_lookahead_buffer_size(); ++i)
                {
                    CASSERT(sbuffer[test.get_lookahead_buffer_limit()-1-i] == test.lookahead_buffer(i),"");
                }
                sbuffer.rotate_left(1);



                unsigned long match_index, match_length;
                unsigned long lookahead_size_before = test.get_lookahead_buffer_size();
                unsigned long history_size_before = test.get_history_buffer_size();
                test.find_match(match_index,match_length,2);

                if (match_length != 0)
                {
                    CASSERT(match_index < history_size_before,
                        "match_index: " << match_index <<
                        "\nhistory_size_before: " << history_size_before);
                    
                }

                    
                CASSERT(test.get_lookahead_buffer_size() == lookahead_size_before-match_length,"");            
                
                sbuffer.rotate_right(1);  // do this because we never put anything in sbuffer[0]
                // verify the match with sbuffer
                for (unsigned int i = 0; i < match_length; ++i)
                {
                    CASSERT(sbuffer[19-i] == sbuffer[match_index+20-i],i);
                }
                sbuffer.rotate_left(1);  // free up sbuffer[0] for new data
                


            } // for (int g = 0; g < 300; ++g)







                
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




    class lz77_buffer_tester : public tester
    {
    public:
        lz77_buffer_tester (
        ) :
            tester ("test_lz77_buffer",
                    "Runs tests on the lz77_buffer component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += lz77_buffer_kernel_test<lz77_buffer::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += lz77_buffer_kernel_test<lz77_buffer::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            n += lz77_buffer_kernel_test<lz77_buffer::kernel_2a>    ("kernel_2a");
            print_spinner();
            n += lz77_buffer_kernel_test<lz77_buffer::kernel_2a_c>  ("kernel_2a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}

