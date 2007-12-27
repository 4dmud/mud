// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <string>
#include <sstream>

#include <dlib/tokenizer.h>
#include "tester.h"

namespace  
{
    using namespace test;
    using namespace std;
    using namespace dlib;
  
    logger dlog("test.tokenizer");

    template <
        typename tok
        >
    int tokenizer_kernel_test (
        const string& type
    )
    /*!
        requires
            - tok is an implementation of tokenizer_kernel_abstract.h
        ensures
            - runs tests on tok for compliance with the specs 
            - returns 0 if no errors are found, 1 otherwise
    !*/
    {        

        try 
        {
 
            tok test;

            CASSERT(test.numbers() == "0123456789","");
            CASSERT(test.uppercase_letters() == "ABCDEFGHIJKLMNOPQRSTUVWXYZ","");
            CASSERT(test.lowercase_letters() == "abcdefghijklmnopqrstuvwxyz","");

            CASSERT(test.get_identifier_body() == "_" + test.lowercase_letters() +
                test.uppercase_letters() + test.numbers(),"");
            CASSERT(test.get_identifier_head() == "_" + test.lowercase_letters() +
                test.uppercase_letters(),"");

            CASSERT(test.stream_is_set() == false,"");
            test.clear();
            CASSERT(test.stream_is_set() == false,"");

            CASSERT(test.get_identifier_body() == "_" + test.lowercase_letters() +
                test.uppercase_letters() + test.numbers(),"");
            CASSERT(test.get_identifier_head() == "_" + test.lowercase_letters() +
                test.uppercase_letters(),"");

            tok test2;

            ostringstream sout;
            istringstream sin;
            test2.set_stream(sin);

            CASSERT(test2.stream_is_set(),"");
            CASSERT(&test2.get_stream() == &sin,"");

            int type;
            string token;

            test2.get_token(type,token);
            CASSERT(type == tok::END_OF_FILE,"");
            test2.get_token(type,token);
            CASSERT(type == tok::END_OF_FILE,"");
            test2.get_token(type,token);
            CASSERT(type == tok::END_OF_FILE,"");            


            sin.clear();
            sin.str("  The cat 123asdf1234 ._ \n test.");

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == "  ","");

            CASSERT(test2.peek_type() == tok::IDENTIFIER,"");
            CASSERT(test2.peek_token() == "The",""); 
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "The","");            

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","");
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "cat","");            

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","");

            test2.get_token(type,token);
            CASSERT(type == tok::NUMBER,"");
            CASSERT(token == "123","token: " << token);
                
            CASSERT(test2.peek_type() == tok::IDENTIFIER,"");
            CASSERT(test2.peek_token() == "asdf1234","");
            CASSERT(test2.peek_type() == tok::IDENTIFIER,"");
            CASSERT(test2.peek_token() == "asdf1234","");
            CASSERT(test2.peek_type() == tok::IDENTIFIER,"");
            CASSERT(test2.peek_token() == "asdf1234","");
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "asdf1234","");
                
            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == ".","token: " << token);

            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "_","");

            CASSERT(test2.peek_type() == tok::WHITE_SPACE,"");
            CASSERT(test2.peek_token() == " ","token: \"" << token << "\"" <<
                "\ntoken size: " << (unsigned int)token.size());

            swap(test,test2);

            CASSERT(test2.stream_is_set() == false,"");

            CASSERT(test.peek_type() == tok::WHITE_SPACE,"");
            CASSERT(test.peek_token() == " ","token: \"" << token << "\"" <<
                "\ntoken size: " << (unsigned int)token.size());
            test.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: \"" << token << "\"" <<
                "\ntoken size: " << (unsigned int)token.size());

            test.get_token(type,token);
            CASSERT(type == tok::END_OF_LINE,"token: " << token);
            CASSERT(token == "\n","token: " << token);
            
            swap(test,test2);
            CASSERT(test.stream_is_set() == false,"");

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "test","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == ".","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::END_OF_FILE,"");










            test2.set_identifier_token("_" + test.uppercase_letters() +
                test.lowercase_letters(),test.numbers() + "_" + test.uppercase_letters()
                +test.lowercase_letters());


            sin.clear();
            sin.str("  The cat 123asdf1234 ._ \n\r test.");

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == "  ","");

            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "The","");            

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","");
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "cat","");            

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","");

            test2.get_token(type,token);
            CASSERT(type == tok::NUMBER,"");
            CASSERT(token == "123","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "asdf1234","");
                
            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == ".","token: " << token);

            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "_","");

            swap(test,test2);

            CASSERT(test2.stream_is_set() == false,"");

            test.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: \"" << token << "\"" <<
                "\ntoken size: " << (unsigned int)token.size());

            test.get_token(type,token);
            CASSERT(type == tok::END_OF_LINE,"token: " << token);
            CASSERT(token == "\n","token: " << token);
            
            swap(test,test2);
            CASSERT(test.stream_is_set() == false,"");

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == "\r ","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "test","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == ".","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::END_OF_FILE,"");













            test2.set_identifier_token(test.uppercase_letters() +
                test.lowercase_letters(),test.numbers() + test.uppercase_letters()
                +test.lowercase_letters());


            sin.clear();
            sin.str("  The cat 123as_df1234 ._ \n test.");

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == "  ","");

            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "The","");            

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","");
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "cat","");            

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","");

            test2.get_token(type,token);
            CASSERT(type == tok::NUMBER,"");
            CASSERT(token == "123","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "as","");
                
            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == "_","token: " << token);

            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "df1234","");

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == ".","token: " << token);

            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == "_","");

            swap(test,test2);

            CASSERT(test2.stream_is_set() == false,"");

            test.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: \"" << token << "\"" <<
                "\ntoken size: " << (unsigned int)token.size());

            test.get_token(type,token);
            CASSERT(type == tok::END_OF_LINE,"token: " << token);
            CASSERT(token == "\n","token: " << token);
            
            swap(test,test2);
            CASSERT(test.stream_is_set() == false,"");

            test2.get_token(type,token);
            CASSERT(type == tok::WHITE_SPACE,"");
            CASSERT(token == " ","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::IDENTIFIER,"");
            CASSERT(token == "test","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::CHAR,"");
            CASSERT(token == ".","token: " << token);
                
            test2.get_token(type,token);
            CASSERT(type == tok::END_OF_FILE,"");














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





    class tokenizer_tester : public tester
    {
    public:
        tokenizer_tester (
        ) :
            tester ("test_tokenizer",
                    "Runs tests on the tokenizer component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += tokenizer_kernel_test<tokenizer::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += tokenizer_kernel_test<tokenizer::kernel_1a_c>  ("kernel_1a_c");
            print_spinner();
            return (n == 0);
        }
    } a;

}


