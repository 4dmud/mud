// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_COMPRESS_STREAM_KERNEl_1_
#define DLIB_COMPRESS_STREAM_KERNEl_1_

#include "../algs.h"
#include <iostream>
#include <streambuf>
#include "compress_stream_kernel_abstract.h"

namespace dlib
{

    template <
        typename fce,
        typename fcd
        >
    class compress_stream_kernel_1
    {
        /*!
            REQUIREMENTS ON fce
                is an implementation of entropy_encoder_model/entropy_encoder_model_kernel_abstract.h
                the alphabet_size of fce must be 257.
                fce and fcd share the same kernel number.

            REQUIREMENTS ON fcd
                is an implementation of entropy_decoder_model/entropy_decoder_model_kernel_abstract.h
                the alphabet_size of fcd must be 257.
                fce and fcd share the same kernel number.



            INITIAL VALUE
                this object has no state

            CONVENTION
                this object has no state
        !*/

        const static unsigned long eof_symbol = 256;

    public:

        class decompression_error : public dlib::error 
        { 
            public: 
                decompression_error(
                    const std::string& i
                ) :
                    dlib::error(i)
                {}
        };


        compress_stream_kernel_1 (
        )
        {}

        ~compress_stream_kernel_1 (
        )
        {}

        void compress (
            std::istream& in,
            std::ostream& out
        ) const;

        void decompress (
            std::istream& in,
            std::ostream& out
        ) const;

    private:

        // restricted functions
        compress_stream_kernel_1(compress_stream_kernel_1<fce,fcd>&);        // copy constructor
        compress_stream_kernel_1<fce,fcd>& operator=(compress_stream_kernel_1<fce,fcd>&);    // assignment operator

    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename fce,
        typename fcd
        >
    void compress_stream_kernel_1<fce,fcd>::
    compress (
        std::istream& in_,
        std::ostream& out_
    ) const
    {
        std::streambuf::int_type temp;

        std::streambuf& in = *in_.rdbuf();

        typename fce::entropy_encoder_type coder;
        coder.set_stream(out_);

        fce model(coder);

        unsigned long count = 0;

        while (true)
        {
            // write out a known value every 20000 symbols
            if (count == 20000)
            {
                count = 0;
                coder.encode(1500,1501,8000);
            }
            ++count;

            // get the next character
            temp = in.sbumpc();

            // if we have hit EOF then encode the marker symbol
            if (temp != EOF)  
            {
                // encode the symbol
                model.encode(static_cast<unsigned long>(temp));
                continue;
            }
            else
            {
                model.encode(eof_symbol);

                // now encode 3 just to make it easy to detect a fake
                // eof symbol due to corruption
                model.encode(3);
                break;
            }
        }      
    }

// ----------------------------------------------------------------------------------------

    template <
        typename fce,
        typename fcd
        >
    void compress_stream_kernel_1<fce,fcd>::
    decompress (
        std::istream& in_,
        std::ostream& out_
    ) const
    {

        std::streambuf& out = *out_.rdbuf();

        typename fcd::entropy_decoder_type coder;
        coder.set_stream(in_);

        fcd model(coder);

        unsigned long symbol;
        unsigned long count = 0;

        // decode until we hit the marker symbol
        while (true)
        {
            // make sure this is the value we expect
            if (count == 20000)
            {
                if (coder.get_target(8000) != 1500)
                {
                    throw decompression_error("Error detected in compressed data stream.");
                }
                count = 0;
                coder.decode(1500,1501);
            }
            ++count;

            // decode the next symbol
            model.decode(symbol);
            if (symbol != eof_symbol)
            {
                // write this symbol to out
                if (out.sputc(static_cast<char>(symbol)) != static_cast<int>(symbol))
                {
                    throw std::ios::failure("error occurred in compress_stream_kernel_1::decompress");
                }
                continue;
            }

            model.decode(symbol);
            if (symbol != 3)
            {
                throw decompression_error("Error detected in compressed data stream.");
            }

            break;
        } // while (true)

    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_COMPRESS_STREAM_KERNEl_1_

