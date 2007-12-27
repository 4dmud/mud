// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_COMPRESS_STREAM_KERNEl_2_
#define DLIB_COMPRESS_STREAM_KERNEl_2_

#include "../algs.h"
#include <iostream>
#include <streambuf>
#include "compress_stream_kernel_abstract.h"

namespace dlib
{

    template <
        typename fce,
        typename fcd,
        typename lz77_buffer,
        typename sliding_buffer,
        typename fce_length,
        typename fcd_length,
        typename fce_index,
        typename fcd_index
        >
    class compress_stream_kernel_2
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

            REQUIREMENTS ON lz77_buffer
                is an implementation of lz77_buffer/lz77_buffer_kernel_abstract.h

            REQUIREMENTS ON sliding_buffer
                is an implementation of sliding_buffer/sliding_buffer_kernel_abstract.h
                is instantiated with T = unsigned char

            REQUIREMENTS ON fce_length
                is an implementation of entropy_encoder_model/entropy_encoder_model_kernel_abstract.h
                the alphabet_size of fce must be 513.  This will be used to encode the length of lz77 matches.
                fce_length and fcd share the same kernel number.

            REQUIREMENTS ON fcd_length
                is an implementation of entropy_decoder_model/entropy_decoder_model_kernel_abstract.h
                the alphabet_size of fcd must be 513.  This will be used to decode the length of lz77 matches.
                fce_length and fcd share the same kernel number.

            REQUIREMENTS ON fce_index
                is an implementation of entropy_encoder_model/entropy_encoder_model_kernel_abstract.h
                the alphabet_size of fce must be 32257.  This will be used to encode the index of lz77 matches.
                fce_index and fcd share the same kernel number.

            REQUIREMENTS ON fcd_index
                is an implementation of entropy_decoder_model/entropy_decoder_model_kernel_abstract.h
                the alphabet_size of fcd must be 32257.  This will be used to decode the index of lz77 matches.
                fce_index and fcd share the same kernel number.

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


        compress_stream_kernel_2 (
        )
        {}

        ~compress_stream_kernel_2 (
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
        compress_stream_kernel_2(compress_stream_kernel_2<fce,fcd,lz77_buffer,sliding_buffer,fce_length,fcd_length,fce_index,fcd_index>&);        // copy constructor
        compress_stream_kernel_2<fce,fcd,lz77_buffer,sliding_buffer,fce_length,fcd_length,fce_index,fcd_index>& operator=(compress_stream_kernel_2<fce,fcd,lz77_buffer,sliding_buffer,fce_length,fcd_length,fce_index,fcd_index>&);    // assignment operator

    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename fce,
        typename fcd,
        typename lz77_buffer,
        typename sliding_buffer,
        typename fce_length,
        typename fcd_length,
        typename fce_index,
        typename fcd_index
        >
    void compress_stream_kernel_2<fce,fcd,lz77_buffer,sliding_buffer,fce_length,fcd_length,fce_index,fcd_index>::
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
        fce_length model_length(coder);
        fce_index model_index(coder);

        const unsigned long LOOKAHEAD_LIMIT = 512; 
        lz77_buffer buffer(15,LOOKAHEAD_LIMIT);
        
      

        unsigned long count = 0;

        unsigned long lz77_count = 1;  // number of times we used lz77 to encode
        unsigned long ppm_count = 1;   // number of times we used ppm to encode


        while (true)
        {
            // write out a known value every 20000 symbols
            if (count == 20000)
            {
                count = 0;
                coder.encode(150,151,400);
            }
            ++count;

            // try to fill the lookahead buffer
            if (buffer.get_lookahead_buffer_size() < buffer.get_lookahead_buffer_limit())
            {
                temp = in.sbumpc();
                while (temp != EOF)
                {
                    buffer.add(static_cast<unsigned char>(temp));
                    if (buffer.get_lookahead_buffer_size() == buffer.get_lookahead_buffer_limit())
                        break;
                    temp = in.sbumpc();
                }
            }

            // compute the sum of ppm_count and lz77_count but make sure
            // it is less than 65536
            unsigned long sum = ppm_count + lz77_count;
            if (sum >= 65536)
            {
                ppm_count >>= 1;                    
                lz77_count >>= 1;
                ppm_count |= 1;
                lz77_count |= 1;
                sum = ppm_count+lz77_count;                    
            }

            // if there are still more symbols in the lookahead buffer to encode
            if (buffer.get_lookahead_buffer_size() > 0)  
            {
                unsigned long match_index, match_length;
                buffer.find_match(match_index,match_length,6);
                if (match_length != 0)
                {
                  
                    // signal the decoder that we are using lz77
                    coder.encode(0,lz77_count,sum);
                    ++lz77_count;
                    
                    // encode the index and length pair
                    model_index.encode(match_index);                   
                    model_length.encode(match_length);                   

                }
                else
                {

                    // signal the decoder that we are using ppm 
                    coder.encode(lz77_count,sum,sum);
                    ++ppm_count;

                    // encode the symbol using the ppm model
                    model.encode(buffer.lookahead_buffer(0));
                    buffer.shift_buffers(1);                    
                }
            }
            else
            {
                // signal the decoder that we are using ppm 
                coder.encode(lz77_count,sum,sum);
                

                model.encode(eof_symbol);
                break;      
            }
        } // while (true)        
    }

// ----------------------------------------------------------------------------------------

    template <
        typename fce,
        typename fcd,
        typename lz77_buffer,
        typename sliding_buffer,
        typename fce_length,
        typename fcd_length,
        typename fce_index,
        typename fcd_index
        >
    void compress_stream_kernel_2<fce,fcd,lz77_buffer,sliding_buffer,fce_length,fcd_length,fce_index,fcd_index>::
    decompress (
        std::istream& in_,
        std::ostream& out_
    ) const
    {

        std::streambuf& out = *out_.rdbuf();

        typename fcd::entropy_decoder_type coder;
        coder.set_stream(in_);

        fcd model(coder);
        fcd_length model_length(coder);
        fcd_index model_index(coder);

        unsigned long symbol;
        unsigned long count = 0;

        sliding_buffer buffer;
        buffer.set_size(15);

        
        unsigned long lz77_count = 1;  // number of times we used lz77 to encode
        unsigned long ppm_count = 1;   // number of times we used ppm to encode
        bool next_block_lz77;


        // decode until we hit the marker symbol
        while (true)
        {
            // make sure this is the value we expect
            if (count == 20000)
            {
                if (coder.get_target(400) != 150)
                {
                    throw decompression_error("Error detected in compressed data stream.");
                }
                count = 0;
                coder.decode(150,151);
            }
            ++count;


            // compute the sum of ppm_count and lz77_count but make sure
            // it is less than 65536
            unsigned long sum = ppm_count + lz77_count;
            if (sum >= 65536)
            {
                ppm_count >>= 1;                    
                lz77_count >>= 1;
                ppm_count |= 1;
                lz77_count |= 1;
                sum = ppm_count+lz77_count;                    
            }

            // check if we are decoding a lz77 or ppm block
            if (coder.get_target(sum) < lz77_count)
            {
                coder.decode(0,lz77_count);
                next_block_lz77 = true;
                ++lz77_count;
            }
            else
            {
                coder.decode(lz77_count,sum);
                next_block_lz77 = false;
                ++ppm_count;
            }


            if (next_block_lz77)
            {
                
                unsigned long match_length, match_index;
                // decode the match index
                model_index.decode(match_index);

                // decode the match length
                model_length.decode(match_length);

                
                match_index += match_length;
                buffer.rotate_left(match_length);
                for (unsigned long i = 0; i < match_length; ++i)
                {
                    unsigned char ch = buffer[match_index-i];
                    buffer[match_length-i-1] = ch;

                    // write this ch to out
                    if (out.sputc(static_cast<char>(ch)) != static_cast<int>(ch))
                    {
                        throw std::ios::failure("error occurred in compress_stream_kernel_2::decompress");
                    }
                }
                
            }
            else
            {

                // decode the next symbol
                model.decode(symbol);
                if (symbol != eof_symbol)
                {
                    buffer.rotate_left(1);
                    buffer[0] = static_cast<unsigned char>(symbol);
                    

                    // write this symbol to out
                    if (out.sputc(static_cast<char>(symbol)) != static_cast<int>(symbol))
                    {
                        throw std::ios::failure("error occurred in compress_stream_kernel_2::decompress");
                    }
                }
                else
                {
                    // this was the marker symbol so we are done
                    break;
                }
            }

        } // while (true)
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_COMPRESS_STREAM_KERNEl_2_

