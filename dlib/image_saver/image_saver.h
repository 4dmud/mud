// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_IMAGE_SAVEr_
#define DLIB_IMAGE_SAVEr_

#include "image_saver_abstract.h"
#include <iostream>
#include <sstream>
#include "../algs.h"
#include "../pixel.h"
#include "../byte_orderer.h"
#include "../entropy_encoder.h"
#include "../entropy_encoder_model.h"
#include "dng_shared.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    class image_save_error : public dlib::error { 
    public: image_save_error(const std::string& str) : error(EIMAGE_SAVE,str){}
    };

// ----------------------------------------------------------------------------------------

    template <
        typename image_type,
        bool grayscale = pixel_traits<typename image_type::type>::grayscale
        >
    struct save_bmp_helper;


    template <typename image_type>
    struct save_bmp_helper<image_type,false>
    {
        static void save_bmp (
            const image_type& image,
            std::ostream& out 
        )
        {
            // we are going to write out a 24bit color image.
            byte_orderer::kernel_1a bo;

            out.write("BM",2);
            
            if (!out)
                throw image_save_error("error writing image to output stream");


            unsigned long pad = 4 - (image.width()*3)%4;
            if (pad == 4)
                pad = 0;

            unsigned long bfSize = 14 + 40 + (image.width()*3 + pad)*image.height();
            unsigned long bfReserved = 0;
            unsigned long bfOffBits = 14 + 40;
            unsigned long biSize = 40;
            unsigned long biWidth = image.width();
            unsigned long biHeight = image.height();
            unsigned short biPlanes = 1;
            unsigned short biBitCount = 24;
            unsigned long biCompression = 0;
            unsigned long biSizeImage = 0;
            unsigned long biXPelsPerMeter = 0;
            unsigned long biYPelsPerMeter = 0;
            unsigned long biClrUsed = 0;
            unsigned long biClrImportant = 0;

            bo.host_to_little(bfSize);
            bo.host_to_little(bfOffBits);
            bo.host_to_little(biSize);
            bo.host_to_little(biWidth);
            bo.host_to_little(biHeight);
            bo.host_to_little(biPlanes);
            bo.host_to_little(biBitCount);

            out.write((char*)&bfSize,4);
            out.write((char*)&bfReserved,4);
            out.write((char*)&bfOffBits,4);
            out.write((char*)&biSize,4);
            out.write((char*)&biWidth,4);
            out.write((char*)&biHeight,4);
            out.write((char*)&biPlanes,2);
            out.write((char*)&biBitCount,2);
            out.write((char*)&biCompression,4);
            out.write((char*)&biSizeImage,4);
            out.write((char*)&biXPelsPerMeter,4);
            out.write((char*)&biYPelsPerMeter,4);
            out.write((char*)&biClrUsed,4);
            out.write((char*)&biClrImportant,4);


            if (!out)
                throw image_save_error("error writing image to output stream");

            // now we write out the pixel data
            for (long row = image.height()-1; row >= 0; --row)
            {
                for (long col = 0; col < image.width(); ++col)
                {
                    rgb_pixel p;
                    assign_pixel(p,image[row][col]);
                    out.write((char*)&p.blue,1);
                    out.write((char*)&p.green,1);
                    out.write((char*)&p.red,1);
                }

                // write out some zeros so that this line is a multiple of 4 bytes
                for (unsigned long i = 0; i < pad; ++i)
                {
                    unsigned char p = 0;
                    out.write((char*)&p,1);
                }
            }

            if (!out)
                throw image_save_error("error writing image to output stream");
        }
    };

    template <typename image_type>
    struct save_bmp_helper<image_type,true>
    {
        static void save_bmp (
            const image_type& image,
            std::ostream& out
        )
        {
            // we are going to write out an 8bit color image.
            byte_orderer::kernel_1a bo;

            out.write("BM",2);
            
            if (!out)
                throw image_save_error("error writing image to output stream");

            unsigned long pad = 4 - image.width()%4;
            if (pad == 4)
                pad = 0;

            unsigned long bfSize = 14 + 40 + (image.width() + pad)*image.height() + 256*4;
            unsigned long bfReserved = 0;
            unsigned long bfOffBits = 14 + 40 + 256*4;
            unsigned long biSize = 40;
            unsigned long biWidth = image.width();
            unsigned long biHeight = image.height();
            unsigned short biPlanes = 1;
            unsigned short biBitCount = 8;
            unsigned long biCompression = 0;
            unsigned long biSizeImage = 0;
            unsigned long biXPelsPerMeter = 0;
            unsigned long biYPelsPerMeter = 0;
            unsigned long biClrUsed = 0;
            unsigned long biClrImportant = 0;

            bo.host_to_little(bfSize);
            bo.host_to_little(bfOffBits);
            bo.host_to_little(biSize);
            bo.host_to_little(biWidth);
            bo.host_to_little(biHeight);
            bo.host_to_little(biPlanes);
            bo.host_to_little(biBitCount);

            out.write((char*)&bfSize,4);
            out.write((char*)&bfReserved,4);
            out.write((char*)&bfOffBits,4);
            out.write((char*)&biSize,4);
            out.write((char*)&biWidth,4);
            out.write((char*)&biHeight,4);
            out.write((char*)&biPlanes,2);
            out.write((char*)&biBitCount,2);
            out.write((char*)&biCompression,4);
            out.write((char*)&biSizeImage,4);
            out.write((char*)&biXPelsPerMeter,4);
            out.write((char*)&biYPelsPerMeter,4);
            out.write((char*)&biClrUsed,4);
            out.write((char*)&biClrImportant,4);


            // write out the color palette
            for (unsigned int i = 0; i <= 255; ++i)
            {
                unsigned char ch = static_cast<unsigned char>(i);
                out.write((char*)&ch,1);
                out.write((char*)&ch,1);
                out.write((char*)&ch,1);
                ch = 0;
                out.write((char*)&ch,1);
            }

            if (!out)
                throw image_save_error("error writing image to output stream");

            // now we write out the pixel data
            for (long row = image.height()-1; row >= 0; --row)
            {
                for (long col = 0; col < image.width(); ++col)
                {
                    unsigned char p;
                    assign_pixel(p,image[row][col]);
                    out.write((char*)&p,1);
                }

                // write out some zeros so that this line is a multiple of 4 bytes
                for (unsigned long i = 0; i < pad; ++i)
                {
                    unsigned char p = 0;
                    out.write((char*)&p,1);
                }
            }

            if (!out)
                throw image_save_error("error writing image to output stream");

        }
    };

// ----------------------------------------------------------------------------------------

    template <
        typename image_type 
        >
    inline void save_bmp (
        const image_type& image,
        std::ostream& out
    )
    {
        save_bmp_helper<image_type>::save_bmp(image,out);
    }

// ----------------------------------------------------------------------------------------

    namespace dng_helpers_namespace
    {

        template <
            typename image_type,
            int pixel_type = static_switch <
                pixel_traits<typename image_type::type>::grayscale,
                pixel_traits<typename image_type::type>::rgb,
                pixel_traits<typename image_type::type>::hsi
                >::value
            >
        struct save_dng_helper;

        typedef entropy_encoder::kernel_2a encoder_type;
        typedef entropy_encoder_model<256,encoder_type>::kernel_5a eem_type; 

        template <typename image_type>
        struct save_dng_helper<image_type,grayscale>
        {
            static void save_dng (
                const image_type& image,
                std::ostream& out 
            )
            {
                out.write("DNG",3);
                unsigned long version = 1;
                serialize(version,out);
                unsigned long type = grayscale;
                serialize(type,out);
                serialize(image.width(),out);
                serialize(image.height(),out);

                encoder_type encoder;
                encoder.set_stream(out);

                eem_type eem(encoder);
                for (long r = 0; r < image.nr(); ++r)
                {
                    for (long c = 0; c < image.nc(); ++c)
                    {
                        eem.encode((unsigned char)(image[r][c] - predictor_grayscale(image,r,c)));
                    }
                }
                // write out the magic byte to make the end of the data
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
            }
        };

        template <typename image_type>
        struct save_dng_helper<image_type,rgb>
        {
            static void save_dng (
                const image_type& image,
                std::ostream& out
            )
            {
                out.write("DNG",3);
                unsigned long version = 1;
                serialize(version,out);

                unsigned long type = rgb;
                // if this is a small image then we will use a different predictor
                if (image.size() < 4000)
                    type = rgb_paeth;

                serialize(type,out);
                serialize(image.width(),out);
                serialize(image.height(),out);

                encoder_type encoder;
                encoder.set_stream(out);

                rgb_pixel pre, cur;
                eem_type eem(encoder);

                if (type == rgb)
                {
                    for (long r = 0; r < image.nr(); ++r)
                    {
                        for (long c = 0; c < image.nc(); ++c)
                        {
                            pre = predictor_rgb(image,r,c);
                            cur = image[r][c];

                            eem.encode((unsigned char)(cur.red - pre.red));
                            eem.encode((unsigned char)(cur.green - pre.green));
                            eem.encode((unsigned char)(cur.blue - pre.blue));
                        }
                    }
                }
                else
                {
                    for (long r = 0; r < image.nr(); ++r)
                    {
                        for (long c = 0; c < image.nc(); ++c)
                        {
                            pre = predictor_rgb_paeth(image,r,c);
                            cur = image[r][c];

                            eem.encode((unsigned char)(cur.red - pre.red));
                            eem.encode((unsigned char)(cur.green - pre.green));
                            eem.encode((unsigned char)(cur.blue - pre.blue));
                        }
                    }
                }
                // write out the magic byte to make the end of the data
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
            }
        };

        template <typename image_type>
        struct save_dng_helper<image_type,hsi>
        {
            static void save_dng (
                const image_type& image,
                std::ostream& out
            )
            {
                out.write("DNG",3);
                unsigned long version = 1;
                serialize(version,out);
                unsigned long type = hsi;
                serialize(type,out);
                serialize(image.width(),out);
                serialize(image.height(),out);

                encoder_type encoder;
                encoder.set_stream(out);

                hsi_pixel pre, cur;
                eem_type eem(encoder);
                for (long r = 0; r < image.nr(); ++r)
                {
                    for (long c = 0; c < image.nc(); ++c)
                    {
                        pre = predictor_hsi(image,r,c);
                        cur = image[r][c];

                        eem.encode((unsigned char)(cur.h - pre.h));
                        eem.encode((unsigned char)(cur.s - pre.s));
                        eem.encode((unsigned char)(cur.i - pre.i));
                    }
                }
                // write out the magic byte to make the end of the data
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
                eem.encode(dng_magic_byte);
            }
        };
    }

// ----------------------------------------------------------------------------------------

    template <
        typename image_type 
        >
    inline void save_dng (
        const image_type& image,
        std::ostream& out
    )
    {
        using namespace dng_helpers_namespace;
        save_dng_helper<image_type>::save_dng(image,out);
    }

// ----------------------------------------------------------------------------------------


}

#endif // DLIB_IMAGE_SAVEr_




