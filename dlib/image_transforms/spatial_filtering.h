// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_SPATIAL_FILTERINg_H_
#define DLIB_SPATIAL_FILTERINg_H_

#include "../pixel.h"
#include "spatial_filtering_abstract.h"
#include "../algs.h"
#include "../assert.h"

namespace dlib
{
    template <
        typename in_image_type,
        typename out_image_type,
        typename filter_type,
        long M,
        long N
        >
    void spatially_filter_image (
        const in_image_type& in_img,
        out_image_type& out_img,
        const filter_type (&filter)[M][N],
        unsigned char scale = 1
    )
    {
        COMPILE_TIME_ASSERT(M%2 == 1);
        COMPILE_TIME_ASSERT(N%2 == 1);
        ASSERT(scale > 0,
            "\tvoid spatially_filter_image()"
            << "\n\tYou can't give a scale of zero"
            );
        ASSERT((void*)&in_img != (void*)&out_img ,
            "\tvoid spatially_filter_image()"
            << "\n\tYou must give two different image objects"
            );



        // if there isn't any input image then don't do anything
        if (in_img.size() == 0)
        {
            in_img.reset();
            out_img.clear();
            return;
        }

        out_img.set_size(in_img.width(),in_img.height());
        in_img.reset();

        // figure out the range that we should apply the filter to
        const long first_row = M/2;
        const long first_col = N/2;
        const long last_row = in_img.height() - M/2;
        const long last_col = in_img.width() - N/2;

        // apply the filter to the image
        for (long r = first_row; r < last_row; ++r)
        {
            for (long c = first_col; c < last_col; ++c)
            {
                hsi_pixel p;
                long temp = 0;
                for (long m = 0; m < M; ++m)
                {
                    for (long n = 0; n < N; ++n)
                    {
                        // pull out the current pixel and put it into p
                        assign_pixel(p,in_img[r-M/2+m][c-N/2+n]);
                        temp += static_cast<long>(p.i)*filter[m][n];
                    }
                }

                temp /= scale;

                // pull out our target pixel so we will have the right values for h and s
                assign_pixel(p,in_img[r][c]);

                // Catch any underflow or overflow that would happen when we convert back 
                // to unsigned char before it happens.
                if (temp < 0)
                    temp = 0;
                else if (temp > 255)
                    temp = 255;

                // apply our new value for the intensity
                p.i = static_cast<unsigned char>(temp);
                // save this pixel to the output image
                assign_pixel(out_img[r][c],p);
            }
        }
    }

}

#endif // DLIB_SPATIAL_FILTERINg_H_




