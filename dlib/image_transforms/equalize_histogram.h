// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_EQUALIZE_HISTOGRAm_
#define DLIB_EQUALIZE_HISTOGRAm_

#include "../pixel.h"
#include "equalize_histogram_abstract.h"

namespace dlib
{

    template <
        typename in_image_type,
        typename out_image_type 
        >
    void equalize_histogram (
        const in_image_type& in_img,
        out_image_type& out_img
    )
    {
        // if there isn't any input image then don't do anything
        if (in_img.size() == 0)
        {
            in_img.reset();
            out_img.clear();
            return;
        }

        out_img.set_size(in_img.width(),in_img.height());
        hsi_pixel p;

        unsigned long histogram[256];
        for (unsigned long i = 0; i < 256; ++i)
            histogram[i] = 0;


        // compute the histogram 
        in_img.reset();
        while (in_img.move_next())
        {
            assign_pixel(p,in_img.element());
            ++histogram[p.i];
        }

        const double scale = 255.0/in_img.size();

        // compute the transform function
        for (unsigned long i = 1; i < 256; ++i)
            histogram[i] += histogram[i-1];
        // scale so that it is in the range [0,255]
        for (unsigned long i = 0; i < 256; ++i)
            histogram[i] = static_cast<unsigned long>(histogram[i]*scale);

        // now do the transform
        for (long row = 0; row < in_img.height(); ++row)
        {
            for (long col = 0; col < in_img.width(); ++col)
            {
                assign_pixel(p,in_img[row][col]);
                p.i = histogram[p.i];
                assign_pixel(out_img[row][col],p);
            }
        }

        in_img.reset();
        out_img.reset();
    }


}

#endif // DLIB_EQUALIZE_HISTOGRAm_



