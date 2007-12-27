// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_THRESHOLDINg_
#define DLIB_THRESHOLDINg_ 

#include "../pixel.h"
#include "thresholding_abstract.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    const unsigned char on_pixel = 255;
    const unsigned char off_pixel = 0;

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type,
        typename out_image_type
        >
    void threshold_image (
        const in_image_type& in_img,
        out_image_type& out_img,
        unsigned char thresh
    )
    {
        ASSERT((void*)&in_img != (void*)&out_img ,
            "\tvoid threshold_image()"
            << "\n\tYou must give two different image objects"
            );
        COMPILE_TIME_ASSERT(pixel_traits<typename out_image_type::type>::grayscale);

        // if there isn't any input image then don't do anything
        if (in_img.size() == 0)
        {
            in_img.reset();
            out_img.clear();
            return;
        }

        out_img.set_size(in_img.width(),in_img.height());
        in_img.reset();

        while (in_img.move_next())
        {
            out_img.move_next();
            unsigned char p;
            assign_pixel(p,in_img.element());
            if (p >= thresh)
                p = on_pixel;
            else
                p = off_pixel;
            out_img.element() = p;
        }

        in_img.reset();
        out_img.reset();
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_THRESHOLDINg_ 

