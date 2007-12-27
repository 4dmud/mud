// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_THRESHOLDINg_ABSTRACT_
#ifdef DLIB_THRESHOLDINg_ABSTRACT_ 

#include "../pixel.h"

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
    );
    /*!
        requires
            - in_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - out_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - pixel_traits<typename in_image_type::type> is defined  
            - pixel_traits<typename out_image_type::type>::grayscale == true  
            - &in_img != &out_img
        ensures
            - #out_img == the thresholded version of in_img.  Pixels in in_img with 
              grayscale values >= thresh have an output value of on_pixel and all
              others have a value of off_pixel.
            - #out_img.width() == in_img.width()
            - #out_img.height() == in_img.height()
            - #out_img.at_start() == true
            - #in_img.at_start() == true
    !*/

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_THRESHOLDINg_ABSTRACT_ 


