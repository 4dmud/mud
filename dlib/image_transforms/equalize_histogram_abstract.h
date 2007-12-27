// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_EQUALIZE_HISTOGRAm_ABSTRACT_
#ifdef DLIB_EQUALIZE_HISTOGRAm_ABSTRACT_

#include "../pixel.h"

namespace dlib
{

    template <
        typename in_image_type,
        typename out_image_type 
        >
    void equalize_histogram (
        const in_image_type& in_img,
        out_image_type& out_img
    );
    /*!
        requires
            - in_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - out_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - pixel_traits<typename in_image_type::type> is defined  
            - pixel_traits<typename out_image_type::type> is defined  
        ensures
            - #out_img == the histogram equalized version of in_img
            - #out_img.width() == in_img.width()
            - #out_img.height() == in_img.height()
            - #out_img.at_start() == true
            - #in_img.at_start() == true
    !*/


}

#endif // DLIB_EQUALIZE_HISTOGRAm_ABSTRACT_


