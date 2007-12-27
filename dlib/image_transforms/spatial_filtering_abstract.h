// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_SPATIAL_FILTERINg_ABSTRACT_
#ifdef DLIB_SPATIAL_FILTERINg_ABSTRACT_

#include "../pixel.h"

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
    );
    /*!
        requires
            - in_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - out_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - pixel_traits<typename in_image_type::type> is defined  
            - pixel_traits<typename out_image_type::type> is defined  
            - &in_img != &out_img
            - scale > 0
            - M % 2 == 1  (i.e. M must be odd)
            - N % 2 == 1  (i.e. N must be odd)
        ensures
            - Applies the given spatial filter to in_img and stores the result in out_img.  Also 
              divides each resulting pixel by scale.  
            - Pixels close enough to the edge of in_img to not have the filter still fit 
              inside the image are copied over without modification to out_img.
            - #out_img.width() == in_img.width()
            - #out_img.height() == in_img.height()
            - #out_img.at_start() == true
            - #in_img.at_start() == true
    !*/


}

#endif // DLIB_SPATIAL_FILTERINg_ABSTRACT_

