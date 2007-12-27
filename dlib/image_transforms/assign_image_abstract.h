// Copyright (C) 2007  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_ASSIGN_IMAGe_ABSTRACT
#ifdef DLIB_ASSIGN_IMAGe_ABSTRACT

#include "../pixel.h"

namespace dlib
{

    template <
        typename dest_image_type,
        typename src_image_type
        >
    void assign_image (
        dest_image_type& dest,
        const src_image_type& src
    );
    /*!
        requires
            - src_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - dest_image_type == is an implementation of array2d/array2d_kernel_abstract.h
            - pixel_traits<typename src_image_type::type> is defined  
            - pixel_traits<typename dest_image_type::type> is defined  
        ensures
            - #dest_img.width() == src_img.width()
            - #dest_img.height() == src_img.height()
            - #dest_img.at_start() == true
            - #src_img.at_start() == true
            - for all valid r and c:
                - performs assign_pixel(#dest_img[r][c],src_img[r][c]) 
                  (i.e. coppies the src image to dest image)
    !*/


}

#endif // DLIB_ASSIGN_IMAGe_ABSTRACT


