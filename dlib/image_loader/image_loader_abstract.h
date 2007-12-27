// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_IMAGE_LOADEr_ABSTRACT_
#ifdef DLIB_IMAGE_LOADEr_ABSTRACT_

#include <iosfwd>
#include "../algs.h"
#include "../pixel.h"

namespace dlib
{
    class image_load_error : public dlib::error 
    { 
        /*!
            WHAT THIS OBJECT REPRESENTS
                This is an exception used to indicate a failure to load an image.
                Its type member variable will be set to EIMAGE_LOAD.
        !*/
    };

// ----------------------------------------------------------------------------------------

    template <
        typename image_type 
        >
    void load_bmp (
        image_type& image,
        std::istream& in
    );
    /*!
        requires
            - image_type == is an implemenation of array2d/array2d_kernel_abstract.h
            - pixel_traits<typename image_type::type> is defined  
        ensures
            - #image == the image of the MS Windows BMP file that was available 
              in the input stream in.  
            - #image[0][0] will be the upper left corner of the image 
            - #image[image.height()-1][image.width()-1] will be the lower right
              corner of the image
        throws
            - image_load_error
                This exception is thrown if there is an error that prevents us
                from loading the image.  If this exception is thrown then 
                #image will have an initial value for its type.
            - std::bad_alloc 
                If this exception is thrown then #image will have an initial
                value for its type.
    !*/

// ----------------------------------------------------------------------------------------

    /*!
        dlib dng file format:
            This is a file format I created for this library.  It is a lossless 
            compressed image format that is similar to the PNG format but uses
            the dlib PPM compression algorithms instead of the DEFLATE algorithm.
    !*/

    template <
        typename image_type 
        >
    void load_dng (
        image_type& image,
        std::istream& in
    );
    /*!
        requires
            - image_type == is an implemenation of array2d/array2d_kernel_abstract.h
            - pixel_traits<typename image_type::type> is defined  
        ensures
            - #image == the image of the dlib dng file that was available 
              in the input stream in. 
            - #image[0][0] will be the upper left corner of the image 
            - #image[image.height()-1][image.width()-1] will be the lower right
              corner of the image
        throws
            - image_load_error
                This exception is thrown if there is an error that prevents us
                from loading the image.  If this exception is thrown then 
                #image will have an initial value for its type.
            - std::bad_alloc 
                If this exception is thrown then #image will have an initial
                value for its type.
    !*/

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_IMAGE_LOADEr_ABSTRACT_

