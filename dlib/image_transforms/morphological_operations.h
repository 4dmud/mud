// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_MORPHOLOGICAL_OPERATIONs_
#define DLIB_MORPHOLOGICAL_OPERATIONs_

#include "../pixel.h"
#include "thresholding.h"
#include "morphological_operations_abstract.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    namespace morphological_operations_helpers
    {
        template <typename image_type>
        bool is_binary_image (
            const image_type& img
        )
        /*!
            ensures
                - returns true if img contains only on_pixel and off_pixel values.
                - returns false otherwise
        !*/
        {
            img.reset();
            bool result = true;
            while (img.move_next())
            {
                if (img.element() != on_pixel && img.element() != off_pixel)
                {
                    result = false;
                    break;
                }
            }
            img.reset();
            return result;
        }

        template <
            long M,
            long N
            >
        bool is_binary_image (
            const unsigned char (&structuring_element)[M][N]
        )
        /*!
            ensures
                - returns true if structuring_element contains only on_pixel and off_pixel values.
                - returns false otherwise
        !*/
        {
            for (long m = 0; m < M; ++m)
            {
                for (long n = 0; n < N; ++n)
                {
                    if (structuring_element[m][n] != on_pixel &&
                        structuring_element[m][n] != off_pixel)
                    {
                        return false;
                    }
                }
            }
            return true;
        }

    }

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type,
        typename out_image_type,
        long M,
        long N
        >
    void binary_dilation (
        const in_image_type& in_img,
        out_image_type& out_img,
        const unsigned char (&structuring_element)[M][N]
    )
    {
        using namespace morphological_operations_helpers;
        COMPILE_TIME_ASSERT(M%2 == 1);
        COMPILE_TIME_ASSERT(N%2 == 1);
        ASSERT((void*)&in_img != (void*)&out_img ,
            "\tvoid binary_dilation()"
            << "\n\tYou must give two different image objects"
            );
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type::type>::grayscale);
        ASSERT(is_binary_image(in_img) ,
            "\tvoid binary_dilation()"
            << "\n\tin_img must be a binary image"
            );
        ASSERT(is_binary_image(structuring_element) ,
            "\tvoid binary_dilation()"
            << "\n\tthe structuring_element must be a binary image"
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

        // apply the filter to the image
        for (long r = 0; r < in_img.height(); ++r)
        {
            for (long c = 0; c < in_img.width(); ++c)
            {
                unsigned char out_pixel = off_pixel;
                for (long m = 0; m < M && out_pixel == off_pixel; ++m)
                {
                    for (long n = 0; n < N && out_pixel == off_pixel; ++n)
                    {
                        if (structuring_element[m][n] == on_pixel)
                        {
                            // if this pixel is inside the image then get it from the image
                            // but if it isn't just pretend it was an off_pixel value
                            if (r+m >= M/2 && c+n >= N/2 &&
                                r+m-M/2 < in_img.height() && c+n-N/2 < in_img.width())
                            {
                                out_pixel = in_img[r+m-M/2][c+n-N/2];
                            }
                        }
                    }
                }
                assign_pixel(out_img[r][c], out_pixel);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type,
        typename out_image_type,
        long M,
        long N
        >
    void binary_erosion (
        const in_image_type& in_img,
        out_image_type& out_img,
        const unsigned char (&structuring_element)[M][N]
    )
    {
        using namespace morphological_operations_helpers;
        COMPILE_TIME_ASSERT(M%2 == 1);
        COMPILE_TIME_ASSERT(N%2 == 1);
        ASSERT((void*)&in_img != (void*)&out_img ,
            "\tvoid binary_erosion()"
            << "\n\tYou must give two different image objects"
            );
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type::type>::grayscale);
        ASSERT(is_binary_image(in_img) ,
            "\tvoid binary_erosion()"
            << "\n\tin_img must be a binary image"
            );
        ASSERT(is_binary_image(structuring_element) ,
            "\tvoid binary_erosion()"
            << "\n\tthe structuring_element must be a binary image"
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

        // apply the filter to the image
        for (long r = 0; r < in_img.height(); ++r)
        {
            for (long c = 0; c < in_img.width(); ++c)
            {
                unsigned char out_pixel = on_pixel;
                for (long m = 0; m < M && out_pixel == on_pixel; ++m)
                {
                    for (long n = 0; n < N && out_pixel == on_pixel; ++n)
                    {
                        if (structuring_element[m][n] == on_pixel)
                        {
                            // if this pixel is inside the image then get it from the image
                            // but if it isn't just pretend it was an off_pixel value
                            if (r+m >= M/2 && c+n >= N/2 &&
                                r+m-M/2 < in_img.height() && c+n-N/2 < in_img.width())
                            {
                                out_pixel = in_img[r+m-M/2][c+n-N/2];
                            }
                            else
                            {
                                out_pixel = off_pixel;
                            }
                        }
                    }
                }
                assign_pixel(out_img[r][c], out_pixel);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type,
        typename out_image_type,
        long M,
        long N
        >
    void binary_open (
        const in_image_type& in_img,
        out_image_type& out_img,
        const unsigned char (&structuring_element)[M][N],
        const unsigned long iter = 1
    )
    {
        using namespace morphological_operations_helpers;
        COMPILE_TIME_ASSERT(M%2 == 1);
        COMPILE_TIME_ASSERT(N%2 == 1);
        ASSERT((void*)&in_img != (void*)&out_img ,
            "\tvoid binary_open()"
            << "\n\tYou must give two different image objects"
            );
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type::type>::grayscale);
        ASSERT(is_binary_image(in_img) ,
            "\tvoid binary_open()"
            << "\n\tin_img must be a binary image"
            );
        ASSERT(is_binary_image(structuring_element) ,
            "\tvoid binary_open()"
            << "\n\tthe structuring_element must be a binary image"
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

        if (iter == 0)
        {
            // just copy the image over
            while (in_img.move_next())
            {
                out_img.move_next();
                assign_pixel(out_img.element(),in_img.element());
            }
            out_img.reset();
            in_img.reset();
        }
        else if (iter == 1)
        {
            in_image_type temp;
            binary_erosion(in_img,temp,structuring_element);
            binary_dilation(temp,out_img,structuring_element);
        }
        else
        {
            in_image_type temp1, temp2;
            binary_erosion(in_img,temp1,structuring_element);

            // do the extra erosions
            for (unsigned long i = 1; i < iter; ++i)
            {
                temp1.swap(temp2);
                binary_erosion(temp2,temp1,structuring_element);
            }

            // do the extra dilations 
            for (unsigned long i = 1; i < iter; ++i)
            {
                temp1.swap(temp2);
                binary_dilation(temp2,temp1,structuring_element);
            }

            binary_dilation(temp1,out_img,structuring_element);
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type,
        typename out_image_type,
        long M,
        long N
        >
    void binary_close (
        const in_image_type& in_img,
        out_image_type& out_img,
        const unsigned char (&structuring_element)[M][N],
        const unsigned long iter = 1
    )
    {
        using namespace morphological_operations_helpers;
        COMPILE_TIME_ASSERT(M%2 == 1);
        COMPILE_TIME_ASSERT(N%2 == 1);
        ASSERT((void*)&in_img != (void*)&out_img ,
            "\tvoid binary_close()"
            << "\n\tYou must give two different image objects"
            );
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type::type>::grayscale);
        ASSERT(is_binary_image(in_img) ,
            "\tvoid binary_close()"
            << "\n\tin_img must be a binary image"
            );
        ASSERT(is_binary_image(structuring_element) ,
            "\tvoid binary_close()"
            << "\n\tthe structuring_element must be a binary image"
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

        if (iter == 0)
        {
            // just copy the image over
            while (in_img.move_next())
            {
                out_img.move_next();
                assign_pixel(out_img.element(),in_img.element());
            }
            out_img.reset();
            in_img.reset();
        }
        else if (iter == 1)
        {
            in_image_type temp;
            binary_dilation(in_img,temp,structuring_element);
            binary_erosion(temp,out_img,structuring_element);
        }
        else
        {
            in_image_type temp1, temp2;
            binary_dilation(in_img,temp1,structuring_element);

            // do the extra dilations 
            for (unsigned long i = 1; i < iter; ++i)
            {
                temp1.swap(temp2);
                binary_dilation(temp2,temp1,structuring_element);
            }

            // do the extra erosions 
            for (unsigned long i = 1; i < iter; ++i)
            {
                temp1.swap(temp2);
                binary_erosion(temp2,temp1,structuring_element);
            }

            binary_erosion(temp1,out_img,structuring_element);
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type1,
        typename in_image_type2,
        typename out_image_type
        >
    void binary_intersection (
        const in_image_type1& in_img1,
        const in_image_type2& in_img2,
        out_image_type& out_img
    )
    {
        using namespace morphological_operations_helpers;
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type1::type>::grayscale);
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type2::type>::grayscale);
        ASSERT(is_binary_image(in_img1) ,
            "\tvoid binary_intersection()"
            << "\n\tin_img1 must be a binary image"
            );
        ASSERT(is_binary_image(in_img2) ,
            "\tvoid binary_intersection()"
            << "\n\tin_img2 must be a binary image"
            );
        ASSERT(in_img1.width() == in_img2.width(),
            "\tvoid binary_intersection()"
            << "\n\tin_img1 and in_img2 must have the same widths."
            << "\n\tin_img1.width(): " << in_img1.width()
            << "\n\tin_img2.width(): " << in_img2.width()
            );
        ASSERT(in_img1.height() == in_img2.height(),
            "\tvoid binary_intersection()"
            << "\n\tin_img1 and in_img2 must have the same heights."
            << "\n\tin_img1.height(): " << in_img1.height()
            << "\n\tin_img2.height(): " << in_img2.height()
            );
            


        // if there isn't any input image then don't do anything
        if (in_img1.size() == 0)
        {
            in_img1.reset();
            in_img2.reset();
            out_img.clear();
            return;
        }

        out_img.set_size(in_img1.width(),in_img1.height());
        in_img1.reset();
        in_img2.reset();

        for (long r = 0; r < in_img1.height(); ++r)
        {
            for (long c = 0; c < in_img1.width(); ++c)
            {
                if (in_img1[r][c] == on_pixel && in_img2[r][c] == on_pixel)
                    assign_pixel(out_img[r][c], on_pixel);
                else
                    assign_pixel(out_img[r][c], off_pixel);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type1,
        typename in_image_type2,
        typename out_image_type
        >
    void binary_difference (
        const in_image_type1& in_img1,
        const in_image_type2& in_img2,
        out_image_type& out_img
    )
    {
        using namespace morphological_operations_helpers;
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type1::type>::grayscale);
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type2::type>::grayscale);
        ASSERT(is_binary_image(in_img1) ,
            "\tvoid binary_difference()"
            << "\n\tin_img1 must be a binary image"
            );
        ASSERT(is_binary_image(in_img2) ,
            "\tvoid binary_difference()"
            << "\n\tin_img2 must be a binary image"
            );
        ASSERT(in_img1.width() == in_img2.width(),
            "\tvoid binary_difference()"
            << "\n\tin_img1 and in_img2 must have the same widths."
            << "\n\tin_img1.width(): " << in_img1.width()
            << "\n\tin_img2.width(): " << in_img2.width()
            );
        ASSERT(in_img1.height() == in_img2.height(),
            "\tvoid binary_difference()"
            << "\n\tin_img1 and in_img2 must have the same heights."
            << "\n\tin_img1.height(): " << in_img1.height()
            << "\n\tin_img2.height(): " << in_img2.height()
            );
            


        // if there isn't any input image then don't do anything
        if (in_img1.size() == 0)
        {
            in_img1.reset();
            in_img2.reset();
            out_img.clear();
            return;
        }

        out_img.set_size(in_img1.width(),in_img1.height());
        in_img1.reset();
        in_img2.reset();

        for (long r = 0; r < in_img1.height(); ++r)
        {
            for (long c = 0; c < in_img1.width(); ++c)
            {
                if (in_img1[r][c] == on_pixel && in_img2[r][c] == off_pixel)
                    assign_pixel(out_img[r][c], on_pixel);
                else
                    assign_pixel(out_img[r][c], off_pixel);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename in_image_type,
        typename out_image_type
        >
    void binary_complement (
        const in_image_type& in_img,
        out_image_type& out_img
    )
    {
        using namespace morphological_operations_helpers;
        COMPILE_TIME_ASSERT(pixel_traits<typename in_image_type::type>::grayscale);
        ASSERT(is_binary_image(in_img) ,
            "\tvoid binary_complement()"
            << "\n\tin_img must be a binary image"
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

        for (long r = 0; r < in_img.height(); ++r)
        {
            for (long c = 0; c < in_img.width(); ++c)
            {
                if (in_img[r][c] == on_pixel)
                    assign_pixel(out_img[r][c], off_pixel);
                else
                    assign_pixel(out_img[r][c], on_pixel);
            }
        }
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_MORPHOLOGICAL_OPERATIONs_

