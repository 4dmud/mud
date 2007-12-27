// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_PIXEl_ 
#define DLIB_PIXEl_

#include <iostream>
#include "serialize.h"
#include <cmath>
#include "algs.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    /*!
        This file contains defintions of pixel objects and related classes and
        functionality.
    !*/

// ----------------------------------------------------------------------------------------

    template <typename T>
    struct pixel_traits;
    /*!
        WHAT THIS OBJECT REPRESENTS
            As the name implies, this is a traits class for pixel types.
            It defines the properties of a pixel (duah).

        This traits class will define the following public constants:
            - grayscale
            - rgb
            - hsi

            - if (rgb == true) then
                - grayscale == false
                - hsi == false
                - the type T will be a struct with 3 public members of type 
                  unsigned char named "red" "green" and "blue".  
                - This type of pixel represents the RGB color space.
            - else if (hsi == true) then
                - grayscale == false
                - rgb == false
                - the type T will be a struct with 3 public members of type
                  unsigned char named "h" "s" and "i".  
                - This type of pixel represents the HSI color space.
            - else
                - rgb == false
                - hsi == false
                - grayscale == true
                - the type T will be an unsigned char type.
                - This type of pixel represents a grayscale color space
    !*/

// ----------------------------------------------------------------------------------------

    struct rgb_pixel
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This is a simple struct that represents an RGB colored graphical pixel.
        !*/

        unsigned char red;
        unsigned char green;
        unsigned char blue;
    };

// ----------------------------------------------------------------------------------------

    struct hsi_pixel
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This is a simple struct that represents an HSI colored graphical pixel.
        !*/

        unsigned char h;
        unsigned char s;
        unsigned char i;
    };

// ----------------------------------------------------------------------------------------

    template <
        typename P1,
        typename P2  
        >
    inline void assign_pixel (
        P1& dest,
        const P2& src
    );
    /*!
        requires
            - pixel_traits<P1> must be defined
            - pixel_traits<P2> must be defined
        ensures
            - assigns pixel src to pixel dest and does any necessary conversions. 
    !*/

// ----------------------------------------------------------------------------------------

    void serialize (
        const rgb_pixel& item, 
        std::ostream& out 
    );   
    /*!
        provides serialization support for the rgb_pixel struct
    !*/

// ----------------------------------------------------------------------------------------

    void deserialize (
        rgb_pixel& item, 
        std::istream& in
    );   
    /*!
        provides deserialization support for the rgb_pixel struct
    !*/

// ----------------------------------------------------------------------------------------

    void serialize (
        const hsi_pixel& item, 
        std::ostream& out 
    );   
    /*!
        provides serialization support for the hsi_pixel struct
    !*/

// ----------------------------------------------------------------------------------------

    void deserialize (
        hsi_pixel& item, 
        std::istream& in
    );   
    /*!
        provides deserialization support for the hsi_pixel struct
    !*/

// ----------------------------------------------------------------------------------------

    template <>
    struct pixel_traits<rgb_pixel>
    {
        const static bool rgb  = true;
        const static bool grayscale = false;
        const static bool hsi = false;
    };

// ----------------------------------------------------------------------------------------

    template <>
    struct pixel_traits<hsi_pixel>
    {
        const static bool rgb  = false;
        const static bool grayscale = false;
        const static bool hsi = true;
    };

// ----------------------------------------------------------------------------------------
    
    template <>
    struct pixel_traits<unsigned char>
    {
        const static bool rgb  = false;
        const static bool grayscale = true;
        const static bool hsi = false;
    };

// ----------------------------------------------------------------------------------------

    // The following is a bunch of conversion stuff for the assign_pixel function.

    namespace assign_pixel_helpers
    {
        enum
        {
            grayscale = 1,
            rgb,
            hsi
        };

        template <
            typename P1,
            typename P2,
            int p1_type = static_switch<
                pixel_traits<P1>::grayscale,
                pixel_traits<P1>::rgb,
                pixel_traits<P1>::hsi  >::value,
            int p2_type = static_switch<
                pixel_traits<P2>::grayscale,
                pixel_traits<P2>::rgb,
                pixel_traits<P2>::hsi  >::value
            >
        struct helper;

    // -----------------------------
        // all the same kind 

        template < typename P1, typename P2 >
        struct helper<P1,P2,grayscale,grayscale>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                dest = src;
            }
        };

        template < typename P1, typename P2 >
        struct helper<P1,P2,rgb,rgb>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                dest.red = src.red; 
                dest.green = src.green; 
                dest.blue = src.blue; 
            }
        };

        template < typename P1, typename P2 >
        struct helper<P1,P2,hsi,hsi>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                dest.h = src.h; 
                dest.s = src.s; 
                dest.i = src.i; 
            }
        };

    // -----------------------------
        // dest is a grayscale

        template < typename P1, typename P2 >
        struct helper<P1,P2,grayscale,rgb>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                dest = (unsigned char)((static_cast<unsigned int>(src.red) +
                        static_cast<unsigned int>(src.green) +  
                        static_cast<unsigned int>(src.blue))/3); 
            }
        };

        template < typename P1, typename P2 >
        struct helper<P1,P2,grayscale,hsi>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                dest = src.i;
            }
        };


    // -----------------------------

        struct HSL
        {
            double h;
            double s;
            double l;
        };

        struct COLOUR
        {
            double r;
            double g;
            double b;
        };

        /*
        Calculate HSL from RGB
        Hue is in degrees
        Lightness is between 0 and 1
        Saturation is between 0 and 1
        */
        HSL RGB2HSL(COLOUR c1);

        /*
        Calculate RGB from HSL, reverse of RGB2HSL()
        Hue is in degrees
        Lightness is between 0 and 1
        Saturation is between 0 and 1
        */
        COLOUR HSL2RGB(HSL c1);


    // -----------------------------
        // dest is a color rgb_pixel

        template < typename P1, typename P2 >
        struct helper<P1,P2,rgb,grayscale>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                dest.red = src; 
                dest.green = src; 
                dest.blue = src; 
            }
        };

        template < typename P1, typename P2 >
        struct helper<P1,P2,rgb,hsi>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                COLOUR c;
                HSL h;
                h.h = src.h;
                h.h = h.h/255.0*360;
                h.s = src.s/255.0;
                h.l = src.i/255.0;
                c = HSL2RGB(h);

                dest.red = static_cast<unsigned char>(c.r*255.0);
                dest.green = static_cast<unsigned char>(c.g*255.0);
                dest.blue = static_cast<unsigned char>(c.b*255.0);
            }
        };

    // -----------------------------
        // dest is an hsi pixel

        template < typename P1, typename P2 >
        struct helper<P1,P2,hsi,grayscale>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                dest.h = 0;
                dest.s = 0;
                dest.i = src;
            }
        };

        template < typename P1, typename P2 >
        struct helper<P1,P2,hsi,rgb>
        {
            static void assign(P1& dest, const P2& src) 
            { 
                COLOUR c1;
                HSL c2;
                c1.r = src.red/255.0;
                c1.g = src.green/255.0;
                c1.b = src.blue/255.0;
                c2 = RGB2HSL(c1);
                
                dest.h = static_cast<unsigned char>(c2.h/360.0*255.0);
                dest.s = static_cast<unsigned char>(c2.s*255.0);
                dest.i = static_cast<unsigned char>(c2.l*255.0);
            }
        };
    }

    // -----------------------------

    template < typename P1, typename P2 >
    inline void assign_pixel (
        P1& dest,
        const P2& src
    ) { assign_pixel_helpers::helper<P1,P2>::assign(dest,src); }

// ----------------------------------------------------------------------------------------

}

#ifdef NO_MAKEFILE
#include "pixel.cpp"
#endif


#endif // DLIB_PIXEl_ 

