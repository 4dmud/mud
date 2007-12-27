// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#ifndef DLIB_FONTs_
#define DLIB_FONTs_

#include "fonts_abstract.h"
#include "../gui_core.h"
#include <string>
#include "../algs.h"
#include "../serialize.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    class letter 
    {    
        /*!
            INITIAL VALUE
                - defined by constructor

            CONVENTION
                - if (points != 0) then
                    - points == an array of count point structs
                - w == width()
                - count == num_of_points()
        !*/
    public:
        struct point 
        {
            point (){}

            point (
                long x_,
                long y_
            ) :
                x(x_),
                y(y_)
            {}

            long x;
            long y;
        };

        letter (
        ) :
            w(0),
            points(0),
            count(0)
        {}

        letter (
            unsigned long width_,
            unsigned long point_count
        ) : 
            w(width_),
            points(new point[point_count]),
            count(point_count)
        {}

        ~letter(
        )
        {
            if (points)
                delete [] points; 
        }
            
        const unsigned long width (
        ) const { return w; }
        
        const unsigned long num_of_points (
        ) const { return count;}

        point& operator[] (
            unsigned long i
        ) 
        { 
            ASSERT (i < num_of_points(),
                    "\tvoid letter::operator[]()"
                    << "\n\ti:               " << i 
                    << "\n\tnum_of_points(): " << num_of_points() );
            return points[i]; 
        }

        const point& operator[] (
            unsigned long i
        ) const 
        { 
            ASSERT (i < num_of_points(),
                    "\tvoid letter::operator[]()"
                    << "\n\ti:               " << i 
                    << "\n\tnum_of_points(): " << num_of_points() );
            return points[i]; 
        }
    
        friend void serialize (
            const letter& item, 
            std::ostream& out 
        );   

        friend void deserialize (
            letter& item, 
            std::istream& in
        );   

    private:
        unsigned long w;
        point* points;
        unsigned long count;
    };

// ----------------------------------------------------------------------------------------

    class font
    {
    public:
        virtual ~font() {}

        virtual const letter& operator[] (
            unsigned char ch
        )const=0;

        virtual const unsigned long height (
        ) const = 0;

        virtual const unsigned long ascender (
        ) const = 0;

        virtual const unsigned long left_overflow (
        ) const = 0;

        virtual const unsigned long right_overflow (
        ) const = 0;

        void compute_size (
            const std::string& str,
            unsigned long& width,
            unsigned long& height,
            std::string::size_type first = 0,
            std::string::size_type last = std::string::npos
        ) const;

        const void draw_string (
            const rectangle& rect,
            const std::string& str,
            const canvas& c,
            unsigned char red = 0,
            unsigned char green = 0,
            unsigned char blue = 0,
            std::string::size_type first = 0,
            std::string::size_type last = std::string::npos,
            unsigned long first_pixel = 0
        ) const;

        const rectangle compute_cursor_rect (
            const rectangle& rect,
            const std::string& str,
            unsigned long index,
            std::string::size_type first = 0,
            std::string::size_type last = std::string::npos
        ) const;

        const unsigned long compute_cursor_pos (
            const rectangle& rect,
            const std::string& str,
            long x,
            long y,
            std::string::size_type first = 0,
            std::string::size_type last = std::string::npos
        ) const;

    };

// ----------------------------------------------------------------------------------------

    class default_font : public font
    {
        letter* l;


        default_font(
        );
        default_font(default_font&);        // copy constructor
        default_font& operator=(default_font&);    // assignment operator   

    public:
        static const font* get_font (
        )
        {
            static default_font f;
            return &f;
        }

        ~default_font(
        )
        {
            delete [] l;
        }

        const unsigned long height (
        ) const { return 16; }

        const unsigned long ascender (
        ) const { return 12; }

        const unsigned long left_overflow (
        ) const { return 1; }

        const unsigned long right_overflow (
        ) const { return 2; }

        const letter& operator[] (
            unsigned char ch
        ) const
        {
            return l[ch];
        }
    };

// ----------------------------------------------------------------------------------------

}

#ifdef NO_MAKEFILE
#include "fonts.cpp"
#endif

#endif // DLIB_FONTs_

