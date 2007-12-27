// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#undef DLIB_FONTs_ABSTRACT_
#ifdef DLIB_FONTs_ABSTRACT_

#include "../gui_core.h"
#include <string>
#include "../serialize.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    class letter 
    {    
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a letter in a font.  It tells you the nominal 
                width of the letter and which pixels form the letter.

            THREAD SAFETY
                const versions of this object are thread safe but if you are going to
                be modifying it then you must serialize access to it.
        !*/
    public:
        struct point 
        {
            /*!
                WHAT THIS OBJECT REPRESENTS
                    This object represents one of the pixels of a letter.  
                    
                    The origin (i.e. (0,0)) of the coordinate plane is at the left 
                    side of the letter's baseline.  Also note that y is negative when 
                    above the baseline and positive below (it is zero on the baseline 
                    itself).

                    The x value is positive going to the right and negative to the left.
                    The meaning of a negative x value is that any points with a negative
                    x value will overlap with the preceding letter.
            !*/

            point (
            );
            /*!
                ensures
                    - This constructor does nothing.  The value of x and y 
                      are undefined after its execution.
            !*/

            point (
                long x_,
                long y_
            );
            /*!
                ensures
                    - #x == x_
                    - #y == y_
            !*/


            long x;
            long y;
        };

        // ---------------------------------

        letter (
        );
        /*!
            ensures
                - #width() == 0 
                - #num_of_points() == 0 
        !*/

        letter (
            unsigned long width_,
            unsigned long point_count
        );
        /*!
            ensures
                - #width() == width_
                - #num_of_points() == point_count
        !*/

        ~letter(
        );
        /*!
            ensures
                - any resources used by *this have been freed
        !*/
            
        const unsigned long width (
        ) const;
        /*!
            ensures
                - returns the width reserved for this letter in pixels.  This is the 
                  number of pixels that are reserved for this letter between adjoining 
                  letters.  It isn't necessarily the width of the actual letter itself.  
                  (for example, you can make a letter with a width less than how wide it 
                  actually is so that it overlaps with its neighbor letters.)
        !*/

        const unsigned long num_of_points (
        ) const;
        /*!
            ensures
                - returns the number of pixels that make up this letter.
        !*/

        point& operator[] (
            unsigned long i
        );
        /*!
            requires
                - i < num_of_points()
            ensures
                - returns a non-const reference to the ith point in this letter.
        !*/

        const point& operator[] (
            unsigned long i
        ) const;
        /*!
            requires
                - i < num_of_points()
            ensures
                - returns a const reference to the ith point in this letter.
        !*/
    
        private:

            // restricted functions
            letter(letter&);        // copy constructor
            letter& operator=(letter&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------

    void serialize (
        const letter& item, 
        std::ostream& out 
    );   
    /*!
        provides serialization support for letter objects
    !*/

    void deserialize (
        letter& item, 
        std::istream& in
    );   
    /*!
        provides deserialization support for letter objects
    !*/

// ----------------------------------------------------------------------------------------

    class font
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object defines an interface for a font type.  It provides metrics
                for the font and functions to help you draw strings on a canvas object.

            THREAD SAFETY
                All the functions in this class are thread safe.
        !*/

    public:

        virtual const letter& operator[] (
            char ch
        )const=0;
        /*!
            ensures
                - returns a letter object that tells you how to draw this character.
        !*/

        virtual const unsigned long height (
        ) const = 0;
        /*!
            ensures
                - returns the height in pixels of the tallest letter in the font                
        !*/

        virtual const unsigned long ascender (
        ) const = 0;
        /*!
            ensures
                - returns the height() minus the number of pixels below the baseline used
                  by the letter that hangs the lowest below the baseline.
        !*/

        virtual const unsigned long left_overflow (
        ) const = 0;
        /*! 
            ensures
                - returns how far outside and to the left of its width a letter
                  from this font may set pixels.  (i.e. how many extra pixels to its
                  left may a font use)
        !*/

        virtual const unsigned long right_overflow (
        ) const = 0;
        /*! 
            ensures
                - returns how far outside and to the right of its width a letter
                  from this font may set pixels.  (i.e. how many extra pixels to its
                  right may a font use)
        !*/

        void compute_size (
            const std::string& str,
            unsigned long& width,
            unsigned long& height,
            std::string::size_type first = 0,
            std::string::size_type last = std::string::npos
        ) const;
        /*!
            requires
                - if (last != std::string::npos) then
                    - first <= last
                    - last < str.size()
            ensures
                - all characters in str with an index < first are ignored by this
                  function.
                - if (last != std::string::npos) then
                    - all characters in str with an index > last are ignored by 
                      this function.
                - if (str.size() == 0) then
                    - #width == 0
                    - #height == 0
                - else
                    - #width == sum of the widths of the characters in the widest 
                      line in str + left_overflow() + right_overflow(). 
                    - #height == (count(str.begin(),str.end(),'\n')+1)*height()
        !*/

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
        /*!
            requires
                - if (last != std::string::npos) then
                    - first <= last
                    - last < str.size()
            ensures
                - all characters in str with an index < first are ignored by this
                  function.
                - if (last != std::string::npos) then
                    - all characters in str with an index > last are ignored by 
                      this function.
                - if (str.size() == 0) then
                    - does nothing
                - else
                    - draws str on the given canvas at the position defined by rect.  
                      Also uses the given colors for the font color.
                - If the string is too big to fit in rect then the right and
                  bottom sides of it will be clipped to make it fit.                  
                - if (first_pixel > 0) then
                    - the string will be drawn shifted left by first_pixel pixels.  Also, 
                      the pixels shifted outside rect will not be drawn.
        !*/

        const rectangle compute_cursor_rect (
            const rectangle& rect,
            const std::string& str,
            unsigned long index,
            std::string::size_type first = 0,
            std::string::size_type last = std::string::npos
        ) const;
        /*!
            requires
                - if (last != std::string::npos) then
                    - first <= last
                    - last < str.size()
            ensures
                - the returned rectangle has a width of 1 and a 
                  height of this->height().
                - computes the location of the cursor that would sit just before
                  the character str[index] if str were drawn on the screen by
                  draw_string(rect,str,...,first,last).  The cursor location is
                  returned in the form of a rectangle.
                - if (index < first) then
                    - the returned cursor will be just before the character str[first].
                - if (last != std::string::npos && index > last) then
                    - the returned cursor will be just after the character str[last]
                - if (str.size() == 0) then
                    - the returned cursor will be just at the start of the rectangle where
                      str would be drawn if it wasn't empty.
                - if (index > str.size()-1) then
                    - the returned cursor will be just after the character str[str.size()-1]
        !*/

        const unsigned long compute_cursor_pos (
            const rectangle& rect,
            const std::string& str,
            long x,
            long y,
            std::string::size_type first = 0,
            std::string::size_type last = std::string::npos
        ) const;
        /*!
            requires
                - if (last != std::string::npos) then
                    - first <= last
                    - last < str.size()
            ensures
                - returns a number idx that has the following properties:
                    - if (first < str.size()) then
                        - first <= idx
                    - else
                        - idx == str.size()
                    - if (last != std::string::npos) then
                        - idx <= last + 1
                    - compute_cursor_rect(rect,str,idx,first,last) == the cursor
                      position that is closest to the pixel (x,y)
        !*/


    private:

        // restricted functions
        font(font&);        // copy constructor
        font& operator=(font&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------

    class default_font : public font
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This is an implementation of the Times New Roman font.

            THREAD SAFETY
                It is safe to call get_font() and access the returned font from any 
                thread and no synchronization is needed as long as it is called 
                after the main() or winmain() function has been entered.
        !*/

    public:
        static const font* get_font(
        );
        /*!
            ensures
                - returns an instance of this font.
                  (Note that you do not need to and should NOT call delete on the 
                  returned pointer)
            throws
                - std::bad_alloc
                    This exception is thrown if there is a problem gathering the needed
                    memory for the font object.
        !*/

    private:

        // restricted functions
        default_font();        // normal constructor
        default_font(default_font&);        // copy constructor
        default_font& operator=(default_font&);    // assignment operator   
    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_FONTs_ABSTRACT_

