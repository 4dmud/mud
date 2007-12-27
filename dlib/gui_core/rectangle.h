// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#ifndef DLIB_RECTANGLe_
#define DLIB_RECTANGLe_

#include "gui_core_kernel_abstract.h"
#include "../algs.h"
#include <algorithm>

namespace dlib
{

// ----------------------------------------------------------------------------------------
    
    class rectangle
    {
        /*!
            INITIAL VALUE
                The initial value of this object is defined by its constructor.

            CONVENTION
                left() == l
                top() == t
                right() == r
                bottom() == b
        !*/

    public:

        rectangle (
            long l_,
            long t_,
            long r_,
            long b_
        ) :
            l(l_),
            t(t_),
            r(r_),
            b(b_)
        {}

        rectangle (
        ) :
            l(1),
            t(1),
            r(0),
            b(0)
        {}

        long top (
        ) const { return t; }

        void set_top (
            long top_
        ) { t = top_; }

        long left (
        ) const { return l; }

        void set_left (
            long left_
        ) { l = left_; }

        long right (
        ) const { return r; }

        void set_right (
            long right_
        ) { r = right_; }

        long bottom (
        ) const { return b; }

        void set_bottom (
            long bottom_
        ) { b = bottom_; }
       
        unsigned long width (
        ) const 
        { 
            if (is_empty())
                return 0;
            else
                return r - l + 1; 
        }

        unsigned long height (
        ) const 
        { 
            if (is_empty())
                return 0;
            else
                return b - t + 1; 
        }

        bool is_empty (
        ) const { return (t > b || l > r); }

        rectangle operator + (
            const rectangle& rhs
        ) const
        {
            if (rhs.is_empty())
                return *this;
            else if (is_empty())
                return rhs;

            return rectangle (
                std::min(l,rhs.l),
                std::min(t,rhs.t),
                std::max(r,rhs.r),
                std::max(b,rhs.b)
                );
        }

        rectangle intersect (
            const rectangle& rhs
        ) const
        {
            return rectangle (
                std::max(l,rhs.l),
                std::max(t,rhs.t),
                std::min(r,rhs.r),
                std::min(b,rhs.b)
                );
        }

        bool contains (
            long x,
            long y
        ) const
        {
            if (x < l || x > r || y < t || y > b)
                return false;
            return true;
        }

        bool operator== (
            const rectangle& rect 
        ) const 
        {
            return (l == rect.l) && (t == rect.t) && (r == rect.r) && (b == rect.b);
        }

        bool operator!= (
            const rectangle& rect 
        ) const 
        {
            return !(*this == rect);
        }

    private:
        long l;
        long t;
        long r;
        long b;   
    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_RECTANGLe_

