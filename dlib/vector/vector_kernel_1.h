// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_VECTOR_KERNEl_1_
#define DLIB_VECTOR_KERNEl_1_

#include <cmath>
#include "vector_kernel_abstract.h"
#include "../algs.h"
#include "../serialize.h"
#include <functional>
#include <iostream>


namespace dlib
{

    template <
        typename T
        >
    class vector_kernel_1
    {
        /*!
            INITIAL VALUE
                - x_value == 0
                - y_value == 0  
                - z_value == 0 

            CONVENTION
                - x_value == x() 
                - y_value == y() 
                - z_value == z()

        !*/

    public:

        typedef T type;
        
        vector_kernel_1 (
        ) :
            x_value(0.0),
            y_value(0.0),
            z_value(0.0)
        {}

        // ---------------------------------------

        vector_kernel_1 (
            const T _x,
            const T _y,
            const T _z
        ) :
            x_value(_x),
            y_value(_y),
            z_value(_z)
        {}

        // ---------------------------------------

        vector_kernel_1 (
            const vector_kernel_1& v
        ) :
            x_value(v.x_value),
            y_value(v.y_value),
            z_value(v.z_value)
        {}

        // ---------------------------------------

        ~vector_kernel_1 (
        ){}

        // ---------------------------------------

        T length(
        ) const 
        { 
            return (T)sqrt((double)(x_value*x_value + y_value*y_value + z_value*z_value)); 
        }

        // ---------------------------------------

        vector_kernel_1 normalize (
        ) const 
        {
            T tmp = (T)sqrt((double)(x_value*x_value + y_value*y_value + z_value*z_value));
            return vector_kernel_1 ( x_value/tmp,
                                     y_value/tmp,
                                     z_value/tmp
                                    );
        }

        // ---------------------------------------

        T& x (
        ) 
        { 
            return x_value; 
        }

        // ---------------------------------------

        T& y (
        ) 
        { 
            return y_value; 
        }

        // ---------------------------------------

        T& z (
        ) 
        { 
            return z_value; 
        }

        // ---------------------------------------

        const T& x (
        ) const
        { 
            return x_value; 
        }

        // ---------------------------------------

        const T& y (
        ) const 
        { 
            return y_value; 
        }

        // ---------------------------------------

        const T& z (
        ) const
        { 
            return z_value; 
        }

        // ---------------------------------------

        T dot (
            const vector_kernel_1& rhs
        ) const 
        { 
            return x_value*rhs.x_value + y_value*rhs.y_value + z_value*rhs.z_value; 
        }

        // ---------------------------------------

        vector_kernel_1 cross (
            const vector_kernel_1& rhs
        ) const
        {
            return vector_kernel_1 (
                y_value*rhs.z_value - z_value*rhs.y_value,
                z_value*rhs.x_value - x_value*rhs.z_value,
                x_value*rhs.y_value - y_value*rhs.x_value
                );
        }

        // ---------------------------------------

        vector_kernel_1 operator+ (
            const vector_kernel_1& rhs
        ) const
        {
            return vector_kernel_1 (
                x_value+rhs.x_value,
                y_value+rhs.y_value,
                z_value+rhs.z_value
            );
        }

        // ---------------------------------------

        vector_kernel_1 operator- (
            const vector_kernel_1& rhs
        ) const
        {
            return vector_kernel_1 (
                x_value-rhs.x_value,
                y_value-rhs.y_value,
                z_value-rhs.z_value
            );
        }

        // ---------------------------------------

        vector_kernel_1& operator= (
            const vector_kernel_1& rhs
        )
        {
            x_value = rhs.x_value;
            y_value = rhs.y_value;
            z_value = rhs.z_value;
            return *this;
        }

        // ---------------------------------------

        vector_kernel_1 operator/ (
            const T rhs
        ) const
        {
            return vector_kernel_1 (
                x_value/rhs,
                y_value/rhs,
                z_value/rhs
            );
        }

        // ---------------------------------------

        vector_kernel_1& operator += (
            const vector_kernel_1& rhs
        )
        {
            x_value += rhs.x_value;
            y_value += rhs.y_value;
            z_value += rhs.z_value;
            return *this;
        }

        // ---------------------------------------

        vector_kernel_1& operator -= (
            const vector_kernel_1& rhs
        )
        {
            x_value -= rhs.x_value;
            y_value -= rhs.y_value;
            z_value -= rhs.z_value;
            return *this;
        }

        // ---------------------------------------

        vector_kernel_1& operator *= (
            const T rhs
        )
        {
            x_value *= rhs;
            y_value *= rhs;
            z_value *= rhs;
            return *this;
        }

        // ---------------------------------------

        vector_kernel_1& operator /= (
            const T rhs
        )
        {
            x_value /= rhs;
            y_value /= rhs;
            z_value /= rhs;
            return *this;
        }

        // ---------------------------------------

        bool operator== (
            const vector_kernel_1& rhs
        ) const
        {
            return (x_value == rhs.x_value &&
                    y_value == rhs.y_value &&
                    z_value == rhs.z_value );
        }

        // ---------------------------------------

        bool operator!= (
            const vector_kernel_1& rhs
        ) const
        {
            return !((*this) == rhs);
        }

        // ---------------------------------------

        void swap (
            vector_kernel_1& item
        )
        {
            exchange(x_value,item.x_value);
            exchange(y_value,item.y_value);
            exchange(z_value,item.z_value);
        }

        // ---------------------------------------

        private:
            T x_value;
            T y_value;
            T z_value;

    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template<typename T, typename U>
    inline vector_kernel_1<T>  operator* (
        const vector_kernel_1<T> & lhs,
        const U rhs
    )
    {
        return vector_kernel_1<T>  (
            lhs.x()*rhs,
            lhs.y()*rhs,
            lhs.z()*rhs
        );
    }

// ----------------------------------------------------------------------------------------

    template<typename T, typename U>
    inline vector_kernel_1<T>  operator* (
        const U lhs,
        const vector_kernel_1<T> & rhs   
    ) { return rhs*lhs; }

// ----------------------------------------------------------------------------------------

    template<typename T>
    inline void swap (
        vector_kernel_1<T> & a, 
        vector_kernel_1<T> & b 
    ) { a.swap(b); }   

// ----------------------------------------------------------------------------------------

    template<typename T>
    inline void serialize (
        const vector_kernel_1<T> & item,  
        std::ostream& out
    )
    {
        try
        {
            serialize(item.x(),out);
            serialize(item.y(),out);
            serialize(item.z(),out);
        }
        catch (serialization_error e)
        { 
            throw serialization_error(e.info + "\n   while serializing object of type vector_kernel_1"); 
        }
    }

    template<typename T>
    inline void deserialize (
        vector_kernel_1<T> & item,  
        std::istream& in
    )
    {
        try
        {
            deserialize(item.x(),in);
            deserialize(item.y(),in);
            deserialize(item.z(),in);
        }
        catch (serialization_error e)
        { 
            item.x() = 0;
            item.y() = 0;
            item.z() = 0;
            throw serialization_error(e.info + "\n   while deserializing object of type vector_kernel_1"); 
        }
    }

// ----------------------------------------------------------------------------------------

    template<typename T>
    std::ostream& operator<< (
        std::ostream& out, 
        const vector_kernel_1<T>& item 
    )
    {
        out << "(" << item.x() << ", " << item.y() << ", " << item.z() << ")";
        return out;
    }

    template<typename T>
    std::istream& operator>>(
        std::istream& in, 
        vector_kernel_1<T>& item 
    )   
    {

        // eat all the crap up to the '(' 
        while (in.peek() == ' ' || in.peek() == '\t')
            in.get();

        // there should be a '(' if not then this is an error
        if (in.get() != '(')
        {
            in.setstate(in.rdstate() | std::ios::failbit);
            return in;
        }

        // eat all the crap up to the first number 
        while (in.peek() == ' ' || in.peek() == '\t')
            in.get();
        in >> item.x();

        if (!in.good())
            return in;
              
        // eat all the crap up to the next number
        while (in.peek() == ' ' || in.peek() == '\t' || in.peek() == ',')
            in.get();
        in >> item.y();

        if (!in.good())
            return in;
              
        // eat all the crap up to the next number
        while (in.peek() == ' ' || in.peek() == '\t' || in.peek() == ',')
            in.get();
        in >> item.z();

        if (!in.good())
            return in;
              
        // eat all the crap up to the ')'
        while (in.peek() == ' ' || in.peek() == '\t')
            in.get();

        // there should be a ')' if not then this is an error
        if (in.get() != ')')
            in.setstate(in.rdstate() | std::ios::failbit);
        return in;
    }

// ----------------------------------------------------------------------------------------

}

namespace std
{
    /*!
        Define std::less<vector_kernel_1<T> > so that you can use vectors in the associative containers.
    !*/
    template<typename T>
    struct less<dlib::vector_kernel_1<T> > : public binary_function<dlib::vector_kernel_1<T> ,dlib::vector_kernel_1<T> ,bool>
    {
        inline bool operator() (const dlib::vector_kernel_1<T> & a, const dlib::vector_kernel_1<T> & b) const
        { 
            if      (a.x() < b.x()) return true;
            else if (a.x() > b.x()) return false;
            else if (a.y() < b.y()) return true;
            else if (a.y() > b.y()) return false;
            else if (a.z() < b.z()) return true;
            else if (a.z() > b.z()) return false;
            else                    return false;
        }
    };
}

#endif // DLIB_VECTOR_KERNEl_1_

