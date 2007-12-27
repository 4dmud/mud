// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_VECTOR_KERNEl_ABSTRACT_
#ifdef DLIB_VECTOR_KERNEl_ABSTRACT_

#include "../serialize.h"
#include <functional>
#include <iostream>

namespace dlib
{

    template <
        typename T
        >
    class vector
    {
        /*!
            REQUIREMENTS ON T
                T should be some object that provides an interface that is 
                compatible with double, float and the like.

            INITIAL VALUE
                x() == 0 
                y() == 0 
                z() == 0

            WHAT THIS OBJECT REPRESENTS
                This object represents a three dimensional vector.

            THREAD SAFETY
                Note that the vector object is not allowed to be reference counted.
                This is to ensure a minimum amount of thread safety.
        !*/

    public:

        typedef T type;
        
        vector (
        );
        /*!
            ensures
               - #*this has been properly initialized
        !*/

        vector (
            const T _x,
            const T _y,
            const T _z
        );
        /*!
            ensures
                - #*this properly initialized 
                - #x() == _x 
                - #y() == _y 
                - #z() == _z 
        !*/

        vector (
            const vector& v
        );
        /*!
            ensures
                - #*this properly initialized 
                - #x() == v.x() 
                - #y() == v.y() 
                - #z() == v.z() 
        !*/

        ~vector (
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/


        T length(
        ) const;
        /*!
            ensures
                - returns the length of the vector
        !*/

        T& x (
        );
        /*!
            ensures
                - returns a reference to the x component of the vector
        !*/

        T& y (
        );
        /*!
            ensures
                - returns a reference to the y component of the vector
        !*/

        T& z (
        );
        /*!
            ensures
                - returns a reference to the z component of the vector
        !*/

        const T& x (
        ) const;
        /*!
            ensures
                - returns a const reference to the x component of the vector
        !*/

        const T& y (
        ) const;
        /*!
            ensures
                - returns a const reference to the y component of the vector
        !*/

        const T& z (
        ) const;
        /*!
            ensures
                - returns a const reference to the z component of the vector
        !*/

        T dot (
            const vector& rhs
        ) const;
        /*!
            ensures
                - returns the result of the dot product between *this and rhs
        !*/

        vector cross (
            const vector& rhs
        ) const;
        /*!
            ensures
                - returns the result of the cross product between *this and rhs
        !*/

        vector normalize (
        ) const;
        /*!
            ensures
                - returns a vector with length() == 1 and in the same direction as *this
        !*/

        vector operator+ (
            const vector& rhs
        ) const;
        /*!
            ensures
                - returns the result of adding *this to rhs
        !*/

        vector operator- (
            const vector& rhs
        ) const;
        /*!
            ensures
                - returns the result of subtracting rhs from *this
        !*/

        vector operator/ (
            const T rhs
        ) const;
        /*!
            ensures
                - returns the result of dividing *this by rhs 
        !*/

        vector& operator= (
            const vector& rhs
        );
        /*!
            ensures
                - #x() == rhs.x() 
                - #y() == rhs.y() 
                - #z() == rhs.z()
                - returns #*this
        !*/

        vector& operator += (
            const vector& rhs
        );
        /*!
            ensures
                - #*this == *this + rhs
                - returns #*this
        !*/

        vector& operator -= (
            const vector& rhs
        );
        /*!
            ensures
                - #*this == *this - rhs
                - returns #*this
        !*/

        vector& operator *= (
            const T rhs
        );
        /*!
            ensures
                - #*this == *this * rhs
                - returns #*this
        !*/

        vector& operator /= (
            const T rhs
        );
        /*!
            ensures
                - #*this == *this / rhs
                - returns #*this
        !*/

        bool operator== (
            const vector& rhs
        ) const;
        /*!
            ensures
                - if (x() == rhs.x() && y() == rhs.y() && z() == rhs.z()) then
                    - returns true
                - else
                    - returns false
        !*/

        bool operator!= (
            const vector& rhs
        ) const;
        /*!
            ensures
                - returns !((*this) == rhs)
        !*/

        void swap (
            vector& item
        );
        /*!
            ensures
                - swaps *this and item
        !*/

    };

    template<typename T, typename U>
    vector<T>  operator* (
        const vector<T> & lhs,
        const U rhs
    );
    /*!
        ensures
            - returns the result of multiplying the scalar rhs by lhs
    !*/
    
    template<typename T, typename U>
    vector<T>  operator* (
        const U lhs,
        const vector<T> & rhs   
    );
    /*! 
        ensures
            - returns the result of multiplying the scalar lhs by rhs
    !*/

    template<typename T>
    inline void swap (
        vector<T> & a, 
        vector<T> & b 
    ) { a.swap(b); }   
    /*!
        provides a global swap function
    !*/

    template<typename T>
    void serialize (
        const vector<T> & item, 
        std::ostream& out
    );   
    /*!
        provides serialization support 
    !*/

    template<typename T>
    void deserialize (
        vector<T> & item, 
        std::istream& in
    );   
    /*!
        provides deserialization support 
    !*/

    template<typename T>
    std::ostream& operator<< (
        std::ostream& out, 
        const vector<T>& item 
    );   
    /*!
        ensures
            - writes item to out in the form "(x, y, z)"
    !*/

    template<typename T>
    std::istream& operator>>(
        std::istream& in, 
        vector<T>& item 
    );   
    /*!
        ensures
            - reads a vector from the input stream in and stores it in #item.
              The data in the input stream should be of the form (x, y, z)
    !*/

}

namespace std
{
    /*!
        Define std::less<vector<T> > so that you can use vectors in the associative containers.
    !*/
    template<typename T>
    struct less<dlib::vector<T> > : public binary_function<dlib::vector<T> ,dlib::vector<T> ,bool>
    {
        inline bool operator() (const dlib::vector<T> & a, const dlib::vector<T> & b) const
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

#endif // DLIB_VECTOR_KERNEl_ABSTRACT_

