// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_ARRAY2D_KERNEl_ABSTRACT_
#ifdef DLIB_ARRAY2D_KERNEl_ABSTRACT_

#include "../interfaces/enumerable.h"
#include "../serialize.h"
#include "../memory_manager/memory_manager_kernel_abstract.h"

namespace dlib
{

    template <
        typename T,
        typename mem_manager = memory_manager<char>::kernel_1a
        >
    class array2d : public enumerable<T>
    {

        /*!
            REQUIREMENTS ON T
                T must have a default constructor.

            REQUIREMENTS ON mem_manager
                must be an implementation of memory_manager/memory_manager_kernel_abstract.h or
                must be an implementation of memory_manager_global/memory_manager_global_kernel_abstract.h or
                must be an implementation of memory_manager_stateless/memory_manager_stateless_kernel_abstract.h 
                mem_manager::type can be set to anything.

            POINTERS AND REFERENCES TO INTERNAL DATA
                No member functions in this object will invalidate pointers
                or references to internal data except for the set_size()
                and clear() member functions.

            INITIAL VALUE
                width() == 0
                height() == 0
                
            ENUMERATION ORDER
                The enumerator will iterate over the elements of the array starting
                with row 0 and then proceeding to row 1 and so on.  Each row will be
                fully enumerated before proceeding on to the next row and the elements
                in a row will be enumerated beginning with the 0th column, then the 1st 
                column and so on.

            WHAT THIS OBJECT REPRESENTS
                This object represents a 2-Dimensional array of objects of 
                type T. 
        !*/


    public:

        // ----------------------------------------

        typedef T type;
         
        // ----------------------------------------

        class row 
        {
            /*!
                POINTERS AND REFERENCES TO INTERNAL DATA
                    No member functions in this object will invalidate pointers
                    or references to internal data.

                WHAT THIS OBJECT REPRESENTS
                    This object represents a row of Ts in an array2d object.
            !*/
        public:
            long width (
            ) const;
            /*!
                ensures
                    - returns the number of Ts in this row
            !*/

            const T& operator[] (
                long column
            ) const;
            /*!
                requires
                    - 0 <= column < width()
                ensures
                    - returns a const reference to the T in the given column 
            !*/

            T& operator[] (
                long column
            );
            /*!
                requires
                    - 0 <= column < width()
                ensures
                    - returns a non-const reference to the T in the given column 
            !*/

        private:
            // restricted functions
            row();
            row(row&);
            row& operator=(row&);
        };

        // ----------------------------------------

        array2d (
        );
        /*!
            ensures 
                - #*this is properly initialized
            throws
                - std::bad_alloc 
        !*/

        virtual ~array2d (
        ); 
        /*!
            ensures
                - all resources associated with *this has been released
        !*/
        
        void clear (
        );
        /*!
            ensures
                - #*this has an initial value for its type
        !*/

        long width (
        ) const;
        /*!
            ensures
                - returns the number of elements there are in a row
        !*/

        long height (
        ) const;
        /*!
            ensures
                - returns the number of rows in *this
        !*/

        long nr (
        ) const;
        /*!
            ensures
                - returns height()
        !*/

        long nc (
        ) const;
        /*!
            ensures
                - returns width()
        !*/

        void set_size (
            long width_,
            long height_
        );
        /*!
            requires
                - width_ > 0
                - height_ > 0
            ensures
                - #width() == width_
                - #height() == height_
                - all elements have valid but undetermined values.  (e.g. this object might
                  buffer old T objects and reuse them without reinitializing them between
                  calls to set_size())
                - #at_start() == true
            throws
                - std::bad_alloc 
                    If this exception is thrown then #*this will have an initial
                    value for its type.
        !*/

        row& operator[] (
            long row
        );
        /*!
            requires
                - 0 <= row < height()
            ensures
                - returns a non-const row of width() Ts that represents the 
                  given row of Ts in *this.
        !*/

        const row& operator[] (
            long row
        ) const;
        /*!
            requires
                - 0 <= row < height()
            ensures
                - returns a const row of width() Ts that represents the 
                  given row of Ts in *this.
        !*/

        void swap (
            array2d& item
        );
        /*!
            ensures
                - swaps *this and item
        !*/ 

    private:

        // restricted functions
        array2d(array2d&);        // copy constructor
        array2d& operator=(array2d&);    // assignment operator

    };

    template <
        typename T
        >
    inline void swap (
        array2d<T>& a, 
        array2d<T>& b 
    ) { a.swap(b); }   
    /*!
        provides a global swap function
    !*/

    template <
        typename T
        >
    void serialize (
        const array2d<T>& item, 
        std::ostream& out 
    );   
    /*!
        provides serialization support 
    !*/

    template <
        typename T 
        >
    void deserialize (
        array2d<T>& item, 
        std::istream& in
    );   
    /*!
        provides deserialization support 
    !*/

}

#endif // DLIB_ARRAY2D_KERNEl_ABSTRACT_ 

