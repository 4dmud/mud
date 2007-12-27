// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_ARRAY2D_KERNEl_1_
#define DLIB_ARRAY2D_KERNEl_1_

#include "array2d_kernel_abstract.h"
#include "../algs.h"
#include "../interfaces/enumerable.h"
#include "../serialize.h"
#include "../memory_manager.h"

namespace dlib
{
    template <
        typename T,
        typename mem_manager = memory_manager<char>::kernel_1a
        >
    class array2d_kernel_1 : public enumerable<T>
    {

        /*!
            INITIAL VALUE
                - width_ == 0 
                - height_ == 0 
                - data == 0 
                - rows == 0
                - at_start_ == true
                - cur == 0
                - last == 0

            CONVENTION
                - width_ == width() 
                - height_ == width() 
                - if (data != 0) then
                    - last == a pointer to the last element in the data array
                    - data == pointer to an array of width_*height_ T objects 
                    - rows == pointer to an array of height_ row objects
                    - for all x < height_:
                        - rows[x].data == data + x*width_
                        - rows[x].width_ == width_
                - else
                    - width_ == 0
                    - height_ == 0
                    - data == 0
                    - rows == 0
                    - last == 0


                - height_ * width_ == size()
                - if (cur == 0) then
                    - current_element_valid() == false
                - else 
                    - current_element_valid() == true
                    - *cur == element()

                - at_start_ == at_start()      
        !*/


        class row_helper;
    public:
         
        typedef T type;

        // -----------------------------------

        class row 
        {
            /*!
                CONVENTION
                    - width_ == width()
                    - for all x < width_:
                        - (*this)[x] == data[x]
            !*/

            friend class array2d_kernel_1;
            friend class row_helper;

        public:
            long width (
            ) const { return width_; }

            const T& operator[] (
                long column
            ) const { return data[column]; }

            T& operator[] (
                long column
            ) { return data[column]; }

        private:

            long width_;
            T* data; 


            // restricted functions
            row(){}
            row(row&);
            row& operator=(row&);
        };

        // -----------------------------------

        array2d_kernel_1 (
        ) : 
            width_(0),
            height_(0),
            rows(0),
            data(0),
            cur(0),
            last(0),
            at_start_(true)
        {
        }

        virtual ~array2d_kernel_1 (
        ) { clear(); }

        long width (
        ) const { return width_; }

        long height (
        ) const { return height_; }

        long nc (
        ) const { return width_; }

        long nr (
        ) const { return height_; }

        row& operator[] (
            long row
        ) { return rows[row]; }

        const row& operator[] (
            long row
        ) const { return rows[row]; }

        void swap (
            array2d_kernel_1& item
        )
        {
            exchange(data,item.data);
            exchange(rows,item.rows);
            exchange(height_,item.height_);
            exchange(width_,item.width_);
            exchange(at_start_,item.at_start_);
            exchange(cur,item.cur);
            exchange(last,item.last);
            pool.swap(item.pool);
            rpool.swap(item.rpool);
        }

        void clear (
        )
        {
            if (data != 0)
            {
                rpool.deallocate_array(reinterpret_cast<row_helper*>(rows));
                pool.deallocate_array(data);
                width_ = 0;
                height_ = 0;
                rows = 0;
                data = 0;
                at_start_ = true;
                cur = 0;
                last = 0;
            }
        }

        void set_size (
            long width__,
            long height__
        );

        bool at_start (
        ) const { return at_start_; }

        void reset (
        ) const { at_start_ = true; cur = 0; }

        bool current_element_valid (
        ) const { return (cur != 0); }

        const T& element (
        ) const { return *cur; }

        T& element (
        ) { return *cur; }

        bool move_next (
        ) const
        {
            if (cur != 0)
            {
                if (cur != last)
                {
                    ++cur;
                    return true;
                }
                cur = 0;
                return false;
            }
            else if (at_start_)
            {
                cur = data;
                at_start_ = false;
                return (data != 0);
            }
            else
            {
                return false;
            }
        }

        unsigned long size (
        ) const { return static_cast<unsigned long>(width_ * height_); }

    private:

        // this object exists just so we can have a row type object that
        // has a public default constructor so the memory_manager can construct it.
        // I would have made rpool a friend of row but some compilers can't handle 
        // that without crapping out.
        class row_helper : public row {};

        typename mem_manager::template rebind<T>::other pool;
        typename mem_manager::template rebind<row_helper>::other rpool;

        long width_;
        long height_;
        row* rows;
        T* data;

        mutable T* cur;
        T* last;
        mutable bool at_start_;

        // restricted functions
        array2d_kernel_1(array2d_kernel_1&);        // copy constructor
        array2d_kernel_1& operator=(array2d_kernel_1&);    // assignment operator

    };

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        typename mem_manager
        >
    inline void swap (
        array2d_kernel_1<T,mem_manager>& a, 
        array2d_kernel_1<T,mem_manager>& b 
    ) { a.swap(b); }   


    template <
        typename T,
        typename mem_manager
        >
    void serialize (
        const array2d_kernel_1<T,mem_manager>& item, 
        std::ostream& out 
    )   
    {
        try
        {
            serialize(item.width(),out);
            serialize(item.height(),out);

            item.reset();
            while (item.move_next())
                serialize(item.element(),out);
            item.reset();
        }
        catch (serialization_error e)
        { 
            throw serialization_error(e.info + "\n   while serializing object of type array2d_kernel_1"); 
        }
    }

    template <
        typename T 
        >
    void deserialize (
        array2d_kernel_1<T>& item, 
        std::istream& in
    )   
    {
        try
        {
            long width, height;
            deserialize(width,in);
            deserialize(height,in);

            item.set_size(width,height);

            while (item.move_next())
                deserialize(item.element(),in); 
            item.reset();
        }
        catch (serialization_error e)
        { 
            item.clear();
            throw serialization_error(e.info + "\n   while deserializing object of type array2d_kernel_1"); 
        }
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename T,
        typename mem_manager
        >
    void array2d_kernel_1<T,mem_manager>::
    set_size (
        long width__,
        long height__
    )
    {
        // set the enumerator back at the start
        at_start_ = true;
        cur = 0;

        // don't do anything if we are already the right size.
        if (width_ == width__ && height_ == height__)
        {
            return;
        }

        width_ = width__;
        height_ = height__;

        // free any existing memory
        if (data != 0)
        {
            pool.deallocate_array(data);
            rpool.deallocate_array(reinterpret_cast<row_helper*>(rows));
            data = 0;
            rows = 0;
        }

        // now setup this object to have the new size
        try
        {
            rows = rpool.allocate_array(height_);
            data = pool.allocate_array(height_*width_);
            last = data + height_*width_ - 1;
        }
        catch (...)
        {
            if (rows)
                rpool.deallocate_array(reinterpret_cast<row_helper*>(rows));
            if (data)
                pool.deallocate_array(data);

            rows = 0;
            data = 0;
            width_ = 0;
            height_ = 0;
            last = 0;
            throw;
        }

        // now set up all the rows
        for (long i = 0; i < height_; ++i)
        {
            rows[i].width_ = width_;
            rows[i].data = data + i*width_;
        }
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_ARRAY2D_KERNEl_1_ 

