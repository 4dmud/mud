// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_MAP_KERNEl_C_
#define DLIB_MAP_KERNEl_C_

#include "map_kernel_abstract.h"
#include "../algs.h"
#include "../assert.h"
#include "../interfaces/map_pair.h"

namespace dlib
{

    template <
        typename map_base
        >
    class map_kernel_c : public map_base
    {

        typedef typename map_base::domain_type domain;
        typedef typename map_base::range_type range;
        
        public:
            void add (
                domain& d,
                range& r
            );

            void remove_any (
                domain& d,
                range& r
            );

            void remove (
                const domain& d,
                domain& d_copy,
                range& r
            );

            range& operator[] (
                const domain& d
            );

            const range& operator[] (
                const domain& d
            ) const;

            const map_pair<domain,range>& element (
            ) const;

            map_pair<domain,range>& element (
            );

    };

    template <
        typename map_base
        >
    inline void swap (
        map_kernel_c<map_base>& a, 
        map_kernel_c<map_base>& b 
    ) { a.swap(b); }  

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename map_base
        >
    void map_kernel_c<map_base>::
    add (
        domain& d,
        range& r
    )
    {
        // make sure requires clause is not broken
        CASSERT( (!is_in_domain(d)) &&
                (reinterpret_cast<void*>(&d) != reinterpret_cast<void*>(&r)),
            "\tvoid map::add"
            << "\n\tdomain element being added must not already be in the map"
            << "\n\tand d and r must not be the same variable"
            << "\n\tis_in_domain(d): " << (is_in_domain(d) ? "true" : "false")
            << "\n\tthis: " << this
            << "\n\t&d:   " << reinterpret_cast<void*>(&d)
            << "\n\t&r:   " << reinterpret_cast<void*>(&r)
            );

        // call the real function
        map_base::add(d,r);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename map_base
        >
    void map_kernel_c<map_base>::
    remove_any (
        domain& d,
        range& r
    )
    {
        // make sure requires clause is not broken
        CASSERT( (this->size() > 0)  &&
                (reinterpret_cast<void*>(&d) != reinterpret_cast<void*>(&r)),
            "\tvoid map::remove_any"
            << "\n\tsize() must be greater than zero if something is going to be removed"
            << "\n\tand d and r must not be the same variable."
            << "\n\tsize(): " << this->size() 
            << "\n\tthis:   " << this
            << "\n\t&d:     " << reinterpret_cast<void*>(&d)
            << "\n\t&r:     " << reinterpret_cast<void*>(&r)
            );

        // call the real function
        map_base::remove_any(d,r);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename map_base
        >
    void map_kernel_c<map_base>::
    remove (
        const domain& d,
        domain& d_copy,
        range& r
    )
    {
        // make sure requires clause is not broken
        CASSERT( (is_in_domain(d)) &&
                (reinterpret_cast<const void*>(&d) != reinterpret_cast<void*>(&r)) &&
                (reinterpret_cast<void*>(&r) != reinterpret_cast<void*>(&d_copy)) &&
                (reinterpret_cast<const void*>(&d) != reinterpret_cast<void*>(&d_copy)),
            "\tvoid map::remove"
            << "\n\tcan't remove something that isn't in the map or paremeters actually"
            << "\n\tthe same variable.  Either way can't remove."
            << "\n\tis_in_domain(d): " << (is_in_domain(d) ? "true" : "false")
            << "\n\tthis:      " << this
            << "\n\t&d:        " << reinterpret_cast<const void*>(&d)
            << "\n\t&r:        " << reinterpret_cast<void*>(&r)
            << "\n\t&d_copy:   " << reinterpret_cast<void*>(&d_copy)
            );

        // call the real function
        map_base::remove(d,d_copy,r);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename map_base
        >
    typename map_base::range_type& map_kernel_c<map_base>::
    operator[] (
        const domain& d
    )
    {
        // make sure requires clause is not broken
        CASSERT( is_in_domain(d),
            "\trange& map::operator[]"
            << "\n\td must be in the domain of the map"
            << "\n\tthis: " << this
            );

        // call the real function
        return map_base::operator[](d);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename map_base
        >
    const typename map_base::range_type& map_kernel_c<map_base>::
    operator[] (
        const domain& d
    ) const
    {
        // make sure requires clause is not broken
        CASSERT( is_in_domain(d),
            "\tconst range& map::operator[]"
            << "\n\td must be in the domain of the map"
            << "\n\tthis: " << this
            );

        // call the real function
        return map_base::operator[](d);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename map_base
        >
    const map_pair<typename map_base::domain_type,typename map_base::range_type>& map_kernel_c<map_base>::
    element (
    ) const
    {
        // make sure requires clause is not broken
        CASSERT(this->current_element_valid() == true,
            "\tconst map_pair<domain,range>& map::element"
            << "\n\tyou can't access the current element if it doesn't exist"
            << "\n\tthis: " << this
            );

        // call the real function
        return map_base::element();
    }

// ----------------------------------------------------------------------------------------

    template <
        typename map_base
        >
    map_pair<typename map_base::domain_type,typename map_base::range_type>& map_kernel_c<map_base>::
    element (
    ) 
    {
        // make sure requires clause is not broken
        CASSERT(this->current_element_valid() == true,
            "\tmap_pair<domain,range>& map::element"
            << "\n\tyou can't access the current element if it doesn't exist"
            << "\n\tthis: " << this
            );

        // call the real function
        return map_base::element();
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_MAP_KERNEl_C_

