// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_HASH_MAP_KERNEl_C_
#define DLIB_HASH_MAP_KERNEl_C_

#include "hash_map_kernel_abstract.h"
#include "../algs.h"
#include "../assert.h"

namespace dlib
{

    template <
        typename hash_map_base
        >
    class hash_map_kernel_c : public hash_map_base
    {

        typedef typename hash_map_base::domain_type domain;
        typedef typename hash_map_base::range_type range;

        
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
        typename hash_map_base
        >
    inline void swap (
        hash_map_kernel_c<hash_map_base>& a, 
        hash_map_kernel_c<hash_map_base>& b 
    ) { a.swap(b); }  

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename hash_map_base
        >
    void hash_map_kernel_c<hash_map_base>::
    add (
        domain& d,
        range& r
    )
    {

        // make sure requires clause is not broken
        CASSERT( (!is_in_domain(d)) &&
                (reinterpret_cast<void*>(&d) != reinterpret_cast<void*>(&r)),
            "\tvoid hash_map::add"
            << "\n\tdomain element being added must not already be in the hash_map"
            << "\n\tand d and r must not be the same variable"
            << "\n\tis_in_domain(d): " << (is_in_domain(d) ? "true" : "false")
            << "\n\tthis: " << this
            << "\n\t&d:   " << reinterpret_cast<void*>(&d)
            << "\n\t&r:   " << reinterpret_cast<void*>(&r)
            );


        // call the real function
        hash_map_base::add(d,r);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename hash_map_base
        >
    void hash_map_kernel_c<hash_map_base>::
    remove_any (
        domain& d,
        range& r
    )
    {


        // make sure requires clause is not broken
        CASSERT( (this->size() > 0)  &&
                (reinterpret_cast<void*>(&d) != reinterpret_cast<void*>(&r)),
            "\tvoid hash_map::remove_any"
            << "\n\tsize() must be greater than zero if something is going to be removed"
            << "\n\tand d and r must not be the same variable."
            << "\n\tsize(): " << this->size() 
            << "\n\tthis:   " << this
            << "\n\t&d:     " << reinterpret_cast<void*>(&d)
            << "\n\t&r:     " << reinterpret_cast<void*>(&r)
            );


        // call the real function
        hash_map_base::remove_any(d,r);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename hash_map_base
        >
    void hash_map_kernel_c<hash_map_base>::
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
            "\tvoid hash_map::remove"
            << "\n\tcan't remove something that isn't in the hash_map or paremeters"
            << "\n\tactually the same variable.  Either way can't remove."
            << "\n\tis_in_domain(d): " << (is_in_domain(d) ? "true" : "false")
            << "\n\tthis:      " << this
            << "\n\t&d:        " << reinterpret_cast<const void*>(&d)
            << "\n\t&r:        " << reinterpret_cast<void*>(&r)
            << "\n\t&d_copy:   " << reinterpret_cast<void*>(&d_copy)
            );


        // call the real function
        hash_map_base::remove(d,d_copy,r);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename hash_map_base
        >
    typename hash_map_base::range_type& hash_map_kernel_c<hash_map_base>::
    operator[] (
        const domain& d
    )
    {
        // make sure requires clause is not broken
        CASSERT( is_in_domain(d),
            "\trange& hash_map::operator[]"
            << "\n\td must be in the domain of the hash_map"
            << "\n\tthis: " << this
            );

        // call the real function
        return hash_map_base::operator[](d);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename hash_map_base
        >
    const typename hash_map_base::range_type& hash_map_kernel_c<hash_map_base>::
    operator[] (
        const domain& d
    ) const
    {
        // make sure requires clause is not broken
        CASSERT( is_in_domain(d),
            "\tconst range& hash_map::operator[]"
            << "\n\td must be in the domain of the hash_map"
            << "\n\tthis: " << this
            );

        // call the real function
        return hash_map_base::operator[](d);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename hash_map_base
        >
    const map_pair<typename hash_map_base::domain_type,typename hash_map_base::range_type>& hash_map_kernel_c<hash_map_base>::
    element (
    ) const
    {
        // make sure requires clause is not broken
        CASSERT(this->current_element_valid() == true,
            "\tconst map_pair<domain,range>& hash_map::element"
            << "\n\tyou can't access the current element if it doesn't exist"
            << "\n\tthis: " << this
            );

        // call the real function
        return hash_map_base::element();
    }

// ----------------------------------------------------------------------------------------

    template <
        typename hash_map_base
        >
    map_pair<typename hash_map_base::domain_type,typename hash_map_base::range_type>& hash_map_kernel_c<hash_map_base>::
    element (
    ) 
    {
        // make sure requires clause is not broken
        CASSERT(this->current_element_valid() == true,
            "\tmap_pair<domain,range>& hash_map::element"
            << "\n\tyou can't access the current element if it doesn't exist"
            << "\n\tthis: " << this
            );

        // call the real function
        return hash_map_base::element();
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_HASH_MAP_KERNEl_C_

