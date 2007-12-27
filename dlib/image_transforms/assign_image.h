// Copyright (C) 2007  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_ASSIGN_IMAGe_
#define DLIB_ASSIGN_IMAGe_

#include "../pixel.h"
#include "assign_image_abstract.h"

namespace dlib
{

    template <
        typename dest_image_type,
        typename src_image_type
        >
    void assign_image (
        dest_image_type& dest,
        const src_image_type& src
    )
    {

        dest.set_size(src.width(),src.height());

        dest.reset();
        src.reset();
        while (dest.move_next())
        {
            src.move_next();
            assign_pixel(dest.element(),src.element());
        }

        dest.reset();
        src.reset();
    }

}

#endif // DLIB_ASSIGN_IMAGe_



