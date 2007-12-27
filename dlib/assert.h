// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_ASSERt_
#define DLIB_ASSERt_


#include <sstream>
#include <iosfwd>
#include "error.h"

// -----------------------------

// Use some stuff from boost here
//  (C) Copyright John Maddock 2001 - 2003.
//  (C) Copyright Darin Adler 2001.
//  (C) Copyright Peter Dimov 2001.
//  (C) Copyright Bill Kempf 2002.
//  (C) Copyright Jens Maurer 2002.
//  (C) Copyright David Abrahams 2002 - 2003.
//  (C) Copyright Gennaro Prota 2003.
//  (C) Copyright Eric Friedman 2003.
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef BOOST_JOIN
#define BOOST_JOIN( X, Y ) BOOST_DO_JOIN( X, Y )
#define BOOST_DO_JOIN( X, Y ) BOOST_DO_JOIN2(X,Y)
#define BOOST_DO_JOIN2( X, Y ) X##Y
#endif

// -----------------------------

namespace dlib
{
    template <bool value> struct compile_time_assert;
    template <> struct compile_time_assert<true> { enum {value=1};  };

    template <typename T, typename U> struct assert_are_same_type;
    template <typename T> struct assert_are_same_type<T,T> {enum{value=1};};
    template <typename T, typename U> struct assert_are_not_same_type {enum{value=1}; };
    template <typename T> struct assert_are_not_same_type<T,T> {};
}
#define COMPILE_TIME_ASSERT(expression) \
        typedef char BOOST_JOIN(DLIB_CTA, __LINE__)[::dlib::compile_time_assert<(bool)(expression)>::value] 

#define ASSERT_ARE_SAME_TYPE(type1, type2) \
        typedef char BOOST_JOIN(DLIB_AAST, __LINE__)[::dlib::assert_are_same_type<type1,type2>::value] 

#define ASSERT_ARE_NOT_SAME_TYPE(type1, type2) \
        typedef char BOOST_JOIN(DLIB_AANST, __LINE__)[::dlib::assert_are_not_same_type<type1,type2>::value] 

// -----------------------------

#define CASSERT(_exp,_message)                                                 \
    {if ( !(_exp) )                                                           \
    {                                                                       \
        std::ostringstream _out;                                             \
        _out << "\n\nError occurred at line " << __LINE__ << ".\n";          \
        _out << "Error occurred in file " << __FILE__ << ".\n\n";            \
        _out << "Failing expression was " << #_exp << ".\n";                  \
        _out << _message << "\n";                                              \
        dlib_assert_breakpoint();                                               \
        throw dlib::fatal_error(dlib::EBROKEN_ASSERT,_out.str());                   \
    }}                                                                       \

#if defined DEBUG || defined ENABLE_ASSERTS || defined _DEBUG
#define ASSERT(_exp,_message) CASSERT(_exp,_message)
// make sure ENABLE_ASSERTS is defined if we are indeed using them.
#ifndef ENABLE_ASSERTS
#define ENABLE_ASSERTS
#endif
#else
#define ASSERT(_exp,_message)
#endif

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

#endif // DLIB_ASSERt_

