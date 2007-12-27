// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_LOGGER_KERNEl_ABSTRACT_
#ifdef DLIB_LOGGER_KERNEl_ABSTRACT_

#include "../threads.h"
#include <limits>
#include <string>
#include <iostream>
#include "../uintn.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    class log_level
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object is a simple named level to log at.  It contains a numeric 
                priority and a name to use in the logging messages.
        !*/
    public:
        log_level(
            int priority_, 
            const char* name_
        );  
        /*!
            ensures
                - #priority = priority_
                - the first 19 characters of name_ are copied into name and name
                  is null terminated.
        !*/

        int priority;
        char name[20];
    };

    const log_level LALL  (std::numeric_limits<int>::min(),"ALL");
    const log_level LNONE (std::numeric_limits<int>::max(),"NONE");
    const log_level LTRACE(-100,"TRACE");
    const log_level LDEBUG(0   ,"DEBUG");
    const log_level LINFO (100 ,"INFO ");
    const log_level LWARN (200 ,"WARN ");
    const log_level LERROR(300 ,"ERROR");
    const log_level LFATAL(400 ,"FATAL");

// ----------------------------------------------------------------------------------------

    void set_all_logging_output_streams (
        std::ostream& out
    );
    /*!
        ensures
            - for all currently existing loggers L:
                - #L.output_streambuf() == out.rdbuf() 
        throws
            - std::bad_alloc
    !*/

    void set_all_logging_levels (
        const log_level& new_level
    );
    /*!
        ensures
            - for all currently existing loggers L:
                - #L.level() == new_level
        throws
            - std::bad_alloc
    !*/

// ----------------------------------------------------------------------------------------

    void print_default_logger_header (
        std::ostream& out,
        const std::string& logger_name,
        const log_level& l,
        const uint64 thread_id
    );
    /*!
        requires
            - is not called more than once at a time (i.e. is not called from multiple
              threads at the same time).
        ensures
            - let MS be the number of milliseconds since program start.  
            - prints a string to out in the form:  "MS l.name [thread_id] logger_name:"
    !*/

// ----------------------------------------------------------------------------------------

    class logger 
    {
        /*!
            INITIAL VALUE
                - name() == a user supplied value given to the constructor
                - level() == LNONE
                - output_streambuf() == std::cout.rdbuf()
                - logger_header() == print_default_logger_header
                - auto_flush() == true

            WHAT THIS OBJECT REPRESENTS
                This object represents a logging output stream in the style of the log4j
                logger available for Java.  Note that by default a logger is disabled (
                because level() == LNONE).  To make it log messages you must set the
                logging level you would like to use.
                
                Also note that unlike most other objects in this library there is only 
                one implementation of this object at a time.  Thus, to create instances 
                of the logger you would simply write logger my_logger("some_name");
            
            THREAD SAFETY
                All methods of this class are thread safe.  Note that it is safe to 
                chain calls to operator << such as:
                    log << LINFO << "message " << variable << " more message";
                The logger ensures that the entire statement executes atomically so the 
                message won't be broken up by other loggers in other threads.
        !*/

        class logger_stream
        {
        public:

            bool is_enabled (
            ) const;
            /*!
                ensures
                    - returns true if this logger stream will print out items
                      given to it by the << operator.  returns false otherwise.
            !*/

            template <typename T>
            logger_stream& operator << (
                const T& item
            );
            /*!
                ensures
                    - if (is_enabled()) then
                        - writes item to this output stream
                    - returns *this
            !*/
        };

    public:

        logger (  
            const char* name_
        );
        /*!
            requires
                - name_ != ""
            ensures                
                - #*this is properly initialized
                - #name() == name_
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~logger (
        );
        /*!
            ensures
                - any resources associated with *this have been released
        !*/

        void clear(
        );
        /*!
            ensures
                - #*this has its initial value
            throws
                - std::bad_alloc or dlib::thread_error
                    If either of these exceptions are thrown then #*this is unusable 
                    until clear() is called and succeeds.
        !*/

        const std::string& name (
        ) const;
        /*!
            ensures
                - returns the name of this logger
        !*/

        logger_stream operator << (
            const log_level& l
        ) const;
        /*!
            ensures
                - if (l.priority >= level().priority) then
                    - returns a logger stream with is_enabled() == true and this
                      stream will write its output to the streambuf given by 
                      output_streambuf().
                - else
                    - returns a logger stream with is_enabled() == false 
            throws
                - std::bad_alloc
        !*/

        bool is_child_of (
            const logger& log
        ) const;
        /*!
            ensures
                - if ( (name().find(log.name() + ".") == 0) || (log.name() == name()) ) then
                    - returns true
                      (i.e. if log.name() + "." is a prefix of name() or if both *this and log
                      have the same name then return true)
                - else
                    - returns false
        !*/

        const log_level level (
        ) const;
        /*!
            ensures
                - returns the current log level of this logger.
        !*/

        void set_level (
            const log_level& new_level
        );
        /*!
            ensures
                - for all currently existing loggers L such that L.is_child_of(*this) == true:
                    - #L.level() == new_level
            throws
                - std::bad_alloc
        !*/

        bool auto_flush (
        );
        /*!
            ensures
                - returns true if the output stream is flushed after every logged message.
                  returns false otherwise.
        !*/

        void set_auto_flush (
            bool enabled
        );
        /*!
            ensures
                - for all currently existing loggers L such that L.is_child_of(*this) == true:
                    - #L.auto_flush() == enabled 
            throws
                - std::bad_alloc
        !*/

        std::streambuf* output_streambuf (
        );
        /*!
            ensures
                - returns the output stream buffer that this logger writes all
                  messages to.
        !*/

        void set_output_stream (
            std::ostream& out
        );
        /*!
            ensures
                - for all currently existing loggers L such that L.is_child_of(*this) == true:
                    - #L.output_streambuf() == out.rdbuf() 
            throws
                - std::bad_alloc
        !*/

        typedef void (*print_header_type)(
                std::ostream& out, 
                const std::string& logger_name, 
                const log_level& l,
                const uint64 thread_id
                );

        print_header_type logger_header (
        ) const;
        /*!
            ensures
                - returns the function that is called to print the header information 
                  onto each logged message.  The arguments to the function have the following
                  meanings:
                    - out == The output stream this function writes the header to.
                    - logger_name == The name of the logger that is printing the log message.
                    - l == The level of the logger that is printing the log message.
                    - thread_id == A number that uniquely identifies the thread trying to log
                      the message.  Note that this number is unique among all threads, past and
                      present.  Also note that this id is not the same one returned by
                      get_thread_id().
                - This logger_header function will also only be called once at a time. This means
                  the logger_header function doesn't need to be thread safe.
        !*/

        void set_logger_header (
            print_header_type print_header
        );
        /*!
            ensures
                - for all currently existing loggers L such that L.is_child_of(*this) == true:
                    - #L.logger_header() == print_header 
            throws
                - std::bad_alloc
        !*/

    private:

        // restricted functions
        logger(const logger&);        // copy constructor
        logger& operator=(const logger&);    // assignment operator

    };    

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_LOGGER_KERNEl_ABSTRACT_

