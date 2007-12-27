// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_LOGGER_KERNEl_1_
#define DLIB_LOGGER_KERNEl_1_

#include "../threads.h"
#include "../misc_api.h"
#include "../set.h"
#include "logger_kernel_abstract.h"
#include <limits>
#include <cstring>
#include "../algs.h"
#include "../assert.h"
#include "../uintn.h"
#include "../map.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    class log_level
    {
    public:
        log_level(
            int priority_, 
            const char* name_
        ) : 
            priority(priority_)
        {
            strncpy(name,name_,19);
            name[19] = '\0';
        }

        int priority;
        char name[20];
    };

    const log_level LALL  (std::numeric_limits<int>::min(),"ALL");
    const log_level LNONE (std::numeric_limits<int>::max(),"NONE");
    const log_level LTRACE(-100,"TRACE");
    const log_level LDEBUG(0  ,"DEBUG");
    const log_level LINFO (100,"INFO ");
    const log_level LWARN (200,"WARN ");
    const log_level LERROR(300,"ERROR");
    const log_level LFATAL(400,"FATAL");

// ----------------------------------------------------------------------------------------

    void set_all_logging_output_streams (
        std::ostream& out
    );

    void set_all_logging_levels (
        const log_level& new_level
    );

// ----------------------------------------------------------------------------------------

    void print_default_logger_header (
        std::ostream& out,
        const std::string& logger_name,
        const log_level& l,
        const uint64 thread_id
    );

// ----------------------------------------------------------------------------------------

    class logger 
    {
        /*!
            INITIAL VALUE
                - print_header == print_default_logger_header
                - out.rdbuf() == std::cout.rdbuf()
                - cur_level == LNONE
                - auto_flush_enabled == true 

            CONVENTION
                - print_header == logger_header()
                - out.rdbuf() == output_streambuf()
                - cur_level == level()
                - logger_name == name()
                - auto_flush_enabled == auto_flush()

                - logger::gd::loggers == a set containing all currently existing loggers.
                - logger::gd::m == the mutex used to lock everything in the logger
                - logger::gd::thread_names == a map of thread ids to thread names.  
                - logger::gd::next_thread_name == the next thread name that will be given out
                  to a thread when we find that it isn't already in thread_names.
        !*/

        class logger_stream
        {
            /*!
                INITIAL VALUE
                    - been_used == false

                CONVENTION
                    - enabled == is_enabled()
                    - if (been_used) then
                        - logger::gd::m is locked
                        - someone has used the << operator to write something to the
                          output stream.
            !*/
        public:
            logger_stream (
                const log_level& l_,
                logger& log_
            ) :
                l(l_),
                log(log_),
                been_used(false),
                enabled (l.priority >= log.cur_level.priority)
            {}

            inline ~logger_stream(
            )
            {
                if (!been_used)
                {
                    return;
                }
                else
                {
                    print_end_of_line();
                }
            }

            bool is_enabled (
            ) const { return enabled; }

            template <typename T>
            inline logger_stream& operator << (
                const T& item
            )
            {
                if (!enabled)
                {
                    return *this;
                }
                else
                {
                    print_header_and_stuff();
                    log.out << item;
                    return *this;
                }
            }

        private:

            void print_header_and_stuff (
            );
            /*!
                ensures
                    - if (!been_used) then
                        - prints the logger header 
                        - locks log.gd.m
                        - #been_used == true
            !*/

            void print_end_of_line (
            );
            /*!
                ensures
                    - prints a newline to log.out
                    - unlocks log.gd.m
            !*/

            const log_level& l;
            logger& log;
            bool been_used;
            const bool enabled;
        };

        friend class logger_stream;
    public:

        logger (  
            const char* name_
        ) : 
            gd(get_global_data()),
            out(std::cout.rdbuf()),
            cur_level(std::numeric_limits<int>::max(),"NONE"),
            logger_name(name_),
            auto_flush_enabled(true)
        {
            ASSERT(name_[0] != '\0',
                "\tlogger::logger()"
                << "\n\tYou can't make a logger with an empty name"
                << "\n\tthis: " << this
                );

            auto_mutex M(gd.m);
            logger* temp = this;
            gd.loggers.add(temp);

            print_header = print_default_logger_header;
        }

        virtual ~logger (
        ) 
        { 
            gd.m.lock();
            logger* junk;
            gd.loggers.remove(this,junk);
            // if this is the last logger then delete our global resources.  This only 
            // happens when the program is ending because in logger_kernel_1.cpp we have 
            // declared a global instance of the logger.  So at the very least that one will
            // hang around until the program ends.
            if (gd.loggers.size() == 0)
            {
                gd.m.unlock();
                delete &gd;
            }
            else
            {
                gd.m.unlock();
            }
        }

        void clear(
        )
        {
            auto_mutex M(gd.m);
            cur_level = LNONE;
            out.rdbuf(std::cout.rdbuf());
            print_header = print_default_logger_header;
            auto_flush_enabled = true;
        }

        const std::string& name (
        ) const { return logger_name; }

        logger_stream operator << (
            const log_level& l
        ) const { return logger_stream(l,const_cast<logger&>(*this)); }

        bool is_child_of (
            const logger& log
        ) const
        {
            return (name().find(log.name() + ".") == 0) || (log.name() == name());
        }

        const log_level level (
        ) const 
        { 
            auto_mutex M(gd.m);
            return log_level(cur_level); 
        };

        void set_level (
            const log_level& new_level
        )
        {
            auto_mutex M(gd.m);
            gd.loggers.reset();
            while (gd.loggers.move_next())
            {
                if (gd.loggers.element()->is_child_of(*this))
                    gd.loggers.element()->cur_level = new_level;
            }
        }

        bool auto_flush (
        ) const 
        { 
            auto_mutex M(gd.m);
            return auto_flush_enabled;
        };

        void set_auto_flush (
            bool enabled
        )
        {
            auto_mutex M(gd.m);
            gd.loggers.reset();
            while (gd.loggers.move_next())
            {
                if (gd.loggers.element()->is_child_of(*this))
                    gd.loggers.element()->auto_flush_enabled = enabled;
            }
        }

        std::streambuf* output_streambuf (
        )
        {
            auto_mutex M(gd.m);
            return out.rdbuf();
        }

        void set_output_stream (
            std::ostream& out_
        ) 
        {
            auto_mutex M(gd.m);
            gd.loggers.reset();
            while (gd.loggers.move_next())
            {
                if (gd.loggers.element()->is_child_of(*this))
                    gd.loggers.element()->out.rdbuf(out_.rdbuf());
            }
        }

        typedef void (*print_header_type)(
            std::ostream& out, 
            const std::string& logger_name, 
            const log_level& l,
            const uint64 thread_id
        );

        print_header_type logger_header (
        ) const { return print_header; }

        void set_logger_header (
            print_header_type ph
        )
        {
            auto_mutex M(gd.m);
            gd.loggers.reset();
            while (gd.loggers.move_next())
            {
                if (gd.loggers.element()->is_child_of(*this))
                    gd.loggers.element()->print_header = ph;
            }
        }

    private:


        struct global_data
        {
            rmutex m;
            set<logger*>::kernel_1b loggers;
            map<thread_id_type,uint64>::kernel_1b thread_names;
            uint64 next_thread_name;

            global_data () : next_thread_name(1) 
            { 
                // make sure the main program thread always has id 0
                thread_id_type main_id = get_main_thread_id();
                uint64 id_zero = 0;
                thread_names.add(main_id,id_zero);
            }

            uint64 get_thread_name (
            );
            /*!
                requires
                    - m is locked
                ensures
                    - returns a unique id for the calling thread.  also makes the number
                      small and nice unlike what you get from get_thread_id()
            !*/

            void thread_end_handler (
            );
            /*!
                ensures
                    - removes the terminated thread from thread_names
            !*/
        };

        static global_data& get_global_data()
        {
            static global_data* gd = new global_data;
            return *gd;
        }

        friend void set_all_logging_levels (
            const log_level& new_level
        );

        friend void set_all_logging_output_streams (
            std::ostream& out
        );


        global_data& gd;

        std::ostream out;
        log_level cur_level;
        print_header_type print_header;
        const std::string logger_name;
        bool auto_flush_enabled;


        // restricted functions
        logger(const logger&);        // copy constructor
        logger& operator=(const logger&);    // assignment operator

    };    

// ----------------------------------------------------------------------------------------

}

#ifdef NO_MAKEFILE
#include "logger_kernel_1.cpp"
#endif

#endif // DLIB_LOGGER_KERNEl_1_

