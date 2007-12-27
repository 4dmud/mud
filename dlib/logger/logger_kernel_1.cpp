// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_LOGGER_KERNEL_1_CPp_
#define DLIB_LOGGER_KERNEL_1_CPp_

#include "logger_kernel_1.h"
#include <iostream>
#include <sstream>

namespace dlib
{
    
// ----------------------------------------------------------------------------------------

    void set_all_logging_output_streams (
        std::ostream& out_
    )
    {
        logger::global_data& gd = logger::get_global_data();
        auto_mutex M(gd.m);
        gd.loggers.reset();
        while (gd.loggers.move_next())
        {
            gd.loggers.element()->out.rdbuf(out_.rdbuf());
        }
    }

    void set_all_logging_levels (
        const log_level& new_level
    )
    {
        logger::global_data& gd = logger::get_global_data();
        auto_mutex M(gd.m);
        gd.loggers.reset();
        while (gd.loggers.move_next())
        {
            gd.loggers.element()->cur_level = new_level;
        }
    }

// ----------------------------------------------------------------------------------------

    namespace logger_helper_stuff
    {
        class helper
        {
        public:
            helper()
            {
                std::ostringstream sout;
                print_default_logger_header(sout,"some_name",LDEBUG,0);
            }
        };
        // do this to make sure all the static members of print_default_logger_header get 
        // initialized when the program turns on.
        static helper a;
        // this logger exists to guarantee that all the loggers won't be destructed before the
        // program ends.
        static logger log("dlib");
    }

// ----------------------------------------------------------------------------------------

    void print_default_logger_header (
        std::ostream& out,
        const std::string& logger_name,
        const log_level& l,
        const uint64 thread_id
    )
    {
        using namespace std;
        static timestamper ts;
        static const uint64 first_time = ts.get_timestamp();

        const uint64 cur_time = (ts.get_timestamp() - first_time)/1000;
        streamsize old_width = out.width(); out.width(5);
        out << cur_time << " " << l.name; 
        out.width(old_width);

        out << " [" << thread_id << "] " << logger_name << ": ";
    }

// ----------------------------------------------------------------------------------------

    void logger::global_data::
    thread_end_handler (
    )
    {
        auto_mutex M(m);
        thread_id_type id = get_thread_id();
        thread_id_type junkd;
        uint64 junkr;
        thread_names.remove(id,junkd,junkr);
    }

// ----------------------------------------------------------------------------------------

    uint64 logger::global_data::
    get_thread_name (
    )
    {
        thread_id_type id = get_thread_id();
        uint64 thread_name;
        if (thread_names.is_in_domain(id))
        {
            thread_name = thread_names[id];
        }
        else
        {
            register_thread_end_handler(*this,&global_data::thread_end_handler);
            thread_name = next_thread_name;
            thread_names.add(id,thread_name);
            thread_name = next_thread_name;
            ++next_thread_name;
        }
        return thread_name;
    }

// ----------------------------------------------------------------------------------------

    void logger::logger_stream::
    print_header_and_stuff (
    )
    {
        if (!been_used)
        {
            log.gd.m.lock();
            log.logger_header()(log.out,log.name(),l,log.gd.get_thread_name());
            been_used = true;
        }
    }

// ----------------------------------------------------------------------------------------

    void logger::logger_stream::
    print_end_of_line (
    )
    {
        auto_unlock M(log.gd.m);
        if (log.auto_flush_enabled)
            log.out << std::endl;
        else
            log.out << "\n";
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_LOGGER_KERNEL_1_CPp_

