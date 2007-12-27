// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_FONTs_CPP_
#define DLIB_FONTs_CPP_

#include "fonts.h"

#include "../serialize.h"
#include <sstream>
#include "../base64.h"
#include "../compress_stream.h"
   
namespace dlib
{
    namespace font_builder_namespace
    {
        /*
            The deal with this stuff here is just to make sure the default font
            is initialized before we get into winmain().
        */
        class helper
        {
        public:
            helper()
            {
                default_font::get_font();
            }
        };

        helper a;
    }
    
// ----------------------------------------------------------------------------------------

    const rectangle font::
    compute_cursor_rect (
        const rectangle& rect,
        const std::string& str,
        unsigned long index,
        std::string::size_type first,
        std::string::size_type last
    ) const
    {
        ASSERT ( (last == std::string::npos) || (first <= last && last < str.size())  ,
                "\trectangle font::compute_cursor_rect()"
                << "\n\tlast == std::string::npos: " << ((last == std::string::npos)?"true":"false") 
                << "\n\tfirst: " << (unsigned long)first 
                << "\n\tlast:  " << (unsigned long)last 
                << "\n\tindex:  " << index
                << "\n\tstr.size():  " << (unsigned long)str.size() );

        const font& f = *this;

        if (last == std::string::npos)
            last = str.size()-1;

        long x = f.left_overflow();
        long y = 0;
        int count = 0;

        if (str.size() != 0)
        {
            for (std::string::size_type i = first; i <= last && i < index; ++i)
            {
                ++count;
                x += f[str[i]].width();
                if (str[i] == '\n')
                {
                    x = f.left_overflow();
                    y += f.height();
                    count = 0;
                }
            }
            x += rect.left();
            y += rect.top();

        }

        // if the cursor is at the start of a line then back it up one pixel
        if (count == 0)
            --x;

        return rectangle(x,y,x,y+f.height()-1);
    }

// ----------------------------------------------------------------------------------------

    const unsigned long font::
    compute_cursor_pos (
        const rectangle& rect,
        const std::string& str,
        long x,
        long y,
        std::string::size_type first,
        std::string::size_type last
    ) const
    {
        ASSERT ( (last == std::string::npos) || (first <= last && last < str.size())  ,
                "\tunsigned long font::compute_cursor_pos()"
                << "\n\tlast == std::string::npos: " << ((last == std::string::npos)?"true":"false") 
                << "\n\tfirst: " << (unsigned long)first 
                << "\n\tlast:  " << (unsigned long)last 
                << "\n\tx:  " << x 
                << "\n\ty:  " << y 
                << "\n\tstr.size():  " << (unsigned long)str.size() );
        const font& f = *this;

        using namespace std;

        if (str.size() == 0)
            return 0;
        else if (first >= str.size())
            return str.size();

        y -= rect.top();
        x -= rect.left();
        if (y < 0)
            y = 0;
        if (x < 0)
            x = 0;

        if (last == std::string::npos)
            last = str.size()-1;


        // first figure out what line we are on
		string::size_type pos = first;
        long line = 0;
        while (static_cast<unsigned long>(y) >= f.height())
        {
            ++line;
            y -= f.height();
        }

        // find the start of the given line
        string::size_type last_pos = pos;
        for (string::size_type i = first; i <= last && line != 0; ++i)
        {
            if (str[i] == '\n')
            {
                --line;
                pos += (i - last_pos);
                last_pos = pos;
            }
        }

        // now str[pos] == the first character of the start of the line
        // that contains the cursor.


        long cur_x = f.left_overflow();
        // set the current cursor position to where the mouse clicked
        while (pos <= last)
        {
            if (x <= cur_x)
                break;

            cur_x += f[str[pos]].width();
            ++pos;
        }

        if (x <= cur_x)
        {
            if (pos != first)
            {
                const long width = f[str[pos-1]].width();
                if (x < cur_x - width/2)
                    --pos;
            }
        }
        return static_cast<unsigned long>(pos);
    }

// ----------------------------------------------------------------------------------------

    void font::
    compute_size (
        const std::string& str,
        unsigned long& width,
        unsigned long& height,
        std::string::size_type first,
        std::string::size_type last
    ) const
    {
        ASSERT ( (last == std::string::npos) || (first <= last && last < str.size())  ,
                "\tvoid font::compute_size()"
                << "\n\tlast == std::string::npos: " << ((last == std::string::npos)?"true":"false") 
                << "\n\tfirst: " << (unsigned long)first 
                << "\n\tlast:  " << (unsigned long)last 
                << "\n\tstr.size():  " << (unsigned long)str.size() );

        using namespace std;
        unsigned long line_width = 0;
        unsigned long newlines = 0;
        width = 0;
        height = 0;

        if (str.size())
        {
            if (last == std::string::npos)
                last = str.size()-1;
            const font& f = *this;

            for (string::size_type i = first; i <= last; ++i)
            {
                if (str[i] == '\n')
                {
                    ++newlines;
                    width = std::max(width,line_width);
                    line_width = 0;
                }
                else
                {
                    line_width += f[str[i]].width();
                }
            }
            width = std::max(width,line_width);

            height = (newlines+1)*f.height();
            width += f.left_overflow() + f.right_overflow();
        }
    }

// ----------------------------------------------------------------------------------------

    const void font::
    draw_string (
        const rectangle& rect,
        const std::string& str,
        const canvas& c,
        const unsigned char red,
        const unsigned char green,
        const unsigned char blue,
        std::string::size_type first,
        std::string::size_type last,
        unsigned long first_pixel
    ) const
    {
        ASSERT ( (last == std::string::npos) || (first <= last && last < str.size())  ,
                "\tvoid font::draw_string()"
                << "\n\tlast == std::string::npos: " << ((last == std::string::npos)?"true":"false") 
                << "\n\tfirst: " << (unsigned long)first 
                << "\n\tlast:  " << (unsigned long)last 
                << "\n\tstr.size():  " << (unsigned long)str.size() );

        rectangle area = rect.intersect(c);
        if (area.is_empty() || str.size() == 0)
            return;

        if (last == std::string::npos)
            last = str.size()-1;

        const font& f = *this;        

        long y_offset = rect.top() + f.ascender() - 1;

        long pos = rect.left()+f.left_overflow() - first_pixel;
        for (std::string::size_type i = first; i <= last; ++i)
        {
            if (str[i] == '\n')
            {
                y_offset += f.height();
                pos = rect.left()+f.left_overflow() - first_pixel;
            }

            // only look at letters in the intersection area
            if (area.left() > pos - static_cast<long>(f.left_overflow()) && 
                pos + static_cast<long>(f[str[i]].width() + f.right_overflow()) < area.left() )
            {
                pos += f[str[i]].width();                
                continue;
            }
            else if (area.right() + static_cast<long>(f.right_overflow()) < pos)
            {
                // keep looking because there might be a '\n' in the string that
                // will wrap us around and put us back into our rectangle.
                continue;
            }
            else if (area.bottom() + static_cast<long>(f.height()) < y_offset)
            {
                // the string is now below our rectangle so we are done
                break;
            }

            // at this point in the loop we know that f[str[i]] overlaps 
            // horizontally with the intersection rectangle area.

            const letter& l = f[str[i]];
            for (unsigned long i = 0; i < l.num_of_points(); ++i)
            {
                const long x = l[i].x + pos;
                const long y = l[i].y + y_offset;
                // draw each pixel of the letter if it is inside the intersection
                // rectangle
                if (area.contains(x,y))
                {
                    canvas::pixel& pixel = c[y-c.top()][x-c.left()];
                    
                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }
            }

            pos += l.width();
        }
    }

// ----------------------------------------------------------------------------------------

    default_font::
    default_font (
    ) 
    {
        using namespace std;
        l = new letter[256];

        try
        {
            ostringstream sout;
            istringstream sin;
            base64::kernel_1a coder;
            compress_stream::kernel_1ea cs;

            // Note that this function was converted to use this base64 encoded junk at 
            // release number 658.

            sout << "AWxK8W+hbyZNdFHTuMJf1mA/LDfa900/QRB9pncPnKtzZUUxcSPO0h2aZBzfwLf3UgHaC0aT6afc\n";
            sout << "1C6M65E6SJ2EEW8X5anPehRywNeFxKs+vVY6Qhj2D5n1q2MQotB8Ktx4iwzC2TpShiqPxU/5PxR4\n";
            sout << "TF0tUri/y/xyxcY9cNT0ZwcJseM5pX+iDlJI/PPjCt6gS78xT4FTvBwsWvwxzjtYCTemnK/T1SRO\n";
            sout << "f1NhGUQTR2NoTvPDEepsQWWm0cC99cLNvQTBI+tIcfG0Hlp2ol7bBd/uXlDKvuTcBbuWGEtf56pq\n";
            sout << "qk5Ej+3ipUMdb7x3//vN0ioBBhf8OAe/GmIz09LrtXnJ79TIEq5UTwnylgBBjYGhbuKUtigKhwkT\n";
            sout << "DK3c6E9pn3ljSuVggYex0mVDQGitSys7S68wCIe4kKjnnQI7rrgXQ/rFnWEN3wGFpf+c+9I5F04w\n";
            sout << "k6M0WpN377s1cC+zrycnM4S3GbsR4oEkiyAvM2YOY5IweU6QDCbLWqlrpWJpkMP8kFUfy4P9eNOV\n";
            sout << "DMGy8F6+oEMkUVyiDkG7MqgRBDUVZcD7EVRzInED+GgMW56k4juwoJOoo8jdItpBN704QJzNyNk5\n";
            sout << "jVJ4lQ++6WJ+iOh8Weh5J0o7qzfNcNFENfaXOXmKOkpVD5AwUmwkjIM6MKuAxXLakcqcpA9Y4cWJ\n";
            sout << "FKKQUGKOQvRLJezuOOlgK6g8soNd4JFQvz77n0EWXhNl7xKTdmnVAb9lQXKoVhF7I98wPtis3/vf\n";
            sout << "OsyGvfIyrFuF/e4f7+lRKD5BjgY8L02rAFXq0McnMDHG0sWO8LhHtM3nO2tcT4BIg/WuX3VyuaoV\n";
            sout << "v04wEh1Vs3+w+/SuVp2RkiJWkXqTv4gy8eeihJhPHFlsjQ0yVLRImjPMNR2JTUDtaQrrXUpNb54i\n";
            sout << "Jj0iAcmJ9zddMZy5stekNVTe0ag5KaofA+wL9HfoMu1oCOVClVSI2GyazaJwLmwrk/je61+5p00W\n";
            sout << "JRTIbsW/87vNjQc4trkrR6Blmg3dQ3WTONyzR/mpR07yCDn/6H792k7npFTE6r9Cq99YQJEAl6lR\n";
            sout << "QV7e3nZY2LlSbAQkxRv7nXzroMo33qXUSKPCdHntQCMqbqN4E6qkZvf3UMlaesWuOIuA3f+LuIeY\n";
            sout << "+LbZ2ZBTPfXLNZRWE+wQKKMIEXHsWX7OlcOmcNZOA/ymMxrRTw6hm2+OeAHmWlJSsV8ojLIuD4Ur\n";
            sout << "v7yCCE3RbSjhWZ62xbhn+O2+vzHY1Is0PitaqNzGRHU98hF7qoYg1AQxZLUIJm3Yf8K11jGZNFly\n";
            sout << "u9lgAkJ0VcL14JOTtL2Ci8zYe2R8Oo3cJvCz1GBhPpIfyNI+CBirWhHtLdpn2tCQ6sHhYPA1PiPl\n";
            sout << "Oxl/sBfcEZf0uOeYy9A5AMN+iplpk+xAQ/k4Bo3OHKN5p5SshECV33jV15pb8hBcjhOrUD/d/d0z\n";
            sout << "Tb0gWdtnZOyCIlexDP4+Zo6AUgc/pfjgNrLuEFmVQk9OUS3M6Zqy9NETLNifab9a50kfqvI7Nlvn\n";
            sout << "B/2YgJdifupG2SWf9XcfIgvvgFQBkF0VbLcd1wuyXpGq8g5zVq5xniMgqE94+W79LqZUfrq9BJOA\n";
            sout << "D7JvawNSbmE3AdYdb5qKVSyByvEClpQUpC1At9hhG4HAMboyWmbG5DPnpyKRQ7EaO6dShiGKSGDo\n";
            sout << "QVUM/jjFj2cxVJWIlvOK3soN/N6rsCMMT4YYG6ufyXhT0x8W59nsUgXvvGE0YyRV/+45ACh3O4s5\n";
            sout << "iEtqllE855cuZSb+tBl0m7uG2RmWLahp0+7nnJrE39CVkpchjYV49zpF1KANoUFRElbjhtZif/ll\n";
            sout << "ykuGwEABQvlT0fnBJCq8MWf4/MeBn7QqJhJs96uVijYzHFtc15Oc65ZZV5QDxhBul/M6cp+DxcQK\n";
            sout << "AzBLXhl9B3bdGsixXewhtlkRK/PseJm+UhqbquGuGSDH0jJ7L4tBLZClqjp6TXiVWs/s1PINFez9\n";
            sout << "dWbfqLAkH+xAA/Wrcq2sIziBCKozy1qvUd7BQAAAAAEAAAABAAABhhVLBxzy\n";

            sin.str(sout.str());
            sout.str("");

            coder.decode(sin,sout);

            sin.str(sout.str());
            sout.str("");

            cs.decompress(sin,sout);
            sin.str(sout.str());

            for (int i = 0; i < 256; ++i)
            {
                deserialize(l[i],sin);
            }
        }
        catch (...)
        {
            delete [] l;
            throw;
        }
    }

// ----------------------------------------------------------------------------------------

    void serialize (
        const letter& item, 
        std::ostream& out 
    )   
    {
        try
        {
            serialize(item.w,out);
            serialize(item.count,out);

            for (unsigned long i = 0; i < item.count; ++i)
            {
                serialize(item.points[i].x,out);
                serialize(item.points[i].y,out);
            }
        }
        catch (serialization_error e)
        { 
            throw serialization_error(e.info + "\n   while serializing object of type letter"); 
        }
    }

    void deserialize (
        letter& item, 
        std::istream& in
    )
    {
        try
        {
            if (item.points)
                delete [] item.points;

            deserialize(item.w,in);
            deserialize(item.count,in);

            if (item.count > 0)
                item.points = new letter::point[item.count];
            else
                item.points = 0;

            for (unsigned long i = 0; i < item.count; ++i)
            {
                deserialize(item.points[i].x,in);
                deserialize(item.points[i].y,in);
            }
        }
        catch (serialization_error e)
        { 
            item.w = 0;
            item.count = 0;
            item.points = 0;
            throw serialization_error(e.info + "\n   while deserializing object of type letter"); 
        }
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_FONTs_CPP_

