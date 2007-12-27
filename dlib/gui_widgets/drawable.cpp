// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_DRAWABLe_CPP_
#define DLIB_DRAWABLe_CPP_

#include "drawable.h"

#include <algorithm>

namespace dlib
{

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// -----------  drawable_window object  ------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    void drawable_window::
    set_background_color (
        unsigned long red_,
        unsigned long green_,
        unsigned long blue_
    )
    {
        wm.lock();
        red = red_;
        green = green_;
        blue = blue_;
        wm.unlock();
        // now repaint the window
        unsigned long width,height;
        get_size(width,height);
        rectangle rect(0,0,width-1,height-1);
        invalidate_rectangle(rect);
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    paint (
        const canvas& c
    )
    {
        ++event_id;
        c.fill(red,green,blue);

        widgets.reset();
        while (widgets.move_next())
        {
            widgets.element().value().reset();
            while (widgets.element().value().move_next())
            {
                // only dispatch a draw() call if this widget isn't hidden
                if (widgets.element().value().element()->hidden == false &&
                    widgets.element().value().element()->event_id != event_id)
                {
                    widgets.element().value().element()->event_id = event_id;
                    widgets.element().value().element()->draw(c);
                }
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_user_event (
        void* p,
        int i
    )
    {
        drawable* d = reinterpret_cast<drawable*>(p);
        if (widget_set.is_member(d))
        {
            d->on_user_event(i);
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_window_moved(
    )
    {
        ++event_id;
        window_moved.reset();
        while (window_moved.move_next())
        {
            if (window_moved.element()->event_id != event_id)
            {
                window_moved.element()->event_id = event_id;
                window_moved.element()->on_window_moved();
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_window_resized(
    )
    {
        ++event_id;
        window_resized.reset();
        while (window_resized.move_next())
        {
            if (window_resized.element()->event_id != event_id)
            {
                window_resized.element()->event_id = event_id;
                window_resized.element()->on_window_resized();
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_keydown (
        unsigned long key,
        bool is_printable,
        bool shift,
        bool ctrl
    )
    {
        ++event_id;
        keyboard.reset();
        while (keyboard.move_next())
        {
            if (keyboard.element()->event_id != event_id)
            {
                keyboard.element()->event_id = event_id;
                keyboard.element()->on_keydown(key,is_printable,shift,ctrl);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_focus_gained (
    )
    {
        ++event_id;
        focus.reset();
        while (focus.move_next())
        {
            if (focus.element()->event_id != event_id)
            {
                focus.element()->event_id = event_id;
                focus.element()->on_focus_gained();
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_focus_lost (
    )
    {
        ++event_id;
        focus.reset();
        while (focus.move_next())
        {
            if (focus.element()->event_id != event_id)
            {
                focus.element()->event_id = event_id;
                focus.element()->on_focus_lost();
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_mouse_down (
        unsigned long btn,
        unsigned long state,
        long x,
        long y,
        bool is_double_click
    )
    {
        lastx = x;
        lasty = y;

        ++event_id;
        mouse_click.reset();
        while (mouse_click.move_next())
        {
            if (mouse_click.element()->event_id != event_id)
            {
                mouse_click.element()->event_id = event_id;
                mouse_click.element()->on_mouse_down(btn,state,x,y,is_double_click);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_mouse_up (
        unsigned long btn,
        unsigned long state,
        long x,
        long y
    )
    {
        lastx = x;
        lasty = y;

        ++event_id;
        mouse_click.reset();
        while (mouse_click.move_next())
        {
            if (mouse_click.element()->event_id != event_id)
            {
                mouse_click.element()->event_id = event_id;
                mouse_click.element()->on_mouse_up(btn,state,x,y);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_mouse_move (
        unsigned long state,
        long x,
        long y
    )
    {
        lastx = x;
        lasty = y;

        ++event_id;
        mouse_move.reset();
        while (mouse_move.move_next())
        {
            if (mouse_move.element()->event_id != event_id)
            {
                mouse_move.element()->event_id = event_id;
                mouse_move.element()->on_mouse_move(state,x,y);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_mouse_leave (
    )
    {
        lastx = -1;
        lasty = -1;

        ++event_id;
        mouse_move.reset();
        while (mouse_move.move_next())
        {
            if (mouse_move.element()->event_id != event_id)
            {
                mouse_move.element()->event_id = event_id;
                mouse_move.element()->on_mouse_leave();
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_mouse_enter (
    )
    {
        ++event_id;
        mouse_move.reset();
        while (mouse_move.move_next())
        {
            if (mouse_move.element()->event_id != event_id)
            {
                mouse_move.element()->event_id = event_id;
                mouse_move.element()->on_mouse_enter();
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_wheel_up (
    )
    {
        ++event_id;
        mouse_wheel.reset();
        while (mouse_wheel.move_next())
        {
            if (mouse_wheel.element()->event_id != event_id)
            {
                mouse_wheel.element()->event_id = event_id;
                mouse_wheel.element()->on_wheel_up();
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable_window::
    on_wheel_down (
    )
    {
        ++event_id;
        mouse_wheel.reset();
        while (mouse_wheel.move_next())
        {
            if (mouse_wheel.element()->event_id != event_id)
            {
                mouse_wheel.element()->event_id = event_id;
                mouse_wheel.element()->on_wheel_down();
            }
        }
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// -----------  drawable object  ----------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    void drawable::
    fill_rect_with_vertical_gradient (
        const rectangle& rect,
        const canvas& c,
        unsigned char red_top,
        unsigned char green_top,
        unsigned char blue_top,
        unsigned char red_bottom,
        unsigned char green_bottom,
        unsigned char blue_bottom
    )
    {
        rectangle area = rect.intersect(c);
        unsigned char red, green, blue;
        const double range = rect.height();

        const double rd = (double)red_bottom - (double)red_top;
        const double gd = (double)green_bottom - (double)green_top;
        const double bd = (double)blue_bottom - (double)blue_top;

        for (long y = area.top(); y <= area.bottom(); ++y)
        {
            red   = static_cast<unsigned char>(red_top   + rd*(y-rect.top())/range);
            green = static_cast<unsigned char>(green_top + gd*(y-rect.top())/range);
            blue  = static_cast<unsigned char>(blue_top  + bd*(y-rect.top())/range);

            for (long x = area.left(); x <= area.right(); ++x)
            {
                canvas::pixel& pixel = c[y-c.top()][x-c.left()];
                pixel.red = red;
                pixel.green = green;
                pixel.blue = blue;
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    fill_rect (
        const rectangle& rect,
        const canvas& c,
        unsigned char red,
        unsigned char green,
        unsigned char blue
    )
    {
        rectangle area = rect.intersect(c);
        for (long y = area.top(); y <= area.bottom(); ++y)
        {
            for (long x = area.left(); x <= area.right(); ++x)
            {
                canvas::pixel& pixel = c[y-c.top()][x-c.left()];
                pixel.red = red;
                pixel.green = green;
                pixel.blue = blue;
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_checkered (
        const rectangle& a,
        const canvas& c,
        unsigned char red1,
        unsigned char green1,
        unsigned char blue1,
        unsigned char red2,
        unsigned char green2,
        unsigned char blue2
    )
    {
        rectangle area = a.intersect(c);
        if (area.is_empty())
            return;

        for (long i = area.left(); i <= area.right(); ++i)
        {
            for (long j = area.top(); j <= area.bottom(); ++j)
            {
                canvas::pixel& p = c[j - c.top()][i - c.left()];
                if (j&0x1 ^ i&0x1)
                {
                    p.red = red1;
                    p.green = green1;
                    p.blue = blue1;
                }
                else
                {
                    p.red = red2;
                    p.green = green2;
                    p.blue = blue2;
                }
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_sunken_rectangle (
        const rectangle& border,
        const canvas& c
    )
    {
        rectangle area = border.intersect(c);
        if (area.is_empty() == false)
        {
            draw_line(border.left(),border.top(),border.right()-1,border.top(),c,128,128,128);

            draw_line(border.left(),border.bottom(),border.right(),border.bottom(),c,255,255,255);
            draw_line(border.left()+1,border.bottom()-1,border.right()-1,border.bottom()-1,c,212,208,200);

            draw_line(border.left(),border.top()+1,border.left(),border.bottom()-1,c,128,128,128);

            draw_line(border.right(),border.top(),border.right(),border.bottom()-1,c,255,255,255);
            draw_line(border.right()-1,border.top()+1,border.right()-1,border.bottom()-2,c,212,208,200);

            draw_line(border.left()+1,border.top()+1,border.left()+1,border.bottom()-2,c,64,64,64);
            draw_line(border.left()+1,border.top()+1,border.right()-2,border.top()+1,c,64,64,64);
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_button_down (
        const rectangle& btn,
        const canvas& c
    )
    {
        rectangle area = btn.intersect(c);
        if (area.is_empty() == false)
        {
            draw_line(btn.left(),btn.top(),btn.right(),btn.top(),c,0,0,0);

            draw_line(btn.left()+1,btn.bottom(),btn.right(),btn.bottom(),c,64,64,64);
            draw_line(btn.left()+1,btn.top()+1,btn.right()-1,btn.top()+1,c,128,128,128);

            draw_line(btn.left(),btn.top()+1,btn.left(),btn.bottom(),c,0,0,0);

            draw_line(btn.right(),btn.top()+1,btn.right(),btn.bottom()-1,c,64,64,64);
            draw_line(btn.left()+1,btn.top()+1,btn.left()+1,btn.bottom()-1,c,128,128,128);
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_button_up (
        const rectangle& btn,
        const canvas& c
    )
    {
        rectangle area = btn.intersect(c);
        if (area.is_empty() == false)
        {
            draw_line(btn.left(),btn.top(),btn.right()-1,btn.top(),c,255,255,255);

            draw_line(btn.left(),btn.bottom(),btn.right(),btn.bottom(),c,64,64,64);
            draw_line(btn.left()+1,btn.bottom()-1,btn.right()-1,btn.bottom()-1,c,128,128,128);

            draw_line(btn.left(),btn.top()+1,btn.left(),btn.bottom()-1,c,255,255,255);

            draw_line(btn.right(),btn.top(),btn.right(),btn.bottom()-1,c,64,64,64);
            draw_line(btn.right()-1,btn.top()+1,btn.right()-1,btn.bottom()-2,c,128,128,128);
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_rectangle (
        rectangle rect,
        const canvas& c,
        unsigned char red,
        unsigned char green,
        unsigned char blue
    )
    {
        // top line
        draw_line(rect.left(),rect.top(),
                  rect.right(),rect.top(),
                  c, red, green, blue);

        // bottom line
        draw_line(rect.left(),rect.bottom(),
                  rect.right(),rect.bottom(),
                  c, red, green, blue);

        // left line
        draw_line(rect.left(),rect.top(),
                  rect.left(),rect.bottom(),
                  c, red, green, blue);

        // right line
        draw_line(rect.right(),rect.top(),
                  rect.right(),rect.bottom(),
                  c, red, green, blue);
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_line (
        long x1,
        long y1,
        long x2,
        long y2,
        const canvas& c,
        unsigned char red,
        unsigned char green,
        unsigned char blue
    ) 
    {
        if (x1 == x2)
        {
            // if the x coordinate is inside the canvas's area
            if (x1 <= c.right() && x1 >= c.left())
            {
                // make sure y1 comes before y2
                if (y1 > y2)
                    swap(y1,y2);

                y1 = std::max(y1,c.top());
                y2 = std::min(y2,c.bottom());
                // this is a vertical line
                for (long y = y1; y <= y2; ++y)
                {
                    canvas::pixel& pixel = c[y-c.top()][x1-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }
            }
        }
        else if (y1 == y2)
        {
            // if the y coordinate is inside the canvas's area
            if (y1 <= c.bottom() && y1 >= c.top())
            {
                // make sure x1 comes before x2
                if (x1 > x2)
                    swap(x1,x2);

                x1 = std::max(x1,c.left());
                x2 = std::min(x2,c.right());
                // this is a horizontal line
                for (long x = x1; x <= x2; ++x)
                {
                    canvas::pixel& pixel = c[y1-c.top()][x-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }
            }
        }
        else
        {
            const long rise = (((long)y2) - ((long)y1));
            const long run = (((long)x2) - ((long)x1));
            if (std::abs(rise) < std::abs(run))
            {
                const double slope = ((double)rise)/run;

                double first, last;

                if (x1 > x2)                
                {
                    first = std::max(x2,c.left());
                    last = std::min(x1,c.right());
                    }
                else
                {
                    first = std::max(x1,c.left());
                    last = std::min(x2,c.right());
                    }                             

                long y;
                long x;
                const double x1f = x1;
                const double y1f = y1;
                for (double i = first; i <= last; ++i)
                {   
                    y = static_cast<long>(slope*(i-x1f) + y1f);
                    x = static_cast<long>(i);

                    if (y < c.top() || y > c.bottom() )
                        continue;

                    canvas::pixel& pixel = c[y-c.top()][x-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }         
            }
            else
            {
                const double slope = ((double)run)/rise;

                double first, last;

                if (y1 > y2)                
                {
                    first = std::max(y2,c.top());
                    last = std::min(y1,c.bottom());
                    }
                else
                {
                    first = std::max(y1,c.top());
                    last = std::min(y2,c.bottom());
                    }                             

                long x;
                long y;
                const double x1f = x1;
                const double y1f = y1;
                for (double i = first; i <= last; ++i)
                {   
                    x = static_cast<long>(slope*(i-y1f) + x1f);
                    y = static_cast<long>(i);

                    if (x < c.left() || x > c.right() )
                        continue;

                    canvas::pixel& pixel = c[y-c.top()][x-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                } 
            }
        }

    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_circle (
        long x,
        long y,
        unsigned long radius,
        const canvas& c,
        unsigned char red,
        unsigned char green,
        unsigned char blue
    )
    {
        if (radius > 1)
        {
            long first_x = x - (radius-2);
            long last_x = x + (radius-2);
            const long rs = (radius-1) * (radius-1);

            // ensure that we only loop over the part of the x dimension that this
            // convas contains.
            if (first_x < c.left())
                first_x = c.left();
            if (last_x > c.right())
                last_x = c.right();

            long top, bottom;

            for (long i = first_x; i <= last_x; ++i)
            {
                const long a = i - x;
                // find the top of the arc
                top = square_root(rs - a*a);
                bottom = y - top;
                top += y;

                if (top >= c.top() && top <= c.bottom() )
                {
                    canvas::pixel& pixel = c[top-c.top()][i-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }

                if (bottom >= c.top() && bottom <= c.bottom() )
                {
                    canvas::pixel& pixel = c[bottom-c.top()][i-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }
            }
        }
        else if (radius == 1 &&
                 x >= c.left() && x <= c.right() &&
                 y >= c.top() && y <= c.bottom() )
        {
            canvas::pixel& pixel = c[y-c.top()][x-c.left()];

            pixel.blue = blue;
            pixel.green = green;
            pixel.red = red;
        }

        // we are doing the same thing as above but this time we loop over the y dimension.
        // doing this make the circle fill out right along its left and right sides
        if (radius > 1)
        {
            long first_y = y - (radius-2);
            long last_y = y + (radius-2);
            const long rs = (radius-1) * (radius-1);

            // ensure that we only loop over the part of the y dimension that this
            // convas contains.
            if (first_y < c.top())
                first_y = c.top();
            if (last_y > c.bottom())
                last_y = c.bottom();

            long left, right;

            for (long i = first_y; i <= last_y; ++i)
            {
                const long a = i - y;
                // find the top of the arc
                left = square_root(rs - a*a);
                right = x - left;
                left += x;

                if (left >= c.left() && left <= c.right() )
                {
                    canvas::pixel& pixel = c[i-c.top()][left-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }

                if (right >= c.left() && right <= c.right() )
                {
                    canvas::pixel& pixel = c[i-c.top()][right-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }
            }
        }

    }

// ----------------------------------------------------------------------------------------

    void drawable::
    draw_solid_circle (
        long x,
        long y,
        unsigned long radius,
        const canvas& c,
        unsigned char red,
        unsigned char green,
        unsigned char blue
    )
    {
        if (radius > 1)
        {
            long first_x = x - (radius-2);
            long last_x = x + (radius-2);
            const long rs = (radius-1) * (radius-1);

            // ensure that we only loop over the part of the x dimension that this
            // convas contains.
            if (first_x < c.left())
                first_x = c.left();
            if (last_x > c.right())
                last_x = c.right();

            long top, bottom;

            for (long i = first_x; i <= last_x; ++i)
            {
                const long a = i - x;
                // find the top of the arc
                top = square_root(rs - a*a);
                bottom = y - top;
                top += y;

                draw_line(i,top,i,bottom,c,red,green,blue);
            }
        }
        else if (radius == 1 &&
                 x >= c.left() && x <= c.right() &&
                 y >= c.top() && y <= c.bottom() )
        {
            canvas::pixel& pixel = c[y-c.top()][x-c.left()];

            pixel.blue = blue;
            pixel.green = green;
            pixel.red = red;
        }

        // we are doing the same thing as above but this time we loop over the y dimension.
        // doing this make the circle fill out right along its left and right sides
        if (radius > 1)
        {
            long first_y = y - (radius-2);
            long last_y = y + (radius-2);
            const long rs = (radius-1) * (radius-1);

            // ensure that we only loop over the part of the y dimension that this
            // convas contains.
            if (first_y < c.top())
                first_y = c.top();
            if (last_y > c.bottom())
                last_y = c.bottom();

            long left, right;

            for (long i = first_y; i <= last_y; ++i)
            {
                const long a = i - y;
                // find the top of the arc
                left = square_root(rs - a*a);
                right = x - left;
                left += x;

                if (left >= c.left() && left <= c.right() )
                {
                    canvas::pixel& pixel = c[i-c.top()][left-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }

                if (right >= c.left() && right <= c.right() )
                {
                    canvas::pixel& pixel = c[i-c.top()][right-c.left()];

                    pixel.blue = blue;
                    pixel.green = green;
                    pixel.red = red;
                }
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    enable_events (
    )
    {
        auto_mutex M(m);
        if (enabled_events == false)
        {
            enabled_events = true;
            drawable* temp = this;
            long zo = z_order_value;

            drawable_window::set_of_drawables* sod = parent.widgets[zo];
            if (sod == 0)
            {
                // this drawable is the first widget at this z order so we need
                // to make its containing set
                drawable_window::set_of_drawables s;
                s.add(temp);
                parent.widgets.add(zo,s);
            }
            else
            {
                sod->add(temp);
            }

            temp = this;
            parent.widget_set.add(temp);

            if (events & MOUSE_MOVE)
            {
                temp = this;
                parent.mouse_move.add(temp);
            }
            if (events & MOUSE_CLICK)
            {
                temp = this;
                parent.mouse_click.add(temp);
            }
            if (events & MOUSE_WHEEL)
            {
                temp = this;
                parent.mouse_wheel.add(temp);
            }
            if (events & WINDOW_RESIZED)
            {
                temp = this;
                parent.window_resized.add(temp);
            }
            if (events & KEYBOARD_EVENTS)
            {
                temp = this;
                parent.keyboard.add(temp);
            }
            if (events & FOCUS_EVENTS)
            {
                temp = this;
                parent.focus.add(temp);
            }
            if (events & WINDOW_MOVED)
            {
                temp = this;
                parent.window_moved.add(temp);
            }
            parent.invalidate_rectangle(rect);
        }
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    set_z_order (
        long order
    )
    {
        auto_mutex M(m);
        if (order != z_order_value && enabled_events)
        {
            // first remove this drawable from widgets
            drawable_window::set_of_drawables* sod = parent.widgets[z_order_value];
            drawable* junk;
            sod->remove(this,junk);

            // if there are no more drawables at this z order then destroy the
            // set for this order
            if (sod->size() == 0)
                parent.widgets.destroy(z_order_value);

            // now add this drawable to its new z order
            sod = parent.widgets[order];                
            if (sod == 0)
            {
                // this drawable is the first widget at this z order so we need
                // to make its containing set
                drawable_window::set_of_drawables s, x;
                s.add(junk);
                long temp_order = order;
                parent.widgets.add(temp_order,s);
            }
            else
            {
                sod->add(junk);
            }
            parent.invalidate_rectangle(rect);

        }
        z_order_value = order;
    }

// ----------------------------------------------------------------------------------------

    void drawable::
    disable_events (
    )
    {
        auto_mutex M(m);
        if (enabled_events)
        {
            enabled_events = false;
            // first remove this drawable from widgets
            drawable_window::set_of_drawables* sod = parent.widgets[z_order_value];
            drawable* junk;
            sod->remove(this,junk);

            // if there are no more drawables at this z order then destroy the
            // set for this order
            if (sod->size() == 0)
                parent.widgets.destroy(z_order_value);

            parent.widget_set.remove(this,junk);

            // now unregister this drawable from all the events it has registered for.
            if (events & MOUSE_MOVE)
                parent.mouse_move.remove(this,junk);
            if (events & MOUSE_CLICK)
                parent.mouse_click.remove(this,junk);
            if (events & MOUSE_WHEEL)
                parent.mouse_wheel.remove(this,junk);
            if (events & WINDOW_RESIZED)
                parent.window_resized.remove(this,junk);
            if (events & KEYBOARD_EVENTS)
                parent.keyboard.remove(this,junk);
            if (events & FOCUS_EVENTS)
                parent.focus.remove(this,junk);
            if (events & WINDOW_MOVED)
                parent.window_moved.remove(this,junk);
        }
    }

// ----------------------------------------------------------------------------------------

    drawable::
    ~drawable (
    )
    {
        ASSERT(events_are_enabled() == false,
            "\tdrawable::~drawable()"
            << "\n\tYou must disable events for drawable objects in their destructor by calling disable_events()."
            << "\n\tthis:     " << this
            );
        disable_events();
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_DRAWABLe_CPP_

