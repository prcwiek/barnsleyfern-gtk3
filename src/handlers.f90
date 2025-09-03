module handlers
    
    use gtk, only: gtk_init, gtk_window_new, gtk_box_new, gtk_window_set_title,&
                   gtk_window_set_default_size, gtk_widget_show, gtk_main,&
                   gtk_container_add, gtk_button_new_with_label,&
                   gtk_widget_set_margin_start, gtk_widget_set_margin_end,&
                   gtk_widget_set_margin_top, gtk_widget_set_margin_bottom,&
                   gtk_adjustment_new, gtk_grid_new, gtk_expander_new, gtk_grid_attach,&
                   gtk_grid_set_column_homogeneous, gtk_grid_set_row_homogeneous,&
                   gtk_expander_set_expanded, gtk_widget_show_all, gtk_label_new,&
                   gtk_spin_button_new, gtk_drawing_area_new, gtk_main_quit,&
                   gtk_widget_set_vexpand, gtk_widget_queue_draw, gtk_notebook_new,&
                   gtk_notebook_append_page, gtk_label_new_with_mnemonic,&
                   gtk_spin_button_get_value, gtk_window_set_resizable,&
                   gtk_radio_button_new_with_label, gtk_radio_button_get_group,&
                   gtk_toggle_button_set_active, gtk_toggle_button_get_active,&
                   gtk_spin_button_set_value, gtk_statusbar_new,&

                   gtk_statusbar_push, gtk_statusbar_get_context_id,&

                   g_signal_connect,&
                   GTK_WINDOW_TOPLEVEL,&
                   GTK_ORIENTATION_HORIZONTAL, GTK_ORIENTATION_VERTICAL,&
                   GDK_COLORSPACE_RGB,&
                   c_null_char, c_null_ptr, TRUE, FALSE
    
    use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                          gdk_pixbuf_get_rowstride, gdk_pixbuf_new
    
    use gdk, only : gdk_cairo_set_source_pixbuf
    
    use gtk_os_dependent, only: gdk_pixbuf_savev
    
    use cairo, only: cairo_paint, cairo_set_source, cairo_surface_write_to_png,&
                     cairo_surface_destroy
    
    use gtk_hl_chooser_modified

    implicit none

contains

    function delete_event(widget, event, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int

        integer (c_int) :: res
        type(c_ptr), value :: widget, event, gdata

        res =FALSE
        call gtk_main_quit()
    end function delete_event
    
    function draw(widget, my_cairo_context, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use global_widgets

        implicit none
        
        integer(c_int)  :: res
        type(c_ptr), value, intent(in) :: widget, my_cairo_context, gdata
        
        call gdk_cairo_set_source_pixbuf(my_cairo_context, pixbuf_area, 0d0, 0d0)
        call cairo_paint(my_cairo_context)
        
        res = FALSE
    end function draw

    recursive function start_calculations(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use global_widgets
        
        implicit none
        
        integer :: niter
        real :: sc
        integer(c_int) :: res, message_status
        type(c_ptr), value :: widget, gdata
        
        ! get number of iterations
        niter = INT(gtk_spin_button_get_value(spin_btn_iter))
        sc = INT(gtk_spin_button_get_value(spin_btn_scale))
        
        message_status = gtk_statusbar_push(status_bar, gtk_statusbar_get_context_id(status_bar,&
                            "BarnsleyFern"//c_null_char), "Running..."//c_null_char)
        call fern(niter, sc)
        
        call gtk_widget_queue_draw(draw_area)
        
        message_status = gtk_statusbar_push(status_bar, gtk_statusbar_get_context_id(status_bar,&
                            "BarnsleyFern"//c_null_char), "Ready"//c_null_char)
        res = FALSE
    end function start_calculations
    
    function clean_draw_area(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use global_widgets
        
        implicit none

        type(c_ptr), value :: widget, gdata
        integer(c_int) :: res
        
        pixel = char(0)
        
        call gtk_widget_queue_draw(draw_area)
        
        res = FALSE
    end function clean_draw_area
        
    function save_draw_area(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only : c_ptr, c_int
        use global_widgets
        
        implicit none
        
        type(c_ptr), value :: widget, gdata
        
        integer(c_int) :: res
        integer(c_int) :: cstatus
        integer(c_int) :: isel

        character(len=120), dimension(:), allocatable :: chfile
        character(len=30), dimension(1) :: filter
        character(len=30), dimension(1) :: filter_name
        character(len=240) :: filename
        
        
        filter(1) = "*png"
        filter_name(1) = "png files"
        
        isel = hl_gtk_file_chooser_show_modified(chfile, create=TRUE,&
                title="Select output file"//c_null_char, filter=filter,&
                filter_name=filter_name, initial_file=trim("barney_fern.png")//c_null_char,&
                wsize=(/ 600_c_int, 400_c_int /), &
                edit_filters=FALSE, confirm_overwrite=TRUE, all=FALSE,&
                parent=window)
        
        if (isel == FALSE) then
            res = FALSE
            return
        end if
        
        filename = chfile(1)
        deallocate(chfile)
        
        cstatus = gdk_pixbuf_savev(pixbuf_area, trim(filename)//c_null_char, "png"//c_null_char,&
                  c_null_ptr, c_null_ptr, c_null_ptr)
        
        res = FALSE
    end function save_draw_area
    
    function radio_1_selected(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use global_widgets
        
        implicit none
        
        type(c_ptr), value :: widget, gdata
        integer(c_int) :: res
        
        call gtk_spin_button_set_value(spin_btn_scale, 45d0)

        res = FALSE
    end function radio_1_selected        
        
    function radio_2_selected(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use global_widgets
        
        implicit none
        
        type(c_ptr), value :: widget, gdata
        integer(c_int) :: res
        
        call gtk_spin_button_set_value(spin_btn_scale, 60d0)

        res = FALSE
    end function radio_2_selected        
        
        
end module handlers
