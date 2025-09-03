module global_widgets

    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int
    
    integer(c_int) :: nch, rowstride, pixwidth, pixheight 
    character(c_char), dimension(:), pointer :: pixel
    
    type(c_ptr) :: draw_area
    type(c_ptr) :: pixbuf_area
    type(c_ptr) :: spin_btn_iter, spin_btn_scale
    type(c_ptr) :: radio_1, radio_2
    type(c_ptr) :: status_bar
    type(c_ptr) :: window
        
end module global_widgets
