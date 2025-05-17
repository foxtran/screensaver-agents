program screensaver
  use, intrinsic :: iso_c_binding, only: c_int
  use const
  use posix
  use field_mod
  implicit none
  integer :: width
  integer :: height
  integer(c_int) :: err
  integer :: lu
  integer(4), pointer :: image_ptr(:)
  type(field_t) :: field
  height = 1080
  width = 1920
  print '(A,I5)', "H: ", height
  print '(A,I5)', "W: ", width
  open(newunit=lu, file='/dev/fb0', access='stream')
  call field%init(height, width, 1200)
  image_ptr => field%get_image()
  ! main loop
  do
    call field%update()
    call field%redraw()
    call display_fb(lu, image_ptr)
  end do
  close(lu)
contains
  subroutine display_fb(lu, image)
    use, intrinsic :: iso_c_binding, only: c_int32_t
    integer(4), intent(in) :: image(:)
    integer :: lu
    write(lu, pos=1) image
  end subroutine display_fb
end program screensaver
