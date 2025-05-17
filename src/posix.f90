module posix
  use, intrinsic :: iso_c_binding, only: c_int32_t, c_int
  implicit none
  private

  public :: usleep

  interface
    function usleep(usec) bind(c, name='usleep')
      import :: c_int, c_int32_t
      implicit none
      integer(kind=c_int32_t), value :: usec
      integer(kind=c_int)            :: usleep
    end function usleep
  end interface
end module posix
