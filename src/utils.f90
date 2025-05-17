module utils
  implicit none
  private
  public :: string2int, random_int
contains
  function string2int(str) result(val)
    character(len=*) :: str
    integer :: val
    read(str, *) val
  end function string2int
  function random_int(imin, imax) result(val)
    integer, intent(in) :: imin, imax
    real :: r
    integer :: val
    call random_number(r)
    r = r * (imax + 1 - imin)
    val = int(r) + imin
  end function random_int
end module utils
