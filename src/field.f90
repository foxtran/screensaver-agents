module field_mod
  use const
  use agent_mod
  implicit none
  private

  type :: field_t
    integer :: height
    integer :: width
    real(FP) :: maxdist
    integer(4), allocatable :: field(:,:)
    type(agent_t), allocatable :: agents(:)
  contains
    procedure :: init
    procedure :: get_image
    procedure :: update
    procedure :: redraw
  end type field_t

  public :: field_t
contains
  subroutine init(field, height, width, n_agents)
    use utils, only: random_int
    class(field_t), intent(inout) :: field
    integer, intent(in) :: height, width, n_agents
    integer :: aid
    !n_agents = 2
    field%height = height
    field%width = width
    field%maxdist = max(field%width, field%height) / 2.0_FP
    if (allocated(field%field)) deallocate(field%field)
    if (allocated(field%agents)) deallocate(field%agents)
    allocate(field%field(width, height), source=0)
    allocate(field%agents(n_agents))
    do aid = 1, n_agents
      block
        real(FP) :: x, y, angle, speed, r(3)
        integer :: RGB(3)
        call random_number(r)
        x = (r(1) - 0.5_FP) * field%maxdist
        y = (r(2) - 0.5_FP) * field%maxdist
        angle = (r(3) * 2._FP - 1._FP) * PI
        speed = 3.0_FP
        RGB = 0
        do while (sum(RGB) < 384)
          RGB(1) = random_int(0, 255)
          RGB(2) = random_int(0, 255)
          RGB(3) = random_int(0, 255)
        end do
        call field%agents(aid)%init(x, y, angle, speed, RGB(1), RGB(2), RGB(3))
      end block
    end do
  end subroutine
  function get_image(field) result(image_ptr)
    class(field_t), intent(in), target :: field
    integer(4), pointer :: image_ptr(:)
    image_ptr(1:size(field%field)) => field%field
  end function get_image
  subroutine update(field)
    class(field_t), intent(inout) :: field
    integer :: aid
    !!$OMP parallel do default(none) &
    !!$OMP private(aid) &
    !!$OMP shared(field)
    !do aid = 1, size(field%agents)
    !  call field%agents(aid)%validate(field%maxdist)
    !end do
    !$OMP parallel do default(none) &
    !$OMP private(aid) &
    !$OMP shared(field)
    do aid = 1, size(field%agents)
      call field%agents(aid)%update(field%agents)
    end do
    !$OMP parallel do default(none) &
    !$OMP private(aid) &
    !$OMP shared(field)
    do aid = 1, size(field%agents)
      call field%agents(aid)%move()
    end do
  end subroutine update
  subroutine redraw(field)
    class(field_t), intent(inout) :: field
    integer :: mw, mh, w, h, aid, ph, pw
    integer, parameter :: d = 4
    mw = size(field%field, dim=1)
    mh = size(field%field, dim=2)
    !$OMP parallel do default(none) &
    !$OMP private(h, w) &
    !$OMP shared(field, mh, mw) &
    !$OMP collapse(2)
    do h = 1, mh
      do w = 1, mw
        if (field%field(w, h) == 0) cycle
        block
          integer :: R, G, B
          R = ISHFT(IAND(field%field(w, h), ISHFT(255, 16)), -16)
          G = ISHFT(IAND(field%field(w, h), ISHFT(255, 8)), -8)
          B = ISHFT(IAND(field%field(w, h), ISHFT(255, 0)), 0)
          R = max(0, R - 5)
          G = max(0, G - 5)
          B = max(0, B - 5)
          field%field(w, h) = ISHFT(IAND(R, 255), 16) + ISHFT(IAND(G, 255), 8) + ISHFT(IAND(B, 255), 0)
        end block
      end do
    end do
    !$OMP parallel do default(none) &
    !$OMP private(aid, pw, ph, h, w) &
    !$OMP shared(field, mw, mh)
    do aid = 1, size(field%agents)
      pw = mw / 2 + int(field%agents(aid)%x)
      ph = mh / 2 + int(field%agents(aid)%y)
      if (pw < 1 - d .or. pw > mw + d) cycle
      if (ph < 1 - d .or. ph > mh + d) cycle
      do h = max(1, ph - d / 2), min(mh, ph + d / 2)
        do w = max(1, pw - d / 2), min(mw, pw + d / 2)
          if ((h - ph)**2 + (w - pw)**2 <= (d/2)**2) then
            field%field(w, h) = ISHFT(IAND(field%agents(aid)%R, 255), 16) + ISHFT(IAND(field%agents(aid)%G, 255), 8) + ISHFT(IAND(field%agents(aid)%B, 255), 0)
          end if
        end do
      end do
    end do
  end subroutine redraw
end module field_mod
