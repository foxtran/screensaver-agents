module agent_mod
  use const
  implicit none
  private

  type :: agent_t
    real(FP) :: x, y
    real(FP) :: angle
    real(FP) :: speed
    integer :: R, G, B
  contains
    procedure :: init
    procedure :: validate
    procedure :: update
    procedure :: move
  end type agent_t

  public :: agent_t
contains
  subroutine init(agent, x, y, angle, speed, R, G, B)
    class(agent_t), intent(inout) :: agent
    real(FP),       intent(in) :: x, y, angle, speed
    integer,        intent(in) :: R, G, B
    agent%x = x
    agent%y = y
    agent%angle = angle
    agent%speed = speed
    agent%R = R
    agent%G = G
    agent%B = B
  end subroutine init
  subroutine validate(agent, maxdist)
    class(agent_t), intent(inout) :: agent
    real(FP),       intent(in) :: maxdist
    real(FP) :: norm
    if (agent%x**2 + agent%y**2 > maxdist**2) then
      agent%angle = agent%angle + PI
      if (agent%angle > PI) then
        agent%angle = agent%angle - 2.0_FP * PI
      end if
      norm = 1 / sqrt(agent%x**2 + agent%y**2)
      agent%x = agent%x * norm * (maxdist - 5._FP)
      agent%y = agent%y * norm * (maxdist - 5._FP)
      return
    end if
  end subroutine validate
  subroutine update(agent, agents)
    class(agent_t), intent(inout) :: agent
    type(agent_t),  intent(in) :: agents(:)
    !
    real(FP), parameter :: lookup_dist = 45._FP
    real(FP), parameter :: max_dangle = PI / 4._FP
    !
    real(FP) :: angle, diffangle, dangle, dth, r2, rang
    integer :: aid

    dangle = 0.0_FP
    do aid = 1, size(agents)
      if (agent%x == agents(aid)%x .and. agent%y == agents(aid)%y) cycle
      r2 = (agent%x - agents(aid)%x)**2 + (agent%y - agents(aid)%y)**2
      if (r2 > lookup_dist**2) cycle
      angle = atan2(agents(aid)%y - agent%y, agents(aid)%x - agent%x)
      diffangle = mod(abs(agent%angle - angle), 2._FP * PI)
      diffangle = min(diffangle, 2._FP * PI - diffangle)
      if (abs(diffangle) > PI / 8._FP) cycle
      dth = mod(agent%angle - angle, 2._FP * PI)
      dth = min(dth, 2._FP * PI - dth)
      if (dth > 0) then
        dangle = dangle - diffangle
      else
        dangle = dangle + diffangle
      end if
    end do
    if (dangle < -max_dangle) dangle = -max_dangle
    if (dangle >  max_dangle) dangle =  max_dangle
    agent%angle = agent%angle + dangle
    if (agent%angle >  PI) agent%angle = agent%angle - 2._FP * PI
    if (agent%angle < -PI) agent%angle = agent%angle + 2._FP * PI
  end subroutine update
  subroutine move(agent)
    class(agent_t), intent(inout) :: agent
    agent%x = agent%x + agent%speed * cos(agent%angle)
    agent%y = agent%y + agent%speed * sin(agent%angle)
    if (agent%x >  1920._FP / 2) agent%x = agent%x - 1920._FP
    if (agent%x < -1920._FP / 2) agent%x = agent%x + 1920._FP
    if (agent%y >  1080._FP / 2) agent%y = agent%y - 1080._FP
    if (agent%y < -1080._FP / 2) agent%y = agent%y + 1080._FP
  end subroutine move
end module agent_mod
