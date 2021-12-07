module propulsions
implicit none
!***************************************************************************
! variables related to the method of characteristics
integer, save :: n, i, j
real, save :: g, rk, rt, tm, te, me, d_theta_exp, d_theta_turn, re
real, save, allocatable, dimension(:,:,:) :: p
! p( line number = i , point number along line = j , specific value = k )
! =======i=========
! line number of 0 is for the wall expansion section, then each line up is
! the next 'parallel-ish' line down the nozzle, there is a total of n lines
! other than the wall expansion section
! =======j=========
! the point numbers start with 1 at the center line and run up all the way
! to the wall at the turning section. the number of points on a line is
! determined by    2 + n - i    this works for all lines 0 through n
! =======k=========
! k = 1 is mach
! k = 2 is theta
! k = 3 is x position aft of sonic line
! k = 4 is y position above the center line
! variable for widget logic
integer, save, dimension(7) :: ready2run
integer, save :: step, firstrun, flag
! viewable widgets id
integer, save :: id_gamma_label, id_gamma_text, id_curvature_label
integer, save :: id_curvature_text, id_thetamax_label, id_thetamax_text
integer, save :: id_thetaexit_label, id_thetaexit_text, id_machexit_label
integer, save :: id_machexit_text, id_nodes_label, id_nodes_text
integer, save :: id_graphics, id_run_pushbutton, id_quit_pushbutton
integer, save :: id_throatradius_label, id_throatradius_text, id_window
integer, save :: id_statusline_label, id_statuspoint_label
integer, save :: id_statusline_pbar, id_statuspoint_pbar
integer, save :: id_step_button, id_statustotal_label, id_statustotal_pbar
! variables for graphics
integer, save :: w,h,s
!***************************************************************************
contains
!***************************************************************************
function rad2degree(rad)
real, intent(in) :: rad
real, parameter :: pi = 3.141592653589793
real :: rad2degree
rad2degree = 180. / pi * rad
end function rad2degree
!***************************************************************************
function degree2rad(deg)
real, intent(in) :: deg
real, parameter :: pi = 3.141592653589793
real :: degree2rad
degree2rad = pi / 180. * deg
end function degree2rad
!***************************************************************************
function nu(gamma, mach)
! returns nu in radians
real, intent(in) :: gamma, mach
real :: nu, a, b
a = (gamma - 1.) / (gamma + 1.)
b = mach ** 2 - 1.
nu = sqrt(1. / a) * atan(sqrt(a * b)) - atan(sqrt(b))
end function nu
!***************************************************************************
function prandtl_meyer(gamma, nue, guess, error, max_iterations)
! nue is in radians
! guess should be 1.1 for supersonic and .9 for subsonic
! error should be a decimal percentage from 0. to 1.
! max_iterations is the total number of cycles that newtons method should
!     run to try to converge on an answer
real, intent(in) :: gamma, nue, guess, error
real :: prandtl_meyer, a, b, ea, f, df, xo, xn
integer, intent(in) :: max_iterations
integer :: i
a = (gamma + 1.) / (gamma - 1.)
b = (gamma - 1.) / (gamma + 1.)
i = 0
xo = guess
ea = 1.
do while(ea > error)
	if (i >= max_iterations) then
		write(*,*)
		write(*,*) 'Newtons Method inside of the Pradtl-Meyer Function'
		write(*,*) 'Failed to Converge within ', i, ' iterations.'
		write(*,*) 'Approximate Error was ', ea
		write(*,*)
		read(*,*)
		exit
	end if
	f = sqrt(a)*atan(sqrt(b*(xo**2-1.)))-atan(sqrt(xo**2-1.))-nue
	df = sqrt(a)*b*xo/sqrt(b*(xo**2-1.))/(b*(xo**2-1.)+1.)-1./xo/&
		sqrt(xo**2-1.)
	xn = xo - f/df
	ea = abs(xn - xo) / xo
	xo = xn
	i = i + 1
end do
prandtl_meyer = xn
end function prandtl_meyer
!***************************************************************************
function internal_flow(p1,p2, g)
! p1 is point 1 and p2 is point 2 and internal_flow(~) will return point 3
! p1 and p2 are arrays with 4 values: mach, theta, x, and y, in that order
! theta values should be in degrees, and will be returned in degrees
real, intent(in), dimension(4) :: p1, p2
real, intent(in) :: g
real, dimension(4) :: internal_flow
real :: m1, m2, m3, theta1, theta2, theta3, x1, x2, x3, y1, y2, y3
real :: nu1, nu2, nu3, mu1, mu2, mu3, K1minus, K2plus, K1m_, K2p_
real :: a, b
! initialize variables
! y1 should be greater than y2 for the method of characeristics to run
! downstream. The if statement ensures this.
if(p1(4) > p2(4)) then
	m1 = p1(1)
	m2 = p2(1)
	theta1 = degree2rad(p1(2))
	theta2 = degree2rad(p2(2))
	x1 = p1(3)
	x2 = p2(3)
	y1 = p1(4)
	y2 = p2(4)
else
	m1 = p2(1)
	m2 = p1(1)
	theta1 = degree2rad(p2(2))
	theta2 = degree2rad(p1(2))
	x1 = p2(3)
	x2 = p1(3)
	y1 = p2(4)
	y2 = p1(4)
end if
! Method of Characteristics
nu1 = nu(g,m1)
mu1 = asin(1./m1)
K1minus = theta1 + nu1
nu2 = nu(g,m2)
mu2 = asin(1./m2)
K2plus = theta2 - nu2
theta3 = (K1minus + K2plus) / 2.
nu3 = (K1minus - K2plus) / 2.
m3 = prandtl_meyer(g, nu3, 1.1, .000001, 500)
mu3 = asin(1./m3)
K1m_ = (theta1 - mu1 + theta3 - mu3) / 2.
K2p_ = (theta2 + mu2 + theta3 + mu3) / 2.
a = x1*tan(K1m_)-x2*tan(K2p_)-y1+y2
b = tan(K1m_)-tan(K2p_)
x3 = a / b
a = tan(K1m_)*tan(K2p_)*(x1-x2)-tan(K2p_)*y1+tan(K1m_)*y2
y3 = a / b
! return variables
internal_flow(1) = m3
internal_flow(2) = rad2degree(theta3)
internal_flow(3) = x3
internal_flow(4) = y3
end function internal_flow
!***************************************************************************
function cl(p, g)
! p is an array with 4 values: mach, theta, x, and y, in that order
! theta values should be in degrees, and will be returned in degrees
! cl(~) will return the centerline array in the same form as p
real, intent(in), dimension(4) :: p
real, intent(in) :: g
real, dimension(4) :: cl
real :: m1, m2, theta1, x1, x2, y1
real :: nu1, nu2, mu1, mu2, Km_
! initialize variables
m1 = p(1)
theta1 = degree2rad(p(2))
x1 = p(3)
y1 = p(4)
! Method of Characteristics
nu1 = nu(g,m1)
mu1 = asin(1./m1)
nu2 = theta1 + nu1
m2 = prandtl_meyer(g, nu2, 1.1, .000001, 500)
mu2 = asin(1./m2)
Km_ = (theta1 - mu1 - mu2) / 2.
x2 = x1 - y1/tan(Km_)
! return variables
cl(1) = m2
cl(2) = 0.
cl(3) = x2
cl(4) = 0.
end function cl
!***************************************************************************
function wall(p1, p2, g, angle)
! p1 is the previous wall point and p2 is the internal flow point that will
! be extended out to the next wall point which is what wall(~) will return
! p1 and p2 are arrays with 4 values: mach, theta, x, and y, in that order
! theta values should be in degrees, and will be returned in degrees
real, intent(in), dimension(4) :: p1, p2
real, intent(in) :: g, angle
real, dimension(4) :: wall
real :: m2, m3, theta2, theta3, x1, x2, x3, y1, y2, y3
real :: nu1, nu2, nu3, mu1, mu2, mu3, Kp_
real :: a, b
! initialize variables
! y1 should be greater than y2 for the method of characeristics to run
! downstream. The if statement ensures this.
if(p1(4) > p2(4)) then
	m2 = p2(1)
	theta2 = degree2rad(p2(2))
	x1 = p1(3)
	x2 = p2(3)
	y1 = p1(4)
	y2 = p2(4)
else
	m2 = p1(1)
	theta2 = degree2rad(p1(2))
	x1 = p2(3)
	x2 = p1(3)
	y1 = p2(4)
	y2 = p1(4)
end if
theta3 = degree2rad(angle)
! Method of Characteristics
nu2 = nu(g,m2)
mu2 = asin(1./m2)
nu3 = theta3 - theta2 + nu2
m3 = prandtl_meyer(g, nu3, 1.1, .000001, 500)
mu3 = asin(1./m3)
Kp_ = (theta2 + mu2 + theta3 + mu3) / 2.
a = x1 * tan(theta3) - x2 * tan(Kp_) + y2 - y1
b = tan(theta3) - tan(Kp_)
x3 = a / b
a = tan(theta3) * tan(Kp_) * (x1 - x2) + tan(theta3) * y2 - tan(Kp_) * y1
y3 = a / b
! return variables
wall(1) = m3
wall(2) = rad2degree(theta3)
wall(3) = x3
wall(4) = y3
end function wall
!***************************************************************************
function expansion_fan(m1,theta, g)
! theta should be in degrees, this function returns exit mach after the
! expansion fan, m1 is the entry mach into the expansion fan, g is gamma
real, intent(in) :: m1, theta, g
real :: expansion_fan, a, b
a = degree2rad(theta)
b = nu(g,m1)
expansion_fan = prandtl_meyer(g,a+b,1.1,.000001,500)
end function expansion_fan
!***************************************************************************
function exp_ratio2mach(g, rt, r, guess, error, max_iterations)
! returns mach given radius at the throat, gamma and radius at desired point
real, intent(in) :: g, rt, r, guess, error
integer, intent(in) :: max_iterations
real :: f, df, exp_ratio2mach, a, b, c, d, xo, xn, ea
integer :: counter
ea = 1.
counter = 0
xo = guess
a = r / rt
b = (g+1.)/2./(g-1.)
c = (1. - 3. * g) / (2. - 2. * g)
do while(ea > error)
	if(counter >= max_iterations) then
		write(*,*)
		write(*,*) 'Newtons Method inside of the exp_ratio2mach Function'
		write(*,*) 'Failed to Converge within ',counter,' iterations.'
		write(*,*) 'Approximate error was ',ea
		write(*,*)
		read(*,*)
	end if
	d = xo**2
	f = (2./(g+1.)*(1.+(g-1.)/2.*d))**b/xo-a
	df = 2.**c*(d-1.)/d/(2.+d*(g-1.))*((1.+(g-1.)/2.*d)/(g+1.))**b
	xn = xo - f / df
	ea = abs(xn - xo) / xo
	xo = xn
	counter = counter + 1
end do
exp_ratio2mach = xn
end function
!***************************************************************************
end module propulsions
