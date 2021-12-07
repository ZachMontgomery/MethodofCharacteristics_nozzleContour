program method_of_characteristics
use dislin
use alloc
use propulsions
implicit none
! container widgets id
integer :: id_main, id_left, id_right
! widget functions
external :: gamma_func, curvature_func, throatradius_func, thetamax_func
external :: thetaexit_func, machexit_func, nodes_func, step_func, run_func
! initialize propulsions module variables
w = 1000
h = 30
s = 5
n = 0
i = 0
j = 0
ready2run = 0
step = 0
firstrun = 1
flag = 0
g = 0.
rk = 0.
rt = 0.
tm = 0.
te = 0.
me = 0.
re = 0.
d_theta_exp = 0.
d_theta_turn = 0.
allocate(p(2,2,2))
! widgets placement and setup
call swgtit('Method of Characteristics')
call swgjus('center','label')
call swgwin(500,100,w+h,27*h)
call wgini('form',id_main)
	! left container
	call swgwin(0,0,w/4,11*h)
	call wgbas(id_main,'form',id_left)
		! gamma
		call swgwin(0,s,w/8,h-2*s)
		call wglab(id_left,'Gamma:',id_gamma_label)
		call swgwin(w/8,s,w/8,h-2*s)
		call swgopt('float','verify')
		call swgopt('change','callback')
		call wgtxt(id_left,'',id_gamma_text)
		call swgfoc(id_gamma_text)
		call swgcbk(id_gamma_text,gamma_func)
		! nodes
		call swgwin(0,h+s,w/8,h-2*s)
		call wglab(id_left,'# of nodes:',id_nodes_label)
		call swgwin(w/8,h+s,w/8,h-2*s)
		call swgopt('digits','verify')
		call swgopt('change','callback')
		call wgtxt(id_left,'',id_nodes_text)
		call swgcbk(id_nodes_text,nodes_func)
		! throat
		call swgwin(0,2*h+s,w/8,h-2*s)
		call wglab(id_left,'Throat radius:',id_throatradius_label)
		call swgwin(w/8,2*h+s,w/8,h-2*s)
		call swgopt('float','verify')
		call swgopt('change','callback')
		call wgtxt(id_left,'',id_throatradius_text)
		call swgcbk(id_throatradius_text,throatradius_func)
		! Curvature
		call swgwin(0,3*h+s,w/8,h-2*s)
		call wglab(id_left,'Curvature:',id_curvature_label)
		call swgwin(w/8,3*h+s,w/8,h-2*s)
		call swgopt('float','verify')
		call swgopt('change','callback')
		call wgtxt(id_left,'',id_curvature_text)
		call swgcbk(id_curvature_text,curvature_func)
		! Theta max
		call swgwin(0,4*h+s,w/8,h-2*s)
		call wglab(id_left,'Max Theta:',id_thetamax_label)
		call swgwin(w/8,4*h+s,w/8,h-2*s)
		call swgopt('float','verify')
		call swgopt('change','callback')
		call wgtxt(id_left,'',id_thetamax_text)
		call swgcbk(id_thetamax_text,thetamax_func)
		! Mach Exit
		call swgwin(0,5*h+s,w/8,h-2*s)
		call wglab(id_left,'Exit Mach:',id_machexit_label)
		call swgwin(w/8,5*h+s,w/8,h-2*s)
		call swgopt('float','verify')
		call swgopt('change','callback')
		call wgtxt(id_left,'',id_machexit_text)
		call swgcbk(id_machexit_text,machexit_func)
		! Theta Exit
		call swgwin(0,6*h+s,w/8,h-2*s)
		call wglab(id_left,'Exit Theta:',id_thetaexit_label)
		call swgwin(w/8,6*h+s,w/8,h-2*s)
		call swgopt('float','verify')
		call swgopt('change','callback')
		call wgtxt(id_left,'',id_thetaexit_text)
		call swgcbk(id_thetaexit_text,thetaexit_func)
		! Single Step
		call swgwin(w/16,7*h+s,w/8,h-2*s)
		call wgbut(id_left,'Single Step',0,id_step_button)
		call swgcbk(id_step_button,step_func)
		! Run
		call swgwin(w/16,8*h+s,w/8,h-2*s)
		call wgpbut(id_left,'Run',id_run_pushbutton)
		call swgcbk(id_run_pushbutton,run_func)
		call swgatt(id_run_pushbutton,'inactive','status')
		! Quit
		call swgwin(w/16,9*h+s,w/8,h-2*s)
		call wgquit(id_left,id_quit_pushbutton)
		! Status
		call swgwin(s,10*h+s,w/4,h-2*s)
		call wglab(id_left,'Line Progress',id_statusline_label)
		call swgwin(s,11*h+s,w/4-2*s,h-2*s)
		call swgopt('smooth','pbar')
		call swgopt('label','pbar')
		call swgclr(0.,0.,1.,'pbar')
		call wgpbar(id_left,0.,1.,.01,id_statusline_pbar)
		call swgval(id_statusline_pbar,0.)
		
		call swgwin(s,12*h+s,w/4,h-2*s)
		call wglab(id_left,'Point Progress',id_statuspoint_label)
		call swgwin(s,13*h+s,w/4-2*s,h-2*s)
		call swgopt('smooth','pbar')
		call swgopt('label','pbar')
		call swgclr(0.,0.,1.,'pbar')
		call wgpbar(id_left,0.,1.,.01,id_statuspoint_pbar)
		call swgval(id_statuspoint_pbar,0.)
		
		call swgwin(s,14*h+s,w/4,h-2*s)
		call wglab(id_left,'Total Progress',id_statustotal_label)
		call swgwin(s,15*h+s,w/4-2*s,h-2*s)
		call swgopt('smooth','pbar')
		call swgopt('label','pbar')
		call swgclr(0.,0.,1.,'pbar')
		call wgpbar(id_left,0.,1.,.01,id_statustotal_pbar)
		call swgval(id_statustotal_pbar,0.)
	! right container
	call swgwin(w/4,0,3*w/4,25*h)
	call wgbas(id_main,'form',id_right)
		! Graphics
		call swgwin(s,s,3*w/4-2*s,25*h-2*s)
		!call swgpos(s,s)
		!call swgdrw(1.)
		call wgdraw(id_right,id_graphics)
		!call swgdrw(5.)
call wgfin
call disfin
end program method_of_characteristics
!***************************************************************************
!***************************************************************************
!***************************************************************************
!***************************************************************************
subroutine gamma_func
use propulsions
call gwgflt(id_gamma_text,g)
if (g > 1.) then
	ready2run(1) = 1
else
	ready2run(1) = 0
end if
call check
end subroutine gamma_func
!***************************************************************************
subroutine curvature_func
use propulsions
call gwgflt(id_curvature_text,rk)
if (rk >= 0.) then
	ready2run(2) = 1
else
	ready2run(2) = 0
end if
call check
end subroutine curvature_func
!***************************************************************************
subroutine throatradius_func
use propulsions
call gwgflt(id_throatradius_text,rt)
if (rt > 0.) then
	ready2run(3) = 1
else
	ready2run(3) = 0
end if
call check
end subroutine throatradius_func
!***************************************************************************
subroutine thetamax_func
use propulsions
if (flag == 0) then
	flag = 1
	call gwgflt(id_thetamax_text,tm)
	if (tm > 2.) then
		ready2run(5) = 1
		ready2run(6) = 0
		if (ready2run(1) == 1) then
			me = prandtl_meyer(g,degree2rad(tm)*2.,1.1,.000001,500)
			call swgflt(id_machexit_text,me,3)
		end if
	else
		ready2run(5) = 0
		ready2run(6) = 0
	end if
	call check
	flag = 0
end if
end subroutine thetamax_func
!***************************************************************************
subroutine thetaexit_func
use propulsions
call gwgflt(id_thetaexit_text,te)
if (te >= 0.) then
	ready2run(4) = 1
else
	ready2run(4) = 0
end if
call check
end subroutine thetaexit_func
!***************************************************************************
subroutine machexit_func
use propulsions
if (flag == 0) then
	flag = 1
	call gwgflt(id_machexit_text,me)
	if (me > 1.) then
		ready2run(6) = 1
		ready2run(5) = 0
		if (ready2run(1) == 1) then
			tm = rad2degree(nu(g, me) / 2.)
			call swgflt(id_thetamax_text,tm,3)
		end if
	else
		ready2run(6) = 0
		ready2run(5) = 0
	end if
	call check
	flag = 0
end if
end subroutine machexit_func
!***************************************************************************
subroutine nodes_func
use propulsions
use alloc
call gwgint(id_nodes_text,n)
if(n > 0) then
	deallocate(p)
	call set(p,n)
	ready2run(7) = 1
else
	ready2run(7) = 0
end if
call check
end subroutine nodes_func
!***************************************************************************
subroutine step_func
use propulsions
if (step == 0) then
	step = 1
else
	step = 0
end if
end subroutine step_func
!***************************************************************************
subroutine run_func
use propulsions
real :: a
integer :: b, c, l
! this if statement will execute the first time the run_func is called to
! initialize additional variables and lock-in the setup variables and setup
! the graphics
if (firstrun == 1) then
	! calculate delta thetas for expansion section and turning section
	d_theta_exp = tm / real(n)
	d_theta_turn = (tm - te) / real(n)
	! set first point mach, theta, x, and y
	p(0,1,1) = 1.
	p(0,1,4) = rt
	i = 0
	j = 2
	! lock variables
	call swgatt(id_gamma_text,'inactive','status')
	call swgatt(id_nodes_text,'inactive','status')
	call swgatt(id_throatradius_text,'inactive','status')
	call swgatt(id_curvature_text,'inactive','status')
	call swgatt(id_thetaexit_text,'inactive','status')
	call swgatt(id_thetamax_text,'inactive','status')
	call swgatt(id_machexit_text,'inactive','status')
	call swgatt(id_step_button,'inactive','status')
	! send dislin graphics to widget window
	call setxid(id_graphics,'widget')
	! setup dislin graphics
	call metafl('XWIN')
	call scrmod('REVERSE')
	call page(2970,3000)
	call disini
	l = 2560
	call winfnt('times new roman bold')
	call messag('Nozzle Half Contour',l/2,2*h)
	call axslen(l,l/4)
	call axspos(l/8,l/16+l/4)
	call name('Y','y')
	call name('X','x')
	call labtyp('vert','x')
	re = (rt/me*(2./(g+1.)*(1.+(g-1.)/2.*me**2))**((g+1.)/2./(g-1.)))
	call graf(0.,real(ceiling(re))*4.,0.,.5,0.,real(ceiling(re)),0.,.5)
	! turn off flag for first run so initialization doesn't happen again
	firstrun = 0
end if
if (step == 0) then
	! calculate all the way through to the end
	c = j
	if (i == 0) then
		do j = c, n + 1
			p(i, j, 1) = expansion_fan(p(i,j-1,1),d_theta_exp,g)
			p(i, j, 2) = real(j-1) * d_theta_exp
			p(i, j, 3) = sin(degree2rad(real(j-1)*d_theta_exp))*rk
			p(i, j, 4) = (1.-cos(degree2rad(real(j-1)*d_theta_exp)))*rk+rt
			! dislin stuff
			call curve((/p(i,j-1,3),p(i,j,3)/),(/p(i,j-1,4),p(i,j,4)/),2)
			call sendbf
			call progress
		end do
		p(i,n+2,:) = p(i,n+1,:)
		i = 1
		j = 1
	end if
	b = i
	c = j
	do i = b, n
		do j = c, 2 + n - i
			if(j == 1) then
				p(i,j,:) = cl(p(i-1,2,:),g)
				! dislin stuff
				call color('red')
				call curve((/p(i-1,2,3),p(i,1,3)/),(/p(i-1,2,4),p(i,1,4)/),&
							2)
				call sendbf
			elseif(j == 2 + n - i) then
				a = tm-d_theta_turn * real(i)
				p(i,j,:) = wall(p(i-1,3+n-i,:),p(i,j-1,:),g,a)
				! dislin stuff
				call color('fore')
				call curve((/p(i-1,3+n-i,3),p(i,j,3)/),(/p(i-1,3+n-i,4),&
							p(i,j,4)/),2)
				call color('green')
				call curve((/p(i,j,3),p(i,j-1,3)/),(/p(i,j,4),p(i,j-1,4)/),&
							2)
				call sendbf
			else
				p(i,j,:) = internal_flow(p(i-1,j+1,:),p(i,j-1,:),g)
				! dislin stuff
				call color('blue')
				call curve((/p(i-1,j+1,3),p(i,j,3),p(i,j-1,3)/),(/p(i-1,j+1&
							,4),p(i,j,4),p(i,j-1,4)/),3)
				call sendbf
			end if
			call progress
		end do
	end do
else
	! calculate one step
	if (i == 0 .and. j <= n + 1) then
		p(i, j, 1) = expansion_fan(p(i,j-1,1),d_theta_exp,g)
		p(i, j, 2) = real(j-1) * d_theta_exp
		p(i, j, 3) = sin(degree2rad(real(j-1)*d_theta_exp))*rk
		p(i, j, 4) = (1.-cos(degree2rad(real(j-1)*d_theta_exp)))*rk+rt
		! dislin stuff
		call curve((/p(i,j-1,3),p(i,j,3)/),(/p(i,j-1,4),p(i,j,4)/),2)
		call sendbf
		if (j == n + 1) then
			p(i,n+2,:) = p(i,n+1,:)
			j = j + 1
		end if
	else
		if(j == 1) then
			p(i,j,:) = cl(p(i-1,2,:),g)
			! dislin stuff
			call color('red')
			call curve((/p(i-1,2,3),p(i,1,3)/),(/p(i-1,2,4),p(i,1,4)/),&
						2)
			call sendbf
		elseif(j == 2 + n - i) then
			a = tm-d_theta_turn * real(i)
			p(i,j,:) = wall(p(i-1,3+n-i,:),p(i,j-1,:),g,a)
			! dislin stuff
			call color('fore')
			call curve((/p(i-1,3+n-i,3),p(i,j,3)/),(/p(i-1,3+n-i,4),&
						p(i,j,4)/),2)
			call color('green')
			call curve((/p(i,j,3),p(i,j-1,3)/),(/p(i,j,4),p(i,j-1,4)/),&
						2)
			call sendbf
		else
			p(i,j,:) = internal_flow(p(i-1,j+1,:),p(i,j-1,:),g)
			! dislin stuff
			call color('blue')
			call curve((/p(i-1,j+1,3),p(i,j,3),p(i,j-1,3)/),(/p(i-1,j+1&
						,4),p(i,j,4),p(i,j-1,4)/),3)
			call sendbf
		end if
	end if
	call progress
	if (j == 2 + n - i) then
		j = 0
		i = i + 1
	end if
	j = j + 1
end if
end subroutine run_func
!***************************************************************************
subroutine check
use propulsions
integer :: k, total
total = 0
do k = 1, 7
	total = total + ready2run(k)
end do
if (total == 6) then
	call swgatt(id_run_pushbutton,'active','status')
else
	call swgatt(id_run_pushbutton,'inactive','status')
end if
end subroutine check
!***************************************************************************
subroutine progress
use propulsions
real :: line, point, total
integer :: k, count1, count2
line = real(i) / real(n)
point = real(j) / (2. + real(n) - real(i))
count1 = 0
do k = 0, n
	count1 = count1 + 2 + n - k
end do
count2 = 0
do k = 0, i-1
	count2 = count2 + 2 + n - k
end do
total = real(count2 + j) / real(count1)
if (total == 1.) then
	call swgatt(id_run_pushbutton,'inactive','status')
	i = 0
	j = 0
	ready2run = 0
	call mach_plots
end if
call swgval(id_statusline_pbar,line)
call swgval(id_statuspoint_pbar,point)
call swgval(id_statustotal_pbar,total)
end subroutine progress
!***************************************************************************
subroutine mach_plots
use propulsions
real :: nozzle_length
real, dimension(0:n+1,2) :: centerline
real, dimension(2+n*2,2) :: wall_mach, a_astar
integer :: l,a,b,c
call endgrf
l = 2560
nozzle_length = p(n,2,3)
call color('red')
call messag('Center-Line',l/4,l+l/16)
call color('blue')
call messag('Wall',l/2+200,l+l/16)
call color('fore')
call messag('2D A/Astar',3*l/4+150,l+l/16)
call messag('Nozzle length is',l/2,l+l/8)
call number(nozzle_length,3,3*l/4-100,l+l/8)
call axslen(l, l/2)
call axspos(l/8, l-l/16)
call name('Mach','y')
call name('X','x')
call labtyp('vert','x')
call graf(0.,real(ceiling(nozzle_length)),0.,.5,1.,real(ceiling(me))+.5&
			,1.,.5)
centerline(0,1) = 0.
centerline(0,2) = 1.
do a = 1, n
	centerline(a,1) = p(a,1,3)
	centerline(a,2) = p(a,1,1)
end do
centerline(n+1,1) = nozzle_length
centerline(n+1,2) = me
do a = 1, n+2
	wall_mach(a,1) = p(0,a,3)
	wall_mach(a,2) = p(0,a,1)
	a_astar(a,2) = exp_ratio2mach(g,rt,p(0,a,4),2.,.00001,900)
end do
a_astar(1,2) = 1.
do a = 1, n
	wall_mach(a+n+2,1) = p(a,2+n-a,3)
	wall_mach(a+n+2,2) = p(a,2+n-a,1)
	a_astar(a+n+2,2) = exp_ratio2mach(g,rt,p(a,2+n-a,4),2.,.00001,900)
end do
a_astar(:,1) = wall_mach(:,1)
call color('red')
call curve(centerline(:,1),centerline(:,2),n+2)
call color('blue')
call curve(wall_mach(:,1),wall_mach(:,2),2+n*2)
call color('fore')
call curve(a_astar(:,1),a_astar(:,2),2+n*2)
call sendbf
end subroutine mach_plots
!***************************************************************************
