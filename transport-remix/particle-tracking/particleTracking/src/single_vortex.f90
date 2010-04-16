subroutine single_vortex(positions,np,nt,dt,Dx,Dy,A,B,type)
	implicit none
	
	integer::p, i, j, nd, np, nv, nt, type
	double precision:: dt, Dx,Dy, A, B
	double precision::rnorm(np)
	double precision::positions(np,np)
	double precision, dimension(2)::D
	double precision:: normrnd, forced, ideal, oseen, vv, R, theta, pi
	
	pi = acos(0d0)*2d0
	nd = 2
	nv = 0 
	
	call rndstart()
	
	D(1) = Dx
	D(2) = Dy
	
	do i=1,nt
		do p=1,np
			
			!R = sqrt(positions(p,1)**2+positions(p,2)**2)
			R = positions(p,1)
			!theta = atan2(positions(p,2),positions(p,1))
			
			select case (type)
				case(1)
			  		vv = forced(A,B,R)
				case(2)
			  		vv = ideal(A,B,R)
				case(3)
					vv = oseen(A,B,R)
			end select
			
				! X
			positions(p,1) = positions(p,1) + &
				normrnd() * sqrt(2*D(1)*dt/dble(nt))
			
				! Y
			positions(p,2) = positions(p,2) + vv*dt/(2*pi*R) + &
				normrnd() * sqrt(2*D(2)*dt/dble(nt)) 
			
			!call dblepr("X", 1, positions(p,1), 1)
			!call dblepr("Y", 1, positions(p,2), 1)	
		end do
	end do
	
	
	
	call rndend()
	
end subroutine


function forced(scale,shape,radius)

	double precision, intent(in):: scale, shape, radius
	double precision:: forced

	forced = scale*radius

end function forced

function ideal(scale,shape,radius)

	double precision, intent(in):: scale, shape, radius
	double precision:: ideal

	ideal = scale/radius

end function ideal

function oseen(scale,shape,radius)

	double precision, intent(in):: scale, shape, radius
	double precision:: oseen

	oseen = scale/radius*(1-exp(-radius**2/shape**2))

end function oseen