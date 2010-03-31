subroutine single_vortex(positions,np,nt,dt,Dx,Dy,A,B,type)
	implicit none
	
	integer::r, i, j, nd, np, nv, nt, type
	double precision:: dt, Dx,Dy, A, B
	double precision::rnorm(np)
	double precision::positions(np,np)
	double precision, dimension(2)::D
	double precision:: normrnd, forced, ideal, oseen, vv
	double precision, external::vortex
	
	nd = 2
	nv = 0 
	
	call rndstart()
	
	D(1) = Dx
	D(2) = Dy
	
	do i=1,nt
		do r=1,np
			
				! Radius
			positions(r,1) = positions(r,1) + &
				normrnd() * sqrt(2*D(1)*dt/dble(nt))
				
			select case (type)
				case(1)
			  		vv = forced(A,B,positions(r,1))
				case(2)
			  		vv = ideal(A,B,positions(r,1))
				case(3)
					vv = oseen(A,B,positions(r,1))
			end select
			
				! Angle
			positions(r,2) = positions(r,2) + &
				vv*dt/dble(nt) + normrnd() * sqrt(2*D(2)*dt/dble(nt)) 
				
		end do
	end do
	
	call rndend()
	
end subroutine


function forced(scale,shape,radius)

	double precision, intent(inout):: scale, shape, radius
	double precision:: forced

	forced = scale*radius

end function forced

function ideal(scale,shape,radius)

	double precision, intent(inout):: scale, shape, radius
	double precision:: ideal

	ideal = scale/radius

end function ideal

function oseen(scale,shape,radius)

	double precision, intent(inout):: scale, shape, radius
	double precision:: oseen

	oseen = scale/radius*(1-exp(-radius**2/shape**2))

end function oseen