subroutine multi_vortex(positions,np,nt,nv,vs,dt,Dx,Dy,A,B,type)
	implicit none
	
	integer::i, p, v, nd, np, nv, nt, type
	double precision:: dt, Dx,Dy, A, B
	double precision::rnorm(np)
	double precision::positions(np+nv,np+nv), vorticies(nv,nv), vs(nv)
	double precision, dimension(2)::u
	double precision:: normrnd, forced, ideal, oseen
	double precision:: u_th, distx, disty, rel_rad
		
	nd = 2
	
	call rndstart()
	
	!save the vorticies so they all move simultaneously
	vorticies = positions((np+1):(np+nv),1:2)
	
	!call dblepr("vorticies(1,1)", 1, vorticies(1,1), 1)
	!call dblepr("vorticies(1,2)", 1, vorticies(1,2), 1)
	!call dblepr("vorticies(2,1)", 1, vorticies(2,1), 1)
	!call dblepr("vorticies(2,2)", 1, vorticies(2,2), 1)
	!call dblepr("vorticies(3,1)", 1, vorticies(3,1), 1)
	!call dblepr("vorticies(3,2)", 1, vorticies(3,2), 1)
		
	do i = 1,nt
		do p = 1,(np+nv)
			
				!add the effects of each vortex
				! get the current point 
				! find the distance and angle to each vortex 
				! add the effects
				
			u = 0
			
			do v=1,nv
				
				! distance from point to vortex
				distx = positions(p,1)-vorticies(v,1)
				disty = positions(p,2)-vorticies(v,2)
				
				!radius relative to current vortex
				rel_rad = sqrt(distx**2+disty**2)
				
				!get angular velocity magnitude
				select case (type)
					case(1)
				  		u_th = forced(A,B,rel_rad)
					case(2)
				  		u_th = ideal(A,B,rel_rad)
					case(3)
						u_th = oseen(A,B,rel_rad)
				end select
				

				!call dblepr("u_th", 4, u_th, 1)
				!call dblepr("rel_rad", 7, rel_rad, 1)
				!call dblepr("u", 1, u, 2)
				!call dblepr("dt", 2, dt, 1)
				
				! direction vector perpendicular to r_point - r_vortex
				!   (switch coordinates and negative x)
				! scale to unit length and then multiply by angular velocity
				if(rel_rad > .00001)then
					u(1) = u(1) + -disty / rel_rad * u_th * vs(v)
					u(2) = u(2) + distx / rel_rad * u_th * vs(v)
				end if
				
				
			end do	
			!if(p < 10)then
			!	call dblepr("u", 1, u, 2)
			!end if
			
				! X
			positions(p,1) = positions(p,1) + u(1)*dt/dble(nt) + &
				normrnd() * sqrt(2*Dx*dt/dble(nt))
		
				! Y
			positions(p,2) = positions(p,2) + u(2)*dt/dble(nt) + &
				normrnd() * sqrt(2*Dy*dt/dble(nt))
	
		end do
	end do
		
	
	call rndend()
	
end subroutine