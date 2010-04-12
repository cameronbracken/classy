subroutine channel_shear(positions,np,dt,Dx,Dy,U0,b)
	implicit none
	
	integer::p, i, j, nd, np, nv, nt
	double precision:: dt, Dx,Dy, U0, b
	double precision::rnorm(np)
	double precision::positions(np,np)
	double precision:: normrnd,unifrnd
	
	nd = 2
	nv = 0 
	
	call rndstart()
	
	do p=1,np
		
		positions(p,1) = positions(p,1) + &
			(U0*(1-(positions(p,2)/b)**2)-2d0/3d0*U0)*dt + normrnd() * sqrt(2*Dx*dt)
			
		positions(p,2) = positions(p,2) + normrnd() * sqrt(2*Dy*dt)
			
		!if(j == 2 .and. (positions(r,j) > b .or. positions(r,j) < -b))then
		!	positions(r,j) = -b + unifrnd()*2d0*b
		!end if
		
		!call dblepr("unif", 4, unifrnd(), 1)
				
		!if(j == 2) then
		!	do while ( positions(r,j) > b .or. positions(r,j) < -b )
		!		positions(r,j) = positions(r,j) + &
		!			(U(j)*(1-(positions(r,swap(j))/b)**2)-2d0/3d0*U(j))*dt/dble(nt) + &
		!			normrnd() * sqrt(2*D(j)*dt/dble(nt))
		!	end do
		!end if
	end do
	
	call rndend()
	
end subroutine