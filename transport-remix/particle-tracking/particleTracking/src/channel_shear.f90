subroutine channel_shear(positions,np,nt,dt,Dx,Dy,U0,b)
	implicit none
	
	integer::r, i, j, nd, np, nv, nt
	double precision:: dt, Dx,Dy, U0, b
	double precision::rnorm(np)
	double precision::positions(np,np)
	double precision, dimension(2)::D, U
	integer, dimension(2)::swap
	double precision:: normrnd
	
	nd = 2
	nv = 0 
	
	call rndstart()
	
	D(1) = Dx
	D(2) = Dy
	U(1) = U0
	U(2) = 0d0
	swap(1) = 2
	swap(2) = 1
	
	do i=1,nt
		do j=1,nd
			do r=1,np
				positions(r,j) = positions(r,j) + &
					(U(j)*(1-(positions(r,swap(j))/b)**2)-2d0/3d0*U(j))*dt/dble(nt) + &
					normrnd() * sqrt(2*D(j)*dt/dble(nt))
					
				!if(j == 2) then
				!	do while ( positions(r,j) > b .or. positions(r,j) < -b )
				!		positions(r,j) = positions(r,j) + &
				!			(U(j)*(1-(positions(r,swap(j))/b)**2)-2d0/3d0*U(j))*dt/dble(nt) + &
				!			normrnd() * sqrt(2*D(j)*dt/dble(nt))
				!	end do
				!end if
			end do
		end do
	end do
	
	call rndend()
	
end subroutine