subroutine linear_shear(positions,np,nt,dt,Dx,Dy,Us)
	implicit none
	
	integer::r, i, j, nd, np, nv, nt
	double precision:: dt, Dx,Dy, Us
	double precision::rnorm(np)
	double precision::positions(np,np)
	double precision, dimension(2)::D,shear
	integer, dimension(2)::swap
	double precision:: normrnd
	
	nd = 2
	nv = 0 
	
	call rndstart()
	
	D(1) = Dx
	D(2) = Dy
	shear(1) = Us
	shear(2) = 0d0
	swap(1) = 2
	swap(2) = 1
	
	do i=1,nt
		do j=1,nd
			do r=1,np
				positions(r,j) = positions(r,j) + &
					(shear(j)*positions(r,swap(j)))*dt/dble(nt) + &
					normrnd() * sqrt(2*D(j)*dt/dble(nt))
			end do
		end do
	end do
	
	call rndend()
	
end subroutine