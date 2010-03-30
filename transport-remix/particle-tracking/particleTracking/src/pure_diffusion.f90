subroutine pure_diffusion(positions,np,nt,dt,Dx,Dy)
	implicit none
	
	integer::r, i, j, nd, np, nv, nt
	double precision:: dt, Dx,Dy
	double precision::rnorm(np)
	double precision::positions(np,np)
	double precision, dimension(2)::D
	double precision:: normrnd
	
	nd = 2
	nv = 0 
	
	call rndstart()
	
	D(1) = Dx
	D(2) = Dy
	
	do i=1,nt
		do j=1,nd
			do r=1,np
				positions(r,j) = positions(r,j) + normrnd() * sqrt(2*D(j)*dt)
			end do
		end do
	end do
	
	call rndend()
	
end subroutine