program simple_advection

    implicit none
    double precision:: g, f, dx, dt, Uave, ich, bumph, cfl
    double precision:: output_time, output_interval, domain_length
    integer::idim, i, itmax, t, j, ictype
    double precision,dimension(:),allocatable:: h, ha, hb
    
    
    open(11,file="h.out")
    
        ! parameters
    g = 9.81d0                     ! m/s^2
    f = 2d0 * 7.2921d-5              ! * sin(0) rad/s
    idim = 100
    domain_length = 1000000d0        ! m
    dx = domain_length/dble(idim)  ! m (100 grid points will be 10000 m)
    Uave = 10d0                    ! m/s
    cfl = 0.8d0
    dt = cfl * dx / Uave           ! seconds 
    itmax = int(domain_length/Uave/dt)    ! max time step number
    ich = 8000d0                     ! m
    bumph = 100d0                  ! m
    output_interval = 600d0        ! s
    ictype = 1
    
    write(*,*)"Simple advection simulation:"
    write(*,*)"\tCFL number: ", cfl
    write(*,*)"\tTime Step: ", dt, "seconds"
    
        ! initilize height
    allocate( h(idim), ha(idim), hb(idim) )
  
    if(ictype==1)then
      
           ! Initial conditions (gaussian bump in middle of domain)
       do i = 2, idim - 1
           h(i) = ich + bumph * exp(-(dble(i)*dx - dble(idim-1)*dx/2d0)**2d0/ & 
               (2.0*(dble(idim-2)*dx*0.04d0)**2d0))
       end do
       
    else if(ictype==2)then
    
          ! square wave
        do i = 2, idim - 1
            if(i >= 40 .and. i <= 60)then
                h(i) = bumph + ich
            else
                h(i) = ich
            end if
        end do
        
    else if(ictype==3)then
      
          ! triangular wave
        do i = 2, idim - 1
            if(i >= 40 .and. i <= 50)then
                h(i) = bumph/10d0*(dble(i) - 40d0) + ich
            else if(i > 50 .and. i <= 60)then
                h(i) = -bumph/10d0*(dble(i) - 50d0) + ich + bumph
            else
                h(i) = ich
            end if
        end do
        
    end if
      
        ! initial boundary conditions
    h(1) = h(idim-1)
    h(idim) = h(2)
    write(11,"(1f12.5)",advance="no") 0d0
    write(11,"(1000f12.5)")(h(j),j=1,idim)
    
    ha = 0d0
    hb = 0d0
    output_time = 0d0
    
    do t = 1, itmax
        
        ! tendency calculation
        do j = 2, idim - 1
            
            if( t == 1 ) then
                ha(j) = h(j) - Uave*dt/dx*(h(j+1)-h(j-1)) 
            else 
                ha(j) = hb(j) - Uave*dt/dx*(h(j+1)-h(j-1)) 
            end if
            
        end do
    
        ha(1) = ha(idim-1)
        ha(idim) = ha(2)
    
        hb = h
        h = ha
        
            ! write output
        output_time = output_time + dt
        
        if(output_time >= output_interval) then
            output_time = 0d0
            write(11,"(1f12.5)",advance="no")dble(t)*dt
            write(11,"(1000f12.5)")(h(j),j=1,idim)
        end if
        
    end do
    

end program