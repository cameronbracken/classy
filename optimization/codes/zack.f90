subroutine zack(q,xnew,n)
      implicit double precision (a-h,o-z)
      parameter(xl=1.,xr=1.,xint=0.,dt=1,d=.001,tmax=1)
      dimension a(n,n),b(n),l(n),m(n),xnew(n),xold(n),bi(n),q(n)
      dx=1/real(n+1)
!      write(*,*)'dx',dx
      istate=0
      do  i=1,n
          b(i)=0.
          xnew(i)=0.
          xold(i)=0.
          do j=1,n
              a(i,j)=0.
          end do
      end do
      bi(1)=d*xl/(dx**2)
      bi(n)=d*xr/(dx**2)
      b(1)=bi(1)
      b(n)=bi(n)
      do  i=1,n
          b(i)=b(i)+q(i)/dx
      end do
! create coefficient matrix
      do  i=1,n
          a(i,i)=d*(+2./(dx**2))
          if(istate.ne.0)then
              a(i,i)=a(i,i)+1./dt
          endif
          if (i.ne.n)then
              a(i,i+1)=-d/(dx**2)
          endif
          if(i.ne.1)then
              a(i,i-1)=-d/(dx**2)
          endif
      end do


!      open(20,file='atest.out')
      !write(*,*)'the a matrix'
      !do i=1,n
      !    write(*,'(1000f10.3)')(a(i,j),j=1,n),b(i)
      !end do
      
      kq=n**2
      call minv(a,n,det,l,m,kq)
      
      !write(*,*)'the a matrix inverse'
      !do i=1,n
      !    write(*,'(1000f10.3)')(a(i,j),j=1,n)
      !end do
      
      xnew=matmul(a,b)
!      write(*,*)'the solution'
      !do i=1,n
      !    write(*,'(1000f10.3)')xnew(i)
      !end do

end
