!program call
!    integer::i 
!	integer,parameter::nnodes=10
!	double precision,dimension(:)::q(nnodes),head(nnodes)
!	head=0
!	q=0
!	!q(5)=-.1
!    call zack(q,head,nnodes)
!    do i=1,nnodes
!		write(*,*)head(i)
!    end do
!end program call

subroutine funobj(mode,n,x,f,g,nstate,nprob,z,nwcore)
  implicit double precision (a-h,o-z)
  double precision::x(n),g(n),z(nwcore)
  integer,parameter::nnodes=10
  double precision::q(nnodes),head(nnodes)

  open(31,file="pumping.out")
  
  !set q= to decision variable x for minos
  q=x
  write(31,"(100f10.5)")(q(i),i=1,nnodes)
  
  !calculate new head levels with gwtrans
  call zack(-q,head,nnodes)
  
  open(32,file="head.out")
  write(32,"(100f12.5)")(head(i),i=1,nnodes)

  !calculate objective maximize the sum of the heads
  cost=0
  do i=1,nnodes
    !cost=cost+q(i)*10.*(50.-head(i))
    cost=cost+head(i)
  end do
  
  !objective for minos
  f=cost
  return
end subroutine funobj


SUBROUTINE FUNCON(MODE,M,N,NJAC,X,F,G,NSTATE,NPROB,Z,NWCORE) 
RETURN 
END 

SUBROUTINE MATMOD(NCYCLE,NPROB,FINISH,M,N,NB,NE,NKA,& 
NS,NSCL,A,HA,KA,BL,BU,ASCALE,HS,ID1,ID2,X,PI,Z,NWCORE) 
RETURN 
END 
