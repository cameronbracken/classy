      subroutine zack(q,xnew,n)
c     implicit double precision (a-h,o-z)
      parameter(xl=1.,xr=1.,xint=0.,dt=1,d=.001,tmax=1)
      dimension a(n,n),b(n),l(n),m(n),xnew(n),xold(n),bi(n),q(n)
      dx=1/real(n+1)
      write(*,*)'dx',dx
c     dimension xint(n),bi(n)
c model solves the diffusion equation
c c dot = A c + b
c determine stability parameter
c      z=d*dt/(dx**2)
c      print*,'Courant/Von Neumann Condition  ',z
c      print*,'steady-state option (0)'
c      read*,istate
      istate=0
c     initialize matrices
c     xint(1)=.666667
c     xint(2)=.333333
      do 10 i=1,n
          b(i)=0.
          xnew(i)=0.
          xold(i)=0.
          do 11 j=1,n
              a(i,j)=0.
11        continue
10    continue
c create boundary condition vector
      bi(1)=d*xl/(dx**2)
      bi(n)=d*xr/(dx**2)
      b(1)=bi(1)
      b(n)=bi(n)
      do 69 i=1,n
          b(i)=b(i)+q(i)/dx
69    continue
c create coefficient matrix
      do 20 i=1,n
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
20    continue
c      open(20,file='atest.out')
      write(*,*)'the a matrix'
      do 30 i=1,n
          write(*,'(1000f10.3)')(a(i,j),j=1,n)
30    continue
c determine matrix inverse
      kq=n**2 
      call minv(a,n,det,l,m,kq)      
      write(*,*)'the matrix inverse-------------------'  
c      do 1235 i=1,n
c          write(20,*)i
c          write(20,*)(a(i,j),j=1,n)
c1235  continue
      print*,'det  ',det
c perform simulations
      do 1000 kk=1,ifix(tmax)
          if(kk.eq.1.and.istate.ne.0)then
          do 1010 j=1,n
c             b(j)=bi(j)+xint(j)/dt
              b(j)=bi(j)+xint/dt
              print*,j,b(j)
1010      continue
          endif

          if(kk.ne.1)then
             do 1011 j=1,n
                b(j)=bi(j)+xold(j)/dt
1011         continue
          endif

c     determine matrix product
          do 1100 i=1,n
              sum=0.
              do 1110 j=1,n
                  sum=sum+a(i,j)*b(j)
1110          continue
              xnew(i)=sum
1100      continue
c        print*,'kk= ',kk
        do 1111 i=1,n 
            write(*,*)xnew(i)
1111    continue
c        do 1200 i=1,n
c            xold(i)=xnew(i)
c1200    continue

        if(istate.eq.0)go to 1234 
1000  continue
1234  return
      end


      SUBROUTINE MINV(A,N,D,L,M,KQ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(KQ),L(N),M(N)
      D=1.0
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
10    IF(DABS(BIGA)-DABS(A(IJ))) 15,20,20 
c 10    if(abs(biga)-abs(a(ij))) 15,20,20
15    BIGA=A(IJ)
      L(K)=I
      M(K)=J
20    CONTINUE
      J=L(K)
      IF(J-K) 35,35,25
25    KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
30    A(JI)=HOLD
35    I=M(K)
      IF(I-K) 45,45,38
38    JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
40    A(JI)=HOLD
45    IF(BIGA) 48,46,48
46    D=0.0
      RETURN
48    DO 55 I=1,N
      IF(I-K) 50,55,50
50    IK=NK+I
      A(IK)=A(IK)/(-BIGA)
55    CONTINUE
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K) 60,65,60
60    IF(J-K) 62,65,62
62    KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
65    CONTINUE
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K) 70,75,70
70    A(KJ)=A(KJ)/BIGA
75    CONTINUE
      D=D*BIGA
      A(KK)=1.0/BIGA
80    CONTINUE
      K=N 
100   K=K-1
      IF(K) 150,150,105
105   I=L(K)
      IF(I-K) 120,120,108
108   JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J
      HOLD=A(JK)
      JI=JR+J
      A(JK)=-A(JI)
110   A(JI)=HOLD
120   J=M(K)
      IF(J-K) 100,100,125
125   KI=K-N
      DO 130 I=1,N
      KI=KI+N
      HOLD=A(KI)
      JI=KI-K+J
      A(KI)=-A(JI)
130   A(JI)=HOLD
      GO TO 100
150   RETURN
      END 
