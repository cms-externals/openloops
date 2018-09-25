
c set error code
      subroutine dd_set_error_code(err)
        implicit none
        integer, intent(in) :: err
        integer :: error_code
        common /dd_error/ error_code
        save /dd_error/
        error_code = err
      end subroutine dd_set_error_code

c retrieve and reset error code
      function dd_get_error_code()
        implicit none
        integer :: dd_get_error_code
        integer :: error_code
        common /dd_error/ error_code
        save /dd_error/
        dd_get_error_code = error_code
        call dd_set_error_code(0)
      end function dd_get_error_code


**********************************************************************
        subroutine DDsetparam(xdeltauv,xmuv2,xdelta2ir,xdelta1ir,
     &			      xmir2,xmx2)
**********************************************************************
*       parameter initalization of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	real*8 xmx2(10)
        integer i

c UV parameters
        deltauv  = xdeltauv 
        muv2     = xmuv2   

c IR parameters
        delta2ir = xdelta2ir
        delta1ir = xdelta1ir
        mir2     = xmir2    

c small mass parameters
        do i=1,10
          mx2(i) = xmx2(i) 
        enddo

        call dd_set_error_code(0)

        end

**********************************************************************
        subroutine DDgetparam(xdeltauv,xmuv2,xdelta2ir,xdelta1ir,
     &			      xmir2,xmx2)
**********************************************************************
*       read parameters of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	real*8 xmx2(10)
        integer i

c UV parameters
        xdeltauv  = deltauv 
        xmuv2     = muv2   

c IR parameters
        xdelta2ir = delta2ir
        xdelta1ir = delta1ir
        xmir2     = mir2    

c small mass parameters
        do i=1,10
          xmx2(i) = mx2(i) 
        enddo

        end

**********************************************************************
        subroutine DDsetmode(xcacc,xdacc,xmode34,xmode5,xmode6,
     &		             xoutlevel)
**********************************************************************
*       set mode of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	integer xmode34,xmode5,xmode6,xoutlevel

	cacc     = xcacc
	dacc     = xdacc
	mode34   = xmode34
	mode5    = xmode5
	mode6    = xmode6
	outlevel = xoutlevel

	end

**********************************************************************
        subroutine DDgetmode(xcacc,xdacc,xmode34,xmode5,xmode6,
     &		             xoutlevel)
**********************************************************************
*       read mode of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	integer xmode34,xmode5,xmode6,xoutlevel

	xcacc     = cacc
	xdacc     = dacc
	xmode34   = mode34
	xmode5    = mode5
	xmode6    = mode6
	xoutlevel = outlevel

	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        FUNCTION cspen_dd(Z)                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SPENCE-FUNKTION KOMPLEX, FREI NACH HOLLIK                     C
C---------------------------------------------------------------------C
C       20.07.83    LAST CHANGED 10.05.89        ANSGAR DENNER        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        COMPLEX*16 cspen_dd,W,SUM,Z,U                                     
        REAL*8 RZ,AZ,A1                                                
        REAL*8 B(9)/                                                   
     1   0.1666666666666666666666666667D0,                             
     2  -0.0333333333333333333333333333D0,                             
     3   0.0238095238095238095238095238D0,                             
     4  -0.0333333333333333333333333333D0,                             
     5   0.0757575757575757575757575758D0,                             
     6  -0.2531135531135531135531135531D0,                             
     7   1.1666666666666666666666666667D0,                             
     8  -7.09215686274509804D0         ,                               
     9  54.97117794486215539D0         /                               
C     BEACHTE:                 B(N)=B2N                                
C     B(1)=1./6.                                                       
C     B(2)=-1./30.                                                     
C     B(3)=1./42.                                                      
C     B(4)=-1./30.                                                     
C     B(5)=5./66.                                                      
C     B(6)=-691./2730.                                                 
C     B(7)=7./6.                                                       
C     B(8)=-3617./510.                                                 
C     B(9)=43867./798.                                                 
C     B(10)=-174611./330.                                              
C     B(11)=854513./138.                                               
C     PI=3.1415926535897932384                                         
C     PI*PI/6.=1.6449..., PI*PI/3=3.28986...                           
C                                                                      
      Z =Z*DCMPLX(1D0)                                                 
      RZ=DREAL(Z)                                                      
      AZ=CDABS(Z)                                                      
      A1=CDABS(1D0-Z)                                                  
C     IF((SNGL(RZ) .EQ. 0.0) .AND. (SNGL(DIMAG(Z)) .EQ. 0.0)) THEN     
C ---> CHANGED  10.5.89                                                
      IF(AZ .LT. 1D-20) THEN                                           
        cspen_dd=-CDLOG(1D0-Z)                                            
        RETURN                                                         
      END IF                                                           
c      IF((SNGL(RZ) .EQ. 1.0) .AND. (SNGL(DIMAG(Z)) .EQ. 0.0)) THEN     
c ---> changed 5.7.94
       IF( (ABS(RZ-1D0).LT.1D-18) .AND. (ABS(DIMAG(Z)).LT.1D-18) ) THEN     
        cspen_dd=1.64493406684822643D0                                    
        RETURN                                                         
      END IF                                                           
      IF(RZ.GT.5D-1) GOTO 20                                           
      IF(AZ.GT.1D0) GOTO 10                                            
      W=-CDLOG(1D0-Z)                                                  
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 2                                     
      DO 1 K=1,9                                                       
      U=U*W*W/DFLOAT(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 2                            
      SUM=SUM+U*B(K)                                                   
 1    CONTINUE                                                         
 2    cspen_dd=SUM                                                        
      RETURN                                                           
10    W=-CDLOG(1D0-1D0/Z)                                              
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 12                                    
                                                                       
      DO 11 K=1,9                                                      
      U=U*W*W/DFLOAT(2*K*(2*K+1))                                      
      IF(CDABS(B(K)*U/SUM).LT.1D-20) GOTO 12                           
      SUM=SUM+U*B(K)                                                   
11    CONTINUE                                                         
12    cspen_dd=-SUM-1.64493406684822643D0-.5D0*CDLOG(-Z)**2               
      RETURN                                                           
20    IF(A1.GT.1D0) GOTO 30                                            
      W=-CDLOG(Z)                                                      
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 22                                    
      DO 21 K=1,9                                                      
      U=U*W*W/DFLOAT(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 22                           
      SUM=SUM+U*B(K)                                                   
21    CONTINUE                                                         
22    cspen_dd=-SUM+1.64493406684822643D0-CDLOG(Z)*CDLOG(1D0-Z)           
      RETURN                                                           
30    W=CDLOG(1D0-1D0/Z)                                               
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 32                                    
      DO 31 K=1,9                                                      
      U=U*W*W/DFLOAT(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 32                           
      SUM=SUM+U*B(K)                                                   
31    CONTINUE                                                         
32    cspen_dd=SUM+3.28986813369645287D0                                  
     *               +.5D0*CDLOG(Z-1D0)**2-CDLOG(Z)*CDLOG(1D0-Z)       
50    CONTINUE                                                         
        END                                                            

***********************************************************************
        FUNCTION eta_dd(C1,C2)                                            
***********************************************************************
*       COMPLEX ETA-FUNKTION                                           
*---------------------------------------------------------------------*
*       8.06.90    ANSGAR DENNER                                       
***********************************************************************
        IMPLICIT   LOGICAL(A-Z)                                        
        COMPLEX*16 eta_dd,C1,C2                                           
        REAL*8     PI,IM1,IM2,IM12                                     

        PI     = 4D0*DATAN(1D0)                                        
        IM1    = DIMAG(C1)                                             
        IM2    = DIMAG(C2)                                             
        IM12   = DIMAG(C1*C2)                                          

	if (((IM1.eq.0d0).and.(DREAL(C1).lt.0d0)).or.
     &	    ((IM2.eq.0d0).and.(DREAL(C2).lt.0d0)).or.
     &	    ((IM12.eq.0d0).and.(DREAL(C1*C2).lt.0d0))) then
	  write(*,*) 'eta function on cut !!!'
	  write(*,*) 'C1    = ',C1
	  write(*,*) 'C2    = ',C2
	  write(*,*) 'C1*C2 = ',C1*C2
	  call DD_debugoutput()
	  call dd_set_error_code(1)
	endif
                                                                       
        IF(IM1.LT.0D0.AND.IM2.LT.0D0.AND.IM12.GT.0D0) THEN             
            eta_dd = DCMPLX(0D0,2D0*PI)                                   
        ELSE IF (IM1.GT.0D0.AND.IM2.GT.0D0.AND.IM12.LT.0D0) THEN       
            eta_dd = DCMPLX(0D0,-2D0*PI)                                  
        ELSE                                                           
            eta_dd = DCMPLX(0D0)                                          
        END IF                                                         
        END                                                            

***********************************************************************
        function sqe_dd(a,b,c)                                            
***********************************************************************
*       Solution of quadratic equation a*x^2 + b*x + c = 0
*----------------------------------------------------------------------
*       9.4.08  Stefan Dittmaier				       
***********************************************************************
        implicit real*8 (a-z)                                        
        complex*16 a,b,c,sqe_dd,x1,x2

	if ((a.eq.(0d0,0d0)).and.(b.eq.(0d0,0d0))) then
	  write(*,*) 'sqe_dd: a=b=0 not allowed'
	  call dd_set_error_code(1)
	elseif ((a.eq.(0d0,0d0)).and.(b.ne.(0d0,0d0))) then
	  sqe_dd = -c/b
	else
	  x1=(-b+sqrt(b**2-4d0*a*c))/2d0/a
	  x2=(-b-sqrt(b**2-4d0*a*c))/2d0/a
	  if (abs(x1).gt.abs(x2)) then
	     sqe_dd=x1
	  else
	     sqe_dd=x2
	  endif
	endif

        end                                                            

***********************************************************************
        function xdet_dd(A,n)
************************************************************************
*	determinant of complex nxn-matrix A
*-----------------------------------------------------------------------
*       10.6.04 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)
	integer n
	complex*16 A(n,n),Q(n,n),R(n,n),detQ,xdet_dd
	integer i

	call xQRdecomp_dd(A,Q,R,detQ,n)

	xdet_dd = detQ*r(n,n)
	do i=1,n-1
	  xdet_dd = xdet_dd*r(i,i)
	enddo

	end

***********************************************************************
        subroutine xinverse_dd(A,iA,detA,n)
************************************************************************
*	inverse of complex nxn-matrix A
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)
	integer n
	complex*16 A(n,n),Q(n,n),R(n,n),iA(n,n),iR(n,n),detQ,w,detA
	complex*16 iAAiA(n,n)
	integer i,j,k,l

	call xQRdecomp_dd(A,Q,R,detQ,n)

	detA = detQ*R(n,n)
	do i=1,n-1
	  detA = detA*R(i,i)
	enddo

c invert R
	do i=n,1,-1
	  do j=1,n
	    if (j.lt.i) then
	      iR(i,j) = 0d0
	    else
	      w = 0d0
	      do k=i+1,n
	        w = w + R(i,k)*iR(k,j)
	      enddo
	      if (i.eq.j) then
	        iR(i,j) = (1d0-w)/R(i,i)
	      else
	        iR(i,j) = -w/R(i,i)
	      endif
	    endif
	  enddo
	enddo

c inverse of A
	do i=1,n
	  do j=1,n
	    iA(i,j) = 0d0
	    do k=1,n
	      iA(i,j) = iA(i,j) + iR(i,k)*dconjg(Q(j,k))
	    enddo
	  enddo
	enddo

	end

***********************************************************************
        subroutine xQRdecomp_dd(A,Q,R,detQ,n)
************************************************************************
*	QR-decomposition of complex nxn-matrix A using Householder matrices
*	Q = orthogonal matrix, R = upper triangle matrix
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)
	integer n
	complex*16 A(n,n),Q(n,n),R(n,n),w(n),wr(n),qw(n),detQ,aux,kap
	complex*16 rmax
	integer i,j,k,imax

	detQ = 1d0

c*** start with R=A and Q=unit matrix 
	do i=1,n
	  do j=1,n
	    r(i,j) = a(i,j)
	    if (i.eq.j) then
	      q(i,j) = 1d0
	    else
	      q(i,j) = 0d0
	    endif
	  enddo
	enddo

c*** n-1 steps of decomposition
	do 200 i=1,n-1

c exchange rows such that |r(i,i)| is maximal
	  imax = i
	  rmax = r(i,i)
	  do j=i+1,n
	    if (abs(r(j,i)).gt.abs(rmax)) then
	      imax = j
	      rmax = r(j,i)
	    endif
	  enddo
	  if (imax.ne.i) then
	    do j=1,n
	      aux       = r(i,j)
	      r(i,j)    = r(imax,j)
	      r(imax,j) = aux
	      aux       = q(j,i)
	      q(j,i)    = q(j,imax)
	      q(j,imax) = aux
	    enddo
	    detQ = -detQ
	  endif
c calculate R and Q if r(i,i) is non-zero
	  if (r(i,i).eq.(0d0,0d0)) goto 200
	  sig2 = 0d0
	  do j=i,n
	    sig2 = sig2 + abs(r(j,i))**2
	  enddo
	  sig = sqrt(sig2)
	  kap   = -r(i,i)/abs(r(i,i))*sig
	  wnorm = sqrt(2d0*sig*(sig+abs(r(i,i))))
	  do j=i,n
	    w(j) = r(j,i)/wnorm
	  enddo
	  w(i) = w(i) - kap/wnorm

	  do j=i,n
	    wr(j) = 0d0
	    do k=i,n
	      wr(j) = wr(j) + dconjg(w(k))*r(k,j)
	    enddo
	  enddo
	  do j=i,n
	    if (j.eq.i) then
	      r(j,i) = r(j,i) - 2d0*w(j)*wr(i)
	    else
	      r(j,i) = 0d0
	    endif
	    do k=i+1,n
	      r(j,k) = r(j,k) - 2d0*w(j)*wr(k)
	    enddo
	  enddo

c calculate matrix Q
	  do j=1,n
	    qw(j) = 0d0
	    do k=i,n
	      qw(j) = qw(j) + q(j,k)*w(k)
	    enddo
	  enddo
	  do j=1,n
	    do k=i,n
	      q(j,k) = q(j,k) - 2d0*dconjg(w(k))*qw(j)
	    enddo
	  enddo
	  detQ = -detQ

200	continue

	end

***********************************************************************
        function det_dd(A,n)
************************************************************************
*	determinant of real nxn-matrix A
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)
	integer n
	real*8 A(n,n),Q(n,n),R(n,n)
	integer i

	call QRdecomp_dd(A,Q,R,detQ,n)

	det_dd = detQ*R(n,n)
	do i=1,n-1
	  det_dd = det_dd*R(i,i)
	enddo

	end

***********************************************************************
        subroutine inverse_dd(A,iA,detA,n)
************************************************************************
*	inverse of real nxn-matrix A
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)
	integer n
	real*8 A(n,n),Q(n,n),R(n,n),iA(n,n),iR(n,n)
	integer i,j,k

	call QRdecomp_dd(A,Q,R,detQ,n)

	detA = detQ*R(n,n)
	do i=1,n-1
	  detA = detA*R(i,i)
	enddo

c invert R
	do i=n,1,-1
	  do j=1,n
	    if (j.lt.i) then
	      iR(i,j) = 0d0
	    else
	      w = 0d0
	      do k=i+1,n
	        w = w + R(i,k)*iR(k,j)
	      enddo
	      if (i.eq.j) then
	        iR(i,j) = (1d0-w)/R(i,i)
	      else
	        iR(i,j) = -w/R(i,i)
	      endif
	    endif
	  enddo
	enddo

c inverse of A
	do i=1,n
	  do j=1,n
	    iA(i,j) = 0d0
	    do k=1,n
	      iA(i,j) = iA(i,j) + iR(i,k)*Q(j,k)
	    enddo
	  enddo
	enddo

	end

***********************************************************************
        subroutine QRdecomp_dd(A,Q,R,detQ,n)
************************************************************************
*	QR-decomposition of real nxn-matrix A using Householder matrices
*	Q = orthogonal matrix, R = upper triangle matrix
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)
	integer n
	real*8 A(n,n),Q(n,n),R(n,n)
	real*8 w(n),wr(n),qw(n)
	integer i,j,k,imax

	detQ = 1d0

c*** start with R=A and Q=unit matrix 
	do i=1,n
	  do j=1,n
	    r(i,j) = a(i,j)
	    if (i.eq.j) then
	      q(i,j) = 1d0
	    else
	      q(i,j) = 0d0
	    endif
	  enddo
	enddo

c*** n-1 steps of decomposition
	do 200 i=1,n-1

c exchange rows such that |r(imax,i)| is maximal
	  imax = i
	  rmax = r(i,i)
	  do j=i+1,n
	    if (abs(r(j,i)).gt.abs(rmax)) then
	      imax = j
	      rmax = r(j,i)
	    endif
	  enddo
	  if (imax.ne.i) then
	    do j=1,n
	      aux       = r(i,j)
	      r(i,j)    = r(imax,j)
	      r(imax,j) = aux
	      aux       = q(j,i)
	      q(j,i)    = q(j,imax)
	      q(j,imax) = aux
	    enddo
	    detQ = -detQ
	  endif
c calculate R and Q if r(i,i) is non-zero
	  if (r(i,i).eq.0d0) goto 200
	  sig2 = 0d0
	  do j=i,n
	    sig2 = sig2 + r(j,i)**2
	  enddo
	  sig = sqrt(sig2)
	  kap   = -sign(1d0,r(i,i))*sig
	  wnorm = sqrt(2d0*sig*(sig+abs(r(i,i))))
	  do j=i,n
	    w(j) = r(j,i)/wnorm
	  enddo
	  w(i) = w(i) - kap/wnorm

	  do j=i,n
	    wr(j) = 0d0
	    do k=i,n
	      wr(j) = wr(j) + w(k)*r(k,j)
	    enddo
	  enddo
	  do j=i,n
	    if (j.eq.i) then
	      r(j,i) = r(j,i) - 2d0*w(j)*wr(i)
	    else
	      r(j,i) = 0d0
	    endif
	    do k=i+1,n
	      r(j,k) = r(j,k) - 2d0*w(j)*wr(k)
	    enddo
	  enddo

c calculate matrix Q
	  do j=1,n
	    qw(j) = 0d0
	    do k=i,n
	      qw(j) = qw(j) + q(j,k)*w(k)
	    enddo
	  enddo
	  do j=1,n
	    do k=i,n
	      q(j,k) = q(j,k) - 2d0*w(k)*qw(j)
	    enddo
	  enddo
	  detQ = -detQ

200	continue

	end

***********************************************************************
        subroutine DD_debugoutput()
************************************************************************
*       write out debug info
*-----------------------------------------------------------------------
*       8.3.13 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)
	integer i

c DD global settings and common blocks
#       include "DD_global.inc"

	write(*,*) 'DD debug info:'
	write(*,*) 'Last call by ',s_DDin
	write(*,*) 'Real input parameters:   ',nr_DDin
	do i=1,nr_DDin
	  write(*,'(g26.16)') r_DDin(i)  
	enddo
	write(*,*) 'Complex input parameters:',nc_DDin
	do i=1,nc_DDin
	  write(*,'(g26.16,3x,g26.16)') c_DDin(i)  
	enddo
	write(*,*) 'Integer input parameters:',ni_DDin
	do i=1,ni_DDin
	  write(*,*) i_DDin(i)  
	enddo

	end
