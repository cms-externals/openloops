!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ************************
!  *  module GramCayley   *
!  *    by Lars Hofer     *
!  ************************
!
!  functions and subroutines:
!  CalcGramMat, CalcGramMat2, CalcVecF, CalcVecF2
!  CalcCayleyMat, CalcCayleyMat0, CalcCayleyMat02, CalcModCayleyMat,
!  CalcModCayleyMat0, CalcModCayleyMat02, CalcX0k
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module GramCayley

  use FourVectors
  use MatrixManipulations

  implicit none
  
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcGramMat(Nm1,mom)
  !
  !  calculates Gram matrix Z^(Nm1) from momenta
  !  mom=((p1_0,...,p1_3),(p2_0,...,p2_3),...,
  !       (pNm1_0,...,pNm1_3))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcGramMat(Nm1,mom)
  
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: mom(0:3,Nm1)
    complex*16 :: CalcGramMat(Nm1,Nm1)
    complex*16 :: p1(0:3), p2(0:3)
    integer :: i,j
    
    do i=1,Nm1
      p1 = mom(0:3,i)
      do j=i,Nm1
        p2 = mom(0:3,j)
        CalcGramMat(j,i) = 2d0 * CalcFScalar(p1,p2)
        CalcGramMat(i,j) = CalcGramMat(j,i)
      end do
    end do
    
  end function CalcGramMat
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcGramMat2(Nm1,MomInv)
  !
  !  calculates Gram matrix Z^(Nm1) from invariants
  !  MomInv=(I(1,1),I(1,2),...,I(1,Nm1),I(2,2),...,I(2,Nm1),...,I(Nm1,Nm1))
  !  with    I(i,j)=(pi-pj)^2 (i=/=j),   I(i,i)=pi^2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcGramMat2(Nm1,MomInv)
  
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MomInv(BinomTable(2,Nm1+1))
    complex*16 :: CalcGramMat2(Nm1,Nm1)
    integer :: i,j,cnt
    
    CalcGramMat2 = 0
    cnt = 1
    do i=1,Nm1
      CalcGramMat2(i,i:Nm1) = CalcGramMat2(i,i:Nm1) + MomInv(cnt)
      CalcGramMat2(1:i,i) = CalcGramMat2(1:i,i) + MomInv(cnt)
      cnt = cnt+1
      
      do j=i+1,Nm1
        CalcGramMat2(i,j) = CalcGramMat2(i,j) - MomInv(cnt)
        cnt = cnt+1
      end do
    end do
    
    do i=1,Nm1
      do j=i+1,Nm1
        CalcGramMat2(j,i) = CalcGramMat2(i,j)
      end do
    end do
    
  end function CalcGramMat2
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcVecF(Nm1,MomP,masses2)
  !
  !  calculates vector of quantities
  !  fk = pk^2 - mk^2 + m0^2
  !  from   MomP = ((p1^0,...,p1^3),(p2^0,...,p2^3),...,
  !                 (pNm1^0,...,pNm1^3))
  !  and    masses2 = (m0^2,...,mNm1^2)
  !  saved as   VecF = (f1,...,fNm1)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  function CalcVecF(Nm1,MomP,masses2)
    
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MomP(0:3,Nm1), masses2(0:Nm1)  
    complex*16, dimension(Nm1) :: CalcVecF
    integer :: i
    
    do i=1,Nm1
      CalcVecF(i) = CalcFNorm2(MomP(0:3,i)) - masses2(i) &
                  & + masses2(0)
    end do
    
  end function CalcVecF
  




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcVecF2(Nm1,MomInv,masses2)
  !
  !  calculates vector of quantities
  !  fk = pk^2 - mk^2 + m0^2
  !  from    MomInv=(I(1,1),I(1,2),...,I(1,Nm1),I(2,2),...,I(2,Nm1),...,I(Nm1,Nm1))
  !  with    I(i,j)=(pi-pj)^2 (i=/=j),   I(i,i)=pi^2
  !  and    masses2 = (m0^2,...,mNm1^2)
  !  saved as   VecF = (f1,...,fNm1)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  function CalcVecF2(Nm1,MomInv,masses2)
    
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MomInv(BinomTable(2,Nm1+1)), masses2(0:Nm1)  
    complex*16, dimension(Nm1) :: CalcVecF2
    integer :: i, cnt
    
    cnt = 1
    do i=1,Nm1
      CalcVecF2(i) = MomInv(cnt) - masses2(i) &
                  & + masses2(0)
      cnt = cnt+Nm1+1-i
    end do
    
  end function CalcVecF2
  

  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcCayleyMat(Nm1,m02,GramMat,VecF)
  !
  !  calculates Cayley matrix X^(Nm1) from Z_ij, f_k and m0^2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcCayleyMat(Nm1,m02,GramMat,VecF)
  
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: m02, GramMat(Nm1,Nm1), VecF(Nm1)
    complex*16 :: CalcCayleyMat(0:Nm1,0:Nm1)
    
    CalcCayleyMat(0,0) = 2d0*m02
    CalcCayleyMat(0,1:Nm1) = VecF
    CalcCayleyMat(1:Nm1,0) = VecF
    CalcCayleyMat(1:Nm1,1:Nm1) = GramMat
  
  end function CalcCayleyMat
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcCayleyMat0(Nm1,MomP,masses2)
  !
  !  calculates Cayley matrix X^(Nm1)
  !  from   MomP = ((p1^0,...,p1^3),(p2^0,...,p2^3),...,
  !                 (pNm1^0,...,pNm1^3))
  !  and    masses2 = (m0^2,...,mNm1^2) 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcCayleyMat0(Nm1,MomP,masses2)
     
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MomP(0:3,Nm1), masses2(0:Nm1)  
    complex*16 :: GramMat(Nm1,Nm1), VecF(Nm1), CalcCayleyMat0(0:Nm1,0:Nm1)
    
    GramMat = CalcGramMat(Nm1,MomP)
    VecF = CalcVecF(Nm1,MomP,masses2)
    CalcCayleyMat0 = CalcCayleyMat(Nm1,masses2(0),GramMat,VecF)
  
  end function CalcCayleyMat0





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcCayleyMat02(Nm1,MomInv,masses2)
  !
  !  calculates Cayley matrix X^(Nm1)
  !  from    MomInv=(I(1,1),I(1,2),...,I(1,Nm1),I(2,2),...,I(2,Nm1),...,I(Nm1,Nm1))
  !  with    I(i,j)=(pi-pj)^2 (i=/=j),   I(i,i)=pi^2
  !  and    masses2 = (m0^2,...,mNm1^2) 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcCayleyMat02(Nm1,MomInv,masses2)
     
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MomInv(BinomTable(2,Nm1+1)), masses2(0:Nm1)  
    complex*16 :: GramMat(Nm1,Nm1), VecF(Nm1), CalcCayleyMat02(0:Nm1,0:Nm1)
    
    GramMat = CalcGramMat(Nm1,MomInv)
    VecF = CalcVecF(Nm1,MomInv,masses2)
    CalcCayleyMat02 = CalcCayleyMat(Nm1,masses2(0),GramMat,VecF)
  
  end function CalcCayleyMat02
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcModCayleyMat(Nm1,m02,GramMat,VecF)
  !
  !  calculates modified Cayley matrix Y^(Nm1) 
  !  from Z_ij, f_k and m0^2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcModCayleyMat(Nm1,m02,GramMat,VecF)
  
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: m02, GramMat(Nm1,Nm1), VecF(Nm1)
    complex*16 :: CalcModCayleyMat(0:Nm1,0:Nm1)
    integer :: i, j
    
    CalcModCayleyMat(0,0) = 2d0*m02
    do i=1,Nm1
      CalcModCayleyMat(i,0) = CalcModCayleyMat(0,0) - VecF(i)
      CalcModCayleyMat(0,i) = CalcModCayleyMat(i,0)
      do j=i,Nm1
        CalcModCayleyMat(j,i) = GramMat(j,i) - VecF(i) + CalcModCayleyMat(0,j)
        CalcModCayleyMat(i,j) = CalcModCayleyMat(j,i)
      end do
    end do
  
  end function CalcModCayleyMat
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcModCayleyMat0(Nm1,MomP,masses2)
  !
  !  calculates modified Cayley matrix Y^(Nm1)
  !  from   MomP = ((p1^0,...,p1^3),(p2^0,...,p2^3),...,
  !                 (pNm1^0,...,pNm1^3))
  !  and    masses2 = (m0^2,...,mNm1^2) 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcModCayleyMat0(Nm1,MomP,masses2)
     
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MomP(0:3,Nm1), masses2(0:Nm1)  
    complex*16 :: GramMat(Nm1,Nm1), VecF(Nm1), CalcModCayleyMat0(0:Nm1,0:Nm1)
    
    GramMat = CalcGramMat(Nm1,MomP)
    VecF = CalcVecF(Nm1,MomP,masses2)
    CalcModCayleyMat0 = CalcModCayleyMat(Nm1,masses2(0),GramMat,VecF)
  
  end function CalcModCayleyMat0





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcModCayleyMat02(Nm1,MomInv,masses2)
  !
  !  calculates modified Cayley matrix Y^(Nm1)
  !  from    MomInv=(I(1,1),I(1,2),...,I(1,Nm1),I(2,2),...,I(2,Nm1),...,I(Nm1,Nm1))
  !  with    I(i,j)=(pi-pj)^2 (i=/=j),   I(i,i)=pi^2
  !  and    masses2 = (m0^2,...,mNm1^2) 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcModCayleyMat02(Nm1,MomInv,masses2)
     
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MomInv(BinomTable(2,Nm1+1)), masses2(0:Nm1)  
    complex*16 :: GramMat(Nm1,Nm1), VecF(Nm1), CalcModCayleyMat02(0:Nm1,0:Nm1)
    
    GramMat = CalcGramMat(Nm1,MomInv)
    VecF = CalcVecF(Nm1,MomInv,masses2)
    CalcModCayleyMat02 = CalcModCayleyMat(Nm1,masses2(0),GramMat,VecF)
  
  end function CalcModCayleyMat02





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcX0k(Nm1,MatX,kbest,MatX0k)
  !
  !  Calculate MatX0k defined in eq. (7.10) of hep-ph/0509141
  !  kbest is determined such that det(MatX0k) is maximal 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcX0k(Nm1,MatX,kbest,MatX0k)
  
    integer, intent(in) :: Nm1
    complex*16, intent(in) :: MatX(0:,0:)
    complex*16, intent(out) :: MatX0k(Nm1,Nm1)
    integer, intent(out) :: kbest
    complex*16 :: det, newdet
    integer :: i,j
  
    ! determine X_(0,Nm1)
    do j=1,Nm1
      do i=1,Nm1
        MatX0k(i,j) = MatX(i,j-1)
      end do
    end do

    det = CalcDet(Nm1,MatX0k)
    kbest = Nm1

    do j=Nm1,2,-1
      do i=1,Nm1
        MatX0k(i,j) = MatX(i,j)
      end do

      newdet =  CalcDet(Nm1,MatX0k)
      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
        det = newdet
      end if
    
    end do
    
    do i=1,Nm1
      MatX0k(i,1) = MatX(i,1)
      MatX0k(i,kbest) = MatX(i,0)
    end do
  
  end subroutine CalcX0k
  
end module GramCayley
  
  