!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ************************
!  *  module LightCone    *
!  *   by Lars Hofer      *
!  ************************
!
!  global constant:
!  MetricTensor_LC, MetricTensorSym_LC
!
!  global variable:
!  MetricTensorProd_LC
!
!  functions and subroutines:
!  SetMetricTensorProd, CalcMetricTensorProd, 
!  CalcFScalar, CalcFNorm2, CalcFDiff
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module LightCone

  use ColiCombinatorics
  use FourVectors
  use TensorManipulations
  use BuildTensors
  use TensorReduction
  

  implicit none


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  constants MetricTensor_LC, MetricTensorSym_LC
  !
  !  represents the metric tensor g^(mu,nu) in the form
  !  MetricTensor = (g^00,g^01,g^02,g^02,g^10,...,g^33)
  !  MetricTensorSym = (g^00,g^01,g^02,g^03,g^11,g^12,g^13,g^22,g^23,g^33)
  !  in lightcone coordinates
  !
  !  
  !  constants ST_to_LC, LT_ST
  !
  !  transformation matrices for changing from standard to lightcone 
  !  coordinates and vice versa
  !  
  !
  !  global variable MetricTensorProd
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  complex*16, dimension(0:15), parameter :: MetricTensor_LC = (/ 0d0, 2d0, 0d0, 0d0, &
                &  2d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, -2d0, 0d0, 0d0, -2d0, 0d0 /)
  complex*16, dimension(1:10), parameter :: MetricTensorSym_LC = (/ 0d0, 2d0, 0d0, 0d0, &
                &  0d0, 0d0, 0d0, 0d0, -2d0, 0d0 /)
  complex*16, dimension(0:3,0:3), parameter :: STtoLC = Reshape((/(1d0,0d0),(1d0,0d0),(0d0,0d0),(0d0,0d0), &
                &  (0d0,0d0),(0d0,0d0),(1d0,0d0),(1d0,0d0),(0d0,0d0),(0d0,0d0),(0d0,1d0),(0d0,-1d0), &
                &  (1d0,0d0),(-1d0,0d0),(0d0,0d0),(0d0,0d0)/),(/4,4/))
  complex*16, dimension(0:3,0:3), parameter :: LCtoST = Reshape((/(0.5d0,0d0),(0d0,0d0),(0d0,0d0),(0.5d0,0d0), &
                &  (0.5d0,0d0),(0d0,0d0),(0d0,0d0),(-0.5d0,0d0),(0d0,0d0),(0.5d0,0d0),(0d0,-0.5d0),(0d0,0d0), &
                &  (0d0,0d0),(0.5d0,0d0),(0d0,0.5d0),(0d0,0d0)/),(/4,4/))
  
  complex*16, dimension(:,:), allocatable :: MetricTensorProd_LC
  complex*16, dimension(:,:,:), allocatable :: Trafo_STtoLC, Trafo_LCtoST
  integer, dimension(:,:,:,:,:), allocatable :: TStruc_LC
  
  
contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMetricTensorProd_LC(kmax)
  !
  !  sets the variable MetricTensorProd_LC to the value CalcMetricTensorProd_LC(kmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMetricTensorProd_LC(kmax)
  
    integer, intent(in) :: kmax
    integer :: k
    
    if (allocated(MetricTensorProd_LC)) then
      deallocate(MetricTensorProd_LC)
    end if
    allocate(MetricTensorProd_LC(RankToSize(4,2*kmax),0:kmax))
    MetricTensorProd_LC = 0
    
    do k=0,kmax
       MetricTensorProd_LC(1:RankToSize(4,2*k),k) = TransformTensor_STtoLC(2*k, &
           &  MetricTensorProd(1:RankToSize(4,2*k),k))
    end do
    
  end subroutine SetMetricTensorProd_LC
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTStruc_LC(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetTStruc_LC(rmax)
  
    integer, intent(in) :: rmax

    if (allocated(TStruc_LC)) then
      deallocate(TStruc_LC)
    end if
    allocate(TStruc_LC(0:1,BinomTable(rmax/2,3+rmax/2),RankToSize(4,rmax),rmax/2,0:rmax))

    TStruc_LC = CalcTStruc_LC(rmax)

  end subroutine




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTStruc_LC(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTStruc_LC(rmax)

    integer, intent(in) :: rmax
    integer :: CalcTStruc_LC(0:1,BinomTable(rmax/2,3+rmax/2),RankToSize(4,rmax),rmax/2,0:rmax)
    integer :: r, k, str, stk, cnt

    CalcTStruc_LC = 0
    do r=0,rmax
      do k=1,(rmax-r)/2
        do str=1,RankToSize(4,r)
          cnt = 1
          do stk=1,RankToSize(4,2*k)
            if (MetricTensorProd_LC(stk,k).ne.0d0) then
              CalcTStruc_LC(0,cnt,str,k,r) = IndsTensorProd2(1,stk,str,2*k,r)
              CalcTStruc_LC(1,cnt,str,k,r) = MetricTensorProd_LC(stk,k)*IndsTensorProd2(0,stk,str,2*k,r)
              cnt = cnt+1
            end if
          end do
        end do
      end do
    end do
  end function CalcTStruc_LC




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTrafo_STtoLC(r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetTrafo_STtoLC(r)

    integer, intent(in) :: r
    integer :: str, i, sti

    if (allocated(Trafo_STtoLC)) then
      deallocate(Trafo_STtoLC)
    end if
    str = RankToSize(4,r)
    allocate(Trafo_STtoLC(str,str,0:r))
    Trafo_STtoLC = 0

    Trafo_STtoLC(1,1,0) = 1
    do i=1,r
      sti = RankToSize(4,i)
      Trafo_STtoLC(1:sti,1:sti,i) = CalcTrafoMatrix(i,STtoLC)
    end do

  end subroutine SetTrafo_STtoLC





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTrafo_LCtoST(r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetTrafo_LCtoST(r)

    integer, intent(in) :: r
    integer :: str, i, sti

    if (allocated(Trafo_LCtoST)) then
      deallocate(Trafo_LCtoST)
    end if
    str = RankToSize(4,r)
    allocate(Trafo_LCtoST(str,str,0:r))
    Trafo_LCtoST = 0

    Trafo_LCtoST(1,1,0) = 1
    do i=1,r
      sti = RankToSize(4,i)
      Trafo_LCtoST(1:sti,1:sti,i) = CalcTrafoMatrix(i,LCtoST)
    end do

  end subroutine SetTrafo_LCtoST




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTrafoMatrix(r,AtoB)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTrafoMatrix(r,AtoB) result(trafo)

    integer, intent(in) :: r
    complex*16, intent(in) :: AtoB(0:3,0:3)
    complex*16 :: trafo(RankToSize(4,r),RankToSize(4,r)), elem
    integer :: str4, str5, inds1(r), inds2(r)
    integer :: i, j, k

    str4 = RankToSize(4,r)
    str5 = RankToSize(5,r)
    trafo = 0

    do i=1,str4
      inds1 = IndCombisEq(1:r,i,r,4)-1

      do j=0,str5
        inds2 = IntToIndices(1:r,j,r)

        elem = 1
        do k=1,r
          elem = elem * AtoB(inds1(k),inds2(k))
        end do
        trafo(IndToSymInd(j),i) = trafo(IndToSymInd(j),i) + elem        

      end do

    end do

  end function CalcTrafoMatrix





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function TransformTensor_STtoLC(r,tensor)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function TransformTensor_STtoLC(r,tensor) result(Tres)

    integer, intent(in) :: r
    complex*16, intent(in) :: tensor(RankToSize(4,r))
    complex*16 :: Tres(RankToSize(4,r))
    integer :: str, i, j

    Tres = 0
    str = RankToSize(4,r)

    do i=1,str
      do j=1,str
        Tres(i) = Tres(i) + Trafo_STtoLC(j,i,r)*tensor(j)
      end do
    end do
    
  end function TransformTensor_STtoLC





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function TransformTensor_LCtoST(r,tensor)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function TransformTensor_LCtoST(r,tensor) result(Tres)

    integer, intent(in) :: r
    complex*16, intent(in) :: tensor(RankToSize(4,r))
    complex*16 :: Tres(RankToSize(4,r))
    integer :: str, i, j

    Tres = 0
    str = RankToSize(4,r)

    do i=1,str
      do j=1,str
        Tres(i) = Tres(i) + Trafo_LCtoST(j,i,r)*tensor(j)
      end do
    end do
    
  end function TransformTensor_LCtoST





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcFScalar_LC(fvec1,fvec2) 
  ! 
  !  calculates the scalar product of the four-vectors fvec1 
  !  and fvec2 given in lightcone coordinates
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcFScalar_LC(fvec1,fvec2)

    complex*16, dimension(0:3), intent(in) :: fvec1, fvec2 
    complex*16 :: CalcFScalar_LC

    CalcFScalar_LC = (fvec1(0)*fvec2(1) + fvec1(1)*fvec2(0) - &
       & fvec1(2)*fvec2(3) - fvec1(3)*fvec2(2))/2d0

  end function CalcFScalar_LC





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcFNorm2_LC(fvec)
  ! 
  !  calculates the norm^2 of the four-vector fvec
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcFNorm2_LC(fvec)

    complex*16, dimension(0:3), intent(in) :: fvec 
    complex*16 :: CalcFNorm2_LC

    CalcFNorm2_LC = fvec(0)*fvec(1) - fvec(2)*fvec(3)

  end function CalcFNorm2_LC





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTensorNr_LC(N,r,MomVec,masses2,MomInv)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTensorTNr_LC(N,r,MomP,masses2,MomInv) result(TNr)

    integer, intent(in) :: N, r
    complex*16, dimension(0:3,N-1), intent(in) :: MomP
    complex*16, dimension(0:N-1), intent(in) :: masses2
    complex*16, dimension(BinomTable(2,N)) :: MomInv
    complex*16, allocatable :: TNr(:,:), Coefs(:,:,:)
    complex*16 :: MomTenInit(1)
    integer :: rts, kmax, bino, Nm1
    
    Nm1 = N-1
    rts = RankToSize(4,r)
    bino = BinomTable(r,r+N-2)
    kmax = r/2

    allocate(TNr(1:rts,0:r))
    allocate(Coefs(bino,0:kmax,0:r))

    Coefs = CalcTNrPVco(N,r,masses2,MomInv)
    TNr = 0
    MomTenInit = 1

    call AddToTNr_LC(Nm1,r,0,1,TNr,Coefs,MomTenInit,MomP)
    
  end function CalcTensorTNr_LC 





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddToTNr_LC(Nm1,rmax,r,ir,TNr,Coefs,MomTenRec,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine AddToTNr_LC(Nm1,rmax,r,ir,TNr,Coefs,MomTenRec,MomVec)

    integer, intent(in) :: Nm1, rmax, r, ir
    complex*16, intent(in) :: MomVec(0:3,Nm1), Coefs(:,0:,0:), MomTenRec(1:RankToSize(4,r))
    complex*16, intent(inout) :: TNr(1:RankToSize(4,rmax),0:rmax)
    complex*16 :: MomTen(1:RankToSize(4,r+1)), Pmu
    integer :: i,jr,k,rts,r2k,a,mu,cnt

    rts = RankToSize(4,r)
    TNr(1:rts,r) = TNr(1:rts,r) + Coefs(ir,0,r)*MomTenRec

    do k=1,(rmax-r)/2
      r2k = r+2*k
      do a = 1,rts
        cnt = 1
        do while ((cnt.le.BinomTable(rmax/2,3+rmax/2)).and.(TStruc_LC(0,cnt,a,k,r).ne.0))
          TNr(TStruc_LC(0,cnt,a,k,r),r2k) = TNr(TStruc_LC(0,cnt,a,k,r),r2k) + &
                & Coefs(ir,k,r2k)*MomTenRec(a)*TStruc_LC(1,cnt,a,k,r)
          cnt = cnt+1
        end do
      end do
    end do

        
    if (r<rmax) then

      if (r.eq.0) then
       
        do i=1,Nm1
          call AddToTNr_LC(Nm1,rmax,1,i,TNr,Coefs,MomVec(0:3,i),MomVec)
        end do
       

      else

        do i=1,Nm1
          jr = IndsPiProd(0,i,ir,Nm1)

          cnt = 1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = rts-BinomTable(r,r+3-mu)+1, rts
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTNr_LC(Nm1,rmax,r+1,jr,TNr,Coefs,MomTen,MomVec)

        end do

      end if

    end if

  end subroutine AddToTNr_LC
  

end module LightCone