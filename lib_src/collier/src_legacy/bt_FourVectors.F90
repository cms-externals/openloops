!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ************************
!  *  module FourVectors  *
!  *    by Lars Hofer     *
!  ************************
!
!  global constant:
!  MetricTensor, MetricTensorSym, MetricTensorDiag
!
!  global variable:
!  MetricTensorProd
!
!  functions and subroutines:
!  SetMetricTensorProd, CalcMetricTensorProd, 
!  CalcFScalar, CalcFNorm2, CalcFDiff
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module FourVectors

  use ColiCombinatorics
  use TensorManipulations
  

  implicit none


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  constants MetricTensor, MetricTensorSym, MetricTensorDiag
  !
  !  represents the metric tensor g^(mu,nu) = diag(1,-1,-1,-1) in the form
  !  MetricTensor = (g^00,g^01,g^02,g^02,g^10,...,g^33)
  !  MetricTensorSym = (g^00,g^01,g^02,g^03,g^11,g^12,g^13,g^22,g^23,g^33)
  !  MetricTensorDiag = (g^00,g^11,g^22,g^33)
  !
  !  global variable MetricTensorProd
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  complex*16, dimension(0:15), parameter :: MetricTensor = (/ 1d0, 0d0, 0d0, 0d0, &
                &  0d0, -1d0, 0d0, 0d0, 0d0, 0d0, -1d0, 0d0, 0d0, 0d0, 0d0, -1d0 /)
  complex*16, dimension(1:10), parameter :: MetricTensorSym = (/ 1d0, 0d0, 0d0, 0d0, &
                &  -1d0, 0d0, 0d0, -1d0, 0d0, -1d0 /)
  complex*16, dimension(0:3), parameter :: MetricTensorDiag = (/ 1d0, -1d0, -1d0, -1d0 /)
  
  complex*16, dimension(:,:), allocatable :: MetricTensorProd
  
  
contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMetricTensorProd(kmax)
  !
  !  sets the variable MetricTensorProd to the value CalcMetricTensorProd(kmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMetricTensorProd(kmax)
  
    integer, intent(in) :: kmax
    
    if (allocated(MetricTensorProd)) then
      deallocate(MetricTensorProd)
    end if
    allocate(MetricTensorProd(RankToSize(4,2*kmax),0:kmax))
    MetricTensorProd = 0
    
    MetricTensorProd(1:RankToSize(4,2*kmax),0:kmax) = CalcMetricTensorProd(kmax)
    
  end subroutine
    




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcMetricTensorProd(kmax)
  !
  !  calculates the vector (G0,G1,...,Gkmax) of the symmetrized tensors
  !  G0 = 1,   G1 = g^{mu,nu},   
  !  G2 = g^{mu,nu}g^{rho,sigma} + g^{mu,rho}g^{nu,sigma} + g^{mu,sigma}g^{nu,rho}   
  !  
  !  the tensors are given as vectors using symmetric indices
  !  e.g. g^{mu,nu} = (g^00,g^01,g^02,g^03,g^11,g^12,g^13,g^22,g^23,g^33)
  !
  !  so that the result of CalcMetricTensorProd(kmax) has the form
  !  MTP = ((G0_1,0,...,0),
  !         (G1_1,...,G1_10,0,...,0),
  !         ...
  !         (Gkmax_1,...,Gkmax_imax(kmax)))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function CalcMetricTensorProd(kmax) result(MTP)
  
    integer, intent(in) :: kmax
    complex*16, dimension(:,:), allocatable :: MTP
    complex*16, dimension(:,:), allocatable :: MTPm1
    complex*16, dimension(:), allocatable :: MTPmu0
    complex*16, dimension(4) :: Gmu0
    integer :: mu0, mu, i, i0, pos
    
    allocate(MTP(RankToSize(4,2*kmax),0:kmax))
    MTP = 0
    if (kmax.eq.0) then
      ! case k=0
      MTP(1,0) = 1
    
    else
      allocate(MTPm1(RankToSize(4,2*kmax-2),0:(kmax-1)))
      allocate(MTPmu0(RankToSize(4,2*kmax-1)))      

      MTPm1 = CalcMetricTensorProd(kmax-1)
      MTP(1:RankToSize(4,2*(kmax-1)),0:(kmax-1)) = MTPm1
      
      pos = 1
      i0 = 1
      do mu0=0,3
      
        do mu=0,3
          Gmu0(mu+1) = MetricTensor(4*mu0+mu)
        end do

        MTPmu0 = CalcSymTensorProd2(1,2*kmax-2,0,kmax-1, &
              &  Gmu0,MTPm1(:,kmax-1))
                  
        do i=i0,RankToSize(4,2*kmax-1)
          MTP(pos,kmax) = MTPmu0(i)
          pos = pos+1
        end do
        i0 = i0 + BinomTable(2*kmax-2,1-mu0+2*kmax)
        
      end do
    
    end if
    
  end function CalcMetricTensorProd





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcFScalar(fvec1,fvec2) 
  ! 
  !  calculates the scalar product of the four-vectors fvec1 
  !  and fvec2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcFScalar(fvec1,fvec2)

    complex*16, dimension(0:3), intent(in) :: fvec1, fvec2 
    complex*16 :: CalcFScalar

    CalcFScalar = fvec1(0)*fvec2(0) - fvec1(1)*fvec2(1) - &
       & fvec1(2)*fvec2(2) - fvec1(3)*fvec2(3)

  end function CalcFScalar





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcFNorm2(fvec)
  ! 
  !  calculates the norm^2 of the four-vector fvec
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcFNorm2(fvec)

    complex*16, dimension(0:3), intent(in) :: fvec 
    complex*16 :: CalcFNorm2

    CalcFNorm2 = CalcFScalar(fvec,fvec)

  end function CalcFNorm2
    
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcFDdiff(fvec1,fvec2)
  !
  !  calculates the difference of two four-vectors fvec1, fvec2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcFDiff(fvec1,fvec2)

    complex*16, dimension(0:3), intent(in) :: fvec1, fvec2 
    complex*16, dimension(0:3) :: CalcFDiff
    integer :: i
    
    do i=0,3
      CalcFDiff(i) = fvec1(i) - fvec2(i)
    end do

  end function CalcFDiff
  
  
end module FourVectors