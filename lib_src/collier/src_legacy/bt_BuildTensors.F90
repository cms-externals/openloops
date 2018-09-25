!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  *************************
!  *  module BuildTensors  *
!  *    by Lars Hofer      *
!  *************************
!
!  global variables:
!  MomDecoMatrix, IndsPiProd 
! 
!  functions and subroutines:
!  SetMomDecoMatrix, CalcMomDecoMatrix, DecomposeMomProd,
!  SetIndsPiProd, CalcIndsPiProd, CalcMomTensorMatrix, CalcTensorBasis,
!  CalcTensorBasisG, CalcTensorBasis2, CalcTensorBasis2_nd, CalcTg, CalcTp,
!  BuildTensorNr, BuildTensorNr_nd, CalcTensorBasisGconP, CalcTensorBasisGconP_nd
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module BuildTensors

  use ColiCombinatorics
  use FourVectors
  use MatrixManipulations
  use TensorManipulations

  implicit none
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  global variables IndsPiProd
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  integer, dimension(:,:,:,:), allocatable :: IndsPiProd
  integer, dimension(:,:,:,:,:), allocatable :: TStruc
  integer, dimension(:,:,:,:,:), allocatable :: TStrucG

contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine init_tables(N,r)
  !
  !  initialize tables for calculations up to N-point functions and up to rank r
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine init_tables(N,r)
  
    integer, intent(in) :: N, r
    
    call SetBinomTable(2*max(N,r))
    call SetIndCombis(N)
    call SetIndCombisEq(max(N,4),r)
    call SetRankToSize(r)
    call SetRankToSizeSiMu(r)
    call SetIntToIndices(r)
    call SetIndToSymInd(r)
    call SetIndsTensorProd1(r)
    call SetIndsTensorProd2(r)
    call SetSingleInds(r)
    call SetMetricTensorProd(r/2)
    call SetDropIndCombisEq(N,N)
    call SetIndsPiProd(N,r)
    call SetTStruc(r)
    call SetTStrucG(r)
    
  end subroutine init_tables




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndsPiProd(Nmax,rmax)
  !
  !  sets the global variable IndsPiProd to the table
  !  IndsPiProd = (CIPP(1,rmax),...,CIPP(Nmax,rmax))
  !  CIPP = CalcIndsPiProd
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndsPiProd(Nm1max,rmax)
  
    integer, intent(in) :: Nm1max, rmax
    integer :: strmax, Nm1
    
    strmax = BinomTable(rmax,rmax+Nm1max-1)
    if (allocated(IndsPiProd)) then
      deallocate(IndsPiProd)
    end if
    allocate(IndsPiProd(0:1,Nm1max,strmax,Nm1max))
    IndsPiProd = 0
    
    do Nm1=1,Nm1max
      strmax = BinomTable(rmax,rmax+Nm1-1)
      IndsPiProd(0:1,1:Nm1,1:strmax,Nm1) = CalcIndsPiProd(Nm1,rmax)
    end do
    
  end subroutine SetIndsPiProd
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndsPiProd(Nm1,r)
  !
  !  calculates resulting indices for product of momentum
  !  tensor (p_i1,...,p_ik) with p_j (im,j = 1,...,Nm1).
  !  Further the number of indices equal to j in the final
  !  result is given.
  !
  !  example: Nm1=2, r=3
  !  momentum tensors: 
  !  rank3: 1: p1p1p1,  2: p1p1p2,  3: p1p2p2,  4: p2p2p2
  !  rank4: 1: p1p1p1p1,  2: p1p1p1p2,  3: p1p1p2p2,  
  !         4: p1p2p2p2,  5: p2p2p2p2
  !
  !  IndsPiProd(2,1,0:1) = (2,3),   IndsPiProd(2,2,0:1) = (3,2)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndsPiProd(Nm1,r) result(IndsPiProd)
  
    integer, intent(in) :: Nm1, r
    integer, dimension(:,:,:), allocatable :: IndsPiProd
    integer :: str, i, j, inds(r), indsp1(r+1), pos, cnt

    str = BinomTable(r,r+Nm1-1)
    allocate(IndsPiProd(0:1,1:Nm1,1:str))
    
    do i=1,str
      inds = IndCombisEq(1:r,i,r,Nm1)
      
      pos = 1
      do j=1,Nm1
        
        cnt = 1
        do while (pos.le.r)
          if (inds(pos)>j) then
            exit
          else if (inds(pos).eq.j) then
            cnt = cnt+1
          end if
          pos = pos+1
        end do
        
        indsp1(1:pos-1) = inds(1:pos-1)
        indsp1(pos) = j
        indsp1(pos+1:r+1) = inds(pos:r)

        IndsPiProd(0,j,i) = CalcPosIndCombisEq(Nm1,r+1,indsp1)
        IndsPiProd(1,j,i) = cnt
        
      end do
      
    end do
  
  end function CalcIndsPiProd





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTStruc(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetTStruc(rmax)
  
    integer, intent(in) :: rmax

    if (allocated(TStruc)) then
      deallocate(TStruc)
    end if
    allocate(TStruc(0:1,BinomTable(rmax/2,3+rmax/2),RankToSize(4,rmax),rmax/2,0:rmax))

    TStruc = CalcTStruc(rmax)

  end subroutine SetTStruc





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTStruc(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTStruc(rmax)

    integer, intent(in) :: rmax
    integer :: CalcTStruc(0:1,BinomTable(rmax/2,3+rmax/2),RankToSize(4,rmax),rmax/2,0:rmax)
    integer :: r, k, str, stk, cnt

    CalcTStruc = 0
    do r=0,rmax
      do k=1,(rmax-r)/2
        do str=1,RankToSize(4,r)
          cnt = 1
          do stk=1,RankToSize(4,2*k)
            if (MetricTensorProd(stk,k).ne.0d0) then
              CalcTStruc(0,cnt,str,k,r) = IndsTensorProd2(1,stk,str,2*k,r)
              CalcTStruc(1,cnt,str,k,r) = MetricTensorProd(stk,k)*IndsTensorProd2(0,stk,str,2*k,r)
              cnt = cnt+1
            end if
          end do
        end do
      end do
    end do
  end function CalcTStruc
  




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTStrucG(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetTStrucG(rmax)
  
    integer, intent(in) :: rmax

    if (allocated(TStrucG)) then
      deallocate(TStrucG)
    end if
    allocate(TStrucG(0:1,BinomTable(rmax/2,3+rmax/2),RankToSize(4,rmax),rmax/2,0:rmax))

    TStrucG = CalcTStrucG(rmax)

  end subroutine SetTStrucG





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTStrucG(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTStrucG(rmax)

    integer, intent(in) :: rmax
    integer :: CalcTStrucG(0:1,BinomTable(rmax/2,3+rmax/2),RankToSize(4,rmax),rmax/2,0:rmax)
    integer, dimension(:), allocatable :: indsT, indsG
    integer :: r, k, str, stk, cnt, nT, nG, mu, la, i, j, a, bino, fac, div

    CalcTStrucG = 0
    CalcTStrucG(0:1,:,:,:,0) = TStruc(0:1,:,:,:,0)

    do r=1,rmax
      if (allocated(indsT)) then
        deallocate(indsT)
      end if
      allocate(indsT(r))

      do k=1,(rmax-r)/2
        if (allocated(indsG)) then
          deallocate(indsG)
        end if       
        allocate(indsG(2*k))

        str = 1
        do mu=0,3
          do i=1,BinomTable(r-1,2+r-mu)

            indsT = IndCombisEq(1:r,str,r,4)

            cnt = 1
            stk = 1
            do la=0,mu
              do j=1,BinomTable(2*k-1,2+2*k-la)
       
                indsG = IndCombisEq(1:2*k,stk,2*k,4)

                if ((MetricTensorProd(stk,k).ne.0d0)) then

                  if (la.eq.mu) then
                    nT = 1
                    do a=2,r
                      if (indsT(a).eq.indsT(1)) then
                        nT = nT+1
                      end if
                    end do

                    nG = 1
                    do a=2,2*k
                      if (indsG(a).eq.indsG(1)) then
                        nG = nG+1
                      end if
                    end do
                    
                    fac = BinomTable(nT,nT+nG-1)
                    div = BinomTable(nT,nT+nG)
                  
                  else
                    fac = 1
                    div = 1

                  end if

                  CalcTStrucG(0,cnt,str,k,r) = IndsTensorProd2(1,stk,str,2*k,r)
                  CalcTStrucG(1,cnt,str,k,r) = MetricTensorProd(stk,k)*IndsTensorProd2(0,stk,str,2*k,r)*fac/div
              
                  cnt = cnt+1

                end if

                stk = stk+1
              
              end do
            end do

            str = str+1

          end do
        end do

      end do

    end do

  end function CalcTStrucG





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTensorTNr(N,r,MomVec,masses2,MomInv)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTensorTNr(N,r,MomP,masses2,MomInv) result(TNr)

    integer, intent(in) :: N, r
    complex*16, dimension(0:3,N-1), intent(in) :: MomP
    complex*16, dimension(0:N-1), intent(in) :: masses2
    complex*16, dimension(BinomTable(2,N)), intent(in) :: MomInv
    complex*16, allocatable :: TNr(:,:), Coefs(:,:,:)
    integer :: rts, kmax, bino, Nm1
    
    Nm1 = N-1
    rts = RankToSize(4,r)
    bino = BinomTable(r,r+N-2)
    kmax = r/2

    allocate(TNr(1:rts,0:r))
    allocate(Coefs(bino,0:kmax,0:r))

    Coefs = CalcTNrPVco(N,r,masses2,MomInv)
    TNr = 0


    call AddToTNr_0(Nm1,r,TNr,Coefs,MomP)
    
  end function CalcTensorTNr 





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddToTNr_0(Nm1,rmax,TNr,Coefs,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine AddToTNr_0(Nm1,rmax,TNr,Coefs,MomVec)

    integer, intent(in) :: Nm1, rmax
    complex*16, intent(in) :: MomVec(0:3,Nm1), Coefs(:,0:,0:)
    complex*16, intent(inout) :: TNr(1:RankToSize(4,rmax),0:rmax)
    integer :: i,k,r2k,stk

    TNr(1,0) = Coefs(1,0,0)

    do k=1,rmax/2
      r2k = 2*k
      stk = RankToSize(4,r2k)
      TNr(1:stk,r2k) = Coefs(1,k,r2k)*MetricTensorProd(1:stk,k)
    end do
        
    if (rmax>0) then

      do i=1,Nm1
        call AddToTNr(Nm1,rmax,1,i,TNr,Coefs,MomVec(0:3,i),MomVec)
      end do
        
    end if


  end subroutine AddToTNr_0





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddToTNr(Nm1,rmax,r,ir,TNr,Coefs,MomTenRec,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine AddToTNr(Nm1,rmax,r,ir,TNr,Coefs,MomTenRec,MomVec)

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
        do while ((cnt.le.BinomTable(rmax/2,3+rmax/2)).and.(TStruc(0,cnt,a,k,r).ne.0))
          TNr(TStruc(0,cnt,a,k,r),r2k) = TNr(TStruc(0,cnt,a,k,r),r2k) + &
                & Coefs(ir,k,r2k)*MomTenRec(a)*TStruc(1,cnt,a,k,r)
          cnt = cnt+1
        end do
      end do
    end do

        
    if (r<rmax) then

      if (r.eq.0) then
       
        do i=1,Nm1
          call AddToTNr(Nm1,rmax,1,i,TNr,Coefs,MomVec(0:3,i),MomVec)
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

          call AddToTNr(Nm1,rmax,r+1,jr,TNr,Coefs,MomTen,MomVec)

        end do

      end if

    end if

  end subroutine AddToTNr





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTNrPVco(N,r,masses2,MomInv)
  ! 
  !  calculates the tensor coefficients T_{0...0,i1,...,ik}
  !  based on Ansgar's functions cBp12345, cCp12345,cDp12345,
  !  cEp1234,cFp1234 in the COLI library
  !
  !  MomInv = (I(1,1),...,I(1,N-1),I(2,2),...,I(2,N-1),...,I(N-1,N-1)
  !  with I(i,i)=pi^2  and I(i,j)=(pi-pj)^2 for i/=j
  !
  !  result is saved as array with indices representing
  !  symmetric momentum index, number of metric tensors, rank
  !
  !  example: N=4, r=3
  !  TNrco = (((D_0, 0, ..., 0), (0, ..., 0)),
  !           ((D_1, D_2, D_3, 0, ..., 0), (0, ..., 0)),
  !           ((D_11, D_12, D_13, D_22, D_23, D_33, 0, 0, 0, 0), (D_00, 0, ..., 0)),
  !           ((D_111, D_112, D_113, D_122, D_123, D_133, D_222, D_223, D_233, D_333),
  !            (D_001, D_002, D_003, 0, ..., 0)))            
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTNrPVco(N,r,masses2,MomInv)

    integer, intent(in) :: N, r
    complex*16, dimension(BinomTable(2,N)), intent(in) :: MomInv
    complex*16, dimension(0:N-1), intent(in) :: masses2
    complex*16, dimension(:,:,:), allocatable :: CalcTNrPVco
    complex*16 :: p10, p21, p31, p41, p51, p20, p32, p42, p52
    complex*16 :: p30, p43, p53, p40, p54, p50
    integer :: i, j, k, kmax, BinoMax, im2k
    
    complex*16 :: Aco, olA0
    complex*16, dimension(:,:,:,:,:,:,:), allocatable :: B0123456, C0123456
    complex*16, dimension(:,:,:,:,:,:), allocatable :: D012345
    complex*16, dimension(:,:,:,:,:,:), allocatable :: E012345, F012345
    integer, dimension(:), allocatable :: inds
    
    kmax = r/2
    
    select case (N)
    
      case (1)
      
        allocate(CalcTNrPVco(1,0:kmax,0:r))
        CalcTNrPVco = 0
      
        Aco = olA0(masses2(0))
        CalcTNrPVco(1,0,0) = Aco
        
        do i=1,kmax
          Aco = Aco + masses2(0)/(i+1)
          CalcTNrPVco(1,i,2*i) = Aco*(masses2(0)**(2*i))/((2**i)*CalcFactorial(i+1))
        end do
        
    
      case (2)
      
        allocate(inds(6))
        allocate(B0123456(0:6,0:1,0:1,0:1,0:1,0:1,0:1))
        B0123456 = 0
        allocate(CalcTNrPVco(1,0:kmax,0:r))
        CalcTNrPVco = 0
      
        p10 = MomInv(1)        
      
        call olBp12345(p10,masses2(0),masses2(1),B0123456(0,0,0,0,0,0,0), &
            & B0123456(1,1,0,0,0,0,0),B0123456(2,0:1,0:1,0,0,0,0),B0123456(3,0:1,0:1,0:1,0,0,0), &
              B0123456(4,0:1,0:1,0:1,0:1,0,0),B0123456(5,0:1,0:1,0:1,0:1,0:1,0), &
            & B0123456(6,0:1,0:1,0:1,0:1,0:1,0:1),r,0)
            
        ! scalar coefficient
        CalcTNrPVco(1,0,0) = B0123456(0,0,0,0,0,0,0)
        
        do i=1,r
          ! non-existing indices are set to zero
          inds = 0
          if (mod(i,2).eq.0) then
            CalcTNrPVco(1,i/2,i) = B0123456(i,inds(1),inds(2),inds(3),inds(4),inds(5),inds(6))
          end if
        
          do k=(i-1)/2,0,-1 
            im2k = i-2*k            
          
            ! value 1 for indices 2k+1,...,i
            inds(2*k+1:i) = 1

            ! result
            CalcTNrPVco(1,k,i) = B0123456(i,inds(1),inds(2),inds(3),inds(4),inds(5),inds(6))
            
          end do
          
        end do   
        
        
      case (3)
      
        allocate(inds(6))
        allocate(C0123456(0:6,0:2,0:2,0:2,0:2,0:2,0:2))
        C0123456 = 0
        allocate(CalcTNrPVco(r+1,0:kmax,0:r))
        CalcTNrPVco = 0
      
        p10 = MomInv(1)
        p21 = MomInv(2)
        p20 = MomInv(3)

        call olCp12345(p10,p21,p20,masses2(0),masses2(1),masses2(2), &
            & C0123456(0,0,0,0,0,0,0),C0123456(1,1:2,0,0,0,0,0),C0123456(2,0:2,0:2,0,0,0,0), &
            & C0123456(3,0:2,0:2,0:2,0,0,0),C0123456(4,0:2,0:2,0:2,0:2,0,0),C0123456(5,0:2,0:2,0:2,0:2,0:2,0), &
            & C0123456(6,0:2,0:2,0:2,0:2,0:2,0:2),r)
        
        ! scalar coefficient
        CalcTNrPVco(1,0,0) = C0123456(0,0,0,0,0,0,0)
        
        do i=1,r
          ! non-existing indices are set to zero
          inds = 0
          if (mod(i,2).eq.0) then
            CalcTNrPVco(1,i/2,i) = C0123456(i,inds(1),inds(2),inds(3),inds(4),inds(5),inds(6))
          end if
        
          do k=(i-1)/2,0,-1            
            im2k = i-2*k            
          
            BinoMax = 1+im2k
            do j=1,BinoMax
              ! value 1,2 for indices 2k+1,...,i
              inds(2*k+1:i) = IndCombisEq(1:im2k,j,im2k,2)
              ! result
              CalcTNrPVco(j,k,i) = C0123456(i,inds(1),inds(2),inds(3),inds(4),inds(5),inds(6))
            end do
            
          end do
          
        end do     


      case (4)
      
        allocate(inds(5))
        allocate(D012345(0:5,0:3,0:3,0:3,0:3,0:3))
        D012345 = 0d0
        allocate(CalcTNrPVco(BinomTable(r,r+2),0:kmax,0:r))
        CalcTNrPVco = 0d0
        
        p10 = MomInv(1)
        p21 = MomInv(2)
        p32 = MomInv(5)
        p30 = MomInv(6)
        p20 = MomInv(4)
        p31 = MomInv(3)
        
        call olDp12345(p10,p21,p32,p30,p20,p31,masses2(0),masses2(1),masses2(2),masses2(3),  &
            & D012345(0,0,0,0,0,0),D012345(1,1:3,0,0,0,0),D012345(2,0:3,0:3,0,0,0), &
            & D012345(3,0:3,0:3,0:3,0,0),D012345(4,0:3,0:3,0:3,0:3,0),D012345(5,0:3,0:3,0:3,0:3,0:3), &
            & r,0)

        ! scalar coefficient
        CalcTNrPVco(1,0,0) = D012345(0,0,0,0,0,0)
        
        do i=1,r
          ! non-existing indices are set to zero
          inds = 0
          if (mod(i,2).eq.0) then
            CalcTNrPVco(1,i/2,i) = D012345(i,inds(1),inds(2),inds(3),inds(4),inds(5))
          end if
        
          do k=(i-1)/2,0,-1            
            im2k = i-2*k            
          
            BinoMax = BinomTable(im2k,2+im2k)
            do j=1,BinoMax
              ! value 1,2,3 for indices 2k+1,...,i
              inds(2*k+1:i) = IndCombisEq(1:im2k,j,im2k,3)
              ! result
              CalcTNrPVco(j,k,i) = D012345(i,inds(1),inds(2),inds(3),inds(4),inds(5))
            end do
            
          end do
          
        end do 
      
        
      case (5)
      
        allocate(inds(5))
        allocate(E012345(0:5,0:4,0:4,0:4,0:4,0:4))
        E012345 = 0
        allocate(CalcTNrPVco(BinomTable(r,r+3),0:kmax,0:r))
        CalcTNrPVco = 0
        
        p10 = MomInv(1)
        p21 = MomInv(2)
        p32 = MomInv(6)
        p43 = MomInv(9)
        p40 = MomInv(10)
        p20 = MomInv(5)
        p31 = MomInv(3)
        p42 = MomInv(7)
        p30 = MomInv(8)
        p41 = MomInv(4)
        
        call olEp12345(p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
            & masses2(0),masses2(1),masses2(2),masses2(3),masses2(4), &
            & E012345(0,0,0,0,0,0),E012345(1,1:4,0,0,0,0),E012345(2,0:4,0:4,0,0,0), &
            & E012345(3,0:4,0:4,0:4,0,0),E012345(4,0:4,0:4,0:4,0:4,0),E012345(5,0:4,0:4,0:4,0:4,0:4),r)

        ! scalar coefficient
        CalcTNrPVco(1,0,0) = E012345(0,0,0,0,0,0)
        
        do i=1,r
          ! non-existing indices are set to zero
          inds = 0
          if (mod(i,2).eq.0) then
            CalcTNrPVco(1,i/2,i) = E012345(i,inds(1),inds(2),inds(3),inds(4),inds(5))
          end if
        
          do k=(i-1)/2,0,-1   
            im2k = i-2*k            
          
            BinoMax = BinomTable(im2k,3+im2k)
            do j=1,BinoMax
              ! value 1,2,3,4 for indices 2k+1,...,i
              inds(2*k+1:i) = IndCombisEq(1:im2k,j,im2k,4)
              ! result
              CalcTNrPVco(j,k,i) = E012345(i,inds(1),inds(2),inds(3),inds(4),inds(5))
            end do
            
          end do
          
        end do
        
        
      case (6)
      
        allocate(inds(5))
        allocate(F012345(0:5,0:5,0:5,0:5,0:5,0:5))
        F012345 = 0
        allocate(CalcTNrPVco(BinomTable(r,r+4),0:kmax,0:r))
        CalcTNrPVco = 0
        
        p10 = MomInv(1)
        p21 = MomInv(2)
        p32 = MomInv(7)
        p43 = MomInv(11)
        p54 = MomInv(14)
        p50 = MomInv(15)
        p20 = MomInv(6)
        p31 = MomInv(3)
        p42 = MomInv(8)
        p53 = MomInv(12)
        p40 = MomInv(13)
        p51 = MomInv(5)
        p30 = MomInv(10)
        p41 = MomInv(4)
        p52 = MomInv(9)
        
        call olFp12345(p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,p51,p30,p41,p52, &
            & masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),masses2(5), &
            & F012345(0,0,0,0,0,0),F012345(1,1:5,0,0,0,0),F012345(2,0:5,0:5,0,0,0), &
            & F012345(3,0:5,0:5,0:5,0,0),F012345(4,0:5,0:5,0:5,0:5,0),F012345(5,0:5,0:5,0:5,0:5,0:5),r)

        ! scalar coefficient
        CalcTNrPVco(1,0,0) = F012345(0,0,0,0,0,0)
        
        do i=1,r
          ! non-existing indices are set to zero
          inds = 0
          if (mod(i,2).eq.0) then
            CalcTNrPVco(i,i/2,1) = F012345(i,inds(1),inds(2),inds(3),inds(4),inds(5))
          end if
        
          do k=(i-1)/2,0,-1            
            im2k = i-2*k            
          
            BinoMax = BinomTable(im2k,4+im2k)
            do j=1,BinoMax
              ! value 1,2,3,4,5 for indices 2k+1,...,i
              inds(2*k+1:i) = IndCombisEq(1:im2k,j,im2k,5)
              ! result
              CalcTNrPVco(j,k,i) = F012345(i,inds(1),inds(2),inds(3),inds(4),inds(5))
            end do
            
          end do
          
        end do
        
        
      case (:0)
        write (*,*) 'BuildTensors: CalcTNrPVco'
        write (*,*) 'N=', N, ' is forbidden. N has to be positive!'
        stop
        
      case (7:)
        write (*,*) 'BuildTensors: CalcTNrPVco'
        write (*,*) 'N=', N, ' is not implemented'
        stop
        
    end select
    

  end function CalcTNrPVco


end module BuildTensors