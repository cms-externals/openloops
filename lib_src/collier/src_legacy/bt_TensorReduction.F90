!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ****************************
!  *  module TensorReduction  *
!  *      by Lars Hofer       *
!  ****************************
!
!  functions and subroutines:
!  init_tables, CalcTensorNr, CalcTNrPVco, CalcTNrPVco_nd, CalcTtilde, CalcTgtilde,
!  CalcTensorEr, EreduD0, EreduD1, EreduDr, CalcTensorFr, FreduE0, FreduEr
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module TensorReduction

  use ColiCombinatorics
  use FourVectors
  use MatrixManipulations
  use TensorManipulations
  use BuildTensors
  use GramCayley

  implicit none


contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcDrm1PVco(rm1,masses2,MomInv)
  ! 
  !  calculates the tensor coefficients D(m)_{0...0,i1,...,ik}
  !  for 5-point tensor integrals with the m'th propagator being dropped
  !  based on Ansgar's function cDp1234 in the COLI library
  !
  !  MomInv = (I(1,1),...,I(1,4),I(2,2),...,I(2,4),...,I(4,4)
  !  with I(i,i)=pi^2  and I(i,j)=(pi-pj)^2 for i/=j
  !
  !  result is saved as array with indices representing
  !  m, symmetric momentum index, number of metric tensors, rank
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcDrm1PVco(rm1,masses2,MomInv)

    integer, intent(in) :: rm1
    complex*16, dimension(10), intent(in) :: MomInv
    complex*16, dimension(0:4), intent(in) :: masses2
    complex*16, dimension(:,:,:,:), allocatable :: CalcDrm1PVco
    complex*16 :: p10, p21, p31, p41, p20, p32, p42
    complex*16 :: p30, p43, p40
    integer :: i, j, k, a, kmax, BinoMax, im2k, r
    complex*16, dimension(0:4,0:5,0:3,0:3,0:3,0:3,0:3) :: D012345
    integer :: inds(5), dropinds(4), ind1, inda, shift
    
    r = rm1+1
    kmax = r/2
    
    D012345 = 0
    allocate(CalcDrm1PVco(0:4,BinomTable(r,r+3),0:kmax,0:r))
    CalcDrm1PVco = 0
    
    p10 = MomInv(1)
    p21 = MomInv(2)
    p31 = MomInv(3)
    p41 = MomInv(4)
    p20 = MomInv(5)        
    p32 = MomInv(6)
    p42 = MomInv(7)
    p30 = MomInv(8)
    p43 = MomInv(9)
    p40 = MomInv(10)
        
    call olDp12345(p10,p21,p32,p30,p20,p31, &
        & masses2(0),masses2(1),masses2(2),masses2(3), &
        & D012345(4,0,0,0,0,0,0),D012345(4,1,1:3,0,0,0,0),D012345(4,2,0:3,0:3,0,0,0), &
        & D012345(4,3,0:3,0:3,0:3,0,0),D012345(4,4,0:3,0:3,0:3,0:3,0),D012345(4,5,0:3,0:3,0:3,0:3,0:3),rm1,1)
        
    call olDp12345(p10,p21,p42,p40,p20,p41, &
        & masses2(0),masses2(1),masses2(2),masses2(4), &
        & D012345(3,0,0,0,0,0,0),D012345(3,1,1:3,0,0,0,0),D012345(3,2,0:3,0:3,0,0,0), &
        & D012345(3,3,0:3,0:3,0:3,0,0),D012345(3,4,0:3,0:3,0:3,0:3,0),D012345(3,5,0:3,0:3,0:3,0:3,0:3),rm1,1)

    call olDp12345(p10,p31,p43,p40,p30,p41, &
        & masses2(0),masses2(1),masses2(3),masses2(4), &
        & D012345(2,0,0,0,0,0,0),D012345(2,1,1:3,0,0,0,0),D012345(2,2,0:3,0:3,0,0,0), &
        & D012345(2,3,0:3,0:3,0:3,0,0),D012345(2,4,0:3,0:3,0:3,0:3,0),D012345(2,5,0:3,0:3,0:3,0:3,0:3),rm1,1)
        
    call olDp12345(p20,p32,p43,p40,p30,p42, &
        & masses2(0),masses2(2),masses2(3),masses2(4), &
        & D012345(1,0,0,0,0,0,0),D012345(1,1,1:3,0,0,0,0),D012345(1,2,0:3,0:3,0,0,0), &
        & D012345(1,3,0:3,0:3,0:3,0,0),D012345(1,4,0:3,0:3,0:3,0:3,0),D012345(1,5,0:3,0:3,0:3,0:3,0:3),rm1,1)

    call olDp12345(p21,p32,p43,p41,p31,p42, &
        & masses2(1),masses2(2),masses2(3),masses2(4), &
        & D012345(0,0,0,0,0,0,0),D012345(0,1,1:3,0,0,0,0),D012345(0,2,0:3,0:3,0,0,0), &
        & D012345(0,3,0:3,0:3,0:3,0,0),D012345(0,4,0:3,0:3,0:3,0:3,0),D012345(0,5,0:3,0:3,0:3,0:3,0:3),rm1,1) 



    ! scalar coefficient
    CalcDrm1PVco(0:4,1,0,0) = D012345(0:4,0,0,0,0,0,0)
        
    do i=1,rm1
      ! non-existing indices are set to zero
      inds = 0
      if (mod(i,2).eq.0) then
        CalcDrm1PVco(0:4,1,i/2,i) = D012345(0:4,i,inds(1),inds(2),inds(3),inds(4),inds(5))
      end if
        
      do k=(i-1)/2,0,-1 
        im2k = i-2*k            
          
        BinoMax = BinomTable(im2k,2+im2k)
        shift = BinomTable(im2k-1,2+im2k)

        do j=1,BinoMax
          ! value 1,2,3,4 for indices 2k+1,...,i
          inds(2*k+1:i) = IndCombisEq(1:im2k,j,im2k,3)
          dropinds(1:4) = DropIndCombisEq(j,1:4,1,im2k,4)

          ! result
          CalcDrm1PVco(0,j+shift,k,i) = D012345(0,i,inds(1),inds(2),inds(3),inds(4),inds(5))
          do a=1,4
            CalcDrm1PVco(a,dropinds(a),k,i) = D012345(a,i,inds(1),inds(2),inds(3),inds(4),inds(5))
          end do
        end do
        
        do j=BinomTable(im2k-1,2+im2k),1,-1
          ind1 = IndsPiProd(0,1,j,4)
          CalcDrm1PVco(0,ind1,k,i) = - CalcDrm1PVco(0,j,k,i-1)
          do a=2,4
            inda = IndsPiProd(0,a,j,4)
            CalcDrm1PVco(0,ind1,k,i) = CalcDrm1PVco(0,ind1,k,i) - CalcDrm1PVco(0,inda,k,i)
          end do
        end do

      end do

    end do

    ! special case rm1+1
    ! non-existing indices are set to zero
    inds = 0
    if (mod(r,2).eq.0) then
      CalcDrm1PVco(0:4,1,r/2,r) = D012345(0:4,r,inds(1),inds(2),inds(3),inds(4),inds(5))
    end if
        
    do k=(r-1)/2,1,-1
      im2k = r-2*k            
          
      BinoMax = BinomTable(im2k,2+im2k)
      shift = BinomTable(im2k-1,2+im2k)

      do j=1,BinoMax
        ! value 1,2,3,4 for indices 2k+1,...,i
        inds(2*k+1:r) = IndCombisEq(1:im2k,j,im2k,3)
        dropinds(1:4) = DropIndCombisEq(j,1:4,1,im2k,4)

        ! result
        CalcDrm1PVco(0,j+shift,k,r) = D012345(0,r,inds(1),inds(2),inds(3),inds(4),inds(5))
        do a=1,4
          CalcDrm1PVco(a,dropinds(a),k,r) = D012345(a,r,inds(1),inds(2),inds(3),inds(4),inds(5))
        end do
      end do
        
      do j=BinomTable(im2k-1,2+im2k),1,-1
        ind1 = IndsPiProd(0,1,j,4)
        CalcDrm1PVco(0,ind1,k,r) = - CalcDrm1PVco(0,j,k,r-1)
        do a=2,4
          inda = IndsPiProd(0,a,j,4)
          CalcDrm1PVco(0,ind1,k,r) = CalcDrm1PVco(0,ind1,k,r) - CalcDrm1PVco(0,inda,k,r)
        end do
      end do
            
    end do    

  end function CalcDrm1PVco





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorDrm1(rm1,MomVec,masses2,MomInv,Drm1,DrG)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorDrm1(rm1,MomP,masses2,MomInv,Drm1,DrG)

    integer, intent(in) :: rm1
    complex*16, dimension(0:3,4), intent(in) :: MomP
    complex*16, dimension(0:4), intent(in) :: masses2
    complex*16, dimension(10), intent(in) :: MomInv
    complex*16, dimension(:,:,:), allocatable, intent(out) :: Drm1, DrG
    complex*16, allocatable :: Coefs(:,:,:,:)
    integer :: rm2, r, rts, rtsG, kmax, bino
    
    rm2 = rm1-1
    r = rm1+1
    rts = RankToSize(4,rm1)
    bino = BinomTable(r,r+3)
    rtsG = RankToSize(4,rm2)
    kmax = r/2

    allocate(Drm1(1:rts,0:4,0:rm1))
    Drm1 = 0
    allocate(DrG(1:rtsG,0:4,0:rm2))
    DrG = 0
    allocate(Coefs(0:4,bino,0:kmax,0:r))

    Coefs = CalcDrm1PVco(rm1,masses2,MomInv)

    call AddToDrm1_0(rm1,Drm1,DrG,Coefs,MomP)
    
  end subroutine CalcTensorDrm1





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddToDrm1_0(rmax,Drm1,DrG,Coefs,MomTenRec,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine AddToDrm1_0(rmax,Drm1,DrG,Coefs,MomVec)

    integer, intent(in) :: rmax
    complex*16, intent(in) :: MomVec(0:3,4), Coefs(0:,:,0:,0:)
    complex*16, intent(out) :: Drm1(1:RankToSize(4,rmax),0:4,0:rmax) 
    complex*16, dimension(:,:,:), allocatable, intent(out) :: DrG
    logical :: flag0(4)
    integer :: i,r,k,stk,stkm1

    Drm1(1,0,0) = Coefs(0,1,0,0)
    do i=1,4
        Drm1(1,i,0) = Coefs(i,1,0,0)
    end do
    
    do k=1,rmax/2
      r=2*k
      stk = RankToSize(4,r)
      Drm1(1:stk,0,r) = Coefs(0,1,k,r)*MetricTensorProd(1:stk,k)
      do i=1,4
        Drm1(1:stk,i,r) = Coefs(i,1,k,r)*MetricTensorProd(1:stk,k)
      end do
    end do
        
    if (rmax>0) then
    
      allocate(DrG(1:RankToSize(4,rmax-1),0:4,0:rmax-1))
      
      do k=0,(rmax-1)/2
        r=2*k
        stk = RankToSize(4,r)
        DrG(1:stk,0,r) = Coefs(0,1,k+1,r+2)*MetricTensorProd(1:stk,k)
        do i=1,4
          DrG(1:stk,i,r) = Coefs(i,1,k+1,r+2)*MetricTensorProd(1:stk,k)
        end do
      end do

      do i=1,4
        flag0 = .true.
        flag0(i) = .false.
        call AddToDrm1(rmax,1,i,Drm1,DrG,Coefs,MomVec(0:3,i),MomVec,flag0)
      end do
      
    end if 

  end subroutine AddToDrm1_0
  




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddToDrm1(rmax,r,ir,Drm1,DrG,Coefs,MomTenRec,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine AddToDrm1(rmax,r,ir,Drm1,DrG,Coefs,MomTenRec,MomVec,flag)

    integer, intent(in) :: rmax, r, ir
    complex*16, intent(in) :: MomVec(0:3,4), Coefs(0:,:,0:,0:), MomTenRec(1:RankToSize(4,r))
    complex*16, intent(inout) :: Drm1(1:RankToSize(4,rmax),0:4,0:rmax) 
    complex*16, intent(inout) :: DrG(1:RankToSize(4,rmax-1),0:4,0:rmax-1)
    logical, intent(in) :: flag(4)
    logical :: flag0(4)
    complex*16 :: MomTen(1:RankToSize(4,r+1)), Pmu
    integer :: i,j,m,jr,k,rts,r2k,a,mu,la,cnt

    rts = RankToSize(4,r)
    Drm1(1:rts,0,r) = Drm1(1:rts,0,r) + Coefs(0,ir,0,r)*MomTenRec
    do i=1,4
      if (flag(i)) then
        Drm1(1:rts,i,r) = Drm1(1:rts,i,r) + Coefs(i,ir,0,r)*MomTenRec
      end if
    end do
    

    do k=1,(rmax-r)/2
      r2k = r+2*k
      do a = 1,rts
        cnt = 1
        do while ((cnt.le.BinomTable(rmax/2,3+rmax/2)).and.(TStruc(0,cnt,a,k,r).ne.0))
          Drm1(TStruc(0,cnt,a,k,r),0,r2k) = Drm1(TStruc(0,cnt,a,k,r),0,r2k) + &
                & Coefs(0,ir,k,r2k)*MomTenRec(a)*TStruc(1,cnt,a,k,r)
          do i=1,4
           if (flag(i)) then
              Drm1(TStruc(0,cnt,a,k,r),i,r2k) = Drm1(TStruc(0,cnt,a,k,r),i,r2k) + &
                    & Coefs(i,ir,k,r2k)*MomTenRec(a)*TStruc(1,cnt,a,k,r)
            end if
          end do
          cnt = cnt+1
        end do
      end do
    end do

    
        
    if (r<rmax) then
           
      DrG(1:rts,0,r) = DrG(1:rts,0,r) + Coefs(0,ir,1,r+2)*MomTenRec(1:rts)
      do i=1,4
        if (flag(i)) then
          DrG(1:rts,i,r) = DrG(1:rts,i,r) + Coefs(i,ir,1,r+2)*MomTenRec(1:rts)
        end if
      end do
      
      do k=1,(rmax-r-1)/2
        r2k = r+2*k
        do a = 1,rts
          cnt = 1
          do while ((cnt.le.BinomTable(rmax/2,3+rmax/2)).and.(TStruc(0,cnt,a,k,r).ne.0))
            DrG(TStruc(0,cnt,a,k,r),0,r2k) = DrG(TStruc(0,cnt,a,k,r),0,r2k) + &
                & Coefs(0,ir,k+1,r2k+2)*MomTenRec(a)*TStruc(1,cnt,a,k,r)
            do i=1,4
              if (flag(i)) then
                DrG(TStruc(0,cnt,a,k,r),i,r2k) = DrG(TStruc(0,cnt,a,k,r),i,r2k) + &
                    & Coefs(i,ir,k+1,r2k+2)*MomTenRec(a)*TStruc(1,cnt,a,k,r)
              end if
            end do
            cnt = cnt+1
          end do
        end do
      end do

      do i=1,4
        flag0 = flag
        flag0(i) = .false.
        jr = IndsPiProd(0,i,ir,4)

        cnt = 1
        do mu=0,3
          Pmu = MomVec(mu,i)
          do a = rts-BinomTable(r,r+3-mu)+1, rts
            MomTen(cnt)=MomTenRec(a)*Pmu
            cnt = cnt+1
          end do
        end do

        call AddToDrm1(rmax,r+1,jr,Drm1,DrG,Coefs,MomTen,MomVec,flag0)

      end do

    end if

  end subroutine AddToDrm1


  
  
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTensorEr(r,MomP,masses2,MomInv)
  !
  !  calculate tensor integral TEr for 5-point functions by direct reduction 
  !  from the tensor integrals TDr for 4-point functions.
  !  the TDr are constructed using the 4-point coefficients calculated with COLI
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcTensorEr(r,MomP,masses2,MomInv) result(TEr)
  
    integer, intent(in) :: r
    complex*16, intent(in) :: MomP(0:3,4), masses2(0:4), MomInv(10)
    complex*16, dimension(RankToSize(4,r),0:r) :: TEr
    complex*16 :: Z(4,4), X(0:4,0:4), InvX(0:4,0:4), DetX
    complex*16 :: VecF(4)
    complex*16, dimension(:,:,:), allocatable :: TDrm1, TDrG
    integer :: i
    
    ! Gram and Cayley matrix
    Z = CalcGramMat2(4,MomInv)
    VecF = CalcVecF2(4,MomInv,masses2)
    X = CalcCayleyMat(4,masses2(0),Z,VecF)
    DetX = CalcDet(5,X)
    InvX = CalcInvMatSym(5,DetX,X)
    
    ! build 4-point tensors
    allocate(TDrm1(RankToSize(4,r-1),0:4,0:r-1))
    allocate(TDrG(RankToSize(4,r-2),0:4,0:r-2))
    call CalcTensorDrm1(r-1,MomP,masses2,MomInv,TDrm1,TDrG)
      

    ! rank 0
    TEr = 0
    TEr(1,0) = EreduD0(InvX,TDrm1(1,0:4,0))
    
    ! rank 1
    if (r.ge.1) then
      TEr(1:4,1) = EreduD1(InvX,TDrm1(1,0:4,0),MomP)
    end if
    
    ! rank >= 2
    do i=2,r
      TEr(1:RankToSize(4,i),i) = EreduDr(i,InvX,TDrm1(1:RankToSize(4,i-1),0:4,i-1), &
                                     &  TDrG(1:RankToSize(4,i-2),0:4,i-2),MomP)
    end do
  
  end function CalcTensorEr

  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function EreduD0(InvX,D0)
  !
  !  calculate the scalar 5-point coefficient E0 from the 4-point coefficients
  !  with one dropped propagator D0
  !  InvX = inverse Cayley matrix 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function EreduD0(InvX,D0)
  
    complex*16, intent(in) :: InvX(0:4,0:4), D0(0:4)
    complex*16 :: EreduD0
    integer :: i
    
    EreduD0 = -InvX(0,0)*D0(0)
    do i=1,4
      EreduD0 = EreduD0 + InvX(0,i)*(D0(i)-D0(0))
    end do
  
  end function EreduD0
 
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function EreduD1(InvX,D0,MomP)
  !  
  !  calculate the rank1 5-point coefficient E0 from the scalar 4-point coefficients
  !  with one dropped propagator D0
  !  InvX = inverse Cayley matrix   
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function EreduD1(InvX,D0,MomP)
  
    complex*16, intent(in) :: InvX(0:4,0:4), D0(0:4)
    complex*16, intent(in) :: MomP(0:3,4)
    complex*16, dimension(4) :: D0_Ni0, EreduD1
    integer :: rm1, str, strm1, pos1, pos2, bino, i, j, m, mu
    
    EreduD1 = 0

    do i=1,4
      EreduD1 = EreduD1 - InvX(i,0)*MomP(0:3,i)*D0(0)
      D0_Ni0(i) = D0(i) - D0(0)  
      do j=1,4
        EreduD1 = EreduD1 + InvX(j,i)*MomP(0:3,i)*D0_Ni0(i)
      end do
    end do
    
  end function EreduD1

   
  
  
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function EreduDr(r,InvX,Drm1,DrG,MomP)
  !
  !  reduce tensor integral TEr for 5-point functions to tensor integrals TDr for 4-point functions.
  !  InvX = inverse Cayley matrix
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function EreduDr(r,InvX,Drm1,DrG,MomP)
  
    integer, intent(in) :: r
    complex*16, intent(in) :: InvX(0:4,0:4), Drm1(:,0:), DrG(:,0:), MomP(0:3,4)
    complex*16, dimension(:,:), allocatable :: Drm1_Ni0, DrG_Ni0
    complex*16, dimension(:), allocatable :: EreduDr
    integer :: rm1, rm2, str, strm1, strm2, i, j, m, a, mu, nu, cnt, cnt_aux, ind_aux
    complex*16 :: Pmu, MomPc(0:3,4)

    str = RankToSize(4,r)
    rm1 = r-1
    strm1 = RankToSize(4,rm1)
    rm2 = rm1-1
    strm2 = RankToSize(4,rm2)
    
    allocate(EreduDr(str))
    EreduDr = 0
    
    allocate(Drm1_Ni0(strm1,4))
    allocate(DrG_Ni0(strm2,4))
    do i=1,4
      Drm1_Ni0(:,i) = Drm1(:,i) - Drm1(:,0)
      DrG_Ni0(:,i) = DrG(:,i) - DrG(:,0)
      MomPc(0,i) = MomP(0,i)
      MomPc(1:3,i) = -MomP(1:3,i)
    end do

    do i=1,4

      do a = 1, strm2
        cnt_aux=1
        do while (TStrucG(0,cnt_aux,a,1,rm2).ne.0)
          EreduDr(TStrucG(0,cnt_aux,a,1,rm2)) = EreduDr(TStrucG(0,cnt_aux,a,1,rm2)) + &
                  &   TStrucG(1,cnt_aux,a,1,rm2)*DrG_Ni0(a,i)*InvX(i,0)
          cnt_aux = cnt_aux+1
        end do
      end do
        
      cnt = 1
      do mu=0,3
        Pmu = MomP(mu,i)
        
        do a = strm2-BinomTable(rm2,rm2+3-mu)+1, strm2
          
          do j=1,4
            do m=1,4
              do nu=mu,3
                ind_aux = IndsTensorProd2(1,mu+1,IndsTensorProd2(1,nu+1,a,1,rm2),1,rm1)
                EreduDr(ind_aux) = EreduDr(ind_aux) - IndsTensorProd2(0,nu+1,a,1,rm2)*MomP(nu,m)* &
                   &  2*(InvX(m,i)*InvX(j,0)-InvX(j,m)*InvX(i,0))*Pmu*DrG_Ni0(a,j)
              end do
            end do
          end do
                
        end do
        
        do a = strm1-BinomTable(rm1,rm1+3-mu)+1, strm1
  
          EreduDr(cnt) = EreduDr(cnt) - InvX(i,0)*Pmu*Drm1(a,0)

          do j=1,4
            EreduDr(cnt) = EreduDr(cnt) + InvX(i,j)*Pmu*Drm1_Ni0(a,j)
          end do  
  
          cnt = cnt+1
  
        end do
  
      end do

    end do

    
  end function EreduDr





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcErm1PVco(N,r,masses2,MomInv)
  ! 
  !  calculates the tensor coefficients E(m)_{0...0,i1,...,ik}
  !  for 6-point tensor integrals with the m'th propagator being dropped
  !  based on Ansgar's function cEp1234 in the COLI library
  !
  !  MomInv = (I(1,1),...,I(1,5),I(2,2),...,I(2,5),...,I(5,5)
  !  with I(i,i)=pi^2  and I(i,j)=(pi-pj)^2 for i/=j
  !
  !  result is saved as array with indices representing
  !  m, symmetric momentum index, number of metric tensors, rank
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcErm1PVco(rm1,masses2,MomInv)

    integer, intent(in) :: rm1
    complex*16, dimension(15), intent(in) :: MomInv
    complex*16, dimension(0:5), intent(in) :: masses2
    complex*16, dimension(:,:,:,:), allocatable :: CalcErm1PVco
    complex*16 :: p10, p21, p31, p41, p51, p20, p32, p42, p52
    complex*16 :: p30, p43, p53, p40, p54, p50
    integer :: i, j, k, a, kmax, BinoMax, im2k
    complex*16, dimension(0:5,0:5,0:4,0:4,0:4,0:4,0:4) :: E012345
    integer :: inds(5), dropinds(5), ind1, inda, shift
    
    kmax = rm1/2
    
    E012345 = 0
    allocate(CalcErm1PVco(0:5,BinomTable(rm1,rm1+4),0:kmax,0:rm1))
    CalcErm1PVco = 0
    
    p10 = MomInv(1)
    p21 = MomInv(2)
    p31 = MomInv(3)
    p41 = MomInv(4)
    p51 = MomInv(5)
    p20 = MomInv(6)        
    p32 = MomInv(7)
    p42 = MomInv(8)
    p52 = MomInv(9)
    p30 = MomInv(10)
    p43 = MomInv(11)
    p53 = MomInv(12)
    p40 = MomInv(13)
    p54 = MomInv(14)
    p50 = MomInv(15)
        
    call olEp12345(p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
        & masses2(0),masses2(1),masses2(2),masses2(3),masses2(4), &
        & E012345(5,0,0,0,0,0,0),E012345(5,1,1:4,0,0,0,0),E012345(5,2,0:4,0:4,0,0,0), &
        & E012345(5,3,0:4,0:4,0:4,0,0),E012345(5,4,0:4,0:4,0:4,0:4,0),E012345(5,5,0:4,0:4,0:4,0:4,0:4),rm1)
        
    call olEp12345(p10,p21,p32,p53,p50,p20,p31,p52,p30,p51, &
        & masses2(0),masses2(1),masses2(2),masses2(3),masses2(5), &
        & E012345(4,0,0,0,0,0,0),E012345(4,1,1:4,0,0,0,0),E012345(4,2,0:4,0:4,0,0,0), &
        & E012345(4,3,0:4,0:4,0:4,0,0),E012345(4,4,0:4,0:4,0:4,0:4,0),E012345(4,5,0:4,0:4,0:4,0:4,0:4),rm1)

    call olEp12345(p10,p21,p42,p54,p50,p20,p41,p52,p40,p51, &
        & masses2(0),masses2(1),masses2(2),masses2(4),masses2(5), &
        & E012345(3,0,0,0,0,0,0),E012345(3,1,1:4,0,0,0,0),E012345(3,2,0:4,0:4,0,0,0), &
        & E012345(3,3,0:4,0:4,0:4,0,0),E012345(3,4,0:4,0:4,0:4,0:4,0),E012345(3,5,0:4,0:4,0:4,0:4,0:4),rm1)
        
    call olEp12345(p10,p31,p43,p54,p50,p30,p41,p53,p40,p51, &
        & masses2(0),masses2(1),masses2(3),masses2(4),masses2(5), &
        & E012345(2,0,0,0,0,0,0),E012345(2,1,1:4,0,0,0,0),E012345(2,2,0:4,0:4,0,0,0), &
        & E012345(2,3,0:4,0:4,0:4,0,0),E012345(2,4,0:4,0:4,0:4,0:4,0),E012345(2,5,0:4,0:4,0:4,0:4,0:4),rm1)

    call olEp12345(p20,p32,p43,p54,p50,p30,p42,p53,p40,p52, &
        & masses2(0),masses2(2),masses2(3),masses2(4),masses2(5), &
        & E012345(1,0,0,0,0,0,0),E012345(1,1,1:4,0,0,0,0),E012345(1,2,0:4,0:4,0,0,0), &
        & E012345(1,3,0:4,0:4,0:4,0,0),E012345(1,4,0:4,0:4,0:4,0:4,0),E012345(1,5,0:4,0:4,0:4,0:4,0:4),rm1)

    call olEp12345(p21,p32,p43,p54,p51,p31,p42,p53,p41,p52, &
        & masses2(1),masses2(2),masses2(3),masses2(4),masses2(5), &
        & E012345(0,0,0,0,0,0,0),E012345(0,1,1:4,0,0,0,0),E012345(0,2,0:4,0:4,0,0,0), &
        & E012345(0,3,0:4,0:4,0:4,0,0),E012345(0,4,0:4,0:4,0:4,0:4,0),E012345(0,5,0:4,0:4,0:4,0:4,0:4),rm1) 




    ! scalar coefficient
    CalcErm1PVco(0:5,1,0,0) = E012345(0:5,0,0,0,0,0,0)
        
    do i=1,rm1
      ! non-existing indices are set to zero
      inds = 0
      if (mod(i,2).eq.0) then
        CalcErm1PVco(0:5,1,i/2,i) = E012345(0:5,i,inds(1),inds(2),inds(3),inds(4),inds(5))
      end if
        
      do k=(i-1)/2,0,-1 
        im2k = i-2*k            
          
        BinoMax = BinomTable(im2k,3+im2k)
        shift = BinomTable(im2k-1,3+im2k)

        do j=1,BinoMax
          ! value 1,2,3,4 for indices 2k+1,...,i
          inds(2*k+1:i) = IndCombisEq(1:im2k,j,im2k,4)
          dropinds(1:5) = DropIndCombisEq(j,1:5,1,im2k,5)

          ! result
          CalcErm1PVco(0,j+shift,k,i) = E012345(0,i,inds(1),inds(2),inds(3),inds(4),inds(5))
          do a=1,5
            CalcErm1PVco(a,dropinds(a),k,i) = E012345(a,i,inds(1),inds(2),inds(3),inds(4),inds(5))
          end do
        end do
        
        do j=BinomTable(im2k-1,3+im2k),1,-1
          ind1 = IndsPiProd(0,1,j,5)
          CalcErm1PVco(0,ind1,k,i) = - CalcErm1PVco(0,j,k,i-1)
          do a=2,5
            inda = IndsPiProd(0,a,j,5)
            CalcErm1PVco(0,ind1,k,i) = CalcErm1PVco(0,ind1,k,i) - CalcErm1PVco(0,inda,k,i)
          end do
        end do
            
      end do
          
    end do    

  end function CalcErm1PVco
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTensorErm1(rm1,MomVec,masses2,MomInv)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcTensorErm1(rm1,MomP,masses2,MomInv) result(Erm1)

    integer, intent(in) :: rm1
    complex*16, dimension(0:3,5), intent(in) :: MomP
    complex*16, dimension(0:5), intent(in) :: masses2
    complex*16, dimension(15), intent(in) :: MomInv
    complex*16, allocatable :: Erm1(:,:,:), Coefs(:,:,:,:)
    integer :: rts, kmax, bino, Nm1
    
    rts = RankToSize(4,rm1)
    bino = BinomTable(rm1,rm1+4)
    kmax = rm1/2

    allocate(Erm1(1:rts,0:5,0:rm1))
    allocate(Coefs(0:5,bino,0:kmax,0:rm1))

    Coefs = CalcErm1PVco(rm1,masses2,MomInv)
    Erm1 = 0

    call AddToErm1_0(rm1,Erm1,Coefs,MomP)
    
  end function CalcTensorErm1





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddToErm1_0(rmax,Erm1,Coefs,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine AddToErm1_0(rmax,Erm1,Coefs,MomVec)

    integer, intent(in) :: rmax
    complex*16, intent(in) :: MomVec(0:3,5), Coefs(0:,:,0:,0:)
    complex*16, intent(inout) :: Erm1(1:RankToSize(4,rmax),0:5,0:rmax)
    logical :: flag0(5)
    integer :: i,k,r2k,stk,r

    Erm1(1,0,0) = Erm1(1,0,0) + Coefs(0,1,0,0)
    do i=1,5
        Erm1(1,i,0) = Erm1(1,i,0) + Coefs(i,1,0,0)
    end do
    
    do k=1,rmax/2
      r=2*k
      stk = RankToSize(4,r)
      Erm1(1:stk,0,r) = Coefs(0,1,k,r)*MetricTensorProd(1:stk,k)
      do i=1,4
        Erm1(1:stk,i,r) = Coefs(i,1,k,r)*MetricTensorProd(1:stk,k)
      end do
    end do
        
    if (rmax>0) then

      do i=1,5
        flag0 = .true.
        flag0(i) = .false.
        call AddToErm1(rmax,1,i,Erm1,Coefs,MomVec(0:3,i),MomVec,flag0)
      end do

    end if

  end subroutine AddToErm1_0





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddToErm1(rmax,r,ir,Erm1,Coefs,MomTenRec,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine AddToErm1(rmax,r,ir,Erm1,Coefs,MomTenRec,MomVec,flag)

    integer, intent(in) :: rmax, r, ir
    complex*16, intent(in) :: MomVec(0:3,5), Coefs(0:,:,0:,0:), MomTenRec(1:RankToSize(4,r))
    complex*16, intent(inout) :: Erm1(1:RankToSize(4,rmax),0:5,0:rmax)
    logical, intent(in) :: flag(5)
    logical :: flag0(5)
    complex*16 :: MomTen(1:RankToSize(4,r+1)), Pmu
    integer :: i,jr,k,rts,r2k,a,mu,cnt


    rts = RankToSize(4,r)
    Erm1(1:rts,0,r) = Erm1(1:rts,0,r) + Coefs(0,ir,0,r)*MomTenRec
    do i=1,5
      if (flag(i)) then
        Erm1(1:rts,i,r) = Erm1(1:rts,i,r) + Coefs(i,ir,0,r)*MomTenRec
      end if
    end do
    

    do k=1,(rmax-r)/2
      r2k = r+2*k
      do a = 1,rts
        cnt = 1
        do while ((cnt.le.BinomTable(rmax/2,3+rmax/2)).and.(TStruc(0,cnt,a,k,r).ne.0))
          Erm1(TStruc(0,cnt,a,k,r),0,r2k) = Erm1(TStruc(0,cnt,a,k,r),0,r2k) + &
                & Coefs(0,ir,k,r2k)*MomTenRec(a)*TStruc(1,cnt,a,k,r)
          do i=1,5
           if (flag(i)) then
              Erm1(TStruc(0,cnt,a,k,r),i,r2k) = Erm1(TStruc(0,cnt,a,k,r),i,r2k) + &
                    & Coefs(i,ir,k,r2k)*MomTenRec(a)*TStruc(1,cnt,a,k,r)
            end if
          end do
          cnt = cnt+1
        end do
      end do
    end do

        
    if (r<rmax) then

      do i=1,5
        flag0 = flag
        flag0(i) = .false.
        jr = IndsPiProd(0,i,ir,5)

        cnt = 1
        do mu=0,3
          Pmu = MomVec(mu,i)
          do a = rts-BinomTable(r,r+3-mu)+1, rts
            MomTen(cnt)=MomTenRec(a)*Pmu
            cnt = cnt+1
          end do
        end do

        call AddToErm1(rmax,r+1,jr,Erm1,Coefs,MomTen,MomVec,flag0)

      end do

    end if

  end subroutine AddToErm1

  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcTensorFr(r,MomP,masses2,MomInv)
  !
  !  calculate tensor integral TFr for 6-point functions by direct reduction 
  !  from the tensor integrals TEr for 5-point functions.
  !  the TEr are constructed using the 5-point coefficients calculated with COLI
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcTensorFr(r,MomP,masses2,MomInv) result(TFr)
  
    integer, intent(in) :: r
    complex*16, intent(in) :: MomP(0:3,5), masses2(0:5), MomInv(15)
    complex*16, dimension(RankToSize(4,r),0:r) :: TFr
    complex*16 :: Z(5,5), X(0:5,0:5), InvX0k(5,5), DetX0k, X0k(5,5)
    complex*16 :: VecF(5), DetX, InvX(0:5,0:5)
    complex*16, dimension(:,:,:,:,:,:), allocatable :: TensorBasis
    complex*16, dimension(:,:,:,:), allocatable :: Coefs
    complex*16, dimension(:,:,:), allocatable :: TErm1
    complex*16, dimension(:,:), allocatable :: TensorP1, TErm1tilde
    integer :: kmax_m1, bino_m1, strm1, rm1, i, sti, im1, stim1, kbest
    
    ! Gram and Cayley matrix
    Z = CalcGramMat2(5,MomInv)
    VecF = CalcVecF2(5,MomInv,masses2)
    X = CalcCayleyMat(5,masses2(0),Z,VecF)
    DetX = CalcDet(6,X)
    InvX = CalcInvMatSym(6,DetX,X)
    
    ! matrix X0k
    call CalcX0k(5,X,kbest,X0k)
    DetX0k = CalcDet(5,X0k)
    InvX0k = CalcInvMat(5,DetX0k,X0k)

    ! build rank5 tensors
    rm1 = r-1
    kmax_m1 = rm1/2
    bino_m1 = BinomTable(rm1,rm1+3)
    strm1 = RankToSize(4,rm1)
    
    allocate(TErm1(strm1,0:5,0:rm1))
    TErm1(1:strm1,0:5,0:rm1) = CalcTensorErm1(rm1,MomP,masses2,MomInv)
    
    ! rank 0
    TFr = 0
    TFr(1,0) = FreduE0(InvX,TErm1(1,0:5,0)) 

    do i=1,r
      im1 = i-1
      sti = RankToSize(4,i)
      stim1 = RankToSize(4,im1)
      
      TFr(1:sti,i) = FreduEr(i,kbest,InvX0k,TErm1(1:stim1,0:5,im1),MomP)
  
    end do
  
  end function CalcTensorFr

  
  
  
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function FreduE0(InvX,E0)
  !
  !  calculate the scalar 6-point coefficient F0 from the 5-point coefficients
  !  with one dropped propagator E0
  !  InvX = inverse Cayley matrix 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function FreduE0(InvX,E0)
  
    complex*16, intent(in) :: InvX(0:5,0:5), E0(0:5)
    complex*16 :: FreduE0
    integer :: i
    
    FreduE0 = -InvX(0,0)*E0(0)
    do i=1,5
      FreduE0 = FreduE0 + InvX(i,0)*(E0(i)-E0(0))
    end do
  
  end function FreduE0

  
  
  
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function FreduEr(r,kbest,InvX0k,Erm1,MomP)
  !
  !  reduce tensor integral TEr for 5-point functions to tensor integrals TDr for 4-point functions.
  !  InvX = inverse Cayley matrix
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function FreduEr(r,kbest,InvX0k,Erm1,MomP)
  
    integer, intent(in) :: r, kbest
    complex*16, intent(in) :: InvX0k(5,5), Erm1(:,0:)
    complex*16, intent(in) :: MomP(0:3,5)
    complex*16, dimension(:), allocatable :: FreduEr
    complex*16 :: Pmu 
    integer :: rm1, str, strm1, i, m, mu, a, cnt
    
    rm1 = r-1
    str = RankToSize(4,r)
    strm1 = RankToSize(4,rm1)
    
    allocate(FreduEr(str))
    FreduEr = 0

    do m=1,5
      if (m.ne.kbest) then
        do i=1,5
          cnt = 1
          do mu=0,3
            Pmu = MomP(mu,m)
            do a = strm1-BinomTable(rm1,rm1+3-mu)+1, strm1
              FreduEr(cnt) = FreduEr(cnt) + InvX0k(i,m)*Pmu*(Erm1(a,i)-Erm1(a,0))
              cnt = cnt+1
            end do
          end do
        end do
      end if
    end do
    
  end function FreduEr
  
     
end module TensorReduction