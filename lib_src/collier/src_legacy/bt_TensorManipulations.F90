!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ********************************
!  *  module TensorManipulations  *
!  *       by Lars Hofer          *
!  ********************************
!
!  global variables:
!  RankToSize, RankToSizeSiMu, IndsTensorProd1, IndsTensorProd2, IntToIndices,
!  IndToSymInd, SingleInds
!
!  functions and subroutines:
!  CalcIndicesToInt, SetIntToIndices, IntToIndices, SetRankToSize, CalcRankToSize, 
!  CalcRankToSizeSym, CalcRankToSizeSingle, SetRankToSizeSiMu, CalcRankToSizeSiMu,
!  SetIndToSymInd, CalcIndToSymInd, GetTensorElement, SetTensorElement, SetSingleInds,
!  SetIndsTensorProd1, SetIndsTensorProd2, CalcIndsTensorProd1, CalcSymTensorProd1  
!  CalcSymElement, CalcTensorProd2, AddToTensorProd2, CalcOrdTensorProd
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module TensorManipulations

  use ColiCombinatorics
  

  implicit none
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  global variables RankToSize
  !  IndsTensorProd, IntToIndices, IndToSymIndList
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  integer, dimension(:,:), allocatable :: RankToSize
  integer, dimension(:,:,:), allocatable :: RankToSizeSiMu
  integer, dimension(:,:,:,:,:), allocatable :: IndsTensorProd1
  integer, dimension(:,:,:,:,:), allocatable :: IndsTensorProd2
  integer, dimension(:,:,:), allocatable :: IntToIndices
  integer, dimension(:), allocatable :: IndToSymInd
  integer, dimension(:,:), allocatable :: SingleInds
  
  
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   function CalcIndicesToInt(r,IndArr)
  !
  !   converts an array IndArr of r tensor indices mu(k)=0,...,3
  !   to the integer    Int = sum_k (mu(k)*4^(r-k))
  !
  !   example: {2,0,1,1} --> 2*4^3 + 0*4^2 + 1*4 + 1*1 = 133
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndicesToInt(r,IndArr)
  
    integer, intent(in) :: r
    integer, dimension(r), intent(in) :: IndArr
    integer :: CalcIndicesToInt, i

    CalcIndicesToInt = 0
    do i=1,r
      CalcIndicesToInt = (4*CalcIndicesToInt) + IndArr(i)
    end do

  end function CalcIndicesToInt
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIntToIndices(r)
  !
  !  sets global variable IntToIndices to the table
  !  IntToIndices = ((CITI(1,0),...,CITI(1,3),0,...,0),
  !                  (CITI(2,0),...,CITI(2,15),0,...,0),
  !                  ...
  !                  (CITI(r,0),...,CITI(r,4**r-1)))
  !  with CITI = CalcIntToIndices
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIntToIndices(r)
  
    integer, intent(in) :: r
    integer :: str, i, j
      
    if (r<0) then
      write (*,*) r, ' is not a valid rank for a tensor'
      stop
    end if
    
    str = RankToSize(5,r)
    
    if (allocated(IntToIndices)) then
      deallocate(IntToIndices)
    end if
    allocate(IntToIndices(1:r,0:str,0:r))
    IntToIndices = -1
    
    IntToIndices(1,0,0) = 0
    do i=1,r
      do j=0,RankToSize(5,i)
        IntToIndices(1:i,j,i) = CalcIntToIndices(i,j)
      end do
    end do
  
  end subroutine SetIntToIndices
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIntToIndices(r,inte)
  !
  !  extracts from the integer Int = sum_k (mu(k)*4^(r-k))
  !  the array mu(k) of tensor indices
  !  r = size of array (needed to determine number of
  !                     leading zeros
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcIntToIndices(r,inte)
  
    integer, intent(in) :: r, inte
    integer :: inted
    integer, dimension(r) :: CalcIntToIndices
    integer :: div, i
    
    div = 4**(r-1)
    inted = inte
    do i=1,r
      CalcIntToIndices(i) = inted/div
      inted = inted - div*CalcIntToIndices(i)
      div = div/4 
    end do

  end function CalcIntToIndices
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetRankToSize(r)
  !
  !  set global variable RankToSize to vector
  !  RankToSize = ((RankToSizeSingle0(0),...,RankToSizeSingle3(0)),
  !                 RankToSizeSym(0),RankToSize(0)),
  !                 ...
  !                (RankToSizeSingle0(r),...,RankToSizeSingle3(r)),
  !                 RankToSizeSym(r),RankToSize(r)))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetRankToSize(r)
  
    integer, intent(in) :: r
    
    if (r<0) then
      write (*,*) r, ' is not a valid rank for a tensor'
      stop
    end if
    
    if (allocated(RankToSize)) then
      deallocate(RankToSize)
    end if
    allocate(RankToSize(0:5,0:r))
    
    RankToSize(0:3,0:r) = CalcRankToSizeSingle(r)
    RankToSize(4,0:r) = CalcRankToSizeSym(r)
    RankToSize(5,0:r) = CalcRankToSize(r)

  end subroutine SetRankToSize
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcRankToSize(r)
  !
  !  calculates conversion vector rank --> size for Lorentz 
  !  tensors with maximum rank r
  !  size s = 4^i-1 for i=0,...,r
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcRankToSize(r)
  
    integer, intent(in) :: r
    integer, dimension(0:r) :: CalcRankToSize
    integer :: i
    
    do i=0,r
      CalcRankToSize(i) = 4**i - 1
    end do
    
  end function CalcRankToSize
    
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcRankToSizeSym(r)
  !
  !  calculates conversion vector rank --> size (= number of independent
  !  tensor elements) for symmetric(!) Lorentz tensors with maximum rank r
  !
  !  e.g.: r=2 --> RankToSizeSym = (1,4,10)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcRankToSizeSym(r)
  
    integer, intent(in) :: r
    integer, dimension(0:r) :: CalcRankToSizeSym
    integer :: i
    
    do i=0,r
      CalcRankToSizeSym(i) = BinomTable(i,i+3)
    end do
    
  end function CalcRankToSizeSym
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcRankToSizeSingle(r)
  !
  !  calculates conversion vector rank --> size for symmetric(!) 
  !  Lorentz tensors with maximum rank r and 0,1,2,3 unpaired indices
  !  (index 0 is always counted as paired)
  !
  !  e.g.: r=2 --> RankToSizeSingle = ((1,0,0,0),(1,3,0,0),(4,3,3,0))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcRankToSizeSingle(r)
  
    integer, intent(in) :: r
    integer, dimension(0:3,0:r) :: CalcRankToSizeSingle
    integer :: i, k, aux
    
    do i=0,r
      do k=0,3
        if (i<k) then
          CalcRankToSizeSingle(k,i) = 0
        else
          aux = (i-k)/2
          CalcRankToSizeSingle(k,i) = BinomTable(k,3) * BinomTable(aux,3+aux)
        end if
      end do
    end do
    
  end function CalcRankToSizeSingle
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetRankToSizeSiMu(r)
  !
  !  sets global variable RankToSizeSiMu to CalcRankToSizeSiMu(r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetRankToSizeSiMu(r)
  
    integer, intent(in) :: r
    
    if (r<0) then
      write (*,*) r, ' is not a valid rank for a tensor'
      stop
    end if
    
    if (allocated(RankToSizeSiMu)) then
      deallocate(RankToSizeSiMu)
    end if
    allocate(RankToSizeSiMu(0:3,0:3,0:r))
    
    RankToSizeSiMu(0:3,0:3,0:r) = CalcRankToSizeSiMu(r)

  end subroutine SetRankToSizeSiMu
  
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcRankToSizeSiMu(r)
  !
  !  calculates conversion vector rank --> size for symmetric(!) 
  !  Lorentz tensors with maximum rank r with 
  !  * 0,1,2,3 unpaired indices (index 0 is always counted as paired)
  !  * first index = 0,1,2,3
  !
  !  e.g.: r=2 --> RankToSizeSym = ((1,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),
  !                                 (1,0,0,0),(0,1,1,1),(0,0,0,0),(0,0,0,0),
  !                                 (1,1,1,1),(3,0,0,0),(0,1,1,1),(0,0,0,0))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcRankToSizeSiMu(r)
  
    integer, intent(in) :: r
    integer, dimension(0:3,0:3,0:r) :: CalcRankToSizeSiMu
    integer :: i, k, aux, aux0, mu
    
    CalcRankToSizeSiMu = 0
    CalcRankToSizeSiMu(0:3,0,0) = 1
    
    do i=1,r
      do k=0,min(i,3)
      
        aux = i-k
        
        ! mu=0
        aux0 = floor((aux-1)/2.0)
        if (aux0.ge.0) then
          CalcRankToSizeSiMu(0,k,i) = BinomTable(k,3)*BinomTable(aux0,3+aux0)
        end if
        
        ! mu=1,2,3
        if (mod(aux,2).eq.0) then
          
          do mu=1,min(4-k,3)
          
            if (k.ge.1) then
              CalcRankToSizeSiMu(mu,k,i) = BinomTable(k-1,3-mu)*BinomTable(aux/2,3-mu+aux/2)
            end if
            
            if ((mu.le.(3-k)).and.(aux.ge.2)) then
              CalcRankToSizeSiMu(mu,k,i) = CalcRankToSizeSiMu(mu,k,i) + &
                   &  BinomTable(k,3-mu)*BinomTable(aux/2-1,2-mu+aux/2)
            end if
          
          end do
          
        end if
        
      end do
    end do
    
  end function CalcRankToSizeSiMu  
    
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndToSymInd(r)
  !
  !  sets global variable IndToSymInd to value CalcIndToSymInd(r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndToSymInd(r)
  
    integer, intent(in) :: r
    
    if (allocated(IndToSymInd)) then
      deallocate(IndToSymInd)
    end if
    allocate(IndToSymInd(0:RankToSize(5,r)))
    
    IndToSymInd = CalcIndToSymInd(r)
  
  end subroutine SetIndToSymInd
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndToSymInd(r)
  !
  !  calculates table for converting integers representing generic tensor indices
  !  of rank r into symmetric tensor indices of rank r
  !  (result is also valid for any rank r'<r if only the first 0:4^r-1 elements 
  !   are considered)
  !
  !  example: r=2
  !  generic indices: 0 --> 00, 1 --> 01, 2 --> 02, 3 --> 03, 4 --> 10, ..., 15 --> 33
  !  symmetric indices: 1 --> 00, 2 --> 01, 3 --> 02, 4 --> 03, 5 --> 11,
  !                     6 --> 12, 7 --> 13, 8 --> 22, 9 --> 23, 10 --> 33
  !  ==> CalcIndToSymInd(r>=2)(0:15) = (1,2,3,4,2,5,6,7,3,6,8,9,4,7,9,10)
  !      (contains CalcIndToSymInd(1)(0:3) = (1,2,3,4))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndToSymInd(r) result(itsi)
  
    integer, intent(in) :: r
    integer, allocatable :: itsi(:), IndList(:)
    integer :: stk, stkm1, swap, cnt, i, k, j
    logical :: ordered
    
    allocate(itsi(0:RankToSize(5,r)))
    if (r.ge.1) then
      itsi(0:3) = (/ 1,2,3,4 /)
      cnt = 5
    else
      itsi(0) = 1
    end if
    
    do k=2,r
      stkm1 = RankToSize(5,k-1)
      stk = RankToSize(5,k)
      
      if (allocated(IndList)) then
        deallocate(IndList)
      end if
      allocate(IndList(k))
      
      do i=stkm1+1,stk
        IndList = IntToIndices(1:k,i,k)
        
        j=1
        ordered = .true.
        do while ((j<k).and.(ordered))
          if (IndList(j)>IndList(j+1)) then
            swap = IndList(j)
            IndList(j) = IndList(j+1)
            IndList(j+1) = swap
            itsi(i) = itsi(CalcIndicesToInt(k,IndList))
            ordered = .false.
          end if
          j = j+1
        end do
        
        if (ordered) then
          itsi(i) = cnt
          cnt = cnt+1
        end if
        
      end do
      
    end do
   
  end function CalcIndToSymInd
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function GetTensorElement(r,tensor,sym,mui)
  !
  !  get element (mu1,mu2,...,mr) of a (rank r)-tensor
  !  with indices represented by integers.
  !  sym = 0: tensor is not symmetric
  !  sym = 1: tensor is symmetric
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetTensorElement(r,tensor,sym,mui)

    integer, intent(in) :: r, sym
    complex*16, dimension(sym:RankToSize(5-sym,r)), intent(in) :: tensor
    integer, dimension(r), intent(in) :: mui
    integer :: inte
    complex*16 :: GetTensorElement

    inte = CalcIndicesToInt(r,mui)
    if (sym.eq.1) then
      inte = IndToSymInd(inte)
    end if
    GetTensorElement = tensor(inte)
    
  end function GetTensorElement





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTensorElement(r,tensor,sym,mui,elem)
  !
  !  set element (mu1,mu2,...,mr) of a (rank r)-tensor
  !  with indices represented by integers.
  !  sym = 0: tensor is not symmetric
  !  sym = 1: tensor is symmetric
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetTensorElement(r,tensor,sym,mui,elem)

    integer, intent(in) :: r, sym
    complex*16, dimension(sym:RankToSize(5-sym,r)), intent(inout) :: tensor
    complex*16, intent(in) :: elem
    integer, dimension(r), intent(in) :: mui
    integer :: inte

    inte = CalcIndicesToInt(r,mui)
    if (sym.eq.1) then
      inte = IndToSymInd(inte)
    end if
    tensor(inte) = elem
    
  end subroutine SetTensorElement
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetSingleInds(r)
  !
  !  sets global variable SingleInds to the table
  !  storing for k=0,1,2,3 all symmetric tensor index combinations
  !  with exactly k unpaired indices (0 is always counted as paired)
  !
  !  example: r=2
  !  symmetric indices: 1-->00, 2-->01, 3-->02, 4-->03,  5-->11,
  !                     6-->12, 7-->13, 8-->22, 9-->23, 10-->33
  !  ==> SingleInds(2) = ((1,5,8,10),
  !                       (2,3,4),
  !                       (5,7,9))
  !  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetSingleInds(r)
  
    integer, intent(in) :: r
    integer, dimension(:), allocatable :: inds
    integer :: sinds(0:3), cnt(3)
    integer :: cntMax, str, i, k, j

    if (RankToSize(1,r)>RankToSize(2,r)) then
      cntMax = RankToSize(1,r)
    else
      cntMax = RankToSize(2,r)
    end if
    
    if (allocated(SingleInds)) then
      deallocate(SingleInds)
    end if
    allocate(SingleInds(cntMax,0:3))
    SingleInds = 0
    
    str = RankToSize(4,r)
    sinds = 1

    allocate(inds(r))
    do i=1,str
      inds = IndCombisEq(1:r,i,r,4) - 1
      cnt = 0
      
      do j=1,r
        if (inds(j).ne.0) then
          cnt(inds(j)) = cnt(inds(j)) + 1
        end if
      end do
      
      k=0
      do j=1,3
        if (mod(cnt(j),2).eq.1) then
          k = k+1
        end if
      end do
      
      SingleInds(sinds(k),k) = i
      sinds(k) = sinds(k) + 1
      
    end do
  
  end subroutine SetSingleInds
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndsTensorProd1(r)
  !
  !  sets global variable IndsTensorProd1 to
  !  IndsTensorProd1 = ((CITP1(0,0), CITP1(0,1), ..., CITP1(0,r)),
  !                     (CITP1(1,0), CITP1(1,1),...,CITP1(1,r-1),0),
  !                     ...
  !                     (CITP1(r,0), 0, ..., 0))
  !  CITP1 = CalcIndsTensorProd1
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndsTensorProd1(r)
  
    integer, intent(in) :: r
    integer :: binoMax, str, r1, r2, r12, str12, bino
    
    if (r<0) then
      write (*,*) r, ' is not a valid rank for a tensor'
      stop
    end if
    
    if (mod(r,2).eq.0) then
      binoMax = BinomTable(r/2,r)
    else
      binoMax = BinomTable((r-1)/2,r)
    end if
    str = RankToSize(4,r)
    
    if (allocated(IndsTensorProd1)) then
      deallocate(IndsTensorProd1)
    end if
    allocate(IndsTensorProd1(0:2,binoMax,str,0:r,0:r))
    
    IndsTensorProd1 = -1
    do r1=0,r
      do r2=0,r-r1
        r12 = r1+r2
        str12 = RankToSize(4,r12)
        bino = BinomTable(r1,r12)
        IndsTensorProd1(0:2,1:bino,1:str12,r2,r1) = CalcIndsTensorProd1(r1,r2)
      end do
    end do
  
  end subroutine SetIndsTensorProd1
   
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndsTensorProd1(r1,r2)
  !
  !  calculates index combinations of symmetric tensors T1, T2 
  !  of rank r1,r2 appearing in the symmetrized product T12
  !
  !  example: r1=1,r2=2 
  !  ==> T12(mu,nu,rho) = T1(mu)*T2(nu,rho) + T1(nu)*T2(mu,rho) +
  !                       T1(rho)*T2(mu,nu)
  !  T12 has 20 independent elements
  !  e.g. T12[5] = T12(0,1,1) 
  !              = T1(0)*T2(1,1) + T1(1)*T2(0,1) + T1(1)*T2(0,1)
  !              = 1*T1[1]*T2[5] + 2*T1[2]*T2[2] 
  !        
  !  ==>  IndTensorProd1(5,1:3,0:2) = ((1,1,5),(2,2,2),(0,0,0))
  !  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndsTensorProd1(r1,r2)
  
    integer, intent(in) :: r1, r2
    integer, dimension(:,:,:), allocatable :: CalcIndsTensorProd1
    integer, dimension(:,:), allocatable :: IndArr1, IndArr2
    integer, dimension(:), allocatable :: inds, inds1, inds2
    integer :: bino, r12, str12, i, j, k, cnt, x, y
    logical :: hit
    
    ! allocate CalcIndsTensorProd
    r12 = r1+r2
    str12 = RankToSize(4,r12)
    bino = BinomTable(r1,r12)
    allocate(CalcIndsTensorProd1(0:2,bino,str12))
    CalcIndsTensorProd1 = 0
            
    ! allocate arrays for indices
    allocate(inds(r12))
    allocate(inds1(r1))
    allocate(inds2(r2))
        
    if (r1.ne.0) then
      allocate(IndArr1(r1,bino))
      IndArr1 = IndCombis(1:r1,1:bino,r1,r12)
    end if
         
    if (r2.ne.0) then
      allocate(IndArr2(r2,bino))
      IndArr2 = IndCombis(1:r2,1:bino,r2,r12)
    end if
    
    if (r12.eq.0) then
      CalcIndsTensorProd1(0:2,1,1) = 1
    
    else
      do i=1,str12
      
        inds = IndCombisEq(1:r12,i,r12,4)
      
        cnt = 0
        do j=1,bino
              
          do k=1,r1
            inds1(k) = inds(IndArr1(k,j))-1
          end do
          
          do k=1,r2
            inds2(k) = inds(IndArr2(k,bino-j+1))-1
          end do
    
          x = IndToSymInd(CalcIndicesToInt(r1,inds1))
          y = IndToSymInd(CalcIndicesToInt(r2,inds2))
            
          k = 1
          hit = .false.
          do while((k.le.cnt).and.(hit.eqv..false.))
            if ((CalcIndsTensorProd1(1,k,i).eq.x).and. &
              & (CalcIndsTensorProd1(2,k,i).eq.y)) then
            
              CalcIndsTensorProd1(0,k,i) = CalcIndsTensorProd1(0,k,i) + 1
              hit = .true.
            end if
            k = k+1
          end do
            
          if (hit.eqv..false.) then
            cnt = cnt+1
            CalcIndsTensorProd1(1,cnt,i) = IndToSymInd(CalcIndicesToInt(r1,inds1))
            CalcIndsTensorProd1(2,cnt,i) = IndToSymInd(CalcIndicesToInt(r2,inds2))
            CalcIndsTensorProd1(0,cnt,i) = 1
          end if
            
        end do
            
      end do
    
    end if
  
  end function CalcIndsTensorProd1 
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndsTensorProd2(r)
  !
  !  sets global variable IndsTensorProd2
  !  IndsTensorProd2(r1,r2,mu1,mu2,0:1) gives the index of 
  !  T12=SymmetricTensorProduct(T1,T2) to which T1(mu1)*T2(mu2)
  !  contributes and the multiplicity with which it contributes
  !  (T1,T2 are symmetric tensors)
  !
  !  example: r1=1,r2=2 
  !  ==> T12(mu,nu,rho) = T1(mu)*T2(nu,rho) + T1(nu)*T2(mu,rho) +
  !                       T1(rho)*T2(mu,nu)
  !  T12 has 20 independent elements
  !  e.g. T12[5] = T12(0,1,1) 
  !              = T1(0)*T2(1,1) + T1(1)*T2(0,1) + T1(1)*T2(0,1)
  !              = 1*T1[1]*T2[5] + 2*T1[2]*T2[2] 
  !        
  !  ==>  IndTensorProd2(1,2,2,2,0:1) = (2,5),
  !       IndTensorProd2(1,2,1,5,0:1) = (1,5)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndsTensorProd2(r)
  
    integer, intent(in) :: r
    integer :: r1, r2, r12, str, str1, str2
    integer :: i1, i2, i12, k, n
    integer, dimension(:), allocatable :: inds1, inds2
    
    if (r<0) then
      write (*,*) r, ' is not a valid rank for a tensor'
      stop
    end if

    str = RankToSize(4,r)    
    if (allocated(IndsTensorProd2)) then
      deallocate(IndsTensorProd2)
    end if
    allocate(IndsTensorProd2(0:1,str,str,0:r,0:r))
    IndsTensorProd2 = 0

    ! case r1=0 or r2=0
    IndsTensorProd2(0:1,1,1,0,0) = 1
    do r1=1,r
      str1 = RankToSize(4,r1)
      do k=1,str1
        IndsTensorProd2(0,k,1,r1,0) = 1
        IndsTensorProd2(1,k,1,r1,0) = k
        IndsTensorProd2(0,1,k,0,r1) = 1
        IndsTensorProd2(1,1,k,0,r1) = k
      end do
    end do

    do r1=1,r
      str1 = RankToSize(4,r1)
      if (allocated(inds1)) then
        deallocate(inds1)
      end if
      allocate(inds1(r1))
      
      do r2=1,r-r1
        str2 = RankToSize(4,r2)
        if (allocated(inds2)) then
          deallocate(inds2)
        end if
        allocate(inds2(r2))
        
        r12 = r1+r2
        
        do i1=1,str1
          inds1 = IndCombisEq(1:r1,i1,r1,4) - 1
          
          do i2=1,str2
            inds2 = IndCombisEq(1:r2,i2,r2,4) - 1
        
            i12 = CalcIndicesToInt(r1,inds1)*(4**r2) + CalcIndicesToInt(r2,inds2)
            i12 = IndToSymInd(i12)
            
            IndsTensorProd2(1,i2,i1,r2,r1) = i12
            
            k=1
            n=0
            do while (n.eq.0) 
              if (IndsTensorProd1(1,k,i12,r2,r1).eq.i1) then
                n = IndsTensorProd1(0,k,i12,r2,r1)
              end if
              k = k+1
            end do
            IndsTensorProd2(0,i2,i1,r2,r1) = n
          
          end do
            
        end do
        
      end do
      
    end do
  
  end subroutine SetIndsTensorProd2

  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcSymTensorProd1(r1,r2,tensor1,tensor2)
  !
  !  calculates symmetric tensor product of the symmtric 
  !  tensors tensor1 and tensor2 of reank r1 and r2
  !  using CalcSymElement
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcSymTensorProd1(r1,r2,tensor1,tensor2)
  
    integer, intent(in) :: r1, r2
    complex*16, dimension(RankToSize(4,r1)), intent(in) :: tensor1
    complex*16, dimension(RankToSize(4,r2)), intent(in) :: tensor2
    complex*16, dimension(:), allocatable :: CalcSymTensorProd1
    integer :: r12, str, i 
  
    r12 = r1+r2
    str = RankToSize(4,r12)
    allocate(CalcSymTensorProd1(1:str))

    do i=1,str
      CalcSymTensorProd1(i) = CalcSymElement(r1,r2,tensor1,tensor2,i)
    end do
    
  end function CalcSymTensorProd1
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcSymElement(r1,r2,tensor1,tensor2,mui)
  !
  !  calculates the element mui (given as "symmetric integer-index")
  !  of the symmetrized product of the symmetric tensors tensor1 
  !  and tensor2 of rank r1 and r2 using the table IndsTensorProd1
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcSymElement(r1,r2,tensor1,tensor2,mui)
  
    integer, intent(in) :: r1, r2, mui
    complex*16, dimension(RankToSize(4,r1)), intent(in) :: tensor1
    complex*16, dimension(RankToSize(4,r2)), intent(in) :: tensor2
    complex*16 :: CalcSymElement
    integer :: r12, bino, i, j, ind1, ind2, n
  
    r12 = r1+r2
    bino = BinomTable(r1,r12)
    
    CalcSymElement = 0
    i = 1
    j = 1
    do while (i.le.bino)
      ind1 = IndsTensorProd1(1,j,mui,r2,r1)
      ind2 = IndsTensorProd1(2,j,mui,r2,r1)
      n = IndsTensorProd1(0,j,mui,r2,r1)
      
      CalcSymElement = CalcSymElement + n*tensor1(ind1)*tensor2(ind2)
      
      i = i+n
      j = j+1
    end do
    
  end function CalcSymElement
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcSymTensorProd2(r1,r2,k1,k2,tensor1,tensor2)
  !
  !  calculates symmetric tensor product of the symmetric 
  !  tensors tensor1 and tensor2 of rank r1 and r2 
  !  containing k1 and k2 metric tensors
  !  (calculation uses AddTensorProd2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcSymTensorProd2(r1,r2,k1,k2,tensor1,tensor2)
  
    integer, intent(in) :: r1, r2, k1, k2
    complex*16, dimension(RankToSize(4,r1)), intent(in) :: tensor1
    complex*16, dimension(RankToSize(4,r2)), intent(in) :: tensor2
    complex*16, dimension(:), allocatable :: CalcSymTensorProd2
    integer :: si1, si2, r12, str, str1, str2, i1, i2, j1, j2, i12, n
    integer, dimension(:), allocatable :: inds1, inds2
  
    r12 = r1+r2
    str = RankToSize(4,r12)
    allocate(CalcSymTensorProd2(1:str))
    CalcSymTensorProd2 = 0
    
    si1 = r1-2*k1
    si2 = r2-2*k2

    do j1=0,min(si1,3)
    
      str1 = RankToSize(j1,r1)
      if (allocated(inds1)) then
        deallocate(inds1)
      end if
      allocate(inds1(str1))
      inds1 = SingleInds(1:str1,j1)
      
      do j2=0,min(si2,3)
       
        str2 = RankToSize(j2,r2)
        if (allocated(inds2)) then
          deallocate(inds2)
        end if
        allocate(inds2(str2))
        inds2 = SingleInds(1:str2,j2)
        
        call AddTensorProd2(r1,r2,str1,str2,inds1,inds2,tensor1,tensor2,CalcSymTensorProd2)
      
      end do
            
    end do
    
  end function CalcSymTensorProd2
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddTensorProd2(r1,r2,inds1,inds2,tensor1,tensor2,tensor12)
  !
  !  calculate contributions of tensor1(mu1)*tensor2(mu2) to tensor12
  !  for all mu1 in inds1 and all mu2 in inds2
  !  (using the table IndsTensorProd2)
  !
  !  example: r1=r2=2, n1=n2=2, inds1=(2,4), inds2=(5,10)
  !  
  !  T1[2]*T2[5]  = T1^01*T2^11 --> contributes with multiplicity 3 to T12^0111 = T12[11]
  !  T1[2]*T2[10] = T1^01*T2^33 -->          "                    1  " T12^0133 = T12[16]
  !  T1[4]*T2[5]  = T1^03*T2^11 -->          "                    1  " T12^0113 = T12[13]
  !  T1[4]*T2[10] = T1^03*T2^33 -->          "                    3  " T12^0333 = T12[20]
  !
  !  ==> call AddTensorProd2:
  !  T12[11] = T12[11] + 3*T1[2]*T2[5],      T12[13] = T12[13] + 1*T1[4]*T2[5],
  !  T12[16] = T12[16] + 1*T1[2]*T2[10],     T12[20] = T12[20] + 3*T1[4]*T2[20],
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine AddTensorProd2(r1,r2,n1,n2,inds1,inds2,tensor1,tensor2,tensor12)
  
    integer, intent(in) :: r1, r2, n1, n2
    integer, intent(in) :: inds1(:), inds2(:)
    complex*16, intent(in) :: tensor1(RankToSize(4,r1)), tensor2(RankToSize(4,r2))
    complex*16, intent(inout) :: tensor12(RankToSize(4,r1+r2))
    integer :: i1, i2, ind1, ind2, ind12, n
    
    do i1=1,n1
      ind1 = inds1(i1)
      
      do i2=1,n2
        ind2 = inds2(i2)
        
        ind12 = IndsTensorProd2(1,ind2,ind1,r2,r1)
        n = IndsTensorProd2(0,ind2,ind1,r2,r1)
            
        tensor12(ind12) = tensor12(ind12) + n*tensor1(ind1)*tensor2(ind2)
      
      end do
            
    end do
    
  end subroutine AddTensorProd2
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcOrdTensorProd(r1,r2,tensor1,tensor2,sym)
  !
  !  calculates the ordered tensor product 
  !  T_12^{mu_1,...,mu_{r1+r2}} = T_1^{mu_1,...,mu_r1} T_2^{mu_{r1+1},...,mu_{r1+r2}}
  !
  !  sym = 0:  T1 and T2 are not symmetric
  !  sym = 1:  T1 and T2 are symmetric
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcOrdTensorProd(r1,r2,tensor1,tensor2,sym)
  
    integer, intent(in) :: r1, r2, sym
    complex*16, dimension(sym:RankToSize(5-sym,r1)), intent(in) :: tensor1
    complex*16, dimension(sym:RankToSize(5-sym,r2)), intent(in) :: tensor2
    complex*16, dimension(:), allocatable :: CalcOrdTensorProd
    integer :: i, r12, str, str1, str2, ind1, ind2 
  
    r12 = r1+r2
    str = RankToSize(5-sym,r12)
    str1 = RankToSize(5-sym,r1)
    str2 = RankToSize(5-sym,r2)
    allocate(CalcOrdTensorProd(sym:str))
  
    if (sym.eq.1) then
      do i=1,str
        ind1 = IndsTensorProd1(1,1,i,r2,r1)
        ind2 = IndsTensorProd1(2,1,i,r2,r1)
      
        CalcOrdTensorProd(i) = tensor1(ind1)*tensor2(ind2)
      end do
      
    else
      do ind1=0,str1
        do ind2=0,str2
          CalcOrdTensorProd(ind1*(4**r2)+ind2) = tensor1(ind1)*tensor2(ind2)
        end do
      end do
      
    end if
  
  end function CalcOrdTensorProd
  
  
end module TensorManipulations