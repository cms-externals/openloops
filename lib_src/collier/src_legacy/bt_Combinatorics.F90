!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  **************************
!  *  module ColiCombinatorics  *
!  *     by Lars Hofer      *
!  **************************
!
!
!  global variables:
!  BinomTable, IndCombis, IndCombisEq, DropIndCombisEq
!
!  functions and subroutines:
!  SetBinomTable, CalcBinomTable, CalcBino, SetIndCombis, CalcIndCombis,
!  SetIndCombisEq, CalcIndCombisEq, SetDropIndCombisEq, CalcDropIndCombisEq, 
!  CalcPosIndCombisEq, CalcFactorial, CalcOrderedCombis, CalcOrderedCombis0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module ColiCombinatorics

  implicit none
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  global variables BinomTable, IndCombis, IndCombisEq, DropIndCombisEq
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  integer, dimension(:,:), allocatable :: BinomTable
  integer, dimension(:,:,:,:), allocatable :: IndCombis
  integer, dimension(:,:,:,:), allocatable :: IndCombisEq
  integer, dimension(:,:,:,:,:), allocatable :: DropIndCombisEq
  integer, dimension(:,:,:), allocatable :: CntInds
  
  
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetBinomTable(N)
  !
  !  sets the global variable BinomTable to the value CalcBinomTable(N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetBinomTable(N)
    
    integer, intent(in) :: N
    
    if (N<0) then
      write (*,*) N, ' is not a non-negative integer'
      stop
    end if
    
    if (allocated(BinomTable)) then
      deallocate(BinomTable)
    end if
    allocate(BinomTable(0:N,0:N))
    
    BinomTable = CalcBinomTable(N)
  
  end subroutine SetBinomTable



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcBinomTable(N)
  !
  !  calculates the table
  !  BinomTable = (((1,0), (1,1), 0, ..., 0),
  !                ((2,0), (2,1), (2,2), 0, ..., 0),
  !                ....
  !                ((N,0), (N,1), (N,2), ...., (N,N))
  !  of all binomial coefficients (n,k) ("n over k") up to n=N
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcBinomTable(N)
  
    integer, intent(in) :: N
    integer, dimension(0:N,0:N) :: CalcBinomTable
    integer :: i,j

    CalcBinomTable = 0
    do i=0,N
      do j=0,i
        CalcBinomTable(j,i) = CalcBino(i,j)
      end do
    end do
    
  end function CalcBinomTable
    
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcBino(n,k)
  !
  !  calculates binomial coefficient "n over k"
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive function CalcBino(n,k) result(bino)
  
    integer, intent(in) :: k,n
    integer bino
  
    if ((k < 0).or.(n < 0).or.(k > n)) then
      write (*,*) 'binomial coefficient ', n, ' over ', k, &
                 & ' not defined'
      stop
    end if
    
    if ((k.eq.0).or.(k.eq.n)) then
      bino = 1
    else
      bino = CalcBino(n-1,k-1) + CalcBino(n-1,k)
    end if
    
  end function CalcBino
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndCombis(N)
  !
  !  sets the global variable IndCombis to the value CalcIndCobis(N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndCombis(N)
  
    integer, intent(in) :: N
    integer :: binoMax
    
    if (N<1) then
      write (*,*) N, ' is not a positive integer'
      stop
    end if
    
    ! determine maximum number of index combinations
    if (mod(N,2).eq.0) then
      binoMax = BinomTable(N/2,N)
    else
      binoMax = BinomTable((N-1)/2,N)
    end if
    
    if (allocated(IndCombis)) then
      deallocate(IndCombis)
    end if
    allocate(IndCombis(N,binoMax,N,N))
    
    IndCombis = CalcIndCombis(N)
  
  end subroutine SetIndCombis
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndCombis(N)
  !
  !  calculates the table
  !  CalcIndCombis(N) = ((COC(1,1,0), 0, .... 0),
  !                      (COC(2,1,0), COC(2,2,0), 0, ..., 0),
  !                      ....             
  !                      (COC(N,1,0), COC(N,2,0), ..., COC(N,N,0))
  !  (COC = CalcOrderedCombis)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndCombis(N)
  
    integer, intent(in) :: N
    integer, dimension(:,:,:,:), allocatable :: CalcIndCombis
    integer :: bino, binoMax, i, j
    
    ! determine maximum number of index combinations
    if (mod(N,2).eq.0) then
      binoMax = BinomTable(N/2,N)
    else
      binoMax = BinomTable((N-1)/2,N)
    end if
    
    allocate(CalcIndCombis(N,binoMax,N,N))
    CalcIndCombis = 0
    
    do i=1,N    
      do j=1,i        
        bino = BinomTable(j,i)
        CalcIndCombis(1:j,1:bino,j,i) = CalcOrderedCombis(i,j,0)
      end do      
    end do
    
  end function CalcIndCombis
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndCombisEq(N,k)
  !
  !  sets the global variable IndCombisEq to the value 
  !  CalcIndCombisEq(N,k)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndCombisEq(N,k)
  
    integer, intent(in) :: N,k
    integer :: binoMax
    
    if (N<1) then
      write (*,*) N, ' is not a positive integer'
      stop
    end if
    
    ! determine maximum number of index combinations
    binoMax = BinomTable(k,N+k-1)
    
    if (allocated(IndCombisEq)) then
      deallocate(IndCombisEq)
    end if
    allocate(IndCombisEq(k,binoMax,k,N))
    
    IndCombisEq = CalcIndCombisEq(N,k)
  
  end subroutine SetIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndCombisEq(N,k)
  !
  !  calculates the table
  !  CalcIndCombis(N) = ((COC(1,1,1), 0, .... 0),
  !                      (COC(2,1,1), COC(2,2,1), 0, ..., 0),
  !                      ....             
  !                      (COC(N,1,1), COC(N,2,1), ..., COC(N,k,0))
  !  (COC = CalcOrderedCombis)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndCombisEq(N,k)
  
    integer, intent(in) :: N, k
    integer, dimension(:,:,:,:), allocatable :: CalcIndCombisEq
    integer :: bino, i, j
    
    allocate(CalcIndCombisEq(k,BinomTable(k,N+k-1),k,N))
    CalcIndCombisEq = 0
    
    do i=1,N    
      do j=1,k        
        bino = BinomTable(j,i+j-1)
        CalcIndCombisEq(1:j,1:bino,j,i) = CalcOrderedCombis(i,j,1)
      end do      
    end do
    
  end function CalcIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetDropIndCombisEq(Nmax,k)
  !
  !  set global variable DropIndCombisEq to
  !  DropIndCombisEq = (((CDICE(2,1,1), 0, ..., 0),
  !                      (CDICE(2,2,1), 0, ..., 0),
  !                      ...
  !                      (CDICE(2,kmax,1),0,..., 0)),
  !                     ((CDICE(3,1,1),CDICE(3,1,2),0,...,0),
  !                      ...
  !                      (CDICE(3,kmax,1),CDICE(3,kmax,2),0,...,0)),
  !                     ((CDICE(Nmax,1,1),...,CDICE(Nmax,kmax,Nmax-1)),
  !                      ...
  !                      ((CDICE(Nmax,kmax,1),...,CDICE(Nmax,kmax,Nmax-1)))
  !  CDICE = CalcDropIndCombisEq
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetDropIndCombisEq(Nmax,kmax)
  
    integer, intent(in) :: Nmax, kmax
    integer :: BinoMax1, BinoMax2, N, k, nd
    
    BinoMax1 = BinomTable(Nmax/2,Nmax)
    BinoMax2 = BinomTable(kmax,Nmax+kmax-2)
    
    if (allocated(DropIndCombisEq)) then
      deallocate(DropIndCombisEq)
    end if
    allocate(DropIndCombisEq(BinoMax2,BinoMax1,Nmax-1,0:kmax,2:Nmax))
    DropIndCombisEq = 0
    
    do N=2,Nmax
      do nd=1,N-1
        DropIndCombisEq(1,1:BinomTable(nd,N),nd,0,n) = 1
        do k=1,kmax
          DropIndCombisEq(1:,1:,nd,k,N) = CalcDropIndCombisEq(N,k,nd)
        end do
      end do
    end do
    
  end subroutine SetDropIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcDropIndCombisEq(N,k,nd)
  !
  !  calculates table of elements of IndCombisEq(N,k) which 
  !  survive when elements (i_1,...,i_nd) in (1,...,N) are dropped
  !
  !  example:
  !  IndCombisEq(3,2) = ((1,1), (1,2), (1,3), (2,2), (2,3), (3,3))
  !                        1      2      3      4      5      6
  !
  !  drop one element
  !  --> possible element to be dropped: ((1),(2),(3))
  !                                        1   2   3
  !  ==> CalcDropIndCombisEq(3,2,1) = ((4,5,6),(1,3,6),(1,2,4))
  !
  !  drop two elements 
  !  --> possible elements to be dropped: ((1,2),(1,3),(2,3))
  !                                          1     2     3
  !  ==> CalcDropIndCombisEq(3,2,2) = ((6),(4),(1))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcDropIndCombisEq(N,k,nd)
  
    integer, intent(in) :: N, k, nd
    integer, dimension(:,:), allocatable :: CalcDropIndCombisEq, inds
    integer :: bino, bino_nd, i, j, m, pos, vals(nd), p, combis_nd
    logical :: contains_j
    
    bino = BinomTable(k,N+k-1)
    bino_nd = BinomTable(k,N+k-nd-1)
    combis_nd = BinomTable(nd,N)
    
    allocate(inds(k,bino))
    inds = IndCombisEq(1:k,1:bino,k,N)
    
    allocate(CalcDropIndCombisEq(bino_nd,combis_nd))
    
    do j=1,combis_nd
      pos = 1
      vals = IndCombis(1:nd,j,nd,N)
    
      do i=1,bino
        contains_j = .false.
        
        do m=1,k
          do p=1,nd
            if (inds(m,i).eq.vals(p)) then
              contains_j = .true.
            end if
          end do
        end do
        
        if (contains_j.eqv..false.) then
          CalcDropIndCombisEq(pos,j) = i
          pos = pos+1
        end if
        
      end do      
    end do
    
  end function CalcDropIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcPosIndCombisEq(N,r,inds)
  !
  !  determines position of index combination inds 
  !  within IndCombisEq(N,r)
  !
  !  example:
  !  IndCombisEq(3,2) = ((1,1), (1,2), (1,3), (2,2), (2,3), (3,3))
  !  ==> CalcPosIndCombisEq(3,2,(2,3)) = 5
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive function CalcPosIndCombisEq(N,r,inds) result(pos)
  
    integer, intent(in) :: N, r, inds(r)
    integer :: k, i1, indsm1(r-1), pos
    
    if (r.eq.1) then
      pos = inds(1)
    
    else
      pos = 0
      i1 = inds(1)
      indsm1 = inds(2:r)-i1+1
      
      do k=1,i1-1
        pos = pos + BinomTable(r-1,N-k+r-1)
      end do
      
      pos = pos + CalcPosIndCombisEq(N-i1+1,r-1,indsm1)
      
    end if
  
  end function CalcPosIndCombisEq

  
  


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcFactorial(n)
  !
  !  calculates the factorial of n
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive function CalcFactorial(n) result(fact)
  
    integer, intent(in) :: n
    integer :: fact
    
    if (n < 0) then 
      write (*,*) 'factorial not defined for negative integer'
      stop
    end if
    
    if (n .eq. 0) then
      fact = 1
    else
      fact = n * CalcFactorial(n-1)
    end if
    
  end function CalcFactorial

  
  
  
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  functions 
  !  CalcOrderedCombis(n,k,opt)    CalcOrderedCombis0(n,k,opt)
  !
  !  determines all ordered(!) combinations taking 
  !  k different (opt=0) or k not necessarily different (opt=1) 
  !  numbers out of 1,...,n (CalcOrderedCombis)
  !  or out of 0,...,n (CalcOrderedCombis0)
  !
  !  example:
  !  CalcOrderedCombis(3,2,0) = ((1,2), (1,3), (2,3))
  !  CalcOrderedCombis(3,2,1) = ((1,1), (1,2), (1,3), (2,2), (2,3), (3,3))
  !  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive function CalcOrderedCombis(n,k,opt) result(combis)
  
    integer, intent(in) :: n, k, opt
    integer, dimension(:,:), allocatable :: combis
    integer, dimension(:,:), allocatable :: CombiRec
    integer :: i, j, BinoRec, bino, num, lb
    
    bino = BinomTable(k,n+opt*(k-1))
    allocate(combis(k,bino))
    
    if (k.eq.1) then
      ! trivial case k=1
      ! result ((1),(2),(3),...,(n))
      do i=1,bino
        combis(1,i) = i
      end do
    
    else
      ! k-1 numbers out of n
      BinoRec = BinomTable(k-1,n+opt*(k-2))
      allocate(CombiRec(k-1,BinoRec))
      CombiRec = CalcOrderedCombis(n,k-1,opt)
      
      ! add the kth number 
      num = 1
      do i=1,BinoRec
        lb = CombiRec(k-1,i) + 1 - opt
        do j=lb,n
          combis(1:k-1,num) = CombiRec(1:k-1,i)
          combis(k,num)=j
          num = num + 1
        end do
      end do
    end if
    
  end function CalcOrderedCombis
  
  
  
  recursive function CalcOrderedCombis0(n,k,opt) result(combis)
  
    integer, intent(in) :: n, k, opt
    integer, dimension(:,:), allocatable :: combis
    integer, dimension(:,:), allocatable :: CombiRec
    integer :: i, j, bino, BinoRec, num, lb
    
    bino = BinomTable(k,n+1+opt*(k-1))
    allocate(combis(k,bino))
    
    if (k.eq.1) then
      ! trivial case k=1
      ! result ((0),(1),(2),...,(n))
      do i=1,n+1
        combis(1,i) = i-1
      end do
    
    else
      ! k-1 numbers out of n
      BinoRec = BinomTable(k-1,n+1+opt*(k-2))
      allocate(CombiRec(k-1,BinoRec))
      CombiRec = CalcOrderedCombis0(n,k-1,opt)
      
      ! add the kth number 
      num = 1
      do i=1,BinoRec
        lb = CombiRec(k-1,i) + 1 - opt
        do j=lb,n
          combis(1:k-1,num) = CombiRec(1:k-1,i)
          combis(k,num)=j
          num = num + 1
        end do
      end do
    end if
    
  end function CalcOrderedCombis0
  
  
end module ColiCombinatorics