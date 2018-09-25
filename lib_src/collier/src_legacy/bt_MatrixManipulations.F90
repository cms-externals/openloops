!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ********************************
!  *  module MatrixManipulations  *
!  *      by Lars Hofer           *
!  ********************************
!
!  functions and subroutines
!  CalcSubmatrix, CalcDet, CalcMatTild_ij, CalcMatTild, CalcMatTildSym,
!  CalcInvMat_ij, CalcInvMat, CalcInvMatSym
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module MatrixManipulations

  implicit none
  
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcSubmatrix(m,n,mat,i,j)
  !
  !  determines submatrix which is obtained from mxn matrix by
  !  ommitting the ith row and the jth column
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcSubmatrix(m,n,mat,i,j)
  
    integer, intent(in) :: m, n, i, j
    complex*16, intent(in) :: mat(n,m) 
    complex*16 :: CalcSubmatrix(n-1,m-1)
    integer :: i1, j1
  
    do i1=1,i-1
      do j1=1,j-1
        CalcSubMatrix(i1,j1) = mat(i1,j1)
      end do
      do j1=j+1,n
        CalcSubmatrix(i1,j1-1) = mat(i1,j1)
      end do
    end do
    do i1=i+1,m
      do j1=1,j-1
        CalcSubMatrix(i1-1,j1) = mat(i1,j1)
      end do
      do j1=j+1,n
        CalcSubmatrix(i1-1,j1-1) = mat(i1,j1)
      end do
    end do 

  end function CalcSubmatrix
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcDet(n,mat)
  !  
  !  calculates determinant of (n x n) matrix mat (recursively)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function CalcDet(n,mat) result(det)
  
    integer, intent(in) :: n
    complex*16, intent(in) :: mat(n,n)
    complex*16 :: det, signum, submat(n-1,n-1)
    integer :: i
    
    if (n .eq. 1) then
      ! case n=1
      det = mat(1,1)

    else

      det = 0
      signum = 1d0
      do i=1,n

        ! construct submatrix for (1,i)-sub-determinant
        submat = CalcSubmatrix(n,n,mat,1,i)

        ! expand determinant
        det = det + signum * mat(1,i) * CalcDet(n-1,submat)
        signum = -signum

      end do

    end if

  end function CalcDet
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcMatTild_ij(n,mat,i,j)
  !
  !  calculates (-1)^(i+j)*det_ij(mat) for the matrix mat
  !  according to def. (2.14) in hep-ph/0509141
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcMatTild_ij(n,mat,i,j)
  
    integer, intent(in) :: n, i, j
    complex*16, intent(in) :: mat(n,n)
    complex*16 :: submat(n-1,n-1), CalcMatTild_ij
    
    submat = CalcSubmatrix(n,n,mat,i,j)
    CalcMatTild_ij = (-1d0)**(i+j) * CalcDet(n-1,submat)
  
  end function CalcMatTild_ij
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcMatTild(n,mat)
  !
  !  calculates the matrix of elements CalcMatTild_ij
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcMatTild(n,mat)
  
    integer, intent(in) :: n
    complex*16 :: mat(n,n), CalcMatTild(n,n)
    integer :: i, j
    
    do i=1,n
      do j=1,n
        CalcMatTild(j,i) = CalcMatTild_ij(n,mat,i,j)
      end do
     end do
    
  end function CalcMatTild
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcMatTildSym(n,mat)
  !
  !  calculates the matrix of elements CalcMatTild_ij
  !  for symmetric mat
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcMatTildSym(n,mat)
  
    integer, intent(in) :: n
    complex*16 :: mat(n,n), CalcMatTildSym(n,n)
    integer :: i, j
    
    do i=1,n
      do j=i,n
        CalcMatTildSym(j,i) = CalcMatTild_ij(n,mat,i,j)
        CalcMatTildSym(i,j) = CalcMatTildSym(j,i)
      end do
    end do
    
  end function CalcMatTildSym

  
  
  
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcInvMat_ij(n,det,mat,i,j)
  !
  !  calculates the (i,j)-th element of the inverse 
  !  of the nxn matrix mat
  !  using (2.13) of hep-ph/0509141v2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcInvMat_ij(n,det,mat,i,j)
  
    integer, intent(in) :: n
    complex*16, intent(in) :: det, mat(n,n)
    complex*16 :: CalcInvMat_ij
    integer :: i,j

    if (n.eq.1) then
      ! case n=1
      CalcInvMat_ij = 1/det
    
    else
      CalcInvMat_ij = CalcMatTild_ij(n,mat,j,i)/det

    end if  

  end function CalcInvMat_ij
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcInvMat(n,det,mat)
  !
  !  calculates inverse of the nxn matrix mat
  !  using (2.13) of hep-ph/0509141v2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcInvMat(n,det,mat)
  
    integer, intent(in) :: n
    complex*16, intent(in) :: det, mat(n,n)
    complex*16 :: CalcInvMat(n,n)
    integer :: i,j

    do i=1,n
      do j=1,n
        CalcInvMat(j,i) = CalcInvMat_ij(n,det,mat,i,j)
      end do
    end do  

  end function CalcInvMat
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcInvMatSym(n,det,mat)
  !
  !  calculates inverse of the symmetric nxn matrix mat
  !  using (2.13) of hep-ph/0509141v2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcInvMatSym(n,det,mat)
  
    integer, intent(in) :: n
    complex*16, intent(in) :: det, mat(n,n)
    complex*16 :: CalcInvMatSym(n,n)
    integer :: i,j

    do i=1,n
      do j=i,n
        CalcInvMatSym(i,j) = CalcInvMat_ij(n,det,mat,i,j)
        CalcInvMatSym(j,i) = CalcInvMatSym(i,j)
      end do
    end do  

  end function CalcInvMatSym
  
  
end module MatrixManipulations