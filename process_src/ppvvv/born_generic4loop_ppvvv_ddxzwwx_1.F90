
! **********************************************************************
module ol_heltables_ppvvv_ddxzwwx_1
! **********************************************************************
  use KIND_TYPES, only: intkind2
  implicit none

  logical :: heltables_not_init = .true.

  ! helicity states of external particles
  ! integer, save :: &
  !   H1(2) = [-1,1], &
  !   H2(3) = [-1,0,1]
  !   ...
  integer, save :: &
    H1(2) = [-1,1], &
    H2(2) = [-1,1], &
    H3(3) = [-1,0,1], &
    H4(3) = [-1,0,1], &
    H5(3) = [-1,0,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(8), n3(3,40), n4(4,1)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,2), t3x18(2,18,3), t3x9(2,9,4), t3x6(2,6,4), t3x12(2,12,11), t3x108(2,108,16), t4x27(3,27,1)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(108,5)
  integer,           save :: exthel(108,5)
  integer,           save :: firstpol(5)

  contains

! **********************************************************************
subroutine init_heltables
! **********************************************************************
  use ol_helicity_init, only: heltable
  implicit none

  ! I/O helicity tables for vertices, propagators and contractions;
  ! helicity table for a vertex call: n_in/n_out are the number helicity states of the incoming/outgoing wave functions
  ! call heltable([<n_in1>, <n_in2>, ..., <n_out>], n, t)
  ! propagators only need the number of helicity configurations which is equal for the incoming and outgoing wave function
  ! n = <n>
  call heltable([2,2,4], n3(:,1), t3x4(:,:,1))
  call heltable([3,3,3,27], n4(:,1), t4x27(:,:,1))
  call heltable([2,2,4], n3(:,2), t3x4(:,:,2))
  n2(1) = 4
  call heltable([3,3,9], n3(:,3), t3x9(:,:,1))
  n2(2) = 9
  call heltable([4,3,12], n3(:,4), t3x12(:,:,1))
  call heltable([4,3,12], n3(:,5), t3x12(:,:,2))
  call heltable([3,3,9], n3(:,6), t3x9(:,:,2))
  n2(3) = 9
  call heltable([3,4,12], n3(:,7), t3x12(:,:,3))
  call heltable([3,4,12], n3(:,8), t3x12(:,:,4))
  call heltable([3,3,9], n3(:,9), t3x9(:,:,3))
  call heltable([4,3,12], n3(:,10), t3x12(:,:,5))
  call heltable([3,2,6], n3(:,11), t3x6(:,:,1))
  call heltable([2,3,6], n3(:,12), t3x6(:,:,2))
  n2(4) = 6
  n2(5) = 6
  call heltable([3,6,18], n3(:,13), t3x18(:,:,1))
  call heltable([3,3,9], n3(:,14), t3x9(:,:,4))
  call heltable([6,2,12], n3(:,15), t3x12(:,:,6))
  n2(6) = 9
  call heltable([6,2,12], n3(:,16), t3x12(:,:,7))
  call heltable([3,2,6], n3(:,17), t3x6(:,:,3))
  call heltable([2,3,6], n3(:,18), t3x6(:,:,4))
  n2(7) = 6
  n2(8) = 6
  call heltable([3,6,18], n3(:,19), t3x18(:,:,2))
  call heltable([2,6,12], n3(:,20), t3x12(:,:,8))
  call heltable([2,6,12], n3(:,21), t3x12(:,:,9))
  call heltable([3,6,18], n3(:,22), t3x18(:,:,3))
  call heltable([2,6,12], n3(:,23), t3x12(:,:,10))
  call heltable([6,2,12], n3(:,24), t3x12(:,:,11))
  call heltable([4,27,108], n3(:,25), t3x108(:,:,1))
  call heltable([27,4,108], n3(:,26), t3x108(:,:,2))
  call heltable([9,12,108], n3(:,27), t3x108(:,:,3))
  call heltable([9,12,108], n3(:,28), t3x108(:,:,4))
  call heltable([9,12,108], n3(:,29), t3x108(:,:,5))
  call heltable([9,12,108], n3(:,30), t3x108(:,:,6))
  call heltable([9,12,108], n3(:,31), t3x108(:,:,7))
  call heltable([6,18,108], n3(:,32), t3x108(:,:,8))
  call heltable([9,12,108], n3(:,33), t3x108(:,:,9))
  call heltable([9,12,108], n3(:,34), t3x108(:,:,10))
  call heltable([6,18,108], n3(:,35), t3x108(:,:,11))
  call heltable([9,12,108], n3(:,36), t3x108(:,:,12))
  call heltable([9,12,108], n3(:,37), t3x108(:,:,13))
  call heltable([6,18,108], n3(:,38), t3x108(:,:,14))
  call heltable([9,12,108], n3(:,39), t3x108(:,:,15))
  call heltable([9,12,108], n3(:,40), t3x108(:,:,16))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppvvv_ddxzwwx_1
