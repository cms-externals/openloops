
! **********************************************************************
module ol_heltables_ppvvv_bbxazz_1
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
    H3(2) = [-1,1], &
    H4(3) = [-1,0,1], &
    H5(3) = [-1,0,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(6), n3(3,23)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x9(2,9,1), t3x4(2,4,2), t3x8(2,8,2), t3x18(2,18,2), t3x6(2,6,4), t3x12(2,12,4), t3x72(2,72,8)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(72,5)
  integer,           save :: exthel(72,5)
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
  call heltable([2,3,6], n3(:,2), t3x6(:,:,1))
  n2(1) = 4
  n2(2) = 6
  call heltable([3,4,12], n3(:,3), t3x12(:,:,1))
  call heltable([2,3,6], n3(:,4), t3x6(:,:,2))
  n2(3) = 6
  call heltable([3,4,12], n3(:,5), t3x12(:,:,2))
  call heltable([3,3,9], n3(:,6), t3x9(:,:,1))
  call heltable([2,4,8], n3(:,7), t3x8(:,:,1))
  call heltable([3,2,6], n3(:,8), t3x6(:,:,3))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,2))
  n2(4) = 6
  n2(5) = 4
  call heltable([3,6,18], n3(:,10), t3x18(:,:,1))
  call heltable([3,2,6], n3(:,11), t3x6(:,:,4))
  n2(6) = 6
  call heltable([3,6,18], n3(:,12), t3x18(:,:,2))
  call heltable([4,2,8], n3(:,13), t3x8(:,:,2))
  call heltable([2,6,12], n3(:,14), t3x12(:,:,3))
  call heltable([2,6,12], n3(:,15), t3x12(:,:,4))
  call heltable([6,12,72], n3(:,16), t3x72(:,:,1))
  call heltable([6,12,72], n3(:,17), t3x72(:,:,2))
  call heltable([9,8,72], n3(:,18), t3x72(:,:,3))
  call heltable([4,18,72], n3(:,19), t3x72(:,:,4))
  call heltable([4,18,72], n3(:,20), t3x72(:,:,5))
  call heltable([9,8,72], n3(:,21), t3x72(:,:,6))
  call heltable([6,12,72], n3(:,22), t3x72(:,:,7))
  call heltable([6,12,72], n3(:,23), t3x72(:,:,8))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppvvv_bbxazz_1
