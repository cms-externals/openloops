
! **********************************************************************
module ol_heltables_pplnajj_neexuxdssxa_1
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
    H4(2) = [-1,1], &
    H5(2) = [-1,1], &
    H6(2) = [-1,1], &
    H7(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(12), n3(3,41)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,7), t3x8(2,8,8), t3x16(2,16,12), t3x128(2,128,14)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(128,7)
  integer,           save :: exthel(128,7)
  integer,           save :: firstpol(7)

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
  call heltable([2,2,4], n3(:,2), t3x4(:,:,2))
  call heltable([2,2,4], n3(:,3), t3x4(:,:,3))
  n2(1) = 4
  n2(2) = 4
  call heltable([4,2,8], n3(:,4), t3x8(:,:,1))
  call heltable([4,4,16], n3(:,5), t3x16(:,:,1))
  n2(3) = 8
  call heltable([4,2,8], n3(:,6), t3x8(:,:,2))
  call heltable([4,4,16], n3(:,7), t3x16(:,:,2))
  n2(4) = 8
  call heltable([2,2,4], n3(:,8), t3x4(:,:,4))
  n2(5) = 4
  call heltable([2,4,8], n3(:,9), t3x8(:,:,3))
  call heltable([4,4,16], n3(:,10), t3x16(:,:,3))
  n2(6) = 8
  call heltable([2,4,8], n3(:,11), t3x8(:,:,4))
  call heltable([4,4,16], n3(:,12), t3x16(:,:,4))
  n2(7) = 8
  call heltable([8,2,16], n3(:,13), t3x16(:,:,5))
  call heltable([8,2,16], n3(:,14), t3x16(:,:,6))
  call heltable([2,4,8], n3(:,15), t3x8(:,:,5))
  n2(8) = 8
  call heltable([2,8,16], n3(:,16), t3x16(:,:,7))
  call heltable([8,2,16], n3(:,17), t3x16(:,:,8))
  call heltable([2,2,4], n3(:,18), t3x4(:,:,5))
  n2(9) = 4
  call heltable([4,2,8], n3(:,19), t3x8(:,:,6))
  call heltable([2,8,16], n3(:,20), t3x16(:,:,9))
  call heltable([8,2,16], n3(:,21), t3x16(:,:,10))
  call heltable([2,2,4], n3(:,22), t3x4(:,:,6))
  n2(10) = 4
  call heltable([2,4,8], n3(:,23), t3x8(:,:,7))
  call heltable([2,2,4], n3(:,24), t3x4(:,:,7))
  n2(11) = 4
  call heltable([2,4,8], n3(:,25), t3x8(:,:,8))
  n2(12) = 8
  call heltable([2,8,16], n3(:,26), t3x16(:,:,11))
  call heltable([8,2,16], n3(:,27), t3x16(:,:,12))
  call heltable([16,8,128], n3(:,28), t3x128(:,:,1))
  call heltable([16,8,128], n3(:,29), t3x128(:,:,2))
  call heltable([16,8,128], n3(:,30), t3x128(:,:,3))
  call heltable([16,8,128], n3(:,31), t3x128(:,:,4))
  call heltable([8,16,128], n3(:,32), t3x128(:,:,5))
  call heltable([8,16,128], n3(:,33), t3x128(:,:,6))
  call heltable([8,16,128], n3(:,34), t3x128(:,:,7))
  call heltable([8,16,128], n3(:,35), t3x128(:,:,8))
  call heltable([8,16,128], n3(:,36), t3x128(:,:,9))
  call heltable([8,16,128], n3(:,37), t3x128(:,:,10))
  call heltable([16,8,128], n3(:,38), t3x128(:,:,11))
  call heltable([16,8,128], n3(:,39), t3x128(:,:,12))
  call heltable([8,16,128], n3(:,40), t3x128(:,:,13))
  call heltable([8,16,128], n3(:,41), t3x128(:,:,14))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_pplnajj_neexuxdssxa_1
