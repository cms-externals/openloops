
! **********************************************************************
module ol_heltables_heftpphjj_hgggg_1
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
    H1(1) = [0], &
    H2(2) = [-1,1], &
    H3(2) = [-1,1], &
    H4(2) = [-1,1], &
    H5(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n3(3,61), n4(4,18), n5(5,3)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x2(2,2,4), t3x4(2,4,21), t3x16(2,16,36), t4x4(3,4,6), t4x8(3,8,12), t5x8(4,8,3)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(16,5)
  integer,           save :: exthel(16,5)
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
  call heltable([1,2,2,2,8], n5(:,1), t5x8(:,:,1))
  call heltable([1,2,2,2,8], n5(:,2), t5x8(:,:,2))
  call heltable([1,2,2,2,8], n5(:,3), t5x8(:,:,3))
  call heltable([1,2,2], n3(:,1), t3x2(:,:,1))
  call heltable([2,2,2,8], n4(:,1), t4x8(:,:,1))
  call heltable([2,2,2,8], n4(:,2), t4x8(:,:,2))
  call heltable([2,2,2,8], n4(:,3), t4x8(:,:,3))
  call heltable([1,2,2], n3(:,2), t3x2(:,:,2))
  call heltable([2,2,2,8], n4(:,4), t4x8(:,:,4))
  call heltable([2,2,2,8], n4(:,5), t4x8(:,:,5))
  call heltable([2,2,2,8], n4(:,6), t4x8(:,:,6))
  call heltable([1,2,2], n3(:,3), t3x2(:,:,3))
  call heltable([2,2,2,8], n4(:,7), t4x8(:,:,7))
  call heltable([2,2,2,8], n4(:,8), t4x8(:,:,8))
  call heltable([2,2,2,8], n4(:,9), t4x8(:,:,9))
  call heltable([1,2,2], n3(:,4), t3x2(:,:,4))
  call heltable([2,2,2,8], n4(:,10), t4x8(:,:,10))
  call heltable([2,2,2,8], n4(:,11), t4x8(:,:,11))
  call heltable([2,2,2,8], n4(:,12), t4x8(:,:,12))
  call heltable([2,2,4], n3(:,5), t3x4(:,:,1))
  call heltable([1,2,2,4], n4(:,13), t4x4(:,:,1))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,2))
  call heltable([1,2,2,4], n4(:,14), t4x4(:,:,2))
  call heltable([2,2,4], n3(:,7), t3x4(:,:,3))
  call heltable([1,2,2,4], n4(:,15), t4x4(:,:,3))
  call heltable([2,2,4], n3(:,8), t3x4(:,:,4))
  call heltable([1,2,2,4], n4(:,16), t4x4(:,:,4))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,5))
  call heltable([1,2,2,4], n4(:,17), t4x4(:,:,5))
  call heltable([2,2,4], n3(:,10), t3x4(:,:,6))
  call heltable([1,2,2,4], n4(:,18), t4x4(:,:,6))
  call heltable([2,2,4], n3(:,11), t3x4(:,:,7))
  call heltable([2,2,4], n3(:,12), t3x4(:,:,8))
  call heltable([2,2,4], n3(:,13), t3x4(:,:,9))
  call heltable([2,2,4], n3(:,14), t3x4(:,:,10))
  call heltable([2,2,4], n3(:,15), t3x4(:,:,11))
  call heltable([2,2,4], n3(:,16), t3x4(:,:,12))
  call heltable([2,2,4], n3(:,17), t3x4(:,:,13))
  call heltable([2,2,4], n3(:,18), t3x4(:,:,14))
  call heltable([1,4,4], n3(:,19), t3x4(:,:,15))
  call heltable([2,2,4], n3(:,20), t3x4(:,:,16))
  call heltable([2,2,4], n3(:,21), t3x4(:,:,17))
  call heltable([2,2,4], n3(:,22), t3x4(:,:,18))
  call heltable([1,4,4], n3(:,23), t3x4(:,:,19))
  call heltable([2,2,4], n3(:,24), t3x4(:,:,20))
  call heltable([1,4,4], n3(:,25), t3x4(:,:,21))
  call heltable([2,8,16], n3(:,26), t3x16(:,:,1))
  call heltable([2,8,16], n3(:,27), t3x16(:,:,2))
  call heltable([2,8,16], n3(:,28), t3x16(:,:,3))
  call heltable([2,8,16], n3(:,29), t3x16(:,:,4))
  call heltable([2,8,16], n3(:,30), t3x16(:,:,5))
  call heltable([2,8,16], n3(:,31), t3x16(:,:,6))
  call heltable([2,8,16], n3(:,32), t3x16(:,:,7))
  call heltable([2,8,16], n3(:,33), t3x16(:,:,8))
  call heltable([2,8,16], n3(:,34), t3x16(:,:,9))
  call heltable([2,8,16], n3(:,35), t3x16(:,:,10))
  call heltable([2,8,16], n3(:,36), t3x16(:,:,11))
  call heltable([2,8,16], n3(:,37), t3x16(:,:,12))
  call heltable([2,8,16], n3(:,38), t3x16(:,:,13))
  call heltable([2,8,16], n3(:,39), t3x16(:,:,14))
  call heltable([2,8,16], n3(:,40), t3x16(:,:,15))
  call heltable([4,4,16], n3(:,41), t3x16(:,:,16))
  call heltable([4,4,16], n3(:,42), t3x16(:,:,17))
  call heltable([4,4,16], n3(:,43), t3x16(:,:,18))
  call heltable([4,4,16], n3(:,44), t3x16(:,:,19))
  call heltable([4,4,16], n3(:,45), t3x16(:,:,20))
  call heltable([4,4,16], n3(:,46), t3x16(:,:,21))
  call heltable([4,4,16], n3(:,47), t3x16(:,:,22))
  call heltable([4,4,16], n3(:,48), t3x16(:,:,23))
  call heltable([4,4,16], n3(:,49), t3x16(:,:,24))
  call heltable([4,4,16], n3(:,50), t3x16(:,:,25))
  call heltable([4,4,16], n3(:,51), t3x16(:,:,26))
  call heltable([4,4,16], n3(:,52), t3x16(:,:,27))
  call heltable([4,4,16], n3(:,53), t3x16(:,:,28))
  call heltable([4,4,16], n3(:,54), t3x16(:,:,29))
  call heltable([4,4,16], n3(:,55), t3x16(:,:,30))
  call heltable([4,4,16], n3(:,56), t3x16(:,:,31))
  call heltable([4,4,16], n3(:,57), t3x16(:,:,32))
  call heltable([4,4,16], n3(:,58), t3x16(:,:,33))
  call heltable([4,4,16], n3(:,59), t3x16(:,:,34))
  call heltable([4,4,16], n3(:,60), t3x16(:,:,35))
  call heltable([4,4,16], n3(:,61), t3x16(:,:,36))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_heftpphjj_hgggg_1
