
! **********************************************************************
module ol_heltables_pphhjj_ddxbbxhh_1
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
    H5(1) = [0], &
    H6(1) = [0]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(9), n3(3,80), n4(4,1)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x1(2,1,1), t3x8(2,8,8), t3x2(2,2,10), t3x4(2,4,24), t3x16(2,16,37), t4x4(3,4,1)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(16,6)
  integer,           save :: exthel(16,6)
  integer,           save :: firstpol(6)

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
  call heltable([1,1,4,4], n4(:,1), t4x4(:,:,1))
  call heltable([2,2,4], n3(:,3), t3x4(:,:,3))
  call heltable([1,1,1], n3(:,4), t3x1(:,:,1))
  call heltable([4,4,16], n3(:,5), t3x16(:,:,1))
  call heltable([4,4,16], n3(:,6), t3x16(:,:,2))
  call heltable([1,4,4], n3(:,7), t3x4(:,:,4))
  call heltable([4,1,4], n3(:,8), t3x4(:,:,5))
  call heltable([1,4,4], n3(:,9), t3x4(:,:,6))
  call heltable([4,1,4], n3(:,10), t3x4(:,:,7))
  call heltable([1,4,4], n3(:,11), t3x4(:,:,8))
  call heltable([1,4,4], n3(:,12), t3x4(:,:,9))
  call heltable([4,1,4], n3(:,13), t3x4(:,:,10))
  call heltable([1,4,4], n3(:,14), t3x4(:,:,11))
  call heltable([4,1,4], n3(:,15), t3x4(:,:,12))
  call heltable([1,4,4], n3(:,16), t3x4(:,:,13))
  call heltable([1,4,4], n3(:,17), t3x4(:,:,14))
  call heltable([1,4,4], n3(:,18), t3x4(:,:,15))
  call heltable([2,2,4], n3(:,19), t3x4(:,:,16))
  call heltable([2,1,2], n3(:,20), t3x2(:,:,1))
  call heltable([1,2,2], n3(:,21), t3x2(:,:,2))
  n2(1) = 2
  n2(2) = 2
  call heltable([4,2,8], n3(:,22), t3x8(:,:,1))
  call heltable([4,2,8], n3(:,23), t3x8(:,:,2))
  call heltable([2,4,8], n3(:,24), t3x8(:,:,3))
  call heltable([2,1,2], n3(:,25), t3x2(:,:,3))
  n2(3) = 8
  call heltable([2,4,8], n3(:,26), t3x8(:,:,4))
  n2(4) = 8
  call heltable([2,2,4], n3(:,27), t3x4(:,:,17))
  call heltable([2,2,4], n3(:,28), t3x4(:,:,18))
  call heltable([2,1,2], n3(:,29), t3x2(:,:,4))
  call heltable([1,2,2], n3(:,30), t3x2(:,:,5))
  n2(5) = 2
  n2(6) = 2
  call heltable([4,2,8], n3(:,31), t3x8(:,:,5))
  call heltable([4,2,8], n3(:,32), t3x8(:,:,6))
  call heltable([4,2,8], n3(:,33), t3x8(:,:,7))
  call heltable([1,2,2], n3(:,34), t3x2(:,:,6))
  n2(7) = 8
  call heltable([4,2,8], n3(:,35), t3x8(:,:,8))
  n2(8) = 8
  call heltable([2,2,4], n3(:,36), t3x4(:,:,19))
  call heltable([2,2,4], n3(:,37), t3x4(:,:,20))
  call heltable([2,1,2], n3(:,38), t3x2(:,:,7))
  call heltable([2,2,4], n3(:,39), t3x4(:,:,21))
  call heltable([2,2,4], n3(:,40), t3x4(:,:,22))
  call heltable([1,2,2], n3(:,41), t3x2(:,:,8))
  call heltable([2,2,4], n3(:,42), t3x4(:,:,23))
  call heltable([2,2,4], n3(:,43), t3x4(:,:,24))
  call heltable([1,2,2], n3(:,44), t3x2(:,:,9))
  call heltable([2,1,2], n3(:,45), t3x2(:,:,10))
  n2(9) = 2
  call heltable([4,4,16], n3(:,46), t3x16(:,:,3))
  call heltable([1,16,16], n3(:,47), t3x16(:,:,4))
  call heltable([1,16,16], n3(:,48), t3x16(:,:,5))
  call heltable([4,4,16], n3(:,49), t3x16(:,:,6))
  call heltable([4,4,16], n3(:,50), t3x16(:,:,7))
  call heltable([4,4,16], n3(:,51), t3x16(:,:,8))
  call heltable([4,4,16], n3(:,52), t3x16(:,:,9))
  call heltable([4,4,16], n3(:,53), t3x16(:,:,10))
  call heltable([4,4,16], n3(:,54), t3x16(:,:,11))
  call heltable([4,4,16], n3(:,55), t3x16(:,:,12))
  call heltable([4,4,16], n3(:,56), t3x16(:,:,13))
  call heltable([2,8,16], n3(:,57), t3x16(:,:,14))
  call heltable([2,8,16], n3(:,58), t3x16(:,:,15))
  call heltable([2,8,16], n3(:,59), t3x16(:,:,16))
  call heltable([2,8,16], n3(:,60), t3x16(:,:,17))
  call heltable([4,4,16], n3(:,61), t3x16(:,:,18))
  call heltable([4,4,16], n3(:,62), t3x16(:,:,19))
  call heltable([2,8,16], n3(:,63), t3x16(:,:,20))
  call heltable([2,8,16], n3(:,64), t3x16(:,:,21))
  call heltable([2,8,16], n3(:,65), t3x16(:,:,22))
  call heltable([2,8,16], n3(:,66), t3x16(:,:,23))
  call heltable([4,4,16], n3(:,67), t3x16(:,:,24))
  call heltable([4,4,16], n3(:,68), t3x16(:,:,25))
  call heltable([8,2,16], n3(:,69), t3x16(:,:,26))
  call heltable([8,2,16], n3(:,70), t3x16(:,:,27))
  call heltable([4,4,16], n3(:,71), t3x16(:,:,28))
  call heltable([4,4,16], n3(:,72), t3x16(:,:,29))
  call heltable([8,2,16], n3(:,73), t3x16(:,:,30))
  call heltable([8,2,16], n3(:,74), t3x16(:,:,31))
  call heltable([4,4,16], n3(:,75), t3x16(:,:,32))
  call heltable([4,4,16], n3(:,76), t3x16(:,:,33))
  call heltable([8,2,16], n3(:,77), t3x16(:,:,34))
  call heltable([8,2,16], n3(:,78), t3x16(:,:,35))
  call heltable([8,2,16], n3(:,79), t3x16(:,:,36))
  call heltable([8,2,16], n3(:,80), t3x16(:,:,37))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_pphhjj_ddxbbxhh_1
