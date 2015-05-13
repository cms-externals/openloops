
! **********************************************************************
module ol_heltables_pphzjj_bbbxbxhz_1
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
    H6(3) = [-1,0,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(19), n3(3,105)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x3(2,3,1), t3x2(2,2,4), t3x24(2,24,4), t3x12(2,12,8), t3x4(2,4,12), t3x8(2,8,12), t3x6(2,6,16), &
    t3x48(2,48,48)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(48,6)
  integer,           save :: exthel(48,6)
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
  call heltable([2,1,2], n3(:,2), t3x2(:,:,1))
  call heltable([2,3,6], n3(:,3), t3x6(:,:,1))
  n2(1) = 2
  n2(2) = 6
  call heltable([4,2,8], n3(:,4), t3x8(:,:,1))
  call heltable([2,4,8], n3(:,5), t3x8(:,:,2))
  call heltable([3,2,6], n3(:,6), t3x6(:,:,2))
  n2(3) = 8
  call heltable([3,2,6], n3(:,7), t3x6(:,:,3))
  call heltable([1,2,2], n3(:,8), t3x2(:,:,2))
  n2(4) = 6
  n2(5) = 2
  call heltable([4,6,24], n3(:,9), t3x24(:,:,1))
  call heltable([4,2,8], n3(:,10), t3x8(:,:,3))
  call heltable([2,3,6], n3(:,11), t3x6(:,:,4))
  n2(6) = 8
  call heltable([6,1,6], n3(:,12), t3x6(:,:,5))
  call heltable([1,6,6], n3(:,13), t3x6(:,:,6))
  call heltable([1,3,3], n3(:,14), t3x3(:,:,1))
  n2(7) = 3
  call heltable([2,3,6], n3(:,15), t3x6(:,:,7))
  call heltable([3,2,6], n3(:,16), t3x6(:,:,8))
  n2(8) = 6
  call heltable([2,1,2], n3(:,17), t3x2(:,:,3))
  call heltable([2,2,4], n3(:,18), t3x4(:,:,2))
  n2(9) = 2
  call heltable([4,2,8], n3(:,19), t3x8(:,:,4))
  call heltable([2,4,8], n3(:,20), t3x8(:,:,5))
  call heltable([3,2,6], n3(:,21), t3x6(:,:,9))
  n2(10) = 8
  call heltable([3,2,6], n3(:,22), t3x6(:,:,10))
  n2(11) = 6
  call heltable([4,6,24], n3(:,23), t3x24(:,:,2))
  call heltable([4,2,8], n3(:,24), t3x8(:,:,6))
  n2(12) = 8
  call heltable([6,1,6], n3(:,25), t3x6(:,:,11))
  call heltable([3,2,6], n3(:,26), t3x6(:,:,12))
  n2(13) = 6
  call heltable([2,2,4], n3(:,27), t3x4(:,:,3))
  call heltable([2,3,6], n3(:,28), t3x6(:,:,13))
  n2(14) = 6
  call heltable([4,2,8], n3(:,29), t3x8(:,:,7))
  call heltable([2,4,8], n3(:,30), t3x8(:,:,8))
  n2(15) = 8
  call heltable([1,2,2], n3(:,31), t3x2(:,:,4))
  n2(16) = 2
  call heltable([4,6,24], n3(:,32), t3x24(:,:,3))
  call heltable([4,2,8], n3(:,33), t3x8(:,:,9))
  call heltable([2,3,6], n3(:,34), t3x6(:,:,14))
  n2(17) = 8
  call heltable([1,6,6], n3(:,35), t3x6(:,:,15))
  call heltable([2,3,6], n3(:,36), t3x6(:,:,16))
  call heltable([2,2,4], n3(:,37), t3x4(:,:,4))
  call heltable([4,2,8], n3(:,38), t3x8(:,:,10))
  call heltable([2,4,8], n3(:,39), t3x8(:,:,11))
  n2(18) = 8
  call heltable([4,6,24], n3(:,40), t3x24(:,:,4))
  call heltable([4,2,8], n3(:,41), t3x8(:,:,12))
  n2(19) = 8
  call heltable([2,2,4], n3(:,42), t3x4(:,:,5))
  call heltable([6,2,12], n3(:,43), t3x12(:,:,1))
  call heltable([6,2,12], n3(:,44), t3x12(:,:,2))
  call heltable([2,2,4], n3(:,45), t3x4(:,:,6))
  call heltable([2,6,12], n3(:,46), t3x12(:,:,3))
  call heltable([2,6,12], n3(:,47), t3x12(:,:,4))
  call heltable([6,2,12], n3(:,48), t3x12(:,:,5))
  call heltable([2,2,4], n3(:,49), t3x4(:,:,7))
  call heltable([2,2,4], n3(:,50), t3x4(:,:,8))
  call heltable([6,2,12], n3(:,51), t3x12(:,:,6))
  call heltable([2,6,12], n3(:,52), t3x12(:,:,7))
  call heltable([2,6,12], n3(:,53), t3x12(:,:,8))
  call heltable([2,2,4], n3(:,54), t3x4(:,:,9))
  call heltable([2,2,4], n3(:,55), t3x4(:,:,10))
  call heltable([2,2,4], n3(:,56), t3x4(:,:,11))
  call heltable([2,2,4], n3(:,57), t3x4(:,:,12))
  call heltable([6,8,48], n3(:,58), t3x48(:,:,1))
  call heltable([6,8,48], n3(:,59), t3x48(:,:,2))
  call heltable([2,24,48], n3(:,60), t3x48(:,:,3))
  call heltable([6,8,48], n3(:,61), t3x48(:,:,4))
  call heltable([8,6,48], n3(:,62), t3x48(:,:,5))
  call heltable([8,6,48], n3(:,63), t3x48(:,:,6))
  call heltable([8,6,48], n3(:,64), t3x48(:,:,7))
  call heltable([8,6,48], n3(:,65), t3x48(:,:,8))
  call heltable([6,8,48], n3(:,66), t3x48(:,:,9))
  call heltable([6,8,48], n3(:,67), t3x48(:,:,10))
  call heltable([2,24,48], n3(:,68), t3x48(:,:,11))
  call heltable([6,8,48], n3(:,69), t3x48(:,:,12))
  call heltable([8,6,48], n3(:,70), t3x48(:,:,13))
  call heltable([6,8,48], n3(:,71), t3x48(:,:,14))
  call heltable([6,8,48], n3(:,72), t3x48(:,:,15))
  call heltable([8,6,48], n3(:,73), t3x48(:,:,16))
  call heltable([6,8,48], n3(:,74), t3x48(:,:,17))
  call heltable([6,8,48], n3(:,75), t3x48(:,:,18))
  call heltable([2,24,48], n3(:,76), t3x48(:,:,19))
  call heltable([6,8,48], n3(:,77), t3x48(:,:,20))
  call heltable([6,8,48], n3(:,78), t3x48(:,:,21))
  call heltable([8,6,48], n3(:,79), t3x48(:,:,22))
  call heltable([8,6,48], n3(:,80), t3x48(:,:,23))
  call heltable([6,8,48], n3(:,81), t3x48(:,:,24))
  call heltable([6,8,48], n3(:,82), t3x48(:,:,25))
  call heltable([6,8,48], n3(:,83), t3x48(:,:,26))
  call heltable([2,24,48], n3(:,84), t3x48(:,:,27))
  call heltable([6,8,48], n3(:,85), t3x48(:,:,28))
  call heltable([6,8,48], n3(:,86), t3x48(:,:,29))
  call heltable([6,8,48], n3(:,87), t3x48(:,:,30))
  call heltable([6,8,48], n3(:,88), t3x48(:,:,31))
  call heltable([6,8,48], n3(:,89), t3x48(:,:,32))
  call heltable([4,12,48], n3(:,90), t3x48(:,:,33))
  call heltable([12,4,48], n3(:,91), t3x48(:,:,34))
  call heltable([4,12,48], n3(:,92), t3x48(:,:,35))
  call heltable([4,12,48], n3(:,93), t3x48(:,:,36))
  call heltable([12,4,48], n3(:,94), t3x48(:,:,37))
  call heltable([4,12,48], n3(:,95), t3x48(:,:,38))
  call heltable([4,12,48], n3(:,96), t3x48(:,:,39))
  call heltable([4,12,48], n3(:,97), t3x48(:,:,40))
  call heltable([12,4,48], n3(:,98), t3x48(:,:,41))
  call heltable([12,4,48], n3(:,99), t3x48(:,:,42))
  call heltable([12,4,48], n3(:,100), t3x48(:,:,43))
  call heltable([12,4,48], n3(:,101), t3x48(:,:,44))
  call heltable([12,4,48], n3(:,102), t3x48(:,:,45))
  call heltable([12,4,48], n3(:,103), t3x48(:,:,46))
  call heltable([12,4,48], n3(:,104), t3x48(:,:,47))
  call heltable([12,4,48], n3(:,105), t3x48(:,:,48))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_pphzjj_bbbxbxhz_1
