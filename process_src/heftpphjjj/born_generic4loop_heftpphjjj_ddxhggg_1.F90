
! **********************************************************************
module ol_heltables_heftpphjjj_ddxhggg_1
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
    H3(1) = [0], &
    H4(2) = [-1,1], &
    H5(2) = [-1,1], &
    H6(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(15), n3(3,152), n4(4,18), n5(5,3)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x2(2,2,3), t3x16(2,16,6), t3x4(2,4,26), t3x8(2,8,33), t3x32(2,32,84), t4x4(3,4,3), t4x8(3,8,6), &
    t4x16(3,16,9), t5x8(4,8,3)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(32,6)
  integer,           save :: exthel(32,6)
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
  call heltable([1,2,2,2,8], n5(:,1), t5x8(:,:,1))
  call heltable([1,2,2,2,8], n5(:,2), t5x8(:,:,2))
  call heltable([1,2,2,2,8], n5(:,3), t5x8(:,:,3))
  call heltable([1,2,2], n3(:,2), t3x2(:,:,1))
  call heltable([4,2,2,16], n4(:,1), t4x16(:,:,1))
  call heltable([2,2,4,16], n4(:,2), t4x16(:,:,2))
  call heltable([2,4,2,16], n4(:,3), t4x16(:,:,3))
  call heltable([1,2,2], n3(:,3), t3x2(:,:,2))
  call heltable([4,2,2,16], n4(:,4), t4x16(:,:,4))
  call heltable([2,2,4,16], n4(:,5), t4x16(:,:,5))
  call heltable([2,4,2,16], n4(:,6), t4x16(:,:,6))
  call heltable([1,2,2], n3(:,4), t3x2(:,:,3))
  call heltable([4,2,2,16], n4(:,7), t4x16(:,:,7))
  call heltable([2,2,4,16], n4(:,8), t4x16(:,:,8))
  call heltable([2,4,2,16], n4(:,9), t4x16(:,:,9))
  call heltable([2,2,2,8], n4(:,10), t4x8(:,:,1))
  call heltable([1,4,4], n3(:,5), t3x4(:,:,2))
  call heltable([2,2,2,8], n4(:,11), t4x8(:,:,2))
  call heltable([2,2,2,8], n4(:,12), t4x8(:,:,3))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,3))
  call heltable([1,4,2,8], n4(:,13), t4x8(:,:,4))
  call heltable([2,2,4], n3(:,7), t3x4(:,:,4))
  call heltable([1,4,2,8], n4(:,14), t4x8(:,:,5))
  call heltable([1,2,2,4], n4(:,15), t4x4(:,:,1))
  call heltable([4,2,8], n3(:,8), t3x8(:,:,1))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,5))
  call heltable([1,4,2,8], n4(:,16), t4x8(:,:,6))
  call heltable([1,2,2,4], n4(:,17), t4x4(:,:,2))
  call heltable([4,2,8], n3(:,10), t3x8(:,:,2))
  call heltable([1,2,2,4], n4(:,18), t4x4(:,:,3))
  call heltable([4,2,8], n3(:,11), t3x8(:,:,3))
  call heltable([2,2,4], n3(:,12), t3x4(:,:,6))
  n2(1) = 4
  call heltable([4,2,8], n3(:,13), t3x8(:,:,4))
  call heltable([2,2,4], n3(:,14), t3x4(:,:,7))
  n2(2) = 4
  call heltable([2,4,8], n3(:,15), t3x8(:,:,5))
  call heltable([2,2,4], n3(:,16), t3x4(:,:,8))
  n2(3) = 4
  call heltable([4,2,8], n3(:,17), t3x8(:,:,6))
  call heltable([2,2,4], n3(:,18), t3x4(:,:,9))
  n2(4) = 4
  call heltable([2,4,8], n3(:,19), t3x8(:,:,7))
  call heltable([2,2,4], n3(:,20), t3x4(:,:,10))
  n2(5) = 4
  call heltable([4,2,8], n3(:,21), t3x8(:,:,8))
  call heltable([2,2,4], n3(:,22), t3x4(:,:,11))
  n2(6) = 4
  call heltable([2,4,8], n3(:,23), t3x8(:,:,9))
  call heltable([4,2,8], n3(:,24), t3x8(:,:,10))
  call heltable([2,2,4], n3(:,25), t3x4(:,:,12))
  call heltable([2,2,4], n3(:,26), t3x4(:,:,13))
  call heltable([4,2,8], n3(:,27), t3x8(:,:,11))
  call heltable([2,2,4], n3(:,28), t3x4(:,:,14))
  call heltable([2,2,4], n3(:,29), t3x4(:,:,15))
  call heltable([4,2,8], n3(:,30), t3x8(:,:,12))
  call heltable([4,2,8], n3(:,31), t3x8(:,:,13))
  call heltable([1,4,4], n3(:,32), t3x4(:,:,16))
  call heltable([2,2,4], n3(:,33), t3x4(:,:,17))
  call heltable([2,2,4], n3(:,34), t3x4(:,:,18))
  call heltable([2,4,8], n3(:,35), t3x8(:,:,14))
  call heltable([1,4,4], n3(:,36), t3x4(:,:,19))
  call heltable([2,4,8], n3(:,37), t3x8(:,:,15))
  call heltable([1,4,4], n3(:,38), t3x4(:,:,20))
  call heltable([4,4,16], n3(:,39), t3x16(:,:,1))
  call heltable([4,4,16], n3(:,40), t3x16(:,:,2))
  call heltable([2,2,4], n3(:,41), t3x4(:,:,21))
  call heltable([2,4,8], n3(:,42), t3x8(:,:,16))
  n2(7) = 4
  call heltable([2,2,4], n3(:,43), t3x4(:,:,22))
  call heltable([2,4,8], n3(:,44), t3x8(:,:,17))
  n2(8) = 4
  call heltable([4,4,16], n3(:,45), t3x16(:,:,3))
  call heltable([4,4,16], n3(:,46), t3x16(:,:,4))
  call heltable([2,2,4], n3(:,47), t3x4(:,:,23))
  call heltable([4,2,8], n3(:,48), t3x8(:,:,18))
  n2(9) = 4
  call heltable([2,2,4], n3(:,49), t3x4(:,:,24))
  call heltable([4,2,8], n3(:,50), t3x8(:,:,19))
  n2(10) = 4
  call heltable([4,4,16], n3(:,51), t3x16(:,:,5))
  call heltable([2,2,4], n3(:,52), t3x4(:,:,25))
  call heltable([2,4,8], n3(:,53), t3x8(:,:,20))
  n2(11) = 4
  call heltable([4,4,16], n3(:,54), t3x16(:,:,6))
  call heltable([2,2,4], n3(:,55), t3x4(:,:,26))
  call heltable([4,2,8], n3(:,56), t3x8(:,:,21))
  n2(12) = 4
  call heltable([2,4,8], n3(:,57), t3x8(:,:,22))
  call heltable([4,2,8], n3(:,58), t3x8(:,:,23))
  call heltable([2,4,8], n3(:,59), t3x8(:,:,24))
  call heltable([4,2,8], n3(:,60), t3x8(:,:,25))
  n2(13) = 8
  call heltable([2,4,8], n3(:,61), t3x8(:,:,26))
  call heltable([4,2,8], n3(:,62), t3x8(:,:,27))
  call heltable([2,4,8], n3(:,63), t3x8(:,:,28))
  call heltable([4,2,8], n3(:,64), t3x8(:,:,29))
  call heltable([2,4,8], n3(:,65), t3x8(:,:,30))
  call heltable([4,2,8], n3(:,66), t3x8(:,:,31))
  n2(14) = 8
  call heltable([2,4,8], n3(:,67), t3x8(:,:,32))
  call heltable([4,2,8], n3(:,68), t3x8(:,:,33))
  n2(15) = 8
  call heltable([4,8,32], n3(:,69), t3x32(:,:,1))
  call heltable([4,8,32], n3(:,70), t3x32(:,:,2))
  call heltable([4,8,32], n3(:,71), t3x32(:,:,3))
  call heltable([2,16,32], n3(:,72), t3x32(:,:,4))
  call heltable([2,16,32], n3(:,73), t3x32(:,:,5))
  call heltable([2,16,32], n3(:,74), t3x32(:,:,6))
  call heltable([2,16,32], n3(:,75), t3x32(:,:,7))
  call heltable([2,16,32], n3(:,76), t3x32(:,:,8))
  call heltable([2,16,32], n3(:,77), t3x32(:,:,9))
  call heltable([2,16,32], n3(:,78), t3x32(:,:,10))
  call heltable([2,16,32], n3(:,79), t3x32(:,:,11))
  call heltable([2,16,32], n3(:,80), t3x32(:,:,12))
  call heltable([8,4,32], n3(:,81), t3x32(:,:,13))
  call heltable([4,8,32], n3(:,82), t3x32(:,:,14))
  call heltable([4,8,32], n3(:,83), t3x32(:,:,15))
  call heltable([4,8,32], n3(:,84), t3x32(:,:,16))
  call heltable([4,8,32], n3(:,85), t3x32(:,:,17))
  call heltable([4,8,32], n3(:,86), t3x32(:,:,18))
  call heltable([4,8,32], n3(:,87), t3x32(:,:,19))
  call heltable([4,8,32], n3(:,88), t3x32(:,:,20))
  call heltable([4,8,32], n3(:,89), t3x32(:,:,21))
  call heltable([4,8,32], n3(:,90), t3x32(:,:,22))
  call heltable([4,8,32], n3(:,91), t3x32(:,:,23))
  call heltable([4,8,32], n3(:,92), t3x32(:,:,24))
  call heltable([4,8,32], n3(:,93), t3x32(:,:,25))
  call heltable([4,8,32], n3(:,94), t3x32(:,:,26))
  call heltable([4,8,32], n3(:,95), t3x32(:,:,27))
  call heltable([4,8,32], n3(:,96), t3x32(:,:,28))
  call heltable([8,4,32], n3(:,97), t3x32(:,:,29))
  call heltable([8,4,32], n3(:,98), t3x32(:,:,30))
  call heltable([4,8,32], n3(:,99), t3x32(:,:,31))
  call heltable([8,4,32], n3(:,100), t3x32(:,:,32))
  call heltable([8,4,32], n3(:,101), t3x32(:,:,33))
  call heltable([4,8,32], n3(:,102), t3x32(:,:,34))
  call heltable([4,8,32], n3(:,103), t3x32(:,:,35))
  call heltable([8,4,32], n3(:,104), t3x32(:,:,36))
  call heltable([8,4,32], n3(:,105), t3x32(:,:,37))
  call heltable([8,4,32], n3(:,106), t3x32(:,:,38))
  call heltable([4,8,32], n3(:,107), t3x32(:,:,39))
  call heltable([8,4,32], n3(:,108), t3x32(:,:,40))
  call heltable([4,8,32], n3(:,109), t3x32(:,:,41))
  call heltable([8,4,32], n3(:,110), t3x32(:,:,42))
  call heltable([2,16,32], n3(:,111), t3x32(:,:,43))
  call heltable([2,16,32], n3(:,112), t3x32(:,:,44))
  call heltable([8,4,32], n3(:,113), t3x32(:,:,45))
  call heltable([8,4,32], n3(:,114), t3x32(:,:,46))
  call heltable([8,4,32], n3(:,115), t3x32(:,:,47))
  call heltable([8,4,32], n3(:,116), t3x32(:,:,48))
  call heltable([8,4,32], n3(:,117), t3x32(:,:,49))
  call heltable([2,16,32], n3(:,118), t3x32(:,:,50))
  call heltable([2,16,32], n3(:,119), t3x32(:,:,51))
  call heltable([8,4,32], n3(:,120), t3x32(:,:,52))
  call heltable([8,4,32], n3(:,121), t3x32(:,:,53))
  call heltable([8,4,32], n3(:,122), t3x32(:,:,54))
  call heltable([8,4,32], n3(:,123), t3x32(:,:,55))
  call heltable([8,4,32], n3(:,124), t3x32(:,:,56))
  call heltable([2,16,32], n3(:,125), t3x32(:,:,57))
  call heltable([8,4,32], n3(:,126), t3x32(:,:,58))
  call heltable([8,4,32], n3(:,127), t3x32(:,:,59))
  call heltable([2,16,32], n3(:,128), t3x32(:,:,60))
  call heltable([8,4,32], n3(:,129), t3x32(:,:,61))
  call heltable([8,4,32], n3(:,130), t3x32(:,:,62))
  call heltable([8,4,32], n3(:,131), t3x32(:,:,63))
  call heltable([4,8,32], n3(:,132), t3x32(:,:,64))
  call heltable([8,4,32], n3(:,133), t3x32(:,:,65))
  call heltable([4,8,32], n3(:,134), t3x32(:,:,66))
  call heltable([4,8,32], n3(:,135), t3x32(:,:,67))
  call heltable([4,8,32], n3(:,136), t3x32(:,:,68))
  call heltable([8,4,32], n3(:,137), t3x32(:,:,69))
  call heltable([4,8,32], n3(:,138), t3x32(:,:,70))
  call heltable([8,4,32], n3(:,139), t3x32(:,:,71))
  call heltable([8,4,32], n3(:,140), t3x32(:,:,72))
  call heltable([4,8,32], n3(:,141), t3x32(:,:,73))
  call heltable([8,4,32], n3(:,142), t3x32(:,:,74))
  call heltable([8,4,32], n3(:,143), t3x32(:,:,75))
  call heltable([4,8,32], n3(:,144), t3x32(:,:,76))
  call heltable([8,4,32], n3(:,145), t3x32(:,:,77))
  call heltable([4,8,32], n3(:,146), t3x32(:,:,78))
  call heltable([4,8,32], n3(:,147), t3x32(:,:,79))
  call heltable([4,8,32], n3(:,148), t3x32(:,:,80))
  call heltable([8,4,32], n3(:,149), t3x32(:,:,81))
  call heltable([8,4,32], n3(:,150), t3x32(:,:,82))
  call heltable([4,8,32], n3(:,151), t3x32(:,:,83))
  call heltable([4,8,32], n3(:,152), t3x32(:,:,84))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_heftpphjjj_ddxhggg_1
