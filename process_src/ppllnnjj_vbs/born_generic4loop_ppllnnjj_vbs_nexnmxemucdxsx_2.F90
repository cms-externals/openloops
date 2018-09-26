
! **********************************************************************
module ol_heltables_ppllnnjj_vbs_nexnmxemucdxsx_2
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
    H7(2) = [-1,1], &
    H8(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(38), n3(3,157), n4(4,1)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,4), t3x32(2,32,8), t3x8(2,8,16), t3x16(2,16,36), t3x256(2,256,93), t4x64(3,64,1)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(256,8)
  integer,           save :: exthel(256,8)
  integer,           save :: firstpol(8)

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
  call heltable([2,2,4], n3(:,4), t3x4(:,:,4))
  n2(1) = 4
  n2(2) = 4
  n2(3) = 4
  n2(4) = 4
  call heltable([4,4,4,64], n4(:,1), t4x64(:,:,1))
  call heltable([4,4,16], n3(:,5), t3x16(:,:,1))
  call heltable([4,4,16], n3(:,6), t3x16(:,:,2))
  call heltable([4,4,16], n3(:,7), t3x16(:,:,3))
  call heltable([4,4,16], n3(:,8), t3x16(:,:,4))
  n2(5) = 16
  call heltable([4,4,16], n3(:,9), t3x16(:,:,5))
  call heltable([4,4,16], n3(:,10), t3x16(:,:,6))
  call heltable([4,4,16], n3(:,11), t3x16(:,:,7))
  call heltable([4,4,16], n3(:,12), t3x16(:,:,8))
  n2(6) = 16
  call heltable([4,2,8], n3(:,13), t3x8(:,:,1))
  call heltable([2,4,8], n3(:,14), t3x8(:,:,2))
  n2(7) = 8
  n2(8) = 8
  call heltable([4,8,32], n3(:,15), t3x32(:,:,1))
  call heltable([8,2,16], n3(:,16), t3x16(:,:,9))
  n2(9) = 16
  call heltable([8,2,16], n3(:,17), t3x16(:,:,10))
  call heltable([4,2,8], n3(:,18), t3x8(:,:,3))
  call heltable([2,4,8], n3(:,19), t3x8(:,:,4))
  n2(10) = 8
  n2(11) = 8
  call heltable([4,8,32], n3(:,20), t3x32(:,:,2))
  call heltable([2,8,16], n3(:,21), t3x16(:,:,11))
  call heltable([2,8,16], n3(:,22), t3x16(:,:,12))
  call heltable([8,2,16], n3(:,23), t3x16(:,:,13))
  call heltable([8,2,16], n3(:,24), t3x16(:,:,14))
  call heltable([2,8,16], n3(:,25), t3x16(:,:,15))
  call heltable([2,8,16], n3(:,26), t3x16(:,:,16))
  call heltable([4,2,8], n3(:,27), t3x8(:,:,5))
  call heltable([2,4,8], n3(:,28), t3x8(:,:,6))
  n2(12) = 8
  n2(13) = 8
  call heltable([4,8,32], n3(:,29), t3x32(:,:,3))
  call heltable([8,2,16], n3(:,30), t3x16(:,:,17))
  n2(14) = 16
  call heltable([8,2,16], n3(:,31), t3x16(:,:,18))
  call heltable([4,2,8], n3(:,32), t3x8(:,:,7))
  call heltable([2,4,8], n3(:,33), t3x8(:,:,8))
  n2(15) = 8
  n2(16) = 8
  call heltable([4,8,32], n3(:,34), t3x32(:,:,4))
  call heltable([2,8,16], n3(:,35), t3x16(:,:,19))
  call heltable([2,8,16], n3(:,36), t3x16(:,:,20))
  call heltable([8,2,16], n3(:,37), t3x16(:,:,21))
  call heltable([8,2,16], n3(:,38), t3x16(:,:,22))
  call heltable([2,8,16], n3(:,39), t3x16(:,:,23))
  call heltable([2,8,16], n3(:,40), t3x16(:,:,24))
  n2(17) = 16
  n2(18) = 16
  n2(19) = 16
  n2(20) = 16
  n2(21) = 16
  n2(22) = 16
  call heltable([2,4,8], n3(:,41), t3x8(:,:,9))
  n2(23) = 8
  call heltable([2,8,16], n3(:,42), t3x16(:,:,25))
  call heltable([2,8,16], n3(:,43), t3x16(:,:,26))
  call heltable([2,4,8], n3(:,44), t3x8(:,:,10))
  n2(24) = 8
  call heltable([2,8,16], n3(:,45), t3x16(:,:,27))
  call heltable([2,8,16], n3(:,46), t3x16(:,:,28))
  call heltable([4,2,8], n3(:,47), t3x8(:,:,11))
  n2(25) = 8
  call heltable([8,2,16], n3(:,48), t3x16(:,:,29))
  call heltable([4,2,8], n3(:,49), t3x8(:,:,12))
  n2(26) = 8
  call heltable([8,2,16], n3(:,50), t3x16(:,:,30))
  call heltable([8,4,32], n3(:,51), t3x32(:,:,5))
  call heltable([8,4,32], n3(:,52), t3x32(:,:,6))
  n2(27) = 16
  n2(28) = 16
  n2(29) = 16
  n2(30) = 16
  call heltable([2,4,8], n3(:,53), t3x8(:,:,13))
  n2(31) = 8
  call heltable([2,8,16], n3(:,54), t3x16(:,:,31))
  call heltable([2,8,16], n3(:,55), t3x16(:,:,32))
  call heltable([2,4,8], n3(:,56), t3x8(:,:,14))
  n2(32) = 8
  call heltable([2,8,16], n3(:,57), t3x16(:,:,33))
  call heltable([2,8,16], n3(:,58), t3x16(:,:,34))
  call heltable([4,2,8], n3(:,59), t3x8(:,:,15))
  n2(33) = 8
  call heltable([8,2,16], n3(:,60), t3x16(:,:,35))
  call heltable([4,2,8], n3(:,61), t3x8(:,:,16))
  n2(34) = 8
  call heltable([8,2,16], n3(:,62), t3x16(:,:,36))
  call heltable([8,4,32], n3(:,63), t3x32(:,:,7))
  call heltable([8,4,32], n3(:,64), t3x32(:,:,8))
  n2(35) = 16
  n2(36) = 16
  n2(37) = 16
  n2(38) = 16
  call heltable([4,64,256], n3(:,65), t3x256(:,:,1))
  call heltable([16,16,256], n3(:,66), t3x256(:,:,2))
  call heltable([16,16,256], n3(:,67), t3x256(:,:,3))
  call heltable([16,16,256], n3(:,68), t3x256(:,:,4))
  call heltable([16,16,256], n3(:,69), t3x256(:,:,5))
  call heltable([16,16,256], n3(:,70), t3x256(:,:,6))
  call heltable([16,16,256], n3(:,71), t3x256(:,:,7))
  call heltable([8,32,256], n3(:,72), t3x256(:,:,8))
  call heltable([16,16,256], n3(:,73), t3x256(:,:,9))
  call heltable([16,16,256], n3(:,74), t3x256(:,:,10))
  call heltable([8,32,256], n3(:,75), t3x256(:,:,11))
  call heltable([16,16,256], n3(:,76), t3x256(:,:,12))
  call heltable([16,16,256], n3(:,77), t3x256(:,:,13))
  call heltable([16,16,256], n3(:,78), t3x256(:,:,14))
  call heltable([16,16,256], n3(:,79), t3x256(:,:,15))
  call heltable([16,16,256], n3(:,80), t3x256(:,:,16))
  call heltable([16,16,256], n3(:,81), t3x256(:,:,17))
  call heltable([8,32,256], n3(:,82), t3x256(:,:,18))
  call heltable([16,16,256], n3(:,83), t3x256(:,:,19))
  call heltable([16,16,256], n3(:,84), t3x256(:,:,20))
  call heltable([8,32,256], n3(:,85), t3x256(:,:,21))
  call heltable([16,16,256], n3(:,86), t3x256(:,:,22))
  call heltable([16,16,256], n3(:,87), t3x256(:,:,23))
  call heltable([16,16,256], n3(:,88), t3x256(:,:,24))
  call heltable([16,16,256], n3(:,89), t3x256(:,:,25))
  call heltable([16,16,256], n3(:,90), t3x256(:,:,26))
  call heltable([16,16,256], n3(:,91), t3x256(:,:,27))
  call heltable([16,16,256], n3(:,92), t3x256(:,:,28))
  call heltable([16,16,256], n3(:,93), t3x256(:,:,29))
  call heltable([16,16,256], n3(:,94), t3x256(:,:,30))
  call heltable([16,16,256], n3(:,95), t3x256(:,:,31))
  call heltable([16,16,256], n3(:,96), t3x256(:,:,32))
  call heltable([16,16,256], n3(:,97), t3x256(:,:,33))
  call heltable([16,16,256], n3(:,98), t3x256(:,:,34))
  call heltable([16,16,256], n3(:,99), t3x256(:,:,35))
  call heltable([16,16,256], n3(:,100), t3x256(:,:,36))
  call heltable([16,16,256], n3(:,101), t3x256(:,:,37))
  call heltable([16,16,256], n3(:,102), t3x256(:,:,38))
  call heltable([16,16,256], n3(:,103), t3x256(:,:,39))
  call heltable([16,16,256], n3(:,104), t3x256(:,:,40))
  call heltable([16,16,256], n3(:,105), t3x256(:,:,41))
  call heltable([16,16,256], n3(:,106), t3x256(:,:,42))
  call heltable([16,16,256], n3(:,107), t3x256(:,:,43))
  call heltable([16,16,256], n3(:,108), t3x256(:,:,44))
  call heltable([16,16,256], n3(:,109), t3x256(:,:,45))
  call heltable([16,16,256], n3(:,110), t3x256(:,:,46))
  call heltable([16,16,256], n3(:,111), t3x256(:,:,47))
  call heltable([16,16,256], n3(:,112), t3x256(:,:,48))
  call heltable([16,16,256], n3(:,113), t3x256(:,:,49))
  call heltable([8,32,256], n3(:,114), t3x256(:,:,50))
  call heltable([8,32,256], n3(:,115), t3x256(:,:,51))
  call heltable([16,16,256], n3(:,116), t3x256(:,:,52))
  call heltable([16,16,256], n3(:,117), t3x256(:,:,53))
  call heltable([16,16,256], n3(:,118), t3x256(:,:,54))
  call heltable([16,16,256], n3(:,119), t3x256(:,:,55))
  call heltable([16,16,256], n3(:,120), t3x256(:,:,56))
  call heltable([16,16,256], n3(:,121), t3x256(:,:,57))
  call heltable([16,16,256], n3(:,122), t3x256(:,:,58))
  call heltable([16,16,256], n3(:,123), t3x256(:,:,59))
  call heltable([16,16,256], n3(:,124), t3x256(:,:,60))
  call heltable([16,16,256], n3(:,125), t3x256(:,:,61))
  call heltable([16,16,256], n3(:,126), t3x256(:,:,62))
  call heltable([16,16,256], n3(:,127), t3x256(:,:,63))
  call heltable([16,16,256], n3(:,128), t3x256(:,:,64))
  call heltable([16,16,256], n3(:,129), t3x256(:,:,65))
  call heltable([16,16,256], n3(:,130), t3x256(:,:,66))
  call heltable([16,16,256], n3(:,131), t3x256(:,:,67))
  call heltable([16,16,256], n3(:,132), t3x256(:,:,68))
  call heltable([16,16,256], n3(:,133), t3x256(:,:,69))
  call heltable([8,32,256], n3(:,134), t3x256(:,:,70))
  call heltable([8,32,256], n3(:,135), t3x256(:,:,71))
  call heltable([16,16,256], n3(:,136), t3x256(:,:,72))
  call heltable([16,16,256], n3(:,137), t3x256(:,:,73))
  call heltable([16,16,256], n3(:,138), t3x256(:,:,74))
  call heltable([16,16,256], n3(:,139), t3x256(:,:,75))
  call heltable([16,16,256], n3(:,140), t3x256(:,:,76))
  call heltable([16,16,256], n3(:,141), t3x256(:,:,77))
  call heltable([16,16,256], n3(:,142), t3x256(:,:,78))
  call heltable([16,16,256], n3(:,143), t3x256(:,:,79))
  call heltable([16,16,256], n3(:,144), t3x256(:,:,80))
  call heltable([16,16,256], n3(:,145), t3x256(:,:,81))
  call heltable([16,16,256], n3(:,146), t3x256(:,:,82))
  call heltable([16,16,256], n3(:,147), t3x256(:,:,83))
  call heltable([16,16,256], n3(:,148), t3x256(:,:,84))
  call heltable([16,16,256], n3(:,149), t3x256(:,:,85))
  call heltable([16,16,256], n3(:,150), t3x256(:,:,86))
  call heltable([16,16,256], n3(:,151), t3x256(:,:,87))
  call heltable([16,16,256], n3(:,152), t3x256(:,:,88))
  call heltable([16,16,256], n3(:,153), t3x256(:,:,89))
  call heltable([16,16,256], n3(:,154), t3x256(:,:,90))
  call heltable([16,16,256], n3(:,155), t3x256(:,:,91))
  call heltable([16,16,256], n3(:,156), t3x256(:,:,92))
  call heltable([16,16,256], n3(:,157), t3x256(:,:,93))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppllnnjj_vbs_nexnmxemucdxsx_2
