
! **********************************************************************
module ol_heltables_ppaajj_uuxaagg_1
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
    H6(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(26), n3(3,67)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x16(2,16,2), t3x4(2,4,9), t3x8(2,8,26), t3x64(2,64,30)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(64,6)
  integer,           save :: exthel(64,6)
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
  call heltable([2,2,4], n3(:,3), t3x4(:,:,3))
  n2(1) = 4
  n2(2) = 4
  call heltable([4,4,16], n3(:,4), t3x16(:,:,1))
  call heltable([2,4,8], n3(:,5), t3x8(:,:,1))
  call heltable([4,2,8], n3(:,6), t3x8(:,:,2))
  n2(3) = 8
  call heltable([4,2,8], n3(:,7), t3x8(:,:,3))
  call heltable([2,4,8], n3(:,8), t3x8(:,:,4))
  n2(4) = 8
  call heltable([2,2,4], n3(:,9), t3x4(:,:,4))
  n2(5) = 4
  call heltable([2,4,8], n3(:,10), t3x8(:,:,5))
  call heltable([4,2,8], n3(:,11), t3x8(:,:,6))
  n2(6) = 8
  call heltable([4,2,8], n3(:,12), t3x8(:,:,7))
  n2(7) = 8
  call heltable([2,2,4], n3(:,13), t3x4(:,:,5))
  n2(8) = 4
  call heltable([4,2,8], n3(:,14), t3x8(:,:,8))
  call heltable([4,2,8], n3(:,15), t3x8(:,:,9))
  n2(9) = 8
  call heltable([2,4,8], n3(:,16), t3x8(:,:,10))
  n2(10) = 8
  call heltable([2,2,4], n3(:,17), t3x4(:,:,6))
  call heltable([2,2,4], n3(:,18), t3x4(:,:,7))
  n2(11) = 4
  n2(12) = 4
  call heltable([4,4,16], n3(:,19), t3x16(:,:,2))
  call heltable([2,4,8], n3(:,20), t3x8(:,:,11))
  call heltable([4,2,8], n3(:,21), t3x8(:,:,12))
  n2(13) = 8
  call heltable([4,2,8], n3(:,22), t3x8(:,:,13))
  call heltable([2,4,8], n3(:,23), t3x8(:,:,14))
  n2(14) = 8
  call heltable([2,2,4], n3(:,24), t3x4(:,:,8))
  n2(15) = 4
  call heltable([2,4,8], n3(:,25), t3x8(:,:,15))
  n2(16) = 8
  call heltable([4,2,8], n3(:,26), t3x8(:,:,16))
  call heltable([2,4,8], n3(:,27), t3x8(:,:,17))
  n2(17) = 8
  call heltable([2,2,4], n3(:,28), t3x4(:,:,9))
  n2(18) = 4
  call heltable([2,4,8], n3(:,29), t3x8(:,:,18))
  n2(19) = 8
  call heltable([2,4,8], n3(:,30), t3x8(:,:,19))
  call heltable([4,2,8], n3(:,31), t3x8(:,:,20))
  n2(20) = 8
  call heltable([2,4,8], n3(:,32), t3x8(:,:,21))
  n2(21) = 8
  call heltable([4,2,8], n3(:,33), t3x8(:,:,22))
  n2(22) = 8
  call heltable([4,2,8], n3(:,34), t3x8(:,:,23))
  n2(23) = 8
  call heltable([2,4,8], n3(:,35), t3x8(:,:,24))
  n2(24) = 8
  call heltable([4,2,8], n3(:,36), t3x8(:,:,25))
  n2(25) = 8
  call heltable([2,4,8], n3(:,37), t3x8(:,:,26))
  n2(26) = 8
  call heltable([4,16,64], n3(:,38), t3x64(:,:,1))
  call heltable([8,8,64], n3(:,39), t3x64(:,:,2))
  call heltable([8,8,64], n3(:,40), t3x64(:,:,3))
  call heltable([8,8,64], n3(:,41), t3x64(:,:,4))
  call heltable([8,8,64], n3(:,42), t3x64(:,:,5))
  call heltable([8,8,64], n3(:,43), t3x64(:,:,6))
  call heltable([8,8,64], n3(:,44), t3x64(:,:,7))
  call heltable([8,8,64], n3(:,45), t3x64(:,:,8))
  call heltable([4,16,64], n3(:,46), t3x64(:,:,9))
  call heltable([8,8,64], n3(:,47), t3x64(:,:,10))
  call heltable([8,8,64], n3(:,48), t3x64(:,:,11))
  call heltable([8,8,64], n3(:,49), t3x64(:,:,12))
  call heltable([8,8,64], n3(:,50), t3x64(:,:,13))
  call heltable([8,8,64], n3(:,51), t3x64(:,:,14))
  call heltable([8,8,64], n3(:,52), t3x64(:,:,15))
  call heltable([8,8,64], n3(:,53), t3x64(:,:,16))
  call heltable([8,8,64], n3(:,54), t3x64(:,:,17))
  call heltable([8,8,64], n3(:,55), t3x64(:,:,18))
  call heltable([8,8,64], n3(:,56), t3x64(:,:,19))
  call heltable([8,8,64], n3(:,57), t3x64(:,:,20))
  call heltable([8,8,64], n3(:,58), t3x64(:,:,21))
  call heltable([8,8,64], n3(:,59), t3x64(:,:,22))
  call heltable([8,8,64], n3(:,60), t3x64(:,:,23))
  call heltable([8,8,64], n3(:,61), t3x64(:,:,24))
  call heltable([8,8,64], n3(:,62), t3x64(:,:,25))
  call heltable([8,8,64], n3(:,63), t3x64(:,:,26))
  call heltable([8,8,64], n3(:,64), t3x64(:,:,27))
  call heltable([8,8,64], n3(:,65), t3x64(:,:,28))
  call heltable([8,8,64], n3(:,66), t3x64(:,:,29))
  call heltable([8,8,64], n3(:,67), t3x64(:,:,30))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppaajj_uuxaagg_1
