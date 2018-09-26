
! **********************************************************************
module ol_heltables_ppllll_nenexeexuux_1
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
  integer(intkind2), save :: n2(17), n3(3,49)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x16(2,16,1), t3x4(2,4,7), t3x64(2,64,19), t3x8(2,8,22)

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
  n2(1) = 4
  call heltable([4,2,8], n3(:,3), t3x8(:,:,1))
  call heltable([2,4,8], n3(:,4), t3x8(:,:,2))
  n2(2) = 8
  call heltable([2,2,4], n3(:,5), t3x4(:,:,3))
  n2(3) = 4
  call heltable([2,4,8], n3(:,6), t3x8(:,:,3))
  call heltable([4,2,8], n3(:,7), t3x8(:,:,4))
  call heltable([2,4,8], n3(:,8), t3x8(:,:,5))
  n2(4) = 8
  call heltable([4,2,8], n3(:,9), t3x8(:,:,6))
  n2(5) = 8
  call heltable([2,2,4], n3(:,10), t3x4(:,:,4))
  call heltable([4,2,8], n3(:,11), t3x8(:,:,7))
  call heltable([2,4,8], n3(:,12), t3x8(:,:,8))
  n2(6) = 8
  call heltable([2,2,4], n3(:,13), t3x4(:,:,5))
  n2(7) = 4
  call heltable([2,4,8], n3(:,14), t3x8(:,:,9))
  call heltable([4,2,8], n3(:,15), t3x8(:,:,10))
  call heltable([2,4,8], n3(:,16), t3x8(:,:,11))
  n2(8) = 8
  call heltable([4,2,8], n3(:,17), t3x8(:,:,12))
  n2(9) = 8
  call heltable([2,2,4], n3(:,18), t3x4(:,:,6))
  call heltable([2,2,4], n3(:,19), t3x4(:,:,7))
  n2(10) = 4
  n2(11) = 4
  call heltable([4,4,16], n3(:,20), t3x16(:,:,1))
  call heltable([4,2,8], n3(:,21), t3x8(:,:,13))
  call heltable([2,4,8], n3(:,22), t3x8(:,:,14))
  n2(12) = 8
  call heltable([4,2,8], n3(:,23), t3x8(:,:,15))
  n2(13) = 8
  call heltable([4,2,8], n3(:,24), t3x8(:,:,16))
  call heltable([2,4,8], n3(:,25), t3x8(:,:,17))
  n2(14) = 8
  call heltable([2,4,8], n3(:,26), t3x8(:,:,18))
  n2(15) = 8
  call heltable([2,4,8], n3(:,27), t3x8(:,:,19))
  call heltable([4,2,8], n3(:,28), t3x8(:,:,20))
  n2(16) = 8
  call heltable([4,2,8], n3(:,29), t3x8(:,:,21))
  n2(17) = 8
  call heltable([2,4,8], n3(:,30), t3x8(:,:,22))
  call heltable([8,8,64], n3(:,31), t3x64(:,:,1))
  call heltable([8,8,64], n3(:,32), t3x64(:,:,2))
  call heltable([8,8,64], n3(:,33), t3x64(:,:,3))
  call heltable([8,8,64], n3(:,34), t3x64(:,:,4))
  call heltable([8,8,64], n3(:,35), t3x64(:,:,5))
  call heltable([8,8,64], n3(:,36), t3x64(:,:,6))
  call heltable([8,8,64], n3(:,37), t3x64(:,:,7))
  call heltable([8,8,64], n3(:,38), t3x64(:,:,8))
  call heltable([4,16,64], n3(:,39), t3x64(:,:,9))
  call heltable([4,16,64], n3(:,40), t3x64(:,:,10))
  call heltable([8,8,64], n3(:,41), t3x64(:,:,11))
  call heltable([8,8,64], n3(:,42), t3x64(:,:,12))
  call heltable([8,8,64], n3(:,43), t3x64(:,:,13))
  call heltable([8,8,64], n3(:,44), t3x64(:,:,14))
  call heltable([8,8,64], n3(:,45), t3x64(:,:,15))
  call heltable([8,8,64], n3(:,46), t3x64(:,:,16))
  call heltable([8,8,64], n3(:,47), t3x64(:,:,17))
  call heltable([8,8,64], n3(:,48), t3x64(:,:,18))
  call heltable([8,8,64], n3(:,49), t3x64(:,:,19))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppllll_nenexeexuux_1
