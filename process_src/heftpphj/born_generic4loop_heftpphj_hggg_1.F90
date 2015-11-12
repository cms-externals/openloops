
! **********************************************************************
module ol_heltables_heftpphj_hggg_1
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
    H4(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n3(3,10), n4(4,1)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x2(2,2,3), t3x4(2,4,3), t3x8(2,8,4), t4x4(3,4,1)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(8,4)
  integer,           save :: exthel(8,4)
  integer,           save :: firstpol(4)

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
  call heltable([1,2,2,4], n4(:,1), t4x4(:,:,1))
  call heltable([1,2,2], n3(:,1), t3x2(:,:,1))
  call heltable([2,2,4], n3(:,2), t3x4(:,:,1))
  call heltable([1,2,2], n3(:,3), t3x2(:,:,2))
  call heltable([2,2,4], n3(:,4), t3x4(:,:,2))
  call heltable([1,2,2], n3(:,5), t3x2(:,:,3))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,3))
  call heltable([2,4,8], n3(:,7), t3x8(:,:,1))
  call heltable([2,4,8], n3(:,8), t3x8(:,:,2))
  call heltable([2,4,8], n3(:,9), t3x8(:,:,3))
  call heltable([2,4,8], n3(:,10), t3x8(:,:,4))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_heftpphj_hggg_1
