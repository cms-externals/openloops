
! **********************************************************************
module ol_heltables_ppwjj_uxccxdwx_1
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
    H5(3) = [-1,0,1]

  ! number of helicity states for wave functions returned by a propagator call
  ! integer(intkind2), save :: &
  !   na, nb, ...

  ! number of helicity states for wave functions in a v-point vertex call (v >= 3) or a contraction (v = 3)
  ! integer(intkind2), save, dimension(v) :: &
  !   nc, nd, ...

  integer(intkind2), save :: &
  n3, n6

  integer(intkind2), save, dimension(3) :: &
  n1, n2, n4, n5, n7, n8, n9


  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude) from an v-point vertex (contraction)
  ! integer(intkind2), save, dimension(v-1,h) :: &
  !   ta, tb, ...

  integer(intkind2), save, dimension(2,4) :: &
  t2

  integer(intkind2), save, dimension(2,12) :: &
  t4

  integer(intkind2), save, dimension(2,8) :: &
  t7

  integer(intkind2), save, dimension(2,6) :: &
  t1, t5

  integer(intkind2), save, dimension(2,48) :: &
  t8, t9


  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(48,5)
  integer,           save :: exthel(48,5)
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
  call heltable([2,3,6], n1, t1)
  call heltable([2,2,4], n2, t2)
  n3 = 6
  call heltable([2,6,12], n4, t4)
  call heltable([3,2,6], n5, t5)
  n6 = 6
  call heltable([2,4,8], n7, t7)
  call heltable([4,12,48], n8, t8)
  call heltable([6,8,48], n9, t9)

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppwjj_uxccxdwx_1
