
module ol_vamp_1_ppllllj_eeexexddx_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj_eeexexddx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj_eeexexddx_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj_eeexexddx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj_eeexexddx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,1) :: G0


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral

end subroutine vamp_1

end module ol_vamp_1_ppllllj_eeexexddx_1_/**/REALKIND
