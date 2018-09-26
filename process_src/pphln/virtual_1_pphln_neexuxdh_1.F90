
module ol_vamp_1_pphln_neexuxdh_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphln_neexuxdh_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphln_neexuxdh_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphln_neexuxdh_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphln_neexuxdh_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,2) :: G0
  complex(REALKIND), dimension(4,5,4,2) :: G1
  complex(REALKIND), dimension(4,15,4,1) :: G2
  complex(REALKIND), dimension(15,1) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,8),ZERO,G1(:,:,:,1))
  call loop_QW_A(G1(:,:,:,1),wf(:,8),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,27),ZERO,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,-2),G2tensor(:,1))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(1)*M(1))) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)

end subroutine vamp_1

end module ol_vamp_1_pphln_neexuxdh_1_/**/REALKIND
