
module ol_vamp_1_pphjj_vbf_uxcdsxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphjj_vbf_uxcdsxh_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphjj_vbf_uxcdsxh_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphjj_vbf_uxcdsxh_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphjj_vbf_uxcdsxh_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,3) :: G0
  complex(REALKIND), dimension(4,5,4,4) :: G1
  complex(REALKIND), dimension(4,15,4,2) :: G2
  complex(REALKIND), dimension(15,2) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,8),ZERO,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,11),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,29),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-1),G2tensor(:,1))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,4),ZERO,G1(:,:,:,3))
  call loop_QW_A(G1(:,:,:,3),wf(:,13),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,30),ZERO,G2(:,:,:,2))
  call check_last_QA_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,2))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(1)*M(1)) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(1)*M(1)) * den(7)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,2)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_1

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_1_pphjj_vbf_uxcdsxh_1_/**/REALKIND
