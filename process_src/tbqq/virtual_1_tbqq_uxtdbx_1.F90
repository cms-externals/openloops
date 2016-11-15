
module ol_vamp_1_tbqq_uxtdbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_tbqq_uxtdbx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_tbqq_uxtdbx_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_tbqq_uxtdbx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_tbqq_uxtdbx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,4) :: G0
  complex(REALKIND), dimension(4,5,4,12) :: G1
  complex(REALKIND), dimension(4,15,4,6) :: G2
  complex(REALKIND), dimension(15,6) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,8),MB,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,1),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,13),MT,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-1),G2tensor(:,1))
  call loop_AQ_W(G1(:,:,:,1),wf(:,-1),G1(:,:,:,3))
  call loop_WA_Q(G1(:,:,:,3),wf(:,0),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,11),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,-2),G2tensor(:,2))
  call loop_WQ_A(G1(:,:,:,3),wf(:,-2),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,14),ZERO,G2(:,:,:,3))
  call check_last_QA_V(l_switch,G2(:,:,:,3),wf(:,0),G2tensor(:,3))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,4),ZERO,G1(:,:,:,6))
  call loop_QW_A(G1(:,:,:,6),wf(:,2),G1(:,:,:,7))
  call loop_Q_A(G1(:,:,:,7),Q(:,14),ZERO,G2(:,:,:,4))
  call check_last_QA_V(l_switch,G2(:,:,:,4),wf(:,0),G2tensor(:,4))
  call loop_QA_W(G1(:,:,:,6),wf(:,0),G1(:,:,:,8))
  call loop_WA_Q(G1(:,:,:,8),wf(:,-3),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,13),MT,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,-1),G2tensor(:,5))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-1),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,2),MT,G1(:,:,:,10))
  call loop_QA_W(G1(:,:,:,10),wf(:,-3),G1(:,:,:,11))
  call loop_WQ_A(G1(:,:,:,11),wf(:,-2),G1(:,:,:,12))
  call loop_Q_A(G1(:,:,:,12),Q(:,14),ZERO,G2(:,:,:,6))
  call check_last_QA_V(l_switch,G2(:,:,:,6),wf(:,0),G2tensor(:,6))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(3)*M(1)) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(3)*M(1)) * den(2)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,5)

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

end module ol_vamp_1_tbqq_uxtdbx_1_/**/REALKIND
