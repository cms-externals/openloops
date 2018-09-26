
module ol_vamp_1_heftpph_hgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_heftpph_hgg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_heftpph_hgg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_heftpph_hgg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_heftpph_hgg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,6) :: G0
  complex(REALKIND), dimension(4,5,4,7) :: G1
  complex(REALKIND), dimension(4,15,4,5) :: G2
  complex(REALKIND), dimension(15,5) :: G2tensor
  complex(REALKIND), dimension(35,2) :: G3tensor
  complex(REALKIND), dimension(70,1) :: G4tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_AV_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,2),MB,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-2),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,6),MB,G2(:,:,:,1))
  call loop_AS_Q(G2(:,:,:,1),wf(:,0),G2(:,:,:,2),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,7),MB,G3tensor(:,1))
  call loop_QV_A(G0(:,:,:,1),wf(:,-1),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,2),MB,G1(:,:,:,3))
  call loop_QV_A(G1(:,:,:,3),wf(:,-2),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,6),MB,G2(:,:,:,3))
  call loop_QS_A(G2(:,:,:,3),wf(:,0),G2(:,:,:,4),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,4),Q(:,7),MB,G3tensor(:,2))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-1),Q(:,2),G1(:,:,:,5))
  call loop_UV_W(G1(:,:,:,5),Q(:,2),wf(:,-2),Q(:,4),G2(:,:,:,5))
  call check_last_GH_G(l_switch,G2(:,:,:,5),Q(:,6),wf(:,0),G4tensor(:,1),Q(:,7))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,-1),G0(:,:,:,4))
  call check_last_GH_G(l_switch,G0(:,:,:,4),Q(:,6),wf(:,0),G2tensor(:,1),Q(:,7))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,-2),G0(:,:,:,5))
  call check_last_GH_G(l_switch,G0(:,:,:,5),Q(:,6),wf(:,0),G2tensor(:,2),Q(:,7))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,-1),G0(:,:,:,6))
  call check_last_GH_G(l_switch,G0(:,:,:,6),Q(:,6),wf(:,0),G2tensor(:,3),Q(:,7))
  call loop_GHG_G(G0(:,:,:,1),Q(:,0),wf(:,0),wf(:,-2),Q(:,4),G1(:,:,:,6),Q(:,5))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,5),wf(:,-1),Q(:,2),G2tensor(:,4))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-2),Q(:,4),G1(:,:,:,7))
  call check_last_GHG_G(l_switch,G1(:,:,:,7),Q(:,4),wf(:,0),wf(:,-1),Q(:,2),G2tensor(:,5),Q(:,7))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(3)*M(1)))
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(3)*M(1)))
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(2)*M(1)))
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,1)
  Gcoeff = (-(c(1)*M(1)))
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(1)*M(1)))
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,2)
  Gcoeff = (0)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(1)*M(1)))
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(1)*M(1)))
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)

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

end module ol_vamp_1_heftpph_hgg_1_/**/REALKIND
