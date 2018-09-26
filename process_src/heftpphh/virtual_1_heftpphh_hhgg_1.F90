
module ol_vamp_1_heftpphh_hhgg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_heftpphh_hhgg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_heftpphh_hhgg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_heftpphh_hhgg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_heftpphh_hhgg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,4) :: G0
  complex(REALKIND), dimension(4,5,4,4) :: G1
  complex(REALKIND), dimension(4,15,4,1) :: G2
  complex(REALKIND), dimension(15,10) :: G2tensor
  complex(REALKIND), dimension(70,2) :: G4tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-2),Q(:,4),G1(:,:,:,1))
  call loop_UV_W(G1(:,:,:,1),Q(:,4),wf(:,-3),Q(:,8),G2(:,:,:,1))
  call check_last_GHH_G(l_switch,G2(:,:,:,1),Q(:,12),wf(:,-1),wf(:,0),G4tensor(:,1),Q(:,15))
  call check_last_GH_G(l_switch,G2(:,:,:,1),Q(:,12),wf(:,2),G4tensor(:,2),Q(:,15))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,-2),G0(:,:,:,2))
  call check_last_GHH_G(l_switch,G0(:,:,:,2),Q(:,12),wf(:,-1),wf(:,0),G2tensor(:,1),Q(:,15))
  call check_last_GH_G(l_switch,G0(:,:,:,2),Q(:,12),wf(:,2),G2tensor(:,2),Q(:,15))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,-3),G0(:,:,:,3))
  call check_last_GHH_G(l_switch,G0(:,:,:,3),Q(:,12),wf(:,-1),wf(:,0),G2tensor(:,3),Q(:,15))
  call check_last_GH_G(l_switch,G0(:,:,:,3),Q(:,12),wf(:,2),G2tensor(:,4),Q(:,15))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,-2),G0(:,:,:,4))
  call check_last_GHH_G(l_switch,G0(:,:,:,4),Q(:,12),wf(:,-1),wf(:,0),G2tensor(:,5),Q(:,15))
  call check_last_GH_G(l_switch,G0(:,:,:,4),Q(:,12),wf(:,2),G2tensor(:,6),Q(:,15))
  call loop_GHHG_G(G0(:,:,:,1),Q(:,0),wf(:,-1),wf(:,0),wf(:,-3),Q(:,8),G1(:,:,:,2),Q(:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,11),wf(:,-2),Q(:,4),G2tensor(:,7))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-3),Q(:,8),G1(:,:,:,3))
  call check_last_GHHG_G(l_switch,G1(:,:,:,3),Q(:,8),wf(:,-1),wf(:,0),wf(:,-2),Q(:,4),G2tensor(:,8),Q(:,15))
  call check_last_GHG_G(l_switch,G1(:,:,:,3),Q(:,8),wf(:,2),wf(:,-2),Q(:,4),G2tensor(:,9),Q(:,15))
  call loop_GHG_G(G0(:,:,:,1),Q(:,0),wf(:,2),wf(:,-3),Q(:,8),G1(:,:,:,4),Q(:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,4),Q(:,11),wf(:,-2),Q(:,4),G2tensor(:,10))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(2)*M(1))
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,1)
  Gcoeff = (-(c(4)*M(1))) * den(1)
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(1)*M(1))
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(1)*M(1))
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,3)
  Gcoeff = (0)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(1)*M(1))
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(1)*M(1))
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(3)*M(1))) * den(1)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(3)*M(1))) * den(1)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(3)*M(1))) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,4)
  Gcoeff = (0) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,6)

end subroutine vamp_1

end module ol_vamp_1_heftpphh_hhgg_1_/**/REALKIND
