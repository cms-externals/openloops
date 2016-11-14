
module ol_vamp_1_heftpphjj_uuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_heftpphjj_uuxddxh_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_heftpphjj_uuxddxh_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_heftpphjj_uuxddxh_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_heftpphjj_uuxddxh_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,10) :: G0
  complex(REALKIND), dimension(4,5,4,30) :: G1
  complex(REALKIND), dimension(4,15,4,17) :: G2
  complex(REALKIND), dimension(4,35,4,20) :: G3
  complex(REALKIND), dimension(4,70,4,8) :: G4
  complex(REALKIND), dimension(15,19) :: G2tensor
  complex(REALKIND), dimension(35,6) :: G3tensor
  complex(REALKIND), dimension(70,11) :: G4tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GHG_G(G0(:,:,:,1),Q(:,0),wf(:,-4),wf(:,1),Q(:,3),G1(:,:,:,1),Q(:,19))
  call loop_VA_Q(G1(:,:,:,1),wf(:,-3),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,27),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-2),G2tensor(:,1))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,19),wf(:,2),Q(:,12),G2tensor(:,2))
  call loop_GHG_G(G0(:,:,:,1),Q(:,0),wf(:,-4),wf(:,2),Q(:,12),G1(:,:,:,3),Q(:,28))
  call loop_VA_Q(G1(:,:,:,3),wf(:,-1),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,30),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,4))
  call loop_AV_Q(G0(:,:,:,1),wf(:,2),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,12),MB,G1(:,:,:,5))
  call loop_AS_Q(G1(:,:,:,5),wf(:,-4),G1(:,:,:,6),gH)
  call loop_A_Q(G1(:,:,:,6),Q(:,28),MB,G2(:,:,:,3))
  call loop_AV_Q(G2(:,:,:,3),wf(:,1),G2(:,:,:,4))
  call check_last_A_Q(l_switch,G2(:,:,:,4),Q(:,31),MB,G3tensor(:,1))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,12),MB,G1(:,:,:,7))
  call loop_QS_A(G1(:,:,:,7),wf(:,-4),G1(:,:,:,8),gH)
  call loop_Q_A(G1(:,:,:,8),Q(:,28),MB,G2(:,:,:,5))
  call loop_QV_A(G2(:,:,:,5),wf(:,1),G2(:,:,:,6))
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,31),MB,G3tensor(:,2))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,12),G1(:,:,:,9))
  call loop_GH_G(G1(:,:,:,9),Q(:,12),wf(:,-4),G3(:,:,:,1),Q(:,28))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,28),wf(:,1),Q(:,3),G4tensor(:,1))
  call loop_VA_Q(G3(:,:,:,1),wf(:,-1),G3(:,:,:,2))
  call loop_A_Q(G3(:,:,:,2),Q(:,30),ZERO,G4(:,:,:,1))
  call check_last_AQ_V(l_switch,G4(:,:,:,1),wf(:,0),G4tensor(:,2))
  call loop_GH_G(G0(:,:,:,1),Q(:,0),wf(:,-4),G2(:,:,:,7),Q(:,16))
  call loop_VA_Q(G2(:,:,:,7),wf(:,-3),G2(:,:,:,8))
  call loop_A_Q(G2(:,:,:,8),Q(:,24),ZERO,G3(:,:,:,3))
  call check_last_AQ_V(l_switch,G3(:,:,:,3),wf(:,25),G3tensor(:,3))
  call loop_AV_Q(G3(:,:,:,3),wf(:,1),G3(:,:,:,4))
  call loop_A_Q(G3(:,:,:,4),Q(:,27),ZERO,G4(:,:,:,2))
  call check_last_AQ_V(l_switch,G4(:,:,:,2),wf(:,-2),G4tensor(:,3))
  call loop_AQ_V(G3(:,:,:,3),wf(:,-2),G3(:,:,:,5))
  call loop_VA_Q(G3(:,:,:,5),wf(:,-1),G3(:,:,:,6))
  call loop_A_Q(G3(:,:,:,6),Q(:,30),ZERO,G4(:,:,:,3))
  call check_last_AQ_V(l_switch,G4(:,:,:,3),wf(:,0),G4tensor(:,4))
  call loop_VA_Q(G2(:,:,:,7),wf(:,26),G2(:,:,:,9))
  call loop_A_Q(G2(:,:,:,9),Q(:,27),ZERO,G3(:,:,:,7))
  call check_last_AQ_V(l_switch,G3(:,:,:,7),wf(:,-2),G3tensor(:,4))
  call loop_VQ_A(G2(:,:,:,7),wf(:,28),G2(:,:,:,10))
  call loop_Q_A(G2(:,:,:,10),Q(:,29),ZERO,G3(:,:,:,8))
  call check_last_QA_V(l_switch,G3(:,:,:,8),wf(:,-1),G3tensor(:,5))
  call loop_VA_Q(G2(:,:,:,7),wf(:,30),G2(:,:,:,11))
  call loop_A_Q(G2(:,:,:,11),Q(:,30),ZERO,G3(:,:,:,9))
  call check_last_AQ_V(l_switch,G3(:,:,:,9),wf(:,0),G3tensor(:,6))
  call loop_UV_W(G2(:,:,:,7),Q(:,16),wf(:,2),Q(:,12),G3(:,:,:,10))
  call loop_VA_Q(G3(:,:,:,10),wf(:,-1),G3(:,:,:,11))
  call loop_A_Q(G3(:,:,:,11),Q(:,30),ZERO,G4(:,:,:,4))
  call check_last_AQ_V(l_switch,G4(:,:,:,4),wf(:,0),G4tensor(:,5))
  call loop_VA_Q(G2(:,:,:,7),wf(:,-1),G2(:,:,:,12))
  call loop_A_Q(G2(:,:,:,12),Q(:,18),ZERO,G3(:,:,:,12))
  call loop_AV_Q(G3(:,:,:,12),wf(:,2),G3(:,:,:,13))
  call loop_A_Q(G3(:,:,:,13),Q(:,30),ZERO,G4(:,:,:,5))
  call check_last_AQ_V(l_switch,G4(:,:,:,5),wf(:,0),G4tensor(:,6))
  call loop_VQ_A(G2(:,:,:,7),wf(:,-2),G2(:,:,:,13))
  call loop_Q_A(G2(:,:,:,13),Q(:,20),ZERO,G3(:,:,:,14))
  call loop_QA_V(G3(:,:,:,14),wf(:,-3),G3(:,:,:,15))
  call loop_VA_Q(G3(:,:,:,15),wf(:,-1),G3(:,:,:,16))
  call loop_A_Q(G3(:,:,:,16),Q(:,30),ZERO,G4(:,:,:,6))
  call check_last_AQ_V(l_switch,G4(:,:,:,6),wf(:,0),G4tensor(:,7))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,7),wf(:,1),wf(:,2),G2tensor(:,5))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,7),wf(:,2),wf(:,1),G2tensor(:,6))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,7),wf(:,1),wf(:,2),G2tensor(:,7))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,8),ZERO,G1(:,:,:,10))
  call loop_AV_Q(G1(:,:,:,10),wf(:,3),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,27),ZERO,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-2),G2tensor(:,8))
  call loop_AQ_V(G1(:,:,:,10),wf(:,-2),G1(:,:,:,12))
  call loop_GH_G(G1(:,:,:,12),Q(:,12),wf(:,-4),G3(:,:,:,17),Q(:,28))
  call check_last_UV_W(l_switch,G3(:,:,:,17),Q(:,28),wf(:,1),Q(:,3),G4tensor(:,8))
  call loop_VA_Q(G3(:,:,:,17),wf(:,-1),G3(:,:,:,18))
  call loop_A_Q(G3(:,:,:,18),Q(:,30),ZERO,G4(:,:,:,7))
  call check_last_AQ_V(l_switch,G4(:,:,:,7),wf(:,0),G4tensor(:,9))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,3),Q(:,19),G1(:,:,:,13))
  call loop_VA_Q(G1(:,:,:,13),wf(:,-3),G1(:,:,:,14))
  call loop_A_Q(G1(:,:,:,14),Q(:,27),ZERO,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-2),G2tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,19),wf(:,2),Q(:,12),G2tensor(:,10))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,4),ZERO,G1(:,:,:,15))
  call loop_QA_V(G1(:,:,:,15),wf(:,-3),G1(:,:,:,16))
  call loop_GH_G(G1(:,:,:,16),Q(:,12),wf(:,-4),G3(:,:,:,19),Q(:,28))
  call check_last_UV_W(l_switch,G3(:,:,:,19),Q(:,28),wf(:,1),Q(:,3),G4tensor(:,10))
  call loop_VA_Q(G3(:,:,:,19),wf(:,-1),G3(:,:,:,20))
  call loop_A_Q(G3(:,:,:,20),Q(:,30),ZERO,G4(:,:,:,8))
  call check_last_AQ_V(l_switch,G4(:,:,:,8),wf(:,0),G4tensor(:,11))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,2),ZERO,G1(:,:,:,17))
  call loop_AV_Q(G1(:,:,:,17),wf(:,22),G1(:,:,:,18))
  call loop_A_Q(G1(:,:,:,18),Q(:,30),ZERO,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,0),G2tensor(:,11))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,22),Q(:,28),G1(:,:,:,19))
  call loop_VA_Q(G1(:,:,:,19),wf(:,-1),G1(:,:,:,20))
  call loop_A_Q(G1(:,:,:,20),Q(:,30),ZERO,G2(:,:,:,17))
  call check_last_AQ_V(l_switch,G2(:,:,:,17),wf(:,0),G2tensor(:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,13))
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,19),ZERO,G1(:,:,:,21))
  call loop_QV_A(G1(:,:,:,21),wf(:,2),G1(:,:,:,22))
  call check_last_Q_A(l_switch,G1(:,:,:,22),Q(:,31),ZERO,G2tensor(:,14))
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,19),MB,G1(:,:,:,23))
  call loop_QV_A(G1(:,:,:,23),wf(:,2),G1(:,:,:,24))
  call check_last_Q_A(l_switch,G1(:,:,:,24),Q(:,31),MB,G2tensor(:,15))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,3),Q(:,19),G1(:,:,:,25))
  call check_last_CV_D(l_switch,G1(:,:,:,25),Q(:,19),wf(:,2),Q(:,12),G2tensor(:,16))
  call loop_QV_A(G0(:,:,:,1),wf(:,22),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,28),ZERO,G1(:,:,:,26))
  call loop_QV_A(G1(:,:,:,26),wf(:,1),G1(:,:,:,27))
  call check_last_Q_A(l_switch,G1(:,:,:,27),Q(:,31),ZERO,G2tensor(:,17))
  call loop_QV_A(G0(:,:,:,1),wf(:,22),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,28),MB,G1(:,:,:,28))
  call loop_QV_A(G1(:,:,:,28),wf(:,1),G1(:,:,:,29))
  call check_last_Q_A(l_switch,G1(:,:,:,29),Q(:,31),MB,G2tensor(:,18))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,22),Q(:,28),G1(:,:,:,30))
  call check_last_CV_D(l_switch,G1(:,:,:,30),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,19))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(2)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(15)*M(1))+c(14)*M(2)) * den(3)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(15)*M(1))+c(14)*M(2)) * den(3)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(13)*M(1))+c(10)*M(2)) * den(3)
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,1)
  Gcoeff = (-(c(12)*M(1))+c(8)*M(2)) * den(19)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,3)
  Gcoeff = (-(c(12)*M(1))+c(8)*M(2)) * den(21)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(6)*M(1)-c(5)*M(2)) * den(16)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(16)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(1)
  T4sum(1:70,2) = T4sum(1:70,2) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(1)
  T4sum(1:70,3) = T4sum(1:70,3) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(6)*M(1)-c(5)*M(2)) * den(1)
  T4sum(1:70,4) = T4sum(1:70,4) + Gcoeff * G4tensor(:,3)
  Gcoeff = (-(c(12)*M(1))+c(8)*M(2)) * den(23)
  T3sum(1:35,4) = T3sum(1:35,4) + Gcoeff * G3tensor(:,5)
  Gcoeff = (-(c(12)*M(1))+c(8)*M(2)) * den(25)
  T3sum(1:35,5) = T3sum(1:35,5) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(6)*M(1)-c(5)*M(2)) * den(13)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(13)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(2)
  T4sum(1:70,5) = T4sum(1:70,5) + Gcoeff * G4tensor(:,5)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(2)
  T4sum(1:70,6) = T4sum(1:70,6) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(6)*M(1)-c(5)*M(2)) * den(2)
  T4sum(1:70,7) = T4sum(1:70,7) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(7)*M(1)-c(9)*M(2))
  T4sum(1:70,8) = T4sum(1:70,8) + Gcoeff * G4tensor(:,11)
  Gcoeff = (-(c(11)*M(1))-c(5)*M(2))
  T4sum(1:70,9) = T4sum(1:70,9) + Gcoeff * G4tensor(:,9)
  Gcoeff = (-(c(11)*M(1))-c(5)*M(2))
  T4sum(1:70,10) = T4sum(1:70,10) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(7)*M(1)-c(9)*M(2))
  T4sum(1:70,11) = T4sum(1:70,11) + Gcoeff * G4tensor(:,7)
  Gcoeff = (-(c(4)*M(1))+c(3)*M(2)) * den(3)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(4)*M(1))+c(3)*M(2)) * den(3)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,6)
  Gcoeff = (0) * den(3)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(4)*M(1))+c(3)*M(2)) * den(3)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(4)*M(1))+c(3)*M(2)) * den(3)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(19)*M(1)-c(18)*M(2)) * den(17)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(19)*M(1)-c(18)*M(2)) * den(17)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(17)*M(1)-c(16)*M(2)) * den(17)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(13)*M(1)-c(10)*M(2)) * den(17)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,16)
  Gcoeff = (-(c(4)*M(1))+c(3)*M(2)) * den(17)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(19)*M(1)-c(18)*M(2)) * den(14)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(19)*M(1)-c(18)*M(2)) * den(14)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(17)*M(1)-c(16)*M(2)) * den(14)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(13)*M(1)-c(10)*M(2)) * den(14)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,19)
  Gcoeff = (-(c(4)*M(1))+c(3)*M(2)) * den(14)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,13)

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

end module ol_vamp_1_heftpphjj_uuxddxh_1_/**/REALKIND
