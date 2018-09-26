
module ol_vamp_1_pphjj_vbf_uuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphjj_vbf_uuxddxh_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphjj_vbf_uuxddxh_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphjj_vbf_uuxddxh_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphjj_vbf_uuxddxh_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,35) :: G0
  complex(REALKIND), dimension(4,5,4,36) :: G1
  complex(REALKIND), dimension(4,15,4,12) :: G2
  complex(REALKIND), dimension(5,8) :: G1tensor
  complex(REALKIND), dimension(15,16) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QA_Z(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2),gZd)
  call loop_VS_V(G0(:,:,:,2),wf(:,-4),G0(:,:,:,3))
  call loop_ZQ_A(G0(:,:,:,3),wf(:,19),G0(:,:,:,4),gZd)
  call check_last_Q_A(l_switch,G0(:,:,:,4),Q(:,31),ZERO,G1tensor(:,1))
  call loop_QA_W(G0(:,:,:,1),wf(:,-3),G0(:,:,:,5))
  call loop_VS_V(G0(:,:,:,5),wf(:,-4),G0(:,:,:,6))
  call loop_WQ_A(G0(:,:,:,6),wf(:,19),G0(:,:,:,7))
  call check_last_Q_A(l_switch,G0(:,:,:,7),Q(:,31),ZERO,G1tensor(:,2))
  call loop_QA_Z(G0(:,:,:,1),wf(:,21),G0(:,:,:,8),gZd)
  call loop_VS_V(G0(:,:,:,8),wf(:,-4),G0(:,:,:,9))
  call loop_ZQ_A(G0(:,:,:,9),wf(:,-2),G0(:,:,:,10),gZd)
  call check_last_Q_A(l_switch,G0(:,:,:,10),Q(:,31),ZERO,G1tensor(:,3))
  call loop_QA_W(G0(:,:,:,1),wf(:,21),G0(:,:,:,11))
  call loop_VS_V(G0(:,:,:,11),wf(:,-4),G0(:,:,:,12))
  call loop_WQ_A(G0(:,:,:,12),wf(:,-2),G0(:,:,:,13))
  call check_last_Q_A(l_switch,G0(:,:,:,13),Q(:,31),ZERO,G1tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,14))
  call loop_A_Q(G0(:,:,:,14),Q(:,8),ZERO,G1(:,:,:,1))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,3),G1(:,:,:,2),gZd)
  call loop_A_Q(G1(:,:,:,2),Q(:,27),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-2),G2tensor(:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,22),G1(:,:,:,3))
  call loop_A_Q(G1(:,:,:,3),Q(:,30),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,2))
  call loop_AQ_Z(G1(:,:,:,1),wf(:,-2),G1(:,:,:,4),gZd)
  call loop_VS_V(G1(:,:,:,4),wf(:,-4),G1(:,:,:,5))
  call loop_ZA_Q(G1(:,:,:,5),wf(:,-1),G1(:,:,:,6),gZu)
  call loop_A_Q(G1(:,:,:,6),Q(:,30),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,0),G2tensor(:,3))
  call loop_ZQ_A(G1(:,:,:,5),wf(:,0),G1(:,:,:,7),gZu)
  call loop_Q_A(G1(:,:,:,7),Q(:,29),ZERO,G2(:,:,:,4))
  call check_last_QA_V(l_switch,G2(:,:,:,4),wf(:,-1),G2tensor(:,4))
  call loop_AQ_W(G1(:,:,:,1),wf(:,0),G1(:,:,:,8))
  call loop_VS_V(G1(:,:,:,8),wf(:,-4),G1(:,:,:,9))
  call loop_WQ_A(G1(:,:,:,9),wf(:,-2),G1(:,:,:,10))
  call loop_Q_A(G1(:,:,:,10),Q(:,29),ZERO,G2(:,:,:,5))
  call check_last_QA_V(l_switch,G2(:,:,:,5),wf(:,-1),G2tensor(:,5))
  call loop_WA_Q(G1(:,:,:,9),wf(:,-1),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,27),ZERO,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,-2),G2tensor(:,6))
  call loop_AQ_Z(G0(:,:,:,1),wf(:,-2),G0(:,:,:,15),gZd)
  call loop_VS_V(G0(:,:,:,15),wf(:,-4),G0(:,:,:,16))
  call loop_ZA_Q(G0(:,:,:,16),wf(:,-3),G0(:,:,:,17),gZd)
  call loop_A_Q(G0(:,:,:,17),Q(:,28),ZERO,G1(:,:,:,12))
  call loop_AV_Q(G1(:,:,:,12),wf(:,17),G1(:,:,:,13))
  call check_last_A_Q(l_switch,G1(:,:,:,13),Q(:,31),ZERO,G2tensor(:,7))
  call loop_AQ_W(G0(:,:,:,1),wf(:,-2),G0(:,:,:,18))
  call loop_VS_V(G0(:,:,:,18),wf(:,-4),G0(:,:,:,19))
  call loop_WA_Q(G0(:,:,:,19),wf(:,-3),G0(:,:,:,20))
  call loop_A_Q(G0(:,:,:,20),Q(:,28),ZERO,G1(:,:,:,14))
  call loop_AV_Q(G1(:,:,:,14),wf(:,17),G1(:,:,:,15))
  call check_last_A_Q(l_switch,G1(:,:,:,15),Q(:,31),ZERO,G2tensor(:,8))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,21))
  call loop_Q_A(G0(:,:,:,21),Q(:,4),ZERO,G1(:,:,:,16))
  call loop_QW_A(G1(:,:,:,16),wf(:,6),G1(:,:,:,17))
  call loop_Q_A(G1(:,:,:,17),Q(:,29),ZERO,G2(:,:,:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,7),wf(:,-1),G2tensor(:,9))
  call loop_QA_Z(G1(:,:,:,16),wf(:,-3),G1(:,:,:,18),gZd)
  call loop_VS_V(G1(:,:,:,18),wf(:,-4),G1(:,:,:,19))
  call loop_ZA_Q(G1(:,:,:,19),wf(:,-1),G1(:,:,:,20),gZu)
  call loop_A_Q(G1(:,:,:,20),Q(:,30),ZERO,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,0),G2tensor(:,10))
  call loop_ZQ_A(G1(:,:,:,19),wf(:,0),G1(:,:,:,21),gZu)
  call loop_Q_A(G1(:,:,:,21),Q(:,29),ZERO,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-1),G2tensor(:,11))
  call loop_QA_W(G1(:,:,:,16),wf(:,-1),G1(:,:,:,22))
  call loop_VS_V(G1(:,:,:,22),wf(:,-4),G1(:,:,:,23))
  call loop_WA_Q(G1(:,:,:,23),wf(:,-3),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,30),ZERO,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,0),G2tensor(:,12))
  call loop_AQ_Z(G0(:,:,:,1),wf(:,25),G0(:,:,:,22),gZu)
  call loop_VS_V(G0(:,:,:,22),wf(:,-4),G0(:,:,:,23))
  call loop_ZA_Q(G0(:,:,:,23),wf(:,-1),G0(:,:,:,24),gZu)
  call check_last_A_Q(l_switch,G0(:,:,:,24),Q(:,31),ZERO,G1tensor(:,5))
  call loop_AQ_W(G0(:,:,:,1),wf(:,25),G0(:,:,:,25))
  call loop_VS_V(G0(:,:,:,25),wf(:,-4),G0(:,:,:,26))
  call loop_WA_Q(G0(:,:,:,26),wf(:,-1),G0(:,:,:,27))
  call check_last_A_Q(l_switch,G0(:,:,:,27),Q(:,31),ZERO,G1tensor(:,6))
  call loop_QA_Z(G0(:,:,:,1),wf(:,27),G0(:,:,:,28),gZu)
  call loop_VS_V(G0(:,:,:,28),wf(:,-4),G0(:,:,:,29))
  call loop_ZQ_A(G0(:,:,:,29),wf(:,0),G0(:,:,:,30),gZu)
  call check_last_Q_A(l_switch,G0(:,:,:,30),Q(:,31),ZERO,G1tensor(:,7))
  call loop_QA_W(G0(:,:,:,1),wf(:,27),G0(:,:,:,31))
  call loop_VS_V(G0(:,:,:,31),wf(:,-4),G0(:,:,:,32))
  call loop_WQ_A(G0(:,:,:,32),wf(:,0),G0(:,:,:,33))
  call check_last_Q_A(l_switch,G0(:,:,:,33),Q(:,31),ZERO,G1tensor(:,8))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,34))
  call loop_A_Q(G0(:,:,:,34),Q(:,2),ZERO,G1(:,:,:,25))
  call loop_AZ_Q(G1(:,:,:,25),wf(:,28),G1(:,:,:,26),gZu)
  call loop_A_Q(G1(:,:,:,26),Q(:,30),ZERO,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,0),G2tensor(:,13))
  call loop_AQ_W(G1(:,:,:,25),wf(:,-2),G1(:,:,:,27))
  call loop_VS_V(G1(:,:,:,27),wf(:,-4),G1(:,:,:,28))
  call loop_WA_Q(G1(:,:,:,28),wf(:,-3),G1(:,:,:,29))
  call loop_A_Q(G1(:,:,:,29),Q(:,30),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,0),G2tensor(:,14))
  call loop_QV_A(G0(:,:,:,1),wf(:,23),G0(:,:,:,35))
  call loop_Q_A(G0(:,:,:,35),Q(:,12),ZERO,G1(:,:,:,30))
  call loop_QA_Z(G1(:,:,:,30),wf(:,-1),G1(:,:,:,31),gZu)
  call loop_VS_V(G1(:,:,:,31),wf(:,-4),G1(:,:,:,32))
  call loop_ZQ_A(G1(:,:,:,32),wf(:,0),G1(:,:,:,33),gZu)
  call check_last_Q_A(l_switch,G1(:,:,:,33),Q(:,31),ZERO,G2tensor(:,15))
  call loop_QA_W(G1(:,:,:,30),wf(:,-1),G1(:,:,:,34))
  call loop_VS_V(G1(:,:,:,34),wf(:,-4),G1(:,:,:,35))
  call loop_WQ_A(G1(:,:,:,35),wf(:,0),G1(:,:,:,36))
  call check_last_Q_A(l_switch,G1(:,:,:,36),Q(:,31),ZERO,G2tensor(:,16))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(5)*M(1)-c(4)*M(2)) * den(9)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(9)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(5)*M(1)-c(4)*M(2)) * den(11)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(11)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(6)*M(2)) * den(13)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*M(1)-c(4)*M(2)) * den(7)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(7)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(3)*M(1))) * den(15)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(17)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(5)*M(1)-c(4)*M(2)) * den(20)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(20)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(5)*M(1)-c(4)*M(2)) * den(22)
  T1sum(1:5,7) = T1sum(1:5,7) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(22)
  T1sum(1:5,8) = T1sum(1:5,8) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(6)*M(2)) * den(24)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(5)*M(1)-c(4)*M(2)) * den(18)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(18)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(5)*M(1)-c(4)*M(2))
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(1)*M(1)-c(2)*M(2))
  T2sum(1:15,10) = T2sum(1:15,10) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(5)*M(1)-c(4)*M(2))
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*M(1)-c(4)*M(2))
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(5)*M(1)-c(4)*M(2))
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(1)*M(1)-c(2)*M(2))
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(1)*M(1)-c(2)*M(2))
  T2sum(1:15,15) = T2sum(1:15,15) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(1)*M(1)-c(2)*M(2))
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,6)

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

end module ol_vamp_1_pphjj_vbf_uuxddxh_1_/**/REALKIND
