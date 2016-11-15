
module ol_vamp_1_pphhjj_uxcdsxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphhjj_uxcdsxhh_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphhjj_uxcdsxhh_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphhjj_uxcdsxhh_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphhjj_uxcdsxhh_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,4) :: G0
  complex(REALKIND), dimension(4,5,4,52) :: G1
  complex(REALKIND), dimension(4,15,4,34) :: G2
  complex(REALKIND), dimension(4,35,4,14) :: G3
  complex(REALKIND), dimension(4,70,4,8) :: G4
  complex(REALKIND), dimension(15,28) :: G2tensor
  complex(REALKIND), dimension(70,8) :: G4tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,8),ZERO,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,3),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,61),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-1),G2tensor(:,1))
  call loop_AQ_W(G1(:,:,:,1),wf(:,-1),G1(:,:,:,3))
  call loop_VSS_V(G1(:,:,:,3),wf(:,-5),wf(:,-4),G1(:,:,:,4))
  call loop_WQ_A(G1(:,:,:,4),wf(:,-2),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,62),ZERO,G2(:,:,:,2))
  call check_last_QA_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,2))
  call loop_WA_Q(G1(:,:,:,4),wf(:,0),G1(:,:,:,6))
  call loop_A_Q(G1(:,:,:,6),Q(:,59),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,-2),G2tensor(:,3))
  call loop_VS_V(G1(:,:,:,3),wf(:,4),G1(:,:,:,7))
  call loop_WQ_A(G1(:,:,:,7),wf(:,-2),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,62),ZERO,G2(:,:,:,4))
  call check_last_QA_V(l_switch,G2(:,:,:,4),wf(:,0),G2tensor(:,4))
  call loop_WA_Q(G1(:,:,:,7),wf(:,0),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,59),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,-2),G2tensor(:,5))
  call loop_VT_S(G1(:,:,:,3),Q(:,10),wf(:,-4),Q(:,16),G2(:,:,:,6))
  call loop_ST_V(G2(:,:,:,6),Q(:,26),wf(:,-5),Q(:,32),G3(:,:,:,1))
  call loop_WQ_A(G3(:,:,:,1),wf(:,-2),G3(:,:,:,2))
  call loop_Q_A(G3(:,:,:,2),Q(:,62),ZERO,G4(:,:,:,1))
  call check_last_QA_V(l_switch,G4(:,:,:,1),wf(:,0),G4tensor(:,1))
  call loop_WA_Q(G3(:,:,:,1),wf(:,0),G3(:,:,:,3))
  call loop_A_Q(G3(:,:,:,3),Q(:,59),ZERO,G4(:,:,:,2))
  call check_last_AQ_V(l_switch,G4(:,:,:,2),wf(:,-2),G4tensor(:,2))
  call loop_VS_V(G1(:,:,:,3),wf(:,-4),G1(:,:,:,10))
  call loop_VS_V(G1(:,:,:,10),wf(:,-5),G1(:,:,:,11))
  call loop_WQ_A(G1(:,:,:,11),wf(:,-2),G1(:,:,:,12))
  call loop_Q_A(G1(:,:,:,12),Q(:,62),ZERO,G2(:,:,:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,7),wf(:,0),G2tensor(:,6))
  call loop_WA_Q(G1(:,:,:,11),wf(:,0),G1(:,:,:,13))
  call loop_A_Q(G1(:,:,:,13),Q(:,59),ZERO,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,-2),G2tensor(:,7))
  call loop_VT_S(G1(:,:,:,3),Q(:,10),wf(:,-5),Q(:,32),G2(:,:,:,9))
  call loop_ST_V(G2(:,:,:,9),Q(:,42),wf(:,-4),Q(:,16),G3(:,:,:,4))
  call loop_WQ_A(G3(:,:,:,4),wf(:,-2),G3(:,:,:,5))
  call loop_Q_A(G3(:,:,:,5),Q(:,62),ZERO,G4(:,:,:,3))
  call check_last_QA_V(l_switch,G4(:,:,:,3),wf(:,0),G4tensor(:,3))
  call loop_WA_Q(G3(:,:,:,4),wf(:,0),G3(:,:,:,6))
  call loop_A_Q(G3(:,:,:,6),Q(:,59),ZERO,G4(:,:,:,4))
  call check_last_AQ_V(l_switch,G4(:,:,:,4),wf(:,-2),G4tensor(:,4))
  call loop_VS_V(G1(:,:,:,3),wf(:,-5),G1(:,:,:,14))
  call loop_VS_V(G1(:,:,:,14),wf(:,-4),G1(:,:,:,15))
  call loop_WQ_A(G1(:,:,:,15),wf(:,-2),G1(:,:,:,16))
  call loop_Q_A(G1(:,:,:,16),Q(:,62),ZERO,G2(:,:,:,10))
  call check_last_QA_V(l_switch,G2(:,:,:,10),wf(:,0),G2tensor(:,8))
  call loop_WA_Q(G1(:,:,:,15),wf(:,0),G1(:,:,:,17))
  call loop_A_Q(G1(:,:,:,17),Q(:,59),ZERO,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,-2),G2tensor(:,9))
  call loop_AW_Q(G1(:,:,:,1),wf(:,40),G1(:,:,:,18))
  call loop_A_Q(G1(:,:,:,18),Q(:,61),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,-1),G2tensor(:,10))
  call loop_AW_Q(G1(:,:,:,1),wf(:,41),G1(:,:,:,19))
  call loop_A_Q(G1(:,:,:,19),Q(:,61),ZERO,G2(:,:,:,13))
  call check_last_AQ_V(l_switch,G2(:,:,:,13),wf(:,-1),G2tensor(:,11))
  call loop_AW_Q(G1(:,:,:,1),wf(:,42),G1(:,:,:,20))
  call loop_A_Q(G1(:,:,:,20),Q(:,61),ZERO,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-1),G2tensor(:,12))
  call loop_AW_Q(G1(:,:,:,1),wf(:,43),G1(:,:,:,21))
  call loop_A_Q(G1(:,:,:,21),Q(:,61),ZERO,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-1),G2tensor(:,13))
  call loop_AW_Q(G1(:,:,:,1),wf(:,44),G1(:,:,:,22))
  call loop_A_Q(G1(:,:,:,22),Q(:,61),ZERO,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,-1),G2tensor(:,14))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,4),ZERO,G1(:,:,:,23))
  call loop_QW_A(G1(:,:,:,23),wf(:,39),G1(:,:,:,24))
  call loop_Q_A(G1(:,:,:,24),Q(:,62),ZERO,G2(:,:,:,17))
  call check_last_QA_V(l_switch,G2(:,:,:,17),wf(:,0),G2tensor(:,15))
  call loop_QA_W(G1(:,:,:,23),wf(:,0),G1(:,:,:,25))
  call loop_VSS_V(G1(:,:,:,25),wf(:,-5),wf(:,-4),G1(:,:,:,26))
  call loop_WA_Q(G1(:,:,:,26),wf(:,-3),G1(:,:,:,27))
  call loop_A_Q(G1(:,:,:,27),Q(:,61),ZERO,G2(:,:,:,18))
  call check_last_AQ_V(l_switch,G2(:,:,:,18),wf(:,-1),G2tensor(:,16))
  call loop_VS_V(G1(:,:,:,25),wf(:,4),G1(:,:,:,28))
  call loop_WA_Q(G1(:,:,:,28),wf(:,-3),G1(:,:,:,29))
  call loop_A_Q(G1(:,:,:,29),Q(:,61),ZERO,G2(:,:,:,19))
  call check_last_AQ_V(l_switch,G2(:,:,:,19),wf(:,-1),G2tensor(:,17))
  call loop_VT_S(G1(:,:,:,25),Q(:,5),wf(:,-4),Q(:,16),G2(:,:,:,20))
  call loop_ST_V(G2(:,:,:,20),Q(:,21),wf(:,-5),Q(:,32),G3(:,:,:,7))
  call loop_WA_Q(G3(:,:,:,7),wf(:,-3),G3(:,:,:,8))
  call loop_A_Q(G3(:,:,:,8),Q(:,61),ZERO,G4(:,:,:,5))
  call check_last_AQ_V(l_switch,G4(:,:,:,5),wf(:,-1),G4tensor(:,5))
  call loop_VS_V(G1(:,:,:,25),wf(:,-4),G1(:,:,:,30))
  call loop_VS_V(G1(:,:,:,30),wf(:,-5),G1(:,:,:,31))
  call loop_WA_Q(G1(:,:,:,31),wf(:,-3),G1(:,:,:,32))
  call loop_A_Q(G1(:,:,:,32),Q(:,61),ZERO,G2(:,:,:,21))
  call check_last_AQ_V(l_switch,G2(:,:,:,21),wf(:,-1),G2tensor(:,18))
  call loop_VT_S(G1(:,:,:,25),Q(:,5),wf(:,-5),Q(:,32),G2(:,:,:,22))
  call loop_ST_V(G2(:,:,:,22),Q(:,37),wf(:,-4),Q(:,16),G3(:,:,:,9))
  call loop_WA_Q(G3(:,:,:,9),wf(:,-3),G3(:,:,:,10))
  call loop_A_Q(G3(:,:,:,10),Q(:,61),ZERO,G4(:,:,:,6))
  call check_last_AQ_V(l_switch,G4(:,:,:,6),wf(:,-1),G4tensor(:,6))
  call loop_VS_V(G1(:,:,:,25),wf(:,-5),G1(:,:,:,33))
  call loop_VS_V(G1(:,:,:,33),wf(:,-4),G1(:,:,:,34))
  call loop_WA_Q(G1(:,:,:,34),wf(:,-3),G1(:,:,:,35))
  call loop_A_Q(G1(:,:,:,35),Q(:,61),ZERO,G2(:,:,:,23))
  call check_last_AQ_V(l_switch,G2(:,:,:,23),wf(:,-1),G2tensor(:,19))
  call loop_QW_A(G1(:,:,:,23),wf(:,45),G1(:,:,:,36))
  call loop_Q_A(G1(:,:,:,36),Q(:,62),ZERO,G2(:,:,:,24))
  call check_last_QA_V(l_switch,G2(:,:,:,24),wf(:,0),G2tensor(:,20))
  call loop_QW_A(G1(:,:,:,23),wf(:,46),G1(:,:,:,37))
  call loop_Q_A(G1(:,:,:,37),Q(:,62),ZERO,G2(:,:,:,25))
  call check_last_QA_V(l_switch,G2(:,:,:,25),wf(:,0),G2tensor(:,21))
  call loop_QW_A(G1(:,:,:,23),wf(:,47),G1(:,:,:,38))
  call loop_Q_A(G1(:,:,:,38),Q(:,62),ZERO,G2(:,:,:,26))
  call check_last_QA_V(l_switch,G2(:,:,:,26),wf(:,0),G2tensor(:,22))
  call loop_QW_A(G1(:,:,:,23),wf(:,48),G1(:,:,:,39))
  call loop_Q_A(G1(:,:,:,39),Q(:,62),ZERO,G2(:,:,:,27))
  call check_last_QA_V(l_switch,G2(:,:,:,27),wf(:,0),G2tensor(:,23))
  call loop_QW_A(G1(:,:,:,23),wf(:,49),G1(:,:,:,40))
  call loop_Q_A(G1(:,:,:,40),Q(:,62),ZERO,G2(:,:,:,28))
  call check_last_QA_V(l_switch,G2(:,:,:,28),wf(:,0),G2tensor(:,24))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-1),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,2),ZERO,G1(:,:,:,41))
  call loop_QA_W(G1(:,:,:,41),wf(:,-3),G1(:,:,:,42))
  call loop_VSS_V(G1(:,:,:,42),wf(:,-5),wf(:,-4),G1(:,:,:,43))
  call loop_WQ_A(G1(:,:,:,43),wf(:,-2),G1(:,:,:,44))
  call loop_Q_A(G1(:,:,:,44),Q(:,62),ZERO,G2(:,:,:,29))
  call check_last_QA_V(l_switch,G2(:,:,:,29),wf(:,0),G2tensor(:,25))
  call loop_VS_V(G1(:,:,:,42),wf(:,4),G1(:,:,:,45))
  call loop_WQ_A(G1(:,:,:,45),wf(:,-2),G1(:,:,:,46))
  call loop_Q_A(G1(:,:,:,46),Q(:,62),ZERO,G2(:,:,:,30))
  call check_last_QA_V(l_switch,G2(:,:,:,30),wf(:,0),G2tensor(:,26))
  call loop_VT_S(G1(:,:,:,42),Q(:,10),wf(:,-5),Q(:,32),G2(:,:,:,31))
  call loop_ST_V(G2(:,:,:,31),Q(:,42),wf(:,-4),Q(:,16),G3(:,:,:,11))
  call loop_WQ_A(G3(:,:,:,11),wf(:,-2),G3(:,:,:,12))
  call loop_Q_A(G3(:,:,:,12),Q(:,62),ZERO,G4(:,:,:,7))
  call check_last_QA_V(l_switch,G4(:,:,:,7),wf(:,0),G4tensor(:,7))
  call loop_VS_V(G1(:,:,:,42),wf(:,-5),G1(:,:,:,47))
  call loop_VS_V(G1(:,:,:,47),wf(:,-4),G1(:,:,:,48))
  call loop_WQ_A(G1(:,:,:,48),wf(:,-2),G1(:,:,:,49))
  call loop_Q_A(G1(:,:,:,49),Q(:,62),ZERO,G2(:,:,:,32))
  call check_last_QA_V(l_switch,G2(:,:,:,32),wf(:,0),G2tensor(:,27))
  call loop_VT_S(G1(:,:,:,42),Q(:,10),wf(:,-4),Q(:,16),G2(:,:,:,33))
  call loop_ST_V(G2(:,:,:,33),Q(:,26),wf(:,-5),Q(:,32),G3(:,:,:,13))
  call loop_WQ_A(G3(:,:,:,13),wf(:,-2),G3(:,:,:,14))
  call loop_Q_A(G3(:,:,:,14),Q(:,62),ZERO,G4(:,:,:,8))
  call check_last_QA_V(l_switch,G4(:,:,:,8),wf(:,0),G4tensor(:,8))
  call loop_VS_V(G1(:,:,:,42),wf(:,-4),G1(:,:,:,50))
  call loop_VS_V(G1(:,:,:,50),wf(:,-5),G1(:,:,:,51))
  call loop_WQ_A(G1(:,:,:,51),wf(:,-2),G1(:,:,:,52))
  call loop_Q_A(G1(:,:,:,52),Q(:,62),ZERO,G2(:,:,:,34))
  call check_last_QA_V(l_switch,G2(:,:,:,34),wf(:,0),G2tensor(:,28))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(6)*M(1)) * den(13)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(6)*M(1)) * den(15)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(4)*M(1))+c(5)*M(2))
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,25)
  Gcoeff = (-(c(4)*M(1))+c(5)*M(2))
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(4)*M(1))+c(5)*M(2))
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,16)
  Gcoeff = (-(c(4)*M(1))+c(5)*M(2))
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(9)*M(1)) * den(17)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(3)*M(1)) * den(18)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(12)*M(1)) * den(18)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(3)*M(1)) * den(21)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(12)*M(1)) * den(21)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(9)*M(1)) * den(23)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(3)*M(1)) * den(24)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(12)*M(1)) * den(24)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(3)*M(1)) * den(27)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(12)*M(1)) * den(27)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,24)
  Gcoeff = (-(c(7)*M(1))+c(8)*M(2)) * den(4)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,26)
  Gcoeff = (-(c(7)*M(1))+c(8)*M(2)) * den(4)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(7)*M(1))+c(8)*M(2)) * den(4)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,17)
  Gcoeff = (-(c(7)*M(1))+c(8)*M(2)) * den(4)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,7)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,27)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,2) = T4sum(1:70,2) + Gcoeff * G4tensor(:,8)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,28)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,3) = T4sum(1:70,3) + Gcoeff * G4tensor(:,1)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,3) = T4sum(1:15,3) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,4) = T4sum(1:70,4) + Gcoeff * G4tensor(:,3)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,5) = T4sum(1:70,5) + Gcoeff * G4tensor(:,5)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,18)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,6) = T4sum(1:70,6) + Gcoeff * G4tensor(:,4)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,7) = T4sum(1:70,7) + Gcoeff * G4tensor(:,6)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,19)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2))
  T4sum(1:70,8) = T4sum(1:70,8) + Gcoeff * G4tensor(:,2)
  Gcoeff = (-(c(10)*M(1))+c(11)*M(2))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,7)

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

end module ol_vamp_1_pphhjj_uxcdsxhh_1_/**/REALKIND
