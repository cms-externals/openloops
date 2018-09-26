
module ol_vamp_2_ppllnnjj_vbs_nexnmxemucdxsx_2_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_2(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllnnjj_vbs_nexnmxemucdxsx_2.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllnnjj_vbs_nexnmxemucdxsx_2.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllnnjj_vbs_nexnmxemucdxsx_2_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllnnjj_vbs_nexnmxemucdxsx_2_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,13) :: G0
  complex(REALKIND), dimension(4,5,4,24) :: G1
  complex(REALKIND), dimension(4,15,4,12) :: G2
  complex(REALKIND), dimension(5,12) :: G1tensor
  complex(REALKIND), dimension(15,12) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,480),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,234),ZERO,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,5),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,239),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-4),G2tensor(:,1))
  call check_last_AQ_V(l_switch,G1(:,:,:,1),wf(:,41),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,482),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,234),ZERO,G1(:,:,:,3))
  call loop_AW_Q(G1(:,:,:,3),wf(:,5),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,239),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,-4),G2tensor(:,2))
  call check_last_AQ_V(l_switch,G1(:,:,:,3),wf(:,41),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,488),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,234),ZERO,G1(:,:,:,5))
  call loop_AW_Q(G1(:,:,:,5),wf(:,5),G1(:,:,:,6))
  call loop_A_Q(G1(:,:,:,6),Q(:,239),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,-4),G2tensor(:,3))
  call check_last_AQ_V(l_switch,G1(:,:,:,5),wf(:,41),G1tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,563),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,213),ZERO,G1(:,:,:,7))
  call loop_AW_Q(G1(:,:,:,7),wf(:,6),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,223),ZERO,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,-5),G2tensor(:,4))
  call check_last_AQ_V(l_switch,G1(:,:,:,7),wf(:,30),G1tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,565),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,213),ZERO,G1(:,:,:,9))
  call loop_AW_Q(G1(:,:,:,9),wf(:,6),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,223),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,-5),G2tensor(:,5))
  call check_last_AQ_V(l_switch,G1(:,:,:,9),wf(:,30),G1tensor(:,5))
  call loop_VA_Q(G0(:,:,:,1),wf(:,573),G0(:,:,:,7))
  call loop_A_Q(G0(:,:,:,7),Q(:,213),ZERO,G1(:,:,:,11))
  call loop_AW_Q(G1(:,:,:,11),wf(:,6),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,223),ZERO,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,-5),G2tensor(:,6))
  call check_last_AQ_V(l_switch,G1(:,:,:,11),wf(:,30),G1tensor(:,6))
  call loop_VQ_A(G0(:,:,:,1),wf(:,596),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,181),ZERO,G1(:,:,:,13))
  call loop_QW_A(G1(:,:,:,13),wf(:,6),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,191),ZERO,G2(:,:,:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,7),wf(:,-6),G2tensor(:,7))
  call check_last_QA_V(l_switch,G1(:,:,:,13),wf(:,42),G1tensor(:,7))
  call loop_VQ_A(G0(:,:,:,1),wf(:,598),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,181),ZERO,G1(:,:,:,15))
  call loop_QW_A(G1(:,:,:,15),wf(:,6),G1(:,:,:,16))
  call loop_Q_A(G1(:,:,:,16),Q(:,191),ZERO,G2(:,:,:,8))
  call check_last_QA_V(l_switch,G2(:,:,:,8),wf(:,-6),G2tensor(:,8))
  call check_last_QA_V(l_switch,G1(:,:,:,15),wf(:,42),G1tensor(:,8))
  call loop_VQ_A(G0(:,:,:,1),wf(:,600),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,181),ZERO,G1(:,:,:,17))
  call loop_QW_A(G1(:,:,:,17),wf(:,6),G1(:,:,:,18))
  call loop_Q_A(G1(:,:,:,18),Q(:,191),ZERO,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-6),G2tensor(:,9))
  call check_last_QA_V(l_switch,G1(:,:,:,17),wf(:,42),G1tensor(:,9))
  call loop_VA_Q(G0(:,:,:,1),wf(:,602),G0(:,:,:,11))
  call loop_A_Q(G0(:,:,:,11),Q(:,229),ZERO,G1(:,:,:,19))
  call loop_AW_Q(G1(:,:,:,19),wf(:,6),G1(:,:,:,20))
  call loop_A_Q(G1(:,:,:,20),Q(:,239),ZERO,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,-4),G2tensor(:,10))
  call check_last_AQ_V(l_switch,G1(:,:,:,19),wf(:,49),G1tensor(:,10))
  call loop_VA_Q(G0(:,:,:,1),wf(:,604),G0(:,:,:,12))
  call loop_A_Q(G0(:,:,:,12),Q(:,229),ZERO,G1(:,:,:,21))
  call loop_AW_Q(G1(:,:,:,21),wf(:,6),G1(:,:,:,22))
  call loop_A_Q(G1(:,:,:,22),Q(:,239),ZERO,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,-4),G2tensor(:,11))
  call check_last_AQ_V(l_switch,G1(:,:,:,21),wf(:,49),G1tensor(:,11))
  call loop_VA_Q(G0(:,:,:,1),wf(:,610),G0(:,:,:,13))
  call loop_A_Q(G0(:,:,:,13),Q(:,229),ZERO,G1(:,:,:,23))
  call loop_AW_Q(G1(:,:,:,23),wf(:,6),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,239),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,-4),G2tensor(:,12))
  call check_last_AQ_V(l_switch,G1(:,:,:,23),wf(:,49),G1tensor(:,12))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(8)*M(1)) * den(701)
  T2sum(1:15,22) = T2sum(1:15,22) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(11)*M(1)) * den(702)
  T2sum(1:15,22) = T2sum(1:15,22) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(11)*M(1)) * den(703)
  T2sum(1:15,22) = T2sum(1:15,22) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(8)*M(1)) * den(706)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(11)*M(1)) * den(707)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(11)*M(1)) * den(709)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(11)*M(1)) * den(710)
  T2sum(1:15,15) = T2sum(1:15,15) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(10)*M(1))) * den(711)
  T2sum(1:15,15) = T2sum(1:15,15) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(11)*M(1)) * den(712)
  T2sum(1:15,15) = T2sum(1:15,15) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(8)*M(1)) * den(713)
  T2sum(1:15,18) = T2sum(1:15,18) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(11)*M(1)) * den(714)
  T2sum(1:15,18) = T2sum(1:15,18) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(11)*M(1)) * den(715)
  T2sum(1:15,18) = T2sum(1:15,18) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(8)*M(1)) * den(754)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(11)*M(1)) * den(755)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(11)*M(1)) * den(756)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(8)*M(1)) * den(760)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(11)*M(1)) * den(761)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(11)*M(1)) * den(765)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(11)*M(1)) * den(766)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(8)*M(1)) * den(767)
  T1sum(1:5,8) = T1sum(1:5,8) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(11)*M(1)) * den(768)
  T1sum(1:5,8) = T1sum(1:5,8) + Gcoeff * G1tensor(:,11)
  Gcoeff = (-(c(10)*M(1))) * den(769)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(11)*M(1)) * den(770)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(11)*M(1)) * den(771)
  T1sum(1:5,8) = T1sum(1:5,8) + Gcoeff * G1tensor(:,12)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_2

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_2_ppllnnjj_vbs_nexnmxemucdxsx_2_/**/REALKIND
