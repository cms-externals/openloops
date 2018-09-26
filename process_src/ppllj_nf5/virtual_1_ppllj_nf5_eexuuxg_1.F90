
module ol_vamp_1_ppllj_nf5_eexuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllj_nf5_eexuuxg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllj_nf5_eexuuxg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllj_nf5_eexuuxg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllj_nf5_eexuuxg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,10) :: G0
  complex(REALKIND), dimension(4,5,4,26) :: G1
  complex(REALKIND), dimension(4,15,4,32) :: G2
  complex(REALKIND), dimension(4,35,4,6) :: G3
  complex(REALKIND), dimension(5,4) :: G1tensor
  complex(REALKIND), dimension(15,12) :: G2tensor
  complex(REALKIND), dimension(35,16) :: G3tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_AV_Q(G0(:,:,:,1),wf(:,12),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,12),ZERO,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-4),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,28),ZERO,G2(:,:,:,1))
  call loop_AV_Q(G2(:,:,:,1),wf(:,1),G2(:,:,:,2))
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,31),ZERO,G3tensor(:,1))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,6),G2(:,:,:,3),gZu)
  call check_last_A_Q(l_switch,G2(:,:,:,3),Q(:,31),ZERO,G3tensor(:,2))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,6),G2(:,:,:,4),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,4),Q(:,31),ZERO,G3tensor(:,3))
  call loop_QV_A(G0(:,:,:,1),wf(:,12),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,12),ZERO,G1(:,:,:,3))
  call loop_QV_A(G1(:,:,:,3),wf(:,-4),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,28),ZERO,G2(:,:,:,5))
  call loop_QV_A(G2(:,:,:,5),wf(:,1),G2(:,:,:,6))
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,31),ZERO,G3tensor(:,4))
  call loop_QZ_A(G2(:,:,:,5),wf(:,6),G2(:,:,:,7),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,7),Q(:,31),ZERO,G3tensor(:,5))
  call loop_QZ_A(G2(:,:,:,5),wf(:,6),G2(:,:,:,8),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,8),Q(:,31),ZERO,G3tensor(:,6))
  call loop_AV_Q(G0(:,:,:,1),wf(:,12),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,12),MB,G1(:,:,:,5))
  call loop_AV_Q(G1(:,:,:,5),wf(:,-4),G1(:,:,:,6))
  call loop_A_Q(G1(:,:,:,6),Q(:,28),MB,G2(:,:,:,9))
  call loop_AV_Q(G2(:,:,:,9),wf(:,1),G2(:,:,:,10))
  call check_last_A_Q(l_switch,G2(:,:,:,10),Q(:,31),MB,G3tensor(:,7))
  call loop_AZ_Q(G2(:,:,:,9),wf(:,6),G2(:,:,:,11),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,11),Q(:,31),MB,G3tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,12),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,12),MB,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,-4),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,28),MB,G2(:,:,:,12))
  call loop_QV_A(G2(:,:,:,12),wf(:,1),G2(:,:,:,13))
  call check_last_Q_A(l_switch,G2(:,:,:,13),Q(:,31),MB,G3tensor(:,9))
  call loop_QZ_A(G2(:,:,:,12),wf(:,6),G2(:,:,:,14),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,31),MB,G3tensor(:,10))
  call loop_VQ_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,20),ZERO,G1(:,:,:,9))
  call loop_QV_A(G1(:,:,:,9),wf(:,1),G1(:,:,:,10))
  call loop_Q_A(G1(:,:,:,10),Q(:,23),ZERO,G2(:,:,:,15))
  call check_last_QA_V(l_switch,G2(:,:,:,15),wf(:,-3),G2tensor(:,1))
  call loop_QZ_A(G1(:,:,:,9),wf(:,6),G1(:,:,:,11),gZu)
  call loop_Q_A(G1(:,:,:,11),Q(:,23),ZERO,G2(:,:,:,16))
  call check_last_QA_V(l_switch,G2(:,:,:,16),wf(:,-3),G2tensor(:,2))
  call check_last_QA_V(l_switch,G1(:,:,:,9),wf(:,23),G1tensor(:,1))
  call check_last_QA_V(l_switch,G1(:,:,:,9),wf(:,24),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,9),G0(:,:,:,7))
  call loop_A_Q(G0(:,:,:,7),Q(:,24),ZERO,G1(:,:,:,12))
  call loop_AV_Q(G1(:,:,:,12),wf(:,1),G1(:,:,:,13))
  call loop_A_Q(G1(:,:,:,13),Q(:,27),ZERO,G2(:,:,:,17))
  call check_last_AQ_V(l_switch,G2(:,:,:,17),wf(:,-2),G2tensor(:,3))
  call loop_AZ_Q(G1(:,:,:,12),wf(:,6),G1(:,:,:,14),gZu)
  call loop_A_Q(G1(:,:,:,14),Q(:,27),ZERO,G2(:,:,:,18))
  call check_last_AQ_V(l_switch,G2(:,:,:,18),wf(:,-2),G2tensor(:,4))
  call check_last_AQ_V(l_switch,G1(:,:,:,12),wf(:,26),G1tensor(:,3))
  call check_last_AQ_V(l_switch,G1(:,:,:,12),wf(:,27),G1tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,8))
  call loop_A_Q(G0(:,:,:,8),Q(:,8),ZERO,G1(:,:,:,15))
  call loop_AV_Q(G1(:,:,:,15),wf(:,-4),G1(:,:,:,16))
  call loop_A_Q(G1(:,:,:,16),Q(:,24),ZERO,G2(:,:,:,19))
  call check_last_AQ_V(l_switch,G2(:,:,:,19),wf(:,26),G2tensor(:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,19),wf(:,27),G2tensor(:,6))
  call loop_AV_Q(G2(:,:,:,19),wf(:,1),G2(:,:,:,20))
  call loop_A_Q(G2(:,:,:,20),Q(:,27),ZERO,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,-2),G3tensor(:,11))
  call loop_AZ_Q(G2(:,:,:,19),wf(:,6),G2(:,:,:,21),gZu)
  call loop_A_Q(G2(:,:,:,21),Q(:,27),ZERO,G3(:,:,:,2))
  call check_last_AQ_V(l_switch,G3(:,:,:,2),wf(:,-2),G3tensor(:,12))
  call loop_AV_Q(G1(:,:,:,15),wf(:,1),G1(:,:,:,17))
  call loop_A_Q(G1(:,:,:,17),Q(:,11),ZERO,G2(:,:,:,22))
  call loop_AV_Q(G2(:,:,:,22),wf(:,-4),G2(:,:,:,23))
  call loop_A_Q(G2(:,:,:,23),Q(:,27),ZERO,G3(:,:,:,3))
  call check_last_AQ_V(l_switch,G3(:,:,:,3),wf(:,-2),G3tensor(:,13))
  call loop_AZ_Q(G1(:,:,:,15),wf(:,6),G1(:,:,:,18),gZu)
  call loop_A_Q(G1(:,:,:,18),Q(:,11),ZERO,G2(:,:,:,24))
  call loop_AV_Q(G2(:,:,:,24),wf(:,-4),G2(:,:,:,25))
  call loop_A_Q(G2(:,:,:,25),Q(:,27),ZERO,G3(:,:,:,4))
  call check_last_AQ_V(l_switch,G3(:,:,:,4),wf(:,-2),G3tensor(:,14))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-4),Q(:,16),G1(:,:,:,19))
  call loop_VA_Q(G1(:,:,:,19),wf(:,-3),G1(:,:,:,20))
  call loop_A_Q(G1(:,:,:,20),Q(:,24),ZERO,G2(:,:,:,26))
  call check_last_AQ_V(l_switch,G2(:,:,:,26),wf(:,26),G2tensor(:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,26),wf(:,27),G2tensor(:,8))
  call loop_AV_Q(G2(:,:,:,26),wf(:,1),G2(:,:,:,27))
  call loop_A_Q(G2(:,:,:,27),Q(:,27),ZERO,G3(:,:,:,5))
  call check_last_AQ_V(l_switch,G3(:,:,:,5),wf(:,-2),G3tensor(:,15))
  call loop_AZ_Q(G2(:,:,:,26),wf(:,6),G2(:,:,:,28),gZu)
  call loop_A_Q(G2(:,:,:,28),Q(:,27),ZERO,G3(:,:,:,6))
  call check_last_AQ_V(l_switch,G3(:,:,:,6),wf(:,-2),G3tensor(:,16))
  call loop_VA_Q(G1(:,:,:,19),wf(:,23),G1(:,:,:,21))
  call loop_A_Q(G1(:,:,:,21),Q(:,27),ZERO,G2(:,:,:,29))
  call check_last_AQ_V(l_switch,G2(:,:,:,29),wf(:,-2),G2tensor(:,9))
  call loop_VA_Q(G1(:,:,:,19),wf(:,24),G1(:,:,:,22))
  call loop_A_Q(G1(:,:,:,22),Q(:,27),ZERO,G2(:,:,:,30))
  call check_last_AQ_V(l_switch,G2(:,:,:,30),wf(:,-2),G2tensor(:,10))
  call loop_VA_Q(G0(:,:,:,1),wf(:,23),G0(:,:,:,9))
  call loop_A_Q(G0(:,:,:,9),Q(:,11),ZERO,G1(:,:,:,23))
  call loop_AV_Q(G1(:,:,:,23),wf(:,-4),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,27),ZERO,G2(:,:,:,31))
  call check_last_AQ_V(l_switch,G2(:,:,:,31),wf(:,-2),G2tensor(:,11))
  call loop_VA_Q(G0(:,:,:,1),wf(:,24),G0(:,:,:,10))
  call loop_A_Q(G0(:,:,:,10),Q(:,11),ZERO,G1(:,:,:,25))
  call loop_AV_Q(G1(:,:,:,25),wf(:,-4),G1(:,:,:,26))
  call loop_A_Q(G1(:,:,:,26),Q(:,27),ZERO,G2(:,:,:,32))
  call check_last_AQ_V(l_switch,G2(:,:,:,32),wf(:,-2),G2tensor(:,12))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(10)*M(1)) * den(21)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(10)*M(1)) * den(21)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,4)
  Gcoeff = (-(c(8)*M(1))) * den(21)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(7)*M(1))) * den(21)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,7)
  Gcoeff = (-(c(8)*M(1))) * den(21)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,4)
  Gcoeff = (-(c(7)*M(1))) * den(21)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,9)
  Gcoeff = (-(c(11)*M(1))) * den(10)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(11)*M(1))) * den(10)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,5)
  Gcoeff = (-(c(11)*M(1))) * den(10)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,3)
  Gcoeff = (-(c(9)*M(1))) * den(10)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,8)
  Gcoeff = (-(c(11)*M(1))) * den(10)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,6)
  Gcoeff = (-(c(9)*M(1))) * den(10)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,10)
  Gcoeff = (-(c(4)*M(1))) * den(3)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(6)*M(1)) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(4)*M(1))) * den(7)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(6)*M(1)) * den(8)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(3)*M(1)) * den(17)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(5)*M(1))) * den(19)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(1)*M(1))) * den(17)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(2)*M(1)) * den(19)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(3)*M(1)) * den(12)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(5)*M(1))) * den(14)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(1)*M(1))) * den(12)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(2)*M(1)) * den(14)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(3)*M(1)) * den(1)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,13)
  Gcoeff = (-(c(5)*M(1))) * den(4)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(3)*M(1)) * den(1)
  T3sum(1:35,4) = T3sum(1:35,4) + Gcoeff * G3tensor(:,11)
  Gcoeff = (-(c(5)*M(1))) * den(4)
  T3sum(1:35,4) = T3sum(1:35,4) + Gcoeff * G3tensor(:,12)
  Gcoeff = (-(c(1)*M(1))) * den(1)
  T3sum(1:35,5) = T3sum(1:35,5) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(2)*M(1)) * den(4)
  T3sum(1:35,5) = T3sum(1:35,5) + Gcoeff * G3tensor(:,16)
  Gcoeff = (-(c(4)*M(1))) * den(13)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(6)*M(1)) * den(15)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(4)*M(1))) * den(18)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(6)*M(1)) * den(20)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,4)

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

end module ol_vamp_1_ppllj_nf5_eexuuxg_1_/**/REALKIND
