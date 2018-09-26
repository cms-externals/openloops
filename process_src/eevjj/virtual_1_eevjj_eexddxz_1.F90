
module ol_vamp_1_eevjj_eexddxz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_eevjj_eexddxz_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_eevjj_eexddxz_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_eevjj_eexddxz_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_eevjj_eexddxz_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,6) :: G0
  complex(REALKIND), dimension(4,5,4,18) :: G1
  complex(REALKIND), dimension(4,15,4,17) :: G2
  complex(REALKIND), dimension(4,35,4,4) :: G3
  complex(REALKIND), dimension(5,4) :: G1tensor
  complex(REALKIND), dimension(15,12) :: G2tensor
  complex(REALKIND), dimension(35,4) :: G3tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,20),ZERO,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,1),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,23),ZERO,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,-3),G2tensor(:,1))
  call loop_QZ_A(G1(:,:,:,1),wf(:,6),G1(:,:,:,3),gZd)
  call loop_Q_A(G1(:,:,:,3),Q(:,23),ZERO,G2(:,:,:,2))
  call check_last_QA_V(l_switch,G2(:,:,:,2),wf(:,-3),G2tensor(:,2))
  call check_last_QA_V(l_switch,G1(:,:,:,1),wf(:,35),G1tensor(:,1))
  call check_last_QA_V(l_switch,G1(:,:,:,1),wf(:,36),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,9),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,24),ZERO,G1(:,:,:,4))
  call loop_AV_Q(G1(:,:,:,4),wf(:,1),G1(:,:,:,5))
  call loop_A_Q(G1(:,:,:,5),Q(:,27),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,-2),G2tensor(:,3))
  call loop_AZ_Q(G1(:,:,:,4),wf(:,6),G1(:,:,:,6),gZd)
  call loop_A_Q(G1(:,:,:,6),Q(:,27),ZERO,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,-2),G2tensor(:,4))
  call check_last_AQ_V(l_switch,G1(:,:,:,4),wf(:,38),G1tensor(:,3))
  call check_last_AQ_V(l_switch,G1(:,:,:,4),wf(:,39),G1tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,8),ZERO,G1(:,:,:,7))
  call loop_AZ_Q(G1(:,:,:,7),wf(:,-4),G1(:,:,:,8),gZd)
  call loop_A_Q(G1(:,:,:,8),Q(:,24),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,38),G2tensor(:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,39),G2tensor(:,6))
  call loop_AV_Q(G2(:,:,:,5),wf(:,1),G2(:,:,:,6))
  call loop_A_Q(G2(:,:,:,6),Q(:,27),ZERO,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,-2),G3tensor(:,1))
  call loop_AZ_Q(G2(:,:,:,5),wf(:,6),G2(:,:,:,7),gZd)
  call loop_A_Q(G2(:,:,:,7),Q(:,27),ZERO,G3(:,:,:,2))
  call check_last_AQ_V(l_switch,G3(:,:,:,2),wf(:,-2),G3tensor(:,2))
  call loop_AV_Q(G1(:,:,:,7),wf(:,1),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,11),ZERO,G2(:,:,:,8))
  call loop_AZ_Q(G2(:,:,:,8),wf(:,-4),G2(:,:,:,9),gZd)
  call loop_A_Q(G2(:,:,:,9),Q(:,27),ZERO,G3(:,:,:,3))
  call check_last_AQ_V(l_switch,G3(:,:,:,3),wf(:,-2),G3tensor(:,3))
  call loop_AZ_Q(G1(:,:,:,7),wf(:,6),G1(:,:,:,10),gZd)
  call loop_A_Q(G1(:,:,:,10),Q(:,11),ZERO,G2(:,:,:,10))
  call loop_AZ_Q(G2(:,:,:,10),wf(:,-4),G2(:,:,:,11),gZd)
  call loop_A_Q(G2(:,:,:,11),Q(:,27),ZERO,G3(:,:,:,4))
  call check_last_AQ_V(l_switch,G3(:,:,:,4),wf(:,-2),G3tensor(:,4))
  call loop_AV_Q(G1(:,:,:,7),wf(:,15),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,27),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,-2),G2tensor(:,7))
  call loop_AZ_Q(G1(:,:,:,7),wf(:,40),G1(:,:,:,12),gZd)
  call loop_A_Q(G1(:,:,:,12),Q(:,27),ZERO,G2(:,:,:,13))
  call check_last_AQ_V(l_switch,G2(:,:,:,13),wf(:,-2),G2tensor(:,8))
  call loop_AV_Q(G1(:,:,:,7),wf(:,21),G1(:,:,:,13))
  call loop_A_Q(G1(:,:,:,13),Q(:,27),ZERO,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-2),G2tensor(:,9))
  call loop_AZ_Q(G1(:,:,:,7),wf(:,41),G1(:,:,:,14),gZd)
  call loop_A_Q(G1(:,:,:,14),Q(:,27),ZERO,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-2),G2tensor(:,10))
  call loop_VA_Q(G0(:,:,:,1),wf(:,35),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,11),ZERO,G1(:,:,:,15))
  call loop_AZ_Q(G1(:,:,:,15),wf(:,-4),G1(:,:,:,16),gZd)
  call loop_A_Q(G1(:,:,:,16),Q(:,27),ZERO,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,-2),G2tensor(:,11))
  call loop_VA_Q(G0(:,:,:,1),wf(:,36),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,11),ZERO,G1(:,:,:,17))
  call loop_AZ_Q(G1(:,:,:,17),wf(:,-4),G1(:,:,:,18),gZd)
  call loop_A_Q(G1(:,:,:,18),Q(:,27),ZERO,G2(:,:,:,17))
  call check_last_AQ_V(l_switch,G2(:,:,:,17),wf(:,-2),G2tensor(:,12))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(1)*M(1))) * den(3)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(2)*M(1))) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(7)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(2)*M(1))) * den(8)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(1)*M(1))) * den(23)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(2)*M(1))) * den(25)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(1)*M(1))) * den(18)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(2)*M(1))) * den(20)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(1)*M(1))) * den(1)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,3)
  Gcoeff = (-(c(2)*M(1))) * den(4)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,4)
  Gcoeff = (-(c(1)*M(1))) * den(1)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(2)*M(1))) * den(4)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(28)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(2)*M(1))) * den(30)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(1)*M(1))) * den(31)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(2)*M(1))) * den(32)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(1)*M(1))) * den(19)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,1)
  Gcoeff = (-(c(2)*M(1))) * den(21)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(24)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,3)
  Gcoeff = (-(c(2)*M(1))) * den(26)
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

end module ol_vamp_1_eevjj_eexddxz_1_/**/REALKIND
