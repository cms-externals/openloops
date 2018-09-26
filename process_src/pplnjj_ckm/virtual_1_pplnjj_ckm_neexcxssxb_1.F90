
module ol_vamp_1_pplnjj_ckm_neexcxssxb_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pplnjj_ckm_neexcxssxb_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pplnjj_ckm_neexcxssxb_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pplnjj_ckm_neexcxssxb_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pplnjj_ckm_neexcxssxb_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,12) :: G0
  complex(REALKIND), dimension(4,5,4,38) :: G1
  complex(REALKIND), dimension(4,15,4,19) :: G2
  complex(REALKIND), dimension(4,35,4,5) :: G3
  complex(REALKIND), dimension(5,2) :: G1tensor
  complex(REALKIND), dimension(15,24) :: G2tensor
  complex(REALKIND), dimension(35,5) :: G3tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,-5),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,32),MB,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,2),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,56),MB,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,6),G2tensor(:,1))
  call loop_QW_A(G2(:,:,:,1),wf(:,3),G2(:,:,:,2))
  call loop_Q_A(G2(:,:,:,2),Q(:,59),ZERO,G3(:,:,:,1))
  call check_last_QA_V(l_switch,G3(:,:,:,1),wf(:,-2),G3tensor(:,1))
  call loop_QW_A(G1(:,:,:,1),wf(:,3),G1(:,:,:,3))
  call loop_Q_A(G1(:,:,:,3),Q(:,35),ZERO,G2(:,:,:,3))
  call check_last_QA_V(l_switch,G2(:,:,:,3),wf(:,9),G2tensor(:,2))
  call loop_QV_A(G2(:,:,:,3),wf(:,2),G2(:,:,:,4))
  call loop_Q_A(G2(:,:,:,4),Q(:,59),ZERO,G3(:,:,:,2))
  call check_last_QA_V(l_switch,G3(:,:,:,2),wf(:,-2),G3tensor(:,2))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,24),G1(:,:,:,4))
  call loop_VQ_A(G1(:,:,:,4),wf(:,-5),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,56),MB,G2(:,:,:,5))
  call check_last_QA_V(l_switch,G2(:,:,:,5),wf(:,6),G2tensor(:,3))
  call loop_QW_A(G2(:,:,:,5),wf(:,3),G2(:,:,:,6))
  call loop_Q_A(G2(:,:,:,6),Q(:,59),ZERO,G3(:,:,:,3))
  call check_last_QA_V(l_switch,G3(:,:,:,3),wf(:,-2),G3tensor(:,3))
  call loop_VQ_A(G1(:,:,:,4),wf(:,17),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,59),ZERO,G2(:,:,:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,7),wf(:,-2),G2tensor(:,4))
  call loop_VQ_A(G0(:,:,:,1),wf(:,17),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,35),ZERO,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,2),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,59),ZERO,G2(:,:,:,8))
  call check_last_QA_V(l_switch,G2(:,:,:,8),wf(:,-2),G2tensor(:,5))
  call check_last_QA_V(l_switch,G1(:,:,:,7),wf(:,9),G1tensor(:,1))
  call loop_VQ_A(G0(:,:,:,1),wf(:,19),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,56),MB,G1(:,:,:,9))
  call loop_QW_A(G1(:,:,:,9),wf(:,3),G1(:,:,:,10))
  call loop_Q_A(G1(:,:,:,10),Q(:,59),ZERO,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-2),G2tensor(:,6))
  call check_last_QA_V(l_switch,G1(:,:,:,9),wf(:,6),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-4),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,16),ZERO,G1(:,:,:,11))
  call loop_AV_Q(G1(:,:,:,11),wf(:,25),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,55),ZERO,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,-3),G2tensor(:,7))
  call loop_AQ_V(G1(:,:,:,11),wf(:,-3),G1(:,:,:,13))
  call loop_VQ_A(G1(:,:,:,13),wf(:,-5),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,56),MB,G2(:,:,:,11))
  call check_last_QA_V(l_switch,G2(:,:,:,11),wf(:,6),G2tensor(:,8))
  call loop_QW_A(G2(:,:,:,11),wf(:,3),G2(:,:,:,12))
  call loop_Q_A(G2(:,:,:,12),Q(:,59),ZERO,G3(:,:,:,4))
  call check_last_QA_V(l_switch,G3(:,:,:,4),wf(:,-2),G3tensor(:,4))
  call loop_VQ_A(G1(:,:,:,13),wf(:,17),G1(:,:,:,15))
  call loop_Q_A(G1(:,:,:,15),Q(:,59),ZERO,G2(:,:,:,13))
  call check_last_QA_V(l_switch,G2(:,:,:,13),wf(:,-2),G2tensor(:,9))
  call loop_AV_Q(G1(:,:,:,11),wf(:,26),G1(:,:,:,16))
  call loop_A_Q(G1(:,:,:,16),Q(:,55),ZERO,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-3),G2tensor(:,10))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,25),Q(:,39),G1(:,:,:,17))
  call loop_VA_Q(G1(:,:,:,17),wf(:,-4),G1(:,:,:,18))
  call loop_A_Q(G1(:,:,:,18),Q(:,55),ZERO,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-3),G2tensor(:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,39),wf(:,2),Q(:,24),G2tensor(:,12))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-3),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,8),ZERO,G1(:,:,:,19))
  call loop_QA_V(G1(:,:,:,19),wf(:,-4),G1(:,:,:,20))
  call loop_VQ_A(G1(:,:,:,20),wf(:,-5),G1(:,:,:,21))
  call loop_Q_A(G1(:,:,:,21),Q(:,56),MB,G2(:,:,:,16))
  call check_last_QA_V(l_switch,G2(:,:,:,16),wf(:,6),G2tensor(:,13))
  call loop_QW_A(G2(:,:,:,16),wf(:,3),G2(:,:,:,17))
  call loop_Q_A(G2(:,:,:,17),Q(:,59),ZERO,G3(:,:,:,5))
  call check_last_QA_V(l_switch,G3(:,:,:,5),wf(:,-2),G3tensor(:,5))
  call loop_VQ_A(G1(:,:,:,20),wf(:,17),G1(:,:,:,22))
  call loop_Q_A(G1(:,:,:,22),Q(:,59),ZERO,G2(:,:,:,18))
  call check_last_QA_V(l_switch,G2(:,:,:,18),wf(:,-2),G2tensor(:,14))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,26),Q(:,39),G1(:,:,:,23))
  call loop_VA_Q(G1(:,:,:,23),wf(:,-4),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,55),ZERO,G2(:,:,:,19))
  call check_last_AQ_V(l_switch,G2(:,:,:,19),wf(:,-3),G2tensor(:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,39),wf(:,2),Q(:,24),G2tensor(:,16))
  call loop_QV_A(G0(:,:,:,1),wf(:,25),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,39),ZERO,G1(:,:,:,25))
  call loop_QV_A(G1(:,:,:,25),wf(:,2),G1(:,:,:,26))
  call check_last_Q_A(l_switch,G1(:,:,:,26),Q(:,63),ZERO,G2tensor(:,17))
  call loop_QV_A(G0(:,:,:,1),wf(:,25),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,39),MT,G1(:,:,:,27))
  call loop_QV_A(G1(:,:,:,27),wf(:,2),G1(:,:,:,28))
  call check_last_Q_A(l_switch,G1(:,:,:,28),Q(:,63),MT,G2tensor(:,18))
  call loop_QV_A(G0(:,:,:,1),wf(:,25),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,39),MB,G1(:,:,:,29))
  call loop_QV_A(G1(:,:,:,29),wf(:,2),G1(:,:,:,30))
  call check_last_Q_A(l_switch,G1(:,:,:,30),Q(:,63),MB,G2tensor(:,19))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,25),Q(:,39),G1(:,:,:,31))
  call check_last_CV_D(l_switch,G1(:,:,:,31),Q(:,39),wf(:,2),Q(:,24),G2tensor(:,20))
  call loop_QV_A(G0(:,:,:,1),wf(:,26),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,39),ZERO,G1(:,:,:,32))
  call loop_QV_A(G1(:,:,:,32),wf(:,2),G1(:,:,:,33))
  call check_last_Q_A(l_switch,G1(:,:,:,33),Q(:,63),ZERO,G2tensor(:,21))
  call loop_QV_A(G0(:,:,:,1),wf(:,26),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,39),MT,G1(:,:,:,34))
  call loop_QV_A(G1(:,:,:,34),wf(:,2),G1(:,:,:,35))
  call check_last_Q_A(l_switch,G1(:,:,:,35),Q(:,63),MT,G2tensor(:,22))
  call loop_QV_A(G0(:,:,:,1),wf(:,26),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,39),MB,G1(:,:,:,36))
  call loop_QV_A(G1(:,:,:,36),wf(:,2),G1(:,:,:,37))
  call check_last_Q_A(l_switch,G1(:,:,:,37),Q(:,63),MB,G2tensor(:,23))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,26),Q(:,39),G1(:,:,:,38))
  call check_last_CV_D(l_switch,G1(:,:,:,38),Q(:,39),wf(:,2),Q(:,24),G2tensor(:,24))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(5)*M(1))+c(6)*M(2)) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2)) * den(5)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(5)*M(1))+c(6)*M(2)) * den(11)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2)) * den(11)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(8)*M(1)-c(12)*M(2)) * den(8)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(8)*M(1)-c(12)*M(2)) * den(14)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(5)*M(1))+c(6)*M(2)) * den(23)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(5)*M(1))+c(6)*M(2)) * den(23)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2)) * den(23)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,3)
  Gcoeff = (-(c(5)*M(1))+c(6)*M(2)) * den(21)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(1)*M(1)-c(2)*M(2)) * den(21)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(9)*M(1))+c(7)*M(2)) * den(4)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(5)*M(1))-c(11)*M(2)) * den(4)
  T2sum(1:15,10) = T2sum(1:15,10) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(5)*M(1))+c(6)*M(2)) * den(22)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(1)*M(1)-c(2)*M(2)) * den(22)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(5)*M(1))-c(11)*M(2)) * den(10)
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(9)*M(1))+c(7)*M(2)) * den(10)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(5)*M(1))-c(11)*M(2)) * den(1)
  T3sum(1:35,4) = T3sum(1:35,4) + Gcoeff * G3tensor(:,5)
  Gcoeff = (-(c(9)*M(1))+c(7)*M(2)) * den(1)
  T3sum(1:35,5) = T3sum(1:35,5) + Gcoeff * G3tensor(:,4)
  Gcoeff = (-(c(16)*M(1))+c(17)*M(2)) * den(24)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,17)
  Gcoeff = (-(c(14)*M(1))+c(15)*M(2)) * den(24)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,18)
  Gcoeff = (-(c(16)*M(1))+c(17)*M(2)) * den(24)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,17)
  Gcoeff = (-(c(14)*M(1))+c(15)*M(2)) * den(24)
  T2sum(1:15,15) = T2sum(1:15,15) + Gcoeff * G2tensor(:,19)
  Gcoeff = (-(c(10)*M(1))+c(13)*M(2)) * den(24)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(3)*M(1)-c(4)*M(2)) * den(24)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(8)*M(1)-c(12)*M(2)) * den(17)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(8)*M(1)-c(12)*M(2)) * den(18)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)
  Gcoeff = (-(c(16)*M(1))+c(17)*M(2)) * den(25)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,21)
  Gcoeff = (-(c(14)*M(1))+c(15)*M(2)) * den(25)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,22)
  Gcoeff = (-(c(16)*M(1))+c(17)*M(2)) * den(25)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,21)
  Gcoeff = (-(c(14)*M(1))+c(15)*M(2)) * den(25)
  T2sum(1:15,15) = T2sum(1:15,15) + Gcoeff * G2tensor(:,23)
  Gcoeff = (-(c(10)*M(1))+c(13)*M(2)) * den(25)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(3)*M(1)-c(4)*M(2)) * den(25)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,16)

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

end module ol_vamp_1_pplnjj_ckm_neexcxssxb_1_/**/REALKIND
