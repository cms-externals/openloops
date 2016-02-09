
module ol_vamp_2_pphllj_eexbbxhg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_2(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphllj_eexbbxhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphllj_eexbbxhg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphllj_eexbbxhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphllj_eexbbxhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,18) :: G0
  complex(REALKIND), dimension(4,5,4,34) :: G1
  complex(REALKIND), dimension(4,15,4,22) :: G2
  complex(REALKIND), dimension(15,12) :: G2tensor
  complex(REALKIND), dimension(35,5) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_AZ_Q(G0(:,:,:,1),wf(:,56),G0(:,:,:,2),gZu)
  call loop_A_Q(G0(:,:,:,2),Q(:,19),MT,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-5),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,51),MT,G2(:,:,:,1))
  call loop_AV_Q(G2(:,:,:,1),wf(:,57),G2(:,:,:,2))
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,63),MT,G3tensor(:,1))
  call loop_QZ_A(G0(:,:,:,1),wf(:,56),G0(:,:,:,3),gZd)
  call loop_Q_A(G0(:,:,:,3),Q(:,19),ZERO,G1(:,:,:,3))
  call loop_QV_A(G1(:,:,:,3),wf(:,-5),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,51),ZERO,G2(:,:,:,3))
  call loop_QV_A(G2(:,:,:,3),wf(:,57),G2(:,:,:,4))
  call check_last_Q_A(l_switch,G2(:,:,:,4),Q(:,63),ZERO,G3tensor(:,2))
  call loop_QZ_A(G0(:,:,:,1),wf(:,56),G0(:,:,:,4),gZd)
  call loop_Q_A(G0(:,:,:,4),Q(:,19),MB,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,-5),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,51),MB,G2(:,:,:,5))
  call loop_QV_A(G2(:,:,:,5),wf(:,57),G2(:,:,:,6))
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,63),MB,G3tensor(:,3))
  call loop_AZ_Q(G0(:,:,:,1),wf(:,56),G0(:,:,:,5),gZd)
  call loop_A_Q(G0(:,:,:,5),Q(:,19),ZERO,G1(:,:,:,7))
  call loop_AV_Q(G1(:,:,:,7),wf(:,-5),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,51),ZERO,G2(:,:,:,7))
  call loop_AV_Q(G2(:,:,:,7),wf(:,57),G2(:,:,:,8))
  call check_last_A_Q(l_switch,G2(:,:,:,8),Q(:,63),ZERO,G3tensor(:,4))
  call loop_AZ_Q(G0(:,:,:,1),wf(:,56),G0(:,:,:,6),gZd)
  call loop_A_Q(G0(:,:,:,6),Q(:,19),MB,G1(:,:,:,9))
  call loop_AV_Q(G1(:,:,:,9),wf(:,-5),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,51),MB,G2(:,:,:,9))
  call loop_AV_Q(G2(:,:,:,9),wf(:,57),G2(:,:,:,10))
  call check_last_A_Q(l_switch,G2(:,:,:,10),Q(:,63),MB,G3tensor(:,5))
  call loop_VQ_A(G0(:,:,:,1),wf(:,203),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,23),MB,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,-5),G1(:,:,:,12))
  call loop_Q_A(G1(:,:,:,12),Q(:,55),MB,G2(:,:,:,11))
  call check_last_QA_V(l_switch,G2(:,:,:,11),wf(:,-3),G2tensor(:,1))
  call loop_VQ_A(G0(:,:,:,1),wf(:,205),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,23),MB,G1(:,:,:,13))
  call loop_QV_A(G1(:,:,:,13),wf(:,-5),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,55),MB,G2(:,:,:,12))
  call check_last_QA_V(l_switch,G2(:,:,:,12),wf(:,-3),G2tensor(:,2))
  call loop_VQ_A(G0(:,:,:,1),wf(:,207),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,23),MB,G1(:,:,:,15))
  call loop_QV_A(G1(:,:,:,15),wf(:,-5),G1(:,:,:,16))
  call loop_Q_A(G1(:,:,:,16),Q(:,55),MB,G2(:,:,:,13))
  call check_last_QA_V(l_switch,G2(:,:,:,13),wf(:,-3),G2tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,209),G0(:,:,:,10))
  call loop_A_Q(G0(:,:,:,10),Q(:,27),MB,G1(:,:,:,17))
  call loop_AV_Q(G1(:,:,:,17),wf(:,-5),G1(:,:,:,18))
  call loop_A_Q(G1(:,:,:,18),Q(:,59),MB,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-2),G2tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,211),G0(:,:,:,11))
  call loop_A_Q(G0(:,:,:,11),Q(:,27),MB,G1(:,:,:,19))
  call loop_AV_Q(G1(:,:,:,19),wf(:,-5),G1(:,:,:,20))
  call loop_A_Q(G1(:,:,:,20),Q(:,59),MB,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-2),G2tensor(:,5))
  call loop_VA_Q(G0(:,:,:,1),wf(:,213),G0(:,:,:,12))
  call loop_A_Q(G0(:,:,:,12),Q(:,27),MB,G1(:,:,:,21))
  call loop_AV_Q(G1(:,:,:,21),wf(:,-5),G1(:,:,:,22))
  call loop_A_Q(G1(:,:,:,22),Q(:,59),MB,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,-2),G2tensor(:,6))
  call loop_VQ_A(G0(:,:,:,1),wf(:,215),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,23),MB,G1(:,:,:,23))
  call loop_QV_A(G1(:,:,:,23),wf(:,-5),G1(:,:,:,24))
  call loop_Q_A(G1(:,:,:,24),Q(:,55),MB,G2(:,:,:,17))
  call check_last_QA_V(l_switch,G2(:,:,:,17),wf(:,-3),G2tensor(:,7))
  call loop_VQ_A(G0(:,:,:,1),wf(:,217),G0(:,:,:,14))
  call loop_Q_A(G0(:,:,:,14),Q(:,23),MB,G1(:,:,:,25))
  call loop_QV_A(G1(:,:,:,25),wf(:,-5),G1(:,:,:,26))
  call loop_Q_A(G1(:,:,:,26),Q(:,55),MB,G2(:,:,:,18))
  call check_last_QA_V(l_switch,G2(:,:,:,18),wf(:,-3),G2tensor(:,8))
  call loop_VQ_A(G0(:,:,:,1),wf(:,219),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,23),MB,G1(:,:,:,27))
  call loop_QV_A(G1(:,:,:,27),wf(:,-5),G1(:,:,:,28))
  call loop_Q_A(G1(:,:,:,28),Q(:,55),MB,G2(:,:,:,19))
  call check_last_QA_V(l_switch,G2(:,:,:,19),wf(:,-3),G2tensor(:,9))
  call loop_VA_Q(G0(:,:,:,1),wf(:,221),G0(:,:,:,16))
  call loop_A_Q(G0(:,:,:,16),Q(:,27),MB,G1(:,:,:,29))
  call loop_AV_Q(G1(:,:,:,29),wf(:,-5),G1(:,:,:,30))
  call loop_A_Q(G1(:,:,:,30),Q(:,59),MB,G2(:,:,:,20))
  call check_last_AQ_V(l_switch,G2(:,:,:,20),wf(:,-2),G2tensor(:,10))
  call loop_VA_Q(G0(:,:,:,1),wf(:,223),G0(:,:,:,17))
  call loop_A_Q(G0(:,:,:,17),Q(:,27),MB,G1(:,:,:,31))
  call loop_AV_Q(G1(:,:,:,31),wf(:,-5),G1(:,:,:,32))
  call loop_A_Q(G1(:,:,:,32),Q(:,59),MB,G2(:,:,:,21))
  call check_last_AQ_V(l_switch,G2(:,:,:,21),wf(:,-2),G2tensor(:,11))
  call loop_VA_Q(G0(:,:,:,1),wf(:,225),G0(:,:,:,18))
  call loop_A_Q(G0(:,:,:,18),Q(:,27),MB,G1(:,:,:,33))
  call loop_AV_Q(G1(:,:,:,33),wf(:,-5),G1(:,:,:,34))
  call loop_A_Q(G1(:,:,:,34),Q(:,59),MB,G2(:,:,:,22))
  call check_last_AQ_V(l_switch,G2(:,:,:,22),wf(:,-2),G2tensor(:,12))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(25)*M(1))) * den(119)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(27)*M(1))) * den(119)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(25)*M(1))) * den(119)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,3)
  Gcoeff = (-(c(27)*M(1))) * den(119)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,4)
  Gcoeff = (-(c(25)*M(1))) * den(119)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(37)*M(1)) * den(221)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(19)*M(1))) * den(222)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(21)*M(1))) * den(223)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(37)*M(1)) * den(224)
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(19)*M(1))) * den(225)
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(21)*M(1))) * den(226)
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(37)*M(1)) * den(227)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(19)*M(1))) * den(228)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(21)*M(1))) * den(229)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(37)*M(1)) * den(230)
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(19)*M(1))) * den(231)
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(21)*M(1))) * den(232)
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,12)

end subroutine vamp_2

end module ol_vamp_2_pphllj_eexbbxhg_1_/**/REALKIND
