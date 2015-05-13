
module ol_vamp_2_ppllllj_nenenexnexbbxg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_2(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj_nenenexnexbbxg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj_nenenexnexbbxg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj_nenenexnexbbxg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj_nenenexnexbbxg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,16) :: G0
  complex(REALKIND), dimension(4,5,4,30) :: G1
  complex(REALKIND), dimension(4,15,4,23) :: G2
  complex(REALKIND), dimension(5,1) :: G1tensor
  complex(REALKIND), dimension(15,9) :: G2tensor
  complex(REALKIND), dimension(35,8) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_AV_Q(G0(:,:,:,1),wf(:,136),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,54),ZERO,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-6),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,118),ZERO,G2(:,:,:,1))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,38),G2(:,:,:,2),gZu)
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,127),ZERO,G3tensor(:,1))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,38),G2(:,:,:,3),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,3),Q(:,127),ZERO,G3tensor(:,2))
  call loop_AV_Q(G0(:,:,:,1),wf(:,136),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,54),MT,G1(:,:,:,3))
  call loop_AV_Q(G1(:,:,:,3),wf(:,-6),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,118),MT,G2(:,:,:,4))
  call loop_AZ_Q(G2(:,:,:,4),wf(:,38),G2(:,:,:,5),gZu)
  call check_last_A_Q(l_switch,G2(:,:,:,5),Q(:,127),MT,G3tensor(:,3))
  call loop_QV_A(G0(:,:,:,1),wf(:,136),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,54),ZERO,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,-6),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,118),ZERO,G2(:,:,:,6))
  call loop_QZ_A(G2(:,:,:,6),wf(:,38),G2(:,:,:,7),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,7),Q(:,127),ZERO,G3tensor(:,4))
  call loop_QZ_A(G2(:,:,:,6),wf(:,38),G2(:,:,:,8),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,8),Q(:,127),ZERO,G3tensor(:,5))
  call loop_QV_A(G0(:,:,:,1),wf(:,136),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,54),MT,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,-6),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,118),MT,G2(:,:,:,9))
  call loop_QZ_A(G2(:,:,:,9),wf(:,38),G2(:,:,:,10),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,10),Q(:,127),MT,G3tensor(:,6))
  call loop_AV_Q(G0(:,:,:,1),wf(:,136),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,54),MB,G1(:,:,:,9))
  call loop_AV_Q(G1(:,:,:,9),wf(:,-6),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,118),MB,G2(:,:,:,11))
  call loop_AZ_Q(G2(:,:,:,11),wf(:,38),G2(:,:,:,12),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,12),Q(:,127),MB,G3tensor(:,7))
  call loop_QV_A(G0(:,:,:,1),wf(:,136),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,54),MB,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,-6),G1(:,:,:,12))
  call loop_Q_A(G1(:,:,:,12),Q(:,118),MB,G2(:,:,:,13))
  call loop_QZ_A(G2(:,:,:,13),wf(:,38),G2(:,:,:,14),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,127),MB,G3tensor(:,8))
  call loop_VA_Q(G0(:,:,:,1),wf(:,188),G0(:,:,:,8))
  call loop_A_Q(G0(:,:,:,8),Q(:,102),MB,G1(:,:,:,13))
  call loop_AZ_Q(G1(:,:,:,13),wf(:,38),G1(:,:,:,14),gZd)
  call loop_A_Q(G1(:,:,:,14),Q(:,111),MB,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-4),G2tensor(:,1))
  call check_last_AQ_V(l_switch,G1(:,:,:,13),wf(:,49),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,226),G0(:,:,:,9))
  call loop_A_Q(G0(:,:,:,9),Q(:,47),MB,G1(:,:,:,15))
  call loop_AV_Q(G1(:,:,:,15),wf(:,-6),G1(:,:,:,16))
  call loop_A_Q(G1(:,:,:,16),Q(:,111),MB,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,-4),G2tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,230),G0(:,:,:,10))
  call loop_A_Q(G0(:,:,:,10),Q(:,47),MB,G1(:,:,:,17))
  call loop_AV_Q(G1(:,:,:,17),wf(:,-6),G1(:,:,:,18))
  call loop_A_Q(G1(:,:,:,18),Q(:,111),MB,G2(:,:,:,17))
  call check_last_AQ_V(l_switch,G2(:,:,:,17),wf(:,-4),G2tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,234),G0(:,:,:,11))
  call loop_A_Q(G0(:,:,:,11),Q(:,47),MB,G1(:,:,:,19))
  call loop_AV_Q(G1(:,:,:,19),wf(:,-6),G1(:,:,:,20))
  call loop_A_Q(G1(:,:,:,20),Q(:,111),MB,G2(:,:,:,18))
  call check_last_AQ_V(l_switch,G2(:,:,:,18),wf(:,-4),G2tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,238),G0(:,:,:,12))
  call loop_A_Q(G0(:,:,:,12),Q(:,47),MB,G1(:,:,:,21))
  call loop_AV_Q(G1(:,:,:,21),wf(:,-6),G1(:,:,:,22))
  call loop_A_Q(G1(:,:,:,22),Q(:,111),MB,G2(:,:,:,19))
  call check_last_AQ_V(l_switch,G2(:,:,:,19),wf(:,-4),G2tensor(:,5))
  call loop_VA_Q(G0(:,:,:,1),wf(:,242),G0(:,:,:,13))
  call loop_A_Q(G0(:,:,:,13),Q(:,47),MB,G1(:,:,:,23))
  call loop_AV_Q(G1(:,:,:,23),wf(:,-6),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,111),MB,G2(:,:,:,20))
  call check_last_AQ_V(l_switch,G2(:,:,:,20),wf(:,-4),G2tensor(:,6))
  call loop_VA_Q(G0(:,:,:,1),wf(:,246),G0(:,:,:,14))
  call loop_A_Q(G0(:,:,:,14),Q(:,47),MB,G1(:,:,:,25))
  call loop_AV_Q(G1(:,:,:,25),wf(:,-6),G1(:,:,:,26))
  call loop_A_Q(G1(:,:,:,26),Q(:,111),MB,G2(:,:,:,21))
  call check_last_AQ_V(l_switch,G2(:,:,:,21),wf(:,-4),G2tensor(:,7))
  call loop_VA_Q(G0(:,:,:,1),wf(:,250),G0(:,:,:,15))
  call loop_A_Q(G0(:,:,:,15),Q(:,47),MB,G1(:,:,:,27))
  call loop_AV_Q(G1(:,:,:,27),wf(:,-6),G1(:,:,:,28))
  call loop_A_Q(G1(:,:,:,28),Q(:,111),MB,G2(:,:,:,22))
  call check_last_AQ_V(l_switch,G2(:,:,:,22),wf(:,-4),G2tensor(:,8))
  call loop_VA_Q(G0(:,:,:,1),wf(:,254),G0(:,:,:,16))
  call loop_A_Q(G0(:,:,:,16),Q(:,47),MB,G1(:,:,:,29))
  call loop_AV_Q(G1(:,:,:,29),wf(:,-6),G1(:,:,:,30))
  call loop_A_Q(G1(:,:,:,30),Q(:,111),MB,G2(:,:,:,23))
  call check_last_AQ_V(l_switch,G2(:,:,:,23),wf(:,-4),G2tensor(:,9))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(8)*M(1))) * den(299)
  T3sum(1:35,42) = T3sum(1:35,42) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(7)*M(1))) * den(299)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,3)
  Gcoeff = (-(c(8)*M(1))) * den(299)
  T3sum(1:35,42) = T3sum(1:35,42) + Gcoeff * G3tensor(:,4)
  Gcoeff = (-(c(7)*M(1))) * den(299)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,6)
  Gcoeff = (-(c(8)*M(1))) * den(299)
  T3sum(1:35,42) = T3sum(1:35,42) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(7)*M(1))) * den(299)
  T3sum(1:35,44) = T3sum(1:35,44) + Gcoeff * G3tensor(:,7)
  Gcoeff = (-(c(8)*M(1))) * den(299)
  T3sum(1:35,42) = T3sum(1:35,42) + Gcoeff * G3tensor(:,5)
  Gcoeff = (-(c(7)*M(1))) * den(299)
  T3sum(1:35,44) = T3sum(1:35,44) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(3)*M(1)) * den(300)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(2)*M(1))) * den(250)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(5)*M(1)) * den(252)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(2)*M(1))) * den(254)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(2)*M(1))) * den(256)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(2)*M(1))) * den(258)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(2)*M(1))) * den(260)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(2)*M(1)) * den(262)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(2)*M(1)) * den(264)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(3)*M(1)) * den(190)
  T1sum(1:5,9) = T1sum(1:5,9) + Gcoeff * G1tensor(:,1)

end subroutine vamp_2

end module ol_vamp_2_ppllllj_nenenexnexbbxg_1_/**/REALKIND
