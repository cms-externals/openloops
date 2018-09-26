
module ol_vamp_4_heftpphjjj_dddxdxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_4(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_heftpphjjj_dddxdxhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_heftpphjjj_dddxdxhg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_heftpphjjj_dddxdxhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_heftpphjjj_dddxdxhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(4)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,26) :: G0
  complex(REALKIND), dimension(4,5,4,63) :: G1
  complex(REALKIND), dimension(15,38) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,379),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,57),MB,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,5),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),MB,G2tensor(:,1))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,379),Q(:,57),G1(:,:,:,3))
  call check_last_CV_D(l_switch,G1(:,:,:,3),Q(:,57),wf(:,5),Q(:,6),G2tensor(:,2))
  call loop_QV_A(G0(:,:,:,1),wf(:,39),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,38),ZERO,G1(:,:,:,4))
  call loop_QV_A(G1(:,:,:,4),wf(:,26),G1(:,:,:,5))
  call check_last_Q_A(l_switch,G1(:,:,:,5),Q(:,63),ZERO,G2tensor(:,3))
  call loop_QV_A(G0(:,:,:,1),wf(:,39),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,38),MB,G1(:,:,:,6))
  call loop_QV_A(G1(:,:,:,6),wf(:,26),G1(:,:,:,7))
  call check_last_Q_A(l_switch,G1(:,:,:,7),Q(:,63),MB,G2tensor(:,4))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,39),Q(:,38),G1(:,:,:,8))
  call check_last_CV_D(l_switch,G1(:,:,:,8),Q(:,38),wf(:,26),Q(:,25),G2tensor(:,5))
  call loop_QV_A(G0(:,:,:,1),wf(:,352),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,54),ZERO,G1(:,:,:,9))
  call loop_QV_A(G1(:,:,:,9),wf(:,4),G1(:,:,:,10))
  call check_last_Q_A(l_switch,G1(:,:,:,10),Q(:,63),ZERO,G2tensor(:,6))
  call loop_QV_A(G0(:,:,:,1),wf(:,352),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,54),MB,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,4),G1(:,:,:,12))
  call check_last_Q_A(l_switch,G1(:,:,:,12),Q(:,63),MB,G2tensor(:,7))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,352),Q(:,54),G1(:,:,:,13))
  call check_last_CV_D(l_switch,G1(:,:,:,13),Q(:,54),wf(:,4),Q(:,9),G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,42),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,38),ZERO,G1(:,:,:,14))
  call loop_QV_A(G1(:,:,:,14),wf(:,26),G1(:,:,:,15))
  call check_last_Q_A(l_switch,G1(:,:,:,15),Q(:,63),ZERO,G2tensor(:,9))
  call loop_QV_A(G0(:,:,:,1),wf(:,42),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,38),MB,G1(:,:,:,16))
  call loop_QV_A(G1(:,:,:,16),wf(:,26),G1(:,:,:,17))
  call check_last_Q_A(l_switch,G1(:,:,:,17),Q(:,63),MB,G2tensor(:,10))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,42),Q(:,38),G1(:,:,:,18))
  call check_last_CV_D(l_switch,G1(:,:,:,18),Q(:,38),wf(:,26),Q(:,25),G2tensor(:,11))
  call loop_QV_A(G0(:,:,:,1),wf(:,365),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,54),ZERO,G1(:,:,:,19))
  call loop_QV_A(G1(:,:,:,19),wf(:,4),G1(:,:,:,20))
  call check_last_Q_A(l_switch,G1(:,:,:,20),Q(:,63),ZERO,G2tensor(:,12))
  call loop_QV_A(G0(:,:,:,1),wf(:,365),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,54),MB,G1(:,:,:,21))
  call loop_QV_A(G1(:,:,:,21),wf(:,4),G1(:,:,:,22))
  call check_last_Q_A(l_switch,G1(:,:,:,22),Q(:,63),MB,G2tensor(:,13))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,365),Q(:,54),G1(:,:,:,23))
  call check_last_CV_D(l_switch,G1(:,:,:,23),Q(:,54),wf(:,4),Q(:,9),G2tensor(:,14))
  call loop_QV_A(G0(:,:,:,1),wf(:,380),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,54),ZERO,G1(:,:,:,24))
  call loop_QV_A(G1(:,:,:,24),wf(:,4),G1(:,:,:,25))
  call check_last_Q_A(l_switch,G1(:,:,:,25),Q(:,63),ZERO,G2tensor(:,15))
  call loop_QV_A(G0(:,:,:,1),wf(:,380),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,54),MB,G1(:,:,:,26))
  call loop_QV_A(G1(:,:,:,26),wf(:,4),G1(:,:,:,27))
  call check_last_Q_A(l_switch,G1(:,:,:,27),Q(:,63),MB,G2tensor(:,16))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,380),Q(:,54),G1(:,:,:,28))
  call check_last_CV_D(l_switch,G1(:,:,:,28),Q(:,54),wf(:,4),Q(:,9),G2tensor(:,17))
  call loop_QV_A(G0(:,:,:,1),wf(:,381),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,54),ZERO,G1(:,:,:,29))
  call loop_QV_A(G1(:,:,:,29),wf(:,4),G1(:,:,:,30))
  call check_last_Q_A(l_switch,G1(:,:,:,30),Q(:,63),ZERO,G2tensor(:,18))
  call loop_QV_A(G0(:,:,:,1),wf(:,381),G0(:,:,:,14))
  call loop_Q_A(G0(:,:,:,14),Q(:,54),MB,G1(:,:,:,31))
  call loop_QV_A(G1(:,:,:,31),wf(:,4),G1(:,:,:,32))
  call check_last_Q_A(l_switch,G1(:,:,:,32),Q(:,63),MB,G2tensor(:,19))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,381),Q(:,54),G1(:,:,:,33))
  call check_last_CV_D(l_switch,G1(:,:,:,33),Q(:,54),wf(:,4),Q(:,9),G2tensor(:,20))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,37),ZERO,G1(:,:,:,34))
  call loop_QV_A(G1(:,:,:,34),wf(:,11),G1(:,:,:,35))
  call check_last_Q_A(l_switch,G1(:,:,:,35),Q(:,63),ZERO,G2tensor(:,21))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,16))
  call loop_Q_A(G0(:,:,:,16),Q(:,37),MB,G1(:,:,:,36))
  call loop_QV_A(G1(:,:,:,36),wf(:,11),G1(:,:,:,37))
  call check_last_Q_A(l_switch,G1(:,:,:,37),Q(:,63),MB,G2tensor(:,22))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,47),Q(:,37),G1(:,:,:,38))
  call check_last_CV_D(l_switch,G1(:,:,:,38),Q(:,37),wf(:,11),Q(:,26),G2tensor(:,23))
  call loop_QV_A(G0(:,:,:,1),wf(:,340),G0(:,:,:,17))
  call loop_Q_A(G0(:,:,:,17),Q(:,53),ZERO,G1(:,:,:,39))
  call loop_QV_A(G1(:,:,:,39),wf(:,2),G1(:,:,:,40))
  call check_last_Q_A(l_switch,G1(:,:,:,40),Q(:,63),ZERO,G2tensor(:,24))
  call loop_QV_A(G0(:,:,:,1),wf(:,340),G0(:,:,:,18))
  call loop_Q_A(G0(:,:,:,18),Q(:,53),MB,G1(:,:,:,41))
  call loop_QV_A(G1(:,:,:,41),wf(:,2),G1(:,:,:,42))
  call check_last_Q_A(l_switch,G1(:,:,:,42),Q(:,63),MB,G2tensor(:,25))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,340),Q(:,53),G1(:,:,:,43))
  call check_last_CV_D(l_switch,G1(:,:,:,43),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,26))
  call loop_QV_A(G0(:,:,:,1),wf(:,362),G0(:,:,:,19))
  call loop_Q_A(G0(:,:,:,19),Q(:,53),ZERO,G1(:,:,:,44))
  call loop_QV_A(G1(:,:,:,44),wf(:,2),G1(:,:,:,45))
  call check_last_Q_A(l_switch,G1(:,:,:,45),Q(:,63),ZERO,G2tensor(:,27))
  call loop_QV_A(G0(:,:,:,1),wf(:,362),G0(:,:,:,20))
  call loop_Q_A(G0(:,:,:,20),Q(:,53),MB,G1(:,:,:,46))
  call loop_QV_A(G1(:,:,:,46),wf(:,2),G1(:,:,:,47))
  call check_last_Q_A(l_switch,G1(:,:,:,47),Q(:,63),MB,G2tensor(:,28))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,362),Q(:,53),G1(:,:,:,48))
  call check_last_CV_D(l_switch,G1(:,:,:,48),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,29))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,21))
  call loop_Q_A(G0(:,:,:,21),Q(:,37),ZERO,G1(:,:,:,49))
  call loop_QV_A(G1(:,:,:,49),wf(:,11),G1(:,:,:,50))
  call check_last_Q_A(l_switch,G1(:,:,:,50),Q(:,63),ZERO,G2tensor(:,30))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,22))
  call loop_Q_A(G0(:,:,:,22),Q(:,37),MB,G1(:,:,:,51))
  call loop_QV_A(G1(:,:,:,51),wf(:,11),G1(:,:,:,52))
  call check_last_Q_A(l_switch,G1(:,:,:,52),Q(:,63),MB,G2tensor(:,31))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,48),Q(:,37),G1(:,:,:,53))
  call check_last_CV_D(l_switch,G1(:,:,:,53),Q(:,37),wf(:,11),Q(:,26),G2tensor(:,32))
  call loop_QV_A(G0(:,:,:,1),wf(:,376),G0(:,:,:,23))
  call loop_Q_A(G0(:,:,:,23),Q(:,53),ZERO,G1(:,:,:,54))
  call loop_QV_A(G1(:,:,:,54),wf(:,2),G1(:,:,:,55))
  call check_last_Q_A(l_switch,G1(:,:,:,55),Q(:,63),ZERO,G2tensor(:,33))
  call loop_QV_A(G0(:,:,:,1),wf(:,376),G0(:,:,:,24))
  call loop_Q_A(G0(:,:,:,24),Q(:,53),MB,G1(:,:,:,56))
  call loop_QV_A(G1(:,:,:,56),wf(:,2),G1(:,:,:,57))
  call check_last_Q_A(l_switch,G1(:,:,:,57),Q(:,63),MB,G2tensor(:,34))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,376),Q(:,53),G1(:,:,:,58))
  call check_last_CV_D(l_switch,G1(:,:,:,58),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,35))
  call loop_QV_A(G0(:,:,:,1),wf(:,377),G0(:,:,:,25))
  call loop_Q_A(G0(:,:,:,25),Q(:,53),ZERO,G1(:,:,:,59))
  call loop_QV_A(G1(:,:,:,59),wf(:,2),G1(:,:,:,60))
  call check_last_Q_A(l_switch,G1(:,:,:,60),Q(:,63),ZERO,G2tensor(:,36))
  call loop_QV_A(G0(:,:,:,1),wf(:,377),G0(:,:,:,26))
  call loop_Q_A(G0(:,:,:,26),Q(:,53),MB,G1(:,:,:,61))
  call loop_QV_A(G1(:,:,:,61),wf(:,2),G1(:,:,:,62))
  call check_last_Q_A(l_switch,G1(:,:,:,62),Q(:,63),MB,G2tensor(:,37))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,377),Q(:,53),G1(:,:,:,63))
  call check_last_CV_D(l_switch,G1(:,:,:,63),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,38))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(26)*M(3))+c(27)*M(4)) * den(405)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(16)*M(3))+c(20)*M(4)) * den(405)
  T2sum(1:15,82) = T2sum(1:15,82) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(28)*M(2))+c(29)*M(4)) * den(265)
  T2sum(1:15,79) = T2sum(1:15,79) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(28)*M(2))+c(29)*M(4)) * den(265)
  T2sum(1:15,79) = T2sum(1:15,79) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(26)*M(2))+c(27)*M(4)) * den(265)
  T2sum(1:15,91) = T2sum(1:15,91) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(16)*M(2))+c(20)*M(4)) * den(265)
  T2sum(1:15,79) = T2sum(1:15,79) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(28)*M(2))+c(29)*M(4)) * den(407)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(28)*M(2))+c(29)*M(4)) * den(407)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(26)*M(2))+c(27)*M(4)) * den(407)
  T2sum(1:15,86) = T2sum(1:15,86) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(16)*M(2))+c(20)*M(4)) * den(407)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(29)*M(1)-c(28)*M(2)) * den(269)
  T2sum(1:15,79) = T2sum(1:15,79) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(29)*M(1)-c(28)*M(2)) * den(269)
  T2sum(1:15,79) = T2sum(1:15,79) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(27)*M(1)-c(26)*M(2)) * den(269)
  T2sum(1:15,91) = T2sum(1:15,91) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(20)*M(1)-c(16)*M(2)) * den(269)
  T2sum(1:15,79) = T2sum(1:15,79) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(29)*M(1)-c(28)*M(2)) * den(409)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(29)*M(1)-c(28)*M(2)) * den(409)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(27)*M(1)-c(26)*M(2)) * den(409)
  T2sum(1:15,86) = T2sum(1:15,86) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(20)*M(1)-c(16)*M(2)) * den(409)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(28)*M(2))+c(29)*M(4)) * den(412)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(28)*M(2))+c(29)*M(4)) * den(412)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(26)*M(2))+c(27)*M(4)) * den(412)
  T2sum(1:15,86) = T2sum(1:15,86) + Gcoeff * G2tensor(:,16)
  Gcoeff = (-(c(16)*M(2))+c(20)*M(4)) * den(412)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(29)*M(1)-c(28)*M(2)) * den(413)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(29)*M(1)-c(28)*M(2)) * den(413)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(27)*M(1)-c(26)*M(2)) * den(413)
  T2sum(1:15,86) = T2sum(1:15,86) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(20)*M(1)-c(16)*M(2)) * den(413)
  T2sum(1:15,81) = T2sum(1:15,81) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(28)*M(1)-c(29)*M(3)) * den(278)
  T2sum(1:15,75) = T2sum(1:15,75) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(28)*M(1)-c(29)*M(3)) * den(278)
  T2sum(1:15,75) = T2sum(1:15,75) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(26)*M(1)-c(27)*M(3)) * den(278)
  T2sum(1:15,90) = T2sum(1:15,90) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(16)*M(1)-c(20)*M(3)) * den(278)
  T2sum(1:15,75) = T2sum(1:15,75) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(28)*M(1)-c(29)*M(3)) * den(414)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(28)*M(1)-c(29)*M(3)) * den(414)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(26)*M(1)-c(27)*M(3)) * den(414)
  T2sum(1:15,85) = T2sum(1:15,85) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(16)*M(1)-c(20)*M(3)) * den(414)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(28)*M(1)-c(29)*M(2)) * den(416)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(28)*M(1)-c(29)*M(2)) * den(416)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(26)*M(1)-c(27)*M(2)) * den(416)
  T2sum(1:15,85) = T2sum(1:15,85) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(16)*M(1)-c(20)*M(2)) * den(416)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(28)*M(1)-c(29)*M(2)) * den(280)
  T2sum(1:15,75) = T2sum(1:15,75) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(28)*M(1)-c(29)*M(2)) * den(280)
  T2sum(1:15,75) = T2sum(1:15,75) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(26)*M(1)-c(27)*M(2)) * den(280)
  T2sum(1:15,90) = T2sum(1:15,90) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(16)*M(1)-c(20)*M(2)) * den(280)
  T2sum(1:15,75) = T2sum(1:15,75) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(28)*M(1)-c(29)*M(3)) * den(419)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(28)*M(1)-c(29)*M(3)) * den(419)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(26)*M(1)-c(27)*M(3)) * den(419)
  T2sum(1:15,85) = T2sum(1:15,85) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(16)*M(1)-c(20)*M(3)) * den(419)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(28)*M(1)-c(29)*M(2)) * den(421)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(28)*M(1)-c(29)*M(2)) * den(421)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(26)*M(1)-c(27)*M(2)) * den(421)
  T2sum(1:15,85) = T2sum(1:15,85) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(16)*M(1)-c(20)*M(2)) * den(421)
  T2sum(1:15,78) = T2sum(1:15,78) + Gcoeff * G2tensor(:,38)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_4

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_4_heftpphjjj_dddxdxhg_1_/**/REALKIND
