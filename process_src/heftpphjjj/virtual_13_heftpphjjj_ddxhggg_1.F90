
module ol_vamp_13_heftpphjjj_ddxhggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_13(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_heftpphjjj_ddxhggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_heftpphjjj_ddxhggg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_heftpphjjj_ddxhggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_heftpphjjj_ddxhggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(11)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,21) :: G0
  complex(REALKIND), dimension(4,5,4,50) :: G1
  complex(REALKIND), dimension(15,30) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,637),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,43),ZERO,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,9),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),ZERO,G2tensor(:,1))
  call loop_QV_A(G0(:,:,:,1),wf(:,637),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,43),MB,G1(:,:,:,3))
  call loop_QV_A(G1(:,:,:,3),wf(:,9),G1(:,:,:,4))
  call check_last_Q_A(l_switch,G1(:,:,:,4),Q(:,63),MB,G2tensor(:,2))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,637),Q(:,43),G1(:,:,:,5))
  call check_last_CV_D(l_switch,G1(:,:,:,5),Q(:,43),wf(:,9),Q(:,20),G2tensor(:,3))
  call loop_QV_A(G0(:,:,:,1),wf(:,639),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,43),ZERO,G1(:,:,:,6))
  call loop_QV_A(G1(:,:,:,6),wf(:,9),G1(:,:,:,7))
  call check_last_Q_A(l_switch,G1(:,:,:,7),Q(:,63),ZERO,G2tensor(:,4))
  call loop_QV_A(G0(:,:,:,1),wf(:,639),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,43),MB,G1(:,:,:,8))
  call loop_QV_A(G1(:,:,:,8),wf(:,9),G1(:,:,:,9))
  call check_last_Q_A(l_switch,G1(:,:,:,9),Q(:,63),MB,G2tensor(:,5))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,639),Q(:,43),G1(:,:,:,10))
  call check_last_CV_D(l_switch,G1(:,:,:,10),Q(:,43),wf(:,9),Q(:,20),G2tensor(:,6))
  call loop_QV_A(G0(:,:,:,1),wf(:,646),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,43),ZERO,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,9),G1(:,:,:,12))
  call check_last_Q_A(l_switch,G1(:,:,:,12),Q(:,63),ZERO,G2tensor(:,7))
  call loop_QV_A(G0(:,:,:,1),wf(:,646),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,43),MB,G1(:,:,:,13))
  call loop_QV_A(G1(:,:,:,13),wf(:,9),G1(:,:,:,14))
  call check_last_Q_A(l_switch,G1(:,:,:,14),Q(:,63),MB,G2tensor(:,8))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,646),Q(:,43),G1(:,:,:,15))
  call check_last_CV_D(l_switch,G1(:,:,:,15),Q(:,43),wf(:,9),Q(:,20),G2tensor(:,9))
  call loop_QV_A(G0(:,:,:,1),wf(:,648),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,43),ZERO,G1(:,:,:,16))
  call loop_QV_A(G1(:,:,:,16),wf(:,9),G1(:,:,:,17))
  call check_last_Q_A(l_switch,G1(:,:,:,17),Q(:,63),ZERO,G2tensor(:,10))
  call loop_QV_A(G0(:,:,:,1),wf(:,648),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,43),MB,G1(:,:,:,18))
  call loop_QV_A(G1(:,:,:,18),wf(:,9),G1(:,:,:,19))
  call check_last_Q_A(l_switch,G1(:,:,:,19),Q(:,63),MB,G2tensor(:,11))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,648),Q(:,43),G1(:,:,:,20))
  call check_last_CV_D(l_switch,G1(:,:,:,20),Q(:,43),wf(:,9),Q(:,20),G2tensor(:,12))
  call loop_QV_A(G0(:,:,:,1),wf(:,674),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,43),ZERO,G1(:,:,:,21))
  call loop_QV_A(G1(:,:,:,21),wf(:,9),G1(:,:,:,22))
  call check_last_Q_A(l_switch,G1(:,:,:,22),Q(:,63),ZERO,G2tensor(:,13))
  call loop_QV_A(G0(:,:,:,1),wf(:,674),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,43),MB,G1(:,:,:,23))
  call loop_QV_A(G1(:,:,:,23),wf(:,9),G1(:,:,:,24))
  call check_last_Q_A(l_switch,G1(:,:,:,24),Q(:,63),MB,G2tensor(:,14))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,674),Q(:,43),G1(:,:,:,25))
  call check_last_CV_D(l_switch,G1(:,:,:,25),Q(:,43),wf(:,9),Q(:,20),G2tensor(:,15))
  call loop_QV_A(G0(:,:,:,1),wf(:,675),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,43),ZERO,G1(:,:,:,26))
  call loop_QV_A(G1(:,:,:,26),wf(:,9),G1(:,:,:,27))
  call check_last_Q_A(l_switch,G1(:,:,:,27),Q(:,63),ZERO,G2tensor(:,16))
  call loop_QV_A(G0(:,:,:,1),wf(:,675),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,43),MB,G1(:,:,:,28))
  call loop_QV_A(G1(:,:,:,28),wf(:,9),G1(:,:,:,29))
  call check_last_Q_A(l_switch,G1(:,:,:,29),Q(:,63),MB,G2tensor(:,17))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,675),Q(:,43),G1(:,:,:,30))
  call check_last_CV_D(l_switch,G1(:,:,:,30),Q(:,43),wf(:,9),Q(:,20),G2tensor(:,18))
  call loop_QV_A(G0(:,:,:,1),wf(:,636),G0(:,:,:,14))
  call loop_Q_A(G0(:,:,:,14),Q(:,39),ZERO,G1(:,:,:,31))
  call loop_QV_A(G1(:,:,:,31),wf(:,21),G1(:,:,:,32))
  call check_last_Q_A(l_switch,G1(:,:,:,32),Q(:,63),ZERO,G2tensor(:,19))
  call loop_QV_A(G0(:,:,:,1),wf(:,636),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,39),MB,G1(:,:,:,33))
  call loop_QV_A(G1(:,:,:,33),wf(:,21),G1(:,:,:,34))
  call check_last_Q_A(l_switch,G1(:,:,:,34),Q(:,63),MB,G2tensor(:,20))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,636),Q(:,39),G1(:,:,:,35))
  call check_last_CV_D(l_switch,G1(:,:,:,35),Q(:,39),wf(:,21),Q(:,24),G2tensor(:,21))
  call loop_QV_A(G0(:,:,:,1),wf(:,645),G0(:,:,:,16))
  call loop_Q_A(G0(:,:,:,16),Q(:,39),ZERO,G1(:,:,:,36))
  call loop_QV_A(G1(:,:,:,36),wf(:,21),G1(:,:,:,37))
  call check_last_Q_A(l_switch,G1(:,:,:,37),Q(:,63),ZERO,G2tensor(:,22))
  call loop_QV_A(G0(:,:,:,1),wf(:,645),G0(:,:,:,17))
  call loop_Q_A(G0(:,:,:,17),Q(:,39),MB,G1(:,:,:,38))
  call loop_QV_A(G1(:,:,:,38),wf(:,21),G1(:,:,:,39))
  call check_last_Q_A(l_switch,G1(:,:,:,39),Q(:,63),MB,G2tensor(:,23))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,645),Q(:,39),G1(:,:,:,40))
  call check_last_CV_D(l_switch,G1(:,:,:,40),Q(:,39),wf(:,21),Q(:,24),G2tensor(:,24))
  call loop_QV_A(G0(:,:,:,1),wf(:,654),G0(:,:,:,18))
  call loop_Q_A(G0(:,:,:,18),Q(:,39),ZERO,G1(:,:,:,41))
  call loop_QV_A(G1(:,:,:,41),wf(:,21),G1(:,:,:,42))
  call check_last_Q_A(l_switch,G1(:,:,:,42),Q(:,63),ZERO,G2tensor(:,25))
  call loop_QV_A(G0(:,:,:,1),wf(:,654),G0(:,:,:,19))
  call loop_Q_A(G0(:,:,:,19),Q(:,39),MB,G1(:,:,:,43))
  call loop_QV_A(G1(:,:,:,43),wf(:,21),G1(:,:,:,44))
  call check_last_Q_A(l_switch,G1(:,:,:,44),Q(:,63),MB,G2tensor(:,26))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,654),Q(:,39),G1(:,:,:,45))
  call check_last_CV_D(l_switch,G1(:,:,:,45),Q(:,39),wf(:,21),Q(:,24),G2tensor(:,27))
  call loop_QV_A(G0(:,:,:,1),wf(:,655),G0(:,:,:,20))
  call loop_Q_A(G0(:,:,:,20),Q(:,39),ZERO,G1(:,:,:,46))
  call loop_QV_A(G1(:,:,:,46),wf(:,21),G1(:,:,:,47))
  call check_last_Q_A(l_switch,G1(:,:,:,47),Q(:,63),ZERO,G2tensor(:,28))
  call loop_QV_A(G0(:,:,:,1),wf(:,655),G0(:,:,:,21))
  call loop_Q_A(G0(:,:,:,21),Q(:,39),MB,G1(:,:,:,48))
  call loop_QV_A(G1(:,:,:,48),wf(:,21),G1(:,:,:,49))
  call check_last_Q_A(l_switch,G1(:,:,:,49),Q(:,63),MB,G2tensor(:,29))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,655),Q(:,39),G1(:,:,:,50))
  call check_last_CV_D(l_switch,G1(:,:,:,50),Q(:,39),wf(:,21),Q(:,24),G2tensor(:,30))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(25)*(M(1)-M(3))) * den(813)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(25)*(M(1)-M(3))) * den(813)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(23)*(M(1)-M(3))) * den(813)
  T2sum(1:15,104) = T2sum(1:15,104) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(7)*(M(1)-M(3))) * den(813)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(29)*M(3)) * den(814)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(29)*M(3)) * den(814)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(27)*M(3)) * den(814)
  T2sum(1:15,104) = T2sum(1:15,104) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(17)*M(3)) * den(814)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(25)*(M(5)-M(6))) * den(817)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(25)*(M(5)-M(6))) * den(817)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(23)*(M(5)-M(6))) * den(817)
  T2sum(1:15,104) = T2sum(1:15,104) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(7)*(M(5)-M(6))) * den(817)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(29)*M(5)) * den(819)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(29)*M(5)) * den(819)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(27)*M(5)) * den(819)
  T2sum(1:15,104) = T2sum(1:15,104) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(17)*M(5)) * den(819)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(25)*(-M(3)+M(4))) * den(822)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(25)*(-M(3)+M(4))) * den(822)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(23)*(-M(3)+M(4))) * den(822)
  T2sum(1:15,104) = T2sum(1:15,104) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(7)*(-M(3)+M(4))) * den(822)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(25)*(-M(2)+M(5))) * den(824)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(25)*(-M(2)+M(5))) * den(824)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(23)*(-M(2)+M(5))) * den(824)
  T2sum(1:15,104) = T2sum(1:15,104) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(7)*(-M(2)+M(5))) * den(824)
  T2sum(1:15,96) = T2sum(1:15,96) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(25)*(-M(1)+M(3))) * den(825)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(25)*(-M(1)+M(3))) * den(825)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(23)*(-M(1)+M(3))) * den(825)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(7)*(-M(1)+M(3))) * den(825)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(25)*(-M(5)+M(6))) * den(827)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(25)*(-M(5)+M(6))) * den(827)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(23)*(-M(5)+M(6))) * den(827)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(7)*(-M(5)+M(6))) * den(827)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(25)*(-M(1)+M(3))) * den(829)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(25)*(-M(1)+M(3))) * den(829)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(23)*(-M(1)+M(3))) * den(829)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(7)*(-M(1)+M(3))) * den(829)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(25)*(-M(5)+M(6))) * den(831)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(25)*(-M(5)+M(6))) * den(831)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(23)*(-M(5)+M(6))) * den(831)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(7)*(-M(5)+M(6))) * den(831)
  T2sum(1:15,87) = T2sum(1:15,87) + Gcoeff * G2tensor(:,30)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_13

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_13_heftpphjjj_ddxhggg_1_/**/REALKIND
