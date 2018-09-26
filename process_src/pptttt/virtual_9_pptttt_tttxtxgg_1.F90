
module ol_vamp_9_pptttt_tttxtxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_9(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pptttt_tttxtxgg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pptttt_tttxtxgg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pptttt_tttxtxgg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pptttt_tttxtxgg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(14)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,23) :: G0
  complex(REALKIND), dimension(4,5,4,60) :: G1
  complex(REALKIND), dimension(15,42) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,90),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,37),MB,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,15),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),MB,G2tensor(:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,25),G1(:,:,:,3))
  call check_last_Q_A(l_switch,G1(:,:,:,3),Q(:,63),MB,G2tensor(:,2))
  call loop_QV_A(G1(:,:,:,1),wf(:,34),G1(:,:,:,4))
  call check_last_Q_A(l_switch,G1(:,:,:,4),Q(:,63),MB,G2tensor(:,3))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,90),Q(:,37),G1(:,:,:,5))
  call check_last_CV_D(l_switch,G1(:,:,:,5),Q(:,37),wf(:,15),Q(:,26),G2tensor(:,4))
  call check_last_CV_D(l_switch,G1(:,:,:,5),Q(:,37),wf(:,25),Q(:,26),G2tensor(:,5))
  call check_last_CV_D(l_switch,G1(:,:,:,5),Q(:,37),wf(:,34),Q(:,26),G2tensor(:,6))
  call loop_QV_A(G0(:,:,:,1),wf(:,532),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,53),ZERO,G1(:,:,:,6))
  call loop_QV_A(G1(:,:,:,6),wf(:,2),G1(:,:,:,7))
  call check_last_Q_A(l_switch,G1(:,:,:,7),Q(:,63),ZERO,G2tensor(:,7))
  call loop_QV_A(G0(:,:,:,1),wf(:,532),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,53),MT,G1(:,:,:,8))
  call loop_QV_A(G1(:,:,:,8),wf(:,2),G1(:,:,:,9))
  call check_last_Q_A(l_switch,G1(:,:,:,9),Q(:,63),MT,G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,532),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,53),MB,G1(:,:,:,10))
  call loop_QV_A(G1(:,:,:,10),wf(:,2),G1(:,:,:,11))
  call check_last_Q_A(l_switch,G1(:,:,:,11),Q(:,63),MB,G2tensor(:,9))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,532),Q(:,53),G1(:,:,:,12))
  call check_last_CV_D(l_switch,G1(:,:,:,12),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,10))
  call loop_QV_A(G0(:,:,:,1),wf(:,538),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,53),ZERO,G1(:,:,:,13))
  call loop_QV_A(G1(:,:,:,13),wf(:,2),G1(:,:,:,14))
  call check_last_Q_A(l_switch,G1(:,:,:,14),Q(:,63),ZERO,G2tensor(:,11))
  call loop_QV_A(G0(:,:,:,1),wf(:,538),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,53),MT,G1(:,:,:,15))
  call loop_QV_A(G1(:,:,:,15),wf(:,2),G1(:,:,:,16))
  call check_last_Q_A(l_switch,G1(:,:,:,16),Q(:,63),MT,G2tensor(:,12))
  call loop_QV_A(G0(:,:,:,1),wf(:,538),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,53),MB,G1(:,:,:,17))
  call loop_QV_A(G1(:,:,:,17),wf(:,2),G1(:,:,:,18))
  call check_last_Q_A(l_switch,G1(:,:,:,18),Q(:,63),MB,G2tensor(:,13))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,538),Q(:,53),G1(:,:,:,19))
  call check_last_CV_D(l_switch,G1(:,:,:,19),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,14))
  call loop_QV_A(G0(:,:,:,1),wf(:,558),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,53),ZERO,G1(:,:,:,20))
  call loop_QV_A(G1(:,:,:,20),wf(:,2),G1(:,:,:,21))
  call check_last_Q_A(l_switch,G1(:,:,:,21),Q(:,63),ZERO,G2tensor(:,15))
  call loop_QV_A(G0(:,:,:,1),wf(:,558),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,53),MT,G1(:,:,:,22))
  call loop_QV_A(G1(:,:,:,22),wf(:,2),G1(:,:,:,23))
  call check_last_Q_A(l_switch,G1(:,:,:,23),Q(:,63),MT,G2tensor(:,16))
  call loop_QV_A(G0(:,:,:,1),wf(:,558),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,53),MB,G1(:,:,:,24))
  call loop_QV_A(G1(:,:,:,24),wf(:,2),G1(:,:,:,25))
  call check_last_Q_A(l_switch,G1(:,:,:,25),Q(:,63),MB,G2tensor(:,17))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,558),Q(:,53),G1(:,:,:,26))
  call check_last_CV_D(l_switch,G1(:,:,:,26),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,18))
  call loop_QV_A(G0(:,:,:,1),wf(:,91),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,37),ZERO,G1(:,:,:,27))
  call loop_QV_A(G1(:,:,:,27),wf(:,15),G1(:,:,:,28))
  call check_last_Q_A(l_switch,G1(:,:,:,28),Q(:,63),ZERO,G2tensor(:,19))
  call loop_QV_A(G1(:,:,:,27),wf(:,25),G1(:,:,:,29))
  call check_last_Q_A(l_switch,G1(:,:,:,29),Q(:,63),ZERO,G2tensor(:,20))
  call loop_QV_A(G1(:,:,:,27),wf(:,34),G1(:,:,:,30))
  call check_last_Q_A(l_switch,G1(:,:,:,30),Q(:,63),ZERO,G2tensor(:,21))
  call loop_QV_A(G0(:,:,:,1),wf(:,91),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,37),MT,G1(:,:,:,31))
  call loop_QV_A(G1(:,:,:,31),wf(:,15),G1(:,:,:,32))
  call check_last_Q_A(l_switch,G1(:,:,:,32),Q(:,63),MT,G2tensor(:,22))
  call loop_QV_A(G1(:,:,:,31),wf(:,25),G1(:,:,:,33))
  call check_last_Q_A(l_switch,G1(:,:,:,33),Q(:,63),MT,G2tensor(:,23))
  call loop_QV_A(G1(:,:,:,31),wf(:,34),G1(:,:,:,34))
  call check_last_Q_A(l_switch,G1(:,:,:,34),Q(:,63),MT,G2tensor(:,24))
  call loop_QV_A(G0(:,:,:,1),wf(:,91),G0(:,:,:,14))
  call loop_Q_A(G0(:,:,:,14),Q(:,37),MB,G1(:,:,:,35))
  call loop_QV_A(G1(:,:,:,35),wf(:,15),G1(:,:,:,36))
  call check_last_Q_A(l_switch,G1(:,:,:,36),Q(:,63),MB,G2tensor(:,25))
  call loop_QV_A(G1(:,:,:,35),wf(:,25),G1(:,:,:,37))
  call check_last_Q_A(l_switch,G1(:,:,:,37),Q(:,63),MB,G2tensor(:,26))
  call loop_QV_A(G1(:,:,:,35),wf(:,34),G1(:,:,:,38))
  call check_last_Q_A(l_switch,G1(:,:,:,38),Q(:,63),MB,G2tensor(:,27))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,91),Q(:,37),G1(:,:,:,39))
  call check_last_CV_D(l_switch,G1(:,:,:,39),Q(:,37),wf(:,15),Q(:,26),G2tensor(:,28))
  call check_last_CV_D(l_switch,G1(:,:,:,39),Q(:,37),wf(:,25),Q(:,26),G2tensor(:,29))
  call check_last_CV_D(l_switch,G1(:,:,:,39),Q(:,37),wf(:,34),Q(:,26),G2tensor(:,30))
  call loop_QV_A(G0(:,:,:,1),wf(:,559),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,53),ZERO,G1(:,:,:,40))
  call loop_QV_A(G1(:,:,:,40),wf(:,2),G1(:,:,:,41))
  call check_last_Q_A(l_switch,G1(:,:,:,41),Q(:,63),ZERO,G2tensor(:,31))
  call loop_QV_A(G0(:,:,:,1),wf(:,559),G0(:,:,:,16))
  call loop_Q_A(G0(:,:,:,16),Q(:,53),MT,G1(:,:,:,42))
  call loop_QV_A(G1(:,:,:,42),wf(:,2),G1(:,:,:,43))
  call check_last_Q_A(l_switch,G1(:,:,:,43),Q(:,63),MT,G2tensor(:,32))
  call loop_QV_A(G0(:,:,:,1),wf(:,559),G0(:,:,:,17))
  call loop_Q_A(G0(:,:,:,17),Q(:,53),MB,G1(:,:,:,44))
  call loop_QV_A(G1(:,:,:,44),wf(:,2),G1(:,:,:,45))
  call check_last_Q_A(l_switch,G1(:,:,:,45),Q(:,63),MB,G2tensor(:,33))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,559),Q(:,53),G1(:,:,:,46))
  call check_last_CV_D(l_switch,G1(:,:,:,46),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,34))
  call loop_QV_A(G0(:,:,:,1),wf(:,576),G0(:,:,:,18))
  call loop_Q_A(G0(:,:,:,18),Q(:,53),ZERO,G1(:,:,:,47))
  call loop_QV_A(G1(:,:,:,47),wf(:,2),G1(:,:,:,48))
  call check_last_Q_A(l_switch,G1(:,:,:,48),Q(:,63),ZERO,G2tensor(:,35))
  call loop_QV_A(G0(:,:,:,1),wf(:,576),G0(:,:,:,19))
  call loop_Q_A(G0(:,:,:,19),Q(:,53),MT,G1(:,:,:,49))
  call loop_QV_A(G1(:,:,:,49),wf(:,2),G1(:,:,:,50))
  call check_last_Q_A(l_switch,G1(:,:,:,50),Q(:,63),MT,G2tensor(:,36))
  call loop_QV_A(G0(:,:,:,1),wf(:,576),G0(:,:,:,20))
  call loop_Q_A(G0(:,:,:,20),Q(:,53),MB,G1(:,:,:,51))
  call loop_QV_A(G1(:,:,:,51),wf(:,2),G1(:,:,:,52))
  call check_last_Q_A(l_switch,G1(:,:,:,52),Q(:,63),MB,G2tensor(:,37))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,576),Q(:,53),G1(:,:,:,53))
  call check_last_CV_D(l_switch,G1(:,:,:,53),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,38))
  call loop_QV_A(G0(:,:,:,1),wf(:,577),G0(:,:,:,21))
  call loop_Q_A(G0(:,:,:,21),Q(:,53),ZERO,G1(:,:,:,54))
  call loop_QV_A(G1(:,:,:,54),wf(:,2),G1(:,:,:,55))
  call check_last_Q_A(l_switch,G1(:,:,:,55),Q(:,63),ZERO,G2tensor(:,39))
  call loop_QV_A(G0(:,:,:,1),wf(:,577),G0(:,:,:,22))
  call loop_Q_A(G0(:,:,:,22),Q(:,53),MT,G1(:,:,:,56))
  call loop_QV_A(G1(:,:,:,56),wf(:,2),G1(:,:,:,57))
  call check_last_Q_A(l_switch,G1(:,:,:,57),Q(:,63),MT,G2tensor(:,40))
  call loop_QV_A(G0(:,:,:,1),wf(:,577),G0(:,:,:,23))
  call loop_Q_A(G0(:,:,:,23),Q(:,53),MB,G1(:,:,:,58))
  call loop_QV_A(G1(:,:,:,58),wf(:,2),G1(:,:,:,59))
  call check_last_Q_A(l_switch,G1(:,:,:,59),Q(:,63),MB,G2tensor(:,41))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,577),Q(:,53),G1(:,:,:,60))
  call check_last_CV_D(l_switch,G1(:,:,:,60),Q(:,53),wf(:,2),Q(:,10),G2tensor(:,42))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(32)*(M(3)-M(7))) * den(446)
  T2sum(1:15,199) = T2sum(1:15,199) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(15)*(M(3)-M(7))) * den(446)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(34)*(M(3)-M(7))) * den(722)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(32)*(M(3)-M(7))) * den(722)
  T2sum(1:15,186) = T2sum(1:15,186) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(34)*(M(3)-M(7))) * den(722)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(32)*(M(3)-M(7))) * den(722)
  T2sum(1:15,187) = T2sum(1:15,187) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(15)*(M(3)-M(7))) * den(722)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(39)*M(5))+c(40)*M(7)) * den(723)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(36)*M(5))+c(37)*M(7)) * den(723)
  T2sum(1:15,186) = T2sum(1:15,186) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(39)*M(5))+c(40)*M(7)) * den(723)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(36)*M(5))+c(37)*M(7)) * den(723)
  T2sum(1:15,187) = T2sum(1:15,187) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(26)*M(5))+c(30)*M(7)) * den(723)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(34)*(-M(2)+M(10))) * den(727)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(32)*(-M(2)+M(10))) * den(727)
  T2sum(1:15,186) = T2sum(1:15,186) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(34)*(-M(2)+M(10))) * den(727)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(32)*(-M(2)+M(10))) * den(727)
  T2sum(1:15,187) = T2sum(1:15,187) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(15)*(-M(2)+M(10))) * den(727)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(34)*(-M(2)+M(10))) * den(451)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(32)*(-M(2)+M(10))) * den(451)
  T2sum(1:15,198) = T2sum(1:15,198) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(34)*(-M(2)+M(10))) * den(451)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(32)*(-M(2)+M(10))) * den(451)
  T2sum(1:15,199) = T2sum(1:15,199) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(15)*(-M(2)+M(10))) * den(451)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,28)
  Gcoeff = (-(c(39)*M(9))+c(40)*M(10)) * den(729)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,31)
  Gcoeff = (-(c(36)*M(9))+c(37)*M(10)) * den(729)
  T2sum(1:15,186) = T2sum(1:15,186) + Gcoeff * G2tensor(:,32)
  Gcoeff = (-(c(39)*M(9))+c(40)*M(10)) * den(729)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,31)
  Gcoeff = (-(c(36)*M(9))+c(37)*M(10)) * den(729)
  T2sum(1:15,187) = T2sum(1:15,187) + Gcoeff * G2tensor(:,33)
  Gcoeff = (-(c(26)*M(9))+c(30)*M(10)) * den(729)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(33)*(M(5)-M(9))+c(34)*(-M(7)+M(11))) * den(731)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(31)*(M(5)-M(9))+c(32)*(-M(7)+M(11))) * den(731)
  T2sum(1:15,186) = T2sum(1:15,186) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(33)*(M(5)-M(9))+c(34)*(-M(7)+M(11))) * den(731)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(31)*(M(5)-M(9))+c(32)*(-M(7)+M(11))) * den(731)
  T2sum(1:15,187) = T2sum(1:15,187) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(11)*(M(5)-M(9))+c(15)*(-M(7)+M(11))) * den(731)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(33)*(M(5)-M(9))+c(34)*(-M(6)+M(10))) * den(733)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(31)*(M(5)-M(9))+c(32)*(-M(6)+M(10))) * den(733)
  T2sum(1:15,186) = T2sum(1:15,186) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(33)*(M(5)-M(9))+c(34)*(-M(6)+M(10))) * den(733)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(31)*(M(5)-M(9))+c(32)*(-M(6)+M(10))) * den(733)
  T2sum(1:15,187) = T2sum(1:15,187) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(11)*(M(5)-M(9))+c(15)*(-M(6)+M(10))) * den(733)
  T2sum(1:15,183) = T2sum(1:15,183) + Gcoeff * G2tensor(:,42)
  Gcoeff = (-(c(36)*M(1))+c(37)*M(3)) * den(476)
  T2sum(1:15,199) = T2sum(1:15,199) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(26)*M(1))+c(30)*M(3)) * den(476)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(39)*M(1))+c(40)*M(10)) * den(479)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,20)
  Gcoeff = (-(c(36)*M(1))+c(37)*M(10)) * den(479)
  T2sum(1:15,198) = T2sum(1:15,198) + Gcoeff * G2tensor(:,23)
  Gcoeff = (-(c(39)*M(1))+c(40)*M(10)) * den(479)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,20)
  Gcoeff = (-(c(36)*M(1))+c(37)*M(10)) * den(479)
  T2sum(1:15,199) = T2sum(1:15,199) + Gcoeff * G2tensor(:,26)
  Gcoeff = (-(c(26)*M(1))+c(30)*M(10)) * den(479)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,29)
  Gcoeff = (-(c(36)*M(1))+c(37)*M(7)) * den(497)
  T2sum(1:15,199) = T2sum(1:15,199) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(26)*M(1))+c(30)*M(7)) * den(497)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(39)*M(1))+c(40)*M(2)) * den(502)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,21)
  Gcoeff = (-(c(36)*M(1))+c(37)*M(2)) * den(502)
  T2sum(1:15,198) = T2sum(1:15,198) + Gcoeff * G2tensor(:,24)
  Gcoeff = (-(c(39)*M(1))+c(40)*M(2)) * den(502)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,21)
  Gcoeff = (-(c(36)*M(1))+c(37)*M(2)) * den(502)
  T2sum(1:15,199) = T2sum(1:15,199) + Gcoeff * G2tensor(:,27)
  Gcoeff = (-(c(26)*M(1))+c(30)*M(2)) * den(502)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,30)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_9

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_9_pptttt_tttxtxgg_1_/**/REALKIND
