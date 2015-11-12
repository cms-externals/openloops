
module ol_vamp_34_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_34(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppjjjj_gggggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppjjjj_gggggg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppjjjj_gggggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppjjjj_gggggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(250)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,1) :: G0
  complex(REALKIND), dimension(4,5,4,1) :: G1
  complex(REALKIND), dimension(4,15,4,247) :: G2
  complex(REALKIND), dimension(4,35,4,86) :: G3
  complex(REALKIND), dimension(4,70,4,26) :: G4
  complex(REALKIND), dimension(4,126,4,6) :: G5
  complex(REALKIND), dimension(35,285) :: G3tensor
  complex(REALKIND), dimension(70,75) :: G4tensor
  complex(REALKIND), dimension(126,20) :: G5tensor
  complex(REALKIND), dimension(210,6) :: G6tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,-4),G1(:,:,:,1))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,67),G2(:,:,:,1))
  call check_last_DV_C(l_switch,G2(:,:,:,1),Q(:,59),wf(:,-2),G3tensor(:,1))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,68),G2(:,:,:,2))
  call check_last_DV_C(l_switch,G2(:,:,:,2),Q(:,59),wf(:,-2),G3tensor(:,2))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,69),G2(:,:,:,3))
  call check_last_DV_C(l_switch,G2(:,:,:,3),Q(:,59),wf(:,-2),G3tensor(:,3))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,76),G2(:,:,:,4))
  call check_last_DV_C(l_switch,G2(:,:,:,4),Q(:,55),wf(:,-3),G3tensor(:,4))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,77),G2(:,:,:,5))
  call check_last_DV_C(l_switch,G2(:,:,:,5),Q(:,55),wf(:,-3),G3tensor(:,5))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,78),G2(:,:,:,6))
  call check_last_DV_C(l_switch,G2(:,:,:,6),Q(:,55),wf(:,-3),G3tensor(:,6))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,-5),G2(:,:,:,7))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,85),G3tensor(:,7))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,86),G3tensor(:,8))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,87),G3tensor(:,9))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,149),G3tensor(:,10))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,150),G3tensor(:,11))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,151),G3tensor(:,12))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,178),G3tensor(:,13))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,179),G3tensor(:,14))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,180),G3tensor(:,15))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1021),G3tensor(:,16))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1022),G3tensor(:,17))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1023),G3tensor(:,18))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,203),G3tensor(:,19))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,204),G3tensor(:,20))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,205),G3tensor(:,21))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,232),G3tensor(:,22))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,233),G3tensor(:,23))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,234),G3tensor(:,24))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1123),G3tensor(:,25))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1124),G3tensor(:,26))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1125),G3tensor(:,27))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,250),G3tensor(:,28))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,251),G3tensor(:,29))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,252),G3tensor(:,30))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1159),G3tensor(:,31))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1160),G3tensor(:,32))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1161),G3tensor(:,33))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1168),G3tensor(:,34))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1169),G3tensor(:,35))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1170),G3tensor(:,36))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,271),G3tensor(:,37))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1321),G3tensor(:,38))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1324),G3tensor(:,39))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,274),G3tensor(:,40))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1333),G3tensor(:,41))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1336),G3tensor(:,42))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,277),G3tensor(:,43))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1345),G3tensor(:,44))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1348),G3tensor(:,45))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1357),G3tensor(:,46))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1360),G3tensor(:,47))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1369),G3tensor(:,48))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1372),G3tensor(:,49))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1381),G3tensor(:,50))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,48),wf(:,1382),G3tensor(:,51))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,14),G2(:,:,:,8))
  call check_last_DV_C(l_switch,G2(:,:,:,8),Q(:,60),wf(:,61),G3tensor(:,52))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,17),G2(:,:,:,9))
  call check_last_DV_C(l_switch,G2(:,:,:,9),Q(:,60),wf(:,61),G3tensor(:,53))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,18),G2(:,:,:,10))
  call check_last_DV_C(l_switch,G2(:,:,:,10),Q(:,60),wf(:,61),G3tensor(:,54))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,96),G2(:,:,:,11))
  call check_last_DV_C(l_switch,G2(:,:,:,11),Q(:,61),wf(:,-1),G3tensor(:,55))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,97),G2(:,:,:,12))
  call check_last_DV_C(l_switch,G2(:,:,:,12),Q(:,61),wf(:,-1),G3tensor(:,56))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,98),G2(:,:,:,13))
  call check_last_DV_C(l_switch,G2(:,:,:,13),Q(:,61),wf(:,-1),G3tensor(:,57))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,863),G2(:,:,:,14))
  call check_last_DV_C(l_switch,G2(:,:,:,14),Q(:,62),wf(:,0),G3tensor(:,58))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,865),G2(:,:,:,15))
  call check_last_DV_C(l_switch,G2(:,:,:,15),Q(:,62),wf(:,0),G3tensor(:,59))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,866),G2(:,:,:,16))
  call check_last_DV_C(l_switch,G2(:,:,:,16),Q(:,62),wf(:,0),G3tensor(:,60))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,118),G2(:,:,:,17))
  call check_last_DV_C(l_switch,G2(:,:,:,17),Q(:,61),wf(:,-1),G3tensor(:,61))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,119),G2(:,:,:,18))
  call check_last_DV_C(l_switch,G2(:,:,:,18),Q(:,61),wf(:,-1),G3tensor(:,62))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,120),G2(:,:,:,19))
  call check_last_DV_C(l_switch,G2(:,:,:,19),Q(:,61),wf(:,-1),G3tensor(:,63))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,889),G2(:,:,:,20))
  call check_last_DV_C(l_switch,G2(:,:,:,20),Q(:,62),wf(:,0),G3tensor(:,64))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,890),G2(:,:,:,21))
  call check_last_DV_C(l_switch,G2(:,:,:,21),Q(:,62),wf(:,0),G3tensor(:,65))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,891),G2(:,:,:,22))
  call check_last_DV_C(l_switch,G2(:,:,:,22),Q(:,62),wf(:,0),G3tensor(:,66))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,136),G2(:,:,:,23))
  call check_last_DV_C(l_switch,G2(:,:,:,23),Q(:,61),wf(:,-1),G3tensor(:,67))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,137),G2(:,:,:,24))
  call check_last_DV_C(l_switch,G2(:,:,:,24),Q(:,61),wf(:,-1),G3tensor(:,68))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,138),G2(:,:,:,25))
  call check_last_DV_C(l_switch,G2(:,:,:,25),Q(:,61),wf(:,-1),G3tensor(:,69))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,898),G2(:,:,:,26))
  call check_last_DV_C(l_switch,G2(:,:,:,26),Q(:,62),wf(:,0),G3tensor(:,70))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,899),G2(:,:,:,27))
  call check_last_DV_C(l_switch,G2(:,:,:,27),Q(:,62),wf(:,0),G3tensor(:,71))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,900),G2(:,:,:,28))
  call check_last_DV_C(l_switch,G2(:,:,:,28),Q(:,62),wf(:,0),G3tensor(:,72))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,907),G2(:,:,:,29))
  call check_last_DV_C(l_switch,G2(:,:,:,29),Q(:,61),wf(:,-1),G3tensor(:,73))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,908),G2(:,:,:,30))
  call check_last_DV_C(l_switch,G2(:,:,:,30),Q(:,61),wf(:,-1),G3tensor(:,74))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,909),G2(:,:,:,31))
  call check_last_DV_C(l_switch,G2(:,:,:,31),Q(:,61),wf(:,-1),G3tensor(:,75))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,901),G2(:,:,:,32))
  call check_last_DV_C(l_switch,G2(:,:,:,32),Q(:,62),wf(:,0),G3tensor(:,76))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,902),G2(:,:,:,33))
  call check_last_DV_C(l_switch,G2(:,:,:,33),Q(:,62),wf(:,0),G3tensor(:,77))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,903),G2(:,:,:,34))
  call check_last_DV_C(l_switch,G2(:,:,:,34),Q(:,62),wf(:,0),G3tensor(:,78))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,-1),G2(:,:,:,35))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,14),G3(:,:,:,1))
  call check_last_DV_C(l_switch,G3(:,:,:,1),Q(:,62),wf(:,0),G4tensor(:,1))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,17),G3(:,:,:,2))
  call check_last_DV_C(l_switch,G3(:,:,:,2),Q(:,62),wf(:,0),G4tensor(:,2))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,18),G3(:,:,:,3))
  call check_last_DV_C(l_switch,G3(:,:,:,3),Q(:,62),wf(:,0),G4tensor(:,3))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,254),G3(:,:,:,4))
  call check_last_DV_C(l_switch,G3(:,:,:,4),Q(:,62),wf(:,0),G4tensor(:,4))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,62),G3(:,:,:,5))
  call loop_DV_C(G3(:,:,:,5),Q(:,30),wf(:,-5),G4(:,:,:,1))
  call check_last_DV_C(l_switch,G4(:,:,:,1),Q(:,62),wf(:,0),G5tensor(:,1))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,263),G3(:,:,:,6))
  call check_last_DV_C(l_switch,G3(:,:,:,6),Q(:,62),wf(:,0),G4tensor(:,5))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,-3),G3(:,:,:,7))
  call loop_DV_C(G3(:,:,:,7),Q(:,26),wf(:,70),G4(:,:,:,2))
  call check_last_DV_C(l_switch,G4(:,:,:,2),Q(:,62),wf(:,0),G5tensor(:,2))
  call loop_DV_C(G3(:,:,:,7),Q(:,26),wf(:,-2),G4(:,:,:,3))
  call loop_DV_C(G4(:,:,:,3),Q(:,30),wf(:,-5),G5(:,:,:,1))
  call check_last_DV_C(l_switch,G5(:,:,:,1),Q(:,62),wf(:,0),G6tensor(:,1))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,264),G3(:,:,:,8))
  call check_last_DV_C(l_switch,G3(:,:,:,8),Q(:,62),wf(:,0),G4tensor(:,6))
  call loop_DV_C(G2(:,:,:,35),Q(:,18),wf(:,-2),G3(:,:,:,9))
  call loop_DV_C(G3(:,:,:,9),Q(:,22),wf(:,79),G4(:,:,:,4))
  call check_last_DV_C(l_switch,G4(:,:,:,4),Q(:,62),wf(:,0),G5tensor(:,3))
  call loop_DV_C(G3(:,:,:,9),Q(:,22),wf(:,-3),G4(:,:,:,5))
  call loop_DV_C(G4(:,:,:,5),Q(:,30),wf(:,-5),G5(:,:,:,2))
  call check_last_DV_C(l_switch,G5(:,:,:,2),Q(:,62),wf(:,0),G6tensor(:,2))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,142),G2(:,:,:,36))
  call check_last_DV_C(l_switch,G2(:,:,:,36),Q(:,55),wf(:,-3),G3tensor(:,79))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,143),G2(:,:,:,37))
  call check_last_DV_C(l_switch,G2(:,:,:,37),Q(:,55),wf(:,-3),G3tensor(:,80))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,144),G2(:,:,:,38))
  call check_last_DV_C(l_switch,G2(:,:,:,38),Q(:,55),wf(:,-3),G3tensor(:,81))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,32),G2(:,:,:,39))
  call check_last_DV_C(l_switch,G2(:,:,:,39),Q(:,58),wf(:,90),G3tensor(:,82))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,35),G2(:,:,:,40))
  call check_last_DV_C(l_switch,G2(:,:,:,40),Q(:,58),wf(:,90),G3tensor(:,83))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,36),G2(:,:,:,41))
  call check_last_DV_C(l_switch,G2(:,:,:,41),Q(:,58),wf(:,90),G3tensor(:,84))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,154),G2(:,:,:,42))
  call check_last_DV_C(l_switch,G2(:,:,:,42),Q(:,59),wf(:,-2),G3tensor(:,85))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,155),G2(:,:,:,43))
  call check_last_DV_C(l_switch,G2(:,:,:,43),Q(:,59),wf(:,-2),G3tensor(:,86))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,156),G2(:,:,:,44))
  call check_last_DV_C(l_switch,G2(:,:,:,44),Q(:,59),wf(:,-2),G3tensor(:,87))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,952),G2(:,:,:,45))
  call check_last_DV_C(l_switch,G2(:,:,:,45),Q(:,62),wf(:,0),G3tensor(:,88))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,953),G2(:,:,:,46))
  call check_last_DV_C(l_switch,G2(:,:,:,46),Q(:,62),wf(:,0),G3tensor(:,89))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,954),G2(:,:,:,47))
  call check_last_DV_C(l_switch,G2(:,:,:,47),Q(:,62),wf(:,0),G3tensor(:,90))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,172),G2(:,:,:,48))
  call check_last_DV_C(l_switch,G2(:,:,:,48),Q(:,59),wf(:,-2),G3tensor(:,91))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,173),G2(:,:,:,49))
  call check_last_DV_C(l_switch,G2(:,:,:,49),Q(:,59),wf(:,-2),G3tensor(:,92))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,174),G2(:,:,:,50))
  call check_last_DV_C(l_switch,G2(:,:,:,50),Q(:,59),wf(:,-2),G3tensor(:,93))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,961),G2(:,:,:,51))
  call check_last_DV_C(l_switch,G2(:,:,:,51),Q(:,62),wf(:,0),G3tensor(:,94))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,962),G2(:,:,:,52))
  call check_last_DV_C(l_switch,G2(:,:,:,52),Q(:,62),wf(:,0),G3tensor(:,95))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,963),G2(:,:,:,53))
  call check_last_DV_C(l_switch,G2(:,:,:,53),Q(:,62),wf(:,0),G3tensor(:,96))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,970),G2(:,:,:,54))
  call check_last_DV_C(l_switch,G2(:,:,:,54),Q(:,59),wf(:,-2),G3tensor(:,97))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,971),G2(:,:,:,55))
  call check_last_DV_C(l_switch,G2(:,:,:,55),Q(:,59),wf(:,-2),G3tensor(:,98))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,972),G2(:,:,:,56))
  call check_last_DV_C(l_switch,G2(:,:,:,56),Q(:,59),wf(:,-2),G3tensor(:,99))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,964),G2(:,:,:,57))
  call check_last_DV_C(l_switch,G2(:,:,:,57),Q(:,62),wf(:,0),G3tensor(:,100))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,965),G2(:,:,:,58))
  call check_last_DV_C(l_switch,G2(:,:,:,58),Q(:,62),wf(:,0),G3tensor(:,101))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,966),G2(:,:,:,59))
  call check_last_DV_C(l_switch,G2(:,:,:,59),Q(:,62),wf(:,0),G3tensor(:,102))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,-2),G2(:,:,:,60))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,32),G3(:,:,:,10))
  call check_last_DV_C(l_switch,G3(:,:,:,10),Q(:,62),wf(:,0),G4tensor(:,7))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,35),G3(:,:,:,11))
  call check_last_DV_C(l_switch,G3(:,:,:,11),Q(:,62),wf(:,0),G4tensor(:,8))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,36),G3(:,:,:,12))
  call check_last_DV_C(l_switch,G3(:,:,:,12),Q(:,62),wf(:,0),G4tensor(:,9))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,49),G3(:,:,:,13))
  call check_last_DV_C(l_switch,G3(:,:,:,13),Q(:,61),wf(:,-1),G4tensor(:,10))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,51),G3(:,:,:,14))
  call check_last_DV_C(l_switch,G3(:,:,:,14),Q(:,61),wf(:,-1),G4tensor(:,11))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,52),G3(:,:,:,15))
  call check_last_DV_C(l_switch,G3(:,:,:,15),Q(:,61),wf(:,-1),G4tensor(:,12))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,79),G3(:,:,:,16))
  call check_last_DV_C(l_switch,G3(:,:,:,16),Q(:,60),wf(:,61),G4tensor(:,13))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,-3),G3(:,:,:,17))
  call loop_DV_C(G3(:,:,:,17),Q(:,28),wf(:,-5),G4(:,:,:,6))
  call check_last_DV_C(l_switch,G4(:,:,:,6),Q(:,60),wf(:,61),G5tensor(:,4))
  call loop_DV_C(G3(:,:,:,17),Q(:,28),wf(:,113),G4(:,:,:,7))
  call check_last_DV_C(l_switch,G4(:,:,:,7),Q(:,61),wf(:,-1),G5tensor(:,5))
  call loop_DV_C(G3(:,:,:,17),Q(:,28),wf(:,99),G4(:,:,:,8))
  call check_last_DV_C(l_switch,G4(:,:,:,8),Q(:,62),wf(:,0),G5tensor(:,6))
  call loop_DV_C(G3(:,:,:,17),Q(:,28),wf(:,-1),G4(:,:,:,9))
  call loop_DV_C(G4(:,:,:,9),Q(:,30),wf(:,-5),G5(:,:,:,3))
  call check_last_DV_C(l_switch,G5(:,:,:,3),Q(:,62),wf(:,0),G6tensor(:,3))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,182),G3(:,:,:,18))
  call check_last_DV_C(l_switch,G3(:,:,:,18),Q(:,61),wf(:,-1),G4tensor(:,14))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,104),G3(:,:,:,19))
  call loop_DV_C(G3(:,:,:,19),Q(:,29),wf(:,-5),G4(:,:,:,10))
  call check_last_DV_C(l_switch,G4(:,:,:,10),Q(:,61),wf(:,-1),G5tensor(:,7))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,236),G3(:,:,:,20))
  call check_last_DV_C(l_switch,G3(:,:,:,20),Q(:,62),wf(:,0),G4tensor(:,15))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,91),G3(:,:,:,21))
  call loop_DV_C(G3(:,:,:,21),Q(:,30),wf(:,-5),G4(:,:,:,11))
  call check_last_DV_C(l_switch,G4(:,:,:,11),Q(:,62),wf(:,0),G5tensor(:,8))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,191),G3(:,:,:,22))
  call check_last_DV_C(l_switch,G3(:,:,:,22),Q(:,61),wf(:,-1),G4tensor(:,16))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,245),G3(:,:,:,23))
  call check_last_DV_C(l_switch,G3(:,:,:,23),Q(:,62),wf(:,0),G4tensor(:,17))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,192),G3(:,:,:,24))
  call check_last_DV_C(l_switch,G3(:,:,:,24),Q(:,61),wf(:,-1),G4tensor(:,18))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,246),G3(:,:,:,25))
  call check_last_DV_C(l_switch,G3(:,:,:,25),Q(:,62),wf(:,0),G4tensor(:,19))
  call loop_DV_C(G2(:,:,:,60),Q(:,20),wf(:,-1),G3(:,:,:,26))
  call loop_DV_C(G3(:,:,:,26),Q(:,22),wf(:,79),G4(:,:,:,12))
  call check_last_DV_C(l_switch,G4(:,:,:,12),Q(:,62),wf(:,0),G5tensor(:,9))
  call loop_DV_C(G3(:,:,:,26),Q(:,22),wf(:,-3),G4(:,:,:,13))
  call loop_DV_C(G4(:,:,:,13),Q(:,30),wf(:,-5),G5(:,:,:,4))
  call check_last_DV_C(l_switch,G5(:,:,:,4),Q(:,62),wf(:,0),G6tensor(:,4))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,44),G2(:,:,:,61))
  call check_last_DV_C(l_switch,G2(:,:,:,61),Q(:,54),wf(:,104),G3tensor(:,103))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,47),G2(:,:,:,62))
  call check_last_DV_C(l_switch,G2(:,:,:,62),Q(:,54),wf(:,104),G3tensor(:,104))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,48),G2(:,:,:,63))
  call check_last_DV_C(l_switch,G2(:,:,:,63),Q(:,54),wf(:,104),G3tensor(:,105))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,187),G2(:,:,:,64))
  call check_last_DV_C(l_switch,G2(:,:,:,64),Q(:,55),wf(:,-3),G3tensor(:,106))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,188),G2(:,:,:,65))
  call check_last_DV_C(l_switch,G2(:,:,:,65),Q(:,55),wf(:,-3),G3tensor(:,107))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,189),G2(:,:,:,66))
  call check_last_DV_C(l_switch,G2(:,:,:,66),Q(:,55),wf(:,-3),G3tensor(:,108))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,994),G2(:,:,:,67))
  call check_last_DV_C(l_switch,G2(:,:,:,67),Q(:,62),wf(:,0),G3tensor(:,109))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,995),G2(:,:,:,68))
  call check_last_DV_C(l_switch,G2(:,:,:,68),Q(:,62),wf(:,0),G3tensor(:,110))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,996),G2(:,:,:,69))
  call check_last_DV_C(l_switch,G2(:,:,:,69),Q(:,62),wf(:,0),G3tensor(:,111))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1003),G2(:,:,:,70))
  call check_last_DV_C(l_switch,G2(:,:,:,70),Q(:,55),wf(:,-3),G3tensor(:,112))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1004),G2(:,:,:,71))
  call check_last_DV_C(l_switch,G2(:,:,:,71),Q(:,55),wf(:,-3),G3tensor(:,113))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1005),G2(:,:,:,72))
  call check_last_DV_C(l_switch,G2(:,:,:,72),Q(:,55),wf(:,-3),G3tensor(:,114))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,997),G2(:,:,:,73))
  call check_last_DV_C(l_switch,G2(:,:,:,73),Q(:,62),wf(:,0),G3tensor(:,115))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,998),G2(:,:,:,74))
  call check_last_DV_C(l_switch,G2(:,:,:,74),Q(:,62),wf(:,0),G3tensor(:,116))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,999),G2(:,:,:,75))
  call check_last_DV_C(l_switch,G2(:,:,:,75),Q(:,62),wf(:,0),G3tensor(:,117))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,-3),G2(:,:,:,76))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,44),G3(:,:,:,27))
  call check_last_DV_C(l_switch,G3(:,:,:,27),Q(:,62),wf(:,0),G4tensor(:,20))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,47),G3(:,:,:,28))
  call check_last_DV_C(l_switch,G3(:,:,:,28),Q(:,62),wf(:,0),G4tensor(:,21))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,48),G3(:,:,:,29))
  call check_last_DV_C(l_switch,G3(:,:,:,29),Q(:,62),wf(:,0),G4tensor(:,22))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,37),G3(:,:,:,30))
  call check_last_DV_C(l_switch,G3(:,:,:,30),Q(:,61),wf(:,-1),G4tensor(:,23))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,39),G3(:,:,:,31))
  call check_last_DV_C(l_switch,G3(:,:,:,31),Q(:,61),wf(:,-1),G4tensor(:,24))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,40),G3(:,:,:,32))
  call check_last_DV_C(l_switch,G3(:,:,:,32),Q(:,61),wf(:,-1),G4tensor(:,25))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,19),G3(:,:,:,33))
  call check_last_DV_C(l_switch,G3(:,:,:,33),Q(:,59),wf(:,-2),G4tensor(:,26))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,21),G3(:,:,:,34))
  call check_last_DV_C(l_switch,G3(:,:,:,34),Q(:,59),wf(:,-2),G4tensor(:,27))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,22),G3(:,:,:,35))
  call check_last_DV_C(l_switch,G3(:,:,:,35),Q(:,59),wf(:,-2),G4tensor(:,28))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,-5),G3(:,:,:,36))
  call check_last_DV_C(l_switch,G3(:,:,:,36),Q(:,56),wf(:,1),G4tensor(:,29))
  call check_last_DV_C(l_switch,G3(:,:,:,36),Q(:,56),wf(:,3),G4tensor(:,30))
  call check_last_DV_C(l_switch,G3(:,:,:,36),Q(:,56),wf(:,4),G4tensor(:,31))
  call check_last_DV_C(l_switch,G3(:,:,:,36),Q(:,56),wf(:,74),G4tensor(:,32))
  call check_last_DV_C(l_switch,G3(:,:,:,36),Q(:,56),wf(:,103),G4tensor(:,33))
  call check_last_DV_C(l_switch,G3(:,:,:,36),Q(:,56),wf(:,117),G4tensor(:,34))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,70),G3(:,:,:,37))
  call check_last_DV_C(l_switch,G3(:,:,:,37),Q(:,60),wf(:,61),G4tensor(:,35))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,89),G3(:,:,:,38))
  call check_last_DV_C(l_switch,G3(:,:,:,38),Q(:,59),wf(:,-2),G4tensor(:,36))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,-2),G3(:,:,:,39))
  call loop_DV_C(G3(:,:,:,39),Q(:,28),wf(:,-5),G4(:,:,:,14))
  call check_last_DV_C(l_switch,G4(:,:,:,14),Q(:,60),wf(:,61),G5tensor(:,10))
  call loop_DV_C(G3(:,:,:,39),Q(:,28),wf(:,113),G4(:,:,:,15))
  call check_last_DV_C(l_switch,G4(:,:,:,15),Q(:,61),wf(:,-1),G5tensor(:,11))
  call loop_DV_C(G3(:,:,:,39),Q(:,28),wf(:,99),G4(:,:,:,16))
  call check_last_DV_C(l_switch,G4(:,:,:,16),Q(:,62),wf(:,0),G5tensor(:,12))
  call loop_DV_C(G3(:,:,:,39),Q(:,28),wf(:,-1),G4(:,:,:,17))
  call loop_DV_C(G4(:,:,:,17),Q(:,30),wf(:,-5),G5(:,:,:,5))
  call check_last_DV_C(l_switch,G5(:,:,:,5),Q(:,62),wf(:,0),G6tensor(:,5))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,99),G3(:,:,:,40))
  call check_last_DV_C(l_switch,G3(:,:,:,40),Q(:,58),wf(:,90),G4tensor(:,37))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,153),G3(:,:,:,41))
  call check_last_DV_C(l_switch,G3(:,:,:,41),Q(:,61),wf(:,-1),G4tensor(:,38))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,90),G3(:,:,:,42))
  call loop_DV_C(G3(:,:,:,42),Q(:,29),wf(:,-5),G4(:,:,:,18))
  call check_last_DV_C(l_switch,G4(:,:,:,18),Q(:,61),wf(:,-1),G5tensor(:,13))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,113),G3(:,:,:,43))
  call check_last_DV_C(l_switch,G3(:,:,:,43),Q(:,57),wf(:,105),G4tensor(:,39))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,207),G3(:,:,:,44))
  call check_last_DV_C(l_switch,G3(:,:,:,44),Q(:,62),wf(:,0),G4tensor(:,40))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,105),G3(:,:,:,45))
  call loop_DV_C(G3(:,:,:,45),Q(:,30),wf(:,-5),G4(:,:,:,19))
  call check_last_DV_C(l_switch,G4(:,:,:,19),Q(:,62),wf(:,0),G5tensor(:,14))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,140),G3(:,:,:,46))
  call check_last_DV_C(l_switch,G3(:,:,:,46),Q(:,59),wf(:,-2),G4tensor(:,41))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,176),G3(:,:,:,47))
  call check_last_DV_C(l_switch,G3(:,:,:,47),Q(:,61),wf(:,-1),G4tensor(:,42))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,141),G3(:,:,:,48))
  call check_last_DV_C(l_switch,G3(:,:,:,48),Q(:,59),wf(:,-2),G4tensor(:,43))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,230),G3(:,:,:,49))
  call check_last_DV_C(l_switch,G3(:,:,:,49),Q(:,62),wf(:,0),G4tensor(:,44))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,177),G3(:,:,:,50))
  call check_last_DV_C(l_switch,G3(:,:,:,50),Q(:,61),wf(:,-1),G4tensor(:,45))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,231),G3(:,:,:,51))
  call check_last_DV_C(l_switch,G3(:,:,:,51),Q(:,62),wf(:,0),G4tensor(:,46))
  call loop_DV_C(G2(:,:,:,76),Q(:,24),wf(:,-1),G3(:,:,:,52))
  call loop_DV_C(G3(:,:,:,52),Q(:,26),wf(:,70),G4(:,:,:,20))
  call check_last_DV_C(l_switch,G4(:,:,:,20),Q(:,62),wf(:,0),G5tensor(:,15))
  call loop_DV_C(G3(:,:,:,52),Q(:,26),wf(:,-2),G4(:,:,:,21))
  call loop_DV_C(G4(:,:,:,21),Q(:,30),wf(:,-5),G5(:,:,:,6))
  call check_last_DV_C(l_switch,G5(:,:,:,6),Q(:,62),wf(:,0),G6tensor(:,6))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,113),G2(:,:,:,77))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,49),wf(:,56),G3tensor(:,118))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,49),wf(:,59),G3tensor(:,119))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,49),wf(:,60),G3tensor(:,120))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,49),wf(:,202),G3tensor(:,121))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,49),wf(:,214),G3tensor(:,122))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,49),wf(:,221),G3tensor(:,123))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1012),G2(:,:,:,78))
  call check_last_DV_C(l_switch,G2(:,:,:,78),Q(:,62),wf(:,0),G3tensor(:,124))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1013),G2(:,:,:,79))
  call check_last_DV_C(l_switch,G2(:,:,:,79),Q(:,62),wf(:,0),G3tensor(:,125))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1014),G2(:,:,:,80))
  call check_last_DV_C(l_switch,G2(:,:,:,80),Q(:,62),wf(:,0),G3tensor(:,126))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,56),G2(:,:,:,81))
  call loop_DV_C(G2(:,:,:,81),Q(:,30),wf(:,-5),G3(:,:,:,53))
  call check_last_DV_C(l_switch,G3(:,:,:,53),Q(:,62),wf(:,0),G4tensor(:,47))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,59),G2(:,:,:,82))
  call loop_DV_C(G2(:,:,:,82),Q(:,30),wf(:,-5),G3(:,:,:,54))
  call check_last_DV_C(l_switch,G3(:,:,:,54),Q(:,62),wf(:,0),G4tensor(:,48))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,60),G2(:,:,:,83))
  call loop_DV_C(G2(:,:,:,83),Q(:,30),wf(:,-5),G3(:,:,:,55))
  call check_last_DV_C(l_switch,G3(:,:,:,55),Q(:,62),wf(:,0),G4tensor(:,49))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,196),G2(:,:,:,84))
  call check_last_DV_C(l_switch,G2(:,:,:,84),Q(:,55),wf(:,-3),G3tensor(:,127))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,197),G2(:,:,:,85))
  call check_last_DV_C(l_switch,G2(:,:,:,85),Q(:,55),wf(:,-3),G3tensor(:,128))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,198),G2(:,:,:,86))
  call check_last_DV_C(l_switch,G2(:,:,:,86),Q(:,55),wf(:,-3),G3tensor(:,129))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,49),G2(:,:,:,87))
  call check_last_DV_C(l_switch,G2(:,:,:,87),Q(:,57),wf(:,105),G3tensor(:,130))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,51),G2(:,:,:,88))
  call check_last_DV_C(l_switch,G2(:,:,:,88),Q(:,57),wf(:,105),G3tensor(:,131))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,52),G2(:,:,:,89))
  call check_last_DV_C(l_switch,G2(:,:,:,89),Q(:,57),wf(:,105),G3tensor(:,132))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,208),G2(:,:,:,90))
  call check_last_DV_C(l_switch,G2(:,:,:,90),Q(:,59),wf(:,-2),G3tensor(:,133))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,209),G2(:,:,:,91))
  call check_last_DV_C(l_switch,G2(:,:,:,91),Q(:,59),wf(:,-2),G3tensor(:,134))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,210),G2(:,:,:,92))
  call check_last_DV_C(l_switch,G2(:,:,:,92),Q(:,59),wf(:,-2),G3tensor(:,135))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1063),G2(:,:,:,93))
  call check_last_DV_C(l_switch,G2(:,:,:,93),Q(:,61),wf(:,-1),G3tensor(:,136))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1064),G2(:,:,:,94))
  call check_last_DV_C(l_switch,G2(:,:,:,94),Q(:,61),wf(:,-1),G3tensor(:,137))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1065),G2(:,:,:,95))
  call check_last_DV_C(l_switch,G2(:,:,:,95),Q(:,61),wf(:,-1),G3tensor(:,138))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,226),G2(:,:,:,96))
  call check_last_DV_C(l_switch,G2(:,:,:,96),Q(:,59),wf(:,-2),G3tensor(:,139))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,227),G2(:,:,:,97))
  call check_last_DV_C(l_switch,G2(:,:,:,97),Q(:,59),wf(:,-2),G3tensor(:,140))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,228),G2(:,:,:,98))
  call check_last_DV_C(l_switch,G2(:,:,:,98),Q(:,59),wf(:,-2),G3tensor(:,141))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1072),G2(:,:,:,99))
  call check_last_DV_C(l_switch,G2(:,:,:,99),Q(:,61),wf(:,-1),G3tensor(:,142))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1073),G2(:,:,:,100))
  call check_last_DV_C(l_switch,G2(:,:,:,100),Q(:,61),wf(:,-1),G3tensor(:,143))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1074),G2(:,:,:,101))
  call check_last_DV_C(l_switch,G2(:,:,:,101),Q(:,61),wf(:,-1),G3tensor(:,144))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1081),G2(:,:,:,102))
  call check_last_DV_C(l_switch,G2(:,:,:,102),Q(:,59),wf(:,-2),G3tensor(:,145))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1082),G2(:,:,:,103))
  call check_last_DV_C(l_switch,G2(:,:,:,103),Q(:,59),wf(:,-2),G3tensor(:,146))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1083),G2(:,:,:,104))
  call check_last_DV_C(l_switch,G2(:,:,:,104),Q(:,59),wf(:,-2),G3tensor(:,147))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1075),G2(:,:,:,105))
  call check_last_DV_C(l_switch,G2(:,:,:,105),Q(:,61),wf(:,-1),G3tensor(:,148))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1076),G2(:,:,:,106))
  call check_last_DV_C(l_switch,G2(:,:,:,106),Q(:,61),wf(:,-1),G3tensor(:,149))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1077),G2(:,:,:,107))
  call check_last_DV_C(l_switch,G2(:,:,:,107),Q(:,61),wf(:,-1),G3tensor(:,150))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,37),G2(:,:,:,108))
  call check_last_DV_C(l_switch,G2(:,:,:,108),Q(:,53),wf(:,91),G3tensor(:,151))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,39),G2(:,:,:,109))
  call check_last_DV_C(l_switch,G2(:,:,:,109),Q(:,53),wf(:,91),G3tensor(:,152))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,40),G2(:,:,:,110))
  call check_last_DV_C(l_switch,G2(:,:,:,110),Q(:,53),wf(:,91),G3tensor(:,153))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,241),G2(:,:,:,111))
  call check_last_DV_C(l_switch,G2(:,:,:,111),Q(:,55),wf(:,-3),G3tensor(:,154))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,242),G2(:,:,:,112))
  call check_last_DV_C(l_switch,G2(:,:,:,112),Q(:,55),wf(:,-3),G3tensor(:,155))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,243),G2(:,:,:,113))
  call check_last_DV_C(l_switch,G2(:,:,:,113),Q(:,55),wf(:,-3),G3tensor(:,156))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1099),G2(:,:,:,114))
  call check_last_DV_C(l_switch,G2(:,:,:,114),Q(:,61),wf(:,-1),G3tensor(:,157))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1100),G2(:,:,:,115))
  call check_last_DV_C(l_switch,G2(:,:,:,115),Q(:,61),wf(:,-1),G3tensor(:,158))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1101),G2(:,:,:,116))
  call check_last_DV_C(l_switch,G2(:,:,:,116),Q(:,61),wf(:,-1),G3tensor(:,159))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1108),G2(:,:,:,117))
  call check_last_DV_C(l_switch,G2(:,:,:,117),Q(:,55),wf(:,-3),G3tensor(:,160))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1109),G2(:,:,:,118))
  call check_last_DV_C(l_switch,G2(:,:,:,118),Q(:,55),wf(:,-3),G3tensor(:,161))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1110),G2(:,:,:,119))
  call check_last_DV_C(l_switch,G2(:,:,:,119),Q(:,55),wf(:,-3),G3tensor(:,162))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1102),G2(:,:,:,120))
  call check_last_DV_C(l_switch,G2(:,:,:,120),Q(:,61),wf(:,-1),G3tensor(:,163))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1103),G2(:,:,:,121))
  call check_last_DV_C(l_switch,G2(:,:,:,121),Q(:,61),wf(:,-1),G3tensor(:,164))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1104),G2(:,:,:,122))
  call check_last_DV_C(l_switch,G2(:,:,:,122),Q(:,61),wf(:,-1),G3tensor(:,165))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,99),G2(:,:,:,123))
  call check_last_DV_C(l_switch,G2(:,:,:,123),Q(:,50),wf(:,25),G3tensor(:,166))
  call check_last_DV_C(l_switch,G2(:,:,:,123),Q(:,50),wf(:,27),G3tensor(:,167))
  call check_last_DV_C(l_switch,G2(:,:,:,123),Q(:,50),wf(:,28),G3tensor(:,168))
  call check_last_DV_C(l_switch,G2(:,:,:,123),Q(:,50),wf(:,148),G3tensor(:,169))
  call check_last_DV_C(l_switch,G2(:,:,:,123),Q(:,50),wf(:,160),G3tensor(:,170))
  call check_last_DV_C(l_switch,G2(:,:,:,123),Q(:,50),wf(:,167),G3tensor(:,171))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1117),G2(:,:,:,124))
  call check_last_DV_C(l_switch,G2(:,:,:,124),Q(:,61),wf(:,-1),G3tensor(:,172))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1118),G2(:,:,:,125))
  call check_last_DV_C(l_switch,G2(:,:,:,125),Q(:,61),wf(:,-1),G3tensor(:,173))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1119),G2(:,:,:,126))
  call check_last_DV_C(l_switch,G2(:,:,:,126),Q(:,61),wf(:,-1),G3tensor(:,174))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,25),G2(:,:,:,127))
  call loop_DV_C(G2(:,:,:,127),Q(:,29),wf(:,-5),G3(:,:,:,56))
  call check_last_DV_C(l_switch,G3(:,:,:,56),Q(:,61),wf(:,-1),G4tensor(:,50))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,27),G2(:,:,:,128))
  call loop_DV_C(G2(:,:,:,128),Q(:,29),wf(:,-5),G3(:,:,:,57))
  call check_last_DV_C(l_switch,G3(:,:,:,57),Q(:,61),wf(:,-1),G4tensor(:,51))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,28),G2(:,:,:,129))
  call loop_DV_C(G2(:,:,:,129),Q(:,29),wf(:,-5),G3(:,:,:,58))
  call check_last_DV_C(l_switch,G3(:,:,:,58),Q(:,61),wf(:,-1),G4tensor(:,52))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,19),G2(:,:,:,130))
  call check_last_DV_C(l_switch,G2(:,:,:,130),Q(:,51),wf(:,62),G3tensor(:,175))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,21),G2(:,:,:,131))
  call check_last_DV_C(l_switch,G2(:,:,:,131),Q(:,51),wf(:,62),G3tensor(:,176))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,22),G2(:,:,:,132))
  call check_last_DV_C(l_switch,G2(:,:,:,132),Q(:,51),wf(:,62),G3tensor(:,177))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,259),G2(:,:,:,133))
  call check_last_DV_C(l_switch,G2(:,:,:,133),Q(:,55),wf(:,-3),G3tensor(:,178))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,260),G2(:,:,:,134))
  call check_last_DV_C(l_switch,G2(:,:,:,134),Q(:,55),wf(:,-3),G3tensor(:,179))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,261),G2(:,:,:,135))
  call check_last_DV_C(l_switch,G2(:,:,:,135),Q(:,55),wf(:,-3),G3tensor(:,180))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1135),G2(:,:,:,136))
  call check_last_DV_C(l_switch,G2(:,:,:,136),Q(:,59),wf(:,-2),G3tensor(:,181))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1136),G2(:,:,:,137))
  call check_last_DV_C(l_switch,G2(:,:,:,137),Q(:,59),wf(:,-2),G3tensor(:,182))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1137),G2(:,:,:,138))
  call check_last_DV_C(l_switch,G2(:,:,:,138),Q(:,59),wf(:,-2),G3tensor(:,183))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1144),G2(:,:,:,139))
  call check_last_DV_C(l_switch,G2(:,:,:,139),Q(:,55),wf(:,-3),G3tensor(:,184))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1145),G2(:,:,:,140))
  call check_last_DV_C(l_switch,G2(:,:,:,140),Q(:,55),wf(:,-3),G3tensor(:,185))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1146),G2(:,:,:,141))
  call check_last_DV_C(l_switch,G2(:,:,:,141),Q(:,55),wf(:,-3),G3tensor(:,186))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1138),G2(:,:,:,142))
  call check_last_DV_C(l_switch,G2(:,:,:,142),Q(:,59),wf(:,-2),G3tensor(:,187))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1139),G2(:,:,:,143))
  call check_last_DV_C(l_switch,G2(:,:,:,143),Q(:,59),wf(:,-2),G3tensor(:,188))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1140),G2(:,:,:,144))
  call check_last_DV_C(l_switch,G2(:,:,:,144),Q(:,59),wf(:,-2),G3tensor(:,189))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,70),G2(:,:,:,145))
  call check_last_DV_C(l_switch,G2(:,:,:,145),Q(:,52),wf(:,7),G3tensor(:,190))
  call check_last_DV_C(l_switch,G2(:,:,:,145),Q(:,52),wf(:,9),G3tensor(:,191))
  call check_last_DV_C(l_switch,G2(:,:,:,145),Q(:,52),wf(:,10),G3tensor(:,192))
  call check_last_DV_C(l_switch,G2(:,:,:,145),Q(:,52),wf(:,83),G3tensor(:,193))
  call check_last_DV_C(l_switch,G2(:,:,:,145),Q(:,52),wf(:,124),G3tensor(:,194))
  call check_last_DV_C(l_switch,G2(:,:,:,145),Q(:,52),wf(:,131),G3tensor(:,195))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1153),G2(:,:,:,146))
  call check_last_DV_C(l_switch,G2(:,:,:,146),Q(:,59),wf(:,-2),G3tensor(:,196))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1154),G2(:,:,:,147))
  call check_last_DV_C(l_switch,G2(:,:,:,147),Q(:,59),wf(:,-2),G3tensor(:,197))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1155),G2(:,:,:,148))
  call check_last_DV_C(l_switch,G2(:,:,:,148),Q(:,59),wf(:,-2),G3tensor(:,198))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,7),G2(:,:,:,149))
  call loop_DV_C(G2(:,:,:,149),Q(:,27),wf(:,-5),G3(:,:,:,59))
  call check_last_DV_C(l_switch,G3(:,:,:,59),Q(:,59),wf(:,-2),G4tensor(:,53))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,9),G2(:,:,:,150))
  call loop_DV_C(G2(:,:,:,150),Q(:,27),wf(:,-5),G3(:,:,:,60))
  call check_last_DV_C(l_switch,G3(:,:,:,60),Q(:,59),wf(:,-2),G4tensor(:,54))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,10),G2(:,:,:,151))
  call loop_DV_C(G2(:,:,:,151),Q(:,27),wf(:,-5),G3(:,:,:,61))
  call check_last_DV_C(l_switch,G3(:,:,:,61),Q(:,59),wf(:,-2),G4tensor(:,55))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,79),G2(:,:,:,152))
  call check_last_DV_C(l_switch,G2(:,:,:,152),Q(:,56),wf(:,1),G3tensor(:,199))
  call check_last_DV_C(l_switch,G2(:,:,:,152),Q(:,56),wf(:,3),G3tensor(:,200))
  call check_last_DV_C(l_switch,G2(:,:,:,152),Q(:,56),wf(:,4),G3tensor(:,201))
  call check_last_DV_C(l_switch,G2(:,:,:,152),Q(:,56),wf(:,74),G3tensor(:,202))
  call check_last_DV_C(l_switch,G2(:,:,:,152),Q(:,56),wf(:,103),G3tensor(:,203))
  call check_last_DV_C(l_switch,G2(:,:,:,152),Q(:,56),wf(:,117),G3tensor(:,204))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1162),G2(:,:,:,153))
  call check_last_DV_C(l_switch,G2(:,:,:,153),Q(:,55),wf(:,-3),G3tensor(:,205))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1163),G2(:,:,:,154))
  call check_last_DV_C(l_switch,G2(:,:,:,154),Q(:,55),wf(:,-3),G3tensor(:,206))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1164),G2(:,:,:,155))
  call check_last_DV_C(l_switch,G2(:,:,:,155),Q(:,55),wf(:,-3),G3tensor(:,207))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,89),G2(:,:,:,156))
  call check_last_DV_C(l_switch,G2(:,:,:,156),Q(:,51),wf(:,62),G3tensor(:,208))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,254),G2(:,:,:,157))
  call check_last_DV_C(l_switch,G2(:,:,:,157),Q(:,60),wf(:,61),G3tensor(:,209))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,62),G2(:,:,:,158))
  call loop_DV_C(G2(:,:,:,158),Q(:,28),wf(:,-5),G3(:,:,:,62))
  call check_last_DV_C(l_switch,G3(:,:,:,62),Q(:,60),wf(:,61),G4tensor(:,56))
  call loop_DV_C(G2(:,:,:,158),Q(:,28),wf(:,113),G3(:,:,:,63))
  call check_last_DV_C(l_switch,G3(:,:,:,63),Q(:,61),wf(:,-1),G4tensor(:,57))
  call loop_DV_C(G2(:,:,:,158),Q(:,28),wf(:,99),G3(:,:,:,64))
  call check_last_DV_C(l_switch,G3(:,:,:,64),Q(:,62),wf(:,0),G4tensor(:,58))
  call loop_DV_C(G2(:,:,:,158),Q(:,28),wf(:,-1),G3(:,:,:,65))
  call loop_DV_C(G3(:,:,:,65),Q(:,30),wf(:,-5),G4(:,:,:,22))
  call check_last_DV_C(l_switch,G4(:,:,:,22),Q(:,62),wf(:,0),G5tensor(:,16))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,263),G2(:,:,:,159))
  call check_last_DV_C(l_switch,G2(:,:,:,159),Q(:,60),wf(:,61),G3tensor(:,210))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,273),G2(:,:,:,160))
  call check_last_DV_C(l_switch,G2(:,:,:,160),Q(:,55),wf(:,-3),G3tensor(:,211))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,264),G2(:,:,:,161))
  call check_last_DV_C(l_switch,G2(:,:,:,161),Q(:,60),wf(:,61),G3tensor(:,212))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1195),G2(:,:,:,162))
  call check_last_DV_C(l_switch,G2(:,:,:,162),Q(:,59),wf(:,-2),G3tensor(:,213))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1323),G2(:,:,:,163))
  call check_last_DV_C(l_switch,G2(:,:,:,163),Q(:,55),wf(:,-3),G3tensor(:,214))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1326),G2(:,:,:,164))
  call check_last_DV_C(l_switch,G2(:,:,:,164),Q(:,59),wf(:,-2),G3tensor(:,215))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,83),G2(:,:,:,165))
  call loop_DV_C(G2(:,:,:,165),Q(:,27),wf(:,-5),G3(:,:,:,66))
  call check_last_DV_C(l_switch,G3(:,:,:,66),Q(:,59),wf(:,-2),G4tensor(:,59))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1330),G2(:,:,:,166))
  call check_last_DV_C(l_switch,G2(:,:,:,166),Q(:,55),wf(:,-3),G3tensor(:,216))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1331),G2(:,:,:,167))
  call check_last_DV_C(l_switch,G2(:,:,:,167),Q(:,59),wf(:,-2),G3tensor(:,217))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,153),G2(:,:,:,168))
  call check_last_DV_C(l_switch,G2(:,:,:,168),Q(:,53),wf(:,91),G3tensor(:,218))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,236),G2(:,:,:,169))
  call check_last_DV_C(l_switch,G2(:,:,:,169),Q(:,58),wf(:,90),G3tensor(:,219))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,91),G2(:,:,:,170))
  call loop_DV_C(G2(:,:,:,170),Q(:,26),wf(:,-5),G3(:,:,:,67))
  call check_last_DV_C(l_switch,G3(:,:,:,67),Q(:,58),wf(:,90),G4tensor(:,60))
  call loop_DV_C(G2(:,:,:,170),Q(:,26),wf(:,113),G3(:,:,:,68))
  call check_last_DV_C(l_switch,G3(:,:,:,68),Q(:,59),wf(:,-2),G4tensor(:,61))
  call loop_DV_C(G2(:,:,:,170),Q(:,26),wf(:,70),G3(:,:,:,69))
  call check_last_DV_C(l_switch,G3(:,:,:,69),Q(:,62),wf(:,0),G4tensor(:,62))
  call loop_DV_C(G2(:,:,:,170),Q(:,26),wf(:,-2),G3(:,:,:,70))
  call loop_DV_C(G3(:,:,:,70),Q(:,30),wf(:,-5),G4(:,:,:,23))
  call check_last_DV_C(l_switch,G4(:,:,:,23),Q(:,62),wf(:,0),G5tensor(:,17))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,245),G2(:,:,:,171))
  call check_last_DV_C(l_switch,G2(:,:,:,171),Q(:,58),wf(:,90),G3tensor(:,220))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,276),G2(:,:,:,172))
  call check_last_DV_C(l_switch,G2(:,:,:,172),Q(:,55),wf(:,-3),G3tensor(:,221))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,246),G2(:,:,:,173))
  call check_last_DV_C(l_switch,G2(:,:,:,173),Q(:,58),wf(:,90),G3tensor(:,222))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1226),G2(:,:,:,174))
  call check_last_DV_C(l_switch,G2(:,:,:,174),Q(:,61),wf(:,-1),G3tensor(:,223))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,90),G2(:,:,:,175))
  call loop_DV_C(G2(:,:,:,175),Q(:,21),wf(:,79),G3(:,:,:,71))
  call check_last_DV_C(l_switch,G3(:,:,:,71),Q(:,61),wf(:,-1),G4tensor(:,63))
  call loop_DV_C(G2(:,:,:,175),Q(:,21),wf(:,-3),G3(:,:,:,72))
  call loop_DV_C(G3(:,:,:,72),Q(:,29),wf(:,-5),G4(:,:,:,24))
  call check_last_DV_C(l_switch,G4(:,:,:,24),Q(:,61),wf(:,-1),G5tensor(:,18))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1335),G2(:,:,:,176))
  call check_last_DV_C(l_switch,G2(:,:,:,176),Q(:,55),wf(:,-3),G3tensor(:,224))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1338),G2(:,:,:,177))
  call check_last_DV_C(l_switch,G2(:,:,:,177),Q(:,61),wf(:,-1),G3tensor(:,225))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,148),G2(:,:,:,178))
  call loop_DV_C(G2(:,:,:,178),Q(:,29),wf(:,-5),G3(:,:,:,73))
  call check_last_DV_C(l_switch,G3(:,:,:,73),Q(:,61),wf(:,-1),G4tensor(:,64))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1342),G2(:,:,:,179))
  call check_last_DV_C(l_switch,G2(:,:,:,179),Q(:,55),wf(:,-3),G3tensor(:,226))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1343),G2(:,:,:,180))
  call check_last_DV_C(l_switch,G2(:,:,:,180),Q(:,61),wf(:,-1),G3tensor(:,227))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,182),G2(:,:,:,181))
  call check_last_DV_C(l_switch,G2(:,:,:,181),Q(:,57),wf(:,105),G3tensor(:,228))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,207),G2(:,:,:,182))
  call check_last_DV_C(l_switch,G2(:,:,:,182),Q(:,54),wf(:,104),G3tensor(:,229))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,104),G2(:,:,:,183))
  call loop_DV_C(G2(:,:,:,183),Q(:,25),wf(:,-5),G3(:,:,:,74))
  call check_last_DV_C(l_switch,G3(:,:,:,74),Q(:,57),wf(:,105),G4tensor(:,65))
  call loop_DV_C(G2(:,:,:,183),Q(:,25),wf(:,99),G3(:,:,:,75))
  call check_last_DV_C(l_switch,G3(:,:,:,75),Q(:,59),wf(:,-2),G4tensor(:,66))
  call loop_DV_C(G2(:,:,:,183),Q(:,25),wf(:,70),G3(:,:,:,76))
  call check_last_DV_C(l_switch,G3(:,:,:,76),Q(:,61),wf(:,-1),G4tensor(:,67))
  call loop_DV_C(G2(:,:,:,183),Q(:,25),wf(:,-2),G3(:,:,:,77))
  call loop_DV_C(G3(:,:,:,77),Q(:,29),wf(:,-5),G4(:,:,:,25))
  call check_last_DV_C(l_switch,G4(:,:,:,25),Q(:,61),wf(:,-1),G5tensor(:,19))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,191),G2(:,:,:,184))
  call check_last_DV_C(l_switch,G2(:,:,:,184),Q(:,57),wf(:,105),G3tensor(:,230))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,279),G2(:,:,:,185))
  call check_last_DV_C(l_switch,G2(:,:,:,185),Q(:,55),wf(:,-3),G3tensor(:,231))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,192),G2(:,:,:,186))
  call check_last_DV_C(l_switch,G2(:,:,:,186),Q(:,57),wf(:,105),G3tensor(:,232))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1253),G2(:,:,:,187))
  call check_last_DV_C(l_switch,G2(:,:,:,187),Q(:,62),wf(:,0),G3tensor(:,233))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,105),G2(:,:,:,188))
  call loop_DV_C(G2(:,:,:,188),Q(:,22),wf(:,79),G3(:,:,:,78))
  call check_last_DV_C(l_switch,G3(:,:,:,78),Q(:,62),wf(:,0),G4tensor(:,68))
  call loop_DV_C(G2(:,:,:,188),Q(:,22),wf(:,-3),G3(:,:,:,79))
  call loop_DV_C(G3(:,:,:,79),Q(:,30),wf(:,-5),G4(:,:,:,26))
  call check_last_DV_C(l_switch,G4(:,:,:,26),Q(:,62),wf(:,0),G5tensor(:,20))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1347),G2(:,:,:,189))
  call check_last_DV_C(l_switch,G2(:,:,:,189),Q(:,55),wf(:,-3),G3tensor(:,234))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1350),G2(:,:,:,190))
  call check_last_DV_C(l_switch,G2(:,:,:,190),Q(:,55),wf(:,-3),G3tensor(:,235))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1352),G2(:,:,:,191))
  call check_last_DV_C(l_switch,G2(:,:,:,191),Q(:,62),wf(:,0),G3tensor(:,236))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,202),G2(:,:,:,192))
  call loop_DV_C(G2(:,:,:,192),Q(:,30),wf(:,-5),G3(:,:,:,80))
  call check_last_DV_C(l_switch,G3(:,:,:,80),Q(:,62),wf(:,0),G4tensor(:,69))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1355),G2(:,:,:,193))
  call check_last_DV_C(l_switch,G2(:,:,:,193),Q(:,62),wf(:,0),G3tensor(:,237))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,230),G2(:,:,:,194))
  call check_last_DV_C(l_switch,G2(:,:,:,194),Q(:,54),wf(:,104),G3tensor(:,238))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,281),G2(:,:,:,195))
  call check_last_DV_C(l_switch,G2(:,:,:,195),Q(:,59),wf(:,-2),G3tensor(:,239))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,231),G2(:,:,:,196))
  call check_last_DV_C(l_switch,G2(:,:,:,196),Q(:,54),wf(:,104),G3tensor(:,240))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1274),G2(:,:,:,197))
  call check_last_DV_C(l_switch,G2(:,:,:,197),Q(:,61),wf(:,-1),G3tensor(:,241))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1359),G2(:,:,:,198))
  call check_last_DV_C(l_switch,G2(:,:,:,198),Q(:,59),wf(:,-2),G3tensor(:,242))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,124),G2(:,:,:,199))
  call loop_DV_C(G2(:,:,:,199),Q(:,27),wf(:,-5),G3(:,:,:,81))
  call check_last_DV_C(l_switch,G3(:,:,:,81),Q(:,59),wf(:,-2),G4tensor(:,70))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1362),G2(:,:,:,200))
  call check_last_DV_C(l_switch,G2(:,:,:,200),Q(:,61),wf(:,-1),G3tensor(:,243))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,160),G2(:,:,:,201))
  call loop_DV_C(G2(:,:,:,201),Q(:,29),wf(:,-5),G3(:,:,:,82))
  call check_last_DV_C(l_switch,G3(:,:,:,82),Q(:,61),wf(:,-1),G4tensor(:,71))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1366),G2(:,:,:,202))
  call check_last_DV_C(l_switch,G2(:,:,:,202),Q(:,59),wf(:,-2),G3tensor(:,244))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1367),G2(:,:,:,203))
  call check_last_DV_C(l_switch,G2(:,:,:,203),Q(:,61),wf(:,-1),G3tensor(:,245))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,176),G2(:,:,:,204))
  call check_last_DV_C(l_switch,G2(:,:,:,204),Q(:,53),wf(:,91),G3tensor(:,246))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,283),G2(:,:,:,205))
  call check_last_DV_C(l_switch,G2(:,:,:,205),Q(:,59),wf(:,-2),G3tensor(:,247))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,177),G2(:,:,:,206))
  call check_last_DV_C(l_switch,G2(:,:,:,206),Q(:,53),wf(:,91),G3tensor(:,248))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1289),G2(:,:,:,207))
  call check_last_DV_C(l_switch,G2(:,:,:,207),Q(:,62),wf(:,0),G3tensor(:,249))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1371),G2(:,:,:,208))
  call check_last_DV_C(l_switch,G2(:,:,:,208),Q(:,59),wf(:,-2),G3tensor(:,250))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,131),G2(:,:,:,209))
  call loop_DV_C(G2(:,:,:,209),Q(:,27),wf(:,-5),G3(:,:,:,83))
  call check_last_DV_C(l_switch,G3(:,:,:,83),Q(:,59),wf(:,-2),G4tensor(:,72))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1374),G2(:,:,:,210))
  call check_last_DV_C(l_switch,G2(:,:,:,210),Q(:,59),wf(:,-2),G3tensor(:,251))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1376),G2(:,:,:,211))
  call check_last_DV_C(l_switch,G2(:,:,:,211),Q(:,62),wf(:,0),G3tensor(:,252))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,214),G2(:,:,:,212))
  call loop_DV_C(G2(:,:,:,212),Q(:,30),wf(:,-5),G3(:,:,:,84))
  call check_last_DV_C(l_switch,G3(:,:,:,84),Q(:,62),wf(:,0),G4tensor(:,73))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1379),G2(:,:,:,213))
  call check_last_DV_C(l_switch,G2(:,:,:,213),Q(:,62),wf(:,0),G3tensor(:,253))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,140),G2(:,:,:,214))
  call check_last_DV_C(l_switch,G2(:,:,:,214),Q(:,51),wf(:,62),G3tensor(:,254))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1312),G2(:,:,:,215))
  call check_last_DV_C(l_switch,G2(:,:,:,215),Q(:,61),wf(:,-1),G3tensor(:,255))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,141),G2(:,:,:,216))
  call check_last_DV_C(l_switch,G2(:,:,:,216),Q(:,51),wf(:,62),G3tensor(:,256))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1305),G2(:,:,:,217))
  call check_last_DV_C(l_switch,G2(:,:,:,217),Q(:,62),wf(:,0),G3tensor(:,257))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1384),G2(:,:,:,218))
  call check_last_DV_C(l_switch,G2(:,:,:,218),Q(:,61),wf(:,-1),G3tensor(:,258))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,167),G2(:,:,:,219))
  call loop_DV_C(G2(:,:,:,219),Q(:,29),wf(:,-5),G3(:,:,:,85))
  call check_last_DV_C(l_switch,G3(:,:,:,85),Q(:,61),wf(:,-1),G4tensor(:,74))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1386),G2(:,:,:,220))
  call check_last_DV_C(l_switch,G2(:,:,:,220),Q(:,61),wf(:,-1),G3tensor(:,259))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1388),G2(:,:,:,221))
  call check_last_DV_C(l_switch,G2(:,:,:,221),Q(:,62),wf(:,0),G3tensor(:,260))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,221),G2(:,:,:,222))
  call loop_DV_C(G2(:,:,:,222),Q(:,30),wf(:,-5),G3(:,:,:,86))
  call check_last_DV_C(l_switch,G3(:,:,:,86),Q(:,62),wf(:,0),G4tensor(:,75))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1390),G2(:,:,:,223))
  call check_last_DV_C(l_switch,G2(:,:,:,223),Q(:,62),wf(:,0),G3tensor(:,261))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1441),G2(:,:,:,224))
  call check_last_DV_C(l_switch,G2(:,:,:,224),Q(:,55),wf(:,-3),G3tensor(:,262))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1442),G2(:,:,:,225))
  call check_last_DV_C(l_switch,G2(:,:,:,225),Q(:,59),wf(:,-2),G3tensor(:,263))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1444),G2(:,:,:,226))
  call check_last_DV_C(l_switch,G2(:,:,:,226),Q(:,55),wf(:,-3),G3tensor(:,264))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1445),G2(:,:,:,227))
  call check_last_DV_C(l_switch,G2(:,:,:,227),Q(:,61),wf(:,-1),G3tensor(:,265))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1447),G2(:,:,:,228))
  call check_last_DV_C(l_switch,G2(:,:,:,228),Q(:,59),wf(:,-2),G3tensor(:,266))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1448),G2(:,:,:,229))
  call check_last_DV_C(l_switch,G2(:,:,:,229),Q(:,61),wf(:,-1),G3tensor(:,267))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1453),G2(:,:,:,230))
  call check_last_DV_C(l_switch,G2(:,:,:,230),Q(:,55),wf(:,-3),G3tensor(:,268))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1454),G2(:,:,:,231))
  call check_last_DV_C(l_switch,G2(:,:,:,231),Q(:,59),wf(:,-2),G3tensor(:,269))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1456),G2(:,:,:,232))
  call check_last_DV_C(l_switch,G2(:,:,:,232),Q(:,55),wf(:,-3),G3tensor(:,270))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1457),G2(:,:,:,233))
  call check_last_DV_C(l_switch,G2(:,:,:,233),Q(:,59),wf(:,-2),G3tensor(:,271))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1459),G2(:,:,:,234))
  call check_last_DV_C(l_switch,G2(:,:,:,234),Q(:,62),wf(:,0),G3tensor(:,272))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1461),G2(:,:,:,235))
  call check_last_DV_C(l_switch,G2(:,:,:,235),Q(:,62),wf(:,0),G3tensor(:,273))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1465),G2(:,:,:,236))
  call check_last_DV_C(l_switch,G2(:,:,:,236),Q(:,55),wf(:,-3),G3tensor(:,274))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1466),G2(:,:,:,237))
  call check_last_DV_C(l_switch,G2(:,:,:,237),Q(:,55),wf(:,-3),G3tensor(:,275))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1467),G2(:,:,:,238))
  call check_last_DV_C(l_switch,G2(:,:,:,238),Q(:,61),wf(:,-1),G3tensor(:,276))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1469),G2(:,:,:,239))
  call check_last_DV_C(l_switch,G2(:,:,:,239),Q(:,61),wf(:,-1),G3tensor(:,277))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1471),G2(:,:,:,240))
  call check_last_DV_C(l_switch,G2(:,:,:,240),Q(:,62),wf(:,0),G3tensor(:,278))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1473),G2(:,:,:,241))
  call check_last_DV_C(l_switch,G2(:,:,:,241),Q(:,62),wf(:,0),G3tensor(:,279))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1477),G2(:,:,:,242))
  call check_last_DV_C(l_switch,G2(:,:,:,242),Q(:,59),wf(:,-2),G3tensor(:,280))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1478),G2(:,:,:,243))
  call check_last_DV_C(l_switch,G2(:,:,:,243),Q(:,59),wf(:,-2),G3tensor(:,281))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1479),G2(:,:,:,244))
  call check_last_DV_C(l_switch,G2(:,:,:,244),Q(:,61),wf(:,-1),G3tensor(:,282))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1480),G2(:,:,:,245))
  call check_last_DV_C(l_switch,G2(:,:,:,245),Q(:,61),wf(:,-1),G3tensor(:,283))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1483),G2(:,:,:,246))
  call check_last_DV_C(l_switch,G2(:,:,:,246),Q(:,62),wf(:,0),G3tensor(:,284))
  call loop_DV_C(G1(:,:,:,1),Q(:,16),wf(:,1484),G2(:,:,:,247))
  call check_last_DV_C(l_switch,G2(:,:,:,247),Q(:,62),wf(:,0),G3tensor(:,285))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(314)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(314)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(314)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(333)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(6)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(333)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(333)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(346)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(346)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(346)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(6)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(29)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(29)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(29)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(6)*(M(148)-M(158)+M(161)-M(163)-M(165)+M(166)+M(168)+M(174)-M(177)-M(182)+M(192)+M(198)-M(201)-M(208)-M(232) &
    +M(246))) * den(376)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(6)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(376)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(6)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(376)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(403)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(403)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(403)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(423)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(6)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(423)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(6)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(423)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(432)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(6)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(432)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(6)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(432)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(6)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(452)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(6)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(452)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(6)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(452)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(443)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(6)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(443)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(443)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(454)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(454)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,74)
  Gcoeff = (c(6)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(454)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,75)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(447)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,76)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(447)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,77)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(447)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,78)
  Gcoeff = (c(5)*(-M(58)+M(70)+M(93)-M(96)-M(102)+M(108)+M(119)-M(121)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(165)+M(189)-M(203) &
    +M(205)+M(207)-M(208)+M(235)-M(241))) * den(28)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(165)-M(175)-M(199) &
    +M(203)-M(204)-M(206)+M(208)+M(241))) * den(28)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(5)*(M(63)-M(70)+M(75)-M(93)+M(105)-M(108)+M(111)-M(119)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(175)-M(189)+M(199) &
    +M(204)-M(205)+M(206)-M(207)-M(235))) * den(28)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(6)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(477)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,79)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(155)-M(157)-M(163)+M(169)-M(173)+M(174)+M(176)-M(178)+M(192)-M(216)+M(230)-M(232)-M(243) &
    +M(249))) * den(477)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,80)
  Gcoeff = (c(6)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(477)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,81)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(488)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(6)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(488)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(6)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(488)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(6)*(-M(142)+M(156)-M(162)-M(167)+M(169)+M(171)-M(172)-M(173)+M(175)+M(184)+M(206)-M(216)-M(222)+M(225)+M(230) &
    -M(240))) * den(66)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,82)
  Gcoeff = (c(6)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(66)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,83)
  Gcoeff = (c(6)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(66)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,84)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(498)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,85)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(498)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,86)
  Gcoeff = (c(6)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(498)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,87)
  Gcoeff = (c(6)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(507)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,88)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(507)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,89)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(507)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,90)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(527)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,91)
  Gcoeff = (c(6)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(527)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,92)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(527)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,93)
  Gcoeff = (c(6)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(518)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,94)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(518)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,95)
  Gcoeff = (c(6)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(518)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,96)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(529)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,97)
  Gcoeff = (c(6)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(529)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,98)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(529)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,99)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(522)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,100)
  Gcoeff = (c(6)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(522)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,101)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(522)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,102)
  Gcoeff = (c(5)*(-M(46)+M(67)+M(90)-M(95)-M(105)+M(108)+M(110)-M(111)-M(112)-M(118)+M(119)+M(124))+c(6)*(-M(141)+M(183)-M(209) &
    +M(211)+M(213)-M(214)+M(229)-M(239))) * den(65)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(141)-M(151)-M(197) &
    +M(209)-M(210)-M(212)+M(214)+M(239))) * den(65)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(151)-M(183)+M(197) &
    +M(210)-M(211)+M(212)-M(213)-M(229))) * den(65)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(540)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(540)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(540)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(83)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,103)
  Gcoeff = (c(6)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(83)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,104)
  Gcoeff = (c(6)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(83)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,105)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(557)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,106)
  Gcoeff = (c(6)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(557)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,107)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(557)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,108)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(548)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,109)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(548)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,110)
  Gcoeff = (c(6)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(548)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,111)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(559)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,112)
  Gcoeff = (c(6)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(559)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,113)
  Gcoeff = (c(6)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(559)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,114)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(552)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,115)
  Gcoeff = (c(6)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(552)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,116)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(552)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,117)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(89)-M(92)+M(102)+M(104)-M(105)-M(106)-M(111)-M(117)+M(121)+M(123))+c(6)*(-M(135)+M(159)-M(215) &
    +M(217)+M(219)-M(220)+M(227)-M(233))) * den(82)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,20)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(135)-M(149)-M(173) &
    +M(215)-M(216)-M(218)+M(220)+M(233))) * den(82)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,21)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(149)-M(159)+M(173) &
    +M(216)-M(217)+M(218)-M(219)-M(227))) * den(82)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,22)
  Gcoeff = (c(6)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(94)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,118)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(94)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,119)
  Gcoeff = (c(6)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(94)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,120)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(574)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(574)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(574)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(568)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,124)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(568)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,125)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(568)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,126)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(65)-M(68)-M(101)-M(102)+M(103)+M(105)+M(109)+M(111)-M(115)-M(121))+c(6)*(-M(203)+M(209)+M(215) &
    -M(217)-M(227)+M(233)+M(239)-M(241))) * den(92)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,47)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(203)-M(205)-M(211) &
    +M(217)+M(227)-M(229)-M(235)+M(241))) * den(92)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,48)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)-M(65)-M(103)-M(105)+M(107)+M(108)-M(109)-M(111)+M(113)+M(119))+c(6)*(M(205)-M(209)+M(211) &
    -M(215)+M(229)-M(233)+M(235)-M(239))) * den(92)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,49)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(603)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,127)
  Gcoeff = (c(6)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(603)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,128)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(603)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,129)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(614)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(6)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(614)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(614)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(6)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(100)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,130)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(100)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,131)
  Gcoeff = (c(6)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(100)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,132)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(624)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,133)
  Gcoeff = (c(6)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(624)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,134)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(624)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,135)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(633)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,136)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(633)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,137)
  Gcoeff = (c(6)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(633)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,138)
  Gcoeff = (c(6)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(653)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,139)
  Gcoeff = (c(6)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(653)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,140)
  Gcoeff = (c(6)*(-M(150)+M(151)-M(152)+M(153)+M(169)-M(170)-M(187)+M(188)+M(193)-M(194)-M(211)+M(212)+M(228)-M(229)+M(230) &
    -M(231))) * den(653)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,141)
  Gcoeff = (c(6)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(644)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,142)
  Gcoeff = (c(6)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(644)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,143)
  Gcoeff = (c(6)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(644)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,144)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(655)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,145)
  Gcoeff = (c(6)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(655)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,146)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(655)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,147)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(648)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,148)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(648)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,149)
  Gcoeff = (c(6)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(648)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,150)
  Gcoeff = (c(5)*(-M(51)-M(63)+M(67)+M(70)+M(73)-M(74)-M(75)-M(76)-M(88)+M(90)+M(93)+M(100))+c(6)*(-M(144)-M(168)+M(181)+M(187) &
    -M(198)-M(200)+M(231)+M(237))) * den(9)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(143)+M(144)-M(167)+M(168) &
    +M(198)+M(200)-M(240)-M(242))) * den(9)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(143)+M(167)-M(181)-M(187) &
    -M(231)-M(237)+M(240)+M(242))) * den(9)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(6)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(666)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(666)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(666)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(115)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,151)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(115)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,152)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(115)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,153)
  Gcoeff = (c(6)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(683)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,154)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(683)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,155)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(683)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,156)
  Gcoeff = (c(6)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(674)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,157)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(674)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,158)
  Gcoeff = (c(6)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(674)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,159)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(685)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,160)
  Gcoeff = (c(6)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(685)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,161)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(685)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,162)
  Gcoeff = (c(6)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(678)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,163)
  Gcoeff = (c(6)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(678)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,164)
  Gcoeff = (c(6)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(678)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,165)
  Gcoeff = (c(5)*(-M(50)+M(55)+M(58)+M(61)-M(62)-M(63)-M(64)-M(75)-M(87)+M(89)+M(96)+M(99))+c(6)*(-M(146)+M(157)+M(163)-M(174) &
    -M(176)-M(192)+M(232)+M(243))) * den(7)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,23)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(-M(145)+M(146)+M(174)+M(176) &
    -M(191)+M(192)-M(234)-M(236))) * den(7)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,24)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(M(145)-M(157)-M(163)+M(191) &
    -M(232)+M(234)+M(236)-M(243))) * den(7)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,25)
  Gcoeff = (c(6)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(124)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,166)
  Gcoeff = (c(6)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(124)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,167)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(124)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,168)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(700)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(6)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(700)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(700)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(6)*(M(148)-M(158)+M(161)-M(163)-M(165)+M(166)+M(168)+M(174)-M(177)-M(182)+M(192)+M(198)-M(201)-M(208)-M(232) &
    +M(246))) * den(694)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,172)
  Gcoeff = (c(6)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(694)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,173)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(694)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,174)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)-M(57)-M(58)+M(60)+M(63)+M(65)+M(72)+M(75)-M(84)-M(96))+c(6)*(-M(163)-M(165)+M(168)+M(174) &
    +M(192)+M(198)-M(208)-M(232))) * den(5)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,50)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(M(163)+M(165)-M(187)-M(189) &
    -M(207)+M(208)-M(231)+M(232))) * den(5)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,51)
  Gcoeff = (c(5)*(M(41)-M(53)-M(60)-M(63)-M(65)+M(68)+M(69)+M(70)-M(72)-M(75)+M(81)+M(93))+c(6)*(-M(168)-M(174)+M(187)+M(189) &
    -M(192)-M(198)+M(207)+M(231))) * den(5)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,52)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(705)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(705)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(705)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(127)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,175)
  Gcoeff = (c(6)*(M(149)-M(151)-M(153)+M(154)-M(163)+M(164)+M(187)-M(188)+M(211)-M(212)-M(217)+M(218)-M(227)+M(229)+M(231) &
    -M(232))) * den(127)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,176)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(127)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,177)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(722)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,178)
  Gcoeff = (c(6)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(722)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,179)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(722)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,180)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(713)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,181)
  Gcoeff = (c(6)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(713)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,182)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(713)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,183)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(724)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,184)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(164)-M(169)+M(170)-M(193)+M(194)+M(217)-M(218)+M(227)-M(228)-M(230) &
    +M(232))) * den(724)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,185)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(724)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,186)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(717)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,187)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(717)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,188)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(717)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,189)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(50)-M(51)-M(52)-M(62)-M(74)-M(86)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(150)-M(152) &
    -M(170)-M(194)+M(238)+M(244))) * den(4)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,26)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(150)+M(152)-M(169)+M(170) &
    -M(193)+M(194)-M(228)-M(230))) * den(4)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,27)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(169)+M(193) &
    +M(228)+M(230)-M(238)-M(244))) * den(4)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,28)
  Gcoeff = (c(6)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(136)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,190)
  Gcoeff = (c(6)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(136)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,191)
  Gcoeff = (c(6)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(136)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,192)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(739)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(739)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(739)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(733)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,196)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(733)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,197)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(733)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,198)
  Gcoeff = (c(5)*(M(41)-M(44)-M(45)-M(46)+M(48)+M(51)-M(56)+M(68)+M(71)+M(74)-M(83)-M(95))+c(6)*(-M(139)-M(141)+M(144)+M(150) &
    +M(194)+M(200)-M(214)-M(238))) * den(2)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,53)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(139)+M(141)-M(181)-M(183) &
    -M(213)+M(214)-M(237)+M(238))) * den(2)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,54)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(144)-M(150)+M(181)+M(183) &
    -M(194)-M(200)+M(213)+M(237))) * den(2)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,55)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(139)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,199)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(139)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,200)
  Gcoeff = (c(6)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(139)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,201)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(748)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(748)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(748)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(742)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,205)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(742)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,206)
  Gcoeff = (c(6)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(742)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,207)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(44)+M(47)+M(50)+M(56)+M(59)+M(62)-M(68)-M(80)-M(92))+c(6)*(-M(133)-M(135)+M(146)+M(152) &
    +M(170)+M(176)-M(220)-M(244))) * den(1)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,29)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(1)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,30)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(146)-M(152)+M(157)+M(159) &
    -M(170)-M(176)+M(219)+M(243))) * den(1)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,31)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(308)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,208)
  Gcoeff = (c(6)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(310)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,209)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(785)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(5)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56)-M(68))+c(6)*(-M(143)+M(145)-M(149)+M(151) &
    +M(212)-M(218)+M(236)-M(242))) * den(13)
  T4sum(1:70,99) = T4sum(1:70,99) + Gcoeff * G4tensor(:,56)
  Gcoeff = (c(6)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(156)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,193)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(158)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,210)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(812)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,211)
  Gcoeff = (c(5)*(M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62)-M(92))+c(6)*(-M(135)-M(145)+M(146)+M(149) &
    +M(176)+M(218)-M(220)-M(236))) * den(17)
  T4sum(1:70,92) = T4sum(1:70,92) + Gcoeff * G4tensor(:,35)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(159)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,202)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(161)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,212)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(797)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,213)
  Gcoeff = (c(5)*(-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(74)-M(95))+c(6)*(-M(141)-M(143)+M(144)+M(151) &
    +M(200)+M(212)-M(214)-M(242))) * den(23)
  T4sum(1:70,86) = T4sum(1:70,86) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(1156)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(1158)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,214)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(44)+M(47)+M(50)+M(56)+M(59)+M(62)-M(68)-M(80)-M(92))+c(6)*(-M(133)-M(135)+M(146)+M(152) &
    +M(170)+M(176)-M(220)-M(244))) * den(152)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,32)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(1159)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(1161)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,215)
  Gcoeff = (c(5)*(M(41)-M(44)-M(45)-M(46)+M(48)+M(51)-M(56)+M(68)+M(71)+M(74)-M(83)-M(95))+c(6)*(-M(139)-M(141)+M(144)+M(150) &
    +M(194)+M(200)-M(214)-M(238))) * den(147)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,59)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(1165)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,216)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(1166)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,217)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(50)-M(51)-M(52)-M(62)-M(74)-M(86)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(150)-M(152) &
    -M(170)-M(194)+M(238)+M(244))) * den(307)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,36)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42) &
    -M(43)+M(44)+M(45)-M(46)-M(47)-M(48)-M(49)+M(50)+M(51)+M(52)+M(56)-M(59)+M(62)-M(68)-M(71)+M(74)+M(80)+M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(143)+M(151)+M(212)-M(242))) * den(11)
  T5sum(1:126,169) = T5sum(1:126,169) + Gcoeff * G5tensor(:,4)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    -M(43)-M(44)+M(45)-M(46)-M(47)-M(48)-M(49)+M(50)+M(51)+M(52)-M(56)-M(59)+M(62)+M(68)-M(71)+M(74)+M(80)+M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(145)+M(149)+M(218)-M(236))) * den(11)
  T5sum(1:126,171) = T5sum(1:126,171) + Gcoeff * G5tensor(:,10)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(370)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,218)
  Gcoeff = (c(6)*(-M(142)+M(156)-M(162)-M(167)+M(169)+M(171)-M(172)-M(173)+M(175)+M(184)+M(206)-M(216)-M(222)+M(225)+M(230) &
    -M(240))) * den(372)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,219)
  Gcoeff = (c(6)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(846)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(5)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(44)-M(53)+M(56)-M(65))+c(6)*(-M(167)+M(169)-M(173)+M(175) &
    +M(206)-M(216)+M(230)-M(240))) * den(34)
  T4sum(1:70,126) = T4sum(1:70,126) + Gcoeff * G4tensor(:,60)
  Gcoeff = (c(6)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(180)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,169)
  Gcoeff = (c(6)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(182)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,220)
  Gcoeff = (c(6)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(873)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,221)
  Gcoeff = (c(5)*(M(11)-M(12)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(50)-M(55)+M(62)-M(89))+c(6)*(M(152)-M(159)-M(169)+M(170) &
    +M(173)+M(216)-M(219)-M(230))) * den(38)
  T4sum(1:70,119) = T4sum(1:70,119) + Gcoeff * G4tensor(:,37)
  Gcoeff = (c(6)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(183)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,203)
  Gcoeff = (c(6)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(185)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,222)
  Gcoeff = (c(6)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(858)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,223)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(37)+M(38)+M(39)-M(40)-M(58)+M(63)+M(75)-M(96))+c(6)*(-M(165)-M(167)+M(168)+M(175) &
    +M(198)+M(206)-M(208)-M(240))) * den(61)
  T4sum(1:70,59) = T4sum(1:70,59) + Gcoeff * G4tensor(:,63)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(1168)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(6)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(1170)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,224)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(146)-M(152)+M(157)+M(159) &
    -M(170)-M(176)+M(219)+M(243))) * den(176)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,33)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(1171)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(6)*(M(148)-M(158)+M(161)-M(163)-M(165)+M(166)+M(168)+M(174)-M(177)-M(182)+M(192)+M(198)-M(201)-M(208)-M(232) &
    +M(246))) * den(1173)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,225)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)-M(57)-M(58)+M(60)+M(63)+M(65)+M(72)+M(75)-M(84)-M(96))+c(6)*(-M(163)-M(165)+M(168)+M(174) &
    +M(192)+M(198)-M(208)-M(232))) * den(171)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,64)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(1177)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,226)
  Gcoeff = (c(6)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(1178)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,227)
  Gcoeff = (c(5)*(-M(50)+M(55)+M(58)+M(61)-M(62)-M(63)-M(64)-M(75)-M(87)+M(89)+M(96)+M(99))+c(6)*(-M(146)+M(157)+M(163)-M(174) &
    -M(176)-M(192)+M(232)+M(243))) * den(369)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,38)
  Gcoeff = (c(4)*(M(9)-M(10)-M(11)+M(12)-M(15)+M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)-M(50) &
    -M(53)-M(54)+M(55)+M(56)+M(57)+M(58)+M(59)-M(60)+M(61)-M(62)-M(63)-M(64)-M(65)-M(72)-M(75)-M(77)+M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(163)-M(174)-M(192)+M(232))) * den(32)
  T5sum(1:126,163) = T5sum(1:126,163) + Gcoeff * G5tensor(:,13)
  Gcoeff = (c(4)*(M(9)-M(10)-M(11)+M(12)-M(15)+M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(37)+M(38)+M(39)-M(40)-M(44)+M(47)-M(50) &
    +M(53)-M(54)+M(55)-M(56)-M(57)-M(58)+M(59)+M(60)+M(61)-M(62)+M(63)-M(64)+M(65)+M(72)+M(75)-M(77)-M(84)-M(87)+M(89)-M(96) &
    +M(99))+c(6)*(-M(165)+M(168)+M(198)-M(208))) * den(32)
  T5sum(1:126,164) = T5sum(1:126,164) + Gcoeff * G5tensor(:,18)
  Gcoeff = (c(6)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(399)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,228)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(401)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,229)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(904)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(5)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(19)-M(20)+M(41)-M(53)-M(65)+M(68))+c(6)*(-M(191)+M(193)-M(197)+M(199)+M(204) &
    -M(210)+M(228)-M(234))) * den(42)
  T4sum(1:70,135) = T4sum(1:70,135) + Gcoeff * G4tensor(:,65)
  Gcoeff = (c(6)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(205)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,230)
  Gcoeff = (c(6)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(206)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,121)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(931)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,231)
  Gcoeff = (c(5)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(13)+M(14)+M(43)-M(55)-M(89)+M(92))+c(6)*(M(133)-M(157)+M(191)-M(193)-M(228) &
    +M(234)-M(243)+M(244))) * den(46)
  T4sum(1:70,127) = T4sum(1:70,127) + Gcoeff * G4tensor(:,39)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(207)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,204)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(209)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,232)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(914)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,233)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(37)-M(38)-M(39)+M(40)-M(102)+M(105)+M(111)-M(121))+c(6)*(-M(197)+M(199)-M(203)+M(204) &
    +M(209)-M(210)+M(239)-M(241))) * den(97)
  T4sum(1:70,23) = T4sum(1:70,23) + Gcoeff * G4tensor(:,68)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(1180)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(1182)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,234)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(200)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,34)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(1183)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(1185)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,235)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(1187)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,236)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(65)-M(68)-M(101)-M(102)+M(103)+M(105)+M(109)+M(111)-M(115)-M(121))+c(6)*(-M(203)+M(209)+M(215) &
    -M(217)-M(227)+M(233)+M(239)-M(241))) * den(197)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,69)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(1190)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,237)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(89)-M(92)+M(102)+M(104)-M(105)-M(106)-M(111)-M(117)+M(121)+M(123))+c(6)*(-M(135)+M(159)-M(215) &
    +M(217)+M(219)-M(220)+M(227)-M(233))) * den(400)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,40)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(5)-M(6)+M(7)-M(8)+M(13)-M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)-M(43)-M(53) &
    -M(54)+M(55)-M(65)+M(68)-M(77)+M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(215)+M(217)+M(227)-M(233))) * den(41)
  T5sum(1:126,115) = T5sum(1:126,115) + Gcoeff * G5tensor(:,14)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(5)-M(6)+M(7)-M(8)+M(13)-M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42)-M(43)+M(53) &
    -M(54)+M(55)+M(65)-M(68)-M(77)+M(80)+M(89)-M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(-M(203)+M(209)+M(239)-M(241))) * den(41)
  T5sum(1:126,116) = T5sum(1:126,116) + Gcoeff * G5tensor(:,20)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(223)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,170)
  Gcoeff = (c(6)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(225)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,238)
  Gcoeff = (c(6)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(974)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,239)
  Gcoeff = (c(5)*(M(5)-M(6)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(51)-M(67)+M(74)-M(90))+c(6)*(M(150)-M(183)-M(193)+M(194)+M(197) &
    +M(210)-M(213)-M(228))) * den(49)
  T4sum(1:70,107) = T4sum(1:70,107) + Gcoeff * G4tensor(:,66)
  Gcoeff = (c(6)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(226)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,194)
  Gcoeff = (c(6)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(228)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,240)
  Gcoeff = (c(6)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(960)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,241)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)-M(31)+M(32)+M(35)-M(36)+M(63)-M(70)+M(75)-M(93))+c(6)*(M(174)-M(189)-M(191)+M(192)+M(199) &
    +M(204)-M(207)-M(234))) * den(70)
  T4sum(1:70,71) = T4sum(1:70,71) + Gcoeff * G4tensor(:,67)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(1192)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(1194)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,242)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(144)-M(150)+M(181)+M(183) &
    -M(194)-M(200)+M(213)+M(237))) * den(219)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,70)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(1195)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(1197)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,243)
  Gcoeff = (c(5)*(M(41)-M(53)-M(60)-M(63)-M(65)+M(68)+M(69)+M(70)-M(72)-M(75)+M(81)+M(93))+c(6)*(-M(168)-M(174)+M(187)+M(189) &
    -M(192)-M(198)+M(207)+M(231))) * den(214)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,71)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(1201)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,244)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(1202)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,245)
  Gcoeff = (c(5)*(-M(51)-M(63)+M(67)+M(70)+M(73)-M(74)-M(75)-M(76)-M(88)+M(90)+M(93)+M(100))+c(6)*(-M(144)-M(168)+M(181)+M(187) &
    -M(198)-M(200)+M(231)+M(237))) * den(398)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)-M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)+M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)-M(74)-M(75)-M(76)-M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(168)+M(187)-M(198)+M(231))) * den(40)
  T5sum(1:126,151) = T5sum(1:126,151) + Gcoeff * G5tensor(:,7)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)+M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)-M(74)+M(75)-M(76)-M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(M(174)-M(189)+M(192)-M(207))) * den(40)
  T5sum(1:126,152) = T5sum(1:126,152) + Gcoeff * G5tensor(:,19)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(242)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,246)
  Gcoeff = (c(6)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(243)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,122)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(1016)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,247)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(15)+M(16)+M(46)-M(67)-M(90)+M(95))+c(6)*(M(139)+M(167)-M(169)-M(181)-M(230) &
    -M(237)+M(238)+M(240))) * den(52)
  T4sum(1:70,112) = T4sum(1:70,112) + Gcoeff * G4tensor(:,61)
  Gcoeff = (c(6)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(244)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,195)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(246)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,248)
  Gcoeff = (c(6)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(1000)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,249)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)+M(31)-M(32)-M(35)+M(36)+M(105)-M(108)+M(111)-M(119))+c(6)*(-M(173)+M(175)-M(205)+M(206) &
    +M(215)-M(216)+M(233)-M(235))) * den(103)
  T4sum(1:70,35) = T4sum(1:70,35) + Gcoeff * G4tensor(:,62)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(1204)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(1206)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,250)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(237)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,72)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(1207)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(1209)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,251)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(1211)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,252)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)-M(65)-M(103)-M(105)+M(107)+M(108)-M(109)-M(111)+M(113)+M(119))+c(6)*(M(205)-M(209)+M(211) &
    -M(215)+M(229)-M(233)+M(235)-M(239))) * den(234)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,73)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(1214)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,253)
  Gcoeff = (c(5)*(-M(46)+M(67)+M(90)-M(95)-M(105)+M(108)+M(110)-M(111)-M(112)-M(118)+M(119)+M(124))+c(6)*(-M(141)+M(183)-M(209) &
    +M(211)+M(213)-M(214)+M(229)-M(239))) * den(371)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)-M(46) &
    -M(53)+M(56)-M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(209)+M(211)+M(229)-M(239))) * den(33)
  T5sum(1:126,103) = T5sum(1:126,103) + Gcoeff * G5tensor(:,8)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)-M(46) &
    +M(53)-M(56)+M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(205)+M(215)+M(233)-M(235))) * den(33)
  T5sum(1:126,104) = T5sum(1:126,104) + Gcoeff * G5tensor(:,17)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(260)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,254)
  Gcoeff = (c(6)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(261)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,123)
  Gcoeff = (c(6)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(1056)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,255)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(25)-M(26)-M(29)+M(30)+M(58)-M(70)-M(93)+M(96))+c(6)*(M(143)-M(145)+M(163)-M(187)-M(231) &
    +M(232)-M(236)+M(242))) * den(73)
  T4sum(1:70,76) = T4sum(1:70,76) + Gcoeff * G4tensor(:,57)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(263)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,256)
  Gcoeff = (c(6)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(264)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,171)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(1042)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,257)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(102)-M(108)-M(119)+M(121))+c(6)*(-M(149)+M(151)-M(211)+M(212) &
    +M(217)-M(218)+M(227)-M(229))) * den(106)
  T4sum(1:70,40) = T4sum(1:70,40) + Gcoeff * G4tensor(:,58)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(1216)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(1217)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(1219)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,258)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(-M(163)-M(165)+M(187)+M(189) &
    +M(207)-M(208)+M(231)-M(232))) * den(257)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,74)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(1221)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,259)
  Gcoeff = (c(6)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(1223)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,260)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(252)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,75)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(1225)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,261)
  Gcoeff = (c(5)*(-M(58)+M(70)+M(93)-M(96)-M(102)+M(108)+M(119)-M(121)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(165)+M(189)-M(203) &
    +M(205)+M(207)-M(208)+M(235)-M(241))) * den(309)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)-M(58)-M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(203)+M(205)+M(235)-M(241))) * den(12)
  T5sum(1:126,67) = T5sum(1:126,67) + Gcoeff * G5tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)-M(58)+M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(211)+M(217)+M(227)-M(229))) * den(12)
  T5sum(1:126,68) = T5sum(1:126,68) + Gcoeff * G5tensor(:,16)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(1276)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,262)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(1277)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,263)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(169)+M(193) &
    +M(228)+M(230)-M(238)-M(244))) * den(259)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,41)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(1279)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,264)
  Gcoeff = (c(6)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(1280)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,265)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(M(145)-M(157)-M(163)+M(191) &
    -M(232)+M(234)+M(236)-M(243))) * den(241)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,42)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(1282)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,266)
  Gcoeff = (c(6)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(1283)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,267)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(143)+M(167)-M(181)-M(187) &
    -M(231)-M(237)+M(240)+M(242))) * den(204)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,16)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)+M(49)-M(55) &
    +M(58)-M(61)-M(67)-M(70)-M(73)-M(79)+M(82)+M(85)-M(89)-M(90)-M(91)+M(92)-M(93)+M(94)+M(95)+M(96)+M(97)+M(98)-M(99)-M(100)) &
    +c(6)*(M(143)-M(187)-M(231)+M(242))) * den(45)
  T5sum(1:126,131) = T5sum(1:126,131) + Gcoeff * G5tensor(:,5)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)+M(43)+M(46)+M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)-M(73)-M(79)+M(82)+M(85)-M(89)-M(90)-M(91)+M(92)+M(93)+M(94)+M(95)-M(96)+M(97)+M(98)-M(99) &
    -M(100))+c(6)*(M(145)-M(163)-M(232)+M(236))) * den(45)
  T5sum(1:126,132) = T5sum(1:126,132) + Gcoeff * G5tensor(:,11)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(1288)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,268)
  Gcoeff = (c(6)*(-M(150)+M(151)-M(152)+M(153)+M(169)-M(170)-M(187)+M(188)+M(193)-M(194)-M(211)+M(212)+M(228)-M(229)+M(230) &
    -M(231))) * den(1289)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,269)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(150)-M(152)+M(169)-M(170) &
    +M(193)-M(194)+M(228)+M(230))) * den(262)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,43)
  Gcoeff = (c(6)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(1291)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,270)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(1292)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,271)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(1294)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,272)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(149)-M(159)+M(173) &
    +M(216)-M(217)+M(218)-M(219)-M(227))) * den(224)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,44)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(1296)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,273)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(151)-M(183)+M(197) &
    +M(210)-M(211)+M(212)-M(213)-M(229))) * den(181)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,17)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(151)-M(211)+M(212)-M(229))) * den(37)
  T5sum(1:126,83) = T5sum(1:126,83) + Gcoeff * G5tensor(:,6)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)-M(17)+M(18)+M(19)-M(20)+M(21)-M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(149)-M(217)+M(218)-M(227))) * den(37)
  T5sum(1:126,84) = T5sum(1:126,84) + Gcoeff * G5tensor(:,12)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(1300)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,274)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(1301)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,275)
  Gcoeff = (c(6)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(1302)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,276)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(145)-M(146)-M(174)-M(176) &
    +M(191)-M(192)+M(234)+M(236))) * den(245)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,45)
  Gcoeff = (c(6)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(1304)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,277)
  Gcoeff = (c(6)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(1306)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,278)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(227)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,46)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(1308)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,279)
  Gcoeff = (c(5)*(M(63)-M(70)+M(75)-M(93)+M(105)-M(108)+M(111)-M(119)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(175)-M(189)+M(199) &
    +M(204)-M(205)+M(206)-M(207)-M(235))) * den(157)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(27)+M(28)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(175)-M(205)+M(206)-M(235))) * den(16)
  T5sum(1:126,47) = T5sum(1:126,47) + Gcoeff * G5tensor(:,2)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(173)-M(215)+M(216)-M(233))) * den(16)
  T5sum(1:126,48) = T5sum(1:126,48) + Gcoeff * G5tensor(:,15)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(1312)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,280)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(1313)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,281)
  Gcoeff = (c(6)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(1314)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,282)
  Gcoeff = (c(6)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(1315)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,283)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(208)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,18)
  Gcoeff = (c(6)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(1318)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,284)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(1319)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,285)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(184)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,19)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(160)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(199)-M(203)+M(204)-M(241))) * den(22)
  T5sum(1:126,23) = T5sum(1:126,23) + Gcoeff * G5tensor(:,3)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(197)-M(209)+M(210)-M(239))) * den(22)
  T5sum(1:126,24) = T5sum(1:126,24) + Gcoeff * G5tensor(:,9)
  Gcoeff = (c(4)*(M(1)+M(3)+M(6)+M(8)+M(10)+M(11)+M(14)+M(15)+M(17)+M(19)+M(22)+M(24)+M(26)+M(27)+M(29)+M(31)+M(34)+M(36)+M(38) &
    +M(39)-M(42)-M(46)-M(53)-M(55)-M(58)-M(65)-M(66)-M(69)-M(78)-M(79)-M(80)-M(81)-M(82)-M(85)-M(89)-M(91)-M(94)-M(95)-M(96)-M(97) &
    -M(103)-M(104)-M(105)-M(109)-M(111)-M(112)-M(118)-M(123)-M(126)-M(128))+c(6)*(-M(209)-M(239)))
  T6sum(1:210,163) = T6sum(1:210,163) + Gcoeff * G6tensor(:,4)
  Gcoeff = (c(4)*(M(2)+M(4)+M(5)+M(7)+M(9)+M(12)+M(13)+M(16)+M(18)+M(20)+M(21)+M(23)+M(25)+M(28)+M(30)+M(32)+M(33)+M(35)+M(37) &
    +M(40)-M(43)-M(45)-M(53)-M(54)-M(57)-M(65)-M(67)-M(70)-M(77)-M(79)-M(82)-M(83)-M(84)-M(85)-M(90)-M(91)-M(92)-M(93)-M(94)-M(97) &
    -M(103)-M(105)-M(106)-M(109)-M(110)-M(111)-M(117)-M(124)-M(125)-M(130))+c(6)*(-M(215)-M(233)))
  T6sum(1:210,166) = T6sum(1:210,166) + Gcoeff * G6tensor(:,6)
  Gcoeff = (c(4)*(M(2)+M(4)+M(5)+M(7)+M(10)+M(11)+M(13)+M(15)+M(17)+M(20)+M(22)+M(24)+M(26)+M(27)+M(29)+M(31)+M(34)+M(36)+M(37) &
    +M(40)-M(41)-M(43)-M(46)-M(54)-M(58)-M(66)-M(68)-M(69)-M(77)-M(78)-M(79)-M(81)-M(82)-M(85)-M(91)-M(92)-M(94)-M(95)-M(96)-M(97) &
    -M(101)-M(102)-M(106)-M(112)-M(115)-M(117)-M(118)-M(121)-M(126)-M(128))+c(6)*(-M(203)-M(241)))
  T6sum(1:210,169) = T6sum(1:210,169) + Gcoeff * G6tensor(:,2)
  Gcoeff = (c(4)*(M(1)+M(3)+M(6)+M(8)+M(9)+M(12)+M(14)+M(16)+M(18)+M(19)+M(21)+M(23)+M(25)+M(28)+M(30)+M(32)+M(33)+M(35)+M(38) &
    +M(39)-M(41)-M(42)-M(45)-M(55)-M(57)-M(67)-M(68)-M(70)-M(79)-M(80)-M(82)-M(83)-M(84)-M(85)-M(89)-M(90)-M(91)-M(93)-M(94)-M(97) &
    -M(101)-M(102)-M(104)-M(110)-M(115)-M(121)-M(123)-M(124)-M(125)-M(130))+c(6)*(-M(217)-M(227)))
  T6sum(1:210,172) = T6sum(1:210,172) + Gcoeff * G6tensor(:,5)
  Gcoeff = (c(4)*(M(1)+M(4)+M(5)+M(8)+M(10)+M(11)+M(13)+M(15)+M(18)+M(20)+M(22)+M(23)+M(25)+M(28)+M(30)+M(31)+M(33)+M(36)+M(37) &
    +M(40)-M(43)-M(44)-M(46)-M(54)-M(56)-M(57)-M(66)-M(70)-M(77)-M(78)-M(79)-M(82)-M(84)-M(85)-M(91)-M(92)-M(93)-M(94)-M(95)-M(97) &
    -M(106)-M(107)-M(108)-M(112)-M(113)-M(117)-M(118)-M(119)-M(125)-M(130))+c(6)*(-M(205)-M(235)))
  T6sum(1:210,175) = T6sum(1:210,175) + Gcoeff * G6tensor(:,1)
  Gcoeff = (c(4)*(M(2)+M(3)+M(6)+M(7)+M(9)+M(12)+M(14)+M(16)+M(17)+M(19)+M(21)+M(24)+M(26)+M(27)+M(29)+M(32)+M(34)+M(35)+M(38) &
    +M(39)-M(42)-M(44)-M(45)-M(55)-M(56)-M(58)-M(67)-M(69)-M(79)-M(80)-M(81)-M(82)-M(83)-M(85)-M(89)-M(90)-M(91)-M(94)-M(96)-M(97) &
    -M(104)-M(107)-M(108)-M(110)-M(113)-M(119)-M(123)-M(124)-M(126)-M(128))+c(6)*(-M(211)-M(229)))
  T6sum(1:210,178) = T6sum(1:210,178) + Gcoeff * G6tensor(:,3)

end subroutine vamp_34

end module ol_vamp_34_ppjjjj_gggggg_1_/**/REALKIND
