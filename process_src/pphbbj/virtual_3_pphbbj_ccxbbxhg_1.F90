
module ol_vamp_3_pphbbj_ccxbbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_3(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphbbj_ccxbbxhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphbbj_ccxbbxhg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphbbj_ccxbbxhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphbbj_ccxbbxhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(4)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,15) :: G0
  complex(REALKIND), dimension(4,5,4,46) :: G1
  complex(REALKIND), dimension(15,36) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,45),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,44),MT,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,29),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),MT,G2tensor(:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,39),G1(:,:,:,3))
  call check_last_Q_A(l_switch,G1(:,:,:,3),Q(:,63),MT,G2tensor(:,2))
  call loop_QV_A(G0(:,:,:,1),wf(:,45),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,44),MB,G1(:,:,:,4))
  call loop_QV_A(G1(:,:,:,4),wf(:,29),G1(:,:,:,5))
  call check_last_Q_A(l_switch,G1(:,:,:,5),Q(:,63),MB,G2tensor(:,3))
  call loop_QV_A(G1(:,:,:,4),wf(:,39),G1(:,:,:,6))
  call check_last_Q_A(l_switch,G1(:,:,:,6),Q(:,63),MB,G2tensor(:,4))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,45),Q(:,44),G1(:,:,:,7))
  call check_last_CV_D(l_switch,G1(:,:,:,7),Q(:,44),wf(:,29),Q(:,19),G2tensor(:,5))
  call check_last_CV_D(l_switch,G1(:,:,:,7),Q(:,44),wf(:,39),Q(:,19),G2tensor(:,6))
  call loop_QV_A(G0(:,:,:,1),wf(:,46),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,44),ZERO,G1(:,:,:,8))
  call loop_QV_A(G1(:,:,:,8),wf(:,29),G1(:,:,:,9))
  call check_last_Q_A(l_switch,G1(:,:,:,9),Q(:,63),ZERO,G2tensor(:,7))
  call loop_QV_A(G1(:,:,:,8),wf(:,39),G1(:,:,:,10))
  call check_last_Q_A(l_switch,G1(:,:,:,10),Q(:,63),ZERO,G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,46),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,44),MC,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,29),G1(:,:,:,12))
  call check_last_Q_A(l_switch,G1(:,:,:,12),Q(:,63),MC,G2tensor(:,9))
  call loop_QV_A(G1(:,:,:,11),wf(:,39),G1(:,:,:,13))
  call check_last_Q_A(l_switch,G1(:,:,:,13),Q(:,63),MC,G2tensor(:,10))
  call loop_QV_A(G0(:,:,:,1),wf(:,46),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,44),MT,G1(:,:,:,14))
  call loop_QV_A(G1(:,:,:,14),wf(:,29),G1(:,:,:,15))
  call check_last_Q_A(l_switch,G1(:,:,:,15),Q(:,63),MT,G2tensor(:,11))
  call loop_QV_A(G1(:,:,:,14),wf(:,39),G1(:,:,:,16))
  call check_last_Q_A(l_switch,G1(:,:,:,16),Q(:,63),MT,G2tensor(:,12))
  call loop_QV_A(G0(:,:,:,1),wf(:,46),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,44),MB,G1(:,:,:,17))
  call loop_QV_A(G1(:,:,:,17),wf(:,29),G1(:,:,:,18))
  call check_last_Q_A(l_switch,G1(:,:,:,18),Q(:,63),MB,G2tensor(:,13))
  call loop_QV_A(G1(:,:,:,17),wf(:,39),G1(:,:,:,19))
  call check_last_Q_A(l_switch,G1(:,:,:,19),Q(:,63),MB,G2tensor(:,14))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,46),Q(:,44),G1(:,:,:,20))
  call check_last_CV_D(l_switch,G1(:,:,:,20),Q(:,44),wf(:,29),Q(:,19),G2tensor(:,15))
  call check_last_CV_D(l_switch,G1(:,:,:,20),Q(:,44),wf(:,39),Q(:,19),G2tensor(:,16))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,35),ZERO,G1(:,:,:,21))
  call loop_QV_A(G1(:,:,:,21),wf(:,10),G1(:,:,:,22))
  call check_last_Q_A(l_switch,G1(:,:,:,22),Q(:,63),ZERO,G2tensor(:,17))
  call loop_QV_A(G1(:,:,:,21),wf(:,20),G1(:,:,:,23))
  call check_last_Q_A(l_switch,G1(:,:,:,23),Q(:,63),ZERO,G2tensor(:,18))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,35),MC,G1(:,:,:,24))
  call loop_QV_A(G1(:,:,:,24),wf(:,10),G1(:,:,:,25))
  call check_last_Q_A(l_switch,G1(:,:,:,25),Q(:,63),MC,G2tensor(:,19))
  call loop_QV_A(G1(:,:,:,24),wf(:,20),G1(:,:,:,26))
  call check_last_Q_A(l_switch,G1(:,:,:,26),Q(:,63),MC,G2tensor(:,20))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,35),MT,G1(:,:,:,27))
  call loop_QV_A(G1(:,:,:,27),wf(:,10),G1(:,:,:,28))
  call check_last_Q_A(l_switch,G1(:,:,:,28),Q(:,63),MT,G2tensor(:,21))
  call loop_QV_A(G1(:,:,:,27),wf(:,20),G1(:,:,:,29))
  call check_last_Q_A(l_switch,G1(:,:,:,29),Q(:,63),MT,G2tensor(:,22))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,35),MB,G1(:,:,:,30))
  call loop_QV_A(G1(:,:,:,30),wf(:,10),G1(:,:,:,31))
  call check_last_Q_A(l_switch,G1(:,:,:,31),Q(:,63),MB,G2tensor(:,23))
  call loop_QV_A(G1(:,:,:,30),wf(:,20),G1(:,:,:,32))
  call check_last_Q_A(l_switch,G1(:,:,:,32),Q(:,63),MB,G2tensor(:,24))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,47),Q(:,35),G1(:,:,:,33))
  call check_last_CV_D(l_switch,G1(:,:,:,33),Q(:,35),wf(:,10),Q(:,28),G2tensor(:,25))
  call check_last_CV_D(l_switch,G1(:,:,:,33),Q(:,35),wf(:,20),Q(:,28),G2tensor(:,26))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,35),ZERO,G1(:,:,:,34))
  call loop_QV_A(G1(:,:,:,34),wf(:,10),G1(:,:,:,35))
  call check_last_Q_A(l_switch,G1(:,:,:,35),Q(:,63),ZERO,G2tensor(:,27))
  call loop_QV_A(G1(:,:,:,34),wf(:,20),G1(:,:,:,36))
  call check_last_Q_A(l_switch,G1(:,:,:,36),Q(:,63),ZERO,G2tensor(:,28))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,35),MC,G1(:,:,:,37))
  call loop_QV_A(G1(:,:,:,37),wf(:,10),G1(:,:,:,38))
  call check_last_Q_A(l_switch,G1(:,:,:,38),Q(:,63),MC,G2tensor(:,29))
  call loop_QV_A(G1(:,:,:,37),wf(:,20),G1(:,:,:,39))
  call check_last_Q_A(l_switch,G1(:,:,:,39),Q(:,63),MC,G2tensor(:,30))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,14))
  call loop_Q_A(G0(:,:,:,14),Q(:,35),MT,G1(:,:,:,40))
  call loop_QV_A(G1(:,:,:,40),wf(:,10),G1(:,:,:,41))
  call check_last_Q_A(l_switch,G1(:,:,:,41),Q(:,63),MT,G2tensor(:,31))
  call loop_QV_A(G1(:,:,:,40),wf(:,20),G1(:,:,:,42))
  call check_last_Q_A(l_switch,G1(:,:,:,42),Q(:,63),MT,G2tensor(:,32))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,35),MB,G1(:,:,:,43))
  call loop_QV_A(G1(:,:,:,43),wf(:,10),G1(:,:,:,44))
  call check_last_Q_A(l_switch,G1(:,:,:,44),Q(:,63),MB,G2tensor(:,33))
  call loop_QV_A(G1(:,:,:,43),wf(:,20),G1(:,:,:,45))
  call check_last_Q_A(l_switch,G1(:,:,:,45),Q(:,63),MB,G2tensor(:,34))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,48),Q(:,35),G1(:,:,:,46))
  call check_last_CV_D(l_switch,G1(:,:,:,46),Q(:,35),wf(:,10),Q(:,28),G2tensor(:,35))
  call check_last_CV_D(l_switch,G1(:,:,:,46),Q(:,35),wf(:,20),Q(:,28),G2tensor(:,36))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(50)*M(2))+c(49)*M(4)) * den(197)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(50)*M(2))+c(49)*M(4)) * den(197)
  T2sum(1:15,108) = T2sum(1:15,108) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(46)*M(2))+c(42)*M(4)) * den(197)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(200)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(200)
  T2sum(1:15,106) = T2sum(1:15,106) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(200)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(52)*M(3))+c(51)*M(4)) * den(200)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(200)
  T2sum(1:15,108) = T2sum(1:15,108) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(46)*M(3))+c(42)*M(4)) * den(200)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(50)*M(2))+c(49)*M(4)) * den(203)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(50)*M(2))+c(49)*M(4)) * den(203)
  T2sum(1:15,108) = T2sum(1:15,108) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(46)*M(2))+c(42)*M(4)) * den(203)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(206)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(206)
  T2sum(1:15,106) = T2sum(1:15,106) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(206)
  T2sum(1:15,107) = T2sum(1:15,107) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(52)*M(3))+c(51)*M(4)) * den(206)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(50)*M(3))+c(49)*M(4)) * den(206)
  T2sum(1:15,108) = T2sum(1:15,108) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(46)*M(3))+c(42)*M(4)) * den(206)
  T2sum(1:15,95) = T2sum(1:15,95) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(209)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(209)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(209)
  T2sum(1:15,101) = T2sum(1:15,101) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(25)*M(1)-c(26)*M(3)) * den(209)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(209)
  T2sum(1:15,102) = T2sum(1:15,102) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(16)*M(1)-c(20)*M(3)) * den(209)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(212)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(212)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(212)
  T2sum(1:15,101) = T2sum(1:15,101) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(25)*M(1)-c(26)*M(2)) * den(212)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(212)
  T2sum(1:15,102) = T2sum(1:15,102) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(16)*M(1)-c(20)*M(2)) * den(212)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(215)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(215)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(215)
  T2sum(1:15,101) = T2sum(1:15,101) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(25)*M(1)-c(26)*M(3)) * den(215)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(23)*M(1)-c(24)*M(3)) * den(215)
  T2sum(1:15,102) = T2sum(1:15,102) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(16)*M(1)-c(20)*M(3)) * den(215)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(218)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(218)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(218)
  T2sum(1:15,101) = T2sum(1:15,101) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(25)*M(1)-c(26)*M(2)) * den(218)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(218)
  T2sum(1:15,102) = T2sum(1:15,102) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(16)*M(1)-c(20)*M(2)) * den(218)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,36)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_3

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_3_pphbbj_ccxbbxhg_1_/**/REALKIND
