
module ol_vamp_2_pphhjj_uuxddxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_2(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphhjj_uuxddxhh_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphhjj_uuxddxhh_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphhjj_uuxddxhh_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphhjj_uuxddxhh_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,3) :: G0
  complex(REALKIND), dimension(4,5,4,1) :: G1
  complex(REALKIND), dimension(5,24) :: G1tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_SS_S(G0(:,:,:,1),wf(:,7),G0(:,:,:,2))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,77),Q(:,15),G1tensor(:,1))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,78),Q(:,15),G1tensor(:,2))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,79),Q(:,15),G1tensor(:,3))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,80),Q(:,15),G1tensor(:,4))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,96),Q(:,15),G1tensor(:,5))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,97),Q(:,15),G1tensor(:,6))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,98),Q(:,15),G1tensor(:,7))
  call check_last_CV_D(l_switch,G0(:,:,:,2),Q(:,48),wf(:,99),Q(:,15),G1tensor(:,8))
  call loop_VS_V(G0(:,:,:,1),wf(:,7),G0(:,:,:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,77),Q(:,15),G1tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,78),Q(:,15),G1tensor(:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,79),Q(:,15),G1tensor(:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,80),Q(:,15),G1tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,96),Q(:,15),G1tensor(:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,97),Q(:,15),G1tensor(:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,98),Q(:,15),G1tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,48),wf(:,99),Q(:,15),G1tensor(:,16))
  call loop_VT_S(G0(:,:,:,1),Q(:,0),wf(:,7),Q(:,48),G1(:,:,:,1))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,77),G1tensor(:,17))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,78),G1tensor(:,18))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,79),G1tensor(:,19))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,80),G1tensor(:,20))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,96),G1tensor(:,21))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,97),G1tensor(:,22))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,98),G1tensor(:,23))
  call check_last_SV_V(l_switch,G1(:,:,:,1),wf(:,99),G1tensor(:,24))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(42)*M(1))+c(41)*M(2)) * den(80)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(42)*M(1)-c(41)*M(2)) * den(80)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(18)*M(1)-c(17)*M(2)) * den(81)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(18)*M(1))+c(17)*M(2)) * den(81)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(46)*M(1))+c(45)*M(2)) * den(80)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(20)*M(1)-c(19)*M(2)) * den(81)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,10)
  Gcoeff = (-(c(42)*M(1))+c(41)*M(2)) * den(80)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(42)*M(1)-c(41)*M(2)) * den(80)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,17)
  Gcoeff = (-(c(84)*M(1))+c(83)*M(2)) * den(81)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(84)*M(1)-c(83)*M(2)) * den(81)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,18)
  Gcoeff = (-(c(42)*M(1))+c(41)*M(2)) * den(82)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(42)*M(1)-c(41)*M(2)) * den(82)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(18)*M(1)-c(17)*M(2)) * den(83)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,4)
  Gcoeff = (-(c(18)*M(1))+c(17)*M(2)) * den(83)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,4)
  Gcoeff = (-(c(46)*M(1))+c(45)*M(2)) * den(82)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(20)*M(1)-c(19)*M(2)) * den(83)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,12)
  Gcoeff = (-(c(42)*M(1))+c(41)*M(2)) * den(82)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(42)*M(1)-c(41)*M(2)) * den(82)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,19)
  Gcoeff = (-(c(84)*M(1))+c(83)*M(2)) * den(83)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(84)*M(1)-c(83)*M(2)) * den(83)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(44)*M(1)-c(43)*M(2)) * den(84)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,5)
  Gcoeff = (-(c(44)*M(1))+c(43)*M(2)) * den(84)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(18)*M(1)-c(17)*M(2)) * den(85)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,6)
  Gcoeff = (-(c(18)*M(1))+c(17)*M(2)) * den(85)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(48)*M(1)-c(47)*M(2)) * den(84)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(20)*M(1)-c(19)*M(2)) * den(85)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(44)*M(1)-c(43)*M(2)) * den(84)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,21)
  Gcoeff = (-(c(44)*M(1))+c(43)*M(2)) * den(84)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,21)
  Gcoeff = (-(c(84)*M(1))+c(83)*M(2)) * den(85)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(84)*M(1)-c(83)*M(2)) * den(85)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(44)*M(1)-c(43)*M(2)) * den(86)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,7)
  Gcoeff = (-(c(44)*M(1))+c(43)*M(2)) * den(86)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(18)*M(1)-c(17)*M(2)) * den(87)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,8)
  Gcoeff = (-(c(18)*M(1))+c(17)*M(2)) * den(87)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(48)*M(1)-c(47)*M(2)) * den(86)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(20)*M(1)-c(19)*M(2)) * den(87)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(44)*M(1)-c(43)*M(2)) * den(86)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,23)
  Gcoeff = (-(c(44)*M(1))+c(43)*M(2)) * den(86)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,23)
  Gcoeff = (-(c(84)*M(1))+c(83)*M(2)) * den(87)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(84)*M(1)-c(83)*M(2)) * den(87)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,24)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_2

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_2_pphhjj_uuxddxhh_1_/**/REALKIND
