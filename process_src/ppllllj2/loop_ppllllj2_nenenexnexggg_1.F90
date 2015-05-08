
module ol_colourmatrix_ppllllj2_nenenexnexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,2), KL(0,2), KL2(2,2), KL2ct(2,2), KL2ct2(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [  7, -2]
  KL2(2,:) = [ -2,  7]
  KL2 = (1._/**/REALKIND / 3) * KL2

  KL2ct(1,:) = [  7, -2]
  KL2ct(2,:) = [ -2,  7]
  KL2ct = (1._/**/REALKIND / 3) * KL2ct

  KL2ct2(1,:) = [  7, -2]
  KL2ct2(2,:) = [ -2,  7]
  KL2ct2 = (1._/**/REALKIND / 3) * KL2ct2

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj2_nenenexnexggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nenenexnexggg_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllllj2_nenenexnexggg_1_/**/REALKIND

module ol_loop_ppllllj2_nenenexnexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:76)
  ! denominators
  complex(REALKIND), save :: den(115)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128)
  ! zero helicity identifier
  logical,           save :: zerohel(128) = .true., zerohel_ct(128) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*countertermnorm*eQED**4*gQCD**3
    f( 2) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 3) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 4) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f( 5) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f( 6) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f( 7) = eQED**4*gQCD**3*integralnorm*SwF
    f( 8) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f( 9) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(10) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(11) = (CI*eQED**4*gQCD**3*integralnorm*MT*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(12) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2)
! P(0:3,npart) = 2 -> n-2 external momenta (standard representation)
! H(npart)     = external-particle helicities
! Writes the tree wave functions to 'wf', denominators to 'den'.
! Returns the Born and counterterm colour vectors M1 and M2.
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/REALKIND ! counterterms
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_wavefunctions_/**/REALKIND
  use ol_propagators_/**/REALKIND
  use ol_vertices_/**/REALKIND
  use ol_counterterms_/**/REALKIND
  implicit none
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(52)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MZ,1_intkind1,wf(:,5))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-6),wf(:,6))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,7))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-5),wf(:,8))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,9))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-4),wf(:,10))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,11))
  call prop_W_W(wf(:,11),Q(:,112),MZ,1_intkind1,wf(:,12))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,-1),wf(:,13))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,12),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,7),ZERO,0_intkind1,wf(:,15))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,16))
  call prop_W_W(wf(:,16),Q(:,112),MZ,1_intkind1,wf(:,17))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,17),wf(:,18))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,-1),wf(:,19))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,4),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,114),ZERO,0_intkind1,wf(:,21))
  call vert_ZQ_A(gZn,wf(:,17),wf(:,-1),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,114),ZERO,0_intkind1,wf(:,23))
  call vert_QA_Z(gZn,wf(:,0),wf(:,-3),wf(:,24))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-2),wf(:,25))
  call prop_W_W(wf(:,24),Q(:,9),MZ,1_intkind1,wf(:,26))
  call prop_W_W(wf(:,25),Q(:,6),MZ,1_intkind1,wf(:,27))
  call counter_VVG_G(wf(:,27),wf(:,26),wf(:,-6),wf(:,28))
  call counter_VVG_G(wf(:,27),wf(:,26),wf(:,-5),wf(:,29))
  call counter_VVG_G(wf(:,27),wf(:,26),wf(:,-4),wf(:,30))
  call vert_ZQ_A(gZn,wf(:,27),wf(:,0),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,7),ZERO,0_intkind1,wf(:,32))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,0),wf(:,33))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,27),wf(:,34))
  call prop_Q_A(wf(:,33),Q(:,113),ZERO,0_intkind1,wf(:,35))
  call vert_ZQ_A(gZn,wf(:,17),wf(:,0),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,113),ZERO,0_intkind1,wf(:,37))
  call vert_ZQ_A(gZn,wf(:,26),wf(:,-1),wf(:,38))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,12),wf(:,39))
  call prop_Q_A(wf(:,38),Q(:,11),ZERO,0_intkind1,wf(:,40))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,17),wf(:,41))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,26),wf(:,42))
  call vert_ZQ_A(gZn,wf(:,5),wf(:,0),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,11),ZERO,0_intkind1,wf(:,44))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,5),wf(:,45))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,46))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,47))
  call counter_GG_S(wf(:,-5),wf(:,7),wf(:,48))
  call counter_GG_S(wf(:,-4),wf(:,9),wf(:,49))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,50))
  call prop_W_W(wf(:,50),Q(:,112),MZ,1_intkind1,wf(:,51))
  call vert_QA_Z(gZn,wf(:,15),wf(:,-3),wf(:,52))
  call prop_A_Q(wf(:,20),Q(:,13),ZERO,0_intkind1,wf(:,53))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,53),wf(:,54))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,7),Q(:,80),wf(:,55))
  call prop_W_W(wf(:,55),Q(:,112),MZ,1_intkind1,wf(:,56))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,9),Q(:,96),wf(:,57))
  call prop_W_W(wf(:,57),Q(:,112),MZ,1_intkind1,wf(:,58))
  call vert_VV_S(wf(:,27),wf(:,26),wf(:,59))
  call vert_QA_Z(gZn,wf(:,32),wf(:,-3),wf(:,60))
  call prop_A_Q(wf(:,34),Q(:,14),ZERO,0_intkind1,wf(:,61))
  call vert_QA_Z(gZn,wf(:,0),wf(:,61),wf(:,62))
  call vert_QA_Z(gZn,wf(:,40),wf(:,-2),wf(:,63))
  call prop_A_Q(wf(:,42),Q(:,13),ZERO,0_intkind1,wf(:,64))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,64),wf(:,65))
  call vert_QA_Z(gZn,wf(:,44),wf(:,-2),wf(:,66))
  call prop_A_Q(wf(:,45),Q(:,14),ZERO,0_intkind1,wf(:,67))
  call vert_QA_Z(gZn,wf(:,0),wf(:,67),wf(:,68))
  call prop_W_W(wf(:,52),Q(:,15),MZ,1_intkind1,wf(:,69))
  call prop_W_W(wf(:,54),Q(:,15),MZ,1_intkind1,wf(:,70))
  call prop_W_W(wf(:,60),Q(:,15),MZ,1_intkind1,wf(:,71))
  call prop_W_W(wf(:,62),Q(:,15),MZ,1_intkind1,wf(:,72))
  call prop_W_W(wf(:,63),Q(:,15),MZ,1_intkind1,wf(:,73))
  call prop_W_W(wf(:,65),Q(:,15),MZ,1_intkind1,wf(:,74))
  call prop_W_W(wf(:,66),Q(:,15),MZ,1_intkind1,wf(:,75))
  call prop_W_W(wf(:,68),Q(:,15),MZ,1_intkind1,wf(:,76))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MZ2)
  den(2) = 1 / (Q(5,10) - MZ2)
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,80))
  den(8) = 1 / (Q(5,96))
  den(10) = 1 / (Q(5,112) - MZ2)
  den(11) = 1 / (Q(5,7))
  den(14) = 1 / (Q(5,114))
  den(17) = 1 / (Q(5,9) - MZ2)
  den(18) = 1 / (Q(5,6) - MZ2)
  den(25) = 1 / (Q(5,113))
  den(28) = 1 / (Q(5,11))
  den(35) = 1 / (Q(5,15) - MH2)
  den(42) = 1 / (Q(5,13))
  den(56) = 1 / (Q(5,14))
  den(77) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(4)*den(8)
  den(12) = den(1)*den(11)
  den(13) = den(10)*den(12)
  den(15) = den(10)*den(14)
  den(16) = den(1)*den(15)
  den(19) = den(17)*den(18)
  den(20) = den(3)*den(19)
  den(21) = den(6)*den(19)
  den(22) = den(8)*den(19)
  den(23) = den(11)*den(18)
  den(24) = den(10)*den(23)
  den(26) = den(10)*den(25)
  den(27) = den(18)*den(26)
  den(29) = den(17)*den(28)
  den(30) = den(10)*den(29)
  den(31) = den(15)*den(17)
  den(32) = den(2)*den(28)
  den(33) = den(10)*den(32)
  den(34) = den(2)*den(26)
  den(36) = den(4)*den(35)
  den(37) = den(3)*den(36)
  den(38) = den(6)*den(36)
  den(39) = den(8)*den(36)
  den(40) = den(3)*den(10)
  den(41) = den(12)*den(40)
  den(43) = den(1)*den(42)
  den(44) = den(40)*den(43)
  den(45) = den(6)*den(10)
  den(46) = den(12)*den(45)
  den(47) = den(43)*den(45)
  den(48) = den(8)*den(10)
  den(49) = den(12)*den(48)
  den(50) = den(43)*den(48)
  den(51) = den(19)*den(35)
  den(52) = den(3)*den(51)
  den(53) = den(6)*den(51)
  den(54) = den(8)*den(51)
  den(55) = den(23)*den(40)
  den(57) = den(18)*den(56)
  den(58) = den(40)*den(57)
  den(59) = den(23)*den(45)
  den(60) = den(45)*den(57)
  den(61) = den(23)*den(48)
  den(62) = den(48)*den(57)
  den(63) = den(29)*den(40)
  den(64) = den(17)*den(42)
  den(65) = den(40)*den(64)
  den(66) = den(29)*den(45)
  den(67) = den(45)*den(64)
  den(68) = den(29)*den(48)
  den(69) = den(48)*den(64)
  den(70) = den(32)*den(40)
  den(71) = den(2)*den(56)
  den(72) = den(40)*den(71)
  den(73) = den(32)*den(45)
  den(74) = den(45)*den(71)
  den(75) = den(32)*den(48)
  den(76) = den(48)*den(71)
  den(78) = den(12)*den(77)
  den(79) = den(43)*den(77)
  den(80) = den(23)*den(77)
  den(81) = den(57)*den(77)
  den(82) = den(29)*den(77)
  den(83) = den(64)*den(77)
  den(84) = den(32)*den(77)
  den(85) = den(71)*den(77)
  den(86) = den(1)*den(2)*den(3)
  den(87) = den(1)*den(2)*den(6)
  den(88) = den(1)*den(2)*den(8)
  den(89) = den(3)*den(78)
  den(90) = den(3)*den(79)
  den(91) = den(6)*den(78)
  den(92) = den(6)*den(79)
  den(93) = den(8)*den(78)
  den(94) = den(8)*den(79)
  den(95) = den(3)*den(17)*den(18)
  den(96) = den(6)*den(17)*den(18)
  den(97) = den(8)*den(17)*den(18)
  den(98) = den(3)*den(80)
  den(99) = den(3)*den(81)
  den(100) = den(6)*den(80)
  den(101) = den(6)*den(81)
  den(102) = den(8)*den(80)
  den(103) = den(8)*den(81)
  den(104) = den(3)*den(82)
  den(105) = den(3)*den(83)
  den(106) = den(6)*den(82)
  den(107) = den(6)*den(83)
  den(108) = den(8)*den(82)
  den(109) = den(8)*den(83)
  den(110) = den(3)*den(84)
  den(111) = den(3)*den(85)
  den(112) = den(6)*den(84)
  den(113) = den(6)*den(85)
  den(114) = den(8)*den(84)
  den(115) = den(8)*den(85)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(52)


  A(1) = cont_VV(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(13)
  A(5) = cont_QA(wf(:,15),wf(:,18)) * den(13)
  A(6) = cont_QA(wf(:,20),wf(:,21)) * den(16)
  A(7) = cont_QA(wf(:,20),wf(:,23)) * den(16)
  A(8) = cont_VV(wf(:,3),wf(:,28)) * den(20)
  A(9) = cont_VV(wf(:,7),wf(:,29)) * den(21)
  A(10) = cont_VV(wf(:,9),wf(:,30)) * den(22)
  A(11) = cont_QA(wf(:,14),wf(:,32)) * den(24)
  A(12) = cont_QA(wf(:,18),wf(:,32)) * den(24)
  A(13) = cont_QA(wf(:,34),wf(:,35)) * den(27)
  A(14) = cont_QA(wf(:,34),wf(:,37)) * den(27)
  A(15) = cont_QA(wf(:,39),wf(:,40)) * den(30)
  A(16) = cont_QA(wf(:,40),wf(:,41)) * den(30)
  A(17) = cont_QA(wf(:,21),wf(:,42)) * den(31)
  A(18) = cont_QA(wf(:,23),wf(:,42)) * den(31)
  A(19) = cont_QA(wf(:,39),wf(:,44)) * den(33)
  A(20) = cont_QA(wf(:,41),wf(:,44)) * den(33)
  A(21) = cont_QA(wf(:,35),wf(:,45)) * den(34)
  A(22) = cont_QA(wf(:,37),wf(:,45)) * den(34)
  A(23) = cont_SS(wf(:,46),wf(:,47)) * den(37)
  A(24) = cont_SS(wf(:,46),wf(:,48)) * den(38)
  A(25) = cont_SS(wf(:,46),wf(:,49)) * den(39)
  A(26) = cont_VV(wf(:,51),wf(:,52)) * den(41)
  A(27) = cont_VV(wf(:,51),wf(:,54)) * den(44)
  A(28) = cont_VV(wf(:,52),wf(:,56)) * den(46)
  A(29) = cont_VV(wf(:,54),wf(:,56)) * den(47)
  A(30) = cont_VV(wf(:,52),wf(:,58)) * den(49)
  A(31) = cont_VV(wf(:,54),wf(:,58)) * den(50)
  A(32) = cont_SS(wf(:,47),wf(:,59)) * den(52)
  A(33) = cont_SS(wf(:,48),wf(:,59)) * den(53)
  A(34) = cont_SS(wf(:,49),wf(:,59)) * den(54)
  A(35) = cont_VV(wf(:,51),wf(:,60)) * den(55)
  A(36) = cont_VV(wf(:,51),wf(:,62)) * den(58)
  A(37) = cont_VV(wf(:,56),wf(:,60)) * den(59)
  A(38) = cont_VV(wf(:,56),wf(:,62)) * den(60)
  A(39) = cont_VV(wf(:,58),wf(:,60)) * den(61)
  A(40) = cont_VV(wf(:,58),wf(:,62)) * den(62)
  A(41) = cont_VV(wf(:,51),wf(:,63)) * den(63)
  A(42) = cont_VV(wf(:,51),wf(:,65)) * den(65)
  A(43) = cont_VV(wf(:,56),wf(:,63)) * den(66)
  A(44) = cont_VV(wf(:,56),wf(:,65)) * den(67)
  A(45) = cont_VV(wf(:,58),wf(:,63)) * den(68)
  A(46) = cont_VV(wf(:,58),wf(:,65)) * den(69)
  A(47) = cont_VV(wf(:,51),wf(:,66)) * den(70)
  A(48) = cont_VV(wf(:,51),wf(:,68)) * den(72)
  A(49) = cont_VV(wf(:,56),wf(:,66)) * den(73)
  A(50) = cont_VV(wf(:,56),wf(:,68)) * den(74)
  A(51) = cont_VV(wf(:,58),wf(:,66)) * den(75)
  A(52) = cont_VV(wf(:,58),wf(:,68)) * den(76)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(52)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (-A(4)-A(6)+A(11)+A(13)+A(15)+A(17)-A(19)-A(21))*f(1)+2*CI*(-A(26)-A(27)+A(28)+A(29)-A(30)-A(31)+A(35)+A(36)-A(37)-A(38) &
       +A(39)+A(40)+A(41)+A(42)-A(43)-A(44)+A(45)+A(46)-A(47)-A(48)+A(49)+A(50)-A(51)-A(52))*f(2)+2*CI*(-A(1)+A(2)-A(3)+A(8)-A(9) &
       +A(10))*f(3)+2*CI*(A(23)-A(24)+A(25)-A(32)+A(33)-A(34))*f(4)
  M2(2) = (-A(5)-A(7)+A(12)+A(14)+A(16)+A(18)-A(20)-A(22))*f(1)+2*CI*(A(26)+A(27)-A(28)-A(29)+A(30)+A(31)-A(35)-A(36)+A(37)+A(38) &
       -A(39)-A(40)-A(41)-A(42)+A(43)+A(44)-A(45)-A(46)+A(47)+A(48)-A(49)-A(50)+A(51)+A(52))*f(2)+2*CI*(A(1)-A(2)+A(3)-A(8)+A(9) &
       -A(10))*f(3)+2*CI*(-A(23)+A(24)-A(25)+A(32)-A(33)+A(34))*f(4)

end subroutine colourvectors

end module ol_loop_ppllllj2_nenenexnexggg_1_/**/REALKIND
