
module ol_colourmatrix_ppllll2_eeexexgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,1), KL(0,1), KL2(1,1), KL2ct(1,1), KL2ct2(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [ 2]

  KL2ct(1,:) = [ 2]

  KL2ct2(1,:) = [ 2]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllll2_eeexexgg_1_/**/REALKIND



module ol_forced_parameters_ppllll2_eeexexgg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll2_eeexexgg_1_/**/REALKIND

module ol_loop_ppllll2_eeexexgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(16), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:107)
  ! denominators
  complex(REALKIND), save :: den(97)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,64)
  ! zero helicity identifier
  logical,           save :: zerohel(64) = .true., zerohel_ct(64) = .true.

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
    f( 1) = CI*countertermnorm*ctAAGG*eQED**4*gQCD**2
    f( 2) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**2
    f( 3) = countertermnorm*ctZGG*eQED**4*gQCD**2
    f( 4) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**2
    f( 5) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MW)/(cw**2*sw)
    f( 6) = (eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 7) = (2*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = (4*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f(10) = (2*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (8*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f(12) = eQED**4*gQCD**2*integralnorm*SwF
    f(13) = (4*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(14) = 2*eQED**4*gQCD**2*integralnorm*SwF
    f(15) = (eQED**4*gQCD**2*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(16) = (eQED**4*gQCD**2*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(0), M2(1)
  complex(REALKIND) :: A(26)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-3),wf(:,4))
  call prop_W_W(wf(:,4),Q(:,10),MZ,1_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-2),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,5),MZ,1_intkind1,wf(:,7))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,7),wf(:,8))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,9))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,10))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,9),wf(:,11))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,12))
  call prop_W_W(wf(:,12),Q(:,6),MZ,1_intkind1,wf(:,13))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-3),wf(:,14))
  call prop_W_W(wf(:,14),Q(:,9),MZ,1_intkind1,wf(:,15))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,15),wf(:,16))
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,17))
  call vert_VV_S(wf(:,7),wf(:,5),wf(:,18))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,19))
  call prop_W_W(wf(:,19),Q(:,48),MZ,1_intkind1,wf(:,20))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,21))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,20),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,7),ZERO,0_intkind1,wf(:,23))
  call vert_ZQ_A(gZl,wf(:,7),wf(:,-1),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,7),ZERO,0_intkind1,wf(:,25))
  call vert_ZQ_A(gZl,wf(:,20),wf(:,-1),wf(:,26))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,27))
  call prop_Q_A(wf(:,26),Q(:,50),ZERO,0_intkind1,wf(:,28))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,7),wf(:,29))
  call vert_VV_S(wf(:,13),wf(:,15),wf(:,30))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,7),ZERO,0_intkind1,wf(:,32))
  call vert_ZQ_A(gZl,wf(:,13),wf(:,0),wf(:,33))
  call prop_Q_A(wf(:,33),Q(:,7),ZERO,0_intkind1,wf(:,34))
  call vert_ZQ_A(gZl,wf(:,20),wf(:,0),wf(:,35))
  call vert_AV_Q(wf(:,-3),wf(:,10),wf(:,36))
  call prop_Q_A(wf(:,35),Q(:,49),ZERO,0_intkind1,wf(:,37))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,13),wf(:,38))
  call vert_VQ_A(wf(:,9),wf(:,-1),wf(:,39))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,20),wf(:,40))
  call prop_Q_A(wf(:,39),Q(:,11),ZERO,0_intkind1,wf(:,41))
  call vert_ZQ_A(gZl,wf(:,15),wf(:,-1),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,11),ZERO,0_intkind1,wf(:,43))
  call vert_AV_Q(wf(:,-2),wf(:,9),wf(:,44))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,15),wf(:,45))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,11),ZERO,0_intkind1,wf(:,47))
  call vert_ZQ_A(gZl,wf(:,5),wf(:,0),wf(:,48))
  call prop_Q_A(wf(:,48),Q(:,11),ZERO,0_intkind1,wf(:,49))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,50))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,5),wf(:,51))
  call vert_QA_V(wf(:,23),wf(:,-3),wf(:,52))
  call vert_QA_Z(gZl,wf(:,23),wf(:,-3),wf(:,53))
  call prop_W_W(wf(:,53),Q(:,15),MZ,1_intkind1,wf(:,54))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,55))
  call vert_QA_Z(gZl,wf(:,25),wf(:,-3),wf(:,56))
  call prop_W_W(wf(:,56),Q(:,15),MZ,1_intkind1,wf(:,57))
  call prop_A_Q(wf(:,27),Q(:,13),ZERO,0_intkind1,wf(:,58))
  call vert_QA_V(wf(:,-1),wf(:,58),wf(:,59))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,58),wf(:,60))
  call prop_W_W(wf(:,60),Q(:,15),MZ,1_intkind1,wf(:,61))
  call prop_A_Q(wf(:,29),Q(:,13),ZERO,0_intkind1,wf(:,62))
  call vert_QA_V(wf(:,-1),wf(:,62),wf(:,63))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,62),wf(:,64))
  call prop_W_W(wf(:,64),Q(:,15),MZ,1_intkind1,wf(:,65))
  call vert_QA_V(wf(:,32),wf(:,-3),wf(:,66))
  call vert_QA_Z(gZl,wf(:,32),wf(:,-3),wf(:,67))
  call prop_W_W(wf(:,67),Q(:,15),MZ,1_intkind1,wf(:,68))
  call vert_QA_V(wf(:,34),wf(:,-3),wf(:,69))
  call vert_QA_Z(gZl,wf(:,34),wf(:,-3),wf(:,70))
  call prop_W_W(wf(:,70),Q(:,15),MZ,1_intkind1,wf(:,71))
  call prop_A_Q(wf(:,36),Q(:,14),ZERO,0_intkind1,wf(:,72))
  call vert_QA_V(wf(:,0),wf(:,72),wf(:,73))
  call prop_A_Q(wf(:,38),Q(:,14),ZERO,0_intkind1,wf(:,74))
  call vert_QA_V(wf(:,0),wf(:,74),wf(:,75))
  call vert_QA_Z(gZl,wf(:,0),wf(:,72),wf(:,76))
  call prop_W_W(wf(:,76),Q(:,15),MZ,1_intkind1,wf(:,77))
  call vert_QA_Z(gZl,wf(:,0),wf(:,74),wf(:,78))
  call prop_W_W(wf(:,78),Q(:,15),MZ,1_intkind1,wf(:,79))
  call vert_QA_V(wf(:,41),wf(:,-2),wf(:,80))
  call vert_QA_Z(gZl,wf(:,41),wf(:,-2),wf(:,81))
  call prop_W_W(wf(:,81),Q(:,15),MZ,1_intkind1,wf(:,82))
  call vert_QA_V(wf(:,43),wf(:,-2),wf(:,83))
  call vert_QA_Z(gZl,wf(:,43),wf(:,-2),wf(:,84))
  call prop_W_W(wf(:,84),Q(:,15),MZ,1_intkind1,wf(:,85))
  call prop_A_Q(wf(:,44),Q(:,13),ZERO,0_intkind1,wf(:,86))
  call vert_QA_V(wf(:,-1),wf(:,86),wf(:,87))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,86),wf(:,88))
  call prop_W_W(wf(:,88),Q(:,15),MZ,1_intkind1,wf(:,89))
  call prop_A_Q(wf(:,45),Q(:,13),ZERO,0_intkind1,wf(:,90))
  call vert_QA_V(wf(:,-1),wf(:,90),wf(:,91))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,90),wf(:,92))
  call prop_W_W(wf(:,92),Q(:,15),MZ,1_intkind1,wf(:,93))
  call vert_QA_V(wf(:,47),wf(:,-2),wf(:,94))
  call vert_QA_Z(gZl,wf(:,47),wf(:,-2),wf(:,95))
  call prop_W_W(wf(:,95),Q(:,15),MZ,1_intkind1,wf(:,96))
  call vert_QA_V(wf(:,49),wf(:,-2),wf(:,97))
  call vert_QA_Z(gZl,wf(:,49),wf(:,-2),wf(:,98))
  call prop_W_W(wf(:,98),Q(:,15),MZ,1_intkind1,wf(:,99))
  call prop_A_Q(wf(:,50),Q(:,14),ZERO,0_intkind1,wf(:,100))
  call vert_QA_V(wf(:,0),wf(:,100),wf(:,101))
  call prop_A_Q(wf(:,51),Q(:,14),ZERO,0_intkind1,wf(:,102))
  call vert_QA_V(wf(:,0),wf(:,102),wf(:,103))
  call vert_QA_Z(gZl,wf(:,0),wf(:,100),wf(:,104))
  call prop_W_W(wf(:,104),Q(:,15),MZ,1_intkind1,wf(:,105))
  call vert_QA_Z(gZl,wf(:,0),wf(:,102),wf(:,106))
  call prop_W_W(wf(:,106),Q(:,15),MZ,1_intkind1,wf(:,107))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(4) = 1 / (Q(5,10) - MZ2)
  den(6) = 1 / (Q(5,5) - MZ2)
  den(9) = 1 / (Q(5,9))
  den(10) = 1 / (Q(5,6))
  den(12) = 1 / (Q(5,6) - MZ2)
  den(14) = 1 / (Q(5,9) - MZ2)
  den(17) = 1 / (Q(5,48) - MH2)
  den(19) = 1 / (Q(5,48) - MZ2)
  den(20) = 1 / (Q(5,7))
  den(25) = 1 / (Q(5,50))
  den(34) = 1 / (Q(5,49))
  den(38) = 1 / (Q(5,11))
  den(51) = 1 / (Q(5,15) - MH2)
  den(53) = 1 / (Q(5,15))
  den(55) = 1 / (Q(5,15) - MZ2)
  den(59) = 1 / (Q(5,13))
  den(71) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(4)*den(6)
  den(11) = den(9)*den(10)
  den(13) = den(9)*den(12)
  den(15) = den(10)*den(14)
  den(16) = den(12)*den(14)
  den(18) = den(8)*den(17)
  den(21) = den(1)*den(20)
  den(22) = den(19)*den(21)
  den(23) = den(6)*den(20)
  den(24) = den(19)*den(23)
  den(26) = den(19)*den(25)
  den(27) = den(1)*den(26)
  den(28) = den(6)*den(26)
  den(29) = den(16)*den(17)
  den(30) = den(10)*den(20)
  den(31) = den(19)*den(30)
  den(32) = den(12)*den(20)
  den(33) = den(19)*den(32)
  den(35) = den(19)*den(34)
  den(36) = den(10)*den(35)
  den(37) = den(12)*den(35)
  den(39) = den(9)*den(38)
  den(40) = den(19)*den(39)
  den(41) = den(14)*den(38)
  den(42) = den(19)*den(41)
  den(43) = den(9)*den(26)
  den(44) = den(14)*den(26)
  den(45) = den(2)*den(38)
  den(46) = den(19)*den(45)
  den(47) = den(4)*den(38)
  den(48) = den(19)*den(47)
  den(49) = den(2)*den(35)
  den(50) = den(4)*den(35)
  den(52) = den(8)*den(51)
  den(54) = den(21)*den(53)
  den(56) = den(21)*den(55)
  den(57) = den(23)*den(53)
  den(58) = den(23)*den(55)
  den(60) = den(1)*den(59)
  den(61) = den(53)*den(60)
  den(62) = den(55)*den(60)
  den(63) = den(6)*den(59)
  den(64) = den(53)*den(63)
  den(65) = den(55)*den(63)
  den(66) = den(16)*den(51)
  den(67) = den(30)*den(53)
  den(68) = den(30)*den(55)
  den(69) = den(32)*den(53)
  den(70) = den(32)*den(55)
  den(72) = den(10)*den(71)
  den(73) = den(53)*den(72)
  den(74) = den(12)*den(71)
  den(75) = den(53)*den(74)
  den(76) = den(55)*den(72)
  den(77) = den(55)*den(74)
  den(78) = den(39)*den(53)
  den(79) = den(39)*den(55)
  den(80) = den(41)*den(53)
  den(81) = den(41)*den(55)
  den(82) = den(9)*den(59)
  den(83) = den(53)*den(82)
  den(84) = den(55)*den(82)
  den(85) = den(14)*den(59)
  den(86) = den(53)*den(85)
  den(87) = den(55)*den(85)
  den(88) = den(45)*den(53)
  den(89) = den(45)*den(55)
  den(90) = den(47)*den(53)
  den(91) = den(47)*den(55)
  den(92) = den(2)*den(71)
  den(93) = den(53)*den(92)
  den(94) = den(4)*den(71)
  den(95) = den(53)*den(94)
  den(96) = den(55)*den(92)
  den(97) = den(55)*den(94)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(26)


  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(3) = cont_VV(wf(:,2),wf(:,8)) * den(7)
  A(4) = cont_VV(wf(:,5),wf(:,8)) * den(8)
  A(5) = cont_VV(wf(:,10),wf(:,11)) * den(11)
  A(6) = cont_VV(wf(:,11),wf(:,13)) * den(13)
  A(7) = cont_VV(wf(:,10),wf(:,16)) * den(15)
  A(8) = cont_VV(wf(:,13),wf(:,16)) * den(16)
  A(9) = cont_SS(wf(:,17),wf(:,18)) * den(18)
  A(10) = cont_QA(wf(:,22),wf(:,23)) * den(22)
  A(11) = cont_QA(wf(:,22),wf(:,25)) * den(24)
  A(12) = cont_QA(wf(:,27),wf(:,28)) * den(27)
  A(13) = cont_QA(wf(:,28),wf(:,29)) * den(28)
  A(14) = cont_SS(wf(:,17),wf(:,30)) * den(29)
  A(15) = cont_QA(wf(:,22),wf(:,32)) * den(31)
  A(16) = cont_QA(wf(:,22),wf(:,34)) * den(33)
  A(17) = cont_QA(wf(:,36),wf(:,37)) * den(36)
  A(18) = cont_QA(wf(:,37),wf(:,38)) * den(37)
  A(19) = cont_QA(wf(:,40),wf(:,41)) * den(40)
  A(20) = cont_QA(wf(:,40),wf(:,43)) * den(42)
  A(21) = cont_QA(wf(:,28),wf(:,44)) * den(43)
  A(22) = cont_QA(wf(:,28),wf(:,45)) * den(44)
  A(23) = cont_QA(wf(:,40),wf(:,47)) * den(46)
  A(24) = cont_QA(wf(:,40),wf(:,49)) * den(48)
  A(25) = cont_QA(wf(:,37),wf(:,50)) * den(49)
  A(26) = cont_QA(wf(:,37),wf(:,51)) * den(50)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(26)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*(-A(1)+A(5))*f(1)+2*(-A(2)-A(3)+A(6)+A(7))*f(2)+2*(A(10)+A(11)+A(12)+A(13)-A(15)-A(16)-A(17)-A(18)-A(19)-A(20)-A(21) &
       -A(22)+A(23)+A(24)+A(25)+A(26))*f(3)+2*(-A(4)+A(8))*f(4)+2*(A(9)-A(14))*f(5)

end subroutine colourvectors

end module ol_loop_ppllll2_eeexexgg_1_/**/REALKIND
