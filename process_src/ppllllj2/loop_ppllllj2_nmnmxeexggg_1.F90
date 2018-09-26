
module ol_colourmatrix_ppllllj2_nmnmxeexggg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nmnmxeexggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nmnmxeexggg_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (MM /= 0) write(*,101) 'MM = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllllj2_nmnmxeexggg_1_/**/REALKIND

module ol_loop_ppllllj2_nmnmxeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(32), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:71)
  ! denominators
  complex(REALKIND), save :: den(115)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128), Mct(2,128), Mcol_loop(2,128)
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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*countertermnorm*eQED**4*gQCD**3
    f( 2) = countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 3) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 4) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 5) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 6) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 7) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f( 8) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f(10) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = eQED**4*gQCD**3*integralnorm*SwF
    f(13) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(14) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(15) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(16) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(2._/**/REALKIND*cw**2*sw**2)
    f(17) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(18) = (CI*countertermnorm*eQED**4*gQCD**3*lambdaHZZ*YE)/(2._/**/REALKIND*cw**2*sw**2)
    f(19) = (CI*countertermnorm*ctZGG*eQED**4*gQCD**3*lambdaHZZ*YE)/(2._/**/REALKIND*cw**2*sw**2)
    f(20) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(2._/**/REALKIND*cw**2*sw**2)
    f(21) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(22) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(23) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(24) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE)/(MQ2sum*MW*sw*2._/**/REALKIND)
    f(25) = (CI*eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(4._/**/REALKIND*MW**2*sw**2)
    f(26) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(27) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(28) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(2._/**/REALKIND*cw**2*sw**2)
    f(29) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(30) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE*YT)/(MQ2sum*MW*sw*2._/**/REALKIND)
    f(31) = (CI*eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(4._/**/REALKIND*MW**2*sw**2)
    f(32) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)


end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2, POLSEL)
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
  integer,           intent(in), optional  :: POLSEL(7)
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(53)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), POLSEL(7))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-6),wf(:,5))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,12),MZ,1_intkind1,wf(:,7))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-6),wf(:,8))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,9))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-5),wf(:,10))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-5),wf(:,11))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,12))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-4),wf(:,13))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-4),wf(:,14))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,15))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,16))
  call prop_W_W(wf(:,16),Q(:,112),MZ,1_intkind1,wf(:,17))
  call vert_SV_V(wf(:,15),wf(:,4),wf(:,18))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,19))
  call prop_W_W(wf(:,19),Q(:,112),MZ,1_intkind1,wf(:,20))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,21))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-2),wf(:,22))
  call vert_AV_Q(wf(:,-3),wf(:,21),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,7),ZERO,0_intkind1,wf(:,24))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,25))
  call vert_AV_Q(wf(:,-3),wf(:,25),wf(:,26))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,17),wf(:,27))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,20),wf(:,28))
  call vert_VQ_A(wf(:,21),wf(:,-2),wf(:,29))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,116),ZERO,0_intkind1,wf(:,31))
  call vert_VQ_A(wf(:,25),wf(:,-2),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,116),ZERO,0_intkind1,wf(:,33))
  call vert_ZQ_A(gZl,wf(:,17),wf(:,-2),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,116),ZERO,0_intkind1,wf(:,35))
  call vert_ZQ_A(gZl,wf(:,20),wf(:,-2),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,116),ZERO,0_intkind1,wf(:,37))
  call vert_ZQ_A(gZn,wf(:,7),wf(:,0),wf(:,38))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,17),wf(:,39))
  call prop_Q_A(wf(:,38),Q(:,13),ZERO,0_intkind1,wf(:,40))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,20),wf(:,41))
  call vert_ZQ_A(gZn,wf(:,17),wf(:,0),wf(:,42))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,7),wf(:,43))
  call prop_Q_A(wf(:,42),Q(:,113),ZERO,0_intkind1,wf(:,44))
  call vert_ZQ_A(gZn,wf(:,20),wf(:,0),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,113),ZERO,0_intkind1,wf(:,46))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,47))
  call prop_W_W(wf(:,18),Q(:,15),MZ,1_intkind1,wf(:,48))
  call vert_VV_S(wf(:,4),wf(:,7),wf(:,49))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,50))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,9),Q(:,80),wf(:,51))
  call counter_GG_S(wf(:,-5),wf(:,9),wf(:,52))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,12),Q(:,96),wf(:,53))
  call counter_GG_S(wf(:,-4),wf(:,12),wf(:,54))
  call vert_AQ_S(gH,wf(:,-3),wf(:,24),wf(:,55))
  call prop_W_W(wf(:,47),Q(:,112),MZ,1_intkind1,wf(:,56))
  call vert_QA_Z(gZl,wf(:,24),wf(:,-3),wf(:,57))
  call prop_A_Q(wf(:,30),Q(:,11),ZERO,0_intkind1,wf(:,58))
  call vert_AQ_S(gH,wf(:,58),wf(:,-2),wf(:,59))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,58),wf(:,60))
  call prop_W_W(wf(:,51),Q(:,112),MZ,1_intkind1,wf(:,61))
  call prop_W_W(wf(:,53),Q(:,112),MZ,1_intkind1,wf(:,62))
  call vert_QA_Z(gZn,wf(:,40),wf(:,-1),wf(:,63))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,64))
  call vert_QA_Z(gZn,wf(:,0),wf(:,64),wf(:,65))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,66))
  call prop_W_W(wf(:,57),Q(:,15),MZ,1_intkind1,wf(:,67))
  call vert_QA_V(wf(:,-2),wf(:,58),wf(:,68))
  call prop_W_W(wf(:,60),Q(:,15),MZ,1_intkind1,wf(:,69))
  call prop_W_W(wf(:,63),Q(:,15),MZ,1_intkind1,wf(:,70))
  call prop_W_W(wf(:,65),Q(:,15),MZ,1_intkind1,wf(:,71))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(9) = 1 / (Q(5,80))
  den(12) = 1 / (Q(5,96))
  den(15) = 1 / (Q(5,12) - MH2)
  den(16) = 1 / (Q(5,112) - MZ2)
  den(19) = 1 / (Q(5,112))
  den(20) = 1 / (Q(5,7))
  den(24) = 1 / (Q(5,116))
  den(29) = 1 / (Q(5,13))
  den(32) = 1 / (Q(5,113))
  den(35) = 1 / (Q(5,15) - MZ2)
  den(38) = 1 / (Q(5,15) - MH2)
  den(45) = 1 / (Q(5,112) - MH2)
  den(50) = 1 / (Q(5,11))
  den(67) = 1 / (Q(5,14))
  den(75) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(4)*den(9)
  den(11) = den(7)*den(9)
  den(13) = den(4)*den(12)
  den(14) = den(7)*den(12)
  den(17) = den(1)*den(15)
  den(18) = den(16)*den(17)
  den(21) = den(1)*den(20)
  den(22) = den(19)*den(21)
  den(23) = den(16)*den(21)
  den(25) = den(19)*den(24)
  den(26) = den(1)*den(25)
  den(27) = den(16)*den(24)
  den(28) = den(1)*den(27)
  den(30) = den(6)*den(29)
  den(31) = den(16)*den(30)
  den(33) = den(16)*den(32)
  den(34) = den(6)*den(33)
  den(36) = den(17)*den(35)
  den(37) = den(3)*den(36)
  den(39) = den(7)*den(38)
  den(40) = den(3)*den(39)
  den(41) = den(9)*den(36)
  den(42) = den(9)*den(39)
  den(43) = den(12)*den(36)
  den(44) = den(12)*den(39)
  den(46) = den(3)*den(45)
  den(47) = den(21)*den(46)
  den(48) = den(3)*den(16)
  den(49) = den(21)*den(48)
  den(51) = den(1)*den(50)
  den(52) = den(46)*den(51)
  den(53) = den(48)*den(51)
  den(54) = den(9)*den(45)
  den(55) = den(21)*den(54)
  den(56) = den(9)*den(16)
  den(57) = den(21)*den(56)
  den(58) = den(51)*den(54)
  den(59) = den(51)*den(56)
  den(60) = den(12)*den(45)
  den(61) = den(21)*den(60)
  den(62) = den(12)*den(16)
  den(63) = den(21)*den(62)
  den(64) = den(51)*den(60)
  den(65) = den(51)*den(62)
  den(66) = den(30)*den(48)
  den(68) = den(6)*den(67)
  den(69) = den(48)*den(68)
  den(70) = den(30)*den(56)
  den(71) = den(56)*den(68)
  den(72) = den(30)*den(62)
  den(73) = den(62)*den(68)
  den(74) = den(21)*den(38)
  den(76) = den(21)*den(75)
  den(77) = den(21)*den(35)
  den(78) = den(38)*den(51)
  den(79) = den(51)*den(75)
  den(80) = den(35)*den(51)
  den(81) = den(30)*den(35)
  den(82) = den(35)*den(68)
  den(83) = den(1)*den(3)*den(15)
  den(84) = den(1)*den(2)*den(3)
  den(85) = den(1)*den(3)*den(6)
  den(86) = den(1)*den(9)*den(15)
  den(87) = den(1)*den(2)*den(9)
  den(88) = den(1)*den(6)*den(9)
  den(89) = den(1)*den(12)*den(15)
  den(90) = den(1)*den(2)*den(12)
  den(91) = den(1)*den(6)*den(12)
  den(92) = den(3)*den(74)
  den(93) = den(3)*den(76)
  den(94) = den(3)*den(77)
  den(95) = den(3)*den(78)
  den(96) = den(3)*den(79)
  den(97) = den(3)*den(80)
  den(98) = den(9)*den(74)
  den(99) = den(9)*den(76)
  den(100) = den(9)*den(77)
  den(101) = den(9)*den(78)
  den(102) = den(9)*den(79)
  den(103) = den(9)*den(80)
  den(104) = den(12)*den(74)
  den(105) = den(12)*den(76)
  den(106) = den(12)*den(77)
  den(107) = den(12)*den(78)
  den(108) = den(12)*den(79)
  den(109) = den(12)*den(80)
  den(110) = den(3)*den(81)
  den(111) = den(3)*den(82)
  den(112) = den(9)*den(81)
  den(113) = den(9)*den(82)
  den(114) = den(12)*den(81)
  den(115) = den(12)*den(82)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(53)


  A(1) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,8)) * den(8)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(10)
  A(4) = cont_VV(wf(:,9),wf(:,11)) * den(11)
  A(5) = cont_VV(wf(:,12),wf(:,13)) * den(13)
  A(6) = cont_VV(wf(:,12),wf(:,14)) * den(14)
  A(7) = cont_VV(wf(:,17),wf(:,18)) * den(18)
  A(8) = cont_VV(wf(:,18),wf(:,20)) * den(18)
  A(9) = cont_QA(wf(:,23),wf(:,24)) * den(22)
  A(10) = cont_QA(wf(:,24),wf(:,26)) * den(22)
  A(11) = cont_QA(wf(:,24),wf(:,27)) * den(23)
  A(12) = cont_QA(wf(:,24),wf(:,28)) * den(23)
  A(13) = cont_QA(wf(:,30),wf(:,31)) * den(26)
  A(14) = cont_QA(wf(:,30),wf(:,33)) * den(26)
  A(15) = cont_QA(wf(:,30),wf(:,35)) * den(28)
  A(16) = cont_QA(wf(:,30),wf(:,37)) * den(28)
  A(17) = cont_QA(wf(:,39),wf(:,40)) * den(31)
  A(18) = cont_QA(wf(:,40),wf(:,41)) * den(31)
  A(19) = cont_QA(wf(:,43),wf(:,44)) * den(34)
  A(20) = cont_QA(wf(:,43),wf(:,46)) * den(34)
  A(21) = cont_VV(wf(:,47),wf(:,48)) * den(37)
  A(22) = cont_SS(wf(:,49),wf(:,50)) * den(40)
  A(23) = cont_SS(wf(:,49),wf(:,50)) * den(40)
  A(24) = cont_VV(wf(:,48),wf(:,51)) * den(41)
  A(25) = cont_SS(wf(:,49),wf(:,52)) * den(42)
  A(26) = cont_SS(wf(:,49),wf(:,52)) * den(42)
  A(27) = cont_VV(wf(:,48),wf(:,53)) * den(43)
  A(28) = cont_SS(wf(:,49),wf(:,54)) * den(44)
  A(29) = cont_SS(wf(:,49),wf(:,54)) * den(44)
  A(30) = cont_SS(wf(:,50),wf(:,55)) * den(47)
  A(31) = cont_SS(wf(:,50),wf(:,55)) * den(47)
  A(32) = cont_VV(wf(:,56),wf(:,57)) * den(49)
  A(33) = cont_SS(wf(:,50),wf(:,59)) * den(52)
  A(34) = cont_SS(wf(:,50),wf(:,59)) * den(52)
  A(35) = cont_VV(wf(:,56),wf(:,60)) * den(53)
  A(36) = cont_SS(wf(:,52),wf(:,55)) * den(55)
  A(37) = cont_SS(wf(:,52),wf(:,55)) * den(55)
  A(38) = cont_VV(wf(:,57),wf(:,61)) * den(57)
  A(39) = cont_SS(wf(:,52),wf(:,59)) * den(58)
  A(40) = cont_SS(wf(:,52),wf(:,59)) * den(58)
  A(41) = cont_VV(wf(:,60),wf(:,61)) * den(59)
  A(42) = cont_SS(wf(:,54),wf(:,55)) * den(61)
  A(43) = cont_SS(wf(:,54),wf(:,55)) * den(61)
  A(44) = cont_VV(wf(:,57),wf(:,62)) * den(63)
  A(45) = cont_SS(wf(:,54),wf(:,59)) * den(64)
  A(46) = cont_SS(wf(:,54),wf(:,59)) * den(64)
  A(47) = cont_VV(wf(:,60),wf(:,62)) * den(65)
  A(48) = cont_VV(wf(:,56),wf(:,63)) * den(66)
  A(49) = cont_VV(wf(:,56),wf(:,65)) * den(69)
  A(50) = cont_VV(wf(:,61),wf(:,63)) * den(70)
  A(51) = cont_VV(wf(:,61),wf(:,65)) * den(71)
  A(52) = cont_VV(wf(:,62),wf(:,63)) * den(72)
  A(53) = cont_VV(wf(:,62),wf(:,65)) * den(73)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(53)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (A(9)+A(11)+A(13)+A(15)+A(17)+A(19))*f(1)+2*CI*(A(1)-A(3)+A(5))*f(2)+2*CI*(A(32)+A(35)-A(38)-A(41)+A(44)+A(47)+A(48) &
       +A(49)-A(50)-A(51)+A(52)+A(53))*f(3)+2*CI*(A(2)-A(4)+A(6))*f(4)+2*CI*(-A(23)+A(26)-A(29))*f(15)-A(7)*f(18)+2*CI*(-A(21) &
       +A(24)-A(27))*f(19)+2*CI*(-A(31)-A(34)+A(37)+A(40)-A(43)-A(46))*f(24)+2*CI*(-A(22)+A(25)-A(28))*f(27)+2*CI*(-A(30)-A(33) &
       +A(36)+A(39)-A(42)-A(45))*f(30)
  M2(2) = (A(10)+A(12)+A(14)+A(16)+A(18)+A(20))*f(1)+2*CI*(-A(1)+A(3)-A(5))*f(2)+2*CI*(-A(32)-A(35)+A(38)+A(41)-A(44)-A(47)-A(48) &
       -A(49)+A(50)+A(51)-A(52)-A(53))*f(3)+2*CI*(-A(2)+A(4)-A(6))*f(4)+2*CI*(A(23)-A(26)+A(29))*f(15)-A(8)*f(18)+2*CI*(A(21) &
       -A(24)+A(27))*f(19)+2*CI*(A(31)+A(34)-A(37)-A(40)+A(43)+A(46))*f(24)+2*CI*(A(22)-A(25)+A(28))*f(27)+2*CI*(A(30)+A(33)-A(36) &
       -A(39)+A(42)+A(45))*f(30)

end subroutine colourvectors

end module ol_loop_ppllllj2_nmnmxeexggg_1_/**/REALKIND
