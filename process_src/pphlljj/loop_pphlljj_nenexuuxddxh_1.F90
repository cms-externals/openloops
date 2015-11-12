
module ol_colourmatrix_pphlljj_nenexuuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   4]
  K1(28,:) = [   4,   0]
  K1(29,:) = [ -12,  -4]
  K1(30,:) = [  -4,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [ -12,  -4]
  K1(38,:) = [  -4,   0]
  K1(39,:) = [   0,   4]
  K1(40,:) = [   4,   0]
  K1(41,:) = [   0,  -4]
  K1(42,:) = [  -4, -12]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [   0,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [   0,   0]
  K1(58,:) = [   0,   0]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlljj_nenexuuxddxh_1_/**/REALKIND



module ol_forced_parameters_pphlljj_nenexuuxddxh_1_/**/REALKIND
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
  if (YE /= 0) write(*,101) 'YE = 0'
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
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphlljj_nenexuuxddxh_1_/**/REALKIND

module ol_loop_pphlljj_nenexuuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(14), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:87)
  ! denominators
  complex(REALKIND), save :: den(83)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64)
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
    f( 1) = (CI*eQED**3*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (countertermnorm*ctZGG*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 7) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw*2._/**/REALKIND)
    f( 8) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 9) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(10) = (2*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(11) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(12) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(13) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(14) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10), 3*f(12), 9*f(12), 3*f(14), 9*f(14) ]
  c = (1._/**/REALKIND / 36) * c
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(33)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_S(P(:,7), rMH, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,4))
  call vert_SV_V(wf(:,-6),wf(:,3),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,28),ZERO,0_intkind1,wf(:,6))
  call prop_W_W(wf(:,5),Q(:,67),MZ,1_intkind1,wf(:,7))
  call vert_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call prop_A_Q(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,10))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,10),wf(:,11))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,12))
  call vert_VQ_A(wf(:,12),wf(:,-2),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,52),ZERO,0_intkind1,wf(:,14))
  call vert_QA_Z(gZu,wf(:,14),wf(:,-3),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,12),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,17))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,17),wf(:,18))
  call counter_GG_V(wf(:,2),Q(:,12),wf(:,12),Q(:,48),wf(:,19))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,20))
  call counter_SG_G(wf(:,-6),wf(:,2),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,19),ZERO,0_intkind1,wf(:,22))
  call vert_QA_V(wf(:,22),wf(:,-5),wf(:,23))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,24))
  call prop_A_Q(wf(:,24),Q(:,35),ZERO,0_intkind1,wf(:,25))
  call vert_QA_V(wf(:,-4),wf(:,25),wf(:,26))
  call counter_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,27))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,28))
  call prop_A_Q(wf(:,28),Q(:,44),ZERO,0_intkind1,wf(:,29))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,29),wf(:,30))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,10),wf(:,31))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,28),ZERO,0_intkind1,wf(:,33))
  call vert_QA_Z(gZd,wf(:,33),wf(:,-5),wf(:,34))
  call vert_ZQ_A(gZu,wf(:,3),wf(:,-2),wf(:,35))
  call counter_SG_G(wf(:,-6),wf(:,12),wf(:,36))
  call prop_Q_A(wf(:,35),Q(:,7),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,37),wf(:,-3),wf(:,38))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,3),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,11),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,-2),wf(:,40),wf(:,41))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,42))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,43))
  call prop_A_Q(wf(:,43),Q(:,56),ZERO,0_intkind1,wf(:,44))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,44),wf(:,45))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,52),ZERO,0_intkind1,wf(:,47))
  call vert_QA_Z(gZu,wf(:,47),wf(:,-3),wf(:,48))
  call counter_QA_Z(gZu,wf(:,14),wf(:,-3),wf(:,49))
  call counter_AV_Q(wf(:,-3),wf(:,12),wf(:,50))
  call prop_A_Q(wf(:,50),Q(:,56),ZERO,0_intkind1,wf(:,51))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,51),wf(:,52))
  call counter_VQ_A(wf(:,12),wf(:,-2),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,52),ZERO,0_intkind1,wf(:,54))
  call vert_QA_Z(gZu,wf(:,54),wf(:,-3),wf(:,55))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,17),wf(:,56))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,57))
  call vert_VQ_A(wf(:,57),wf(:,-4),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,28),ZERO,0_intkind1,wf(:,59))
  call vert_QA_Z(gZd,wf(:,59),wf(:,-5),wf(:,60))
  call vert_AV_Q(wf(:,-5),wf(:,57),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,44),ZERO,0_intkind1,wf(:,62))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,62),wf(:,63))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,64))
  call vert_VQ_A(wf(:,64),wf(:,-4),wf(:,65))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,66))
  call prop_Q_A(wf(:,65),Q(:,28),ZERO,0_intkind1,wf(:,67))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-4),wf(:,68))
  call vert_AV_Q(wf(:,-5),wf(:,64),wf(:,69))
  call prop_Q_A(wf(:,68),Q(:,83),ZERO,0_intkind1,wf(:,70))
  call counter_Q_A(ctqq,wf(:,6),Q(:,28),wf(:,71))
  call prop_A_Q(wf(:,66),Q(:,99),ZERO,0_intkind1,wf(:,72))
  call counter_A_Q(ctqq,wf(:,10),Q(:,44),wf(:,73))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,7),wf(:,74))
  call counter_Q_A(ctqq,wf(:,14),Q(:,52),wf(:,75))
  call prop_A_Q(wf(:,74),Q(:,75),ZERO,0_intkind1,wf(:,76))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,-2),wf(:,77))
  call counter_A_Q(ctqq,wf(:,17),Q(:,56),wf(:,78))
  call prop_Q_A(wf(:,77),Q(:,71),ZERO,0_intkind1,wf(:,79))
  call counter_V_V(ctGG,wf(:,12),Q(:,48),wf(:,80))
  call vert_VQ_A(wf(:,80),wf(:,-2),wf(:,81))
  call prop_Q_A(wf(:,81),Q(:,52),ZERO,0_intkind1,wf(:,82))
  call vert_AV_Q(wf(:,-3),wf(:,80),wf(:,83))
  call vert_QA_V(wf(:,79),wf(:,-3),wf(:,84))
  call vert_QA_V(wf(:,-2),wf(:,76),wf(:,85))
  call vert_QA_V(wf(:,70),wf(:,-5),wf(:,86))
  call vert_QA_V(wf(:,-4),wf(:,72),wf(:,87))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,12))
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,28))
  den(5) = 1 / (Q(5,67) - MZ2)
  den(8) = 1 / (Q(5,44))
  den(11) = 1 / (Q(5,48))
  den(12) = 1 / (Q(5,52))
  den(15) = 1 / (Q(5,56))
  den(20) = 1 / (Q(5,19))
  den(22) = 1 / (Q(5,76))
  den(25) = 1 / (Q(5,35))
  den(28) = 1 / (Q(5,7))
  den(30) = 1 / (Q(5,112))
  den(33) = 1 / (Q(5,11))
  den(39) = 1 / (Q(5,83))
  den(42) = 1 / (Q(5,99))
  den(46) = 1 / (Q(5,75))
  den(49) = 1 / (Q(5,71))
  den(56) = 1 / (Q(5,51))
  den(59) = 1 / (Q(5,15))
  den(62) = 1 / (Q(5,79))
  den(65) = 1 / (Q(5,115))

  ! denominators
  den(4) = den(1)*den(3)
  den(6) = den(2)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(1)*den(8)
  den(10) = den(6)*den(9)
  den(13) = den(11)*den(12)
  den(14) = den(6)*den(13)
  den(16) = den(11)*den(15)
  den(17) = den(6)*den(16)
  den(18) = den(1)*den(11)
  den(19) = den(6)*den(18)
  den(21) = den(2)*den(20)
  den(23) = den(1)*den(22)
  den(24) = den(21)*den(23)
  den(26) = den(2)*den(25)
  den(27) = den(23)*den(26)
  den(29) = den(2)*den(28)
  den(31) = den(11)*den(30)
  den(32) = den(29)*den(31)
  den(34) = den(2)*den(33)
  den(35) = den(31)*den(34)
  den(36) = den(1)**2
  den(37) = den(3)*den(36)
  den(38) = den(6)*den(37)
  den(40) = den(6)*den(39)
  den(41) = den(36)*den(40)
  den(43) = den(6)*den(42)
  den(44) = den(4)*den(43)
  den(45) = den(9)*den(40)
  den(47) = den(6)*den(46)
  den(48) = den(13)*den(47)
  den(50) = den(6)*den(49)
  den(51) = den(16)*den(50)
  den(52) = den(11)**2
  den(53) = den(12)*den(52)
  den(54) = den(6)*den(53)
  den(55) = den(50)*den(52)
  den(57) = den(21)*den(56)
  den(58) = den(26)*den(56)
  den(60) = den(29)*den(59)
  den(61) = den(34)*den(59)
  den(63) = den(50)*den(62)
  den(64) = den(47)*den(62)
  den(66) = den(40)*den(65)
  den(67) = den(43)*den(65)
  den(68) = den(1)*den(6)*den(11)
  den(69) = den(1)*den(2)*den(11)
  den(70) = den(1)*den(57)
  den(71) = den(1)*den(58)
  den(72) = den(1)*den(40)
  den(73) = den(1)*den(43)
  den(74) = den(1)*den(6)
  den(75) = den(11)*den(60)
  den(76) = den(11)*den(61)
  den(77) = den(11)*den(50)
  den(78) = den(11)*den(47)
  den(79) = den(6)*den(11)
  den(80) = den(1)*den(66)
  den(81) = den(1)*den(67)
  den(82) = den(11)*den(63)
  den(83) = den(11)*den(64)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(33)

  A(1) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_VV(wf(:,7),wf(:,11)) * den(10)
  A(3) = cont_VV(wf(:,7),wf(:,15)) * den(14)
  A(4) = cont_VV(wf(:,7),wf(:,18)) * den(17)

  A(5) = cont_VV(wf(:,7),wf(:,19)) * den(19)
  A(6) = cont_VV(wf(:,21),wf(:,23)) * den(24)
  A(7) = cont_VV(wf(:,21),wf(:,23)) * den(24)
  A(8) = cont_VV(wf(:,21),wf(:,26)) * den(27)
  A(9) = cont_VV(wf(:,21),wf(:,26)) * den(27)
  A(10) = cont_VV(wf(:,7),wf(:,27)) * den(7)
  A(11) = cont_VV(wf(:,7),wf(:,30)) * den(10)
  A(12) = cont_VV(wf(:,7),wf(:,31)) * den(10)
  A(13) = cont_VV(wf(:,7),wf(:,34)) * den(7)
  A(14) = cont_VV(wf(:,36),wf(:,38)) * den(32)
  A(15) = cont_VV(wf(:,36),wf(:,38)) * den(32)
  A(16) = cont_VV(wf(:,36),wf(:,41)) * den(35)
  A(17) = cont_VV(wf(:,36),wf(:,41)) * den(35)
  A(18) = cont_VV(wf(:,7),wf(:,45)) * den(17)
  A(19) = cont_VV(wf(:,7),wf(:,48)) * den(14)
  A(20) = cont_VV(wf(:,7),wf(:,49)) * den(14)
  A(21) = cont_VV(wf(:,7),wf(:,52)) * den(17)
  A(22) = cont_VV(wf(:,7),wf(:,55)) * den(14)
  A(23) = cont_VV(wf(:,7),wf(:,56)) * den(17)
  A(24) = cont_VV(wf(:,7),wf(:,60)) * den(7)
  A(25) = cont_VV(wf(:,7),wf(:,63)) * den(10)
  A(26) = cont_QA(wf(:,66),wf(:,67)) * den(38)
  A(27) = cont_QA(wf(:,69),wf(:,70)) * den(41)
  A(28) = cont_QA(wf(:,71),wf(:,72)) * den(44)
  A(29) = cont_QA(wf(:,70),wf(:,73)) * den(45)
  A(30) = cont_QA(wf(:,75),wf(:,76)) * den(48)
  A(31) = cont_QA(wf(:,78),wf(:,79)) * den(51)
  A(32) = cont_QA(wf(:,74),wf(:,82)) * den(54)
  A(33) = cont_QA(wf(:,79),wf(:,83)) * den(55)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(33)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2)+A(3)+A(4))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(26)-A(27)-A(28)-A(29)-A(30)-A(31)-A(32)-A(33))*f(2))/2._/**/REALKIND+((A(11)+A(13)+A(18)+A(19)+A(21)+A(22)+A(24) &
       +A(25))*f(3))/2._/**/REALKIND+((A(10)+A(12)+A(20)+A(23))*f(4))/2._/**/REALKIND-(A(5)*f(5))/2._/**/REALKIND+((A(7)+A(9) &
       +A(15)+A(17))*f(11))/2._/**/REALKIND+((A(6)+A(8)+A(14)+A(16))*f(13))/2._/**/REALKIND
  M2(2) = ((A(26)+A(27)+A(28)+A(29)+A(30)+A(31)+A(32)+A(33))*f(2))/6._/**/REALKIND+((-A(11)-A(13)-A(18)-A(19)-A(21)-A(22)-A(24) &
       -A(25))*f(3))/6._/**/REALKIND+((-A(10)-A(12)-A(20)-A(23))*f(4))/6._/**/REALKIND+(A(5)*f(5))/6._/**/REALKIND+((-A(7)-A(9) &
       -A(15)-A(17))*f(11))/6._/**/REALKIND+((-A(6)-A(8)-A(14)-A(16))*f(13))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlljj_nenexuuxddxh_1_/**/REALKIND
