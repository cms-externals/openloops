
module ol_colourmatrix_pphllj_nenexbbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  16]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   2]
  K1(11,:) = [  16]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [ -18]
  K1(20,:) = [ -18]
  K1(21,:) = [   0]
  K1(22,:) = [  36]
  K1(23,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphllj_nenexbbxhg_1_/**/REALKIND



module ol_forced_parameters_pphllj_nenexbbxhg_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphllj_nenexbbxhg_1_/**/REALKIND

module ol_loop_pphllj_nenexbbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(22), c(11)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:103)
  ! denominators
  complex(REALKIND), save :: den(93)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,32)
  ! zero helicity identifier
  logical,           save :: zerohel(32) = .true., zerohel_ct(32) = .true.

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
    f( 1) = (CI*eQED**3*gQCD*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (countertermnorm*ctZGG*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 7) = (eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 8) = (eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f( 9) = (2*eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(10) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MB*YB)/MQ2sum
    f(11) = (CI*eQED**3*gQCD*YB)/(2._/**/REALKIND*MW*sw)
    f(12) = (CI*countertermnorm*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(13) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(14) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(15) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(16) = (countertermnorm*ctZGG*eQED**3*gQCD**3*YB)/(MW*sw*2._/**/REALKIND)
    f(17) = (CI*eQED**3*gQCD**3*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f(18) = (eQED**3*gQCD**3*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f(19) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(20) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw)
    f(21) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MT*YT)/MQ2sum
    f(22) = (eQED**3*gQCD**3*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), f(7), 8*f(7), 3*f(8), 3*f(9), 9*CI*f(17), f(18), 8*f(18), 3*f(19), 3*f(20), 3*f(22) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(51)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,6))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,8))
  call vert_VQ_A(wf(:,-5),wf(:,5),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,11),MB,1_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,11))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,36),MB,1_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,24),MB,1_intkind1,wf(:,14))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,13),wf(:,15))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),MB,1_intkind1,wf(:,18))
  call vert_QS_A(gH,wf(:,13),wf(:,-4),wf(:,19))
  call vert_QA_Z(gZd,wf(:,13),wf(:,-3),wf(:,20))
  call vert_SV_V(wf(:,-4),wf(:,4),wf(:,21))
  call prop_W_W(wf(:,20),Q(:,44),MZ,1_intkind1,wf(:,22))
  call vert_SA_Q(gH,wf(:,-4),wf(:,6),wf(:,23))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,6),wf(:,24))
  call prop_W_W(wf(:,24),Q(:,44),MZ,1_intkind1,wf(:,25))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,26))
  call counter_GG_V(wf(:,26),Q(:,12),wf(:,-5),Q(:,32),wf(:,27))
  call prop_W_W(wf(:,21),Q(:,19),MZ,1_intkind1,wf(:,28))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,29))
  call counter_VQ_A(wf(:,-5),wf(:,5),wf(:,30))
  call vert_QA_V(wf(:,5),wf(:,-3),wf(:,31))
  call counter_VG_G(wf(:,4),wf(:,-5),Q(:,32),wf(:,32),Q(:,35))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,13),wf(:,33))
  call counter_AV_Q(wf(:,14),wf(:,-5),wf(:,34))
  call vert_QA_V(wf(:,-2),wf(:,14),wf(:,35))
  call counter_QS_A(gH,wf(:,13),wf(:,-4),wf(:,36))
  call counter_SA_Q(gH,wf(:,-4),wf(:,6),wf(:,37))
  call counter_SG_G(wf(:,-4),wf(:,-5),wf(:,38))
  call vert_AV_Q(wf(:,-3),wf(:,38),wf(:,39))
  call vert_VQ_A(wf(:,38),wf(:,-2),wf(:,40))
  call prop_Q_A(wf(:,40),Q(:,52),MB,1_intkind1,wf(:,41))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,42))
  call prop_Q_A(wf(:,9),Q(:,52),MB,1_intkind1,wf(:,43))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,44))
  call prop_A_Q(wf(:,44),Q(:,40),MB,1_intkind1,wf(:,45))
  call counter_QA_Z(gZd,wf(:,13),wf(:,-3),wf(:,46))
  call prop_Q_A(wf(:,19),Q(:,52),MB,1_intkind1,wf(:,47))
  call vert_SA_Q(gH,wf(:,-4),wf(:,45),wf(:,48))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,45),wf(:,49))
  call prop_W_W(wf(:,49),Q(:,44),MZ,1_intkind1,wf(:,50))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,24),MB,1_intkind1,wf(:,52))
  call vert_AV_Q(wf(:,52),wf(:,-5),wf(:,53))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,54))
  call prop_A_Q(wf(:,17),Q(:,56),MB,1_intkind1,wf(:,55))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,36),MB,1_intkind1,wf(:,57))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,57),wf(:,58))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,6),wf(:,59))
  call prop_A_Q(wf(:,23),Q(:,56),MB,1_intkind1,wf(:,60))
  call vert_QS_A(gH,wf(:,57),wf(:,-4),wf(:,61))
  call vert_QA_Z(gZd,wf(:,57),wf(:,-3),wf(:,62))
  call prop_W_W(wf(:,62),Q(:,44),MZ,1_intkind1,wf(:,63))
  call counter_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,64))
  call prop_Q_A(wf(:,64),Q(:,20),MB,1_intkind1,wf(:,65))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,65),wf(:,66))
  call vert_VQ_A(wf(:,-5),wf(:,65),wf(:,67))
  call vert_AZ_Q(gZd,wf(:,6),wf(:,4),wf(:,68))
  call counter_Q_A(ctbb,wf(:,5),Q(:,20),wf(:,69))
  call prop_A_Q(wf(:,68),Q(:,43),MB,1_intkind1,wf(:,70))
  call counter_A_Q(ctbb,wf(:,6),Q(:,40),wf(:,71))
  call prop_Q_A(wf(:,7),Q(:,23),MB,1_intkind1,wf(:,72))
  call prop_Q_A(wf(:,69),Q(:,20),MB,1_intkind1,wf(:,73))
  call vert_VQ_A(wf(:,-5),wf(:,73),wf(:,74))
  call counter_A_Q(ctbb,wf(:,10),Q(:,11),wf(:,75))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,4),wf(:,76))
  call counter_Q_A(ctbb,wf(:,13),Q(:,36),wf(:,77))
  call prop_A_Q(wf(:,76),Q(:,27),MB,1_intkind1,wf(:,78))
  call counter_A_Q(ctbb,wf(:,14),Q(:,24),wf(:,79))
  call prop_Q_A(wf(:,15),Q(:,39),MB,1_intkind1,wf(:,80))
  call counter_Q_A(ctbb,wf(:,18),Q(:,7),wf(:,81))
  call prop_A_Q(wf(:,79),Q(:,24),MB,1_intkind1,wf(:,82))
  call vert_AV_Q(wf(:,82),wf(:,-5),wf(:,83))
  call prop_Q_A(wf(:,77),Q(:,36),MB,1_intkind1,wf(:,84))
  call vert_QA_Z(gZd,wf(:,84),wf(:,-3),wf(:,85))
  call vert_QS_A(gH,wf(:,84),wf(:,-4),wf(:,86))
  call prop_A_Q(wf(:,71),Q(:,40),MB,1_intkind1,wf(:,87))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,87),wf(:,88))
  call vert_SA_Q(gH,wf(:,-4),wf(:,87),wf(:,89))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,90))
  call vert_QS_A(gH,wf(:,18),wf(:,-4),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,23),MB,1_intkind1,wf(:,92))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,39),MB,1_intkind1,wf(:,94))
  call vert_QA_V(wf(:,-2),wf(:,10),wf(:,95))
  call vert_SA_Q(gH,wf(:,-4),wf(:,10),wf(:,96))
  call prop_A_Q(wf(:,96),Q(:,27),MB,1_intkind1,wf(:,97))
  call vert_AV_Q(wf(:,10),wf(:,-5),wf(:,98))
  call prop_A_Q(wf(:,98),Q(:,43),MB,1_intkind1,wf(:,99))
  call vert_ZQ_A(gZd,wf(:,28),wf(:,-2),wf(:,100))
  call prop_Q_A(wf(:,100),Q(:,23),MB,1_intkind1,wf(:,101))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,28),wf(:,102))
  call prop_A_Q(wf(:,102),Q(:,27),MB,1_intkind1,wf(:,103))

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
  den(2) = 1 / (Q(5,20) - MB2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(6) = 1 / (Q(5,11) - MB2)
  den(9) = 1 / (Q(5,36) - MB2)
  den(10) = 1 / (Q(5,24) - MB2)
  den(13) = 1 / (Q(5,7) - MB2)
  den(17) = 1 / (Q(5,44) - MZ2)
  den(23) = 1 / (Q(5,12))
  den(24) = 1 / (Q(5,19) - MZ2)
  den(27) = 1 / (Q(5,28))
  den(32) = 1 / (Q(5,48))
  den(34) = 1 / (Q(5,52) - MB2)
  den(42) = 1 / (Q(5,56) - MB2)
  den(49) = 1 / (Q(5,43) - MB2)
  den(52) = 1 / (Q(5,23) - MB2)
  den(59) = 1 / (Q(5,27) - MB2)
  den(62) = 1 / (Q(5,39) - MB2)
  den(76) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(18) = den(9)*den(17)
  den(19) = den(1)*den(18)
  den(20) = den(3)*den(14)
  den(21) = den(3)*den(17)
  den(22) = den(1)*den(21)
  den(25) = den(1)*den(24)
  den(26) = den(23)*den(25)
  den(28) = den(2)*den(27)
  den(29) = den(1)*den(28)
  den(30) = den(10)*den(27)
  den(31) = den(1)*den(30)
  den(33) = den(14)*den(32)
  den(35) = den(32)*den(34)
  den(36) = den(1)*den(35)
  den(37) = den(2)*den(34)
  den(38) = den(1)*den(37)
  den(39) = den(9)*den(25)
  den(40) = den(9)*den(34)
  den(41) = den(1)*den(40)
  den(43) = den(10)*den(42)
  den(44) = den(1)*den(43)
  den(45) = den(3)*den(25)
  den(46) = den(3)*den(42)
  den(47) = den(1)*den(46)
  den(48) = den(1)*den(3)
  den(50) = den(48)*den(49)
  den(51) = den(2)*den(50)
  den(53) = den(4)*den(52)
  den(54) = den(3)*den(53)
  den(55) = den(2)**2
  den(56) = den(7)*den(55)
  den(57) = den(7)*den(37)
  den(58) = den(1)*den(10)
  den(60) = den(58)*den(59)
  den(61) = den(9)*den(60)
  den(63) = den(11)*den(62)
  den(64) = den(10)*den(63)
  den(65) = den(14)*den(43)
  den(66) = den(10)**2
  den(67) = den(14)*den(66)
  den(68) = den(9)**2
  den(69) = den(25)*den(68)
  den(70) = den(7)*den(68)
  den(71) = den(7)*den(40)
  den(72) = den(3)**2
  den(73) = den(25)*den(72)
  den(74) = den(14)*den(46)
  den(75) = den(14)*den(72)
  den(77) = den(14)*den(76)
  den(78) = den(14)*den(52)
  den(79) = den(14)*den(62)
  den(80) = den(7)*den(76)
  den(81) = den(7)*den(59)
  den(82) = den(7)*den(49)
  den(83) = den(25)*den(52)
  den(84) = den(25)*den(59)
  den(85) = den(1)*den(23)
  den(86) = den(1)*den(2)*den(3)
  den(87) = den(1)*den(9)*den(10)
  den(88) = den(2)*den(82)
  den(89) = den(10)*den(79)
  den(90) = den(9)*den(81)
  den(91) = den(9)*den(84)
  den(92) = den(3)*den(78)
  den(93) = den(3)*den(83)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(51)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_VV(wf(:,21),wf(:,22)) * den(19)
  A(7) = cont_QA(wf(:,18),wf(:,23)) * den(20)
  A(8) = cont_VV(wf(:,21),wf(:,25)) * den(22)

  A(9) = cont_VV(wf(:,27),wf(:,28)) * den(26)
  A(10) = cont_QA(wf(:,6),wf(:,29)) * den(5)
  A(11) = cont_QA(wf(:,10),wf(:,30)) * den(8)
  A(12) = cont_VV(wf(:,31),wf(:,32)) * den(29)
  A(13) = cont_QA(wf(:,14),wf(:,33)) * den(12)
  A(14) = cont_QA(wf(:,18),wf(:,34)) * den(15)
  A(15) = cont_VV(wf(:,32),wf(:,35)) * den(31)
  A(16) = cont_QA(wf(:,10),wf(:,36)) * den(16)
  A(17) = cont_QA(wf(:,18),wf(:,37)) * den(20)
  A(18) = cont_QA(wf(:,18),wf(:,39)) * den(33)
  A(19) = cont_QA(wf(:,18),wf(:,39)) * den(33)
  A(20) = cont_QA(wf(:,8),wf(:,41)) * den(36)
  A(21) = cont_QA(wf(:,8),wf(:,41)) * den(36)
  A(22) = cont_QA(wf(:,42),wf(:,43)) * den(38)
  A(23) = cont_QA(wf(:,7),wf(:,45)) * den(5)
  A(24) = cont_VV(wf(:,28),wf(:,46)) * den(39)
  A(25) = cont_QA(wf(:,42),wf(:,47)) * den(41)
  A(26) = cont_QA(wf(:,18),wf(:,48)) * den(20)
  A(27) = cont_VV(wf(:,21),wf(:,50)) * den(22)
  A(28) = cont_QA(wf(:,15),wf(:,52)) * den(12)
  A(29) = cont_QA(wf(:,18),wf(:,53)) * den(15)
  A(30) = cont_QA(wf(:,54),wf(:,55)) * den(44)
  A(31) = cont_QA(wf(:,14),wf(:,58)) * den(12)
  A(32) = cont_VV(wf(:,28),wf(:,59)) * den(45)
  A(33) = cont_QA(wf(:,54),wf(:,60)) * den(47)
  A(34) = cont_QA(wf(:,10),wf(:,61)) * den(16)
  A(35) = cont_VV(wf(:,21),wf(:,63)) * den(19)
  A(36) = cont_QA(wf(:,6),wf(:,66)) * den(5)
  A(37) = cont_QA(wf(:,10),wf(:,67)) * den(8)
  A(38) = cont_QA(wf(:,69),wf(:,70)) * den(51)
  A(39) = cont_QA(wf(:,71),wf(:,72)) * den(54)
  A(40) = cont_QA(wf(:,10),wf(:,74)) * den(56)
  A(41) = cont_QA(wf(:,43),wf(:,75)) * den(57)
  A(42) = cont_QA(wf(:,77),wf(:,78)) * den(61)
  A(43) = cont_QA(wf(:,79),wf(:,80)) * den(64)
  A(44) = cont_QA(wf(:,55),wf(:,81)) * den(65)
  A(45) = cont_QA(wf(:,18),wf(:,83)) * den(67)
  A(46) = cont_VV(wf(:,28),wf(:,85)) * den(69)
  A(47) = cont_QA(wf(:,10),wf(:,86)) * den(70)
  A(48) = cont_QA(wf(:,47),wf(:,75)) * den(71)
  A(49) = cont_VV(wf(:,28),wf(:,88)) * den(73)
  A(50) = cont_QA(wf(:,60),wf(:,81)) * den(74)
  A(51) = cont_QA(wf(:,18),wf(:,89)) * den(75)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(51)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(6)+A(8))*f(1)+(A(1)+A(2)+A(3)+A(4)+A(5)+A(7))*f(11)

  M2(1) = (-A(46)-A(49))*f(2)+(A(27)+A(35))*f(3)+(A(24)+A(32))*f(4)-A(9)*f(5)+(A(19)+A(21))*f(10)+(-A(38)-A(39)-A(40)-A(41)-A(42) &
       -A(43)-A(44)-A(45)-A(47)-A(48)-A(50)-A(51))*f(12)+(A(11)+A(14)+A(23)+A(26)+A(31)+A(34))*f(13)+(A(16)+A(17)+A(28)+A(29) &
       +A(36)+A(37))*f(14)+(A(10)+A(13)+A(22)+A(25)+A(30)+A(33))*f(15)+(-A(12)-A(15))*f(16)+(A(18)+A(20))*f(21)

end subroutine colourvectors

end module ol_loop_pphllj_nenexbbxhg_1_/**/REALKIND
