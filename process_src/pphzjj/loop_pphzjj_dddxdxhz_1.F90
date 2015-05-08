
module ol_colourmatrix_pphzjj_dddxdxhz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphzjj_dddxdxhz_1_/**/REALKIND



module ol_forced_parameters_pphzjj_dddxdxhz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphzjj_dddxdxhz_1_/**/REALKIND

module ol_loop_pphzjj_dddxdxhz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(14), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:106)
  ! denominators
  complex(REALKIND), save :: den(115)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,48)
  ! zero helicity identifier
  logical,           save :: zerohel(48) = .true., zerohel_ct(48) = .true.

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
    f( 1) = (CI*eQED**2*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (countertermnorm*ctZGG*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 7) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw*2._/**/REALKIND)
    f( 8) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 9) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(10) = (2*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(11) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/MQ2sum
    f(12) = (eQED**2*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(13) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/MQ2sum
    f(14) = (eQED**2*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(66)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rMZ, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_SV_V(wf(:,-4),wf(:,-5),wf(:,2))
  call prop_W_W(wf(:,2),Q(:,48),MZ,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,4))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,3),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,7),ZERO,0_intkind1,wf(:,6))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-1),wf(:,7))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,50),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,10))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,11))
  call prop_Q_A(wf(:,11),Q(:,7),ZERO,0_intkind1,wf(:,12))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,0),wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,10),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,49),ZERO,0_intkind1,wf(:,15))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,16))
  call vert_VQ_A(wf(:,16),wf(:,-1),wf(:,17))
  call vert_AZ_Q(gZd,wf(:,-2),wf(:,3),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,11),ZERO,0_intkind1,wf(:,19))
  call vert_AV_Q(wf(:,-2),wf(:,16),wf(:,20))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,21))
  call vert_VQ_A(wf(:,21),wf(:,0),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,11),ZERO,0_intkind1,wf(:,23))
  call vert_AV_Q(wf(:,-2),wf(:,21),wf(:,24))
  call counter_GG_V(wf(:,1),Q(:,5),wf(:,21),Q(:,10),wf(:,25))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,-1),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,34),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,27),wf(:,-3),wf(:,28))
  call counter_SG_G(wf(:,-4),wf(:,1),wf(:,29))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,40),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-1),wf(:,31),wf(:,32))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,3),wf(:,33))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,34))
  call counter_GG_V(wf(:,10),Q(:,6),wf(:,16),Q(:,9),wf(:,35))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,0),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,33),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,37),wf(:,-3),wf(:,38))
  call counter_SG_G(wf(:,-4),wf(:,10),wf(:,39))
  call vert_QA_V(wf(:,0),wf(:,31),wf(:,40))
  call counter_AV_Q(wf(:,-3),wf(:,10),wf(:,41))
  call vert_QA_V(wf(:,27),wf(:,-2),wf(:,42))
  call counter_SG_G(wf(:,-4),wf(:,16),wf(:,43))
  call vert_AZ_Q(gZd,wf(:,-2),wf(:,-5),wf(:,44))
  call prop_A_Q(wf(:,44),Q(:,36),ZERO,0_intkind1,wf(:,45))
  call vert_QA_V(wf(:,-1),wf(:,45),wf(:,46))
  call vert_QA_V(wf(:,37),wf(:,-2),wf(:,47))
  call counter_SG_G(wf(:,-4),wf(:,21),wf(:,48))
  call vert_QA_V(wf(:,0),wf(:,45),wf(:,49))
  call counter_AZ_Q(gZd,wf(:,-2),wf(:,3),wf(:,50))
  call counter_AV_Q(wf(:,-2),wf(:,16),wf(:,51))
  call counter_AV_Q(wf(:,-2),wf(:,21),wf(:,52))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-1),wf(:,53))
  call prop_A_Q(wf(:,8),Q(:,13),ZERO,0_intkind1,wf(:,54))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,55))
  call prop_A_Q(wf(:,5),Q(:,56),ZERO,0_intkind1,wf(:,56))
  call prop_A_Q(wf(:,20),Q(:,13),ZERO,0_intkind1,wf(:,57))
  call counter_VQ_A(wf(:,16),wf(:,-1),wf(:,58))
  call prop_A_Q(wf(:,18),Q(:,52),ZERO,0_intkind1,wf(:,59))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,60))
  call vert_AV_Q(wf(:,-2),wf(:,60),wf(:,61))
  call vert_VQ_A(wf(:,60),wf(:,0),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,11),ZERO,0_intkind1,wf(:,63))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,64))
  call vert_AV_Q(wf(:,-3),wf(:,64),wf(:,65))
  call vert_VQ_A(wf(:,64),wf(:,0),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,7),ZERO,0_intkind1,wf(:,67))
  call counter_VQ_A(wf(:,10),wf(:,0),wf(:,68))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,0),wf(:,69))
  call prop_A_Q(wf(:,14),Q(:,14),ZERO,0_intkind1,wf(:,70))
  call counter_VQ_A(wf(:,21),wf(:,0),wf(:,71))
  call prop_A_Q(wf(:,24),Q(:,14),ZERO,0_intkind1,wf(:,72))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,73))
  call vert_VQ_A(wf(:,73),wf(:,-1),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,11),ZERO,0_intkind1,wf(:,75))
  call vert_AV_Q(wf(:,-2),wf(:,73),wf(:,76))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,77))
  call vert_VQ_A(wf(:,77),wf(:,-1),wf(:,78))
  call prop_Q_A(wf(:,78),Q(:,7),ZERO,0_intkind1,wf(:,79))
  call vert_AV_Q(wf(:,-3),wf(:,77),wf(:,80))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,81))
  call vert_VQ_A(wf(:,81),wf(:,-1),wf(:,82))
  call vert_AV_Q(wf(:,-3),wf(:,81),wf(:,83))
  call counter_Q_A(ctqq,wf(:,6),Q(:,7),wf(:,84))
  call counter_Q_A(ctqq,wf(:,9),Q(:,50),wf(:,85))
  call counter_V_V(ctGG,wf(:,10),Q(:,6),wf(:,86))
  call vert_VQ_A(wf(:,86),wf(:,0),wf(:,87))
  call counter_Q_A(ctqq,wf(:,12),Q(:,7),wf(:,88))
  call counter_Q_A(ctqq,wf(:,15),Q(:,49),wf(:,89))
  call vert_AV_Q(wf(:,-3),wf(:,86),wf(:,90))
  call counter_V_V(ctGG,wf(:,16),Q(:,9),wf(:,91))
  call vert_VQ_A(wf(:,91),wf(:,-1),wf(:,92))
  call vert_AV_Q(wf(:,-2),wf(:,91),wf(:,93))
  call counter_Q_A(ctqq,wf(:,19),Q(:,11),wf(:,94))
  call counter_V_V(ctGG,wf(:,21),Q(:,10),wf(:,95))
  call vert_VQ_A(wf(:,95),wf(:,0),wf(:,96))
  call counter_Q_A(ctqq,wf(:,23),Q(:,11),wf(:,97))
  call vert_AV_Q(wf(:,-2),wf(:,95),wf(:,98))
  call vert_QA_V(wf(:,15),wf(:,-2),wf(:,99))
  call vert_QA_V(wf(:,0),wf(:,59),wf(:,100))
  call vert_QA_V(wf(:,15),wf(:,-3),wf(:,101))
  call vert_QA_V(wf(:,0),wf(:,56),wf(:,102))
  call vert_QA_V(wf(:,9),wf(:,-2),wf(:,103))
  call vert_QA_V(wf(:,-1),wf(:,59),wf(:,104))
  call vert_QA_V(wf(:,9),wf(:,-3),wf(:,105))
  call vert_QA_V(wf(:,-1),wf(:,56),wf(:,106))

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
  den(2) = 1 / (Q(5,48) - MZ2)
  den(3) = 1 / (Q(5,7))
  den(6) = 1 / (Q(5,50))
  den(9) = 1 / (Q(5,6))
  den(12) = 1 / (Q(5,49))
  den(15) = 1 / (Q(5,9))
  den(16) = 1 / (Q(5,11))
  den(20) = 1 / (Q(5,10))
  den(26) = 1 / (Q(5,34))
  den(27) = 1 / (Q(5,42))
  den(30) = 1 / (Q(5,40))
  den(35) = 1 / (Q(5,33))
  den(36) = 1 / (Q(5,41))
  den(41) = 1 / (Q(5,38))
  den(44) = 1 / (Q(5,36))
  den(47) = 1 / (Q(5,37))
  den(52) = 1 / (Q(5,13))
  den(55) = 1 / (Q(5,56))
  den(60) = 1 / (Q(5,52))
  den(64) = 1 / (Q(5,14))
  den(90) = 1 / (Q(5,53))
  den(93) = 1 / (Q(5,57))
  den(96) = 1 / (Q(5,54))
  den(99) = 1 / (Q(5,58))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(10) = den(3)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(9)*den(13)
  den(17) = den(15)*den(16)
  den(18) = den(2)*den(17)
  den(19) = den(7)*den(15)
  den(21) = den(16)*den(20)
  den(22) = den(2)*den(21)
  den(23) = den(13)*den(20)
  den(24) = den(1)*den(20)
  den(25) = den(2)*den(24)
  den(28) = den(26)*den(27)
  den(29) = den(1)*den(28)
  den(31) = den(27)*den(30)
  den(32) = den(1)*den(31)
  den(33) = den(9)*den(15)
  den(34) = den(2)*den(33)
  den(37) = den(35)*den(36)
  den(38) = den(9)*den(37)
  den(39) = den(30)*den(36)
  den(40) = den(9)*den(39)
  den(42) = den(26)*den(41)
  den(43) = den(15)*den(42)
  den(45) = den(41)*den(44)
  den(46) = den(15)*den(45)
  den(48) = den(35)*den(47)
  den(49) = den(20)*den(48)
  den(50) = den(44)*den(47)
  den(51) = den(20)*den(50)
  den(53) = den(1)*den(52)
  den(54) = den(2)*den(53)
  den(56) = den(2)*den(55)
  den(57) = den(1)*den(56)
  den(58) = den(15)*den(52)
  den(59) = den(2)*den(58)
  den(61) = den(2)*den(60)
  den(62) = den(15)*den(61)
  den(63) = den(9)*den(56)
  den(65) = den(9)*den(64)
  den(66) = den(2)*den(65)
  den(67) = den(20)*den(61)
  den(68) = den(20)*den(64)
  den(69) = den(2)*den(68)
  den(70) = den(1)**2
  den(71) = den(56)*den(70)
  den(72) = den(7)*den(70)
  den(73) = den(4)*den(56)
  den(74) = den(7)*den(53)
  den(75) = den(9)**2
  den(76) = den(56)*den(75)
  den(77) = den(10)*den(56)
  den(78) = den(13)*den(65)
  den(79) = den(13)*den(75)
  den(80) = den(15)**2
  den(81) = den(61)*den(80)
  den(82) = den(7)*den(80)
  den(83) = den(17)*den(61)
  den(84) = den(7)*den(58)
  den(85) = den(20)**2
  den(86) = den(61)*den(85)
  den(87) = den(21)*den(61)
  den(88) = den(13)*den(68)
  den(89) = den(13)*den(85)
  den(91) = den(13)*den(90)
  den(92) = den(61)*den(90)
  den(94) = den(13)*den(93)
  den(95) = den(56)*den(93)
  den(97) = den(7)*den(96)
  den(98) = den(61)*den(96)
  den(100) = den(7)*den(99)
  den(101) = den(56)*den(99)
  den(102) = den(1)*den(2)*den(20)
  den(103) = den(1)*den(2)
  den(104) = den(2)*den(9)*den(15)
  den(105) = den(2)*den(9)
  den(106) = den(2)*den(15)
  den(107) = den(2)*den(20)
  den(108) = den(1)*den(100)
  den(109) = den(1)*den(101)
  den(110) = den(9)*den(94)
  den(111) = den(9)*den(95)
  den(112) = den(15)*den(97)
  den(113) = den(15)*den(98)
  den(114) = den(20)*den(91)
  den(115) = den(20)*den(92)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(66)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,5),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(14)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(6) = cont_QA(wf(:,9),wf(:,20)) * den(19)
  A(7) = cont_QA(wf(:,18),wf(:,23)) * den(22)
  A(8) = cont_QA(wf(:,15),wf(:,24)) * den(23)

  A(9) = cont_VV(wf(:,3),wf(:,25)) * den(25)
  A(10) = cont_VV(wf(:,28),wf(:,29)) * den(29)
  A(11) = cont_VV(wf(:,28),wf(:,29)) * den(29)
  A(12) = cont_VV(wf(:,29),wf(:,32)) * den(32)
  A(13) = cont_VV(wf(:,29),wf(:,32)) * den(32)
  A(14) = cont_QA(wf(:,6),wf(:,33)) * den(5)
  A(15) = cont_QA(wf(:,9),wf(:,34)) * den(8)
  A(16) = cont_VV(wf(:,3),wf(:,35)) * den(34)
  A(17) = cont_VV(wf(:,38),wf(:,39)) * den(38)
  A(18) = cont_VV(wf(:,38),wf(:,39)) * den(38)
  A(19) = cont_VV(wf(:,39),wf(:,40)) * den(40)
  A(20) = cont_VV(wf(:,39),wf(:,40)) * den(40)
  A(21) = cont_QA(wf(:,12),wf(:,33)) * den(11)
  A(22) = cont_QA(wf(:,15),wf(:,41)) * den(14)
  A(23) = cont_VV(wf(:,42),wf(:,43)) * den(43)
  A(24) = cont_VV(wf(:,42),wf(:,43)) * den(43)
  A(25) = cont_VV(wf(:,43),wf(:,46)) * den(46)
  A(26) = cont_VV(wf(:,43),wf(:,46)) * den(46)
  A(27) = cont_VV(wf(:,47),wf(:,48)) * den(49)
  A(28) = cont_VV(wf(:,47),wf(:,48)) * den(49)
  A(29) = cont_VV(wf(:,48),wf(:,49)) * den(51)
  A(30) = cont_VV(wf(:,48),wf(:,49)) * den(51)
  A(31) = cont_QA(wf(:,19),wf(:,50)) * den(18)
  A(32) = cont_QA(wf(:,9),wf(:,51)) * den(19)
  A(33) = cont_QA(wf(:,23),wf(:,50)) * den(22)
  A(34) = cont_QA(wf(:,15),wf(:,52)) * den(23)
  A(35) = cont_QA(wf(:,53),wf(:,54)) * den(54)
  A(36) = cont_QA(wf(:,55),wf(:,56)) * den(57)
  A(37) = cont_QA(wf(:,53),wf(:,57)) * den(59)
  A(38) = cont_QA(wf(:,58),wf(:,59)) * den(62)
  A(39) = cont_QA(wf(:,15),wf(:,61)) * den(23)
  A(40) = cont_QA(wf(:,18),wf(:,63)) * den(22)
  A(41) = cont_QA(wf(:,15),wf(:,65)) * den(14)
  A(42) = cont_QA(wf(:,5),wf(:,67)) * den(11)
  A(43) = cont_QA(wf(:,56),wf(:,68)) * den(63)
  A(44) = cont_QA(wf(:,69),wf(:,70)) * den(66)
  A(45) = cont_QA(wf(:,59),wf(:,71)) * den(67)
  A(46) = cont_QA(wf(:,69),wf(:,72)) * den(69)
  A(47) = cont_QA(wf(:,18),wf(:,75)) * den(18)
  A(48) = cont_QA(wf(:,9),wf(:,76)) * den(19)
  A(49) = cont_QA(wf(:,5),wf(:,79)) * den(5)
  A(50) = cont_QA(wf(:,9),wf(:,80)) * den(8)
  A(51) = cont_QA(wf(:,56),wf(:,82)) * den(71)
  A(52) = cont_QA(wf(:,9),wf(:,83)) * den(72)
  A(53) = cont_QA(wf(:,56),wf(:,84)) * den(73)
  A(54) = cont_QA(wf(:,54),wf(:,85)) * den(74)
  A(55) = cont_QA(wf(:,56),wf(:,87)) * den(76)
  A(56) = cont_QA(wf(:,56),wf(:,88)) * den(77)
  A(57) = cont_QA(wf(:,70),wf(:,89)) * den(78)
  A(58) = cont_QA(wf(:,15),wf(:,90)) * den(79)
  A(59) = cont_QA(wf(:,59),wf(:,92)) * den(81)
  A(60) = cont_QA(wf(:,9),wf(:,93)) * den(82)
  A(61) = cont_QA(wf(:,59),wf(:,94)) * den(83)
  A(62) = cont_QA(wf(:,57),wf(:,85)) * den(84)
  A(63) = cont_QA(wf(:,59),wf(:,96)) * den(86)
  A(64) = cont_QA(wf(:,59),wf(:,97)) * den(87)
  A(65) = cont_QA(wf(:,72),wf(:,89)) * den(88)
  A(66) = cont_QA(wf(:,15),wf(:,98)) * den(89)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(66)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(3)+A(4)+A(5)+A(6))*f(1))/6._/**/REALKIND+((A(1)+A(2)+A(7)+A(8))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(3)-A(4)-A(5)-A(6))*f(1))/2._/**/REALKIND+((-A(1)-A(2)-A(7)-A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(55)-A(56)-A(57)-A(58)-A(59)-A(60)-A(61)-A(62))*f(2))/6._/**/REALKIND+((-A(51)-A(52)-A(53)-A(54)-A(63)-A(64)-A(65) &
       -A(66))*f(2))/2._/**/REALKIND+((A(22)+A(32)+A(38)+A(41)+A(42)+A(43)+A(47)+A(48))*f(3))/6._/**/REALKIND+((A(15)+A(34)+A(36) &
       +A(39)+A(40)+A(45)+A(49)+A(50))*f(3))/2._/**/REALKIND+((A(21)+A(31)+A(37)+A(44))*f(4))/6._/**/REALKIND+((A(14)+A(33)+A(35) &
       +A(46))*f(4))/2._/**/REALKIND-(A(9)*f(5))/2._/**/REALKIND-(A(16)*f(5))/6._/**/REALKIND+((A(18)+A(20)+A(24) &
       +A(26))*f(11))/6._/**/REALKIND+((A(11)+A(13)+A(28)+A(30))*f(11))/2._/**/REALKIND+((A(17)+A(19)+A(23) &
       +A(25))*f(13))/6._/**/REALKIND+((A(10)+A(12)+A(27)+A(29))*f(13))/2._/**/REALKIND
  M2(2) = ((A(55)+A(56)+A(57)+A(58)+A(59)+A(60)+A(61)+A(62))*f(2))/2._/**/REALKIND+((A(51)+A(52)+A(53)+A(54)+A(63)+A(64)+A(65) &
       +A(66))*f(2))/6._/**/REALKIND+((-A(22)-A(32)-A(38)-A(41)-A(42)-A(43)-A(47)-A(48))*f(3))/2._/**/REALKIND+((-A(15)-A(34) &
       -A(36)-A(39)-A(40)-A(45)-A(49)-A(50))*f(3))/6._/**/REALKIND+((-A(21)-A(31)-A(37)-A(44))*f(4))/2._/**/REALKIND+((-A(14) &
       -A(33)-A(35)-A(46))*f(4))/6._/**/REALKIND+(A(9)*f(5))/6._/**/REALKIND+(A(16)*f(5))/2._/**/REALKIND+((-A(18)-A(20)-A(24) &
       -A(26))*f(11))/2._/**/REALKIND+((-A(11)-A(13)-A(28)-A(30))*f(11))/6._/**/REALKIND+((-A(17)-A(19)-A(23) &
       -A(25))*f(13))/2._/**/REALKIND+((-A(10)-A(12)-A(27)-A(29))*f(13))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphzjj_dddxdxhz_1_/**/REALKIND
