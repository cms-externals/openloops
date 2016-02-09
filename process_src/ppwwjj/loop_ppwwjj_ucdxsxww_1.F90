
module ol_colourmatrix_ppwwjj_ucdxsxww_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_ucdxsxww_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_ucdxsxww_1_/**/REALKIND
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
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwwjj_ucdxsxww_1_/**/REALKIND

module ol_loop_ppwwjj_ucdxsxww_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:118)
  ! denominators
  complex(REALKIND), save :: den(116)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,144)
  ! zero helicity identifier
  logical,           save :: zerohel(144) = .true., zerohel_ct(144) = .true.

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
    f( 1) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 3) = (CI*countertermnorm*ctGcc*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 6) = (CI*countertermnorm*ctVsc*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f( 8) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f( 9) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(10) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(11) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(7), 27*CI*f(7), 18*f(8), 54*f(8), f(9), 3*f(9), 6*f(9), 8*f(9), 10*f(9), 18*f(9), 21*f(9), 24*f(9), 54*f(9) &
    , 3*f(10), 9*f(10), 3*f(11), 9*f(11) ]
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
  complex(REALKIND) :: A(64)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_WQ_A(wf(:,-4),wf(:,0),wf(:,1))
  call vert_WQ_A(wf(:,-5),wf(:,-1),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,34),ZERO,0_intkind1,wf(:,4))
  call vert_QA_V(wf(:,3),wf(:,-2),wf(:,5))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,6))
  call vert_AW_Q(wf(:,-3),wf(:,-5),wf(:,7))
  call prop_A_Q(wf(:,7),Q(:,40),ZERO,0_intkind1,wf(:,8))
  call vert_QA_V(wf(:,-1),wf(:,8),wf(:,9))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,10))
  call vert_WQ_A(wf(:,-4),wf(:,-1),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,33),ZERO,0_intkind1,wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,18),ZERO,0_intkind1,wf(:,13))
  call vert_QA_V(wf(:,12),wf(:,-2),wf(:,14))
  call vert_QA_V(wf(:,13),wf(:,-3),wf(:,15))
  call vert_AW_Q(wf(:,-2),wf(:,-5),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,36),ZERO,0_intkind1,wf(:,17))
  call vert_QA_V(wf(:,0),wf(:,17),wf(:,18))
  call vert_AW_Q(wf(:,-2),wf(:,-4),wf(:,19))
  call prop_A_Q(wf(:,19),Q(:,20),ZERO,0_intkind1,wf(:,20))
  call vert_QA_V(wf(:,0),wf(:,20),wf(:,21))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,22))
  call prop_A_Q(wf(:,22),Q(:,24),ZERO,0_intkind1,wf(:,23))
  call vert_QA_V(wf(:,-1),wf(:,23),wf(:,24))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,25))
  call counter_AW_Q(wf(:,-3),wf(:,-5),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,40),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,-1),wf(:,27),wf(:,28))
  call counter_QA_V(wf(:,13),wf(:,-3),wf(:,29))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,24),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-1),wf(:,31),wf(:,32))
  call counter_QA_V(wf(:,3),wf(:,-2),wf(:,33))
  call counter_QA_V(wf(:,12),wf(:,-2),wf(:,34))
  call counter_AW_Q(wf(:,-2),wf(:,-5),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,36),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,0),wf(:,36),wf(:,37))
  call counter_AW_Q(wf(:,-2),wf(:,-4),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,20),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,0),wf(:,39),wf(:,40))
  call counter_QA_V(wf(:,-1),wf(:,8),wf(:,41))
  call counter_WQ_A(wf(:,-5),wf(:,-1),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,34),ZERO,0_intkind1,wf(:,43))
  call vert_QA_V(wf(:,43),wf(:,-3),wf(:,44))
  call counter_QA_V(wf(:,-1),wf(:,23),wf(:,45))
  call counter_WQ_A(wf(:,-4),wf(:,-1),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,18),ZERO,0_intkind1,wf(:,47))
  call vert_QA_V(wf(:,47),wf(:,-3),wf(:,48))
  call counter_QA_V(wf(:,0),wf(:,17),wf(:,49))
  call counter_WQ_A(wf(:,-5),wf(:,0),wf(:,50))
  call prop_Q_A(wf(:,50),Q(:,33),ZERO,0_intkind1,wf(:,51))
  call vert_QA_V(wf(:,51),wf(:,-2),wf(:,52))
  call counter_QA_V(wf(:,0),wf(:,20),wf(:,53))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,17),ZERO,0_intkind1,wf(:,55))
  call vert_QA_V(wf(:,55),wf(:,-2),wf(:,56))
  call counter_Q_A(ctqq,wf(:,3),Q(:,17),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,17),ZERO,0_intkind1,wf(:,58))
  call vert_QA_V(wf(:,58),wf(:,-2),wf(:,59))
  call counter_Q_A(ctqq,wf(:,4),Q(:,34),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,34),ZERO,0_intkind1,wf(:,61))
  call vert_QA_V(wf(:,61),wf(:,-3),wf(:,62))
  call counter_V_V(ctGG,wf(:,5),Q(:,21),wf(:,63))
  call counter_V_V(ctGG,wf(:,9),Q(:,42),wf(:,64))
  call counter_A_Q(ctcc,wf(:,8),Q(:,40),wf(:,65))
  call prop_A_Q(wf(:,65),Q(:,40),ZERO,0_intkind1,wf(:,66))
  call vert_QA_V(wf(:,-1),wf(:,66),wf(:,67))
  call counter_Q_A(ctqq,wf(:,12),Q(:,33),wf(:,68))
  call prop_Q_A(wf(:,68),Q(:,33),ZERO,0_intkind1,wf(:,69))
  call vert_QA_V(wf(:,69),wf(:,-2),wf(:,70))
  call counter_Q_A(ctqq,wf(:,13),Q(:,18),wf(:,71))
  call prop_Q_A(wf(:,71),Q(:,18),ZERO,0_intkind1,wf(:,72))
  call vert_QA_V(wf(:,72),wf(:,-3),wf(:,73))
  call counter_V_V(ctGG,wf(:,14),Q(:,37),wf(:,74))
  call counter_A_Q(ctqq,wf(:,17),Q(:,36),wf(:,75))
  call prop_A_Q(wf(:,75),Q(:,36),ZERO,0_intkind1,wf(:,76))
  call vert_QA_V(wf(:,0),wf(:,76),wf(:,77))
  call counter_V_V(ctGG,wf(:,18),Q(:,37),wf(:,78))
  call counter_A_Q(ctqq,wf(:,20),Q(:,20),wf(:,79))
  call prop_A_Q(wf(:,79),Q(:,20),ZERO,0_intkind1,wf(:,80))
  call vert_QA_V(wf(:,0),wf(:,80),wf(:,81))
  call counter_V_V(ctGG,wf(:,21),Q(:,21),wf(:,82))
  call counter_V_V(ctGG,wf(:,24),Q(:,26),wf(:,83))
  call counter_A_Q(ctcc,wf(:,23),Q(:,24),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,24),ZERO,0_intkind1,wf(:,85))
  call vert_QA_V(wf(:,-1),wf(:,85),wf(:,86))
  call vert_VQ_A(wf(:,5),wf(:,-1),wf(:,87))
  call prop_Q_A(wf(:,87),Q(:,23),ZERO,0_intkind1,wf(:,88))
  call vert_AV_Q(wf(:,-3),wf(:,5),wf(:,89))
  call prop_A_Q(wf(:,89),Q(:,29),ZERO,0_intkind1,wf(:,90))
  call vert_VQ_A(wf(:,15),wf(:,0),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,27),ZERO,0_intkind1,wf(:,92))
  call vert_AV_Q(wf(:,-2),wf(:,15),wf(:,93))
  call prop_A_Q(wf(:,93),Q(:,30),ZERO,0_intkind1,wf(:,94))
  call vert_VQ_A(wf(:,21),wf(:,-1),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,23),ZERO,0_intkind1,wf(:,96))
  call vert_AV_Q(wf(:,-3),wf(:,21),wf(:,97))
  call prop_A_Q(wf(:,97),Q(:,29),ZERO,0_intkind1,wf(:,98))
  call vert_VQ_A(wf(:,24),wf(:,0),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,27),ZERO,0_intkind1,wf(:,100))
  call vert_AV_Q(wf(:,-2),wf(:,24),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,30),ZERO,0_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,14),wf(:,-1),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,39),ZERO,0_intkind1,wf(:,104))
  call vert_AV_Q(wf(:,-3),wf(:,14),wf(:,105))
  call prop_A_Q(wf(:,105),Q(:,45),ZERO,0_intkind1,wf(:,106))
  call vert_VQ_A(wf(:,6),wf(:,0),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,43),ZERO,0_intkind1,wf(:,108))
  call vert_AV_Q(wf(:,-2),wf(:,6),wf(:,109))
  call prop_A_Q(wf(:,109),Q(:,46),ZERO,0_intkind1,wf(:,110))
  call vert_VQ_A(wf(:,18),wf(:,-1),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,39),ZERO,0_intkind1,wf(:,112))
  call vert_AV_Q(wf(:,-3),wf(:,18),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,45),ZERO,0_intkind1,wf(:,114))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,43),ZERO,0_intkind1,wf(:,116))
  call vert_AV_Q(wf(:,-2),wf(:,9),wf(:,117))
  call prop_A_Q(wf(:,117),Q(:,46),ZERO,0_intkind1,wf(:,118))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,17))
  den(2) = 1 / (Q(5,34))
  den(3) = 1 / (Q(5,21))
  den(6) = 1 / (Q(5,40))
  den(7) = 1 / (Q(5,42))
  den(10) = 1 / (Q(5,33))
  den(11) = 1 / (Q(5,18))
  den(12) = 1 / (Q(5,37))
  den(15) = 1 / (Q(5,36))
  den(18) = 1 / (Q(5,20))
  den(22) = 1 / (Q(5,24))
  den(23) = 1 / (Q(5,26))
  den(69) = 1 / (Q(5,23))
  den(71) = 1 / (Q(5,29))
  den(73) = 1 / (Q(5,27))
  den(75) = 1 / (Q(5,30))
  den(81) = 1 / (Q(5,39))
  den(83) = 1 / (Q(5,45))
  den(85) = 1 / (Q(5,43))
  den(87) = 1 / (Q(5,46))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(1)*den(8)
  den(13) = den(10)*den(12)
  den(14) = den(11)*den(13)
  den(16) = den(12)*den(15)
  den(17) = den(11)*den(16)
  den(19) = den(3)*den(18)
  den(20) = den(2)*den(19)
  den(21) = den(6)*den(19)
  den(24) = den(22)*den(23)
  den(25) = den(10)*den(24)
  den(26) = den(16)*den(22)
  den(27) = den(2)*den(7)
  den(28) = den(1)*den(27)
  den(29) = den(11)*den(23)
  den(30) = den(10)*den(29)
  den(31) = den(4)*den(6)
  den(32) = den(13)*den(22)
  den(33) = den(15)*den(29)
  den(34) = den(18)*den(27)
  den(35) = den(8)*den(18)
  den(36) = den(15)*den(24)
  den(37) = den(1)**2
  den(38) = den(27)*den(37)
  den(39) = den(2)**2
  den(40) = den(4)*den(39)
  den(41) = den(4)*den(27)
  den(42) = den(8)*den(37)
  den(43) = den(4)*den(8)
  den(44) = den(6)**2
  den(45) = den(4)*den(44)
  den(46) = den(10)**2
  den(47) = den(29)*den(46)
  den(48) = den(11)**2
  den(49) = den(13)*den(48)
  den(50) = den(13)*den(29)
  den(51) = den(15)**2
  den(52) = den(29)*den(51)
  den(53) = den(16)*den(29)
  den(54) = den(16)*den(48)
  den(55) = den(18)**2
  den(56) = den(27)*den(55)
  den(57) = den(19)*den(27)
  den(58) = den(19)*den(39)
  den(59) = den(8)*den(19)
  den(60) = den(8)*den(55)
  den(61) = den(19)*den(44)
  den(62) = den(24)*den(46)
  den(63) = den(13)*den(24)
  den(64) = den(22)**2
  den(65) = den(13)*den(64)
  den(66) = den(16)*den(24)
  den(67) = den(24)*den(51)
  den(68) = den(16)*den(64)
  den(70) = den(4)*den(69)
  den(72) = den(4)*den(71)
  den(74) = den(29)*den(73)
  den(76) = den(29)*den(75)
  den(77) = den(19)*den(69)
  den(78) = den(19)*den(71)
  den(79) = den(24)*den(73)
  den(80) = den(24)*den(75)
  den(82) = den(13)*den(81)
  den(84) = den(13)*den(83)
  den(86) = den(27)*den(85)
  den(88) = den(27)*den(87)
  den(89) = den(16)*den(81)
  den(90) = den(16)*den(83)
  den(91) = den(8)*den(85)
  den(92) = den(8)*den(87)
  den(93) = den(1)*den(2)
  den(94) = den(1)*den(6)
  den(95) = den(10)*den(11)
  den(96) = den(11)*den(15)
  den(97) = den(2)*den(18)
  den(98) = den(6)*den(18)
  den(99) = den(10)*den(22)
  den(100) = den(15)*den(22)
  den(101) = den(2)*den(72)
  den(102) = den(1)*den(88)
  den(103) = den(6)*den(70)
  den(104) = den(1)*den(92)
  den(105) = den(11)*den(84)
  den(106) = den(10)*den(76)
  den(107) = den(11)*den(90)
  den(108) = den(15)*den(74)
  den(109) = den(2)*den(78)
  den(110) = den(18)*den(86)
  den(111) = den(6)*den(77)
  den(112) = den(18)*den(91)
  den(113) = den(22)*den(82)
  den(114) = den(10)*den(80)
  den(115) = den(22)*den(89)
  den(116) = den(15)*den(79)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(64)

  A(1) = cont_VV(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,5),wf(:,9)) * den(9)
  A(3) = cont_VV(wf(:,14),wf(:,15)) * den(14)
  A(4) = cont_VV(wf(:,15),wf(:,18)) * den(17)
  A(5) = cont_VV(wf(:,6),wf(:,21)) * den(20)
  A(6) = cont_VV(wf(:,9),wf(:,21)) * den(21)
  A(7) = cont_VV(wf(:,14),wf(:,24)) * den(25)
  A(8) = cont_VV(wf(:,18),wf(:,24)) * den(26)

  A(9) = cont_VV(wf(:,5),wf(:,25)) * den(5)
  A(10) = cont_VV(wf(:,5),wf(:,28)) * den(9)
  A(11) = cont_VV(wf(:,14),wf(:,29)) * den(14)
  A(12) = cont_VV(wf(:,18),wf(:,29)) * den(17)
  A(13) = cont_VV(wf(:,21),wf(:,25)) * den(20)
  A(14) = cont_VV(wf(:,21),wf(:,28)) * den(21)
  A(15) = cont_VV(wf(:,14),wf(:,32)) * den(25)
  A(16) = cont_VV(wf(:,18),wf(:,32)) * den(26)
  A(17) = cont_VV(wf(:,6),wf(:,33)) * den(28)
  A(18) = cont_VV(wf(:,9),wf(:,33)) * den(9)
  A(19) = cont_VV(wf(:,15),wf(:,34)) * den(30)
  A(20) = cont_VV(wf(:,15),wf(:,37)) * den(17)
  A(21) = cont_VV(wf(:,24),wf(:,34)) * den(25)
  A(22) = cont_VV(wf(:,24),wf(:,37)) * den(26)
  A(23) = cont_VV(wf(:,6),wf(:,40)) * den(20)
  A(24) = cont_VV(wf(:,9),wf(:,40)) * den(21)
  A(25) = cont_VV(wf(:,5),wf(:,41)) * den(31)
  A(26) = cont_VV(wf(:,5),wf(:,44)) * den(5)
  A(27) = cont_VV(wf(:,21),wf(:,41)) * den(21)
  A(28) = cont_VV(wf(:,21),wf(:,44)) * den(20)
  A(29) = cont_VV(wf(:,14),wf(:,45)) * den(32)
  A(30) = cont_VV(wf(:,18),wf(:,45)) * den(26)
  A(31) = cont_VV(wf(:,14),wf(:,48)) * den(14)
  A(32) = cont_VV(wf(:,18),wf(:,48)) * den(17)
  A(33) = cont_VV(wf(:,15),wf(:,49)) * den(33)
  A(34) = cont_VV(wf(:,15),wf(:,52)) * den(14)
  A(35) = cont_VV(wf(:,6),wf(:,53)) * den(34)
  A(36) = cont_VV(wf(:,9),wf(:,53)) * den(35)
  A(37) = cont_VV(wf(:,24),wf(:,49)) * den(36)
  A(38) = cont_VV(wf(:,24),wf(:,52)) * den(25)
  A(39) = cont_VV(wf(:,6),wf(:,56)) * den(5)
  A(40) = cont_VV(wf(:,9),wf(:,56)) * den(9)
  A(41) = cont_VV(wf(:,6),wf(:,59)) * den(38)
  A(42) = cont_VV(wf(:,5),wf(:,62)) * den(40)
  A(43) = cont_VV(wf(:,6),wf(:,63)) * den(41)
  A(44) = cont_VV(wf(:,9),wf(:,59)) * den(42)
  A(45) = cont_VV(wf(:,5),wf(:,64)) * den(43)
  A(46) = cont_VV(wf(:,5),wf(:,67)) * den(45)
  A(47) = cont_VV(wf(:,15),wf(:,70)) * den(47)
  A(48) = cont_VV(wf(:,14),wf(:,73)) * den(49)
  A(49) = cont_VV(wf(:,15),wf(:,74)) * den(50)
  A(50) = cont_VV(wf(:,15),wf(:,77)) * den(52)
  A(51) = cont_VV(wf(:,15),wf(:,78)) * den(53)
  A(52) = cont_VV(wf(:,18),wf(:,73)) * den(54)
  A(53) = cont_VV(wf(:,6),wf(:,81)) * den(56)
  A(54) = cont_VV(wf(:,6),wf(:,82)) * den(57)
  A(55) = cont_VV(wf(:,21),wf(:,62)) * den(58)
  A(56) = cont_VV(wf(:,9),wf(:,82)) * den(59)
  A(57) = cont_VV(wf(:,9),wf(:,81)) * den(60)
  A(58) = cont_VV(wf(:,21),wf(:,67)) * den(61)
  A(59) = cont_VV(wf(:,24),wf(:,70)) * den(62)
  A(60) = cont_VV(wf(:,14),wf(:,83)) * den(63)
  A(61) = cont_VV(wf(:,14),wf(:,86)) * den(65)
  A(62) = cont_VV(wf(:,24),wf(:,78)) * den(66)
  A(63) = cont_VV(wf(:,24),wf(:,77)) * den(67)
  A(64) = cont_VV(wf(:,18),wf(:,86)) * den(68)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(64)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(7)-A(8))*f(1))/2._/**/REALKIND
  M1(2) = ((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(7)+A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((A(41)+A(42)+A(43)+A(44)+A(45)+A(46)+A(47)+A(48)+A(49)+A(50)+A(51)+A(52)+A(53)+A(54)+A(55)+A(56)+A(57)+A(58)+A(59) &
       +A(60)+A(61)+A(62)+A(63)+A(64))*f(2))/2._/**/REALKIND+((-A(25)-A(27)-A(29)-A(30))*f(3))/2._/**/REALKIND+((-A(9)-A(11)-A(12) &
       -A(13)-A(17)-A(18)-A(19)-A(21)-A(33)-A(35)-A(36)-A(37))*f(4))/2._/**/REALKIND+((-A(20)-A(22)-A(23)-A(24)-A(34)-A(38)-A(39) &
       -A(40))*f(5))/2._/**/REALKIND+((-A(10)-A(14)-A(15)-A(16)-A(26)-A(28)-A(31)-A(32))*f(6))/2._/**/REALKIND
  M2(2) = ((-A(41)-A(42)-A(43)-A(44)-A(45)-A(46)-A(47)-A(48)-A(49)-A(50)-A(51)-A(52)-A(53)-A(54)-A(55)-A(56)-A(57)-A(58)-A(59) &
       -A(60)-A(61)-A(62)-A(63)-A(64))*f(2))/6._/**/REALKIND+((A(25)+A(27)+A(29)+A(30))*f(3))/6._/**/REALKIND+((A(9)+A(11)+A(12) &
       +A(13)+A(17)+A(18)+A(19)+A(21)+A(33)+A(35)+A(36)+A(37))*f(4))/6._/**/REALKIND+((A(20)+A(22)+A(23)+A(24)+A(34)+A(38)+A(39) &
       +A(40))*f(5))/6._/**/REALKIND+((A(10)+A(14)+A(15)+A(16)+A(26)+A(28)+A(31)+A(32))*f(6))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_ucdxsxww_1_/**/REALKIND
