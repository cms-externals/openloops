
module ol_colourmatrix_ppllll_neeexexuxd_1_/**/REALKIND
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

  K1( 1,:) = [  3]
  K1( 2,:) = [  0]
  K1( 3,:) = [  0]
  K1( 4,:) = [  0]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  0]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [  0]
  K1(11,:) = [  0]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  4]
  K1(17,:) = [  0]
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [ -4]
  K1(22,:) = [  4]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllll_neeexexuxd_1_/**/REALKIND



module ol_forced_parameters_ppllll_neeexexuxd_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll_neeexexuxd_1_/**/REALKIND

module ol_loop_ppllll_neeexexuxd_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(18), c(5)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:104)
  ! denominators
  complex(REALKIND), save :: den(93)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,64)
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
    f( 1) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f( 2) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*cw*eQED**4)/(2._/**/REALKIND*sw**3)
    f( 4) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f( 5) = (CI*eQED**4)/(6._/**/REALKIND*sw**2)
    f( 6) = (CI*eQED**4)/(3._/**/REALKIND*sw**2)
    f( 7) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(14) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(15) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(16) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(17) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(18) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)

  c = [ 4*f(14), 4*f(15), 4*f(16), 4*f(17), 4*f(18) ]
  c = (1._/**/REALKIND / 3) * c
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
  complex(REALKIND) :: A(56)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_QA_W(wf(:,-5),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MW,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,3),Q(:,48),MW,1_intkind1,wf(:,5))
  call vert_UV_W(wf(:,2),Q(:,10),wf(:,4),Q(:,5),wf(:,6))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-3),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,10),MZ,1_intkind1,wf(:,8))
  call vert_UV_W(wf(:,8),Q(:,10),wf(:,4),Q(:,5),wf(:,9))
  call vert_AW_Q(wf(:,-4),wf(:,4),wf(:,10))
  call vert_VQ_A(wf(:,2),wf(:,-5),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,21),ZERO,0_intkind1,wf(:,12))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,-5),wf(:,13))
  call vert_AV_Q(wf(:,-4),wf(:,2),wf(:,14))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,26),ZERO,0_intkind1,wf(:,16))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,8),wf(:,17))
  call prop_A_Q(wf(:,17),Q(:,26),ZERO,0_intkind1,wf(:,18))
  call vert_WQ_A(wf(:,4),wf(:,-1),wf(:,19))
  call vert_AW_Q(wf(:,-3),wf(:,5),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,7),ZERO,0_intkind1,wf(:,21))
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,22))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,23))
  call prop_W_W(wf(:,22),Q(:,9),MW,1_intkind1,wf(:,24))
  call vert_UV_W(wf(:,23),Q(:,6),wf(:,24),Q(:,9),wf(:,25))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,26))
  call prop_W_W(wf(:,26),Q(:,6),MZ,1_intkind1,wf(:,27))
  call vert_UV_W(wf(:,27),Q(:,6),wf(:,24),Q(:,9),wf(:,28))
  call vert_AW_Q(wf(:,-4),wf(:,24),wf(:,29))
  call vert_VQ_A(wf(:,23),wf(:,-5),wf(:,30))
  call prop_A_Q(wf(:,29),Q(:,25),ZERO,0_intkind1,wf(:,31))
  call vert_ZQ_A(gZd,wf(:,27),wf(:,-5),wf(:,32))
  call vert_AV_Q(wf(:,-4),wf(:,23),wf(:,33))
  call vert_WQ_A(wf(:,24),wf(:,-5),wf(:,34))
  call prop_A_Q(wf(:,33),Q(:,22),ZERO,0_intkind1,wf(:,35))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,27),wf(:,36))
  call prop_A_Q(wf(:,36),Q(:,22),ZERO,0_intkind1,wf(:,37))
  call vert_ZQ_A(gZn,wf(:,27),wf(:,0),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,7),ZERO,0_intkind1,wf(:,39))
  call vert_WQ_A(wf(:,5),wf(:,0),wf(:,40))
  call vert_AV_Q(wf(:,-3),wf(:,23),wf(:,41))
  call prop_Q_A(wf(:,40),Q(:,49),ZERO,0_intkind1,wf(:,42))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,27),wf(:,43))
  call vert_WQ_A(wf(:,24),wf(:,-1),wf(:,44))
  call vert_AW_Q(wf(:,-2),wf(:,5),wf(:,45))
  call prop_Q_A(wf(:,44),Q(:,11),ZERO,0_intkind1,wf(:,46))
  call vert_ZQ_A(gZn,wf(:,8),wf(:,0),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,11),ZERO,0_intkind1,wf(:,48))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,49))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,8),wf(:,50))
  call counter_VQ_A(wf(:,2),wf(:,-5),wf(:,51))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,-5),wf(:,52))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,53))
  call counter_AV_Q(wf(:,-4),wf(:,2),wf(:,54))
  call prop_Q_A(wf(:,15),Q(:,37),ZERO,0_intkind1,wf(:,55))
  call counter_AZ_Q(gZu,wf(:,-4),wf(:,8),wf(:,56))
  call counter_AW_Q(wf(:,-4),wf(:,4),wf(:,57))
  call prop_Q_A(wf(:,11),Q(:,42),ZERO,0_intkind1,wf(:,58))
  call prop_Q_A(wf(:,13),Q(:,42),ZERO,0_intkind1,wf(:,59))
  call counter_QA_W(wf(:,-5),wf(:,-4),wf(:,60))
  call prop_W_W(wf(:,60),Q(:,48),MW,1_intkind1,wf(:,61))
  call vert_AW_Q(wf(:,-3),wf(:,61),wf(:,62))
  call counter_VQ_A(wf(:,23),wf(:,-5),wf(:,63))
  call counter_ZQ_A(gZd,wf(:,27),wf(:,-5),wf(:,64))
  call counter_WQ_A(wf(:,24),wf(:,-5),wf(:,65))
  call counter_AV_Q(wf(:,-4),wf(:,23),wf(:,66))
  call prop_Q_A(wf(:,34),Q(:,41),ZERO,0_intkind1,wf(:,67))
  call counter_AZ_Q(gZu,wf(:,-4),wf(:,27),wf(:,68))
  call counter_AW_Q(wf(:,-4),wf(:,24),wf(:,69))
  call prop_Q_A(wf(:,30),Q(:,38),ZERO,0_intkind1,wf(:,70))
  call prop_Q_A(wf(:,32),Q(:,38),ZERO,0_intkind1,wf(:,71))
  call vert_WQ_A(wf(:,61),wf(:,0),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,49),ZERO,0_intkind1,wf(:,73))
  call vert_AW_Q(wf(:,-2),wf(:,61),wf(:,74))
  call counter_A_Q(ctqq,wf(:,12),Q(:,21),wf(:,75))
  call counter_A_Q(ctqq,wf(:,16),Q(:,26),wf(:,76))
  call counter_A_Q(ctqq,wf(:,18),Q(:,26),wf(:,77))
  call counter_A_Q(ctqq,wf(:,31),Q(:,25),wf(:,78))
  call counter_A_Q(ctqq,wf(:,35),Q(:,22),wf(:,79))
  call counter_A_Q(ctqq,wf(:,37),Q(:,22),wf(:,80))
  call prop_W_W(wf(:,6),Q(:,15),MW,1_intkind1,wf(:,81))
  call prop_W_W(wf(:,9),Q(:,15),MW,1_intkind1,wf(:,82))
  call vert_QA_W(wf(:,21),wf(:,-3),wf(:,83))
  call prop_W_W(wf(:,83),Q(:,15),MW,1_intkind1,wf(:,84))
  call prop_W_W(wf(:,25),Q(:,15),MW,1_intkind1,wf(:,85))
  call prop_W_W(wf(:,28),Q(:,15),MW,1_intkind1,wf(:,86))
  call vert_QA_W(wf(:,39),wf(:,-3),wf(:,87))
  call prop_W_W(wf(:,87),Q(:,15),MW,1_intkind1,wf(:,88))
  call prop_A_Q(wf(:,41),Q(:,14),ZERO,0_intkind1,wf(:,89))
  call vert_QA_W(wf(:,0),wf(:,89),wf(:,90))
  call prop_W_W(wf(:,90),Q(:,15),MW,1_intkind1,wf(:,91))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,92))
  call vert_QA_W(wf(:,0),wf(:,92),wf(:,93))
  call prop_W_W(wf(:,93),Q(:,15),MW,1_intkind1,wf(:,94))
  call vert_QA_W(wf(:,46),wf(:,-2),wf(:,95))
  call prop_W_W(wf(:,95),Q(:,15),MW,1_intkind1,wf(:,96))
  call vert_QA_W(wf(:,48),wf(:,-2),wf(:,97))
  call prop_W_W(wf(:,97),Q(:,15),MW,1_intkind1,wf(:,98))
  call prop_A_Q(wf(:,49),Q(:,14),ZERO,0_intkind1,wf(:,99))
  call vert_QA_W(wf(:,0),wf(:,99),wf(:,100))
  call prop_W_W(wf(:,100),Q(:,15),MW,1_intkind1,wf(:,101))
  call prop_A_Q(wf(:,50),Q(:,14),ZERO,0_intkind1,wf(:,102))
  call vert_QA_W(wf(:,0),wf(:,102),wf(:,103))
  call prop_W_W(wf(:,103),Q(:,15),MW,1_intkind1,wf(:,104))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MW2)
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,48) - MW2)
  den(6) = 1 / (Q(5,10) - MZ2)
  den(9) = 1 / (Q(5,21))
  den(13) = 1 / (Q(5,26))
  den(18) = 1 / (Q(5,7))
  den(21) = 1 / (Q(5,9) - MW2)
  den(22) = 1 / (Q(5,6))
  den(25) = 1 / (Q(5,6) - MZ2)
  den(28) = 1 / (Q(5,25))
  den(32) = 1 / (Q(5,22))
  den(39) = 1 / (Q(5,49))
  den(43) = 1 / (Q(5,11))
  den(50) = 1 / (Q(5,37))
  den(54) = 1 / (Q(5,42))
  den(59) = 1 / (Q(5,41))
  den(63) = 1 / (Q(5,38))
  den(76) = 1 / (Q(5,15) - MW2)
  den(83) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(12) = den(6)*den(10)
  den(14) = den(2)*den(13)
  den(15) = den(1)*den(14)
  den(16) = den(6)*den(13)
  den(17) = den(1)*den(16)
  den(19) = den(1)*den(18)
  den(20) = den(3)*den(19)
  den(23) = den(21)*den(22)
  den(24) = den(3)*den(23)
  den(26) = den(21)*den(25)
  den(27) = den(3)*den(26)
  den(29) = den(21)*den(28)
  den(30) = den(22)*den(29)
  den(31) = den(25)*den(29)
  den(33) = den(22)*den(32)
  den(34) = den(21)*den(33)
  den(35) = den(25)*den(32)
  den(36) = den(21)*den(35)
  den(37) = den(18)*den(25)
  den(38) = den(3)*den(37)
  den(40) = den(3)*den(39)
  den(41) = den(22)*den(40)
  den(42) = den(25)*den(40)
  den(44) = den(21)*den(43)
  den(45) = den(3)*den(44)
  den(46) = den(6)*den(43)
  den(47) = den(3)*den(46)
  den(48) = den(2)*den(40)
  den(49) = den(6)*den(40)
  den(51) = den(1)*den(50)
  den(52) = den(2)*den(51)
  den(53) = den(6)*den(51)
  den(55) = den(2)*den(54)
  den(56) = den(1)*den(55)
  den(57) = den(6)*den(54)
  den(58) = den(1)*den(57)
  den(60) = den(21)*den(59)
  den(61) = den(22)*den(60)
  den(62) = den(25)*den(60)
  den(64) = den(22)*den(63)
  den(65) = den(21)*den(64)
  den(66) = den(25)*den(63)
  den(67) = den(21)*den(66)
  den(68) = den(10)*den(55)
  den(69) = den(10)*den(57)
  den(70) = den(14)*den(51)
  den(71) = den(16)*den(51)
  den(72) = den(29)*den(64)
  den(73) = den(29)*den(66)
  den(74) = den(33)*den(60)
  den(75) = den(35)*den(60)
  den(77) = den(4)*den(76)
  den(78) = den(7)*den(76)
  den(79) = den(19)*den(76)
  den(80) = den(23)*den(76)
  den(81) = den(26)*den(76)
  den(82) = den(37)*den(76)
  den(84) = den(22)*den(83)
  den(85) = den(76)*den(84)
  den(86) = den(25)*den(83)
  den(87) = den(76)*den(86)
  den(88) = den(44)*den(76)
  den(89) = den(46)*den(76)
  den(90) = den(2)*den(83)
  den(91) = den(76)*den(90)
  den(92) = den(6)*den(83)
  den(93) = den(76)*den(92)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(56)

  A(1) = cont_VV(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,5),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(5) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(6) = cont_QA(wf(:,15),wf(:,18)) * den(17)
  A(7) = cont_QA(wf(:,20),wf(:,21)) * den(20)
  A(8) = cont_VV(wf(:,5),wf(:,25)) * den(24)
  A(9) = cont_VV(wf(:,5),wf(:,28)) * den(27)
  A(10) = cont_QA(wf(:,30),wf(:,31)) * den(30)
  A(11) = cont_QA(wf(:,31),wf(:,32)) * den(31)
  A(12) = cont_QA(wf(:,34),wf(:,35)) * den(34)
  A(13) = cont_QA(wf(:,34),wf(:,37)) * den(36)
  A(14) = cont_QA(wf(:,20),wf(:,39)) * den(38)
  A(15) = cont_QA(wf(:,41),wf(:,42)) * den(41)
  A(16) = cont_QA(wf(:,42),wf(:,43)) * den(42)
  A(17) = cont_QA(wf(:,45),wf(:,46)) * den(45)
  A(18) = cont_QA(wf(:,45),wf(:,48)) * den(47)
  A(19) = cont_QA(wf(:,42),wf(:,49)) * den(48)
  A(20) = cont_QA(wf(:,42),wf(:,50)) * den(49)

  A(21) = cont_QA(wf(:,12),wf(:,51)) * den(11)
  A(22) = cont_QA(wf(:,12),wf(:,52)) * den(12)
  A(23) = cont_QA(wf(:,16),wf(:,53)) * den(15)
  A(24) = cont_QA(wf(:,18),wf(:,53)) * den(17)
  A(25) = cont_QA(wf(:,54),wf(:,55)) * den(52)
  A(26) = cont_QA(wf(:,55),wf(:,56)) * den(53)
  A(27) = cont_QA(wf(:,57),wf(:,58)) * den(56)
  A(28) = cont_QA(wf(:,57),wf(:,59)) * den(58)
  A(29) = cont_VV(wf(:,6),wf(:,61)) * den(5)
  A(30) = cont_VV(wf(:,9),wf(:,61)) * den(8)
  A(31) = cont_QA(wf(:,21),wf(:,62)) * den(20)
  A(32) = cont_QA(wf(:,31),wf(:,63)) * den(30)
  A(33) = cont_QA(wf(:,31),wf(:,64)) * den(31)
  A(34) = cont_QA(wf(:,35),wf(:,65)) * den(34)
  A(35) = cont_QA(wf(:,37),wf(:,65)) * den(36)
  A(36) = cont_QA(wf(:,66),wf(:,67)) * den(61)
  A(37) = cont_QA(wf(:,67),wf(:,68)) * den(62)
  A(38) = cont_QA(wf(:,69),wf(:,70)) * den(65)
  A(39) = cont_QA(wf(:,69),wf(:,71)) * den(67)
  A(40) = cont_VV(wf(:,25),wf(:,61)) * den(24)
  A(41) = cont_VV(wf(:,28),wf(:,61)) * den(27)
  A(42) = cont_QA(wf(:,39),wf(:,62)) * den(38)
  A(43) = cont_QA(wf(:,41),wf(:,73)) * den(41)
  A(44) = cont_QA(wf(:,43),wf(:,73)) * den(42)
  A(45) = cont_QA(wf(:,46),wf(:,74)) * den(45)
  A(46) = cont_QA(wf(:,48),wf(:,74)) * den(47)
  A(47) = cont_QA(wf(:,49),wf(:,73)) * den(48)
  A(48) = cont_QA(wf(:,50),wf(:,73)) * den(49)
  A(49) = cont_QA(wf(:,58),wf(:,75)) * den(68)
  A(50) = cont_QA(wf(:,59),wf(:,75)) * den(69)
  A(51) = cont_QA(wf(:,55),wf(:,76)) * den(70)
  A(52) = cont_QA(wf(:,55),wf(:,77)) * den(71)
  A(53) = cont_QA(wf(:,70),wf(:,78)) * den(72)
  A(54) = cont_QA(wf(:,71),wf(:,78)) * den(73)
  A(55) = cont_QA(wf(:,67),wf(:,79)) * den(74)
  A(56) = cont_QA(wf(:,67),wf(:,80)) * den(75)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(56)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(7)-A(17))*f(1)+(A(2)-A(9))*f(3)+(A(3)-A(10))*f(5)+(-A(5)+A(12))*f(6)+(-A(1)+A(4)+A(6)+A(8)-A(11)-A(13)-A(14)-A(15) &
       -A(16)+A(18)+A(19)+A(20))*f(7)

  M2(1) = (A(31)-A(45))*f(2)+(A(30)-A(41))*f(4)+(-A(49)+A(53))*f(8)+(A(51)-A(55))*f(9)+(-A(50)-A(52)+A(54)+A(56))*f(10)+(A(21) &
       +A(27)-A(32)-A(38))*f(11)+(-A(23)-A(25)+A(34)+A(36))*f(12)+(A(22)+A(24)+A(26)+A(28)-A(29)-A(33)-A(35)-A(37)-A(39)+A(40) &
       -A(42)-A(43)-A(44)+A(46)+A(47)+A(48))*f(13)

end subroutine colourvectors

end module ol_loop_ppllll_neeexexuxd_1_/**/REALKIND
