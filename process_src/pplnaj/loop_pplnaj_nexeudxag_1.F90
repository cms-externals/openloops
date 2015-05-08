
module ol_colourmatrix_pplnaj_nexeudxag_1_/**/REALKIND
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
end module ol_colourmatrix_pplnaj_nexeudxag_1_/**/REALKIND



module ol_forced_parameters_pplnaj_nexeudxag_1_/**/REALKIND
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
end module ol_forced_parameters_pplnaj_nexeudxag_1_/**/REALKIND

module ol_loop_pplnaj_nexeudxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(21), c(12)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:102)
  ! denominators
  complex(REALKIND), save :: den(91)
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
    f( 1) = (CI*eQED**3*gQCD)/(6._/**/REALKIND*sw**2)
    f( 2) = (CI*eQED**3*gQCD)/(3._/**/REALKIND*sw**2)
    f( 3) = (CI*eQED**3*gQCD)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*eQED**3*gQCD**3)/(6._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*eQED**3*gQCD**3)/(3._/**/REALKIND*sw**2)
    f( 6) = (CI*countertermnorm*eQED**3*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3)/(6._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3)/(3._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3)/(3._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*eQED**3*gQCD**3*integralnorm*SwB)/(6._/**/REALKIND*sw**2)
    f(14) = (CI*eQED**3*gQCD**3*integralnorm*SwB)/(3._/**/REALKIND*sw**2)
    f(15) = (CI*eQED**3*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(16) = (eQED**3*gQCD**3*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(17) = (eQED**3*gQCD**3*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(18) = (eQED**3*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(19) = (eQED**3*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(20) = (eQED**3*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(21) = (2*eQED**3*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)

  c = [ 9*CI*f(13), 9*CI*f(14), 9*CI*f(15), f(16), 8*f(16), f(17), 8*f(17), f(18), 8*f(18), 3*f(19), 3*f(20), 3*f(21) ]
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
  complex(REALKIND) :: A(52)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,6))
  call vert_WQ_A(wf(:,4),wf(:,5),wf(:,7))
  call vert_AW_Q(wf(:,-3),wf(:,4),wf(:,8))
  call vert_VQ_A(wf(:,-5),wf(:,5),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,11),ZERO,0_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,36),ZERO,0_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,24),ZERO,0_intkind1,wf(:,14))
  call vert_WQ_A(wf(:,4),wf(:,13),wf(:,15))
  call vert_WQ_A(wf(:,4),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,19))
  call vert_QA_W(wf(:,13),wf(:,-3),wf(:,20))
  call vert_UV_W(wf(:,4),Q(:,3),wf(:,-4),Q(:,16),wf(:,21))
  call prop_W_W(wf(:,20),Q(:,44),MW,1_intkind1,wf(:,22))
  call vert_AV_Q(wf(:,6),wf(:,-4),wf(:,23))
  call vert_QA_W(wf(:,-2),wf(:,6),wf(:,24))
  call prop_W_W(wf(:,24),Q(:,44),MW,1_intkind1,wf(:,25))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,18),ZERO,0_intkind1,wf(:,27))
  call vert_QA_W(wf(:,27),wf(:,0),wf(:,28))
  call prop_W_W(wf(:,28),Q(:,19),MW,1_intkind1,wf(:,29))
  call counter_WQ_A(wf(:,4),wf(:,5),wf(:,30))
  call counter_VQ_A(wf(:,-5),wf(:,5),wf(:,31))
  call counter_WQ_A(wf(:,4),wf(:,13),wf(:,32))
  call counter_AV_Q(wf(:,14),wf(:,-5),wf(:,33))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,34))
  call counter_AV_Q(wf(:,6),wf(:,-4),wf(:,35))
  call counter_AW_Q(wf(:,-3),wf(:,4),wf(:,36))
  call prop_Q_A(wf(:,9),Q(:,52),ZERO,0_intkind1,wf(:,37))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,40),ZERO,0_intkind1,wf(:,39))
  call counter_QA_W(wf(:,13),wf(:,-3),wf(:,40))
  call prop_W_W(wf(:,21),Q(:,19),MW,1_intkind1,wf(:,41))
  call prop_Q_A(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,42))
  call vert_AV_Q(wf(:,39),wf(:,-4),wf(:,43))
  call vert_QA_W(wf(:,-2),wf(:,39),wf(:,44))
  call prop_W_W(wf(:,44),Q(:,44),MW,1_intkind1,wf(:,45))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,24),ZERO,0_intkind1,wf(:,47))
  call vert_AV_Q(wf(:,47),wf(:,-5),wf(:,48))
  call counter_WQ_A(wf(:,4),wf(:,-2),wf(:,49))
  call prop_A_Q(wf(:,17),Q(:,56),ZERO,0_intkind1,wf(:,50))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,36),ZERO,0_intkind1,wf(:,52))
  call vert_WQ_A(wf(:,4),wf(:,52),wf(:,53))
  call counter_QA_W(wf(:,-2),wf(:,6),wf(:,54))
  call prop_A_Q(wf(:,23),Q(:,56),ZERO,0_intkind1,wf(:,55))
  call vert_VQ_A(wf(:,-4),wf(:,52),wf(:,56))
  call vert_QA_W(wf(:,52),wf(:,-3),wf(:,57))
  call prop_W_W(wf(:,57),Q(:,44),MW,1_intkind1,wf(:,58))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,20),ZERO,0_intkind1,wf(:,60))
  call vert_WQ_A(wf(:,4),wf(:,60),wf(:,61))
  call vert_VQ_A(wf(:,-5),wf(:,60),wf(:,62))
  call vert_AW_Q(wf(:,6),wf(:,4),wf(:,63))
  call counter_Q_A(ctqq,wf(:,5),Q(:,20),wf(:,64))
  call prop_A_Q(wf(:,63),Q(:,43),ZERO,0_intkind1,wf(:,65))
  call counter_A_Q(ctqq,wf(:,6),Q(:,40),wf(:,66))
  call prop_Q_A(wf(:,7),Q(:,23),ZERO,0_intkind1,wf(:,67))
  call prop_Q_A(wf(:,64),Q(:,20),ZERO,0_intkind1,wf(:,68))
  call vert_VQ_A(wf(:,-5),wf(:,68),wf(:,69))
  call counter_A_Q(ctqq,wf(:,10),Q(:,11),wf(:,70))
  call vert_AW_Q(wf(:,14),wf(:,4),wf(:,71))
  call counter_Q_A(ctqq,wf(:,13),Q(:,36),wf(:,72))
  call prop_A_Q(wf(:,71),Q(:,27),ZERO,0_intkind1,wf(:,73))
  call counter_A_Q(ctqq,wf(:,14),Q(:,24),wf(:,74))
  call prop_Q_A(wf(:,15),Q(:,39),ZERO,0_intkind1,wf(:,75))
  call counter_Q_A(ctqq,wf(:,18),Q(:,7),wf(:,76))
  call prop_A_Q(wf(:,74),Q(:,24),ZERO,0_intkind1,wf(:,77))
  call vert_AV_Q(wf(:,77),wf(:,-5),wf(:,78))
  call prop_Q_A(wf(:,72),Q(:,36),ZERO,0_intkind1,wf(:,79))
  call vert_QA_W(wf(:,79),wf(:,-3),wf(:,80))
  call vert_VQ_A(wf(:,-4),wf(:,79),wf(:,81))
  call prop_A_Q(wf(:,66),Q(:,40),ZERO,0_intkind1,wf(:,82))
  call vert_QA_W(wf(:,-2),wf(:,82),wf(:,83))
  call vert_AV_Q(wf(:,82),wf(:,-4),wf(:,84))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,85))
  call vert_VQ_A(wf(:,-4),wf(:,18),wf(:,86))
  call prop_Q_A(wf(:,86),Q(:,23),ZERO,0_intkind1,wf(:,87))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,88))
  call prop_Q_A(wf(:,88),Q(:,39),ZERO,0_intkind1,wf(:,89))
  call vert_QA_V(wf(:,-2),wf(:,10),wf(:,90))
  call vert_AV_Q(wf(:,10),wf(:,-4),wf(:,91))
  call prop_A_Q(wf(:,91),Q(:,27),ZERO,0_intkind1,wf(:,92))
  call vert_AV_Q(wf(:,10),wf(:,-5),wf(:,93))
  call prop_A_Q(wf(:,93),Q(:,43),ZERO,0_intkind1,wf(:,94))
  call vert_WQ_A(wf(:,41),wf(:,-2),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,23),ZERO,0_intkind1,wf(:,96))
  call vert_AW_Q(wf(:,-3),wf(:,41),wf(:,97))
  call prop_A_Q(wf(:,97),Q(:,27),ZERO,0_intkind1,wf(:,98))
  call vert_WQ_A(wf(:,29),wf(:,-2),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,23),ZERO,0_intkind1,wf(:,100))
  call vert_AW_Q(wf(:,-3),wf(:,29),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,27),ZERO,0_intkind1,wf(:,102))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MW2)
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,11))
  den(9) = 1 / (Q(5,36))
  den(10) = 1 / (Q(5,24))
  den(13) = 1 / (Q(5,7))
  den(17) = 1 / (Q(5,44) - MW2)
  den(23) = 1 / (Q(5,18))
  den(24) = 1 / (Q(5,19) - MW2)
  den(28) = 1 / (Q(5,52))
  den(35) = 1 / (Q(5,56))
  den(42) = 1 / (Q(5,43))
  den(45) = 1 / (Q(5,23))
  den(52) = 1 / (Q(5,27))
  den(55) = 1 / (Q(5,39))
  den(71) = 1 / (Q(5,15))

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
  den(25) = den(23)*den(24)
  den(26) = den(9)*den(25)
  den(27) = den(3)*den(25)
  den(29) = den(2)*den(28)
  den(30) = den(1)*den(29)
  den(31) = den(1)*den(24)
  den(32) = den(9)*den(31)
  den(33) = den(9)*den(28)
  den(34) = den(1)*den(33)
  den(36) = den(10)*den(35)
  den(37) = den(1)*den(36)
  den(38) = den(3)*den(31)
  den(39) = den(3)*den(35)
  den(40) = den(1)*den(39)
  den(41) = den(1)*den(3)
  den(43) = den(41)*den(42)
  den(44) = den(2)*den(43)
  den(46) = den(4)*den(45)
  den(47) = den(3)*den(46)
  den(48) = den(2)**2
  den(49) = den(7)*den(48)
  den(50) = den(7)*den(29)
  den(51) = den(1)*den(10)
  den(53) = den(51)*den(52)
  den(54) = den(9)*den(53)
  den(56) = den(11)*den(55)
  den(57) = den(10)*den(56)
  den(58) = den(14)*den(36)
  den(59) = den(10)**2
  den(60) = den(14)*den(59)
  den(61) = den(9)**2
  den(62) = den(31)*den(61)
  den(63) = den(7)*den(61)
  den(64) = den(7)*den(33)
  den(65) = den(3)**2
  den(66) = den(31)*den(65)
  den(67) = den(14)*den(39)
  den(68) = den(14)*den(65)
  den(69) = den(25)*den(61)
  den(70) = den(25)*den(65)
  den(72) = den(14)*den(71)
  den(73) = den(14)*den(45)
  den(74) = den(14)*den(55)
  den(75) = den(7)*den(71)
  den(76) = den(7)*den(52)
  den(77) = den(7)*den(42)
  den(78) = den(31)*den(45)
  den(79) = den(31)*den(52)
  den(80) = den(25)*den(45)
  den(81) = den(25)*den(52)
  den(82) = den(1)*den(2)*den(3)
  den(83) = den(1)*den(9)*den(10)
  den(84) = den(2)*den(77)
  den(85) = den(10)*den(74)
  den(86) = den(9)*den(76)
  den(87) = den(9)*den(79)
  den(88) = den(3)*den(73)
  den(89) = den(3)*den(78)
  den(90) = den(9)*den(81)
  den(91) = den(3)*den(80)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(52)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_VV(wf(:,21),wf(:,22)) * den(19)
  A(7) = cont_QA(wf(:,18),wf(:,23)) * den(20)
  A(8) = cont_VV(wf(:,21),wf(:,25)) * den(22)
  A(9) = cont_VV(wf(:,20),wf(:,29)) * den(26)
  A(10) = cont_VV(wf(:,24),wf(:,29)) * den(27)

  A(11) = cont_QA(wf(:,6),wf(:,30)) * den(5)
  A(12) = cont_QA(wf(:,10),wf(:,31)) * den(8)
  A(13) = cont_QA(wf(:,14),wf(:,32)) * den(12)
  A(14) = cont_QA(wf(:,18),wf(:,33)) * den(15)
  A(15) = cont_QA(wf(:,10),wf(:,34)) * den(16)
  A(16) = cont_QA(wf(:,18),wf(:,35)) * den(20)
  A(17) = cont_QA(wf(:,36),wf(:,37)) * den(30)
  A(18) = cont_QA(wf(:,7),wf(:,39)) * den(5)
  A(19) = cont_VV(wf(:,40),wf(:,41)) * den(32)
  A(20) = cont_QA(wf(:,36),wf(:,42)) * den(34)
  A(21) = cont_QA(wf(:,18),wf(:,43)) * den(20)
  A(22) = cont_VV(wf(:,21),wf(:,45)) * den(22)
  A(23) = cont_QA(wf(:,15),wf(:,47)) * den(12)
  A(24) = cont_QA(wf(:,18),wf(:,48)) * den(15)
  A(25) = cont_QA(wf(:,49),wf(:,50)) * den(37)
  A(26) = cont_QA(wf(:,14),wf(:,53)) * den(12)
  A(27) = cont_VV(wf(:,41),wf(:,54)) * den(38)
  A(28) = cont_QA(wf(:,49),wf(:,55)) * den(40)
  A(29) = cont_QA(wf(:,10),wf(:,56)) * den(16)
  A(30) = cont_VV(wf(:,21),wf(:,58)) * den(19)
  A(31) = cont_QA(wf(:,6),wf(:,61)) * den(5)
  A(32) = cont_QA(wf(:,10),wf(:,62)) * den(8)
  A(33) = cont_VV(wf(:,29),wf(:,40)) * den(26)
  A(34) = cont_VV(wf(:,29),wf(:,44)) * den(27)
  A(35) = cont_VV(wf(:,29),wf(:,54)) * den(27)
  A(36) = cont_VV(wf(:,29),wf(:,57)) * den(26)
  A(37) = cont_QA(wf(:,64),wf(:,65)) * den(44)
  A(38) = cont_QA(wf(:,66),wf(:,67)) * den(47)
  A(39) = cont_QA(wf(:,10),wf(:,69)) * den(49)
  A(40) = cont_QA(wf(:,37),wf(:,70)) * den(50)
  A(41) = cont_QA(wf(:,72),wf(:,73)) * den(54)
  A(42) = cont_QA(wf(:,74),wf(:,75)) * den(57)
  A(43) = cont_QA(wf(:,50),wf(:,76)) * den(58)
  A(44) = cont_QA(wf(:,18),wf(:,78)) * den(60)
  A(45) = cont_VV(wf(:,41),wf(:,80)) * den(62)
  A(46) = cont_QA(wf(:,10),wf(:,81)) * den(63)
  A(47) = cont_QA(wf(:,42),wf(:,70)) * den(64)
  A(48) = cont_VV(wf(:,41),wf(:,83)) * den(66)
  A(49) = cont_QA(wf(:,55),wf(:,76)) * den(67)
  A(50) = cont_QA(wf(:,18),wf(:,84)) * den(68)
  A(51) = cont_VV(wf(:,29),wf(:,80)) * den(69)
  A(52) = cont_VV(wf(:,29),wf(:,83)) * den(70)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(52)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(3)+A(4)+A(7))*f(1)+(-A(1)-A(2)-A(5))*f(2)+(-A(6)-A(8)+A(9)+A(10))*f(3)

  M2(1) = (-A(41)-A(42)-A(43)-A(44)-A(49)-A(50))*f(4)+(A(37)+A(38)+A(39)+A(40)+A(46)+A(47))*f(5)+(A(45)+A(48)-A(51)-A(52))*f(6) &
       +(A(14)+A(21)+A(26))*f(7)+(-A(12)-A(18)-A(29))*f(8)+(-A(22)-A(30)+A(34)+A(36))*f(9)+(A(13)+A(16)+A(23)+A(24)+A(25) &
       +A(28))*f(10)+(-A(11)-A(15)-A(17)-A(20)-A(31)-A(32))*f(11)+(-A(19)-A(27)+A(33)+A(35))*f(12)

end subroutine colourvectors

end module ol_loop_pplnaj_nexeudxag_1_/**/REALKIND
