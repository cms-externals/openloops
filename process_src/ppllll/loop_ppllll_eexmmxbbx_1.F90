
module ol_colourmatrix_ppllll_eexmmxbbx_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll_eexmmxbbx_1_/**/REALKIND



module ol_forced_parameters_ppllll_eexmmxbbx_1_/**/REALKIND
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
  if (MM /= 0) write(*,101) 'MM = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllll_eexmmxbbx_1_/**/REALKIND

module ol_loop_ppllll_eexmmxbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(4)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:107)
  ! denominators
  complex(REALKIND), save :: den(102)
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
    f( 1) = (CI*eQED**4)/9._/**/REALKIND
    f( 2) = (CI*eQED**4)/3._/**/REALKIND
    f( 3) = CI*eQED**4
    f( 4) = (CI*countertermnorm*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*eQED**4*gQCD**2
    f( 7) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 8) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctVbb*eQED**4*gQCD**2
    f(10) = (CI*eQED**4*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(11) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(12) = (eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(13) = (eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(14) = eQED**4*gQCD**2*integralnorm*SwB
    f(15) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 4*f(12), 4*f(13), 4*f(14), 4*f(15) ]
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
  complex(REALKIND) :: A(66)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,2))
  call vert_AQ_S(gH,wf(:,-5),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,12),MZ,1_intkind1,wf(:,5))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,6))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,7))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,8))
  call vert_VQ_A(wf(:,7),wf(:,-4),wf(:,9))
  call vert_AV_Q(wf(:,-5),wf(:,8),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,19),MB,1_intkind1,wf(:,11))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,5),wf(:,12))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,19),MB,1_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,8),wf(:,-4),wf(:,15))
  call vert_AV_Q(wf(:,-5),wf(:,7),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,28),MB,1_intkind1,wf(:,17))
  call vert_ZQ_A(gZd,wf(:,5),wf(:,-4),wf(:,18))
  call prop_Q_A(wf(:,18),Q(:,28),MB,1_intkind1,wf(:,19))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,20))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,21))
  call vert_VQ_A(wf(:,7),wf(:,-2),wf(:,22))
  call vert_AV_Q(wf(:,-3),wf(:,21),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,7),ZERO,0_intkind1,wf(:,24))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,25))
  call prop_W_W(wf(:,25),Q(:,48),MZ,1_intkind1,wf(:,26))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,26),wf(:,27))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-2),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,7),ZERO,0_intkind1,wf(:,29))
  call vert_VQ_A(wf(:,21),wf(:,-2),wf(:,30))
  call vert_AV_Q(wf(:,-3),wf(:,7),wf(:,31))
  call prop_Q_A(wf(:,30),Q(:,52),ZERO,0_intkind1,wf(:,32))
  call vert_ZQ_A(gZl,wf(:,26),wf(:,-2),wf(:,33))
  call prop_Q_A(wf(:,33),Q(:,52),ZERO,0_intkind1,wf(:,34))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,35))
  call vert_VQ_A(wf(:,8),wf(:,0),wf(:,36))
  call vert_AV_Q(wf(:,-1),wf(:,21),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,13),ZERO,0_intkind1,wf(:,38))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,26),wf(:,39))
  call vert_ZQ_A(gZl,wf(:,5),wf(:,0),wf(:,40))
  call prop_Q_A(wf(:,40),Q(:,13),ZERO,0_intkind1,wf(:,41))
  call vert_VQ_A(wf(:,21),wf(:,0),wf(:,42))
  call vert_AV_Q(wf(:,-1),wf(:,8),wf(:,43))
  call prop_Q_A(wf(:,42),Q(:,49),ZERO,0_intkind1,wf(:,44))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,5),wf(:,45))
  call vert_ZQ_A(gZl,wf(:,26),wf(:,0),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,49),ZERO,0_intkind1,wf(:,47))
  call counter_AV_Q(wf(:,-5),wf(:,8),wf(:,48))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,5),wf(:,49))
  call counter_AV_Q(wf(:,-5),wf(:,7),wf(:,50))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,51))
  call counter_VQ_A(wf(:,8),wf(:,-4),wf(:,52))
  call prop_A_Q(wf(:,16),Q(:,35),MB,1_intkind1,wf(:,53))
  call counter_ZQ_A(gZd,wf(:,5),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,20),Q(:,35),MB,1_intkind1,wf(:,55))
  call counter_VQ_A(wf(:,7),wf(:,-4),wf(:,56))
  call prop_A_Q(wf(:,10),Q(:,44),MB,1_intkind1,wf(:,57))
  call prop_A_Q(wf(:,12),Q(:,44),MB,1_intkind1,wf(:,58))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,59))
  call counter_AQ_S(gH,wf(:,-5),wf(:,-4),wf(:,60))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,61))
  call vert_AV_Q(wf(:,-3),wf(:,61),wf(:,62))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,63))
  call prop_W_W(wf(:,63),Q(:,48),MZ,1_intkind1,wf(:,64))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,64),wf(:,65))
  call vert_VQ_A(wf(:,61),wf(:,-2),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,52),ZERO,0_intkind1,wf(:,67))
  call vert_ZQ_A(gZl,wf(:,64),wf(:,-2),wf(:,68))
  call prop_Q_A(wf(:,68),Q(:,52),ZERO,0_intkind1,wf(:,69))
  call vert_AV_Q(wf(:,-1),wf(:,61),wf(:,70))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,64),wf(:,71))
  call vert_VQ_A(wf(:,61),wf(:,0),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,49),ZERO,0_intkind1,wf(:,73))
  call vert_ZQ_A(gZl,wf(:,64),wf(:,0),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,49),ZERO,0_intkind1,wf(:,75))
  call counter_Q_A(ctbb,wf(:,11),Q(:,19),wf(:,76))
  call counter_Q_A(ctbb,wf(:,14),Q(:,19),wf(:,77))
  call counter_Q_A(ctbb,wf(:,17),Q(:,28),wf(:,78))
  call counter_Q_A(ctbb,wf(:,19),Q(:,28),wf(:,79))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,80))
  call vert_QA_Z(gZl,wf(:,24),wf(:,-3),wf(:,81))
  call prop_W_W(wf(:,81),Q(:,15),MZ,1_intkind1,wf(:,82))
  call vert_QA_V(wf(:,29),wf(:,-3),wf(:,83))
  call vert_QA_Z(gZl,wf(:,29),wf(:,-3),wf(:,84))
  call prop_W_W(wf(:,84),Q(:,15),MZ,1_intkind1,wf(:,85))
  call prop_A_Q(wf(:,31),Q(:,11),ZERO,0_intkind1,wf(:,86))
  call vert_QA_V(wf(:,-2),wf(:,86),wf(:,87))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,86),wf(:,88))
  call prop_W_W(wf(:,88),Q(:,15),MZ,1_intkind1,wf(:,89))
  call prop_A_Q(wf(:,35),Q(:,11),ZERO,0_intkind1,wf(:,90))
  call vert_QA_V(wf(:,-2),wf(:,90),wf(:,91))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,90),wf(:,92))
  call prop_W_W(wf(:,92),Q(:,15),MZ,1_intkind1,wf(:,93))
  call vert_QA_V(wf(:,38),wf(:,-1),wf(:,94))
  call vert_QA_Z(gZl,wf(:,38),wf(:,-1),wf(:,95))
  call prop_W_W(wf(:,95),Q(:,15),MZ,1_intkind1,wf(:,96))
  call vert_QA_V(wf(:,41),wf(:,-1),wf(:,97))
  call vert_QA_Z(gZl,wf(:,41),wf(:,-1),wf(:,98))
  call prop_W_W(wf(:,98),Q(:,15),MZ,1_intkind1,wf(:,99))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,100))
  call vert_QA_V(wf(:,0),wf(:,100),wf(:,101))
  call prop_A_Q(wf(:,45),Q(:,14),ZERO,0_intkind1,wf(:,102))
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
  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12) - MZ2)
  den(3) = 1 / (Q(5,48) - MH2)
  den(6) = 1 / (Q(5,3))
  den(7) = 1 / (Q(5,12))
  den(8) = 1 / (Q(5,19) - MB2)
  den(15) = 1 / (Q(5,28) - MB2)
  den(22) = 1 / (Q(5,48))
  den(23) = 1 / (Q(5,7))
  den(26) = 1 / (Q(5,48) - MZ2)
  den(31) = 1 / (Q(5,52))
  den(38) = 1 / (Q(5,13))
  den(45) = 1 / (Q(5,49))
  den(52) = 1 / (Q(5,35) - MB2)
  den(59) = 1 / (Q(5,44) - MB2)
  den(74) = 1 / (Q(5,15) - MH2)
  den(76) = 1 / (Q(5,15))
  den(78) = 1 / (Q(5,15) - MZ2)
  den(82) = 1 / (Q(5,11))
  den(93) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(9) = den(6)*den(8)
  den(10) = den(7)*den(9)
  den(11) = den(2)*den(9)
  den(12) = den(1)*den(8)
  den(13) = den(7)*den(12)
  den(14) = den(2)*den(12)
  den(16) = den(7)*den(15)
  den(17) = den(6)*den(16)
  den(18) = den(2)*den(15)
  den(19) = den(6)*den(18)
  den(20) = den(1)*den(16)
  den(21) = den(1)*den(18)
  den(24) = den(6)*den(23)
  den(25) = den(22)*den(24)
  den(27) = den(24)*den(26)
  den(28) = den(1)*den(23)
  den(29) = den(22)*den(28)
  den(30) = den(26)*den(28)
  den(32) = den(22)*den(31)
  den(33) = den(6)*den(32)
  den(34) = den(26)*den(31)
  den(35) = den(6)*den(34)
  den(36) = den(1)*den(32)
  den(37) = den(1)*den(34)
  den(39) = den(7)*den(38)
  den(40) = den(22)*den(39)
  den(41) = den(26)*den(39)
  den(42) = den(2)*den(38)
  den(43) = den(22)*den(42)
  den(44) = den(26)*den(42)
  den(46) = den(22)*den(45)
  den(47) = den(7)*den(46)
  den(48) = den(2)*den(46)
  den(49) = den(26)*den(45)
  den(50) = den(7)*den(49)
  den(51) = den(2)*den(49)
  den(53) = den(6)*den(52)
  den(54) = den(7)*den(53)
  den(55) = den(2)*den(53)
  den(56) = den(1)*den(52)
  den(57) = den(7)*den(56)
  den(58) = den(2)*den(56)
  den(60) = den(7)*den(59)
  den(61) = den(6)*den(60)
  den(62) = den(2)*den(59)
  den(63) = den(6)*den(62)
  den(64) = den(1)*den(60)
  den(65) = den(1)*den(62)
  den(66) = den(9)*den(60)
  den(67) = den(9)*den(62)
  den(68) = den(12)*den(60)
  den(69) = den(12)*den(62)
  den(70) = den(16)*den(53)
  den(71) = den(18)*den(53)
  den(72) = den(16)*den(56)
  den(73) = den(18)*den(56)
  den(75) = den(4)*den(74)
  den(77) = den(24)*den(76)
  den(79) = den(24)*den(78)
  den(80) = den(28)*den(76)
  den(81) = den(28)*den(78)
  den(83) = den(6)*den(82)
  den(84) = den(76)*den(83)
  den(85) = den(78)*den(83)
  den(86) = den(1)*den(82)
  den(87) = den(76)*den(86)
  den(88) = den(78)*den(86)
  den(89) = den(39)*den(76)
  den(90) = den(39)*den(78)
  den(91) = den(42)*den(76)
  den(92) = den(42)*den(78)
  den(94) = den(7)*den(93)
  den(95) = den(76)*den(94)
  den(96) = den(2)*den(93)
  den(97) = den(76)*den(96)
  den(98) = den(78)*den(94)
  den(99) = den(78)*den(96)
  den(100) = den(6)*den(7)
  den(101) = den(2)*den(6)
  den(102) = den(1)*den(7)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(66)

  A(1) = cont_SS(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(10)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,10),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,12),wf(:,14)) * den(14)
  A(6) = cont_QA(wf(:,16),wf(:,17)) * den(17)
  A(7) = cont_QA(wf(:,16),wf(:,19)) * den(19)
  A(8) = cont_QA(wf(:,17),wf(:,20)) * den(20)
  A(9) = cont_QA(wf(:,19),wf(:,20)) * den(21)
  A(10) = cont_QA(wf(:,23),wf(:,24)) * den(25)
  A(11) = cont_QA(wf(:,24),wf(:,27)) * den(27)
  A(12) = cont_QA(wf(:,23),wf(:,29)) * den(29)
  A(13) = cont_QA(wf(:,27),wf(:,29)) * den(30)
  A(14) = cont_QA(wf(:,31),wf(:,32)) * den(33)
  A(15) = cont_QA(wf(:,31),wf(:,34)) * den(35)
  A(16) = cont_QA(wf(:,32),wf(:,35)) * den(36)
  A(17) = cont_QA(wf(:,34),wf(:,35)) * den(37)
  A(18) = cont_QA(wf(:,37),wf(:,38)) * den(40)
  A(19) = cont_QA(wf(:,38),wf(:,39)) * den(41)
  A(20) = cont_QA(wf(:,37),wf(:,41)) * den(43)
  A(21) = cont_QA(wf(:,39),wf(:,41)) * den(44)
  A(22) = cont_QA(wf(:,43),wf(:,44)) * den(47)
  A(23) = cont_QA(wf(:,44),wf(:,45)) * den(48)
  A(24) = cont_QA(wf(:,43),wf(:,47)) * den(50)
  A(25) = cont_QA(wf(:,45),wf(:,47)) * den(51)

  A(26) = cont_QA(wf(:,11),wf(:,48)) * den(10)
  A(27) = cont_QA(wf(:,11),wf(:,49)) * den(11)
  A(28) = cont_QA(wf(:,14),wf(:,48)) * den(13)
  A(29) = cont_QA(wf(:,14),wf(:,49)) * den(14)
  A(30) = cont_QA(wf(:,17),wf(:,50)) * den(17)
  A(31) = cont_QA(wf(:,19),wf(:,50)) * den(19)
  A(32) = cont_QA(wf(:,17),wf(:,51)) * den(20)
  A(33) = cont_QA(wf(:,19),wf(:,51)) * den(21)
  A(34) = cont_QA(wf(:,52),wf(:,53)) * den(54)
  A(35) = cont_QA(wf(:,53),wf(:,54)) * den(55)
  A(36) = cont_QA(wf(:,52),wf(:,55)) * den(57)
  A(37) = cont_QA(wf(:,54),wf(:,55)) * den(58)
  A(38) = cont_QA(wf(:,56),wf(:,57)) * den(61)
  A(39) = cont_QA(wf(:,56),wf(:,58)) * den(63)
  A(40) = cont_QA(wf(:,57),wf(:,59)) * den(64)
  A(41) = cont_QA(wf(:,58),wf(:,59)) * den(65)
  A(42) = cont_SS(wf(:,6),wf(:,60)) * den(5)
  A(43) = cont_QA(wf(:,24),wf(:,62)) * den(25)
  A(44) = cont_QA(wf(:,24),wf(:,65)) * den(27)
  A(45) = cont_QA(wf(:,29),wf(:,62)) * den(29)
  A(46) = cont_QA(wf(:,29),wf(:,65)) * den(30)
  A(47) = cont_QA(wf(:,31),wf(:,67)) * den(33)
  A(48) = cont_QA(wf(:,31),wf(:,69)) * den(35)
  A(49) = cont_QA(wf(:,35),wf(:,67)) * den(36)
  A(50) = cont_QA(wf(:,35),wf(:,69)) * den(37)
  A(51) = cont_QA(wf(:,38),wf(:,70)) * den(40)
  A(52) = cont_QA(wf(:,38),wf(:,71)) * den(41)
  A(53) = cont_QA(wf(:,41),wf(:,70)) * den(43)
  A(54) = cont_QA(wf(:,41),wf(:,71)) * den(44)
  A(55) = cont_QA(wf(:,43),wf(:,73)) * den(47)
  A(56) = cont_QA(wf(:,45),wf(:,73)) * den(48)
  A(57) = cont_QA(wf(:,43),wf(:,75)) * den(50)
  A(58) = cont_QA(wf(:,45),wf(:,75)) * den(51)
  A(59) = cont_QA(wf(:,57),wf(:,76)) * den(66)
  A(60) = cont_QA(wf(:,58),wf(:,76)) * den(67)
  A(61) = cont_QA(wf(:,57),wf(:,77)) * den(68)
  A(62) = cont_QA(wf(:,58),wf(:,77)) * den(69)
  A(63) = cont_QA(wf(:,53),wf(:,78)) * den(70)
  A(64) = cont_QA(wf(:,53),wf(:,79)) * den(71)
  A(65) = cont_QA(wf(:,55),wf(:,78)) * den(72)
  A(66) = cont_QA(wf(:,55),wf(:,79)) * den(73)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(66)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(2)+A(6))*f(1)+(A(3)+A(4)+A(7)+A(8)+A(10)+A(12)+A(14)+A(16)+A(18)+A(20)+A(22)+A(23))*f(2)+(A(5)+A(9)+A(11)+A(13)+A(15) &
       +A(17)+A(19)+A(21)+A(24)+A(25))*f(3)-A(1)*f(10)

  M2(1) = (-A(59)-A(63))*f(4)+(-A(60)-A(61)-A(64)-A(65))*f(5)+(-A(62)-A(66))*f(6)+(A(26)+A(30)+A(34)+A(38))*f(7)+(A(27)+A(28) &
       +A(31)+A(32)+A(35)+A(36)+A(39)+A(40)+A(43)+A(45)+A(47)+A(49)+A(51)+A(53)+A(55)+A(56))*f(8)+(A(29)+A(33)+A(37)+A(41)+A(44) &
       +A(46)+A(48)+A(50)+A(52)+A(54)+A(57)+A(58))*f(9)-A(42)*f(11)

end subroutine colourvectors

end module ol_loop_ppllll_eexmmxbbx_1_/**/REALKIND
