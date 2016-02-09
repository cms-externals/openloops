
module ol_colourmatrix_ppllll_eexmmxddx_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll_eexmmxddx_1_/**/REALKIND



module ol_forced_parameters_ppllll_eexmmxddx_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllll_eexmmxddx_1_/**/REALKIND

module ol_loop_ppllll_eexmmxddx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:104)
  ! denominators
  complex(REALKIND), save :: den(98)
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
    f( 7) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 8) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctVqq*eQED**4*gQCD**2
    f(10) = (eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(11) = (eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(12) = eQED**4*gQCD**2*integralnorm*SwB

  c = [ 4*f(10), 4*f(11), 4*f(12) ]
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
  complex(REALKIND) :: A(64)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,3))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,19),ZERO,0_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,12),MZ,1_intkind1,wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,8))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,3),MZ,1_intkind1,wf(:,10))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,11),Q(:,19),ZERO,0_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,13))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,28),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-4),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,28),ZERO,0_intkind1,wf(:,17))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,10),wf(:,18))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,19))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,20))
  call vert_AV_Q(wf(:,-3),wf(:,19),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,7),ZERO,0_intkind1,wf(:,22))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,23))
  call prop_W_W(wf(:,23),Q(:,48),MZ,1_intkind1,wf(:,24))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,24),wf(:,25))
  call vert_ZQ_A(gZl,wf(:,10),wf(:,-2),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,7),ZERO,0_intkind1,wf(:,27))
  call vert_VQ_A(wf(:,19),wf(:,-2),wf(:,28))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,29))
  call prop_Q_A(wf(:,28),Q(:,52),ZERO,0_intkind1,wf(:,30))
  call vert_ZQ_A(gZl,wf(:,24),wf(:,-2),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,52),ZERO,0_intkind1,wf(:,32))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,10),wf(:,33))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,34))
  call vert_AV_Q(wf(:,-1),wf(:,19),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,13),ZERO,0_intkind1,wf(:,36))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,24),wf(:,37))
  call vert_ZQ_A(gZl,wf(:,7),wf(:,0),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,13),ZERO,0_intkind1,wf(:,39))
  call vert_VQ_A(wf(:,19),wf(:,0),wf(:,40))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,41))
  call prop_Q_A(wf(:,40),Q(:,49),ZERO,0_intkind1,wf(:,42))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,7),wf(:,43))
  call vert_ZQ_A(gZl,wf(:,24),wf(:,0),wf(:,44))
  call prop_Q_A(wf(:,44),Q(:,49),ZERO,0_intkind1,wf(:,45))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,46))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,47))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,48))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,10),wf(:,49))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,50))
  call prop_A_Q(wf(:,14),Q(:,35),ZERO,0_intkind1,wf(:,51))
  call counter_ZQ_A(gZd,wf(:,7),wf(:,-4),wf(:,52))
  call prop_A_Q(wf(:,18),Q(:,35),ZERO,0_intkind1,wf(:,53))
  call counter_VQ_A(wf(:,1),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,4),Q(:,44),ZERO,0_intkind1,wf(:,55))
  call prop_A_Q(wf(:,8),Q(:,44),ZERO,0_intkind1,wf(:,56))
  call counter_ZQ_A(gZd,wf(:,10),wf(:,-4),wf(:,57))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,58))
  call vert_AV_Q(wf(:,-3),wf(:,58),wf(:,59))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,60))
  call prop_W_W(wf(:,60),Q(:,48),MZ,1_intkind1,wf(:,61))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,61),wf(:,62))
  call vert_VQ_A(wf(:,58),wf(:,-2),wf(:,63))
  call prop_Q_A(wf(:,63),Q(:,52),ZERO,0_intkind1,wf(:,64))
  call vert_ZQ_A(gZl,wf(:,61),wf(:,-2),wf(:,65))
  call prop_Q_A(wf(:,65),Q(:,52),ZERO,0_intkind1,wf(:,66))
  call vert_AV_Q(wf(:,-1),wf(:,58),wf(:,67))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,61),wf(:,68))
  call vert_VQ_A(wf(:,58),wf(:,0),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,49),ZERO,0_intkind1,wf(:,70))
  call vert_ZQ_A(gZl,wf(:,61),wf(:,0),wf(:,71))
  call prop_Q_A(wf(:,71),Q(:,49),ZERO,0_intkind1,wf(:,72))
  call counter_Q_A(ctqq,wf(:,5),Q(:,19),wf(:,73))
  call counter_Q_A(ctqq,wf(:,12),Q(:,19),wf(:,74))
  call counter_Q_A(ctqq,wf(:,15),Q(:,28),wf(:,75))
  call counter_Q_A(ctqq,wf(:,17),Q(:,28),wf(:,76))
  call vert_QA_V(wf(:,22),wf(:,-3),wf(:,77))
  call vert_QA_Z(gZl,wf(:,22),wf(:,-3),wf(:,78))
  call prop_W_W(wf(:,78),Q(:,15),MZ,1_intkind1,wf(:,79))
  call vert_QA_V(wf(:,27),wf(:,-3),wf(:,80))
  call vert_QA_Z(gZl,wf(:,27),wf(:,-3),wf(:,81))
  call prop_W_W(wf(:,81),Q(:,15),MZ,1_intkind1,wf(:,82))
  call prop_A_Q(wf(:,29),Q(:,11),ZERO,0_intkind1,wf(:,83))
  call vert_QA_V(wf(:,-2),wf(:,83),wf(:,84))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,83),wf(:,85))
  call prop_W_W(wf(:,85),Q(:,15),MZ,1_intkind1,wf(:,86))
  call prop_A_Q(wf(:,33),Q(:,11),ZERO,0_intkind1,wf(:,87))
  call vert_QA_V(wf(:,-2),wf(:,87),wf(:,88))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,87),wf(:,89))
  call prop_W_W(wf(:,89),Q(:,15),MZ,1_intkind1,wf(:,90))
  call vert_QA_V(wf(:,36),wf(:,-1),wf(:,91))
  call vert_QA_Z(gZl,wf(:,36),wf(:,-1),wf(:,92))
  call prop_W_W(wf(:,92),Q(:,15),MZ,1_intkind1,wf(:,93))
  call vert_QA_V(wf(:,39),wf(:,-1),wf(:,94))
  call vert_QA_Z(gZl,wf(:,39),wf(:,-1),wf(:,95))
  call prop_W_W(wf(:,95),Q(:,15),MZ,1_intkind1,wf(:,96))
  call prop_A_Q(wf(:,41),Q(:,14),ZERO,0_intkind1,wf(:,97))
  call vert_QA_V(wf(:,0),wf(:,97),wf(:,98))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,99))
  call vert_QA_V(wf(:,0),wf(:,99),wf(:,100))
  call vert_QA_Z(gZl,wf(:,0),wf(:,97),wf(:,101))
  call prop_W_W(wf(:,101),Q(:,15),MZ,1_intkind1,wf(:,102))
  call vert_QA_Z(gZl,wf(:,0),wf(:,99),wf(:,103))
  call prop_W_W(wf(:,103),Q(:,15),MZ,1_intkind1,wf(:,104))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,19))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(8) = 1 / (Q(5,3) - MZ2)
  den(12) = 1 / (Q(5,28))
  den(19) = 1 / (Q(5,48))
  den(20) = 1 / (Q(5,7))
  den(23) = 1 / (Q(5,48) - MZ2)
  den(28) = 1 / (Q(5,52))
  den(35) = 1 / (Q(5,13))
  den(42) = 1 / (Q(5,49))
  den(49) = 1 / (Q(5,35))
  den(56) = 1 / (Q(5,44))
  den(71) = 1 / (Q(5,15))
  den(73) = 1 / (Q(5,15) - MZ2)
  den(77) = 1 / (Q(5,11))
  den(88) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(3)*den(8)
  den(10) = den(2)*den(9)
  den(11) = den(6)*den(9)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(15) = den(6)*den(12)
  den(16) = den(1)*den(15)
  den(17) = den(8)*den(13)
  den(18) = den(8)*den(15)
  den(21) = den(1)*den(20)
  den(22) = den(19)*den(21)
  den(24) = den(21)*den(23)
  den(25) = den(8)*den(20)
  den(26) = den(19)*den(25)
  den(27) = den(23)*den(25)
  den(29) = den(19)*den(28)
  den(30) = den(1)*den(29)
  den(31) = den(23)*den(28)
  den(32) = den(1)*den(31)
  den(33) = den(8)*den(29)
  den(34) = den(8)*den(31)
  den(36) = den(2)*den(35)
  den(37) = den(19)*den(36)
  den(38) = den(23)*den(36)
  den(39) = den(6)*den(35)
  den(40) = den(19)*den(39)
  den(41) = den(23)*den(39)
  den(43) = den(19)*den(42)
  den(44) = den(2)*den(43)
  den(45) = den(6)*den(43)
  den(46) = den(23)*den(42)
  den(47) = den(2)*den(46)
  den(48) = den(6)*den(46)
  den(50) = den(1)*den(49)
  den(51) = den(2)*den(50)
  den(52) = den(6)*den(50)
  den(53) = den(8)*den(49)
  den(54) = den(2)*den(53)
  den(55) = den(6)*den(53)
  den(57) = den(2)*den(56)
  den(58) = den(1)*den(57)
  den(59) = den(6)*den(56)
  den(60) = den(1)*den(59)
  den(61) = den(8)*den(57)
  den(62) = den(8)*den(59)
  den(63) = den(4)*den(57)
  den(64) = den(4)*den(59)
  den(65) = den(9)*den(57)
  den(66) = den(9)*den(59)
  den(67) = den(13)*den(50)
  den(68) = den(15)*den(50)
  den(69) = den(13)*den(53)
  den(70) = den(15)*den(53)
  den(72) = den(21)*den(71)
  den(74) = den(21)*den(73)
  den(75) = den(25)*den(71)
  den(76) = den(25)*den(73)
  den(78) = den(1)*den(77)
  den(79) = den(71)*den(78)
  den(80) = den(73)*den(78)
  den(81) = den(8)*den(77)
  den(82) = den(71)*den(81)
  den(83) = den(73)*den(81)
  den(84) = den(36)*den(71)
  den(85) = den(36)*den(73)
  den(86) = den(39)*den(71)
  den(87) = den(39)*den(73)
  den(89) = den(2)*den(88)
  den(90) = den(71)*den(89)
  den(91) = den(6)*den(88)
  den(92) = den(71)*den(91)
  den(93) = den(73)*den(89)
  den(94) = den(73)*den(91)
  den(95) = den(1)*den(2)
  den(96) = den(1)*den(6)
  den(97) = den(2)*den(8)
  den(98) = den(6)*den(8)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(64)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(5)
  A(2) = cont_QA(wf(:,5),wf(:,8)) * den(7)
  A(3) = cont_QA(wf(:,4),wf(:,12)) * den(10)
  A(4) = cont_QA(wf(:,8),wf(:,12)) * den(11)
  A(5) = cont_QA(wf(:,14),wf(:,15)) * den(14)
  A(6) = cont_QA(wf(:,14),wf(:,17)) * den(16)
  A(7) = cont_QA(wf(:,15),wf(:,18)) * den(17)
  A(8) = cont_QA(wf(:,17),wf(:,18)) * den(18)
  A(9) = cont_QA(wf(:,21),wf(:,22)) * den(22)
  A(10) = cont_QA(wf(:,22),wf(:,25)) * den(24)
  A(11) = cont_QA(wf(:,21),wf(:,27)) * den(26)
  A(12) = cont_QA(wf(:,25),wf(:,27)) * den(27)
  A(13) = cont_QA(wf(:,29),wf(:,30)) * den(30)
  A(14) = cont_QA(wf(:,29),wf(:,32)) * den(32)
  A(15) = cont_QA(wf(:,30),wf(:,33)) * den(33)
  A(16) = cont_QA(wf(:,32),wf(:,33)) * den(34)
  A(17) = cont_QA(wf(:,35),wf(:,36)) * den(37)
  A(18) = cont_QA(wf(:,36),wf(:,37)) * den(38)
  A(19) = cont_QA(wf(:,35),wf(:,39)) * den(40)
  A(20) = cont_QA(wf(:,37),wf(:,39)) * den(41)
  A(21) = cont_QA(wf(:,41),wf(:,42)) * den(44)
  A(22) = cont_QA(wf(:,42),wf(:,43)) * den(45)
  A(23) = cont_QA(wf(:,41),wf(:,45)) * den(47)
  A(24) = cont_QA(wf(:,43),wf(:,45)) * den(48)

  A(25) = cont_QA(wf(:,5),wf(:,46)) * den(5)
  A(26) = cont_QA(wf(:,5),wf(:,47)) * den(7)
  A(27) = cont_QA(wf(:,12),wf(:,46)) * den(10)
  A(28) = cont_QA(wf(:,12),wf(:,47)) * den(11)
  A(29) = cont_QA(wf(:,15),wf(:,48)) * den(14)
  A(30) = cont_QA(wf(:,17),wf(:,48)) * den(16)
  A(31) = cont_QA(wf(:,15),wf(:,49)) * den(17)
  A(32) = cont_QA(wf(:,17),wf(:,49)) * den(18)
  A(33) = cont_QA(wf(:,50),wf(:,51)) * den(51)
  A(34) = cont_QA(wf(:,51),wf(:,52)) * den(52)
  A(35) = cont_QA(wf(:,50),wf(:,53)) * den(54)
  A(36) = cont_QA(wf(:,52),wf(:,53)) * den(55)
  A(37) = cont_QA(wf(:,54),wf(:,55)) * den(58)
  A(38) = cont_QA(wf(:,54),wf(:,56)) * den(60)
  A(39) = cont_QA(wf(:,55),wf(:,57)) * den(61)
  A(40) = cont_QA(wf(:,56),wf(:,57)) * den(62)
  A(41) = cont_QA(wf(:,22),wf(:,59)) * den(22)
  A(42) = cont_QA(wf(:,22),wf(:,62)) * den(24)
  A(43) = cont_QA(wf(:,27),wf(:,59)) * den(26)
  A(44) = cont_QA(wf(:,27),wf(:,62)) * den(27)
  A(45) = cont_QA(wf(:,29),wf(:,64)) * den(30)
  A(46) = cont_QA(wf(:,29),wf(:,66)) * den(32)
  A(47) = cont_QA(wf(:,33),wf(:,64)) * den(33)
  A(48) = cont_QA(wf(:,33),wf(:,66)) * den(34)
  A(49) = cont_QA(wf(:,36),wf(:,67)) * den(37)
  A(50) = cont_QA(wf(:,36),wf(:,68)) * den(38)
  A(51) = cont_QA(wf(:,39),wf(:,67)) * den(40)
  A(52) = cont_QA(wf(:,39),wf(:,68)) * den(41)
  A(53) = cont_QA(wf(:,41),wf(:,70)) * den(44)
  A(54) = cont_QA(wf(:,43),wf(:,70)) * den(45)
  A(55) = cont_QA(wf(:,41),wf(:,72)) * den(47)
  A(56) = cont_QA(wf(:,43),wf(:,72)) * den(48)
  A(57) = cont_QA(wf(:,55),wf(:,73)) * den(63)
  A(58) = cont_QA(wf(:,56),wf(:,73)) * den(64)
  A(59) = cont_QA(wf(:,55),wf(:,74)) * den(65)
  A(60) = cont_QA(wf(:,56),wf(:,74)) * den(66)
  A(61) = cont_QA(wf(:,51),wf(:,75)) * den(67)
  A(62) = cont_QA(wf(:,51),wf(:,76)) * den(68)
  A(63) = cont_QA(wf(:,53),wf(:,75)) * den(69)
  A(64) = cont_QA(wf(:,53),wf(:,76)) * den(70)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(64)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(5))*f(1)+(A(2)+A(3)+A(6)+A(7)+A(9)+A(11)+A(13)+A(15)+A(17)+A(19)+A(21)+A(22))*f(2)+(A(4)+A(8)+A(10)+A(12)+A(14) &
       +A(16)+A(18)+A(20)+A(23)+A(24))*f(3)

  M2(1) = (-A(57)-A(61))*f(4)+(-A(58)-A(59)-A(62)-A(63))*f(5)+(-A(60)-A(64))*f(6)+(A(25)+A(29)+A(33)+A(37))*f(7)+(A(26)+A(27) &
       +A(30)+A(31)+A(34)+A(35)+A(38)+A(39)+A(41)+A(43)+A(45)+A(47)+A(49)+A(51)+A(53)+A(54))*f(8)+(A(28)+A(32)+A(36)+A(40)+A(42) &
       +A(44)+A(46)+A(48)+A(50)+A(52)+A(55)+A(56))*f(9)

end subroutine colourvectors

end module ol_loop_ppllll_eexmmxddx_1_/**/REALKIND
