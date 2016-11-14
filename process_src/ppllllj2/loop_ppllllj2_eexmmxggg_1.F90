
module ol_colourmatrix_ppllllj2_eexmmxggg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_eexmmxggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_eexmmxggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_eexmmxggg_1_/**/REALKIND

module ol_loop_ppllllj2_eexmmxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(28), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:101)
  ! denominators
  complex(REALKIND), save :: den(169)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB, DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*countertermnorm*eQED**4*gQCD**3
    f( 2) = countertermnorm*ctAAGG*eQED**4*gQCD**3
    f( 3) = countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 4) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 5) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 6) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f( 7) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 9) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(11) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = (8*CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(13) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f(14) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(15) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f(16) = (eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(17) = (2*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(18) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(19) = (4*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(20) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(21) = (8*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(22) = eQED**4*gQCD**3*integralnorm*SwF
    f(23) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(24) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(25) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(26) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(27) = (CI*eQED**4*gQCD**3*integralnorm*MT*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(28) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND) :: A(71)
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
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-6),wf(:,4))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,12),MZ,1_intkind1,wf(:,6))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-6),wf(:,7))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,8))
  call prop_W_W(wf(:,8),Q(:,3),MZ,1_intkind1,wf(:,9))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-6),wf(:,10))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-6),wf(:,11))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,12))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-5),wf(:,13))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-5),wf(:,14))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-5),wf(:,15))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-5),wf(:,16))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,17))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-4),wf(:,18))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-4),wf(:,19))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-4),wf(:,20))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-4),wf(:,21))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,22))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,23))
  call vert_AV_Q(wf(:,-3),wf(:,22),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,7),ZERO,0_intkind1,wf(:,25))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,26))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,27))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,28))
  call prop_W_W(wf(:,28),Q(:,112),MZ,1_intkind1,wf(:,29))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,29),wf(:,30))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,31))
  call prop_W_W(wf(:,31),Q(:,112),MZ,1_intkind1,wf(:,32))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,32),wf(:,33))
  call vert_ZQ_A(gZl,wf(:,9),wf(:,-2),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,7),ZERO,0_intkind1,wf(:,35))
  call vert_VQ_A(wf(:,22),wf(:,-2),wf(:,36))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,116),ZERO,0_intkind1,wf(:,38))
  call vert_VQ_A(wf(:,26),wf(:,-2),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,116),ZERO,0_intkind1,wf(:,40))
  call vert_ZQ_A(gZl,wf(:,29),wf(:,-2),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,116),ZERO,0_intkind1,wf(:,42))
  call vert_ZQ_A(gZl,wf(:,32),wf(:,-2),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,116),ZERO,0_intkind1,wf(:,44))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,9),wf(:,45))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,46))
  call vert_AV_Q(wf(:,-1),wf(:,22),wf(:,47))
  call prop_Q_A(wf(:,46),Q(:,13),ZERO,0_intkind1,wf(:,48))
  call vert_AV_Q(wf(:,-1),wf(:,26),wf(:,49))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,29),wf(:,50))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,32),wf(:,51))
  call vert_ZQ_A(gZl,wf(:,6),wf(:,0),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,13),ZERO,0_intkind1,wf(:,53))
  call vert_VQ_A(wf(:,22),wf(:,0),wf(:,54))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,55))
  call prop_Q_A(wf(:,54),Q(:,113),ZERO,0_intkind1,wf(:,56))
  call vert_VQ_A(wf(:,26),wf(:,0),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,113),ZERO,0_intkind1,wf(:,58))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,6),wf(:,59))
  call vert_ZQ_A(gZl,wf(:,29),wf(:,0),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,113),ZERO,0_intkind1,wf(:,61))
  call vert_ZQ_A(gZl,wf(:,32),wf(:,0),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,113),ZERO,0_intkind1,wf(:,63))
  call vert_VV_S(wf(:,9),wf(:,6),wf(:,64))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,65))
  call counter_GG_S(wf(:,-5),wf(:,12),wf(:,66))
  call counter_GG_S(wf(:,-4),wf(:,17),wf(:,67))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,68))
  call prop_W_W(wf(:,68),Q(:,112),MZ,1_intkind1,wf(:,69))
  call vert_QA_Z(gZl,wf(:,25),wf(:,-3),wf(:,70))
  call vert_QA_Z(gZl,wf(:,35),wf(:,-3),wf(:,71))
  call prop_A_Q(wf(:,37),Q(:,11),ZERO,0_intkind1,wf(:,72))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,72),wf(:,73))
  call prop_A_Q(wf(:,45),Q(:,11),ZERO,0_intkind1,wf(:,74))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,74),wf(:,75))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,12),Q(:,80),wf(:,76))
  call prop_W_W(wf(:,76),Q(:,112),MZ,1_intkind1,wf(:,77))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,17),Q(:,96),wf(:,78))
  call prop_W_W(wf(:,78),Q(:,112),MZ,1_intkind1,wf(:,79))
  call vert_QA_Z(gZl,wf(:,48),wf(:,-1),wf(:,80))
  call vert_QA_Z(gZl,wf(:,53),wf(:,-1),wf(:,81))
  call prop_A_Q(wf(:,55),Q(:,14),ZERO,0_intkind1,wf(:,82))
  call vert_QA_Z(gZl,wf(:,0),wf(:,82),wf(:,83))
  call prop_A_Q(wf(:,59),Q(:,14),ZERO,0_intkind1,wf(:,84))
  call vert_QA_Z(gZl,wf(:,0),wf(:,84),wf(:,85))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,86))
  call prop_W_W(wf(:,70),Q(:,15),MZ,1_intkind1,wf(:,87))
  call vert_QA_V(wf(:,35),wf(:,-3),wf(:,88))
  call prop_W_W(wf(:,71),Q(:,15),MZ,1_intkind1,wf(:,89))
  call vert_QA_V(wf(:,-2),wf(:,72),wf(:,90))
  call prop_W_W(wf(:,73),Q(:,15),MZ,1_intkind1,wf(:,91))
  call vert_QA_V(wf(:,-2),wf(:,74),wf(:,92))
  call prop_W_W(wf(:,75),Q(:,15),MZ,1_intkind1,wf(:,93))
  call vert_QA_V(wf(:,48),wf(:,-1),wf(:,94))
  call prop_W_W(wf(:,80),Q(:,15),MZ,1_intkind1,wf(:,95))
  call vert_QA_V(wf(:,53),wf(:,-1),wf(:,96))
  call prop_W_W(wf(:,81),Q(:,15),MZ,1_intkind1,wf(:,97))
  call vert_QA_V(wf(:,0),wf(:,82),wf(:,98))
  call vert_QA_V(wf(:,0),wf(:,84),wf(:,99))
  call prop_W_W(wf(:,83),Q(:,15),MZ,1_intkind1,wf(:,100))
  call prop_W_W(wf(:,85),Q(:,15),MZ,1_intkind1,wf(:,101))

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
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(9) = 1 / (Q(5,3) - MZ2)
  den(14) = 1 / (Q(5,80))
  den(19) = 1 / (Q(5,96))
  den(24) = 1 / (Q(5,112))
  den(25) = 1 / (Q(5,7))
  den(28) = 1 / (Q(5,112) - MZ2)
  den(33) = 1 / (Q(5,116))
  den(40) = 1 / (Q(5,13))
  den(47) = 1 / (Q(5,113))
  den(54) = 1 / (Q(5,15) - MH2)
  den(62) = 1 / (Q(5,11))
  den(79) = 1 / (Q(5,14))
  den(92) = 1 / (Q(5,15))
  den(94) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(3)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(3)*den(12)
  den(15) = den(4)*den(14)
  den(16) = den(7)*den(14)
  den(17) = den(10)*den(14)
  den(18) = den(12)*den(14)
  den(20) = den(4)*den(19)
  den(21) = den(7)*den(19)
  den(22) = den(10)*den(19)
  den(23) = den(12)*den(19)
  den(26) = den(1)*den(25)
  den(27) = den(24)*den(26)
  den(29) = den(26)*den(28)
  den(30) = den(9)*den(25)
  den(31) = den(24)*den(30)
  den(32) = den(28)*den(30)
  den(34) = den(24)*den(33)
  den(35) = den(1)*den(34)
  den(36) = den(28)*den(33)
  den(37) = den(1)*den(36)
  den(38) = den(9)*den(34)
  den(39) = den(9)*den(36)
  den(41) = den(2)*den(40)
  den(42) = den(24)*den(41)
  den(43) = den(28)*den(41)
  den(44) = den(6)*den(40)
  den(45) = den(24)*den(44)
  den(46) = den(28)*den(44)
  den(48) = den(24)*den(47)
  den(49) = den(2)*den(48)
  den(50) = den(6)*den(48)
  den(51) = den(28)*den(47)
  den(52) = den(2)*den(51)
  den(53) = den(6)*den(51)
  den(55) = den(12)*den(54)
  den(56) = den(3)*den(55)
  den(57) = den(14)*den(55)
  den(58) = den(19)*den(55)
  den(59) = den(3)*den(28)
  den(60) = den(26)*den(59)
  den(61) = den(30)*den(59)
  den(63) = den(1)*den(62)
  den(64) = den(59)*den(63)
  den(65) = den(9)*den(62)
  den(66) = den(59)*den(65)
  den(67) = den(14)*den(28)
  den(68) = den(26)*den(67)
  den(69) = den(30)*den(67)
  den(70) = den(63)*den(67)
  den(71) = den(65)*den(67)
  den(72) = den(19)*den(28)
  den(73) = den(26)*den(72)
  den(74) = den(30)*den(72)
  den(75) = den(63)*den(72)
  den(76) = den(65)*den(72)
  den(77) = den(41)*den(59)
  den(78) = den(44)*den(59)
  den(80) = den(2)*den(79)
  den(81) = den(59)*den(80)
  den(82) = den(6)*den(79)
  den(83) = den(59)*den(82)
  den(84) = den(41)*den(67)
  den(85) = den(44)*den(67)
  den(86) = den(67)*den(80)
  den(87) = den(67)*den(82)
  den(88) = den(41)*den(72)
  den(89) = den(44)*den(72)
  den(90) = den(72)*den(80)
  den(91) = den(72)*den(82)
  den(93) = den(26)*den(92)
  den(95) = den(26)*den(94)
  den(96) = den(30)*den(92)
  den(97) = den(30)*den(94)
  den(98) = den(63)*den(92)
  den(99) = den(63)*den(94)
  den(100) = den(65)*den(92)
  den(101) = den(65)*den(94)
  den(102) = den(41)*den(92)
  den(103) = den(41)*den(94)
  den(104) = den(44)*den(92)
  den(105) = den(44)*den(94)
  den(106) = den(80)*den(92)
  den(107) = den(82)*den(92)
  den(108) = den(80)*den(94)
  den(109) = den(82)*den(94)
  den(110) = den(1)*den(2)*den(3)
  den(111) = den(1)*den(3)*den(6)
  den(112) = den(2)*den(3)*den(9)
  den(113) = den(3)*den(6)*den(9)
  den(114) = den(1)*den(2)*den(14)
  den(115) = den(1)*den(6)*den(14)
  den(116) = den(2)*den(9)*den(14)
  den(117) = den(6)*den(9)*den(14)
  den(118) = den(1)*den(2)*den(19)
  den(119) = den(1)*den(6)*den(19)
  den(120) = den(2)*den(9)*den(19)
  den(121) = den(6)*den(9)*den(19)
  den(122) = den(3)*den(93)
  den(123) = den(3)*den(95)
  den(124) = den(3)*den(96)
  den(125) = den(3)*den(97)
  den(126) = den(3)*den(98)
  den(127) = den(3)*den(99)
  den(128) = den(3)*den(100)
  den(129) = den(3)*den(101)
  den(130) = den(14)*den(93)
  den(131) = den(14)*den(95)
  den(132) = den(14)*den(96)
  den(133) = den(14)*den(97)
  den(134) = den(14)*den(98)
  den(135) = den(14)*den(99)
  den(136) = den(14)*den(100)
  den(137) = den(14)*den(101)
  den(138) = den(19)*den(93)
  den(139) = den(19)*den(95)
  den(140) = den(19)*den(96)
  den(141) = den(19)*den(97)
  den(142) = den(19)*den(98)
  den(143) = den(19)*den(99)
  den(144) = den(19)*den(100)
  den(145) = den(19)*den(101)
  den(146) = den(3)*den(102)
  den(147) = den(3)*den(103)
  den(148) = den(3)*den(104)
  den(149) = den(3)*den(105)
  den(150) = den(3)*den(106)
  den(151) = den(3)*den(107)
  den(152) = den(3)*den(108)
  den(153) = den(3)*den(109)
  den(154) = den(14)*den(102)
  den(155) = den(14)*den(103)
  den(156) = den(14)*den(104)
  den(157) = den(14)*den(105)
  den(158) = den(14)*den(106)
  den(159) = den(14)*den(107)
  den(160) = den(14)*den(108)
  den(161) = den(14)*den(109)
  den(162) = den(19)*den(102)
  den(163) = den(19)*den(103)
  den(164) = den(19)*den(104)
  den(165) = den(19)*den(105)
  den(166) = den(19)*den(106)
  den(167) = den(19)*den(107)
  den(168) = den(19)*den(108)
  den(169) = den(19)*den(109)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(71)


  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,7)) * den(8)
  A(3) = cont_VV(wf(:,3),wf(:,10)) * den(11)
  A(4) = cont_VV(wf(:,3),wf(:,11)) * den(13)
  A(5) = cont_VV(wf(:,12),wf(:,13)) * den(15)
  A(6) = cont_VV(wf(:,12),wf(:,14)) * den(16)
  A(7) = cont_VV(wf(:,12),wf(:,15)) * den(17)
  A(8) = cont_VV(wf(:,12),wf(:,16)) * den(18)
  A(9) = cont_VV(wf(:,17),wf(:,18)) * den(20)
  A(10) = cont_VV(wf(:,17),wf(:,19)) * den(21)
  A(11) = cont_VV(wf(:,17),wf(:,20)) * den(22)
  A(12) = cont_VV(wf(:,17),wf(:,21)) * den(23)
  A(13) = cont_QA(wf(:,24),wf(:,25)) * den(27)
  A(14) = cont_QA(wf(:,25),wf(:,27)) * den(27)
  A(15) = cont_QA(wf(:,25),wf(:,30)) * den(29)
  A(16) = cont_QA(wf(:,25),wf(:,33)) * den(29)
  A(17) = cont_QA(wf(:,24),wf(:,35)) * den(31)
  A(18) = cont_QA(wf(:,27),wf(:,35)) * den(31)
  A(19) = cont_QA(wf(:,30),wf(:,35)) * den(32)
  A(20) = cont_QA(wf(:,33),wf(:,35)) * den(32)
  A(21) = cont_QA(wf(:,37),wf(:,38)) * den(35)
  A(22) = cont_QA(wf(:,37),wf(:,40)) * den(35)
  A(23) = cont_QA(wf(:,37),wf(:,42)) * den(37)
  A(24) = cont_QA(wf(:,37),wf(:,44)) * den(37)
  A(25) = cont_QA(wf(:,38),wf(:,45)) * den(38)
  A(26) = cont_QA(wf(:,40),wf(:,45)) * den(38)
  A(27) = cont_QA(wf(:,42),wf(:,45)) * den(39)
  A(28) = cont_QA(wf(:,44),wf(:,45)) * den(39)
  A(29) = cont_QA(wf(:,47),wf(:,48)) * den(42)
  A(30) = cont_QA(wf(:,48),wf(:,49)) * den(42)
  A(31) = cont_QA(wf(:,48),wf(:,50)) * den(43)
  A(32) = cont_QA(wf(:,48),wf(:,51)) * den(43)
  A(33) = cont_QA(wf(:,47),wf(:,53)) * den(45)
  A(34) = cont_QA(wf(:,49),wf(:,53)) * den(45)
  A(35) = cont_QA(wf(:,50),wf(:,53)) * den(46)
  A(36) = cont_QA(wf(:,51),wf(:,53)) * den(46)
  A(37) = cont_QA(wf(:,55),wf(:,56)) * den(49)
  A(38) = cont_QA(wf(:,55),wf(:,58)) * den(49)
  A(39) = cont_QA(wf(:,56),wf(:,59)) * den(50)
  A(40) = cont_QA(wf(:,58),wf(:,59)) * den(50)
  A(41) = cont_QA(wf(:,55),wf(:,61)) * den(52)
  A(42) = cont_QA(wf(:,55),wf(:,63)) * den(52)
  A(43) = cont_QA(wf(:,59),wf(:,61)) * den(53)
  A(44) = cont_QA(wf(:,59),wf(:,63)) * den(53)
  A(45) = cont_SS(wf(:,64),wf(:,65)) * den(56)
  A(46) = cont_SS(wf(:,64),wf(:,66)) * den(57)
  A(47) = cont_SS(wf(:,64),wf(:,67)) * den(58)
  A(48) = cont_VV(wf(:,69),wf(:,70)) * den(60)
  A(49) = cont_VV(wf(:,69),wf(:,71)) * den(61)
  A(50) = cont_VV(wf(:,69),wf(:,73)) * den(64)
  A(51) = cont_VV(wf(:,69),wf(:,75)) * den(66)
  A(52) = cont_VV(wf(:,70),wf(:,77)) * den(68)
  A(53) = cont_VV(wf(:,71),wf(:,77)) * den(69)
  A(54) = cont_VV(wf(:,73),wf(:,77)) * den(70)
  A(55) = cont_VV(wf(:,75),wf(:,77)) * den(71)
  A(56) = cont_VV(wf(:,70),wf(:,79)) * den(73)
  A(57) = cont_VV(wf(:,71),wf(:,79)) * den(74)
  A(58) = cont_VV(wf(:,73),wf(:,79)) * den(75)
  A(59) = cont_VV(wf(:,75),wf(:,79)) * den(76)
  A(60) = cont_VV(wf(:,69),wf(:,80)) * den(77)
  A(61) = cont_VV(wf(:,69),wf(:,81)) * den(78)
  A(62) = cont_VV(wf(:,69),wf(:,83)) * den(81)
  A(63) = cont_VV(wf(:,69),wf(:,85)) * den(83)
  A(64) = cont_VV(wf(:,77),wf(:,80)) * den(84)
  A(65) = cont_VV(wf(:,77),wf(:,81)) * den(85)
  A(66) = cont_VV(wf(:,77),wf(:,83)) * den(86)
  A(67) = cont_VV(wf(:,77),wf(:,85)) * den(87)
  A(68) = cont_VV(wf(:,79),wf(:,80)) * den(88)
  A(69) = cont_VV(wf(:,79),wf(:,81)) * den(89)
  A(70) = cont_VV(wf(:,79),wf(:,83)) * den(90)
  A(71) = cont_VV(wf(:,79),wf(:,85)) * den(91)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(71)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (A(13)+A(15)+A(17)+A(19)+A(21)+A(23)+A(25)+A(27)+A(29)+A(31)+A(33)+A(35)+A(37)+A(39)+A(41)+A(43))*f(1)+2*CI*(A(1)-A(5) &
       +A(9))*f(2)+2*CI*(A(2)+A(3)-A(6)-A(7)+A(10)+A(11))*f(3)+2*CI*(A(48)+A(49)+A(50)+A(51)-A(52)-A(53)-A(54)-A(55)+A(56)+A(57) &
       +A(58)+A(59)+A(60)+A(61)+A(62)+A(63)-A(64)-A(65)-A(66)-A(67)+A(68)+A(69)+A(70)+A(71))*f(4)+2*CI*(A(4)-A(8)+A(12))*f(5) &
       +2*CI*(-A(45)+A(46)-A(47))*f(6)
  M2(2) = (A(14)+A(16)+A(18)+A(20)+A(22)+A(24)+A(26)+A(28)+A(30)+A(32)+A(34)+A(36)+A(38)+A(40)+A(42)+A(44))*f(1)+2*CI*(-A(1)+A(5) &
       -A(9))*f(2)+2*CI*(-A(2)-A(3)+A(6)+A(7)-A(10)-A(11))*f(3)+2*CI*(-A(48)-A(49)-A(50)-A(51)+A(52)+A(53)+A(54)+A(55)-A(56)-A(57) &
       -A(58)-A(59)-A(60)-A(61)-A(62)-A(63)+A(64)+A(65)+A(66)+A(67)-A(68)-A(69)-A(70)-A(71))*f(4)+2*CI*(-A(4)+A(8)-A(12))*f(5) &
       +2*CI*(A(45)-A(46)+A(47))*f(6)

end subroutine colourvectors

end module ol_loop_ppllllj2_eexmmxggg_1_/**/REALKIND
