
module ol_colourmatrix_pplljj_nenexdddxdx_1_/**/REALKIND
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
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,  -4]
  K1(28,:) = [  -4, -12]
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
  K1(39,:) = [   0,  -4]
  K1(40,:) = [  -4, -12]
  K1(41,:) = [   0,   4]
  K1(42,:) = [   4,   0]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplljj_nenexdddxdx_1_/**/REALKIND



module ol_forced_parameters_pplljj_nenexdddxdx_1_/**/REALKIND
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
end module ol_forced_parameters_pplljj_nenexdddxdx_1_/**/REALKIND

module ol_loop_pplljj_nenexdddxdx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:86)
  ! denominators
  complex(REALKIND), save :: den(89)
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
    f( 1) = CI*eQED**2*gQCD**2
    f( 2) = CI*countertermnorm*eQED**2*gQCD**4
    f( 3) = CI*countertermnorm*ctGqq*eQED**2*gQCD**4
    f( 4) = CI*countertermnorm*ctVqq*eQED**2*gQCD**4
    f( 5) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f( 6) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f( 7) = (eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f( 8) = eQED**2*gQCD**4*integralnorm*SwB
    f( 9) = eQED**2*gQCD**4*integralnorm*SwF
    f(10) = 2*eQED**2*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10) ]
  c = (1._/**/REALKIND / 36) * c
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(50)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_A(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_A(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-3),wf(:,4))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,11),ZERO,0_intkind1,wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,-3),wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,28),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,-3),wf(:,-4),wf(:,10))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-2),wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,10),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,7),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,10),wf(:,-2),wf(:,14))
  call prop_Q_A(wf(:,14),Q(:,28),ZERO,0_intkind1,wf(:,15))
  call vert_QA_V(wf(:,-2),wf(:,-5),wf(:,16))
  call vert_AV_Q(wf(:,-4),wf(:,16),wf(:,17))
  call vert_VQ_A(wf(:,16),wf(:,-3),wf(:,18))
  call vert_AZ_Q(gZd,wf(:,-4),wf(:,3),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,44),ZERO,0_intkind1,wf(:,20))
  call vert_QA_V(wf(:,-3),wf(:,-5),wf(:,21))
  call vert_AV_Q(wf(:,-4),wf(:,21),wf(:,22))
  call vert_VQ_A(wf(:,21),wf(:,-2),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,44),ZERO,0_intkind1,wf(:,24))
  call counter_VG_G(wf(:,3),wf(:,2),Q(:,20),wf(:,25),Q(:,23))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,26))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,27))
  call counter_VG_G(wf(:,3),wf(:,16),Q(:,36),wf(:,28),Q(:,39))
  call counter_AV_Q(wf(:,-5),wf(:,10),wf(:,29))
  call counter_AV_Q(wf(:,-4),wf(:,16),wf(:,30))
  call counter_AZ_Q(gZd,wf(:,-4),wf(:,3),wf(:,31))
  call counter_AV_Q(wf(:,-4),wf(:,21),wf(:,32))
  call counter_VQ_A(wf(:,2),wf(:,-3),wf(:,33))
  call prop_A_Q(wf(:,8),Q(:,35),ZERO,0_intkind1,wf(:,34))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-3),wf(:,35))
  call prop_A_Q(wf(:,5),Q(:,52),ZERO,0_intkind1,wf(:,36))
  call counter_VQ_A(wf(:,16),wf(:,-3),wf(:,37))
  call prop_A_Q(wf(:,19),Q(:,19),ZERO,0_intkind1,wf(:,38))
  call prop_A_Q(wf(:,17),Q(:,52),ZERO,0_intkind1,wf(:,39))
  call counter_QA_V(wf(:,-3),wf(:,-5),wf(:,40))
  call vert_AV_Q(wf(:,-4),wf(:,40),wf(:,41))
  call vert_VQ_A(wf(:,40),wf(:,-2),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,44),ZERO,0_intkind1,wf(:,43))
  call counter_QA_V(wf(:,-3),wf(:,-4),wf(:,44))
  call vert_AV_Q(wf(:,-5),wf(:,44),wf(:,45))
  call vert_VQ_A(wf(:,44),wf(:,-2),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,28),ZERO,0_intkind1,wf(:,47))
  call counter_VQ_A(wf(:,10),wf(:,-2),wf(:,48))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-2),wf(:,49))
  call prop_A_Q(wf(:,12),Q(:,56),ZERO,0_intkind1,wf(:,50))
  call counter_VQ_A(wf(:,21),wf(:,-2),wf(:,51))
  call prop_A_Q(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,52))
  call counter_QA_V(wf(:,-2),wf(:,-5),wf(:,53))
  call vert_AV_Q(wf(:,-4),wf(:,53),wf(:,54))
  call vert_VQ_A(wf(:,53),wf(:,-3),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,44),ZERO,0_intkind1,wf(:,56))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,57))
  call vert_AV_Q(wf(:,-5),wf(:,57),wf(:,58))
  call vert_VQ_A(wf(:,57),wf(:,-3),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,28),ZERO,0_intkind1,wf(:,60))
  call counter_V_V(ctGG,wf(:,2),Q(:,20),wf(:,61))
  call vert_VQ_A(wf(:,61),wf(:,-3),wf(:,62))
  call vert_AV_Q(wf(:,-5),wf(:,61),wf(:,63))
  call counter_Q_A(ctqq,wf(:,6),Q(:,11),wf(:,64))
  call counter_Q_A(ctqq,wf(:,9),Q(:,28),wf(:,65))
  call counter_V_V(ctGG,wf(:,10),Q(:,24),wf(:,66))
  call vert_VQ_A(wf(:,66),wf(:,-2),wf(:,67))
  call counter_Q_A(ctqq,wf(:,13),Q(:,7),wf(:,68))
  call counter_Q_A(ctqq,wf(:,15),Q(:,28),wf(:,69))
  call vert_AV_Q(wf(:,-5),wf(:,66),wf(:,70))
  call counter_V_V(ctGG,wf(:,16),Q(:,36),wf(:,71))
  call vert_VQ_A(wf(:,71),wf(:,-3),wf(:,72))
  call vert_AV_Q(wf(:,-4),wf(:,71),wf(:,73))
  call counter_Q_A(ctqq,wf(:,20),Q(:,44),wf(:,74))
  call counter_V_V(ctGG,wf(:,21),Q(:,40),wf(:,75))
  call vert_VQ_A(wf(:,75),wf(:,-2),wf(:,76))
  call counter_Q_A(ctqq,wf(:,24),Q(:,44),wf(:,77))
  call vert_AV_Q(wf(:,-4),wf(:,75),wf(:,78))
  call vert_QA_V(wf(:,13),wf(:,-4),wf(:,79))
  call vert_QA_V(wf(:,13),wf(:,-5),wf(:,80))
  call vert_QA_V(wf(:,6),wf(:,-4),wf(:,81))
  call vert_QA_V(wf(:,6),wf(:,-5),wf(:,82))
  call vert_QA_V(wf(:,-2),wf(:,38),wf(:,83))
  call vert_QA_V(wf(:,-3),wf(:,38),wf(:,84))
  call vert_QA_V(wf(:,-2),wf(:,34),wf(:,85))
  call vert_QA_V(wf(:,-3),wf(:,34),wf(:,86))

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
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,11))
  den(6) = 1 / (Q(5,28))
  den(9) = 1 / (Q(5,24))
  den(10) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,36))
  den(17) = 1 / (Q(5,44))
  den(20) = 1 / (Q(5,40))
  den(28) = 1 / (Q(5,35))
  den(31) = 1 / (Q(5,52))
  den(34) = 1 / (Q(5,19))
  den(40) = 1 / (Q(5,56))
  den(66) = 1 / (Q(5,23))
  den(68) = 1 / (Q(5,39))
  den(70) = 1 / (Q(5,27))
  den(72) = 1 / (Q(5,43))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(1)*den(10)
  den(12) = den(9)*den(11)
  den(13) = den(6)*den(9)
  den(14) = den(1)*den(13)
  den(16) = den(4)*den(15)
  den(18) = den(15)*den(17)
  den(19) = den(1)*den(18)
  den(21) = den(11)*den(20)
  den(22) = den(17)*den(20)
  den(23) = den(1)*den(22)
  den(24) = den(1)*den(2)
  den(25) = den(20)*den(24)
  den(26) = den(1)*den(15)
  den(27) = den(9)*den(26)
  den(29) = den(1)*den(28)
  den(30) = den(2)*den(29)
  den(32) = den(2)*den(31)
  den(33) = den(1)*den(32)
  den(35) = den(1)*den(34)
  den(36) = den(15)*den(35)
  den(37) = den(15)*den(31)
  den(38) = den(1)*den(37)
  den(39) = den(9)*den(29)
  den(41) = den(9)*den(40)
  den(42) = den(1)*den(41)
  den(43) = den(20)*den(35)
  den(44) = den(20)*den(40)
  den(45) = den(1)*den(44)
  den(46) = den(2)**2
  den(47) = den(29)*den(46)
  den(48) = den(4)*den(46)
  den(49) = den(4)*den(32)
  den(50) = den(7)*den(29)
  den(51) = den(9)**2
  den(52) = den(29)*den(51)
  den(53) = den(11)*den(41)
  den(54) = den(13)*den(29)
  den(55) = den(11)*den(51)
  den(56) = den(15)**2
  den(57) = den(35)*den(56)
  den(58) = den(4)*den(56)
  den(59) = den(4)*den(37)
  den(60) = den(18)*den(35)
  den(61) = den(20)**2
  den(62) = den(35)*den(61)
  den(63) = den(11)*den(44)
  den(64) = den(22)*den(35)
  den(65) = den(11)*den(61)
  den(67) = den(11)*den(66)
  den(69) = den(11)*den(68)
  den(71) = den(4)*den(70)
  den(73) = den(4)*den(72)
  den(74) = den(35)*den(66)
  den(75) = den(35)*den(70)
  den(76) = den(29)*den(68)
  den(77) = den(29)*den(72)
  den(78) = den(1)*den(2)*den(20)
  den(79) = den(1)*den(9)*den(15)
  den(80) = den(1)*den(9)
  den(81) = den(1)*den(20)
  den(82) = den(2)*den(73)
  den(83) = den(2)*den(77)
  den(84) = den(9)*den(69)
  den(85) = den(9)*den(76)
  den(86) = den(15)*den(71)
  den(87) = den(15)*den(75)
  den(88) = den(20)*den(67)
  den(89) = den(20)*den(74)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(50)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(4) = cont_QA(wf(:,8),wf(:,15)) * den(14)
  A(5) = cont_QA(wf(:,6),wf(:,17)) * den(16)
  A(6) = cont_QA(wf(:,19),wf(:,20)) * den(19)
  A(7) = cont_QA(wf(:,13),wf(:,22)) * den(21)
  A(8) = cont_QA(wf(:,19),wf(:,24)) * den(23)

  A(9) = cont_VV(wf(:,21),wf(:,25)) * den(25)
  A(10) = cont_QA(wf(:,6),wf(:,26)) * den(5)
  A(11) = cont_QA(wf(:,9),wf(:,27)) * den(8)
  A(12) = cont_VV(wf(:,10),wf(:,28)) * den(27)
  A(13) = cont_QA(wf(:,13),wf(:,29)) * den(12)
  A(14) = cont_QA(wf(:,15),wf(:,27)) * den(14)
  A(15) = cont_QA(wf(:,6),wf(:,30)) * den(16)
  A(16) = cont_QA(wf(:,20),wf(:,31)) * den(19)
  A(17) = cont_QA(wf(:,13),wf(:,32)) * den(21)
  A(18) = cont_QA(wf(:,24),wf(:,31)) * den(23)
  A(19) = cont_QA(wf(:,33),wf(:,34)) * den(30)
  A(20) = cont_QA(wf(:,35),wf(:,36)) * den(33)
  A(21) = cont_QA(wf(:,37),wf(:,38)) * den(36)
  A(22) = cont_QA(wf(:,35),wf(:,39)) * den(38)
  A(23) = cont_QA(wf(:,13),wf(:,41)) * den(21)
  A(24) = cont_QA(wf(:,19),wf(:,43)) * den(23)
  A(25) = cont_QA(wf(:,13),wf(:,45)) * den(12)
  A(26) = cont_QA(wf(:,8),wf(:,47)) * den(14)
  A(27) = cont_QA(wf(:,34),wf(:,48)) * den(39)
  A(28) = cont_QA(wf(:,49),wf(:,50)) * den(42)
  A(29) = cont_QA(wf(:,38),wf(:,51)) * den(43)
  A(30) = cont_QA(wf(:,49),wf(:,52)) * den(45)
  A(31) = cont_QA(wf(:,6),wf(:,54)) * den(16)
  A(32) = cont_QA(wf(:,19),wf(:,56)) * den(19)
  A(33) = cont_QA(wf(:,6),wf(:,58)) * den(5)
  A(34) = cont_QA(wf(:,8),wf(:,60)) * den(8)
  A(35) = cont_QA(wf(:,34),wf(:,62)) * den(47)
  A(36) = cont_QA(wf(:,6),wf(:,63)) * den(48)
  A(37) = cont_QA(wf(:,36),wf(:,64)) * den(49)
  A(38) = cont_QA(wf(:,34),wf(:,65)) * den(50)
  A(39) = cont_QA(wf(:,34),wf(:,67)) * den(52)
  A(40) = cont_QA(wf(:,50),wf(:,68)) * den(53)
  A(41) = cont_QA(wf(:,34),wf(:,69)) * den(54)
  A(42) = cont_QA(wf(:,13),wf(:,70)) * den(55)
  A(43) = cont_QA(wf(:,38),wf(:,72)) * den(57)
  A(44) = cont_QA(wf(:,6),wf(:,73)) * den(58)
  A(45) = cont_QA(wf(:,39),wf(:,64)) * den(59)
  A(46) = cont_QA(wf(:,38),wf(:,74)) * den(60)
  A(47) = cont_QA(wf(:,38),wf(:,76)) * den(62)
  A(48) = cont_QA(wf(:,52),wf(:,68)) * den(63)
  A(49) = cont_QA(wf(:,38),wf(:,77)) * den(64)
  A(50) = cont_QA(wf(:,13),wf(:,78)) * den(65)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(50)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(3)-A(4)-A(5)-A(6))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(7)-A(8))*f(1))/2._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(5)+A(6))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(7)+A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((A(39)+A(40)+A(41)+A(42)+A(43)+A(44)+A(45)+A(46))*f(2))/6._/**/REALKIND+((A(35)+A(36)+A(37)+A(38)+A(47)+A(48)+A(49) &
       +A(50))*f(2))/2._/**/REALKIND+((-A(13)-A(15)-A(21)-A(25)-A(26)-A(27)-A(31)-A(32))*f(3))/6._/**/REALKIND+((-A(10)-A(17) &
       -A(19)-A(23)-A(24)-A(29)-A(33)-A(34))*f(3))/2._/**/REALKIND+((-A(14)-A(16)-A(22)-A(28))*f(4))/6._/**/REALKIND+((-A(11) &
       -A(18)-A(20)-A(30))*f(4))/2._/**/REALKIND+(A(9)*f(5))/2._/**/REALKIND+(A(12)*f(5))/6._/**/REALKIND
  M2(2) = ((-A(39)-A(40)-A(41)-A(42)-A(43)-A(44)-A(45)-A(46))*f(2))/2._/**/REALKIND+((-A(35)-A(36)-A(37)-A(38)-A(47)-A(48)-A(49) &
       -A(50))*f(2))/6._/**/REALKIND+((A(13)+A(15)+A(21)+A(25)+A(26)+A(27)+A(31)+A(32))*f(3))/2._/**/REALKIND+((A(10)+A(17)+A(19) &
       +A(23)+A(24)+A(29)+A(33)+A(34))*f(3))/6._/**/REALKIND+((A(14)+A(16)+A(22)+A(28))*f(4))/2._/**/REALKIND+((A(11)+A(18)+A(20) &
       +A(30))*f(4))/6._/**/REALKIND-(A(9)*f(5))/6._/**/REALKIND-(A(12)*f(5))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplljj_nenexdddxdx_1_/**/REALKIND
