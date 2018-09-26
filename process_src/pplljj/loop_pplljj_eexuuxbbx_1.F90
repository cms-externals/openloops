
module ol_colourmatrix_pplljj_eexuuxbbx_1_/**/REALKIND
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

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplljj_eexuuxbbx_1_/**/REALKIND



module ol_forced_parameters_pplljj_eexuuxbbx_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplljj_eexuuxbbx_1_/**/REALKIND

module ol_loop_pplljj_eexuuxbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(30), c(47)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:70)
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
    f( 1) = (CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 2) = (2*CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 3) = CI*eQED**2*gQCD**2
    f( 4) = (CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 5) = (2*CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*eQED**2*gQCD**4
    f( 7) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 8) = (2*CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctGbb*eQED**2*gQCD**4
    f(10) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f(11) = (2*CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f(12) = CI*countertermnorm*ctGqq*eQED**2*gQCD**4
    f(13) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f(14) = CI*countertermnorm*ctVbb*eQED**2*gQCD**4
    f(15) = (2*CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f(16) = CI*countertermnorm*ctVqq*eQED**2*gQCD**4
    f(17) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f(18) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(19) = (2*CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(20) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f(21) = (eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(22) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(23) = (eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(24) = (2*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(25) = eQED**2*gQCD**4*integralnorm*SwB
    f(26) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(27) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(28) = eQED**2*gQCD**4*integralnorm*SwF
    f(29) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(30) = 2*eQED**2*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(18), 27*CI*f(18), 9*CI*f(19), 27*CI*f(19), 9*CI*f(20), 27*CI*f(20), 18*f(21), 54*f(21), f(22), 3*f(22), 6*f(22) &
    , 8*f(22), 10*f(22), 18*f(22), 21*f(22), 24*f(22), 54*f(22), 18*f(23), 54*f(23), f(24), 3*f(24), 6*f(24), 8*f(24), 10*f(24) &
    , 18*f(24), 21*f(24), 24*f(24), 54*f(24), f(25), 3*f(25), 6*f(25), 8*f(25), 10*f(25), 18*f(25), 21*f(25), 24*f(25), 54*f(25) &
    , 3*f(26), 9*f(26), 3*f(27), 9*f(27), 3*f(28), 9*f(28), 3*f(29), 9*f(29), 3*f(30), 9*f(30) ]
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
  complex(REALKIND) :: A(49)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_Q(P(:,5), rMB, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_A(P(:,6), rMB, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_Q(P(:,5), rMB, H(5), wf(:,-4), 0)
    call pol_wf_A(P(:,6), rMB, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,3))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,19),MB,1_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,3),MZ,1_intkind1,wf(:,7))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-4),wf(:,8))
  call prop_Q_A(wf(:,8),Q(:,19),MB,1_intkind1,wf(:,9))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,10))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,28),MB,1_intkind1,wf(:,12))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,13))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,14))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,14),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,-2),wf(:,18))
  call prop_Q_A(wf(:,18),Q(:,7),ZERO,0_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,14),wf(:,-2),wf(:,20))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,52),ZERO,0_intkind1,wf(:,22))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,7),wf(:,23))
  call counter_VG_G(wf(:,7),wf(:,2),Q(:,12),wf(:,24),Q(:,15))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,25))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,26))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,27))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,28))
  call prop_A_Q(wf(:,11),Q(:,35),MB,1_intkind1,wf(:,29))
  call prop_A_Q(wf(:,13),Q(:,35),MB,1_intkind1,wf(:,30))
  call counter_VQ_A(wf(:,1),wf(:,-4),wf(:,31))
  call prop_A_Q(wf(:,4),Q(:,44),MB,1_intkind1,wf(:,32))
  call counter_ZQ_A(gZd,wf(:,7),wf(:,-4),wf(:,33))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,34))
  call vert_AV_Q(wf(:,-3),wf(:,34),wf(:,35))
  call vert_VQ_A(wf(:,34),wf(:,-2),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,52),ZERO,0_intkind1,wf(:,37))
  call counter_AV_Q(wf(:,-3),wf(:,14),wf(:,38))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,39))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,7),wf(:,40))
  call counter_VQ_A(wf(:,14),wf(:,-2),wf(:,41))
  call prop_A_Q(wf(:,21),Q(:,11),ZERO,0_intkind1,wf(:,42))
  call prop_A_Q(wf(:,23),Q(:,11),ZERO,0_intkind1,wf(:,43))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,44))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,45))
  call counter_ZQ_A(gZu,wf(:,7),wf(:,-2),wf(:,46))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,47))
  call vert_AV_Q(wf(:,-5),wf(:,47),wf(:,48))
  call vert_VQ_A(wf(:,47),wf(:,-4),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,28),MB,1_intkind1,wf(:,50))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,51))
  call vert_VQ_A(wf(:,51),wf(:,-4),wf(:,52))
  call vert_AV_Q(wf(:,-5),wf(:,51),wf(:,53))
  call counter_Q_A(ctbb,wf(:,5),Q(:,19),wf(:,54))
  call counter_Q_A(ctbb,wf(:,9),Q(:,19),wf(:,55))
  call counter_Q_A(ctbb,wf(:,12),Q(:,28),wf(:,56))
  call counter_Q_A(ctqq,wf(:,17),Q(:,7),wf(:,57))
  call counter_Q_A(ctqq,wf(:,19),Q(:,7),wf(:,58))
  call counter_Q_A(ctqq,wf(:,22),Q(:,52),wf(:,59))
  call counter_V_V(ctGG,wf(:,14),Q(:,48),wf(:,60))
  call vert_VQ_A(wf(:,60),wf(:,-2),wf(:,61))
  call vert_AV_Q(wf(:,-3),wf(:,60),wf(:,62))
  call vert_QA_V(wf(:,17),wf(:,-3),wf(:,63))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,64))
  call vert_QA_V(wf(:,-2),wf(:,42),wf(:,65))
  call vert_QA_V(wf(:,-2),wf(:,43),wf(:,66))
  call vert_QA_V(wf(:,5),wf(:,-5),wf(:,67))
  call vert_QA_V(wf(:,9),wf(:,-5),wf(:,68))
  call vert_QA_V(wf(:,-4),wf(:,29),wf(:,69))
  call vert_QA_V(wf(:,-4),wf(:,30),wf(:,70))

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
  den(3) = 1 / (Q(5,19) - MB2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,28) - MB2)
  den(13) = 1 / (Q(5,48))
  den(14) = 1 / (Q(5,7))
  den(19) = 1 / (Q(5,52))
  den(25) = 1 / (Q(5,35) - MB2)
  den(30) = 1 / (Q(5,44) - MB2)
  den(34) = 1 / (Q(5,11))
  den(39) = 1 / (Q(5,56))
  den(61) = 1 / (Q(5,15))
  den(66) = 1 / (Q(5,51))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(3)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(12) = den(6)*den(10)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(6)*den(14)
  den(18) = den(13)*den(17)
  den(20) = den(13)*den(19)
  den(21) = den(1)*den(20)
  den(22) = den(6)*den(20)
  den(23) = den(2)*den(6)
  den(24) = den(13)*den(23)
  den(26) = den(1)*den(25)
  den(27) = den(2)*den(26)
  den(28) = den(6)*den(25)
  den(29) = den(2)*den(28)
  den(31) = den(2)*den(30)
  den(32) = den(1)*den(31)
  den(33) = den(6)*den(31)
  den(35) = den(1)*den(34)
  den(36) = den(13)*den(35)
  den(37) = den(6)*den(34)
  den(38) = den(13)*den(37)
  den(40) = den(13)*den(39)
  den(41) = den(1)*den(40)
  den(42) = den(6)*den(40)
  den(43) = den(2)**2
  den(44) = den(26)*den(43)
  den(45) = den(28)*den(43)
  den(46) = den(4)*den(43)
  den(47) = den(7)*den(43)
  den(48) = den(4)*den(31)
  den(49) = den(7)*den(31)
  den(50) = den(10)*den(26)
  den(51) = den(10)*den(28)
  den(52) = den(15)*den(40)
  den(53) = den(17)*den(40)
  den(54) = den(20)*den(35)
  den(55) = den(20)*den(37)
  den(56) = den(13)**2
  den(57) = den(35)*den(56)
  den(58) = den(37)*den(56)
  den(59) = den(15)*den(56)
  den(60) = den(17)*den(56)
  den(62) = den(15)*den(61)
  den(63) = den(17)*den(61)
  den(64) = den(35)*den(61)
  den(65) = den(37)*den(61)
  den(67) = den(4)*den(66)
  den(68) = den(7)*den(66)
  den(69) = den(26)*den(66)
  den(70) = den(28)*den(66)
  den(71) = den(1)*den(2)*den(13)
  den(72) = den(2)*den(6)*den(13)
  den(73) = den(1)*den(2)
  den(74) = den(1)*den(13)
  den(75) = den(6)*den(13)
  den(76) = den(2)*den(67)
  den(77) = den(2)*den(68)
  den(78) = den(2)*den(69)
  den(79) = den(2)*den(70)
  den(80) = den(13)*den(62)
  den(81) = den(13)*den(63)
  den(82) = den(13)*den(64)
  den(83) = den(13)*den(65)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(49)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(5)
  A(2) = cont_QA(wf(:,4),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(5) = cont_QA(wf(:,16),wf(:,17)) * den(16)
  A(6) = cont_QA(wf(:,16),wf(:,19)) * den(18)
  A(7) = cont_QA(wf(:,21),wf(:,22)) * den(21)
  A(8) = cont_QA(wf(:,22),wf(:,23)) * den(22)

  A(9) = cont_VV(wf(:,14),wf(:,24)) * den(24)
  A(10) = cont_QA(wf(:,5),wf(:,25)) * den(5)
  A(11) = cont_QA(wf(:,9),wf(:,25)) * den(8)
  A(12) = cont_QA(wf(:,12),wf(:,26)) * den(11)
  A(13) = cont_QA(wf(:,12),wf(:,27)) * den(12)
  A(14) = cont_QA(wf(:,28),wf(:,29)) * den(27)
  A(15) = cont_QA(wf(:,28),wf(:,30)) * den(29)
  A(16) = cont_QA(wf(:,31),wf(:,32)) * den(32)
  A(17) = cont_QA(wf(:,32),wf(:,33)) * den(33)
  A(18) = cont_QA(wf(:,17),wf(:,35)) * den(16)
  A(19) = cont_QA(wf(:,19),wf(:,35)) * den(18)
  A(20) = cont_QA(wf(:,21),wf(:,37)) * den(21)
  A(21) = cont_QA(wf(:,23),wf(:,37)) * den(22)
  A(22) = cont_QA(wf(:,17),wf(:,38)) * den(16)
  A(23) = cont_QA(wf(:,19),wf(:,38)) * den(18)
  A(24) = cont_QA(wf(:,22),wf(:,39)) * den(21)
  A(25) = cont_QA(wf(:,22),wf(:,40)) * den(22)
  A(26) = cont_QA(wf(:,41),wf(:,42)) * den(36)
  A(27) = cont_QA(wf(:,41),wf(:,43)) * den(38)
  A(28) = cont_QA(wf(:,44),wf(:,45)) * den(41)
  A(29) = cont_QA(wf(:,45),wf(:,46)) * den(42)
  A(30) = cont_QA(wf(:,5),wf(:,48)) * den(5)
  A(31) = cont_QA(wf(:,9),wf(:,48)) * den(8)
  A(32) = cont_QA(wf(:,11),wf(:,50)) * den(11)
  A(33) = cont_QA(wf(:,13),wf(:,50)) * den(12)
  A(34) = cont_QA(wf(:,29),wf(:,52)) * den(44)
  A(35) = cont_QA(wf(:,30),wf(:,52)) * den(45)
  A(36) = cont_QA(wf(:,5),wf(:,53)) * den(46)
  A(37) = cont_QA(wf(:,9),wf(:,53)) * den(47)
  A(38) = cont_QA(wf(:,32),wf(:,54)) * den(48)
  A(39) = cont_QA(wf(:,32),wf(:,55)) * den(49)
  A(40) = cont_QA(wf(:,29),wf(:,56)) * den(50)
  A(41) = cont_QA(wf(:,30),wf(:,56)) * den(51)
  A(42) = cont_QA(wf(:,45),wf(:,57)) * den(52)
  A(43) = cont_QA(wf(:,45),wf(:,58)) * den(53)
  A(44) = cont_QA(wf(:,42),wf(:,59)) * den(54)
  A(45) = cont_QA(wf(:,43),wf(:,59)) * den(55)
  A(46) = cont_QA(wf(:,42),wf(:,61)) * den(57)
  A(47) = cont_QA(wf(:,43),wf(:,61)) * den(58)
  A(48) = cont_QA(wf(:,17),wf(:,62)) * den(59)
  A(49) = cont_QA(wf(:,19),wf(:,62)) * den(60)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(49)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(3))*f(1))/2._/**/REALKIND+((-A(5)-A(7))*f(2))/2._/**/REALKIND+((A(2)+A(4)+A(6)+A(8))*f(3))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(3))*f(1))/6._/**/REALKIND+((A(5)+A(7))*f(2))/6._/**/REALKIND+((-A(2)-A(4)-A(6)-A(8))*f(3))/6._/**/REALKIND

  M2(1) = ((-A(34)-A(36)-A(38)-A(40))*f(4))/2._/**/REALKIND+((A(42)+A(44)+A(46)+A(48))*f(5))/2._/**/REALKIND+((-A(35)-A(37)-A(39) &
       -A(41)-A(43)-A(45)-A(47)-A(49))*f(6))/2._/**/REALKIND+((A(10)+A(14))*f(7))/2._/**/REALKIND+((-A(18) &
       -A(20))*f(8))/2._/**/REALKIND+((A(11)+A(15)+A(19)+A(21))*f(9))/2._/**/REALKIND+((A(30)+A(32))*f(10))/2._/**/REALKIND+(( &
       -A(22)-A(26))*f(11))/2._/**/REALKIND+((A(23)+A(27)+A(31)+A(33))*f(12))/2._/**/REALKIND+((A(12) &
       +A(16))*f(13))/2._/**/REALKIND+((A(13)+A(17))*f(14))/2._/**/REALKIND+((-A(24)-A(28))*f(15))/2._/**/REALKIND+((A(25) &
       +A(29))*f(16))/2._/**/REALKIND-(A(9)*f(17))/2._/**/REALKIND
  M2(2) = ((A(34)+A(36)+A(38)+A(40))*f(4))/6._/**/REALKIND+((-A(42)-A(44)-A(46)-A(48))*f(5))/6._/**/REALKIND+((A(35)+A(37)+A(39) &
       +A(41)+A(43)+A(45)+A(47)+A(49))*f(6))/6._/**/REALKIND+((-A(10)-A(14))*f(7))/6._/**/REALKIND+((A(18) &
       +A(20))*f(8))/6._/**/REALKIND+((-A(11)-A(15)-A(19)-A(21))*f(9))/6._/**/REALKIND+((-A(30)-A(32))*f(10))/6._/**/REALKIND &
       +((A(22)+A(26))*f(11))/6._/**/REALKIND+((-A(23)-A(27)-A(31)-A(33))*f(12))/6._/**/REALKIND+((-A(12) &
       -A(16))*f(13))/6._/**/REALKIND+((-A(13)-A(17))*f(14))/6._/**/REALKIND+((A(24)+A(28))*f(15))/6._/**/REALKIND+((-A(25) &
       -A(29))*f(16))/6._/**/REALKIND+(A(9)*f(17))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplljj_eexuuxbbx_1_/**/REALKIND
