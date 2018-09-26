
module ol_colourmatrix_pplnjj_ckm_neexuxsgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,3)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
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
  K1(13,:) = [  64,  -8]
  K1(14,:) = [  -8,  64]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [  -1, -10]
  K1(20,:) = [ -10,  -1]
  K1(21,:) = [  64,  -8]
  K1(22,:) = [  -8,  64]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [ -72,   9]
  K1(28,:) = [   9,   9]
  K1(29,:) = [   9,   9]
  K1(30,:) = [   9, -72]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   9,   9]
  K1(38,:) = [   9, -72]
  K1(39,:) = [ -72,   9]
  K1(40,:) = [   9,   9]
  K1(41,:) = [ -81,   0]
  K1(42,:) = [   0, -81]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2]
  K2(2,:) = [ -2, 16]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplnjj_ckm_neexuxsgg_1_/**/REALKIND



module ol_forced_parameters_pplnjj_ckm_neexuxsgg_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
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
end module ol_forced_parameters_pplnjj_ckm_neexuxsgg_1_/**/REALKIND

module ol_loop_pplnjj_ckm_neexuxsgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(16), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:91)
  ! denominators
  complex(REALKIND), save :: den(81)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64), Mct(2,64), Mcol_loop(3,64)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**2*gQCD**2*VCKMsu)/(2._/**/REALKIND*sw**2)
    f( 2) = (eQED**2*gQCD**2*VCKMsu)/(sw**2*2._/**/REALKIND)
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**4*VCKMsu)/(2._/**/REALKIND*sw**2)
    f( 4) = (countertermnorm*eQED**2*gQCD**4*VCKMsu)/(sw**2*2._/**/REALKIND)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*VCKMsu)/(2._/**/REALKIND*sw**2)
    f( 6) = (countertermnorm*ctGqq*eQED**2*gQCD**4*VCKMsu)/(sw**2*2._/**/REALKIND)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*VCKMsu)/(2._/**/REALKIND*sw**2)
    f( 8) = (countertermnorm*ctVqq*eQED**2*gQCD**4*VCKMsu)/(sw**2*2._/**/REALKIND)
    f( 9) = (countertermnorm*ctVVV*eQED**2*gQCD**4*VCKMsu)/(sw**2*2._/**/REALKIND)
    f(10) = (CI*eQED**2*gQCD**4*integralnorm*SwB*VCKMsu)/(4._/**/REALKIND*sw**2)
    f(11) = (CI*eQED**2*gQCD**4*integralnorm*SwB*VCKMsu)/(2._/**/REALKIND*sw**2)
    f(12) = (eQED**2*gQCD**4*integralnorm*SwB*VCKMsu)/(sw**2*2._/**/REALKIND)
    f(13) = (CI*eQED**2*gQCD**4*integralnorm*SwF*VCKMsu)/(2._/**/REALKIND*sw**2)
    f(14) = (CI*eQED**2*gQCD**4*integralnorm*SwF*VCKMsu)/sw**2
    f(15) = (eQED**2*gQCD**4*integralnorm*SwF*VCKMsu)/(sw**2*2._/**/REALKIND)
    f(16) = (eQED**2*gQCD**4*integralnorm*SwF*VCKMsu)/sw**2

  c = [ 9*CI*f(10), 18*CI*f(10), CI*f(11), 3*CI*f(11), 8*CI*f(11), 9*CI*f(11), 18*CI*f(11), f(12), 3*f(12), 8*f(12), 9*f(12) &
    , 3*CI*f(13), 3*CI*f(14), f(15), 3*f(15), f(16), 3*f(16) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND) :: A(48)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_AV_Q(wf(:,-2),wf(:,-4),wf(:,2))
  call vert_VQ_A(wf(:,-5),wf(:,-3),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,5))
  call prop_Q_A(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,6))
  call vert_AW_Q(wf(:,5),wf(:,4),wf(:,7))
  call vert_WQ_A(wf(:,4),wf(:,-3),wf(:,8))
  call vert_AV_Q(wf(:,5),wf(:,-5),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,11),ZERO,0_intkind1,wf(:,10))
  call vert_AV_Q(wf(:,-2),wf(:,-5),wf(:,11))
  call vert_VQ_A(wf(:,-4),wf(:,-3),wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,36),ZERO,0_intkind1,wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,24),ZERO,0_intkind1,wf(:,14))
  call vert_AW_Q(wf(:,13),wf(:,4),wf(:,15))
  call vert_AW_Q(wf(:,-2),wf(:,4),wf(:,16))
  call vert_VQ_A(wf(:,-5),wf(:,14),wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call vert_AV_Q(wf(:,13),wf(:,-4),wf(:,19))
  call vert_VQ_A(wf(:,-4),wf(:,6),wf(:,20))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,21))
  call vert_VQ_A(wf(:,21),wf(:,-3),wf(:,22))
  call vert_AV_Q(wf(:,-2),wf(:,21),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,52),ZERO,0_intkind1,wf(:,24))
  call counter_AW_Q(wf(:,5),wf(:,4),wf(:,25))
  call counter_AV_Q(wf(:,5),wf(:,-5),wf(:,26))
  call counter_AW_Q(wf(:,13),wf(:,4),wf(:,27))
  call counter_VQ_A(wf(:,-5),wf(:,14),wf(:,28))
  call counter_AV_Q(wf(:,13),wf(:,-4),wf(:,29))
  call counter_VQ_A(wf(:,-4),wf(:,6),wf(:,30))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,31))
  call vert_VQ_A(wf(:,31),wf(:,-3),wf(:,32))
  call vert_AV_Q(wf(:,-2),wf(:,31),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,52),ZERO,0_intkind1,wf(:,34))
  call counter_WQ_A(wf(:,4),wf(:,-3),wf(:,35))
  call prop_A_Q(wf(:,9),Q(:,52),ZERO,0_intkind1,wf(:,36))
  call counter_VQ_A(wf(:,-5),wf(:,-3),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,40),ZERO,0_intkind1,wf(:,38))
  call prop_A_Q(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,39))
  call counter_VQ_A(wf(:,21),wf(:,-3),wf(:,40))
  call vert_VQ_A(wf(:,-4),wf(:,38),wf(:,41))
  call counter_VQ_A(wf(:,-4),wf(:,-3),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,24),ZERO,0_intkind1,wf(:,43))
  call vert_VQ_A(wf(:,-5),wf(:,43),wf(:,44))
  call counter_AW_Q(wf(:,-2),wf(:,4),wf(:,45))
  call prop_Q_A(wf(:,17),Q(:,56),ZERO,0_intkind1,wf(:,46))
  call counter_AV_Q(wf(:,-2),wf(:,-5),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,36),ZERO,0_intkind1,wf(:,48))
  call vert_AW_Q(wf(:,48),wf(:,4),wf(:,49))
  call prop_Q_A(wf(:,20),Q(:,56),ZERO,0_intkind1,wf(:,50))
  call counter_AV_Q(wf(:,-2),wf(:,21),wf(:,51))
  call prop_Q_A(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,52))
  call vert_AV_Q(wf(:,48),wf(:,-4),wf(:,53))
  call counter_AV_Q(wf(:,-2),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,20),ZERO,0_intkind1,wf(:,55))
  call vert_AW_Q(wf(:,55),wf(:,4),wf(:,56))
  call vert_AV_Q(wf(:,55),wf(:,-5),wf(:,57))
  call vert_WQ_A(wf(:,4),wf(:,6),wf(:,58))
  call counter_A_Q(ctqq,wf(:,5),Q(:,20),wf(:,59))
  call prop_Q_A(wf(:,58),Q(:,43),ZERO,0_intkind1,wf(:,60))
  call counter_Q_A(ctqq,wf(:,6),Q(:,40),wf(:,61))
  call prop_A_Q(wf(:,7),Q(:,23),ZERO,0_intkind1,wf(:,62))
  call prop_A_Q(wf(:,59),Q(:,20),ZERO,0_intkind1,wf(:,63))
  call vert_AV_Q(wf(:,63),wf(:,-5),wf(:,64))
  call counter_Q_A(ctqq,wf(:,10),Q(:,11),wf(:,65))
  call vert_WQ_A(wf(:,4),wf(:,14),wf(:,66))
  call counter_A_Q(ctqq,wf(:,13),Q(:,36),wf(:,67))
  call prop_Q_A(wf(:,66),Q(:,27),ZERO,0_intkind1,wf(:,68))
  call counter_Q_A(ctqq,wf(:,14),Q(:,24),wf(:,69))
  call prop_A_Q(wf(:,15),Q(:,39),ZERO,0_intkind1,wf(:,70))
  call counter_A_Q(ctqq,wf(:,18),Q(:,7),wf(:,71))
  call prop_Q_A(wf(:,69),Q(:,24),ZERO,0_intkind1,wf(:,72))
  call vert_VQ_A(wf(:,-5),wf(:,72),wf(:,73))
  call prop_A_Q(wf(:,67),Q(:,36),ZERO,0_intkind1,wf(:,74))
  call vert_AV_Q(wf(:,74),wf(:,-4),wf(:,75))
  call prop_Q_A(wf(:,61),Q(:,40),ZERO,0_intkind1,wf(:,76))
  call vert_VQ_A(wf(:,-4),wf(:,76),wf(:,77))
  call counter_A_Q(ctqq,wf(:,24),Q(:,52),wf(:,78))
  call counter_V_V(ctGG,wf(:,21),Q(:,48),wf(:,79))
  call vert_AV_Q(wf(:,-2),wf(:,79),wf(:,80))
  call vert_VQ_A(wf(:,79),wf(:,-3),wf(:,81))
  call vert_QA_V(wf(:,-3),wf(:,18),wf(:,82))
  call vert_AV_Q(wf(:,18),wf(:,-4),wf(:,83))
  call prop_A_Q(wf(:,83),Q(:,23),ZERO,0_intkind1,wf(:,84))
  call vert_AV_Q(wf(:,18),wf(:,-5),wf(:,85))
  call prop_A_Q(wf(:,85),Q(:,39),ZERO,0_intkind1,wf(:,86))
  call vert_QA_V(wf(:,10),wf(:,-2),wf(:,87))
  call vert_VQ_A(wf(:,-4),wf(:,10),wf(:,88))
  call prop_Q_A(wf(:,88),Q(:,27),ZERO,0_intkind1,wf(:,89))
  call vert_VQ_A(wf(:,-5),wf(:,10),wf(:,90))
  call prop_Q_A(wf(:,90),Q(:,43),ZERO,0_intkind1,wf(:,91))

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
  den(18) = 1 / (Q(5,48))
  den(20) = 1 / (Q(5,52))
  den(27) = 1 / (Q(5,56))
  den(36) = 1 / (Q(5,43))
  den(39) = 1 / (Q(5,23))
  den(46) = 1 / (Q(5,27))
  den(49) = 1 / (Q(5,39))
  den(66) = 1 / (Q(5,15))

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
  den(17) = den(3)*den(14)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(1)*den(21)
  den(23) = den(2)*den(20)
  den(24) = den(1)*den(23)
  den(25) = den(9)*den(20)
  den(26) = den(1)*den(25)
  den(28) = den(10)*den(27)
  den(29) = den(1)*den(28)
  den(30) = den(3)*den(27)
  den(31) = den(1)*den(30)
  den(32) = den(7)*den(18)
  den(33) = den(18)*den(27)
  den(34) = den(1)*den(33)
  den(35) = den(1)*den(3)
  den(37) = den(35)*den(36)
  den(38) = den(2)*den(37)
  den(40) = den(4)*den(39)
  den(41) = den(3)*den(40)
  den(42) = den(2)**2
  den(43) = den(7)*den(42)
  den(44) = den(7)*den(23)
  den(45) = den(1)*den(10)
  den(47) = den(45)*den(46)
  den(48) = den(9)*den(47)
  den(50) = den(11)*den(49)
  den(51) = den(10)*den(50)
  den(52) = den(14)*den(28)
  den(53) = den(10)**2
  den(54) = den(14)*den(53)
  den(55) = den(9)**2
  den(56) = den(7)*den(55)
  den(57) = den(7)*den(25)
  den(58) = den(14)*den(30)
  den(59) = den(3)**2
  den(60) = den(14)*den(59)
  den(61) = den(14)*den(33)
  den(62) = den(7)*den(21)
  den(63) = den(18)**2
  den(64) = den(7)*den(63)
  den(65) = den(14)*den(63)
  den(67) = den(14)*den(66)
  den(68) = den(14)*den(39)
  den(69) = den(14)*den(49)
  den(70) = den(7)*den(66)
  den(71) = den(7)*den(46)
  den(72) = den(7)*den(36)
  den(73) = den(1)*den(2)*den(3)
  den(74) = den(1)*den(9)*den(10)
  den(75) = den(1)*den(18)
  den(76) = den(2)*den(72)
  den(77) = den(10)*den(69)
  den(78) = den(9)*den(71)
  den(79) = den(3)*den(68)
  den(80) = den(18)*den(67)
  den(81) = den(18)*den(70)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(48)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(17)
  A(7) = cont_QA(wf(:,18),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,8),wf(:,24)) * den(22)

  A(9) = cont_QA(wf(:,6),wf(:,25)) * den(5)
  A(10) = cont_QA(wf(:,10),wf(:,26)) * den(8)
  A(11) = cont_QA(wf(:,14),wf(:,27)) * den(12)
  A(12) = cont_QA(wf(:,18),wf(:,28)) * den(15)
  A(13) = cont_QA(wf(:,10),wf(:,29)) * den(16)
  A(14) = cont_QA(wf(:,18),wf(:,30)) * den(17)
  A(15) = cont_QA(wf(:,18),wf(:,32)) * den(19)
  A(16) = cont_QA(wf(:,8),wf(:,34)) * den(22)
  A(17) = cont_QA(wf(:,35),wf(:,36)) * den(24)
  A(18) = cont_QA(wf(:,7),wf(:,38)) * den(5)
  A(19) = cont_QA(wf(:,35),wf(:,39)) * den(26)
  A(20) = cont_QA(wf(:,18),wf(:,40)) * den(19)
  A(21) = cont_QA(wf(:,24),wf(:,35)) * den(22)
  A(22) = cont_QA(wf(:,18),wf(:,41)) * den(17)
  A(23) = cont_QA(wf(:,15),wf(:,43)) * den(12)
  A(24) = cont_QA(wf(:,18),wf(:,44)) * den(15)
  A(25) = cont_QA(wf(:,45),wf(:,46)) * den(29)
  A(26) = cont_QA(wf(:,14),wf(:,49)) * den(12)
  A(27) = cont_QA(wf(:,45),wf(:,50)) * den(31)
  A(28) = cont_QA(wf(:,10),wf(:,51)) * den(32)
  A(29) = cont_QA(wf(:,45),wf(:,52)) * den(34)
  A(30) = cont_QA(wf(:,10),wf(:,53)) * den(16)
  A(31) = cont_QA(wf(:,6),wf(:,56)) * den(5)
  A(32) = cont_QA(wf(:,10),wf(:,57)) * den(8)
  A(33) = cont_QA(wf(:,59),wf(:,60)) * den(38)
  A(34) = cont_QA(wf(:,61),wf(:,62)) * den(41)
  A(35) = cont_QA(wf(:,10),wf(:,64)) * den(43)
  A(36) = cont_QA(wf(:,36),wf(:,65)) * den(44)
  A(37) = cont_QA(wf(:,67),wf(:,68)) * den(48)
  A(38) = cont_QA(wf(:,69),wf(:,70)) * den(51)
  A(39) = cont_QA(wf(:,46),wf(:,71)) * den(52)
  A(40) = cont_QA(wf(:,18),wf(:,73)) * den(54)
  A(41) = cont_QA(wf(:,10),wf(:,75)) * den(56)
  A(42) = cont_QA(wf(:,39),wf(:,65)) * den(57)
  A(43) = cont_QA(wf(:,50),wf(:,71)) * den(58)
  A(44) = cont_QA(wf(:,18),wf(:,77)) * den(60)
  A(45) = cont_QA(wf(:,52),wf(:,71)) * den(61)
  A(46) = cont_QA(wf(:,10),wf(:,78)) * den(62)
  A(47) = cont_QA(wf(:,10),wf(:,80)) * den(64)
  A(48) = cont_QA(wf(:,18),wf(:,81)) * den(65)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(48)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (-A(1)-A(2)-A(6))*f(1)+CI*(-A(7)-A(8))*f(2)
  M1(2) = (-A(3)-A(4)-A(5))*f(1)+CI*(A(7)+A(8))*f(2)

  M2(1) = (A(33)+A(34)+A(35)+A(36)+A(43)+A(44))*f(3)+CI*(A(45)+A(46)+A(47)+A(48))*f(4)+(-A(10)-A(14)-A(18)-A(22)-A(31)-A(32))*f(5) &
       +CI*(-A(20)-A(28))*f(6)+(-A(9)-A(17)-A(27))*f(7)+CI*(-A(21)-A(29))*f(8)+CI*(-A(15)-A(16))*f(9)
  M2(2) = (A(37)+A(38)+A(39)+A(40)+A(41)+A(42))*f(3)+CI*(-A(45)-A(46)-A(47)-A(48))*f(4)+(-A(12)-A(13)-A(23)-A(24)-A(26) &
       -A(30))*f(5)+CI*(A(20)+A(28))*f(6)+(-A(11)-A(19)-A(25))*f(7)+CI*(A(21)+A(29))*f(8)+CI*(A(15)+A(16))*f(9)

end subroutine colourvectors

end module ol_loop_pplnjj_ckm_neexuxsgg_1_/**/REALKIND
