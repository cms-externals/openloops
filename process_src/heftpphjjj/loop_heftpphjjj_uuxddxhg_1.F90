
module ol_colourmatrix_heftpphjjj_uuxddxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(92,4), K2(4,4), KL(4,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  36,  12,  12,   0]
  K1( 2,:) = [  12,  36,   0,  12]
  K1( 3,:) = [  12,   0,  36,  12]
  K1( 4,:) = [   0,  12,  12,  36]
  K1( 5,:) = [  48,  16,  16,   0]
  K1( 6,:) = [  16,  48,   0,  16]
  K1( 7,:) = [  16,   0,  48,  16]
  K1( 8,:) = [   0,  16,  16,  48]
  K1( 9,:) = [   6,   2,   2,   0]
  K1(10,:) = [   2,   0,  -6, -16]
  K1(11,:) = [   2,  -6,   0, -16]
  K1(12,:) = [   0, -16, -16, -48]
  K1(13,:) = [  48,  16,  16,   0]
  K1(14,:) = [  16,  48,   0,  16]
  K1(15,:) = [  16,   0,  48,  16]
  K1(16,:) = [   0,  16,  16,  48]
  K1(17,:) = [   0,  16,  -2,   6]
  K1(18,:) = [  16,   0,   6,  -2]
  K1(19,:) = [  -2,   6,   0,  16]
  K1(20,:) = [   6,  -2,  16,   0]
  K1(21,:) = [   0,   2, -16,  -6]
  K1(22,:) = [   2,   6,   0,   2]
  K1(23,:) = [ -16,   0, -48, -16]
  K1(24,:) = [  -6,   2, -16,   0]
  K1(25,:) = [  48,  16,  16,   0]
  K1(26,:) = [  16,  48,   0,  16]
  K1(27,:) = [  16,   0,  48,  16]
  K1(28,:) = [   0,  16,  16,  48]
  K1(29,:) = [   0, -16,   2,  -6]
  K1(30,:) = [ -16, -48,   0, -16]
  K1(31,:) = [   2,   0,   6,   2]
  K1(32,:) = [  -6, -16,   2,   0]
  K1(33,:) = [   0,  -2,  16,   6]
  K1(34,:) = [  -2,   0,   6,  16]
  K1(35,:) = [  16,   6,   0,  -2]
  K1(36,:) = [   6,  16,  -2,   0]
  K1(37,:) = [ -48, -16, -16,   0]
  K1(38,:) = [ -16,   0,  -6,   2]
  K1(39,:) = [ -16,  -6,   0,   2]
  K1(40,:) = [   0,   2,   2,   6]
  K1(41,:) = [  48,  16,  16,   0]
  K1(42,:) = [  16,  48,   0,  16]
  K1(43,:) = [  16,   0,  48,  16]
  K1(44,:) = [   0,  16,  16,  48]
  K1(45,:) = [   0,   0,   0,   0]
  K1(46,:) = [   0,   0,   0,   0]
  K1(47,:) = [   0,   0,   0,   0]
  K1(48,:) = [   0,   0,   0,   0]
  K1(49,:) = [   0,   0,   0,   0]
  K1(50,:) = [   0,   0,   0,   0]
  K1(51,:) = [   0,   0,   0,   0]
  K1(52,:) = [   0,   0,   0,   0]
  K1(53,:) = [   0,   0,   0,   0]
  K1(54,:) = [   0,   0,   0,   0]
  K1(55,:) = [   0,   0,   0,   0]
  K1(56,:) = [   0,   0,   0,   0]
  K1(57,:) = [   0,   0,   0,   0]
  K1(58,:) = [   0,   0,   0,   0]
  K1(59,:) = [   0,   0,   0,   0]
  K1(60,:) = [   0,   0,   0,   0]
  K1(61,:) = [   0,   0,   0,   0]
  K1(62,:) = [   0,   0,   0,   0]
  K1(63,:) = [   0,   0,   0,   0]
  K1(64,:) = [   0,   0,   0,   0]
  K1(65,:) = [ -54, -18, -18,   0]
  K1(66,:) = [ -18,   0,   0,  18]
  K1(67,:) = [ -18,   0, -54, -18]
  K1(68,:) = [   0,  18, -18,   0]
  K1(69,:) = [ -54, -18, -18,   0]
  K1(70,:) = [ -18, -54,   0, -18]
  K1(71,:) = [ -18,   0,   0,  18]
  K1(72,:) = [   0, -18,  18,   0]
  K1(73,:) = [   0, -18,  18,   0]
  K1(74,:) = [ -18, -54,   0, -18]
  K1(75,:) = [  18,   0,   0, -18]
  K1(76,:) = [   0, -18, -18, -54]
  K1(77,:) = [   0,  18, -18,   0]
  K1(78,:) = [  18,   0,   0, -18]
  K1(79,:) = [ -18,   0, -54, -18]
  K1(80,:) = [   0, -18, -18, -54]
  K1(81,:) = [   0,   0,   0,   0]
  K1(82,:) = [   0,   0,   0,   0]
  K1(83,:) = [   0,   0,   0,   0]
  K1(84,:) = [   0,   0,   0,   0]
  K1(85,:) = [ 108,  36,  36,   0]
  K1(86,:) = [  36, 108,   0,  36]
  K1(87,:) = [  36,   0, 108,  36]
  K1(88,:) = [   0,  36,  36, 108]
  K1(89,:) = [   0,   0,   0,   0]
  K1(90,:) = [   0,   0,   0,   0]
  K1(91,:) = [   0,   0,   0,   0]
  K1(92,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 12,  4,  4,  0]
  K2(2,:) = [  4, 12,  0,  4]
  K2(3,:) = [  4,  0, 12,  4]
  K2(4,:) = [  0,  4,  4, 12]

  KL(1,:) = [ 12,  4,  4,  0]
  KL(2,:) = [  4, 12,  0,  4]
  KL(3,:) = [  4,  0, 12,  4]
  KL(4,:) = [  0,  4,  4, 12]

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphjjj_uuxddxhg_1_/**/REALKIND



module ol_forced_parameters_heftpphjjj_uuxddxhg_1_/**/REALKIND
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
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphjjj_uuxddxhg_1_/**/REALKIND

module ol_loop_heftpphjjj_uuxddxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(22), c(29)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:230)
  ! denominators
  complex(REALKIND), save :: den(229)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(4,32)
  ! zero helicity identifier
  logical,           save :: zerohel(32) = .true., zerohel_ct(32) = .true.

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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED*gQCD**5)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 2) = (eQED*gQCD**5)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 3) = (CI*countertermnorm*eQED*gQCD**7)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 4) = (countertermnorm*eQED*gQCD**7)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 5) = (CI*countertermnorm*ctGqq*eQED*gQCD**7)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 6) = (countertermnorm*ctGqq*eQED*gQCD**7)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 7) = (countertermnorm*ctHEFTgggh*eQED*gQCD**7)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 8) = (countertermnorm*ctVVV*eQED*gQCD**7)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 9) = (CI*countertermnorm*eQED*gQCD**7*R2HEFTghqq)/(24._/**/REALKIND*MW*pi**2*sw)
    f(10) = (countertermnorm*eQED*gQCD**7*R2HEFTghqq)/(MW*pi**2*sw*24._/**/REALKIND)
    f(11) = (CI*countertermnorm*eQED*gQCD**7*R2HEFThqq)/(24._/**/REALKIND*MW*pi**2*sw)
    f(12) = (countertermnorm*eQED*gQCD**7*R2HEFThqq)/(MW*pi**2*sw*24._/**/REALKIND)
    f(13) = (CI*eQED*gQCD**7*integralnorm*SwB)/(48._/**/REALKIND*MW*pi**2*sw)
    f(14) = (CI*eQED*gQCD**7*integralnorm*SwB)/(24._/**/REALKIND*MW*pi**2*sw)
    f(15) = (eQED*gQCD**7*integralnorm*SwB)/(MW*pi**2*sw*48._/**/REALKIND)
    f(16) = (eQED*gQCD**7*integralnorm*SwB)/(MW*pi**2*sw*24._/**/REALKIND)
    f(17) = (CI*eQED*gQCD**5*integralnorm*MB*SwF)/(2._/**/REALKIND*MW*sw)
    f(18) = (eQED*gQCD**5*integralnorm*MB*SwF)/(MW*sw*2._/**/REALKIND)
    f(19) = (CI*eQED*gQCD**7*integralnorm*SwF)/(24._/**/REALKIND*MW*pi**2*sw)
    f(20) = (CI*eQED*gQCD**7*integralnorm*SwF)/(12._/**/REALKIND*MW*pi**2*sw)
    f(21) = (eQED*gQCD**7*integralnorm*SwF)/(MW*pi**2*sw*24._/**/REALKIND)
    f(22) = (eQED*gQCD**7*integralnorm*SwF)/(MW*pi**2*sw*12._/**/REALKIND)

  c = [ 27*CI*f(13), 54*CI*f(13), 3*CI*f(14), 9*CI*f(14), 24*CI*f(14), 27*CI*f(14), 54*CI*f(14), 18*f(15), 54*f(15), f(16) &
    , 3*f(16), 6*f(16), 8*f(16), 9*f(16), 10*f(16), 18*f(16), 21*f(16), 24*f(16), 27*f(16), 54*f(16), 9*CI*f(17), 3*f(18), 9*f(18) &
    , 9*CI*f(19), 9*CI*f(20), 3*f(21), 9*f(21), 3*f(22), 9*f(22) ]
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
  complex(REALKIND), intent(out) :: M1(4), M2(4)
  complex(REALKIND) :: A(132)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_HGG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,3),Q(:,51))
  call vert_HG_G(wf(:,-4),wf(:,-5),Q(:,32),wf(:,4),Q(:,48))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,2),Q(:,12),wf(:,5))
  call vert_HG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,6),Q(:,19))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,-5),Q(:,32),wf(:,7))
  call vert_HG_G(wf(:,-4),wf(:,2),Q(:,12),wf(:,8),Q(:,28))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,9))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,10))
  call prop_Q_A(wf(:,10),Q(:,36),ZERO,0_intkind1,wf(:,11))
  call vert_QA_V(wf(:,11),wf(:,-3),wf(:,12))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,40),ZERO,0_intkind1,wf(:,14))
  call vert_QA_V(wf(:,-2),wf(:,14),wf(:,15))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,-3),wf(:,4),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,4),wf(:,-2),wf(:,19))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,21))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,33),ZERO,0_intkind1,wf(:,23))
  call vert_QA_V(wf(:,23),wf(:,-1),wf(:,24))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,25))
  call prop_A_Q(wf(:,25),Q(:,34),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,0),wf(:,26),wf(:,27))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,28))
  call vert_AV_Q(wf(:,-1),wf(:,4),wf(:,29))
  call prop_Q_A(wf(:,28),Q(:,13),ZERO,0_intkind1,wf(:,30))
  call vert_VQ_A(wf(:,4),wf(:,0),wf(:,31))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,32))
  call prop_Q_A(wf(:,31),Q(:,49),ZERO,0_intkind1,wf(:,33))
  call counter_HGG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,34),Q(:,51))
  call counter_AVH_Q(wf(:,-3),wf(:,1),wf(:,-4),wf(:,35))
  call counter_AVH_Q(wf(:,-3),wf(:,-5),wf(:,-4),wf(:,36))
  call prop_A_Q(wf(:,36),Q(:,56),ZERO,0_intkind1,wf(:,37))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,38))
  call counter_VHQ_A(wf(:,1),wf(:,-4),wf(:,-2),wf(:,39))
  call counter_VHQ_A(wf(:,-5),wf(:,-4),wf(:,-2),wf(:,40))
  call prop_Q_A(wf(:,40),Q(:,52),ZERO,0_intkind1,wf(:,41))
  call counter_HQA_V(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,42))
  call counter_HQA_V(wf(:,-4),wf(:,23),wf(:,-1),wf(:,43))
  call counter_AVH_Q(wf(:,-1),wf(:,-5),wf(:,-4),wf(:,44))
  call prop_A_Q(wf(:,44),Q(:,50),ZERO,0_intkind1,wf(:,45))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,46))
  call vert_HGG_G(wf(:,-4),wf(:,46),Q(:,3),wf(:,-5),Q(:,32),wf(:,47),Q(:,51))
  call counter_HQA_V(wf(:,-4),wf(:,0),wf(:,26),wf(:,48))
  call counter_VHQ_A(wf(:,-5),wf(:,-4),wf(:,0),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,49),ZERO,0_intkind1,wf(:,50))
  call counter_HQA_V(wf(:,-4),wf(:,0),wf(:,-1),wf(:,51))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,2),Q(:,12),wf(:,52))
  call counter_UV_W(wf(:,2),Q(:,12),wf(:,-5),Q(:,32),wf(:,53))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,54))
  call vert_HGG_G(wf(:,-4),wf(:,2),Q(:,12),wf(:,-5),Q(:,32),wf(:,55),Q(:,60))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,56))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,57))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,2),Q(:,12),wf(:,58),Q(:,28))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,1),Q(:,3),wf(:,59),Q(:,19))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,-5),Q(:,32),wf(:,60),Q(:,48))
  call counter_QH_A(wf(:,11),Q(:,36),wf(:,-4),wf(:,61),Q(:,52))
  call prop_A_Q(wf(:,20),Q(:,11),ZERO,0_intkind1,wf(:,62))
  call counter_HA_Q(wf(:,-4),wf(:,14),Q(:,40),wf(:,63),Q(:,56))
  call vert_AV_Q(wf(:,-3),wf(:,60),wf(:,64))
  call vert_VQ_A(wf(:,60),wf(:,-2),wf(:,65))
  call prop_Q_A(wf(:,65),Q(:,52),ZERO,0_intkind1,wf(:,66))
  call counter_QA_V(wf(:,11),wf(:,-3),wf(:,67))
  call counter_AV_Q(wf(:,-3),wf(:,4),wf(:,68))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,69))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,70))
  call prop_A_Q(wf(:,70),Q(:,40),ZERO,0_intkind1,wf(:,71))
  call vert_QA_V(wf(:,-2),wf(:,71),wf(:,72))
  call counter_HA_Q(wf(:,-4),wf(:,-3),Q(:,8),wf(:,73),Q(:,24))
  call prop_A_Q(wf(:,73),Q(:,24),ZERO,0_intkind1,wf(:,74))
  call vert_VQ_A(wf(:,1),wf(:,11),wf(:,75))
  call vert_AV_Q(wf(:,74),wf(:,-5),wf(:,76))
  call vert_QA_V(wf(:,-2),wf(:,74),wf(:,77))
  call counter_QA_V(wf(:,-2),wf(:,14),wf(:,78))
  call counter_VQ_A(wf(:,4),wf(:,-2),wf(:,79))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,80))
  call prop_A_Q(wf(:,17),Q(:,56),ZERO,0_intkind1,wf(:,81))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,82))
  call prop_Q_A(wf(:,82),Q(:,36),ZERO,0_intkind1,wf(:,83))
  call vert_QA_V(wf(:,83),wf(:,-3),wf(:,84))
  call counter_QH_A(wf(:,-2),Q(:,4),wf(:,-4),wf(:,85),Q(:,20))
  call prop_Q_A(wf(:,85),Q(:,20),ZERO,0_intkind1,wf(:,86))
  call vert_VQ_A(wf(:,1),wf(:,86),wf(:,87))
  call vert_VQ_A(wf(:,-5),wf(:,86),wf(:,88))
  call vert_QA_V(wf(:,86),wf(:,-3),wf(:,89))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,38),Q(:,12),wf(:,90))
  call vert_UV_W(wf(:,38),Q(:,12),wf(:,-5),Q(:,32),wf(:,91))
  call vert_HG_G(wf(:,-4),wf(:,38),Q(:,12),wf(:,92),Q(:,28))
  call counter_QH_A(wf(:,23),Q(:,33),wf(:,-4),wf(:,93),Q(:,49))
  call prop_A_Q(wf(:,32),Q(:,14),ZERO,0_intkind1,wf(:,94))
  call counter_HA_Q(wf(:,-4),wf(:,26),Q(:,34),wf(:,95),Q(:,50))
  call vert_AV_Q(wf(:,-1),wf(:,60),wf(:,96))
  call vert_VQ_A(wf(:,60),wf(:,0),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,49),ZERO,0_intkind1,wf(:,98))
  call vert_AV_Q(wf(:,-1),wf(:,38),wf(:,99))
  call vert_VQ_A(wf(:,38),wf(:,0),wf(:,100))
  call prop_Q_A(wf(:,100),Q(:,13),ZERO,0_intkind1,wf(:,101))
  call counter_QA_V(wf(:,23),wf(:,-1),wf(:,102))
  call counter_AV_Q(wf(:,-1),wf(:,4),wf(:,103))
  call counter_AV_Q(wf(:,-1),wf(:,2),wf(:,104))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,105))
  call prop_A_Q(wf(:,105),Q(:,34),ZERO,0_intkind1,wf(:,106))
  call vert_QA_V(wf(:,0),wf(:,106),wf(:,107))
  call counter_HA_Q(wf(:,-4),wf(:,-1),Q(:,2),wf(:,108),Q(:,18))
  call prop_A_Q(wf(:,108),Q(:,18),ZERO,0_intkind1,wf(:,109))
  call vert_QA_V(wf(:,23),wf(:,109),wf(:,110))
  call vert_AV_Q(wf(:,109),wf(:,-5),wf(:,111))
  call vert_QA_V(wf(:,0),wf(:,109),wf(:,112))
  call counter_QA_V(wf(:,0),wf(:,26),wf(:,113))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,114))
  call prop_A_Q(wf(:,29),Q(:,50),ZERO,0_intkind1,wf(:,115))
  call counter_VQ_A(wf(:,4),wf(:,0),wf(:,116))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,117))
  call prop_Q_A(wf(:,117),Q(:,33),ZERO,0_intkind1,wf(:,118))
  call vert_QA_V(wf(:,118),wf(:,-1),wf(:,119))
  call counter_QH_A(wf(:,0),Q(:,1),wf(:,-4),wf(:,120),Q(:,17))
  call prop_Q_A(wf(:,120),Q(:,17),ZERO,0_intkind1,wf(:,121))
  call vert_QA_V(wf(:,121),wf(:,26),wf(:,122))
  call vert_QA_V(wf(:,121),wf(:,-1),wf(:,123))
  call vert_VQ_A(wf(:,-5),wf(:,121),wf(:,124))
  call vert_UV_W(wf(:,46),Q(:,3),wf(:,2),Q(:,12),wf(:,125))
  call vert_HG_G(wf(:,-4),wf(:,46),Q(:,3),wf(:,126),Q(:,19))
  call vert_UV_W(wf(:,46),Q(:,3),wf(:,-5),Q(:,32),wf(:,127))
  call vert_VQ_A(wf(:,46),wf(:,-2),wf(:,128))
  call prop_Q_A(wf(:,128),Q(:,7),ZERO,0_intkind1,wf(:,129))
  call vert_AV_Q(wf(:,-3),wf(:,46),wf(:,130))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,4),Q(:,48),wf(:,131))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,4),Q(:,48),wf(:,132))
  call counter_V_V(ctGG,wf(:,4),Q(:,48),wf(:,133))
  call vert_HG_G(wf(:,-4),wf(:,56),Q(:,3),wf(:,134),Q(:,19))
  call vert_UV_W(wf(:,56),Q(:,3),wf(:,-5),Q(:,32),wf(:,135))
  call vert_HG_G(wf(:,-4),wf(:,57),Q(:,12),wf(:,136),Q(:,28))
  call vert_UV_W(wf(:,57),Q(:,12),wf(:,-5),Q(:,32),wf(:,137))
  call counter_V_V(ctGG,wf(:,6),Q(:,19),wf(:,138))
  call counter_V_V(ctGG,wf(:,8),Q(:,28),wf(:,139))
  call counter_Q_A(ctqq,wf(:,11),Q(:,36),wf(:,140))
  call prop_Q_A(wf(:,140),Q(:,36),ZERO,0_intkind1,wf(:,141))
  call vert_QA_V(wf(:,141),wf(:,-3),wf(:,142))
  call counter_V_V(ctGG,wf(:,12),Q(:,44),wf(:,143))
  call counter_A_Q(ctqq,wf(:,14),Q(:,40),wf(:,144))
  call prop_A_Q(wf(:,144),Q(:,40),ZERO,0_intkind1,wf(:,145))
  call vert_QA_V(wf(:,-2),wf(:,145),wf(:,146))
  call counter_V_V(ctGG,wf(:,15),Q(:,44),wf(:,147))
  call vert_VQ_A(wf(:,56),wf(:,-2),wf(:,148))
  call vert_AV_Q(wf(:,-3),wf(:,56),wf(:,149))
  call counter_Q_A(ctqq,wf(:,18),Q(:,7),wf(:,150))
  call counter_Q_A(ctqq,wf(:,21),Q(:,52),wf(:,151))
  call vert_VQ_A(wf(:,133),wf(:,-2),wf(:,152))
  call vert_AV_Q(wf(:,-3),wf(:,133),wf(:,153))
  call counter_Q_A(ctqq,wf(:,23),Q(:,33),wf(:,154))
  call prop_Q_A(wf(:,154),Q(:,33),ZERO,0_intkind1,wf(:,155))
  call vert_QA_V(wf(:,155),wf(:,-1),wf(:,156))
  call counter_V_V(ctGG,wf(:,24),Q(:,35),wf(:,157))
  call counter_A_Q(ctqq,wf(:,26),Q(:,34),wf(:,158))
  call prop_A_Q(wf(:,158),Q(:,34),ZERO,0_intkind1,wf(:,159))
  call vert_QA_V(wf(:,0),wf(:,159),wf(:,160))
  call counter_V_V(ctGG,wf(:,27),Q(:,35),wf(:,161))
  call counter_Q_A(ctqq,wf(:,30),Q(:,13),wf(:,162))
  call counter_Q_A(ctqq,wf(:,33),Q(:,49),wf(:,163))
  call vert_VQ_A(wf(:,57),wf(:,0),wf(:,164))
  call vert_VQ_A(wf(:,133),wf(:,0),wf(:,165))
  call vert_AV_Q(wf(:,-1),wf(:,57),wf(:,166))
  call vert_AV_Q(wf(:,-1),wf(:,133),wf(:,167))
  call prop_Q_A(wf(:,75),Q(:,39),ZERO,0_intkind1,wf(:,168))
  call vert_AV_Q(wf(:,14),wf(:,1),wf(:,169))
  call prop_A_Q(wf(:,169),Q(:,43),ZERO,0_intkind1,wf(:,170))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,171))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,172))
  call prop_Q_A(wf(:,172),Q(:,39),ZERO,0_intkind1,wf(:,173))
  call vert_QA_V(wf(:,-2),wf(:,62),wf(:,174))
  call vert_AV_Q(wf(:,62),wf(:,-5),wf(:,175))
  call prop_A_Q(wf(:,175),Q(:,43),ZERO,0_intkind1,wf(:,176))
  call vert_VQ_A(wf(:,6),wf(:,-2),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,23),ZERO,0_intkind1,wf(:,178))
  call vert_AV_Q(wf(:,-3),wf(:,6),wf(:,179))
  call prop_A_Q(wf(:,179),Q(:,27),ZERO,0_intkind1,wf(:,180))
  call vert_UV_W(wf(:,6),Q(:,19),wf(:,-5),Q(:,32),wf(:,181))
  call vert_VQ_A(wf(:,9),wf(:,-2),wf(:,182))
  call prop_Q_A(wf(:,182),Q(:,39),ZERO,0_intkind1,wf(:,183))
  call vert_AV_Q(wf(:,-3),wf(:,9),wf(:,184))
  call prop_A_Q(wf(:,184),Q(:,43),ZERO,0_intkind1,wf(:,185))
  call vert_HG_G(wf(:,-4),wf(:,9),Q(:,35),wf(:,186),Q(:,51))
  call vert_VQ_A(wf(:,2),wf(:,23),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,45),ZERO,0_intkind1,wf(:,188))
  call vert_AV_Q(wf(:,26),wf(:,2),wf(:,189))
  call prop_A_Q(wf(:,189),Q(:,46),ZERO,0_intkind1,wf(:,190))
  call vert_QA_V(wf(:,30),wf(:,-1),wf(:,191))
  call vert_QA_V(wf(:,0),wf(:,94),wf(:,192))
  call vert_VQ_A(wf(:,-5),wf(:,30),wf(:,193))
  call prop_Q_A(wf(:,193),Q(:,45),ZERO,0_intkind1,wf(:,194))
  call vert_VQ_A(wf(:,8),wf(:,0),wf(:,195))
  call prop_Q_A(wf(:,195),Q(:,29),ZERO,0_intkind1,wf(:,196))
  call vert_VQ_A(wf(:,7),wf(:,0),wf(:,197))
  call prop_Q_A(wf(:,197),Q(:,45),ZERO,0_intkind1,wf(:,198))
  call vert_AV_Q(wf(:,94),wf(:,-5),wf(:,199))
  call prop_A_Q(wf(:,199),Q(:,46),ZERO,0_intkind1,wf(:,200))
  call vert_AV_Q(wf(:,-1),wf(:,8),wf(:,201))
  call prop_A_Q(wf(:,201),Q(:,30),ZERO,0_intkind1,wf(:,202))
  call vert_AV_Q(wf(:,-1),wf(:,7),wf(:,203))
  call prop_A_Q(wf(:,203),Q(:,46),ZERO,0_intkind1,wf(:,204))
  call vert_UV_W(wf(:,8),Q(:,28),wf(:,-5),Q(:,32),wf(:,205))
  call vert_HG_G(wf(:,-4),wf(:,7),Q(:,44),wf(:,206),Q(:,60))
  call vert_VQ_A(wf(:,24),wf(:,-2),wf(:,207))
  call prop_Q_A(wf(:,207),Q(:,39),ZERO,0_intkind1,wf(:,208))
  call vert_AV_Q(wf(:,-3),wf(:,24),wf(:,209))
  call prop_A_Q(wf(:,209),Q(:,43),ZERO,0_intkind1,wf(:,210))
  call vert_HG_G(wf(:,-4),wf(:,24),Q(:,35),wf(:,211),Q(:,51))
  call vert_VQ_A(wf(:,27),wf(:,-2),wf(:,212))
  call prop_Q_A(wf(:,212),Q(:,39),ZERO,0_intkind1,wf(:,213))
  call vert_AV_Q(wf(:,-3),wf(:,27),wf(:,214))
  call prop_A_Q(wf(:,214),Q(:,43),ZERO,0_intkind1,wf(:,215))
  call vert_HG_G(wf(:,-4),wf(:,27),Q(:,35),wf(:,216),Q(:,51))
  call vert_VQ_A(wf(:,12),wf(:,0),wf(:,217))
  call prop_Q_A(wf(:,217),Q(:,45),ZERO,0_intkind1,wf(:,218))
  call vert_AV_Q(wf(:,-1),wf(:,12),wf(:,219))
  call prop_A_Q(wf(:,219),Q(:,46),ZERO,0_intkind1,wf(:,220))
  call vert_HG_G(wf(:,-4),wf(:,12),Q(:,44),wf(:,221),Q(:,60))
  call vert_VQ_A(wf(:,15),wf(:,0),wf(:,222))
  call prop_Q_A(wf(:,222),Q(:,45),ZERO,0_intkind1,wf(:,223))
  call vert_AV_Q(wf(:,-1),wf(:,15),wf(:,224))
  call prop_A_Q(wf(:,224),Q(:,46),ZERO,0_intkind1,wf(:,225))
  call vert_HG_G(wf(:,-4),wf(:,15),Q(:,44),wf(:,226),Q(:,60))
  call vert_QA_V(wf(:,33),wf(:,-1),wf(:,227))
  call vert_QA_V(wf(:,0),wf(:,115),wf(:,228))
  call vert_QA_V(wf(:,21),wf(:,-3),wf(:,229))
  call vert_QA_V(wf(:,-2),wf(:,81),wf(:,230))

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
  den(4) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,19))
  den(9) = 1 / (Q(5,28))
  den(12) = 1 / (Q(5,36))
  den(13) = 1 / (Q(5,44))
  den(16) = 1 / (Q(5,40))
  den(19) = 1 / (Q(5,7))
  den(22) = 1 / (Q(5,52))
  den(25) = 1 / (Q(5,33))
  den(26) = 1 / (Q(5,35))
  den(29) = 1 / (Q(5,34))
  den(32) = 1 / (Q(5,13))
  den(35) = 1 / (Q(5,49))
  den(39) = 1 / (Q(5,56))
  den(47) = 1 / (Q(5,50))
  den(54) = 1 / (Q(5,60))
  den(57) = 1 / (Q(5,51))
  den(64) = 1 / (Q(5,11))
  den(69) = 1 / (Q(5,24))
  den(78) = 1 / (Q(5,20))
  den(84) = 1 / (Q(5,14))
  den(93) = 1 / (Q(5,18))
  den(105) = 1 / (Q(5,17))
  den(119) = 1 / (Q(5,15))
  den(159) = 1 / (Q(5,39))
  den(161) = 1 / (Q(5,43))
  den(167) = 1 / (Q(5,23))
  den(169) = 1 / (Q(5,27))
  den(175) = 1 / (Q(5,45))
  den(177) = 1 / (Q(5,46))
  den(182) = 1 / (Q(5,29))
  den(186) = 1 / (Q(5,30))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(12)*den(13)
  den(15) = den(1)*den(14)
  den(17) = den(13)*den(16)
  den(18) = den(1)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(4)*den(20)
  den(23) = den(4)*den(22)
  den(24) = den(1)*den(23)
  den(27) = den(25)*den(26)
  den(28) = den(2)*den(27)
  den(30) = den(26)*den(29)
  den(31) = den(2)*den(30)
  den(33) = den(2)*den(32)
  den(34) = den(4)*den(33)
  den(36) = den(4)*den(35)
  den(37) = den(2)*den(36)
  den(38) = den(1)*den(12)
  den(40) = den(1)*den(39)
  den(41) = den(1)*den(16)
  den(42) = den(1)*den(22)
  den(43) = den(1)*den(9)
  den(44) = den(9)*den(25)
  den(45) = den(9)*den(29)
  den(46) = den(2)*den(25)
  den(48) = den(2)*den(47)
  den(49) = den(2)*den(29)
  den(50) = den(2)*den(35)
  den(51) = den(2)*den(6)
  den(52) = den(6)*den(12)
  den(53) = den(6)*den(16)
  den(55) = den(2)*den(54)
  den(56) = den(1)*den(55)
  den(58) = den(1)*den(57)
  den(59) = den(2)*den(58)
  den(60) = den(1)*den(26)
  den(61) = den(2)*den(60)
  den(62) = den(2)*den(13)
  den(63) = den(1)*den(62)
  den(65) = den(1)*den(64)
  den(66) = den(12)*den(65)
  den(67) = den(16)*den(20)
  den(68) = den(7)*den(12)
  den(70) = den(38)*den(69)
  den(71) = den(20)*den(69)
  den(72) = den(9)*den(69)
  den(73) = den(1)*den(72)
  den(74) = den(7)*den(16)
  den(75) = den(4)*den(65)
  den(76) = den(4)*den(39)
  den(77) = den(1)*den(76)
  den(79) = den(1)*den(78)
  den(80) = den(16)*den(79)
  den(81) = den(65)*den(78)
  den(82) = den(9)*den(78)
  den(83) = den(1)*den(82)
  den(85) = den(2)*den(84)
  den(86) = den(25)*den(85)
  den(87) = den(29)*den(33)
  den(88) = den(27)*den(69)
  den(89) = den(30)*den(69)
  den(90) = den(27)*den(78)
  den(91) = den(30)*den(78)
  den(92) = den(10)*den(25)
  den(94) = den(25)*den(93)
  den(95) = den(2)*den(94)
  den(96) = den(33)*den(93)
  den(97) = den(6)*den(93)
  den(98) = den(2)*den(97)
  den(99) = den(12)*den(97)
  den(100) = den(16)*den(97)
  den(101) = den(10)*den(29)
  den(102) = den(4)*den(47)
  den(103) = den(2)*den(102)
  den(104) = den(4)*den(85)
  den(106) = den(29)*den(105)
  den(107) = den(2)*den(106)
  den(108) = den(6)*den(105)
  den(109) = den(2)*den(108)
  den(110) = den(85)*den(105)
  den(111) = den(12)*den(108)
  den(112) = den(16)*den(108)
  den(113) = den(2)*den(4)
  den(114) = den(54)*den(113)
  den(115) = den(1)*den(114)
  den(116) = den(1)*den(4)
  den(117) = den(57)*den(116)
  den(118) = den(2)*den(117)
  den(120) = den(3)*den(119)
  den(121) = den(4)*den(120)
  den(122) = den(1)**2
  den(123) = den(62)*den(122)
  den(124) = den(10)*den(122)
  den(125) = den(2)**2
  den(126) = den(60)*den(125)
  den(127) = den(7)*den(125)
  den(128) = den(7)*den(62)
  den(129) = den(10)*den(60)
  den(130) = den(14)*den(122)
  den(131) = den(12)**2
  den(132) = den(7)*den(131)
  den(133) = den(7)*den(14)
  den(134) = den(17)*den(122)
  den(135) = den(16)**2
  den(136) = den(7)*den(135)
  den(137) = den(7)*den(17)
  den(138) = den(76)*den(122)
  den(139) = den(23)*den(122)
  den(140) = den(20)*den(76)
  den(141) = den(23)*den(65)
  den(142) = den(4)**2
  den(143) = den(65)*den(142)
  den(144) = den(20)*den(142)
  den(145) = den(25)**2
  den(146) = den(10)*den(145)
  den(147) = den(10)*den(27)
  den(148) = den(27)*den(125)
  den(149) = den(29)**2
  den(150) = den(10)*den(149)
  den(151) = den(10)*den(30)
  den(152) = den(30)*den(125)
  den(153) = den(33)*den(102)
  den(154) = den(36)*den(85)
  den(155) = den(102)*den(125)
  den(156) = den(85)*den(142)
  den(157) = den(36)*den(125)
  den(158) = den(33)*den(142)
  den(160) = den(38)*den(159)
  den(162) = den(41)*den(161)
  den(163) = den(20)*den(119)
  den(164) = den(20)*den(159)
  den(165) = den(65)*den(119)
  den(166) = den(65)*den(161)
  den(168) = den(7)*den(167)
  den(170) = den(7)*den(169)
  den(171) = den(7)*den(57)
  den(172) = den(60)*den(159)
  den(173) = den(60)*den(161)
  den(174) = den(57)*den(60)
  den(176) = den(46)*den(175)
  den(178) = den(49)*den(177)
  den(179) = den(33)*den(119)
  den(180) = den(85)*den(119)
  den(181) = den(33)*den(175)
  den(183) = den(10)*den(182)
  den(184) = den(62)*den(175)
  den(185) = den(85)*den(177)
  den(187) = den(10)*den(186)
  den(188) = den(62)*den(177)
  den(189) = den(10)*den(54)
  den(190) = den(54)*den(62)
  den(191) = den(27)*den(159)
  den(192) = den(27)*den(161)
  den(193) = den(27)*den(57)
  den(194) = den(30)*den(159)
  den(195) = den(30)*den(161)
  den(196) = den(30)*den(57)
  den(197) = den(14)*den(175)
  den(198) = den(14)*den(177)
  den(199) = den(14)*den(54)
  den(200) = den(17)*den(175)
  den(201) = den(17)*den(177)
  den(202) = den(17)*den(54)
  den(203) = den(36)*den(57)
  den(204) = den(57)*den(102)
  den(205) = den(23)*den(54)
  den(206) = den(54)*den(76)
  den(207) = den(1)*den(2)*den(4)
  den(208) = den(1)*den(113)
  den(209) = den(2)*den(116)
  den(210) = den(2)*den(171)
  den(211) = den(2)*den(174)
  den(212) = den(1)*den(189)
  den(213) = den(1)*den(190)
  den(214) = den(12)*den(170)
  den(215) = den(1)*den(199)
  den(216) = den(16)*den(168)
  den(217) = den(1)*den(202)
  den(218) = den(4)*den(163)
  den(219) = den(4)*den(165)
  den(220) = den(1)*den(205)
  den(221) = den(1)*den(206)
  den(222) = den(2)*den(193)
  den(223) = den(25)*den(187)
  den(224) = den(2)*den(196)
  den(225) = den(29)*den(183)
  den(226) = den(4)*den(179)
  den(227) = den(2)*den(203)
  den(228) = den(4)*den(180)
  den(229) = den(2)*den(204)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(132)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,4),wf(:,5)) * den(5)
  A(3) = cont_VV(wf(:,6),wf(:,7)) * den(8)
  A(4) = cont_VV(wf(:,8),wf(:,9)) * den(11)
  A(5) = cont_VV(wf(:,6),wf(:,12)) * den(15)
  A(6) = cont_VV(wf(:,6),wf(:,15)) * den(18)
  A(7) = cont_QA(wf(:,17),wf(:,18)) * den(21)
  A(8) = cont_QA(wf(:,20),wf(:,21)) * den(24)
  A(9) = cont_VV(wf(:,8),wf(:,24)) * den(28)
  A(10) = cont_VV(wf(:,8),wf(:,27)) * den(31)
  A(11) = cont_QA(wf(:,29),wf(:,30)) * den(34)
  A(12) = cont_QA(wf(:,32),wf(:,33)) * den(37)

  A(13) = cont_VV(wf(:,2),wf(:,34)) * den(3)
  A(14) = cont_QA(wf(:,11),wf(:,35)) * den(38)
  A(15) = cont_QA(wf(:,16),wf(:,37)) * den(40)
  A(16) = cont_VV(wf(:,3),wf(:,38)) * den(3)
  A(17) = cont_QA(wf(:,14),wf(:,39)) * den(41)
  A(18) = cont_QA(wf(:,20),wf(:,41)) * den(42)
  A(19) = cont_VV(wf(:,9),wf(:,42)) * den(43)
  A(20) = cont_VV(wf(:,24),wf(:,42)) * den(44)
  A(21) = cont_VV(wf(:,27),wf(:,42)) * den(45)
  A(22) = cont_VV(wf(:,2),wf(:,43)) * den(46)
  A(23) = cont_QA(wf(:,28),wf(:,45)) * den(48)
  A(24) = cont_VV(wf(:,2),wf(:,47)) * den(3)
  A(25) = cont_VV(wf(:,2),wf(:,48)) * den(49)
  A(26) = cont_QA(wf(:,32),wf(:,50)) * den(50)
  A(27) = cont_VV(wf(:,7),wf(:,51)) * den(51)
  A(28) = cont_VV(wf(:,12),wf(:,51)) * den(52)
  A(29) = cont_VV(wf(:,15),wf(:,51)) * den(53)
  A(30) = cont_VV(wf(:,4),wf(:,52)) * den(5)
  A(31) = cont_VV(wf(:,6),wf(:,53)) * den(8)
  A(32) = cont_VV(wf(:,8),wf(:,54)) * den(11)
  A(33) = cont_VV(wf(:,55),wf(:,56)) * den(56)
  A(34) = cont_VV(wf(:,3),wf(:,57)) * den(59)
  A(35) = cont_VV(wf(:,9),wf(:,58)) * den(61)
  A(36) = cont_VV(wf(:,7),wf(:,59)) * den(63)
  A(37) = cont_VV(wf(:,5),wf(:,60)) * den(5)
  A(38) = cont_QA(wf(:,61),wf(:,62)) * den(66)
  A(39) = cont_VV(wf(:,12),wf(:,59)) * den(15)
  A(40) = cont_QA(wf(:,18),wf(:,63)) * den(67)
  A(41) = cont_VV(wf(:,15),wf(:,59)) * den(18)
  A(42) = cont_QA(wf(:,18),wf(:,64)) * den(21)
  A(43) = cont_QA(wf(:,20),wf(:,66)) * den(24)
  A(44) = cont_VV(wf(:,6),wf(:,67)) * den(68)
  A(45) = cont_QA(wf(:,18),wf(:,68)) * den(21)
  A(46) = cont_QA(wf(:,21),wf(:,69)) * den(24)
  A(47) = cont_VV(wf(:,6),wf(:,72)) * den(18)
  A(48) = cont_QA(wf(:,74),wf(:,75)) * den(70)
  A(49) = cont_QA(wf(:,18),wf(:,76)) * den(71)
  A(50) = cont_VV(wf(:,9),wf(:,77)) * den(73)
  A(51) = cont_VV(wf(:,6),wf(:,78)) * den(74)
  A(52) = cont_QA(wf(:,62),wf(:,79)) * den(75)
  A(53) = cont_QA(wf(:,80),wf(:,81)) * den(77)
  A(54) = cont_VV(wf(:,6),wf(:,84)) * den(15)
  A(55) = cont_QA(wf(:,14),wf(:,87)) * den(80)
  A(56) = cont_QA(wf(:,62),wf(:,88)) * den(81)
  A(57) = cont_VV(wf(:,9),wf(:,89)) * den(83)
  A(58) = cont_VV(wf(:,4),wf(:,90)) * den(5)
  A(59) = cont_VV(wf(:,6),wf(:,91)) * den(8)
  A(60) = cont_VV(wf(:,9),wf(:,92)) * den(11)
  A(61) = cont_VV(wf(:,24),wf(:,58)) * den(28)
  A(62) = cont_QA(wf(:,93),wf(:,94)) * den(86)
  A(63) = cont_VV(wf(:,27),wf(:,58)) * den(31)
  A(64) = cont_QA(wf(:,30),wf(:,95)) * den(87)
  A(65) = cont_QA(wf(:,30),wf(:,96)) * den(34)
  A(66) = cont_QA(wf(:,32),wf(:,98)) * den(37)
  A(67) = cont_VV(wf(:,24),wf(:,77)) * den(88)
  A(68) = cont_VV(wf(:,27),wf(:,77)) * den(89)
  A(69) = cont_VV(wf(:,24),wf(:,89)) * den(90)
  A(70) = cont_VV(wf(:,27),wf(:,89)) * den(91)
  A(71) = cont_VV(wf(:,24),wf(:,92)) * den(28)
  A(72) = cont_VV(wf(:,27),wf(:,92)) * den(31)
  A(73) = cont_QA(wf(:,33),wf(:,99)) * den(37)
  A(74) = cont_QA(wf(:,29),wf(:,101)) * den(34)
  A(75) = cont_VV(wf(:,8),wf(:,102)) * den(92)
  A(76) = cont_QA(wf(:,30),wf(:,103)) * den(34)
  A(77) = cont_QA(wf(:,33),wf(:,104)) * den(37)
  A(78) = cont_VV(wf(:,8),wf(:,107)) * den(31)
  A(79) = cont_VV(wf(:,2),wf(:,110)) * den(95)
  A(80) = cont_QA(wf(:,30),wf(:,111)) * den(96)
  A(81) = cont_VV(wf(:,7),wf(:,112)) * den(98)
  A(82) = cont_VV(wf(:,12),wf(:,112)) * den(99)
  A(83) = cont_VV(wf(:,15),wf(:,112)) * den(100)
  A(84) = cont_VV(wf(:,8),wf(:,113)) * den(101)
  A(85) = cont_QA(wf(:,114),wf(:,115)) * den(103)
  A(86) = cont_QA(wf(:,94),wf(:,116)) * den(104)
  A(87) = cont_VV(wf(:,8),wf(:,119)) * den(28)
  A(88) = cont_VV(wf(:,2),wf(:,122)) * den(107)
  A(89) = cont_VV(wf(:,7),wf(:,123)) * den(109)
  A(90) = cont_QA(wf(:,94),wf(:,124)) * den(110)
  A(91) = cont_VV(wf(:,12),wf(:,123)) * den(111)
  A(92) = cont_VV(wf(:,15),wf(:,123)) * den(112)
  A(93) = cont_VV(wf(:,4),wf(:,125)) * den(5)
  A(94) = cont_VV(wf(:,7),wf(:,126)) * den(8)
  A(95) = cont_VV(wf(:,8),wf(:,127)) * den(11)
  A(96) = cont_VV(wf(:,12),wf(:,126)) * den(15)
  A(97) = cont_VV(wf(:,15),wf(:,126)) * den(18)
  A(98) = cont_QA(wf(:,17),wf(:,129)) * den(21)
  A(99) = cont_QA(wf(:,21),wf(:,130)) * den(24)
  A(100) = cont_VV(wf(:,56),wf(:,131)) * den(115)
  A(101) = cont_VV(wf(:,57),wf(:,132)) * den(118)
  A(102) = cont_VV(wf(:,5),wf(:,133)) * den(121)
  A(103) = cont_VV(wf(:,7),wf(:,134)) * den(123)
  A(104) = cont_VV(wf(:,8),wf(:,135)) * den(124)
  A(105) = cont_VV(wf(:,9),wf(:,136)) * den(126)
  A(106) = cont_VV(wf(:,6),wf(:,137)) * den(127)
  A(107) = cont_VV(wf(:,7),wf(:,138)) * den(128)
  A(108) = cont_VV(wf(:,9),wf(:,139)) * den(129)
  A(109) = cont_VV(wf(:,12),wf(:,134)) * den(130)
  A(110) = cont_VV(wf(:,6),wf(:,142)) * den(132)
  A(111) = cont_VV(wf(:,6),wf(:,143)) * den(133)
  A(112) = cont_VV(wf(:,15),wf(:,134)) * den(134)
  A(113) = cont_VV(wf(:,6),wf(:,146)) * den(136)
  A(114) = cont_VV(wf(:,6),wf(:,147)) * den(137)
  A(115) = cont_QA(wf(:,81),wf(:,148)) * den(138)
  A(116) = cont_QA(wf(:,21),wf(:,149)) * den(139)
  A(117) = cont_QA(wf(:,81),wf(:,150)) * den(140)
  A(118) = cont_QA(wf(:,62),wf(:,151)) * den(141)
  A(119) = cont_QA(wf(:,62),wf(:,152)) * den(143)
  A(120) = cont_QA(wf(:,18),wf(:,153)) * den(144)
  A(121) = cont_VV(wf(:,8),wf(:,156)) * den(146)
  A(122) = cont_VV(wf(:,8),wf(:,157)) * den(147)
  A(123) = cont_VV(wf(:,24),wf(:,136)) * den(148)
  A(124) = cont_VV(wf(:,8),wf(:,160)) * den(150)
  A(125) = cont_VV(wf(:,8),wf(:,161)) * den(151)
  A(126) = cont_VV(wf(:,27),wf(:,136)) * den(152)
  A(127) = cont_QA(wf(:,115),wf(:,162)) * den(153)
  A(128) = cont_QA(wf(:,94),wf(:,163)) * den(154)
  A(129) = cont_QA(wf(:,115),wf(:,164)) * den(155)
  A(130) = cont_QA(wf(:,94),wf(:,165)) * den(156)
  A(131) = cont_QA(wf(:,33),wf(:,166)) * den(157)
  A(132) = cont_QA(wf(:,30),wf(:,167)) * den(158)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(132)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((A(9)+A(10)+A(11)+A(12))*f(1))/6._/**/REALKIND
  M1(2) = ((-A(5)-A(8)-A(10)-A(11))*f(1))/2._/**/REALKIND+(CI*(-A(1)+A(2)+A(3)-A(4))*f(2))/2._/**/REALKIND
  M1(3) = ((-A(6)-A(7)-A(9)-A(12))*f(1))/2._/**/REALKIND+(CI*(A(1)-A(2)-A(3)+A(4))*f(2))/2._/**/REALKIND
  M1(4) = ((A(5)+A(6)+A(7)+A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((A(61)+A(63)+A(65)+A(66)-A(121)-A(122)-A(123)-A(124)-A(125)-A(126)-A(127)-A(128)-A(129)-A(130)-A(131) &
       -A(132))*f(3))/6._/**/REALKIND+((A(71)+A(72)+A(73)+A(74)+A(75)+A(76)+A(77)+A(78)+A(84)+A(85)+A(86) &
       +A(87))*f(5))/6._/**/REALKIND+((-A(20)-A(21)-A(22)-A(23)-A(25)-A(26))*f(9))/6._/**/REALKIND+((-A(62)-A(64)-A(67)-A(68) &
       -A(69)-A(70)-A(79)-A(80)-A(88)-A(90))*f(11))/6._/**/REALKIND
  M2(2) = ((-A(39)-A(43)-A(63)-A(65)+A(109)+A(110)+A(111)+A(116)+A(118)+A(119)+A(124)+A(125)+A(126)+A(127)+A(129) &
       +A(132))*f(3))/2._/**/REALKIND+(CI*(-A(33)+A(34)-A(35)+A(36)+A(37)-A(100)+A(101)-A(102)-A(103)+A(104)+A(105)-A(106)-A(107) &
       +A(108))*f(4))/2._/**/REALKIND+((-A(44)-A(46)-A(52)-A(54)-A(72)-A(74)-A(76)-A(78)-A(84)-A(85)-A(96) &
       -A(99))*f(5))/2._/**/REALKIND+(CI*(-A(16)-A(24)+A(58)+A(59)-A(60)+A(93)+A(94)-A(95))*f(6))/2._/**/REALKIND &
       -(CI*A(13)*f(7))/2._/**/REALKIND+(CI*(A(30)+A(31)-A(32))*f(8))/2._/**/REALKIND+((A(14)+A(18)+A(21)+A(23)+A(25) &
       +A(28))*f(9))/2._/**/REALKIND+(CI*(A(19)-A(27))*f(10))/2._/**/REALKIND+((A(38)+A(48)+A(56)+A(64)+A(68)+A(70)+A(80)+A(82) &
       +A(88)+A(91))*f(11))/2._/**/REALKIND+(CI*(A(50)+A(57)-A(81)-A(89))*f(12))/2._/**/REALKIND
  M2(3) = ((-A(41)-A(42)-A(61)-A(66)+A(112)+A(113)+A(114)+A(115)+A(117)+A(120)+A(121)+A(122)+A(123)+A(128)+A(130) &
       +A(131))*f(3))/2._/**/REALKIND+(CI*(A(33)-A(34)+A(35)-A(36)-A(37)+A(100)-A(101)+A(102)+A(103)-A(104)-A(105)+A(106)+A(107) &
       -A(108))*f(4))/2._/**/REALKIND+((-A(45)-A(47)-A(51)-A(53)-A(71)-A(73)-A(75)-A(77)-A(86)-A(87)-A(97) &
       -A(98))*f(5))/2._/**/REALKIND+(CI*(A(16)+A(24)-A(58)-A(59)+A(60)-A(93)-A(94)+A(95))*f(6))/2._/**/REALKIND &
       +(CI*A(13)*f(7))/2._/**/REALKIND+(CI*(-A(30)-A(31)+A(32))*f(8))/2._/**/REALKIND+((A(15)+A(17)+A(20)+A(22)+A(26) &
       +A(29))*f(9))/2._/**/REALKIND+(CI*(-A(19)+A(27))*f(10))/2._/**/REALKIND+((A(40)+A(49)+A(55)+A(62)+A(67)+A(69)+A(79)+A(83) &
       +A(90)+A(92))*f(11))/2._/**/REALKIND+(CI*(-A(50)-A(57)+A(81)+A(89))*f(12))/2._/**/REALKIND
  M2(4) = ((A(39)+A(41)+A(42)+A(43)-A(109)-A(110)-A(111)-A(112)-A(113)-A(114)-A(115)-A(116)-A(117)-A(118)-A(119) &
       -A(120))*f(3))/6._/**/REALKIND+((A(44)+A(45)+A(46)+A(47)+A(51)+A(52)+A(53)+A(54)+A(96)+A(97)+A(98) &
       +A(99))*f(5))/6._/**/REALKIND+((-A(14)-A(15)-A(17)-A(18)-A(28)-A(29))*f(9))/6._/**/REALKIND+((-A(38)-A(40)-A(48)-A(49) &
       -A(55)-A(56)-A(82)-A(83)-A(91)-A(92))*f(11))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_heftpphjjj_uuxddxhg_1_/**/REALKIND
