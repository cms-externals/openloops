
module ol_colourmatrix_heftpphjjj_dddxdxhg_1_/**/REALKIND
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
  K1( 9,:) = [   0,  16,  -2,   6]
  K1(10,:) = [  16,   0,   6,  -2]
  K1(11,:) = [  -2,   6,   0,  16]
  K1(12,:) = [   6,  -2,  16,   0]
  K1(13,:) = [  48,  16,  16,   0]
  K1(14,:) = [  16,  48,   0,  16]
  K1(15,:) = [  16,   0,  48,  16]
  K1(16,:) = [   0,  16,  16,  48]
  K1(17,:) = [   6,   2,   2,   0]
  K1(18,:) = [   2,   0,  -6, -16]
  K1(19,:) = [   2,  -6,   0, -16]
  K1(20,:) = [   0, -16, -16, -48]
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
  K1(33,:) = [ -48, -16, -16,   0]
  K1(34,:) = [ -16,   0,  -6,   2]
  K1(35,:) = [ -16,  -6,   0,   2]
  K1(36,:) = [   0,   2,   2,   6]
  K1(37,:) = [   0,  -2,  16,   6]
  K1(38,:) = [  -2,   0,   6,  16]
  K1(39,:) = [  16,   6,   0,  -2]
  K1(40,:) = [   6,  16,  -2,   0]
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
  K1(69,:) = [   0, -18,  18,   0]
  K1(70,:) = [ -18, -54,   0, -18]
  K1(71,:) = [  18,   0,   0, -18]
  K1(72,:) = [   0, -18, -18, -54]
  K1(73,:) = [ -54, -18, -18,   0]
  K1(74,:) = [ -18, -54,   0, -18]
  K1(75,:) = [ -18,   0,   0,  18]
  K1(76,:) = [   0, -18,  18,   0]
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
end module ol_colourmatrix_heftpphjjj_dddxdxhg_1_/**/REALKIND



module ol_forced_parameters_heftpphjjj_dddxdxhg_1_/**/REALKIND
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
end module ol_forced_parameters_heftpphjjj_dddxdxhg_1_/**/REALKIND

module ol_loop_heftpphjjj_dddxdxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(22), c(29)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:383)
  ! denominators
  complex(REALKIND), save :: den(421)
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
  complex(REALKIND), intent(out) :: M1(4), M2(4)
  complex(REALKIND) :: A(264)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_HGG_G(wf(:,-4),wf(:,1),Q(:,5),wf(:,-5),Q(:,32),wf(:,3),Q(:,53))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,4))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,5))
  call vert_HGG_G(wf(:,-4),wf(:,4),Q(:,9),wf(:,-5),Q(:,32),wf(:,6),Q(:,57))
  call vert_HG_G(wf(:,-4),wf(:,-5),Q(:,32),wf(:,7),Q(:,48))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,2),Q(:,10),wf(:,8))
  call vert_HG_G(wf(:,-4),wf(:,1),Q(:,5),wf(:,9),Q(:,21))
  call vert_UV_W(wf(:,2),Q(:,10),wf(:,-5),Q(:,32),wf(:,10))
  call vert_HG_G(wf(:,-4),wf(:,2),Q(:,10),wf(:,11),Q(:,26))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,-5),Q(:,32),wf(:,12))
  call vert_VQ_A(wf(:,-5),wf(:,-1),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,34),ZERO,0_intkind1,wf(:,14))
  call vert_QA_V(wf(:,14),wf(:,-3),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,40),ZERO,0_intkind1,wf(:,17))
  call vert_QA_V(wf(:,-1),wf(:,17),wf(:,18))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,19))
  call vert_AV_Q(wf(:,-3),wf(:,7),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,7),ZERO,0_intkind1,wf(:,21))
  call vert_VQ_A(wf(:,7),wf(:,-1),wf(:,22))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,50),ZERO,0_intkind1,wf(:,24))
  call vert_UV_W(wf(:,5),Q(:,6),wf(:,4),Q(:,9),wf(:,25))
  call vert_HG_G(wf(:,-4),wf(:,4),Q(:,9),wf(:,26),Q(:,25))
  call vert_UV_W(wf(:,5),Q(:,6),wf(:,-5),Q(:,32),wf(:,27))
  call vert_HG_G(wf(:,-4),wf(:,5),Q(:,6),wf(:,28),Q(:,22))
  call vert_UV_W(wf(:,4),Q(:,9),wf(:,-5),Q(:,32),wf(:,29))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,33),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,31),wf(:,-3),wf(:,32))
  call vert_QA_V(wf(:,0),wf(:,17),wf(:,33))
  call vert_VQ_A(wf(:,5),wf(:,0),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,7),ZERO,0_intkind1,wf(:,35))
  call vert_VQ_A(wf(:,7),wf(:,0),wf(:,36))
  call vert_AV_Q(wf(:,-3),wf(:,5),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,49),ZERO,0_intkind1,wf(:,38))
  call vert_QA_V(wf(:,14),wf(:,-2),wf(:,39))
  call vert_AV_Q(wf(:,-2),wf(:,-5),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,36),ZERO,0_intkind1,wf(:,41))
  call vert_QA_V(wf(:,-1),wf(:,41),wf(:,42))
  call vert_VQ_A(wf(:,4),wf(:,-1),wf(:,43))
  call vert_AV_Q(wf(:,-2),wf(:,7),wf(:,44))
  call prop_Q_A(wf(:,43),Q(:,11),ZERO,0_intkind1,wf(:,45))
  call vert_AV_Q(wf(:,-2),wf(:,4),wf(:,46))
  call vert_QA_V(wf(:,31),wf(:,-2),wf(:,47))
  call vert_QA_V(wf(:,0),wf(:,41),wf(:,48))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,11),ZERO,0_intkind1,wf(:,50))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,51))
  call counter_HGG_G(wf(:,-4),wf(:,1),Q(:,5),wf(:,-5),Q(:,32),wf(:,52),Q(:,53))
  call counter_AVH_Q(wf(:,-3),wf(:,1),wf(:,-4),wf(:,53))
  call counter_AVH_Q(wf(:,-3),wf(:,-5),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,56),ZERO,0_intkind1,wf(:,55))
  call counter_HGG_G(wf(:,-4),wf(:,4),Q(:,9),wf(:,-5),Q(:,32),wf(:,56),Q(:,57))
  call counter_HQA_V(wf(:,-4),wf(:,31),wf(:,-3),wf(:,57))
  call counter_AVH_Q(wf(:,-2),wf(:,4),wf(:,-4),wf(:,58))
  call counter_AVH_Q(wf(:,-2),wf(:,-5),wf(:,-4),wf(:,59))
  call prop_A_Q(wf(:,59),Q(:,52),ZERO,0_intkind1,wf(:,60))
  call counter_HQA_V(wf(:,-4),wf(:,31),wf(:,-2),wf(:,61))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,62))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,63))
  call counter_VHQ_A(wf(:,1),wf(:,-4),wf(:,-1),wf(:,64))
  call counter_VHQ_A(wf(:,-5),wf(:,-4),wf(:,-1),wf(:,65))
  call prop_Q_A(wf(:,65),Q(:,50),ZERO,0_intkind1,wf(:,66))
  call counter_HQA_V(wf(:,-4),wf(:,-1),wf(:,-3),wf(:,67))
  call counter_VHQ_A(wf(:,4),wf(:,-4),wf(:,-1),wf(:,68))
  call counter_HQA_V(wf(:,-4),wf(:,-1),wf(:,-2),wf(:,69))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,70))
  call vert_HGG_G(wf(:,-4),wf(:,70),Q(:,9),wf(:,-5),Q(:,32),wf(:,71),Q(:,57))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,72))
  call vert_HGG_G(wf(:,-4),wf(:,72),Q(:,5),wf(:,-5),Q(:,32),wf(:,73),Q(:,53))
  call counter_VHQ_A(wf(:,5),wf(:,-4),wf(:,0),wf(:,74))
  call counter_VHQ_A(wf(:,-5),wf(:,-4),wf(:,0),wf(:,75))
  call prop_Q_A(wf(:,75),Q(:,49),ZERO,0_intkind1,wf(:,76))
  call counter_HQA_V(wf(:,-4),wf(:,0),wf(:,-3),wf(:,77))
  call counter_VHQ_A(wf(:,2),wf(:,-4),wf(:,0),wf(:,78))
  call counter_HQA_V(wf(:,-4),wf(:,0),wf(:,-2),wf(:,79))
  call counter_UV_W(wf(:,1),Q(:,5),wf(:,2),Q(:,10),wf(:,80))
  call counter_UV_W(wf(:,2),Q(:,10),wf(:,-5),Q(:,32),wf(:,81))
  call counter_UV_W(wf(:,1),Q(:,5),wf(:,-5),Q(:,32),wf(:,82))
  call vert_HGG_G(wf(:,-4),wf(:,2),Q(:,10),wf(:,-5),Q(:,32),wf(:,83),Q(:,58))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,84))
  call counter_V_V(ctGG,wf(:,2),Q(:,10),wf(:,85))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,2),Q(:,10),wf(:,86),Q(:,26))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,1),Q(:,5),wf(:,87),Q(:,21))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,-5),Q(:,32),wf(:,88),Q(:,48))
  call counter_QH_A(wf(:,14),Q(:,34),wf(:,-4),wf(:,89),Q(:,50))
  call prop_A_Q(wf(:,23),Q(:,13),ZERO,0_intkind1,wf(:,90))
  call counter_HA_Q(wf(:,-4),wf(:,17),Q(:,40),wf(:,91),Q(:,56))
  call vert_AV_Q(wf(:,-3),wf(:,88),wf(:,92))
  call vert_VQ_A(wf(:,88),wf(:,-1),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,50),ZERO,0_intkind1,wf(:,94))
  call counter_QA_V(wf(:,14),wf(:,-3),wf(:,95))
  call counter_AV_Q(wf(:,-3),wf(:,7),wf(:,96))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,97))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,98))
  call prop_A_Q(wf(:,98),Q(:,40),ZERO,0_intkind1,wf(:,99))
  call vert_QA_V(wf(:,-1),wf(:,99),wf(:,100))
  call counter_HA_Q(wf(:,-4),wf(:,-3),Q(:,8),wf(:,101),Q(:,24))
  call prop_A_Q(wf(:,101),Q(:,24),ZERO,0_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,1),wf(:,14),wf(:,103))
  call vert_AV_Q(wf(:,102),wf(:,-5),wf(:,104))
  call vert_QA_V(wf(:,-1),wf(:,102),wf(:,105))
  call counter_UV_W(wf(:,5),Q(:,6),wf(:,4),Q(:,9),wf(:,106))
  call counter_UV_W(wf(:,5),Q(:,6),wf(:,-5),Q(:,32),wf(:,107))
  call counter_UV_W(wf(:,4),Q(:,9),wf(:,-5),Q(:,32),wf(:,108))
  call vert_HGG_G(wf(:,-4),wf(:,5),Q(:,6),wf(:,-5),Q(:,32),wf(:,109),Q(:,54))
  call counter_V_V(ctGG,wf(:,4),Q(:,9),wf(:,110))
  call counter_V_V(ctGG,wf(:,5),Q(:,6),wf(:,111))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,5),Q(:,6),wf(:,112),Q(:,22))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,4),Q(:,9),wf(:,113),Q(:,25))
  call counter_QH_A(wf(:,31),Q(:,33),wf(:,-4),wf(:,114),Q(:,49))
  call prop_A_Q(wf(:,37),Q(:,14),ZERO,0_intkind1,wf(:,115))
  call vert_VQ_A(wf(:,88),wf(:,0),wf(:,116))
  call prop_Q_A(wf(:,116),Q(:,49),ZERO,0_intkind1,wf(:,117))
  call counter_QA_V(wf(:,31),wf(:,-3),wf(:,118))
  call counter_AV_Q(wf(:,-3),wf(:,5),wf(:,119))
  call vert_QA_V(wf(:,0),wf(:,99),wf(:,120))
  call vert_VQ_A(wf(:,5),wf(:,31),wf(:,121))
  call vert_QA_V(wf(:,0),wf(:,102),wf(:,122))
  call prop_A_Q(wf(:,46),Q(:,13),ZERO,0_intkind1,wf(:,123))
  call counter_HA_Q(wf(:,-4),wf(:,41),Q(:,36),wf(:,124),Q(:,52))
  call vert_AV_Q(wf(:,-2),wf(:,88),wf(:,125))
  call prop_A_Q(wf(:,51),Q(:,14),ZERO,0_intkind1,wf(:,126))
  call counter_QA_V(wf(:,14),wf(:,-2),wf(:,127))
  call counter_AV_Q(wf(:,-2),wf(:,7),wf(:,128))
  call counter_AV_Q(wf(:,-2),wf(:,4),wf(:,129))
  call counter_AV_Q(wf(:,-2),wf(:,-5),wf(:,130))
  call prop_A_Q(wf(:,130),Q(:,36),ZERO,0_intkind1,wf(:,131))
  call vert_QA_V(wf(:,-1),wf(:,131),wf(:,132))
  call counter_HA_Q(wf(:,-4),wf(:,-2),Q(:,4),wf(:,133),Q(:,20))
  call prop_A_Q(wf(:,133),Q(:,20),ZERO,0_intkind1,wf(:,134))
  call vert_VQ_A(wf(:,4),wf(:,14),wf(:,135))
  call vert_AV_Q(wf(:,134),wf(:,-5),wf(:,136))
  call vert_QA_V(wf(:,-1),wf(:,134),wf(:,137))
  call counter_QA_V(wf(:,31),wf(:,-2),wf(:,138))
  call counter_AV_Q(wf(:,-2),wf(:,2),wf(:,139))
  call vert_QA_V(wf(:,0),wf(:,131),wf(:,140))
  call vert_VQ_A(wf(:,2),wf(:,31),wf(:,141))
  call vert_QA_V(wf(:,0),wf(:,134),wf(:,142))
  call counter_QA_V(wf(:,-1),wf(:,17),wf(:,143))
  call counter_VQ_A(wf(:,7),wf(:,-1),wf(:,144))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,145))
  call prop_A_Q(wf(:,20),Q(:,56),ZERO,0_intkind1,wf(:,146))
  call counter_VQ_A(wf(:,-5),wf(:,-1),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,34),ZERO,0_intkind1,wf(:,148))
  call vert_QA_V(wf(:,148),wf(:,-3),wf(:,149))
  call counter_QH_A(wf(:,-1),Q(:,2),wf(:,-4),wf(:,150),Q(:,18))
  call prop_Q_A(wf(:,150),Q(:,18),ZERO,0_intkind1,wf(:,151))
  call vert_VQ_A(wf(:,1),wf(:,151),wf(:,152))
  call vert_VQ_A(wf(:,-5),wf(:,151),wf(:,153))
  call vert_QA_V(wf(:,151),wf(:,-3),wf(:,154))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,62),Q(:,10),wf(:,155))
  call vert_UV_W(wf(:,62),Q(:,10),wf(:,-5),Q(:,32),wf(:,156))
  call vert_HG_G(wf(:,-4),wf(:,62),Q(:,10),wf(:,157),Q(:,26))
  call counter_QA_V(wf(:,-1),wf(:,41),wf(:,158))
  call counter_VQ_A(wf(:,4),wf(:,-1),wf(:,159))
  call prop_A_Q(wf(:,44),Q(:,52),ZERO,0_intkind1,wf(:,160))
  call vert_QA_V(wf(:,148),wf(:,-2),wf(:,161))
  call vert_VQ_A(wf(:,4),wf(:,151),wf(:,162))
  call vert_QA_V(wf(:,151),wf(:,-2),wf(:,163))
  call vert_AV_Q(wf(:,-2),wf(:,62),wf(:,164))
  call vert_VQ_A(wf(:,62),wf(:,0),wf(:,165))
  call prop_Q_A(wf(:,165),Q(:,11),ZERO,0_intkind1,wf(:,166))
  call vert_UV_W(wf(:,63),Q(:,6),wf(:,4),Q(:,9),wf(:,167))
  call vert_UV_W(wf(:,63),Q(:,6),wf(:,-5),Q(:,32),wf(:,168))
  call vert_HG_G(wf(:,-4),wf(:,63),Q(:,6),wf(:,169),Q(:,22))
  call vert_AV_Q(wf(:,-3),wf(:,63),wf(:,170))
  call vert_VQ_A(wf(:,63),wf(:,0),wf(:,171))
  call prop_Q_A(wf(:,171),Q(:,7),ZERO,0_intkind1,wf(:,172))
  call counter_QA_V(wf(:,0),wf(:,17),wf(:,173))
  call counter_VQ_A(wf(:,5),wf(:,0),wf(:,174))
  call counter_VQ_A(wf(:,7),wf(:,0),wf(:,175))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,176))
  call prop_Q_A(wf(:,176),Q(:,33),ZERO,0_intkind1,wf(:,177))
  call vert_QA_V(wf(:,177),wf(:,-3),wf(:,178))
  call counter_QH_A(wf(:,0),Q(:,1),wf(:,-4),wf(:,179),Q(:,17))
  call prop_Q_A(wf(:,179),Q(:,17),ZERO,0_intkind1,wf(:,180))
  call vert_VQ_A(wf(:,5),wf(:,180),wf(:,181))
  call vert_QA_V(wf(:,180),wf(:,-3),wf(:,182))
  call vert_VQ_A(wf(:,-5),wf(:,180),wf(:,183))
  call vert_UV_W(wf(:,5),Q(:,6),wf(:,70),Q(:,9),wf(:,184))
  call vert_HG_G(wf(:,-4),wf(:,70),Q(:,9),wf(:,185),Q(:,25))
  call vert_UV_W(wf(:,70),Q(:,9),wf(:,-5),Q(:,32),wf(:,186))
  call counter_QA_V(wf(:,0),wf(:,41),wf(:,187))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,188))
  call vert_QA_V(wf(:,177),wf(:,-2),wf(:,189))
  call vert_VQ_A(wf(:,2),wf(:,180),wf(:,190))
  call vert_QA_V(wf(:,180),wf(:,-2),wf(:,191))
  call vert_VQ_A(wf(:,70),wf(:,-1),wf(:,192))
  call prop_Q_A(wf(:,192),Q(:,11),ZERO,0_intkind1,wf(:,193))
  call vert_AV_Q(wf(:,-2),wf(:,70),wf(:,194))
  call vert_UV_W(wf(:,72),Q(:,5),wf(:,2),Q(:,10),wf(:,195))
  call vert_HG_G(wf(:,-4),wf(:,72),Q(:,5),wf(:,196),Q(:,21))
  call vert_UV_W(wf(:,72),Q(:,5),wf(:,-5),Q(:,32),wf(:,197))
  call vert_VQ_A(wf(:,72),wf(:,-1),wf(:,198))
  call prop_Q_A(wf(:,198),Q(:,7),ZERO,0_intkind1,wf(:,199))
  call vert_AV_Q(wf(:,-3),wf(:,72),wf(:,200))
  call vert_UV_W(wf(:,2),Q(:,10),wf(:,7),Q(:,48),wf(:,201))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,7),Q(:,48),wf(:,202))
  call counter_V_V(ctGG,wf(:,7),Q(:,48),wf(:,203))
  call vert_HG_G(wf(:,-4),wf(:,84),Q(:,5),wf(:,204),Q(:,21))
  call vert_UV_W(wf(:,84),Q(:,5),wf(:,-5),Q(:,32),wf(:,205))
  call vert_HG_G(wf(:,-4),wf(:,85),Q(:,10),wf(:,206),Q(:,26))
  call vert_UV_W(wf(:,85),Q(:,10),wf(:,-5),Q(:,32),wf(:,207))
  call counter_V_V(ctGG,wf(:,9),Q(:,21),wf(:,208))
  call counter_V_V(ctGG,wf(:,11),Q(:,26),wf(:,209))
  call counter_Q_A(ctqq,wf(:,14),Q(:,34),wf(:,210))
  call prop_Q_A(wf(:,210),Q(:,34),ZERO,0_intkind1,wf(:,211))
  call vert_QA_V(wf(:,211),wf(:,-3),wf(:,212))
  call counter_V_V(ctGG,wf(:,15),Q(:,42),wf(:,213))
  call counter_A_Q(ctqq,wf(:,17),Q(:,40),wf(:,214))
  call prop_A_Q(wf(:,214),Q(:,40),ZERO,0_intkind1,wf(:,215))
  call vert_QA_V(wf(:,-1),wf(:,215),wf(:,216))
  call counter_V_V(ctGG,wf(:,18),Q(:,42),wf(:,217))
  call vert_VQ_A(wf(:,84),wf(:,-1),wf(:,218))
  call vert_AV_Q(wf(:,-3),wf(:,84),wf(:,219))
  call counter_Q_A(ctqq,wf(:,21),Q(:,7),wf(:,220))
  call counter_Q_A(ctqq,wf(:,24),Q(:,50),wf(:,221))
  call vert_VQ_A(wf(:,203),wf(:,-1),wf(:,222))
  call vert_AV_Q(wf(:,-3),wf(:,203),wf(:,223))
  call vert_UV_W(wf(:,5),Q(:,6),wf(:,7),Q(:,48),wf(:,224))
  call vert_UV_W(wf(:,4),Q(:,9),wf(:,7),Q(:,48),wf(:,225))
  call vert_HG_G(wf(:,-4),wf(:,110),Q(:,9),wf(:,226),Q(:,25))
  call vert_UV_W(wf(:,110),Q(:,9),wf(:,-5),Q(:,32),wf(:,227))
  call vert_HG_G(wf(:,-4),wf(:,111),Q(:,6),wf(:,228),Q(:,22))
  call vert_UV_W(wf(:,111),Q(:,6),wf(:,-5),Q(:,32),wf(:,229))
  call counter_V_V(ctGG,wf(:,26),Q(:,25),wf(:,230))
  call counter_V_V(ctGG,wf(:,28),Q(:,22),wf(:,231))
  call counter_Q_A(ctqq,wf(:,31),Q(:,33),wf(:,232))
  call prop_Q_A(wf(:,232),Q(:,33),ZERO,0_intkind1,wf(:,233))
  call vert_QA_V(wf(:,233),wf(:,-3),wf(:,234))
  call counter_V_V(ctGG,wf(:,32),Q(:,41),wf(:,235))
  call vert_QA_V(wf(:,0),wf(:,215),wf(:,236))
  call counter_V_V(ctGG,wf(:,33),Q(:,41),wf(:,237))
  call vert_VQ_A(wf(:,111),wf(:,0),wf(:,238))
  call counter_Q_A(ctqq,wf(:,35),Q(:,7),wf(:,239))
  call counter_Q_A(ctqq,wf(:,38),Q(:,49),wf(:,240))
  call vert_VQ_A(wf(:,203),wf(:,0),wf(:,241))
  call vert_AV_Q(wf(:,-3),wf(:,111),wf(:,242))
  call vert_QA_V(wf(:,211),wf(:,-2),wf(:,243))
  call counter_V_V(ctGG,wf(:,39),Q(:,38),wf(:,244))
  call counter_A_Q(ctqq,wf(:,41),Q(:,36),wf(:,245))
  call prop_A_Q(wf(:,245),Q(:,36),ZERO,0_intkind1,wf(:,246))
  call vert_QA_V(wf(:,-1),wf(:,246),wf(:,247))
  call counter_V_V(ctGG,wf(:,42),Q(:,38),wf(:,248))
  call vert_VQ_A(wf(:,110),wf(:,-1),wf(:,249))
  call vert_AV_Q(wf(:,-2),wf(:,110),wf(:,250))
  call counter_Q_A(ctqq,wf(:,45),Q(:,11),wf(:,251))
  call vert_AV_Q(wf(:,-2),wf(:,203),wf(:,252))
  call vert_QA_V(wf(:,233),wf(:,-2),wf(:,253))
  call counter_V_V(ctGG,wf(:,47),Q(:,37),wf(:,254))
  call vert_QA_V(wf(:,0),wf(:,246),wf(:,255))
  call counter_V_V(ctGG,wf(:,48),Q(:,37),wf(:,256))
  call vert_VQ_A(wf(:,85),wf(:,0),wf(:,257))
  call counter_Q_A(ctqq,wf(:,50),Q(:,11),wf(:,258))
  call vert_AV_Q(wf(:,-2),wf(:,85),wf(:,259))
  call prop_Q_A(wf(:,103),Q(:,39),ZERO,0_intkind1,wf(:,260))
  call vert_AV_Q(wf(:,17),wf(:,1),wf(:,261))
  call prop_A_Q(wf(:,261),Q(:,45),ZERO,0_intkind1,wf(:,262))
  call vert_QA_V(wf(:,21),wf(:,-3),wf(:,263))
  call vert_VQ_A(wf(:,-5),wf(:,21),wf(:,264))
  call prop_Q_A(wf(:,264),Q(:,39),ZERO,0_intkind1,wf(:,265))
  call vert_QA_V(wf(:,-1),wf(:,90),wf(:,266))
  call vert_AV_Q(wf(:,90),wf(:,-5),wf(:,267))
  call prop_A_Q(wf(:,267),Q(:,45),ZERO,0_intkind1,wf(:,268))
  call vert_VQ_A(wf(:,9),wf(:,-1),wf(:,269))
  call prop_Q_A(wf(:,269),Q(:,23),ZERO,0_intkind1,wf(:,270))
  call vert_AV_Q(wf(:,-3),wf(:,9),wf(:,271))
  call prop_A_Q(wf(:,271),Q(:,29),ZERO,0_intkind1,wf(:,272))
  call vert_UV_W(wf(:,9),Q(:,21),wf(:,-5),Q(:,32),wf(:,273))
  call vert_VQ_A(wf(:,12),wf(:,-1),wf(:,274))
  call prop_Q_A(wf(:,274),Q(:,39),ZERO,0_intkind1,wf(:,275))
  call vert_AV_Q(wf(:,-3),wf(:,12),wf(:,276))
  call prop_A_Q(wf(:,276),Q(:,45),ZERO,0_intkind1,wf(:,277))
  call vert_HG_G(wf(:,-4),wf(:,12),Q(:,37),wf(:,278),Q(:,53))
  call prop_Q_A(wf(:,121),Q(:,39),ZERO,0_intkind1,wf(:,279))
  call vert_AV_Q(wf(:,17),wf(:,5),wf(:,280))
  call prop_A_Q(wf(:,280),Q(:,46),ZERO,0_intkind1,wf(:,281))
  call vert_QA_V(wf(:,35),wf(:,-3),wf(:,282))
  call vert_VQ_A(wf(:,-5),wf(:,35),wf(:,283))
  call prop_Q_A(wf(:,283),Q(:,39),ZERO,0_intkind1,wf(:,284))
  call vert_QA_V(wf(:,0),wf(:,115),wf(:,285))
  call vert_VQ_A(wf(:,28),wf(:,0),wf(:,286))
  call prop_Q_A(wf(:,286),Q(:,23),ZERO,0_intkind1,wf(:,287))
  call vert_VQ_A(wf(:,27),wf(:,0),wf(:,288))
  call prop_Q_A(wf(:,288),Q(:,39),ZERO,0_intkind1,wf(:,289))
  call vert_AV_Q(wf(:,115),wf(:,-5),wf(:,290))
  call prop_A_Q(wf(:,290),Q(:,46),ZERO,0_intkind1,wf(:,291))
  call vert_AV_Q(wf(:,-3),wf(:,28),wf(:,292))
  call prop_A_Q(wf(:,292),Q(:,30),ZERO,0_intkind1,wf(:,293))
  call vert_UV_W(wf(:,28),Q(:,22),wf(:,-5),Q(:,32),wf(:,294))
  call vert_AV_Q(wf(:,-3),wf(:,27),wf(:,295))
  call prop_A_Q(wf(:,295),Q(:,46),ZERO,0_intkind1,wf(:,296))
  call vert_HG_G(wf(:,-4),wf(:,27),Q(:,38),wf(:,297),Q(:,54))
  call prop_Q_A(wf(:,135),Q(:,43),ZERO,0_intkind1,wf(:,298))
  call vert_AV_Q(wf(:,41),wf(:,4),wf(:,299))
  call prop_A_Q(wf(:,299),Q(:,45),ZERO,0_intkind1,wf(:,300))
  call vert_QA_V(wf(:,45),wf(:,-2),wf(:,301))
  call vert_VQ_A(wf(:,-5),wf(:,45),wf(:,302))
  call prop_Q_A(wf(:,302),Q(:,43),ZERO,0_intkind1,wf(:,303))
  call vert_QA_V(wf(:,-1),wf(:,123),wf(:,304))
  call vert_AV_Q(wf(:,123),wf(:,-5),wf(:,305))
  call prop_A_Q(wf(:,305),Q(:,45),ZERO,0_intkind1,wf(:,306))
  call vert_VQ_A(wf(:,26),wf(:,-1),wf(:,307))
  call prop_Q_A(wf(:,307),Q(:,27),ZERO,0_intkind1,wf(:,308))
  call vert_AV_Q(wf(:,-2),wf(:,26),wf(:,309))
  call prop_A_Q(wf(:,309),Q(:,29),ZERO,0_intkind1,wf(:,310))
  call vert_UV_W(wf(:,26),Q(:,25),wf(:,-5),Q(:,32),wf(:,311))
  call vert_VQ_A(wf(:,29),wf(:,-1),wf(:,312))
  call prop_Q_A(wf(:,312),Q(:,43),ZERO,0_intkind1,wf(:,313))
  call vert_AV_Q(wf(:,-2),wf(:,29),wf(:,314))
  call prop_A_Q(wf(:,314),Q(:,45),ZERO,0_intkind1,wf(:,315))
  call vert_HG_G(wf(:,-4),wf(:,29),Q(:,41),wf(:,316),Q(:,57))
  call prop_Q_A(wf(:,141),Q(:,43),ZERO,0_intkind1,wf(:,317))
  call vert_AV_Q(wf(:,41),wf(:,2),wf(:,318))
  call prop_A_Q(wf(:,318),Q(:,46),ZERO,0_intkind1,wf(:,319))
  call vert_QA_V(wf(:,50),wf(:,-2),wf(:,320))
  call vert_VQ_A(wf(:,-5),wf(:,50),wf(:,321))
  call prop_Q_A(wf(:,321),Q(:,43),ZERO,0_intkind1,wf(:,322))
  call vert_QA_V(wf(:,0),wf(:,126),wf(:,323))
  call vert_VQ_A(wf(:,11),wf(:,0),wf(:,324))
  call prop_Q_A(wf(:,324),Q(:,27),ZERO,0_intkind1,wf(:,325))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,326))
  call prop_Q_A(wf(:,326),Q(:,43),ZERO,0_intkind1,wf(:,327))
  call vert_AV_Q(wf(:,126),wf(:,-5),wf(:,328))
  call prop_A_Q(wf(:,328),Q(:,46),ZERO,0_intkind1,wf(:,329))
  call vert_AV_Q(wf(:,-2),wf(:,11),wf(:,330))
  call prop_A_Q(wf(:,330),Q(:,30),ZERO,0_intkind1,wf(:,331))
  call vert_UV_W(wf(:,11),Q(:,26),wf(:,-5),Q(:,32),wf(:,332))
  call vert_AV_Q(wf(:,-2),wf(:,10),wf(:,333))
  call prop_A_Q(wf(:,333),Q(:,46),ZERO,0_intkind1,wf(:,334))
  call vert_HG_G(wf(:,-4),wf(:,10),Q(:,42),wf(:,335),Q(:,58))
  call vert_VQ_A(wf(:,47),wf(:,-1),wf(:,336))
  call prop_Q_A(wf(:,336),Q(:,39),ZERO,0_intkind1,wf(:,337))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,338))
  call prop_A_Q(wf(:,338),Q(:,45),ZERO,0_intkind1,wf(:,339))
  call vert_HG_G(wf(:,-4),wf(:,47),Q(:,37),wf(:,340),Q(:,53))
  call vert_VQ_A(wf(:,32),wf(:,-1),wf(:,341))
  call prop_Q_A(wf(:,341),Q(:,43),ZERO,0_intkind1,wf(:,342))
  call vert_AV_Q(wf(:,-2),wf(:,32),wf(:,343))
  call prop_A_Q(wf(:,343),Q(:,45),ZERO,0_intkind1,wf(:,344))
  call vert_HG_G(wf(:,-4),wf(:,32),Q(:,41),wf(:,345),Q(:,57))
  call vert_VQ_A(wf(:,39),wf(:,0),wf(:,346))
  call prop_Q_A(wf(:,346),Q(:,39),ZERO,0_intkind1,wf(:,347))
  call vert_VQ_A(wf(:,15),wf(:,0),wf(:,348))
  call prop_Q_A(wf(:,348),Q(:,43),ZERO,0_intkind1,wf(:,349))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,350))
  call prop_A_Q(wf(:,350),Q(:,46),ZERO,0_intkind1,wf(:,351))
  call vert_HG_G(wf(:,-4),wf(:,39),Q(:,38),wf(:,352),Q(:,54))
  call vert_AV_Q(wf(:,-2),wf(:,15),wf(:,353))
  call prop_A_Q(wf(:,353),Q(:,46),ZERO,0_intkind1,wf(:,354))
  call vert_HG_G(wf(:,-4),wf(:,15),Q(:,42),wf(:,355),Q(:,58))
  call vert_VQ_A(wf(:,48),wf(:,-1),wf(:,356))
  call prop_Q_A(wf(:,356),Q(:,39),ZERO,0_intkind1,wf(:,357))
  call vert_VQ_A(wf(:,42),wf(:,0),wf(:,358))
  call prop_Q_A(wf(:,358),Q(:,39),ZERO,0_intkind1,wf(:,359))
  call vert_AV_Q(wf(:,-3),wf(:,48),wf(:,360))
  call prop_A_Q(wf(:,360),Q(:,45),ZERO,0_intkind1,wf(:,361))
  call vert_HG_G(wf(:,-4),wf(:,48),Q(:,37),wf(:,362),Q(:,53))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,363))
  call prop_A_Q(wf(:,363),Q(:,46),ZERO,0_intkind1,wf(:,364))
  call vert_HG_G(wf(:,-4),wf(:,42),Q(:,38),wf(:,365),Q(:,54))
  call vert_VQ_A(wf(:,33),wf(:,-1),wf(:,366))
  call prop_Q_A(wf(:,366),Q(:,43),ZERO,0_intkind1,wf(:,367))
  call vert_VQ_A(wf(:,18),wf(:,0),wf(:,368))
  call prop_Q_A(wf(:,368),Q(:,43),ZERO,0_intkind1,wf(:,369))
  call vert_AV_Q(wf(:,-2),wf(:,33),wf(:,370))
  call prop_A_Q(wf(:,370),Q(:,45),ZERO,0_intkind1,wf(:,371))
  call vert_HG_G(wf(:,-4),wf(:,33),Q(:,41),wf(:,372),Q(:,57))
  call vert_AV_Q(wf(:,-2),wf(:,18),wf(:,373))
  call prop_A_Q(wf(:,373),Q(:,46),ZERO,0_intkind1,wf(:,374))
  call vert_HG_G(wf(:,-4),wf(:,18),Q(:,42),wf(:,375),Q(:,58))
  call vert_QA_V(wf(:,38),wf(:,-2),wf(:,376))
  call vert_QA_V(wf(:,0),wf(:,160),wf(:,377))
  call vert_QA_V(wf(:,38),wf(:,-3),wf(:,378))
  call vert_QA_V(wf(:,0),wf(:,146),wf(:,379))
  call vert_QA_V(wf(:,24),wf(:,-2),wf(:,380))
  call vert_QA_V(wf(:,-1),wf(:,160),wf(:,381))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,382))
  call vert_QA_V(wf(:,-1),wf(:,146),wf(:,383))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(4) = 1 / (Q(5,9))
  den(5) = 1 / (Q(5,6))
  den(7) = 1 / (Q(5,48))
  den(9) = 1 / (Q(5,21))
  den(12) = 1 / (Q(5,26))
  den(15) = 1 / (Q(5,34))
  den(16) = 1 / (Q(5,42))
  den(19) = 1 / (Q(5,40))
  den(22) = 1 / (Q(5,7))
  den(25) = 1 / (Q(5,50))
  den(29) = 1 / (Q(5,25))
  den(32) = 1 / (Q(5,22))
  den(35) = 1 / (Q(5,33))
  den(36) = 1 / (Q(5,41))
  den(43) = 1 / (Q(5,49))
  den(46) = 1 / (Q(5,38))
  den(49) = 1 / (Q(5,36))
  den(52) = 1 / (Q(5,11))
  den(56) = 1 / (Q(5,37))
  den(65) = 1 / (Q(5,56))
  den(70) = 1 / (Q(5,52))
  den(94) = 1 / (Q(5,58))
  den(97) = 1 / (Q(5,53))
  den(104) = 1 / (Q(5,13))
  den(109) = 1 / (Q(5,24))
  den(114) = 1 / (Q(5,54))
  den(117) = 1 / (Q(5,57))
  den(124) = 1 / (Q(5,14))
  den(144) = 1 / (Q(5,20))
  den(162) = 1 / (Q(5,18))
  den(184) = 1 / (Q(5,17))
  den(208) = 1 / (Q(5,15))
  den(288) = 1 / (Q(5,39))
  den(290) = 1 / (Q(5,45))
  den(296) = 1 / (Q(5,23))
  den(298) = 1 / (Q(5,29))
  den(305) = 1 / (Q(5,46))
  den(313) = 1 / (Q(5,30))
  den(318) = 1 / (Q(5,43))
  den(325) = 1 / (Q(5,27))

  ! denominators
  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(17) = den(15)*den(16)
  den(18) = den(1)*den(17)
  den(20) = den(16)*den(19)
  den(21) = den(1)*den(20)
  den(23) = den(1)*den(22)
  den(24) = den(7)*den(23)
  den(26) = den(7)*den(25)
  den(27) = den(1)*den(26)
  den(28) = den(6)*den(7)
  den(30) = den(4)*den(29)
  den(31) = den(5)*den(30)
  den(33) = den(5)*den(32)
  den(34) = den(4)*den(33)
  den(37) = den(35)*den(36)
  den(38) = den(5)*den(37)
  den(39) = den(19)*den(36)
  den(40) = den(5)*den(39)
  den(41) = den(5)*den(22)
  den(42) = den(7)*den(41)
  den(44) = den(7)*den(43)
  den(45) = den(5)*den(44)
  den(47) = den(15)*den(46)
  den(48) = den(4)*den(47)
  den(50) = den(46)*den(49)
  den(51) = den(4)*den(50)
  den(53) = den(4)*den(52)
  den(54) = den(7)*den(53)
  den(55) = den(4)*den(26)
  den(57) = den(35)*den(56)
  den(58) = den(2)*den(57)
  den(59) = den(49)*den(56)
  den(60) = den(2)*den(59)
  den(61) = den(2)*den(52)
  den(62) = den(7)*den(61)
  den(63) = den(2)*den(44)
  den(64) = den(1)*den(15)
  den(66) = den(1)*den(65)
  den(67) = den(5)*den(35)
  den(68) = den(5)*den(65)
  den(69) = den(4)*den(15)
  den(71) = den(4)*den(70)
  den(72) = den(2)*den(35)
  den(73) = den(2)*den(70)
  den(74) = den(1)*den(19)
  den(75) = den(1)*den(25)
  den(76) = den(1)*den(12)
  den(77) = den(4)*den(49)
  den(78) = den(4)*den(25)
  den(79) = den(12)*den(35)
  den(80) = den(12)*den(49)
  den(81) = den(4)*den(32)
  den(82) = den(32)*den(35)
  den(83) = den(19)*den(32)
  den(84) = den(5)*den(19)
  den(85) = den(5)*den(43)
  den(86) = den(5)*den(29)
  den(87) = den(2)*den(49)
  den(88) = den(2)*den(43)
  den(89) = den(15)*den(29)
  den(90) = den(29)*den(49)
  den(91) = den(2)*den(9)
  den(92) = den(9)*den(15)
  den(93) = den(9)*den(19)
  den(95) = den(2)*den(94)
  den(96) = den(1)*den(95)
  den(98) = den(1)*den(97)
  den(99) = den(2)*den(98)
  den(100) = den(1)*den(56)
  den(101) = den(2)*den(100)
  den(102) = den(2)*den(16)
  den(103) = den(1)*den(102)
  den(105) = den(1)*den(104)
  den(106) = den(15)*den(105)
  den(107) = den(19)*den(23)
  den(108) = den(10)*den(15)
  den(110) = den(64)*den(109)
  den(111) = den(23)*den(109)
  den(112) = den(12)*den(109)
  den(113) = den(1)*den(112)
  den(115) = den(5)*den(114)
  den(116) = den(4)*den(115)
  den(118) = den(4)*den(117)
  den(119) = den(5)*den(118)
  den(120) = den(4)*den(36)
  den(121) = den(5)*den(120)
  den(122) = den(5)*den(46)
  den(123) = den(4)*den(122)
  den(125) = den(5)*den(124)
  den(126) = den(35)*den(125)
  den(127) = den(19)*den(41)
  den(128) = den(33)*den(35)
  den(129) = den(67)*den(109)
  den(130) = den(41)*den(109)
  den(131) = den(29)*den(109)
  den(132) = den(5)*den(131)
  den(133) = den(4)*den(104)
  den(134) = den(15)*den(133)
  den(135) = den(49)*den(53)
  den(136) = den(2)*den(124)
  den(137) = den(35)*den(136)
  den(138) = den(49)*den(61)
  den(139) = den(35)*den(112)
  den(140) = den(15)*den(131)
  den(141) = den(59)*den(109)
  den(142) = den(49)*den(131)
  den(143) = den(15)*den(30)
  den(145) = den(69)*den(144)
  den(146) = den(53)*den(144)
  den(147) = den(32)*den(144)
  den(148) = den(4)*den(147)
  den(149) = den(13)*den(35)
  den(150) = den(72)*den(144)
  den(151) = den(61)*den(144)
  den(152) = den(9)*den(144)
  den(153) = den(2)*den(152)
  den(154) = den(35)*den(147)
  den(155) = den(15)*den(152)
  den(156) = den(39)*den(144)
  den(157) = den(19)*den(152)
  den(158) = den(10)*den(19)
  den(159) = den(7)*den(105)
  den(160) = den(7)*den(65)
  den(161) = den(1)*den(160)
  den(163) = den(1)*den(162)
  den(164) = den(19)*den(163)
  den(165) = den(105)*den(162)
  den(166) = den(12)*den(162)
  den(167) = den(1)*den(166)
  den(168) = den(30)*den(49)
  den(169) = den(7)*den(133)
  den(170) = den(7)*den(70)
  den(171) = den(4)*den(170)
  den(172) = den(4)*den(162)
  den(173) = den(49)*den(172)
  den(174) = den(133)*den(162)
  den(175) = den(32)*den(162)
  den(176) = den(4)*den(175)
  den(177) = den(57)*den(162)
  den(178) = den(35)*den(175)
  den(179) = den(59)*den(162)
  den(180) = den(39)*den(162)
  den(181) = den(19)*den(33)
  den(182) = den(5)*den(160)
  den(183) = den(7)*den(125)
  den(185) = den(5)*den(184)
  den(186) = den(19)*den(185)
  den(187) = den(29)*den(184)
  den(188) = den(5)*den(187)
  den(189) = den(125)*den(184)
  den(190) = den(13)*den(49)
  den(191) = den(2)*den(170)
  den(192) = den(7)*den(136)
  den(193) = den(2)*den(184)
  den(194) = den(49)*den(193)
  den(195) = den(9)*den(184)
  den(196) = den(2)*den(195)
  den(197) = den(136)*den(184)
  den(198) = den(15)*den(195)
  den(199) = den(47)*den(184)
  den(200) = den(50)*den(184)
  den(201) = den(20)*den(184)
  den(202) = den(2)*den(7)
  den(203) = den(94)*den(202)
  den(204) = den(1)*den(203)
  den(205) = den(1)*den(7)
  den(206) = den(97)*den(205)
  den(207) = den(2)*den(206)
  den(209) = den(3)*den(208)
  den(210) = den(7)*den(209)
  den(211) = den(1)**2
  den(212) = den(102)*den(211)
  den(213) = den(13)*den(211)
  den(214) = den(2)**2
  den(215) = den(100)*den(214)
  den(216) = den(10)*den(214)
  den(217) = den(10)*den(102)
  den(218) = den(13)*den(100)
  den(219) = den(17)*den(211)
  den(220) = den(15)**2
  den(221) = den(10)*den(220)
  den(222) = den(10)*den(17)
  den(223) = den(20)*den(211)
  den(224) = den(19)**2
  den(225) = den(10)*den(224)
  den(226) = den(10)*den(20)
  den(227) = den(160)*den(211)
  den(228) = den(26)*den(211)
  den(229) = den(23)*den(160)
  den(230) = den(26)*den(105)
  den(231) = den(7)**2
  den(232) = den(105)*den(231)
  den(233) = den(23)*den(231)
  den(234) = den(5)*den(7)
  den(235) = den(114)*den(234)
  den(236) = den(4)*den(235)
  den(237) = den(4)*den(7)
  den(238) = den(117)*den(237)
  den(239) = den(5)*den(238)
  den(240) = den(6)*den(208)
  den(241) = den(7)*den(240)
  den(242) = den(4)**2
  den(243) = den(122)*den(242)
  den(244) = den(33)*den(242)
  den(245) = den(5)**2
  den(246) = den(120)*den(245)
  den(247) = den(30)*den(245)
  den(248) = den(30)*den(122)
  den(249) = den(33)*den(120)
  den(250) = den(35)**2
  den(251) = den(33)*den(250)
  den(252) = den(37)*den(245)
  den(253) = den(33)*den(37)
  den(254) = den(33)*den(224)
  den(255) = den(33)*den(39)
  den(256) = den(39)*den(245)
  den(257) = den(160)*den(245)
  den(258) = den(41)*den(160)
  den(259) = den(44)*den(125)
  den(260) = den(125)*den(231)
  den(261) = den(44)*den(245)
  den(262) = den(41)*den(231)
  den(263) = den(47)*den(242)
  den(264) = den(30)*den(220)
  den(265) = den(30)*den(47)
  den(266) = den(50)*den(242)
  den(267) = den(49)**2
  den(268) = den(30)*den(267)
  den(269) = den(30)*den(50)
  den(270) = den(170)*den(242)
  den(271) = den(26)*den(242)
  den(272) = den(53)*den(170)
  den(273) = den(26)*den(133)
  den(274) = den(133)*den(231)
  den(275) = den(53)*den(231)
  den(276) = den(13)*den(250)
  den(277) = den(57)*den(214)
  den(278) = den(13)*den(57)
  den(279) = den(13)*den(267)
  den(280) = den(13)*den(59)
  den(281) = den(59)*den(214)
  den(282) = den(170)*den(214)
  den(283) = den(61)*den(170)
  den(284) = den(44)*den(136)
  den(285) = den(136)*den(231)
  den(286) = den(44)*den(214)
  den(287) = den(61)*den(231)
  den(289) = den(64)*den(288)
  den(291) = den(74)*den(290)
  den(292) = den(23)*den(208)
  den(293) = den(23)*den(288)
  den(294) = den(105)*den(208)
  den(295) = den(105)*den(290)
  den(297) = den(10)*den(296)
  den(299) = den(10)*den(298)
  den(300) = den(10)*den(97)
  den(301) = den(100)*den(288)
  den(302) = den(100)*den(290)
  den(303) = den(97)*den(100)
  den(304) = den(67)*den(288)
  den(306) = den(84)*den(305)
  den(307) = den(41)*den(208)
  den(308) = den(41)*den(288)
  den(309) = den(125)*den(208)
  den(310) = den(33)*den(296)
  den(311) = den(122)*den(288)
  den(312) = den(125)*den(305)
  den(314) = den(33)*den(313)
  den(315) = den(33)*den(114)
  den(316) = den(122)*den(305)
  den(317) = den(114)*den(122)
  den(319) = den(69)*den(318)
  den(320) = den(77)*den(290)
  den(321) = den(53)*den(208)
  den(322) = den(53)*den(318)
  den(323) = den(133)*den(208)
  den(324) = den(133)*den(290)
  den(326) = den(30)*den(325)
  den(327) = den(30)*den(298)
  den(328) = den(30)*den(117)
  den(329) = den(120)*den(318)
  den(330) = den(120)*den(290)
  den(331) = den(117)*den(120)
  den(332) = den(72)*den(318)
  den(333) = den(87)*den(305)
  den(334) = den(61)*den(208)
  den(335) = den(61)*den(318)
  den(336) = den(136)*den(208)
  den(337) = den(13)*den(325)
  den(338) = den(102)*den(318)
  den(339) = den(136)*den(305)
  den(340) = den(13)*den(313)
  den(341) = den(13)*den(94)
  den(342) = den(102)*den(305)
  den(343) = den(94)*den(102)
  den(344) = den(57)*den(288)
  den(345) = den(57)*den(290)
  den(346) = den(57)*den(97)
  den(347) = den(37)*den(318)
  den(348) = den(37)*den(290)
  den(349) = den(37)*den(117)
  den(350) = den(47)*den(288)
  den(351) = den(17)*den(318)
  den(352) = den(47)*den(305)
  den(353) = den(47)*den(114)
  den(354) = den(17)*den(305)
  den(355) = den(17)*den(94)
  den(356) = den(59)*den(288)
  den(357) = den(50)*den(288)
  den(358) = den(59)*den(290)
  den(359) = den(59)*den(97)
  den(360) = den(50)*den(305)
  den(361) = den(50)*den(114)
  den(362) = den(39)*den(318)
  den(363) = den(20)*den(318)
  den(364) = den(39)*den(290)
  den(365) = den(39)*den(117)
  den(366) = den(20)*den(305)
  den(367) = den(20)*den(94)
  den(368) = den(44)*den(97)
  den(369) = den(97)*den(170)
  den(370) = den(44)*den(117)
  den(371) = den(117)*den(160)
  den(372) = den(26)*den(114)
  den(373) = den(114)*den(170)
  den(374) = den(26)*den(94)
  den(375) = den(94)*den(160)
  den(376) = den(1)*den(2)*den(7)
  den(377) = den(4)*den(5)*den(7)
  den(378) = den(1)*den(202)
  den(379) = den(2)*den(205)
  den(380) = den(4)*den(234)
  den(381) = den(5)*den(237)
  den(382) = den(2)*den(300)
  den(383) = den(2)*den(303)
  den(384) = den(1)*den(341)
  den(385) = den(1)*den(343)
  den(386) = den(15)*den(299)
  den(387) = den(1)*den(355)
  den(388) = den(19)*den(297)
  den(389) = den(1)*den(367)
  den(390) = den(7)*den(292)
  den(391) = den(7)*den(294)
  den(392) = den(1)*den(374)
  den(393) = den(1)*den(375)
  den(394) = den(5)*den(328)
  den(395) = den(5)*den(331)
  den(396) = den(4)*den(315)
  den(397) = den(4)*den(317)
  den(398) = den(5)*den(349)
  den(399) = den(35)*den(314)
  den(400) = den(5)*den(365)
  den(401) = den(19)*den(310)
  den(402) = den(7)*den(307)
  den(403) = den(5)*den(370)
  den(404) = den(7)*den(309)
  den(405) = den(5)*den(371)
  den(406) = den(15)*den(327)
  den(407) = den(4)*den(353)
  den(408) = den(49)*den(326)
  den(409) = den(4)*den(361)
  den(410) = den(7)*den(321)
  den(411) = den(7)*den(323)
  den(412) = den(4)*den(372)
  den(413) = den(4)*den(373)
  den(414) = den(2)*den(346)
  den(415) = den(35)*den(340)
  den(416) = den(2)*den(359)
  den(417) = den(49)*den(337)
  den(418) = den(7)*den(334)
  den(419) = den(2)*den(368)
  den(420) = den(7)*den(336)
  den(421) = den(2)*den(369)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(264)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,5),wf(:,6)) * den(6)
  A(3) = cont_VV(wf(:,7),wf(:,8)) * den(8)
  A(4) = cont_VV(wf(:,9),wf(:,10)) * den(11)
  A(5) = cont_VV(wf(:,11),wf(:,12)) * den(14)
  A(6) = cont_VV(wf(:,9),wf(:,15)) * den(18)
  A(7) = cont_VV(wf(:,9),wf(:,18)) * den(21)
  A(8) = cont_QA(wf(:,20),wf(:,21)) * den(24)
  A(9) = cont_QA(wf(:,23),wf(:,24)) * den(27)
  A(10) = cont_VV(wf(:,7),wf(:,25)) * den(28)
  A(11) = cont_VV(wf(:,26),wf(:,27)) * den(31)
  A(12) = cont_VV(wf(:,28),wf(:,29)) * den(34)
  A(13) = cont_VV(wf(:,28),wf(:,32)) * den(38)
  A(14) = cont_VV(wf(:,28),wf(:,33)) * den(40)
  A(15) = cont_QA(wf(:,20),wf(:,35)) * den(42)
  A(16) = cont_QA(wf(:,37),wf(:,38)) * den(45)
  A(17) = cont_VV(wf(:,26),wf(:,39)) * den(48)
  A(18) = cont_VV(wf(:,26),wf(:,42)) * den(51)
  A(19) = cont_QA(wf(:,44),wf(:,45)) * den(54)
  A(20) = cont_QA(wf(:,24),wf(:,46)) * den(55)
  A(21) = cont_VV(wf(:,11),wf(:,47)) * den(58)
  A(22) = cont_VV(wf(:,11),wf(:,48)) * den(60)
  A(23) = cont_QA(wf(:,44),wf(:,50)) * den(62)
  A(24) = cont_QA(wf(:,38),wf(:,51)) * den(63)

  A(25) = cont_VV(wf(:,2),wf(:,52)) * den(3)
  A(26) = cont_QA(wf(:,14),wf(:,53)) * den(64)
  A(27) = cont_QA(wf(:,19),wf(:,55)) * den(66)
  A(28) = cont_VV(wf(:,5),wf(:,56)) * den(6)
  A(29) = cont_VV(wf(:,5),wf(:,57)) * den(67)
  A(30) = cont_QA(wf(:,34),wf(:,55)) * den(68)
  A(31) = cont_QA(wf(:,14),wf(:,58)) * den(69)
  A(32) = cont_QA(wf(:,43),wf(:,60)) * den(71)
  A(33) = cont_VV(wf(:,2),wf(:,61)) * den(72)
  A(34) = cont_QA(wf(:,49),wf(:,60)) * den(73)
  A(35) = cont_VV(wf(:,3),wf(:,62)) * den(3)
  A(36) = cont_VV(wf(:,6),wf(:,63)) * den(6)
  A(37) = cont_QA(wf(:,17),wf(:,64)) * den(74)
  A(38) = cont_QA(wf(:,23),wf(:,66)) * den(75)
  A(39) = cont_VV(wf(:,12),wf(:,67)) * den(76)
  A(40) = cont_QA(wf(:,41),wf(:,68)) * den(77)
  A(41) = cont_QA(wf(:,46),wf(:,66)) * den(78)
  A(42) = cont_VV(wf(:,47),wf(:,67)) * den(79)
  A(43) = cont_VV(wf(:,48),wf(:,67)) * den(80)
  A(44) = cont_VV(wf(:,29),wf(:,69)) * den(81)
  A(45) = cont_VV(wf(:,32),wf(:,69)) * den(82)
  A(46) = cont_VV(wf(:,33),wf(:,69)) * den(83)
  A(47) = cont_VV(wf(:,5),wf(:,71)) * den(6)
  A(48) = cont_VV(wf(:,2),wf(:,73)) * den(3)
  A(49) = cont_QA(wf(:,17),wf(:,74)) * den(84)
  A(50) = cont_QA(wf(:,37),wf(:,76)) * den(85)
  A(51) = cont_VV(wf(:,27),wf(:,77)) * den(86)
  A(52) = cont_QA(wf(:,41),wf(:,78)) * den(87)
  A(53) = cont_QA(wf(:,51),wf(:,76)) * den(88)
  A(54) = cont_VV(wf(:,39),wf(:,77)) * den(89)
  A(55) = cont_VV(wf(:,42),wf(:,77)) * den(90)
  A(56) = cont_VV(wf(:,10),wf(:,79)) * den(91)
  A(57) = cont_VV(wf(:,15),wf(:,79)) * den(92)
  A(58) = cont_VV(wf(:,18),wf(:,79)) * den(93)
  A(59) = cont_VV(wf(:,7),wf(:,80)) * den(8)
  A(60) = cont_VV(wf(:,9),wf(:,81)) * den(11)
  A(61) = cont_VV(wf(:,11),wf(:,82)) * den(14)
  A(62) = cont_VV(wf(:,83),wf(:,84)) * den(96)
  A(63) = cont_VV(wf(:,3),wf(:,85)) * den(99)
  A(64) = cont_VV(wf(:,12),wf(:,86)) * den(101)
  A(65) = cont_VV(wf(:,10),wf(:,87)) * den(103)
  A(66) = cont_VV(wf(:,8),wf(:,88)) * den(8)
  A(67) = cont_QA(wf(:,89),wf(:,90)) * den(106)
  A(68) = cont_VV(wf(:,15),wf(:,87)) * den(18)
  A(69) = cont_QA(wf(:,21),wf(:,91)) * den(107)
  A(70) = cont_VV(wf(:,18),wf(:,87)) * den(21)
  A(71) = cont_QA(wf(:,21),wf(:,92)) * den(24)
  A(72) = cont_QA(wf(:,23),wf(:,94)) * den(27)
  A(73) = cont_VV(wf(:,9),wf(:,95)) * den(108)
  A(74) = cont_QA(wf(:,21),wf(:,96)) * den(24)
  A(75) = cont_QA(wf(:,24),wf(:,97)) * den(27)
  A(76) = cont_VV(wf(:,9),wf(:,100)) * den(21)
  A(77) = cont_QA(wf(:,102),wf(:,103)) * den(110)
  A(78) = cont_QA(wf(:,21),wf(:,104)) * den(111)
  A(79) = cont_VV(wf(:,12),wf(:,105)) * den(113)
  A(80) = cont_VV(wf(:,7),wf(:,106)) * den(28)
  A(81) = cont_VV(wf(:,26),wf(:,107)) * den(31)
  A(82) = cont_VV(wf(:,28),wf(:,108)) * den(34)
  A(83) = cont_VV(wf(:,109),wf(:,110)) * den(116)
  A(84) = cont_VV(wf(:,6),wf(:,111)) * den(119)
  A(85) = cont_VV(wf(:,29),wf(:,112)) * den(121)
  A(86) = cont_VV(wf(:,27),wf(:,113)) * den(123)
  A(87) = cont_VV(wf(:,25),wf(:,88)) * den(28)
  A(88) = cont_VV(wf(:,32),wf(:,112)) * den(38)
  A(89) = cont_QA(wf(:,114),wf(:,115)) * den(126)
  A(90) = cont_QA(wf(:,35),wf(:,91)) * den(127)
  A(91) = cont_VV(wf(:,33),wf(:,112)) * den(40)
  A(92) = cont_QA(wf(:,35),wf(:,92)) * den(42)
  A(93) = cont_QA(wf(:,37),wf(:,117)) * den(45)
  A(94) = cont_VV(wf(:,28),wf(:,118)) * den(128)
  A(95) = cont_QA(wf(:,35),wf(:,96)) * den(42)
  A(96) = cont_QA(wf(:,38),wf(:,119)) * den(45)
  A(97) = cont_VV(wf(:,28),wf(:,120)) * den(40)
  A(98) = cont_QA(wf(:,102),wf(:,121)) * den(129)
  A(99) = cont_QA(wf(:,35),wf(:,104)) * den(130)
  A(100) = cont_VV(wf(:,27),wf(:,122)) * den(132)
  A(101) = cont_QA(wf(:,89),wf(:,123)) * den(134)
  A(102) = cont_VV(wf(:,39),wf(:,113)) * den(48)
  A(103) = cont_QA(wf(:,45),wf(:,124)) * den(135)
  A(104) = cont_VV(wf(:,42),wf(:,113)) * den(51)
  A(105) = cont_QA(wf(:,45),wf(:,125)) * den(54)
  A(106) = cont_QA(wf(:,46),wf(:,94)) * den(55)
  A(107) = cont_VV(wf(:,47),wf(:,86)) * den(58)
  A(108) = cont_QA(wf(:,114),wf(:,126)) * den(137)
  A(109) = cont_QA(wf(:,50),wf(:,124)) * den(138)
  A(110) = cont_VV(wf(:,48),wf(:,86)) * den(60)
  A(111) = cont_QA(wf(:,50),wf(:,125)) * den(62)
  A(112) = cont_QA(wf(:,51),wf(:,117)) * den(63)
  A(113) = cont_VV(wf(:,47),wf(:,105)) * den(139)
  A(114) = cont_VV(wf(:,39),wf(:,122)) * den(140)
  A(115) = cont_VV(wf(:,48),wf(:,105)) * den(141)
  A(116) = cont_VV(wf(:,42),wf(:,122)) * den(142)
  A(117) = cont_VV(wf(:,26),wf(:,127)) * den(143)
  A(118) = cont_QA(wf(:,45),wf(:,128)) * den(54)
  A(119) = cont_QA(wf(:,24),wf(:,129)) * den(55)
  A(120) = cont_VV(wf(:,26),wf(:,132)) * den(51)
  A(121) = cont_QA(wf(:,134),wf(:,135)) * den(145)
  A(122) = cont_QA(wf(:,45),wf(:,136)) * den(146)
  A(123) = cont_VV(wf(:,29),wf(:,137)) * den(148)
  A(124) = cont_VV(wf(:,11),wf(:,138)) * den(149)
  A(125) = cont_QA(wf(:,50),wf(:,128)) * den(62)
  A(126) = cont_QA(wf(:,38),wf(:,139)) * den(63)
  A(127) = cont_VV(wf(:,11),wf(:,140)) * den(60)
  A(128) = cont_QA(wf(:,134),wf(:,141)) * den(150)
  A(129) = cont_QA(wf(:,50),wf(:,136)) * den(151)
  A(130) = cont_VV(wf(:,10),wf(:,142)) * den(153)
  A(131) = cont_VV(wf(:,32),wf(:,137)) * den(154)
  A(132) = cont_VV(wf(:,15),wf(:,142)) * den(155)
  A(133) = cont_VV(wf(:,33),wf(:,137)) * den(156)
  A(134) = cont_VV(wf(:,18),wf(:,142)) * den(157)
  A(135) = cont_VV(wf(:,9),wf(:,143)) * den(158)
  A(136) = cont_QA(wf(:,90),wf(:,144)) * den(159)
  A(137) = cont_QA(wf(:,145),wf(:,146)) * den(161)
  A(138) = cont_VV(wf(:,9),wf(:,149)) * den(18)
  A(139) = cont_QA(wf(:,17),wf(:,152)) * den(164)
  A(140) = cont_QA(wf(:,90),wf(:,153)) * den(165)
  A(141) = cont_VV(wf(:,12),wf(:,154)) * den(167)
  A(142) = cont_VV(wf(:,7),wf(:,155)) * den(8)
  A(143) = cont_VV(wf(:,9),wf(:,156)) * den(11)
  A(144) = cont_VV(wf(:,12),wf(:,157)) * den(14)
  A(145) = cont_VV(wf(:,26),wf(:,158)) * den(168)
  A(146) = cont_QA(wf(:,123),wf(:,144)) * den(169)
  A(147) = cont_QA(wf(:,159),wf(:,160)) * den(171)
  A(148) = cont_VV(wf(:,26),wf(:,161)) * den(48)
  A(149) = cont_QA(wf(:,41),wf(:,162)) * den(173)
  A(150) = cont_QA(wf(:,123),wf(:,153)) * den(174)
  A(151) = cont_VV(wf(:,29),wf(:,163)) * den(176)
  A(152) = cont_VV(wf(:,47),wf(:,154)) * den(177)
  A(153) = cont_VV(wf(:,32),wf(:,163)) * den(178)
  A(154) = cont_VV(wf(:,48),wf(:,154)) * den(179)
  A(155) = cont_VV(wf(:,33),wf(:,163)) * den(180)
  A(156) = cont_VV(wf(:,47),wf(:,157)) * den(58)
  A(157) = cont_VV(wf(:,48),wf(:,157)) * den(60)
  A(158) = cont_QA(wf(:,38),wf(:,164)) * den(63)
  A(159) = cont_QA(wf(:,44),wf(:,166)) * den(62)
  A(160) = cont_VV(wf(:,7),wf(:,167)) * den(28)
  A(161) = cont_VV(wf(:,26),wf(:,168)) * den(31)
  A(162) = cont_VV(wf(:,29),wf(:,169)) * den(34)
  A(163) = cont_VV(wf(:,32),wf(:,169)) * den(38)
  A(164) = cont_VV(wf(:,33),wf(:,169)) * den(40)
  A(165) = cont_QA(wf(:,38),wf(:,170)) * den(45)
  A(166) = cont_QA(wf(:,20),wf(:,172)) * den(42)
  A(167) = cont_VV(wf(:,28),wf(:,173)) * den(181)
  A(168) = cont_QA(wf(:,146),wf(:,174)) * den(182)
  A(169) = cont_QA(wf(:,115),wf(:,175)) * den(183)
  A(170) = cont_VV(wf(:,28),wf(:,178)) * den(38)
  A(171) = cont_QA(wf(:,17),wf(:,181)) * den(186)
  A(172) = cont_VV(wf(:,27),wf(:,182)) * den(188)
  A(173) = cont_QA(wf(:,115),wf(:,183)) * den(189)
  A(174) = cont_VV(wf(:,7),wf(:,184)) * den(28)
  A(175) = cont_VV(wf(:,27),wf(:,185)) * den(31)
  A(176) = cont_VV(wf(:,28),wf(:,186)) * den(34)
  A(177) = cont_VV(wf(:,11),wf(:,187)) * den(190)
  A(178) = cont_QA(wf(:,160),wf(:,188)) * den(191)
  A(179) = cont_QA(wf(:,126),wf(:,175)) * den(192)
  A(180) = cont_VV(wf(:,11),wf(:,189)) * den(58)
  A(181) = cont_QA(wf(:,41),wf(:,190)) * den(194)
  A(182) = cont_VV(wf(:,10),wf(:,191)) * den(196)
  A(183) = cont_QA(wf(:,126),wf(:,183)) * den(197)
  A(184) = cont_VV(wf(:,15),wf(:,191)) * den(198)
  A(185) = cont_VV(wf(:,39),wf(:,182)) * den(199)
  A(186) = cont_VV(wf(:,42),wf(:,182)) * den(200)
  A(187) = cont_VV(wf(:,18),wf(:,191)) * den(201)
  A(188) = cont_VV(wf(:,39),wf(:,185)) * den(48)
  A(189) = cont_VV(wf(:,42),wf(:,185)) * den(51)
  A(190) = cont_QA(wf(:,44),wf(:,193)) * den(54)
  A(191) = cont_QA(wf(:,24),wf(:,194)) * den(55)
  A(192) = cont_VV(wf(:,7),wf(:,195)) * den(8)
  A(193) = cont_VV(wf(:,10),wf(:,196)) * den(11)
  A(194) = cont_VV(wf(:,11),wf(:,197)) * den(14)
  A(195) = cont_VV(wf(:,15),wf(:,196)) * den(18)
  A(196) = cont_VV(wf(:,18),wf(:,196)) * den(21)
  A(197) = cont_QA(wf(:,20),wf(:,199)) * den(24)
  A(198) = cont_QA(wf(:,24),wf(:,200)) * den(27)
  A(199) = cont_VV(wf(:,84),wf(:,201)) * den(204)
  A(200) = cont_VV(wf(:,85),wf(:,202)) * den(207)
  A(201) = cont_VV(wf(:,8),wf(:,203)) * den(210)
  A(202) = cont_VV(wf(:,10),wf(:,204)) * den(212)
  A(203) = cont_VV(wf(:,11),wf(:,205)) * den(213)
  A(204) = cont_VV(wf(:,12),wf(:,206)) * den(215)
  A(205) = cont_VV(wf(:,9),wf(:,207)) * den(216)
  A(206) = cont_VV(wf(:,10),wf(:,208)) * den(217)
  A(207) = cont_VV(wf(:,12),wf(:,209)) * den(218)
  A(208) = cont_VV(wf(:,15),wf(:,204)) * den(219)
  A(209) = cont_VV(wf(:,9),wf(:,212)) * den(221)
  A(210) = cont_VV(wf(:,9),wf(:,213)) * den(222)
  A(211) = cont_VV(wf(:,18),wf(:,204)) * den(223)
  A(212) = cont_VV(wf(:,9),wf(:,216)) * den(225)
  A(213) = cont_VV(wf(:,9),wf(:,217)) * den(226)
  A(214) = cont_QA(wf(:,146),wf(:,218)) * den(227)
  A(215) = cont_QA(wf(:,24),wf(:,219)) * den(228)
  A(216) = cont_QA(wf(:,146),wf(:,220)) * den(229)
  A(217) = cont_QA(wf(:,90),wf(:,221)) * den(230)
  A(218) = cont_QA(wf(:,90),wf(:,222)) * den(232)
  A(219) = cont_QA(wf(:,21),wf(:,223)) * den(233)
  A(220) = cont_VV(wf(:,110),wf(:,224)) * den(236)
  A(221) = cont_VV(wf(:,111),wf(:,225)) * den(239)
  A(222) = cont_VV(wf(:,25),wf(:,203)) * den(241)
  A(223) = cont_VV(wf(:,27),wf(:,226)) * den(243)
  A(224) = cont_VV(wf(:,28),wf(:,227)) * den(244)
  A(225) = cont_VV(wf(:,29),wf(:,228)) * den(246)
  A(226) = cont_VV(wf(:,26),wf(:,229)) * den(247)
  A(227) = cont_VV(wf(:,27),wf(:,230)) * den(248)
  A(228) = cont_VV(wf(:,29),wf(:,231)) * den(249)
  A(229) = cont_VV(wf(:,28),wf(:,234)) * den(251)
  A(230) = cont_VV(wf(:,32),wf(:,228)) * den(252)
  A(231) = cont_VV(wf(:,28),wf(:,235)) * den(253)
  A(232) = cont_VV(wf(:,28),wf(:,236)) * den(254)
  A(233) = cont_VV(wf(:,28),wf(:,237)) * den(255)
  A(234) = cont_VV(wf(:,33),wf(:,228)) * den(256)
  A(235) = cont_QA(wf(:,146),wf(:,238)) * den(257)
  A(236) = cont_QA(wf(:,146),wf(:,239)) * den(258)
  A(237) = cont_QA(wf(:,115),wf(:,240)) * den(259)
  A(238) = cont_QA(wf(:,115),wf(:,241)) * den(260)
  A(239) = cont_QA(wf(:,38),wf(:,242)) * den(261)
  A(240) = cont_QA(wf(:,35),wf(:,223)) * den(262)
  A(241) = cont_VV(wf(:,39),wf(:,226)) * den(263)
  A(242) = cont_VV(wf(:,26),wf(:,243)) * den(264)
  A(243) = cont_VV(wf(:,26),wf(:,244)) * den(265)
  A(244) = cont_VV(wf(:,42),wf(:,226)) * den(266)
  A(245) = cont_VV(wf(:,26),wf(:,247)) * den(268)
  A(246) = cont_VV(wf(:,26),wf(:,248)) * den(269)
  A(247) = cont_QA(wf(:,160),wf(:,249)) * den(270)
  A(248) = cont_QA(wf(:,24),wf(:,250)) * den(271)
  A(249) = cont_QA(wf(:,160),wf(:,251)) * den(272)
  A(250) = cont_QA(wf(:,123),wf(:,221)) * den(273)
  A(251) = cont_QA(wf(:,123),wf(:,222)) * den(274)
  A(252) = cont_QA(wf(:,45),wf(:,252)) * den(275)
  A(253) = cont_VV(wf(:,11),wf(:,253)) * den(276)
  A(254) = cont_VV(wf(:,47),wf(:,206)) * den(277)
  A(255) = cont_VV(wf(:,11),wf(:,254)) * den(278)
  A(256) = cont_VV(wf(:,11),wf(:,255)) * den(279)
  A(257) = cont_VV(wf(:,11),wf(:,256)) * den(280)
  A(258) = cont_VV(wf(:,48),wf(:,206)) * den(281)
  A(259) = cont_QA(wf(:,160),wf(:,257)) * den(282)
  A(260) = cont_QA(wf(:,160),wf(:,258)) * den(283)
  A(261) = cont_QA(wf(:,126),wf(:,240)) * den(284)
  A(262) = cont_QA(wf(:,126),wf(:,241)) * den(285)
  A(263) = cont_QA(wf(:,38),wf(:,259)) * den(286)
  A(264) = cont_QA(wf(:,50),wf(:,252)) * den(287)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(264)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((A(13)+A(16)+A(18)+A(19))*f(1))/2._/**/REALKIND+((A(21)+A(22)+A(23)+A(24))*f(1))/6._/**/REALKIND+(CI*(-A(2)-A(10)+A(11) &
       -A(12))*f(2))/2._/**/REALKIND
  M1(2) = ((-A(17)-A(18)-A(19)-A(20))*f(1))/6._/**/REALKIND+((-A(6)-A(9)-A(22)-A(23))*f(1))/2._/**/REALKIND+(CI*(-A(1)+A(3)+A(4) &
       -A(5))*f(2))/2._/**/REALKIND
  M1(3) = ((-A(13)-A(14)-A(15)-A(16))*f(1))/6._/**/REALKIND+((-A(7)-A(8)-A(21)-A(24))*f(1))/2._/**/REALKIND+(CI*(A(1)-A(3)-A(4) &
       +A(5))*f(2))/2._/**/REALKIND
  M1(4) = ((A(6)+A(7)+A(8)+A(9))*f(1))/6._/**/REALKIND+((A(14)+A(15)+A(17)+A(20))*f(1))/2._/**/REALKIND+(CI*(A(2)+A(10)-A(11) &
       +A(12))*f(2))/2._/**/REALKIND

  M2(1) = ((A(88)+A(93)+A(104)+A(105)-A(229)-A(230)-A(231)-A(237)-A(238)-A(239)-A(244)-A(245)-A(246)-A(247)-A(249) &
       -A(252))*f(3))/2._/**/REALKIND+((A(107)+A(110)+A(111)+A(112)-A(253)-A(254)-A(255)-A(256)-A(257)-A(258)-A(259)-A(260)-A(261) &
       -A(262)-A(263)-A(264))*f(3))/6._/**/REALKIND+(CI*(-A(83)+A(84)-A(85)+A(86)-A(87)-A(220)+A(221)+A(222)-A(223)+A(224)+A(225) &
       -A(226)-A(227)+A(228))*f(4))/2._/**/REALKIND+((A(124)+A(125)+A(126)+A(127)+A(156)+A(157)+A(158)+A(159)+A(177)+A(178)+A(179) &
       +A(180))*f(5))/6._/**/REALKIND+((A(94)+A(96)+A(118)+A(120)+A(145)+A(147)+A(163)+A(165)+A(169)+A(170)+A(189) &
       +A(190))*f(5))/2._/**/REALKIND+(CI*(-A(36)-A(47)-A(160)+A(161)-A(162)-A(174)+A(175)-A(176))*f(6))/2._/**/REALKIND &
       -(CI*A(28)*f(7))/2._/**/REALKIND+(CI*(-A(80)+A(81)-A(82))*f(8))/2._/**/REALKIND+((-A(33)-A(34)-A(42)-A(43)-A(52) &
       -A(53))*f(9))/6._/**/REALKIND+((-A(29)-A(32)-A(40)-A(45)-A(50)-A(55))*f(9))/2._/**/REALKIND+(CI*(A(44) &
       -A(51))*f(10))/2._/**/REALKIND+((-A(108)-A(109)-A(113)-A(115)-A(128)-A(129)-A(152)-A(154)-A(181) &
       -A(183))*f(11))/6._/**/REALKIND+((-A(89)-A(98)-A(103)-A(116)-A(122)-A(131)-A(149)-A(153)-A(173) &
       -A(186))*f(11))/2._/**/REALKIND+(CI*(-A(100)+A(123)+A(151)-A(172))*f(12))/2._/**/REALKIND
  M2(2) = ((-A(102)-A(104)-A(105)-A(106)+A(241)+A(242)+A(243)+A(244)+A(245)+A(246)+A(247)+A(248)+A(249)+A(250)+A(251) &
       +A(252))*f(3))/6._/**/REALKIND+((-A(68)-A(72)-A(110)-A(111)+A(208)+A(209)+A(210)+A(215)+A(217)+A(218)+A(256)+A(257)+A(258) &
       +A(259)+A(260)+A(264))*f(3))/2._/**/REALKIND+(CI*(-A(62)+A(63)-A(64)+A(65)+A(66)-A(199)+A(200)-A(201)-A(202)+A(203)+A(204) &
       -A(205)-A(206)+A(207))*f(4))/2._/**/REALKIND+((-A(117)-A(118)-A(119)-A(120)-A(145)-A(146)-A(147)-A(148)-A(188)-A(189) &
       -A(190)-A(191))*f(5))/6._/**/REALKIND+((-A(73)-A(75)-A(125)-A(127)-A(136)-A(138)-A(157)-A(159)-A(177)-A(178)-A(195) &
       -A(198))*f(5))/2._/**/REALKIND+(CI*(-A(35)-A(48)+A(142)+A(143)-A(144)+A(192)+A(193)-A(194))*f(6))/2._/**/REALKIND &
       -(CI*A(25)*f(7))/2._/**/REALKIND+(CI*(A(59)+A(60)-A(61))*f(8))/2._/**/REALKIND+((A(31)+A(32)+A(40)+A(41)+A(54) &
       +A(55))*f(9))/6._/**/REALKIND+((A(26)+A(34)+A(38)+A(43)+A(52)+A(57))*f(9))/2._/**/REALKIND+(CI*(A(39) &
       -A(56))*f(10))/2._/**/REALKIND+((A(67)+A(77)+A(109)+A(115)+A(129)+A(132)+A(140)+A(154)+A(181) &
       +A(184))*f(11))/2._/**/REALKIND+((A(101)+A(103)+A(114)+A(116)+A(121)+A(122)+A(149)+A(150)+A(185) &
       +A(186))*f(11))/6._/**/REALKIND+(CI*(A(79)-A(130)+A(141)-A(182))*f(12))/2._/**/REALKIND
  M2(3) = ((-A(88)-A(91)-A(92)-A(93)+A(229)+A(230)+A(231)+A(232)+A(233)+A(234)+A(235)+A(236)+A(237)+A(238)+A(239) &
       +A(240))*f(3))/6._/**/REALKIND+((-A(70)-A(71)-A(107)-A(112)+A(211)+A(212)+A(213)+A(214)+A(216)+A(219)+A(253)+A(254)+A(255) &
       +A(261)+A(262)+A(263))*f(3))/2._/**/REALKIND+(CI*(A(62)-A(63)+A(64)-A(65)-A(66)+A(199)-A(200)+A(201)+A(202)-A(203)-A(204) &
       +A(205)+A(206)-A(207))*f(4))/2._/**/REALKIND+((-A(94)-A(95)-A(96)-A(97)-A(163)-A(164)-A(165)-A(166)-A(167)-A(168)-A(169) &
       -A(170))*f(5))/6._/**/REALKIND+((-A(74)-A(76)-A(124)-A(126)-A(135)-A(137)-A(156)-A(158)-A(179)-A(180)-A(196) &
       -A(197))*f(5))/2._/**/REALKIND+(CI*(A(35)+A(48)-A(142)-A(143)+A(144)-A(192)-A(193)+A(194))*f(6))/2._/**/REALKIND &
       +(CI*A(25)*f(7))/2._/**/REALKIND+(CI*(-A(59)-A(60)+A(61))*f(8))/2._/**/REALKIND+((A(29)+A(30)+A(45)+A(46)+A(49) &
       +A(50))*f(9))/6._/**/REALKIND+((A(27)+A(33)+A(37)+A(42)+A(53)+A(58))*f(9))/2._/**/REALKIND+(CI*(-A(39) &
       +A(56))*f(10))/2._/**/REALKIND+((A(89)+A(90)+A(98)+A(99)+A(131)+A(133)+A(153)+A(155)+A(171)+A(173))*f(11))/6._/**/REALKIND &
       +((A(69)+A(78)+A(108)+A(113)+A(128)+A(134)+A(139)+A(152)+A(183)+A(187))*f(11))/2._/**/REALKIND+(CI*(-A(79)+A(130)-A(141) &
       +A(182))*f(12))/2._/**/REALKIND
  M2(4) = ((A(68)+A(70)+A(71)+A(72)-A(208)-A(209)-A(210)-A(211)-A(212)-A(213)-A(214)-A(215)-A(216)-A(217)-A(218) &
       -A(219))*f(3))/6._/**/REALKIND+((A(91)+A(92)+A(102)+A(106)-A(232)-A(233)-A(234)-A(235)-A(236)-A(240)-A(241)-A(242)-A(243) &
       -A(248)-A(250)-A(251))*f(3))/2._/**/REALKIND+(CI*(A(83)-A(84)+A(85)-A(86)+A(87)+A(220)-A(221)-A(222)+A(223)-A(224)-A(225) &
       +A(226)+A(227)-A(228))*f(4))/2._/**/REALKIND+((A(95)+A(97)+A(117)+A(119)+A(146)+A(148)+A(164)+A(166)+A(167)+A(168)+A(188) &
       +A(191))*f(5))/2._/**/REALKIND+((A(73)+A(74)+A(75)+A(76)+A(135)+A(136)+A(137)+A(138)+A(195)+A(196)+A(197) &
       +A(198))*f(5))/6._/**/REALKIND+(CI*(A(36)+A(47)+A(160)-A(161)+A(162)+A(174)-A(175)+A(176))*f(6))/2._/**/REALKIND &
       +(CI*A(28)*f(7))/2._/**/REALKIND+(CI*(A(80)-A(81)+A(82))*f(8))/2._/**/REALKIND+((-A(30)-A(31)-A(41)-A(46)-A(49) &
       -A(54))*f(9))/2._/**/REALKIND+((-A(26)-A(27)-A(37)-A(38)-A(57)-A(58))*f(9))/6._/**/REALKIND+(CI*(-A(44) &
       +A(51))*f(10))/2._/**/REALKIND+((-A(90)-A(99)-A(101)-A(114)-A(121)-A(133)-A(150)-A(155)-A(171) &
       -A(185))*f(11))/2._/**/REALKIND+((-A(67)-A(69)-A(77)-A(78)-A(132)-A(134)-A(139)-A(140)-A(184) &
       -A(187))*f(11))/6._/**/REALKIND+(CI*(A(100)-A(123)-A(151)+A(172))*f(12))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_heftpphjjj_dddxdxhg_1_/**/REALKIND
