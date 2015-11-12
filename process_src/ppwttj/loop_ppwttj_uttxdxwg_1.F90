
module ol_colourmatrix_ppwttj_uttxdxwg_1_/**/REALKIND
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
end module ol_colourmatrix_ppwttj_uttxdxwg_1_/**/REALKIND



module ol_forced_parameters_ppwttj_uttxdxwg_1_/**/REALKIND
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
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwttj_uttxdxwg_1_/**/REALKIND

module ol_loop_ppwttj_uttxdxwg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:147)
  ! denominators
  complex(REALKIND), save :: den(154)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(4,96)
  ! zero helicity identifier
  logical,           save :: zerohel(96) = .true., zerohel_ct(96) = .true.

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
    f( 1) = (CI*eQED*gQCD**3)/(sqrt2*sw)
    f( 2) = (eQED*gQCD**3)/(sqrt2*sw)
    f( 3) = (CI*countertermnorm*eQED*gQCD**5)/(sqrt2*sw)
    f( 4) = (countertermnorm*eQED*gQCD**5)/(sqrt2*sw)
    f( 5) = (CI*countertermnorm*ctGqq*eQED*gQCD**5)/(sqrt2*sw)
    f( 6) = (countertermnorm*ctGqq*eQED*gQCD**5)/(sqrt2*sw)
    f( 7) = (CI*countertermnorm*ctGtt*eQED*gQCD**5)/(sqrt2*sw)
    f( 8) = (countertermnorm*ctGtt*eQED*gQCD**5)/(sqrt2*sw)
    f( 9) = (CI*countertermnorm*ctVqq*eQED*gQCD**5)/(sqrt2*sw)
    f(10) = (countertermnorm*ctVqq*eQED*gQCD**5)/(sqrt2*sw)
    f(11) = (countertermnorm*ctVVV*eQED*gQCD**5)/(sqrt2*sw)
    f(12) = (CI*eQED*gQCD**5*integralnorm*SwB)/(2._/**/REALKIND*sqrt2*sw)
    f(13) = (CI*eQED*gQCD**5*integralnorm*SwB)/(sqrt2*sw)
    f(14) = (eQED*gQCD**5*integralnorm*SwB)/(sqrt2*sw*2._/**/REALKIND)
    f(15) = (eQED*gQCD**5*integralnorm*SwB)/(sqrt2*sw)
    f(16) = (CI*eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)
    f(17) = (2*CI*eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)
    f(18) = (eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)
    f(19) = (2*eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)

  c = [ 27*CI*f(12), 54*CI*f(12), 3*CI*f(13), 9*CI*f(13), 24*CI*f(13), 27*CI*f(13), 54*CI*f(13), 18*f(14), 54*f(14), f(15) &
    , 3*f(15), 6*f(15), 8*f(15), 9*f(15), 10*f(15), 18*f(15), 21*f(15), 24*f(15), 27*f(15), 54*f(15), 9*CI*f(16), 9*CI*f(17) &
    , 3*f(18), 9*f(18), 3*f(19), 9*f(19) ]
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
  complex(REALKIND) :: A(96)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rMT, H(2), wf(:,-1))
  call wf_A(P(:,3), rMT, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_WQ_A(wf(:,-4),wf(:,0),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,2),wf(:,4),wf(:,6))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,7))
  call vert_UV_W(wf(:,2),Q(:,6),wf(:,-5),Q(:,32),wf(:,8))
  call vert_AV_Q(wf(:,-3),wf(:,2),wf(:,9))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,14),ZERO,0_intkind1,wf(:,11))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,12))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,33),ZERO,0_intkind1,wf(:,14))
  call prop_A_Q(wf(:,13),Q(:,24),ZERO,0_intkind1,wf(:,15))
  call vert_VQ_A(wf(:,2),wf(:,14),wf(:,16))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,17))
  call vert_AV_Q(wf(:,15),wf(:,-5),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,7),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,0),wf(:,15),wf(:,20))
  call vert_WQ_A(wf(:,-4),wf(:,14),wf(:,21))
  call vert_AW_Q(wf(:,5),wf(:,-4),wf(:,22))
  call vert_VQ_A(wf(:,-5),wf(:,-1),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,34),MT,1_intkind1,wf(:,24))
  call vert_QA_V(wf(:,24),wf(:,-2),wf(:,25))
  call vert_AV_Q(wf(:,-2),wf(:,-5),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,36),MT,1_intkind1,wf(:,27))
  call vert_QA_V(wf(:,-1),wf(:,27),wf(:,28))
  call counter_VQ_A(wf(:,2),wf(:,4),wf(:,29))
  call counter_UV_W(wf(:,2),Q(:,6),wf(:,-5),Q(:,32),wf(:,30))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,31))
  call counter_VQ_A(wf(:,2),wf(:,14),wf(:,32))
  call counter_AV_Q(wf(:,15),wf(:,-5),wf(:,33))
  call counter_WQ_A(wf(:,-4),wf(:,14),wf(:,34))
  call counter_AW_Q(wf(:,5),wf(:,-4),wf(:,35))
  call counter_AV_Q(wf(:,-3),wf(:,2),wf(:,36))
  call prop_Q_A(wf(:,10),Q(:,49),ZERO,0_intkind1,wf(:,37))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,38))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,40),ZERO,0_intkind1,wf(:,40))
  call prop_Q_A(wf(:,21),Q(:,49),ZERO,0_intkind1,wf(:,41))
  call vert_AW_Q(wf(:,40),wf(:,-4),wf(:,42))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,43))
  call prop_A_Q(wf(:,43),Q(:,24),ZERO,0_intkind1,wf(:,44))
  call vert_AV_Q(wf(:,44),wf(:,-5),wf(:,45))
  call vert_QA_V(wf(:,0),wf(:,44),wf(:,46))
  call counter_QA_V(wf(:,24),wf(:,-2),wf(:,47))
  call counter_AV_Q(wf(:,-2),wf(:,-5),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,36),MT,1_intkind1,wf(:,49))
  call vert_QA_V(wf(:,-1),wf(:,49),wf(:,50))
  call counter_QA_V(wf(:,-1),wf(:,27),wf(:,51))
  call counter_VQ_A(wf(:,-5),wf(:,-1),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,34),MT,1_intkind1,wf(:,53))
  call vert_QA_V(wf(:,53),wf(:,-2),wf(:,54))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,55))
  call vert_VQ_A(wf(:,55),wf(:,4),wf(:,56))
  call vert_UV_W(wf(:,55),Q(:,6),wf(:,-5),Q(:,32),wf(:,57))
  call vert_AV_Q(wf(:,-3),wf(:,55),wf(:,58))
  call prop_A_Q(wf(:,58),Q(:,14),ZERO,0_intkind1,wf(:,59))
  call vert_VQ_A(wf(:,55),wf(:,14),wf(:,60))
  call vert_VQ_A(wf(:,55),wf(:,0),wf(:,61))
  call prop_Q_A(wf(:,61),Q(:,7),ZERO,0_intkind1,wf(:,62))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,63))
  call prop_A_Q(wf(:,18),Q(:,56),ZERO,0_intkind1,wf(:,64))
  call counter_QA_V(wf(:,0),wf(:,15),wf(:,65))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,33),ZERO,0_intkind1,wf(:,67))
  call vert_VQ_A(wf(:,2),wf(:,67),wf(:,68))
  call prop_A_Q(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,69))
  call vert_WQ_A(wf(:,-4),wf(:,67),wf(:,70))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,71))
  call prop_Q_A(wf(:,71),Q(:,17),ZERO,0_intkind1,wf(:,72))
  call vert_VQ_A(wf(:,2),wf(:,72),wf(:,73))
  call vert_QA_V(wf(:,72),wf(:,-3),wf(:,74))
  call vert_VQ_A(wf(:,-5),wf(:,72),wf(:,75))
  call vert_AV_Q(wf(:,5),wf(:,2),wf(:,76))
  call counter_Q_A(ctqq,wf(:,4),Q(:,17),wf(:,77))
  call prop_A_Q(wf(:,76),Q(:,46),ZERO,0_intkind1,wf(:,78))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,79))
  call counter_V_V(ctGG,wf(:,2),Q(:,6),wf(:,80))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,81))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,82))
  call prop_Q_A(wf(:,77),Q(:,17),ZERO,0_intkind1,wf(:,83))
  call vert_QA_V(wf(:,83),wf(:,-3),wf(:,84))
  call vert_VQ_A(wf(:,-5),wf(:,83),wf(:,85))
  call vert_AV_Q(wf(:,-3),wf(:,80),wf(:,86))
  call vert_UV_W(wf(:,80),Q(:,6),wf(:,-5),Q(:,32),wf(:,87))
  call counter_V_V(ctGG,wf(:,7),Q(:,25),wf(:,88))
  call counter_A_Q(ctqq,wf(:,11),Q(:,14),wf(:,89))
  call vert_AV_Q(wf(:,15),wf(:,2),wf(:,90))
  call counter_Q_A(ctqq,wf(:,14),Q(:,33),wf(:,91))
  call prop_A_Q(wf(:,90),Q(:,30),ZERO,0_intkind1,wf(:,92))
  call vert_QA_V(wf(:,14),wf(:,15),wf(:,93))
  call counter_A_Q(ctqq,wf(:,15),Q(:,24),wf(:,94))
  call prop_Q_A(wf(:,16),Q(:,39),ZERO,0_intkind1,wf(:,95))
  call vert_VQ_A(wf(:,80),wf(:,0),wf(:,96))
  call prop_A_Q(wf(:,94),Q(:,24),ZERO,0_intkind1,wf(:,97))
  call vert_QA_V(wf(:,0),wf(:,97),wf(:,98))
  call counter_Q_A(ctqq,wf(:,19),Q(:,7),wf(:,99))
  call counter_V_V(ctGG,wf(:,20),Q(:,25),wf(:,100))
  call vert_AV_Q(wf(:,97),wf(:,-5),wf(:,101))
  call prop_Q_A(wf(:,91),Q(:,33),ZERO,0_intkind1,wf(:,102))
  call vert_WQ_A(wf(:,-4),wf(:,102),wf(:,103))
  call prop_A_Q(wf(:,81),Q(:,40),ZERO,0_intkind1,wf(:,104))
  call vert_AW_Q(wf(:,104),wf(:,-4),wf(:,105))
  call counter_Q_A(cttt,wf(:,24),Q(:,34),wf(:,106))
  call prop_Q_A(wf(:,106),Q(:,34),MT,1_intkind1,wf(:,107))
  call vert_QA_V(wf(:,107),wf(:,-2),wf(:,108))
  call counter_V_V(ctGG,wf(:,25),Q(:,38),wf(:,109))
  call counter_A_Q(cttt,wf(:,27),Q(:,36),wf(:,110))
  call prop_A_Q(wf(:,110),Q(:,36),MT,1_intkind1,wf(:,111))
  call vert_QA_V(wf(:,-1),wf(:,111),wf(:,112))
  call counter_V_V(ctGG,wf(:,28),Q(:,38),wf(:,113))
  call vert_WQ_A(wf(:,-4),wf(:,19),wf(:,114))
  call prop_Q_A(wf(:,114),Q(:,23),ZERO,0_intkind1,wf(:,115))
  call vert_VQ_A(wf(:,-5),wf(:,19),wf(:,116))
  call prop_Q_A(wf(:,116),Q(:,39),ZERO,0_intkind1,wf(:,117))
  call vert_VQ_A(wf(:,8),wf(:,0),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,39),ZERO,0_intkind1,wf(:,119))
  call vert_AW_Q(wf(:,11),wf(:,-4),wf(:,120))
  call prop_A_Q(wf(:,120),Q(:,30),ZERO,0_intkind1,wf(:,121))
  call vert_AV_Q(wf(:,11),wf(:,-5),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,46),ZERO,0_intkind1,wf(:,123))
  call vert_AV_Q(wf(:,-3),wf(:,8),wf(:,124))
  call prop_A_Q(wf(:,124),Q(:,46),ZERO,0_intkind1,wf(:,125))
  call vert_VQ_A(wf(:,7),wf(:,-1),wf(:,126))
  call prop_Q_A(wf(:,126),Q(:,27),MT,1_intkind1,wf(:,127))
  call vert_AV_Q(wf(:,-2),wf(:,7),wf(:,128))
  call prop_A_Q(wf(:,128),Q(:,29),MT,1_intkind1,wf(:,129))
  call vert_UV_W(wf(:,7),Q(:,25),wf(:,-5),Q(:,32),wf(:,130))
  call vert_QA_V(wf(:,37),wf(:,-3),wf(:,131))
  call vert_VQ_A(wf(:,20),wf(:,-1),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,27),MT,1_intkind1,wf(:,133))
  call vert_AV_Q(wf(:,-2),wf(:,20),wf(:,134))
  call prop_A_Q(wf(:,134),Q(:,29),MT,1_intkind1,wf(:,135))
  call vert_UV_W(wf(:,20),Q(:,25),wf(:,-5),Q(:,32),wf(:,136))
  call vert_QA_V(wf(:,0),wf(:,64),wf(:,137))
  call vert_QA_V(wf(:,41),wf(:,-3),wf(:,138))
  call vert_VQ_A(wf(:,25),wf(:,0),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,39),ZERO,0_intkind1,wf(:,140))
  call vert_AV_Q(wf(:,-3),wf(:,25),wf(:,141))
  call prop_A_Q(wf(:,141),Q(:,46),ZERO,0_intkind1,wf(:,142))
  call vert_VQ_A(wf(:,28),wf(:,0),wf(:,143))
  call prop_Q_A(wf(:,143),Q(:,39),ZERO,0_intkind1,wf(:,144))
  call vert_AV_Q(wf(:,-3),wf(:,28),wf(:,145))
  call prop_A_Q(wf(:,145),Q(:,46),ZERO,0_intkind1,wf(:,146))
  call vert_QA_V(wf(:,0),wf(:,69),wf(:,147))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,17))
  den(2) = 1 / (Q(5,6))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,25))
  den(9) = 1 / (Q(5,14))
  den(12) = 1 / (Q(5,33))
  den(13) = 1 / (Q(5,24))
  den(16) = 1 / (Q(5,7))
  den(23) = 1 / (Q(5,34) - MT2)
  den(24) = 1 / (Q(5,38))
  den(27) = 1 / (Q(5,36) - MT2)
  den(32) = 1 / (Q(5,49))
  den(41) = 1 / (Q(5,56))
  den(50) = 1 / (Q(5,46))
  den(54) = 1 / (Q(5,57))
  den(57) = 1 / (Q(5,23))
  den(69) = 1 / (Q(5,30))
  den(75) = 1 / (Q(5,39))
  den(113) = 1 / (Q(5,27) - MT2)
  den(115) = 1 / (Q(5,29) - MT2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(2)*den(12)
  den(15) = den(13)*den(14)
  den(17) = den(2)*den(16)
  den(18) = den(13)*den(17)
  den(19) = den(6)*den(13)
  den(20) = den(2)*den(19)
  den(21) = den(10)*den(12)
  den(22) = den(3)*den(17)
  den(25) = den(23)*den(24)
  den(26) = den(1)*den(25)
  den(28) = den(24)*den(27)
  den(29) = den(1)*den(28)
  den(30) = den(19)*den(23)
  den(31) = den(19)*den(27)
  den(33) = den(1)*den(32)
  den(34) = den(2)*den(33)
  den(35) = den(2)*den(24)
  den(36) = den(1)*den(35)
  den(37) = den(12)*den(32)
  den(38) = den(2)*den(37)
  den(39) = den(7)*den(23)
  den(40) = den(7)*den(27)
  den(42) = den(13)*den(41)
  den(43) = den(2)*den(42)
  den(44) = den(13)*den(35)
  den(45) = den(3)*den(41)
  den(46) = den(2)*den(45)
  den(47) = den(13)*den(25)
  den(48) = den(13)*den(28)
  den(49) = den(2)*den(3)
  den(51) = den(49)*den(50)
  den(52) = den(1)*den(51)
  den(53) = den(1)*den(3)
  den(55) = den(53)*den(54)
  den(56) = den(2)*den(55)
  den(58) = den(4)*den(57)
  den(59) = den(3)*den(58)
  den(60) = den(1)**2
  den(61) = den(35)*den(60)
  den(62) = den(10)*den(60)
  den(63) = den(2)**2
  den(64) = den(33)*den(63)
  den(65) = den(7)*den(63)
  den(66) = den(7)*den(35)
  den(67) = den(10)*den(33)
  den(68) = den(2)*den(13)
  den(70) = den(68)*den(69)
  den(71) = den(12)*den(70)
  den(72) = den(12)*den(13)
  den(73) = den(54)*den(72)
  den(74) = den(2)*den(73)
  den(76) = den(14)*den(75)
  den(77) = den(13)*den(76)
  den(78) = den(42)*den(63)
  den(79) = den(13)**2
  den(80) = den(35)*den(79)
  den(81) = den(17)*den(42)
  den(82) = den(19)*den(35)
  den(83) = den(19)*den(63)
  den(84) = den(17)*den(79)
  den(85) = den(12)**2
  den(86) = den(10)*den(85)
  den(87) = den(37)*den(63)
  den(88) = den(10)*den(37)
  den(89) = den(45)*den(63)
  den(90) = den(17)*den(45)
  den(91) = den(3)**2
  den(92) = den(17)*den(91)
  den(93) = den(25)*den(60)
  den(94) = den(23)**2
  den(95) = den(7)*den(94)
  den(96) = den(7)*den(25)
  den(97) = den(28)*den(60)
  den(98) = den(27)**2
  den(99) = den(7)*den(98)
  den(100) = den(7)*den(28)
  den(101) = den(19)*den(25)
  den(102) = den(25)*den(79)
  den(103) = den(19)*den(94)
  den(104) = den(19)*den(28)
  den(105) = den(28)*den(79)
  den(106) = den(19)*den(98)
  den(107) = den(17)*den(57)
  den(108) = den(17)*den(75)
  den(109) = den(35)*den(75)
  den(110) = den(10)*den(69)
  den(111) = den(10)*den(50)
  den(112) = den(35)*den(50)
  den(114) = den(7)*den(113)
  den(116) = den(7)*den(115)
  den(117) = den(7)*den(54)
  den(118) = den(33)*den(54)
  den(119) = den(19)*den(113)
  den(120) = den(19)*den(115)
  den(121) = den(19)*den(54)
  den(122) = den(42)*den(54)
  den(123) = den(37)*den(54)
  den(124) = den(25)*den(75)
  den(125) = den(25)*den(50)
  den(126) = den(28)*den(75)
  den(127) = den(28)*den(50)
  den(128) = den(45)*den(54)
  den(129) = den(1)*den(2)*den(3)
  den(130) = den(2)*den(12)*den(13)
  den(131) = den(1)*den(23)
  den(132) = den(1)*den(27)
  den(133) = den(13)*den(23)
  den(134) = den(13)*den(27)
  den(135) = den(2)*den(117)
  den(136) = den(2)*den(118)
  den(137) = den(1)*den(111)
  den(138) = den(1)*den(112)
  den(139) = den(13)*den(108)
  den(140) = den(2)*den(121)
  den(141) = den(13)*den(109)
  den(142) = den(2)*den(122)
  den(143) = den(2)*den(123)
  den(144) = den(12)*den(110)
  den(145) = den(3)*den(107)
  den(146) = den(2)*den(128)
  den(147) = den(23)*den(116)
  den(148) = den(1)*den(125)
  den(149) = den(27)*den(114)
  den(150) = den(1)*den(127)
  den(151) = den(23)*den(120)
  den(152) = den(13)*den(124)
  den(153) = den(27)*den(119)
  den(154) = den(13)*den(126)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(96)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(8)
  A(3) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(6) = cont_VV(wf(:,8),wf(:,20)) * den(20)
  A(7) = cont_QA(wf(:,11),wf(:,21)) * den(21)
  A(8) = cont_QA(wf(:,19),wf(:,22)) * den(22)
  A(9) = cont_VV(wf(:,7),wf(:,25)) * den(26)
  A(10) = cont_VV(wf(:,7),wf(:,28)) * den(29)
  A(11) = cont_VV(wf(:,20),wf(:,25)) * den(30)
  A(12) = cont_VV(wf(:,20),wf(:,28)) * den(31)

  A(13) = cont_QA(wf(:,5),wf(:,29)) * den(5)
  A(14) = cont_VV(wf(:,7),wf(:,30)) * den(8)
  A(15) = cont_QA(wf(:,11),wf(:,31)) * den(11)
  A(16) = cont_QA(wf(:,15),wf(:,32)) * den(15)
  A(17) = cont_QA(wf(:,19),wf(:,33)) * den(18)
  A(18) = cont_VV(wf(:,20),wf(:,30)) * den(20)
  A(19) = cont_QA(wf(:,11),wf(:,34)) * den(21)
  A(20) = cont_QA(wf(:,19),wf(:,35)) * den(22)
  A(21) = cont_QA(wf(:,36),wf(:,37)) * den(34)
  A(22) = cont_VV(wf(:,8),wf(:,38)) * den(36)
  A(23) = cont_QA(wf(:,6),wf(:,40)) * den(5)
  A(24) = cont_QA(wf(:,36),wf(:,41)) * den(38)
  A(25) = cont_QA(wf(:,19),wf(:,42)) * den(22)
  A(26) = cont_QA(wf(:,16),wf(:,44)) * den(15)
  A(27) = cont_QA(wf(:,19),wf(:,45)) * den(18)
  A(28) = cont_VV(wf(:,8),wf(:,46)) * den(20)
  A(29) = cont_VV(wf(:,25),wf(:,38)) * den(26)
  A(30) = cont_VV(wf(:,28),wf(:,38)) * den(29)
  A(31) = cont_VV(wf(:,25),wf(:,46)) * den(30)
  A(32) = cont_VV(wf(:,28),wf(:,46)) * den(31)
  A(33) = cont_VV(wf(:,7),wf(:,47)) * den(39)
  A(34) = cont_VV(wf(:,7),wf(:,50)) * den(29)
  A(35) = cont_VV(wf(:,20),wf(:,47)) * den(30)
  A(36) = cont_VV(wf(:,20),wf(:,50)) * den(31)
  A(37) = cont_VV(wf(:,7),wf(:,51)) * den(40)
  A(38) = cont_VV(wf(:,7),wf(:,54)) * den(26)
  A(39) = cont_VV(wf(:,20),wf(:,51)) * den(31)
  A(40) = cont_VV(wf(:,20),wf(:,54)) * den(30)
  A(41) = cont_QA(wf(:,5),wf(:,56)) * den(5)
  A(42) = cont_VV(wf(:,7),wf(:,57)) * den(8)
  A(43) = cont_QA(wf(:,10),wf(:,59)) * den(11)
  A(44) = cont_QA(wf(:,15),wf(:,60)) * den(15)
  A(45) = cont_VV(wf(:,20),wf(:,57)) * den(20)
  A(46) = cont_QA(wf(:,18),wf(:,62)) * den(18)
  A(47) = cont_QA(wf(:,21),wf(:,59)) * den(21)
  A(48) = cont_QA(wf(:,22),wf(:,62)) * den(22)
  A(49) = cont_QA(wf(:,63),wf(:,64)) * den(43)
  A(50) = cont_VV(wf(:,8),wf(:,65)) * den(44)
  A(51) = cont_QA(wf(:,15),wf(:,68)) * den(15)
  A(52) = cont_QA(wf(:,63),wf(:,69)) * den(46)
  A(53) = cont_QA(wf(:,11),wf(:,70)) * den(21)
  A(54) = cont_QA(wf(:,5),wf(:,73)) * den(5)
  A(55) = cont_VV(wf(:,8),wf(:,74)) * den(8)
  A(56) = cont_QA(wf(:,11),wf(:,75)) * den(11)
  A(57) = cont_VV(wf(:,25),wf(:,65)) * den(47)
  A(58) = cont_VV(wf(:,28),wf(:,65)) * den(48)
  A(59) = cont_VV(wf(:,25),wf(:,74)) * den(26)
  A(60) = cont_VV(wf(:,28),wf(:,74)) * den(29)
  A(61) = cont_QA(wf(:,77),wf(:,78)) * den(52)
  A(62) = cont_VV(wf(:,79),wf(:,80)) * den(56)
  A(63) = cont_QA(wf(:,81),wf(:,82)) * den(59)
  A(64) = cont_VV(wf(:,8),wf(:,84)) * den(61)
  A(65) = cont_QA(wf(:,11),wf(:,85)) * den(62)
  A(66) = cont_QA(wf(:,37),wf(:,86)) * den(64)
  A(67) = cont_VV(wf(:,7),wf(:,87)) * den(65)
  A(68) = cont_VV(wf(:,8),wf(:,88)) * den(66)
  A(69) = cont_QA(wf(:,37),wf(:,89)) * den(67)
  A(70) = cont_QA(wf(:,91),wf(:,92)) * den(71)
  A(71) = cont_VV(wf(:,80),wf(:,93)) * den(74)
  A(72) = cont_QA(wf(:,94),wf(:,95)) * den(77)
  A(73) = cont_QA(wf(:,64),wf(:,96)) * den(78)
  A(74) = cont_VV(wf(:,8),wf(:,98)) * den(80)
  A(75) = cont_QA(wf(:,64),wf(:,99)) * den(81)
  A(76) = cont_VV(wf(:,8),wf(:,100)) * den(82)
  A(77) = cont_VV(wf(:,20),wf(:,87)) * den(83)
  A(78) = cont_QA(wf(:,19),wf(:,101)) * den(84)
  A(79) = cont_QA(wf(:,11),wf(:,103)) * den(86)
  A(80) = cont_QA(wf(:,41),wf(:,86)) * den(87)
  A(81) = cont_QA(wf(:,41),wf(:,89)) * den(88)
  A(82) = cont_QA(wf(:,69),wf(:,96)) * den(89)
  A(83) = cont_QA(wf(:,69),wf(:,99)) * den(90)
  A(84) = cont_QA(wf(:,19),wf(:,105)) * den(92)
  A(85) = cont_VV(wf(:,25),wf(:,84)) * den(93)
  A(86) = cont_VV(wf(:,7),wf(:,108)) * den(95)
  A(87) = cont_VV(wf(:,7),wf(:,109)) * den(96)
  A(88) = cont_VV(wf(:,28),wf(:,84)) * den(97)
  A(89) = cont_VV(wf(:,7),wf(:,112)) * den(99)
  A(90) = cont_VV(wf(:,7),wf(:,113)) * den(100)
  A(91) = cont_VV(wf(:,25),wf(:,100)) * den(101)
  A(92) = cont_VV(wf(:,25),wf(:,98)) * den(102)
  A(93) = cont_VV(wf(:,20),wf(:,108)) * den(103)
  A(94) = cont_VV(wf(:,28),wf(:,100)) * den(104)
  A(95) = cont_VV(wf(:,28),wf(:,98)) * den(105)
  A(96) = cont_VV(wf(:,20),wf(:,112)) * den(106)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(96)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((-A(3)-A(4)-A(7)-A(10)-A(12))*f(1))/2._/**/REALKIND+(CI*(-A(2)-A(6))*f(2))/2._/**/REALKIND
  M1(2) = ((A(9)+A(10)+A(11)+A(12))*f(1))/6._/**/REALKIND
  M1(3) = ((A(1)+A(3)+A(4)+A(5)+A(7)+A(8))*f(1))/6._/**/REALKIND
  M1(4) = ((-A(1)-A(5)-A(8)-A(9)-A(11))*f(1))/2._/**/REALKIND+(CI*(A(2)+A(6))*f(2))/2._/**/REALKIND

  M2(1) = ((A(65)+A(66)+A(69)+A(70)+A(71)+A(72)+A(79)+A(80)+A(81)+A(88)+A(89)+A(90)+A(94)+A(95)+A(96))*f(3))/2._/**/REALKIND &
       +(CI*(A(64)+A(67)+A(68)+A(74)+A(76)+A(77))*f(4))/2._/**/REALKIND+((-A(15)-A(16)-A(21)-A(24)-A(30)-A(51)-A(53) &
       -A(58))*f(5))/2._/**/REALKIND+(CI*(-A(22)-A(50))*f(6))/2._/**/REALKIND+((-A(34)-A(36)-A(37)-A(39)-A(43)-A(44) &
       -A(47))*f(7))/2._/**/REALKIND+(CI*(-A(42)-A(45))*f(8))/2._/**/REALKIND+((-A(19)-A(26)-A(32)-A(56) &
       -A(60))*f(9))/2._/**/REALKIND+(CI*(-A(28)-A(55))*f(10))/2._/**/REALKIND+(CI*(-A(14)-A(18))*f(11))/2._/**/REALKIND
  M2(2) = ((-A(85)-A(86)-A(87)-A(88)-A(89)-A(90)-A(91)-A(92)-A(93)-A(94)-A(95)-A(96))*f(3))/6._/**/REALKIND+((A(29)+A(30)+A(57) &
       +A(58))*f(5))/6._/**/REALKIND+((A(33)+A(34)+A(35)+A(36)+A(37)+A(38)+A(39)+A(40))*f(7))/6._/**/REALKIND+((A(31)+A(32)+A(59) &
       +A(60))*f(9))/6._/**/REALKIND
  M2(3) = ((-A(61)-A(62)-A(63)-A(65)-A(66)-A(69)-A(70)-A(71)-A(72)-A(73)-A(75)-A(78)-A(79)-A(80)-A(81)-A(82)-A(83) &
       -A(84))*f(3))/6._/**/REALKIND+((A(13)+A(15)+A(16)+A(17)+A(21)+A(23)+A(24)+A(25)+A(49)+A(51)+A(52) &
       +A(53))*f(5))/6._/**/REALKIND+((A(41)+A(43)+A(44)+A(46)+A(47)+A(48))*f(7))/6._/**/REALKIND+((A(19)+A(20)+A(26)+A(27)+A(54) &
       +A(56))*f(9))/6._/**/REALKIND
  M2(4) = ((A(61)+A(62)+A(63)+A(73)+A(75)+A(78)+A(82)+A(83)+A(84)+A(85)+A(86)+A(87)+A(91)+A(92)+A(93))*f(3))/2._/**/REALKIND+(CI*( &
       -A(64)-A(67)-A(68)-A(74)-A(76)-A(77))*f(4))/2._/**/REALKIND+((-A(13)-A(17)-A(23)-A(25)-A(29)-A(49)-A(52) &
       -A(57))*f(5))/2._/**/REALKIND+(CI*(A(22)+A(50))*f(6))/2._/**/REALKIND+((-A(33)-A(35)-A(38)-A(40)-A(41)-A(46) &
       -A(48))*f(7))/2._/**/REALKIND+(CI*(A(42)+A(45))*f(8))/2._/**/REALKIND+((-A(20)-A(27)-A(31)-A(54) &
       -A(59))*f(9))/2._/**/REALKIND+(CI*(A(28)+A(55))*f(10))/2._/**/REALKIND+(CI*(A(14)+A(18))*f(11))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwttj_uttxdxwg_1_/**/REALKIND
