
module ol_colourmatrix_ppwjjj_uddxdxwg_1_/**/REALKIND
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
end module ol_colourmatrix_ppwjjj_uddxdxwg_1_/**/REALKIND



module ol_forced_parameters_ppwjjj_uddxdxwg_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwjjj_uddxdxwg_1_/**/REALKIND

module ol_loop_ppwjjj_uddxdxwg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(17), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:253)
  ! denominators
  complex(REALKIND), save :: den(286)
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
    f( 7) = (CI*countertermnorm*ctVqq*eQED*gQCD**5)/(sqrt2*sw)
    f( 8) = (countertermnorm*ctVqq*eQED*gQCD**5)/(sqrt2*sw)
    f( 9) = (countertermnorm*ctVVV*eQED*gQCD**5)/(sqrt2*sw)
    f(10) = (CI*eQED*gQCD**5*integralnorm*SwB)/(2._/**/REALKIND*sqrt2*sw)
    f(11) = (CI*eQED*gQCD**5*integralnorm*SwB)/(sqrt2*sw)
    f(12) = (eQED*gQCD**5*integralnorm*SwB)/(sqrt2*sw*2._/**/REALKIND)
    f(13) = (eQED*gQCD**5*integralnorm*SwB)/(sqrt2*sw)
    f(14) = (CI*eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)
    f(15) = (2*CI*eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)
    f(16) = (eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)
    f(17) = (2*eQED*gQCD**5*integralnorm*SwF)/(sqrt2*sw)

  c = [ 27*CI*f(10), 54*CI*f(10), 3*CI*f(11), 9*CI*f(11), 24*CI*f(11), 27*CI*f(11), 54*CI*f(11), 18*f(12), 54*f(12), f(13) &
    , 3*f(13), 6*f(13), 8*f(13), 9*f(13), 10*f(13), 18*f(13), 21*f(13), 24*f(13), 27*f(13), 54*f(13), 9*CI*f(14), 9*CI*f(15) &
    , 3*f(16), 9*f(16), 3*f(17), 9*f(17) ]
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
  complex(REALKIND) :: A(192)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
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
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,23))
  call vert_AV_Q(wf(:,-2),wf(:,-5),wf(:,24))
  call prop_A_Q(wf(:,24),Q(:,36),ZERO,0_intkind1,wf(:,25))
  call vert_VQ_A(wf(:,23),wf(:,4),wf(:,26))
  call vert_QA_V(wf(:,4),wf(:,-2),wf(:,27))
  call vert_UV_W(wf(:,23),Q(:,10),wf(:,-5),Q(:,32),wf(:,28))
  call vert_AV_Q(wf(:,-2),wf(:,23),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,14),ZERO,0_intkind1,wf(:,30))
  call vert_AW_Q(wf(:,-2),wf(:,-4),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,20),ZERO,0_intkind1,wf(:,32))
  call vert_VQ_A(wf(:,23),wf(:,14),wf(:,33))
  call vert_VQ_A(wf(:,23),wf(:,0),wf(:,34))
  call vert_AV_Q(wf(:,32),wf(:,-5),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,11),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,0),wf(:,32),wf(:,37))
  call vert_AW_Q(wf(:,25),wf(:,-4),wf(:,38))
  call vert_VQ_A(wf(:,-5),wf(:,-1),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,34),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,40),wf(:,-3),wf(:,41))
  call vert_QA_V(wf(:,40),wf(:,-2),wf(:,42))
  call vert_QA_V(wf(:,-1),wf(:,25),wf(:,43))
  call vert_QA_V(wf(:,-1),wf(:,5),wf(:,44))
  call counter_VQ_A(wf(:,2),wf(:,4),wf(:,45))
  call counter_UV_W(wf(:,2),Q(:,6),wf(:,-5),Q(:,32),wf(:,46))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,47))
  call counter_VQ_A(wf(:,2),wf(:,14),wf(:,48))
  call counter_AV_Q(wf(:,15),wf(:,-5),wf(:,49))
  call counter_WQ_A(wf(:,-4),wf(:,14),wf(:,50))
  call counter_AW_Q(wf(:,5),wf(:,-4),wf(:,51))
  call counter_AV_Q(wf(:,-3),wf(:,2),wf(:,52))
  call prop_Q_A(wf(:,10),Q(:,49),ZERO,0_intkind1,wf(:,53))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,54))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,55))
  call prop_A_Q(wf(:,55),Q(:,40),ZERO,0_intkind1,wf(:,56))
  call prop_Q_A(wf(:,21),Q(:,49),ZERO,0_intkind1,wf(:,57))
  call vert_AW_Q(wf(:,56),wf(:,-4),wf(:,58))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,59))
  call prop_A_Q(wf(:,59),Q(:,24),ZERO,0_intkind1,wf(:,60))
  call vert_AV_Q(wf(:,60),wf(:,-5),wf(:,61))
  call vert_QA_V(wf(:,0),wf(:,60),wf(:,62))
  call counter_VQ_A(wf(:,23),wf(:,4),wf(:,63))
  call counter_UV_W(wf(:,23),Q(:,10),wf(:,-5),Q(:,32),wf(:,64))
  call counter_VQ_A(wf(:,23),wf(:,14),wf(:,65))
  call counter_AV_Q(wf(:,32),wf(:,-5),wf(:,66))
  call counter_AW_Q(wf(:,25),wf(:,-4),wf(:,67))
  call counter_QA_V(wf(:,40),wf(:,-3),wf(:,68))
  call vert_QA_V(wf(:,-1),wf(:,56),wf(:,69))
  call counter_AV_Q(wf(:,-2),wf(:,23),wf(:,70))
  call counter_QA_V(wf(:,4),wf(:,-2),wf(:,71))
  call counter_AV_Q(wf(:,-2),wf(:,-5),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,36),ZERO,0_intkind1,wf(:,73))
  call vert_AW_Q(wf(:,73),wf(:,-4),wf(:,74))
  call counter_AW_Q(wf(:,-2),wf(:,-4),wf(:,75))
  call prop_A_Q(wf(:,75),Q(:,20),ZERO,0_intkind1,wf(:,76))
  call vert_AV_Q(wf(:,76),wf(:,-5),wf(:,77))
  call vert_QA_V(wf(:,0),wf(:,76),wf(:,78))
  call counter_QA_V(wf(:,40),wf(:,-2),wf(:,79))
  call vert_QA_V(wf(:,-1),wf(:,73),wf(:,80))
  call counter_QA_V(wf(:,-1),wf(:,25),wf(:,81))
  call counter_QA_V(wf(:,-1),wf(:,5),wf(:,82))
  call counter_VQ_A(wf(:,-5),wf(:,-1),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,34),ZERO,0_intkind1,wf(:,84))
  call vert_QA_V(wf(:,84),wf(:,-3),wf(:,85))
  call vert_QA_V(wf(:,84),wf(:,-2),wf(:,86))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,87))
  call vert_VQ_A(wf(:,87),wf(:,4),wf(:,88))
  call vert_UV_W(wf(:,87),Q(:,10),wf(:,-5),Q(:,32),wf(:,89))
  call vert_AV_Q(wf(:,-2),wf(:,87),wf(:,90))
  call prop_A_Q(wf(:,90),Q(:,14),ZERO,0_intkind1,wf(:,91))
  call vert_VQ_A(wf(:,87),wf(:,14),wf(:,92))
  call vert_VQ_A(wf(:,87),wf(:,0),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,11),ZERO,0_intkind1,wf(:,94))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,95))
  call vert_VQ_A(wf(:,95),wf(:,4),wf(:,96))
  call vert_UV_W(wf(:,95),Q(:,6),wf(:,-5),Q(:,32),wf(:,97))
  call vert_AV_Q(wf(:,-3),wf(:,95),wf(:,98))
  call prop_A_Q(wf(:,98),Q(:,14),ZERO,0_intkind1,wf(:,99))
  call vert_VQ_A(wf(:,95),wf(:,14),wf(:,100))
  call vert_VQ_A(wf(:,95),wf(:,0),wf(:,101))
  call prop_Q_A(wf(:,101),Q(:,7),ZERO,0_intkind1,wf(:,102))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,103))
  call prop_A_Q(wf(:,18),Q(:,56),ZERO,0_intkind1,wf(:,104))
  call counter_QA_V(wf(:,0),wf(:,15),wf(:,105))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,106))
  call prop_Q_A(wf(:,106),Q(:,33),ZERO,0_intkind1,wf(:,107))
  call vert_VQ_A(wf(:,2),wf(:,107),wf(:,108))
  call prop_A_Q(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,109))
  call vert_WQ_A(wf(:,-4),wf(:,107),wf(:,110))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,17),ZERO,0_intkind1,wf(:,112))
  call vert_VQ_A(wf(:,2),wf(:,112),wf(:,113))
  call vert_QA_V(wf(:,112),wf(:,-3),wf(:,114))
  call vert_VQ_A(wf(:,-5),wf(:,112),wf(:,115))
  call counter_VQ_A(wf(:,23),wf(:,0),wf(:,116))
  call prop_A_Q(wf(:,35),Q(:,52),ZERO,0_intkind1,wf(:,117))
  call counter_QA_V(wf(:,0),wf(:,32),wf(:,118))
  call vert_VQ_A(wf(:,23),wf(:,107),wf(:,119))
  call prop_A_Q(wf(:,38),Q(:,52),ZERO,0_intkind1,wf(:,120))
  call vert_VQ_A(wf(:,23),wf(:,112),wf(:,121))
  call vert_QA_V(wf(:,112),wf(:,-2),wf(:,122))
  call vert_AV_Q(wf(:,5),wf(:,2),wf(:,123))
  call counter_Q_A(ctqq,wf(:,4),Q(:,17),wf(:,124))
  call prop_A_Q(wf(:,123),Q(:,46),ZERO,0_intkind1,wf(:,125))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,126))
  call counter_V_V(ctGG,wf(:,2),Q(:,6),wf(:,127))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,128))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,129))
  call prop_Q_A(wf(:,124),Q(:,17),ZERO,0_intkind1,wf(:,130))
  call vert_QA_V(wf(:,130),wf(:,-3),wf(:,131))
  call vert_VQ_A(wf(:,-5),wf(:,130),wf(:,132))
  call vert_AV_Q(wf(:,-3),wf(:,127),wf(:,133))
  call vert_UV_W(wf(:,127),Q(:,6),wf(:,-5),Q(:,32),wf(:,134))
  call counter_V_V(ctGG,wf(:,7),Q(:,25),wf(:,135))
  call counter_A_Q(ctqq,wf(:,11),Q(:,14),wf(:,136))
  call vert_AV_Q(wf(:,15),wf(:,2),wf(:,137))
  call counter_Q_A(ctqq,wf(:,14),Q(:,33),wf(:,138))
  call prop_A_Q(wf(:,137),Q(:,30),ZERO,0_intkind1,wf(:,139))
  call vert_QA_V(wf(:,14),wf(:,15),wf(:,140))
  call counter_A_Q(ctqq,wf(:,15),Q(:,24),wf(:,141))
  call prop_Q_A(wf(:,16),Q(:,39),ZERO,0_intkind1,wf(:,142))
  call vert_VQ_A(wf(:,127),wf(:,0),wf(:,143))
  call prop_A_Q(wf(:,141),Q(:,24),ZERO,0_intkind1,wf(:,144))
  call vert_QA_V(wf(:,0),wf(:,144),wf(:,145))
  call counter_Q_A(ctqq,wf(:,19),Q(:,7),wf(:,146))
  call counter_V_V(ctGG,wf(:,20),Q(:,25),wf(:,147))
  call vert_AV_Q(wf(:,144),wf(:,-5),wf(:,148))
  call prop_Q_A(wf(:,138),Q(:,33),ZERO,0_intkind1,wf(:,149))
  call vert_WQ_A(wf(:,-4),wf(:,149),wf(:,150))
  call prop_A_Q(wf(:,128),Q(:,40),ZERO,0_intkind1,wf(:,151))
  call vert_AW_Q(wf(:,151),wf(:,-4),wf(:,152))
  call vert_AV_Q(wf(:,25),wf(:,23),wf(:,153))
  call prop_A_Q(wf(:,153),Q(:,46),ZERO,0_intkind1,wf(:,154))
  call vert_QA_V(wf(:,4),wf(:,25),wf(:,155))
  call counter_V_V(ctGG,wf(:,23),Q(:,10),wf(:,156))
  call counter_A_Q(ctqq,wf(:,25),Q(:,36),wf(:,157))
  call prop_Q_A(wf(:,26),Q(:,27),ZERO,0_intkind1,wf(:,158))
  call vert_QA_V(wf(:,130),wf(:,-2),wf(:,159))
  call vert_AV_Q(wf(:,-2),wf(:,156),wf(:,160))
  call vert_UV_W(wf(:,156),Q(:,10),wf(:,-5),Q(:,32),wf(:,161))
  call counter_V_V(ctGG,wf(:,27),Q(:,21),wf(:,162))
  call counter_A_Q(ctqq,wf(:,30),Q(:,14),wf(:,163))
  call vert_AV_Q(wf(:,32),wf(:,23),wf(:,164))
  call prop_A_Q(wf(:,164),Q(:,30),ZERO,0_intkind1,wf(:,165))
  call vert_QA_V(wf(:,14),wf(:,32),wf(:,166))
  call counter_A_Q(ctqq,wf(:,32),Q(:,20),wf(:,167))
  call prop_Q_A(wf(:,33),Q(:,43),ZERO,0_intkind1,wf(:,168))
  call vert_VQ_A(wf(:,156),wf(:,0),wf(:,169))
  call prop_A_Q(wf(:,167),Q(:,20),ZERO,0_intkind1,wf(:,170))
  call vert_QA_V(wf(:,0),wf(:,170),wf(:,171))
  call counter_Q_A(ctqq,wf(:,36),Q(:,11),wf(:,172))
  call counter_V_V(ctGG,wf(:,37),Q(:,21),wf(:,173))
  call vert_AV_Q(wf(:,170),wf(:,-5),wf(:,174))
  call prop_A_Q(wf(:,157),Q(:,36),ZERO,0_intkind1,wf(:,175))
  call vert_AW_Q(wf(:,175),wf(:,-4),wf(:,176))
  call counter_Q_A(ctqq,wf(:,40),Q(:,34),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,34),ZERO,0_intkind1,wf(:,178))
  call vert_QA_V(wf(:,178),wf(:,-2),wf(:,179))
  call vert_QA_V(wf(:,178),wf(:,-3),wf(:,180))
  call counter_V_V(ctGG,wf(:,42),Q(:,38),wf(:,181))
  call vert_QA_V(wf(:,-1),wf(:,175),wf(:,182))
  call counter_V_V(ctGG,wf(:,43),Q(:,38),wf(:,183))
  call counter_V_V(ctGG,wf(:,44),Q(:,42),wf(:,184))
  call vert_QA_V(wf(:,-1),wf(:,151),wf(:,185))
  call vert_WQ_A(wf(:,-4),wf(:,19),wf(:,186))
  call prop_Q_A(wf(:,186),Q(:,23),ZERO,0_intkind1,wf(:,187))
  call vert_VQ_A(wf(:,-5),wf(:,19),wf(:,188))
  call prop_Q_A(wf(:,188),Q(:,39),ZERO,0_intkind1,wf(:,189))
  call vert_VQ_A(wf(:,8),wf(:,0),wf(:,190))
  call prop_Q_A(wf(:,190),Q(:,39),ZERO,0_intkind1,wf(:,191))
  call vert_AW_Q(wf(:,11),wf(:,-4),wf(:,192))
  call prop_A_Q(wf(:,192),Q(:,30),ZERO,0_intkind1,wf(:,193))
  call vert_AV_Q(wf(:,11),wf(:,-5),wf(:,194))
  call prop_A_Q(wf(:,194),Q(:,46),ZERO,0_intkind1,wf(:,195))
  call vert_AV_Q(wf(:,-3),wf(:,8),wf(:,196))
  call prop_A_Q(wf(:,196),Q(:,46),ZERO,0_intkind1,wf(:,197))
  call vert_WQ_A(wf(:,-4),wf(:,36),wf(:,198))
  call prop_Q_A(wf(:,198),Q(:,27),ZERO,0_intkind1,wf(:,199))
  call vert_VQ_A(wf(:,-5),wf(:,36),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,43),ZERO,0_intkind1,wf(:,201))
  call vert_VQ_A(wf(:,28),wf(:,0),wf(:,202))
  call prop_Q_A(wf(:,202),Q(:,43),ZERO,0_intkind1,wf(:,203))
  call vert_AW_Q(wf(:,30),wf(:,-4),wf(:,204))
  call prop_A_Q(wf(:,204),Q(:,30),ZERO,0_intkind1,wf(:,205))
  call vert_AV_Q(wf(:,30),wf(:,-5),wf(:,206))
  call prop_A_Q(wf(:,206),Q(:,46),ZERO,0_intkind1,wf(:,207))
  call vert_AV_Q(wf(:,-2),wf(:,28),wf(:,208))
  call prop_A_Q(wf(:,208),Q(:,46),ZERO,0_intkind1,wf(:,209))
  call vert_VQ_A(wf(:,27),wf(:,-1),wf(:,210))
  call prop_Q_A(wf(:,210),Q(:,23),ZERO,0_intkind1,wf(:,211))
  call vert_AV_Q(wf(:,-3),wf(:,27),wf(:,212))
  call prop_A_Q(wf(:,212),Q(:,29),ZERO,0_intkind1,wf(:,213))
  call vert_UV_W(wf(:,27),Q(:,21),wf(:,-5),Q(:,32),wf(:,214))
  call vert_VQ_A(wf(:,7),wf(:,-1),wf(:,215))
  call prop_Q_A(wf(:,215),Q(:,27),ZERO,0_intkind1,wf(:,216))
  call vert_AV_Q(wf(:,-2),wf(:,7),wf(:,217))
  call prop_A_Q(wf(:,217),Q(:,29),ZERO,0_intkind1,wf(:,218))
  call vert_UV_W(wf(:,7),Q(:,25),wf(:,-5),Q(:,32),wf(:,219))
  call vert_QA_V(wf(:,53),wf(:,-2),wf(:,220))
  call vert_QA_V(wf(:,53),wf(:,-3),wf(:,221))
  call vert_VQ_A(wf(:,37),wf(:,-1),wf(:,222))
  call prop_Q_A(wf(:,222),Q(:,23),ZERO,0_intkind1,wf(:,223))
  call vert_AV_Q(wf(:,-3),wf(:,37),wf(:,224))
  call prop_A_Q(wf(:,224),Q(:,29),ZERO,0_intkind1,wf(:,225))
  call vert_UV_W(wf(:,37),Q(:,21),wf(:,-5),Q(:,32),wf(:,226))
  call vert_QA_V(wf(:,0),wf(:,117),wf(:,227))
  call vert_VQ_A(wf(:,20),wf(:,-1),wf(:,228))
  call prop_Q_A(wf(:,228),Q(:,27),ZERO,0_intkind1,wf(:,229))
  call vert_AV_Q(wf(:,-2),wf(:,20),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,29),ZERO,0_intkind1,wf(:,231))
  call vert_UV_W(wf(:,20),Q(:,25),wf(:,-5),Q(:,32),wf(:,232))
  call vert_QA_V(wf(:,0),wf(:,104),wf(:,233))
  call vert_QA_V(wf(:,57),wf(:,-2),wf(:,234))
  call vert_QA_V(wf(:,57),wf(:,-3),wf(:,235))
  call vert_VQ_A(wf(:,42),wf(:,0),wf(:,236))
  call prop_Q_A(wf(:,236),Q(:,39),ZERO,0_intkind1,wf(:,237))
  call vert_VQ_A(wf(:,41),wf(:,0),wf(:,238))
  call prop_Q_A(wf(:,238),Q(:,43),ZERO,0_intkind1,wf(:,239))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,240))
  call prop_A_Q(wf(:,240),Q(:,46),ZERO,0_intkind1,wf(:,241))
  call vert_AV_Q(wf(:,-2),wf(:,41),wf(:,242))
  call prop_A_Q(wf(:,242),Q(:,46),ZERO,0_intkind1,wf(:,243))
  call vert_VQ_A(wf(:,43),wf(:,0),wf(:,244))
  call prop_Q_A(wf(:,244),Q(:,39),ZERO,0_intkind1,wf(:,245))
  call vert_QA_V(wf(:,0),wf(:,120),wf(:,246))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,247))
  call prop_A_Q(wf(:,247),Q(:,46),ZERO,0_intkind1,wf(:,248))
  call vert_VQ_A(wf(:,44),wf(:,0),wf(:,249))
  call prop_Q_A(wf(:,249),Q(:,43),ZERO,0_intkind1,wf(:,250))
  call vert_QA_V(wf(:,0),wf(:,109),wf(:,251))
  call vert_AV_Q(wf(:,-2),wf(:,44),wf(:,252))
  call prop_A_Q(wf(:,252),Q(:,46),ZERO,0_intkind1,wf(:,253))

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
  den(23) = 1 / (Q(5,10))
  den(24) = 1 / (Q(5,36))
  den(27) = 1 / (Q(5,21))
  den(32) = 1 / (Q(5,20))
  den(35) = 1 / (Q(5,11))
  den(42) = 1 / (Q(5,34))
  den(44) = 1 / (Q(5,38))
  den(49) = 1 / (Q(5,42))
  den(56) = 1 / (Q(5,49))
  den(72) = 1 / (Q(5,56))
  den(78) = 1 / (Q(5,52))
  den(89) = 1 / (Q(5,46))
  den(93) = 1 / (Q(5,57))
  den(96) = 1 / (Q(5,23))
  den(108) = 1 / (Q(5,30))
  den(114) = 1 / (Q(5,39))
  den(136) = 1 / (Q(5,53))
  den(139) = 1 / (Q(5,27))
  den(155) = 1 / (Q(5,43))
  den(210) = 1 / (Q(5,29))

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
  den(25) = den(1)*den(23)
  den(26) = den(24)*den(25)
  den(28) = den(1)*den(27)
  den(29) = den(23)*den(28)
  den(30) = den(9)*den(23)
  den(31) = den(1)*den(30)
  den(33) = den(12)*den(23)
  den(34) = den(32)*den(33)
  den(36) = den(23)*den(35)
  den(37) = den(32)*den(36)
  den(38) = den(27)*den(32)
  den(39) = den(23)*den(38)
  den(40) = den(12)*den(30)
  den(41) = den(24)*den(36)
  den(43) = den(28)*den(42)
  den(45) = den(42)*den(44)
  den(46) = den(1)*den(45)
  den(47) = den(24)*den(44)
  den(48) = den(1)*den(47)
  den(50) = den(3)*den(49)
  den(51) = den(1)*den(50)
  den(52) = den(38)*den(42)
  den(53) = den(3)*den(38)
  den(54) = den(19)*den(42)
  den(55) = den(19)*den(24)
  den(57) = den(1)*den(56)
  den(58) = den(2)*den(57)
  den(59) = den(2)*den(44)
  den(60) = den(1)*den(59)
  den(61) = den(12)*den(56)
  den(62) = den(2)*den(61)
  den(63) = den(23)*den(57)
  den(64) = den(23)*den(49)
  den(65) = den(1)*den(64)
  den(66) = den(23)*den(61)
  den(67) = den(7)*den(42)
  den(68) = den(42)*den(49)
  den(69) = den(1)*den(68)
  den(70) = den(7)*den(24)
  den(71) = den(3)*den(28)
  den(73) = den(13)*den(72)
  den(74) = den(2)*den(73)
  den(75) = den(13)*den(59)
  den(76) = den(3)*den(72)
  den(77) = den(2)*den(76)
  den(79) = den(32)*den(78)
  den(80) = den(23)*den(79)
  den(81) = den(32)*den(64)
  den(82) = den(24)*den(78)
  den(83) = den(23)*den(82)
  den(84) = den(32)*den(68)
  den(85) = den(32)*den(50)
  den(86) = den(13)*den(45)
  den(87) = den(13)*den(47)
  den(88) = den(2)*den(3)
  den(90) = den(88)*den(89)
  den(91) = den(1)*den(90)
  den(92) = den(1)*den(3)
  den(94) = den(92)*den(93)
  den(95) = den(2)*den(94)
  den(97) = den(4)*den(96)
  den(98) = den(3)*den(97)
  den(99) = den(1)**2
  den(100) = den(59)*den(99)
  den(101) = den(10)*den(99)
  den(102) = den(2)**2
  den(103) = den(57)*den(102)
  den(104) = den(7)*den(102)
  den(105) = den(7)*den(59)
  den(106) = den(10)*den(57)
  den(107) = den(2)*den(13)
  den(109) = den(107)*den(108)
  den(110) = den(12)*den(109)
  den(111) = den(12)*den(13)
  den(112) = den(93)*den(111)
  den(113) = den(2)*den(112)
  den(115) = den(14)*den(114)
  den(116) = den(13)*den(115)
  den(117) = den(73)*den(102)
  den(118) = den(13)**2
  den(119) = den(59)*den(118)
  den(120) = den(17)*den(73)
  den(121) = den(19)*den(59)
  den(122) = den(19)*den(102)
  den(123) = den(17)*den(118)
  den(124) = den(12)**2
  den(125) = den(10)*den(124)
  den(126) = den(61)*den(102)
  den(127) = den(10)*den(61)
  den(128) = den(76)*den(102)
  den(129) = den(17)*den(76)
  den(130) = den(3)**2
  den(131) = den(17)*den(130)
  den(132) = den(23)*den(24)
  den(133) = den(89)*den(132)
  den(134) = den(1)*den(133)
  den(135) = den(1)*den(24)
  den(137) = den(135)*den(136)
  den(138) = den(23)*den(137)
  den(140) = den(25)*den(139)
  den(141) = den(24)*den(140)
  den(142) = den(64)*den(99)
  den(143) = den(30)*den(99)
  den(144) = den(23)**2
  den(145) = den(57)*den(144)
  den(146) = den(28)*den(144)
  den(147) = den(28)*den(64)
  den(148) = den(30)*den(57)
  den(149) = den(23)*den(32)
  den(150) = den(108)*den(149)
  den(151) = den(12)*den(150)
  den(152) = den(12)*den(32)
  den(153) = den(136)*den(152)
  den(154) = den(23)*den(153)
  den(156) = den(33)*den(155)
  den(157) = den(32)*den(156)
  den(158) = den(79)*den(144)
  den(159) = den(32)**2
  den(160) = den(64)*den(159)
  den(161) = den(36)*den(79)
  den(162) = den(38)*den(64)
  den(163) = den(38)*den(144)
  den(164) = den(36)*den(159)
  den(165) = den(30)*den(124)
  den(166) = den(61)*den(144)
  den(167) = den(30)*den(61)
  den(168) = den(82)*den(144)
  den(169) = den(36)*den(82)
  den(170) = den(24)**2
  den(171) = den(36)*den(170)
  den(172) = den(68)*den(99)
  den(173) = den(45)*den(99)
  den(174) = den(42)**2
  den(175) = den(7)*den(174)
  den(176) = den(28)*den(174)
  den(177) = den(28)*den(68)
  den(178) = den(7)*den(45)
  den(179) = den(47)*den(99)
  den(180) = den(7)*den(170)
  den(181) = den(7)*den(47)
  den(182) = den(50)*den(99)
  den(183) = den(28)*den(50)
  den(184) = den(28)*den(130)
  den(185) = den(68)*den(159)
  den(186) = den(38)*den(68)
  den(187) = den(38)*den(174)
  den(188) = den(38)*den(50)
  den(189) = den(50)*den(159)
  den(190) = den(38)*den(130)
  den(191) = den(19)*den(45)
  den(192) = den(45)*den(118)
  den(193) = den(19)*den(174)
  den(194) = den(19)*den(47)
  den(195) = den(47)*den(118)
  den(196) = den(19)*den(170)
  den(197) = den(17)*den(96)
  den(198) = den(17)*den(114)
  den(199) = den(59)*den(114)
  den(200) = den(10)*den(108)
  den(201) = den(10)*den(89)
  den(202) = den(59)*den(89)
  den(203) = den(36)*den(139)
  den(204) = den(36)*den(155)
  den(205) = den(64)*den(155)
  den(206) = den(30)*den(108)
  den(207) = den(30)*den(89)
  den(208) = den(64)*den(89)
  den(209) = den(28)*den(96)
  den(211) = den(28)*den(210)
  den(212) = den(28)*den(136)
  den(213) = den(7)*den(139)
  den(214) = den(7)*den(210)
  den(215) = den(7)*den(93)
  den(216) = den(57)*den(136)
  den(217) = den(57)*den(93)
  den(218) = den(38)*den(96)
  den(219) = den(38)*den(210)
  den(220) = den(38)*den(136)
  den(221) = den(79)*den(136)
  den(222) = den(19)*den(139)
  den(223) = den(19)*den(210)
  den(224) = den(19)*den(93)
  den(225) = den(73)*den(93)
  den(226) = den(61)*den(136)
  den(227) = den(61)*den(93)
  den(228) = den(45)*den(114)
  den(229) = den(68)*den(155)
  den(230) = den(45)*den(89)
  den(231) = den(68)*den(89)
  den(232) = den(47)*den(114)
  den(233) = den(82)*den(136)
  den(234) = den(47)*den(89)
  den(235) = den(50)*den(155)
  den(236) = den(76)*den(93)
  den(237) = den(50)*den(89)
  den(238) = den(1)*den(2)*den(3)
  den(239) = den(2)*den(12)*den(13)
  den(240) = den(1)*den(23)*den(24)
  den(241) = den(12)*den(23)*den(32)
  den(242) = den(1)*den(42)
  den(243) = den(32)*den(42)
  den(244) = den(3)*den(32)
  den(245) = den(13)*den(42)
  den(246) = den(13)*den(24)
  den(247) = den(2)*den(215)
  den(248) = den(2)*den(217)
  den(249) = den(1)*den(201)
  den(250) = den(1)*den(202)
  den(251) = den(13)*den(198)
  den(252) = den(2)*den(224)
  den(253) = den(13)*den(199)
  den(254) = den(2)*den(225)
  den(255) = den(2)*den(227)
  den(256) = den(12)*den(200)
  den(257) = den(3)*den(197)
  den(258) = den(2)*den(236)
  den(259) = den(23)*den(212)
  den(260) = den(23)*den(216)
  den(261) = den(1)*den(207)
  den(262) = den(1)*den(208)
  den(263) = den(32)*den(204)
  den(264) = den(23)*den(220)
  den(265) = den(32)*den(205)
  den(266) = den(23)*den(221)
  den(267) = den(23)*den(226)
  den(268) = den(12)*den(206)
  den(269) = den(24)*den(203)
  den(270) = den(23)*den(233)
  den(271) = den(42)*den(211)
  den(272) = den(42)*den(214)
  den(273) = den(1)*den(230)
  den(274) = den(1)*den(231)
  den(275) = den(24)*den(213)
  den(276) = den(1)*den(234)
  den(277) = den(3)*den(209)
  den(278) = den(1)*den(237)
  den(279) = den(42)*den(219)
  den(280) = den(32)*den(229)
  den(281) = den(3)*den(218)
  den(282) = den(32)*den(235)
  den(283) = den(42)*den(223)
  den(284) = den(13)*den(228)
  den(285) = den(24)*den(222)
  den(286) = den(13)*den(232)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(192)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(8)
  A(3) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(6) = cont_VV(wf(:,8),wf(:,20)) * den(20)
  A(7) = cont_QA(wf(:,11),wf(:,21)) * den(21)
  A(8) = cont_QA(wf(:,19),wf(:,22)) * den(22)
  A(9) = cont_QA(wf(:,25),wf(:,26)) * den(26)
  A(10) = cont_VV(wf(:,27),wf(:,28)) * den(29)
  A(11) = cont_QA(wf(:,10),wf(:,30)) * den(31)
  A(12) = cont_QA(wf(:,32),wf(:,33)) * den(34)
  A(13) = cont_QA(wf(:,35),wf(:,36)) * den(37)
  A(14) = cont_VV(wf(:,28),wf(:,37)) * den(39)
  A(15) = cont_QA(wf(:,21),wf(:,30)) * den(40)
  A(16) = cont_QA(wf(:,36),wf(:,38)) * den(41)
  A(17) = cont_VV(wf(:,27),wf(:,41)) * den(43)
  A(18) = cont_VV(wf(:,7),wf(:,42)) * den(46)
  A(19) = cont_VV(wf(:,7),wf(:,43)) * den(48)
  A(20) = cont_VV(wf(:,27),wf(:,44)) * den(51)
  A(21) = cont_VV(wf(:,37),wf(:,41)) * den(52)
  A(22) = cont_VV(wf(:,37),wf(:,44)) * den(53)
  A(23) = cont_VV(wf(:,20),wf(:,42)) * den(54)
  A(24) = cont_VV(wf(:,20),wf(:,43)) * den(55)

  A(25) = cont_QA(wf(:,5),wf(:,45)) * den(5)
  A(26) = cont_VV(wf(:,7),wf(:,46)) * den(8)
  A(27) = cont_QA(wf(:,11),wf(:,47)) * den(11)
  A(28) = cont_QA(wf(:,15),wf(:,48)) * den(15)
  A(29) = cont_QA(wf(:,19),wf(:,49)) * den(18)
  A(30) = cont_VV(wf(:,20),wf(:,46)) * den(20)
  A(31) = cont_QA(wf(:,11),wf(:,50)) * den(21)
  A(32) = cont_QA(wf(:,19),wf(:,51)) * den(22)
  A(33) = cont_QA(wf(:,52),wf(:,53)) * den(58)
  A(34) = cont_VV(wf(:,8),wf(:,54)) * den(60)
  A(35) = cont_QA(wf(:,6),wf(:,56)) * den(5)
  A(36) = cont_QA(wf(:,52),wf(:,57)) * den(62)
  A(37) = cont_QA(wf(:,19),wf(:,58)) * den(22)
  A(38) = cont_QA(wf(:,16),wf(:,60)) * den(15)
  A(39) = cont_QA(wf(:,19),wf(:,61)) * den(18)
  A(40) = cont_VV(wf(:,8),wf(:,62)) * den(20)
  A(41) = cont_QA(wf(:,25),wf(:,63)) * den(26)
  A(42) = cont_VV(wf(:,27),wf(:,64)) * den(29)
  A(43) = cont_QA(wf(:,30),wf(:,47)) * den(31)
  A(44) = cont_QA(wf(:,32),wf(:,65)) * den(34)
  A(45) = cont_QA(wf(:,36),wf(:,66)) * den(37)
  A(46) = cont_VV(wf(:,37),wf(:,64)) * den(39)
  A(47) = cont_QA(wf(:,30),wf(:,50)) * den(40)
  A(48) = cont_QA(wf(:,36),wf(:,67)) * den(41)
  A(49) = cont_VV(wf(:,27),wf(:,68)) * den(43)
  A(50) = cont_VV(wf(:,42),wf(:,54)) * den(46)
  A(51) = cont_VV(wf(:,43),wf(:,54)) * den(48)
  A(52) = cont_VV(wf(:,27),wf(:,69)) * den(51)
  A(53) = cont_VV(wf(:,37),wf(:,68)) * den(52)
  A(54) = cont_VV(wf(:,37),wf(:,69)) * den(53)
  A(55) = cont_VV(wf(:,42),wf(:,62)) * den(54)
  A(56) = cont_VV(wf(:,43),wf(:,62)) * den(55)
  A(57) = cont_QA(wf(:,53),wf(:,70)) * den(63)
  A(58) = cont_VV(wf(:,28),wf(:,71)) * den(65)
  A(59) = cont_QA(wf(:,26),wf(:,73)) * den(26)
  A(60) = cont_QA(wf(:,57),wf(:,70)) * den(66)
  A(61) = cont_QA(wf(:,36),wf(:,74)) * den(41)
  A(62) = cont_QA(wf(:,33),wf(:,76)) * den(34)
  A(63) = cont_QA(wf(:,36),wf(:,77)) * den(37)
  A(64) = cont_VV(wf(:,28),wf(:,78)) * den(39)
  A(65) = cont_VV(wf(:,7),wf(:,79)) * den(67)
  A(66) = cont_VV(wf(:,41),wf(:,71)) * den(69)
  A(67) = cont_VV(wf(:,44),wf(:,71)) * den(51)
  A(68) = cont_VV(wf(:,7),wf(:,80)) * den(48)
  A(69) = cont_VV(wf(:,20),wf(:,79)) * den(54)
  A(70) = cont_VV(wf(:,20),wf(:,80)) * den(55)
  A(71) = cont_VV(wf(:,41),wf(:,78)) * den(52)
  A(72) = cont_VV(wf(:,44),wf(:,78)) * den(53)
  A(73) = cont_VV(wf(:,7),wf(:,81)) * den(70)
  A(74) = cont_VV(wf(:,27),wf(:,82)) * den(71)
  A(75) = cont_VV(wf(:,27),wf(:,85)) * den(43)
  A(76) = cont_VV(wf(:,7),wf(:,86)) * den(46)
  A(77) = cont_VV(wf(:,37),wf(:,82)) * den(53)
  A(78) = cont_VV(wf(:,37),wf(:,85)) * den(52)
  A(79) = cont_VV(wf(:,20),wf(:,81)) * den(55)
  A(80) = cont_VV(wf(:,20),wf(:,86)) * den(54)
  A(81) = cont_QA(wf(:,25),wf(:,88)) * den(26)
  A(82) = cont_VV(wf(:,27),wf(:,89)) * den(29)
  A(83) = cont_QA(wf(:,10),wf(:,91)) * den(31)
  A(84) = cont_QA(wf(:,32),wf(:,92)) * den(34)
  A(85) = cont_VV(wf(:,37),wf(:,89)) * den(39)
  A(86) = cont_QA(wf(:,35),wf(:,94)) * den(37)
  A(87) = cont_QA(wf(:,21),wf(:,91)) * den(40)
  A(88) = cont_QA(wf(:,38),wf(:,94)) * den(41)
  A(89) = cont_QA(wf(:,5),wf(:,96)) * den(5)
  A(90) = cont_VV(wf(:,7),wf(:,97)) * den(8)
  A(91) = cont_QA(wf(:,10),wf(:,99)) * den(11)
  A(92) = cont_QA(wf(:,15),wf(:,100)) * den(15)
  A(93) = cont_VV(wf(:,20),wf(:,97)) * den(20)
  A(94) = cont_QA(wf(:,18),wf(:,102)) * den(18)
  A(95) = cont_QA(wf(:,21),wf(:,99)) * den(21)
  A(96) = cont_QA(wf(:,22),wf(:,102)) * den(22)
  A(97) = cont_QA(wf(:,103),wf(:,104)) * den(74)
  A(98) = cont_VV(wf(:,8),wf(:,105)) * den(75)
  A(99) = cont_QA(wf(:,15),wf(:,108)) * den(15)
  A(100) = cont_QA(wf(:,103),wf(:,109)) * den(77)
  A(101) = cont_QA(wf(:,11),wf(:,110)) * den(21)
  A(102) = cont_QA(wf(:,5),wf(:,113)) * den(5)
  A(103) = cont_VV(wf(:,8),wf(:,114)) * den(8)
  A(104) = cont_QA(wf(:,11),wf(:,115)) * den(11)
  A(105) = cont_QA(wf(:,116),wf(:,117)) * den(80)
  A(106) = cont_VV(wf(:,28),wf(:,118)) * den(81)
  A(107) = cont_QA(wf(:,32),wf(:,119)) * den(34)
  A(108) = cont_QA(wf(:,116),wf(:,120)) * den(83)
  A(109) = cont_QA(wf(:,30),wf(:,110)) * den(40)
  A(110) = cont_QA(wf(:,25),wf(:,121)) * den(26)
  A(111) = cont_VV(wf(:,28),wf(:,122)) * den(29)
  A(112) = cont_QA(wf(:,30),wf(:,115)) * den(31)
  A(113) = cont_VV(wf(:,41),wf(:,118)) * den(84)
  A(114) = cont_VV(wf(:,44),wf(:,118)) * den(85)
  A(115) = cont_VV(wf(:,42),wf(:,105)) * den(86)
  A(116) = cont_VV(wf(:,43),wf(:,105)) * den(87)
  A(117) = cont_VV(wf(:,41),wf(:,122)) * den(43)
  A(118) = cont_VV(wf(:,42),wf(:,114)) * den(46)
  A(119) = cont_VV(wf(:,43),wf(:,114)) * den(48)
  A(120) = cont_VV(wf(:,44),wf(:,122)) * den(51)
  A(121) = cont_QA(wf(:,124),wf(:,125)) * den(91)
  A(122) = cont_VV(wf(:,126),wf(:,127)) * den(95)
  A(123) = cont_QA(wf(:,128),wf(:,129)) * den(98)
  A(124) = cont_VV(wf(:,8),wf(:,131)) * den(100)
  A(125) = cont_QA(wf(:,11),wf(:,132)) * den(101)
  A(126) = cont_QA(wf(:,53),wf(:,133)) * den(103)
  A(127) = cont_VV(wf(:,7),wf(:,134)) * den(104)
  A(128) = cont_VV(wf(:,8),wf(:,135)) * den(105)
  A(129) = cont_QA(wf(:,53),wf(:,136)) * den(106)
  A(130) = cont_QA(wf(:,138),wf(:,139)) * den(110)
  A(131) = cont_VV(wf(:,127),wf(:,140)) * den(113)
  A(132) = cont_QA(wf(:,141),wf(:,142)) * den(116)
  A(133) = cont_QA(wf(:,104),wf(:,143)) * den(117)
  A(134) = cont_VV(wf(:,8),wf(:,145)) * den(119)
  A(135) = cont_QA(wf(:,104),wf(:,146)) * den(120)
  A(136) = cont_VV(wf(:,8),wf(:,147)) * den(121)
  A(137) = cont_VV(wf(:,20),wf(:,134)) * den(122)
  A(138) = cont_QA(wf(:,19),wf(:,148)) * den(123)
  A(139) = cont_QA(wf(:,11),wf(:,150)) * den(125)
  A(140) = cont_QA(wf(:,57),wf(:,133)) * den(126)
  A(141) = cont_QA(wf(:,57),wf(:,136)) * den(127)
  A(142) = cont_QA(wf(:,109),wf(:,143)) * den(128)
  A(143) = cont_QA(wf(:,109),wf(:,146)) * den(129)
  A(144) = cont_QA(wf(:,19),wf(:,152)) * den(131)
  A(145) = cont_QA(wf(:,124),wf(:,154)) * den(134)
  A(146) = cont_VV(wf(:,155),wf(:,156)) * den(138)
  A(147) = cont_QA(wf(:,157),wf(:,158)) * den(141)
  A(148) = cont_VV(wf(:,28),wf(:,159)) * den(142)
  A(149) = cont_QA(wf(:,30),wf(:,132)) * den(143)
  A(150) = cont_QA(wf(:,53),wf(:,160)) * den(145)
  A(151) = cont_VV(wf(:,27),wf(:,161)) * den(146)
  A(152) = cont_VV(wf(:,28),wf(:,162)) * den(147)
  A(153) = cont_QA(wf(:,53),wf(:,163)) * den(148)
  A(154) = cont_QA(wf(:,138),wf(:,165)) * den(151)
  A(155) = cont_VV(wf(:,156),wf(:,166)) * den(154)
  A(156) = cont_QA(wf(:,167),wf(:,168)) * den(157)
  A(157) = cont_QA(wf(:,117),wf(:,169)) * den(158)
  A(158) = cont_VV(wf(:,28),wf(:,171)) * den(160)
  A(159) = cont_QA(wf(:,117),wf(:,172)) * den(161)
  A(160) = cont_VV(wf(:,28),wf(:,173)) * den(162)
  A(161) = cont_VV(wf(:,37),wf(:,161)) * den(163)
  A(162) = cont_QA(wf(:,36),wf(:,174)) * den(164)
  A(163) = cont_QA(wf(:,30),wf(:,150)) * den(165)
  A(164) = cont_QA(wf(:,57),wf(:,160)) * den(166)
  A(165) = cont_QA(wf(:,57),wf(:,163)) * den(167)
  A(166) = cont_QA(wf(:,120),wf(:,169)) * den(168)
  A(167) = cont_QA(wf(:,120),wf(:,172)) * den(169)
  A(168) = cont_QA(wf(:,36),wf(:,176)) * den(171)
  A(169) = cont_VV(wf(:,41),wf(:,159)) * den(172)
  A(170) = cont_VV(wf(:,42),wf(:,131)) * den(173)
  A(171) = cont_VV(wf(:,7),wf(:,179)) * den(175)
  A(172) = cont_VV(wf(:,27),wf(:,180)) * den(176)
  A(173) = cont_VV(wf(:,41),wf(:,162)) * den(177)
  A(174) = cont_VV(wf(:,7),wf(:,181)) * den(178)
  A(175) = cont_VV(wf(:,43),wf(:,131)) * den(179)
  A(176) = cont_VV(wf(:,7),wf(:,182)) * den(180)
  A(177) = cont_VV(wf(:,7),wf(:,183)) * den(181)
  A(178) = cont_VV(wf(:,44),wf(:,159)) * den(182)
  A(179) = cont_VV(wf(:,27),wf(:,184)) * den(183)
  A(180) = cont_VV(wf(:,27),wf(:,185)) * den(184)
  A(181) = cont_VV(wf(:,41),wf(:,171)) * den(185)
  A(182) = cont_VV(wf(:,41),wf(:,173)) * den(186)
  A(183) = cont_VV(wf(:,37),wf(:,180)) * den(187)
  A(184) = cont_VV(wf(:,44),wf(:,173)) * den(188)
  A(185) = cont_VV(wf(:,44),wf(:,171)) * den(189)
  A(186) = cont_VV(wf(:,37),wf(:,185)) * den(190)
  A(187) = cont_VV(wf(:,42),wf(:,147)) * den(191)
  A(188) = cont_VV(wf(:,42),wf(:,145)) * den(192)
  A(189) = cont_VV(wf(:,20),wf(:,179)) * den(193)
  A(190) = cont_VV(wf(:,43),wf(:,147)) * den(194)
  A(191) = cont_VV(wf(:,43),wf(:,145)) * den(195)
  A(192) = cont_VV(wf(:,20),wf(:,182)) * den(196)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(192)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((-A(9)-A(11)-A(12)-A(13)-A(15)-A(16))*f(1))/6._/**/REALKIND+((-A(3)-A(4)-A(7)-A(19)-A(24))*f(1))/2._/**/REALKIND+(CI*( &
       -A(2)-A(6))*f(2))/2._/**/REALKIND
  M1(2) = ((A(9)+A(13)+A(16)+A(17)+A(21))*f(1))/2._/**/REALKIND+((A(18)+A(19)+A(23)+A(24))*f(1))/6._/**/REALKIND+(CI*(-A(10) &
       -A(14))*f(2))/2._/**/REALKIND
  M1(3) = ((A(1)+A(3)+A(4)+A(5)+A(7)+A(8))*f(1))/6._/**/REALKIND+((A(11)+A(12)+A(15)+A(20)+A(22))*f(1))/2._/**/REALKIND+(CI*(A(10) &
       +A(14))*f(2))/2._/**/REALKIND
  M1(4) = ((-A(17)-A(20)-A(21)-A(22))*f(1))/6._/**/REALKIND+((-A(1)-A(5)-A(8)-A(18)-A(23))*f(1))/2._/**/REALKIND+(CI*(A(2) &
       +A(6))*f(2))/2._/**/REALKIND

  M2(1) = ((A(145)+A(146)+A(147)+A(149)+A(150)+A(153)+A(154)+A(155)+A(156)+A(157)+A(159)+A(162)+A(163)+A(164)+A(165)+A(166)+A(167) &
       +A(168))*f(3))/6._/**/REALKIND+((A(125)+A(126)+A(129)+A(130)+A(131)+A(132)+A(139)+A(140)+A(141)+A(175)+A(176)+A(177)+A(190) &
       +A(191)+A(192))*f(3))/2._/**/REALKIND+(CI*(A(124)+A(127)+A(128)+A(134)+A(136)+A(137))*f(4))/2._/**/REALKIND+((-A(41)-A(43) &
       -A(44)-A(45)-A(57)-A(59)-A(60)-A(61)-A(81)-A(83)-A(84)-A(86)-A(87)-A(88)-A(105)-A(107)-A(108)-A(109))*f(5))/6._/**/REALKIND &
       +((-A(27)-A(28)-A(33)-A(36)-A(51)-A(68)-A(70)-A(73)-A(79)-A(91)-A(92)-A(95)-A(99)-A(101)-A(116))*f(5))/2._/**/REALKIND &
       +(CI*(-A(34)-A(90)-A(93)-A(98))*f(6))/2._/**/REALKIND+((-A(47)-A(48)-A(62)-A(63)-A(110)-A(112))*f(7))/6._/**/REALKIND+(( &
       -A(31)-A(38)-A(56)-A(104)-A(119))*f(7))/2._/**/REALKIND+(CI*(-A(40)-A(103))*f(8))/2._/**/REALKIND+(CI*(-A(26) &
       -A(30))*f(9))/2._/**/REALKIND
  M2(2) = ((-A(145)-A(146)-A(147)-A(157)-A(159)-A(162)-A(166)-A(167)-A(168)-A(169)-A(172)-A(173)-A(181)-A(182) &
       -A(183))*f(3))/2._/**/REALKIND+((-A(170)-A(171)-A(174)-A(175)-A(176)-A(177)-A(187)-A(188)-A(189)-A(190)-A(191) &
       -A(192))*f(3))/6._/**/REALKIND+(CI*(A(148)+A(151)+A(152)+A(158)+A(160)+A(161))*f(4))/2._/**/REALKIND+((A(41)+A(45)+A(49) &
       +A(53)+A(59)+A(61)+A(66)+A(75)+A(78)+A(81)+A(86)+A(88)+A(105)+A(108)+A(113))*f(5))/2._/**/REALKIND+((A(50)+A(51)+A(65) &
       +A(68)+A(69)+A(70)+A(73)+A(76)+A(79)+A(80)+A(115)+A(116))*f(5))/6._/**/REALKIND+(CI*(-A(58)-A(82)-A(85) &
       -A(106))*f(6))/2._/**/REALKIND+((A(48)+A(63)+A(71)+A(110)+A(117))*f(7))/2._/**/REALKIND+((A(55)+A(56)+A(118) &
       +A(119))*f(7))/6._/**/REALKIND+(CI*(-A(64)-A(111))*f(8))/2._/**/REALKIND+(CI*(-A(42)-A(46))*f(9))/2._/**/REALKIND
  M2(3) = ((-A(121)-A(122)-A(123)-A(125)-A(126)-A(129)-A(130)-A(131)-A(132)-A(133)-A(135)-A(138)-A(139)-A(140)-A(141)-A(142) &
       -A(143)-A(144))*f(3))/6._/**/REALKIND+((-A(149)-A(150)-A(153)-A(154)-A(155)-A(156)-A(163)-A(164)-A(165)-A(178)-A(179) &
       -A(180)-A(184)-A(185)-A(186))*f(3))/2._/**/REALKIND+(CI*(-A(148)-A(151)-A(152)-A(158)-A(160)-A(161))*f(4))/2._/**/REALKIND &
       +((A(25)+A(27)+A(28)+A(29)+A(33)+A(35)+A(36)+A(37)+A(89)+A(91)+A(92)+A(94)+A(95)+A(96)+A(97)+A(99)+A(100) &
       +A(101))*f(5))/6._/**/REALKIND+((A(43)+A(44)+A(52)+A(54)+A(57)+A(60)+A(67)+A(74)+A(77)+A(83)+A(84)+A(87)+A(107)+A(109) &
       +A(114))*f(5))/2._/**/REALKIND+(CI*(A(58)+A(82)+A(85)+A(106))*f(6))/2._/**/REALKIND+((A(31)+A(32)+A(38)+A(39)+A(102) &
       +A(104))*f(7))/6._/**/REALKIND+((A(47)+A(62)+A(72)+A(112)+A(120))*f(7))/2._/**/REALKIND+(CI*(A(64) &
       +A(111))*f(8))/2._/**/REALKIND+(CI*(A(42)+A(46))*f(9))/2._/**/REALKIND
  M2(4) = ((A(169)+A(172)+A(173)+A(178)+A(179)+A(180)+A(181)+A(182)+A(183)+A(184)+A(185)+A(186))*f(3))/6._/**/REALKIND+((A(121) &
       +A(122)+A(123)+A(133)+A(135)+A(138)+A(142)+A(143)+A(144)+A(170)+A(171)+A(174)+A(187)+A(188)+A(189))*f(3))/2._/**/REALKIND &
       +(CI*(-A(124)-A(127)-A(128)-A(134)-A(136)-A(137))*f(4))/2._/**/REALKIND+((-A(49)-A(52)-A(53)-A(54)-A(66)-A(67)-A(74)-A(75) &
       -A(77)-A(78)-A(113)-A(114))*f(5))/6._/**/REALKIND+((-A(25)-A(29)-A(35)-A(37)-A(50)-A(65)-A(69)-A(76)-A(80)-A(89)-A(94) &
       -A(96)-A(97)-A(100)-A(115))*f(5))/2._/**/REALKIND+(CI*(A(34)+A(90)+A(93)+A(98))*f(6))/2._/**/REALKIND+((-A(32)-A(39)-A(55) &
       -A(102)-A(118))*f(7))/2._/**/REALKIND+((-A(71)-A(72)-A(117)-A(120))*f(7))/6._/**/REALKIND+(CI*(A(40) &
       +A(103))*f(8))/2._/**/REALKIND+(CI*(A(26)+A(30))*f(9))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwjjj_uddxdxwg_1_/**/REALKIND
