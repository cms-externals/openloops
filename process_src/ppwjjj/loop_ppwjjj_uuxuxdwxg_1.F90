
module ol_colourmatrix_ppwjjj_uuxuxdwxg_1_/**/REALKIND
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
  K1(17,:) = [   0, -16,   2,  -6]
  K1(18,:) = [ -16, -48,   0, -16]
  K1(19,:) = [   2,   0,   6,   2]
  K1(20,:) = [  -6, -16,   2,   0]
  K1(21,:) = [   0,  -2,  16,   6]
  K1(22,:) = [  -2,   0,   6,  16]
  K1(23,:) = [  16,   6,   0,  -2]
  K1(24,:) = [   6,  16,  -2,   0]
  K1(25,:) = [  48,  16,  16,   0]
  K1(26,:) = [  16,  48,   0,  16]
  K1(27,:) = [  16,   0,  48,  16]
  K1(28,:) = [   0,  16,  16,  48]
  K1(29,:) = [   0,  16,  -2,   6]
  K1(30,:) = [  16,   0,   6,  -2]
  K1(31,:) = [  -2,   6,   0,  16]
  K1(32,:) = [   6,  -2,  16,   0]
  K1(33,:) = [   0,   2, -16,  -6]
  K1(34,:) = [   2,   6,   0,   2]
  K1(35,:) = [ -16,   0, -48, -16]
  K1(36,:) = [  -6,   2, -16,   0]
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
  K1(73,:) = [   0,  18, -18,   0]
  K1(74,:) = [  18,   0,   0, -18]
  K1(75,:) = [ -18,   0, -54, -18]
  K1(76,:) = [   0, -18, -18, -54]
  K1(77,:) = [   0, -18,  18,   0]
  K1(78,:) = [ -18, -54,   0, -18]
  K1(79,:) = [  18,   0,   0, -18]
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
end module ol_colourmatrix_ppwjjj_uuxuxdwxg_1_/**/REALKIND



module ol_forced_parameters_ppwjjj_uuxuxdwxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppwjjj_uuxuxdwxg_1_/**/REALKIND

module ol_loop_ppwjjj_uuxuxdwxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(17), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:254)
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
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_AW_Q(wf(:,-2),wf(:,-4),wf(:,2))
  call vert_VQ_A(wf(:,-5),wf(:,-3),wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_AV_Q(wf(:,4),wf(:,1),wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-3),wf(:,7))
  call vert_AV_Q(wf(:,4),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,11),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,-3),wf(:,4),wf(:,10))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,11))
  call vert_AV_Q(wf(:,-2),wf(:,-5),wf(:,12))
  call vert_WQ_A(wf(:,-4),wf(:,-3),wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,36),ZERO,0_intkind1,wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,24),ZERO,0_intkind1,wf(:,15))
  call vert_AV_Q(wf(:,14),wf(:,1),wf(:,16))
  call vert_AV_Q(wf(:,-2),wf(:,1),wf(:,17))
  call vert_VQ_A(wf(:,-5),wf(:,15),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,7),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,15),wf(:,-2),wf(:,20))
  call vert_AW_Q(wf(:,14),wf(:,-4),wf(:,21))
  call vert_WQ_A(wf(:,-4),wf(:,5),wf(:,22))
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,23))
  call vert_AW_Q(wf(:,-1),wf(:,-4),wf(:,24))
  call prop_A_Q(wf(:,24),Q(:,18),ZERO,0_intkind1,wf(:,25))
  call vert_AV_Q(wf(:,25),wf(:,23),wf(:,26))
  call vert_VQ_A(wf(:,23),wf(:,-3),wf(:,27))
  call vert_AV_Q(wf(:,25),wf(:,-5),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,13),ZERO,0_intkind1,wf(:,29))
  call vert_QA_V(wf(:,-3),wf(:,25),wf(:,30))
  call vert_UV_W(wf(:,23),Q(:,5),wf(:,-5),Q(:,32),wf(:,31))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,32))
  call prop_A_Q(wf(:,32),Q(:,34),ZERO,0_intkind1,wf(:,33))
  call vert_AV_Q(wf(:,33),wf(:,23),wf(:,34))
  call vert_AV_Q(wf(:,-1),wf(:,23),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,7),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,15),wf(:,-1),wf(:,37))
  call vert_AW_Q(wf(:,33),wf(:,-4),wf(:,38))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,33),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,40),wf(:,-2),wf(:,41))
  call vert_QA_V(wf(:,0),wf(:,14),wf(:,42))
  call vert_QA_V(wf(:,40),wf(:,-1),wf(:,43))
  call vert_QA_V(wf(:,0),wf(:,33),wf(:,44))
  call counter_AV_Q(wf(:,4),wf(:,1),wf(:,45))
  call counter_AV_Q(wf(:,4),wf(:,-5),wf(:,46))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,47))
  call counter_AV_Q(wf(:,14),wf(:,1),wf(:,48))
  call counter_VQ_A(wf(:,-5),wf(:,15),wf(:,49))
  call counter_AW_Q(wf(:,14),wf(:,-4),wf(:,50))
  call counter_WQ_A(wf(:,-4),wf(:,5),wf(:,51))
  call counter_QA_V(wf(:,-3),wf(:,4),wf(:,52))
  call counter_VQ_A(wf(:,1),wf(:,-3),wf(:,53))
  call prop_A_Q(wf(:,8),Q(:,52),ZERO,0_intkind1,wf(:,54))
  call counter_VQ_A(wf(:,-5),wf(:,-3),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,40),ZERO,0_intkind1,wf(:,56))
  call prop_A_Q(wf(:,21),Q(:,52),ZERO,0_intkind1,wf(:,57))
  call vert_WQ_A(wf(:,-4),wf(:,56),wf(:,58))
  call counter_WQ_A(wf(:,-4),wf(:,-3),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,24),ZERO,0_intkind1,wf(:,60))
  call vert_VQ_A(wf(:,-5),wf(:,60),wf(:,61))
  call vert_QA_V(wf(:,60),wf(:,-2),wf(:,62))
  call counter_QA_V(wf(:,15),wf(:,-2),wf(:,63))
  call counter_AV_Q(wf(:,-2),wf(:,1),wf(:,64))
  call prop_Q_A(wf(:,18),Q(:,56),ZERO,0_intkind1,wf(:,65))
  call counter_AV_Q(wf(:,-2),wf(:,-5),wf(:,66))
  call prop_A_Q(wf(:,66),Q(:,36),ZERO,0_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,67),wf(:,1),wf(:,68))
  call prop_Q_A(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,69))
  call vert_AW_Q(wf(:,67),wf(:,-4),wf(:,70))
  call counter_AW_Q(wf(:,-2),wf(:,-4),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,20),ZERO,0_intkind1,wf(:,72))
  call vert_AV_Q(wf(:,72),wf(:,1),wf(:,73))
  call vert_AV_Q(wf(:,72),wf(:,-5),wf(:,74))
  call vert_QA_V(wf(:,-3),wf(:,72),wf(:,75))
  call counter_AV_Q(wf(:,25),wf(:,23),wf(:,76))
  call counter_AV_Q(wf(:,25),wf(:,-5),wf(:,77))
  call counter_UV_W(wf(:,23),Q(:,5),wf(:,-5),Q(:,32),wf(:,78))
  call counter_AV_Q(wf(:,33),wf(:,23),wf(:,79))
  call counter_AW_Q(wf(:,33),wf(:,-4),wf(:,80))
  call counter_QA_V(wf(:,-3),wf(:,25),wf(:,81))
  call counter_VQ_A(wf(:,23),wf(:,-3),wf(:,82))
  call prop_A_Q(wf(:,28),Q(:,50),ZERO,0_intkind1,wf(:,83))
  call prop_A_Q(wf(:,38),Q(:,50),ZERO,0_intkind1,wf(:,84))
  call vert_QA_V(wf(:,60),wf(:,-1),wf(:,85))
  call counter_QA_V(wf(:,40),wf(:,-2),wf(:,86))
  call vert_QA_V(wf(:,0),wf(:,67),wf(:,87))
  call counter_QA_V(wf(:,15),wf(:,-1),wf(:,88))
  call counter_AV_Q(wf(:,-1),wf(:,23),wf(:,89))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,90))
  call prop_A_Q(wf(:,90),Q(:,34),ZERO,0_intkind1,wf(:,91))
  call vert_AV_Q(wf(:,91),wf(:,23),wf(:,92))
  call vert_AW_Q(wf(:,91),wf(:,-4),wf(:,93))
  call counter_AW_Q(wf(:,-1),wf(:,-4),wf(:,94))
  call prop_A_Q(wf(:,94),Q(:,18),ZERO,0_intkind1,wf(:,95))
  call vert_AV_Q(wf(:,95),wf(:,23),wf(:,96))
  call vert_AV_Q(wf(:,95),wf(:,-5),wf(:,97))
  call vert_QA_V(wf(:,-3),wf(:,95),wf(:,98))
  call counter_QA_V(wf(:,40),wf(:,-1),wf(:,99))
  call vert_QA_V(wf(:,0),wf(:,91),wf(:,100))
  call counter_QA_V(wf(:,0),wf(:,14),wf(:,101))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,102))
  call prop_Q_A(wf(:,102),Q(:,33),ZERO,0_intkind1,wf(:,103))
  call vert_QA_V(wf(:,103),wf(:,-2),wf(:,104))
  call counter_QA_V(wf(:,0),wf(:,33),wf(:,105))
  call vert_QA_V(wf(:,103),wf(:,-1),wf(:,106))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,107))
  call vert_AV_Q(wf(:,25),wf(:,107),wf(:,108))
  call vert_VQ_A(wf(:,107),wf(:,-3),wf(:,109))
  call prop_Q_A(wf(:,109),Q(:,13),ZERO,0_intkind1,wf(:,110))
  call vert_UV_W(wf(:,107),Q(:,5),wf(:,-5),Q(:,32),wf(:,111))
  call vert_AV_Q(wf(:,33),wf(:,107),wf(:,112))
  call vert_AV_Q(wf(:,-1),wf(:,107),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,7),ZERO,0_intkind1,wf(:,114))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,115))
  call vert_AV_Q(wf(:,4),wf(:,115),wf(:,116))
  call vert_VQ_A(wf(:,115),wf(:,-3),wf(:,117))
  call prop_Q_A(wf(:,117),Q(:,11),ZERO,0_intkind1,wf(:,118))
  call vert_UV_W(wf(:,115),Q(:,3),wf(:,-5),Q(:,32),wf(:,119))
  call vert_AV_Q(wf(:,14),wf(:,115),wf(:,120))
  call vert_AV_Q(wf(:,-2),wf(:,115),wf(:,121))
  call prop_A_Q(wf(:,121),Q(:,7),ZERO,0_intkind1,wf(:,122))
  call vert_QA_V(wf(:,5),wf(:,4),wf(:,123))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,124))
  call vert_VQ_A(wf(:,1),wf(:,5),wf(:,125))
  call counter_A_Q(ctqq,wf(:,4),Q(:,20),wf(:,126))
  call prop_Q_A(wf(:,125),Q(:,43),ZERO,0_intkind1,wf(:,127))
  call counter_Q_A(ctqq,wf(:,5),Q(:,40),wf(:,128))
  call prop_A_Q(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,129))
  call vert_VQ_A(wf(:,124),wf(:,-3),wf(:,130))
  call vert_UV_W(wf(:,124),Q(:,3),wf(:,-5),Q(:,32),wf(:,131))
  call prop_A_Q(wf(:,126),Q(:,20),ZERO,0_intkind1,wf(:,132))
  call vert_QA_V(wf(:,-3),wf(:,132),wf(:,133))
  call vert_AV_Q(wf(:,132),wf(:,-5),wf(:,134))
  call counter_Q_A(ctqq,wf(:,9),Q(:,11),wf(:,135))
  call counter_V_V(ctGG,wf(:,10),Q(:,28),wf(:,136))
  call vert_QA_V(wf(:,15),wf(:,14),wf(:,137))
  call vert_VQ_A(wf(:,1),wf(:,15),wf(:,138))
  call counter_A_Q(ctqq,wf(:,14),Q(:,36),wf(:,139))
  call prop_Q_A(wf(:,138),Q(:,27),ZERO,0_intkind1,wf(:,140))
  call counter_Q_A(ctqq,wf(:,15),Q(:,24),wf(:,141))
  call prop_A_Q(wf(:,16),Q(:,39),ZERO,0_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,-2),wf(:,124),wf(:,143))
  call prop_Q_A(wf(:,141),Q(:,24),ZERO,0_intkind1,wf(:,144))
  call vert_QA_V(wf(:,144),wf(:,-2),wf(:,145))
  call counter_A_Q(ctqq,wf(:,19),Q(:,7),wf(:,146))
  call counter_V_V(ctGG,wf(:,20),Q(:,28),wf(:,147))
  call vert_VQ_A(wf(:,-5),wf(:,144),wf(:,148))
  call prop_A_Q(wf(:,139),Q(:,36),ZERO,0_intkind1,wf(:,149))
  call vert_AW_Q(wf(:,149),wf(:,-4),wf(:,150))
  call prop_Q_A(wf(:,128),Q(:,40),ZERO,0_intkind1,wf(:,151))
  call vert_WQ_A(wf(:,-4),wf(:,151),wf(:,152))
  call vert_QA_V(wf(:,5),wf(:,25),wf(:,153))
  call counter_V_V(ctGG,wf(:,23),Q(:,5),wf(:,154))
  call vert_VQ_A(wf(:,23),wf(:,5),wf(:,155))
  call counter_A_Q(ctqq,wf(:,25),Q(:,18),wf(:,156))
  call prop_Q_A(wf(:,155),Q(:,45),ZERO,0_intkind1,wf(:,157))
  call prop_A_Q(wf(:,26),Q(:,23),ZERO,0_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,154),wf(:,-3),wf(:,159))
  call vert_UV_W(wf(:,154),Q(:,5),wf(:,-5),Q(:,32),wf(:,160))
  call prop_A_Q(wf(:,156),Q(:,18),ZERO,0_intkind1,wf(:,161))
  call vert_QA_V(wf(:,-3),wf(:,161),wf(:,162))
  call vert_AV_Q(wf(:,161),wf(:,-5),wf(:,163))
  call counter_Q_A(ctqq,wf(:,29),Q(:,13),wf(:,164))
  call counter_V_V(ctGG,wf(:,30),Q(:,26),wf(:,165))
  call vert_QA_V(wf(:,15),wf(:,33),wf(:,166))
  call vert_VQ_A(wf(:,23),wf(:,15),wf(:,167))
  call counter_A_Q(ctqq,wf(:,33),Q(:,34),wf(:,168))
  call prop_Q_A(wf(:,167),Q(:,29),ZERO,0_intkind1,wf(:,169))
  call prop_A_Q(wf(:,34),Q(:,39),ZERO,0_intkind1,wf(:,170))
  call vert_AV_Q(wf(:,-1),wf(:,154),wf(:,171))
  call vert_QA_V(wf(:,144),wf(:,-1),wf(:,172))
  call counter_A_Q(ctqq,wf(:,36),Q(:,7),wf(:,173))
  call counter_V_V(ctGG,wf(:,37),Q(:,26),wf(:,174))
  call prop_A_Q(wf(:,168),Q(:,34),ZERO,0_intkind1,wf(:,175))
  call vert_AW_Q(wf(:,175),wf(:,-4),wf(:,176))
  call counter_Q_A(ctqq,wf(:,40),Q(:,33),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,33),ZERO,0_intkind1,wf(:,178))
  call vert_QA_V(wf(:,178),wf(:,-2),wf(:,179))
  call counter_V_V(ctGG,wf(:,41),Q(:,37),wf(:,180))
  call vert_QA_V(wf(:,0),wf(:,149),wf(:,181))
  call counter_V_V(ctGG,wf(:,42),Q(:,37),wf(:,182))
  call vert_QA_V(wf(:,178),wf(:,-1),wf(:,183))
  call counter_V_V(ctGG,wf(:,43),Q(:,35),wf(:,184))
  call vert_QA_V(wf(:,0),wf(:,175),wf(:,185))
  call counter_V_V(ctGG,wf(:,44),Q(:,35),wf(:,186))
  call vert_AW_Q(wf(:,19),wf(:,-4),wf(:,187))
  call prop_A_Q(wf(:,187),Q(:,23),ZERO,0_intkind1,wf(:,188))
  call vert_AV_Q(wf(:,19),wf(:,-5),wf(:,189))
  call prop_A_Q(wf(:,189),Q(:,39),ZERO,0_intkind1,wf(:,190))
  call vert_WQ_A(wf(:,-4),wf(:,9),wf(:,191))
  call prop_Q_A(wf(:,191),Q(:,27),ZERO,0_intkind1,wf(:,192))
  call vert_VQ_A(wf(:,-5),wf(:,9),wf(:,193))
  call prop_Q_A(wf(:,193),Q(:,43),ZERO,0_intkind1,wf(:,194))
  call vert_AV_Q(wf(:,-2),wf(:,11),wf(:,195))
  call prop_A_Q(wf(:,195),Q(:,39),ZERO,0_intkind1,wf(:,196))
  call vert_VQ_A(wf(:,11),wf(:,-3),wf(:,197))
  call prop_Q_A(wf(:,197),Q(:,43),ZERO,0_intkind1,wf(:,198))
  call vert_AW_Q(wf(:,36),wf(:,-4),wf(:,199))
  call prop_A_Q(wf(:,199),Q(:,23),ZERO,0_intkind1,wf(:,200))
  call vert_AV_Q(wf(:,36),wf(:,-5),wf(:,201))
  call prop_A_Q(wf(:,201),Q(:,39),ZERO,0_intkind1,wf(:,202))
  call vert_WQ_A(wf(:,-4),wf(:,29),wf(:,203))
  call prop_Q_A(wf(:,203),Q(:,29),ZERO,0_intkind1,wf(:,204))
  call vert_VQ_A(wf(:,-5),wf(:,29),wf(:,205))
  call prop_Q_A(wf(:,205),Q(:,45),ZERO,0_intkind1,wf(:,206))
  call vert_AV_Q(wf(:,-1),wf(:,31),wf(:,207))
  call prop_A_Q(wf(:,207),Q(:,39),ZERO,0_intkind1,wf(:,208))
  call vert_VQ_A(wf(:,31),wf(:,-3),wf(:,209))
  call prop_Q_A(wf(:,209),Q(:,45),ZERO,0_intkind1,wf(:,210))
  call vert_VQ_A(wf(:,30),wf(:,0),wf(:,211))
  call prop_Q_A(wf(:,211),Q(:,27),ZERO,0_intkind1,wf(:,212))
  call vert_AV_Q(wf(:,-2),wf(:,30),wf(:,213))
  call prop_A_Q(wf(:,213),Q(:,30),ZERO,0_intkind1,wf(:,214))
  call vert_UV_W(wf(:,30),Q(:,26),wf(:,-5),Q(:,32),wf(:,215))
  call vert_QA_V(wf(:,-3),wf(:,83),wf(:,216))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,217))
  call prop_Q_A(wf(:,217),Q(:,29),ZERO,0_intkind1,wf(:,218))
  call vert_AV_Q(wf(:,-1),wf(:,10),wf(:,219))
  call prop_A_Q(wf(:,219),Q(:,30),ZERO,0_intkind1,wf(:,220))
  call vert_UV_W(wf(:,10),Q(:,28),wf(:,-5),Q(:,32),wf(:,221))
  call vert_QA_V(wf(:,-3),wf(:,54),wf(:,222))
  call vert_VQ_A(wf(:,37),wf(:,0),wf(:,223))
  call prop_Q_A(wf(:,223),Q(:,27),ZERO,0_intkind1,wf(:,224))
  call vert_VQ_A(wf(:,20),wf(:,0),wf(:,225))
  call prop_Q_A(wf(:,225),Q(:,29),ZERO,0_intkind1,wf(:,226))
  call vert_AV_Q(wf(:,-2),wf(:,37),wf(:,227))
  call prop_A_Q(wf(:,227),Q(:,30),ZERO,0_intkind1,wf(:,228))
  call vert_AV_Q(wf(:,-1),wf(:,20),wf(:,229))
  call prop_A_Q(wf(:,229),Q(:,30),ZERO,0_intkind1,wf(:,230))
  call vert_UV_W(wf(:,37),Q(:,26),wf(:,-5),Q(:,32),wf(:,231))
  call vert_QA_V(wf(:,65),wf(:,-1),wf(:,232))
  call vert_UV_W(wf(:,20),Q(:,28),wf(:,-5),Q(:,32),wf(:,233))
  call vert_QA_V(wf(:,65),wf(:,-2),wf(:,234))
  call vert_AV_Q(wf(:,-2),wf(:,43),wf(:,235))
  call prop_A_Q(wf(:,235),Q(:,39),ZERO,0_intkind1,wf(:,236))
  call vert_VQ_A(wf(:,43),wf(:,-3),wf(:,237))
  call prop_Q_A(wf(:,237),Q(:,43),ZERO,0_intkind1,wf(:,238))
  call vert_AV_Q(wf(:,-1),wf(:,41),wf(:,239))
  call prop_A_Q(wf(:,239),Q(:,39),ZERO,0_intkind1,wf(:,240))
  call vert_VQ_A(wf(:,41),wf(:,-3),wf(:,241))
  call prop_Q_A(wf(:,241),Q(:,45),ZERO,0_intkind1,wf(:,242))
  call vert_AV_Q(wf(:,-2),wf(:,44),wf(:,243))
  call prop_A_Q(wf(:,243),Q(:,39),ZERO,0_intkind1,wf(:,244))
  call vert_VQ_A(wf(:,44),wf(:,-3),wf(:,245))
  call prop_Q_A(wf(:,245),Q(:,43),ZERO,0_intkind1,wf(:,246))
  call vert_QA_V(wf(:,-3),wf(:,84),wf(:,247))
  call vert_AV_Q(wf(:,-1),wf(:,42),wf(:,248))
  call prop_A_Q(wf(:,248),Q(:,39),ZERO,0_intkind1,wf(:,249))
  call vert_VQ_A(wf(:,42),wf(:,-3),wf(:,250))
  call prop_Q_A(wf(:,250),Q(:,45),ZERO,0_intkind1,wf(:,251))
  call vert_QA_V(wf(:,-3),wf(:,57),wf(:,252))
  call vert_QA_V(wf(:,69),wf(:,-1),wf(:,253))
  call vert_QA_V(wf(:,69),wf(:,-2),wf(:,254))

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
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,11))
  den(9) = 1 / (Q(5,28))
  den(12) = 1 / (Q(5,36))
  den(13) = 1 / (Q(5,24))
  den(16) = 1 / (Q(5,7))
  den(23) = 1 / (Q(5,5))
  den(24) = 1 / (Q(5,18))
  den(27) = 1 / (Q(5,13))
  den(30) = 1 / (Q(5,26))
  den(33) = 1 / (Q(5,34))
  den(42) = 1 / (Q(5,33))
  den(43) = 1 / (Q(5,37))
  den(48) = 1 / (Q(5,35))
  den(59) = 1 / (Q(5,52))
  den(65) = 1 / (Q(5,56))
  den(72) = 1 / (Q(5,50))
  den(89) = 1 / (Q(5,60))
  den(93) = 1 / (Q(5,43))
  den(96) = 1 / (Q(5,23))
  den(111) = 1 / (Q(5,27))
  den(114) = 1 / (Q(5,39))
  den(133) = 1 / (Q(5,58))
  den(137) = 1 / (Q(5,45))
  den(154) = 1 / (Q(5,29))
  den(210) = 1 / (Q(5,30))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(1)*den(12)
  den(15) = den(13)*den(14)
  den(17) = den(1)*den(16)
  den(18) = den(13)*den(17)
  den(19) = den(9)*den(13)
  den(20) = den(1)*den(19)
  den(21) = den(7)*den(12)
  den(22) = den(3)*den(17)
  den(25) = den(23)*den(24)
  den(26) = den(3)*den(25)
  den(28) = den(23)*den(27)
  den(29) = den(24)*den(28)
  den(31) = den(24)*den(30)
  den(32) = den(23)*den(31)
  den(34) = den(23)*den(33)
  den(35) = den(13)*den(34)
  den(36) = den(16)*den(23)
  den(37) = den(13)*den(36)
  den(38) = den(13)*den(30)
  den(39) = den(23)*den(38)
  den(40) = den(28)*den(33)
  den(41) = den(3)*den(36)
  den(44) = den(42)*den(43)
  den(45) = den(24)*den(44)
  den(46) = den(12)*den(43)
  den(47) = den(24)*den(46)
  den(49) = den(42)*den(48)
  den(50) = den(2)*den(49)
  den(51) = den(33)*den(48)
  den(52) = den(2)*den(51)
  den(53) = den(13)*den(49)
  den(54) = den(38)*den(42)
  den(55) = den(13)*den(51)
  den(56) = den(13)*den(46)
  den(57) = den(1)*den(48)
  den(58) = den(2)*den(57)
  den(60) = den(2)*den(59)
  den(61) = den(1)*den(60)
  den(62) = den(12)*den(59)
  den(63) = den(1)*den(62)
  den(64) = den(13)*den(57)
  den(66) = den(13)*den(65)
  den(67) = den(1)*den(66)
  den(68) = den(3)*den(65)
  den(69) = den(1)*den(68)
  den(70) = den(23)*den(43)
  den(71) = den(24)*den(70)
  den(73) = den(24)*den(72)
  den(74) = den(23)*den(73)
  den(75) = den(33)*den(72)
  den(76) = den(23)*den(75)
  den(77) = den(31)*den(42)
  den(78) = den(13)*den(70)
  den(79) = den(23)*den(66)
  den(80) = den(23)*den(68)
  den(81) = den(10)*den(42)
  den(82) = den(13)*den(44)
  den(83) = den(19)*den(42)
  den(84) = den(12)*den(31)
  den(85) = den(10)*den(33)
  den(86) = den(19)*den(33)
  den(87) = den(12)*den(38)
  den(88) = den(2)*den(3)
  den(90) = den(88)*den(89)
  den(91) = den(1)*den(90)
  den(92) = den(1)*den(3)
  den(94) = den(92)*den(93)
  den(95) = den(2)*den(94)
  den(97) = den(4)*den(96)
  den(98) = den(3)*den(97)
  den(99) = den(1)**2
  den(100) = den(60)*den(99)
  den(101) = den(10)*den(99)
  den(102) = den(2)**2
  den(103) = den(57)*den(102)
  den(104) = den(7)*den(102)
  den(105) = den(7)*den(60)
  den(106) = den(10)*den(57)
  den(107) = den(12)*den(13)
  den(108) = den(89)*den(107)
  den(109) = den(1)*den(108)
  den(110) = den(1)*den(13)
  den(112) = den(110)*den(111)
  den(113) = den(12)*den(112)
  den(115) = den(14)*den(114)
  den(116) = den(13)*den(115)
  den(117) = den(66)*den(99)
  den(118) = den(19)*den(99)
  den(119) = den(13)**2
  den(120) = den(57)*den(119)
  den(121) = den(17)*den(66)
  den(122) = den(19)*den(57)
  den(123) = den(17)*den(119)
  den(124) = den(62)*den(99)
  den(125) = den(12)**2
  den(126) = den(7)*den(125)
  den(127) = den(7)*den(62)
  den(128) = den(68)*den(99)
  den(129) = den(17)*den(68)
  den(130) = den(3)**2
  den(131) = den(17)*den(130)
  den(132) = den(3)*den(24)
  den(134) = den(132)*den(133)
  den(135) = den(23)*den(134)
  den(136) = den(3)*den(23)
  den(138) = den(136)*den(137)
  den(139) = den(24)*den(138)
  den(140) = den(25)*den(96)
  den(141) = den(3)*den(140)
  den(142) = den(23)**2
  den(143) = den(73)*den(142)
  den(144) = den(31)*den(142)
  den(145) = den(24)**2
  den(146) = den(70)*den(145)
  den(147) = den(28)*den(145)
  den(148) = den(28)*den(73)
  den(149) = den(31)*den(70)
  den(150) = den(13)*den(33)
  den(151) = den(133)*den(150)
  den(152) = den(23)*den(151)
  den(153) = den(13)*den(23)
  den(155) = den(153)*den(154)
  den(156) = den(33)*den(155)
  den(157) = den(34)*den(114)
  den(158) = den(13)*den(157)
  den(159) = den(66)*den(142)
  den(160) = den(38)*den(142)
  den(161) = den(70)*den(119)
  den(162) = den(36)*den(66)
  den(163) = den(38)*den(70)
  den(164) = den(36)*den(119)
  den(165) = den(75)*den(142)
  den(166) = den(33)**2
  den(167) = den(28)*den(166)
  den(168) = den(28)*den(75)
  den(169) = den(68)*den(142)
  den(170) = den(36)*den(68)
  den(171) = den(36)*den(130)
  den(172) = den(42)**2
  den(173) = den(31)*den(172)
  den(174) = den(44)*den(145)
  den(175) = den(31)*den(44)
  den(176) = den(31)*den(125)
  den(177) = den(31)*den(46)
  den(178) = den(46)*den(145)
  den(179) = den(10)*den(172)
  den(180) = den(10)*den(49)
  den(181) = den(49)*den(102)
  den(182) = den(10)*den(166)
  den(183) = den(10)*den(51)
  den(184) = den(51)*den(102)
  den(185) = den(19)*den(172)
  den(186) = den(38)*den(172)
  den(187) = den(19)*den(49)
  den(188) = den(38)*den(44)
  den(189) = den(44)*den(119)
  den(190) = den(49)*den(119)
  den(191) = den(19)*den(166)
  den(192) = den(19)*den(51)
  den(193) = den(51)*den(119)
  den(194) = den(38)*den(46)
  den(195) = den(38)*den(125)
  den(196) = den(46)*den(119)
  den(197) = den(17)*den(96)
  den(198) = den(17)*den(114)
  den(199) = den(7)*den(111)
  den(200) = den(7)*den(93)
  den(201) = den(57)*den(114)
  den(202) = den(57)*den(93)
  den(203) = den(36)*den(96)
  den(204) = den(36)*den(114)
  den(205) = den(28)*den(154)
  den(206) = den(28)*den(137)
  den(207) = den(70)*den(114)
  den(208) = den(70)*den(137)
  den(209) = den(31)*den(111)
  den(211) = den(31)*den(210)
  den(212) = den(31)*den(133)
  den(213) = den(73)*den(133)
  den(214) = den(10)*den(154)
  den(215) = den(10)*den(210)
  den(216) = den(10)*den(89)
  den(217) = den(60)*den(89)
  den(218) = den(38)*den(111)
  den(219) = den(19)*den(154)
  den(220) = den(38)*den(210)
  den(221) = den(19)*den(210)
  den(222) = den(38)*den(133)
  den(223) = den(66)*den(133)
  den(224) = den(19)*den(89)
  den(225) = den(66)*den(89)
  den(226) = den(49)*den(114)
  den(227) = den(49)*den(93)
  den(228) = den(44)*den(114)
  den(229) = den(44)*den(137)
  den(230) = den(51)*den(114)
  den(231) = den(51)*den(93)
  den(232) = den(75)*den(133)
  den(233) = den(46)*den(114)
  den(234) = den(46)*den(137)
  den(235) = den(62)*den(89)
  den(236) = den(68)*den(133)
  den(237) = den(68)*den(89)
  den(238) = den(1)*den(2)*den(3)
  den(239) = den(1)*den(12)*den(13)
  den(240) = den(3)*den(23)*den(24)
  den(241) = den(13)*den(23)*den(33)
  den(242) = den(24)*den(42)
  den(243) = den(12)*den(24)
  den(244) = den(2)*den(42)
  den(245) = den(2)*den(33)
  den(246) = den(13)*den(42)
  den(247) = den(2)*den(200)
  den(248) = den(2)*den(202)
  den(249) = den(1)*den(216)
  den(250) = den(1)*den(217)
  den(251) = den(13)*den(198)
  den(252) = den(13)*den(201)
  den(253) = den(1)*den(224)
  den(254) = den(1)*den(225)
  den(255) = den(12)*den(199)
  den(256) = den(1)*den(235)
  den(257) = den(3)*den(197)
  den(258) = den(1)*den(237)
  den(259) = den(24)*den(206)
  den(260) = den(24)*den(208)
  den(261) = den(23)*den(212)
  den(262) = den(23)*den(213)
  den(263) = den(13)*den(204)
  den(264) = den(13)*den(207)
  den(265) = den(23)*den(222)
  den(266) = den(23)*den(223)
  den(267) = den(33)*den(205)
  den(268) = den(23)*den(232)
  den(269) = den(3)*den(203)
  den(270) = den(23)*den(236)
  den(271) = den(24)*den(229)
  den(272) = den(42)*den(211)
  den(273) = den(24)*den(234)
  den(274) = den(12)*den(209)
  den(275) = den(2)*den(227)
  den(276) = den(42)*den(215)
  den(277) = den(2)*den(231)
  den(278) = den(33)*den(214)
  den(279) = den(13)*den(226)
  den(280) = den(13)*den(228)
  den(281) = den(42)*den(220)
  den(282) = den(42)*den(221)
  den(283) = den(13)*den(230)
  den(284) = den(33)*den(219)
  den(285) = den(13)*den(233)
  den(286) = den(12)*den(218)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(192)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_VV(wf(:,10),wf(:,11)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(6) = cont_VV(wf(:,11),wf(:,20)) * den(20)
  A(7) = cont_QA(wf(:,9),wf(:,21)) * den(21)
  A(8) = cont_QA(wf(:,19),wf(:,22)) * den(22)
  A(9) = cont_QA(wf(:,5),wf(:,26)) * den(26)
  A(10) = cont_QA(wf(:,28),wf(:,29)) * den(29)
  A(11) = cont_VV(wf(:,30),wf(:,31)) * den(32)
  A(12) = cont_QA(wf(:,15),wf(:,34)) * den(35)
  A(13) = cont_QA(wf(:,18),wf(:,36)) * den(37)
  A(14) = cont_VV(wf(:,31),wf(:,37)) * den(39)
  A(15) = cont_QA(wf(:,29),wf(:,38)) * den(40)
  A(16) = cont_QA(wf(:,22),wf(:,36)) * den(41)
  A(17) = cont_VV(wf(:,30),wf(:,41)) * den(45)
  A(18) = cont_VV(wf(:,30),wf(:,42)) * den(47)
  A(19) = cont_VV(wf(:,10),wf(:,43)) * den(50)
  A(20) = cont_VV(wf(:,10),wf(:,44)) * den(52)
  A(21) = cont_VV(wf(:,20),wf(:,43)) * den(53)
  A(22) = cont_VV(wf(:,37),wf(:,41)) * den(54)
  A(23) = cont_VV(wf(:,20),wf(:,44)) * den(55)
  A(24) = cont_VV(wf(:,37),wf(:,42)) * den(56)

  A(25) = cont_QA(wf(:,5),wf(:,45)) * den(5)
  A(26) = cont_QA(wf(:,9),wf(:,46)) * den(8)
  A(27) = cont_VV(wf(:,10),wf(:,47)) * den(11)
  A(28) = cont_QA(wf(:,15),wf(:,48)) * den(15)
  A(29) = cont_QA(wf(:,19),wf(:,49)) * den(18)
  A(30) = cont_VV(wf(:,20),wf(:,47)) * den(20)
  A(31) = cont_QA(wf(:,9),wf(:,50)) * den(21)
  A(32) = cont_QA(wf(:,19),wf(:,51)) * den(22)
  A(33) = cont_VV(wf(:,11),wf(:,52)) * den(58)
  A(34) = cont_QA(wf(:,53),wf(:,54)) * den(61)
  A(35) = cont_QA(wf(:,6),wf(:,56)) * den(5)
  A(36) = cont_QA(wf(:,53),wf(:,57)) * den(63)
  A(37) = cont_QA(wf(:,19),wf(:,58)) * den(22)
  A(38) = cont_QA(wf(:,16),wf(:,60)) * den(15)
  A(39) = cont_QA(wf(:,19),wf(:,61)) * den(18)
  A(40) = cont_VV(wf(:,11),wf(:,62)) * den(20)
  A(41) = cont_VV(wf(:,11),wf(:,63)) * den(64)
  A(42) = cont_QA(wf(:,64),wf(:,65)) * den(67)
  A(43) = cont_QA(wf(:,15),wf(:,68)) * den(15)
  A(44) = cont_QA(wf(:,64),wf(:,69)) * den(69)
  A(45) = cont_QA(wf(:,9),wf(:,70)) * den(21)
  A(46) = cont_QA(wf(:,5),wf(:,73)) * den(5)
  A(47) = cont_QA(wf(:,9),wf(:,74)) * den(8)
  A(48) = cont_VV(wf(:,11),wf(:,75)) * den(11)
  A(49) = cont_QA(wf(:,5),wf(:,76)) * den(26)
  A(50) = cont_QA(wf(:,29),wf(:,77)) * den(29)
  A(51) = cont_VV(wf(:,30),wf(:,78)) * den(32)
  A(52) = cont_QA(wf(:,15),wf(:,79)) * den(35)
  A(53) = cont_QA(wf(:,36),wf(:,49)) * den(37)
  A(54) = cont_VV(wf(:,37),wf(:,78)) * den(39)
  A(55) = cont_QA(wf(:,29),wf(:,80)) * den(40)
  A(56) = cont_QA(wf(:,36),wf(:,51)) * den(41)
  A(57) = cont_VV(wf(:,31),wf(:,81)) * den(71)
  A(58) = cont_QA(wf(:,82),wf(:,83)) * den(74)
  A(59) = cont_QA(wf(:,26),wf(:,56)) * den(26)
  A(60) = cont_QA(wf(:,82),wf(:,84)) * den(76)
  A(61) = cont_QA(wf(:,36),wf(:,58)) * den(41)
  A(62) = cont_QA(wf(:,34),wf(:,60)) * den(35)
  A(63) = cont_QA(wf(:,36),wf(:,61)) * den(37)
  A(64) = cont_VV(wf(:,31),wf(:,85)) * den(39)
  A(65) = cont_VV(wf(:,41),wf(:,81)) * den(45)
  A(66) = cont_VV(wf(:,42),wf(:,81)) * den(47)
  A(67) = cont_VV(wf(:,43),wf(:,52)) * den(50)
  A(68) = cont_VV(wf(:,44),wf(:,52)) * den(52)
  A(69) = cont_VV(wf(:,43),wf(:,62)) * den(53)
  A(70) = cont_VV(wf(:,41),wf(:,85)) * den(54)
  A(71) = cont_VV(wf(:,44),wf(:,62)) * den(55)
  A(72) = cont_VV(wf(:,42),wf(:,85)) * den(56)
  A(73) = cont_VV(wf(:,30),wf(:,86)) * den(77)
  A(74) = cont_VV(wf(:,30),wf(:,87)) * den(47)
  A(75) = cont_VV(wf(:,43),wf(:,63)) * den(53)
  A(76) = cont_VV(wf(:,37),wf(:,86)) * den(54)
  A(77) = cont_VV(wf(:,44),wf(:,63)) * den(55)
  A(78) = cont_VV(wf(:,37),wf(:,87)) * den(56)
  A(79) = cont_VV(wf(:,43),wf(:,75)) * den(50)
  A(80) = cont_VV(wf(:,44),wf(:,75)) * den(52)
  A(81) = cont_VV(wf(:,31),wf(:,88)) * den(78)
  A(82) = cont_QA(wf(:,65),wf(:,89)) * den(79)
  A(83) = cont_QA(wf(:,15),wf(:,92)) * den(35)
  A(84) = cont_QA(wf(:,69),wf(:,89)) * den(80)
  A(85) = cont_QA(wf(:,29),wf(:,93)) * den(40)
  A(86) = cont_QA(wf(:,5),wf(:,96)) * den(26)
  A(87) = cont_QA(wf(:,29),wf(:,97)) * den(29)
  A(88) = cont_VV(wf(:,31),wf(:,98)) * den(32)
  A(89) = cont_VV(wf(:,10),wf(:,99)) * den(81)
  A(90) = cont_VV(wf(:,10),wf(:,100)) * den(52)
  A(91) = cont_VV(wf(:,41),wf(:,88)) * den(82)
  A(92) = cont_VV(wf(:,20),wf(:,99)) * den(83)
  A(93) = cont_VV(wf(:,42),wf(:,88)) * den(56)
  A(94) = cont_VV(wf(:,20),wf(:,100)) * den(55)
  A(95) = cont_VV(wf(:,41),wf(:,98)) * den(45)
  A(96) = cont_VV(wf(:,42),wf(:,98)) * den(47)
  A(97) = cont_VV(wf(:,30),wf(:,101)) * den(84)
  A(98) = cont_VV(wf(:,30),wf(:,104)) * den(45)
  A(99) = cont_VV(wf(:,10),wf(:,105)) * den(85)
  A(100) = cont_VV(wf(:,10),wf(:,106)) * den(50)
  A(101) = cont_VV(wf(:,20),wf(:,105)) * den(86)
  A(102) = cont_VV(wf(:,37),wf(:,101)) * den(87)
  A(103) = cont_VV(wf(:,20),wf(:,106)) * den(53)
  A(104) = cont_VV(wf(:,37),wf(:,104)) * den(54)
  A(105) = cont_QA(wf(:,5),wf(:,108)) * den(26)
  A(106) = cont_QA(wf(:,28),wf(:,110)) * den(29)
  A(107) = cont_VV(wf(:,30),wf(:,111)) * den(32)
  A(108) = cont_QA(wf(:,15),wf(:,112)) * den(35)
  A(109) = cont_QA(wf(:,18),wf(:,114)) * den(37)
  A(110) = cont_VV(wf(:,37),wf(:,111)) * den(39)
  A(111) = cont_QA(wf(:,38),wf(:,110)) * den(40)
  A(112) = cont_QA(wf(:,22),wf(:,114)) * den(41)
  A(113) = cont_QA(wf(:,5),wf(:,116)) * den(5)
  A(114) = cont_QA(wf(:,8),wf(:,118)) * den(8)
  A(115) = cont_VV(wf(:,10),wf(:,119)) * den(11)
  A(116) = cont_QA(wf(:,15),wf(:,120)) * den(15)
  A(117) = cont_QA(wf(:,18),wf(:,122)) * den(18)
  A(118) = cont_VV(wf(:,20),wf(:,119)) * den(20)
  A(119) = cont_QA(wf(:,21),wf(:,118)) * den(21)
  A(120) = cont_QA(wf(:,22),wf(:,122)) * den(22)
  A(121) = cont_VV(wf(:,123),wf(:,124)) * den(91)
  A(122) = cont_QA(wf(:,126),wf(:,127)) * den(95)
  A(123) = cont_QA(wf(:,128),wf(:,129)) * den(98)
  A(124) = cont_QA(wf(:,54),wf(:,130)) * den(100)
  A(125) = cont_VV(wf(:,10),wf(:,131)) * den(101)
  A(126) = cont_VV(wf(:,11),wf(:,133)) * den(103)
  A(127) = cont_QA(wf(:,9),wf(:,134)) * den(104)
  A(128) = cont_QA(wf(:,54),wf(:,135)) * den(105)
  A(129) = cont_VV(wf(:,11),wf(:,136)) * den(106)
  A(130) = cont_VV(wf(:,124),wf(:,137)) * den(109)
  A(131) = cont_QA(wf(:,139),wf(:,140)) * den(113)
  A(132) = cont_QA(wf(:,141),wf(:,142)) * den(116)
  A(133) = cont_QA(wf(:,65),wf(:,143)) * den(117)
  A(134) = cont_VV(wf(:,20),wf(:,131)) * den(118)
  A(135) = cont_VV(wf(:,11),wf(:,145)) * den(120)
  A(136) = cont_QA(wf(:,65),wf(:,146)) * den(121)
  A(137) = cont_VV(wf(:,11),wf(:,147)) * den(122)
  A(138) = cont_QA(wf(:,19),wf(:,148)) * den(123)
  A(139) = cont_QA(wf(:,57),wf(:,130)) * den(124)
  A(140) = cont_QA(wf(:,9),wf(:,150)) * den(126)
  A(141) = cont_QA(wf(:,57),wf(:,135)) * den(127)
  A(142) = cont_QA(wf(:,69),wf(:,143)) * den(128)
  A(143) = cont_QA(wf(:,69),wf(:,146)) * den(129)
  A(144) = cont_QA(wf(:,19),wf(:,152)) * den(131)
  A(145) = cont_VV(wf(:,153),wf(:,154)) * den(135)
  A(146) = cont_QA(wf(:,156),wf(:,157)) * den(139)
  A(147) = cont_QA(wf(:,128),wf(:,158)) * den(141)
  A(148) = cont_QA(wf(:,83),wf(:,159)) * den(143)
  A(149) = cont_VV(wf(:,30),wf(:,160)) * den(144)
  A(150) = cont_VV(wf(:,31),wf(:,162)) * den(146)
  A(151) = cont_QA(wf(:,29),wf(:,163)) * den(147)
  A(152) = cont_QA(wf(:,83),wf(:,164)) * den(148)
  A(153) = cont_VV(wf(:,31),wf(:,165)) * den(149)
  A(154) = cont_VV(wf(:,154),wf(:,166)) * den(152)
  A(155) = cont_QA(wf(:,168),wf(:,169)) * den(156)
  A(156) = cont_QA(wf(:,141),wf(:,170)) * den(158)
  A(157) = cont_QA(wf(:,65),wf(:,171)) * den(159)
  A(158) = cont_VV(wf(:,37),wf(:,160)) * den(160)
  A(159) = cont_VV(wf(:,31),wf(:,172)) * den(161)
  A(160) = cont_QA(wf(:,65),wf(:,173)) * den(162)
  A(161) = cont_VV(wf(:,31),wf(:,174)) * den(163)
  A(162) = cont_QA(wf(:,36),wf(:,148)) * den(164)
  A(163) = cont_QA(wf(:,84),wf(:,159)) * den(165)
  A(164) = cont_QA(wf(:,29),wf(:,176)) * den(167)
  A(165) = cont_QA(wf(:,84),wf(:,164)) * den(168)
  A(166) = cont_QA(wf(:,69),wf(:,171)) * den(169)
  A(167) = cont_QA(wf(:,69),wf(:,173)) * den(170)
  A(168) = cont_QA(wf(:,36),wf(:,152)) * den(171)
  A(169) = cont_VV(wf(:,30),wf(:,179)) * den(173)
  A(170) = cont_VV(wf(:,41),wf(:,162)) * den(174)
  A(171) = cont_VV(wf(:,30),wf(:,180)) * den(175)
  A(172) = cont_VV(wf(:,30),wf(:,181)) * den(176)
  A(173) = cont_VV(wf(:,30),wf(:,182)) * den(177)
  A(174) = cont_VV(wf(:,42),wf(:,162)) * den(178)
  A(175) = cont_VV(wf(:,10),wf(:,183)) * den(179)
  A(176) = cont_VV(wf(:,10),wf(:,184)) * den(180)
  A(177) = cont_VV(wf(:,43),wf(:,133)) * den(181)
  A(178) = cont_VV(wf(:,10),wf(:,185)) * den(182)
  A(179) = cont_VV(wf(:,10),wf(:,186)) * den(183)
  A(180) = cont_VV(wf(:,44),wf(:,133)) * den(184)
  A(181) = cont_VV(wf(:,20),wf(:,183)) * den(185)
  A(182) = cont_VV(wf(:,37),wf(:,179)) * den(186)
  A(183) = cont_VV(wf(:,20),wf(:,184)) * den(187)
  A(184) = cont_VV(wf(:,41),wf(:,174)) * den(188)
  A(185) = cont_VV(wf(:,41),wf(:,172)) * den(189)
  A(186) = cont_VV(wf(:,43),wf(:,145)) * den(190)
  A(187) = cont_VV(wf(:,20),wf(:,185)) * den(191)
  A(188) = cont_VV(wf(:,20),wf(:,186)) * den(192)
  A(189) = cont_VV(wf(:,44),wf(:,145)) * den(193)
  A(190) = cont_VV(wf(:,37),wf(:,182)) * den(194)
  A(191) = cont_VV(wf(:,37),wf(:,181)) * den(195)
  A(192) = cont_VV(wf(:,42),wf(:,172)) * den(196)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(192)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((-A(10)-A(12)-A(15)-A(17)-A(22))*f(1))/2._/**/REALKIND+((-A(19)-A(20)-A(21)-A(23))*f(1))/6._/**/REALKIND+(CI*(A(11) &
       +A(14))*f(2))/2._/**/REALKIND
  M1(2) = ((A(9)+A(10)+A(12)+A(13)+A(15)+A(16))*f(1))/6._/**/REALKIND+((A(1)+A(5)+A(8)+A(20)+A(23))*f(1))/2._/**/REALKIND &
       +(CI*(A(3)+A(6))*f(2))/2._/**/REALKIND
  M1(3) = ((A(2)+A(4)+A(7)+A(19)+A(21))*f(1))/2._/**/REALKIND+((A(17)+A(18)+A(22)+A(24))*f(1))/6._/**/REALKIND+(CI*(-A(3) &
       -A(6))*f(2))/2._/**/REALKIND
  M1(4) = ((-A(1)-A(2)-A(4)-A(5)-A(7)-A(8))*f(1))/6._/**/REALKIND+((-A(9)-A(13)-A(16)-A(18)-A(24))*f(1))/2._/**/REALKIND+(CI*( &
       -A(11)-A(14))*f(2))/2._/**/REALKIND

  M2(1) = ((A(148)+A(151)+A(152)+A(154)+A(155)+A(156)+A(163)+A(164)+A(165)+A(169)+A(170)+A(171)+A(182)+A(184) &
       +A(185))*f(3))/2._/**/REALKIND+((A(175)+A(176)+A(177)+A(178)+A(179)+A(180)+A(181)+A(183)+A(186)+A(187)+A(188) &
       +A(189))*f(3))/6._/**/REALKIND+(CI*(-A(149)-A(150)-A(153)-A(158)-A(159)-A(161))*f(4))/2._/**/REALKIND+((-A(67)-A(68)-A(75) &
       -A(77)-A(89)-A(90)-A(92)-A(94)-A(99)-A(100)-A(101)-A(103))*f(5))/6._/**/REALKIND+((-A(50)-A(52)-A(58)-A(60)-A(65)-A(73) &
       -A(76)-A(83)-A(85)-A(91)-A(98)-A(104)-A(106)-A(108)-A(111))*f(5))/2._/**/REALKIND+(CI*(A(57)+A(81)+A(107) &
       +A(110))*f(6))/2._/**/REALKIND+((-A(69)-A(71)-A(79)-A(80))*f(7))/6._/**/REALKIND+((-A(55)-A(62)-A(70)-A(87) &
       -A(95))*f(7))/2._/**/REALKIND+(CI*(A(64)+A(88))*f(8))/2._/**/REALKIND+(CI*(A(51)+A(54))*f(9))/2._/**/REALKIND
  M2(2) = ((-A(145)-A(146)-A(147)-A(148)-A(151)-A(152)-A(154)-A(155)-A(156)-A(157)-A(160)-A(162)-A(163)-A(164)-A(165)-A(166) &
       -A(167)-A(168))*f(3))/6._/**/REALKIND+((-A(121)-A(122)-A(123)-A(133)-A(136)-A(138)-A(142)-A(143)-A(144)-A(178)-A(179) &
       -A(180)-A(187)-A(188)-A(189))*f(3))/2._/**/REALKIND+(CI*(-A(125)-A(126)-A(129)-A(134)-A(135)-A(137))*f(4))/2._/**/REALKIND &
       +((A(49)+A(50)+A(52)+A(53)+A(58)+A(59)+A(60)+A(61)+A(82)+A(83)+A(84)+A(85)+A(105)+A(106)+A(108)+A(109)+A(111) &
       +A(112))*f(5))/6._/**/REALKIND+((A(25)+A(29)+A(35)+A(37)+A(42)+A(44)+A(68)+A(77)+A(90)+A(94)+A(99)+A(101)+A(113)+A(117) &
       +A(120))*f(5))/2._/**/REALKIND+(CI*(A(33)+A(41)+A(115)+A(118))*f(6))/2._/**/REALKIND+((A(32)+A(39)+A(46)+A(71) &
       +A(80))*f(7))/2._/**/REALKIND+((A(55)+A(56)+A(62)+A(63)+A(86)+A(87))*f(7))/6._/**/REALKIND+(CI*(A(40) &
       +A(48))*f(8))/2._/**/REALKIND+(CI*(A(27)+A(30))*f(9))/2._/**/REALKIND
  M2(3) = ((-A(124)-A(127)-A(128)-A(130)-A(131)-A(132)-A(139)-A(140)-A(141)-A(175)-A(176)-A(177)-A(181)-A(183) &
       -A(186))*f(3))/2._/**/REALKIND+((-A(169)-A(170)-A(171)-A(172)-A(173)-A(174)-A(182)-A(184)-A(185)-A(190)-A(191) &
       -A(192))*f(3))/6._/**/REALKIND+(CI*(A(125)+A(126)+A(129)+A(134)+A(135)+A(137))*f(4))/2._/**/REALKIND+((A(65)+A(66)+A(73) &
       +A(74)+A(76)+A(78)+A(91)+A(93)+A(97)+A(98)+A(102)+A(104))*f(5))/6._/**/REALKIND+((A(26)+A(28)+A(34)+A(36)+A(43)+A(45)+A(67) &
       +A(75)+A(89)+A(92)+A(100)+A(103)+A(114)+A(116)+A(119))*f(5))/2._/**/REALKIND+(CI*(-A(33)-A(41)-A(115) &
       -A(118))*f(6))/2._/**/REALKIND+((A(31)+A(38)+A(47)+A(69)+A(79))*f(7))/2._/**/REALKIND+((A(70)+A(72)+A(95) &
       +A(96))*f(7))/6._/**/REALKIND+(CI*(-A(40)-A(48))*f(8))/2._/**/REALKIND+(CI*(-A(27)-A(30))*f(9))/2._/**/REALKIND
  M2(4) = ((A(121)+A(122)+A(123)+A(124)+A(127)+A(128)+A(130)+A(131)+A(132)+A(133)+A(136)+A(138)+A(139)+A(140)+A(141)+A(142)+A(143) &
       +A(144))*f(3))/6._/**/REALKIND+((A(145)+A(146)+A(147)+A(157)+A(160)+A(162)+A(166)+A(167)+A(168)+A(172)+A(173)+A(174)+A(190) &
       +A(191)+A(192))*f(3))/2._/**/REALKIND+(CI*(A(149)+A(150)+A(153)+A(158)+A(159)+A(161))*f(4))/2._/**/REALKIND+((-A(49)-A(53) &
       -A(59)-A(61)-A(66)-A(74)-A(78)-A(82)-A(84)-A(93)-A(97)-A(102)-A(105)-A(109)-A(112))*f(5))/2._/**/REALKIND+((-A(25)-A(26) &
       -A(28)-A(29)-A(34)-A(35)-A(36)-A(37)-A(42)-A(43)-A(44)-A(45)-A(113)-A(114)-A(116)-A(117)-A(119) &
       -A(120))*f(5))/6._/**/REALKIND+(CI*(-A(57)-A(81)-A(107)-A(110))*f(6))/2._/**/REALKIND+((-A(31)-A(32)-A(38)-A(39)-A(46) &
       -A(47))*f(7))/6._/**/REALKIND+((-A(56)-A(63)-A(72)-A(86)-A(96))*f(7))/2._/**/REALKIND+(CI*(-A(64) &
       -A(88))*f(8))/2._/**/REALKIND+(CI*(-A(51)-A(54))*f(9))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwjjj_uuxuxdwxg_1_/**/REALKIND
