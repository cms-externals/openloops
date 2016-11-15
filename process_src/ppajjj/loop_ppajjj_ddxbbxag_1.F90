
module ol_colourmatrix_ppajjj_ddxbbxag_1_/**/REALKIND
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
end module ol_colourmatrix_ppajjj_ddxbbxag_1_/**/REALKIND



module ol_forced_parameters_ppajjj_ddxbbxag_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppajjj_ddxbbxag_1_/**/REALKIND

module ol_loop_ppajjj_ddxbbxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(24), c(29)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:275)
  ! denominators
  complex(REALKIND), save :: den(309)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(4,64)
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
    f( 1) = (CI*eQED*gQCD**3)/3._/**/REALKIND
    f( 2) = (eQED*gQCD**3)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*eQED*gQCD**5)/3._/**/REALKIND
    f( 4) = CI*countertermnorm*eQED*gQCD**5
    f( 5) = (countertermnorm*eQED*gQCD**5)/3._/**/REALKIND
    f( 6) = (CI*countertermnorm*ctGbb*eQED*gQCD**5)/3._/**/REALKIND
    f( 7) = (countertermnorm*ctGbb*eQED*gQCD**5)/3._/**/REALKIND
    f( 8) = (CI*countertermnorm*ctGqq*eQED*gQCD**5)/3._/**/REALKIND
    f( 9) = (countertermnorm*ctGqq*eQED*gQCD**5)/3._/**/REALKIND
    f(10) = (CI*countertermnorm*ctVbb*eQED*gQCD**5)/3._/**/REALKIND
    f(11) = (countertermnorm*ctVbb*eQED*gQCD**5)/3._/**/REALKIND
    f(12) = (CI*countertermnorm*ctVqq*eQED*gQCD**5)/3._/**/REALKIND
    f(13) = (countertermnorm*ctVqq*eQED*gQCD**5)/3._/**/REALKIND
    f(14) = (countertermnorm*ctVVV*eQED*gQCD**5)/3._/**/REALKIND
    f(15) = (CI*eQED*gQCD**5*integralnorm*SwB)/6._/**/REALKIND
    f(16) = (CI*eQED*gQCD**5*integralnorm*SwB)/3._/**/REALKIND
    f(17) = (eQED*gQCD**5*integralnorm*SwB)/6._/**/REALKIND
    f(18) = (eQED*gQCD**5*integralnorm*SwB)/3._/**/REALKIND
    f(19) = (CI*eQED*gQCD**5*integralnorm*SwF)/3._/**/REALKIND
    f(20) = (2*CI*eQED*gQCD**5*integralnorm*SwF)/3._/**/REALKIND
    f(21) = (4*CI*eQED*gQCD**5*integralnorm*SwF)/3._/**/REALKIND
    f(22) = (eQED*gQCD**5*integralnorm*SwF)/3._/**/REALKIND
    f(23) = (2*eQED*gQCD**5*integralnorm*SwF)/3._/**/REALKIND
    f(24) = (4*eQED*gQCD**5*integralnorm*SwF)/3._/**/REALKIND

  c = [ 27*CI*f(15), 54*CI*f(15), 3*CI*f(16), 9*CI*f(16), 24*CI*f(16), 27*CI*f(16), 54*CI*f(16), 18*f(17), 54*f(17), f(18) &
    , 3*f(18), 6*f(18), 8*f(18), 9*f(18), 10*f(18), 18*f(18), 21*f(18), 24*f(18), 27*f(18), 54*f(18), 9*CI*f(19), 9*CI*f(20) &
    , 9*CI*f(21), 3*f(22), 9*f(22), 3*f(23), 9*f(23), 3*f(24), 9*f(24) ]
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
  complex(REALKIND) :: A(194)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),MB,1_intkind1,wf(:,9))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,10))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,11))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,12))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,36),MB,1_intkind1,wf(:,14))
  call prop_A_Q(wf(:,13),Q(:,24),MB,1_intkind1,wf(:,15))
  call vert_VQ_A(wf(:,1),wf(:,14),wf(:,16))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,17))
  call vert_AV_Q(wf(:,15),wf(:,-5),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,7),MB,1_intkind1,wf(:,19))
  call vert_QA_V(wf(:,-2),wf(:,15),wf(:,20))
  call vert_VQ_A(wf(:,-4),wf(:,14),wf(:,21))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,22))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,23))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,24))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,25))
  call prop_Q_A(wf(:,23),Q(:,17),ZERO,0_intkind1,wf(:,26))
  call prop_A_Q(wf(:,24),Q(:,34),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,26),wf(:,27),wf(:,28))
  call vert_QA_V(wf(:,26),wf(:,-1),wf(:,29))
  call vert_UV_W(wf(:,25),Q(:,12),wf(:,-5),Q(:,32),wf(:,30))
  call vert_AV_Q(wf(:,-1),wf(:,25),wf(:,31))
  call vert_VQ_A(wf(:,-5),wf(:,26),wf(:,32))
  call prop_A_Q(wf(:,31),Q(:,14),ZERO,0_intkind1,wf(:,33))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,34))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,33),ZERO,0_intkind1,wf(:,36))
  call prop_A_Q(wf(:,35),Q(:,18),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,36),wf(:,37),wf(:,38))
  call vert_QA_V(wf(:,0),wf(:,37),wf(:,39))
  call vert_VQ_A(wf(:,25),wf(:,0),wf(:,40))
  call vert_AV_Q(wf(:,37),wf(:,-5),wf(:,41))
  call prop_Q_A(wf(:,40),Q(:,13),ZERO,0_intkind1,wf(:,42))
  call vert_VQ_A(wf(:,-4),wf(:,36),wf(:,43))
  call vert_AV_Q(wf(:,27),wf(:,-4),wf(:,44))
  call vert_QA_V(wf(:,14),wf(:,-3),wf(:,45))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,46))
  call vert_QA_V(wf(:,36),wf(:,-1),wf(:,47))
  call vert_QA_V(wf(:,0),wf(:,27),wf(:,48))
  call counter_VGG_G(ctAGGG,wf(:,-4),wf(:,1),wf(:,-5),wf(:,49))
  call counter_VGG_G(ctAGGG,wf(:,-4),wf(:,-5),wf(:,1),wf(:,50))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,51))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,52))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,53))
  call counter_VQ_A(wf(:,1),wf(:,14),wf(:,54))
  call counter_AV_Q(wf(:,15),wf(:,-5),wf(:,55))
  call counter_VQ_A(wf(:,-4),wf(:,14),wf(:,56))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,57))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,58))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,59))
  call prop_Q_A(wf(:,8),Q(:,52),MB,1_intkind1,wf(:,60))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,40),MB,1_intkind1,wf(:,62))
  call prop_Q_A(wf(:,21),Q(:,52),MB,1_intkind1,wf(:,63))
  call vert_AV_Q(wf(:,62),wf(:,-4),wf(:,64))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,65))
  call prop_A_Q(wf(:,65),Q(:,24),MB,1_intkind1,wf(:,66))
  call vert_AV_Q(wf(:,66),wf(:,-5),wf(:,67))
  call vert_QA_V(wf(:,-2),wf(:,66),wf(:,68))
  call counter_QA_V(wf(:,-2),wf(:,15),wf(:,69))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,70))
  call prop_A_Q(wf(:,18),Q(:,56),MB,1_intkind1,wf(:,71))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,36),MB,1_intkind1,wf(:,73))
  call vert_VQ_A(wf(:,1),wf(:,73),wf(:,74))
  call prop_A_Q(wf(:,22),Q(:,56),MB,1_intkind1,wf(:,75))
  call vert_VQ_A(wf(:,-4),wf(:,73),wf(:,76))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,20),MB,1_intkind1,wf(:,78))
  call vert_VQ_A(wf(:,1),wf(:,78),wf(:,79))
  call vert_VQ_A(wf(:,-5),wf(:,78),wf(:,80))
  call vert_QA_V(wf(:,78),wf(:,-3),wf(:,81))
  call counter_QA_V(wf(:,26),wf(:,27),wf(:,82))
  call counter_UV_W(wf(:,25),Q(:,12),wf(:,-5),Q(:,32),wf(:,83))
  call counter_VQ_A(wf(:,-5),wf(:,26),wf(:,84))
  call counter_QA_V(wf(:,36),wf(:,37),wf(:,85))
  call counter_AV_Q(wf(:,37),wf(:,-5),wf(:,86))
  call counter_VQ_A(wf(:,-4),wf(:,36),wf(:,87))
  call counter_AV_Q(wf(:,27),wf(:,-4),wf(:,88))
  call counter_QA_V(wf(:,14),wf(:,-3),wf(:,89))
  call vert_QA_V(wf(:,-2),wf(:,62),wf(:,90))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,91))
  call vert_QA_V(wf(:,73),wf(:,-3),wf(:,92))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,93))
  call vert_UV_W(wf(:,93),Q(:,12),wf(:,-5),Q(:,32),wf(:,94))
  call vert_AV_Q(wf(:,-1),wf(:,93),wf(:,95))
  call prop_A_Q(wf(:,95),Q(:,14),ZERO,0_intkind1,wf(:,96))
  call vert_VQ_A(wf(:,93),wf(:,0),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,13),ZERO,0_intkind1,wf(:,98))
  call counter_AV_Q(wf(:,-1),wf(:,25),wf(:,99))
  call prop_Q_A(wf(:,32),Q(:,49),ZERO,0_intkind1,wf(:,100))
  call counter_QA_V(wf(:,26),wf(:,-1),wf(:,101))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,102))
  call prop_A_Q(wf(:,102),Q(:,34),ZERO,0_intkind1,wf(:,103))
  call vert_QA_V(wf(:,26),wf(:,103),wf(:,104))
  call prop_Q_A(wf(:,43),Q(:,49),ZERO,0_intkind1,wf(:,105))
  call vert_AV_Q(wf(:,103),wf(:,-4),wf(:,106))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,107))
  call prop_A_Q(wf(:,107),Q(:,18),ZERO,0_intkind1,wf(:,108))
  call vert_QA_V(wf(:,36),wf(:,108),wf(:,109))
  call vert_AV_Q(wf(:,108),wf(:,-5),wf(:,110))
  call vert_QA_V(wf(:,0),wf(:,108),wf(:,111))
  call counter_QA_V(wf(:,36),wf(:,-1),wf(:,112))
  call vert_QA_V(wf(:,0),wf(:,103),wf(:,113))
  call counter_QA_V(wf(:,0),wf(:,37),wf(:,114))
  call counter_VQ_A(wf(:,25),wf(:,0),wf(:,115))
  call prop_A_Q(wf(:,41),Q(:,50),ZERO,0_intkind1,wf(:,116))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,117))
  call prop_Q_A(wf(:,117),Q(:,33),ZERO,0_intkind1,wf(:,118))
  call vert_QA_V(wf(:,118),wf(:,37),wf(:,119))
  call prop_A_Q(wf(:,44),Q(:,50),ZERO,0_intkind1,wf(:,120))
  call vert_VQ_A(wf(:,-4),wf(:,118),wf(:,121))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,122))
  call prop_Q_A(wf(:,122),Q(:,17),ZERO,0_intkind1,wf(:,123))
  call vert_QA_V(wf(:,123),wf(:,27),wf(:,124))
  call vert_QA_V(wf(:,123),wf(:,-1),wf(:,125))
  call vert_VQ_A(wf(:,-5),wf(:,123),wf(:,126))
  call counter_QA_V(wf(:,0),wf(:,27),wf(:,127))
  call vert_QA_V(wf(:,118),wf(:,-1),wf(:,128))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,129))
  call vert_VQ_A(wf(:,129),wf(:,4),wf(:,130))
  call vert_AV_Q(wf(:,-3),wf(:,129),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,11),MB,1_intkind1,wf(:,132))
  call vert_UV_W(wf(:,129),Q(:,3),wf(:,-5),Q(:,32),wf(:,133))
  call vert_VQ_A(wf(:,129),wf(:,14),wf(:,134))
  call vert_VQ_A(wf(:,129),wf(:,-2),wf(:,135))
  call prop_Q_A(wf(:,135),Q(:,7),MB,1_intkind1,wf(:,136))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,137))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,138))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,139))
  call counter_Q_A(ctbb,wf(:,4),Q(:,20),wf(:,140))
  call prop_A_Q(wf(:,139),Q(:,43),MB,1_intkind1,wf(:,141))
  call counter_A_Q(ctbb,wf(:,5),Q(:,40),wf(:,142))
  call prop_Q_A(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,143))
  call vert_AV_Q(wf(:,-3),wf(:,138),wf(:,144))
  call vert_UV_W(wf(:,138),Q(:,3),wf(:,-5),Q(:,32),wf(:,145))
  call prop_Q_A(wf(:,140),Q(:,20),MB,1_intkind1,wf(:,146))
  call vert_QA_V(wf(:,146),wf(:,-3),wf(:,147))
  call vert_VQ_A(wf(:,-5),wf(:,146),wf(:,148))
  call counter_A_Q(ctbb,wf(:,9),Q(:,11),wf(:,149))
  call counter_V_V(ctGG,wf(:,10),Q(:,28),wf(:,150))
  call vert_QA_V(wf(:,14),wf(:,15),wf(:,151))
  call vert_AV_Q(wf(:,15),wf(:,1),wf(:,152))
  call counter_Q_A(ctbb,wf(:,14),Q(:,36),wf(:,153))
  call prop_A_Q(wf(:,152),Q(:,27),MB,1_intkind1,wf(:,154))
  call counter_A_Q(ctbb,wf(:,15),Q(:,24),wf(:,155))
  call prop_Q_A(wf(:,16),Q(:,39),MB,1_intkind1,wf(:,156))
  call vert_VQ_A(wf(:,138),wf(:,-2),wf(:,157))
  call prop_A_Q(wf(:,155),Q(:,24),MB,1_intkind1,wf(:,158))
  call vert_QA_V(wf(:,-2),wf(:,158),wf(:,159))
  call counter_Q_A(ctbb,wf(:,19),Q(:,7),wf(:,160))
  call counter_V_V(ctGG,wf(:,20),Q(:,28),wf(:,161))
  call vert_AV_Q(wf(:,158),wf(:,-5),wf(:,162))
  call prop_Q_A(wf(:,153),Q(:,36),MB,1_intkind1,wf(:,163))
  call vert_VQ_A(wf(:,-4),wf(:,163),wf(:,164))
  call prop_A_Q(wf(:,142),Q(:,40),MB,1_intkind1,wf(:,165))
  call vert_AV_Q(wf(:,165),wf(:,-4),wf(:,166))
  call vert_AV_Q(wf(:,27),wf(:,25),wf(:,167))
  call counter_Q_A(ctqq,wf(:,26),Q(:,17),wf(:,168))
  call prop_A_Q(wf(:,167),Q(:,46),ZERO,0_intkind1,wf(:,169))
  call vert_VQ_A(wf(:,25),wf(:,26),wf(:,170))
  call counter_A_Q(ctqq,wf(:,27),Q(:,34),wf(:,171))
  call prop_Q_A(wf(:,170),Q(:,29),ZERO,0_intkind1,wf(:,172))
  call counter_V_V(ctGG,wf(:,25),Q(:,12),wf(:,173))
  call prop_Q_A(wf(:,168),Q(:,17),ZERO,0_intkind1,wf(:,174))
  call vert_QA_V(wf(:,174),wf(:,-1),wf(:,175))
  call vert_VQ_A(wf(:,-5),wf(:,174),wf(:,176))
  call vert_AV_Q(wf(:,-1),wf(:,173),wf(:,177))
  call counter_V_V(ctGG,wf(:,29),Q(:,19),wf(:,178))
  call counter_A_Q(ctqq,wf(:,33),Q(:,14),wf(:,179))
  call vert_UV_W(wf(:,173),Q(:,12),wf(:,-5),Q(:,32),wf(:,180))
  call vert_AV_Q(wf(:,37),wf(:,25),wf(:,181))
  call counter_Q_A(ctqq,wf(:,36),Q(:,33),wf(:,182))
  call prop_A_Q(wf(:,181),Q(:,30),ZERO,0_intkind1,wf(:,183))
  call vert_VQ_A(wf(:,25),wf(:,36),wf(:,184))
  call counter_A_Q(ctqq,wf(:,37),Q(:,18),wf(:,185))
  call prop_Q_A(wf(:,184),Q(:,45),ZERO,0_intkind1,wf(:,186))
  call prop_A_Q(wf(:,185),Q(:,18),ZERO,0_intkind1,wf(:,187))
  call vert_QA_V(wf(:,0),wf(:,187),wf(:,188))
  call vert_VQ_A(wf(:,173),wf(:,0),wf(:,189))
  call counter_V_V(ctGG,wf(:,39),Q(:,19),wf(:,190))
  call counter_Q_A(ctqq,wf(:,42),Q(:,13),wf(:,191))
  call vert_AV_Q(wf(:,187),wf(:,-5),wf(:,192))
  call prop_Q_A(wf(:,182),Q(:,33),ZERO,0_intkind1,wf(:,193))
  call vert_VQ_A(wf(:,-4),wf(:,193),wf(:,194))
  call prop_A_Q(wf(:,171),Q(:,34),ZERO,0_intkind1,wf(:,195))
  call vert_AV_Q(wf(:,195),wf(:,-4),wf(:,196))
  call vert_QA_V(wf(:,163),wf(:,-3),wf(:,197))
  call vert_QA_V(wf(:,-2),wf(:,165),wf(:,198))
  call vert_QA_V(wf(:,193),wf(:,-1),wf(:,199))
  call counter_V_V(ctGG,wf(:,47),Q(:,35),wf(:,200))
  call vert_QA_V(wf(:,0),wf(:,195),wf(:,201))
  call counter_V_V(ctGG,wf(:,48),Q(:,35),wf(:,202))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,25),Q(:,12),wf(:,203))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,204))
  call vert_VQ_A(wf(:,-4),wf(:,19),wf(:,205))
  call prop_Q_A(wf(:,205),Q(:,23),MB,1_intkind1,wf(:,206))
  call vert_VQ_A(wf(:,-5),wf(:,19),wf(:,207))
  call prop_Q_A(wf(:,207),Q(:,39),MB,1_intkind1,wf(:,208))
  call vert_QA_V(wf(:,-2),wf(:,9),wf(:,209))
  call vert_AV_Q(wf(:,9),wf(:,-4),wf(:,210))
  call prop_A_Q(wf(:,210),Q(:,27),MB,1_intkind1,wf(:,211))
  call vert_AV_Q(wf(:,9),wf(:,-5),wf(:,212))
  call prop_A_Q(wf(:,212),Q(:,43),MB,1_intkind1,wf(:,213))
  call vert_VQ_A(wf(:,11),wf(:,-2),wf(:,214))
  call prop_Q_A(wf(:,214),Q(:,39),MB,1_intkind1,wf(:,215))
  call vert_AV_Q(wf(:,-3),wf(:,11),wf(:,216))
  call prop_A_Q(wf(:,216),Q(:,43),MB,1_intkind1,wf(:,217))
  call vert_QA_V(wf(:,42),wf(:,-1),wf(:,218))
  call vert_QA_V(wf(:,0),wf(:,33),wf(:,219))
  call vert_VQ_A(wf(:,-4),wf(:,42),wf(:,220))
  call prop_Q_A(wf(:,220),Q(:,29),ZERO,0_intkind1,wf(:,221))
  call vert_VQ_A(wf(:,-5),wf(:,42),wf(:,222))
  call prop_Q_A(wf(:,222),Q(:,45),ZERO,0_intkind1,wf(:,223))
  call vert_VQ_A(wf(:,30),wf(:,0),wf(:,224))
  call prop_Q_A(wf(:,224),Q(:,45),ZERO,0_intkind1,wf(:,225))
  call vert_AV_Q(wf(:,33),wf(:,-4),wf(:,226))
  call prop_A_Q(wf(:,226),Q(:,30),ZERO,0_intkind1,wf(:,227))
  call vert_AV_Q(wf(:,33),wf(:,-5),wf(:,228))
  call prop_A_Q(wf(:,228),Q(:,46),ZERO,0_intkind1,wf(:,229))
  call vert_AV_Q(wf(:,-1),wf(:,30),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,46),ZERO,0_intkind1,wf(:,231))
  call vert_VQ_A(wf(:,29),wf(:,-2),wf(:,232))
  call prop_Q_A(wf(:,232),Q(:,23),MB,1_intkind1,wf(:,233))
  call vert_AV_Q(wf(:,-3),wf(:,29),wf(:,234))
  call prop_A_Q(wf(:,234),Q(:,27),MB,1_intkind1,wf(:,235))
  call vert_UV_W(wf(:,29),Q(:,19),wf(:,-5),Q(:,32),wf(:,236))
  call vert_QA_V(wf(:,100),wf(:,-1),wf(:,237))
  call vert_VQ_A(wf(:,39),wf(:,-2),wf(:,238))
  call prop_Q_A(wf(:,238),Q(:,23),MB,1_intkind1,wf(:,239))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,240))
  call prop_A_Q(wf(:,240),Q(:,27),MB,1_intkind1,wf(:,241))
  call vert_UV_W(wf(:,39),Q(:,19),wf(:,-5),Q(:,32),wf(:,242))
  call vert_QA_V(wf(:,0),wf(:,116),wf(:,243))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,244))
  call prop_Q_A(wf(:,244),Q(:,29),ZERO,0_intkind1,wf(:,245))
  call vert_AV_Q(wf(:,-1),wf(:,10),wf(:,246))
  call prop_A_Q(wf(:,246),Q(:,30),ZERO,0_intkind1,wf(:,247))
  call vert_UV_W(wf(:,10),Q(:,28),wf(:,-5),Q(:,32),wf(:,248))
  call vert_QA_V(wf(:,60),wf(:,-3),wf(:,249))
  call vert_VQ_A(wf(:,20),wf(:,0),wf(:,250))
  call prop_Q_A(wf(:,250),Q(:,29),ZERO,0_intkind1,wf(:,251))
  call vert_AV_Q(wf(:,-1),wf(:,20),wf(:,252))
  call prop_A_Q(wf(:,252),Q(:,30),ZERO,0_intkind1,wf(:,253))
  call vert_UV_W(wf(:,20),Q(:,28),wf(:,-5),Q(:,32),wf(:,254))
  call vert_QA_V(wf(:,-2),wf(:,71),wf(:,255))
  call vert_VQ_A(wf(:,47),wf(:,-2),wf(:,256))
  call prop_Q_A(wf(:,256),Q(:,39),MB,1_intkind1,wf(:,257))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,258))
  call prop_A_Q(wf(:,258),Q(:,43),MB,1_intkind1,wf(:,259))
  call vert_QA_V(wf(:,105),wf(:,-1),wf(:,260))
  call vert_VQ_A(wf(:,48),wf(:,-2),wf(:,261))
  call prop_Q_A(wf(:,261),Q(:,39),MB,1_intkind1,wf(:,262))
  call vert_AV_Q(wf(:,-3),wf(:,48),wf(:,263))
  call prop_A_Q(wf(:,263),Q(:,43),MB,1_intkind1,wf(:,264))
  call vert_QA_V(wf(:,0),wf(:,120),wf(:,265))
  call vert_VQ_A(wf(:,45),wf(:,0),wf(:,266))
  call prop_Q_A(wf(:,266),Q(:,45),ZERO,0_intkind1,wf(:,267))
  call vert_AV_Q(wf(:,-1),wf(:,45),wf(:,268))
  call prop_A_Q(wf(:,268),Q(:,46),ZERO,0_intkind1,wf(:,269))
  call vert_QA_V(wf(:,63),wf(:,-3),wf(:,270))
  call vert_VQ_A(wf(:,46),wf(:,0),wf(:,271))
  call prop_Q_A(wf(:,271),Q(:,45),ZERO,0_intkind1,wf(:,272))
  call vert_AV_Q(wf(:,-1),wf(:,46),wf(:,273))
  call prop_A_Q(wf(:,273),Q(:,46),ZERO,0_intkind1,wf(:,274))
  call vert_QA_V(wf(:,-2),wf(:,75),wf(:,275))

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
  den(2) = 1 / (Q(5,20) - MB2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(6) = 1 / (Q(5,11) - MB2)
  den(9) = 1 / (Q(5,28))
  den(12) = 1 / (Q(5,36) - MB2)
  den(13) = 1 / (Q(5,24) - MB2)
  den(16) = 1 / (Q(5,7) - MB2)
  den(23) = 1 / (Q(5,17))
  den(24) = 1 / (Q(5,34))
  den(25) = 1 / (Q(5,12))
  den(28) = 1 / (Q(5,19))
  den(31) = 1 / (Q(5,14))
  den(34) = 1 / (Q(5,33))
  den(35) = 1 / (Q(5,18))
  den(40) = 1 / (Q(5,13))
  den(49) = 1 / (Q(5,35))
  den(59) = 1 / (Q(5,52) - MB2)
  den(65) = 1 / (Q(5,56) - MB2)
  den(70) = 1 / (Q(5,49))
  den(73) = 1 / (Q(5,44))
  den(85) = 1 / (Q(5,50))
  den(95) = 1 / (Q(5,60))
  den(99) = 1 / (Q(5,43) - MB2)
  den(102) = 1 / (Q(5,23) - MB2)
  den(117) = 1 / (Q(5,27) - MB2)
  den(120) = 1 / (Q(5,39) - MB2)
  den(139) = 1 / (Q(5,46))
  den(143) = 1 / (Q(5,29))
  den(146) = 1 / (Q(5,51))
  den(158) = 1 / (Q(5,30))
  den(162) = 1 / (Q(5,45))
  den(206) = 1 / (Q(5,15))

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
  den(26) = den(23)*den(24)
  den(27) = den(25)*den(26)
  den(29) = den(23)*den(28)
  den(30) = den(25)*den(29)
  den(32) = den(25)*den(31)
  den(33) = den(23)*den(32)
  den(36) = den(34)*den(35)
  den(37) = den(25)*den(36)
  den(38) = den(28)*den(35)
  den(39) = den(25)*den(38)
  den(41) = den(25)*den(40)
  den(42) = den(35)*den(41)
  den(43) = den(32)*den(34)
  den(44) = den(24)*den(41)
  den(45) = den(12)*den(29)
  den(46) = den(3)*den(29)
  den(47) = den(12)*den(38)
  den(48) = den(3)*den(38)
  den(50) = den(34)*den(49)
  den(51) = den(2)*den(50)
  den(52) = den(24)*den(49)
  den(53) = den(2)*den(52)
  den(54) = den(13)*den(50)
  den(55) = den(13)*den(52)
  den(56) = den(1)*den(25)
  den(57) = den(1)*den(49)
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
  den(71) = den(23)*den(70)
  den(72) = den(25)*den(71)
  den(74) = den(25)*den(73)
  den(75) = den(23)*den(74)
  den(76) = den(34)*den(70)
  den(77) = den(25)*den(76)
  den(78) = den(12)*den(73)
  den(79) = den(23)*den(78)
  den(80) = den(3)*den(73)
  den(81) = den(23)*den(80)
  den(82) = den(10)*den(34)
  den(83) = den(19)*den(34)
  den(84) = den(35)*den(74)
  den(86) = den(35)*den(85)
  den(87) = den(25)*den(86)
  den(88) = den(24)*den(85)
  den(89) = den(25)*den(88)
  den(90) = den(35)*den(78)
  den(91) = den(35)*den(80)
  den(92) = den(10)*den(24)
  den(93) = den(19)*den(24)
  den(94) = den(2)*den(3)
  den(96) = den(94)*den(95)
  den(97) = den(1)*den(96)
  den(98) = den(1)*den(3)
  den(100) = den(98)*den(99)
  den(101) = den(2)*den(100)
  den(103) = den(4)*den(102)
  den(104) = den(3)*den(103)
  den(105) = den(1)**2
  den(106) = den(60)*den(105)
  den(107) = den(10)*den(105)
  den(108) = den(2)**2
  den(109) = den(57)*den(108)
  den(110) = den(7)*den(108)
  den(111) = den(7)*den(60)
  den(112) = den(10)*den(57)
  den(113) = den(12)*den(13)
  den(114) = den(95)*den(113)
  den(115) = den(1)*den(114)
  den(116) = den(1)*den(13)
  den(118) = den(116)*den(117)
  den(119) = den(12)*den(118)
  den(121) = den(14)*den(120)
  den(122) = den(13)*den(121)
  den(123) = den(66)*den(105)
  den(124) = den(19)*den(105)
  den(125) = den(13)**2
  den(126) = den(57)*den(125)
  den(127) = den(17)*den(66)
  den(128) = den(19)*den(57)
  den(129) = den(17)*den(125)
  den(130) = den(62)*den(105)
  den(131) = den(12)**2
  den(132) = den(7)*den(131)
  den(133) = den(7)*den(62)
  den(134) = den(68)*den(105)
  den(135) = den(17)*den(68)
  den(136) = den(3)**2
  den(137) = den(17)*den(136)
  den(138) = den(24)*den(25)
  den(140) = den(138)*den(139)
  den(141) = den(23)*den(140)
  den(142) = den(23)*den(25)
  den(144) = den(142)*den(143)
  den(145) = den(24)*den(144)
  den(147) = den(26)*den(146)
  den(148) = den(25)*den(147)
  den(149) = den(23)**2
  den(150) = den(74)*den(149)
  den(151) = den(32)*den(149)
  den(152) = den(25)**2
  den(153) = den(71)*den(152)
  den(154) = den(29)*den(74)
  den(155) = den(32)*den(71)
  den(156) = den(29)*den(152)
  den(157) = den(25)*den(35)
  den(159) = den(157)*den(158)
  den(160) = den(34)*den(159)
  den(161) = den(25)*den(34)
  den(163) = den(161)*den(162)
  den(164) = den(35)*den(163)
  den(165) = den(36)*den(146)
  den(166) = den(25)*den(165)
  den(167) = den(35)**2
  den(168) = den(74)*den(167)
  den(169) = den(86)*den(152)
  den(170) = den(38)*den(74)
  den(171) = den(41)*den(86)
  den(172) = den(41)*den(167)
  den(173) = den(38)*den(152)
  den(174) = den(34)**2
  den(175) = den(32)*den(174)
  den(176) = den(76)*den(152)
  den(177) = den(32)*den(76)
  den(178) = den(88)*den(152)
  den(179) = den(41)*den(88)
  den(180) = den(24)**2
  den(181) = den(41)*den(180)
  den(182) = den(78)*den(149)
  den(183) = den(29)*den(78)
  den(184) = den(29)*den(131)
  den(185) = den(80)*den(149)
  den(186) = den(29)*den(80)
  den(187) = den(29)*den(136)
  den(188) = den(78)*den(167)
  den(189) = den(38)*den(78)
  den(190) = den(38)*den(131)
  den(191) = den(80)*den(167)
  den(192) = den(38)*den(80)
  den(193) = den(38)*den(136)
  den(194) = den(10)*den(174)
  den(195) = den(10)*den(50)
  den(196) = den(50)*den(108)
  den(197) = den(10)*den(180)
  den(198) = den(10)*den(52)
  den(199) = den(52)*den(108)
  den(200) = den(19)*den(174)
  den(201) = den(19)*den(50)
  den(202) = den(50)*den(125)
  den(203) = den(19)*den(180)
  den(204) = den(19)*den(52)
  den(205) = den(52)*den(125)
  den(207) = den(56)*den(206)
  den(208) = den(17)*den(206)
  den(209) = den(17)*den(102)
  den(210) = den(17)*den(120)
  den(211) = den(7)*den(206)
  den(212) = den(7)*den(117)
  den(213) = den(7)*den(99)
  den(214) = den(57)*den(120)
  den(215) = den(57)*den(99)
  den(216) = den(41)*den(206)
  den(217) = den(32)*den(206)
  den(218) = den(41)*den(143)
  den(219) = den(41)*den(162)
  den(220) = den(74)*den(162)
  den(221) = den(32)*den(158)
  den(222) = den(32)*den(139)
  den(223) = den(74)*den(139)
  den(224) = den(29)*den(102)
  den(225) = den(29)*den(117)
  den(226) = den(29)*den(146)
  den(227) = den(71)*den(146)
  den(228) = den(38)*den(102)
  den(229) = den(38)*den(117)
  den(230) = den(38)*den(146)
  den(231) = den(86)*den(146)
  den(232) = den(10)*den(143)
  den(233) = den(10)*den(158)
  den(234) = den(10)*den(95)
  den(235) = den(60)*den(95)
  den(236) = den(19)*den(143)
  den(237) = den(19)*den(158)
  den(238) = den(19)*den(95)
  den(239) = den(66)*den(95)
  den(240) = den(50)*den(120)
  den(241) = den(50)*den(99)
  den(242) = den(76)*den(146)
  den(243) = den(52)*den(120)
  den(244) = den(52)*den(99)
  den(245) = den(88)*den(146)
  den(246) = den(78)*den(162)
  den(247) = den(78)*den(139)
  den(248) = den(62)*den(95)
  den(249) = den(80)*den(162)
  den(250) = den(80)*den(139)
  den(251) = den(68)*den(95)
  den(252) = den(25)*den(57)
  den(253) = den(1)*den(74)
  den(254) = den(1)*den(2)*den(3)
  den(255) = den(1)*den(12)*den(13)
  den(256) = den(1)*den(78)
  den(257) = den(1)*den(80)
  den(258) = den(23)*den(24)*den(25)
  den(259) = den(25)*den(34)*den(35)
  den(260) = den(25)*den(50)
  den(261) = den(25)*den(52)
  den(262) = den(12)*den(23)
  den(263) = den(3)*den(23)
  den(264) = den(12)*den(35)
  den(265) = den(3)*den(35)
  den(266) = den(2)*den(34)
  den(267) = den(2)*den(24)
  den(268) = den(13)*den(34)
  den(269) = den(13)*den(24)
  den(270) = den(2)*den(213)
  den(271) = den(2)*den(215)
  den(272) = den(1)*den(234)
  den(273) = den(1)*den(235)
  den(274) = den(13)*den(210)
  den(275) = den(13)*den(214)
  den(276) = den(1)*den(238)
  den(277) = den(1)*den(239)
  den(278) = den(12)*den(212)
  den(279) = den(1)*den(248)
  den(280) = den(3)*den(209)
  den(281) = den(1)*den(251)
  den(282) = den(25)*den(226)
  den(283) = den(25)*den(227)
  den(284) = den(23)*den(222)
  den(285) = den(23)*den(223)
  den(286) = den(25)*den(230)
  den(287) = den(35)*den(219)
  den(288) = den(25)*den(231)
  den(289) = den(35)*den(220)
  den(290) = den(25)*den(242)
  den(291) = den(34)*den(221)
  den(292) = den(24)*den(218)
  den(293) = den(25)*den(245)
  den(294) = den(12)*den(225)
  den(295) = den(23)*den(247)
  den(296) = den(3)*den(224)
  den(297) = den(23)*den(250)
  den(298) = den(12)*den(229)
  den(299) = den(35)*den(246)
  den(300) = den(3)*den(228)
  den(301) = den(35)*den(249)
  den(302) = den(2)*den(241)
  den(303) = den(34)*den(233)
  den(304) = den(2)*den(244)
  den(305) = den(24)*den(232)
  den(306) = den(13)*den(240)
  den(307) = den(34)*den(237)
  den(308) = den(13)*den(243)
  den(309) = den(24)*den(236)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(194)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_VV(wf(:,10),wf(:,11)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(6) = cont_VV(wf(:,11),wf(:,20)) * den(20)
  A(7) = cont_QA(wf(:,9),wf(:,21)) * den(21)
  A(8) = cont_QA(wf(:,19),wf(:,22)) * den(22)
  A(9) = cont_VV(wf(:,25),wf(:,28)) * den(27)
  A(10) = cont_VV(wf(:,29),wf(:,30)) * den(30)
  A(11) = cont_QA(wf(:,32),wf(:,33)) * den(33)
  A(12) = cont_VV(wf(:,25),wf(:,38)) * den(37)
  A(13) = cont_VV(wf(:,30),wf(:,39)) * den(39)
  A(14) = cont_QA(wf(:,41),wf(:,42)) * den(42)
  A(15) = cont_QA(wf(:,33),wf(:,43)) * den(43)
  A(16) = cont_QA(wf(:,42),wf(:,44)) * den(44)
  A(17) = cont_VV(wf(:,29),wf(:,45)) * den(45)
  A(18) = cont_VV(wf(:,29),wf(:,46)) * den(46)
  A(19) = cont_VV(wf(:,39),wf(:,45)) * den(47)
  A(20) = cont_VV(wf(:,39),wf(:,46)) * den(48)
  A(21) = cont_VV(wf(:,10),wf(:,47)) * den(51)
  A(22) = cont_VV(wf(:,10),wf(:,48)) * den(53)
  A(23) = cont_VV(wf(:,20),wf(:,47)) * den(54)
  A(24) = cont_VV(wf(:,20),wf(:,48)) * den(55)

  A(25) = cont_VV(wf(:,25),wf(:,49)) * den(56)
  A(26) = cont_VV(wf(:,25),wf(:,50)) * den(56)
  A(27) = cont_QA(wf(:,5),wf(:,51)) * den(5)
  A(28) = cont_QA(wf(:,9),wf(:,52)) * den(8)
  A(29) = cont_VV(wf(:,10),wf(:,53)) * den(11)
  A(30) = cont_QA(wf(:,15),wf(:,54)) * den(15)
  A(31) = cont_QA(wf(:,19),wf(:,55)) * den(18)
  A(32) = cont_VV(wf(:,20),wf(:,53)) * den(20)
  A(33) = cont_QA(wf(:,9),wf(:,56)) * den(21)
  A(34) = cont_QA(wf(:,19),wf(:,57)) * den(22)
  A(35) = cont_VV(wf(:,11),wf(:,58)) * den(58)
  A(36) = cont_QA(wf(:,59),wf(:,60)) * den(61)
  A(37) = cont_QA(wf(:,6),wf(:,62)) * den(5)
  A(38) = cont_QA(wf(:,59),wf(:,63)) * den(63)
  A(39) = cont_QA(wf(:,19),wf(:,64)) * den(22)
  A(40) = cont_QA(wf(:,16),wf(:,66)) * den(15)
  A(41) = cont_QA(wf(:,19),wf(:,67)) * den(18)
  A(42) = cont_VV(wf(:,11),wf(:,68)) * den(20)
  A(43) = cont_VV(wf(:,11),wf(:,69)) * den(64)
  A(44) = cont_QA(wf(:,70),wf(:,71)) * den(67)
  A(45) = cont_QA(wf(:,15),wf(:,74)) * den(15)
  A(46) = cont_QA(wf(:,70),wf(:,75)) * den(69)
  A(47) = cont_QA(wf(:,9),wf(:,76)) * den(21)
  A(48) = cont_QA(wf(:,5),wf(:,79)) * den(5)
  A(49) = cont_QA(wf(:,9),wf(:,80)) * den(8)
  A(50) = cont_VV(wf(:,11),wf(:,81)) * den(11)
  A(51) = cont_VV(wf(:,25),wf(:,82)) * den(27)
  A(52) = cont_VV(wf(:,29),wf(:,83)) * den(30)
  A(53) = cont_QA(wf(:,33),wf(:,84)) * den(33)
  A(54) = cont_VV(wf(:,25),wf(:,85)) * den(37)
  A(55) = cont_VV(wf(:,39),wf(:,83)) * den(39)
  A(56) = cont_QA(wf(:,42),wf(:,86)) * den(42)
  A(57) = cont_QA(wf(:,33),wf(:,87)) * den(43)
  A(58) = cont_QA(wf(:,42),wf(:,88)) * den(44)
  A(59) = cont_VV(wf(:,29),wf(:,89)) * den(45)
  A(60) = cont_VV(wf(:,29),wf(:,90)) * den(46)
  A(61) = cont_VV(wf(:,39),wf(:,89)) * den(47)
  A(62) = cont_VV(wf(:,39),wf(:,90)) * den(48)
  A(63) = cont_VV(wf(:,47),wf(:,58)) * den(51)
  A(64) = cont_VV(wf(:,48),wf(:,58)) * den(53)
  A(65) = cont_VV(wf(:,47),wf(:,68)) * den(54)
  A(66) = cont_VV(wf(:,48),wf(:,68)) * den(55)
  A(67) = cont_VV(wf(:,29),wf(:,91)) * den(46)
  A(68) = cont_VV(wf(:,29),wf(:,92)) * den(45)
  A(69) = cont_VV(wf(:,39),wf(:,91)) * den(48)
  A(70) = cont_VV(wf(:,39),wf(:,92)) * den(47)
  A(71) = cont_VV(wf(:,47),wf(:,69)) * den(54)
  A(72) = cont_VV(wf(:,48),wf(:,69)) * den(55)
  A(73) = cont_VV(wf(:,47),wf(:,81)) * den(51)
  A(74) = cont_VV(wf(:,48),wf(:,81)) * den(53)
  A(75) = cont_VV(wf(:,28),wf(:,93)) * den(27)
  A(76) = cont_VV(wf(:,29),wf(:,94)) * den(30)
  A(77) = cont_QA(wf(:,32),wf(:,96)) * den(33)
  A(78) = cont_VV(wf(:,38),wf(:,93)) * den(37)
  A(79) = cont_VV(wf(:,39),wf(:,94)) * den(39)
  A(80) = cont_QA(wf(:,41),wf(:,98)) * den(42)
  A(81) = cont_QA(wf(:,43),wf(:,96)) * den(43)
  A(82) = cont_QA(wf(:,44),wf(:,98)) * den(44)
  A(83) = cont_QA(wf(:,99),wf(:,100)) * den(72)
  A(84) = cont_VV(wf(:,30),wf(:,101)) * den(75)
  A(85) = cont_VV(wf(:,25),wf(:,104)) * den(27)
  A(86) = cont_QA(wf(:,99),wf(:,105)) * den(77)
  A(87) = cont_QA(wf(:,42),wf(:,106)) * den(44)
  A(88) = cont_VV(wf(:,25),wf(:,109)) * den(37)
  A(89) = cont_QA(wf(:,42),wf(:,110)) * den(42)
  A(90) = cont_VV(wf(:,30),wf(:,111)) * den(39)
  A(91) = cont_VV(wf(:,45),wf(:,101)) * den(79)
  A(92) = cont_VV(wf(:,46),wf(:,101)) * den(81)
  A(93) = cont_VV(wf(:,10),wf(:,112)) * den(82)
  A(94) = cont_VV(wf(:,10),wf(:,113)) * den(53)
  A(95) = cont_VV(wf(:,20),wf(:,112)) * den(83)
  A(96) = cont_VV(wf(:,20),wf(:,113)) * den(55)
  A(97) = cont_VV(wf(:,45),wf(:,111)) * den(47)
  A(98) = cont_VV(wf(:,46),wf(:,111)) * den(48)
  A(99) = cont_VV(wf(:,30),wf(:,114)) * den(84)
  A(100) = cont_QA(wf(:,115),wf(:,116)) * den(87)
  A(101) = cont_VV(wf(:,25),wf(:,119)) * den(37)
  A(102) = cont_QA(wf(:,115),wf(:,120)) * den(89)
  A(103) = cont_QA(wf(:,33),wf(:,121)) * den(43)
  A(104) = cont_VV(wf(:,25),wf(:,124)) * den(27)
  A(105) = cont_VV(wf(:,30),wf(:,125)) * den(30)
  A(106) = cont_QA(wf(:,33),wf(:,126)) * den(33)
  A(107) = cont_VV(wf(:,45),wf(:,114)) * den(90)
  A(108) = cont_VV(wf(:,46),wf(:,114)) * den(91)
  A(109) = cont_VV(wf(:,10),wf(:,127)) * den(92)
  A(110) = cont_VV(wf(:,10),wf(:,128)) * den(51)
  A(111) = cont_VV(wf(:,20),wf(:,127)) * den(93)
  A(112) = cont_VV(wf(:,20),wf(:,128)) * den(54)
  A(113) = cont_VV(wf(:,45),wf(:,125)) * den(45)
  A(114) = cont_VV(wf(:,46),wf(:,125)) * den(46)
  A(115) = cont_QA(wf(:,5),wf(:,130)) * den(5)
  A(116) = cont_QA(wf(:,8),wf(:,132)) * den(8)
  A(117) = cont_VV(wf(:,10),wf(:,133)) * den(11)
  A(118) = cont_QA(wf(:,15),wf(:,134)) * den(15)
  A(119) = cont_QA(wf(:,18),wf(:,136)) * den(18)
  A(120) = cont_VV(wf(:,20),wf(:,133)) * den(20)
  A(121) = cont_QA(wf(:,21),wf(:,132)) * den(21)
  A(122) = cont_QA(wf(:,22),wf(:,136)) * den(22)
  A(123) = cont_VV(wf(:,137),wf(:,138)) * den(97)
  A(124) = cont_QA(wf(:,140),wf(:,141)) * den(101)
  A(125) = cont_QA(wf(:,142),wf(:,143)) * den(104)
  A(126) = cont_QA(wf(:,60),wf(:,144)) * den(106)
  A(127) = cont_VV(wf(:,10),wf(:,145)) * den(107)
  A(128) = cont_VV(wf(:,11),wf(:,147)) * den(109)
  A(129) = cont_QA(wf(:,9),wf(:,148)) * den(110)
  A(130) = cont_QA(wf(:,60),wf(:,149)) * den(111)
  A(131) = cont_VV(wf(:,11),wf(:,150)) * den(112)
  A(132) = cont_VV(wf(:,138),wf(:,151)) * den(115)
  A(133) = cont_QA(wf(:,153),wf(:,154)) * den(119)
  A(134) = cont_QA(wf(:,155),wf(:,156)) * den(122)
  A(135) = cont_QA(wf(:,71),wf(:,157)) * den(123)
  A(136) = cont_VV(wf(:,20),wf(:,145)) * den(124)
  A(137) = cont_VV(wf(:,11),wf(:,159)) * den(126)
  A(138) = cont_QA(wf(:,71),wf(:,160)) * den(127)
  A(139) = cont_VV(wf(:,11),wf(:,161)) * den(128)
  A(140) = cont_QA(wf(:,19),wf(:,162)) * den(129)
  A(141) = cont_QA(wf(:,63),wf(:,144)) * den(130)
  A(142) = cont_QA(wf(:,9),wf(:,164)) * den(132)
  A(143) = cont_QA(wf(:,63),wf(:,149)) * den(133)
  A(144) = cont_QA(wf(:,75),wf(:,157)) * den(134)
  A(145) = cont_QA(wf(:,75),wf(:,160)) * den(135)
  A(146) = cont_QA(wf(:,19),wf(:,166)) * den(137)
  A(147) = cont_QA(wf(:,168),wf(:,169)) * den(141)
  A(148) = cont_QA(wf(:,171),wf(:,172)) * den(145)
  A(149) = cont_VV(wf(:,28),wf(:,173)) * den(148)
  A(150) = cont_VV(wf(:,30),wf(:,175)) * den(150)
  A(151) = cont_QA(wf(:,33),wf(:,176)) * den(151)
  A(152) = cont_QA(wf(:,100),wf(:,177)) * den(153)
  A(153) = cont_VV(wf(:,30),wf(:,178)) * den(154)
  A(154) = cont_QA(wf(:,100),wf(:,179)) * den(155)
  A(155) = cont_VV(wf(:,29),wf(:,180)) * den(156)
  A(156) = cont_QA(wf(:,182),wf(:,183)) * den(160)
  A(157) = cont_QA(wf(:,185),wf(:,186)) * den(164)
  A(158) = cont_VV(wf(:,38),wf(:,173)) * den(166)
  A(159) = cont_VV(wf(:,30),wf(:,188)) * den(168)
  A(160) = cont_QA(wf(:,116),wf(:,189)) * den(169)
  A(161) = cont_VV(wf(:,30),wf(:,190)) * den(170)
  A(162) = cont_QA(wf(:,116),wf(:,191)) * den(171)
  A(163) = cont_QA(wf(:,42),wf(:,192)) * den(172)
  A(164) = cont_VV(wf(:,39),wf(:,180)) * den(173)
  A(165) = cont_QA(wf(:,33),wf(:,194)) * den(175)
  A(166) = cont_QA(wf(:,105),wf(:,177)) * den(176)
  A(167) = cont_QA(wf(:,105),wf(:,179)) * den(177)
  A(168) = cont_QA(wf(:,120),wf(:,189)) * den(178)
  A(169) = cont_QA(wf(:,120),wf(:,191)) * den(179)
  A(170) = cont_QA(wf(:,42),wf(:,196)) * den(181)
  A(171) = cont_VV(wf(:,45),wf(:,175)) * den(182)
  A(172) = cont_VV(wf(:,45),wf(:,178)) * den(183)
  A(173) = cont_VV(wf(:,29),wf(:,197)) * den(184)
  A(174) = cont_VV(wf(:,46),wf(:,175)) * den(185)
  A(175) = cont_VV(wf(:,46),wf(:,178)) * den(186)
  A(176) = cont_VV(wf(:,29),wf(:,198)) * den(187)
  A(177) = cont_VV(wf(:,45),wf(:,188)) * den(188)
  A(178) = cont_VV(wf(:,45),wf(:,190)) * den(189)
  A(179) = cont_VV(wf(:,39),wf(:,197)) * den(190)
  A(180) = cont_VV(wf(:,46),wf(:,188)) * den(191)
  A(181) = cont_VV(wf(:,46),wf(:,190)) * den(192)
  A(182) = cont_VV(wf(:,39),wf(:,198)) * den(193)
  A(183) = cont_VV(wf(:,10),wf(:,199)) * den(194)
  A(184) = cont_VV(wf(:,10),wf(:,200)) * den(195)
  A(185) = cont_VV(wf(:,47),wf(:,147)) * den(196)
  A(186) = cont_VV(wf(:,10),wf(:,201)) * den(197)
  A(187) = cont_VV(wf(:,10),wf(:,202)) * den(198)
  A(188) = cont_VV(wf(:,48),wf(:,147)) * den(199)
  A(189) = cont_VV(wf(:,20),wf(:,199)) * den(200)
  A(190) = cont_VV(wf(:,20),wf(:,200)) * den(201)
  A(191) = cont_VV(wf(:,47),wf(:,159)) * den(202)
  A(192) = cont_VV(wf(:,20),wf(:,201)) * den(203)
  A(193) = cont_VV(wf(:,20),wf(:,202)) * den(204)
  A(194) = cont_VV(wf(:,48),wf(:,159)) * den(205)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(194)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((A(9)+A(11)+A(12)+A(14)+A(15)+A(16)+A(21)+A(22)+A(23)+A(24))*f(1))/6._/**/REALKIND
  M1(2) = ((-A(2)-A(4)-A(7)-A(9)-A(14)-A(16)-A(17)-A(19)-A(22)-A(24))*f(1))/2._/**/REALKIND+(CI*(-A(3)-A(6)+A(10) &
       +A(13))*f(2))/2._/**/REALKIND
  M1(3) = ((-A(1)-A(5)-A(8)-A(11)-A(12)-A(15)-A(18)-A(20)-A(21)-A(23))*f(1))/2._/**/REALKIND+(CI*(A(3)+A(6)-A(10) &
       -A(13))*f(2))/2._/**/REALKIND
  M1(4) = ((A(1)+A(2)+A(4)+A(5)+A(7)+A(8)+A(17)+A(18)+A(19)+A(20))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(147)-A(148)-A(149)-A(151)-A(152)-A(154)-A(156)-A(157)-A(158)-A(160)-A(162)-A(163)-A(165)-A(166)-A(167)-A(168) &
       -A(169)-A(170)-A(183)-A(184)-A(185)-A(186)-A(187)-A(188)-A(189)-A(190)-A(191)-A(192)-A(193)-A(194))*f(3))/6._/**/REALKIND &
       +((A(25)+A(26))*f(4))/12._/**/REALKIND+((A(63)+A(64)+A(71)+A(72)+A(75)+A(77)+A(78)+A(80)+A(81)+A(82))*f(6))/6._/**/REALKIND &
       +((A(51)+A(53)+A(54)+A(56)+A(83)+A(85)+A(86)+A(87)+A(93)+A(94)+A(95)+A(96)+A(100)+A(101)+A(102)+A(103)+A(109)+A(110)+A(111) &
       +A(112))*f(8))/6._/**/REALKIND+((A(65)+A(66)+A(73)+A(74))*f(10))/6._/**/REALKIND+((A(57)+A(58)+A(88)+A(89)+A(104) &
       +A(106))*f(12))/6._/**/REALKIND
  M2(2) = ((A(126)+A(129)+A(130)+A(132)+A(133)+A(134)+A(141)+A(142)+A(143)+A(147)+A(148)+A(149)+A(160)+A(162)+A(163)+A(168)+A(169) &
       +A(170)+A(171)+A(172)+A(173)+A(177)+A(178)+A(179)+A(186)+A(187)+A(188)+A(192)+A(193)+A(194))*f(3))/2._/**/REALKIND &
       -(A(25)*f(4))/4._/**/REALKIND+(CI*(A(127)+A(128)+A(131)+A(136)+A(137)+A(139)-A(150)-A(153)-A(155)-A(159)-A(161) &
       -A(164))*f(5))/2._/**/REALKIND+((-A(28)-A(30)-A(36)-A(38)-A(45)-A(47)-A(59)-A(61)-A(64)-A(68)-A(70)-A(72)-A(75)-A(80) &
       -A(82))*f(6))/2._/**/REALKIND+(CI*(-A(35)-A(43)+A(76)+A(79))*f(7))/2._/**/REALKIND+((-A(51)-A(56)-A(85)-A(87)-A(91)-A(94) &
       -A(96)-A(100)-A(102)-A(107)-A(109)-A(111)-A(116)-A(118)-A(121))*f(8))/2._/**/REALKIND+(CI*(A(84)+A(99)-A(117) &
       -A(120))*f(9))/2._/**/REALKIND+((-A(33)-A(40)-A(49)-A(66)-A(74))*f(10))/2._/**/REALKIND+(CI*(-A(42) &
       -A(50))*f(11))/2._/**/REALKIND+((-A(58)-A(89)-A(97)-A(104)-A(113))*f(12))/2._/**/REALKIND+(CI*(A(90) &
       +A(105))*f(13))/2._/**/REALKIND+(CI*(-A(29)-A(32)+A(52)+A(55))*f(14))/2._/**/REALKIND
  M2(3) = ((A(123)+A(124)+A(125)+A(135)+A(138)+A(140)+A(144)+A(145)+A(146)+A(151)+A(152)+A(154)+A(156)+A(157)+A(158)+A(165)+A(166) &
       +A(167)+A(174)+A(175)+A(176)+A(180)+A(181)+A(182)+A(183)+A(184)+A(185)+A(189)+A(190)+A(191))*f(3))/2._/**/REALKIND &
       -(A(26)*f(4))/4._/**/REALKIND+(CI*(-A(127)-A(128)-A(131)-A(136)-A(137)-A(139)+A(150)+A(153)+A(155)+A(159)+A(161) &
       +A(164))*f(5))/2._/**/REALKIND+((-A(27)-A(31)-A(37)-A(39)-A(44)-A(46)-A(60)-A(62)-A(63)-A(67)-A(69)-A(71)-A(77)-A(78) &
       -A(81))*f(6))/2._/**/REALKIND+(CI*(A(35)+A(43)-A(76)-A(79))*f(7))/2._/**/REALKIND+((-A(53)-A(54)-A(83)-A(86)-A(92)-A(93) &
       -A(95)-A(101)-A(103)-A(108)-A(110)-A(112)-A(115)-A(119)-A(122))*f(8))/2._/**/REALKIND+(CI*(-A(84)-A(99)+A(117) &
       +A(120))*f(9))/2._/**/REALKIND+((-A(34)-A(41)-A(48)-A(65)-A(73))*f(10))/2._/**/REALKIND+(CI*(A(42) &
       +A(50))*f(11))/2._/**/REALKIND+((-A(57)-A(88)-A(98)-A(106)-A(114))*f(12))/2._/**/REALKIND+(CI*(-A(90) &
       -A(105))*f(13))/2._/**/REALKIND+(CI*(A(29)+A(32)-A(52)-A(55))*f(14))/2._/**/REALKIND
  M2(4) = ((-A(123)-A(124)-A(125)-A(126)-A(129)-A(130)-A(132)-A(133)-A(134)-A(135)-A(138)-A(140)-A(141)-A(142)-A(143)-A(144) &
       -A(145)-A(146)-A(171)-A(172)-A(173)-A(174)-A(175)-A(176)-A(177)-A(178)-A(179)-A(180)-A(181)-A(182))*f(3))/6._/**/REALKIND &
       +((A(25)+A(26))*f(4))/12._/**/REALKIND+((A(27)+A(28)+A(30)+A(31)+A(36)+A(37)+A(38)+A(39)+A(44)+A(45)+A(46)+A(47)+A(59) &
       +A(60)+A(61)+A(62)+A(67)+A(68)+A(69)+A(70))*f(6))/6._/**/REALKIND+((A(91)+A(92)+A(107)+A(108)+A(115)+A(116)+A(118)+A(119) &
       +A(121)+A(122))*f(8))/6._/**/REALKIND+((A(33)+A(34)+A(40)+A(41)+A(48)+A(49))*f(10))/6._/**/REALKIND+((A(97)+A(98)+A(113) &
       +A(114))*f(12))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppajjj_ddxbbxag_1_/**/REALKIND
