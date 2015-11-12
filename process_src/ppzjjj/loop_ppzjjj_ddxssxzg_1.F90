
module ol_colourmatrix_ppzjjj_ddxssxzg_1_/**/REALKIND
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
end module ol_colourmatrix_ppzjjj_ddxssxzg_1_/**/REALKIND



module ol_forced_parameters_ppzjjj_ddxssxzg_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzjjj_ddxssxzg_1_/**/REALKIND

module ol_loop_ppzjjj_ddxssxzg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:284)
  ! denominators
  complex(REALKIND), save :: den(317)
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
    f( 1) = CI*eQED*gQCD**3
    f( 2) = eQED*gQCD**3
    f( 3) = CI*countertermnorm*eQED*gQCD**5
    f( 4) = countertermnorm*eQED*gQCD**5
    f( 5) = CI*countertermnorm*ctGqq*eQED*gQCD**5
    f( 6) = countertermnorm*ctGqq*eQED*gQCD**5
    f( 7) = CI*countertermnorm*ctVqq*eQED*gQCD**5
    f( 8) = countertermnorm*ctVqq*eQED*gQCD**5
    f( 9) = countertermnorm*ctVVV*eQED*gQCD**5
    f(10) = CI*countertermnorm*ctZGG*eQED*gQCD**5
    f(11) = countertermnorm*ctZGG*eQED*gQCD**5
    f(12) = (CI*eQED*gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(13) = CI*eQED*gQCD**5*integralnorm*SwB
    f(14) = (eQED*gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(15) = eQED*gQCD**5*integralnorm*SwB
    f(16) = CI*eQED*gQCD**5*integralnorm*SwF
    f(17) = 2*CI*eQED*gQCD**5*integralnorm*SwF
    f(18) = eQED*gQCD**5*integralnorm*SwF
    f(19) = 2*eQED*gQCD**5*integralnorm*SwF

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
  complex(REALKIND) :: A(205)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,10))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,11))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,12))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,36),ZERO,0_intkind1,wf(:,14))
  call prop_A_Q(wf(:,13),Q(:,24),ZERO,0_intkind1,wf(:,15))
  call vert_VQ_A(wf(:,1),wf(:,14),wf(:,16))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,17))
  call vert_AV_Q(wf(:,15),wf(:,-5),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,7),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,-2),wf(:,15),wf(:,20))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,14),wf(:,21))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,-4),wf(:,22))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,23))
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
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,33),ZERO,0_intkind1,wf(:,36))
  call prop_A_Q(wf(:,35),Q(:,18),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,36),wf(:,37),wf(:,38))
  call vert_QA_V(wf(:,0),wf(:,37),wf(:,39))
  call vert_VQ_A(wf(:,25),wf(:,0),wf(:,40))
  call vert_AV_Q(wf(:,37),wf(:,-5),wf(:,41))
  call prop_Q_A(wf(:,40),Q(:,13),ZERO,0_intkind1,wf(:,42))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,36),wf(:,43))
  call vert_AZ_Q(gZd,wf(:,27),wf(:,-4),wf(:,44))
  call vert_QA_V(wf(:,14),wf(:,-3),wf(:,45))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,46))
  call vert_QA_V(wf(:,36),wf(:,-1),wf(:,47))
  call vert_QA_V(wf(:,0),wf(:,27),wf(:,48))
  call counter_VGG_G(ctZGGG,wf(:,-4),wf(:,1),wf(:,-5),wf(:,49))
  call counter_VGG_G(ctZGGG,wf(:,-4),wf(:,-5),wf(:,1),wf(:,50))
  call counter_VG_G(wf(:,-4),wf(:,25),Q(:,12),wf(:,51),Q(:,28))
  call counter_VG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,52),Q(:,19))
  call counter_VG_G(wf(:,-4),wf(:,-5),Q(:,32),wf(:,53),Q(:,48))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,25),Q(:,12),wf(:,54))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,55))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,56))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,57))
  call counter_VQ_A(wf(:,1),wf(:,14),wf(:,58))
  call counter_AV_Q(wf(:,15),wf(:,-5),wf(:,59))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,14),wf(:,60))
  call counter_AZ_Q(gZd,wf(:,5),wf(:,-4),wf(:,61))
  call vert_AV_Q(wf(:,-3),wf(:,53),wf(:,62))
  call vert_VQ_A(wf(:,53),wf(:,-2),wf(:,63))
  call prop_Q_A(wf(:,63),Q(:,52),ZERO,0_intkind1,wf(:,64))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,65))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,66))
  call prop_Q_A(wf(:,8),Q(:,52),ZERO,0_intkind1,wf(:,67))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,40),ZERO,0_intkind1,wf(:,69))
  call prop_Q_A(wf(:,21),Q(:,52),ZERO,0_intkind1,wf(:,70))
  call vert_AZ_Q(gZd,wf(:,69),wf(:,-4),wf(:,71))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,24),ZERO,0_intkind1,wf(:,73))
  call vert_AV_Q(wf(:,73),wf(:,-5),wf(:,74))
  call vert_QA_V(wf(:,-2),wf(:,73),wf(:,75))
  call counter_QA_V(wf(:,-2),wf(:,15),wf(:,76))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,77))
  call prop_A_Q(wf(:,18),Q(:,56),ZERO,0_intkind1,wf(:,78))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,79))
  call prop_Q_A(wf(:,79),Q(:,36),ZERO,0_intkind1,wf(:,80))
  call vert_VQ_A(wf(:,1),wf(:,80),wf(:,81))
  call prop_A_Q(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,82))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,80),wf(:,83))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,84))
  call prop_Q_A(wf(:,84),Q(:,20),ZERO,0_intkind1,wf(:,85))
  call vert_VQ_A(wf(:,1),wf(:,85),wf(:,86))
  call vert_VQ_A(wf(:,-5),wf(:,85),wf(:,87))
  call vert_QA_V(wf(:,85),wf(:,-3),wf(:,88))
  call counter_QA_V(wf(:,26),wf(:,27),wf(:,89))
  call counter_UV_W(wf(:,25),Q(:,12),wf(:,-5),Q(:,32),wf(:,90))
  call counter_VQ_A(wf(:,-5),wf(:,26),wf(:,91))
  call counter_QA_V(wf(:,36),wf(:,37),wf(:,92))
  call counter_AV_Q(wf(:,37),wf(:,-5),wf(:,93))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,36),wf(:,94))
  call counter_AZ_Q(gZd,wf(:,27),wf(:,-4),wf(:,95))
  call vert_AV_Q(wf(:,-1),wf(:,53),wf(:,96))
  call vert_VQ_A(wf(:,53),wf(:,0),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,49),ZERO,0_intkind1,wf(:,98))
  call counter_QA_V(wf(:,14),wf(:,-3),wf(:,99))
  call vert_QA_V(wf(:,-2),wf(:,69),wf(:,100))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,101))
  call vert_QA_V(wf(:,80),wf(:,-3),wf(:,102))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,103))
  call vert_UV_W(wf(:,103),Q(:,12),wf(:,-5),Q(:,32),wf(:,104))
  call vert_AV_Q(wf(:,-1),wf(:,103),wf(:,105))
  call prop_A_Q(wf(:,105),Q(:,14),ZERO,0_intkind1,wf(:,106))
  call vert_VQ_A(wf(:,103),wf(:,0),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,13),ZERO,0_intkind1,wf(:,108))
  call counter_AV_Q(wf(:,-1),wf(:,25),wf(:,109))
  call prop_Q_A(wf(:,32),Q(:,49),ZERO,0_intkind1,wf(:,110))
  call counter_QA_V(wf(:,26),wf(:,-1),wf(:,111))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,34),ZERO,0_intkind1,wf(:,113))
  call vert_QA_V(wf(:,26),wf(:,113),wf(:,114))
  call prop_Q_A(wf(:,43),Q(:,49),ZERO,0_intkind1,wf(:,115))
  call vert_AZ_Q(gZd,wf(:,113),wf(:,-4),wf(:,116))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,117))
  call prop_A_Q(wf(:,117),Q(:,18),ZERO,0_intkind1,wf(:,118))
  call vert_QA_V(wf(:,36),wf(:,118),wf(:,119))
  call vert_AV_Q(wf(:,118),wf(:,-5),wf(:,120))
  call vert_QA_V(wf(:,0),wf(:,118),wf(:,121))
  call counter_QA_V(wf(:,36),wf(:,-1),wf(:,122))
  call vert_QA_V(wf(:,0),wf(:,113),wf(:,123))
  call counter_QA_V(wf(:,0),wf(:,37),wf(:,124))
  call counter_VQ_A(wf(:,25),wf(:,0),wf(:,125))
  call prop_A_Q(wf(:,41),Q(:,50),ZERO,0_intkind1,wf(:,126))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,127))
  call prop_Q_A(wf(:,127),Q(:,33),ZERO,0_intkind1,wf(:,128))
  call vert_QA_V(wf(:,128),wf(:,37),wf(:,129))
  call prop_A_Q(wf(:,44),Q(:,50),ZERO,0_intkind1,wf(:,130))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,128),wf(:,131))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,17),ZERO,0_intkind1,wf(:,133))
  call vert_QA_V(wf(:,133),wf(:,27),wf(:,134))
  call vert_QA_V(wf(:,133),wf(:,-1),wf(:,135))
  call vert_VQ_A(wf(:,-5),wf(:,133),wf(:,136))
  call counter_QA_V(wf(:,0),wf(:,27),wf(:,137))
  call vert_QA_V(wf(:,128),wf(:,-1),wf(:,138))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,139))
  call vert_VQ_A(wf(:,139),wf(:,4),wf(:,140))
  call vert_AV_Q(wf(:,-3),wf(:,139),wf(:,141))
  call prop_A_Q(wf(:,141),Q(:,11),ZERO,0_intkind1,wf(:,142))
  call vert_UV_W(wf(:,139),Q(:,3),wf(:,-5),Q(:,32),wf(:,143))
  call vert_VQ_A(wf(:,139),wf(:,14),wf(:,144))
  call vert_VQ_A(wf(:,139),wf(:,-2),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,7),ZERO,0_intkind1,wf(:,146))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,147))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,148))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,149))
  call counter_Q_A(ctqq,wf(:,4),Q(:,20),wf(:,150))
  call prop_A_Q(wf(:,149),Q(:,43),ZERO,0_intkind1,wf(:,151))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,152))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,153))
  call vert_AV_Q(wf(:,-3),wf(:,148),wf(:,154))
  call vert_UV_W(wf(:,148),Q(:,3),wf(:,-5),Q(:,32),wf(:,155))
  call prop_Q_A(wf(:,150),Q(:,20),ZERO,0_intkind1,wf(:,156))
  call vert_QA_V(wf(:,156),wf(:,-3),wf(:,157))
  call vert_VQ_A(wf(:,-5),wf(:,156),wf(:,158))
  call counter_A_Q(ctqq,wf(:,9),Q(:,11),wf(:,159))
  call counter_V_V(ctGG,wf(:,10),Q(:,28),wf(:,160))
  call vert_QA_V(wf(:,14),wf(:,15),wf(:,161))
  call vert_AV_Q(wf(:,15),wf(:,1),wf(:,162))
  call counter_Q_A(ctqq,wf(:,14),Q(:,36),wf(:,163))
  call prop_A_Q(wf(:,162),Q(:,27),ZERO,0_intkind1,wf(:,164))
  call counter_A_Q(ctqq,wf(:,15),Q(:,24),wf(:,165))
  call prop_Q_A(wf(:,16),Q(:,39),ZERO,0_intkind1,wf(:,166))
  call vert_VQ_A(wf(:,148),wf(:,-2),wf(:,167))
  call prop_A_Q(wf(:,165),Q(:,24),ZERO,0_intkind1,wf(:,168))
  call vert_QA_V(wf(:,-2),wf(:,168),wf(:,169))
  call counter_Q_A(ctqq,wf(:,19),Q(:,7),wf(:,170))
  call counter_V_V(ctGG,wf(:,20),Q(:,28),wf(:,171))
  call vert_AV_Q(wf(:,168),wf(:,-5),wf(:,172))
  call prop_Q_A(wf(:,163),Q(:,36),ZERO,0_intkind1,wf(:,173))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,173),wf(:,174))
  call prop_A_Q(wf(:,152),Q(:,40),ZERO,0_intkind1,wf(:,175))
  call vert_AZ_Q(gZd,wf(:,175),wf(:,-4),wf(:,176))
  call vert_AV_Q(wf(:,27),wf(:,25),wf(:,177))
  call counter_Q_A(ctqq,wf(:,26),Q(:,17),wf(:,178))
  call prop_A_Q(wf(:,177),Q(:,46),ZERO,0_intkind1,wf(:,179))
  call vert_VQ_A(wf(:,25),wf(:,26),wf(:,180))
  call counter_A_Q(ctqq,wf(:,27),Q(:,34),wf(:,181))
  call prop_Q_A(wf(:,180),Q(:,29),ZERO,0_intkind1,wf(:,182))
  call counter_V_V(ctGG,wf(:,25),Q(:,12),wf(:,183))
  call prop_Q_A(wf(:,178),Q(:,17),ZERO,0_intkind1,wf(:,184))
  call vert_QA_V(wf(:,184),wf(:,-1),wf(:,185))
  call vert_VQ_A(wf(:,-5),wf(:,184),wf(:,186))
  call vert_AV_Q(wf(:,-1),wf(:,183),wf(:,187))
  call counter_V_V(ctGG,wf(:,29),Q(:,19),wf(:,188))
  call counter_A_Q(ctqq,wf(:,33),Q(:,14),wf(:,189))
  call vert_UV_W(wf(:,183),Q(:,12),wf(:,-5),Q(:,32),wf(:,190))
  call vert_AV_Q(wf(:,37),wf(:,25),wf(:,191))
  call counter_Q_A(ctqq,wf(:,36),Q(:,33),wf(:,192))
  call prop_A_Q(wf(:,191),Q(:,30),ZERO,0_intkind1,wf(:,193))
  call vert_VQ_A(wf(:,25),wf(:,36),wf(:,194))
  call counter_A_Q(ctqq,wf(:,37),Q(:,18),wf(:,195))
  call prop_Q_A(wf(:,194),Q(:,45),ZERO,0_intkind1,wf(:,196))
  call prop_A_Q(wf(:,195),Q(:,18),ZERO,0_intkind1,wf(:,197))
  call vert_QA_V(wf(:,0),wf(:,197),wf(:,198))
  call vert_VQ_A(wf(:,183),wf(:,0),wf(:,199))
  call counter_V_V(ctGG,wf(:,39),Q(:,19),wf(:,200))
  call counter_Q_A(ctqq,wf(:,42),Q(:,13),wf(:,201))
  call vert_AV_Q(wf(:,197),wf(:,-5),wf(:,202))
  call prop_Q_A(wf(:,192),Q(:,33),ZERO,0_intkind1,wf(:,203))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,203),wf(:,204))
  call prop_A_Q(wf(:,181),Q(:,34),ZERO,0_intkind1,wf(:,205))
  call vert_AZ_Q(gZd,wf(:,205),wf(:,-4),wf(:,206))
  call vert_QA_V(wf(:,173),wf(:,-3),wf(:,207))
  call vert_QA_V(wf(:,-2),wf(:,175),wf(:,208))
  call vert_QA_V(wf(:,203),wf(:,-1),wf(:,209))
  call counter_V_V(ctGG,wf(:,47),Q(:,35),wf(:,210))
  call vert_QA_V(wf(:,0),wf(:,205),wf(:,211))
  call counter_V_V(ctGG,wf(:,48),Q(:,35),wf(:,212))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,213))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,19),wf(:,214))
  call prop_Q_A(wf(:,214),Q(:,23),ZERO,0_intkind1,wf(:,215))
  call vert_VQ_A(wf(:,-5),wf(:,19),wf(:,216))
  call prop_Q_A(wf(:,216),Q(:,39),ZERO,0_intkind1,wf(:,217))
  call vert_QA_V(wf(:,-2),wf(:,9),wf(:,218))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-4),wf(:,219))
  call prop_A_Q(wf(:,219),Q(:,27),ZERO,0_intkind1,wf(:,220))
  call vert_AV_Q(wf(:,9),wf(:,-5),wf(:,221))
  call prop_A_Q(wf(:,221),Q(:,43),ZERO,0_intkind1,wf(:,222))
  call vert_VQ_A(wf(:,11),wf(:,-2),wf(:,223))
  call prop_Q_A(wf(:,223),Q(:,39),ZERO,0_intkind1,wf(:,224))
  call vert_AV_Q(wf(:,-3),wf(:,11),wf(:,225))
  call prop_A_Q(wf(:,225),Q(:,43),ZERO,0_intkind1,wf(:,226))
  call vert_QA_V(wf(:,42),wf(:,-1),wf(:,227))
  call vert_QA_V(wf(:,0),wf(:,33),wf(:,228))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,42),wf(:,229))
  call prop_Q_A(wf(:,229),Q(:,29),ZERO,0_intkind1,wf(:,230))
  call vert_VQ_A(wf(:,-5),wf(:,42),wf(:,231))
  call prop_Q_A(wf(:,231),Q(:,45),ZERO,0_intkind1,wf(:,232))
  call vert_VQ_A(wf(:,30),wf(:,0),wf(:,233))
  call prop_Q_A(wf(:,233),Q(:,45),ZERO,0_intkind1,wf(:,234))
  call vert_AZ_Q(gZd,wf(:,33),wf(:,-4),wf(:,235))
  call prop_A_Q(wf(:,235),Q(:,30),ZERO,0_intkind1,wf(:,236))
  call vert_AV_Q(wf(:,33),wf(:,-5),wf(:,237))
  call prop_A_Q(wf(:,237),Q(:,46),ZERO,0_intkind1,wf(:,238))
  call vert_AV_Q(wf(:,-1),wf(:,30),wf(:,239))
  call prop_A_Q(wf(:,239),Q(:,46),ZERO,0_intkind1,wf(:,240))
  call vert_VQ_A(wf(:,29),wf(:,-2),wf(:,241))
  call prop_Q_A(wf(:,241),Q(:,23),ZERO,0_intkind1,wf(:,242))
  call vert_AV_Q(wf(:,-3),wf(:,29),wf(:,243))
  call prop_A_Q(wf(:,243),Q(:,27),ZERO,0_intkind1,wf(:,244))
  call vert_UV_W(wf(:,29),Q(:,19),wf(:,-5),Q(:,32),wf(:,245))
  call vert_QA_V(wf(:,110),wf(:,-1),wf(:,246))
  call vert_VQ_A(wf(:,39),wf(:,-2),wf(:,247))
  call prop_Q_A(wf(:,247),Q(:,23),ZERO,0_intkind1,wf(:,248))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,249))
  call prop_A_Q(wf(:,249),Q(:,27),ZERO,0_intkind1,wf(:,250))
  call vert_UV_W(wf(:,39),Q(:,19),wf(:,-5),Q(:,32),wf(:,251))
  call vert_QA_V(wf(:,0),wf(:,126),wf(:,252))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,253))
  call prop_Q_A(wf(:,253),Q(:,29),ZERO,0_intkind1,wf(:,254))
  call vert_AV_Q(wf(:,-1),wf(:,10),wf(:,255))
  call prop_A_Q(wf(:,255),Q(:,30),ZERO,0_intkind1,wf(:,256))
  call vert_UV_W(wf(:,10),Q(:,28),wf(:,-5),Q(:,32),wf(:,257))
  call vert_QA_V(wf(:,67),wf(:,-3),wf(:,258))
  call vert_VQ_A(wf(:,20),wf(:,0),wf(:,259))
  call prop_Q_A(wf(:,259),Q(:,29),ZERO,0_intkind1,wf(:,260))
  call vert_AV_Q(wf(:,-1),wf(:,20),wf(:,261))
  call prop_A_Q(wf(:,261),Q(:,30),ZERO,0_intkind1,wf(:,262))
  call vert_UV_W(wf(:,20),Q(:,28),wf(:,-5),Q(:,32),wf(:,263))
  call vert_QA_V(wf(:,-2),wf(:,78),wf(:,264))
  call vert_VQ_A(wf(:,47),wf(:,-2),wf(:,265))
  call prop_Q_A(wf(:,265),Q(:,39),ZERO,0_intkind1,wf(:,266))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,267))
  call prop_A_Q(wf(:,267),Q(:,43),ZERO,0_intkind1,wf(:,268))
  call vert_QA_V(wf(:,115),wf(:,-1),wf(:,269))
  call vert_VQ_A(wf(:,48),wf(:,-2),wf(:,270))
  call prop_Q_A(wf(:,270),Q(:,39),ZERO,0_intkind1,wf(:,271))
  call vert_AV_Q(wf(:,-3),wf(:,48),wf(:,272))
  call prop_A_Q(wf(:,272),Q(:,43),ZERO,0_intkind1,wf(:,273))
  call vert_QA_V(wf(:,0),wf(:,130),wf(:,274))
  call vert_VQ_A(wf(:,45),wf(:,0),wf(:,275))
  call prop_Q_A(wf(:,275),Q(:,45),ZERO,0_intkind1,wf(:,276))
  call vert_AV_Q(wf(:,-1),wf(:,45),wf(:,277))
  call prop_A_Q(wf(:,277),Q(:,46),ZERO,0_intkind1,wf(:,278))
  call vert_QA_V(wf(:,70),wf(:,-3),wf(:,279))
  call vert_VQ_A(wf(:,46),wf(:,0),wf(:,280))
  call prop_Q_A(wf(:,280),Q(:,45),ZERO,0_intkind1,wf(:,281))
  call vert_AV_Q(wf(:,-1),wf(:,46),wf(:,282))
  call prop_A_Q(wf(:,282),Q(:,46),ZERO,0_intkind1,wf(:,283))
  call vert_QA_V(wf(:,-2),wf(:,82),wf(:,284))

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
  den(23) = 1 / (Q(5,17))
  den(24) = 1 / (Q(5,34))
  den(25) = 1 / (Q(5,12))
  den(28) = 1 / (Q(5,19))
  den(31) = 1 / (Q(5,14))
  den(34) = 1 / (Q(5,33))
  den(35) = 1 / (Q(5,18))
  den(40) = 1 / (Q(5,13))
  den(49) = 1 / (Q(5,35))
  den(59) = 1 / (Q(5,44))
  den(62) = 1 / (Q(5,48))
  den(69) = 1 / (Q(5,52))
  den(78) = 1 / (Q(5,56))
  den(86) = 1 / (Q(5,49))
  den(99) = 1 / (Q(5,50))
  den(109) = 1 / (Q(5,60))
  den(113) = 1 / (Q(5,43))
  den(116) = 1 / (Q(5,23))
  den(131) = 1 / (Q(5,27))
  den(134) = 1 / (Q(5,39))
  den(153) = 1 / (Q(5,46))
  den(157) = 1 / (Q(5,29))
  den(160) = 1 / (Q(5,51))
  den(172) = 1 / (Q(5,30))
  den(176) = 1 / (Q(5,45))
  den(220) = 1 / (Q(5,15))

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
  den(58) = den(25)*den(57)
  den(60) = den(25)*den(59)
  den(61) = den(1)*den(60)
  den(63) = den(56)*den(62)
  den(64) = den(12)*den(59)
  den(65) = den(1)*den(64)
  den(66) = den(3)*den(59)
  den(67) = den(1)*den(66)
  den(68) = den(17)*den(62)
  den(70) = den(62)*den(69)
  den(71) = den(1)*den(70)
  den(72) = den(2)*den(57)
  den(73) = den(2)*den(69)
  den(74) = den(1)*den(73)
  den(75) = den(12)*den(69)
  den(76) = den(1)*den(75)
  den(77) = den(13)*den(57)
  den(79) = den(13)*den(78)
  den(80) = den(1)*den(79)
  den(81) = den(3)*den(78)
  den(82) = den(1)*den(81)
  den(83) = den(25)*den(50)
  den(84) = den(25)*den(52)
  den(85) = den(41)*den(62)
  den(87) = den(62)*den(86)
  den(88) = den(25)*den(87)
  den(89) = den(23)*den(86)
  den(90) = den(25)*den(89)
  den(91) = den(23)*den(60)
  den(92) = den(34)*den(86)
  den(93) = den(25)*den(92)
  den(94) = den(23)*den(64)
  den(95) = den(23)*den(66)
  den(96) = den(10)*den(34)
  den(97) = den(19)*den(34)
  den(98) = den(35)*den(60)
  den(100) = den(35)*den(99)
  den(101) = den(25)*den(100)
  den(102) = den(24)*den(99)
  den(103) = den(25)*den(102)
  den(104) = den(35)*den(64)
  den(105) = den(35)*den(66)
  den(106) = den(10)*den(24)
  den(107) = den(19)*den(24)
  den(108) = den(2)*den(3)
  den(110) = den(108)*den(109)
  den(111) = den(1)*den(110)
  den(112) = den(1)*den(3)
  den(114) = den(112)*den(113)
  den(115) = den(2)*den(114)
  den(117) = den(4)*den(116)
  den(118) = den(3)*den(117)
  den(119) = den(1)**2
  den(120) = den(73)*den(119)
  den(121) = den(10)*den(119)
  den(122) = den(2)**2
  den(123) = den(57)*den(122)
  den(124) = den(7)*den(122)
  den(125) = den(7)*den(73)
  den(126) = den(10)*den(57)
  den(127) = den(12)*den(13)
  den(128) = den(109)*den(127)
  den(129) = den(1)*den(128)
  den(130) = den(1)*den(13)
  den(132) = den(130)*den(131)
  den(133) = den(12)*den(132)
  den(135) = den(14)*den(134)
  den(136) = den(13)*den(135)
  den(137) = den(79)*den(119)
  den(138) = den(19)*den(119)
  den(139) = den(13)**2
  den(140) = den(57)*den(139)
  den(141) = den(17)*den(79)
  den(142) = den(19)*den(57)
  den(143) = den(17)*den(139)
  den(144) = den(75)*den(119)
  den(145) = den(12)**2
  den(146) = den(7)*den(145)
  den(147) = den(7)*den(75)
  den(148) = den(81)*den(119)
  den(149) = den(17)*den(81)
  den(150) = den(3)**2
  den(151) = den(17)*den(150)
  den(152) = den(24)*den(25)
  den(154) = den(152)*den(153)
  den(155) = den(23)*den(154)
  den(156) = den(23)*den(25)
  den(158) = den(156)*den(157)
  den(159) = den(24)*den(158)
  den(161) = den(26)*den(160)
  den(162) = den(25)*den(161)
  den(163) = den(23)**2
  den(164) = den(60)*den(163)
  den(165) = den(32)*den(163)
  den(166) = den(25)**2
  den(167) = den(89)*den(166)
  den(168) = den(29)*den(60)
  den(169) = den(32)*den(89)
  den(170) = den(29)*den(166)
  den(171) = den(25)*den(35)
  den(173) = den(171)*den(172)
  den(174) = den(34)*den(173)
  den(175) = den(25)*den(34)
  den(177) = den(175)*den(176)
  den(178) = den(35)*den(177)
  den(179) = den(36)*den(160)
  den(180) = den(25)*den(179)
  den(181) = den(35)**2
  den(182) = den(60)*den(181)
  den(183) = den(100)*den(166)
  den(184) = den(38)*den(60)
  den(185) = den(41)*den(100)
  den(186) = den(41)*den(181)
  den(187) = den(38)*den(166)
  den(188) = den(34)**2
  den(189) = den(32)*den(188)
  den(190) = den(92)*den(166)
  den(191) = den(32)*den(92)
  den(192) = den(102)*den(166)
  den(193) = den(41)*den(102)
  den(194) = den(24)**2
  den(195) = den(41)*den(194)
  den(196) = den(64)*den(163)
  den(197) = den(29)*den(64)
  den(198) = den(29)*den(145)
  den(199) = den(66)*den(163)
  den(200) = den(29)*den(66)
  den(201) = den(29)*den(150)
  den(202) = den(64)*den(181)
  den(203) = den(38)*den(64)
  den(204) = den(38)*den(145)
  den(205) = den(66)*den(181)
  den(206) = den(38)*den(66)
  den(207) = den(38)*den(150)
  den(208) = den(10)*den(188)
  den(209) = den(10)*den(50)
  den(210) = den(50)*den(122)
  den(211) = den(10)*den(194)
  den(212) = den(10)*den(52)
  den(213) = den(52)*den(122)
  den(214) = den(19)*den(188)
  den(215) = den(19)*den(50)
  den(216) = den(50)*den(139)
  den(217) = den(19)*den(194)
  den(218) = den(19)*den(52)
  den(219) = den(52)*den(139)
  den(221) = den(56)*den(220)
  den(222) = den(17)*den(220)
  den(223) = den(17)*den(116)
  den(224) = den(17)*den(134)
  den(225) = den(7)*den(220)
  den(226) = den(7)*den(131)
  den(227) = den(7)*den(113)
  den(228) = den(57)*den(134)
  den(229) = den(57)*den(113)
  den(230) = den(41)*den(220)
  den(231) = den(32)*den(220)
  den(232) = den(41)*den(157)
  den(233) = den(41)*den(176)
  den(234) = den(60)*den(176)
  den(235) = den(32)*den(172)
  den(236) = den(32)*den(153)
  den(237) = den(60)*den(153)
  den(238) = den(29)*den(116)
  den(239) = den(29)*den(131)
  den(240) = den(29)*den(160)
  den(241) = den(89)*den(160)
  den(242) = den(38)*den(116)
  den(243) = den(38)*den(131)
  den(244) = den(38)*den(160)
  den(245) = den(100)*den(160)
  den(246) = den(10)*den(157)
  den(247) = den(10)*den(172)
  den(248) = den(10)*den(109)
  den(249) = den(73)*den(109)
  den(250) = den(19)*den(157)
  den(251) = den(19)*den(172)
  den(252) = den(19)*den(109)
  den(253) = den(79)*den(109)
  den(254) = den(50)*den(134)
  den(255) = den(50)*den(113)
  den(256) = den(92)*den(160)
  den(257) = den(52)*den(134)
  den(258) = den(52)*den(113)
  den(259) = den(102)*den(160)
  den(260) = den(64)*den(176)
  den(261) = den(64)*den(153)
  den(262) = den(75)*den(109)
  den(263) = den(66)*den(176)
  den(264) = den(66)*den(153)
  den(265) = den(81)*den(109)
  den(266) = den(1)*den(2)*den(3)
  den(267) = den(1)*den(12)*den(13)
  den(268) = den(23)*den(24)*den(25)
  den(269) = den(25)*den(34)*den(35)
  den(270) = den(12)*den(23)
  den(271) = den(3)*den(23)
  den(272) = den(12)*den(35)
  den(273) = den(3)*den(35)
  den(274) = den(2)*den(34)
  den(275) = den(2)*den(24)
  den(276) = den(13)*den(34)
  den(277) = den(13)*den(24)
  den(278) = den(2)*den(227)
  den(279) = den(2)*den(229)
  den(280) = den(1)*den(248)
  den(281) = den(1)*den(249)
  den(282) = den(13)*den(224)
  den(283) = den(13)*den(228)
  den(284) = den(1)*den(252)
  den(285) = den(1)*den(253)
  den(286) = den(12)*den(226)
  den(287) = den(1)*den(262)
  den(288) = den(3)*den(223)
  den(289) = den(1)*den(265)
  den(290) = den(25)*den(240)
  den(291) = den(25)*den(241)
  den(292) = den(23)*den(236)
  den(293) = den(23)*den(237)
  den(294) = den(25)*den(244)
  den(295) = den(35)*den(233)
  den(296) = den(25)*den(245)
  den(297) = den(35)*den(234)
  den(298) = den(25)*den(256)
  den(299) = den(34)*den(235)
  den(300) = den(24)*den(232)
  den(301) = den(25)*den(259)
  den(302) = den(12)*den(239)
  den(303) = den(23)*den(261)
  den(304) = den(3)*den(238)
  den(305) = den(23)*den(264)
  den(306) = den(12)*den(243)
  den(307) = den(35)*den(260)
  den(308) = den(3)*den(242)
  den(309) = den(35)*den(263)
  den(310) = den(2)*den(255)
  den(311) = den(34)*den(247)
  den(312) = den(2)*den(258)
  den(313) = den(24)*den(246)
  den(314) = den(13)*den(254)
  den(315) = den(34)*den(251)
  den(316) = den(13)*den(257)
  den(317) = den(24)*den(250)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(205)

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
  A(27) = cont_VV(wf(:,11),wf(:,51)) * den(58)
  A(28) = cont_VV(wf(:,30),wf(:,52)) * den(61)
  A(29) = cont_VV(wf(:,53),wf(:,54)) * den(63)
  A(30) = cont_QA(wf(:,5),wf(:,55)) * den(5)
  A(31) = cont_QA(wf(:,9),wf(:,56)) * den(8)
  A(32) = cont_VV(wf(:,10),wf(:,57)) * den(11)
  A(33) = cont_QA(wf(:,15),wf(:,58)) * den(15)
  A(34) = cont_QA(wf(:,19),wf(:,59)) * den(18)
  A(35) = cont_VV(wf(:,20),wf(:,57)) * den(20)
  A(36) = cont_QA(wf(:,9),wf(:,60)) * den(21)
  A(37) = cont_VV(wf(:,45),wf(:,52)) * den(65)
  A(38) = cont_QA(wf(:,19),wf(:,61)) * den(22)
  A(39) = cont_VV(wf(:,46),wf(:,52)) * den(67)
  A(40) = cont_QA(wf(:,19),wf(:,62)) * den(68)
  A(41) = cont_QA(wf(:,7),wf(:,64)) * den(71)
  A(42) = cont_VV(wf(:,11),wf(:,65)) * den(72)
  A(43) = cont_QA(wf(:,66),wf(:,67)) * den(74)
  A(44) = cont_QA(wf(:,6),wf(:,69)) * den(5)
  A(45) = cont_QA(wf(:,66),wf(:,70)) * den(76)
  A(46) = cont_QA(wf(:,19),wf(:,71)) * den(22)
  A(47) = cont_QA(wf(:,16),wf(:,73)) * den(15)
  A(48) = cont_QA(wf(:,19),wf(:,74)) * den(18)
  A(49) = cont_VV(wf(:,11),wf(:,75)) * den(20)
  A(50) = cont_VV(wf(:,11),wf(:,76)) * den(77)
  A(51) = cont_QA(wf(:,77),wf(:,78)) * den(80)
  A(52) = cont_QA(wf(:,15),wf(:,81)) * den(15)
  A(53) = cont_QA(wf(:,77),wf(:,82)) * den(82)
  A(54) = cont_QA(wf(:,9),wf(:,83)) * den(21)
  A(55) = cont_QA(wf(:,5),wf(:,86)) * den(5)
  A(56) = cont_QA(wf(:,9),wf(:,87)) * den(8)
  A(57) = cont_VV(wf(:,11),wf(:,88)) * den(11)
  A(58) = cont_VV(wf(:,25),wf(:,89)) * den(27)
  A(59) = cont_VV(wf(:,29),wf(:,90)) * den(30)
  A(60) = cont_QA(wf(:,33),wf(:,91)) * den(33)
  A(61) = cont_VV(wf(:,25),wf(:,92)) * den(37)
  A(62) = cont_VV(wf(:,39),wf(:,90)) * den(39)
  A(63) = cont_QA(wf(:,42),wf(:,93)) * den(42)
  A(64) = cont_VV(wf(:,47),wf(:,51)) * den(83)
  A(65) = cont_QA(wf(:,33),wf(:,94)) * den(43)
  A(66) = cont_VV(wf(:,48),wf(:,51)) * den(84)
  A(67) = cont_QA(wf(:,42),wf(:,95)) * den(44)
  A(68) = cont_QA(wf(:,42),wf(:,96)) * den(85)
  A(69) = cont_QA(wf(:,31),wf(:,98)) * den(88)
  A(70) = cont_VV(wf(:,29),wf(:,99)) * den(45)
  A(71) = cont_VV(wf(:,29),wf(:,100)) * den(46)
  A(72) = cont_VV(wf(:,39),wf(:,99)) * den(47)
  A(73) = cont_VV(wf(:,39),wf(:,100)) * den(48)
  A(74) = cont_VV(wf(:,47),wf(:,65)) * den(51)
  A(75) = cont_VV(wf(:,48),wf(:,65)) * den(53)
  A(76) = cont_VV(wf(:,47),wf(:,75)) * den(54)
  A(77) = cont_VV(wf(:,48),wf(:,75)) * den(55)
  A(78) = cont_VV(wf(:,29),wf(:,101)) * den(46)
  A(79) = cont_VV(wf(:,29),wf(:,102)) * den(45)
  A(80) = cont_VV(wf(:,39),wf(:,101)) * den(48)
  A(81) = cont_VV(wf(:,39),wf(:,102)) * den(47)
  A(82) = cont_VV(wf(:,47),wf(:,76)) * den(54)
  A(83) = cont_VV(wf(:,48),wf(:,76)) * den(55)
  A(84) = cont_VV(wf(:,47),wf(:,88)) * den(51)
  A(85) = cont_VV(wf(:,48),wf(:,88)) * den(53)
  A(86) = cont_VV(wf(:,28),wf(:,103)) * den(27)
  A(87) = cont_VV(wf(:,29),wf(:,104)) * den(30)
  A(88) = cont_QA(wf(:,32),wf(:,106)) * den(33)
  A(89) = cont_VV(wf(:,38),wf(:,103)) * den(37)
  A(90) = cont_VV(wf(:,39),wf(:,104)) * den(39)
  A(91) = cont_QA(wf(:,41),wf(:,108)) * den(42)
  A(92) = cont_QA(wf(:,43),wf(:,106)) * den(43)
  A(93) = cont_QA(wf(:,44),wf(:,108)) * den(44)
  A(94) = cont_QA(wf(:,109),wf(:,110)) * den(90)
  A(95) = cont_VV(wf(:,30),wf(:,111)) * den(91)
  A(96) = cont_VV(wf(:,25),wf(:,114)) * den(27)
  A(97) = cont_QA(wf(:,109),wf(:,115)) * den(93)
  A(98) = cont_QA(wf(:,42),wf(:,116)) * den(44)
  A(99) = cont_VV(wf(:,25),wf(:,119)) * den(37)
  A(100) = cont_QA(wf(:,42),wf(:,120)) * den(42)
  A(101) = cont_VV(wf(:,30),wf(:,121)) * den(39)
  A(102) = cont_VV(wf(:,45),wf(:,111)) * den(94)
  A(103) = cont_VV(wf(:,46),wf(:,111)) * den(95)
  A(104) = cont_VV(wf(:,10),wf(:,122)) * den(96)
  A(105) = cont_VV(wf(:,10),wf(:,123)) * den(53)
  A(106) = cont_VV(wf(:,20),wf(:,122)) * den(97)
  A(107) = cont_VV(wf(:,20),wf(:,123)) * den(55)
  A(108) = cont_VV(wf(:,45),wf(:,121)) * den(47)
  A(109) = cont_VV(wf(:,46),wf(:,121)) * den(48)
  A(110) = cont_VV(wf(:,30),wf(:,124)) * den(98)
  A(111) = cont_QA(wf(:,125),wf(:,126)) * den(101)
  A(112) = cont_VV(wf(:,25),wf(:,129)) * den(37)
  A(113) = cont_QA(wf(:,125),wf(:,130)) * den(103)
  A(114) = cont_QA(wf(:,33),wf(:,131)) * den(43)
  A(115) = cont_VV(wf(:,25),wf(:,134)) * den(27)
  A(116) = cont_VV(wf(:,30),wf(:,135)) * den(30)
  A(117) = cont_QA(wf(:,33),wf(:,136)) * den(33)
  A(118) = cont_VV(wf(:,45),wf(:,124)) * den(104)
  A(119) = cont_VV(wf(:,46),wf(:,124)) * den(105)
  A(120) = cont_VV(wf(:,10),wf(:,137)) * den(106)
  A(121) = cont_VV(wf(:,10),wf(:,138)) * den(51)
  A(122) = cont_VV(wf(:,20),wf(:,137)) * den(107)
  A(123) = cont_VV(wf(:,20),wf(:,138)) * den(54)
  A(124) = cont_VV(wf(:,45),wf(:,135)) * den(45)
  A(125) = cont_VV(wf(:,46),wf(:,135)) * den(46)
  A(126) = cont_QA(wf(:,5),wf(:,140)) * den(5)
  A(127) = cont_QA(wf(:,8),wf(:,142)) * den(8)
  A(128) = cont_VV(wf(:,10),wf(:,143)) * den(11)
  A(129) = cont_QA(wf(:,15),wf(:,144)) * den(15)
  A(130) = cont_QA(wf(:,18),wf(:,146)) * den(18)
  A(131) = cont_VV(wf(:,20),wf(:,143)) * den(20)
  A(132) = cont_QA(wf(:,21),wf(:,142)) * den(21)
  A(133) = cont_QA(wf(:,22),wf(:,146)) * den(22)
  A(134) = cont_VV(wf(:,147),wf(:,148)) * den(111)
  A(135) = cont_QA(wf(:,150),wf(:,151)) * den(115)
  A(136) = cont_QA(wf(:,152),wf(:,153)) * den(118)
  A(137) = cont_QA(wf(:,67),wf(:,154)) * den(120)
  A(138) = cont_VV(wf(:,10),wf(:,155)) * den(121)
  A(139) = cont_VV(wf(:,11),wf(:,157)) * den(123)
  A(140) = cont_QA(wf(:,9),wf(:,158)) * den(124)
  A(141) = cont_QA(wf(:,67),wf(:,159)) * den(125)
  A(142) = cont_VV(wf(:,11),wf(:,160)) * den(126)
  A(143) = cont_VV(wf(:,148),wf(:,161)) * den(129)
  A(144) = cont_QA(wf(:,163),wf(:,164)) * den(133)
  A(145) = cont_QA(wf(:,165),wf(:,166)) * den(136)
  A(146) = cont_QA(wf(:,78),wf(:,167)) * den(137)
  A(147) = cont_VV(wf(:,20),wf(:,155)) * den(138)
  A(148) = cont_VV(wf(:,11),wf(:,169)) * den(140)
  A(149) = cont_QA(wf(:,78),wf(:,170)) * den(141)
  A(150) = cont_VV(wf(:,11),wf(:,171)) * den(142)
  A(151) = cont_QA(wf(:,19),wf(:,172)) * den(143)
  A(152) = cont_QA(wf(:,70),wf(:,154)) * den(144)
  A(153) = cont_QA(wf(:,9),wf(:,174)) * den(146)
  A(154) = cont_QA(wf(:,70),wf(:,159)) * den(147)
  A(155) = cont_QA(wf(:,82),wf(:,167)) * den(148)
  A(156) = cont_QA(wf(:,82),wf(:,170)) * den(149)
  A(157) = cont_QA(wf(:,19),wf(:,176)) * den(151)
  A(158) = cont_QA(wf(:,178),wf(:,179)) * den(155)
  A(159) = cont_QA(wf(:,181),wf(:,182)) * den(159)
  A(160) = cont_VV(wf(:,28),wf(:,183)) * den(162)
  A(161) = cont_VV(wf(:,30),wf(:,185)) * den(164)
  A(162) = cont_QA(wf(:,33),wf(:,186)) * den(165)
  A(163) = cont_QA(wf(:,110),wf(:,187)) * den(167)
  A(164) = cont_VV(wf(:,30),wf(:,188)) * den(168)
  A(165) = cont_QA(wf(:,110),wf(:,189)) * den(169)
  A(166) = cont_VV(wf(:,29),wf(:,190)) * den(170)
  A(167) = cont_QA(wf(:,192),wf(:,193)) * den(174)
  A(168) = cont_QA(wf(:,195),wf(:,196)) * den(178)
  A(169) = cont_VV(wf(:,38),wf(:,183)) * den(180)
  A(170) = cont_VV(wf(:,30),wf(:,198)) * den(182)
  A(171) = cont_QA(wf(:,126),wf(:,199)) * den(183)
  A(172) = cont_VV(wf(:,30),wf(:,200)) * den(184)
  A(173) = cont_QA(wf(:,126),wf(:,201)) * den(185)
  A(174) = cont_QA(wf(:,42),wf(:,202)) * den(186)
  A(175) = cont_VV(wf(:,39),wf(:,190)) * den(187)
  A(176) = cont_QA(wf(:,33),wf(:,204)) * den(189)
  A(177) = cont_QA(wf(:,115),wf(:,187)) * den(190)
  A(178) = cont_QA(wf(:,115),wf(:,189)) * den(191)
  A(179) = cont_QA(wf(:,130),wf(:,199)) * den(192)
  A(180) = cont_QA(wf(:,130),wf(:,201)) * den(193)
  A(181) = cont_QA(wf(:,42),wf(:,206)) * den(195)
  A(182) = cont_VV(wf(:,45),wf(:,185)) * den(196)
  A(183) = cont_VV(wf(:,45),wf(:,188)) * den(197)
  A(184) = cont_VV(wf(:,29),wf(:,207)) * den(198)
  A(185) = cont_VV(wf(:,46),wf(:,185)) * den(199)
  A(186) = cont_VV(wf(:,46),wf(:,188)) * den(200)
  A(187) = cont_VV(wf(:,29),wf(:,208)) * den(201)
  A(188) = cont_VV(wf(:,45),wf(:,198)) * den(202)
  A(189) = cont_VV(wf(:,45),wf(:,200)) * den(203)
  A(190) = cont_VV(wf(:,39),wf(:,207)) * den(204)
  A(191) = cont_VV(wf(:,46),wf(:,198)) * den(205)
  A(192) = cont_VV(wf(:,46),wf(:,200)) * den(206)
  A(193) = cont_VV(wf(:,39),wf(:,208)) * den(207)
  A(194) = cont_VV(wf(:,10),wf(:,209)) * den(208)
  A(195) = cont_VV(wf(:,10),wf(:,210)) * den(209)
  A(196) = cont_VV(wf(:,47),wf(:,157)) * den(210)
  A(197) = cont_VV(wf(:,10),wf(:,211)) * den(211)
  A(198) = cont_VV(wf(:,10),wf(:,212)) * den(212)
  A(199) = cont_VV(wf(:,48),wf(:,157)) * den(213)
  A(200) = cont_VV(wf(:,20),wf(:,209)) * den(214)
  A(201) = cont_VV(wf(:,20),wf(:,210)) * den(215)
  A(202) = cont_VV(wf(:,47),wf(:,169)) * den(216)
  A(203) = cont_VV(wf(:,20),wf(:,211)) * den(217)
  A(204) = cont_VV(wf(:,20),wf(:,212)) * den(218)
  A(205) = cont_VV(wf(:,48),wf(:,169)) * den(219)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(205)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((A(9)+A(11)+A(12)+A(14)+A(15)+A(16)+A(21)+A(22)+A(23)+A(24))*f(1))/6._/**/REALKIND
  M1(2) = ((-A(2)-A(4)-A(7)-A(9)-A(14)-A(16)-A(17)-A(19)-A(22)-A(24))*f(1))/2._/**/REALKIND+(CI*(-A(3)-A(6)+A(10) &
       +A(13))*f(2))/2._/**/REALKIND
  M1(3) = ((-A(1)-A(5)-A(8)-A(11)-A(12)-A(15)-A(18)-A(20)-A(21)-A(23))*f(1))/2._/**/REALKIND+(CI*(A(3)+A(6)-A(10) &
       -A(13))*f(2))/2._/**/REALKIND
  M1(4) = ((A(1)+A(2)+A(4)+A(5)+A(7)+A(8)+A(17)+A(18)+A(19)+A(20))*f(1))/6._/**/REALKIND

  M2(1) = ((A(25)+A(26))*f(3))/12._/**/REALKIND+((-A(158)-A(159)-A(160)-A(162)-A(163)-A(165)-A(167)-A(168)-A(169)-A(171)-A(173) &
       -A(174)-A(176)-A(177)-A(178)-A(179)-A(180)-A(181)-A(194)-A(195)-A(196)-A(197)-A(198)-A(199)-A(200)-A(201)-A(202)-A(203) &
       -A(204)-A(205))*f(3))/6._/**/REALKIND+((A(58)+A(60)+A(61)+A(63)+A(74)+A(75)+A(82)+A(83)+A(86)+A(88)+A(89)+A(91)+A(92)+A(93) &
       +A(94)+A(96)+A(97)+A(98)+A(104)+A(105)+A(106)+A(107)+A(111)+A(112)+A(113)+A(114)+A(120)+A(121)+A(122) &
       +A(123))*f(5))/6._/**/REALKIND+((A(65)+A(67)+A(76)+A(77)+A(84)+A(85)+A(99)+A(100)+A(115)+A(117))*f(7))/6._/**/REALKIND+(( &
       -A(64)-A(66)-A(68)-A(69))*f(11))/6._/**/REALKIND
  M2(2) = -(A(25)*f(3))/4._/**/REALKIND+((A(137)+A(140)+A(141)+A(143)+A(144)+A(145)+A(152)+A(153)+A(154)+A(158)+A(159)+A(160) &
       +A(171)+A(173)+A(174)+A(179)+A(180)+A(181)+A(182)+A(183)+A(184)+A(188)+A(189)+A(190)+A(197)+A(198)+A(199)+A(203)+A(204) &
       +A(205))*f(3))/2._/**/REALKIND+(CI*(A(138)+A(139)+A(142)+A(147)+A(148)+A(150)-A(161)-A(164)-A(166)-A(170)-A(172) &
       -A(175))*f(4))/2._/**/REALKIND+((-A(31)-A(33)-A(43)-A(45)-A(52)-A(54)-A(58)-A(63)-A(70)-A(72)-A(75)-A(79)-A(81)-A(83)-A(86) &
       -A(91)-A(93)-A(96)-A(98)-A(102)-A(105)-A(107)-A(111)-A(113)-A(118)-A(120)-A(122)-A(127)-A(129) &
       -A(132))*f(5))/2._/**/REALKIND+(CI*(-A(42)-A(50)+A(87)+A(90)+A(95)+A(110)-A(128)-A(131))*f(6))/2._/**/REALKIND+((-A(36) &
       -A(47)-A(56)-A(67)-A(77)-A(85)-A(100)-A(108)-A(115)-A(124))*f(7))/2._/**/REALKIND+(CI*(-A(49)-A(57)+A(101) &
       +A(116))*f(8))/2._/**/REALKIND+(CI*(-A(32)-A(35)+A(59)+A(62))*f(9))/2._/**/REALKIND+(CI*(-A(27)+A(28) &
       +A(29))*f(10))/2._/**/REALKIND+((A(37)+A(41)+A(66)+A(68))*f(11))/2._/**/REALKIND
  M2(3) = -(A(26)*f(3))/4._/**/REALKIND+((A(134)+A(135)+A(136)+A(146)+A(149)+A(151)+A(155)+A(156)+A(157)+A(162)+A(163)+A(165) &
       +A(167)+A(168)+A(169)+A(176)+A(177)+A(178)+A(185)+A(186)+A(187)+A(191)+A(192)+A(193)+A(194)+A(195)+A(196)+A(200)+A(201) &
       +A(202))*f(3))/2._/**/REALKIND+(CI*(-A(138)-A(139)-A(142)-A(147)-A(148)-A(150)+A(161)+A(164)+A(166)+A(170)+A(172) &
       +A(175))*f(4))/2._/**/REALKIND+((-A(30)-A(34)-A(44)-A(46)-A(51)-A(53)-A(60)-A(61)-A(71)-A(73)-A(74)-A(78)-A(80)-A(82)-A(88) &
       -A(89)-A(92)-A(94)-A(97)-A(103)-A(104)-A(106)-A(112)-A(114)-A(119)-A(121)-A(123)-A(126)-A(130) &
       -A(133))*f(5))/2._/**/REALKIND+(CI*(A(42)+A(50)-A(87)-A(90)-A(95)-A(110)+A(128)+A(131))*f(6))/2._/**/REALKIND+((-A(38) &
       -A(48)-A(55)-A(65)-A(76)-A(84)-A(99)-A(109)-A(117)-A(125))*f(7))/2._/**/REALKIND+(CI*(A(49)+A(57)-A(101) &
       -A(116))*f(8))/2._/**/REALKIND+(CI*(A(32)+A(35)-A(59)-A(62))*f(9))/2._/**/REALKIND+(CI*(A(27)-A(28) &
       -A(29))*f(10))/2._/**/REALKIND+((A(39)+A(40)+A(64)+A(69))*f(11))/2._/**/REALKIND
  M2(4) = ((A(25)+A(26))*f(3))/12._/**/REALKIND+((-A(134)-A(135)-A(136)-A(137)-A(140)-A(141)-A(143)-A(144)-A(145)-A(146)-A(149) &
       -A(151)-A(152)-A(153)-A(154)-A(155)-A(156)-A(157)-A(182)-A(183)-A(184)-A(185)-A(186)-A(187)-A(188)-A(189)-A(190)-A(191) &
       -A(192)-A(193))*f(3))/6._/**/REALKIND+((A(30)+A(31)+A(33)+A(34)+A(43)+A(44)+A(45)+A(46)+A(51)+A(52)+A(53)+A(54)+A(70)+A(71) &
       +A(72)+A(73)+A(78)+A(79)+A(80)+A(81)+A(102)+A(103)+A(118)+A(119)+A(126)+A(127)+A(129)+A(130)+A(132) &
       +A(133))*f(5))/6._/**/REALKIND+((A(36)+A(38)+A(47)+A(48)+A(55)+A(56)+A(108)+A(109)+A(124)+A(125))*f(7))/6._/**/REALKIND+(( &
       -A(37)-A(39)-A(40)-A(41))*f(11))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppzjjj_ddxssxzg_1_/**/REALKIND
