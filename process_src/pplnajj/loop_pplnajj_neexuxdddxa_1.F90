
module ol_colourmatrix_pplnajj_neexuxdddxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2), K2(2,2), KL(2,2)
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
  K1(27,:) = [ -12,  -4]
  K1(28,:) = [  -4,   0]
  K1(29,:) = [   0,   4]
  K1(30,:) = [   4,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   4]
  K1(38,:) = [   4,   0]
  K1(39,:) = [ -12,  -4]
  K1(40,:) = [  -4,   0]
  K1(41,:) = [   0,  -4]
  K1(42,:) = [  -4, -12]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [   0,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [   0,   0]
  K1(58,:) = [   0,   0]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplnajj_neexuxdddxa_1_/**/REALKIND



module ol_forced_parameters_pplnajj_neexuxdddxa_1_/**/REALKIND
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
end module ol_forced_parameters_pplnajj_neexuxdddxa_1_/**/REALKIND

module ol_loop_pplnajj_neexuxdddxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(25), c(47)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:288)
  ! denominators
  complex(REALKIND), save :: den(365)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,128)
  ! zero helicity identifier
  logical,           save :: zerohel(128) = .true., zerohel_ct(128) = .true.

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
    f( 1) = (CI*eQED**3*gQCD**2)/(6._/**/REALKIND*sw**2)
    f( 2) = (CI*eQED**3*gQCD**2)/(3._/**/REALKIND*sw**2)
    f( 3) = (CI*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f( 6) = (CI*countertermnorm*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(6._/**/REALKIND*sw**2)
    f(14) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(3._/**/REALKIND*sw**2)
    f(15) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(16) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*12._/**/REALKIND)
    f(17) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(18) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(19) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(20) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(21) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(22) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(23) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(24) = (2*eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(25) = (eQED**3*gQCD**4*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(13), 27*CI*f(13), 9*CI*f(14), 27*CI*f(14), 9*CI*f(15), 27*CI*f(15), 18*f(16), 54*f(16), f(17), 3*f(17), 6*f(17) &
    , 8*f(17), 10*f(17), 18*f(17), 21*f(17), 24*f(17), 54*f(17), 18*f(18), 54*f(18), f(19), 3*f(19), 6*f(19), 8*f(19), 10*f(19) &
    , 18*f(19), 21*f(19), 24*f(19), 54*f(19), f(20), 3*f(20), 6*f(20), 8*f(20), 10*f(20), 18*f(20), 21*f(20), 24*f(20), 54*f(20) &
    , 3*f(21), 9*f(21), 3*f(22), 9*f(22), 3*f(23), 9*f(23), 3*f(24), 9*f(24), 3*f(25), 9*f(25) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(208)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_AV_Q(wf(:,-2),wf(:,-6),wf(:,2))
  call vert_QA_V(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,68),ZERO,0_intkind1,wf(:,5))
  call vert_WQ_A(wf(:,4),wf(:,-4),wf(:,6))
  call vert_AV_Q(wf(:,5),wf(:,3),wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,19),ZERO,0_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,3),wf(:,-4),wf(:,9))
  call vert_AW_Q(wf(:,5),wf(:,4),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,56),ZERO,0_intkind1,wf(:,11))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,12),Q(:,80),ZERO,0_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,-2),wf(:,4),wf(:,14))
  call vert_VQ_A(wf(:,3),wf(:,13),wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,16))
  call vert_AV_Q(wf(:,-2),wf(:,3),wf(:,17))
  call vert_WQ_A(wf(:,4),wf(:,13),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,44),ZERO,0_intkind1,wf(:,19))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,20))
  call vert_AV_Q(wf(:,19),wf(:,-6),wf(:,21))
  call vert_UV_W(wf(:,-6),Q(:,64),wf(:,4),Q(:,3),wf(:,22))
  call prop_W_W(wf(:,22),Q(:,67),MW,1_intkind1,wf(:,23))
  call vert_QA_W(wf(:,-4),wf(:,19),wf(:,24))
  call vert_QA_W(wf(:,11),wf(:,-2),wf(:,25))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,26))
  call vert_WQ_A(wf(:,4),wf(:,-3),wf(:,27))
  call vert_AV_Q(wf(:,5),wf(:,26),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,11),ZERO,0_intkind1,wf(:,29))
  call vert_VQ_A(wf(:,26),wf(:,-3),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,56),ZERO,0_intkind1,wf(:,31))
  call vert_VQ_A(wf(:,-6),wf(:,-3),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,72),ZERO,0_intkind1,wf(:,33))
  call vert_VQ_A(wf(:,26),wf(:,33),wf(:,34))
  call vert_AV_Q(wf(:,-2),wf(:,26),wf(:,35))
  call vert_WQ_A(wf(:,4),wf(:,33),wf(:,36))
  call prop_A_Q(wf(:,35),Q(:,52),ZERO,0_intkind1,wf(:,37))
  call vert_AV_Q(wf(:,37),wf(:,-6),wf(:,38))
  call vert_QA_W(wf(:,-3),wf(:,37),wf(:,39))
  call vert_QA_W(wf(:,31),wf(:,-2),wf(:,40))
  call vert_QA_V(wf(:,33),wf(:,-5),wf(:,41))
  call vert_QA_V(wf(:,-4),wf(:,16),wf(:,42))
  call vert_QA_V(wf(:,8),wf(:,-2),wf(:,43))
  call vert_QA_V(wf(:,13),wf(:,-5),wf(:,44))
  call vert_QA_V(wf(:,-3),wf(:,16),wf(:,45))
  call vert_QA_V(wf(:,29),wf(:,-2),wf(:,46))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,96),ZERO,0_intkind1,wf(:,48))
  call vert_QA_V(wf(:,-4),wf(:,48),wf(:,49))
  call vert_QA_V(wf(:,-3),wf(:,48),wf(:,50))
  call vert_AV_Q(wf(:,-2),wf(:,50),wf(:,51))
  call vert_AV_Q(wf(:,-1),wf(:,-6),wf(:,52))
  call prop_A_Q(wf(:,52),Q(:,66),ZERO,0_intkind1,wf(:,53))
  call vert_QA_W(wf(:,0),wf(:,53),wf(:,54))
  call prop_W_W(wf(:,54),Q(:,67),MW,1_intkind1,wf(:,55))
  call vert_AW_Q(wf(:,-2),wf(:,55),wf(:,56))
  call vert_WQ_A(wf(:,55),wf(:,-4),wf(:,57))
  call vert_WQ_A(wf(:,55),wf(:,-3),wf(:,58))
  call counter_AV_Q(wf(:,5),wf(:,3),wf(:,59))
  call counter_AW_Q(wf(:,5),wf(:,4),wf(:,60))
  call counter_VQ_A(wf(:,3),wf(:,13),wf(:,61))
  call counter_WQ_A(wf(:,4),wf(:,13),wf(:,62))
  call counter_AV_Q(wf(:,16),wf(:,-6),wf(:,63))
  call counter_AV_Q(wf(:,19),wf(:,-6),wf(:,64))
  call counter_AV_Q(wf(:,5),wf(:,26),wf(:,65))
  call counter_VQ_A(wf(:,26),wf(:,33),wf(:,66))
  call counter_WQ_A(wf(:,4),wf(:,33),wf(:,67))
  call counter_AV_Q(wf(:,37),wf(:,-6),wf(:,68))
  call counter_QA_V(wf(:,33),wf(:,-5),wf(:,69))
  call counter_QA_V(wf(:,13),wf(:,-5),wf(:,70))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,96),ZERO,0_intkind1,wf(:,72))
  call vert_QA_V(wf(:,-4),wf(:,72),wf(:,73))
  call vert_QA_V(wf(:,-3),wf(:,72),wf(:,74))
  call vert_AV_Q(wf(:,-2),wf(:,74),wf(:,75))
  call counter_VQ_A(wf(:,3),wf(:,-4),wf(:,76))
  call prop_A_Q(wf(:,10),Q(:,71),ZERO,0_intkind1,wf(:,77))
  call counter_WQ_A(wf(:,4),wf(:,-4),wf(:,78))
  call prop_A_Q(wf(:,7),Q(:,108),ZERO,0_intkind1,wf(:,79))
  call prop_Q_A(wf(:,76),Q(:,56),ZERO,0_intkind1,wf(:,80))
  call counter_QA_W(wf(:,-4),wf(:,19),wf(:,81))
  call vert_QA_W(wf(:,80),wf(:,-2),wf(:,82))
  call prop_Q_A(wf(:,78),Q(:,19),ZERO,0_intkind1,wf(:,83))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,84))
  call prop_Q_A(wf(:,84),Q(:,80),ZERO,0_intkind1,wf(:,85))
  call vert_VQ_A(wf(:,3),wf(:,85),wf(:,86))
  call vert_WQ_A(wf(:,4),wf(:,85),wf(:,87))
  call counter_QA_V(wf(:,-4),wf(:,16),wf(:,88))
  call vert_QA_V(wf(:,83),wf(:,-2),wf(:,89))
  call counter_QA_V(wf(:,-4),wf(:,48),wf(:,90))
  call vert_QA_V(wf(:,85),wf(:,-5),wf(:,91))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,92))
  call vert_AV_Q(wf(:,5),wf(:,92),wf(:,93))
  call vert_VQ_A(wf(:,92),wf(:,-3),wf(:,94))
  call prop_Q_A(wf(:,94),Q(:,56),ZERO,0_intkind1,wf(:,95))
  call vert_VQ_A(wf(:,92),wf(:,33),wf(:,96))
  call vert_AV_Q(wf(:,-2),wf(:,92),wf(:,97))
  call prop_A_Q(wf(:,97),Q(:,52),ZERO,0_intkind1,wf(:,98))
  call vert_AV_Q(wf(:,98),wf(:,-6),wf(:,99))
  call vert_QA_W(wf(:,95),wf(:,-2),wf(:,100))
  call vert_QA_W(wf(:,-3),wf(:,98),wf(:,101))
  call counter_VQ_A(wf(:,26),wf(:,-3),wf(:,102))
  call counter_WQ_A(wf(:,4),wf(:,-3),wf(:,103))
  call prop_A_Q(wf(:,28),Q(:,116),ZERO,0_intkind1,wf(:,104))
  call prop_Q_A(wf(:,102),Q(:,56),ZERO,0_intkind1,wf(:,105))
  call counter_QA_W(wf(:,-3),wf(:,37),wf(:,106))
  call vert_QA_W(wf(:,105),wf(:,-2),wf(:,107))
  call prop_Q_A(wf(:,103),Q(:,11),ZERO,0_intkind1,wf(:,108))
  call counter_VQ_A(wf(:,-6),wf(:,-3),wf(:,109))
  call prop_Q_A(wf(:,109),Q(:,72),ZERO,0_intkind1,wf(:,110))
  call vert_VQ_A(wf(:,26),wf(:,110),wf(:,111))
  call vert_WQ_A(wf(:,4),wf(:,110),wf(:,112))
  call counter_QA_V(wf(:,-3),wf(:,16),wf(:,113))
  call vert_QA_V(wf(:,108),wf(:,-2),wf(:,114))
  call counter_QA_V(wf(:,-3),wf(:,48),wf(:,115))
  call vert_AV_Q(wf(:,-2),wf(:,115),wf(:,116))
  call vert_QA_V(wf(:,110),wf(:,-5),wf(:,117))
  call counter_QA_V(wf(:,-3),wf(:,-5),wf(:,118))
  call vert_AV_Q(wf(:,5),wf(:,118),wf(:,119))
  call vert_VQ_A(wf(:,118),wf(:,-4),wf(:,120))
  call prop_Q_A(wf(:,120),Q(:,56),ZERO,0_intkind1,wf(:,121))
  call vert_VQ_A(wf(:,118),wf(:,13),wf(:,122))
  call vert_AV_Q(wf(:,-2),wf(:,118),wf(:,123))
  call prop_A_Q(wf(:,123),Q(:,44),ZERO,0_intkind1,wf(:,124))
  call vert_AV_Q(wf(:,124),wf(:,-6),wf(:,125))
  call vert_QA_W(wf(:,121),wf(:,-2),wf(:,126))
  call vert_QA_W(wf(:,-4),wf(:,124),wf(:,127))
  call counter_AV_Q(wf(:,-2),wf(:,3),wf(:,128))
  call prop_Q_A(wf(:,18),Q(:,83),ZERO,0_intkind1,wf(:,129))
  call counter_AW_Q(wf(:,-2),wf(:,4),wf(:,130))
  call prop_Q_A(wf(:,15),Q(:,120),ZERO,0_intkind1,wf(:,131))
  call prop_A_Q(wf(:,128),Q(:,44),ZERO,0_intkind1,wf(:,132))
  call vert_AV_Q(wf(:,132),wf(:,-6),wf(:,133))
  call vert_QA_W(wf(:,-4),wf(:,132),wf(:,134))
  call counter_QA_W(wf(:,11),wf(:,-2),wf(:,135))
  call prop_A_Q(wf(:,130),Q(:,7),ZERO,0_intkind1,wf(:,136))
  call vert_AV_Q(wf(:,136),wf(:,-6),wf(:,137))
  call counter_AV_Q(wf(:,-2),wf(:,-6),wf(:,138))
  call prop_A_Q(wf(:,138),Q(:,68),ZERO,0_intkind1,wf(:,139))
  call vert_AV_Q(wf(:,139),wf(:,3),wf(:,140))
  call vert_AW_Q(wf(:,139),wf(:,4),wf(:,141))
  call counter_AV_Q(wf(:,-2),wf(:,26),wf(:,142))
  call prop_Q_A(wf(:,36),Q(:,75),ZERO,0_intkind1,wf(:,143))
  call prop_Q_A(wf(:,34),Q(:,120),ZERO,0_intkind1,wf(:,144))
  call prop_A_Q(wf(:,142),Q(:,52),ZERO,0_intkind1,wf(:,145))
  call vert_AV_Q(wf(:,145),wf(:,-6),wf(:,146))
  call vert_QA_W(wf(:,-3),wf(:,145),wf(:,147))
  call counter_QA_W(wf(:,31),wf(:,-2),wf(:,148))
  call vert_AV_Q(wf(:,139),wf(:,26),wf(:,149))
  call counter_QA_V(wf(:,8),wf(:,-2),wf(:,150))
  call vert_QA_V(wf(:,-4),wf(:,136),wf(:,151))
  call counter_QA_V(wf(:,29),wf(:,-2),wf(:,152))
  call vert_QA_V(wf(:,-3),wf(:,136),wf(:,153))
  call counter_AV_Q(wf(:,-2),wf(:,50),wf(:,154))
  call counter_WQ_A(wf(:,55),wf(:,-4),wf(:,155))
  call counter_WQ_A(wf(:,55),wf(:,-3),wf(:,156))
  call counter_AW_Q(wf(:,-2),wf(:,55),wf(:,157))
  call counter_A_Q(ctqq,wf(:,5),Q(:,68),wf(:,158))
  call prop_A_Q(wf(:,158),Q(:,68),ZERO,0_intkind1,wf(:,159))
  call vert_AV_Q(wf(:,159),wf(:,3),wf(:,160))
  call vert_AW_Q(wf(:,159),wf(:,4),wf(:,161))
  call counter_V_V(ctGG,wf(:,3),Q(:,40),wf(:,162))
  call vert_VQ_A(wf(:,162),wf(:,-4),wf(:,163))
  call vert_AV_Q(wf(:,5),wf(:,162),wf(:,164))
  call counter_Q_A(ctqq,wf(:,8),Q(:,19),wf(:,165))
  call counter_Q_A(ctqq,wf(:,11),Q(:,56),wf(:,166))
  call vert_AV_Q(wf(:,-2),wf(:,162),wf(:,167))
  call counter_A_Q(ctqq,wf(:,16),Q(:,7),wf(:,168))
  call counter_A_Q(ctqq,wf(:,19),Q(:,44),wf(:,169))
  call vert_AV_Q(wf(:,16),wf(:,162),wf(:,170))
  call counter_Q_A(ctqq,wf(:,13),Q(:,80),wf(:,171))
  call prop_Q_A(wf(:,171),Q(:,80),ZERO,0_intkind1,wf(:,172))
  call vert_QA_V(wf(:,172),wf(:,16),wf(:,173))
  call vert_WQ_A(wf(:,4),wf(:,172),wf(:,174))
  call vert_VQ_A(wf(:,-6),wf(:,8),wf(:,175))
  call prop_A_Q(wf(:,167),Q(:,44),ZERO,0_intkind1,wf(:,176))
  call vert_WQ_A(wf(:,23),wf(:,-4),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,83),ZERO,0_intkind1,wf(:,178))
  call vert_AW_Q(wf(:,-2),wf(:,23),wf(:,179))
  call prop_A_Q(wf(:,179),Q(:,71),ZERO,0_intkind1,wf(:,180))
  call vert_VQ_A(wf(:,-6),wf(:,11),wf(:,181))
  call prop_Q_A(wf(:,181),Q(:,120),ZERO,0_intkind1,wf(:,182))
  call prop_Q_A(wf(:,175),Q(:,83),ZERO,0_intkind1,wf(:,183))
  call prop_Q_A(wf(:,163),Q(:,56),ZERO,0_intkind1,wf(:,184))
  call prop_A_Q(wf(:,20),Q(:,71),ZERO,0_intkind1,wf(:,185))
  call prop_A_Q(wf(:,21),Q(:,108),ZERO,0_intkind1,wf(:,186))
  call vert_QA_V(wf(:,29),wf(:,159),wf(:,187))
  call counter_V_V(ctGG,wf(:,26),Q(:,48),wf(:,188))
  call vert_VQ_A(wf(:,188),wf(:,-3),wf(:,189))
  call counter_Q_A(ctqq,wf(:,29),Q(:,11),wf(:,190))
  call counter_Q_A(ctqq,wf(:,31),Q(:,56),wf(:,191))
  call vert_AV_Q(wf(:,5),wf(:,188),wf(:,192))
  call vert_AV_Q(wf(:,-2),wf(:,188),wf(:,193))
  call counter_A_Q(ctqq,wf(:,37),Q(:,52),wf(:,194))
  call counter_Q_A(ctqq,wf(:,33),Q(:,72),wf(:,195))
  call prop_Q_A(wf(:,195),Q(:,72),ZERO,0_intkind1,wf(:,196))
  call vert_QA_V(wf(:,196),wf(:,16),wf(:,197))
  call vert_WQ_A(wf(:,4),wf(:,196),wf(:,198))
  call vert_AV_Q(wf(:,16),wf(:,188),wf(:,199))
  call vert_WQ_A(wf(:,23),wf(:,-3),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,75),ZERO,0_intkind1,wf(:,201))
  call vert_VQ_A(wf(:,-6),wf(:,29),wf(:,202))
  call prop_A_Q(wf(:,193),Q(:,52),ZERO,0_intkind1,wf(:,203))
  call vert_VQ_A(wf(:,-6),wf(:,31),wf(:,204))
  call prop_Q_A(wf(:,204),Q(:,120),ZERO,0_intkind1,wf(:,205))
  call prop_Q_A(wf(:,202),Q(:,75),ZERO,0_intkind1,wf(:,206))
  call prop_Q_A(wf(:,189),Q(:,56),ZERO,0_intkind1,wf(:,207))
  call prop_A_Q(wf(:,38),Q(:,116),ZERO,0_intkind1,wf(:,208))
  call vert_VQ_A(wf(:,41),wf(:,-4),wf(:,209))
  call prop_Q_A(wf(:,209),Q(:,120),ZERO,0_intkind1,wf(:,210))
  call vert_AV_Q(wf(:,-2),wf(:,41),wf(:,211))
  call prop_A_Q(wf(:,211),Q(:,108),ZERO,0_intkind1,wf(:,212))
  call counter_V_V(ctGG,wf(:,41),Q(:,104),wf(:,213))
  call vert_QA_V(wf(:,196),wf(:,-5),wf(:,214))
  call vert_VQ_A(wf(:,44),wf(:,-3),wf(:,215))
  call prop_Q_A(wf(:,215),Q(:,120),ZERO,0_intkind1,wf(:,216))
  call vert_AV_Q(wf(:,-2),wf(:,44),wf(:,217))
  call prop_A_Q(wf(:,217),Q(:,116),ZERO,0_intkind1,wf(:,218))
  call counter_V_V(ctGG,wf(:,44),Q(:,112),wf(:,219))
  call vert_QA_V(wf(:,172),wf(:,-5),wf(:,220))
  call vert_VQ_A(wf(:,49),wf(:,-3),wf(:,221))
  call prop_Q_A(wf(:,221),Q(:,120),ZERO,0_intkind1,wf(:,222))
  call vert_AV_Q(wf(:,-2),wf(:,49),wf(:,223))
  call prop_A_Q(wf(:,223),Q(:,116),ZERO,0_intkind1,wf(:,224))
  call counter_V_V(ctGG,wf(:,50),Q(:,104),wf(:,225))
  call vert_VQ_A(wf(:,50),wf(:,-4),wf(:,226))
  call prop_Q_A(wf(:,226),Q(:,120),ZERO,0_intkind1,wf(:,227))
  call counter_V_V(ctGG,wf(:,49),Q(:,112),wf(:,228))
  call prop_A_Q(wf(:,51),Q(:,108),ZERO,0_intkind1,wf(:,229))
  call counter_A_Q(ctqq,wf(:,48),Q(:,96),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,96),ZERO,0_intkind1,wf(:,231))
  call vert_QA_V(wf(:,-3),wf(:,231),wf(:,232))
  call vert_QA_V(wf(:,-4),wf(:,231),wf(:,233))
  call prop_A_Q(wf(:,56),Q(:,71),ZERO,0_intkind1,wf(:,234))
  call prop_Q_A(wf(:,57),Q(:,83),ZERO,0_intkind1,wf(:,235))
  call prop_Q_A(wf(:,58),Q(:,75),ZERO,0_intkind1,wf(:,236))
  call vert_AV_Q(wf(:,16),wf(:,3),wf(:,237))
  call prop_A_Q(wf(:,237),Q(:,47),ZERO,0_intkind1,wf(:,238))
  call vert_VQ_A(wf(:,3),wf(:,8),wf(:,239))
  call prop_Q_A(wf(:,239),Q(:,59),ZERO,0_intkind1,wf(:,240))
  call vert_AW_Q(wf(:,19),wf(:,4),wf(:,241))
  call prop_A_Q(wf(:,241),Q(:,47),ZERO,0_intkind1,wf(:,242))
  call vert_WQ_A(wf(:,4),wf(:,11),wf(:,243))
  call prop_Q_A(wf(:,243),Q(:,59),ZERO,0_intkind1,wf(:,244))
  call vert_AV_Q(wf(:,16),wf(:,26),wf(:,245))
  call prop_A_Q(wf(:,245),Q(:,55),ZERO,0_intkind1,wf(:,246))
  call vert_VQ_A(wf(:,26),wf(:,29),wf(:,247))
  call prop_Q_A(wf(:,247),Q(:,59),ZERO,0_intkind1,wf(:,248))
  call vert_AW_Q(wf(:,37),wf(:,4),wf(:,249))
  call prop_A_Q(wf(:,249),Q(:,55),ZERO,0_intkind1,wf(:,250))
  call vert_WQ_A(wf(:,4),wf(:,31),wf(:,251))
  call prop_Q_A(wf(:,251),Q(:,59),ZERO,0_intkind1,wf(:,252))
  call vert_QA_V(wf(:,29),wf(:,5),wf(:,253))
  call vert_QA_V(wf(:,8),wf(:,5),wf(:,254))
  call vert_QA_V(wf(:,-3),wf(:,77),wf(:,255))
  call vert_QA_V(wf(:,-4),wf(:,77),wf(:,256))
  call vert_QA_V(wf(:,33),wf(:,16),wf(:,257))
  call vert_QA_V(wf(:,143),wf(:,-2),wf(:,258))
  call vert_QA_V(wf(:,13),wf(:,16),wf(:,259))
  call vert_QA_V(wf(:,129),wf(:,-2),wf(:,260))
  call vert_VQ_A(wf(:,45),wf(:,-4),wf(:,261))
  call prop_Q_A(wf(:,261),Q(:,31),ZERO,0_intkind1,wf(:,262))
  call vert_AV_Q(wf(:,-5),wf(:,45),wf(:,263))
  call prop_A_Q(wf(:,263),Q(:,47),ZERO,0_intkind1,wf(:,264))
  call vert_VQ_A(wf(:,42),wf(:,-3),wf(:,265))
  call prop_Q_A(wf(:,265),Q(:,31),ZERO,0_intkind1,wf(:,266))
  call vert_AV_Q(wf(:,-5),wf(:,42),wf(:,267))
  call prop_A_Q(wf(:,267),Q(:,55),ZERO,0_intkind1,wf(:,268))
  call vert_QA_V(wf(:,-3),wf(:,185),wf(:,269))
  call vert_QA_V(wf(:,-4),wf(:,185),wf(:,270))
  call vert_VQ_A(wf(:,46),wf(:,-4),wf(:,271))
  call prop_Q_A(wf(:,271),Q(:,31),ZERO,0_intkind1,wf(:,272))
  call vert_AV_Q(wf(:,-5),wf(:,46),wf(:,273))
  call prop_A_Q(wf(:,273),Q(:,47),ZERO,0_intkind1,wf(:,274))
  call vert_QA_V(wf(:,206),wf(:,-2),wf(:,275))
  call vert_VQ_A(wf(:,43),wf(:,-3),wf(:,276))
  call prop_Q_A(wf(:,276),Q(:,31),ZERO,0_intkind1,wf(:,277))
  call vert_AV_Q(wf(:,-5),wf(:,43),wf(:,278))
  call prop_A_Q(wf(:,278),Q(:,55),ZERO,0_intkind1,wf(:,279))
  call vert_QA_V(wf(:,183),wf(:,-2),wf(:,280))
  call vert_QA_V(wf(:,-3),wf(:,180),wf(:,281))
  call vert_QA_V(wf(:,201),wf(:,-2),wf(:,282))
  call vert_QA_V(wf(:,-4),wf(:,180),wf(:,283))
  call vert_QA_V(wf(:,178),wf(:,-2),wf(:,284))
  call vert_QA_V(wf(:,-3),wf(:,234),wf(:,285))
  call vert_QA_V(wf(:,-4),wf(:,234),wf(:,286))
  call vert_QA_V(wf(:,236),wf(:,-2),wf(:,287))
  call vert_QA_V(wf(:,235),wf(:,-2),wf(:,288))

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
  den(2) = 1 / (Q(5,68))
  den(3) = 1 / (Q(5,40))
  den(5) = 1 / (Q(5,19))
  den(9) = 1 / (Q(5,56))
  den(12) = 1 / (Q(5,80))
  den(14) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,44))
  den(23) = 1 / (Q(5,67) - MW2)
  den(27) = 1 / (Q(5,48))
  den(29) = 1 / (Q(5,11))
  den(34) = 1 / (Q(5,72))
  den(38) = 1 / (Q(5,52))
  den(45) = 1 / (Q(5,104))
  den(49) = 1 / (Q(5,112))
  den(53) = 1 / (Q(5,96))
  den(60) = 1 / (Q(5,66))
  den(66) = 1 / (Q(5,71))
  den(69) = 1 / (Q(5,108))
  den(73) = 1 / (Q(5,116))
  den(76) = 1 / (Q(5,83))
  den(79) = 1 / (Q(5,120))
  den(82) = 1 / (Q(5,75))
  den(165) = 1 / (Q(5,23))
  den(176) = 1 / (Q(5,15))
  den(213) = 1 / (Q(5,47))
  den(216) = 1 / (Q(5,59))
  den(223) = 1 / (Q(5,55))
  den(232) = 1 / (Q(5,79))
  den(235) = 1 / (Q(5,87))
  den(245) = 1 / (Q(5,31))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(2)
  den(10) = den(3)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(3)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(1)*den(12)
  den(19) = den(3)*den(18)
  den(20) = den(17)*den(19)
  den(21) = den(10)*den(15)
  den(22) = den(6)*den(19)
  den(24) = den(1)*den(23)
  den(25) = den(19)*den(24)
  den(26) = den(10)*den(24)
  den(28) = den(2)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(28)*den(30)
  den(32) = den(9)*den(27)
  den(33) = den(8)*den(32)
  den(35) = den(27)*den(34)
  den(36) = den(15)*den(35)
  den(37) = den(1)*den(34)
  den(39) = den(27)*den(38)
  den(40) = den(37)*den(39)
  den(41) = den(15)*den(32)
  den(42) = den(30)*den(39)
  den(43) = den(24)*den(39)
  den(44) = den(24)*den(32)
  den(46) = den(34)*den(45)
  den(47) = den(15)*den(46)
  den(48) = den(6)*den(46)
  den(50) = den(12)*den(49)
  den(51) = den(15)*den(50)
  den(52) = den(30)*den(50)
  den(54) = den(49)*den(53)
  den(55) = den(15)*den(54)
  den(56) = den(45)*den(53)
  den(57) = den(15)*den(56)
  den(58) = den(30)*den(54)
  den(59) = den(6)*den(56)
  den(61) = den(23)*den(60)
  den(62) = den(10)*den(61)
  den(63) = den(19)*den(61)
  den(64) = den(32)*den(61)
  den(65) = den(39)*den(61)
  den(67) = den(8)*den(66)
  den(68) = den(3)*den(67)
  den(70) = den(4)*den(69)
  den(71) = den(1)*den(70)
  den(72) = den(27)*den(67)
  den(74) = den(28)*den(73)
  den(75) = den(1)*den(74)
  den(77) = den(17)*den(76)
  den(78) = den(3)*den(77)
  den(80) = den(13)*den(79)
  den(81) = den(1)*den(80)
  den(83) = den(37)*den(82)
  den(84) = den(27)*den(83)
  den(85) = den(35)*den(79)
  den(86) = den(1)*den(85)
  den(87) = den(2)**2
  den(88) = den(3)*den(87)
  den(89) = den(6)*den(88)
  den(90) = den(1)*den(87)
  den(91) = den(10)*den(90)
  den(92) = den(3)**2
  den(93) = den(67)*den(92)
  den(94) = den(2)*den(92)
  den(95) = den(6)*den(94)
  den(96) = den(6)*den(70)
  den(97) = den(10)*den(67)
  den(98) = den(77)*den(92)
  den(99) = den(15)*den(80)
  den(100) = den(19)*den(77)
  den(101) = den(15)*den(92)
  den(102) = den(12)*den(101)
  den(103) = den(12)**2
  den(104) = den(15)*den(103)
  den(105) = den(3)*den(104)
  den(106) = den(1)*den(103)
  den(107) = den(19)*den(106)
  den(108) = den(18)*den(92)
  den(109) = den(6)*den(108)
  den(110) = den(24)*den(108)
  den(111) = den(24)*den(76)
  den(112) = den(19)*den(111)
  den(113) = den(24)*den(66)
  den(114) = den(10)*den(113)
  den(115) = den(10)*den(79)
  den(116) = den(15)*den(115)
  den(117) = den(6)*den(76)
  den(118) = den(19)*den(117)
  den(119) = den(9)*den(92)
  den(120) = den(15)*den(119)
  den(121) = den(92)*den(113)
  den(122) = den(15)*den(66)
  den(123) = den(10)*den(122)
  den(124) = den(19)*den(69)
  den(125) = den(6)*den(124)
  den(126) = den(30)*den(87)
  den(127) = den(27)*den(126)
  den(128) = den(32)*den(90)
  den(129) = den(27)**2
  den(130) = den(67)*den(129)
  den(131) = den(30)*den(74)
  den(132) = den(32)*den(67)
  den(133) = den(2)*den(129)
  den(134) = den(30)*den(133)
  den(135) = den(83)*den(129)
  den(136) = den(15)*den(85)
  den(137) = den(39)*den(83)
  den(138) = den(34)**2
  den(139) = den(15)*den(138)
  den(140) = den(27)*den(139)
  den(141) = den(1)*den(138)
  den(142) = den(39)*den(141)
  den(143) = den(15)*den(129)
  den(144) = den(34)*den(143)
  den(145) = den(24)*den(82)
  den(146) = den(39)*den(145)
  den(147) = den(32)*den(113)
  den(148) = den(38)*den(129)
  den(149) = den(30)*den(148)
  den(150) = den(24)*den(148)
  den(151) = den(32)*den(79)
  den(152) = den(15)*den(151)
  den(153) = den(30)*den(82)
  den(154) = den(39)*den(153)
  den(155) = den(9)*den(129)
  den(156) = den(15)*den(155)
  den(157) = den(113)*den(129)
  den(158) = den(32)*den(122)
  den(159) = den(39)*den(73)
  den(160) = den(30)*den(159)
  den(161) = den(46)*den(79)
  den(162) = den(15)*den(161)
  den(163) = den(46)*den(69)
  den(164) = den(6)*den(163)
  den(166) = den(6)*den(165)
  den(167) = den(46)*den(166)
  den(168) = den(15)*den(165)
  den(169) = den(138)*den(168)
  den(170) = den(138)*den(166)
  den(171) = den(46)*den(168)
  den(172) = den(50)*den(79)
  den(173) = den(15)*den(172)
  den(174) = den(50)*den(73)
  den(175) = den(30)*den(174)
  den(177) = den(30)*den(176)
  den(178) = den(50)*den(177)
  den(179) = den(15)*den(176)
  den(180) = den(50)*den(179)
  den(181) = den(103)*den(179)
  den(182) = den(103)*den(177)
  den(183) = den(54)*den(79)
  den(184) = den(15)*den(183)
  den(185) = den(54)*den(73)
  den(186) = den(30)*den(185)
  den(187) = den(56)*den(166)
  den(188) = den(56)*den(79)
  den(189) = den(15)*den(188)
  den(190) = den(54)*den(177)
  den(191) = den(56)*den(69)
  den(192) = den(6)*den(191)
  den(193) = den(54)*den(179)
  den(194) = den(56)*den(168)
  den(195) = den(53)**2
  den(196) = den(45)*den(195)
  den(197) = den(15)*den(196)
  den(198) = den(166)*den(195)
  den(199) = den(179)*den(195)
  den(200) = den(177)*den(195)
  den(201) = den(61)*den(108)
  den(202) = den(61)*den(66)
  den(203) = den(10)*den(202)
  den(204) = den(61)*den(76)
  den(205) = den(19)*den(204)
  den(206) = den(92)*den(202)
  den(207) = den(32)*den(202)
  den(208) = den(61)*den(82)
  den(209) = den(39)*den(208)
  den(210) = den(61)*den(148)
  den(211) = den(129)*den(202)
  den(212) = den(3)*den(15)
  den(214) = den(212)*den(213)
  den(215) = den(3)*den(6)
  den(217) = den(215)*den(216)
  den(218) = den(1)*den(19)
  den(219) = den(213)*den(218)
  den(220) = den(1)*den(10)
  den(221) = den(216)*den(220)
  den(222) = den(15)*den(27)
  den(224) = den(222)*den(223)
  den(225) = den(27)*den(30)
  den(226) = den(216)*den(225)
  den(227) = den(1)*den(39)
  den(228) = den(223)*den(227)
  den(229) = den(1)*den(32)
  den(230) = den(216)*den(229)
  den(231) = den(2)*den(30)
  den(233) = den(231)*den(232)
  den(234) = den(2)*den(6)
  den(236) = den(234)*den(235)
  den(237) = den(67)*den(232)
  den(238) = den(67)*den(235)
  den(239) = den(15)*den(34)
  den(240) = den(232)*den(239)
  den(241) = den(83)*den(232)
  den(242) = den(12)*den(15)
  den(243) = den(235)*den(242)
  den(244) = den(77)*den(235)
  den(246) = den(179)*den(245)
  den(247) = den(179)*den(213)
  den(248) = den(168)*den(245)
  den(249) = den(168)*den(223)
  den(250) = den(122)*den(232)
  den(251) = den(122)*den(235)
  den(252) = den(177)*den(245)
  den(253) = den(177)*den(213)
  den(254) = den(153)*den(232)
  den(255) = den(166)*den(245)
  den(256) = den(166)*den(223)
  den(257) = den(117)*den(235)
  den(258) = den(113)*den(232)
  den(259) = den(145)*den(232)
  den(260) = den(113)*den(235)
  den(261) = den(111)*den(235)
  den(262) = den(202)*den(232)
  den(263) = den(202)*den(235)
  den(264) = den(208)*den(232)
  den(265) = den(204)*den(235)
  den(266) = den(2)*den(3)*den(6)
  den(267) = den(1)*den(2)*den(10)
  den(268) = den(1)*den(2)*den(3)
  den(269) = den(3)*den(12)*den(15)
  den(270) = den(1)*den(12)*den(19)
  den(271) = den(1)*den(3)*den(12)
  den(272) = den(3)*den(168)
  den(273) = den(3)*den(122)
  den(274) = den(3)*den(166)
  den(275) = den(3)*den(117)
  den(276) = den(3)*den(113)
  den(277) = den(3)*den(111)
  den(278) = den(3)*den(24)
  den(279) = den(1)*den(124)
  den(280) = den(1)*den(115)
  den(281) = den(1)*den(3)
  den(282) = den(2)*den(27)*den(30)
  den(283) = den(1)*den(2)*den(32)
  den(284) = den(1)*den(2)*den(27)
  den(285) = den(15)*den(27)*den(34)
  den(286) = den(1)*den(34)*den(39)
  den(287) = den(1)*den(27)*den(34)
  den(288) = den(27)*den(179)
  den(289) = den(27)*den(122)
  den(290) = den(27)*den(177)
  den(291) = den(27)*den(153)
  den(292) = den(27)*den(113)
  den(293) = den(27)*den(145)
  den(294) = den(24)*den(27)
  den(295) = den(1)*den(159)
  den(296) = den(1)*den(151)
  den(297) = den(1)*den(27)
  den(298) = den(34)*den(168)
  den(299) = den(34)*den(166)
  den(300) = den(6)*den(34)
  den(301) = den(1)*den(163)
  den(302) = den(1)*den(161)
  den(303) = den(1)*den(46)
  den(304) = den(12)*den(179)
  den(305) = den(12)*den(177)
  den(306) = den(12)*den(30)
  den(307) = den(1)*den(174)
  den(308) = den(1)*den(172)
  den(309) = den(1)*den(50)
  den(310) = den(53)*den(179)
  den(311) = den(53)*den(168)
  den(312) = den(15)*den(53)
  den(313) = den(53)*den(177)
  den(314) = den(30)*den(53)
  den(315) = den(53)*den(166)
  den(316) = den(6)*den(53)
  den(317) = den(1)*den(191)
  den(318) = den(1)*den(185)
  den(319) = den(1)*den(188)
  den(320) = den(1)*den(183)
  den(321) = den(1)*den(56)
  den(322) = den(1)*den(54)
  den(323) = den(1)*den(53)
  den(324) = den(3)*den(202)
  den(325) = den(3)*den(204)
  den(326) = den(3)*den(61)
  den(327) = den(27)*den(202)
  den(328) = den(27)*den(208)
  den(329) = den(27)*den(61)
  den(330) = den(3)*den(236)
  den(331) = den(2)*den(217)
  den(332) = den(3)*den(238)
  den(333) = den(2)*den(221)
  den(334) = den(12)*den(214)
  den(335) = den(3)*den(243)
  den(336) = den(12)*den(219)
  den(337) = den(3)*den(244)
  den(338) = den(3)*den(251)
  den(339) = den(3)*den(257)
  den(340) = den(3)*den(260)
  den(341) = den(3)*den(261)
  den(342) = den(27)*den(233)
  den(343) = den(2)*den(226)
  den(344) = den(27)*den(237)
  den(345) = den(2)*den(230)
  den(346) = den(27)*den(240)
  den(347) = den(34)*den(224)
  den(348) = den(34)*den(228)
  den(349) = den(27)*den(241)
  den(350) = den(27)*den(250)
  den(351) = den(27)*den(254)
  den(352) = den(27)*den(258)
  den(353) = den(27)*den(259)
  den(354) = den(34)*den(249)
  den(355) = den(34)*den(256)
  den(356) = den(12)*den(247)
  den(357) = den(12)*den(253)
  den(358) = den(53)*den(246)
  den(359) = den(53)*den(248)
  den(360) = den(53)*den(252)
  den(361) = den(53)*den(255)
  den(362) = den(3)*den(263)
  den(363) = den(3)*den(265)
  den(364) = den(27)*den(262)
  den(365) = den(27)*den(264)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(208)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_QA(wf(:,11),wf(:,20)) * den(21)
  A(6) = cont_QA(wf(:,8),wf(:,21)) * den(22)
  A(7) = cont_VV(wf(:,23),wf(:,24)) * den(25)
  A(8) = cont_VV(wf(:,23),wf(:,25)) * den(26)
  A(9) = cont_QA(wf(:,28),wf(:,29)) * den(31)
  A(10) = cont_QA(wf(:,10),wf(:,31)) * den(33)
  A(11) = cont_QA(wf(:,16),wf(:,34)) * den(36)
  A(12) = cont_QA(wf(:,36),wf(:,37)) * den(40)
  A(13) = cont_QA(wf(:,20),wf(:,31)) * den(41)
  A(14) = cont_QA(wf(:,29),wf(:,38)) * den(42)
  A(15) = cont_VV(wf(:,23),wf(:,39)) * den(43)
  A(16) = cont_VV(wf(:,23),wf(:,40)) * den(44)
  A(17) = cont_VV(wf(:,41),wf(:,42)) * den(47)
  A(18) = cont_VV(wf(:,41),wf(:,43)) * den(48)
  A(19) = cont_VV(wf(:,44),wf(:,45)) * den(51)
  A(20) = cont_VV(wf(:,44),wf(:,46)) * den(52)
  A(21) = cont_VV(wf(:,45),wf(:,49)) * den(55)
  A(22) = cont_VV(wf(:,42),wf(:,50)) * den(57)
  A(23) = cont_VV(wf(:,46),wf(:,49)) * den(58)
  A(24) = cont_QA(wf(:,8),wf(:,51)) * den(59)
  A(25) = cont_QA(wf(:,11),wf(:,56)) * den(62)
  A(26) = cont_QA(wf(:,19),wf(:,57)) * den(63)
  A(27) = cont_QA(wf(:,31),wf(:,56)) * den(64)
  A(28) = cont_QA(wf(:,37),wf(:,58)) * den(65)

  A(29) = cont_QA(wf(:,8),wf(:,59)) * den(7)
  A(30) = cont_QA(wf(:,11),wf(:,60)) * den(11)
  A(31) = cont_QA(wf(:,16),wf(:,61)) * den(16)
  A(32) = cont_QA(wf(:,19),wf(:,62)) * den(20)
  A(33) = cont_QA(wf(:,11),wf(:,63)) * den(21)
  A(34) = cont_QA(wf(:,8),wf(:,64)) * den(22)
  A(35) = cont_QA(wf(:,29),wf(:,65)) * den(31)
  A(36) = cont_QA(wf(:,31),wf(:,60)) * den(33)
  A(37) = cont_QA(wf(:,16),wf(:,66)) * den(36)
  A(38) = cont_QA(wf(:,37),wf(:,67)) * den(40)
  A(39) = cont_QA(wf(:,31),wf(:,63)) * den(41)
  A(40) = cont_QA(wf(:,29),wf(:,68)) * den(42)
  A(41) = cont_VV(wf(:,42),wf(:,69)) * den(47)
  A(42) = cont_VV(wf(:,43),wf(:,69)) * den(48)
  A(43) = cont_VV(wf(:,45),wf(:,70)) * den(51)
  A(44) = cont_VV(wf(:,46),wf(:,70)) * den(52)
  A(45) = cont_VV(wf(:,45),wf(:,73)) * den(55)
  A(46) = cont_VV(wf(:,42),wf(:,74)) * den(57)
  A(47) = cont_VV(wf(:,46),wf(:,73)) * den(58)
  A(48) = cont_QA(wf(:,8),wf(:,75)) * den(59)
  A(49) = cont_QA(wf(:,76),wf(:,77)) * den(68)
  A(50) = cont_QA(wf(:,78),wf(:,79)) * den(71)
  A(51) = cont_QA(wf(:,20),wf(:,80)) * den(21)
  A(52) = cont_VV(wf(:,23),wf(:,81)) * den(25)
  A(53) = cont_VV(wf(:,23),wf(:,82)) * den(26)
  A(54) = cont_QA(wf(:,21),wf(:,83)) * den(22)
  A(55) = cont_QA(wf(:,16),wf(:,86)) * den(16)
  A(56) = cont_QA(wf(:,19),wf(:,87)) * den(20)
  A(57) = cont_VV(wf(:,41),wf(:,88)) * den(47)
  A(58) = cont_VV(wf(:,41),wf(:,89)) * den(48)
  A(59) = cont_VV(wf(:,45),wf(:,90)) * den(55)
  A(60) = cont_VV(wf(:,50),wf(:,88)) * den(57)
  A(61) = cont_VV(wf(:,46),wf(:,90)) * den(58)
  A(62) = cont_QA(wf(:,51),wf(:,83)) * den(59)
  A(63) = cont_VV(wf(:,45),wf(:,91)) * den(51)
  A(64) = cont_VV(wf(:,46),wf(:,91)) * den(52)
  A(65) = cont_QA(wf(:,29),wf(:,93)) * den(31)
  A(66) = cont_QA(wf(:,10),wf(:,95)) * den(33)
  A(67) = cont_QA(wf(:,16),wf(:,96)) * den(36)
  A(68) = cont_QA(wf(:,36),wf(:,98)) * den(40)
  A(69) = cont_QA(wf(:,20),wf(:,95)) * den(41)
  A(70) = cont_QA(wf(:,29),wf(:,99)) * den(42)
  A(71) = cont_VV(wf(:,23),wf(:,100)) * den(44)
  A(72) = cont_VV(wf(:,23),wf(:,101)) * den(43)
  A(73) = cont_QA(wf(:,77),wf(:,102)) * den(72)
  A(74) = cont_QA(wf(:,103),wf(:,104)) * den(75)
  A(75) = cont_QA(wf(:,20),wf(:,105)) * den(41)
  A(76) = cont_VV(wf(:,23),wf(:,106)) * den(43)
  A(77) = cont_VV(wf(:,23),wf(:,107)) * den(44)
  A(78) = cont_QA(wf(:,38),wf(:,108)) * den(42)
  A(79) = cont_QA(wf(:,16),wf(:,111)) * den(36)
  A(80) = cont_QA(wf(:,37),wf(:,112)) * den(40)
  A(81) = cont_VV(wf(:,44),wf(:,113)) * den(51)
  A(82) = cont_VV(wf(:,44),wf(:,114)) * den(52)
  A(83) = cont_VV(wf(:,42),wf(:,115)) * den(57)
  A(84) = cont_VV(wf(:,49),wf(:,113)) * den(55)
  A(85) = cont_QA(wf(:,8),wf(:,116)) * den(59)
  A(86) = cont_VV(wf(:,49),wf(:,114)) * den(58)
  A(87) = cont_VV(wf(:,42),wf(:,117)) * den(47)
  A(88) = cont_VV(wf(:,43),wf(:,117)) * den(48)
  A(89) = cont_QA(wf(:,8),wf(:,119)) * den(7)
  A(90) = cont_QA(wf(:,10),wf(:,121)) * den(11)
  A(91) = cont_QA(wf(:,16),wf(:,122)) * den(16)
  A(92) = cont_QA(wf(:,18),wf(:,124)) * den(20)
  A(93) = cont_QA(wf(:,20),wf(:,121)) * den(21)
  A(94) = cont_QA(wf(:,8),wf(:,125)) * den(22)
  A(95) = cont_VV(wf(:,23),wf(:,126)) * den(26)
  A(96) = cont_VV(wf(:,23),wf(:,127)) * den(25)
  A(97) = cont_QA(wf(:,128),wf(:,129)) * den(78)
  A(98) = cont_QA(wf(:,130),wf(:,131)) * den(81)
  A(99) = cont_QA(wf(:,8),wf(:,133)) * den(22)
  A(100) = cont_VV(wf(:,23),wf(:,134)) * den(25)
  A(101) = cont_VV(wf(:,23),wf(:,135)) * den(26)
  A(102) = cont_QA(wf(:,11),wf(:,137)) * den(21)
  A(103) = cont_QA(wf(:,8),wf(:,140)) * den(7)
  A(104) = cont_QA(wf(:,11),wf(:,141)) * den(11)
  A(105) = cont_QA(wf(:,142),wf(:,143)) * den(84)
  A(106) = cont_QA(wf(:,130),wf(:,144)) * den(86)
  A(107) = cont_QA(wf(:,29),wf(:,146)) * den(42)
  A(108) = cont_VV(wf(:,23),wf(:,147)) * den(43)
  A(109) = cont_VV(wf(:,23),wf(:,148)) * den(44)
  A(110) = cont_QA(wf(:,31),wf(:,137)) * den(41)
  A(111) = cont_QA(wf(:,29),wf(:,149)) * den(31)
  A(112) = cont_QA(wf(:,31),wf(:,141)) * den(33)
  A(113) = cont_VV(wf(:,41),wf(:,150)) * den(48)
  A(114) = cont_VV(wf(:,41),wf(:,151)) * den(47)
  A(115) = cont_VV(wf(:,44),wf(:,152)) * den(52)
  A(116) = cont_VV(wf(:,44),wf(:,153)) * den(51)
  A(117) = cont_VV(wf(:,49),wf(:,152)) * den(58)
  A(118) = cont_QA(wf(:,8),wf(:,154)) * den(59)
  A(119) = cont_VV(wf(:,49),wf(:,153)) * den(55)
  A(120) = cont_VV(wf(:,50),wf(:,151)) * den(57)
  A(121) = cont_QA(wf(:,56),wf(:,80)) * den(62)
  A(122) = cont_QA(wf(:,19),wf(:,155)) * den(63)
  A(123) = cont_QA(wf(:,56),wf(:,95)) * den(64)
  A(124) = cont_QA(wf(:,58),wf(:,98)) * den(65)
  A(125) = cont_QA(wf(:,56),wf(:,105)) * den(64)
  A(126) = cont_QA(wf(:,37),wf(:,156)) * den(65)
  A(127) = cont_QA(wf(:,56),wf(:,121)) * den(62)
  A(128) = cont_QA(wf(:,57),wf(:,124)) * den(63)
  A(129) = cont_QA(wf(:,57),wf(:,132)) * den(63)
  A(130) = cont_QA(wf(:,11),wf(:,157)) * den(62)
  A(131) = cont_QA(wf(:,58),wf(:,145)) * den(65)
  A(132) = cont_QA(wf(:,31),wf(:,157)) * den(64)
  A(133) = cont_QA(wf(:,8),wf(:,160)) * den(89)
  A(134) = cont_QA(wf(:,11),wf(:,161)) * den(91)
  A(135) = cont_QA(wf(:,77),wf(:,163)) * den(93)
  A(136) = cont_QA(wf(:,8),wf(:,164)) * den(95)
  A(137) = cont_QA(wf(:,79),wf(:,165)) * den(96)
  A(138) = cont_QA(wf(:,77),wf(:,166)) * den(97)
  A(139) = cont_QA(wf(:,129),wf(:,167)) * den(98)
  A(140) = cont_QA(wf(:,131),wf(:,168)) * den(99)
  A(141) = cont_QA(wf(:,129),wf(:,169)) * den(100)
  A(142) = cont_QA(wf(:,13),wf(:,170)) * den(102)
  A(143) = cont_VV(wf(:,3),wf(:,173)) * den(105)
  A(144) = cont_QA(wf(:,19),wf(:,174)) * den(107)
  A(145) = cont_QA(wf(:,175),wf(:,176)) * den(109)
  A(146) = cont_QA(wf(:,176),wf(:,177)) * den(110)
  A(147) = cont_QA(wf(:,169),wf(:,178)) * den(112)
  A(148) = cont_QA(wf(:,166),wf(:,180)) * den(114)
  A(149) = cont_QA(wf(:,168),wf(:,182)) * den(116)
  A(150) = cont_QA(wf(:,169),wf(:,183)) * den(118)
  A(151) = cont_QA(wf(:,20),wf(:,184)) * den(120)
  A(152) = cont_QA(wf(:,163),wf(:,180)) * den(121)
  A(153) = cont_QA(wf(:,166),wf(:,185)) * den(123)
  A(154) = cont_QA(wf(:,165),wf(:,186)) * den(125)
  A(155) = cont_VV(wf(:,26),wf(:,187)) * den(127)
  A(156) = cont_QA(wf(:,31),wf(:,161)) * den(128)
  A(157) = cont_QA(wf(:,77),wf(:,189)) * den(130)
  A(158) = cont_QA(wf(:,104),wf(:,190)) * den(131)
  A(159) = cont_QA(wf(:,77),wf(:,191)) * den(132)
  A(160) = cont_QA(wf(:,29),wf(:,192)) * den(134)
  A(161) = cont_QA(wf(:,143),wf(:,193)) * den(135)
  A(162) = cont_QA(wf(:,144),wf(:,168)) * den(136)
  A(163) = cont_QA(wf(:,143),wf(:,194)) * den(137)
  A(164) = cont_VV(wf(:,26),wf(:,197)) * den(140)
  A(165) = cont_QA(wf(:,37),wf(:,198)) * den(142)
  A(166) = cont_QA(wf(:,33),wf(:,199)) * den(144)
  A(167) = cont_QA(wf(:,194),wf(:,201)) * den(146)
  A(168) = cont_QA(wf(:,180),wf(:,191)) * den(147)
  A(169) = cont_QA(wf(:,202),wf(:,203)) * den(149)
  A(170) = cont_QA(wf(:,200),wf(:,203)) * den(150)
  A(171) = cont_QA(wf(:,168),wf(:,205)) * den(152)
  A(172) = cont_QA(wf(:,194),wf(:,206)) * den(154)
  A(173) = cont_QA(wf(:,20),wf(:,207)) * den(156)
  A(174) = cont_QA(wf(:,180),wf(:,189)) * den(157)
  A(175) = cont_QA(wf(:,185),wf(:,191)) * den(158)
  A(176) = cont_QA(wf(:,190),wf(:,208)) * den(160)
  A(177) = cont_QA(wf(:,168),wf(:,210)) * den(162)
  A(178) = cont_QA(wf(:,165),wf(:,212)) * den(164)
  A(179) = cont_VV(wf(:,43),wf(:,213)) * den(167)
  A(180) = cont_VV(wf(:,42),wf(:,214)) * den(169)
  A(181) = cont_VV(wf(:,43),wf(:,214)) * den(170)
  A(182) = cont_VV(wf(:,42),wf(:,213)) * den(171)
  A(183) = cont_QA(wf(:,168),wf(:,216)) * den(173)
  A(184) = cont_QA(wf(:,190),wf(:,218)) * den(175)
  A(185) = cont_VV(wf(:,46),wf(:,219)) * den(178)
  A(186) = cont_VV(wf(:,45),wf(:,219)) * den(180)
  A(187) = cont_VV(wf(:,45),wf(:,220)) * den(181)
  A(188) = cont_VV(wf(:,46),wf(:,220)) * den(182)
  A(189) = cont_QA(wf(:,168),wf(:,222)) * den(184)
  A(190) = cont_QA(wf(:,190),wf(:,224)) * den(186)
  A(191) = cont_VV(wf(:,43),wf(:,225)) * den(187)
  A(192) = cont_QA(wf(:,168),wf(:,227)) * den(189)
  A(193) = cont_VV(wf(:,46),wf(:,228)) * den(190)
  A(194) = cont_QA(wf(:,165),wf(:,229)) * den(192)
  A(195) = cont_VV(wf(:,45),wf(:,228)) * den(193)
  A(196) = cont_VV(wf(:,42),wf(:,225)) * den(194)
  A(197) = cont_VV(wf(:,42),wf(:,232)) * den(197)
  A(198) = cont_VV(wf(:,43),wf(:,232)) * den(198)
  A(199) = cont_VV(wf(:,45),wf(:,233)) * den(199)
  A(200) = cont_VV(wf(:,46),wf(:,233)) * den(200)
  A(201) = cont_QA(wf(:,57),wf(:,176)) * den(201)
  A(202) = cont_QA(wf(:,166),wf(:,234)) * den(203)
  A(203) = cont_QA(wf(:,169),wf(:,235)) * den(205)
  A(204) = cont_QA(wf(:,163),wf(:,234)) * den(206)
  A(205) = cont_QA(wf(:,191),wf(:,234)) * den(207)
  A(206) = cont_QA(wf(:,194),wf(:,236)) * den(209)
  A(207) = cont_QA(wf(:,58),wf(:,203)) * den(210)
  A(208) = cont_QA(wf(:,189),wf(:,234)) * den(211)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(208)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(11)+A(12)+A(13)+A(19)+A(20)+A(21)+A(23))*f(1))/2._/**/REALKIND+((A(3)+A(4)+A(5)+A(17)+A(18)+A(22) &
       +A(24))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(6))*f(2))/6._/**/REALKIND+((-A(9)-A(10)-A(14))*f(2))/2._/**/REALKIND+((-A(7) &
       -A(8)+A(25)+A(26))*f(3))/6._/**/REALKIND+((-A(15)-A(16)+A(27)+A(28))*f(3))/2._/**/REALKIND
  M1(2) = ((-A(11)-A(12)-A(13)-A(19)-A(20)-A(21)-A(23))*f(1))/6._/**/REALKIND+((-A(3)-A(4)-A(5)-A(17)-A(18)-A(22) &
       -A(24))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(6))*f(2))/2._/**/REALKIND+((A(9)+A(10)+A(14))*f(2))/6._/**/REALKIND+((A(7)+A(8) &
       -A(25)-A(26))*f(3))/2._/**/REALKIND+((A(15)+A(16)-A(27)-A(28))*f(3))/6._/**/REALKIND

  M2(1) = ((-A(139)-A(140)-A(141)-A(142)-A(143)-A(144)-A(149)-A(151)-A(153)-A(177)-A(178)-A(179)-A(180)-A(181)-A(182)-A(191) &
       -A(192)-A(194)-A(196)-A(197)-A(198))*f(4))/6._/**/REALKIND+((-A(161)-A(162)-A(163)-A(164)-A(165)-A(166)-A(171)-A(173) &
       -A(175)-A(183)-A(184)-A(185)-A(186)-A(187)-A(188)-A(189)-A(190)-A(193)-A(195)-A(199)-A(200))*f(4))/2._/**/REALKIND+((A(133) &
       +A(134)+A(135)+A(136)+A(137)+A(138)+A(145)+A(150)+A(154))*f(5))/6._/**/REALKIND+((A(155)+A(156)+A(157)+A(158)+A(159)+A(160) &
       +A(169)+A(172)+A(176))*f(5))/2._/**/REALKIND+((A(146)+A(147)+A(148)+A(152)-A(201)-A(202)-A(203) &
       -A(204))*f(6))/6._/**/REALKIND+((A(167)+A(168)+A(170)+A(174)-A(205)-A(206)-A(207)-A(208))*f(6))/2._/**/REALKIND+((A(37) &
       +A(43)+A(44)+A(59)+A(61)+A(67)+A(68)+A(69)+A(75)+A(81)+A(84)+A(105)+A(115)+A(117))*f(7))/2._/**/REALKIND+((A(31)+A(41) &
       +A(42)+A(51)+A(57)+A(60)+A(83)+A(85)+A(91)+A(92)+A(93)+A(97)+A(113)+A(118))*f(7))/6._/**/REALKIND+((-A(29)-A(49)-A(89) &
       -A(90)-A(94)-A(99))*f(8))/6._/**/REALKIND+((-A(35)-A(65)-A(66)-A(70)-A(73)-A(107))*f(8))/2._/**/REALKIND+((-A(53)-A(95) &
       -A(96)-A(100)+A(121)+A(127)+A(128)+A(129))*f(9))/6._/**/REALKIND+((-A(71)-A(72)-A(77)-A(108)+A(123)+A(124)+A(125) &
       +A(131))*f(9))/2._/**/REALKIND+((A(38)+A(39)+A(45)+A(47)+A(63)+A(64)+A(79)+A(80)+A(82)+A(86)+A(106)+A(110)+A(116) &
       +A(119))*f(10))/2._/**/REALKIND+((A(32)+A(33)+A(46)+A(48)+A(55)+A(56)+A(58)+A(62)+A(87)+A(88)+A(98)+A(102)+A(114) &
       +A(120))*f(10))/6._/**/REALKIND+((-A(30)-A(34)-A(50)-A(54)-A(103)-A(104))*f(11))/6._/**/REALKIND+((-A(36)-A(40)-A(74)-A(78) &
       -A(111)-A(112))*f(11))/2._/**/REALKIND+((-A(52)-A(101)+A(122)+A(130))*f(12))/6._/**/REALKIND+((-A(76)-A(109)+A(126) &
       +A(132))*f(12))/2._/**/REALKIND
  M2(2) = ((A(139)+A(140)+A(141)+A(142)+A(143)+A(144)+A(149)+A(151)+A(153)+A(177)+A(178)+A(179)+A(180)+A(181)+A(182)+A(191)+A(192) &
       +A(194)+A(196)+A(197)+A(198))*f(4))/2._/**/REALKIND+((A(161)+A(162)+A(163)+A(164)+A(165)+A(166)+A(171)+A(173)+A(175)+A(183) &
       +A(184)+A(185)+A(186)+A(187)+A(188)+A(189)+A(190)+A(193)+A(195)+A(199)+A(200))*f(4))/6._/**/REALKIND+((-A(133)-A(134) &
       -A(135)-A(136)-A(137)-A(138)-A(145)-A(150)-A(154))*f(5))/2._/**/REALKIND+((-A(155)-A(156)-A(157)-A(158)-A(159)-A(160) &
       -A(169)-A(172)-A(176))*f(5))/6._/**/REALKIND+((-A(146)-A(147)-A(148)-A(152)+A(201)+A(202)+A(203) &
       +A(204))*f(6))/2._/**/REALKIND+((-A(167)-A(168)-A(170)-A(174)+A(205)+A(206)+A(207)+A(208))*f(6))/6._/**/REALKIND+((-A(37) &
       -A(43)-A(44)-A(59)-A(61)-A(67)-A(68)-A(69)-A(75)-A(81)-A(84)-A(105)-A(115)-A(117))*f(7))/6._/**/REALKIND+((-A(31)-A(41) &
       -A(42)-A(51)-A(57)-A(60)-A(83)-A(85)-A(91)-A(92)-A(93)-A(97)-A(113)-A(118))*f(7))/2._/**/REALKIND+((A(29)+A(49)+A(89)+A(90) &
       +A(94)+A(99))*f(8))/2._/**/REALKIND+((A(35)+A(65)+A(66)+A(70)+A(73)+A(107))*f(8))/6._/**/REALKIND+((A(53)+A(95)+A(96) &
       +A(100)-A(121)-A(127)-A(128)-A(129))*f(9))/2._/**/REALKIND+((A(71)+A(72)+A(77)+A(108)-A(123)-A(124)-A(125) &
       -A(131))*f(9))/6._/**/REALKIND+((-A(38)-A(39)-A(45)-A(47)-A(63)-A(64)-A(79)-A(80)-A(82)-A(86)-A(106)-A(110)-A(116) &
       -A(119))*f(10))/6._/**/REALKIND+((-A(32)-A(33)-A(46)-A(48)-A(55)-A(56)-A(58)-A(62)-A(87)-A(88)-A(98)-A(102)-A(114) &
       -A(120))*f(10))/2._/**/REALKIND+((A(30)+A(34)+A(50)+A(54)+A(103)+A(104))*f(11))/2._/**/REALKIND+((A(36)+A(40)+A(74)+A(78) &
       +A(111)+A(112))*f(11))/6._/**/REALKIND+((A(52)+A(101)-A(122)-A(130))*f(12))/2._/**/REALKIND+((A(76)+A(109)-A(126) &
       -A(132))*f(12))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnajj_neexuxdddxa_1_/**/REALKIND
