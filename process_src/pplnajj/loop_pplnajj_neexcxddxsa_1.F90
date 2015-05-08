
module ol_colourmatrix_pplnajj_neexcxddxsa_1_/**/REALKIND
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
end module ol_colourmatrix_pplnajj_neexcxddxsa_1_/**/REALKIND



module ol_forced_parameters_pplnajj_neexcxddxsa_1_/**/REALKIND
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
end module ol_forced_parameters_pplnajj_neexcxddxsa_1_/**/REALKIND

module ol_loop_pplnajj_neexcxddxsa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(29), c(47)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:172)
  ! denominators
  complex(REALKIND), save :: den(201)
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
    f( 7) = (CI*countertermnorm*ctGcc*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctGcc*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*ctGcc*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVcc*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f(15) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(17) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(6._/**/REALKIND*sw**2)
    f(18) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(3._/**/REALKIND*sw**2)
    f(19) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(20) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*12._/**/REALKIND)
    f(21) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(22) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(23) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(24) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(25) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(26) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(27) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(28) = (2*eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(29) = (eQED**3*gQCD**4*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(17), 27*CI*f(17), 9*CI*f(18), 27*CI*f(18), 9*CI*f(19), 27*CI*f(19), 18*f(20), 54*f(20), f(21), 3*f(21), 6*f(21) &
    , 8*f(21), 10*f(21), 18*f(21), 21*f(21), 24*f(21), 54*f(21), 18*f(22), 54*f(22), f(23), 3*f(23), 6*f(23), 8*f(23), 10*f(23) &
    , 18*f(23), 21*f(23), 24*f(23), 54*f(23), f(24), 3*f(24), 6*f(24), 8*f(24), 10*f(24), 18*f(24), 21*f(24), 24*f(24), 54*f(24) &
    , 3*f(25), 9*f(25), 3*f(26), 9*f(26), 3*f(27), 9*f(27), 3*f(28), 9*f(28), 3*f(29), 9*f(29) ]
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
  complex(REALKIND) :: A(104)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_AV_Q(wf(:,-2),wf(:,-6),wf(:,2))
  call vert_QA_V(wf(:,-3),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,68),ZERO,0_intkind1,wf(:,5))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,6))
  call vert_AV_Q(wf(:,5),wf(:,3),wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,3),wf(:,-5),wf(:,9))
  call vert_AW_Q(wf(:,5),wf(:,4),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,56),ZERO,0_intkind1,wf(:,11))
  call vert_VQ_A(wf(:,-6),wf(:,-5),wf(:,12))
  call prop_Q_A(wf(:,12),Q(:,96),ZERO,0_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,-2),wf(:,4),wf(:,14))
  call vert_VQ_A(wf(:,3),wf(:,13),wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,16))
  call vert_AV_Q(wf(:,-2),wf(:,3),wf(:,17))
  call vert_WQ_A(wf(:,4),wf(:,13),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,28),ZERO,0_intkind1,wf(:,19))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,20))
  call vert_AV_Q(wf(:,19),wf(:,-6),wf(:,21))
  call vert_UV_W(wf(:,-6),Q(:,64),wf(:,4),Q(:,3),wf(:,22))
  call prop_W_W(wf(:,22),Q(:,67),MW,1_intkind1,wf(:,23))
  call vert_QA_W(wf(:,-5),wf(:,19),wf(:,24))
  call vert_QA_W(wf(:,11),wf(:,-2),wf(:,25))
  call vert_VQ_A(wf(:,-6),wf(:,-3),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,72),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,27),wf(:,-4),wf(:,28))
  call vert_QA_V(wf(:,-5),wf(:,16),wf(:,29))
  call vert_AV_Q(wf(:,-2),wf(:,28),wf(:,30))
  call vert_AV_Q(wf(:,-4),wf(:,-6),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,80),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,-3),wf(:,32),wf(:,33))
  call vert_AV_Q(wf(:,-2),wf(:,33),wf(:,34))
  call vert_AV_Q(wf(:,-1),wf(:,-6),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,66),ZERO,0_intkind1,wf(:,36))
  call vert_QA_W(wf(:,0),wf(:,36),wf(:,37))
  call prop_W_W(wf(:,37),Q(:,67),MW,1_intkind1,wf(:,38))
  call vert_AW_Q(wf(:,-2),wf(:,38),wf(:,39))
  call vert_WQ_A(wf(:,38),wf(:,-5),wf(:,40))
  call counter_AV_Q(wf(:,5),wf(:,3),wf(:,41))
  call counter_AW_Q(wf(:,5),wf(:,4),wf(:,42))
  call counter_VQ_A(wf(:,3),wf(:,13),wf(:,43))
  call counter_WQ_A(wf(:,4),wf(:,13),wf(:,44))
  call counter_AV_Q(wf(:,16),wf(:,-6),wf(:,45))
  call counter_AV_Q(wf(:,19),wf(:,-6),wf(:,46))
  call counter_VQ_A(wf(:,3),wf(:,-5),wf(:,47))
  call prop_A_Q(wf(:,10),Q(:,71),ZERO,0_intkind1,wf(:,48))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,49))
  call prop_A_Q(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,50))
  call prop_Q_A(wf(:,47),Q(:,56),ZERO,0_intkind1,wf(:,51))
  call counter_QA_W(wf(:,-5),wf(:,19),wf(:,52))
  call vert_QA_W(wf(:,51),wf(:,-2),wf(:,53))
  call prop_Q_A(wf(:,49),Q(:,35),ZERO,0_intkind1,wf(:,54))
  call counter_VQ_A(wf(:,-6),wf(:,-5),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,96),ZERO,0_intkind1,wf(:,56))
  call vert_VQ_A(wf(:,3),wf(:,56),wf(:,57))
  call vert_WQ_A(wf(:,4),wf(:,56),wf(:,58))
  call counter_QA_V(wf(:,-5),wf(:,16),wf(:,59))
  call counter_QA_V(wf(:,27),wf(:,-4),wf(:,60))
  call vert_AV_Q(wf(:,-2),wf(:,60),wf(:,61))
  call counter_AV_Q(wf(:,-4),wf(:,-6),wf(:,62))
  call prop_A_Q(wf(:,62),Q(:,80),ZERO,0_intkind1,wf(:,63))
  call vert_QA_V(wf(:,-3),wf(:,63),wf(:,64))
  call vert_AV_Q(wf(:,-2),wf(:,64),wf(:,65))
  call counter_QA_V(wf(:,-3),wf(:,32),wf(:,66))
  call vert_AV_Q(wf(:,-2),wf(:,66),wf(:,67))
  call counter_VQ_A(wf(:,-6),wf(:,-3),wf(:,68))
  call prop_Q_A(wf(:,68),Q(:,72),ZERO,0_intkind1,wf(:,69))
  call vert_QA_V(wf(:,69),wf(:,-4),wf(:,70))
  call vert_AV_Q(wf(:,-2),wf(:,70),wf(:,71))
  call counter_QA_V(wf(:,-3),wf(:,-4),wf(:,72))
  call vert_AV_Q(wf(:,5),wf(:,72),wf(:,73))
  call vert_VQ_A(wf(:,72),wf(:,-5),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,56),ZERO,0_intkind1,wf(:,75))
  call vert_VQ_A(wf(:,72),wf(:,13),wf(:,76))
  call vert_AV_Q(wf(:,-2),wf(:,72),wf(:,77))
  call prop_A_Q(wf(:,77),Q(:,28),ZERO,0_intkind1,wf(:,78))
  call vert_AV_Q(wf(:,78),wf(:,-6),wf(:,79))
  call vert_QA_W(wf(:,75),wf(:,-2),wf(:,80))
  call vert_QA_W(wf(:,-5),wf(:,78),wf(:,81))
  call counter_AV_Q(wf(:,-2),wf(:,3),wf(:,82))
  call prop_Q_A(wf(:,18),Q(:,99),ZERO,0_intkind1,wf(:,83))
  call counter_AW_Q(wf(:,-2),wf(:,4),wf(:,84))
  call prop_Q_A(wf(:,15),Q(:,120),ZERO,0_intkind1,wf(:,85))
  call prop_A_Q(wf(:,82),Q(:,28),ZERO,0_intkind1,wf(:,86))
  call vert_AV_Q(wf(:,86),wf(:,-6),wf(:,87))
  call vert_QA_W(wf(:,-5),wf(:,86),wf(:,88))
  call counter_QA_W(wf(:,11),wf(:,-2),wf(:,89))
  call prop_A_Q(wf(:,84),Q(:,7),ZERO,0_intkind1,wf(:,90))
  call vert_AV_Q(wf(:,90),wf(:,-6),wf(:,91))
  call counter_AV_Q(wf(:,-2),wf(:,-6),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,68),ZERO,0_intkind1,wf(:,93))
  call vert_AV_Q(wf(:,93),wf(:,3),wf(:,94))
  call vert_AW_Q(wf(:,93),wf(:,4),wf(:,95))
  call counter_AV_Q(wf(:,-2),wf(:,28),wf(:,96))
  call vert_QA_V(wf(:,-5),wf(:,90),wf(:,97))
  call counter_AV_Q(wf(:,-2),wf(:,33),wf(:,98))
  call counter_WQ_A(wf(:,38),wf(:,-5),wf(:,99))
  call counter_AW_Q(wf(:,-2),wf(:,38),wf(:,100))
  call counter_A_Q(ctcc,wf(:,5),Q(:,68),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,68),ZERO,0_intkind1,wf(:,102))
  call vert_AV_Q(wf(:,102),wf(:,3),wf(:,103))
  call vert_AW_Q(wf(:,102),wf(:,4),wf(:,104))
  call counter_V_V(ctGG,wf(:,3),Q(:,24),wf(:,105))
  call vert_VQ_A(wf(:,105),wf(:,-5),wf(:,106))
  call vert_AV_Q(wf(:,5),wf(:,105),wf(:,107))
  call counter_Q_A(ctcc,wf(:,8),Q(:,35),wf(:,108))
  call counter_Q_A(ctqq,wf(:,11),Q(:,56),wf(:,109))
  call vert_AV_Q(wf(:,-2),wf(:,105),wf(:,110))
  call counter_A_Q(ctqq,wf(:,16),Q(:,7),wf(:,111))
  call counter_A_Q(ctcc,wf(:,19),Q(:,28),wf(:,112))
  call vert_AV_Q(wf(:,16),wf(:,105),wf(:,113))
  call counter_Q_A(ctqq,wf(:,13),Q(:,96),wf(:,114))
  call prop_Q_A(wf(:,114),Q(:,96),ZERO,0_intkind1,wf(:,115))
  call vert_QA_V(wf(:,115),wf(:,16),wf(:,116))
  call vert_WQ_A(wf(:,4),wf(:,115),wf(:,117))
  call vert_VQ_A(wf(:,-6),wf(:,8),wf(:,118))
  call prop_A_Q(wf(:,110),Q(:,28),ZERO,0_intkind1,wf(:,119))
  call vert_WQ_A(wf(:,23),wf(:,-5),wf(:,120))
  call prop_Q_A(wf(:,120),Q(:,99),ZERO,0_intkind1,wf(:,121))
  call vert_AW_Q(wf(:,-2),wf(:,23),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,71),ZERO,0_intkind1,wf(:,123))
  call vert_VQ_A(wf(:,-6),wf(:,11),wf(:,124))
  call prop_Q_A(wf(:,124),Q(:,120),ZERO,0_intkind1,wf(:,125))
  call prop_Q_A(wf(:,118),Q(:,99),ZERO,0_intkind1,wf(:,126))
  call prop_Q_A(wf(:,106),Q(:,56),ZERO,0_intkind1,wf(:,127))
  call prop_A_Q(wf(:,20),Q(:,71),ZERO,0_intkind1,wf(:,128))
  call prop_A_Q(wf(:,21),Q(:,92),ZERO,0_intkind1,wf(:,129))
  call vert_QA_V(wf(:,8),wf(:,-2),wf(:,130))
  call counter_V_V(ctGG,wf(:,28),Q(:,88),wf(:,131))
  call vert_VQ_A(wf(:,28),wf(:,-5),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,120),ZERO,0_intkind1,wf(:,133))
  call prop_A_Q(wf(:,30),Q(:,92),ZERO,0_intkind1,wf(:,134))
  call counter_Q_A(ctqq,wf(:,27),Q(:,72),wf(:,135))
  call prop_Q_A(wf(:,135),Q(:,72),ZERO,0_intkind1,wf(:,136))
  call vert_QA_V(wf(:,136),wf(:,-4),wf(:,137))
  call counter_V_V(ctGG,wf(:,33),Q(:,88),wf(:,138))
  call vert_VQ_A(wf(:,33),wf(:,-5),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,120),ZERO,0_intkind1,wf(:,140))
  call prop_A_Q(wf(:,34),Q(:,92),ZERO,0_intkind1,wf(:,141))
  call counter_A_Q(ctqq,wf(:,32),Q(:,80),wf(:,142))
  call prop_A_Q(wf(:,142),Q(:,80),ZERO,0_intkind1,wf(:,143))
  call vert_QA_V(wf(:,-3),wf(:,143),wf(:,144))
  call prop_A_Q(wf(:,39),Q(:,71),ZERO,0_intkind1,wf(:,145))
  call prop_Q_A(wf(:,40),Q(:,99),ZERO,0_intkind1,wf(:,146))
  call vert_AV_Q(wf(:,16),wf(:,3),wf(:,147))
  call prop_A_Q(wf(:,147),Q(:,31),ZERO,0_intkind1,wf(:,148))
  call vert_VQ_A(wf(:,3),wf(:,8),wf(:,149))
  call prop_Q_A(wf(:,149),Q(:,59),ZERO,0_intkind1,wf(:,150))
  call vert_AW_Q(wf(:,19),wf(:,4),wf(:,151))
  call prop_A_Q(wf(:,151),Q(:,31),ZERO,0_intkind1,wf(:,152))
  call vert_WQ_A(wf(:,4),wf(:,11),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,59),ZERO,0_intkind1,wf(:,154))
  call vert_QA_V(wf(:,8),wf(:,5),wf(:,155))
  call vert_QA_V(wf(:,-5),wf(:,48),wf(:,156))
  call vert_QA_V(wf(:,13),wf(:,16),wf(:,157))
  call vert_QA_V(wf(:,83),wf(:,-2),wf(:,158))
  call vert_VQ_A(wf(:,29),wf(:,-3),wf(:,159))
  call prop_Q_A(wf(:,159),Q(:,47),ZERO,0_intkind1,wf(:,160))
  call vert_AV_Q(wf(:,-4),wf(:,29),wf(:,161))
  call prop_A_Q(wf(:,161),Q(:,55),ZERO,0_intkind1,wf(:,162))
  call vert_QA_V(wf(:,-5),wf(:,128),wf(:,163))
  call vert_VQ_A(wf(:,130),wf(:,-3),wf(:,164))
  call prop_Q_A(wf(:,164),Q(:,47),ZERO,0_intkind1,wf(:,165))
  call vert_AV_Q(wf(:,-4),wf(:,130),wf(:,166))
  call prop_A_Q(wf(:,166),Q(:,55),ZERO,0_intkind1,wf(:,167))
  call vert_QA_V(wf(:,126),wf(:,-2),wf(:,168))
  call vert_QA_V(wf(:,-5),wf(:,123),wf(:,169))
  call vert_QA_V(wf(:,121),wf(:,-2),wf(:,170))
  call vert_QA_V(wf(:,-5),wf(:,145),wf(:,171))
  call vert_QA_V(wf(:,146),wf(:,-2),wf(:,172))

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
  den(3) = 1 / (Q(5,24))
  den(5) = 1 / (Q(5,35))
  den(9) = 1 / (Q(5,56))
  den(12) = 1 / (Q(5,96))
  den(14) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,28))
  den(23) = 1 / (Q(5,67) - MW2)
  den(27) = 1 / (Q(5,72))
  den(28) = 1 / (Q(5,88))
  den(32) = 1 / (Q(5,80))
  den(36) = 1 / (Q(5,66))
  den(40) = 1 / (Q(5,71))
  den(43) = 1 / (Q(5,92))
  den(46) = 1 / (Q(5,99))
  den(49) = 1 / (Q(5,120))
  den(91) = 1 / (Q(5,39))
  den(121) = 1 / (Q(5,31))
  den(124) = 1 / (Q(5,59))
  den(131) = 1 / (Q(5,103))
  den(137) = 1 / (Q(5,47))
  den(139) = 1 / (Q(5,55))

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
  den(29) = den(27)*den(28)
  den(30) = den(15)*den(29)
  den(31) = den(6)*den(29)
  den(33) = den(28)*den(32)
  den(34) = den(15)*den(33)
  den(35) = den(6)*den(33)
  den(37) = den(23)*den(36)
  den(38) = den(10)*den(37)
  den(39) = den(19)*den(37)
  den(41) = den(8)*den(40)
  den(42) = den(3)*den(41)
  den(44) = den(4)*den(43)
  den(45) = den(1)*den(44)
  den(47) = den(17)*den(46)
  den(48) = den(3)*den(47)
  den(50) = den(13)*den(49)
  den(51) = den(1)*den(50)
  den(52) = den(2)**2
  den(53) = den(3)*den(52)
  den(54) = den(6)*den(53)
  den(55) = den(1)*den(52)
  den(56) = den(10)*den(55)
  den(57) = den(3)**2
  den(58) = den(41)*den(57)
  den(59) = den(2)*den(57)
  den(60) = den(6)*den(59)
  den(61) = den(6)*den(44)
  den(62) = den(10)*den(41)
  den(63) = den(47)*den(57)
  den(64) = den(15)*den(50)
  den(65) = den(19)*den(47)
  den(66) = den(15)*den(57)
  den(67) = den(12)*den(66)
  den(68) = den(12)**2
  den(69) = den(15)*den(68)
  den(70) = den(3)*den(69)
  den(71) = den(1)*den(68)
  den(72) = den(19)*den(71)
  den(73) = den(18)*den(57)
  den(74) = den(6)*den(73)
  den(75) = den(24)*den(73)
  den(76) = den(24)*den(46)
  den(77) = den(19)*den(76)
  den(78) = den(24)*den(40)
  den(79) = den(10)*den(78)
  den(80) = den(10)*den(49)
  den(81) = den(15)*den(80)
  den(82) = den(6)*den(46)
  den(83) = den(19)*den(82)
  den(84) = den(9)*den(57)
  den(85) = den(15)*den(84)
  den(86) = den(57)*den(78)
  den(87) = den(15)*den(40)
  den(88) = den(10)*den(87)
  den(89) = den(19)*den(43)
  den(90) = den(6)*den(89)
  den(92) = den(6)*den(91)
  den(93) = den(29)*den(92)
  den(94) = den(29)*den(49)
  den(95) = den(15)*den(94)
  den(96) = den(29)*den(43)
  den(97) = den(6)*den(96)
  den(98) = den(27)**2
  den(99) = den(28)*den(98)
  den(100) = den(15)*den(99)
  den(101) = den(92)*den(98)
  den(102) = den(15)*den(91)
  den(103) = den(29)*den(102)
  den(104) = den(33)*den(92)
  den(105) = den(33)*den(49)
  den(106) = den(15)*den(105)
  den(107) = den(33)*den(43)
  den(108) = den(6)*den(107)
  den(109) = den(32)**2
  den(110) = den(28)*den(109)
  den(111) = den(15)*den(110)
  den(112) = den(92)*den(109)
  den(113) = den(33)*den(102)
  den(114) = den(37)*den(73)
  den(115) = den(37)*den(40)
  den(116) = den(10)*den(115)
  den(117) = den(37)*den(46)
  den(118) = den(19)*den(117)
  den(119) = den(57)*den(115)
  den(120) = den(3)*den(15)
  den(122) = den(120)*den(121)
  den(123) = den(3)*den(6)
  den(125) = den(123)*den(124)
  den(126) = den(1)*den(19)
  den(127) = den(121)*den(126)
  den(128) = den(1)*den(10)
  den(129) = den(124)*den(128)
  den(130) = den(2)*den(6)
  den(132) = den(130)*den(131)
  den(133) = den(41)*den(131)
  den(134) = den(12)*den(15)
  den(135) = den(131)*den(134)
  den(136) = den(47)*den(131)
  den(138) = den(102)*den(137)
  den(140) = den(102)*den(139)
  den(141) = den(87)*den(131)
  den(142) = den(92)*den(137)
  den(143) = den(92)*den(139)
  den(144) = den(82)*den(131)
  den(145) = den(78)*den(131)
  den(146) = den(76)*den(131)
  den(147) = den(115)*den(131)
  den(148) = den(117)*den(131)
  den(149) = den(2)*den(3)*den(6)
  den(150) = den(1)*den(2)*den(10)
  den(151) = den(1)*den(2)*den(3)
  den(152) = den(3)*den(12)*den(15)
  den(153) = den(1)*den(12)*den(19)
  den(154) = den(1)*den(3)*den(12)
  den(155) = den(3)*den(102)
  den(156) = den(3)*den(87)
  den(157) = den(3)*den(92)
  den(158) = den(3)*den(82)
  den(159) = den(3)*den(78)
  den(160) = den(3)*den(76)
  den(161) = den(3)*den(24)
  den(162) = den(1)*den(89)
  den(163) = den(1)*den(80)
  den(164) = den(1)*den(3)
  den(165) = den(27)*den(102)
  den(166) = den(15)*den(27)
  den(167) = den(27)*den(92)
  den(168) = den(6)*den(27)
  den(169) = den(1)*den(96)
  den(170) = den(1)*den(94)
  den(171) = den(1)*den(29)
  den(172) = den(1)*den(27)
  den(173) = den(32)*den(102)
  den(174) = den(15)*den(32)
  den(175) = den(32)*den(92)
  den(176) = den(6)*den(32)
  den(177) = den(1)*den(107)
  den(178) = den(1)*den(105)
  den(179) = den(1)*den(33)
  den(180) = den(1)*den(32)
  den(181) = den(3)*den(115)
  den(182) = den(3)*den(117)
  den(183) = den(3)*den(37)
  den(184) = den(3)*den(132)
  den(185) = den(2)*den(125)
  den(186) = den(3)*den(133)
  den(187) = den(2)*den(129)
  den(188) = den(12)*den(122)
  den(189) = den(3)*den(135)
  den(190) = den(12)*den(127)
  den(191) = den(3)*den(136)
  den(192) = den(3)*den(141)
  den(193) = den(3)*den(144)
  den(194) = den(3)*den(145)
  den(195) = den(3)*den(146)
  den(196) = den(27)*den(140)
  den(197) = den(27)*den(143)
  den(198) = den(32)*den(138)
  den(199) = den(32)*den(142)
  den(200) = den(3)*den(147)
  den(201) = den(3)*den(148)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(104)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_QA(wf(:,11),wf(:,20)) * den(21)
  A(6) = cont_QA(wf(:,8),wf(:,21)) * den(22)
  A(7) = cont_VV(wf(:,23),wf(:,24)) * den(25)
  A(8) = cont_VV(wf(:,23),wf(:,25)) * den(26)
  A(9) = cont_VV(wf(:,28),wf(:,29)) * den(30)
  A(10) = cont_QA(wf(:,8),wf(:,30)) * den(31)
  A(11) = cont_VV(wf(:,29),wf(:,33)) * den(34)
  A(12) = cont_QA(wf(:,8),wf(:,34)) * den(35)
  A(13) = cont_QA(wf(:,11),wf(:,39)) * den(38)
  A(14) = cont_QA(wf(:,19),wf(:,40)) * den(39)

  A(15) = cont_QA(wf(:,8),wf(:,41)) * den(7)
  A(16) = cont_QA(wf(:,11),wf(:,42)) * den(11)
  A(17) = cont_QA(wf(:,16),wf(:,43)) * den(16)
  A(18) = cont_QA(wf(:,19),wf(:,44)) * den(20)
  A(19) = cont_QA(wf(:,11),wf(:,45)) * den(21)
  A(20) = cont_QA(wf(:,8),wf(:,46)) * den(22)
  A(21) = cont_QA(wf(:,47),wf(:,48)) * den(42)
  A(22) = cont_QA(wf(:,49),wf(:,50)) * den(45)
  A(23) = cont_QA(wf(:,20),wf(:,51)) * den(21)
  A(24) = cont_VV(wf(:,23),wf(:,52)) * den(25)
  A(25) = cont_VV(wf(:,23),wf(:,53)) * den(26)
  A(26) = cont_QA(wf(:,21),wf(:,54)) * den(22)
  A(27) = cont_QA(wf(:,16),wf(:,57)) * den(16)
  A(28) = cont_QA(wf(:,19),wf(:,58)) * den(20)
  A(29) = cont_VV(wf(:,28),wf(:,59)) * den(30)
  A(30) = cont_QA(wf(:,30),wf(:,54)) * den(31)
  A(31) = cont_VV(wf(:,33),wf(:,59)) * den(34)
  A(32) = cont_QA(wf(:,34),wf(:,54)) * den(35)
  A(33) = cont_VV(wf(:,29),wf(:,60)) * den(30)
  A(34) = cont_QA(wf(:,8),wf(:,61)) * den(31)
  A(35) = cont_VV(wf(:,29),wf(:,64)) * den(34)
  A(36) = cont_QA(wf(:,8),wf(:,65)) * den(35)
  A(37) = cont_VV(wf(:,29),wf(:,66)) * den(34)
  A(38) = cont_QA(wf(:,8),wf(:,67)) * den(35)
  A(39) = cont_VV(wf(:,29),wf(:,70)) * den(30)
  A(40) = cont_QA(wf(:,8),wf(:,71)) * den(31)
  A(41) = cont_QA(wf(:,8),wf(:,73)) * den(7)
  A(42) = cont_QA(wf(:,10),wf(:,75)) * den(11)
  A(43) = cont_QA(wf(:,16),wf(:,76)) * den(16)
  A(44) = cont_QA(wf(:,18),wf(:,78)) * den(20)
  A(45) = cont_QA(wf(:,20),wf(:,75)) * den(21)
  A(46) = cont_QA(wf(:,8),wf(:,79)) * den(22)
  A(47) = cont_VV(wf(:,23),wf(:,80)) * den(26)
  A(48) = cont_VV(wf(:,23),wf(:,81)) * den(25)
  A(49) = cont_QA(wf(:,82),wf(:,83)) * den(48)
  A(50) = cont_QA(wf(:,84),wf(:,85)) * den(51)
  A(51) = cont_QA(wf(:,8),wf(:,87)) * den(22)
  A(52) = cont_VV(wf(:,23),wf(:,88)) * den(25)
  A(53) = cont_VV(wf(:,23),wf(:,89)) * den(26)
  A(54) = cont_QA(wf(:,11),wf(:,91)) * den(21)
  A(55) = cont_QA(wf(:,8),wf(:,94)) * den(7)
  A(56) = cont_QA(wf(:,11),wf(:,95)) * den(11)
  A(57) = cont_QA(wf(:,8),wf(:,96)) * den(31)
  A(58) = cont_VV(wf(:,28),wf(:,97)) * den(30)
  A(59) = cont_QA(wf(:,8),wf(:,98)) * den(35)
  A(60) = cont_VV(wf(:,33),wf(:,97)) * den(34)
  A(61) = cont_QA(wf(:,39),wf(:,51)) * den(38)
  A(62) = cont_QA(wf(:,19),wf(:,99)) * den(39)
  A(63) = cont_QA(wf(:,39),wf(:,75)) * den(38)
  A(64) = cont_QA(wf(:,40),wf(:,78)) * den(39)
  A(65) = cont_QA(wf(:,40),wf(:,86)) * den(39)
  A(66) = cont_QA(wf(:,11),wf(:,100)) * den(38)
  A(67) = cont_QA(wf(:,8),wf(:,103)) * den(54)
  A(68) = cont_QA(wf(:,11),wf(:,104)) * den(56)
  A(69) = cont_QA(wf(:,48),wf(:,106)) * den(58)
  A(70) = cont_QA(wf(:,8),wf(:,107)) * den(60)
  A(71) = cont_QA(wf(:,50),wf(:,108)) * den(61)
  A(72) = cont_QA(wf(:,48),wf(:,109)) * den(62)
  A(73) = cont_QA(wf(:,83),wf(:,110)) * den(63)
  A(74) = cont_QA(wf(:,85),wf(:,111)) * den(64)
  A(75) = cont_QA(wf(:,83),wf(:,112)) * den(65)
  A(76) = cont_QA(wf(:,13),wf(:,113)) * den(67)
  A(77) = cont_VV(wf(:,3),wf(:,116)) * den(70)
  A(78) = cont_QA(wf(:,19),wf(:,117)) * den(72)
  A(79) = cont_QA(wf(:,118),wf(:,119)) * den(74)
  A(80) = cont_QA(wf(:,119),wf(:,120)) * den(75)
  A(81) = cont_QA(wf(:,112),wf(:,121)) * den(77)
  A(82) = cont_QA(wf(:,109),wf(:,123)) * den(79)
  A(83) = cont_QA(wf(:,111),wf(:,125)) * den(81)
  A(84) = cont_QA(wf(:,112),wf(:,126)) * den(83)
  A(85) = cont_QA(wf(:,20),wf(:,127)) * den(85)
  A(86) = cont_QA(wf(:,106),wf(:,123)) * den(86)
  A(87) = cont_QA(wf(:,109),wf(:,128)) * den(88)
  A(88) = cont_QA(wf(:,108),wf(:,129)) * den(90)
  A(89) = cont_VV(wf(:,130),wf(:,131)) * den(93)
  A(90) = cont_QA(wf(:,111),wf(:,133)) * den(95)
  A(91) = cont_QA(wf(:,108),wf(:,134)) * den(97)
  A(92) = cont_VV(wf(:,29),wf(:,137)) * den(100)
  A(93) = cont_VV(wf(:,130),wf(:,137)) * den(101)
  A(94) = cont_VV(wf(:,29),wf(:,131)) * den(103)
  A(95) = cont_VV(wf(:,130),wf(:,138)) * den(104)
  A(96) = cont_QA(wf(:,111),wf(:,140)) * den(106)
  A(97) = cont_QA(wf(:,108),wf(:,141)) * den(108)
  A(98) = cont_VV(wf(:,29),wf(:,144)) * den(111)
  A(99) = cont_VV(wf(:,130),wf(:,144)) * den(112)
  A(100) = cont_VV(wf(:,29),wf(:,138)) * den(113)
  A(101) = cont_QA(wf(:,40),wf(:,119)) * den(114)
  A(102) = cont_QA(wf(:,109),wf(:,145)) * den(116)
  A(103) = cont_QA(wf(:,112),wf(:,146)) * den(118)
  A(104) = cont_QA(wf(:,106),wf(:,145)) * den(119)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(104)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(3)-A(4)-A(5)-A(9)-A(10)-A(11)-A(12))*f(1))/6._/**/REALKIND+((A(1)+A(2)+A(6))*f(2))/6._/**/REALKIND+((A(7)+A(8) &
       -A(13)-A(14))*f(3))/6._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(5)+A(9)+A(10)+A(11)+A(12))*f(1))/2._/**/REALKIND+((-A(1)-A(2)-A(6))*f(2))/2._/**/REALKIND+((-A(7)-A(8) &
       +A(13)+A(14))*f(3))/2._/**/REALKIND

  M2(1) = ((A(73)+A(74)+A(75)+A(76)+A(77)+A(78)+A(83)+A(85)+A(87)+A(89)+A(90)+A(91)+A(92)+A(93)+A(94)+A(95)+A(96)+A(97)+A(98) &
       +A(99)+A(100))*f(4))/6._/**/REALKIND+((-A(67)-A(68)-A(69)-A(70)-A(71)-A(72)-A(79)-A(84)-A(88))*f(5))/6._/**/REALKIND+(( &
       -A(80)-A(81)-A(82)-A(86)+A(101)+A(102)+A(103)+A(104))*f(6))/6._/**/REALKIND+((-A(49)-A(57)-A(59))*f(7))/6._/**/REALKIND &
       +((A(15)+A(51))*f(8))/6._/**/REALKIND+((A(52)-A(65))*f(9))/6._/**/REALKIND+((-A(17)-A(23)-A(29)-A(31)-A(33)-A(34)-A(37) &
       -A(38)-A(43)-A(44)-A(45))*f(10))/6._/**/REALKIND+((A(21)+A(41)+A(42)+A(46))*f(11))/6._/**/REALKIND+((A(25)+A(47)+A(48) &
       -A(61)-A(63)-A(64))*f(12))/6._/**/REALKIND+((A(20)+A(55)+A(56))*f(13))/6._/**/REALKIND+((-A(18)-A(19)-A(27)-A(28)-A(30) &
       -A(32)-A(35)-A(36)-A(39)-A(40)-A(50)-A(54)-A(58)-A(60))*f(14))/6._/**/REALKIND+((A(16)+A(22)+A(26))*f(15))/6._/**/REALKIND &
       +((A(24)+A(53)-A(62)-A(66))*f(16))/6._/**/REALKIND
  M2(2) = ((-A(73)-A(74)-A(75)-A(76)-A(77)-A(78)-A(83)-A(85)-A(87)-A(89)-A(90)-A(91)-A(92)-A(93)-A(94)-A(95)-A(96)-A(97)-A(98) &
       -A(99)-A(100))*f(4))/2._/**/REALKIND+((A(67)+A(68)+A(69)+A(70)+A(71)+A(72)+A(79)+A(84)+A(88))*f(5))/2._/**/REALKIND+((A(80) &
       +A(81)+A(82)+A(86)-A(101)-A(102)-A(103)-A(104))*f(6))/2._/**/REALKIND+((A(49)+A(57)+A(59))*f(7))/2._/**/REALKIND+((-A(15) &
       -A(51))*f(8))/2._/**/REALKIND+((-A(52)+A(65))*f(9))/2._/**/REALKIND+((A(17)+A(23)+A(29)+A(31)+A(33)+A(34)+A(37)+A(38)+A(43) &
       +A(44)+A(45))*f(10))/2._/**/REALKIND+((-A(21)-A(41)-A(42)-A(46))*f(11))/2._/**/REALKIND+((-A(25)-A(47)-A(48)+A(61)+A(63) &
       +A(64))*f(12))/2._/**/REALKIND+((-A(20)-A(55)-A(56))*f(13))/2._/**/REALKIND+((A(18)+A(19)+A(27)+A(28)+A(30)+A(32)+A(35) &
       +A(36)+A(39)+A(40)+A(50)+A(54)+A(58)+A(60))*f(14))/2._/**/REALKIND+((-A(16)-A(22)-A(26))*f(15))/2._/**/REALKIND+((-A(24) &
       -A(53)+A(62)+A(66))*f(16))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnajj_neexcxddxsa_1_/**/REALKIND
