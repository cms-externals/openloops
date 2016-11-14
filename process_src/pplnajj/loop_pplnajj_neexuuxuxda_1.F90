
module ol_colourmatrix_pplnajj_neexuuxuxda_1_/**/REALKIND
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
end module ol_colourmatrix_pplnajj_neexuuxuxda_1_/**/REALKIND



module ol_forced_parameters_pplnajj_neexuuxuxda_1_/**/REALKIND
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
end module ol_forced_parameters_pplnajj_neexuuxuxda_1_/**/REALKIND

module ol_loop_pplnajj_neexuuxuxda_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(25), c(47)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:302)
  ! denominators
  complex(REALKIND), save :: den(368)
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
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_AV_Q(wf(:,-4),wf(:,-6),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,5))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,6))
  call vert_AV_Q(wf(:,5),wf(:,2),wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,2),wf(:,-5),wf(:,9))
  call vert_AW_Q(wf(:,5),wf(:,4),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,11))
  call vert_VQ_A(wf(:,-6),wf(:,-5),wf(:,12))
  call prop_Q_A(wf(:,12),Q(:,96),ZERO,0_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,-4),wf(:,4),wf(:,14))
  call vert_VQ_A(wf(:,2),wf(:,13),wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,19),ZERO,0_intkind1,wf(:,16))
  call vert_AV_Q(wf(:,-4),wf(:,2),wf(:,17))
  call vert_WQ_A(wf(:,4),wf(:,13),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,28),ZERO,0_intkind1,wf(:,19))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,20))
  call vert_AV_Q(wf(:,19),wf(:,-6),wf(:,21))
  call vert_UV_W(wf(:,-6),Q(:,64),wf(:,4),Q(:,3),wf(:,22))
  call prop_W_W(wf(:,22),Q(:,67),MW,1_intkind1,wf(:,23))
  call vert_QA_W(wf(:,-5),wf(:,19),wf(:,24))
  call vert_QA_W(wf(:,11),wf(:,-4),wf(:,25))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,26))
  call vert_AV_Q(wf(:,-3),wf(:,-6),wf(:,27))
  call prop_A_Q(wf(:,27),Q(:,72),ZERO,0_intkind1,wf(:,28))
  call vert_AV_Q(wf(:,28),wf(:,26),wf(:,29))
  call vert_VQ_A(wf(:,26),wf(:,-5),wf(:,30))
  call vert_AW_Q(wf(:,28),wf(:,4),wf(:,31))
  call prop_Q_A(wf(:,30),Q(:,52),ZERO,0_intkind1,wf(:,32))
  call vert_AW_Q(wf(:,-3),wf(:,4),wf(:,33))
  call vert_VQ_A(wf(:,26),wf(:,13),wf(:,34))
  call prop_A_Q(wf(:,33),Q(:,11),ZERO,0_intkind1,wf(:,35))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,36))
  call prop_A_Q(wf(:,36),Q(:,28),ZERO,0_intkind1,wf(:,37))
  call vert_AV_Q(wf(:,35),wf(:,-6),wf(:,38))
  call vert_AV_Q(wf(:,37),wf(:,-6),wf(:,39))
  call vert_QA_W(wf(:,-5),wf(:,37),wf(:,40))
  call vert_QA_W(wf(:,32),wf(:,-3),wf(:,41))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,68),ZERO,0_intkind1,wf(:,43))
  call vert_QA_V(wf(:,43),wf(:,-4),wf(:,44))
  call vert_QA_V(wf(:,-5),wf(:,35),wf(:,45))
  call vert_QA_V(wf(:,43),wf(:,-3),wf(:,46))
  call vert_VQ_A(wf(:,46),wf(:,-5),wf(:,47))
  call vert_AV_Q(wf(:,-4),wf(:,46),wf(:,48))
  call vert_AV_Q(wf(:,-3),wf(:,44),wf(:,49))
  call vert_QA_V(wf(:,-2),wf(:,28),wf(:,50))
  call vert_VQ_A(wf(:,50),wf(:,-5),wf(:,51))
  call vert_AV_Q(wf(:,-4),wf(:,50),wf(:,52))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,53))
  call vert_VQ_A(wf(:,53),wf(:,-5),wf(:,54))
  call vert_AV_Q(wf(:,-3),wf(:,53),wf(:,55))
  call vert_AV_Q(wf(:,-1),wf(:,-6),wf(:,56))
  call prop_A_Q(wf(:,56),Q(:,66),ZERO,0_intkind1,wf(:,57))
  call vert_QA_W(wf(:,0),wf(:,57),wf(:,58))
  call prop_W_W(wf(:,58),Q(:,67),MW,1_intkind1,wf(:,59))
  call vert_AW_Q(wf(:,-4),wf(:,59),wf(:,60))
  call vert_WQ_A(wf(:,59),wf(:,-5),wf(:,61))
  call vert_AW_Q(wf(:,-3),wf(:,59),wf(:,62))
  call counter_AV_Q(wf(:,5),wf(:,2),wf(:,63))
  call counter_AW_Q(wf(:,5),wf(:,4),wf(:,64))
  call counter_VQ_A(wf(:,2),wf(:,13),wf(:,65))
  call counter_WQ_A(wf(:,4),wf(:,13),wf(:,66))
  call counter_AV_Q(wf(:,16),wf(:,-6),wf(:,67))
  call counter_AV_Q(wf(:,19),wf(:,-6),wf(:,68))
  call counter_VQ_A(wf(:,2),wf(:,-5),wf(:,69))
  call prop_A_Q(wf(:,10),Q(:,83),ZERO,0_intkind1,wf(:,70))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,71))
  call prop_A_Q(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,72))
  call prop_Q_A(wf(:,69),Q(:,44),ZERO,0_intkind1,wf(:,73))
  call counter_QA_W(wf(:,-5),wf(:,19),wf(:,74))
  call vert_QA_W(wf(:,73),wf(:,-4),wf(:,75))
  call prop_Q_A(wf(:,71),Q(:,35),ZERO,0_intkind1,wf(:,76))
  call counter_VQ_A(wf(:,-6),wf(:,-5),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,96),ZERO,0_intkind1,wf(:,78))
  call vert_VQ_A(wf(:,2),wf(:,78),wf(:,79))
  call vert_WQ_A(wf(:,4),wf(:,78),wf(:,80))
  call counter_AV_Q(wf(:,-4),wf(:,2),wf(:,81))
  call prop_Q_A(wf(:,18),Q(:,99),ZERO,0_intkind1,wf(:,82))
  call counter_AW_Q(wf(:,-4),wf(:,4),wf(:,83))
  call prop_Q_A(wf(:,15),Q(:,108),ZERO,0_intkind1,wf(:,84))
  call prop_A_Q(wf(:,81),Q(:,28),ZERO,0_intkind1,wf(:,85))
  call vert_AV_Q(wf(:,85),wf(:,-6),wf(:,86))
  call counter_QA_W(wf(:,11),wf(:,-4),wf(:,87))
  call vert_QA_W(wf(:,-5),wf(:,85),wf(:,88))
  call prop_A_Q(wf(:,83),Q(:,19),ZERO,0_intkind1,wf(:,89))
  call vert_AV_Q(wf(:,89),wf(:,-6),wf(:,90))
  call counter_AV_Q(wf(:,-4),wf(:,-6),wf(:,91))
  call prop_A_Q(wf(:,91),Q(:,80),ZERO,0_intkind1,wf(:,92))
  call vert_AV_Q(wf(:,92),wf(:,2),wf(:,93))
  call vert_AW_Q(wf(:,92),wf(:,4),wf(:,94))
  call counter_AV_Q(wf(:,28),wf(:,26),wf(:,95))
  call counter_AW_Q(wf(:,28),wf(:,4),wf(:,96))
  call counter_VQ_A(wf(:,26),wf(:,13),wf(:,97))
  call counter_AV_Q(wf(:,35),wf(:,-6),wf(:,98))
  call counter_AV_Q(wf(:,37),wf(:,-6),wf(:,99))
  call counter_VQ_A(wf(:,26),wf(:,-5),wf(:,100))
  call prop_A_Q(wf(:,31),Q(:,75),ZERO,0_intkind1,wf(:,101))
  call prop_A_Q(wf(:,29),Q(:,92),ZERO,0_intkind1,wf(:,102))
  call prop_Q_A(wf(:,100),Q(:,52),ZERO,0_intkind1,wf(:,103))
  call counter_QA_W(wf(:,-5),wf(:,37),wf(:,104))
  call vert_QA_W(wf(:,103),wf(:,-3),wf(:,105))
  call vert_VQ_A(wf(:,26),wf(:,78),wf(:,106))
  call counter_QA_V(wf(:,-5),wf(:,35),wf(:,107))
  call counter_VQ_A(wf(:,46),wf(:,-5),wf(:,108))
  call counter_VQ_A(wf(:,50),wf(:,-5),wf(:,109))
  call counter_VQ_A(wf(:,53),wf(:,-5),wf(:,110))
  call counter_QA_V(wf(:,43),wf(:,-4),wf(:,111))
  call counter_AV_Q(wf(:,-4),wf(:,46),wf(:,112))
  call vert_AV_Q(wf(:,-3),wf(:,111),wf(:,113))
  call counter_AV_Q(wf(:,-4),wf(:,50),wf(:,114))
  call vert_QA_V(wf(:,-2),wf(:,92),wf(:,115))
  call vert_VQ_A(wf(:,115),wf(:,-5),wf(:,116))
  call vert_AV_Q(wf(:,-3),wf(:,115),wf(:,117))
  call counter_AV_Q(wf(:,-3),wf(:,26),wf(:,118))
  call counter_AW_Q(wf(:,-3),wf(:,4),wf(:,119))
  call prop_Q_A(wf(:,34),Q(:,116),ZERO,0_intkind1,wf(:,120))
  call prop_A_Q(wf(:,118),Q(:,28),ZERO,0_intkind1,wf(:,121))
  call vert_AV_Q(wf(:,121),wf(:,-6),wf(:,122))
  call counter_QA_W(wf(:,32),wf(:,-3),wf(:,123))
  call vert_QA_W(wf(:,-5),wf(:,121),wf(:,124))
  call prop_A_Q(wf(:,119),Q(:,11),ZERO,0_intkind1,wf(:,125))
  call vert_AV_Q(wf(:,125),wf(:,-6),wf(:,126))
  call counter_AV_Q(wf(:,-3),wf(:,-6),wf(:,127))
  call prop_A_Q(wf(:,127),Q(:,72),ZERO,0_intkind1,wf(:,128))
  call vert_AV_Q(wf(:,128),wf(:,26),wf(:,129))
  call vert_AW_Q(wf(:,128),wf(:,4),wf(:,130))
  call counter_QA_V(wf(:,43),wf(:,-3),wf(:,131))
  call vert_VQ_A(wf(:,131),wf(:,-5),wf(:,132))
  call counter_AV_Q(wf(:,-3),wf(:,44),wf(:,133))
  call vert_AV_Q(wf(:,-4),wf(:,131),wf(:,134))
  call vert_QA_V(wf(:,-5),wf(:,125),wf(:,135))
  call counter_AV_Q(wf(:,-3),wf(:,53),wf(:,136))
  call vert_QA_V(wf(:,-2),wf(:,128),wf(:,137))
  call vert_VQ_A(wf(:,137),wf(:,-5),wf(:,138))
  call vert_AV_Q(wf(:,-4),wf(:,137),wf(:,139))
  call counter_QA_V(wf(:,-2),wf(:,28),wf(:,140))
  call vert_VQ_A(wf(:,140),wf(:,-5),wf(:,141))
  call vert_AV_Q(wf(:,-4),wf(:,140),wf(:,142))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,143))
  call vert_VQ_A(wf(:,143),wf(:,-5),wf(:,144))
  call vert_AV_Q(wf(:,-3),wf(:,143),wf(:,145))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,146))
  call prop_Q_A(wf(:,146),Q(:,68),ZERO,0_intkind1,wf(:,147))
  call vert_QA_V(wf(:,147),wf(:,-4),wf(:,148))
  call vert_QA_V(wf(:,147),wf(:,-3),wf(:,149))
  call vert_VQ_A(wf(:,149),wf(:,-5),wf(:,150))
  call vert_AV_Q(wf(:,-4),wf(:,149),wf(:,151))
  call vert_AV_Q(wf(:,-3),wf(:,148),wf(:,152))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,153))
  call vert_AV_Q(wf(:,28),wf(:,153),wf(:,154))
  call vert_VQ_A(wf(:,153),wf(:,-5),wf(:,155))
  call prop_Q_A(wf(:,155),Q(:,52),ZERO,0_intkind1,wf(:,156))
  call vert_VQ_A(wf(:,153),wf(:,13),wf(:,157))
  call vert_AV_Q(wf(:,-3),wf(:,153),wf(:,158))
  call prop_A_Q(wf(:,158),Q(:,28),ZERO,0_intkind1,wf(:,159))
  call vert_AV_Q(wf(:,159),wf(:,-6),wf(:,160))
  call vert_QA_W(wf(:,-5),wf(:,159),wf(:,161))
  call vert_QA_W(wf(:,156),wf(:,-3),wf(:,162))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,163))
  call vert_AV_Q(wf(:,5),wf(:,163),wf(:,164))
  call vert_VQ_A(wf(:,163),wf(:,-5),wf(:,165))
  call prop_Q_A(wf(:,165),Q(:,44),ZERO,0_intkind1,wf(:,166))
  call vert_VQ_A(wf(:,163),wf(:,13),wf(:,167))
  call vert_AV_Q(wf(:,-4),wf(:,163),wf(:,168))
  call prop_A_Q(wf(:,168),Q(:,28),ZERO,0_intkind1,wf(:,169))
  call vert_AV_Q(wf(:,169),wf(:,-6),wf(:,170))
  call vert_QA_W(wf(:,-5),wf(:,169),wf(:,171))
  call vert_QA_W(wf(:,166),wf(:,-4),wf(:,172))
  call counter_WQ_A(wf(:,59),wf(:,-5),wf(:,173))
  call counter_AW_Q(wf(:,-4),wf(:,59),wf(:,174))
  call counter_AW_Q(wf(:,-3),wf(:,59),wf(:,175))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,176))
  call vert_VQ_A(wf(:,176),wf(:,-5),wf(:,177))
  call vert_AV_Q(wf(:,5),wf(:,176),wf(:,178))
  call counter_A_Q(ctqq,wf(:,5),Q(:,80),wf(:,179))
  call prop_A_Q(wf(:,179),Q(:,80),ZERO,0_intkind1,wf(:,180))
  call vert_AV_Q(wf(:,180),wf(:,2),wf(:,181))
  call vert_AW_Q(wf(:,180),wf(:,4),wf(:,182))
  call counter_Q_A(ctqq,wf(:,8),Q(:,35),wf(:,183))
  call counter_Q_A(ctqq,wf(:,11),Q(:,44),wf(:,184))
  call vert_AV_Q(wf(:,-4),wf(:,176),wf(:,185))
  call vert_AV_Q(wf(:,16),wf(:,176),wf(:,186))
  call counter_A_Q(ctqq,wf(:,16),Q(:,19),wf(:,187))
  call counter_A_Q(ctqq,wf(:,19),Q(:,28),wf(:,188))
  call counter_Q_A(ctqq,wf(:,13),Q(:,96),wf(:,189))
  call prop_Q_A(wf(:,189),Q(:,96),ZERO,0_intkind1,wf(:,190))
  call vert_VQ_A(wf(:,2),wf(:,190),wf(:,191))
  call vert_WQ_A(wf(:,4),wf(:,190),wf(:,192))
  call vert_VQ_A(wf(:,-6),wf(:,8),wf(:,193))
  call prop_A_Q(wf(:,185),Q(:,28),ZERO,0_intkind1,wf(:,194))
  call vert_WQ_A(wf(:,23),wf(:,-5),wf(:,195))
  call prop_Q_A(wf(:,177),Q(:,44),ZERO,0_intkind1,wf(:,196))
  call vert_AW_Q(wf(:,-4),wf(:,23),wf(:,197))
  call prop_A_Q(wf(:,197),Q(:,83),ZERO,0_intkind1,wf(:,198))
  call prop_Q_A(wf(:,195),Q(:,99),ZERO,0_intkind1,wf(:,199))
  call vert_VQ_A(wf(:,-6),wf(:,11),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,108),ZERO,0_intkind1,wf(:,201))
  call prop_Q_A(wf(:,193),Q(:,99),ZERO,0_intkind1,wf(:,202))
  call prop_A_Q(wf(:,20),Q(:,83),ZERO,0_intkind1,wf(:,203))
  call prop_A_Q(wf(:,21),Q(:,92),ZERO,0_intkind1,wf(:,204))
  call counter_V_V(ctGG,wf(:,26),Q(:,20),wf(:,205))
  call vert_VQ_A(wf(:,205),wf(:,-5),wf(:,206))
  call vert_AV_Q(wf(:,28),wf(:,205),wf(:,207))
  call counter_A_Q(ctqq,wf(:,28),Q(:,72),wf(:,208))
  call prop_A_Q(wf(:,208),Q(:,72),ZERO,0_intkind1,wf(:,209))
  call vert_AV_Q(wf(:,209),wf(:,26),wf(:,210))
  call vert_AW_Q(wf(:,209),wf(:,4),wf(:,211))
  call counter_Q_A(ctqq,wf(:,32),Q(:,52),wf(:,212))
  call vert_AV_Q(wf(:,-3),wf(:,205),wf(:,213))
  call vert_AV_Q(wf(:,35),wf(:,205),wf(:,214))
  call counter_A_Q(ctqq,wf(:,35),Q(:,11),wf(:,215))
  call counter_A_Q(ctqq,wf(:,37),Q(:,28),wf(:,216))
  call vert_VQ_A(wf(:,26),wf(:,190),wf(:,217))
  call prop_A_Q(wf(:,213),Q(:,28),ZERO,0_intkind1,wf(:,218))
  call prop_Q_A(wf(:,206),Q(:,52),ZERO,0_intkind1,wf(:,219))
  call vert_AW_Q(wf(:,-3),wf(:,23),wf(:,220))
  call prop_A_Q(wf(:,220),Q(:,75),ZERO,0_intkind1,wf(:,221))
  call vert_VQ_A(wf(:,-6),wf(:,32),wf(:,222))
  call prop_Q_A(wf(:,222),Q(:,116),ZERO,0_intkind1,wf(:,223))
  call prop_A_Q(wf(:,38),Q(:,75),ZERO,0_intkind1,wf(:,224))
  call prop_A_Q(wf(:,39),Q(:,92),ZERO,0_intkind1,wf(:,225))
  call counter_Q_A(ctqq,wf(:,43),Q(:,68),wf(:,226))
  call prop_Q_A(wf(:,226),Q(:,68),ZERO,0_intkind1,wf(:,227))
  call vert_QA_V(wf(:,227),wf(:,-3),wf(:,228))
  call vert_QA_V(wf(:,-5),wf(:,16),wf(:,229))
  call vert_QA_V(wf(:,8),wf(:,-4),wf(:,230))
  call vert_QA_V(wf(:,227),wf(:,-4),wf(:,231))
  call vert_QA_V(wf(:,8),wf(:,-3),wf(:,232))
  call counter_V_V(ctGG,wf(:,46),Q(:,76),wf(:,233))
  call counter_V_V(ctGG,wf(:,44),Q(:,84),wf(:,234))
  call vert_VQ_A(wf(:,44),wf(:,-5),wf(:,235))
  call prop_Q_A(wf(:,235),Q(:,116),ZERO,0_intkind1,wf(:,236))
  call prop_A_Q(wf(:,49),Q(:,92),ZERO,0_intkind1,wf(:,237))
  call prop_Q_A(wf(:,47),Q(:,108),ZERO,0_intkind1,wf(:,238))
  call prop_A_Q(wf(:,48),Q(:,92),ZERO,0_intkind1,wf(:,239))
  call vert_QA_V(wf(:,-2),wf(:,209),wf(:,240))
  call counter_V_V(ctGG,wf(:,50),Q(:,76),wf(:,241))
  call prop_Q_A(wf(:,51),Q(:,108),ZERO,0_intkind1,wf(:,242))
  call prop_A_Q(wf(:,52),Q(:,92),ZERO,0_intkind1,wf(:,243))
  call counter_V_V(ctGG,wf(:,53),Q(:,84),wf(:,244))
  call vert_QA_V(wf(:,-2),wf(:,180),wf(:,245))
  call prop_Q_A(wf(:,54),Q(:,116),ZERO,0_intkind1,wf(:,246))
  call prop_A_Q(wf(:,55),Q(:,92),ZERO,0_intkind1,wf(:,247))
  call prop_A_Q(wf(:,60),Q(:,83),ZERO,0_intkind1,wf(:,248))
  call prop_Q_A(wf(:,61),Q(:,99),ZERO,0_intkind1,wf(:,249))
  call prop_A_Q(wf(:,62),Q(:,75),ZERO,0_intkind1,wf(:,250))
  call vert_AV_Q(wf(:,16),wf(:,2),wf(:,251))
  call prop_A_Q(wf(:,251),Q(:,31),ZERO,0_intkind1,wf(:,252))
  call vert_VQ_A(wf(:,2),wf(:,8),wf(:,253))
  call prop_Q_A(wf(:,253),Q(:,47),ZERO,0_intkind1,wf(:,254))
  call vert_AW_Q(wf(:,19),wf(:,4),wf(:,255))
  call prop_A_Q(wf(:,255),Q(:,31),ZERO,0_intkind1,wf(:,256))
  call vert_WQ_A(wf(:,4),wf(:,11),wf(:,257))
  call prop_Q_A(wf(:,257),Q(:,47),ZERO,0_intkind1,wf(:,258))
  call vert_AV_Q(wf(:,35),wf(:,26),wf(:,259))
  call prop_A_Q(wf(:,259),Q(:,31),ZERO,0_intkind1,wf(:,260))
  call vert_VQ_A(wf(:,26),wf(:,8),wf(:,261))
  call prop_Q_A(wf(:,261),Q(:,55),ZERO,0_intkind1,wf(:,262))
  call vert_AW_Q(wf(:,37),wf(:,4),wf(:,263))
  call prop_A_Q(wf(:,263),Q(:,31),ZERO,0_intkind1,wf(:,264))
  call vert_WQ_A(wf(:,4),wf(:,32),wf(:,265))
  call prop_Q_A(wf(:,265),Q(:,55),ZERO,0_intkind1,wf(:,266))
  call vert_QA_V(wf(:,8),wf(:,28),wf(:,267))
  call vert_QA_V(wf(:,-5),wf(:,101),wf(:,268))
  call vert_QA_V(wf(:,8),wf(:,5),wf(:,269))
  call vert_QA_V(wf(:,-5),wf(:,70),wf(:,270))
  call vert_QA_V(wf(:,13),wf(:,35),wf(:,271))
  call vert_QA_V(wf(:,13),wf(:,16),wf(:,272))
  call vert_QA_V(wf(:,82),wf(:,-3),wf(:,273))
  call vert_QA_V(wf(:,82),wf(:,-4),wf(:,274))
  call vert_VQ_A(wf(:,45),wf(:,-2),wf(:,275))
  call prop_Q_A(wf(:,275),Q(:,47),ZERO,0_intkind1,wf(:,276))
  call vert_AV_Q(wf(:,-4),wf(:,45),wf(:,277))
  call prop_A_Q(wf(:,277),Q(:,59),ZERO,0_intkind1,wf(:,278))
  call vert_QA_V(wf(:,-5),wf(:,224),wf(:,279))
  call vert_VQ_A(wf(:,229),wf(:,-2),wf(:,280))
  call prop_Q_A(wf(:,280),Q(:,55),ZERO,0_intkind1,wf(:,281))
  call vert_AV_Q(wf(:,-3),wf(:,229),wf(:,282))
  call prop_A_Q(wf(:,282),Q(:,59),ZERO,0_intkind1,wf(:,283))
  call vert_QA_V(wf(:,-5),wf(:,203),wf(:,284))
  call vert_VQ_A(wf(:,232),wf(:,-2),wf(:,285))
  call prop_Q_A(wf(:,285),Q(:,47),ZERO,0_intkind1,wf(:,286))
  call vert_VQ_A(wf(:,230),wf(:,-2),wf(:,287))
  call prop_Q_A(wf(:,287),Q(:,55),ZERO,0_intkind1,wf(:,288))
  call vert_AV_Q(wf(:,-4),wf(:,232),wf(:,289))
  call prop_A_Q(wf(:,289),Q(:,59),ZERO,0_intkind1,wf(:,290))
  call vert_AV_Q(wf(:,-3),wf(:,230),wf(:,291))
  call prop_A_Q(wf(:,291),Q(:,59),ZERO,0_intkind1,wf(:,292))
  call vert_QA_V(wf(:,202),wf(:,-3),wf(:,293))
  call vert_QA_V(wf(:,202),wf(:,-4),wf(:,294))
  call vert_QA_V(wf(:,-5),wf(:,221),wf(:,295))
  call vert_QA_V(wf(:,199),wf(:,-3),wf(:,296))
  call vert_QA_V(wf(:,-5),wf(:,198),wf(:,297))
  call vert_QA_V(wf(:,199),wf(:,-4),wf(:,298))
  call vert_QA_V(wf(:,-5),wf(:,250),wf(:,299))
  call vert_QA_V(wf(:,-5),wf(:,248),wf(:,300))
  call vert_QA_V(wf(:,249),wf(:,-3),wf(:,301))
  call vert_QA_V(wf(:,249),wf(:,-4),wf(:,302))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,35))
  den(9) = 1 / (Q(5,44))
  den(12) = 1 / (Q(5,96))
  den(14) = 1 / (Q(5,19))
  den(18) = 1 / (Q(5,28))
  den(23) = 1 / (Q(5,67) - MW2)
  den(27) = 1 / (Q(5,20))
  den(28) = 1 / (Q(5,72))
  den(32) = 1 / (Q(5,52))
  den(36) = 1 / (Q(5,11))
  den(45) = 1 / (Q(5,68))
  den(46) = 1 / (Q(5,84))
  den(49) = 1 / (Q(5,76))
  den(60) = 1 / (Q(5,66))
  den(66) = 1 / (Q(5,83))
  den(69) = 1 / (Q(5,92))
  den(72) = 1 / (Q(5,99))
  den(75) = 1 / (Q(5,108))
  den(78) = 1 / (Q(5,75))
  den(84) = 1 / (Q(5,116))
  den(167) = 1 / (Q(5,43))
  den(170) = 1 / (Q(5,51))
  den(216) = 1 / (Q(5,31))
  den(219) = 1 / (Q(5,47))
  den(228) = 1 / (Q(5,55))
  den(235) = 1 / (Q(5,107))
  den(239) = 1 / (Q(5,115))
  den(249) = 1 / (Q(5,59))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(3)
  den(10) = den(2)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(2)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(1)*den(12)
  den(19) = den(2)*den(18)
  den(20) = den(17)*den(19)
  den(21) = den(10)*den(15)
  den(22) = den(6)*den(19)
  den(24) = den(1)*den(23)
  den(25) = den(19)*den(24)
  den(26) = den(10)*den(24)
  den(29) = den(27)*den(28)
  den(30) = den(6)*den(29)
  den(31) = den(1)*den(28)
  den(33) = den(27)*den(32)
  den(34) = den(31)*den(33)
  den(35) = den(12)*den(27)
  den(37) = den(1)*den(36)
  den(38) = den(35)*den(37)
  den(39) = den(18)*den(27)
  den(40) = den(17)*den(39)
  den(41) = den(33)*den(37)
  den(42) = den(6)*den(39)
  den(43) = den(24)*den(39)
  den(44) = den(24)*den(33)
  den(47) = den(45)*den(46)
  den(48) = den(37)*den(47)
  den(50) = den(45)*den(49)
  den(51) = den(15)*den(50)
  den(52) = den(6)*den(50)
  den(53) = den(6)*den(47)
  den(54) = den(28)*den(49)
  den(55) = den(15)*den(54)
  den(56) = den(6)*den(54)
  den(57) = den(3)*den(46)
  den(58) = den(37)*den(57)
  den(59) = den(6)*den(57)
  den(61) = den(23)*den(60)
  den(62) = den(10)*den(61)
  den(63) = den(19)*den(61)
  den(64) = den(33)*den(61)
  den(65) = den(39)*den(61)
  den(67) = den(8)*den(66)
  den(68) = den(2)*den(67)
  den(70) = den(4)*den(69)
  den(71) = den(1)*den(70)
  den(73) = den(17)*den(72)
  den(74) = den(2)*den(73)
  den(76) = den(13)*den(75)
  den(77) = den(1)*den(76)
  den(79) = den(31)*den(78)
  den(80) = den(27)*den(79)
  den(81) = den(29)*den(69)
  den(82) = den(1)*den(81)
  den(83) = den(27)*den(73)
  den(85) = den(35)*den(84)
  den(86) = den(1)*den(85)
  den(87) = den(2)**2
  den(88) = den(67)*den(87)
  den(89) = den(3)*den(87)
  den(90) = den(6)*den(89)
  den(91) = den(3)**2
  den(92) = den(2)*den(91)
  den(93) = den(6)*den(92)
  den(94) = den(1)*den(91)
  den(95) = den(10)*den(94)
  den(96) = den(6)*den(70)
  den(97) = den(10)*den(67)
  den(98) = den(73)*den(87)
  den(99) = den(15)*den(87)
  den(100) = den(12)*den(99)
  den(101) = den(15)*den(76)
  den(102) = den(19)*den(73)
  den(103) = den(12)**2
  den(104) = den(2)*den(103)
  den(105) = den(15)*den(104)
  den(106) = den(1)*den(103)
  den(107) = den(19)*den(106)
  den(108) = den(18)*den(87)
  den(109) = den(6)*den(108)
  den(110) = den(24)*den(108)
  den(111) = den(9)*den(87)
  den(112) = den(15)*den(111)
  den(113) = den(24)*den(66)
  den(114) = den(87)*den(113)
  den(115) = den(24)*den(72)
  den(116) = den(19)*den(115)
  den(117) = den(10)*den(113)
  den(118) = den(10)*den(75)
  den(119) = den(15)*den(118)
  den(120) = den(6)*den(72)
  den(121) = den(19)*den(120)
  den(122) = den(15)*den(66)
  den(123) = den(10)*den(122)
  den(124) = den(19)*den(69)
  den(125) = den(6)*den(124)
  den(126) = den(27)**2
  den(127) = den(79)*den(126)
  den(128) = den(28)*den(126)
  den(129) = den(6)*den(128)
  den(130) = den(28)**2
  den(131) = den(27)*den(130)
  den(132) = den(6)*den(131)
  den(133) = den(1)*den(130)
  den(134) = den(33)*den(133)
  den(135) = den(6)*den(81)
  den(136) = den(33)*den(79)
  den(137) = den(73)*den(126)
  den(138) = den(37)*den(126)
  den(139) = den(12)*den(138)
  den(140) = den(37)*den(85)
  den(141) = den(39)*den(73)
  den(142) = den(27)*den(103)
  den(143) = den(37)*den(142)
  den(144) = den(39)*den(106)
  den(145) = den(18)*den(126)
  den(146) = den(6)*den(145)
  den(147) = den(24)*den(145)
  den(148) = den(32)*den(126)
  den(149) = den(37)*den(148)
  den(150) = den(24)*den(78)
  den(151) = den(126)*den(150)
  den(152) = den(39)*den(115)
  den(153) = den(33)*den(150)
  den(154) = den(33)*den(84)
  den(155) = den(37)*den(154)
  den(156) = den(39)*den(120)
  den(157) = den(37)*den(78)
  den(158) = den(33)*den(157)
  den(159) = den(39)*den(69)
  den(160) = den(6)*den(159)
  den(161) = den(45)**2
  den(162) = den(49)*den(161)
  den(163) = den(15)*den(162)
  den(164) = den(6)*den(162)
  den(165) = den(46)*den(161)
  den(166) = den(37)*den(165)
  den(168) = den(6)*den(167)
  den(169) = den(161)*den(168)
  den(171) = den(6)*den(170)
  den(172) = den(50)*den(171)
  den(173) = den(47)*den(168)
  den(174) = den(47)*den(84)
  den(175) = den(37)*den(174)
  den(176) = den(15)*den(170)
  den(177) = den(50)*den(176)
  den(178) = den(47)*den(69)
  den(179) = den(6)*den(178)
  den(180) = den(37)*den(167)
  den(181) = den(47)*den(180)
  den(182) = den(50)*den(75)
  den(183) = den(15)*den(182)
  den(184) = den(50)*den(69)
  den(185) = den(6)*den(184)
  den(186) = den(49)*den(130)
  den(187) = den(15)*den(186)
  den(188) = den(6)*den(186)
  den(189) = den(54)*den(171)
  den(190) = den(54)*den(176)
  den(191) = den(54)*den(75)
  den(192) = den(15)*den(191)
  den(193) = den(54)*den(69)
  den(194) = den(6)*den(193)
  den(195) = den(57)*den(168)
  den(196) = den(46)*den(91)
  den(197) = den(37)*den(196)
  den(198) = den(6)*den(196)
  den(199) = den(57)*den(180)
  den(200) = den(57)*den(84)
  den(201) = den(37)*den(200)
  den(202) = den(57)*den(69)
  den(203) = den(6)*den(202)
  den(204) = den(61)*den(108)
  den(205) = den(61)*den(66)
  den(206) = den(87)*den(205)
  den(207) = den(10)*den(205)
  den(208) = den(61)*den(72)
  den(209) = den(19)*den(208)
  den(210) = den(61)*den(145)
  den(211) = den(61)*den(78)
  den(212) = den(126)*den(211)
  den(213) = den(33)*den(211)
  den(214) = den(39)*den(208)
  den(215) = den(2)*den(15)
  den(217) = den(215)*den(216)
  den(218) = den(2)*den(6)
  den(220) = den(218)*den(219)
  den(221) = den(1)*den(19)
  den(222) = den(216)*den(221)
  den(223) = den(1)*den(10)
  den(224) = den(219)*den(223)
  den(225) = den(27)*den(37)
  den(226) = den(216)*den(225)
  den(227) = den(6)*den(27)
  den(229) = den(227)*den(228)
  den(230) = den(1)*den(39)
  den(231) = den(216)*den(230)
  den(232) = den(1)*den(33)
  den(233) = den(228)*den(232)
  den(234) = den(6)*den(28)
  den(236) = den(234)*den(235)
  den(237) = den(79)*den(235)
  den(238) = den(3)*den(6)
  den(240) = den(238)*den(239)
  den(241) = den(67)*den(239)
  den(242) = den(12)*den(37)
  den(243) = den(235)*den(242)
  den(244) = den(12)*den(15)
  den(245) = den(239)*den(244)
  den(246) = den(73)*den(235)
  den(247) = den(73)*den(239)
  den(248) = den(180)*den(219)
  den(250) = den(180)*den(249)
  den(251) = den(157)*den(235)
  den(252) = den(176)*den(228)
  den(253) = den(176)*den(249)
  den(254) = den(122)*den(239)
  den(255) = den(168)*den(219)
  den(256) = den(171)*den(228)
  den(257) = den(168)*den(249)
  den(258) = den(171)*den(249)
  den(259) = den(120)*den(235)
  den(260) = den(120)*den(239)
  den(261) = den(150)*den(235)
  den(262) = den(115)*den(235)
  den(263) = den(113)*den(239)
  den(264) = den(115)*den(239)
  den(265) = den(211)*den(235)
  den(266) = den(205)*den(239)
  den(267) = den(208)*den(235)
  den(268) = den(208)*den(239)
  den(269) = den(2)*den(3)*den(6)
  den(270) = den(1)*den(3)*den(10)
  den(271) = den(1)*den(2)*den(3)
  den(272) = den(2)*den(12)*den(15)
  den(273) = den(1)*den(12)*den(19)
  den(274) = den(1)*den(2)*den(12)
  den(275) = den(2)*den(176)
  den(276) = den(2)*den(122)
  den(277) = den(2)*den(171)
  den(278) = den(2)*den(120)
  den(279) = den(2)*den(113)
  den(280) = den(2)*den(115)
  den(281) = den(2)*den(24)
  den(282) = den(1)*den(124)
  den(283) = den(1)*den(118)
  den(284) = den(1)*den(2)
  den(285) = den(6)*den(27)*den(28)
  den(286) = den(1)*den(28)*den(33)
  den(287) = den(1)*den(27)*den(28)
  den(288) = den(12)*den(27)*den(37)
  den(289) = den(1)*den(12)*den(39)
  den(290) = den(1)*den(12)*den(27)
  den(291) = den(27)*den(180)
  den(292) = den(27)*den(157)
  den(293) = den(27)*den(168)
  den(294) = den(27)*den(120)
  den(295) = den(27)*den(150)
  den(296) = den(27)*den(115)
  den(297) = den(24)*den(27)
  den(298) = den(1)*den(159)
  den(299) = den(1)*den(154)
  den(300) = den(1)*den(27)
  den(301) = den(45)*den(180)
  den(302) = den(37)*den(45)
  den(303) = den(45)*den(176)
  den(304) = den(15)*den(45)
  den(305) = den(45)*den(168)
  den(306) = den(45)*den(171)
  den(307) = den(6)*den(45)
  den(308) = den(1)*den(184)
  den(309) = den(1)*den(182)
  den(310) = den(1)*den(50)
  den(311) = den(1)*den(178)
  den(312) = den(1)*den(174)
  den(313) = den(1)*den(47)
  den(314) = den(1)*den(45)
  den(315) = den(28)*den(176)
  den(316) = den(15)*den(28)
  den(317) = den(28)*den(171)
  den(318) = den(1)*den(193)
  den(319) = den(1)*den(191)
  den(320) = den(1)*den(54)
  den(321) = den(3)*den(180)
  den(322) = den(3)*den(37)
  den(323) = den(3)*den(168)
  den(324) = den(1)*den(202)
  den(325) = den(1)*den(200)
  den(326) = den(1)*den(57)
  den(327) = den(2)*den(205)
  den(328) = den(2)*den(208)
  den(329) = den(2)*den(61)
  den(330) = den(27)*den(211)
  den(331) = den(27)*den(208)
  den(332) = den(27)*den(61)
  den(333) = den(3)*den(220)
  den(334) = den(2)*den(240)
  den(335) = den(3)*den(224)
  den(336) = den(2)*den(241)
  den(337) = den(12)*den(217)
  den(338) = den(2)*den(245)
  den(339) = den(12)*den(222)
  den(340) = den(2)*den(247)
  den(341) = den(2)*den(254)
  den(342) = den(2)*den(260)
  den(343) = den(2)*den(263)
  den(344) = den(2)*den(264)
  den(345) = den(28)*den(229)
  den(346) = den(27)*den(236)
  den(347) = den(28)*den(233)
  den(348) = den(27)*den(237)
  den(349) = den(12)*den(226)
  den(350) = den(27)*den(243)
  den(351) = den(12)*den(231)
  den(352) = den(27)*den(246)
  den(353) = den(27)*den(251)
  den(354) = den(27)*den(259)
  den(355) = den(27)*den(261)
  den(356) = den(27)*den(262)
  den(357) = den(45)*den(250)
  den(358) = den(45)*den(253)
  den(359) = den(45)*den(257)
  den(360) = den(45)*den(258)
  den(361) = den(28)*den(252)
  den(362) = den(28)*den(256)
  den(363) = den(3)*den(248)
  den(364) = den(3)*den(255)
  den(365) = den(2)*den(266)
  den(366) = den(2)*den(268)
  den(367) = den(27)*den(265)
  den(368) = den(27)*den(267)

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
  A(9) = cont_QA(wf(:,8),wf(:,29)) * den(30)
  A(10) = cont_QA(wf(:,31),wf(:,32)) * den(34)
  A(11) = cont_QA(wf(:,34),wf(:,35)) * den(38)
  A(12) = cont_QA(wf(:,18),wf(:,37)) * den(40)
  A(13) = cont_QA(wf(:,32),wf(:,38)) * den(41)
  A(14) = cont_QA(wf(:,8),wf(:,39)) * den(42)
  A(15) = cont_VV(wf(:,23),wf(:,40)) * den(43)
  A(16) = cont_VV(wf(:,23),wf(:,41)) * den(44)
  A(17) = cont_VV(wf(:,44),wf(:,45)) * den(48)
  A(18) = cont_QA(wf(:,16),wf(:,47)) * den(51)
  A(19) = cont_QA(wf(:,8),wf(:,48)) * den(52)
  A(20) = cont_QA(wf(:,8),wf(:,49)) * den(53)
  A(21) = cont_QA(wf(:,16),wf(:,51)) * den(55)
  A(22) = cont_QA(wf(:,8),wf(:,52)) * den(56)
  A(23) = cont_QA(wf(:,35),wf(:,54)) * den(58)
  A(24) = cont_QA(wf(:,8),wf(:,55)) * den(59)
  A(25) = cont_QA(wf(:,11),wf(:,60)) * den(62)
  A(26) = cont_QA(wf(:,19),wf(:,61)) * den(63)
  A(27) = cont_QA(wf(:,32),wf(:,62)) * den(64)
  A(28) = cont_QA(wf(:,37),wf(:,61)) * den(65)

  A(29) = cont_QA(wf(:,8),wf(:,63)) * den(7)
  A(30) = cont_QA(wf(:,11),wf(:,64)) * den(11)
  A(31) = cont_QA(wf(:,16),wf(:,65)) * den(16)
  A(32) = cont_QA(wf(:,19),wf(:,66)) * den(20)
  A(33) = cont_QA(wf(:,11),wf(:,67)) * den(21)
  A(34) = cont_QA(wf(:,8),wf(:,68)) * den(22)
  A(35) = cont_QA(wf(:,69),wf(:,70)) * den(68)
  A(36) = cont_QA(wf(:,71),wf(:,72)) * den(71)
  A(37) = cont_QA(wf(:,20),wf(:,73)) * den(21)
  A(38) = cont_VV(wf(:,23),wf(:,74)) * den(25)
  A(39) = cont_VV(wf(:,23),wf(:,75)) * den(26)
  A(40) = cont_QA(wf(:,21),wf(:,76)) * den(22)
  A(41) = cont_QA(wf(:,16),wf(:,79)) * den(16)
  A(42) = cont_QA(wf(:,19),wf(:,80)) * den(20)
  A(43) = cont_QA(wf(:,81),wf(:,82)) * den(74)
  A(44) = cont_QA(wf(:,83),wf(:,84)) * den(77)
  A(45) = cont_QA(wf(:,8),wf(:,86)) * den(22)
  A(46) = cont_VV(wf(:,23),wf(:,87)) * den(26)
  A(47) = cont_VV(wf(:,23),wf(:,88)) * den(25)
  A(48) = cont_QA(wf(:,11),wf(:,90)) * den(21)
  A(49) = cont_QA(wf(:,8),wf(:,93)) * den(7)
  A(50) = cont_QA(wf(:,11),wf(:,94)) * den(11)
  A(51) = cont_QA(wf(:,8),wf(:,95)) * den(30)
  A(52) = cont_QA(wf(:,32),wf(:,96)) * den(34)
  A(53) = cont_QA(wf(:,35),wf(:,97)) * den(38)
  A(54) = cont_QA(wf(:,37),wf(:,66)) * den(40)
  A(55) = cont_QA(wf(:,32),wf(:,98)) * den(41)
  A(56) = cont_QA(wf(:,8),wf(:,99)) * den(42)
  A(57) = cont_QA(wf(:,100),wf(:,101)) * den(80)
  A(58) = cont_QA(wf(:,71),wf(:,102)) * den(82)
  A(59) = cont_QA(wf(:,38),wf(:,103)) * den(41)
  A(60) = cont_VV(wf(:,23),wf(:,104)) * den(43)
  A(61) = cont_VV(wf(:,23),wf(:,105)) * den(44)
  A(62) = cont_QA(wf(:,39),wf(:,76)) * den(42)
  A(63) = cont_QA(wf(:,35),wf(:,106)) * den(38)
  A(64) = cont_QA(wf(:,37),wf(:,80)) * den(40)
  A(65) = cont_VV(wf(:,44),wf(:,107)) * den(48)
  A(66) = cont_QA(wf(:,16),wf(:,108)) * den(51)
  A(67) = cont_QA(wf(:,48),wf(:,76)) * den(52)
  A(68) = cont_QA(wf(:,49),wf(:,76)) * den(53)
  A(69) = cont_QA(wf(:,16),wf(:,109)) * den(55)
  A(70) = cont_QA(wf(:,52),wf(:,76)) * den(56)
  A(71) = cont_QA(wf(:,35),wf(:,110)) * den(58)
  A(72) = cont_QA(wf(:,55),wf(:,76)) * den(59)
  A(73) = cont_VV(wf(:,45),wf(:,111)) * den(48)
  A(74) = cont_QA(wf(:,8),wf(:,112)) * den(52)
  A(75) = cont_QA(wf(:,8),wf(:,113)) * den(53)
  A(76) = cont_QA(wf(:,47),wf(:,89)) * den(51)
  A(77) = cont_QA(wf(:,8),wf(:,114)) * den(56)
  A(78) = cont_QA(wf(:,51),wf(:,89)) * den(55)
  A(79) = cont_QA(wf(:,35),wf(:,116)) * den(58)
  A(80) = cont_QA(wf(:,8),wf(:,117)) * den(59)
  A(81) = cont_QA(wf(:,82),wf(:,118)) * den(83)
  A(82) = cont_QA(wf(:,119),wf(:,120)) * den(86)
  A(83) = cont_QA(wf(:,8),wf(:,122)) * den(42)
  A(84) = cont_VV(wf(:,23),wf(:,123)) * den(44)
  A(85) = cont_VV(wf(:,23),wf(:,124)) * den(43)
  A(86) = cont_QA(wf(:,32),wf(:,126)) * den(41)
  A(87) = cont_QA(wf(:,8),wf(:,129)) * den(30)
  A(88) = cont_QA(wf(:,32),wf(:,130)) * den(34)
  A(89) = cont_QA(wf(:,16),wf(:,132)) * den(51)
  A(90) = cont_QA(wf(:,8),wf(:,133)) * den(53)
  A(91) = cont_QA(wf(:,8),wf(:,134)) * den(52)
  A(92) = cont_VV(wf(:,44),wf(:,135)) * den(48)
  A(93) = cont_QA(wf(:,8),wf(:,136)) * den(59)
  A(94) = cont_QA(wf(:,54),wf(:,125)) * den(58)
  A(95) = cont_QA(wf(:,16),wf(:,138)) * den(55)
  A(96) = cont_QA(wf(:,8),wf(:,139)) * den(56)
  A(97) = cont_QA(wf(:,16),wf(:,141)) * den(55)
  A(98) = cont_QA(wf(:,8),wf(:,142)) * den(56)
  A(99) = cont_QA(wf(:,35),wf(:,144)) * den(58)
  A(100) = cont_QA(wf(:,8),wf(:,145)) * den(59)
  A(101) = cont_VV(wf(:,45),wf(:,148)) * den(48)
  A(102) = cont_QA(wf(:,16),wf(:,150)) * den(51)
  A(103) = cont_QA(wf(:,8),wf(:,151)) * den(52)
  A(104) = cont_QA(wf(:,8),wf(:,152)) * den(53)
  A(105) = cont_QA(wf(:,8),wf(:,154)) * den(30)
  A(106) = cont_QA(wf(:,31),wf(:,156)) * den(34)
  A(107) = cont_QA(wf(:,35),wf(:,157)) * den(38)
  A(108) = cont_QA(wf(:,18),wf(:,159)) * den(40)
  A(109) = cont_QA(wf(:,38),wf(:,156)) * den(41)
  A(110) = cont_QA(wf(:,8),wf(:,160)) * den(42)
  A(111) = cont_VV(wf(:,23),wf(:,161)) * den(43)
  A(112) = cont_VV(wf(:,23),wf(:,162)) * den(44)
  A(113) = cont_QA(wf(:,8),wf(:,164)) * den(7)
  A(114) = cont_QA(wf(:,10),wf(:,166)) * den(11)
  A(115) = cont_QA(wf(:,16),wf(:,167)) * den(16)
  A(116) = cont_QA(wf(:,18),wf(:,169)) * den(20)
  A(117) = cont_QA(wf(:,20),wf(:,166)) * den(21)
  A(118) = cont_QA(wf(:,8),wf(:,170)) * den(22)
  A(119) = cont_VV(wf(:,23),wf(:,171)) * den(25)
  A(120) = cont_VV(wf(:,23),wf(:,172)) * den(26)
  A(121) = cont_QA(wf(:,60),wf(:,73)) * den(62)
  A(122) = cont_QA(wf(:,19),wf(:,173)) * den(63)
  A(123) = cont_QA(wf(:,61),wf(:,85)) * den(63)
  A(124) = cont_QA(wf(:,11),wf(:,174)) * den(62)
  A(125) = cont_QA(wf(:,62),wf(:,103)) * den(64)
  A(126) = cont_QA(wf(:,37),wf(:,173)) * den(65)
  A(127) = cont_QA(wf(:,61),wf(:,121)) * den(65)
  A(128) = cont_QA(wf(:,32),wf(:,175)) * den(64)
  A(129) = cont_QA(wf(:,62),wf(:,156)) * den(64)
  A(130) = cont_QA(wf(:,61),wf(:,159)) * den(65)
  A(131) = cont_QA(wf(:,60),wf(:,166)) * den(62)
  A(132) = cont_QA(wf(:,61),wf(:,169)) * den(63)
  A(133) = cont_QA(wf(:,70),wf(:,177)) * den(88)
  A(134) = cont_QA(wf(:,8),wf(:,178)) * den(90)
  A(135) = cont_QA(wf(:,8),wf(:,181)) * den(93)
  A(136) = cont_QA(wf(:,11),wf(:,182)) * den(95)
  A(137) = cont_QA(wf(:,72),wf(:,183)) * den(96)
  A(138) = cont_QA(wf(:,70),wf(:,184)) * den(97)
  A(139) = cont_QA(wf(:,82),wf(:,185)) * den(98)
  A(140) = cont_QA(wf(:,13),wf(:,186)) * den(100)
  A(141) = cont_QA(wf(:,84),wf(:,187)) * den(101)
  A(142) = cont_QA(wf(:,82),wf(:,188)) * den(102)
  A(143) = cont_QA(wf(:,16),wf(:,191)) * den(105)
  A(144) = cont_QA(wf(:,19),wf(:,192)) * den(107)
  A(145) = cont_QA(wf(:,193),wf(:,194)) * den(109)
  A(146) = cont_QA(wf(:,194),wf(:,195)) * den(110)
  A(147) = cont_QA(wf(:,20),wf(:,196)) * den(112)
  A(148) = cont_QA(wf(:,177),wf(:,198)) * den(114)
  A(149) = cont_QA(wf(:,188),wf(:,199)) * den(116)
  A(150) = cont_QA(wf(:,184),wf(:,198)) * den(117)
  A(151) = cont_QA(wf(:,187),wf(:,201)) * den(119)
  A(152) = cont_QA(wf(:,188),wf(:,202)) * den(121)
  A(153) = cont_QA(wf(:,184),wf(:,203)) * den(123)
  A(154) = cont_QA(wf(:,183),wf(:,204)) * den(125)
  A(155) = cont_QA(wf(:,101),wf(:,206)) * den(127)
  A(156) = cont_QA(wf(:,8),wf(:,207)) * den(129)
  A(157) = cont_QA(wf(:,8),wf(:,210)) * den(132)
  A(158) = cont_QA(wf(:,32),wf(:,211)) * den(134)
  A(159) = cont_QA(wf(:,102),wf(:,183)) * den(135)
  A(160) = cont_QA(wf(:,101),wf(:,212)) * den(136)
  A(161) = cont_QA(wf(:,82),wf(:,213)) * den(137)
  A(162) = cont_QA(wf(:,13),wf(:,214)) * den(139)
  A(163) = cont_QA(wf(:,120),wf(:,215)) * den(140)
  A(164) = cont_QA(wf(:,82),wf(:,216)) * den(141)
  A(165) = cont_QA(wf(:,35),wf(:,217)) * den(143)
  A(166) = cont_QA(wf(:,37),wf(:,192)) * den(144)
  A(167) = cont_QA(wf(:,193),wf(:,218)) * den(146)
  A(168) = cont_QA(wf(:,195),wf(:,218)) * den(147)
  A(169) = cont_QA(wf(:,38),wf(:,219)) * den(149)
  A(170) = cont_QA(wf(:,206),wf(:,221)) * den(151)
  A(171) = cont_QA(wf(:,199),wf(:,216)) * den(152)
  A(172) = cont_QA(wf(:,212),wf(:,221)) * den(153)
  A(173) = cont_QA(wf(:,215),wf(:,223)) * den(155)
  A(174) = cont_QA(wf(:,202),wf(:,216)) * den(156)
  A(175) = cont_QA(wf(:,212),wf(:,224)) * den(158)
  A(176) = cont_QA(wf(:,183),wf(:,225)) * den(160)
  A(177) = cont_VV(wf(:,228),wf(:,229)) * den(163)
  A(178) = cont_VV(wf(:,228),wf(:,230)) * den(164)
  A(179) = cont_VV(wf(:,45),wf(:,231)) * den(166)
  A(180) = cont_VV(wf(:,231),wf(:,232)) * den(169)
  A(181) = cont_VV(wf(:,230),wf(:,233)) * den(172)
  A(182) = cont_VV(wf(:,232),wf(:,234)) * den(173)
  A(183) = cont_QA(wf(:,215),wf(:,236)) * den(175)
  A(184) = cont_VV(wf(:,229),wf(:,233)) * den(177)
  A(185) = cont_QA(wf(:,183),wf(:,237)) * den(179)
  A(186) = cont_VV(wf(:,45),wf(:,234)) * den(181)
  A(187) = cont_QA(wf(:,187),wf(:,238)) * den(183)
  A(188) = cont_QA(wf(:,183),wf(:,239)) * den(185)
  A(189) = cont_VV(wf(:,229),wf(:,240)) * den(187)
  A(190) = cont_VV(wf(:,230),wf(:,240)) * den(188)
  A(191) = cont_VV(wf(:,230),wf(:,241)) * den(189)
  A(192) = cont_VV(wf(:,229),wf(:,241)) * den(190)
  A(193) = cont_QA(wf(:,187),wf(:,242)) * den(192)
  A(194) = cont_QA(wf(:,183),wf(:,243)) * den(194)
  A(195) = cont_VV(wf(:,232),wf(:,244)) * den(195)
  A(196) = cont_VV(wf(:,45),wf(:,245)) * den(197)
  A(197) = cont_VV(wf(:,232),wf(:,245)) * den(198)
  A(198) = cont_VV(wf(:,45),wf(:,244)) * den(199)
  A(199) = cont_QA(wf(:,215),wf(:,246)) * den(201)
  A(200) = cont_QA(wf(:,183),wf(:,247)) * den(203)
  A(201) = cont_QA(wf(:,61),wf(:,194)) * den(204)
  A(202) = cont_QA(wf(:,177),wf(:,248)) * den(206)
  A(203) = cont_QA(wf(:,184),wf(:,248)) * den(207)
  A(204) = cont_QA(wf(:,188),wf(:,249)) * den(209)
  A(205) = cont_QA(wf(:,61),wf(:,218)) * den(210)
  A(206) = cont_QA(wf(:,206),wf(:,250)) * den(212)
  A(207) = cont_QA(wf(:,212),wf(:,250)) * den(213)
  A(208) = cont_QA(wf(:,216),wf(:,249)) * den(214)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(208)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(3)+A(4)+A(5))*f(1))/2._/**/REALKIND+((A(11)+A(12)+A(13))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(6)-A(18)-A(19)-A(21) &
       -A(22))*f(2))/2._/**/REALKIND+((-A(9)-A(10)-A(14)-A(17)-A(20)-A(23)-A(24))*f(2))/6._/**/REALKIND+((-A(7)-A(8)+A(25) &
       +A(26))*f(3))/2._/**/REALKIND+((-A(15)-A(16)+A(27)+A(28))*f(3))/6._/**/REALKIND
  M1(2) = ((-A(3)-A(4)-A(5))*f(1))/6._/**/REALKIND+((-A(11)-A(12)-A(13))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(6)+A(18)+A(19)+A(21) &
       +A(22))*f(2))/6._/**/REALKIND+((A(9)+A(10)+A(14)+A(17)+A(20)+A(23)+A(24))*f(2))/2._/**/REALKIND+((A(7)+A(8)-A(25) &
       -A(26))*f(3))/6._/**/REALKIND+((A(15)+A(16)-A(27)-A(28))*f(3))/2._/**/REALKIND

  M2(1) = ((-A(139)-A(140)-A(141)-A(142)-A(143)-A(144)-A(147)-A(151)-A(153))*f(4))/2._/**/REALKIND+((-A(161)-A(162)-A(163)-A(164) &
       -A(165)-A(166)-A(169)-A(173)-A(175))*f(4))/6._/**/REALKIND+((A(133)+A(134)+A(135)+A(136)+A(137)+A(138)+A(145)+A(152)+A(154) &
       +A(177)+A(178)+A(181)+A(184)+A(187)+A(188)+A(189)+A(190)+A(191)+A(192)+A(193)+A(194))*f(5))/2._/**/REALKIND+((A(155)+A(156) &
       +A(157)+A(158)+A(159)+A(160)+A(167)+A(174)+A(176)+A(179)+A(180)+A(182)+A(183)+A(185)+A(186)+A(195)+A(196)+A(197)+A(198) &
       +A(199)+A(200))*f(5))/6._/**/REALKIND+((A(146)+A(148)+A(149)+A(150)-A(201)-A(202)-A(203)-A(204))*f(6))/2._/**/REALKIND &
       +((A(168)+A(170)+A(171)+A(172)-A(205)-A(206)-A(207)-A(208))*f(6))/6._/**/REALKIND+((A(53)+A(59)+A(81)+A(107)+A(108) &
       +A(109))*f(7))/6._/**/REALKIND+((A(31)+A(37)+A(43)+A(115)+A(116)+A(117))*f(7))/2._/**/REALKIND+((-A(51)-A(57)-A(65)-A(71) &
       -A(73)-A(75)-A(83)-A(90)-A(93)-A(99)-A(100)-A(105)-A(106)-A(110))*f(8))/6._/**/REALKIND+((-A(29)-A(35)-A(45)-A(66)-A(69) &
       -A(74)-A(77)-A(89)-A(91)-A(97)-A(98)-A(113)-A(114)-A(118))*f(8))/2._/**/REALKIND+((-A(61)-A(85)-A(111)-A(112)+A(125)+A(127) &
       +A(129)+A(130))*f(9))/6._/**/REALKIND+((-A(39)-A(47)-A(119)-A(120)+A(121)+A(123)+A(131)+A(132))*f(9))/2._/**/REALKIND &
       +((A(32)+A(33)+A(41)+A(42)+A(44)+A(48))*f(10))/2._/**/REALKIND+((A(54)+A(55)+A(63)+A(64)+A(82) &
       +A(86))*f(10))/6._/**/REALKIND+((-A(30)-A(34)-A(36)-A(40)-A(49)-A(50)-A(67)-A(70)-A(76)-A(78)-A(95)-A(96)-A(102) &
       -A(103))*f(11))/2._/**/REALKIND+((-A(52)-A(56)-A(58)-A(62)-A(68)-A(72)-A(79)-A(80)-A(87)-A(88)-A(92)-A(94)-A(101) &
       -A(104))*f(11))/6._/**/REALKIND+((-A(38)-A(46)+A(122)+A(124))*f(12))/2._/**/REALKIND+((-A(60)-A(84)+A(126) &
       +A(128))*f(12))/6._/**/REALKIND
  M2(2) = ((A(139)+A(140)+A(141)+A(142)+A(143)+A(144)+A(147)+A(151)+A(153))*f(4))/6._/**/REALKIND+((A(161)+A(162)+A(163)+A(164) &
       +A(165)+A(166)+A(169)+A(173)+A(175))*f(4))/2._/**/REALKIND+((-A(133)-A(134)-A(135)-A(136)-A(137)-A(138)-A(145)-A(152) &
       -A(154)-A(177)-A(178)-A(181)-A(184)-A(187)-A(188)-A(189)-A(190)-A(191)-A(192)-A(193)-A(194))*f(5))/6._/**/REALKIND+(( &
       -A(155)-A(156)-A(157)-A(158)-A(159)-A(160)-A(167)-A(174)-A(176)-A(179)-A(180)-A(182)-A(183)-A(185)-A(186)-A(195)-A(196) &
       -A(197)-A(198)-A(199)-A(200))*f(5))/2._/**/REALKIND+((-A(146)-A(148)-A(149)-A(150)+A(201)+A(202)+A(203) &
       +A(204))*f(6))/6._/**/REALKIND+((-A(168)-A(170)-A(171)-A(172)+A(205)+A(206)+A(207)+A(208))*f(6))/2._/**/REALKIND+((-A(53) &
       -A(59)-A(81)-A(107)-A(108)-A(109))*f(7))/2._/**/REALKIND+((-A(31)-A(37)-A(43)-A(115)-A(116)-A(117))*f(7))/6._/**/REALKIND &
       +((A(51)+A(57)+A(65)+A(71)+A(73)+A(75)+A(83)+A(90)+A(93)+A(99)+A(100)+A(105)+A(106)+A(110))*f(8))/2._/**/REALKIND+((A(29) &
       +A(35)+A(45)+A(66)+A(69)+A(74)+A(77)+A(89)+A(91)+A(97)+A(98)+A(113)+A(114)+A(118))*f(8))/6._/**/REALKIND+((A(61)+A(85) &
       +A(111)+A(112)-A(125)-A(127)-A(129)-A(130))*f(9))/2._/**/REALKIND+((A(39)+A(47)+A(119)+A(120)-A(121)-A(123)-A(131) &
       -A(132))*f(9))/6._/**/REALKIND+((-A(32)-A(33)-A(41)-A(42)-A(44)-A(48))*f(10))/6._/**/REALKIND+((-A(54)-A(55)-A(63)-A(64) &
       -A(82)-A(86))*f(10))/2._/**/REALKIND+((A(30)+A(34)+A(36)+A(40)+A(49)+A(50)+A(67)+A(70)+A(76)+A(78)+A(95)+A(96)+A(102) &
       +A(103))*f(11))/6._/**/REALKIND+((A(52)+A(56)+A(58)+A(62)+A(68)+A(72)+A(79)+A(80)+A(87)+A(88)+A(92)+A(94)+A(101) &
       +A(104))*f(11))/2._/**/REALKIND+((A(38)+A(46)-A(122)-A(124))*f(12))/6._/**/REALKIND+((A(60)+A(84)-A(126) &
       -A(128))*f(12))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnajj_neexuuxuxda_1_/**/REALKIND
