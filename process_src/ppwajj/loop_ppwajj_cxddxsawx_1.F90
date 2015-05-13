
module ol_colourmatrix_ppwajj_cxddxsawx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppwajj_cxddxsawx_1_/**/REALKIND



module ol_forced_parameters_ppwajj_cxddxsawx_1_/**/REALKIND
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
end module ol_forced_parameters_ppwajj_cxddxsawx_1_/**/REALKIND

module ol_loop_ppwajj_cxddxsawx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(29), c(47)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:147)
  ! denominators
  complex(REALKIND), save :: den(153)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,96)
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
    f( 1) = (CI*eQED**2*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f( 2) = (2*CI*eQED**2*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f( 3) = (CI*eQED**2*gQCD**2)/(sqrt2*sw)
    f( 4) = (CI*countertermnorm*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f( 5) = (2*CI*countertermnorm*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f( 6) = (CI*countertermnorm*eQED**2*gQCD**4)/(sqrt2*sw)
    f( 7) = (CI*countertermnorm*ctGcc*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f( 8) = (2*CI*countertermnorm*ctGcc*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f( 9) = (CI*countertermnorm*ctGcc*eQED**2*gQCD**4)/(sqrt2*sw)
    f(10) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f(11) = (2*CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f(12) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f(13) = (2*CI*countertermnorm*ctVcc*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f(14) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f(15) = (2*CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f(16) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f(17) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(3._/**/REALKIND*sqrt2*sw)
    f(18) = (2*CI*eQED**2*gQCD**4*integralnorm*SwB)/(3._/**/REALKIND*sqrt2*sw)
    f(19) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(20) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*6._/**/REALKIND)
    f(21) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(22) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*2._/**/REALKIND)
    f(23) = (2*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(24) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(25) = (eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw*3._/**/REALKIND)
    f(26) = (2*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw*3._/**/REALKIND)
    f(27) = (eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)
    f(28) = (4*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw*3._/**/REALKIND)
    f(29) = (2*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)

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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(92)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_AV_Q(wf(:,0),wf(:,-4),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,2))
  call vert_WQ_A(wf(:,-5),wf(:,-3),wf(:,3))
  call prop_A_Q(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_AV_Q(wf(:,4),wf(:,2),wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,-3),wf(:,7))
  call vert_AW_Q(wf(:,4),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,14),ZERO,0_intkind1,wf(:,9))
  call vert_AW_Q(wf(:,0),wf(:,-5),wf(:,10))
  call vert_VQ_A(wf(:,-4),wf(:,-3),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,33),ZERO,0_intkind1,wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,24),ZERO,0_intkind1,wf(:,13))
  call vert_AV_Q(wf(:,12),wf(:,2),wf(:,14))
  call vert_AV_Q(wf(:,0),wf(:,2),wf(:,15))
  call vert_WQ_A(wf(:,-5),wf(:,13),wf(:,16))
  call prop_A_Q(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_AV_Q(wf(:,12),wf(:,-4),wf(:,18))
  call vert_VQ_A(wf(:,-4),wf(:,5),wf(:,19))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,48),MW,1_intkind1,wf(:,21))
  call vert_WQ_A(wf(:,21),wf(:,-3),wf(:,22))
  call vert_AW_Q(wf(:,0),wf(:,21),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,49),ZERO,0_intkind1,wf(:,24))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,18),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,26),wf(:,-2),wf(:,27))
  call vert_QA_V(wf(:,-3),wf(:,12),wf(:,28))
  call vert_QA_V(wf(:,5),wf(:,0),wf(:,29))
  call vert_AV_Q(wf(:,-2),wf(:,-4),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,20),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-1),wf(:,31),wf(:,32))
  call counter_AV_Q(wf(:,4),wf(:,2),wf(:,33))
  call counter_AW_Q(wf(:,4),wf(:,-5),wf(:,34))
  call counter_AV_Q(wf(:,12),wf(:,2),wf(:,35))
  call counter_WQ_A(wf(:,-5),wf(:,13),wf(:,36))
  call counter_AV_Q(wf(:,12),wf(:,-4),wf(:,37))
  call counter_VQ_A(wf(:,-4),wf(:,5),wf(:,38))
  call counter_VQ_A(wf(:,2),wf(:,-3),wf(:,39))
  call prop_A_Q(wf(:,8),Q(:,49),ZERO,0_intkind1,wf(:,40))
  call counter_WQ_A(wf(:,-5),wf(:,-3),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,40),ZERO,0_intkind1,wf(:,42))
  call prop_A_Q(wf(:,18),Q(:,49),ZERO,0_intkind1,wf(:,43))
  call counter_WQ_A(wf(:,21),wf(:,-3),wf(:,44))
  call vert_VQ_A(wf(:,-4),wf(:,42),wf(:,45))
  call counter_VQ_A(wf(:,-4),wf(:,-3),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,24),ZERO,0_intkind1,wf(:,47))
  call vert_WQ_A(wf(:,-5),wf(:,47),wf(:,48))
  call counter_QA_V(wf(:,-3),wf(:,12),wf(:,49))
  call vert_QA_V(wf(:,42),wf(:,0),wf(:,50))
  call counter_QA_V(wf(:,26),wf(:,-2),wf(:,51))
  call counter_AV_Q(wf(:,-2),wf(:,-4),wf(:,52))
  call prop_A_Q(wf(:,52),Q(:,20),ZERO,0_intkind1,wf(:,53))
  call vert_QA_V(wf(:,-1),wf(:,53),wf(:,54))
  call counter_QA_V(wf(:,-1),wf(:,31),wf(:,55))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,18),ZERO,0_intkind1,wf(:,57))
  call vert_QA_V(wf(:,57),wf(:,-2),wf(:,58))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,59))
  call vert_AV_Q(wf(:,4),wf(:,59),wf(:,60))
  call vert_VQ_A(wf(:,59),wf(:,-3),wf(:,61))
  call prop_Q_A(wf(:,61),Q(:,14),ZERO,0_intkind1,wf(:,62))
  call vert_AV_Q(wf(:,12),wf(:,59),wf(:,63))
  call vert_AV_Q(wf(:,0),wf(:,59),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,7),ZERO,0_intkind1,wf(:,65))
  call counter_AV_Q(wf(:,0),wf(:,2),wf(:,66))
  call prop_Q_A(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,67))
  call counter_AW_Q(wf(:,0),wf(:,-5),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,33),ZERO,0_intkind1,wf(:,69))
  call vert_AV_Q(wf(:,69),wf(:,2),wf(:,70))
  call prop_Q_A(wf(:,19),Q(:,56),ZERO,0_intkind1,wf(:,71))
  call prop_Q_A(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,72))
  call counter_AW_Q(wf(:,0),wf(:,21),wf(:,73))
  call vert_AV_Q(wf(:,69),wf(:,-4),wf(:,74))
  call counter_AV_Q(wf(:,0),wf(:,-4),wf(:,75))
  call prop_A_Q(wf(:,75),Q(:,17),ZERO,0_intkind1,wf(:,76))
  call vert_AV_Q(wf(:,76),wf(:,2),wf(:,77))
  call vert_AW_Q(wf(:,76),wf(:,-5),wf(:,78))
  call counter_QA_V(wf(:,5),wf(:,0),wf(:,79))
  call vert_QA_V(wf(:,-3),wf(:,69),wf(:,80))
  call vert_VQ_A(wf(:,2),wf(:,5),wf(:,81))
  call counter_A_Q(ctcc,wf(:,4),Q(:,17),wf(:,82))
  call prop_Q_A(wf(:,81),Q(:,46),ZERO,0_intkind1,wf(:,83))
  call vert_QA_V(wf(:,5),wf(:,4),wf(:,84))
  call counter_V_V(ctGG,wf(:,2),Q(:,6),wf(:,85))
  call counter_Q_A(ctcc,wf(:,5),Q(:,40),wf(:,86))
  call prop_A_Q(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,87))
  call prop_A_Q(wf(:,82),Q(:,17),ZERO,0_intkind1,wf(:,88))
  call vert_AW_Q(wf(:,88),wf(:,-5),wf(:,89))
  call vert_VQ_A(wf(:,85),wf(:,-3),wf(:,90))
  call counter_Q_A(ctqq,wf(:,9),Q(:,14),wf(:,91))
  call vert_VQ_A(wf(:,2),wf(:,13),wf(:,92))
  call counter_A_Q(ctqq,wf(:,12),Q(:,33),wf(:,93))
  call prop_Q_A(wf(:,92),Q(:,30),ZERO,0_intkind1,wf(:,94))
  call vert_QA_V(wf(:,13),wf(:,12),wf(:,95))
  call counter_Q_A(ctqq,wf(:,13),Q(:,24),wf(:,96))
  call prop_A_Q(wf(:,14),Q(:,39),ZERO,0_intkind1,wf(:,97))
  call vert_AV_Q(wf(:,0),wf(:,85),wf(:,98))
  call counter_A_Q(ctcc,wf(:,17),Q(:,7),wf(:,99))
  call prop_Q_A(wf(:,96),Q(:,24),ZERO,0_intkind1,wf(:,100))
  call vert_WQ_A(wf(:,-5),wf(:,100),wf(:,101))
  call prop_A_Q(wf(:,93),Q(:,33),ZERO,0_intkind1,wf(:,102))
  call vert_AV_Q(wf(:,102),wf(:,-4),wf(:,103))
  call prop_Q_A(wf(:,86),Q(:,40),ZERO,0_intkind1,wf(:,104))
  call vert_VQ_A(wf(:,-4),wf(:,104),wf(:,105))
  call counter_A_Q(ctqq,wf(:,24),Q(:,49),wf(:,106))
  call vert_QA_V(wf(:,-3),wf(:,102),wf(:,107))
  call counter_Q_A(ctqq,wf(:,26),Q(:,18),wf(:,108))
  call prop_Q_A(wf(:,108),Q(:,18),ZERO,0_intkind1,wf(:,109))
  call vert_QA_V(wf(:,109),wf(:,-2),wf(:,110))
  call counter_V_V(ctGG,wf(:,27),Q(:,22),wf(:,111))
  call counter_V_V(ctGG,wf(:,29),Q(:,41),wf(:,112))
  call vert_QA_V(wf(:,104),wf(:,0),wf(:,113))
  call counter_A_Q(ctqq,wf(:,31),Q(:,20),wf(:,114))
  call prop_A_Q(wf(:,114),Q(:,20),ZERO,0_intkind1,wf(:,115))
  call vert_QA_V(wf(:,-1),wf(:,115),wf(:,116))
  call counter_V_V(ctGG,wf(:,32),Q(:,22),wf(:,117))
  call vert_AV_Q(wf(:,17),wf(:,-4),wf(:,118))
  call prop_A_Q(wf(:,118),Q(:,23),ZERO,0_intkind1,wf(:,119))
  call vert_AW_Q(wf(:,17),wf(:,-5),wf(:,120))
  call prop_A_Q(wf(:,120),Q(:,39),ZERO,0_intkind1,wf(:,121))
  call vert_VQ_A(wf(:,-4),wf(:,9),wf(:,122))
  call prop_Q_A(wf(:,122),Q(:,30),ZERO,0_intkind1,wf(:,123))
  call vert_WQ_A(wf(:,-5),wf(:,9),wf(:,124))
  call prop_Q_A(wf(:,124),Q(:,46),ZERO,0_intkind1,wf(:,125))
  call vert_QA_V(wf(:,-3),wf(:,40),wf(:,126))
  call vert_AV_Q(wf(:,0),wf(:,27),wf(:,127))
  call prop_A_Q(wf(:,127),Q(:,23),ZERO,0_intkind1,wf(:,128))
  call vert_VQ_A(wf(:,27),wf(:,-3),wf(:,129))
  call prop_Q_A(wf(:,129),Q(:,30),ZERO,0_intkind1,wf(:,130))
  call vert_AV_Q(wf(:,0),wf(:,32),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,23),ZERO,0_intkind1,wf(:,132))
  call vert_VQ_A(wf(:,32),wf(:,-3),wf(:,133))
  call prop_Q_A(wf(:,133),Q(:,30),ZERO,0_intkind1,wf(:,134))
  call vert_QA_V(wf(:,67),wf(:,0),wf(:,135))
  call vert_VQ_A(wf(:,28),wf(:,-1),wf(:,136))
  call prop_Q_A(wf(:,136),Q(:,43),ZERO,0_intkind1,wf(:,137))
  call vert_AV_Q(wf(:,-2),wf(:,28),wf(:,138))
  call prop_A_Q(wf(:,138),Q(:,45),ZERO,0_intkind1,wf(:,139))
  call vert_QA_V(wf(:,-3),wf(:,43),wf(:,140))
  call vert_VQ_A(wf(:,29),wf(:,-1),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,43),ZERO,0_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,-2),wf(:,29),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,45),ZERO,0_intkind1,wf(:,144))
  call vert_QA_V(wf(:,71),wf(:,0),wf(:,145))
  call vert_QA_V(wf(:,-3),wf(:,24),wf(:,146))
  call vert_QA_V(wf(:,72),wf(:,0),wf(:,147))

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
  den(6) = 1 / (Q(5,14))
  den(9) = 1 / (Q(5,33))
  den(10) = 1 / (Q(5,24))
  den(13) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,48) - MW2)
  den(20) = 1 / (Q(5,49))
  den(23) = 1 / (Q(5,18))
  den(24) = 1 / (Q(5,22))
  den(27) = 1 / (Q(5,41))
  den(30) = 1 / (Q(5,20))
  den(41) = 1 / (Q(5,56))
  den(52) = 1 / (Q(5,46))
  den(56) = 1 / (Q(5,57))
  den(59) = 1 / (Q(5,23))
  den(68) = 1 / (Q(5,30))
  den(74) = 1 / (Q(5,39))
  den(117) = 1 / (Q(5,43))
  den(119) = 1 / (Q(5,45))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(2)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(2)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(3)*den(14)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(2)*den(21)
  den(25) = den(23)*den(24)
  den(26) = den(9)*den(25)
  den(28) = den(3)*den(27)
  den(29) = den(23)*den(28)
  den(31) = den(24)*den(30)
  den(32) = den(9)*den(31)
  den(33) = den(28)*den(30)
  den(34) = den(1)*den(20)
  den(35) = den(2)*den(34)
  den(36) = den(9)*den(20)
  den(37) = den(2)*den(36)
  den(38) = den(9)*den(27)
  den(39) = den(23)*den(38)
  den(40) = den(30)*den(38)
  den(42) = den(10)*den(41)
  den(43) = den(2)*den(42)
  den(44) = den(3)*den(41)
  den(45) = den(2)*den(44)
  den(46) = den(18)*den(41)
  den(47) = den(2)*den(46)
  den(48) = den(7)*den(18)
  den(49) = den(3)*den(25)
  den(50) = den(3)*den(31)
  den(51) = den(2)*den(3)
  den(53) = den(51)*den(52)
  den(54) = den(1)*den(53)
  den(55) = den(1)*den(3)
  den(57) = den(55)*den(56)
  den(58) = den(2)*den(57)
  den(60) = den(4)*den(59)
  den(61) = den(3)*den(60)
  den(62) = den(1)**2
  den(63) = den(7)*den(62)
  den(64) = den(2)**2
  den(65) = den(34)*den(64)
  den(66) = den(7)*den(34)
  den(67) = den(2)*den(10)
  den(69) = den(67)*den(68)
  den(70) = den(9)*den(69)
  den(71) = den(9)*den(10)
  den(72) = den(56)*den(71)
  den(73) = den(2)*den(72)
  den(75) = den(11)*den(74)
  den(76) = den(10)*den(75)
  den(77) = den(42)*den(64)
  den(78) = den(14)*den(42)
  den(79) = den(10)**2
  den(80) = den(14)*den(79)
  den(81) = den(9)**2
  den(82) = den(7)*den(81)
  den(83) = den(36)*den(64)
  den(84) = den(7)*den(36)
  den(85) = den(44)*den(64)
  den(86) = den(14)*den(44)
  den(87) = den(3)**2
  den(88) = den(14)*den(87)
  den(89) = den(46)*den(64)
  den(90) = den(14)*den(46)
  den(91) = den(7)*den(21)
  den(92) = den(21)*den(64)
  den(93) = den(25)*den(81)
  den(94) = den(23)**2
  den(95) = den(38)*den(94)
  den(96) = den(25)*den(38)
  den(97) = den(25)*den(28)
  den(98) = den(25)*den(87)
  den(99) = den(28)*den(94)
  den(100) = den(31)*den(81)
  den(101) = den(30)**2
  den(102) = den(38)*den(101)
  den(103) = den(31)*den(38)
  den(104) = den(28)*den(31)
  den(105) = den(31)*den(87)
  den(106) = den(28)*den(101)
  den(107) = den(14)*den(59)
  den(108) = den(14)*den(74)
  den(109) = den(7)*den(68)
  den(110) = den(7)*den(52)
  den(111) = den(34)*den(56)
  den(112) = den(25)*den(59)
  den(113) = den(25)*den(68)
  den(114) = den(31)*den(59)
  den(115) = den(31)*den(68)
  den(116) = den(42)*den(56)
  den(118) = den(38)*den(117)
  den(120) = den(38)*den(119)
  den(121) = den(36)*den(56)
  den(122) = den(28)*den(117)
  den(123) = den(28)*den(119)
  den(124) = den(44)*den(56)
  den(125) = den(21)*den(56)
  den(126) = den(46)*den(56)
  den(127) = den(1)*den(2)*den(3)
  den(128) = den(2)*den(9)*den(10)
  den(129) = den(2)*den(38)
  den(130) = den(2)*den(28)
  den(131) = den(2)*den(18)
  den(132) = den(9)*den(23)
  den(133) = den(3)*den(23)
  den(134) = den(9)*den(30)
  den(135) = den(3)*den(30)
  den(136) = den(2)*den(111)
  den(137) = den(1)*den(110)
  den(138) = den(10)*den(108)
  den(139) = den(2)*den(116)
  den(140) = den(2)*den(121)
  den(141) = den(9)*den(109)
  den(142) = den(3)*den(107)
  den(143) = den(2)*den(124)
  den(144) = den(2)*den(125)
  den(145) = den(2)*den(126)
  den(146) = den(23)*den(120)
  den(147) = den(9)*den(113)
  den(148) = den(23)*den(123)
  den(149) = den(3)*den(112)
  den(150) = den(30)*den(118)
  den(151) = den(9)*den(115)
  den(152) = den(30)*den(122)
  den(153) = den(3)*den(114)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(92)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,17),wf(:,19)) * den(17)
  A(7) = cont_QA(wf(:,17),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,7),wf(:,24)) * den(22)
  A(9) = cont_VV(wf(:,27),wf(:,28)) * den(26)
  A(10) = cont_VV(wf(:,27),wf(:,29)) * den(29)
  A(11) = cont_VV(wf(:,28),wf(:,32)) * den(32)
  A(12) = cont_VV(wf(:,29),wf(:,32)) * den(33)

  A(13) = cont_QA(wf(:,5),wf(:,33)) * den(5)
  A(14) = cont_QA(wf(:,9),wf(:,34)) * den(8)
  A(15) = cont_QA(wf(:,13),wf(:,35)) * den(12)
  A(16) = cont_QA(wf(:,17),wf(:,36)) * den(15)
  A(17) = cont_QA(wf(:,9),wf(:,37)) * den(16)
  A(18) = cont_QA(wf(:,17),wf(:,38)) * den(17)
  A(19) = cont_QA(wf(:,39),wf(:,40)) * den(35)
  A(20) = cont_QA(wf(:,6),wf(:,42)) * den(5)
  A(21) = cont_QA(wf(:,39),wf(:,43)) * den(37)
  A(22) = cont_QA(wf(:,17),wf(:,44)) * den(19)
  A(23) = cont_QA(wf(:,24),wf(:,39)) * den(22)
  A(24) = cont_QA(wf(:,17),wf(:,45)) * den(17)
  A(25) = cont_QA(wf(:,14),wf(:,47)) * den(12)
  A(26) = cont_QA(wf(:,17),wf(:,48)) * den(15)
  A(27) = cont_VV(wf(:,27),wf(:,49)) * den(26)
  A(28) = cont_VV(wf(:,27),wf(:,50)) * den(29)
  A(29) = cont_VV(wf(:,32),wf(:,49)) * den(32)
  A(30) = cont_VV(wf(:,32),wf(:,50)) * den(33)
  A(31) = cont_VV(wf(:,28),wf(:,51)) * den(39)
  A(32) = cont_VV(wf(:,29),wf(:,51)) * den(29)
  A(33) = cont_VV(wf(:,28),wf(:,54)) * den(32)
  A(34) = cont_VV(wf(:,29),wf(:,54)) * den(33)
  A(35) = cont_VV(wf(:,28),wf(:,55)) * den(40)
  A(36) = cont_VV(wf(:,29),wf(:,55)) * den(33)
  A(37) = cont_VV(wf(:,28),wf(:,58)) * den(26)
  A(38) = cont_VV(wf(:,29),wf(:,58)) * den(29)
  A(39) = cont_QA(wf(:,5),wf(:,60)) * den(5)
  A(40) = cont_QA(wf(:,8),wf(:,62)) * den(8)
  A(41) = cont_QA(wf(:,13),wf(:,63)) * den(12)
  A(42) = cont_QA(wf(:,16),wf(:,65)) * den(15)
  A(43) = cont_QA(wf(:,18),wf(:,62)) * den(16)
  A(44) = cont_QA(wf(:,19),wf(:,65)) * den(17)
  A(45) = cont_QA(wf(:,24),wf(:,61)) * den(22)
  A(46) = cont_QA(wf(:,22),wf(:,65)) * den(19)
  A(47) = cont_QA(wf(:,66),wf(:,67)) * den(43)
  A(48) = cont_QA(wf(:,13),wf(:,70)) * den(12)
  A(49) = cont_QA(wf(:,66),wf(:,71)) * den(45)
  A(50) = cont_QA(wf(:,66),wf(:,72)) * den(47)
  A(51) = cont_QA(wf(:,9),wf(:,73)) * den(48)
  A(52) = cont_QA(wf(:,9),wf(:,74)) * den(16)
  A(53) = cont_QA(wf(:,5),wf(:,77)) * den(5)
  A(54) = cont_QA(wf(:,9),wf(:,78)) * den(8)
  A(55) = cont_VV(wf(:,27),wf(:,79)) * den(49)
  A(56) = cont_VV(wf(:,27),wf(:,80)) * den(26)
  A(57) = cont_VV(wf(:,32),wf(:,79)) * den(50)
  A(58) = cont_VV(wf(:,32),wf(:,80)) * den(32)
  A(59) = cont_QA(wf(:,82),wf(:,83)) * den(54)
  A(60) = cont_VV(wf(:,84),wf(:,85)) * den(58)
  A(61) = cont_QA(wf(:,86),wf(:,87)) * den(61)
  A(62) = cont_QA(wf(:,9),wf(:,89)) * den(63)
  A(63) = cont_QA(wf(:,40),wf(:,90)) * den(65)
  A(64) = cont_QA(wf(:,40),wf(:,91)) * den(66)
  A(65) = cont_QA(wf(:,93),wf(:,94)) * den(70)
  A(66) = cont_VV(wf(:,85),wf(:,95)) * den(73)
  A(67) = cont_QA(wf(:,96),wf(:,97)) * den(76)
  A(68) = cont_QA(wf(:,67),wf(:,98)) * den(77)
  A(69) = cont_QA(wf(:,67),wf(:,99)) * den(78)
  A(70) = cont_QA(wf(:,17),wf(:,101)) * den(80)
  A(71) = cont_QA(wf(:,9),wf(:,103)) * den(82)
  A(72) = cont_QA(wf(:,43),wf(:,90)) * den(83)
  A(73) = cont_QA(wf(:,43),wf(:,91)) * den(84)
  A(74) = cont_QA(wf(:,71),wf(:,98)) * den(85)
  A(75) = cont_QA(wf(:,71),wf(:,99)) * den(86)
  A(76) = cont_QA(wf(:,17),wf(:,105)) * den(88)
  A(77) = cont_QA(wf(:,72),wf(:,98)) * den(89)
  A(78) = cont_QA(wf(:,72),wf(:,99)) * den(90)
  A(79) = cont_QA(wf(:,9),wf(:,106)) * den(91)
  A(80) = cont_QA(wf(:,24),wf(:,90)) * den(92)
  A(81) = cont_VV(wf(:,27),wf(:,107)) * den(93)
  A(82) = cont_VV(wf(:,28),wf(:,110)) * den(95)
  A(83) = cont_VV(wf(:,28),wf(:,111)) * den(96)
  A(84) = cont_VV(wf(:,27),wf(:,112)) * den(97)
  A(85) = cont_VV(wf(:,27),wf(:,113)) * den(98)
  A(86) = cont_VV(wf(:,29),wf(:,110)) * den(99)
  A(87) = cont_VV(wf(:,32),wf(:,107)) * den(100)
  A(88) = cont_VV(wf(:,28),wf(:,116)) * den(102)
  A(89) = cont_VV(wf(:,28),wf(:,117)) * den(103)
  A(90) = cont_VV(wf(:,32),wf(:,112)) * den(104)
  A(91) = cont_VV(wf(:,32),wf(:,113)) * den(105)
  A(92) = cont_VV(wf(:,29),wf(:,116)) * den(106)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(92)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(3)+A(4)+A(5)+A(9)+A(10)+A(11)+A(12))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(6))*f(2))/6._/**/REALKIND+((-A(7) &
       -A(8))*f(3))/6._/**/REALKIND
  M1(2) = ((-A(3)-A(4)-A(5)-A(9)-A(10)-A(11)-A(12))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(6))*f(2))/2._/**/REALKIND+((A(7) &
       +A(8))*f(3))/2._/**/REALKIND

  M2(1) = ((-A(65)-A(66)-A(67)-A(68)-A(69)-A(70)-A(71)-A(72)-A(73)-A(81)-A(82)-A(83)-A(84)-A(85)-A(86)-A(87)-A(88)-A(89)-A(90) &
       -A(91)-A(92))*f(4))/6._/**/REALKIND+((A(59)+A(60)+A(61)+A(62)+A(63)+A(64)+A(74)+A(75)+A(76))*f(5))/6._/**/REALKIND+((A(77) &
       +A(78)+A(79)+A(80))*f(6))/6._/**/REALKIND+((A(47)+A(55)+A(57))*f(7))/6._/**/REALKIND+((-A(13)-A(49))*f(8))/6._/**/REALKIND &
       -(A(50)*f(9))/6._/**/REALKIND+((A(15)+A(21)+A(27)+A(29)+A(31)+A(32)+A(35)+A(36)+A(41)+A(42)+A(43))*f(10))/6._/**/REALKIND &
       +((-A(19)-A(39)-A(40)-A(44))*f(11))/6._/**/REALKIND+((-A(23)-A(45)-A(46))*f(12))/6._/**/REALKIND+((-A(18)-A(53) &
       -A(54))*f(13))/6._/**/REALKIND+((A(16)+A(17)+A(25)+A(26)+A(28)+A(30)+A(33)+A(34)+A(37)+A(38)+A(48)+A(52)+A(56) &
       +A(58))*f(14))/6._/**/REALKIND+((-A(14)-A(20)-A(24))*f(15))/6._/**/REALKIND+((-A(22)-A(51))*f(16))/6._/**/REALKIND
  M2(2) = ((A(65)+A(66)+A(67)+A(68)+A(69)+A(70)+A(71)+A(72)+A(73)+A(81)+A(82)+A(83)+A(84)+A(85)+A(86)+A(87)+A(88)+A(89)+A(90) &
       +A(91)+A(92))*f(4))/2._/**/REALKIND+((-A(59)-A(60)-A(61)-A(62)-A(63)-A(64)-A(74)-A(75)-A(76))*f(5))/2._/**/REALKIND+(( &
       -A(77)-A(78)-A(79)-A(80))*f(6))/2._/**/REALKIND+((-A(47)-A(55)-A(57))*f(7))/2._/**/REALKIND+((A(13) &
       +A(49))*f(8))/2._/**/REALKIND+(A(50)*f(9))/2._/**/REALKIND+((-A(15)-A(21)-A(27)-A(29)-A(31)-A(32)-A(35)-A(36)-A(41)-A(42) &
       -A(43))*f(10))/2._/**/REALKIND+((A(19)+A(39)+A(40)+A(44))*f(11))/2._/**/REALKIND+((A(23)+A(45) &
       +A(46))*f(12))/2._/**/REALKIND+((A(18)+A(53)+A(54))*f(13))/2._/**/REALKIND+((-A(16)-A(17)-A(25)-A(26)-A(28)-A(30)-A(33) &
       -A(34)-A(37)-A(38)-A(48)-A(52)-A(56)-A(58))*f(14))/2._/**/REALKIND+((A(14)+A(20)+A(24))*f(15))/2._/**/REALKIND+((A(22) &
       +A(51))*f(16))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwajj_cxddxsawx_1_/**/REALKIND
