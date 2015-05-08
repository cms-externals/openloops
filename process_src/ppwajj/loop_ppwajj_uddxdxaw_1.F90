
module ol_colourmatrix_ppwajj_uddxdxaw_1_/**/REALKIND
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
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
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
end module ol_colourmatrix_ppwajj_uddxdxaw_1_/**/REALKIND



module ol_forced_parameters_ppwajj_uddxdxaw_1_/**/REALKIND
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
end module ol_forced_parameters_ppwajj_uddxdxaw_1_/**/REALKIND

module ol_loop_ppwajj_uddxdxaw_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(25), c(47)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:248)
  ! denominators
  complex(REALKIND), save :: den(282)
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
    f( 7) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f( 8) = (2*CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f( 9) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f(10) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f(11) = (2*CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(3._/**/REALKIND*sqrt2*sw)
    f(12) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f(13) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(3._/**/REALKIND*sqrt2*sw)
    f(14) = (2*CI*eQED**2*gQCD**4*integralnorm*SwB)/(3._/**/REALKIND*sqrt2*sw)
    f(15) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(16) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*6._/**/REALKIND)
    f(17) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(18) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*2._/**/REALKIND)
    f(19) = (2*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(20) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(21) = (eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw*3._/**/REALKIND)
    f(22) = (2*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw*3._/**/REALKIND)
    f(23) = (eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)
    f(24) = (4*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw*3._/**/REALKIND)
    f(25) = (2*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)

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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(184)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,2))
  call vert_AW_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,2),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,2),wf(:,7))
  call vert_WQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,14),ZERO,0_intkind1,wf(:,9))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,10))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,33),ZERO,0_intkind1,wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,24),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,2),wf(:,12),wf(:,14))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,15))
  call vert_AW_Q(wf(:,13),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_VQ_A(wf(:,-4),wf(:,12),wf(:,18))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,19))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,48),MW,1_intkind1,wf(:,21))
  call vert_AW_Q(wf(:,-3),wf(:,21),wf(:,22))
  call vert_WQ_A(wf(:,21),wf(:,0),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,49),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,25))
  call vert_AW_Q(wf(:,-2),wf(:,-5),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,36),ZERO,0_intkind1,wf(:,27))
  call vert_VQ_A(wf(:,25),wf(:,4),wf(:,28))
  call vert_AV_Q(wf(:,-2),wf(:,25),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,14),ZERO,0_intkind1,wf(:,30))
  call vert_AV_Q(wf(:,-2),wf(:,-4),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,20),ZERO,0_intkind1,wf(:,32))
  call vert_VQ_A(wf(:,25),wf(:,12),wf(:,33))
  call vert_VQ_A(wf(:,25),wf(:,0),wf(:,34))
  call vert_AW_Q(wf(:,32),wf(:,-5),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,11),ZERO,0_intkind1,wf(:,36))
  call vert_AV_Q(wf(:,27),wf(:,-4),wf(:,37))
  call vert_AW_Q(wf(:,-2),wf(:,21),wf(:,38))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,18),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,12),wf(:,-2),wf(:,41))
  call vert_QA_V(wf(:,40),wf(:,-3),wf(:,42))
  call vert_QA_V(wf(:,40),wf(:,-2),wf(:,43))
  call vert_QA_V(wf(:,12),wf(:,-3),wf(:,44))
  call vert_QA_V(wf(:,0),wf(:,27),wf(:,45))
  call vert_QA_V(wf(:,0),wf(:,5),wf(:,46))
  call vert_QA_V(wf(:,-1),wf(:,32),wf(:,47))
  call vert_QA_V(wf(:,-1),wf(:,13),wf(:,48))
  call counter_VQ_A(wf(:,2),wf(:,4),wf(:,49))
  call counter_WQ_A(wf(:,-5),wf(:,4),wf(:,50))
  call counter_VQ_A(wf(:,2),wf(:,12),wf(:,51))
  call counter_AW_Q(wf(:,13),wf(:,-5),wf(:,52))
  call counter_VQ_A(wf(:,-4),wf(:,12),wf(:,53))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,54))
  call counter_AV_Q(wf(:,-3),wf(:,2),wf(:,55))
  call prop_Q_A(wf(:,8),Q(:,49),ZERO,0_intkind1,wf(:,56))
  call counter_AW_Q(wf(:,-3),wf(:,-5),wf(:,57))
  call prop_A_Q(wf(:,57),Q(:,40),ZERO,0_intkind1,wf(:,58))
  call prop_Q_A(wf(:,18),Q(:,49),ZERO,0_intkind1,wf(:,59))
  call counter_AW_Q(wf(:,-3),wf(:,21),wf(:,60))
  call vert_AV_Q(wf(:,58),wf(:,-4),wf(:,61))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,62))
  call prop_A_Q(wf(:,62),Q(:,24),ZERO,0_intkind1,wf(:,63))
  call vert_AW_Q(wf(:,63),wf(:,-5),wf(:,64))
  call counter_VQ_A(wf(:,25),wf(:,4),wf(:,65))
  call counter_VQ_A(wf(:,25),wf(:,12),wf(:,66))
  call counter_AW_Q(wf(:,32),wf(:,-5),wf(:,67))
  call counter_AV_Q(wf(:,27),wf(:,-4),wf(:,68))
  call counter_QA_V(wf(:,40),wf(:,-3),wf(:,69))
  call counter_QA_V(wf(:,12),wf(:,-3),wf(:,70))
  call vert_QA_V(wf(:,0),wf(:,58),wf(:,71))
  call vert_QA_V(wf(:,-1),wf(:,63),wf(:,72))
  call counter_AV_Q(wf(:,-2),wf(:,25),wf(:,73))
  call counter_AW_Q(wf(:,-2),wf(:,-5),wf(:,74))
  call prop_A_Q(wf(:,74),Q(:,36),ZERO,0_intkind1,wf(:,75))
  call counter_AW_Q(wf(:,-2),wf(:,21),wf(:,76))
  call vert_AV_Q(wf(:,75),wf(:,-4),wf(:,77))
  call counter_AV_Q(wf(:,-2),wf(:,-4),wf(:,78))
  call prop_A_Q(wf(:,78),Q(:,20),ZERO,0_intkind1,wf(:,79))
  call vert_AW_Q(wf(:,79),wf(:,-5),wf(:,80))
  call counter_QA_V(wf(:,40),wf(:,-2),wf(:,81))
  call counter_QA_V(wf(:,12),wf(:,-2),wf(:,82))
  call vert_QA_V(wf(:,0),wf(:,75),wf(:,83))
  call vert_QA_V(wf(:,-1),wf(:,79),wf(:,84))
  call counter_QA_V(wf(:,-1),wf(:,32),wf(:,85))
  call counter_QA_V(wf(:,-1),wf(:,13),wf(:,86))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,87))
  call prop_Q_A(wf(:,87),Q(:,18),ZERO,0_intkind1,wf(:,88))
  call vert_QA_V(wf(:,88),wf(:,-3),wf(:,89))
  call vert_QA_V(wf(:,88),wf(:,-2),wf(:,90))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,91))
  call vert_VQ_A(wf(:,91),wf(:,4),wf(:,92))
  call vert_AV_Q(wf(:,-2),wf(:,91),wf(:,93))
  call prop_A_Q(wf(:,93),Q(:,14),ZERO,0_intkind1,wf(:,94))
  call vert_VQ_A(wf(:,91),wf(:,12),wf(:,95))
  call vert_VQ_A(wf(:,91),wf(:,0),wf(:,96))
  call prop_Q_A(wf(:,96),Q(:,11),ZERO,0_intkind1,wf(:,97))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,98))
  call vert_VQ_A(wf(:,98),wf(:,4),wf(:,99))
  call vert_AV_Q(wf(:,-3),wf(:,98),wf(:,100))
  call prop_A_Q(wf(:,100),Q(:,14),ZERO,0_intkind1,wf(:,101))
  call vert_VQ_A(wf(:,98),wf(:,12),wf(:,102))
  call vert_VQ_A(wf(:,98),wf(:,0),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,7),ZERO,0_intkind1,wf(:,104))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,105))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,106))
  call counter_WQ_A(wf(:,-5),wf(:,0),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,33),ZERO,0_intkind1,wf(:,108))
  call vert_VQ_A(wf(:,2),wf(:,108),wf(:,109))
  call prop_A_Q(wf(:,19),Q(:,56),ZERO,0_intkind1,wf(:,110))
  call prop_A_Q(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,111))
  call counter_WQ_A(wf(:,21),wf(:,0),wf(:,112))
  call vert_VQ_A(wf(:,-4),wf(:,108),wf(:,113))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,114))
  call prop_Q_A(wf(:,114),Q(:,17),ZERO,0_intkind1,wf(:,115))
  call vert_VQ_A(wf(:,2),wf(:,115),wf(:,116))
  call vert_WQ_A(wf(:,-5),wf(:,115),wf(:,117))
  call counter_VQ_A(wf(:,25),wf(:,0),wf(:,118))
  call prop_A_Q(wf(:,35),Q(:,52),ZERO,0_intkind1,wf(:,119))
  call vert_VQ_A(wf(:,25),wf(:,108),wf(:,120))
  call prop_A_Q(wf(:,37),Q(:,52),ZERO,0_intkind1,wf(:,121))
  call prop_A_Q(wf(:,38),Q(:,52),ZERO,0_intkind1,wf(:,122))
  call vert_VQ_A(wf(:,25),wf(:,115),wf(:,123))
  call counter_QA_V(wf(:,0),wf(:,27),wf(:,124))
  call counter_QA_V(wf(:,0),wf(:,5),wf(:,125))
  call vert_QA_V(wf(:,108),wf(:,-2),wf(:,126))
  call vert_QA_V(wf(:,108),wf(:,-3),wf(:,127))
  call vert_AV_Q(wf(:,5),wf(:,2),wf(:,128))
  call counter_Q_A(ctqq,wf(:,4),Q(:,17),wf(:,129))
  call prop_A_Q(wf(:,128),Q(:,46),ZERO,0_intkind1,wf(:,130))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,131))
  call counter_V_V(ctGG,wf(:,2),Q(:,6),wf(:,132))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,133))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,134))
  call prop_Q_A(wf(:,129),Q(:,17),ZERO,0_intkind1,wf(:,135))
  call vert_WQ_A(wf(:,-5),wf(:,135),wf(:,136))
  call vert_AV_Q(wf(:,-3),wf(:,132),wf(:,137))
  call counter_A_Q(ctqq,wf(:,9),Q(:,14),wf(:,138))
  call vert_AV_Q(wf(:,13),wf(:,2),wf(:,139))
  call counter_Q_A(ctqq,wf(:,12),Q(:,33),wf(:,140))
  call prop_A_Q(wf(:,139),Q(:,30),ZERO,0_intkind1,wf(:,141))
  call vert_QA_V(wf(:,12),wf(:,13),wf(:,142))
  call counter_A_Q(ctqq,wf(:,13),Q(:,24),wf(:,143))
  call prop_Q_A(wf(:,14),Q(:,39),ZERO,0_intkind1,wf(:,144))
  call vert_VQ_A(wf(:,132),wf(:,0),wf(:,145))
  call counter_Q_A(ctqq,wf(:,17),Q(:,7),wf(:,146))
  call prop_A_Q(wf(:,143),Q(:,24),ZERO,0_intkind1,wf(:,147))
  call vert_AW_Q(wf(:,147),wf(:,-5),wf(:,148))
  call prop_Q_A(wf(:,140),Q(:,33),ZERO,0_intkind1,wf(:,149))
  call vert_VQ_A(wf(:,-4),wf(:,149),wf(:,150))
  call prop_A_Q(wf(:,133),Q(:,40),ZERO,0_intkind1,wf(:,151))
  call vert_AV_Q(wf(:,151),wf(:,-4),wf(:,152))
  call counter_Q_A(ctqq,wf(:,24),Q(:,49),wf(:,153))
  call vert_AV_Q(wf(:,27),wf(:,25),wf(:,154))
  call prop_A_Q(wf(:,154),Q(:,46),ZERO,0_intkind1,wf(:,155))
  call vert_QA_V(wf(:,4),wf(:,27),wf(:,156))
  call counter_V_V(ctGG,wf(:,25),Q(:,10),wf(:,157))
  call counter_A_Q(ctqq,wf(:,27),Q(:,36),wf(:,158))
  call prop_Q_A(wf(:,28),Q(:,27),ZERO,0_intkind1,wf(:,159))
  call vert_AV_Q(wf(:,-2),wf(:,157),wf(:,160))
  call counter_A_Q(ctqq,wf(:,30),Q(:,14),wf(:,161))
  call vert_AV_Q(wf(:,32),wf(:,25),wf(:,162))
  call prop_A_Q(wf(:,162),Q(:,30),ZERO,0_intkind1,wf(:,163))
  call vert_QA_V(wf(:,12),wf(:,32),wf(:,164))
  call counter_A_Q(ctqq,wf(:,32),Q(:,20),wf(:,165))
  call prop_Q_A(wf(:,33),Q(:,43),ZERO,0_intkind1,wf(:,166))
  call vert_VQ_A(wf(:,157),wf(:,0),wf(:,167))
  call counter_Q_A(ctqq,wf(:,36),Q(:,11),wf(:,168))
  call prop_A_Q(wf(:,165),Q(:,20),ZERO,0_intkind1,wf(:,169))
  call vert_AW_Q(wf(:,169),wf(:,-5),wf(:,170))
  call prop_A_Q(wf(:,158),Q(:,36),ZERO,0_intkind1,wf(:,171))
  call vert_AV_Q(wf(:,171),wf(:,-4),wf(:,172))
  call vert_QA_V(wf(:,149),wf(:,-2),wf(:,173))
  call vert_QA_V(wf(:,149),wf(:,-3),wf(:,174))
  call counter_Q_A(ctqq,wf(:,40),Q(:,18),wf(:,175))
  call prop_Q_A(wf(:,175),Q(:,18),ZERO,0_intkind1,wf(:,176))
  call vert_QA_V(wf(:,176),wf(:,-2),wf(:,177))
  call vert_QA_V(wf(:,176),wf(:,-3),wf(:,178))
  call counter_V_V(ctGG,wf(:,41),Q(:,37),wf(:,179))
  call counter_V_V(ctGG,wf(:,43),Q(:,22),wf(:,180))
  call vert_QA_V(wf(:,0),wf(:,171),wf(:,181))
  call counter_V_V(ctGG,wf(:,45),Q(:,37),wf(:,182))
  call counter_V_V(ctGG,wf(:,46),Q(:,41),wf(:,183))
  call vert_QA_V(wf(:,0),wf(:,151),wf(:,184))
  call vert_QA_V(wf(:,-1),wf(:,169),wf(:,185))
  call counter_V_V(ctGG,wf(:,47),Q(:,22),wf(:,186))
  call counter_V_V(ctGG,wf(:,48),Q(:,26),wf(:,187))
  call vert_QA_V(wf(:,-1),wf(:,147),wf(:,188))
  call vert_VQ_A(wf(:,-4),wf(:,17),wf(:,189))
  call prop_Q_A(wf(:,189),Q(:,23),ZERO,0_intkind1,wf(:,190))
  call vert_WQ_A(wf(:,-5),wf(:,17),wf(:,191))
  call prop_Q_A(wf(:,191),Q(:,39),ZERO,0_intkind1,wf(:,192))
  call vert_AV_Q(wf(:,9),wf(:,-4),wf(:,193))
  call prop_A_Q(wf(:,193),Q(:,30),ZERO,0_intkind1,wf(:,194))
  call vert_AW_Q(wf(:,9),wf(:,-5),wf(:,195))
  call prop_A_Q(wf(:,195),Q(:,46),ZERO,0_intkind1,wf(:,196))
  call vert_VQ_A(wf(:,-4),wf(:,36),wf(:,197))
  call prop_Q_A(wf(:,197),Q(:,27),ZERO,0_intkind1,wf(:,198))
  call vert_WQ_A(wf(:,-5),wf(:,36),wf(:,199))
  call prop_Q_A(wf(:,199),Q(:,43),ZERO,0_intkind1,wf(:,200))
  call vert_AV_Q(wf(:,30),wf(:,-4),wf(:,201))
  call prop_A_Q(wf(:,201),Q(:,30),ZERO,0_intkind1,wf(:,202))
  call vert_AW_Q(wf(:,30),wf(:,-5),wf(:,203))
  call prop_A_Q(wf(:,203),Q(:,46),ZERO,0_intkind1,wf(:,204))
  call vert_QA_V(wf(:,56),wf(:,-2),wf(:,205))
  call vert_QA_V(wf(:,56),wf(:,-3),wf(:,206))
  call vert_VQ_A(wf(:,43),wf(:,0),wf(:,207))
  call prop_Q_A(wf(:,207),Q(:,23),ZERO,0_intkind1,wf(:,208))
  call vert_VQ_A(wf(:,42),wf(:,0),wf(:,209))
  call prop_Q_A(wf(:,209),Q(:,27),ZERO,0_intkind1,wf(:,210))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,211))
  call prop_A_Q(wf(:,211),Q(:,30),ZERO,0_intkind1,wf(:,212))
  call vert_AV_Q(wf(:,-2),wf(:,42),wf(:,213))
  call prop_A_Q(wf(:,213),Q(:,30),ZERO,0_intkind1,wf(:,214))
  call vert_VQ_A(wf(:,47),wf(:,0),wf(:,215))
  call prop_Q_A(wf(:,215),Q(:,23),ZERO,0_intkind1,wf(:,216))
  call vert_QA_V(wf(:,0),wf(:,119),wf(:,217))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,218))
  call prop_A_Q(wf(:,218),Q(:,30),ZERO,0_intkind1,wf(:,219))
  call vert_VQ_A(wf(:,48),wf(:,0),wf(:,220))
  call prop_Q_A(wf(:,220),Q(:,27),ZERO,0_intkind1,wf(:,221))
  call vert_QA_V(wf(:,0),wf(:,106),wf(:,222))
  call vert_AV_Q(wf(:,-2),wf(:,48),wf(:,223))
  call prop_A_Q(wf(:,223),Q(:,30),ZERO,0_intkind1,wf(:,224))
  call vert_VQ_A(wf(:,41),wf(:,-1),wf(:,225))
  call prop_Q_A(wf(:,225),Q(:,39),ZERO,0_intkind1,wf(:,226))
  call vert_AV_Q(wf(:,-3),wf(:,41),wf(:,227))
  call prop_A_Q(wf(:,227),Q(:,45),ZERO,0_intkind1,wf(:,228))
  call vert_VQ_A(wf(:,44),wf(:,-1),wf(:,229))
  call prop_Q_A(wf(:,229),Q(:,43),ZERO,0_intkind1,wf(:,230))
  call vert_AV_Q(wf(:,-2),wf(:,44),wf(:,231))
  call prop_A_Q(wf(:,231),Q(:,45),ZERO,0_intkind1,wf(:,232))
  call vert_QA_V(wf(:,59),wf(:,-2),wf(:,233))
  call vert_QA_V(wf(:,59),wf(:,-3),wf(:,234))
  call vert_VQ_A(wf(:,45),wf(:,-1),wf(:,235))
  call prop_Q_A(wf(:,235),Q(:,39),ZERO,0_intkind1,wf(:,236))
  call vert_AV_Q(wf(:,-3),wf(:,45),wf(:,237))
  call prop_A_Q(wf(:,237),Q(:,45),ZERO,0_intkind1,wf(:,238))
  call vert_QA_V(wf(:,0),wf(:,121),wf(:,239))
  call vert_VQ_A(wf(:,46),wf(:,-1),wf(:,240))
  call prop_Q_A(wf(:,240),Q(:,43),ZERO,0_intkind1,wf(:,241))
  call vert_AV_Q(wf(:,-2),wf(:,46),wf(:,242))
  call prop_A_Q(wf(:,242),Q(:,45),ZERO,0_intkind1,wf(:,243))
  call vert_QA_V(wf(:,0),wf(:,110),wf(:,244))
  call vert_QA_V(wf(:,24),wf(:,-2),wf(:,245))
  call vert_QA_V(wf(:,0),wf(:,122),wf(:,246))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,247))
  call vert_QA_V(wf(:,0),wf(:,111),wf(:,248))

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
  den(23) = 1 / (Q(5,10))
  den(24) = 1 / (Q(5,36))
  den(29) = 1 / (Q(5,20))
  den(32) = 1 / (Q(5,11))
  den(39) = 1 / (Q(5,18))
  den(40) = 1 / (Q(5,37))
  den(43) = 1 / (Q(5,22))
  den(48) = 1 / (Q(5,41))
  den(54) = 1 / (Q(5,26))
  den(70) = 1 / (Q(5,56))
  den(78) = 1 / (Q(5,52))
  den(91) = 1 / (Q(5,46))
  den(95) = 1 / (Q(5,57))
  den(98) = 1 / (Q(5,23))
  den(107) = 1 / (Q(5,30))
  den(113) = 1 / (Q(5,39))
  den(136) = 1 / (Q(5,53))
  den(139) = 1 / (Q(5,27))
  den(152) = 1 / (Q(5,43))
  den(216) = 1 / (Q(5,45))

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
  den(25) = den(1)*den(23)
  den(26) = den(24)*den(25)
  den(27) = den(6)*den(23)
  den(28) = den(1)*den(27)
  den(30) = den(9)*den(23)
  den(31) = den(29)*den(30)
  den(33) = den(23)*den(32)
  den(34) = den(29)*den(33)
  den(35) = den(9)*den(27)
  den(36) = den(24)*den(33)
  den(37) = den(18)*den(33)
  den(38) = den(21)*den(23)
  den(41) = den(9)*den(40)
  den(42) = den(39)*den(41)
  den(44) = den(39)*den(43)
  den(45) = den(9)*den(44)
  den(46) = den(24)*den(40)
  den(47) = den(39)*den(46)
  den(49) = den(3)*den(48)
  den(50) = den(39)*den(49)
  den(51) = den(29)*den(43)
  den(52) = den(9)*den(51)
  den(53) = den(29)*den(49)
  den(55) = den(10)*den(54)
  den(56) = den(9)*den(55)
  den(57) = den(10)*den(46)
  den(58) = den(1)*den(20)
  den(59) = den(2)*den(58)
  den(60) = den(9)*den(20)
  den(61) = den(2)*den(60)
  den(62) = den(23)*den(58)
  den(63) = den(23)*den(60)
  den(64) = den(9)*den(48)
  den(65) = den(39)*den(64)
  den(66) = den(39)*den(54)
  den(67) = den(9)*den(66)
  den(68) = den(29)*den(64)
  den(69) = den(10)*den(41)
  den(71) = den(10)*den(70)
  den(72) = den(2)*den(71)
  den(73) = den(3)*den(70)
  den(74) = den(2)*den(73)
  den(75) = den(18)*den(70)
  den(76) = den(2)*den(75)
  den(77) = den(7)*den(18)
  den(79) = den(29)*den(78)
  den(80) = den(23)*den(79)
  den(81) = den(24)*den(78)
  den(82) = den(23)*den(81)
  den(83) = den(18)*den(78)
  den(84) = den(23)*den(83)
  den(85) = den(18)*den(27)
  den(86) = den(24)*den(66)
  den(87) = den(3)*den(44)
  den(88) = den(3)*den(51)
  den(89) = den(24)*den(55)
  den(90) = den(2)*den(3)
  den(92) = den(90)*den(91)
  den(93) = den(1)*den(92)
  den(94) = den(1)*den(3)
  den(96) = den(94)*den(95)
  den(97) = den(2)*den(96)
  den(99) = den(4)*den(98)
  den(100) = den(3)*den(99)
  den(101) = den(1)**2
  den(102) = den(7)*den(101)
  den(103) = den(2)**2
  den(104) = den(58)*den(103)
  den(105) = den(7)*den(58)
  den(106) = den(2)*den(10)
  den(108) = den(106)*den(107)
  den(109) = den(9)*den(108)
  den(110) = den(9)*den(10)
  den(111) = den(95)*den(110)
  den(112) = den(2)*den(111)
  den(114) = den(11)*den(113)
  den(115) = den(10)*den(114)
  den(116) = den(71)*den(103)
  den(117) = den(14)*den(71)
  den(118) = den(10)**2
  den(119) = den(14)*den(118)
  den(120) = den(9)**2
  den(121) = den(7)*den(120)
  den(122) = den(60)*den(103)
  den(123) = den(7)*den(60)
  den(124) = den(73)*den(103)
  den(125) = den(14)*den(73)
  den(126) = den(3)**2
  den(127) = den(14)*den(126)
  den(128) = den(75)*den(103)
  den(129) = den(14)*den(75)
  den(130) = den(7)*den(21)
  den(131) = den(21)*den(103)
  den(132) = den(23)*den(24)
  den(133) = den(91)*den(132)
  den(134) = den(1)*den(133)
  den(135) = den(1)*den(24)
  den(137) = den(135)*den(136)
  den(138) = den(23)*den(137)
  den(140) = den(25)*den(139)
  den(141) = den(24)*den(140)
  den(142) = den(27)*den(101)
  den(143) = den(23)**2
  den(144) = den(58)*den(143)
  den(145) = den(27)*den(58)
  den(146) = den(23)*den(29)
  den(147) = den(107)*den(146)
  den(148) = den(9)*den(147)
  den(149) = den(9)*den(29)
  den(150) = den(136)*den(149)
  den(151) = den(23)*den(150)
  den(153) = den(30)*den(152)
  den(154) = den(29)*den(153)
  den(155) = den(79)*den(143)
  den(156) = den(33)*den(79)
  den(157) = den(29)**2
  den(158) = den(33)*den(157)
  den(159) = den(27)*den(120)
  den(160) = den(60)*den(143)
  den(161) = den(27)*den(60)
  den(162) = den(81)*den(143)
  den(163) = den(33)*den(81)
  den(164) = den(24)**2
  den(165) = den(33)*den(164)
  den(166) = den(83)*den(143)
  den(167) = den(33)*den(83)
  den(168) = den(21)*den(27)
  den(169) = den(21)*den(143)
  den(170) = den(66)*den(120)
  den(171) = den(44)*den(120)
  den(172) = den(39)**2
  den(173) = den(64)*den(172)
  den(174) = den(41)*den(172)
  den(175) = den(41)*den(66)
  den(176) = den(44)*den(64)
  den(177) = den(66)*den(164)
  den(178) = den(46)*den(66)
  den(179) = den(46)*den(172)
  den(180) = den(44)*den(49)
  den(181) = den(44)*den(126)
  den(182) = den(49)*den(172)
  den(183) = den(51)*den(120)
  den(184) = den(64)*den(157)
  den(185) = den(51)*den(64)
  den(186) = den(49)*den(51)
  den(187) = den(51)*den(126)
  den(188) = den(49)*den(157)
  den(189) = den(55)*den(120)
  den(190) = den(41)*den(55)
  den(191) = den(41)*den(118)
  den(192) = den(46)*den(55)
  den(193) = den(55)*den(164)
  den(194) = den(46)*den(118)
  den(195) = den(14)*den(98)
  den(196) = den(14)*den(113)
  den(197) = den(7)*den(107)
  den(198) = den(7)*den(91)
  den(199) = den(33)*den(139)
  den(200) = den(33)*den(152)
  den(201) = den(27)*den(107)
  den(202) = den(27)*den(91)
  den(203) = den(58)*den(136)
  den(204) = den(58)*den(95)
  den(205) = den(44)*den(98)
  den(206) = den(66)*den(139)
  den(207) = den(44)*den(107)
  den(208) = den(66)*den(107)
  den(209) = den(51)*den(98)
  den(210) = den(79)*den(136)
  den(211) = den(51)*den(107)
  den(212) = den(55)*den(139)
  den(213) = den(71)*den(95)
  den(214) = den(55)*den(107)
  den(215) = den(41)*den(113)
  den(217) = den(41)*den(216)
  den(218) = den(64)*den(152)
  den(219) = den(64)*den(216)
  den(220) = den(60)*den(136)
  den(221) = den(60)*den(95)
  den(222) = den(46)*den(113)
  den(223) = den(46)*den(216)
  den(224) = den(81)*den(136)
  den(225) = den(49)*den(152)
  den(226) = den(49)*den(216)
  den(227) = den(73)*den(95)
  den(228) = den(21)*den(136)
  den(229) = den(83)*den(136)
  den(230) = den(21)*den(95)
  den(231) = den(75)*den(95)
  den(232) = den(1)*den(2)*den(3)
  den(233) = den(2)*den(9)*den(10)
  den(234) = den(2)*den(64)
  den(235) = den(2)*den(49)
  den(236) = den(2)*den(18)
  den(237) = den(1)*den(23)*den(24)
  den(238) = den(9)*den(23)*den(29)
  den(239) = den(23)*den(41)
  den(240) = den(23)*den(46)
  den(241) = den(18)*den(23)
  den(242) = den(9)*den(39)
  den(243) = den(24)*den(39)
  den(244) = den(3)*den(39)
  den(245) = den(3)*den(29)
  den(246) = den(10)*den(24)
  den(247) = den(2)*den(204)
  den(248) = den(1)*den(198)
  den(249) = den(10)*den(196)
  den(250) = den(2)*den(213)
  den(251) = den(2)*den(221)
  den(252) = den(9)*den(197)
  den(253) = den(3)*den(195)
  den(254) = den(2)*den(227)
  den(255) = den(2)*den(230)
  den(256) = den(2)*den(231)
  den(257) = den(23)*den(203)
  den(258) = den(1)*den(202)
  den(259) = den(29)*den(200)
  den(260) = den(23)*den(210)
  den(261) = den(23)*den(220)
  den(262) = den(9)*den(201)
  den(263) = den(24)*den(199)
  den(264) = den(23)*den(224)
  den(265) = den(23)*den(228)
  den(266) = den(23)*den(229)
  den(267) = den(39)*den(217)
  den(268) = den(39)*den(219)
  den(269) = den(9)*den(207)
  den(270) = den(9)*den(208)
  den(271) = den(39)*den(223)
  den(272) = den(24)*den(206)
  den(273) = den(39)*den(226)
  den(274) = den(3)*den(205)
  den(275) = den(29)*den(218)
  den(276) = den(9)*den(211)
  den(277) = den(29)*den(225)
  den(278) = den(3)*den(209)
  den(279) = den(10)*den(215)
  den(280) = den(9)*den(214)
  den(281) = den(10)*den(222)
  den(282) = den(24)*den(212)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(184)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,17),wf(:,19)) * den(17)
  A(7) = cont_QA(wf(:,17),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,7),wf(:,24)) * den(22)
  A(9) = cont_QA(wf(:,27),wf(:,28)) * den(26)
  A(10) = cont_QA(wf(:,8),wf(:,30)) * den(28)
  A(11) = cont_QA(wf(:,32),wf(:,33)) * den(31)
  A(12) = cont_QA(wf(:,35),wf(:,36)) * den(34)
  A(13) = cont_QA(wf(:,18),wf(:,30)) * den(35)
  A(14) = cont_QA(wf(:,36),wf(:,37)) * den(36)
  A(15) = cont_QA(wf(:,36),wf(:,38)) * den(37)
  A(16) = cont_QA(wf(:,24),wf(:,29)) * den(38)
  A(17) = cont_VV(wf(:,41),wf(:,42)) * den(42)
  A(18) = cont_VV(wf(:,43),wf(:,44)) * den(45)
  A(19) = cont_VV(wf(:,42),wf(:,45)) * den(47)
  A(20) = cont_VV(wf(:,43),wf(:,46)) * den(50)
  A(21) = cont_VV(wf(:,44),wf(:,47)) * den(52)
  A(22) = cont_VV(wf(:,46),wf(:,47)) * den(53)
  A(23) = cont_VV(wf(:,41),wf(:,48)) * den(56)
  A(24) = cont_VV(wf(:,45),wf(:,48)) * den(57)

  A(25) = cont_QA(wf(:,5),wf(:,49)) * den(5)
  A(26) = cont_QA(wf(:,9),wf(:,50)) * den(8)
  A(27) = cont_QA(wf(:,13),wf(:,51)) * den(12)
  A(28) = cont_QA(wf(:,17),wf(:,52)) * den(15)
  A(29) = cont_QA(wf(:,9),wf(:,53)) * den(16)
  A(30) = cont_QA(wf(:,17),wf(:,54)) * den(17)
  A(31) = cont_QA(wf(:,55),wf(:,56)) * den(59)
  A(32) = cont_QA(wf(:,6),wf(:,58)) * den(5)
  A(33) = cont_QA(wf(:,55),wf(:,59)) * den(61)
  A(34) = cont_QA(wf(:,17),wf(:,60)) * den(19)
  A(35) = cont_QA(wf(:,24),wf(:,55)) * den(22)
  A(36) = cont_QA(wf(:,17),wf(:,61)) * den(17)
  A(37) = cont_QA(wf(:,14),wf(:,63)) * den(12)
  A(38) = cont_QA(wf(:,17),wf(:,64)) * den(15)
  A(39) = cont_QA(wf(:,27),wf(:,65)) * den(26)
  A(40) = cont_QA(wf(:,30),wf(:,50)) * den(28)
  A(41) = cont_QA(wf(:,32),wf(:,66)) * den(31)
  A(42) = cont_QA(wf(:,36),wf(:,67)) * den(34)
  A(43) = cont_QA(wf(:,30),wf(:,53)) * den(35)
  A(44) = cont_QA(wf(:,36),wf(:,68)) * den(36)
  A(45) = cont_VV(wf(:,41),wf(:,69)) * den(42)
  A(46) = cont_VV(wf(:,43),wf(:,70)) * den(45)
  A(47) = cont_VV(wf(:,45),wf(:,69)) * den(47)
  A(48) = cont_VV(wf(:,43),wf(:,71)) * den(50)
  A(49) = cont_VV(wf(:,47),wf(:,70)) * den(52)
  A(50) = cont_VV(wf(:,47),wf(:,71)) * den(53)
  A(51) = cont_VV(wf(:,41),wf(:,72)) * den(56)
  A(52) = cont_VV(wf(:,45),wf(:,72)) * den(57)
  A(53) = cont_QA(wf(:,56),wf(:,73)) * den(62)
  A(54) = cont_QA(wf(:,28),wf(:,75)) * den(26)
  A(55) = cont_QA(wf(:,59),wf(:,73)) * den(63)
  A(56) = cont_QA(wf(:,36),wf(:,76)) * den(37)
  A(57) = cont_QA(wf(:,24),wf(:,73)) * den(38)
  A(58) = cont_QA(wf(:,36),wf(:,77)) * den(36)
  A(59) = cont_QA(wf(:,33),wf(:,79)) * den(31)
  A(60) = cont_QA(wf(:,36),wf(:,80)) * den(34)
  A(61) = cont_VV(wf(:,44),wf(:,81)) * den(65)
  A(62) = cont_VV(wf(:,42),wf(:,82)) * den(67)
  A(63) = cont_VV(wf(:,46),wf(:,81)) * den(50)
  A(64) = cont_VV(wf(:,42),wf(:,83)) * den(47)
  A(65) = cont_VV(wf(:,48),wf(:,82)) * den(56)
  A(66) = cont_VV(wf(:,48),wf(:,83)) * den(57)
  A(67) = cont_VV(wf(:,44),wf(:,84)) * den(52)
  A(68) = cont_VV(wf(:,46),wf(:,84)) * den(53)
  A(69) = cont_VV(wf(:,44),wf(:,85)) * den(68)
  A(70) = cont_VV(wf(:,46),wf(:,85)) * den(53)
  A(71) = cont_VV(wf(:,41),wf(:,86)) * den(69)
  A(72) = cont_VV(wf(:,45),wf(:,86)) * den(57)
  A(73) = cont_VV(wf(:,41),wf(:,89)) * den(42)
  A(74) = cont_VV(wf(:,44),wf(:,90)) * den(45)
  A(75) = cont_VV(wf(:,45),wf(:,89)) * den(47)
  A(76) = cont_VV(wf(:,46),wf(:,90)) * den(50)
  A(77) = cont_QA(wf(:,27),wf(:,92)) * den(26)
  A(78) = cont_QA(wf(:,8),wf(:,94)) * den(28)
  A(79) = cont_QA(wf(:,32),wf(:,95)) * den(31)
  A(80) = cont_QA(wf(:,35),wf(:,97)) * den(34)
  A(81) = cont_QA(wf(:,18),wf(:,94)) * den(35)
  A(82) = cont_QA(wf(:,37),wf(:,97)) * den(36)
  A(83) = cont_QA(wf(:,24),wf(:,93)) * den(38)
  A(84) = cont_QA(wf(:,38),wf(:,97)) * den(37)
  A(85) = cont_QA(wf(:,5),wf(:,99)) * den(5)
  A(86) = cont_QA(wf(:,8),wf(:,101)) * den(8)
  A(87) = cont_QA(wf(:,13),wf(:,102)) * den(12)
  A(88) = cont_QA(wf(:,16),wf(:,104)) * den(15)
  A(89) = cont_QA(wf(:,18),wf(:,101)) * den(16)
  A(90) = cont_QA(wf(:,19),wf(:,104)) * den(17)
  A(91) = cont_QA(wf(:,24),wf(:,100)) * den(22)
  A(92) = cont_QA(wf(:,22),wf(:,104)) * den(19)
  A(93) = cont_QA(wf(:,105),wf(:,106)) * den(72)
  A(94) = cont_QA(wf(:,13),wf(:,109)) * den(12)
  A(95) = cont_QA(wf(:,105),wf(:,110)) * den(74)
  A(96) = cont_QA(wf(:,105),wf(:,111)) * den(76)
  A(97) = cont_QA(wf(:,9),wf(:,112)) * den(77)
  A(98) = cont_QA(wf(:,9),wf(:,113)) * den(16)
  A(99) = cont_QA(wf(:,5),wf(:,116)) * den(5)
  A(100) = cont_QA(wf(:,9),wf(:,117)) * den(8)
  A(101) = cont_QA(wf(:,118),wf(:,119)) * den(80)
  A(102) = cont_QA(wf(:,32),wf(:,120)) * den(31)
  A(103) = cont_QA(wf(:,118),wf(:,121)) * den(82)
  A(104) = cont_QA(wf(:,118),wf(:,122)) * den(84)
  A(105) = cont_QA(wf(:,30),wf(:,112)) * den(85)
  A(106) = cont_QA(wf(:,30),wf(:,113)) * den(35)
  A(107) = cont_QA(wf(:,27),wf(:,123)) * den(26)
  A(108) = cont_QA(wf(:,30),wf(:,117)) * den(28)
  A(109) = cont_VV(wf(:,42),wf(:,124)) * den(86)
  A(110) = cont_VV(wf(:,43),wf(:,125)) * den(87)
  A(111) = cont_VV(wf(:,42),wf(:,126)) * den(42)
  A(112) = cont_VV(wf(:,43),wf(:,127)) * den(45)
  A(113) = cont_VV(wf(:,47),wf(:,125)) * den(88)
  A(114) = cont_VV(wf(:,47),wf(:,127)) * den(52)
  A(115) = cont_VV(wf(:,48),wf(:,124)) * den(89)
  A(116) = cont_VV(wf(:,48),wf(:,126)) * den(56)
  A(117) = cont_QA(wf(:,129),wf(:,130)) * den(93)
  A(118) = cont_VV(wf(:,131),wf(:,132)) * den(97)
  A(119) = cont_QA(wf(:,133),wf(:,134)) * den(100)
  A(120) = cont_QA(wf(:,9),wf(:,136)) * den(102)
  A(121) = cont_QA(wf(:,56),wf(:,137)) * den(104)
  A(122) = cont_QA(wf(:,56),wf(:,138)) * den(105)
  A(123) = cont_QA(wf(:,140),wf(:,141)) * den(109)
  A(124) = cont_VV(wf(:,132),wf(:,142)) * den(112)
  A(125) = cont_QA(wf(:,143),wf(:,144)) * den(115)
  A(126) = cont_QA(wf(:,106),wf(:,145)) * den(116)
  A(127) = cont_QA(wf(:,106),wf(:,146)) * den(117)
  A(128) = cont_QA(wf(:,17),wf(:,148)) * den(119)
  A(129) = cont_QA(wf(:,9),wf(:,150)) * den(121)
  A(130) = cont_QA(wf(:,59),wf(:,137)) * den(122)
  A(131) = cont_QA(wf(:,59),wf(:,138)) * den(123)
  A(132) = cont_QA(wf(:,110),wf(:,145)) * den(124)
  A(133) = cont_QA(wf(:,110),wf(:,146)) * den(125)
  A(134) = cont_QA(wf(:,17),wf(:,152)) * den(127)
  A(135) = cont_QA(wf(:,111),wf(:,145)) * den(128)
  A(136) = cont_QA(wf(:,111),wf(:,146)) * den(129)
  A(137) = cont_QA(wf(:,9),wf(:,153)) * den(130)
  A(138) = cont_QA(wf(:,24),wf(:,137)) * den(131)
  A(139) = cont_QA(wf(:,129),wf(:,155)) * den(134)
  A(140) = cont_VV(wf(:,156),wf(:,157)) * den(138)
  A(141) = cont_QA(wf(:,158),wf(:,159)) * den(141)
  A(142) = cont_QA(wf(:,30),wf(:,136)) * den(142)
  A(143) = cont_QA(wf(:,56),wf(:,160)) * den(144)
  A(144) = cont_QA(wf(:,56),wf(:,161)) * den(145)
  A(145) = cont_QA(wf(:,140),wf(:,163)) * den(148)
  A(146) = cont_VV(wf(:,157),wf(:,164)) * den(151)
  A(147) = cont_QA(wf(:,165),wf(:,166)) * den(154)
  A(148) = cont_QA(wf(:,119),wf(:,167)) * den(155)
  A(149) = cont_QA(wf(:,119),wf(:,168)) * den(156)
  A(150) = cont_QA(wf(:,36),wf(:,170)) * den(158)
  A(151) = cont_QA(wf(:,30),wf(:,150)) * den(159)
  A(152) = cont_QA(wf(:,59),wf(:,160)) * den(160)
  A(153) = cont_QA(wf(:,59),wf(:,161)) * den(161)
  A(154) = cont_QA(wf(:,121),wf(:,167)) * den(162)
  A(155) = cont_QA(wf(:,121),wf(:,168)) * den(163)
  A(156) = cont_QA(wf(:,36),wf(:,172)) * den(165)
  A(157) = cont_QA(wf(:,122),wf(:,167)) * den(166)
  A(158) = cont_QA(wf(:,122),wf(:,168)) * den(167)
  A(159) = cont_QA(wf(:,30),wf(:,153)) * den(168)
  A(160) = cont_QA(wf(:,24),wf(:,160)) * den(169)
  A(161) = cont_VV(wf(:,42),wf(:,173)) * den(170)
  A(162) = cont_VV(wf(:,43),wf(:,174)) * den(171)
  A(163) = cont_VV(wf(:,44),wf(:,177)) * den(173)
  A(164) = cont_VV(wf(:,41),wf(:,178)) * den(174)
  A(165) = cont_VV(wf(:,42),wf(:,179)) * den(175)
  A(166) = cont_VV(wf(:,44),wf(:,180)) * den(176)
  A(167) = cont_VV(wf(:,42),wf(:,181)) * den(177)
  A(168) = cont_VV(wf(:,42),wf(:,182)) * den(178)
  A(169) = cont_VV(wf(:,45),wf(:,178)) * den(179)
  A(170) = cont_VV(wf(:,43),wf(:,183)) * den(180)
  A(171) = cont_VV(wf(:,43),wf(:,184)) * den(181)
  A(172) = cont_VV(wf(:,46),wf(:,177)) * den(182)
  A(173) = cont_VV(wf(:,47),wf(:,174)) * den(183)
  A(174) = cont_VV(wf(:,44),wf(:,185)) * den(184)
  A(175) = cont_VV(wf(:,44),wf(:,186)) * den(185)
  A(176) = cont_VV(wf(:,47),wf(:,183)) * den(186)
  A(177) = cont_VV(wf(:,47),wf(:,184)) * den(187)
  A(178) = cont_VV(wf(:,46),wf(:,185)) * den(188)
  A(179) = cont_VV(wf(:,48),wf(:,173)) * den(189)
  A(180) = cont_VV(wf(:,41),wf(:,187)) * den(190)
  A(181) = cont_VV(wf(:,41),wf(:,188)) * den(191)
  A(182) = cont_VV(wf(:,48),wf(:,182)) * den(192)
  A(183) = cont_VV(wf(:,48),wf(:,181)) * den(193)
  A(184) = cont_VV(wf(:,45),wf(:,188)) * den(194)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(184)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(3)-A(4)-A(5)-A(18)-A(20)-A(21)-A(22))*f(1))/6._/**/REALKIND+((-A(11)-A(12)-A(13)-A(17)-A(19)-A(23) &
       -A(24))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(6))*f(2))/6._/**/REALKIND+((A(9)+A(10)+A(14))*f(2))/2._/**/REALKIND+((A(7) &
       +A(8))*f(3))/6._/**/REALKIND+((A(15)+A(16))*f(3))/2._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(5)+A(18)+A(20)+A(21)+A(22))*f(1))/2._/**/REALKIND+((A(11)+A(12)+A(13)+A(17)+A(19)+A(23) &
       +A(24))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(6))*f(2))/2._/**/REALKIND+((-A(9)-A(10)-A(14))*f(2))/6._/**/REALKIND+((-A(7) &
       -A(8))*f(3))/2._/**/REALKIND+((-A(15)-A(16))*f(3))/6._/**/REALKIND

  M2(1) = ((A(123)+A(124)+A(125)+A(126)+A(127)+A(128)+A(129)+A(130)+A(131)+A(162)+A(163)+A(166)+A(170)+A(171)+A(172)+A(173)+A(174) &
       +A(175)+A(176)+A(177)+A(178))*f(4))/6._/**/REALKIND+((A(145)+A(146)+A(147)+A(148)+A(149)+A(150)+A(151)+A(152)+A(153)+A(161) &
       +A(164)+A(165)+A(167)+A(168)+A(169)+A(179)+A(180)+A(181)+A(182)+A(183)+A(184))*f(4))/2._/**/REALKIND+((-A(117)-A(118) &
       -A(119)-A(120)-A(121)-A(122)-A(132)-A(133)-A(134))*f(5))/6._/**/REALKIND+((-A(139)-A(140)-A(141)-A(142)-A(143)-A(144) &
       -A(154)-A(155)-A(156))*f(5))/2._/**/REALKIND+((-A(135)-A(136)-A(137)-A(138))*f(6))/6._/**/REALKIND+((-A(157)-A(158)-A(159) &
       -A(160))*f(6))/2._/**/REALKIND+((-A(27)-A(33)-A(46)-A(49)-A(61)-A(63)-A(69)-A(70)-A(87)-A(88)-A(89)-A(93)-A(110) &
       -A(113))*f(7))/6._/**/REALKIND+((-A(41)-A(45)-A(47)-A(55)-A(62)-A(65)-A(71)-A(72)-A(79)-A(80)-A(81)-A(101)-A(109) &
       -A(115))*f(7))/2._/**/REALKIND+((A(25)+A(31)+A(85)+A(86)+A(90)+A(95))*f(8))/6._/**/REALKIND+((A(39)+A(53)+A(77)+A(78)+A(82) &
       +A(103))*f(8))/2._/**/REALKIND+((A(35)+A(91)+A(92)+A(96))*f(9))/6._/**/REALKIND+((A(57)+A(83)+A(84) &
       +A(104))*f(9))/2._/**/REALKIND+((-A(28)-A(29)-A(37)-A(38)-A(48)-A(50)-A(67)-A(68)-A(74)-A(76)-A(94)-A(98)-A(112) &
       -A(114))*f(10))/6._/**/REALKIND+((-A(42)-A(43)-A(51)-A(52)-A(59)-A(60)-A(64)-A(66)-A(73)-A(75)-A(102)-A(106)-A(111) &
       -A(116))*f(10))/2._/**/REALKIND+((A(26)+A(30)+A(32)+A(36)+A(99)+A(100))*f(11))/6._/**/REALKIND+((A(40)+A(44)+A(54)+A(58) &
       +A(107)+A(108))*f(11))/2._/**/REALKIND+((A(34)+A(97))*f(12))/6._/**/REALKIND+((A(56)+A(105))*f(12))/2._/**/REALKIND
  M2(2) = ((-A(123)-A(124)-A(125)-A(126)-A(127)-A(128)-A(129)-A(130)-A(131)-A(162)-A(163)-A(166)-A(170)-A(171)-A(172)-A(173) &
       -A(174)-A(175)-A(176)-A(177)-A(178))*f(4))/2._/**/REALKIND+((-A(145)-A(146)-A(147)-A(148)-A(149)-A(150)-A(151)-A(152) &
       -A(153)-A(161)-A(164)-A(165)-A(167)-A(168)-A(169)-A(179)-A(180)-A(181)-A(182)-A(183)-A(184))*f(4))/6._/**/REALKIND+((A(117) &
       +A(118)+A(119)+A(120)+A(121)+A(122)+A(132)+A(133)+A(134))*f(5))/2._/**/REALKIND+((A(139)+A(140)+A(141)+A(142)+A(143)+A(144) &
       +A(154)+A(155)+A(156))*f(5))/6._/**/REALKIND+((A(135)+A(136)+A(137)+A(138))*f(6))/2._/**/REALKIND+((A(157)+A(158)+A(159) &
       +A(160))*f(6))/6._/**/REALKIND+((A(27)+A(33)+A(46)+A(49)+A(61)+A(63)+A(69)+A(70)+A(87)+A(88)+A(89)+A(93)+A(110) &
       +A(113))*f(7))/2._/**/REALKIND+((A(41)+A(45)+A(47)+A(55)+A(62)+A(65)+A(71)+A(72)+A(79)+A(80)+A(81)+A(101)+A(109) &
       +A(115))*f(7))/6._/**/REALKIND+((-A(25)-A(31)-A(85)-A(86)-A(90)-A(95))*f(8))/2._/**/REALKIND+((-A(39)-A(53)-A(77)-A(78) &
       -A(82)-A(103))*f(8))/6._/**/REALKIND+((-A(35)-A(91)-A(92)-A(96))*f(9))/2._/**/REALKIND+((-A(57)-A(83)-A(84) &
       -A(104))*f(9))/6._/**/REALKIND+((A(28)+A(29)+A(37)+A(38)+A(48)+A(50)+A(67)+A(68)+A(74)+A(76)+A(94)+A(98)+A(112) &
       +A(114))*f(10))/2._/**/REALKIND+((A(42)+A(43)+A(51)+A(52)+A(59)+A(60)+A(64)+A(66)+A(73)+A(75)+A(102)+A(106)+A(111) &
       +A(116))*f(10))/6._/**/REALKIND+((-A(26)-A(30)-A(32)-A(36)-A(99)-A(100))*f(11))/2._/**/REALKIND+((-A(40)-A(44)-A(54)-A(58) &
       -A(107)-A(108))*f(11))/6._/**/REALKIND+((-A(34)-A(97))*f(12))/2._/**/REALKIND+((-A(56)-A(105))*f(12))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwajj_uddxdxaw_1_/**/REALKIND
