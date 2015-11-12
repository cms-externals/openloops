
module ol_colourmatrix_ppzwjj_uuuxdxzw_1_/**/REALKIND
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
end module ol_colourmatrix_ppzwjj_uuuxdxzw_1_/**/REALKIND



module ol_forced_parameters_ppzwjj_uuuxdxzw_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzwjj_uuuxdxzw_1_/**/REALKIND

module ol_loop_ppzwjj_uuuxdxzw_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(34)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:251)
  ! denominators
  complex(REALKIND), save :: den(282)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,144)
  ! zero helicity identifier
  logical,           save :: zerohel(144) = .true., zerohel_ct(144) = .true.

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
    f( 1) = (CI*cw*eQED**2*gQCD**2)/(sqrt2*sw**2)
    f( 2) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/(sqrt2*sw**2)
    f( 3) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/(sqrt2*sw**2)
    f( 4) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/(sqrt2*sw**2)
    f( 5) = (CI*eQED**2*gQCD**2)/(sqrt2*sw)
    f( 6) = (CI*countertermnorm*eQED**2*gQCD**4)/(sqrt2*sw)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f( 8) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f( 9) = (countertermnorm*ctZGG*eQED**2*gQCD**4)/(sqrt2*sw)
    f(10) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw**2)
    f(11) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw**2*2._/**/REALKIND)
    f(12) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw**2)
    f(13) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(14) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*2._/**/REALKIND)
    f(15) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(16) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw**2)
    f(17) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw**2)
    f(18) = (eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)
    f(19) = (2*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)

  c = [ 9*CI*f(10), 27*CI*f(10), 18*f(11), 54*f(11), f(12), 3*f(12), 6*f(12), 8*f(12), 10*f(12), 18*f(12), 21*f(12), 24*f(12) &
    , 54*f(12), 9*CI*f(13), 27*CI*f(13), 18*f(14), 54*f(14), f(15), 3*f(15), 6*f(15), 8*f(15), 10*f(15), 18*f(15), 21*f(15) &
    , 24*f(15), 54*f(15), 3*f(16), 9*f(16), 3*f(17), 9*f(17), 3*f(18), 9*f(18), 3*f(19), 9*f(19) ]
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
  complex(REALKIND) :: A(188)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,-1),wf(:,2))
  call vert_AW_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,18),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_WQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,13),ZERO,0_intkind1,wf(:,9))
  call vert_WQ_A(wf(:,-5),wf(:,-1),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,34),ZERO,0_intkind1,wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,24),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,12),wf(:,14))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,15))
  call vert_AW_Q(wf(:,13),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,18))
  call vert_AZ_Q(gZu,wf(:,5),wf(:,-4),wf(:,19))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,48),MW,1_intkind1,wf(:,21))
  call vert_AW_Q(wf(:,-3),wf(:,21),wf(:,22))
  call vert_WQ_A(wf(:,21),wf(:,-1),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,50),ZERO,0_intkind1,wf(:,24))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,25))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,26))
  call prop_Q_A(wf(:,25),Q(:,17),ZERO,0_intkind1,wf(:,27))
  call vert_VQ_A(wf(:,26),wf(:,27),wf(:,28))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,29))
  call vert_WQ_A(wf(:,-5),wf(:,27),wf(:,30))
  call prop_A_Q(wf(:,29),Q(:,14),ZERO,0_intkind1,wf(:,31))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,33),ZERO,0_intkind1,wf(:,33))
  call vert_VQ_A(wf(:,26),wf(:,33),wf(:,34))
  call vert_VQ_A(wf(:,26),wf(:,0),wf(:,35))
  call prop_Q_A(wf(:,35),Q(:,7),ZERO,0_intkind1,wf(:,36))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,33),wf(:,37))
  call vert_WQ_A(wf(:,21),wf(:,0),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,49),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,27),wf(:,-2),wf(:,40))
  call vert_QA_V(wf(:,12),wf(:,-3),wf(:,41))
  call vert_QA_V(wf(:,-1),wf(:,5),wf(:,42))
  call vert_QA_V(wf(:,4),wf(:,-2),wf(:,43))
  call vert_QA_V(wf(:,33),wf(:,-3),wf(:,44))
  call vert_QA_V(wf(:,0),wf(:,5),wf(:,45))
  call vert_AZ_Q(gZu,wf(:,-2),wf(:,-4),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,20),ZERO,0_intkind1,wf(:,47))
  call vert_QA_V(wf(:,-1),wf(:,47),wf(:,48))
  call vert_QA_V(wf(:,0),wf(:,47),wf(:,49))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,50))
  call counter_WQ_A(wf(:,-5),wf(:,4),wf(:,51))
  call counter_VQ_A(wf(:,1),wf(:,12),wf(:,52))
  call counter_AW_Q(wf(:,13),wf(:,-5),wf(:,53))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,54))
  call counter_VG_G(wf(:,-4),wf(:,1),Q(:,5),wf(:,55),Q(:,21))
  call counter_AZ_Q(gZu,wf(:,5),wf(:,-4),wf(:,56))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,57))
  call prop_Q_A(wf(:,8),Q(:,50),ZERO,0_intkind1,wf(:,58))
  call counter_AW_Q(wf(:,-3),wf(:,-5),wf(:,59))
  call prop_A_Q(wf(:,59),Q(:,40),ZERO,0_intkind1,wf(:,60))
  call prop_Q_A(wf(:,18),Q(:,50),ZERO,0_intkind1,wf(:,61))
  call counter_AW_Q(wf(:,-3),wf(:,21),wf(:,62))
  call vert_AZ_Q(gZu,wf(:,60),wf(:,-4),wf(:,63))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,24),ZERO,0_intkind1,wf(:,65))
  call vert_AW_Q(wf(:,65),wf(:,-5),wf(:,66))
  call counter_VQ_A(wf(:,26),wf(:,27),wf(:,67))
  call counter_WQ_A(wf(:,-5),wf(:,27),wf(:,68))
  call counter_VQ_A(wf(:,26),wf(:,33),wf(:,69))
  call counter_VG_G(wf(:,-4),wf(:,26),Q(:,6),wf(:,70),Q(:,22))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,33),wf(:,71))
  call counter_AV_Q(wf(:,-3),wf(:,26),wf(:,72))
  call prop_Q_A(wf(:,30),Q(:,49),ZERO,0_intkind1,wf(:,73))
  call prop_Q_A(wf(:,37),Q(:,49),ZERO,0_intkind1,wf(:,74))
  call counter_QA_V(wf(:,12),wf(:,-3),wf(:,75))
  call vert_QA_V(wf(:,-1),wf(:,60),wf(:,76))
  call counter_QA_V(wf(:,33),wf(:,-3),wf(:,77))
  call vert_QA_V(wf(:,0),wf(:,60),wf(:,78))
  call counter_QA_V(wf(:,27),wf(:,-2),wf(:,79))
  call counter_QA_V(wf(:,4),wf(:,-2),wf(:,80))
  call counter_AZ_Q(gZu,wf(:,-2),wf(:,-4),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,20),ZERO,0_intkind1,wf(:,82))
  call vert_QA_V(wf(:,-1),wf(:,82),wf(:,83))
  call vert_QA_V(wf(:,0),wf(:,82),wf(:,84))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,85))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,86))
  call counter_WQ_A(wf(:,-5),wf(:,-1),wf(:,87))
  call prop_Q_A(wf(:,87),Q(:,34),ZERO,0_intkind1,wf(:,88))
  call vert_VQ_A(wf(:,1),wf(:,88),wf(:,89))
  call prop_A_Q(wf(:,19),Q(:,56),ZERO,0_intkind1,wf(:,90))
  call counter_WQ_A(wf(:,21),wf(:,-1),wf(:,91))
  call prop_A_Q(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,92))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,88),wf(:,93))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,-1),wf(:,94))
  call prop_Q_A(wf(:,94),Q(:,18),ZERO,0_intkind1,wf(:,95))
  call vert_VQ_A(wf(:,1),wf(:,95),wf(:,96))
  call vert_WQ_A(wf(:,-5),wf(:,95),wf(:,97))
  call counter_QA_V(wf(:,-1),wf(:,5),wf(:,98))
  call vert_QA_V(wf(:,88),wf(:,-3),wf(:,99))
  call counter_QA_V(wf(:,-1),wf(:,47),wf(:,100))
  call vert_QA_V(wf(:,95),wf(:,-2),wf(:,101))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,102))
  call vert_VQ_A(wf(:,102),wf(:,27),wf(:,103))
  call vert_AV_Q(wf(:,-3),wf(:,102),wf(:,104))
  call prop_A_Q(wf(:,104),Q(:,14),ZERO,0_intkind1,wf(:,105))
  call vert_VQ_A(wf(:,102),wf(:,33),wf(:,106))
  call vert_VQ_A(wf(:,102),wf(:,0),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,7),ZERO,0_intkind1,wf(:,108))
  call counter_VQ_A(wf(:,26),wf(:,0),wf(:,109))
  call counter_WQ_A(wf(:,-5),wf(:,0),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,33),ZERO,0_intkind1,wf(:,111))
  call vert_VQ_A(wf(:,26),wf(:,111),wf(:,112))
  call counter_WQ_A(wf(:,21),wf(:,0),wf(:,113))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,111),wf(:,114))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,17),ZERO,0_intkind1,wf(:,116))
  call vert_VQ_A(wf(:,26),wf(:,116),wf(:,117))
  call vert_WQ_A(wf(:,-5),wf(:,116),wf(:,118))
  call counter_QA_V(wf(:,0),wf(:,5),wf(:,119))
  call vert_QA_V(wf(:,111),wf(:,-3),wf(:,120))
  call counter_QA_V(wf(:,0),wf(:,47),wf(:,121))
  call vert_QA_V(wf(:,116),wf(:,-2),wf(:,122))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,123))
  call vert_VQ_A(wf(:,123),wf(:,4),wf(:,124))
  call vert_AV_Q(wf(:,-3),wf(:,123),wf(:,125))
  call prop_A_Q(wf(:,125),Q(:,13),ZERO,0_intkind1,wf(:,126))
  call vert_VQ_A(wf(:,123),wf(:,12),wf(:,127))
  call vert_VQ_A(wf(:,123),wf(:,-1),wf(:,128))
  call prop_Q_A(wf(:,128),Q(:,7),ZERO,0_intkind1,wf(:,129))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,130))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,131))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,132))
  call counter_Q_A(ctqq,wf(:,4),Q(:,18),wf(:,133))
  call prop_A_Q(wf(:,132),Q(:,45),ZERO,0_intkind1,wf(:,134))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,135))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,136))
  call vert_AV_Q(wf(:,-3),wf(:,131),wf(:,137))
  call prop_Q_A(wf(:,133),Q(:,18),ZERO,0_intkind1,wf(:,138))
  call vert_WQ_A(wf(:,-5),wf(:,138),wf(:,139))
  call counter_A_Q(ctqq,wf(:,9),Q(:,13),wf(:,140))
  call vert_QA_V(wf(:,12),wf(:,13),wf(:,141))
  call vert_AV_Q(wf(:,13),wf(:,1),wf(:,142))
  call counter_Q_A(ctqq,wf(:,12),Q(:,34),wf(:,143))
  call prop_A_Q(wf(:,142),Q(:,29),ZERO,0_intkind1,wf(:,144))
  call counter_A_Q(ctqq,wf(:,13),Q(:,24),wf(:,145))
  call prop_Q_A(wf(:,14),Q(:,39),ZERO,0_intkind1,wf(:,146))
  call vert_VQ_A(wf(:,131),wf(:,-1),wf(:,147))
  call counter_Q_A(ctqq,wf(:,17),Q(:,7),wf(:,148))
  call prop_A_Q(wf(:,145),Q(:,24),ZERO,0_intkind1,wf(:,149))
  call vert_AW_Q(wf(:,149),wf(:,-5),wf(:,150))
  call prop_Q_A(wf(:,143),Q(:,34),ZERO,0_intkind1,wf(:,151))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,151),wf(:,152))
  call prop_A_Q(wf(:,135),Q(:,40),ZERO,0_intkind1,wf(:,153))
  call vert_AZ_Q(gZu,wf(:,153),wf(:,-4),wf(:,154))
  call counter_Q_A(ctqq,wf(:,24),Q(:,50),wf(:,155))
  call vert_AV_Q(wf(:,5),wf(:,26),wf(:,156))
  call counter_Q_A(ctqq,wf(:,27),Q(:,17),wf(:,157))
  call prop_A_Q(wf(:,156),Q(:,46),ZERO,0_intkind1,wf(:,158))
  call vert_QA_V(wf(:,27),wf(:,5),wf(:,159))
  call counter_V_V(ctGG,wf(:,26),Q(:,6),wf(:,160))
  call prop_Q_A(wf(:,28),Q(:,23),ZERO,0_intkind1,wf(:,161))
  call prop_Q_A(wf(:,157),Q(:,17),ZERO,0_intkind1,wf(:,162))
  call vert_WQ_A(wf(:,-5),wf(:,162),wf(:,163))
  call vert_AV_Q(wf(:,-3),wf(:,160),wf(:,164))
  call counter_A_Q(ctqq,wf(:,31),Q(:,14),wf(:,165))
  call vert_AV_Q(wf(:,13),wf(:,26),wf(:,166))
  call counter_Q_A(ctqq,wf(:,33),Q(:,33),wf(:,167))
  call prop_A_Q(wf(:,166),Q(:,30),ZERO,0_intkind1,wf(:,168))
  call vert_QA_V(wf(:,33),wf(:,13),wf(:,169))
  call prop_Q_A(wf(:,34),Q(:,39),ZERO,0_intkind1,wf(:,170))
  call vert_VQ_A(wf(:,160),wf(:,0),wf(:,171))
  call counter_Q_A(ctqq,wf(:,36),Q(:,7),wf(:,172))
  call prop_Q_A(wf(:,167),Q(:,33),ZERO,0_intkind1,wf(:,173))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,173),wf(:,174))
  call counter_Q_A(ctqq,wf(:,39),Q(:,49),wf(:,175))
  call vert_QA_V(wf(:,162),wf(:,-2),wf(:,176))
  call vert_QA_V(wf(:,151),wf(:,-3),wf(:,177))
  call counter_V_V(ctGG,wf(:,40),Q(:,21),wf(:,178))
  call counter_V_V(ctGG,wf(:,42),Q(:,42),wf(:,179))
  call vert_QA_V(wf(:,-1),wf(:,153),wf(:,180))
  call vert_QA_V(wf(:,173),wf(:,-3),wf(:,181))
  call vert_QA_V(wf(:,138),wf(:,-2),wf(:,182))
  call counter_V_V(ctGG,wf(:,43),Q(:,22),wf(:,183))
  call counter_V_V(ctGG,wf(:,45),Q(:,41),wf(:,184))
  call vert_QA_V(wf(:,0),wf(:,153),wf(:,185))
  call counter_A_Q(ctqq,wf(:,47),Q(:,20),wf(:,186))
  call prop_A_Q(wf(:,186),Q(:,20),ZERO,0_intkind1,wf(:,187))
  call vert_QA_V(wf(:,-1),wf(:,187),wf(:,188))
  call counter_V_V(ctGG,wf(:,48),Q(:,22),wf(:,189))
  call vert_QA_V(wf(:,0),wf(:,187),wf(:,190))
  call counter_V_V(ctGG,wf(:,49),Q(:,21),wf(:,191))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,17),wf(:,192))
  call prop_Q_A(wf(:,192),Q(:,23),ZERO,0_intkind1,wf(:,193))
  call vert_WQ_A(wf(:,-5),wf(:,17),wf(:,194))
  call prop_Q_A(wf(:,194),Q(:,39),ZERO,0_intkind1,wf(:,195))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-4),wf(:,196))
  call prop_A_Q(wf(:,196),Q(:,29),ZERO,0_intkind1,wf(:,197))
  call vert_AW_Q(wf(:,9),wf(:,-5),wf(:,198))
  call prop_A_Q(wf(:,198),Q(:,45),ZERO,0_intkind1,wf(:,199))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,36),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,23),ZERO,0_intkind1,wf(:,201))
  call vert_WQ_A(wf(:,-5),wf(:,36),wf(:,202))
  call prop_Q_A(wf(:,202),Q(:,39),ZERO,0_intkind1,wf(:,203))
  call vert_AZ_Q(gZd,wf(:,31),wf(:,-4),wf(:,204))
  call prop_A_Q(wf(:,204),Q(:,30),ZERO,0_intkind1,wf(:,205))
  call vert_AW_Q(wf(:,31),wf(:,-5),wf(:,206))
  call prop_A_Q(wf(:,206),Q(:,46),ZERO,0_intkind1,wf(:,207))
  call vert_VQ_A(wf(:,40),wf(:,-1),wf(:,208))
  call prop_Q_A(wf(:,208),Q(:,23),ZERO,0_intkind1,wf(:,209))
  call vert_AV_Q(wf(:,-3),wf(:,40),wf(:,210))
  call prop_A_Q(wf(:,210),Q(:,29),ZERO,0_intkind1,wf(:,211))
  call vert_QA_V(wf(:,73),wf(:,-3),wf(:,212))
  call vert_VQ_A(wf(:,43),wf(:,0),wf(:,213))
  call prop_Q_A(wf(:,213),Q(:,23),ZERO,0_intkind1,wf(:,214))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,215))
  call prop_A_Q(wf(:,215),Q(:,30),ZERO,0_intkind1,wf(:,216))
  call vert_QA_V(wf(:,58),wf(:,-3),wf(:,217))
  call vert_VQ_A(wf(:,49),wf(:,-1),wf(:,218))
  call prop_Q_A(wf(:,218),Q(:,23),ZERO,0_intkind1,wf(:,219))
  call vert_VQ_A(wf(:,48),wf(:,0),wf(:,220))
  call prop_Q_A(wf(:,220),Q(:,23),ZERO,0_intkind1,wf(:,221))
  call vert_AV_Q(wf(:,-3),wf(:,49),wf(:,222))
  call prop_A_Q(wf(:,222),Q(:,29),ZERO,0_intkind1,wf(:,223))
  call vert_AV_Q(wf(:,-3),wf(:,48),wf(:,224))
  call prop_A_Q(wf(:,224),Q(:,30),ZERO,0_intkind1,wf(:,225))
  call vert_QA_V(wf(:,0),wf(:,86),wf(:,226))
  call vert_QA_V(wf(:,-1),wf(:,86),wf(:,227))
  call vert_VQ_A(wf(:,44),wf(:,-1),wf(:,228))
  call prop_Q_A(wf(:,228),Q(:,43),ZERO,0_intkind1,wf(:,229))
  call vert_AV_Q(wf(:,-2),wf(:,44),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,45),ZERO,0_intkind1,wf(:,231))
  call vert_QA_V(wf(:,74),wf(:,-3),wf(:,232))
  call vert_VQ_A(wf(:,41),wf(:,0),wf(:,233))
  call prop_Q_A(wf(:,233),Q(:,43),ZERO,0_intkind1,wf(:,234))
  call vert_AV_Q(wf(:,-2),wf(:,41),wf(:,235))
  call prop_A_Q(wf(:,235),Q(:,46),ZERO,0_intkind1,wf(:,236))
  call vert_QA_V(wf(:,61),wf(:,-3),wf(:,237))
  call vert_VQ_A(wf(:,45),wf(:,-1),wf(:,238))
  call prop_Q_A(wf(:,238),Q(:,43),ZERO,0_intkind1,wf(:,239))
  call vert_VQ_A(wf(:,42),wf(:,0),wf(:,240))
  call prop_Q_A(wf(:,240),Q(:,43),ZERO,0_intkind1,wf(:,241))
  call vert_AV_Q(wf(:,-2),wf(:,45),wf(:,242))
  call prop_A_Q(wf(:,242),Q(:,45),ZERO,0_intkind1,wf(:,243))
  call vert_QA_V(wf(:,0),wf(:,90),wf(:,244))
  call vert_AV_Q(wf(:,-2),wf(:,42),wf(:,245))
  call prop_A_Q(wf(:,245),Q(:,46),ZERO,0_intkind1,wf(:,246))
  call vert_QA_V(wf(:,-1),wf(:,90),wf(:,247))
  call vert_QA_V(wf(:,39),wf(:,-3),wf(:,248))
  call vert_QA_V(wf(:,0),wf(:,92),wf(:,249))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,250))
  call vert_QA_V(wf(:,-1),wf(:,92),wf(:,251))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,18))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,13))
  den(9) = 1 / (Q(5,34))
  den(10) = 1 / (Q(5,24))
  den(13) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,48) - MW2)
  den(20) = 1 / (Q(5,50))
  den(23) = 1 / (Q(5,17))
  den(24) = 1 / (Q(5,6))
  den(27) = 1 / (Q(5,14))
  den(30) = 1 / (Q(5,33))
  den(38) = 1 / (Q(5,49))
  den(41) = 1 / (Q(5,21))
  den(44) = 1 / (Q(5,42))
  den(47) = 1 / (Q(5,22))
  den(50) = 1 / (Q(5,41))
  den(53) = 1 / (Q(5,20))
  den(76) = 1 / (Q(5,56))
  den(95) = 1 / (Q(5,58))
  den(99) = 1 / (Q(5,45))
  den(102) = 1 / (Q(5,23))
  den(114) = 1 / (Q(5,29))
  den(117) = 1 / (Q(5,39))
  den(137) = 1 / (Q(5,46))
  den(141) = 1 / (Q(5,57))
  den(152) = 1 / (Q(5,30))
  den(219) = 1 / (Q(5,43))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(3)*den(14)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(1)*den(21)
  den(25) = den(23)*den(24)
  den(26) = den(3)*den(25)
  den(28) = den(24)*den(27)
  den(29) = den(23)*den(28)
  den(31) = den(24)*den(30)
  den(32) = den(10)*den(31)
  den(33) = den(13)*den(24)
  den(34) = den(10)*den(33)
  den(35) = den(28)*den(30)
  den(36) = den(3)*den(33)
  den(37) = den(18)*den(33)
  den(39) = den(18)*den(38)
  den(40) = den(24)*den(39)
  den(42) = den(23)*den(41)
  den(43) = den(9)*den(42)
  den(45) = den(3)*den(44)
  den(46) = den(23)*den(45)
  den(48) = den(2)*den(47)
  den(49) = den(30)*den(48)
  den(51) = den(3)*den(50)
  den(52) = den(2)*den(51)
  den(54) = den(47)*den(53)
  den(55) = den(30)*den(54)
  den(56) = den(41)*den(53)
  den(57) = den(9)*den(56)
  den(58) = den(3)*den(56)
  den(59) = den(51)*den(53)
  den(60) = den(9)*den(44)
  den(61) = den(1)*den(60)
  den(62) = den(1)*den(45)
  den(63) = den(2)*den(20)
  den(64) = den(1)*den(63)
  den(65) = den(9)*den(20)
  den(66) = den(1)*den(65)
  den(67) = den(30)*den(50)
  den(68) = den(24)*den(67)
  den(69) = den(24)*den(51)
  den(70) = den(23)*den(38)
  den(71) = den(24)*den(70)
  den(72) = den(30)*den(38)
  den(73) = den(24)*den(72)
  den(74) = den(23)*den(60)
  den(75) = den(2)*den(67)
  den(77) = den(10)*den(76)
  den(78) = den(1)*den(77)
  den(79) = den(3)*den(76)
  den(80) = den(1)*den(79)
  den(81) = den(7)*den(18)
  den(82) = den(18)*den(76)
  den(83) = den(1)*den(82)
  den(84) = den(3)*den(42)
  den(85) = den(53)*den(67)
  den(86) = den(24)*den(77)
  den(87) = den(24)*den(79)
  den(88) = den(24)*den(82)
  den(89) = den(18)*den(28)
  den(90) = den(3)*den(48)
  den(91) = den(53)*den(60)
  den(92) = den(45)*den(53)
  den(93) = den(3)*den(54)
  den(94) = den(2)*den(3)
  den(96) = den(94)*den(95)
  den(97) = den(1)*den(96)
  den(98) = den(1)*den(3)
  den(100) = den(98)*den(99)
  den(101) = den(2)*den(100)
  den(103) = den(4)*den(102)
  den(104) = den(3)*den(103)
  den(105) = den(1)**2
  den(106) = den(63)*den(105)
  den(107) = den(2)**2
  den(108) = den(7)*den(107)
  den(109) = den(7)*den(63)
  den(110) = den(9)*den(10)
  den(111) = den(95)*den(110)
  den(112) = den(1)*den(111)
  den(113) = den(1)*den(10)
  den(115) = den(113)*den(114)
  den(116) = den(9)*den(115)
  den(118) = den(11)*den(117)
  den(119) = den(10)*den(118)
  den(120) = den(77)*den(105)
  den(121) = den(14)*den(77)
  den(122) = den(10)**2
  den(123) = den(14)*den(122)
  den(124) = den(65)*den(105)
  den(125) = den(9)**2
  den(126) = den(7)*den(125)
  den(127) = den(7)*den(65)
  den(128) = den(79)*den(105)
  den(129) = den(14)*den(79)
  den(130) = den(3)**2
  den(131) = den(14)*den(130)
  den(132) = den(82)*den(105)
  den(133) = den(21)*den(105)
  den(134) = den(14)*den(82)
  den(135) = den(7)*den(21)
  den(136) = den(3)*den(24)
  den(138) = den(136)*den(137)
  den(139) = den(23)*den(138)
  den(140) = den(3)*den(23)
  den(142) = den(140)*den(141)
  den(143) = den(24)*den(142)
  den(144) = den(25)*den(102)
  den(145) = den(3)*den(144)
  den(146) = den(23)**2
  den(147) = den(28)*den(146)
  den(148) = den(24)**2
  den(149) = den(70)*den(148)
  den(150) = den(28)*den(70)
  den(151) = den(10)*den(24)
  den(153) = den(151)*den(152)
  den(154) = den(30)*den(153)
  den(155) = den(10)*den(30)
  den(156) = den(141)*den(155)
  den(157) = den(24)*den(156)
  den(158) = den(31)*den(117)
  den(159) = den(10)*den(158)
  den(160) = den(77)*den(148)
  den(161) = den(33)*den(77)
  den(162) = den(33)*den(122)
  den(163) = den(30)**2
  den(164) = den(28)*den(163)
  den(165) = den(72)*den(148)
  den(166) = den(28)*den(72)
  den(167) = den(79)*den(148)
  den(168) = den(33)*den(79)
  den(169) = den(33)*den(130)
  den(170) = den(82)*den(148)
  den(171) = den(33)*den(82)
  den(172) = den(28)*den(39)
  den(173) = den(39)*den(148)
  den(174) = den(60)*den(146)
  den(175) = den(42)*den(125)
  den(176) = den(42)*den(60)
  den(177) = den(45)*den(146)
  den(178) = den(42)*den(45)
  den(179) = den(42)*den(130)
  den(180) = den(48)*den(163)
  den(181) = den(67)*den(107)
  den(182) = den(48)*den(67)
  den(183) = den(48)*den(51)
  den(184) = den(48)*den(130)
  den(185) = den(51)*den(107)
  den(186) = den(54)*den(163)
  den(187) = den(53)**2
  den(188) = den(67)*den(187)
  den(189) = den(54)*den(67)
  den(190) = den(60)*den(187)
  den(191) = den(56)*den(60)
  den(192) = den(56)*den(125)
  den(193) = den(45)*den(56)
  den(194) = den(51)*den(54)
  den(195) = den(45)*den(187)
  den(196) = den(54)*den(130)
  den(197) = den(51)*den(187)
  den(198) = den(56)*den(130)
  den(199) = den(14)*den(102)
  den(200) = den(14)*den(117)
  den(201) = den(7)*den(114)
  den(202) = den(7)*den(99)
  den(203) = den(33)*den(102)
  den(204) = den(33)*den(117)
  den(205) = den(28)*den(152)
  den(206) = den(28)*den(137)
  den(207) = den(42)*den(102)
  den(208) = den(42)*den(114)
  den(209) = den(70)*den(141)
  den(210) = den(48)*den(102)
  den(211) = den(48)*den(152)
  den(212) = den(63)*den(95)
  den(213) = den(56)*den(102)
  den(214) = den(54)*den(102)
  den(215) = den(56)*den(114)
  den(216) = den(54)*den(152)
  den(217) = den(77)*den(141)
  den(218) = den(77)*den(95)
  den(220) = den(67)*den(219)
  den(221) = den(67)*den(99)
  den(222) = den(72)*den(141)
  den(223) = den(60)*den(219)
  den(224) = den(60)*den(137)
  den(225) = den(65)*den(95)
  den(226) = den(51)*den(219)
  den(227) = den(45)*den(219)
  den(228) = den(51)*den(99)
  den(229) = den(79)*den(141)
  den(230) = den(45)*den(137)
  den(231) = den(79)*den(95)
  den(232) = den(39)*den(141)
  den(233) = den(82)*den(141)
  den(234) = den(21)*den(95)
  den(235) = den(82)*den(95)
  den(236) = den(1)*den(2)*den(3)
  den(237) = den(1)*den(9)*den(10)
  den(238) = den(1)*den(18)
  den(239) = den(3)*den(23)*den(24)
  den(240) = den(10)*den(24)*den(30)
  den(241) = den(18)*den(24)
  den(242) = den(9)*den(23)
  den(243) = den(2)*den(30)
  den(244) = den(30)*den(53)
  den(245) = den(9)*den(53)
  den(246) = den(3)*den(53)
  den(247) = den(2)*den(202)
  den(248) = den(1)*den(212)
  den(249) = den(10)*den(200)
  den(250) = den(1)*den(218)
  den(251) = den(9)*den(201)
  den(252) = den(1)*den(225)
  den(253) = den(3)*den(199)
  den(254) = den(1)*den(231)
  den(255) = den(1)*den(234)
  den(256) = den(1)*den(235)
  den(257) = den(24)*den(209)
  den(258) = den(23)*den(206)
  den(259) = den(10)*den(204)
  den(260) = den(24)*den(217)
  den(261) = den(24)*den(222)
  den(262) = den(30)*den(205)
  den(263) = den(3)*den(203)
  den(264) = den(24)*den(229)
  den(265) = den(24)*den(232)
  den(266) = den(24)*den(233)
  den(267) = den(9)*den(208)
  den(268) = den(23)*den(224)
  den(269) = den(3)*den(207)
  den(270) = den(23)*den(230)
  den(271) = den(2)*den(221)
  den(272) = den(30)*den(211)
  den(273) = den(2)*den(228)
  den(274) = den(3)*den(210)
  den(275) = den(53)*den(220)
  den(276) = den(30)*den(216)
  den(277) = den(9)*den(215)
  den(278) = den(53)*den(223)
  den(279) = den(3)*den(213)
  den(280) = den(53)*den(226)
  den(281) = den(3)*den(214)
  den(282) = den(53)*den(227)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(188)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,17),wf(:,19)) * den(17)
  A(7) = cont_QA(wf(:,17),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,7),wf(:,24)) * den(22)
  A(9) = cont_QA(wf(:,5),wf(:,28)) * den(26)
  A(10) = cont_QA(wf(:,30),wf(:,31)) * den(29)
  A(11) = cont_QA(wf(:,13),wf(:,34)) * den(32)
  A(12) = cont_QA(wf(:,16),wf(:,36)) * den(34)
  A(13) = cont_QA(wf(:,31),wf(:,37)) * den(35)
  A(14) = cont_QA(wf(:,19),wf(:,36)) * den(36)
  A(15) = cont_QA(wf(:,22),wf(:,36)) * den(37)
  A(16) = cont_QA(wf(:,29),wf(:,39)) * den(40)
  A(17) = cont_VV(wf(:,40),wf(:,41)) * den(43)
  A(18) = cont_VV(wf(:,40),wf(:,42)) * den(46)
  A(19) = cont_VV(wf(:,43),wf(:,44)) * den(49)
  A(20) = cont_VV(wf(:,43),wf(:,45)) * den(52)
  A(21) = cont_VV(wf(:,44),wf(:,48)) * den(55)
  A(22) = cont_VV(wf(:,41),wf(:,49)) * den(57)
  A(23) = cont_VV(wf(:,42),wf(:,49)) * den(58)
  A(24) = cont_VV(wf(:,45),wf(:,48)) * den(59)

  A(25) = cont_QA(wf(:,5),wf(:,50)) * den(5)
  A(26) = cont_QA(wf(:,9),wf(:,51)) * den(8)
  A(27) = cont_QA(wf(:,13),wf(:,52)) * den(12)
  A(28) = cont_QA(wf(:,17),wf(:,53)) * den(15)
  A(29) = cont_QA(wf(:,9),wf(:,54)) * den(16)
  A(30) = cont_VV(wf(:,41),wf(:,55)) * den(61)
  A(31) = cont_QA(wf(:,17),wf(:,56)) * den(17)
  A(32) = cont_VV(wf(:,42),wf(:,55)) * den(62)
  A(33) = cont_QA(wf(:,57),wf(:,58)) * den(64)
  A(34) = cont_QA(wf(:,6),wf(:,60)) * den(5)
  A(35) = cont_QA(wf(:,57),wf(:,61)) * den(66)
  A(36) = cont_QA(wf(:,17),wf(:,62)) * den(19)
  A(37) = cont_QA(wf(:,24),wf(:,57)) * den(22)
  A(38) = cont_QA(wf(:,17),wf(:,63)) * den(17)
  A(39) = cont_QA(wf(:,14),wf(:,65)) * den(12)
  A(40) = cont_QA(wf(:,17),wf(:,66)) * den(15)
  A(41) = cont_QA(wf(:,5),wf(:,67)) * den(26)
  A(42) = cont_QA(wf(:,31),wf(:,68)) * den(29)
  A(43) = cont_QA(wf(:,13),wf(:,69)) * den(32)
  A(44) = cont_QA(wf(:,36),wf(:,53)) * den(34)
  A(45) = cont_VV(wf(:,44),wf(:,70)) * den(68)
  A(46) = cont_QA(wf(:,31),wf(:,71)) * den(35)
  A(47) = cont_QA(wf(:,36),wf(:,56)) * den(36)
  A(48) = cont_VV(wf(:,45),wf(:,70)) * den(69)
  A(49) = cont_QA(wf(:,72),wf(:,73)) * den(71)
  A(50) = cont_QA(wf(:,28),wf(:,60)) * den(26)
  A(51) = cont_QA(wf(:,72),wf(:,74)) * den(73)
  A(52) = cont_QA(wf(:,36),wf(:,62)) * den(37)
  A(53) = cont_QA(wf(:,39),wf(:,72)) * den(40)
  A(54) = cont_QA(wf(:,36),wf(:,63)) * den(36)
  A(55) = cont_QA(wf(:,34),wf(:,65)) * den(32)
  A(56) = cont_QA(wf(:,36),wf(:,66)) * den(34)
  A(57) = cont_VV(wf(:,40),wf(:,75)) * den(43)
  A(58) = cont_VV(wf(:,40),wf(:,76)) * den(46)
  A(59) = cont_VV(wf(:,43),wf(:,77)) * den(49)
  A(60) = cont_VV(wf(:,43),wf(:,78)) * den(52)
  A(61) = cont_VV(wf(:,48),wf(:,77)) * den(55)
  A(62) = cont_VV(wf(:,49),wf(:,75)) * den(57)
  A(63) = cont_VV(wf(:,49),wf(:,76)) * den(58)
  A(64) = cont_VV(wf(:,48),wf(:,78)) * den(59)
  A(65) = cont_VV(wf(:,41),wf(:,79)) * den(74)
  A(66) = cont_VV(wf(:,42),wf(:,79)) * den(46)
  A(67) = cont_VV(wf(:,44),wf(:,80)) * den(75)
  A(68) = cont_VV(wf(:,45),wf(:,80)) * den(52)
  A(69) = cont_VV(wf(:,44),wf(:,83)) * den(55)
  A(70) = cont_VV(wf(:,41),wf(:,84)) * den(57)
  A(71) = cont_VV(wf(:,45),wf(:,83)) * den(59)
  A(72) = cont_VV(wf(:,42),wf(:,84)) * den(58)
  A(73) = cont_QA(wf(:,85),wf(:,86)) * den(78)
  A(74) = cont_QA(wf(:,13),wf(:,89)) * den(12)
  A(75) = cont_QA(wf(:,85),wf(:,90)) * den(80)
  A(76) = cont_QA(wf(:,9),wf(:,91)) * den(81)
  A(77) = cont_QA(wf(:,85),wf(:,92)) * den(83)
  A(78) = cont_QA(wf(:,9),wf(:,93)) * den(16)
  A(79) = cont_QA(wf(:,5),wf(:,96)) * den(5)
  A(80) = cont_QA(wf(:,9),wf(:,97)) * den(8)
  A(81) = cont_VV(wf(:,40),wf(:,98)) * den(84)
  A(82) = cont_VV(wf(:,40),wf(:,99)) * den(43)
  A(83) = cont_VV(wf(:,44),wf(:,100)) * den(85)
  A(84) = cont_VV(wf(:,49),wf(:,98)) * den(58)
  A(85) = cont_VV(wf(:,45),wf(:,100)) * den(59)
  A(86) = cont_VV(wf(:,49),wf(:,99)) * den(57)
  A(87) = cont_VV(wf(:,44),wf(:,101)) * den(49)
  A(88) = cont_VV(wf(:,45),wf(:,101)) * den(52)
  A(89) = cont_QA(wf(:,5),wf(:,103)) * den(26)
  A(90) = cont_QA(wf(:,30),wf(:,105)) * den(29)
  A(91) = cont_QA(wf(:,13),wf(:,106)) * den(32)
  A(92) = cont_QA(wf(:,16),wf(:,108)) * den(34)
  A(93) = cont_QA(wf(:,37),wf(:,105)) * den(35)
  A(94) = cont_QA(wf(:,19),wf(:,108)) * den(36)
  A(95) = cont_QA(wf(:,39),wf(:,104)) * den(40)
  A(96) = cont_QA(wf(:,22),wf(:,108)) * den(37)
  A(97) = cont_QA(wf(:,86),wf(:,109)) * den(86)
  A(98) = cont_QA(wf(:,13),wf(:,112)) * den(32)
  A(99) = cont_QA(wf(:,90),wf(:,109)) * den(87)
  A(100) = cont_QA(wf(:,92),wf(:,109)) * den(88)
  A(101) = cont_QA(wf(:,31),wf(:,113)) * den(89)
  A(102) = cont_QA(wf(:,31),wf(:,114)) * den(35)
  A(103) = cont_QA(wf(:,5),wf(:,117)) * den(26)
  A(104) = cont_QA(wf(:,31),wf(:,118)) * den(29)
  A(105) = cont_VV(wf(:,43),wf(:,119)) * den(90)
  A(106) = cont_VV(wf(:,43),wf(:,120)) * den(49)
  A(107) = cont_VV(wf(:,41),wf(:,121)) * den(91)
  A(108) = cont_VV(wf(:,42),wf(:,121)) * den(92)
  A(109) = cont_VV(wf(:,48),wf(:,119)) * den(93)
  A(110) = cont_VV(wf(:,48),wf(:,120)) * den(55)
  A(111) = cont_VV(wf(:,41),wf(:,122)) * den(43)
  A(112) = cont_VV(wf(:,42),wf(:,122)) * den(46)
  A(113) = cont_QA(wf(:,5),wf(:,124)) * den(5)
  A(114) = cont_QA(wf(:,8),wf(:,126)) * den(8)
  A(115) = cont_QA(wf(:,13),wf(:,127)) * den(12)
  A(116) = cont_QA(wf(:,16),wf(:,129)) * den(15)
  A(117) = cont_QA(wf(:,18),wf(:,126)) * den(16)
  A(118) = cont_QA(wf(:,19),wf(:,129)) * den(17)
  A(119) = cont_QA(wf(:,22),wf(:,129)) * den(19)
  A(120) = cont_QA(wf(:,24),wf(:,125)) * den(22)
  A(121) = cont_VV(wf(:,130),wf(:,131)) * den(97)
  A(122) = cont_QA(wf(:,133),wf(:,134)) * den(101)
  A(123) = cont_QA(wf(:,135),wf(:,136)) * den(104)
  A(124) = cont_QA(wf(:,58),wf(:,137)) * den(106)
  A(125) = cont_QA(wf(:,9),wf(:,139)) * den(108)
  A(126) = cont_QA(wf(:,58),wf(:,140)) * den(109)
  A(127) = cont_VV(wf(:,131),wf(:,141)) * den(112)
  A(128) = cont_QA(wf(:,143),wf(:,144)) * den(116)
  A(129) = cont_QA(wf(:,145),wf(:,146)) * den(119)
  A(130) = cont_QA(wf(:,86),wf(:,147)) * den(120)
  A(131) = cont_QA(wf(:,86),wf(:,148)) * den(121)
  A(132) = cont_QA(wf(:,17),wf(:,150)) * den(123)
  A(133) = cont_QA(wf(:,61),wf(:,137)) * den(124)
  A(134) = cont_QA(wf(:,9),wf(:,152)) * den(126)
  A(135) = cont_QA(wf(:,61),wf(:,140)) * den(127)
  A(136) = cont_QA(wf(:,90),wf(:,147)) * den(128)
  A(137) = cont_QA(wf(:,90),wf(:,148)) * den(129)
  A(138) = cont_QA(wf(:,17),wf(:,154)) * den(131)
  A(139) = cont_QA(wf(:,92),wf(:,147)) * den(132)
  A(140) = cont_QA(wf(:,24),wf(:,137)) * den(133)
  A(141) = cont_QA(wf(:,92),wf(:,148)) * den(134)
  A(142) = cont_QA(wf(:,9),wf(:,155)) * den(135)
  A(143) = cont_QA(wf(:,157),wf(:,158)) * den(139)
  A(144) = cont_VV(wf(:,159),wf(:,160)) * den(143)
  A(145) = cont_QA(wf(:,135),wf(:,161)) * den(145)
  A(146) = cont_QA(wf(:,31),wf(:,163)) * den(147)
  A(147) = cont_QA(wf(:,73),wf(:,164)) * den(149)
  A(148) = cont_QA(wf(:,73),wf(:,165)) * den(150)
  A(149) = cont_QA(wf(:,167),wf(:,168)) * den(154)
  A(150) = cont_VV(wf(:,160),wf(:,169)) * den(157)
  A(151) = cont_QA(wf(:,145),wf(:,170)) * den(159)
  A(152) = cont_QA(wf(:,86),wf(:,171)) * den(160)
  A(153) = cont_QA(wf(:,86),wf(:,172)) * den(161)
  A(154) = cont_QA(wf(:,36),wf(:,150)) * den(162)
  A(155) = cont_QA(wf(:,31),wf(:,174)) * den(164)
  A(156) = cont_QA(wf(:,74),wf(:,164)) * den(165)
  A(157) = cont_QA(wf(:,74),wf(:,165)) * den(166)
  A(158) = cont_QA(wf(:,90),wf(:,171)) * den(167)
  A(159) = cont_QA(wf(:,90),wf(:,172)) * den(168)
  A(160) = cont_QA(wf(:,36),wf(:,154)) * den(169)
  A(161) = cont_QA(wf(:,92),wf(:,171)) * den(170)
  A(162) = cont_QA(wf(:,92),wf(:,172)) * den(171)
  A(163) = cont_QA(wf(:,31),wf(:,175)) * den(172)
  A(164) = cont_QA(wf(:,39),wf(:,164)) * den(173)
  A(165) = cont_VV(wf(:,41),wf(:,176)) * den(174)
  A(166) = cont_VV(wf(:,40),wf(:,177)) * den(175)
  A(167) = cont_VV(wf(:,41),wf(:,178)) * den(176)
  A(168) = cont_VV(wf(:,42),wf(:,176)) * den(177)
  A(169) = cont_VV(wf(:,40),wf(:,179)) * den(178)
  A(170) = cont_VV(wf(:,40),wf(:,180)) * den(179)
  A(171) = cont_VV(wf(:,43),wf(:,181)) * den(180)
  A(172) = cont_VV(wf(:,44),wf(:,182)) * den(181)
  A(173) = cont_VV(wf(:,44),wf(:,183)) * den(182)
  A(174) = cont_VV(wf(:,43),wf(:,184)) * den(183)
  A(175) = cont_VV(wf(:,43),wf(:,185)) * den(184)
  A(176) = cont_VV(wf(:,45),wf(:,182)) * den(185)
  A(177) = cont_VV(wf(:,48),wf(:,181)) * den(186)
  A(178) = cont_VV(wf(:,44),wf(:,188)) * den(188)
  A(179) = cont_VV(wf(:,44),wf(:,189)) * den(189)
  A(180) = cont_VV(wf(:,41),wf(:,190)) * den(190)
  A(181) = cont_VV(wf(:,41),wf(:,191)) * den(191)
  A(182) = cont_VV(wf(:,49),wf(:,177)) * den(192)
  A(183) = cont_VV(wf(:,42),wf(:,191)) * den(193)
  A(184) = cont_VV(wf(:,48),wf(:,184)) * den(194)
  A(185) = cont_VV(wf(:,42),wf(:,190)) * den(195)
  A(186) = cont_VV(wf(:,48),wf(:,185)) * den(196)
  A(187) = cont_VV(wf(:,45),wf(:,188)) * den(197)
  A(188) = cont_VV(wf(:,49),wf(:,180)) * den(198)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(188)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(7)-A(8))*f(1))/2._/**/REALKIND+((-A(15)-A(16))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(17)-A(18) &
       -A(22)-A(23))*f(5))/2._/**/REALKIND+((-A(9)-A(10)-A(11)-A(12)-A(13)-A(14)-A(19)-A(20)-A(21)-A(24))*f(5))/6._/**/REALKIND
  M1(2) = ((A(7)+A(8))*f(1))/6._/**/REALKIND+((A(15)+A(16))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(17)+A(18) &
       +A(22)+A(23))*f(5))/6._/**/REALKIND+((A(9)+A(10)+A(11)+A(12)+A(13)+A(14)+A(19)+A(20)+A(21)+A(24))*f(5))/2._/**/REALKIND

  M2(1) = ((A(139)+A(140)+A(141)+A(142))*f(2))/2._/**/REALKIND+((A(161)+A(162)+A(163)+A(164))*f(2))/6._/**/REALKIND+((-A(53)-A(95) &
       -A(96)-A(100))*f(3))/6._/**/REALKIND+((-A(37)-A(77)-A(119)-A(120))*f(3))/2._/**/REALKIND+((-A(36) &
       -A(76))*f(4))/2._/**/REALKIND+((-A(52)-A(101))*f(4))/6._/**/REALKIND+((A(143)+A(144)+A(145)+A(146)+A(147)+A(148)+A(149) &
       +A(150)+A(151)+A(152)+A(153)+A(154)+A(155)+A(156)+A(157)+A(158)+A(159)+A(160)+A(171)+A(172)+A(173)+A(174)+A(175)+A(176) &
       +A(177)+A(178)+A(179)+A(184)+A(186)+A(187))*f(6))/6._/**/REALKIND+((A(121)+A(122)+A(123)+A(124)+A(125)+A(126)+A(127)+A(128) &
       +A(129)+A(130)+A(131)+A(132)+A(133)+A(134)+A(135)+A(136)+A(137)+A(138)+A(165)+A(166)+A(167)+A(168)+A(169)+A(170)+A(180) &
       +A(181)+A(182)+A(183)+A(185)+A(188))*f(6))/2._/**/REALKIND+((-A(41)-A(43)-A(49)-A(51)-A(59)-A(61)-A(67)-A(68)-A(83)-A(85) &
       -A(89)-A(90)-A(91)-A(92)-A(93)-A(94)-A(97)-A(99)-A(105)-A(109))*f(7))/6._/**/REALKIND+((-A(25)-A(27)-A(33)-A(35)-A(57) &
       -A(62)-A(65)-A(66)-A(73)-A(75)-A(81)-A(84)-A(107)-A(108)-A(113)-A(114)-A(115)-A(116)-A(117)-A(118))*f(7))/2._/**/REALKIND &
       +((-A(42)-A(44)-A(46)-A(47)-A(50)-A(54)-A(55)-A(56)-A(60)-A(64)-A(69)-A(71)-A(87)-A(88)-A(98)-A(102)-A(103)-A(104)-A(106) &
       -A(110))*f(8))/6._/**/REALKIND+((-A(26)-A(28)-A(29)-A(31)-A(34)-A(38)-A(39)-A(40)-A(58)-A(63)-A(70)-A(72)-A(74)-A(78)-A(79) &
       -A(80)-A(82)-A(86)-A(111)-A(112))*f(8))/2._/**/REALKIND+((A(30)+A(32))*f(9))/2._/**/REALKIND+((A(45) &
       +A(48))*f(9))/6._/**/REALKIND
  M2(2) = ((-A(139)-A(140)-A(141)-A(142))*f(2))/6._/**/REALKIND+((-A(161)-A(162)-A(163)-A(164))*f(2))/2._/**/REALKIND+((A(53) &
       +A(95)+A(96)+A(100))*f(3))/2._/**/REALKIND+((A(37)+A(77)+A(119)+A(120))*f(3))/6._/**/REALKIND+((A(36) &
       +A(76))*f(4))/6._/**/REALKIND+((A(52)+A(101))*f(4))/2._/**/REALKIND+((-A(143)-A(144)-A(145)-A(146)-A(147)-A(148)-A(149) &
       -A(150)-A(151)-A(152)-A(153)-A(154)-A(155)-A(156)-A(157)-A(158)-A(159)-A(160)-A(171)-A(172)-A(173)-A(174)-A(175)-A(176) &
       -A(177)-A(178)-A(179)-A(184)-A(186)-A(187))*f(6))/2._/**/REALKIND+((-A(121)-A(122)-A(123)-A(124)-A(125)-A(126)-A(127) &
       -A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(134)-A(135)-A(136)-A(137)-A(138)-A(165)-A(166)-A(167)-A(168)-A(169)-A(170) &
       -A(180)-A(181)-A(182)-A(183)-A(185)-A(188))*f(6))/6._/**/REALKIND+((A(41)+A(43)+A(49)+A(51)+A(59)+A(61)+A(67)+A(68)+A(83) &
       +A(85)+A(89)+A(90)+A(91)+A(92)+A(93)+A(94)+A(97)+A(99)+A(105)+A(109))*f(7))/2._/**/REALKIND+((A(25)+A(27)+A(33)+A(35)+A(57) &
       +A(62)+A(65)+A(66)+A(73)+A(75)+A(81)+A(84)+A(107)+A(108)+A(113)+A(114)+A(115)+A(116)+A(117)+A(118))*f(7))/6._/**/REALKIND &
       +((A(42)+A(44)+A(46)+A(47)+A(50)+A(54)+A(55)+A(56)+A(60)+A(64)+A(69)+A(71)+A(87)+A(88)+A(98)+A(102)+A(103)+A(104)+A(106) &
       +A(110))*f(8))/2._/**/REALKIND+((A(26)+A(28)+A(29)+A(31)+A(34)+A(38)+A(39)+A(40)+A(58)+A(63)+A(70)+A(72)+A(74)+A(78)+A(79) &
       +A(80)+A(82)+A(86)+A(111)+A(112))*f(8))/6._/**/REALKIND+((-A(30)-A(32))*f(9))/6._/**/REALKIND+((-A(45) &
       -A(48))*f(9))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppzwjj_uuuxdxzw_1_/**/REALKIND
