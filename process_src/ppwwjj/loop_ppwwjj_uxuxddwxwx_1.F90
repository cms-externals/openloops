
module ol_colourmatrix_ppwwjj_uxuxddwxwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_uxuxddwxwx_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_uxuxddwxwx_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwwjj_uxuxddwxwx_1_/**/REALKIND

module ol_loop_ppwwjj_uxuxddwxwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(9), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:188)
  ! denominators
  complex(REALKIND), save :: den(204)
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
    f(1) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(2) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(3) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(4) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(5) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(6) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(7) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(8) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(9) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(5), 27*CI*f(5), 18*f(6), 54*f(6), f(7), 3*f(7), 6*f(7), 8*f(7), 10*f(7), 18*f(7), 21*f(7), 24*f(7), 54*f(7), 3*f(8) &
    , 9*f(8), 3*f(9), 9*f(9) ]
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
  complex(REALKIND) :: A(128)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_AW_Q(wf(:,0),wf(:,-4),wf(:,1))
  call vert_AW_Q(wf(:,-1),wf(:,-5),wf(:,2))
  call prop_A_Q(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,34),ZERO,0_intkind1,wf(:,4))
  call vert_QA_V(wf(:,-2),wf(:,3),wf(:,5))
  call vert_QA_V(wf(:,-3),wf(:,4),wf(:,6))
  call vert_QA_V(wf(:,-2),wf(:,4),wf(:,7))
  call vert_QA_V(wf(:,-3),wf(:,3),wf(:,8))
  call vert_WQ_A(wf(:,-5),wf(:,-2),wf(:,9))
  call prop_Q_A(wf(:,9),Q(:,36),ZERO,0_intkind1,wf(:,10))
  call vert_QA_V(wf(:,10),wf(:,-1),wf(:,11))
  call vert_WQ_A(wf(:,-5),wf(:,-3),wf(:,12))
  call prop_Q_A(wf(:,12),Q(:,40),ZERO,0_intkind1,wf(:,13))
  call vert_QA_V(wf(:,13),wf(:,-1),wf(:,14))
  call vert_AW_Q(wf(:,0),wf(:,-5),wf(:,15))
  call vert_AW_Q(wf(:,-1),wf(:,-4),wf(:,16))
  call prop_A_Q(wf(:,15),Q(:,33),ZERO,0_intkind1,wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,18),ZERO,0_intkind1,wf(:,18))
  call vert_QA_V(wf(:,-2),wf(:,17),wf(:,19))
  call vert_QA_V(wf(:,-3),wf(:,18),wf(:,20))
  call vert_QA_V(wf(:,-2),wf(:,18),wf(:,21))
  call vert_QA_V(wf(:,-3),wf(:,17),wf(:,22))
  call vert_QA_V(wf(:,10),wf(:,0),wf(:,23))
  call vert_QA_V(wf(:,13),wf(:,0),wf(:,24))
  call vert_WQ_A(wf(:,-4),wf(:,-2),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,20),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,26),wf(:,-1),wf(:,27))
  call vert_QA_V(wf(:,26),wf(:,0),wf(:,28))
  call vert_WQ_A(wf(:,-4),wf(:,-3),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,24),ZERO,0_intkind1,wf(:,30))
  call vert_QA_V(wf(:,30),wf(:,-1),wf(:,31))
  call vert_QA_V(wf(:,30),wf(:,0),wf(:,32))
  call counter_QA_V(wf(:,-3),wf(:,4),wf(:,33))
  call counter_QA_V(wf(:,-3),wf(:,3),wf(:,34))
  call counter_WQ_A(wf(:,-5),wf(:,-3),wf(:,35))
  call prop_Q_A(wf(:,35),Q(:,40),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,36),wf(:,-1),wf(:,37))
  call counter_QA_V(wf(:,-3),wf(:,18),wf(:,38))
  call counter_QA_V(wf(:,-3),wf(:,17),wf(:,39))
  call vert_QA_V(wf(:,36),wf(:,0),wf(:,40))
  call counter_WQ_A(wf(:,-4),wf(:,-3),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,24),ZERO,0_intkind1,wf(:,42))
  call vert_QA_V(wf(:,42),wf(:,-1),wf(:,43))
  call vert_QA_V(wf(:,42),wf(:,0),wf(:,44))
  call counter_QA_V(wf(:,-2),wf(:,4),wf(:,45))
  call counter_QA_V(wf(:,-2),wf(:,3),wf(:,46))
  call counter_WQ_A(wf(:,-5),wf(:,-2),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,36),ZERO,0_intkind1,wf(:,48))
  call vert_QA_V(wf(:,48),wf(:,-1),wf(:,49))
  call counter_QA_V(wf(:,-2),wf(:,18),wf(:,50))
  call counter_QA_V(wf(:,-2),wf(:,17),wf(:,51))
  call vert_QA_V(wf(:,48),wf(:,0),wf(:,52))
  call counter_WQ_A(wf(:,-4),wf(:,-2),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,20),ZERO,0_intkind1,wf(:,54))
  call vert_QA_V(wf(:,54),wf(:,-1),wf(:,55))
  call vert_QA_V(wf(:,54),wf(:,0),wf(:,56))
  call counter_QA_V(wf(:,10),wf(:,-1),wf(:,57))
  call counter_QA_V(wf(:,13),wf(:,-1),wf(:,58))
  call counter_AW_Q(wf(:,-1),wf(:,-5),wf(:,59))
  call prop_A_Q(wf(:,59),Q(:,34),ZERO,0_intkind1,wf(:,60))
  call vert_QA_V(wf(:,-3),wf(:,60),wf(:,61))
  call vert_QA_V(wf(:,-2),wf(:,60),wf(:,62))
  call counter_QA_V(wf(:,26),wf(:,-1),wf(:,63))
  call counter_QA_V(wf(:,30),wf(:,-1),wf(:,64))
  call counter_AW_Q(wf(:,-1),wf(:,-4),wf(:,65))
  call prop_A_Q(wf(:,65),Q(:,18),ZERO,0_intkind1,wf(:,66))
  call vert_QA_V(wf(:,-3),wf(:,66),wf(:,67))
  call vert_QA_V(wf(:,-2),wf(:,66),wf(:,68))
  call counter_QA_V(wf(:,10),wf(:,0),wf(:,69))
  call counter_QA_V(wf(:,13),wf(:,0),wf(:,70))
  call counter_AW_Q(wf(:,0),wf(:,-5),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,33),ZERO,0_intkind1,wf(:,72))
  call vert_QA_V(wf(:,-2),wf(:,72),wf(:,73))
  call vert_QA_V(wf(:,-3),wf(:,72),wf(:,74))
  call counter_QA_V(wf(:,26),wf(:,0),wf(:,75))
  call counter_QA_V(wf(:,30),wf(:,0),wf(:,76))
  call counter_AW_Q(wf(:,0),wf(:,-4),wf(:,77))
  call prop_A_Q(wf(:,77),Q(:,17),ZERO,0_intkind1,wf(:,78))
  call vert_QA_V(wf(:,-2),wf(:,78),wf(:,79))
  call vert_QA_V(wf(:,-3),wf(:,78),wf(:,80))
  call counter_A_Q(ctqq,wf(:,3),Q(:,17),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,17),ZERO,0_intkind1,wf(:,82))
  call vert_QA_V(wf(:,-2),wf(:,82),wf(:,83))
  call vert_QA_V(wf(:,-3),wf(:,82),wf(:,84))
  call counter_A_Q(ctqq,wf(:,4),Q(:,34),wf(:,85))
  call prop_A_Q(wf(:,85),Q(:,34),ZERO,0_intkind1,wf(:,86))
  call vert_QA_V(wf(:,-2),wf(:,86),wf(:,87))
  call vert_QA_V(wf(:,-3),wf(:,86),wf(:,88))
  call counter_V_V(ctGG,wf(:,5),Q(:,21),wf(:,89))
  call counter_V_V(ctGG,wf(:,7),Q(:,38),wf(:,90))
  call counter_Q_A(ctqq,wf(:,10),Q(:,36),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,36),ZERO,0_intkind1,wf(:,92))
  call vert_QA_V(wf(:,92),wf(:,-1),wf(:,93))
  call counter_V_V(ctGG,wf(:,11),Q(:,38),wf(:,94))
  call counter_V_V(ctGG,wf(:,14),Q(:,42),wf(:,95))
  call counter_Q_A(ctqq,wf(:,13),Q(:,40),wf(:,96))
  call prop_Q_A(wf(:,96),Q(:,40),ZERO,0_intkind1,wf(:,97))
  call vert_QA_V(wf(:,97),wf(:,-1),wf(:,98))
  call counter_A_Q(ctqq,wf(:,17),Q(:,33),wf(:,99))
  call prop_A_Q(wf(:,99),Q(:,33),ZERO,0_intkind1,wf(:,100))
  call vert_QA_V(wf(:,-2),wf(:,100),wf(:,101))
  call vert_QA_V(wf(:,-3),wf(:,100),wf(:,102))
  call counter_A_Q(ctqq,wf(:,18),Q(:,18),wf(:,103))
  call prop_A_Q(wf(:,103),Q(:,18),ZERO,0_intkind1,wf(:,104))
  call vert_QA_V(wf(:,-2),wf(:,104),wf(:,105))
  call vert_QA_V(wf(:,-3),wf(:,104),wf(:,106))
  call counter_V_V(ctGG,wf(:,19),Q(:,37),wf(:,107))
  call counter_V_V(ctGG,wf(:,21),Q(:,22),wf(:,108))
  call vert_QA_V(wf(:,92),wf(:,0),wf(:,109))
  call counter_V_V(ctGG,wf(:,23),Q(:,37),wf(:,110))
  call counter_V_V(ctGG,wf(:,24),Q(:,41),wf(:,111))
  call vert_QA_V(wf(:,97),wf(:,0),wf(:,112))
  call counter_Q_A(ctqq,wf(:,26),Q(:,20),wf(:,113))
  call prop_Q_A(wf(:,113),Q(:,20),ZERO,0_intkind1,wf(:,114))
  call vert_QA_V(wf(:,114),wf(:,-1),wf(:,115))
  call counter_V_V(ctGG,wf(:,27),Q(:,22),wf(:,116))
  call vert_QA_V(wf(:,114),wf(:,0),wf(:,117))
  call counter_V_V(ctGG,wf(:,28),Q(:,21),wf(:,118))
  call counter_V_V(ctGG,wf(:,31),Q(:,26),wf(:,119))
  call counter_Q_A(ctqq,wf(:,30),Q(:,24),wf(:,120))
  call prop_Q_A(wf(:,120),Q(:,24),ZERO,0_intkind1,wf(:,121))
  call vert_QA_V(wf(:,121),wf(:,-1),wf(:,122))
  call counter_V_V(ctGG,wf(:,32),Q(:,25),wf(:,123))
  call vert_QA_V(wf(:,121),wf(:,0),wf(:,124))
  call vert_AV_Q(wf(:,-1),wf(:,5),wf(:,125))
  call prop_A_Q(wf(:,125),Q(:,23),ZERO,0_intkind1,wf(:,126))
  call vert_VQ_A(wf(:,5),wf(:,-3),wf(:,127))
  call prop_Q_A(wf(:,127),Q(:,29),ZERO,0_intkind1,wf(:,128))
  call vert_AV_Q(wf(:,-1),wf(:,8),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,27),ZERO,0_intkind1,wf(:,130))
  call vert_VQ_A(wf(:,8),wf(:,-2),wf(:,131))
  call prop_Q_A(wf(:,131),Q(:,29),ZERO,0_intkind1,wf(:,132))
  call vert_AV_Q(wf(:,0),wf(:,21),wf(:,133))
  call prop_A_Q(wf(:,133),Q(:,23),ZERO,0_intkind1,wf(:,134))
  call vert_AV_Q(wf(:,0),wf(:,20),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,27),ZERO,0_intkind1,wf(:,136))
  call vert_VQ_A(wf(:,21),wf(:,-3),wf(:,137))
  call prop_Q_A(wf(:,137),Q(:,30),ZERO,0_intkind1,wf(:,138))
  call vert_VQ_A(wf(:,20),wf(:,-2),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,30),ZERO,0_intkind1,wf(:,140))
  call vert_AV_Q(wf(:,-1),wf(:,28),wf(:,141))
  call prop_A_Q(wf(:,141),Q(:,23),ZERO,0_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,0),wf(:,27),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,23),ZERO,0_intkind1,wf(:,144))
  call vert_VQ_A(wf(:,28),wf(:,-3),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,29),ZERO,0_intkind1,wf(:,146))
  call vert_VQ_A(wf(:,27),wf(:,-3),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,30),ZERO,0_intkind1,wf(:,148))
  call vert_AV_Q(wf(:,-1),wf(:,32),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,27),ZERO,0_intkind1,wf(:,150))
  call vert_AV_Q(wf(:,0),wf(:,31),wf(:,151))
  call prop_A_Q(wf(:,151),Q(:,27),ZERO,0_intkind1,wf(:,152))
  call vert_VQ_A(wf(:,32),wf(:,-2),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,29),ZERO,0_intkind1,wf(:,154))
  call vert_VQ_A(wf(:,31),wf(:,-2),wf(:,155))
  call prop_Q_A(wf(:,155),Q(:,30),ZERO,0_intkind1,wf(:,156))
  call vert_AV_Q(wf(:,-1),wf(:,19),wf(:,157))
  call prop_A_Q(wf(:,157),Q(:,39),ZERO,0_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,19),wf(:,-3),wf(:,159))
  call prop_Q_A(wf(:,159),Q(:,45),ZERO,0_intkind1,wf(:,160))
  call vert_AV_Q(wf(:,-1),wf(:,22),wf(:,161))
  call prop_A_Q(wf(:,161),Q(:,43),ZERO,0_intkind1,wf(:,162))
  call vert_VQ_A(wf(:,22),wf(:,-2),wf(:,163))
  call prop_Q_A(wf(:,163),Q(:,45),ZERO,0_intkind1,wf(:,164))
  call vert_AV_Q(wf(:,0),wf(:,7),wf(:,165))
  call prop_A_Q(wf(:,165),Q(:,39),ZERO,0_intkind1,wf(:,166))
  call vert_AV_Q(wf(:,0),wf(:,6),wf(:,167))
  call prop_A_Q(wf(:,167),Q(:,43),ZERO,0_intkind1,wf(:,168))
  call vert_VQ_A(wf(:,7),wf(:,-3),wf(:,169))
  call prop_Q_A(wf(:,169),Q(:,46),ZERO,0_intkind1,wf(:,170))
  call vert_VQ_A(wf(:,6),wf(:,-2),wf(:,171))
  call prop_Q_A(wf(:,171),Q(:,46),ZERO,0_intkind1,wf(:,172))
  call vert_AV_Q(wf(:,-1),wf(:,23),wf(:,173))
  call prop_A_Q(wf(:,173),Q(:,39),ZERO,0_intkind1,wf(:,174))
  call vert_AV_Q(wf(:,0),wf(:,11),wf(:,175))
  call prop_A_Q(wf(:,175),Q(:,39),ZERO,0_intkind1,wf(:,176))
  call vert_VQ_A(wf(:,23),wf(:,-3),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,45),ZERO,0_intkind1,wf(:,178))
  call vert_VQ_A(wf(:,11),wf(:,-3),wf(:,179))
  call prop_Q_A(wf(:,179),Q(:,46),ZERO,0_intkind1,wf(:,180))
  call vert_AV_Q(wf(:,-1),wf(:,24),wf(:,181))
  call prop_A_Q(wf(:,181),Q(:,43),ZERO,0_intkind1,wf(:,182))
  call vert_AV_Q(wf(:,0),wf(:,14),wf(:,183))
  call prop_A_Q(wf(:,183),Q(:,43),ZERO,0_intkind1,wf(:,184))
  call vert_VQ_A(wf(:,24),wf(:,-2),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,45),ZERO,0_intkind1,wf(:,186))
  call vert_VQ_A(wf(:,14),wf(:,-2),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,46),ZERO,0_intkind1,wf(:,188))

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
  den(2) = 1 / (Q(5,34))
  den(3) = 1 / (Q(5,21))
  den(6) = 1 / (Q(5,38))
  den(9) = 1 / (Q(5,36))
  den(12) = 1 / (Q(5,40))
  den(13) = 1 / (Q(5,42))
  den(16) = 1 / (Q(5,33))
  den(17) = 1 / (Q(5,18))
  den(18) = 1 / (Q(5,37))
  den(21) = 1 / (Q(5,22))
  den(26) = 1 / (Q(5,41))
  den(29) = 1 / (Q(5,20))
  den(36) = 1 / (Q(5,24))
  den(37) = 1 / (Q(5,26))
  den(40) = 1 / (Q(5,25))
  den(121) = 1 / (Q(5,23))
  den(123) = 1 / (Q(5,29))
  den(125) = 1 / (Q(5,27))
  den(130) = 1 / (Q(5,30))
  den(141) = 1 / (Q(5,39))
  den(143) = 1 / (Q(5,45))
  den(145) = 1 / (Q(5,43))
  den(150) = 1 / (Q(5,46))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(10) = den(6)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(12)*den(13)
  den(15) = den(1)*den(14)
  den(19) = den(16)*den(18)
  den(20) = den(17)*den(19)
  den(22) = den(17)*den(21)
  den(23) = den(16)*den(22)
  den(24) = den(9)*den(18)
  den(25) = den(17)*den(24)
  den(27) = den(12)*den(26)
  den(28) = den(17)*den(27)
  den(30) = den(21)*den(29)
  den(31) = den(16)*den(30)
  den(32) = den(3)*den(29)
  den(33) = den(2)*den(32)
  den(34) = den(12)*den(32)
  den(35) = den(27)*den(29)
  den(38) = den(36)*den(37)
  den(39) = den(16)*den(38)
  den(41) = den(36)*den(40)
  den(42) = den(2)*den(41)
  den(43) = den(24)*den(36)
  den(44) = den(9)*den(41)
  den(45) = den(1)*den(40)
  den(46) = den(2)*den(45)
  den(47) = den(2)*den(13)
  den(48) = den(1)*den(47)
  den(49) = den(16)*den(26)
  den(50) = den(17)*den(49)
  den(51) = den(17)*den(37)
  den(52) = den(16)*den(51)
  den(53) = den(9)*den(45)
  den(54) = den(4)*den(12)
  den(55) = den(29)*den(49)
  den(56) = den(19)*den(36)
  den(57) = den(9)*den(51)
  den(58) = den(12)*den(22)
  den(59) = den(29)*den(47)
  den(60) = den(14)*den(29)
  den(61) = den(12)*den(30)
  den(62) = den(7)*den(36)
  den(63) = den(9)*den(38)
  den(64) = den(10)*den(36)
  den(65) = den(1)**2
  den(66) = den(47)*den(65)
  den(67) = den(7)*den(65)
  den(68) = den(2)**2
  den(69) = den(45)*den(68)
  den(70) = den(4)*den(68)
  den(71) = den(4)*den(47)
  den(72) = den(7)*den(45)
  den(73) = den(10)*den(65)
  den(74) = den(9)**2
  den(75) = den(45)*den(74)
  den(76) = den(10)*den(45)
  den(77) = den(14)*den(65)
  den(78) = den(4)*den(14)
  den(79) = den(12)**2
  den(80) = den(4)*den(79)
  den(81) = den(16)**2
  den(82) = den(51)*den(81)
  den(83) = den(22)*den(81)
  den(84) = den(17)**2
  den(85) = den(49)*den(84)
  den(86) = den(19)*den(84)
  den(87) = den(19)*den(51)
  den(88) = den(22)*den(49)
  den(89) = den(51)*den(74)
  den(90) = den(24)*den(51)
  den(91) = den(24)*den(84)
  den(92) = den(22)*den(27)
  den(93) = den(22)*den(79)
  den(94) = den(27)*den(84)
  den(95) = den(30)*den(81)
  den(96) = den(29)**2
  den(97) = den(49)*den(96)
  den(98) = den(30)*den(49)
  den(99) = den(47)*den(96)
  den(100) = den(32)*den(47)
  den(101) = den(32)*den(68)
  den(102) = den(14)*den(32)
  den(103) = den(27)*den(30)
  den(104) = den(14)*den(96)
  den(105) = den(30)*den(79)
  den(106) = den(27)*den(96)
  den(107) = den(32)*den(79)
  den(108) = den(38)*den(81)
  den(109) = den(19)*den(38)
  den(110) = den(36)**2
  den(111) = den(19)*den(110)
  den(112) = den(7)*den(41)
  den(113) = den(7)*den(110)
  den(114) = den(41)*den(68)
  den(115) = den(24)*den(38)
  den(116) = den(10)*den(41)
  den(117) = den(38)*den(74)
  den(118) = den(10)*den(110)
  den(119) = den(41)*den(74)
  den(120) = den(24)*den(110)
  den(122) = den(4)*den(121)
  den(124) = den(4)*den(123)
  den(126) = den(45)*den(125)
  den(127) = den(45)*den(123)
  den(128) = den(22)*den(121)
  den(129) = den(51)*den(125)
  den(131) = den(22)*den(130)
  den(132) = den(51)*den(130)
  den(133) = den(32)*den(121)
  den(134) = den(30)*den(121)
  den(135) = den(32)*den(123)
  den(136) = den(30)*den(130)
  den(137) = den(41)*den(125)
  den(138) = den(38)*den(125)
  den(139) = den(41)*den(123)
  den(140) = den(38)*den(130)
  den(142) = den(19)*den(141)
  den(144) = den(19)*den(143)
  den(146) = den(49)*den(145)
  den(147) = den(49)*den(143)
  den(148) = den(7)*den(141)
  den(149) = den(47)*den(145)
  den(151) = den(7)*den(150)
  den(152) = den(47)*den(150)
  den(153) = den(24)*den(141)
  den(154) = den(10)*den(141)
  den(155) = den(24)*den(143)
  den(156) = den(10)*den(150)
  den(157) = den(27)*den(145)
  den(158) = den(14)*den(145)
  den(159) = den(27)*den(143)
  den(160) = den(14)*den(150)
  den(161) = den(1)*den(2)
  den(162) = den(1)*den(9)
  den(163) = den(1)*den(12)
  den(164) = den(16)*den(17)
  den(165) = den(9)*den(17)
  den(166) = den(12)*den(17)
  den(167) = den(16)*den(29)
  den(168) = den(2)*den(29)
  den(169) = den(12)*den(29)
  den(170) = den(16)*den(36)
  den(171) = den(2)*den(36)
  den(172) = den(9)*den(36)
  den(173) = den(2)*den(124)
  den(174) = den(2)*den(127)
  den(175) = den(1)*den(151)
  den(176) = den(1)*den(152)
  den(177) = den(9)*den(126)
  den(178) = den(1)*den(156)
  den(179) = den(12)*den(122)
  den(180) = den(1)*den(160)
  den(181) = den(17)*den(144)
  den(182) = den(17)*den(147)
  den(183) = den(16)*den(131)
  den(184) = den(16)*den(132)
  den(185) = den(17)*den(155)
  den(186) = den(9)*den(129)
  den(187) = den(17)*den(159)
  den(188) = den(12)*den(128)
  den(189) = den(29)*den(146)
  den(190) = den(16)*den(136)
  den(191) = den(2)*den(135)
  den(192) = den(29)*den(149)
  den(193) = den(12)*den(133)
  den(194) = den(29)*den(157)
  den(195) = den(12)*den(134)
  den(196) = den(29)*den(158)
  den(197) = den(36)*den(142)
  den(198) = den(16)*den(140)
  den(199) = den(2)*den(139)
  den(200) = den(36)*den(148)
  den(201) = den(36)*den(153)
  den(202) = den(9)*den(137)
  den(203) = den(36)*den(154)
  den(204) = den(9)*den(138)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(128)

  A(1) = cont_VV(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(8)
  A(3) = cont_VV(wf(:,8),wf(:,11)) * den(11)
  A(4) = cont_VV(wf(:,5),wf(:,14)) * den(15)
  A(5) = cont_VV(wf(:,19),wf(:,20)) * den(20)
  A(6) = cont_VV(wf(:,21),wf(:,22)) * den(23)
  A(7) = cont_VV(wf(:,20),wf(:,23)) * den(25)
  A(8) = cont_VV(wf(:,21),wf(:,24)) * den(28)
  A(9) = cont_VV(wf(:,22),wf(:,27)) * den(31)
  A(10) = cont_VV(wf(:,6),wf(:,28)) * den(33)
  A(11) = cont_VV(wf(:,14),wf(:,28)) * den(34)
  A(12) = cont_VV(wf(:,24),wf(:,27)) * den(35)
  A(13) = cont_VV(wf(:,19),wf(:,31)) * den(39)
  A(14) = cont_VV(wf(:,7),wf(:,32)) * den(42)
  A(15) = cont_VV(wf(:,23),wf(:,31)) * den(43)
  A(16) = cont_VV(wf(:,11),wf(:,32)) * den(44)

  A(17) = cont_VV(wf(:,5),wf(:,33)) * den(5)
  A(18) = cont_VV(wf(:,7),wf(:,34)) * den(8)
  A(19) = cont_VV(wf(:,11),wf(:,34)) * den(11)
  A(20) = cont_VV(wf(:,5),wf(:,37)) * den(15)
  A(21) = cont_VV(wf(:,19),wf(:,38)) * den(20)
  A(22) = cont_VV(wf(:,21),wf(:,39)) * den(23)
  A(23) = cont_VV(wf(:,23),wf(:,38)) * den(25)
  A(24) = cont_VV(wf(:,21),wf(:,40)) * den(28)
  A(25) = cont_VV(wf(:,27),wf(:,39)) * den(31)
  A(26) = cont_VV(wf(:,28),wf(:,33)) * den(33)
  A(27) = cont_VV(wf(:,28),wf(:,37)) * den(34)
  A(28) = cont_VV(wf(:,27),wf(:,40)) * den(35)
  A(29) = cont_VV(wf(:,19),wf(:,43)) * den(39)
  A(30) = cont_VV(wf(:,7),wf(:,44)) * den(42)
  A(31) = cont_VV(wf(:,23),wf(:,43)) * den(43)
  A(32) = cont_VV(wf(:,11),wf(:,44)) * den(44)
  A(33) = cont_VV(wf(:,8),wf(:,45)) * den(46)
  A(34) = cont_VV(wf(:,6),wf(:,46)) * den(48)
  A(35) = cont_VV(wf(:,14),wf(:,46)) * den(15)
  A(36) = cont_VV(wf(:,8),wf(:,49)) * den(11)
  A(37) = cont_VV(wf(:,22),wf(:,50)) * den(50)
  A(38) = cont_VV(wf(:,20),wf(:,51)) * den(52)
  A(39) = cont_VV(wf(:,24),wf(:,50)) * den(28)
  A(40) = cont_VV(wf(:,20),wf(:,52)) * den(25)
  A(41) = cont_VV(wf(:,31),wf(:,51)) * den(39)
  A(42) = cont_VV(wf(:,32),wf(:,45)) * den(42)
  A(43) = cont_VV(wf(:,32),wf(:,49)) * den(44)
  A(44) = cont_VV(wf(:,31),wf(:,52)) * den(43)
  A(45) = cont_VV(wf(:,22),wf(:,55)) * den(31)
  A(46) = cont_VV(wf(:,6),wf(:,56)) * den(33)
  A(47) = cont_VV(wf(:,24),wf(:,55)) * den(35)
  A(48) = cont_VV(wf(:,14),wf(:,56)) * den(34)
  A(49) = cont_VV(wf(:,8),wf(:,57)) * den(53)
  A(50) = cont_VV(wf(:,5),wf(:,58)) * den(54)
  A(51) = cont_VV(wf(:,5),wf(:,61)) * den(5)
  A(52) = cont_VV(wf(:,8),wf(:,62)) * den(8)
  A(53) = cont_VV(wf(:,22),wf(:,63)) * den(55)
  A(54) = cont_VV(wf(:,28),wf(:,58)) * den(34)
  A(55) = cont_VV(wf(:,24),wf(:,63)) * den(35)
  A(56) = cont_VV(wf(:,28),wf(:,61)) * den(33)
  A(57) = cont_VV(wf(:,19),wf(:,64)) * den(56)
  A(58) = cont_VV(wf(:,23),wf(:,64)) * den(43)
  A(59) = cont_VV(wf(:,32),wf(:,57)) * den(44)
  A(60) = cont_VV(wf(:,32),wf(:,62)) * den(42)
  A(61) = cont_VV(wf(:,19),wf(:,67)) * den(20)
  A(62) = cont_VV(wf(:,22),wf(:,68)) * den(23)
  A(63) = cont_VV(wf(:,23),wf(:,67)) * den(25)
  A(64) = cont_VV(wf(:,24),wf(:,68)) * den(28)
  A(65) = cont_VV(wf(:,20),wf(:,69)) * den(57)
  A(66) = cont_VV(wf(:,21),wf(:,70)) * den(58)
  A(67) = cont_VV(wf(:,20),wf(:,73)) * den(20)
  A(68) = cont_VV(wf(:,21),wf(:,74)) * den(23)
  A(69) = cont_VV(wf(:,6),wf(:,75)) * den(59)
  A(70) = cont_VV(wf(:,14),wf(:,75)) * den(60)
  A(71) = cont_VV(wf(:,27),wf(:,70)) * den(61)
  A(72) = cont_VV(wf(:,27),wf(:,74)) * den(31)
  A(73) = cont_VV(wf(:,7),wf(:,76)) * den(62)
  A(74) = cont_VV(wf(:,31),wf(:,69)) * den(63)
  A(75) = cont_VV(wf(:,11),wf(:,76)) * den(64)
  A(76) = cont_VV(wf(:,31),wf(:,73)) * den(39)
  A(77) = cont_VV(wf(:,6),wf(:,79)) * den(5)
  A(78) = cont_VV(wf(:,7),wf(:,80)) * den(8)
  A(79) = cont_VV(wf(:,11),wf(:,80)) * den(11)
  A(80) = cont_VV(wf(:,14),wf(:,79)) * den(15)
  A(81) = cont_VV(wf(:,6),wf(:,83)) * den(66)
  A(82) = cont_VV(wf(:,7),wf(:,84)) * den(67)
  A(83) = cont_VV(wf(:,8),wf(:,87)) * den(69)
  A(84) = cont_VV(wf(:,5),wf(:,88)) * den(70)
  A(85) = cont_VV(wf(:,6),wf(:,89)) * den(71)
  A(86) = cont_VV(wf(:,8),wf(:,90)) * den(72)
  A(87) = cont_VV(wf(:,11),wf(:,84)) * den(73)
  A(88) = cont_VV(wf(:,8),wf(:,93)) * den(75)
  A(89) = cont_VV(wf(:,8),wf(:,94)) * den(76)
  A(90) = cont_VV(wf(:,14),wf(:,83)) * den(77)
  A(91) = cont_VV(wf(:,5),wf(:,95)) * den(78)
  A(92) = cont_VV(wf(:,5),wf(:,98)) * den(80)
  A(93) = cont_VV(wf(:,20),wf(:,101)) * den(82)
  A(94) = cont_VV(wf(:,21),wf(:,102)) * den(83)
  A(95) = cont_VV(wf(:,22),wf(:,105)) * den(85)
  A(96) = cont_VV(wf(:,19),wf(:,106)) * den(86)
  A(97) = cont_VV(wf(:,20),wf(:,107)) * den(87)
  A(98) = cont_VV(wf(:,22),wf(:,108)) * den(88)
  A(99) = cont_VV(wf(:,20),wf(:,109)) * den(89)
  A(100) = cont_VV(wf(:,20),wf(:,110)) * den(90)
  A(101) = cont_VV(wf(:,23),wf(:,106)) * den(91)
  A(102) = cont_VV(wf(:,21),wf(:,111)) * den(92)
  A(103) = cont_VV(wf(:,21),wf(:,112)) * den(93)
  A(104) = cont_VV(wf(:,24),wf(:,105)) * den(94)
  A(105) = cont_VV(wf(:,27),wf(:,102)) * den(95)
  A(106) = cont_VV(wf(:,22),wf(:,115)) * den(97)
  A(107) = cont_VV(wf(:,22),wf(:,116)) * den(98)
  A(108) = cont_VV(wf(:,6),wf(:,117)) * den(99)
  A(109) = cont_VV(wf(:,6),wf(:,118)) * den(100)
  A(110) = cont_VV(wf(:,28),wf(:,88)) * den(101)
  A(111) = cont_VV(wf(:,14),wf(:,118)) * den(102)
  A(112) = cont_VV(wf(:,27),wf(:,111)) * den(103)
  A(113) = cont_VV(wf(:,14),wf(:,117)) * den(104)
  A(114) = cont_VV(wf(:,27),wf(:,112)) * den(105)
  A(115) = cont_VV(wf(:,24),wf(:,115)) * den(106)
  A(116) = cont_VV(wf(:,28),wf(:,98)) * den(107)
  A(117) = cont_VV(wf(:,31),wf(:,101)) * den(108)
  A(118) = cont_VV(wf(:,19),wf(:,119)) * den(109)
  A(119) = cont_VV(wf(:,19),wf(:,122)) * den(111)
  A(120) = cont_VV(wf(:,7),wf(:,123)) * den(112)
  A(121) = cont_VV(wf(:,7),wf(:,124)) * den(113)
  A(122) = cont_VV(wf(:,32),wf(:,87)) * den(114)
  A(123) = cont_VV(wf(:,31),wf(:,110)) * den(115)
  A(124) = cont_VV(wf(:,11),wf(:,123)) * den(116)
  A(125) = cont_VV(wf(:,31),wf(:,109)) * den(117)
  A(126) = cont_VV(wf(:,11),wf(:,124)) * den(118)
  A(127) = cont_VV(wf(:,32),wf(:,93)) * den(119)
  A(128) = cont_VV(wf(:,23),wf(:,122)) * den(120)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(128)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(4)-A(5)-A(7)-A(10)-A(11)-A(13)-A(15))*f(1))/2._/**/REALKIND+((-A(2)-A(3)-A(6)-A(8)-A(9)-A(12)-A(14) &
       -A(16))*f(1))/6._/**/REALKIND
  M1(2) = ((A(1)+A(4)+A(5)+A(7)+A(10)+A(11)+A(13)+A(15))*f(1))/6._/**/REALKIND+((A(2)+A(3)+A(6)+A(8)+A(9)+A(12)+A(14) &
       +A(16))*f(1))/2._/**/REALKIND

  M2(1) = ((A(82)+A(83)+A(86)+A(87)+A(88)+A(89)+A(94)+A(95)+A(98)+A(102)+A(103)+A(104)+A(105)+A(106)+A(107)+A(112)+A(114)+A(115) &
       +A(120)+A(121)+A(122)+A(124)+A(126)+A(127))*f(2))/6._/**/REALKIND+((A(81)+A(84)+A(85)+A(90)+A(91)+A(92)+A(93)+A(96)+A(97) &
       +A(99)+A(100)+A(101)+A(108)+A(109)+A(110)+A(111)+A(113)+A(116)+A(117)+A(118)+A(119)+A(123)+A(125) &
       +A(128))*f(2))/2._/**/REALKIND+((-A(17)-A(21)-A(23)-A(26)-A(34)-A(35)-A(38)-A(41)-A(50)-A(54)-A(57)-A(58)-A(65)-A(69)-A(70) &
       -A(74))*f(3))/2._/**/REALKIND+((-A(18)-A(19)-A(22)-A(25)-A(33)-A(37)-A(39)-A(42)-A(49)-A(53)-A(55)-A(59)-A(66)-A(71)-A(73) &
       -A(75))*f(3))/6._/**/REALKIND+((-A(24)-A(28)-A(30)-A(32)-A(36)-A(43)-A(45)-A(47)-A(52)-A(60)-A(62)-A(64)-A(68)-A(72)-A(78) &
       -A(79))*f(4))/6._/**/REALKIND+((-A(20)-A(27)-A(29)-A(31)-A(40)-A(44)-A(46)-A(48)-A(51)-A(56)-A(61)-A(63)-A(67)-A(76)-A(77) &
       -A(80))*f(4))/2._/**/REALKIND
  M2(2) = ((-A(82)-A(83)-A(86)-A(87)-A(88)-A(89)-A(94)-A(95)-A(98)-A(102)-A(103)-A(104)-A(105)-A(106)-A(107)-A(112)-A(114)-A(115) &
       -A(120)-A(121)-A(122)-A(124)-A(126)-A(127))*f(2))/2._/**/REALKIND+((-A(81)-A(84)-A(85)-A(90)-A(91)-A(92)-A(93)-A(96)-A(97) &
       -A(99)-A(100)-A(101)-A(108)-A(109)-A(110)-A(111)-A(113)-A(116)-A(117)-A(118)-A(119)-A(123)-A(125) &
       -A(128))*f(2))/6._/**/REALKIND+((A(17)+A(21)+A(23)+A(26)+A(34)+A(35)+A(38)+A(41)+A(50)+A(54)+A(57)+A(58)+A(65)+A(69)+A(70) &
       +A(74))*f(3))/6._/**/REALKIND+((A(18)+A(19)+A(22)+A(25)+A(33)+A(37)+A(39)+A(42)+A(49)+A(53)+A(55)+A(59)+A(66)+A(71)+A(73) &
       +A(75))*f(3))/2._/**/REALKIND+((A(24)+A(28)+A(30)+A(32)+A(36)+A(43)+A(45)+A(47)+A(52)+A(60)+A(62)+A(64)+A(68)+A(72)+A(78) &
       +A(79))*f(4))/2._/**/REALKIND+((A(20)+A(27)+A(29)+A(31)+A(40)+A(44)+A(46)+A(48)+A(51)+A(56)+A(61)+A(63)+A(67)+A(76)+A(77) &
       +A(80))*f(4))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_uxuxddwxwx_1_/**/REALKIND
