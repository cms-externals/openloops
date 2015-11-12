
module ol_colourmatrix_pplltt_eexttxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,3), KL(2,3)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
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
  K1(13,:) = [  64,  -8]
  K1(14,:) = [  -8,  64]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [  -1, -10]
  K1(20,:) = [ -10,  -1]
  K1(21,:) = [  64,  -8]
  K1(22,:) = [  -8,  64]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   9,   9]
  K1(28,:) = [   9, -72]
  K1(29,:) = [ -72,   9]
  K1(30,:) = [   9,   9]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [ -72,   9]
  K1(38,:) = [   9,   9]
  K1(39,:) = [   9,   9]
  K1(40,:) = [   9, -72]
  K1(41,:) = [ -81,   0]
  K1(42,:) = [   0, -81]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2,  6]
  K2(2,:) = [ -2, 16,  6]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplltt_eexttxgg_1_/**/REALKIND



module ol_forced_parameters_pplltt_eexttxgg_1_/**/REALKIND
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
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplltt_eexttxgg_1_/**/REALKIND

module ol_loop_pplltt_eexttxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(36), c(37)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:136)
  ! denominators
  complex(REALKIND), save :: den(163)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64)
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
    f( 1) = (2*CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 2) = CI*eQED**2*gQCD**2
    f( 3) = (2*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 4) = eQED**2*gQCD**2
    f( 5) = (2*CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*eQED**2*gQCD**4
    f( 7) = (2*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 8) = countertermnorm*eQED**2*gQCD**4
    f( 9) = (2*CI*countertermnorm*ctGtt*eQED**2*gQCD**4)/3._/**/REALKIND
    f(10) = CI*countertermnorm*ctGtt*eQED**2*gQCD**4
    f(11) = (2*countertermnorm*ctGtt*eQED**2*gQCD**4)/3._/**/REALKIND
    f(12) = countertermnorm*ctGtt*eQED**2*gQCD**4
    f(13) = (2*CI*countertermnorm*ctVtt*eQED**2*gQCD**4)/3._/**/REALKIND
    f(14) = CI*countertermnorm*ctVtt*eQED**2*gQCD**4
    f(15) = (2*countertermnorm*ctVtt*eQED**2*gQCD**4)/3._/**/REALKIND
    f(16) = countertermnorm*ctVtt*eQED**2*gQCD**4
    f(17) = (2*countertermnorm*ctVVV*eQED**2*gQCD**4)/3._/**/REALKIND
    f(18) = countertermnorm*ctVVV*eQED**2*gQCD**4
    f(19) = CI*countertermnorm*ctZGG*eQED**2*gQCD**4
    f(20) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f(21) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(22) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(23) = (2*CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(24) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f(25) = (2*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(26) = eQED**2*gQCD**4*integralnorm*SwB
    f(27) = (CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(28) = (2*CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(29) = CI*eQED**2*gQCD**4*integralnorm*SwF
    f(30) = (4*CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(31) = 2*CI*eQED**2*gQCD**4*integralnorm*SwF
    f(32) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(33) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(34) = eQED**2*gQCD**4*integralnorm*SwF
    f(35) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(36) = 2*eQED**2*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(21), 18*CI*f(21), 9*CI*f(22), 18*CI*f(22), CI*f(23), 3*CI*f(23), 8*CI*f(23), 9*CI*f(23), 18*CI*f(23), CI*f(24) &
    , 3*CI*f(24), 8*CI*f(24), 9*CI*f(24), 18*CI*f(24), f(25), 3*f(25), 8*f(25), 9*f(25), f(26), 3*f(26), 8*f(26), 9*f(26) &
    , 3*CI*f(27), 3*CI*f(28), 3*CI*f(29), 3*CI*f(30), 3*CI*f(31), f(32), 3*f(32), f(33), 3*f(33), f(34), 3*f(34), f(35), 3*f(35) &
    , f(36), 3*f(36) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND), intent(out) :: M1(2), M2(3)
  complex(REALKIND) :: A(107)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMT, H(3), wf(:,-2))
  call wf_A(P(:,4), rMT, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),MT,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),MT,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,3),MZ,1_intkind1,wf(:,8))
  call vert_ZQ_A(gZu,wf(:,8),wf(:,4),wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,11),MT,1_intkind1,wf(:,12))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,8),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,11),MT,1_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,36),MT,1_intkind1,wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,24),MT,1_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,1),wf(:,17),wf(:,19))
  call vert_ZQ_A(gZu,wf(:,8),wf(:,17),wf(:,20))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,21))
  call vert_AV_Q(wf(:,18),wf(:,-5),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,7),MT,1_intkind1,wf(:,23))
  call vert_ZQ_A(gZu,wf(:,8),wf(:,-2),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,7),MT,1_intkind1,wf(:,25))
  call vert_VQ_A(wf(:,-4),wf(:,17),wf(:,26))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,27))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,28))
  call vert_AV_Q(wf(:,-3),wf(:,28),wf(:,29))
  call vert_VQ_A(wf(:,28),wf(:,-2),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,52),MT,1_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,32))
  call counter_VGG_G(ctAGGG,wf(:,1),wf(:,-4),wf(:,-5),wf(:,33))
  call counter_VGG_G(ctAGGG,wf(:,1),wf(:,-5),wf(:,-4),wf(:,34))
  call counter_VGG_G(ctZGGG,wf(:,8),wf(:,-4),wf(:,-5),wf(:,35))
  call counter_VGG_G(ctZGGG,wf(:,8),wf(:,-5),wf(:,-4),wf(:,36))
  call counter_VG_G(wf(:,8),wf(:,32),Q(:,12),wf(:,37),Q(:,15))
  call vert_UV_W(wf(:,32),Q(:,12),wf(:,-4),Q(:,16),wf(:,38))
  call counter_VG_G(wf(:,8),wf(:,-5),Q(:,32),wf(:,39),Q(:,35))
  call vert_UV_W(wf(:,32),Q(:,12),wf(:,-5),Q(:,32),wf(:,40))
  call counter_VG_G(wf(:,8),wf(:,-4),Q(:,16),wf(:,41),Q(:,19))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,42))
  call counter_ZQ_A(gZu,wf(:,8),wf(:,4),wf(:,43))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,44))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,45))
  call counter_VQ_A(wf(:,1),wf(:,17),wf(:,46))
  call counter_ZQ_A(gZu,wf(:,8),wf(:,17),wf(:,47))
  call counter_AV_Q(wf(:,18),wf(:,-5),wf(:,48))
  call vert_QA_V(wf(:,-2),wf(:,18),wf(:,49))
  call counter_VQ_A(wf(:,-4),wf(:,17),wf(:,50))
  call vert_QA_V(wf(:,17),wf(:,-3),wf(:,51))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,52))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,53))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,54))
  call vert_AV_Q(wf(:,-3),wf(:,54),wf(:,55))
  call vert_VQ_A(wf(:,54),wf(:,-2),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,52),MT,1_intkind1,wf(:,57))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,58))
  call prop_Q_A(wf(:,11),Q(:,52),MT,1_intkind1,wf(:,59))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,8),wf(:,60))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,40),MT,1_intkind1,wf(:,62))
  call prop_Q_A(wf(:,26),Q(:,52),MT,1_intkind1,wf(:,63))
  call counter_AV_Q(wf(:,-3),wf(:,28),wf(:,64))
  call vert_AV_Q(wf(:,62),wf(:,-4),wf(:,65))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,66))
  call prop_A_Q(wf(:,66),Q(:,24),MT,1_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,67),wf(:,-5),wf(:,68))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,69))
  call prop_A_Q(wf(:,22),Q(:,56),MT,1_intkind1,wf(:,70))
  call counter_ZQ_A(gZu,wf(:,8),wf(:,-2),wf(:,71))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,36),MT,1_intkind1,wf(:,73))
  call vert_VQ_A(wf(:,1),wf(:,73),wf(:,74))
  call vert_ZQ_A(gZu,wf(:,8),wf(:,73),wf(:,75))
  call prop_A_Q(wf(:,27),Q(:,56),MT,1_intkind1,wf(:,76))
  call counter_VQ_A(wf(:,28),wf(:,-2),wf(:,77))
  call prop_A_Q(wf(:,29),Q(:,56),MT,1_intkind1,wf(:,78))
  call vert_VQ_A(wf(:,-4),wf(:,73),wf(:,79))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,80))
  call prop_Q_A(wf(:,80),Q(:,20),MT,1_intkind1,wf(:,81))
  call vert_VQ_A(wf(:,1),wf(:,81),wf(:,82))
  call vert_ZQ_A(gZu,wf(:,8),wf(:,81),wf(:,83))
  call vert_VQ_A(wf(:,-5),wf(:,81),wf(:,84))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,85))
  call counter_Q_A(cttt,wf(:,4),Q(:,20),wf(:,86))
  call prop_A_Q(wf(:,85),Q(:,43),MT,1_intkind1,wf(:,87))
  call vert_AZ_Q(gZu,wf(:,5),wf(:,8),wf(:,88))
  call prop_A_Q(wf(:,88),Q(:,43),MT,1_intkind1,wf(:,89))
  call counter_A_Q(cttt,wf(:,5),Q(:,40),wf(:,90))
  call prop_Q_A(wf(:,6),Q(:,23),MT,1_intkind1,wf(:,91))
  call prop_Q_A(wf(:,9),Q(:,23),MT,1_intkind1,wf(:,92))
  call prop_Q_A(wf(:,86),Q(:,20),MT,1_intkind1,wf(:,93))
  call vert_VQ_A(wf(:,-5),wf(:,93),wf(:,94))
  call counter_A_Q(cttt,wf(:,12),Q(:,11),wf(:,95))
  call counter_A_Q(cttt,wf(:,14),Q(:,11),wf(:,96))
  call vert_AV_Q(wf(:,18),wf(:,1),wf(:,97))
  call counter_Q_A(cttt,wf(:,17),Q(:,36),wf(:,98))
  call prop_A_Q(wf(:,97),Q(:,27),MT,1_intkind1,wf(:,99))
  call vert_AZ_Q(gZu,wf(:,18),wf(:,8),wf(:,100))
  call prop_A_Q(wf(:,100),Q(:,27),MT,1_intkind1,wf(:,101))
  call counter_A_Q(cttt,wf(:,18),Q(:,24),wf(:,102))
  call prop_Q_A(wf(:,19),Q(:,39),MT,1_intkind1,wf(:,103))
  call prop_Q_A(wf(:,20),Q(:,39),MT,1_intkind1,wf(:,104))
  call counter_Q_A(cttt,wf(:,23),Q(:,7),wf(:,105))
  call counter_Q_A(cttt,wf(:,25),Q(:,7),wf(:,106))
  call prop_A_Q(wf(:,102),Q(:,24),MT,1_intkind1,wf(:,107))
  call vert_AV_Q(wf(:,107),wf(:,-5),wf(:,108))
  call prop_Q_A(wf(:,98),Q(:,36),MT,1_intkind1,wf(:,109))
  call vert_VQ_A(wf(:,-4),wf(:,109),wf(:,110))
  call prop_A_Q(wf(:,90),Q(:,40),MT,1_intkind1,wf(:,111))
  call vert_AV_Q(wf(:,111),wf(:,-4),wf(:,112))
  call counter_Q_A(cttt,wf(:,31),Q(:,52),wf(:,113))
  call counter_V_V(ctGG,wf(:,28),Q(:,48),wf(:,114))
  call vert_VQ_A(wf(:,114),wf(:,-2),wf(:,115))
  call vert_AV_Q(wf(:,-3),wf(:,114),wf(:,116))
  call vert_QA_V(wf(:,23),wf(:,-3),wf(:,117))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,118))
  call vert_VQ_A(wf(:,-4),wf(:,23),wf(:,119))
  call prop_Q_A(wf(:,119),Q(:,23),MT,1_intkind1,wf(:,120))
  call vert_VQ_A(wf(:,-4),wf(:,25),wf(:,121))
  call prop_Q_A(wf(:,121),Q(:,23),MT,1_intkind1,wf(:,122))
  call vert_VQ_A(wf(:,-5),wf(:,23),wf(:,123))
  call prop_Q_A(wf(:,123),Q(:,39),MT,1_intkind1,wf(:,124))
  call vert_VQ_A(wf(:,-5),wf(:,25),wf(:,125))
  call prop_Q_A(wf(:,125),Q(:,39),MT,1_intkind1,wf(:,126))
  call vert_QA_V(wf(:,-2),wf(:,12),wf(:,127))
  call vert_QA_V(wf(:,-2),wf(:,14),wf(:,128))
  call vert_AV_Q(wf(:,12),wf(:,-4),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,27),MT,1_intkind1,wf(:,130))
  call vert_AV_Q(wf(:,14),wf(:,-4),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,27),MT,1_intkind1,wf(:,132))
  call vert_AV_Q(wf(:,12),wf(:,-5),wf(:,133))
  call prop_A_Q(wf(:,133),Q(:,43),MT,1_intkind1,wf(:,134))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,43),MT,1_intkind1,wf(:,136))

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
  den(2) = 1 / (Q(5,20) - MT2)
  den(3) = 1 / (Q(5,40) - MT2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,11) - MT2)
  den(14) = 1 / (Q(5,36) - MT2)
  den(15) = 1 / (Q(5,24) - MT2)
  den(20) = 1 / (Q(5,7) - MT2)
  den(29) = 1 / (Q(5,48))
  den(32) = 1 / (Q(5,52) - MT2)
  den(36) = 1 / (Q(5,12))
  den(40) = 1 / (Q(5,28))
  den(43) = 1 / (Q(5,44))
  den(60) = 1 / (Q(5,56) - MT2)
  den(73) = 1 / (Q(5,43) - MT2)
  den(79) = 1 / (Q(5,23) - MT2)
  den(90) = 1 / (Q(5,27) - MT2)
  den(96) = 1 / (Q(5,39) - MT2)
  den(125) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(2)*den(12)
  den(16) = den(1)*den(14)
  den(17) = den(15)*den(16)
  den(18) = den(6)*den(14)
  den(19) = den(15)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(15)*den(21)
  den(23) = den(6)*den(20)
  den(24) = den(15)*den(23)
  den(25) = den(10)*den(14)
  den(26) = den(12)*den(14)
  den(27) = den(3)*den(21)
  den(28) = den(3)*den(23)
  den(30) = den(21)*den(29)
  den(31) = den(23)*den(29)
  den(33) = den(29)*den(32)
  den(34) = den(1)*den(33)
  den(35) = den(6)*den(33)
  den(37) = den(1)*den(36)
  den(38) = den(6)*den(36)
  den(39) = den(29)*den(38)
  den(41) = den(36)*den(40)
  den(42) = den(6)*den(41)
  den(44) = den(36)*den(43)
  den(45) = den(6)*den(44)
  den(46) = den(2)*den(40)
  den(47) = den(6)*den(46)
  den(48) = den(15)*den(40)
  den(49) = den(6)*den(48)
  den(50) = den(14)*den(43)
  den(51) = den(6)*den(50)
  den(52) = den(3)*den(43)
  den(53) = den(6)*den(52)
  den(54) = den(2)*den(32)
  den(55) = den(1)*den(54)
  den(56) = den(6)*den(54)
  den(57) = den(14)*den(32)
  den(58) = den(1)*den(57)
  den(59) = den(6)*den(57)
  den(61) = den(15)*den(60)
  den(62) = den(1)*den(61)
  den(63) = den(6)*den(61)
  den(64) = den(3)*den(60)
  den(65) = den(1)*den(64)
  den(66) = den(6)*den(64)
  den(67) = den(10)*den(29)
  den(68) = den(12)*den(29)
  den(69) = den(29)*den(60)
  den(70) = den(1)*den(69)
  den(71) = den(6)*den(69)
  den(72) = den(1)*den(3)
  den(74) = den(72)*den(73)
  den(75) = den(2)*den(74)
  den(76) = den(3)*den(6)
  den(77) = den(73)*den(76)
  den(78) = den(2)*den(77)
  den(80) = den(4)*den(79)
  den(81) = den(3)*den(80)
  den(82) = den(7)*den(79)
  den(83) = den(3)*den(82)
  den(84) = den(2)**2
  den(85) = den(10)*den(84)
  den(86) = den(12)*den(84)
  den(87) = den(10)*den(54)
  den(88) = den(12)*den(54)
  den(89) = den(1)*den(15)
  den(91) = den(89)*den(90)
  den(92) = den(14)*den(91)
  den(93) = den(6)*den(15)
  den(94) = den(90)*den(93)
  den(95) = den(14)*den(94)
  den(97) = den(16)*den(96)
  den(98) = den(15)*den(97)
  den(99) = den(18)*den(96)
  den(100) = den(15)*den(99)
  den(101) = den(21)*den(61)
  den(102) = den(23)*den(61)
  den(103) = den(15)**2
  den(104) = den(21)*den(103)
  den(105) = den(23)*den(103)
  den(106) = den(14)**2
  den(107) = den(10)*den(106)
  den(108) = den(12)*den(106)
  den(109) = den(10)*den(57)
  den(110) = den(12)*den(57)
  den(111) = den(21)*den(64)
  den(112) = den(23)*den(64)
  den(113) = den(3)**2
  den(114) = den(21)*den(113)
  den(115) = den(23)*den(113)
  den(116) = den(21)*den(69)
  den(117) = den(23)*den(69)
  den(118) = den(10)*den(33)
  den(119) = den(12)*den(33)
  den(120) = den(29)**2
  den(121) = den(10)*den(120)
  den(122) = den(12)*den(120)
  den(123) = den(21)*den(120)
  den(124) = den(23)*den(120)
  den(126) = den(21)*den(125)
  den(127) = den(23)*den(125)
  den(128) = den(21)*den(79)
  den(129) = den(23)*den(79)
  den(130) = den(21)*den(96)
  den(131) = den(23)*den(96)
  den(132) = den(10)*den(125)
  den(133) = den(12)*den(125)
  den(134) = den(10)*den(90)
  den(135) = den(12)*den(90)
  den(136) = den(10)*den(73)
  den(137) = den(12)*den(73)
  den(138) = den(1)*den(29)*den(36)
  den(139) = den(6)*den(29)*den(36)
  den(140) = den(1)*den(41)
  den(141) = den(1)*den(44)
  den(142) = den(1)*den(2)*den(3)
  den(143) = den(2)*den(3)*den(6)
  den(144) = den(1)*den(46)
  den(145) = den(1)*den(14)*den(15)
  den(146) = den(6)*den(14)*den(15)
  den(147) = den(1)*den(48)
  den(148) = den(1)*den(50)
  den(149) = den(1)*den(52)
  den(150) = den(1)*den(29)
  den(151) = den(6)*den(29)
  den(152) = den(2)*den(136)
  den(153) = den(2)*den(137)
  den(154) = den(15)*den(130)
  den(155) = den(15)*den(131)
  den(156) = den(14)*den(134)
  den(157) = den(14)*den(135)
  den(158) = den(3)*den(128)
  den(159) = den(3)*den(129)
  den(160) = den(29)*den(126)
  den(161) = den(29)*den(127)
  den(162) = den(29)*den(132)
  den(163) = den(29)*den(133)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(107)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,5),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,11),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(19)
  A(7) = cont_QA(wf(:,22),wf(:,23)) * den(22)
  A(8) = cont_QA(wf(:,22),wf(:,25)) * den(24)
  A(9) = cont_QA(wf(:,12),wf(:,26)) * den(25)
  A(10) = cont_QA(wf(:,14),wf(:,26)) * den(26)
  A(11) = cont_QA(wf(:,23),wf(:,27)) * den(27)
  A(12) = cont_QA(wf(:,25),wf(:,27)) * den(28)
  A(13) = cont_QA(wf(:,23),wf(:,29)) * den(30)
  A(14) = cont_QA(wf(:,25),wf(:,29)) * den(31)
  A(15) = cont_QA(wf(:,10),wf(:,31)) * den(34)
  A(16) = cont_QA(wf(:,13),wf(:,31)) * den(35)

  A(17) = cont_VV(wf(:,32),wf(:,33)) * den(37)
  A(18) = cont_VV(wf(:,32),wf(:,34)) * den(37)
  A(19) = cont_VV(wf(:,32),wf(:,35)) * den(38)
  A(20) = cont_VV(wf(:,32),wf(:,36)) * den(38)
  A(21) = cont_VV(wf(:,28),wf(:,37)) * den(39)
  A(22) = cont_VV(wf(:,38),wf(:,39)) * den(42)
  A(23) = cont_VV(wf(:,40),wf(:,41)) * den(45)
  A(24) = cont_QA(wf(:,5),wf(:,42)) * den(5)
  A(25) = cont_QA(wf(:,5),wf(:,43)) * den(8)
  A(26) = cont_QA(wf(:,12),wf(:,44)) * den(11)
  A(27) = cont_QA(wf(:,14),wf(:,44)) * den(13)
  A(28) = cont_VV(wf(:,39),wf(:,45)) * den(47)
  A(29) = cont_QA(wf(:,18),wf(:,46)) * den(17)
  A(30) = cont_QA(wf(:,18),wf(:,47)) * den(19)
  A(31) = cont_QA(wf(:,23),wf(:,48)) * den(22)
  A(32) = cont_QA(wf(:,25),wf(:,48)) * den(24)
  A(33) = cont_VV(wf(:,39),wf(:,49)) * den(49)
  A(34) = cont_QA(wf(:,12),wf(:,50)) * den(25)
  A(35) = cont_QA(wf(:,14),wf(:,50)) * den(26)
  A(36) = cont_VV(wf(:,41),wf(:,51)) * den(51)
  A(37) = cont_QA(wf(:,23),wf(:,52)) * den(27)
  A(38) = cont_QA(wf(:,25),wf(:,52)) * den(28)
  A(39) = cont_VV(wf(:,41),wf(:,53)) * den(53)
  A(40) = cont_QA(wf(:,23),wf(:,55)) * den(30)
  A(41) = cont_QA(wf(:,25),wf(:,55)) * den(31)
  A(42) = cont_QA(wf(:,10),wf(:,57)) * den(34)
  A(43) = cont_QA(wf(:,13),wf(:,57)) * den(35)
  A(44) = cont_QA(wf(:,58),wf(:,59)) * den(55)
  A(45) = cont_QA(wf(:,59),wf(:,60)) * den(56)
  A(46) = cont_QA(wf(:,6),wf(:,62)) * den(5)
  A(47) = cont_QA(wf(:,9),wf(:,62)) * den(8)
  A(48) = cont_QA(wf(:,58),wf(:,63)) * den(58)
  A(49) = cont_QA(wf(:,60),wf(:,63)) * den(59)
  A(50) = cont_QA(wf(:,23),wf(:,64)) * den(30)
  A(51) = cont_QA(wf(:,25),wf(:,64)) * den(31)
  A(52) = cont_QA(wf(:,31),wf(:,58)) * den(34)
  A(53) = cont_QA(wf(:,31),wf(:,60)) * den(35)
  A(54) = cont_QA(wf(:,23),wf(:,65)) * den(27)
  A(55) = cont_QA(wf(:,25),wf(:,65)) * den(28)
  A(56) = cont_QA(wf(:,19),wf(:,67)) * den(17)
  A(57) = cont_QA(wf(:,20),wf(:,67)) * den(19)
  A(58) = cont_QA(wf(:,23),wf(:,68)) * den(22)
  A(59) = cont_QA(wf(:,25),wf(:,68)) * den(24)
  A(60) = cont_QA(wf(:,69),wf(:,70)) * den(62)
  A(61) = cont_QA(wf(:,70),wf(:,71)) * den(63)
  A(62) = cont_QA(wf(:,18),wf(:,74)) * den(17)
  A(63) = cont_QA(wf(:,18),wf(:,75)) * den(19)
  A(64) = cont_QA(wf(:,69),wf(:,76)) * den(65)
  A(65) = cont_QA(wf(:,71),wf(:,76)) * den(66)
  A(66) = cont_QA(wf(:,12),wf(:,77)) * den(67)
  A(67) = cont_QA(wf(:,14),wf(:,77)) * den(68)
  A(68) = cont_QA(wf(:,69),wf(:,78)) * den(70)
  A(69) = cont_QA(wf(:,71),wf(:,78)) * den(71)
  A(70) = cont_QA(wf(:,12),wf(:,79)) * den(25)
  A(71) = cont_QA(wf(:,14),wf(:,79)) * den(26)
  A(72) = cont_QA(wf(:,5),wf(:,82)) * den(5)
  A(73) = cont_QA(wf(:,5),wf(:,83)) * den(8)
  A(74) = cont_QA(wf(:,12),wf(:,84)) * den(11)
  A(75) = cont_QA(wf(:,14),wf(:,84)) * den(13)
  A(76) = cont_QA(wf(:,86),wf(:,87)) * den(75)
  A(77) = cont_QA(wf(:,86),wf(:,89)) * den(78)
  A(78) = cont_QA(wf(:,90),wf(:,91)) * den(81)
  A(79) = cont_QA(wf(:,90),wf(:,92)) * den(83)
  A(80) = cont_QA(wf(:,12),wf(:,94)) * den(85)
  A(81) = cont_QA(wf(:,14),wf(:,94)) * den(86)
  A(82) = cont_QA(wf(:,59),wf(:,95)) * den(87)
  A(83) = cont_QA(wf(:,59),wf(:,96)) * den(88)
  A(84) = cont_QA(wf(:,98),wf(:,99)) * den(92)
  A(85) = cont_QA(wf(:,98),wf(:,101)) * den(95)
  A(86) = cont_QA(wf(:,102),wf(:,103)) * den(98)
  A(87) = cont_QA(wf(:,102),wf(:,104)) * den(100)
  A(88) = cont_QA(wf(:,70),wf(:,105)) * den(101)
  A(89) = cont_QA(wf(:,70),wf(:,106)) * den(102)
  A(90) = cont_QA(wf(:,23),wf(:,108)) * den(104)
  A(91) = cont_QA(wf(:,25),wf(:,108)) * den(105)
  A(92) = cont_QA(wf(:,12),wf(:,110)) * den(107)
  A(93) = cont_QA(wf(:,14),wf(:,110)) * den(108)
  A(94) = cont_QA(wf(:,63),wf(:,95)) * den(109)
  A(95) = cont_QA(wf(:,63),wf(:,96)) * den(110)
  A(96) = cont_QA(wf(:,76),wf(:,105)) * den(111)
  A(97) = cont_QA(wf(:,76),wf(:,106)) * den(112)
  A(98) = cont_QA(wf(:,23),wf(:,112)) * den(114)
  A(99) = cont_QA(wf(:,25),wf(:,112)) * den(115)
  A(100) = cont_QA(wf(:,78),wf(:,105)) * den(116)
  A(101) = cont_QA(wf(:,78),wf(:,106)) * den(117)
  A(102) = cont_QA(wf(:,12),wf(:,113)) * den(118)
  A(103) = cont_QA(wf(:,14),wf(:,113)) * den(119)
  A(104) = cont_QA(wf(:,12),wf(:,115)) * den(121)
  A(105) = cont_QA(wf(:,14),wf(:,115)) * den(122)
  A(106) = cont_QA(wf(:,23),wf(:,116)) * den(123)
  A(107) = cont_QA(wf(:,25),wf(:,116)) * den(124)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(107)
  complex(REALKIND), intent(out) :: M1(2), M2(3)

  M1(1) = (-A(5)-A(7)-A(9))*f(1)+(A(6)+A(8)+A(10))*f(2)+CI*(-A(13)-A(15))*f(3)+CI*(A(14)+A(16))*f(4)
  M1(2) = (-A(1)-A(3)-A(11))*f(1)+(A(2)+A(4)+A(12))*f(2)+CI*(A(13)+A(15))*f(3)+CI*(-A(14)-A(16))*f(4)

  M2(1) = (A(84)+A(86)+A(88)+A(90)+A(92)+A(94))*f(5)+((A(17)+A(19))*f(6))/2._/**/REALKIND+(-A(85)-A(87)-A(89)-A(91)-A(93) &
       -A(95))*f(6)+CI*(A(100)+A(102)+A(104)+A(106))*f(7)+CI*(-A(101)-A(103)-A(105)-A(107))*f(8)+(-A(31)-A(34)-A(56)-A(58)-A(62) &
       -A(70))*f(9)+(A(32)+A(35)+A(57)+A(59)+A(63)+A(71))*f(10)+CI*(-A(50)-A(66))*f(11)+CI*(A(51)+A(67))*f(12)+(-A(29)-A(48) &
       -A(60))*f(13)+(A(30)+A(49)+A(61))*f(14)+CI*(-A(52)-A(68))*f(15)+CI*(A(53)+A(69))*f(16)+CI*(-A(40)-A(42))*f(17)+CI*(A(41) &
       +A(43))*f(18)+CI*(A(21)+A(22)-A(23))*f(19)+(-A(33)-A(36))*f(20)
  M2(2) = (A(76)+A(78)+A(80)+A(82)+A(96)+A(98))*f(5)+((A(18)+A(20))*f(6))/2._/**/REALKIND+(-A(77)-A(79)-A(81)-A(83)-A(97) &
       -A(99))*f(6)+CI*(-A(100)-A(102)-A(104)-A(106))*f(7)+CI*(A(101)+A(103)+A(105)+A(107))*f(8)+(-A(26)-A(37)-A(46)-A(54)-A(72) &
       -A(74))*f(9)+(A(27)+A(38)+A(47)+A(55)+A(73)+A(75))*f(10)+CI*(A(50)+A(66))*f(11)+CI*(-A(51)-A(67))*f(12)+(-A(24)-A(44) &
       -A(64))*f(13)+(A(25)+A(45)+A(65))*f(14)+CI*(A(52)+A(68))*f(15)+CI*(-A(53)-A(69))*f(16)+CI*(A(40)+A(42))*f(17)+CI*(-A(41) &
       -A(43))*f(18)+CI*(-A(21)-A(22)+A(23))*f(19)+(-A(28)-A(39))*f(20)
  M2(3) = ((-A(17)-A(18)-A(19)-A(20))*f(6))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplltt_eexttxgg_1_/**/REALKIND
