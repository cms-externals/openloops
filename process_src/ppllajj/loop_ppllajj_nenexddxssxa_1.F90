
module ol_colourmatrix_ppllajj_nenexddxssxa_1_/**/REALKIND
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
end module ol_colourmatrix_ppllajj_nenexddxssxa_1_/**/REALKIND



module ol_forced_parameters_ppllajj_nenexddxssxa_1_/**/REALKIND
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
end module ol_forced_parameters_ppllajj_nenexddxssxa_1_/**/REALKIND

module ol_loop_ppllajj_nenexddxssxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(19)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:265)
  ! denominators
  complex(REALKIND), save :: den(325)
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
    f( 1) = (CI*eQED**3*gQCD**2)/3._/**/REALKIND
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**4)/3._/**/REALKIND
    f( 3) = CI*countertermnorm*ctAZGG*eQED**3*gQCD**4
    f( 4) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/3._/**/REALKIND
    f( 5) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/3._/**/REALKIND
    f( 6) = (countertermnorm*ctZGG*eQED**3*gQCD**4)/3._/**/REALKIND
    f( 7) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f( 8) = (eQED**3*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f( 9) = (eQED**3*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(10) = (eQED**3*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (2*eQED**3*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(12) = (4*eQED**3*gQCD**4*integralnorm*SwF)/3._/**/REALKIND

  c = [ 9*CI*f(7), 27*CI*f(7), 18*f(8), 54*f(8), f(9), 3*f(9), 6*f(9), 8*f(9), 10*f(9), 18*f(9), 21*f(9), 24*f(9), 54*f(9) &
    , 3*f(10), 9*f(10), 3*f(11), 9*f(11), 3*f(12), 9*f(12) ]
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
  complex(REALKIND) :: A(165)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,5),wf(:,7))
  call prop_A_Q(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,96),ZERO,0_intkind1,wf(:,13))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,14))
  call vert_AV_Q(wf(:,13),wf(:,2),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,19),ZERO,0_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,17))
  call vert_AZ_Q(gZd,wf(:,13),wf(:,4),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,28),ZERO,0_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,-6),wf(:,16),wf(:,20))
  call vert_VQ_A(wf(:,-6),wf(:,19),wf(:,21))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,22))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,68),ZERO,0_intkind1,wf(:,24))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,25))
  call vert_VQ_A(wf(:,23),wf(:,24),wf(:,26))
  call prop_A_Q(wf(:,25),Q(:,11),ZERO,0_intkind1,wf(:,27))
  call vert_AV_Q(wf(:,-3),wf(:,23),wf(:,28))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,24),wf(:,29))
  call prop_A_Q(wf(:,28),Q(:,56),ZERO,0_intkind1,wf(:,30))
  call vert_AV_Q(wf(:,-3),wf(:,-6),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,72),ZERO,0_intkind1,wf(:,32))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,33))
  call vert_AV_Q(wf(:,32),wf(:,23),wf(:,34))
  call prop_Q_A(wf(:,33),Q(:,7),ZERO,0_intkind1,wf(:,35))
  call vert_VQ_A(wf(:,23),wf(:,-2),wf(:,36))
  call vert_AZ_Q(gZd,wf(:,32),wf(:,4),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,52),ZERO,0_intkind1,wf(:,38))
  call vert_VQ_A(wf(:,-6),wf(:,35),wf(:,39))
  call vert_VQ_A(wf(:,-6),wf(:,38),wf(:,40))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,41))
  call vert_AV_Q(wf(:,-5),wf(:,41),wf(:,42))
  call vert_VQ_A(wf(:,41),wf(:,-4),wf(:,43))
  call vert_QA_V(wf(:,-2),wf(:,32),wf(:,44))
  call vert_AV_Q(wf(:,-5),wf(:,44),wf(:,45))
  call vert_VQ_A(wf(:,44),wf(:,-4),wf(:,46))
  call vert_QA_V(wf(:,5),wf(:,-5),wf(:,47))
  call vert_QA_V(wf(:,35),wf(:,-3),wf(:,48))
  call vert_QA_V(wf(:,-2),wf(:,27),wf(:,49))
  call vert_QA_V(wf(:,-4),wf(:,13),wf(:,50))
  call counter_VVG_G(wf(:,4),wf(:,-6),wf(:,2),wf(:,51))
  call counter_VQ_A(wf(:,2),wf(:,5),wf(:,52))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,53))
  call counter_VG_G(wf(:,4),wf(:,2),Q(:,12),wf(:,54),Q(:,15))
  call counter_AV_Q(wf(:,13),wf(:,2),wf(:,55))
  call counter_AZ_Q(gZd,wf(:,13),wf(:,4),wf(:,56))
  call counter_VQ_A(wf(:,-6),wf(:,16),wf(:,57))
  call counter_VQ_A(wf(:,-6),wf(:,19),wf(:,58))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,59))
  call prop_Q_A(wf(:,10),Q(:,83),ZERO,0_intkind1,wf(:,60))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,61))
  call prop_Q_A(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,62))
  call prop_A_Q(wf(:,59),Q(:,44),ZERO,0_intkind1,wf(:,63))
  call prop_A_Q(wf(:,61),Q(:,35),ZERO,0_intkind1,wf(:,64))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,65))
  call prop_A_Q(wf(:,65),Q(:,96),ZERO,0_intkind1,wf(:,66))
  call vert_AV_Q(wf(:,66),wf(:,2),wf(:,67))
  call vert_AZ_Q(gZd,wf(:,66),wf(:,4),wf(:,68))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,69))
  call prop_A_Q(wf(:,18),Q(:,99),ZERO,0_intkind1,wf(:,70))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,71))
  call prop_A_Q(wf(:,15),Q(:,108),ZERO,0_intkind1,wf(:,72))
  call prop_Q_A(wf(:,69),Q(:,28),ZERO,0_intkind1,wf(:,73))
  call vert_VQ_A(wf(:,-6),wf(:,73),wf(:,74))
  call prop_Q_A(wf(:,71),Q(:,19),ZERO,0_intkind1,wf(:,75))
  call vert_VQ_A(wf(:,-6),wf(:,75),wf(:,76))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,80),ZERO,0_intkind1,wf(:,78))
  call vert_VQ_A(wf(:,2),wf(:,78),wf(:,79))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,78),wf(:,80))
  call counter_VQ_A(wf(:,23),wf(:,24),wf(:,81))
  call counter_VG_G(wf(:,4),wf(:,23),Q(:,48),wf(:,82),Q(:,51))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,24),wf(:,83))
  call counter_AV_Q(wf(:,32),wf(:,23),wf(:,84))
  call counter_AZ_Q(gZd,wf(:,32),wf(:,4),wf(:,85))
  call counter_VQ_A(wf(:,-6),wf(:,35),wf(:,86))
  call counter_VQ_A(wf(:,-6),wf(:,38),wf(:,87))
  call counter_AV_Q(wf(:,-5),wf(:,41),wf(:,88))
  call counter_AV_Q(wf(:,-5),wf(:,44),wf(:,89))
  call counter_QA_V(wf(:,5),wf(:,-5),wf(:,90))
  call vert_QA_V(wf(:,-4),wf(:,66),wf(:,91))
  call counter_VQ_A(wf(:,41),wf(:,-4),wf(:,92))
  call counter_VQ_A(wf(:,44),wf(:,-4),wf(:,93))
  call counter_QA_V(wf(:,-4),wf(:,13),wf(:,94))
  call vert_QA_V(wf(:,78),wf(:,-5),wf(:,95))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,96))
  call vert_VQ_A(wf(:,96),wf(:,24),wf(:,97))
  call vert_AV_Q(wf(:,-3),wf(:,96),wf(:,98))
  call prop_A_Q(wf(:,98),Q(:,56),ZERO,0_intkind1,wf(:,99))
  call vert_AV_Q(wf(:,32),wf(:,96),wf(:,100))
  call vert_VQ_A(wf(:,96),wf(:,-2),wf(:,101))
  call prop_Q_A(wf(:,101),Q(:,52),ZERO,0_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,-6),wf(:,102),wf(:,103))
  call counter_AV_Q(wf(:,-3),wf(:,23),wf(:,104))
  call prop_Q_A(wf(:,29),Q(:,71),ZERO,0_intkind1,wf(:,105))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,106))
  call prop_Q_A(wf(:,26),Q(:,116),ZERO,0_intkind1,wf(:,107))
  call prop_A_Q(wf(:,104),Q(:,56),ZERO,0_intkind1,wf(:,108))
  call prop_A_Q(wf(:,106),Q(:,11),ZERO,0_intkind1,wf(:,109))
  call counter_AV_Q(wf(:,-3),wf(:,-6),wf(:,110))
  call prop_A_Q(wf(:,110),Q(:,72),ZERO,0_intkind1,wf(:,111))
  call vert_AV_Q(wf(:,111),wf(:,23),wf(:,112))
  call vert_AZ_Q(gZd,wf(:,111),wf(:,4),wf(:,113))
  call counter_QA_V(wf(:,24),wf(:,-3),wf(:,114))
  call vert_AV_Q(wf(:,-5),wf(:,114),wf(:,115))
  call vert_VQ_A(wf(:,114),wf(:,-4),wf(:,116))
  call counter_QA_V(wf(:,35),wf(:,-3),wf(:,117))
  call vert_QA_V(wf(:,-2),wf(:,109),wf(:,118))
  call vert_QA_V(wf(:,-2),wf(:,111),wf(:,119))
  call vert_AV_Q(wf(:,-5),wf(:,119),wf(:,120))
  call vert_VQ_A(wf(:,119),wf(:,-4),wf(:,121))
  call counter_VQ_A(wf(:,23),wf(:,-2),wf(:,122))
  call prop_A_Q(wf(:,37),Q(:,75),ZERO,0_intkind1,wf(:,123))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,124))
  call prop_A_Q(wf(:,34),Q(:,120),ZERO,0_intkind1,wf(:,125))
  call prop_Q_A(wf(:,122),Q(:,52),ZERO,0_intkind1,wf(:,126))
  call vert_VQ_A(wf(:,-6),wf(:,126),wf(:,127))
  call prop_Q_A(wf(:,124),Q(:,7),ZERO,0_intkind1,wf(:,128))
  call vert_VQ_A(wf(:,-6),wf(:,128),wf(:,129))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,130))
  call prop_Q_A(wf(:,130),Q(:,68),ZERO,0_intkind1,wf(:,131))
  call vert_VQ_A(wf(:,23),wf(:,131),wf(:,132))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,131),wf(:,133))
  call counter_QA_V(wf(:,-2),wf(:,32),wf(:,134))
  call vert_AV_Q(wf(:,-5),wf(:,134),wf(:,135))
  call vert_VQ_A(wf(:,134),wf(:,-4),wf(:,136))
  call counter_QA_V(wf(:,-2),wf(:,27),wf(:,137))
  call vert_QA_V(wf(:,128),wf(:,-3),wf(:,138))
  call vert_QA_V(wf(:,131),wf(:,-3),wf(:,139))
  call vert_AV_Q(wf(:,-5),wf(:,139),wf(:,140))
  call vert_VQ_A(wf(:,139),wf(:,-4),wf(:,141))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,142))
  call vert_VQ_A(wf(:,142),wf(:,5),wf(:,143))
  call vert_AV_Q(wf(:,-5),wf(:,142),wf(:,144))
  call prop_A_Q(wf(:,144),Q(:,44),ZERO,0_intkind1,wf(:,145))
  call vert_AV_Q(wf(:,13),wf(:,142),wf(:,146))
  call vert_VQ_A(wf(:,142),wf(:,-4),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,28),ZERO,0_intkind1,wf(:,148))
  call vert_VQ_A(wf(:,-6),wf(:,148),wf(:,149))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,150))
  call vert_AV_Q(wf(:,-5),wf(:,150),wf(:,151))
  call vert_VQ_A(wf(:,150),wf(:,5),wf(:,152))
  call counter_Q_A(ctqq,wf(:,5),Q(:,80),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,80),ZERO,0_intkind1,wf(:,154))
  call vert_VQ_A(wf(:,2),wf(:,154),wf(:,155))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,154),wf(:,156))
  call counter_A_Q(ctqq,wf(:,8),Q(:,35),wf(:,157))
  call counter_A_Q(ctqq,wf(:,11),Q(:,44),wf(:,158))
  call vert_VQ_A(wf(:,150),wf(:,-4),wf(:,159))
  call vert_VQ_A(wf(:,150),wf(:,16),wf(:,160))
  call counter_Q_A(ctqq,wf(:,16),Q(:,19),wf(:,161))
  call counter_Q_A(ctqq,wf(:,19),Q(:,28),wf(:,162))
  call counter_A_Q(ctqq,wf(:,13),Q(:,96),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,96),ZERO,0_intkind1,wf(:,164))
  call vert_AV_Q(wf(:,164),wf(:,2),wf(:,165))
  call vert_AZ_Q(gZd,wf(:,164),wf(:,4),wf(:,166))
  call vert_AV_Q(wf(:,8),wf(:,-6),wf(:,167))
  call prop_Q_A(wf(:,159),Q(:,28),ZERO,0_intkind1,wf(:,168))
  call prop_A_Q(wf(:,151),Q(:,44),ZERO,0_intkind1,wf(:,169))
  call vert_AV_Q(wf(:,11),wf(:,-6),wf(:,170))
  call prop_A_Q(wf(:,170),Q(:,108),ZERO,0_intkind1,wf(:,171))
  call prop_A_Q(wf(:,167),Q(:,99),ZERO,0_intkind1,wf(:,172))
  call prop_Q_A(wf(:,20),Q(:,83),ZERO,0_intkind1,wf(:,173))
  call prop_Q_A(wf(:,21),Q(:,92),ZERO,0_intkind1,wf(:,174))
  call counter_Q_A(ctqq,wf(:,24),Q(:,68),wf(:,175))
  call prop_Q_A(wf(:,175),Q(:,68),ZERO,0_intkind1,wf(:,176))
  call vert_QA_V(wf(:,176),wf(:,27),wf(:,177))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,176),wf(:,178))
  call counter_V_V(ctGG,wf(:,23),Q(:,48),wf(:,179))
  call vert_AV_Q(wf(:,-3),wf(:,179),wf(:,180))
  call counter_A_Q(ctqq,wf(:,27),Q(:,11),wf(:,181))
  call counter_A_Q(ctqq,wf(:,30),Q(:,56),wf(:,182))
  call vert_VQ_A(wf(:,179),wf(:,24),wf(:,183))
  call vert_VQ_A(wf(:,179),wf(:,-2),wf(:,184))
  call counter_Q_A(ctqq,wf(:,35),Q(:,7),wf(:,185))
  call counter_Q_A(ctqq,wf(:,38),Q(:,52),wf(:,186))
  call counter_A_Q(ctqq,wf(:,32),Q(:,72),wf(:,187))
  call prop_A_Q(wf(:,187),Q(:,72),ZERO,0_intkind1,wf(:,188))
  call vert_QA_V(wf(:,35),wf(:,188),wf(:,189))
  call vert_AZ_Q(gZd,wf(:,188),wf(:,4),wf(:,190))
  call vert_VQ_A(wf(:,179),wf(:,35),wf(:,191))
  call vert_AV_Q(wf(:,27),wf(:,-6),wf(:,192))
  call prop_Q_A(wf(:,184),Q(:,52),ZERO,0_intkind1,wf(:,193))
  call vert_AV_Q(wf(:,30),wf(:,-6),wf(:,194))
  call prop_A_Q(wf(:,194),Q(:,120),ZERO,0_intkind1,wf(:,195))
  call prop_A_Q(wf(:,192),Q(:,75),ZERO,0_intkind1,wf(:,196))
  call prop_A_Q(wf(:,180),Q(:,56),ZERO,0_intkind1,wf(:,197))
  call prop_Q_A(wf(:,39),Q(:,71),ZERO,0_intkind1,wf(:,198))
  call prop_Q_A(wf(:,40),Q(:,116),ZERO,0_intkind1,wf(:,199))
  call vert_QA_V(wf(:,176),wf(:,-3),wf(:,200))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,201))
  call vert_QA_V(wf(:,-4),wf(:,8),wf(:,202))
  call counter_V_V(ctGG,wf(:,41),Q(:,76),wf(:,203))
  call prop_A_Q(wf(:,42),Q(:,108),ZERO,0_intkind1,wf(:,204))
  call prop_Q_A(wf(:,43),Q(:,92),ZERO,0_intkind1,wf(:,205))
  call vert_QA_V(wf(:,-2),wf(:,188),wf(:,206))
  call counter_V_V(ctGG,wf(:,44),Q(:,76),wf(:,207))
  call prop_A_Q(wf(:,45),Q(:,108),ZERO,0_intkind1,wf(:,208))
  call prop_Q_A(wf(:,46),Q(:,92),ZERO,0_intkind1,wf(:,209))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,210))
  call prop_A_Q(wf(:,210),Q(:,120),ZERO,0_intkind1,wf(:,211))
  call vert_VQ_A(wf(:,47),wf(:,-2),wf(:,212))
  call prop_Q_A(wf(:,212),Q(:,116),ZERO,0_intkind1,wf(:,213))
  call counter_V_V(ctGG,wf(:,47),Q(:,112),wf(:,214))
  call vert_QA_V(wf(:,154),wf(:,-5),wf(:,215))
  call vert_AV_Q(wf(:,-3),wf(:,50),wf(:,216))
  call prop_A_Q(wf(:,216),Q(:,120),ZERO,0_intkind1,wf(:,217))
  call vert_VQ_A(wf(:,50),wf(:,-2),wf(:,218))
  call prop_Q_A(wf(:,218),Q(:,116),ZERO,0_intkind1,wf(:,219))
  call counter_V_V(ctGG,wf(:,50),Q(:,112),wf(:,220))
  call vert_QA_V(wf(:,-4),wf(:,164),wf(:,221))
  call vert_VQ_A(wf(:,2),wf(:,16),wf(:,222))
  call prop_Q_A(wf(:,222),Q(:,31),ZERO,0_intkind1,wf(:,223))
  call vert_AV_Q(wf(:,8),wf(:,2),wf(:,224))
  call prop_A_Q(wf(:,224),Q(:,47),ZERO,0_intkind1,wf(:,225))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,19),wf(:,226))
  call prop_Q_A(wf(:,226),Q(:,31),ZERO,0_intkind1,wf(:,227))
  call vert_AZ_Q(gZd,wf(:,11),wf(:,4),wf(:,228))
  call prop_A_Q(wf(:,228),Q(:,47),ZERO,0_intkind1,wf(:,229))
  call vert_VQ_A(wf(:,23),wf(:,35),wf(:,230))
  call prop_Q_A(wf(:,230),Q(:,55),ZERO,0_intkind1,wf(:,231))
  call vert_AV_Q(wf(:,27),wf(:,23),wf(:,232))
  call prop_A_Q(wf(:,232),Q(:,59),ZERO,0_intkind1,wf(:,233))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,38),wf(:,234))
  call prop_Q_A(wf(:,234),Q(:,55),ZERO,0_intkind1,wf(:,235))
  call vert_AZ_Q(gZd,wf(:,30),wf(:,4),wf(:,236))
  call prop_A_Q(wf(:,236),Q(:,59),ZERO,0_intkind1,wf(:,237))
  call vert_QA_V(wf(:,24),wf(:,27),wf(:,238))
  call vert_QA_V(wf(:,105),wf(:,-3),wf(:,239))
  call vert_QA_V(wf(:,35),wf(:,32),wf(:,240))
  call vert_QA_V(wf(:,-2),wf(:,123),wf(:,241))
  call vert_QA_V(wf(:,5),wf(:,8),wf(:,242))
  call vert_QA_V(wf(:,60),wf(:,-5),wf(:,243))
  call vert_QA_V(wf(:,16),wf(:,13),wf(:,244))
  call vert_QA_V(wf(:,-4),wf(:,70),wf(:,245))
  call vert_VQ_A(wf(:,48),wf(:,-4),wf(:,246))
  call prop_Q_A(wf(:,246),Q(:,31),ZERO,0_intkind1,wf(:,247))
  call vert_AV_Q(wf(:,-5),wf(:,48),wf(:,248))
  call prop_A_Q(wf(:,248),Q(:,47),ZERO,0_intkind1,wf(:,249))
  call vert_QA_V(wf(:,198),wf(:,-3),wf(:,250))
  call vert_VQ_A(wf(:,49),wf(:,-4),wf(:,251))
  call prop_Q_A(wf(:,251),Q(:,31),ZERO,0_intkind1,wf(:,252))
  call vert_AV_Q(wf(:,-5),wf(:,49),wf(:,253))
  call prop_A_Q(wf(:,253),Q(:,47),ZERO,0_intkind1,wf(:,254))
  call vert_QA_V(wf(:,-2),wf(:,196),wf(:,255))
  call vert_VQ_A(wf(:,201),wf(:,-2),wf(:,256))
  call prop_Q_A(wf(:,256),Q(:,55),ZERO,0_intkind1,wf(:,257))
  call vert_AV_Q(wf(:,-3),wf(:,201),wf(:,258))
  call prop_A_Q(wf(:,258),Q(:,59),ZERO,0_intkind1,wf(:,259))
  call vert_QA_V(wf(:,173),wf(:,-5),wf(:,260))
  call vert_VQ_A(wf(:,202),wf(:,-2),wf(:,261))
  call prop_Q_A(wf(:,261),Q(:,55),ZERO,0_intkind1,wf(:,262))
  call vert_AV_Q(wf(:,-3),wf(:,202),wf(:,263))
  call prop_A_Q(wf(:,263),Q(:,59),ZERO,0_intkind1,wf(:,264))
  call vert_QA_V(wf(:,-4),wf(:,172),wf(:,265))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,35))
  den(9) = 1 / (Q(5,44))
  den(12) = 1 / (Q(5,96))
  den(14) = 1 / (Q(5,19))
  den(18) = 1 / (Q(5,28))
  den(23) = 1 / (Q(5,68))
  den(24) = 1 / (Q(5,48))
  den(26) = 1 / (Q(5,11))
  den(30) = 1 / (Q(5,56))
  den(33) = 1 / (Q(5,72))
  den(35) = 1 / (Q(5,7))
  den(39) = 1 / (Q(5,52))
  den(44) = 1 / (Q(5,76))
  den(51) = 1 / (Q(5,112))
  den(62) = 1 / (Q(5,83))
  den(65) = 1 / (Q(5,92))
  den(68) = 1 / (Q(5,99))
  den(71) = 1 / (Q(5,108))
  den(77) = 1 / (Q(5,71))
  den(80) = 1 / (Q(5,116))
  den(83) = 1 / (Q(5,75))
  den(86) = 1 / (Q(5,120))
  den(158) = 1 / (Q(5,51))
  den(180) = 1 / (Q(5,15))
  den(196) = 1 / (Q(5,31))
  den(199) = 1 / (Q(5,47))
  den(206) = 1 / (Q(5,55))
  den(209) = 1 / (Q(5,59))
  den(216) = 1 / (Q(5,79))
  den(223) = 1 / (Q(5,115))

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
  den(25) = den(23)*den(24)
  den(27) = den(1)*den(26)
  den(28) = den(25)*den(27)
  den(29) = den(1)*den(23)
  den(31) = den(24)*den(30)
  den(32) = den(29)*den(31)
  den(34) = den(24)*den(33)
  den(36) = den(1)*den(35)
  den(37) = den(34)*den(36)
  den(38) = den(1)*den(33)
  den(40) = den(24)*den(39)
  den(41) = den(38)*den(40)
  den(42) = den(31)*den(36)
  den(43) = den(27)*den(40)
  den(45) = den(23)*den(44)
  den(46) = den(15)*den(45)
  den(47) = den(6)*den(45)
  den(48) = den(33)*den(44)
  den(49) = den(15)*den(48)
  den(50) = den(6)*den(48)
  den(52) = den(3)*den(51)
  den(53) = den(36)*den(52)
  den(54) = den(27)*den(52)
  den(55) = den(12)*den(51)
  den(56) = den(36)*den(55)
  den(57) = den(27)*den(55)
  den(58) = den(1)*den(2)
  den(59) = den(24)*den(58)
  den(60) = den(52)*den(58)
  den(61) = den(55)*den(58)
  den(63) = den(8)*den(62)
  den(64) = den(2)*den(63)
  den(66) = den(4)*den(65)
  den(67) = den(1)*den(66)
  den(69) = den(17)*den(68)
  den(70) = den(2)*den(69)
  den(72) = den(13)*den(71)
  den(73) = den(1)*den(72)
  den(74) = den(1)*den(24)
  den(75) = den(45)*den(74)
  den(76) = den(48)*den(74)
  den(78) = den(29)*den(77)
  den(79) = den(24)*den(78)
  den(81) = den(25)*den(80)
  den(82) = den(1)*den(81)
  den(84) = den(38)*den(83)
  den(85) = den(24)*den(84)
  den(87) = den(34)*den(86)
  den(88) = den(1)*den(87)
  den(89) = den(2)**2
  den(90) = den(63)*den(89)
  den(91) = den(3)*den(89)
  den(92) = den(6)*den(91)
  den(93) = den(3)**2
  den(94) = den(2)*den(93)
  den(95) = den(6)*den(94)
  den(96) = den(1)*den(93)
  den(97) = den(10)*den(96)
  den(98) = den(6)*den(66)
  den(99) = den(10)*den(63)
  den(100) = den(69)*den(89)
  den(101) = den(15)*den(89)
  den(102) = den(12)*den(101)
  den(103) = den(15)*den(72)
  den(104) = den(19)*den(69)
  den(105) = den(12)**2
  den(106) = den(2)*den(105)
  den(107) = den(15)*den(106)
  den(108) = den(1)*den(105)
  den(109) = den(19)*den(108)
  den(110) = den(18)*den(89)
  den(111) = den(6)*den(110)
  den(112) = den(9)*den(89)
  den(113) = den(15)*den(112)
  den(114) = den(10)*den(71)
  den(115) = den(15)*den(114)
  den(116) = den(6)*den(68)
  den(117) = den(19)*den(116)
  den(118) = den(15)*den(62)
  den(119) = den(10)*den(118)
  den(120) = den(19)*den(65)
  den(121) = den(6)*den(120)
  den(122) = den(23)**2
  den(123) = den(27)*den(122)
  den(124) = den(24)*den(123)
  den(125) = den(1)*den(122)
  den(126) = den(31)*den(125)
  den(127) = den(24)**2
  den(128) = den(78)*den(127)
  den(129) = den(27)*den(81)
  den(130) = den(31)*den(78)
  den(131) = den(23)*den(127)
  den(132) = den(27)*den(131)
  den(133) = den(84)*den(127)
  den(134) = den(36)*den(87)
  den(135) = den(40)*den(84)
  den(136) = den(33)**2
  den(137) = den(36)*den(136)
  den(138) = den(24)*den(137)
  den(139) = den(1)*den(136)
  den(140) = den(40)*den(139)
  den(141) = den(36)*den(127)
  den(142) = den(33)*den(141)
  den(143) = den(39)*den(127)
  den(144) = den(27)*den(143)
  den(145) = den(31)*den(86)
  den(146) = den(36)*den(145)
  den(147) = den(27)*den(83)
  den(148) = den(40)*den(147)
  den(149) = den(30)*den(127)
  den(150) = den(36)*den(149)
  den(151) = den(36)*den(77)
  den(152) = den(31)*den(151)
  den(153) = den(40)*den(80)
  den(154) = den(27)*den(153)
  den(155) = den(44)*den(122)
  den(156) = den(15)*den(155)
  den(157) = den(6)*den(155)
  den(159) = den(6)*den(158)
  den(160) = den(45)*den(159)
  den(161) = den(15)*den(158)
  den(162) = den(45)*den(161)
  den(163) = den(45)*den(71)
  den(164) = den(15)*den(163)
  den(165) = den(45)*den(65)
  den(166) = den(6)*den(165)
  den(167) = den(44)*den(136)
  den(168) = den(15)*den(167)
  den(169) = den(6)*den(167)
  den(170) = den(48)*den(159)
  den(171) = den(48)*den(161)
  den(172) = den(48)*den(71)
  den(173) = den(15)*den(172)
  den(174) = den(48)*den(65)
  den(175) = den(6)*den(174)
  den(176) = den(52)*den(86)
  den(177) = den(36)*den(176)
  den(178) = den(52)*den(80)
  den(179) = den(27)*den(178)
  den(181) = den(27)*den(180)
  den(182) = den(52)*den(181)
  den(183) = den(36)*den(180)
  den(184) = den(52)*den(183)
  den(185) = den(93)*den(183)
  den(186) = den(93)*den(181)
  den(187) = den(55)*den(86)
  den(188) = den(36)*den(187)
  den(189) = den(55)*den(80)
  den(190) = den(27)*den(189)
  den(191) = den(55)*den(181)
  den(192) = den(55)*den(183)
  den(193) = den(105)*den(183)
  den(194) = den(105)*den(181)
  den(195) = den(2)*den(15)
  den(197) = den(195)*den(196)
  den(198) = den(2)*den(6)
  den(200) = den(198)*den(199)
  den(201) = den(1)*den(19)
  den(202) = den(196)*den(201)
  den(203) = den(1)*den(10)
  den(204) = den(199)*den(203)
  den(205) = den(24)*den(36)
  den(207) = den(205)*den(206)
  den(208) = den(24)*den(27)
  den(210) = den(208)*den(209)
  den(211) = den(1)*den(40)
  den(212) = den(206)*den(211)
  den(213) = den(1)*den(31)
  den(214) = den(209)*den(213)
  den(215) = den(23)*den(27)
  den(217) = den(215)*den(216)
  den(218) = den(78)*den(216)
  den(219) = den(33)*den(36)
  den(220) = den(216)*den(219)
  den(221) = den(84)*den(216)
  den(222) = den(3)*den(6)
  den(224) = den(222)*den(223)
  den(225) = den(63)*den(223)
  den(226) = den(12)*den(15)
  den(227) = den(223)*den(226)
  den(228) = den(69)*den(223)
  den(229) = den(183)*den(196)
  den(230) = den(183)*den(199)
  den(231) = den(151)*den(216)
  den(232) = den(181)*den(196)
  den(233) = den(181)*den(199)
  den(234) = den(147)*den(216)
  den(235) = den(161)*den(206)
  den(236) = den(161)*den(209)
  den(237) = den(118)*den(223)
  den(238) = den(159)*den(206)
  den(239) = den(159)*den(209)
  den(240) = den(116)*den(223)
  den(241) = den(1)*den(2)*den(24)
  den(242) = den(2)*den(3)*den(6)
  den(243) = den(1)*den(3)*den(10)
  den(244) = den(1)*den(2)*den(52)
  den(245) = den(1)*den(2)*den(3)
  den(246) = den(2)*den(12)*den(15)
  den(247) = den(1)*den(12)*den(19)
  den(248) = den(1)*den(2)*den(55)
  den(249) = den(1)*den(2)*den(12)
  den(250) = den(2)*den(161)
  den(251) = den(2)*den(118)
  den(252) = den(2)*den(159)
  den(253) = den(2)*den(116)
  den(254) = den(1)*den(120)
  den(255) = den(1)*den(114)
  den(256) = den(23)*den(24)*den(27)
  den(257) = den(1)*den(24)*den(45)
  den(258) = den(1)*den(23)*den(31)
  den(259) = den(1)*den(23)*den(24)
  den(260) = den(24)*den(33)*den(36)
  den(261) = den(1)*den(24)*den(48)
  den(262) = den(1)*den(33)*den(40)
  den(263) = den(1)*den(24)*den(33)
  den(264) = den(24)*den(183)
  den(265) = den(24)*den(151)
  den(266) = den(24)*den(181)
  den(267) = den(24)*den(147)
  den(268) = den(1)*den(153)
  den(269) = den(1)*den(145)
  den(270) = den(23)*den(161)
  den(271) = den(15)*den(23)
  den(272) = den(23)*den(159)
  den(273) = den(6)*den(23)
  den(274) = den(1)*den(165)
  den(275) = den(1)*den(163)
  den(276) = den(1)*den(45)
  den(277) = den(33)*den(161)
  den(278) = den(15)*den(33)
  den(279) = den(33)*den(159)
  den(280) = den(6)*den(33)
  den(281) = den(1)*den(174)
  den(282) = den(1)*den(172)
  den(283) = den(1)*den(48)
  den(284) = den(3)*den(183)
  den(285) = den(3)*den(36)
  den(286) = den(3)*den(181)
  den(287) = den(3)*den(27)
  den(288) = den(1)*den(178)
  den(289) = den(1)*den(176)
  den(290) = den(1)*den(52)
  den(291) = den(12)*den(183)
  den(292) = den(12)*den(36)
  den(293) = den(12)*den(181)
  den(294) = den(12)*den(27)
  den(295) = den(1)*den(189)
  den(296) = den(1)*den(187)
  den(297) = den(1)*den(55)
  den(298) = den(3)*den(200)
  den(299) = den(2)*den(224)
  den(300) = den(3)*den(204)
  den(301) = den(2)*den(225)
  den(302) = den(12)*den(197)
  den(303) = den(2)*den(227)
  den(304) = den(12)*den(202)
  den(305) = den(2)*den(228)
  den(306) = den(2)*den(237)
  den(307) = den(2)*den(240)
  den(308) = den(24)*den(217)
  den(309) = den(23)*den(210)
  den(310) = den(24)*den(218)
  den(311) = den(23)*den(214)
  den(312) = den(24)*den(220)
  den(313) = den(33)*den(207)
  den(314) = den(33)*den(212)
  den(315) = den(24)*den(221)
  den(316) = den(24)*den(231)
  den(317) = den(24)*den(234)
  den(318) = den(23)*den(236)
  den(319) = den(23)*den(239)
  den(320) = den(33)*den(235)
  den(321) = den(33)*den(238)
  den(322) = den(3)*den(230)
  den(323) = den(3)*den(233)
  den(324) = den(12)*den(229)
  den(325) = den(12)*den(232)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(165)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_QA(wf(:,11),wf(:,20)) * den(21)
  A(6) = cont_QA(wf(:,8),wf(:,21)) * den(22)
  A(7) = cont_QA(wf(:,26),wf(:,27)) * den(28)
  A(8) = cont_QA(wf(:,29),wf(:,30)) * den(32)
  A(9) = cont_QA(wf(:,34),wf(:,35)) * den(37)
  A(10) = cont_QA(wf(:,37),wf(:,38)) * den(41)
  A(11) = cont_QA(wf(:,30),wf(:,39)) * den(42)
  A(12) = cont_QA(wf(:,27),wf(:,40)) * den(43)
  A(13) = cont_QA(wf(:,16),wf(:,42)) * den(46)
  A(14) = cont_QA(wf(:,8),wf(:,43)) * den(47)
  A(15) = cont_QA(wf(:,16),wf(:,45)) * den(49)
  A(16) = cont_QA(wf(:,8),wf(:,46)) * den(50)
  A(17) = cont_VV(wf(:,47),wf(:,48)) * den(53)
  A(18) = cont_VV(wf(:,47),wf(:,49)) * den(54)
  A(19) = cont_VV(wf(:,48),wf(:,50)) * den(56)
  A(20) = cont_VV(wf(:,49),wf(:,50)) * den(57)

  A(21) = cont_VV(wf(:,23),wf(:,51)) * den(59)
  A(22) = cont_QA(wf(:,8),wf(:,52)) * den(7)
  A(23) = cont_QA(wf(:,11),wf(:,53)) * den(11)
  A(24) = cont_VV(wf(:,47),wf(:,54)) * den(60)
  A(25) = cont_QA(wf(:,16),wf(:,55)) * den(16)
  A(26) = cont_QA(wf(:,19),wf(:,56)) * den(20)
  A(27) = cont_VV(wf(:,50),wf(:,54)) * den(61)
  A(28) = cont_QA(wf(:,11),wf(:,57)) * den(21)
  A(29) = cont_QA(wf(:,8),wf(:,58)) * den(22)
  A(30) = cont_QA(wf(:,59),wf(:,60)) * den(64)
  A(31) = cont_QA(wf(:,61),wf(:,62)) * den(67)
  A(32) = cont_QA(wf(:,20),wf(:,63)) * den(21)
  A(33) = cont_QA(wf(:,21),wf(:,64)) * den(22)
  A(34) = cont_QA(wf(:,16),wf(:,67)) * den(16)
  A(35) = cont_QA(wf(:,19),wf(:,68)) * den(20)
  A(36) = cont_QA(wf(:,69),wf(:,70)) * den(70)
  A(37) = cont_QA(wf(:,71),wf(:,72)) * den(73)
  A(38) = cont_QA(wf(:,8),wf(:,74)) * den(22)
  A(39) = cont_QA(wf(:,11),wf(:,76)) * den(21)
  A(40) = cont_QA(wf(:,8),wf(:,79)) * den(7)
  A(41) = cont_QA(wf(:,11),wf(:,80)) * den(11)
  A(42) = cont_QA(wf(:,27),wf(:,81)) * den(28)
  A(43) = cont_VV(wf(:,41),wf(:,82)) * den(75)
  A(44) = cont_QA(wf(:,30),wf(:,83)) * den(32)
  A(45) = cont_QA(wf(:,35),wf(:,84)) * den(37)
  A(46) = cont_VV(wf(:,44),wf(:,82)) * den(76)
  A(47) = cont_QA(wf(:,38),wf(:,85)) * den(41)
  A(48) = cont_QA(wf(:,30),wf(:,86)) * den(42)
  A(49) = cont_QA(wf(:,27),wf(:,87)) * den(43)
  A(50) = cont_QA(wf(:,16),wf(:,88)) * den(46)
  A(51) = cont_QA(wf(:,43),wf(:,64)) * den(47)
  A(52) = cont_QA(wf(:,16),wf(:,89)) * den(49)
  A(53) = cont_QA(wf(:,46),wf(:,64)) * den(50)
  A(54) = cont_VV(wf(:,48),wf(:,90)) * den(53)
  A(55) = cont_VV(wf(:,49),wf(:,90)) * den(54)
  A(56) = cont_VV(wf(:,48),wf(:,91)) * den(56)
  A(57) = cont_VV(wf(:,49),wf(:,91)) * den(57)
  A(58) = cont_QA(wf(:,8),wf(:,92)) * den(47)
  A(59) = cont_QA(wf(:,42),wf(:,75)) * den(46)
  A(60) = cont_QA(wf(:,8),wf(:,93)) * den(50)
  A(61) = cont_QA(wf(:,45),wf(:,75)) * den(49)
  A(62) = cont_VV(wf(:,48),wf(:,94)) * den(56)
  A(63) = cont_VV(wf(:,49),wf(:,94)) * den(57)
  A(64) = cont_VV(wf(:,48),wf(:,95)) * den(53)
  A(65) = cont_VV(wf(:,49),wf(:,95)) * den(54)
  A(66) = cont_QA(wf(:,27),wf(:,97)) * den(28)
  A(67) = cont_QA(wf(:,29),wf(:,99)) * den(32)
  A(68) = cont_QA(wf(:,35),wf(:,100)) * den(37)
  A(69) = cont_QA(wf(:,37),wf(:,102)) * den(41)
  A(70) = cont_QA(wf(:,39),wf(:,99)) * den(42)
  A(71) = cont_QA(wf(:,27),wf(:,103)) * den(43)
  A(72) = cont_QA(wf(:,104),wf(:,105)) * den(79)
  A(73) = cont_QA(wf(:,106),wf(:,107)) * den(82)
  A(74) = cont_QA(wf(:,39),wf(:,108)) * den(42)
  A(75) = cont_QA(wf(:,40),wf(:,109)) * den(43)
  A(76) = cont_QA(wf(:,35),wf(:,112)) * den(37)
  A(77) = cont_QA(wf(:,38),wf(:,113)) * den(41)
  A(78) = cont_QA(wf(:,16),wf(:,115)) * den(46)
  A(79) = cont_QA(wf(:,8),wf(:,116)) * den(47)
  A(80) = cont_VV(wf(:,47),wf(:,117)) * den(53)
  A(81) = cont_VV(wf(:,47),wf(:,118)) * den(54)
  A(82) = cont_VV(wf(:,50),wf(:,117)) * den(56)
  A(83) = cont_VV(wf(:,50),wf(:,118)) * den(57)
  A(84) = cont_QA(wf(:,16),wf(:,120)) * den(49)
  A(85) = cont_QA(wf(:,8),wf(:,121)) * den(50)
  A(86) = cont_QA(wf(:,122),wf(:,123)) * den(85)
  A(87) = cont_QA(wf(:,124),wf(:,125)) * den(88)
  A(88) = cont_QA(wf(:,27),wf(:,127)) * den(43)
  A(89) = cont_QA(wf(:,30),wf(:,129)) * den(42)
  A(90) = cont_QA(wf(:,27),wf(:,132)) * den(28)
  A(91) = cont_QA(wf(:,30),wf(:,133)) * den(32)
  A(92) = cont_QA(wf(:,16),wf(:,135)) * den(49)
  A(93) = cont_QA(wf(:,8),wf(:,136)) * den(50)
  A(94) = cont_VV(wf(:,47),wf(:,137)) * den(54)
  A(95) = cont_VV(wf(:,47),wf(:,138)) * den(53)
  A(96) = cont_VV(wf(:,50),wf(:,137)) * den(57)
  A(97) = cont_VV(wf(:,50),wf(:,138)) * den(56)
  A(98) = cont_QA(wf(:,16),wf(:,140)) * den(46)
  A(99) = cont_QA(wf(:,8),wf(:,141)) * den(47)
  A(100) = cont_QA(wf(:,8),wf(:,143)) * den(7)
  A(101) = cont_QA(wf(:,10),wf(:,145)) * den(11)
  A(102) = cont_QA(wf(:,16),wf(:,146)) * den(16)
  A(103) = cont_QA(wf(:,18),wf(:,148)) * den(20)
  A(104) = cont_QA(wf(:,20),wf(:,145)) * den(21)
  A(105) = cont_QA(wf(:,8),wf(:,149)) * den(22)
  A(106) = cont_QA(wf(:,60),wf(:,151)) * den(90)
  A(107) = cont_QA(wf(:,8),wf(:,152)) * den(92)
  A(108) = cont_QA(wf(:,8),wf(:,155)) * den(95)
  A(109) = cont_QA(wf(:,11),wf(:,156)) * den(97)
  A(110) = cont_QA(wf(:,62),wf(:,157)) * den(98)
  A(111) = cont_QA(wf(:,60),wf(:,158)) * den(99)
  A(112) = cont_QA(wf(:,70),wf(:,159)) * den(100)
  A(113) = cont_QA(wf(:,13),wf(:,160)) * den(102)
  A(114) = cont_QA(wf(:,72),wf(:,161)) * den(103)
  A(115) = cont_QA(wf(:,70),wf(:,162)) * den(104)
  A(116) = cont_QA(wf(:,16),wf(:,165)) * den(107)
  A(117) = cont_QA(wf(:,19),wf(:,166)) * den(109)
  A(118) = cont_QA(wf(:,167),wf(:,168)) * den(111)
  A(119) = cont_QA(wf(:,20),wf(:,169)) * den(113)
  A(120) = cont_QA(wf(:,161),wf(:,171)) * den(115)
  A(121) = cont_QA(wf(:,162),wf(:,172)) * den(117)
  A(122) = cont_QA(wf(:,158),wf(:,173)) * den(119)
  A(123) = cont_QA(wf(:,157),wf(:,174)) * den(121)
  A(124) = cont_VV(wf(:,23),wf(:,177)) * den(124)
  A(125) = cont_QA(wf(:,30),wf(:,178)) * den(126)
  A(126) = cont_QA(wf(:,105),wf(:,180)) * den(128)
  A(127) = cont_QA(wf(:,107),wf(:,181)) * den(129)
  A(128) = cont_QA(wf(:,105),wf(:,182)) * den(130)
  A(129) = cont_QA(wf(:,27),wf(:,183)) * den(132)
  A(130) = cont_QA(wf(:,123),wf(:,184)) * den(133)
  A(131) = cont_QA(wf(:,125),wf(:,185)) * den(134)
  A(132) = cont_QA(wf(:,123),wf(:,186)) * den(135)
  A(133) = cont_VV(wf(:,23),wf(:,189)) * den(138)
  A(134) = cont_QA(wf(:,38),wf(:,190)) * den(140)
  A(135) = cont_QA(wf(:,32),wf(:,191)) * den(142)
  A(136) = cont_QA(wf(:,192),wf(:,193)) * den(144)
  A(137) = cont_QA(wf(:,185),wf(:,195)) * den(146)
  A(138) = cont_QA(wf(:,186),wf(:,196)) * den(148)
  A(139) = cont_QA(wf(:,39),wf(:,197)) * den(150)
  A(140) = cont_QA(wf(:,182),wf(:,198)) * den(152)
  A(141) = cont_QA(wf(:,181),wf(:,199)) * den(154)
  A(142) = cont_VV(wf(:,200),wf(:,201)) * den(156)
  A(143) = cont_VV(wf(:,200),wf(:,202)) * den(157)
  A(144) = cont_VV(wf(:,202),wf(:,203)) * den(160)
  A(145) = cont_VV(wf(:,201),wf(:,203)) * den(162)
  A(146) = cont_QA(wf(:,161),wf(:,204)) * den(164)
  A(147) = cont_QA(wf(:,157),wf(:,205)) * den(166)
  A(148) = cont_VV(wf(:,201),wf(:,206)) * den(168)
  A(149) = cont_VV(wf(:,202),wf(:,206)) * den(169)
  A(150) = cont_VV(wf(:,202),wf(:,207)) * den(170)
  A(151) = cont_VV(wf(:,201),wf(:,207)) * den(171)
  A(152) = cont_QA(wf(:,161),wf(:,208)) * den(173)
  A(153) = cont_QA(wf(:,157),wf(:,209)) * den(175)
  A(154) = cont_QA(wf(:,185),wf(:,211)) * den(177)
  A(155) = cont_QA(wf(:,181),wf(:,213)) * den(179)
  A(156) = cont_VV(wf(:,49),wf(:,214)) * den(182)
  A(157) = cont_VV(wf(:,48),wf(:,214)) * den(184)
  A(158) = cont_VV(wf(:,48),wf(:,215)) * den(185)
  A(159) = cont_VV(wf(:,49),wf(:,215)) * den(186)
  A(160) = cont_QA(wf(:,185),wf(:,217)) * den(188)
  A(161) = cont_QA(wf(:,181),wf(:,219)) * den(190)
  A(162) = cont_VV(wf(:,49),wf(:,220)) * den(191)
  A(163) = cont_VV(wf(:,48),wf(:,220)) * den(192)
  A(164) = cont_VV(wf(:,48),wf(:,221)) * den(193)
  A(165) = cont_VV(wf(:,49),wf(:,221)) * den(194)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(165)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(7)-A(8)-A(9)-A(10)-A(11)-A(12)-A(13)-A(14)-A(15)-A(16)-A(17)-A(18)-A(19) &
       -A(20))*f(1))/2._/**/REALKIND
  M1(2) = ((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(7)+A(8)+A(9)+A(10)+A(11)+A(12)+A(13)+A(14)+A(15)+A(16)+A(17)+A(18)+A(19) &
       +A(20))*f(1))/6._/**/REALKIND

  M2(1) = ((A(106)+A(107)+A(108)+A(109)+A(110)+A(111)+A(112)+A(113)+A(114)+A(115)+A(116)+A(117)+A(118)+A(119)+A(120)+A(121)+A(122) &
       +A(123)+A(124)+A(125)+A(126)+A(127)+A(128)+A(129)+A(130)+A(131)+A(132)+A(133)+A(134)+A(135)+A(136)+A(137)+A(138)+A(139) &
       +A(140)+A(141)+A(142)+A(143)+A(144)+A(145)+A(146)+A(147)+A(148)+A(149)+A(150)+A(151)+A(152)+A(153)+A(154)+A(155)+A(156) &
       +A(157)+A(158)+A(159)+A(160)+A(161)+A(162)+A(163)+A(164)+A(165))*f(2))/2._/**/REALKIND-(A(21)*f(3))/2._/**/REALKIND+(( &
       -A(22)-A(25)-A(30)-A(32)-A(36)-A(38)-A(42)-A(45)-A(50)-A(52)-A(54)-A(55)-A(58)-A(60)-A(62)-A(63)-A(66)-A(67)-A(68)-A(69) &
       -A(70)-A(71)-A(72)-A(74)-A(78)-A(79)-A(80)-A(82)-A(86)-A(88)-A(92)-A(93)-A(94)-A(96)-A(100)-A(101)-A(102)-A(103)-A(104) &
       -A(105))*f(4))/2._/**/REALKIND+((-A(23)-A(26)-A(28)-A(29)-A(31)-A(33)-A(34)-A(35)-A(37)-A(39)-A(40)-A(41)-A(44)-A(47)-A(48) &
       -A(49)-A(51)-A(53)-A(56)-A(57)-A(59)-A(61)-A(64)-A(65)-A(73)-A(75)-A(76)-A(77)-A(81)-A(83)-A(84)-A(85)-A(87)-A(89)-A(90) &
       -A(91)-A(95)-A(97)-A(98)-A(99))*f(5))/2._/**/REALKIND+((A(24)+A(27)+A(43)+A(46))*f(6))/2._/**/REALKIND
  M2(2) = ((-A(106)-A(107)-A(108)-A(109)-A(110)-A(111)-A(112)-A(113)-A(114)-A(115)-A(116)-A(117)-A(118)-A(119)-A(120)-A(121) &
       -A(122)-A(123)-A(124)-A(125)-A(126)-A(127)-A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(134)-A(135)-A(136)-A(137)-A(138) &
       -A(139)-A(140)-A(141)-A(142)-A(143)-A(144)-A(145)-A(146)-A(147)-A(148)-A(149)-A(150)-A(151)-A(152)-A(153)-A(154)-A(155) &
       -A(156)-A(157)-A(158)-A(159)-A(160)-A(161)-A(162)-A(163)-A(164)-A(165))*f(2))/6._/**/REALKIND+(A(21)*f(3))/6._/**/REALKIND &
       +((A(22)+A(25)+A(30)+A(32)+A(36)+A(38)+A(42)+A(45)+A(50)+A(52)+A(54)+A(55)+A(58)+A(60)+A(62)+A(63)+A(66)+A(67)+A(68)+A(69) &
       +A(70)+A(71)+A(72)+A(74)+A(78)+A(79)+A(80)+A(82)+A(86)+A(88)+A(92)+A(93)+A(94)+A(96)+A(100)+A(101)+A(102)+A(103)+A(104) &
       +A(105))*f(4))/6._/**/REALKIND+((A(23)+A(26)+A(28)+A(29)+A(31)+A(33)+A(34)+A(35)+A(37)+A(39)+A(40)+A(41)+A(44)+A(47)+A(48) &
       +A(49)+A(51)+A(53)+A(56)+A(57)+A(59)+A(61)+A(64)+A(65)+A(73)+A(75)+A(76)+A(77)+A(81)+A(83)+A(84)+A(85)+A(87)+A(89)+A(90) &
       +A(91)+A(95)+A(97)+A(98)+A(99))*f(5))/6._/**/REALKIND+((-A(24)-A(27)-A(43)-A(46))*f(6))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppllajj_nenexddxssxa_1_/**/REALKIND
