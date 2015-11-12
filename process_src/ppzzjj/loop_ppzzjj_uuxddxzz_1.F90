
module ol_colourmatrix_ppzzjj_uuxddxzz_1_/**/REALKIND
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
end module ol_colourmatrix_ppzzjj_uuxddxzz_1_/**/REALKIND



module ol_forced_parameters_ppzzjj_uuxddxzz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzzjj_uuxddxzz_1_/**/REALKIND

module ol_loop_ppzzjj_uuxddxzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(14), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:253)
  ! denominators
  complex(REALKIND), save :: den(270)
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
    f( 1) = CI*eQED**2*gQCD**2
    f( 2) = CI*countertermnorm*eQED**2*gQCD**4
    f( 3) = CI*countertermnorm*ctGqq*eQED**2*gQCD**4
    f( 4) = CI*countertermnorm*ctVqq*eQED**2*gQCD**4
    f( 5) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f( 6) = CI*countertermnorm*ctZZGG*eQED**2*gQCD**4
    f( 7) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/(cw**2*sw)
    f( 8) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f( 9) = (eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(10) = eQED**2*gQCD**4*integralnorm*SwB
    f(11) = eQED**2*gQCD**4*integralnorm*SwF
    f(12) = 2*eQED**2*gQCD**4*integralnorm*SwF
    f(13) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(14) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(8), 27*CI*f(8), 18*f(9), 54*f(9), f(10), 3*f(10), 6*f(10), 8*f(10), 10*f(10), 18*f(10), 21*f(10), 24*f(10) &
    , 54*f(10), 3*f(11), 9*f(11), 3*f(12), 9*f(12), 3*f(13), 9*f(13), 3*f(14), 9*f(14) ]
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
  complex(REALKIND) :: A(170)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))
  call wf_V(P(:,6), rMZ, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),ZERO,0_intkind1,wf(:,9))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,36),ZERO,0_intkind1,wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,24),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,12),wf(:,14))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,15))
  call vert_AZ_Q(gZd,wf(:,13),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,18))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,-4),wf(:,19))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,20))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-5),wf(:,21))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,22))
  call prop_Q_A(wf(:,20),Q(:,17),ZERO,0_intkind1,wf(:,23))
  call prop_A_Q(wf(:,21),Q(:,34),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,23),wf(:,24),wf(:,25))
  call vert_AV_Q(wf(:,-1),wf(:,22),wf(:,26))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,23),wf(:,27))
  call prop_A_Q(wf(:,26),Q(:,14),ZERO,0_intkind1,wf(:,28))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,0),wf(:,29))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-4),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,33),ZERO,0_intkind1,wf(:,31))
  call prop_A_Q(wf(:,30),Q(:,18),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,31),wf(:,32),wf(:,33))
  call vert_VQ_A(wf(:,22),wf(:,0),wf(:,34))
  call vert_AZ_Q(gZu,wf(:,32),wf(:,-5),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,13),ZERO,0_intkind1,wf(:,36))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,31),wf(:,37))
  call vert_AZ_Q(gZu,wf(:,24),wf(:,-4),wf(:,38))
  call vert_QA_V(wf(:,23),wf(:,-1),wf(:,39))
  call vert_QA_V(wf(:,12),wf(:,-3),wf(:,40))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,41))
  call vert_QA_V(wf(:,0),wf(:,32),wf(:,42))
  call vert_QA_V(wf(:,31),wf(:,-1),wf(:,43))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,44))
  call vert_QA_V(wf(:,0),wf(:,24),wf(:,45))
  call vert_QA_V(wf(:,-2),wf(:,13),wf(:,46))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,47))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,48))
  call counter_GG_S(wf(:,1),wf(:,22),wf(:,49))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,50))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,51))
  call counter_VG_G(wf(:,-5),wf(:,1),Q(:,3),wf(:,52),Q(:,35))
  call counter_VQ_A(wf(:,1),wf(:,12),wf(:,53))
  call counter_AZ_Q(gZd,wf(:,13),wf(:,-5),wf(:,54))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,55))
  call counter_VG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,56),Q(:,19))
  call counter_AZ_Q(gZd,wf(:,5),wf(:,-4),wf(:,57))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,58))
  call prop_Q_A(wf(:,8),Q(:,52),ZERO,0_intkind1,wf(:,59))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,60))
  call prop_A_Q(wf(:,60),Q(:,40),ZERO,0_intkind1,wf(:,61))
  call prop_Q_A(wf(:,18),Q(:,52),ZERO,0_intkind1,wf(:,62))
  call vert_AZ_Q(gZd,wf(:,61),wf(:,-4),wf(:,63))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,24),ZERO,0_intkind1,wf(:,65))
  call vert_AZ_Q(gZd,wf(:,65),wf(:,-5),wf(:,66))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,67))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,68))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,36),ZERO,0_intkind1,wf(:,70))
  call vert_VQ_A(wf(:,1),wf(:,70),wf(:,71))
  call prop_A_Q(wf(:,19),Q(:,56),ZERO,0_intkind1,wf(:,72))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,70),wf(:,73))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,20),ZERO,0_intkind1,wf(:,75))
  call vert_VQ_A(wf(:,1),wf(:,75),wf(:,76))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,75),wf(:,77))
  call counter_QA_V(wf(:,23),wf(:,24),wf(:,78))
  call counter_VG_G(wf(:,-5),wf(:,22),Q(:,12),wf(:,79),Q(:,44))
  call counter_ZQ_A(gZu,wf(:,-5),wf(:,23),wf(:,80))
  call counter_QA_V(wf(:,31),wf(:,32),wf(:,81))
  call counter_AZ_Q(gZu,wf(:,32),wf(:,-5),wf(:,82))
  call counter_VG_G(wf(:,-4),wf(:,22),Q(:,12),wf(:,83),Q(:,28))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,31),wf(:,84))
  call counter_AZ_Q(gZu,wf(:,24),wf(:,-4),wf(:,85))
  call counter_QA_V(wf(:,12),wf(:,-3),wf(:,86))
  call vert_QA_V(wf(:,-2),wf(:,61),wf(:,87))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,88))
  call vert_QA_V(wf(:,-2),wf(:,65),wf(:,89))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,90))
  call vert_QA_V(wf(:,70),wf(:,-3),wf(:,91))
  call counter_QA_V(wf(:,-2),wf(:,13),wf(:,92))
  call vert_QA_V(wf(:,75),wf(:,-3),wf(:,93))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,94))
  call vert_AV_Q(wf(:,-1),wf(:,94),wf(:,95))
  call prop_A_Q(wf(:,95),Q(:,14),ZERO,0_intkind1,wf(:,96))
  call vert_VQ_A(wf(:,94),wf(:,0),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,13),ZERO,0_intkind1,wf(:,98))
  call counter_AV_Q(wf(:,-1),wf(:,22),wf(:,99))
  call prop_Q_A(wf(:,27),Q(:,49),ZERO,0_intkind1,wf(:,100))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-5),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,34),ZERO,0_intkind1,wf(:,102))
  call vert_QA_V(wf(:,23),wf(:,102),wf(:,103))
  call prop_Q_A(wf(:,37),Q(:,49),ZERO,0_intkind1,wf(:,104))
  call vert_AZ_Q(gZu,wf(:,102),wf(:,-4),wf(:,105))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-4),wf(:,106))
  call prop_A_Q(wf(:,106),Q(:,18),ZERO,0_intkind1,wf(:,107))
  call vert_QA_V(wf(:,31),wf(:,107),wf(:,108))
  call vert_AZ_Q(gZu,wf(:,107),wf(:,-5),wf(:,109))
  call counter_QA_V(wf(:,23),wf(:,-1),wf(:,110))
  call counter_QA_V(wf(:,31),wf(:,-1),wf(:,111))
  call vert_QA_V(wf(:,0),wf(:,102),wf(:,112))
  call vert_QA_V(wf(:,0),wf(:,107),wf(:,113))
  call counter_VQ_A(wf(:,22),wf(:,0),wf(:,114))
  call prop_A_Q(wf(:,35),Q(:,50),ZERO,0_intkind1,wf(:,115))
  call counter_ZQ_A(gZu,wf(:,-5),wf(:,0),wf(:,116))
  call prop_Q_A(wf(:,116),Q(:,33),ZERO,0_intkind1,wf(:,117))
  call vert_QA_V(wf(:,117),wf(:,32),wf(:,118))
  call prop_A_Q(wf(:,38),Q(:,50),ZERO,0_intkind1,wf(:,119))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,117),wf(:,120))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,121))
  call prop_Q_A(wf(:,121),Q(:,17),ZERO,0_intkind1,wf(:,122))
  call vert_QA_V(wf(:,122),wf(:,24),wf(:,123))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,122),wf(:,124))
  call counter_QA_V(wf(:,0),wf(:,32),wf(:,125))
  call counter_QA_V(wf(:,0),wf(:,24),wf(:,126))
  call vert_QA_V(wf(:,117),wf(:,-1),wf(:,127))
  call vert_QA_V(wf(:,122),wf(:,-1),wf(:,128))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,129))
  call vert_VQ_A(wf(:,129),wf(:,4),wf(:,130))
  call vert_AV_Q(wf(:,-3),wf(:,129),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,11),ZERO,0_intkind1,wf(:,132))
  call vert_VQ_A(wf(:,129),wf(:,12),wf(:,133))
  call vert_VQ_A(wf(:,129),wf(:,-2),wf(:,134))
  call prop_Q_A(wf(:,134),Q(:,7),ZERO,0_intkind1,wf(:,135))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,136))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,137))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,138))
  call counter_Q_A(ctqq,wf(:,4),Q(:,20),wf(:,139))
  call prop_A_Q(wf(:,138),Q(:,43),ZERO,0_intkind1,wf(:,140))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,141))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,-3),wf(:,137),wf(:,143))
  call prop_Q_A(wf(:,139),Q(:,20),ZERO,0_intkind1,wf(:,144))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,144),wf(:,145))
  call counter_A_Q(ctqq,wf(:,9),Q(:,11),wf(:,146))
  call vert_QA_V(wf(:,12),wf(:,13),wf(:,147))
  call vert_AV_Q(wf(:,13),wf(:,1),wf(:,148))
  call counter_Q_A(ctqq,wf(:,12),Q(:,36),wf(:,149))
  call prop_A_Q(wf(:,148),Q(:,27),ZERO,0_intkind1,wf(:,150))
  call counter_A_Q(ctqq,wf(:,13),Q(:,24),wf(:,151))
  call prop_Q_A(wf(:,14),Q(:,39),ZERO,0_intkind1,wf(:,152))
  call vert_VQ_A(wf(:,137),wf(:,-2),wf(:,153))
  call counter_Q_A(ctqq,wf(:,17),Q(:,7),wf(:,154))
  call prop_A_Q(wf(:,151),Q(:,24),ZERO,0_intkind1,wf(:,155))
  call vert_AZ_Q(gZd,wf(:,155),wf(:,-5),wf(:,156))
  call prop_Q_A(wf(:,149),Q(:,36),ZERO,0_intkind1,wf(:,157))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,157),wf(:,158))
  call prop_A_Q(wf(:,141),Q(:,40),ZERO,0_intkind1,wf(:,159))
  call vert_AZ_Q(gZd,wf(:,159),wf(:,-4),wf(:,160))
  call vert_AV_Q(wf(:,24),wf(:,22),wf(:,161))
  call counter_Q_A(ctqq,wf(:,23),Q(:,17),wf(:,162))
  call prop_A_Q(wf(:,161),Q(:,46),ZERO,0_intkind1,wf(:,163))
  call vert_VQ_A(wf(:,22),wf(:,23),wf(:,164))
  call counter_A_Q(ctqq,wf(:,24),Q(:,34),wf(:,165))
  call prop_Q_A(wf(:,164),Q(:,29),ZERO,0_intkind1,wf(:,166))
  call counter_V_V(ctGG,wf(:,22),Q(:,12),wf(:,167))
  call prop_Q_A(wf(:,162),Q(:,17),ZERO,0_intkind1,wf(:,168))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,168),wf(:,169))
  call vert_AV_Q(wf(:,-1),wf(:,167),wf(:,170))
  call counter_A_Q(ctqq,wf(:,28),Q(:,14),wf(:,171))
  call vert_AV_Q(wf(:,32),wf(:,22),wf(:,172))
  call counter_Q_A(ctqq,wf(:,31),Q(:,33),wf(:,173))
  call prop_A_Q(wf(:,172),Q(:,30),ZERO,0_intkind1,wf(:,174))
  call vert_VQ_A(wf(:,22),wf(:,31),wf(:,175))
  call counter_A_Q(ctqq,wf(:,32),Q(:,18),wf(:,176))
  call prop_Q_A(wf(:,175),Q(:,45),ZERO,0_intkind1,wf(:,177))
  call vert_VQ_A(wf(:,167),wf(:,0),wf(:,178))
  call counter_Q_A(ctqq,wf(:,36),Q(:,13),wf(:,179))
  call prop_A_Q(wf(:,176),Q(:,18),ZERO,0_intkind1,wf(:,180))
  call vert_AZ_Q(gZu,wf(:,180),wf(:,-5),wf(:,181))
  call prop_Q_A(wf(:,173),Q(:,33),ZERO,0_intkind1,wf(:,182))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,182),wf(:,183))
  call prop_A_Q(wf(:,165),Q(:,34),ZERO,0_intkind1,wf(:,184))
  call vert_AZ_Q(gZu,wf(:,184),wf(:,-4),wf(:,185))
  call vert_QA_V(wf(:,168),wf(:,-1),wf(:,186))
  call counter_V_V(ctGG,wf(:,39),Q(:,19),wf(:,187))
  call vert_QA_V(wf(:,157),wf(:,-3),wf(:,188))
  call vert_QA_V(wf(:,-2),wf(:,159),wf(:,189))
  call vert_QA_V(wf(:,0),wf(:,180),wf(:,190))
  call counter_V_V(ctGG,wf(:,42),Q(:,19),wf(:,191))
  call vert_QA_V(wf(:,182),wf(:,-1),wf(:,192))
  call counter_V_V(ctGG,wf(:,43),Q(:,35),wf(:,193))
  call vert_QA_V(wf(:,144),wf(:,-3),wf(:,194))
  call vert_QA_V(wf(:,0),wf(:,184),wf(:,195))
  call counter_V_V(ctGG,wf(:,45),Q(:,35),wf(:,196))
  call vert_QA_V(wf(:,-2),wf(:,155),wf(:,197))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,17),wf(:,198))
  call prop_Q_A(wf(:,198),Q(:,23),ZERO,0_intkind1,wf(:,199))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,17),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,39),ZERO,0_intkind1,wf(:,201))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-4),wf(:,202))
  call prop_A_Q(wf(:,202),Q(:,27),ZERO,0_intkind1,wf(:,203))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-5),wf(:,204))
  call prop_A_Q(wf(:,204),Q(:,43),ZERO,0_intkind1,wf(:,205))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,36),wf(:,206))
  call prop_Q_A(wf(:,206),Q(:,29),ZERO,0_intkind1,wf(:,207))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,36),wf(:,208))
  call prop_Q_A(wf(:,208),Q(:,45),ZERO,0_intkind1,wf(:,209))
  call vert_AZ_Q(gZu,wf(:,28),wf(:,-4),wf(:,210))
  call prop_A_Q(wf(:,210),Q(:,30),ZERO,0_intkind1,wf(:,211))
  call vert_AZ_Q(gZu,wf(:,28),wf(:,-5),wf(:,212))
  call prop_A_Q(wf(:,212),Q(:,46),ZERO,0_intkind1,wf(:,213))
  call vert_VQ_A(wf(:,39),wf(:,-2),wf(:,214))
  call prop_Q_A(wf(:,214),Q(:,23),ZERO,0_intkind1,wf(:,215))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,216))
  call prop_A_Q(wf(:,216),Q(:,27),ZERO,0_intkind1,wf(:,217))
  call vert_QA_V(wf(:,100),wf(:,-1),wf(:,218))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,219))
  call prop_Q_A(wf(:,219),Q(:,23),ZERO,0_intkind1,wf(:,220))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,221))
  call prop_A_Q(wf(:,221),Q(:,27),ZERO,0_intkind1,wf(:,222))
  call vert_QA_V(wf(:,0),wf(:,115),wf(:,223))
  call vert_VQ_A(wf(:,44),wf(:,0),wf(:,224))
  call prop_Q_A(wf(:,224),Q(:,29),ZERO,0_intkind1,wf(:,225))
  call vert_AV_Q(wf(:,-1),wf(:,44),wf(:,226))
  call prop_A_Q(wf(:,226),Q(:,30),ZERO,0_intkind1,wf(:,227))
  call vert_QA_V(wf(:,59),wf(:,-3),wf(:,228))
  call vert_VQ_A(wf(:,46),wf(:,0),wf(:,229))
  call prop_Q_A(wf(:,229),Q(:,29),ZERO,0_intkind1,wf(:,230))
  call vert_AV_Q(wf(:,-1),wf(:,46),wf(:,231))
  call prop_A_Q(wf(:,231),Q(:,30),ZERO,0_intkind1,wf(:,232))
  call vert_QA_V(wf(:,-2),wf(:,68),wf(:,233))
  call vert_VQ_A(wf(:,43),wf(:,-2),wf(:,234))
  call prop_Q_A(wf(:,234),Q(:,39),ZERO,0_intkind1,wf(:,235))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,236))
  call prop_A_Q(wf(:,236),Q(:,43),ZERO,0_intkind1,wf(:,237))
  call vert_QA_V(wf(:,104),wf(:,-1),wf(:,238))
  call vert_VQ_A(wf(:,45),wf(:,-2),wf(:,239))
  call prop_Q_A(wf(:,239),Q(:,39),ZERO,0_intkind1,wf(:,240))
  call vert_AV_Q(wf(:,-3),wf(:,45),wf(:,241))
  call prop_A_Q(wf(:,241),Q(:,43),ZERO,0_intkind1,wf(:,242))
  call vert_QA_V(wf(:,0),wf(:,119),wf(:,243))
  call vert_VQ_A(wf(:,40),wf(:,0),wf(:,244))
  call prop_Q_A(wf(:,244),Q(:,45),ZERO,0_intkind1,wf(:,245))
  call vert_AV_Q(wf(:,-1),wf(:,40),wf(:,246))
  call prop_A_Q(wf(:,246),Q(:,46),ZERO,0_intkind1,wf(:,247))
  call vert_QA_V(wf(:,62),wf(:,-3),wf(:,248))
  call vert_VQ_A(wf(:,41),wf(:,0),wf(:,249))
  call prop_Q_A(wf(:,249),Q(:,45),ZERO,0_intkind1,wf(:,250))
  call vert_AV_Q(wf(:,-1),wf(:,41),wf(:,251))
  call prop_A_Q(wf(:,251),Q(:,46),ZERO,0_intkind1,wf(:,252))
  call vert_QA_V(wf(:,-2),wf(:,72),wf(:,253))

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
  den(9) = 1 / (Q(5,36))
  den(10) = 1 / (Q(5,24))
  den(13) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,17))
  den(19) = 1 / (Q(5,34))
  den(20) = 1 / (Q(5,12))
  den(23) = 1 / (Q(5,14))
  den(26) = 1 / (Q(5,33))
  den(27) = 1 / (Q(5,18))
  den(30) = 1 / (Q(5,13))
  den(35) = 1 / (Q(5,19))
  den(42) = 1 / (Q(5,35))
  den(50) = 1 / (Q(5,48) - MH2)
  den(52) = 1 / (Q(5,28))
  den(57) = 1 / (Q(5,44))
  den(62) = 1 / (Q(5,52))
  den(67) = 1 / (Q(5,56))
  den(76) = 1 / (Q(5,49))
  den(85) = 1 / (Q(5,50))
  den(95) = 1 / (Q(5,60))
  den(99) = 1 / (Q(5,43))
  den(102) = 1 / (Q(5,23))
  den(114) = 1 / (Q(5,27))
  den(117) = 1 / (Q(5,39))
  den(133) = 1 / (Q(5,46))
  den(137) = 1 / (Q(5,29))
  den(140) = 1 / (Q(5,51))
  den(149) = 1 / (Q(5,30))
  den(153) = 1 / (Q(5,45))

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
  den(21) = den(18)*den(19)
  den(22) = den(20)*den(21)
  den(24) = den(20)*den(23)
  den(25) = den(18)*den(24)
  den(28) = den(26)*den(27)
  den(29) = den(20)*den(28)
  den(31) = den(20)*den(30)
  den(32) = den(27)*den(31)
  den(33) = den(24)*den(26)
  den(34) = den(19)*den(31)
  den(36) = den(18)*den(35)
  den(37) = den(9)*den(36)
  den(38) = den(3)*den(36)
  den(39) = den(27)*den(35)
  den(40) = den(9)*den(39)
  den(41) = den(3)*den(39)
  den(43) = den(26)*den(42)
  den(44) = den(2)*den(43)
  den(45) = den(19)*den(42)
  den(46) = den(2)*den(45)
  den(47) = den(10)*den(43)
  den(48) = den(10)*den(45)
  den(49) = den(1)*den(20)
  den(51) = den(49)*den(50)
  den(53) = den(2)*den(52)
  den(54) = den(1)*den(53)
  den(55) = den(10)*den(52)
  den(56) = den(1)*den(55)
  den(58) = den(9)*den(57)
  den(59) = den(1)*den(58)
  den(60) = den(3)*den(57)
  den(61) = den(1)*den(60)
  den(63) = den(2)*den(62)
  den(64) = den(1)*den(63)
  den(65) = den(9)*den(62)
  den(66) = den(1)*den(65)
  den(68) = den(10)*den(67)
  den(69) = den(1)*den(68)
  den(70) = den(3)*den(67)
  den(71) = den(1)*den(70)
  den(72) = den(20)*den(36)
  den(73) = den(20)*den(39)
  den(74) = den(20)*den(43)
  den(75) = den(20)*den(45)
  den(77) = den(18)*den(76)
  den(78) = den(20)*den(77)
  den(79) = den(26)*den(76)
  den(80) = den(20)*den(79)
  den(81) = den(18)*den(58)
  den(82) = den(18)*den(60)
  den(83) = den(26)*den(53)
  den(84) = den(26)*den(55)
  den(86) = den(27)*den(85)
  den(87) = den(20)*den(86)
  den(88) = den(19)*den(85)
  den(89) = den(20)*den(88)
  den(90) = den(27)*den(58)
  den(91) = den(27)*den(60)
  den(92) = den(19)*den(53)
  den(93) = den(19)*den(55)
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
  den(120) = den(68)*den(105)
  den(121) = den(14)*den(68)
  den(122) = den(10)**2
  den(123) = den(14)*den(122)
  den(124) = den(65)*den(105)
  den(125) = den(9)**2
  den(126) = den(7)*den(125)
  den(127) = den(7)*den(65)
  den(128) = den(70)*den(105)
  den(129) = den(14)*den(70)
  den(130) = den(3)**2
  den(131) = den(14)*den(130)
  den(132) = den(19)*den(20)
  den(134) = den(132)*den(133)
  den(135) = den(18)*den(134)
  den(136) = den(18)*den(20)
  den(138) = den(136)*den(137)
  den(139) = den(19)*den(138)
  den(141) = den(21)*den(140)
  den(142) = den(20)*den(141)
  den(143) = den(18)**2
  den(144) = den(24)*den(143)
  den(145) = den(20)**2
  den(146) = den(77)*den(145)
  den(147) = den(24)*den(77)
  den(148) = den(20)*den(27)
  den(150) = den(148)*den(149)
  den(151) = den(26)*den(150)
  den(152) = den(20)*den(26)
  den(154) = den(152)*den(153)
  den(155) = den(27)*den(154)
  den(156) = den(28)*den(140)
  den(157) = den(20)*den(156)
  den(158) = den(86)*den(145)
  den(159) = den(31)*den(86)
  den(160) = den(27)**2
  den(161) = den(31)*den(160)
  den(162) = den(26)**2
  den(163) = den(24)*den(162)
  den(164) = den(79)*den(145)
  den(165) = den(24)*den(79)
  den(166) = den(88)*den(145)
  den(167) = den(31)*den(88)
  den(168) = den(19)**2
  den(169) = den(31)*den(168)
  den(170) = den(58)*den(143)
  den(171) = den(36)*den(58)
  den(172) = den(36)*den(125)
  den(173) = den(60)*den(143)
  den(174) = den(36)*den(60)
  den(175) = den(36)*den(130)
  den(176) = den(58)*den(160)
  den(177) = den(39)*den(58)
  den(178) = den(39)*den(125)
  den(179) = den(60)*den(160)
  den(180) = den(39)*den(60)
  den(181) = den(39)*den(130)
  den(182) = den(53)*den(162)
  den(183) = den(43)*den(53)
  den(184) = den(43)*den(107)
  den(185) = den(53)*den(168)
  den(186) = den(45)*den(53)
  den(187) = den(45)*den(107)
  den(188) = den(55)*den(162)
  den(189) = den(43)*den(55)
  den(190) = den(43)*den(122)
  den(191) = den(55)*den(168)
  den(192) = den(45)*den(55)
  den(193) = den(45)*den(122)
  den(194) = den(14)*den(102)
  den(195) = den(14)*den(117)
  den(196) = den(7)*den(114)
  den(197) = den(7)*den(99)
  den(198) = den(31)*den(137)
  den(199) = den(31)*den(153)
  den(200) = den(24)*den(149)
  den(201) = den(24)*den(133)
  den(202) = den(36)*den(102)
  den(203) = den(36)*den(114)
  den(204) = den(77)*den(140)
  den(205) = den(39)*den(102)
  den(206) = den(39)*den(114)
  den(207) = den(86)*den(140)
  den(208) = den(53)*den(137)
  den(209) = den(53)*den(149)
  den(210) = den(63)*den(95)
  den(211) = den(55)*den(137)
  den(212) = den(55)*den(149)
  den(213) = den(68)*den(95)
  den(214) = den(43)*den(117)
  den(215) = den(43)*den(99)
  den(216) = den(79)*den(140)
  den(217) = den(45)*den(117)
  den(218) = den(45)*den(99)
  den(219) = den(88)*den(140)
  den(220) = den(58)*den(153)
  den(221) = den(58)*den(133)
  den(222) = den(65)*den(95)
  den(223) = den(60)*den(153)
  den(224) = den(60)*den(133)
  den(225) = den(70)*den(95)
  den(226) = den(1)*den(20)*den(50)
  den(227) = den(1)*den(2)*den(3)
  den(228) = den(1)*den(9)*den(10)
  den(229) = den(18)*den(19)*den(20)
  den(230) = den(20)*den(26)*den(27)
  den(231) = den(9)*den(18)
  den(232) = den(3)*den(18)
  den(233) = den(9)*den(27)
  den(234) = den(3)*den(27)
  den(235) = den(2)*den(26)
  den(236) = den(2)*den(19)
  den(237) = den(10)*den(26)
  den(238) = den(10)*den(19)
  den(239) = den(2)*den(197)
  den(240) = den(1)*den(210)
  den(241) = den(10)*den(195)
  den(242) = den(1)*den(213)
  den(243) = den(9)*den(196)
  den(244) = den(1)*den(222)
  den(245) = den(3)*den(194)
  den(246) = den(1)*den(225)
  den(247) = den(20)*den(204)
  den(248) = den(18)*den(201)
  den(249) = den(27)*den(199)
  den(250) = den(20)*den(207)
  den(251) = den(20)*den(216)
  den(252) = den(26)*den(200)
  den(253) = den(19)*den(198)
  den(254) = den(20)*den(219)
  den(255) = den(9)*den(203)
  den(256) = den(18)*den(221)
  den(257) = den(3)*den(202)
  den(258) = den(18)*den(224)
  den(259) = den(9)*den(206)
  den(260) = den(27)*den(220)
  den(261) = den(3)*den(205)
  den(262) = den(27)*den(223)
  den(263) = den(2)*den(215)
  den(264) = den(26)*den(209)
  den(265) = den(2)*den(218)
  den(266) = den(19)*den(208)
  den(267) = den(10)*den(214)
  den(268) = den(26)*den(212)
  den(269) = den(10)*den(217)
  den(270) = den(19)*den(211)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(170)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,17),wf(:,19)) * den(17)
  A(7) = cont_VV(wf(:,22),wf(:,25)) * den(22)
  A(8) = cont_QA(wf(:,27),wf(:,28)) * den(25)
  A(9) = cont_VV(wf(:,22),wf(:,33)) * den(29)
  A(10) = cont_QA(wf(:,35),wf(:,36)) * den(32)
  A(11) = cont_QA(wf(:,28),wf(:,37)) * den(33)
  A(12) = cont_QA(wf(:,36),wf(:,38)) * den(34)
  A(13) = cont_VV(wf(:,39),wf(:,40)) * den(37)
  A(14) = cont_VV(wf(:,39),wf(:,41)) * den(38)
  A(15) = cont_VV(wf(:,40),wf(:,42)) * den(40)
  A(16) = cont_VV(wf(:,41),wf(:,42)) * den(41)
  A(17) = cont_VV(wf(:,43),wf(:,44)) * den(44)
  A(18) = cont_VV(wf(:,44),wf(:,45)) * den(46)
  A(19) = cont_VV(wf(:,43),wf(:,46)) * den(47)
  A(20) = cont_VV(wf(:,45),wf(:,46)) * den(48)

  A(21) = cont_VV(wf(:,22),wf(:,47)) * den(49)
  A(22) = cont_SS(wf(:,48),wf(:,49)) * den(51)
  A(23) = cont_QA(wf(:,5),wf(:,50)) * den(5)
  A(24) = cont_QA(wf(:,9),wf(:,51)) * den(8)
  A(25) = cont_VV(wf(:,44),wf(:,52)) * den(54)
  A(26) = cont_QA(wf(:,13),wf(:,53)) * den(12)
  A(27) = cont_QA(wf(:,17),wf(:,54)) * den(15)
  A(28) = cont_VV(wf(:,46),wf(:,52)) * den(56)
  A(29) = cont_QA(wf(:,9),wf(:,55)) * den(16)
  A(30) = cont_VV(wf(:,40),wf(:,56)) * den(59)
  A(31) = cont_QA(wf(:,17),wf(:,57)) * den(17)
  A(32) = cont_VV(wf(:,41),wf(:,56)) * den(61)
  A(33) = cont_QA(wf(:,58),wf(:,59)) * den(64)
  A(34) = cont_QA(wf(:,6),wf(:,61)) * den(5)
  A(35) = cont_QA(wf(:,58),wf(:,62)) * den(66)
  A(36) = cont_QA(wf(:,17),wf(:,63)) * den(17)
  A(37) = cont_QA(wf(:,14),wf(:,65)) * den(12)
  A(38) = cont_QA(wf(:,17),wf(:,66)) * den(15)
  A(39) = cont_QA(wf(:,67),wf(:,68)) * den(69)
  A(40) = cont_QA(wf(:,13),wf(:,71)) * den(12)
  A(41) = cont_QA(wf(:,67),wf(:,72)) * den(71)
  A(42) = cont_QA(wf(:,9),wf(:,73)) * den(16)
  A(43) = cont_QA(wf(:,5),wf(:,76)) * den(5)
  A(44) = cont_QA(wf(:,9),wf(:,77)) * den(8)
  A(45) = cont_VV(wf(:,22),wf(:,78)) * den(22)
  A(46) = cont_VV(wf(:,39),wf(:,79)) * den(72)
  A(47) = cont_QA(wf(:,28),wf(:,80)) * den(25)
  A(48) = cont_VV(wf(:,22),wf(:,81)) * den(29)
  A(49) = cont_VV(wf(:,42),wf(:,79)) * den(73)
  A(50) = cont_QA(wf(:,36),wf(:,82)) * den(32)
  A(51) = cont_VV(wf(:,43),wf(:,83)) * den(74)
  A(52) = cont_QA(wf(:,28),wf(:,84)) * den(33)
  A(53) = cont_VV(wf(:,45),wf(:,83)) * den(75)
  A(54) = cont_QA(wf(:,36),wf(:,85)) * den(34)
  A(55) = cont_VV(wf(:,39),wf(:,86)) * den(37)
  A(56) = cont_VV(wf(:,39),wf(:,87)) * den(38)
  A(57) = cont_VV(wf(:,42),wf(:,86)) * den(40)
  A(58) = cont_VV(wf(:,42),wf(:,87)) * den(41)
  A(59) = cont_VV(wf(:,43),wf(:,88)) * den(44)
  A(60) = cont_VV(wf(:,45),wf(:,88)) * den(46)
  A(61) = cont_VV(wf(:,43),wf(:,89)) * den(47)
  A(62) = cont_VV(wf(:,45),wf(:,89)) * den(48)
  A(63) = cont_VV(wf(:,39),wf(:,90)) * den(38)
  A(64) = cont_VV(wf(:,39),wf(:,91)) * den(37)
  A(65) = cont_VV(wf(:,42),wf(:,90)) * den(41)
  A(66) = cont_VV(wf(:,42),wf(:,91)) * den(40)
  A(67) = cont_VV(wf(:,43),wf(:,92)) * den(47)
  A(68) = cont_VV(wf(:,45),wf(:,92)) * den(48)
  A(69) = cont_VV(wf(:,43),wf(:,93)) * den(44)
  A(70) = cont_VV(wf(:,45),wf(:,93)) * den(46)
  A(71) = cont_VV(wf(:,25),wf(:,94)) * den(22)
  A(72) = cont_QA(wf(:,27),wf(:,96)) * den(25)
  A(73) = cont_VV(wf(:,33),wf(:,94)) * den(29)
  A(74) = cont_QA(wf(:,35),wf(:,98)) * den(32)
  A(75) = cont_QA(wf(:,37),wf(:,96)) * den(33)
  A(76) = cont_QA(wf(:,38),wf(:,98)) * den(34)
  A(77) = cont_QA(wf(:,99),wf(:,100)) * den(78)
  A(78) = cont_VV(wf(:,22),wf(:,103)) * den(22)
  A(79) = cont_QA(wf(:,99),wf(:,104)) * den(80)
  A(80) = cont_QA(wf(:,36),wf(:,105)) * den(34)
  A(81) = cont_VV(wf(:,22),wf(:,108)) * den(29)
  A(82) = cont_QA(wf(:,36),wf(:,109)) * den(32)
  A(83) = cont_VV(wf(:,40),wf(:,110)) * den(81)
  A(84) = cont_VV(wf(:,41),wf(:,110)) * den(82)
  A(85) = cont_VV(wf(:,44),wf(:,111)) * den(83)
  A(86) = cont_VV(wf(:,44),wf(:,112)) * den(46)
  A(87) = cont_VV(wf(:,46),wf(:,111)) * den(84)
  A(88) = cont_VV(wf(:,46),wf(:,112)) * den(48)
  A(89) = cont_VV(wf(:,40),wf(:,113)) * den(40)
  A(90) = cont_VV(wf(:,41),wf(:,113)) * den(41)
  A(91) = cont_QA(wf(:,114),wf(:,115)) * den(87)
  A(92) = cont_VV(wf(:,22),wf(:,118)) * den(29)
  A(93) = cont_QA(wf(:,114),wf(:,119)) * den(89)
  A(94) = cont_QA(wf(:,28),wf(:,120)) * den(33)
  A(95) = cont_VV(wf(:,22),wf(:,123)) * den(22)
  A(96) = cont_QA(wf(:,28),wf(:,124)) * den(25)
  A(97) = cont_VV(wf(:,40),wf(:,125)) * den(90)
  A(98) = cont_VV(wf(:,41),wf(:,125)) * den(91)
  A(99) = cont_VV(wf(:,44),wf(:,126)) * den(92)
  A(100) = cont_VV(wf(:,44),wf(:,127)) * den(44)
  A(101) = cont_VV(wf(:,46),wf(:,126)) * den(93)
  A(102) = cont_VV(wf(:,46),wf(:,127)) * den(47)
  A(103) = cont_VV(wf(:,40),wf(:,128)) * den(37)
  A(104) = cont_VV(wf(:,41),wf(:,128)) * den(38)
  A(105) = cont_QA(wf(:,5),wf(:,130)) * den(5)
  A(106) = cont_QA(wf(:,8),wf(:,132)) * den(8)
  A(107) = cont_QA(wf(:,13),wf(:,133)) * den(12)
  A(108) = cont_QA(wf(:,16),wf(:,135)) * den(15)
  A(109) = cont_QA(wf(:,18),wf(:,132)) * den(16)
  A(110) = cont_QA(wf(:,19),wf(:,135)) * den(17)
  A(111) = cont_VV(wf(:,136),wf(:,137)) * den(97)
  A(112) = cont_QA(wf(:,139),wf(:,140)) * den(101)
  A(113) = cont_QA(wf(:,141),wf(:,142)) * den(104)
  A(114) = cont_QA(wf(:,59),wf(:,143)) * den(106)
  A(115) = cont_QA(wf(:,9),wf(:,145)) * den(108)
  A(116) = cont_QA(wf(:,59),wf(:,146)) * den(109)
  A(117) = cont_VV(wf(:,137),wf(:,147)) * den(112)
  A(118) = cont_QA(wf(:,149),wf(:,150)) * den(116)
  A(119) = cont_QA(wf(:,151),wf(:,152)) * den(119)
  A(120) = cont_QA(wf(:,68),wf(:,153)) * den(120)
  A(121) = cont_QA(wf(:,68),wf(:,154)) * den(121)
  A(122) = cont_QA(wf(:,17),wf(:,156)) * den(123)
  A(123) = cont_QA(wf(:,62),wf(:,143)) * den(124)
  A(124) = cont_QA(wf(:,9),wf(:,158)) * den(126)
  A(125) = cont_QA(wf(:,62),wf(:,146)) * den(127)
  A(126) = cont_QA(wf(:,72),wf(:,153)) * den(128)
  A(127) = cont_QA(wf(:,72),wf(:,154)) * den(129)
  A(128) = cont_QA(wf(:,17),wf(:,160)) * den(131)
  A(129) = cont_QA(wf(:,162),wf(:,163)) * den(135)
  A(130) = cont_QA(wf(:,165),wf(:,166)) * den(139)
  A(131) = cont_VV(wf(:,25),wf(:,167)) * den(142)
  A(132) = cont_QA(wf(:,28),wf(:,169)) * den(144)
  A(133) = cont_QA(wf(:,100),wf(:,170)) * den(146)
  A(134) = cont_QA(wf(:,100),wf(:,171)) * den(147)
  A(135) = cont_QA(wf(:,173),wf(:,174)) * den(151)
  A(136) = cont_QA(wf(:,176),wf(:,177)) * den(155)
  A(137) = cont_VV(wf(:,33),wf(:,167)) * den(157)
  A(138) = cont_QA(wf(:,115),wf(:,178)) * den(158)
  A(139) = cont_QA(wf(:,115),wf(:,179)) * den(159)
  A(140) = cont_QA(wf(:,36),wf(:,181)) * den(161)
  A(141) = cont_QA(wf(:,28),wf(:,183)) * den(163)
  A(142) = cont_QA(wf(:,104),wf(:,170)) * den(164)
  A(143) = cont_QA(wf(:,104),wf(:,171)) * den(165)
  A(144) = cont_QA(wf(:,119),wf(:,178)) * den(166)
  A(145) = cont_QA(wf(:,119),wf(:,179)) * den(167)
  A(146) = cont_QA(wf(:,36),wf(:,185)) * den(169)
  A(147) = cont_VV(wf(:,40),wf(:,186)) * den(170)
  A(148) = cont_VV(wf(:,40),wf(:,187)) * den(171)
  A(149) = cont_VV(wf(:,39),wf(:,188)) * den(172)
  A(150) = cont_VV(wf(:,41),wf(:,186)) * den(173)
  A(151) = cont_VV(wf(:,41),wf(:,187)) * den(174)
  A(152) = cont_VV(wf(:,39),wf(:,189)) * den(175)
  A(153) = cont_VV(wf(:,40),wf(:,190)) * den(176)
  A(154) = cont_VV(wf(:,40),wf(:,191)) * den(177)
  A(155) = cont_VV(wf(:,42),wf(:,188)) * den(178)
  A(156) = cont_VV(wf(:,41),wf(:,190)) * den(179)
  A(157) = cont_VV(wf(:,41),wf(:,191)) * den(180)
  A(158) = cont_VV(wf(:,42),wf(:,189)) * den(181)
  A(159) = cont_VV(wf(:,44),wf(:,192)) * den(182)
  A(160) = cont_VV(wf(:,44),wf(:,193)) * den(183)
  A(161) = cont_VV(wf(:,43),wf(:,194)) * den(184)
  A(162) = cont_VV(wf(:,44),wf(:,195)) * den(185)
  A(163) = cont_VV(wf(:,44),wf(:,196)) * den(186)
  A(164) = cont_VV(wf(:,45),wf(:,194)) * den(187)
  A(165) = cont_VV(wf(:,46),wf(:,192)) * den(188)
  A(166) = cont_VV(wf(:,46),wf(:,193)) * den(189)
  A(167) = cont_VV(wf(:,43),wf(:,197)) * den(190)
  A(168) = cont_VV(wf(:,46),wf(:,195)) * den(191)
  A(169) = cont_VV(wf(:,46),wf(:,196)) * den(192)
  A(170) = cont_VV(wf(:,45),wf(:,197)) * den(193)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(170)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(7)+A(8)+A(9)+A(10)+A(11)+A(12)+A(13)+A(14)+A(15)+A(16)+A(17)+A(18)+A(19) &
       +A(20))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(7)-A(8)-A(9)-A(10)-A(11)-A(12)-A(13)-A(14)-A(15)-A(16)-A(17)-A(18)-A(19) &
       -A(20))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(111)-A(112)-A(113)-A(114)-A(115)-A(116)-A(117)-A(118)-A(119)-A(120)-A(121)-A(122)-A(123)-A(124)-A(125)-A(126) &
       -A(127)-A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(134)-A(135)-A(136)-A(137)-A(138)-A(139)-A(140)-A(141)-A(142)-A(143) &
       -A(144)-A(145)-A(146)-A(147)-A(148)-A(149)-A(150)-A(151)-A(152)-A(153)-A(154)-A(155)-A(156)-A(157)-A(158)-A(159)-A(160) &
       -A(161)-A(162)-A(163)-A(164)-A(165)-A(166)-A(167)-A(168)-A(169)-A(170))*f(2))/2._/**/REALKIND+((A(23)+A(26)+A(33)+A(35) &
       +A(39)+A(41)+A(45)+A(48)+A(55)+A(57)+A(59)+A(60)+A(63)+A(65)+A(67)+A(68)+A(71)+A(72)+A(73)+A(74)+A(75)+A(76)+A(77)+A(79) &
       +A(83)+A(84)+A(85)+A(87)+A(91)+A(93)+A(97)+A(98)+A(99)+A(101)+A(105)+A(106)+A(107)+A(108)+A(109) &
       +A(110))*f(3))/2._/**/REALKIND+((A(24)+A(27)+A(29)+A(31)+A(34)+A(36)+A(37)+A(38)+A(40)+A(42)+A(43)+A(44)+A(47)+A(50)+A(52) &
       +A(54)+A(56)+A(58)+A(61)+A(62)+A(64)+A(66)+A(69)+A(70)+A(78)+A(80)+A(81)+A(82)+A(86)+A(88)+A(89)+A(90)+A(92)+A(94)+A(95) &
       +A(96)+A(100)+A(102)+A(103)+A(104))*f(4))/2._/**/REALKIND+((-A(25)-A(28)-A(30)-A(32)-A(46)-A(49)-A(51) &
       -A(53))*f(5))/2._/**/REALKIND+(A(21)*f(6))/2._/**/REALKIND-(A(22)*f(7))/2._/**/REALKIND
  M2(2) = ((A(111)+A(112)+A(113)+A(114)+A(115)+A(116)+A(117)+A(118)+A(119)+A(120)+A(121)+A(122)+A(123)+A(124)+A(125)+A(126)+A(127) &
       +A(128)+A(129)+A(130)+A(131)+A(132)+A(133)+A(134)+A(135)+A(136)+A(137)+A(138)+A(139)+A(140)+A(141)+A(142)+A(143)+A(144) &
       +A(145)+A(146)+A(147)+A(148)+A(149)+A(150)+A(151)+A(152)+A(153)+A(154)+A(155)+A(156)+A(157)+A(158)+A(159)+A(160)+A(161) &
       +A(162)+A(163)+A(164)+A(165)+A(166)+A(167)+A(168)+A(169)+A(170))*f(2))/6._/**/REALKIND+((-A(23)-A(26)-A(33)-A(35)-A(39) &
       -A(41)-A(45)-A(48)-A(55)-A(57)-A(59)-A(60)-A(63)-A(65)-A(67)-A(68)-A(71)-A(72)-A(73)-A(74)-A(75)-A(76)-A(77)-A(79)-A(83) &
       -A(84)-A(85)-A(87)-A(91)-A(93)-A(97)-A(98)-A(99)-A(101)-A(105)-A(106)-A(107)-A(108)-A(109)-A(110))*f(3))/6._/**/REALKIND+(( &
       -A(24)-A(27)-A(29)-A(31)-A(34)-A(36)-A(37)-A(38)-A(40)-A(42)-A(43)-A(44)-A(47)-A(50)-A(52)-A(54)-A(56)-A(58)-A(61)-A(62) &
       -A(64)-A(66)-A(69)-A(70)-A(78)-A(80)-A(81)-A(82)-A(86)-A(88)-A(89)-A(90)-A(92)-A(94)-A(95)-A(96)-A(100)-A(102)-A(103) &
       -A(104))*f(4))/6._/**/REALKIND+((A(25)+A(28)+A(30)+A(32)+A(46)+A(49)+A(51)+A(53))*f(5))/6._/**/REALKIND &
       -(A(21)*f(6))/6._/**/REALKIND+(A(22)*f(7))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppzzjj_uuxddxzz_1_/**/REALKIND
