
module ol_colourmatrix_ppaajj_ddxssxaa_1_/**/REALKIND
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
end module ol_colourmatrix_ppaajj_ddxssxaa_1_/**/REALKIND



module ol_forced_parameters_ppaajj_ddxssxaa_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
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
end module ol_forced_parameters_ppaajj_ddxssxaa_1_/**/REALKIND

module ol_loop_ppaajj_ddxssxaa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:247)
  ! denominators
  complex(REALKIND), save :: den(267)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64), Mct(2,64), Mcol_loop(2,64)
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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**2*gQCD**2)/9._/**/REALKIND
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4)/9._/**/REALKIND
    f( 3) = CI*countertermnorm*ctAAGG*eQED**2*gQCD**4
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/9._/**/REALKIND
    f( 6) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/9._/**/REALKIND
    f( 7) = (eQED**2*gQCD**4*integralnorm*SwB)/18._/**/REALKIND
    f( 8) = (eQED**2*gQCD**4*integralnorm*SwB)/9._/**/REALKIND
    f( 9) = (eQED**2*gQCD**4*integralnorm*SwF)/9._/**/REALKIND
    f(10) = (2*eQED**2*gQCD**4*integralnorm*SwF)/9._/**/REALKIND
    f(11) = (4*eQED**2*gQCD**4*integralnorm*SwF)/9._/**/REALKIND
    f(12) = (8*eQED**2*gQCD**4*integralnorm*SwF)/9._/**/REALKIND

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10), 3*f(11), 9*f(11), 3*f(12), 9*f(12) ]
  c = (1._/**/REALKIND / 36) * c
end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2, POLSEL)
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
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(161)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),ZERO,0_intkind1,wf(:,9))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,10))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,36),ZERO,0_intkind1,wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,24),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,12),wf(:,14))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,15))
  call vert_AV_Q(wf(:,13),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_VQ_A(wf(:,-4),wf(:,12),wf(:,18))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,19))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,20))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,21))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,22))
  call prop_Q_A(wf(:,20),Q(:,17),ZERO,0_intkind1,wf(:,23))
  call prop_A_Q(wf(:,21),Q(:,34),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,23),wf(:,24),wf(:,25))
  call vert_AV_Q(wf(:,-1),wf(:,22),wf(:,26))
  call vert_VQ_A(wf(:,-5),wf(:,23),wf(:,27))
  call prop_A_Q(wf(:,26),Q(:,14),ZERO,0_intkind1,wf(:,28))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,29))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,33),ZERO,0_intkind1,wf(:,31))
  call prop_A_Q(wf(:,30),Q(:,18),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,31),wf(:,32),wf(:,33))
  call vert_VQ_A(wf(:,22),wf(:,0),wf(:,34))
  call vert_AV_Q(wf(:,32),wf(:,-5),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,13),ZERO,0_intkind1,wf(:,36))
  call vert_VQ_A(wf(:,-4),wf(:,31),wf(:,37))
  call vert_AV_Q(wf(:,24),wf(:,-4),wf(:,38))
  call vert_QA_V(wf(:,23),wf(:,-1),wf(:,39))
  call vert_QA_V(wf(:,12),wf(:,-3),wf(:,40))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,41))
  call vert_QA_V(wf(:,0),wf(:,32),wf(:,42))
  call vert_QA_V(wf(:,31),wf(:,-1),wf(:,43))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,44))
  call vert_QA_V(wf(:,0),wf(:,24),wf(:,45))
  call vert_QA_V(wf(:,-2),wf(:,13),wf(:,46))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,47))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,48))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,49))
  call counter_VQ_A(wf(:,1),wf(:,12),wf(:,50))
  call counter_AV_Q(wf(:,13),wf(:,-5),wf(:,51))
  call counter_VQ_A(wf(:,-4),wf(:,12),wf(:,52))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,53))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,54))
  call prop_Q_A(wf(:,8),Q(:,52),ZERO,0_intkind1,wf(:,55))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,56))
  call prop_A_Q(wf(:,56),Q(:,40),ZERO,0_intkind1,wf(:,57))
  call prop_Q_A(wf(:,18),Q(:,52),ZERO,0_intkind1,wf(:,58))
  call vert_AV_Q(wf(:,57),wf(:,-4),wf(:,59))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,60))
  call prop_A_Q(wf(:,60),Q(:,24),ZERO,0_intkind1,wf(:,61))
  call vert_AV_Q(wf(:,61),wf(:,-5),wf(:,62))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,63))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,64))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,65))
  call prop_Q_A(wf(:,65),Q(:,36),ZERO,0_intkind1,wf(:,66))
  call vert_VQ_A(wf(:,1),wf(:,66),wf(:,67))
  call prop_A_Q(wf(:,19),Q(:,56),ZERO,0_intkind1,wf(:,68))
  call vert_VQ_A(wf(:,-4),wf(:,66),wf(:,69))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,70))
  call prop_Q_A(wf(:,70),Q(:,20),ZERO,0_intkind1,wf(:,71))
  call vert_VQ_A(wf(:,1),wf(:,71),wf(:,72))
  call vert_VQ_A(wf(:,-5),wf(:,71),wf(:,73))
  call counter_QA_V(wf(:,23),wf(:,24),wf(:,74))
  call counter_VQ_A(wf(:,-5),wf(:,23),wf(:,75))
  call counter_QA_V(wf(:,31),wf(:,32),wf(:,76))
  call counter_AV_Q(wf(:,32),wf(:,-5),wf(:,77))
  call counter_VQ_A(wf(:,-4),wf(:,31),wf(:,78))
  call counter_AV_Q(wf(:,24),wf(:,-4),wf(:,79))
  call counter_QA_V(wf(:,12),wf(:,-3),wf(:,80))
  call vert_QA_V(wf(:,-2),wf(:,57),wf(:,81))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,82))
  call vert_QA_V(wf(:,-2),wf(:,61),wf(:,83))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,84))
  call vert_QA_V(wf(:,66),wf(:,-3),wf(:,85))
  call counter_QA_V(wf(:,-2),wf(:,13),wf(:,86))
  call vert_QA_V(wf(:,71),wf(:,-3),wf(:,87))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,88))
  call vert_AV_Q(wf(:,-1),wf(:,88),wf(:,89))
  call prop_A_Q(wf(:,89),Q(:,14),ZERO,0_intkind1,wf(:,90))
  call vert_VQ_A(wf(:,88),wf(:,0),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,13),ZERO,0_intkind1,wf(:,92))
  call counter_AV_Q(wf(:,-1),wf(:,22),wf(:,93))
  call prop_Q_A(wf(:,27),Q(:,49),ZERO,0_intkind1,wf(:,94))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,95))
  call prop_A_Q(wf(:,95),Q(:,34),ZERO,0_intkind1,wf(:,96))
  call vert_QA_V(wf(:,23),wf(:,96),wf(:,97))
  call prop_Q_A(wf(:,37),Q(:,49),ZERO,0_intkind1,wf(:,98))
  call vert_AV_Q(wf(:,96),wf(:,-4),wf(:,99))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,100))
  call prop_A_Q(wf(:,100),Q(:,18),ZERO,0_intkind1,wf(:,101))
  call vert_QA_V(wf(:,31),wf(:,101),wf(:,102))
  call vert_AV_Q(wf(:,101),wf(:,-5),wf(:,103))
  call counter_QA_V(wf(:,23),wf(:,-1),wf(:,104))
  call counter_QA_V(wf(:,31),wf(:,-1),wf(:,105))
  call vert_QA_V(wf(:,0),wf(:,96),wf(:,106))
  call vert_QA_V(wf(:,0),wf(:,101),wf(:,107))
  call counter_VQ_A(wf(:,22),wf(:,0),wf(:,108))
  call prop_A_Q(wf(:,35),Q(:,50),ZERO,0_intkind1,wf(:,109))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,33),ZERO,0_intkind1,wf(:,111))
  call vert_QA_V(wf(:,111),wf(:,32),wf(:,112))
  call prop_A_Q(wf(:,38),Q(:,50),ZERO,0_intkind1,wf(:,113))
  call vert_VQ_A(wf(:,-4),wf(:,111),wf(:,114))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,17),ZERO,0_intkind1,wf(:,116))
  call vert_QA_V(wf(:,116),wf(:,24),wf(:,117))
  call vert_VQ_A(wf(:,-5),wf(:,116),wf(:,118))
  call counter_QA_V(wf(:,0),wf(:,32),wf(:,119))
  call counter_QA_V(wf(:,0),wf(:,24),wf(:,120))
  call vert_QA_V(wf(:,111),wf(:,-1),wf(:,121))
  call vert_QA_V(wf(:,116),wf(:,-1),wf(:,122))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,123))
  call vert_VQ_A(wf(:,123),wf(:,4),wf(:,124))
  call vert_AV_Q(wf(:,-3),wf(:,123),wf(:,125))
  call prop_A_Q(wf(:,125),Q(:,11),ZERO,0_intkind1,wf(:,126))
  call vert_VQ_A(wf(:,123),wf(:,12),wf(:,127))
  call vert_VQ_A(wf(:,123),wf(:,-2),wf(:,128))
  call prop_Q_A(wf(:,128),Q(:,7),ZERO,0_intkind1,wf(:,129))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,130))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,131))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,132))
  call counter_Q_A(ctqq,wf(:,4),Q(:,20),wf(:,133))
  call prop_A_Q(wf(:,132),Q(:,43),ZERO,0_intkind1,wf(:,134))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,135))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,136))
  call vert_AV_Q(wf(:,-3),wf(:,131),wf(:,137))
  call prop_Q_A(wf(:,133),Q(:,20),ZERO,0_intkind1,wf(:,138))
  call vert_VQ_A(wf(:,-5),wf(:,138),wf(:,139))
  call counter_A_Q(ctqq,wf(:,9),Q(:,11),wf(:,140))
  call vert_QA_V(wf(:,12),wf(:,13),wf(:,141))
  call vert_AV_Q(wf(:,13),wf(:,1),wf(:,142))
  call counter_Q_A(ctqq,wf(:,12),Q(:,36),wf(:,143))
  call prop_A_Q(wf(:,142),Q(:,27),ZERO,0_intkind1,wf(:,144))
  call counter_A_Q(ctqq,wf(:,13),Q(:,24),wf(:,145))
  call prop_Q_A(wf(:,14),Q(:,39),ZERO,0_intkind1,wf(:,146))
  call vert_VQ_A(wf(:,131),wf(:,-2),wf(:,147))
  call counter_Q_A(ctqq,wf(:,17),Q(:,7),wf(:,148))
  call prop_A_Q(wf(:,145),Q(:,24),ZERO,0_intkind1,wf(:,149))
  call vert_AV_Q(wf(:,149),wf(:,-5),wf(:,150))
  call prop_Q_A(wf(:,143),Q(:,36),ZERO,0_intkind1,wf(:,151))
  call vert_VQ_A(wf(:,-4),wf(:,151),wf(:,152))
  call prop_A_Q(wf(:,135),Q(:,40),ZERO,0_intkind1,wf(:,153))
  call vert_AV_Q(wf(:,153),wf(:,-4),wf(:,154))
  call vert_AV_Q(wf(:,24),wf(:,22),wf(:,155))
  call counter_Q_A(ctqq,wf(:,23),Q(:,17),wf(:,156))
  call prop_A_Q(wf(:,155),Q(:,46),ZERO,0_intkind1,wf(:,157))
  call vert_VQ_A(wf(:,22),wf(:,23),wf(:,158))
  call counter_A_Q(ctqq,wf(:,24),Q(:,34),wf(:,159))
  call prop_Q_A(wf(:,158),Q(:,29),ZERO,0_intkind1,wf(:,160))
  call counter_V_V(ctGG,wf(:,22),Q(:,12),wf(:,161))
  call prop_Q_A(wf(:,156),Q(:,17),ZERO,0_intkind1,wf(:,162))
  call vert_VQ_A(wf(:,-5),wf(:,162),wf(:,163))
  call vert_AV_Q(wf(:,-1),wf(:,161),wf(:,164))
  call counter_A_Q(ctqq,wf(:,28),Q(:,14),wf(:,165))
  call vert_AV_Q(wf(:,32),wf(:,22),wf(:,166))
  call counter_Q_A(ctqq,wf(:,31),Q(:,33),wf(:,167))
  call prop_A_Q(wf(:,166),Q(:,30),ZERO,0_intkind1,wf(:,168))
  call vert_VQ_A(wf(:,22),wf(:,31),wf(:,169))
  call counter_A_Q(ctqq,wf(:,32),Q(:,18),wf(:,170))
  call prop_Q_A(wf(:,169),Q(:,45),ZERO,0_intkind1,wf(:,171))
  call vert_VQ_A(wf(:,161),wf(:,0),wf(:,172))
  call counter_Q_A(ctqq,wf(:,36),Q(:,13),wf(:,173))
  call prop_A_Q(wf(:,170),Q(:,18),ZERO,0_intkind1,wf(:,174))
  call vert_AV_Q(wf(:,174),wf(:,-5),wf(:,175))
  call prop_Q_A(wf(:,167),Q(:,33),ZERO,0_intkind1,wf(:,176))
  call vert_VQ_A(wf(:,-4),wf(:,176),wf(:,177))
  call prop_A_Q(wf(:,159),Q(:,34),ZERO,0_intkind1,wf(:,178))
  call vert_AV_Q(wf(:,178),wf(:,-4),wf(:,179))
  call vert_QA_V(wf(:,162),wf(:,-1),wf(:,180))
  call counter_V_V(ctGG,wf(:,39),Q(:,19),wf(:,181))
  call vert_QA_V(wf(:,151),wf(:,-3),wf(:,182))
  call vert_QA_V(wf(:,-2),wf(:,153),wf(:,183))
  call vert_QA_V(wf(:,0),wf(:,174),wf(:,184))
  call counter_V_V(ctGG,wf(:,42),Q(:,19),wf(:,185))
  call vert_QA_V(wf(:,176),wf(:,-1),wf(:,186))
  call counter_V_V(ctGG,wf(:,43),Q(:,35),wf(:,187))
  call vert_QA_V(wf(:,138),wf(:,-3),wf(:,188))
  call vert_QA_V(wf(:,0),wf(:,178),wf(:,189))
  call counter_V_V(ctGG,wf(:,45),Q(:,35),wf(:,190))
  call vert_QA_V(wf(:,-2),wf(:,149),wf(:,191))
  call vert_VQ_A(wf(:,-4),wf(:,17),wf(:,192))
  call prop_Q_A(wf(:,192),Q(:,23),ZERO,0_intkind1,wf(:,193))
  call vert_VQ_A(wf(:,-5),wf(:,17),wf(:,194))
  call prop_Q_A(wf(:,194),Q(:,39),ZERO,0_intkind1,wf(:,195))
  call vert_AV_Q(wf(:,9),wf(:,-4),wf(:,196))
  call prop_A_Q(wf(:,196),Q(:,27),ZERO,0_intkind1,wf(:,197))
  call vert_AV_Q(wf(:,9),wf(:,-5),wf(:,198))
  call prop_A_Q(wf(:,198),Q(:,43),ZERO,0_intkind1,wf(:,199))
  call vert_VQ_A(wf(:,-4),wf(:,36),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,29),ZERO,0_intkind1,wf(:,201))
  call vert_VQ_A(wf(:,-5),wf(:,36),wf(:,202))
  call prop_Q_A(wf(:,202),Q(:,45),ZERO,0_intkind1,wf(:,203))
  call vert_AV_Q(wf(:,28),wf(:,-4),wf(:,204))
  call prop_A_Q(wf(:,204),Q(:,30),ZERO,0_intkind1,wf(:,205))
  call vert_AV_Q(wf(:,28),wf(:,-5),wf(:,206))
  call prop_A_Q(wf(:,206),Q(:,46),ZERO,0_intkind1,wf(:,207))
  call vert_VQ_A(wf(:,39),wf(:,-2),wf(:,208))
  call prop_Q_A(wf(:,208),Q(:,23),ZERO,0_intkind1,wf(:,209))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,210))
  call prop_A_Q(wf(:,210),Q(:,27),ZERO,0_intkind1,wf(:,211))
  call vert_QA_V(wf(:,94),wf(:,-1),wf(:,212))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,213))
  call prop_Q_A(wf(:,213),Q(:,23),ZERO,0_intkind1,wf(:,214))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,215))
  call prop_A_Q(wf(:,215),Q(:,27),ZERO,0_intkind1,wf(:,216))
  call vert_QA_V(wf(:,0),wf(:,109),wf(:,217))
  call vert_VQ_A(wf(:,44),wf(:,0),wf(:,218))
  call prop_Q_A(wf(:,218),Q(:,29),ZERO,0_intkind1,wf(:,219))
  call vert_AV_Q(wf(:,-1),wf(:,44),wf(:,220))
  call prop_A_Q(wf(:,220),Q(:,30),ZERO,0_intkind1,wf(:,221))
  call vert_QA_V(wf(:,55),wf(:,-3),wf(:,222))
  call vert_VQ_A(wf(:,46),wf(:,0),wf(:,223))
  call prop_Q_A(wf(:,223),Q(:,29),ZERO,0_intkind1,wf(:,224))
  call vert_AV_Q(wf(:,-1),wf(:,46),wf(:,225))
  call prop_A_Q(wf(:,225),Q(:,30),ZERO,0_intkind1,wf(:,226))
  call vert_QA_V(wf(:,-2),wf(:,64),wf(:,227))
  call vert_VQ_A(wf(:,43),wf(:,-2),wf(:,228))
  call prop_Q_A(wf(:,228),Q(:,39),ZERO,0_intkind1,wf(:,229))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,43),ZERO,0_intkind1,wf(:,231))
  call vert_QA_V(wf(:,98),wf(:,-1),wf(:,232))
  call vert_VQ_A(wf(:,45),wf(:,-2),wf(:,233))
  call prop_Q_A(wf(:,233),Q(:,39),ZERO,0_intkind1,wf(:,234))
  call vert_AV_Q(wf(:,-3),wf(:,45),wf(:,235))
  call prop_A_Q(wf(:,235),Q(:,43),ZERO,0_intkind1,wf(:,236))
  call vert_QA_V(wf(:,0),wf(:,113),wf(:,237))
  call vert_VQ_A(wf(:,40),wf(:,0),wf(:,238))
  call prop_Q_A(wf(:,238),Q(:,45),ZERO,0_intkind1,wf(:,239))
  call vert_AV_Q(wf(:,-1),wf(:,40),wf(:,240))
  call prop_A_Q(wf(:,240),Q(:,46),ZERO,0_intkind1,wf(:,241))
  call vert_QA_V(wf(:,58),wf(:,-3),wf(:,242))
  call vert_VQ_A(wf(:,41),wf(:,0),wf(:,243))
  call prop_Q_A(wf(:,243),Q(:,45),ZERO,0_intkind1,wf(:,244))
  call vert_AV_Q(wf(:,-1),wf(:,41),wf(:,245))
  call prop_A_Q(wf(:,245),Q(:,46),ZERO,0_intkind1,wf(:,246))
  call vert_QA_V(wf(:,-2),wf(:,68),wf(:,247))

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
  den(50) = 1 / (Q(5,52))
  den(55) = 1 / (Q(5,56))
  den(60) = 1 / (Q(5,49))
  den(65) = 1 / (Q(5,44))
  den(70) = 1 / (Q(5,28))
  den(75) = 1 / (Q(5,50))
  den(85) = 1 / (Q(5,60))
  den(89) = 1 / (Q(5,43))
  den(92) = 1 / (Q(5,23))
  den(104) = 1 / (Q(5,27))
  den(107) = 1 / (Q(5,39))
  den(123) = 1 / (Q(5,46))
  den(127) = 1 / (Q(5,29))
  den(130) = 1 / (Q(5,51))
  den(139) = 1 / (Q(5,30))
  den(143) = 1 / (Q(5,45))

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
  den(51) = den(2)*den(50)
  den(52) = den(1)*den(51)
  den(53) = den(9)*den(50)
  den(54) = den(1)*den(53)
  den(56) = den(10)*den(55)
  den(57) = den(1)*den(56)
  den(58) = den(3)*den(55)
  den(59) = den(1)*den(58)
  den(61) = den(18)*den(60)
  den(62) = den(20)*den(61)
  den(63) = den(26)*den(60)
  den(64) = den(20)*den(63)
  den(66) = den(9)*den(65)
  den(67) = den(18)*den(66)
  den(68) = den(3)*den(65)
  den(69) = den(18)*den(68)
  den(71) = den(2)*den(70)
  den(72) = den(26)*den(71)
  den(73) = den(10)*den(70)
  den(74) = den(26)*den(73)
  den(76) = den(27)*den(75)
  den(77) = den(20)*den(76)
  den(78) = den(19)*den(75)
  den(79) = den(20)*den(78)
  den(80) = den(27)*den(66)
  den(81) = den(27)*den(68)
  den(82) = den(19)*den(71)
  den(83) = den(19)*den(73)
  den(84) = den(2)*den(3)
  den(86) = den(84)*den(85)
  den(87) = den(1)*den(86)
  den(88) = den(1)*den(3)
  den(90) = den(88)*den(89)
  den(91) = den(2)*den(90)
  den(93) = den(4)*den(92)
  den(94) = den(3)*den(93)
  den(95) = den(1)**2
  den(96) = den(51)*den(95)
  den(97) = den(2)**2
  den(98) = den(7)*den(97)
  den(99) = den(7)*den(51)
  den(100) = den(9)*den(10)
  den(101) = den(85)*den(100)
  den(102) = den(1)*den(101)
  den(103) = den(1)*den(10)
  den(105) = den(103)*den(104)
  den(106) = den(9)*den(105)
  den(108) = den(11)*den(107)
  den(109) = den(10)*den(108)
  den(110) = den(56)*den(95)
  den(111) = den(14)*den(56)
  den(112) = den(10)**2
  den(113) = den(14)*den(112)
  den(114) = den(53)*den(95)
  den(115) = den(9)**2
  den(116) = den(7)*den(115)
  den(117) = den(7)*den(53)
  den(118) = den(58)*den(95)
  den(119) = den(14)*den(58)
  den(120) = den(3)**2
  den(121) = den(14)*den(120)
  den(122) = den(19)*den(20)
  den(124) = den(122)*den(123)
  den(125) = den(18)*den(124)
  den(126) = den(18)*den(20)
  den(128) = den(126)*den(127)
  den(129) = den(19)*den(128)
  den(131) = den(21)*den(130)
  den(132) = den(20)*den(131)
  den(133) = den(18)**2
  den(134) = den(24)*den(133)
  den(135) = den(20)**2
  den(136) = den(61)*den(135)
  den(137) = den(24)*den(61)
  den(138) = den(20)*den(27)
  den(140) = den(138)*den(139)
  den(141) = den(26)*den(140)
  den(142) = den(20)*den(26)
  den(144) = den(142)*den(143)
  den(145) = den(27)*den(144)
  den(146) = den(28)*den(130)
  den(147) = den(20)*den(146)
  den(148) = den(76)*den(135)
  den(149) = den(31)*den(76)
  den(150) = den(27)**2
  den(151) = den(31)*den(150)
  den(152) = den(26)**2
  den(153) = den(24)*den(152)
  den(154) = den(63)*den(135)
  den(155) = den(24)*den(63)
  den(156) = den(78)*den(135)
  den(157) = den(31)*den(78)
  den(158) = den(19)**2
  den(159) = den(31)*den(158)
  den(160) = den(66)*den(133)
  den(161) = den(36)*den(66)
  den(162) = den(36)*den(115)
  den(163) = den(68)*den(133)
  den(164) = den(36)*den(68)
  den(165) = den(36)*den(120)
  den(166) = den(66)*den(150)
  den(167) = den(39)*den(66)
  den(168) = den(39)*den(115)
  den(169) = den(68)*den(150)
  den(170) = den(39)*den(68)
  den(171) = den(39)*den(120)
  den(172) = den(71)*den(152)
  den(173) = den(43)*den(71)
  den(174) = den(43)*den(97)
  den(175) = den(71)*den(158)
  den(176) = den(45)*den(71)
  den(177) = den(45)*den(97)
  den(178) = den(73)*den(152)
  den(179) = den(43)*den(73)
  den(180) = den(43)*den(112)
  den(181) = den(73)*den(158)
  den(182) = den(45)*den(73)
  den(183) = den(45)*den(112)
  den(184) = den(14)*den(92)
  den(185) = den(14)*den(107)
  den(186) = den(7)*den(104)
  den(187) = den(7)*den(89)
  den(188) = den(31)*den(127)
  den(189) = den(31)*den(143)
  den(190) = den(24)*den(139)
  den(191) = den(24)*den(123)
  den(192) = den(36)*den(92)
  den(193) = den(36)*den(104)
  den(194) = den(61)*den(130)
  den(195) = den(39)*den(92)
  den(196) = den(39)*den(104)
  den(197) = den(76)*den(130)
  den(198) = den(71)*den(127)
  den(199) = den(71)*den(139)
  den(200) = den(51)*den(85)
  den(201) = den(73)*den(127)
  den(202) = den(73)*den(139)
  den(203) = den(56)*den(85)
  den(204) = den(43)*den(107)
  den(205) = den(43)*den(89)
  den(206) = den(63)*den(130)
  den(207) = den(45)*den(107)
  den(208) = den(45)*den(89)
  den(209) = den(78)*den(130)
  den(210) = den(66)*den(143)
  den(211) = den(66)*den(123)
  den(212) = den(53)*den(85)
  den(213) = den(68)*den(143)
  den(214) = den(68)*den(123)
  den(215) = den(58)*den(85)
  den(216) = den(1)*den(2)*den(3)
  den(217) = den(1)*den(71)
  den(218) = den(1)*den(9)*den(10)
  den(219) = den(1)*den(73)
  den(220) = den(1)*den(66)
  den(221) = den(1)*den(68)
  den(222) = den(18)*den(19)*den(20)
  den(223) = den(20)*den(36)
  den(224) = den(20)*den(26)*den(27)
  den(225) = den(20)*den(39)
  den(226) = den(20)*den(43)
  den(227) = den(20)*den(45)
  den(228) = den(9)*den(18)
  den(229) = den(3)*den(18)
  den(230) = den(9)*den(27)
  den(231) = den(3)*den(27)
  den(232) = den(2)*den(26)
  den(233) = den(2)*den(19)
  den(234) = den(10)*den(26)
  den(235) = den(10)*den(19)
  den(236) = den(2)*den(187)
  den(237) = den(1)*den(200)
  den(238) = den(10)*den(185)
  den(239) = den(1)*den(203)
  den(240) = den(9)*den(186)
  den(241) = den(1)*den(212)
  den(242) = den(3)*den(184)
  den(243) = den(1)*den(215)
  den(244) = den(20)*den(194)
  den(245) = den(18)*den(191)
  den(246) = den(27)*den(189)
  den(247) = den(20)*den(197)
  den(248) = den(20)*den(206)
  den(249) = den(26)*den(190)
  den(250) = den(19)*den(188)
  den(251) = den(20)*den(209)
  den(252) = den(9)*den(193)
  den(253) = den(18)*den(211)
  den(254) = den(3)*den(192)
  den(255) = den(18)*den(214)
  den(256) = den(9)*den(196)
  den(257) = den(27)*den(210)
  den(258) = den(3)*den(195)
  den(259) = den(27)*den(213)
  den(260) = den(2)*den(205)
  den(261) = den(26)*den(199)
  den(262) = den(2)*den(208)
  den(263) = den(19)*den(198)
  den(264) = den(10)*den(204)
  den(265) = den(26)*den(202)
  den(266) = den(10)*den(207)
  den(267) = den(19)*den(201)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(161)

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
  A(22) = cont_QA(wf(:,5),wf(:,48)) * den(5)
  A(23) = cont_QA(wf(:,9),wf(:,49)) * den(8)
  A(24) = cont_QA(wf(:,13),wf(:,50)) * den(12)
  A(25) = cont_QA(wf(:,17),wf(:,51)) * den(15)
  A(26) = cont_QA(wf(:,9),wf(:,52)) * den(16)
  A(27) = cont_QA(wf(:,17),wf(:,53)) * den(17)
  A(28) = cont_QA(wf(:,54),wf(:,55)) * den(52)
  A(29) = cont_QA(wf(:,6),wf(:,57)) * den(5)
  A(30) = cont_QA(wf(:,54),wf(:,58)) * den(54)
  A(31) = cont_QA(wf(:,17),wf(:,59)) * den(17)
  A(32) = cont_QA(wf(:,14),wf(:,61)) * den(12)
  A(33) = cont_QA(wf(:,17),wf(:,62)) * den(15)
  A(34) = cont_QA(wf(:,63),wf(:,64)) * den(57)
  A(35) = cont_QA(wf(:,13),wf(:,67)) * den(12)
  A(36) = cont_QA(wf(:,63),wf(:,68)) * den(59)
  A(37) = cont_QA(wf(:,9),wf(:,69)) * den(16)
  A(38) = cont_QA(wf(:,5),wf(:,72)) * den(5)
  A(39) = cont_QA(wf(:,9),wf(:,73)) * den(8)
  A(40) = cont_VV(wf(:,22),wf(:,74)) * den(22)
  A(41) = cont_QA(wf(:,28),wf(:,75)) * den(25)
  A(42) = cont_VV(wf(:,22),wf(:,76)) * den(29)
  A(43) = cont_QA(wf(:,36),wf(:,77)) * den(32)
  A(44) = cont_QA(wf(:,28),wf(:,78)) * den(33)
  A(45) = cont_QA(wf(:,36),wf(:,79)) * den(34)
  A(46) = cont_VV(wf(:,39),wf(:,80)) * den(37)
  A(47) = cont_VV(wf(:,39),wf(:,81)) * den(38)
  A(48) = cont_VV(wf(:,42),wf(:,80)) * den(40)
  A(49) = cont_VV(wf(:,42),wf(:,81)) * den(41)
  A(50) = cont_VV(wf(:,43),wf(:,82)) * den(44)
  A(51) = cont_VV(wf(:,45),wf(:,82)) * den(46)
  A(52) = cont_VV(wf(:,43),wf(:,83)) * den(47)
  A(53) = cont_VV(wf(:,45),wf(:,83)) * den(48)
  A(54) = cont_VV(wf(:,39),wf(:,84)) * den(38)
  A(55) = cont_VV(wf(:,39),wf(:,85)) * den(37)
  A(56) = cont_VV(wf(:,42),wf(:,84)) * den(41)
  A(57) = cont_VV(wf(:,42),wf(:,85)) * den(40)
  A(58) = cont_VV(wf(:,43),wf(:,86)) * den(47)
  A(59) = cont_VV(wf(:,45),wf(:,86)) * den(48)
  A(60) = cont_VV(wf(:,43),wf(:,87)) * den(44)
  A(61) = cont_VV(wf(:,45),wf(:,87)) * den(46)
  A(62) = cont_VV(wf(:,25),wf(:,88)) * den(22)
  A(63) = cont_QA(wf(:,27),wf(:,90)) * den(25)
  A(64) = cont_VV(wf(:,33),wf(:,88)) * den(29)
  A(65) = cont_QA(wf(:,35),wf(:,92)) * den(32)
  A(66) = cont_QA(wf(:,37),wf(:,90)) * den(33)
  A(67) = cont_QA(wf(:,38),wf(:,92)) * den(34)
  A(68) = cont_QA(wf(:,93),wf(:,94)) * den(62)
  A(69) = cont_VV(wf(:,22),wf(:,97)) * den(22)
  A(70) = cont_QA(wf(:,93),wf(:,98)) * den(64)
  A(71) = cont_QA(wf(:,36),wf(:,99)) * den(34)
  A(72) = cont_VV(wf(:,22),wf(:,102)) * den(29)
  A(73) = cont_QA(wf(:,36),wf(:,103)) * den(32)
  A(74) = cont_VV(wf(:,40),wf(:,104)) * den(67)
  A(75) = cont_VV(wf(:,41),wf(:,104)) * den(69)
  A(76) = cont_VV(wf(:,44),wf(:,105)) * den(72)
  A(77) = cont_VV(wf(:,44),wf(:,106)) * den(46)
  A(78) = cont_VV(wf(:,46),wf(:,105)) * den(74)
  A(79) = cont_VV(wf(:,46),wf(:,106)) * den(48)
  A(80) = cont_VV(wf(:,40),wf(:,107)) * den(40)
  A(81) = cont_VV(wf(:,41),wf(:,107)) * den(41)
  A(82) = cont_QA(wf(:,108),wf(:,109)) * den(77)
  A(83) = cont_VV(wf(:,22),wf(:,112)) * den(29)
  A(84) = cont_QA(wf(:,108),wf(:,113)) * den(79)
  A(85) = cont_QA(wf(:,28),wf(:,114)) * den(33)
  A(86) = cont_VV(wf(:,22),wf(:,117)) * den(22)
  A(87) = cont_QA(wf(:,28),wf(:,118)) * den(25)
  A(88) = cont_VV(wf(:,40),wf(:,119)) * den(80)
  A(89) = cont_VV(wf(:,41),wf(:,119)) * den(81)
  A(90) = cont_VV(wf(:,44),wf(:,120)) * den(82)
  A(91) = cont_VV(wf(:,44),wf(:,121)) * den(44)
  A(92) = cont_VV(wf(:,46),wf(:,120)) * den(83)
  A(93) = cont_VV(wf(:,46),wf(:,121)) * den(47)
  A(94) = cont_VV(wf(:,40),wf(:,122)) * den(37)
  A(95) = cont_VV(wf(:,41),wf(:,122)) * den(38)
  A(96) = cont_QA(wf(:,5),wf(:,124)) * den(5)
  A(97) = cont_QA(wf(:,8),wf(:,126)) * den(8)
  A(98) = cont_QA(wf(:,13),wf(:,127)) * den(12)
  A(99) = cont_QA(wf(:,16),wf(:,129)) * den(15)
  A(100) = cont_QA(wf(:,18),wf(:,126)) * den(16)
  A(101) = cont_QA(wf(:,19),wf(:,129)) * den(17)
  A(102) = cont_VV(wf(:,130),wf(:,131)) * den(87)
  A(103) = cont_QA(wf(:,133),wf(:,134)) * den(91)
  A(104) = cont_QA(wf(:,135),wf(:,136)) * den(94)
  A(105) = cont_QA(wf(:,55),wf(:,137)) * den(96)
  A(106) = cont_QA(wf(:,9),wf(:,139)) * den(98)
  A(107) = cont_QA(wf(:,55),wf(:,140)) * den(99)
  A(108) = cont_VV(wf(:,131),wf(:,141)) * den(102)
  A(109) = cont_QA(wf(:,143),wf(:,144)) * den(106)
  A(110) = cont_QA(wf(:,145),wf(:,146)) * den(109)
  A(111) = cont_QA(wf(:,64),wf(:,147)) * den(110)
  A(112) = cont_QA(wf(:,64),wf(:,148)) * den(111)
  A(113) = cont_QA(wf(:,17),wf(:,150)) * den(113)
  A(114) = cont_QA(wf(:,58),wf(:,137)) * den(114)
  A(115) = cont_QA(wf(:,9),wf(:,152)) * den(116)
  A(116) = cont_QA(wf(:,58),wf(:,140)) * den(117)
  A(117) = cont_QA(wf(:,68),wf(:,147)) * den(118)
  A(118) = cont_QA(wf(:,68),wf(:,148)) * den(119)
  A(119) = cont_QA(wf(:,17),wf(:,154)) * den(121)
  A(120) = cont_QA(wf(:,156),wf(:,157)) * den(125)
  A(121) = cont_QA(wf(:,159),wf(:,160)) * den(129)
  A(122) = cont_VV(wf(:,25),wf(:,161)) * den(132)
  A(123) = cont_QA(wf(:,28),wf(:,163)) * den(134)
  A(124) = cont_QA(wf(:,94),wf(:,164)) * den(136)
  A(125) = cont_QA(wf(:,94),wf(:,165)) * den(137)
  A(126) = cont_QA(wf(:,167),wf(:,168)) * den(141)
  A(127) = cont_QA(wf(:,170),wf(:,171)) * den(145)
  A(128) = cont_VV(wf(:,33),wf(:,161)) * den(147)
  A(129) = cont_QA(wf(:,109),wf(:,172)) * den(148)
  A(130) = cont_QA(wf(:,109),wf(:,173)) * den(149)
  A(131) = cont_QA(wf(:,36),wf(:,175)) * den(151)
  A(132) = cont_QA(wf(:,28),wf(:,177)) * den(153)
  A(133) = cont_QA(wf(:,98),wf(:,164)) * den(154)
  A(134) = cont_QA(wf(:,98),wf(:,165)) * den(155)
  A(135) = cont_QA(wf(:,113),wf(:,172)) * den(156)
  A(136) = cont_QA(wf(:,113),wf(:,173)) * den(157)
  A(137) = cont_QA(wf(:,36),wf(:,179)) * den(159)
  A(138) = cont_VV(wf(:,40),wf(:,180)) * den(160)
  A(139) = cont_VV(wf(:,40),wf(:,181)) * den(161)
  A(140) = cont_VV(wf(:,39),wf(:,182)) * den(162)
  A(141) = cont_VV(wf(:,41),wf(:,180)) * den(163)
  A(142) = cont_VV(wf(:,41),wf(:,181)) * den(164)
  A(143) = cont_VV(wf(:,39),wf(:,183)) * den(165)
  A(144) = cont_VV(wf(:,40),wf(:,184)) * den(166)
  A(145) = cont_VV(wf(:,40),wf(:,185)) * den(167)
  A(146) = cont_VV(wf(:,42),wf(:,182)) * den(168)
  A(147) = cont_VV(wf(:,41),wf(:,184)) * den(169)
  A(148) = cont_VV(wf(:,41),wf(:,185)) * den(170)
  A(149) = cont_VV(wf(:,42),wf(:,183)) * den(171)
  A(150) = cont_VV(wf(:,44),wf(:,186)) * den(172)
  A(151) = cont_VV(wf(:,44),wf(:,187)) * den(173)
  A(152) = cont_VV(wf(:,43),wf(:,188)) * den(174)
  A(153) = cont_VV(wf(:,44),wf(:,189)) * den(175)
  A(154) = cont_VV(wf(:,44),wf(:,190)) * den(176)
  A(155) = cont_VV(wf(:,45),wf(:,188)) * den(177)
  A(156) = cont_VV(wf(:,46),wf(:,186)) * den(178)
  A(157) = cont_VV(wf(:,46),wf(:,187)) * den(179)
  A(158) = cont_VV(wf(:,43),wf(:,191)) * den(180)
  A(159) = cont_VV(wf(:,46),wf(:,189)) * den(181)
  A(160) = cont_VV(wf(:,46),wf(:,190)) * den(182)
  A(161) = cont_VV(wf(:,45),wf(:,191)) * den(183)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(161)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(7)+A(8)+A(9)+A(10)+A(11)+A(12)+A(13)+A(14)+A(15)+A(16)+A(17)+A(18)+A(19) &
       +A(20))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(7)-A(8)-A(9)-A(10)-A(11)-A(12)-A(13)-A(14)-A(15)-A(16)-A(17)-A(18)-A(19) &
       -A(20))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(102)-A(103)-A(104)-A(105)-A(106)-A(107)-A(108)-A(109)-A(110)-A(111)-A(112)-A(113)-A(114)-A(115)-A(116)-A(117) &
       -A(118)-A(119)-A(120)-A(121)-A(122)-A(123)-A(124)-A(125)-A(126)-A(127)-A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(134) &
       -A(135)-A(136)-A(137)-A(138)-A(139)-A(140)-A(141)-A(142)-A(143)-A(144)-A(145)-A(146)-A(147)-A(148)-A(149)-A(150)-A(151) &
       -A(152)-A(153)-A(154)-A(155)-A(156)-A(157)-A(158)-A(159)-A(160)-A(161))*f(2))/2._/**/REALKIND+(A(21)*f(3))/2._/**/REALKIND &
       +((A(22)+A(24)+A(28)+A(30)+A(34)+A(36)+A(40)+A(42)+A(46)+A(48)+A(50)+A(51)+A(54)+A(56)+A(58)+A(59)+A(62)+A(63)+A(64)+A(65) &
       +A(66)+A(67)+A(68)+A(70)+A(74)+A(75)+A(76)+A(78)+A(82)+A(84)+A(88)+A(89)+A(90)+A(92)+A(96)+A(97)+A(98)+A(99)+A(100) &
       +A(101))*f(4))/2._/**/REALKIND+((A(23)+A(25)+A(26)+A(27)+A(29)+A(31)+A(32)+A(33)+A(35)+A(37)+A(38)+A(39)+A(41)+A(43)+A(44) &
       +A(45)+A(47)+A(49)+A(52)+A(53)+A(55)+A(57)+A(60)+A(61)+A(69)+A(71)+A(72)+A(73)+A(77)+A(79)+A(80)+A(81)+A(83)+A(85)+A(86) &
       +A(87)+A(91)+A(93)+A(94)+A(95))*f(5))/2._/**/REALKIND
  M2(2) = ((A(102)+A(103)+A(104)+A(105)+A(106)+A(107)+A(108)+A(109)+A(110)+A(111)+A(112)+A(113)+A(114)+A(115)+A(116)+A(117)+A(118) &
       +A(119)+A(120)+A(121)+A(122)+A(123)+A(124)+A(125)+A(126)+A(127)+A(128)+A(129)+A(130)+A(131)+A(132)+A(133)+A(134)+A(135) &
       +A(136)+A(137)+A(138)+A(139)+A(140)+A(141)+A(142)+A(143)+A(144)+A(145)+A(146)+A(147)+A(148)+A(149)+A(150)+A(151)+A(152) &
       +A(153)+A(154)+A(155)+A(156)+A(157)+A(158)+A(159)+A(160)+A(161))*f(2))/6._/**/REALKIND-(A(21)*f(3))/6._/**/REALKIND+(( &
       -A(22)-A(24)-A(28)-A(30)-A(34)-A(36)-A(40)-A(42)-A(46)-A(48)-A(50)-A(51)-A(54)-A(56)-A(58)-A(59)-A(62)-A(63)-A(64)-A(65) &
       -A(66)-A(67)-A(68)-A(70)-A(74)-A(75)-A(76)-A(78)-A(82)-A(84)-A(88)-A(89)-A(90)-A(92)-A(96)-A(97)-A(98)-A(99)-A(100) &
       -A(101))*f(4))/6._/**/REALKIND+((-A(23)-A(25)-A(26)-A(27)-A(29)-A(31)-A(32)-A(33)-A(35)-A(37)-A(38)-A(39)-A(41)-A(43)-A(44) &
       -A(45)-A(47)-A(49)-A(52)-A(53)-A(55)-A(57)-A(60)-A(61)-A(69)-A(71)-A(72)-A(73)-A(77)-A(79)-A(80)-A(81)-A(83)-A(85)-A(86) &
       -A(87)-A(91)-A(93)-A(94)-A(95))*f(5))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppaajj_ddxssxaa_1_/**/REALKIND
