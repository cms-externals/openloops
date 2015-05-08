
module ol_colourmatrix_ppllaj_eexddxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  16]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   2]
  K1(11,:) = [  16]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [ -18]
  K1(20,:) = [ -18]
  K1(21,:) = [   0]
  K1(22,:) = [  36]
  K1(23,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllaj_eexddxag_1_/**/REALKIND



module ol_forced_parameters_ppllaj_eexddxag_1_/**/REALKIND
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
end module ol_forced_parameters_ppllaj_eexddxag_1_/**/REALKIND

module ol_loop_ppllaj_eexddxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(31), c(18)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:156)
  ! denominators
  complex(REALKIND), save :: den(165)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,64)
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
    f( 1) = (CI*eQED**3*gQCD)/9._/**/REALKIND
    f( 2) = (CI*eQED**3*gQCD)/3._/**/REALKIND
    f( 3) = CI*eQED**3*gQCD
    f( 4) = (CI*countertermnorm*eQED**3*gQCD**3)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*eQED**3*gQCD**3)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*eQED**3*gQCD**3
    f( 7) = CI*countertermnorm*ctAAGG*eQED**3*gQCD**3
    f( 8) = CI*countertermnorm*ctAZGG*eQED**3*gQCD**3
    f( 9) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3)/9._/**/REALKIND
    f(10) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3)/3._/**/REALKIND
    f(11) = CI*countertermnorm*ctGqq*eQED**3*gQCD**3
    f(12) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3)/9._/**/REALKIND
    f(13) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3)/3._/**/REALKIND
    f(14) = CI*countertermnorm*ctVqq*eQED**3*gQCD**3
    f(15) = (countertermnorm*ctZGG*eQED**3*gQCD**3)/3._/**/REALKIND
    f(16) = countertermnorm*ctZGG*eQED**3*gQCD**3
    f(17) = (CI*eQED**3*gQCD**3*integralnorm*SwB)/9._/**/REALKIND
    f(18) = (CI*eQED**3*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(19) = CI*eQED**3*gQCD**3*integralnorm*SwB
    f(20) = (eQED**3*gQCD**3*integralnorm*SwB)/9._/**/REALKIND
    f(21) = (eQED**3*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(22) = eQED**3*gQCD**3*integralnorm*SwB
    f(23) = (eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(24) = (2*eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(25) = (eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(26) = (4*eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(27) = (2*eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(28) = (8*eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(29) = eQED**3*gQCD**3*integralnorm*SwF
    f(30) = (4*eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(31) = 2*eQED**3*gQCD**3*integralnorm*SwF

  c = [ 9*CI*f(17), 9*CI*f(18), 9*CI*f(19), f(20), 8*f(20), f(21), 8*f(21), f(22), 8*f(22), 3*f(23), 3*f(24), 3*f(25), 3*f(26) &
    , 3*f(27), 3*f(28), 3*f(29), 3*f(30), 3*f(31) ]
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(110)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,3),MZ,1_intkind1,wf(:,8))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,11),ZERO,0_intkind1,wf(:,12))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,11),ZERO,0_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,36),ZERO,0_intkind1,wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,24),ZERO,0_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,1),wf(:,17),wf(:,19))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,17),wf(:,20))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,21))
  call vert_AV_Q(wf(:,18),wf(:,-5),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,7),ZERO,0_intkind1,wf(:,23))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,7),ZERO,0_intkind1,wf(:,25))
  call vert_VQ_A(wf(:,-4),wf(:,17),wf(:,26))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,27))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,17),ZERO,0_intkind1,wf(:,29))
  call vert_QA_V(wf(:,29),wf(:,-1),wf(:,30))
  call vert_QA_V(wf(:,17),wf(:,-3),wf(:,31))
  call vert_QA_Z(gZl,wf(:,29),wf(:,-1),wf(:,32))
  call vert_QA_Z(gZd,wf(:,17),wf(:,-3),wf(:,33))
  call prop_W_W(wf(:,32),Q(:,19),MZ,1_intkind1,wf(:,34))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,35))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,36))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,18),ZERO,0_intkind1,wf(:,38))
  call vert_QA_V(wf(:,0),wf(:,38),wf(:,39))
  call vert_QA_Z(gZl,wf(:,0),wf(:,38),wf(:,40))
  call prop_W_W(wf(:,40),Q(:,19),MZ,1_intkind1,wf(:,41))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,42))
  call counter_VVG_G(wf(:,1),wf(:,-4),wf(:,-5),wf(:,43))
  call counter_VVG_G(wf(:,8),wf(:,-4),wf(:,-5),wf(:,44))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,45))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,46))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,47))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,48))
  call counter_VG_G(wf(:,8),wf(:,-5),Q(:,32),wf(:,49),Q(:,35))
  call counter_VQ_A(wf(:,1),wf(:,17),wf(:,50))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,17),wf(:,51))
  call counter_AV_Q(wf(:,18),wf(:,-5),wf(:,52))
  call vert_QA_V(wf(:,-2),wf(:,18),wf(:,53))
  call counter_VQ_A(wf(:,-4),wf(:,17),wf(:,54))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,55))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,56))
  call prop_Q_A(wf(:,11),Q(:,52),ZERO,0_intkind1,wf(:,57))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,58))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,59))
  call prop_A_Q(wf(:,59),Q(:,40),ZERO,0_intkind1,wf(:,60))
  call prop_Q_A(wf(:,26),Q(:,52),ZERO,0_intkind1,wf(:,61))
  call vert_AV_Q(wf(:,60),wf(:,-4),wf(:,62))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,63))
  call prop_A_Q(wf(:,63),Q(:,24),ZERO,0_intkind1,wf(:,64))
  call vert_AV_Q(wf(:,64),wf(:,-5),wf(:,65))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,66))
  call prop_A_Q(wf(:,22),Q(:,56),ZERO,0_intkind1,wf(:,67))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,68))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,36),ZERO,0_intkind1,wf(:,70))
  call vert_VQ_A(wf(:,1),wf(:,70),wf(:,71))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,70),wf(:,72))
  call prop_A_Q(wf(:,27),Q(:,56),ZERO,0_intkind1,wf(:,73))
  call vert_VQ_A(wf(:,-4),wf(:,70),wf(:,74))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,75))
  call prop_Q_A(wf(:,75),Q(:,20),ZERO,0_intkind1,wf(:,76))
  call vert_VQ_A(wf(:,1),wf(:,76),wf(:,77))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,76),wf(:,78))
  call vert_VQ_A(wf(:,-5),wf(:,76),wf(:,79))
  call counter_GG_V(wf(:,42),Q(:,12),wf(:,-5),Q(:,32),wf(:,80))
  call counter_QA_V(wf(:,17),wf(:,-3),wf(:,81))
  call counter_QA_Z(gZd,wf(:,17),wf(:,-3),wf(:,82))
  call vert_QA_V(wf(:,-2),wf(:,60),wf(:,83))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,60),wf(:,84))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,85))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,86))
  call vert_QA_V(wf(:,70),wf(:,-3),wf(:,87))
  call vert_QA_Z(gZd,wf(:,70),wf(:,-3),wf(:,88))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,89))
  call counter_Q_A(ctqq,wf(:,4),Q(:,20),wf(:,90))
  call prop_A_Q(wf(:,89),Q(:,43),ZERO,0_intkind1,wf(:,91))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,8),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,43),ZERO,0_intkind1,wf(:,93))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,94))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,95))
  call prop_Q_A(wf(:,9),Q(:,23),ZERO,0_intkind1,wf(:,96))
  call prop_Q_A(wf(:,90),Q(:,20),ZERO,0_intkind1,wf(:,97))
  call vert_VQ_A(wf(:,-5),wf(:,97),wf(:,98))
  call counter_A_Q(ctqq,wf(:,12),Q(:,11),wf(:,99))
  call counter_A_Q(ctqq,wf(:,14),Q(:,11),wf(:,100))
  call vert_AV_Q(wf(:,18),wf(:,1),wf(:,101))
  call counter_Q_A(ctqq,wf(:,17),Q(:,36),wf(:,102))
  call prop_A_Q(wf(:,101),Q(:,27),ZERO,0_intkind1,wf(:,103))
  call vert_AZ_Q(gZd,wf(:,18),wf(:,8),wf(:,104))
  call prop_A_Q(wf(:,104),Q(:,27),ZERO,0_intkind1,wf(:,105))
  call counter_A_Q(ctqq,wf(:,18),Q(:,24),wf(:,106))
  call prop_Q_A(wf(:,19),Q(:,39),ZERO,0_intkind1,wf(:,107))
  call prop_Q_A(wf(:,20),Q(:,39),ZERO,0_intkind1,wf(:,108))
  call counter_Q_A(ctqq,wf(:,23),Q(:,7),wf(:,109))
  call counter_Q_A(ctqq,wf(:,25),Q(:,7),wf(:,110))
  call prop_A_Q(wf(:,106),Q(:,24),ZERO,0_intkind1,wf(:,111))
  call vert_AV_Q(wf(:,111),wf(:,-5),wf(:,112))
  call prop_Q_A(wf(:,102),Q(:,36),ZERO,0_intkind1,wf(:,113))
  call vert_VQ_A(wf(:,-4),wf(:,113),wf(:,114))
  call prop_A_Q(wf(:,94),Q(:,40),ZERO,0_intkind1,wf(:,115))
  call vert_AV_Q(wf(:,115),wf(:,-4),wf(:,116))
  call vert_QA_V(wf(:,113),wf(:,-3),wf(:,117))
  call vert_QA_Z(gZd,wf(:,113),wf(:,-3),wf(:,118))
  call vert_QA_V(wf(:,-2),wf(:,115),wf(:,119))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,115),wf(:,120))
  call vert_QA_V(wf(:,23),wf(:,-3),wf(:,121))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,122))
  call vert_VQ_A(wf(:,-4),wf(:,23),wf(:,123))
  call prop_Q_A(wf(:,123),Q(:,23),ZERO,0_intkind1,wf(:,124))
  call vert_VQ_A(wf(:,-4),wf(:,25),wf(:,125))
  call prop_Q_A(wf(:,125),Q(:,23),ZERO,0_intkind1,wf(:,126))
  call vert_VQ_A(wf(:,-5),wf(:,23),wf(:,127))
  call prop_Q_A(wf(:,127),Q(:,39),ZERO,0_intkind1,wf(:,128))
  call vert_VQ_A(wf(:,-5),wf(:,25),wf(:,129))
  call prop_Q_A(wf(:,129),Q(:,39),ZERO,0_intkind1,wf(:,130))
  call vert_QA_V(wf(:,-2),wf(:,12),wf(:,131))
  call vert_QA_V(wf(:,-2),wf(:,14),wf(:,132))
  call vert_AV_Q(wf(:,12),wf(:,-4),wf(:,133))
  call prop_A_Q(wf(:,133),Q(:,27),ZERO,0_intkind1,wf(:,134))
  call vert_AV_Q(wf(:,14),wf(:,-4),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,27),ZERO,0_intkind1,wf(:,136))
  call vert_AV_Q(wf(:,12),wf(:,-5),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,43),ZERO,0_intkind1,wf(:,138))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,139))
  call prop_A_Q(wf(:,139),Q(:,43),ZERO,0_intkind1,wf(:,140))
  call vert_VQ_A(wf(:,30),wf(:,-2),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,23),ZERO,0_intkind1,wf(:,142))
  call vert_ZQ_A(gZd,wf(:,34),wf(:,-2),wf(:,143))
  call prop_Q_A(wf(:,143),Q(:,23),ZERO,0_intkind1,wf(:,144))
  call vert_AV_Q(wf(:,-3),wf(:,30),wf(:,145))
  call prop_A_Q(wf(:,145),Q(:,27),ZERO,0_intkind1,wf(:,146))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,34),wf(:,147))
  call prop_A_Q(wf(:,147),Q(:,27),ZERO,0_intkind1,wf(:,148))
  call vert_VQ_A(wf(:,39),wf(:,-2),wf(:,149))
  call prop_Q_A(wf(:,149),Q(:,23),ZERO,0_intkind1,wf(:,150))
  call vert_ZQ_A(gZd,wf(:,41),wf(:,-2),wf(:,151))
  call prop_Q_A(wf(:,151),Q(:,23),ZERO,0_intkind1,wf(:,152))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,153))
  call prop_A_Q(wf(:,153),Q(:,27),ZERO,0_intkind1,wf(:,154))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,41),wf(:,155))
  call prop_A_Q(wf(:,155),Q(:,27),ZERO,0_intkind1,wf(:,156))

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
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,11))
  den(14) = 1 / (Q(5,36))
  den(15) = 1 / (Q(5,24))
  den(20) = 1 / (Q(5,7))
  den(29) = 1 / (Q(5,17))
  den(30) = 1 / (Q(5,19))
  den(33) = 1 / (Q(5,19) - MZ2)
  den(38) = 1 / (Q(5,18))
  den(45) = 1 / (Q(5,12))
  den(48) = 1 / (Q(5,28))
  den(53) = 1 / (Q(5,52))
  den(60) = 1 / (Q(5,56))
  den(70) = 1 / (Q(5,43))
  den(76) = 1 / (Q(5,23))
  den(87) = 1 / (Q(5,27))
  den(93) = 1 / (Q(5,39))
  den(121) = 1 / (Q(5,15))

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
  den(31) = den(29)*den(30)
  den(32) = den(14)*den(31)
  den(34) = den(29)*den(33)
  den(35) = den(14)*den(34)
  den(36) = den(3)*den(31)
  den(37) = den(3)*den(34)
  den(39) = den(30)*den(38)
  den(40) = den(14)*den(39)
  den(41) = den(33)*den(38)
  den(42) = den(14)*den(41)
  den(43) = den(3)*den(39)
  den(44) = den(3)*den(41)
  den(46) = den(1)*den(45)
  den(47) = den(6)*den(45)
  den(49) = den(2)*den(48)
  den(50) = den(6)*den(49)
  den(51) = den(15)*den(48)
  den(52) = den(6)*den(51)
  den(54) = den(2)*den(53)
  den(55) = den(1)*den(54)
  den(56) = den(6)*den(54)
  den(57) = den(14)*den(53)
  den(58) = den(1)*den(57)
  den(59) = den(6)*den(57)
  den(61) = den(15)*den(60)
  den(62) = den(1)*den(61)
  den(63) = den(6)*den(61)
  den(64) = den(3)*den(60)
  den(65) = den(1)*den(64)
  den(66) = den(6)*den(64)
  den(67) = den(34)*den(45)
  den(68) = den(41)*den(45)
  den(69) = den(1)*den(3)
  den(71) = den(69)*den(70)
  den(72) = den(2)*den(71)
  den(73) = den(3)*den(6)
  den(74) = den(70)*den(73)
  den(75) = den(2)*den(74)
  den(77) = den(4)*den(76)
  den(78) = den(3)*den(77)
  den(79) = den(7)*den(76)
  den(80) = den(3)*den(79)
  den(81) = den(2)**2
  den(82) = den(10)*den(81)
  den(83) = den(12)*den(81)
  den(84) = den(10)*den(54)
  den(85) = den(12)*den(54)
  den(86) = den(1)*den(15)
  den(88) = den(86)*den(87)
  den(89) = den(14)*den(88)
  den(90) = den(6)*den(15)
  den(91) = den(87)*den(90)
  den(92) = den(14)*den(91)
  den(94) = den(16)*den(93)
  den(95) = den(15)*den(94)
  den(96) = den(18)*den(93)
  den(97) = den(15)*den(96)
  den(98) = den(21)*den(61)
  den(99) = den(23)*den(61)
  den(100) = den(15)**2
  den(101) = den(21)*den(100)
  den(102) = den(23)*den(100)
  den(103) = den(14)**2
  den(104) = den(10)*den(103)
  den(105) = den(12)*den(103)
  den(106) = den(10)*den(57)
  den(107) = den(12)*den(57)
  den(108) = den(21)*den(64)
  den(109) = den(23)*den(64)
  den(110) = den(3)**2
  den(111) = den(21)*den(110)
  den(112) = den(23)*den(110)
  den(113) = den(31)*den(103)
  den(114) = den(34)*den(103)
  den(115) = den(31)*den(110)
  den(116) = den(34)*den(110)
  den(117) = den(39)*den(103)
  den(118) = den(41)*den(103)
  den(119) = den(39)*den(110)
  den(120) = den(41)*den(110)
  den(122) = den(21)*den(121)
  den(123) = den(23)*den(121)
  den(124) = den(21)*den(76)
  den(125) = den(23)*den(76)
  den(126) = den(21)*den(93)
  den(127) = den(23)*den(93)
  den(128) = den(10)*den(121)
  den(129) = den(12)*den(121)
  den(130) = den(10)*den(87)
  den(131) = den(12)*den(87)
  den(132) = den(10)*den(70)
  den(133) = den(12)*den(70)
  den(134) = den(31)*den(76)
  den(135) = den(34)*den(76)
  den(136) = den(31)*den(87)
  den(137) = den(34)*den(87)
  den(138) = den(39)*den(76)
  den(139) = den(41)*den(76)
  den(140) = den(39)*den(87)
  den(141) = den(41)*den(87)
  den(142) = den(1)*den(2)*den(3)
  den(143) = den(2)*den(3)*den(6)
  den(144) = den(1)*den(49)
  den(145) = den(1)*den(14)*den(15)
  den(146) = den(6)*den(14)*den(15)
  den(147) = den(1)*den(51)
  den(148) = den(31)*den(45)
  den(149) = den(39)*den(45)
  den(150) = den(2)*den(132)
  den(151) = den(2)*den(133)
  den(152) = den(15)*den(126)
  den(153) = den(15)*den(127)
  den(154) = den(14)*den(130)
  den(155) = den(14)*den(131)
  den(156) = den(3)*den(124)
  den(157) = den(3)*den(125)
  den(158) = den(14)*den(136)
  den(159) = den(14)*den(137)
  den(160) = den(3)*den(134)
  den(161) = den(3)*den(135)
  den(162) = den(14)*den(140)
  den(163) = den(14)*den(141)
  den(164) = den(3)*den(138)
  den(165) = den(3)*den(139)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(110)

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
  A(13) = cont_VV(wf(:,30),wf(:,31)) * den(32)
  A(14) = cont_VV(wf(:,33),wf(:,34)) * den(35)
  A(15) = cont_VV(wf(:,30),wf(:,35)) * den(36)
  A(16) = cont_VV(wf(:,34),wf(:,36)) * den(37)
  A(17) = cont_VV(wf(:,31),wf(:,39)) * den(40)
  A(18) = cont_VV(wf(:,33),wf(:,41)) * den(42)
  A(19) = cont_VV(wf(:,35),wf(:,39)) * den(43)
  A(20) = cont_VV(wf(:,36),wf(:,41)) * den(44)

  A(21) = cont_VV(wf(:,42),wf(:,43)) * den(46)
  A(22) = cont_VV(wf(:,42),wf(:,44)) * den(47)
  A(23) = cont_QA(wf(:,5),wf(:,45)) * den(5)
  A(24) = cont_QA(wf(:,5),wf(:,46)) * den(8)
  A(25) = cont_QA(wf(:,12),wf(:,47)) * den(11)
  A(26) = cont_QA(wf(:,14),wf(:,47)) * den(13)
  A(27) = cont_VV(wf(:,48),wf(:,49)) * den(50)
  A(28) = cont_QA(wf(:,18),wf(:,50)) * den(17)
  A(29) = cont_QA(wf(:,18),wf(:,51)) * den(19)
  A(30) = cont_QA(wf(:,23),wf(:,52)) * den(22)
  A(31) = cont_QA(wf(:,25),wf(:,52)) * den(24)
  A(32) = cont_VV(wf(:,49),wf(:,53)) * den(52)
  A(33) = cont_QA(wf(:,12),wf(:,54)) * den(25)
  A(34) = cont_QA(wf(:,14),wf(:,54)) * den(26)
  A(35) = cont_QA(wf(:,23),wf(:,55)) * den(27)
  A(36) = cont_QA(wf(:,25),wf(:,55)) * den(28)
  A(37) = cont_QA(wf(:,56),wf(:,57)) * den(55)
  A(38) = cont_QA(wf(:,57),wf(:,58)) * den(56)
  A(39) = cont_QA(wf(:,6),wf(:,60)) * den(5)
  A(40) = cont_QA(wf(:,9),wf(:,60)) * den(8)
  A(41) = cont_QA(wf(:,56),wf(:,61)) * den(58)
  A(42) = cont_QA(wf(:,58),wf(:,61)) * den(59)
  A(43) = cont_QA(wf(:,23),wf(:,62)) * den(27)
  A(44) = cont_QA(wf(:,25),wf(:,62)) * den(28)
  A(45) = cont_QA(wf(:,19),wf(:,64)) * den(17)
  A(46) = cont_QA(wf(:,20),wf(:,64)) * den(19)
  A(47) = cont_QA(wf(:,23),wf(:,65)) * den(22)
  A(48) = cont_QA(wf(:,25),wf(:,65)) * den(24)
  A(49) = cont_QA(wf(:,66),wf(:,67)) * den(62)
  A(50) = cont_QA(wf(:,67),wf(:,68)) * den(63)
  A(51) = cont_QA(wf(:,18),wf(:,71)) * den(17)
  A(52) = cont_QA(wf(:,18),wf(:,72)) * den(19)
  A(53) = cont_QA(wf(:,66),wf(:,73)) * den(65)
  A(54) = cont_QA(wf(:,68),wf(:,73)) * den(66)
  A(55) = cont_QA(wf(:,12),wf(:,74)) * den(25)
  A(56) = cont_QA(wf(:,14),wf(:,74)) * den(26)
  A(57) = cont_QA(wf(:,5),wf(:,77)) * den(5)
  A(58) = cont_QA(wf(:,5),wf(:,78)) * den(8)
  A(59) = cont_QA(wf(:,12),wf(:,79)) * den(11)
  A(60) = cont_QA(wf(:,14),wf(:,79)) * den(13)
  A(61) = cont_VV(wf(:,34),wf(:,80)) * den(67)
  A(62) = cont_VV(wf(:,41),wf(:,80)) * den(68)
  A(63) = cont_VV(wf(:,30),wf(:,81)) * den(32)
  A(64) = cont_VV(wf(:,34),wf(:,82)) * den(35)
  A(65) = cont_VV(wf(:,30),wf(:,83)) * den(36)
  A(66) = cont_VV(wf(:,34),wf(:,84)) * den(37)
  A(67) = cont_VV(wf(:,39),wf(:,81)) * den(40)
  A(68) = cont_VV(wf(:,41),wf(:,82)) * den(42)
  A(69) = cont_VV(wf(:,39),wf(:,83)) * den(43)
  A(70) = cont_VV(wf(:,41),wf(:,84)) * den(44)
  A(71) = cont_VV(wf(:,30),wf(:,85)) * den(36)
  A(72) = cont_VV(wf(:,34),wf(:,86)) * den(37)
  A(73) = cont_VV(wf(:,30),wf(:,87)) * den(32)
  A(74) = cont_VV(wf(:,34),wf(:,88)) * den(35)
  A(75) = cont_VV(wf(:,39),wf(:,85)) * den(43)
  A(76) = cont_VV(wf(:,41),wf(:,86)) * den(44)
  A(77) = cont_VV(wf(:,39),wf(:,87)) * den(40)
  A(78) = cont_VV(wf(:,41),wf(:,88)) * den(42)
  A(79) = cont_QA(wf(:,90),wf(:,91)) * den(72)
  A(80) = cont_QA(wf(:,90),wf(:,93)) * den(75)
  A(81) = cont_QA(wf(:,94),wf(:,95)) * den(78)
  A(82) = cont_QA(wf(:,94),wf(:,96)) * den(80)
  A(83) = cont_QA(wf(:,12),wf(:,98)) * den(82)
  A(84) = cont_QA(wf(:,14),wf(:,98)) * den(83)
  A(85) = cont_QA(wf(:,57),wf(:,99)) * den(84)
  A(86) = cont_QA(wf(:,57),wf(:,100)) * den(85)
  A(87) = cont_QA(wf(:,102),wf(:,103)) * den(89)
  A(88) = cont_QA(wf(:,102),wf(:,105)) * den(92)
  A(89) = cont_QA(wf(:,106),wf(:,107)) * den(95)
  A(90) = cont_QA(wf(:,106),wf(:,108)) * den(97)
  A(91) = cont_QA(wf(:,67),wf(:,109)) * den(98)
  A(92) = cont_QA(wf(:,67),wf(:,110)) * den(99)
  A(93) = cont_QA(wf(:,23),wf(:,112)) * den(101)
  A(94) = cont_QA(wf(:,25),wf(:,112)) * den(102)
  A(95) = cont_QA(wf(:,12),wf(:,114)) * den(104)
  A(96) = cont_QA(wf(:,14),wf(:,114)) * den(105)
  A(97) = cont_QA(wf(:,61),wf(:,99)) * den(106)
  A(98) = cont_QA(wf(:,61),wf(:,100)) * den(107)
  A(99) = cont_QA(wf(:,73),wf(:,109)) * den(108)
  A(100) = cont_QA(wf(:,73),wf(:,110)) * den(109)
  A(101) = cont_QA(wf(:,23),wf(:,116)) * den(111)
  A(102) = cont_QA(wf(:,25),wf(:,116)) * den(112)
  A(103) = cont_VV(wf(:,30),wf(:,117)) * den(113)
  A(104) = cont_VV(wf(:,34),wf(:,118)) * den(114)
  A(105) = cont_VV(wf(:,30),wf(:,119)) * den(115)
  A(106) = cont_VV(wf(:,34),wf(:,120)) * den(116)
  A(107) = cont_VV(wf(:,39),wf(:,117)) * den(117)
  A(108) = cont_VV(wf(:,41),wf(:,118)) * den(118)
  A(109) = cont_VV(wf(:,39),wf(:,119)) * den(119)
  A(110) = cont_VV(wf(:,41),wf(:,120)) * den(120)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(110)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(3)-A(5)-A(7)-A(9)-A(11))*f(1)+(-A(2)-A(4)-A(6)-A(8)-A(10)-A(12)-A(13)-A(15)-A(17)-A(19))*f(2)+(-A(14)-A(16) &
       -A(18)-A(20))*f(3)

  M2(1) = (A(79)+A(81)+A(83)+A(85)+A(87)+A(89)+A(91)+A(93)+A(95)+A(97)+A(99)+A(101))*f(4)+(A(80)+A(82)+A(84)+A(86)+A(88)+A(90) &
       +A(92)+A(94)+A(96)+A(98)+A(100)+A(102)+A(103)+A(105)+A(107)+A(109))*f(5)+(A(104)+A(106)+A(108)+A(110))*f(6)-A(21)*f(7) &
       -A(22)*f(8)+(-A(25)-A(30)-A(39)-A(43)-A(51)-A(55))*f(9)+(-A(26)-A(31)-A(40)-A(44)-A(52)-A(56)-A(65)-A(69)-A(73) &
       -A(77))*f(10)+(-A(66)-A(70)-A(74)-A(78))*f(11)+(-A(23)-A(28)-A(33)-A(35)-A(37)-A(41)-A(45)-A(47)-A(49)-A(53)-A(57) &
       -A(59))*f(12)+(-A(24)-A(29)-A(34)-A(36)-A(38)-A(42)-A(46)-A(48)-A(50)-A(54)-A(58)-A(60)-A(63)-A(67)-A(71)-A(75))*f(13)+( &
       -A(64)-A(68)-A(72)-A(76))*f(14)+(A(27)+A(32))*f(15)+(A(61)+A(62))*f(16)

end subroutine colourvectors

end module ol_loop_ppllaj_eexddxag_1_/**/REALKIND
