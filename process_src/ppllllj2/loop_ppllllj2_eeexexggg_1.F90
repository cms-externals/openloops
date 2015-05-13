
module ol_colourmatrix_ppllllj2_eeexexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,2), KL(0,2), KL2(2,2), KL2ct(2,2), KL2ct2(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [  7, -2]
  KL2(2,:) = [ -2,  7]
  KL2 = (1._/**/REALKIND / 3) * KL2

  KL2ct(1,:) = [  7, -2]
  KL2ct(2,:) = [ -2,  7]
  KL2ct = (1._/**/REALKIND / 3) * KL2ct

  KL2ct2(1,:) = [  7, -2]
  KL2ct2(2,:) = [ -2,  7]
  KL2ct2 = (1._/**/REALKIND / 3) * KL2ct2

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj2_eeexexggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_eeexexggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_eeexexggg_1_/**/REALKIND

module ol_loop_ppllllj2_eeexexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(28), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:160)
  ! denominators
  complex(REALKIND), save :: den(317)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128)
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
    f( 1) = CI*countertermnorm*eQED**4*gQCD**3
    f( 2) = countertermnorm*ctAAGG*eQED**4*gQCD**3
    f( 3) = countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 4) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 5) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 6) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f( 7) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 9) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(11) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = (8*CI*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(13) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f(14) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(15) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f(16) = (eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(17) = (2*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(18) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(19) = (4*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(20) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(21) = (8*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(22) = eQED**4*gQCD**3*integralnorm*SwF
    f(23) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(24) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(25) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(26) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(27) = (CI*eQED**4*gQCD**3*integralnorm*MT*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(28) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(142)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-6),wf(:,4))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-3),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,10),MZ,1_intkind1,wf(:,6))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-6),wf(:,7))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-2),wf(:,8))
  call prop_W_W(wf(:,8),Q(:,5),MZ,1_intkind1,wf(:,9))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-6),wf(:,10))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-6),wf(:,11))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,12))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-5),wf(:,13))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-5),wf(:,14))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-5),wf(:,15))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-5),wf(:,16))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,17))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-4),wf(:,18))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-4),wf(:,19))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-4),wf(:,20))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-4),wf(:,21))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,22))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,23))
  call vert_AV_Q(wf(:,-3),wf(:,22),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,7),ZERO,0_intkind1,wf(:,25))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,26))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,27))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,28))
  call prop_W_W(wf(:,28),Q(:,112),MZ,1_intkind1,wf(:,29))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,29),wf(:,30))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,31))
  call prop_W_W(wf(:,31),Q(:,112),MZ,1_intkind1,wf(:,32))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,32),wf(:,33))
  call vert_ZQ_A(gZl,wf(:,9),wf(:,-1),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,7),ZERO,0_intkind1,wf(:,35))
  call vert_VQ_A(wf(:,22),wf(:,-1),wf(:,36))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,114),ZERO,0_intkind1,wf(:,38))
  call vert_VQ_A(wf(:,26),wf(:,-1),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,114),ZERO,0_intkind1,wf(:,40))
  call vert_ZQ_A(gZl,wf(:,29),wf(:,-1),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,114),ZERO,0_intkind1,wf(:,42))
  call vert_ZQ_A(gZl,wf(:,32),wf(:,-1),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,114),ZERO,0_intkind1,wf(:,44))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,9),wf(:,45))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,46))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,47))
  call counter_VVG_G(wf(:,47),wf(:,46),wf(:,-6),wf(:,48))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,49))
  call prop_W_W(wf(:,49),Q(:,6),MZ,1_intkind1,wf(:,50))
  call counter_VVG_G(wf(:,50),wf(:,46),wf(:,-6),wf(:,51))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-3),wf(:,52))
  call prop_W_W(wf(:,52),Q(:,9),MZ,1_intkind1,wf(:,53))
  call counter_VVG_G(wf(:,47),wf(:,53),wf(:,-6),wf(:,54))
  call counter_VVG_G(wf(:,50),wf(:,53),wf(:,-6),wf(:,55))
  call counter_VVG_G(wf(:,47),wf(:,46),wf(:,-5),wf(:,56))
  call counter_VVG_G(wf(:,50),wf(:,46),wf(:,-5),wf(:,57))
  call counter_VVG_G(wf(:,47),wf(:,53),wf(:,-5),wf(:,58))
  call counter_VVG_G(wf(:,50),wf(:,53),wf(:,-5),wf(:,59))
  call counter_VVG_G(wf(:,47),wf(:,46),wf(:,-4),wf(:,60))
  call counter_VVG_G(wf(:,50),wf(:,46),wf(:,-4),wf(:,61))
  call counter_VVG_G(wf(:,47),wf(:,53),wf(:,-4),wf(:,62))
  call counter_VVG_G(wf(:,50),wf(:,53),wf(:,-4),wf(:,63))
  call vert_VQ_A(wf(:,47),wf(:,0),wf(:,64))
  call prop_Q_A(wf(:,64),Q(:,7),ZERO,0_intkind1,wf(:,65))
  call vert_ZQ_A(gZl,wf(:,50),wf(:,0),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,7),ZERO,0_intkind1,wf(:,67))
  call vert_VQ_A(wf(:,22),wf(:,0),wf(:,68))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,69))
  call prop_Q_A(wf(:,68),Q(:,113),ZERO,0_intkind1,wf(:,70))
  call vert_VQ_A(wf(:,26),wf(:,0),wf(:,71))
  call prop_Q_A(wf(:,71),Q(:,113),ZERO,0_intkind1,wf(:,72))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,50),wf(:,73))
  call vert_ZQ_A(gZl,wf(:,29),wf(:,0),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,113),ZERO,0_intkind1,wf(:,75))
  call vert_ZQ_A(gZl,wf(:,32),wf(:,0),wf(:,76))
  call prop_Q_A(wf(:,76),Q(:,113),ZERO,0_intkind1,wf(:,77))
  call vert_VQ_A(wf(:,46),wf(:,-1),wf(:,78))
  call vert_AV_Q(wf(:,-2),wf(:,22),wf(:,79))
  call prop_Q_A(wf(:,78),Q(:,11),ZERO,0_intkind1,wf(:,80))
  call vert_AV_Q(wf(:,-2),wf(:,26),wf(:,81))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,29),wf(:,82))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,32),wf(:,83))
  call vert_ZQ_A(gZl,wf(:,53),wf(:,-1),wf(:,84))
  call prop_Q_A(wf(:,84),Q(:,11),ZERO,0_intkind1,wf(:,85))
  call vert_AV_Q(wf(:,-2),wf(:,46),wf(:,86))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,53),wf(:,87))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,88))
  call prop_Q_A(wf(:,88),Q(:,11),ZERO,0_intkind1,wf(:,89))
  call vert_ZQ_A(gZl,wf(:,6),wf(:,0),wf(:,90))
  call prop_Q_A(wf(:,90),Q(:,11),ZERO,0_intkind1,wf(:,91))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,92))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,6),wf(:,93))
  call vert_VV_S(wf(:,9),wf(:,6),wf(:,94))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,95))
  call counter_GG_S(wf(:,-5),wf(:,12),wf(:,96))
  call counter_GG_S(wf(:,-4),wf(:,17),wf(:,97))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,98))
  call prop_W_W(wf(:,98),Q(:,112),MZ,1_intkind1,wf(:,99))
  call vert_QA_Z(gZl,wf(:,25),wf(:,-3),wf(:,100))
  call vert_QA_Z(gZl,wf(:,35),wf(:,-3),wf(:,101))
  call prop_A_Q(wf(:,37),Q(:,13),ZERO,0_intkind1,wf(:,102))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,102),wf(:,103))
  call prop_A_Q(wf(:,45),Q(:,13),ZERO,0_intkind1,wf(:,104))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,104),wf(:,105))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,12),Q(:,80),wf(:,106))
  call prop_W_W(wf(:,106),Q(:,112),MZ,1_intkind1,wf(:,107))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,17),Q(:,96),wf(:,108))
  call prop_W_W(wf(:,108),Q(:,112),MZ,1_intkind1,wf(:,109))
  call vert_VV_S(wf(:,50),wf(:,53),wf(:,110))
  call vert_QA_Z(gZl,wf(:,65),wf(:,-3),wf(:,111))
  call vert_QA_Z(gZl,wf(:,67),wf(:,-3),wf(:,112))
  call prop_A_Q(wf(:,69),Q(:,14),ZERO,0_intkind1,wf(:,113))
  call vert_QA_Z(gZl,wf(:,0),wf(:,113),wf(:,114))
  call prop_A_Q(wf(:,73),Q(:,14),ZERO,0_intkind1,wf(:,115))
  call vert_QA_Z(gZl,wf(:,0),wf(:,115),wf(:,116))
  call vert_QA_Z(gZl,wf(:,80),wf(:,-2),wf(:,117))
  call vert_QA_Z(gZl,wf(:,85),wf(:,-2),wf(:,118))
  call prop_A_Q(wf(:,86),Q(:,13),ZERO,0_intkind1,wf(:,119))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,119),wf(:,120))
  call prop_A_Q(wf(:,87),Q(:,13),ZERO,0_intkind1,wf(:,121))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,121),wf(:,122))
  call vert_QA_Z(gZl,wf(:,89),wf(:,-2),wf(:,123))
  call vert_QA_Z(gZl,wf(:,91),wf(:,-2),wf(:,124))
  call prop_A_Q(wf(:,92),Q(:,14),ZERO,0_intkind1,wf(:,125))
  call vert_QA_Z(gZl,wf(:,0),wf(:,125),wf(:,126))
  call prop_A_Q(wf(:,93),Q(:,14),ZERO,0_intkind1,wf(:,127))
  call vert_QA_Z(gZl,wf(:,0),wf(:,127),wf(:,128))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,129))
  call prop_W_W(wf(:,100),Q(:,15),MZ,1_intkind1,wf(:,130))
  call vert_QA_V(wf(:,35),wf(:,-3),wf(:,131))
  call prop_W_W(wf(:,101),Q(:,15),MZ,1_intkind1,wf(:,132))
  call vert_QA_V(wf(:,-1),wf(:,102),wf(:,133))
  call prop_W_W(wf(:,103),Q(:,15),MZ,1_intkind1,wf(:,134))
  call vert_QA_V(wf(:,-1),wf(:,104),wf(:,135))
  call prop_W_W(wf(:,105),Q(:,15),MZ,1_intkind1,wf(:,136))
  call vert_QA_V(wf(:,65),wf(:,-3),wf(:,137))
  call prop_W_W(wf(:,111),Q(:,15),MZ,1_intkind1,wf(:,138))
  call vert_QA_V(wf(:,67),wf(:,-3),wf(:,139))
  call prop_W_W(wf(:,112),Q(:,15),MZ,1_intkind1,wf(:,140))
  call vert_QA_V(wf(:,0),wf(:,113),wf(:,141))
  call vert_QA_V(wf(:,0),wf(:,115),wf(:,142))
  call prop_W_W(wf(:,114),Q(:,15),MZ,1_intkind1,wf(:,143))
  call prop_W_W(wf(:,116),Q(:,15),MZ,1_intkind1,wf(:,144))
  call vert_QA_V(wf(:,80),wf(:,-2),wf(:,145))
  call prop_W_W(wf(:,117),Q(:,15),MZ,1_intkind1,wf(:,146))
  call vert_QA_V(wf(:,85),wf(:,-2),wf(:,147))
  call prop_W_W(wf(:,118),Q(:,15),MZ,1_intkind1,wf(:,148))
  call vert_QA_V(wf(:,-1),wf(:,119),wf(:,149))
  call prop_W_W(wf(:,120),Q(:,15),MZ,1_intkind1,wf(:,150))
  call vert_QA_V(wf(:,-1),wf(:,121),wf(:,151))
  call prop_W_W(wf(:,122),Q(:,15),MZ,1_intkind1,wf(:,152))
  call vert_QA_V(wf(:,89),wf(:,-2),wf(:,153))
  call prop_W_W(wf(:,123),Q(:,15),MZ,1_intkind1,wf(:,154))
  call vert_QA_V(wf(:,91),wf(:,-2),wf(:,155))
  call prop_W_W(wf(:,124),Q(:,15),MZ,1_intkind1,wf(:,156))
  call vert_QA_V(wf(:,0),wf(:,125),wf(:,157))
  call vert_QA_V(wf(:,0),wf(:,127),wf(:,158))
  call prop_W_W(wf(:,126),Q(:,15),MZ,1_intkind1,wf(:,159))
  call prop_W_W(wf(:,128),Q(:,15),MZ,1_intkind1,wf(:,160))

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
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,10) - MZ2)
  den(9) = 1 / (Q(5,5) - MZ2)
  den(14) = 1 / (Q(5,80))
  den(19) = 1 / (Q(5,96))
  den(24) = 1 / (Q(5,112))
  den(25) = 1 / (Q(5,7))
  den(28) = 1 / (Q(5,112) - MZ2)
  den(33) = 1 / (Q(5,114))
  den(40) = 1 / (Q(5,9))
  den(41) = 1 / (Q(5,6))
  den(44) = 1 / (Q(5,6) - MZ2)
  den(47) = 1 / (Q(5,9) - MZ2)
  den(66) = 1 / (Q(5,113))
  den(73) = 1 / (Q(5,11))
  den(94) = 1 / (Q(5,15) - MH2)
  den(102) = 1 / (Q(5,13))
  den(123) = 1 / (Q(5,14))
  den(164) = 1 / (Q(5,15))
  den(166) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(3)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(3)*den(12)
  den(15) = den(4)*den(14)
  den(16) = den(7)*den(14)
  den(17) = den(10)*den(14)
  den(18) = den(12)*den(14)
  den(20) = den(4)*den(19)
  den(21) = den(7)*den(19)
  den(22) = den(10)*den(19)
  den(23) = den(12)*den(19)
  den(26) = den(1)*den(25)
  den(27) = den(24)*den(26)
  den(29) = den(26)*den(28)
  den(30) = den(9)*den(25)
  den(31) = den(24)*den(30)
  den(32) = den(28)*den(30)
  den(34) = den(24)*den(33)
  den(35) = den(1)*den(34)
  den(36) = den(28)*den(33)
  den(37) = den(1)*den(36)
  den(38) = den(9)*den(34)
  den(39) = den(9)*den(36)
  den(42) = den(40)*den(41)
  den(43) = den(3)*den(42)
  den(45) = den(40)*den(44)
  den(46) = den(3)*den(45)
  den(48) = den(41)*den(47)
  den(49) = den(3)*den(48)
  den(50) = den(44)*den(47)
  den(51) = den(3)*den(50)
  den(52) = den(14)*den(42)
  den(53) = den(14)*den(45)
  den(54) = den(14)*den(48)
  den(55) = den(14)*den(50)
  den(56) = den(19)*den(42)
  den(57) = den(19)*den(45)
  den(58) = den(19)*den(48)
  den(59) = den(19)*den(50)
  den(60) = den(25)*den(41)
  den(61) = den(24)*den(60)
  den(62) = den(28)*den(60)
  den(63) = den(25)*den(44)
  den(64) = den(24)*den(63)
  den(65) = den(28)*den(63)
  den(67) = den(24)*den(66)
  den(68) = den(41)*den(67)
  den(69) = den(44)*den(67)
  den(70) = den(28)*den(66)
  den(71) = den(41)*den(70)
  den(72) = den(44)*den(70)
  den(74) = den(40)*den(73)
  den(75) = den(24)*den(74)
  den(76) = den(28)*den(74)
  den(77) = den(47)*den(73)
  den(78) = den(24)*den(77)
  den(79) = den(28)*den(77)
  den(80) = den(34)*den(40)
  den(81) = den(36)*den(40)
  den(82) = den(34)*den(47)
  den(83) = den(36)*den(47)
  den(84) = den(2)*den(73)
  den(85) = den(24)*den(84)
  den(86) = den(28)*den(84)
  den(87) = den(6)*den(73)
  den(88) = den(24)*den(87)
  den(89) = den(28)*den(87)
  den(90) = den(2)*den(67)
  den(91) = den(6)*den(67)
  den(92) = den(2)*den(70)
  den(93) = den(6)*den(70)
  den(95) = den(12)*den(94)
  den(96) = den(3)*den(95)
  den(97) = den(14)*den(95)
  den(98) = den(19)*den(95)
  den(99) = den(3)*den(28)
  den(100) = den(26)*den(99)
  den(101) = den(30)*den(99)
  den(103) = den(1)*den(102)
  den(104) = den(99)*den(103)
  den(105) = den(9)*den(102)
  den(106) = den(99)*den(105)
  den(107) = den(14)*den(28)
  den(108) = den(26)*den(107)
  den(109) = den(30)*den(107)
  den(110) = den(103)*den(107)
  den(111) = den(105)*den(107)
  den(112) = den(19)*den(28)
  den(113) = den(26)*den(112)
  den(114) = den(30)*den(112)
  den(115) = den(103)*den(112)
  den(116) = den(105)*den(112)
  den(117) = den(50)*den(94)
  den(118) = den(3)*den(117)
  den(119) = den(14)*den(117)
  den(120) = den(19)*den(117)
  den(121) = den(60)*den(99)
  den(122) = den(63)*den(99)
  den(124) = den(41)*den(123)
  den(125) = den(99)*den(124)
  den(126) = den(44)*den(123)
  den(127) = den(99)*den(126)
  den(128) = den(60)*den(107)
  den(129) = den(63)*den(107)
  den(130) = den(107)*den(124)
  den(131) = den(107)*den(126)
  den(132) = den(60)*den(112)
  den(133) = den(63)*den(112)
  den(134) = den(112)*den(124)
  den(135) = den(112)*den(126)
  den(136) = den(74)*den(99)
  den(137) = den(77)*den(99)
  den(138) = den(40)*den(102)
  den(139) = den(99)*den(138)
  den(140) = den(47)*den(102)
  den(141) = den(99)*den(140)
  den(142) = den(74)*den(107)
  den(143) = den(77)*den(107)
  den(144) = den(107)*den(138)
  den(145) = den(107)*den(140)
  den(146) = den(74)*den(112)
  den(147) = den(77)*den(112)
  den(148) = den(112)*den(138)
  den(149) = den(112)*den(140)
  den(150) = den(84)*den(99)
  den(151) = den(87)*den(99)
  den(152) = den(2)*den(123)
  den(153) = den(99)*den(152)
  den(154) = den(6)*den(123)
  den(155) = den(99)*den(154)
  den(156) = den(84)*den(107)
  den(157) = den(87)*den(107)
  den(158) = den(107)*den(152)
  den(159) = den(107)*den(154)
  den(160) = den(84)*den(112)
  den(161) = den(87)*den(112)
  den(162) = den(112)*den(152)
  den(163) = den(112)*den(154)
  den(165) = den(26)*den(164)
  den(167) = den(26)*den(166)
  den(168) = den(30)*den(164)
  den(169) = den(30)*den(166)
  den(170) = den(103)*den(164)
  den(171) = den(103)*den(166)
  den(172) = den(105)*den(164)
  den(173) = den(105)*den(166)
  den(174) = den(60)*den(164)
  den(175) = den(60)*den(166)
  den(176) = den(63)*den(164)
  den(177) = den(63)*den(166)
  den(178) = den(124)*den(164)
  den(179) = den(126)*den(164)
  den(180) = den(124)*den(166)
  den(181) = den(126)*den(166)
  den(182) = den(74)*den(164)
  den(183) = den(74)*den(166)
  den(184) = den(77)*den(164)
  den(185) = den(77)*den(166)
  den(186) = den(138)*den(164)
  den(187) = den(138)*den(166)
  den(188) = den(140)*den(164)
  den(189) = den(140)*den(166)
  den(190) = den(84)*den(164)
  den(191) = den(84)*den(166)
  den(192) = den(87)*den(164)
  den(193) = den(87)*den(166)
  den(194) = den(152)*den(164)
  den(195) = den(154)*den(164)
  den(196) = den(152)*den(166)
  den(197) = den(154)*den(166)
  den(198) = den(1)*den(2)*den(3)
  den(199) = den(1)*den(3)*den(6)
  den(200) = den(2)*den(3)*den(9)
  den(201) = den(3)*den(6)*den(9)
  den(202) = den(1)*den(2)*den(14)
  den(203) = den(1)*den(6)*den(14)
  den(204) = den(2)*den(9)*den(14)
  den(205) = den(6)*den(9)*den(14)
  den(206) = den(1)*den(2)*den(19)
  den(207) = den(1)*den(6)*den(19)
  den(208) = den(2)*den(9)*den(19)
  den(209) = den(6)*den(9)*den(19)
  den(210) = den(3)*den(165)
  den(211) = den(3)*den(167)
  den(212) = den(3)*den(168)
  den(213) = den(3)*den(169)
  den(214) = den(3)*den(170)
  den(215) = den(3)*den(171)
  den(216) = den(3)*den(172)
  den(217) = den(3)*den(173)
  den(218) = den(14)*den(165)
  den(219) = den(14)*den(167)
  den(220) = den(14)*den(168)
  den(221) = den(14)*den(169)
  den(222) = den(14)*den(170)
  den(223) = den(14)*den(171)
  den(224) = den(14)*den(172)
  den(225) = den(14)*den(173)
  den(226) = den(19)*den(165)
  den(227) = den(19)*den(167)
  den(228) = den(19)*den(168)
  den(229) = den(19)*den(169)
  den(230) = den(19)*den(170)
  den(231) = den(19)*den(171)
  den(232) = den(19)*den(172)
  den(233) = den(19)*den(173)
  den(234) = den(3)*den(40)*den(41)
  den(235) = den(3)*den(40)*den(44)
  den(236) = den(3)*den(41)*den(47)
  den(237) = den(3)*den(44)*den(47)
  den(238) = den(14)*den(40)*den(41)
  den(239) = den(14)*den(40)*den(44)
  den(240) = den(14)*den(41)*den(47)
  den(241) = den(14)*den(44)*den(47)
  den(242) = den(19)*den(40)*den(41)
  den(243) = den(19)*den(40)*den(44)
  den(244) = den(19)*den(41)*den(47)
  den(245) = den(19)*den(44)*den(47)
  den(246) = den(3)*den(174)
  den(247) = den(3)*den(175)
  den(248) = den(3)*den(176)
  den(249) = den(3)*den(177)
  den(250) = den(3)*den(178)
  den(251) = den(3)*den(179)
  den(252) = den(3)*den(180)
  den(253) = den(3)*den(181)
  den(254) = den(14)*den(174)
  den(255) = den(14)*den(175)
  den(256) = den(14)*den(176)
  den(257) = den(14)*den(177)
  den(258) = den(14)*den(178)
  den(259) = den(14)*den(179)
  den(260) = den(14)*den(180)
  den(261) = den(14)*den(181)
  den(262) = den(19)*den(174)
  den(263) = den(19)*den(175)
  den(264) = den(19)*den(176)
  den(265) = den(19)*den(177)
  den(266) = den(19)*den(178)
  den(267) = den(19)*den(179)
  den(268) = den(19)*den(180)
  den(269) = den(19)*den(181)
  den(270) = den(3)*den(182)
  den(271) = den(3)*den(183)
  den(272) = den(3)*den(184)
  den(273) = den(3)*den(185)
  den(274) = den(3)*den(186)
  den(275) = den(3)*den(187)
  den(276) = den(3)*den(188)
  den(277) = den(3)*den(189)
  den(278) = den(14)*den(182)
  den(279) = den(14)*den(183)
  den(280) = den(14)*den(184)
  den(281) = den(14)*den(185)
  den(282) = den(14)*den(186)
  den(283) = den(14)*den(187)
  den(284) = den(14)*den(188)
  den(285) = den(14)*den(189)
  den(286) = den(19)*den(182)
  den(287) = den(19)*den(183)
  den(288) = den(19)*den(184)
  den(289) = den(19)*den(185)
  den(290) = den(19)*den(186)
  den(291) = den(19)*den(187)
  den(292) = den(19)*den(188)
  den(293) = den(19)*den(189)
  den(294) = den(3)*den(190)
  den(295) = den(3)*den(191)
  den(296) = den(3)*den(192)
  den(297) = den(3)*den(193)
  den(298) = den(3)*den(194)
  den(299) = den(3)*den(195)
  den(300) = den(3)*den(196)
  den(301) = den(3)*den(197)
  den(302) = den(14)*den(190)
  den(303) = den(14)*den(191)
  den(304) = den(14)*den(192)
  den(305) = den(14)*den(193)
  den(306) = den(14)*den(194)
  den(307) = den(14)*den(195)
  den(308) = den(14)*den(196)
  den(309) = den(14)*den(197)
  den(310) = den(19)*den(190)
  den(311) = den(19)*den(191)
  den(312) = den(19)*den(192)
  den(313) = den(19)*den(193)
  den(314) = den(19)*den(194)
  den(315) = den(19)*den(195)
  den(316) = den(19)*den(196)
  den(317) = den(19)*den(197)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(142)


  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,7)) * den(8)
  A(3) = cont_VV(wf(:,3),wf(:,10)) * den(11)
  A(4) = cont_VV(wf(:,3),wf(:,11)) * den(13)
  A(5) = cont_VV(wf(:,12),wf(:,13)) * den(15)
  A(6) = cont_VV(wf(:,12),wf(:,14)) * den(16)
  A(7) = cont_VV(wf(:,12),wf(:,15)) * den(17)
  A(8) = cont_VV(wf(:,12),wf(:,16)) * den(18)
  A(9) = cont_VV(wf(:,17),wf(:,18)) * den(20)
  A(10) = cont_VV(wf(:,17),wf(:,19)) * den(21)
  A(11) = cont_VV(wf(:,17),wf(:,20)) * den(22)
  A(12) = cont_VV(wf(:,17),wf(:,21)) * den(23)
  A(13) = cont_QA(wf(:,24),wf(:,25)) * den(27)
  A(14) = cont_QA(wf(:,25),wf(:,27)) * den(27)
  A(15) = cont_QA(wf(:,25),wf(:,30)) * den(29)
  A(16) = cont_QA(wf(:,25),wf(:,33)) * den(29)
  A(17) = cont_QA(wf(:,24),wf(:,35)) * den(31)
  A(18) = cont_QA(wf(:,27),wf(:,35)) * den(31)
  A(19) = cont_QA(wf(:,30),wf(:,35)) * den(32)
  A(20) = cont_QA(wf(:,33),wf(:,35)) * den(32)
  A(21) = cont_QA(wf(:,37),wf(:,38)) * den(35)
  A(22) = cont_QA(wf(:,37),wf(:,40)) * den(35)
  A(23) = cont_QA(wf(:,37),wf(:,42)) * den(37)
  A(24) = cont_QA(wf(:,37),wf(:,44)) * den(37)
  A(25) = cont_QA(wf(:,38),wf(:,45)) * den(38)
  A(26) = cont_QA(wf(:,40),wf(:,45)) * den(38)
  A(27) = cont_QA(wf(:,42),wf(:,45)) * den(39)
  A(28) = cont_QA(wf(:,44),wf(:,45)) * den(39)
  A(29) = cont_VV(wf(:,3),wf(:,48)) * den(43)
  A(30) = cont_VV(wf(:,3),wf(:,51)) * den(46)
  A(31) = cont_VV(wf(:,3),wf(:,54)) * den(49)
  A(32) = cont_VV(wf(:,3),wf(:,55)) * den(51)
  A(33) = cont_VV(wf(:,12),wf(:,56)) * den(52)
  A(34) = cont_VV(wf(:,12),wf(:,57)) * den(53)
  A(35) = cont_VV(wf(:,12),wf(:,58)) * den(54)
  A(36) = cont_VV(wf(:,12),wf(:,59)) * den(55)
  A(37) = cont_VV(wf(:,17),wf(:,60)) * den(56)
  A(38) = cont_VV(wf(:,17),wf(:,61)) * den(57)
  A(39) = cont_VV(wf(:,17),wf(:,62)) * den(58)
  A(40) = cont_VV(wf(:,17),wf(:,63)) * den(59)
  A(41) = cont_QA(wf(:,24),wf(:,65)) * den(61)
  A(42) = cont_QA(wf(:,27),wf(:,65)) * den(61)
  A(43) = cont_QA(wf(:,30),wf(:,65)) * den(62)
  A(44) = cont_QA(wf(:,33),wf(:,65)) * den(62)
  A(45) = cont_QA(wf(:,24),wf(:,67)) * den(64)
  A(46) = cont_QA(wf(:,27),wf(:,67)) * den(64)
  A(47) = cont_QA(wf(:,30),wf(:,67)) * den(65)
  A(48) = cont_QA(wf(:,33),wf(:,67)) * den(65)
  A(49) = cont_QA(wf(:,69),wf(:,70)) * den(68)
  A(50) = cont_QA(wf(:,69),wf(:,72)) * den(68)
  A(51) = cont_QA(wf(:,70),wf(:,73)) * den(69)
  A(52) = cont_QA(wf(:,72),wf(:,73)) * den(69)
  A(53) = cont_QA(wf(:,69),wf(:,75)) * den(71)
  A(54) = cont_QA(wf(:,69),wf(:,77)) * den(71)
  A(55) = cont_QA(wf(:,73),wf(:,75)) * den(72)
  A(56) = cont_QA(wf(:,73),wf(:,77)) * den(72)
  A(57) = cont_QA(wf(:,79),wf(:,80)) * den(75)
  A(58) = cont_QA(wf(:,80),wf(:,81)) * den(75)
  A(59) = cont_QA(wf(:,80),wf(:,82)) * den(76)
  A(60) = cont_QA(wf(:,80),wf(:,83)) * den(76)
  A(61) = cont_QA(wf(:,79),wf(:,85)) * den(78)
  A(62) = cont_QA(wf(:,81),wf(:,85)) * den(78)
  A(63) = cont_QA(wf(:,82),wf(:,85)) * den(79)
  A(64) = cont_QA(wf(:,83),wf(:,85)) * den(79)
  A(65) = cont_QA(wf(:,38),wf(:,86)) * den(80)
  A(66) = cont_QA(wf(:,40),wf(:,86)) * den(80)
  A(67) = cont_QA(wf(:,42),wf(:,86)) * den(81)
  A(68) = cont_QA(wf(:,44),wf(:,86)) * den(81)
  A(69) = cont_QA(wf(:,38),wf(:,87)) * den(82)
  A(70) = cont_QA(wf(:,40),wf(:,87)) * den(82)
  A(71) = cont_QA(wf(:,42),wf(:,87)) * den(83)
  A(72) = cont_QA(wf(:,44),wf(:,87)) * den(83)
  A(73) = cont_QA(wf(:,79),wf(:,89)) * den(85)
  A(74) = cont_QA(wf(:,81),wf(:,89)) * den(85)
  A(75) = cont_QA(wf(:,82),wf(:,89)) * den(86)
  A(76) = cont_QA(wf(:,83),wf(:,89)) * den(86)
  A(77) = cont_QA(wf(:,79),wf(:,91)) * den(88)
  A(78) = cont_QA(wf(:,81),wf(:,91)) * den(88)
  A(79) = cont_QA(wf(:,82),wf(:,91)) * den(89)
  A(80) = cont_QA(wf(:,83),wf(:,91)) * den(89)
  A(81) = cont_QA(wf(:,70),wf(:,92)) * den(90)
  A(82) = cont_QA(wf(:,72),wf(:,92)) * den(90)
  A(83) = cont_QA(wf(:,70),wf(:,93)) * den(91)
  A(84) = cont_QA(wf(:,72),wf(:,93)) * den(91)
  A(85) = cont_QA(wf(:,75),wf(:,92)) * den(92)
  A(86) = cont_QA(wf(:,77),wf(:,92)) * den(92)
  A(87) = cont_QA(wf(:,75),wf(:,93)) * den(93)
  A(88) = cont_QA(wf(:,77),wf(:,93)) * den(93)
  A(89) = cont_SS(wf(:,94),wf(:,95)) * den(96)
  A(90) = cont_SS(wf(:,94),wf(:,96)) * den(97)
  A(91) = cont_SS(wf(:,94),wf(:,97)) * den(98)
  A(92) = cont_VV(wf(:,99),wf(:,100)) * den(100)
  A(93) = cont_VV(wf(:,99),wf(:,101)) * den(101)
  A(94) = cont_VV(wf(:,99),wf(:,103)) * den(104)
  A(95) = cont_VV(wf(:,99),wf(:,105)) * den(106)
  A(96) = cont_VV(wf(:,100),wf(:,107)) * den(108)
  A(97) = cont_VV(wf(:,101),wf(:,107)) * den(109)
  A(98) = cont_VV(wf(:,103),wf(:,107)) * den(110)
  A(99) = cont_VV(wf(:,105),wf(:,107)) * den(111)
  A(100) = cont_VV(wf(:,100),wf(:,109)) * den(113)
  A(101) = cont_VV(wf(:,101),wf(:,109)) * den(114)
  A(102) = cont_VV(wf(:,103),wf(:,109)) * den(115)
  A(103) = cont_VV(wf(:,105),wf(:,109)) * den(116)
  A(104) = cont_SS(wf(:,95),wf(:,110)) * den(118)
  A(105) = cont_SS(wf(:,96),wf(:,110)) * den(119)
  A(106) = cont_SS(wf(:,97),wf(:,110)) * den(120)
  A(107) = cont_VV(wf(:,99),wf(:,111)) * den(121)
  A(108) = cont_VV(wf(:,99),wf(:,112)) * den(122)
  A(109) = cont_VV(wf(:,99),wf(:,114)) * den(125)
  A(110) = cont_VV(wf(:,99),wf(:,116)) * den(127)
  A(111) = cont_VV(wf(:,107),wf(:,111)) * den(128)
  A(112) = cont_VV(wf(:,107),wf(:,112)) * den(129)
  A(113) = cont_VV(wf(:,107),wf(:,114)) * den(130)
  A(114) = cont_VV(wf(:,107),wf(:,116)) * den(131)
  A(115) = cont_VV(wf(:,109),wf(:,111)) * den(132)
  A(116) = cont_VV(wf(:,109),wf(:,112)) * den(133)
  A(117) = cont_VV(wf(:,109),wf(:,114)) * den(134)
  A(118) = cont_VV(wf(:,109),wf(:,116)) * den(135)
  A(119) = cont_VV(wf(:,99),wf(:,117)) * den(136)
  A(120) = cont_VV(wf(:,99),wf(:,118)) * den(137)
  A(121) = cont_VV(wf(:,99),wf(:,120)) * den(139)
  A(122) = cont_VV(wf(:,99),wf(:,122)) * den(141)
  A(123) = cont_VV(wf(:,107),wf(:,117)) * den(142)
  A(124) = cont_VV(wf(:,107),wf(:,118)) * den(143)
  A(125) = cont_VV(wf(:,107),wf(:,120)) * den(144)
  A(126) = cont_VV(wf(:,107),wf(:,122)) * den(145)
  A(127) = cont_VV(wf(:,109),wf(:,117)) * den(146)
  A(128) = cont_VV(wf(:,109),wf(:,118)) * den(147)
  A(129) = cont_VV(wf(:,109),wf(:,120)) * den(148)
  A(130) = cont_VV(wf(:,109),wf(:,122)) * den(149)
  A(131) = cont_VV(wf(:,99),wf(:,123)) * den(150)
  A(132) = cont_VV(wf(:,99),wf(:,124)) * den(151)
  A(133) = cont_VV(wf(:,99),wf(:,126)) * den(153)
  A(134) = cont_VV(wf(:,99),wf(:,128)) * den(155)
  A(135) = cont_VV(wf(:,107),wf(:,123)) * den(156)
  A(136) = cont_VV(wf(:,107),wf(:,124)) * den(157)
  A(137) = cont_VV(wf(:,107),wf(:,126)) * den(158)
  A(138) = cont_VV(wf(:,107),wf(:,128)) * den(159)
  A(139) = cont_VV(wf(:,109),wf(:,123)) * den(160)
  A(140) = cont_VV(wf(:,109),wf(:,124)) * den(161)
  A(141) = cont_VV(wf(:,109),wf(:,126)) * den(162)
  A(142) = cont_VV(wf(:,109),wf(:,128)) * den(163)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(142)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (-A(13)-A(15)-A(17)-A(19)-A(21)-A(23)-A(25)-A(27)+A(41)+A(43)+A(45)+A(47)+A(49)+A(51)+A(53)+A(55)+A(57)+A(59)+A(61) &
       +A(63)+A(65)+A(67)+A(69)+A(71)-A(73)-A(75)-A(77)-A(79)-A(81)-A(83)-A(85)-A(87))*f(1)+2*CI*(-A(1)+A(5)-A(9)+A(29)-A(33) &
       +A(37))*f(2)+2*CI*(-A(2)-A(3)+A(6)+A(7)-A(10)-A(11)+A(30)+A(31)-A(34)-A(35)+A(38)+A(39))*f(3)+2*CI*(-A(92)-A(93)-A(94) &
       -A(95)+A(96)+A(97)+A(98)+A(99)-A(100)-A(101)-A(102)-A(103)+A(107)+A(108)+A(109)+A(110)-A(111)-A(112)-A(113)-A(114)+A(115) &
       +A(116)+A(117)+A(118)+A(119)+A(120)+A(121)+A(122)-A(123)-A(124)-A(125)-A(126)+A(127)+A(128)+A(129)+A(130)-A(131)-A(132) &
       -A(133)-A(134)+A(135)+A(136)+A(137)+A(138)-A(139)-A(140)-A(141)-A(142))*f(4)+2*CI*(-A(4)+A(8)-A(12)+A(32)-A(36)+A(40))*f(5) &
       +2*CI*(A(89)-A(90)+A(91)-A(104)+A(105)-A(106))*f(6)
  M2(2) = (-A(14)-A(16)-A(18)-A(20)-A(22)-A(24)-A(26)-A(28)+A(42)+A(44)+A(46)+A(48)+A(50)+A(52)+A(54)+A(56)+A(58)+A(60)+A(62) &
       +A(64)+A(66)+A(68)+A(70)+A(72)-A(74)-A(76)-A(78)-A(80)-A(82)-A(84)-A(86)-A(88))*f(1)+2*CI*(A(1)-A(5)+A(9)-A(29)+A(33) &
       -A(37))*f(2)+2*CI*(A(2)+A(3)-A(6)-A(7)+A(10)+A(11)-A(30)-A(31)+A(34)+A(35)-A(38)-A(39))*f(3)+2*CI*(A(92)+A(93)+A(94)+A(95) &
       -A(96)-A(97)-A(98)-A(99)+A(100)+A(101)+A(102)+A(103)-A(107)-A(108)-A(109)-A(110)+A(111)+A(112)+A(113)+A(114)-A(115)-A(116) &
       -A(117)-A(118)-A(119)-A(120)-A(121)-A(122)+A(123)+A(124)+A(125)+A(126)-A(127)-A(128)-A(129)-A(130)+A(131)+A(132)+A(133) &
       +A(134)-A(135)-A(136)-A(137)-A(138)+A(139)+A(140)+A(141)+A(142))*f(4)+2*CI*(A(4)-A(8)+A(12)-A(32)+A(36)-A(40))*f(5)+2*CI*( &
       -A(89)+A(90)-A(91)+A(104)-A(105)+A(106))*f(6)

end subroutine colourvectors

end module ol_loop_ppllllj2_eeexexggg_1_/**/REALKIND
