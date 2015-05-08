
module ol_colourmatrix_ppllllj2_eeexexuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,1), KL(0,1), KL2(1,1), KL2ct(1,1), KL2ct2(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [ 4]

  KL2ct(1,:) = [ 4]

  KL2ct2(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj2_eeexexuuxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_eeexexuuxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_eeexexuuxg_1_/**/REALKIND

module ol_loop_ppllllj2_eeexexuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(17), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:158)
  ! denominators
  complex(REALKIND), save :: den(190)
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
    f( 1) = CI*countertermnorm*ctAAGG*eQED**4*gQCD**3
    f( 2) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 3) = (2*countertermnorm*ctZGG*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 4) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 5) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 6) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f( 7) = (eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (2*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 9) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (4*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(11) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = (8*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(13) = eQED**4*gQCD**3*integralnorm*SwF
    f(14) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(15) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(16) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(17) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND), intent(out) :: M1(0), M2(1)
  complex(REALKIND) :: A(42)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-6),wf(:,4))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-3),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,10),MZ,1_intkind1,wf(:,6))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-6),wf(:,7))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-2),wf(:,8))
  call prop_W_W(wf(:,8),Q(:,5),MZ,1_intkind1,wf(:,9))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-6),wf(:,10))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-6),wf(:,11))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,12))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,13))
  call counter_VVG_G(wf(:,13),wf(:,12),wf(:,-6),wf(:,14))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,15))
  call prop_W_W(wf(:,15),Q(:,6),MZ,1_intkind1,wf(:,16))
  call counter_VVG_G(wf(:,16),wf(:,12),wf(:,-6),wf(:,17))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-3),wf(:,18))
  call prop_W_W(wf(:,18),Q(:,9),MZ,1_intkind1,wf(:,19))
  call counter_VVG_G(wf(:,13),wf(:,19),wf(:,-6),wf(:,20))
  call counter_VVG_G(wf(:,16),wf(:,19),wf(:,-6),wf(:,21))
  call vert_VV_S(wf(:,9),wf(:,6),wf(:,22))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,23))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,24))
  call counter_VG_G(wf(:,6),wf(:,-6),Q(:,64),wf(:,25),Q(:,74))
  call prop_Q_A(wf(:,24),Q(:,21),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,26),wf(:,-5),wf(:,27))
  call vert_ZQ_A(gZu,wf(:,9),wf(:,-4),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,21),ZERO,0_intkind1,wf(:,29))
  call vert_QA_V(wf(:,29),wf(:,-5),wf(:,30))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,37),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,-4),wf(:,32),wf(:,33))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,9),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,37),ZERO,0_intkind1,wf(:,35))
  call vert_QA_V(wf(:,-4),wf(:,35),wf(:,36))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,37))
  call counter_VG_G(wf(:,9),wf(:,-6),Q(:,64),wf(:,38),Q(:,69))
  call prop_Q_A(wf(:,37),Q(:,26),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,39),wf(:,-5),wf(:,40))
  call vert_ZQ_A(gZu,wf(:,6),wf(:,-4),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,26),ZERO,0_intkind1,wf(:,42))
  call vert_QA_V(wf(:,42),wf(:,-5),wf(:,43))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,44))
  call prop_A_Q(wf(:,44),Q(:,42),ZERO,0_intkind1,wf(:,45))
  call vert_QA_V(wf(:,-4),wf(:,45),wf(:,46))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,6),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,42),ZERO,0_intkind1,wf(:,48))
  call vert_QA_V(wf(:,-4),wf(:,48),wf(:,49))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,50))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,51))
  call prop_Q_A(wf(:,50),Q(:,7),ZERO,0_intkind1,wf(:,52))
  call prop_W_W(wf(:,51),Q(:,112),MZ,1_intkind1,wf(:,53))
  call vert_QA_Z(gZl,wf(:,52),wf(:,-3),wf(:,54))
  call vert_ZQ_A(gZl,wf(:,9),wf(:,-1),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,7),ZERO,0_intkind1,wf(:,56))
  call vert_QA_Z(gZl,wf(:,56),wf(:,-3),wf(:,57))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,58))
  call prop_A_Q(wf(:,58),Q(:,13),ZERO,0_intkind1,wf(:,59))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,59),wf(:,60))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,9),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,13),ZERO,0_intkind1,wf(:,62))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,62),wf(:,63))
  call vert_VV_S(wf(:,16),wf(:,19),wf(:,64))
  call vert_VQ_A(wf(:,12),wf(:,-4),wf(:,65))
  call counter_VG_G(wf(:,16),wf(:,-6),Q(:,64),wf(:,66),Q(:,70))
  call prop_Q_A(wf(:,65),Q(:,25),ZERO,0_intkind1,wf(:,67))
  call vert_QA_V(wf(:,67),wf(:,-5),wf(:,68))
  call vert_ZQ_A(gZu,wf(:,19),wf(:,-4),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,25),ZERO,0_intkind1,wf(:,70))
  call vert_QA_V(wf(:,70),wf(:,-5),wf(:,71))
  call vert_AV_Q(wf(:,-5),wf(:,12),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,41),ZERO,0_intkind1,wf(:,73))
  call vert_QA_V(wf(:,-4),wf(:,73),wf(:,74))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,19),wf(:,75))
  call prop_A_Q(wf(:,75),Q(:,41),ZERO,0_intkind1,wf(:,76))
  call vert_QA_V(wf(:,-4),wf(:,76),wf(:,77))
  call vert_VQ_A(wf(:,13),wf(:,-4),wf(:,78))
  call counter_VG_G(wf(:,19),wf(:,-6),Q(:,64),wf(:,79),Q(:,73))
  call prop_Q_A(wf(:,78),Q(:,22),ZERO,0_intkind1,wf(:,80))
  call vert_QA_V(wf(:,80),wf(:,-5),wf(:,81))
  call vert_ZQ_A(gZu,wf(:,16),wf(:,-4),wf(:,82))
  call prop_Q_A(wf(:,82),Q(:,22),ZERO,0_intkind1,wf(:,83))
  call vert_QA_V(wf(:,83),wf(:,-5),wf(:,84))
  call vert_AV_Q(wf(:,-5),wf(:,13),wf(:,85))
  call prop_A_Q(wf(:,85),Q(:,38),ZERO,0_intkind1,wf(:,86))
  call vert_QA_V(wf(:,-4),wf(:,86),wf(:,87))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,16),wf(:,88))
  call prop_A_Q(wf(:,88),Q(:,38),ZERO,0_intkind1,wf(:,89))
  call vert_QA_V(wf(:,-4),wf(:,89),wf(:,90))
  call vert_VQ_A(wf(:,13),wf(:,0),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,7),ZERO,0_intkind1,wf(:,92))
  call vert_QA_Z(gZl,wf(:,92),wf(:,-3),wf(:,93))
  call vert_ZQ_A(gZl,wf(:,16),wf(:,0),wf(:,94))
  call prop_Q_A(wf(:,94),Q(:,7),ZERO,0_intkind1,wf(:,95))
  call vert_QA_Z(gZl,wf(:,95),wf(:,-3),wf(:,96))
  call vert_AV_Q(wf(:,-3),wf(:,13),wf(:,97))
  call prop_A_Q(wf(:,97),Q(:,14),ZERO,0_intkind1,wf(:,98))
  call vert_QA_Z(gZl,wf(:,0),wf(:,98),wf(:,99))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,16),wf(:,100))
  call prop_A_Q(wf(:,100),Q(:,14),ZERO,0_intkind1,wf(:,101))
  call vert_QA_Z(gZl,wf(:,0),wf(:,101),wf(:,102))
  call vert_VQ_A(wf(:,12),wf(:,-1),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,11),ZERO,0_intkind1,wf(:,104))
  call vert_QA_Z(gZl,wf(:,104),wf(:,-2),wf(:,105))
  call vert_ZQ_A(gZl,wf(:,19),wf(:,-1),wf(:,106))
  call prop_Q_A(wf(:,106),Q(:,11),ZERO,0_intkind1,wf(:,107))
  call vert_QA_Z(gZl,wf(:,107),wf(:,-2),wf(:,108))
  call vert_AV_Q(wf(:,-2),wf(:,12),wf(:,109))
  call prop_A_Q(wf(:,109),Q(:,13),ZERO,0_intkind1,wf(:,110))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,110),wf(:,111))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,19),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,13),ZERO,0_intkind1,wf(:,113))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,113),wf(:,114))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,11),ZERO,0_intkind1,wf(:,116))
  call vert_QA_Z(gZl,wf(:,116),wf(:,-2),wf(:,117))
  call vert_ZQ_A(gZl,wf(:,6),wf(:,0),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,11),ZERO,0_intkind1,wf(:,119))
  call vert_QA_Z(gZl,wf(:,119),wf(:,-2),wf(:,120))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,121))
  call prop_A_Q(wf(:,121),Q(:,14),ZERO,0_intkind1,wf(:,122))
  call vert_QA_Z(gZl,wf(:,0),wf(:,122),wf(:,123))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,6),wf(:,124))
  call prop_A_Q(wf(:,124),Q(:,14),ZERO,0_intkind1,wf(:,125))
  call vert_QA_Z(gZl,wf(:,0),wf(:,125),wf(:,126))
  call vert_QA_V(wf(:,52),wf(:,-3),wf(:,127))
  call prop_W_W(wf(:,54),Q(:,15),MZ,1_intkind1,wf(:,128))
  call vert_QA_V(wf(:,56),wf(:,-3),wf(:,129))
  call prop_W_W(wf(:,57),Q(:,15),MZ,1_intkind1,wf(:,130))
  call vert_QA_V(wf(:,-1),wf(:,59),wf(:,131))
  call prop_W_W(wf(:,60),Q(:,15),MZ,1_intkind1,wf(:,132))
  call vert_QA_V(wf(:,-1),wf(:,62),wf(:,133))
  call prop_W_W(wf(:,63),Q(:,15),MZ,1_intkind1,wf(:,134))
  call vert_QA_V(wf(:,92),wf(:,-3),wf(:,135))
  call prop_W_W(wf(:,93),Q(:,15),MZ,1_intkind1,wf(:,136))
  call vert_QA_V(wf(:,95),wf(:,-3),wf(:,137))
  call prop_W_W(wf(:,96),Q(:,15),MZ,1_intkind1,wf(:,138))
  call vert_QA_V(wf(:,0),wf(:,98),wf(:,139))
  call vert_QA_V(wf(:,0),wf(:,101),wf(:,140))
  call prop_W_W(wf(:,99),Q(:,15),MZ,1_intkind1,wf(:,141))
  call prop_W_W(wf(:,102),Q(:,15),MZ,1_intkind1,wf(:,142))
  call vert_QA_V(wf(:,104),wf(:,-2),wf(:,143))
  call prop_W_W(wf(:,105),Q(:,15),MZ,1_intkind1,wf(:,144))
  call vert_QA_V(wf(:,107),wf(:,-2),wf(:,145))
  call prop_W_W(wf(:,108),Q(:,15),MZ,1_intkind1,wf(:,146))
  call vert_QA_V(wf(:,-1),wf(:,110),wf(:,147))
  call prop_W_W(wf(:,111),Q(:,15),MZ,1_intkind1,wf(:,148))
  call vert_QA_V(wf(:,-1),wf(:,113),wf(:,149))
  call prop_W_W(wf(:,114),Q(:,15),MZ,1_intkind1,wf(:,150))
  call vert_QA_V(wf(:,116),wf(:,-2),wf(:,151))
  call prop_W_W(wf(:,117),Q(:,15),MZ,1_intkind1,wf(:,152))
  call vert_QA_V(wf(:,119),wf(:,-2),wf(:,153))
  call prop_W_W(wf(:,120),Q(:,15),MZ,1_intkind1,wf(:,154))
  call vert_QA_V(wf(:,0),wf(:,122),wf(:,155))
  call vert_QA_V(wf(:,0),wf(:,125),wf(:,156))
  call prop_W_W(wf(:,123),Q(:,15),MZ,1_intkind1,wf(:,157))
  call prop_W_W(wf(:,126),Q(:,15),MZ,1_intkind1,wf(:,158))

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
  den(14) = 1 / (Q(5,9))
  den(15) = 1 / (Q(5,6))
  den(18) = 1 / (Q(5,6) - MZ2)
  den(21) = 1 / (Q(5,9) - MZ2)
  den(26) = 1 / (Q(5,15) - MH2)
  den(29) = 1 / (Q(5,21))
  den(31) = 1 / (Q(5,74))
  den(36) = 1 / (Q(5,37))
  den(41) = 1 / (Q(5,26))
  den(43) = 1 / (Q(5,69))
  den(48) = 1 / (Q(5,42))
  den(53) = 1 / (Q(5,7))
  den(55) = 1 / (Q(5,112) - MZ2)
  den(60) = 1 / (Q(5,13))
  den(67) = 1 / (Q(5,25))
  den(69) = 1 / (Q(5,70))
  den(74) = 1 / (Q(5,41))
  den(79) = 1 / (Q(5,22))
  den(81) = 1 / (Q(5,73))
  den(86) = 1 / (Q(5,38))
  den(95) = 1 / (Q(5,14))
  den(100) = 1 / (Q(5,11))
  den(117) = 1 / (Q(5,15))
  den(119) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(3)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(3)*den(12)
  den(16) = den(14)*den(15)
  den(17) = den(3)*den(16)
  den(19) = den(14)*den(18)
  den(20) = den(3)*den(19)
  den(22) = den(15)*den(21)
  den(23) = den(3)*den(22)
  den(24) = den(18)*den(21)
  den(25) = den(3)*den(24)
  den(27) = den(12)*den(26)
  den(28) = den(3)*den(27)
  den(30) = den(1)*den(29)
  den(32) = den(6)*den(31)
  den(33) = den(30)*den(32)
  den(34) = den(9)*den(29)
  den(35) = den(32)*den(34)
  den(37) = den(1)*den(36)
  den(38) = den(32)*den(37)
  den(39) = den(9)*den(36)
  den(40) = den(32)*den(39)
  den(42) = den(2)*den(41)
  den(44) = den(9)*den(43)
  den(45) = den(42)*den(44)
  den(46) = den(6)*den(41)
  den(47) = den(44)*den(46)
  den(49) = den(2)*den(48)
  den(50) = den(44)*den(49)
  den(51) = den(6)*den(48)
  den(52) = den(44)*den(51)
  den(54) = den(1)*den(53)
  den(56) = den(3)*den(55)
  den(57) = den(54)*den(56)
  den(58) = den(9)*den(53)
  den(59) = den(56)*den(58)
  den(61) = den(1)*den(60)
  den(62) = den(56)*den(61)
  den(63) = den(9)*den(60)
  den(64) = den(56)*den(63)
  den(65) = den(24)*den(26)
  den(66) = den(3)*den(65)
  den(68) = den(14)*den(67)
  den(70) = den(18)*den(69)
  den(71) = den(68)*den(70)
  den(72) = den(21)*den(67)
  den(73) = den(70)*den(72)
  den(75) = den(14)*den(74)
  den(76) = den(70)*den(75)
  den(77) = den(21)*den(74)
  den(78) = den(70)*den(77)
  den(80) = den(15)*den(79)
  den(82) = den(21)*den(81)
  den(83) = den(80)*den(82)
  den(84) = den(18)*den(79)
  den(85) = den(82)*den(84)
  den(87) = den(15)*den(86)
  den(88) = den(82)*den(87)
  den(89) = den(18)*den(86)
  den(90) = den(82)*den(89)
  den(91) = den(15)*den(53)
  den(92) = den(56)*den(91)
  den(93) = den(18)*den(53)
  den(94) = den(56)*den(93)
  den(96) = den(15)*den(95)
  den(97) = den(56)*den(96)
  den(98) = den(18)*den(95)
  den(99) = den(56)*den(98)
  den(101) = den(14)*den(100)
  den(102) = den(56)*den(101)
  den(103) = den(21)*den(100)
  den(104) = den(56)*den(103)
  den(105) = den(14)*den(60)
  den(106) = den(56)*den(105)
  den(107) = den(21)*den(60)
  den(108) = den(56)*den(107)
  den(109) = den(2)*den(100)
  den(110) = den(56)*den(109)
  den(111) = den(6)*den(100)
  den(112) = den(56)*den(111)
  den(113) = den(2)*den(95)
  den(114) = den(56)*den(113)
  den(115) = den(6)*den(95)
  den(116) = den(56)*den(115)
  den(118) = den(54)*den(117)
  den(120) = den(54)*den(119)
  den(121) = den(58)*den(117)
  den(122) = den(58)*den(119)
  den(123) = den(61)*den(117)
  den(124) = den(61)*den(119)
  den(125) = den(63)*den(117)
  den(126) = den(63)*den(119)
  den(127) = den(91)*den(117)
  den(128) = den(91)*den(119)
  den(129) = den(93)*den(117)
  den(130) = den(93)*den(119)
  den(131) = den(96)*den(117)
  den(132) = den(98)*den(117)
  den(133) = den(96)*den(119)
  den(134) = den(98)*den(119)
  den(135) = den(101)*den(117)
  den(136) = den(101)*den(119)
  den(137) = den(103)*den(117)
  den(138) = den(103)*den(119)
  den(139) = den(105)*den(117)
  den(140) = den(105)*den(119)
  den(141) = den(107)*den(117)
  den(142) = den(107)*den(119)
  den(143) = den(109)*den(117)
  den(144) = den(109)*den(119)
  den(145) = den(111)*den(117)
  den(146) = den(111)*den(119)
  den(147) = den(113)*den(117)
  den(148) = den(115)*den(117)
  den(149) = den(113)*den(119)
  den(150) = den(115)*den(119)
  den(151) = den(1)*den(2)*den(3)
  den(152) = den(1)*den(3)*den(6)
  den(153) = den(2)*den(3)*den(9)
  den(154) = den(3)*den(6)*den(9)
  den(155) = den(3)*den(118)
  den(156) = den(3)*den(120)
  den(157) = den(3)*den(121)
  den(158) = den(3)*den(122)
  den(159) = den(3)*den(123)
  den(160) = den(3)*den(124)
  den(161) = den(3)*den(125)
  den(162) = den(3)*den(126)
  den(163) = den(3)*den(14)*den(15)
  den(164) = den(3)*den(14)*den(18)
  den(165) = den(3)*den(15)*den(21)
  den(166) = den(3)*den(18)*den(21)
  den(167) = den(3)*den(127)
  den(168) = den(3)*den(128)
  den(169) = den(3)*den(129)
  den(170) = den(3)*den(130)
  den(171) = den(3)*den(131)
  den(172) = den(3)*den(132)
  den(173) = den(3)*den(133)
  den(174) = den(3)*den(134)
  den(175) = den(3)*den(135)
  den(176) = den(3)*den(136)
  den(177) = den(3)*den(137)
  den(178) = den(3)*den(138)
  den(179) = den(3)*den(139)
  den(180) = den(3)*den(140)
  den(181) = den(3)*den(141)
  den(182) = den(3)*den(142)
  den(183) = den(3)*den(143)
  den(184) = den(3)*den(144)
  den(185) = den(3)*den(145)
  den(186) = den(3)*den(146)
  den(187) = den(3)*den(147)
  den(188) = den(3)*den(148)
  den(189) = den(3)*den(149)
  den(190) = den(3)*den(150)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(42)


  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,7)) * den(8)
  A(3) = cont_VV(wf(:,3),wf(:,10)) * den(11)
  A(4) = cont_VV(wf(:,3),wf(:,11)) * den(13)
  A(5) = cont_VV(wf(:,3),wf(:,14)) * den(17)
  A(6) = cont_VV(wf(:,3),wf(:,17)) * den(20)
  A(7) = cont_VV(wf(:,3),wf(:,20)) * den(23)
  A(8) = cont_VV(wf(:,3),wf(:,21)) * den(25)
  A(9) = cont_SS(wf(:,22),wf(:,23)) * den(28)
  A(10) = cont_VV(wf(:,25),wf(:,27)) * den(33)
  A(11) = cont_VV(wf(:,25),wf(:,30)) * den(35)
  A(12) = cont_VV(wf(:,25),wf(:,33)) * den(38)
  A(13) = cont_VV(wf(:,25),wf(:,36)) * den(40)
  A(14) = cont_VV(wf(:,38),wf(:,40)) * den(45)
  A(15) = cont_VV(wf(:,38),wf(:,43)) * den(47)
  A(16) = cont_VV(wf(:,38),wf(:,46)) * den(50)
  A(17) = cont_VV(wf(:,38),wf(:,49)) * den(52)
  A(18) = cont_VV(wf(:,53),wf(:,54)) * den(57)
  A(19) = cont_VV(wf(:,53),wf(:,57)) * den(59)
  A(20) = cont_VV(wf(:,53),wf(:,60)) * den(62)
  A(21) = cont_VV(wf(:,53),wf(:,63)) * den(64)
  A(22) = cont_SS(wf(:,23),wf(:,64)) * den(66)
  A(23) = cont_VV(wf(:,66),wf(:,68)) * den(71)
  A(24) = cont_VV(wf(:,66),wf(:,71)) * den(73)
  A(25) = cont_VV(wf(:,66),wf(:,74)) * den(76)
  A(26) = cont_VV(wf(:,66),wf(:,77)) * den(78)
  A(27) = cont_VV(wf(:,79),wf(:,81)) * den(83)
  A(28) = cont_VV(wf(:,79),wf(:,84)) * den(85)
  A(29) = cont_VV(wf(:,79),wf(:,87)) * den(88)
  A(30) = cont_VV(wf(:,79),wf(:,90)) * den(90)
  A(31) = cont_VV(wf(:,53),wf(:,93)) * den(92)
  A(32) = cont_VV(wf(:,53),wf(:,96)) * den(94)
  A(33) = cont_VV(wf(:,53),wf(:,99)) * den(97)
  A(34) = cont_VV(wf(:,53),wf(:,102)) * den(99)
  A(35) = cont_VV(wf(:,53),wf(:,105)) * den(102)
  A(36) = cont_VV(wf(:,53),wf(:,108)) * den(104)
  A(37) = cont_VV(wf(:,53),wf(:,111)) * den(106)
  A(38) = cont_VV(wf(:,53),wf(:,114)) * den(108)
  A(39) = cont_VV(wf(:,53),wf(:,117)) * den(110)
  A(40) = cont_VV(wf(:,53),wf(:,120)) * den(112)
  A(41) = cont_VV(wf(:,53),wf(:,123)) * den(114)
  A(42) = cont_VV(wf(:,53),wf(:,126)) * den(116)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(42)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = (-A(1)+A(5))*f(1)+(-A(2)-A(3)+A(6)+A(7))*f(2)+(-A(10)-A(12)-A(14)-A(16)+A(23)+A(25)+A(27)+A(29))*f(3)+(A(11)+A(13)+A(15) &
       +A(17)+A(18)+A(19)+A(20)+A(21)-A(24)-A(26)-A(28)-A(30)-A(31)-A(32)-A(33)-A(34)-A(35)-A(36)-A(37)-A(38)+A(39)+A(40)+A(41) &
       +A(42))*f(4)+(-A(4)+A(8))*f(5)+(A(9)-A(22))*f(6)

end subroutine colourvectors

end module ol_loop_ppllllj2_eeexexuuxg_1_/**/REALKIND
