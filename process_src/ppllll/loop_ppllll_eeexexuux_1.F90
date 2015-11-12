
module ol_colourmatrix_ppllll_eeexexuux_1_/**/REALKIND
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

  K1( 1,:) = [  3]
  K1( 2,:) = [  0]
  K1( 3,:) = [  0]
  K1( 4,:) = [  0]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  0]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [  0]
  K1(11,:) = [  0]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  4]
  K1(17,:) = [  0]
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [ -4]
  K1(22,:) = [  4]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllll_eeexexuux_1_/**/REALKIND



module ol_forced_parameters_ppllll_eeexexuux_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll_eeexexuux_1_/**/REALKIND

module ol_loop_ppllll_eeexexuux_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:178)
  ! denominators
  complex(REALKIND), save :: den(182)
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
    f( 1) = (4*CI*eQED**4)/9._/**/REALKIND
    f( 2) = (2*CI*eQED**4)/3._/**/REALKIND
    f( 3) = CI*eQED**4
    f( 4) = (4*CI*countertermnorm*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 5) = (2*CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*eQED**4*gQCD**2
    f( 7) = (4*CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 8) = (2*CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctVqq*eQED**4*gQCD**2
    f(10) = (4*eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(11) = (2*eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(12) = eQED**4*gQCD**2*integralnorm*SwB

  c = [ 4*f(10), 4*f(11), 4*f(12) ]
  c = (1._/**/REALKIND / 3) * c
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
  complex(REALKIND) :: A(128)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,3))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,21),ZERO,0_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-3),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,10),MZ,1_intkind1,wf(:,7))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,7),wf(:,8))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-2),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,5),MZ,1_intkind1,wf(:,10))
  call vert_ZQ_A(gZu,wf(:,10),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,11),Q(:,21),ZERO,0_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,13))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,26),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,-4),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,26),ZERO,0_intkind1,wf(:,17))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,10),wf(:,18))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,19))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,20))
  call vert_AV_Q(wf(:,-3),wf(:,19),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,7),ZERO,0_intkind1,wf(:,22))
  call vert_QA_Z(gZu,wf(:,-4),wf(:,-5),wf(:,23))
  call prop_W_W(wf(:,23),Q(:,48),MZ,1_intkind1,wf(:,24))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,24),wf(:,25))
  call vert_ZQ_A(gZl,wf(:,10),wf(:,-1),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,7),ZERO,0_intkind1,wf(:,27))
  call vert_VQ_A(wf(:,19),wf(:,-1),wf(:,28))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,29))
  call prop_Q_A(wf(:,28),Q(:,50),ZERO,0_intkind1,wf(:,30))
  call vert_ZQ_A(gZl,wf(:,24),wf(:,-1),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,50),ZERO,0_intkind1,wf(:,32))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,10),wf(:,33))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,34))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,35))
  call vert_VQ_A(wf(:,34),wf(:,-4),wf(:,36))
  call vert_AV_Q(wf(:,-5),wf(:,35),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,25),ZERO,0_intkind1,wf(:,38))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,39))
  call prop_W_W(wf(:,39),Q(:,6),MZ,1_intkind1,wf(:,40))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,40),wf(:,41))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-3),wf(:,42))
  call prop_W_W(wf(:,42),Q(:,9),MZ,1_intkind1,wf(:,43))
  call vert_ZQ_A(gZu,wf(:,43),wf(:,-4),wf(:,44))
  call prop_Q_A(wf(:,44),Q(:,25),ZERO,0_intkind1,wf(:,45))
  call vert_VQ_A(wf(:,35),wf(:,-4),wf(:,46))
  call vert_AV_Q(wf(:,-5),wf(:,34),wf(:,47))
  call prop_Q_A(wf(:,46),Q(:,22),ZERO,0_intkind1,wf(:,48))
  call vert_ZQ_A(gZu,wf(:,40),wf(:,-4),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,22),ZERO,0_intkind1,wf(:,50))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,43),wf(:,51))
  call vert_VQ_A(wf(:,35),wf(:,0),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,7),ZERO,0_intkind1,wf(:,53))
  call vert_ZQ_A(gZl,wf(:,40),wf(:,0),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,7),ZERO,0_intkind1,wf(:,55))
  call vert_VQ_A(wf(:,19),wf(:,0),wf(:,56))
  call vert_AV_Q(wf(:,-3),wf(:,35),wf(:,57))
  call prop_Q_A(wf(:,56),Q(:,49),ZERO,0_intkind1,wf(:,58))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,40),wf(:,59))
  call vert_ZQ_A(gZl,wf(:,24),wf(:,0),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,49),ZERO,0_intkind1,wf(:,61))
  call vert_VQ_A(wf(:,34),wf(:,-1),wf(:,62))
  call vert_AV_Q(wf(:,-2),wf(:,19),wf(:,63))
  call prop_Q_A(wf(:,62),Q(:,11),ZERO,0_intkind1,wf(:,64))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,24),wf(:,65))
  call vert_ZQ_A(gZl,wf(:,43),wf(:,-1),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,11),ZERO,0_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,-2),wf(:,34),wf(:,68))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,43),wf(:,69))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,70))
  call prop_Q_A(wf(:,70),Q(:,11),ZERO,0_intkind1,wf(:,71))
  call vert_ZQ_A(gZl,wf(:,7),wf(:,0),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,11),ZERO,0_intkind1,wf(:,73))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,74))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,7),wf(:,75))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,76))
  call counter_AZ_Q(gZu,wf(:,-5),wf(:,7),wf(:,77))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,78))
  call counter_AZ_Q(gZu,wf(:,-5),wf(:,10),wf(:,79))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,80))
  call prop_A_Q(wf(:,14),Q(:,37),ZERO,0_intkind1,wf(:,81))
  call counter_ZQ_A(gZu,wf(:,7),wf(:,-4),wf(:,82))
  call prop_A_Q(wf(:,18),Q(:,37),ZERO,0_intkind1,wf(:,83))
  call counter_VQ_A(wf(:,1),wf(:,-4),wf(:,84))
  call prop_A_Q(wf(:,4),Q(:,42),ZERO,0_intkind1,wf(:,85))
  call prop_A_Q(wf(:,8),Q(:,42),ZERO,0_intkind1,wf(:,86))
  call counter_ZQ_A(gZu,wf(:,10),wf(:,-4),wf(:,87))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,88))
  call vert_AV_Q(wf(:,-3),wf(:,88),wf(:,89))
  call counter_QA_Z(gZu,wf(:,-4),wf(:,-5),wf(:,90))
  call prop_W_W(wf(:,90),Q(:,48),MZ,1_intkind1,wf(:,91))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,91),wf(:,92))
  call vert_VQ_A(wf(:,88),wf(:,-1),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,50),ZERO,0_intkind1,wf(:,94))
  call vert_ZQ_A(gZl,wf(:,91),wf(:,-1),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,50),ZERO,0_intkind1,wf(:,96))
  call counter_AV_Q(wf(:,-5),wf(:,35),wf(:,97))
  call counter_AZ_Q(gZu,wf(:,-5),wf(:,40),wf(:,98))
  call counter_AV_Q(wf(:,-5),wf(:,34),wf(:,99))
  call counter_AZ_Q(gZu,wf(:,-5),wf(:,43),wf(:,100))
  call counter_VQ_A(wf(:,35),wf(:,-4),wf(:,101))
  call prop_A_Q(wf(:,47),Q(:,41),ZERO,0_intkind1,wf(:,102))
  call counter_ZQ_A(gZu,wf(:,40),wf(:,-4),wf(:,103))
  call prop_A_Q(wf(:,51),Q(:,41),ZERO,0_intkind1,wf(:,104))
  call counter_VQ_A(wf(:,34),wf(:,-4),wf(:,105))
  call prop_A_Q(wf(:,37),Q(:,38),ZERO,0_intkind1,wf(:,106))
  call prop_A_Q(wf(:,41),Q(:,38),ZERO,0_intkind1,wf(:,107))
  call counter_ZQ_A(gZu,wf(:,43),wf(:,-4),wf(:,108))
  call vert_VQ_A(wf(:,88),wf(:,0),wf(:,109))
  call prop_Q_A(wf(:,109),Q(:,49),ZERO,0_intkind1,wf(:,110))
  call vert_ZQ_A(gZl,wf(:,91),wf(:,0),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,49),ZERO,0_intkind1,wf(:,112))
  call vert_AV_Q(wf(:,-2),wf(:,88),wf(:,113))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,91),wf(:,114))
  call counter_Q_A(ctqq,wf(:,5),Q(:,21),wf(:,115))
  call counter_Q_A(ctqq,wf(:,12),Q(:,21),wf(:,116))
  call counter_Q_A(ctqq,wf(:,15),Q(:,26),wf(:,117))
  call counter_Q_A(ctqq,wf(:,17),Q(:,26),wf(:,118))
  call counter_Q_A(ctqq,wf(:,38),Q(:,25),wf(:,119))
  call counter_Q_A(ctqq,wf(:,45),Q(:,25),wf(:,120))
  call counter_Q_A(ctqq,wf(:,48),Q(:,22),wf(:,121))
  call counter_Q_A(ctqq,wf(:,50),Q(:,22),wf(:,122))
  call vert_QA_V(wf(:,22),wf(:,-3),wf(:,123))
  call vert_QA_Z(gZl,wf(:,22),wf(:,-3),wf(:,124))
  call prop_W_W(wf(:,124),Q(:,15),MZ,1_intkind1,wf(:,125))
  call vert_QA_V(wf(:,27),wf(:,-3),wf(:,126))
  call vert_QA_Z(gZl,wf(:,27),wf(:,-3),wf(:,127))
  call prop_W_W(wf(:,127),Q(:,15),MZ,1_intkind1,wf(:,128))
  call prop_A_Q(wf(:,29),Q(:,13),ZERO,0_intkind1,wf(:,129))
  call vert_QA_V(wf(:,-1),wf(:,129),wf(:,130))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,129),wf(:,131))
  call prop_W_W(wf(:,131),Q(:,15),MZ,1_intkind1,wf(:,132))
  call prop_A_Q(wf(:,33),Q(:,13),ZERO,0_intkind1,wf(:,133))
  call vert_QA_V(wf(:,-1),wf(:,133),wf(:,134))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,133),wf(:,135))
  call prop_W_W(wf(:,135),Q(:,15),MZ,1_intkind1,wf(:,136))
  call vert_QA_V(wf(:,53),wf(:,-3),wf(:,137))
  call vert_QA_Z(gZl,wf(:,53),wf(:,-3),wf(:,138))
  call prop_W_W(wf(:,138),Q(:,15),MZ,1_intkind1,wf(:,139))
  call vert_QA_V(wf(:,55),wf(:,-3),wf(:,140))
  call vert_QA_Z(gZl,wf(:,55),wf(:,-3),wf(:,141))
  call prop_W_W(wf(:,141),Q(:,15),MZ,1_intkind1,wf(:,142))
  call prop_A_Q(wf(:,57),Q(:,14),ZERO,0_intkind1,wf(:,143))
  call vert_QA_V(wf(:,0),wf(:,143),wf(:,144))
  call prop_A_Q(wf(:,59),Q(:,14),ZERO,0_intkind1,wf(:,145))
  call vert_QA_V(wf(:,0),wf(:,145),wf(:,146))
  call vert_QA_Z(gZl,wf(:,0),wf(:,143),wf(:,147))
  call prop_W_W(wf(:,147),Q(:,15),MZ,1_intkind1,wf(:,148))
  call vert_QA_Z(gZl,wf(:,0),wf(:,145),wf(:,149))
  call prop_W_W(wf(:,149),Q(:,15),MZ,1_intkind1,wf(:,150))
  call vert_QA_V(wf(:,64),wf(:,-2),wf(:,151))
  call vert_QA_Z(gZl,wf(:,64),wf(:,-2),wf(:,152))
  call prop_W_W(wf(:,152),Q(:,15),MZ,1_intkind1,wf(:,153))
  call vert_QA_V(wf(:,67),wf(:,-2),wf(:,154))
  call vert_QA_Z(gZl,wf(:,67),wf(:,-2),wf(:,155))
  call prop_W_W(wf(:,155),Q(:,15),MZ,1_intkind1,wf(:,156))
  call prop_A_Q(wf(:,68),Q(:,13),ZERO,0_intkind1,wf(:,157))
  call vert_QA_V(wf(:,-1),wf(:,157),wf(:,158))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,157),wf(:,159))
  call prop_W_W(wf(:,159),Q(:,15),MZ,1_intkind1,wf(:,160))
  call prop_A_Q(wf(:,69),Q(:,13),ZERO,0_intkind1,wf(:,161))
  call vert_QA_V(wf(:,-1),wf(:,161),wf(:,162))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,161),wf(:,163))
  call prop_W_W(wf(:,163),Q(:,15),MZ,1_intkind1,wf(:,164))
  call vert_QA_V(wf(:,71),wf(:,-2),wf(:,165))
  call vert_QA_Z(gZl,wf(:,71),wf(:,-2),wf(:,166))
  call prop_W_W(wf(:,166),Q(:,15),MZ,1_intkind1,wf(:,167))
  call vert_QA_V(wf(:,73),wf(:,-2),wf(:,168))
  call vert_QA_Z(gZl,wf(:,73),wf(:,-2),wf(:,169))
  call prop_W_W(wf(:,169),Q(:,15),MZ,1_intkind1,wf(:,170))
  call prop_A_Q(wf(:,74),Q(:,14),ZERO,0_intkind1,wf(:,171))
  call vert_QA_V(wf(:,0),wf(:,171),wf(:,172))
  call prop_A_Q(wf(:,75),Q(:,14),ZERO,0_intkind1,wf(:,173))
  call vert_QA_V(wf(:,0),wf(:,173),wf(:,174))
  call vert_QA_Z(gZl,wf(:,0),wf(:,171),wf(:,175))
  call prop_W_W(wf(:,175),Q(:,15),MZ,1_intkind1,wf(:,176))
  call vert_QA_Z(gZl,wf(:,0),wf(:,173),wf(:,177))
  call prop_W_W(wf(:,177),Q(:,15),MZ,1_intkind1,wf(:,178))

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
  den(3) = 1 / (Q(5,21))
  den(6) = 1 / (Q(5,10) - MZ2)
  den(8) = 1 / (Q(5,5) - MZ2)
  den(12) = 1 / (Q(5,26))
  den(19) = 1 / (Q(5,48))
  den(20) = 1 / (Q(5,7))
  den(23) = 1 / (Q(5,48) - MZ2)
  den(28) = 1 / (Q(5,50))
  den(35) = 1 / (Q(5,9))
  den(36) = 1 / (Q(5,6))
  den(37) = 1 / (Q(5,25))
  den(40) = 1 / (Q(5,6) - MZ2)
  den(42) = 1 / (Q(5,9) - MZ2)
  den(46) = 1 / (Q(5,22))
  den(59) = 1 / (Q(5,49))
  den(66) = 1 / (Q(5,11))
  den(87) = 1 / (Q(5,37))
  den(94) = 1 / (Q(5,42))
  den(101) = 1 / (Q(5,41))
  den(108) = 1 / (Q(5,38))
  den(131) = 1 / (Q(5,15))
  den(133) = 1 / (Q(5,15) - MZ2)
  den(137) = 1 / (Q(5,13))
  den(148) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(3)*den(8)
  den(10) = den(2)*den(9)
  den(11) = den(6)*den(9)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(15) = den(6)*den(12)
  den(16) = den(1)*den(15)
  den(17) = den(8)*den(13)
  den(18) = den(8)*den(15)
  den(21) = den(1)*den(20)
  den(22) = den(19)*den(21)
  den(24) = den(21)*den(23)
  den(25) = den(8)*den(20)
  den(26) = den(19)*den(25)
  den(27) = den(23)*den(25)
  den(29) = den(19)*den(28)
  den(30) = den(1)*den(29)
  den(31) = den(23)*den(28)
  den(32) = den(1)*den(31)
  den(33) = den(8)*den(29)
  den(34) = den(8)*den(31)
  den(38) = den(35)*den(37)
  den(39) = den(36)*den(38)
  den(41) = den(38)*den(40)
  den(43) = den(37)*den(42)
  den(44) = den(36)*den(43)
  den(45) = den(40)*den(43)
  den(47) = den(36)*den(46)
  den(48) = den(35)*den(47)
  den(49) = den(40)*den(46)
  den(50) = den(35)*den(49)
  den(51) = den(42)*den(47)
  den(52) = den(42)*den(49)
  den(53) = den(20)*den(36)
  den(54) = den(19)*den(53)
  den(55) = den(23)*den(53)
  den(56) = den(20)*den(40)
  den(57) = den(19)*den(56)
  den(58) = den(23)*den(56)
  den(60) = den(19)*den(59)
  den(61) = den(36)*den(60)
  den(62) = den(40)*den(60)
  den(63) = den(23)*den(59)
  den(64) = den(36)*den(63)
  den(65) = den(40)*den(63)
  den(67) = den(35)*den(66)
  den(68) = den(19)*den(67)
  den(69) = den(23)*den(67)
  den(70) = den(42)*den(66)
  den(71) = den(19)*den(70)
  den(72) = den(23)*den(70)
  den(73) = den(29)*den(35)
  den(74) = den(31)*den(35)
  den(75) = den(29)*den(42)
  den(76) = den(31)*den(42)
  den(77) = den(2)*den(66)
  den(78) = den(19)*den(77)
  den(79) = den(23)*den(77)
  den(80) = den(6)*den(66)
  den(81) = den(19)*den(80)
  den(82) = den(23)*den(80)
  den(83) = den(2)*den(60)
  den(84) = den(6)*den(60)
  den(85) = den(2)*den(63)
  den(86) = den(6)*den(63)
  den(88) = den(1)*den(87)
  den(89) = den(2)*den(88)
  den(90) = den(6)*den(88)
  den(91) = den(8)*den(87)
  den(92) = den(2)*den(91)
  den(93) = den(6)*den(91)
  den(95) = den(2)*den(94)
  den(96) = den(1)*den(95)
  den(97) = den(6)*den(94)
  den(98) = den(1)*den(97)
  den(99) = den(8)*den(95)
  den(100) = den(8)*den(97)
  den(102) = den(35)*den(101)
  den(103) = den(36)*den(102)
  den(104) = den(40)*den(102)
  den(105) = den(42)*den(101)
  den(106) = den(36)*den(105)
  den(107) = den(40)*den(105)
  den(109) = den(36)*den(108)
  den(110) = den(35)*den(109)
  den(111) = den(40)*den(108)
  den(112) = den(35)*den(111)
  den(113) = den(42)*den(109)
  den(114) = den(42)*den(111)
  den(115) = den(4)*den(95)
  den(116) = den(4)*den(97)
  den(117) = den(9)*den(95)
  den(118) = den(9)*den(97)
  den(119) = den(13)*den(88)
  den(120) = den(15)*den(88)
  den(121) = den(13)*den(91)
  den(122) = den(15)*den(91)
  den(123) = den(38)*den(109)
  den(124) = den(38)*den(111)
  den(125) = den(43)*den(109)
  den(126) = den(43)*den(111)
  den(127) = den(47)*den(102)
  den(128) = den(49)*den(102)
  den(129) = den(47)*den(105)
  den(130) = den(49)*den(105)
  den(132) = den(21)*den(131)
  den(134) = den(21)*den(133)
  den(135) = den(25)*den(131)
  den(136) = den(25)*den(133)
  den(138) = den(1)*den(137)
  den(139) = den(131)*den(138)
  den(140) = den(133)*den(138)
  den(141) = den(8)*den(137)
  den(142) = den(131)*den(141)
  den(143) = den(133)*den(141)
  den(144) = den(53)*den(131)
  den(145) = den(53)*den(133)
  den(146) = den(56)*den(131)
  den(147) = den(56)*den(133)
  den(149) = den(36)*den(148)
  den(150) = den(131)*den(149)
  den(151) = den(40)*den(148)
  den(152) = den(131)*den(151)
  den(153) = den(133)*den(149)
  den(154) = den(133)*den(151)
  den(155) = den(67)*den(131)
  den(156) = den(67)*den(133)
  den(157) = den(70)*den(131)
  den(158) = den(70)*den(133)
  den(159) = den(35)*den(137)
  den(160) = den(131)*den(159)
  den(161) = den(133)*den(159)
  den(162) = den(42)*den(137)
  den(163) = den(131)*den(162)
  den(164) = den(133)*den(162)
  den(165) = den(77)*den(131)
  den(166) = den(77)*den(133)
  den(167) = den(80)*den(131)
  den(168) = den(80)*den(133)
  den(169) = den(2)*den(148)
  den(170) = den(131)*den(169)
  den(171) = den(6)*den(148)
  den(172) = den(131)*den(171)
  den(173) = den(133)*den(169)
  den(174) = den(133)*den(171)
  den(175) = den(1)*den(2)
  den(176) = den(1)*den(6)
  den(177) = den(2)*den(8)
  den(178) = den(6)*den(8)
  den(179) = den(35)*den(36)
  den(180) = den(35)*den(40)
  den(181) = den(36)*den(42)
  den(182) = den(40)*den(42)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(128)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(5)
  A(2) = cont_QA(wf(:,5),wf(:,8)) * den(7)
  A(3) = cont_QA(wf(:,4),wf(:,12)) * den(10)
  A(4) = cont_QA(wf(:,8),wf(:,12)) * den(11)
  A(5) = cont_QA(wf(:,14),wf(:,15)) * den(14)
  A(6) = cont_QA(wf(:,14),wf(:,17)) * den(16)
  A(7) = cont_QA(wf(:,15),wf(:,18)) * den(17)
  A(8) = cont_QA(wf(:,17),wf(:,18)) * den(18)
  A(9) = cont_QA(wf(:,21),wf(:,22)) * den(22)
  A(10) = cont_QA(wf(:,22),wf(:,25)) * den(24)
  A(11) = cont_QA(wf(:,21),wf(:,27)) * den(26)
  A(12) = cont_QA(wf(:,25),wf(:,27)) * den(27)
  A(13) = cont_QA(wf(:,29),wf(:,30)) * den(30)
  A(14) = cont_QA(wf(:,29),wf(:,32)) * den(32)
  A(15) = cont_QA(wf(:,30),wf(:,33)) * den(33)
  A(16) = cont_QA(wf(:,32),wf(:,33)) * den(34)
  A(17) = cont_QA(wf(:,37),wf(:,38)) * den(39)
  A(18) = cont_QA(wf(:,38),wf(:,41)) * den(41)
  A(19) = cont_QA(wf(:,37),wf(:,45)) * den(44)
  A(20) = cont_QA(wf(:,41),wf(:,45)) * den(45)
  A(21) = cont_QA(wf(:,47),wf(:,48)) * den(48)
  A(22) = cont_QA(wf(:,47),wf(:,50)) * den(50)
  A(23) = cont_QA(wf(:,48),wf(:,51)) * den(51)
  A(24) = cont_QA(wf(:,50),wf(:,51)) * den(52)
  A(25) = cont_QA(wf(:,21),wf(:,53)) * den(54)
  A(26) = cont_QA(wf(:,25),wf(:,53)) * den(55)
  A(27) = cont_QA(wf(:,21),wf(:,55)) * den(57)
  A(28) = cont_QA(wf(:,25),wf(:,55)) * den(58)
  A(29) = cont_QA(wf(:,57),wf(:,58)) * den(61)
  A(30) = cont_QA(wf(:,58),wf(:,59)) * den(62)
  A(31) = cont_QA(wf(:,57),wf(:,61)) * den(64)
  A(32) = cont_QA(wf(:,59),wf(:,61)) * den(65)
  A(33) = cont_QA(wf(:,63),wf(:,64)) * den(68)
  A(34) = cont_QA(wf(:,64),wf(:,65)) * den(69)
  A(35) = cont_QA(wf(:,63),wf(:,67)) * den(71)
  A(36) = cont_QA(wf(:,65),wf(:,67)) * den(72)
  A(37) = cont_QA(wf(:,30),wf(:,68)) * den(73)
  A(38) = cont_QA(wf(:,32),wf(:,68)) * den(74)
  A(39) = cont_QA(wf(:,30),wf(:,69)) * den(75)
  A(40) = cont_QA(wf(:,32),wf(:,69)) * den(76)
  A(41) = cont_QA(wf(:,63),wf(:,71)) * den(78)
  A(42) = cont_QA(wf(:,65),wf(:,71)) * den(79)
  A(43) = cont_QA(wf(:,63),wf(:,73)) * den(81)
  A(44) = cont_QA(wf(:,65),wf(:,73)) * den(82)
  A(45) = cont_QA(wf(:,58),wf(:,74)) * den(83)
  A(46) = cont_QA(wf(:,58),wf(:,75)) * den(84)
  A(47) = cont_QA(wf(:,61),wf(:,74)) * den(85)
  A(48) = cont_QA(wf(:,61),wf(:,75)) * den(86)

  A(49) = cont_QA(wf(:,5),wf(:,76)) * den(5)
  A(50) = cont_QA(wf(:,5),wf(:,77)) * den(7)
  A(51) = cont_QA(wf(:,12),wf(:,76)) * den(10)
  A(52) = cont_QA(wf(:,12),wf(:,77)) * den(11)
  A(53) = cont_QA(wf(:,15),wf(:,78)) * den(14)
  A(54) = cont_QA(wf(:,17),wf(:,78)) * den(16)
  A(55) = cont_QA(wf(:,15),wf(:,79)) * den(17)
  A(56) = cont_QA(wf(:,17),wf(:,79)) * den(18)
  A(57) = cont_QA(wf(:,80),wf(:,81)) * den(89)
  A(58) = cont_QA(wf(:,81),wf(:,82)) * den(90)
  A(59) = cont_QA(wf(:,80),wf(:,83)) * den(92)
  A(60) = cont_QA(wf(:,82),wf(:,83)) * den(93)
  A(61) = cont_QA(wf(:,84),wf(:,85)) * den(96)
  A(62) = cont_QA(wf(:,84),wf(:,86)) * den(98)
  A(63) = cont_QA(wf(:,85),wf(:,87)) * den(99)
  A(64) = cont_QA(wf(:,86),wf(:,87)) * den(100)
  A(65) = cont_QA(wf(:,22),wf(:,89)) * den(22)
  A(66) = cont_QA(wf(:,22),wf(:,92)) * den(24)
  A(67) = cont_QA(wf(:,27),wf(:,89)) * den(26)
  A(68) = cont_QA(wf(:,27),wf(:,92)) * den(27)
  A(69) = cont_QA(wf(:,29),wf(:,94)) * den(30)
  A(70) = cont_QA(wf(:,29),wf(:,96)) * den(32)
  A(71) = cont_QA(wf(:,33),wf(:,94)) * den(33)
  A(72) = cont_QA(wf(:,33),wf(:,96)) * den(34)
  A(73) = cont_QA(wf(:,38),wf(:,97)) * den(39)
  A(74) = cont_QA(wf(:,38),wf(:,98)) * den(41)
  A(75) = cont_QA(wf(:,45),wf(:,97)) * den(44)
  A(76) = cont_QA(wf(:,45),wf(:,98)) * den(45)
  A(77) = cont_QA(wf(:,48),wf(:,99)) * den(48)
  A(78) = cont_QA(wf(:,50),wf(:,99)) * den(50)
  A(79) = cont_QA(wf(:,48),wf(:,100)) * den(51)
  A(80) = cont_QA(wf(:,50),wf(:,100)) * den(52)
  A(81) = cont_QA(wf(:,101),wf(:,102)) * den(103)
  A(82) = cont_QA(wf(:,102),wf(:,103)) * den(104)
  A(83) = cont_QA(wf(:,101),wf(:,104)) * den(106)
  A(84) = cont_QA(wf(:,103),wf(:,104)) * den(107)
  A(85) = cont_QA(wf(:,105),wf(:,106)) * den(110)
  A(86) = cont_QA(wf(:,105),wf(:,107)) * den(112)
  A(87) = cont_QA(wf(:,106),wf(:,108)) * den(113)
  A(88) = cont_QA(wf(:,107),wf(:,108)) * den(114)
  A(89) = cont_QA(wf(:,53),wf(:,89)) * den(54)
  A(90) = cont_QA(wf(:,53),wf(:,92)) * den(55)
  A(91) = cont_QA(wf(:,55),wf(:,89)) * den(57)
  A(92) = cont_QA(wf(:,55),wf(:,92)) * den(58)
  A(93) = cont_QA(wf(:,57),wf(:,110)) * den(61)
  A(94) = cont_QA(wf(:,59),wf(:,110)) * den(62)
  A(95) = cont_QA(wf(:,57),wf(:,112)) * den(64)
  A(96) = cont_QA(wf(:,59),wf(:,112)) * den(65)
  A(97) = cont_QA(wf(:,64),wf(:,113)) * den(68)
  A(98) = cont_QA(wf(:,64),wf(:,114)) * den(69)
  A(99) = cont_QA(wf(:,67),wf(:,113)) * den(71)
  A(100) = cont_QA(wf(:,67),wf(:,114)) * den(72)
  A(101) = cont_QA(wf(:,68),wf(:,94)) * den(73)
  A(102) = cont_QA(wf(:,68),wf(:,96)) * den(74)
  A(103) = cont_QA(wf(:,69),wf(:,94)) * den(75)
  A(104) = cont_QA(wf(:,69),wf(:,96)) * den(76)
  A(105) = cont_QA(wf(:,71),wf(:,113)) * den(78)
  A(106) = cont_QA(wf(:,71),wf(:,114)) * den(79)
  A(107) = cont_QA(wf(:,73),wf(:,113)) * den(81)
  A(108) = cont_QA(wf(:,73),wf(:,114)) * den(82)
  A(109) = cont_QA(wf(:,74),wf(:,110)) * den(83)
  A(110) = cont_QA(wf(:,75),wf(:,110)) * den(84)
  A(111) = cont_QA(wf(:,74),wf(:,112)) * den(85)
  A(112) = cont_QA(wf(:,75),wf(:,112)) * den(86)
  A(113) = cont_QA(wf(:,85),wf(:,115)) * den(115)
  A(114) = cont_QA(wf(:,86),wf(:,115)) * den(116)
  A(115) = cont_QA(wf(:,85),wf(:,116)) * den(117)
  A(116) = cont_QA(wf(:,86),wf(:,116)) * den(118)
  A(117) = cont_QA(wf(:,81),wf(:,117)) * den(119)
  A(118) = cont_QA(wf(:,81),wf(:,118)) * den(120)
  A(119) = cont_QA(wf(:,83),wf(:,117)) * den(121)
  A(120) = cont_QA(wf(:,83),wf(:,118)) * den(122)
  A(121) = cont_QA(wf(:,106),wf(:,119)) * den(123)
  A(122) = cont_QA(wf(:,107),wf(:,119)) * den(124)
  A(123) = cont_QA(wf(:,106),wf(:,120)) * den(125)
  A(124) = cont_QA(wf(:,107),wf(:,120)) * den(126)
  A(125) = cont_QA(wf(:,102),wf(:,121)) * den(127)
  A(126) = cont_QA(wf(:,102),wf(:,122)) * den(128)
  A(127) = cont_QA(wf(:,104),wf(:,121)) * den(129)
  A(128) = cont_QA(wf(:,104),wf(:,122)) * den(130)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(128)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(5)+A(17)+A(21))*f(1)+(A(2)+A(3)+A(6)+A(7)+A(9)+A(11)+A(13)+A(15)-A(18)-A(19)-A(22)-A(23)-A(25)-A(27)-A(29) &
       -A(30)-A(33)-A(35)-A(37)-A(39)+A(41)+A(43)+A(45)+A(46))*f(2)+(-A(4)-A(8)-A(10)-A(12)-A(14)-A(16)+A(20)+A(24)+A(26)+A(28) &
       +A(31)+A(32)+A(34)+A(36)+A(38)+A(40)-A(42)-A(44)-A(47)-A(48))*f(3)

  M2(1) = (A(113)+A(117)-A(121)-A(125))*f(4)+(-A(114)-A(115)-A(118)-A(119)+A(122)+A(123)+A(126)+A(127))*f(5)+(A(116)+A(120)-A(124) &
       -A(128))*f(6)+(-A(49)-A(53)-A(57)-A(61)+A(73)+A(77)+A(81)+A(85))*f(7)+(A(50)+A(51)+A(54)+A(55)+A(58)+A(59)+A(62)+A(63) &
       +A(65)+A(67)+A(69)+A(71)-A(74)-A(75)-A(78)-A(79)-A(82)-A(83)-A(86)-A(87)-A(89)-A(91)-A(93)-A(94)-A(97)-A(99)-A(101)-A(103) &
       +A(105)+A(107)+A(109)+A(110))*f(8)+(-A(52)-A(56)-A(60)-A(64)-A(66)-A(68)-A(70)-A(72)+A(76)+A(80)+A(84)+A(88)+A(90)+A(92) &
       +A(95)+A(96)+A(98)+A(100)+A(102)+A(104)-A(106)-A(108)-A(111)-A(112))*f(9)

end subroutine colourvectors

end module ol_loop_ppllll_eeexexuux_1_/**/REALKIND
