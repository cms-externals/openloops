
module ol_colourmatrix_eevvjj_eexuuxzz_1_/**/REALKIND
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
  K1( 7,:) = [  4]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [ -4]
  K1(11,:) = [  4]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  0]
  K1(17,:) = [  0]
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [  0]
  K1(22,:) = [  0]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_eevvjj_eexuuxzz_1_/**/REALKIND



module ol_forced_parameters_eevvjj_eexuuxzz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_eevvjj_eexuuxzz_1_/**/REALKIND

module ol_loop_eevvjj_eexuuxzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:250)
  ! denominators
  complex(REALKIND), save :: den(243)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,144)
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
    f( 1) = (2*CI*eQED**4)/3._/**/REALKIND
    f( 2) = CI*eQED**4
    f( 3) = (2*CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 4) = CI*countertermnorm*eQED**4*gQCD**2
    f( 5) = (2*CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*ctVqq*eQED**4*gQCD**2
    f( 7) = (CI*eQED**4*MW**2)/(cw**4*sw**2)
    f( 8) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*MW**2)/(cw**4*sw**2)
    f( 9) = (2*eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(10) = eQED**4*gQCD**2*integralnorm*SwB
    f(11) = (eQED**4*gQCD**2*integralnorm*MW**2*SwB)/(cw**4*sw**2)

  c = [ 4*f(9), 4*f(10), 4*f(11) ]
  c = (1._/**/REALKIND / 3) * c
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(166)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rMZ, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rMZ, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rMZ, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rMZ, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,12),MZ,1_intkind1,wf(:,5))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,6))
  call vert_VV_S(wf(:,4),wf(:,-4),wf(:,7))
  call vert_VV_S(wf(:,5),wf(:,-5),wf(:,8))
  call vert_VV_S(wf(:,5),wf(:,-4),wf(:,9))
  call vert_VV_S(wf(:,4),wf(:,-5),wf(:,10))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,11))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,-2),wf(:,12))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,-5),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,20),ZERO,0_intkind1,wf(:,14))
  call prop_A_Q(wf(:,13),Q(:,40),ZERO,0_intkind1,wf(:,15))
  call vert_VQ_A(wf(:,11),wf(:,14),wf(:,16))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,14),wf(:,17))
  call vert_AV_Q(wf(:,-3),wf(:,11),wf(:,18))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,14),wf(:,19))
  call prop_A_Q(wf(:,18),Q(:,11),ZERO,0_intkind1,wf(:,20))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,4),wf(:,21))
  call prop_A_Q(wf(:,21),Q(:,11),ZERO,0_intkind1,wf(:,22))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,-2),wf(:,23))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,-4),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,36),ZERO,0_intkind1,wf(:,25))
  call prop_A_Q(wf(:,24),Q(:,24),ZERO,0_intkind1,wf(:,26))
  call vert_VQ_A(wf(:,11),wf(:,25),wf(:,27))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,25),wf(:,28))
  call vert_VQ_A(wf(:,11),wf(:,-2),wf(:,29))
  call vert_AZ_Q(gZu,wf(:,26),wf(:,-5),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,7),ZERO,0_intkind1,wf(:,31))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-2),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,7),ZERO,0_intkind1,wf(:,33))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,25),wf(:,34))
  call vert_AZ_Q(gZu,wf(:,15),wf(:,-4),wf(:,35))
  call vert_ZQ_A(gZl,wf(:,-4),wf(:,0),wf(:,36))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,-5),wf(:,37))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,38))
  call prop_Q_A(wf(:,36),Q(:,17),ZERO,0_intkind1,wf(:,39))
  call prop_A_Q(wf(:,37),Q(:,34),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,39),wf(:,40),wf(:,41))
  call vert_QA_Z(gZl,wf(:,39),wf(:,40),wf(:,42))
  call vert_AV_Q(wf(:,-1),wf(:,38),wf(:,43))
  call vert_ZQ_A(gZl,wf(:,-5),wf(:,39),wf(:,44))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,45))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,5),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,14),ZERO,0_intkind1,wf(:,47))
  call vert_ZQ_A(gZl,wf(:,-5),wf(:,0),wf(:,48))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,-4),wf(:,49))
  call prop_Q_A(wf(:,48),Q(:,33),ZERO,0_intkind1,wf(:,50))
  call prop_A_Q(wf(:,49),Q(:,18),ZERO,0_intkind1,wf(:,51))
  call vert_QA_V(wf(:,50),wf(:,51),wf(:,52))
  call vert_QA_Z(gZl,wf(:,50),wf(:,51),wf(:,53))
  call vert_VQ_A(wf(:,38),wf(:,0),wf(:,54))
  call vert_AZ_Q(gZl,wf(:,51),wf(:,-5),wf(:,55))
  call prop_Q_A(wf(:,54),Q(:,13),ZERO,0_intkind1,wf(:,56))
  call vert_ZQ_A(gZl,wf(:,5),wf(:,0),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,13),ZERO,0_intkind1,wf(:,58))
  call vert_ZQ_A(gZl,wf(:,-4),wf(:,50),wf(:,59))
  call vert_AZ_Q(gZl,wf(:,40),wf(:,-4),wf(:,60))
  call vert_QA_V(wf(:,39),wf(:,-1),wf(:,61))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,62))
  call vert_QA_Z(gZl,wf(:,39),wf(:,-1),wf(:,63))
  call vert_QA_Z(gZu,wf(:,25),wf(:,-3),wf(:,64))
  call prop_W_W(wf(:,63),Q(:,19),MZ,1_intkind1,wf(:,65))
  call vert_QA_V(wf(:,-2),wf(:,15),wf(:,66))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,15),wf(:,67))
  call vert_QA_V(wf(:,0),wf(:,51),wf(:,68))
  call vert_QA_Z(gZl,wf(:,0),wf(:,51),wf(:,69))
  call prop_W_W(wf(:,69),Q(:,19),MZ,1_intkind1,wf(:,70))
  call vert_QA_V(wf(:,50),wf(:,-1),wf(:,71))
  call vert_QA_V(wf(:,14),wf(:,-3),wf(:,72))
  call vert_QA_Z(gZl,wf(:,50),wf(:,-1),wf(:,73))
  call vert_QA_Z(gZu,wf(:,14),wf(:,-3),wf(:,74))
  call prop_W_W(wf(:,73),Q(:,35),MZ,1_intkind1,wf(:,75))
  call vert_QA_V(wf(:,0),wf(:,40),wf(:,76))
  call vert_QA_Z(gZl,wf(:,0),wf(:,40),wf(:,77))
  call prop_W_W(wf(:,77),Q(:,35),MZ,1_intkind1,wf(:,78))
  call vert_QA_V(wf(:,-2),wf(:,26),wf(:,79))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,26),wf(:,80))
  call counter_VQ_A(wf(:,11),wf(:,14),wf(:,81))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,14),wf(:,82))
  call counter_ZQ_A(gZu,wf(:,-5),wf(:,14),wf(:,83))
  call counter_VQ_A(wf(:,11),wf(:,25),wf(:,84))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,25),wf(:,85))
  call counter_AZ_Q(gZu,wf(:,26),wf(:,-5),wf(:,86))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,25),wf(:,87))
  call counter_AZ_Q(gZu,wf(:,15),wf(:,-4),wf(:,88))
  call counter_AV_Q(wf(:,-3),wf(:,11),wf(:,89))
  call prop_Q_A(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,90))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,4),wf(:,91))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,-5),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,40),ZERO,0_intkind1,wf(:,93))
  call prop_Q_A(wf(:,34),Q(:,52),ZERO,0_intkind1,wf(:,94))
  call vert_AZ_Q(gZu,wf(:,93),wf(:,-4),wf(:,95))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,-4),wf(:,96))
  call prop_A_Q(wf(:,96),Q(:,24),ZERO,0_intkind1,wf(:,97))
  call vert_AZ_Q(gZu,wf(:,97),wf(:,-5),wf(:,98))
  call counter_VQ_A(wf(:,11),wf(:,-2),wf(:,99))
  call prop_A_Q(wf(:,30),Q(:,56),ZERO,0_intkind1,wf(:,100))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,-2),wf(:,101))
  call counter_ZQ_A(gZu,wf(:,-5),wf(:,-2),wf(:,102))
  call prop_Q_A(wf(:,102),Q(:,36),ZERO,0_intkind1,wf(:,103))
  call vert_VQ_A(wf(:,11),wf(:,103),wf(:,104))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,103),wf(:,105))
  call prop_A_Q(wf(:,35),Q(:,56),ZERO,0_intkind1,wf(:,106))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,103),wf(:,107))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,-2),wf(:,108))
  call prop_Q_A(wf(:,108),Q(:,20),ZERO,0_intkind1,wf(:,109))
  call vert_VQ_A(wf(:,11),wf(:,109),wf(:,110))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,109),wf(:,111))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,109),wf(:,112))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,-3),wf(:,113))
  call prop_W_W(wf(:,113),Q(:,12),MZ,1_intkind1,wf(:,114))
  call vert_VV_S(wf(:,4),wf(:,114),wf(:,115))
  call vert_VV_S(wf(:,114),wf(:,-5),wf(:,116))
  call vert_VV_S(wf(:,114),wf(:,-4),wf(:,117))
  call counter_QA_V(wf(:,25),wf(:,-3),wf(:,118))
  call counter_QA_Z(gZu,wf(:,25),wf(:,-3),wf(:,119))
  call vert_QA_V(wf(:,-2),wf(:,93),wf(:,120))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,93),wf(:,121))
  call counter_QA_V(wf(:,14),wf(:,-3),wf(:,122))
  call counter_QA_Z(gZu,wf(:,14),wf(:,-3),wf(:,123))
  call vert_QA_V(wf(:,-2),wf(:,97),wf(:,124))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,97),wf(:,125))
  call counter_QA_V(wf(:,-2),wf(:,15),wf(:,126))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,15),wf(:,127))
  call vert_QA_V(wf(:,103),wf(:,-3),wf(:,128))
  call vert_QA_Z(gZu,wf(:,103),wf(:,-3),wf(:,129))
  call counter_QA_V(wf(:,-2),wf(:,26),wf(:,130))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,26),wf(:,131))
  call vert_QA_V(wf(:,109),wf(:,-3),wf(:,132))
  call vert_QA_Z(gZu,wf(:,109),wf(:,-3),wf(:,133))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,134))
  call vert_AV_Q(wf(:,-1),wf(:,134),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,14),ZERO,0_intkind1,wf(:,136))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,114),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,14),ZERO,0_intkind1,wf(:,138))
  call vert_VQ_A(wf(:,134),wf(:,0),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,13),ZERO,0_intkind1,wf(:,140))
  call vert_ZQ_A(gZl,wf(:,114),wf(:,0),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,13),ZERO,0_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,15),wf(:,11),wf(:,143))
  call counter_Q_A(ctqq,wf(:,14),Q(:,20),wf(:,144))
  call prop_A_Q(wf(:,143),Q(:,43),ZERO,0_intkind1,wf(:,145))
  call vert_AZ_Q(gZu,wf(:,15),wf(:,4),wf(:,146))
  call prop_A_Q(wf(:,146),Q(:,43),ZERO,0_intkind1,wf(:,147))
  call counter_A_Q(ctqq,wf(:,15),Q(:,40),wf(:,148))
  call prop_Q_A(wf(:,16),Q(:,23),ZERO,0_intkind1,wf(:,149))
  call prop_Q_A(wf(:,17),Q(:,23),ZERO,0_intkind1,wf(:,150))
  call prop_Q_A(wf(:,144),Q(:,20),ZERO,0_intkind1,wf(:,151))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,151),wf(:,152))
  call counter_A_Q(ctqq,wf(:,20),Q(:,11),wf(:,153))
  call counter_A_Q(ctqq,wf(:,22),Q(:,11),wf(:,154))
  call vert_AV_Q(wf(:,26),wf(:,11),wf(:,155))
  call counter_Q_A(ctqq,wf(:,25),Q(:,36),wf(:,156))
  call prop_A_Q(wf(:,155),Q(:,27),ZERO,0_intkind1,wf(:,157))
  call vert_AZ_Q(gZu,wf(:,26),wf(:,4),wf(:,158))
  call prop_A_Q(wf(:,158),Q(:,27),ZERO,0_intkind1,wf(:,159))
  call counter_A_Q(ctqq,wf(:,26),Q(:,24),wf(:,160))
  call prop_Q_A(wf(:,27),Q(:,39),ZERO,0_intkind1,wf(:,161))
  call prop_Q_A(wf(:,28),Q(:,39),ZERO,0_intkind1,wf(:,162))
  call counter_Q_A(ctqq,wf(:,31),Q(:,7),wf(:,163))
  call counter_Q_A(ctqq,wf(:,33),Q(:,7),wf(:,164))
  call prop_A_Q(wf(:,160),Q(:,24),ZERO,0_intkind1,wf(:,165))
  call vert_AZ_Q(gZu,wf(:,165),wf(:,-5),wf(:,166))
  call prop_Q_A(wf(:,156),Q(:,36),ZERO,0_intkind1,wf(:,167))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,167),wf(:,168))
  call prop_A_Q(wf(:,148),Q(:,40),ZERO,0_intkind1,wf(:,169))
  call vert_AZ_Q(gZu,wf(:,169),wf(:,-4),wf(:,170))
  call vert_QA_V(wf(:,167),wf(:,-3),wf(:,171))
  call vert_QA_Z(gZu,wf(:,167),wf(:,-3),wf(:,172))
  call vert_QA_V(wf(:,-2),wf(:,169),wf(:,173))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,169),wf(:,174))
  call vert_QA_V(wf(:,151),wf(:,-3),wf(:,175))
  call vert_QA_Z(gZu,wf(:,151),wf(:,-3),wf(:,176))
  call vert_QA_V(wf(:,-2),wf(:,165),wf(:,177))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,165),wf(:,178))
  call vert_SV_V(wf(:,3),wf(:,4),wf(:,179))
  call prop_W_W(wf(:,179),Q(:,51),MZ,1_intkind1,wf(:,180))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,31),wf(:,181))
  call prop_Q_A(wf(:,181),Q(:,23),ZERO,0_intkind1,wf(:,182))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,33),wf(:,183))
  call prop_Q_A(wf(:,183),Q(:,23),ZERO,0_intkind1,wf(:,184))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,31),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,39),ZERO,0_intkind1,wf(:,186))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,33),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,39),ZERO,0_intkind1,wf(:,188))
  call vert_AZ_Q(gZu,wf(:,20),wf(:,-4),wf(:,189))
  call prop_A_Q(wf(:,189),Q(:,27),ZERO,0_intkind1,wf(:,190))
  call vert_AZ_Q(gZu,wf(:,22),wf(:,-4),wf(:,191))
  call prop_A_Q(wf(:,191),Q(:,27),ZERO,0_intkind1,wf(:,192))
  call vert_AZ_Q(gZu,wf(:,20),wf(:,-5),wf(:,193))
  call prop_A_Q(wf(:,193),Q(:,43),ZERO,0_intkind1,wf(:,194))
  call vert_AZ_Q(gZu,wf(:,22),wf(:,-5),wf(:,195))
  call prop_A_Q(wf(:,195),Q(:,43),ZERO,0_intkind1,wf(:,196))
  call vert_SV_V(wf(:,7),wf(:,-5),wf(:,197))
  call prop_W_W(wf(:,197),Q(:,51),MZ,1_intkind1,wf(:,198))
  call vert_SV_V(wf(:,10),wf(:,-4),wf(:,199))
  call prop_W_W(wf(:,199),Q(:,51),MZ,1_intkind1,wf(:,200))
  call prop_W_W(wf(:,42),Q(:,51),MZ,1_intkind1,wf(:,201))
  call vert_VQ_A(wf(:,61),wf(:,-2),wf(:,202))
  call prop_Q_A(wf(:,202),Q(:,23),ZERO,0_intkind1,wf(:,203))
  call vert_ZQ_A(gZu,wf(:,65),wf(:,-2),wf(:,204))
  call prop_Q_A(wf(:,204),Q(:,23),ZERO,0_intkind1,wf(:,205))
  call vert_AV_Q(wf(:,-3),wf(:,61),wf(:,206))
  call prop_A_Q(wf(:,206),Q(:,27),ZERO,0_intkind1,wf(:,207))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,65),wf(:,208))
  call prop_A_Q(wf(:,208),Q(:,27),ZERO,0_intkind1,wf(:,209))
  call prop_Q_A(wf(:,44),Q(:,49),ZERO,0_intkind1,wf(:,210))
  call vert_QA_V(wf(:,210),wf(:,-1),wf(:,211))
  call vert_QA_Z(gZl,wf(:,210),wf(:,-1),wf(:,212))
  call prop_W_W(wf(:,212),Q(:,51),MZ,1_intkind1,wf(:,213))
  call prop_W_W(wf(:,53),Q(:,51),MZ,1_intkind1,wf(:,214))
  call vert_VQ_A(wf(:,68),wf(:,-2),wf(:,215))
  call prop_Q_A(wf(:,215),Q(:,23),ZERO,0_intkind1,wf(:,216))
  call vert_ZQ_A(gZu,wf(:,70),wf(:,-2),wf(:,217))
  call prop_Q_A(wf(:,217),Q(:,23),ZERO,0_intkind1,wf(:,218))
  call vert_AV_Q(wf(:,-3),wf(:,68),wf(:,219))
  call prop_A_Q(wf(:,219),Q(:,27),ZERO,0_intkind1,wf(:,220))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,70),wf(:,221))
  call prop_A_Q(wf(:,221),Q(:,27),ZERO,0_intkind1,wf(:,222))
  call prop_A_Q(wf(:,55),Q(:,50),ZERO,0_intkind1,wf(:,223))
  call vert_QA_V(wf(:,0),wf(:,223),wf(:,224))
  call vert_QA_Z(gZl,wf(:,0),wf(:,223),wf(:,225))
  call prop_W_W(wf(:,225),Q(:,51),MZ,1_intkind1,wf(:,226))
  call vert_VQ_A(wf(:,71),wf(:,-2),wf(:,227))
  call prop_Q_A(wf(:,227),Q(:,39),ZERO,0_intkind1,wf(:,228))
  call vert_ZQ_A(gZu,wf(:,75),wf(:,-2),wf(:,229))
  call prop_Q_A(wf(:,229),Q(:,39),ZERO,0_intkind1,wf(:,230))
  call vert_AV_Q(wf(:,-3),wf(:,71),wf(:,231))
  call prop_A_Q(wf(:,231),Q(:,43),ZERO,0_intkind1,wf(:,232))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,75),wf(:,233))
  call prop_A_Q(wf(:,233),Q(:,43),ZERO,0_intkind1,wf(:,234))
  call prop_Q_A(wf(:,59),Q(:,49),ZERO,0_intkind1,wf(:,235))
  call vert_QA_V(wf(:,235),wf(:,-1),wf(:,236))
  call vert_QA_Z(gZl,wf(:,235),wf(:,-1),wf(:,237))
  call prop_W_W(wf(:,237),Q(:,51),MZ,1_intkind1,wf(:,238))
  call vert_VQ_A(wf(:,76),wf(:,-2),wf(:,239))
  call prop_Q_A(wf(:,239),Q(:,39),ZERO,0_intkind1,wf(:,240))
  call vert_ZQ_A(gZu,wf(:,78),wf(:,-2),wf(:,241))
  call prop_Q_A(wf(:,241),Q(:,39),ZERO,0_intkind1,wf(:,242))
  call vert_AV_Q(wf(:,-3),wf(:,76),wf(:,243))
  call prop_A_Q(wf(:,243),Q(:,43),ZERO,0_intkind1,wf(:,244))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,78),wf(:,245))
  call prop_A_Q(wf(:,245),Q(:,43),ZERO,0_intkind1,wf(:,246))
  call prop_A_Q(wf(:,60),Q(:,50),ZERO,0_intkind1,wf(:,247))
  call vert_QA_V(wf(:,0),wf(:,247),wf(:,248))
  call vert_QA_Z(gZl,wf(:,0),wf(:,247),wf(:,249))
  call prop_W_W(wf(:,249),Q(:,51),MZ,1_intkind1,wf(:,250))

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
  den(2) = 1 / (Q(5,12) - MZ2)
  den(3) = 1 / (Q(5,48) - MH2)
  den(6) = 1 / (Q(5,19) - MH2)
  den(9) = 1 / (Q(5,28) - MH2)
  den(12) = 1 / (Q(5,3))
  den(13) = 1 / (Q(5,20))
  den(14) = 1 / (Q(5,40))
  den(19) = 1 / (Q(5,11))
  den(24) = 1 / (Q(5,36))
  den(25) = 1 / (Q(5,24))
  den(30) = 1 / (Q(5,7))
  den(39) = 1 / (Q(5,17))
  den(40) = 1 / (Q(5,34))
  den(41) = 1 / (Q(5,12))
  den(45) = 1 / (Q(5,14))
  den(50) = 1 / (Q(5,33))
  den(51) = 1 / (Q(5,18))
  den(55) = 1 / (Q(5,13))
  den(64) = 1 / (Q(5,19))
  den(67) = 1 / (Q(5,19) - MZ2)
  den(78) = 1 / (Q(5,35))
  den(81) = 1 / (Q(5,35) - MZ2)
  den(92) = 1 / (Q(5,52))
  den(99) = 1 / (Q(5,56))
  den(107) = 1 / (Q(5,43))
  den(113) = 1 / (Q(5,23))
  den(124) = 1 / (Q(5,27))
  den(130) = 1 / (Q(5,39))
  den(167) = 1 / (Q(5,51) - MZ2)
  den(178) = 1 / (Q(5,35) - MH2)
  den(181) = 1 / (Q(5,51))
  den(188) = 1 / (Q(5,49))
  den(198) = 1 / (Q(5,50))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(15) = den(12)*den(13)
  den(16) = den(14)*den(15)
  den(17) = den(1)*den(13)
  den(18) = den(14)*den(17)
  den(20) = den(12)*den(19)
  den(21) = den(13)*den(20)
  den(22) = den(1)*den(19)
  den(23) = den(13)*den(22)
  den(26) = den(12)*den(24)
  den(27) = den(25)*den(26)
  den(28) = den(1)*den(24)
  den(29) = den(25)*den(28)
  den(31) = den(12)*den(30)
  den(32) = den(25)*den(31)
  den(33) = den(1)*den(30)
  den(34) = den(25)*den(33)
  den(35) = den(20)*den(24)
  den(36) = den(22)*den(24)
  den(37) = den(14)*den(31)
  den(38) = den(14)*den(33)
  den(42) = den(39)*den(40)
  den(43) = den(41)*den(42)
  den(44) = den(2)*den(42)
  den(46) = den(41)*den(45)
  den(47) = den(39)*den(46)
  den(48) = den(2)*den(45)
  den(49) = den(39)*den(48)
  den(52) = den(50)*den(51)
  den(53) = den(41)*den(52)
  den(54) = den(2)*den(52)
  den(56) = den(41)*den(55)
  den(57) = den(51)*den(56)
  den(58) = den(2)*den(55)
  den(59) = den(51)*den(58)
  den(60) = den(46)*den(50)
  den(61) = den(48)*den(50)
  den(62) = den(40)*den(56)
  den(63) = den(40)*den(58)
  den(65) = den(39)*den(64)
  den(66) = den(24)*den(65)
  den(68) = den(39)*den(67)
  den(69) = den(24)*den(68)
  den(70) = den(14)*den(65)
  den(71) = den(14)*den(68)
  den(72) = den(51)*den(64)
  den(73) = den(24)*den(72)
  den(74) = den(51)*den(67)
  den(75) = den(24)*den(74)
  den(76) = den(14)*den(72)
  den(77) = den(14)*den(74)
  den(79) = den(50)*den(78)
  den(80) = den(13)*den(79)
  den(82) = den(50)*den(81)
  den(83) = den(13)*den(82)
  den(84) = den(40)*den(78)
  den(85) = den(13)*den(84)
  den(86) = den(40)*den(81)
  den(87) = den(13)*den(86)
  den(88) = den(25)*den(79)
  den(89) = den(25)*den(82)
  den(90) = den(25)*den(84)
  den(91) = den(25)*den(86)
  den(93) = den(13)*den(92)
  den(94) = den(12)*den(93)
  den(95) = den(1)*den(93)
  den(96) = den(24)*den(92)
  den(97) = den(12)*den(96)
  den(98) = den(1)*den(96)
  den(100) = den(25)*den(99)
  den(101) = den(12)*den(100)
  den(102) = den(1)*den(100)
  den(103) = den(14)*den(99)
  den(104) = den(12)*den(103)
  den(105) = den(1)*den(103)
  den(106) = den(12)*den(14)
  den(108) = den(106)*den(107)
  den(109) = den(13)*den(108)
  den(110) = den(1)*den(14)
  den(111) = den(107)*den(110)
  den(112) = den(13)*den(111)
  den(114) = den(15)*den(113)
  den(115) = den(14)*den(114)
  den(116) = den(17)*den(113)
  den(117) = den(14)*den(116)
  den(118) = den(13)**2
  den(119) = den(20)*den(118)
  den(120) = den(22)*den(118)
  den(121) = den(20)*den(93)
  den(122) = den(22)*den(93)
  den(123) = den(12)*den(25)
  den(125) = den(123)*den(124)
  den(126) = den(24)*den(125)
  den(127) = den(1)*den(25)
  den(128) = den(124)*den(127)
  den(129) = den(24)*den(128)
  den(131) = den(26)*den(130)
  den(132) = den(25)*den(131)
  den(133) = den(28)*den(130)
  den(134) = den(25)*den(133)
  den(135) = den(31)*den(100)
  den(136) = den(33)*den(100)
  den(137) = den(25)**2
  den(138) = den(31)*den(137)
  den(139) = den(33)*den(137)
  den(140) = den(24)**2
  den(141) = den(20)*den(140)
  den(142) = den(22)*den(140)
  den(143) = den(20)*den(96)
  den(144) = den(22)*den(96)
  den(145) = den(31)*den(103)
  den(146) = den(33)*den(103)
  den(147) = den(14)**2
  den(148) = den(31)*den(147)
  den(149) = den(33)*den(147)
  den(150) = den(65)*den(140)
  den(151) = den(68)*den(140)
  den(152) = den(65)*den(147)
  den(153) = den(68)*den(147)
  den(154) = den(72)*den(140)
  den(155) = den(74)*den(140)
  den(156) = den(72)*den(147)
  den(157) = den(74)*den(147)
  den(158) = den(79)*den(118)
  den(159) = den(82)*den(118)
  den(160) = den(84)*den(118)
  den(161) = den(86)*den(118)
  den(162) = den(79)*den(137)
  den(163) = den(82)*den(137)
  den(164) = den(84)*den(137)
  den(165) = den(86)*den(137)
  den(166) = den(1)*den(3)
  den(168) = den(166)*den(167)
  den(169) = den(31)*den(113)
  den(170) = den(33)*den(113)
  den(171) = den(31)*den(130)
  den(172) = den(33)*den(130)
  den(173) = den(20)*den(124)
  den(174) = den(22)*den(124)
  den(175) = den(20)*den(107)
  den(176) = den(22)*den(107)
  den(177) = den(7)*den(167)
  den(179) = den(1)*den(178)
  den(180) = den(167)*den(179)
  den(182) = den(42)*den(181)
  den(183) = den(42)*den(167)
  den(184) = den(65)*den(113)
  den(185) = den(68)*den(113)
  den(186) = den(65)*den(124)
  den(187) = den(68)*den(124)
  den(189) = den(39)*den(188)
  den(190) = den(181)*den(189)
  den(191) = den(167)*den(189)
  den(192) = den(52)*den(181)
  den(193) = den(52)*den(167)
  den(194) = den(72)*den(113)
  den(195) = den(74)*den(113)
  den(196) = den(72)*den(124)
  den(197) = den(74)*den(124)
  den(199) = den(51)*den(198)
  den(200) = den(181)*den(199)
  den(201) = den(167)*den(199)
  den(202) = den(79)*den(130)
  den(203) = den(82)*den(130)
  den(204) = den(79)*den(107)
  den(205) = den(82)*den(107)
  den(206) = den(50)*den(188)
  den(207) = den(181)*den(206)
  den(208) = den(167)*den(206)
  den(209) = den(84)*den(130)
  den(210) = den(86)*den(130)
  den(211) = den(84)*den(107)
  den(212) = den(86)*den(107)
  den(213) = den(40)*den(198)
  den(214) = den(181)*den(213)
  den(215) = den(167)*den(213)
  den(216) = den(12)*den(13)*den(14)
  den(217) = den(1)*den(13)*den(14)
  den(218) = den(12)*den(24)*den(25)
  den(219) = den(1)*den(24)*den(25)
  den(220) = den(13)*den(175)
  den(221) = den(13)*den(176)
  den(222) = den(25)*den(171)
  den(223) = den(25)*den(172)
  den(224) = den(24)*den(173)
  den(225) = den(24)*den(174)
  den(226) = den(14)*den(169)
  den(227) = den(14)*den(170)
  den(228) = den(24)*den(186)
  den(229) = den(24)*den(187)
  den(230) = den(14)*den(184)
  den(231) = den(14)*den(185)
  den(232) = den(24)*den(196)
  den(233) = den(24)*den(197)
  den(234) = den(14)*den(194)
  den(235) = den(14)*den(195)
  den(236) = den(13)*den(204)
  den(237) = den(13)*den(205)
  den(238) = den(13)*den(211)
  den(239) = den(13)*den(212)
  den(240) = den(25)*den(202)
  den(241) = den(25)*den(203)
  den(242) = den(25)*den(209)
  den(243) = den(25)*den(210)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(166)

  A(1) = cont_SS(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_SS(wf(:,7),wf(:,8)) * den(8)
  A(3) = cont_SS(wf(:,9),wf(:,10)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(5) = cont_QA(wf(:,15),wf(:,17)) * den(18)
  A(6) = cont_QA(wf(:,19),wf(:,20)) * den(21)
  A(7) = cont_QA(wf(:,19),wf(:,22)) * den(23)
  A(8) = cont_QA(wf(:,26),wf(:,27)) * den(27)
  A(9) = cont_QA(wf(:,26),wf(:,28)) * den(29)
  A(10) = cont_QA(wf(:,30),wf(:,31)) * den(32)
  A(11) = cont_QA(wf(:,30),wf(:,33)) * den(34)
  A(12) = cont_QA(wf(:,20),wf(:,34)) * den(35)
  A(13) = cont_QA(wf(:,22),wf(:,34)) * den(36)
  A(14) = cont_QA(wf(:,31),wf(:,35)) * den(37)
  A(15) = cont_QA(wf(:,33),wf(:,35)) * den(38)
  A(16) = cont_VV(wf(:,38),wf(:,41)) * den(43)
  A(17) = cont_VV(wf(:,5),wf(:,42)) * den(44)
  A(18) = cont_QA(wf(:,44),wf(:,45)) * den(47)
  A(19) = cont_QA(wf(:,44),wf(:,47)) * den(49)
  A(20) = cont_VV(wf(:,38),wf(:,52)) * den(53)
  A(21) = cont_VV(wf(:,5),wf(:,53)) * den(54)
  A(22) = cont_QA(wf(:,55),wf(:,56)) * den(57)
  A(23) = cont_QA(wf(:,55),wf(:,58)) * den(59)
  A(24) = cont_QA(wf(:,45),wf(:,59)) * den(60)
  A(25) = cont_QA(wf(:,47),wf(:,59)) * den(61)
  A(26) = cont_QA(wf(:,56),wf(:,60)) * den(62)
  A(27) = cont_QA(wf(:,58),wf(:,60)) * den(63)
  A(28) = cont_VV(wf(:,61),wf(:,62)) * den(66)
  A(29) = cont_VV(wf(:,64),wf(:,65)) * den(69)
  A(30) = cont_VV(wf(:,61),wf(:,66)) * den(70)
  A(31) = cont_VV(wf(:,65),wf(:,67)) * den(71)
  A(32) = cont_VV(wf(:,62),wf(:,68)) * den(73)
  A(33) = cont_VV(wf(:,64),wf(:,70)) * den(75)
  A(34) = cont_VV(wf(:,66),wf(:,68)) * den(76)
  A(35) = cont_VV(wf(:,67),wf(:,70)) * den(77)
  A(36) = cont_VV(wf(:,71),wf(:,72)) * den(80)
  A(37) = cont_VV(wf(:,74),wf(:,75)) * den(83)
  A(38) = cont_VV(wf(:,72),wf(:,76)) * den(85)
  A(39) = cont_VV(wf(:,74),wf(:,78)) * den(87)
  A(40) = cont_VV(wf(:,71),wf(:,79)) * den(88)
  A(41) = cont_VV(wf(:,75),wf(:,80)) * den(89)
  A(42) = cont_VV(wf(:,76),wf(:,79)) * den(90)
  A(43) = cont_VV(wf(:,78),wf(:,80)) * den(91)

  A(44) = cont_QA(wf(:,15),wf(:,81)) * den(16)
  A(45) = cont_QA(wf(:,15),wf(:,82)) * den(18)
  A(46) = cont_QA(wf(:,20),wf(:,83)) * den(21)
  A(47) = cont_QA(wf(:,22),wf(:,83)) * den(23)
  A(48) = cont_QA(wf(:,26),wf(:,84)) * den(27)
  A(49) = cont_QA(wf(:,26),wf(:,85)) * den(29)
  A(50) = cont_QA(wf(:,31),wf(:,86)) * den(32)
  A(51) = cont_QA(wf(:,33),wf(:,86)) * den(34)
  A(52) = cont_QA(wf(:,20),wf(:,87)) * den(35)
  A(53) = cont_QA(wf(:,22),wf(:,87)) * den(36)
  A(54) = cont_QA(wf(:,31),wf(:,88)) * den(37)
  A(55) = cont_QA(wf(:,33),wf(:,88)) * den(38)
  A(56) = cont_QA(wf(:,89),wf(:,90)) * den(94)
  A(57) = cont_QA(wf(:,90),wf(:,91)) * den(95)
  A(58) = cont_QA(wf(:,16),wf(:,93)) * den(16)
  A(59) = cont_QA(wf(:,17),wf(:,93)) * den(18)
  A(60) = cont_QA(wf(:,89),wf(:,94)) * den(97)
  A(61) = cont_QA(wf(:,91),wf(:,94)) * den(98)
  A(62) = cont_QA(wf(:,31),wf(:,95)) * den(37)
  A(63) = cont_QA(wf(:,33),wf(:,95)) * den(38)
  A(64) = cont_QA(wf(:,27),wf(:,97)) * den(27)
  A(65) = cont_QA(wf(:,28),wf(:,97)) * den(29)
  A(66) = cont_QA(wf(:,31),wf(:,98)) * den(32)
  A(67) = cont_QA(wf(:,33),wf(:,98)) * den(34)
  A(68) = cont_QA(wf(:,99),wf(:,100)) * den(101)
  A(69) = cont_QA(wf(:,100),wf(:,101)) * den(102)
  A(70) = cont_QA(wf(:,26),wf(:,104)) * den(27)
  A(71) = cont_QA(wf(:,26),wf(:,105)) * den(29)
  A(72) = cont_QA(wf(:,99),wf(:,106)) * den(104)
  A(73) = cont_QA(wf(:,101),wf(:,106)) * den(105)
  A(74) = cont_QA(wf(:,20),wf(:,107)) * den(35)
  A(75) = cont_QA(wf(:,22),wf(:,107)) * den(36)
  A(76) = cont_QA(wf(:,15),wf(:,110)) * den(16)
  A(77) = cont_QA(wf(:,15),wf(:,111)) * den(18)
  A(78) = cont_QA(wf(:,20),wf(:,112)) * den(21)
  A(79) = cont_QA(wf(:,22),wf(:,112)) * den(23)
  A(80) = cont_SS(wf(:,3),wf(:,115)) * den(5)
  A(81) = cont_SS(wf(:,7),wf(:,116)) * den(8)
  A(82) = cont_SS(wf(:,10),wf(:,117)) * den(11)
  A(83) = cont_VV(wf(:,61),wf(:,118)) * den(66)
  A(84) = cont_VV(wf(:,65),wf(:,119)) * den(69)
  A(85) = cont_VV(wf(:,61),wf(:,120)) * den(70)
  A(86) = cont_VV(wf(:,65),wf(:,121)) * den(71)
  A(87) = cont_VV(wf(:,68),wf(:,118)) * den(73)
  A(88) = cont_VV(wf(:,70),wf(:,119)) * den(75)
  A(89) = cont_VV(wf(:,68),wf(:,120)) * den(76)
  A(90) = cont_VV(wf(:,70),wf(:,121)) * den(77)
  A(91) = cont_VV(wf(:,71),wf(:,122)) * den(80)
  A(92) = cont_VV(wf(:,75),wf(:,123)) * den(83)
  A(93) = cont_VV(wf(:,76),wf(:,122)) * den(85)
  A(94) = cont_VV(wf(:,78),wf(:,123)) * den(87)
  A(95) = cont_VV(wf(:,71),wf(:,124)) * den(88)
  A(96) = cont_VV(wf(:,75),wf(:,125)) * den(89)
  A(97) = cont_VV(wf(:,76),wf(:,124)) * den(90)
  A(98) = cont_VV(wf(:,78),wf(:,125)) * den(91)
  A(99) = cont_VV(wf(:,61),wf(:,126)) * den(70)
  A(100) = cont_VV(wf(:,65),wf(:,127)) * den(71)
  A(101) = cont_VV(wf(:,61),wf(:,128)) * den(66)
  A(102) = cont_VV(wf(:,65),wf(:,129)) * den(69)
  A(103) = cont_VV(wf(:,68),wf(:,126)) * den(76)
  A(104) = cont_VV(wf(:,70),wf(:,127)) * den(77)
  A(105) = cont_VV(wf(:,68),wf(:,128)) * den(73)
  A(106) = cont_VV(wf(:,70),wf(:,129)) * den(75)
  A(107) = cont_VV(wf(:,71),wf(:,130)) * den(88)
  A(108) = cont_VV(wf(:,75),wf(:,131)) * den(89)
  A(109) = cont_VV(wf(:,76),wf(:,130)) * den(90)
  A(110) = cont_VV(wf(:,78),wf(:,131)) * den(91)
  A(111) = cont_VV(wf(:,71),wf(:,132)) * den(80)
  A(112) = cont_VV(wf(:,75),wf(:,133)) * den(83)
  A(113) = cont_VV(wf(:,76),wf(:,132)) * den(85)
  A(114) = cont_VV(wf(:,78),wf(:,133)) * den(87)
  A(115) = cont_VV(wf(:,41),wf(:,134)) * den(43)
  A(116) = cont_VV(wf(:,42),wf(:,114)) * den(44)
  A(117) = cont_QA(wf(:,44),wf(:,136)) * den(47)
  A(118) = cont_QA(wf(:,44),wf(:,138)) * den(49)
  A(119) = cont_VV(wf(:,52),wf(:,134)) * den(53)
  A(120) = cont_VV(wf(:,53),wf(:,114)) * den(54)
  A(121) = cont_QA(wf(:,55),wf(:,140)) * den(57)
  A(122) = cont_QA(wf(:,55),wf(:,142)) * den(59)
  A(123) = cont_QA(wf(:,59),wf(:,136)) * den(60)
  A(124) = cont_QA(wf(:,59),wf(:,138)) * den(61)
  A(125) = cont_QA(wf(:,60),wf(:,140)) * den(62)
  A(126) = cont_QA(wf(:,60),wf(:,142)) * den(63)
  A(127) = cont_QA(wf(:,144),wf(:,145)) * den(109)
  A(128) = cont_QA(wf(:,144),wf(:,147)) * den(112)
  A(129) = cont_QA(wf(:,148),wf(:,149)) * den(115)
  A(130) = cont_QA(wf(:,148),wf(:,150)) * den(117)
  A(131) = cont_QA(wf(:,20),wf(:,152)) * den(119)
  A(132) = cont_QA(wf(:,22),wf(:,152)) * den(120)
  A(133) = cont_QA(wf(:,90),wf(:,153)) * den(121)
  A(134) = cont_QA(wf(:,90),wf(:,154)) * den(122)
  A(135) = cont_QA(wf(:,156),wf(:,157)) * den(126)
  A(136) = cont_QA(wf(:,156),wf(:,159)) * den(129)
  A(137) = cont_QA(wf(:,160),wf(:,161)) * den(132)
  A(138) = cont_QA(wf(:,160),wf(:,162)) * den(134)
  A(139) = cont_QA(wf(:,100),wf(:,163)) * den(135)
  A(140) = cont_QA(wf(:,100),wf(:,164)) * den(136)
  A(141) = cont_QA(wf(:,31),wf(:,166)) * den(138)
  A(142) = cont_QA(wf(:,33),wf(:,166)) * den(139)
  A(143) = cont_QA(wf(:,20),wf(:,168)) * den(141)
  A(144) = cont_QA(wf(:,22),wf(:,168)) * den(142)
  A(145) = cont_QA(wf(:,94),wf(:,153)) * den(143)
  A(146) = cont_QA(wf(:,94),wf(:,154)) * den(144)
  A(147) = cont_QA(wf(:,106),wf(:,163)) * den(145)
  A(148) = cont_QA(wf(:,106),wf(:,164)) * den(146)
  A(149) = cont_QA(wf(:,31),wf(:,170)) * den(148)
  A(150) = cont_QA(wf(:,33),wf(:,170)) * den(149)
  A(151) = cont_VV(wf(:,61),wf(:,171)) * den(150)
  A(152) = cont_VV(wf(:,65),wf(:,172)) * den(151)
  A(153) = cont_VV(wf(:,61),wf(:,173)) * den(152)
  A(154) = cont_VV(wf(:,65),wf(:,174)) * den(153)
  A(155) = cont_VV(wf(:,68),wf(:,171)) * den(154)
  A(156) = cont_VV(wf(:,70),wf(:,172)) * den(155)
  A(157) = cont_VV(wf(:,68),wf(:,173)) * den(156)
  A(158) = cont_VV(wf(:,70),wf(:,174)) * den(157)
  A(159) = cont_VV(wf(:,71),wf(:,175)) * den(158)
  A(160) = cont_VV(wf(:,75),wf(:,176)) * den(159)
  A(161) = cont_VV(wf(:,76),wf(:,175)) * den(160)
  A(162) = cont_VV(wf(:,78),wf(:,176)) * den(161)
  A(163) = cont_VV(wf(:,71),wf(:,177)) * den(162)
  A(164) = cont_VV(wf(:,75),wf(:,178)) * den(163)
  A(165) = cont_VV(wf(:,76),wf(:,177)) * den(164)
  A(166) = cont_VV(wf(:,78),wf(:,178)) * den(165)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(166)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(4)-A(6)-A(8)-A(10)-A(12)-A(14)-A(16)-A(18)-A(20)-A(22)-A(24)-A(26)-A(28)-A(30)-A(32)-A(34)-A(36)-A(38)-A(40) &
       -A(42))*f(1)+(A(5)+A(7)+A(9)+A(11)+A(13)+A(15)+A(17)+A(19)+A(21)+A(23)+A(25)+A(27)+A(29)+A(31)+A(33)+A(35)+A(37)+A(39) &
       +A(41)+A(43))*f(2)+(-A(1)-A(2)-A(3))*f(7)

  M2(1) = (A(127)+A(129)+A(131)+A(133)+A(135)+A(137)+A(139)+A(141)+A(143)+A(145)+A(147)+A(149)+A(151)+A(153)+A(155)+A(157)+A(159) &
       +A(161)+A(163)+A(165))*f(3)+(-A(128)-A(130)-A(132)-A(134)-A(136)-A(138)-A(140)-A(142)-A(144)-A(146)-A(148)-A(150)-A(152) &
       -A(154)-A(156)-A(158)-A(160)-A(162)-A(164)-A(166))*f(4)+(-A(44)-A(46)-A(48)-A(50)-A(52)-A(54)-A(56)-A(58)-A(60)-A(62)-A(64) &
       -A(66)-A(68)-A(70)-A(72)-A(74)-A(76)-A(78)-A(83)-A(85)-A(87)-A(89)-A(91)-A(93)-A(95)-A(97)-A(99)-A(101)-A(103)-A(105) &
       -A(107)-A(109)-A(111)-A(113)-A(115)-A(117)-A(119)-A(121)-A(123)-A(125))*f(5)+(A(45)+A(47)+A(49)+A(51)+A(53)+A(55)+A(57) &
       +A(59)+A(61)+A(63)+A(65)+A(67)+A(69)+A(71)+A(73)+A(75)+A(77)+A(79)+A(84)+A(86)+A(88)+A(90)+A(92)+A(94)+A(96)+A(98)+A(100) &
       +A(102)+A(104)+A(106)+A(108)+A(110)+A(112)+A(114)+A(116)+A(118)+A(120)+A(122)+A(124)+A(126))*f(6)+(-A(80)-A(81) &
       -A(82))*f(8)

end subroutine colourvectors

end module ol_loop_eevvjj_eexuuxzz_1_/**/REALKIND
