
module ol_colourmatrix_ppllaa_eexbbxaa_1_/**/REALKIND
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
end module ol_colourmatrix_ppllaa_eexbbxaa_1_/**/REALKIND



module ol_forced_parameters_ppllaa_eexbbxaa_1_/**/REALKIND
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
  if (ME /= 0) write(*,101) 'ME = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllaa_eexbbxaa_1_/**/REALKIND

module ol_loop_ppllaa_eexbbxaa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(4)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:235)
  ! denominators
  complex(REALKIND), save :: den(228)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,64), Mct(1,64), Mcol_loop(1,64)
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
    f( 1) = (CI*eQED**4)/27._/**/REALKIND
    f( 2) = (CI*eQED**4)/9._/**/REALKIND
    f( 3) = (CI*eQED**4)/3._/**/REALKIND
    f( 4) = CI*eQED**4
    f( 5) = (CI*countertermnorm*eQED**4*gQCD**2)/27._/**/REALKIND
    f( 6) = (CI*countertermnorm*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 7) = (CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 8) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/27._/**/REALKIND
    f( 9) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/9._/**/REALKIND
    f(10) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/3._/**/REALKIND
    f(11) = CI*countertermnorm*ctVbb*eQED**4*gQCD**2
    f(12) = (eQED**4*gQCD**2*integralnorm*SwB)/27._/**/REALKIND
    f(13) = (eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(14) = (eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(15) = eQED**4*gQCD**2*integralnorm*SwB

  c = [ 4*f(12), 4*f(13), 4*f(14), 4*f(15) ]
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
  complex(REALKIND) :: A(160)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,3),MZ,1_intkind1,wf(:,8))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,11),MB,1_intkind1,wf(:,12))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,11),MB,1_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,36),MB,1_intkind1,wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,24),MB,1_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,1),wf(:,17),wf(:,19))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,17),wf(:,20))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,21))
  call vert_AV_Q(wf(:,18),wf(:,-5),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,7),MB,1_intkind1,wf(:,23))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,7),MB,1_intkind1,wf(:,25))
  call vert_VQ_A(wf(:,-4),wf(:,17),wf(:,26))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,27))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,28))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,29))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,30))
  call prop_Q_A(wf(:,28),Q(:,17),ZERO,0_intkind1,wf(:,31))
  call prop_A_Q(wf(:,29),Q(:,34),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,31),wf(:,32),wf(:,33))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,34))
  call prop_W_W(wf(:,34),Q(:,12),MZ,1_intkind1,wf(:,35))
  call vert_QA_Z(gZl,wf(:,31),wf(:,32),wf(:,36))
  call vert_AV_Q(wf(:,-1),wf(:,30),wf(:,37))
  call vert_VQ_A(wf(:,-5),wf(:,31),wf(:,38))
  call prop_A_Q(wf(:,37),Q(:,14),ZERO,0_intkind1,wf(:,39))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,35),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,14),ZERO,0_intkind1,wf(:,41))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,42))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,43))
  call prop_Q_A(wf(:,42),Q(:,33),ZERO,0_intkind1,wf(:,44))
  call prop_A_Q(wf(:,43),Q(:,18),ZERO,0_intkind1,wf(:,45))
  call vert_QA_V(wf(:,44),wf(:,45),wf(:,46))
  call vert_QA_Z(gZl,wf(:,44),wf(:,45),wf(:,47))
  call vert_VQ_A(wf(:,30),wf(:,0),wf(:,48))
  call vert_AV_Q(wf(:,45),wf(:,-5),wf(:,49))
  call prop_Q_A(wf(:,48),Q(:,13),ZERO,0_intkind1,wf(:,50))
  call vert_ZQ_A(gZl,wf(:,35),wf(:,0),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,13),ZERO,0_intkind1,wf(:,52))
  call vert_VQ_A(wf(:,-4),wf(:,44),wf(:,53))
  call vert_AV_Q(wf(:,32),wf(:,-4),wf(:,54))
  call vert_QA_V(wf(:,31),wf(:,-1),wf(:,55))
  call vert_QA_V(wf(:,17),wf(:,-3),wf(:,56))
  call vert_QA_Z(gZl,wf(:,31),wf(:,-1),wf(:,57))
  call vert_QA_Z(gZd,wf(:,17),wf(:,-3),wf(:,58))
  call prop_W_W(wf(:,57),Q(:,19),MZ,1_intkind1,wf(:,59))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,60))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,61))
  call vert_QA_V(wf(:,0),wf(:,45),wf(:,62))
  call vert_QA_Z(gZl,wf(:,0),wf(:,45),wf(:,63))
  call prop_W_W(wf(:,63),Q(:,19),MZ,1_intkind1,wf(:,64))
  call vert_QA_V(wf(:,44),wf(:,-1),wf(:,65))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,66))
  call vert_QA_Z(gZl,wf(:,44),wf(:,-1),wf(:,67))
  call vert_QA_Z(gZd,wf(:,4),wf(:,-3),wf(:,68))
  call prop_W_W(wf(:,67),Q(:,35),MZ,1_intkind1,wf(:,69))
  call vert_QA_V(wf(:,0),wf(:,32),wf(:,70))
  call vert_QA_Z(gZl,wf(:,0),wf(:,32),wf(:,71))
  call prop_W_W(wf(:,71),Q(:,35),MZ,1_intkind1,wf(:,72))
  call vert_QA_V(wf(:,-2),wf(:,18),wf(:,73))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,18),wf(:,74))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,75))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,76))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,77))
  call counter_VQ_A(wf(:,1),wf(:,17),wf(:,78))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,17),wf(:,79))
  call counter_AV_Q(wf(:,18),wf(:,-5),wf(:,80))
  call counter_VQ_A(wf(:,-4),wf(:,17),wf(:,81))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,82))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,83))
  call prop_Q_A(wf(:,11),Q(:,52),MB,1_intkind1,wf(:,84))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,85))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,40),MB,1_intkind1,wf(:,87))
  call prop_Q_A(wf(:,26),Q(:,52),MB,1_intkind1,wf(:,88))
  call vert_AV_Q(wf(:,87),wf(:,-4),wf(:,89))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,90))
  call prop_A_Q(wf(:,90),Q(:,24),MB,1_intkind1,wf(:,91))
  call vert_AV_Q(wf(:,91),wf(:,-5),wf(:,92))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,93))
  call prop_A_Q(wf(:,22),Q(:,56),MB,1_intkind1,wf(:,94))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,95))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,96))
  call prop_Q_A(wf(:,96),Q(:,36),MB,1_intkind1,wf(:,97))
  call vert_VQ_A(wf(:,1),wf(:,97),wf(:,98))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,97),wf(:,99))
  call prop_A_Q(wf(:,27),Q(:,56),MB,1_intkind1,wf(:,100))
  call vert_VQ_A(wf(:,-4),wf(:,97),wf(:,101))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,102))
  call prop_Q_A(wf(:,102),Q(:,20),MB,1_intkind1,wf(:,103))
  call vert_VQ_A(wf(:,1),wf(:,103),wf(:,104))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,103),wf(:,105))
  call vert_VQ_A(wf(:,-5),wf(:,103),wf(:,106))
  call counter_QA_V(wf(:,17),wf(:,-3),wf(:,107))
  call counter_QA_Z(gZd,wf(:,17),wf(:,-3),wf(:,108))
  call vert_QA_V(wf(:,-2),wf(:,87),wf(:,109))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,87),wf(:,110))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,111))
  call counter_QA_Z(gZd,wf(:,4),wf(:,-3),wf(:,112))
  call vert_QA_V(wf(:,-2),wf(:,91),wf(:,113))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,91),wf(:,114))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,115))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,116))
  call vert_QA_V(wf(:,97),wf(:,-3),wf(:,117))
  call vert_QA_Z(gZd,wf(:,97),wf(:,-3),wf(:,118))
  call counter_QA_V(wf(:,-2),wf(:,18),wf(:,119))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,18),wf(:,120))
  call vert_QA_V(wf(:,103),wf(:,-3),wf(:,121))
  call vert_QA_Z(gZd,wf(:,103),wf(:,-3),wf(:,122))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,123))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,124))
  call prop_W_W(wf(:,124),Q(:,12),MZ,1_intkind1,wf(:,125))
  call vert_AV_Q(wf(:,-1),wf(:,123),wf(:,126))
  call prop_A_Q(wf(:,126),Q(:,14),ZERO,0_intkind1,wf(:,127))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,125),wf(:,128))
  call prop_A_Q(wf(:,128),Q(:,14),ZERO,0_intkind1,wf(:,129))
  call vert_VQ_A(wf(:,123),wf(:,0),wf(:,130))
  call prop_Q_A(wf(:,130),Q(:,13),ZERO,0_intkind1,wf(:,131))
  call vert_ZQ_A(gZl,wf(:,125),wf(:,0),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,13),ZERO,0_intkind1,wf(:,133))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,134))
  call counter_Q_A(ctbb,wf(:,4),Q(:,20),wf(:,135))
  call prop_A_Q(wf(:,134),Q(:,43),MB,1_intkind1,wf(:,136))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,8),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,43),MB,1_intkind1,wf(:,138))
  call counter_A_Q(ctbb,wf(:,5),Q(:,40),wf(:,139))
  call prop_Q_A(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,140))
  call prop_Q_A(wf(:,9),Q(:,23),MB,1_intkind1,wf(:,141))
  call prop_Q_A(wf(:,135),Q(:,20),MB,1_intkind1,wf(:,142))
  call vert_VQ_A(wf(:,-5),wf(:,142),wf(:,143))
  call counter_A_Q(ctbb,wf(:,12),Q(:,11),wf(:,144))
  call counter_A_Q(ctbb,wf(:,14),Q(:,11),wf(:,145))
  call vert_AV_Q(wf(:,18),wf(:,1),wf(:,146))
  call counter_Q_A(ctbb,wf(:,17),Q(:,36),wf(:,147))
  call prop_A_Q(wf(:,146),Q(:,27),MB,1_intkind1,wf(:,148))
  call vert_AZ_Q(gZd,wf(:,18),wf(:,8),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,27),MB,1_intkind1,wf(:,150))
  call counter_A_Q(ctbb,wf(:,18),Q(:,24),wf(:,151))
  call prop_Q_A(wf(:,19),Q(:,39),MB,1_intkind1,wf(:,152))
  call prop_Q_A(wf(:,20),Q(:,39),MB,1_intkind1,wf(:,153))
  call counter_Q_A(ctbb,wf(:,23),Q(:,7),wf(:,154))
  call counter_Q_A(ctbb,wf(:,25),Q(:,7),wf(:,155))
  call prop_A_Q(wf(:,151),Q(:,24),MB,1_intkind1,wf(:,156))
  call vert_AV_Q(wf(:,156),wf(:,-5),wf(:,157))
  call prop_Q_A(wf(:,147),Q(:,36),MB,1_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,-4),wf(:,158),wf(:,159))
  call prop_A_Q(wf(:,139),Q(:,40),MB,1_intkind1,wf(:,160))
  call vert_AV_Q(wf(:,160),wf(:,-4),wf(:,161))
  call vert_QA_V(wf(:,158),wf(:,-3),wf(:,162))
  call vert_QA_Z(gZd,wf(:,158),wf(:,-3),wf(:,163))
  call vert_QA_V(wf(:,-2),wf(:,160),wf(:,164))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,160),wf(:,165))
  call vert_QA_V(wf(:,142),wf(:,-3),wf(:,166))
  call vert_QA_Z(gZd,wf(:,142),wf(:,-3),wf(:,167))
  call vert_QA_V(wf(:,-2),wf(:,156),wf(:,168))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,156),wf(:,169))
  call vert_VQ_A(wf(:,-4),wf(:,23),wf(:,170))
  call prop_Q_A(wf(:,170),Q(:,23),MB,1_intkind1,wf(:,171))
  call vert_VQ_A(wf(:,-4),wf(:,25),wf(:,172))
  call prop_Q_A(wf(:,172),Q(:,23),MB,1_intkind1,wf(:,173))
  call vert_VQ_A(wf(:,-5),wf(:,23),wf(:,174))
  call prop_Q_A(wf(:,174),Q(:,39),MB,1_intkind1,wf(:,175))
  call vert_VQ_A(wf(:,-5),wf(:,25),wf(:,176))
  call prop_Q_A(wf(:,176),Q(:,39),MB,1_intkind1,wf(:,177))
  call vert_AV_Q(wf(:,12),wf(:,-4),wf(:,178))
  call prop_A_Q(wf(:,178),Q(:,27),MB,1_intkind1,wf(:,179))
  call vert_AV_Q(wf(:,14),wf(:,-4),wf(:,180))
  call prop_A_Q(wf(:,180),Q(:,27),MB,1_intkind1,wf(:,181))
  call vert_AV_Q(wf(:,12),wf(:,-5),wf(:,182))
  call prop_A_Q(wf(:,182),Q(:,43),MB,1_intkind1,wf(:,183))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,184))
  call prop_A_Q(wf(:,184),Q(:,43),MB,1_intkind1,wf(:,185))
  call prop_W_W(wf(:,36),Q(:,51),MZ,1_intkind1,wf(:,186))
  call vert_VQ_A(wf(:,55),wf(:,-2),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,23),MB,1_intkind1,wf(:,188))
  call vert_ZQ_A(gZd,wf(:,59),wf(:,-2),wf(:,189))
  call prop_Q_A(wf(:,189),Q(:,23),MB,1_intkind1,wf(:,190))
  call vert_AV_Q(wf(:,-3),wf(:,55),wf(:,191))
  call prop_A_Q(wf(:,191),Q(:,27),MB,1_intkind1,wf(:,192))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,59),wf(:,193))
  call prop_A_Q(wf(:,193),Q(:,27),MB,1_intkind1,wf(:,194))
  call prop_Q_A(wf(:,38),Q(:,49),ZERO,0_intkind1,wf(:,195))
  call vert_QA_V(wf(:,195),wf(:,-1),wf(:,196))
  call vert_QA_Z(gZl,wf(:,195),wf(:,-1),wf(:,197))
  call prop_W_W(wf(:,197),Q(:,51),MZ,1_intkind1,wf(:,198))
  call prop_W_W(wf(:,47),Q(:,51),MZ,1_intkind1,wf(:,199))
  call vert_VQ_A(wf(:,62),wf(:,-2),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,23),MB,1_intkind1,wf(:,201))
  call vert_ZQ_A(gZd,wf(:,64),wf(:,-2),wf(:,202))
  call prop_Q_A(wf(:,202),Q(:,23),MB,1_intkind1,wf(:,203))
  call vert_AV_Q(wf(:,-3),wf(:,62),wf(:,204))
  call prop_A_Q(wf(:,204),Q(:,27),MB,1_intkind1,wf(:,205))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,64),wf(:,206))
  call prop_A_Q(wf(:,206),Q(:,27),MB,1_intkind1,wf(:,207))
  call prop_A_Q(wf(:,49),Q(:,50),ZERO,0_intkind1,wf(:,208))
  call vert_QA_V(wf(:,0),wf(:,208),wf(:,209))
  call vert_QA_Z(gZl,wf(:,0),wf(:,208),wf(:,210))
  call prop_W_W(wf(:,210),Q(:,51),MZ,1_intkind1,wf(:,211))
  call vert_VQ_A(wf(:,65),wf(:,-2),wf(:,212))
  call prop_Q_A(wf(:,212),Q(:,39),MB,1_intkind1,wf(:,213))
  call vert_ZQ_A(gZd,wf(:,69),wf(:,-2),wf(:,214))
  call prop_Q_A(wf(:,214),Q(:,39),MB,1_intkind1,wf(:,215))
  call vert_AV_Q(wf(:,-3),wf(:,65),wf(:,216))
  call prop_A_Q(wf(:,216),Q(:,43),MB,1_intkind1,wf(:,217))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,69),wf(:,218))
  call prop_A_Q(wf(:,218),Q(:,43),MB,1_intkind1,wf(:,219))
  call prop_Q_A(wf(:,53),Q(:,49),ZERO,0_intkind1,wf(:,220))
  call vert_QA_V(wf(:,220),wf(:,-1),wf(:,221))
  call vert_QA_Z(gZl,wf(:,220),wf(:,-1),wf(:,222))
  call prop_W_W(wf(:,222),Q(:,51),MZ,1_intkind1,wf(:,223))
  call vert_VQ_A(wf(:,70),wf(:,-2),wf(:,224))
  call prop_Q_A(wf(:,224),Q(:,39),MB,1_intkind1,wf(:,225))
  call vert_ZQ_A(gZd,wf(:,72),wf(:,-2),wf(:,226))
  call prop_Q_A(wf(:,226),Q(:,39),MB,1_intkind1,wf(:,227))
  call vert_AV_Q(wf(:,-3),wf(:,70),wf(:,228))
  call prop_A_Q(wf(:,228),Q(:,43),MB,1_intkind1,wf(:,229))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,72),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,43),MB,1_intkind1,wf(:,231))
  call prop_A_Q(wf(:,54),Q(:,50),ZERO,0_intkind1,wf(:,232))
  call vert_QA_V(wf(:,0),wf(:,232),wf(:,233))
  call vert_QA_Z(gZl,wf(:,0),wf(:,232),wf(:,234))
  call prop_W_W(wf(:,234),Q(:,51),MZ,1_intkind1,wf(:,235))

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
  den(2) = 1 / (Q(5,20) - MB2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,11) - MB2)
  den(14) = 1 / (Q(5,36) - MB2)
  den(15) = 1 / (Q(5,24) - MB2)
  den(20) = 1 / (Q(5,7) - MB2)
  den(29) = 1 / (Q(5,17))
  den(30) = 1 / (Q(5,34))
  den(31) = 1 / (Q(5,12))
  den(34) = 1 / (Q(5,12) - MZ2)
  den(36) = 1 / (Q(5,14))
  den(41) = 1 / (Q(5,33))
  den(42) = 1 / (Q(5,18))
  den(46) = 1 / (Q(5,13))
  den(55) = 1 / (Q(5,19))
  den(58) = 1 / (Q(5,19) - MZ2)
  den(69) = 1 / (Q(5,35))
  den(72) = 1 / (Q(5,35) - MZ2)
  den(83) = 1 / (Q(5,52) - MB2)
  den(90) = 1 / (Q(5,56) - MB2)
  den(98) = 1 / (Q(5,43) - MB2)
  den(104) = 1 / (Q(5,23) - MB2)
  den(115) = 1 / (Q(5,27) - MB2)
  den(121) = 1 / (Q(5,39) - MB2)
  den(165) = 1 / (Q(5,51))
  den(167) = 1 / (Q(5,51) - MZ2)
  den(173) = 1 / (Q(5,49))
  den(183) = 1 / (Q(5,50))

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
  den(32) = den(29)*den(30)
  den(33) = den(31)*den(32)
  den(35) = den(32)*den(34)
  den(37) = den(31)*den(36)
  den(38) = den(29)*den(37)
  den(39) = den(34)*den(36)
  den(40) = den(29)*den(39)
  den(43) = den(41)*den(42)
  den(44) = den(31)*den(43)
  den(45) = den(34)*den(43)
  den(47) = den(31)*den(46)
  den(48) = den(42)*den(47)
  den(49) = den(34)*den(46)
  den(50) = den(42)*den(49)
  den(51) = den(37)*den(41)
  den(52) = den(39)*den(41)
  den(53) = den(30)*den(47)
  den(54) = den(30)*den(49)
  den(56) = den(29)*den(55)
  den(57) = den(14)*den(56)
  den(59) = den(29)*den(58)
  den(60) = den(14)*den(59)
  den(61) = den(3)*den(56)
  den(62) = den(3)*den(59)
  den(63) = den(42)*den(55)
  den(64) = den(14)*den(63)
  den(65) = den(42)*den(58)
  den(66) = den(14)*den(65)
  den(67) = den(3)*den(63)
  den(68) = den(3)*den(65)
  den(70) = den(41)*den(69)
  den(71) = den(2)*den(70)
  den(73) = den(41)*den(72)
  den(74) = den(2)*den(73)
  den(75) = den(30)*den(69)
  den(76) = den(2)*den(75)
  den(77) = den(30)*den(72)
  den(78) = den(2)*den(77)
  den(79) = den(15)*den(70)
  den(80) = den(15)*den(73)
  den(81) = den(15)*den(75)
  den(82) = den(15)*den(77)
  den(84) = den(2)*den(83)
  den(85) = den(1)*den(84)
  den(86) = den(6)*den(84)
  den(87) = den(14)*den(83)
  den(88) = den(1)*den(87)
  den(89) = den(6)*den(87)
  den(91) = den(15)*den(90)
  den(92) = den(1)*den(91)
  den(93) = den(6)*den(91)
  den(94) = den(3)*den(90)
  den(95) = den(1)*den(94)
  den(96) = den(6)*den(94)
  den(97) = den(1)*den(3)
  den(99) = den(97)*den(98)
  den(100) = den(2)*den(99)
  den(101) = den(3)*den(6)
  den(102) = den(98)*den(101)
  den(103) = den(2)*den(102)
  den(105) = den(4)*den(104)
  den(106) = den(3)*den(105)
  den(107) = den(7)*den(104)
  den(108) = den(3)*den(107)
  den(109) = den(2)**2
  den(110) = den(10)*den(109)
  den(111) = den(12)*den(109)
  den(112) = den(10)*den(84)
  den(113) = den(12)*den(84)
  den(114) = den(1)*den(15)
  den(116) = den(114)*den(115)
  den(117) = den(14)*den(116)
  den(118) = den(6)*den(15)
  den(119) = den(115)*den(118)
  den(120) = den(14)*den(119)
  den(122) = den(16)*den(121)
  den(123) = den(15)*den(122)
  den(124) = den(18)*den(121)
  den(125) = den(15)*den(124)
  den(126) = den(21)*den(91)
  den(127) = den(23)*den(91)
  den(128) = den(15)**2
  den(129) = den(21)*den(128)
  den(130) = den(23)*den(128)
  den(131) = den(14)**2
  den(132) = den(10)*den(131)
  den(133) = den(12)*den(131)
  den(134) = den(10)*den(87)
  den(135) = den(12)*den(87)
  den(136) = den(21)*den(94)
  den(137) = den(23)*den(94)
  den(138) = den(3)**2
  den(139) = den(21)*den(138)
  den(140) = den(23)*den(138)
  den(141) = den(56)*den(131)
  den(142) = den(59)*den(131)
  den(143) = den(56)*den(138)
  den(144) = den(59)*den(138)
  den(145) = den(63)*den(131)
  den(146) = den(65)*den(131)
  den(147) = den(63)*den(138)
  den(148) = den(65)*den(138)
  den(149) = den(70)*den(109)
  den(150) = den(73)*den(109)
  den(151) = den(75)*den(109)
  den(152) = den(77)*den(109)
  den(153) = den(70)*den(128)
  den(154) = den(73)*den(128)
  den(155) = den(75)*den(128)
  den(156) = den(77)*den(128)
  den(157) = den(21)*den(104)
  den(158) = den(23)*den(104)
  den(159) = den(21)*den(121)
  den(160) = den(23)*den(121)
  den(161) = den(10)*den(115)
  den(162) = den(12)*den(115)
  den(163) = den(10)*den(98)
  den(164) = den(12)*den(98)
  den(166) = den(32)*den(165)
  den(168) = den(32)*den(167)
  den(169) = den(56)*den(104)
  den(170) = den(59)*den(104)
  den(171) = den(56)*den(115)
  den(172) = den(59)*den(115)
  den(174) = den(29)*den(173)
  den(175) = den(165)*den(174)
  den(176) = den(167)*den(174)
  den(177) = den(43)*den(165)
  den(178) = den(43)*den(167)
  den(179) = den(63)*den(104)
  den(180) = den(65)*den(104)
  den(181) = den(63)*den(115)
  den(182) = den(65)*den(115)
  den(184) = den(42)*den(183)
  den(185) = den(165)*den(184)
  den(186) = den(167)*den(184)
  den(187) = den(70)*den(121)
  den(188) = den(73)*den(121)
  den(189) = den(70)*den(98)
  den(190) = den(73)*den(98)
  den(191) = den(41)*den(173)
  den(192) = den(165)*den(191)
  den(193) = den(167)*den(191)
  den(194) = den(75)*den(121)
  den(195) = den(77)*den(121)
  den(196) = den(75)*den(98)
  den(197) = den(77)*den(98)
  den(198) = den(30)*den(183)
  den(199) = den(165)*den(198)
  den(200) = den(167)*den(198)
  den(201) = den(1)*den(2)*den(3)
  den(202) = den(2)*den(3)*den(6)
  den(203) = den(1)*den(14)*den(15)
  den(204) = den(6)*den(14)*den(15)
  den(205) = den(2)*den(163)
  den(206) = den(2)*den(164)
  den(207) = den(15)*den(159)
  den(208) = den(15)*den(160)
  den(209) = den(14)*den(161)
  den(210) = den(14)*den(162)
  den(211) = den(3)*den(157)
  den(212) = den(3)*den(158)
  den(213) = den(14)*den(171)
  den(214) = den(14)*den(172)
  den(215) = den(3)*den(169)
  den(216) = den(3)*den(170)
  den(217) = den(14)*den(181)
  den(218) = den(14)*den(182)
  den(219) = den(3)*den(179)
  den(220) = den(3)*den(180)
  den(221) = den(2)*den(189)
  den(222) = den(2)*den(190)
  den(223) = den(2)*den(196)
  den(224) = den(2)*den(197)
  den(225) = den(15)*den(187)
  den(226) = den(15)*den(188)
  den(227) = den(15)*den(194)
  den(228) = den(15)*den(195)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(160)

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
  A(13) = cont_VV(wf(:,30),wf(:,33)) * den(33)
  A(14) = cont_VV(wf(:,35),wf(:,36)) * den(35)
  A(15) = cont_QA(wf(:,38),wf(:,39)) * den(38)
  A(16) = cont_QA(wf(:,38),wf(:,41)) * den(40)
  A(17) = cont_VV(wf(:,30),wf(:,46)) * den(44)
  A(18) = cont_VV(wf(:,35),wf(:,47)) * den(45)
  A(19) = cont_QA(wf(:,49),wf(:,50)) * den(48)
  A(20) = cont_QA(wf(:,49),wf(:,52)) * den(50)
  A(21) = cont_QA(wf(:,39),wf(:,53)) * den(51)
  A(22) = cont_QA(wf(:,41),wf(:,53)) * den(52)
  A(23) = cont_QA(wf(:,50),wf(:,54)) * den(53)
  A(24) = cont_QA(wf(:,52),wf(:,54)) * den(54)
  A(25) = cont_VV(wf(:,55),wf(:,56)) * den(57)
  A(26) = cont_VV(wf(:,58),wf(:,59)) * den(60)
  A(27) = cont_VV(wf(:,55),wf(:,60)) * den(61)
  A(28) = cont_VV(wf(:,59),wf(:,61)) * den(62)
  A(29) = cont_VV(wf(:,56),wf(:,62)) * den(64)
  A(30) = cont_VV(wf(:,58),wf(:,64)) * den(66)
  A(31) = cont_VV(wf(:,60),wf(:,62)) * den(67)
  A(32) = cont_VV(wf(:,61),wf(:,64)) * den(68)
  A(33) = cont_VV(wf(:,65),wf(:,66)) * den(71)
  A(34) = cont_VV(wf(:,68),wf(:,69)) * den(74)
  A(35) = cont_VV(wf(:,66),wf(:,70)) * den(76)
  A(36) = cont_VV(wf(:,68),wf(:,72)) * den(78)
  A(37) = cont_VV(wf(:,65),wf(:,73)) * den(79)
  A(38) = cont_VV(wf(:,69),wf(:,74)) * den(80)
  A(39) = cont_VV(wf(:,70),wf(:,73)) * den(81)
  A(40) = cont_VV(wf(:,72),wf(:,74)) * den(82)

  A(41) = cont_QA(wf(:,5),wf(:,75)) * den(5)
  A(42) = cont_QA(wf(:,5),wf(:,76)) * den(8)
  A(43) = cont_QA(wf(:,12),wf(:,77)) * den(11)
  A(44) = cont_QA(wf(:,14),wf(:,77)) * den(13)
  A(45) = cont_QA(wf(:,18),wf(:,78)) * den(17)
  A(46) = cont_QA(wf(:,18),wf(:,79)) * den(19)
  A(47) = cont_QA(wf(:,23),wf(:,80)) * den(22)
  A(48) = cont_QA(wf(:,25),wf(:,80)) * den(24)
  A(49) = cont_QA(wf(:,12),wf(:,81)) * den(25)
  A(50) = cont_QA(wf(:,14),wf(:,81)) * den(26)
  A(51) = cont_QA(wf(:,23),wf(:,82)) * den(27)
  A(52) = cont_QA(wf(:,25),wf(:,82)) * den(28)
  A(53) = cont_QA(wf(:,83),wf(:,84)) * den(85)
  A(54) = cont_QA(wf(:,84),wf(:,85)) * den(86)
  A(55) = cont_QA(wf(:,6),wf(:,87)) * den(5)
  A(56) = cont_QA(wf(:,9),wf(:,87)) * den(8)
  A(57) = cont_QA(wf(:,83),wf(:,88)) * den(88)
  A(58) = cont_QA(wf(:,85),wf(:,88)) * den(89)
  A(59) = cont_QA(wf(:,23),wf(:,89)) * den(27)
  A(60) = cont_QA(wf(:,25),wf(:,89)) * den(28)
  A(61) = cont_QA(wf(:,19),wf(:,91)) * den(17)
  A(62) = cont_QA(wf(:,20),wf(:,91)) * den(19)
  A(63) = cont_QA(wf(:,23),wf(:,92)) * den(22)
  A(64) = cont_QA(wf(:,25),wf(:,92)) * den(24)
  A(65) = cont_QA(wf(:,93),wf(:,94)) * den(92)
  A(66) = cont_QA(wf(:,94),wf(:,95)) * den(93)
  A(67) = cont_QA(wf(:,18),wf(:,98)) * den(17)
  A(68) = cont_QA(wf(:,18),wf(:,99)) * den(19)
  A(69) = cont_QA(wf(:,93),wf(:,100)) * den(95)
  A(70) = cont_QA(wf(:,95),wf(:,100)) * den(96)
  A(71) = cont_QA(wf(:,12),wf(:,101)) * den(25)
  A(72) = cont_QA(wf(:,14),wf(:,101)) * den(26)
  A(73) = cont_QA(wf(:,5),wf(:,104)) * den(5)
  A(74) = cont_QA(wf(:,5),wf(:,105)) * den(8)
  A(75) = cont_QA(wf(:,12),wf(:,106)) * den(11)
  A(76) = cont_QA(wf(:,14),wf(:,106)) * den(13)
  A(77) = cont_VV(wf(:,55),wf(:,107)) * den(57)
  A(78) = cont_VV(wf(:,59),wf(:,108)) * den(60)
  A(79) = cont_VV(wf(:,55),wf(:,109)) * den(61)
  A(80) = cont_VV(wf(:,59),wf(:,110)) * den(62)
  A(81) = cont_VV(wf(:,62),wf(:,107)) * den(64)
  A(82) = cont_VV(wf(:,64),wf(:,108)) * den(66)
  A(83) = cont_VV(wf(:,62),wf(:,109)) * den(67)
  A(84) = cont_VV(wf(:,64),wf(:,110)) * den(68)
  A(85) = cont_VV(wf(:,65),wf(:,111)) * den(71)
  A(86) = cont_VV(wf(:,69),wf(:,112)) * den(74)
  A(87) = cont_VV(wf(:,70),wf(:,111)) * den(76)
  A(88) = cont_VV(wf(:,72),wf(:,112)) * den(78)
  A(89) = cont_VV(wf(:,65),wf(:,113)) * den(79)
  A(90) = cont_VV(wf(:,69),wf(:,114)) * den(80)
  A(91) = cont_VV(wf(:,70),wf(:,113)) * den(81)
  A(92) = cont_VV(wf(:,72),wf(:,114)) * den(82)
  A(93) = cont_VV(wf(:,55),wf(:,115)) * den(61)
  A(94) = cont_VV(wf(:,59),wf(:,116)) * den(62)
  A(95) = cont_VV(wf(:,55),wf(:,117)) * den(57)
  A(96) = cont_VV(wf(:,59),wf(:,118)) * den(60)
  A(97) = cont_VV(wf(:,62),wf(:,115)) * den(67)
  A(98) = cont_VV(wf(:,64),wf(:,116)) * den(68)
  A(99) = cont_VV(wf(:,62),wf(:,117)) * den(64)
  A(100) = cont_VV(wf(:,64),wf(:,118)) * den(66)
  A(101) = cont_VV(wf(:,65),wf(:,119)) * den(79)
  A(102) = cont_VV(wf(:,69),wf(:,120)) * den(80)
  A(103) = cont_VV(wf(:,70),wf(:,119)) * den(81)
  A(104) = cont_VV(wf(:,72),wf(:,120)) * den(82)
  A(105) = cont_VV(wf(:,65),wf(:,121)) * den(71)
  A(106) = cont_VV(wf(:,69),wf(:,122)) * den(74)
  A(107) = cont_VV(wf(:,70),wf(:,121)) * den(76)
  A(108) = cont_VV(wf(:,72),wf(:,122)) * den(78)
  A(109) = cont_VV(wf(:,33),wf(:,123)) * den(33)
  A(110) = cont_VV(wf(:,36),wf(:,125)) * den(35)
  A(111) = cont_QA(wf(:,38),wf(:,127)) * den(38)
  A(112) = cont_QA(wf(:,38),wf(:,129)) * den(40)
  A(113) = cont_VV(wf(:,46),wf(:,123)) * den(44)
  A(114) = cont_VV(wf(:,47),wf(:,125)) * den(45)
  A(115) = cont_QA(wf(:,49),wf(:,131)) * den(48)
  A(116) = cont_QA(wf(:,49),wf(:,133)) * den(50)
  A(117) = cont_QA(wf(:,53),wf(:,127)) * den(51)
  A(118) = cont_QA(wf(:,53),wf(:,129)) * den(52)
  A(119) = cont_QA(wf(:,54),wf(:,131)) * den(53)
  A(120) = cont_QA(wf(:,54),wf(:,133)) * den(54)
  A(121) = cont_QA(wf(:,135),wf(:,136)) * den(100)
  A(122) = cont_QA(wf(:,135),wf(:,138)) * den(103)
  A(123) = cont_QA(wf(:,139),wf(:,140)) * den(106)
  A(124) = cont_QA(wf(:,139),wf(:,141)) * den(108)
  A(125) = cont_QA(wf(:,12),wf(:,143)) * den(110)
  A(126) = cont_QA(wf(:,14),wf(:,143)) * den(111)
  A(127) = cont_QA(wf(:,84),wf(:,144)) * den(112)
  A(128) = cont_QA(wf(:,84),wf(:,145)) * den(113)
  A(129) = cont_QA(wf(:,147),wf(:,148)) * den(117)
  A(130) = cont_QA(wf(:,147),wf(:,150)) * den(120)
  A(131) = cont_QA(wf(:,151),wf(:,152)) * den(123)
  A(132) = cont_QA(wf(:,151),wf(:,153)) * den(125)
  A(133) = cont_QA(wf(:,94),wf(:,154)) * den(126)
  A(134) = cont_QA(wf(:,94),wf(:,155)) * den(127)
  A(135) = cont_QA(wf(:,23),wf(:,157)) * den(129)
  A(136) = cont_QA(wf(:,25),wf(:,157)) * den(130)
  A(137) = cont_QA(wf(:,12),wf(:,159)) * den(132)
  A(138) = cont_QA(wf(:,14),wf(:,159)) * den(133)
  A(139) = cont_QA(wf(:,88),wf(:,144)) * den(134)
  A(140) = cont_QA(wf(:,88),wf(:,145)) * den(135)
  A(141) = cont_QA(wf(:,100),wf(:,154)) * den(136)
  A(142) = cont_QA(wf(:,100),wf(:,155)) * den(137)
  A(143) = cont_QA(wf(:,23),wf(:,161)) * den(139)
  A(144) = cont_QA(wf(:,25),wf(:,161)) * den(140)
  A(145) = cont_VV(wf(:,55),wf(:,162)) * den(141)
  A(146) = cont_VV(wf(:,59),wf(:,163)) * den(142)
  A(147) = cont_VV(wf(:,55),wf(:,164)) * den(143)
  A(148) = cont_VV(wf(:,59),wf(:,165)) * den(144)
  A(149) = cont_VV(wf(:,62),wf(:,162)) * den(145)
  A(150) = cont_VV(wf(:,64),wf(:,163)) * den(146)
  A(151) = cont_VV(wf(:,62),wf(:,164)) * den(147)
  A(152) = cont_VV(wf(:,64),wf(:,165)) * den(148)
  A(153) = cont_VV(wf(:,65),wf(:,166)) * den(149)
  A(154) = cont_VV(wf(:,69),wf(:,167)) * den(150)
  A(155) = cont_VV(wf(:,70),wf(:,166)) * den(151)
  A(156) = cont_VV(wf(:,72),wf(:,167)) * den(152)
  A(157) = cont_VV(wf(:,65),wf(:,168)) * den(153)
  A(158) = cont_VV(wf(:,69),wf(:,169)) * den(154)
  A(159) = cont_VV(wf(:,70),wf(:,168)) * den(155)
  A(160) = cont_VV(wf(:,72),wf(:,169)) * den(156)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(160)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(3)+A(5)+A(7)+A(9)+A(11))*f(1)+(A(2)+A(4)+A(6)+A(8)+A(10)+A(12)+A(25)+A(27)+A(29)+A(31)+A(33)+A(35)+A(37) &
       +A(39))*f(2)+(A(13)+A(15)+A(17)+A(19)+A(21)+A(23)+A(26)+A(28)+A(30)+A(32)+A(34)+A(36)+A(38)+A(40))*f(3)+(A(14)+A(16)+A(18) &
       +A(20)+A(22)+A(24))*f(4)

  M2(1) = (-A(121)-A(123)-A(125)-A(127)-A(129)-A(131)-A(133)-A(135)-A(137)-A(139)-A(141)-A(143))*f(5)+(-A(122)-A(124)-A(126) &
       -A(128)-A(130)-A(132)-A(134)-A(136)-A(138)-A(140)-A(142)-A(144)-A(145)-A(147)-A(149)-A(151)-A(153)-A(155)-A(157) &
       -A(159))*f(6)+(-A(146)-A(148)-A(150)-A(152)-A(154)-A(156)-A(158)-A(160))*f(7)+(A(41)+A(43)+A(45)+A(47)+A(49)+A(51)+A(53) &
       +A(55)+A(57)+A(59)+A(61)+A(63)+A(65)+A(67)+A(69)+A(71)+A(73)+A(75))*f(8)+(A(42)+A(44)+A(46)+A(48)+A(50)+A(52)+A(54)+A(56) &
       +A(58)+A(60)+A(62)+A(64)+A(66)+A(68)+A(70)+A(72)+A(74)+A(76)+A(77)+A(79)+A(81)+A(83)+A(85)+A(87)+A(89)+A(91)+A(93)+A(95) &
       +A(97)+A(99)+A(101)+A(103)+A(105)+A(107))*f(9)+(A(78)+A(80)+A(82)+A(84)+A(86)+A(88)+A(90)+A(92)+A(94)+A(96)+A(98)+A(100) &
       +A(102)+A(104)+A(106)+A(108)+A(109)+A(111)+A(113)+A(115)+A(117)+A(119))*f(10)+(A(110)+A(112)+A(114)+A(116)+A(118) &
       +A(120))*f(11)

end subroutine colourvectors

end module ol_loop_ppllaa_eexbbxaa_1_/**/REALKIND
