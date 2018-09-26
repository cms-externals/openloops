
module ol_colourmatrix_eevvjj_eexbbxaz_1_/**/REALKIND
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
end module ol_colourmatrix_eevvjj_eexbbxaz_1_/**/REALKIND



module ol_forced_parameters_eevvjj_eexbbxaz_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_eevvjj_eexbbxaz_1_/**/REALKIND

module ol_loop_eevvjj_eexbbxaz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(20), c(5)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:254)
  ! denominators
  complex(REALKIND), save :: den(249)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,96)
  ! zero helicity identifier
  logical,           save :: zerohel(96) = .true., zerohel_ct(96) = .true.

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
    f( 1) = (CI*eQED**4)/9._/**/REALKIND
    f( 2) = (CI*eQED**4)/3._/**/REALKIND
    f( 3) = CI*eQED**4
    f( 4) = (CI*countertermnorm*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*eQED**4*gQCD**2
    f( 7) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 8) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctVbb*eQED**4*gQCD**2
    f(10) = (CI*eQED**4*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(11) = (CI*eQED**4*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(12) = (CI*countertermnorm*eQED**4*gQCD**2*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(13) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(14) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(15) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(16) = (eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(17) = (eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(18) = eQED**4*gQCD**2*integralnorm*SwB
    f(19) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(cw**2*sw**2*6._/**/REALKIND)
    f(20) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 4*f(16), 4*f(17), 4*f(18), 4*f(19), 4*f(20) ]
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
  complex(REALKIND) :: A(172)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rMZ, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rMZ, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,3),MZ,1_intkind1,wf(:,8))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,11),MB,1_intkind1,wf(:,12))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,11),MB,1_intkind1,wf(:,14))
  call vert_AQ_S(gH,wf(:,-3),wf(:,4),wf(:,15))
  call vert_VV_S(wf(:,8),wf(:,-5),wf(:,16))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,17))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,36),MB,1_intkind1,wf(:,19))
  call prop_A_Q(wf(:,18),Q(:,24),MB,1_intkind1,wf(:,20))
  call vert_VQ_A(wf(:,1),wf(:,19),wf(:,21))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,19),wf(:,22))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,23))
  call vert_AZ_Q(gZd,wf(:,20),wf(:,-5),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,7),MB,1_intkind1,wf(:,25))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,7),MB,1_intkind1,wf(:,27))
  call vert_AQ_S(gH,wf(:,20),wf(:,-2),wf(:,28))
  call vert_VQ_A(wf(:,-4),wf(:,19),wf(:,29))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,30))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,31))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,-5),wf(:,32))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,33))
  call prop_Q_A(wf(:,31),Q(:,17),ZERO,0_intkind1,wf(:,34))
  call prop_A_Q(wf(:,32),Q(:,34),ZERO,0_intkind1,wf(:,35))
  call vert_QA_V(wf(:,34),wf(:,35),wf(:,36))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,37))
  call prop_W_W(wf(:,37),Q(:,12),MZ,1_intkind1,wf(:,38))
  call vert_QA_Z(gZl,wf(:,34),wf(:,35),wf(:,39))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,40))
  call vert_QA_Z(gZl,wf(:,34),wf(:,-1),wf(:,41))
  call vert_SV_V(wf(:,40),wf(:,-5),wf(:,42))
  call prop_W_W(wf(:,41),Q(:,19),MZ,1_intkind1,wf(:,43))
  call vert_AV_Q(wf(:,-1),wf(:,33),wf(:,44))
  call vert_ZQ_A(gZl,wf(:,-5),wf(:,34),wf(:,45))
  call prop_A_Q(wf(:,44),Q(:,14),ZERO,0_intkind1,wf(:,46))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,38),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,14),ZERO,0_intkind1,wf(:,48))
  call vert_ZQ_A(gZl,wf(:,-5),wf(:,0),wf(:,49))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,50))
  call prop_Q_A(wf(:,49),Q(:,33),ZERO,0_intkind1,wf(:,51))
  call prop_A_Q(wf(:,50),Q(:,18),ZERO,0_intkind1,wf(:,52))
  call vert_QA_V(wf(:,51),wf(:,52),wf(:,53))
  call vert_QA_Z(gZl,wf(:,51),wf(:,52),wf(:,54))
  call vert_QA_Z(gZl,wf(:,0),wf(:,52),wf(:,55))
  call prop_W_W(wf(:,55),Q(:,19),MZ,1_intkind1,wf(:,56))
  call vert_VQ_A(wf(:,33),wf(:,0),wf(:,57))
  call vert_AZ_Q(gZl,wf(:,52),wf(:,-5),wf(:,58))
  call prop_Q_A(wf(:,57),Q(:,13),ZERO,0_intkind1,wf(:,59))
  call vert_ZQ_A(gZl,wf(:,38),wf(:,0),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,13),ZERO,0_intkind1,wf(:,61))
  call vert_VQ_A(wf(:,-4),wf(:,51),wf(:,62))
  call vert_AV_Q(wf(:,35),wf(:,-4),wf(:,63))
  call vert_QA_V(wf(:,34),wf(:,-1),wf(:,64))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,65))
  call vert_QA_Z(gZd,wf(:,19),wf(:,-3),wf(:,66))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,67))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,68))
  call vert_QA_V(wf(:,0),wf(:,52),wf(:,69))
  call vert_QA_V(wf(:,51),wf(:,-1),wf(:,70))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,71))
  call vert_QA_Z(gZl,wf(:,51),wf(:,-1),wf(:,72))
  call vert_QA_Z(gZd,wf(:,4),wf(:,-3),wf(:,73))
  call prop_W_W(wf(:,72),Q(:,35),MZ,1_intkind1,wf(:,74))
  call vert_QA_V(wf(:,0),wf(:,35),wf(:,75))
  call vert_QA_Z(gZl,wf(:,0),wf(:,35),wf(:,76))
  call prop_W_W(wf(:,76),Q(:,35),MZ,1_intkind1,wf(:,77))
  call vert_QA_V(wf(:,-2),wf(:,20),wf(:,78))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,20),wf(:,79))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,80))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,81))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,82))
  call counter_VQ_A(wf(:,1),wf(:,19),wf(:,83))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,19),wf(:,84))
  call counter_AZ_Q(gZd,wf(:,20),wf(:,-5),wf(:,85))
  call counter_VQ_A(wf(:,-4),wf(:,19),wf(:,86))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,87))
  call counter_AQ_S(gH,wf(:,-3),wf(:,4),wf(:,88))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,89))
  call prop_Q_A(wf(:,11),Q(:,52),MB,1_intkind1,wf(:,90))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,91))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,40),MB,1_intkind1,wf(:,93))
  call prop_Q_A(wf(:,29),Q(:,52),MB,1_intkind1,wf(:,94))
  call vert_AV_Q(wf(:,93),wf(:,-4),wf(:,95))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,96))
  call prop_A_Q(wf(:,96),Q(:,24),MB,1_intkind1,wf(:,97))
  call vert_AZ_Q(gZd,wf(:,97),wf(:,-5),wf(:,98))
  call vert_AQ_S(gH,wf(:,97),wf(:,-2),wf(:,99))
  call counter_AQ_S(gH,wf(:,20),wf(:,-2),wf(:,100))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,101))
  call prop_A_Q(wf(:,24),Q(:,56),MB,1_intkind1,wf(:,102))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,103))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,104))
  call prop_Q_A(wf(:,104),Q(:,36),MB,1_intkind1,wf(:,105))
  call vert_VQ_A(wf(:,1),wf(:,105),wf(:,106))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,105),wf(:,107))
  call prop_A_Q(wf(:,30),Q(:,56),MB,1_intkind1,wf(:,108))
  call vert_VQ_A(wf(:,-4),wf(:,105),wf(:,109))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,20),MB,1_intkind1,wf(:,111))
  call vert_VQ_A(wf(:,1),wf(:,111),wf(:,112))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,111),wf(:,113))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,111),wf(:,114))
  call vert_AQ_S(gH,wf(:,-3),wf(:,111),wf(:,115))
  call counter_QA_V(wf(:,19),wf(:,-3),wf(:,116))
  call counter_QA_Z(gZd,wf(:,19),wf(:,-3),wf(:,117))
  call vert_QA_V(wf(:,-2),wf(:,93),wf(:,118))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,93),wf(:,119))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,120))
  call counter_QA_Z(gZd,wf(:,4),wf(:,-3),wf(:,121))
  call vert_QA_V(wf(:,-2),wf(:,97),wf(:,122))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,97),wf(:,123))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,124))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,125))
  call vert_QA_V(wf(:,105),wf(:,-3),wf(:,126))
  call vert_QA_Z(gZd,wf(:,105),wf(:,-3),wf(:,127))
  call counter_QA_V(wf(:,-2),wf(:,20),wf(:,128))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,20),wf(:,129))
  call vert_QA_V(wf(:,111),wf(:,-3),wf(:,130))
  call vert_QA_Z(gZd,wf(:,111),wf(:,-3),wf(:,131))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,132))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,133))
  call prop_W_W(wf(:,133),Q(:,12),MZ,1_intkind1,wf(:,134))
  call counter_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,135))
  call vert_SV_V(wf(:,135),wf(:,-5),wf(:,136))
  call vert_AV_Q(wf(:,-1),wf(:,132),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,14),ZERO,0_intkind1,wf(:,138))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,134),wf(:,139))
  call prop_A_Q(wf(:,139),Q(:,14),ZERO,0_intkind1,wf(:,140))
  call vert_VQ_A(wf(:,132),wf(:,0),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,13),ZERO,0_intkind1,wf(:,142))
  call vert_ZQ_A(gZl,wf(:,134),wf(:,0),wf(:,143))
  call prop_Q_A(wf(:,143),Q(:,13),ZERO,0_intkind1,wf(:,144))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,145))
  call counter_Q_A(ctbb,wf(:,4),Q(:,20),wf(:,146))
  call prop_A_Q(wf(:,145),Q(:,43),MB,1_intkind1,wf(:,147))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,8),wf(:,148))
  call prop_A_Q(wf(:,148),Q(:,43),MB,1_intkind1,wf(:,149))
  call counter_A_Q(ctbb,wf(:,5),Q(:,40),wf(:,150))
  call prop_Q_A(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,151))
  call prop_Q_A(wf(:,9),Q(:,23),MB,1_intkind1,wf(:,152))
  call prop_Q_A(wf(:,146),Q(:,20),MB,1_intkind1,wf(:,153))
  call vert_AQ_S(gH,wf(:,-3),wf(:,153),wf(:,154))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,153),wf(:,155))
  call counter_A_Q(ctbb,wf(:,12),Q(:,11),wf(:,156))
  call counter_A_Q(ctbb,wf(:,14),Q(:,11),wf(:,157))
  call vert_AV_Q(wf(:,20),wf(:,1),wf(:,158))
  call counter_Q_A(ctbb,wf(:,19),Q(:,36),wf(:,159))
  call prop_A_Q(wf(:,158),Q(:,27),MB,1_intkind1,wf(:,160))
  call vert_AZ_Q(gZd,wf(:,20),wf(:,8),wf(:,161))
  call prop_A_Q(wf(:,161),Q(:,27),MB,1_intkind1,wf(:,162))
  call counter_A_Q(ctbb,wf(:,20),Q(:,24),wf(:,163))
  call prop_Q_A(wf(:,21),Q(:,39),MB,1_intkind1,wf(:,164))
  call prop_Q_A(wf(:,22),Q(:,39),MB,1_intkind1,wf(:,165))
  call prop_A_Q(wf(:,163),Q(:,24),MB,1_intkind1,wf(:,166))
  call vert_AQ_S(gH,wf(:,166),wf(:,-2),wf(:,167))
  call counter_Q_A(ctbb,wf(:,25),Q(:,7),wf(:,168))
  call counter_Q_A(ctbb,wf(:,27),Q(:,7),wf(:,169))
  call vert_AZ_Q(gZd,wf(:,166),wf(:,-5),wf(:,170))
  call prop_Q_A(wf(:,159),Q(:,36),MB,1_intkind1,wf(:,171))
  call vert_VQ_A(wf(:,-4),wf(:,171),wf(:,172))
  call prop_A_Q(wf(:,150),Q(:,40),MB,1_intkind1,wf(:,173))
  call vert_AV_Q(wf(:,173),wf(:,-4),wf(:,174))
  call vert_QA_V(wf(:,171),wf(:,-3),wf(:,175))
  call vert_QA_Z(gZd,wf(:,171),wf(:,-3),wf(:,176))
  call vert_QA_V(wf(:,-2),wf(:,173),wf(:,177))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,173),wf(:,178))
  call vert_QA_V(wf(:,153),wf(:,-3),wf(:,179))
  call vert_QA_Z(gZd,wf(:,153),wf(:,-3),wf(:,180))
  call vert_QA_V(wf(:,-2),wf(:,166),wf(:,181))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,166),wf(:,182))
  call vert_VQ_A(wf(:,-4),wf(:,25),wf(:,183))
  call prop_Q_A(wf(:,183),Q(:,23),MB,1_intkind1,wf(:,184))
  call vert_VQ_A(wf(:,-4),wf(:,27),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,23),MB,1_intkind1,wf(:,186))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,25),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,39),MB,1_intkind1,wf(:,188))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,27),wf(:,189))
  call prop_Q_A(wf(:,189),Q(:,39),MB,1_intkind1,wf(:,190))
  call vert_AV_Q(wf(:,12),wf(:,-4),wf(:,191))
  call prop_A_Q(wf(:,191),Q(:,27),MB,1_intkind1,wf(:,192))
  call vert_AV_Q(wf(:,14),wf(:,-4),wf(:,193))
  call prop_A_Q(wf(:,193),Q(:,27),MB,1_intkind1,wf(:,194))
  call vert_AZ_Q(gZd,wf(:,12),wf(:,-5),wf(:,195))
  call prop_A_Q(wf(:,195),Q(:,43),MB,1_intkind1,wf(:,196))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,-5),wf(:,197))
  call prop_A_Q(wf(:,197),Q(:,43),MB,1_intkind1,wf(:,198))
  call vert_QS_A(gH,wf(:,-2),wf(:,16),wf(:,199))
  call prop_Q_A(wf(:,199),Q(:,39),MB,1_intkind1,wf(:,200))
  call vert_SA_Q(gH,wf(:,16),wf(:,-3),wf(:,201))
  call prop_A_Q(wf(:,201),Q(:,43),MB,1_intkind1,wf(:,202))
  call prop_W_W(wf(:,39),Q(:,51),MZ,1_intkind1,wf(:,203))
  call vert_VQ_A(wf(:,64),wf(:,-2),wf(:,204))
  call prop_Q_A(wf(:,204),Q(:,23),MB,1_intkind1,wf(:,205))
  call vert_ZQ_A(gZd,wf(:,43),wf(:,-2),wf(:,206))
  call prop_Q_A(wf(:,206),Q(:,23),MB,1_intkind1,wf(:,207))
  call vert_AV_Q(wf(:,-3),wf(:,64),wf(:,208))
  call prop_A_Q(wf(:,208),Q(:,27),MB,1_intkind1,wf(:,209))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,43),wf(:,210))
  call prop_A_Q(wf(:,210),Q(:,27),MB,1_intkind1,wf(:,211))
  call vert_VV_S(wf(:,43),wf(:,-5),wf(:,212))
  call prop_Q_A(wf(:,45),Q(:,49),ZERO,0_intkind1,wf(:,213))
  call vert_QA_V(wf(:,213),wf(:,-1),wf(:,214))
  call vert_QA_Z(gZl,wf(:,213),wf(:,-1),wf(:,215))
  call prop_W_W(wf(:,215),Q(:,51),MZ,1_intkind1,wf(:,216))
  call prop_W_W(wf(:,54),Q(:,51),MZ,1_intkind1,wf(:,217))
  call vert_VQ_A(wf(:,69),wf(:,-2),wf(:,218))
  call prop_Q_A(wf(:,218),Q(:,23),MB,1_intkind1,wf(:,219))
  call vert_ZQ_A(gZd,wf(:,56),wf(:,-2),wf(:,220))
  call prop_Q_A(wf(:,220),Q(:,23),MB,1_intkind1,wf(:,221))
  call vert_AV_Q(wf(:,-3),wf(:,69),wf(:,222))
  call prop_A_Q(wf(:,222),Q(:,27),MB,1_intkind1,wf(:,223))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,56),wf(:,224))
  call prop_A_Q(wf(:,224),Q(:,27),MB,1_intkind1,wf(:,225))
  call vert_VV_S(wf(:,56),wf(:,-5),wf(:,226))
  call prop_A_Q(wf(:,58),Q(:,50),ZERO,0_intkind1,wf(:,227))
  call vert_QA_V(wf(:,0),wf(:,227),wf(:,228))
  call vert_QA_Z(gZl,wf(:,0),wf(:,227),wf(:,229))
  call prop_W_W(wf(:,229),Q(:,51),MZ,1_intkind1,wf(:,230))
  call vert_VQ_A(wf(:,70),wf(:,-2),wf(:,231))
  call prop_Q_A(wf(:,231),Q(:,39),MB,1_intkind1,wf(:,232))
  call vert_ZQ_A(gZd,wf(:,74),wf(:,-2),wf(:,233))
  call prop_Q_A(wf(:,233),Q(:,39),MB,1_intkind1,wf(:,234))
  call vert_AV_Q(wf(:,-3),wf(:,70),wf(:,235))
  call prop_A_Q(wf(:,235),Q(:,43),MB,1_intkind1,wf(:,236))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,74),wf(:,237))
  call prop_A_Q(wf(:,237),Q(:,43),MB,1_intkind1,wf(:,238))
  call prop_Q_A(wf(:,62),Q(:,49),ZERO,0_intkind1,wf(:,239))
  call vert_QA_V(wf(:,239),wf(:,-1),wf(:,240))
  call vert_QA_Z(gZl,wf(:,239),wf(:,-1),wf(:,241))
  call prop_W_W(wf(:,241),Q(:,51),MZ,1_intkind1,wf(:,242))
  call vert_VQ_A(wf(:,75),wf(:,-2),wf(:,243))
  call prop_Q_A(wf(:,243),Q(:,39),MB,1_intkind1,wf(:,244))
  call vert_ZQ_A(gZd,wf(:,77),wf(:,-2),wf(:,245))
  call prop_Q_A(wf(:,245),Q(:,39),MB,1_intkind1,wf(:,246))
  call vert_AV_Q(wf(:,-3),wf(:,75),wf(:,247))
  call prop_A_Q(wf(:,247),Q(:,43),MB,1_intkind1,wf(:,248))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,77),wf(:,249))
  call prop_A_Q(wf(:,249),Q(:,43),MB,1_intkind1,wf(:,250))
  call prop_A_Q(wf(:,63),Q(:,50),ZERO,0_intkind1,wf(:,251))
  call vert_QA_V(wf(:,0),wf(:,251),wf(:,252))
  call vert_QA_Z(gZl,wf(:,0),wf(:,251),wf(:,253))
  call prop_W_W(wf(:,253),Q(:,51),MZ,1_intkind1,wf(:,254))

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
  den(14) = 1 / (Q(5,28) - MH2)
  den(17) = 1 / (Q(5,36) - MB2)
  den(18) = 1 / (Q(5,24) - MB2)
  den(23) = 1 / (Q(5,7) - MB2)
  den(34) = 1 / (Q(5,17))
  den(35) = 1 / (Q(5,34))
  den(36) = 1 / (Q(5,12))
  den(39) = 1 / (Q(5,12) - MZ2)
  den(41) = 1 / (Q(5,12) - MH2)
  den(42) = 1 / (Q(5,19) - MZ2)
  den(45) = 1 / (Q(5,14))
  den(50) = 1 / (Q(5,33))
  den(51) = 1 / (Q(5,18))
  den(57) = 1 / (Q(5,13))
  den(66) = 1 / (Q(5,19))
  den(77) = 1 / (Q(5,35))
  den(80) = 1 / (Q(5,35) - MZ2)
  den(91) = 1 / (Q(5,35) - MH2)
  den(94) = 1 / (Q(5,52) - MB2)
  den(102) = 1 / (Q(5,56) - MB2)
  den(110) = 1 / (Q(5,43) - MB2)
  den(116) = 1 / (Q(5,23) - MB2)
  den(128) = 1 / (Q(5,27) - MB2)
  den(134) = 1 / (Q(5,39) - MB2)
  den(181) = 1 / (Q(5,51))
  den(183) = 1 / (Q(5,51) - MZ2)
  den(189) = 1 / (Q(5,51) - MH2)
  den(191) = 1 / (Q(5,49))
  den(202) = 1 / (Q(5,50))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(2)*den(12)
  den(15) = den(2)*den(14)
  den(16) = den(6)*den(15)
  den(19) = den(1)*den(17)
  den(20) = den(18)*den(19)
  den(21) = den(6)*den(17)
  den(22) = den(18)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(18)*den(24)
  den(26) = den(6)*den(23)
  den(27) = den(18)*den(26)
  den(28) = den(14)*den(18)
  den(29) = den(6)*den(28)
  den(30) = den(10)*den(17)
  den(31) = den(12)*den(17)
  den(32) = den(3)*den(24)
  den(33) = den(3)*den(26)
  den(37) = den(34)*den(35)
  den(38) = den(36)*den(37)
  den(40) = den(37)*den(39)
  den(43) = den(34)*den(42)
  den(44) = den(41)*den(43)
  den(46) = den(36)*den(45)
  den(47) = den(34)*den(46)
  den(48) = den(39)*den(45)
  den(49) = den(34)*den(48)
  den(52) = den(50)*den(51)
  den(53) = den(36)*den(52)
  den(54) = den(39)*den(52)
  den(55) = den(42)*den(51)
  den(56) = den(41)*den(55)
  den(58) = den(36)*den(57)
  den(59) = den(51)*den(58)
  den(60) = den(39)*den(57)
  den(61) = den(51)*den(60)
  den(62) = den(46)*den(50)
  den(63) = den(48)*den(50)
  den(64) = den(35)*den(58)
  den(65) = den(35)*den(60)
  den(67) = den(34)*den(66)
  den(68) = den(17)*den(67)
  den(69) = den(17)*den(43)
  den(70) = den(3)*den(67)
  den(71) = den(3)*den(43)
  den(72) = den(51)*den(66)
  den(73) = den(17)*den(72)
  den(74) = den(17)*den(55)
  den(75) = den(3)*den(72)
  den(76) = den(3)*den(55)
  den(78) = den(50)*den(77)
  den(79) = den(2)*den(78)
  den(81) = den(50)*den(80)
  den(82) = den(2)*den(81)
  den(83) = den(35)*den(77)
  den(84) = den(2)*den(83)
  den(85) = den(35)*den(80)
  den(86) = den(2)*den(85)
  den(87) = den(18)*den(78)
  den(88) = den(18)*den(81)
  den(89) = den(18)*den(83)
  den(90) = den(18)*den(85)
  den(92) = den(6)*den(91)
  den(93) = den(2)*den(92)
  den(95) = den(2)*den(94)
  den(96) = den(1)*den(95)
  den(97) = den(6)*den(95)
  den(98) = den(17)*den(94)
  den(99) = den(1)*den(98)
  den(100) = den(6)*den(98)
  den(101) = den(18)*den(92)
  den(103) = den(18)*den(102)
  den(104) = den(1)*den(103)
  den(105) = den(6)*den(103)
  den(106) = den(3)*den(102)
  den(107) = den(1)*den(106)
  den(108) = den(6)*den(106)
  den(109) = den(1)*den(3)
  den(111) = den(109)*den(110)
  den(112) = den(2)*den(111)
  den(113) = den(3)*den(6)
  den(114) = den(110)*den(113)
  den(115) = den(2)*den(114)
  den(117) = den(4)*den(116)
  den(118) = den(3)*den(117)
  den(119) = den(7)*den(116)
  den(120) = den(3)*den(119)
  den(121) = den(2)**2
  den(122) = den(92)*den(121)
  den(123) = den(10)*den(121)
  den(124) = den(12)*den(121)
  den(125) = den(10)*den(95)
  den(126) = den(12)*den(95)
  den(127) = den(1)*den(18)
  den(129) = den(127)*den(128)
  den(130) = den(17)*den(129)
  den(131) = den(6)*den(18)
  den(132) = den(128)*den(131)
  den(133) = den(17)*den(132)
  den(135) = den(19)*den(134)
  den(136) = den(18)*den(135)
  den(137) = den(21)*den(134)
  den(138) = den(18)*den(137)
  den(139) = den(18)**2
  den(140) = den(92)*den(139)
  den(141) = den(24)*den(103)
  den(142) = den(26)*den(103)
  den(143) = den(24)*den(139)
  den(144) = den(26)*den(139)
  den(145) = den(17)**2
  den(146) = den(10)*den(145)
  den(147) = den(12)*den(145)
  den(148) = den(10)*den(98)
  den(149) = den(12)*den(98)
  den(150) = den(24)*den(106)
  den(151) = den(26)*den(106)
  den(152) = den(3)**2
  den(153) = den(24)*den(152)
  den(154) = den(26)*den(152)
  den(155) = den(67)*den(145)
  den(156) = den(43)*den(145)
  den(157) = den(67)*den(152)
  den(158) = den(43)*den(152)
  den(159) = den(72)*den(145)
  den(160) = den(55)*den(145)
  den(161) = den(72)*den(152)
  den(162) = den(55)*den(152)
  den(163) = den(78)*den(121)
  den(164) = den(81)*den(121)
  den(165) = den(83)*den(121)
  den(166) = den(85)*den(121)
  den(167) = den(78)*den(139)
  den(168) = den(81)*den(139)
  den(169) = den(83)*den(139)
  den(170) = den(85)*den(139)
  den(171) = den(24)*den(116)
  den(172) = den(26)*den(116)
  den(173) = den(24)*den(134)
  den(174) = den(26)*den(134)
  den(175) = den(10)*den(128)
  den(176) = den(12)*den(128)
  den(177) = den(10)*den(110)
  den(178) = den(12)*den(110)
  den(179) = den(92)*den(134)
  den(180) = den(92)*den(110)
  den(182) = den(37)*den(181)
  den(184) = den(37)*den(183)
  den(185) = den(67)*den(116)
  den(186) = den(43)*den(116)
  den(187) = den(67)*den(128)
  den(188) = den(43)*den(128)
  den(190) = den(43)*den(189)
  den(192) = den(34)*den(191)
  den(193) = den(181)*den(192)
  den(194) = den(183)*den(192)
  den(195) = den(52)*den(181)
  den(196) = den(52)*den(183)
  den(197) = den(72)*den(116)
  den(198) = den(55)*den(116)
  den(199) = den(72)*den(128)
  den(200) = den(55)*den(128)
  den(201) = den(55)*den(189)
  den(203) = den(51)*den(202)
  den(204) = den(181)*den(203)
  den(205) = den(183)*den(203)
  den(206) = den(78)*den(134)
  den(207) = den(81)*den(134)
  den(208) = den(78)*den(110)
  den(209) = den(81)*den(110)
  den(210) = den(50)*den(191)
  den(211) = den(181)*den(210)
  den(212) = den(183)*den(210)
  den(213) = den(83)*den(134)
  den(214) = den(85)*den(134)
  den(215) = den(83)*den(110)
  den(216) = den(85)*den(110)
  den(217) = den(35)*den(202)
  den(218) = den(181)*den(217)
  den(219) = den(183)*den(217)
  den(220) = den(1)*den(2)*den(3)
  den(221) = den(2)*den(3)*den(6)
  den(222) = den(1)*den(17)*den(18)
  den(223) = den(6)*den(17)*den(18)
  den(224) = den(2)*den(177)
  den(225) = den(2)*den(178)
  den(226) = den(2)*den(180)
  den(227) = den(18)*den(173)
  den(228) = den(18)*den(174)
  den(229) = den(18)*den(179)
  den(230) = den(17)*den(175)
  den(231) = den(17)*den(176)
  den(232) = den(3)*den(171)
  den(233) = den(3)*den(172)
  den(234) = den(17)*den(187)
  den(235) = den(17)*den(188)
  den(236) = den(3)*den(185)
  den(237) = den(3)*den(186)
  den(238) = den(17)*den(199)
  den(239) = den(17)*den(200)
  den(240) = den(3)*den(197)
  den(241) = den(3)*den(198)
  den(242) = den(2)*den(208)
  den(243) = den(2)*den(209)
  den(244) = den(2)*den(215)
  den(245) = den(2)*den(216)
  den(246) = den(18)*den(206)
  den(247) = den(18)*den(207)
  den(248) = den(18)*den(213)
  den(249) = den(18)*den(214)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(172)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,5),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,11),wf(:,14)) * den(13)
  A(5) = cont_SS(wf(:,15),wf(:,16)) * den(16)
  A(6) = cont_QA(wf(:,20),wf(:,21)) * den(20)
  A(7) = cont_QA(wf(:,20),wf(:,22)) * den(22)
  A(8) = cont_QA(wf(:,24),wf(:,25)) * den(25)
  A(9) = cont_QA(wf(:,24),wf(:,27)) * den(27)
  A(10) = cont_SS(wf(:,16),wf(:,28)) * den(29)
  A(11) = cont_QA(wf(:,12),wf(:,29)) * den(30)
  A(12) = cont_QA(wf(:,14),wf(:,29)) * den(31)
  A(13) = cont_QA(wf(:,25),wf(:,30)) * den(32)
  A(14) = cont_QA(wf(:,27),wf(:,30)) * den(33)
  A(15) = cont_VV(wf(:,33),wf(:,36)) * den(38)
  A(16) = cont_VV(wf(:,38),wf(:,39)) * den(40)
  A(17) = cont_VV(wf(:,42),wf(:,43)) * den(44)
  A(18) = cont_QA(wf(:,45),wf(:,46)) * den(47)
  A(19) = cont_QA(wf(:,45),wf(:,48)) * den(49)
  A(20) = cont_VV(wf(:,33),wf(:,53)) * den(53)
  A(21) = cont_VV(wf(:,38),wf(:,54)) * den(54)
  A(22) = cont_VV(wf(:,42),wf(:,56)) * den(56)
  A(23) = cont_QA(wf(:,58),wf(:,59)) * den(59)
  A(24) = cont_QA(wf(:,58),wf(:,61)) * den(61)
  A(25) = cont_QA(wf(:,46),wf(:,62)) * den(62)
  A(26) = cont_QA(wf(:,48),wf(:,62)) * den(63)
  A(27) = cont_QA(wf(:,59),wf(:,63)) * den(64)
  A(28) = cont_QA(wf(:,61),wf(:,63)) * den(65)
  A(29) = cont_VV(wf(:,64),wf(:,65)) * den(68)
  A(30) = cont_VV(wf(:,43),wf(:,66)) * den(69)
  A(31) = cont_VV(wf(:,64),wf(:,67)) * den(70)
  A(32) = cont_VV(wf(:,43),wf(:,68)) * den(71)
  A(33) = cont_VV(wf(:,65),wf(:,69)) * den(73)
  A(34) = cont_VV(wf(:,56),wf(:,66)) * den(74)
  A(35) = cont_VV(wf(:,67),wf(:,69)) * den(75)
  A(36) = cont_VV(wf(:,56),wf(:,68)) * den(76)
  A(37) = cont_VV(wf(:,70),wf(:,71)) * den(79)
  A(38) = cont_VV(wf(:,73),wf(:,74)) * den(82)
  A(39) = cont_VV(wf(:,71),wf(:,75)) * den(84)
  A(40) = cont_VV(wf(:,73),wf(:,77)) * den(86)
  A(41) = cont_VV(wf(:,70),wf(:,78)) * den(87)
  A(42) = cont_VV(wf(:,74),wf(:,79)) * den(88)
  A(43) = cont_VV(wf(:,75),wf(:,78)) * den(89)
  A(44) = cont_VV(wf(:,77),wf(:,79)) * den(90)

  A(45) = cont_QA(wf(:,5),wf(:,80)) * den(5)
  A(46) = cont_QA(wf(:,5),wf(:,81)) * den(8)
  A(47) = cont_QA(wf(:,12),wf(:,82)) * den(11)
  A(48) = cont_QA(wf(:,14),wf(:,82)) * den(13)
  A(49) = cont_QA(wf(:,20),wf(:,83)) * den(20)
  A(50) = cont_QA(wf(:,20),wf(:,84)) * den(22)
  A(51) = cont_QA(wf(:,25),wf(:,85)) * den(25)
  A(52) = cont_QA(wf(:,27),wf(:,85)) * den(27)
  A(53) = cont_QA(wf(:,12),wf(:,86)) * den(30)
  A(54) = cont_QA(wf(:,14),wf(:,86)) * den(31)
  A(55) = cont_QA(wf(:,25),wf(:,87)) * den(32)
  A(56) = cont_QA(wf(:,27),wf(:,87)) * den(33)
  A(57) = cont_SS(wf(:,16),wf(:,88)) * den(93)
  A(58) = cont_QA(wf(:,89),wf(:,90)) * den(96)
  A(59) = cont_QA(wf(:,90),wf(:,91)) * den(97)
  A(60) = cont_QA(wf(:,6),wf(:,93)) * den(5)
  A(61) = cont_QA(wf(:,9),wf(:,93)) * den(8)
  A(62) = cont_QA(wf(:,89),wf(:,94)) * den(99)
  A(63) = cont_QA(wf(:,91),wf(:,94)) * den(100)
  A(64) = cont_QA(wf(:,25),wf(:,95)) * den(32)
  A(65) = cont_QA(wf(:,27),wf(:,95)) * den(33)
  A(66) = cont_QA(wf(:,21),wf(:,97)) * den(20)
  A(67) = cont_QA(wf(:,22),wf(:,97)) * den(22)
  A(68) = cont_QA(wf(:,25),wf(:,98)) * den(25)
  A(69) = cont_QA(wf(:,27),wf(:,98)) * den(27)
  A(70) = cont_SS(wf(:,16),wf(:,99)) * den(29)
  A(71) = cont_SS(wf(:,16),wf(:,100)) * den(101)
  A(72) = cont_QA(wf(:,101),wf(:,102)) * den(104)
  A(73) = cont_QA(wf(:,102),wf(:,103)) * den(105)
  A(74) = cont_QA(wf(:,20),wf(:,106)) * den(20)
  A(75) = cont_QA(wf(:,20),wf(:,107)) * den(22)
  A(76) = cont_QA(wf(:,101),wf(:,108)) * den(107)
  A(77) = cont_QA(wf(:,103),wf(:,108)) * den(108)
  A(78) = cont_QA(wf(:,12),wf(:,109)) * den(30)
  A(79) = cont_QA(wf(:,14),wf(:,109)) * den(31)
  A(80) = cont_QA(wf(:,5),wf(:,112)) * den(5)
  A(81) = cont_QA(wf(:,5),wf(:,113)) * den(8)
  A(82) = cont_QA(wf(:,12),wf(:,114)) * den(11)
  A(83) = cont_QA(wf(:,14),wf(:,114)) * den(13)
  A(84) = cont_SS(wf(:,16),wf(:,115)) * den(16)
  A(85) = cont_VV(wf(:,64),wf(:,116)) * den(68)
  A(86) = cont_VV(wf(:,43),wf(:,117)) * den(69)
  A(87) = cont_VV(wf(:,64),wf(:,118)) * den(70)
  A(88) = cont_VV(wf(:,43),wf(:,119)) * den(71)
  A(89) = cont_VV(wf(:,69),wf(:,116)) * den(73)
  A(90) = cont_VV(wf(:,56),wf(:,117)) * den(74)
  A(91) = cont_VV(wf(:,69),wf(:,118)) * den(75)
  A(92) = cont_VV(wf(:,56),wf(:,119)) * den(76)
  A(93) = cont_VV(wf(:,70),wf(:,120)) * den(79)
  A(94) = cont_VV(wf(:,74),wf(:,121)) * den(82)
  A(95) = cont_VV(wf(:,75),wf(:,120)) * den(84)
  A(96) = cont_VV(wf(:,77),wf(:,121)) * den(86)
  A(97) = cont_VV(wf(:,70),wf(:,122)) * den(87)
  A(98) = cont_VV(wf(:,74),wf(:,123)) * den(88)
  A(99) = cont_VV(wf(:,75),wf(:,122)) * den(89)
  A(100) = cont_VV(wf(:,77),wf(:,123)) * den(90)
  A(101) = cont_VV(wf(:,64),wf(:,124)) * den(70)
  A(102) = cont_VV(wf(:,43),wf(:,125)) * den(71)
  A(103) = cont_VV(wf(:,64),wf(:,126)) * den(68)
  A(104) = cont_VV(wf(:,43),wf(:,127)) * den(69)
  A(105) = cont_VV(wf(:,69),wf(:,124)) * den(75)
  A(106) = cont_VV(wf(:,56),wf(:,125)) * den(76)
  A(107) = cont_VV(wf(:,69),wf(:,126)) * den(73)
  A(108) = cont_VV(wf(:,56),wf(:,127)) * den(74)
  A(109) = cont_VV(wf(:,70),wf(:,128)) * den(87)
  A(110) = cont_VV(wf(:,74),wf(:,129)) * den(88)
  A(111) = cont_VV(wf(:,75),wf(:,128)) * den(89)
  A(112) = cont_VV(wf(:,77),wf(:,129)) * den(90)
  A(113) = cont_VV(wf(:,70),wf(:,130)) * den(79)
  A(114) = cont_VV(wf(:,74),wf(:,131)) * den(82)
  A(115) = cont_VV(wf(:,75),wf(:,130)) * den(84)
  A(116) = cont_VV(wf(:,77),wf(:,131)) * den(86)
  A(117) = cont_VV(wf(:,36),wf(:,132)) * den(38)
  A(118) = cont_VV(wf(:,39),wf(:,134)) * den(40)
  A(119) = cont_VV(wf(:,43),wf(:,136)) * den(44)
  A(120) = cont_QA(wf(:,45),wf(:,138)) * den(47)
  A(121) = cont_QA(wf(:,45),wf(:,140)) * den(49)
  A(122) = cont_VV(wf(:,53),wf(:,132)) * den(53)
  A(123) = cont_VV(wf(:,54),wf(:,134)) * den(54)
  A(124) = cont_VV(wf(:,56),wf(:,136)) * den(56)
  A(125) = cont_QA(wf(:,58),wf(:,142)) * den(59)
  A(126) = cont_QA(wf(:,58),wf(:,144)) * den(61)
  A(127) = cont_QA(wf(:,62),wf(:,138)) * den(62)
  A(128) = cont_QA(wf(:,62),wf(:,140)) * den(63)
  A(129) = cont_QA(wf(:,63),wf(:,142)) * den(64)
  A(130) = cont_QA(wf(:,63),wf(:,144)) * den(65)
  A(131) = cont_QA(wf(:,146),wf(:,147)) * den(112)
  A(132) = cont_QA(wf(:,146),wf(:,149)) * den(115)
  A(133) = cont_QA(wf(:,150),wf(:,151)) * den(118)
  A(134) = cont_QA(wf(:,150),wf(:,152)) * den(120)
  A(135) = cont_SS(wf(:,16),wf(:,154)) * den(122)
  A(136) = cont_QA(wf(:,12),wf(:,155)) * den(123)
  A(137) = cont_QA(wf(:,14),wf(:,155)) * den(124)
  A(138) = cont_QA(wf(:,90),wf(:,156)) * den(125)
  A(139) = cont_QA(wf(:,90),wf(:,157)) * den(126)
  A(140) = cont_QA(wf(:,159),wf(:,160)) * den(130)
  A(141) = cont_QA(wf(:,159),wf(:,162)) * den(133)
  A(142) = cont_QA(wf(:,163),wf(:,164)) * den(136)
  A(143) = cont_QA(wf(:,163),wf(:,165)) * den(138)
  A(144) = cont_SS(wf(:,16),wf(:,167)) * den(140)
  A(145) = cont_QA(wf(:,102),wf(:,168)) * den(141)
  A(146) = cont_QA(wf(:,102),wf(:,169)) * den(142)
  A(147) = cont_QA(wf(:,25),wf(:,170)) * den(143)
  A(148) = cont_QA(wf(:,27),wf(:,170)) * den(144)
  A(149) = cont_QA(wf(:,12),wf(:,172)) * den(146)
  A(150) = cont_QA(wf(:,14),wf(:,172)) * den(147)
  A(151) = cont_QA(wf(:,94),wf(:,156)) * den(148)
  A(152) = cont_QA(wf(:,94),wf(:,157)) * den(149)
  A(153) = cont_QA(wf(:,108),wf(:,168)) * den(150)
  A(154) = cont_QA(wf(:,108),wf(:,169)) * den(151)
  A(155) = cont_QA(wf(:,25),wf(:,174)) * den(153)
  A(156) = cont_QA(wf(:,27),wf(:,174)) * den(154)
  A(157) = cont_VV(wf(:,64),wf(:,175)) * den(155)
  A(158) = cont_VV(wf(:,43),wf(:,176)) * den(156)
  A(159) = cont_VV(wf(:,64),wf(:,177)) * den(157)
  A(160) = cont_VV(wf(:,43),wf(:,178)) * den(158)
  A(161) = cont_VV(wf(:,69),wf(:,175)) * den(159)
  A(162) = cont_VV(wf(:,56),wf(:,176)) * den(160)
  A(163) = cont_VV(wf(:,69),wf(:,177)) * den(161)
  A(164) = cont_VV(wf(:,56),wf(:,178)) * den(162)
  A(165) = cont_VV(wf(:,70),wf(:,179)) * den(163)
  A(166) = cont_VV(wf(:,74),wf(:,180)) * den(164)
  A(167) = cont_VV(wf(:,75),wf(:,179)) * den(165)
  A(168) = cont_VV(wf(:,77),wf(:,180)) * den(166)
  A(169) = cont_VV(wf(:,70),wf(:,181)) * den(167)
  A(170) = cont_VV(wf(:,74),wf(:,182)) * den(168)
  A(171) = cont_VV(wf(:,75),wf(:,181)) * den(169)
  A(172) = cont_VV(wf(:,77),wf(:,182)) * den(170)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(172)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(3)+A(6)+A(8)+A(11)+A(13)+A(37)+A(39)+A(41)+A(43))*f(1)+(A(2)+A(4)+A(7)+A(9)+A(12)+A(14)+A(15)+A(18)+A(20)+A(23) &
       +A(25)+A(27)+A(29)+A(31)+A(33)+A(35)+A(38)+A(40)+A(42)+A(44))*f(2)+(A(16)+A(19)+A(21)+A(24)+A(26)+A(28)+A(30)+A(32)+A(34) &
       +A(36))*f(3)+(-A(5)-A(10))*f(10)+(-A(17)-A(22))*f(11)

  M2(1) = (-A(131)-A(133)-A(136)-A(138)-A(140)-A(142)-A(145)-A(147)-A(149)-A(151)-A(153)-A(155)-A(165)-A(167)-A(169)-A(171))*f(4) &
       +(-A(132)-A(134)-A(137)-A(139)-A(141)-A(143)-A(146)-A(148)-A(150)-A(152)-A(154)-A(156)-A(157)-A(159)-A(161)-A(163)-A(166) &
       -A(168)-A(170)-A(172))*f(5)+(-A(158)-A(160)-A(162)-A(164))*f(6)+(A(45)+A(47)+A(49)+A(51)+A(53)+A(55)+A(58)+A(60)+A(62) &
       +A(64)+A(66)+A(68)+A(72)+A(74)+A(76)+A(78)+A(80)+A(82)+A(93)+A(95)+A(97)+A(99)+A(109)+A(111)+A(113)+A(115))*f(7)+(A(46) &
       +A(48)+A(50)+A(52)+A(54)+A(56)+A(59)+A(61)+A(63)+A(65)+A(67)+A(69)+A(73)+A(75)+A(77)+A(79)+A(81)+A(83)+A(85)+A(87)+A(89) &
       +A(91)+A(94)+A(96)+A(98)+A(100)+A(101)+A(103)+A(105)+A(107)+A(110)+A(112)+A(114)+A(116)+A(117)+A(120)+A(122)+A(125)+A(127) &
       +A(129))*f(8)+(A(86)+A(88)+A(90)+A(92)+A(102)+A(104)+A(106)+A(108)+A(118)+A(121)+A(123)+A(126)+A(128)+A(130))*f(9)+(A(135) &
       +A(144))*f(12)+(-A(57)-A(71))*f(13)+(-A(119)-A(124))*f(14)+(-A(70)-A(84))*f(15)

end subroutine colourvectors

end module ol_loop_eevvjj_eexbbxaz_1_/**/REALKIND
