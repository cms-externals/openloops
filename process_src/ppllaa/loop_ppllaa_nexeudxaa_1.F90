
module ol_colourmatrix_ppllaa_nexeudxaa_1_/**/REALKIND
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
end module ol_colourmatrix_ppllaa_nexeudxaa_1_/**/REALKIND



module ol_forced_parameters_ppllaa_nexeudxaa_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllaa_nexeudxaa_1_/**/REALKIND

module ol_loop_ppllaa_nexeudxaa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(23), c(6)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:158)
  ! denominators
  complex(REALKIND), save :: den(135)
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
    f( 1) = (CI*eQED**4)/(18._/**/REALKIND*sw**2)
    f( 2) = (CI*eQED**4)/(9._/**/REALKIND*sw**2)
    f( 3) = (CI*eQED**4)/(6._/**/REALKIND*sw**2)
    f( 4) = (2*CI*eQED**4)/(9._/**/REALKIND*sw**2)
    f( 5) = (CI*eQED**4)/(3._/**/REALKIND*sw**2)
    f( 6) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*eQED**4*gQCD**2)/(18._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*eQED**4*gQCD**2)/(9._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(10) = (2*CI*countertermnorm*eQED**4*gQCD**2)/(9._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(18._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(9._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(15) = (2*CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(9._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(17) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(18) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*18._/**/REALKIND)
    f(19) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*9._/**/REALKIND)
    f(20) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(21) = (2*eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*9._/**/REALKIND)
    f(22) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(23) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)

  c = [ 4*f(18), 4*f(19), 4*f(20), 4*f(21), 4*f(22), 4*f(23) ]
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
  complex(REALKIND) :: A(82)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_QA_W(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,3))
  call prop_W_W(wf(:,2),Q(:,12),MW,1_intkind1,wf(:,4))
  call vert_WWV_V(wf(:,-4),wf(:,-5),wf(:,3),wf(:,5))
  call vert_UV_W(wf(:,3),Q(:,3),wf(:,-4),Q(:,16),wf(:,6))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,4),Q(:,12),wf(:,7))
  call prop_W_W(wf(:,6),Q(:,19),MW,1_intkind1,wf(:,8))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,4),Q(:,12),wf(:,9))
  call vert_UV_W(wf(:,3),Q(:,3),wf(:,-5),Q(:,32),wf(:,10))
  call prop_W_W(wf(:,9),Q(:,28),MW,1_intkind1,wf(:,11))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,12))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,20),ZERO,0_intkind1,wf(:,14))
  call prop_A_Q(wf(:,13),Q(:,40),ZERO,0_intkind1,wf(:,15))
  call vert_WQ_A(wf(:,3),wf(:,14),wf(:,16))
  call vert_AW_Q(wf(:,-3),wf(:,3),wf(:,17))
  call vert_VQ_A(wf(:,-5),wf(:,14),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,11),ZERO,0_intkind1,wf(:,19))
  call vert_QA_W(wf(:,14),wf(:,-3),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,28),MW,1_intkind1,wf(:,21))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,22))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,36),ZERO,0_intkind1,wf(:,24))
  call prop_A_Q(wf(:,23),Q(:,24),ZERO,0_intkind1,wf(:,25))
  call vert_WQ_A(wf(:,3),wf(:,24),wf(:,26))
  call vert_WQ_A(wf(:,3),wf(:,-2),wf(:,27))
  call vert_AV_Q(wf(:,25),wf(:,-5),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,7),ZERO,0_intkind1,wf(:,29))
  call vert_QA_W(wf(:,-2),wf(:,25),wf(:,30))
  call prop_W_W(wf(:,30),Q(:,28),MW,1_intkind1,wf(:,31))
  call vert_VQ_A(wf(:,-4),wf(:,24),wf(:,32))
  call vert_QA_W(wf(:,24),wf(:,-3),wf(:,33))
  call prop_W_W(wf(:,33),Q(:,44),MW,1_intkind1,wf(:,34))
  call vert_AV_Q(wf(:,15),wf(:,-4),wf(:,35))
  call vert_QA_W(wf(:,-2),wf(:,15),wf(:,36))
  call prop_W_W(wf(:,36),Q(:,44),MW,1_intkind1,wf(:,37))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,18),ZERO,0_intkind1,wf(:,39))
  call vert_QA_W(wf(:,39),wf(:,0),wf(:,40))
  call prop_W_W(wf(:,40),Q(:,19),MW,1_intkind1,wf(:,41))
  call vert_AW_Q(wf(:,0),wf(:,4),wf(:,42))
  call vert_VQ_A(wf(:,-5),wf(:,39),wf(:,43))
  call prop_A_Q(wf(:,42),Q(:,13),ZERO,0_intkind1,wf(:,44))
  call vert_VQ_A(wf(:,-5),wf(:,-1),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,34),ZERO,0_intkind1,wf(:,46))
  call vert_QA_W(wf(:,46),wf(:,0),wf(:,47))
  call prop_W_W(wf(:,47),Q(:,35),MW,1_intkind1,wf(:,48))
  call vert_VQ_A(wf(:,-4),wf(:,46),wf(:,49))
  call counter_QA_W(wf(:,-2),wf(:,-3),wf(:,50))
  call prop_W_W(wf(:,50),Q(:,12),MW,1_intkind1,wf(:,51))
  call counter_WQ_A(wf(:,3),wf(:,14),wf(:,52))
  call counter_VQ_A(wf(:,-5),wf(:,14),wf(:,53))
  call counter_WQ_A(wf(:,3),wf(:,24),wf(:,54))
  call counter_AV_Q(wf(:,25),wf(:,-5),wf(:,55))
  call counter_VQ_A(wf(:,-4),wf(:,24),wf(:,56))
  call counter_AV_Q(wf(:,15),wf(:,-4),wf(:,57))
  call counter_QA_W(wf(:,14),wf(:,-3),wf(:,58))
  call prop_W_W(wf(:,10),Q(:,35),MW,1_intkind1,wf(:,59))
  call counter_AW_Q(wf(:,-3),wf(:,3),wf(:,60))
  call prop_Q_A(wf(:,18),Q(:,52),ZERO,0_intkind1,wf(:,61))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,62))
  call prop_A_Q(wf(:,62),Q(:,40),ZERO,0_intkind1,wf(:,63))
  call counter_QA_W(wf(:,24),wf(:,-3),wf(:,64))
  call prop_Q_A(wf(:,32),Q(:,52),ZERO,0_intkind1,wf(:,65))
  call vert_AV_Q(wf(:,63),wf(:,-4),wf(:,66))
  call vert_QA_W(wf(:,-2),wf(:,63),wf(:,67))
  call prop_W_W(wf(:,67),Q(:,44),MW,1_intkind1,wf(:,68))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,69))
  call prop_A_Q(wf(:,69),Q(:,24),ZERO,0_intkind1,wf(:,70))
  call vert_AV_Q(wf(:,70),wf(:,-5),wf(:,71))
  call vert_QA_W(wf(:,-2),wf(:,70),wf(:,72))
  call prop_W_W(wf(:,72),Q(:,28),MW,1_intkind1,wf(:,73))
  call counter_QA_W(wf(:,-2),wf(:,25),wf(:,74))
  call counter_WQ_A(wf(:,3),wf(:,-2),wf(:,75))
  call prop_A_Q(wf(:,28),Q(:,56),ZERO,0_intkind1,wf(:,76))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,36),ZERO,0_intkind1,wf(:,78))
  call vert_WQ_A(wf(:,3),wf(:,78),wf(:,79))
  call counter_QA_W(wf(:,-2),wf(:,15),wf(:,80))
  call prop_A_Q(wf(:,35),Q(:,56),ZERO,0_intkind1,wf(:,81))
  call vert_VQ_A(wf(:,-4),wf(:,78),wf(:,82))
  call vert_QA_W(wf(:,78),wf(:,-3),wf(:,83))
  call prop_W_W(wf(:,83),Q(:,44),MW,1_intkind1,wf(:,84))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,85))
  call prop_Q_A(wf(:,85),Q(:,20),ZERO,0_intkind1,wf(:,86))
  call vert_WQ_A(wf(:,3),wf(:,86),wf(:,87))
  call vert_VQ_A(wf(:,-5),wf(:,86),wf(:,88))
  call vert_QA_W(wf(:,86),wf(:,-3),wf(:,89))
  call prop_W_W(wf(:,89),Q(:,28),MW,1_intkind1,wf(:,90))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,51),Q(:,12),wf(:,91))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,51),Q(:,12),wf(:,92))
  call prop_W_W(wf(:,92),Q(:,28),MW,1_intkind1,wf(:,93))
  call vert_AW_Q(wf(:,0),wf(:,51),wf(:,94))
  call prop_A_Q(wf(:,94),Q(:,13),ZERO,0_intkind1,wf(:,95))
  call vert_AW_Q(wf(:,15),wf(:,3),wf(:,96))
  call counter_Q_A(ctqq,wf(:,14),Q(:,20),wf(:,97))
  call prop_A_Q(wf(:,96),Q(:,43),ZERO,0_intkind1,wf(:,98))
  call counter_A_Q(ctqq,wf(:,15),Q(:,40),wf(:,99))
  call prop_Q_A(wf(:,16),Q(:,23),ZERO,0_intkind1,wf(:,100))
  call prop_Q_A(wf(:,97),Q(:,20),ZERO,0_intkind1,wf(:,101))
  call vert_QA_W(wf(:,101),wf(:,-3),wf(:,102))
  call vert_VQ_A(wf(:,-5),wf(:,101),wf(:,103))
  call counter_A_Q(ctqq,wf(:,19),Q(:,11),wf(:,104))
  call vert_AW_Q(wf(:,25),wf(:,3),wf(:,105))
  call counter_Q_A(ctqq,wf(:,24),Q(:,36),wf(:,106))
  call prop_A_Q(wf(:,105),Q(:,27),ZERO,0_intkind1,wf(:,107))
  call counter_A_Q(ctqq,wf(:,25),Q(:,24),wf(:,108))
  call prop_Q_A(wf(:,26),Q(:,39),ZERO,0_intkind1,wf(:,109))
  call prop_A_Q(wf(:,108),Q(:,24),ZERO,0_intkind1,wf(:,110))
  call vert_QA_W(wf(:,-2),wf(:,110),wf(:,111))
  call counter_Q_A(ctqq,wf(:,29),Q(:,7),wf(:,112))
  call vert_AV_Q(wf(:,110),wf(:,-5),wf(:,113))
  call prop_Q_A(wf(:,106),Q(:,36),ZERO,0_intkind1,wf(:,114))
  call vert_QA_W(wf(:,114),wf(:,-3),wf(:,115))
  call vert_VQ_A(wf(:,-4),wf(:,114),wf(:,116))
  call prop_A_Q(wf(:,99),Q(:,40),ZERO,0_intkind1,wf(:,117))
  call vert_QA_W(wf(:,-2),wf(:,117),wf(:,118))
  call vert_AV_Q(wf(:,117),wf(:,-4),wf(:,119))
  call prop_W_W(wf(:,5),Q(:,51),MW,1_intkind1,wf(:,120))
  call vert_VQ_A(wf(:,-4),wf(:,29),wf(:,121))
  call prop_Q_A(wf(:,121),Q(:,23),ZERO,0_intkind1,wf(:,122))
  call vert_VQ_A(wf(:,-5),wf(:,29),wf(:,123))
  call prop_Q_A(wf(:,123),Q(:,39),ZERO,0_intkind1,wf(:,124))
  call vert_AV_Q(wf(:,19),wf(:,-4),wf(:,125))
  call prop_A_Q(wf(:,125),Q(:,27),ZERO,0_intkind1,wf(:,126))
  call vert_AV_Q(wf(:,19),wf(:,-5),wf(:,127))
  call prop_A_Q(wf(:,127),Q(:,43),ZERO,0_intkind1,wf(:,128))
  call vert_WQ_A(wf(:,8),wf(:,-2),wf(:,129))
  call prop_Q_A(wf(:,129),Q(:,23),ZERO,0_intkind1,wf(:,130))
  call vert_AW_Q(wf(:,-3),wf(:,8),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,27),ZERO,0_intkind1,wf(:,132))
  call vert_UV_W(wf(:,8),Q(:,19),wf(:,-5),Q(:,32),wf(:,133))
  call prop_W_W(wf(:,133),Q(:,51),MW,1_intkind1,wf(:,134))
  call vert_WQ_A(wf(:,59),wf(:,-2),wf(:,135))
  call prop_Q_A(wf(:,135),Q(:,39),ZERO,0_intkind1,wf(:,136))
  call vert_AW_Q(wf(:,-3),wf(:,59),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,43),ZERO,0_intkind1,wf(:,138))
  call vert_UV_W(wf(:,59),Q(:,35),wf(:,-4),Q(:,16),wf(:,139))
  call prop_W_W(wf(:,139),Q(:,51),MW,1_intkind1,wf(:,140))
  call vert_WQ_A(wf(:,41),wf(:,-2),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,23),ZERO,0_intkind1,wf(:,142))
  call vert_AW_Q(wf(:,-3),wf(:,41),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,27),ZERO,0_intkind1,wf(:,144))
  call vert_UV_W(wf(:,41),Q(:,19),wf(:,-5),Q(:,32),wf(:,145))
  call prop_W_W(wf(:,145),Q(:,51),MW,1_intkind1,wf(:,146))
  call prop_Q_A(wf(:,43),Q(:,50),ZERO,0_intkind1,wf(:,147))
  call vert_QA_W(wf(:,147),wf(:,0),wf(:,148))
  call prop_W_W(wf(:,148),Q(:,51),MW,1_intkind1,wf(:,149))
  call vert_WQ_A(wf(:,48),wf(:,-2),wf(:,150))
  call prop_Q_A(wf(:,150),Q(:,39),ZERO,0_intkind1,wf(:,151))
  call vert_AW_Q(wf(:,-3),wf(:,48),wf(:,152))
  call prop_A_Q(wf(:,152),Q(:,43),ZERO,0_intkind1,wf(:,153))
  call vert_UV_W(wf(:,48),Q(:,35),wf(:,-4),Q(:,16),wf(:,154))
  call prop_W_W(wf(:,154),Q(:,51),MW,1_intkind1,wf(:,155))
  call prop_Q_A(wf(:,49),Q(:,50),ZERO,0_intkind1,wf(:,156))
  call vert_QA_W(wf(:,156),wf(:,0),wf(:,157))
  call prop_W_W(wf(:,157),Q(:,51),MW,1_intkind1,wf(:,158))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MW2)
  den(2) = 1 / (Q(5,12) - MW2)
  den(4) = 1 / (Q(5,19) - MW2)
  den(7) = 1 / (Q(5,28) - MW2)
  den(10) = 1 / (Q(5,20))
  den(11) = 1 / (Q(5,40))
  den(14) = 1 / (Q(5,11))
  den(19) = 1 / (Q(5,36))
  den(20) = 1 / (Q(5,24))
  den(23) = 1 / (Q(5,7))
  den(29) = 1 / (Q(5,44) - MW2)
  den(35) = 1 / (Q(5,18))
  den(38) = 1 / (Q(5,13))
  den(41) = 1 / (Q(5,34))
  den(42) = 1 / (Q(5,35) - MW2)
  den(52) = 1 / (Q(5,52))
  den(59) = 1 / (Q(5,56))
  den(66) = 1 / (Q(5,43))
  den(69) = 1 / (Q(5,23))
  den(77) = 1 / (Q(5,27))
  den(80) = 1 / (Q(5,39))
  den(99) = 1 / (Q(5,51) - MW2)
  den(114) = 1 / (Q(5,50))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(6) = den(2)*den(5)
  den(8) = den(2)*den(7)
  den(9) = den(1)*den(8)
  den(12) = den(1)*den(10)
  den(13) = den(11)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(10)*den(15)
  den(17) = den(7)*den(10)
  den(18) = den(1)*den(17)
  den(21) = den(1)*den(19)
  den(22) = den(20)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(20)*den(24)
  den(26) = den(7)*den(20)
  den(27) = den(1)*den(26)
  den(28) = den(15)*den(19)
  den(30) = den(19)*den(29)
  den(31) = den(1)*den(30)
  den(32) = den(11)*den(24)
  den(33) = den(11)*den(29)
  den(34) = den(1)*den(33)
  den(36) = den(4)*den(35)
  den(37) = den(2)*den(36)
  den(39) = den(2)*den(38)
  den(40) = den(35)*den(39)
  den(43) = den(41)*den(42)
  den(44) = den(2)*den(43)
  den(45) = den(39)*den(41)
  den(46) = den(19)*den(36)
  den(47) = den(11)*den(36)
  den(48) = den(10)*den(43)
  den(49) = den(20)*den(43)
  den(50) = den(1)*den(42)
  den(51) = den(10)*den(50)
  den(53) = den(10)*den(52)
  den(54) = den(1)*den(53)
  den(55) = den(5)*den(19)
  den(56) = den(19)*den(52)
  den(57) = den(1)*den(56)
  den(58) = den(20)*den(50)
  den(60) = den(20)*den(59)
  den(61) = den(1)*den(60)
  den(62) = den(5)*den(11)
  den(63) = den(11)*den(59)
  den(64) = den(1)*den(63)
  den(65) = den(1)*den(11)
  den(67) = den(65)*den(66)
  den(68) = den(10)*den(67)
  den(70) = den(12)*den(69)
  den(71) = den(11)*den(70)
  den(72) = den(10)**2
  den(73) = den(50)*den(72)
  den(74) = den(15)*den(72)
  den(75) = den(15)*den(53)
  den(76) = den(1)*den(20)
  den(78) = den(76)*den(77)
  den(79) = den(19)*den(78)
  den(81) = den(21)*den(80)
  den(82) = den(20)*den(81)
  den(83) = den(20)**2
  den(84) = den(50)*den(83)
  den(85) = den(24)*den(60)
  den(86) = den(24)*den(83)
  den(87) = den(19)**2
  den(88) = den(5)*den(87)
  den(89) = den(15)*den(87)
  den(90) = den(15)*den(56)
  den(91) = den(11)**2
  den(92) = den(5)*den(91)
  den(93) = den(24)*den(63)
  den(94) = den(24)*den(91)
  den(95) = den(36)*den(87)
  den(96) = den(36)*den(91)
  den(97) = den(43)*den(72)
  den(98) = den(43)*den(83)
  den(100) = den(1)*den(99)
  den(101) = den(24)*den(69)
  den(102) = den(24)*den(80)
  den(103) = den(15)*den(77)
  den(104) = den(15)*den(66)
  den(105) = den(5)*den(69)
  den(106) = den(5)*den(77)
  den(107) = den(5)*den(99)
  den(108) = den(50)*den(80)
  den(109) = den(50)*den(66)
  den(110) = den(50)*den(99)
  den(111) = den(36)*den(69)
  den(112) = den(36)*den(77)
  den(113) = den(36)*den(99)
  den(115) = den(35)*den(114)
  den(116) = den(99)*den(115)
  den(117) = den(43)*den(80)
  den(118) = den(43)*den(66)
  den(119) = den(43)*den(99)
  den(120) = den(41)*den(114)
  den(121) = den(99)*den(120)
  den(122) = den(1)*den(10)*den(11)
  den(123) = den(1)*den(19)*den(20)
  den(124) = den(10)*den(104)
  den(125) = den(10)*den(109)
  den(126) = den(20)*den(102)
  den(127) = den(20)*den(108)
  den(128) = den(19)*den(103)
  den(129) = den(19)*den(106)
  den(130) = den(11)*den(101)
  den(131) = den(11)*den(105)
  den(132) = den(19)*den(112)
  den(133) = den(11)*den(111)
  den(134) = den(10)*den(118)
  den(135) = den(20)*den(117)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(82)

  A(1) = cont_VV(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(6)
  A(3) = cont_VV(wf(:,10),wf(:,11)) * den(9)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(13)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(16)
  A(6) = cont_VV(wf(:,10),wf(:,21)) * den(18)
  A(7) = cont_QA(wf(:,25),wf(:,26)) * den(22)
  A(8) = cont_QA(wf(:,28),wf(:,29)) * den(25)
  A(9) = cont_VV(wf(:,10),wf(:,31)) * den(27)
  A(10) = cont_QA(wf(:,19),wf(:,32)) * den(28)
  A(11) = cont_VV(wf(:,6),wf(:,34)) * den(31)
  A(12) = cont_QA(wf(:,29),wf(:,35)) * den(32)
  A(13) = cont_VV(wf(:,6),wf(:,37)) * den(34)
  A(14) = cont_VV(wf(:,7),wf(:,41)) * den(37)
  A(15) = cont_QA(wf(:,43),wf(:,44)) * den(40)
  A(16) = cont_VV(wf(:,9),wf(:,48)) * den(44)
  A(17) = cont_QA(wf(:,44),wf(:,49)) * den(45)
  A(18) = cont_VV(wf(:,33),wf(:,41)) * den(46)
  A(19) = cont_VV(wf(:,36),wf(:,41)) * den(47)
  A(20) = cont_VV(wf(:,20),wf(:,48)) * den(48)
  A(21) = cont_VV(wf(:,30),wf(:,48)) * den(49)

  A(22) = cont_VV(wf(:,5),wf(:,51)) * den(3)
  A(23) = cont_QA(wf(:,15),wf(:,52)) * den(13)
  A(24) = cont_QA(wf(:,19),wf(:,53)) * den(16)
  A(25) = cont_QA(wf(:,25),wf(:,54)) * den(22)
  A(26) = cont_QA(wf(:,29),wf(:,55)) * den(25)
  A(27) = cont_QA(wf(:,19),wf(:,56)) * den(28)
  A(28) = cont_QA(wf(:,29),wf(:,57)) * den(32)
  A(29) = cont_VV(wf(:,58),wf(:,59)) * den(51)
  A(30) = cont_QA(wf(:,60),wf(:,61)) * den(54)
  A(31) = cont_QA(wf(:,16),wf(:,63)) * den(13)
  A(32) = cont_VV(wf(:,8),wf(:,64)) * den(55)
  A(33) = cont_QA(wf(:,60),wf(:,65)) * den(57)
  A(34) = cont_QA(wf(:,29),wf(:,66)) * den(32)
  A(35) = cont_VV(wf(:,6),wf(:,68)) * den(34)
  A(36) = cont_QA(wf(:,26),wf(:,70)) * den(22)
  A(37) = cont_QA(wf(:,29),wf(:,71)) * den(25)
  A(38) = cont_VV(wf(:,10),wf(:,73)) * den(27)
  A(39) = cont_VV(wf(:,59),wf(:,74)) * den(58)
  A(40) = cont_QA(wf(:,75),wf(:,76)) * den(61)
  A(41) = cont_QA(wf(:,25),wf(:,79)) * den(22)
  A(42) = cont_VV(wf(:,8),wf(:,80)) * den(62)
  A(43) = cont_QA(wf(:,75),wf(:,81)) * den(64)
  A(44) = cont_QA(wf(:,19),wf(:,82)) * den(28)
  A(45) = cont_VV(wf(:,6),wf(:,84)) * den(31)
  A(46) = cont_QA(wf(:,15),wf(:,87)) * den(13)
  A(47) = cont_QA(wf(:,19),wf(:,88)) * den(16)
  A(48) = cont_VV(wf(:,10),wf(:,90)) * den(18)
  A(49) = cont_VV(wf(:,8),wf(:,91)) * den(6)
  A(50) = cont_VV(wf(:,10),wf(:,93)) * den(9)
  A(51) = cont_VV(wf(:,41),wf(:,64)) * den(46)
  A(52) = cont_VV(wf(:,41),wf(:,67)) * den(47)
  A(53) = cont_VV(wf(:,48),wf(:,58)) * den(48)
  A(54) = cont_VV(wf(:,48),wf(:,72)) * den(49)
  A(55) = cont_VV(wf(:,41),wf(:,80)) * den(47)
  A(56) = cont_VV(wf(:,41),wf(:,83)) * den(46)
  A(57) = cont_VV(wf(:,48),wf(:,74)) * den(49)
  A(58) = cont_VV(wf(:,48),wf(:,89)) * den(48)
  A(59) = cont_VV(wf(:,41),wf(:,91)) * den(37)
  A(60) = cont_QA(wf(:,43),wf(:,95)) * den(40)
  A(61) = cont_VV(wf(:,48),wf(:,92)) * den(44)
  A(62) = cont_QA(wf(:,49),wf(:,95)) * den(45)
  A(63) = cont_QA(wf(:,97),wf(:,98)) * den(68)
  A(64) = cont_QA(wf(:,99),wf(:,100)) * den(71)
  A(65) = cont_VV(wf(:,59),wf(:,102)) * den(73)
  A(66) = cont_QA(wf(:,19),wf(:,103)) * den(74)
  A(67) = cont_QA(wf(:,61),wf(:,104)) * den(75)
  A(68) = cont_QA(wf(:,106),wf(:,107)) * den(79)
  A(69) = cont_QA(wf(:,108),wf(:,109)) * den(82)
  A(70) = cont_VV(wf(:,59),wf(:,111)) * den(84)
  A(71) = cont_QA(wf(:,76),wf(:,112)) * den(85)
  A(72) = cont_QA(wf(:,29),wf(:,113)) * den(86)
  A(73) = cont_VV(wf(:,8),wf(:,115)) * den(88)
  A(74) = cont_QA(wf(:,19),wf(:,116)) * den(89)
  A(75) = cont_QA(wf(:,65),wf(:,104)) * den(90)
  A(76) = cont_VV(wf(:,8),wf(:,118)) * den(92)
  A(77) = cont_QA(wf(:,81),wf(:,112)) * den(93)
  A(78) = cont_QA(wf(:,29),wf(:,119)) * den(94)
  A(79) = cont_VV(wf(:,41),wf(:,115)) * den(95)
  A(80) = cont_VV(wf(:,41),wf(:,118)) * den(96)
  A(81) = cont_VV(wf(:,48),wf(:,102)) * den(97)
  A(82) = cont_VV(wf(:,48),wf(:,111)) * den(98)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(82)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(8)-A(12))*f(1)+(A(4)+A(7))*f(2)+(A(9)+A(13)-A(19)-A(21))*f(3)+(-A(5)-A(10))*f(4)+(-A(6)-A(11)+A(18)+A(20))*f(5) &
       +(A(1)-A(2)-A(3)+A(14)-A(15)+A(16)-A(17))*f(6)

  M2(1) = (A(71)+A(72)+A(77)+A(78))*f(7)+(-A(63)-A(64)-A(68)-A(69))*f(8)+(-A(70)-A(76)+A(80)+A(82))*f(9)+(A(66)+A(67)+A(74) &
       +A(75))*f(10)+(A(65)+A(73)-A(79)-A(81))*f(11)+(-A(26)-A(28)-A(34)-A(37)-A(40)-A(43))*f(12)+(A(23)+A(25)+A(31)+A(36)+A(41) &
       +A(46))*f(13)+(A(35)+A(38)+A(39)+A(42)-A(52)-A(54)-A(55)-A(57))*f(14)+(-A(24)-A(27)-A(30)-A(33)-A(44)-A(47))*f(15)+(-A(29) &
       -A(32)-A(45)-A(48)+A(51)+A(53)+A(56)+A(58))*f(16)+(A(22)-A(49)-A(50)+A(59)-A(60)+A(61)-A(62))*f(17)

end subroutine colourvectors

end module ol_loop_ppllaa_nexeudxaa_1_/**/REALKIND
