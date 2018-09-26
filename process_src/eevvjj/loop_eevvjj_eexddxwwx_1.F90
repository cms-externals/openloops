
module ol_colourmatrix_eevvjj_eexddxwwx_1_/**/REALKIND
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
end module ol_colourmatrix_eevvjj_eexddxwwx_1_/**/REALKIND



module ol_forced_parameters_eevvjj_eexddxwwx_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_eevvjj_eexddxwwx_1_/**/REALKIND

module ol_loop_eevvjj_eexddxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(45), c(12)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:204)
  ! denominators
  complex(REALKIND), save :: den(217)
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
    f( 1) = (CI*eQED**4)/9._/**/REALKIND
    f( 2) = (CI*eQED**4)/3._/**/REALKIND
    f( 3) = CI*eQED**4
    f( 4) = (CI*countertermnorm*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 6) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 7) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 8) = CI*countertermnorm*ctVqq*eQED**4*gQCD**2
    f( 9) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f(10) = (CI*countertermnorm*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f(11) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f(12) = (CI*cw*eQED**4)/(2._/**/REALKIND*sw**3)
    f(13) = (CI*countertermnorm*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(14) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(15) = (CI*eQED**4)/(6._/**/REALKIND*sw**2)
    f(16) = (CI*eQED**4)/(3._/**/REALKIND*sw**2)
    f(17) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f(18) = (CI*cw**2*eQED**4)/sw**2
    f(19) = (CI*countertermnorm*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(20) = (CI*countertermnorm*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(21) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(22) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(23) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(24) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(25) = (CI*countertermnorm*ctVqq*cw**2*eQED**4*gQCD**2)/sw**2
    f(26) = (CI*eQED**4*MW**2)/(cw**2*sw**2)
    f(27) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*MW**2)/(cw**2*sw**2)
    f(28) = (CI*cw*eQED**4)/(3._/**/REALKIND*sw)
    f(29) = (CI*cw*eQED**4)/sw
    f(30) = (CI*countertermnorm*cw*eQED**4*gQCD**2)/(3._/**/REALKIND*sw)
    f(31) = (CI*countertermnorm*cw*eQED**4*gQCD**2)/sw
    f(32) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**2)/(3._/**/REALKIND*sw)
    f(33) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**2)/sw
    f(34) = (eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(35) = (eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(36) = eQED**4*gQCD**2*integralnorm*SwB
    f(37) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(38) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(39) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(40) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(41) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(42) = (cw**2*eQED**4*gQCD**2*integralnorm*SwB)/sw**2
    f(43) = (eQED**4*gQCD**2*integralnorm*MW**2*SwB)/(cw**2*sw**2)
    f(44) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw*3._/**/REALKIND)
    f(45) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/sw

  c = [ 4*f(34), 4*f(35), 4*f(36), 4*f(37), 4*f(38), 4*f(39), 4*f(40), 4*f(41), 4*f(42), 4*f(43), 4*f(44), 4*f(45) ]
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
  complex(REALKIND) :: A(152)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rMW, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rMW, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_WWV_V(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,4))
  call prop_W_W(wf(:,4),Q(:,12),MZ,1_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,3),MZ,1_intkind1,wf(:,7))
  call vert_WWV_V(wf(:,-4),wf(:,-5),wf(:,7),wf(:,8))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,9))
  call vert_VV_S(wf(:,7),wf(:,5),wf(:,10))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,1),Q(:,3),wf(:,11))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,-5),Q(:,32),wf(:,12))
  call prop_W_W(wf(:,11),Q(:,19),MW,1_intkind1,wf(:,13))
  call vert_UV_W(wf(:,5),Q(:,12),wf(:,-5),Q(:,32),wf(:,14))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,7),Q(:,3),wf(:,15))
  call prop_W_W(wf(:,15),Q(:,19),MW,1_intkind1,wf(:,16))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,2),Q(:,12),wf(:,17))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,18))
  call prop_W_W(wf(:,17),Q(:,28),MW,1_intkind1,wf(:,19))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,5),Q(:,12),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,28),MW,1_intkind1,wf(:,21))
  call vert_UV_W(wf(:,7),Q(:,3),wf(:,-5),Q(:,32),wf(:,22))
  call vert_WQ_A(wf(:,-5),wf(:,-2),wf(:,23))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,36),ZERO,0_intkind1,wf(:,25))
  call prop_A_Q(wf(:,24),Q(:,24),ZERO,0_intkind1,wf(:,26))
  call vert_VQ_A(wf(:,1),wf(:,25),wf(:,27))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,25),wf(:,28))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,29))
  call vert_AW_Q(wf(:,26),wf(:,-5),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,7),ZERO,0_intkind1,wf(:,31))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-2),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,7),ZERO,0_intkind1,wf(:,33))
  call vert_QA_W(wf(:,-2),wf(:,26),wf(:,34))
  call prop_W_W(wf(:,34),Q(:,28),MW,1_intkind1,wf(:,35))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,36))
  call vert_WQ_A(wf(:,-4),wf(:,25),wf(:,37))
  call prop_A_Q(wf(:,36),Q(:,11),ZERO,0_intkind1,wf(:,38))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,7),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,11),ZERO,0_intkind1,wf(:,40))
  call vert_QA_W(wf(:,25),wf(:,-3),wf(:,41))
  call prop_W_W(wf(:,41),Q(:,44),MW,1_intkind1,wf(:,42))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,43))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,44))
  call prop_W_W(wf(:,43),Q(:,48),MZ,1_intkind1,wf(:,45))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,45),wf(:,46))
  call vert_VQ_A(wf(:,43),wf(:,-2),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,52),ZERO,0_intkind1,wf(:,48))
  call vert_ZQ_A(gZd,wf(:,45),wf(:,-2),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,52),ZERO,0_intkind1,wf(:,50))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,51))
  call vert_AW_Q(wf(:,-1),wf(:,-4),wf(:,52))
  call prop_Q_A(wf(:,51),Q(:,33),ZERO,0_intkind1,wf(:,53))
  call prop_A_Q(wf(:,52),Q(:,18),ZERO,0_intkind1,wf(:,54))
  call vert_QA_Z(gZn,wf(:,53),wf(:,54),wf(:,55))
  call vert_QA_W(wf(:,0),wf(:,54),wf(:,56))
  call prop_W_W(wf(:,56),Q(:,19),MW,1_intkind1,wf(:,57))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,58))
  call vert_AW_Q(wf(:,54),wf(:,-5),wf(:,59))
  call prop_Q_A(wf(:,58),Q(:,13),ZERO,0_intkind1,wf(:,60))
  call vert_ZQ_A(gZl,wf(:,5),wf(:,0),wf(:,61))
  call prop_Q_A(wf(:,61),Q(:,13),ZERO,0_intkind1,wf(:,62))
  call vert_QA_W(wf(:,53),wf(:,-1),wf(:,63))
  call prop_W_W(wf(:,63),Q(:,35),MW,1_intkind1,wf(:,64))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,65))
  call vert_WQ_A(wf(:,-4),wf(:,53),wf(:,66))
  call prop_A_Q(wf(:,65),Q(:,14),ZERO,0_intkind1,wf(:,67))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,5),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,14),ZERO,0_intkind1,wf(:,69))
  call vert_AV_Q(wf(:,-1),wf(:,43),wf(:,70))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,45),wf(:,71))
  call vert_VQ_A(wf(:,43),wf(:,0),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,49),ZERO,0_intkind1,wf(:,73))
  call vert_ZQ_A(gZl,wf(:,45),wf(:,0),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,49),ZERO,0_intkind1,wf(:,75))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,76))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,77))
  call prop_W_W(wf(:,77),Q(:,12),MZ,1_intkind1,wf(:,78))
  call counter_VQ_A(wf(:,1),wf(:,25),wf(:,79))
  call counter_ZQ_A(gZu,wf(:,7),wf(:,25),wf(:,80))
  call counter_AW_Q(wf(:,26),wf(:,-5),wf(:,81))
  call counter_WQ_A(wf(:,-4),wf(:,25),wf(:,82))
  call counter_QA_W(wf(:,25),wf(:,-3),wf(:,83))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,84))
  call prop_Q_A(wf(:,37),Q(:,52),ZERO,0_intkind1,wf(:,85))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,7),wf(:,86))
  call counter_AV_Q(wf(:,-3),wf(:,43),wf(:,87))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,45),wf(:,88))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,89))
  call prop_A_Q(wf(:,89),Q(:,24),ZERO,0_intkind1,wf(:,90))
  call vert_AW_Q(wf(:,90),wf(:,-5),wf(:,91))
  call vert_QA_W(wf(:,-2),wf(:,90),wf(:,92))
  call prop_W_W(wf(:,92),Q(:,28),MW,1_intkind1,wf(:,93))
  call counter_QA_W(wf(:,-2),wf(:,26),wf(:,94))
  call prop_W_W(wf(:,18),Q(:,35),MW,1_intkind1,wf(:,95))
  call prop_W_W(wf(:,22),Q(:,35),MW,1_intkind1,wf(:,96))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,97))
  call prop_A_Q(wf(:,30),Q(:,56),ZERO,0_intkind1,wf(:,98))
  call counter_ZQ_A(gZd,wf(:,7),wf(:,-2),wf(:,99))
  call counter_WQ_A(wf(:,-5),wf(:,-2),wf(:,100))
  call prop_Q_A(wf(:,100),Q(:,36),ZERO,0_intkind1,wf(:,101))
  call vert_VQ_A(wf(:,1),wf(:,101),wf(:,102))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,101),wf(:,103))
  call counter_VQ_A(wf(:,43),wf(:,-2),wf(:,104))
  call counter_ZQ_A(gZd,wf(:,45),wf(:,-2),wf(:,105))
  call prop_A_Q(wf(:,44),Q(:,56),ZERO,0_intkind1,wf(:,106))
  call prop_A_Q(wf(:,46),Q(:,56),ZERO,0_intkind1,wf(:,107))
  call vert_WQ_A(wf(:,-4),wf(:,101),wf(:,108))
  call vert_QA_W(wf(:,101),wf(:,-3),wf(:,109))
  call prop_W_W(wf(:,109),Q(:,44),MW,1_intkind1,wf(:,110))
  call vert_VV_S(wf(:,7),wf(:,78),wf(:,111))
  call vert_UV_W(wf(:,76),Q(:,12),wf(:,-5),Q(:,32),wf(:,112))
  call vert_UV_W(wf(:,78),Q(:,12),wf(:,-5),Q(:,32),wf(:,113))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,76),Q(:,12),wf(:,114))
  call prop_W_W(wf(:,114),Q(:,28),MW,1_intkind1,wf(:,115))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,78),Q(:,12),wf(:,116))
  call prop_W_W(wf(:,116),Q(:,28),MW,1_intkind1,wf(:,117))
  call vert_VQ_A(wf(:,76),wf(:,0),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,13),ZERO,0_intkind1,wf(:,119))
  call vert_ZQ_A(gZl,wf(:,78),wf(:,0),wf(:,120))
  call prop_Q_A(wf(:,120),Q(:,13),ZERO,0_intkind1,wf(:,121))
  call vert_AV_Q(wf(:,-1),wf(:,76),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,14),ZERO,0_intkind1,wf(:,123))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,78),wf(:,124))
  call prop_A_Q(wf(:,124),Q(:,14),ZERO,0_intkind1,wf(:,125))
  call vert_AV_Q(wf(:,26),wf(:,1),wf(:,126))
  call counter_Q_A(ctqq,wf(:,25),Q(:,36),wf(:,127))
  call prop_A_Q(wf(:,126),Q(:,27),ZERO,0_intkind1,wf(:,128))
  call vert_AZ_Q(gZu,wf(:,26),wf(:,7),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,27),ZERO,0_intkind1,wf(:,130))
  call counter_A_Q(ctqq,wf(:,26),Q(:,24),wf(:,131))
  call prop_Q_A(wf(:,27),Q(:,39),ZERO,0_intkind1,wf(:,132))
  call prop_Q_A(wf(:,28),Q(:,39),ZERO,0_intkind1,wf(:,133))
  call prop_A_Q(wf(:,131),Q(:,24),ZERO,0_intkind1,wf(:,134))
  call vert_QA_W(wf(:,-2),wf(:,134),wf(:,135))
  call counter_Q_A(ctqq,wf(:,31),Q(:,7),wf(:,136))
  call counter_Q_A(ctqq,wf(:,33),Q(:,7),wf(:,137))
  call vert_AW_Q(wf(:,134),wf(:,-5),wf(:,138))
  call prop_Q_A(wf(:,127),Q(:,36),ZERO,0_intkind1,wf(:,139))
  call vert_QA_W(wf(:,139),wf(:,-3),wf(:,140))
  call vert_WQ_A(wf(:,-4),wf(:,139),wf(:,141))
  call counter_A_Q(ctqq,wf(:,38),Q(:,11),wf(:,142))
  call counter_A_Q(ctqq,wf(:,40),Q(:,11),wf(:,143))
  call counter_Q_A(ctqq,wf(:,48),Q(:,52),wf(:,144))
  call counter_Q_A(ctqq,wf(:,50),Q(:,52),wf(:,145))
  call prop_W_W(wf(:,3),Q(:,51),MZ,1_intkind1,wf(:,146))
  call prop_W_W(wf(:,8),Q(:,51),MZ,1_intkind1,wf(:,147))
  call vert_SV_V(wf(:,9),wf(:,7),wf(:,148))
  call prop_W_W(wf(:,148),Q(:,51),MZ,1_intkind1,wf(:,149))
  call vert_WQ_A(wf(:,-5),wf(:,31),wf(:,150))
  call prop_Q_A(wf(:,150),Q(:,39),ZERO,0_intkind1,wf(:,151))
  call vert_WQ_A(wf(:,-5),wf(:,33),wf(:,152))
  call prop_Q_A(wf(:,152),Q(:,39),ZERO,0_intkind1,wf(:,153))
  call vert_AW_Q(wf(:,38),wf(:,-4),wf(:,154))
  call prop_A_Q(wf(:,154),Q(:,27),ZERO,0_intkind1,wf(:,155))
  call vert_AW_Q(wf(:,40),wf(:,-4),wf(:,156))
  call prop_A_Q(wf(:,156),Q(:,27),ZERO,0_intkind1,wf(:,157))
  call vert_AW_Q(wf(:,-3),wf(:,13),wf(:,158))
  call prop_A_Q(wf(:,158),Q(:,27),ZERO,0_intkind1,wf(:,159))
  call vert_AW_Q(wf(:,-3),wf(:,16),wf(:,160))
  call prop_A_Q(wf(:,160),Q(:,27),ZERO,0_intkind1,wf(:,161))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,13),Q(:,19),wf(:,162))
  call prop_W_W(wf(:,162),Q(:,51),MZ,1_intkind1,wf(:,163))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,16),Q(:,19),wf(:,164))
  call prop_W_W(wf(:,164),Q(:,51),MZ,1_intkind1,wf(:,165))
  call vert_WQ_A(wf(:,95),wf(:,-2),wf(:,166))
  call prop_Q_A(wf(:,166),Q(:,39),ZERO,0_intkind1,wf(:,167))
  call vert_WQ_A(wf(:,96),wf(:,-2),wf(:,168))
  call prop_Q_A(wf(:,168),Q(:,39),ZERO,0_intkind1,wf(:,169))
  call vert_UV_W(wf(:,95),Q(:,35),wf(:,-4),Q(:,16),wf(:,170))
  call prop_W_W(wf(:,170),Q(:,51),MZ,1_intkind1,wf(:,171))
  call vert_UV_W(wf(:,96),Q(:,35),wf(:,-4),Q(:,16),wf(:,172))
  call prop_W_W(wf(:,172),Q(:,51),MZ,1_intkind1,wf(:,173))
  call prop_W_W(wf(:,55),Q(:,51),MZ,1_intkind1,wf(:,174))
  call vert_AW_Q(wf(:,-3),wf(:,57),wf(:,175))
  call prop_A_Q(wf(:,175),Q(:,27),ZERO,0_intkind1,wf(:,176))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,57),Q(:,19),wf(:,177))
  call prop_W_W(wf(:,177),Q(:,51),MZ,1_intkind1,wf(:,178))
  call prop_A_Q(wf(:,59),Q(:,50),ZERO,0_intkind1,wf(:,179))
  call vert_QA_V(wf(:,0),wf(:,179),wf(:,180))
  call vert_QA_Z(gZl,wf(:,0),wf(:,179),wf(:,181))
  call prop_W_W(wf(:,181),Q(:,51),MZ,1_intkind1,wf(:,182))
  call vert_WQ_A(wf(:,64),wf(:,-2),wf(:,183))
  call prop_Q_A(wf(:,183),Q(:,39),ZERO,0_intkind1,wf(:,184))
  call vert_UV_W(wf(:,64),Q(:,35),wf(:,-4),Q(:,16),wf(:,185))
  call prop_W_W(wf(:,185),Q(:,51),MZ,1_intkind1,wf(:,186))
  call prop_Q_A(wf(:,66),Q(:,49),ZERO,0_intkind1,wf(:,187))
  call vert_QA_V(wf(:,187),wf(:,-1),wf(:,188))
  call vert_QA_Z(gZl,wf(:,187),wf(:,-1),wf(:,189))
  call prop_W_W(wf(:,189),Q(:,51),MZ,1_intkind1,wf(:,190))
  call vert_QA_V(wf(:,73),wf(:,-1),wf(:,191))
  call vert_QA_Z(gZl,wf(:,73),wf(:,-1),wf(:,192))
  call prop_W_W(wf(:,192),Q(:,51),MZ,1_intkind1,wf(:,193))
  call vert_QA_V(wf(:,75),wf(:,-1),wf(:,194))
  call vert_QA_Z(gZl,wf(:,75),wf(:,-1),wf(:,195))
  call prop_W_W(wf(:,195),Q(:,51),MZ,1_intkind1,wf(:,196))
  call prop_A_Q(wf(:,70),Q(:,50),ZERO,0_intkind1,wf(:,197))
  call vert_QA_V(wf(:,0),wf(:,197),wf(:,198))
  call prop_A_Q(wf(:,71),Q(:,50),ZERO,0_intkind1,wf(:,199))
  call vert_QA_V(wf(:,0),wf(:,199),wf(:,200))
  call vert_QA_Z(gZl,wf(:,0),wf(:,197),wf(:,201))
  call prop_W_W(wf(:,201),Q(:,51),MZ,1_intkind1,wf(:,202))
  call vert_QA_Z(gZl,wf(:,0),wf(:,199),wf(:,203))
  call prop_W_W(wf(:,203),Q(:,51),MZ,1_intkind1,wf(:,204))

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
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,12) - MZ2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,48) - MH2)
  den(11) = 1 / (Q(5,19) - MW2)
  den(18) = 1 / (Q(5,28) - MW2)
  den(25) = 1 / (Q(5,36))
  den(26) = 1 / (Q(5,24))
  den(31) = 1 / (Q(5,7))
  den(39) = 1 / (Q(5,11))
  den(44) = 1 / (Q(5,44) - MW2)
  den(48) = 1 / (Q(5,48))
  den(50) = 1 / (Q(5,48) - MZ2)
  den(54) = 1 / (Q(5,52))
  den(61) = 1 / (Q(5,33))
  den(62) = 1 / (Q(5,18))
  den(68) = 1 / (Q(5,13))
  den(73) = 1 / (Q(5,35) - MW2)
  den(77) = 1 / (Q(5,14))
  den(86) = 1 / (Q(5,49))
  den(104) = 1 / (Q(5,56))
  den(119) = 1 / (Q(5,27))
  den(125) = 1 / (Q(5,39))
  den(154) = 1 / (Q(5,51))
  den(156) = 1 / (Q(5,51) - MZ2)
  den(182) = 1 / (Q(5,50))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(4)*den(6)
  den(10) = den(8)*den(9)
  den(12) = den(1)*den(11)
  den(13) = den(2)*den(12)
  den(14) = den(4)*den(12)
  den(15) = den(6)*den(11)
  den(16) = den(2)*den(15)
  den(17) = den(4)*den(15)
  den(19) = den(2)*den(18)
  den(20) = den(1)*den(19)
  den(21) = den(4)*den(18)
  den(22) = den(1)*den(21)
  den(23) = den(6)*den(19)
  den(24) = den(6)*den(21)
  den(27) = den(1)*den(25)
  den(28) = den(26)*den(27)
  den(29) = den(6)*den(25)
  den(30) = den(26)*den(29)
  den(32) = den(1)*den(31)
  den(33) = den(26)*den(32)
  den(34) = den(6)*den(31)
  den(35) = den(26)*den(34)
  den(36) = den(18)*den(26)
  den(37) = den(1)*den(36)
  den(38) = den(6)*den(36)
  den(40) = den(1)*den(39)
  den(41) = den(25)*den(40)
  den(42) = den(6)*den(39)
  den(43) = den(25)*den(42)
  den(45) = den(25)*den(44)
  den(46) = den(1)*den(45)
  den(47) = den(6)*den(45)
  den(49) = den(32)*den(48)
  den(51) = den(32)*den(50)
  den(52) = den(34)*den(48)
  den(53) = den(34)*den(50)
  den(55) = den(48)*den(54)
  den(56) = den(1)*den(55)
  den(57) = den(50)*den(54)
  den(58) = den(1)*den(57)
  den(59) = den(6)*den(55)
  den(60) = den(6)*den(57)
  den(63) = den(61)*den(62)
  den(64) = den(4)*den(63)
  den(65) = den(11)*den(62)
  den(66) = den(2)*den(65)
  den(67) = den(4)*den(65)
  den(69) = den(2)*den(68)
  den(70) = den(62)*den(69)
  den(71) = den(4)*den(68)
  den(72) = den(62)*den(71)
  den(74) = den(61)*den(73)
  den(75) = den(2)*den(74)
  den(76) = den(4)*den(74)
  den(78) = den(2)*den(77)
  den(79) = den(61)*den(78)
  den(80) = den(4)*den(77)
  den(81) = den(61)*den(80)
  den(82) = den(48)*den(69)
  den(83) = den(50)*den(69)
  den(84) = den(48)*den(71)
  den(85) = den(50)*den(71)
  den(87) = den(48)*den(86)
  den(88) = den(2)*den(87)
  den(89) = den(4)*den(87)
  den(90) = den(50)*den(86)
  den(91) = den(2)*den(90)
  den(92) = den(4)*den(90)
  den(93) = den(25)*den(65)
  den(94) = den(26)*den(74)
  den(95) = den(12)*den(25)
  den(96) = den(15)*den(25)
  den(97) = den(25)*den(54)
  den(98) = den(1)*den(97)
  den(99) = den(6)*den(97)
  den(100) = den(1)*den(73)
  den(101) = den(26)*den(100)
  den(102) = den(6)*den(73)
  den(103) = den(26)*den(102)
  den(105) = den(26)*den(104)
  den(106) = den(1)*den(105)
  den(107) = den(6)*den(105)
  den(108) = den(40)*den(48)
  den(109) = den(40)*den(50)
  den(110) = den(42)*den(48)
  den(111) = den(42)*den(50)
  den(112) = den(48)*den(104)
  den(113) = den(1)*den(112)
  den(114) = den(50)*den(104)
  den(115) = den(1)*den(114)
  den(116) = den(6)*den(112)
  den(117) = den(6)*den(114)
  den(118) = den(1)*den(26)
  den(120) = den(118)*den(119)
  den(121) = den(25)*den(120)
  den(122) = den(6)*den(26)
  den(123) = den(119)*den(122)
  den(124) = den(25)*den(123)
  den(126) = den(27)*den(125)
  den(127) = den(26)*den(126)
  den(128) = den(29)*den(125)
  den(129) = den(26)*den(128)
  den(130) = den(26)**2
  den(131) = den(100)*den(130)
  den(132) = den(102)*den(130)
  den(133) = den(32)*den(105)
  den(134) = den(34)*den(105)
  den(135) = den(32)*den(130)
  den(136) = den(34)*den(130)
  den(137) = den(25)**2
  den(138) = den(12)*den(137)
  den(139) = den(15)*den(137)
  den(140) = den(40)*den(137)
  den(141) = den(42)*den(137)
  den(142) = den(40)*den(97)
  den(143) = den(42)*den(97)
  den(144) = den(32)*den(112)
  den(145) = den(32)*den(114)
  den(146) = den(34)*den(112)
  den(147) = den(34)*den(114)
  den(148) = den(40)*den(55)
  den(149) = den(40)*den(57)
  den(150) = den(42)*den(55)
  den(151) = den(42)*den(57)
  den(152) = den(65)*den(137)
  den(153) = den(74)*den(130)
  den(155) = den(1)*den(154)
  den(157) = den(1)*den(156)
  den(158) = den(6)*den(154)
  den(159) = den(6)*den(156)
  den(160) = den(6)*den(9)
  den(161) = den(156)*den(160)
  den(162) = den(32)*den(125)
  den(163) = den(34)*den(125)
  den(164) = den(40)*den(119)
  den(165) = den(42)*den(119)
  den(166) = den(12)*den(119)
  den(167) = den(15)*den(119)
  den(168) = den(12)*den(154)
  den(169) = den(12)*den(156)
  den(170) = den(15)*den(154)
  den(171) = den(15)*den(156)
  den(172) = den(100)*den(125)
  den(173) = den(102)*den(125)
  den(174) = den(100)*den(154)
  den(175) = den(100)*den(156)
  den(176) = den(102)*den(154)
  den(177) = den(102)*den(156)
  den(178) = den(63)*den(156)
  den(179) = den(65)*den(119)
  den(180) = den(65)*den(154)
  den(181) = den(65)*den(156)
  den(183) = den(62)*den(182)
  den(184) = den(154)*den(183)
  den(185) = den(156)*den(183)
  den(186) = den(74)*den(125)
  den(187) = den(74)*den(154)
  den(188) = den(74)*den(156)
  den(189) = den(61)*den(86)
  den(190) = den(154)*den(189)
  den(191) = den(156)*den(189)
  den(192) = den(87)*den(154)
  den(193) = den(87)*den(156)
  den(194) = den(90)*den(154)
  den(195) = den(90)*den(156)
  den(196) = den(48)*den(182)
  den(197) = den(154)*den(196)
  den(198) = den(50)*den(182)
  den(199) = den(154)*den(198)
  den(200) = den(156)*den(196)
  den(201) = den(156)*den(198)
  den(202) = den(1)*den(25)*den(26)
  den(203) = den(6)*den(25)*den(26)
  den(204) = den(1)*den(48)
  den(205) = den(1)*den(50)
  den(206) = den(6)*den(48)
  den(207) = den(6)*den(50)
  den(208) = den(26)*den(162)
  den(209) = den(26)*den(163)
  den(210) = den(26)*den(172)
  den(211) = den(26)*den(173)
  den(212) = den(25)*den(164)
  den(213) = den(25)*den(165)
  den(214) = den(25)*den(166)
  den(215) = den(25)*den(167)
  den(216) = den(25)*den(179)
  den(217) = den(26)*den(186)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(152)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(3) = cont_VV(wf(:,2),wf(:,8)) * den(7)
  A(4) = cont_VV(wf(:,5),wf(:,8)) * den(8)
  A(5) = cont_SS(wf(:,9),wf(:,10)) * den(10)
  A(6) = cont_VV(wf(:,12),wf(:,13)) * den(13)
  A(7) = cont_VV(wf(:,13),wf(:,14)) * den(14)
  A(8) = cont_VV(wf(:,12),wf(:,16)) * den(16)
  A(9) = cont_VV(wf(:,14),wf(:,16)) * den(17)
  A(10) = cont_VV(wf(:,18),wf(:,19)) * den(20)
  A(11) = cont_VV(wf(:,18),wf(:,21)) * den(22)
  A(12) = cont_VV(wf(:,19),wf(:,22)) * den(23)
  A(13) = cont_VV(wf(:,21),wf(:,22)) * den(24)
  A(14) = cont_QA(wf(:,26),wf(:,27)) * den(28)
  A(15) = cont_QA(wf(:,26),wf(:,28)) * den(30)
  A(16) = cont_QA(wf(:,30),wf(:,31)) * den(33)
  A(17) = cont_QA(wf(:,30),wf(:,33)) * den(35)
  A(18) = cont_VV(wf(:,18),wf(:,35)) * den(37)
  A(19) = cont_VV(wf(:,22),wf(:,35)) * den(38)
  A(20) = cont_QA(wf(:,37),wf(:,38)) * den(41)
  A(21) = cont_QA(wf(:,37),wf(:,40)) * den(43)
  A(22) = cont_VV(wf(:,11),wf(:,42)) * den(46)
  A(23) = cont_VV(wf(:,15),wf(:,42)) * den(47)
  A(24) = cont_QA(wf(:,31),wf(:,44)) * den(49)
  A(25) = cont_QA(wf(:,31),wf(:,46)) * den(51)
  A(26) = cont_QA(wf(:,33),wf(:,44)) * den(52)
  A(27) = cont_QA(wf(:,33),wf(:,46)) * den(53)
  A(28) = cont_QA(wf(:,36),wf(:,48)) * den(56)
  A(29) = cont_QA(wf(:,36),wf(:,50)) * den(58)
  A(30) = cont_QA(wf(:,39),wf(:,48)) * den(59)
  A(31) = cont_QA(wf(:,39),wf(:,50)) * den(60)
  A(32) = cont_VV(wf(:,5),wf(:,55)) * den(64)
  A(33) = cont_VV(wf(:,12),wf(:,57)) * den(66)
  A(34) = cont_VV(wf(:,14),wf(:,57)) * den(67)
  A(35) = cont_QA(wf(:,59),wf(:,60)) * den(70)
  A(36) = cont_QA(wf(:,59),wf(:,62)) * den(72)
  A(37) = cont_VV(wf(:,17),wf(:,64)) * den(75)
  A(38) = cont_VV(wf(:,20),wf(:,64)) * den(76)
  A(39) = cont_QA(wf(:,66),wf(:,67)) * den(79)
  A(40) = cont_QA(wf(:,66),wf(:,69)) * den(81)
  A(41) = cont_QA(wf(:,60),wf(:,70)) * den(82)
  A(42) = cont_QA(wf(:,60),wf(:,71)) * den(83)
  A(43) = cont_QA(wf(:,62),wf(:,70)) * den(84)
  A(44) = cont_QA(wf(:,62),wf(:,71)) * den(85)
  A(45) = cont_QA(wf(:,65),wf(:,73)) * den(88)
  A(46) = cont_QA(wf(:,68),wf(:,73)) * den(89)
  A(47) = cont_QA(wf(:,65),wf(:,75)) * den(91)
  A(48) = cont_QA(wf(:,68),wf(:,75)) * den(92)
  A(49) = cont_VV(wf(:,41),wf(:,57)) * den(93)
  A(50) = cont_VV(wf(:,34),wf(:,64)) * den(94)

  A(51) = cont_VV(wf(:,3),wf(:,76)) * den(3)
  A(52) = cont_VV(wf(:,3),wf(:,78)) * den(5)
  A(53) = cont_VV(wf(:,8),wf(:,76)) * den(7)
  A(54) = cont_VV(wf(:,8),wf(:,78)) * den(8)
  A(55) = cont_QA(wf(:,26),wf(:,79)) * den(28)
  A(56) = cont_QA(wf(:,26),wf(:,80)) * den(30)
  A(57) = cont_QA(wf(:,31),wf(:,81)) * den(33)
  A(58) = cont_QA(wf(:,33),wf(:,81)) * den(35)
  A(59) = cont_QA(wf(:,38),wf(:,82)) * den(41)
  A(60) = cont_QA(wf(:,40),wf(:,82)) * den(43)
  A(61) = cont_VV(wf(:,13),wf(:,83)) * den(95)
  A(62) = cont_VV(wf(:,16),wf(:,83)) * den(96)
  A(63) = cont_QA(wf(:,84),wf(:,85)) * den(98)
  A(64) = cont_QA(wf(:,85),wf(:,86)) * den(99)
  A(65) = cont_QA(wf(:,31),wf(:,87)) * den(49)
  A(66) = cont_QA(wf(:,31),wf(:,88)) * den(51)
  A(67) = cont_QA(wf(:,33),wf(:,87)) * den(52)
  A(68) = cont_QA(wf(:,33),wf(:,88)) * den(53)
  A(69) = cont_QA(wf(:,48),wf(:,84)) * den(56)
  A(70) = cont_QA(wf(:,50),wf(:,84)) * den(58)
  A(71) = cont_QA(wf(:,48),wf(:,86)) * den(59)
  A(72) = cont_QA(wf(:,50),wf(:,86)) * den(60)
  A(73) = cont_QA(wf(:,27),wf(:,90)) * den(28)
  A(74) = cont_QA(wf(:,28),wf(:,90)) * den(30)
  A(75) = cont_QA(wf(:,31),wf(:,91)) * den(33)
  A(76) = cont_QA(wf(:,33),wf(:,91)) * den(35)
  A(77) = cont_VV(wf(:,18),wf(:,93)) * den(37)
  A(78) = cont_VV(wf(:,22),wf(:,93)) * den(38)
  A(79) = cont_VV(wf(:,94),wf(:,95)) * den(101)
  A(80) = cont_VV(wf(:,94),wf(:,96)) * den(103)
  A(81) = cont_QA(wf(:,97),wf(:,98)) * den(106)
  A(82) = cont_QA(wf(:,98),wf(:,99)) * den(107)
  A(83) = cont_QA(wf(:,26),wf(:,102)) * den(28)
  A(84) = cont_QA(wf(:,26),wf(:,103)) * den(30)
  A(85) = cont_QA(wf(:,38),wf(:,104)) * den(108)
  A(86) = cont_QA(wf(:,38),wf(:,105)) * den(109)
  A(87) = cont_QA(wf(:,40),wf(:,104)) * den(110)
  A(88) = cont_QA(wf(:,40),wf(:,105)) * den(111)
  A(89) = cont_QA(wf(:,97),wf(:,106)) * den(113)
  A(90) = cont_QA(wf(:,97),wf(:,107)) * den(115)
  A(91) = cont_QA(wf(:,99),wf(:,106)) * den(116)
  A(92) = cont_QA(wf(:,99),wf(:,107)) * den(117)
  A(93) = cont_QA(wf(:,38),wf(:,108)) * den(41)
  A(94) = cont_QA(wf(:,40),wf(:,108)) * den(43)
  A(95) = cont_VV(wf(:,11),wf(:,110)) * den(46)
  A(96) = cont_VV(wf(:,15),wf(:,110)) * den(47)
  A(97) = cont_SS(wf(:,9),wf(:,111)) * den(10)
  A(98) = cont_VV(wf(:,13),wf(:,112)) * den(13)
  A(99) = cont_VV(wf(:,13),wf(:,113)) * den(14)
  A(100) = cont_VV(wf(:,16),wf(:,112)) * den(16)
  A(101) = cont_VV(wf(:,16),wf(:,113)) * den(17)
  A(102) = cont_VV(wf(:,18),wf(:,115)) * den(20)
  A(103) = cont_VV(wf(:,18),wf(:,117)) * den(22)
  A(104) = cont_VV(wf(:,22),wf(:,115)) * den(23)
  A(105) = cont_VV(wf(:,22),wf(:,117)) * den(24)
  A(106) = cont_VV(wf(:,57),wf(:,83)) * den(93)
  A(107) = cont_VV(wf(:,64),wf(:,92)) * den(94)
  A(108) = cont_VV(wf(:,57),wf(:,109)) * den(93)
  A(109) = cont_VV(wf(:,64),wf(:,94)) * den(94)
  A(110) = cont_VV(wf(:,55),wf(:,78)) * den(64)
  A(111) = cont_VV(wf(:,57),wf(:,112)) * den(66)
  A(112) = cont_VV(wf(:,57),wf(:,113)) * den(67)
  A(113) = cont_QA(wf(:,59),wf(:,119)) * den(70)
  A(114) = cont_QA(wf(:,59),wf(:,121)) * den(72)
  A(115) = cont_VV(wf(:,64),wf(:,114)) * den(75)
  A(116) = cont_VV(wf(:,64),wf(:,116)) * den(76)
  A(117) = cont_QA(wf(:,66),wf(:,123)) * den(79)
  A(118) = cont_QA(wf(:,66),wf(:,125)) * den(81)
  A(119) = cont_QA(wf(:,73),wf(:,122)) * den(88)
  A(120) = cont_QA(wf(:,73),wf(:,124)) * den(89)
  A(121) = cont_QA(wf(:,75),wf(:,122)) * den(91)
  A(122) = cont_QA(wf(:,75),wf(:,124)) * den(92)
  A(123) = cont_QA(wf(:,70),wf(:,119)) * den(82)
  A(124) = cont_QA(wf(:,71),wf(:,119)) * den(83)
  A(125) = cont_QA(wf(:,70),wf(:,121)) * den(84)
  A(126) = cont_QA(wf(:,71),wf(:,121)) * den(85)
  A(127) = cont_QA(wf(:,127),wf(:,128)) * den(121)
  A(128) = cont_QA(wf(:,127),wf(:,130)) * den(124)
  A(129) = cont_QA(wf(:,131),wf(:,132)) * den(127)
  A(130) = cont_QA(wf(:,131),wf(:,133)) * den(129)
  A(131) = cont_VV(wf(:,95),wf(:,135)) * den(131)
  A(132) = cont_VV(wf(:,96),wf(:,135)) * den(132)
  A(133) = cont_QA(wf(:,98),wf(:,136)) * den(133)
  A(134) = cont_QA(wf(:,98),wf(:,137)) * den(134)
  A(135) = cont_QA(wf(:,31),wf(:,138)) * den(135)
  A(136) = cont_QA(wf(:,33),wf(:,138)) * den(136)
  A(137) = cont_VV(wf(:,13),wf(:,140)) * den(138)
  A(138) = cont_VV(wf(:,16),wf(:,140)) * den(139)
  A(139) = cont_QA(wf(:,38),wf(:,141)) * den(140)
  A(140) = cont_QA(wf(:,40),wf(:,141)) * den(141)
  A(141) = cont_QA(wf(:,85),wf(:,142)) * den(142)
  A(142) = cont_QA(wf(:,85),wf(:,143)) * den(143)
  A(143) = cont_QA(wf(:,106),wf(:,136)) * den(144)
  A(144) = cont_QA(wf(:,107),wf(:,136)) * den(145)
  A(145) = cont_QA(wf(:,106),wf(:,137)) * den(146)
  A(146) = cont_QA(wf(:,107),wf(:,137)) * den(147)
  A(147) = cont_QA(wf(:,38),wf(:,144)) * den(148)
  A(148) = cont_QA(wf(:,38),wf(:,145)) * den(149)
  A(149) = cont_QA(wf(:,40),wf(:,144)) * den(150)
  A(150) = cont_QA(wf(:,40),wf(:,145)) * den(151)
  A(151) = cont_VV(wf(:,57),wf(:,140)) * den(152)
  A(152) = cont_VV(wf(:,64),wf(:,135)) * den(153)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(152)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(24)-A(28))*f(1)+(-A(1)+A(6)+A(10)-A(26)-A(30)-A(41)-A(45))*f(2)+(-A(43)-A(46))*f(3)+(A(49)+A(50))*f(9)+(A(19)+A(23) &
       +A(34)+A(38))*f(12)+(A(16)+A(20)-A(33)+A(35)-A(37)+A(39))*f(15)-A(14)*f(16)+(A(15)+A(17)-A(18)+A(21)-A(22)+A(32)+A(36) &
       +A(40))*f(17)+(-A(4)+A(9)+A(13))*f(18)-A(5)*f(26)+(A(3)-A(8)-A(12)+A(25)+A(29)+A(42)+A(47))*f(28)+(A(2)-A(7)-A(11)+A(27) &
       +A(31)+A(44)+A(48))*f(29)

  M2(1) = (A(143)+A(147))*f(4)+(A(145)+A(149))*f(5)+(-A(65)-A(69)-A(85)-A(89))*f(6)+(-A(51)-A(67)-A(71)-A(87)-A(91)+A(98)+A(102) &
       -A(119)-A(123))*f(7)+(-A(120)-A(125))*f(8)+(-A(151)-A(152))*f(10)+(A(106)+A(107)+A(108)+A(109))*f(11)+(-A(132) &
       -A(138))*f(13)+(A(62)+A(78)+A(80)+A(96)+A(112)+A(116))*f(14)+(-A(133)-A(135)-A(139)-A(141))*f(19)+(A(127)+A(129))*f(20)+( &
       -A(128)-A(130)+A(131)-A(134)-A(136)+A(137)-A(140)-A(142))*f(21)+(A(57)+A(59)+A(63)+A(75)+A(81)+A(93)-A(111)+A(113)-A(115) &
       +A(117))*f(22)+(-A(55)-A(73)-A(83))*f(23)+(A(56)+A(58)+A(60)-A(61)+A(64)+A(74)+A(76)-A(77)-A(79)+A(82)+A(84)+A(94)-A(95) &
       +A(110)+A(114)+A(118))*f(24)+(-A(54)+A(101)+A(105))*f(25)-A(97)*f(27)+(-A(144)-A(148))*f(30)+(-A(146)-A(150))*f(31)+(A(53) &
       +A(66)+A(70)+A(86)+A(90)-A(100)-A(104)+A(121)+A(124))*f(32)+(A(52)+A(68)+A(72)+A(88)+A(92)-A(99)-A(103)+A(122) &
       +A(126))*f(33)

end subroutine colourvectors

end module ol_loop_eevvjj_eexddxwwx_1_/**/REALKIND
