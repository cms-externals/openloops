
module ol_colourmatrix_pptllj_eexuxtdbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  12]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   0]
  K1(11,:) = [  12]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [ -12]
  K1(15,:) = [   0]
  K1(16,:) = [  12]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [   0]
  K1(20,:) = [ -12]
  K1(21,:) = [   0]
  K1(22,:) = [  12]
  K1(23,:) = [   0]

  K2(1,:) = [ 9]

  KL(1,:) = [ 9, 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pptllj_eexuxtdbx_1_/**/REALKIND



module ol_forced_parameters_pptllj_eexuxtdbx_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wME /= 0) write(*,101) 'wME = 0'
  if (wME /= 0) write(*,101) 'wME = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pptllj_eexuxtdbx_1_/**/REALKIND

module ol_loop_pptllj_eexuxtdbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(67), c(29)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:134)
  ! denominators
  complex(REALKIND), save :: den(91)
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
    f( 1) = -(CI*countertermnorm*ctSqq*eQED**4*gQCD**2*(cw - sw)*(cw + sw))/(4._/**/REALKIND*cw*MW**2*sw**3)
    f( 2) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 5) = (CI*eQED**4*ME)/(4._/**/REALKIND*sw**4)
    f( 6) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*sw**4)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*sw**4)
    f( 8) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*MB*ME)/(8._/**/REALKIND*MW**4*sw**4)
    f( 9) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*ME**2)/(4._/**/REALKIND*MW**4*sw**4)
    f(10) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*ME*MH**2)/(8._/**/REALKIND*MW**4*sw**4)
    f(11) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*ME*MT)/(8._/**/REALKIND*MW**4*sw**4)
    f(12) = (CI*eQED**4*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(13) = (CI*eQED**4*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f(14) = (CI*countertermnorm*eQED**4*gQCD**2*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(15) = (CI*countertermnorm*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f(16) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(17) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f(18) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(19) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f(20) = (CI*eQED**4*MB*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(21) = (CI*countertermnorm*eQED**4*gQCD**2*MB*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(22) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(23) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2*MB*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(24) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*MB*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f(25) = (CI*eQED**4*ME*MT)/(8._/**/REALKIND*MW**2*sw**4)
    f(26) = (CI*countertermnorm*eQED**4*gQCD**2*ME*MT)/(8._/**/REALKIND*MW**2*sw**4)
    f(27) = (CI*countertermnorm*ctStt*eQED**4*gQCD**2*ME*MT)/(8._/**/REALKIND*MW**2*sw**4)
    f(28) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2*ME*MT)/(8._/**/REALKIND*MW**2*sw**4)
    f(29) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*ME*MT)/(8._/**/REALKIND*MW**2*sw**4)
    f(30) = (CI*cw*eQED**4)/(2._/**/REALKIND*sw**3)
    f(31) = (CI*countertermnorm*ctVbt*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(32) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(33) = (CI*eQED**4)/(6._/**/REALKIND*sw**2)
    f(34) = (CI*eQED**4)/(3._/**/REALKIND*sw**2)
    f(35) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f(36) = (CI*countertermnorm*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(37) = (CI*countertermnorm*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(38) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(39) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(40) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(41) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(42) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(43) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(44) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(45) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(46) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(47) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(48) = (CI*countertermnorm*ctVtt*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(49) = (CI*countertermnorm*ctVtt*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(50) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2)/(6._/**/REALKIND*MW**2*sw**2)
    f(51) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2)/(3._/**/REALKIND*MW**2*sw**2)
    f(52) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2)/(2._/**/REALKIND*MW**2*sw**2)
    f(53) = (CI*eQED**4)/(2._/**/REALKIND*cw*sw)
    f(54) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*cw*sw)
    f(55) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2)/(2._/**/REALKIND*cw*sw)
    f(56) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*cw*sw)
    f(57) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(58) = (eQED**4*gQCD**2*integralnorm*ME*SwB)/(sw**4*4._/**/REALKIND)
    f(59) = (eQED**4*gQCD**2*integralnorm*ME*SwB)/(MW**2*sw**4*8._/**/REALKIND)
    f(60) = (eQED**4*gQCD**2*integralnorm*ME*SwB)/(MW**2*sw**4*4._/**/REALKIND)
    f(61) = (eQED**4*gQCD**2*integralnorm*MB*ME*SwB)/(MW**2*sw**4*8._/**/REALKIND)
    f(62) = (eQED**4*gQCD**2*integralnorm*ME*MT*SwB)/(MW**2*sw**4*8._/**/REALKIND)
    f(63) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(64) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(65) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(66) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(67) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw*sw*2._/**/REALKIND)

  c = [ 8*f(57), f(58), 3*f(58), 8*f(58), f(59), 3*f(59), 8*f(59), 8*f(60), f(61), 3*f(61), 8*f(61), f(62), 3*f(62), 8*f(62) &
    , f(63), 3*f(63), 8*f(63), f(64), 3*f(64), 8*f(64), f(65), 3*f(65), 8*f(65), f(66), 3*f(66), 8*f(66), f(67), 3*f(67) &
    , 8*f(67) ]
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
  complex(REALKIND) :: A(108)
  ! external WFs
  call wf_Q(P(:,1), rME, H(1), wf(:,0))
  call wf_A(P(:,2), rME, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rMT, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))

  ! internal WFs
  call vert_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,1))
  call vert_QA_W(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AQ_S(gPbt,wf(:,-5),wf(:,-3),wf(:,3))
  call vert_TV_S(wf(:,1),Q(:,3),wf(:,2),Q(:,20),wf(:,4))
  call vert_AQ_S(gX,wf(:,-1),wf(:,0),wf(:,5))
  call vert_TV_S(wf(:,5),Q(:,3),wf(:,2),Q(:,20),wf(:,6))
  call vert_QA_W(wf(:,-3),wf(:,-5),wf(:,7))
  call vert_SV_V(wf(:,1),wf(:,2),wf(:,8))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,9))
  call vert_VV_S(wf(:,9),wf(:,2),wf(:,10))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,11))
  call vert_VV_S(wf(:,11),wf(:,2),wf(:,12))
  call vert_UV_W(wf(:,2),Q(:,20),wf(:,9),Q(:,3),wf(:,13))
  call vert_UV_W(wf(:,2),Q(:,20),wf(:,11),Q(:,3),wf(:,14))
  call vert_QS_A(gH,wf(:,-3),wf(:,1),wf(:,15))
  call vert_AW_Q(wf(:,-5),wf(:,2),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,11),MT,1_intkind1,wf(:,17))
  call vert_QS_A(gX,wf(:,-3),wf(:,5),wf(:,18))
  call prop_Q_A(wf(:,18),Q(:,11),MT,1_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,9),wf(:,-3),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,11),MT,1_intkind1,wf(:,21))
  call vert_ZQ_A(gZu,wf(:,11),wf(:,-3),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,11),MT,1_intkind1,wf(:,23))
  call vert_WQ_A(wf(:,2),wf(:,-3),wf(:,24))
  call vert_SA_Q(gH,wf(:,1),wf(:,-5),wf(:,25))
  call prop_Q_A(wf(:,24),Q(:,28),MB,1_intkind1,wf(:,26))
  call vert_SA_Q(gX,wf(:,5),wf(:,-5),wf(:,27))
  call vert_AV_Q(wf(:,-5),wf(:,9),wf(:,28))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,11),wf(:,29))
  call vert_AV_Q(wf(:,-2),wf(:,9),wf(:,30))
  call vert_WQ_A(wf(:,7),wf(:,-4),wf(:,31))
  call prop_A_Q(wf(:,30),Q(:,7),ZERO,0_intkind1,wf(:,32))
  call vert_AZ_Q(gZu,wf(:,-2),wf(:,11),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,7),ZERO,0_intkind1,wf(:,34))
  call vert_AW_Q(wf(:,-2),wf(:,7),wf(:,35))
  call vert_VQ_A(wf(:,9),wf(:,-4),wf(:,36))
  call prop_A_Q(wf(:,35),Q(:,44),ZERO,0_intkind1,wf(:,37))
  call vert_ZQ_A(gZd,wf(:,11),wf(:,-4),wf(:,38))
  call vert_QS_A(gPnl,wf(:,0),wf(:,3),wf(:,39))
  call vert_AW_Q(wf(:,-1),wf(:,2),wf(:,40))
  call prop_Q_A(wf(:,39),Q(:,41),ZERO,0_intkind1,wf(:,41))
  call vert_WQ_A(wf(:,7),wf(:,0),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,41),ZERO,0_intkind1,wf(:,43))
  call counter_AW_Q(wf(:,-5),wf(:,2),wf(:,44))
  call counter_SA_Q(gH,wf(:,1),wf(:,-5),wf(:,45))
  call counter_SA_Q(gX,wf(:,5),wf(:,-5),wf(:,46))
  call counter_AV_Q(wf(:,-5),wf(:,9),wf(:,47))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,11),wf(:,48))
  call counter_QS_A(gPud,wf(:,-4),wf(:,3),wf(:,49))
  call counter_WQ_A(wf(:,7),wf(:,-4),wf(:,50))
  call counter_VQ_A(wf(:,9),wf(:,-4),wf(:,51))
  call counter_ZQ_A(gZd,wf(:,11),wf(:,-4),wf(:,52))
  call counter_WQ_A(wf(:,2),wf(:,-3),wf(:,53))
  call prop_A_Q(wf(:,25),Q(:,35),MB,1_intkind1,wf(:,54))
  call prop_A_Q(wf(:,27),Q(:,35),MB,1_intkind1,wf(:,55))
  call prop_A_Q(wf(:,28),Q(:,35),MB,1_intkind1,wf(:,56))
  call prop_A_Q(wf(:,29),Q(:,35),MB,1_intkind1,wf(:,57))
  call counter_QS_A(gH,wf(:,-3),wf(:,1),wf(:,58))
  call prop_A_Q(wf(:,16),Q(:,52),MT,1_intkind1,wf(:,59))
  call counter_QS_A(gX,wf(:,-3),wf(:,5),wf(:,60))
  call counter_VQ_A(wf(:,9),wf(:,-3),wf(:,61))
  call counter_ZQ_A(gZu,wf(:,11),wf(:,-3),wf(:,62))
  call counter_AQ_S(ctSbt,wf(:,-5),wf(:,-3),wf(:,63))
  call counter_QA_W(wf(:,-3),wf(:,-5),wf(:,64))
  call vert_WQ_A(wf(:,64),wf(:,-4),wf(:,65))
  call vert_AW_Q(wf(:,-2),wf(:,64),wf(:,66))
  call prop_A_Q(wf(:,66),Q(:,44),ZERO,0_intkind1,wf(:,67))
  call counter_SA_Q(gPud,wf(:,3),wf(:,-2),wf(:,68))
  call prop_Q_A(wf(:,36),Q(:,19),ZERO,0_intkind1,wf(:,69))
  call prop_Q_A(wf(:,38),Q(:,19),ZERO,0_intkind1,wf(:,70))
  call counter_AW_Q(wf(:,-2),wf(:,7),wf(:,71))
  call counter_AV_Q(wf(:,-2),wf(:,9),wf(:,72))
  call prop_Q_A(wf(:,31),Q(:,56),ZERO,0_intkind1,wf(:,73))
  call counter_AZ_Q(gZu,wf(:,-2),wf(:,11),wf(:,74))
  call counter_AQ_S(gPud,wf(:,-2),wf(:,-4),wf(:,75))
  call vert_SS_S(wf(:,1),wf(:,75),wf(:,76))
  call vert_ST_V(wf(:,75),Q(:,20),wf(:,1),Q(:,3),wf(:,77))
  call vert_ST_V(wf(:,75),Q(:,20),wf(:,5),Q(:,3),wf(:,78))
  call counter_QA_W(wf(:,-4),wf(:,-2),wf(:,79))
  call vert_TV_S(wf(:,1),Q(:,3),wf(:,79),Q(:,20),wf(:,80))
  call vert_TV_S(wf(:,5),Q(:,3),wf(:,79),Q(:,20),wf(:,81))
  call vert_SV_V(wf(:,1),wf(:,79),wf(:,82))
  call vert_TV_S(wf(:,75),Q(:,20),wf(:,9),Q(:,3),wf(:,83))
  call vert_TV_S(wf(:,75),Q(:,20),wf(:,11),Q(:,3),wf(:,84))
  call vert_SV_V(wf(:,75),wf(:,9),wf(:,85))
  call vert_SV_V(wf(:,75),wf(:,11),wf(:,86))
  call vert_VV_S(wf(:,9),wf(:,79),wf(:,87))
  call vert_VV_S(wf(:,11),wf(:,79),wf(:,88))
  call vert_UV_W(wf(:,79),Q(:,20),wf(:,9),Q(:,3),wf(:,89))
  call vert_UV_W(wf(:,79),Q(:,20),wf(:,11),Q(:,3),wf(:,90))
  call vert_SA_Q(gPbt,wf(:,75),wf(:,-5),wf(:,91))
  call vert_AW_Q(wf(:,-5),wf(:,79),wf(:,92))
  call vert_QS_A(gPbt,wf(:,-3),wf(:,75),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,28),MB,1_intkind1,wf(:,94))
  call vert_WQ_A(wf(:,79),wf(:,-3),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,28),MB,1_intkind1,wf(:,96))
  call vert_QS_A(gPnl,wf(:,0),wf(:,63),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,41),ZERO,0_intkind1,wf(:,98))
  call vert_WQ_A(wf(:,64),wf(:,0),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,41),ZERO,0_intkind1,wf(:,100))
  call vert_SA_Q(gPln,wf(:,75),wf(:,-1),wf(:,101))
  call vert_AW_Q(wf(:,-1),wf(:,79),wf(:,102))
  call counter_Q_A(cttt,wf(:,17),Q(:,11),wf(:,103))
  call counter_Q_A(cttt,wf(:,19),Q(:,11),wf(:,104))
  call counter_Q_A(cttt,wf(:,21),Q(:,11),wf(:,105))
  call counter_Q_A(cttt,wf(:,23),Q(:,11),wf(:,106))
  call counter_Q_A(ctbb,wf(:,26),Q(:,28),wf(:,107))
  call counter_A_Q(ctqq,wf(:,32),Q(:,7),wf(:,108))
  call counter_A_Q(ctqq,wf(:,34),Q(:,7),wf(:,109))
  call counter_A_Q(ctqq,wf(:,37),Q(:,44),wf(:,110))
  call vert_ST_V(wf(:,3),Q(:,40),wf(:,1),Q(:,3),wf(:,111))
  call vert_ST_V(wf(:,3),Q(:,40),wf(:,5),Q(:,3),wf(:,112))
  call vert_SV_V(wf(:,1),wf(:,7),wf(:,113))
  call vert_SV_V(wf(:,3),wf(:,9),wf(:,114))
  call vert_SV_V(wf(:,3),wf(:,11),wf(:,115))
  call vert_UV_W(wf(:,9),Q(:,3),wf(:,7),Q(:,40),wf(:,116))
  call vert_UV_W(wf(:,11),Q(:,3),wf(:,7),Q(:,40),wf(:,117))
  call vert_QA_W(wf(:,-4),wf(:,32),wf(:,118))
  call vert_QA_W(wf(:,-4),wf(:,34),wf(:,119))
  call vert_QA_W(wf(:,17),wf(:,-5),wf(:,120))
  call vert_QA_W(wf(:,19),wf(:,-5),wf(:,121))
  call vert_QA_W(wf(:,21),wf(:,-5),wf(:,122))
  call vert_QA_W(wf(:,23),wf(:,-5),wf(:,123))
  call vert_QA_W(wf(:,69),wf(:,-2),wf(:,124))
  call vert_QA_W(wf(:,70),wf(:,-2),wf(:,125))
  call vert_QA_W(wf(:,-3),wf(:,54),wf(:,126))
  call vert_QA_W(wf(:,-3),wf(:,55),wf(:,127))
  call vert_QA_W(wf(:,-3),wf(:,56),wf(:,128))
  call vert_QA_W(wf(:,-3),wf(:,57),wf(:,129))
  call prop_A_Q(wf(:,40),Q(:,22),ZERO,0_intkind1,wf(:,130))
  call vert_AQ_S(gPnl,wf(:,130),wf(:,0),wf(:,131))
  call vert_QA_W(wf(:,0),wf(:,130),wf(:,132))
  call vert_QA_W(wf(:,41),wf(:,-1),wf(:,133))
  call vert_QA_W(wf(:,43),wf(:,-1),wf(:,134))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MH2)
  den(2) = 1 / (Q(5,20) - MW2)
  den(3) = 1 / (Q(5,40) - MW2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,3))
  den(12) = 1 / (Q(5,11) - MT2)
  den(19) = 1 / (Q(5,28) - MB2)
  den(24) = 1 / (Q(5,7))
  den(29) = 1 / (Q(5,44))
  den(33) = 1 / (Q(5,41))
  den(36) = 1 / (Q(5,35) - MB2)
  den(43) = 1 / (Q(5,52) - MT2)
  den(48) = 1 / (Q(5,19))
  den(53) = 1 / (Q(5,56))
  den(67) = 1 / (Q(5,23) - MW2)
  den(72) = 1 / (Q(5,43) - MW2)
  den(88) = 1 / (Q(5,22))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(3)*den(10)
  den(13) = den(1)*den(12)
  den(14) = den(2)*den(13)
  den(15) = den(6)*den(12)
  den(16) = den(2)*den(15)
  den(17) = den(9)*den(12)
  den(18) = den(2)*den(17)
  den(20) = den(2)*den(19)
  den(21) = den(1)*den(20)
  den(22) = den(6)*den(20)
  den(23) = den(9)*den(20)
  den(25) = den(9)*den(24)
  den(26) = den(3)*den(25)
  den(27) = den(6)*den(24)
  den(28) = den(3)*den(27)
  den(30) = den(3)*den(29)
  den(31) = den(9)*den(30)
  den(32) = den(6)*den(30)
  den(34) = den(3)*den(33)
  den(35) = den(2)*den(34)
  den(37) = den(1)*den(36)
  den(38) = den(2)*den(37)
  den(39) = den(6)*den(36)
  den(40) = den(2)*den(39)
  den(41) = den(9)*den(36)
  den(42) = den(2)*den(41)
  den(44) = den(2)*den(43)
  den(45) = den(1)*den(44)
  den(46) = den(6)*den(44)
  den(47) = den(9)*den(44)
  den(49) = den(9)*den(48)
  den(50) = den(3)*den(49)
  den(51) = den(6)*den(48)
  den(52) = den(3)*den(51)
  den(54) = den(3)*den(53)
  den(55) = den(9)*den(54)
  den(56) = den(6)*den(54)
  den(57) = den(13)*den(44)
  den(58) = den(15)*den(44)
  den(59) = den(17)*den(44)
  den(60) = den(20)*den(37)
  den(61) = den(20)*den(39)
  den(62) = den(20)*den(41)
  den(63) = den(25)*den(54)
  den(64) = den(27)*den(54)
  den(65) = den(30)*den(49)
  den(66) = den(30)*den(51)
  den(68) = den(4)*den(67)
  den(69) = den(7)*den(67)
  den(70) = den(10)*den(67)
  den(71) = den(1)*den(3)
  den(73) = den(71)*den(72)
  den(74) = den(3)*den(6)
  den(75) = den(72)*den(74)
  den(76) = den(3)*den(9)
  den(77) = den(72)*den(76)
  den(78) = den(25)*den(67)
  den(79) = den(27)*den(67)
  den(80) = den(13)*den(72)
  den(81) = den(15)*den(72)
  den(82) = den(17)*den(72)
  den(83) = den(49)*den(67)
  den(84) = den(51)*den(67)
  den(85) = den(37)*den(72)
  den(86) = den(39)*den(72)
  den(87) = den(41)*den(72)
  den(89) = den(2)*den(88)
  den(90) = den(67)*den(89)
  den(91) = den(34)*den(72)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(108)

  A(1) = cont_SS(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_SS(wf(:,3),wf(:,6)) * den(8)
  A(3) = cont_VV(wf(:,7),wf(:,8)) * den(5)
  A(4) = cont_SS(wf(:,3),wf(:,10)) * den(11)
  A(5) = cont_SS(wf(:,3),wf(:,12)) * den(8)
  A(6) = cont_VV(wf(:,7),wf(:,13)) * den(11)
  A(7) = cont_VV(wf(:,7),wf(:,14)) * den(8)
  A(8) = cont_QA(wf(:,16),wf(:,17)) * den(14)
  A(9) = cont_QA(wf(:,16),wf(:,19)) * den(16)
  A(10) = cont_QA(wf(:,16),wf(:,21)) * den(18)
  A(11) = cont_QA(wf(:,16),wf(:,23)) * den(16)
  A(12) = cont_QA(wf(:,25),wf(:,26)) * den(21)
  A(13) = cont_QA(wf(:,26),wf(:,27)) * den(22)
  A(14) = cont_QA(wf(:,26),wf(:,28)) * den(23)
  A(15) = cont_QA(wf(:,26),wf(:,29)) * den(22)
  A(16) = cont_QA(wf(:,31),wf(:,32)) * den(26)
  A(17) = cont_QA(wf(:,31),wf(:,34)) * den(28)
  A(18) = cont_QA(wf(:,36),wf(:,37)) * den(31)
  A(19) = cont_QA(wf(:,37),wf(:,38)) * den(32)
  A(20) = cont_QA(wf(:,40),wf(:,41)) * den(35)
  A(21) = cont_QA(wf(:,40),wf(:,43)) * den(35)

  A(22) = cont_QA(wf(:,17),wf(:,44)) * den(14)
  A(23) = cont_QA(wf(:,19),wf(:,44)) * den(16)
  A(24) = cont_QA(wf(:,21),wf(:,44)) * den(18)
  A(25) = cont_QA(wf(:,23),wf(:,44)) * den(16)
  A(26) = cont_QA(wf(:,26),wf(:,45)) * den(21)
  A(27) = cont_QA(wf(:,26),wf(:,46)) * den(22)
  A(28) = cont_QA(wf(:,26),wf(:,47)) * den(23)
  A(29) = cont_QA(wf(:,26),wf(:,48)) * den(22)
  A(30) = cont_QA(wf(:,32),wf(:,49)) * den(26)
  A(31) = cont_QA(wf(:,34),wf(:,49)) * den(28)
  A(32) = cont_QA(wf(:,32),wf(:,50)) * den(26)
  A(33) = cont_QA(wf(:,34),wf(:,50)) * den(28)
  A(34) = cont_QA(wf(:,37),wf(:,51)) * den(31)
  A(35) = cont_QA(wf(:,37),wf(:,52)) * den(32)
  A(36) = cont_QA(wf(:,53),wf(:,54)) * den(38)
  A(37) = cont_QA(wf(:,53),wf(:,55)) * den(40)
  A(38) = cont_QA(wf(:,53),wf(:,56)) * den(42)
  A(39) = cont_QA(wf(:,53),wf(:,57)) * den(40)
  A(40) = cont_QA(wf(:,58),wf(:,59)) * den(45)
  A(41) = cont_QA(wf(:,59),wf(:,60)) * den(46)
  A(42) = cont_QA(wf(:,59),wf(:,61)) * den(47)
  A(43) = cont_QA(wf(:,59),wf(:,62)) * den(46)
  A(44) = cont_SS(wf(:,4),wf(:,63)) * den(5)
  A(45) = cont_SS(wf(:,6),wf(:,63)) * den(8)
  A(46) = cont_VV(wf(:,8),wf(:,64)) * den(5)
  A(47) = cont_SS(wf(:,10),wf(:,63)) * den(11)
  A(48) = cont_SS(wf(:,12),wf(:,63)) * den(8)
  A(49) = cont_VV(wf(:,13),wf(:,64)) * den(11)
  A(50) = cont_VV(wf(:,14),wf(:,64)) * den(8)
  A(51) = cont_QA(wf(:,32),wf(:,65)) * den(26)
  A(52) = cont_QA(wf(:,34),wf(:,65)) * den(28)
  A(53) = cont_QA(wf(:,36),wf(:,67)) * den(31)
  A(54) = cont_QA(wf(:,38),wf(:,67)) * den(32)
  A(55) = cont_QA(wf(:,68),wf(:,69)) * den(50)
  A(56) = cont_QA(wf(:,68),wf(:,70)) * den(52)
  A(57) = cont_QA(wf(:,69),wf(:,71)) * den(50)
  A(58) = cont_QA(wf(:,70),wf(:,71)) * den(52)
  A(59) = cont_QA(wf(:,72),wf(:,73)) * den(55)
  A(60) = cont_QA(wf(:,73),wf(:,74)) * den(56)
  A(61) = cont_SS(wf(:,3),wf(:,76)) * den(5)
  A(62) = cont_VV(wf(:,7),wf(:,77)) * den(5)
  A(63) = cont_VV(wf(:,7),wf(:,78)) * den(8)
  A(64) = cont_SS(wf(:,3),wf(:,80)) * den(5)
  A(65) = cont_SS(wf(:,3),wf(:,81)) * den(8)
  A(66) = cont_VV(wf(:,7),wf(:,82)) * den(5)
  A(67) = cont_SS(wf(:,3),wf(:,83)) * den(11)
  A(68) = cont_SS(wf(:,3),wf(:,84)) * den(8)
  A(69) = cont_VV(wf(:,7),wf(:,85)) * den(11)
  A(70) = cont_VV(wf(:,7),wf(:,86)) * den(8)
  A(71) = cont_SS(wf(:,3),wf(:,87)) * den(11)
  A(72) = cont_SS(wf(:,3),wf(:,88)) * den(8)
  A(73) = cont_VV(wf(:,7),wf(:,89)) * den(11)
  A(74) = cont_VV(wf(:,7),wf(:,90)) * den(8)
  A(75) = cont_QA(wf(:,17),wf(:,91)) * den(14)
  A(76) = cont_QA(wf(:,19),wf(:,91)) * den(16)
  A(77) = cont_QA(wf(:,17),wf(:,92)) * den(14)
  A(78) = cont_QA(wf(:,19),wf(:,92)) * den(16)
  A(79) = cont_QA(wf(:,21),wf(:,91)) * den(18)
  A(80) = cont_QA(wf(:,23),wf(:,91)) * den(16)
  A(81) = cont_QA(wf(:,21),wf(:,92)) * den(18)
  A(82) = cont_QA(wf(:,23),wf(:,92)) * den(16)
  A(83) = cont_QA(wf(:,25),wf(:,94)) * den(21)
  A(84) = cont_QA(wf(:,27),wf(:,94)) * den(22)
  A(85) = cont_QA(wf(:,25),wf(:,96)) * den(21)
  A(86) = cont_QA(wf(:,27),wf(:,96)) * den(22)
  A(87) = cont_QA(wf(:,28),wf(:,94)) * den(23)
  A(88) = cont_QA(wf(:,29),wf(:,94)) * den(22)
  A(89) = cont_QA(wf(:,28),wf(:,96)) * den(23)
  A(90) = cont_QA(wf(:,29),wf(:,96)) * den(22)
  A(91) = cont_QA(wf(:,40),wf(:,98)) * den(35)
  A(92) = cont_QA(wf(:,40),wf(:,100)) * den(35)
  A(93) = cont_QA(wf(:,41),wf(:,101)) * den(35)
  A(94) = cont_QA(wf(:,41),wf(:,102)) * den(35)
  A(95) = cont_QA(wf(:,43),wf(:,101)) * den(35)
  A(96) = cont_QA(wf(:,43),wf(:,102)) * den(35)
  A(97) = cont_QA(wf(:,59),wf(:,103)) * den(57)
  A(98) = cont_QA(wf(:,59),wf(:,104)) * den(58)
  A(99) = cont_QA(wf(:,59),wf(:,105)) * den(59)
  A(100) = cont_QA(wf(:,59),wf(:,106)) * den(58)
  A(101) = cont_QA(wf(:,54),wf(:,107)) * den(60)
  A(102) = cont_QA(wf(:,55),wf(:,107)) * den(61)
  A(103) = cont_QA(wf(:,56),wf(:,107)) * den(62)
  A(104) = cont_QA(wf(:,57),wf(:,107)) * den(61)
  A(105) = cont_QA(wf(:,73),wf(:,108)) * den(63)
  A(106) = cont_QA(wf(:,73),wf(:,109)) * den(64)
  A(107) = cont_QA(wf(:,69),wf(:,110)) * den(65)
  A(108) = cont_QA(wf(:,70),wf(:,110)) * den(66)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(108)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(21)*f(2)-A(3)*f(5)+(-A(1)+A(2))*f(12)-A(20)*f(13)+(-A(12)+A(13))*f(20)+(-A(8)-A(9))*f(25)+A(7)*f(30)+(A(14) &
       +A(18))*f(33)+(-A(10)-A(16))*f(34)+(A(4)-A(6)+A(11)+A(15)+A(17)+A(19))*f(35)+A(5)*f(53)

  M2(1) = -(A(68)*f(1))+A(92)*f(3)+A(96)*f(4)-A(46)*f(6)-A(66)*f(7)+(-A(83)+A(84))*f(8)-A(93)*f(9)-A(61)*f(10)+(-A(75) &
       -A(76))*f(11)+(-A(44)+A(45))*f(14)-A(91)*f(15)+(-A(62)-A(63))*f(16)+A(95)*f(17)+(-A(64)+A(65))*f(18)-A(94)*f(19)+(A(101) &
       -A(102))*f(21)+(-A(26)+A(27))*f(22)+(-A(36)+A(37))*f(23)+(-A(85)+A(86))*f(24)+(A(97)+A(98))*f(26)+(-A(40)-A(41))*f(27)+( &
       -A(22)-A(23))*f(28)+(-A(77)-A(78))*f(29)+A(50)*f(31)+A(74)*f(32)+(-A(103)-A(107))*f(36)+(A(99)+A(105))*f(37)+(A(47)-A(100) &
       -A(104)-A(106)-A(108))*f(38)-A(69)*f(39)+A(28)*f(40)+A(29)*f(41)+(A(38)+A(53))*f(42)+(-A(24)-A(51))*f(43)+(A(25)+A(39) &
       -A(49)+A(52)+A(54))*f(44)+(A(34)+A(57)+A(89))*f(45)+(-A(32)-A(59)-A(81))*f(46)+(A(33)+A(35)+A(58)+A(60)+A(71)-A(73)+A(82) &
       +A(90))*f(47)-A(42)*f(48)+A(43)*f(49)+(A(55)+A(87))*f(50)+(-A(30)-A(79))*f(51)+(A(31)+A(56)-A(67)+A(80)+A(88))*f(52) &
       +A(48)*f(54)-A(70)*f(55)+A(72)*f(56)

end subroutine colourvectors

end module ol_loop_pptllj_eexuxtdbx_1_/**/REALKIND
