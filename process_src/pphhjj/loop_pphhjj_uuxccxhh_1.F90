
module ol_colourmatrix_pphhjj_uuxccxhh_1_/**/REALKIND
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
  K1( 2,:) = [  12]
  K1( 3,:) = [ -12]
  K1( 4,:) = [  12]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  12]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [ -12]
  K1(11,:) = [  12]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [   0]
  K1(20,:) = [   0]
  K1(21,:) = [   0]
  K1(22,:) = [   0]
  K1(23,:) = [   0]

  K2(1,:) = [ 9]

  KL(1,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphhjj_uuxccxhh_1_/**/REALKIND



module ol_forced_parameters_pphhjj_uuxccxhh_1_/**/REALKIND
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
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphhjj_uuxccxhh_1_/**/REALKIND

module ol_loop_pphhjj_uuxccxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(65), c(110)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:55)
  ! denominators
  complex(REALKIND), save :: den(61)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

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
    f( 1) = (CI*eQED**4)/(4._/**/REALKIND*cw**2*sw**2)
    f( 2) = (CI*eQED**4)/(2._/**/REALKIND*cw**2*sw**2)
    f( 3) = (CI*countertermnorm*ctVcc*eQED**4*gQCD**2)/(4._/**/REALKIND*cw**2*sw**2)
    f( 4) = (CI*countertermnorm*ctVcc*eQED**4*gQCD**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 5) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*cw**2*sw**2)
    f( 6) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 7) = (3*CI*eQED**4*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 8) = (3*CI*countertermnorm*ctVcc*eQED**4*gQCD**2*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 9) = (3*CI*countertermnorm*ctVqq*eQED**4*gQCD**2*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f(10) = (CI*eQED**4*lambdaHZZ**2*MW**2)/(cw**4*sw**2)
    f(11) = (CI*countertermnorm*ctVcc*eQED**4*gQCD**2*lambdaHZZ**2*MW**2)/(cw**4*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*lambdaHZZ**2*MW**2)/(cw**4*sw**2)
    f(13) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*8._/**/REALKIND)
    f(14) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(15) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHWW*MH**2*SwB)/(sw**4*4._/**/REALKIND)
    f(16) = (eQED**4*gQCD**2*integralnorm*lambdaHWW**2*MW**2*SwB)/(sw**4*2._/**/REALKIND)
    f(17) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*4._/**/REALKIND)
    f(18) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(19) = (3*cw*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwB)/(sw**3*4._/**/REALKIND)
    f(20) = (3*cw*eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHWW*MH**2*SwB)/(sw**3*2._/**/REALKIND)
    f(21) = (cw*eQED**4*gQCD**2*integralnorm*MW**2*SwB)/(sw**3*4._/**/REALKIND)
    f(22) = (cw*eQED**4*gQCD**2*integralnorm*lambdaHWW**2*MW**2*SwB)/sw**3
    f(23) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(24) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(25) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw**2*sw**2*4._/**/REALKIND)
    f(26) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(27) = (eQED**4*gQCD**2*integralnorm*MH**2*SwB)/(sw**2*6._/**/REALKIND)
    f(28) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwB)/(sw**2*2._/**/REALKIND)
    f(29) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHWW*MH**2*SwB)/sw**2
    f(30) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHZZ*MH**2*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(31) = (eQED**4*gQCD**2*integralnorm*MH**2*SwB)/(MW**2*sw**2*6._/**/REALKIND)
    f(32) = (eQED**4*gQCD**2*integralnorm*MH**4*SwB)/(MW**2*sw**2*6._/**/REALKIND)
    f(33) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**4*SwB)/(MW**2*sw**2*2._/**/REALKIND)
    f(34) = (eQED**4*gQCD**2*integralnorm*MW**2*SwB)/(sw**2*6._/**/REALKIND)
    f(35) = (eQED**4*gQCD**2*integralnorm*lambdaHWW*MW**2*SwB)/(sw**2*3._/**/REALKIND)
    f(36) = (2*eQED**4*gQCD**2*integralnorm*lambdaHWW**2*MW**2*SwB)/(sw**2*3._/**/REALKIND)
    f(37) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ**2*MW**2*SwB)/(cw**4*sw**2)
    f(38) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw*sw*4._/**/REALKIND)
    f(39) = (eQED**4*gQCD**2*integralnorm*MH**2*SwB)/(cw*sw*4._/**/REALKIND)
    f(40) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwB)/(cw*sw*4._/**/REALKIND)
    f(41) = (eQED**4*gQCD**2*integralnorm*lambdaHWW*MW**2*SwB)/(cw*sw*2._/**/REALKIND)
    f(42) = -((eQED**4*gQCD**2*integralnorm*(cw - sw)*(cw + sw)*SwB)/(cw*sw**3*8._/**/REALKIND))
    f(43) = -((eQED**4*gQCD**2*integralnorm*MH**2*(cw - sw)*(cw + sw)*SwB)/(cw*MW**2*sw**3*8._/**/REALKIND))
    f(44) = -((eQED**4*gQCD**2*integralnorm*MH**4*(cw - sw)*(cw + sw)*SwB)/(cw*MW**2*sw**3*8._/**/REALKIND))
    f(45) = (-3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**4*(cw - sw)*(cw + sw)*SwB)/(cw*MW**2*sw**3*8._/**/REALKIND)
    f(46) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YB)/(MW**2*sw**2*6._/**/REALKIND)
    f(47) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YB)/(MW**2*sw**2*4._/**/REALKIND)
    f(48) = (eQED**4*gQCD**2*integralnorm*SwF*YB**2)/(MW**2*sw**2*18._/**/REALKIND)
    f(49) = (eQED**4*gQCD**2*integralnorm*SwF*YB**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(50) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YE)/(MW**2*sw**2*2._/**/REALKIND)
    f(51) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(52) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*6._/**/REALKIND)
    f(53) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(54) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YL)/(MW**2*sw**2*2._/**/REALKIND)
    f(55) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YL)/(MW**2*sw**2*4._/**/REALKIND)
    f(56) = (eQED**4*gQCD**2*integralnorm*SwF*YL**2)/(MW**2*sw**2*6._/**/REALKIND)
    f(57) = (eQED**4*gQCD**2*integralnorm*SwF*YL**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(58) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YM)/(MW**2*sw**2*2._/**/REALKIND)
    f(59) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(60) = (eQED**4*gQCD**2*integralnorm*SwF*YM**2)/(MW**2*sw**2*6._/**/REALKIND)
    f(61) = (eQED**4*gQCD**2*integralnorm*SwF*YM**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(62) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YT)/(MW**2*sw**2*3._/**/REALKIND)
    f(63) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YT)/(MW**2*sw**2*4._/**/REALKIND)
    f(64) = (eQED**4*gQCD**2*integralnorm*SwF*YT**2)/(MW**2*sw**2*9._/**/REALKIND)
    f(65) = (eQED**4*gQCD**2*integralnorm*SwF*YT**2)/(MW**2*sw**2*4._/**/REALKIND)

  c = [ f(13), 3*f(13), f(14), 3*f(14), f(15), 3*f(15), f(16), 3*f(16), f(17), 3*f(17), f(18), 3*f(18), f(19), 3*f(19), f(20) &
    , 3*f(20), f(21), 3*f(21), f(22), 3*f(22), f(23), 3*f(23), f(24), 3*f(24), f(25), 3*f(25), 8*f(25), f(26), 3*f(26), 8*f(26) &
    , f(27), 3*f(27), f(28), 3*f(28), f(29), 3*f(29), f(30), 3*f(30), 8*f(30), f(31), 3*f(31), f(32), 3*f(32), f(33), 3*f(33) &
    , f(34), 3*f(34), f(35), 3*f(35), f(36), 3*f(36), f(37), 3*f(37), 8*f(37), f(38), 3*f(38), f(39), 3*f(39), f(40), 3*f(40) &
    , f(41), 3*f(41), f(42), 3*f(42), f(43), 3*f(43), f(44), 3*f(44), f(45), 3*f(45), 3*f(46), 9*f(46), 3*f(47), 9*f(47), 3*f(48) &
    , 9*f(48), 3*f(49), 9*f(49), f(50), 3*f(50), f(51), 3*f(51), f(52), 3*f(52), f(53), 3*f(53), f(54), 3*f(54), f(55), 3*f(55) &
    , f(56), 3*f(56), f(57), 3*f(57), f(58), 3*f(58), f(59), 3*f(59), f(60), 3*f(60), f(61), 3*f(61), 3*f(62), 9*f(62), 3*f(63) &
    , 9*f(63), 3*f(64), 9*f(64), 3*f(65), 9*f(65) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND) :: A(18)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZu,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,-3),wf(:,2))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_SS_S(wf(:,-4),wf(:,-5),wf(:,4))
  call vert_VV_S(wf(:,1),wf(:,2),wf(:,5))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,1),Q(:,3),wf(:,6))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,2),Q(:,12),wf(:,7))
  call vert_SV_V(wf(:,-4),wf(:,1),wf(:,8))
  call vert_SV_V(wf(:,-5),wf(:,2),wf(:,9))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,2),Q(:,12),wf(:,10))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,1),Q(:,3),wf(:,11))
  call vert_SV_V(wf(:,-4),wf(:,2),wf(:,12))
  call vert_SV_V(wf(:,-5),wf(:,1),wf(:,13))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,-3),wf(:,14))
  call counter_QA_Z(gZu,wf(:,0),wf(:,-1),wf(:,15))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,15),wf(:,16))
  call vert_VV_S(wf(:,1),wf(:,14),wf(:,17))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,14),Q(:,12),wf(:,18))
  call vert_SV_V(wf(:,-5),wf(:,14),wf(:,19))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,14),Q(:,12),wf(:,20))
  call vert_SV_V(wf(:,-4),wf(:,14),wf(:,21))
  call vert_VV_S(wf(:,15),wf(:,2),wf(:,22))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,15),Q(:,3),wf(:,23))
  call vert_SV_V(wf(:,-4),wf(:,15),wf(:,24))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,15),Q(:,3),wf(:,25))
  call vert_SV_V(wf(:,-5),wf(:,15),wf(:,26))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,27))
  call vert_VQ_A(wf(:,27),wf(:,-2),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,7),ZERO,0_intkind1,wf(:,29))
  call vert_AV_Q(wf(:,-3),wf(:,27),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,11),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,32))
  call vert_VQ_A(wf(:,32),wf(:,0),wf(:,33))
  call prop_Q_A(wf(:,33),Q(:,13),ZERO,0_intkind1,wf(:,34))
  call vert_AV_Q(wf(:,-1),wf(:,32),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,14),ZERO,0_intkind1,wf(:,36))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,2),wf(:,37))
  call vert_SV_V(wf(:,4),wf(:,1),wf(:,38))
  call vert_QA_V(wf(:,29),wf(:,-3),wf(:,39))
  call vert_QA_Z(gZu,wf(:,29),wf(:,-3),wf(:,40))
  call vert_QA_V(wf(:,-2),wf(:,31),wf(:,41))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,31),wf(:,42))
  call vert_ST_V(wf(:,6),Q(:,19),wf(:,-5),Q(:,32),wf(:,43))
  call vert_SV_V(wf(:,-5),wf(:,8),wf(:,44))
  call vert_ST_V(wf(:,11),Q(:,35),wf(:,-4),Q(:,16),wf(:,45))
  call vert_SV_V(wf(:,-4),wf(:,13),wf(:,46))
  call vert_SV_V(wf(:,4),wf(:,2),wf(:,47))
  call vert_QA_V(wf(:,34),wf(:,-1),wf(:,48))
  call vert_QA_Z(gZu,wf(:,34),wf(:,-1),wf(:,49))
  call vert_QA_V(wf(:,0),wf(:,36),wf(:,50))
  call vert_QA_Z(gZu,wf(:,0),wf(:,36),wf(:,51))
  call vert_ST_V(wf(:,10),Q(:,28),wf(:,-5),Q(:,32),wf(:,52))
  call vert_SV_V(wf(:,-5),wf(:,12),wf(:,53))
  call vert_ST_V(wf(:,7),Q(:,44),wf(:,-4),Q(:,16),wf(:,54))
  call vert_SV_V(wf(:,-4),wf(:,9),wf(:,55))

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
  den(4) = 1 / (Q(5,48) - MH2)
  den(6) = 1 / (Q(5,19) - MZ2)
  den(9) = 1 / (Q(5,28) - MZ2)
  den(12) = 1 / (Q(5,3))
  den(13) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,11))
  den(17) = 1 / (Q(5,51) - MZ2)
  den(19) = 1 / (Q(5,12))
  den(20) = 1 / (Q(5,13))
  den(22) = 1 / (Q(5,14))
  den(24) = 1 / (Q(5,60) - MZ2)
  den(28) = 1 / (Q(5,15))
  den(30) = 1 / (Q(5,15) - MZ2)
  den(35) = 1 / (Q(5,35) - MZ2)
  den(45) = 1 / (Q(5,44) - MZ2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(12)*den(13)
  den(16) = den(12)*den(15)
  den(18) = den(1)*den(17)
  den(21) = den(19)*den(20)
  den(23) = den(19)*den(22)
  den(25) = den(2)*den(24)
  den(26) = den(1)*den(4)
  den(27) = den(17)*den(26)
  den(29) = den(14)*den(28)
  den(31) = den(14)*den(30)
  den(32) = den(16)*den(28)
  den(33) = den(16)*den(30)
  den(34) = den(7)*den(17)
  den(36) = den(1)*den(35)
  den(37) = den(17)*den(36)
  den(38) = den(2)*den(4)
  den(39) = den(24)*den(38)
  den(40) = den(21)*den(28)
  den(41) = den(21)*den(30)
  den(42) = den(23)*den(28)
  den(43) = den(23)*den(30)
  den(44) = den(10)*den(24)
  den(46) = den(2)*den(45)
  den(47) = den(24)*den(46)
  den(48) = den(4)*den(14)
  den(49) = den(4)*den(16)
  den(50) = den(4)*den(12)
  den(51) = den(4)*den(21)
  den(52) = den(4)*den(23)
  den(53) = den(4)*den(19)
  den(54) = den(4)*den(29)
  den(55) = den(4)*den(31)
  den(56) = den(4)*den(32)
  den(57) = den(4)*den(33)
  den(58) = den(4)*den(40)
  den(59) = den(4)*den(41)
  den(60) = den(4)*den(42)
  den(61) = den(4)*den(43)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(18)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_SS(wf(:,4),wf(:,5)) * den(5)
  A(3) = cont_SS(wf(:,6),wf(:,7)) * den(8)
  A(4) = cont_VV(wf(:,8),wf(:,9)) * den(8)
  A(5) = cont_SS(wf(:,10),wf(:,11)) * den(11)
  A(6) = cont_VV(wf(:,12),wf(:,13)) * den(11)

  A(7) = cont_VV(wf(:,3),wf(:,14)) * den(3)
  A(8) = cont_VV(wf(:,2),wf(:,16)) * den(3)
  A(9) = cont_SS(wf(:,4),wf(:,17)) * den(5)
  A(10) = cont_SS(wf(:,6),wf(:,18)) * den(8)
  A(11) = cont_VV(wf(:,8),wf(:,19)) * den(8)
  A(12) = cont_SS(wf(:,11),wf(:,20)) * den(11)
  A(13) = cont_VV(wf(:,13),wf(:,21)) * den(11)
  A(14) = cont_SS(wf(:,4),wf(:,22)) * den(5)
  A(15) = cont_SS(wf(:,7),wf(:,23)) * den(8)
  A(16) = cont_VV(wf(:,9),wf(:,24)) * den(8)
  A(17) = cont_SS(wf(:,10),wf(:,25)) * den(11)
  A(18) = cont_VV(wf(:,12),wf(:,26)) * den(11)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(18)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(3)+A(5))*f(1)+A(1)*f(2)+A(2)*f(7)+(A(4)+A(6))*f(10)

  M2(1) = (A(10)+A(12))*f(3)+A(7)*f(4)+(A(15)+A(17))*f(5)+A(8)*f(6)+A(9)*f(8)+A(14)*f(9)+(A(11)+A(13))*f(11)+(A(16)+A(18))*f(12)

end subroutine colourvectors

end module ol_loop_pphhjj_uuxccxhh_1_/**/REALKIND
