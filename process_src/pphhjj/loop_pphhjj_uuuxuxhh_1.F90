
module ol_colourmatrix_pphhjj_uuuxuxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphhjj_uuuxuxhh_1_/**/REALKIND



module ol_forced_parameters_pphhjj_uuuxuxhh_1_/**/REALKIND
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
end module ol_forced_parameters_pphhjj_uuuxuxhh_1_/**/REALKIND

module ol_loop_pphhjj_uuuxuxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(61), c(110)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:109)
  ! denominators
  complex(REALKIND), save :: den(115)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16)
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
    f( 3) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*cw**2*sw**2)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 5) = (3*CI*eQED**4*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 6) = (3*CI*countertermnorm*ctVqq*eQED**4*gQCD**2*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 7) = (CI*eQED**4*lambdaHZZ**2*MW**2)/(cw**4*sw**2)
    f( 8) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*lambdaHZZ**2*MW**2)/(cw**4*sw**2)
    f( 9) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*8._/**/REALKIND)
    f(10) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(11) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHWW*MH**2*SwB)/(sw**4*4._/**/REALKIND)
    f(12) = (eQED**4*gQCD**2*integralnorm*lambdaHWW**2*MW**2*SwB)/(sw**4*2._/**/REALKIND)
    f(13) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*4._/**/REALKIND)
    f(14) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(15) = (3*cw*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwB)/(sw**3*4._/**/REALKIND)
    f(16) = (3*cw*eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHWW*MH**2*SwB)/(sw**3*2._/**/REALKIND)
    f(17) = (cw*eQED**4*gQCD**2*integralnorm*MW**2*SwB)/(sw**3*4._/**/REALKIND)
    f(18) = (cw*eQED**4*gQCD**2*integralnorm*lambdaHWW**2*MW**2*SwB)/sw**3
    f(19) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(20) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(21) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw**2*sw**2*4._/**/REALKIND)
    f(22) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(23) = (eQED**4*gQCD**2*integralnorm*MH**2*SwB)/(sw**2*6._/**/REALKIND)
    f(24) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwB)/(sw**2*2._/**/REALKIND)
    f(25) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHWW*MH**2*SwB)/sw**2
    f(26) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHZZ*MH**2*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(27) = (eQED**4*gQCD**2*integralnorm*MH**2*SwB)/(MW**2*sw**2*6._/**/REALKIND)
    f(28) = (eQED**4*gQCD**2*integralnorm*MH**4*SwB)/(MW**2*sw**2*6._/**/REALKIND)
    f(29) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**4*SwB)/(MW**2*sw**2*2._/**/REALKIND)
    f(30) = (eQED**4*gQCD**2*integralnorm*MW**2*SwB)/(sw**2*6._/**/REALKIND)
    f(31) = (eQED**4*gQCD**2*integralnorm*lambdaHWW*MW**2*SwB)/(sw**2*3._/**/REALKIND)
    f(32) = (2*eQED**4*gQCD**2*integralnorm*lambdaHWW**2*MW**2*SwB)/(sw**2*3._/**/REALKIND)
    f(33) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ**2*MW**2*SwB)/(cw**4*sw**2)
    f(34) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw*sw*4._/**/REALKIND)
    f(35) = (eQED**4*gQCD**2*integralnorm*MH**2*SwB)/(cw*sw*4._/**/REALKIND)
    f(36) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwB)/(cw*sw*4._/**/REALKIND)
    f(37) = (eQED**4*gQCD**2*integralnorm*lambdaHWW*MW**2*SwB)/(cw*sw*2._/**/REALKIND)
    f(38) = -((eQED**4*gQCD**2*integralnorm*(cw - sw)*(cw + sw)*SwB)/(cw*sw**3*8._/**/REALKIND))
    f(39) = -((eQED**4*gQCD**2*integralnorm*MH**2*(cw - sw)*(cw + sw)*SwB)/(cw*MW**2*sw**3*8._/**/REALKIND))
    f(40) = -((eQED**4*gQCD**2*integralnorm*MH**4*(cw - sw)*(cw + sw)*SwB)/(cw*MW**2*sw**3*8._/**/REALKIND))
    f(41) = (-3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**4*(cw - sw)*(cw + sw)*SwB)/(cw*MW**2*sw**3*8._/**/REALKIND)
    f(42) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YB)/(MW**2*sw**2*6._/**/REALKIND)
    f(43) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YB)/(MW**2*sw**2*4._/**/REALKIND)
    f(44) = (eQED**4*gQCD**2*integralnorm*SwF*YB**2)/(MW**2*sw**2*18._/**/REALKIND)
    f(45) = (eQED**4*gQCD**2*integralnorm*SwF*YB**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(46) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YE)/(MW**2*sw**2*2._/**/REALKIND)
    f(47) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(48) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*6._/**/REALKIND)
    f(49) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(50) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YL)/(MW**2*sw**2*2._/**/REALKIND)
    f(51) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YL)/(MW**2*sw**2*4._/**/REALKIND)
    f(52) = (eQED**4*gQCD**2*integralnorm*SwF*YL**2)/(MW**2*sw**2*6._/**/REALKIND)
    f(53) = (eQED**4*gQCD**2*integralnorm*SwF*YL**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(54) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YM)/(MW**2*sw**2*2._/**/REALKIND)
    f(55) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(56) = (eQED**4*gQCD**2*integralnorm*SwF*YM**2)/(MW**2*sw**2*6._/**/REALKIND)
    f(57) = (eQED**4*gQCD**2*integralnorm*SwF*YM**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(58) = (eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YT)/(MW**2*sw**2*3._/**/REALKIND)
    f(59) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YT)/(MW**2*sw**2*4._/**/REALKIND)
    f(60) = (eQED**4*gQCD**2*integralnorm*SwF*YT**2)/(MW**2*sw**2*9._/**/REALKIND)
    f(61) = (eQED**4*gQCD**2*integralnorm*SwF*YT**2)/(MW**2*sw**2*4._/**/REALKIND)

  c = [ f(9), 3*f(9), f(10), 3*f(10), f(11), 3*f(11), f(12), 3*f(12), f(13), 3*f(13), f(14), 3*f(14), f(15), 3*f(15), f(16) &
    , 3*f(16), f(17), 3*f(17), f(18), 3*f(18), f(19), 3*f(19), f(20), 3*f(20), f(21), 3*f(21), 8*f(21), f(22), 3*f(22), 8*f(22) &
    , f(23), 3*f(23), f(24), 3*f(24), f(25), 3*f(25), f(26), 3*f(26), 8*f(26), f(27), 3*f(27), f(28), 3*f(28), f(29), 3*f(29) &
    , f(30), 3*f(30), f(31), 3*f(31), f(32), 3*f(32), f(33), 3*f(33), 8*f(33), f(34), 3*f(34), f(35), 3*f(35), f(36), 3*f(36) &
    , f(37), 3*f(37), f(38), 3*f(38), f(39), 3*f(39), f(40), 3*f(40), f(41), 3*f(41), 3*f(42), 9*f(42), 3*f(43), 9*f(43), 3*f(44) &
    , 9*f(44), 3*f(45), 9*f(45), f(46), 3*f(46), f(47), 3*f(47), f(48), 3*f(48), f(49), 3*f(49), f(50), 3*f(50), f(51), 3*f(51) &
    , f(52), 3*f(52), f(53), 3*f(53), f(54), 3*f(54), f(55), 3*f(55), f(56), 3*f(56), f(57), 3*f(57), 3*f(58), 9*f(58), 3*f(59) &
    , 9*f(59), 3*f(60), 9*f(60), 3*f(61), 9*f(61) ]
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(36)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZu,wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_Z(gZu,wf(:,-1),wf(:,-3),wf(:,2))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_QA_Z(gZu,wf(:,0),wf(:,-3),wf(:,4))
  call vert_QA_Z(gZu,wf(:,-1),wf(:,-2),wf(:,5))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,4),wf(:,6))
  call vert_SS_S(wf(:,-4),wf(:,-5),wf(:,7))
  call vert_VV_S(wf(:,1),wf(:,2),wf(:,8))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,1),Q(:,5),wf(:,9))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,2),Q(:,10),wf(:,10))
  call vert_SV_V(wf(:,-4),wf(:,1),wf(:,11))
  call vert_SV_V(wf(:,-5),wf(:,2),wf(:,12))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,2),Q(:,10),wf(:,13))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,1),Q(:,5),wf(:,14))
  call vert_SV_V(wf(:,-4),wf(:,2),wf(:,15))
  call vert_SV_V(wf(:,-5),wf(:,1),wf(:,16))
  call vert_VV_S(wf(:,5),wf(:,4),wf(:,17))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,4),Q(:,9),wf(:,18))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,5),Q(:,6),wf(:,19))
  call vert_SV_V(wf(:,-4),wf(:,4),wf(:,20))
  call vert_SV_V(wf(:,-5),wf(:,5),wf(:,21))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,5),Q(:,6),wf(:,22))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,4),Q(:,9),wf(:,23))
  call vert_SV_V(wf(:,-4),wf(:,5),wf(:,24))
  call vert_SV_V(wf(:,-5),wf(:,4),wf(:,25))
  call counter_QA_Z(gZu,wf(:,-1),wf(:,-3),wf(:,26))
  call counter_QA_Z(gZu,wf(:,-1),wf(:,-2),wf(:,27))
  call counter_QA_Z(gZu,wf(:,0),wf(:,-3),wf(:,28))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,28),wf(:,29))
  call counter_QA_Z(gZu,wf(:,0),wf(:,-2),wf(:,30))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,30),wf(:,31))
  call vert_VV_S(wf(:,1),wf(:,26),wf(:,32))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,26),Q(:,10),wf(:,33))
  call vert_SV_V(wf(:,-5),wf(:,26),wf(:,34))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,26),Q(:,10),wf(:,35))
  call vert_SV_V(wf(:,-4),wf(:,26),wf(:,36))
  call vert_VV_S(wf(:,27),wf(:,4),wf(:,37))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,27),Q(:,6),wf(:,38))
  call vert_SV_V(wf(:,-5),wf(:,27),wf(:,39))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,27),Q(:,6),wf(:,40))
  call vert_SV_V(wf(:,-4),wf(:,27),wf(:,41))
  call vert_VV_S(wf(:,5),wf(:,28),wf(:,42))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,28),Q(:,9),wf(:,43))
  call vert_SV_V(wf(:,-4),wf(:,28),wf(:,44))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,28),Q(:,9),wf(:,45))
  call vert_SV_V(wf(:,-5),wf(:,28),wf(:,46))
  call vert_VV_S(wf(:,30),wf(:,2),wf(:,47))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,30),Q(:,5),wf(:,48))
  call vert_SV_V(wf(:,-4),wf(:,30),wf(:,49))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,30),Q(:,5),wf(:,50))
  call vert_SV_V(wf(:,-5),wf(:,30),wf(:,51))
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,52))
  call vert_VQ_A(wf(:,52),wf(:,-1),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,7),ZERO,0_intkind1,wf(:,54))
  call vert_AV_Q(wf(:,-3),wf(:,52),wf(:,55))
  call prop_A_Q(wf(:,55),Q(:,13),ZERO,0_intkind1,wf(:,56))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,57))
  call vert_VQ_A(wf(:,57),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,7),ZERO,0_intkind1,wf(:,59))
  call vert_AV_Q(wf(:,-3),wf(:,57),wf(:,60))
  call prop_A_Q(wf(:,60),Q(:,14),ZERO,0_intkind1,wf(:,61))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,5),wf(:,62))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,63))
  call vert_VQ_A(wf(:,63),wf(:,-1),wf(:,64))
  call prop_Q_A(wf(:,64),Q(:,11),ZERO,0_intkind1,wf(:,65))
  call vert_AV_Q(wf(:,-2),wf(:,63),wf(:,66))
  call prop_A_Q(wf(:,66),Q(:,13),ZERO,0_intkind1,wf(:,67))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,68))
  call vert_VQ_A(wf(:,68),wf(:,0),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,11),ZERO,0_intkind1,wf(:,70))
  call vert_AV_Q(wf(:,-2),wf(:,68),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,14),ZERO,0_intkind1,wf(:,72))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,2),wf(:,73))
  call vert_SV_V(wf(:,7),wf(:,1),wf(:,74))
  call vert_QA_V(wf(:,54),wf(:,-3),wf(:,75))
  call vert_QA_Z(gZu,wf(:,54),wf(:,-3),wf(:,76))
  call vert_QA_V(wf(:,-1),wf(:,56),wf(:,77))
  call vert_QA_Z(gZu,wf(:,-1),wf(:,56),wf(:,78))
  call vert_ST_V(wf(:,9),Q(:,21),wf(:,-5),Q(:,32),wf(:,79))
  call vert_SV_V(wf(:,-5),wf(:,11),wf(:,80))
  call vert_ST_V(wf(:,14),Q(:,37),wf(:,-4),Q(:,16),wf(:,81))
  call vert_SV_V(wf(:,-4),wf(:,16),wf(:,82))
  call vert_SV_V(wf(:,7),wf(:,5),wf(:,83))
  call vert_QA_V(wf(:,59),wf(:,-3),wf(:,84))
  call vert_QA_Z(gZu,wf(:,59),wf(:,-3),wf(:,85))
  call vert_QA_V(wf(:,0),wf(:,61),wf(:,86))
  call vert_QA_Z(gZu,wf(:,0),wf(:,61),wf(:,87))
  call vert_ST_V(wf(:,22),Q(:,22),wf(:,-5),Q(:,32),wf(:,88))
  call vert_SV_V(wf(:,-5),wf(:,24),wf(:,89))
  call vert_ST_V(wf(:,19),Q(:,38),wf(:,-4),Q(:,16),wf(:,90))
  call vert_SV_V(wf(:,-4),wf(:,21),wf(:,91))
  call vert_SV_V(wf(:,7),wf(:,4),wf(:,92))
  call vert_QA_V(wf(:,65),wf(:,-2),wf(:,93))
  call vert_QA_Z(gZu,wf(:,65),wf(:,-2),wf(:,94))
  call vert_QA_V(wf(:,-1),wf(:,67),wf(:,95))
  call vert_QA_Z(gZu,wf(:,-1),wf(:,67),wf(:,96))
  call vert_ST_V(wf(:,18),Q(:,25),wf(:,-5),Q(:,32),wf(:,97))
  call vert_SV_V(wf(:,-5),wf(:,20),wf(:,98))
  call vert_ST_V(wf(:,23),Q(:,41),wf(:,-4),Q(:,16),wf(:,99))
  call vert_SV_V(wf(:,-4),wf(:,25),wf(:,100))
  call vert_SV_V(wf(:,7),wf(:,2),wf(:,101))
  call vert_QA_V(wf(:,70),wf(:,-2),wf(:,102))
  call vert_QA_Z(gZu,wf(:,70),wf(:,-2),wf(:,103))
  call vert_QA_V(wf(:,0),wf(:,72),wf(:,104))
  call vert_QA_Z(gZu,wf(:,0),wf(:,72),wf(:,105))
  call vert_ST_V(wf(:,13),Q(:,26),wf(:,-5),Q(:,32),wf(:,106))
  call vert_SV_V(wf(:,-5),wf(:,15),wf(:,107))
  call vert_ST_V(wf(:,10),Q(:,42),wf(:,-4),Q(:,16),wf(:,108))
  call vert_SV_V(wf(:,-4),wf(:,12),wf(:,109))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MZ2)
  den(2) = 1 / (Q(5,10) - MZ2)
  den(4) = 1 / (Q(5,9) - MZ2)
  den(5) = 1 / (Q(5,6) - MZ2)
  den(7) = 1 / (Q(5,48) - MH2)
  den(9) = 1 / (Q(5,21) - MZ2)
  den(12) = 1 / (Q(5,26) - MZ2)
  den(16) = 1 / (Q(5,25) - MZ2)
  den(19) = 1 / (Q(5,22) - MZ2)
  den(22) = 1 / (Q(5,5))
  den(23) = 1 / (Q(5,7))
  den(25) = 1 / (Q(5,13))
  den(27) = 1 / (Q(5,53) - MZ2)
  den(29) = 1 / (Q(5,6))
  den(31) = 1 / (Q(5,14))
  den(33) = 1 / (Q(5,54) - MZ2)
  den(35) = 1 / (Q(5,9))
  den(36) = 1 / (Q(5,11))
  den(39) = 1 / (Q(5,57) - MZ2)
  den(41) = 1 / (Q(5,10))
  den(44) = 1 / (Q(5,58) - MZ2)
  den(48) = 1 / (Q(5,15))
  den(50) = 1 / (Q(5,15) - MZ2)
  den(55) = 1 / (Q(5,37) - MZ2)
  den(65) = 1 / (Q(5,38) - MZ2)
  den(75) = 1 / (Q(5,41) - MZ2)
  den(85) = 1 / (Q(5,42) - MZ2)

  ! denominators
  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(15) = den(6)*den(7)
  den(17) = den(4)*den(16)
  den(18) = den(5)*den(17)
  den(20) = den(5)*den(19)
  den(21) = den(4)*den(20)
  den(24) = den(22)*den(23)
  den(26) = den(22)*den(25)
  den(28) = den(1)*den(27)
  den(30) = den(23)*den(29)
  den(32) = den(29)*den(31)
  den(34) = den(5)*den(33)
  den(37) = den(35)*den(36)
  den(38) = den(25)*den(35)
  den(40) = den(4)*den(39)
  den(42) = den(36)*den(41)
  den(43) = den(31)*den(41)
  den(45) = den(2)*den(44)
  den(46) = den(1)*den(7)
  den(47) = den(27)*den(46)
  den(49) = den(24)*den(48)
  den(51) = den(24)*den(50)
  den(52) = den(26)*den(48)
  den(53) = den(26)*den(50)
  den(54) = den(10)*den(27)
  den(56) = den(1)*den(55)
  den(57) = den(27)*den(56)
  den(58) = den(5)*den(7)
  den(59) = den(33)*den(58)
  den(60) = den(30)*den(48)
  den(61) = den(30)*den(50)
  den(62) = den(32)*den(48)
  den(63) = den(32)*den(50)
  den(64) = den(20)*den(33)
  den(66) = den(5)*den(65)
  den(67) = den(33)*den(66)
  den(68) = den(4)*den(7)
  den(69) = den(39)*den(68)
  den(70) = den(37)*den(48)
  den(71) = den(37)*den(50)
  den(72) = den(38)*den(48)
  den(73) = den(38)*den(50)
  den(74) = den(17)*den(39)
  den(76) = den(4)*den(75)
  den(77) = den(39)*den(76)
  den(78) = den(2)*den(7)
  den(79) = den(44)*den(78)
  den(80) = den(42)*den(48)
  den(81) = den(42)*den(50)
  den(82) = den(43)*den(48)
  den(83) = den(43)*den(50)
  den(84) = den(13)*den(44)
  den(86) = den(2)*den(85)
  den(87) = den(44)*den(86)
  den(88) = den(7)*den(24)
  den(89) = den(7)*den(26)
  den(90) = den(7)*den(22)
  den(91) = den(7)*den(30)
  den(92) = den(7)*den(32)
  den(93) = den(7)*den(29)
  den(94) = den(7)*den(37)
  den(95) = den(7)*den(38)
  den(96) = den(7)*den(35)
  den(97) = den(7)*den(42)
  den(98) = den(7)*den(43)
  den(99) = den(7)*den(41)
  den(100) = den(7)*den(49)
  den(101) = den(7)*den(51)
  den(102) = den(7)*den(52)
  den(103) = den(7)*den(53)
  den(104) = den(7)*den(60)
  den(105) = den(7)*den(61)
  den(106) = den(7)*den(62)
  den(107) = den(7)*den(63)
  den(108) = den(7)*den(70)
  den(109) = den(7)*den(71)
  den(110) = den(7)*den(72)
  den(111) = den(7)*den(73)
  den(112) = den(7)*den(80)
  den(113) = den(7)*den(81)
  den(114) = den(7)*den(82)
  den(115) = den(7)*den(83)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(36)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,5),wf(:,6)) * den(6)
  A(3) = cont_SS(wf(:,7),wf(:,8)) * den(8)
  A(4) = cont_SS(wf(:,9),wf(:,10)) * den(11)
  A(5) = cont_VV(wf(:,11),wf(:,12)) * den(11)
  A(6) = cont_SS(wf(:,13),wf(:,14)) * den(14)
  A(7) = cont_VV(wf(:,15),wf(:,16)) * den(14)
  A(8) = cont_SS(wf(:,7),wf(:,17)) * den(15)
  A(9) = cont_SS(wf(:,18),wf(:,19)) * den(18)
  A(10) = cont_VV(wf(:,20),wf(:,21)) * den(18)
  A(11) = cont_SS(wf(:,22),wf(:,23)) * den(21)
  A(12) = cont_VV(wf(:,24),wf(:,25)) * den(21)

  A(13) = cont_VV(wf(:,3),wf(:,26)) * den(3)
  A(14) = cont_VV(wf(:,6),wf(:,27)) * den(6)
  A(15) = cont_VV(wf(:,5),wf(:,29)) * den(6)
  A(16) = cont_VV(wf(:,2),wf(:,31)) * den(3)
  A(17) = cont_SS(wf(:,7),wf(:,32)) * den(8)
  A(18) = cont_SS(wf(:,9),wf(:,33)) * den(11)
  A(19) = cont_VV(wf(:,11),wf(:,34)) * den(11)
  A(20) = cont_SS(wf(:,14),wf(:,35)) * den(14)
  A(21) = cont_VV(wf(:,16),wf(:,36)) * den(14)
  A(22) = cont_SS(wf(:,7),wf(:,37)) * den(15)
  A(23) = cont_SS(wf(:,18),wf(:,38)) * den(18)
  A(24) = cont_VV(wf(:,20),wf(:,39)) * den(18)
  A(25) = cont_SS(wf(:,23),wf(:,40)) * den(21)
  A(26) = cont_VV(wf(:,25),wf(:,41)) * den(21)
  A(27) = cont_SS(wf(:,7),wf(:,42)) * den(15)
  A(28) = cont_SS(wf(:,19),wf(:,43)) * den(18)
  A(29) = cont_VV(wf(:,21),wf(:,44)) * den(18)
  A(30) = cont_SS(wf(:,22),wf(:,45)) * den(21)
  A(31) = cont_VV(wf(:,24),wf(:,46)) * den(21)
  A(32) = cont_SS(wf(:,7),wf(:,47)) * den(8)
  A(33) = cont_SS(wf(:,10),wf(:,48)) * den(11)
  A(34) = cont_VV(wf(:,12),wf(:,49)) * den(11)
  A(35) = cont_SS(wf(:,13),wf(:,50)) * den(14)
  A(36) = cont_VV(wf(:,15),wf(:,51)) * den(14)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(36)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (A(9)+A(11))*f(1)+A(2)*f(2)+A(8)*f(5)+(A(10)+A(12))*f(7)
  M1(2) = (-A(4)-A(6))*f(1)-A(1)*f(2)-A(3)*f(5)+(-A(5)-A(7))*f(7)

  M2(1) = (A(23)+A(25)+A(28)+A(30))*f(3)+(A(14)+A(15))*f(4)+(A(22)+A(27))*f(6)+(A(24)+A(26)+A(29)+A(31))*f(8)
  M2(2) = (-A(18)-A(20)-A(33)-A(35))*f(3)+(-A(13)-A(16))*f(4)+(-A(17)-A(32))*f(6)+(-A(19)-A(21)-A(34)-A(36))*f(8)

end subroutine colourvectors

end module ol_loop_pphhjj_uuuxuxhh_1_/**/REALKIND
