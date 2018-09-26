
module ol_colourmatrix_ppllllj2_nenexeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,2), KL(0,2), KL2(2,2), KL2ct(2,2), KL2ct2(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [  7, -2]
  KL2(2,:) = [ -2,  7]
  KL2 = (1._/**/REALKIND / 3) * KL2

  KL2ct(1,:) = [  7, -2]
  KL2ct(2,:) = [ -2,  7]
  KL2ct = (1._/**/REALKIND / 3) * KL2ct

  KL2ct2(1,:) = [  7, -2]
  KL2ct2(2,:) = [ -2,  7]
  KL2ct2 = (1._/**/REALKIND / 3) * KL2ct2

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj2_nenexeexggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nenexeexggg_1_/**/REALKIND
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
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllllj2_nenexeexggg_1_/**/REALKIND

module ol_loop_ppllllj2_nenexeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(67), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:103)
  ! denominators
  complex(REALKIND), save :: den(200)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128), Mct(2,128), Mcol_loop(2,128)
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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*countertermnorm*eQED**4*gQCD**3
    f( 2) = countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 3) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 4) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 5) = (CI*countertermnorm*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 6) = (CI*countertermnorm*ctZGG*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 7) = (CI*countertermnorm*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 8) = (countertermnorm*ctWWGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f( 9) = (CI*countertermnorm*ctZGG*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f(13) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(14) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f(15) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(16) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(17) = eQED**4*gQCD**3*integralnorm*SwF
    f(18) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(19) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(20) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(4._/**/REALKIND*sw**4)
    f(21) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**4)
    f(22) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(23) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(24) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**3)
    f(25) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(26) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(27) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(28) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(6._/**/REALKIND*sw**2)
    f(29) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(3._/**/REALKIND*sw**2)
    f(30) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(31) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/(3._/**/REALKIND*sw**2)
    f(32) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/sw**2
    f(33) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(34) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(35) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(36) = (2*eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(37) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2
    f(38) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHWW*MB*MW*YB)/(MQ2sum*sw**3*2._/**/REALKIND)
    f(39) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(40) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHWW*SwF*YB)/(4._/**/REALKIND*sw**4)
    f(41) = (eQED**4*gQCD**3*integralnorm*lambdaHWW*SwF*YB)/(sw**4*4._/**/REALKIND)
    f(42) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(2._/**/REALKIND*cw**2*sw**2)
    f(43) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(44) = (CI*countertermnorm*eQED**4*gQCD**3*lambdaHZZ*YE)/(2._/**/REALKIND*cw**2*sw**2)
    f(45) = (CI*countertermnorm*ctZGG*eQED**4*gQCD**3*lambdaHZZ*YE)/(2._/**/REALKIND*cw**2*sw**2)
    f(46) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(2._/**/REALKIND*cw**2*sw**2)
    f(47) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(48) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(49) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(50) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE)/(MQ2sum*MW*sw**3*4._/**/REALKIND)
    f(51) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE)/(MQ2sum*MW*sw*2._/**/REALKIND)
    f(52) = (CI*eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(8._/**/REALKIND*MW**2*sw**4)
    f(53) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(MW**2*sw**4*8._/**/REALKIND)
    f(54) = (CI*eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(4._/**/REALKIND*MW**2*sw**2)
    f(55) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(56) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHWW*MT*MW*YT)/(MQ2sum*sw**3*2._/**/REALKIND)
    f(57) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(58) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHWW*SwF*YT)/(4._/**/REALKIND*sw**4)
    f(59) = (eQED**4*gQCD**3*integralnorm*lambdaHWW*SwF*YT)/(sw**4*4._/**/REALKIND)
    f(60) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(2._/**/REALKIND*cw**2*sw**2)
    f(61) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(62) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE*YT)/(MQ2sum*MW*sw**3*4._/**/REALKIND)
    f(63) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE*YT)/(MQ2sum*MW*sw*2._/**/REALKIND)
    f(64) = (CI*eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(8._/**/REALKIND*MW**2*sw**4)
    f(65) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(MW**2*sw**4*8._/**/REALKIND)
    f(66) = (CI*eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(4._/**/REALKIND*MW**2*sw**2)
    f(67) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)


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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  integer,           intent(in), optional  :: POLSEL(7)
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(105)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), POLSEL(7))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-6),wf(:,5))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,12),MZ,1_intkind1,wf(:,7))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-6),wf(:,8))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,9))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-5),wf(:,10))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-5),wf(:,11))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,12))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-4),wf(:,13))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-4),wf(:,14))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,15))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,16))
  call prop_W_W(wf(:,16),Q(:,112),MZ,1_intkind1,wf(:,17))
  call vert_SV_V(wf(:,15),wf(:,4),wf(:,18))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,19))
  call prop_W_W(wf(:,19),Q(:,112),MZ,1_intkind1,wf(:,20))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,21))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-2),wf(:,22))
  call vert_AV_Q(wf(:,-3),wf(:,21),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,7),ZERO,0_intkind1,wf(:,24))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,25))
  call vert_AV_Q(wf(:,-3),wf(:,25),wf(:,26))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,17),wf(:,27))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,20),wf(:,28))
  call vert_VQ_A(wf(:,21),wf(:,-2),wf(:,29))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,116),ZERO,0_intkind1,wf(:,31))
  call vert_VQ_A(wf(:,25),wf(:,-2),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,116),ZERO,0_intkind1,wf(:,33))
  call vert_ZQ_A(gZl,wf(:,17),wf(:,-2),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,116),ZERO,0_intkind1,wf(:,35))
  call vert_ZQ_A(gZl,wf(:,20),wf(:,-2),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,116),ZERO,0_intkind1,wf(:,37))
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,38))
  call vert_QA_W(wf(:,-2),wf(:,-1),wf(:,39))
  call prop_W_W(wf(:,38),Q(:,9),MW,1_intkind1,wf(:,40))
  call prop_W_W(wf(:,39),Q(:,6),MW,1_intkind1,wf(:,41))
  call counter_VVG_G(wf(:,41),wf(:,40),wf(:,-6),wf(:,42))
  call counter_VVG_G(wf(:,41),wf(:,40),wf(:,-5),wf(:,43))
  call counter_VVG_G(wf(:,41),wf(:,40),wf(:,-4),wf(:,44))
  call vert_UV_W(wf(:,40),Q(:,9),wf(:,41),Q(:,6),wf(:,45))
  call vert_WQ_A(wf(:,41),wf(:,0),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,7),ZERO,0_intkind1,wf(:,47))
  call vert_ZQ_A(gZn,wf(:,17),wf(:,0),wf(:,48))
  call vert_AW_Q(wf(:,-3),wf(:,41),wf(:,49))
  call prop_Q_A(wf(:,48),Q(:,113),ZERO,0_intkind1,wf(:,50))
  call vert_ZQ_A(gZn,wf(:,20),wf(:,0),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,113),ZERO,0_intkind1,wf(:,52))
  call vert_AW_Q(wf(:,-1),wf(:,40),wf(:,53))
  call prop_A_Q(wf(:,53),Q(:,11),ZERO,0_intkind1,wf(:,54))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,17),wf(:,55))
  call vert_WQ_A(wf(:,40),wf(:,-2),wf(:,56))
  call prop_A_Q(wf(:,55),Q(:,114),ZERO,0_intkind1,wf(:,57))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,20),wf(:,58))
  call prop_A_Q(wf(:,58),Q(:,114),ZERO,0_intkind1,wf(:,59))
  call vert_ZQ_A(gZn,wf(:,7),wf(:,0),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,13),ZERO,0_intkind1,wf(:,61))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,7),wf(:,62))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,63))
  call prop_W_W(wf(:,18),Q(:,15),MZ,1_intkind1,wf(:,64))
  call vert_VV_S(wf(:,4),wf(:,7),wf(:,65))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,66))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,9),Q(:,80),wf(:,67))
  call counter_GG_S(wf(:,-5),wf(:,9),wf(:,68))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,12),Q(:,96),wf(:,69))
  call counter_GG_S(wf(:,-4),wf(:,12),wf(:,70))
  call vert_AQ_S(gH,wf(:,-3),wf(:,24),wf(:,71))
  call prop_W_W(wf(:,63),Q(:,112),MZ,1_intkind1,wf(:,72))
  call vert_QA_Z(gZl,wf(:,24),wf(:,-3),wf(:,73))
  call prop_A_Q(wf(:,30),Q(:,11),ZERO,0_intkind1,wf(:,74))
  call vert_AQ_S(gH,wf(:,74),wf(:,-2),wf(:,75))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,74),wf(:,76))
  call prop_W_W(wf(:,67),Q(:,112),MZ,1_intkind1,wf(:,77))
  call prop_W_W(wf(:,69),Q(:,112),MZ,1_intkind1,wf(:,78))
  call vert_VV_S(wf(:,41),wf(:,40),wf(:,79))
  call prop_W_W(wf(:,45),Q(:,15),MZ,1_intkind1,wf(:,80))
  call vert_AQ_S(gH,wf(:,-3),wf(:,47),wf(:,81))
  call vert_QA_Z(gZl,wf(:,47),wf(:,-3),wf(:,82))
  call prop_A_Q(wf(:,49),Q(:,14),ZERO,0_intkind1,wf(:,83))
  call vert_QA_Z(gZn,wf(:,0),wf(:,83),wf(:,84))
  call vert_AQ_S(gH,wf(:,54),wf(:,-2),wf(:,85))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,54),wf(:,86))
  call prop_Q_A(wf(:,56),Q(:,13),ZERO,0_intkind1,wf(:,87))
  call vert_QA_Z(gZn,wf(:,87),wf(:,-1),wf(:,88))
  call vert_QA_Z(gZn,wf(:,61),wf(:,-1),wf(:,89))
  call prop_A_Q(wf(:,62),Q(:,14),ZERO,0_intkind1,wf(:,90))
  call vert_QA_Z(gZn,wf(:,0),wf(:,90),wf(:,91))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,92))
  call prop_W_W(wf(:,73),Q(:,15),MZ,1_intkind1,wf(:,93))
  call vert_QA_V(wf(:,-2),wf(:,74),wf(:,94))
  call prop_W_W(wf(:,76),Q(:,15),MZ,1_intkind1,wf(:,95))
  call vert_QA_V(wf(:,47),wf(:,-3),wf(:,96))
  call prop_W_W(wf(:,82),Q(:,15),MZ,1_intkind1,wf(:,97))
  call prop_W_W(wf(:,84),Q(:,15),MZ,1_intkind1,wf(:,98))
  call vert_QA_V(wf(:,-2),wf(:,54),wf(:,99))
  call prop_W_W(wf(:,86),Q(:,15),MZ,1_intkind1,wf(:,100))
  call prop_W_W(wf(:,88),Q(:,15),MZ,1_intkind1,wf(:,101))
  call prop_W_W(wf(:,89),Q(:,15),MZ,1_intkind1,wf(:,102))
  call prop_W_W(wf(:,91),Q(:,15),MZ,1_intkind1,wf(:,103))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(9) = 1 / (Q(5,80))
  den(12) = 1 / (Q(5,96))
  den(15) = 1 / (Q(5,12) - MH2)
  den(16) = 1 / (Q(5,112) - MZ2)
  den(19) = 1 / (Q(5,112))
  den(20) = 1 / (Q(5,7))
  den(24) = 1 / (Q(5,116))
  den(29) = 1 / (Q(5,9) - MW2)
  den(30) = 1 / (Q(5,6) - MW2)
  den(40) = 1 / (Q(5,113))
  den(43) = 1 / (Q(5,11))
  den(47) = 1 / (Q(5,114))
  den(50) = 1 / (Q(5,13))
  den(54) = 1 / (Q(5,15) - MZ2)
  den(57) = 1 / (Q(5,15) - MH2)
  den(64) = 1 / (Q(5,112) - MH2)
  den(94) = 1 / (Q(5,14))
  den(121) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(4)*den(9)
  den(11) = den(7)*den(9)
  den(13) = den(4)*den(12)
  den(14) = den(7)*den(12)
  den(17) = den(1)*den(15)
  den(18) = den(16)*den(17)
  den(21) = den(1)*den(20)
  den(22) = den(19)*den(21)
  den(23) = den(16)*den(21)
  den(25) = den(19)*den(24)
  den(26) = den(1)*den(25)
  den(27) = den(16)*den(24)
  den(28) = den(1)*den(27)
  den(31) = den(29)*den(30)
  den(32) = den(3)*den(31)
  den(33) = den(9)*den(31)
  den(34) = den(12)*den(31)
  den(35) = den(19)*den(31)
  den(36) = den(16)*den(31)
  den(37) = den(20)*den(30)
  den(38) = den(19)*den(37)
  den(39) = den(16)*den(37)
  den(41) = den(16)*den(40)
  den(42) = den(30)*den(41)
  den(44) = den(29)*den(43)
  den(45) = den(19)*den(44)
  den(46) = den(16)*den(44)
  den(48) = den(16)*den(47)
  den(49) = den(29)*den(48)
  den(51) = den(6)*den(50)
  den(52) = den(16)*den(51)
  den(53) = den(6)*den(41)
  den(55) = den(17)*den(54)
  den(56) = den(3)*den(55)
  den(58) = den(7)*den(57)
  den(59) = den(3)*den(58)
  den(60) = den(9)*den(55)
  den(61) = den(9)*den(58)
  den(62) = den(12)*den(55)
  den(63) = den(12)*den(58)
  den(65) = den(3)*den(64)
  den(66) = den(21)*den(65)
  den(67) = den(3)*den(16)
  den(68) = den(21)*den(67)
  den(69) = den(1)*den(43)
  den(70) = den(65)*den(69)
  den(71) = den(67)*den(69)
  den(72) = den(9)*den(64)
  den(73) = den(21)*den(72)
  den(74) = den(9)*den(16)
  den(75) = den(21)*den(74)
  den(76) = den(69)*den(72)
  den(77) = den(69)*den(74)
  den(78) = den(12)*den(64)
  den(79) = den(21)*den(78)
  den(80) = den(12)*den(16)
  den(81) = den(21)*den(80)
  den(82) = den(69)*den(78)
  den(83) = den(69)*den(80)
  den(84) = den(31)*den(57)
  den(85) = den(3)*den(84)
  den(86) = den(31)*den(54)
  den(87) = den(3)*den(86)
  den(88) = den(9)*den(84)
  den(89) = den(9)*den(86)
  den(90) = den(12)*den(84)
  den(91) = den(12)*den(86)
  den(92) = den(37)*den(65)
  den(93) = den(37)*den(67)
  den(95) = den(30)*den(94)
  den(96) = den(67)*den(95)
  den(97) = den(37)*den(72)
  den(98) = den(37)*den(74)
  den(99) = den(74)*den(95)
  den(100) = den(37)*den(78)
  den(101) = den(37)*den(80)
  den(102) = den(80)*den(95)
  den(103) = den(44)*den(65)
  den(104) = den(44)*den(67)
  den(105) = den(29)*den(50)
  den(106) = den(67)*den(105)
  den(107) = den(44)*den(72)
  den(108) = den(44)*den(74)
  den(109) = den(74)*den(105)
  den(110) = den(44)*den(78)
  den(111) = den(44)*den(80)
  den(112) = den(80)*den(105)
  den(113) = den(51)*den(67)
  den(114) = den(6)*den(94)
  den(115) = den(67)*den(114)
  den(116) = den(51)*den(74)
  den(117) = den(74)*den(114)
  den(118) = den(51)*den(80)
  den(119) = den(80)*den(114)
  den(120) = den(21)*den(57)
  den(122) = den(21)*den(121)
  den(123) = den(21)*den(54)
  den(124) = den(57)*den(69)
  den(125) = den(69)*den(121)
  den(126) = den(54)*den(69)
  den(127) = den(31)*den(121)
  den(128) = den(37)*den(57)
  den(129) = den(37)*den(121)
  den(130) = den(37)*den(54)
  den(131) = den(54)*den(95)
  den(132) = den(44)*den(57)
  den(133) = den(44)*den(121)
  den(134) = den(44)*den(54)
  den(135) = den(54)*den(105)
  den(136) = den(51)*den(54)
  den(137) = den(54)*den(114)
  den(138) = den(1)*den(3)*den(15)
  den(139) = den(1)*den(2)*den(3)
  den(140) = den(1)*den(3)*den(6)
  den(141) = den(1)*den(9)*den(15)
  den(142) = den(1)*den(2)*den(9)
  den(143) = den(1)*den(6)*den(9)
  den(144) = den(1)*den(12)*den(15)
  den(145) = den(1)*den(2)*den(12)
  den(146) = den(1)*den(6)*den(12)
  den(147) = den(3)*den(120)
  den(148) = den(3)*den(122)
  den(149) = den(3)*den(123)
  den(150) = den(3)*den(124)
  den(151) = den(3)*den(125)
  den(152) = den(3)*den(126)
  den(153) = den(9)*den(120)
  den(154) = den(9)*den(122)
  den(155) = den(9)*den(123)
  den(156) = den(9)*den(124)
  den(157) = den(9)*den(125)
  den(158) = den(9)*den(126)
  den(159) = den(12)*den(120)
  den(160) = den(12)*den(122)
  den(161) = den(12)*den(123)
  den(162) = den(12)*den(124)
  den(163) = den(12)*den(125)
  den(164) = den(12)*den(126)
  den(165) = den(3)*den(127)
  den(166) = den(3)*den(29)*den(30)
  den(167) = den(9)*den(127)
  den(168) = den(9)*den(29)*den(30)
  den(169) = den(12)*den(127)
  den(170) = den(12)*den(29)*den(30)
  den(171) = den(3)*den(128)
  den(172) = den(3)*den(129)
  den(173) = den(3)*den(130)
  den(174) = den(3)*den(131)
  den(175) = den(9)*den(128)
  den(176) = den(9)*den(129)
  den(177) = den(9)*den(130)
  den(178) = den(9)*den(131)
  den(179) = den(12)*den(128)
  den(180) = den(12)*den(129)
  den(181) = den(12)*den(130)
  den(182) = den(12)*den(131)
  den(183) = den(3)*den(132)
  den(184) = den(3)*den(133)
  den(185) = den(3)*den(134)
  den(186) = den(3)*den(135)
  den(187) = den(9)*den(132)
  den(188) = den(9)*den(133)
  den(189) = den(9)*den(134)
  den(190) = den(9)*den(135)
  den(191) = den(12)*den(132)
  den(192) = den(12)*den(133)
  den(193) = den(12)*den(134)
  den(194) = den(12)*den(135)
  den(195) = den(3)*den(136)
  den(196) = den(3)*den(137)
  den(197) = den(9)*den(136)
  den(198) = den(9)*den(137)
  den(199) = den(12)*den(136)
  den(200) = den(12)*den(137)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(105)


  A(1) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,8)) * den(8)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(10)
  A(4) = cont_VV(wf(:,9),wf(:,11)) * den(11)
  A(5) = cont_VV(wf(:,12),wf(:,13)) * den(13)
  A(6) = cont_VV(wf(:,12),wf(:,14)) * den(14)
  A(7) = cont_VV(wf(:,17),wf(:,18)) * den(18)
  A(8) = cont_VV(wf(:,18),wf(:,20)) * den(18)
  A(9) = cont_QA(wf(:,23),wf(:,24)) * den(22)
  A(10) = cont_QA(wf(:,24),wf(:,26)) * den(22)
  A(11) = cont_QA(wf(:,24),wf(:,27)) * den(23)
  A(12) = cont_QA(wf(:,24),wf(:,28)) * den(23)
  A(13) = cont_QA(wf(:,30),wf(:,31)) * den(26)
  A(14) = cont_QA(wf(:,30),wf(:,33)) * den(26)
  A(15) = cont_QA(wf(:,30),wf(:,35)) * den(28)
  A(16) = cont_QA(wf(:,30),wf(:,37)) * den(28)
  A(17) = cont_VV(wf(:,3),wf(:,42)) * den(32)
  A(18) = cont_VV(wf(:,9),wf(:,43)) * den(33)
  A(19) = cont_VV(wf(:,12),wf(:,44)) * den(34)
  A(20) = cont_VV(wf(:,21),wf(:,45)) * den(35)
  A(21) = cont_VV(wf(:,25),wf(:,45)) * den(35)
  A(22) = cont_VV(wf(:,17),wf(:,45)) * den(36)
  A(23) = cont_VV(wf(:,20),wf(:,45)) * den(36)
  A(24) = cont_QA(wf(:,23),wf(:,47)) * den(38)
  A(25) = cont_QA(wf(:,26),wf(:,47)) * den(38)
  A(26) = cont_QA(wf(:,27),wf(:,47)) * den(39)
  A(27) = cont_QA(wf(:,28),wf(:,47)) * den(39)
  A(28) = cont_QA(wf(:,49),wf(:,50)) * den(42)
  A(29) = cont_QA(wf(:,49),wf(:,52)) * den(42)
  A(30) = cont_QA(wf(:,29),wf(:,54)) * den(45)
  A(31) = cont_QA(wf(:,32),wf(:,54)) * den(45)
  A(32) = cont_QA(wf(:,34),wf(:,54)) * den(46)
  A(33) = cont_QA(wf(:,36),wf(:,54)) * den(46)
  A(34) = cont_QA(wf(:,56),wf(:,57)) * den(49)
  A(35) = cont_QA(wf(:,56),wf(:,59)) * den(49)
  A(36) = cont_QA(wf(:,55),wf(:,61)) * den(52)
  A(37) = cont_QA(wf(:,58),wf(:,61)) * den(52)
  A(38) = cont_QA(wf(:,50),wf(:,62)) * den(53)
  A(39) = cont_QA(wf(:,52),wf(:,62)) * den(53)
  A(40) = cont_VV(wf(:,63),wf(:,64)) * den(56)
  A(41) = cont_SS(wf(:,65),wf(:,66)) * den(59)
  A(42) = cont_SS(wf(:,65),wf(:,66)) * den(59)
  A(43) = cont_VV(wf(:,64),wf(:,67)) * den(60)
  A(44) = cont_SS(wf(:,65),wf(:,68)) * den(61)
  A(45) = cont_SS(wf(:,65),wf(:,68)) * den(61)
  A(46) = cont_VV(wf(:,64),wf(:,69)) * den(62)
  A(47) = cont_SS(wf(:,65),wf(:,70)) * den(63)
  A(48) = cont_SS(wf(:,65),wf(:,70)) * den(63)
  A(49) = cont_SS(wf(:,66),wf(:,71)) * den(66)
  A(50) = cont_SS(wf(:,66),wf(:,71)) * den(66)
  A(51) = cont_VV(wf(:,72),wf(:,73)) * den(68)
  A(52) = cont_SS(wf(:,66),wf(:,75)) * den(70)
  A(53) = cont_SS(wf(:,66),wf(:,75)) * den(70)
  A(54) = cont_VV(wf(:,72),wf(:,76)) * den(71)
  A(55) = cont_SS(wf(:,68),wf(:,71)) * den(73)
  A(56) = cont_SS(wf(:,68),wf(:,71)) * den(73)
  A(57) = cont_VV(wf(:,73),wf(:,77)) * den(75)
  A(58) = cont_SS(wf(:,68),wf(:,75)) * den(76)
  A(59) = cont_SS(wf(:,68),wf(:,75)) * den(76)
  A(60) = cont_VV(wf(:,76),wf(:,77)) * den(77)
  A(61) = cont_SS(wf(:,70),wf(:,71)) * den(79)
  A(62) = cont_SS(wf(:,70),wf(:,71)) * den(79)
  A(63) = cont_VV(wf(:,73),wf(:,78)) * den(81)
  A(64) = cont_SS(wf(:,70),wf(:,75)) * den(82)
  A(65) = cont_SS(wf(:,70),wf(:,75)) * den(82)
  A(66) = cont_VV(wf(:,76),wf(:,78)) * den(83)
  A(67) = cont_SS(wf(:,66),wf(:,79)) * den(85)
  A(68) = cont_SS(wf(:,66),wf(:,79)) * den(85)
  A(69) = cont_VV(wf(:,63),wf(:,80)) * den(87)
  A(70) = cont_SS(wf(:,68),wf(:,79)) * den(88)
  A(71) = cont_SS(wf(:,68),wf(:,79)) * den(88)
  A(72) = cont_VV(wf(:,67),wf(:,80)) * den(89)
  A(73) = cont_SS(wf(:,70),wf(:,79)) * den(90)
  A(74) = cont_SS(wf(:,70),wf(:,79)) * den(90)
  A(75) = cont_VV(wf(:,69),wf(:,80)) * den(91)
  A(76) = cont_SS(wf(:,66),wf(:,81)) * den(92)
  A(77) = cont_SS(wf(:,66),wf(:,81)) * den(92)
  A(78) = cont_VV(wf(:,72),wf(:,82)) * den(93)
  A(79) = cont_VV(wf(:,72),wf(:,84)) * den(96)
  A(80) = cont_SS(wf(:,68),wf(:,81)) * den(97)
  A(81) = cont_SS(wf(:,68),wf(:,81)) * den(97)
  A(82) = cont_VV(wf(:,77),wf(:,82)) * den(98)
  A(83) = cont_VV(wf(:,77),wf(:,84)) * den(99)
  A(84) = cont_SS(wf(:,70),wf(:,81)) * den(100)
  A(85) = cont_SS(wf(:,70),wf(:,81)) * den(100)
  A(86) = cont_VV(wf(:,78),wf(:,82)) * den(101)
  A(87) = cont_VV(wf(:,78),wf(:,84)) * den(102)
  A(88) = cont_SS(wf(:,66),wf(:,85)) * den(103)
  A(89) = cont_SS(wf(:,66),wf(:,85)) * den(103)
  A(90) = cont_VV(wf(:,72),wf(:,86)) * den(104)
  A(91) = cont_VV(wf(:,72),wf(:,88)) * den(106)
  A(92) = cont_SS(wf(:,68),wf(:,85)) * den(107)
  A(93) = cont_SS(wf(:,68),wf(:,85)) * den(107)
  A(94) = cont_VV(wf(:,77),wf(:,86)) * den(108)
  A(95) = cont_VV(wf(:,77),wf(:,88)) * den(109)
  A(96) = cont_SS(wf(:,70),wf(:,85)) * den(110)
  A(97) = cont_SS(wf(:,70),wf(:,85)) * den(110)
  A(98) = cont_VV(wf(:,78),wf(:,86)) * den(111)
  A(99) = cont_VV(wf(:,78),wf(:,88)) * den(112)
  A(100) = cont_VV(wf(:,72),wf(:,89)) * den(113)
  A(101) = cont_VV(wf(:,72),wf(:,91)) * den(115)
  A(102) = cont_VV(wf(:,77),wf(:,89)) * den(116)
  A(103) = cont_VV(wf(:,77),wf(:,91)) * den(117)
  A(104) = cont_VV(wf(:,78),wf(:,89)) * den(118)
  A(105) = cont_VV(wf(:,78),wf(:,91)) * den(119)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(105)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (A(9)+A(11)+A(13)+A(15)+A(36)+A(38))*f(1)+2*CI*(A(1)-A(3)+A(5))*f(2)+2*CI*(A(51)+A(54)-A(57)-A(60)+A(63)+A(66)+A(100) &
       +A(101)-A(102)-A(103)+A(104)+A(105))*f(3)+2*CI*(A(2)-A(4)+A(6))*f(4)-A(22)*f(5)+2*CI*(-A(69)+A(72)-A(75))*f(6)+(A(20)-A(24) &
       -A(26)-A(28)-A(30)-A(32)-A(34))*f(7)+2*CI*(-A(17)+A(18)-A(19))*f(8)+2*CI*(-A(78)-A(79)+A(82)+A(83)-A(86)-A(87)-A(90)-A(91) &
       +A(94)+A(95)-A(98)-A(99))*f(9)+2*CI*(A(68)-A(71)+A(74))*f(38)+2*CI*(-A(42)+A(45)-A(48))*f(39)-A(7)*f(44)+2*CI*(-A(40)+A(43) &
       -A(46))*f(45)+2*CI*(A(77)-A(81)+A(85)+A(89)-A(93)+A(97))*f(50)+2*CI*(-A(50)-A(53)+A(56)+A(59)-A(62)-A(65))*f(51) &
       +2*CI*(A(67)-A(70)+A(73))*f(56)+2*CI*(-A(41)+A(44)-A(47))*f(57)+2*CI*(A(76)-A(80)+A(84)+A(88)-A(92)+A(96))*f(62)+2*CI*( &
       -A(49)-A(52)+A(55)+A(58)-A(61)-A(64))*f(63)
  M2(2) = (A(10)+A(12)+A(14)+A(16)+A(37)+A(39))*f(1)+2*CI*(-A(1)+A(3)-A(5))*f(2)+2*CI*(-A(51)-A(54)+A(57)+A(60)-A(63)-A(66)-A(100) &
       -A(101)+A(102)+A(103)-A(104)-A(105))*f(3)+2*CI*(-A(2)+A(4)-A(6))*f(4)-A(23)*f(5)+2*CI*(A(69)-A(72)+A(75))*f(6)+(A(21)-A(25) &
       -A(27)-A(29)-A(31)-A(33)-A(35))*f(7)+2*CI*(A(17)-A(18)+A(19))*f(8)+2*CI*(A(78)+A(79)-A(82)-A(83)+A(86)+A(87)+A(90)+A(91) &
       -A(94)-A(95)+A(98)+A(99))*f(9)+2*CI*(-A(68)+A(71)-A(74))*f(38)+2*CI*(A(42)-A(45)+A(48))*f(39)-A(8)*f(44)+2*CI*(A(40)-A(43) &
       +A(46))*f(45)+2*CI*(-A(77)+A(81)-A(85)-A(89)+A(93)-A(97))*f(50)+2*CI*(A(50)+A(53)-A(56)-A(59)+A(62)+A(65))*f(51)+2*CI*( &
       -A(67)+A(70)-A(73))*f(56)+2*CI*(A(41)-A(44)+A(47))*f(57)+2*CI*(-A(76)+A(80)-A(84)-A(88)+A(92)-A(96))*f(62)+2*CI*(A(49) &
       +A(52)-A(55)-A(58)+A(61)+A(64))*f(63)

end subroutine colourvectors

end module ol_loop_ppllllj2_nenexeexggg_1_/**/REALKIND
