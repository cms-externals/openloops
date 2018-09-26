
module ol_colourmatrix_ppllll2_eexmmxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,1), KL(0,1), KL2(1,1), KL2ct(1,1), KL2ct2(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [ 2]

  KL2ct(1,:) = [ 2]

  KL2ct2(1,:) = [ 2]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllll2_eexmmxgg_1_/**/REALKIND



module ol_forced_parameters_ppllll2_eexmmxgg_1_/**/REALKIND
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
  if (MM /= 0) write(*,101) 'MM = 0'
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
end module ol_forced_parameters_ppllll2_eexmmxgg_1_/**/REALKIND

module ol_loop_ppllll2_eexmmxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(58), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:104)
  ! denominators
  complex(REALKIND), save :: den(110)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,64), Mct(1,64), Mcol_loop(1,64)
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
    f( 1) = CI*countertermnorm*ctAAGG*eQED**4*gQCD**2
    f( 2) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**2
    f( 3) = countertermnorm*ctZGG*eQED**4*gQCD**2
    f( 4) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**2
    f( 5) = (eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 6) = (2*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 7) = (eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 8) = (4*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 9) = (2*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (8*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f(11) = eQED**4*gQCD**2*integralnorm*SwF
    f(12) = (4*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(13) = 2*eQED**4*gQCD**2*integralnorm*SwF
    f(14) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(15) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(16) = (countertermnorm*ctZGG*eQED**4*gQCD**2*lambdaHZZ*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(17) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(18) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(19) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YE)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(20) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE)/(MW**2*sw**2*12._/**/REALKIND)
    f(21) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(22) = (countertermnorm*ctZGG*eQED**4*gQCD**2*lambdaHZZ*YM)/(cw**2*sw**2*2._/**/REALKIND)
    f(23) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YM)/(cw**2*sw**2*2._/**/REALKIND)
    f(24) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YM)/(cw**2*sw**2)
    f(25) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YM)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(26) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YM)/(MW**2*sw**2*12._/**/REALKIND)
    f(27) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(28) = (countertermnorm*ctZGG*eQED**4*gQCD**2*YE*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(29) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YM)/(MW**2*sw**2*12._/**/REALKIND)
    f(30) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YM)/(MW**2*sw**2*6._/**/REALKIND)
    f(31) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(32) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YM)/(MW**2*sw**2*3._/**/REALKIND)
    f(33) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YM)/(MW**2*sw**2*2._/**/REALKIND)
    f(34) = (3*CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHHH*MB*MH**2*YB*YE*YM)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(35) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YB*YE*YM)/(MW**4*sw**4*16._/**/REALKIND)
    f(36) = (eQED**4*gQCD**2*integralnorm*SwF*YB**2*YE*YM)/(MW**4*sw**4*16._/**/REALKIND)
    f(37) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**2*YB2*YE*YM)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)
    f(38) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**2*YC2*YE*YM)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)
    f(39) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YE**2*YM)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(40) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE**2*YM)/(MW**4*sw**4*16._/**/REALKIND)
    f(41) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YE*YM**2)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(42) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE*YM**2)/(MW**4*sw**4*16._/**/REALKIND)
    f(43) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(44) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(45) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(46) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YT)/(MW**2*sw**2*6._/**/REALKIND)
    f(47) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)
    f(48) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YM*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(49) = (eQED**4*gQCD**2*integralnorm*SwF*YM*YT)/(MW**2*sw**2*6._/**/REALKIND)
    f(50) = (eQED**4*gQCD**2*integralnorm*SwF*YM*YT)/(MW**2*sw**2*4._/**/REALKIND)
    f(51) = (3*CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHHH*MH**2*MT*YE*YM*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(52) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YE*YM*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(53) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE**2*YM*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(54) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2*YM*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(55) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE*YM**2*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(56) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YM**2*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(57) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YM*YT**2)/(MW**4*sw**4*16._/**/REALKIND)
    f(58) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**2*YE*YM*YT2)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)


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
  complex(REALKIND), intent(out) :: M1(0), M2(1)
  complex(REALKIND) :: A(49)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,1))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,2))
  call counter_GGS_S(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,4))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,5))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,4),wf(:,6))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,12),MZ,1_intkind1,wf(:,8))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,3),MZ,1_intkind1,wf(:,10))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,10),wf(:,11))
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,12))
  call vert_SS_S(wf(:,1),wf(:,2),wf(:,13))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,14))
  call prop_W_W(wf(:,14),Q(:,48),MZ,1_intkind1,wf(:,15))
  call vert_SV_V(wf(:,1),wf(:,8),wf(:,16))
  call vert_SV_V(wf(:,2),wf(:,10),wf(:,17))
  call vert_VV_S(wf(:,10),wf(:,8),wf(:,18))
  call vert_QS_A(gH,wf(:,-2),wf(:,1),wf(:,19))
  call vert_SA_Q(gH,wf(:,12),wf(:,-3),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,7),ZERO,0_intkind1,wf(:,21))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,15),wf(:,22))
  call vert_VQ_A(wf(:,4),wf(:,-2),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,7),ZERO,0_intkind1,wf(:,24))
  call vert_ZQ_A(gZl,wf(:,10),wf(:,-2),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,7),ZERO,0_intkind1,wf(:,26))
  call vert_QS_A(gH,wf(:,-2),wf(:,12),wf(:,27))
  call vert_SA_Q(gH,wf(:,1),wf(:,-3),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,52),ZERO,0_intkind1,wf(:,29))
  call vert_ZQ_A(gZl,wf(:,15),wf(:,-2),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,52),ZERO,0_intkind1,wf(:,31))
  call vert_AV_Q(wf(:,-3),wf(:,4),wf(:,32))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,10),wf(:,33))
  call vert_QS_A(gH,wf(:,0),wf(:,2),wf(:,34))
  call vert_SA_Q(gH,wf(:,12),wf(:,-1),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,13),ZERO,0_intkind1,wf(:,36))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,15),wf(:,37))
  call vert_VQ_A(wf(:,5),wf(:,0),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,13),ZERO,0_intkind1,wf(:,39))
  call vert_ZQ_A(gZl,wf(:,8),wf(:,0),wf(:,40))
  call prop_Q_A(wf(:,40),Q(:,13),ZERO,0_intkind1,wf(:,41))
  call vert_QS_A(gH,wf(:,0),wf(:,12),wf(:,42))
  call vert_SA_Q(gH,wf(:,2),wf(:,-1),wf(:,43))
  call prop_Q_A(wf(:,42),Q(:,49),ZERO,0_intkind1,wf(:,44))
  call vert_AV_Q(wf(:,-1),wf(:,5),wf(:,45))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,8),wf(:,46))
  call vert_ZQ_A(gZl,wf(:,15),wf(:,0),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,49),ZERO,0_intkind1,wf(:,48))
  call prop_W_W(wf(:,16),Q(:,15),MZ,1_intkind1,wf(:,49))
  call prop_W_W(wf(:,17),Q(:,15),MZ,1_intkind1,wf(:,50))
  call vert_AQ_S(gH,wf(:,-3),wf(:,21),wf(:,51))
  call vert_QA_V(wf(:,21),wf(:,-3),wf(:,52))
  call vert_QA_Z(gZl,wf(:,21),wf(:,-3),wf(:,53))
  call prop_W_W(wf(:,53),Q(:,15),MZ,1_intkind1,wf(:,54))
  call vert_AQ_S(gH,wf(:,-3),wf(:,24),wf(:,55))
  call vert_AQ_S(gH,wf(:,-3),wf(:,26),wf(:,56))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,57))
  call vert_QA_Z(gZl,wf(:,24),wf(:,-3),wf(:,58))
  call prop_W_W(wf(:,58),Q(:,15),MZ,1_intkind1,wf(:,59))
  call vert_QA_V(wf(:,26),wf(:,-3),wf(:,60))
  call vert_QA_Z(gZl,wf(:,26),wf(:,-3),wf(:,61))
  call prop_W_W(wf(:,61),Q(:,15),MZ,1_intkind1,wf(:,62))
  call prop_A_Q(wf(:,28),Q(:,11),ZERO,0_intkind1,wf(:,63))
  call vert_AQ_S(gH,wf(:,63),wf(:,-2),wf(:,64))
  call vert_QA_V(wf(:,-2),wf(:,63),wf(:,65))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,63),wf(:,66))
  call prop_W_W(wf(:,66),Q(:,15),MZ,1_intkind1,wf(:,67))
  call prop_A_Q(wf(:,32),Q(:,11),ZERO,0_intkind1,wf(:,68))
  call vert_AQ_S(gH,wf(:,68),wf(:,-2),wf(:,69))
  call prop_A_Q(wf(:,33),Q(:,11),ZERO,0_intkind1,wf(:,70))
  call vert_AQ_S(gH,wf(:,70),wf(:,-2),wf(:,71))
  call vert_QA_V(wf(:,-2),wf(:,68),wf(:,72))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,68),wf(:,73))
  call prop_W_W(wf(:,73),Q(:,15),MZ,1_intkind1,wf(:,74))
  call vert_QA_V(wf(:,-2),wf(:,70),wf(:,75))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,70),wf(:,76))
  call prop_W_W(wf(:,76),Q(:,15),MZ,1_intkind1,wf(:,77))
  call vert_AQ_S(gH,wf(:,-1),wf(:,36),wf(:,78))
  call vert_QA_V(wf(:,36),wf(:,-1),wf(:,79))
  call vert_QA_Z(gZl,wf(:,36),wf(:,-1),wf(:,80))
  call prop_W_W(wf(:,80),Q(:,15),MZ,1_intkind1,wf(:,81))
  call vert_AQ_S(gH,wf(:,-1),wf(:,39),wf(:,82))
  call vert_AQ_S(gH,wf(:,-1),wf(:,41),wf(:,83))
  call vert_QA_V(wf(:,39),wf(:,-1),wf(:,84))
  call vert_QA_Z(gZl,wf(:,39),wf(:,-1),wf(:,85))
  call prop_W_W(wf(:,85),Q(:,15),MZ,1_intkind1,wf(:,86))
  call vert_QA_V(wf(:,41),wf(:,-1),wf(:,87))
  call vert_QA_Z(gZl,wf(:,41),wf(:,-1),wf(:,88))
  call prop_W_W(wf(:,88),Q(:,15),MZ,1_intkind1,wf(:,89))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,90))
  call vert_AQ_S(gH,wf(:,90),wf(:,0),wf(:,91))
  call prop_A_Q(wf(:,45),Q(:,14),ZERO,0_intkind1,wf(:,92))
  call vert_AQ_S(gH,wf(:,92),wf(:,0),wf(:,93))
  call prop_A_Q(wf(:,46),Q(:,14),ZERO,0_intkind1,wf(:,94))
  call vert_AQ_S(gH,wf(:,94),wf(:,0),wf(:,95))
  call vert_QA_V(wf(:,0),wf(:,90),wf(:,96))
  call vert_QA_Z(gZl,wf(:,0),wf(:,90),wf(:,97))
  call prop_W_W(wf(:,97),Q(:,15),MZ,1_intkind1,wf(:,98))
  call vert_QA_V(wf(:,0),wf(:,92),wf(:,99))
  call vert_QA_V(wf(:,0),wf(:,94),wf(:,100))
  call vert_QA_Z(gZl,wf(:,0),wf(:,92),wf(:,101))
  call prop_W_W(wf(:,101),Q(:,15),MZ,1_intkind1,wf(:,102))
  call vert_QA_Z(gZl,wf(:,0),wf(:,94),wf(:,103))
  call prop_W_W(wf(:,103),Q(:,15),MZ,1_intkind1,wf(:,104))

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
  den(2) = 1 / (Q(5,12) - MH2)
  den(4) = 1 / (Q(5,3))
  den(5) = 1 / (Q(5,12))
  den(7) = 1 / (Q(5,12) - MZ2)
  den(9) = 1 / (Q(5,3) - MZ2)
  den(12) = 1 / (Q(5,48) - MH2)
  den(14) = 1 / (Q(5,48) - MZ2)
  den(20) = 1 / (Q(5,7))
  den(30) = 1 / (Q(5,52))
  den(39) = 1 / (Q(5,13))
  den(49) = 1 / (Q(5,49))
  den(58) = 1 / (Q(5,15) - MH2)
  den(60) = 1 / (Q(5,15) - MZ2)
  den(65) = 1 / (Q(5,15))
  den(74) = 1 / (Q(5,11))
  den(96) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(8) = den(4)*den(7)
  den(10) = den(5)*den(9)
  den(11) = den(7)*den(9)
  den(13) = den(3)*den(12)
  den(15) = den(1)*den(7)
  den(16) = den(14)*den(15)
  den(17) = den(2)*den(9)
  den(18) = den(14)*den(17)
  den(19) = den(11)*den(12)
  den(21) = den(1)*den(20)
  den(22) = den(12)*den(21)
  den(23) = den(14)*den(21)
  den(24) = den(4)*den(20)
  den(25) = den(12)*den(24)
  den(26) = den(9)*den(20)
  den(27) = den(12)*den(26)
  den(28) = den(14)*den(24)
  den(29) = den(14)*den(26)
  den(31) = den(12)*den(30)
  den(32) = den(1)*den(31)
  den(33) = den(14)*den(30)
  den(34) = den(1)*den(33)
  den(35) = den(4)*den(31)
  den(36) = den(9)*den(31)
  den(37) = den(4)*den(33)
  den(38) = den(9)*den(33)
  den(40) = den(2)*den(39)
  den(41) = den(12)*den(40)
  den(42) = den(14)*den(40)
  den(43) = den(5)*den(39)
  den(44) = den(12)*den(43)
  den(45) = den(7)*den(39)
  den(46) = den(12)*den(45)
  den(47) = den(14)*den(43)
  den(48) = den(14)*den(45)
  den(50) = den(12)*den(49)
  den(51) = den(2)*den(50)
  den(52) = den(5)*den(50)
  den(53) = den(7)*den(50)
  den(54) = den(14)*den(49)
  den(55) = den(2)*den(54)
  den(56) = den(5)*den(54)
  den(57) = den(7)*den(54)
  den(59) = den(3)*den(58)
  den(61) = den(15)*den(60)
  den(62) = den(17)*den(60)
  den(63) = den(11)*den(58)
  den(64) = den(21)*den(58)
  den(66) = den(21)*den(65)
  den(67) = den(21)*den(60)
  den(68) = den(24)*den(58)
  den(69) = den(26)*den(58)
  den(70) = den(24)*den(65)
  den(71) = den(24)*den(60)
  den(72) = den(26)*den(65)
  den(73) = den(26)*den(60)
  den(75) = den(1)*den(74)
  den(76) = den(58)*den(75)
  den(77) = den(65)*den(75)
  den(78) = den(60)*den(75)
  den(79) = den(4)*den(74)
  den(80) = den(58)*den(79)
  den(81) = den(9)*den(74)
  den(82) = den(58)*den(81)
  den(83) = den(65)*den(79)
  den(84) = den(60)*den(79)
  den(85) = den(65)*den(81)
  den(86) = den(60)*den(81)
  den(87) = den(40)*den(58)
  den(88) = den(40)*den(65)
  den(89) = den(40)*den(60)
  den(90) = den(43)*den(58)
  den(91) = den(45)*den(58)
  den(92) = den(43)*den(65)
  den(93) = den(43)*den(60)
  den(94) = den(45)*den(65)
  den(95) = den(45)*den(60)
  den(97) = den(2)*den(96)
  den(98) = den(58)*den(97)
  den(99) = den(5)*den(96)
  den(100) = den(58)*den(99)
  den(101) = den(7)*den(96)
  den(102) = den(58)*den(101)
  den(103) = den(65)*den(97)
  den(104) = den(60)*den(97)
  den(105) = den(65)*den(99)
  den(106) = den(65)*den(101)
  den(107) = den(60)*den(99)
  den(108) = den(60)*den(101)
  den(109) = den(1)*den(5)
  den(110) = den(2)*den(4)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(49)


  A(1) = cont_SS(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_SS(wf(:,2),wf(:,3)) * den(3)
  A(3) = cont_SS(wf(:,2),wf(:,3)) * den(3)
  A(4) = cont_VV(wf(:,5),wf(:,6)) * den(6)
  A(5) = cont_VV(wf(:,6),wf(:,8)) * den(8)
  A(6) = cont_VV(wf(:,5),wf(:,11)) * den(10)
  A(7) = cont_VV(wf(:,8),wf(:,11)) * den(11)
  A(8) = cont_SS(wf(:,12),wf(:,13)) * den(13)
  A(9) = cont_SS(wf(:,12),wf(:,13)) * den(13)
  A(10) = cont_VV(wf(:,15),wf(:,16)) * den(16)
  A(11) = cont_VV(wf(:,15),wf(:,17)) * den(18)
  A(12) = cont_SS(wf(:,12),wf(:,18)) * den(19)
  A(13) = cont_SS(wf(:,12),wf(:,18)) * den(19)
  A(14) = cont_QA(wf(:,20),wf(:,21)) * den(22)
  A(15) = cont_QA(wf(:,20),wf(:,21)) * den(22)
  A(16) = cont_QA(wf(:,21),wf(:,22)) * den(23)
  A(17) = cont_QA(wf(:,20),wf(:,24)) * den(25)
  A(18) = cont_QA(wf(:,20),wf(:,24)) * den(25)
  A(19) = cont_QA(wf(:,20),wf(:,26)) * den(27)
  A(20) = cont_QA(wf(:,20),wf(:,26)) * den(27)
  A(21) = cont_QA(wf(:,22),wf(:,24)) * den(28)
  A(22) = cont_QA(wf(:,22),wf(:,26)) * den(29)
  A(23) = cont_QA(wf(:,28),wf(:,29)) * den(32)
  A(24) = cont_QA(wf(:,28),wf(:,29)) * den(32)
  A(25) = cont_QA(wf(:,28),wf(:,31)) * den(34)
  A(26) = cont_QA(wf(:,29),wf(:,32)) * den(35)
  A(27) = cont_QA(wf(:,29),wf(:,32)) * den(35)
  A(28) = cont_QA(wf(:,29),wf(:,33)) * den(36)
  A(29) = cont_QA(wf(:,29),wf(:,33)) * den(36)
  A(30) = cont_QA(wf(:,31),wf(:,32)) * den(37)
  A(31) = cont_QA(wf(:,31),wf(:,33)) * den(38)
  A(32) = cont_QA(wf(:,35),wf(:,36)) * den(41)
  A(33) = cont_QA(wf(:,35),wf(:,36)) * den(41)
  A(34) = cont_QA(wf(:,36),wf(:,37)) * den(42)
  A(35) = cont_QA(wf(:,35),wf(:,39)) * den(44)
  A(36) = cont_QA(wf(:,35),wf(:,39)) * den(44)
  A(37) = cont_QA(wf(:,35),wf(:,41)) * den(46)
  A(38) = cont_QA(wf(:,35),wf(:,41)) * den(46)
  A(39) = cont_QA(wf(:,37),wf(:,39)) * den(47)
  A(40) = cont_QA(wf(:,37),wf(:,41)) * den(48)
  A(41) = cont_QA(wf(:,43),wf(:,44)) * den(51)
  A(42) = cont_QA(wf(:,43),wf(:,44)) * den(51)
  A(43) = cont_QA(wf(:,44),wf(:,45)) * den(52)
  A(44) = cont_QA(wf(:,44),wf(:,45)) * den(52)
  A(45) = cont_QA(wf(:,44),wf(:,46)) * den(53)
  A(46) = cont_QA(wf(:,44),wf(:,46)) * den(53)
  A(47) = cont_QA(wf(:,43),wf(:,48)) * den(55)
  A(48) = cont_QA(wf(:,45),wf(:,48)) * den(56)
  A(49) = cont_QA(wf(:,46),wf(:,48)) * den(57)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(49)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*A(4)*f(1)+2*(A(5)+A(6))*f(2)+2*(-A(21)-A(22)-A(30)-A(31)-A(39)-A(40)-A(48)-A(49))*f(3)+2*A(7)*f(4)-2*A(13)*f(14) &
       +2*A(10)*f(16)+2*(-A(36)-A(38)-A(44)-A(46))*f(19)+2*A(11)*f(22)+2*(-A(18)-A(20)-A(27)-A(29))*f(25)+2*(A(16)+A(25)+A(34) &
       +A(47))*f(28)+2*A(9)*f(34)-2*A(2)*f(37)-2*A(3)*f(38)+2*(A(33)+A(42))*f(39)+2*(A(15)+A(24))*f(41)-2*A(12)*f(43)+2*(-A(35) &
       -A(37)-A(43)-A(45))*f(45)+2*(-A(17)-A(19)-A(26)-A(28))*f(48)+2*A(8)*f(51)+2*(A(32)+A(41))*f(53)+2*(A(14)+A(23))*f(55) &
       -2*A(1)*f(58)

end subroutine colourvectors

end module ol_loop_ppllll2_eexmmxgg_1_/**/REALKIND
