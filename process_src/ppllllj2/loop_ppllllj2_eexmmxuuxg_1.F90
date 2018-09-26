
module ol_colourmatrix_ppllllj2_eexmmxuuxg_1_/**/REALKIND
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

  KL2(1,:) = [ 4]

  KL2ct(1,:) = [ 4]

  KL2ct2(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj2_eexmmxuuxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_eexmmxuuxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_eexmmxuuxg_1_/**/REALKIND

module ol_loop_ppllllj2_eexmmxuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(63), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:123)
  ! denominators
  complex(REALKIND), save :: den(189)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128), Mct(1,128), Mcol_loop(1,128)
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
    f( 1) = CI*countertermnorm*ctAAGG*eQED**4*gQCD**3
    f( 2) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 3) = (2*countertermnorm*ctZGG*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 4) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 5) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 6) = (eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 7) = (2*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = (4*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(10) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (8*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(12) = eQED**4*gQCD**3*integralnorm*SwF
    f(13) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(14) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(15) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(16) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(17) = (countertermnorm*ctZGG*eQED**4*gQCD**3*lambdaHZZ*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(18) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(19) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(20) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE)/(3._/**/REALKIND*MQ2sum*MW*sw)
    f(21) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(22) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(MW**2*sw**2*12._/**/REALKIND)
    f(23) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(24) = (countertermnorm*ctZGG*eQED**4*gQCD**3*lambdaHZZ*YM)/(cw**2*sw**2*2._/**/REALKIND)
    f(25) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YM)/(cw**2*sw**2*2._/**/REALKIND)
    f(26) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YM)/(cw**2*sw**2)
    f(27) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YM)/(3._/**/REALKIND*MQ2sum*MW*sw)
    f(28) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YM)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(29) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YM)/(MW**2*sw**2*12._/**/REALKIND)
    f(30) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(31) = (countertermnorm*ctZGG*eQED**4*gQCD**3*YE*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(32) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YM)/(MW**2*sw**2*12._/**/REALKIND)
    f(33) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YM)/(MW**2*sw**2*6._/**/REALKIND)
    f(34) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YM)/(MW**2*sw**2*4._/**/REALKIND)
    f(35) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YM)/(MW**2*sw**2*3._/**/REALKIND)
    f(36) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YM)/(MW**2*sw**2*2._/**/REALKIND)
    f(37) = (3*CI*countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHHH*MB*MH**2*YB*YE*YM)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(38) = (3*eQED**4*gQCD**3*integralnorm*lambdaHHH*MH**2*SwF*YB*YE*YM)/(MW**4*sw**4*16._/**/REALKIND)
    f(39) = (eQED**4*gQCD**3*integralnorm*SwF*YB**2*YE*YM)/(MW**4*sw**4*16._/**/REALKIND)
    f(40) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**3*YB2*YE*YM)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)
    f(41) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**3*YC2*YE*YM)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)
    f(42) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE**2*YM)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(43) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE**2*YM)/(MW**4*sw**4*16._/**/REALKIND)
    f(44) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE*YM**2)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(45) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE*YM**2)/(MW**4*sw**4*16._/**/REALKIND)
    f(46) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(47) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(48) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE*YT)/(3._/**/REALKIND*MQ2sum*MW*sw)
    f(49) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(50) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(MW**2*sw**2*6._/**/REALKIND)
    f(51) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)
    f(52) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YM*YT)/(3._/**/REALKIND*MQ2sum*MW*sw)
    f(53) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YM*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(54) = (eQED**4*gQCD**3*integralnorm*SwF*YM*YT)/(MW**2*sw**2*6._/**/REALKIND)
    f(55) = (eQED**4*gQCD**3*integralnorm*SwF*YM*YT)/(MW**2*sw**2*4._/**/REALKIND)
    f(56) = (3*CI*countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHHH*MH**2*MT*YE*YM*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(57) = (3*eQED**4*gQCD**3*integralnorm*lambdaHHH*MH**2*SwF*YE*YM*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(58) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE**2*YM*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(59) = (eQED**4*gQCD**3*integralnorm*SwF*YE**2*YM*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(60) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE*YM**2*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(61) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YM**2*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(62) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YM*YT**2)/(MW**4*sw**4*16._/**/REALKIND)
    f(63) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**3*YE*YM*YT2)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)


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
  complex(REALKIND), intent(out) :: M1(0), M2(1)
  complex(REALKIND) :: A(73)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), POLSEL(7))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), 0)
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), 0)

  end if

  ! internal WFs
  call vert_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,1))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call counter_SSG_G(wf(:,1),wf(:,2),wf(:,-6),wf(:,4))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,5))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,6))
  call counter_VVG_G(wf(:,5),wf(:,6),wf(:,-6),wf(:,7))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,8))
  call prop_W_W(wf(:,8),Q(:,12),MZ,1_intkind1,wf(:,9))
  call counter_VVG_G(wf(:,5),wf(:,9),wf(:,-6),wf(:,10))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,11))
  call prop_W_W(wf(:,11),Q(:,3),MZ,1_intkind1,wf(:,12))
  call counter_VVG_G(wf(:,12),wf(:,6),wf(:,-6),wf(:,13))
  call counter_VVG_G(wf(:,12),wf(:,9),wf(:,-6),wf(:,14))
  call vert_SS_S(wf(:,1),wf(:,2),wf(:,15))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,16))
  call vert_SV_V(wf(:,1),wf(:,9),wf(:,17))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,18))
  call prop_W_W(wf(:,17),Q(:,15),MZ,1_intkind1,wf(:,19))
  call vert_SV_V(wf(:,2),wf(:,12),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,15),MZ,1_intkind1,wf(:,21))
  call vert_VV_S(wf(:,12),wf(:,9),wf(:,22))
  call vert_VQ_A(wf(:,5),wf(:,-4),wf(:,23))
  call counter_SG_G(wf(:,2),wf(:,-6),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,19),ZERO,0_intkind1,wf(:,25))
  call vert_QA_V(wf(:,25),wf(:,-5),wf(:,26))
  call vert_ZQ_A(gZu,wf(:,12),wf(:,-4),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,19),ZERO,0_intkind1,wf(:,28))
  call vert_QA_V(wf(:,28),wf(:,-5),wf(:,29))
  call counter_VG_G(wf(:,9),wf(:,-6),Q(:,64),wf(:,30),Q(:,76))
  call vert_AV_Q(wf(:,-5),wf(:,5),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,35),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,-4),wf(:,32),wf(:,33))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,12),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,35),ZERO,0_intkind1,wf(:,35))
  call vert_QA_V(wf(:,-4),wf(:,35),wf(:,36))
  call vert_VQ_A(wf(:,6),wf(:,-4),wf(:,37))
  call counter_SG_G(wf(:,1),wf(:,-6),wf(:,38))
  call prop_Q_A(wf(:,37),Q(:,28),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,39),wf(:,-5),wf(:,40))
  call vert_ZQ_A(gZu,wf(:,9),wf(:,-4),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,28),ZERO,0_intkind1,wf(:,42))
  call vert_QA_V(wf(:,42),wf(:,-5),wf(:,43))
  call counter_VG_G(wf(:,12),wf(:,-6),Q(:,64),wf(:,44),Q(:,67))
  call vert_AV_Q(wf(:,-5),wf(:,6),wf(:,45))
  call prop_A_Q(wf(:,45),Q(:,44),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,-4),wf(:,46),wf(:,47))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,9),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,44),ZERO,0_intkind1,wf(:,49))
  call vert_QA_V(wf(:,-4),wf(:,49),wf(:,50))
  call vert_QS_A(gH,wf(:,-2),wf(:,1),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,7),ZERO,0_intkind1,wf(:,52))
  call vert_AQ_S(gH,wf(:,-3),wf(:,52),wf(:,53))
  call prop_W_W(wf(:,18),Q(:,112),MZ,1_intkind1,wf(:,54))
  call vert_QA_Z(gZl,wf(:,52),wf(:,-3),wf(:,55))
  call vert_VQ_A(wf(:,5),wf(:,-2),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,7),ZERO,0_intkind1,wf(:,57))
  call vert_AQ_S(gH,wf(:,-3),wf(:,57),wf(:,58))
  call vert_ZQ_A(gZl,wf(:,12),wf(:,-2),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,7),ZERO,0_intkind1,wf(:,60))
  call vert_AQ_S(gH,wf(:,-3),wf(:,60),wf(:,61))
  call vert_QA_Z(gZl,wf(:,57),wf(:,-3),wf(:,62))
  call vert_QA_Z(gZl,wf(:,60),wf(:,-3),wf(:,63))
  call vert_SA_Q(gH,wf(:,1),wf(:,-3),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,11),ZERO,0_intkind1,wf(:,65))
  call vert_AQ_S(gH,wf(:,65),wf(:,-2),wf(:,66))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,65),wf(:,67))
  call vert_AV_Q(wf(:,-3),wf(:,5),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,11),ZERO,0_intkind1,wf(:,69))
  call vert_AQ_S(gH,wf(:,69),wf(:,-2),wf(:,70))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,12),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,11),ZERO,0_intkind1,wf(:,72))
  call vert_AQ_S(gH,wf(:,72),wf(:,-2),wf(:,73))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,69),wf(:,74))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,72),wf(:,75))
  call vert_QS_A(gH,wf(:,0),wf(:,2),wf(:,76))
  call prop_Q_A(wf(:,76),Q(:,13),ZERO,0_intkind1,wf(:,77))
  call vert_AQ_S(gH,wf(:,-1),wf(:,77),wf(:,78))
  call vert_QA_Z(gZl,wf(:,77),wf(:,-1),wf(:,79))
  call vert_VQ_A(wf(:,6),wf(:,0),wf(:,80))
  call prop_Q_A(wf(:,80),Q(:,13),ZERO,0_intkind1,wf(:,81))
  call vert_AQ_S(gH,wf(:,-1),wf(:,81),wf(:,82))
  call vert_ZQ_A(gZl,wf(:,9),wf(:,0),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,13),ZERO,0_intkind1,wf(:,84))
  call vert_AQ_S(gH,wf(:,-1),wf(:,84),wf(:,85))
  call vert_QA_Z(gZl,wf(:,81),wf(:,-1),wf(:,86))
  call vert_QA_Z(gZl,wf(:,84),wf(:,-1),wf(:,87))
  call vert_SA_Q(gH,wf(:,2),wf(:,-1),wf(:,88))
  call prop_A_Q(wf(:,88),Q(:,14),ZERO,0_intkind1,wf(:,89))
  call vert_AQ_S(gH,wf(:,89),wf(:,0),wf(:,90))
  call vert_AV_Q(wf(:,-1),wf(:,6),wf(:,91))
  call prop_A_Q(wf(:,91),Q(:,14),ZERO,0_intkind1,wf(:,92))
  call vert_AQ_S(gH,wf(:,92),wf(:,0),wf(:,93))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,9),wf(:,94))
  call prop_A_Q(wf(:,94),Q(:,14),ZERO,0_intkind1,wf(:,95))
  call vert_AQ_S(gH,wf(:,95),wf(:,0),wf(:,96))
  call vert_QA_Z(gZl,wf(:,0),wf(:,89),wf(:,97))
  call vert_QA_Z(gZl,wf(:,0),wf(:,92),wf(:,98))
  call vert_QA_Z(gZl,wf(:,0),wf(:,95),wf(:,99))
  call vert_QA_V(wf(:,52),wf(:,-3),wf(:,100))
  call prop_W_W(wf(:,55),Q(:,15),MZ,1_intkind1,wf(:,101))
  call vert_QA_V(wf(:,57),wf(:,-3),wf(:,102))
  call prop_W_W(wf(:,62),Q(:,15),MZ,1_intkind1,wf(:,103))
  call vert_QA_V(wf(:,60),wf(:,-3),wf(:,104))
  call prop_W_W(wf(:,63),Q(:,15),MZ,1_intkind1,wf(:,105))
  call vert_QA_V(wf(:,-2),wf(:,65),wf(:,106))
  call prop_W_W(wf(:,67),Q(:,15),MZ,1_intkind1,wf(:,107))
  call vert_QA_V(wf(:,-2),wf(:,69),wf(:,108))
  call prop_W_W(wf(:,74),Q(:,15),MZ,1_intkind1,wf(:,109))
  call vert_QA_V(wf(:,-2),wf(:,72),wf(:,110))
  call prop_W_W(wf(:,75),Q(:,15),MZ,1_intkind1,wf(:,111))
  call vert_QA_V(wf(:,77),wf(:,-1),wf(:,112))
  call prop_W_W(wf(:,79),Q(:,15),MZ,1_intkind1,wf(:,113))
  call vert_QA_V(wf(:,81),wf(:,-1),wf(:,114))
  call prop_W_W(wf(:,86),Q(:,15),MZ,1_intkind1,wf(:,115))
  call vert_QA_V(wf(:,84),wf(:,-1),wf(:,116))
  call prop_W_W(wf(:,87),Q(:,15),MZ,1_intkind1,wf(:,117))
  call vert_QA_V(wf(:,0),wf(:,89),wf(:,118))
  call prop_W_W(wf(:,97),Q(:,15),MZ,1_intkind1,wf(:,119))
  call vert_QA_V(wf(:,0),wf(:,92),wf(:,120))
  call vert_QA_V(wf(:,0),wf(:,95),wf(:,121))
  call prop_W_W(wf(:,98),Q(:,15),MZ,1_intkind1,wf(:,122))
  call prop_W_W(wf(:,99),Q(:,15),MZ,1_intkind1,wf(:,123))

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
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,3))
  den(7) = 1 / (Q(5,12))
  den(10) = 1 / (Q(5,12) - MZ2)
  den(13) = 1 / (Q(5,3) - MZ2)
  den(18) = 1 / (Q(5,15) - MH2)
  den(22) = 1 / (Q(5,15) - MZ2)
  den(30) = 1 / (Q(5,19))
  den(32) = 1 / (Q(5,76))
  den(40) = 1 / (Q(5,35))
  den(47) = 1 / (Q(5,28))
  den(49) = 1 / (Q(5,67))
  den(57) = 1 / (Q(5,44))
  den(64) = 1 / (Q(5,7))
  den(66) = 1 / (Q(5,112) - MH2)
  den(69) = 1 / (Q(5,112) - MZ2)
  den(78) = 1 / (Q(5,11))
  den(88) = 1 / (Q(5,13))
  den(98) = 1 / (Q(5,14))
  den(109) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(3)*den(8)
  den(11) = den(6)*den(10)
  den(12) = den(3)*den(11)
  den(14) = den(7)*den(13)
  den(15) = den(3)*den(14)
  den(16) = den(10)*den(13)
  den(17) = den(3)*den(16)
  den(19) = den(4)*den(18)
  den(20) = den(3)*den(19)
  den(21) = den(1)*den(10)
  den(23) = den(21)*den(22)
  den(24) = den(3)*den(23)
  den(25) = den(2)*den(13)
  den(26) = den(22)*den(25)
  den(27) = den(3)*den(26)
  den(28) = den(16)*den(18)
  den(29) = den(3)*den(28)
  den(31) = den(6)*den(30)
  den(33) = den(2)*den(32)
  den(34) = den(31)*den(33)
  den(35) = den(13)*den(30)
  den(36) = den(33)*den(35)
  den(37) = den(10)*den(32)
  den(38) = den(31)*den(37)
  den(39) = den(35)*den(37)
  den(41) = den(6)*den(40)
  den(42) = den(33)*den(41)
  den(43) = den(13)*den(40)
  den(44) = den(33)*den(43)
  den(45) = den(37)*den(41)
  den(46) = den(37)*den(43)
  den(48) = den(7)*den(47)
  den(50) = den(1)*den(49)
  den(51) = den(48)*den(50)
  den(52) = den(10)*den(47)
  den(53) = den(50)*den(52)
  den(54) = den(13)*den(49)
  den(55) = den(48)*den(54)
  den(56) = den(52)*den(54)
  den(58) = den(7)*den(57)
  den(59) = den(50)*den(58)
  den(60) = den(10)*den(57)
  den(61) = den(50)*den(60)
  den(62) = den(54)*den(58)
  den(63) = den(54)*den(60)
  den(65) = den(1)*den(64)
  den(67) = den(3)*den(66)
  den(68) = den(65)*den(67)
  den(70) = den(3)*den(69)
  den(71) = den(65)*den(70)
  den(72) = den(6)*den(64)
  den(73) = den(67)*den(72)
  den(74) = den(13)*den(64)
  den(75) = den(67)*den(74)
  den(76) = den(70)*den(72)
  den(77) = den(70)*den(74)
  den(79) = den(1)*den(78)
  den(80) = den(67)*den(79)
  den(81) = den(70)*den(79)
  den(82) = den(6)*den(78)
  den(83) = den(67)*den(82)
  den(84) = den(13)*den(78)
  den(85) = den(67)*den(84)
  den(86) = den(70)*den(82)
  den(87) = den(70)*den(84)
  den(89) = den(2)*den(88)
  den(90) = den(67)*den(89)
  den(91) = den(70)*den(89)
  den(92) = den(7)*den(88)
  den(93) = den(67)*den(92)
  den(94) = den(10)*den(88)
  den(95) = den(67)*den(94)
  den(96) = den(70)*den(92)
  den(97) = den(70)*den(94)
  den(99) = den(2)*den(98)
  den(100) = den(67)*den(99)
  den(101) = den(7)*den(98)
  den(102) = den(67)*den(101)
  den(103) = den(10)*den(98)
  den(104) = den(67)*den(103)
  den(105) = den(70)*den(99)
  den(106) = den(70)*den(101)
  den(107) = den(70)*den(103)
  den(108) = den(18)*den(65)
  den(110) = den(65)*den(109)
  den(111) = den(22)*den(65)
  den(112) = den(18)*den(72)
  den(113) = den(18)*den(74)
  den(114) = den(72)*den(109)
  den(115) = den(22)*den(72)
  den(116) = den(74)*den(109)
  den(117) = den(22)*den(74)
  den(118) = den(18)*den(79)
  den(119) = den(79)*den(109)
  den(120) = den(22)*den(79)
  den(121) = den(18)*den(82)
  den(122) = den(18)*den(84)
  den(123) = den(82)*den(109)
  den(124) = den(22)*den(82)
  den(125) = den(84)*den(109)
  den(126) = den(22)*den(84)
  den(127) = den(18)*den(89)
  den(128) = den(89)*den(109)
  den(129) = den(22)*den(89)
  den(130) = den(18)*den(92)
  den(131) = den(18)*den(94)
  den(132) = den(92)*den(109)
  den(133) = den(22)*den(92)
  den(134) = den(94)*den(109)
  den(135) = den(22)*den(94)
  den(136) = den(18)*den(99)
  den(137) = den(18)*den(101)
  den(138) = den(18)*den(103)
  den(139) = den(99)*den(109)
  den(140) = den(22)*den(99)
  den(141) = den(101)*den(109)
  den(142) = den(103)*den(109)
  den(143) = den(22)*den(101)
  den(144) = den(22)*den(103)
  den(145) = den(1)*den(2)*den(3)
  den(146) = den(1)*den(3)*den(7)
  den(147) = den(1)*den(3)*den(10)
  den(148) = den(2)*den(3)*den(6)
  den(149) = den(2)*den(3)*den(13)
  den(150) = den(3)*den(6)*den(7)
  den(151) = den(3)*den(6)*den(10)
  den(152) = den(3)*den(7)*den(13)
  den(153) = den(3)*den(10)*den(13)
  den(154) = den(3)*den(108)
  den(155) = den(3)*den(110)
  den(156) = den(3)*den(111)
  den(157) = den(3)*den(112)
  den(158) = den(3)*den(113)
  den(159) = den(3)*den(114)
  den(160) = den(3)*den(115)
  den(161) = den(3)*den(116)
  den(162) = den(3)*den(117)
  den(163) = den(3)*den(118)
  den(164) = den(3)*den(119)
  den(165) = den(3)*den(120)
  den(166) = den(3)*den(121)
  den(167) = den(3)*den(122)
  den(168) = den(3)*den(123)
  den(169) = den(3)*den(124)
  den(170) = den(3)*den(125)
  den(171) = den(3)*den(126)
  den(172) = den(3)*den(127)
  den(173) = den(3)*den(128)
  den(174) = den(3)*den(129)
  den(175) = den(3)*den(130)
  den(176) = den(3)*den(131)
  den(177) = den(3)*den(132)
  den(178) = den(3)*den(133)
  den(179) = den(3)*den(134)
  den(180) = den(3)*den(135)
  den(181) = den(3)*den(136)
  den(182) = den(3)*den(137)
  den(183) = den(3)*den(138)
  den(184) = den(3)*den(139)
  den(185) = den(3)*den(140)
  den(186) = den(3)*den(141)
  den(187) = den(3)*den(142)
  den(188) = den(3)*den(143)
  den(189) = den(3)*den(144)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(73)


  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(3) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(4) = cont_VV(wf(:,3),wf(:,7)) * den(9)
  A(5) = cont_VV(wf(:,3),wf(:,10)) * den(12)
  A(6) = cont_VV(wf(:,3),wf(:,13)) * den(15)
  A(7) = cont_VV(wf(:,3),wf(:,14)) * den(17)
  A(8) = cont_SS(wf(:,15),wf(:,16)) * den(20)
  A(9) = cont_SS(wf(:,15),wf(:,16)) * den(20)
  A(10) = cont_VV(wf(:,18),wf(:,19)) * den(24)
  A(11) = cont_VV(wf(:,18),wf(:,21)) * den(27)
  A(12) = cont_SS(wf(:,16),wf(:,22)) * den(29)
  A(13) = cont_SS(wf(:,16),wf(:,22)) * den(29)
  A(14) = cont_VV(wf(:,24),wf(:,26)) * den(34)
  A(15) = cont_VV(wf(:,24),wf(:,26)) * den(34)
  A(16) = cont_VV(wf(:,24),wf(:,29)) * den(36)
  A(17) = cont_VV(wf(:,24),wf(:,29)) * den(36)
  A(18) = cont_VV(wf(:,26),wf(:,30)) * den(38)
  A(19) = cont_VV(wf(:,29),wf(:,30)) * den(39)
  A(20) = cont_VV(wf(:,24),wf(:,33)) * den(42)
  A(21) = cont_VV(wf(:,24),wf(:,33)) * den(42)
  A(22) = cont_VV(wf(:,24),wf(:,36)) * den(44)
  A(23) = cont_VV(wf(:,24),wf(:,36)) * den(44)
  A(24) = cont_VV(wf(:,30),wf(:,33)) * den(45)
  A(25) = cont_VV(wf(:,30),wf(:,36)) * den(46)
  A(26) = cont_VV(wf(:,38),wf(:,40)) * den(51)
  A(27) = cont_VV(wf(:,38),wf(:,40)) * den(51)
  A(28) = cont_VV(wf(:,38),wf(:,43)) * den(53)
  A(29) = cont_VV(wf(:,38),wf(:,43)) * den(53)
  A(30) = cont_VV(wf(:,40),wf(:,44)) * den(55)
  A(31) = cont_VV(wf(:,43),wf(:,44)) * den(56)
  A(32) = cont_VV(wf(:,38),wf(:,47)) * den(59)
  A(33) = cont_VV(wf(:,38),wf(:,47)) * den(59)
  A(34) = cont_VV(wf(:,38),wf(:,50)) * den(61)
  A(35) = cont_VV(wf(:,38),wf(:,50)) * den(61)
  A(36) = cont_VV(wf(:,44),wf(:,47)) * den(62)
  A(37) = cont_VV(wf(:,44),wf(:,50)) * den(63)
  A(38) = cont_SS(wf(:,16),wf(:,53)) * den(68)
  A(39) = cont_SS(wf(:,16),wf(:,53)) * den(68)
  A(40) = cont_VV(wf(:,54),wf(:,55)) * den(71)
  A(41) = cont_SS(wf(:,16),wf(:,58)) * den(73)
  A(42) = cont_SS(wf(:,16),wf(:,58)) * den(73)
  A(43) = cont_SS(wf(:,16),wf(:,61)) * den(75)
  A(44) = cont_SS(wf(:,16),wf(:,61)) * den(75)
  A(45) = cont_VV(wf(:,54),wf(:,62)) * den(76)
  A(46) = cont_VV(wf(:,54),wf(:,63)) * den(77)
  A(47) = cont_SS(wf(:,16),wf(:,66)) * den(80)
  A(48) = cont_SS(wf(:,16),wf(:,66)) * den(80)
  A(49) = cont_VV(wf(:,54),wf(:,67)) * den(81)
  A(50) = cont_SS(wf(:,16),wf(:,70)) * den(83)
  A(51) = cont_SS(wf(:,16),wf(:,70)) * den(83)
  A(52) = cont_SS(wf(:,16),wf(:,73)) * den(85)
  A(53) = cont_SS(wf(:,16),wf(:,73)) * den(85)
  A(54) = cont_VV(wf(:,54),wf(:,74)) * den(86)
  A(55) = cont_VV(wf(:,54),wf(:,75)) * den(87)
  A(56) = cont_SS(wf(:,16),wf(:,78)) * den(90)
  A(57) = cont_SS(wf(:,16),wf(:,78)) * den(90)
  A(58) = cont_VV(wf(:,54),wf(:,79)) * den(91)
  A(59) = cont_SS(wf(:,16),wf(:,82)) * den(93)
  A(60) = cont_SS(wf(:,16),wf(:,82)) * den(93)
  A(61) = cont_SS(wf(:,16),wf(:,85)) * den(95)
  A(62) = cont_SS(wf(:,16),wf(:,85)) * den(95)
  A(63) = cont_VV(wf(:,54),wf(:,86)) * den(96)
  A(64) = cont_VV(wf(:,54),wf(:,87)) * den(97)
  A(65) = cont_SS(wf(:,16),wf(:,90)) * den(100)
  A(66) = cont_SS(wf(:,16),wf(:,90)) * den(100)
  A(67) = cont_SS(wf(:,16),wf(:,93)) * den(102)
  A(68) = cont_SS(wf(:,16),wf(:,93)) * den(102)
  A(69) = cont_SS(wf(:,16),wf(:,96)) * den(104)
  A(70) = cont_SS(wf(:,16),wf(:,96)) * den(104)
  A(71) = cont_VV(wf(:,54),wf(:,97)) * den(105)
  A(72) = cont_VV(wf(:,54),wf(:,98)) * den(106)
  A(73) = cont_VV(wf(:,54),wf(:,99)) * den(107)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(73)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = A(4)*f(1)+(A(5)+A(6))*f(2)+(A(18)+A(24)+A(30)+A(36))*f(3)+(-A(19)-A(25)-A(31)-A(37)-A(45)-A(46)-A(54)-A(55)-A(63)-A(64) &
       -A(72)-A(73))*f(4)+A(7)*f(5)-A(13)*f(15)+A(10)*f(17)+(A(27)+A(33))*f(20)+(-A(29)-A(35)-A(60)-A(62)-A(68)-A(70))*f(21) &
       +A(11)*f(24)+(A(15)+A(21))*f(27)+(-A(17)-A(23)-A(42)-A(44)-A(51)-A(53))*f(28)+(A(40)+A(49)+A(58)+A(71))*f(31)+A(9)*f(37) &
       -A(2)*f(40)-A(3)*f(41)+(A(57)+A(66))*f(42)+(A(39)+A(48))*f(44)-A(12)*f(46)+(A(26)+A(32))*f(48)+(-A(28)-A(34)-A(59)-A(61) &
       -A(67)-A(69))*f(49)+(A(14)+A(20))*f(52)+(-A(16)-A(22)-A(41)-A(43)-A(50)-A(52))*f(53)+A(8)*f(56)+(A(56)+A(65))*f(58)+(A(38) &
       +A(47))*f(60)-A(1)*f(63)

end subroutine colourvectors

end module ol_loop_ppllllj2_eexmmxuuxg_1_/**/REALKIND
