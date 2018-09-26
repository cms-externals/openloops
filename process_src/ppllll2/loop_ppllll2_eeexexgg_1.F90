
module ol_colourmatrix_ppllll2_eeexexgg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll2_eeexexgg_1_/**/REALKIND



module ol_forced_parameters_ppllll2_eeexexgg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll2_eeexexgg_1_/**/REALKIND

module ol_loop_ppllll2_eeexexgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(45), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:193)
  ! denominators
  complex(REALKIND), save :: den(205)
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
    f(22) = (countertermnorm*ctZGG*eQED**4*gQCD**2*YE**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(23) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*12._/**/REALKIND)
    f(24) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*6._/**/REALKIND)
    f(25) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*4._/**/REALKIND)
    f(26) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*3._/**/REALKIND)
    f(27) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2)/(MW**2*sw**2*2._/**/REALKIND)
    f(28) = (3*CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHHH*MB*MH**2*YB*YE**2)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(29) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YB*YE**2)/(MW**4*sw**4*16._/**/REALKIND)
    f(30) = (eQED**4*gQCD**2*integralnorm*SwF*YB**2*YE**2)/(MW**4*sw**4*16._/**/REALKIND)
    f(31) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**2*YB2*YE**2)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)
    f(32) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**2*YC2*YE**2)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)
    f(33) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YE**3)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(34) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE**3)/(MW**4*sw**4*16._/**/REALKIND)
    f(35) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(36) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(37) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(38) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YT)/(MW**2*sw**2*6._/**/REALKIND)
    f(39) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)
    f(40) = (3*CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHHH*MH**2*MT*YE**2*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(41) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*MH**2*SwF*YE**2*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(42) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE**3*YT)/(8._/**/REALKIND*MQ2sum*MW**3*sw**3)
    f(43) = (eQED**4*gQCD**2*integralnorm*SwF*YE**3*YT)/(MW**4*sw**4*16._/**/REALKIND)
    f(44) = (eQED**4*gQCD**2*integralnorm*SwF*YE**2*YT**2)/(MW**4*sw**4*16._/**/REALKIND)
    f(45) = (CI*countertermnorm*ctHHGG*eQED**4*gQCD**2*YE**2*YT2)/(4._/**/REALKIND*MQ2sum*MW**2*sw**2)


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
  complex(REALKIND) :: A(98)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_AQ_S(gH,wf(:,-2),wf(:,0),wf(:,1))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-1),wf(:,2))
  call counter_GGS_S(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,4))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,5))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,4),wf(:,6))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-3),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,10),MZ,1_intkind1,wf(:,8))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-2),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,5),MZ,1_intkind1,wf(:,10))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,10),wf(:,11))
  call vert_AQ_S(gH,wf(:,-3),wf(:,0),wf(:,12))
  call vert_AQ_S(gH,wf(:,-2),wf(:,-1),wf(:,13))
  call counter_GGS_S(wf(:,-4),wf(:,-5),wf(:,12),wf(:,14))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,15))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,16))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,15),wf(:,17))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,18))
  call prop_W_W(wf(:,18),Q(:,6),MZ,1_intkind1,wf(:,19))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-3),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,9),MZ,1_intkind1,wf(:,21))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,21),wf(:,22))
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,23))
  call vert_SS_S(wf(:,1),wf(:,2),wf(:,24))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,25))
  call prop_W_W(wf(:,25),Q(:,48),MZ,1_intkind1,wf(:,26))
  call vert_SV_V(wf(:,1),wf(:,8),wf(:,27))
  call vert_SV_V(wf(:,2),wf(:,10),wf(:,28))
  call vert_VV_S(wf(:,10),wf(:,8),wf(:,29))
  call vert_QS_A(gH,wf(:,-1),wf(:,1),wf(:,30))
  call vert_SA_Q(gH,wf(:,23),wf(:,-3),wf(:,31))
  call prop_Q_A(wf(:,30),Q(:,7),ZERO,0_intkind1,wf(:,32))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,26),wf(:,33))
  call vert_VQ_A(wf(:,4),wf(:,-1),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,7),ZERO,0_intkind1,wf(:,35))
  call vert_ZQ_A(gZl,wf(:,10),wf(:,-1),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,7),ZERO,0_intkind1,wf(:,37))
  call vert_QS_A(gH,wf(:,-1),wf(:,23),wf(:,38))
  call vert_SA_Q(gH,wf(:,1),wf(:,-3),wf(:,39))
  call prop_Q_A(wf(:,38),Q(:,50),ZERO,0_intkind1,wf(:,40))
  call vert_ZQ_A(gZl,wf(:,26),wf(:,-1),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,50),ZERO,0_intkind1,wf(:,42))
  call vert_AV_Q(wf(:,-3),wf(:,4),wf(:,43))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,10),wf(:,44))
  call vert_SS_S(wf(:,13),wf(:,12),wf(:,45))
  call vert_SV_V(wf(:,12),wf(:,19),wf(:,46))
  call vert_SV_V(wf(:,13),wf(:,21),wf(:,47))
  call vert_VV_S(wf(:,19),wf(:,21),wf(:,48))
  call vert_QS_A(gH,wf(:,0),wf(:,13),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,7),ZERO,0_intkind1,wf(:,50))
  call vert_VQ_A(wf(:,16),wf(:,0),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,7),ZERO,0_intkind1,wf(:,52))
  call vert_ZQ_A(gZl,wf(:,19),wf(:,0),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,7),ZERO,0_intkind1,wf(:,54))
  call vert_QS_A(gH,wf(:,0),wf(:,23),wf(:,55))
  call vert_SA_Q(gH,wf(:,13),wf(:,-3),wf(:,56))
  call prop_Q_A(wf(:,55),Q(:,49),ZERO,0_intkind1,wf(:,57))
  call vert_AV_Q(wf(:,-3),wf(:,16),wf(:,58))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,19),wf(:,59))
  call vert_ZQ_A(gZl,wf(:,26),wf(:,0),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,49),ZERO,0_intkind1,wf(:,61))
  call vert_QS_A(gH,wf(:,-1),wf(:,12),wf(:,62))
  call vert_SA_Q(gH,wf(:,23),wf(:,-2),wf(:,63))
  call prop_Q_A(wf(:,62),Q(:,11),ZERO,0_intkind1,wf(:,64))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,26),wf(:,65))
  call vert_VQ_A(wf(:,15),wf(:,-1),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,11),ZERO,0_intkind1,wf(:,67))
  call vert_ZQ_A(gZl,wf(:,21),wf(:,-1),wf(:,68))
  call prop_Q_A(wf(:,68),Q(:,11),ZERO,0_intkind1,wf(:,69))
  call vert_SA_Q(gH,wf(:,12),wf(:,-2),wf(:,70))
  call vert_AV_Q(wf(:,-2),wf(:,15),wf(:,71))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,21),wf(:,72))
  call vert_QS_A(gH,wf(:,0),wf(:,2),wf(:,73))
  call prop_Q_A(wf(:,73),Q(:,11),ZERO,0_intkind1,wf(:,74))
  call vert_VQ_A(wf(:,5),wf(:,0),wf(:,75))
  call prop_Q_A(wf(:,75),Q(:,11),ZERO,0_intkind1,wf(:,76))
  call vert_ZQ_A(gZl,wf(:,8),wf(:,0),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,11),ZERO,0_intkind1,wf(:,78))
  call vert_SA_Q(gH,wf(:,2),wf(:,-2),wf(:,79))
  call vert_AV_Q(wf(:,-2),wf(:,5),wf(:,80))
  call vert_AZ_Q(gZl,wf(:,-2),wf(:,8),wf(:,81))
  call prop_W_W(wf(:,27),Q(:,15),MZ,1_intkind1,wf(:,82))
  call prop_W_W(wf(:,28),Q(:,15),MZ,1_intkind1,wf(:,83))
  call vert_AQ_S(gH,wf(:,-3),wf(:,32),wf(:,84))
  call vert_QA_V(wf(:,32),wf(:,-3),wf(:,85))
  call vert_QA_Z(gZl,wf(:,32),wf(:,-3),wf(:,86))
  call prop_W_W(wf(:,86),Q(:,15),MZ,1_intkind1,wf(:,87))
  call vert_AQ_S(gH,wf(:,-3),wf(:,35),wf(:,88))
  call vert_AQ_S(gH,wf(:,-3),wf(:,37),wf(:,89))
  call vert_QA_V(wf(:,35),wf(:,-3),wf(:,90))
  call vert_QA_Z(gZl,wf(:,35),wf(:,-3),wf(:,91))
  call prop_W_W(wf(:,91),Q(:,15),MZ,1_intkind1,wf(:,92))
  call vert_QA_V(wf(:,37),wf(:,-3),wf(:,93))
  call vert_QA_Z(gZl,wf(:,37),wf(:,-3),wf(:,94))
  call prop_W_W(wf(:,94),Q(:,15),MZ,1_intkind1,wf(:,95))
  call prop_A_Q(wf(:,39),Q(:,13),ZERO,0_intkind1,wf(:,96))
  call vert_AQ_S(gH,wf(:,96),wf(:,-1),wf(:,97))
  call vert_QA_V(wf(:,-1),wf(:,96),wf(:,98))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,96),wf(:,99))
  call prop_W_W(wf(:,99),Q(:,15),MZ,1_intkind1,wf(:,100))
  call prop_A_Q(wf(:,43),Q(:,13),ZERO,0_intkind1,wf(:,101))
  call vert_AQ_S(gH,wf(:,101),wf(:,-1),wf(:,102))
  call prop_A_Q(wf(:,44),Q(:,13),ZERO,0_intkind1,wf(:,103))
  call vert_AQ_S(gH,wf(:,103),wf(:,-1),wf(:,104))
  call vert_QA_V(wf(:,-1),wf(:,101),wf(:,105))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,101),wf(:,106))
  call prop_W_W(wf(:,106),Q(:,15),MZ,1_intkind1,wf(:,107))
  call vert_QA_V(wf(:,-1),wf(:,103),wf(:,108))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,103),wf(:,109))
  call prop_W_W(wf(:,109),Q(:,15),MZ,1_intkind1,wf(:,110))
  call prop_W_W(wf(:,46),Q(:,15),MZ,1_intkind1,wf(:,111))
  call prop_W_W(wf(:,47),Q(:,15),MZ,1_intkind1,wf(:,112))
  call vert_AQ_S(gH,wf(:,-3),wf(:,50),wf(:,113))
  call vert_QA_V(wf(:,50),wf(:,-3),wf(:,114))
  call vert_QA_Z(gZl,wf(:,50),wf(:,-3),wf(:,115))
  call prop_W_W(wf(:,115),Q(:,15),MZ,1_intkind1,wf(:,116))
  call vert_AQ_S(gH,wf(:,-3),wf(:,52),wf(:,117))
  call vert_AQ_S(gH,wf(:,-3),wf(:,54),wf(:,118))
  call vert_QA_V(wf(:,52),wf(:,-3),wf(:,119))
  call vert_QA_Z(gZl,wf(:,52),wf(:,-3),wf(:,120))
  call prop_W_W(wf(:,120),Q(:,15),MZ,1_intkind1,wf(:,121))
  call vert_QA_V(wf(:,54),wf(:,-3),wf(:,122))
  call vert_QA_Z(gZl,wf(:,54),wf(:,-3),wf(:,123))
  call prop_W_W(wf(:,123),Q(:,15),MZ,1_intkind1,wf(:,124))
  call prop_A_Q(wf(:,56),Q(:,14),ZERO,0_intkind1,wf(:,125))
  call vert_AQ_S(gH,wf(:,125),wf(:,0),wf(:,126))
  call prop_A_Q(wf(:,58),Q(:,14),ZERO,0_intkind1,wf(:,127))
  call vert_AQ_S(gH,wf(:,127),wf(:,0),wf(:,128))
  call prop_A_Q(wf(:,59),Q(:,14),ZERO,0_intkind1,wf(:,129))
  call vert_AQ_S(gH,wf(:,129),wf(:,0),wf(:,130))
  call vert_QA_V(wf(:,0),wf(:,125),wf(:,131))
  call vert_QA_Z(gZl,wf(:,0),wf(:,125),wf(:,132))
  call prop_W_W(wf(:,132),Q(:,15),MZ,1_intkind1,wf(:,133))
  call vert_QA_V(wf(:,0),wf(:,127),wf(:,134))
  call vert_QA_V(wf(:,0),wf(:,129),wf(:,135))
  call vert_QA_Z(gZl,wf(:,0),wf(:,127),wf(:,136))
  call prop_W_W(wf(:,136),Q(:,15),MZ,1_intkind1,wf(:,137))
  call vert_QA_Z(gZl,wf(:,0),wf(:,129),wf(:,138))
  call prop_W_W(wf(:,138),Q(:,15),MZ,1_intkind1,wf(:,139))
  call vert_AQ_S(gH,wf(:,-2),wf(:,64),wf(:,140))
  call vert_QA_V(wf(:,64),wf(:,-2),wf(:,141))
  call vert_QA_Z(gZl,wf(:,64),wf(:,-2),wf(:,142))
  call prop_W_W(wf(:,142),Q(:,15),MZ,1_intkind1,wf(:,143))
  call vert_AQ_S(gH,wf(:,-2),wf(:,67),wf(:,144))
  call vert_AQ_S(gH,wf(:,-2),wf(:,69),wf(:,145))
  call vert_QA_V(wf(:,67),wf(:,-2),wf(:,146))
  call vert_QA_Z(gZl,wf(:,67),wf(:,-2),wf(:,147))
  call prop_W_W(wf(:,147),Q(:,15),MZ,1_intkind1,wf(:,148))
  call vert_QA_V(wf(:,69),wf(:,-2),wf(:,149))
  call vert_QA_Z(gZl,wf(:,69),wf(:,-2),wf(:,150))
  call prop_W_W(wf(:,150),Q(:,15),MZ,1_intkind1,wf(:,151))
  call prop_A_Q(wf(:,70),Q(:,13),ZERO,0_intkind1,wf(:,152))
  call vert_AQ_S(gH,wf(:,152),wf(:,-1),wf(:,153))
  call vert_QA_V(wf(:,-1),wf(:,152),wf(:,154))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,152),wf(:,155))
  call prop_W_W(wf(:,155),Q(:,15),MZ,1_intkind1,wf(:,156))
  call prop_A_Q(wf(:,71),Q(:,13),ZERO,0_intkind1,wf(:,157))
  call vert_AQ_S(gH,wf(:,157),wf(:,-1),wf(:,158))
  call prop_A_Q(wf(:,72),Q(:,13),ZERO,0_intkind1,wf(:,159))
  call vert_AQ_S(gH,wf(:,159),wf(:,-1),wf(:,160))
  call vert_QA_V(wf(:,-1),wf(:,157),wf(:,161))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,157),wf(:,162))
  call prop_W_W(wf(:,162),Q(:,15),MZ,1_intkind1,wf(:,163))
  call vert_QA_V(wf(:,-1),wf(:,159),wf(:,164))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,159),wf(:,165))
  call prop_W_W(wf(:,165),Q(:,15),MZ,1_intkind1,wf(:,166))
  call vert_AQ_S(gH,wf(:,-2),wf(:,74),wf(:,167))
  call vert_QA_V(wf(:,74),wf(:,-2),wf(:,168))
  call vert_QA_Z(gZl,wf(:,74),wf(:,-2),wf(:,169))
  call prop_W_W(wf(:,169),Q(:,15),MZ,1_intkind1,wf(:,170))
  call vert_AQ_S(gH,wf(:,-2),wf(:,76),wf(:,171))
  call vert_AQ_S(gH,wf(:,-2),wf(:,78),wf(:,172))
  call vert_QA_V(wf(:,76),wf(:,-2),wf(:,173))
  call vert_QA_Z(gZl,wf(:,76),wf(:,-2),wf(:,174))
  call prop_W_W(wf(:,174),Q(:,15),MZ,1_intkind1,wf(:,175))
  call vert_QA_V(wf(:,78),wf(:,-2),wf(:,176))
  call vert_QA_Z(gZl,wf(:,78),wf(:,-2),wf(:,177))
  call prop_W_W(wf(:,177),Q(:,15),MZ,1_intkind1,wf(:,178))
  call prop_A_Q(wf(:,79),Q(:,14),ZERO,0_intkind1,wf(:,179))
  call vert_AQ_S(gH,wf(:,179),wf(:,0),wf(:,180))
  call prop_A_Q(wf(:,80),Q(:,14),ZERO,0_intkind1,wf(:,181))
  call vert_AQ_S(gH,wf(:,181),wf(:,0),wf(:,182))
  call prop_A_Q(wf(:,81),Q(:,14),ZERO,0_intkind1,wf(:,183))
  call vert_AQ_S(gH,wf(:,183),wf(:,0),wf(:,184))
  call vert_QA_V(wf(:,0),wf(:,179),wf(:,185))
  call vert_QA_Z(gZl,wf(:,0),wf(:,179),wf(:,186))
  call prop_W_W(wf(:,186),Q(:,15),MZ,1_intkind1,wf(:,187))
  call vert_QA_V(wf(:,0),wf(:,181),wf(:,188))
  call vert_QA_V(wf(:,0),wf(:,183),wf(:,189))
  call vert_QA_Z(gZl,wf(:,0),wf(:,181),wf(:,190))
  call prop_W_W(wf(:,190),Q(:,15),MZ,1_intkind1,wf(:,191))
  call vert_QA_Z(gZl,wf(:,0),wf(:,183),wf(:,192))
  call prop_W_W(wf(:,192),Q(:,15),MZ,1_intkind1,wf(:,193))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MH2)
  den(2) = 1 / (Q(5,10) - MH2)
  den(4) = 1 / (Q(5,5))
  den(5) = 1 / (Q(5,10))
  den(7) = 1 / (Q(5,10) - MZ2)
  den(9) = 1 / (Q(5,5) - MZ2)
  den(12) = 1 / (Q(5,9) - MH2)
  den(13) = 1 / (Q(5,6) - MH2)
  den(15) = 1 / (Q(5,9))
  den(16) = 1 / (Q(5,6))
  den(18) = 1 / (Q(5,6) - MZ2)
  den(20) = 1 / (Q(5,9) - MZ2)
  den(23) = 1 / (Q(5,48) - MH2)
  den(25) = 1 / (Q(5,48) - MZ2)
  den(31) = 1 / (Q(5,7))
  den(41) = 1 / (Q(5,50))
  den(65) = 1 / (Q(5,49))
  den(74) = 1 / (Q(5,11))
  den(105) = 1 / (Q(5,15) - MH2)
  den(107) = 1 / (Q(5,15) - MZ2)
  den(112) = 1 / (Q(5,15))
  den(121) = 1 / (Q(5,13))
  den(147) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(8) = den(4)*den(7)
  den(10) = den(5)*den(9)
  den(11) = den(7)*den(9)
  den(14) = den(12)*den(13)
  den(17) = den(15)*den(16)
  den(19) = den(15)*den(18)
  den(21) = den(16)*den(20)
  den(22) = den(18)*den(20)
  den(24) = den(3)*den(23)
  den(26) = den(1)*den(7)
  den(27) = den(25)*den(26)
  den(28) = den(2)*den(9)
  den(29) = den(25)*den(28)
  den(30) = den(11)*den(23)
  den(32) = den(1)*den(31)
  den(33) = den(23)*den(32)
  den(34) = den(25)*den(32)
  den(35) = den(4)*den(31)
  den(36) = den(23)*den(35)
  den(37) = den(9)*den(31)
  den(38) = den(23)*den(37)
  den(39) = den(25)*den(35)
  den(40) = den(25)*den(37)
  den(42) = den(23)*den(41)
  den(43) = den(1)*den(42)
  den(44) = den(25)*den(41)
  den(45) = den(1)*den(44)
  den(46) = den(4)*den(42)
  den(47) = den(9)*den(42)
  den(48) = den(4)*den(44)
  den(49) = den(9)*den(44)
  den(50) = den(14)*den(23)
  den(51) = den(12)*den(18)
  den(52) = den(25)*den(51)
  den(53) = den(13)*den(20)
  den(54) = den(25)*den(53)
  den(55) = den(22)*den(23)
  den(56) = den(13)*den(31)
  den(57) = den(23)*den(56)
  den(58) = den(25)*den(56)
  den(59) = den(16)*den(31)
  den(60) = den(23)*den(59)
  den(61) = den(18)*den(31)
  den(62) = den(23)*den(61)
  den(63) = den(25)*den(59)
  den(64) = den(25)*den(61)
  den(66) = den(23)*den(65)
  den(67) = den(13)*den(66)
  den(68) = den(16)*den(66)
  den(69) = den(18)*den(66)
  den(70) = den(25)*den(65)
  den(71) = den(13)*den(70)
  den(72) = den(16)*den(70)
  den(73) = den(18)*den(70)
  den(75) = den(12)*den(74)
  den(76) = den(23)*den(75)
  den(77) = den(25)*den(75)
  den(78) = den(15)*den(74)
  den(79) = den(23)*den(78)
  den(80) = den(20)*den(74)
  den(81) = den(23)*den(80)
  den(82) = den(25)*den(78)
  den(83) = den(25)*den(80)
  den(84) = den(12)*den(42)
  den(85) = den(12)*den(44)
  den(86) = den(15)*den(42)
  den(87) = den(20)*den(42)
  den(88) = den(15)*den(44)
  den(89) = den(20)*den(44)
  den(90) = den(2)*den(74)
  den(91) = den(23)*den(90)
  den(92) = den(25)*den(90)
  den(93) = den(5)*den(74)
  den(94) = den(23)*den(93)
  den(95) = den(7)*den(74)
  den(96) = den(23)*den(95)
  den(97) = den(25)*den(93)
  den(98) = den(25)*den(95)
  den(99) = den(2)*den(66)
  den(100) = den(5)*den(66)
  den(101) = den(7)*den(66)
  den(102) = den(2)*den(70)
  den(103) = den(5)*den(70)
  den(104) = den(7)*den(70)
  den(106) = den(3)*den(105)
  den(108) = den(26)*den(107)
  den(109) = den(28)*den(107)
  den(110) = den(11)*den(105)
  den(111) = den(32)*den(105)
  den(113) = den(32)*den(112)
  den(114) = den(32)*den(107)
  den(115) = den(35)*den(105)
  den(116) = den(37)*den(105)
  den(117) = den(35)*den(112)
  den(118) = den(35)*den(107)
  den(119) = den(37)*den(112)
  den(120) = den(37)*den(107)
  den(122) = den(1)*den(121)
  den(123) = den(105)*den(122)
  den(124) = den(112)*den(122)
  den(125) = den(107)*den(122)
  den(126) = den(4)*den(121)
  den(127) = den(105)*den(126)
  den(128) = den(9)*den(121)
  den(129) = den(105)*den(128)
  den(130) = den(112)*den(126)
  den(131) = den(107)*den(126)
  den(132) = den(112)*den(128)
  den(133) = den(107)*den(128)
  den(134) = den(14)*den(105)
  den(135) = den(51)*den(107)
  den(136) = den(53)*den(107)
  den(137) = den(22)*den(105)
  den(138) = den(56)*den(105)
  den(139) = den(56)*den(112)
  den(140) = den(56)*den(107)
  den(141) = den(59)*den(105)
  den(142) = den(61)*den(105)
  den(143) = den(59)*den(112)
  den(144) = den(59)*den(107)
  den(145) = den(61)*den(112)
  den(146) = den(61)*den(107)
  den(148) = den(13)*den(147)
  den(149) = den(105)*den(148)
  den(150) = den(16)*den(147)
  den(151) = den(105)*den(150)
  den(152) = den(18)*den(147)
  den(153) = den(105)*den(152)
  den(154) = den(112)*den(148)
  den(155) = den(107)*den(148)
  den(156) = den(112)*den(150)
  den(157) = den(112)*den(152)
  den(158) = den(107)*den(150)
  den(159) = den(107)*den(152)
  den(160) = den(75)*den(105)
  den(161) = den(75)*den(112)
  den(162) = den(75)*den(107)
  den(163) = den(78)*den(105)
  den(164) = den(80)*den(105)
  den(165) = den(78)*den(112)
  den(166) = den(78)*den(107)
  den(167) = den(80)*den(112)
  den(168) = den(80)*den(107)
  den(169) = den(12)*den(121)
  den(170) = den(105)*den(169)
  den(171) = den(112)*den(169)
  den(172) = den(107)*den(169)
  den(173) = den(15)*den(121)
  den(174) = den(105)*den(173)
  den(175) = den(20)*den(121)
  den(176) = den(105)*den(175)
  den(177) = den(112)*den(173)
  den(178) = den(107)*den(173)
  den(179) = den(112)*den(175)
  den(180) = den(107)*den(175)
  den(181) = den(90)*den(105)
  den(182) = den(90)*den(112)
  den(183) = den(90)*den(107)
  den(184) = den(93)*den(105)
  den(185) = den(95)*den(105)
  den(186) = den(93)*den(112)
  den(187) = den(93)*den(107)
  den(188) = den(95)*den(112)
  den(189) = den(95)*den(107)
  den(190) = den(2)*den(147)
  den(191) = den(105)*den(190)
  den(192) = den(5)*den(147)
  den(193) = den(105)*den(192)
  den(194) = den(7)*den(147)
  den(195) = den(105)*den(194)
  den(196) = den(112)*den(190)
  den(197) = den(107)*den(190)
  den(198) = den(112)*den(192)
  den(199) = den(112)*den(194)
  den(200) = den(107)*den(192)
  den(201) = den(107)*den(194)
  den(202) = den(1)*den(5)
  den(203) = den(2)*den(4)
  den(204) = den(12)*den(16)
  den(205) = den(13)*den(15)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(98)


  A(1) = cont_SS(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_SS(wf(:,2),wf(:,3)) * den(3)
  A(3) = cont_SS(wf(:,2),wf(:,3)) * den(3)
  A(4) = cont_VV(wf(:,5),wf(:,6)) * den(6)
  A(5) = cont_VV(wf(:,6),wf(:,8)) * den(8)
  A(6) = cont_VV(wf(:,5),wf(:,11)) * den(10)
  A(7) = cont_VV(wf(:,8),wf(:,11)) * den(11)
  A(8) = cont_SS(wf(:,13),wf(:,14)) * den(14)
  A(9) = cont_SS(wf(:,13),wf(:,14)) * den(14)
  A(10) = cont_SS(wf(:,13),wf(:,14)) * den(14)
  A(11) = cont_VV(wf(:,16),wf(:,17)) * den(17)
  A(12) = cont_VV(wf(:,17),wf(:,19)) * den(19)
  A(13) = cont_VV(wf(:,16),wf(:,22)) * den(21)
  A(14) = cont_VV(wf(:,19),wf(:,22)) * den(22)
  A(15) = cont_SS(wf(:,23),wf(:,24)) * den(24)
  A(16) = cont_SS(wf(:,23),wf(:,24)) * den(24)
  A(17) = cont_VV(wf(:,26),wf(:,27)) * den(27)
  A(18) = cont_VV(wf(:,26),wf(:,28)) * den(29)
  A(19) = cont_SS(wf(:,23),wf(:,29)) * den(30)
  A(20) = cont_SS(wf(:,23),wf(:,29)) * den(30)
  A(21) = cont_QA(wf(:,31),wf(:,32)) * den(33)
  A(22) = cont_QA(wf(:,31),wf(:,32)) * den(33)
  A(23) = cont_QA(wf(:,32),wf(:,33)) * den(34)
  A(24) = cont_QA(wf(:,31),wf(:,35)) * den(36)
  A(25) = cont_QA(wf(:,31),wf(:,35)) * den(36)
  A(26) = cont_QA(wf(:,31),wf(:,37)) * den(38)
  A(27) = cont_QA(wf(:,31),wf(:,37)) * den(38)
  A(28) = cont_QA(wf(:,33),wf(:,35)) * den(39)
  A(29) = cont_QA(wf(:,33),wf(:,37)) * den(40)
  A(30) = cont_QA(wf(:,39),wf(:,40)) * den(43)
  A(31) = cont_QA(wf(:,39),wf(:,40)) * den(43)
  A(32) = cont_QA(wf(:,39),wf(:,42)) * den(45)
  A(33) = cont_QA(wf(:,40),wf(:,43)) * den(46)
  A(34) = cont_QA(wf(:,40),wf(:,43)) * den(46)
  A(35) = cont_QA(wf(:,40),wf(:,44)) * den(47)
  A(36) = cont_QA(wf(:,40),wf(:,44)) * den(47)
  A(37) = cont_QA(wf(:,42),wf(:,43)) * den(48)
  A(38) = cont_QA(wf(:,42),wf(:,44)) * den(49)
  A(39) = cont_SS(wf(:,23),wf(:,45)) * den(50)
  A(40) = cont_SS(wf(:,23),wf(:,45)) * den(50)
  A(41) = cont_VV(wf(:,26),wf(:,46)) * den(52)
  A(42) = cont_VV(wf(:,26),wf(:,47)) * den(54)
  A(43) = cont_SS(wf(:,23),wf(:,48)) * den(55)
  A(44) = cont_SS(wf(:,23),wf(:,48)) * den(55)
  A(45) = cont_QA(wf(:,31),wf(:,50)) * den(57)
  A(46) = cont_QA(wf(:,31),wf(:,50)) * den(57)
  A(47) = cont_QA(wf(:,33),wf(:,50)) * den(58)
  A(48) = cont_QA(wf(:,31),wf(:,52)) * den(60)
  A(49) = cont_QA(wf(:,31),wf(:,52)) * den(60)
  A(50) = cont_QA(wf(:,31),wf(:,54)) * den(62)
  A(51) = cont_QA(wf(:,31),wf(:,54)) * den(62)
  A(52) = cont_QA(wf(:,33),wf(:,52)) * den(63)
  A(53) = cont_QA(wf(:,33),wf(:,54)) * den(64)
  A(54) = cont_QA(wf(:,56),wf(:,57)) * den(67)
  A(55) = cont_QA(wf(:,56),wf(:,57)) * den(67)
  A(56) = cont_QA(wf(:,57),wf(:,58)) * den(68)
  A(57) = cont_QA(wf(:,57),wf(:,58)) * den(68)
  A(58) = cont_QA(wf(:,57),wf(:,59)) * den(69)
  A(59) = cont_QA(wf(:,57),wf(:,59)) * den(69)
  A(60) = cont_QA(wf(:,56),wf(:,61)) * den(71)
  A(61) = cont_QA(wf(:,58),wf(:,61)) * den(72)
  A(62) = cont_QA(wf(:,59),wf(:,61)) * den(73)
  A(63) = cont_QA(wf(:,63),wf(:,64)) * den(76)
  A(64) = cont_QA(wf(:,63),wf(:,64)) * den(76)
  A(65) = cont_QA(wf(:,64),wf(:,65)) * den(77)
  A(66) = cont_QA(wf(:,63),wf(:,67)) * den(79)
  A(67) = cont_QA(wf(:,63),wf(:,67)) * den(79)
  A(68) = cont_QA(wf(:,63),wf(:,69)) * den(81)
  A(69) = cont_QA(wf(:,63),wf(:,69)) * den(81)
  A(70) = cont_QA(wf(:,65),wf(:,67)) * den(82)
  A(71) = cont_QA(wf(:,65),wf(:,69)) * den(83)
  A(72) = cont_QA(wf(:,40),wf(:,70)) * den(84)
  A(73) = cont_QA(wf(:,40),wf(:,70)) * den(84)
  A(74) = cont_QA(wf(:,42),wf(:,70)) * den(85)
  A(75) = cont_QA(wf(:,40),wf(:,71)) * den(86)
  A(76) = cont_QA(wf(:,40),wf(:,71)) * den(86)
  A(77) = cont_QA(wf(:,40),wf(:,72)) * den(87)
  A(78) = cont_QA(wf(:,40),wf(:,72)) * den(87)
  A(79) = cont_QA(wf(:,42),wf(:,71)) * den(88)
  A(80) = cont_QA(wf(:,42),wf(:,72)) * den(89)
  A(81) = cont_QA(wf(:,63),wf(:,74)) * den(91)
  A(82) = cont_QA(wf(:,63),wf(:,74)) * den(91)
  A(83) = cont_QA(wf(:,65),wf(:,74)) * den(92)
  A(84) = cont_QA(wf(:,63),wf(:,76)) * den(94)
  A(85) = cont_QA(wf(:,63),wf(:,76)) * den(94)
  A(86) = cont_QA(wf(:,63),wf(:,78)) * den(96)
  A(87) = cont_QA(wf(:,63),wf(:,78)) * den(96)
  A(88) = cont_QA(wf(:,65),wf(:,76)) * den(97)
  A(89) = cont_QA(wf(:,65),wf(:,78)) * den(98)
  A(90) = cont_QA(wf(:,57),wf(:,79)) * den(99)
  A(91) = cont_QA(wf(:,57),wf(:,79)) * den(99)
  A(92) = cont_QA(wf(:,57),wf(:,80)) * den(100)
  A(93) = cont_QA(wf(:,57),wf(:,80)) * den(100)
  A(94) = cont_QA(wf(:,57),wf(:,81)) * den(101)
  A(95) = cont_QA(wf(:,57),wf(:,81)) * den(101)
  A(96) = cont_QA(wf(:,61),wf(:,79)) * den(102)
  A(97) = cont_QA(wf(:,61),wf(:,80)) * den(103)
  A(98) = cont_QA(wf(:,61),wf(:,81)) * den(104)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(98)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*(-A(4)+A(11))*f(1)+2*(-A(5)-A(6)+A(12)+A(13))*f(2)+2*(A(28)+A(29)+A(37)+A(38)-A(52)-A(53)-A(61)-A(62)-A(70)-A(71) &
       -A(79)-A(80)+A(88)+A(89)+A(97)+A(98))*f(3)+2*(-A(7)+A(14))*f(4)+2*(A(20)-A(44))*f(14)+2*(-A(17)-A(18)+A(41)+A(42))*f(16) &
       +2*(A(25)+A(27)+A(34)+A(36)-A(49)-A(51)-A(57)-A(59)-A(67)-A(69)-A(76)-A(78)+A(85)+A(87)+A(93)+A(95))*f(19)+2*(-A(23)-A(32) &
       +A(47)+A(60)+A(65)+A(74)-A(83)-A(96))*f(22)+2*(-A(16)+A(40))*f(28)+2*(A(2)-A(9))*f(31)+2*(A(3)-A(10))*f(32)+2*(-A(22)-A(31) &
       +A(46)+A(55)+A(64)+A(73)-A(82)-A(91))*f(33)+2*(A(19)-A(43))*f(35)+2*(A(24)+A(26)+A(33)+A(35)-A(48)-A(50)-A(56)-A(58)-A(66) &
       -A(68)-A(75)-A(77)+A(84)+A(86)+A(92)+A(94))*f(37)+2*(-A(15)+A(39))*f(40)+2*(-A(21)-A(30)+A(45)+A(54)+A(63)+A(72)-A(81) &
       -A(90))*f(42)+2*(A(1)-A(8))*f(45)

end subroutine colourvectors

end module ol_loop_ppllll2_eeexexgg_1_/**/REALKIND
