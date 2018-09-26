
module ol_colourmatrix_ppllll2_nenexeexgg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll2_nenexeexgg_1_/**/REALKIND



module ol_forced_parameters_ppllll2_nenexeexgg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll2_nenexeexgg_1_/**/REALKIND

module ol_loop_ppllll2_nenexeexgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(39), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:71)
  ! denominators
  complex(REALKIND), save :: den(71)
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
    f( 1) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**2
    f( 2) = countertermnorm*ctZGG*eQED**4*gQCD**2
    f( 3) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**2
    f( 4) = (countertermnorm*ctZGG*cw*eQED**4*gQCD**2)/(sw**3*2._/**/REALKIND)
    f( 5) = (CI*countertermnorm*ctWWGG*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 6) = (countertermnorm*ctZGG*eQED**4*gQCD**2)/(sw**2*2._/**/REALKIND)
    f( 7) = (eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 8) = (2*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = eQED**4*gQCD**2*integralnorm*SwF
    f(10) = (4*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(11) = 2*eQED**4*gQCD**2*integralnorm*SwF
    f(12) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(13) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(14) = (cw*eQED**4*gQCD**2*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(15) = (cw*eQED**4*gQCD**2*integralnorm*SwF)/sw**3
    f(16) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(17) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(18) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(19) = (2*eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(20) = (eQED**4*gQCD**2*integralnorm*SwF)/sw**2
    f(21) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHWW*MB*MW*YB)/(2._/**/REALKIND*MQ2sum*sw**3)
    f(22) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(23) = (eQED**4*gQCD**2*integralnorm*lambdaHWW*SwF*YB)/(sw**4*4._/**/REALKIND)
    f(24) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(25) = (countertermnorm*ctZGG*eQED**4*gQCD**2*lambdaHZZ*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(26) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(27) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(28) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YE)/(4._/**/REALKIND*MQ2sum*MW*sw**3)
    f(29) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YE)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(30) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE)/(MW**2*sw**4*8._/**/REALKIND)
    f(31) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(32) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHWW*MT*MW*YT)/(2._/**/REALKIND*MQ2sum*sw**3)
    f(33) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(34) = (eQED**4*gQCD**2*integralnorm*lambdaHWW*SwF*YT)/(sw**4*4._/**/REALKIND)
    f(35) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(36) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE*YT)/(4._/**/REALKIND*MQ2sum*MW*sw**3)
    f(37) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(38) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YT)/(MW**2*sw**4*8._/**/REALKIND)
    f(39) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)


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
  complex(REALKIND) :: A(25)
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
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,3),wf(:,4))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,12),MZ,1_intkind1,wf(:,6))
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,7))
  call vert_QA_W(wf(:,-2),wf(:,-1),wf(:,8))
  call prop_W_W(wf(:,7),Q(:,9),MW,1_intkind1,wf(:,9))
  call prop_W_W(wf(:,8),Q(:,6),MW,1_intkind1,wf(:,10))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,9),wf(:,11))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,12))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,13))
  call prop_W_W(wf(:,13),Q(:,48),MZ,1_intkind1,wf(:,14))
  call vert_SV_V(wf(:,12),wf(:,3),wf(:,15))
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,16))
  call vert_VV_S(wf(:,3),wf(:,6),wf(:,17))
  call vert_ZQ_A(gZl,wf(:,3),wf(:,-2),wf(:,18))
  call vert_SA_Q(gH,wf(:,16),wf(:,-3),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,7),ZERO,0_intkind1,wf(:,20))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,14),wf(:,21))
  call vert_QS_A(gH,wf(:,-2),wf(:,16),wf(:,22))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,3),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,52),ZERO,0_intkind1,wf(:,24))
  call vert_ZQ_A(gZl,wf(:,14),wf(:,-2),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,52),ZERO,0_intkind1,wf(:,26))
  call vert_VV_S(wf(:,10),wf(:,9),wf(:,27))
  call vert_UV_W(wf(:,9),Q(:,9),wf(:,10),Q(:,6),wf(:,28))
  call vert_WQ_A(wf(:,10),wf(:,0),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,7),ZERO,0_intkind1,wf(:,30))
  call vert_ZQ_A(gZn,wf(:,14),wf(:,0),wf(:,31))
  call vert_AW_Q(wf(:,-3),wf(:,10),wf(:,32))
  call prop_Q_A(wf(:,31),Q(:,49),ZERO,0_intkind1,wf(:,33))
  call vert_AW_Q(wf(:,-1),wf(:,9),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,11),ZERO,0_intkind1,wf(:,35))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,14),wf(:,36))
  call vert_WQ_A(wf(:,9),wf(:,-2),wf(:,37))
  call prop_A_Q(wf(:,36),Q(:,50),ZERO,0_intkind1,wf(:,38))
  call vert_ZQ_A(gZn,wf(:,6),wf(:,0),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,13),ZERO,0_intkind1,wf(:,40))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,6),wf(:,41))
  call prop_W_W(wf(:,15),Q(:,15),MZ,1_intkind1,wf(:,42))
  call vert_AQ_S(gH,wf(:,-3),wf(:,20),wf(:,43))
  call vert_QA_V(wf(:,20),wf(:,-3),wf(:,44))
  call vert_QA_Z(gZl,wf(:,20),wf(:,-3),wf(:,45))
  call prop_W_W(wf(:,45),Q(:,15),MZ,1_intkind1,wf(:,46))
  call prop_A_Q(wf(:,23),Q(:,11),ZERO,0_intkind1,wf(:,47))
  call vert_AQ_S(gH,wf(:,47),wf(:,-2),wf(:,48))
  call vert_QA_V(wf(:,-2),wf(:,47),wf(:,49))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,47),wf(:,50))
  call prop_W_W(wf(:,50),Q(:,15),MZ,1_intkind1,wf(:,51))
  call prop_W_W(wf(:,28),Q(:,15),MZ,1_intkind1,wf(:,52))
  call vert_AQ_S(gH,wf(:,-3),wf(:,30),wf(:,53))
  call vert_QA_V(wf(:,30),wf(:,-3),wf(:,54))
  call vert_QA_Z(gZl,wf(:,30),wf(:,-3),wf(:,55))
  call prop_W_W(wf(:,55),Q(:,15),MZ,1_intkind1,wf(:,56))
  call prop_A_Q(wf(:,32),Q(:,14),ZERO,0_intkind1,wf(:,57))
  call vert_QA_Z(gZn,wf(:,0),wf(:,57),wf(:,58))
  call prop_W_W(wf(:,58),Q(:,15),MZ,1_intkind1,wf(:,59))
  call vert_AQ_S(gH,wf(:,35),wf(:,-2),wf(:,60))
  call vert_QA_V(wf(:,-2),wf(:,35),wf(:,61))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,35),wf(:,62))
  call prop_W_W(wf(:,62),Q(:,15),MZ,1_intkind1,wf(:,63))
  call prop_Q_A(wf(:,37),Q(:,13),ZERO,0_intkind1,wf(:,64))
  call vert_QA_Z(gZn,wf(:,64),wf(:,-1),wf(:,65))
  call prop_W_W(wf(:,65),Q(:,15),MZ,1_intkind1,wf(:,66))
  call vert_QA_Z(gZn,wf(:,40),wf(:,-1),wf(:,67))
  call prop_W_W(wf(:,67),Q(:,15),MZ,1_intkind1,wf(:,68))
  call prop_A_Q(wf(:,41),Q(:,14),ZERO,0_intkind1,wf(:,69))
  call vert_QA_Z(gZn,wf(:,0),wf(:,69),wf(:,70))
  call prop_W_W(wf(:,70),Q(:,15),MZ,1_intkind1,wf(:,71))

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
  den(4) = 1 / (Q(5,12) - MZ2)
  den(6) = 1 / (Q(5,9) - MW2)
  den(7) = 1 / (Q(5,6) - MW2)
  den(9) = 1 / (Q(5,12) - MH2)
  den(10) = 1 / (Q(5,48) - MZ2)
  den(13) = 1 / (Q(5,48) - MH2)
  den(15) = 1 / (Q(5,7))
  den(19) = 1 / (Q(5,52))
  den(29) = 1 / (Q(5,49))
  den(32) = 1 / (Q(5,11))
  den(36) = 1 / (Q(5,50))
  den(39) = 1 / (Q(5,13))
  den(43) = 1 / (Q(5,15) - MZ2)
  den(45) = 1 / (Q(5,15) - MH2)
  den(48) = 1 / (Q(5,15))
  den(61) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(5)*den(13)
  den(16) = den(1)*den(15)
  den(17) = den(13)*den(16)
  den(18) = den(10)*den(16)
  den(20) = den(13)*den(19)
  den(21) = den(1)*den(20)
  den(22) = den(10)*den(19)
  den(23) = den(1)*den(22)
  den(24) = den(8)*den(13)
  den(25) = den(8)*den(10)
  den(26) = den(7)*den(15)
  den(27) = den(13)*den(26)
  den(28) = den(10)*den(26)
  den(30) = den(10)*den(29)
  den(31) = den(7)*den(30)
  den(33) = den(6)*den(32)
  den(34) = den(13)*den(33)
  den(35) = den(10)*den(33)
  den(37) = den(10)*den(36)
  den(38) = den(6)*den(37)
  den(40) = den(4)*den(39)
  den(41) = den(10)*den(40)
  den(42) = den(4)*den(30)
  den(44) = den(11)*den(43)
  den(46) = den(5)*den(45)
  den(47) = den(16)*den(45)
  den(49) = den(16)*den(48)
  den(50) = den(16)*den(43)
  den(51) = den(1)*den(32)
  den(52) = den(45)*den(51)
  den(53) = den(48)*den(51)
  den(54) = den(43)*den(51)
  den(55) = den(8)*den(45)
  den(56) = den(8)*den(48)
  den(57) = den(8)*den(43)
  den(58) = den(26)*den(45)
  den(59) = den(26)*den(48)
  den(60) = den(26)*den(43)
  den(62) = den(7)*den(61)
  den(63) = den(43)*den(62)
  den(64) = den(33)*den(45)
  den(65) = den(33)*den(48)
  den(66) = den(33)*den(43)
  den(67) = den(6)*den(39)
  den(68) = den(43)*den(67)
  den(69) = den(40)*den(43)
  den(70) = den(4)*den(61)
  den(71) = den(43)*den(70)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(25)


  A(1) = cont_VV(wf(:,2),wf(:,4)) * den(3)
  A(2) = cont_VV(wf(:,4),wf(:,6)) * den(5)
  A(3) = cont_VV(wf(:,10),wf(:,11)) * den(8)
  A(4) = cont_VV(wf(:,14),wf(:,15)) * den(12)
  A(5) = cont_SS(wf(:,16),wf(:,17)) * den(14)
  A(6) = cont_SS(wf(:,16),wf(:,17)) * den(14)
  A(7) = cont_QA(wf(:,19),wf(:,20)) * den(17)
  A(8) = cont_QA(wf(:,19),wf(:,20)) * den(17)
  A(9) = cont_QA(wf(:,20),wf(:,21)) * den(18)
  A(10) = cont_QA(wf(:,23),wf(:,24)) * den(21)
  A(11) = cont_QA(wf(:,23),wf(:,24)) * den(21)
  A(12) = cont_QA(wf(:,23),wf(:,26)) * den(23)
  A(13) = cont_SS(wf(:,16),wf(:,27)) * den(24)
  A(14) = cont_SS(wf(:,16),wf(:,27)) * den(24)
  A(15) = cont_VV(wf(:,14),wf(:,28)) * den(25)
  A(16) = cont_QA(wf(:,19),wf(:,30)) * den(27)
  A(17) = cont_QA(wf(:,19),wf(:,30)) * den(27)
  A(18) = cont_QA(wf(:,21),wf(:,30)) * den(28)
  A(19) = cont_QA(wf(:,32),wf(:,33)) * den(31)
  A(20) = cont_QA(wf(:,22),wf(:,35)) * den(34)
  A(21) = cont_QA(wf(:,22),wf(:,35)) * den(34)
  A(22) = cont_QA(wf(:,25),wf(:,35)) * den(35)
  A(23) = cont_QA(wf(:,37),wf(:,38)) * den(38)
  A(24) = cont_QA(wf(:,36),wf(:,40)) * den(41)
  A(25) = cont_QA(wf(:,33),wf(:,41)) * den(42)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(25)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*A(1)*f(1)+2*(-A(9)-A(12)-A(24)-A(25))*f(2)+2*A(2)*f(3)+2*A(15)*f(4)-2*A(3)*f(5)+2*(A(18)+A(19)+A(22)+A(23))*f(6) &
       +2*A(14)*f(21)-2*A(6)*f(22)+2*A(4)*f(25)+2*(A(17)+A(21))*f(28)+2*(-A(8)-A(11))*f(29)+2*A(13)*f(32)-2*A(5)*f(33)+2*(A(16) &
       +A(20))*f(36)+2*(-A(7)-A(10))*f(37)

end subroutine colourvectors

end module ol_loop_ppllll2_nenexeexgg_1_/**/REALKIND
