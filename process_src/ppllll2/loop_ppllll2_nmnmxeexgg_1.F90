
module ol_colourmatrix_ppllll2_nmnmxeexgg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll2_nmnmxeexgg_1_/**/REALKIND



module ol_forced_parameters_ppllll2_nmnmxeexgg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll2_nmnmxeexgg_1_/**/REALKIND

module ol_loop_ppllll2_nmnmxeexgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:42)
  ! denominators
  complex(REALKIND), save :: den(43)
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
    f( 4) = (eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 5) = (2*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 6) = eQED**4*gQCD**2*integralnorm*SwF
    f( 7) = (4*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 8) = 2*eQED**4*gQCD**2*integralnorm*SwF
    f( 9) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(10) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(11) = (countertermnorm*ctZGG*eQED**4*gQCD**2*lambdaHZZ*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(12) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(13) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(14) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MB*YB*YE)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(15) = (eQED**4*gQCD**2*integralnorm*SwF*YB*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(16) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(17) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(18) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MT*YE*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(19) = (eQED**4*gQCD**2*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)


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
  complex(REALKIND) :: A(13)
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
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,7))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,8))
  call prop_W_W(wf(:,8),Q(:,48),MZ,1_intkind1,wf(:,9))
  call vert_SV_V(wf(:,7),wf(:,3),wf(:,10))
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,11))
  call vert_VV_S(wf(:,3),wf(:,6),wf(:,12))
  call vert_ZQ_A(gZl,wf(:,3),wf(:,-2),wf(:,13))
  call vert_SA_Q(gH,wf(:,11),wf(:,-3),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,7),ZERO,0_intkind1,wf(:,15))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,9),wf(:,16))
  call vert_QS_A(gH,wf(:,-2),wf(:,11),wf(:,17))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,3),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,52),ZERO,0_intkind1,wf(:,19))
  call vert_ZQ_A(gZl,wf(:,9),wf(:,-2),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,52),ZERO,0_intkind1,wf(:,21))
  call vert_ZQ_A(gZn,wf(:,6),wf(:,0),wf(:,22))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,9),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,13),ZERO,0_intkind1,wf(:,24))
  call vert_ZQ_A(gZn,wf(:,9),wf(:,0),wf(:,25))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,6),wf(:,26))
  call prop_Q_A(wf(:,25),Q(:,49),ZERO,0_intkind1,wf(:,27))
  call prop_W_W(wf(:,10),Q(:,15),MZ,1_intkind1,wf(:,28))
  call vert_AQ_S(gH,wf(:,-3),wf(:,15),wf(:,29))
  call vert_QA_V(wf(:,15),wf(:,-3),wf(:,30))
  call vert_QA_Z(gZl,wf(:,15),wf(:,-3),wf(:,31))
  call prop_W_W(wf(:,31),Q(:,15),MZ,1_intkind1,wf(:,32))
  call prop_A_Q(wf(:,18),Q(:,11),ZERO,0_intkind1,wf(:,33))
  call vert_AQ_S(gH,wf(:,33),wf(:,-2),wf(:,34))
  call vert_QA_V(wf(:,-2),wf(:,33),wf(:,35))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,33),wf(:,36))
  call prop_W_W(wf(:,36),Q(:,15),MZ,1_intkind1,wf(:,37))
  call vert_QA_Z(gZn,wf(:,24),wf(:,-1),wf(:,38))
  call prop_W_W(wf(:,38),Q(:,15),MZ,1_intkind1,wf(:,39))
  call prop_A_Q(wf(:,26),Q(:,14),ZERO,0_intkind1,wf(:,40))
  call vert_QA_Z(gZn,wf(:,0),wf(:,40),wf(:,41))
  call prop_W_W(wf(:,41),Q(:,15),MZ,1_intkind1,wf(:,42))

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
  den(6) = 1 / (Q(5,12) - MH2)
  den(7) = 1 / (Q(5,48) - MZ2)
  den(10) = 1 / (Q(5,48) - MH2)
  den(12) = 1 / (Q(5,7))
  den(16) = 1 / (Q(5,52))
  den(21) = 1 / (Q(5,13))
  den(24) = 1 / (Q(5,49))
  den(27) = 1 / (Q(5,15) - MZ2)
  den(29) = 1 / (Q(5,15) - MH2)
  den(32) = 1 / (Q(5,15))
  den(35) = 1 / (Q(5,11))
  den(41) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(1)*den(6)
  den(9) = den(7)*den(8)
  den(11) = den(5)*den(10)
  den(13) = den(1)*den(12)
  den(14) = den(10)*den(13)
  den(15) = den(7)*den(13)
  den(17) = den(10)*den(16)
  den(18) = den(1)*den(17)
  den(19) = den(7)*den(16)
  den(20) = den(1)*den(19)
  den(22) = den(4)*den(21)
  den(23) = den(7)*den(22)
  den(25) = den(7)*den(24)
  den(26) = den(4)*den(25)
  den(28) = den(8)*den(27)
  den(30) = den(5)*den(29)
  den(31) = den(13)*den(29)
  den(33) = den(13)*den(32)
  den(34) = den(13)*den(27)
  den(36) = den(1)*den(35)
  den(37) = den(29)*den(36)
  den(38) = den(32)*den(36)
  den(39) = den(27)*den(36)
  den(40) = den(22)*den(27)
  den(42) = den(4)*den(41)
  den(43) = den(27)*den(42)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(13)


  A(1) = cont_VV(wf(:,2),wf(:,4)) * den(3)
  A(2) = cont_VV(wf(:,4),wf(:,6)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(4) = cont_SS(wf(:,11),wf(:,12)) * den(11)
  A(5) = cont_SS(wf(:,11),wf(:,12)) * den(11)
  A(6) = cont_QA(wf(:,14),wf(:,15)) * den(14)
  A(7) = cont_QA(wf(:,14),wf(:,15)) * den(14)
  A(8) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(9) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(10) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(11) = cont_QA(wf(:,18),wf(:,21)) * den(20)
  A(12) = cont_QA(wf(:,23),wf(:,24)) * den(23)
  A(13) = cont_QA(wf(:,26),wf(:,27)) * den(26)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(13)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*A(1)*f(1)+2*(-A(8)-A(11)-A(12)-A(13))*f(2)+2*A(2)*f(3)-2*A(5)*f(9)+2*A(3)*f(11)+2*(-A(7)-A(10))*f(14)-2*A(4)*f(16)+2*( &
       -A(6)-A(9))*f(18)

end subroutine colourvectors

end module ol_loop_ppllll2_nmnmxeexgg_1_/**/REALKIND
