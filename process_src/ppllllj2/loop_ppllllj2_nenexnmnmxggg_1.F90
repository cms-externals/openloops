
module ol_colourmatrix_ppllllj2_nenexnmnmxggg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nenexnmnmxggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nenexnmnmxggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_nenexnmnmxggg_1_/**/REALKIND

module ol_loop_ppllllj2_nenexnmnmxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(13), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:52)
  ! denominators
  complex(REALKIND), save :: den(66)
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
    f( 2) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 3) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 4) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f( 5) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f( 6) = eQED**4*gQCD**3*integralnorm*SwF
    f( 7) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f( 8) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f( 9) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(2._/**/REALKIND*cw**2*sw**2)
    f(10) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(11) = (countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(12) = (CI*eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(2._/**/REALKIND*cw**2*sw**2)
    f(13) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND) :: A(29)
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
  call vert_QA_Z(gZn,wf(:,-2),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,12),MZ,1_intkind1,wf(:,5))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-6),wf(:,6))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,7))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-5),wf(:,8))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,9))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-4),wf(:,10))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,11))
  call prop_W_W(wf(:,11),Q(:,112),MZ,1_intkind1,wf(:,12))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,-2),wf(:,13))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,12),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,7),ZERO,0_intkind1,wf(:,15))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,16))
  call prop_W_W(wf(:,16),Q(:,112),MZ,1_intkind1,wf(:,17))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,17),wf(:,18))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,-2),wf(:,19))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,4),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,116),ZERO,0_intkind1,wf(:,21))
  call vert_ZQ_A(gZn,wf(:,17),wf(:,-2),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,116),ZERO,0_intkind1,wf(:,23))
  call vert_ZQ_A(gZn,wf(:,5),wf(:,0),wf(:,24))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,12),wf(:,25))
  call prop_Q_A(wf(:,24),Q(:,13),ZERO,0_intkind1,wf(:,26))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,17),wf(:,27))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,0),wf(:,28))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,5),wf(:,29))
  call prop_Q_A(wf(:,28),Q(:,113),ZERO,0_intkind1,wf(:,30))
  call vert_ZQ_A(gZn,wf(:,17),wf(:,0),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,113),ZERO,0_intkind1,wf(:,32))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,33))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,34))
  call counter_GG_S(wf(:,-5),wf(:,7),wf(:,35))
  call counter_GG_S(wf(:,-4),wf(:,9),wf(:,36))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,37))
  call prop_W_W(wf(:,37),Q(:,112),MZ,1_intkind1,wf(:,38))
  call vert_QA_Z(gZn,wf(:,15),wf(:,-3),wf(:,39))
  call prop_A_Q(wf(:,20),Q(:,11),ZERO,0_intkind1,wf(:,40))
  call vert_QA_Z(gZn,wf(:,-2),wf(:,40),wf(:,41))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,7),Q(:,80),wf(:,42))
  call prop_W_W(wf(:,42),Q(:,112),MZ,1_intkind1,wf(:,43))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,9),Q(:,96),wf(:,44))
  call prop_W_W(wf(:,44),Q(:,112),MZ,1_intkind1,wf(:,45))
  call vert_QA_Z(gZn,wf(:,26),wf(:,-1),wf(:,46))
  call prop_A_Q(wf(:,29),Q(:,14),ZERO,0_intkind1,wf(:,47))
  call vert_QA_Z(gZn,wf(:,0),wf(:,47),wf(:,48))
  call prop_W_W(wf(:,39),Q(:,15),MZ,1_intkind1,wf(:,49))
  call prop_W_W(wf(:,41),Q(:,15),MZ,1_intkind1,wf(:,50))
  call prop_W_W(wf(:,46),Q(:,15),MZ,1_intkind1,wf(:,51))
  call prop_W_W(wf(:,48),Q(:,15),MZ,1_intkind1,wf(:,52))

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
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,80))
  den(8) = 1 / (Q(5,96))
  den(10) = 1 / (Q(5,112) - MZ2)
  den(11) = 1 / (Q(5,7))
  den(14) = 1 / (Q(5,116))
  den(17) = 1 / (Q(5,13))
  den(20) = 1 / (Q(5,113))
  den(23) = 1 / (Q(5,15) - MH2)
  den(30) = 1 / (Q(5,11))
  den(40) = 1 / (Q(5,14))
  den(47) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(4)*den(8)
  den(12) = den(1)*den(11)
  den(13) = den(10)*den(12)
  den(15) = den(10)*den(14)
  den(16) = den(1)*den(15)
  den(18) = den(2)*den(17)
  den(19) = den(10)*den(18)
  den(21) = den(10)*den(20)
  den(22) = den(2)*den(21)
  den(24) = den(4)*den(23)
  den(25) = den(3)*den(24)
  den(26) = den(6)*den(24)
  den(27) = den(8)*den(24)
  den(28) = den(3)*den(10)
  den(29) = den(12)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(28)*den(31)
  den(33) = den(6)*den(10)
  den(34) = den(12)*den(33)
  den(35) = den(31)*den(33)
  den(36) = den(8)*den(10)
  den(37) = den(12)*den(36)
  den(38) = den(31)*den(36)
  den(39) = den(18)*den(28)
  den(41) = den(2)*den(40)
  den(42) = den(28)*den(41)
  den(43) = den(18)*den(33)
  den(44) = den(33)*den(41)
  den(45) = den(18)*den(36)
  den(46) = den(36)*den(41)
  den(48) = den(12)*den(47)
  den(49) = den(31)*den(47)
  den(50) = den(18)*den(47)
  den(51) = den(41)*den(47)
  den(52) = den(1)*den(2)*den(3)
  den(53) = den(1)*den(2)*den(6)
  den(54) = den(1)*den(2)*den(8)
  den(55) = den(3)*den(48)
  den(56) = den(3)*den(49)
  den(57) = den(6)*den(48)
  den(58) = den(6)*den(49)
  den(59) = den(8)*den(48)
  den(60) = den(8)*den(49)
  den(61) = den(3)*den(50)
  den(62) = den(3)*den(51)
  den(63) = den(6)*den(50)
  den(64) = den(6)*den(51)
  den(65) = den(8)*den(50)
  den(66) = den(8)*den(51)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(29)


  A(1) = cont_VV(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(13)
  A(5) = cont_QA(wf(:,15),wf(:,18)) * den(13)
  A(6) = cont_QA(wf(:,20),wf(:,21)) * den(16)
  A(7) = cont_QA(wf(:,20),wf(:,23)) * den(16)
  A(8) = cont_QA(wf(:,25),wf(:,26)) * den(19)
  A(9) = cont_QA(wf(:,26),wf(:,27)) * den(19)
  A(10) = cont_QA(wf(:,29),wf(:,30)) * den(22)
  A(11) = cont_QA(wf(:,29),wf(:,32)) * den(22)
  A(12) = cont_SS(wf(:,33),wf(:,34)) * den(25)
  A(13) = cont_SS(wf(:,33),wf(:,34)) * den(25)
  A(14) = cont_SS(wf(:,33),wf(:,35)) * den(26)
  A(15) = cont_SS(wf(:,33),wf(:,35)) * den(26)
  A(16) = cont_SS(wf(:,33),wf(:,36)) * den(27)
  A(17) = cont_SS(wf(:,33),wf(:,36)) * den(27)
  A(18) = cont_VV(wf(:,38),wf(:,39)) * den(29)
  A(19) = cont_VV(wf(:,38),wf(:,41)) * den(32)
  A(20) = cont_VV(wf(:,39),wf(:,43)) * den(34)
  A(21) = cont_VV(wf(:,41),wf(:,43)) * den(35)
  A(22) = cont_VV(wf(:,39),wf(:,45)) * den(37)
  A(23) = cont_VV(wf(:,41),wf(:,45)) * den(38)
  A(24) = cont_VV(wf(:,38),wf(:,46)) * den(39)
  A(25) = cont_VV(wf(:,38),wf(:,48)) * den(42)
  A(26) = cont_VV(wf(:,43),wf(:,46)) * den(43)
  A(27) = cont_VV(wf(:,43),wf(:,48)) * den(44)
  A(28) = cont_VV(wf(:,45),wf(:,46)) * den(45)
  A(29) = cont_VV(wf(:,45),wf(:,48)) * den(46)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(29)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (A(4)+A(6)+A(8)+A(10))*f(1)+2*CI*(A(18)+A(19)-A(20)-A(21)+A(22)+A(23)+A(24)+A(25)-A(26)-A(27)+A(28)+A(29))*f(2) &
       +2*CI*(A(1)-A(2)+A(3))*f(3)+2*CI*(-A(13)+A(15)-A(17))*f(8)+2*CI*(-A(12)+A(14)-A(16))*f(11)
  M2(2) = (A(5)+A(7)+A(9)+A(11))*f(1)+2*CI*(-A(18)-A(19)+A(20)+A(21)-A(22)-A(23)-A(24)-A(25)+A(26)+A(27)-A(28)-A(29))*f(2)+2*CI*( &
       -A(1)+A(2)-A(3))*f(3)+2*CI*(A(13)-A(15)+A(17))*f(8)+2*CI*(A(12)-A(14)+A(16))*f(11)

end subroutine colourvectors

end module ol_loop_ppllllj2_nenexnmnmxggg_1_/**/REALKIND
