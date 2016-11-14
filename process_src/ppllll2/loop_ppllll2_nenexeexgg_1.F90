
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
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (MM /= 0) write(*,101) 'MM = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
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
  complex(REALKIND), save :: f(26), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:61)
  ! denominators
  complex(REALKIND), save :: den(58)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,64)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB, DOI
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
    f( 5) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MW)/(2._/**/REALKIND*sw**3)
    f( 6) = (CI*countertermnorm*ctWWGG*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 7) = (countertermnorm*ctZGG*eQED**4*gQCD**2)/(sw**2*2._/**/REALKIND)
    f( 8) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MW)/(cw**2*sw)
    f( 9) = (eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (2*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(11) = eQED**4*gQCD**2*integralnorm*SwF
    f(12) = (4*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(13) = 2*eQED**4*gQCD**2*integralnorm*SwF
    f(14) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(15) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(16) = (eQED**4*gQCD**2*integralnorm*MB*SwF)/(sw**4*4._/**/REALKIND)
    f(17) = (eQED**4*gQCD**2*integralnorm*MT*SwF)/(sw**4*4._/**/REALKIND)
    f(18) = (cw*eQED**4*gQCD**2*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(19) = (cw*eQED**4*gQCD**2*integralnorm*SwF)/sw**3
    f(20) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(21) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(22) = (eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(23) = (2*eQED**4*gQCD**2*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(24) = (eQED**4*gQCD**2*integralnorm*SwF)/sw**2
    f(25) = (eQED**4*gQCD**2*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(26) = (eQED**4*gQCD**2*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND) :: A(14)
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
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,12))
  call vert_VV_S(wf(:,3),wf(:,6),wf(:,13))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,14))
  call prop_W_W(wf(:,14),Q(:,48),MZ,1_intkind1,wf(:,15))
  call vert_ZQ_A(gZl,wf(:,3),wf(:,-2),wf(:,16))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,15),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call vert_ZQ_A(gZl,wf(:,15),wf(:,-2),wf(:,19))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,3),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,21))
  call vert_VV_S(wf(:,10),wf(:,9),wf(:,22))
  call vert_UV_W(wf(:,9),Q(:,9),wf(:,10),Q(:,6),wf(:,23))
  call vert_WQ_A(wf(:,10),wf(:,0),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,7),ZERO,0_intkind1,wf(:,25))
  call vert_ZQ_A(gZn,wf(:,15),wf(:,0),wf(:,26))
  call vert_AW_Q(wf(:,-3),wf(:,10),wf(:,27))
  call prop_Q_A(wf(:,26),Q(:,49),ZERO,0_intkind1,wf(:,28))
  call vert_AW_Q(wf(:,-1),wf(:,9),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,11),ZERO,0_intkind1,wf(:,30))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,15),wf(:,31))
  call vert_WQ_A(wf(:,9),wf(:,-2),wf(:,32))
  call prop_A_Q(wf(:,31),Q(:,50),ZERO,0_intkind1,wf(:,33))
  call vert_ZQ_A(gZn,wf(:,6),wf(:,0),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,13),ZERO,0_intkind1,wf(:,35))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,6),wf(:,36))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,37))
  call vert_QA_Z(gZl,wf(:,18),wf(:,-3),wf(:,38))
  call prop_W_W(wf(:,38),Q(:,15),MZ,1_intkind1,wf(:,39))
  call prop_A_Q(wf(:,20),Q(:,11),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,-2),wf(:,40),wf(:,41))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,40),wf(:,42))
  call prop_W_W(wf(:,42),Q(:,15),MZ,1_intkind1,wf(:,43))
  call prop_W_W(wf(:,23),Q(:,15),MZ,1_intkind1,wf(:,44))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,45))
  call vert_QA_Z(gZl,wf(:,25),wf(:,-3),wf(:,46))
  call prop_W_W(wf(:,46),Q(:,15),MZ,1_intkind1,wf(:,47))
  call prop_A_Q(wf(:,27),Q(:,14),ZERO,0_intkind1,wf(:,48))
  call vert_QA_Z(gZn,wf(:,0),wf(:,48),wf(:,49))
  call prop_W_W(wf(:,49),Q(:,15),MZ,1_intkind1,wf(:,50))
  call vert_QA_V(wf(:,-2),wf(:,30),wf(:,51))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,30),wf(:,52))
  call prop_W_W(wf(:,52),Q(:,15),MZ,1_intkind1,wf(:,53))
  call prop_Q_A(wf(:,32),Q(:,13),ZERO,0_intkind1,wf(:,54))
  call vert_QA_Z(gZn,wf(:,54),wf(:,-1),wf(:,55))
  call prop_W_W(wf(:,55),Q(:,15),MZ,1_intkind1,wf(:,56))
  call vert_QA_Z(gZn,wf(:,35),wf(:,-1),wf(:,57))
  call prop_W_W(wf(:,57),Q(:,15),MZ,1_intkind1,wf(:,58))
  call prop_A_Q(wf(:,36),Q(:,14),ZERO,0_intkind1,wf(:,59))
  call vert_QA_Z(gZn,wf(:,0),wf(:,59),wf(:,60))
  call prop_W_W(wf(:,60),Q(:,15),MZ,1_intkind1,wf(:,61))

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
  den(9) = 1 / (Q(5,48) - MH2)
  den(11) = 1 / (Q(5,48) - MZ2)
  den(12) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,52))
  den(22) = 1 / (Q(5,49))
  den(25) = 1 / (Q(5,11))
  den(28) = 1 / (Q(5,50))
  den(31) = 1 / (Q(5,13))
  den(35) = 1 / (Q(5,15) - MH2)
  den(37) = 1 / (Q(5,15))
  den(39) = 1 / (Q(5,15) - MZ2)
  den(49) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(5)*den(9)
  den(13) = den(1)*den(12)
  den(14) = den(11)*den(13)
  den(16) = den(11)*den(15)
  den(17) = den(1)*den(16)
  den(18) = den(8)*den(9)
  den(19) = den(8)*den(11)
  den(20) = den(7)*den(12)
  den(21) = den(11)*den(20)
  den(23) = den(11)*den(22)
  den(24) = den(7)*den(23)
  den(26) = den(6)*den(25)
  den(27) = den(11)*den(26)
  den(29) = den(11)*den(28)
  den(30) = den(6)*den(29)
  den(32) = den(4)*den(31)
  den(33) = den(11)*den(32)
  den(34) = den(4)*den(23)
  den(36) = den(5)*den(35)
  den(38) = den(13)*den(37)
  den(40) = den(13)*den(39)
  den(41) = den(1)*den(25)
  den(42) = den(37)*den(41)
  den(43) = den(39)*den(41)
  den(44) = den(8)*den(35)
  den(45) = den(8)*den(37)
  den(46) = den(8)*den(39)
  den(47) = den(20)*den(37)
  den(48) = den(20)*den(39)
  den(50) = den(7)*den(49)
  den(51) = den(39)*den(50)
  den(52) = den(26)*den(37)
  den(53) = den(26)*den(39)
  den(54) = den(6)*den(31)
  den(55) = den(39)*den(54)
  den(56) = den(32)*den(39)
  den(57) = den(4)*den(49)
  den(58) = den(39)*den(57)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(14)


  A(1) = cont_VV(wf(:,2),wf(:,4)) * den(3)
  A(2) = cont_VV(wf(:,4),wf(:,6)) * den(5)
  A(3) = cont_VV(wf(:,10),wf(:,11)) * den(8)
  A(4) = cont_SS(wf(:,12),wf(:,13)) * den(10)
  A(5) = cont_QA(wf(:,17),wf(:,18)) * den(14)
  A(6) = cont_QA(wf(:,20),wf(:,21)) * den(17)
  A(7) = cont_SS(wf(:,12),wf(:,22)) * den(18)
  A(8) = cont_VV(wf(:,15),wf(:,23)) * den(19)
  A(9) = cont_QA(wf(:,17),wf(:,25)) * den(21)
  A(10) = cont_QA(wf(:,27),wf(:,28)) * den(24)
  A(11) = cont_QA(wf(:,19),wf(:,30)) * den(27)
  A(12) = cont_QA(wf(:,32),wf(:,33)) * den(30)
  A(13) = cont_QA(wf(:,31),wf(:,35)) * den(33)
  A(14) = cont_QA(wf(:,28),wf(:,36)) * den(34)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(14)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*A(1)*f(1)+2*(-A(5)-A(6)-A(13)-A(14))*f(2)+2*A(2)*f(3)+2*A(8)*f(4)+2*A(7)*f(5)-2*A(3)*f(6)+2*(A(9)+A(10)+A(11) &
       +A(12))*f(7)-2*A(4)*f(8)

end subroutine colourvectors

end module ol_loop_ppllll2_nenexeexgg_1_/**/REALKIND
