
module ol_colourmatrix_ppllllj2_nenexnmnmxuuxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nenexnmnmxuuxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nenexnmnmxuuxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_nenexnmnmxuuxg_1_/**/REALKIND

module ol_loop_ppllllj2_nenexnmnmxuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:40)
  ! denominators
  complex(REALKIND), save :: den(48)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB, DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f(2) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f(3) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f(4) = eQED**4*gQCD**3*integralnorm*SwF
    f(5) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(6) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(7) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND) :: A(10)
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
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-2),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,12),MZ,1_intkind1,wf(:,5))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-6),wf(:,6))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,7))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,8))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-4),wf(:,9))
  call counter_VG_G(wf(:,5),wf(:,-6),Q(:,64),wf(:,10),Q(:,76))
  call prop_Q_A(wf(:,9),Q(:,19),ZERO,0_intkind1,wf(:,11))
  call vert_QA_V(wf(:,11),wf(:,-5),wf(:,12))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,4),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,35),ZERO,0_intkind1,wf(:,14))
  call vert_QA_V(wf(:,-4),wf(:,14),wf(:,15))
  call vert_ZQ_A(gZu,wf(:,5),wf(:,-4),wf(:,16))
  call counter_VG_G(wf(:,4),wf(:,-6),Q(:,64),wf(:,17),Q(:,67))
  call prop_Q_A(wf(:,16),Q(:,28),ZERO,0_intkind1,wf(:,18))
  call vert_QA_V(wf(:,18),wf(:,-5),wf(:,19))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,5),wf(:,20))
  call prop_A_Q(wf(:,20),Q(:,44),ZERO,0_intkind1,wf(:,21))
  call vert_QA_V(wf(:,-4),wf(:,21),wf(:,22))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,-2),wf(:,23))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,7),ZERO,0_intkind1,wf(:,25))
  call prop_W_W(wf(:,24),Q(:,112),MZ,1_intkind1,wf(:,26))
  call vert_QA_Z(gZn,wf(:,25),wf(:,-3),wf(:,27))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,4),wf(:,28))
  call prop_A_Q(wf(:,28),Q(:,11),ZERO,0_intkind1,wf(:,29))
  call vert_QA_Z(gZn,wf(:,-2),wf(:,29),wf(:,30))
  call vert_ZQ_A(gZn,wf(:,5),wf(:,0),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,13),ZERO,0_intkind1,wf(:,32))
  call vert_QA_Z(gZn,wf(:,32),wf(:,-1),wf(:,33))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,5),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,14),ZERO,0_intkind1,wf(:,35))
  call vert_QA_Z(gZn,wf(:,0),wf(:,35),wf(:,36))
  call prop_W_W(wf(:,27),Q(:,15),MZ,1_intkind1,wf(:,37))
  call prop_W_W(wf(:,30),Q(:,15),MZ,1_intkind1,wf(:,38))
  call prop_W_W(wf(:,33),Q(:,15),MZ,1_intkind1,wf(:,39))
  call prop_W_W(wf(:,36),Q(:,15),MZ,1_intkind1,wf(:,40))

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
  den(6) = 1 / (Q(5,15) - MH2)
  den(9) = 1 / (Q(5,19))
  den(11) = 1 / (Q(5,76))
  den(14) = 1 / (Q(5,35))
  den(17) = 1 / (Q(5,28))
  den(19) = 1 / (Q(5,67))
  den(22) = 1 / (Q(5,44))
  den(25) = 1 / (Q(5,7))
  den(27) = 1 / (Q(5,112) - MZ2)
  den(30) = 1 / (Q(5,11))
  den(33) = 1 / (Q(5,13))
  den(36) = 1 / (Q(5,14))
  den(39) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(4)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(12) = den(2)*den(11)
  den(13) = den(10)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(12)*den(15)
  den(18) = den(2)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(18)*den(20)
  den(23) = den(2)*den(22)
  den(24) = den(20)*den(23)
  den(26) = den(1)*den(25)
  den(28) = den(3)*den(27)
  den(29) = den(26)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(28)*den(31)
  den(34) = den(2)*den(33)
  den(35) = den(28)*den(34)
  den(37) = den(2)*den(36)
  den(38) = den(28)*den(37)
  den(40) = den(26)*den(39)
  den(41) = den(31)*den(39)
  den(42) = den(34)*den(39)
  den(43) = den(37)*den(39)
  den(44) = den(1)*den(2)*den(3)
  den(45) = den(3)*den(40)
  den(46) = den(3)*den(41)
  den(47) = den(3)*den(42)
  den(48) = den(3)*den(43)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(10)


  A(1) = cont_VV(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_SS(wf(:,7),wf(:,8)) * den(8)
  A(3) = cont_VV(wf(:,10),wf(:,12)) * den(13)
  A(4) = cont_VV(wf(:,10),wf(:,15)) * den(16)
  A(5) = cont_VV(wf(:,17),wf(:,19)) * den(21)
  A(6) = cont_VV(wf(:,17),wf(:,22)) * den(24)
  A(7) = cont_VV(wf(:,26),wf(:,27)) * den(29)
  A(8) = cont_VV(wf(:,26),wf(:,30)) * den(32)
  A(9) = cont_VV(wf(:,26),wf(:,33)) * den(35)
  A(10) = cont_VV(wf(:,26),wf(:,36)) * den(38)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(10)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = (-A(3)-A(4)-A(5)-A(6)-A(7)-A(8)-A(9)-A(10))*f(1)+A(1)*f(2)-A(2)*f(3)

end subroutine colourvectors

end module ol_loop_ppllllj2_nenexnmnmxuuxg_1_/**/REALKIND
