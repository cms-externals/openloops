
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
end module ol_forced_parameters_ppllll2_eexmmxgg_1_/**/REALKIND

module ol_loop_ppllll2_eexmmxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(16), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:58)
  ! denominators
  complex(REALKIND), save :: den(55)
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
    f( 1) = CI*countertermnorm*ctAAGG*eQED**4*gQCD**2
    f( 2) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**2
    f( 3) = countertermnorm*ctZGG*eQED**4*gQCD**2
    f( 4) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**2
    f( 5) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*MW)/(cw**2*sw)
    f( 6) = (eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 7) = (2*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = (4*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f(10) = (2*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (8*eQED**4*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f(12) = eQED**4*gQCD**2*integralnorm*SwF
    f(13) = (4*eQED**4*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(14) = 2*eQED**4*gQCD**2*integralnorm*SwF
    f(15) = (eQED**4*gQCD**2*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(16) = (eQED**4*gQCD**2*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,4))
  call prop_W_W(wf(:,4),Q(:,12),MZ,1_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,3),MZ,1_intkind1,wf(:,7))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,7),wf(:,8))
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,9))
  call vert_VV_S(wf(:,7),wf(:,5),wf(:,10))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,11))
  call prop_W_W(wf(:,11),Q(:,48),MZ,1_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,13))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,12),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,7),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZl,wf(:,7),wf(:,-2),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_ZQ_A(gZl,wf(:,12),wf(:,-2),wf(:,18))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,52),ZERO,0_intkind1,wf(:,20))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,7),wf(:,21))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,22))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,12),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,13),ZERO,0_intkind1,wf(:,24))
  call vert_ZQ_A(gZl,wf(:,5),wf(:,0),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,13),ZERO,0_intkind1,wf(:,26))
  call vert_ZQ_A(gZl,wf(:,12),wf(:,0),wf(:,27))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,49),ZERO,0_intkind1,wf(:,29))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,5),wf(:,30))
  call vert_QA_V(wf(:,15),wf(:,-3),wf(:,31))
  call vert_QA_Z(gZl,wf(:,15),wf(:,-3),wf(:,32))
  call prop_W_W(wf(:,32),Q(:,15),MZ,1_intkind1,wf(:,33))
  call vert_QA_V(wf(:,17),wf(:,-3),wf(:,34))
  call vert_QA_Z(gZl,wf(:,17),wf(:,-3),wf(:,35))
  call prop_W_W(wf(:,35),Q(:,15),MZ,1_intkind1,wf(:,36))
  call prop_A_Q(wf(:,19),Q(:,11),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,-2),wf(:,37),wf(:,38))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,37),wf(:,39))
  call prop_W_W(wf(:,39),Q(:,15),MZ,1_intkind1,wf(:,40))
  call prop_A_Q(wf(:,21),Q(:,11),ZERO,0_intkind1,wf(:,41))
  call vert_QA_V(wf(:,-2),wf(:,41),wf(:,42))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,41),wf(:,43))
  call prop_W_W(wf(:,43),Q(:,15),MZ,1_intkind1,wf(:,44))
  call vert_QA_V(wf(:,24),wf(:,-1),wf(:,45))
  call vert_QA_Z(gZl,wf(:,24),wf(:,-1),wf(:,46))
  call prop_W_W(wf(:,46),Q(:,15),MZ,1_intkind1,wf(:,47))
  call vert_QA_V(wf(:,26),wf(:,-1),wf(:,48))
  call vert_QA_Z(gZl,wf(:,26),wf(:,-1),wf(:,49))
  call prop_W_W(wf(:,49),Q(:,15),MZ,1_intkind1,wf(:,50))
  call prop_A_Q(wf(:,28),Q(:,14),ZERO,0_intkind1,wf(:,51))
  call vert_QA_V(wf(:,0),wf(:,51),wf(:,52))
  call prop_A_Q(wf(:,30),Q(:,14),ZERO,0_intkind1,wf(:,53))
  call vert_QA_V(wf(:,0),wf(:,53),wf(:,54))
  call vert_QA_Z(gZl,wf(:,0),wf(:,51),wf(:,55))
  call prop_W_W(wf(:,55),Q(:,15),MZ,1_intkind1,wf(:,56))
  call vert_QA_Z(gZl,wf(:,0),wf(:,53),wf(:,57))
  call prop_W_W(wf(:,57),Q(:,15),MZ,1_intkind1,wf(:,58))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,12) - MZ2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,48) - MH2)
  den(11) = 1 / (Q(5,48) - MZ2)
  den(12) = 1 / (Q(5,7))
  den(17) = 1 / (Q(5,52))
  den(21) = 1 / (Q(5,13))
  den(26) = 1 / (Q(5,49))
  den(30) = 1 / (Q(5,15) - MH2)
  den(32) = 1 / (Q(5,15))
  den(34) = 1 / (Q(5,15) - MZ2)
  den(38) = 1 / (Q(5,11))
  den(49) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(4)*den(6)
  den(10) = den(8)*den(9)
  den(13) = den(1)*den(12)
  den(14) = den(11)*den(13)
  den(15) = den(6)*den(12)
  den(16) = den(11)*den(15)
  den(18) = den(11)*den(17)
  den(19) = den(1)*den(18)
  den(20) = den(6)*den(18)
  den(22) = den(2)*den(21)
  den(23) = den(11)*den(22)
  den(24) = den(4)*den(21)
  den(25) = den(11)*den(24)
  den(27) = den(11)*den(26)
  den(28) = den(2)*den(27)
  den(29) = den(4)*den(27)
  den(31) = den(8)*den(30)
  den(33) = den(13)*den(32)
  den(35) = den(13)*den(34)
  den(36) = den(15)*den(32)
  den(37) = den(15)*den(34)
  den(39) = den(1)*den(38)
  den(40) = den(32)*den(39)
  den(41) = den(34)*den(39)
  den(42) = den(6)*den(38)
  den(43) = den(32)*den(42)
  den(44) = den(34)*den(42)
  den(45) = den(22)*den(32)
  den(46) = den(22)*den(34)
  den(47) = den(24)*den(32)
  den(48) = den(24)*den(34)
  den(50) = den(2)*den(49)
  den(51) = den(32)*den(50)
  den(52) = den(4)*den(49)
  den(53) = den(32)*den(52)
  den(54) = den(34)*den(50)
  den(55) = den(34)*den(52)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(13)


  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(3) = cont_VV(wf(:,2),wf(:,8)) * den(7)
  A(4) = cont_VV(wf(:,5),wf(:,8)) * den(8)
  A(5) = cont_SS(wf(:,9),wf(:,10)) * den(10)
  A(6) = cont_QA(wf(:,14),wf(:,15)) * den(14)
  A(7) = cont_QA(wf(:,14),wf(:,17)) * den(16)
  A(8) = cont_QA(wf(:,19),wf(:,20)) * den(19)
  A(9) = cont_QA(wf(:,20),wf(:,21)) * den(20)
  A(10) = cont_QA(wf(:,23),wf(:,24)) * den(23)
  A(11) = cont_QA(wf(:,23),wf(:,26)) * den(25)
  A(12) = cont_QA(wf(:,28),wf(:,29)) * den(28)
  A(13) = cont_QA(wf(:,29),wf(:,30)) * den(29)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(13)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*A(1)*f(1)+2*(A(2)+A(3))*f(2)+2*(-A(6)-A(7)-A(8)-A(9)-A(10)-A(11)-A(12)-A(13))*f(3)+2*A(4)*f(4)-2*A(5)*f(5)

end subroutine colourvectors

end module ol_loop_ppllll2_eexmmxgg_1_/**/REALKIND
