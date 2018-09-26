
module ol_colourmatrix_eevjj_eexudxw_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(17,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  3]
  K1( 2,:) = [  0]
  K1( 3,:) = [  0]
  K1( 4,:) = [  0]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  4]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [ -4]
  K1(11,:) = [  4]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  0]
  K1(17,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_eevjj_eexudxw_1_/**/REALKIND



module ol_forced_parameters_eevjj_eexudxw_1_/**/REALKIND
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
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_eevjj_eexudxw_1_/**/REALKIND

module ol_loop_eevjj_eexudxw_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(18), c(5)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:37)
  ! denominators
  complex(REALKIND), save :: den(27)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,48)
  ! zero helicity identifier
  logical,           save :: zerohel(48) = .true., zerohel_ct(48) = .true.

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
    f( 1) = (CI*eQED**3*sqrt2)/(4._/**/REALKIND*sw**3)
    f( 2) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*sqrt2)/(4._/**/REALKIND*sw**3)
    f( 3) = (CI*cw*eQED**3)/(sqrt2*sw**2)
    f( 4) = (CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/(sqrt2*sw**2)
    f( 5) = (CI*eQED**3)/(3._/**/REALKIND*sqrt2*sw)
    f( 6) = (2*CI*eQED**3)/(3._/**/REALKIND*sqrt2*sw)
    f( 7) = (CI*eQED**3)/(sqrt2*sw)
    f( 8) = (CI*countertermnorm*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f( 9) = (2*CI*countertermnorm*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(10) = (CI*countertermnorm*eQED**3*gQCD**2)/(sqrt2*sw)
    f(11) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(12) = (2*CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(13) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(sqrt2*sw)
    f(14) = (eQED**3*gQCD**2*integralnorm*sqrt2*SwB)/(sw**3*4._/**/REALKIND)
    f(15) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw**2)
    f(16) = (eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(17) = (2*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(18) = (eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw)

  c = [ 4*f(14), 4*f(15), 4*f(16), 4*f(17), 4*f(18) ]
  c = (1._/**/REALKIND / 3) * c
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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  integer,           intent(in), optional  :: POLSEL(5)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(22)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_W(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,2),Q(:,12),MW,1_intkind1,wf(:,3))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,1),Q(:,3),wf(:,4))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,3),MZ,1_intkind1,wf(:,6))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,6),Q(:,3),wf(:,7))
  call vert_WQ_A(wf(:,-4),wf(:,-2),wf(:,8))
  call prop_Q_A(wf(:,8),Q(:,20),ZERO,0_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,6),wf(:,11))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,24),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,14))
  call vert_ZQ_A(gZu,wf(:,6),wf(:,-2),wf(:,15))
  call vert_AW_Q(wf(:,-1),wf(:,-4),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,18),ZERO,0_intkind1,wf(:,17))
  call vert_QA_W(wf(:,0),wf(:,17),wf(:,18))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,19))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,6),wf(:,20))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,21))
  call prop_A_Q(wf(:,21),Q(:,24),ZERO,0_intkind1,wf(:,22))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,23))
  call counter_ZQ_A(gZu,wf(:,6),wf(:,-2),wf(:,24))
  call counter_WQ_A(wf(:,-4),wf(:,-2),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,20),ZERO,0_intkind1,wf(:,26))
  call counter_QA_W(wf(:,-2),wf(:,-3),wf(:,27))
  call prop_W_W(wf(:,27),Q(:,12),MW,1_intkind1,wf(:,28))
  call counter_Q_A(ctqq,wf(:,9),Q(:,20),wf(:,29))
  call prop_A_Q(wf(:,10),Q(:,11),ZERO,0_intkind1,wf(:,30))
  call prop_A_Q(wf(:,11),Q(:,11),ZERO,0_intkind1,wf(:,31))
  call counter_A_Q(ctqq,wf(:,13),Q(:,24),wf(:,32))
  call prop_Q_A(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,33))
  call prop_Q_A(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,34))
  call prop_W_W(wf(:,4),Q(:,19),MW,1_intkind1,wf(:,35))
  call prop_W_W(wf(:,7),Q(:,19),MW,1_intkind1,wf(:,36))
  call prop_W_W(wf(:,18),Q(:,19),MW,1_intkind1,wf(:,37))

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
  den(2) = 1 / (Q(5,12) - MW2)
  den(4) = 1 / (Q(5,3) - MZ2)
  den(6) = 1 / (Q(5,20))
  den(9) = 1 / (Q(5,24))
  den(12) = 1 / (Q(5,18))
  den(14) = 1 / (Q(5,11))
  den(19) = 1 / (Q(5,7))
  den(24) = 1 / (Q(5,19) - MW2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(4)*den(6)
  den(10) = den(1)*den(9)
  den(11) = den(4)*den(9)
  den(13) = den(2)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(6)*den(15)
  den(17) = den(4)*den(14)
  den(18) = den(6)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(9)*den(20)
  den(22) = den(4)*den(19)
  den(23) = den(9)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(4)*den(24)
  den(27) = den(12)*den(24)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(22)

  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(3)
  A(2) = cont_VV(wf(:,3),wf(:,7)) * den(5)
  A(3) = cont_QA(wf(:,9),wf(:,10)) * den(7)
  A(4) = cont_QA(wf(:,9),wf(:,11)) * den(8)
  A(5) = cont_QA(wf(:,13),wf(:,14)) * den(10)
  A(6) = cont_QA(wf(:,13),wf(:,15)) * den(11)
  A(7) = cont_VV(wf(:,3),wf(:,18)) * den(13)

  A(8) = cont_QA(wf(:,9),wf(:,19)) * den(7)
  A(9) = cont_QA(wf(:,9),wf(:,20)) * den(8)
  A(10) = cont_QA(wf(:,14),wf(:,22)) * den(10)
  A(11) = cont_QA(wf(:,15),wf(:,22)) * den(11)
  A(12) = cont_QA(wf(:,13),wf(:,23)) * den(10)
  A(13) = cont_QA(wf(:,13),wf(:,24)) * den(11)
  A(14) = cont_QA(wf(:,10),wf(:,26)) * den(7)
  A(15) = cont_QA(wf(:,11),wf(:,26)) * den(8)
  A(16) = cont_VV(wf(:,4),wf(:,28)) * den(3)
  A(17) = cont_VV(wf(:,7),wf(:,28)) * den(5)
  A(18) = cont_VV(wf(:,18),wf(:,28)) * den(13)
  A(19) = cont_QA(wf(:,29),wf(:,30)) * den(16)
  A(20) = cont_QA(wf(:,29),wf(:,31)) * den(18)
  A(21) = cont_QA(wf(:,32),wf(:,33)) * den(21)
  A(22) = cont_QA(wf(:,32),wf(:,34)) * den(23)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(22)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = -(A(7)*f(1))-A(2)*f(3)-A(3)*f(5)+A(5)*f(6)+(A(1)-A(4)-A(6))*f(7)

  M2(1) = -(A(18)*f(2))-A(17)*f(4)+A(19)*f(8)-A(21)*f(9)+(A(20)+A(22))*f(10)+(-A(8)-A(14))*f(11)+(A(10)+A(12))*f(12)+(-A(9)-A(11) &
       -A(13)-A(15)+A(16))*f(13)

end subroutine colourvectors

end module ol_loop_eevjj_eexudxw_1_/**/REALKIND
