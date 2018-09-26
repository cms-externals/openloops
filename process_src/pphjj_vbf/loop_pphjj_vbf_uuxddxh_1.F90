
module ol_colourmatrix_pphjj_vbf_uuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(34,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphjj_vbf_uuxddxh_1_/**/REALKIND



module ol_forced_parameters_pphjj_vbf_uuxddxh_1_/**/REALKIND
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
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphjj_vbf_uuxddxh_1_/**/REALKIND

module ol_loop_pphjj_vbf_uuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(6)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:28)
  ! denominators
  complex(REALKIND), save :: den(24)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16), Mct(2,16), Mcol_loop(2,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

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
    f(1) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**2)/(4._/**/REALKIND*MW*sw**3)
    f(2) = (CI*eQED**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(3) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(4) = (CI*eQED**3*lambdaHZZ*MW)/(cw**2*sw)
    f(5) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f(6) = (eQED**3*gQCD**2*integralnorm*lambdaHWW*MW*SwB)/(sw**3*2._/**/REALKIND)
    f(7) = (eQED**3*gQCD**2*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)

  c = [ f(6), 3*f(6), 8*f(6), f(7), 3*f(7), 8*f(7) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(8)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZu,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,2))
  call vert_SV_V(wf(:,-4),wf(:,1),wf(:,3))
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,4))
  call vert_QA_W(wf(:,-2),wf(:,-1),wf(:,5))
  call vert_SV_V(wf(:,-4),wf(:,4),wf(:,6))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,7))
  call counter_AQ_S(gPud,wf(:,-1),wf(:,-2),wf(:,8))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,4),Q(:,9),wf(:,9))
  call counter_QA_W(wf(:,-2),wf(:,-1),wf(:,10))
  call counter_AQ_S(gPdu,wf(:,-3),wf(:,0),wf(:,11))
  call vert_ST_V(wf(:,11),Q(:,9),wf(:,-4),Q(:,16),wf(:,12))
  call counter_QA_W(wf(:,0),wf(:,-3),wf(:,13))
  call vert_SV_V(wf(:,-4),wf(:,13),wf(:,14))
  call counter_QA_Z(gZu,wf(:,0),wf(:,-1),wf(:,15))
  call vert_SV_V(wf(:,-4),wf(:,15),wf(:,16))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,17))
  call vert_VQ_A(wf(:,17),wf(:,-2),wf(:,18))
  call prop_Q_A(wf(:,18),Q(:,7),ZERO,0_intkind1,wf(:,19))
  call vert_AV_Q(wf(:,-3),wf(:,17),wf(:,20))
  call prop_A_Q(wf(:,20),Q(:,11),ZERO,0_intkind1,wf(:,21))
  call vert_SV_V(wf(:,-4),wf(:,5),wf(:,22))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,23))
  call vert_VQ_A(wf(:,23),wf(:,0),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,13),ZERO,0_intkind1,wf(:,25))
  call vert_AV_Q(wf(:,-1),wf(:,23),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,14),ZERO,0_intkind1,wf(:,27))
  call vert_SV_V(wf(:,-4),wf(:,2),wf(:,28))

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
  den(4) = 1 / (Q(5,9) - MW2)
  den(5) = 1 / (Q(5,6) - MW2)
  den(7) = 1 / (Q(5,3))
  den(8) = 1 / (Q(5,7))
  den(10) = 1 / (Q(5,11))
  den(12) = 1 / (Q(5,19) - MZ2)
  den(14) = 1 / (Q(5,22) - MW2)
  den(16) = 1 / (Q(5,25) - MW2)
  den(18) = 1 / (Q(5,12))
  den(19) = 1 / (Q(5,13))
  den(21) = 1 / (Q(5,14))
  den(23) = 1 / (Q(5,28) - MZ2)

  ! denominators
  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(9) = den(7)*den(8)
  den(11) = den(7)*den(10)
  den(13) = den(1)*den(12)
  den(15) = den(5)*den(14)
  den(17) = den(4)*den(16)
  den(20) = den(18)*den(19)
  den(22) = den(18)*den(21)
  den(24) = den(2)*den(23)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(8)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,5),wf(:,6)) * den(6)

  A(3) = cont_VV(wf(:,3),wf(:,7)) * den(3)
  A(4) = cont_SS(wf(:,8),wf(:,9)) * den(6)
  A(5) = cont_VV(wf(:,6),wf(:,10)) * den(6)
  A(6) = cont_VV(wf(:,5),wf(:,12)) * den(6)
  A(7) = cont_VV(wf(:,5),wf(:,14)) * den(6)
  A(8) = cont_VV(wf(:,2),wf(:,16)) * den(3)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(8)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = -(A(2)*f(2))
  M1(2) = A(1)*f(4)

  M2(1) = (-A(4)-A(6))*f(1)+(-A(5)-A(7))*f(3)
  M2(2) = (A(3)+A(8))*f(5)

end subroutine colourvectors

end module ol_loop_pphjj_vbf_uuxddxh_1_/**/REALKIND
