
module ol_colourmatrix_ppwj_ckm_ubxwg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(12,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12]
  K1( 2,:) = [  16]
  K1( 3,:) = [   2]
  K1( 4,:) = [  16]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [   0]
  K1( 8,:) = [ -18]
  K1( 9,:) = [ -18]
  K1(10,:) = [   0]
  K1(11,:) = [  36]
  K1(12,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppwj_ckm_ubxwg_1_/**/REALKIND



module ol_forced_parameters_ppwj_ckm_ubxwg_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwj_ckm_ubxwg_1_/**/REALKIND

module ol_loop_ppwj_ckm_ubxwg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:14)
  ! denominators
  complex(REALKIND), save :: den(6)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,24), Mct(1,24), Mcol_loop(1,24)
  ! zero helicity identifier
  logical,           save :: zerohel(24) = .true., zerohel_ct(24) = .true.

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
    f(1) = (CI*eQED*gQCD*VCKMbu)/(sqrt2*sw)
    f(2) = (CI*countertermnorm*eQED*gQCD**3*VCKMbu)/(sqrt2*sw)
    f(3) = (CI*countertermnorm*ctGbb*eQED*gQCD**3*VCKMbu)/(sqrt2*sw)
    f(4) = (CI*countertermnorm*ctGqq*eQED*gQCD**3*VCKMbu)/(sqrt2*sw)
    f(5) = (CI*countertermnorm*ctVbu*eQED*gQCD**3*VCKMbu)/(sqrt2*sw)
    f(6) = (CI*eQED*gQCD**3*integralnorm*SwB*VCKMbu)/(sqrt2*sw)
    f(7) = (eQED*gQCD**3*integralnorm*SwB*VCKMbu)/(sqrt2*sw)

  c = [ 9*CI*f(6), f(7), 8*f(7) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,4)
  integer,           intent(in)  :: H(4)
  integer,           intent(in), optional  :: POLSEL(4)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(8)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rMB, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rMW, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rMB, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rMW, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)

  end if

  ! internal WFs
  call vert_WQ_A(wf(:,-2),wf(:,0),wf(:,1))
  call vert_AV_Q(wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),MB,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,-3),wf(:,0),wf(:,4))
  call vert_AW_Q(wf(:,-1),wf(:,-2),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,9),ZERO,0_intkind1,wf(:,6))
  call counter_AV_Q(wf(:,-1),wf(:,-3),wf(:,7))
  call counter_AW_Q(wf(:,-1),wf(:,-2),wf(:,8))
  call counter_VQ_A(wf(:,-3),wf(:,0),wf(:,9))
  call prop_A_Q(wf(:,5),Q(:,6),ZERO,0_intkind1,wf(:,10))
  call counter_WQ_A(wf(:,-2),wf(:,0),wf(:,11))
  call prop_A_Q(wf(:,2),Q(:,10),MB,1_intkind1,wf(:,12))
  call counter_Q_A(ctbb,wf(:,3),Q(:,5),wf(:,13))
  call counter_Q_A(ctqq,wf(:,6),Q(:,9),wf(:,14))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MB2)
  den(2) = 1 / (Q(5,9))
  den(3) = 1 / (Q(5,6))
  den(4) = 1 / (Q(5,10) - MB2)

  ! denominators
  den(5) = den(1)*den(4)
  den(6) = den(2)*den(3)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(8)

  A(1) = cont_QA(wf(:,2),wf(:,3)) * den(1)
  A(2) = cont_QA(wf(:,5),wf(:,6)) * den(2)

  A(3) = cont_QA(wf(:,3),wf(:,7)) * den(1)
  A(4) = cont_QA(wf(:,6),wf(:,8)) * den(2)
  A(5) = cont_QA(wf(:,9),wf(:,10)) * den(3)
  A(6) = cont_QA(wf(:,11),wf(:,12)) * den(4)
  A(7) = cont_QA(wf(:,12),wf(:,13)) * den(5)
  A(8) = cont_QA(wf(:,10),wf(:,14)) * den(6)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(8)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(2))*f(1)

  M2(1) = (A(7)+A(8))*f(2)-A(3)*f(3)-A(5)*f(4)+(-A(4)-A(6))*f(5)

end subroutine colourvectors

end module ol_loop_ppwj_ckm_ubxwg_1_/**/REALKIND
