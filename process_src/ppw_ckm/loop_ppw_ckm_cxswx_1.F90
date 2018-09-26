
module ol_colourmatrix_ppw_ckm_cxswx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(8,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(1,:) = [  3]
  K1(2,:) = [  4]
  K1(3,:) = [ -4]
  K1(4,:) = [  4]
  K1(5,:) = [  0]
  K1(6,:) = [  0]
  K1(7,:) = [  0]
  K1(8,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppw_ckm_cxswx_1_/**/REALKIND



module ol_forced_parameters_ppw_ckm_cxswx_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppw_ckm_cxswx_1_/**/REALKIND

module ol_loop_ppw_ckm_cxswx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(3), c(1)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-3+1:2)
  ! denominators
  complex(REALKIND), save :: den(0)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,12), Mct(1,12), Mcol_loop(1,12)
  ! zero helicity identifier
  logical,           save :: zerohel(12) = .true., zerohel_ct(12) = .true.

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
    f(1) = (CI*eQED*VCKMsc)/(sqrt2*sw)
    f(2) = (CI*countertermnorm*ctVsc*eQED*gQCD**2*VCKMsc)/(sqrt2*sw)
    f(3) = (eQED*gQCD**2*integralnorm*SwB*VCKMsc)/(sqrt2*sw)

  c = [ 4*f(3) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,3)
  integer,           intent(in)  :: H(3)
  integer,           intent(in), optional  :: POLSEL(3)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(2)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rMW, H(3), wf(:,-2), POLSEL(3))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rMW, H(3), wf(:,-2), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call counter_QA_W(wf(:,-1),wf(:,0),wf(:,2))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators

  ! denominators

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(2)

  A(1) = cont_VV(wf(:,-2),wf(:,1))

  A(2) = cont_VV(wf(:,-2),wf(:,2))

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(2)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(1)*f(1)

  M2(1) = A(2)*f(2)

end subroutine colourvectors

end module ol_loop_ppw_ckm_cxswx_1_/**/REALKIND
