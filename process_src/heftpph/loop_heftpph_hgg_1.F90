
module ol_colourmatrix_heftpph_hgg_1_/**/REALKIND
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

  K1(1,:) = [  2]
  K1(2,:) = [  0]
  K1(3,:) = [  0]
  K1(4,:) = [  6]
  K1(5,:) = [  0]
  K1(6,:) = [ -6]
  K1(7,:) = [  6]
  K1(8,:) = [  0]

  K2(1,:) = [ 2]

  KL(1,:) = [ 2]

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpph_hgg_1_/**/REALKIND



module ol_forced_parameters_heftpph_hgg_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpph_hgg_1_/**/REALKIND

module ol_loop_heftpph_hgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(5), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-3+1:2)
  ! denominators
  complex(REALKIND), save :: den(0)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,4)
  ! zero helicity identifier
  logical,           save :: zerohel(4) = .true., zerohel_ct(4) = .true.

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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED*gQCD**2)/(24._/**/REALKIND*MW*pi**2*sw)
    f(2) = (CI*countertermnorm*eQED*gQCD**4)/(24._/**/REALKIND*MW*pi**2*sw)
    f(3) = (eQED*gQCD**4*integralnorm*SwB)/(MW*pi**2*sw*48._/**/REALKIND)
    f(4) = (eQED*gQCD**4*integralnorm*SwB)/(MW*pi**2*sw*24._/**/REALKIND)
    f(5) = (eQED*gQCD**2*integralnorm*MB*SwF)/(MW*sw*2._/**/REALKIND)

  c = [ 6*f(3), 6*f(4), f(5) ]
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
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), POLSEL(1))
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))

  else
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), 0)
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)

  end if

  ! internal WFs
  call vert_HG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,1),Q(:,3))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-1),Q(:,2),wf(:,2),Q(:,3))

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

  M1(1) = 2*A(1)*f(1)

  M2(1) = 2*A(2)*f(2)

end subroutine colourvectors

end module ol_loop_heftpph_hgg_1_/**/REALKIND
