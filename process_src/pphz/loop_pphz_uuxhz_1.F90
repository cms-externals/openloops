
module ol_colourmatrix_pphz_uuxhz_1_/**/REALKIND
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

  K1( 1,:) = [  3]
  K1( 2,:) = [  4]
  K1( 3,:) = [ -4]
  K1( 4,:) = [  4]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  0]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [  0]
  K1(11,:) = [  0]
  K1(12,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphz_uuxhz_1_/**/REALKIND



module ol_forced_parameters_pphz_uuxhz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphz_uuxhz_1_/**/REALKIND

module ol_loop_pphz_uuxhz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(3), c(1)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:5)
  ! denominators
  complex(REALKIND), save :: den(2)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,12)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2*lambdaHZZ*MW)/(cw**2*sw)
    f(2) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f(3) = (eQED**2*gQCD**2*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)

  c = [ 4*f(3) ]
  c = (1._/**/REALKIND / 3) * c
end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2)
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(2)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_S(P(:,3), rMH, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))

  ! internal WFs
  call vert_QA_Z(gZu,wf(:,0),wf(:,-1),wf(:,1))
  call vert_SV_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call counter_QA_Z(gZu,wf(:,0),wf(:,-1),wf(:,4))
  call prop_W_W(wf(:,2),Q(:,12),MZ,1_intkind1,wf(:,5))

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

  ! denominators

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(2)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(1)

  A(2) = cont_VV(wf(:,4),wf(:,5)) * den(2)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(2)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = -(A(1)*f(1))

  M2(1) = -(A(2)*f(2))

end subroutine colourvectors

end module ol_loop_pphz_uuxhz_1_/**/REALKIND
