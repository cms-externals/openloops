
module ol_colourmatrix_pptj_utxdxb_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(12,1), K2(1,1), KL(1,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9]
  K1( 2,:) = [  12]
  K1( 3,:) = [   0]
  K1( 4,:) = [  12]
  K1( 5,:) = [ -12]
  K1( 6,:) = [   0]
  K1( 7,:) = [  12]
  K1( 8,:) = [   0]
  K1( 9,:) = [ -12]
  K1(10,:) = [   0]
  K1(11,:) = [  12]
  K1(12,:) = [   0]

  K2(1,:) = [ 9]

  KL(1,:) = [ 9, 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pptj_utxdxb_1_/**/REALKIND



module ol_forced_parameters_pptj_utxdxb_1_/**/REALKIND
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
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pptj_utxdxb_1_/**/REALKIND

module ol_loop_pptj_utxdxb_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(5), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:6)
  ! denominators
  complex(REALKIND), save :: den(2)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,16)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2)/(2._/**/REALKIND*sw**2)
    f(2) = (CI*countertermnorm*ctVbt*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(3) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(4) = (CI*countertermnorm*ctSqq*eQED**2*gQCD**2)/(2._/**/REALKIND*MW**2*sw**2)
    f(5) = (eQED**2*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)

  c = [ f(5), 3*f(5), 8*f(5) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND) :: A(4)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rMT, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rMB, H(4), wf(:,-3))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_W(wf(:,-3),wf(:,-1),wf(:,2))
  call counter_QA_W(wf(:,-3),wf(:,-1),wf(:,3))
  call vert_AQ_S(gPtb,wf(:,-1),wf(:,-3),wf(:,4))
  call counter_AQ_S(gPdu,wf(:,-2),wf(:,0),wf(:,5))
  call counter_QA_W(wf(:,0),wf(:,-2),wf(:,6))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MW2)
  den(2) = 1 / (Q(5,10) - MW2)

  ! denominators

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(4)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)

  A(2) = cont_VV(wf(:,1),wf(:,3)) * den(1)
  A(3) = cont_SS(wf(:,4),wf(:,5)) * den(2)
  A(4) = cont_VV(wf(:,2),wf(:,6)) * den(2)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(4)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(1)*f(1)

  M2(1) = A(2)*f(2)+A(4)*f(3)+A(3)*f(4)

end subroutine colourvectors

end module ol_loop_pptj_utxdxb_1_/**/REALKIND
