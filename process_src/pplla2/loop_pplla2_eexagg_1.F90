
module ol_colourmatrix_pplla2_eexagg_1_/**/REALKIND
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
end module ol_colourmatrix_pplla2_eexagg_1_/**/REALKIND



module ol_forced_parameters_pplla2_eexagg_1_/**/REALKIND
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
  if (YE /= 0) write(*,101) 'YE = 0'
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
end module ol_forced_parameters_pplla2_eexagg_1_/**/REALKIND

module ol_loop_pplla2_eexagg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:16)
  ! denominators
  complex(REALKIND), save :: den(13)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,32)
  ! zero helicity identifier
  logical,           save :: zerohel(32) = .true., zerohel_ct(32) = .true.

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
    f( 1) = CI*countertermnorm*ctAAGG*eQED**3*gQCD**2
    f( 2) = CI*countertermnorm*ctAZGG*eQED**3*gQCD**2
    f( 3) = countertermnorm*ctZGG*eQED**3*gQCD**2
    f( 4) = (eQED**3*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 5) = (2*eQED**3*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 6) = (eQED**3*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 7) = (4*eQED**3*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (2*eQED**3*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = (8*eQED**3*gQCD**2*integralnorm*SwF)/9._/**/REALKIND
    f(10) = eQED**3*gQCD**2*integralnorm*SwF
    f(11) = (4*eQED**3*gQCD**2*integralnorm*SwF)/3._/**/REALKIND
    f(12) = 2*eQED**3*gQCD**2*integralnorm*SwF


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
  complex(REALKIND), intent(out) :: M1(0), M2(1)
  complex(REALKIND) :: A(4)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call counter_VVG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,2))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,3))
  call prop_W_W(wf(:,3),Q(:,3),MZ,1_intkind1,wf(:,4))
  call vert_VQ_A(wf(:,-2),wf(:,0),wf(:,5))
  call counter_GG_V(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,6))
  call prop_Q_A(wf(:,5),Q(:,5),ZERO,0_intkind1,wf(:,7))
  call prop_W_W(wf(:,6),Q(:,24),MZ,1_intkind1,wf(:,8))
  call vert_QA_Z(gZl,wf(:,7),wf(:,-1),wf(:,9))
  call vert_AV_Q(wf(:,-1),wf(:,-2),wf(:,10))
  call prop_A_Q(wf(:,10),Q(:,6),ZERO,0_intkind1,wf(:,11))
  call vert_QA_Z(gZl,wf(:,0),wf(:,11),wf(:,12))
  call vert_QA_V(wf(:,7),wf(:,-1),wf(:,13))
  call prop_W_W(wf(:,9),Q(:,7),MZ,1_intkind1,wf(:,14))
  call vert_QA_V(wf(:,0),wf(:,11),wf(:,15))
  call prop_W_W(wf(:,12),Q(:,7),MZ,1_intkind1,wf(:,16))

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
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,5))
  den(4) = 1 / (Q(5,24) - MZ2)
  den(6) = 1 / (Q(5,6))
  den(8) = 1 / (Q(5,7))
  den(10) = 1 / (Q(5,7) - MZ2)

  ! denominators
  den(5) = den(3)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(3)*den(8)
  den(11) = den(3)*den(10)
  den(12) = den(6)*den(8)
  den(13) = den(6)*den(10)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(4)


  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,2),wf(:,4)) * den(2)
  A(3) = cont_VV(wf(:,8),wf(:,9)) * den(5)
  A(4) = cont_VV(wf(:,8),wf(:,12)) * den(7)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(4)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = -2*A(1)*f(1)-2*A(2)*f(2)+2*(A(3)+A(4))*f(3)

end subroutine colourvectors

end module ol_loop_pplla2_eexagg_1_/**/REALKIND
