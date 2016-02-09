
module ol_colourmatrix_ppllaj2_eexuuxag_1_/**/REALKIND
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
end module ol_colourmatrix_ppllaj2_eexuuxag_1_/**/REALKIND



module ol_forced_parameters_ppllaj2_eexuuxag_1_/**/REALKIND
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
end module ol_forced_parameters_ppllaj2_eexuuxag_1_/**/REALKIND

module ol_loop_ppllaj2_eexuuxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(13), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:24)
  ! denominators
  complex(REALKIND), save :: den(24)
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
    f( 1) = CI*countertermnorm*ctAAGG*eQED**3*gQCD**3
    f( 2) = CI*countertermnorm*ctAZGG*eQED**3*gQCD**3
    f( 3) = (2*countertermnorm*ctZGG*eQED**3*gQCD**3)/3._/**/REALKIND
    f( 4) = countertermnorm*ctZGG*eQED**3*gQCD**3
    f( 5) = (eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 6) = (2*eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 7) = (eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 8) = (4*eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 9) = (2*eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (8*eQED**3*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(11) = eQED**3*gQCD**3*integralnorm*SwF
    f(12) = (4*eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(13) = 2*eQED**3*gQCD**3*integralnorm*SwF


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
  complex(REALKIND) :: A(6)
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
  call counter_VVG_G(wf(:,1),wf(:,-4),wf(:,-5),wf(:,3))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,4))
  call prop_W_W(wf(:,4),Q(:,3),MZ,1_intkind1,wf(:,5))
  call counter_VVG_G(wf(:,5),wf(:,-4),wf(:,-5),wf(:,6))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,7))
  call prop_Q_A(wf(:,7),Q(:,20),ZERO,0_intkind1,wf(:,8))
  call vert_QA_V(wf(:,8),wf(:,-3),wf(:,9))
  call counter_VG_G(wf(:,5),wf(:,-5),Q(:,32),wf(:,10),Q(:,35))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,11))
  call prop_A_Q(wf(:,11),Q(:,24),ZERO,0_intkind1,wf(:,12))
  call vert_QA_V(wf(:,-2),wf(:,12),wf(:,13))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,14))
  call prop_Q_A(wf(:,14),Q(:,17),ZERO,0_intkind1,wf(:,15))
  call vert_QA_Z(gZl,wf(:,15),wf(:,-1),wf(:,16))
  call counter_GG_V(wf(:,2),Q(:,12),wf(:,-5),Q(:,32),wf(:,17))
  call prop_W_W(wf(:,16),Q(:,19),MZ,1_intkind1,wf(:,18))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,19))
  call prop_A_Q(wf(:,19),Q(:,18),ZERO,0_intkind1,wf(:,20))
  call vert_QA_Z(gZl,wf(:,0),wf(:,20),wf(:,21))
  call prop_W_W(wf(:,21),Q(:,19),MZ,1_intkind1,wf(:,22))
  call vert_QA_V(wf(:,15),wf(:,-1),wf(:,23))
  call vert_QA_V(wf(:,0),wf(:,20),wf(:,24))

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
  den(4) = 1 / (Q(5,3) - MZ2)
  den(6) = 1 / (Q(5,20))
  den(7) = 1 / (Q(5,28))
  den(10) = 1 / (Q(5,24))
  den(13) = 1 / (Q(5,17))
  den(14) = 1 / (Q(5,19) - MZ2)
  den(17) = 1 / (Q(5,18))
  den(20) = 1 / (Q(5,19))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(4)*den(8)
  den(11) = den(7)*den(10)
  den(12) = den(4)*den(11)
  den(15) = den(13)*den(14)
  den(16) = den(2)*den(15)
  den(18) = den(14)*den(17)
  den(19) = den(2)*den(18)
  den(21) = den(13)*den(20)
  den(22) = den(17)*den(20)
  den(23) = den(2)*den(21)
  den(24) = den(2)*den(22)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(6)


  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,2),wf(:,6)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(4) = cont_VV(wf(:,10),wf(:,13)) * den(12)
  A(5) = cont_VV(wf(:,17),wf(:,18)) * den(16)
  A(6) = cont_VV(wf(:,17),wf(:,22)) * den(19)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(6)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = -(A(1)*f(1))-A(2)*f(2)+(-A(3)-A(4))*f(3)+(A(5)+A(6))*f(4)

end subroutine colourvectors

end module ol_loop_ppllaj2_eexuuxag_1_/**/REALKIND
