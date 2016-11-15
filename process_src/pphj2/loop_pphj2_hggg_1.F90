
module ol_colourmatrix_pphj2_hggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,2), KL(0,2), KL2(2,2), KL2ct(2,2), KL2ct2(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [  7, -2]
  KL2(2,:) = [ -2,  7]
  KL2 = (1._/**/REALKIND / 3) * KL2

  KL2ct(1,:) = [  7, -2]
  KL2ct(2,:) = [ -2,  7]
  KL2ct = (1._/**/REALKIND / 3) * KL2ct

  KL2ct2(1,:) = [  7, -2]
  KL2ct2(2,:) = [ -2,  7]
  KL2ct2 = (1._/**/REALKIND / 3) * KL2ct2

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphj2_hggg_1_/**/REALKIND



module ol_forced_parameters_pphj2_hggg_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphj2_hggg_1_/**/REALKIND

module ol_loop_pphj2_hggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(6), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:6)
  ! denominators
  complex(REALKIND), save :: den(3)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,8)
  ! zero helicity identifier
  logical,           save :: zerohel(8) = .true., zerohel_ct(8) = .true.

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
    f(1) = (countertermnorm*ctHGG*eQED*gQCD**3*MB*YB)/MQ2sum
    f(2) = (CI*eQED*gQCD**3*integralnorm*SwF*YB)/(2._/**/REALKIND*MW*sw)
    f(3) = (eQED*gQCD**3*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(4) = (countertermnorm*ctHGG*eQED*gQCD**3*MT*YT)/MQ2sum
    f(5) = (CI*eQED*gQCD**3*integralnorm*SwF*YT)/(2._/**/REALKIND*MW*sw)
    f(6) = (eQED*gQCD**3*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)


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
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(6)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), POLSEL(1))
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))

  else
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), 0)
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)

  end if

  ! internal WFs
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,1))
  call counter_SG_G(wf(:,0),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,3))
  call counter_SG_G(wf(:,0),wf(:,-2),wf(:,4))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,5))
  call counter_SG_G(wf(:,0),wf(:,-1),wf(:,6))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,6))
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,12))

  ! denominators

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(6)


  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(3) = cont_VV(wf(:,3),wf(:,4)) * den(2)
  A(4) = cont_VV(wf(:,3),wf(:,4)) * den(2)
  A(5) = cont_VV(wf(:,5),wf(:,6)) * den(3)
  A(6) = cont_VV(wf(:,5),wf(:,6)) * den(3)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(6)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = 2*CI*(A(2)-A(4)+A(6))*f(1)+2*CI*(A(1)-A(3)+A(5))*f(4)
  M2(2) = 2*CI*(-A(2)+A(4)-A(6))*f(1)+2*CI*(-A(1)+A(3)-A(5))*f(4)

end subroutine colourvectors

end module ol_loop_pphj2_hggg_1_/**/REALKIND
