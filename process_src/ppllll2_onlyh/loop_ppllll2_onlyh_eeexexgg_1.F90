
module ol_colourmatrix_ppllll2_onlyh_eeexexgg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll2_onlyh_eeexexgg_1_/**/REALKIND



module ol_forced_parameters_ppllll2_onlyh_eeexexgg_1_/**/REALKIND
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
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllll2_onlyh_eeexexgg_1_/**/REALKIND

module ol_loop_ppllll2_onlyh_eeexexgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(4), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:11)
  ! denominators
  complex(REALKIND), save :: den(12)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(2) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(3) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**2*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(4) = (eQED**4*gQCD**2*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)


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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(0), M2(1)
  complex(REALKIND) :: A(4)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-3),wf(:,2))
  call counter_GG_S(wf(:,-4),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MZ,1_intkind1,wf(:,5))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,6))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-3),wf(:,7))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,8))
  call prop_W_W(wf(:,7),Q(:,9),MZ,1_intkind1,wf(:,9))
  call prop_W_W(wf(:,8),Q(:,6),MZ,1_intkind1,wf(:,10))
  call vert_VV_S(wf(:,10),wf(:,9),wf(:,11))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MZ2)
  den(2) = 1 / (Q(5,10) - MZ2)
  den(3) = 1 / (Q(5,48) - MH2)
  den(6) = 1 / (Q(5,9) - MZ2)
  den(7) = 1 / (Q(5,6) - MZ2)
  den(10) = 1 / (Q(5,15) - MH2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(3)*den(8)
  den(11) = den(4)*den(10)
  den(12) = den(8)*den(10)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(4)


  A(1) = cont_SS(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_SS(wf(:,3),wf(:,6)) * den(5)
  A(3) = cont_SS(wf(:,3),wf(:,11)) * den(9)
  A(4) = cont_SS(wf(:,3),wf(:,11)) * den(9)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(4)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = 2*(A(2)-A(4))*f(1)+2*(A(1)-A(3))*f(3)

end subroutine colourvectors

end module ol_loop_ppllll2_onlyh_eeexexgg_1_/**/REALKIND
