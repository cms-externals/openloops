
module ol_colourmatrix_ppww_bbxwwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppww_bbxwwx_1_/**/REALKIND



module ol_forced_parameters_ppww_bbxwwx_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppww_bbxwwx_1_/**/REALKIND

module ol_loop_ppww_bbxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(13), c(4)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:17)
  ! denominators
  complex(REALKIND), save :: den(9)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,36)
  ! zero helicity identifier
  logical,           save :: zerohel(36) = .true., zerohel_ct(36) = .true.

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
    f( 1) = (CI*eQED**2)/3._/**/REALKIND
    f( 2) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 3) = (CI*eQED**2)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*ctVbt*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 6) = (CI*eQED**2*MB)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f( 8) = (CI*cw*eQED**2)/sw
    f( 9) = (CI*countertermnorm*ctVbb*cw*eQED**2*gQCD**2)/sw
    f(10) = (eQED**2*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(11) = (eQED**2*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(12) = (eQED**2*gQCD**2*integralnorm*MB*SwB)/(sw**2*2._/**/REALKIND)
    f(13) = (cw*eQED**2*gQCD**2*integralnorm*SwB)/sw

  c = [ 4*f(10), 4*f(11), 4*f(12), 4*f(13) ]
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
  complex(REALKIND) :: A(10)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rMW, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))

  ! internal WFs
  call vert_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,1))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,3))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-2),Q(:,4),wf(:,4))
  call vert_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,3),MZ,1_intkind1,wf(:,6))
  call vert_WQ_A(wf(:,-3),wf(:,0),wf(:,7))
  call vert_AW_Q(wf(:,-1),wf(:,-2),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,9),MT,1_intkind1,wf(:,9))
  call counter_AW_Q(wf(:,-1),wf(:,-2),wf(:,10))
  call counter_WQ_A(wf(:,-3),wf(:,0),wf(:,11))
  call prop_A_Q(wf(:,8),Q(:,6),MT,1_intkind1,wf(:,12))
  call counter_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,13))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,14))
  call counter_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,15))
  call prop_W_W(wf(:,4),Q(:,12),MZ,1_intkind1,wf(:,16))
  call counter_Q_A(cttt,wf(:,9),Q(:,9),wf(:,17))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MH2)
  den(2) = 1 / (Q(5,3))
  den(3) = 1 / (Q(5,3) - MZ2)
  den(4) = 1 / (Q(5,9) - MT2)
  den(5) = 1 / (Q(5,6) - MT2)
  den(6) = 1 / (Q(5,12) - MH2)
  den(7) = 1 / (Q(5,12))
  den(8) = 1 / (Q(5,12) - MZ2)

  ! denominators
  den(9) = den(4)*den(5)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(10)

  A(1) = cont_SS(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,3),wf(:,4)) * den(2)
  A(3) = cont_VV(wf(:,4),wf(:,6)) * den(3)
  A(4) = cont_QA(wf(:,8),wf(:,9)) * den(4)

  A(5) = cont_QA(wf(:,9),wf(:,10)) * den(4)
  A(6) = cont_QA(wf(:,11),wf(:,12)) * den(5)
  A(7) = cont_SS(wf(:,2),wf(:,13)) * den(6)
  A(8) = cont_VV(wf(:,4),wf(:,14)) * den(7)
  A(9) = cont_VV(wf(:,15),wf(:,16)) * den(8)
  A(10) = cont_QA(wf(:,12),wf(:,17)) * den(9)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(10)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = -(A(2)*f(1))+A(4)*f(3)-A(1)*f(6)+A(3)*f(8)

  M2(1) = -(A(8)*f(2))-A(10)*f(4)+(A(5)+A(6))*f(5)-A(7)*f(7)+A(9)*f(9)

end subroutine colourvectors

end module ol_loop_ppww_bbxwwx_1_/**/REALKIND
