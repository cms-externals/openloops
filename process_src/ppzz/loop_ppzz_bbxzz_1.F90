
module ol_colourmatrix_ppzz_bbxzz_1_/**/REALKIND
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
end module ol_colourmatrix_ppzz_bbxzz_1_/**/REALKIND



module ol_forced_parameters_ppzz_bbxzz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzz_bbxzz_1_/**/REALKIND

module ol_loop_ppzz_bbxzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(2)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:17)
  ! denominators
  complex(REALKIND), save :: den(8)
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
    f(1) = CI*eQED**2
    f(2) = CI*countertermnorm*eQED**2*gQCD**2
    f(3) = CI*countertermnorm*ctVbb*eQED**2*gQCD**2
    f(4) = (CI*eQED**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(5) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(6) = eQED**2*gQCD**2*integralnorm*SwB
    f(7) = (eQED**2*gQCD**2*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 4*f(6), 4*f(7) ]
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
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))

  ! internal WFs
  call vert_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,1))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,3))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,5),MB,1_intkind1,wf(:,5))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,6))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,9),MB,1_intkind1,wf(:,8))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,9))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,10))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,11))
  call prop_A_Q(wf(:,7),Q(:,6),MB,1_intkind1,wf(:,12))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,13))
  call prop_A_Q(wf(:,4),Q(:,10),MB,1_intkind1,wf(:,14))
  call counter_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,15))
  call counter_Q_A(ctbb,wf(:,5),Q(:,5),wf(:,16))
  call counter_Q_A(ctbb,wf(:,8),Q(:,9),wf(:,17))

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
  den(2) = 1 / (Q(5,5) - MB2)
  den(3) = 1 / (Q(5,9) - MB2)
  den(4) = 1 / (Q(5,6) - MB2)
  den(5) = 1 / (Q(5,10) - MB2)
  den(6) = 1 / (Q(5,12) - MH2)

  ! denominators
  den(7) = den(2)*den(5)
  den(8) = den(3)*den(4)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(10)

  A(1) = cont_SS(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_QA(wf(:,4),wf(:,5)) * den(2)
  A(3) = cont_QA(wf(:,7),wf(:,8)) * den(3)

  A(4) = cont_QA(wf(:,5),wf(:,9)) * den(2)
  A(5) = cont_QA(wf(:,8),wf(:,10)) * den(3)
  A(6) = cont_QA(wf(:,11),wf(:,12)) * den(4)
  A(7) = cont_QA(wf(:,13),wf(:,14)) * den(5)
  A(8) = cont_SS(wf(:,2),wf(:,15)) * den(6)
  A(9) = cont_QA(wf(:,14),wf(:,16)) * den(7)
  A(10) = cont_QA(wf(:,12),wf(:,17)) * den(8)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(10)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(2)+A(3))*f(1)-A(1)*f(4)

  M2(1) = (-A(9)-A(10))*f(2)+(A(4)+A(5)+A(6)+A(7))*f(3)-A(8)*f(5)

end subroutine colourvectors

end module ol_loop_ppzz_bbxzz_1_/**/REALKIND
