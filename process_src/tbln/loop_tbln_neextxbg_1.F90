
module ol_colourmatrix_tbln_neextxbg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(17,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  16]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   2]
  K1(11,:) = [  16]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [ -18]
  K1(15,:) = [ -18]
  K1(16,:) = [  36]
  K1(17,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_tbln_neextxbg_1_/**/REALKIND



module ol_forced_parameters_tbln_neextxbg_1_/**/REALKIND
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
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wME /= 0) write(*,101) 'wME = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_tbln_neextxbg_1_/**/REALKIND

module ol_loop_tbln_neextxbg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:18)
  ! denominators
  complex(REALKIND), save :: den(11)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,32)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2*gQCD)/(2._/**/REALKIND*sw**2)
    f(2) = (CI*countertermnorm*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(3) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(4) = (CI*countertermnorm*ctGtt*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(5) = (CI*countertermnorm*ctVbt*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(6) = (CI*eQED**2*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(7) = (eQED**2*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(6), f(7), 8*f(7) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(8)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rME, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_A(P(:,3), rMT, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_Q(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rME, H(2), wf(:,-1), 0)
    call pol_wf_A(P(:,3), rMT, H(3), wf(:,-2), 0)
    call pol_wf_Q(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_AV_Q(wf(:,-2),wf(:,-4),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,20),MT,1_intkind1,wf(:,4))
  call vert_WQ_A(wf(:,3),wf(:,-3),wf(:,5))
  call vert_VQ_A(wf(:,-4),wf(:,-3),wf(:,6))
  call prop_Q_A(wf(:,6),Q(:,24),MB,1_intkind1,wf(:,7))
  call vert_AW_Q(wf(:,-2),wf(:,3),wf(:,8))
  call counter_WQ_A(wf(:,3),wf(:,-3),wf(:,9))
  call counter_VQ_A(wf(:,-4),wf(:,-3),wf(:,10))
  call prop_Q_A(wf(:,10),Q(:,24),MB,1_intkind1,wf(:,11))
  call counter_AW_Q(wf(:,-2),wf(:,3),wf(:,12))
  call counter_AV_Q(wf(:,-2),wf(:,-4),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,20),MT,1_intkind1,wf(:,14))
  call counter_A_Q(cttt,wf(:,4),Q(:,20),wf(:,15))
  call prop_Q_A(wf(:,5),Q(:,11),MT,1_intkind1,wf(:,16))
  call counter_Q_A(ctbb,wf(:,7),Q(:,24),wf(:,17))
  call prop_A_Q(wf(:,8),Q(:,7),MB,1_intkind1,wf(:,18))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MW2)
  den(2) = 1 / (Q(5,20) - MT2)
  den(4) = 1 / (Q(5,24) - MB2)
  den(6) = 1 / (Q(5,11) - MT2)
  den(9) = 1 / (Q(5,7) - MB2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(4)*den(10)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(8)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)

  A(3) = cont_QA(wf(:,4),wf(:,9)) * den(3)
  A(4) = cont_QA(wf(:,8),wf(:,11)) * den(5)
  A(5) = cont_QA(wf(:,7),wf(:,12)) * den(5)
  A(6) = cont_QA(wf(:,5),wf(:,14)) * den(3)
  A(7) = cont_QA(wf(:,15),wf(:,16)) * den(8)
  A(8) = cont_QA(wf(:,17),wf(:,18)) * den(11)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(8)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(2))*f(1)

  M2(1) = (A(7)+A(8))*f(2)-A(4)*f(3)-A(6)*f(4)+(-A(3)-A(5))*f(5)

end subroutine colourvectors

end module ol_loop_tbln_neextxbg_1_/**/REALKIND
