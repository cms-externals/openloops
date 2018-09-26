
module ol_colourmatrix_bbhj_ccxhg_1_/**/REALKIND
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

  K1( 1,:) = [  12]
  K1( 2,:) = [  16]
  K1( 3,:) = [   2]
  K1( 4,:) = [  16]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [   0]
  K1( 8,:) = [ -18]
  K1( 9,:) = [ -18]
  K1(10,:) = [   0]
  K1(11,:) = [  36]
  K1(12,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_bbhj_ccxhg_1_/**/REALKIND



module ol_forced_parameters_bbhj_ccxhg_1_/**/REALKIND
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
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_bbhj_ccxhg_1_/**/REALKIND

module ol_loop_bbhj_ccxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(6)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:16)
  ! denominators
  complex(REALKIND), save :: den(7)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,8), Mct(1,8), Mcol_loop(1,8)
  ! zero helicity identifier
  logical,           save :: zerohel(8) = .true., zerohel_ct(8) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_decl_/**/DREALKIND, only: do_ew_renorm
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  logical :: force_ew_renorm
  if (do_ew_renorm <= 0 .and. 0 >= 1) then
    ! If do_ew_renorm is needed, but not already enabled,
    ! force calling loop_parameters_init().
    force_ew_renorm = .true.
  else
    force_ew_renorm = .false.
  end if
  do_ew_renorm = 0
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0 .or. force_ew_renorm) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*countertermnorm*ctHGG*eQED*gQCD**3*MB*YB)/MQ2sum
    f( 2) = (eQED*gQCD**3*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f( 3) = (CI*countertermnorm*ctHGG*eQED*gQCD**3*MC*YC)/MQ2sum
    f( 4) = (CI*eQED*gQCD*YC)/(2._/**/REALKIND*MW*sw)
    f( 5) = (CI*countertermnorm*eQED*gQCD**3*YC)/(2._/**/REALKIND*MW*sw)
    f( 6) = (CI*countertermnorm*ctGcc*eQED*gQCD**3*YC)/(2._/**/REALKIND*MW*sw)
    f( 7) = (CI*countertermnorm*ctScc*eQED*gQCD**3*YC)/(2._/**/REALKIND*MW*sw)
    f( 8) = (CI*eQED*gQCD**3*integralnorm*SwB*YC)/(2._/**/REALKIND*MW*sw)
    f( 9) = (eQED*gQCD**3*integralnorm*SwB*YC)/(MW*sw*2._/**/REALKIND)
    f(10) = (eQED*gQCD**3*integralnorm*SwF*YC)/(MW*sw*2._/**/REALKIND)
    f(11) = (CI*countertermnorm*ctHGG*eQED*gQCD**3*MT*YT)/MQ2sum
    f(12) = (eQED*gQCD**3*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 3*f(2), 9*CI*f(8), f(9), 8*f(9), 3*f(10), 3*f(12) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,4)
  integer,           intent(in)  :: H(4)
  integer,           intent(in), optional  :: POLSEL(4)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(11)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rMC, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rMC, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))

  else
    call pol_wf_Q(P(:,1), rMC, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rMC, H(2), wf(:,-1), 0)
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)

  end if

  ! internal WFs
  call vert_QS_A(gH,wf(:,0),wf(:,-2),wf(:,1))
  call vert_AV_Q(wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),MC,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,-3),wf(:,0),wf(:,4))
  call vert_SA_Q(gH,wf(:,-2),wf(:,-1),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,9),MC,1_intkind1,wf(:,6))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,7))
  call counter_SG_G(wf(:,-2),wf(:,-3),wf(:,8))
  call counter_AV_Q(wf(:,-1),wf(:,-3),wf(:,9))
  call counter_SA_Q(gH,wf(:,-2),wf(:,-1),wf(:,10))
  call counter_VQ_A(wf(:,-3),wf(:,0),wf(:,11))
  call prop_A_Q(wf(:,5),Q(:,6),MC,1_intkind1,wf(:,12))
  call counter_QS_A(gH,wf(:,0),wf(:,-2),wf(:,13))
  call prop_A_Q(wf(:,2),Q(:,10),MC,1_intkind1,wf(:,14))
  call counter_Q_A(ctcc,wf(:,3),Q(:,5),wf(:,15))
  call counter_Q_A(ctcc,wf(:,6),Q(:,9),wf(:,16))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MC2)
  den(2) = 1 / (Q(5,9) - MC2)
  den(3) = 1 / (Q(5,3))
  den(4) = 1 / (Q(5,6) - MC2)
  den(5) = 1 / (Q(5,10) - MC2)

  ! denominators
  den(6) = den(1)*den(5)
  den(7) = den(2)*den(4)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(11)

  A(1) = cont_QA(wf(:,2),wf(:,3)) * den(1)
  A(2) = cont_QA(wf(:,5),wf(:,6)) * den(2)

  A(3) = cont_VV(wf(:,7),wf(:,8)) * den(3)
  A(4) = cont_VV(wf(:,7),wf(:,8)) * den(3)
  A(5) = cont_VV(wf(:,7),wf(:,8)) * den(3)
  A(6) = cont_QA(wf(:,3),wf(:,9)) * den(1)
  A(7) = cont_QA(wf(:,6),wf(:,10)) * den(2)
  A(8) = cont_QA(wf(:,11),wf(:,12)) * den(4)
  A(9) = cont_QA(wf(:,13),wf(:,14)) * den(5)
  A(10) = cont_QA(wf(:,14),wf(:,15)) * den(6)
  A(11) = cont_QA(wf(:,12),wf(:,16)) * den(7)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(11)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(2))*f(4)

  M2(1) = A(4)*f(1)+A(5)*f(3)+(-A(10)-A(11))*f(5)+(A(6)+A(8))*f(6)+(A(7)+A(9))*f(7)+A(3)*f(11)

end subroutine colourvectors

end module ol_loop_bbhj_ccxhg_1_/**/REALKIND
