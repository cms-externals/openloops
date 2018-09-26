
module ol_colourmatrix_pphbb_ccxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(34,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphbb_ccxddxh_1_/**/REALKIND



module ol_forced_parameters_pphbb_ccxddxh_1_/**/REALKIND
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
end module ol_forced_parameters_pphbb_ccxddxh_1_/**/REALKIND

module ol_loop_pphbb_ccxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:25)
  ! denominators
  complex(REALKIND), save :: den(18)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16), Mct(2,16), Mcol_loop(2,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

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
    f( 1) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MB*YB)/MQ2sum
    f( 2) = (eQED*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f( 3) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MC*YC)/MQ2sum
    f( 4) = (CI*eQED*gQCD**2*YC)/(2._/**/REALKIND*MW*sw)
    f( 5) = (CI*countertermnorm*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f( 6) = (CI*countertermnorm*ctGcc*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f( 7) = (CI*countertermnorm*ctGqq*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f( 8) = (CI*countertermnorm*ctScc*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f( 9) = (CI*eQED*gQCD**4*integralnorm*SwB*YC)/(2._/**/REALKIND*MW*sw)
    f(10) = (eQED*gQCD**4*integralnorm*SwB*YC)/(MW*sw*4._/**/REALKIND)
    f(11) = (eQED*gQCD**4*integralnorm*SwB*YC)/(MW*sw*2._/**/REALKIND)
    f(12) = (eQED*gQCD**4*integralnorm*SwF*YC)/(MW*sw*2._/**/REALKIND)
    f(13) = (eQED*gQCD**4*integralnorm*SwF*YC)/(MW*sw)
    f(14) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MT*YT)/MQ2sum
    f(15) = (eQED*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 3*f(2), 9*f(2), 9*CI*f(9), 27*CI*f(9), 18*f(10), 54*f(10), f(11), 3*f(11), 6*f(11), 8*f(11), 10*f(11), 18*f(11), 21*f(11) &
    , 24*f(11), 54*f(11), 3*f(12), 9*f(12), 3*f(13), 9*f(13), 3*f(15), 9*f(15) ]
  c = (1._/**/REALKIND / 36) * c
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(15)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rMC, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rMC, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rMC, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rMC, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QS_A(gH,wf(:,0),wf(:,-4),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,17),MC,1_intkind1,wf(:,3))
  call vert_QA_V(wf(:,3),wf(:,-1),wf(:,4))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-1),wf(:,5))
  call prop_A_Q(wf(:,5),Q(:,18),MC,1_intkind1,wf(:,6))
  call vert_QA_V(wf(:,0),wf(:,6),wf(:,7))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,8))
  call counter_SG_G(wf(:,-4),wf(:,8),wf(:,9))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,10))
  call counter_QA_V(wf(:,3),wf(:,-1),wf(:,11))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-1),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,18),MC,1_intkind1,wf(:,13))
  call vert_QA_V(wf(:,0),wf(:,13),wf(:,14))
  call counter_QA_V(wf(:,0),wf(:,6),wf(:,15))
  call counter_QS_A(gH,wf(:,0),wf(:,-4),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,17),MC,1_intkind1,wf(:,17))
  call vert_QA_V(wf(:,17),wf(:,-1),wf(:,18))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,19))
  call counter_Q_A(ctcc,wf(:,3),Q(:,17),wf(:,20))
  call prop_A_Q(wf(:,19),Q(:,14),MC,1_intkind1,wf(:,21))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,22))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,23))
  call counter_A_Q(ctcc,wf(:,6),Q(:,18),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,13),MC,1_intkind1,wf(:,25))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,17) - MC2)
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,18) - MC2)
  den(6) = 1 / (Q(5,3))
  den(8) = 1 / (Q(5,14) - MC2)
  den(11) = 1 / (Q(5,19))
  den(14) = 1 / (Q(5,13) - MC2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(9) = den(2)*den(8)
  den(10) = den(1)*den(9)
  den(12) = den(1)*den(11)
  den(13) = den(2)*den(12)
  den(15) = den(2)*den(14)
  den(16) = den(4)*den(15)
  den(17) = den(4)*den(11)
  den(18) = den(2)*den(17)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(15)

  A(1) = cont_VV(wf(:,2),wf(:,4)) * den(3)
  A(2) = cont_VV(wf(:,2),wf(:,7)) * den(5)

  A(3) = cont_VV(wf(:,2),wf(:,9)) * den(7)
  A(4) = cont_VV(wf(:,2),wf(:,9)) * den(7)
  A(5) = cont_VV(wf(:,2),wf(:,9)) * den(7)
  A(6) = cont_VV(wf(:,4),wf(:,10)) * den(3)
  A(7) = cont_VV(wf(:,7),wf(:,10)) * den(5)
  A(8) = cont_VV(wf(:,2),wf(:,11)) * den(3)
  A(9) = cont_VV(wf(:,2),wf(:,14)) * den(5)
  A(10) = cont_VV(wf(:,2),wf(:,15)) * den(5)
  A(11) = cont_VV(wf(:,2),wf(:,18)) * den(3)
  A(12) = cont_QA(wf(:,20),wf(:,21)) * den(10)
  A(13) = cont_VV(wf(:,4),wf(:,22)) * den(13)
  A(14) = cont_QA(wf(:,24),wf(:,25)) * den(16)
  A(15) = cont_VV(wf(:,7),wf(:,22)) * den(18)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(15)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2))*f(4))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2))*f(4))/6._/**/REALKIND

  M2(1) = (A(4)*f(1))/2._/**/REALKIND+(A(5)*f(3))/2._/**/REALKIND+((-A(12)-A(13)-A(14)-A(15))*f(5))/2._/**/REALKIND+((A(8) &
       +A(10))*f(6))/2._/**/REALKIND+((A(6)+A(7))*f(7))/2._/**/REALKIND+((A(9)+A(11))*f(8))/2._/**/REALKIND &
       +(A(3)*f(14))/2._/**/REALKIND
  M2(2) = -(A(4)*f(1))/6._/**/REALKIND-(A(5)*f(3))/6._/**/REALKIND+((A(12)+A(13)+A(14)+A(15))*f(5))/6._/**/REALKIND+((-A(8) &
       -A(10))*f(6))/6._/**/REALKIND+((-A(6)-A(7))*f(7))/6._/**/REALKIND+((-A(9)-A(11))*f(8))/6._/**/REALKIND &
       -(A(3)*f(14))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphbb_ccxddxh_1_/**/REALKIND
