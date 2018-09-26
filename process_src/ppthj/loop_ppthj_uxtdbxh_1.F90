
module ol_colourmatrix_ppthj_uxtdbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(17,1), K2(1,1), KL(1,2)
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
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]

  K2(1,:) = [ 9]

  KL(1,:) = [ 9, 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppthj_uxtdbxh_1_/**/REALKIND



module ol_forced_parameters_ppthj_uxtdbxh_1_/**/REALKIND
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
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppthj_uxtdbxh_1_/**/REALKIND

module ol_loop_ppthj_uxtdbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(24), c(12)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:37)
  ! denominators
  complex(REALKIND), save :: den(19)
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
    f( 1) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**2*MH**2)/(4._/**/REALKIND*MW**3*sw**3)
    f( 2) = (CI*eQED**3)/(4._/**/REALKIND*MW*sw**3)
    f( 3) = (CI*countertermnorm*eQED**3*gQCD**2)/(4._/**/REALKIND*MW*sw**3)
    f( 4) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**2)/(4._/**/REALKIND*MW*sw**3)
    f( 5) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(4._/**/REALKIND*MW*sw**3)
    f( 6) = (CI*eQED**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 7) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**2*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 8) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 9) = (eQED**3*gQCD**2*integralnorm*SwB)/(MW*sw**3*4._/**/REALKIND)
    f(10) = (eQED**3*gQCD**2*integralnorm*lambdaHWW*MW*SwB)/(sw**3*2._/**/REALKIND)
    f(11) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**2*YB)/(4._/**/REALKIND*MW**3*sw**3)
    f(12) = (CI*eQED**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(13) = (CI*countertermnorm*eQED**3*gQCD**2*YB)/(4._/**/REALKIND*MW*sw**3)
    f(14) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*YB)/(4._/**/REALKIND*MW*sw**3)
    f(15) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**2*YB)/(4._/**/REALKIND*MW*sw**3)
    f(16) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*YB)/(4._/**/REALKIND*MW*sw**3)
    f(17) = (eQED**3*gQCD**2*integralnorm*SwB*YB)/(MW*sw**3*4._/**/REALKIND)
    f(18) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**2*YT)/(4._/**/REALKIND*MW**3*sw**3)
    f(19) = (CI*eQED**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(20) = (CI*countertermnorm*eQED**3*gQCD**2*YT)/(4._/**/REALKIND*MW*sw**3)
    f(21) = (CI*countertermnorm*ctStt*eQED**3*gQCD**2*YT)/(4._/**/REALKIND*MW*sw**3)
    f(22) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**2*YT)/(4._/**/REALKIND*MW*sw**3)
    f(23) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*YT)/(4._/**/REALKIND*MW*sw**3)
    f(24) = (eQED**3*gQCD**2*integralnorm*SwB*YT)/(MW*sw**3*4._/**/REALKIND)

  c = [ f(9), 3*f(9), 8*f(9), f(10), 3*f(10), 8*f(10), f(17), 3*f(17), 8*f(17), f(24), 3*f(24), 8*f(24) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(20)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rMT, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_W(wf(:,-2),wf(:,0),wf(:,1))
  call vert_AQ_S(gPbt,wf(:,-3),wf(:,-1),wf(:,2))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,1),Q(:,5),wf(:,3))
  call vert_QA_W(wf(:,-1),wf(:,-3),wf(:,4))
  call vert_SV_V(wf(:,-4),wf(:,1),wf(:,5))
  call vert_QS_A(gH,wf(:,-1),wf(:,-4),wf(:,6))
  call prop_Q_A(wf(:,6),Q(:,18),MT,1_intkind1,wf(:,7))
  call vert_AW_Q(wf(:,-3),wf(:,1),wf(:,8))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,9))
  call prop_A_Q(wf(:,9),Q(:,24),MB,1_intkind1,wf(:,10))
  call vert_WQ_A(wf(:,1),wf(:,-1),wf(:,11))
  call counter_AW_Q(wf(:,-3),wf(:,1),wf(:,12))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,24),MB,1_intkind1,wf(:,14))
  call counter_WQ_A(wf(:,1),wf(:,-1),wf(:,15))
  call counter_QS_A(gH,wf(:,-1),wf(:,-4),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,18),MT,1_intkind1,wf(:,17))
  call counter_AQ_S(ctSbt,wf(:,-3),wf(:,-1),wf(:,18))
  call counter_QA_W(wf(:,-1),wf(:,-3),wf(:,19))
  call counter_AQ_S(gPud,wf(:,0),wf(:,-2),wf(:,20))
  call vert_SS_S(wf(:,20),wf(:,-4),wf(:,21))
  call vert_ST_V(wf(:,20),Q(:,5),wf(:,-4),Q(:,16),wf(:,22))
  call counter_QA_W(wf(:,-2),wf(:,0),wf(:,23))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,23),Q(:,5),wf(:,24))
  call vert_SV_V(wf(:,-4),wf(:,23),wf(:,25))
  call vert_SA_Q(gPbt,wf(:,20),wf(:,-3),wf(:,26))
  call vert_AW_Q(wf(:,-3),wf(:,23),wf(:,27))
  call vert_QS_A(gPbt,wf(:,-1),wf(:,20),wf(:,28))
  call vert_WQ_A(wf(:,23),wf(:,-1),wf(:,29))
  call counter_Q_A(cttt,wf(:,7),Q(:,18),wf(:,30))
  call prop_A_Q(wf(:,8),Q(:,13),MT,1_intkind1,wf(:,31))
  call counter_A_Q(ctbb,wf(:,10),Q(:,24),wf(:,32))
  call prop_Q_A(wf(:,11),Q(:,7),MB,1_intkind1,wf(:,33))
  call vert_ST_V(wf(:,2),Q(:,10),wf(:,-4),Q(:,16),wf(:,34))
  call vert_SV_V(wf(:,-4),wf(:,4),wf(:,35))
  call vert_QA_W(wf(:,7),wf(:,-3),wf(:,36))
  call vert_QA_W(wf(:,-1),wf(:,10),wf(:,37))

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
  den(4) = 1 / (Q(5,18) - MT2)
  den(6) = 1 / (Q(5,24) - MB2)
  den(8) = 1 / (Q(5,13) - MT2)
  den(11) = 1 / (Q(5,7) - MB2)
  den(14) = 1 / (Q(5,21) - MW2)
  den(16) = 1 / (Q(5,26) - MW2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(1)*den(8)
  den(10) = den(4)*den(9)
  den(12) = den(1)*den(11)
  den(13) = den(6)*den(12)
  den(15) = den(1)*den(14)
  den(17) = den(2)*den(16)
  den(18) = den(4)*den(16)
  den(19) = den(6)*den(16)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(20)

  A(1) = cont_SS(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,4),wf(:,5)) * den(3)
  A(3) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(4) = cont_QA(wf(:,10),wf(:,11)) * den(7)

  A(5) = cont_QA(wf(:,7),wf(:,12)) * den(5)
  A(6) = cont_QA(wf(:,11),wf(:,14)) * den(7)
  A(7) = cont_QA(wf(:,10),wf(:,15)) * den(7)
  A(8) = cont_QA(wf(:,8),wf(:,17)) * den(5)
  A(9) = cont_SS(wf(:,3),wf(:,18)) * den(3)
  A(10) = cont_VV(wf(:,5),wf(:,19)) * den(3)
  A(11) = cont_SS(wf(:,2),wf(:,21)) * den(3)
  A(12) = cont_VV(wf(:,4),wf(:,22)) * den(3)
  A(13) = cont_SS(wf(:,2),wf(:,24)) * den(3)
  A(14) = cont_VV(wf(:,4),wf(:,25)) * den(3)
  A(15) = cont_QA(wf(:,7),wf(:,26)) * den(5)
  A(16) = cont_QA(wf(:,7),wf(:,27)) * den(5)
  A(17) = cont_QA(wf(:,10),wf(:,28)) * den(7)
  A(18) = cont_QA(wf(:,10),wf(:,29)) * den(7)
  A(19) = cont_QA(wf(:,30),wf(:,31)) * den(10)
  A(20) = cont_QA(wf(:,32),wf(:,33)) * den(13)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(20)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(1)*f(2)+A(2)*f(6)+A(4)*f(12)+A(3)*f(19)

  M2(1) = A(11)*f(1)+A(9)*f(3)+A(12)*f(4)+A(13)*f(5)+A(10)*f(7)+A(14)*f(8)+A(17)*f(11)-A(20)*f(13)+A(6)*f(14)+A(7)*f(15) &
       +A(18)*f(16)+A(15)*f(18)-A(19)*f(20)+A(8)*f(21)+A(5)*f(22)+A(16)*f(23)

end subroutine colourvectors

end module ol_loop_ppthj_uxtdbxh_1_/**/REALKIND
