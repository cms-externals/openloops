
module ol_colourmatrix_tbqq_uxtdbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(34,2), K2(2,2), KL(2,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12,   0]
  K1( 2,:) = [   0,  12]
  K1( 3,:) = [  16,   0]
  K1( 4,:) = [   0,  16]
  K1( 5,:) = [   0,  -2]
  K1( 6,:) = [  -2,   0]
  K1( 7,:) = [  16,   0]
  K1( 8,:) = [   0,  16]
  K1( 9,:) = [   2,   0]
  K1(10,:) = [   0, -16]
  K1(11,:) = [   0,   2]
  K1(12,:) = [   2,   0]
  K1(13,:) = [  16,   0]
  K1(14,:) = [   0,  16]
  K1(15,:) = [   0,   2]
  K1(16,:) = [   2,   0]
  K1(17,:) = [ -16,   0]
  K1(18,:) = [   0,   2]
  K1(19,:) = [   0,  -2]
  K1(20,:) = [  -2,   0]
  K1(21,:) = [  16,   0]
  K1(22,:) = [   0,  16]
  K1(23,:) = [ -18,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0, -18]
  K1(27,:) = [ -18,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0, -18]
  K1(31,:) = [  36,   0]
  K1(32,:) = [   0,  36]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]

  K2(1,:) = [ 12,  0]
  K2(2,:) = [  0, 12]

  KL(1,:) = [  4, 12,  0,  4]
  KL(2,:) = [  4,  0, 12,  4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_tbqq_uxtdbxg_1_/**/REALKIND



module ol_forced_parameters_tbqq_uxtdbxg_1_/**/REALKIND
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
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_tbqq_uxtdbxg_1_/**/REALKIND

module ol_loop_tbqq_uxtdbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(5)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:50)
  ! denominators
  complex(REALKIND), save :: den(28)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,32)
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
    f( 1) = (CI*eQED**2*gQCD)/(2._/**/REALKIND*sw**2)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 3) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*ctGtt*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 6) = (CI*countertermnorm*ctVbt*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctSqq*eQED**2*gQCD**3)/(2._/**/REALKIND*MW**2*sw**2)
    f( 9) = (CI*eQED**2*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(10) = (eQED**2*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)

  c = [ 3*CI*f(9), 9*CI*f(9), f(10), 3*f(10), 8*f(10) ]
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(24)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rMT, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rMT, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,-2),wf(:,0),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,2))
  call prop_Q_A(wf(:,2),Q(:,18),MT,1_intkind1,wf(:,3))
  call vert_AW_Q(wf(:,-3),wf(:,1),wf(:,4))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,5))
  call prop_A_Q(wf(:,5),Q(:,24),MB,1_intkind1,wf(:,6))
  call vert_WQ_A(wf(:,1),wf(:,-1),wf(:,7))
  call vert_AV_Q(wf(:,0),wf(:,-4),wf(:,8))
  call vert_QA_W(wf(:,-1),wf(:,-3),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,17),ZERO,0_intkind1,wf(:,10))
  call vert_QA_W(wf(:,-2),wf(:,10),wf(:,11))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,12))
  call prop_Q_A(wf(:,12),Q(:,20),ZERO,0_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,0),wf(:,9),wf(:,14))
  call counter_AW_Q(wf(:,-3),wf(:,1),wf(:,15))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,24),MB,1_intkind1,wf(:,17))
  call vert_AQ_S(gPbt,wf(:,-3),wf(:,-1),wf(:,18))
  call counter_AQ_S(gPud,wf(:,10),wf(:,-2),wf(:,19))
  call counter_QA_W(wf(:,-2),wf(:,10),wf(:,20))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,20),ZERO,0_intkind1,wf(:,22))
  call counter_WQ_A(wf(:,1),wf(:,-1),wf(:,23))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,18),MT,1_intkind1,wf(:,25))
  call counter_QA_W(wf(:,-1),wf(:,-3),wf(:,26))
  call vert_AW_Q(wf(:,0),wf(:,26),wf(:,27))
  call counter_SA_Q(gPud,wf(:,18),wf(:,0),wf(:,28))
  call counter_AW_Q(wf(:,0),wf(:,9),wf(:,29))
  call counter_AV_Q(wf(:,0),wf(:,-4),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,17),ZERO,0_intkind1,wf(:,31))
  call vert_QA_W(wf(:,-2),wf(:,31),wf(:,32))
  call counter_AQ_S(gPud,wf(:,0),wf(:,-2),wf(:,33))
  call vert_SA_Q(gPbt,wf(:,33),wf(:,-3),wf(:,34))
  call counter_QA_W(wf(:,-2),wf(:,0),wf(:,35))
  call vert_AW_Q(wf(:,-3),wf(:,35),wf(:,36))
  call vert_QS_A(gPbt,wf(:,-1),wf(:,33),wf(:,37))
  call vert_WQ_A(wf(:,35),wf(:,-1),wf(:,38))
  call counter_Q_A(cttt,wf(:,3),Q(:,18),wf(:,39))
  call prop_A_Q(wf(:,4),Q(:,13),MT,1_intkind1,wf(:,40))
  call counter_A_Q(ctbb,wf(:,6),Q(:,24),wf(:,41))
  call prop_Q_A(wf(:,7),Q(:,7),MB,1_intkind1,wf(:,42))
  call vert_WQ_A(wf(:,9),wf(:,-2),wf(:,43))
  call counter_A_Q(ctqq,wf(:,10),Q(:,17),wf(:,44))
  call prop_Q_A(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,45))
  call counter_Q_A(ctqq,wf(:,13),Q(:,20),wf(:,46))
  call prop_A_Q(wf(:,14),Q(:,11),ZERO,0_intkind1,wf(:,47))
  call vert_QA_W(wf(:,3),wf(:,-3),wf(:,48))
  call vert_QA_W(wf(:,13),wf(:,0),wf(:,49))
  call vert_QA_W(wf(:,-1),wf(:,6),wf(:,50))

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
  den(2) = 1 / (Q(5,18) - MT2)
  den(4) = 1 / (Q(5,24) - MB2)
  den(6) = 1 / (Q(5,17))
  den(7) = 1 / (Q(5,10) - MW2)
  den(9) = 1 / (Q(5,20))
  den(11) = 1 / (Q(5,13) - MT2)
  den(14) = 1 / (Q(5,7) - MB2)
  den(17) = 1 / (Q(5,14))
  den(20) = 1 / (Q(5,11))
  den(23) = 1 / (Q(5,21) - MW2)
  den(25) = 1 / (Q(5,26) - MW2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(12) = den(1)*den(11)
  den(13) = den(2)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(4)*den(15)
  den(18) = den(7)*den(17)
  den(19) = den(6)*den(18)
  den(21) = den(7)*den(20)
  den(22) = den(9)*den(21)
  den(24) = den(6)*den(23)
  den(26) = den(2)*den(25)
  den(27) = den(9)*den(23)
  den(28) = den(4)*den(25)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(24)

  A(1) = cont_QA(wf(:,3),wf(:,4)) * den(3)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,11)) * den(8)
  A(4) = cont_QA(wf(:,13),wf(:,14)) * den(10)

  A(5) = cont_QA(wf(:,3),wf(:,15)) * den(3)
  A(6) = cont_QA(wf(:,7),wf(:,17)) * den(5)
  A(7) = cont_SS(wf(:,18),wf(:,19)) * den(8)
  A(8) = cont_VV(wf(:,9),wf(:,20)) * den(8)
  A(9) = cont_QA(wf(:,14),wf(:,22)) * den(10)
  A(10) = cont_QA(wf(:,6),wf(:,23)) * den(5)
  A(11) = cont_QA(wf(:,4),wf(:,25)) * den(3)
  A(12) = cont_VV(wf(:,11),wf(:,26)) * den(8)
  A(13) = cont_QA(wf(:,13),wf(:,27)) * den(10)
  A(14) = cont_QA(wf(:,13),wf(:,28)) * den(10)
  A(15) = cont_QA(wf(:,13),wf(:,29)) * den(10)
  A(16) = cont_VV(wf(:,9),wf(:,32)) * den(8)
  A(17) = cont_QA(wf(:,3),wf(:,34)) * den(3)
  A(18) = cont_QA(wf(:,3),wf(:,36)) * den(3)
  A(19) = cont_QA(wf(:,6),wf(:,37)) * den(5)
  A(20) = cont_QA(wf(:,6),wf(:,38)) * den(5)
  A(21) = cont_QA(wf(:,39),wf(:,40)) * den(13)
  A(22) = cont_QA(wf(:,41),wf(:,42)) * den(16)
  A(23) = cont_QA(wf(:,44),wf(:,45)) * den(19)
  A(24) = cont_QA(wf(:,46),wf(:,47)) * den(22)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(24)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (A(3)+A(4))*f(1)
  M1(2) = (A(1)+A(2))*f(1)

  M2(1) = (-A(23)-A(24))*f(2)+(A(9)+A(16))*f(4)+(A(12)+A(13))*f(6)+(A(8)+A(15))*f(7)+(A(7)+A(14))*f(8)
  M2(2) = (-A(21)-A(22))*f(2)+A(6)*f(3)+A(11)*f(5)+(A(5)+A(10))*f(6)+(A(18)+A(20))*f(7)+(A(17)+A(19))*f(8)

end subroutine colourvectors

end module ol_loop_tbqq_uxtdbxg_1_/**/REALKIND
