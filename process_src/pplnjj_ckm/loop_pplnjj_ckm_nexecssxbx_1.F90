
module ol_colourmatrix_pplnjj_ckm_nexecssxbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,  -4]
  K1(28,:) = [  -4, -12]
  K1(29,:) = [ -12,  -4]
  K1(30,:) = [  -4,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [ -12,  -4]
  K1(38,:) = [  -4,   0]
  K1(39,:) = [   0,  -4]
  K1(40,:) = [  -4, -12]
  K1(41,:) = [   0,   4]
  K1(42,:) = [   4,   0]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplnjj_ckm_nexecssxbx_1_/**/REALKIND



module ol_forced_parameters_pplnjj_ckm_nexecssxbx_1_/**/REALKIND
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
  if (ME /= 0) write(*,101) 'ME = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplnjj_ckm_nexecssxbx_1_/**/REALKIND

module ol_loop_pplnjj_ckm_nexecssxbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:26)
  ! denominators
  complex(REALKIND), save :: den(25)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64), Mct(2,64), Mcol_loop(2,64)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**2*gQCD**2*VCKMbc)/(2._/**/REALKIND*sw**2)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4*VCKMbc)/(2._/**/REALKIND*sw**2)
    f( 3) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4*VCKMbc)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*ctGcc*eQED**2*gQCD**4*VCKMbc)/(2._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*VCKMbc)/(2._/**/REALKIND*sw**2)
    f( 6) = (CI*countertermnorm*ctVbc*eQED**2*gQCD**4*VCKMbc)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*eQED**2*gQCD**4*integralnorm*SwB*VCKMbc)/(2._/**/REALKIND*sw**2)
    f( 8) = (eQED**2*gQCD**4*integralnorm*SwB*VCKMbc)/(sw**2*4._/**/REALKIND)
    f( 9) = (eQED**2*gQCD**4*integralnorm*SwB*VCKMbc)/(sw**2*2._/**/REALKIND)
    f(10) = (eQED**2*gQCD**4*integralnorm*SwF*VCKMbc)/(sw**2*2._/**/REALKIND)
    f(11) = (eQED**2*gQCD**4*integralnorm*SwF*VCKMbc)/sw**2

  c = [ 9*CI*f(7), 27*CI*f(7), 18*f(8), 54*f(8), f(9), 3*f(9), 6*f(9), 8*f(9), 10*f(9), 18*f(9), 21*f(9), 24*f(9), 54*f(9) &
    , 3*f(10), 9*f(10), 3*f(11), 9*f(11) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(12)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_A(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_A(P(:,6), rMB, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_A(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_A(P(:,6), rMB, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_QA_V(wf(:,-3),wf(:,-4),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,3))
  call vert_WQ_A(wf(:,3),wf(:,-2),wf(:,4))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,7),MB,1_intkind1,wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,-2),wf(:,7))
  call vert_AW_Q(wf(:,-5),wf(:,3),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,28),ZERO,0_intkind1,wf(:,9))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,10))
  call counter_AW_Q(wf(:,-5),wf(:,3),wf(:,11))
  call counter_QA_V(wf(:,-3),wf(:,-4),wf(:,12))
  call vert_AV_Q(wf(:,-5),wf(:,12),wf(:,13))
  call vert_VQ_A(wf(:,12),wf(:,-2),wf(:,14))
  call prop_Q_A(wf(:,14),Q(:,28),ZERO,0_intkind1,wf(:,15))
  call counter_VQ_A(wf(:,2),wf(:,-2),wf(:,16))
  call prop_A_Q(wf(:,8),Q(:,35),ZERO,0_intkind1,wf(:,17))
  call counter_WQ_A(wf(:,3),wf(:,-2),wf(:,18))
  call prop_A_Q(wf(:,5),Q(:,56),MB,1_intkind1,wf(:,19))
  call counter_V_V(ctGG,wf(:,2),Q(:,24),wf(:,20))
  call vert_VQ_A(wf(:,20),wf(:,-2),wf(:,21))
  call counter_Q_A(ctbb,wf(:,6),Q(:,7),wf(:,22))
  call counter_Q_A(ctcc,wf(:,9),Q(:,28),wf(:,23))
  call vert_AV_Q(wf(:,-5),wf(:,20),wf(:,24))
  call vert_QA_V(wf(:,6),wf(:,-5),wf(:,25))
  call vert_QA_V(wf(:,-2),wf(:,17),wf(:,26))

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
  den(2) = 1 / (Q(5,24))
  den(3) = 1 / (Q(5,7) - MB2)
  den(6) = 1 / (Q(5,28))
  den(9) = 1 / (Q(5,35))
  den(12) = 1 / (Q(5,56) - MB2)
  den(20) = 1 / (Q(5,39))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(15) = den(2)**2
  den(16) = den(10)*den(15)
  den(17) = den(4)*den(13)
  den(18) = den(7)*den(10)
  den(19) = den(4)*den(15)
  den(21) = den(4)*den(20)
  den(22) = den(10)*den(20)
  den(23) = den(1)*den(2)
  den(24) = den(2)*den(21)
  den(25) = den(2)*den(22)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(12)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)

  A(3) = cont_QA(wf(:,6),wf(:,10)) * den(5)
  A(4) = cont_QA(wf(:,9),wf(:,11)) * den(8)
  A(5) = cont_QA(wf(:,6),wf(:,13)) * den(5)
  A(6) = cont_QA(wf(:,8),wf(:,15)) * den(8)
  A(7) = cont_QA(wf(:,16),wf(:,17)) * den(11)
  A(8) = cont_QA(wf(:,18),wf(:,19)) * den(14)
  A(9) = cont_QA(wf(:,17),wf(:,21)) * den(16)
  A(10) = cont_QA(wf(:,19),wf(:,22)) * den(17)
  A(11) = cont_QA(wf(:,17),wf(:,23)) * den(18)
  A(12) = cont_QA(wf(:,6),wf(:,24)) * den(19)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(12)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2))*f(1))/6._/**/REALKIND
  M1(2) = ((-A(1)-A(2))*f(1))/2._/**/REALKIND

  M2(1) = ((-A(9)-A(10)-A(11)-A(12))*f(2))/6._/**/REALKIND+(A(3)*f(3))/6._/**/REALKIND+(A(7)*f(4))/6._/**/REALKIND+((A(5) &
       +A(6))*f(5))/6._/**/REALKIND+((A(4)+A(8))*f(6))/6._/**/REALKIND
  M2(2) = ((A(9)+A(10)+A(11)+A(12))*f(2))/2._/**/REALKIND-(A(3)*f(3))/2._/**/REALKIND-(A(7)*f(4))/2._/**/REALKIND+((-A(5) &
       -A(6))*f(5))/2._/**/REALKIND+((-A(4)-A(8))*f(6))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnjj_ckm_nexecssxbx_1_/**/REALKIND
