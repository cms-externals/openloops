
module ol_colourmatrix_ppwjj_ckm_cxdbbxwx_1_/**/REALKIND
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
  K1( 9,:) = [ -12,  -4]
  K1(10,:) = [  -4,   0]
  K1(11,:) = [   0,   4]
  K1(12,:) = [   4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [   0,   4]
  K1(16,:) = [   4,   0]
  K1(17,:) = [ -12,  -4]
  K1(18,:) = [  -4,   0]
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
end module ol_colourmatrix_ppwjj_ckm_cxdbbxwx_1_/**/REALKIND



module ol_forced_parameters_ppwjj_ckm_cxdbbxwx_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwjj_ckm_cxdbbxwx_1_/**/REALKIND

module ol_loop_ppwjj_ckm_cxdbbxwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:23)
  ! denominators
  complex(REALKIND), save :: den(16)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,48), Mct(2,48), Mcol_loop(2,48)
  ! zero helicity identifier
  logical,           save :: zerohel(48) = .true., zerohel_ct(48) = .true.

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
    f( 1) = (CI*eQED*gQCD**2*VCKMdc)/(sqrt2*sw)
    f( 2) = (CI*countertermnorm*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 3) = (CI*countertermnorm*ctGbb*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 4) = (CI*countertermnorm*ctGcc*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 5) = (CI*countertermnorm*ctGqq*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 6) = (CI*countertermnorm*ctVqq*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 7) = (CI*eQED*gQCD**4*integralnorm*SwB*VCKMdc)/(sqrt2*sw)
    f( 8) = (eQED*gQCD**4*integralnorm*SwB*VCKMdc)/(sqrt2*sw*2._/**/REALKIND)
    f( 9) = (eQED*gQCD**4*integralnorm*SwB*VCKMdc)/(sqrt2*sw)
    f(10) = (eQED*gQCD**4*integralnorm*SwF*VCKMdc)/(sqrt2*sw)
    f(11) = (2*eQED*gQCD**4*integralnorm*SwF*VCKMdc)/(sqrt2*sw)

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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  integer,           intent(in), optional  :: POLSEL(5)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(12)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_AW_Q(wf(:,0),wf(:,-4),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_A_Q(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,3))
  call vert_QA_V(wf(:,-1),wf(:,3),wf(:,4))
  call vert_WQ_A(wf(:,-4),wf(:,-1),wf(:,5))
  call prop_Q_A(wf(:,5),Q(:,18),ZERO,0_intkind1,wf(:,6))
  call vert_QA_V(wf(:,6),wf(:,0),wf(:,7))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,8))
  call counter_QA_V(wf(:,-1),wf(:,3),wf(:,9))
  call counter_WQ_A(wf(:,-4),wf(:,-1),wf(:,10))
  call prop_Q_A(wf(:,10),Q(:,18),ZERO,0_intkind1,wf(:,11))
  call vert_QA_V(wf(:,11),wf(:,0),wf(:,12))
  call counter_QA_V(wf(:,6),wf(:,0),wf(:,13))
  call counter_AW_Q(wf(:,0),wf(:,-4),wf(:,14))
  call prop_A_Q(wf(:,14),Q(:,17),ZERO,0_intkind1,wf(:,15))
  call vert_QA_V(wf(:,-1),wf(:,15),wf(:,16))
  call vert_VQ_A(wf(:,2),wf(:,-1),wf(:,17))
  call counter_A_Q(ctqq,wf(:,3),Q(:,17),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,14),ZERO,0_intkind1,wf(:,19))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,20))
  call vert_AV_Q(wf(:,0),wf(:,2),wf(:,21))
  call counter_Q_A(ctcc,wf(:,6),Q(:,18),wf(:,22))
  call prop_A_Q(wf(:,21),Q(:,13),ZERO,0_intkind1,wf(:,23))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,17))
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,18))
  den(6) = 1 / (Q(5,14))
  den(9) = 1 / (Q(5,19))
  den(12) = 1 / (Q(5,13))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(4)*den(13)
  den(15) = den(4)*den(9)
  den(16) = den(2)*den(15)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(12)

  A(1) = cont_VV(wf(:,2),wf(:,4)) * den(3)
  A(2) = cont_VV(wf(:,2),wf(:,7)) * den(5)

  A(3) = cont_VV(wf(:,4),wf(:,8)) * den(3)
  A(4) = cont_VV(wf(:,7),wf(:,8)) * den(5)
  A(5) = cont_VV(wf(:,2),wf(:,9)) * den(3)
  A(6) = cont_VV(wf(:,2),wf(:,12)) * den(5)
  A(7) = cont_VV(wf(:,2),wf(:,13)) * den(5)
  A(8) = cont_VV(wf(:,2),wf(:,16)) * den(3)
  A(9) = cont_QA(wf(:,18),wf(:,19)) * den(8)
  A(10) = cont_VV(wf(:,4),wf(:,20)) * den(11)
  A(11) = cont_QA(wf(:,22),wf(:,23)) * den(14)
  A(12) = cont_VV(wf(:,7),wf(:,20)) * den(16)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(12)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(9)-A(10)-A(11)-A(12))*f(2))/2._/**/REALKIND+((A(3)+A(4))*f(3))/2._/**/REALKIND+(A(7)*f(4))/2._/**/REALKIND &
       +(A(5)*f(5))/2._/**/REALKIND+((A(6)+A(8))*f(6))/2._/**/REALKIND
  M2(2) = ((A(9)+A(10)+A(11)+A(12))*f(2))/6._/**/REALKIND+((-A(3)-A(4))*f(3))/6._/**/REALKIND-(A(7)*f(4))/6._/**/REALKIND &
       -(A(5)*f(5))/6._/**/REALKIND+((-A(6)-A(8))*f(6))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwjj_ckm_cxdbbxwx_1_/**/REALKIND
