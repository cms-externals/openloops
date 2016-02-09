
module ol_colourmatrix_ppajj_ddxbbxa_1_/**/REALKIND
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
end module ol_colourmatrix_ppajj_ddxbbxa_1_/**/REALKIND



module ol_forced_parameters_ppajj_ddxbbxa_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppajj_ddxbbxa_1_/**/REALKIND

module ol_loop_ppajj_ddxbbxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(19)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:46)
  ! denominators
  complex(REALKIND), save :: den(33)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED*gQCD**2)/3._/**/REALKIND
    f( 2) = (CI*countertermnorm*eQED*gQCD**4)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*ctGbb*eQED*gQCD**4)/3._/**/REALKIND
    f( 4) = (CI*countertermnorm*ctGqq*eQED*gQCD**4)/3._/**/REALKIND
    f( 5) = (CI*countertermnorm*ctVbb*eQED*gQCD**4)/3._/**/REALKIND
    f( 6) = (CI*countertermnorm*ctVqq*eQED*gQCD**4)/3._/**/REALKIND
    f( 7) = (CI*eQED*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f( 8) = (eQED*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f( 9) = (eQED*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(10) = (eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (2*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(12) = (4*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND

  c = [ 9*CI*f(7), 27*CI*f(7), 18*f(8), 54*f(8), f(9), 3*f(9), 6*f(9), 8*f(9), 10*f(9), 18*f(9), 21*f(9), 24*f(9), 54*f(9) &
    , 3*f(10), 9*f(10), 3*f(11), 9*f(11), 3*f(12), 9*f(12) ]
  c = (1._/**/REALKIND / 36) * c
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(24)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,3))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,4))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,5))
  call prop_A_Q(wf(:,5),Q(:,24),MB,1_intkind1,wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,7))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,8))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,17),ZERO,0_intkind1,wf(:,10))
  call vert_QA_V(wf(:,10),wf(:,-1),wf(:,11))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,18),ZERO,0_intkind1,wf(:,13))
  call vert_QA_V(wf(:,0),wf(:,13),wf(:,14))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,15))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,24),MB,1_intkind1,wf(:,17))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,18))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,19))
  call prop_Q_A(wf(:,19),Q(:,20),MB,1_intkind1,wf(:,20))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,21))
  call counter_QA_V(wf(:,10),wf(:,-1),wf(:,22))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,18),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,0),wf(:,24),wf(:,25))
  call counter_QA_V(wf(:,0),wf(:,13),wf(:,26))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,17),ZERO,0_intkind1,wf(:,28))
  call vert_QA_V(wf(:,28),wf(:,-1),wf(:,29))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,30))
  call vert_AV_Q(wf(:,-3),wf(:,30),wf(:,31))
  call vert_VQ_A(wf(:,30),wf(:,-2),wf(:,32))
  call vert_QA_V(wf(:,3),wf(:,-3),wf(:,33))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,34))
  call counter_Q_A(ctbb,wf(:,3),Q(:,20),wf(:,35))
  call prop_A_Q(wf(:,4),Q(:,11),MB,1_intkind1,wf(:,36))
  call vert_QA_V(wf(:,-2),wf(:,6),wf(:,37))
  call counter_A_Q(ctbb,wf(:,6),Q(:,24),wf(:,38))
  call prop_Q_A(wf(:,7),Q(:,7),MB,1_intkind1,wf(:,39))
  call vert_AV_Q(wf(:,-1),wf(:,9),wf(:,40))
  call counter_Q_A(ctqq,wf(:,10),Q(:,17),wf(:,41))
  call prop_A_Q(wf(:,40),Q(:,14),ZERO,0_intkind1,wf(:,42))
  call counter_V_V(ctGG,wf(:,9),Q(:,12),wf(:,43))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,44))
  call counter_A_Q(ctqq,wf(:,13),Q(:,18),wf(:,45))
  call prop_Q_A(wf(:,44),Q(:,13),ZERO,0_intkind1,wf(:,46))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,20) - MB2)
  den(4) = 1 / (Q(5,24) - MB2)
  den(6) = 1 / (Q(5,17))
  den(7) = 1 / (Q(5,12))
  den(9) = 1 / (Q(5,18))
  den(11) = 1 / (Q(5,28))
  den(14) = 1 / (Q(5,11) - MB2)
  den(19) = 1 / (Q(5,7) - MB2)
  den(22) = 1 / (Q(5,14))
  den(25) = 1 / (Q(5,19))
  den(28) = 1 / (Q(5,13))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(12) = den(2)*den(11)
  den(13) = den(1)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(2)*den(15)
  den(17) = den(4)*den(11)
  den(18) = den(1)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(4)*den(20)
  den(23) = den(7)*den(22)
  den(24) = den(6)*den(23)
  den(26) = den(6)*den(25)
  den(27) = den(7)*den(26)
  den(29) = den(7)*den(28)
  den(30) = den(9)*den(29)
  den(31) = den(9)*den(25)
  den(32) = den(7)*den(31)
  den(33) = den(1)*den(7)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(24)

  A(1) = cont_QA(wf(:,3),wf(:,4)) * den(3)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,11)) * den(8)
  A(4) = cont_VV(wf(:,9),wf(:,14)) * den(10)

  A(5) = cont_QA(wf(:,3),wf(:,15)) * den(3)
  A(6) = cont_QA(wf(:,7),wf(:,17)) * den(5)
  A(7) = cont_QA(wf(:,6),wf(:,18)) * den(5)
  A(8) = cont_QA(wf(:,4),wf(:,20)) * den(3)
  A(9) = cont_VV(wf(:,11),wf(:,21)) * den(8)
  A(10) = cont_VV(wf(:,14),wf(:,21)) * den(10)
  A(11) = cont_VV(wf(:,9),wf(:,22)) * den(8)
  A(12) = cont_VV(wf(:,9),wf(:,25)) * den(10)
  A(13) = cont_VV(wf(:,9),wf(:,26)) * den(10)
  A(14) = cont_VV(wf(:,9),wf(:,29)) * den(8)
  A(15) = cont_QA(wf(:,3),wf(:,31)) * den(3)
  A(16) = cont_QA(wf(:,6),wf(:,32)) * den(5)
  A(17) = cont_VV(wf(:,33),wf(:,34)) * den(13)
  A(18) = cont_QA(wf(:,35),wf(:,36)) * den(16)
  A(19) = cont_VV(wf(:,34),wf(:,37)) * den(18)
  A(20) = cont_QA(wf(:,38),wf(:,39)) * den(21)
  A(21) = cont_QA(wf(:,41),wf(:,42)) * den(24)
  A(22) = cont_VV(wf(:,11),wf(:,43)) * den(27)
  A(23) = cont_QA(wf(:,45),wf(:,46)) * den(30)
  A(24) = cont_VV(wf(:,14),wf(:,43)) * den(32)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(24)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(2)-A(3)-A(4))*f(1))/2._/**/REALKIND
  M1(2) = ((A(1)+A(2)+A(3)+A(4))*f(1))/6._/**/REALKIND

  M2(1) = ((A(17)+A(18)+A(19)+A(20)+A(21)+A(22)+A(23)+A(24))*f(2))/2._/**/REALKIND+((-A(5)-A(7)-A(9)-A(10))*f(3))/2._/**/REALKIND &
       +((-A(11)-A(13)-A(15)-A(16))*f(4))/2._/**/REALKIND+((-A(6)-A(8))*f(5))/2._/**/REALKIND+((-A(12) &
       -A(14))*f(6))/2._/**/REALKIND
  M2(2) = ((-A(17)-A(18)-A(19)-A(20)-A(21)-A(22)-A(23)-A(24))*f(2))/6._/**/REALKIND+((A(5)+A(7)+A(9)+A(10))*f(3))/6._/**/REALKIND &
       +((A(11)+A(13)+A(15)+A(16))*f(4))/6._/**/REALKIND+((A(6)+A(8))*f(5))/6._/**/REALKIND+((A(12)+A(14))*f(6))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppajj_ddxbbxa_1_/**/REALKIND
