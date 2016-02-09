
module ol_colourmatrix_pplla_eexbbxa_1_/**/REALKIND
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

  K1( 1,:) = [  3]
  K1( 2,:) = [  0]
  K1( 3,:) = [  0]
  K1( 4,:) = [  0]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  4]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [ -4]
  K1(11,:) = [  4]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  0]
  K1(17,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplla_eexbbxa_1_/**/REALKIND



module ol_forced_parameters_pplla_eexbbxa_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplla_eexbbxa_1_/**/REALKIND

module ol_loop_pplla_eexbbxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:41)
  ! denominators
  complex(REALKIND), save :: den(32)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**3)/9._/**/REALKIND
    f( 2) = (CI*eQED**3)/3._/**/REALKIND
    f( 3) = CI*eQED**3
    f( 4) = (CI*countertermnorm*eQED**3*gQCD**2)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*eQED**3*gQCD**2)/3._/**/REALKIND
    f( 6) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/9._/**/REALKIND
    f( 7) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/3._/**/REALKIND
    f( 8) = CI*countertermnorm*ctVbb*eQED**3*gQCD**2
    f( 9) = (eQED**3*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(10) = (eQED**3*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(11) = eQED**3*gQCD**2*integralnorm*SwB

  c = [ 4*f(9), 4*f(10), 4*f(11) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
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
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,3),MZ,1_intkind1,wf(:,6))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,6),wf(:,7))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,8))
  call prop_A_Q(wf(:,8),Q(:,24),MB,1_intkind1,wf(:,9))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,10))
  call vert_ZQ_A(gZd,wf(:,6),wf(:,-2),wf(:,11))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,12))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,17),ZERO,0_intkind1,wf(:,14))
  call vert_QA_V(wf(:,14),wf(:,-1),wf(:,15))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,16))
  call prop_W_W(wf(:,16),Q(:,12),MZ,1_intkind1,wf(:,17))
  call vert_QA_Z(gZl,wf(:,14),wf(:,-1),wf(:,18))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,19))
  call prop_A_Q(wf(:,19),Q(:,18),ZERO,0_intkind1,wf(:,20))
  call vert_QA_V(wf(:,0),wf(:,20),wf(:,21))
  call vert_QA_Z(gZl,wf(:,0),wf(:,20),wf(:,22))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,23))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,6),wf(:,24))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,25))
  call prop_A_Q(wf(:,25),Q(:,24),MB,1_intkind1,wf(:,26))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,27))
  call counter_ZQ_A(gZd,wf(:,6),wf(:,-2),wf(:,28))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,20),MB,1_intkind1,wf(:,30))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,31))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,32))
  call prop_W_W(wf(:,32),Q(:,12),MZ,1_intkind1,wf(:,33))
  call counter_Q_A(ctbb,wf(:,3),Q(:,20),wf(:,34))
  call prop_A_Q(wf(:,4),Q(:,11),MB,1_intkind1,wf(:,35))
  call prop_A_Q(wf(:,7),Q(:,11),MB,1_intkind1,wf(:,36))
  call counter_A_Q(ctbb,wf(:,9),Q(:,24),wf(:,37))
  call prop_Q_A(wf(:,10),Q(:,7),MB,1_intkind1,wf(:,38))
  call prop_Q_A(wf(:,11),Q(:,7),MB,1_intkind1,wf(:,39))
  call prop_W_W(wf(:,18),Q(:,19),MZ,1_intkind1,wf(:,40))
  call prop_W_W(wf(:,22),Q(:,19),MZ,1_intkind1,wf(:,41))

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
  den(4) = 1 / (Q(5,3) - MZ2)
  den(6) = 1 / (Q(5,24) - MB2)
  den(9) = 1 / (Q(5,17))
  den(10) = 1 / (Q(5,12))
  den(12) = 1 / (Q(5,12) - MZ2)
  den(14) = 1 / (Q(5,18))
  den(17) = 1 / (Q(5,11) - MB2)
  den(22) = 1 / (Q(5,7) - MB2)
  den(27) = 1 / (Q(5,19))
  den(29) = 1 / (Q(5,19) - MZ2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(4)*den(6)
  den(11) = den(9)*den(10)
  den(13) = den(9)*den(12)
  den(15) = den(10)*den(14)
  den(16) = den(12)*den(14)
  den(18) = den(1)*den(17)
  den(19) = den(2)*den(18)
  den(20) = den(4)*den(17)
  den(21) = den(2)*den(20)
  den(23) = den(1)*den(22)
  den(24) = den(6)*den(23)
  den(25) = den(4)*den(22)
  den(26) = den(6)*den(25)
  den(28) = den(9)*den(27)
  den(30) = den(9)*den(29)
  den(31) = den(14)*den(27)
  den(32) = den(14)*den(29)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(24)

  A(1) = cont_QA(wf(:,3),wf(:,4)) * den(3)
  A(2) = cont_QA(wf(:,3),wf(:,7)) * den(5)
  A(3) = cont_QA(wf(:,9),wf(:,10)) * den(7)
  A(4) = cont_QA(wf(:,9),wf(:,11)) * den(8)
  A(5) = cont_VV(wf(:,13),wf(:,15)) * den(11)
  A(6) = cont_VV(wf(:,17),wf(:,18)) * den(13)
  A(7) = cont_VV(wf(:,13),wf(:,21)) * den(15)
  A(8) = cont_VV(wf(:,17),wf(:,22)) * den(16)

  A(9) = cont_QA(wf(:,3),wf(:,23)) * den(3)
  A(10) = cont_QA(wf(:,3),wf(:,24)) * den(5)
  A(11) = cont_QA(wf(:,10),wf(:,26)) * den(7)
  A(12) = cont_QA(wf(:,11),wf(:,26)) * den(8)
  A(13) = cont_QA(wf(:,9),wf(:,27)) * den(7)
  A(14) = cont_QA(wf(:,9),wf(:,28)) * den(8)
  A(15) = cont_QA(wf(:,4),wf(:,30)) * den(3)
  A(16) = cont_QA(wf(:,7),wf(:,30)) * den(5)
  A(17) = cont_VV(wf(:,15),wf(:,31)) * den(11)
  A(18) = cont_VV(wf(:,18),wf(:,33)) * den(13)
  A(19) = cont_VV(wf(:,21),wf(:,31)) * den(15)
  A(20) = cont_VV(wf(:,22),wf(:,33)) * den(16)
  A(21) = cont_QA(wf(:,34),wf(:,35)) * den(19)
  A(22) = cont_QA(wf(:,34),wf(:,36)) * den(21)
  A(23) = cont_QA(wf(:,37),wf(:,38)) * den(24)
  A(24) = cont_QA(wf(:,37),wf(:,39)) * den(26)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(24)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(3))*f(1)+(-A(2)-A(4)-A(5)-A(7))*f(2)+(-A(6)-A(8))*f(3)

  M2(1) = (A(21)+A(23))*f(4)+(A(22)+A(24))*f(5)+(-A(9)-A(11)-A(13)-A(15))*f(6)+(-A(10)-A(12)-A(14)-A(16)-A(17)-A(19))*f(7)+(-A(18) &
       -A(20))*f(8)

end subroutine colourvectors

end module ol_loop_pplla_eexbbxa_1_/**/REALKIND
