
module ol_colourmatrix_ppllll_nenmnmxexuxd_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,1)
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
  K1( 7,:) = [  0]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [  0]
  K1(11,:) = [  0]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  4]
  K1(17,:) = [  0]
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [ -4]
  K1(22,:) = [  4]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllll_nenmnmxexuxd_1_/**/REALKIND



module ol_forced_parameters_ppllll_nenmnmxexuxd_1_/**/REALKIND
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
  if (MM /= 0) write(*,101) 'MM = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllll_nenmnmxexuxd_1_/**/REALKIND

module ol_loop_ppllll_nenmnmxexuxd_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:46)
  ! denominators
  complex(REALKIND), save :: den(37)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,64)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f( 2) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*cw*eQED**4)/(2._/**/REALKIND*sw**3)
    f( 4) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f( 5) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f( 6) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 8) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f( 9) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(10) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)

  c = [ 4*f(8), 4*f(9), 4*f(10) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(16)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-2),wf(:,2))
  call vert_QA_W(wf(:,-5),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,9),MW,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,6),MZ,1_intkind1,wf(:,5))
  call prop_W_W(wf(:,3),Q(:,48),MW,1_intkind1,wf(:,6))
  call vert_UV_W(wf(:,5),Q(:,6),wf(:,4),Q(:,9),wf(:,7))
  call vert_AW_Q(wf(:,-4),wf(:,4),wf(:,8))
  call vert_ZQ_A(gZd,wf(:,5),wf(:,-5),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,25),ZERO,0_intkind1,wf(:,10))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,5),wf(:,11))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,22),ZERO,0_intkind1,wf(:,13))
  call vert_ZQ_A(gZn,wf(:,5),wf(:,0),wf(:,14))
  call vert_AW_Q(wf(:,-3),wf(:,6),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,16))
  call vert_WQ_A(wf(:,6),wf(:,0),wf(:,17))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,5),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,49),ZERO,0_intkind1,wf(:,19))
  call vert_WQ_A(wf(:,6),wf(:,-1),wf(:,20))
  call vert_AW_Q(wf(:,-2),wf(:,4),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,50),ZERO,0_intkind1,wf(:,22))
  call counter_ZQ_A(gZd,wf(:,5),wf(:,-5),wf(:,23))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,24))
  call counter_AZ_Q(gZu,wf(:,-4),wf(:,5),wf(:,25))
  call prop_Q_A(wf(:,12),Q(:,41),ZERO,0_intkind1,wf(:,26))
  call counter_AW_Q(wf(:,-4),wf(:,4),wf(:,27))
  call prop_Q_A(wf(:,9),Q(:,38),ZERO,0_intkind1,wf(:,28))
  call counter_QA_W(wf(:,-5),wf(:,-4),wf(:,29))
  call prop_W_W(wf(:,29),Q(:,48),MW,1_intkind1,wf(:,30))
  call vert_AW_Q(wf(:,-3),wf(:,30),wf(:,31))
  call vert_WQ_A(wf(:,30),wf(:,0),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,49),ZERO,0_intkind1,wf(:,33))
  call vert_WQ_A(wf(:,30),wf(:,-1),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,50),ZERO,0_intkind1,wf(:,35))
  call counter_A_Q(ctqq,wf(:,10),Q(:,25),wf(:,36))
  call counter_A_Q(ctqq,wf(:,13),Q(:,22),wf(:,37))
  call prop_W_W(wf(:,7),Q(:,15),MW,1_intkind1,wf(:,38))
  call vert_QA_W(wf(:,16),wf(:,-3),wf(:,39))
  call prop_W_W(wf(:,39),Q(:,15),MW,1_intkind1,wf(:,40))
  call prop_A_Q(wf(:,18),Q(:,14),ZERO,0_intkind1,wf(:,41))
  call vert_QA_W(wf(:,0),wf(:,41),wf(:,42))
  call prop_W_W(wf(:,42),Q(:,15),MW,1_intkind1,wf(:,43))
  call prop_A_Q(wf(:,21),Q(:,13),ZERO,0_intkind1,wf(:,44))
  call vert_QA_W(wf(:,-1),wf(:,44),wf(:,45))
  call prop_W_W(wf(:,45),Q(:,15),MW,1_intkind1,wf(:,46))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,9) - MW2)
  den(2) = 1 / (Q(5,6) - MZ2)
  den(3) = 1 / (Q(5,48) - MW2)
  den(6) = 1 / (Q(5,25))
  den(9) = 1 / (Q(5,22))
  den(12) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,49))
  den(18) = 1 / (Q(5,50))
  den(21) = 1 / (Q(5,41))
  den(24) = 1 / (Q(5,38))
  den(29) = 1 / (Q(5,15) - MW2)
  den(32) = 1 / (Q(5,14))
  den(35) = 1 / (Q(5,13))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(3)*den(13)
  den(16) = den(3)*den(15)
  den(17) = den(2)*den(16)
  den(19) = den(3)*den(18)
  den(20) = den(1)*den(19)
  den(22) = den(1)*den(21)
  den(23) = den(2)*den(22)
  den(25) = den(2)*den(24)
  den(26) = den(1)*den(25)
  den(27) = den(7)*den(25)
  den(28) = den(10)*den(22)
  den(30) = den(4)*den(29)
  den(31) = den(13)*den(29)
  den(33) = den(2)*den(32)
  den(34) = den(29)*den(33)
  den(36) = den(1)*den(35)
  den(37) = den(29)*den(36)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(16)

  A(1) = cont_VV(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(14)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(6) = cont_QA(wf(:,21),wf(:,22)) * den(20)

  A(7) = cont_QA(wf(:,10),wf(:,23)) * den(8)
  A(8) = cont_QA(wf(:,13),wf(:,24)) * den(11)
  A(9) = cont_QA(wf(:,25),wf(:,26)) * den(23)
  A(10) = cont_QA(wf(:,27),wf(:,28)) * den(26)
  A(11) = cont_VV(wf(:,7),wf(:,30)) * den(5)
  A(12) = cont_QA(wf(:,16),wf(:,31)) * den(14)
  A(13) = cont_QA(wf(:,18),wf(:,33)) * den(17)
  A(14) = cont_QA(wf(:,21),wf(:,35)) * den(20)
  A(15) = cont_QA(wf(:,28),wf(:,36)) * den(27)
  A(16) = cont_QA(wf(:,26),wf(:,37)) * den(28)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(16)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = -(A(6)*f(1))-A(1)*f(3)+(-A(2)-A(3)-A(4)-A(5))*f(5)

  M2(1) = -(A(14)*f(2))-A(11)*f(4)+(A(15)+A(16))*f(6)+(-A(7)-A(8)-A(9)-A(10)-A(12)-A(13))*f(7)

end subroutine colourvectors

end module ol_loop_ppllll_nenmnmxexuxd_1_/**/REALKIND
