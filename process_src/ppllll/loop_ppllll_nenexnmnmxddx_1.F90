
module ol_colourmatrix_ppllll_nenexnmnmxddx_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll_nenexnmnmxddx_1_/**/REALKIND



module ol_forced_parameters_ppllll_nenexnmnmxddx_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll_nenexnmnmxddx_1_/**/REALKIND

module ol_loop_ppllll_nenexnmnmxddx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(4), c(1)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:50)
  ! denominators
  complex(REALKIND), save :: den(39)
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
    f(1) = CI*eQED**4
    f(2) = CI*countertermnorm*eQED**4*gQCD**2
    f(3) = CI*countertermnorm*ctVqq*eQED**4*gQCD**2
    f(4) = eQED**4*gQCD**2*integralnorm*SwB

  c = [ 4*f(4) ]
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
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call prop_W_W(wf(:,2),Q(:,12),MZ,1_intkind1,wf(:,4))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,6))
  call prop_Q_A(wf(:,5),Q(:,19),ZERO,0_intkind1,wf(:,7))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,8))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,28),ZERO,0_intkind1,wf(:,10))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,11))
  call prop_W_W(wf(:,11),Q(:,48),MZ,1_intkind1,wf(:,12))
  call vert_ZQ_A(gZn,wf(:,3),wf(:,-2),wf(:,13))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,12),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,7),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,-2),wf(:,16))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,3),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,52),ZERO,0_intkind1,wf(:,18))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,0),wf(:,19))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,12),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,13),ZERO,0_intkind1,wf(:,21))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,0),wf(:,22))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,4),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,49),ZERO,0_intkind1,wf(:,24))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,25))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,26))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,27))
  call prop_A_Q(wf(:,9),Q(:,35),ZERO,0_intkind1,wf(:,28))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,29))
  call prop_A_Q(wf(:,6),Q(:,44),ZERO,0_intkind1,wf(:,30))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,31))
  call prop_W_W(wf(:,31),Q(:,48),MZ,1_intkind1,wf(:,32))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,32),wf(:,33))
  call vert_ZQ_A(gZn,wf(:,32),wf(:,-2),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,52),ZERO,0_intkind1,wf(:,35))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,32),wf(:,36))
  call vert_ZQ_A(gZn,wf(:,32),wf(:,0),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,49),ZERO,0_intkind1,wf(:,38))
  call counter_Q_A(ctqq,wf(:,7),Q(:,19),wf(:,39))
  call counter_Q_A(ctqq,wf(:,10),Q(:,28),wf(:,40))
  call vert_QA_Z(gZn,wf(:,15),wf(:,-3),wf(:,41))
  call prop_W_W(wf(:,41),Q(:,15),MZ,1_intkind1,wf(:,42))
  call prop_A_Q(wf(:,17),Q(:,11),ZERO,0_intkind1,wf(:,43))
  call vert_QA_Z(gZn,wf(:,-2),wf(:,43),wf(:,44))
  call prop_W_W(wf(:,44),Q(:,15),MZ,1_intkind1,wf(:,45))
  call vert_QA_Z(gZn,wf(:,21),wf(:,-1),wf(:,46))
  call prop_W_W(wf(:,46),Q(:,15),MZ,1_intkind1,wf(:,47))
  call prop_A_Q(wf(:,23),Q(:,14),ZERO,0_intkind1,wf(:,48))
  call vert_QA_Z(gZn,wf(:,0),wf(:,48),wf(:,49))
  call prop_W_W(wf(:,49),Q(:,15),MZ,1_intkind1,wf(:,50))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12) - MZ2)
  den(3) = 1 / (Q(5,19))
  den(6) = 1 / (Q(5,28))
  den(9) = 1 / (Q(5,48) - MZ2)
  den(10) = 1 / (Q(5,7))
  den(13) = 1 / (Q(5,52))
  den(16) = 1 / (Q(5,13))
  den(19) = 1 / (Q(5,49))
  den(22) = 1 / (Q(5,35))
  den(25) = 1 / (Q(5,44))
  den(30) = 1 / (Q(5,15) - MZ2)
  den(32) = 1 / (Q(5,11))
  den(36) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(1)*den(10)
  den(12) = den(9)*den(11)
  den(14) = den(9)*den(13)
  den(15) = den(1)*den(14)
  den(17) = den(2)*den(16)
  den(18) = den(9)*den(17)
  den(20) = den(9)*den(19)
  den(21) = den(2)*den(20)
  den(23) = den(1)*den(22)
  den(24) = den(2)*den(23)
  den(26) = den(2)*den(25)
  den(27) = den(1)*den(26)
  den(28) = den(4)*den(26)
  den(29) = den(7)*den(23)
  den(31) = den(11)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(30)*den(33)
  den(35) = den(17)*den(30)
  den(37) = den(2)*den(36)
  den(38) = den(30)*den(37)
  den(39) = den(1)*den(2)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(16)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,20),wf(:,21)) * den(18)
  A(6) = cont_QA(wf(:,23),wf(:,24)) * den(21)

  A(7) = cont_QA(wf(:,7),wf(:,25)) * den(5)
  A(8) = cont_QA(wf(:,10),wf(:,26)) * den(8)
  A(9) = cont_QA(wf(:,27),wf(:,28)) * den(24)
  A(10) = cont_QA(wf(:,29),wf(:,30)) * den(27)
  A(11) = cont_QA(wf(:,15),wf(:,33)) * den(12)
  A(12) = cont_QA(wf(:,17),wf(:,35)) * den(15)
  A(13) = cont_QA(wf(:,21),wf(:,36)) * den(18)
  A(14) = cont_QA(wf(:,23),wf(:,38)) * den(21)
  A(15) = cont_QA(wf(:,30),wf(:,39)) * den(28)
  A(16) = cont_QA(wf(:,28),wf(:,40)) * den(29)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(16)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(2)+A(3)+A(4)+A(5)+A(6))*f(1)

  M2(1) = (-A(15)-A(16))*f(2)+(A(7)+A(8)+A(9)+A(10)+A(11)+A(12)+A(13)+A(14))*f(3)

end subroutine colourvectors

end module ol_loop_ppllll_nenexnmnmxddx_1_/**/REALKIND
