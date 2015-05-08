
module ol_colourmatrix_ppllll_nmnmxeexddx_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll_nmnmxeexddx_1_/**/REALKIND



module ol_forced_parameters_ppllll_nmnmxeexddx_1_/**/REALKIND
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
end module ol_forced_parameters_ppllll_nmnmxeexddx_1_/**/REALKIND

module ol_loop_ppllll_nmnmxeexddx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(8), c(2)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:68)
  ! denominators
  complex(REALKIND), save :: den(56)
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
    f(1) = (CI*eQED**4)/3._/**/REALKIND
    f(2) = CI*eQED**4
    f(3) = (CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f(4) = CI*countertermnorm*eQED**4*gQCD**2
    f(5) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/3._/**/REALKIND
    f(6) = CI*countertermnorm*ctVqq*eQED**4*gQCD**2
    f(7) = (eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(8) = eQED**4*gQCD**2*integralnorm*SwB

  c = [ 4*f(7), 4*f(8) ]
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
  complex(REALKIND) :: A(28)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,4))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,19),ZERO,0_intkind1,wf(:,6))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,12),MZ,1_intkind1,wf(:,8))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,8),wf(:,9))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,28),ZERO,0_intkind1,wf(:,12))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,-4),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,28),ZERO,0_intkind1,wf(:,14))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,15))
  call vert_ZQ_A(gZl,wf(:,3),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,-3),wf(:,15),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,19))
  call prop_W_W(wf(:,19),Q(:,48),MZ,1_intkind1,wf(:,20))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,20),wf(:,21))
  call vert_VQ_A(wf(:,15),wf(:,-2),wf(:,22))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,3),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,52),ZERO,0_intkind1,wf(:,24))
  call vert_ZQ_A(gZl,wf(:,20),wf(:,-2),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,52),ZERO,0_intkind1,wf(:,26))
  call vert_ZQ_A(gZn,wf(:,8),wf(:,0),wf(:,27))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,20),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,13),ZERO,0_intkind1,wf(:,29))
  call vert_ZQ_A(gZn,wf(:,20),wf(:,0),wf(:,30))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,8),wf(:,31))
  call prop_Q_A(wf(:,30),Q(:,49),ZERO,0_intkind1,wf(:,32))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,33))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,8),wf(:,34))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,35))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,36))
  call prop_A_Q(wf(:,11),Q(:,35),ZERO,0_intkind1,wf(:,37))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,-4),wf(:,38))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,39))
  call prop_A_Q(wf(:,5),Q(:,44),ZERO,0_intkind1,wf(:,40))
  call prop_A_Q(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,41))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,42))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,43))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,44))
  call prop_W_W(wf(:,44),Q(:,48),MZ,1_intkind1,wf(:,45))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,45),wf(:,46))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,52),ZERO,0_intkind1,wf(:,48))
  call vert_ZQ_A(gZl,wf(:,45),wf(:,-2),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,52),ZERO,0_intkind1,wf(:,50))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,45),wf(:,51))
  call vert_ZQ_A(gZn,wf(:,45),wf(:,0),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,49),ZERO,0_intkind1,wf(:,53))
  call counter_Q_A(ctqq,wf(:,6),Q(:,19),wf(:,54))
  call counter_Q_A(ctqq,wf(:,12),Q(:,28),wf(:,55))
  call counter_Q_A(ctqq,wf(:,14),Q(:,28),wf(:,56))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,57))
  call vert_QA_Z(gZl,wf(:,18),wf(:,-3),wf(:,58))
  call prop_W_W(wf(:,58),Q(:,15),MZ,1_intkind1,wf(:,59))
  call prop_A_Q(wf(:,23),Q(:,11),ZERO,0_intkind1,wf(:,60))
  call vert_QA_V(wf(:,-2),wf(:,60),wf(:,61))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,60),wf(:,62))
  call prop_W_W(wf(:,62),Q(:,15),MZ,1_intkind1,wf(:,63))
  call vert_QA_Z(gZn,wf(:,29),wf(:,-1),wf(:,64))
  call prop_W_W(wf(:,64),Q(:,15),MZ,1_intkind1,wf(:,65))
  call prop_A_Q(wf(:,31),Q(:,14),ZERO,0_intkind1,wf(:,66))
  call vert_QA_Z(gZn,wf(:,0),wf(:,66),wf(:,67))
  call prop_W_W(wf(:,67),Q(:,15),MZ,1_intkind1,wf(:,68))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,19))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(8) = 1 / (Q(5,28))
  den(13) = 1 / (Q(5,48))
  den(14) = 1 / (Q(5,7))
  den(17) = 1 / (Q(5,48) - MZ2)
  den(19) = 1 / (Q(5,52))
  den(24) = 1 / (Q(5,13))
  den(27) = 1 / (Q(5,49))
  den(30) = 1 / (Q(5,35))
  den(34) = 1 / (Q(5,44))
  den(43) = 1 / (Q(5,15))
  den(45) = 1 / (Q(5,15) - MZ2)
  den(47) = 1 / (Q(5,11))
  den(52) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(2)*den(8)
  den(10) = den(1)*den(9)
  den(11) = den(6)*den(8)
  den(12) = den(1)*den(11)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(18) = den(15)*den(17)
  den(20) = den(13)*den(19)
  den(21) = den(1)*den(20)
  den(22) = den(17)*den(19)
  den(23) = den(1)*den(22)
  den(25) = den(6)*den(24)
  den(26) = den(17)*den(25)
  den(28) = den(17)*den(27)
  den(29) = den(6)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(2)*den(31)
  den(33) = den(6)*den(31)
  den(35) = den(2)*den(34)
  den(36) = den(1)*den(35)
  den(37) = den(6)*den(34)
  den(38) = den(1)*den(37)
  den(39) = den(4)*den(35)
  den(40) = den(4)*den(37)
  den(41) = den(9)*den(31)
  den(42) = den(11)*den(31)
  den(44) = den(15)*den(43)
  den(46) = den(15)*den(45)
  den(48) = den(1)*den(47)
  den(49) = den(43)*den(48)
  den(50) = den(45)*den(48)
  den(51) = den(25)*den(45)
  den(53) = den(6)*den(52)
  den(54) = den(45)*den(53)
  den(55) = den(1)*den(2)
  den(56) = den(1)*den(6)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(28)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,6),wf(:,9)) * den(7)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(10)
  A(4) = cont_QA(wf(:,11),wf(:,14)) * den(12)
  A(5) = cont_QA(wf(:,17),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,18),wf(:,21)) * den(18)
  A(7) = cont_QA(wf(:,23),wf(:,24)) * den(21)
  A(8) = cont_QA(wf(:,23),wf(:,26)) * den(23)
  A(9) = cont_QA(wf(:,28),wf(:,29)) * den(26)
  A(10) = cont_QA(wf(:,31),wf(:,32)) * den(29)

  A(11) = cont_QA(wf(:,6),wf(:,33)) * den(5)
  A(12) = cont_QA(wf(:,6),wf(:,34)) * den(7)
  A(13) = cont_QA(wf(:,12),wf(:,35)) * den(10)
  A(14) = cont_QA(wf(:,14),wf(:,35)) * den(12)
  A(15) = cont_QA(wf(:,36),wf(:,37)) * den(32)
  A(16) = cont_QA(wf(:,37),wf(:,38)) * den(33)
  A(17) = cont_QA(wf(:,39),wf(:,40)) * den(36)
  A(18) = cont_QA(wf(:,39),wf(:,41)) * den(38)
  A(19) = cont_QA(wf(:,18),wf(:,43)) * den(16)
  A(20) = cont_QA(wf(:,18),wf(:,46)) * den(18)
  A(21) = cont_QA(wf(:,23),wf(:,48)) * den(21)
  A(22) = cont_QA(wf(:,23),wf(:,50)) * den(23)
  A(23) = cont_QA(wf(:,29),wf(:,51)) * den(26)
  A(24) = cont_QA(wf(:,31),wf(:,53)) * den(29)
  A(25) = cont_QA(wf(:,40),wf(:,54)) * den(39)
  A(26) = cont_QA(wf(:,41),wf(:,54)) * den(40)
  A(27) = cont_QA(wf(:,37),wf(:,55)) * den(41)
  A(28) = cont_QA(wf(:,37),wf(:,56)) * den(42)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(28)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(3)+A(5)+A(7))*f(1)+(A(2)+A(4)+A(6)+A(8)+A(9)+A(10))*f(2)

  M2(1) = (-A(25)-A(27))*f(3)+(-A(26)-A(28))*f(4)+(A(11)+A(13)+A(15)+A(17)+A(19)+A(21))*f(5)+(A(12)+A(14)+A(16)+A(18)+A(20)+A(22) &
       +A(23)+A(24))*f(6)

end subroutine colourvectors

end module ol_loop_ppllll_nmnmxeexddx_1_/**/REALKIND
