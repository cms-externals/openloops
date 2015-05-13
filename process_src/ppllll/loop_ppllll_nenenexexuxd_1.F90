
module ol_colourmatrix_ppllll_nenenexexuxd_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll_nenenexexuxd_1_/**/REALKIND



module ol_forced_parameters_ppllll_nenenexexuxd_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllll_nenenexexuxd_1_/**/REALKIND

module ol_loop_ppllll_nenenexexuxd_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:78)
  ! denominators
  complex(REALKIND), save :: den(65)
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
  complex(REALKIND) :: A(32)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_W(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_QA_W(wf(:,-5),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MW,1_intkind1,wf(:,5))
  call prop_W_W(wf(:,3),Q(:,48),MW,1_intkind1,wf(:,6))
  call vert_UV_W(wf(:,4),Q(:,5),wf(:,5),Q(:,10),wf(:,7))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,4),wf(:,8))
  call vert_WQ_A(wf(:,5),wf(:,-5),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,21),ZERO,0_intkind1,wf(:,10))
  call vert_AW_Q(wf(:,-4),wf(:,5),wf(:,11))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-5),wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,26),ZERO,0_intkind1,wf(:,13))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,-1),wf(:,14))
  call vert_AW_Q(wf(:,-3),wf(:,6),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,16))
  call vert_WQ_A(wf(:,6),wf(:,-1),wf(:,17))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,50),ZERO,0_intkind1,wf(:,19))
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,20))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-2),wf(:,21))
  call prop_W_W(wf(:,20),Q(:,9),MW,1_intkind1,wf(:,22))
  call prop_W_W(wf(:,21),Q(:,6),MZ,1_intkind1,wf(:,23))
  call vert_UV_W(wf(:,23),Q(:,6),wf(:,22),Q(:,9),wf(:,24))
  call vert_AW_Q(wf(:,-4),wf(:,22),wf(:,25))
  call vert_ZQ_A(gZd,wf(:,23),wf(:,-5),wf(:,26))
  call prop_A_Q(wf(:,25),Q(:,25),ZERO,0_intkind1,wf(:,27))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,23),wf(:,28))
  call vert_WQ_A(wf(:,22),wf(:,-5),wf(:,29))
  call prop_A_Q(wf(:,28),Q(:,22),ZERO,0_intkind1,wf(:,30))
  call vert_ZQ_A(gZn,wf(:,23),wf(:,0),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,7),ZERO,0_intkind1,wf(:,32))
  call vert_WQ_A(wf(:,6),wf(:,0),wf(:,33))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,23),wf(:,34))
  call prop_Q_A(wf(:,33),Q(:,49),ZERO,0_intkind1,wf(:,35))
  call vert_AW_Q(wf(:,-2),wf(:,22),wf(:,36))
  call vert_AW_Q(wf(:,-2),wf(:,5),wf(:,37))
  call counter_WQ_A(wf(:,5),wf(:,-5),wf(:,38))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-5),wf(:,39))
  call counter_AW_Q(wf(:,-4),wf(:,5),wf(:,40))
  call prop_Q_A(wf(:,12),Q(:,37),ZERO,0_intkind1,wf(:,41))
  call counter_AZ_Q(gZu,wf(:,-4),wf(:,4),wf(:,42))
  call prop_Q_A(wf(:,9),Q(:,42),ZERO,0_intkind1,wf(:,43))
  call counter_QA_W(wf(:,-5),wf(:,-4),wf(:,44))
  call prop_W_W(wf(:,44),Q(:,48),MW,1_intkind1,wf(:,45))
  call vert_AW_Q(wf(:,-3),wf(:,45),wf(:,46))
  call vert_WQ_A(wf(:,45),wf(:,-1),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,50),ZERO,0_intkind1,wf(:,48))
  call counter_ZQ_A(gZd,wf(:,23),wf(:,-5),wf(:,49))
  call counter_WQ_A(wf(:,22),wf(:,-5),wf(:,50))
  call counter_AZ_Q(gZu,wf(:,-4),wf(:,23),wf(:,51))
  call prop_Q_A(wf(:,29),Q(:,41),ZERO,0_intkind1,wf(:,52))
  call counter_AW_Q(wf(:,-4),wf(:,22),wf(:,53))
  call prop_Q_A(wf(:,26),Q(:,38),ZERO,0_intkind1,wf(:,54))
  call vert_WQ_A(wf(:,45),wf(:,0),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,49),ZERO,0_intkind1,wf(:,56))
  call counter_A_Q(ctqq,wf(:,10),Q(:,21),wf(:,57))
  call counter_A_Q(ctqq,wf(:,13),Q(:,26),wf(:,58))
  call counter_A_Q(ctqq,wf(:,27),Q(:,25),wf(:,59))
  call counter_A_Q(ctqq,wf(:,30),Q(:,22),wf(:,60))
  call prop_W_W(wf(:,7),Q(:,15),MW,1_intkind1,wf(:,61))
  call vert_QA_W(wf(:,16),wf(:,-3),wf(:,62))
  call prop_W_W(wf(:,62),Q(:,15),MW,1_intkind1,wf(:,63))
  call prop_A_Q(wf(:,18),Q(:,13),ZERO,0_intkind1,wf(:,64))
  call vert_QA_W(wf(:,-1),wf(:,64),wf(:,65))
  call prop_W_W(wf(:,65),Q(:,15),MW,1_intkind1,wf(:,66))
  call prop_W_W(wf(:,24),Q(:,15),MW,1_intkind1,wf(:,67))
  call vert_QA_W(wf(:,32),wf(:,-3),wf(:,68))
  call prop_W_W(wf(:,68),Q(:,15),MW,1_intkind1,wf(:,69))
  call prop_A_Q(wf(:,34),Q(:,14),ZERO,0_intkind1,wf(:,70))
  call vert_QA_W(wf(:,0),wf(:,70),wf(:,71))
  call prop_W_W(wf(:,71),Q(:,15),MW,1_intkind1,wf(:,72))
  call prop_A_Q(wf(:,36),Q(:,13),ZERO,0_intkind1,wf(:,73))
  call vert_QA_W(wf(:,-1),wf(:,73),wf(:,74))
  call prop_W_W(wf(:,74),Q(:,15),MW,1_intkind1,wf(:,75))
  call prop_A_Q(wf(:,37),Q(:,14),ZERO,0_intkind1,wf(:,76))
  call vert_QA_W(wf(:,0),wf(:,76),wf(:,77))
  call prop_W_W(wf(:,77),Q(:,15),MW,1_intkind1,wf(:,78))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MZ2)
  den(2) = 1 / (Q(5,10) - MW2)
  den(3) = 1 / (Q(5,48) - MW2)
  den(6) = 1 / (Q(5,21))
  den(9) = 1 / (Q(5,26))
  den(12) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,50))
  den(18) = 1 / (Q(5,9) - MW2)
  den(19) = 1 / (Q(5,6) - MZ2)
  den(22) = 1 / (Q(5,25))
  den(25) = 1 / (Q(5,22))
  den(30) = 1 / (Q(5,49))
  den(35) = 1 / (Q(5,37))
  den(38) = 1 / (Q(5,42))
  den(41) = 1 / (Q(5,41))
  den(44) = 1 / (Q(5,38))
  den(51) = 1 / (Q(5,15) - MW2)
  den(54) = 1 / (Q(5,13))
  den(59) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(13) = den(1)*den(12)
  den(14) = den(3)*den(13)
  den(16) = den(3)*den(15)
  den(17) = den(1)*den(16)
  den(20) = den(18)*den(19)
  den(21) = den(3)*den(20)
  den(23) = den(18)*den(22)
  den(24) = den(19)*den(23)
  den(26) = den(19)*den(25)
  den(27) = den(18)*den(26)
  den(28) = den(12)*den(19)
  den(29) = den(3)*den(28)
  den(31) = den(3)*den(30)
  den(32) = den(19)*den(31)
  den(33) = den(16)*den(18)
  den(34) = den(2)*den(31)
  den(36) = den(1)*den(35)
  den(37) = den(2)*den(36)
  den(39) = den(2)*den(38)
  den(40) = den(1)*den(39)
  den(42) = den(18)*den(41)
  den(43) = den(19)*den(42)
  den(45) = den(19)*den(44)
  den(46) = den(18)*den(45)
  den(47) = den(7)*den(39)
  den(48) = den(10)*den(36)
  den(49) = den(23)*den(45)
  den(50) = den(26)*den(42)
  den(52) = den(4)*den(51)
  den(53) = den(13)*den(51)
  den(55) = den(1)*den(54)
  den(56) = den(51)*den(55)
  den(57) = den(20)*den(51)
  den(58) = den(28)*den(51)
  den(60) = den(19)*den(59)
  den(61) = den(51)*den(60)
  den(62) = den(18)*den(54)
  den(63) = den(51)*den(62)
  den(64) = den(2)*den(59)
  den(65) = den(51)*den(64)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(32)

  A(1) = cont_VV(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(14)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(6) = cont_VV(wf(:,6),wf(:,24)) * den(21)
  A(7) = cont_QA(wf(:,26),wf(:,27)) * den(24)
  A(8) = cont_QA(wf(:,29),wf(:,30)) * den(27)
  A(9) = cont_QA(wf(:,15),wf(:,32)) * den(29)
  A(10) = cont_QA(wf(:,34),wf(:,35)) * den(32)
  A(11) = cont_QA(wf(:,19),wf(:,36)) * den(33)
  A(12) = cont_QA(wf(:,35),wf(:,37)) * den(34)

  A(13) = cont_QA(wf(:,10),wf(:,38)) * den(8)
  A(14) = cont_QA(wf(:,13),wf(:,39)) * den(11)
  A(15) = cont_QA(wf(:,40),wf(:,41)) * den(37)
  A(16) = cont_QA(wf(:,42),wf(:,43)) * den(40)
  A(17) = cont_VV(wf(:,7),wf(:,45)) * den(5)
  A(18) = cont_QA(wf(:,16),wf(:,46)) * den(14)
  A(19) = cont_QA(wf(:,18),wf(:,48)) * den(17)
  A(20) = cont_QA(wf(:,27),wf(:,49)) * den(24)
  A(21) = cont_QA(wf(:,30),wf(:,50)) * den(27)
  A(22) = cont_QA(wf(:,51),wf(:,52)) * den(43)
  A(23) = cont_QA(wf(:,53),wf(:,54)) * den(46)
  A(24) = cont_VV(wf(:,24),wf(:,45)) * den(21)
  A(25) = cont_QA(wf(:,32),wf(:,46)) * den(29)
  A(26) = cont_QA(wf(:,34),wf(:,56)) * den(32)
  A(27) = cont_QA(wf(:,36),wf(:,48)) * den(33)
  A(28) = cont_QA(wf(:,37),wf(:,56)) * den(34)
  A(29) = cont_QA(wf(:,43),wf(:,57)) * den(47)
  A(30) = cont_QA(wf(:,41),wf(:,58)) * den(48)
  A(31) = cont_QA(wf(:,54),wf(:,59)) * den(49)
  A(32) = cont_QA(wf(:,52),wf(:,60)) * den(50)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(32)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(11)+A(12))*f(1)+(A(1)-A(6))*f(3)+(A(2)+A(3)+A(4)+A(5)-A(7)-A(8)-A(9)-A(10))*f(5)

  M2(1) = (-A(27)+A(28))*f(2)+(A(17)-A(24))*f(4)+(-A(29)-A(30)+A(31)+A(32))*f(6)+(A(13)+A(14)+A(15)+A(16)+A(18)+A(19)-A(20)-A(21) &
       -A(22)-A(23)-A(25)-A(26))*f(7)

end subroutine colourvectors

end module ol_loop_ppllll_nenenexexuxd_1_/**/REALKIND
