
module ol_colourmatrix_ppajj_uuuxuxa_1_/**/REALKIND
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
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
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
end module ol_colourmatrix_ppajj_uuuxuxa_1_/**/REALKIND



module ol_forced_parameters_ppajj_uuuxuxa_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppajj_uuuxuxa_1_/**/REALKIND

module ol_loop_ppajj_uuuxuxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(19)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:72)
  ! denominators
  complex(REALKIND), save :: den(58)
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
    f( 1) = (2*CI*eQED*gQCD**2)/3._/**/REALKIND
    f( 2) = (2*CI*countertermnorm*eQED*gQCD**4)/3._/**/REALKIND
    f( 3) = (2*CI*countertermnorm*ctGqq*eQED*gQCD**4)/3._/**/REALKIND
    f( 4) = (2*CI*countertermnorm*ctVqq*eQED*gQCD**4)/3._/**/REALKIND
    f( 5) = (2*CI*eQED*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f( 6) = (eQED*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f( 7) = (2*eQED*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f( 8) = (eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = (2*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (4*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND

  c = [ 9*CI*f(5), 27*CI*f(5), 18*f(6), 54*f(6), f(7), 3*f(7), 6*f(7), 8*f(7), 10*f(7), 18*f(7), 21*f(7), 24*f(7), 54*f(7), 3*f(8) &
    , 9*f(8), 3*f(9), 9*f(9), 3*f(10), 9*f(10) ]
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
  complex(REALKIND) :: A(48)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,2))
  call prop_Q_A(wf(:,2),Q(:,18),ZERO,0_intkind1,wf(:,3))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,4))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,5))
  call prop_A_Q(wf(:,5),Q(:,24),ZERO,0_intkind1,wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,7))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,8))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,17),ZERO,0_intkind1,wf(:,10))
  call vert_QA_V(wf(:,10),wf(:,-3),wf(:,11))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,12))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,13))
  call vert_AV_Q(wf(:,-2),wf(:,13),wf(:,14))
  call vert_AV_Q(wf(:,-2),wf(:,-4),wf(:,15))
  call prop_A_Q(wf(:,15),Q(:,20),ZERO,0_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,13),wf(:,-1),wf(:,17))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,18))
  call vert_QA_V(wf(:,10),wf(:,-2),wf(:,19))
  call vert_VQ_A(wf(:,18),wf(:,0),wf(:,20))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,21))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,22))
  call prop_A_Q(wf(:,22),Q(:,24),ZERO,0_intkind1,wf(:,23))
  call counter_QA_V(wf(:,10),wf(:,-3),wf(:,24))
  call counter_AV_Q(wf(:,-2),wf(:,13),wf(:,25))
  call counter_AV_Q(wf(:,-2),wf(:,-4),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,20),ZERO,0_intkind1,wf(:,27))
  call counter_QA_V(wf(:,10),wf(:,-2),wf(:,28))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,29))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,18),ZERO,0_intkind1,wf(:,31))
  call counter_VQ_A(wf(:,13),wf(:,-1),wf(:,32))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,33))
  call vert_VQ_A(wf(:,33),wf(:,0),wf(:,34))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,35))
  call vert_VQ_A(wf(:,35),wf(:,0),wf(:,36))
  call counter_VQ_A(wf(:,9),wf(:,0),wf(:,37))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,17),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,39),wf(:,-3),wf(:,40))
  call counter_VQ_A(wf(:,18),wf(:,0),wf(:,41))
  call vert_QA_V(wf(:,39),wf(:,-2),wf(:,42))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,43))
  call vert_AV_Q(wf(:,-2),wf(:,43),wf(:,44))
  call vert_VQ_A(wf(:,43),wf(:,-1),wf(:,45))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,46))
  call vert_AV_Q(wf(:,-3),wf(:,46),wf(:,47))
  call vert_VQ_A(wf(:,46),wf(:,-1),wf(:,48))
  call vert_QA_V(wf(:,3),wf(:,-3),wf(:,49))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,50))
  call counter_Q_A(ctqq,wf(:,3),Q(:,18),wf(:,51))
  call prop_A_Q(wf(:,4),Q(:,13),ZERO,0_intkind1,wf(:,52))
  call vert_QA_V(wf(:,-1),wf(:,6),wf(:,53))
  call counter_A_Q(ctqq,wf(:,6),Q(:,24),wf(:,54))
  call prop_Q_A(wf(:,7),Q(:,7),ZERO,0_intkind1,wf(:,55))
  call vert_AV_Q(wf(:,-3),wf(:,9),wf(:,56))
  call counter_Q_A(ctqq,wf(:,10),Q(:,17),wf(:,57))
  call prop_A_Q(wf(:,56),Q(:,14),ZERO,0_intkind1,wf(:,58))
  call counter_V_V(ctGG,wf(:,9),Q(:,6),wf(:,59))
  call vert_QA_V(wf(:,0),wf(:,6),wf(:,60))
  call prop_Q_A(wf(:,12),Q(:,7),ZERO,0_intkind1,wf(:,61))
  call vert_QA_V(wf(:,3),wf(:,-2),wf(:,62))
  call counter_V_V(ctGG,wf(:,13),Q(:,9),wf(:,63))
  call prop_A_Q(wf(:,14),Q(:,13),ZERO,0_intkind1,wf(:,64))
  call vert_QA_V(wf(:,-1),wf(:,16),wf(:,65))
  call counter_A_Q(ctqq,wf(:,16),Q(:,20),wf(:,66))
  call prop_Q_A(wf(:,17),Q(:,11),ZERO,0_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,-2),wf(:,18),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,14),ZERO,0_intkind1,wf(:,69))
  call counter_V_V(ctGG,wf(:,18),Q(:,10),wf(:,70))
  call vert_QA_V(wf(:,0),wf(:,16),wf(:,71))
  call prop_Q_A(wf(:,20),Q(:,11),ZERO,0_intkind1,wf(:,72))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,18))
  den(4) = 1 / (Q(5,24))
  den(6) = 1 / (Q(5,17))
  den(7) = 1 / (Q(5,6))
  den(10) = 1 / (Q(5,9))
  den(12) = 1 / (Q(5,20))
  den(14) = 1 / (Q(5,10))
  den(17) = 1 / (Q(5,26))
  den(20) = 1 / (Q(5,13))
  den(25) = 1 / (Q(5,7))
  den(28) = 1 / (Q(5,14))
  den(31) = 1 / (Q(5,25))
  den(38) = 1 / (Q(5,22))
  den(45) = 1 / (Q(5,11))
  den(50) = 1 / (Q(5,21))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(4)*den(7)
  den(11) = den(2)*den(10)
  den(13) = den(10)*den(12)
  den(15) = den(6)*den(14)
  den(16) = den(12)*den(14)
  den(18) = den(2)*den(17)
  den(19) = den(1)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(2)*den(21)
  den(23) = den(4)*den(17)
  den(24) = den(1)*den(23)
  den(26) = den(1)*den(25)
  den(27) = den(4)*den(26)
  den(29) = den(7)*den(28)
  den(30) = den(6)*den(29)
  den(32) = den(6)*den(31)
  den(33) = den(7)*den(32)
  den(34) = den(4)*den(31)
  den(35) = den(7)*den(34)
  den(36) = den(7)*den(25)
  den(37) = den(4)*den(36)
  den(39) = den(2)*den(38)
  den(40) = den(10)*den(39)
  den(41) = den(10)*den(20)
  den(42) = den(2)*den(41)
  den(43) = den(12)*den(38)
  den(44) = den(10)*den(43)
  den(46) = den(10)*den(45)
  den(47) = den(12)*den(46)
  den(48) = den(14)*den(28)
  den(49) = den(6)*den(48)
  den(51) = den(6)*den(50)
  den(52) = den(14)*den(51)
  den(53) = den(12)*den(50)
  den(54) = den(14)*den(53)
  den(55) = den(14)*den(45)
  den(56) = den(12)*den(55)
  den(57) = den(1)*den(14)
  den(58) = den(7)*den(10)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(48)

  A(1) = cont_QA(wf(:,3),wf(:,4)) * den(3)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,11)) * den(8)
  A(4) = cont_QA(wf(:,6),wf(:,12)) * den(9)
  A(5) = cont_QA(wf(:,3),wf(:,14)) * den(11)
  A(6) = cont_QA(wf(:,16),wf(:,17)) * den(13)
  A(7) = cont_VV(wf(:,18),wf(:,19)) * den(15)
  A(8) = cont_QA(wf(:,16),wf(:,20)) * den(16)

  A(9) = cont_QA(wf(:,3),wf(:,21)) * den(3)
  A(10) = cont_QA(wf(:,7),wf(:,23)) * den(5)
  A(11) = cont_VV(wf(:,9),wf(:,24)) * den(8)
  A(12) = cont_QA(wf(:,12),wf(:,23)) * den(9)
  A(13) = cont_QA(wf(:,3),wf(:,25)) * den(11)
  A(14) = cont_QA(wf(:,17),wf(:,27)) * den(13)
  A(15) = cont_VV(wf(:,18),wf(:,28)) * den(15)
  A(16) = cont_QA(wf(:,20),wf(:,27)) * den(16)
  A(17) = cont_QA(wf(:,6),wf(:,29)) * den(5)
  A(18) = cont_QA(wf(:,4),wf(:,31)) * den(3)
  A(19) = cont_QA(wf(:,16),wf(:,32)) * den(13)
  A(20) = cont_QA(wf(:,14),wf(:,31)) * den(11)
  A(21) = cont_VV(wf(:,19),wf(:,33)) * den(15)
  A(22) = cont_QA(wf(:,16),wf(:,34)) * den(16)
  A(23) = cont_VV(wf(:,11),wf(:,35)) * den(8)
  A(24) = cont_QA(wf(:,6),wf(:,36)) * den(9)
  A(25) = cont_QA(wf(:,6),wf(:,37)) * den(9)
  A(26) = cont_VV(wf(:,9),wf(:,40)) * den(8)
  A(27) = cont_QA(wf(:,16),wf(:,41)) * den(16)
  A(28) = cont_VV(wf(:,18),wf(:,42)) * den(15)
  A(29) = cont_QA(wf(:,3),wf(:,44)) * den(11)
  A(30) = cont_QA(wf(:,16),wf(:,45)) * den(13)
  A(31) = cont_QA(wf(:,3),wf(:,47)) * den(3)
  A(32) = cont_QA(wf(:,6),wf(:,48)) * den(5)
  A(33) = cont_VV(wf(:,49),wf(:,50)) * den(19)
  A(34) = cont_QA(wf(:,51),wf(:,52)) * den(22)
  A(35) = cont_VV(wf(:,50),wf(:,53)) * den(24)
  A(36) = cont_QA(wf(:,54),wf(:,55)) * den(27)
  A(37) = cont_QA(wf(:,57),wf(:,58)) * den(30)
  A(38) = cont_VV(wf(:,11),wf(:,59)) * den(33)
  A(39) = cont_VV(wf(:,59),wf(:,60)) * den(35)
  A(40) = cont_QA(wf(:,54),wf(:,61)) * den(37)
  A(41) = cont_VV(wf(:,62),wf(:,63)) * den(40)
  A(42) = cont_QA(wf(:,51),wf(:,64)) * den(42)
  A(43) = cont_VV(wf(:,63),wf(:,65)) * den(44)
  A(44) = cont_QA(wf(:,66),wf(:,67)) * den(47)
  A(45) = cont_QA(wf(:,57),wf(:,69)) * den(49)
  A(46) = cont_VV(wf(:,19),wf(:,70)) * den(52)
  A(47) = cont_VV(wf(:,70),wf(:,71)) * den(54)
  A(48) = cont_QA(wf(:,66),wf(:,72)) * den(56)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(48)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(3)-A(4)-A(5)-A(6))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(7)-A(8))*f(1))/2._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(5)+A(6))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(7)+A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((A(37)+A(38)+A(39)+A(40)+A(41)+A(42)+A(43)+A(44))*f(2))/6._/**/REALKIND+((A(33)+A(34)+A(35)+A(36)+A(45)+A(46)+A(47) &
       +A(48))*f(2))/2._/**/REALKIND+((-A(11)-A(13)-A(19)-A(23)-A(24)-A(25)-A(29)-A(30))*f(3))/6._/**/REALKIND+((-A(9)-A(15)-A(17) &
       -A(21)-A(22)-A(27)-A(31)-A(32))*f(3))/2._/**/REALKIND+((-A(12)-A(14)-A(20)-A(26))*f(4))/6._/**/REALKIND+((-A(10)-A(16) &
       -A(18)-A(28))*f(4))/2._/**/REALKIND
  M2(2) = ((-A(37)-A(38)-A(39)-A(40)-A(41)-A(42)-A(43)-A(44))*f(2))/2._/**/REALKIND+((-A(33)-A(34)-A(35)-A(36)-A(45)-A(46)-A(47) &
       -A(48))*f(2))/6._/**/REALKIND+((A(11)+A(13)+A(19)+A(23)+A(24)+A(25)+A(29)+A(30))*f(3))/2._/**/REALKIND+((A(9)+A(15)+A(17) &
       +A(21)+A(22)+A(27)+A(31)+A(32))*f(3))/6._/**/REALKIND+((A(12)+A(14)+A(20)+A(26))*f(4))/2._/**/REALKIND+((A(10)+A(16)+A(18) &
       +A(28))*f(4))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppajj_uuuxuxa_1_/**/REALKIND
