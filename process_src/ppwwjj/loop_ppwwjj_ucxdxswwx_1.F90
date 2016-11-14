
module ol_colourmatrix_ppwwjj_ucxdxswwx_1_/**/REALKIND
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
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppwwjj_ucxdxswwx_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_ucxdxswwx_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwwjj_ucxdxswwx_1_/**/REALKIND

module ol_loop_ppwwjj_ucxdxswwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:59)
  ! denominators
  complex(REALKIND), save :: den(58)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,144)
  ! zero helicity identifier
  logical,           save :: zerohel(144) = .true., zerohel_ct(144) = .true.

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
    f( 1) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 3) = (CI*countertermnorm*ctGcc*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 6) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f( 7) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f( 8) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f( 9) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(10) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(32)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_WQ_A(wf(:,-4),wf(:,0),wf(:,1))
  call vert_AW_Q(wf(:,-1),wf(:,-5),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,34),ZERO,0_intkind1,wf(:,4))
  call vert_QA_V(wf(:,3),wf(:,-2),wf(:,5))
  call vert_QA_V(wf(:,-3),wf(:,4),wf(:,6))
  call vert_WQ_A(wf(:,-5),wf(:,-3),wf(:,7))
  call prop_Q_A(wf(:,7),Q(:,40),ZERO,0_intkind1,wf(:,8))
  call vert_QA_V(wf(:,8),wf(:,-1),wf(:,9))
  call vert_AW_Q(wf(:,-2),wf(:,-4),wf(:,10))
  call prop_A_Q(wf(:,10),Q(:,20),ZERO,0_intkind1,wf(:,11))
  call vert_QA_V(wf(:,0),wf(:,11),wf(:,12))
  call counter_QA_V(wf(:,-3),wf(:,4),wf(:,13))
  call counter_WQ_A(wf(:,-5),wf(:,-3),wf(:,14))
  call prop_Q_A(wf(:,14),Q(:,40),ZERO,0_intkind1,wf(:,15))
  call vert_QA_V(wf(:,15),wf(:,-1),wf(:,16))
  call counter_QA_V(wf(:,3),wf(:,-2),wf(:,17))
  call counter_AW_Q(wf(:,-2),wf(:,-4),wf(:,18))
  call prop_A_Q(wf(:,18),Q(:,20),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,0),wf(:,19),wf(:,20))
  call counter_QA_V(wf(:,8),wf(:,-1),wf(:,21))
  call counter_AW_Q(wf(:,-1),wf(:,-5),wf(:,22))
  call prop_A_Q(wf(:,22),Q(:,34),ZERO,0_intkind1,wf(:,23))
  call vert_QA_V(wf(:,-3),wf(:,23),wf(:,24))
  call counter_QA_V(wf(:,0),wf(:,11),wf(:,25))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,17),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,27),wf(:,-2),wf(:,28))
  call counter_Q_A(ctqq,wf(:,3),Q(:,17),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,17),ZERO,0_intkind1,wf(:,30))
  call vert_QA_V(wf(:,30),wf(:,-2),wf(:,31))
  call counter_A_Q(ctqq,wf(:,4),Q(:,34),wf(:,32))
  call prop_A_Q(wf(:,32),Q(:,34),ZERO,0_intkind1,wf(:,33))
  call vert_QA_V(wf(:,-3),wf(:,33),wf(:,34))
  call counter_V_V(ctGG,wf(:,5),Q(:,21),wf(:,35))
  call counter_V_V(ctGG,wf(:,9),Q(:,42),wf(:,36))
  call counter_Q_A(ctcc,wf(:,8),Q(:,40),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,40),ZERO,0_intkind1,wf(:,38))
  call vert_QA_V(wf(:,38),wf(:,-1),wf(:,39))
  call counter_A_Q(ctqq,wf(:,11),Q(:,20),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,20),ZERO,0_intkind1,wf(:,41))
  call vert_QA_V(wf(:,0),wf(:,41),wf(:,42))
  call counter_V_V(ctGG,wf(:,12),Q(:,21),wf(:,43))
  call vert_AV_Q(wf(:,-1),wf(:,5),wf(:,44))
  call prop_A_Q(wf(:,44),Q(:,23),ZERO,0_intkind1,wf(:,45))
  call vert_VQ_A(wf(:,5),wf(:,-3),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,29),ZERO,0_intkind1,wf(:,47))
  call vert_AV_Q(wf(:,-1),wf(:,12),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,23),ZERO,0_intkind1,wf(:,49))
  call vert_VQ_A(wf(:,12),wf(:,-3),wf(:,50))
  call prop_Q_A(wf(:,50),Q(:,29),ZERO,0_intkind1,wf(:,51))
  call vert_VQ_A(wf(:,6),wf(:,0),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,43),ZERO,0_intkind1,wf(:,53))
  call vert_AV_Q(wf(:,-2),wf(:,6),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,46),ZERO,0_intkind1,wf(:,55))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,43),ZERO,0_intkind1,wf(:,57))
  call vert_AV_Q(wf(:,-2),wf(:,9),wf(:,58))
  call prop_A_Q(wf(:,58),Q(:,46),ZERO,0_intkind1,wf(:,59))

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
  den(2) = 1 / (Q(5,34))
  den(3) = 1 / (Q(5,21))
  den(6) = 1 / (Q(5,40))
  den(7) = 1 / (Q(5,42))
  den(10) = 1 / (Q(5,20))
  den(35) = 1 / (Q(5,23))
  den(37) = 1 / (Q(5,29))
  den(41) = 1 / (Q(5,43))
  den(43) = 1 / (Q(5,46))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(1)*den(8)
  den(11) = den(3)*den(10)
  den(12) = den(2)*den(11)
  den(13) = den(6)*den(11)
  den(14) = den(2)*den(7)
  den(15) = den(1)*den(14)
  den(16) = den(4)*den(6)
  den(17) = den(10)*den(14)
  den(18) = den(8)*den(10)
  den(19) = den(1)**2
  den(20) = den(14)*den(19)
  den(21) = den(2)**2
  den(22) = den(4)*den(21)
  den(23) = den(4)*den(14)
  den(24) = den(8)*den(19)
  den(25) = den(4)*den(8)
  den(26) = den(6)**2
  den(27) = den(4)*den(26)
  den(28) = den(10)**2
  den(29) = den(14)*den(28)
  den(30) = den(11)*den(14)
  den(31) = den(11)*den(21)
  den(32) = den(8)*den(11)
  den(33) = den(8)*den(28)
  den(34) = den(11)*den(26)
  den(36) = den(4)*den(35)
  den(38) = den(4)*den(37)
  den(39) = den(11)*den(35)
  den(40) = den(11)*den(37)
  den(42) = den(14)*den(41)
  den(44) = den(14)*den(43)
  den(45) = den(8)*den(41)
  den(46) = den(8)*den(43)
  den(47) = den(1)*den(2)
  den(48) = den(1)*den(6)
  den(49) = den(2)*den(10)
  den(50) = den(6)*den(10)
  den(51) = den(2)*den(38)
  den(52) = den(1)*den(44)
  den(53) = den(6)*den(36)
  den(54) = den(1)*den(46)
  den(55) = den(2)*den(40)
  den(56) = den(10)*den(42)
  den(57) = den(6)*den(39)
  den(58) = den(10)*den(45)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(32)

  A(1) = cont_VV(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,5),wf(:,9)) * den(9)
  A(3) = cont_VV(wf(:,6),wf(:,12)) * den(12)
  A(4) = cont_VV(wf(:,9),wf(:,12)) * den(13)

  A(5) = cont_VV(wf(:,5),wf(:,13)) * den(5)
  A(6) = cont_VV(wf(:,5),wf(:,16)) * den(9)
  A(7) = cont_VV(wf(:,12),wf(:,13)) * den(12)
  A(8) = cont_VV(wf(:,12),wf(:,16)) * den(13)
  A(9) = cont_VV(wf(:,6),wf(:,17)) * den(15)
  A(10) = cont_VV(wf(:,9),wf(:,17)) * den(9)
  A(11) = cont_VV(wf(:,6),wf(:,20)) * den(12)
  A(12) = cont_VV(wf(:,9),wf(:,20)) * den(13)
  A(13) = cont_VV(wf(:,5),wf(:,21)) * den(16)
  A(14) = cont_VV(wf(:,5),wf(:,24)) * den(5)
  A(15) = cont_VV(wf(:,12),wf(:,21)) * den(13)
  A(16) = cont_VV(wf(:,12),wf(:,24)) * den(12)
  A(17) = cont_VV(wf(:,6),wf(:,25)) * den(17)
  A(18) = cont_VV(wf(:,9),wf(:,25)) * den(18)
  A(19) = cont_VV(wf(:,6),wf(:,28)) * den(5)
  A(20) = cont_VV(wf(:,9),wf(:,28)) * den(9)
  A(21) = cont_VV(wf(:,6),wf(:,31)) * den(20)
  A(22) = cont_VV(wf(:,5),wf(:,34)) * den(22)
  A(23) = cont_VV(wf(:,6),wf(:,35)) * den(23)
  A(24) = cont_VV(wf(:,9),wf(:,31)) * den(24)
  A(25) = cont_VV(wf(:,5),wf(:,36)) * den(25)
  A(26) = cont_VV(wf(:,5),wf(:,39)) * den(27)
  A(27) = cont_VV(wf(:,6),wf(:,42)) * den(29)
  A(28) = cont_VV(wf(:,6),wf(:,43)) * den(30)
  A(29) = cont_VV(wf(:,12),wf(:,34)) * den(31)
  A(30) = cont_VV(wf(:,9),wf(:,43)) * den(32)
  A(31) = cont_VV(wf(:,9),wf(:,42)) * den(33)
  A(32) = cont_VV(wf(:,12),wf(:,39)) * den(34)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(32)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(2)-A(3)-A(4))*f(1))/6._/**/REALKIND
  M1(2) = ((A(1)+A(2)+A(3)+A(4))*f(1))/2._/**/REALKIND

  M2(1) = ((A(21)+A(22)+A(23)+A(24)+A(25)+A(26)+A(27)+A(28)+A(29)+A(30)+A(31)+A(32))*f(2))/6._/**/REALKIND+((-A(13) &
       -A(15))*f(3))/6._/**/REALKIND+((-A(5)-A(7)-A(9)-A(10)-A(17)-A(18))*f(4))/6._/**/REALKIND+((-A(6)-A(8)-A(11)-A(12)-A(14) &
       -A(16)-A(19)-A(20))*f(5))/6._/**/REALKIND
  M2(2) = ((-A(21)-A(22)-A(23)-A(24)-A(25)-A(26)-A(27)-A(28)-A(29)-A(30)-A(31)-A(32))*f(2))/2._/**/REALKIND+((A(13) &
       +A(15))*f(3))/2._/**/REALKIND+((A(5)+A(7)+A(9)+A(10)+A(17)+A(18))*f(4))/2._/**/REALKIND+((A(6)+A(8)+A(11)+A(12)+A(14)+A(16) &
       +A(19)+A(20))*f(5))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_ucxdxswwx_1_/**/REALKIND
