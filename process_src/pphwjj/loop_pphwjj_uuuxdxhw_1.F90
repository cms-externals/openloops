
module ol_colourmatrix_pphwjj_uuuxdxhw_1_/**/REALKIND
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
end module ol_colourmatrix_pphwjj_uuuxdxhw_1_/**/REALKIND



module ol_forced_parameters_pphwjj_uuuxdxhw_1_/**/REALKIND
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
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphwjj_uuuxdxhw_1_/**/REALKIND

module ol_loop_pphwjj_uuuxdxhw_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(13), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:59)
  ! denominators
  complex(REALKIND), save :: den(59)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,48)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**2*gQCD**2*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 5) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sqrt2*sw**2)
    f( 6) = (eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sqrt2*sw**2*2._/**/REALKIND)
    f( 7) = (eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sqrt2*sw**2)
    f( 8) = (eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sqrt2*sw**2)
    f( 9) = (2*eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sqrt2*sw**2)
    f(10) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/(MQ2sum*sqrt2*sw)
    f(11) = (eQED**2*gQCD**4*integralnorm*SwF*YB)/(MW*sqrt2*sw**2*2._/**/REALKIND)
    f(12) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/(MQ2sum*sqrt2*sw)
    f(13) = (eQED**2*gQCD**4*integralnorm*SwF*YT)/(MW*sqrt2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(5), 27*CI*f(5), 18*f(6), 54*f(6), f(7), 3*f(7), 6*f(7), 8*f(7), 10*f(7), 18*f(7), 21*f(7), 24*f(7), 54*f(7), 3*f(8) &
    , 9*f(8), 3*f(9), 9*f(9), 3*f(11), 9*f(11), 3*f(13), 9*f(13) ]
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
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_SV_V(wf(:,-4),wf(:,-5),wf(:,2))
  call prop_W_W(wf(:,2),Q(:,48),MW,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,4))
  call vert_AW_Q(wf(:,-3),wf(:,3),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,7),ZERO,0_intkind1,wf(:,6))
  call vert_WQ_A(wf(:,3),wf(:,-1),wf(:,7))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,50),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,10))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,11))
  call prop_Q_A(wf(:,11),Q(:,7),ZERO,0_intkind1,wf(:,12))
  call vert_WQ_A(wf(:,3),wf(:,0),wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,10),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,49),ZERO,0_intkind1,wf(:,15))
  call vert_WQ_A(wf(:,-5),wf(:,-1),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,34),ZERO,0_intkind1,wf(:,17))
  call vert_QA_V(wf(:,17),wf(:,-3),wf(:,18))
  call counter_SG_G(wf(:,-4),wf(:,1),wf(:,19))
  call vert_AW_Q(wf(:,-3),wf(:,-5),wf(:,20))
  call prop_A_Q(wf(:,20),Q(:,40),ZERO,0_intkind1,wf(:,21))
  call vert_QA_V(wf(:,-1),wf(:,21),wf(:,22))
  call counter_AW_Q(wf(:,-3),wf(:,3),wf(:,23))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,24))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,33),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,26),wf(:,-3),wf(:,27))
  call counter_SG_G(wf(:,-4),wf(:,10),wf(:,28))
  call vert_QA_V(wf(:,0),wf(:,21),wf(:,29))
  call counter_AV_Q(wf(:,-3),wf(:,10),wf(:,30))
  call counter_WQ_A(wf(:,3),wf(:,-1),wf(:,31))
  call prop_A_Q(wf(:,8),Q(:,13),ZERO,0_intkind1,wf(:,32))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,33))
  call prop_A_Q(wf(:,5),Q(:,56),ZERO,0_intkind1,wf(:,34))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,35))
  call vert_AV_Q(wf(:,-3),wf(:,35),wf(:,36))
  call vert_VQ_A(wf(:,35),wf(:,0),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,7),ZERO,0_intkind1,wf(:,38))
  call counter_VQ_A(wf(:,10),wf(:,0),wf(:,39))
  call counter_WQ_A(wf(:,3),wf(:,0),wf(:,40))
  call prop_A_Q(wf(:,14),Q(:,14),ZERO,0_intkind1,wf(:,41))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,42))
  call vert_VQ_A(wf(:,42),wf(:,-1),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,7),ZERO,0_intkind1,wf(:,44))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,45))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,46))
  call vert_VQ_A(wf(:,46),wf(:,-1),wf(:,47))
  call vert_AV_Q(wf(:,-3),wf(:,46),wf(:,48))
  call counter_Q_A(ctqq,wf(:,6),Q(:,7),wf(:,49))
  call counter_Q_A(ctqq,wf(:,9),Q(:,50),wf(:,50))
  call counter_V_V(ctGG,wf(:,10),Q(:,6),wf(:,51))
  call vert_VQ_A(wf(:,51),wf(:,0),wf(:,52))
  call counter_Q_A(ctqq,wf(:,12),Q(:,7),wf(:,53))
  call counter_Q_A(ctqq,wf(:,15),Q(:,49),wf(:,54))
  call vert_AV_Q(wf(:,-3),wf(:,51),wf(:,55))
  call vert_QA_V(wf(:,15),wf(:,-3),wf(:,56))
  call vert_QA_V(wf(:,0),wf(:,34),wf(:,57))
  call vert_QA_V(wf(:,9),wf(:,-3),wf(:,58))
  call vert_QA_V(wf(:,-1),wf(:,34),wf(:,59))

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
  den(2) = 1 / (Q(5,48) - MW2)
  den(3) = 1 / (Q(5,7))
  den(6) = 1 / (Q(5,50))
  den(9) = 1 / (Q(5,6))
  den(12) = 1 / (Q(5,49))
  den(15) = 1 / (Q(5,34))
  den(16) = 1 / (Q(5,42))
  den(19) = 1 / (Q(5,40))
  den(22) = 1 / (Q(5,33))
  den(23) = 1 / (Q(5,41))
  den(28) = 1 / (Q(5,13))
  den(31) = 1 / (Q(5,56))
  den(35) = 1 / (Q(5,14))
  den(48) = 1 / (Q(5,57))
  den(51) = 1 / (Q(5,58))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(10) = den(3)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(9)*den(13)
  den(17) = den(15)*den(16)
  den(18) = den(1)*den(17)
  den(20) = den(16)*den(19)
  den(21) = den(1)*den(20)
  den(24) = den(22)*den(23)
  den(25) = den(9)*den(24)
  den(26) = den(19)*den(23)
  den(27) = den(9)*den(26)
  den(29) = den(1)*den(28)
  den(30) = den(2)*den(29)
  den(32) = den(2)*den(31)
  den(33) = den(1)*den(32)
  den(34) = den(9)*den(32)
  den(36) = den(9)*den(35)
  den(37) = den(2)*den(36)
  den(38) = den(1)**2
  den(39) = den(32)*den(38)
  den(40) = den(7)*den(38)
  den(41) = den(4)*den(32)
  den(42) = den(7)*den(29)
  den(43) = den(9)**2
  den(44) = den(32)*den(43)
  den(45) = den(10)*den(32)
  den(46) = den(13)*den(36)
  den(47) = den(13)*den(43)
  den(49) = den(13)*den(48)
  den(50) = den(32)*den(48)
  den(52) = den(7)*den(51)
  den(53) = den(32)*den(51)
  den(54) = den(1)*den(2)
  den(55) = den(2)*den(9)
  den(56) = den(1)*den(52)
  den(57) = den(1)*den(53)
  den(58) = den(9)*den(49)
  den(59) = den(9)*den(50)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(32)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,5),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(14)

  A(5) = cont_VV(wf(:,18),wf(:,19)) * den(18)
  A(6) = cont_VV(wf(:,18),wf(:,19)) * den(18)
  A(7) = cont_VV(wf(:,19),wf(:,22)) * den(21)
  A(8) = cont_VV(wf(:,19),wf(:,22)) * den(21)
  A(9) = cont_QA(wf(:,6),wf(:,23)) * den(5)
  A(10) = cont_QA(wf(:,9),wf(:,24)) * den(8)
  A(11) = cont_VV(wf(:,27),wf(:,28)) * den(25)
  A(12) = cont_VV(wf(:,27),wf(:,28)) * den(25)
  A(13) = cont_VV(wf(:,28),wf(:,29)) * den(27)
  A(14) = cont_VV(wf(:,28),wf(:,29)) * den(27)
  A(15) = cont_QA(wf(:,12),wf(:,23)) * den(11)
  A(16) = cont_QA(wf(:,15),wf(:,30)) * den(14)
  A(17) = cont_QA(wf(:,31),wf(:,32)) * den(30)
  A(18) = cont_QA(wf(:,33),wf(:,34)) * den(33)
  A(19) = cont_QA(wf(:,15),wf(:,36)) * den(14)
  A(20) = cont_QA(wf(:,5),wf(:,38)) * den(11)
  A(21) = cont_QA(wf(:,34),wf(:,39)) * den(34)
  A(22) = cont_QA(wf(:,40),wf(:,41)) * den(37)
  A(23) = cont_QA(wf(:,5),wf(:,44)) * den(5)
  A(24) = cont_QA(wf(:,9),wf(:,45)) * den(8)
  A(25) = cont_QA(wf(:,34),wf(:,47)) * den(39)
  A(26) = cont_QA(wf(:,9),wf(:,48)) * den(40)
  A(27) = cont_QA(wf(:,34),wf(:,49)) * den(41)
  A(28) = cont_QA(wf(:,32),wf(:,50)) * den(42)
  A(29) = cont_QA(wf(:,34),wf(:,52)) * den(44)
  A(30) = cont_QA(wf(:,34),wf(:,53)) * den(45)
  A(31) = cont_QA(wf(:,41),wf(:,54)) * den(46)
  A(32) = cont_QA(wf(:,15),wf(:,55)) * den(47)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(32)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2))*f(1))/2._/**/REALKIND+((A(3)+A(4))*f(1))/6._/**/REALKIND
  M1(2) = ((-A(1)-A(2))*f(1))/6._/**/REALKIND+((-A(3)-A(4))*f(1))/2._/**/REALKIND

  M2(1) = ((-A(25)-A(26)-A(27)-A(28))*f(2))/2._/**/REALKIND+((-A(29)-A(30)-A(31)-A(32))*f(2))/6._/**/REALKIND+((A(16)+A(19)+A(20) &
       +A(21))*f(3))/6._/**/REALKIND+((A(10)+A(18)+A(23)+A(24))*f(3))/2._/**/REALKIND+((A(9)+A(17))*f(4))/2._/**/REALKIND+((A(15) &
       +A(22))*f(4))/6._/**/REALKIND+((A(6)+A(8))*f(10))/2._/**/REALKIND+((A(12)+A(14))*f(10))/6._/**/REALKIND+((A(5) &
       +A(7))*f(12))/2._/**/REALKIND+((A(11)+A(13))*f(12))/6._/**/REALKIND
  M2(2) = ((A(25)+A(26)+A(27)+A(28))*f(2))/6._/**/REALKIND+((A(29)+A(30)+A(31)+A(32))*f(2))/2._/**/REALKIND+((-A(16)-A(19)-A(20) &
       -A(21))*f(3))/2._/**/REALKIND+((-A(10)-A(18)-A(23)-A(24))*f(3))/6._/**/REALKIND+((-A(9)-A(17))*f(4))/6._/**/REALKIND+(( &
       -A(15)-A(22))*f(4))/2._/**/REALKIND+((-A(6)-A(8))*f(10))/6._/**/REALKIND+((-A(12)-A(14))*f(10))/2._/**/REALKIND+((-A(5) &
       -A(7))*f(12))/6._/**/REALKIND+((-A(11)-A(13))*f(12))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphwjj_uuuxdxhw_1_/**/REALKIND
