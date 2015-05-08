
module ol_colourmatrix_pplnjj_neexuuxuxd_1_/**/REALKIND
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
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [ -12,  -4]
  K1(28,:) = [  -4,   0]
  K1(29,:) = [   0,   4]
  K1(30,:) = [   4,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   4]
  K1(38,:) = [   4,   0]
  K1(39,:) = [ -12,  -4]
  K1(40,:) = [  -4,   0]
  K1(41,:) = [   0,  -4]
  K1(42,:) = [  -4, -12]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplnjj_neexuuxuxd_1_/**/REALKIND



module ol_forced_parameters_pplnjj_neexuuxuxd_1_/**/REALKIND
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
end module ol_forced_parameters_pplnjj_neexuuxuxd_1_/**/REALKIND

module ol_loop_pplnjj_neexuuxuxd_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(9), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:47)
  ! denominators
  complex(REALKIND), save :: den(46)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64)
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
    f(1) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(2) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(3) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(4) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(5) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(6) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(7) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(8) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(9) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(5), 27*CI*f(5), 18*f(6), 54*f(6), f(7), 3*f(7), 6*f(7), 8*f(7), 10*f(7), 18*f(7), 21*f(7), 24*f(7), 54*f(7), 3*f(8) &
    , 9*f(8), 3*f(9), 9*f(9) ]
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
  complex(REALKIND) :: A(24)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,3))
  call vert_AW_Q(wf(:,-4),wf(:,3),wf(:,4))
  call vert_VQ_A(wf(:,2),wf(:,-5),wf(:,5))
  call prop_A_Q(wf(:,4),Q(:,19),ZERO,0_intkind1,wf(:,6))
  call vert_AV_Q(wf(:,-4),wf(:,2),wf(:,7))
  call vert_WQ_A(wf(:,3),wf(:,-5),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,28),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,10))
  call vert_AW_Q(wf(:,-3),wf(:,3),wf(:,11))
  call vert_VQ_A(wf(:,10),wf(:,-5),wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,11),ZERO,0_intkind1,wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,10),wf(:,14))
  call prop_A_Q(wf(:,14),Q(:,28),ZERO,0_intkind1,wf(:,15))
  call counter_VQ_A(wf(:,2),wf(:,-5),wf(:,16))
  call counter_WQ_A(wf(:,3),wf(:,-5),wf(:,17))
  call counter_AV_Q(wf(:,-4),wf(:,2),wf(:,18))
  call prop_Q_A(wf(:,8),Q(:,35),ZERO,0_intkind1,wf(:,19))
  call counter_AW_Q(wf(:,-4),wf(:,3),wf(:,20))
  call prop_Q_A(wf(:,5),Q(:,44),ZERO,0_intkind1,wf(:,21))
  call counter_VQ_A(wf(:,10),wf(:,-5),wf(:,22))
  call counter_AV_Q(wf(:,-3),wf(:,10),wf(:,23))
  call counter_AW_Q(wf(:,-3),wf(:,3),wf(:,24))
  call prop_Q_A(wf(:,12),Q(:,52),ZERO,0_intkind1,wf(:,25))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,26))
  call vert_VQ_A(wf(:,26),wf(:,-5),wf(:,27))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,28))
  call prop_A_Q(wf(:,28),Q(:,28),ZERO,0_intkind1,wf(:,29))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,30))
  call vert_VQ_A(wf(:,30),wf(:,-5),wf(:,31))
  call vert_AV_Q(wf(:,-4),wf(:,30),wf(:,32))
  call prop_A_Q(wf(:,32),Q(:,28),ZERO,0_intkind1,wf(:,33))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,34))
  call vert_AV_Q(wf(:,-4),wf(:,34),wf(:,35))
  call vert_VQ_A(wf(:,34),wf(:,-5),wf(:,36))
  call counter_A_Q(ctqq,wf(:,6),Q(:,19),wf(:,37))
  call counter_A_Q(ctqq,wf(:,9),Q(:,28),wf(:,38))
  call counter_V_V(ctGG,wf(:,10),Q(:,20),wf(:,39))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,40))
  call vert_VQ_A(wf(:,39),wf(:,-5),wf(:,41))
  call counter_A_Q(ctqq,wf(:,13),Q(:,11),wf(:,42))
  call counter_A_Q(ctqq,wf(:,15),Q(:,28),wf(:,43))
  call vert_QA_V(wf(:,-5),wf(:,13),wf(:,44))
  call vert_QA_V(wf(:,-5),wf(:,6),wf(:,45))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,46))
  call vert_QA_V(wf(:,19),wf(:,-4),wf(:,47))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MW2)
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,19))
  den(6) = 1 / (Q(5,28))
  den(9) = 1 / (Q(5,20))
  den(10) = 1 / (Q(5,11))
  den(15) = 1 / (Q(5,35))
  den(18) = 1 / (Q(5,44))
  den(22) = 1 / (Q(5,52))
  den(35) = 1 / (Q(5,43))
  den(37) = 1 / (Q(5,51))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(1)*den(10)
  den(12) = den(9)*den(11)
  den(13) = den(6)*den(9)
  den(14) = den(1)*den(13)
  den(16) = den(1)*den(15)
  den(17) = den(2)*den(16)
  den(19) = den(2)*den(18)
  den(20) = den(1)*den(19)
  den(21) = den(9)*den(16)
  den(23) = den(9)*den(22)
  den(24) = den(1)*den(23)
  den(25) = den(2)**2
  den(26) = den(16)*den(25)
  den(27) = den(4)*den(25)
  den(28) = den(4)*den(19)
  den(29) = den(7)*den(16)
  den(30) = den(9)**2
  den(31) = den(16)*den(30)
  den(32) = den(11)*den(30)
  den(33) = den(11)*den(23)
  den(34) = den(13)*den(16)
  den(36) = den(11)*den(35)
  den(38) = den(4)*den(37)
  den(39) = den(16)*den(35)
  den(40) = den(16)*den(37)
  den(41) = den(1)*den(2)
  den(42) = den(1)*den(9)
  den(43) = den(2)*den(38)
  den(44) = den(2)*den(40)
  den(45) = den(9)*den(36)
  den(46) = den(9)*den(39)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(24)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(4) = cont_QA(wf(:,8),wf(:,15)) * den(14)

  A(5) = cont_QA(wf(:,6),wf(:,16)) * den(5)
  A(6) = cont_QA(wf(:,9),wf(:,17)) * den(8)
  A(7) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(8) = cont_QA(wf(:,20),wf(:,21)) * den(20)
  A(9) = cont_QA(wf(:,13),wf(:,22)) * den(12)
  A(10) = cont_QA(wf(:,15),wf(:,17)) * den(14)
  A(11) = cont_QA(wf(:,19),wf(:,23)) * den(21)
  A(12) = cont_QA(wf(:,24),wf(:,25)) * den(24)
  A(13) = cont_QA(wf(:,13),wf(:,27)) * den(12)
  A(14) = cont_QA(wf(:,8),wf(:,29)) * den(14)
  A(15) = cont_QA(wf(:,6),wf(:,31)) * den(5)
  A(16) = cont_QA(wf(:,8),wf(:,33)) * den(8)
  A(17) = cont_QA(wf(:,19),wf(:,35)) * den(26)
  A(18) = cont_QA(wf(:,6),wf(:,36)) * den(27)
  A(19) = cont_QA(wf(:,21),wf(:,37)) * den(28)
  A(20) = cont_QA(wf(:,19),wf(:,38)) * den(29)
  A(21) = cont_QA(wf(:,19),wf(:,40)) * den(31)
  A(22) = cont_QA(wf(:,13),wf(:,41)) * den(32)
  A(23) = cont_QA(wf(:,25),wf(:,42)) * den(33)
  A(24) = cont_QA(wf(:,19),wf(:,43)) * den(34)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(24)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(2))*f(1))/2._/**/REALKIND+((-A(3)-A(4))*f(1))/6._/**/REALKIND
  M1(2) = ((A(1)+A(2))*f(1))/6._/**/REALKIND+((A(3)+A(4))*f(1))/2._/**/REALKIND

  M2(1) = ((A(17)+A(18)+A(19)+A(20))*f(2))/2._/**/REALKIND+((A(21)+A(22)+A(23)+A(24))*f(2))/6._/**/REALKIND+((-A(9)-A(11)-A(13) &
       -A(14))*f(3))/6._/**/REALKIND+((-A(5)-A(7)-A(15)-A(16))*f(3))/2._/**/REALKIND+((-A(6)-A(8))*f(4))/2._/**/REALKIND+((-A(10) &
       -A(12))*f(4))/6._/**/REALKIND
  M2(2) = ((-A(17)-A(18)-A(19)-A(20))*f(2))/6._/**/REALKIND+((-A(21)-A(22)-A(23)-A(24))*f(2))/2._/**/REALKIND+((A(9)+A(11)+A(13) &
       +A(14))*f(3))/2._/**/REALKIND+((A(5)+A(7)+A(15)+A(16))*f(3))/6._/**/REALKIND+((A(6)+A(8))*f(4))/6._/**/REALKIND+((A(10) &
       +A(12))*f(4))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnjj_neexuuxuxd_1_/**/REALKIND
