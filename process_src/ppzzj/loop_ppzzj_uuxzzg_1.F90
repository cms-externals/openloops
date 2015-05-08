
module ol_colourmatrix_ppzzj_uuxzzg_1_/**/REALKIND
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

  K1( 1,:) = [  12]
  K1( 2,:) = [  16]
  K1( 3,:) = [   2]
  K1( 4,:) = [  16]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [   0]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   0]
  K1(11,:) = [   0]
  K1(12,:) = [ -18]
  K1(13,:) = [ -18]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [  36]
  K1(17,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppzzj_uuxzzg_1_/**/REALKIND



module ol_forced_parameters_ppzzj_uuxzzg_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzzj_uuxzzg_1_/**/REALKIND

module ol_loop_ppzzj_uuxzzg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(13), c(7)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:76)
  ! denominators
  complex(REALKIND), save :: den(57)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,72)
  ! zero helicity identifier
  logical,           save :: zerohel(72) = .true., zerohel_ct(72) = .true.

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
    f( 1) = CI*eQED**2*gQCD
    f( 2) = CI*countertermnorm*eQED**2*gQCD**3
    f( 3) = CI*countertermnorm*ctGqq*eQED**2*gQCD**3
    f( 4) = CI*countertermnorm*ctVqq*eQED**2*gQCD**3
    f( 5) = countertermnorm*ctZGG*eQED**2*gQCD**3
    f( 6) = CI*countertermnorm*ctZZGG*eQED**2*gQCD**3
    f( 7) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**3*MW)/(cw**2*sw)
    f( 8) = CI*eQED**2*gQCD**3*integralnorm*SwB
    f( 9) = eQED**2*gQCD**3*integralnorm*SwB
    f(10) = eQED**2*gQCD**3*integralnorm*SwF
    f(11) = 2*eQED**2*gQCD**3*integralnorm*SwF
    f(12) = (eQED**2*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(13) = (eQED**2*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(8), f(9), 8*f(9), 3*f(10), 3*f(11), 3*f(12), 3*f(13) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND) :: A(42)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,0),wf(:,1))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),ZERO,0_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,10),ZERO,0_intkind1,wf(:,4))
  call vert_VQ_A(wf(:,-4),wf(:,3),wf(:,5))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,6))
  call prop_A_Q(wf(:,6),Q(:,18),ZERO,0_intkind1,wf(:,7))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,3),wf(:,8))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,0),wf(:,9))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-2),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,9),ZERO,0_intkind1,wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,6),ZERO,0_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,-4),wf(:,11),wf(:,13))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,14))
  call prop_Q_A(wf(:,14),Q(:,17),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,15),wf(:,16))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,11),wf(:,17))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,15),wf(:,18))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,19))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,20))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,21))
  call counter_GG_S(wf(:,19),wf(:,-4),wf(:,22))
  call counter_VQ_A(wf(:,-4),wf(:,3),wf(:,23))
  call counter_ZQ_A(gZu,wf(:,-3),wf(:,3),wf(:,24))
  call counter_VG_G(wf(:,-3),wf(:,-4),Q(:,16),wf(:,25),Q(:,24))
  call vert_QA_V(wf(:,3),wf(:,-1),wf(:,26))
  call counter_VQ_A(wf(:,-4),wf(:,11),wf(:,27))
  call counter_ZQ_A(gZu,wf(:,-3),wf(:,15),wf(:,28))
  call vert_QA_V(wf(:,0),wf(:,12),wf(:,29))
  call counter_ZQ_A(gZu,wf(:,-2),wf(:,11),wf(:,30))
  call counter_VG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,31),Q(:,20))
  call vert_QA_V(wf(:,11),wf(:,-1),wf(:,32))
  call counter_ZQ_A(gZu,wf(:,-2),wf(:,15),wf(:,33))
  call vert_QA_V(wf(:,0),wf(:,4),wf(:,34))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,18),ZERO,0_intkind1,wf(:,36))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-3),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,10),ZERO,0_intkind1,wf(:,38))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-2),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,6),ZERO,0_intkind1,wf(:,40))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,17),ZERO,0_intkind1,wf(:,42))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,42),wf(:,43))
  call counter_ZQ_A(gZu,wf(:,-3),wf(:,0),wf(:,44))
  call prop_Q_A(wf(:,44),Q(:,9),ZERO,0_intkind1,wf(:,45))
  call vert_VQ_A(wf(:,-4),wf(:,45),wf(:,46))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,42),wf(:,47))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,45),wf(:,48))
  call counter_ZQ_A(gZu,wf(:,-2),wf(:,0),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,5),ZERO,0_intkind1,wf(:,50))
  call vert_VQ_A(wf(:,-4),wf(:,50),wf(:,51))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,50),wf(:,52))
  call vert_AV_Q(wf(:,4),wf(:,-4),wf(:,53))
  call counter_Q_A(ctqq,wf(:,3),Q(:,5),wf(:,54))
  call prop_A_Q(wf(:,53),Q(:,26),ZERO,0_intkind1,wf(:,55))
  call counter_A_Q(ctqq,wf(:,4),Q(:,10),wf(:,56))
  call prop_Q_A(wf(:,5),Q(:,21),ZERO,0_intkind1,wf(:,57))
  call vert_AZ_Q(gZu,wf(:,7),wf(:,-3),wf(:,58))
  call prop_A_Q(wf(:,58),Q(:,26),ZERO,0_intkind1,wf(:,59))
  call counter_A_Q(ctqq,wf(:,7),Q(:,18),wf(:,60))
  call prop_Q_A(wf(:,8),Q(:,13),ZERO,0_intkind1,wf(:,61))
  call vert_AV_Q(wf(:,12),wf(:,-4),wf(:,62))
  call counter_Q_A(ctqq,wf(:,11),Q(:,9),wf(:,63))
  call prop_A_Q(wf(:,62),Q(:,22),ZERO,0_intkind1,wf(:,64))
  call counter_A_Q(ctqq,wf(:,12),Q(:,6),wf(:,65))
  call prop_Q_A(wf(:,13),Q(:,25),ZERO,0_intkind1,wf(:,66))
  call vert_AZ_Q(gZu,wf(:,12),wf(:,-3),wf(:,67))
  call counter_Q_A(ctqq,wf(:,15),Q(:,17),wf(:,68))
  call prop_A_Q(wf(:,67),Q(:,14),ZERO,0_intkind1,wf(:,69))
  call prop_Q_A(wf(:,16),Q(:,25),ZERO,0_intkind1,wf(:,70))
  call vert_AZ_Q(gZu,wf(:,7),wf(:,-2),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,22),ZERO,0_intkind1,wf(:,72))
  call prop_Q_A(wf(:,17),Q(:,13),ZERO,0_intkind1,wf(:,73))
  call vert_AZ_Q(gZu,wf(:,4),wf(:,-2),wf(:,74))
  call prop_A_Q(wf(:,74),Q(:,14),ZERO,0_intkind1,wf(:,75))
  call prop_Q_A(wf(:,18),Q(:,21),ZERO,0_intkind1,wf(:,76))

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
  den(2) = 1 / (Q(5,10))
  den(4) = 1 / (Q(5,18))
  den(6) = 1 / (Q(5,9))
  den(7) = 1 / (Q(5,6))
  den(9) = 1 / (Q(5,17))
  den(13) = 1 / (Q(5,3))
  den(14) = 1 / (Q(5,12) - MH2)
  den(16) = 1 / (Q(5,24))
  den(19) = 1 / (Q(5,20))
  den(22) = 1 / (Q(5,26))
  den(25) = 1 / (Q(5,21))
  den(30) = 1 / (Q(5,13))
  den(33) = 1 / (Q(5,22))
  den(36) = 1 / (Q(5,25))
  den(39) = 1 / (Q(5,14))
  den(52) = 1 / (Q(5,7))
  den(55) = 1 / (Q(5,11))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(11) = den(4)*den(6)
  den(12) = den(2)*den(9)
  den(15) = den(13)*den(14)
  den(17) = den(1)*den(16)
  den(18) = den(7)*den(16)
  den(20) = den(6)*den(19)
  den(21) = den(2)*den(19)
  den(23) = den(2)*den(22)
  den(24) = den(1)*den(23)
  den(26) = den(1)*den(25)
  den(27) = den(2)*den(26)
  den(28) = den(4)*den(22)
  den(29) = den(1)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(4)*den(31)
  den(34) = den(7)*den(33)
  den(35) = den(6)*den(34)
  den(37) = den(6)*den(36)
  den(38) = den(7)*den(37)
  den(40) = den(7)*den(39)
  den(41) = den(9)*den(40)
  den(42) = den(9)*den(36)
  den(43) = den(7)*den(42)
  den(44) = den(4)*den(33)
  den(45) = den(6)*den(44)
  den(46) = den(6)*den(30)
  den(47) = den(4)*den(46)
  den(48) = den(2)*den(39)
  den(49) = den(9)*den(48)
  den(50) = den(9)*den(25)
  den(51) = den(2)*den(50)
  den(53) = den(1)*den(52)
  den(54) = den(7)*den(52)
  den(56) = den(6)*den(55)
  den(57) = den(2)*den(55)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(42)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(8)
  A(4) = cont_QA(wf(:,12),wf(:,16)) * den(10)
  A(5) = cont_QA(wf(:,7),wf(:,17)) * den(11)
  A(6) = cont_QA(wf(:,4),wf(:,18)) * den(12)

  A(7) = cont_VV(wf(:,19),wf(:,20)) * den(13)
  A(8) = cont_SS(wf(:,21),wf(:,22)) * den(15)
  A(9) = cont_QA(wf(:,4),wf(:,23)) * den(3)
  A(10) = cont_QA(wf(:,7),wf(:,24)) * den(5)
  A(11) = cont_VV(wf(:,25),wf(:,26)) * den(17)
  A(12) = cont_QA(wf(:,12),wf(:,27)) * den(8)
  A(13) = cont_QA(wf(:,12),wf(:,28)) * den(10)
  A(14) = cont_VV(wf(:,25),wf(:,29)) * den(18)
  A(15) = cont_QA(wf(:,7),wf(:,30)) * den(11)
  A(16) = cont_VV(wf(:,31),wf(:,32)) * den(20)
  A(17) = cont_QA(wf(:,4),wf(:,33)) * den(12)
  A(18) = cont_VV(wf(:,31),wf(:,34)) * den(21)
  A(19) = cont_QA(wf(:,8),wf(:,36)) * den(5)
  A(20) = cont_QA(wf(:,5),wf(:,38)) * den(3)
  A(21) = cont_QA(wf(:,17),wf(:,36)) * den(11)
  A(22) = cont_QA(wf(:,18),wf(:,38)) * den(12)
  A(23) = cont_QA(wf(:,13),wf(:,40)) * den(8)
  A(24) = cont_QA(wf(:,16),wf(:,40)) * den(10)
  A(25) = cont_QA(wf(:,12),wf(:,43)) * den(10)
  A(26) = cont_QA(wf(:,12),wf(:,46)) * den(8)
  A(27) = cont_QA(wf(:,4),wf(:,47)) * den(12)
  A(28) = cont_QA(wf(:,7),wf(:,48)) * den(11)
  A(29) = cont_QA(wf(:,4),wf(:,51)) * den(3)
  A(30) = cont_QA(wf(:,7),wf(:,52)) * den(5)
  A(31) = cont_QA(wf(:,54),wf(:,55)) * den(24)
  A(32) = cont_QA(wf(:,56),wf(:,57)) * den(27)
  A(33) = cont_QA(wf(:,54),wf(:,59)) * den(29)
  A(34) = cont_QA(wf(:,60),wf(:,61)) * den(32)
  A(35) = cont_QA(wf(:,63),wf(:,64)) * den(35)
  A(36) = cont_QA(wf(:,65),wf(:,66)) * den(38)
  A(37) = cont_QA(wf(:,68),wf(:,69)) * den(41)
  A(38) = cont_QA(wf(:,65),wf(:,70)) * den(43)
  A(39) = cont_QA(wf(:,63),wf(:,72)) * den(45)
  A(40) = cont_QA(wf(:,60),wf(:,73)) * den(47)
  A(41) = cont_QA(wf(:,68),wf(:,75)) * den(49)
  A(42) = cont_QA(wf(:,56),wf(:,76)) * den(51)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(42)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(2)+A(3)+A(4)+A(5)+A(6))*f(1)

  M2(1) = (-A(31)-A(32)-A(33)-A(34)-A(35)-A(36)-A(37)-A(38)-A(39)-A(40)-A(41)-A(42))*f(2)+(A(9)+A(12)+A(19)+A(21)+A(25) &
       +A(27))*f(3)+(A(10)+A(13)+A(15)+A(17)+A(20)+A(22)+A(23)+A(24)+A(26)+A(28)+A(29)+A(30))*f(4)+(-A(11)-A(14)-A(16)-A(18))*f(5) &
       +A(7)*f(6)-A(8)*f(7)

end subroutine colourvectors

end module ol_loop_ppzzj_uuxzzg_1_/**/REALKIND
