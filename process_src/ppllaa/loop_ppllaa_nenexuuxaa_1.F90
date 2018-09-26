
module ol_colourmatrix_ppllaa_nenexuuxaa_1_/**/REALKIND
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
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [  0]
  K1(22,:) = [  0]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllaa_nenexuuxaa_1_/**/REALKIND



module ol_forced_parameters_ppllaa_nenexuuxaa_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
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
end module ol_forced_parameters_ppllaa_nenexuuxaa_1_/**/REALKIND

module ol_loop_ppllaa_nenexuuxaa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(4), c(1)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:74)
  ! denominators
  complex(REALKIND), save :: den(63)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,64), Mct(1,64), Mcol_loop(1,64)
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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = (4*CI*eQED**4)/9._/**/REALKIND
    f(2) = (4*CI*countertermnorm*eQED**4*gQCD**2)/9._/**/REALKIND
    f(3) = (4*CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/9._/**/REALKIND
    f(4) = (4*eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND

  c = [ 4*f(4) ]
  c = (1._/**/REALKIND / 3) * c
end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2, POLSEL)
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
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(36)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,6))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,5),wf(:,7))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,4),wf(:,8))
  call vert_VQ_A(wf(:,-5),wf(:,5),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,11),ZERO,0_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,36),ZERO,0_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,24),ZERO,0_intkind1,wf(:,14))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,13),wf(:,15))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,19))
  call vert_AV_Q(wf(:,6),wf(:,-4),wf(:,20))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,5),wf(:,21))
  call counter_VQ_A(wf(:,-5),wf(:,5),wf(:,22))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,13),wf(:,23))
  call counter_AV_Q(wf(:,14),wf(:,-5),wf(:,24))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,25))
  call counter_AV_Q(wf(:,6),wf(:,-4),wf(:,26))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,4),wf(:,27))
  call prop_Q_A(wf(:,9),Q(:,52),ZERO,0_intkind1,wf(:,28))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,40),ZERO,0_intkind1,wf(:,30))
  call prop_Q_A(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,31))
  call vert_AV_Q(wf(:,30),wf(:,-4),wf(:,32))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,24),ZERO,0_intkind1,wf(:,34))
  call vert_AV_Q(wf(:,34),wf(:,-5),wf(:,35))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,-2),wf(:,36))
  call prop_A_Q(wf(:,17),Q(:,56),ZERO,0_intkind1,wf(:,37))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,36),ZERO,0_intkind1,wf(:,39))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,39),wf(:,40))
  call prop_A_Q(wf(:,20),Q(:,56),ZERO,0_intkind1,wf(:,41))
  call vert_VQ_A(wf(:,-4),wf(:,39),wf(:,42))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,20),ZERO,0_intkind1,wf(:,44))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,44),wf(:,45))
  call vert_VQ_A(wf(:,-5),wf(:,44),wf(:,46))
  call vert_AZ_Q(gZu,wf(:,6),wf(:,4),wf(:,47))
  call counter_Q_A(ctqq,wf(:,5),Q(:,20),wf(:,48))
  call prop_A_Q(wf(:,47),Q(:,43),ZERO,0_intkind1,wf(:,49))
  call counter_A_Q(ctqq,wf(:,6),Q(:,40),wf(:,50))
  call prop_Q_A(wf(:,7),Q(:,23),ZERO,0_intkind1,wf(:,51))
  call prop_Q_A(wf(:,48),Q(:,20),ZERO,0_intkind1,wf(:,52))
  call vert_VQ_A(wf(:,-5),wf(:,52),wf(:,53))
  call counter_A_Q(ctqq,wf(:,10),Q(:,11),wf(:,54))
  call vert_AZ_Q(gZu,wf(:,14),wf(:,4),wf(:,55))
  call counter_Q_A(ctqq,wf(:,13),Q(:,36),wf(:,56))
  call prop_A_Q(wf(:,55),Q(:,27),ZERO,0_intkind1,wf(:,57))
  call counter_A_Q(ctqq,wf(:,14),Q(:,24),wf(:,58))
  call prop_Q_A(wf(:,15),Q(:,39),ZERO,0_intkind1,wf(:,59))
  call counter_Q_A(ctqq,wf(:,18),Q(:,7),wf(:,60))
  call prop_A_Q(wf(:,58),Q(:,24),ZERO,0_intkind1,wf(:,61))
  call vert_AV_Q(wf(:,61),wf(:,-5),wf(:,62))
  call prop_Q_A(wf(:,56),Q(:,36),ZERO,0_intkind1,wf(:,63))
  call vert_VQ_A(wf(:,-4),wf(:,63),wf(:,64))
  call prop_A_Q(wf(:,50),Q(:,40),ZERO,0_intkind1,wf(:,65))
  call vert_AV_Q(wf(:,65),wf(:,-4),wf(:,66))
  call vert_VQ_A(wf(:,-4),wf(:,18),wf(:,67))
  call prop_Q_A(wf(:,67),Q(:,23),ZERO,0_intkind1,wf(:,68))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,39),ZERO,0_intkind1,wf(:,70))
  call vert_AV_Q(wf(:,10),wf(:,-4),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,27),ZERO,0_intkind1,wf(:,72))
  call vert_AV_Q(wf(:,10),wf(:,-5),wf(:,73))
  call prop_A_Q(wf(:,73),Q(:,43),ZERO,0_intkind1,wf(:,74))

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
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,11))
  den(9) = 1 / (Q(5,36))
  den(10) = 1 / (Q(5,24))
  den(13) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,52))
  den(23) = 1 / (Q(5,56))
  den(29) = 1 / (Q(5,43))
  den(32) = 1 / (Q(5,23))
  den(39) = 1 / (Q(5,27))
  den(42) = 1 / (Q(5,39))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(3)*den(14)
  den(19) = den(2)*den(18)
  den(20) = den(1)*den(19)
  den(21) = den(9)*den(18)
  den(22) = den(1)*den(21)
  den(24) = den(10)*den(23)
  den(25) = den(1)*den(24)
  den(26) = den(3)*den(23)
  den(27) = den(1)*den(26)
  den(28) = den(1)*den(3)
  den(30) = den(28)*den(29)
  den(31) = den(2)*den(30)
  den(33) = den(4)*den(32)
  den(34) = den(3)*den(33)
  den(35) = den(2)**2
  den(36) = den(7)*den(35)
  den(37) = den(7)*den(19)
  den(38) = den(1)*den(10)
  den(40) = den(38)*den(39)
  den(41) = den(9)*den(40)
  den(43) = den(11)*den(42)
  den(44) = den(10)*den(43)
  den(45) = den(14)*den(24)
  den(46) = den(10)**2
  den(47) = den(14)*den(46)
  den(48) = den(9)**2
  den(49) = den(7)*den(48)
  den(50) = den(7)*den(21)
  den(51) = den(14)*den(26)
  den(52) = den(3)**2
  den(53) = den(14)*den(52)
  den(54) = den(14)*den(32)
  den(55) = den(14)*den(42)
  den(56) = den(7)*den(39)
  den(57) = den(7)*den(29)
  den(58) = den(1)*den(2)*den(3)
  den(59) = den(1)*den(9)*den(10)
  den(60) = den(2)*den(57)
  den(61) = den(10)*den(55)
  den(62) = den(9)*den(56)
  den(63) = den(3)*den(54)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(36)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(17)

  A(7) = cont_QA(wf(:,6),wf(:,21)) * den(5)
  A(8) = cont_QA(wf(:,10),wf(:,22)) * den(8)
  A(9) = cont_QA(wf(:,14),wf(:,23)) * den(12)
  A(10) = cont_QA(wf(:,18),wf(:,24)) * den(15)
  A(11) = cont_QA(wf(:,10),wf(:,25)) * den(16)
  A(12) = cont_QA(wf(:,18),wf(:,26)) * den(17)
  A(13) = cont_QA(wf(:,27),wf(:,28)) * den(20)
  A(14) = cont_QA(wf(:,7),wf(:,30)) * den(5)
  A(15) = cont_QA(wf(:,27),wf(:,31)) * den(22)
  A(16) = cont_QA(wf(:,18),wf(:,32)) * den(17)
  A(17) = cont_QA(wf(:,15),wf(:,34)) * den(12)
  A(18) = cont_QA(wf(:,18),wf(:,35)) * den(15)
  A(19) = cont_QA(wf(:,36),wf(:,37)) * den(25)
  A(20) = cont_QA(wf(:,14),wf(:,40)) * den(12)
  A(21) = cont_QA(wf(:,36),wf(:,41)) * den(27)
  A(22) = cont_QA(wf(:,10),wf(:,42)) * den(16)
  A(23) = cont_QA(wf(:,6),wf(:,45)) * den(5)
  A(24) = cont_QA(wf(:,10),wf(:,46)) * den(8)
  A(25) = cont_QA(wf(:,48),wf(:,49)) * den(31)
  A(26) = cont_QA(wf(:,50),wf(:,51)) * den(34)
  A(27) = cont_QA(wf(:,10),wf(:,53)) * den(36)
  A(28) = cont_QA(wf(:,28),wf(:,54)) * den(37)
  A(29) = cont_QA(wf(:,56),wf(:,57)) * den(41)
  A(30) = cont_QA(wf(:,58),wf(:,59)) * den(44)
  A(31) = cont_QA(wf(:,37),wf(:,60)) * den(45)
  A(32) = cont_QA(wf(:,18),wf(:,62)) * den(47)
  A(33) = cont_QA(wf(:,10),wf(:,64)) * den(49)
  A(34) = cont_QA(wf(:,31),wf(:,54)) * den(50)
  A(35) = cont_QA(wf(:,41),wf(:,60)) * den(51)
  A(36) = cont_QA(wf(:,18),wf(:,66)) * den(53)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(36)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(2)+A(3)+A(4)+A(5)+A(6))*f(1)

  M2(1) = (-A(25)-A(26)-A(27)-A(28)-A(29)-A(30)-A(31)-A(32)-A(33)-A(34)-A(35)-A(36))*f(2)+(A(7)+A(8)+A(9)+A(10)+A(11)+A(12)+A(13) &
       +A(14)+A(15)+A(16)+A(17)+A(18)+A(19)+A(20)+A(21)+A(22)+A(23)+A(24))*f(3)

end subroutine colourvectors

end module ol_loop_ppllaa_nenexuuxaa_1_/**/REALKIND
