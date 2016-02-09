
module ol_colourmatrix_ppvvv_uuxazz_1_/**/REALKIND
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

  K1( 1,:) = [  3]
  K1( 2,:) = [  4]
  K1( 3,:) = [ -4]
  K1( 4,:) = [  4]
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
  K1(16,:) = [  0]
  K1(17,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvv_uuxazz_1_/**/REALKIND



module ol_forced_parameters_ppvvv_uuxazz_1_/**/REALKIND
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
end module ol_forced_parameters_ppvvv_uuxazz_1_/**/REALKIND

module ol_loop_ppvvv_uuxazz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(4), c(1)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:66)
  ! denominators
  complex(REALKIND), save :: den(42)
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
    f(1) = (2*CI*eQED**3)/3._/**/REALKIND
    f(2) = (2*CI*countertermnorm*eQED**3*gQCD**2)/3._/**/REALKIND
    f(3) = (2*CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/3._/**/REALKIND
    f(4) = (2*eQED**3*gQCD**2*integralnorm*SwB)/3._/**/REALKIND

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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(36)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))

  ! internal WFs
  call vert_VQ_A(wf(:,-2),wf(:,0),wf(:,1))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),ZERO,0_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,10),ZERO,0_intkind1,wf(:,4))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,3),wf(:,5))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-4),wf(:,6))
  call prop_A_Q(wf(:,6),Q(:,18),ZERO,0_intkind1,wf(:,7))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,3),wf(:,8))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,0),wf(:,9))
  call vert_AV_Q(wf(:,-1),wf(:,-2),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,9),ZERO,0_intkind1,wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,6),ZERO,0_intkind1,wf(:,12))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,11),wf(:,13))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,14))
  call prop_Q_A(wf(:,14),Q(:,17),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,15),wf(:,16))
  call vert_VQ_A(wf(:,-2),wf(:,11),wf(:,17))
  call vert_VQ_A(wf(:,-2),wf(:,15),wf(:,18))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,3),wf(:,19))
  call counter_ZQ_A(gZu,wf(:,-3),wf(:,3),wf(:,20))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,11),wf(:,21))
  call counter_ZQ_A(gZu,wf(:,-3),wf(:,15),wf(:,22))
  call counter_VQ_A(wf(:,-2),wf(:,11),wf(:,23))
  call counter_VQ_A(wf(:,-2),wf(:,15),wf(:,24))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-4),wf(:,25))
  call prop_A_Q(wf(:,25),Q(:,18),ZERO,0_intkind1,wf(:,26))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-3),wf(:,27))
  call prop_A_Q(wf(:,27),Q(:,10),ZERO,0_intkind1,wf(:,28))
  call counter_AV_Q(wf(:,-1),wf(:,-2),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,6),ZERO,0_intkind1,wf(:,30))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,17),ZERO,0_intkind1,wf(:,32))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,32),wf(:,33))
  call counter_ZQ_A(gZu,wf(:,-3),wf(:,0),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,9),ZERO,0_intkind1,wf(:,35))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,35),wf(:,36))
  call vert_VQ_A(wf(:,-2),wf(:,32),wf(:,37))
  call vert_VQ_A(wf(:,-2),wf(:,35),wf(:,38))
  call counter_VQ_A(wf(:,-2),wf(:,0),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,5),ZERO,0_intkind1,wf(:,40))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,40),wf(:,41))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,40),wf(:,42))
  call vert_AZ_Q(gZu,wf(:,4),wf(:,-4),wf(:,43))
  call counter_Q_A(ctqq,wf(:,3),Q(:,5),wf(:,44))
  call prop_A_Q(wf(:,43),Q(:,26),ZERO,0_intkind1,wf(:,45))
  call counter_A_Q(ctqq,wf(:,4),Q(:,10),wf(:,46))
  call prop_Q_A(wf(:,5),Q(:,21),ZERO,0_intkind1,wf(:,47))
  call vert_AZ_Q(gZu,wf(:,7),wf(:,-3),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,26),ZERO,0_intkind1,wf(:,49))
  call counter_A_Q(ctqq,wf(:,7),Q(:,18),wf(:,50))
  call prop_Q_A(wf(:,8),Q(:,13),ZERO,0_intkind1,wf(:,51))
  call vert_AZ_Q(gZu,wf(:,12),wf(:,-4),wf(:,52))
  call counter_Q_A(ctqq,wf(:,11),Q(:,9),wf(:,53))
  call prop_A_Q(wf(:,52),Q(:,22),ZERO,0_intkind1,wf(:,54))
  call counter_A_Q(ctqq,wf(:,12),Q(:,6),wf(:,55))
  call prop_Q_A(wf(:,13),Q(:,25),ZERO,0_intkind1,wf(:,56))
  call vert_AZ_Q(gZu,wf(:,12),wf(:,-3),wf(:,57))
  call counter_Q_A(ctqq,wf(:,15),Q(:,17),wf(:,58))
  call prop_A_Q(wf(:,57),Q(:,14),ZERO,0_intkind1,wf(:,59))
  call prop_Q_A(wf(:,16),Q(:,25),ZERO,0_intkind1,wf(:,60))
  call vert_AV_Q(wf(:,7),wf(:,-2),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,22),ZERO,0_intkind1,wf(:,62))
  call prop_Q_A(wf(:,17),Q(:,13),ZERO,0_intkind1,wf(:,63))
  call vert_AV_Q(wf(:,4),wf(:,-2),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,14),ZERO,0_intkind1,wf(:,65))
  call prop_Q_A(wf(:,18),Q(:,21),ZERO,0_intkind1,wf(:,66))

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
  den(13) = 1 / (Q(5,26))
  den(16) = 1 / (Q(5,21))
  den(21) = 1 / (Q(5,13))
  den(24) = 1 / (Q(5,22))
  den(27) = 1 / (Q(5,25))
  den(30) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(11) = den(4)*den(6)
  den(12) = den(2)*den(9)
  den(14) = den(2)*den(13)
  den(15) = den(1)*den(14)
  den(17) = den(1)*den(16)
  den(18) = den(2)*den(17)
  den(19) = den(4)*den(13)
  den(20) = den(1)*den(19)
  den(22) = den(1)*den(21)
  den(23) = den(4)*den(22)
  den(25) = den(7)*den(24)
  den(26) = den(6)*den(25)
  den(28) = den(6)*den(27)
  den(29) = den(7)*den(28)
  den(31) = den(7)*den(30)
  den(32) = den(9)*den(31)
  den(33) = den(9)*den(27)
  den(34) = den(7)*den(33)
  den(35) = den(4)*den(24)
  den(36) = den(6)*den(35)
  den(37) = den(6)*den(21)
  den(38) = den(4)*den(37)
  den(39) = den(2)*den(30)
  den(40) = den(9)*den(39)
  den(41) = den(9)*den(16)
  den(42) = den(2)*den(41)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(36)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(8)
  A(4) = cont_QA(wf(:,12),wf(:,16)) * den(10)
  A(5) = cont_QA(wf(:,7),wf(:,17)) * den(11)
  A(6) = cont_QA(wf(:,4),wf(:,18)) * den(12)

  A(7) = cont_QA(wf(:,4),wf(:,19)) * den(3)
  A(8) = cont_QA(wf(:,7),wf(:,20)) * den(5)
  A(9) = cont_QA(wf(:,12),wf(:,21)) * den(8)
  A(10) = cont_QA(wf(:,12),wf(:,22)) * den(10)
  A(11) = cont_QA(wf(:,7),wf(:,23)) * den(11)
  A(12) = cont_QA(wf(:,4),wf(:,24)) * den(12)
  A(13) = cont_QA(wf(:,8),wf(:,26)) * den(5)
  A(14) = cont_QA(wf(:,5),wf(:,28)) * den(3)
  A(15) = cont_QA(wf(:,17),wf(:,26)) * den(11)
  A(16) = cont_QA(wf(:,18),wf(:,28)) * den(12)
  A(17) = cont_QA(wf(:,13),wf(:,30)) * den(8)
  A(18) = cont_QA(wf(:,16),wf(:,30)) * den(10)
  A(19) = cont_QA(wf(:,12),wf(:,33)) * den(10)
  A(20) = cont_QA(wf(:,12),wf(:,36)) * den(8)
  A(21) = cont_QA(wf(:,4),wf(:,37)) * den(12)
  A(22) = cont_QA(wf(:,7),wf(:,38)) * den(11)
  A(23) = cont_QA(wf(:,4),wf(:,41)) * den(3)
  A(24) = cont_QA(wf(:,7),wf(:,42)) * den(5)
  A(25) = cont_QA(wf(:,44),wf(:,45)) * den(15)
  A(26) = cont_QA(wf(:,46),wf(:,47)) * den(18)
  A(27) = cont_QA(wf(:,44),wf(:,49)) * den(20)
  A(28) = cont_QA(wf(:,50),wf(:,51)) * den(23)
  A(29) = cont_QA(wf(:,53),wf(:,54)) * den(26)
  A(30) = cont_QA(wf(:,55),wf(:,56)) * den(29)
  A(31) = cont_QA(wf(:,58),wf(:,59)) * den(32)
  A(32) = cont_QA(wf(:,55),wf(:,60)) * den(34)
  A(33) = cont_QA(wf(:,53),wf(:,62)) * den(36)
  A(34) = cont_QA(wf(:,50),wf(:,63)) * den(38)
  A(35) = cont_QA(wf(:,58),wf(:,65)) * den(40)
  A(36) = cont_QA(wf(:,46),wf(:,66)) * den(42)

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

end module ol_loop_ppvvv_uuxazz_1_/**/REALKIND
