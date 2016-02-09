
module ol_colourmatrix_pphjj2_hgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,6), KL(0,6), KL2(6,6), KL2ct(6,6), KL2ct2(6,6)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [ 19, -2, -2, -2, -2,  4]
  KL2(2,:) = [ -2, 19, -2,  4, -2, -2]
  KL2(3,:) = [ -2, -2, 19, -2,  4, -2]
  KL2(4,:) = [ -2,  4, -2, 19, -2, -2]
  KL2(5,:) = [ -2, -2,  4, -2, 19, -2]
  KL2(6,:) = [  4, -2, -2, -2, -2, 19]
  KL2 = (1._/**/REALKIND / 6) * KL2

  KL2ct(1,:) = [ 19, -2, -2, -2, -2,  4]
  KL2ct(2,:) = [ -2, 19, -2,  4, -2, -2]
  KL2ct(3,:) = [ -2, -2, 19, -2,  4, -2]
  KL2ct(4,:) = [ -2,  4, -2, 19, -2, -2]
  KL2ct(5,:) = [ -2, -2,  4, -2, 19, -2]
  KL2ct(6,:) = [  4, -2, -2, -2, -2, 19]
  KL2ct = (1._/**/REALKIND / 6) * KL2ct

  KL2ct2(1,:) = [ 19, -2, -2, -2, -2,  4]
  KL2ct2(2,:) = [ -2, 19, -2,  4, -2, -2]
  KL2ct2(3,:) = [ -2, -2, 19, -2,  4, -2]
  KL2ct2(4,:) = [ -2,  4, -2, 19, -2, -2]
  KL2ct2(5,:) = [ -2, -2,  4, -2, 19, -2]
  KL2ct2(6,:) = [  4, -2, -2, -2, -2, 19]
  KL2ct2 = (1._/**/REALKIND / 6) * KL2ct2

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphjj2_hgggg_1_/**/REALKIND



module ol_forced_parameters_pphjj2_hgggg_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphjj2_hgggg_1_/**/REALKIND

module ol_loop_pphjj2_hgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(6), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:49)
  ! denominators
  complex(REALKIND), save :: den(41)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB, DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MB*YB)/MQ2sum
    f(2) = (CI*eQED*gQCD**4*integralnorm*SwF*YB)/(2._/**/REALKIND*MW*sw)
    f(3) = (eQED*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(4) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MT*YT)/MQ2sum
    f(5) = (CI*eQED*gQCD**4*integralnorm*SwF*YT)/(2._/**/REALKIND*MW*sw)
    f(6) = (eQED*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)


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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  integer,           intent(in), optional  :: POLSEL(5)
  complex(REALKIND), intent(out) :: M1(0), M2(6)
  complex(REALKIND) :: A(54)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), POLSEL(1))
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), 0)
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_GGG_G(wf(:,-1),wf(:,-2),wf(:,-3),wf(:,1))
  call counter_SG_G(wf(:,0),wf(:,-4),wf(:,2))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-1),wf(:,3))
  call vert_GGG_G(wf(:,-3),wf(:,-1),wf(:,-2),wf(:,4))
  call vert_GGG_G(wf(:,-1),wf(:,-2),wf(:,-4),wf(:,5))
  call counter_SG_G(wf(:,0),wf(:,-3),wf(:,6))
  call vert_GGG_G(wf(:,-2),wf(:,-4),wf(:,-1),wf(:,7))
  call vert_GGG_G(wf(:,-4),wf(:,-1),wf(:,-2),wf(:,8))
  call vert_GGG_G(wf(:,-1),wf(:,-3),wf(:,-4),wf(:,9))
  call counter_SG_G(wf(:,0),wf(:,-2),wf(:,10))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-1),wf(:,11))
  call vert_GGG_G(wf(:,-4),wf(:,-1),wf(:,-3),wf(:,12))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,13))
  call counter_SG_G(wf(:,0),wf(:,-1),wf(:,14))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,15))
  call vert_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,16))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,17))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,18))
  call counter_SG_G(wf(:,0),wf(:,17),wf(:,19))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,2),Q(:,17),wf(:,20))
  call vert_UV_W(wf(:,6),Q(:,9),wf(:,-4),Q(:,16),wf(:,21))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,22))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,23))
  call counter_SG_G(wf(:,0),wf(:,22),wf(:,24))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,2),Q(:,17),wf(:,25))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-4),Q(:,16),wf(:,26))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,27))
  call counter_SG_G(wf(:,0),wf(:,26),wf(:,28))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,2),Q(:,17),wf(:,29))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,6),Q(:,9),wf(:,30))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,6),Q(:,9),wf(:,31))
  call vert_UV_W(wf(:,10),Q(:,5),wf(:,-4),Q(:,16),wf(:,32))
  call vert_UV_W(wf(:,10),Q(:,5),wf(:,-3),Q(:,8),wf(:,33))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,10),Q(:,5),wf(:,34))
  call vert_UV_W(wf(:,14),Q(:,3),wf(:,-4),Q(:,16),wf(:,35))
  call vert_UV_W(wf(:,14),Q(:,3),wf(:,-3),Q(:,8),wf(:,36))
  call vert_UV_W(wf(:,14),Q(:,3),wf(:,-2),Q(:,4),wf(:,37))
  call vert_UV_W(wf(:,17),Q(:,6),wf(:,-3),Q(:,8),wf(:,38))
  call vert_UV_W(wf(:,17),Q(:,6),wf(:,-4),Q(:,16),wf(:,39))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,22),Q(:,10),wf(:,40))
  call vert_UV_W(wf(:,22),Q(:,10),wf(:,-4),Q(:,16),wf(:,41))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,27),Q(:,12),wf(:,42))
  call vert_UV_W(wf(:,27),Q(:,12),wf(:,-4),Q(:,16),wf(:,43))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,26),Q(:,18),wf(:,44))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,26),Q(:,18),wf(:,45))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,23),Q(:,20),wf(:,46))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,23),Q(:,20),wf(:,47))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,18),Q(:,24),wf(:,48))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,18),Q(:,24),wf(:,49))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,14))
  den(2) = 1 / (Q(5,22))
  den(3) = 1 / (Q(5,26))
  den(4) = 1 / (Q(5,28))
  den(5) = 1 / (Q(5,6))
  den(6) = 1 / (Q(5,24))
  den(8) = 1 / (Q(5,17))
  den(10) = 1 / (Q(5,9))
  den(12) = 1 / (Q(5,10))
  den(13) = 1 / (Q(5,20))
  den(16) = 1 / (Q(5,18))
  den(17) = 1 / (Q(5,12))
  den(22) = 1 / (Q(5,5))
  den(26) = 1 / (Q(5,3))

  ! denominators
  den(7) = den(5)*den(6)
  den(9) = den(5)*den(8)
  den(11) = den(5)*den(10)
  den(14) = den(12)*den(13)
  den(15) = den(8)*den(12)
  den(18) = den(16)*den(17)
  den(19) = den(8)*den(17)
  den(20) = den(10)*den(16)
  den(21) = den(10)*den(13)
  den(23) = den(12)*den(22)
  den(24) = den(16)*den(22)
  den(25) = den(6)*den(22)
  den(27) = den(17)*den(26)
  den(28) = den(13)*den(26)
  den(29) = den(6)*den(26)
  den(30) = den(1)*den(5)
  den(31) = den(2)*den(5)
  den(32) = den(1)*den(12)
  den(33) = den(3)*den(12)
  den(34) = den(1)*den(17)
  den(35) = den(4)*den(17)
  den(36) = den(2)*den(16)
  den(37) = den(3)*den(16)
  den(38) = den(2)*den(13)
  den(39) = den(4)*den(13)
  den(40) = den(3)*den(6)
  den(41) = den(4)*den(6)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(54)


  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,2),wf(:,3)) * den(1)
  A(3) = cont_VV(wf(:,2),wf(:,4)) * den(1)
  A(4) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(5) = cont_VV(wf(:,2),wf(:,3)) * den(1)
  A(6) = cont_VV(wf(:,2),wf(:,4)) * den(1)
  A(7) = cont_VV(wf(:,5),wf(:,6)) * den(2)
  A(8) = cont_VV(wf(:,6),wf(:,7)) * den(2)
  A(9) = cont_VV(wf(:,6),wf(:,8)) * den(2)
  A(10) = cont_VV(wf(:,5),wf(:,6)) * den(2)
  A(11) = cont_VV(wf(:,6),wf(:,7)) * den(2)
  A(12) = cont_VV(wf(:,6),wf(:,8)) * den(2)
  A(13) = cont_VV(wf(:,9),wf(:,10)) * den(3)
  A(14) = cont_VV(wf(:,10),wf(:,11)) * den(3)
  A(15) = cont_VV(wf(:,10),wf(:,12)) * den(3)
  A(16) = cont_VV(wf(:,9),wf(:,10)) * den(3)
  A(17) = cont_VV(wf(:,10),wf(:,11)) * den(3)
  A(18) = cont_VV(wf(:,10),wf(:,12)) * den(3)
  A(19) = cont_VV(wf(:,13),wf(:,14)) * den(4)
  A(20) = cont_VV(wf(:,14),wf(:,15)) * den(4)
  A(21) = cont_VV(wf(:,14),wf(:,16)) * den(4)
  A(22) = cont_VV(wf(:,13),wf(:,14)) * den(4)
  A(23) = cont_VV(wf(:,14),wf(:,15)) * den(4)
  A(24) = cont_VV(wf(:,14),wf(:,16)) * den(4)
  A(25) = cont_VV(wf(:,18),wf(:,19)) * den(7)
  A(26) = cont_VV(wf(:,18),wf(:,19)) * den(7)
  A(27) = cont_VV(wf(:,17),wf(:,20)) * den(9)
  A(28) = cont_VV(wf(:,17),wf(:,20)) * den(9)
  A(29) = cont_VV(wf(:,17),wf(:,21)) * den(11)
  A(30) = cont_VV(wf(:,17),wf(:,21)) * den(11)
  A(31) = cont_VV(wf(:,23),wf(:,24)) * den(14)
  A(32) = cont_VV(wf(:,23),wf(:,24)) * den(14)
  A(33) = cont_VV(wf(:,22),wf(:,25)) * den(15)
  A(34) = cont_VV(wf(:,22),wf(:,25)) * den(15)
  A(35) = cont_VV(wf(:,27),wf(:,28)) * den(18)
  A(36) = cont_VV(wf(:,27),wf(:,28)) * den(18)
  A(37) = cont_VV(wf(:,27),wf(:,29)) * den(19)
  A(38) = cont_VV(wf(:,27),wf(:,29)) * den(19)
  A(39) = cont_VV(wf(:,26),wf(:,30)) * den(20)
  A(40) = cont_VV(wf(:,26),wf(:,30)) * den(20)
  A(41) = cont_VV(wf(:,23),wf(:,31)) * den(21)
  A(42) = cont_VV(wf(:,23),wf(:,31)) * den(21)
  A(43) = cont_VV(wf(:,22),wf(:,32)) * den(23)
  A(44) = cont_VV(wf(:,22),wf(:,32)) * den(23)
  A(45) = cont_VV(wf(:,26),wf(:,33)) * den(24)
  A(46) = cont_VV(wf(:,26),wf(:,33)) * den(24)
  A(47) = cont_VV(wf(:,18),wf(:,34)) * den(25)
  A(48) = cont_VV(wf(:,18),wf(:,34)) * den(25)
  A(49) = cont_VV(wf(:,27),wf(:,35)) * den(27)
  A(50) = cont_VV(wf(:,27),wf(:,35)) * den(27)
  A(51) = cont_VV(wf(:,23),wf(:,36)) * den(28)
  A(52) = cont_VV(wf(:,23),wf(:,36)) * den(28)
  A(53) = cont_VV(wf(:,18),wf(:,37)) * den(29)
  A(54) = cont_VV(wf(:,18),wf(:,37)) * den(29)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(54)
  complex(REALKIND), intent(out) :: M1(0), M2(6)


  M2(1) = 2*(A(4)-A(5)-A(10)+A(12)+A(17)-A(18)+A(22)-A(23)+A(26)+A(28)+A(30)-A(36)-A(38)-A(40)-A(46)+A(48)-A(50)+A(54))*f(1) &
       +2*(A(1)-A(2)-A(7)+A(9)+A(14)-A(15)+A(19)-A(20)+A(25)+A(27)+A(29)-A(35)-A(37)-A(39)-A(45)+A(47)-A(49)+A(53))*f(4)
  M2(2) = 2*(-A(4)+A(6)+A(10)-A(11)+A(16)-A(17)+A(23)-A(24)-A(26)-A(28)-A(30)-A(32)-A(34)-A(42)-A(44)-A(48)-A(52)-A(54))*f(1)+2*( &
       -A(1)+A(3)+A(7)-A(8)+A(13)-A(14)+A(20)-A(21)-A(25)-A(27)-A(29)-A(31)-A(33)-A(41)-A(43)-A(47)-A(51)-A(53))*f(4)
  M2(3) = 2*(A(5)-A(6)+A(11)-A(12)-A(16)+A(18)-A(22)+A(24)+A(32)+A(34)+A(36)+A(38)+A(40)+A(42)+A(44)+A(46)+A(50)+A(52))*f(1) &
       +2*(A(2)-A(3)+A(8)-A(9)-A(13)+A(15)-A(19)+A(21)+A(31)+A(33)+A(35)+A(37)+A(39)+A(41)+A(43)+A(45)+A(49)+A(51))*f(4)
  M2(4) = 2*(-A(4)+A(6)+A(10)-A(11)+A(16)-A(17)+A(23)-A(24)-A(26)-A(28)-A(30)-A(32)-A(34)-A(42)-A(44)-A(48)-A(52)-A(54))*f(1)+2*( &
       -A(1)+A(3)+A(7)-A(8)+A(13)-A(14)+A(20)-A(21)-A(25)-A(27)-A(29)-A(31)-A(33)-A(41)-A(43)-A(47)-A(51)-A(53))*f(4)
  M2(5) = 2*(A(5)-A(6)+A(11)-A(12)-A(16)+A(18)-A(22)+A(24)+A(32)+A(34)+A(36)+A(38)+A(40)+A(42)+A(44)+A(46)+A(50)+A(52))*f(1) &
       +2*(A(2)-A(3)+A(8)-A(9)-A(13)+A(15)-A(19)+A(21)+A(31)+A(33)+A(35)+A(37)+A(39)+A(41)+A(43)+A(45)+A(49)+A(51))*f(4)
  M2(6) = 2*(A(4)-A(5)-A(10)+A(12)+A(17)-A(18)+A(22)-A(23)+A(26)+A(28)+A(30)-A(36)-A(38)-A(40)-A(46)+A(48)-A(50)+A(54))*f(1) &
       +2*(A(1)-A(2)-A(7)+A(9)+A(14)-A(15)+A(19)-A(20)+A(25)+A(27)+A(29)-A(35)-A(37)-A(39)-A(45)+A(47)-A(49)+A(53))*f(4)

end subroutine colourvectors

end module ol_loop_pphjj2_hgggg_1_/**/REALKIND
