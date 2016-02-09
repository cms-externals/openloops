
module ol_colourmatrix_ppjj_gggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(72,6), K2(6,9), KL(6,9)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  38,  -4,  -4,  -4,  -4,   8]
  K1( 2,:) = [  -4,  38,  -4,   8,  -4,  -4]
  K1( 3,:) = [  -4,  -4,  38,  -4,   8,  -4]
  K1( 4,:) = [  -4,   8,  -4,  38,  -4,  -4]
  K1( 5,:) = [  -4,  -4,   8,  -4,  38,  -4]
  K1( 6,:) = [   8,  -4,  -4,  -4,  -4,  38]
  K1( 7,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 8,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 9,:) = [ -12, -12, 114, -12,  24, -12]
  K1(10,:) = [ -12,  24, -12, 114, -12, -12]
  K1(11,:) = [ -12, -12,  24, -12, 114, -12]
  K1(12,:) = [  24, -12, -12, -12, -12, 114]
  K1(13,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1(14,:) = [  -2, -65,   7, -20,   7,  -2]
  K1(15,:) = [   7,   7,  16,   7,  16,   7]
  K1(16,:) = [  -2, -20,   7, -65,   7,  -2]
  K1(17,:) = [   7,   7,  16,   7,  16,   7]
  K1(18,:) = [ -20,  -2,   7,  -2,   7, -65]
  K1(19,:) = [ 114, -12, -12, -12, -12,  24]
  K1(20,:) = [ -12, 114, -12,  24, -12, -12]
  K1(21,:) = [ -12, -12, 114, -12,  24, -12]
  K1(22,:) = [ -12,  24, -12, 114, -12, -12]
  K1(23,:) = [ -12, -12,  24, -12, 114, -12]
  K1(24,:) = [  24, -12, -12, -12, -12, 114]
  K1(25,:) = [  16,   7,   7,   7,   7,  16]
  K1(26,:) = [   7, -65,  -2, -20,  -2,   7]
  K1(27,:) = [   7,  -2, -65,  -2, -20,   7]
  K1(28,:) = [   7, -20,  -2, -65,  -2,   7]
  K1(29,:) = [   7,  -2, -20,  -2, -65,   7]
  K1(30,:) = [  16,   7,   7,   7,   7,  16]
  K1(31,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1(32,:) = [   7,  16,   7,  16,   7,   7]
  K1(33,:) = [  -2,   7, -65,   7, -20,  -2]
  K1(34,:) = [   7,  16,   7,  16,   7,   7]
  K1(35,:) = [  -2,   7, -20,   7, -65,  -2]
  K1(36,:) = [ -20,   7,  -2,   7,  -2, -65]
  K1(37,:) = [ 114, -12, -12, -12, -12,  24]
  K1(38,:) = [ -12, 114, -12,  24, -12, -12]
  K1(39,:) = [ -12, -12, 114, -12,  24, -12]
  K1(40,:) = [ -12,  24, -12, 114, -12, -12]
  K1(41,:) = [ -12, -12,  24, -12, 114, -12]
  K1(42,:) = [  24, -12, -12, -12, -12, 114]
  K1(43,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1(44,:) = [   7,  16,   7,  16,   7,   7]
  K1(45,:) = [  -2,   7, -65,   7, -20,  -2]
  K1(46,:) = [   7,  16,   7,  16,   7,   7]
  K1(47,:) = [  -2,   7, -20,   7, -65,  -2]
  K1(48,:) = [ -20,   7,  -2,   7,  -2, -65]
  K1(49,:) = [  16,   7,   7,   7,   7,  16]
  K1(50,:) = [   7, -65,  -2, -20,  -2,   7]
  K1(51,:) = [   7,  -2, -65,  -2, -20,   7]
  K1(52,:) = [   7, -20,  -2, -65,  -2,   7]
  K1(53,:) = [   7,  -2, -20,  -2, -65,   7]
  K1(54,:) = [  16,   7,   7,   7,   7,  16]
  K1(55,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1(56,:) = [  -2, -65,   7, -20,   7,  -2]
  K1(57,:) = [   7,   7,  16,   7,  16,   7]
  K1(58,:) = [  -2, -20,   7, -65,   7,  -2]
  K1(59,:) = [   7,   7,  16,   7,  16,   7]
  K1(60,:) = [ -20,  -2,   7,  -2,   7, -65]
  K1(61,:) = [ 114, -12, -12, -12, -12,  24]
  K1(62,:) = [ -12, 114, -12,  24, -12, -12]
  K1(63,:) = [ -12, -12, 114, -12,  24, -12]
  K1(64,:) = [ -12,  24, -12, 114, -12, -12]
  K1(65,:) = [ -12, -12,  24, -12, 114, -12]
  K1(66,:) = [  24, -12, -12, -12, -12, 114]
  K1(67,:) = [   0,   0,   0,   0,   0,   0]
  K1(68,:) = [   0,   0,   0,   0,   0,   0]
  K1(69,:) = [   0,   0,   0,   0,   0,   0]
  K1(70,:) = [   0,   0,   0,   0,   0,   0]
  K1(71,:) = [   0,   0,   0,   0,   0,   0]
  K1(72,:) = [   0,   0,   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 12) * K1

  K2(1,:) = [  8, -1,  8, 19, -2, -2, -2, -2,  4]
  K2(2,:) = [ -1,  8,  8, -2, 19, -2,  4, -2, -2]
  K2(3,:) = [  8,  8, -1, -2, -2, 19, -2,  4, -2]
  K2(4,:) = [ -1,  8,  8, -2,  4, -2, 19, -2, -2]
  K2(5,:) = [  8,  8, -1, -2, -2,  4, -2, 19, -2]
  K2(6,:) = [  8, -1,  8,  4, -2, -2, -2, -2, 19]
  K2 = (1._/**/REALKIND / 6) * K2

  KL(1,:) = [  8, -1,  8, 19, -2, -2, -2, -2,  4]
  KL(2,:) = [ -1,  8,  8, -2, 19, -2,  4, -2, -2]
  KL(3,:) = [  8,  8, -1, -2, -2, 19, -2,  4, -2]
  KL(4,:) = [ -1,  8,  8, -2,  4, -2, 19, -2, -2]
  KL(5,:) = [  8,  8, -1, -2, -2,  4, -2, 19, -2]
  KL(6,:) = [  8, -1,  8,  4, -2, -2, -2, -2, 19]
  KL = (1._/**/REALKIND / 6) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppjj_gggg_1_/**/REALKIND



module ol_forced_parameters_ppjj_gggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppjj_gggg_1_/**/REALKIND

module ol_loop_ppjj_gggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(10)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:21)
  ! denominators
  complex(REALKIND), save :: den(9)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(6,16)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*gQCD**2
    f( 2) = CI*countertermnorm*gQCD**4
    f( 3) = CI*countertermnorm*ctVVV*gQCD**4
    f( 4) = CI*countertermnorm*ctVVVV*gQCD**4
    f( 5) = CI*countertermnorm*gQCD**4*R2GGGG
    f( 6) = (gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f( 7) = gQCD**4*integralnorm*SwB
    f( 8) = CI*gQCD**4*integralnorm*SwF
    f( 9) = 2*CI*gQCD**4*integralnorm*SwF
    f(10) = gQCD**4*integralnorm*SwF
    f(11) = 2*gQCD**4*integralnorm*SwF

  c = [ 2*f(6), 3*f(6), 6*f(6), 2*f(7), 3*f(7), 6*f(7), CI*f(8), CI*f(9), f(10), f(11) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,4)
  integer,           intent(in)  :: H(4)
  complex(REALKIND), intent(out) :: M1(6), M2(9)
  complex(REALKIND) :: A(21)
  ! external WFs
  call wf_V(P(:,1), rZERO, H(1), wf(:,0))
  call wf_V(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_V(P(:,4), rZERO, H(4), wf(:,-3))

  ! internal WFs
  call vert_GGG_G(wf(:,0),wf(:,-1),wf(:,-2),wf(:,1))
  call vert_GGG_G(wf(:,-1),wf(:,-2),wf(:,0),wf(:,2))
  call vert_GGG_G(wf(:,-2),wf(:,0),wf(:,-1),wf(:,3))
  call vert_UV_W(wf(:,0),Q(:,1),wf(:,-1),Q(:,2),wf(:,4))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,5))
  call vert_UV_W(wf(:,0),Q(:,1),wf(:,-2),Q(:,4),wf(:,6))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,7))
  call vert_UV_W(wf(:,0),Q(:,1),wf(:,-3),Q(:,8),wf(:,8))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,9))
  call counter_GGG_G(wf(:,0),wf(:,-1),wf(:,-2),wf(:,10))
  call counter_GGG_G(wf(:,-1),wf(:,-2),wf(:,0),wf(:,11))
  call counter_GGG_G(wf(:,-2),wf(:,0),wf(:,-1),wf(:,12))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,13))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,14))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,15))
  call counter_UV_W(wf(:,0),Q(:,1),wf(:,-3),Q(:,8),wf(:,16))
  call counter_UV_W(wf(:,0),Q(:,1),wf(:,-2),Q(:,4),wf(:,17))
  call counter_UV_W(wf(:,0),Q(:,1),wf(:,-1),Q(:,2),wf(:,18))
  call counter_V_V(ctGG,wf(:,4),Q(:,3),wf(:,19))
  call counter_V_V(ctGG,wf(:,6),Q(:,5),wf(:,20))
  call counter_V_V(ctGG,wf(:,8),Q(:,9),wf(:,21))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,5))
  den(3) = 1 / (Q(5,9))
  den(4) = 1 / (Q(5,6))
  den(5) = 1 / (Q(5,10))
  den(6) = 1 / (Q(5,12))

  ! denominators
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(5)
  den(9) = den(3)*den(4)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(21)

  A(1) = cont_VV(wf(:,-3),wf(:,1))
  A(2) = cont_VV(wf(:,-3),wf(:,2))
  A(3) = cont_VV(wf(:,-3),wf(:,3))
  A(4) = cont_VV(wf(:,4),wf(:,5)) * den(1)
  A(5) = cont_VV(wf(:,6),wf(:,7)) * den(2)
  A(6) = cont_VV(wf(:,8),wf(:,9)) * den(3)

  A(7) = cont_VV(wf(:,-3),wf(:,10))
  A(8) = cont_VV(wf(:,-3),wf(:,11))
  A(9) = cont_VV(wf(:,-3),wf(:,12))
  A(10) = cont_VV(wf(:,-3),wf(:,10))
  A(11) = cont_VV(wf(:,-3),wf(:,11))
  A(12) = cont_VV(wf(:,-3),wf(:,12))
  A(13) = cont_VV(wf(:,4),wf(:,13)) * den(1)
  A(14) = cont_VV(wf(:,6),wf(:,14)) * den(2)
  A(15) = cont_VV(wf(:,8),wf(:,15)) * den(3)
  A(16) = cont_VV(wf(:,9),wf(:,16)) * den(4)
  A(17) = cont_VV(wf(:,7),wf(:,17)) * den(5)
  A(18) = cont_VV(wf(:,5),wf(:,18)) * den(6)
  A(19) = cont_VV(wf(:,5),wf(:,19)) * den(7)
  A(20) = cont_VV(wf(:,7),wf(:,20)) * den(8)
  A(21) = cont_VV(wf(:,9),wf(:,21)) * den(9)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(21)
  complex(REALKIND), intent(out) :: M1(6), M2(9)

  M1(1) = 2*(A(1)-A(2)+A(4)-A(6))*f(1)
  M1(2) = 2*(-A(1)+A(3)-A(4)-A(5))*f(1)
  M1(3) = 2*(A(2)-A(3)+A(5)+A(6))*f(1)
  M1(4) = 2*(-A(1)+A(3)-A(4)-A(5))*f(1)
  M1(5) = 2*(A(2)-A(3)+A(5)+A(6))*f(1)
  M1(6) = 2*(A(1)-A(2)+A(4)-A(6))*f(1)

  M2(1) = (4*(-A(10)-A(11)-A(12))*f(5))/3._/**/REALKIND
  M2(2) = (4*(-A(10)-A(11)-A(12))*f(5))/3._/**/REALKIND
  M2(3) = (4*(-A(10)-A(11)-A(12))*f(5))/3._/**/REALKIND
  M2(4) = 2*(-A(19)+A(21))*f(2)+2*(A(13)-A(15)-A(16)+A(18))*f(3)+2*(-A(7)-A(8))*f(4)+4*A(9)*f(4)+19*(A(10)+A(11))*f(5) &
       -36*A(12)*f(5)
  M2(5) = 2*(A(19)+A(20))*f(2)+2*(-A(13)-A(14)-A(17)-A(18))*f(3)+4*A(8)*f(4)+2*(-A(7)-A(9))*f(4)-36*A(11)*f(5)+19*(A(10) &
       +A(12))*f(5)
  M2(6) = 2*(-A(20)-A(21))*f(2)+2*(A(14)+A(15)+A(16)+A(17))*f(3)+4*A(7)*f(4)+2*(-A(8)-A(9))*f(4)-36*A(10)*f(5)+19*(A(11) &
       +A(12))*f(5)
  M2(7) = 2*(A(19)+A(20))*f(2)+2*(-A(13)-A(14)-A(17)-A(18))*f(3)+4*A(8)*f(4)+2*(-A(7)-A(9))*f(4)-36*A(11)*f(5)+19*(A(10) &
       +A(12))*f(5)
  M2(8) = 2*(-A(20)-A(21))*f(2)+2*(A(14)+A(15)+A(16)+A(17))*f(3)+4*A(7)*f(4)+2*(-A(8)-A(9))*f(4)-36*A(10)*f(5)+19*(A(11) &
       +A(12))*f(5)
  M2(9) = 2*(-A(19)+A(21))*f(2)+2*(A(13)-A(15)-A(16)+A(18))*f(3)+2*(-A(7)-A(8))*f(4)+4*A(9)*f(4)+19*(A(10)+A(11))*f(5) &
       -36*A(12)*f(5)

end subroutine colourvectors

end module ol_loop_ppjj_gggg_1_/**/REALKIND
