
module ol_colourmatrix_ppajj2_agggg_1_/**/REALKIND
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
end module ol_colourmatrix_ppajj2_agggg_1_/**/REALKIND



module ol_forced_parameters_ppajj2_agggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppajj2_agggg_1_/**/REALKIND

module ol_loop_ppajj2_agggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:42)
  ! denominators
  complex(REALKIND), save :: den(25)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,32)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB, DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = countertermnorm*eQED*gQCD**4
    f(2) = (CI*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(3) = (2*CI*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(4) = (4*CI*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(5) = (eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(6) = (2*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(7) = (4*eQED*gQCD**4*integralnorm*SwF)/3._/**/REALKIND


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
  complex(REALKIND) :: A(12)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_V(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_V(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,1))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-3),wf(:,-4),wf(:,2))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-4),wf(:,-3),wf(:,3))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,4))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-2),wf(:,-4),wf(:,5))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-4),wf(:,-2),wf(:,6))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-4),Q(:,16),wf(:,7))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-2),wf(:,-3),wf(:,8))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-3),wf(:,-2),wf(:,9))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,10))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-1),wf(:,-4),wf(:,11))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-4),wf(:,-1),wf(:,12))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,13))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-1),wf(:,-3),wf(:,14))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-3),wf(:,-1),wf(:,15))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,16))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-1),wf(:,-2),wf(:,17))
  call counter_VGG_G(ctAGGG,wf(:,0),wf(:,-2),wf(:,-1),wf(:,18))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,19))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,20))
  call vert_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,21))
  call vert_GGG_G(wf(:,-1),wf(:,-3),wf(:,-4),wf(:,22))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-1),wf(:,23))
  call vert_GGG_G(wf(:,-4),wf(:,-1),wf(:,-3),wf(:,24))
  call vert_GGG_G(wf(:,-1),wf(:,-2),wf(:,-4),wf(:,25))
  call vert_GGG_G(wf(:,-2),wf(:,-4),wf(:,-1),wf(:,26))
  call vert_GGG_G(wf(:,-4),wf(:,-1),wf(:,-2),wf(:,27))
  call vert_GGG_G(wf(:,-1),wf(:,-2),wf(:,-3),wf(:,28))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-1),wf(:,29))
  call vert_GGG_G(wf(:,-3),wf(:,-1),wf(:,-2),wf(:,30))
  call vert_UV_W(wf(:,1),Q(:,6),wf(:,-3),Q(:,8),wf(:,31))
  call vert_UV_W(wf(:,1),Q(:,6),wf(:,-4),Q(:,16),wf(:,32))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,4),Q(:,10),wf(:,33))
  call vert_UV_W(wf(:,4),Q(:,10),wf(:,-4),Q(:,16),wf(:,34))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,10),Q(:,12),wf(:,35))
  call vert_UV_W(wf(:,10),Q(:,12),wf(:,-4),Q(:,16),wf(:,36))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,7),Q(:,18),wf(:,37))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,7),Q(:,18),wf(:,38))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,13),Q(:,20),wf(:,39))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,13),Q(:,20),wf(:,40))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,16),Q(:,24),wf(:,41))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,16),Q(:,24),wf(:,42))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,6))
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,18))
  den(4) = 1 / (Q(5,12))
  den(5) = 1 / (Q(5,20))
  den(6) = 1 / (Q(5,24))
  den(7) = 1 / (Q(5,28))
  den(8) = 1 / (Q(5,26))
  den(9) = 1 / (Q(5,22))
  den(10) = 1 / (Q(5,14))

  ! denominators
  den(11) = den(1)*den(10)
  den(12) = den(1)*den(9)
  den(13) = den(2)*den(10)
  den(14) = den(2)*den(8)
  den(15) = den(4)*den(10)
  den(16) = den(4)*den(7)
  den(17) = den(3)*den(9)
  den(18) = den(3)*den(8)
  den(19) = den(5)*den(9)
  den(20) = den(5)*den(7)
  den(21) = den(6)*den(8)
  den(22) = den(6)*den(7)
  den(23) = den(1)*den(6)
  den(24) = den(2)*den(5)
  den(25) = den(3)*den(4)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(12)


  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,1),wf(:,3)) * den(1)
  A(3) = cont_VV(wf(:,4),wf(:,5)) * den(2)
  A(4) = cont_VV(wf(:,4),wf(:,6)) * den(2)
  A(5) = cont_VV(wf(:,7),wf(:,8)) * den(3)
  A(6) = cont_VV(wf(:,7),wf(:,9)) * den(3)
  A(7) = cont_VV(wf(:,10),wf(:,11)) * den(4)
  A(8) = cont_VV(wf(:,10),wf(:,12)) * den(4)
  A(9) = cont_VV(wf(:,13),wf(:,14)) * den(5)
  A(10) = cont_VV(wf(:,13),wf(:,15)) * den(5)
  A(11) = cont_VV(wf(:,16),wf(:,17)) * den(6)
  A(12) = cont_VV(wf(:,16),wf(:,18)) * den(6)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(12)
  complex(REALKIND), intent(out) :: M1(0), M2(6)


  M2(1) = CI*(-A(1)+A(5)-A(8)-A(11))*f(1)
  M2(2) = CI*(-A(2)+A(3)-A(10)+A(11))*f(1)
  M2(3) = CI*(-A(3)+A(6)+A(8)-A(9))*f(1)
  M2(4) = CI*(A(1)-A(4)+A(9)-A(12))*f(1)
  M2(5) = CI*(A(4)-A(5)-A(7)+A(10))*f(1)
  M2(6) = CI*(A(2)-A(6)+A(7)+A(12))*f(1)

end subroutine colourvectors

end module ol_loop_ppajj2_agggg_1_/**/REALKIND
