
module ol_colourmatrix_heftpphhj_hhggg_1_/**/REALKIND
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

  K1( 1,:) = [  14,  -4]
  K1( 2,:) = [  -4,  14]
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
  K1(13,:) = [  42, -12]
  K1(14,:) = [ -12,  42]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [ -21,   6]
  K1(20,:) = [   6, -21]
  K1(21,:) = [  42, -12]
  K1(22,:) = [ -12,  42]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [ -21,   6]
  K1(28,:) = [   6, -21]
  K1(29,:) = [ -21,   6]
  K1(30,:) = [   6, -21]
  K1(31,:) = [  42, -12]
  K1(32,:) = [ -12,  42]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 6) * K1

  K2(1,:) = [  7, -2]
  K2(2,:) = [ -2,  7]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [  7, -2]
  KL(2,:) = [ -2,  7]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphhj_hhggg_1_/**/REALKIND



module ol_forced_parameters_heftpphhj_hhggg_1_/**/REALKIND
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
  if (MB /= 0) write(*,101) 'MB = 0'
  if (YB /= 0) write(*,101) 'YB = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphhj_hhggg_1_/**/REALKIND

module ol_loop_heftpphhj_hhggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(21), c(16)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:74)
  ! denominators
  complex(REALKIND), save :: den(43)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,8)
  ! zero helicity identifier
  logical,           save :: zerohel(8) = .true., zerohel_ct(8) = .true.

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
    f( 1) = (DOI*eQED**2*gQCD**5)/(MW**2*pi**4*sw**2*1152._/**/REALKIND)
    f( 2) = (eQED**2*gQCD**3)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f( 3) = (countertermnorm*eQED**2*gQCD**5)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f( 4) = (countertermnorm*ctHEFTgggh*eQED**2*gQCD**5)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f( 5) = (countertermnorm*ctVVV*eQED**2*gQCD**5)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f( 6) = (eQED**2*gQCD**3*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f( 7) = (countertermnorm*eQED**2*gQCD**5*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f( 8) = (countertermnorm*ctHEFTgggh*eQED**2*gQCD**5*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f( 9) = (countertermnorm*ctVVV*eQED**2*gQCD**5*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(10) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/(96._/**/REALKIND*MW**2*pi**2*sw**2)
    f(11) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(12) = (CI*eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwB)/(32._/**/REALKIND*MW**2*pi**2*sw**2)
    f(13) = (CI*eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwB)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(14) = (CI*eQED**2*gQCD**5*integralnorm*SwF)/(24._/**/REALKIND*MW**2*pi**2*sw**2)
    f(15) = (CI*eQED**2*gQCD**5*integralnorm*SwF)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(16) = (eQED**2*gQCD**5*integralnorm*SwF)/(MW**2*pi**2*sw**2*24._/**/REALKIND)
    f(17) = (eQED**2*gQCD**5*integralnorm*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(18) = (CI*eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwF)/(8._/**/REALKIND*MW**2*pi**2*sw**2)
    f(19) = (3*CI*eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwF)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(20) = (eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*8._/**/REALKIND)
    f(21) = (3*eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)

  c = [ 3*CI*f(10), 6*CI*f(10), 3*CI*f(11), 6*CI*f(11), 3*CI*f(12), 6*CI*f(12), 3*CI*f(13), 6*CI*f(13), CI*f(14), CI*f(15), f(16) &
    , f(17), CI*f(18), CI*f(19), f(20), f(21) ]
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(64)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), POLSEL(1))
    call pol_wf_S(P(:,2), rMH, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), 0)
    call pol_wf_S(P(:,2), rMH, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,1),Q(:,15))
  call vert_SS_S(wf(:,0),wf(:,-1),wf(:,2))
  call vert_GGG_H(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,3))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,4))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,-4),Q(:,16),wf(:,5),Q(:,19))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,6))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,7),Q(:,11))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,8))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,9),Q(:,7))
  call vert_HG_G(wf(:,2),wf(:,-4),Q(:,16),wf(:,10),Q(:,19))
  call vert_HG_G(wf(:,2),wf(:,-3),Q(:,8),wf(:,11),Q(:,11))
  call vert_HG_G(wf(:,2),wf(:,-2),Q(:,4),wf(:,12),Q(:,7))
  call counter_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,13),Q(:,15))
  call counter_GGG_H(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,14))
  call vert_HG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,15),Q(:,5))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,16),Q(:,26))
  call vert_HG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,17),Q(:,9))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,18),Q(:,22))
  call vert_HG_G(wf(:,0),wf(:,-4),Q(:,16),wf(:,19),Q(:,17))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,20),Q(:,14))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,21))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,22))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,23))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,24),Q(:,13))
  call counter_HG_G_vert(wf(:,-1),wf(:,-4),Q(:,16),wf(:,25),Q(:,18))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,26),Q(:,21))
  call counter_HG_G_vert(wf(:,-1),wf(:,-3),Q(:,8),wf(:,27),Q(:,10))
  call vert_HGG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,28),Q(:,25))
  call counter_HG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,29),Q(:,6))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,30),Q(:,14))
  call counter_HG_G_vert(wf(:,0),wf(:,-4),Q(:,16),wf(:,31),Q(:,17))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,32),Q(:,22))
  call counter_HG_G_vert(wf(:,0),wf(:,-3),Q(:,8),wf(:,33),Q(:,9))
  call vert_HGG_G(wf(:,-1),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,34),Q(:,26))
  call counter_HG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,35),Q(:,5))
  call vert_HG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,36),Q(:,6))
  call counter_HGG_G_vert(wf(:,0),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,37),Q(:,25))
  call vert_HG_G(wf(:,-1),wf(:,-3),Q(:,8),wf(:,38),Q(:,10))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,39),Q(:,21))
  call vert_HG_G(wf(:,-1),wf(:,-4),Q(:,16),wf(:,40),Q(:,18))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,41),Q(:,13))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,-4),Q(:,16),wf(:,42),Q(:,19))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,43),Q(:,11))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,44),Q(:,7))
  call counter_HG_G(ctHEFTggh,wf(:,2),wf(:,-4),Q(:,16),wf(:,45),Q(:,19))
  call counter_HG_G(ctHEFTggh,wf(:,2),wf(:,-3),Q(:,8),wf(:,46),Q(:,11))
  call counter_HG_G(ctHEFTggh,wf(:,2),wf(:,-2),Q(:,4),wf(:,47),Q(:,7))
  call counter_HG_G_vert(wf(:,-1),wf(:,15),Q(:,5),wf(:,48),Q(:,7))
  call vert_UV_W(wf(:,15),Q(:,5),wf(:,-3),Q(:,8),wf(:,49))
  call vert_UV_W(wf(:,15),Q(:,5),wf(:,-4),Q(:,16),wf(:,50))
  call counter_HG_G_vert(wf(:,-1),wf(:,17),Q(:,9),wf(:,51),Q(:,11))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,17),Q(:,9),wf(:,52))
  call counter_HG_G_vert(wf(:,-1),wf(:,19),Q(:,17),wf(:,53),Q(:,19))
  call vert_HG_G(wf(:,0),wf(:,25),Q(:,18),wf(:,54),Q(:,19))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,19),Q(:,17),wf(:,55))
  call vert_HG_G(wf(:,0),wf(:,27),Q(:,10),wf(:,56),Q(:,11))
  call vert_UV_W(wf(:,17),Q(:,9),wf(:,-4),Q(:,16),wf(:,57))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,19),Q(:,17),wf(:,58))
  call vert_HG_G(wf(:,0),wf(:,29),Q(:,6),wf(:,59),Q(:,7))
  call counter_V_V(ctGG,wf(:,4),Q(:,12),wf(:,60))
  call counter_V_V(ctGG,wf(:,6),Q(:,20),wf(:,61))
  call counter_V_V(ctGG,wf(:,8),Q(:,24),wf(:,62))
  call counter_HG_G_vert(wf(:,0),wf(:,36),Q(:,6),wf(:,63),Q(:,7))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,31),Q(:,17),wf(:,64))
  call vert_UV_W(wf(:,33),Q(:,9),wf(:,-4),Q(:,16),wf(:,65))
  call counter_HG_G_vert(wf(:,0),wf(:,38),Q(:,10),wf(:,66),Q(:,11))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,31),Q(:,17),wf(:,67))
  call counter_HG_G_vert(wf(:,0),wf(:,40),Q(:,18),wf(:,68),Q(:,19))
  call vert_HG_G(wf(:,-1),wf(:,31),Q(:,17),wf(:,69),Q(:,19))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,33),Q(:,9),wf(:,70))
  call vert_HG_G(wf(:,-1),wf(:,33),Q(:,9),wf(:,71),Q(:,11))
  call vert_UV_W(wf(:,35),Q(:,5),wf(:,-4),Q(:,16),wf(:,72))
  call vert_UV_W(wf(:,35),Q(:,5),wf(:,-3),Q(:,8),wf(:,73))
  call vert_HG_G(wf(:,-1),wf(:,35),Q(:,5),wf(:,74),Q(:,7))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MH2)
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,20))
  den(4) = 1 / (Q(5,24))
  den(8) = 1 / (Q(5,5))
  den(9) = 1 / (Q(5,9))
  den(10) = 1 / (Q(5,17))
  den(11) = 1 / (Q(5,7))
  den(12) = 1 / (Q(5,11))
  den(13) = 1 / (Q(5,19))
  den(14) = 1 / (Q(5,13))
  den(15) = 1 / (Q(5,21))
  den(16) = 1 / (Q(5,25))
  den(17) = 1 / (Q(5,14))
  den(18) = 1 / (Q(5,22))
  den(19) = 1 / (Q(5,26))
  den(20) = 1 / (Q(5,6))
  den(21) = 1 / (Q(5,10))
  den(22) = 1 / (Q(5,18))

  ! denominators
  den(5) = den(1)*den(2)
  den(6) = den(1)*den(3)
  den(7) = den(1)*den(4)
  den(23) = den(4)*den(8)
  den(24) = den(8)*den(22)
  den(25) = den(8)*den(21)
  den(26) = den(3)*den(9)
  den(27) = den(9)*den(22)
  den(28) = den(2)*den(10)
  den(29) = den(2)*den(22)
  den(30) = den(10)*den(21)
  den(31) = den(3)*den(21)
  den(32) = den(9)*den(20)
  den(33) = den(10)*den(20)
  den(34) = den(4)*den(20)
  den(35) = den(2)*den(13)
  den(36) = den(3)*den(12)
  den(37) = den(4)*den(11)
  den(38) = den(1)*den(13)
  den(39) = den(2)*den(38)
  den(40) = den(1)*den(12)
  den(41) = den(3)*den(40)
  den(42) = den(1)*den(11)
  den(43) = den(4)*den(42)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(64)

  A(1) = cont_VV(wf(:,-4),wf(:,1))
  A(2) = cont_SS(wf(:,2),wf(:,3)) * den(1)
  A(3) = cont_VV(wf(:,4),wf(:,5)) * den(2)
  A(4) = cont_VV(wf(:,6),wf(:,7)) * den(3)
  A(5) = cont_VV(wf(:,8),wf(:,9)) * den(4)
  A(6) = cont_VV(wf(:,4),wf(:,10)) * den(5)
  A(7) = cont_VV(wf(:,6),wf(:,11)) * den(6)
  A(8) = cont_VV(wf(:,8),wf(:,12)) * den(7)

  A(9) = cont_VV(wf(:,-4),wf(:,13))
  A(10) = cont_SS(wf(:,2),wf(:,14)) * den(1)
  A(11) = cont_VV(wf(:,15),wf(:,16)) * den(8)
  A(12) = cont_VV(wf(:,17),wf(:,18)) * den(9)
  A(13) = cont_VV(wf(:,19),wf(:,20)) * den(10)
  A(14) = cont_VV(wf(:,9),wf(:,21)) * den(11)
  A(15) = cont_VV(wf(:,7),wf(:,22)) * den(12)
  A(16) = cont_VV(wf(:,5),wf(:,23)) * den(13)
  A(17) = cont_VV(wf(:,24),wf(:,25)) * den(14)
  A(18) = cont_VV(wf(:,26),wf(:,27)) * den(15)
  A(19) = cont_VV(wf(:,28),wf(:,29)) * den(16)
  A(20) = cont_VV(wf(:,30),wf(:,31)) * den(17)
  A(21) = cont_VV(wf(:,32),wf(:,33)) * den(18)
  A(22) = cont_VV(wf(:,34),wf(:,35)) * den(19)
  A(23) = cont_VV(wf(:,36),wf(:,37)) * den(20)
  A(24) = cont_VV(wf(:,38),wf(:,39)) * den(21)
  A(25) = cont_VV(wf(:,40),wf(:,41)) * den(22)
  A(26) = cont_VV(wf(:,4),wf(:,42)) * den(2)
  A(27) = cont_VV(wf(:,6),wf(:,43)) * den(3)
  A(28) = cont_VV(wf(:,8),wf(:,44)) * den(4)
  A(29) = cont_VV(wf(:,4),wf(:,45)) * den(5)
  A(30) = cont_VV(wf(:,6),wf(:,46)) * den(6)
  A(31) = cont_VV(wf(:,12),wf(:,21)) * den(7)
  A(32) = cont_VV(wf(:,8),wf(:,47)) * den(7)
  A(33) = cont_VV(wf(:,11),wf(:,22)) * den(6)
  A(34) = cont_VV(wf(:,10),wf(:,23)) * den(5)
  A(35) = cont_VV(wf(:,8),wf(:,48)) * den(23)
  A(36) = cont_VV(wf(:,25),wf(:,49)) * den(24)
  A(37) = cont_VV(wf(:,27),wf(:,50)) * den(25)
  A(38) = cont_VV(wf(:,6),wf(:,51)) * den(26)
  A(39) = cont_VV(wf(:,25),wf(:,52)) * den(27)
  A(40) = cont_VV(wf(:,4),wf(:,53)) * den(28)
  A(41) = cont_VV(wf(:,4),wf(:,54)) * den(29)
  A(42) = cont_VV(wf(:,27),wf(:,55)) * den(30)
  A(43) = cont_VV(wf(:,6),wf(:,56)) * den(31)
  A(44) = cont_VV(wf(:,29),wf(:,57)) * den(32)
  A(45) = cont_VV(wf(:,29),wf(:,58)) * den(33)
  A(46) = cont_VV(wf(:,8),wf(:,59)) * den(34)
  A(47) = cont_VV(wf(:,5),wf(:,60)) * den(35)
  A(48) = cont_VV(wf(:,7),wf(:,61)) * den(36)
  A(49) = cont_VV(wf(:,9),wf(:,62)) * den(37)
  A(50) = cont_VV(wf(:,8),wf(:,63)) * den(34)
  A(51) = cont_VV(wf(:,36),wf(:,64)) * den(33)
  A(52) = cont_VV(wf(:,36),wf(:,65)) * den(32)
  A(53) = cont_VV(wf(:,6),wf(:,66)) * den(31)
  A(54) = cont_VV(wf(:,38),wf(:,67)) * den(30)
  A(55) = cont_VV(wf(:,4),wf(:,68)) * den(29)
  A(56) = cont_VV(wf(:,4),wf(:,69)) * den(28)
  A(57) = cont_VV(wf(:,40),wf(:,70)) * den(27)
  A(58) = cont_VV(wf(:,6),wf(:,71)) * den(26)
  A(59) = cont_VV(wf(:,38),wf(:,72)) * den(25)
  A(60) = cont_VV(wf(:,40),wf(:,73)) * den(24)
  A(61) = cont_VV(wf(:,8),wf(:,74)) * den(23)
  A(62) = cont_VV(wf(:,10),wf(:,60)) * den(39)
  A(63) = cont_VV(wf(:,11),wf(:,61)) * den(41)
  A(64) = cont_VV(wf(:,12),wf(:,62)) * den(43)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(64)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = 2*CI*(A(1)+A(3)-A(4)+A(5))*f(2)+2*CI*(-A(2)-A(6)+A(7)-A(8))*f(6)
  M1(2) = 2*CI*(-A(1)-A(3)+A(4)-A(5))*f(2)+2*CI*(A(2)+A(6)-A(7)+A(8))*f(6)

  M2(1) = 2*CI*(A(11)-A(12)+A(13)+A(17)-A(18)+A(19)+A(20)-A(21)+A(22)+A(23)-A(24)+A(25)+A(35)+A(36)-A(37)-A(38)+A(39)+A(40)+A(41) &
       -A(42)-A(43)+A(44)+A(45)+A(46)+A(50)+A(51)+A(52)-A(53)-A(54)+A(55)+A(56)+A(57)-A(58)-A(59)+A(60)+A(61))*f(1)+2*CI*(A(26) &
       -A(27)+A(28)-A(47)+A(48)-A(49))*f(3)+2*CI*A(9)*f(4)+2*CI*(A(14)-A(15)+A(16))*f(5)+2*CI*(-A(29)+A(30)-A(32)+A(62)-A(63) &
       +A(64))*f(7)-2*CI*A(10)*f(8)+2*CI*(-A(31)+A(33)-A(34))*f(9)
  M2(2) = 2*CI*(-A(11)+A(12)-A(13)-A(17)+A(18)-A(19)-A(20)+A(21)-A(22)-A(23)+A(24)-A(25)-A(35)-A(36)+A(37)+A(38)-A(39)-A(40)-A(41) &
       +A(42)+A(43)-A(44)-A(45)-A(46)-A(50)-A(51)-A(52)+A(53)+A(54)-A(55)-A(56)-A(57)+A(58)+A(59)-A(60)-A(61))*f(1)+2*CI*(-A(26) &
       +A(27)-A(28)+A(47)-A(48)+A(49))*f(3)-2*CI*A(9)*f(4)+2*CI*(-A(14)+A(15)-A(16))*f(5)+2*CI*(A(29)-A(30)+A(32)-A(62)+A(63) &
       -A(64))*f(7)+2*CI*A(10)*f(8)+2*CI*(A(31)-A(33)+A(34))*f(9)

end subroutine colourvectors

end module ol_loop_heftpphhj_hhggg_1_/**/REALKIND
