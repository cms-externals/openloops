
module ol_colourmatrix_ppvvv_uuxaazg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1)
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
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [ -18]
  K1(18,:) = [ -18]
  K1(19,:) = [   0]
  K1(20,:) = [   0]
  K1(21,:) = [   0]
  K1(22,:) = [  36]
  K1(23,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvv_uuxaazg_1_/**/REALKIND



module ol_forced_parameters_ppvvv_uuxaazg_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvv_uuxaazg_1_/**/REALKIND

module ol_tree_ppvvv_uuxaazg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(1)
  complex(REALKIND), save :: den(58)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 96 ! number of helicity configurations
  integer(intkind2), save :: nhel = 96 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(96) ! physical helicity states
  complex(DREALKIND) :: M1helarr(1,96) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (4*CI*eQED**3*gQCD)/9._/**/REALKIND

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,21))
  den(6) = 1 / (Q(5,26))
  den(9) = 1 / (Q(5,18))
  den(10) = 1 / (Q(5,13))
  den(15) = 1 / (Q(5,34))
  den(17) = 1 / (Q(5,42))
  den(20) = 1 / (Q(5,9))
  den(21) = 1 / (Q(5,6))
  den(22) = 1 / (Q(5,25))
  den(25) = 1 / (Q(5,22))
  den(28) = 1 / (Q(5,17))
  den(31) = 1 / (Q(5,14))
  den(34) = 1 / (Q(5,33))
  den(35) = 1 / (Q(5,41))
  den(44) = 1 / (Q(5,38))
  den(51) = 1 / (Q(5,37))

  ! denominators

  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(1)*den(10)
  den(12) = den(9)*den(11)
  den(13) = den(6)*den(9)
  den(14) = den(1)*den(13)
  den(16) = den(11)*den(15)
  den(18) = den(15)*den(17)
  den(19) = den(1)*den(18)
  den(23) = den(20)*den(22)
  den(24) = den(21)*den(23)
  den(26) = den(21)*den(25)
  den(27) = den(20)*den(26)
  den(29) = den(22)*den(28)
  den(30) = den(21)*den(29)
  den(32) = den(21)*den(31)
  den(33) = den(28)*den(32)
  den(36) = den(34)*den(35)
  den(37) = den(21)*den(36)
  den(38) = den(32)*den(34)
  den(39) = den(10)*den(20)
  den(40) = den(9)*den(39)
  den(41) = den(9)*den(25)
  den(42) = den(20)*den(41)
  den(43) = den(15)*den(39)
  den(45) = den(15)*den(44)
  den(46) = den(20)*den(45)
  den(47) = den(3)*den(28)
  den(48) = den(2)*den(47)
  den(49) = den(2)*den(31)
  den(50) = den(28)*den(49)
  den(52) = den(34)*den(51)
  den(53) = den(2)*den(52)
  den(54) = den(34)*den(49)
  den(55) = den(15)*den(47)
  den(56) = den(28)*den(45)
  den(57) = den(9)*den(52)
  den(58) = den(34)*den(41)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppvvv_uuxaazg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppvvv_uuxaazg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for up anti-up gamma gamma Z glue -> 0
! I   = emitter, 0 means none (replace wave function I in the current crossing by the momentum MOM),
!       for I < 0 emitter for PowHeg's B^mu,nu
! MOM = external "polarisation vector" for gluon emitter
! nextcombs is the length of the array extcombs
! The elements of the array extcombs specify for which external particle combinations
!   the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
!   i=j=0 -> 0 means no colour insertion.
! M2munu = Spin correlated born squared amplitude in PowHeg format B^mu,nu for emitter -I
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND !, only: ci, parameters_status, ZERO, scalefactor, >masses<
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_init
  use ol_kinematics_/**/REALKIND, only: conv_mom_scatt2in, internal_momenta
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_data_types_/**/REALKIND
  use ol_helicity_bookkeeping_/**/REALKIND, only: &
    & helbookkeeping_wf, helsync, flip_phase
  use ol_helicity_init, only: helbookkeeping_flip, helsync_flip
  use ol_h_propagators_/**/REALKIND
  use ol_h_wavefunctions_/**/REALKIND
  use ol_h_vertices_/**/REALKIND
  use ol_h_contractions_/**/REALKIND
  use ol_external_ppvvv_uuxaazg_1, only: external_perm_ppvvv_uuxaazg_1, &
    & external_perm_inv_ppvvv_uuxaazg_1, extcomb_perm_ppvvv_uuxaazg_1, &
    & average_factor_ppvvv_uuxaazg_1
  use ol_external_ppvvv_uuxaazg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppvvv_uuxaazg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppvvv_uuxaazg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppvvv_uuxaazg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, l, m, n
  real(REALKIND)    :: P(0:3,6)
  real(REALKIND)    :: extmasses2(6)
  real(REALKIND)    :: M2add(0:23-1), M2munuadd
  complex(REALKIND) :: MOM_LC(4), M1(1), M1helarray(1,96)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(3), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,12), wf6(6,4), wf8(8,20), wf12(12,20), wf96(96,24)

  type(polcont) :: A(96,24)
  complex(REALKIND) :: Aj(24)

  complex(REALKIND) :: omega(2) ! phases for helicity correlations

  call ensure_mp_init()

  if (hel_not_initialised) call hel_init()
  if (colmat_not_initialised) call colourmatrix_init()
  if (factors_status /= parameters_status) then
    ! Note: if factors_init would only be called when parameters changed which are relevant for the factors,
    ! a different 'status' would have to be used, because check_forced_parameters should be called after every parameter change.
    call check_forced_parameters()
    call factors_init()
  end if

  if (momenta_nan_check(P_scatt) /= 0) then
    M2 = 0
    return
  end if

  ! Convert 2 -> n-2 PS-point to n -> 0 (so that P(1) + ... + P(n) = 0)
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rMZ2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppvvv_uuxaazg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppvvv_uuxaazg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppvvv_uuxaazg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppvvv_uuxaazg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_V(P(:,3), rZERO, H3, ex3)
  call wf_V(P(:,4), rZERO, H4, ex4)
  call wf_V(P(:,5), rMZ, H5, ex5)
  call wf_V(P(:,6), rZERO, H6, ex6)


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...
    call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H1, ex1, shift)
    call helbookkeeping_flip(H2, 2, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H2, ex2, shift)
    call helbookkeeping_flip(H3, 3, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H3, ex3, shift)
    call helbookkeeping_flip(H4, 4, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H4, ex4, shift)
    call helbookkeeping_flip(H5, 5, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H5, ex5, shift)
    call helbookkeeping_flip(H6, 6, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H6, ex6, shift)

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_VQ_A(ntry, ex3, ex1, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_AV_Q(ntry, ex2, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call prop_Q_A(ntry, wf4(:,1), Q(:,5), ZERO, 0_intkind1, wf4(:,3), n2(1))
  call prop_A_Q(ntry, wf4(:,2), Q(:,10), ZERO, 0_intkind1, wf4(:,4), n2(2))
  call vert_ZQ_A(gZu,ntry, ex5, wf4(:,3), wf12(:,1), n3(:,3), t3x12(:,:,1))
  call vert_AV_Q(ntry, wf4(:,4), ex6, wf8(:,1), n3(:,4), t3x8(:,:,1))
  call prop_Q_A(ntry, wf12(:,1), Q(:,21), ZERO, 0_intkind1, wf12(:,2), n2(3))
  call vert_AZ_Q(gZu,ntry, wf4(:,4), ex5, wf12(:,3), n3(:,5), t3x12(:,:,2))
  call vert_VQ_A(ntry, ex6, wf4(:,3), wf8(:,2), n3(:,6), t3x8(:,:,2))
  call prop_A_Q(ntry, wf12(:,3), Q(:,26), ZERO, 0_intkind1, wf12(:,4), n2(4))
  call vert_AZ_Q(gZu,ntry, ex2, ex5, wf6(:,1), n3(:,7), t3x6(:,:,1))
  call prop_A_Q(ntry, wf6(:,1), Q(:,18), ZERO, 0_intkind1, wf6(:,2), n2(5))
  call vert_VQ_A(ntry, ex4, wf4(:,3), wf8(:,3), n3(:,8), t3x8(:,:,3))
  call vert_AV_Q(ntry, wf6(:,2), ex6, wf12(:,5), n3(:,9), t3x12(:,:,3))
  call prop_Q_A(ntry, wf8(:,3), Q(:,13), ZERO, 0_intkind1, wf8(:,4), n2(6))
  call vert_AV_Q(ntry, wf6(:,2), ex4, wf12(:,6), n3(:,10), t3x12(:,:,4))
  call prop_A_Q(ntry, wf12(:,6), Q(:,26), ZERO, 0_intkind1, wf12(:,7), n2(7))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,5), n3(:,11), t3x4(:,:,3))
  call prop_A_Q(ntry, wf4(:,5), Q(:,34), ZERO, 0_intkind1, wf4(:,6), n2(8))
  call vert_AZ_Q(gZu,ntry, wf4(:,6), ex5, wf12(:,8), n3(:,12), t3x12(:,:,5))
  call vert_AV_Q(ntry, wf4(:,6), ex4, wf8(:,5), n3(:,13), t3x8(:,:,4))
  call prop_A_Q(ntry, wf8(:,5), Q(:,42), ZERO, 0_intkind1, wf8(:,6), n2(9))
  call vert_VQ_A(ntry, ex4, ex1, wf4(:,7), n3(:,14), t3x4(:,:,4))
  call vert_AV_Q(ntry, ex2, ex3, wf4(:,8), n3(:,15), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,7), Q(:,9), ZERO, 0_intkind1, wf4(:,9), n2(10))
  call prop_A_Q(ntry, wf4(:,8), Q(:,6), ZERO, 0_intkind1, wf4(:,10), n2(11))
  call vert_ZQ_A(gZu,ntry, ex5, wf4(:,9), wf12(:,9), n3(:,16), t3x12(:,:,6))
  call vert_AV_Q(ntry, wf4(:,10), ex6, wf8(:,7), n3(:,17), t3x8(:,:,5))
  call prop_Q_A(ntry, wf12(:,9), Q(:,25), ZERO, 0_intkind1, wf12(:,10), n2(12))
  call vert_AZ_Q(gZu,ntry, wf4(:,10), ex5, wf12(:,11), n3(:,18), t3x12(:,:,7))
  call vert_VQ_A(ntry, ex6, wf4(:,9), wf8(:,8), n3(:,19), t3x8(:,:,6))
  call prop_A_Q(ntry, wf12(:,11), Q(:,22), ZERO, 0_intkind1, wf12(:,12), n2(13))
  call vert_ZQ_A(gZu,ntry, ex5, ex1, wf6(:,3), n3(:,20), t3x6(:,:,2))
  call prop_Q_A(ntry, wf6(:,3), Q(:,17), ZERO, 0_intkind1, wf6(:,4), n2(14))
  call vert_VQ_A(ntry, ex4, wf6(:,4), wf12(:,13), n3(:,21), t3x12(:,:,8))
  call prop_Q_A(ntry, wf12(:,13), Q(:,25), ZERO, 0_intkind1, wf12(:,14), n2(15))
  call vert_AV_Q(ntry, wf4(:,10), ex4, wf8(:,9), n3(:,22), t3x8(:,:,7))
  call vert_VQ_A(ntry, ex6, wf6(:,4), wf12(:,15), n3(:,23), t3x12(:,:,9))
  call prop_A_Q(ntry, wf8(:,9), Q(:,14), ZERO, 0_intkind1, wf8(:,10), n2(16))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,11), n3(:,24), t3x4(:,:,6))
  call prop_Q_A(ntry, wf4(:,11), Q(:,33), ZERO, 0_intkind1, wf4(:,12), n2(17))
  call vert_VQ_A(ntry, ex4, wf4(:,12), wf8(:,11), n3(:,25), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,11), Q(:,41), ZERO, 0_intkind1, wf8(:,12), n2(18))
  call vert_ZQ_A(gZu,ntry, ex5, wf4(:,12), wf12(:,16), n3(:,26), t3x12(:,:,10))
  call vert_VQ_A(ntry, ex3, wf4(:,9), wf8(:,13), n3(:,27), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,13), Q(:,13), ZERO, 0_intkind1, wf8(:,14), n2(19))
  call vert_AV_Q(ntry, wf6(:,2), ex3, wf12(:,17), n3(:,28), t3x12(:,:,11))
  call prop_A_Q(ntry, wf12(:,17), Q(:,22), ZERO, 0_intkind1, wf12(:,18), n2(20))
  call vert_AV_Q(ntry, wf4(:,6), ex3, wf8(:,15), n3(:,29), t3x8(:,:,10))
  call prop_A_Q(ntry, wf8(:,15), Q(:,38), ZERO, 0_intkind1, wf8(:,16), n2(21))
  call vert_VQ_A(ntry, ex3, wf6(:,4), wf12(:,19), n3(:,30), t3x12(:,:,12))
  call prop_Q_A(ntry, wf12(:,19), Q(:,21), ZERO, 0_intkind1, wf12(:,20), n2(22))
  call vert_AV_Q(ntry, wf4(:,4), ex3, wf8(:,17), n3(:,31), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,17), Q(:,14), ZERO, 0_intkind1, wf8(:,18), n2(23))
  call vert_VQ_A(ntry, ex3, wf4(:,12), wf8(:,19), n3(:,32), t3x8(:,:,12))
  call prop_Q_A(ntry, wf8(:,19), Q(:,37), ZERO, 0_intkind1, wf8(:,20), n2(24))


  ! colour-stripped amplitudes
  do nsync = ntry+ntry-1, ntry+1  !  nsync = 1,2  for 1st point and nsync = 3 later
    call diagrams()
    if (nsync == 1) then
      call helsync(nsync, A, nhel, Hel)
      call helsync_flip(nsync, nhel, Hel, eflip, exthel)
    end if
  end do

  do k = 1, nhel
    call colourvector(A, k, M1helarray(:,k))
  end do
  M1helarray(:,nhel+1:) = 0
  M1helarr = M1helarray ! fill cache

  M2 = 0
  if (ReplacePol == 0) then ! no helicity correlation

    do k = 1, nhel
      call colint(M1helarray(:,k), M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do

  else ! helicity correlation

    call flip_phase(P(:,ReplacePol), firstpol(ReplacePol), MOM, omega)
    do k = 1, nhel
      M1 = M1helarray(:,k)
      r = eflip(k, ReplacePol) ! Flip helicity of external particle ReplacePol (gluon emitter).
      if (r <= nhel) then      ! Only add flipped helicity configuration if it doesn't vanish.
        M1 = M1 + omega(exthel(k,ReplacePol)) * M1helarray(:,r)
      end if
      call colint(M1, M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do
    M2 = 0.25_/**/REALKIND * M2

  end if

  if ( JBmunu /= 0 ) then ! Bmunu helicity correlation
    M2munu = 0
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppvvv_uuxaazg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppvvv_uuxaazg_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*6-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_QA(nsync, wf8(:,1), wf12(:,2), A(:,1), n3(:,33), t3x96(:,:,1), nhel, den(5))
    call cont_QA(nsync, wf8(:,2), wf12(:,4), A(:,2), n3(:,34), t3x96(:,:,2), nhel, den(8))
    call cont_QA(nsync, wf12(:,5), wf8(:,4), A(:,3), n3(:,35), t3x96(:,:,3), nhel, den(12))
    call cont_QA(nsync, wf8(:,2), wf12(:,7), A(:,4), n3(:,36), t3x96(:,:,4), nhel, den(14))
    call cont_QA(nsync, wf8(:,4), wf12(:,8), A(:,5), n3(:,37), t3x96(:,:,5), nhel, den(16))
    call cont_QA(nsync, wf12(:,1), wf8(:,6), A(:,6), n3(:,38), t3x96(:,:,6), nhel, den(19))
    call cont_QA(nsync, wf8(:,7), wf12(:,10), A(:,7), n3(:,39), t3x96(:,:,7), nhel, den(24))
    call cont_QA(nsync, wf8(:,8), wf12(:,12), A(:,8), n3(:,40), t3x96(:,:,8), nhel, den(27))
    call cont_QA(nsync, wf8(:,7), wf12(:,14), A(:,9), n3(:,41), t3x96(:,:,9), nhel, den(30))
    call cont_QA(nsync, wf12(:,15), wf8(:,10), A(:,10), n3(:,42), t3x96(:,:,10), nhel, den(33))
    call cont_QA(nsync, wf12(:,11), wf8(:,12), A(:,11), n3(:,43), t3x96(:,:,11), nhel, den(37))
    call cont_QA(nsync, wf8(:,10), wf12(:,16), A(:,12), n3(:,44), t3x96(:,:,12), nhel, den(38))
    call cont_QA(nsync, wf12(:,5), wf8(:,14), A(:,13), n3(:,45), t3x96(:,:,13), nhel, den(40))
    call cont_QA(nsync, wf8(:,8), wf12(:,18), A(:,14), n3(:,46), t3x96(:,:,14), nhel, den(42))
    call cont_QA(nsync, wf12(:,8), wf8(:,14), A(:,15), n3(:,47), t3x96(:,:,15), nhel, den(43))
    call cont_QA(nsync, wf12(:,9), wf8(:,16), A(:,16), n3(:,48), t3x96(:,:,16), nhel, den(46))
    call cont_QA(nsync, wf8(:,1), wf12(:,20), A(:,17), n3(:,49), t3x96(:,:,17), nhel, den(48))
    call cont_QA(nsync, wf12(:,15), wf8(:,18), A(:,18), n3(:,50), t3x96(:,:,18), nhel, den(50))
    call cont_QA(nsync, wf12(:,3), wf8(:,20), A(:,19), n3(:,51), t3x96(:,:,19), nhel, den(53))
    call cont_QA(nsync, wf12(:,16), wf8(:,18), A(:,20), n3(:,52), t3x96(:,:,20), nhel, den(54))
    call cont_QA(nsync, wf8(:,5), wf12(:,20), A(:,21), n3(:,53), t3x96(:,:,21), nhel, den(55))
    call cont_QA(nsync, wf12(:,13), wf8(:,16), A(:,22), n3(:,54), t3x96(:,:,22), nhel, den(56))
    call cont_QA(nsync, wf12(:,6), wf8(:,20), A(:,23), n3(:,55), t3x96(:,:,23), nhel, den(57))
    call cont_QA(nsync, wf8(:,11), wf12(:,18), A(:,24), n3(:,56), t3x96(:,:,24), nhel, den(58))

end subroutine diagrams


elemental function diagmap(j, n)
  implicit none
  integer, intent(in) :: j, n
  complex(REALKIND) :: diagmap
  diagmap = A(j,n)%j
end function diagmap

function diagsum(j, pos, neg)
  implicit none
  integer, intent(in) :: j, pos(:), neg(:)
  complex(REALKIND) :: diagsum
  diagsum = sum(diagmap(j, pos)) - sum(diagmap(j, neg))
end function diagsum

subroutine colourvector(A, j, M1)
  implicit none
  type(polcont) :: A(:,:)
  integer, intent(in) :: j
  complex(REALKIND), intent(out) :: M1(1) ! M1helarray(1,96)
  integer :: empty(0)

  M1(1) = (-A(j,1)%j-A(j,2)%j-A(j,3)%j-A(j,4)%j-A(j,5)%j-A(j,6)%j-A(j,7)%j-A(j,8)%j-A(j,9)%j-A(j,10)%j-A(j,11)%j-A(j,12)%j &
       -A(j,13)%j-A(j,14)%j-A(j,15)%j-A(j,16)%j-A(j,17)%j-A(j,18)%j-A(j,19)%j-A(j,20)%j-A(j,21)%j-A(j,22)%j-A(j,23)%j &
       -A(j,24)%j)*f(1)

end subroutine colourvector


! **********************************************************************
subroutine colint(M, M2colint, extcombs)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! The elements of the array extcombs specifies for which external particle
! combinations the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
! i=j=0 -> 0 means no colour insertion.
! **********************************************************************
  use ol_colourmatrix_ppvvv_uuxaazg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(1)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 1*extcomb
    do i = 1, 1
      do j = 1, 1
        M2colint(extcomb) = M2colint(extcomb) + real(conjg(M(i))*K1(i+colmatpos,j)*M(j))
      end do
    end do
  end do

end subroutine colint


! **********************************************************************
subroutine colintmunu(M1, M2, M2colint)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! **********************************************************************
  use ol_colourmatrix_ppvvv_uuxaazg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(1)
  complex(REALKIND), intent(in)  :: M2(1)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 1
    do j = 1, 1
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppvvv_uuxaazg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(1,96)
  integer, intent(out) :: nhelout
  M1out = M1helarr
  nhelout = nhel
end subroutine colourvector
#endif


! =================================================== !
! Only interfaces for easier usage of AMP2_<procname> !
! =================================================== !

#ifdef PRECISION_dp
subroutine amp2tree(P, M2) &
    & bind(c,name="ol_f_amp2tree_ppvvv_uuxaazg_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_ppvvv_uuxaazg_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  if (J <= I) then
    extcomb = I*(I-1)/2 + J
  else
    extcomb = J*(J-1)/2 + I
  end if
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ extcomb ], M2munu)
  M2 = M2tmp(extcomb)
end subroutine amp2ccone


#ifdef PRECISION_dp
subroutine amp2ccall(P, M2) &
    & bind(c,name="ol_f_amp2ccall_ppvvv_uuxaazg_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    23, [ (k, k = 0, 23-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_ppvvv_uuxaazg_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  if (J <= I) then
    extcomb = I*(I-1)/2 + J
  else
    extcomb = J*(J-1)/2 + I
  end if
  call amp2(P, M2tmp, I, MOM, 1, [ extcomb ], M2munu)
  M2 = M2tmp(extcomb)
end subroutine amp2hcone


#ifdef PRECISION_dp
subroutine amp2hcall(P, M2, I, MOM) &
    & bind(c,name="ol_f_amp2hcall_ppvvv_uuxaazg_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(6)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(6)
  do J = 1, 6
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 6,extcombs, M2munu)
  do J = 1, 6
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_ppvvv_uuxaazg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_ppvvv_uuxaazg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_ppvvv_uuxaazg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_ppvvv_uuxaazg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_j = j
  f_mom = mom
  call amp2hcone(f_p, f_m2, f_i, f_j, f_mom)
  m2 = f_m2
end subroutine amp2hcone_c

subroutine amp2hcall_c(p, m2, i, mom) &
    & bind(c,name="ol_amp2hcall_ppvvv_uuxaazg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_c



! Only for compatibility with the old interface
subroutine amp2tree_legacy(p, m2) &
    & bind(c,name="amp2tree_ppvvv_uuxaazg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_ppvvv_uuxaazg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_ppvvv_uuxaazg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_ppvvv_uuxaazg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_j = j
  f_mom = mom
  call amp2hcone(f_p, f_m2, f_i, f_j, f_mom)
  m2 = f_m2
end subroutine amp2hcone_legacy

subroutine amp2hcall_legacy(p, m2, i, mom) &
    & bind(c,name="amp2hcall_ppvvv_uuxaazg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_ppvvv_uuxaazg_1_/**/REALKIND
