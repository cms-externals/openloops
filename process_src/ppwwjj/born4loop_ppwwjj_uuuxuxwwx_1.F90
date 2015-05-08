
module ol_tree_ppwwjj_uuuxuxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(3)
  complex(REALKIND), save :: den(60)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 144 ! number of helicity configurations
  integer(intkind2), save :: nhel = 144 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(144) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,144) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (2*CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f(2) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(3) = (CI*cw*eQED**2*gQCD**2)/sw

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,18))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,13))
  den(9) = 1 / (Q(5,7))
  den(12) = 1 / (Q(5,48))
  den(14) = 1 / (Q(5,48) - MZ2)
  den(16) = 1 / (Q(5,50))
  den(21) = 1 / (Q(5,17))
  den(22) = 1 / (Q(5,6))
  den(25) = 1 / (Q(5,14))
  den(32) = 1 / (Q(5,49))
  den(37) = 1 / (Q(5,9))
  den(38) = 1 / (Q(5,36))
  den(43) = 1 / (Q(5,11))
  den(50) = 1 / (Q(5,10))

  ! denominators

  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(3)*den(10)
  den(13) = den(10)*den(12)
  den(15) = den(10)*den(14)
  den(17) = den(12)*den(16)
  den(18) = den(1)*den(17)
  den(19) = den(14)*den(16)
  den(20) = den(1)*den(19)
  den(23) = den(21)*den(22)
  den(24) = den(3)*den(23)
  den(26) = den(22)*den(25)
  den(27) = den(21)*den(26)
  den(28) = den(9)*den(22)
  den(29) = den(3)*den(28)
  den(30) = den(12)*den(28)
  den(31) = den(14)*den(28)
  den(33) = den(12)*den(32)
  den(34) = den(22)*den(33)
  den(35) = den(14)*den(32)
  den(36) = den(22)*den(35)
  den(39) = den(2)*den(37)
  den(40) = den(38)*den(39)
  den(41) = den(6)*den(37)
  den(42) = den(2)*den(41)
  den(44) = den(37)*den(43)
  den(45) = den(38)*den(44)
  den(46) = den(12)*den(44)
  den(47) = den(14)*den(44)
  den(48) = den(17)*den(37)
  den(49) = den(19)*den(37)
  den(51) = den(21)*den(50)
  den(52) = den(38)*den(51)
  den(53) = den(25)*den(50)
  den(54) = den(21)*den(53)
  den(55) = den(43)*den(50)
  den(56) = den(38)*den(55)
  den(57) = den(12)*den(55)
  den(58) = den(14)*den(55)
  den(59) = den(33)*den(50)
  den(60) = den(35)*den(50)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppwwjj_uuuxuxwwx_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppwwjj_uuuxuxwwx_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for up up anti-up anti-up W- W+ -> 0
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
  use ol_external_ppwwjj_uuuxuxwwx_1, only: external_perm_ppwwjj_uuuxuxwwx_1, &
    & external_perm_inv_ppwwjj_uuuxuxwwx_1, extcomb_perm_ppwwjj_uuuxuxwwx_1, &
    & average_factor_ppwwjj_uuuxuxwwx_1
  use ol_external_ppwwjj_uuuxuxwwx_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppwwjj_uuuxuxwwx_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppwwjj_uuuxuxwwx_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppwwjj_uuuxuxwwx_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,144)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(3), ex6(3)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,4), wf6(6,8), wf8(8,16), wf9(9,2), wf18(18,16), wf24(24,4), wf144(144,28)

  type(polcont) :: A(144,28)
  complex(REALKIND) :: Aj(28)

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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rMW2, rMW2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppwwjj_uuuxuxwwx_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppwwjj_uuuxuxwwx_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppwwjj_uuuxuxwwx_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppwwjj_uuuxuxwwx_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_Q(P(:,2), rZERO, H2, ex2)
  call wf_A(P(:,3), rZERO, H3, ex3)
  call wf_A(P(:,4), rZERO, H4, ex4)
  call wf_V(P(:,5), rMW, H5, ex5)
  call wf_V(P(:,6), rMW, H6, ex6)


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
  call vert_QA_V(ntry, ex1, ex3, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_WQ_A(ntry, ex5, ex2, wf6(:,1), n3(:,2), t3x6(:,:,1))
  call vert_AW_Q(ntry, ex4, ex6, wf6(:,2), n3(:,3), t3x6(:,:,2))
  call prop_Q_A(ntry, wf6(:,1), Q(:,18), ZERO, 0_intkind1, wf6(:,3), n2(1))
  call prop_A_Q(ntry, wf6(:,2), Q(:,40), ZERO, 0_intkind1, wf6(:,4), n2(2))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,3), wf24(:,1), n3(:,4), t3x24(:,:,1))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,1), n3(:,5), t3x8(:,:,1))
  call vert_WQ_A(ntry, ex6, wf6(:,3), wf18(:,1), n3(:,6), t3x18(:,:,1))
  call prop_A_Q(ntry, wf8(:,1), Q(:,13), ZERO, 0_intkind1, wf8(:,2), n2(3))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,3), n3(:,7), t3x8(:,:,2))
  call vert_AW_Q(ntry, wf6(:,4), ex5, wf18(:,2), n3(:,8), t3x18(:,:,2))
  call prop_Q_A(ntry, wf8(:,3), Q(:,7), ZERO, 0_intkind1, wf8(:,4), n2(4))
  call vert_UV_W(ntry, ex6, Q(:,32), ex5, Q(:,16), wf9(:,1), n3(:,9), t3x9(:,:,1))
  call vert_AV_Q(ntry, ex4, wf9(:,1), wf18(:,3), n3(:,10), t3x18(:,:,3))
  call prop_W_W(ntry, wf9(:,1), Q(:,48), MZ, 1_intkind1, wf9(:,2), n2(5))
  call vert_AZ_Q(gZu,ntry, ex4, wf9(:,2), wf18(:,4), n3(:,11), t3x18(:,:,4))
  call vert_VQ_A(ntry, wf9(:,1), ex2, wf18(:,5), n3(:,12), t3x18(:,:,5))
  call prop_Q_A(ntry, wf18(:,5), Q(:,50), ZERO, 0_intkind1, wf18(:,6), n2(6))
  call vert_ZQ_A(gZu,ntry, wf9(:,2), ex2, wf18(:,7), n3(:,13), t3x18(:,:,6))
  call prop_Q_A(ntry, wf18(:,7), Q(:,50), ZERO, 0_intkind1, wf18(:,8), n2(7))
  call vert_WQ_A(ntry, ex5, ex1, wf6(:,5), n3(:,14), t3x6(:,:,3))
  call vert_QA_V(ntry, ex2, ex3, wf4(:,2), n3(:,15), t3x4(:,:,2))
  call prop_Q_A(ntry, wf6(:,5), Q(:,17), ZERO, 0_intkind1, wf6(:,6), n2(8))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,6), wf24(:,2), n3(:,16), t3x24(:,:,2))
  call vert_AV_Q(ntry, ex4, wf4(:,2), wf8(:,5), n3(:,17), t3x8(:,:,3))
  call vert_WQ_A(ntry, ex6, wf6(:,6), wf18(:,9), n3(:,18), t3x18(:,:,7))
  call prop_A_Q(ntry, wf8(:,5), Q(:,14), ZERO, 0_intkind1, wf8(:,6), n2(9))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,7), n3(:,19), t3x8(:,:,4))
  call prop_Q_A(ntry, wf8(:,7), Q(:,7), ZERO, 0_intkind1, wf8(:,8), n2(10))
  call vert_VQ_A(ntry, wf9(:,1), ex1, wf18(:,10), n3(:,20), t3x18(:,:,8))
  call prop_Q_A(ntry, wf18(:,10), Q(:,49), ZERO, 0_intkind1, wf18(:,11), n2(11))
  call vert_ZQ_A(gZu,ntry, wf9(:,2), ex1, wf18(:,12), n3(:,21), t3x18(:,:,9))
  call prop_Q_A(ntry, wf18(:,12), Q(:,49), ZERO, 0_intkind1, wf18(:,13), n2(12))
  call vert_QA_V(ntry, ex1, ex4, wf4(:,3), n3(:,22), t3x4(:,:,3))
  call vert_AW_Q(ntry, ex3, ex6, wf6(:,7), n3(:,23), t3x6(:,:,4))
  call prop_A_Q(ntry, wf6(:,7), Q(:,36), ZERO, 0_intkind1, wf6(:,8), n2(13))
  call vert_VQ_A(ntry, wf4(:,3), wf6(:,3), wf24(:,3), n3(:,24), t3x24(:,:,3))
  call vert_AV_Q(ntry, ex3, wf4(:,3), wf8(:,9), n3(:,25), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,9), Q(:,13), ZERO, 0_intkind1, wf8(:,10), n2(14))
  call vert_VQ_A(ntry, wf4(:,3), ex2, wf8(:,11), n3(:,26), t3x8(:,:,6))
  call vert_AW_Q(ntry, wf6(:,8), ex5, wf18(:,14), n3(:,27), t3x18(:,:,10))
  call prop_Q_A(ntry, wf8(:,11), Q(:,11), ZERO, 0_intkind1, wf8(:,12), n2(15))
  call vert_AV_Q(ntry, ex3, wf9(:,1), wf18(:,15), n3(:,28), t3x18(:,:,11))
  call vert_AZ_Q(gZu,ntry, ex3, wf9(:,2), wf18(:,16), n3(:,29), t3x18(:,:,12))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,4), n3(:,30), t3x4(:,:,4))
  call vert_VQ_A(ntry, wf4(:,4), wf6(:,6), wf24(:,4), n3(:,31), t3x24(:,:,4))
  call vert_AV_Q(ntry, ex3, wf4(:,4), wf8(:,13), n3(:,32), t3x8(:,:,7))
  call prop_A_Q(ntry, wf8(:,13), Q(:,14), ZERO, 0_intkind1, wf8(:,14), n2(16))
  call vert_VQ_A(ntry, wf4(:,4), ex1, wf8(:,15), n3(:,33), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,15), Q(:,11), ZERO, 0_intkind1, wf8(:,16), n2(17))


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
  M2add = M2 / average_factor_ppwwjj_uuuxuxwwx_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppwwjj_uuuxuxwwx_1(k))
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

    call cont_QA(nsync, wf6(:,4), wf24(:,1), A(:,1), n3(:,34), t3x144(:,:,1), nhel, den(5))
    call cont_QA(nsync, wf18(:,1), wf8(:,2), A(:,2), n3(:,35), t3x144(:,:,2), nhel, den(8))
    call cont_QA(nsync, wf18(:,2), wf8(:,4), A(:,3), n3(:,36), t3x144(:,:,3), nhel, den(11))
    call cont_QA(nsync, wf8(:,4), wf18(:,3), A(:,4), n3(:,37), t3x144(:,:,4), nhel, den(13))
    call cont_QA(nsync, wf8(:,4), wf18(:,4), A(:,5), n3(:,38), t3x144(:,:,5), nhel, den(15))
    call cont_QA(nsync, wf8(:,1), wf18(:,6), A(:,6), n3(:,39), t3x144(:,:,6), nhel, den(18))
    call cont_QA(nsync, wf8(:,1), wf18(:,8), A(:,7), n3(:,40), t3x144(:,:,7), nhel, den(20))
    call cont_QA(nsync, wf6(:,4), wf24(:,2), A(:,8), n3(:,41), t3x144(:,:,8), nhel, den(24))
    call cont_QA(nsync, wf18(:,9), wf8(:,6), A(:,9), n3(:,42), t3x144(:,:,9), nhel, den(27))
    call cont_QA(nsync, wf18(:,2), wf8(:,8), A(:,10), n3(:,43), t3x144(:,:,10), nhel, den(29))
    call cont_QA(nsync, wf18(:,3), wf8(:,8), A(:,11), n3(:,44), t3x144(:,:,11), nhel, den(30))
    call cont_QA(nsync, wf18(:,4), wf8(:,8), A(:,12), n3(:,45), t3x144(:,:,12), nhel, den(31))
    call cont_QA(nsync, wf8(:,5), wf18(:,11), A(:,13), n3(:,46), t3x144(:,:,13), nhel, den(34))
    call cont_QA(nsync, wf8(:,5), wf18(:,13), A(:,14), n3(:,47), t3x144(:,:,14), nhel, den(36))
    call cont_QA(nsync, wf6(:,8), wf24(:,3), A(:,15), n3(:,48), t3x144(:,:,15), nhel, den(40))
    call cont_QA(nsync, wf18(:,1), wf8(:,10), A(:,16), n3(:,49), t3x144(:,:,16), nhel, den(42))
    call cont_QA(nsync, wf18(:,14), wf8(:,12), A(:,17), n3(:,50), t3x144(:,:,17), nhel, den(45))
    call cont_QA(nsync, wf8(:,12), wf18(:,15), A(:,18), n3(:,51), t3x144(:,:,18), nhel, den(46))
    call cont_QA(nsync, wf8(:,12), wf18(:,16), A(:,19), n3(:,52), t3x144(:,:,19), nhel, den(47))
    call cont_QA(nsync, wf18(:,6), wf8(:,9), A(:,20), n3(:,53), t3x144(:,:,20), nhel, den(48))
    call cont_QA(nsync, wf18(:,8), wf8(:,9), A(:,21), n3(:,54), t3x144(:,:,21), nhel, den(49))
    call cont_QA(nsync, wf6(:,8), wf24(:,4), A(:,22), n3(:,55), t3x144(:,:,22), nhel, den(52))
    call cont_QA(nsync, wf18(:,9), wf8(:,14), A(:,23), n3(:,56), t3x144(:,:,23), nhel, den(54))
    call cont_QA(nsync, wf18(:,14), wf8(:,16), A(:,24), n3(:,57), t3x144(:,:,24), nhel, den(56))
    call cont_QA(nsync, wf18(:,15), wf8(:,16), A(:,25), n3(:,58), t3x144(:,:,25), nhel, den(57))
    call cont_QA(nsync, wf18(:,16), wf8(:,16), A(:,26), n3(:,59), t3x144(:,:,26), nhel, den(58))
    call cont_QA(nsync, wf18(:,11), wf8(:,13), A(:,27), n3(:,60), t3x144(:,:,27), nhel, den(59))
    call cont_QA(nsync, wf18(:,13), wf8(:,13), A(:,28), n3(:,61), t3x144(:,:,28), nhel, den(60))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,144)
  integer :: empty(0)

  M1(1) = ((-A(j,11)%j-A(j,13)%j-A(j,18)%j-A(j,20)%j)*f(1))/6._/**/REALKIND+((-A(j,4)%j-A(j,6)%j-A(j,25)%j &
       -A(j,27)%j)*f(1))/2._/**/REALKIND+((-A(j,8)%j-A(j,9)%j-A(j,10)%j-A(j,15)%j-A(j,16)%j-A(j,17)%j)*f(2))/6._/**/REALKIND+(( &
       -A(j,1)%j-A(j,2)%j-A(j,3)%j-A(j,22)%j-A(j,23)%j-A(j,24)%j)*f(2))/2._/**/REALKIND+((-A(j,12)%j-A(j,14)%j-A(j,19)%j &
       -A(j,21)%j)*f(3))/6._/**/REALKIND+((-A(j,5)%j-A(j,7)%j-A(j,26)%j-A(j,28)%j)*f(3))/2._/**/REALKIND
  M1(2) = ((A(j,11)%j+A(j,13)%j+A(j,18)%j+A(j,20)%j)*f(1))/2._/**/REALKIND+((A(j,4)%j+A(j,6)%j+A(j,25)%j &
       +A(j,27)%j)*f(1))/6._/**/REALKIND+((A(j,8)%j+A(j,9)%j+A(j,10)%j+A(j,15)%j+A(j,16)%j+A(j,17)%j)*f(2))/2._/**/REALKIND &
       +((A(j,1)%j+A(j,2)%j+A(j,3)%j+A(j,22)%j+A(j,23)%j+A(j,24)%j)*f(2))/6._/**/REALKIND+((A(j,12)%j+A(j,14)%j+A(j,19)%j &
       +A(j,21)%j)*f(3))/2._/**/REALKIND+((A(j,5)%j+A(j,7)%j+A(j,26)%j+A(j,28)%j)*f(3))/6._/**/REALKIND

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
  use ol_colourmatrix_ppwwjj_uuuxuxwwx_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(2)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 2*extcomb
    do i = 1, 2
      do j = 1, 2
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
  use ol_colourmatrix_ppwwjj_uuuxuxwwx_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2)
  complex(REALKIND), intent(in)  :: M2(2)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 2
    do j = 1, 2
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppwwjj_uuuxuxwwx_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,144)
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
    & bind(c,name="ol_f_amp2tree_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_f_amp2ccone_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_f_amp2ccall_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_f_amp2hcone_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_f_amp2hcall_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_amp2tree_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_amp2ccone_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_amp2ccall_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_amp2hcone_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="ol_amp2hcall_ppwwjj_uuuxuxwwx_1")
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
    & bind(c,name="amp2tree_ppwwjj_uuuxuxwwx_1_")
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
    & bind(c,name="amp2ccone_ppwwjj_uuuxuxwwx_1_")
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
    & bind(c,name="amp2ccall_ppwwjj_uuuxuxwwx_1_")
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
    & bind(c,name="amp2hcone_ppwwjj_uuuxuxwwx_1_")
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
    & bind(c,name="amp2hcall_ppwwjj_uuuxuxwwx_1_")
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

end module ol_tree_ppwwjj_uuuxuxwwx_1_/**/REALKIND
