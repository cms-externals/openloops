
module ol_colourmatrix_pptt_ttxbbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(68,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  36,  12,  12,   0]
  K1( 2,:) = [  12,  36,   0,  12]
  K1( 3,:) = [  12,   0,  36,  12]
  K1( 4,:) = [   0,  12,  12,  36]
  K1( 5,:) = [  48,  16,  16,   0]
  K1( 6,:) = [  16,  48,   0,  16]
  K1( 7,:) = [  16,   0,  48,  16]
  K1( 8,:) = [   0,  16,  16,  48]
  K1( 9,:) = [   6,   2,   2,   0]
  K1(10,:) = [   2,   0,  -6, -16]
  K1(11,:) = [   2,  -6,   0, -16]
  K1(12,:) = [   0, -16, -16, -48]
  K1(13,:) = [  48,  16,  16,   0]
  K1(14,:) = [  16,  48,   0,  16]
  K1(15,:) = [  16,   0,  48,  16]
  K1(16,:) = [   0,  16,  16,  48]
  K1(17,:) = [   0,  16,  -2,   6]
  K1(18,:) = [  16,   0,   6,  -2]
  K1(19,:) = [  -2,   6,   0,  16]
  K1(20,:) = [   6,  -2,  16,   0]
  K1(21,:) = [   0,   2, -16,  -6]
  K1(22,:) = [   2,   6,   0,   2]
  K1(23,:) = [ -16,   0, -48, -16]
  K1(24,:) = [  -6,   2, -16,   0]
  K1(25,:) = [  48,  16,  16,   0]
  K1(26,:) = [  16,  48,   0,  16]
  K1(27,:) = [  16,   0,  48,  16]
  K1(28,:) = [   0,  16,  16,  48]
  K1(29,:) = [   0, -16,   2,  -6]
  K1(30,:) = [ -16, -48,   0, -16]
  K1(31,:) = [   2,   0,   6,   2]
  K1(32,:) = [  -6, -16,   2,   0]
  K1(33,:) = [   0,  -2,  16,   6]
  K1(34,:) = [  -2,   0,   6,  16]
  K1(35,:) = [  16,   6,   0,  -2]
  K1(36,:) = [   6,  16,  -2,   0]
  K1(37,:) = [ -48, -16, -16,   0]
  K1(38,:) = [ -16,   0,  -6,   2]
  K1(39,:) = [ -16,  -6,   0,   2]
  K1(40,:) = [   0,   2,   2,   6]
  K1(41,:) = [  48,  16,  16,   0]
  K1(42,:) = [  16,  48,   0,  16]
  K1(43,:) = [  16,   0,  48,  16]
  K1(44,:) = [   0,  16,  16,  48]
  K1(45,:) = [ -54, -18, -18,   0]
  K1(46,:) = [ -18,   0,   0,  18]
  K1(47,:) = [ -18,   0, -54, -18]
  K1(48,:) = [   0,  18, -18,   0]
  K1(49,:) = [ -54, -18, -18,   0]
  K1(50,:) = [ -18, -54,   0, -18]
  K1(51,:) = [ -18,   0,   0,  18]
  K1(52,:) = [   0, -18,  18,   0]
  K1(53,:) = [   0, -18,  18,   0]
  K1(54,:) = [ -18, -54,   0, -18]
  K1(55,:) = [  18,   0,   0, -18]
  K1(56,:) = [   0, -18, -18, -54]
  K1(57,:) = [   0,  18, -18,   0]
  K1(58,:) = [  18,   0,   0, -18]
  K1(59,:) = [ -18,   0, -54, -18]
  K1(60,:) = [   0, -18, -18, -54]
  K1(61,:) = [ 108,  36,  36,   0]
  K1(62,:) = [  36, 108,   0,  36]
  K1(63,:) = [  36,   0, 108,  36]
  K1(64,:) = [   0,  36,  36, 108]
  K1(65,:) = [   0,   0,   0,   0]
  K1(66,:) = [   0,   0,   0,   0]
  K1(67,:) = [   0,   0,   0,   0]
  K1(68,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_pptt_ttxbbxg_1_/**/REALKIND



module ol_forced_parameters_pptt_ttxbbxg_1_/**/REALKIND
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
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pptt_ttxbbxg_1_/**/REALKIND

module ol_tree_pptt_ttxbbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(11)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 32 ! number of helicity configurations
  integer(intkind2), save :: nhel = 32 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(32) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,32) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*gQCD**3
    f(2) = gQCD**3

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,20) - MB2)
  den(6) = 1 / (Q(5,24) - MB2)
  den(8) = 1 / (Q(5,17) - MT2)
  den(10) = 1 / (Q(5,18) - MT2)

  ! denominators

  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(2)*den(8)
  den(11) = den(2)*den(10)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pptt_ttxbbxg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pptt_ttxbbxg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for top anti-top bottom anti-bottom glue -> 0
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
  use ol_external_pptt_ttxbbxg_1, only: external_perm_pptt_ttxbbxg_1, &
    & external_perm_inv_pptt_ttxbbxg_1, extcomb_perm_pptt_ttxbbxg_1, &
    & average_factor_pptt_ttxbbxg_1
  use ol_external_pptt_ttxbbxg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pptt_ttxbbxg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pptt_ttxbbxg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pptt_ttxbbxg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,5)
  real(REALKIND),  intent(out) :: M2(0:17-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, l, m, n
  real(REALKIND)    :: P(0:3,5)
  real(REALKIND)    :: extmasses2(5)
  real(REALKIND)    :: M2add(0:17-1), M2munuadd
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,32)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,10), wf8(8,5), wf32(32,5)

  type(polcont) :: A(32,5)
  complex(REALKIND) :: Aj(5)

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
  extmasses2 = [ rMT2, rMT2, rMB2, rMB2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pptt_ttxbbxg_1,5)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,5)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pptt_ttxbbxg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pptt_ttxbbxg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pptt_ttxbbxg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rMT, H1, ex1)
  call wf_A(P(:,2), rMT, H2, ex2)
  call wf_Q(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_V(P(:,5), rZERO, H5, ex5)


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

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex5, Q(:,16), wf8(:,1), n3(:,3), t3x8(:,:,1))
  call vert_VQ_A(ntry, ex5, ex3, wf4(:,3), n3(:,4), t3x4(:,:,3))
  call prop_Q_A(ntry, wf4(:,3), Q(:,20), MB, 1_intkind1, wf4(:,4), n2(1))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,2), n3(:,5), t3x8(:,:,2))
  call vert_AV_Q(ntry, ex4, ex5, wf4(:,5), n3(:,6), t3x4(:,:,4))
  call prop_A_Q(ntry, wf4(:,5), Q(:,24), MB, 1_intkind1, wf4(:,6), n2(2))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,3), n3(:,7), t3x8(:,:,3))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,7), n3(:,8), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,7), Q(:,17), MT, 1_intkind1, wf4(:,8), n2(3))
  call vert_QA_V(ntry, wf4(:,8), ex2, wf8(:,4), n3(:,9), t3x8(:,:,4))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,9), n3(:,10), t3x4(:,:,6))
  call prop_A_Q(ntry, wf4(:,9), Q(:,18), MT, 1_intkind1, wf4(:,10), n2(4))
  call vert_QA_V(ntry, ex1, wf4(:,10), wf8(:,5), n3(:,11), t3x8(:,:,5))


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
  M2add = M2 / average_factor_pptt_ttxbbxg_1

  do k = 0, 17-1
    M2(k) = M2add(extcomb_perm_pptt_ttxbbxg_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*5-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_VV(nsync, wf4(:,2), wf8(:,1), A(:,1), n3(:,12), t3x32(:,:,1), nhel, den(3))
    call cont_QA(nsync, wf4(:,4), wf8(:,2), A(:,2), n3(:,13), t3x32(:,:,2), nhel, den(5))
    call cont_QA(nsync, wf4(:,6), wf8(:,3), A(:,3), n3(:,14), t3x32(:,:,3), nhel, den(7))
    call cont_VV(nsync, wf4(:,2), wf8(:,4), A(:,4), n3(:,15), t3x32(:,:,4), nhel, den(9))
    call cont_VV(nsync, wf4(:,2), wf8(:,5), A(:,5), n3(:,16), t3x32(:,:,5), nhel, den(11))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,32)
  integer :: empty(0)

  M1(1) = ((-A(j,4)%j-A(j,5)%j)*f(1))/6._/**/REALKIND
  M1(2) = ((A(j,2)%j+A(j,5)%j)*f(1))/2._/**/REALKIND+(CI*A(j,1)%j*f(2))/2._/**/REALKIND
  M1(3) = ((A(j,3)%j+A(j,4)%j)*f(1))/2._/**/REALKIND-(CI*A(j,1)%j*f(2))/2._/**/REALKIND
  M1(4) = ((-A(j,2)%j-A(j,3)%j)*f(1))/6._/**/REALKIND

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
  use ol_colourmatrix_pptt_ttxbbxg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(4)
  real(REALKIND),    intent(out) :: M2colint(0:17-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 4*extcomb
    do i = 1, 4
      do j = 1, 4
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
  use ol_colourmatrix_pptt_ttxbbxg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(4)
  complex(REALKIND), intent(in)  :: M2(4)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 4
    do j = 1, 4
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_pptt_ttxbbxg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,32)
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
    & bind(c,name="ol_f_amp2tree_pptt_ttxbbxg_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,5)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:17-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_pptt_ttxbbxg_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,5)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:17-1)
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
    & bind(c,name="ol_f_amp2ccall_pptt_ttxbbxg_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,5)
  real(REALKIND),  intent(out) :: M2(0:17-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    17, [ (k, k = 0, 17-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_pptt_ttxbbxg_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,5)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:17-1)
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
    & bind(c,name="ol_f_amp2hcall_pptt_ttxbbxg_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,5)
  real(REALKIND),  intent(out) :: M2(5)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:17-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(5)
  do J = 1, 5
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 5,extcombs, M2munu)
  do J = 1, 5
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_pptt_ttxbbxg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_pptt_ttxbbxg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_pptt_ttxbbxg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2(0:17-1)
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2(0:17-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_pptt_ttxbbxg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,5)
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
    & bind(c,name="ol_amp2hcall_pptt_ttxbbxg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2(5)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2(5)
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
    & bind(c,name="amp2tree_pptt_ttxbbxg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_pptt_ttxbbxg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_pptt_ttxbbxg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2(0:17-1)
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2(0:17-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_pptt_ttxbbxg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,5)
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
    & bind(c,name="amp2hcall_pptt_ttxbbxg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,5)
  real(c_double), intent(out) :: m2(5)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,5)
  real(DREALKIND) :: f_m2(5)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_pptt_ttxbbxg_1_/**/REALKIND
