
module ol_external_heftpphjjj_dddxdxhgg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_heftpphjjj_dddxdxhgg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_heftpphjjj_dddxdxhgg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_heftpphjjj_dddxdxhgg_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_heftpphjjj_dddxdxhgg_1(7) = &
                     [ 1, 1, 2, 2, 3, 4, 4 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_heftpphjjj_dddxdxhgg_1(7) = &
                     [ 6, 6, 6, 6, 1, 16, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_heftpphjjj_dddxdxhgg_1 = &
                     144
  integer, save :: channel_number_heftpphjjj_dddxdxhgg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,64) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(64,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_heftpphjjj_dddxdxhgg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_heftpphjjj_dddxdxhgg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_heftpphjjj_dddxdxhgg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_heftpphjjj_dddxdxhgg_1(7)
    external_perm_heftpphjjj_dddxdxhgg_1 = perm
    do i = 1, 7
      external_perm_inv_heftpphjjj_dddxdxhgg_1( &
        external_perm_heftpphjjj_dddxdxhgg_1(i)) = i
      particle_types_perm_heftpphjjj_dddxdxhgg_1(i) = &
        particle_types_heftpphjjj_dddxdxhgg_1( &
        external_perm_heftpphjjj_dddxdxhgg_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_heftpphjjj_dddxdxhgg_1(i) >= &
          external_perm_heftpphjjj_dddxdxhgg_1(j)) then
          ii = external_perm_heftpphjjj_dddxdxhgg_1(i)
          jj = external_perm_heftpphjjj_dddxdxhgg_1(j)
        else
          ii = external_perm_heftpphjjj_dddxdxhgg_1(j)
          jj = external_perm_heftpphjjj_dddxdxhgg_1(i)
        end if
        extcomb_perm_heftpphjjj_dddxdxhgg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_heftpphjjj_dddxdxhgg_1 = &
      average_factors_heftpphjjj_dddxdxhgg_1( &
      external_perm_heftpphjjj_dddxdxhgg_1(1)) &
      * average_factors_heftpphjjj_dddxdxhgg_1( &
      external_perm_heftpphjjj_dddxdxhgg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_heftpphjjj_dddxdxhgg_1 = &
          average_factor_heftpphjjj_dddxdxhgg_1 &
          * factorial(count(particle_types_perm_heftpphjjj_dddxdxhgg_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_heftpphjjj_dddxdxhgg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_heftpphjjj_dddxdxhgg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rMH_unscaled, rZERO, rZERO ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_heftpphjjj_dddxdxhgg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_heftpphjjj_dddxdxhgg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_heftpphjjj_dddxdxhgg_1")
    use KIND_TYPES, only: DREALKIND
    use ol_kinematics_/**/DREALKIND, only: rambo_generic => rambo
    implicit none
    real(DREALKIND), intent(in) :: sqrt_s
    real(DREALKIND), intent(out) :: p_rambo(0:3,7)
    real(DREALKIND) :: m_ex(7)
    call get_masses(m_ex)
    call rambo_generic(sqrt_s, m_ex, p_rambo)
  end subroutine rambo


  subroutine rambo_c(sqrt_s, p_rambo) &
      & bind(c,name="ol_rambo_heftpphjjj_dddxdxhgg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in) :: sqrt_s
    real(c_double), intent(out) :: p_rambo(0:3,7)
    real(DREALKIND) :: f_sqrt_s
    real(DREALKIND) :: f_p_rambo(0:3,7)
    f_sqrt_s = sqrt_s
    call rambo(f_sqrt_s, f_p_rambo)
    p_rambo = f_p_rambo
  end subroutine rambo_c


  subroutine hel_init
    implicit none
    integer :: binpos, flip, binco
    hel_not_initialised = .false.
    ! helicity configurations for this process
  H(:, 1) = [ -1, -1, -1, -1,  0, -1, -1 ]
  H(:, 2) = [ -1, -1, -1, -1,  0, -1,  1 ]
  H(:, 3) = [ -1, -1, -1, -1,  0,  1, -1 ]
  H(:, 4) = [ -1, -1, -1, -1,  0,  1,  1 ]
  H(:, 5) = [ -1, -1, -1,  1,  0, -1, -1 ]
  H(:, 6) = [ -1, -1, -1,  1,  0, -1,  1 ]
  H(:, 7) = [ -1, -1, -1,  1,  0,  1, -1 ]
  H(:, 8) = [ -1, -1, -1,  1,  0,  1,  1 ]
  H(:, 9) = [ -1, -1,  1, -1,  0, -1, -1 ]
  H(:,10) = [ -1, -1,  1, -1,  0, -1,  1 ]
  H(:,11) = [ -1, -1,  1, -1,  0,  1, -1 ]
  H(:,12) = [ -1, -1,  1, -1,  0,  1,  1 ]
  H(:,13) = [ -1, -1,  1,  1,  0, -1, -1 ]
  H(:,14) = [ -1, -1,  1,  1,  0, -1,  1 ]
  H(:,15) = [ -1, -1,  1,  1,  0,  1, -1 ]
  H(:,16) = [ -1, -1,  1,  1,  0,  1,  1 ]
  H(:,17) = [ -1,  1, -1, -1,  0, -1, -1 ]
  H(:,18) = [ -1,  1, -1, -1,  0, -1,  1 ]
  H(:,19) = [ -1,  1, -1, -1,  0,  1, -1 ]
  H(:,20) = [ -1,  1, -1, -1,  0,  1,  1 ]
  H(:,21) = [ -1,  1, -1,  1,  0, -1, -1 ]
  H(:,22) = [ -1,  1, -1,  1,  0, -1,  1 ]
  H(:,23) = [ -1,  1, -1,  1,  0,  1, -1 ]
  H(:,24) = [ -1,  1, -1,  1,  0,  1,  1 ]
  H(:,25) = [ -1,  1,  1, -1,  0, -1, -1 ]
  H(:,26) = [ -1,  1,  1, -1,  0, -1,  1 ]
  H(:,27) = [ -1,  1,  1, -1,  0,  1, -1 ]
  H(:,28) = [ -1,  1,  1, -1,  0,  1,  1 ]
  H(:,29) = [ -1,  1,  1,  1,  0, -1, -1 ]
  H(:,30) = [ -1,  1,  1,  1,  0, -1,  1 ]
  H(:,31) = [ -1,  1,  1,  1,  0,  1, -1 ]
  H(:,32) = [ -1,  1,  1,  1,  0,  1,  1 ]
  H(:,33) = [  1, -1, -1, -1,  0, -1, -1 ]
  H(:,34) = [  1, -1, -1, -1,  0, -1,  1 ]
  H(:,35) = [  1, -1, -1, -1,  0,  1, -1 ]
  H(:,36) = [  1, -1, -1, -1,  0,  1,  1 ]
  H(:,37) = [  1, -1, -1,  1,  0, -1, -1 ]
  H(:,38) = [  1, -1, -1,  1,  0, -1,  1 ]
  H(:,39) = [  1, -1, -1,  1,  0,  1, -1 ]
  H(:,40) = [  1, -1, -1,  1,  0,  1,  1 ]
  H(:,41) = [  1, -1,  1, -1,  0, -1, -1 ]
  H(:,42) = [  1, -1,  1, -1,  0, -1,  1 ]
  H(:,43) = [  1, -1,  1, -1,  0,  1, -1 ]
  H(:,44) = [  1, -1,  1, -1,  0,  1,  1 ]
  H(:,45) = [  1, -1,  1,  1,  0, -1, -1 ]
  H(:,46) = [  1, -1,  1,  1,  0, -1,  1 ]
  H(:,47) = [  1, -1,  1,  1,  0,  1, -1 ]
  H(:,48) = [  1, -1,  1,  1,  0,  1,  1 ]
  H(:,49) = [  1,  1, -1, -1,  0, -1, -1 ]
  H(:,50) = [  1,  1, -1, -1,  0, -1,  1 ]
  H(:,51) = [  1,  1, -1, -1,  0,  1, -1 ]
  H(:,52) = [  1,  1, -1, -1,  0,  1,  1 ]
  H(:,53) = [  1,  1, -1,  1,  0, -1, -1 ]
  H(:,54) = [  1,  1, -1,  1,  0, -1,  1 ]
  H(:,55) = [  1,  1, -1,  1,  0,  1, -1 ]
  H(:,56) = [  1,  1, -1,  1,  0,  1,  1 ]
  H(:,57) = [  1,  1,  1, -1,  0, -1, -1 ]
  H(:,58) = [  1,  1,  1, -1,  0, -1,  1 ]
  H(:,59) = [  1,  1,  1, -1,  0,  1, -1 ]
  H(:,60) = [  1,  1,  1, -1,  0,  1,  1 ]
  H(:,61) = [  1,  1,  1,  1,  0, -1, -1 ]
  H(:,62) = [  1,  1,  1,  1,  0, -1,  1 ]
  H(:,63) = [  1,  1,  1,  1,  0,  1, -1 ]
  H(:,64) = [  1,  1,  1,  1,  0,  1,  1 ]

  H_HC(:,6) = [ ((((2*(binco-1)+flip)*2+binpos, flip = 0, 1), binpos = 1, 2), binco = 1, 64/2/2) ]
  H_HC(:,7) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 64/1/2) ]
  end subroutine hel_init

end module ol_external_heftpphjjj_dddxdxhgg_1


module colour_basis_heftpphjjj_dddxdxhgg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,0,2,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_heftpphjjj_dddxdxhgg_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(7)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 12
    ncoupl = 1
    maxpows = 1
    nhel = 64
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_heftpphjjj_dddxdxhgg_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,12)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([2], [1,1])
#endif
#if 12 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [2,564,1,2,558,1,2,540,1,2,534,1,3,590,1,3,592,1,3,596,1,3,598,1,3,710,1,3,712,1,3,716,1,3,718,1], &
      [3,12])
#endif
  end subroutine tree_colbasis

end module colour_basis_heftpphjjj_dddxdxhgg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_heftpphjjj_dddxdxhgg_1(perm)
  use ol_external_heftpphjjj_dddxdxhgg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_heftpphjjj_dddxdxhgg_1

! **********************************************************************
module ol_heltables_heftpphjjj_dddxdxhgg_1
! **********************************************************************
  use KIND_TYPES, only: intkind2
  implicit none

  logical :: heltables_not_init = .true.

  ! helicity states of external particles
  ! integer, save :: &
  !   H1(2) = [-1,1], &
  !   H2(3) = [-1,0,1]
  !   ...
  integer, save :: &
    H1(2) = [-1,1], &
    H2(2) = [-1,1], &
    H3(2) = [-1,1], &
    H4(2) = [-1,1], &
    H5(1) = [0], &
    H6(2) = [-1,1], &
    H7(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(38), n3(3,548), n4(4,35), n5(5,6)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x2(2,2,2), t3x4(2,4,28), t3x16(2,16,86), t3x8(2,8,120), t3x64(2,64,312), t4x4(3,4,1), t4x8(3,8,8), &
    t4x32(3,32,12), t4x16(3,16,14), t5x16(4,16,6)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(64,7)
  integer,           save :: exthel(64,7)
  integer,           save :: firstpol(7)

  contains

! **********************************************************************
subroutine init_heltables
! **********************************************************************
  use ol_helicity_init, only: heltable
  implicit none

  ! I/O helicity tables for vertices, propagators and contractions;
  ! helicity table for a vertex call: n_in/n_out are the number helicity states of the incoming/outgoing wave functions
  ! call heltable([<n_in1>, <n_in2>, ..., <n_out>], n, t)
  ! propagators only need the number of helicity configurations which is equal for the incoming and outgoing wave function
  ! n = <n>
  call heltable([2,2,4], n3(:,1), t3x4(:,:,1))
  call heltable([2,2,4], n3(:,2), t3x4(:,:,2))
  call heltable([1,4,2,2,16], n5(:,1), t5x16(:,:,1))
  call heltable([1,2,2,4,16], n5(:,2), t5x16(:,:,2))
  call heltable([1,2,4,2,16], n5(:,3), t5x16(:,:,3))
  call heltable([2,2,4], n3(:,3), t3x4(:,:,3))
  call heltable([2,2,4], n3(:,4), t3x4(:,:,4))
  call heltable([1,4,2,2,16], n5(:,4), t5x16(:,:,4))
  call heltable([1,2,2,4,16], n5(:,5), t5x16(:,:,5))
  call heltable([1,2,4,2,16], n5(:,6), t5x16(:,:,6))
  call heltable([1,2,2], n3(:,5), t3x2(:,:,1))
  call heltable([4,4,2,32], n4(:,1), t4x32(:,:,1))
  call heltable([4,2,4,32], n4(:,2), t4x32(:,:,2))
  call heltable([2,4,4,32], n4(:,3), t4x32(:,:,3))
  call heltable([1,2,2], n3(:,6), t3x2(:,:,2))
  call heltable([4,4,2,32], n4(:,4), t4x32(:,:,4))
  call heltable([4,2,4,32], n4(:,5), t4x32(:,:,5))
  call heltable([2,4,4,32], n4(:,6), t4x32(:,:,6))
  call heltable([1,4,4], n3(:,7), t3x4(:,:,5))
  call heltable([4,2,2,16], n4(:,7), t4x16(:,:,1))
  call heltable([2,2,4,16], n4(:,8), t4x16(:,:,2))
  call heltable([2,4,2,16], n4(:,9), t4x16(:,:,3))
  call heltable([1,4,4], n3(:,8), t3x4(:,:,6))
  call heltable([4,2,2,16], n4(:,10), t4x16(:,:,4))
  call heltable([2,2,4,16], n4(:,11), t4x16(:,:,5))
  call heltable([2,4,2,16], n4(:,12), t4x16(:,:,6))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,7))
  call heltable([1,4,4,16], n4(:,13), t4x16(:,:,7))
  call heltable([4,2,8], n3(:,10), t3x8(:,:,1))
  call heltable([1,4,2,8], n4(:,14), t4x8(:,:,1))
  call heltable([4,2,8], n3(:,11), t3x8(:,:,2))
  call heltable([1,4,2,8], n4(:,15), t4x8(:,:,2))
  call heltable([4,2,8], n3(:,12), t3x8(:,:,3))
  call heltable([1,4,2,8], n4(:,16), t4x8(:,:,3))
  call heltable([4,2,8], n3(:,13), t3x8(:,:,4))
  call heltable([1,4,2,8], n4(:,17), t4x8(:,:,4))
  call heltable([1,2,2,4], n4(:,18), t4x4(:,:,1))
  call heltable([4,4,16], n3(:,14), t3x16(:,:,1))
  call heltable([2,2,4], n3(:,15), t3x4(:,:,8))
  n2(1) = 4
  call heltable([4,2,8], n3(:,16), t3x8(:,:,5))
  call heltable([2,2,4], n3(:,17), t3x4(:,:,9))
  n2(2) = 4
  call heltable([2,4,8], n3(:,18), t3x8(:,:,6))
  call heltable([2,2,4], n3(:,19), t3x4(:,:,10))
  n2(3) = 4
  call heltable([4,2,8], n3(:,20), t3x8(:,:,7))
  call heltable([2,2,4], n3(:,21), t3x4(:,:,11))
  n2(4) = 4
  call heltable([2,4,8], n3(:,22), t3x8(:,:,8))
  call heltable([4,2,8], n3(:,23), t3x8(:,:,9))
  call heltable([2,4,8], n3(:,24), t3x8(:,:,10))
  n2(5) = 8
  call heltable([4,2,8], n3(:,25), t3x8(:,:,11))
  call heltable([2,4,8], n3(:,26), t3x8(:,:,12))
  n2(6) = 8
  call heltable([4,4,2,32], n4(:,19), t4x32(:,:,7))
  call heltable([4,2,4,32], n4(:,20), t4x32(:,:,8))
  call heltable([2,4,4,32], n4(:,21), t4x32(:,:,9))
  call heltable([4,4,2,32], n4(:,22), t4x32(:,:,10))
  call heltable([4,2,4,32], n4(:,23), t4x32(:,:,11))
  call heltable([2,4,4,32], n4(:,24), t4x32(:,:,12))
  call heltable([1,4,4], n3(:,27), t3x4(:,:,12))
  call heltable([4,2,2,16], n4(:,25), t4x16(:,:,8))
  call heltable([2,2,4,16], n4(:,26), t4x16(:,:,9))
  call heltable([2,4,2,16], n4(:,27), t4x16(:,:,10))
  call heltable([1,4,4], n3(:,28), t3x4(:,:,13))
  call heltable([4,2,2,16], n4(:,28), t4x16(:,:,11))
  call heltable([2,2,4,16], n4(:,29), t4x16(:,:,12))
  call heltable([2,4,2,16], n4(:,30), t4x16(:,:,13))
  call heltable([1,4,4,16], n4(:,31), t4x16(:,:,14))
  call heltable([4,2,8], n3(:,29), t3x8(:,:,13))
  call heltable([1,4,2,8], n4(:,32), t4x8(:,:,5))
  call heltable([4,2,8], n3(:,30), t3x8(:,:,14))
  call heltable([1,4,2,8], n4(:,33), t4x8(:,:,6))
  call heltable([4,2,8], n3(:,31), t3x8(:,:,15))
  call heltable([1,4,2,8], n4(:,34), t4x8(:,:,7))
  call heltable([4,2,8], n3(:,32), t3x8(:,:,16))
  call heltable([1,4,2,8], n4(:,35), t4x8(:,:,8))
  call heltable([4,4,16], n3(:,33), t3x16(:,:,2))
  call heltable([2,2,4], n3(:,34), t3x4(:,:,14))
  n2(7) = 4
  call heltable([4,2,8], n3(:,35), t3x8(:,:,17))
  call heltable([2,4,8], n3(:,36), t3x8(:,:,18))
  call heltable([2,2,4], n3(:,37), t3x4(:,:,15))
  n2(8) = 4
  call heltable([4,2,8], n3(:,38), t3x8(:,:,19))
  call heltable([2,4,8], n3(:,39), t3x8(:,:,20))
  call heltable([4,2,8], n3(:,40), t3x8(:,:,21))
  n2(9) = 8
  call heltable([4,2,8], n3(:,41), t3x8(:,:,22))
  call heltable([2,4,8], n3(:,42), t3x8(:,:,23))
  n2(10) = 8
  call heltable([4,2,8], n3(:,43), t3x8(:,:,24))
  call heltable([2,2,4], n3(:,44), t3x4(:,:,16))
  n2(11) = 4
  call heltable([2,4,8], n3(:,45), t3x8(:,:,25))
  call heltable([4,2,8], n3(:,46), t3x8(:,:,26))
  call heltable([2,2,4], n3(:,47), t3x4(:,:,17))
  n2(12) = 4
  call heltable([2,4,8], n3(:,48), t3x8(:,:,27))
  call heltable([4,2,8], n3(:,49), t3x8(:,:,28))
  call heltable([2,4,8], n3(:,50), t3x8(:,:,29))
  n2(13) = 8
  call heltable([2,4,8], n3(:,51), t3x8(:,:,30))
  call heltable([4,2,8], n3(:,52), t3x8(:,:,31))
  call heltable([2,4,8], n3(:,53), t3x8(:,:,32))
  call heltable([4,2,8], n3(:,54), t3x8(:,:,33))
  call heltable([2,4,8], n3(:,55), t3x8(:,:,34))
  call heltable([4,2,8], n3(:,56), t3x8(:,:,35))
  n2(14) = 8
  call heltable([2,4,8], n3(:,57), t3x8(:,:,36))
  call heltable([4,2,8], n3(:,58), t3x8(:,:,37))
  call heltable([4,2,8], n3(:,59), t3x8(:,:,38))
  call heltable([2,2,4], n3(:,60), t3x4(:,:,18))
  call heltable([4,2,8], n3(:,61), t3x8(:,:,39))
  call heltable([4,2,8], n3(:,62), t3x8(:,:,40))
  call heltable([2,2,4], n3(:,63), t3x4(:,:,19))
  call heltable([4,4,16], n3(:,64), t3x16(:,:,3))
  call heltable([4,4,16], n3(:,65), t3x16(:,:,4))
  call heltable([1,4,4], n3(:,66), t3x4(:,:,20))
  call heltable([4,2,8], n3(:,67), t3x8(:,:,41))
  call heltable([4,2,8], n3(:,68), t3x8(:,:,42))
  call heltable([4,2,8], n3(:,69), t3x8(:,:,43))
  call heltable([1,8,8], n3(:,70), t3x8(:,:,44))
  call heltable([4,2,8], n3(:,71), t3x8(:,:,45))
  call heltable([1,8,8], n3(:,72), t3x8(:,:,46))
  call heltable([4,4,16], n3(:,73), t3x16(:,:,5))
  call heltable([2,4,8], n3(:,74), t3x8(:,:,47))
  n2(15) = 8
  call heltable([2,2,4], n3(:,75), t3x4(:,:,21))
  call heltable([4,4,16], n3(:,76), t3x16(:,:,6))
  n2(16) = 4
  call heltable([8,2,16], n3(:,77), t3x16(:,:,7))
  call heltable([2,4,8], n3(:,78), t3x8(:,:,48))
  n2(17) = 8
  call heltable([2,4,8], n3(:,79), t3x8(:,:,49))
  call heltable([1,8,8], n3(:,80), t3x8(:,:,50))
  call heltable([4,4,16], n3(:,81), t3x16(:,:,8))
  call heltable([4,2,8], n3(:,82), t3x8(:,:,51))
  call heltable([2,2,4], n3(:,83), t3x4(:,:,22))
  call heltable([4,4,16], n3(:,84), t3x16(:,:,9))
  n2(18) = 4
  call heltable([8,2,16], n3(:,85), t3x16(:,:,10))
  call heltable([4,2,8], n3(:,86), t3x8(:,:,52))
  n2(19) = 8
  call heltable([4,2,8], n3(:,87), t3x8(:,:,53))
  call heltable([1,8,8], n3(:,88), t3x8(:,:,54))
  call heltable([2,4,8], n3(:,89), t3x8(:,:,55))
  call heltable([2,2,4], n3(:,90), t3x4(:,:,23))
  call heltable([4,4,16], n3(:,91), t3x16(:,:,11))
  n2(20) = 4
  call heltable([4,2,8], n3(:,92), t3x8(:,:,56))
  call heltable([2,2,4], n3(:,93), t3x4(:,:,24))
  call heltable([4,4,16], n3(:,94), t3x16(:,:,12))
  n2(21) = 4
  call heltable([8,2,16], n3(:,95), t3x16(:,:,13))
  call heltable([2,8,16], n3(:,96), t3x16(:,:,14))
  call heltable([2,8,16], n3(:,97), t3x16(:,:,15))
  call heltable([2,4,8], n3(:,98), t3x8(:,:,57))
  call heltable([4,2,8], n3(:,99), t3x8(:,:,58))
  call heltable([2,4,8], n3(:,100), t3x8(:,:,59))
  call heltable([2,8,16], n3(:,101), t3x16(:,:,16))
  call heltable([2,4,8], n3(:,102), t3x8(:,:,60))
  n2(22) = 8
  call heltable([1,8,8], n3(:,103), t3x8(:,:,61))
  call heltable([2,8,16], n3(:,104), t3x16(:,:,17))
  call heltable([4,2,8], n3(:,105), t3x8(:,:,62))
  n2(23) = 8
  call heltable([1,8,8], n3(:,106), t3x8(:,:,63))
  call heltable([2,8,16], n3(:,107), t3x16(:,:,18))
  call heltable([2,4,8], n3(:,108), t3x8(:,:,64))
  call heltable([4,2,8], n3(:,109), t3x8(:,:,65))
  call heltable([2,4,8], n3(:,110), t3x8(:,:,66))
  call heltable([2,4,8], n3(:,111), t3x8(:,:,67))
  n2(24) = 8
  call heltable([2,8,16], n3(:,112), t3x16(:,:,19))
  call heltable([4,2,8], n3(:,113), t3x8(:,:,68))
  n2(25) = 8
  call heltable([8,2,16], n3(:,114), t3x16(:,:,20))
  call heltable([4,2,8], n3(:,115), t3x8(:,:,69))
  call heltable([4,2,8], n3(:,116), t3x8(:,:,70))
  call heltable([4,2,8], n3(:,117), t3x8(:,:,71))
  call heltable([4,2,8], n3(:,118), t3x8(:,:,72))
  call heltable([4,4,16], n3(:,119), t3x16(:,:,21))
  call heltable([4,4,16], n3(:,120), t3x16(:,:,22))
  call heltable([4,2,8], n3(:,121), t3x8(:,:,73))
  call heltable([4,2,8], n3(:,122), t3x8(:,:,74))
  call heltable([4,2,8], n3(:,123), t3x8(:,:,75))
  call heltable([1,8,8], n3(:,124), t3x8(:,:,76))
  call heltable([4,2,8], n3(:,125), t3x8(:,:,77))
  call heltable([1,8,8], n3(:,126), t3x8(:,:,78))
  call heltable([4,4,16], n3(:,127), t3x16(:,:,23))
  call heltable([2,4,8], n3(:,128), t3x8(:,:,79))
  n2(26) = 8
  call heltable([4,4,16], n3(:,129), t3x16(:,:,24))
  call heltable([8,2,16], n3(:,130), t3x16(:,:,25))
  call heltable([1,8,8], n3(:,131), t3x8(:,:,80))
  call heltable([2,4,8], n3(:,132), t3x8(:,:,81))
  n2(27) = 8
  call heltable([2,4,8], n3(:,133), t3x8(:,:,82))
  call heltable([4,4,16], n3(:,134), t3x16(:,:,26))
  call heltable([2,2,4], n3(:,135), t3x4(:,:,25))
  call heltable([4,4,16], n3(:,136), t3x16(:,:,27))
  n2(28) = 4
  call heltable([1,8,8], n3(:,137), t3x8(:,:,83))
  call heltable([8,2,16], n3(:,138), t3x16(:,:,28))
  call heltable([4,2,8], n3(:,139), t3x8(:,:,84))
  call heltable([2,4,8], n3(:,140), t3x8(:,:,85))
  call heltable([4,4,16], n3(:,141), t3x16(:,:,29))
  call heltable([2,2,4], n3(:,142), t3x4(:,:,26))
  call heltable([4,4,16], n3(:,143), t3x16(:,:,30))
  n2(29) = 4
  call heltable([8,2,16], n3(:,144), t3x16(:,:,31))
  call heltable([2,8,16], n3(:,145), t3x16(:,:,32))
  call heltable([4,2,8], n3(:,146), t3x8(:,:,86))
  call heltable([2,8,16], n3(:,147), t3x16(:,:,33))
  call heltable([2,4,8], n3(:,148), t3x8(:,:,87))
  call heltable([2,4,8], n3(:,149), t3x8(:,:,88))
  call heltable([2,8,16], n3(:,150), t3x16(:,:,34))
  call heltable([1,8,8], n3(:,151), t3x8(:,:,89))
  call heltable([2,4,8], n3(:,152), t3x8(:,:,90))
  n2(30) = 8
  call heltable([1,8,8], n3(:,153), t3x8(:,:,91))
  call heltable([2,8,16], n3(:,154), t3x16(:,:,35))
  call heltable([2,8,16], n3(:,155), t3x16(:,:,36))
  call heltable([4,2,8], n3(:,156), t3x8(:,:,92))
  call heltable([2,4,8], n3(:,157), t3x8(:,:,93))
  call heltable([2,4,8], n3(:,158), t3x8(:,:,94))
  call heltable([2,8,16], n3(:,159), t3x16(:,:,37))
  call heltable([4,2,8], n3(:,160), t3x8(:,:,95))
  n2(31) = 8
  call heltable([8,2,16], n3(:,161), t3x16(:,:,38))
  call heltable([4,4,16], n3(:,162), t3x16(:,:,39))
  n2(32) = 8
  call heltable([2,2,4], n3(:,163), t3x4(:,:,27))
  call heltable([4,4,16], n3(:,164), t3x16(:,:,40))
  n2(33) = 4
  call heltable([8,2,16], n3(:,165), t3x16(:,:,41))
  call heltable([2,4,8], n3(:,166), t3x8(:,:,96))
  call heltable([1,8,8], n3(:,167), t3x8(:,:,97))
  call heltable([4,4,16], n3(:,168), t3x16(:,:,42))
  call heltable([4,2,8], n3(:,169), t3x8(:,:,98))
  call heltable([4,4,16], n3(:,170), t3x16(:,:,43))
  call heltable([8,2,16], n3(:,171), t3x16(:,:,44))
  call heltable([4,2,8], n3(:,172), t3x8(:,:,99))
  n2(34) = 8
  call heltable([4,2,8], n3(:,173), t3x8(:,:,100))
  call heltable([1,8,8], n3(:,174), t3x8(:,:,101))
  call heltable([2,2,4], n3(:,175), t3x4(:,:,28))
  call heltable([4,4,16], n3(:,176), t3x16(:,:,45))
  n2(35) = 4
  call heltable([4,2,8], n3(:,177), t3x8(:,:,102))
  call heltable([4,4,16], n3(:,178), t3x16(:,:,46))
  call heltable([8,2,16], n3(:,179), t3x16(:,:,47))
  call heltable([2,8,16], n3(:,180), t3x16(:,:,48))
  call heltable([2,8,16], n3(:,181), t3x16(:,:,49))
  call heltable([4,2,8], n3(:,182), t3x8(:,:,103))
  call heltable([2,4,8], n3(:,183), t3x8(:,:,104))
  call heltable([2,8,16], n3(:,184), t3x16(:,:,50))
  call heltable([1,8,8], n3(:,185), t3x8(:,:,105))
  call heltable([2,8,16], n3(:,186), t3x16(:,:,51))
  call heltable([4,2,8], n3(:,187), t3x8(:,:,106))
  n2(36) = 8
  call heltable([1,8,8], n3(:,188), t3x8(:,:,107))
  call heltable([2,8,16], n3(:,189), t3x16(:,:,52))
  call heltable([4,2,8], n3(:,190), t3x8(:,:,108))
  call heltable([2,4,8], n3(:,191), t3x8(:,:,109))
  call heltable([2,4,8], n3(:,192), t3x8(:,:,110))
  n2(37) = 8
  call heltable([2,8,16], n3(:,193), t3x16(:,:,53))
  call heltable([8,2,16], n3(:,194), t3x16(:,:,54))
  call heltable([4,4,16], n3(:,195), t3x16(:,:,55))
  n2(38) = 8
  call heltable([4,4,16], n3(:,196), t3x16(:,:,56))
  call heltable([8,2,16], n3(:,197), t3x16(:,:,57))
  call heltable([1,8,8], n3(:,198), t3x8(:,:,111))
  call heltable([2,4,8], n3(:,199), t3x8(:,:,112))
  call heltable([4,4,16], n3(:,200), t3x16(:,:,58))
  call heltable([4,4,16], n3(:,201), t3x16(:,:,59))
  call heltable([1,8,8], n3(:,202), t3x8(:,:,113))
  call heltable([8,2,16], n3(:,203), t3x16(:,:,60))
  call heltable([4,2,8], n3(:,204), t3x8(:,:,114))
  call heltable([4,4,16], n3(:,205), t3x16(:,:,61))
  call heltable([4,4,16], n3(:,206), t3x16(:,:,62))
  call heltable([8,2,16], n3(:,207), t3x16(:,:,63))
  call heltable([2,8,16], n3(:,208), t3x16(:,:,64))
  call heltable([4,2,8], n3(:,209), t3x8(:,:,115))
  call heltable([2,8,16], n3(:,210), t3x16(:,:,65))
  call heltable([2,4,8], n3(:,211), t3x8(:,:,116))
  call heltable([2,8,16], n3(:,212), t3x16(:,:,66))
  call heltable([1,8,8], n3(:,213), t3x8(:,:,117))
  call heltable([1,8,8], n3(:,214), t3x8(:,:,118))
  call heltable([2,8,16], n3(:,215), t3x16(:,:,67))
  call heltable([2,8,16], n3(:,216), t3x16(:,:,68))
  call heltable([4,2,8], n3(:,217), t3x8(:,:,119))
  call heltable([2,4,8], n3(:,218), t3x8(:,:,120))
  call heltable([2,8,16], n3(:,219), t3x16(:,:,69))
  call heltable([8,2,16], n3(:,220), t3x16(:,:,70))
  call heltable([8,2,16], n3(:,221), t3x16(:,:,71))
  call heltable([8,2,16], n3(:,222), t3x16(:,:,72))
  call heltable([8,2,16], n3(:,223), t3x16(:,:,73))
  call heltable([8,2,16], n3(:,224), t3x16(:,:,74))
  call heltable([2,8,16], n3(:,225), t3x16(:,:,75))
  call heltable([8,2,16], n3(:,226), t3x16(:,:,76))
  call heltable([8,2,16], n3(:,227), t3x16(:,:,77))
  call heltable([2,8,16], n3(:,228), t3x16(:,:,78))
  call heltable([8,2,16], n3(:,229), t3x16(:,:,79))
  call heltable([8,2,16], n3(:,230), t3x16(:,:,80))
  call heltable([8,2,16], n3(:,231), t3x16(:,:,81))
  call heltable([8,2,16], n3(:,232), t3x16(:,:,82))
  call heltable([2,8,16], n3(:,233), t3x16(:,:,83))
  call heltable([8,2,16], n3(:,234), t3x16(:,:,84))
  call heltable([8,2,16], n3(:,235), t3x16(:,:,85))
  call heltable([2,8,16], n3(:,236), t3x16(:,:,86))
  call heltable([4,16,64], n3(:,237), t3x64(:,:,1))
  call heltable([4,16,64], n3(:,238), t3x64(:,:,2))
  call heltable([4,16,64], n3(:,239), t3x64(:,:,3))
  call heltable([4,16,64], n3(:,240), t3x64(:,:,4))
  call heltable([4,16,64], n3(:,241), t3x64(:,:,5))
  call heltable([4,16,64], n3(:,242), t3x64(:,:,6))
  call heltable([2,32,64], n3(:,243), t3x64(:,:,7))
  call heltable([2,32,64], n3(:,244), t3x64(:,:,8))
  call heltable([2,32,64], n3(:,245), t3x64(:,:,9))
  call heltable([2,32,64], n3(:,246), t3x64(:,:,10))
  call heltable([2,32,64], n3(:,247), t3x64(:,:,11))
  call heltable([2,32,64], n3(:,248), t3x64(:,:,12))
  call heltable([4,16,64], n3(:,249), t3x64(:,:,13))
  call heltable([4,16,64], n3(:,250), t3x64(:,:,14))
  call heltable([4,16,64], n3(:,251), t3x64(:,:,15))
  call heltable([4,16,64], n3(:,252), t3x64(:,:,16))
  call heltable([4,16,64], n3(:,253), t3x64(:,:,17))
  call heltable([4,16,64], n3(:,254), t3x64(:,:,18))
  call heltable([4,16,64], n3(:,255), t3x64(:,:,19))
  call heltable([8,8,64], n3(:,256), t3x64(:,:,20))
  call heltable([8,8,64], n3(:,257), t3x64(:,:,21))
  call heltable([8,8,64], n3(:,258), t3x64(:,:,22))
  call heltable([8,8,64], n3(:,259), t3x64(:,:,23))
  call heltable([4,16,64], n3(:,260), t3x64(:,:,24))
  call heltable([8,8,64], n3(:,261), t3x64(:,:,25))
  call heltable([8,8,64], n3(:,262), t3x64(:,:,26))
  call heltable([8,8,64], n3(:,263), t3x64(:,:,27))
  call heltable([8,8,64], n3(:,264), t3x64(:,:,28))
  call heltable([8,8,64], n3(:,265), t3x64(:,:,29))
  call heltable([8,8,64], n3(:,266), t3x64(:,:,30))
  call heltable([2,32,64], n3(:,267), t3x64(:,:,31))
  call heltable([2,32,64], n3(:,268), t3x64(:,:,32))
  call heltable([2,32,64], n3(:,269), t3x64(:,:,33))
  call heltable([2,32,64], n3(:,270), t3x64(:,:,34))
  call heltable([2,32,64], n3(:,271), t3x64(:,:,35))
  call heltable([2,32,64], n3(:,272), t3x64(:,:,36))
  call heltable([4,16,64], n3(:,273), t3x64(:,:,37))
  call heltable([4,16,64], n3(:,274), t3x64(:,:,38))
  call heltable([4,16,64], n3(:,275), t3x64(:,:,39))
  call heltable([4,16,64], n3(:,276), t3x64(:,:,40))
  call heltable([4,16,64], n3(:,277), t3x64(:,:,41))
  call heltable([4,16,64], n3(:,278), t3x64(:,:,42))
  call heltable([4,16,64], n3(:,279), t3x64(:,:,43))
  call heltable([8,8,64], n3(:,280), t3x64(:,:,44))
  call heltable([8,8,64], n3(:,281), t3x64(:,:,45))
  call heltable([8,8,64], n3(:,282), t3x64(:,:,46))
  call heltable([8,8,64], n3(:,283), t3x64(:,:,47))
  call heltable([4,16,64], n3(:,284), t3x64(:,:,48))
  call heltable([8,8,64], n3(:,285), t3x64(:,:,49))
  call heltable([8,8,64], n3(:,286), t3x64(:,:,50))
  call heltable([8,8,64], n3(:,287), t3x64(:,:,51))
  call heltable([8,8,64], n3(:,288), t3x64(:,:,52))
  call heltable([8,8,64], n3(:,289), t3x64(:,:,53))
  call heltable([8,8,64], n3(:,290), t3x64(:,:,54))
  call heltable([8,8,64], n3(:,291), t3x64(:,:,55))
  call heltable([8,8,64], n3(:,292), t3x64(:,:,56))
  call heltable([8,8,64], n3(:,293), t3x64(:,:,57))
  call heltable([8,8,64], n3(:,294), t3x64(:,:,58))
  call heltable([8,8,64], n3(:,295), t3x64(:,:,59))
  call heltable([8,8,64], n3(:,296), t3x64(:,:,60))
  call heltable([8,8,64], n3(:,297), t3x64(:,:,61))
  call heltable([8,8,64], n3(:,298), t3x64(:,:,62))
  call heltable([8,8,64], n3(:,299), t3x64(:,:,63))
  call heltable([8,8,64], n3(:,300), t3x64(:,:,64))
  call heltable([8,8,64], n3(:,301), t3x64(:,:,65))
  call heltable([8,8,64], n3(:,302), t3x64(:,:,66))
  call heltable([8,8,64], n3(:,303), t3x64(:,:,67))
  call heltable([8,8,64], n3(:,304), t3x64(:,:,68))
  call heltable([16,4,64], n3(:,305), t3x64(:,:,69))
  call heltable([8,8,64], n3(:,306), t3x64(:,:,70))
  call heltable([8,8,64], n3(:,307), t3x64(:,:,71))
  call heltable([16,4,64], n3(:,308), t3x64(:,:,72))
  call heltable([4,16,64], n3(:,309), t3x64(:,:,73))
  call heltable([4,16,64], n3(:,310), t3x64(:,:,74))
  call heltable([16,4,64], n3(:,311), t3x64(:,:,75))
  call heltable([8,8,64], n3(:,312), t3x64(:,:,76))
  call heltable([8,8,64], n3(:,313), t3x64(:,:,77))
  call heltable([8,8,64], n3(:,314), t3x64(:,:,78))
  call heltable([8,8,64], n3(:,315), t3x64(:,:,79))
  call heltable([8,8,64], n3(:,316), t3x64(:,:,80))
  call heltable([8,8,64], n3(:,317), t3x64(:,:,81))
  call heltable([4,16,64], n3(:,318), t3x64(:,:,82))
  call heltable([8,8,64], n3(:,319), t3x64(:,:,83))
  call heltable([8,8,64], n3(:,320), t3x64(:,:,84))
  call heltable([16,4,64], n3(:,321), t3x64(:,:,85))
  call heltable([4,16,64], n3(:,322), t3x64(:,:,86))
  call heltable([8,8,64], n3(:,323), t3x64(:,:,87))
  call heltable([8,8,64], n3(:,324), t3x64(:,:,88))
  call heltable([4,16,64], n3(:,325), t3x64(:,:,89))
  call heltable([8,8,64], n3(:,326), t3x64(:,:,90))
  call heltable([8,8,64], n3(:,327), t3x64(:,:,91))
  call heltable([16,4,64], n3(:,328), t3x64(:,:,92))
  call heltable([4,16,64], n3(:,329), t3x64(:,:,93))
  call heltable([8,8,64], n3(:,330), t3x64(:,:,94))
  call heltable([8,8,64], n3(:,331), t3x64(:,:,95))
  call heltable([8,8,64], n3(:,332), t3x64(:,:,96))
  call heltable([8,8,64], n3(:,333), t3x64(:,:,97))
  call heltable([16,4,64], n3(:,334), t3x64(:,:,98))
  call heltable([8,8,64], n3(:,335), t3x64(:,:,99))
  call heltable([8,8,64], n3(:,336), t3x64(:,:,100))
  call heltable([16,4,64], n3(:,337), t3x64(:,:,101))
  call heltable([4,16,64], n3(:,338), t3x64(:,:,102))
  call heltable([4,16,64], n3(:,339), t3x64(:,:,103))
  call heltable([4,16,64], n3(:,340), t3x64(:,:,104))
  call heltable([8,8,64], n3(:,341), t3x64(:,:,105))
  call heltable([8,8,64], n3(:,342), t3x64(:,:,106))
  call heltable([8,8,64], n3(:,343), t3x64(:,:,107))
  call heltable([4,16,64], n3(:,344), t3x64(:,:,108))
  call heltable([8,8,64], n3(:,345), t3x64(:,:,109))
  call heltable([8,8,64], n3(:,346), t3x64(:,:,110))
  call heltable([4,16,64], n3(:,347), t3x64(:,:,111))
  call heltable([8,8,64], n3(:,348), t3x64(:,:,112))
  call heltable([8,8,64], n3(:,349), t3x64(:,:,113))
  call heltable([4,16,64], n3(:,350), t3x64(:,:,114))
  call heltable([4,16,64], n3(:,351), t3x64(:,:,115))
  call heltable([4,16,64], n3(:,352), t3x64(:,:,116))
  call heltable([8,8,64], n3(:,353), t3x64(:,:,117))
  call heltable([8,8,64], n3(:,354), t3x64(:,:,118))
  call heltable([8,8,64], n3(:,355), t3x64(:,:,119))
  call heltable([4,16,64], n3(:,356), t3x64(:,:,120))
  call heltable([4,16,64], n3(:,357), t3x64(:,:,121))
  call heltable([4,16,64], n3(:,358), t3x64(:,:,122))
  call heltable([4,16,64], n3(:,359), t3x64(:,:,123))
  call heltable([8,8,64], n3(:,360), t3x64(:,:,124))
  call heltable([8,8,64], n3(:,361), t3x64(:,:,125))
  call heltable([16,4,64], n3(:,362), t3x64(:,:,126))
  call heltable([8,8,64], n3(:,363), t3x64(:,:,127))
  call heltable([8,8,64], n3(:,364), t3x64(:,:,128))
  call heltable([16,4,64], n3(:,365), t3x64(:,:,129))
  call heltable([4,16,64], n3(:,366), t3x64(:,:,130))
  call heltable([4,16,64], n3(:,367), t3x64(:,:,131))
  call heltable([16,4,64], n3(:,368), t3x64(:,:,132))
  call heltable([8,8,64], n3(:,369), t3x64(:,:,133))
  call heltable([8,8,64], n3(:,370), t3x64(:,:,134))
  call heltable([8,8,64], n3(:,371), t3x64(:,:,135))
  call heltable([8,8,64], n3(:,372), t3x64(:,:,136))
  call heltable([8,8,64], n3(:,373), t3x64(:,:,137))
  call heltable([8,8,64], n3(:,374), t3x64(:,:,138))
  call heltable([4,16,64], n3(:,375), t3x64(:,:,139))
  call heltable([8,8,64], n3(:,376), t3x64(:,:,140))
  call heltable([8,8,64], n3(:,377), t3x64(:,:,141))
  call heltable([4,16,64], n3(:,378), t3x64(:,:,142))
  call heltable([4,16,64], n3(:,379), t3x64(:,:,143))
  call heltable([8,8,64], n3(:,380), t3x64(:,:,144))
  call heltable([8,8,64], n3(:,381), t3x64(:,:,145))
  call heltable([4,16,64], n3(:,382), t3x64(:,:,146))
  call heltable([8,8,64], n3(:,383), t3x64(:,:,147))
  call heltable([8,8,64], n3(:,384), t3x64(:,:,148))
  call heltable([16,4,64], n3(:,385), t3x64(:,:,149))
  call heltable([8,8,64], n3(:,386), t3x64(:,:,150))
  call heltable([4,16,64], n3(:,387), t3x64(:,:,151))
  call heltable([8,8,64], n3(:,388), t3x64(:,:,152))
  call heltable([8,8,64], n3(:,389), t3x64(:,:,153))
  call heltable([8,8,64], n3(:,390), t3x64(:,:,154))
  call heltable([4,16,64], n3(:,391), t3x64(:,:,155))
  call heltable([8,8,64], n3(:,392), t3x64(:,:,156))
  call heltable([8,8,64], n3(:,393), t3x64(:,:,157))
  call heltable([16,4,64], n3(:,394), t3x64(:,:,158))
  call heltable([4,16,64], n3(:,395), t3x64(:,:,159))
  call heltable([4,16,64], n3(:,396), t3x64(:,:,160))
  call heltable([8,8,64], n3(:,397), t3x64(:,:,161))
  call heltable([4,16,64], n3(:,398), t3x64(:,:,162))
  call heltable([8,8,64], n3(:,399), t3x64(:,:,163))
  call heltable([8,8,64], n3(:,400), t3x64(:,:,164))
  call heltable([4,16,64], n3(:,401), t3x64(:,:,165))
  call heltable([8,8,64], n3(:,402), t3x64(:,:,166))
  call heltable([8,8,64], n3(:,403), t3x64(:,:,167))
  call heltable([8,8,64], n3(:,404), t3x64(:,:,168))
  call heltable([4,16,64], n3(:,405), t3x64(:,:,169))
  call heltable([8,8,64], n3(:,406), t3x64(:,:,170))
  call heltable([4,16,64], n3(:,407), t3x64(:,:,171))
  call heltable([4,16,64], n3(:,408), t3x64(:,:,172))
  call heltable([8,8,64], n3(:,409), t3x64(:,:,173))
  call heltable([4,16,64], n3(:,410), t3x64(:,:,174))
  call heltable([8,8,64], n3(:,411), t3x64(:,:,175))
  call heltable([8,8,64], n3(:,412), t3x64(:,:,176))
  call heltable([4,16,64], n3(:,413), t3x64(:,:,177))
  call heltable([4,16,64], n3(:,414), t3x64(:,:,178))
  call heltable([4,16,64], n3(:,415), t3x64(:,:,179))
  call heltable([4,16,64], n3(:,416), t3x64(:,:,180))
  call heltable([4,16,64], n3(:,417), t3x64(:,:,181))
  call heltable([8,8,64], n3(:,418), t3x64(:,:,182))
  call heltable([8,8,64], n3(:,419), t3x64(:,:,183))
  call heltable([16,4,64], n3(:,420), t3x64(:,:,184))
  call heltable([4,16,64], n3(:,421), t3x64(:,:,185))
  call heltable([8,8,64], n3(:,422), t3x64(:,:,186))
  call heltable([8,8,64], n3(:,423), t3x64(:,:,187))
  call heltable([4,16,64], n3(:,424), t3x64(:,:,188))
  call heltable([8,8,64], n3(:,425), t3x64(:,:,189))
  call heltable([8,8,64], n3(:,426), t3x64(:,:,190))
  call heltable([4,16,64], n3(:,427), t3x64(:,:,191))
  call heltable([4,16,64], n3(:,428), t3x64(:,:,192))
  call heltable([8,8,64], n3(:,429), t3x64(:,:,193))
  call heltable([8,8,64], n3(:,430), t3x64(:,:,194))
  call heltable([8,8,64], n3(:,431), t3x64(:,:,195))
  call heltable([8,8,64], n3(:,432), t3x64(:,:,196))
  call heltable([16,4,64], n3(:,433), t3x64(:,:,197))
  call heltable([8,8,64], n3(:,434), t3x64(:,:,198))
  call heltable([8,8,64], n3(:,435), t3x64(:,:,199))
  call heltable([4,16,64], n3(:,436), t3x64(:,:,200))
  call heltable([4,16,64], n3(:,437), t3x64(:,:,201))
  call heltable([4,16,64], n3(:,438), t3x64(:,:,202))
  call heltable([4,16,64], n3(:,439), t3x64(:,:,203))
  call heltable([8,8,64], n3(:,440), t3x64(:,:,204))
  call heltable([8,8,64], n3(:,441), t3x64(:,:,205))
  call heltable([8,8,64], n3(:,442), t3x64(:,:,206))
  call heltable([4,16,64], n3(:,443), t3x64(:,:,207))
  call heltable([8,8,64], n3(:,444), t3x64(:,:,208))
  call heltable([8,8,64], n3(:,445), t3x64(:,:,209))
  call heltable([4,16,64], n3(:,446), t3x64(:,:,210))
  call heltable([8,8,64], n3(:,447), t3x64(:,:,211))
  call heltable([8,8,64], n3(:,448), t3x64(:,:,212))
  call heltable([4,16,64], n3(:,449), t3x64(:,:,213))
  call heltable([4,16,64], n3(:,450), t3x64(:,:,214))
  call heltable([4,16,64], n3(:,451), t3x64(:,:,215))
  call heltable([8,8,64], n3(:,452), t3x64(:,:,216))
  call heltable([8,8,64], n3(:,453), t3x64(:,:,217))
  call heltable([8,8,64], n3(:,454), t3x64(:,:,218))
  call heltable([4,16,64], n3(:,455), t3x64(:,:,219))
  call heltable([4,16,64], n3(:,456), t3x64(:,:,220))
  call heltable([4,16,64], n3(:,457), t3x64(:,:,221))
  call heltable([4,16,64], n3(:,458), t3x64(:,:,222))
  call heltable([4,16,64], n3(:,459), t3x64(:,:,223))
  call heltable([8,8,64], n3(:,460), t3x64(:,:,224))
  call heltable([8,8,64], n3(:,461), t3x64(:,:,225))
  call heltable([4,16,64], n3(:,462), t3x64(:,:,226))
  call heltable([4,16,64], n3(:,463), t3x64(:,:,227))
  call heltable([8,8,64], n3(:,464), t3x64(:,:,228))
  call heltable([8,8,64], n3(:,465), t3x64(:,:,229))
  call heltable([4,16,64], n3(:,466), t3x64(:,:,230))
  call heltable([8,8,64], n3(:,467), t3x64(:,:,231))
  call heltable([8,8,64], n3(:,468), t3x64(:,:,232))
  call heltable([4,16,64], n3(:,469), t3x64(:,:,233))
  call heltable([8,8,64], n3(:,470), t3x64(:,:,234))
  call heltable([4,16,64], n3(:,471), t3x64(:,:,235))
  call heltable([8,8,64], n3(:,472), t3x64(:,:,236))
  call heltable([8,8,64], n3(:,473), t3x64(:,:,237))
  call heltable([8,8,64], n3(:,474), t3x64(:,:,238))
  call heltable([4,16,64], n3(:,475), t3x64(:,:,239))
  call heltable([8,8,64], n3(:,476), t3x64(:,:,240))
  call heltable([8,8,64], n3(:,477), t3x64(:,:,241))
  call heltable([4,16,64], n3(:,478), t3x64(:,:,242))
  call heltable([4,16,64], n3(:,479), t3x64(:,:,243))
  call heltable([4,16,64], n3(:,480), t3x64(:,:,244))
  call heltable([8,8,64], n3(:,481), t3x64(:,:,245))
  call heltable([4,16,64], n3(:,482), t3x64(:,:,246))
  call heltable([8,8,64], n3(:,483), t3x64(:,:,247))
  call heltable([8,8,64], n3(:,484), t3x64(:,:,248))
  call heltable([4,16,64], n3(:,485), t3x64(:,:,249))
  call heltable([8,8,64], n3(:,486), t3x64(:,:,250))
  call heltable([8,8,64], n3(:,487), t3x64(:,:,251))
  call heltable([8,8,64], n3(:,488), t3x64(:,:,252))
  call heltable([4,16,64], n3(:,489), t3x64(:,:,253))
  call heltable([8,8,64], n3(:,490), t3x64(:,:,254))
  call heltable([4,16,64], n3(:,491), t3x64(:,:,255))
  call heltable([4,16,64], n3(:,492), t3x64(:,:,256))
  call heltable([8,8,64], n3(:,493), t3x64(:,:,257))
  call heltable([4,16,64], n3(:,494), t3x64(:,:,258))
  call heltable([8,8,64], n3(:,495), t3x64(:,:,259))
  call heltable([8,8,64], n3(:,496), t3x64(:,:,260))
  call heltable([4,16,64], n3(:,497), t3x64(:,:,261))
  call heltable([4,16,64], n3(:,498), t3x64(:,:,262))
  call heltable([4,16,64], n3(:,499), t3x64(:,:,263))
  call heltable([4,16,64], n3(:,500), t3x64(:,:,264))
  call heltable([8,8,64], n3(:,501), t3x64(:,:,265))
  call heltable([8,8,64], n3(:,502), t3x64(:,:,266))
  call heltable([8,8,64], n3(:,503), t3x64(:,:,267))
  call heltable([8,8,64], n3(:,504), t3x64(:,:,268))
  call heltable([4,16,64], n3(:,505), t3x64(:,:,269))
  call heltable([8,8,64], n3(:,506), t3x64(:,:,270))
  call heltable([8,8,64], n3(:,507), t3x64(:,:,271))
  call heltable([8,8,64], n3(:,508), t3x64(:,:,272))
  call heltable([8,8,64], n3(:,509), t3x64(:,:,273))
  call heltable([8,8,64], n3(:,510), t3x64(:,:,274))
  call heltable([8,8,64], n3(:,511), t3x64(:,:,275))
  call heltable([8,8,64], n3(:,512), t3x64(:,:,276))
  call heltable([4,16,64], n3(:,513), t3x64(:,:,277))
  call heltable([8,8,64], n3(:,514), t3x64(:,:,278))
  call heltable([8,8,64], n3(:,515), t3x64(:,:,279))
  call heltable([8,8,64], n3(:,516), t3x64(:,:,280))
  call heltable([8,8,64], n3(:,517), t3x64(:,:,281))
  call heltable([8,8,64], n3(:,518), t3x64(:,:,282))
  call heltable([8,8,64], n3(:,519), t3x64(:,:,283))
  call heltable([8,8,64], n3(:,520), t3x64(:,:,284))
  call heltable([4,16,64], n3(:,521), t3x64(:,:,285))
  call heltable([4,16,64], n3(:,522), t3x64(:,:,286))
  call heltable([4,16,64], n3(:,523), t3x64(:,:,287))
  call heltable([8,8,64], n3(:,524), t3x64(:,:,288))
  call heltable([8,8,64], n3(:,525), t3x64(:,:,289))
  call heltable([8,8,64], n3(:,526), t3x64(:,:,290))
  call heltable([8,8,64], n3(:,527), t3x64(:,:,291))
  call heltable([8,8,64], n3(:,528), t3x64(:,:,292))
  call heltable([4,16,64], n3(:,529), t3x64(:,:,293))
  call heltable([4,16,64], n3(:,530), t3x64(:,:,294))
  call heltable([4,16,64], n3(:,531), t3x64(:,:,295))
  call heltable([8,8,64], n3(:,532), t3x64(:,:,296))
  call heltable([4,16,64], n3(:,533), t3x64(:,:,297))
  call heltable([8,8,64], n3(:,534), t3x64(:,:,298))
  call heltable([8,8,64], n3(:,535), t3x64(:,:,299))
  call heltable([8,8,64], n3(:,536), t3x64(:,:,300))
  call heltable([4,16,64], n3(:,537), t3x64(:,:,301))
  call heltable([8,8,64], n3(:,538), t3x64(:,:,302))
  call heltable([8,8,64], n3(:,539), t3x64(:,:,303))
  call heltable([8,8,64], n3(:,540), t3x64(:,:,304))
  call heltable([4,16,64], n3(:,541), t3x64(:,:,305))
  call heltable([4,16,64], n3(:,542), t3x64(:,:,306))
  call heltable([4,16,64], n3(:,543), t3x64(:,:,307))
  call heltable([8,8,64], n3(:,544), t3x64(:,:,308))
  call heltable([4,16,64], n3(:,545), t3x64(:,:,309))
  call heltable([4,16,64], n3(:,546), t3x64(:,:,310))
  call heltable([4,16,64], n3(:,547), t3x64(:,:,311))
  call heltable([8,8,64], n3(:,548), t3x64(:,:,312))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_heftpphjjj_dddxdxhgg_1
