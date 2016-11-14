
module ol_external_pphhjj_uuxbbxhh_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_pphhjj_uuxbbxhh_1(6) = &
                     [ (dummy_counter, dummy_counter = 1, 6) ]
  integer, save :: external_perm_inv_pphhjj_uuxbbxhh_1(6) = &
                     [ (dummy_counter, dummy_counter = 1, 6) ]
  integer, save :: extcomb_perm_pphhjj_uuxbbxhh_1(0:22) = &
                     [ (dummy_counter, dummy_counter = 0, 22) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_pphhjj_uuxbbxhh_1(6) = &
                     [ 1, 2, 3, 4, 5, 5 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_pphhjj_uuxbbxhh_1(6) = &
                     [ 6, 6, 6, 6, 1, 1 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_pphhjj_uuxbbxhh_1 = &
                     72
  integer, save :: channel_number_pphhjj_uuxbbxhh_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(6,16) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(16,6)
  integer, save :: POLSEL(6) = 0

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_pphhjj_uuxbbxhh_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 6
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_pphhjj_uuxbbxhh_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 6
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_pphhjj_uuxbbxhh_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_external_decl_/**/DREALKIND, only: n_scatt
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(6)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_pphhjj_uuxbbxhh_1(6)
    external_perm_pphhjj_uuxbbxhh_1 = perm
    do i = 1, 6
      external_perm_inv_pphhjj_uuxbbxhh_1( &
        external_perm_pphhjj_uuxbbxhh_1(i)) = i
      particle_types_perm_pphhjj_uuxbbxhh_1(i) = &
        particle_types_pphhjj_uuxbbxhh_1( &
        external_perm_pphhjj_uuxbbxhh_1(i))
    end do
    do i = 1, 6
      do j = 1, i
        if (external_perm_pphhjj_uuxbbxhh_1(i) >= &
          external_perm_pphhjj_uuxbbxhh_1(j)) then
          ii = external_perm_pphhjj_uuxbbxhh_1(i)
          jj = external_perm_pphhjj_uuxbbxhh_1(j)
        else
          ii = external_perm_pphhjj_uuxbbxhh_1(j)
          jj = external_perm_pphhjj_uuxbbxhh_1(i)
        end if
        extcomb_perm_pphhjj_uuxbbxhh_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_pphhjj_uuxbbxhh_1 = 1
    do i = 1, n_scatt
      average_factor_pphhjj_uuxbbxhh_1 = &
        average_factor_pphhjj_uuxbbxhh_1 &
        * average_factors_pphhjj_uuxbbxhh_1( &
        external_perm_pphhjj_uuxbbxhh_1(i))
    end do
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 6
        average_factor_pphhjj_uuxbbxhh_1 = &
          average_factor_pphhjj_uuxbbxhh_1 &
          * factorial(count(particle_types_perm_pphhjj_uuxbbxhh_1(n_scatt+1:6) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_pphhjj_uuxbbxhh_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(6)
    integer :: f_perm(6)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_pphhjj_uuxbbxhh_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(6)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(6)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rMB_unscaled, rMB_unscaled, rMH_unscaled, rMH_unscaled ]
    do i = 1, 6
      m_ex(i) = m_ex_orig(external_perm_pphhjj_uuxbbxhh_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_pphhjj_uuxbbxhh_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(6)
    real(DREALKIND) :: f_m_ex(6)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_pphhjj_uuxbbxhh_1")
    use KIND_TYPES, only: DREALKIND
    use ol_kinematics_/**/DREALKIND, only: rambo_generic => rambo
    implicit none
    real(DREALKIND), intent(in) :: sqrt_s
    real(DREALKIND), intent(out) :: p_rambo(0:3,6)
    real(DREALKIND) :: m_ex(6)
    call get_masses(m_ex)
    call rambo_generic(sqrt_s, m_ex, p_rambo)
  end subroutine rambo


  subroutine rambo_c(sqrt_s, p_rambo) &
      & bind(c,name="ol_rambo_pphhjj_uuxbbxhh_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in) :: sqrt_s
    real(c_double), intent(out) :: p_rambo(0:3,6)
    real(DREALKIND) :: f_sqrt_s
    real(DREALKIND) :: f_p_rambo(0:3,6)
    f_sqrt_s = sqrt_s
    call rambo(f_sqrt_s, f_p_rambo)
    p_rambo = f_p_rambo
  end subroutine rambo_c


  subroutine hel_init
    implicit none
    integer :: binpos, flip, binco
    hel_not_initialised = .false.
    ! helicity configurations for this process
  H(:, 1) = [ -1, -1, -1, -1,  0,  0 ]
  H(:, 2) = [ -1, -1, -1,  1,  0,  0 ]
  H(:, 3) = [ -1, -1,  1, -1,  0,  0 ]
  H(:, 4) = [ -1, -1,  1,  1,  0,  0 ]
  H(:, 5) = [ -1,  1, -1, -1,  0,  0 ]
  H(:, 6) = [ -1,  1, -1,  1,  0,  0 ]
  H(:, 7) = [ -1,  1,  1, -1,  0,  0 ]
  H(:, 8) = [ -1,  1,  1,  1,  0,  0 ]
  H(:, 9) = [  1, -1, -1, -1,  0,  0 ]
  H(:,10) = [  1, -1, -1,  1,  0,  0 ]
  H(:,11) = [  1, -1,  1, -1,  0,  0 ]
  H(:,12) = [  1, -1,  1,  1,  0,  0 ]
  H(:,13) = [  1,  1, -1, -1,  0,  0 ]
  H(:,14) = [  1,  1, -1,  1,  0,  0 ]
  H(:,15) = [  1,  1,  1, -1,  0,  0 ]
  H(:,16) = [  1,  1,  1,  1,  0,  0 ]

  end subroutine hel_init


  subroutine pol_init(pol) &
      & bind(c,name="ol_f_pol_init_pphhjj_uuxbbxhh_1")
    implicit none
    integer, intent(in) :: pol(6)
    POLSEL = pol
  end subroutine pol_init

end module ol_external_pphhjj_uuxbbxhh_1


module colour_basis_pphhjj_uuxbbxhh_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(6) = [1,1,1,1,0,0]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_pphhjj_uuxbbxhh_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(6)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 1
    ncoupl = 1
    maxpows = 1
    nhel = 16
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_pphhjj_uuxbbxhh_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,1)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([8], [1,1])
#endif
#if 1 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [1,8,4], &
      [3,1])
#endif
  end subroutine tree_colbasis

end module colour_basis_pphhjj_uuxbbxhh_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_pphhjj_uuxbbxhh_1(perm)
  use ol_external_pphhjj_uuxbbxhh_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(6)
  call set_permutation(perm)
end subroutine set_permutation_pphhjj_uuxbbxhh_1
