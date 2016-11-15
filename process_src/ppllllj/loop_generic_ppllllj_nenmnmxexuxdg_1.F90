
module ol_external_ppllllj_nenmnmxexuxdg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppllllj_nenmnmxexuxdg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_ppllllj_nenmnmxexuxdg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_ppllllj_nenmnmxexuxdg_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppllllj_nenmnmxexuxdg_1(7) = &
                     [ 1, 2, 3, 4, 5, 6, 7 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppllllj_nenmnmxexuxdg_1(7) = &
                     [ 2, 2, 2, 2, 6, 6, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppllllj_nenmnmxexuxdg_1 = &
                     4
  integer, save :: channel_number_ppllllj_nenmnmxexuxdg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,128) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(128,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppllllj_nenmnmxexuxdg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppllllj_nenmnmxexuxdg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppllllj_nenmnmxexuxdg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppllllj_nenmnmxexuxdg_1(7)
    external_perm_ppllllj_nenmnmxexuxdg_1 = perm
    do i = 1, 7
      external_perm_inv_ppllllj_nenmnmxexuxdg_1( &
        external_perm_ppllllj_nenmnmxexuxdg_1(i)) = i
      particle_types_perm_ppllllj_nenmnmxexuxdg_1(i) = &
        particle_types_ppllllj_nenmnmxexuxdg_1( &
        external_perm_ppllllj_nenmnmxexuxdg_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_ppllllj_nenmnmxexuxdg_1(i) >= &
          external_perm_ppllllj_nenmnmxexuxdg_1(j)) then
          ii = external_perm_ppllllj_nenmnmxexuxdg_1(i)
          jj = external_perm_ppllllj_nenmnmxexuxdg_1(j)
        else
          ii = external_perm_ppllllj_nenmnmxexuxdg_1(j)
          jj = external_perm_ppllllj_nenmnmxexuxdg_1(i)
        end if
        extcomb_perm_ppllllj_nenmnmxexuxdg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppllllj_nenmnmxexuxdg_1 = &
      average_factors_ppllllj_nenmnmxexuxdg_1( &
      external_perm_ppllllj_nenmnmxexuxdg_1(1)) &
      * average_factors_ppllllj_nenmnmxexuxdg_1( &
      external_perm_ppllllj_nenmnmxexuxdg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_ppllllj_nenmnmxexuxdg_1 = &
          average_factor_ppllllj_nenmnmxexuxdg_1 &
          * factorial(count(particle_types_perm_ppllllj_nenmnmxexuxdg_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppllllj_nenmnmxexuxdg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppllllj_nenmnmxexuxdg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rZERO, rZERO, rZERO ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_ppllllj_nenmnmxexuxdg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppllllj_nenmnmxexuxdg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppllllj_nenmnmxexuxdg_1")
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
      & bind(c,name="ol_rambo_ppllllj_nenmnmxexuxdg_1")
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
  H(:,  1) = [ -1, -1, -1, -1, -1, -1, -1 ]
  H(:,  2) = [ -1, -1, -1, -1, -1, -1,  1 ]
  H(:,  3) = [ -1, -1, -1, -1, -1,  1, -1 ]
  H(:,  4) = [ -1, -1, -1, -1, -1,  1,  1 ]
  H(:,  5) = [ -1, -1, -1, -1,  1, -1, -1 ]
  H(:,  6) = [ -1, -1, -1, -1,  1, -1,  1 ]
  H(:,  7) = [ -1, -1, -1, -1,  1,  1, -1 ]
  H(:,  8) = [ -1, -1, -1, -1,  1,  1,  1 ]
  H(:,  9) = [ -1, -1, -1,  1, -1, -1, -1 ]
  H(:, 10) = [ -1, -1, -1,  1, -1, -1,  1 ]
  H(:, 11) = [ -1, -1, -1,  1, -1,  1, -1 ]
  H(:, 12) = [ -1, -1, -1,  1, -1,  1,  1 ]
  H(:, 13) = [ -1, -1, -1,  1,  1, -1, -1 ]
  H(:, 14) = [ -1, -1, -1,  1,  1, -1,  1 ]
  H(:, 15) = [ -1, -1, -1,  1,  1,  1, -1 ]
  H(:, 16) = [ -1, -1, -1,  1,  1,  1,  1 ]
  H(:, 17) = [ -1, -1,  1, -1, -1, -1, -1 ]
  H(:, 18) = [ -1, -1,  1, -1, -1, -1,  1 ]
  H(:, 19) = [ -1, -1,  1, -1, -1,  1, -1 ]
  H(:, 20) = [ -1, -1,  1, -1, -1,  1,  1 ]
  H(:, 21) = [ -1, -1,  1, -1,  1, -1, -1 ]
  H(:, 22) = [ -1, -1,  1, -1,  1, -1,  1 ]
  H(:, 23) = [ -1, -1,  1, -1,  1,  1, -1 ]
  H(:, 24) = [ -1, -1,  1, -1,  1,  1,  1 ]
  H(:, 25) = [ -1, -1,  1,  1, -1, -1, -1 ]
  H(:, 26) = [ -1, -1,  1,  1, -1, -1,  1 ]
  H(:, 27) = [ -1, -1,  1,  1, -1,  1, -1 ]
  H(:, 28) = [ -1, -1,  1,  1, -1,  1,  1 ]
  H(:, 29) = [ -1, -1,  1,  1,  1, -1, -1 ]
  H(:, 30) = [ -1, -1,  1,  1,  1, -1,  1 ]
  H(:, 31) = [ -1, -1,  1,  1,  1,  1, -1 ]
  H(:, 32) = [ -1, -1,  1,  1,  1,  1,  1 ]
  H(:, 33) = [ -1,  1, -1, -1, -1, -1, -1 ]
  H(:, 34) = [ -1,  1, -1, -1, -1, -1,  1 ]
  H(:, 35) = [ -1,  1, -1, -1, -1,  1, -1 ]
  H(:, 36) = [ -1,  1, -1, -1, -1,  1,  1 ]
  H(:, 37) = [ -1,  1, -1, -1,  1, -1, -1 ]
  H(:, 38) = [ -1,  1, -1, -1,  1, -1,  1 ]
  H(:, 39) = [ -1,  1, -1, -1,  1,  1, -1 ]
  H(:, 40) = [ -1,  1, -1, -1,  1,  1,  1 ]
  H(:, 41) = [ -1,  1, -1,  1, -1, -1, -1 ]
  H(:, 42) = [ -1,  1, -1,  1, -1, -1,  1 ]
  H(:, 43) = [ -1,  1, -1,  1, -1,  1, -1 ]
  H(:, 44) = [ -1,  1, -1,  1, -1,  1,  1 ]
  H(:, 45) = [ -1,  1, -1,  1,  1, -1, -1 ]
  H(:, 46) = [ -1,  1, -1,  1,  1, -1,  1 ]
  H(:, 47) = [ -1,  1, -1,  1,  1,  1, -1 ]
  H(:, 48) = [ -1,  1, -1,  1,  1,  1,  1 ]
  H(:, 49) = [ -1,  1,  1, -1, -1, -1, -1 ]
  H(:, 50) = [ -1,  1,  1, -1, -1, -1,  1 ]
  H(:, 51) = [ -1,  1,  1, -1, -1,  1, -1 ]
  H(:, 52) = [ -1,  1,  1, -1, -1,  1,  1 ]
  H(:, 53) = [ -1,  1,  1, -1,  1, -1, -1 ]
  H(:, 54) = [ -1,  1,  1, -1,  1, -1,  1 ]
  H(:, 55) = [ -1,  1,  1, -1,  1,  1, -1 ]
  H(:, 56) = [ -1,  1,  1, -1,  1,  1,  1 ]
  H(:, 57) = [ -1,  1,  1,  1, -1, -1, -1 ]
  H(:, 58) = [ -1,  1,  1,  1, -1, -1,  1 ]
  H(:, 59) = [ -1,  1,  1,  1, -1,  1, -1 ]
  H(:, 60) = [ -1,  1,  1,  1, -1,  1,  1 ]
  H(:, 61) = [ -1,  1,  1,  1,  1, -1, -1 ]
  H(:, 62) = [ -1,  1,  1,  1,  1, -1,  1 ]
  H(:, 63) = [ -1,  1,  1,  1,  1,  1, -1 ]
  H(:, 64) = [ -1,  1,  1,  1,  1,  1,  1 ]
  H(:, 65) = [  1, -1, -1, -1, -1, -1, -1 ]
  H(:, 66) = [  1, -1, -1, -1, -1, -1,  1 ]
  H(:, 67) = [  1, -1, -1, -1, -1,  1, -1 ]
  H(:, 68) = [  1, -1, -1, -1, -1,  1,  1 ]
  H(:, 69) = [  1, -1, -1, -1,  1, -1, -1 ]
  H(:, 70) = [  1, -1, -1, -1,  1, -1,  1 ]
  H(:, 71) = [  1, -1, -1, -1,  1,  1, -1 ]
  H(:, 72) = [  1, -1, -1, -1,  1,  1,  1 ]
  H(:, 73) = [  1, -1, -1,  1, -1, -1, -1 ]
  H(:, 74) = [  1, -1, -1,  1, -1, -1,  1 ]
  H(:, 75) = [  1, -1, -1,  1, -1,  1, -1 ]
  H(:, 76) = [  1, -1, -1,  1, -1,  1,  1 ]
  H(:, 77) = [  1, -1, -1,  1,  1, -1, -1 ]
  H(:, 78) = [  1, -1, -1,  1,  1, -1,  1 ]
  H(:, 79) = [  1, -1, -1,  1,  1,  1, -1 ]
  H(:, 80) = [  1, -1, -1,  1,  1,  1,  1 ]
  H(:, 81) = [  1, -1,  1, -1, -1, -1, -1 ]
  H(:, 82) = [  1, -1,  1, -1, -1, -1,  1 ]
  H(:, 83) = [  1, -1,  1, -1, -1,  1, -1 ]
  H(:, 84) = [  1, -1,  1, -1, -1,  1,  1 ]
  H(:, 85) = [  1, -1,  1, -1,  1, -1, -1 ]
  H(:, 86) = [  1, -1,  1, -1,  1, -1,  1 ]
  H(:, 87) = [  1, -1,  1, -1,  1,  1, -1 ]
  H(:, 88) = [  1, -1,  1, -1,  1,  1,  1 ]
  H(:, 89) = [  1, -1,  1,  1, -1, -1, -1 ]
  H(:, 90) = [  1, -1,  1,  1, -1, -1,  1 ]
  H(:, 91) = [  1, -1,  1,  1, -1,  1, -1 ]
  H(:, 92) = [  1, -1,  1,  1, -1,  1,  1 ]
  H(:, 93) = [  1, -1,  1,  1,  1, -1, -1 ]
  H(:, 94) = [  1, -1,  1,  1,  1, -1,  1 ]
  H(:, 95) = [  1, -1,  1,  1,  1,  1, -1 ]
  H(:, 96) = [  1, -1,  1,  1,  1,  1,  1 ]
  H(:, 97) = [  1,  1, -1, -1, -1, -1, -1 ]
  H(:, 98) = [  1,  1, -1, -1, -1, -1,  1 ]
  H(:, 99) = [  1,  1, -1, -1, -1,  1, -1 ]
  H(:,100) = [  1,  1, -1, -1, -1,  1,  1 ]
  H(:,101) = [  1,  1, -1, -1,  1, -1, -1 ]
  H(:,102) = [  1,  1, -1, -1,  1, -1,  1 ]
  H(:,103) = [  1,  1, -1, -1,  1,  1, -1 ]
  H(:,104) = [  1,  1, -1, -1,  1,  1,  1 ]
  H(:,105) = [  1,  1, -1,  1, -1, -1, -1 ]
  H(:,106) = [  1,  1, -1,  1, -1, -1,  1 ]
  H(:,107) = [  1,  1, -1,  1, -1,  1, -1 ]
  H(:,108) = [  1,  1, -1,  1, -1,  1,  1 ]
  H(:,109) = [  1,  1, -1,  1,  1, -1, -1 ]
  H(:,110) = [  1,  1, -1,  1,  1, -1,  1 ]
  H(:,111) = [  1,  1, -1,  1,  1,  1, -1 ]
  H(:,112) = [  1,  1, -1,  1,  1,  1,  1 ]
  H(:,113) = [  1,  1,  1, -1, -1, -1, -1 ]
  H(:,114) = [  1,  1,  1, -1, -1, -1,  1 ]
  H(:,115) = [  1,  1,  1, -1, -1,  1, -1 ]
  H(:,116) = [  1,  1,  1, -1, -1,  1,  1 ]
  H(:,117) = [  1,  1,  1, -1,  1, -1, -1 ]
  H(:,118) = [  1,  1,  1, -1,  1, -1,  1 ]
  H(:,119) = [  1,  1,  1, -1,  1,  1, -1 ]
  H(:,120) = [  1,  1,  1, -1,  1,  1,  1 ]
  H(:,121) = [  1,  1,  1,  1, -1, -1, -1 ]
  H(:,122) = [  1,  1,  1,  1, -1, -1,  1 ]
  H(:,123) = [  1,  1,  1,  1, -1,  1, -1 ]
  H(:,124) = [  1,  1,  1,  1, -1,  1,  1 ]
  H(:,125) = [  1,  1,  1,  1,  1, -1, -1 ]
  H(:,126) = [  1,  1,  1,  1,  1, -1,  1 ]
  H(:,127) = [  1,  1,  1,  1,  1,  1, -1 ]
  H(:,128) = [  1,  1,  1,  1,  1,  1,  1 ]

  H_HC(:,7) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 128/1/2) ]
  end subroutine hel_init

end module ol_external_ppllllj_nenmnmxexuxdg_1


module colour_basis_ppllllj_nenmnmxexuxdg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [0,0,0,0,1,1,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppllllj_nenmnmxexuxdg_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(7)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 1
    ncoupl = 1
    maxpows = 1
    nhel = 128
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_ppllllj_nenmnmxexuxdg_1")
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
      [1,5,4], &
      [3,1])
#endif
  end subroutine tree_colbasis

end module colour_basis_ppllllj_nenmnmxexuxdg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppllllj_nenmnmxexuxdg_1(perm)
  use ol_external_ppllllj_nenmnmxexuxdg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_ppllllj_nenmnmxexuxdg_1
