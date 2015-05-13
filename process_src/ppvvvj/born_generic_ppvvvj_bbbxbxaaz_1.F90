
module ol_external_ppvvvj_bbbxbxaaz_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppvvvj_bbbxbxaaz_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_ppvvvj_bbbxbxaaz_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_ppvvvj_bbbxbxaaz_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppvvvj_bbbxbxaaz_1(7) = &
                     [ 1, 1, 2, 2, 3, 3, 4 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppvvvj_bbbxbxaaz_1(7) = &
                     [ 6, 6, 6, 6, 2, 2, 3 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppvvvj_bbbxbxaaz_1 = &
                     144
  integer, save :: channel_number_ppvvvj_bbbxbxaaz_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,192) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(192,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppvvvj_bbbxbxaaz_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppvvvj_bbbxbxaaz_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppvvvj_bbbxbxaaz_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppvvvj_bbbxbxaaz_1(7)
    external_perm_ppvvvj_bbbxbxaaz_1 = perm
    do i = 1, 7
      external_perm_inv_ppvvvj_bbbxbxaaz_1( &
        external_perm_ppvvvj_bbbxbxaaz_1(i)) = i
      particle_types_perm_ppvvvj_bbbxbxaaz_1(i) = &
        particle_types_ppvvvj_bbbxbxaaz_1( &
        external_perm_ppvvvj_bbbxbxaaz_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_ppvvvj_bbbxbxaaz_1(i) >= &
          external_perm_ppvvvj_bbbxbxaaz_1(j)) then
          ii = external_perm_ppvvvj_bbbxbxaaz_1(i)
          jj = external_perm_ppvvvj_bbbxbxaaz_1(j)
        else
          ii = external_perm_ppvvvj_bbbxbxaaz_1(j)
          jj = external_perm_ppvvvj_bbbxbxaaz_1(i)
        end if
        extcomb_perm_ppvvvj_bbbxbxaaz_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppvvvj_bbbxbxaaz_1 = &
      average_factors_ppvvvj_bbbxbxaaz_1( &
      external_perm_ppvvvj_bbbxbxaaz_1(1)) &
      * average_factors_ppvvvj_bbbxbxaaz_1( &
      external_perm_ppvvvj_bbbxbxaaz_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_ppvvvj_bbbxbxaaz_1 = &
          average_factor_ppvvvj_bbbxbxaaz_1 &
          * factorial(count(particle_types_perm_ppvvvj_bbbxbxaaz_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppvvvj_bbbxbxaaz_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppvvvj_bbbxbxaaz_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rMB_unscaled, rMB_unscaled, rMB_unscaled, rMB_unscaled, rZERO, rZERO, rMZ_unscaled ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_ppvvvj_bbbxbxaaz_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppvvvj_bbbxbxaaz_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppvvvj_bbbxbxaaz_1")
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
      & bind(c,name="ol_rambo_ppvvvj_bbbxbxaaz_1")
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
  H(:,  2) = [ -1, -1, -1, -1, -1, -1,  0 ]
  H(:,  3) = [ -1, -1, -1, -1, -1, -1,  1 ]
  H(:,  4) = [ -1, -1, -1, -1, -1,  1, -1 ]
  H(:,  5) = [ -1, -1, -1, -1, -1,  1,  0 ]
  H(:,  6) = [ -1, -1, -1, -1, -1,  1,  1 ]
  H(:,  7) = [ -1, -1, -1, -1,  1, -1, -1 ]
  H(:,  8) = [ -1, -1, -1, -1,  1, -1,  0 ]
  H(:,  9) = [ -1, -1, -1, -1,  1, -1,  1 ]
  H(:, 10) = [ -1, -1, -1, -1,  1,  1, -1 ]
  H(:, 11) = [ -1, -1, -1, -1,  1,  1,  0 ]
  H(:, 12) = [ -1, -1, -1, -1,  1,  1,  1 ]
  H(:, 13) = [ -1, -1, -1,  1, -1, -1, -1 ]
  H(:, 14) = [ -1, -1, -1,  1, -1, -1,  0 ]
  H(:, 15) = [ -1, -1, -1,  1, -1, -1,  1 ]
  H(:, 16) = [ -1, -1, -1,  1, -1,  1, -1 ]
  H(:, 17) = [ -1, -1, -1,  1, -1,  1,  0 ]
  H(:, 18) = [ -1, -1, -1,  1, -1,  1,  1 ]
  H(:, 19) = [ -1, -1, -1,  1,  1, -1, -1 ]
  H(:, 20) = [ -1, -1, -1,  1,  1, -1,  0 ]
  H(:, 21) = [ -1, -1, -1,  1,  1, -1,  1 ]
  H(:, 22) = [ -1, -1, -1,  1,  1,  1, -1 ]
  H(:, 23) = [ -1, -1, -1,  1,  1,  1,  0 ]
  H(:, 24) = [ -1, -1, -1,  1,  1,  1,  1 ]
  H(:, 25) = [ -1, -1,  1, -1, -1, -1, -1 ]
  H(:, 26) = [ -1, -1,  1, -1, -1, -1,  0 ]
  H(:, 27) = [ -1, -1,  1, -1, -1, -1,  1 ]
  H(:, 28) = [ -1, -1,  1, -1, -1,  1, -1 ]
  H(:, 29) = [ -1, -1,  1, -1, -1,  1,  0 ]
  H(:, 30) = [ -1, -1,  1, -1, -1,  1,  1 ]
  H(:, 31) = [ -1, -1,  1, -1,  1, -1, -1 ]
  H(:, 32) = [ -1, -1,  1, -1,  1, -1,  0 ]
  H(:, 33) = [ -1, -1,  1, -1,  1, -1,  1 ]
  H(:, 34) = [ -1, -1,  1, -1,  1,  1, -1 ]
  H(:, 35) = [ -1, -1,  1, -1,  1,  1,  0 ]
  H(:, 36) = [ -1, -1,  1, -1,  1,  1,  1 ]
  H(:, 37) = [ -1, -1,  1,  1, -1, -1, -1 ]
  H(:, 38) = [ -1, -1,  1,  1, -1, -1,  0 ]
  H(:, 39) = [ -1, -1,  1,  1, -1, -1,  1 ]
  H(:, 40) = [ -1, -1,  1,  1, -1,  1, -1 ]
  H(:, 41) = [ -1, -1,  1,  1, -1,  1,  0 ]
  H(:, 42) = [ -1, -1,  1,  1, -1,  1,  1 ]
  H(:, 43) = [ -1, -1,  1,  1,  1, -1, -1 ]
  H(:, 44) = [ -1, -1,  1,  1,  1, -1,  0 ]
  H(:, 45) = [ -1, -1,  1,  1,  1, -1,  1 ]
  H(:, 46) = [ -1, -1,  1,  1,  1,  1, -1 ]
  H(:, 47) = [ -1, -1,  1,  1,  1,  1,  0 ]
  H(:, 48) = [ -1, -1,  1,  1,  1,  1,  1 ]
  H(:, 49) = [ -1,  1, -1, -1, -1, -1, -1 ]
  H(:, 50) = [ -1,  1, -1, -1, -1, -1,  0 ]
  H(:, 51) = [ -1,  1, -1, -1, -1, -1,  1 ]
  H(:, 52) = [ -1,  1, -1, -1, -1,  1, -1 ]
  H(:, 53) = [ -1,  1, -1, -1, -1,  1,  0 ]
  H(:, 54) = [ -1,  1, -1, -1, -1,  1,  1 ]
  H(:, 55) = [ -1,  1, -1, -1,  1, -1, -1 ]
  H(:, 56) = [ -1,  1, -1, -1,  1, -1,  0 ]
  H(:, 57) = [ -1,  1, -1, -1,  1, -1,  1 ]
  H(:, 58) = [ -1,  1, -1, -1,  1,  1, -1 ]
  H(:, 59) = [ -1,  1, -1, -1,  1,  1,  0 ]
  H(:, 60) = [ -1,  1, -1, -1,  1,  1,  1 ]
  H(:, 61) = [ -1,  1, -1,  1, -1, -1, -1 ]
  H(:, 62) = [ -1,  1, -1,  1, -1, -1,  0 ]
  H(:, 63) = [ -1,  1, -1,  1, -1, -1,  1 ]
  H(:, 64) = [ -1,  1, -1,  1, -1,  1, -1 ]
  H(:, 65) = [ -1,  1, -1,  1, -1,  1,  0 ]
  H(:, 66) = [ -1,  1, -1,  1, -1,  1,  1 ]
  H(:, 67) = [ -1,  1, -1,  1,  1, -1, -1 ]
  H(:, 68) = [ -1,  1, -1,  1,  1, -1,  0 ]
  H(:, 69) = [ -1,  1, -1,  1,  1, -1,  1 ]
  H(:, 70) = [ -1,  1, -1,  1,  1,  1, -1 ]
  H(:, 71) = [ -1,  1, -1,  1,  1,  1,  0 ]
  H(:, 72) = [ -1,  1, -1,  1,  1,  1,  1 ]
  H(:, 73) = [ -1,  1,  1, -1, -1, -1, -1 ]
  H(:, 74) = [ -1,  1,  1, -1, -1, -1,  0 ]
  H(:, 75) = [ -1,  1,  1, -1, -1, -1,  1 ]
  H(:, 76) = [ -1,  1,  1, -1, -1,  1, -1 ]
  H(:, 77) = [ -1,  1,  1, -1, -1,  1,  0 ]
  H(:, 78) = [ -1,  1,  1, -1, -1,  1,  1 ]
  H(:, 79) = [ -1,  1,  1, -1,  1, -1, -1 ]
  H(:, 80) = [ -1,  1,  1, -1,  1, -1,  0 ]
  H(:, 81) = [ -1,  1,  1, -1,  1, -1,  1 ]
  H(:, 82) = [ -1,  1,  1, -1,  1,  1, -1 ]
  H(:, 83) = [ -1,  1,  1, -1,  1,  1,  0 ]
  H(:, 84) = [ -1,  1,  1, -1,  1,  1,  1 ]
  H(:, 85) = [ -1,  1,  1,  1, -1, -1, -1 ]
  H(:, 86) = [ -1,  1,  1,  1, -1, -1,  0 ]
  H(:, 87) = [ -1,  1,  1,  1, -1, -1,  1 ]
  H(:, 88) = [ -1,  1,  1,  1, -1,  1, -1 ]
  H(:, 89) = [ -1,  1,  1,  1, -1,  1,  0 ]
  H(:, 90) = [ -1,  1,  1,  1, -1,  1,  1 ]
  H(:, 91) = [ -1,  1,  1,  1,  1, -1, -1 ]
  H(:, 92) = [ -1,  1,  1,  1,  1, -1,  0 ]
  H(:, 93) = [ -1,  1,  1,  1,  1, -1,  1 ]
  H(:, 94) = [ -1,  1,  1,  1,  1,  1, -1 ]
  H(:, 95) = [ -1,  1,  1,  1,  1,  1,  0 ]
  H(:, 96) = [ -1,  1,  1,  1,  1,  1,  1 ]
  H(:, 97) = [  1, -1, -1, -1, -1, -1, -1 ]
  H(:, 98) = [  1, -1, -1, -1, -1, -1,  0 ]
  H(:, 99) = [  1, -1, -1, -1, -1, -1,  1 ]
  H(:,100) = [  1, -1, -1, -1, -1,  1, -1 ]
  H(:,101) = [  1, -1, -1, -1, -1,  1,  0 ]
  H(:,102) = [  1, -1, -1, -1, -1,  1,  1 ]
  H(:,103) = [  1, -1, -1, -1,  1, -1, -1 ]
  H(:,104) = [  1, -1, -1, -1,  1, -1,  0 ]
  H(:,105) = [  1, -1, -1, -1,  1, -1,  1 ]
  H(:,106) = [  1, -1, -1, -1,  1,  1, -1 ]
  H(:,107) = [  1, -1, -1, -1,  1,  1,  0 ]
  H(:,108) = [  1, -1, -1, -1,  1,  1,  1 ]
  H(:,109) = [  1, -1, -1,  1, -1, -1, -1 ]
  H(:,110) = [  1, -1, -1,  1, -1, -1,  0 ]
  H(:,111) = [  1, -1, -1,  1, -1, -1,  1 ]
  H(:,112) = [  1, -1, -1,  1, -1,  1, -1 ]
  H(:,113) = [  1, -1, -1,  1, -1,  1,  0 ]
  H(:,114) = [  1, -1, -1,  1, -1,  1,  1 ]
  H(:,115) = [  1, -1, -1,  1,  1, -1, -1 ]
  H(:,116) = [  1, -1, -1,  1,  1, -1,  0 ]
  H(:,117) = [  1, -1, -1,  1,  1, -1,  1 ]
  H(:,118) = [  1, -1, -1,  1,  1,  1, -1 ]
  H(:,119) = [  1, -1, -1,  1,  1,  1,  0 ]
  H(:,120) = [  1, -1, -1,  1,  1,  1,  1 ]
  H(:,121) = [  1, -1,  1, -1, -1, -1, -1 ]
  H(:,122) = [  1, -1,  1, -1, -1, -1,  0 ]
  H(:,123) = [  1, -1,  1, -1, -1, -1,  1 ]
  H(:,124) = [  1, -1,  1, -1, -1,  1, -1 ]
  H(:,125) = [  1, -1,  1, -1, -1,  1,  0 ]
  H(:,126) = [  1, -1,  1, -1, -1,  1,  1 ]
  H(:,127) = [  1, -1,  1, -1,  1, -1, -1 ]
  H(:,128) = [  1, -1,  1, -1,  1, -1,  0 ]
  H(:,129) = [  1, -1,  1, -1,  1, -1,  1 ]
  H(:,130) = [  1, -1,  1, -1,  1,  1, -1 ]
  H(:,131) = [  1, -1,  1, -1,  1,  1,  0 ]
  H(:,132) = [  1, -1,  1, -1,  1,  1,  1 ]
  H(:,133) = [  1, -1,  1,  1, -1, -1, -1 ]
  H(:,134) = [  1, -1,  1,  1, -1, -1,  0 ]
  H(:,135) = [  1, -1,  1,  1, -1, -1,  1 ]
  H(:,136) = [  1, -1,  1,  1, -1,  1, -1 ]
  H(:,137) = [  1, -1,  1,  1, -1,  1,  0 ]
  H(:,138) = [  1, -1,  1,  1, -1,  1,  1 ]
  H(:,139) = [  1, -1,  1,  1,  1, -1, -1 ]
  H(:,140) = [  1, -1,  1,  1,  1, -1,  0 ]
  H(:,141) = [  1, -1,  1,  1,  1, -1,  1 ]
  H(:,142) = [  1, -1,  1,  1,  1,  1, -1 ]
  H(:,143) = [  1, -1,  1,  1,  1,  1,  0 ]
  H(:,144) = [  1, -1,  1,  1,  1,  1,  1 ]
  H(:,145) = [  1,  1, -1, -1, -1, -1, -1 ]
  H(:,146) = [  1,  1, -1, -1, -1, -1,  0 ]
  H(:,147) = [  1,  1, -1, -1, -1, -1,  1 ]
  H(:,148) = [  1,  1, -1, -1, -1,  1, -1 ]
  H(:,149) = [  1,  1, -1, -1, -1,  1,  0 ]
  H(:,150) = [  1,  1, -1, -1, -1,  1,  1 ]
  H(:,151) = [  1,  1, -1, -1,  1, -1, -1 ]
  H(:,152) = [  1,  1, -1, -1,  1, -1,  0 ]
  H(:,153) = [  1,  1, -1, -1,  1, -1,  1 ]
  H(:,154) = [  1,  1, -1, -1,  1,  1, -1 ]
  H(:,155) = [  1,  1, -1, -1,  1,  1,  0 ]
  H(:,156) = [  1,  1, -1, -1,  1,  1,  1 ]
  H(:,157) = [  1,  1, -1,  1, -1, -1, -1 ]
  H(:,158) = [  1,  1, -1,  1, -1, -1,  0 ]
  H(:,159) = [  1,  1, -1,  1, -1, -1,  1 ]
  H(:,160) = [  1,  1, -1,  1, -1,  1, -1 ]
  H(:,161) = [  1,  1, -1,  1, -1,  1,  0 ]
  H(:,162) = [  1,  1, -1,  1, -1,  1,  1 ]
  H(:,163) = [  1,  1, -1,  1,  1, -1, -1 ]
  H(:,164) = [  1,  1, -1,  1,  1, -1,  0 ]
  H(:,165) = [  1,  1, -1,  1,  1, -1,  1 ]
  H(:,166) = [  1,  1, -1,  1,  1,  1, -1 ]
  H(:,167) = [  1,  1, -1,  1,  1,  1,  0 ]
  H(:,168) = [  1,  1, -1,  1,  1,  1,  1 ]
  H(:,169) = [  1,  1,  1, -1, -1, -1, -1 ]
  H(:,170) = [  1,  1,  1, -1, -1, -1,  0 ]
  H(:,171) = [  1,  1,  1, -1, -1, -1,  1 ]
  H(:,172) = [  1,  1,  1, -1, -1,  1, -1 ]
  H(:,173) = [  1,  1,  1, -1, -1,  1,  0 ]
  H(:,174) = [  1,  1,  1, -1, -1,  1,  1 ]
  H(:,175) = [  1,  1,  1, -1,  1, -1, -1 ]
  H(:,176) = [  1,  1,  1, -1,  1, -1,  0 ]
  H(:,177) = [  1,  1,  1, -1,  1, -1,  1 ]
  H(:,178) = [  1,  1,  1, -1,  1,  1, -1 ]
  H(:,179) = [  1,  1,  1, -1,  1,  1,  0 ]
  H(:,180) = [  1,  1,  1, -1,  1,  1,  1 ]
  H(:,181) = [  1,  1,  1,  1, -1, -1, -1 ]
  H(:,182) = [  1,  1,  1,  1, -1, -1,  0 ]
  H(:,183) = [  1,  1,  1,  1, -1, -1,  1 ]
  H(:,184) = [  1,  1,  1,  1, -1,  1, -1 ]
  H(:,185) = [  1,  1,  1,  1, -1,  1,  0 ]
  H(:,186) = [  1,  1,  1,  1, -1,  1,  1 ]
  H(:,187) = [  1,  1,  1,  1,  1, -1, -1 ]
  H(:,188) = [  1,  1,  1,  1,  1, -1,  0 ]
  H(:,189) = [  1,  1,  1,  1,  1, -1,  1 ]
  H(:,190) = [  1,  1,  1,  1,  1,  1, -1 ]
  H(:,191) = [  1,  1,  1,  1,  1,  1,  0 ]
  H(:,192) = [  1,  1,  1,  1,  1,  1,  1 ]

  H_HC(:,5) = [ ((((2*(binco-1)+flip)*6+binpos, flip = 0, 1), binpos = 1, 6), binco = 1, 192/6/2) ]
  H_HC(:,6) = [ ((((2*(binco-1)+flip)*3+binpos, flip = 0, 1), binpos = 1, 3), binco = 1, 192/3/2) ]
  end subroutine hel_init

end module ol_external_ppvvvj_bbbxbxaaz_1


module colour_basis_ppvvvj_bbbxbxaaz_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,0,0,0]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppvvvj_bbbxbxaaz_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(7)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 2
    ncoupl = 1
    maxpows = 1
    nhel = 192
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_ppvvvj_bbbxbxaaz_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,2)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([6], [1,1])
#endif
#if 2 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [1,16,3,1,14,3], &
      [3,2])
#endif
  end subroutine tree_colbasis

end module colour_basis_ppvvvj_bbbxbxaaz_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppvvvj_bbbxbxaaz_1(perm)
  use ol_external_ppvvvj_bbbxbxaaz_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_ppvvvj_bbbxbxaaz_1

! **********************************************************************
module ol_heltables_ppvvvj_bbbxbxaaz_1
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
    H5(2) = [-1,1], &
    H6(2) = [-1,1], &
    H7(3) = [-1,0,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(44), n3(3,432)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x6(2,6,4), t3x4(2,4,12), t3x12(2,12,24), t3x8(2,8,32), t3x24(2,24,48), t3x16(2,16,72), &
    t3x192(2,192,240)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(192,7)
  integer,           save :: exthel(192,7)
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
  call heltable([2,2,4], n3(:,3), t3x4(:,:,3))
  n2(1) = 4
  n2(2) = 4
  call heltable([3,4,12], n3(:,4), t3x12(:,:,1))
  call heltable([4,4,16], n3(:,5), t3x16(:,:,1))
  n2(3) = 12
  call heltable([4,3,12], n3(:,6), t3x12(:,:,2))
  call heltable([4,4,16], n3(:,7), t3x16(:,:,2))
  n2(4) = 12
  call heltable([2,3,6], n3(:,8), t3x6(:,:,1))
  n2(5) = 6
  call heltable([2,4,8], n3(:,9), t3x8(:,:,1))
  call heltable([6,4,24], n3(:,10), t3x24(:,:,1))
  n2(6) = 8
  call heltable([6,2,12], n3(:,11), t3x12(:,:,3))
  n2(7) = 12
  call heltable([2,4,8], n3(:,12), t3x8(:,:,2))
  n2(8) = 8
  call heltable([8,3,24], n3(:,13), t3x24(:,:,2))
  call heltable([8,2,16], n3(:,14), t3x16(:,:,3))
  call heltable([2,2,4], n3(:,15), t3x4(:,:,4))
  call heltable([2,2,4], n3(:,16), t3x4(:,:,5))
  n2(9) = 4
  n2(10) = 4
  call heltable([3,4,12], n3(:,17), t3x12(:,:,4))
  call heltable([4,4,16], n3(:,18), t3x16(:,:,4))
  n2(11) = 12
  call heltable([4,3,12], n3(:,19), t3x12(:,:,5))
  call heltable([4,4,16], n3(:,20), t3x16(:,:,5))
  n2(12) = 12
  call heltable([3,2,6], n3(:,21), t3x6(:,:,2))
  n2(13) = 6
  call heltable([2,6,12], n3(:,22), t3x12(:,:,6))
  n2(14) = 12
  call heltable([4,2,8], n3(:,23), t3x8(:,:,3))
  call heltable([4,6,24], n3(:,24), t3x24(:,:,3))
  n2(15) = 8
  call heltable([4,2,8], n3(:,25), t3x8(:,:,4))
  n2(16) = 8
  call heltable([2,8,16], n3(:,26), t3x16(:,:,6))
  call heltable([3,8,24], n3(:,27), t3x24(:,:,4))
  call heltable([2,4,8], n3(:,28), t3x8(:,:,5))
  n2(17) = 8
  call heltable([6,2,12], n3(:,29), t3x12(:,:,7))
  n2(18) = 12
  call heltable([8,2,16], n3(:,30), t3x16(:,:,7))
  call heltable([2,6,12], n3(:,31), t3x12(:,:,8))
  n2(19) = 12
  call heltable([4,2,8], n3(:,32), t3x8(:,:,6))
  n2(20) = 8
  call heltable([2,8,16], n3(:,33), t3x16(:,:,8))
  call heltable([2,2,4], n3(:,34), t3x4(:,:,6))
  call heltable([2,2,4], n3(:,35), t3x4(:,:,7))
  n2(21) = 4
  call heltable([3,4,12], n3(:,36), t3x12(:,:,9))
  call heltable([4,4,16], n3(:,37), t3x16(:,:,9))
  n2(22) = 12
  call heltable([4,4,16], n3(:,38), t3x16(:,:,10))
  call heltable([2,4,8], n3(:,39), t3x8(:,:,7))
  call heltable([6,4,24], n3(:,40), t3x24(:,:,5))
  n2(23) = 8
  call heltable([2,4,8], n3(:,41), t3x8(:,:,8))
  n2(24) = 8
  call heltable([8,3,24], n3(:,42), t3x24(:,:,6))
  call heltable([8,2,16], n3(:,43), t3x16(:,:,11))
  call heltable([2,2,4], n3(:,44), t3x4(:,:,8))
  n2(25) = 4
  call heltable([3,4,12], n3(:,45), t3x12(:,:,10))
  call heltable([4,4,16], n3(:,46), t3x16(:,:,12))
  n2(26) = 12
  call heltable([4,4,16], n3(:,47), t3x16(:,:,13))
  call heltable([3,2,6], n3(:,48), t3x6(:,:,3))
  n2(27) = 6
  call heltable([2,6,12], n3(:,49), t3x12(:,:,11))
  n2(28) = 12
  call heltable([4,6,24], n3(:,50), t3x24(:,:,7))
  call heltable([4,2,8], n3(:,51), t3x8(:,:,9))
  n2(29) = 8
  call heltable([2,8,16], n3(:,52), t3x16(:,:,14))
  call heltable([3,8,24], n3(:,53), t3x24(:,:,8))
  call heltable([2,4,8], n3(:,54), t3x8(:,:,10))
  n2(30) = 8
  call heltable([8,2,16], n3(:,55), t3x16(:,:,15))
  call heltable([2,6,12], n3(:,56), t3x12(:,:,12))
  n2(31) = 12
  call heltable([2,8,16], n3(:,57), t3x16(:,:,16))
  call heltable([2,2,4], n3(:,58), t3x4(:,:,9))
  call heltable([2,2,4], n3(:,59), t3x4(:,:,10))
  n2(32) = 4
  call heltable([4,4,16], n3(:,60), t3x16(:,:,17))
  call heltable([4,3,12], n3(:,61), t3x12(:,:,13))
  call heltable([4,4,16], n3(:,62), t3x16(:,:,18))
  n2(33) = 12
  call heltable([2,3,6], n3(:,63), t3x6(:,:,4))
  n2(34) = 6
  call heltable([6,4,24], n3(:,64), t3x24(:,:,9))
  call heltable([6,2,12], n3(:,65), t3x12(:,:,14))
  n2(35) = 12
  call heltable([2,4,8], n3(:,66), t3x8(:,:,11))
  n2(36) = 8
  call heltable([8,3,24], n3(:,67), t3x24(:,:,10))
  call heltable([8,2,16], n3(:,68), t3x16(:,:,19))
  call heltable([2,2,4], n3(:,69), t3x4(:,:,11))
  n2(37) = 4
  call heltable([4,4,16], n3(:,70), t3x16(:,:,20))
  call heltable([4,3,12], n3(:,71), t3x12(:,:,15))
  call heltable([4,4,16], n3(:,72), t3x16(:,:,21))
  n2(38) = 12
  call heltable([4,2,8], n3(:,73), t3x8(:,:,12))
  call heltable([4,6,24], n3(:,74), t3x24(:,:,11))
  n2(39) = 8
  call heltable([4,2,8], n3(:,75), t3x8(:,:,13))
  n2(40) = 8
  call heltable([2,8,16], n3(:,76), t3x16(:,:,22))
  call heltable([3,8,24], n3(:,77), t3x24(:,:,12))
  call heltable([6,2,12], n3(:,78), t3x12(:,:,16))
  n2(41) = 12
  call heltable([8,2,16], n3(:,79), t3x16(:,:,23))
  call heltable([4,2,8], n3(:,80), t3x8(:,:,14))
  n2(42) = 8
  call heltable([2,8,16], n3(:,81), t3x16(:,:,24))
  call heltable([2,2,4], n3(:,82), t3x4(:,:,12))
  call heltable([4,4,16], n3(:,83), t3x16(:,:,25))
  call heltable([4,4,16], n3(:,84), t3x16(:,:,26))
  call heltable([6,4,24], n3(:,85), t3x24(:,:,13))
  call heltable([2,4,8], n3(:,86), t3x8(:,:,15))
  n2(43) = 8
  call heltable([8,3,24], n3(:,87), t3x24(:,:,14))
  call heltable([8,2,16], n3(:,88), t3x16(:,:,27))
  call heltable([4,4,16], n3(:,89), t3x16(:,:,28))
  call heltable([4,4,16], n3(:,90), t3x16(:,:,29))
  call heltable([4,6,24], n3(:,91), t3x24(:,:,15))
  call heltable([4,2,8], n3(:,92), t3x8(:,:,16))
  n2(44) = 8
  call heltable([2,8,16], n3(:,93), t3x16(:,:,30))
  call heltable([3,8,24], n3(:,94), t3x24(:,:,16))
  call heltable([8,2,16], n3(:,95), t3x16(:,:,31))
  call heltable([2,8,16], n3(:,96), t3x16(:,:,32))
  call heltable([4,2,8], n3(:,97), t3x8(:,:,17))
  call heltable([4,6,24], n3(:,98), t3x24(:,:,17))
  call heltable([4,2,8], n3(:,99), t3x8(:,:,18))
  call heltable([4,6,24], n3(:,100), t3x24(:,:,18))
  call heltable([4,2,8], n3(:,101), t3x8(:,:,19))
  call heltable([4,6,24], n3(:,102), t3x24(:,:,19))
  call heltable([4,2,8], n3(:,103), t3x8(:,:,20))
  call heltable([4,6,24], n3(:,104), t3x24(:,:,20))
  call heltable([2,8,16], n3(:,105), t3x16(:,:,33))
  call heltable([2,8,16], n3(:,106), t3x16(:,:,34))
  call heltable([2,8,16], n3(:,107), t3x16(:,:,35))
  call heltable([2,8,16], n3(:,108), t3x16(:,:,36))
  call heltable([6,4,24], n3(:,109), t3x24(:,:,21))
  call heltable([6,2,12], n3(:,110), t3x12(:,:,17))
  call heltable([4,4,16], n3(:,111), t3x16(:,:,37))
  call heltable([2,4,8], n3(:,112), t3x8(:,:,21))
  call heltable([2,6,12], n3(:,113), t3x12(:,:,18))
  call heltable([8,2,16], n3(:,114), t3x16(:,:,38))
  call heltable([2,8,16], n3(:,115), t3x16(:,:,39))
  call heltable([6,4,24], n3(:,116), t3x24(:,:,22))
  call heltable([6,2,12], n3(:,117), t3x12(:,:,19))
  call heltable([4,4,16], n3(:,118), t3x16(:,:,40))
  call heltable([2,6,12], n3(:,119), t3x12(:,:,20))
  call heltable([2,4,8], n3(:,120), t3x8(:,:,22))
  call heltable([8,2,16], n3(:,121), t3x16(:,:,41))
  call heltable([2,8,16], n3(:,122), t3x16(:,:,42))
  call heltable([2,12,24], n3(:,123), t3x24(:,:,23))
  call heltable([2,12,24], n3(:,124), t3x24(:,:,24))
  call heltable([2,12,24], n3(:,125), t3x24(:,:,25))
  call heltable([2,12,24], n3(:,126), t3x24(:,:,26))
  call heltable([4,2,8], n3(:,127), t3x8(:,:,23))
  call heltable([4,6,24], n3(:,128), t3x24(:,:,27))
  call heltable([4,2,8], n3(:,129), t3x8(:,:,24))
  call heltable([4,6,24], n3(:,130), t3x24(:,:,28))
  call heltable([4,2,8], n3(:,131), t3x8(:,:,25))
  call heltable([4,6,24], n3(:,132), t3x24(:,:,29))
  call heltable([4,2,8], n3(:,133), t3x8(:,:,26))
  call heltable([4,6,24], n3(:,134), t3x24(:,:,30))
  call heltable([2,8,16], n3(:,135), t3x16(:,:,43))
  call heltable([2,8,16], n3(:,136), t3x16(:,:,44))
  call heltable([2,8,16], n3(:,137), t3x16(:,:,45))
  call heltable([2,8,16], n3(:,138), t3x16(:,:,46))
  call heltable([6,2,12], n3(:,139), t3x12(:,:,21))
  call heltable([4,4,16], n3(:,140), t3x16(:,:,47))
  call heltable([6,4,24], n3(:,141), t3x24(:,:,31))
  call heltable([2,4,8], n3(:,142), t3x8(:,:,27))
  call heltable([2,6,12], n3(:,143), t3x12(:,:,22))
  call heltable([2,8,16], n3(:,144), t3x16(:,:,48))
  call heltable([8,2,16], n3(:,145), t3x16(:,:,49))
  call heltable([6,2,12], n3(:,146), t3x12(:,:,23))
  call heltable([4,4,16], n3(:,147), t3x16(:,:,50))
  call heltable([6,4,24], n3(:,148), t3x24(:,:,32))
  call heltable([2,6,12], n3(:,149), t3x12(:,:,24))
  call heltable([2,4,8], n3(:,150), t3x8(:,:,28))
  call heltable([2,8,16], n3(:,151), t3x16(:,:,51))
  call heltable([8,2,16], n3(:,152), t3x16(:,:,52))
  call heltable([2,12,24], n3(:,153), t3x24(:,:,33))
  call heltable([2,12,24], n3(:,154), t3x24(:,:,34))
  call heltable([2,12,24], n3(:,155), t3x24(:,:,35))
  call heltable([2,12,24], n3(:,156), t3x24(:,:,36))
  call heltable([6,4,24], n3(:,157), t3x24(:,:,37))
  call heltable([4,4,16], n3(:,158), t3x16(:,:,53))
  call heltable([2,4,8], n3(:,159), t3x8(:,:,29))
  call heltable([8,2,16], n3(:,160), t3x16(:,:,54))
  call heltable([2,8,16], n3(:,161), t3x16(:,:,55))
  call heltable([4,4,16], n3(:,162), t3x16(:,:,56))
  call heltable([6,4,24], n3(:,163), t3x24(:,:,38))
  call heltable([2,4,8], n3(:,164), t3x8(:,:,30))
  call heltable([2,8,16], n3(:,165), t3x16(:,:,57))
  call heltable([8,2,16], n3(:,166), t3x16(:,:,58))
  call heltable([8,2,16], n3(:,167), t3x16(:,:,59))
  call heltable([8,2,16], n3(:,168), t3x16(:,:,60))
  call heltable([8,2,16], n3(:,169), t3x16(:,:,61))
  call heltable([8,2,16], n3(:,170), t3x16(:,:,62))
  call heltable([12,2,24], n3(:,171), t3x24(:,:,39))
  call heltable([12,2,24], n3(:,172), t3x24(:,:,40))
  call heltable([12,2,24], n3(:,173), t3x24(:,:,41))
  call heltable([12,2,24], n3(:,174), t3x24(:,:,42))
  call heltable([6,4,24], n3(:,175), t3x24(:,:,43))
  call heltable([4,4,16], n3(:,176), t3x16(:,:,63))
  call heltable([2,4,8], n3(:,177), t3x8(:,:,31))
  call heltable([8,2,16], n3(:,178), t3x16(:,:,64))
  call heltable([2,8,16], n3(:,179), t3x16(:,:,65))
  call heltable([4,4,16], n3(:,180), t3x16(:,:,66))
  call heltable([6,4,24], n3(:,181), t3x24(:,:,44))
  call heltable([2,4,8], n3(:,182), t3x8(:,:,32))
  call heltable([2,8,16], n3(:,183), t3x16(:,:,67))
  call heltable([8,2,16], n3(:,184), t3x16(:,:,68))
  call heltable([8,2,16], n3(:,185), t3x16(:,:,69))
  call heltable([8,2,16], n3(:,186), t3x16(:,:,70))
  call heltable([8,2,16], n3(:,187), t3x16(:,:,71))
  call heltable([8,2,16], n3(:,188), t3x16(:,:,72))
  call heltable([12,2,24], n3(:,189), t3x24(:,:,45))
  call heltable([12,2,24], n3(:,190), t3x24(:,:,46))
  call heltable([12,2,24], n3(:,191), t3x24(:,:,47))
  call heltable([12,2,24], n3(:,192), t3x24(:,:,48))
  call heltable([16,12,192], n3(:,193), t3x192(:,:,1))
  call heltable([16,12,192], n3(:,194), t3x192(:,:,2))
  call heltable([24,8,192], n3(:,195), t3x192(:,:,3))
  call heltable([16,12,192], n3(:,196), t3x192(:,:,4))
  call heltable([8,24,192], n3(:,197), t3x192(:,:,5))
  call heltable([12,16,192], n3(:,198), t3x192(:,:,6))
  call heltable([16,12,192], n3(:,199), t3x192(:,:,7))
  call heltable([16,12,192], n3(:,200), t3x192(:,:,8))
  call heltable([16,12,192], n3(:,201), t3x192(:,:,9))
  call heltable([24,8,192], n3(:,202), t3x192(:,:,10))
  call heltable([12,16,192], n3(:,203), t3x192(:,:,11))
  call heltable([8,24,192], n3(:,204), t3x192(:,:,12))
  call heltable([24,8,192], n3(:,205), t3x192(:,:,13))
  call heltable([16,12,192], n3(:,206), t3x192(:,:,14))
  call heltable([24,8,192], n3(:,207), t3x192(:,:,15))
  call heltable([12,16,192], n3(:,208), t3x192(:,:,16))
  call heltable([16,12,192], n3(:,209), t3x192(:,:,17))
  call heltable([24,8,192], n3(:,210), t3x192(:,:,18))
  call heltable([12,16,192], n3(:,211), t3x192(:,:,19))
  call heltable([24,8,192], n3(:,212), t3x192(:,:,20))
  call heltable([16,12,192], n3(:,213), t3x192(:,:,21))
  call heltable([12,16,192], n3(:,214), t3x192(:,:,22))
  call heltable([12,16,192], n3(:,215), t3x192(:,:,23))
  call heltable([16,12,192], n3(:,216), t3x192(:,:,24))
  call heltable([16,12,192], n3(:,217), t3x192(:,:,25))
  call heltable([12,16,192], n3(:,218), t3x192(:,:,26))
  call heltable([24,8,192], n3(:,219), t3x192(:,:,27))
  call heltable([12,16,192], n3(:,220), t3x192(:,:,28))
  call heltable([8,24,192], n3(:,221), t3x192(:,:,29))
  call heltable([12,16,192], n3(:,222), t3x192(:,:,30))
  call heltable([16,12,192], n3(:,223), t3x192(:,:,31))
  call heltable([12,16,192], n3(:,224), t3x192(:,:,32))
  call heltable([16,12,192], n3(:,225), t3x192(:,:,33))
  call heltable([8,24,192], n3(:,226), t3x192(:,:,34))
  call heltable([12,16,192], n3(:,227), t3x192(:,:,35))
  call heltable([8,24,192], n3(:,228), t3x192(:,:,36))
  call heltable([24,8,192], n3(:,229), t3x192(:,:,37))
  call heltable([12,16,192], n3(:,230), t3x192(:,:,38))
  call heltable([24,8,192], n3(:,231), t3x192(:,:,39))
  call heltable([12,16,192], n3(:,232), t3x192(:,:,40))
  call heltable([16,12,192], n3(:,233), t3x192(:,:,41))
  call heltable([8,24,192], n3(:,234), t3x192(:,:,42))
  call heltable([12,16,192], n3(:,235), t3x192(:,:,43))
  call heltable([8,24,192], n3(:,236), t3x192(:,:,44))
  call heltable([16,12,192], n3(:,237), t3x192(:,:,45))
  call heltable([12,16,192], n3(:,238), t3x192(:,:,46))
  call heltable([12,16,192], n3(:,239), t3x192(:,:,47))
  call heltable([12,16,192], n3(:,240), t3x192(:,:,48))
  call heltable([12,16,192], n3(:,241), t3x192(:,:,49))
  call heltable([16,12,192], n3(:,242), t3x192(:,:,50))
  call heltable([8,24,192], n3(:,243), t3x192(:,:,51))
  call heltable([16,12,192], n3(:,244), t3x192(:,:,52))
  call heltable([8,24,192], n3(:,245), t3x192(:,:,53))
  call heltable([12,16,192], n3(:,246), t3x192(:,:,54))
  call heltable([12,16,192], n3(:,247), t3x192(:,:,55))
  call heltable([16,12,192], n3(:,248), t3x192(:,:,56))
  call heltable([12,16,192], n3(:,249), t3x192(:,:,57))
  call heltable([24,8,192], n3(:,250), t3x192(:,:,58))
  call heltable([12,16,192], n3(:,251), t3x192(:,:,59))
  call heltable([8,24,192], n3(:,252), t3x192(:,:,60))
  call heltable([8,24,192], n3(:,253), t3x192(:,:,61))
  call heltable([16,12,192], n3(:,254), t3x192(:,:,62))
  call heltable([8,24,192], n3(:,255), t3x192(:,:,63))
  call heltable([12,16,192], n3(:,256), t3x192(:,:,64))
  call heltable([12,16,192], n3(:,257), t3x192(:,:,65))
  call heltable([24,8,192], n3(:,258), t3x192(:,:,66))
  call heltable([12,16,192], n3(:,259), t3x192(:,:,67))
  call heltable([24,8,192], n3(:,260), t3x192(:,:,68))
  call heltable([12,16,192], n3(:,261), t3x192(:,:,69))
  call heltable([12,16,192], n3(:,262), t3x192(:,:,70))
  call heltable([12,16,192], n3(:,263), t3x192(:,:,71))
  call heltable([16,12,192], n3(:,264), t3x192(:,:,72))
  call heltable([12,16,192], n3(:,265), t3x192(:,:,73))
  call heltable([12,16,192], n3(:,266), t3x192(:,:,74))
  call heltable([8,24,192], n3(:,267), t3x192(:,:,75))
  call heltable([12,16,192], n3(:,268), t3x192(:,:,76))
  call heltable([8,24,192], n3(:,269), t3x192(:,:,77))
  call heltable([12,16,192], n3(:,270), t3x192(:,:,78))
  call heltable([12,16,192], n3(:,271), t3x192(:,:,79))
  call heltable([12,16,192], n3(:,272), t3x192(:,:,80))
  call heltable([12,16,192], n3(:,273), t3x192(:,:,81))
  call heltable([8,24,192], n3(:,274), t3x192(:,:,82))
  call heltable([12,16,192], n3(:,275), t3x192(:,:,83))
  call heltable([8,24,192], n3(:,276), t3x192(:,:,84))
  call heltable([8,24,192], n3(:,277), t3x192(:,:,85))
  call heltable([12,16,192], n3(:,278), t3x192(:,:,86))
  call heltable([8,24,192], n3(:,279), t3x192(:,:,87))
  call heltable([12,16,192], n3(:,280), t3x192(:,:,88))
  call heltable([12,16,192], n3(:,281), t3x192(:,:,89))
  call heltable([8,24,192], n3(:,282), t3x192(:,:,90))
  call heltable([12,16,192], n3(:,283), t3x192(:,:,91))
  call heltable([8,24,192], n3(:,284), t3x192(:,:,92))
  call heltable([12,16,192], n3(:,285), t3x192(:,:,93))
  call heltable([12,16,192], n3(:,286), t3x192(:,:,94))
  call heltable([12,16,192], n3(:,287), t3x192(:,:,95))
  call heltable([12,16,192], n3(:,288), t3x192(:,:,96))
  call heltable([8,24,192], n3(:,289), t3x192(:,:,97))
  call heltable([8,24,192], n3(:,290), t3x192(:,:,98))
  call heltable([8,24,192], n3(:,291), t3x192(:,:,99))
  call heltable([8,24,192], n3(:,292), t3x192(:,:,100))
  call heltable([12,16,192], n3(:,293), t3x192(:,:,101))
  call heltable([12,16,192], n3(:,294), t3x192(:,:,102))
  call heltable([12,16,192], n3(:,295), t3x192(:,:,103))
  call heltable([12,16,192], n3(:,296), t3x192(:,:,104))
  call heltable([8,24,192], n3(:,297), t3x192(:,:,105))
  call heltable([12,16,192], n3(:,298), t3x192(:,:,106))
  call heltable([24,8,192], n3(:,299), t3x192(:,:,107))
  call heltable([16,12,192], n3(:,300), t3x192(:,:,108))
  call heltable([12,16,192], n3(:,301), t3x192(:,:,109))
  call heltable([12,16,192], n3(:,302), t3x192(:,:,110))
  call heltable([8,24,192], n3(:,303), t3x192(:,:,111))
  call heltable([12,16,192], n3(:,304), t3x192(:,:,112))
  call heltable([16,12,192], n3(:,305), t3x192(:,:,113))
  call heltable([24,8,192], n3(:,306), t3x192(:,:,114))
  call heltable([12,16,192], n3(:,307), t3x192(:,:,115))
  call heltable([12,16,192], n3(:,308), t3x192(:,:,116))
  call heltable([12,16,192], n3(:,309), t3x192(:,:,117))
  call heltable([12,16,192], n3(:,310), t3x192(:,:,118))
  call heltable([8,24,192], n3(:,311), t3x192(:,:,119))
  call heltable([8,24,192], n3(:,312), t3x192(:,:,120))
  call heltable([12,16,192], n3(:,313), t3x192(:,:,121))
  call heltable([8,24,192], n3(:,314), t3x192(:,:,122))
  call heltable([12,16,192], n3(:,315), t3x192(:,:,123))
  call heltable([8,24,192], n3(:,316), t3x192(:,:,124))
  call heltable([8,24,192], n3(:,317), t3x192(:,:,125))
  call heltable([8,24,192], n3(:,318), t3x192(:,:,126))
  call heltable([8,24,192], n3(:,319), t3x192(:,:,127))
  call heltable([8,24,192], n3(:,320), t3x192(:,:,128))
  call heltable([12,16,192], n3(:,321), t3x192(:,:,129))
  call heltable([12,16,192], n3(:,322), t3x192(:,:,130))
  call heltable([12,16,192], n3(:,323), t3x192(:,:,131))
  call heltable([12,16,192], n3(:,324), t3x192(:,:,132))
  call heltable([12,16,192], n3(:,325), t3x192(:,:,133))
  call heltable([8,24,192], n3(:,326), t3x192(:,:,134))
  call heltable([24,8,192], n3(:,327), t3x192(:,:,135))
  call heltable([16,12,192], n3(:,328), t3x192(:,:,136))
  call heltable([12,16,192], n3(:,329), t3x192(:,:,137))
  call heltable([12,16,192], n3(:,330), t3x192(:,:,138))
  call heltable([12,16,192], n3(:,331), t3x192(:,:,139))
  call heltable([8,24,192], n3(:,332), t3x192(:,:,140))
  call heltable([16,12,192], n3(:,333), t3x192(:,:,141))
  call heltable([24,8,192], n3(:,334), t3x192(:,:,142))
  call heltable([12,16,192], n3(:,335), t3x192(:,:,143))
  call heltable([12,16,192], n3(:,336), t3x192(:,:,144))
  call heltable([8,24,192], n3(:,337), t3x192(:,:,145))
  call heltable([8,24,192], n3(:,338), t3x192(:,:,146))
  call heltable([12,16,192], n3(:,339), t3x192(:,:,147))
  call heltable([12,16,192], n3(:,340), t3x192(:,:,148))
  call heltable([8,24,192], n3(:,341), t3x192(:,:,149))
  call heltable([12,16,192], n3(:,342), t3x192(:,:,150))
  call heltable([8,24,192], n3(:,343), t3x192(:,:,151))
  call heltable([12,16,192], n3(:,344), t3x192(:,:,152))
  call heltable([8,24,192], n3(:,345), t3x192(:,:,153))
  call heltable([12,16,192], n3(:,346), t3x192(:,:,154))
  call heltable([24,8,192], n3(:,347), t3x192(:,:,155))
  call heltable([12,16,192], n3(:,348), t3x192(:,:,156))
  call heltable([12,16,192], n3(:,349), t3x192(:,:,157))
  call heltable([12,16,192], n3(:,350), t3x192(:,:,158))
  call heltable([12,16,192], n3(:,351), t3x192(:,:,159))
  call heltable([8,24,192], n3(:,352), t3x192(:,:,160))
  call heltable([24,8,192], n3(:,353), t3x192(:,:,161))
  call heltable([12,16,192], n3(:,354), t3x192(:,:,162))
  call heltable([12,16,192], n3(:,355), t3x192(:,:,163))
  call heltable([12,16,192], n3(:,356), t3x192(:,:,164))
  call heltable([24,8,192], n3(:,357), t3x192(:,:,165))
  call heltable([8,24,192], n3(:,358), t3x192(:,:,166))
  call heltable([24,8,192], n3(:,359), t3x192(:,:,167))
  call heltable([8,24,192], n3(:,360), t3x192(:,:,168))
  call heltable([12,16,192], n3(:,361), t3x192(:,:,169))
  call heltable([12,16,192], n3(:,362), t3x192(:,:,170))
  call heltable([12,16,192], n3(:,363), t3x192(:,:,171))
  call heltable([12,16,192], n3(:,364), t3x192(:,:,172))
  call heltable([8,24,192], n3(:,365), t3x192(:,:,173))
  call heltable([12,16,192], n3(:,366), t3x192(:,:,174))
  call heltable([12,16,192], n3(:,367), t3x192(:,:,175))
  call heltable([8,24,192], n3(:,368), t3x192(:,:,176))
  call heltable([12,16,192], n3(:,369), t3x192(:,:,177))
  call heltable([8,24,192], n3(:,370), t3x192(:,:,178))
  call heltable([12,16,192], n3(:,371), t3x192(:,:,179))
  call heltable([8,24,192], n3(:,372), t3x192(:,:,180))
  call heltable([8,24,192], n3(:,373), t3x192(:,:,181))
  call heltable([12,16,192], n3(:,374), t3x192(:,:,182))
  call heltable([12,16,192], n3(:,375), t3x192(:,:,183))
  call heltable([24,8,192], n3(:,376), t3x192(:,:,184))
  call heltable([12,16,192], n3(:,377), t3x192(:,:,185))
  call heltable([12,16,192], n3(:,378), t3x192(:,:,186))
  call heltable([12,16,192], n3(:,379), t3x192(:,:,187))
  call heltable([8,24,192], n3(:,380), t3x192(:,:,188))
  call heltable([12,16,192], n3(:,381), t3x192(:,:,189))
  call heltable([24,8,192], n3(:,382), t3x192(:,:,190))
  call heltable([12,16,192], n3(:,383), t3x192(:,:,191))
  call heltable([12,16,192], n3(:,384), t3x192(:,:,192))
  call heltable([8,24,192], n3(:,385), t3x192(:,:,193))
  call heltable([24,8,192], n3(:,386), t3x192(:,:,194))
  call heltable([8,24,192], n3(:,387), t3x192(:,:,195))
  call heltable([24,8,192], n3(:,388), t3x192(:,:,196))
  call heltable([12,16,192], n3(:,389), t3x192(:,:,197))
  call heltable([12,16,192], n3(:,390), t3x192(:,:,198))
  call heltable([12,16,192], n3(:,391), t3x192(:,:,199))
  call heltable([12,16,192], n3(:,392), t3x192(:,:,200))
  call heltable([8,24,192], n3(:,393), t3x192(:,:,201))
  call heltable([12,16,192], n3(:,394), t3x192(:,:,202))
  call heltable([12,16,192], n3(:,395), t3x192(:,:,203))
  call heltable([8,24,192], n3(:,396), t3x192(:,:,204))
  call heltable([8,24,192], n3(:,397), t3x192(:,:,205))
  call heltable([12,16,192], n3(:,398), t3x192(:,:,206))
  call heltable([8,24,192], n3(:,399), t3x192(:,:,207))
  call heltable([12,16,192], n3(:,400), t3x192(:,:,208))
  call heltable([12,16,192], n3(:,401), t3x192(:,:,209))
  call heltable([12,16,192], n3(:,402), t3x192(:,:,210))
  call heltable([8,24,192], n3(:,403), t3x192(:,:,211))
  call heltable([8,24,192], n3(:,404), t3x192(:,:,212))
  call heltable([12,16,192], n3(:,405), t3x192(:,:,213))
  call heltable([8,24,192], n3(:,406), t3x192(:,:,214))
  call heltable([12,16,192], n3(:,407), t3x192(:,:,215))
  call heltable([8,24,192], n3(:,408), t3x192(:,:,216))
  call heltable([8,24,192], n3(:,409), t3x192(:,:,217))
  call heltable([8,24,192], n3(:,410), t3x192(:,:,218))
  call heltable([12,16,192], n3(:,411), t3x192(:,:,219))
  call heltable([12,16,192], n3(:,412), t3x192(:,:,220))
  call heltable([8,24,192], n3(:,413), t3x192(:,:,221))
  call heltable([12,16,192], n3(:,414), t3x192(:,:,222))
  call heltable([8,24,192], n3(:,415), t3x192(:,:,223))
  call heltable([12,16,192], n3(:,416), t3x192(:,:,224))
  call heltable([8,24,192], n3(:,417), t3x192(:,:,225))
  call heltable([12,16,192], n3(:,418), t3x192(:,:,226))
  call heltable([12,16,192], n3(:,419), t3x192(:,:,227))
  call heltable([8,24,192], n3(:,420), t3x192(:,:,228))
  call heltable([12,16,192], n3(:,421), t3x192(:,:,229))
  call heltable([8,24,192], n3(:,422), t3x192(:,:,230))
  call heltable([12,16,192], n3(:,423), t3x192(:,:,231))
  call heltable([8,24,192], n3(:,424), t3x192(:,:,232))
  call heltable([8,24,192], n3(:,425), t3x192(:,:,233))
  call heltable([12,16,192], n3(:,426), t3x192(:,:,234))
  call heltable([12,16,192], n3(:,427), t3x192(:,:,235))
  call heltable([8,24,192], n3(:,428), t3x192(:,:,236))
  call heltable([8,24,192], n3(:,429), t3x192(:,:,237))
  call heltable([12,16,192], n3(:,430), t3x192(:,:,238))
  call heltable([8,24,192], n3(:,431), t3x192(:,:,239))
  call heltable([12,16,192], n3(:,432), t3x192(:,:,240))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppvvvj_bbbxbxaaz_1
