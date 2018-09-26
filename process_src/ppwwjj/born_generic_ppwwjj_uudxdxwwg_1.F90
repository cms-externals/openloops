
module ol_external_ppwwjj_uudxdxwwg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppwwjj_uudxdxwwg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_ppwwjj_uudxdxwwg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_ppwwjj_uudxdxwwg_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppwwjj_uudxdxwwg_1(7) = &
                     [ 1, 1, 2, 2, 3, 3, 4 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppwwjj_uudxdxwwg_1(7) = &
                     [ 6, 6, 6, 6, 3, 3, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppwwjj_uudxdxwwg_1 = &
                     144
  integer, save :: channel_number_ppwwjj_uudxdxwwg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,288) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(288,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppwwjj_uudxdxwwg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppwwjj_uudxdxwwg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppwwjj_uudxdxwwg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppwwjj_uudxdxwwg_1(7)
    external_perm_ppwwjj_uudxdxwwg_1 = perm
    do i = 1, 7
      external_perm_inv_ppwwjj_uudxdxwwg_1( &
        external_perm_ppwwjj_uudxdxwwg_1(i)) = i
      particle_types_perm_ppwwjj_uudxdxwwg_1(i) = &
        particle_types_ppwwjj_uudxdxwwg_1( &
        external_perm_ppwwjj_uudxdxwwg_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_ppwwjj_uudxdxwwg_1(i) >= &
          external_perm_ppwwjj_uudxdxwwg_1(j)) then
          ii = external_perm_ppwwjj_uudxdxwwg_1(i)
          jj = external_perm_ppwwjj_uudxdxwwg_1(j)
        else
          ii = external_perm_ppwwjj_uudxdxwwg_1(j)
          jj = external_perm_ppwwjj_uudxdxwwg_1(i)
        end if
        extcomb_perm_ppwwjj_uudxdxwwg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppwwjj_uudxdxwwg_1 = &
      average_factors_ppwwjj_uudxdxwwg_1( &
      external_perm_ppwwjj_uudxdxwwg_1(1)) &
      * average_factors_ppwwjj_uudxdxwwg_1( &
      external_perm_ppwwjj_uudxdxwwg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_ppwwjj_uudxdxwwg_1 = &
          average_factor_ppwwjj_uudxdxwwg_1 &
          * factorial(count(particle_types_perm_ppwwjj_uudxdxwwg_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppwwjj_uudxdxwwg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppwwjj_uudxdxwwg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rMW_unscaled, rMW_unscaled, rZERO ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_ppwwjj_uudxdxwwg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppwwjj_uudxdxwwg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppwwjj_uudxdxwwg_1")
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
      & bind(c,name="ol_rambo_ppwwjj_uudxdxwwg_1")
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
  H(:,  3) = [ -1, -1, -1, -1, -1,  0, -1 ]
  H(:,  4) = [ -1, -1, -1, -1, -1,  0,  1 ]
  H(:,  5) = [ -1, -1, -1, -1, -1,  1, -1 ]
  H(:,  6) = [ -1, -1, -1, -1, -1,  1,  1 ]
  H(:,  7) = [ -1, -1, -1, -1,  0, -1, -1 ]
  H(:,  8) = [ -1, -1, -1, -1,  0, -1,  1 ]
  H(:,  9) = [ -1, -1, -1, -1,  0,  0, -1 ]
  H(:, 10) = [ -1, -1, -1, -1,  0,  0,  1 ]
  H(:, 11) = [ -1, -1, -1, -1,  0,  1, -1 ]
  H(:, 12) = [ -1, -1, -1, -1,  0,  1,  1 ]
  H(:, 13) = [ -1, -1, -1, -1,  1, -1, -1 ]
  H(:, 14) = [ -1, -1, -1, -1,  1, -1,  1 ]
  H(:, 15) = [ -1, -1, -1, -1,  1,  0, -1 ]
  H(:, 16) = [ -1, -1, -1, -1,  1,  0,  1 ]
  H(:, 17) = [ -1, -1, -1, -1,  1,  1, -1 ]
  H(:, 18) = [ -1, -1, -1, -1,  1,  1,  1 ]
  H(:, 19) = [ -1, -1, -1,  1, -1, -1, -1 ]
  H(:, 20) = [ -1, -1, -1,  1, -1, -1,  1 ]
  H(:, 21) = [ -1, -1, -1,  1, -1,  0, -1 ]
  H(:, 22) = [ -1, -1, -1,  1, -1,  0,  1 ]
  H(:, 23) = [ -1, -1, -1,  1, -1,  1, -1 ]
  H(:, 24) = [ -1, -1, -1,  1, -1,  1,  1 ]
  H(:, 25) = [ -1, -1, -1,  1,  0, -1, -1 ]
  H(:, 26) = [ -1, -1, -1,  1,  0, -1,  1 ]
  H(:, 27) = [ -1, -1, -1,  1,  0,  0, -1 ]
  H(:, 28) = [ -1, -1, -1,  1,  0,  0,  1 ]
  H(:, 29) = [ -1, -1, -1,  1,  0,  1, -1 ]
  H(:, 30) = [ -1, -1, -1,  1,  0,  1,  1 ]
  H(:, 31) = [ -1, -1, -1,  1,  1, -1, -1 ]
  H(:, 32) = [ -1, -1, -1,  1,  1, -1,  1 ]
  H(:, 33) = [ -1, -1, -1,  1,  1,  0, -1 ]
  H(:, 34) = [ -1, -1, -1,  1,  1,  0,  1 ]
  H(:, 35) = [ -1, -1, -1,  1,  1,  1, -1 ]
  H(:, 36) = [ -1, -1, -1,  1,  1,  1,  1 ]
  H(:, 37) = [ -1, -1,  1, -1, -1, -1, -1 ]
  H(:, 38) = [ -1, -1,  1, -1, -1, -1,  1 ]
  H(:, 39) = [ -1, -1,  1, -1, -1,  0, -1 ]
  H(:, 40) = [ -1, -1,  1, -1, -1,  0,  1 ]
  H(:, 41) = [ -1, -1,  1, -1, -1,  1, -1 ]
  H(:, 42) = [ -1, -1,  1, -1, -1,  1,  1 ]
  H(:, 43) = [ -1, -1,  1, -1,  0, -1, -1 ]
  H(:, 44) = [ -1, -1,  1, -1,  0, -1,  1 ]
  H(:, 45) = [ -1, -1,  1, -1,  0,  0, -1 ]
  H(:, 46) = [ -1, -1,  1, -1,  0,  0,  1 ]
  H(:, 47) = [ -1, -1,  1, -1,  0,  1, -1 ]
  H(:, 48) = [ -1, -1,  1, -1,  0,  1,  1 ]
  H(:, 49) = [ -1, -1,  1, -1,  1, -1, -1 ]
  H(:, 50) = [ -1, -1,  1, -1,  1, -1,  1 ]
  H(:, 51) = [ -1, -1,  1, -1,  1,  0, -1 ]
  H(:, 52) = [ -1, -1,  1, -1,  1,  0,  1 ]
  H(:, 53) = [ -1, -1,  1, -1,  1,  1, -1 ]
  H(:, 54) = [ -1, -1,  1, -1,  1,  1,  1 ]
  H(:, 55) = [ -1, -1,  1,  1, -1, -1, -1 ]
  H(:, 56) = [ -1, -1,  1,  1, -1, -1,  1 ]
  H(:, 57) = [ -1, -1,  1,  1, -1,  0, -1 ]
  H(:, 58) = [ -1, -1,  1,  1, -1,  0,  1 ]
  H(:, 59) = [ -1, -1,  1,  1, -1,  1, -1 ]
  H(:, 60) = [ -1, -1,  1,  1, -1,  1,  1 ]
  H(:, 61) = [ -1, -1,  1,  1,  0, -1, -1 ]
  H(:, 62) = [ -1, -1,  1,  1,  0, -1,  1 ]
  H(:, 63) = [ -1, -1,  1,  1,  0,  0, -1 ]
  H(:, 64) = [ -1, -1,  1,  1,  0,  0,  1 ]
  H(:, 65) = [ -1, -1,  1,  1,  0,  1, -1 ]
  H(:, 66) = [ -1, -1,  1,  1,  0,  1,  1 ]
  H(:, 67) = [ -1, -1,  1,  1,  1, -1, -1 ]
  H(:, 68) = [ -1, -1,  1,  1,  1, -1,  1 ]
  H(:, 69) = [ -1, -1,  1,  1,  1,  0, -1 ]
  H(:, 70) = [ -1, -1,  1,  1,  1,  0,  1 ]
  H(:, 71) = [ -1, -1,  1,  1,  1,  1, -1 ]
  H(:, 72) = [ -1, -1,  1,  1,  1,  1,  1 ]
  H(:, 73) = [ -1,  1, -1, -1, -1, -1, -1 ]
  H(:, 74) = [ -1,  1, -1, -1, -1, -1,  1 ]
  H(:, 75) = [ -1,  1, -1, -1, -1,  0, -1 ]
  H(:, 76) = [ -1,  1, -1, -1, -1,  0,  1 ]
  H(:, 77) = [ -1,  1, -1, -1, -1,  1, -1 ]
  H(:, 78) = [ -1,  1, -1, -1, -1,  1,  1 ]
  H(:, 79) = [ -1,  1, -1, -1,  0, -1, -1 ]
  H(:, 80) = [ -1,  1, -1, -1,  0, -1,  1 ]
  H(:, 81) = [ -1,  1, -1, -1,  0,  0, -1 ]
  H(:, 82) = [ -1,  1, -1, -1,  0,  0,  1 ]
  H(:, 83) = [ -1,  1, -1, -1,  0,  1, -1 ]
  H(:, 84) = [ -1,  1, -1, -1,  0,  1,  1 ]
  H(:, 85) = [ -1,  1, -1, -1,  1, -1, -1 ]
  H(:, 86) = [ -1,  1, -1, -1,  1, -1,  1 ]
  H(:, 87) = [ -1,  1, -1, -1,  1,  0, -1 ]
  H(:, 88) = [ -1,  1, -1, -1,  1,  0,  1 ]
  H(:, 89) = [ -1,  1, -1, -1,  1,  1, -1 ]
  H(:, 90) = [ -1,  1, -1, -1,  1,  1,  1 ]
  H(:, 91) = [ -1,  1, -1,  1, -1, -1, -1 ]
  H(:, 92) = [ -1,  1, -1,  1, -1, -1,  1 ]
  H(:, 93) = [ -1,  1, -1,  1, -1,  0, -1 ]
  H(:, 94) = [ -1,  1, -1,  1, -1,  0,  1 ]
  H(:, 95) = [ -1,  1, -1,  1, -1,  1, -1 ]
  H(:, 96) = [ -1,  1, -1,  1, -1,  1,  1 ]
  H(:, 97) = [ -1,  1, -1,  1,  0, -1, -1 ]
  H(:, 98) = [ -1,  1, -1,  1,  0, -1,  1 ]
  H(:, 99) = [ -1,  1, -1,  1,  0,  0, -1 ]
  H(:,100) = [ -1,  1, -1,  1,  0,  0,  1 ]
  H(:,101) = [ -1,  1, -1,  1,  0,  1, -1 ]
  H(:,102) = [ -1,  1, -1,  1,  0,  1,  1 ]
  H(:,103) = [ -1,  1, -1,  1,  1, -1, -1 ]
  H(:,104) = [ -1,  1, -1,  1,  1, -1,  1 ]
  H(:,105) = [ -1,  1, -1,  1,  1,  0, -1 ]
  H(:,106) = [ -1,  1, -1,  1,  1,  0,  1 ]
  H(:,107) = [ -1,  1, -1,  1,  1,  1, -1 ]
  H(:,108) = [ -1,  1, -1,  1,  1,  1,  1 ]
  H(:,109) = [ -1,  1,  1, -1, -1, -1, -1 ]
  H(:,110) = [ -1,  1,  1, -1, -1, -1,  1 ]
  H(:,111) = [ -1,  1,  1, -1, -1,  0, -1 ]
  H(:,112) = [ -1,  1,  1, -1, -1,  0,  1 ]
  H(:,113) = [ -1,  1,  1, -1, -1,  1, -1 ]
  H(:,114) = [ -1,  1,  1, -1, -1,  1,  1 ]
  H(:,115) = [ -1,  1,  1, -1,  0, -1, -1 ]
  H(:,116) = [ -1,  1,  1, -1,  0, -1,  1 ]
  H(:,117) = [ -1,  1,  1, -1,  0,  0, -1 ]
  H(:,118) = [ -1,  1,  1, -1,  0,  0,  1 ]
  H(:,119) = [ -1,  1,  1, -1,  0,  1, -1 ]
  H(:,120) = [ -1,  1,  1, -1,  0,  1,  1 ]
  H(:,121) = [ -1,  1,  1, -1,  1, -1, -1 ]
  H(:,122) = [ -1,  1,  1, -1,  1, -1,  1 ]
  H(:,123) = [ -1,  1,  1, -1,  1,  0, -1 ]
  H(:,124) = [ -1,  1,  1, -1,  1,  0,  1 ]
  H(:,125) = [ -1,  1,  1, -1,  1,  1, -1 ]
  H(:,126) = [ -1,  1,  1, -1,  1,  1,  1 ]
  H(:,127) = [ -1,  1,  1,  1, -1, -1, -1 ]
  H(:,128) = [ -1,  1,  1,  1, -1, -1,  1 ]
  H(:,129) = [ -1,  1,  1,  1, -1,  0, -1 ]
  H(:,130) = [ -1,  1,  1,  1, -1,  0,  1 ]
  H(:,131) = [ -1,  1,  1,  1, -1,  1, -1 ]
  H(:,132) = [ -1,  1,  1,  1, -1,  1,  1 ]
  H(:,133) = [ -1,  1,  1,  1,  0, -1, -1 ]
  H(:,134) = [ -1,  1,  1,  1,  0, -1,  1 ]
  H(:,135) = [ -1,  1,  1,  1,  0,  0, -1 ]
  H(:,136) = [ -1,  1,  1,  1,  0,  0,  1 ]
  H(:,137) = [ -1,  1,  1,  1,  0,  1, -1 ]
  H(:,138) = [ -1,  1,  1,  1,  0,  1,  1 ]
  H(:,139) = [ -1,  1,  1,  1,  1, -1, -1 ]
  H(:,140) = [ -1,  1,  1,  1,  1, -1,  1 ]
  H(:,141) = [ -1,  1,  1,  1,  1,  0, -1 ]
  H(:,142) = [ -1,  1,  1,  1,  1,  0,  1 ]
  H(:,143) = [ -1,  1,  1,  1,  1,  1, -1 ]
  H(:,144) = [ -1,  1,  1,  1,  1,  1,  1 ]
  H(:,145) = [  1, -1, -1, -1, -1, -1, -1 ]
  H(:,146) = [  1, -1, -1, -1, -1, -1,  1 ]
  H(:,147) = [  1, -1, -1, -1, -1,  0, -1 ]
  H(:,148) = [  1, -1, -1, -1, -1,  0,  1 ]
  H(:,149) = [  1, -1, -1, -1, -1,  1, -1 ]
  H(:,150) = [  1, -1, -1, -1, -1,  1,  1 ]
  H(:,151) = [  1, -1, -1, -1,  0, -1, -1 ]
  H(:,152) = [  1, -1, -1, -1,  0, -1,  1 ]
  H(:,153) = [  1, -1, -1, -1,  0,  0, -1 ]
  H(:,154) = [  1, -1, -1, -1,  0,  0,  1 ]
  H(:,155) = [  1, -1, -1, -1,  0,  1, -1 ]
  H(:,156) = [  1, -1, -1, -1,  0,  1,  1 ]
  H(:,157) = [  1, -1, -1, -1,  1, -1, -1 ]
  H(:,158) = [  1, -1, -1, -1,  1, -1,  1 ]
  H(:,159) = [  1, -1, -1, -1,  1,  0, -1 ]
  H(:,160) = [  1, -1, -1, -1,  1,  0,  1 ]
  H(:,161) = [  1, -1, -1, -1,  1,  1, -1 ]
  H(:,162) = [  1, -1, -1, -1,  1,  1,  1 ]
  H(:,163) = [  1, -1, -1,  1, -1, -1, -1 ]
  H(:,164) = [  1, -1, -1,  1, -1, -1,  1 ]
  H(:,165) = [  1, -1, -1,  1, -1,  0, -1 ]
  H(:,166) = [  1, -1, -1,  1, -1,  0,  1 ]
  H(:,167) = [  1, -1, -1,  1, -1,  1, -1 ]
  H(:,168) = [  1, -1, -1,  1, -1,  1,  1 ]
  H(:,169) = [  1, -1, -1,  1,  0, -1, -1 ]
  H(:,170) = [  1, -1, -1,  1,  0, -1,  1 ]
  H(:,171) = [  1, -1, -1,  1,  0,  0, -1 ]
  H(:,172) = [  1, -1, -1,  1,  0,  0,  1 ]
  H(:,173) = [  1, -1, -1,  1,  0,  1, -1 ]
  H(:,174) = [  1, -1, -1,  1,  0,  1,  1 ]
  H(:,175) = [  1, -1, -1,  1,  1, -1, -1 ]
  H(:,176) = [  1, -1, -1,  1,  1, -1,  1 ]
  H(:,177) = [  1, -1, -1,  1,  1,  0, -1 ]
  H(:,178) = [  1, -1, -1,  1,  1,  0,  1 ]
  H(:,179) = [  1, -1, -1,  1,  1,  1, -1 ]
  H(:,180) = [  1, -1, -1,  1,  1,  1,  1 ]
  H(:,181) = [  1, -1,  1, -1, -1, -1, -1 ]
  H(:,182) = [  1, -1,  1, -1, -1, -1,  1 ]
  H(:,183) = [  1, -1,  1, -1, -1,  0, -1 ]
  H(:,184) = [  1, -1,  1, -1, -1,  0,  1 ]
  H(:,185) = [  1, -1,  1, -1, -1,  1, -1 ]
  H(:,186) = [  1, -1,  1, -1, -1,  1,  1 ]
  H(:,187) = [  1, -1,  1, -1,  0, -1, -1 ]
  H(:,188) = [  1, -1,  1, -1,  0, -1,  1 ]
  H(:,189) = [  1, -1,  1, -1,  0,  0, -1 ]
  H(:,190) = [  1, -1,  1, -1,  0,  0,  1 ]
  H(:,191) = [  1, -1,  1, -1,  0,  1, -1 ]
  H(:,192) = [  1, -1,  1, -1,  0,  1,  1 ]
  H(:,193) = [  1, -1,  1, -1,  1, -1, -1 ]
  H(:,194) = [  1, -1,  1, -1,  1, -1,  1 ]
  H(:,195) = [  1, -1,  1, -1,  1,  0, -1 ]
  H(:,196) = [  1, -1,  1, -1,  1,  0,  1 ]
  H(:,197) = [  1, -1,  1, -1,  1,  1, -1 ]
  H(:,198) = [  1, -1,  1, -1,  1,  1,  1 ]
  H(:,199) = [  1, -1,  1,  1, -1, -1, -1 ]
  H(:,200) = [  1, -1,  1,  1, -1, -1,  1 ]
  H(:,201) = [  1, -1,  1,  1, -1,  0, -1 ]
  H(:,202) = [  1, -1,  1,  1, -1,  0,  1 ]
  H(:,203) = [  1, -1,  1,  1, -1,  1, -1 ]
  H(:,204) = [  1, -1,  1,  1, -1,  1,  1 ]
  H(:,205) = [  1, -1,  1,  1,  0, -1, -1 ]
  H(:,206) = [  1, -1,  1,  1,  0, -1,  1 ]
  H(:,207) = [  1, -1,  1,  1,  0,  0, -1 ]
  H(:,208) = [  1, -1,  1,  1,  0,  0,  1 ]
  H(:,209) = [  1, -1,  1,  1,  0,  1, -1 ]
  H(:,210) = [  1, -1,  1,  1,  0,  1,  1 ]
  H(:,211) = [  1, -1,  1,  1,  1, -1, -1 ]
  H(:,212) = [  1, -1,  1,  1,  1, -1,  1 ]
  H(:,213) = [  1, -1,  1,  1,  1,  0, -1 ]
  H(:,214) = [  1, -1,  1,  1,  1,  0,  1 ]
  H(:,215) = [  1, -1,  1,  1,  1,  1, -1 ]
  H(:,216) = [  1, -1,  1,  1,  1,  1,  1 ]
  H(:,217) = [  1,  1, -1, -1, -1, -1, -1 ]
  H(:,218) = [  1,  1, -1, -1, -1, -1,  1 ]
  H(:,219) = [  1,  1, -1, -1, -1,  0, -1 ]
  H(:,220) = [  1,  1, -1, -1, -1,  0,  1 ]
  H(:,221) = [  1,  1, -1, -1, -1,  1, -1 ]
  H(:,222) = [  1,  1, -1, -1, -1,  1,  1 ]
  H(:,223) = [  1,  1, -1, -1,  0, -1, -1 ]
  H(:,224) = [  1,  1, -1, -1,  0, -1,  1 ]
  H(:,225) = [  1,  1, -1, -1,  0,  0, -1 ]
  H(:,226) = [  1,  1, -1, -1,  0,  0,  1 ]
  H(:,227) = [  1,  1, -1, -1,  0,  1, -1 ]
  H(:,228) = [  1,  1, -1, -1,  0,  1,  1 ]
  H(:,229) = [  1,  1, -1, -1,  1, -1, -1 ]
  H(:,230) = [  1,  1, -1, -1,  1, -1,  1 ]
  H(:,231) = [  1,  1, -1, -1,  1,  0, -1 ]
  H(:,232) = [  1,  1, -1, -1,  1,  0,  1 ]
  H(:,233) = [  1,  1, -1, -1,  1,  1, -1 ]
  H(:,234) = [  1,  1, -1, -1,  1,  1,  1 ]
  H(:,235) = [  1,  1, -1,  1, -1, -1, -1 ]
  H(:,236) = [  1,  1, -1,  1, -1, -1,  1 ]
  H(:,237) = [  1,  1, -1,  1, -1,  0, -1 ]
  H(:,238) = [  1,  1, -1,  1, -1,  0,  1 ]
  H(:,239) = [  1,  1, -1,  1, -1,  1, -1 ]
  H(:,240) = [  1,  1, -1,  1, -1,  1,  1 ]
  H(:,241) = [  1,  1, -1,  1,  0, -1, -1 ]
  H(:,242) = [  1,  1, -1,  1,  0, -1,  1 ]
  H(:,243) = [  1,  1, -1,  1,  0,  0, -1 ]
  H(:,244) = [  1,  1, -1,  1,  0,  0,  1 ]
  H(:,245) = [  1,  1, -1,  1,  0,  1, -1 ]
  H(:,246) = [  1,  1, -1,  1,  0,  1,  1 ]
  H(:,247) = [  1,  1, -1,  1,  1, -1, -1 ]
  H(:,248) = [  1,  1, -1,  1,  1, -1,  1 ]
  H(:,249) = [  1,  1, -1,  1,  1,  0, -1 ]
  H(:,250) = [  1,  1, -1,  1,  1,  0,  1 ]
  H(:,251) = [  1,  1, -1,  1,  1,  1, -1 ]
  H(:,252) = [  1,  1, -1,  1,  1,  1,  1 ]
  H(:,253) = [  1,  1,  1, -1, -1, -1, -1 ]
  H(:,254) = [  1,  1,  1, -1, -1, -1,  1 ]
  H(:,255) = [  1,  1,  1, -1, -1,  0, -1 ]
  H(:,256) = [  1,  1,  1, -1, -1,  0,  1 ]
  H(:,257) = [  1,  1,  1, -1, -1,  1, -1 ]
  H(:,258) = [  1,  1,  1, -1, -1,  1,  1 ]
  H(:,259) = [  1,  1,  1, -1,  0, -1, -1 ]
  H(:,260) = [  1,  1,  1, -1,  0, -1,  1 ]
  H(:,261) = [  1,  1,  1, -1,  0,  0, -1 ]
  H(:,262) = [  1,  1,  1, -1,  0,  0,  1 ]
  H(:,263) = [  1,  1,  1, -1,  0,  1, -1 ]
  H(:,264) = [  1,  1,  1, -1,  0,  1,  1 ]
  H(:,265) = [  1,  1,  1, -1,  1, -1, -1 ]
  H(:,266) = [  1,  1,  1, -1,  1, -1,  1 ]
  H(:,267) = [  1,  1,  1, -1,  1,  0, -1 ]
  H(:,268) = [  1,  1,  1, -1,  1,  0,  1 ]
  H(:,269) = [  1,  1,  1, -1,  1,  1, -1 ]
  H(:,270) = [  1,  1,  1, -1,  1,  1,  1 ]
  H(:,271) = [  1,  1,  1,  1, -1, -1, -1 ]
  H(:,272) = [  1,  1,  1,  1, -1, -1,  1 ]
  H(:,273) = [  1,  1,  1,  1, -1,  0, -1 ]
  H(:,274) = [  1,  1,  1,  1, -1,  0,  1 ]
  H(:,275) = [  1,  1,  1,  1, -1,  1, -1 ]
  H(:,276) = [  1,  1,  1,  1, -1,  1,  1 ]
  H(:,277) = [  1,  1,  1,  1,  0, -1, -1 ]
  H(:,278) = [  1,  1,  1,  1,  0, -1,  1 ]
  H(:,279) = [  1,  1,  1,  1,  0,  0, -1 ]
  H(:,280) = [  1,  1,  1,  1,  0,  0,  1 ]
  H(:,281) = [  1,  1,  1,  1,  0,  1, -1 ]
  H(:,282) = [  1,  1,  1,  1,  0,  1,  1 ]
  H(:,283) = [  1,  1,  1,  1,  1, -1, -1 ]
  H(:,284) = [  1,  1,  1,  1,  1, -1,  1 ]
  H(:,285) = [  1,  1,  1,  1,  1,  0, -1 ]
  H(:,286) = [  1,  1,  1,  1,  1,  0,  1 ]
  H(:,287) = [  1,  1,  1,  1,  1,  1, -1 ]
  H(:,288) = [  1,  1,  1,  1,  1,  1,  1 ]

  H_HC(:,7) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 288/1/2) ]
  end subroutine hel_init

end module ol_external_ppwwjj_uudxdxwwg_1


module colour_basis_ppwwjj_uudxdxwwg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,0,0,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppwwjj_uudxdxwwg_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(7)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 4
    ncoupl = 1
    maxpows = 1
    nhel = 288
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_ppwwjj_uudxdxwwg_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,4)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([4], [1,1])
#endif
#if 4 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [1,110,2,1,112,2,1,116,2,1,118,2], &
      [3,4])
#endif
  end subroutine tree_colbasis

end module colour_basis_ppwwjj_uudxdxwwg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppwwjj_uudxdxwwg_1(perm)
  use ol_external_ppwwjj_uudxdxwwg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_ppwwjj_uudxdxwwg_1

! **********************************************************************
module ol_heltables_ppwwjj_uudxdxwwg_1
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
    H5(3) = [-1,0,1], &
    H6(3) = [-1,0,1], &
    H7(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(28), n3(3,216)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,4), t3x6(2,6,8), t3x12(2,12,32), t3x24(2,24,60), t3x288(2,288,112)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(288,7)
  integer,           save :: exthel(288,7)
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
  call heltable([3,2,6], n3(:,1), t3x6(:,:,1))
  call heltable([3,2,6], n3(:,2), t3x6(:,:,2))
  call heltable([2,2,4], n3(:,3), t3x4(:,:,1))
  n2(1) = 6
  n2(2) = 6
  n2(3) = 4
  call heltable([6,2,12], n3(:,4), t3x12(:,:,1))
  call heltable([6,4,24], n3(:,5), t3x24(:,:,1))
  call heltable([6,2,12], n3(:,6), t3x12(:,:,2))
  call heltable([6,4,24], n3(:,7), t3x24(:,:,2))
  call heltable([2,2,4], n3(:,8), t3x4(:,:,2))
  n2(4) = 4
  call heltable([6,2,12], n3(:,9), t3x12(:,:,3))
  call heltable([6,4,24], n3(:,10), t3x24(:,:,3))
  call heltable([6,2,12], n3(:,11), t3x12(:,:,4))
  call heltable([6,4,24], n3(:,12), t3x24(:,:,4))
  call heltable([12,2,24], n3(:,13), t3x24(:,:,5))
  call heltable([2,6,12], n3(:,14), t3x12(:,:,5))
  n2(5) = 12
  call heltable([2,12,24], n3(:,15), t3x24(:,:,6))
  call heltable([12,2,24], n3(:,16), t3x24(:,:,7))
  call heltable([2,12,24], n3(:,17), t3x24(:,:,8))
  call heltable([2,6,12], n3(:,18), t3x12(:,:,6))
  n2(6) = 12
  call heltable([2,12,24], n3(:,19), t3x24(:,:,9))
  call heltable([2,12,24], n3(:,20), t3x24(:,:,10))
  call heltable([2,2,4], n3(:,21), t3x4(:,:,3))
  call heltable([2,3,6], n3(:,22), t3x6(:,:,3))
  n2(7) = 4
  n2(8) = 6
  call heltable([4,6,24], n3(:,23), t3x24(:,:,11))
  call heltable([2,6,12], n3(:,24), t3x12(:,:,7))
  call heltable([12,2,24], n3(:,25), t3x24(:,:,12))
  call heltable([6,2,12], n3(:,26), t3x12(:,:,8))
  n2(9) = 12
  call heltable([12,2,24], n3(:,27), t3x24(:,:,13))
  call heltable([2,12,24], n3(:,28), t3x24(:,:,14))
  call heltable([2,3,6], n3(:,29), t3x6(:,:,4))
  n2(10) = 6
  call heltable([4,6,24], n3(:,30), t3x24(:,:,15))
  call heltable([2,6,12], n3(:,31), t3x12(:,:,9))
  call heltable([6,2,12], n3(:,32), t3x12(:,:,10))
  n2(11) = 12
  call heltable([12,2,24], n3(:,33), t3x24(:,:,16))
  call heltable([12,2,24], n3(:,34), t3x24(:,:,17))
  call heltable([2,12,24], n3(:,35), t3x24(:,:,18))
  call heltable([3,4,12], n3(:,36), t3x12(:,:,11))
  n2(12) = 12
  call heltable([4,3,12], n3(:,37), t3x12(:,:,12))
  n2(13) = 12
  call heltable([4,3,12], n3(:,38), t3x12(:,:,13))
  n2(14) = 12
  call heltable([3,2,6], n3(:,39), t3x6(:,:,5))
  call heltable([3,2,6], n3(:,40), t3x6(:,:,6))
  n2(15) = 6
  n2(16) = 6
  call heltable([6,2,12], n3(:,41), t3x12(:,:,14))
  call heltable([6,4,24], n3(:,42), t3x24(:,:,19))
  call heltable([6,2,12], n3(:,43), t3x12(:,:,15))
  call heltable([6,4,24], n3(:,44), t3x24(:,:,20))
  call heltable([6,2,12], n3(:,45), t3x12(:,:,16))
  call heltable([6,4,24], n3(:,46), t3x24(:,:,21))
  call heltable([6,2,12], n3(:,47), t3x12(:,:,17))
  call heltable([6,4,24], n3(:,48), t3x24(:,:,22))
  call heltable([12,2,24], n3(:,49), t3x24(:,:,23))
  call heltable([2,6,12], n3(:,50), t3x12(:,:,18))
  n2(17) = 12
  call heltable([2,12,24], n3(:,51), t3x24(:,:,24))
  call heltable([12,2,24], n3(:,52), t3x24(:,:,25))
  call heltable([2,12,24], n3(:,53), t3x24(:,:,26))
  call heltable([2,6,12], n3(:,54), t3x12(:,:,19))
  n2(18) = 12
  call heltable([2,12,24], n3(:,55), t3x24(:,:,27))
  call heltable([2,12,24], n3(:,56), t3x24(:,:,28))
  call heltable([2,2,4], n3(:,57), t3x4(:,:,4))
  n2(19) = 4
  call heltable([4,6,24], n3(:,58), t3x24(:,:,29))
  call heltable([2,6,12], n3(:,59), t3x12(:,:,20))
  call heltable([2,12,24], n3(:,60), t3x24(:,:,30))
  call heltable([12,2,24], n3(:,61), t3x24(:,:,31))
  call heltable([12,2,24], n3(:,62), t3x24(:,:,32))
  call heltable([4,6,24], n3(:,63), t3x24(:,:,33))
  call heltable([2,6,12], n3(:,64), t3x12(:,:,21))
  call heltable([2,12,24], n3(:,65), t3x24(:,:,34))
  call heltable([12,2,24], n3(:,66), t3x24(:,:,35))
  call heltable([12,2,24], n3(:,67), t3x24(:,:,36))
  call heltable([3,4,12], n3(:,68), t3x12(:,:,22))
  n2(20) = 12
  call heltable([2,3,6], n3(:,69), t3x6(:,:,7))
  n2(21) = 6
  call heltable([4,6,24], n3(:,70), t3x24(:,:,37))
  call heltable([2,6,12], n3(:,71), t3x12(:,:,23))
  call heltable([12,2,24], n3(:,72), t3x24(:,:,38))
  call heltable([6,2,12], n3(:,73), t3x12(:,:,24))
  n2(22) = 12
  call heltable([12,2,24], n3(:,74), t3x24(:,:,39))
  call heltable([2,12,24], n3(:,75), t3x24(:,:,40))
  call heltable([4,6,24], n3(:,76), t3x24(:,:,41))
  call heltable([2,6,12], n3(:,77), t3x12(:,:,25))
  call heltable([2,12,24], n3(:,78), t3x24(:,:,42))
  call heltable([12,2,24], n3(:,79), t3x24(:,:,43))
  call heltable([12,2,24], n3(:,80), t3x24(:,:,44))
  call heltable([12,2,24], n3(:,81), t3x24(:,:,45))
  call heltable([12,2,24], n3(:,82), t3x24(:,:,46))
  call heltable([12,2,24], n3(:,83), t3x24(:,:,47))
  call heltable([12,2,24], n3(:,84), t3x24(:,:,48))
  call heltable([2,3,6], n3(:,85), t3x6(:,:,8))
  n2(23) = 6
  call heltable([4,6,24], n3(:,86), t3x24(:,:,49))
  call heltable([2,6,12], n3(:,87), t3x12(:,:,26))
  call heltable([6,2,12], n3(:,88), t3x12(:,:,27))
  n2(24) = 12
  call heltable([12,2,24], n3(:,89), t3x24(:,:,50))
  call heltable([12,2,24], n3(:,90), t3x24(:,:,51))
  call heltable([2,12,24], n3(:,91), t3x24(:,:,52))
  call heltable([4,6,24], n3(:,92), t3x24(:,:,53))
  call heltable([2,6,12], n3(:,93), t3x12(:,:,28))
  call heltable([2,12,24], n3(:,94), t3x24(:,:,54))
  call heltable([12,2,24], n3(:,95), t3x24(:,:,55))
  call heltable([12,2,24], n3(:,96), t3x24(:,:,56))
  call heltable([12,2,24], n3(:,97), t3x24(:,:,57))
  call heltable([12,2,24], n3(:,98), t3x24(:,:,58))
  call heltable([12,2,24], n3(:,99), t3x24(:,:,59))
  call heltable([12,2,24], n3(:,100), t3x24(:,:,60))
  call heltable([3,4,12], n3(:,101), t3x12(:,:,29))
  n2(25) = 12
  call heltable([4,3,12], n3(:,102), t3x12(:,:,30))
  n2(26) = 12
  call heltable([4,3,12], n3(:,103), t3x12(:,:,31))
  n2(27) = 12
  call heltable([3,4,12], n3(:,104), t3x12(:,:,32))
  n2(28) = 12
  call heltable([12,24,288], n3(:,105), t3x288(:,:,1))
  call heltable([12,24,288], n3(:,106), t3x288(:,:,2))
  call heltable([12,24,288], n3(:,107), t3x288(:,:,3))
  call heltable([12,24,288], n3(:,108), t3x288(:,:,4))
  call heltable([12,24,288], n3(:,109), t3x288(:,:,5))
  call heltable([12,24,288], n3(:,110), t3x288(:,:,6))
  call heltable([12,24,288], n3(:,111), t3x288(:,:,7))
  call heltable([12,24,288], n3(:,112), t3x288(:,:,8))
  call heltable([12,24,288], n3(:,113), t3x288(:,:,9))
  call heltable([12,24,288], n3(:,114), t3x288(:,:,10))
  call heltable([12,24,288], n3(:,115), t3x288(:,:,11))
  call heltable([24,12,288], n3(:,116), t3x288(:,:,12))
  call heltable([12,24,288], n3(:,117), t3x288(:,:,13))
  call heltable([12,24,288], n3(:,118), t3x288(:,:,14))
  call heltable([12,24,288], n3(:,119), t3x288(:,:,15))
  call heltable([12,24,288], n3(:,120), t3x288(:,:,16))
  call heltable([24,12,288], n3(:,121), t3x288(:,:,17))
  call heltable([12,24,288], n3(:,122), t3x288(:,:,18))
  call heltable([12,24,288], n3(:,123), t3x288(:,:,19))
  call heltable([12,24,288], n3(:,124), t3x288(:,:,20))
  call heltable([24,12,288], n3(:,125), t3x288(:,:,21))
  call heltable([24,12,288], n3(:,126), t3x288(:,:,22))
  call heltable([24,12,288], n3(:,127), t3x288(:,:,23))
  call heltable([24,12,288], n3(:,128), t3x288(:,:,24))
  call heltable([12,24,288], n3(:,129), t3x288(:,:,25))
  call heltable([12,24,288], n3(:,130), t3x288(:,:,26))
  call heltable([12,24,288], n3(:,131), t3x288(:,:,27))
  call heltable([12,24,288], n3(:,132), t3x288(:,:,28))
  call heltable([12,24,288], n3(:,133), t3x288(:,:,29))
  call heltable([12,24,288], n3(:,134), t3x288(:,:,30))
  call heltable([12,24,288], n3(:,135), t3x288(:,:,31))
  call heltable([12,24,288], n3(:,136), t3x288(:,:,32))
  call heltable([12,24,288], n3(:,137), t3x288(:,:,33))
  call heltable([12,24,288], n3(:,138), t3x288(:,:,34))
  call heltable([12,24,288], n3(:,139), t3x288(:,:,35))
  call heltable([24,12,288], n3(:,140), t3x288(:,:,36))
  call heltable([12,24,288], n3(:,141), t3x288(:,:,37))
  call heltable([12,24,288], n3(:,142), t3x288(:,:,38))
  call heltable([12,24,288], n3(:,143), t3x288(:,:,39))
  call heltable([12,24,288], n3(:,144), t3x288(:,:,40))
  call heltable([24,12,288], n3(:,145), t3x288(:,:,41))
  call heltable([12,24,288], n3(:,146), t3x288(:,:,42))
  call heltable([12,24,288], n3(:,147), t3x288(:,:,43))
  call heltable([12,24,288], n3(:,148), t3x288(:,:,44))
  call heltable([24,12,288], n3(:,149), t3x288(:,:,45))
  call heltable([24,12,288], n3(:,150), t3x288(:,:,46))
  call heltable([12,24,288], n3(:,151), t3x288(:,:,47))
  call heltable([12,24,288], n3(:,152), t3x288(:,:,48))
  call heltable([12,24,288], n3(:,153), t3x288(:,:,49))
  call heltable([24,12,288], n3(:,154), t3x288(:,:,50))
  call heltable([12,24,288], n3(:,155), t3x288(:,:,51))
  call heltable([12,24,288], n3(:,156), t3x288(:,:,52))
  call heltable([12,24,288], n3(:,157), t3x288(:,:,53))
  call heltable([12,24,288], n3(:,158), t3x288(:,:,54))
  call heltable([24,12,288], n3(:,159), t3x288(:,:,55))
  call heltable([12,24,288], n3(:,160), t3x288(:,:,56))
  call heltable([12,24,288], n3(:,161), t3x288(:,:,57))
  call heltable([12,24,288], n3(:,162), t3x288(:,:,58))
  call heltable([24,12,288], n3(:,163), t3x288(:,:,59))
  call heltable([12,24,288], n3(:,164), t3x288(:,:,60))
  call heltable([24,12,288], n3(:,165), t3x288(:,:,61))
  call heltable([12,24,288], n3(:,166), t3x288(:,:,62))
  call heltable([12,24,288], n3(:,167), t3x288(:,:,63))
  call heltable([12,24,288], n3(:,168), t3x288(:,:,64))
  call heltable([12,24,288], n3(:,169), t3x288(:,:,65))
  call heltable([12,24,288], n3(:,170), t3x288(:,:,66))
  call heltable([12,24,288], n3(:,171), t3x288(:,:,67))
  call heltable([24,12,288], n3(:,172), t3x288(:,:,68))
  call heltable([12,24,288], n3(:,173), t3x288(:,:,69))
  call heltable([12,24,288], n3(:,174), t3x288(:,:,70))
  call heltable([12,24,288], n3(:,175), t3x288(:,:,71))
  call heltable([12,24,288], n3(:,176), t3x288(:,:,72))
  call heltable([12,24,288], n3(:,177), t3x288(:,:,73))
  call heltable([24,12,288], n3(:,178), t3x288(:,:,74))
  call heltable([12,24,288], n3(:,179), t3x288(:,:,75))
  call heltable([12,24,288], n3(:,180), t3x288(:,:,76))
  call heltable([12,24,288], n3(:,181), t3x288(:,:,77))
  call heltable([12,24,288], n3(:,182), t3x288(:,:,78))
  call heltable([24,12,288], n3(:,183), t3x288(:,:,79))
  call heltable([12,24,288], n3(:,184), t3x288(:,:,80))
  call heltable([12,24,288], n3(:,185), t3x288(:,:,81))
  call heltable([12,24,288], n3(:,186), t3x288(:,:,82))
  call heltable([12,24,288], n3(:,187), t3x288(:,:,83))
  call heltable([24,12,288], n3(:,188), t3x288(:,:,84))
  call heltable([12,24,288], n3(:,189), t3x288(:,:,85))
  call heltable([24,12,288], n3(:,190), t3x288(:,:,86))
  call heltable([12,24,288], n3(:,191), t3x288(:,:,87))
  call heltable([12,24,288], n3(:,192), t3x288(:,:,88))
  call heltable([12,24,288], n3(:,193), t3x288(:,:,89))
  call heltable([12,24,288], n3(:,194), t3x288(:,:,90))
  call heltable([24,12,288], n3(:,195), t3x288(:,:,91))
  call heltable([12,24,288], n3(:,196), t3x288(:,:,92))
  call heltable([12,24,288], n3(:,197), t3x288(:,:,93))
  call heltable([12,24,288], n3(:,198), t3x288(:,:,94))
  call heltable([12,24,288], n3(:,199), t3x288(:,:,95))
  call heltable([12,24,288], n3(:,200), t3x288(:,:,96))
  call heltable([24,12,288], n3(:,201), t3x288(:,:,97))
  call heltable([24,12,288], n3(:,202), t3x288(:,:,98))
  call heltable([24,12,288], n3(:,203), t3x288(:,:,99))
  call heltable([24,12,288], n3(:,204), t3x288(:,:,100))
  call heltable([24,12,288], n3(:,205), t3x288(:,:,101))
  call heltable([24,12,288], n3(:,206), t3x288(:,:,102))
  call heltable([24,12,288], n3(:,207), t3x288(:,:,103))
  call heltable([24,12,288], n3(:,208), t3x288(:,:,104))
  call heltable([24,12,288], n3(:,209), t3x288(:,:,105))
  call heltable([24,12,288], n3(:,210), t3x288(:,:,106))
  call heltable([24,12,288], n3(:,211), t3x288(:,:,107))
  call heltable([24,12,288], n3(:,212), t3x288(:,:,108))
  call heltable([24,12,288], n3(:,213), t3x288(:,:,109))
  call heltable([24,12,288], n3(:,214), t3x288(:,:,110))
  call heltable([24,12,288], n3(:,215), t3x288(:,:,111))
  call heltable([24,12,288], n3(:,216), t3x288(:,:,112))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppwwjj_uudxdxwwg_1
