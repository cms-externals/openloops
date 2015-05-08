
module ol_external_ppwjjj_uuxcddxsxw_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppwjjj_uuxcddxsxw_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_ppwjjj_uuxcddxsxw_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_ppwjjj_uuxcddxsxw_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppwjjj_uuxcddxsxw_1(7) = &
                     [ 1, 2, 3, 4, 5, 6, 7 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppwjjj_uuxcddxsxw_1(7) = &
                     [ 6, 6, 6, 6, 6, 6, 3 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppwjjj_uuxcddxsxw_1 = &
                     36
  integer, save :: channel_number_ppwjjj_uuxcddxsxw_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,192) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(192,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppwjjj_uuxcddxsxw_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppwjjj_uuxcddxsxw_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppwjjj_uuxcddxsxw_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppwjjj_uuxcddxsxw_1(7)
    external_perm_ppwjjj_uuxcddxsxw_1 = perm
    do i = 1, 7
      external_perm_inv_ppwjjj_uuxcddxsxw_1( &
        external_perm_ppwjjj_uuxcddxsxw_1(i)) = i
      particle_types_perm_ppwjjj_uuxcddxsxw_1(i) = &
        particle_types_ppwjjj_uuxcddxsxw_1( &
        external_perm_ppwjjj_uuxcddxsxw_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_ppwjjj_uuxcddxsxw_1(i) >= &
          external_perm_ppwjjj_uuxcddxsxw_1(j)) then
          ii = external_perm_ppwjjj_uuxcddxsxw_1(i)
          jj = external_perm_ppwjjj_uuxcddxsxw_1(j)
        else
          ii = external_perm_ppwjjj_uuxcddxsxw_1(j)
          jj = external_perm_ppwjjj_uuxcddxsxw_1(i)
        end if
        extcomb_perm_ppwjjj_uuxcddxsxw_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppwjjj_uuxcddxsxw_1 = &
      average_factors_ppwjjj_uuxcddxsxw_1( &
      external_perm_ppwjjj_uuxcddxsxw_1(1)) &
      * average_factors_ppwjjj_uuxcddxsxw_1( &
      external_perm_ppwjjj_uuxcddxsxw_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_ppwjjj_uuxcddxsxw_1 = &
          average_factor_ppwjjj_uuxcddxsxw_1 &
          * factorial(count(particle_types_perm_ppwjjj_uuxcddxsxw_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppwjjj_uuxcddxsxw_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppwjjj_uuxcddxsxw_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rZERO, rZERO, rMW_unscaled ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_ppwjjj_uuxcddxsxw_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppwjjj_uuxcddxsxw_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppwjjj_uuxcddxsxw_1")
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
      & bind(c,name="ol_rambo_ppwjjj_uuxcddxsxw_1")
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

  end subroutine hel_init

end module ol_external_ppwjjj_uuxcddxsxw_1


module colour_basis_ppwjjj_uuxcddxsxw_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,1,1,0]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppwjjj_uuxcddxsxw_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(7)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 6
    ncoupl = 1
    maxpows = 1
    nhel = 192
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_ppwjjj_uuxcddxsxw_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,6)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([2], [1,1])
#endif
#if 6 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [1,184,1,1,160,1,1,182,1,1,136,1,1,158,1,1,134,1], &
      [3,6])
#endif
  end subroutine tree_colbasis

end module colour_basis_ppwjjj_uuxcddxsxw_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppwjjj_uuxcddxsxw_1(perm)
  use ol_external_ppwjjj_uuxcddxsxw_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_ppwjjj_uuxcddxsxw_1

! **********************************************************************
module ol_heltables_ppwjjj_uuxcddxsxw_1
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
  integer(intkind2), save :: n2(10), n3(3,43)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,2), t3x6(2,6,2), t3x12(2,12,2), t3x16(2,16,5), t3x8(2,8,8), t3x24(2,24,8), t3x192(2,192,16)

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
  call heltable([3,2,6], n3(:,2), t3x6(:,:,1))
  call heltable([2,2,4], n3(:,3), t3x4(:,:,2))
  n2(1) = 6
  call heltable([2,4,8], n3(:,4), t3x8(:,:,1))
  call heltable([4,6,24], n3(:,5), t3x24(:,:,1))
  n2(2) = 8
  call heltable([6,2,12], n3(:,6), t3x12(:,:,1))
  call heltable([4,4,16], n3(:,7), t3x16(:,:,1))
  call heltable([2,4,8], n3(:,8), t3x8(:,:,2))
  call heltable([4,6,24], n3(:,9), t3x24(:,:,2))
  n2(3) = 8
  call heltable([2,3,6], n3(:,10), t3x6(:,:,2))
  n2(4) = 6
  call heltable([4,2,8], n3(:,11), t3x8(:,:,3))
  call heltable([6,4,24], n3(:,12), t3x24(:,:,3))
  n2(5) = 8
  call heltable([4,2,8], n3(:,13), t3x8(:,:,4))
  call heltable([6,4,24], n3(:,14), t3x24(:,:,4))
  n2(6) = 8
  call heltable([2,6,12], n3(:,15), t3x12(:,:,2))
  call heltable([3,8,24], n3(:,16), t3x24(:,:,5))
  call heltable([3,8,24], n3(:,17), t3x24(:,:,6))
  call heltable([4,2,8], n3(:,18), t3x8(:,:,5))
  n2(7) = 8
  call heltable([8,2,16], n3(:,19), t3x16(:,:,2))
  call heltable([2,4,8], n3(:,20), t3x8(:,:,6))
  n2(8) = 8
  call heltable([2,8,16], n3(:,21), t3x16(:,:,3))
  call heltable([2,12,24], n3(:,22), t3x24(:,:,7))
  call heltable([12,2,24], n3(:,23), t3x24(:,:,8))
  call heltable([4,2,8], n3(:,24), t3x8(:,:,7))
  n2(9) = 8
  call heltable([8,2,16], n3(:,25), t3x16(:,:,4))
  call heltable([2,4,8], n3(:,26), t3x8(:,:,8))
  n2(10) = 8
  call heltable([2,8,16], n3(:,27), t3x16(:,:,5))
  call heltable([24,8,192], n3(:,28), t3x192(:,:,1))
  call heltable([12,16,192], n3(:,29), t3x192(:,:,2))
  call heltable([24,8,192], n3(:,30), t3x192(:,:,3))
  call heltable([24,8,192], n3(:,31), t3x192(:,:,4))
  call heltable([24,8,192], n3(:,32), t3x192(:,:,5))
  call heltable([16,12,192], n3(:,33), t3x192(:,:,6))
  call heltable([8,24,192], n3(:,34), t3x192(:,:,7))
  call heltable([8,24,192], n3(:,35), t3x192(:,:,8))
  call heltable([12,16,192], n3(:,36), t3x192(:,:,9))
  call heltable([12,16,192], n3(:,37), t3x192(:,:,10))
  call heltable([8,24,192], n3(:,38), t3x192(:,:,11))
  call heltable([8,24,192], n3(:,39), t3x192(:,:,12))
  call heltable([12,16,192], n3(:,40), t3x192(:,:,13))
  call heltable([12,16,192], n3(:,41), t3x192(:,:,14))
  call heltable([12,16,192], n3(:,42), t3x192(:,:,15))
  call heltable([12,16,192], n3(:,43), t3x192(:,:,16))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppwjjj_uuxcddxsxw_1
