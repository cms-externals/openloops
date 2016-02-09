
module ol_external_ppzjjj_dddxdxzgg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppzjjj_dddxdxzgg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_ppzjjj_dddxdxzgg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_ppzjjj_dddxdxzgg_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppzjjj_dddxdxzgg_1(7) = &
                     [ 1, 1, 2, 2, 3, 4, 4 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppzjjj_dddxdxzgg_1(7) = &
                     [ 6, 6, 6, 6, 3, 16, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppzjjj_dddxdxzgg_1 = &
                     144
  integer, save :: channel_number_ppzjjj_dddxdxzgg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,192) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(192,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppzjjj_dddxdxzgg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppzjjj_dddxdxzgg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppzjjj_dddxdxzgg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppzjjj_dddxdxzgg_1(7)
    external_perm_ppzjjj_dddxdxzgg_1 = perm
    do i = 1, 7
      external_perm_inv_ppzjjj_dddxdxzgg_1( &
        external_perm_ppzjjj_dddxdxzgg_1(i)) = i
      particle_types_perm_ppzjjj_dddxdxzgg_1(i) = &
        particle_types_ppzjjj_dddxdxzgg_1( &
        external_perm_ppzjjj_dddxdxzgg_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_ppzjjj_dddxdxzgg_1(i) >= &
          external_perm_ppzjjj_dddxdxzgg_1(j)) then
          ii = external_perm_ppzjjj_dddxdxzgg_1(i)
          jj = external_perm_ppzjjj_dddxdxzgg_1(j)
        else
          ii = external_perm_ppzjjj_dddxdxzgg_1(j)
          jj = external_perm_ppzjjj_dddxdxzgg_1(i)
        end if
        extcomb_perm_ppzjjj_dddxdxzgg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppzjjj_dddxdxzgg_1 = &
      average_factors_ppzjjj_dddxdxzgg_1( &
      external_perm_ppzjjj_dddxdxzgg_1(1)) &
      * average_factors_ppzjjj_dddxdxzgg_1( &
      external_perm_ppzjjj_dddxdxzgg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_ppzjjj_dddxdxzgg_1 = &
          average_factor_ppzjjj_dddxdxzgg_1 &
          * factorial(count(particle_types_perm_ppzjjj_dddxdxzgg_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppzjjj_dddxdxzgg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppzjjj_dddxdxzgg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rMZ_unscaled, rZERO, rZERO ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_ppzjjj_dddxdxzgg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppzjjj_dddxdxzgg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppzjjj_dddxdxzgg_1")
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
      & bind(c,name="ol_rambo_ppzjjj_dddxdxzgg_1")
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
  H(:,  5) = [ -1, -1, -1, -1,  0, -1, -1 ]
  H(:,  6) = [ -1, -1, -1, -1,  0, -1,  1 ]
  H(:,  7) = [ -1, -1, -1, -1,  0,  1, -1 ]
  H(:,  8) = [ -1, -1, -1, -1,  0,  1,  1 ]
  H(:,  9) = [ -1, -1, -1, -1,  1, -1, -1 ]
  H(:, 10) = [ -1, -1, -1, -1,  1, -1,  1 ]
  H(:, 11) = [ -1, -1, -1, -1,  1,  1, -1 ]
  H(:, 12) = [ -1, -1, -1, -1,  1,  1,  1 ]
  H(:, 13) = [ -1, -1, -1,  1, -1, -1, -1 ]
  H(:, 14) = [ -1, -1, -1,  1, -1, -1,  1 ]
  H(:, 15) = [ -1, -1, -1,  1, -1,  1, -1 ]
  H(:, 16) = [ -1, -1, -1,  1, -1,  1,  1 ]
  H(:, 17) = [ -1, -1, -1,  1,  0, -1, -1 ]
  H(:, 18) = [ -1, -1, -1,  1,  0, -1,  1 ]
  H(:, 19) = [ -1, -1, -1,  1,  0,  1, -1 ]
  H(:, 20) = [ -1, -1, -1,  1,  0,  1,  1 ]
  H(:, 21) = [ -1, -1, -1,  1,  1, -1, -1 ]
  H(:, 22) = [ -1, -1, -1,  1,  1, -1,  1 ]
  H(:, 23) = [ -1, -1, -1,  1,  1,  1, -1 ]
  H(:, 24) = [ -1, -1, -1,  1,  1,  1,  1 ]
  H(:, 25) = [ -1, -1,  1, -1, -1, -1, -1 ]
  H(:, 26) = [ -1, -1,  1, -1, -1, -1,  1 ]
  H(:, 27) = [ -1, -1,  1, -1, -1,  1, -1 ]
  H(:, 28) = [ -1, -1,  1, -1, -1,  1,  1 ]
  H(:, 29) = [ -1, -1,  1, -1,  0, -1, -1 ]
  H(:, 30) = [ -1, -1,  1, -1,  0, -1,  1 ]
  H(:, 31) = [ -1, -1,  1, -1,  0,  1, -1 ]
  H(:, 32) = [ -1, -1,  1, -1,  0,  1,  1 ]
  H(:, 33) = [ -1, -1,  1, -1,  1, -1, -1 ]
  H(:, 34) = [ -1, -1,  1, -1,  1, -1,  1 ]
  H(:, 35) = [ -1, -1,  1, -1,  1,  1, -1 ]
  H(:, 36) = [ -1, -1,  1, -1,  1,  1,  1 ]
  H(:, 37) = [ -1, -1,  1,  1, -1, -1, -1 ]
  H(:, 38) = [ -1, -1,  1,  1, -1, -1,  1 ]
  H(:, 39) = [ -1, -1,  1,  1, -1,  1, -1 ]
  H(:, 40) = [ -1, -1,  1,  1, -1,  1,  1 ]
  H(:, 41) = [ -1, -1,  1,  1,  0, -1, -1 ]
  H(:, 42) = [ -1, -1,  1,  1,  0, -1,  1 ]
  H(:, 43) = [ -1, -1,  1,  1,  0,  1, -1 ]
  H(:, 44) = [ -1, -1,  1,  1,  0,  1,  1 ]
  H(:, 45) = [ -1, -1,  1,  1,  1, -1, -1 ]
  H(:, 46) = [ -1, -1,  1,  1,  1, -1,  1 ]
  H(:, 47) = [ -1, -1,  1,  1,  1,  1, -1 ]
  H(:, 48) = [ -1, -1,  1,  1,  1,  1,  1 ]
  H(:, 49) = [ -1,  1, -1, -1, -1, -1, -1 ]
  H(:, 50) = [ -1,  1, -1, -1, -1, -1,  1 ]
  H(:, 51) = [ -1,  1, -1, -1, -1,  1, -1 ]
  H(:, 52) = [ -1,  1, -1, -1, -1,  1,  1 ]
  H(:, 53) = [ -1,  1, -1, -1,  0, -1, -1 ]
  H(:, 54) = [ -1,  1, -1, -1,  0, -1,  1 ]
  H(:, 55) = [ -1,  1, -1, -1,  0,  1, -1 ]
  H(:, 56) = [ -1,  1, -1, -1,  0,  1,  1 ]
  H(:, 57) = [ -1,  1, -1, -1,  1, -1, -1 ]
  H(:, 58) = [ -1,  1, -1, -1,  1, -1,  1 ]
  H(:, 59) = [ -1,  1, -1, -1,  1,  1, -1 ]
  H(:, 60) = [ -1,  1, -1, -1,  1,  1,  1 ]
  H(:, 61) = [ -1,  1, -1,  1, -1, -1, -1 ]
  H(:, 62) = [ -1,  1, -1,  1, -1, -1,  1 ]
  H(:, 63) = [ -1,  1, -1,  1, -1,  1, -1 ]
  H(:, 64) = [ -1,  1, -1,  1, -1,  1,  1 ]
  H(:, 65) = [ -1,  1, -1,  1,  0, -1, -1 ]
  H(:, 66) = [ -1,  1, -1,  1,  0, -1,  1 ]
  H(:, 67) = [ -1,  1, -1,  1,  0,  1, -1 ]
  H(:, 68) = [ -1,  1, -1,  1,  0,  1,  1 ]
  H(:, 69) = [ -1,  1, -1,  1,  1, -1, -1 ]
  H(:, 70) = [ -1,  1, -1,  1,  1, -1,  1 ]
  H(:, 71) = [ -1,  1, -1,  1,  1,  1, -1 ]
  H(:, 72) = [ -1,  1, -1,  1,  1,  1,  1 ]
  H(:, 73) = [ -1,  1,  1, -1, -1, -1, -1 ]
  H(:, 74) = [ -1,  1,  1, -1, -1, -1,  1 ]
  H(:, 75) = [ -1,  1,  1, -1, -1,  1, -1 ]
  H(:, 76) = [ -1,  1,  1, -1, -1,  1,  1 ]
  H(:, 77) = [ -1,  1,  1, -1,  0, -1, -1 ]
  H(:, 78) = [ -1,  1,  1, -1,  0, -1,  1 ]
  H(:, 79) = [ -1,  1,  1, -1,  0,  1, -1 ]
  H(:, 80) = [ -1,  1,  1, -1,  0,  1,  1 ]
  H(:, 81) = [ -1,  1,  1, -1,  1, -1, -1 ]
  H(:, 82) = [ -1,  1,  1, -1,  1, -1,  1 ]
  H(:, 83) = [ -1,  1,  1, -1,  1,  1, -1 ]
  H(:, 84) = [ -1,  1,  1, -1,  1,  1,  1 ]
  H(:, 85) = [ -1,  1,  1,  1, -1, -1, -1 ]
  H(:, 86) = [ -1,  1,  1,  1, -1, -1,  1 ]
  H(:, 87) = [ -1,  1,  1,  1, -1,  1, -1 ]
  H(:, 88) = [ -1,  1,  1,  1, -1,  1,  1 ]
  H(:, 89) = [ -1,  1,  1,  1,  0, -1, -1 ]
  H(:, 90) = [ -1,  1,  1,  1,  0, -1,  1 ]
  H(:, 91) = [ -1,  1,  1,  1,  0,  1, -1 ]
  H(:, 92) = [ -1,  1,  1,  1,  0,  1,  1 ]
  H(:, 93) = [ -1,  1,  1,  1,  1, -1, -1 ]
  H(:, 94) = [ -1,  1,  1,  1,  1, -1,  1 ]
  H(:, 95) = [ -1,  1,  1,  1,  1,  1, -1 ]
  H(:, 96) = [ -1,  1,  1,  1,  1,  1,  1 ]
  H(:, 97) = [  1, -1, -1, -1, -1, -1, -1 ]
  H(:, 98) = [  1, -1, -1, -1, -1, -1,  1 ]
  H(:, 99) = [  1, -1, -1, -1, -1,  1, -1 ]
  H(:,100) = [  1, -1, -1, -1, -1,  1,  1 ]
  H(:,101) = [  1, -1, -1, -1,  0, -1, -1 ]
  H(:,102) = [  1, -1, -1, -1,  0, -1,  1 ]
  H(:,103) = [  1, -1, -1, -1,  0,  1, -1 ]
  H(:,104) = [  1, -1, -1, -1,  0,  1,  1 ]
  H(:,105) = [  1, -1, -1, -1,  1, -1, -1 ]
  H(:,106) = [  1, -1, -1, -1,  1, -1,  1 ]
  H(:,107) = [  1, -1, -1, -1,  1,  1, -1 ]
  H(:,108) = [  1, -1, -1, -1,  1,  1,  1 ]
  H(:,109) = [  1, -1, -1,  1, -1, -1, -1 ]
  H(:,110) = [  1, -1, -1,  1, -1, -1,  1 ]
  H(:,111) = [  1, -1, -1,  1, -1,  1, -1 ]
  H(:,112) = [  1, -1, -1,  1, -1,  1,  1 ]
  H(:,113) = [  1, -1, -1,  1,  0, -1, -1 ]
  H(:,114) = [  1, -1, -1,  1,  0, -1,  1 ]
  H(:,115) = [  1, -1, -1,  1,  0,  1, -1 ]
  H(:,116) = [  1, -1, -1,  1,  0,  1,  1 ]
  H(:,117) = [  1, -1, -1,  1,  1, -1, -1 ]
  H(:,118) = [  1, -1, -1,  1,  1, -1,  1 ]
  H(:,119) = [  1, -1, -1,  1,  1,  1, -1 ]
  H(:,120) = [  1, -1, -1,  1,  1,  1,  1 ]
  H(:,121) = [  1, -1,  1, -1, -1, -1, -1 ]
  H(:,122) = [  1, -1,  1, -1, -1, -1,  1 ]
  H(:,123) = [  1, -1,  1, -1, -1,  1, -1 ]
  H(:,124) = [  1, -1,  1, -1, -1,  1,  1 ]
  H(:,125) = [  1, -1,  1, -1,  0, -1, -1 ]
  H(:,126) = [  1, -1,  1, -1,  0, -1,  1 ]
  H(:,127) = [  1, -1,  1, -1,  0,  1, -1 ]
  H(:,128) = [  1, -1,  1, -1,  0,  1,  1 ]
  H(:,129) = [  1, -1,  1, -1,  1, -1, -1 ]
  H(:,130) = [  1, -1,  1, -1,  1, -1,  1 ]
  H(:,131) = [  1, -1,  1, -1,  1,  1, -1 ]
  H(:,132) = [  1, -1,  1, -1,  1,  1,  1 ]
  H(:,133) = [  1, -1,  1,  1, -1, -1, -1 ]
  H(:,134) = [  1, -1,  1,  1, -1, -1,  1 ]
  H(:,135) = [  1, -1,  1,  1, -1,  1, -1 ]
  H(:,136) = [  1, -1,  1,  1, -1,  1,  1 ]
  H(:,137) = [  1, -1,  1,  1,  0, -1, -1 ]
  H(:,138) = [  1, -1,  1,  1,  0, -1,  1 ]
  H(:,139) = [  1, -1,  1,  1,  0,  1, -1 ]
  H(:,140) = [  1, -1,  1,  1,  0,  1,  1 ]
  H(:,141) = [  1, -1,  1,  1,  1, -1, -1 ]
  H(:,142) = [  1, -1,  1,  1,  1, -1,  1 ]
  H(:,143) = [  1, -1,  1,  1,  1,  1, -1 ]
  H(:,144) = [  1, -1,  1,  1,  1,  1,  1 ]
  H(:,145) = [  1,  1, -1, -1, -1, -1, -1 ]
  H(:,146) = [  1,  1, -1, -1, -1, -1,  1 ]
  H(:,147) = [  1,  1, -1, -1, -1,  1, -1 ]
  H(:,148) = [  1,  1, -1, -1, -1,  1,  1 ]
  H(:,149) = [  1,  1, -1, -1,  0, -1, -1 ]
  H(:,150) = [  1,  1, -1, -1,  0, -1,  1 ]
  H(:,151) = [  1,  1, -1, -1,  0,  1, -1 ]
  H(:,152) = [  1,  1, -1, -1,  0,  1,  1 ]
  H(:,153) = [  1,  1, -1, -1,  1, -1, -1 ]
  H(:,154) = [  1,  1, -1, -1,  1, -1,  1 ]
  H(:,155) = [  1,  1, -1, -1,  1,  1, -1 ]
  H(:,156) = [  1,  1, -1, -1,  1,  1,  1 ]
  H(:,157) = [  1,  1, -1,  1, -1, -1, -1 ]
  H(:,158) = [  1,  1, -1,  1, -1, -1,  1 ]
  H(:,159) = [  1,  1, -1,  1, -1,  1, -1 ]
  H(:,160) = [  1,  1, -1,  1, -1,  1,  1 ]
  H(:,161) = [  1,  1, -1,  1,  0, -1, -1 ]
  H(:,162) = [  1,  1, -1,  1,  0, -1,  1 ]
  H(:,163) = [  1,  1, -1,  1,  0,  1, -1 ]
  H(:,164) = [  1,  1, -1,  1,  0,  1,  1 ]
  H(:,165) = [  1,  1, -1,  1,  1, -1, -1 ]
  H(:,166) = [  1,  1, -1,  1,  1, -1,  1 ]
  H(:,167) = [  1,  1, -1,  1,  1,  1, -1 ]
  H(:,168) = [  1,  1, -1,  1,  1,  1,  1 ]
  H(:,169) = [  1,  1,  1, -1, -1, -1, -1 ]
  H(:,170) = [  1,  1,  1, -1, -1, -1,  1 ]
  H(:,171) = [  1,  1,  1, -1, -1,  1, -1 ]
  H(:,172) = [  1,  1,  1, -1, -1,  1,  1 ]
  H(:,173) = [  1,  1,  1, -1,  0, -1, -1 ]
  H(:,174) = [  1,  1,  1, -1,  0, -1,  1 ]
  H(:,175) = [  1,  1,  1, -1,  0,  1, -1 ]
  H(:,176) = [  1,  1,  1, -1,  0,  1,  1 ]
  H(:,177) = [  1,  1,  1, -1,  1, -1, -1 ]
  H(:,178) = [  1,  1,  1, -1,  1, -1,  1 ]
  H(:,179) = [  1,  1,  1, -1,  1,  1, -1 ]
  H(:,180) = [  1,  1,  1, -1,  1,  1,  1 ]
  H(:,181) = [  1,  1,  1,  1, -1, -1, -1 ]
  H(:,182) = [  1,  1,  1,  1, -1, -1,  1 ]
  H(:,183) = [  1,  1,  1,  1, -1,  1, -1 ]
  H(:,184) = [  1,  1,  1,  1, -1,  1,  1 ]
  H(:,185) = [  1,  1,  1,  1,  0, -1, -1 ]
  H(:,186) = [  1,  1,  1,  1,  0, -1,  1 ]
  H(:,187) = [  1,  1,  1,  1,  0,  1, -1 ]
  H(:,188) = [  1,  1,  1,  1,  0,  1,  1 ]
  H(:,189) = [  1,  1,  1,  1,  1, -1, -1 ]
  H(:,190) = [  1,  1,  1,  1,  1, -1,  1 ]
  H(:,191) = [  1,  1,  1,  1,  1,  1, -1 ]
  H(:,192) = [  1,  1,  1,  1,  1,  1,  1 ]

  H_HC(:,6) = [ ((((2*(binco-1)+flip)*2+binpos, flip = 0, 1), binpos = 1, 2), binco = 1, 192/2/2) ]
  H_HC(:,7) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 192/1/2) ]
  end subroutine hel_init

end module ol_external_ppzjjj_dddxdxzgg_1


module colour_basis_ppzjjj_dddxdxzgg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,0,2,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppzjjj_dddxdxzgg_1")
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
    nhel = 192
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_ppzjjj_dddxdxzgg_1")
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

end module colour_basis_ppzjjj_dddxdxzgg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppzjjj_dddxdxzgg_1(perm)
  use ol_external_ppzjjj_dddxdxzgg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_ppzjjj_dddxdxzgg_1

! **********************************************************************
module ol_heltables_ppzjjj_dddxdxzgg_1
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
    H6(2) = [-1,1], &
    H7(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(48), n3(3,689), n4(4,12)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x6(2,6,4), t3x4(2,4,13), t3x12(2,12,24), t3x8(2,8,44), t3x24(2,24,94), t3x16(2,16,102), &
    t3x192(2,192,408), t4x16(3,16,12)

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
  n2(1) = 6
  call heltable([6,2,12], n3(:,3), t3x12(:,:,1))
  call heltable([4,2,2,16], n4(:,1), t4x16(:,:,1))
  call heltable([2,2,4,16], n4(:,2), t4x16(:,:,2))
  call heltable([2,4,2,16], n4(:,3), t4x16(:,:,3))
  call heltable([2,3,6], n3(:,4), t3x6(:,:,2))
  n2(2) = 6
  call heltable([2,6,12], n3(:,5), t3x12(:,:,2))
  call heltable([3,2,6], n3(:,6), t3x6(:,:,3))
  call heltable([2,2,4], n3(:,7), t3x4(:,:,2))
  n2(3) = 6
  call heltable([6,2,12], n3(:,8), t3x12(:,:,3))
  call heltable([4,2,2,16], n4(:,4), t4x16(:,:,4))
  call heltable([2,2,4,16], n4(:,5), t4x16(:,:,5))
  call heltable([2,4,2,16], n4(:,6), t4x16(:,:,6))
  call heltable([2,6,12], n3(:,9), t3x12(:,:,4))
  call heltable([2,2,4], n3(:,10), t3x4(:,:,3))
  call heltable([6,2,12], n3(:,11), t3x12(:,:,5))
  call heltable([4,2,2,16], n4(:,7), t4x16(:,:,7))
  call heltable([2,2,4,16], n4(:,8), t4x16(:,:,8))
  call heltable([2,4,2,16], n4(:,9), t4x16(:,:,9))
  call heltable([2,3,6], n3(:,12), t3x6(:,:,4))
  n2(4) = 6
  call heltable([2,6,12], n3(:,13), t3x12(:,:,6))
  call heltable([2,2,4], n3(:,14), t3x4(:,:,4))
  call heltable([6,2,12], n3(:,15), t3x12(:,:,7))
  call heltable([4,2,2,16], n4(:,10), t4x16(:,:,10))
  call heltable([2,2,4,16], n4(:,11), t4x16(:,:,11))
  call heltable([2,4,2,16], n4(:,12), t4x16(:,:,12))
  call heltable([2,6,12], n3(:,16), t3x12(:,:,8))
  call heltable([2,2,4], n3(:,17), t3x4(:,:,5))
  n2(5) = 4
  call heltable([4,2,8], n3(:,18), t3x8(:,:,1))
  call heltable([6,4,24], n3(:,19), t3x24(:,:,1))
  call heltable([2,6,12], n3(:,20), t3x12(:,:,9))
  call heltable([4,4,16], n3(:,21), t3x16(:,:,1))
  n2(6) = 12
  call heltable([4,2,8], n3(:,22), t3x8(:,:,2))
  call heltable([4,6,24], n3(:,23), t3x24(:,:,2))
  n2(7) = 8
  call heltable([2,2,4], n3(:,24), t3x4(:,:,6))
  n2(8) = 4
  call heltable([4,2,8], n3(:,25), t3x8(:,:,3))
  call heltable([6,4,24], n3(:,26), t3x24(:,:,3))
  call heltable([2,6,12], n3(:,27), t3x12(:,:,10))
  call heltable([4,4,16], n3(:,28), t3x16(:,:,2))
  n2(9) = 12
  call heltable([4,2,8], n3(:,29), t3x8(:,:,4))
  n2(10) = 8
  call heltable([2,2,4], n3(:,30), t3x4(:,:,7))
  call heltable([2,4,8], n3(:,31), t3x8(:,:,5))
  call heltable([4,6,24], n3(:,32), t3x24(:,:,4))
  n2(11) = 8
  call heltable([4,4,16], n3(:,33), t3x16(:,:,3))
  call heltable([2,4,8], n3(:,34), t3x8(:,:,6))
  n2(12) = 8
  call heltable([8,2,16], n3(:,35), t3x16(:,:,4))
  call heltable([8,2,16], n3(:,36), t3x16(:,:,5))
  call heltable([12,2,24], n3(:,37), t3x24(:,:,5))
  call heltable([2,8,16], n3(:,38), t3x16(:,:,6))
  call heltable([12,2,24], n3(:,39), t3x24(:,:,6))
  call heltable([12,2,24], n3(:,40), t3x24(:,:,7))
  call heltable([2,2,4], n3(:,41), t3x4(:,:,8))
  n2(13) = 4
  call heltable([4,6,24], n3(:,42), t3x24(:,:,8))
  call heltable([2,4,8], n3(:,43), t3x8(:,:,7))
  call heltable([6,4,24], n3(:,44), t3x24(:,:,9))
  n2(14) = 8
  call heltable([6,2,12], n3(:,45), t3x12(:,:,11))
  call heltable([4,4,16], n3(:,46), t3x16(:,:,7))
  n2(15) = 12
  call heltable([2,2,4], n3(:,47), t3x4(:,:,9))
  n2(16) = 4
  call heltable([4,6,24], n3(:,48), t3x24(:,:,10))
  call heltable([2,4,8], n3(:,49), t3x8(:,:,8))
  n2(17) = 8
  call heltable([6,2,12], n3(:,50), t3x12(:,:,12))
  call heltable([4,4,16], n3(:,51), t3x16(:,:,8))
  n2(18) = 12
  call heltable([4,2,8], n3(:,52), t3x8(:,:,9))
  call heltable([6,4,24], n3(:,53), t3x24(:,:,11))
  n2(19) = 8
  call heltable([4,2,8], n3(:,54), t3x8(:,:,10))
  n2(20) = 8
  call heltable([2,8,16], n3(:,55), t3x16(:,:,9))
  call heltable([2,8,16], n3(:,56), t3x16(:,:,10))
  call heltable([12,2,24], n3(:,57), t3x24(:,:,12))
  call heltable([8,2,16], n3(:,58), t3x16(:,:,11))
  call heltable([12,2,24], n3(:,59), t3x24(:,:,13))
  call heltable([2,12,24], n3(:,60), t3x24(:,:,14))
  call heltable([3,4,12], n3(:,61), t3x12(:,:,13))
  n2(21) = 12
  call heltable([4,3,12], n3(:,62), t3x12(:,:,14))
  n2(22) = 12
  call heltable([8,3,24], n3(:,63), t3x24(:,:,15))
  call heltable([12,2,24], n3(:,64), t3x24(:,:,16))
  call heltable([3,4,12], n3(:,65), t3x12(:,:,15))
  n2(23) = 12
  call heltable([4,3,12], n3(:,66), t3x12(:,:,16))
  n2(24) = 12
  call heltable([3,8,24], n3(:,67), t3x24(:,:,17))
  call heltable([2,12,24], n3(:,68), t3x24(:,:,18))
  call heltable([12,2,24], n3(:,69), t3x24(:,:,19))
  call heltable([2,12,24], n3(:,70), t3x24(:,:,20))
  call heltable([3,8,24], n3(:,71), t3x24(:,:,21))
  call heltable([2,6,12], n3(:,72), t3x12(:,:,17))
  call heltable([4,4,16], n3(:,73), t3x16(:,:,12))
  n2(25) = 12
  call heltable([4,2,8], n3(:,74), t3x8(:,:,11))
  call heltable([6,4,24], n3(:,75), t3x24(:,:,22))
  call heltable([4,6,24], n3(:,76), t3x24(:,:,23))
  call heltable([2,6,12], n3(:,77), t3x12(:,:,18))
  call heltable([4,4,16], n3(:,78), t3x16(:,:,13))
  n2(26) = 12
  call heltable([4,2,8], n3(:,79), t3x8(:,:,12))
  call heltable([6,4,24], n3(:,80), t3x24(:,:,24))
  call heltable([4,4,16], n3(:,81), t3x16(:,:,14))
  call heltable([2,4,8], n3(:,82), t3x8(:,:,13))
  call heltable([4,6,24], n3(:,83), t3x24(:,:,25))
  n2(27) = 8
  call heltable([12,2,24], n3(:,84), t3x24(:,:,26))
  call heltable([12,2,24], n3(:,85), t3x24(:,:,27))
  call heltable([8,2,16], n3(:,86), t3x16(:,:,15))
  call heltable([12,2,24], n3(:,87), t3x24(:,:,28))
  call heltable([8,2,16], n3(:,88), t3x16(:,:,16))
  call heltable([2,8,16], n3(:,89), t3x16(:,:,17))
  call heltable([2,2,4], n3(:,90), t3x4(:,:,10))
  n2(28) = 4
  call heltable([2,4,8], n3(:,91), t3x8(:,:,14))
  call heltable([6,4,24], n3(:,92), t3x24(:,:,29))
  n2(29) = 8
  call heltable([4,6,24], n3(:,93), t3x24(:,:,30))
  call heltable([4,4,16], n3(:,94), t3x16(:,:,18))
  call heltable([2,2,4], n3(:,95), t3x4(:,:,11))
  n2(30) = 4
  call heltable([2,4,8], n3(:,96), t3x8(:,:,15))
  n2(31) = 8
  call heltable([4,6,24], n3(:,97), t3x24(:,:,31))
  call heltable([4,4,16], n3(:,98), t3x16(:,:,19))
  call heltable([4,2,8], n3(:,99), t3x8(:,:,16))
  n2(32) = 8
  call heltable([4,2,8], n3(:,100), t3x8(:,:,17))
  n2(33) = 8
  call heltable([2,8,16], n3(:,101), t3x16(:,:,20))
  call heltable([2,8,16], n3(:,102), t3x16(:,:,21))
  call heltable([12,2,24], n3(:,103), t3x24(:,:,32))
  call heltable([12,2,24], n3(:,104), t3x24(:,:,33))
  call heltable([8,2,16], n3(:,105), t3x16(:,:,22))
  call heltable([2,12,24], n3(:,106), t3x24(:,:,34))
  call heltable([3,4,12], n3(:,107), t3x12(:,:,19))
  n2(34) = 12
  call heltable([12,2,24], n3(:,108), t3x24(:,:,35))
  call heltable([8,3,24], n3(:,109), t3x24(:,:,36))
  call heltable([3,4,12], n3(:,110), t3x12(:,:,20))
  n2(35) = 12
  call heltable([3,8,24], n3(:,111), t3x24(:,:,37))
  call heltable([2,12,24], n3(:,112), t3x24(:,:,38))
  call heltable([12,2,24], n3(:,113), t3x24(:,:,39))
  call heltable([2,12,24], n3(:,114), t3x24(:,:,40))
  call heltable([3,8,24], n3(:,115), t3x24(:,:,41))
  call heltable([2,2,4], n3(:,116), t3x4(:,:,12))
  n2(36) = 4
  call heltable([4,2,8], n3(:,117), t3x8(:,:,18))
  call heltable([6,4,24], n3(:,118), t3x24(:,:,42))
  call heltable([4,4,16], n3(:,119), t3x16(:,:,23))
  call heltable([4,2,8], n3(:,120), t3x8(:,:,19))
  call heltable([4,6,24], n3(:,121), t3x24(:,:,43))
  n2(37) = 8
  call heltable([2,2,4], n3(:,122), t3x4(:,:,13))
  n2(38) = 4
  call heltable([4,2,8], n3(:,123), t3x8(:,:,20))
  call heltable([6,4,24], n3(:,124), t3x24(:,:,44))
  call heltable([4,4,16], n3(:,125), t3x16(:,:,24))
  call heltable([4,2,8], n3(:,126), t3x8(:,:,21))
  n2(39) = 8
  call heltable([2,4,8], n3(:,127), t3x8(:,:,22))
  n2(40) = 8
  call heltable([4,4,16], n3(:,128), t3x16(:,:,25))
  call heltable([2,4,8], n3(:,129), t3x8(:,:,23))
  n2(41) = 8
  call heltable([8,2,16], n3(:,130), t3x16(:,:,26))
  call heltable([8,2,16], n3(:,131), t3x16(:,:,27))
  call heltable([12,2,24], n3(:,132), t3x24(:,:,45))
  call heltable([2,8,16], n3(:,133), t3x16(:,:,28))
  call heltable([12,2,24], n3(:,134), t3x24(:,:,46))
  call heltable([12,2,24], n3(:,135), t3x24(:,:,47))
  call heltable([4,6,24], n3(:,136), t3x24(:,:,48))
  call heltable([6,4,24], n3(:,137), t3x24(:,:,49))
  call heltable([6,2,12], n3(:,138), t3x12(:,:,21))
  call heltable([4,4,16], n3(:,139), t3x16(:,:,29))
  n2(42) = 12
  call heltable([4,6,24], n3(:,140), t3x24(:,:,50))
  call heltable([6,2,12], n3(:,141), t3x12(:,:,22))
  call heltable([4,4,16], n3(:,142), t3x16(:,:,30))
  n2(43) = 12
  call heltable([4,2,8], n3(:,143), t3x8(:,:,24))
  call heltable([6,4,24], n3(:,144), t3x24(:,:,51))
  n2(44) = 8
  call heltable([2,8,16], n3(:,145), t3x16(:,:,31))
  call heltable([2,8,16], n3(:,146), t3x16(:,:,32))
  call heltable([12,2,24], n3(:,147), t3x24(:,:,52))
  call heltable([8,2,16], n3(:,148), t3x16(:,:,33))
  call heltable([12,2,24], n3(:,149), t3x24(:,:,53))
  call heltable([2,12,24], n3(:,150), t3x24(:,:,54))
  call heltable([4,3,12], n3(:,151), t3x12(:,:,23))
  n2(45) = 12
  call heltable([8,3,24], n3(:,152), t3x24(:,:,55))
  call heltable([12,2,24], n3(:,153), t3x24(:,:,56))
  call heltable([4,3,12], n3(:,154), t3x12(:,:,24))
  n2(46) = 12
  call heltable([3,8,24], n3(:,155), t3x24(:,:,57))
  call heltable([2,12,24], n3(:,156), t3x24(:,:,58))
  call heltable([12,2,24], n3(:,157), t3x24(:,:,59))
  call heltable([2,12,24], n3(:,158), t3x24(:,:,60))
  call heltable([4,4,16], n3(:,159), t3x16(:,:,34))
  call heltable([4,2,8], n3(:,160), t3x8(:,:,25))
  call heltable([6,4,24], n3(:,161), t3x24(:,:,61))
  call heltable([4,6,24], n3(:,162), t3x24(:,:,62))
  call heltable([4,4,16], n3(:,163), t3x16(:,:,35))
  call heltable([4,2,8], n3(:,164), t3x8(:,:,26))
  call heltable([6,4,24], n3(:,165), t3x24(:,:,63))
  call heltable([4,4,16], n3(:,166), t3x16(:,:,36))
  call heltable([2,4,8], n3(:,167), t3x8(:,:,27))
  n2(47) = 8
  call heltable([12,2,24], n3(:,168), t3x24(:,:,64))
  call heltable([12,2,24], n3(:,169), t3x24(:,:,65))
  call heltable([8,2,16], n3(:,170), t3x16(:,:,37))
  call heltable([12,2,24], n3(:,171), t3x24(:,:,66))
  call heltable([8,2,16], n3(:,172), t3x16(:,:,38))
  call heltable([2,8,16], n3(:,173), t3x16(:,:,39))
  call heltable([6,4,24], n3(:,174), t3x24(:,:,67))
  call heltable([4,6,24], n3(:,175), t3x24(:,:,68))
  call heltable([4,4,16], n3(:,176), t3x16(:,:,40))
  call heltable([4,6,24], n3(:,177), t3x24(:,:,69))
  call heltable([4,4,16], n3(:,178), t3x16(:,:,41))
  call heltable([4,2,8], n3(:,179), t3x8(:,:,28))
  n2(48) = 8
  call heltable([2,8,16], n3(:,180), t3x16(:,:,42))
  call heltable([2,8,16], n3(:,181), t3x16(:,:,43))
  call heltable([12,2,24], n3(:,182), t3x24(:,:,70))
  call heltable([12,2,24], n3(:,183), t3x24(:,:,71))
  call heltable([8,2,16], n3(:,184), t3x16(:,:,44))
  call heltable([2,12,24], n3(:,185), t3x24(:,:,72))
  call heltable([12,2,24], n3(:,186), t3x24(:,:,73))
  call heltable([8,3,24], n3(:,187), t3x24(:,:,74))
  call heltable([3,8,24], n3(:,188), t3x24(:,:,75))
  call heltable([2,12,24], n3(:,189), t3x24(:,:,76))
  call heltable([12,2,24], n3(:,190), t3x24(:,:,77))
  call heltable([2,12,24], n3(:,191), t3x24(:,:,78))
  call heltable([4,4,16], n3(:,192), t3x16(:,:,45))
  call heltable([4,2,8], n3(:,193), t3x8(:,:,29))
  call heltable([4,4,16], n3(:,194), t3x16(:,:,46))
  call heltable([4,2,8], n3(:,195), t3x8(:,:,30))
  call heltable([2,12,24], n3(:,196), t3x24(:,:,79))
  call heltable([8,2,16], n3(:,197), t3x16(:,:,47))
  call heltable([2,12,24], n3(:,198), t3x24(:,:,80))
  call heltable([2,8,16], n3(:,199), t3x16(:,:,48))
  call heltable([2,8,16], n3(:,200), t3x16(:,:,49))
  call heltable([4,4,16], n3(:,201), t3x16(:,:,50))
  call heltable([4,2,8], n3(:,202), t3x8(:,:,31))
  call heltable([2,4,8], n3(:,203), t3x8(:,:,32))
  call heltable([2,4,8], n3(:,204), t3x8(:,:,33))
  call heltable([8,2,16], n3(:,205), t3x16(:,:,51))
  call heltable([12,2,24], n3(:,206), t3x24(:,:,81))
  call heltable([2,8,16], n3(:,207), t3x16(:,:,52))
  call heltable([4,4,16], n3(:,208), t3x16(:,:,53))
  call heltable([4,2,8], n3(:,209), t3x8(:,:,34))
  call heltable([2,4,8], n3(:,210), t3x8(:,:,35))
  call heltable([2,4,8], n3(:,211), t3x8(:,:,36))
  call heltable([12,2,24], n3(:,212), t3x24(:,:,82))
  call heltable([8,2,16], n3(:,213), t3x16(:,:,54))
  call heltable([2,8,16], n3(:,214), t3x16(:,:,55))
  call heltable([2,8,16], n3(:,215), t3x16(:,:,56))
  call heltable([2,8,16], n3(:,216), t3x16(:,:,57))
  call heltable([2,8,16], n3(:,217), t3x16(:,:,58))
  call heltable([2,8,16], n3(:,218), t3x16(:,:,59))
  call heltable([2,8,16], n3(:,219), t3x16(:,:,60))
  call heltable([2,8,16], n3(:,220), t3x16(:,:,61))
  call heltable([2,8,16], n3(:,221), t3x16(:,:,62))
  call heltable([8,2,16], n3(:,222), t3x16(:,:,63))
  call heltable([2,8,16], n3(:,223), t3x16(:,:,64))
  call heltable([8,2,16], n3(:,224), t3x16(:,:,65))
  call heltable([4,2,8], n3(:,225), t3x8(:,:,37))
  call heltable([4,4,16], n3(:,226), t3x16(:,:,66))
  call heltable([4,2,8], n3(:,227), t3x8(:,:,38))
  call heltable([4,4,16], n3(:,228), t3x16(:,:,67))
  call heltable([8,2,16], n3(:,229), t3x16(:,:,68))
  call heltable([2,8,16], n3(:,230), t3x16(:,:,69))
  call heltable([2,8,16], n3(:,231), t3x16(:,:,70))
  call heltable([2,12,24], n3(:,232), t3x24(:,:,83))
  call heltable([2,12,24], n3(:,233), t3x24(:,:,84))
  call heltable([4,2,8], n3(:,234), t3x8(:,:,39))
  call heltable([4,4,16], n3(:,235), t3x16(:,:,71))
  call heltable([2,4,8], n3(:,236), t3x8(:,:,40))
  call heltable([2,4,8], n3(:,237), t3x8(:,:,41))
  call heltable([2,8,16], n3(:,238), t3x16(:,:,72))
  call heltable([8,2,16], n3(:,239), t3x16(:,:,73))
  call heltable([12,2,24], n3(:,240), t3x24(:,:,85))
  call heltable([4,2,8], n3(:,241), t3x8(:,:,42))
  call heltable([4,4,16], n3(:,242), t3x16(:,:,74))
  call heltable([2,4,8], n3(:,243), t3x8(:,:,43))
  call heltable([2,4,8], n3(:,244), t3x8(:,:,44))
  call heltable([2,8,16], n3(:,245), t3x16(:,:,75))
  call heltable([12,2,24], n3(:,246), t3x24(:,:,86))
  call heltable([8,2,16], n3(:,247), t3x16(:,:,76))
  call heltable([2,8,16], n3(:,248), t3x16(:,:,77))
  call heltable([2,8,16], n3(:,249), t3x16(:,:,78))
  call heltable([2,8,16], n3(:,250), t3x16(:,:,79))
  call heltable([2,8,16], n3(:,251), t3x16(:,:,80))
  call heltable([2,8,16], n3(:,252), t3x16(:,:,81))
  call heltable([2,8,16], n3(:,253), t3x16(:,:,82))
  call heltable([2,8,16], n3(:,254), t3x16(:,:,83))
  call heltable([2,8,16], n3(:,255), t3x16(:,:,84))
  call heltable([8,2,16], n3(:,256), t3x16(:,:,85))
  call heltable([8,2,16], n3(:,257), t3x16(:,:,86))
  call heltable([8,2,16], n3(:,258), t3x16(:,:,87))
  call heltable([2,12,24], n3(:,259), t3x24(:,:,87))
  call heltable([2,12,24], n3(:,260), t3x24(:,:,88))
  call heltable([8,2,16], n3(:,261), t3x16(:,:,88))
  call heltable([12,2,24], n3(:,262), t3x24(:,:,89))
  call heltable([8,2,16], n3(:,263), t3x16(:,:,89))
  call heltable([12,2,24], n3(:,264), t3x24(:,:,90))
  call heltable([8,2,16], n3(:,265), t3x16(:,:,90))
  call heltable([8,2,16], n3(:,266), t3x16(:,:,91))
  call heltable([8,2,16], n3(:,267), t3x16(:,:,92))
  call heltable([8,2,16], n3(:,268), t3x16(:,:,93))
  call heltable([8,2,16], n3(:,269), t3x16(:,:,94))
  call heltable([8,2,16], n3(:,270), t3x16(:,:,95))
  call heltable([2,12,24], n3(:,271), t3x24(:,:,91))
  call heltable([2,12,24], n3(:,272), t3x24(:,:,92))
  call heltable([8,2,16], n3(:,273), t3x16(:,:,96))
  call heltable([8,2,16], n3(:,274), t3x16(:,:,97))
  call heltable([12,2,24], n3(:,275), t3x24(:,:,93))
  call heltable([8,2,16], n3(:,276), t3x16(:,:,98))
  call heltable([12,2,24], n3(:,277), t3x24(:,:,94))
  call heltable([8,2,16], n3(:,278), t3x16(:,:,99))
  call heltable([8,2,16], n3(:,279), t3x16(:,:,100))
  call heltable([8,2,16], n3(:,280), t3x16(:,:,101))
  call heltable([8,2,16], n3(:,281), t3x16(:,:,102))
  call heltable([12,16,192], n3(:,282), t3x192(:,:,1))
  call heltable([12,16,192], n3(:,283), t3x192(:,:,2))
  call heltable([12,16,192], n3(:,284), t3x192(:,:,3))
  call heltable([16,12,192], n3(:,285), t3x192(:,:,4))
  call heltable([16,12,192], n3(:,286), t3x192(:,:,5))
  call heltable([16,12,192], n3(:,287), t3x192(:,:,6))
  call heltable([12,16,192], n3(:,288), t3x192(:,:,7))
  call heltable([12,16,192], n3(:,289), t3x192(:,:,8))
  call heltable([12,16,192], n3(:,290), t3x192(:,:,9))
  call heltable([16,12,192], n3(:,291), t3x192(:,:,10))
  call heltable([16,12,192], n3(:,292), t3x192(:,:,11))
  call heltable([16,12,192], n3(:,293), t3x192(:,:,12))
  call heltable([12,16,192], n3(:,294), t3x192(:,:,13))
  call heltable([12,16,192], n3(:,295), t3x192(:,:,14))
  call heltable([12,16,192], n3(:,296), t3x192(:,:,15))
  call heltable([16,12,192], n3(:,297), t3x192(:,:,16))
  call heltable([16,12,192], n3(:,298), t3x192(:,:,17))
  call heltable([16,12,192], n3(:,299), t3x192(:,:,18))
  call heltable([12,16,192], n3(:,300), t3x192(:,:,19))
  call heltable([12,16,192], n3(:,301), t3x192(:,:,20))
  call heltable([12,16,192], n3(:,302), t3x192(:,:,21))
  call heltable([16,12,192], n3(:,303), t3x192(:,:,22))
  call heltable([16,12,192], n3(:,304), t3x192(:,:,23))
  call heltable([16,12,192], n3(:,305), t3x192(:,:,24))
  call heltable([8,24,192], n3(:,306), t3x192(:,:,25))
  call heltable([16,12,192], n3(:,307), t3x192(:,:,26))
  call heltable([24,8,192], n3(:,308), t3x192(:,:,27))
  call heltable([8,24,192], n3(:,309), t3x192(:,:,28))
  call heltable([16,12,192], n3(:,310), t3x192(:,:,29))
  call heltable([24,8,192], n3(:,311), t3x192(:,:,30))
  call heltable([24,8,192], n3(:,312), t3x192(:,:,31))
  call heltable([12,16,192], n3(:,313), t3x192(:,:,32))
  call heltable([24,8,192], n3(:,314), t3x192(:,:,33))
  call heltable([12,16,192], n3(:,315), t3x192(:,:,34))
  call heltable([12,16,192], n3(:,316), t3x192(:,:,35))
  call heltable([8,24,192], n3(:,317), t3x192(:,:,36))
  call heltable([12,16,192], n3(:,318), t3x192(:,:,37))
  call heltable([8,24,192], n3(:,319), t3x192(:,:,38))
  call heltable([8,24,192], n3(:,320), t3x192(:,:,39))
  call heltable([8,24,192], n3(:,321), t3x192(:,:,40))
  call heltable([24,8,192], n3(:,322), t3x192(:,:,41))
  call heltable([16,12,192], n3(:,323), t3x192(:,:,42))
  call heltable([8,24,192], n3(:,324), t3x192(:,:,43))
  call heltable([24,8,192], n3(:,325), t3x192(:,:,44))
  call heltable([16,12,192], n3(:,326), t3x192(:,:,45))
  call heltable([24,8,192], n3(:,327), t3x192(:,:,46))
  call heltable([12,16,192], n3(:,328), t3x192(:,:,47))
  call heltable([24,8,192], n3(:,329), t3x192(:,:,48))
  call heltable([12,16,192], n3(:,330), t3x192(:,:,49))
  call heltable([12,16,192], n3(:,331), t3x192(:,:,50))
  call heltable([8,24,192], n3(:,332), t3x192(:,:,51))
  call heltable([12,16,192], n3(:,333), t3x192(:,:,52))
  call heltable([8,24,192], n3(:,334), t3x192(:,:,53))
  call heltable([8,24,192], n3(:,335), t3x192(:,:,54))
  call heltable([16,12,192], n3(:,336), t3x192(:,:,55))
  call heltable([16,12,192], n3(:,337), t3x192(:,:,56))
  call heltable([16,12,192], n3(:,338), t3x192(:,:,57))
  call heltable([8,24,192], n3(:,339), t3x192(:,:,58))
  call heltable([8,24,192], n3(:,340), t3x192(:,:,59))
  call heltable([16,12,192], n3(:,341), t3x192(:,:,60))
  call heltable([16,12,192], n3(:,342), t3x192(:,:,61))
  call heltable([8,24,192], n3(:,343), t3x192(:,:,62))
  call heltable([16,12,192], n3(:,344), t3x192(:,:,63))
  call heltable([8,24,192], n3(:,345), t3x192(:,:,64))
  call heltable([16,12,192], n3(:,346), t3x192(:,:,65))
  call heltable([8,24,192], n3(:,347), t3x192(:,:,66))
  call heltable([8,24,192], n3(:,348), t3x192(:,:,67))
  call heltable([8,24,192], n3(:,349), t3x192(:,:,68))
  call heltable([16,12,192], n3(:,350), t3x192(:,:,69))
  call heltable([8,24,192], n3(:,351), t3x192(:,:,70))
  call heltable([8,24,192], n3(:,352), t3x192(:,:,71))
  call heltable([8,24,192], n3(:,353), t3x192(:,:,72))
  call heltable([16,12,192], n3(:,354), t3x192(:,:,73))
  call heltable([8,24,192], n3(:,355), t3x192(:,:,74))
  call heltable([8,24,192], n3(:,356), t3x192(:,:,75))
  call heltable([16,12,192], n3(:,357), t3x192(:,:,76))
  call heltable([8,24,192], n3(:,358), t3x192(:,:,77))
  call heltable([8,24,192], n3(:,359), t3x192(:,:,78))
  call heltable([12,16,192], n3(:,360), t3x192(:,:,79))
  call heltable([24,8,192], n3(:,361), t3x192(:,:,80))
  call heltable([8,24,192], n3(:,362), t3x192(:,:,81))
  call heltable([8,24,192], n3(:,363), t3x192(:,:,82))
  call heltable([8,24,192], n3(:,364), t3x192(:,:,83))
  call heltable([12,16,192], n3(:,365), t3x192(:,:,84))
  call heltable([8,24,192], n3(:,366), t3x192(:,:,85))
  call heltable([12,16,192], n3(:,367), t3x192(:,:,86))
  call heltable([12,16,192], n3(:,368), t3x192(:,:,87))
  call heltable([24,8,192], n3(:,369), t3x192(:,:,88))
  call heltable([8,24,192], n3(:,370), t3x192(:,:,89))
  call heltable([12,16,192], n3(:,371), t3x192(:,:,90))
  call heltable([24,8,192], n3(:,372), t3x192(:,:,91))
  call heltable([8,24,192], n3(:,373), t3x192(:,:,92))
  call heltable([12,16,192], n3(:,374), t3x192(:,:,93))
  call heltable([24,8,192], n3(:,375), t3x192(:,:,94))
  call heltable([12,16,192], n3(:,376), t3x192(:,:,95))
  call heltable([24,8,192], n3(:,377), t3x192(:,:,96))
  call heltable([12,16,192], n3(:,378), t3x192(:,:,97))
  call heltable([12,16,192], n3(:,379), t3x192(:,:,98))
  call heltable([8,24,192], n3(:,380), t3x192(:,:,99))
  call heltable([8,24,192], n3(:,381), t3x192(:,:,100))
  call heltable([12,16,192], n3(:,382), t3x192(:,:,101))
  call heltable([8,24,192], n3(:,383), t3x192(:,:,102))
  call heltable([16,12,192], n3(:,384), t3x192(:,:,103))
  call heltable([12,16,192], n3(:,385), t3x192(:,:,104))
  call heltable([16,12,192], n3(:,386), t3x192(:,:,105))
  call heltable([8,24,192], n3(:,387), t3x192(:,:,106))
  call heltable([8,24,192], n3(:,388), t3x192(:,:,107))
  call heltable([16,12,192], n3(:,389), t3x192(:,:,108))
  call heltable([12,16,192], n3(:,390), t3x192(:,:,109))
  call heltable([8,24,192], n3(:,391), t3x192(:,:,110))
  call heltable([12,16,192], n3(:,392), t3x192(:,:,111))
  call heltable([8,24,192], n3(:,393), t3x192(:,:,112))
  call heltable([16,12,192], n3(:,394), t3x192(:,:,113))
  call heltable([8,24,192], n3(:,395), t3x192(:,:,114))
  call heltable([8,24,192], n3(:,396), t3x192(:,:,115))
  call heltable([8,24,192], n3(:,397), t3x192(:,:,116))
  call heltable([12,16,192], n3(:,398), t3x192(:,:,117))
  call heltable([8,24,192], n3(:,399), t3x192(:,:,118))
  call heltable([8,24,192], n3(:,400), t3x192(:,:,119))
  call heltable([8,24,192], n3(:,401), t3x192(:,:,120))
  call heltable([8,24,192], n3(:,402), t3x192(:,:,121))
  call heltable([12,16,192], n3(:,403), t3x192(:,:,122))
  call heltable([24,8,192], n3(:,404), t3x192(:,:,123))
  call heltable([8,24,192], n3(:,405), t3x192(:,:,124))
  call heltable([12,16,192], n3(:,406), t3x192(:,:,125))
  call heltable([24,8,192], n3(:,407), t3x192(:,:,126))
  call heltable([24,8,192], n3(:,408), t3x192(:,:,127))
  call heltable([12,16,192], n3(:,409), t3x192(:,:,128))
  call heltable([24,8,192], n3(:,410), t3x192(:,:,129))
  call heltable([12,16,192], n3(:,411), t3x192(:,:,130))
  call heltable([12,16,192], n3(:,412), t3x192(:,:,131))
  call heltable([8,24,192], n3(:,413), t3x192(:,:,132))
  call heltable([12,16,192], n3(:,414), t3x192(:,:,133))
  call heltable([8,24,192], n3(:,415), t3x192(:,:,134))
  call heltable([8,24,192], n3(:,416), t3x192(:,:,135))
  call heltable([8,24,192], n3(:,417), t3x192(:,:,136))
  call heltable([8,24,192], n3(:,418), t3x192(:,:,137))
  call heltable([16,12,192], n3(:,419), t3x192(:,:,138))
  call heltable([8,24,192], n3(:,420), t3x192(:,:,139))
  call heltable([8,24,192], n3(:,421), t3x192(:,:,140))
  call heltable([16,12,192], n3(:,422), t3x192(:,:,141))
  call heltable([24,8,192], n3(:,423), t3x192(:,:,142))
  call heltable([12,16,192], n3(:,424), t3x192(:,:,143))
  call heltable([8,24,192], n3(:,425), t3x192(:,:,144))
  call heltable([12,16,192], n3(:,426), t3x192(:,:,145))
  call heltable([12,16,192], n3(:,427), t3x192(:,:,146))
  call heltable([8,24,192], n3(:,428), t3x192(:,:,147))
  call heltable([12,16,192], n3(:,429), t3x192(:,:,148))
  call heltable([8,24,192], n3(:,430), t3x192(:,:,149))
  call heltable([8,24,192], n3(:,431), t3x192(:,:,150))
  call heltable([12,16,192], n3(:,432), t3x192(:,:,151))
  call heltable([16,12,192], n3(:,433), t3x192(:,:,152))
  call heltable([12,16,192], n3(:,434), t3x192(:,:,153))
  call heltable([8,24,192], n3(:,435), t3x192(:,:,154))
  call heltable([8,24,192], n3(:,436), t3x192(:,:,155))
  call heltable([12,16,192], n3(:,437), t3x192(:,:,156))
  call heltable([16,12,192], n3(:,438), t3x192(:,:,157))
  call heltable([8,24,192], n3(:,439), t3x192(:,:,158))
  call heltable([16,12,192], n3(:,440), t3x192(:,:,159))
  call heltable([8,24,192], n3(:,441), t3x192(:,:,160))
  call heltable([12,16,192], n3(:,442), t3x192(:,:,161))
  call heltable([8,24,192], n3(:,443), t3x192(:,:,162))
  call heltable([8,24,192], n3(:,444), t3x192(:,:,163))
  call heltable([8,24,192], n3(:,445), t3x192(:,:,164))
  call heltable([16,12,192], n3(:,446), t3x192(:,:,165))
  call heltable([8,24,192], n3(:,447), t3x192(:,:,166))
  call heltable([8,24,192], n3(:,448), t3x192(:,:,167))
  call heltable([24,8,192], n3(:,449), t3x192(:,:,168))
  call heltable([12,16,192], n3(:,450), t3x192(:,:,169))
  call heltable([8,24,192], n3(:,451), t3x192(:,:,170))
  call heltable([8,24,192], n3(:,452), t3x192(:,:,171))
  call heltable([12,16,192], n3(:,453), t3x192(:,:,172))
  call heltable([8,24,192], n3(:,454), t3x192(:,:,173))
  call heltable([8,24,192], n3(:,455), t3x192(:,:,174))
  call heltable([12,16,192], n3(:,456), t3x192(:,:,175))
  call heltable([24,8,192], n3(:,457), t3x192(:,:,176))
  call heltable([8,24,192], n3(:,458), t3x192(:,:,177))
  call heltable([8,24,192], n3(:,459), t3x192(:,:,178))
  call heltable([8,24,192], n3(:,460), t3x192(:,:,179))
  call heltable([12,16,192], n3(:,461), t3x192(:,:,180))
  call heltable([8,24,192], n3(:,462), t3x192(:,:,181))
  call heltable([12,16,192], n3(:,463), t3x192(:,:,182))
  call heltable([12,16,192], n3(:,464), t3x192(:,:,183))
  call heltable([8,24,192], n3(:,465), t3x192(:,:,184))
  call heltable([8,24,192], n3(:,466), t3x192(:,:,185))
  call heltable([12,16,192], n3(:,467), t3x192(:,:,186))
  call heltable([8,24,192], n3(:,468), t3x192(:,:,187))
  call heltable([8,24,192], n3(:,469), t3x192(:,:,188))
  call heltable([12,16,192], n3(:,470), t3x192(:,:,189))
  call heltable([24,8,192], n3(:,471), t3x192(:,:,190))
  call heltable([12,16,192], n3(:,472), t3x192(:,:,191))
  call heltable([8,24,192], n3(:,473), t3x192(:,:,192))
  call heltable([12,16,192], n3(:,474), t3x192(:,:,193))
  call heltable([12,16,192], n3(:,475), t3x192(:,:,194))
  call heltable([8,24,192], n3(:,476), t3x192(:,:,195))
  call heltable([8,24,192], n3(:,477), t3x192(:,:,196))
  call heltable([12,16,192], n3(:,478), t3x192(:,:,197))
  call heltable([8,24,192], n3(:,479), t3x192(:,:,198))
  call heltable([12,16,192], n3(:,480), t3x192(:,:,199))
  call heltable([12,16,192], n3(:,481), t3x192(:,:,200))
  call heltable([12,16,192], n3(:,482), t3x192(:,:,201))
  call heltable([8,24,192], n3(:,483), t3x192(:,:,202))
  call heltable([8,24,192], n3(:,484), t3x192(:,:,203))
  call heltable([12,16,192], n3(:,485), t3x192(:,:,204))
  call heltable([12,16,192], n3(:,486), t3x192(:,:,205))
  call heltable([8,24,192], n3(:,487), t3x192(:,:,206))
  call heltable([12,16,192], n3(:,488), t3x192(:,:,207))
  call heltable([8,24,192], n3(:,489), t3x192(:,:,208))
  call heltable([12,16,192], n3(:,490), t3x192(:,:,209))
  call heltable([8,24,192], n3(:,491), t3x192(:,:,210))
  call heltable([8,24,192], n3(:,492), t3x192(:,:,211))
  call heltable([8,24,192], n3(:,493), t3x192(:,:,212))
  call heltable([12,16,192], n3(:,494), t3x192(:,:,213))
  call heltable([8,24,192], n3(:,495), t3x192(:,:,214))
  call heltable([8,24,192], n3(:,496), t3x192(:,:,215))
  call heltable([24,8,192], n3(:,497), t3x192(:,:,216))
  call heltable([12,16,192], n3(:,498), t3x192(:,:,217))
  call heltable([24,8,192], n3(:,499), t3x192(:,:,218))
  call heltable([12,16,192], n3(:,500), t3x192(:,:,219))
  call heltable([24,8,192], n3(:,501), t3x192(:,:,220))
  call heltable([24,8,192], n3(:,502), t3x192(:,:,221))
  call heltable([8,24,192], n3(:,503), t3x192(:,:,222))
  call heltable([12,16,192], n3(:,504), t3x192(:,:,223))
  call heltable([8,24,192], n3(:,505), t3x192(:,:,224))
  call heltable([12,16,192], n3(:,506), t3x192(:,:,225))
  call heltable([12,16,192], n3(:,507), t3x192(:,:,226))
  call heltable([12,16,192], n3(:,508), t3x192(:,:,227))
  call heltable([24,8,192], n3(:,509), t3x192(:,:,228))
  call heltable([24,8,192], n3(:,510), t3x192(:,:,229))
  call heltable([24,8,192], n3(:,511), t3x192(:,:,230))
  call heltable([12,16,192], n3(:,512), t3x192(:,:,231))
  call heltable([8,24,192], n3(:,513), t3x192(:,:,232))
  call heltable([12,16,192], n3(:,514), t3x192(:,:,233))
  call heltable([12,16,192], n3(:,515), t3x192(:,:,234))
  call heltable([24,8,192], n3(:,516), t3x192(:,:,235))
  call heltable([24,8,192], n3(:,517), t3x192(:,:,236))
  call heltable([24,8,192], n3(:,518), t3x192(:,:,237))
  call heltable([8,24,192], n3(:,519), t3x192(:,:,238))
  call heltable([12,16,192], n3(:,520), t3x192(:,:,239))
  call heltable([12,16,192], n3(:,521), t3x192(:,:,240))
  call heltable([24,8,192], n3(:,522), t3x192(:,:,241))
  call heltable([8,24,192], n3(:,523), t3x192(:,:,242))
  call heltable([12,16,192], n3(:,524), t3x192(:,:,243))
  call heltable([8,24,192], n3(:,525), t3x192(:,:,244))
  call heltable([12,16,192], n3(:,526), t3x192(:,:,245))
  call heltable([12,16,192], n3(:,527), t3x192(:,:,246))
  call heltable([12,16,192], n3(:,528), t3x192(:,:,247))
  call heltable([8,24,192], n3(:,529), t3x192(:,:,248))
  call heltable([12,16,192], n3(:,530), t3x192(:,:,249))
  call heltable([8,24,192], n3(:,531), t3x192(:,:,250))
  call heltable([12,16,192], n3(:,532), t3x192(:,:,251))
  call heltable([12,16,192], n3(:,533), t3x192(:,:,252))
  call heltable([8,24,192], n3(:,534), t3x192(:,:,253))
  call heltable([12,16,192], n3(:,535), t3x192(:,:,254))
  call heltable([12,16,192], n3(:,536), t3x192(:,:,255))
  call heltable([12,16,192], n3(:,537), t3x192(:,:,256))
  call heltable([24,8,192], n3(:,538), t3x192(:,:,257))
  call heltable([12,16,192], n3(:,539), t3x192(:,:,258))
  call heltable([24,8,192], n3(:,540), t3x192(:,:,259))
  call heltable([12,16,192], n3(:,541), t3x192(:,:,260))
  call heltable([12,16,192], n3(:,542), t3x192(:,:,261))
  call heltable([12,16,192], n3(:,543), t3x192(:,:,262))
  call heltable([24,8,192], n3(:,544), t3x192(:,:,263))
  call heltable([12,16,192], n3(:,545), t3x192(:,:,264))
  call heltable([8,24,192], n3(:,546), t3x192(:,:,265))
  call heltable([8,24,192], n3(:,547), t3x192(:,:,266))
  call heltable([24,8,192], n3(:,548), t3x192(:,:,267))
  call heltable([12,16,192], n3(:,549), t3x192(:,:,268))
  call heltable([24,8,192], n3(:,550), t3x192(:,:,269))
  call heltable([24,8,192], n3(:,551), t3x192(:,:,270))
  call heltable([12,16,192], n3(:,552), t3x192(:,:,271))
  call heltable([12,16,192], n3(:,553), t3x192(:,:,272))
  call heltable([8,24,192], n3(:,554), t3x192(:,:,273))
  call heltable([24,8,192], n3(:,555), t3x192(:,:,274))
  call heltable([12,16,192], n3(:,556), t3x192(:,:,275))
  call heltable([24,8,192], n3(:,557), t3x192(:,:,276))
  call heltable([24,8,192], n3(:,558), t3x192(:,:,277))
  call heltable([12,16,192], n3(:,559), t3x192(:,:,278))
  call heltable([8,24,192], n3(:,560), t3x192(:,:,279))
  call heltable([12,16,192], n3(:,561), t3x192(:,:,280))
  call heltable([12,16,192], n3(:,562), t3x192(:,:,281))
  call heltable([12,16,192], n3(:,563), t3x192(:,:,282))
  call heltable([24,8,192], n3(:,564), t3x192(:,:,283))
  call heltable([12,16,192], n3(:,565), t3x192(:,:,284))
  call heltable([8,24,192], n3(:,566), t3x192(:,:,285))
  call heltable([8,24,192], n3(:,567), t3x192(:,:,286))
  call heltable([12,16,192], n3(:,568), t3x192(:,:,287))
  call heltable([12,16,192], n3(:,569), t3x192(:,:,288))
  call heltable([8,24,192], n3(:,570), t3x192(:,:,289))
  call heltable([12,16,192], n3(:,571), t3x192(:,:,290))
  call heltable([8,24,192], n3(:,572), t3x192(:,:,291))
  call heltable([12,16,192], n3(:,573), t3x192(:,:,292))
  call heltable([8,24,192], n3(:,574), t3x192(:,:,293))
  call heltable([12,16,192], n3(:,575), t3x192(:,:,294))
  call heltable([12,16,192], n3(:,576), t3x192(:,:,295))
  call heltable([12,16,192], n3(:,577), t3x192(:,:,296))
  call heltable([24,8,192], n3(:,578), t3x192(:,:,297))
  call heltable([24,8,192], n3(:,579), t3x192(:,:,298))
  call heltable([12,16,192], n3(:,580), t3x192(:,:,299))
  call heltable([24,8,192], n3(:,581), t3x192(:,:,300))
  call heltable([24,8,192], n3(:,582), t3x192(:,:,301))
  call heltable([12,16,192], n3(:,583), t3x192(:,:,302))
  call heltable([8,24,192], n3(:,584), t3x192(:,:,303))
  call heltable([24,8,192], n3(:,585), t3x192(:,:,304))
  call heltable([24,8,192], n3(:,586), t3x192(:,:,305))
  call heltable([12,16,192], n3(:,587), t3x192(:,:,306))
  call heltable([24,8,192], n3(:,588), t3x192(:,:,307))
  call heltable([8,24,192], n3(:,589), t3x192(:,:,308))
  call heltable([24,8,192], n3(:,590), t3x192(:,:,309))
  call heltable([12,16,192], n3(:,591), t3x192(:,:,310))
  call heltable([12,16,192], n3(:,592), t3x192(:,:,311))
  call heltable([24,8,192], n3(:,593), t3x192(:,:,312))
  call heltable([12,16,192], n3(:,594), t3x192(:,:,313))
  call heltable([24,8,192], n3(:,595), t3x192(:,:,314))
  call heltable([8,24,192], n3(:,596), t3x192(:,:,315))
  call heltable([12,16,192], n3(:,597), t3x192(:,:,316))
  call heltable([8,24,192], n3(:,598), t3x192(:,:,317))
  call heltable([12,16,192], n3(:,599), t3x192(:,:,318))
  call heltable([24,8,192], n3(:,600), t3x192(:,:,319))
  call heltable([12,16,192], n3(:,601), t3x192(:,:,320))
  call heltable([24,8,192], n3(:,602), t3x192(:,:,321))
  call heltable([12,16,192], n3(:,603), t3x192(:,:,322))
  call heltable([8,24,192], n3(:,604), t3x192(:,:,323))
  call heltable([8,24,192], n3(:,605), t3x192(:,:,324))
  call heltable([24,8,192], n3(:,606), t3x192(:,:,325))
  call heltable([12,16,192], n3(:,607), t3x192(:,:,326))
  call heltable([8,24,192], n3(:,608), t3x192(:,:,327))
  call heltable([12,16,192], n3(:,609), t3x192(:,:,328))
  call heltable([8,24,192], n3(:,610), t3x192(:,:,329))
  call heltable([12,16,192], n3(:,611), t3x192(:,:,330))
  call heltable([24,8,192], n3(:,612), t3x192(:,:,331))
  call heltable([12,16,192], n3(:,613), t3x192(:,:,332))
  call heltable([8,24,192], n3(:,614), t3x192(:,:,333))
  call heltable([8,24,192], n3(:,615), t3x192(:,:,334))
  call heltable([8,24,192], n3(:,616), t3x192(:,:,335))
  call heltable([12,16,192], n3(:,617), t3x192(:,:,336))
  call heltable([24,8,192], n3(:,618), t3x192(:,:,337))
  call heltable([24,8,192], n3(:,619), t3x192(:,:,338))
  call heltable([24,8,192], n3(:,620), t3x192(:,:,339))
  call heltable([12,16,192], n3(:,621), t3x192(:,:,340))
  call heltable([12,16,192], n3(:,622), t3x192(:,:,341))
  call heltable([24,8,192], n3(:,623), t3x192(:,:,342))
  call heltable([8,24,192], n3(:,624), t3x192(:,:,343))
  call heltable([24,8,192], n3(:,625), t3x192(:,:,344))
  call heltable([24,8,192], n3(:,626), t3x192(:,:,345))
  call heltable([24,8,192], n3(:,627), t3x192(:,:,346))
  call heltable([12,16,192], n3(:,628), t3x192(:,:,347))
  call heltable([8,24,192], n3(:,629), t3x192(:,:,348))
  call heltable([12,16,192], n3(:,630), t3x192(:,:,349))
  call heltable([24,8,192], n3(:,631), t3x192(:,:,350))
  call heltable([24,8,192], n3(:,632), t3x192(:,:,351))
  call heltable([12,16,192], n3(:,633), t3x192(:,:,352))
  call heltable([24,8,192], n3(:,634), t3x192(:,:,353))
  call heltable([12,16,192], n3(:,635), t3x192(:,:,354))
  call heltable([12,16,192], n3(:,636), t3x192(:,:,355))
  call heltable([8,24,192], n3(:,637), t3x192(:,:,356))
  call heltable([12,16,192], n3(:,638), t3x192(:,:,357))
  call heltable([8,24,192], n3(:,639), t3x192(:,:,358))
  call heltable([12,16,192], n3(:,640), t3x192(:,:,359))
  call heltable([24,8,192], n3(:,641), t3x192(:,:,360))
  call heltable([12,16,192], n3(:,642), t3x192(:,:,361))
  call heltable([24,8,192], n3(:,643), t3x192(:,:,362))
  call heltable([8,24,192], n3(:,644), t3x192(:,:,363))
  call heltable([8,24,192], n3(:,645), t3x192(:,:,364))
  call heltable([12,16,192], n3(:,646), t3x192(:,:,365))
  call heltable([24,8,192], n3(:,647), t3x192(:,:,366))
  call heltable([12,16,192], n3(:,648), t3x192(:,:,367))
  call heltable([8,24,192], n3(:,649), t3x192(:,:,368))
  call heltable([12,16,192], n3(:,650), t3x192(:,:,369))
  call heltable([8,24,192], n3(:,651), t3x192(:,:,370))
  call heltable([12,16,192], n3(:,652), t3x192(:,:,371))
  call heltable([24,8,192], n3(:,653), t3x192(:,:,372))
  call heltable([8,24,192], n3(:,654), t3x192(:,:,373))
  call heltable([8,24,192], n3(:,655), t3x192(:,:,374))
  call heltable([8,24,192], n3(:,656), t3x192(:,:,375))
  call heltable([12,16,192], n3(:,657), t3x192(:,:,376))
  call heltable([12,16,192], n3(:,658), t3x192(:,:,377))
  call heltable([12,16,192], n3(:,659), t3x192(:,:,378))
  call heltable([12,16,192], n3(:,660), t3x192(:,:,379))
  call heltable([12,16,192], n3(:,661), t3x192(:,:,380))
  call heltable([12,16,192], n3(:,662), t3x192(:,:,381))
  call heltable([12,16,192], n3(:,663), t3x192(:,:,382))
  call heltable([12,16,192], n3(:,664), t3x192(:,:,383))
  call heltable([12,16,192], n3(:,665), t3x192(:,:,384))
  call heltable([12,16,192], n3(:,666), t3x192(:,:,385))
  call heltable([12,16,192], n3(:,667), t3x192(:,:,386))
  call heltable([12,16,192], n3(:,668), t3x192(:,:,387))
  call heltable([12,16,192], n3(:,669), t3x192(:,:,388))
  call heltable([12,16,192], n3(:,670), t3x192(:,:,389))
  call heltable([12,16,192], n3(:,671), t3x192(:,:,390))
  call heltable([12,16,192], n3(:,672), t3x192(:,:,391))
  call heltable([12,16,192], n3(:,673), t3x192(:,:,392))
  call heltable([12,16,192], n3(:,674), t3x192(:,:,393))
  call heltable([12,16,192], n3(:,675), t3x192(:,:,394))
  call heltable([12,16,192], n3(:,676), t3x192(:,:,395))
  call heltable([12,16,192], n3(:,677), t3x192(:,:,396))
  call heltable([12,16,192], n3(:,678), t3x192(:,:,397))
  call heltable([12,16,192], n3(:,679), t3x192(:,:,398))
  call heltable([12,16,192], n3(:,680), t3x192(:,:,399))
  call heltable([12,16,192], n3(:,681), t3x192(:,:,400))
  call heltable([12,16,192], n3(:,682), t3x192(:,:,401))
  call heltable([12,16,192], n3(:,683), t3x192(:,:,402))
  call heltable([12,16,192], n3(:,684), t3x192(:,:,403))
  call heltable([12,16,192], n3(:,685), t3x192(:,:,404))
  call heltable([12,16,192], n3(:,686), t3x192(:,:,405))
  call heltable([12,16,192], n3(:,687), t3x192(:,:,406))
  call heltable([12,16,192], n3(:,688), t3x192(:,:,407))
  call heltable([12,16,192], n3(:,689), t3x192(:,:,408))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppzjjj_dddxdxzgg_1
