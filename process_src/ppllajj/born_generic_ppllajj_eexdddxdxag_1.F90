
module ol_external_ppllajj_eexdddxdxag_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppllajj_eexdddxdxag_1(8) = &
                     [ (dummy_counter, dummy_counter = 1, 8) ]
  integer, save :: external_perm_inv_ppllajj_eexdddxdxag_1(8) = &
                     [ (dummy_counter, dummy_counter = 1, 8) ]
  integer, save :: extcomb_perm_ppllajj_eexdddxdxag_1(0:37) = &
                     [ (dummy_counter, dummy_counter = 0, 37) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppllajj_eexdddxdxag_1(8) = &
                     [ 1, 2, 3, 3, 4, 4, 5, 6 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppllajj_eexdddxdxag_1(8) = &
                     [ 2, 2, 6, 6, 6, 6, 2, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppllajj_eexdddxdxag_1 = &
                     16
  integer, save :: channel_number_ppllajj_eexdddxdxag_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(8,256) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(256,8)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppllajj_eexdddxdxag_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 8
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppllajj_eexdddxdxag_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 8
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppllajj_eexdddxdxag_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(8)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppllajj_eexdddxdxag_1(8)
    external_perm_ppllajj_eexdddxdxag_1 = perm
    do i = 1, 8
      external_perm_inv_ppllajj_eexdddxdxag_1( &
        external_perm_ppllajj_eexdddxdxag_1(i)) = i
      particle_types_perm_ppllajj_eexdddxdxag_1(i) = &
        particle_types_ppllajj_eexdddxdxag_1( &
        external_perm_ppllajj_eexdddxdxag_1(i))
    end do
    do i = 1, 8
      do j = 1, i
        if (external_perm_ppllajj_eexdddxdxag_1(i) >= &
          external_perm_ppllajj_eexdddxdxag_1(j)) then
          ii = external_perm_ppllajj_eexdddxdxag_1(i)
          jj = external_perm_ppllajj_eexdddxdxag_1(j)
        else
          ii = external_perm_ppllajj_eexdddxdxag_1(j)
          jj = external_perm_ppllajj_eexdddxdxag_1(i)
        end if
        extcomb_perm_ppllajj_eexdddxdxag_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppllajj_eexdddxdxag_1 = &
      average_factors_ppllajj_eexdddxdxag_1( &
      external_perm_ppllajj_eexdddxdxag_1(1)) &
      * average_factors_ppllajj_eexdddxdxag_1( &
      external_perm_ppllajj_eexdddxdxag_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 8
        average_factor_ppllajj_eexdddxdxag_1 = &
          average_factor_ppllajj_eexdddxdxag_1 &
          * factorial(count(particle_types_perm_ppllajj_eexdddxdxag_1(3:8) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppllajj_eexdddxdxag_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(8)
    integer :: f_perm(8)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppllajj_eexdddxdxag_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(8)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(8)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rZERO, rZERO, rZERO, rZERO ]
    do i = 1, 8
      m_ex(i) = m_ex_orig(external_perm_ppllajj_eexdddxdxag_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppllajj_eexdddxdxag_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(8)
    real(DREALKIND) :: f_m_ex(8)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppllajj_eexdddxdxag_1")
    use KIND_TYPES, only: DREALKIND
    use ol_kinematics_/**/DREALKIND, only: rambo_generic => rambo
    implicit none
    real(DREALKIND), intent(in) :: sqrt_s
    real(DREALKIND), intent(out) :: p_rambo(0:3,8)
    real(DREALKIND) :: m_ex(8)
    call get_masses(m_ex)
    call rambo_generic(sqrt_s, m_ex, p_rambo)
  end subroutine rambo


  subroutine rambo_c(sqrt_s, p_rambo) &
      & bind(c,name="ol_rambo_ppllajj_eexdddxdxag_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in) :: sqrt_s
    real(c_double), intent(out) :: p_rambo(0:3,8)
    real(DREALKIND) :: f_sqrt_s
    real(DREALKIND) :: f_p_rambo(0:3,8)
    f_sqrt_s = sqrt_s
    call rambo(f_sqrt_s, f_p_rambo)
    p_rambo = f_p_rambo
  end subroutine rambo_c


  subroutine hel_init
    implicit none
    integer :: binpos, flip, binco
    hel_not_initialised = .false.
    ! helicity configurations for this process
  H(:,  1) = [ -1, -1, -1, -1, -1, -1, -1, -1 ]
  H(:,  2) = [ -1, -1, -1, -1, -1, -1, -1,  1 ]
  H(:,  3) = [ -1, -1, -1, -1, -1, -1,  1, -1 ]
  H(:,  4) = [ -1, -1, -1, -1, -1, -1,  1,  1 ]
  H(:,  5) = [ -1, -1, -1, -1, -1,  1, -1, -1 ]
  H(:,  6) = [ -1, -1, -1, -1, -1,  1, -1,  1 ]
  H(:,  7) = [ -1, -1, -1, -1, -1,  1,  1, -1 ]
  H(:,  8) = [ -1, -1, -1, -1, -1,  1,  1,  1 ]
  H(:,  9) = [ -1, -1, -1, -1,  1, -1, -1, -1 ]
  H(:, 10) = [ -1, -1, -1, -1,  1, -1, -1,  1 ]
  H(:, 11) = [ -1, -1, -1, -1,  1, -1,  1, -1 ]
  H(:, 12) = [ -1, -1, -1, -1,  1, -1,  1,  1 ]
  H(:, 13) = [ -1, -1, -1, -1,  1,  1, -1, -1 ]
  H(:, 14) = [ -1, -1, -1, -1,  1,  1, -1,  1 ]
  H(:, 15) = [ -1, -1, -1, -1,  1,  1,  1, -1 ]
  H(:, 16) = [ -1, -1, -1, -1,  1,  1,  1,  1 ]
  H(:, 17) = [ -1, -1, -1,  1, -1, -1, -1, -1 ]
  H(:, 18) = [ -1, -1, -1,  1, -1, -1, -1,  1 ]
  H(:, 19) = [ -1, -1, -1,  1, -1, -1,  1, -1 ]
  H(:, 20) = [ -1, -1, -1,  1, -1, -1,  1,  1 ]
  H(:, 21) = [ -1, -1, -1,  1, -1,  1, -1, -1 ]
  H(:, 22) = [ -1, -1, -1,  1, -1,  1, -1,  1 ]
  H(:, 23) = [ -1, -1, -1,  1, -1,  1,  1, -1 ]
  H(:, 24) = [ -1, -1, -1,  1, -1,  1,  1,  1 ]
  H(:, 25) = [ -1, -1, -1,  1,  1, -1, -1, -1 ]
  H(:, 26) = [ -1, -1, -1,  1,  1, -1, -1,  1 ]
  H(:, 27) = [ -1, -1, -1,  1,  1, -1,  1, -1 ]
  H(:, 28) = [ -1, -1, -1,  1,  1, -1,  1,  1 ]
  H(:, 29) = [ -1, -1, -1,  1,  1,  1, -1, -1 ]
  H(:, 30) = [ -1, -1, -1,  1,  1,  1, -1,  1 ]
  H(:, 31) = [ -1, -1, -1,  1,  1,  1,  1, -1 ]
  H(:, 32) = [ -1, -1, -1,  1,  1,  1,  1,  1 ]
  H(:, 33) = [ -1, -1,  1, -1, -1, -1, -1, -1 ]
  H(:, 34) = [ -1, -1,  1, -1, -1, -1, -1,  1 ]
  H(:, 35) = [ -1, -1,  1, -1, -1, -1,  1, -1 ]
  H(:, 36) = [ -1, -1,  1, -1, -1, -1,  1,  1 ]
  H(:, 37) = [ -1, -1,  1, -1, -1,  1, -1, -1 ]
  H(:, 38) = [ -1, -1,  1, -1, -1,  1, -1,  1 ]
  H(:, 39) = [ -1, -1,  1, -1, -1,  1,  1, -1 ]
  H(:, 40) = [ -1, -1,  1, -1, -1,  1,  1,  1 ]
  H(:, 41) = [ -1, -1,  1, -1,  1, -1, -1, -1 ]
  H(:, 42) = [ -1, -1,  1, -1,  1, -1, -1,  1 ]
  H(:, 43) = [ -1, -1,  1, -1,  1, -1,  1, -1 ]
  H(:, 44) = [ -1, -1,  1, -1,  1, -1,  1,  1 ]
  H(:, 45) = [ -1, -1,  1, -1,  1,  1, -1, -1 ]
  H(:, 46) = [ -1, -1,  1, -1,  1,  1, -1,  1 ]
  H(:, 47) = [ -1, -1,  1, -1,  1,  1,  1, -1 ]
  H(:, 48) = [ -1, -1,  1, -1,  1,  1,  1,  1 ]
  H(:, 49) = [ -1, -1,  1,  1, -1, -1, -1, -1 ]
  H(:, 50) = [ -1, -1,  1,  1, -1, -1, -1,  1 ]
  H(:, 51) = [ -1, -1,  1,  1, -1, -1,  1, -1 ]
  H(:, 52) = [ -1, -1,  1,  1, -1, -1,  1,  1 ]
  H(:, 53) = [ -1, -1,  1,  1, -1,  1, -1, -1 ]
  H(:, 54) = [ -1, -1,  1,  1, -1,  1, -1,  1 ]
  H(:, 55) = [ -1, -1,  1,  1, -1,  1,  1, -1 ]
  H(:, 56) = [ -1, -1,  1,  1, -1,  1,  1,  1 ]
  H(:, 57) = [ -1, -1,  1,  1,  1, -1, -1, -1 ]
  H(:, 58) = [ -1, -1,  1,  1,  1, -1, -1,  1 ]
  H(:, 59) = [ -1, -1,  1,  1,  1, -1,  1, -1 ]
  H(:, 60) = [ -1, -1,  1,  1,  1, -1,  1,  1 ]
  H(:, 61) = [ -1, -1,  1,  1,  1,  1, -1, -1 ]
  H(:, 62) = [ -1, -1,  1,  1,  1,  1, -1,  1 ]
  H(:, 63) = [ -1, -1,  1,  1,  1,  1,  1, -1 ]
  H(:, 64) = [ -1, -1,  1,  1,  1,  1,  1,  1 ]
  H(:, 65) = [ -1,  1, -1, -1, -1, -1, -1, -1 ]
  H(:, 66) = [ -1,  1, -1, -1, -1, -1, -1,  1 ]
  H(:, 67) = [ -1,  1, -1, -1, -1, -1,  1, -1 ]
  H(:, 68) = [ -1,  1, -1, -1, -1, -1,  1,  1 ]
  H(:, 69) = [ -1,  1, -1, -1, -1,  1, -1, -1 ]
  H(:, 70) = [ -1,  1, -1, -1, -1,  1, -1,  1 ]
  H(:, 71) = [ -1,  1, -1, -1, -1,  1,  1, -1 ]
  H(:, 72) = [ -1,  1, -1, -1, -1,  1,  1,  1 ]
  H(:, 73) = [ -1,  1, -1, -1,  1, -1, -1, -1 ]
  H(:, 74) = [ -1,  1, -1, -1,  1, -1, -1,  1 ]
  H(:, 75) = [ -1,  1, -1, -1,  1, -1,  1, -1 ]
  H(:, 76) = [ -1,  1, -1, -1,  1, -1,  1,  1 ]
  H(:, 77) = [ -1,  1, -1, -1,  1,  1, -1, -1 ]
  H(:, 78) = [ -1,  1, -1, -1,  1,  1, -1,  1 ]
  H(:, 79) = [ -1,  1, -1, -1,  1,  1,  1, -1 ]
  H(:, 80) = [ -1,  1, -1, -1,  1,  1,  1,  1 ]
  H(:, 81) = [ -1,  1, -1,  1, -1, -1, -1, -1 ]
  H(:, 82) = [ -1,  1, -1,  1, -1, -1, -1,  1 ]
  H(:, 83) = [ -1,  1, -1,  1, -1, -1,  1, -1 ]
  H(:, 84) = [ -1,  1, -1,  1, -1, -1,  1,  1 ]
  H(:, 85) = [ -1,  1, -1,  1, -1,  1, -1, -1 ]
  H(:, 86) = [ -1,  1, -1,  1, -1,  1, -1,  1 ]
  H(:, 87) = [ -1,  1, -1,  1, -1,  1,  1, -1 ]
  H(:, 88) = [ -1,  1, -1,  1, -1,  1,  1,  1 ]
  H(:, 89) = [ -1,  1, -1,  1,  1, -1, -1, -1 ]
  H(:, 90) = [ -1,  1, -1,  1,  1, -1, -1,  1 ]
  H(:, 91) = [ -1,  1, -1,  1,  1, -1,  1, -1 ]
  H(:, 92) = [ -1,  1, -1,  1,  1, -1,  1,  1 ]
  H(:, 93) = [ -1,  1, -1,  1,  1,  1, -1, -1 ]
  H(:, 94) = [ -1,  1, -1,  1,  1,  1, -1,  1 ]
  H(:, 95) = [ -1,  1, -1,  1,  1,  1,  1, -1 ]
  H(:, 96) = [ -1,  1, -1,  1,  1,  1,  1,  1 ]
  H(:, 97) = [ -1,  1,  1, -1, -1, -1, -1, -1 ]
  H(:, 98) = [ -1,  1,  1, -1, -1, -1, -1,  1 ]
  H(:, 99) = [ -1,  1,  1, -1, -1, -1,  1, -1 ]
  H(:,100) = [ -1,  1,  1, -1, -1, -1,  1,  1 ]
  H(:,101) = [ -1,  1,  1, -1, -1,  1, -1, -1 ]
  H(:,102) = [ -1,  1,  1, -1, -1,  1, -1,  1 ]
  H(:,103) = [ -1,  1,  1, -1, -1,  1,  1, -1 ]
  H(:,104) = [ -1,  1,  1, -1, -1,  1,  1,  1 ]
  H(:,105) = [ -1,  1,  1, -1,  1, -1, -1, -1 ]
  H(:,106) = [ -1,  1,  1, -1,  1, -1, -1,  1 ]
  H(:,107) = [ -1,  1,  1, -1,  1, -1,  1, -1 ]
  H(:,108) = [ -1,  1,  1, -1,  1, -1,  1,  1 ]
  H(:,109) = [ -1,  1,  1, -1,  1,  1, -1, -1 ]
  H(:,110) = [ -1,  1,  1, -1,  1,  1, -1,  1 ]
  H(:,111) = [ -1,  1,  1, -1,  1,  1,  1, -1 ]
  H(:,112) = [ -1,  1,  1, -1,  1,  1,  1,  1 ]
  H(:,113) = [ -1,  1,  1,  1, -1, -1, -1, -1 ]
  H(:,114) = [ -1,  1,  1,  1, -1, -1, -1,  1 ]
  H(:,115) = [ -1,  1,  1,  1, -1, -1,  1, -1 ]
  H(:,116) = [ -1,  1,  1,  1, -1, -1,  1,  1 ]
  H(:,117) = [ -1,  1,  1,  1, -1,  1, -1, -1 ]
  H(:,118) = [ -1,  1,  1,  1, -1,  1, -1,  1 ]
  H(:,119) = [ -1,  1,  1,  1, -1,  1,  1, -1 ]
  H(:,120) = [ -1,  1,  1,  1, -1,  1,  1,  1 ]
  H(:,121) = [ -1,  1,  1,  1,  1, -1, -1, -1 ]
  H(:,122) = [ -1,  1,  1,  1,  1, -1, -1,  1 ]
  H(:,123) = [ -1,  1,  1,  1,  1, -1,  1, -1 ]
  H(:,124) = [ -1,  1,  1,  1,  1, -1,  1,  1 ]
  H(:,125) = [ -1,  1,  1,  1,  1,  1, -1, -1 ]
  H(:,126) = [ -1,  1,  1,  1,  1,  1, -1,  1 ]
  H(:,127) = [ -1,  1,  1,  1,  1,  1,  1, -1 ]
  H(:,128) = [ -1,  1,  1,  1,  1,  1,  1,  1 ]
  H(:,129) = [  1, -1, -1, -1, -1, -1, -1, -1 ]
  H(:,130) = [  1, -1, -1, -1, -1, -1, -1,  1 ]
  H(:,131) = [  1, -1, -1, -1, -1, -1,  1, -1 ]
  H(:,132) = [  1, -1, -1, -1, -1, -1,  1,  1 ]
  H(:,133) = [  1, -1, -1, -1, -1,  1, -1, -1 ]
  H(:,134) = [  1, -1, -1, -1, -1,  1, -1,  1 ]
  H(:,135) = [  1, -1, -1, -1, -1,  1,  1, -1 ]
  H(:,136) = [  1, -1, -1, -1, -1,  1,  1,  1 ]
  H(:,137) = [  1, -1, -1, -1,  1, -1, -1, -1 ]
  H(:,138) = [  1, -1, -1, -1,  1, -1, -1,  1 ]
  H(:,139) = [  1, -1, -1, -1,  1, -1,  1, -1 ]
  H(:,140) = [  1, -1, -1, -1,  1, -1,  1,  1 ]
  H(:,141) = [  1, -1, -1, -1,  1,  1, -1, -1 ]
  H(:,142) = [  1, -1, -1, -1,  1,  1, -1,  1 ]
  H(:,143) = [  1, -1, -1, -1,  1,  1,  1, -1 ]
  H(:,144) = [  1, -1, -1, -1,  1,  1,  1,  1 ]
  H(:,145) = [  1, -1, -1,  1, -1, -1, -1, -1 ]
  H(:,146) = [  1, -1, -1,  1, -1, -1, -1,  1 ]
  H(:,147) = [  1, -1, -1,  1, -1, -1,  1, -1 ]
  H(:,148) = [  1, -1, -1,  1, -1, -1,  1,  1 ]
  H(:,149) = [  1, -1, -1,  1, -1,  1, -1, -1 ]
  H(:,150) = [  1, -1, -1,  1, -1,  1, -1,  1 ]
  H(:,151) = [  1, -1, -1,  1, -1,  1,  1, -1 ]
  H(:,152) = [  1, -1, -1,  1, -1,  1,  1,  1 ]
  H(:,153) = [  1, -1, -1,  1,  1, -1, -1, -1 ]
  H(:,154) = [  1, -1, -1,  1,  1, -1, -1,  1 ]
  H(:,155) = [  1, -1, -1,  1,  1, -1,  1, -1 ]
  H(:,156) = [  1, -1, -1,  1,  1, -1,  1,  1 ]
  H(:,157) = [  1, -1, -1,  1,  1,  1, -1, -1 ]
  H(:,158) = [  1, -1, -1,  1,  1,  1, -1,  1 ]
  H(:,159) = [  1, -1, -1,  1,  1,  1,  1, -1 ]
  H(:,160) = [  1, -1, -1,  1,  1,  1,  1,  1 ]
  H(:,161) = [  1, -1,  1, -1, -1, -1, -1, -1 ]
  H(:,162) = [  1, -1,  1, -1, -1, -1, -1,  1 ]
  H(:,163) = [  1, -1,  1, -1, -1, -1,  1, -1 ]
  H(:,164) = [  1, -1,  1, -1, -1, -1,  1,  1 ]
  H(:,165) = [  1, -1,  1, -1, -1,  1, -1, -1 ]
  H(:,166) = [  1, -1,  1, -1, -1,  1, -1,  1 ]
  H(:,167) = [  1, -1,  1, -1, -1,  1,  1, -1 ]
  H(:,168) = [  1, -1,  1, -1, -1,  1,  1,  1 ]
  H(:,169) = [  1, -1,  1, -1,  1, -1, -1, -1 ]
  H(:,170) = [  1, -1,  1, -1,  1, -1, -1,  1 ]
  H(:,171) = [  1, -1,  1, -1,  1, -1,  1, -1 ]
  H(:,172) = [  1, -1,  1, -1,  1, -1,  1,  1 ]
  H(:,173) = [  1, -1,  1, -1,  1,  1, -1, -1 ]
  H(:,174) = [  1, -1,  1, -1,  1,  1, -1,  1 ]
  H(:,175) = [  1, -1,  1, -1,  1,  1,  1, -1 ]
  H(:,176) = [  1, -1,  1, -1,  1,  1,  1,  1 ]
  H(:,177) = [  1, -1,  1,  1, -1, -1, -1, -1 ]
  H(:,178) = [  1, -1,  1,  1, -1, -1, -1,  1 ]
  H(:,179) = [  1, -1,  1,  1, -1, -1,  1, -1 ]
  H(:,180) = [  1, -1,  1,  1, -1, -1,  1,  1 ]
  H(:,181) = [  1, -1,  1,  1, -1,  1, -1, -1 ]
  H(:,182) = [  1, -1,  1,  1, -1,  1, -1,  1 ]
  H(:,183) = [  1, -1,  1,  1, -1,  1,  1, -1 ]
  H(:,184) = [  1, -1,  1,  1, -1,  1,  1,  1 ]
  H(:,185) = [  1, -1,  1,  1,  1, -1, -1, -1 ]
  H(:,186) = [  1, -1,  1,  1,  1, -1, -1,  1 ]
  H(:,187) = [  1, -1,  1,  1,  1, -1,  1, -1 ]
  H(:,188) = [  1, -1,  1,  1,  1, -1,  1,  1 ]
  H(:,189) = [  1, -1,  1,  1,  1,  1, -1, -1 ]
  H(:,190) = [  1, -1,  1,  1,  1,  1, -1,  1 ]
  H(:,191) = [  1, -1,  1,  1,  1,  1,  1, -1 ]
  H(:,192) = [  1, -1,  1,  1,  1,  1,  1,  1 ]
  H(:,193) = [  1,  1, -1, -1, -1, -1, -1, -1 ]
  H(:,194) = [  1,  1, -1, -1, -1, -1, -1,  1 ]
  H(:,195) = [  1,  1, -1, -1, -1, -1,  1, -1 ]
  H(:,196) = [  1,  1, -1, -1, -1, -1,  1,  1 ]
  H(:,197) = [  1,  1, -1, -1, -1,  1, -1, -1 ]
  H(:,198) = [  1,  1, -1, -1, -1,  1, -1,  1 ]
  H(:,199) = [  1,  1, -1, -1, -1,  1,  1, -1 ]
  H(:,200) = [  1,  1, -1, -1, -1,  1,  1,  1 ]
  H(:,201) = [  1,  1, -1, -1,  1, -1, -1, -1 ]
  H(:,202) = [  1,  1, -1, -1,  1, -1, -1,  1 ]
  H(:,203) = [  1,  1, -1, -1,  1, -1,  1, -1 ]
  H(:,204) = [  1,  1, -1, -1,  1, -1,  1,  1 ]
  H(:,205) = [  1,  1, -1, -1,  1,  1, -1, -1 ]
  H(:,206) = [  1,  1, -1, -1,  1,  1, -1,  1 ]
  H(:,207) = [  1,  1, -1, -1,  1,  1,  1, -1 ]
  H(:,208) = [  1,  1, -1, -1,  1,  1,  1,  1 ]
  H(:,209) = [  1,  1, -1,  1, -1, -1, -1, -1 ]
  H(:,210) = [  1,  1, -1,  1, -1, -1, -1,  1 ]
  H(:,211) = [  1,  1, -1,  1, -1, -1,  1, -1 ]
  H(:,212) = [  1,  1, -1,  1, -1, -1,  1,  1 ]
  H(:,213) = [  1,  1, -1,  1, -1,  1, -1, -1 ]
  H(:,214) = [  1,  1, -1,  1, -1,  1, -1,  1 ]
  H(:,215) = [  1,  1, -1,  1, -1,  1,  1, -1 ]
  H(:,216) = [  1,  1, -1,  1, -1,  1,  1,  1 ]
  H(:,217) = [  1,  1, -1,  1,  1, -1, -1, -1 ]
  H(:,218) = [  1,  1, -1,  1,  1, -1, -1,  1 ]
  H(:,219) = [  1,  1, -1,  1,  1, -1,  1, -1 ]
  H(:,220) = [  1,  1, -1,  1,  1, -1,  1,  1 ]
  H(:,221) = [  1,  1, -1,  1,  1,  1, -1, -1 ]
  H(:,222) = [  1,  1, -1,  1,  1,  1, -1,  1 ]
  H(:,223) = [  1,  1, -1,  1,  1,  1,  1, -1 ]
  H(:,224) = [  1,  1, -1,  1,  1,  1,  1,  1 ]
  H(:,225) = [  1,  1,  1, -1, -1, -1, -1, -1 ]
  H(:,226) = [  1,  1,  1, -1, -1, -1, -1,  1 ]
  H(:,227) = [  1,  1,  1, -1, -1, -1,  1, -1 ]
  H(:,228) = [  1,  1,  1, -1, -1, -1,  1,  1 ]
  H(:,229) = [  1,  1,  1, -1, -1,  1, -1, -1 ]
  H(:,230) = [  1,  1,  1, -1, -1,  1, -1,  1 ]
  H(:,231) = [  1,  1,  1, -1, -1,  1,  1, -1 ]
  H(:,232) = [  1,  1,  1, -1, -1,  1,  1,  1 ]
  H(:,233) = [  1,  1,  1, -1,  1, -1, -1, -1 ]
  H(:,234) = [  1,  1,  1, -1,  1, -1, -1,  1 ]
  H(:,235) = [  1,  1,  1, -1,  1, -1,  1, -1 ]
  H(:,236) = [  1,  1,  1, -1,  1, -1,  1,  1 ]
  H(:,237) = [  1,  1,  1, -1,  1,  1, -1, -1 ]
  H(:,238) = [  1,  1,  1, -1,  1,  1, -1,  1 ]
  H(:,239) = [  1,  1,  1, -1,  1,  1,  1, -1 ]
  H(:,240) = [  1,  1,  1, -1,  1,  1,  1,  1 ]
  H(:,241) = [  1,  1,  1,  1, -1, -1, -1, -1 ]
  H(:,242) = [  1,  1,  1,  1, -1, -1, -1,  1 ]
  H(:,243) = [  1,  1,  1,  1, -1, -1,  1, -1 ]
  H(:,244) = [  1,  1,  1,  1, -1, -1,  1,  1 ]
  H(:,245) = [  1,  1,  1,  1, -1,  1, -1, -1 ]
  H(:,246) = [  1,  1,  1,  1, -1,  1, -1,  1 ]
  H(:,247) = [  1,  1,  1,  1, -1,  1,  1, -1 ]
  H(:,248) = [  1,  1,  1,  1, -1,  1,  1,  1 ]
  H(:,249) = [  1,  1,  1,  1,  1, -1, -1, -1 ]
  H(:,250) = [  1,  1,  1,  1,  1, -1, -1,  1 ]
  H(:,251) = [  1,  1,  1,  1,  1, -1,  1, -1 ]
  H(:,252) = [  1,  1,  1,  1,  1, -1,  1,  1 ]
  H(:,253) = [  1,  1,  1,  1,  1,  1, -1, -1 ]
  H(:,254) = [  1,  1,  1,  1,  1,  1, -1,  1 ]
  H(:,255) = [  1,  1,  1,  1,  1,  1,  1, -1 ]
  H(:,256) = [  1,  1,  1,  1,  1,  1,  1,  1 ]

  H_HC(:,7) = [ ((((2*(binco-1)+flip)*2+binpos, flip = 0, 1), binpos = 1, 2), binco = 1, 256/2/2) ]
  H_HC(:,8) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 256/1/2) ]
  end subroutine hel_init

end module ol_external_ppllajj_eexdddxdxag_1


module colour_basis_ppllajj_eexdddxdxag_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(8) = [0,0,1,1,1,1,0,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppllajj_eexdddxdxag_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(8)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 4
    ncoupl = 1
    maxpows = 1
    nhel = 256
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_ppllajj_eexdddxdxag_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,4)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([6], [1,1])
#endif
#if 4 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [1,110,3,1,112,3,1,116,3,1,118,3], &
      [3,4])
#endif
  end subroutine tree_colbasis

end module colour_basis_ppllajj_eexdddxdxag_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppllajj_eexdddxdxag_1(perm)
  use ol_external_ppllajj_eexdddxdxag_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(8)
  call set_permutation(perm)
end subroutine set_permutation_ppllajj_eexdddxdxag_1

! **********************************************************************
module ol_heltables_ppllajj_eexdddxdxag_1
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
    H7(2) = [-1,1], &
    H8(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(141), n3(3,1100)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,16), t3x64(2,64,40), t3x8(2,8,48), t3x32(2,32,76), t3x16(2,16,168), t3x256(2,256,752)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(256,8)
  integer,           save :: exthel(256,8)
  integer,           save :: firstpol(8)

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
  call heltable([2,2,4], n3(:,4), t3x4(:,:,4))
  n2(1) = 4
  n2(2) = 4
  call heltable([4,4,16], n3(:,5), t3x16(:,:,1))
  call heltable([4,4,16], n3(:,6), t3x16(:,:,2))
  n2(3) = 16
  call heltable([2,2,4], n3(:,7), t3x4(:,:,5))
  n2(4) = 4
  call heltable([4,4,16], n3(:,8), t3x16(:,:,3))
  n2(5) = 16
  call heltable([4,4,16], n3(:,9), t3x16(:,:,4))
  call heltable([4,4,16], n3(:,10), t3x16(:,:,5))
  n2(6) = 16
  call heltable([4,4,16], n3(:,11), t3x16(:,:,6))
  n2(7) = 16
  call heltable([2,4,8], n3(:,12), t3x8(:,:,1))
  call heltable([4,2,8], n3(:,13), t3x8(:,:,2))
  n2(8) = 8
  call heltable([4,8,32], n3(:,14), t3x32(:,:,1))
  call heltable([2,4,8], n3(:,15), t3x8(:,:,3))
  n2(9) = 8
  call heltable([4,8,32], n3(:,16), t3x32(:,:,2))
  call heltable([2,4,8], n3(:,17), t3x8(:,:,4))
  n2(10) = 8
  call heltable([8,4,32], n3(:,18), t3x32(:,:,3))
  call heltable([8,4,32], n3(:,19), t3x32(:,:,4))
  n2(11) = 16
  call heltable([8,2,16], n3(:,20), t3x16(:,:,7))
  call heltable([8,2,16], n3(:,21), t3x16(:,:,8))
  call heltable([2,4,8], n3(:,22), t3x8(:,:,5))
  n2(12) = 8
  call heltable([8,4,32], n3(:,23), t3x32(:,:,5))
  call heltable([8,4,32], n3(:,24), t3x32(:,:,6))
  call heltable([8,2,16], n3(:,25), t3x16(:,:,9))
  call heltable([2,8,16], n3(:,26), t3x16(:,:,10))
  call heltable([2,2,4], n3(:,27), t3x4(:,:,6))
  call heltable([2,2,4], n3(:,28), t3x4(:,:,7))
  n2(13) = 4
  n2(14) = 4
  call heltable([4,4,16], n3(:,29), t3x16(:,:,11))
  call heltable([4,4,16], n3(:,30), t3x16(:,:,12))
  n2(15) = 16
  call heltable([4,4,16], n3(:,31), t3x16(:,:,13))
  n2(16) = 16
  call heltable([4,4,16], n3(:,32), t3x16(:,:,14))
  call heltable([4,4,16], n3(:,33), t3x16(:,:,15))
  n2(17) = 16
  call heltable([4,4,16], n3(:,34), t3x16(:,:,16))
  n2(18) = 16
  call heltable([4,2,8], n3(:,35), t3x8(:,:,6))
  n2(19) = 8
  call heltable([8,4,32], n3(:,36), t3x32(:,:,7))
  call heltable([4,2,8], n3(:,37), t3x8(:,:,7))
  n2(20) = 8
  call heltable([8,4,32], n3(:,38), t3x32(:,:,8))
  n2(21) = 16
  call heltable([2,8,16], n3(:,39), t3x16(:,:,17))
  call heltable([2,8,16], n3(:,40), t3x16(:,:,18))
  call heltable([4,2,8], n3(:,41), t3x8(:,:,8))
  n2(22) = 8
  call heltable([4,8,32], n3(:,42), t3x32(:,:,9))
  call heltable([4,8,32], n3(:,43), t3x32(:,:,10))
  call heltable([4,2,8], n3(:,44), t3x8(:,:,9))
  n2(23) = 8
  call heltable([2,8,16], n3(:,45), t3x16(:,:,19))
  call heltable([4,8,32], n3(:,46), t3x32(:,:,11))
  call heltable([4,8,32], n3(:,47), t3x32(:,:,12))
  call heltable([8,2,16], n3(:,48), t3x16(:,:,20))
  call heltable([2,4,8], n3(:,49), t3x8(:,:,10))
  n2(24) = 8
  n2(25) = 16
  call heltable([8,2,16], n3(:,50), t3x16(:,:,21))
  call heltable([8,2,16], n3(:,51), t3x16(:,:,22))
  call heltable([8,2,16], n3(:,52), t3x16(:,:,23))
  n2(26) = 16
  call heltable([2,8,16], n3(:,53), t3x16(:,:,24))
  call heltable([2,8,16], n3(:,54), t3x16(:,:,25))
  call heltable([4,2,8], n3(:,55), t3x8(:,:,11))
  n2(27) = 8
  call heltable([2,8,16], n3(:,56), t3x16(:,:,26))
  n2(28) = 16
  n2(29) = 16
  n2(30) = 16
  n2(31) = 16
  n2(32) = 16
  n2(33) = 16
  n2(34) = 16
  n2(35) = 16
  call heltable([2,2,4], n3(:,57), t3x4(:,:,8))
  call heltable([2,2,4], n3(:,58), t3x4(:,:,9))
  n2(36) = 4
  call heltable([4,4,16], n3(:,59), t3x16(:,:,27))
  call heltable([4,4,16], n3(:,60), t3x16(:,:,28))
  n2(37) = 16
  call heltable([4,4,16], n3(:,61), t3x16(:,:,29))
  n2(38) = 16
  call heltable([4,4,16], n3(:,62), t3x16(:,:,30))
  call heltable([2,4,8], n3(:,63), t3x8(:,:,12))
  n2(39) = 8
  call heltable([8,4,32], n3(:,64), t3x32(:,:,13))
  call heltable([8,4,32], n3(:,65), t3x32(:,:,14))
  call heltable([4,2,8], n3(:,66), t3x8(:,:,13))
  call heltable([4,8,32], n3(:,67), t3x32(:,:,15))
  call heltable([4,8,32], n3(:,68), t3x32(:,:,16))
  n2(40) = 16
  call heltable([2,4,8], n3(:,69), t3x8(:,:,14))
  n2(41) = 8
  call heltable([8,4,32], n3(:,70), t3x32(:,:,17))
  call heltable([8,4,32], n3(:,71), t3x32(:,:,18))
  call heltable([8,2,16], n3(:,72), t3x16(:,:,31))
  call heltable([2,8,16], n3(:,73), t3x16(:,:,32))
  call heltable([2,2,4], n3(:,74), t3x4(:,:,10))
  n2(42) = 4
  call heltable([4,4,16], n3(:,75), t3x16(:,:,33))
  call heltable([4,4,16], n3(:,76), t3x16(:,:,34))
  n2(43) = 16
  call heltable([4,4,16], n3(:,77), t3x16(:,:,35))
  n2(44) = 16
  call heltable([4,4,16], n3(:,78), t3x16(:,:,36))
  call heltable([4,2,8], n3(:,79), t3x8(:,:,15))
  n2(45) = 8
  n2(46) = 16
  call heltable([2,8,16], n3(:,80), t3x16(:,:,37))
  call heltable([4,2,8], n3(:,81), t3x8(:,:,16))
  n2(47) = 8
  call heltable([2,8,16], n3(:,82), t3x16(:,:,38))
  call heltable([8,4,32], n3(:,83), t3x32(:,:,19))
  call heltable([8,4,32], n3(:,84), t3x32(:,:,20))
  call heltable([4,8,32], n3(:,85), t3x32(:,:,21))
  call heltable([4,8,32], n3(:,86), t3x32(:,:,22))
  call heltable([4,2,8], n3(:,87), t3x8(:,:,17))
  n2(48) = 8
  call heltable([2,8,16], n3(:,88), t3x16(:,:,39))
  call heltable([4,8,32], n3(:,89), t3x32(:,:,23))
  call heltable([4,8,32], n3(:,90), t3x32(:,:,24))
  call heltable([8,2,16], n3(:,91), t3x16(:,:,40))
  call heltable([2,4,8], n3(:,92), t3x8(:,:,18))
  n2(49) = 8
  n2(50) = 16
  call heltable([8,2,16], n3(:,93), t3x16(:,:,41))
  n2(51) = 16
  call heltable([2,8,16], n3(:,94), t3x16(:,:,42))
  call heltable([2,8,16], n3(:,95), t3x16(:,:,43))
  call heltable([2,8,16], n3(:,96), t3x16(:,:,44))
  n2(52) = 16
  n2(53) = 16
  n2(54) = 16
  n2(55) = 16
  n2(56) = 16
  n2(57) = 16
  call heltable([2,2,4], n3(:,97), t3x4(:,:,11))
  call heltable([2,2,4], n3(:,98), t3x4(:,:,12))
  n2(58) = 4
  call heltable([4,4,16], n3(:,99), t3x16(:,:,45))
  call heltable([4,4,16], n3(:,100), t3x16(:,:,46))
  call heltable([4,4,16], n3(:,101), t3x16(:,:,47))
  n2(59) = 16
  call heltable([4,4,16], n3(:,102), t3x16(:,:,48))
  n2(60) = 16
  call heltable([2,4,8], n3(:,103), t3x8(:,:,19))
  call heltable([4,2,8], n3(:,104), t3x8(:,:,20))
  n2(61) = 8
  call heltable([4,8,32], n3(:,105), t3x32(:,:,25))
  call heltable([2,4,8], n3(:,106), t3x8(:,:,21))
  n2(62) = 8
  call heltable([4,8,32], n3(:,107), t3x32(:,:,26))
  call heltable([8,4,32], n3(:,108), t3x32(:,:,27))
  call heltable([8,4,32], n3(:,109), t3x32(:,:,28))
  n2(63) = 16
  call heltable([8,2,16], n3(:,110), t3x16(:,:,49))
  call heltable([8,2,16], n3(:,111), t3x16(:,:,50))
  call heltable([2,4,8], n3(:,112), t3x8(:,:,22))
  n2(64) = 8
  call heltable([8,4,32], n3(:,113), t3x32(:,:,29))
  call heltable([8,4,32], n3(:,114), t3x32(:,:,30))
  call heltable([8,2,16], n3(:,115), t3x16(:,:,51))
  call heltable([2,8,16], n3(:,116), t3x16(:,:,52))
  call heltable([2,2,4], n3(:,117), t3x4(:,:,13))
  n2(65) = 4
  call heltable([4,4,16], n3(:,118), t3x16(:,:,53))
  call heltable([4,4,16], n3(:,119), t3x16(:,:,54))
  call heltable([4,4,16], n3(:,120), t3x16(:,:,55))
  n2(66) = 16
  call heltable([4,4,16], n3(:,121), t3x16(:,:,56))
  n2(67) = 16
  call heltable([8,4,32], n3(:,122), t3x32(:,:,31))
  call heltable([8,4,32], n3(:,123), t3x32(:,:,32))
  n2(68) = 16
  call heltable([4,2,8], n3(:,124), t3x8(:,:,23))
  n2(69) = 8
  call heltable([4,8,32], n3(:,125), t3x32(:,:,33))
  call heltable([4,8,32], n3(:,126), t3x32(:,:,34))
  call heltable([4,2,8], n3(:,127), t3x8(:,:,24))
  n2(70) = 8
  call heltable([2,8,16], n3(:,128), t3x16(:,:,57))
  call heltable([4,8,32], n3(:,129), t3x32(:,:,35))
  call heltable([4,8,32], n3(:,130), t3x32(:,:,36))
  call heltable([8,2,16], n3(:,131), t3x16(:,:,58))
  n2(71) = 16
  call heltable([8,2,16], n3(:,132), t3x16(:,:,59))
  call heltable([8,2,16], n3(:,133), t3x16(:,:,60))
  call heltable([8,2,16], n3(:,134), t3x16(:,:,61))
  n2(72) = 16
  call heltable([4,2,8], n3(:,135), t3x8(:,:,25))
  n2(73) = 8
  call heltable([2,8,16], n3(:,136), t3x16(:,:,62))
  n2(74) = 16
  n2(75) = 16
  n2(76) = 16
  n2(77) = 16
  n2(78) = 16
  n2(79) = 16
  call heltable([2,2,4], n3(:,137), t3x4(:,:,14))
  call heltable([4,4,16], n3(:,138), t3x16(:,:,63))
  call heltable([4,4,16], n3(:,139), t3x16(:,:,64))
  call heltable([8,4,32], n3(:,140), t3x32(:,:,37))
  call heltable([8,4,32], n3(:,141), t3x32(:,:,38))
  call heltable([4,2,8], n3(:,142), t3x8(:,:,26))
  call heltable([4,8,32], n3(:,143), t3x32(:,:,39))
  call heltable([4,8,32], n3(:,144), t3x32(:,:,40))
  n2(80) = 16
  call heltable([2,4,8], n3(:,145), t3x8(:,:,27))
  n2(81) = 8
  call heltable([8,4,32], n3(:,146), t3x32(:,:,41))
  call heltable([8,4,32], n3(:,147), t3x32(:,:,42))
  call heltable([8,2,16], n3(:,148), t3x16(:,:,65))
  call heltable([2,8,16], n3(:,149), t3x16(:,:,66))
  call heltable([4,4,16], n3(:,150), t3x16(:,:,67))
  call heltable([4,4,16], n3(:,151), t3x16(:,:,68))
  n2(82) = 16
  call heltable([8,4,32], n3(:,152), t3x32(:,:,43))
  call heltable([8,4,32], n3(:,153), t3x32(:,:,44))
  call heltable([4,8,32], n3(:,154), t3x32(:,:,45))
  call heltable([4,8,32], n3(:,155), t3x32(:,:,46))
  call heltable([4,2,8], n3(:,156), t3x8(:,:,28))
  n2(83) = 8
  call heltable([2,8,16], n3(:,157), t3x16(:,:,69))
  call heltable([4,8,32], n3(:,158), t3x32(:,:,47))
  call heltable([4,8,32], n3(:,159), t3x32(:,:,48))
  call heltable([8,2,16], n3(:,160), t3x16(:,:,70))
  n2(84) = 16
  call heltable([8,2,16], n3(:,161), t3x16(:,:,71))
  n2(85) = 16
  call heltable([2,8,16], n3(:,162), t3x16(:,:,72))
  n2(86) = 16
  n2(87) = 16
  n2(88) = 16
  n2(89) = 16
  call heltable([4,2,8], n3(:,163), t3x8(:,:,29))
  call heltable([4,8,32], n3(:,164), t3x32(:,:,49))
  call heltable([4,8,32], n3(:,165), t3x32(:,:,50))
  call heltable([4,2,8], n3(:,166), t3x8(:,:,30))
  call heltable([4,2,8], n3(:,167), t3x8(:,:,31))
  call heltable([8,4,32], n3(:,168), t3x32(:,:,51))
  call heltable([4,2,8], n3(:,169), t3x8(:,:,32))
  call heltable([8,4,32], n3(:,170), t3x32(:,:,52))
  call heltable([2,8,16], n3(:,171), t3x16(:,:,73))
  call heltable([2,8,16], n3(:,172), t3x16(:,:,74))
  call heltable([2,8,16], n3(:,173), t3x16(:,:,75))
  call heltable([2,8,16], n3(:,174), t3x16(:,:,76))
  call heltable([8,4,32], n3(:,175), t3x32(:,:,53))
  call heltable([8,4,32], n3(:,176), t3x32(:,:,54))
  call heltable([4,4,16], n3(:,177), t3x16(:,:,77))
  call heltable([8,2,16], n3(:,178), t3x16(:,:,78))
  call heltable([8,2,16], n3(:,179), t3x16(:,:,79))
  call heltable([2,4,8], n3(:,180), t3x8(:,:,33))
  call heltable([8,4,32], n3(:,181), t3x32(:,:,55))
  call heltable([2,8,16], n3(:,182), t3x16(:,:,80))
  call heltable([2,8,16], n3(:,183), t3x16(:,:,81))
  call heltable([8,2,16], n3(:,184), t3x16(:,:,82))
  call heltable([2,8,16], n3(:,185), t3x16(:,:,83))
  call heltable([8,8,64], n3(:,186), t3x64(:,:,1))
  call heltable([8,8,64], n3(:,187), t3x64(:,:,2))
  call heltable([4,4,16], n3(:,188), t3x16(:,:,84))
  call heltable([8,2,16], n3(:,189), t3x16(:,:,85))
  call heltable([8,2,16], n3(:,190), t3x16(:,:,86))
  call heltable([2,8,16], n3(:,191), t3x16(:,:,87))
  call heltable([2,8,16], n3(:,192), t3x16(:,:,88))
  call heltable([2,4,8], n3(:,193), t3x8(:,:,34))
  call heltable([8,4,32], n3(:,194), t3x32(:,:,56))
  call heltable([8,2,16], n3(:,195), t3x16(:,:,89))
  call heltable([2,8,16], n3(:,196), t3x16(:,:,90))
  call heltable([8,2,16], n3(:,197), t3x16(:,:,91))
  n2(90) = 16
  call heltable([8,2,16], n3(:,198), t3x16(:,:,92))
  n2(91) = 16
  call heltable([8,2,16], n3(:,199), t3x16(:,:,93))
  call heltable([8,2,16], n3(:,200), t3x16(:,:,94))
  n2(92) = 16
  n2(93) = 16
  call heltable([4,2,8], n3(:,201), t3x8(:,:,35))
  call heltable([4,2,8], n3(:,202), t3x8(:,:,36))
  call heltable([4,8,32], n3(:,203), t3x32(:,:,57))
  call heltable([4,8,32], n3(:,204), t3x32(:,:,58))
  call heltable([4,2,8], n3(:,205), t3x8(:,:,37))
  call heltable([8,4,32], n3(:,206), t3x32(:,:,59))
  call heltable([4,2,8], n3(:,207), t3x8(:,:,38))
  call heltable([8,4,32], n3(:,208), t3x32(:,:,60))
  call heltable([2,8,16], n3(:,209), t3x16(:,:,95))
  call heltable([2,8,16], n3(:,210), t3x16(:,:,96))
  call heltable([2,8,16], n3(:,211), t3x16(:,:,97))
  call heltable([2,8,16], n3(:,212), t3x16(:,:,98))
  call heltable([4,4,16], n3(:,213), t3x16(:,:,99))
  call heltable([8,2,16], n3(:,214), t3x16(:,:,100))
  call heltable([8,2,16], n3(:,215), t3x16(:,:,101))
  call heltable([8,4,32], n3(:,216), t3x32(:,:,61))
  call heltable([8,4,32], n3(:,217), t3x32(:,:,62))
  call heltable([2,4,8], n3(:,218), t3x8(:,:,39))
  call heltable([8,4,32], n3(:,219), t3x32(:,:,63))
  call heltable([2,8,16], n3(:,220), t3x16(:,:,102))
  call heltable([2,8,16], n3(:,221), t3x16(:,:,103))
  call heltable([2,8,16], n3(:,222), t3x16(:,:,104))
  call heltable([8,2,16], n3(:,223), t3x16(:,:,105))
  call heltable([4,4,16], n3(:,224), t3x16(:,:,106))
  call heltable([8,2,16], n3(:,225), t3x16(:,:,107))
  call heltable([8,2,16], n3(:,226), t3x16(:,:,108))
  call heltable([8,8,64], n3(:,227), t3x64(:,:,3))
  call heltable([8,8,64], n3(:,228), t3x64(:,:,4))
  call heltable([2,8,16], n3(:,229), t3x16(:,:,109))
  call heltable([2,8,16], n3(:,230), t3x16(:,:,110))
  call heltable([2,4,8], n3(:,231), t3x8(:,:,40))
  call heltable([8,4,32], n3(:,232), t3x32(:,:,64))
  call heltable([2,8,16], n3(:,233), t3x16(:,:,111))
  call heltable([8,2,16], n3(:,234), t3x16(:,:,112))
  call heltable([8,2,16], n3(:,235), t3x16(:,:,113))
  call heltable([8,2,16], n3(:,236), t3x16(:,:,114))
  call heltable([8,2,16], n3(:,237), t3x16(:,:,115))
  call heltable([8,2,16], n3(:,238), t3x16(:,:,116))
  n2(94) = 16
  n2(95) = 16
  n2(96) = 16
  n2(97) = 16
  call heltable([4,4,16], n3(:,239), t3x16(:,:,117))
  call heltable([2,4,8], n3(:,240), t3x8(:,:,41))
  call heltable([8,4,32], n3(:,241), t3x32(:,:,65))
  call heltable([8,2,16], n3(:,242), t3x16(:,:,118))
  call heltable([2,8,16], n3(:,243), t3x16(:,:,119))
  call heltable([4,4,16], n3(:,244), t3x16(:,:,120))
  call heltable([2,4,8], n3(:,245), t3x8(:,:,42))
  call heltable([8,4,32], n3(:,246), t3x32(:,:,66))
  call heltable([2,8,16], n3(:,247), t3x16(:,:,121))
  call heltable([8,2,16], n3(:,248), t3x16(:,:,122))
  call heltable([8,8,64], n3(:,249), t3x64(:,:,5))
  call heltable([8,8,64], n3(:,250), t3x64(:,:,6))
  call heltable([8,8,64], n3(:,251), t3x64(:,:,7))
  call heltable([8,8,64], n3(:,252), t3x64(:,:,8))
  call heltable([8,8,64], n3(:,253), t3x64(:,:,9))
  call heltable([8,8,64], n3(:,254), t3x64(:,:,10))
  call heltable([8,8,64], n3(:,255), t3x64(:,:,11))
  call heltable([8,8,64], n3(:,256), t3x64(:,:,12))
  call heltable([8,2,16], n3(:,257), t3x16(:,:,123))
  call heltable([8,2,16], n3(:,258), t3x16(:,:,124))
  call heltable([8,2,16], n3(:,259), t3x16(:,:,125))
  call heltable([8,2,16], n3(:,260), t3x16(:,:,126))
  call heltable([8,2,16], n3(:,261), t3x16(:,:,127))
  call heltable([2,8,16], n3(:,262), t3x16(:,:,128))
  n2(98) = 16
  n2(99) = 16
  call heltable([8,2,16], n3(:,263), t3x16(:,:,129))
  call heltable([2,8,16], n3(:,264), t3x16(:,:,130))
  n2(100) = 16
  n2(101) = 16
  call heltable([8,8,64], n3(:,265), t3x64(:,:,13))
  call heltable([8,8,64], n3(:,266), t3x64(:,:,14))
  call heltable([4,4,16], n3(:,267), t3x16(:,:,131))
  call heltable([2,4,8], n3(:,268), t3x8(:,:,43))
  call heltable([8,4,32], n3(:,269), t3x32(:,:,67))
  call heltable([8,2,16], n3(:,270), t3x16(:,:,132))
  call heltable([2,8,16], n3(:,271), t3x16(:,:,133))
  call heltable([4,4,16], n3(:,272), t3x16(:,:,134))
  call heltable([8,8,64], n3(:,273), t3x64(:,:,15))
  call heltable([8,8,64], n3(:,274), t3x64(:,:,16))
  call heltable([2,4,8], n3(:,275), t3x8(:,:,44))
  call heltable([8,4,32], n3(:,276), t3x32(:,:,68))
  call heltable([2,8,16], n3(:,277), t3x16(:,:,135))
  call heltable([8,2,16], n3(:,278), t3x16(:,:,136))
  call heltable([8,8,64], n3(:,279), t3x64(:,:,17))
  call heltable([8,8,64], n3(:,280), t3x64(:,:,18))
  call heltable([8,8,64], n3(:,281), t3x64(:,:,19))
  call heltable([8,8,64], n3(:,282), t3x64(:,:,20))
  call heltable([8,8,64], n3(:,283), t3x64(:,:,21))
  call heltable([8,8,64], n3(:,284), t3x64(:,:,22))
  call heltable([8,8,64], n3(:,285), t3x64(:,:,23))
  call heltable([8,8,64], n3(:,286), t3x64(:,:,24))
  call heltable([8,2,16], n3(:,287), t3x16(:,:,137))
  call heltable([8,2,16], n3(:,288), t3x16(:,:,138))
  call heltable([8,2,16], n3(:,289), t3x16(:,:,139))
  call heltable([8,2,16], n3(:,290), t3x16(:,:,140))
  call heltable([2,8,16], n3(:,291), t3x16(:,:,141))
  call heltable([8,2,16], n3(:,292), t3x16(:,:,142))
  n2(102) = 16
  n2(103) = 16
  call heltable([2,8,16], n3(:,293), t3x16(:,:,143))
  call heltable([8,2,16], n3(:,294), t3x16(:,:,144))
  n2(104) = 16
  n2(105) = 16
  n2(106) = 16
  n2(107) = 16
  call heltable([8,2,16], n3(:,295), t3x16(:,:,145))
  call heltable([8,2,16], n3(:,296), t3x16(:,:,146))
  n2(108) = 16
  n2(109) = 16
  call heltable([8,2,16], n3(:,297), t3x16(:,:,147))
  call heltable([8,2,16], n3(:,298), t3x16(:,:,148))
  n2(110) = 16
  n2(111) = 16
  n2(112) = 16
  n2(113) = 16
  call heltable([2,8,16], n3(:,299), t3x16(:,:,149))
  n2(114) = 16
  n2(115) = 16
  call heltable([2,8,16], n3(:,300), t3x16(:,:,150))
  n2(116) = 16
  n2(117) = 16
  call heltable([2,8,16], n3(:,301), t3x16(:,:,151))
  n2(118) = 16
  n2(119) = 16
  call heltable([2,8,16], n3(:,302), t3x16(:,:,152))
  n2(120) = 16
  n2(121) = 16
  call heltable([2,2,4], n3(:,303), t3x4(:,:,15))
  n2(122) = 4
  call heltable([4,2,8], n3(:,304), t3x8(:,:,45))
  call heltable([2,8,16], n3(:,305), t3x16(:,:,153))
  call heltable([4,2,8], n3(:,306), t3x8(:,:,46))
  n2(123) = 8
  call heltable([2,8,16], n3(:,307), t3x16(:,:,154))
  call heltable([8,4,32], n3(:,308), t3x32(:,:,69))
  call heltable([8,4,32], n3(:,309), t3x32(:,:,70))
  call heltable([8,2,16], n3(:,310), t3x16(:,:,155))
  call heltable([8,2,16], n3(:,311), t3x16(:,:,156))
  call heltable([8,8,64], n3(:,312), t3x64(:,:,25))
  call heltable([8,8,64], n3(:,313), t3x64(:,:,26))
  n2(124) = 16
  n2(125) = 16
  n2(126) = 16
  n2(127) = 16
  call heltable([2,2,4], n3(:,314), t3x4(:,:,16))
  n2(128) = 4
  call heltable([2,4,8], n3(:,315), t3x8(:,:,47))
  call heltable([2,8,16], n3(:,316), t3x16(:,:,157))
  call heltable([2,4,8], n3(:,317), t3x8(:,:,48))
  n2(129) = 8
  call heltable([2,8,16], n3(:,318), t3x16(:,:,158))
  call heltable([8,4,32], n3(:,319), t3x32(:,:,71))
  call heltable([8,4,32], n3(:,320), t3x32(:,:,72))
  call heltable([8,2,16], n3(:,321), t3x16(:,:,159))
  call heltable([8,2,16], n3(:,322), t3x16(:,:,160))
  call heltable([8,8,64], n3(:,323), t3x64(:,:,27))
  call heltable([8,8,64], n3(:,324), t3x64(:,:,28))
  n2(130) = 16
  n2(131) = 16
  n2(132) = 16
  n2(133) = 16
  call heltable([8,4,32], n3(:,325), t3x32(:,:,73))
  call heltable([8,4,32], n3(:,326), t3x32(:,:,74))
  call heltable([8,2,16], n3(:,327), t3x16(:,:,161))
  call heltable([8,2,16], n3(:,328), t3x16(:,:,162))
  call heltable([8,8,64], n3(:,329), t3x64(:,:,29))
  call heltable([8,8,64], n3(:,330), t3x64(:,:,30))
  n2(134) = 16
  n2(135) = 16
  call heltable([8,4,32], n3(:,331), t3x32(:,:,75))
  call heltable([8,4,32], n3(:,332), t3x32(:,:,76))
  call heltable([8,2,16], n3(:,333), t3x16(:,:,163))
  call heltable([8,2,16], n3(:,334), t3x16(:,:,164))
  call heltable([8,8,64], n3(:,335), t3x64(:,:,31))
  call heltable([8,8,64], n3(:,336), t3x64(:,:,32))
  n2(136) = 16
  n2(137) = 16
  call heltable([2,8,16], n3(:,337), t3x16(:,:,165))
  call heltable([2,8,16], n3(:,338), t3x16(:,:,166))
  call heltable([8,8,64], n3(:,339), t3x64(:,:,33))
  call heltable([8,8,64], n3(:,340), t3x64(:,:,34))
  n2(138) = 16
  n2(139) = 16
  call heltable([2,8,16], n3(:,341), t3x16(:,:,167))
  call heltable([2,8,16], n3(:,342), t3x16(:,:,168))
  call heltable([8,8,64], n3(:,343), t3x64(:,:,35))
  call heltable([8,8,64], n3(:,344), t3x64(:,:,36))
  n2(140) = 16
  n2(141) = 16
  call heltable([8,8,64], n3(:,345), t3x64(:,:,37))
  call heltable([8,8,64], n3(:,346), t3x64(:,:,38))
  call heltable([8,8,64], n3(:,347), t3x64(:,:,39))
  call heltable([8,8,64], n3(:,348), t3x64(:,:,40))
  call heltable([16,16,256], n3(:,349), t3x256(:,:,1))
  call heltable([16,16,256], n3(:,350), t3x256(:,:,2))
  call heltable([16,16,256], n3(:,351), t3x256(:,:,3))
  call heltable([16,16,256], n3(:,352), t3x256(:,:,4))
  call heltable([8,32,256], n3(:,353), t3x256(:,:,5))
  call heltable([8,32,256], n3(:,354), t3x256(:,:,6))
  call heltable([8,32,256], n3(:,355), t3x256(:,:,7))
  call heltable([8,32,256], n3(:,356), t3x256(:,:,8))
  call heltable([16,16,256], n3(:,357), t3x256(:,:,9))
  call heltable([16,16,256], n3(:,358), t3x256(:,:,10))
  call heltable([8,32,256], n3(:,359), t3x256(:,:,11))
  call heltable([8,32,256], n3(:,360), t3x256(:,:,12))
  call heltable([16,16,256], n3(:,361), t3x256(:,:,13))
  call heltable([16,16,256], n3(:,362), t3x256(:,:,14))
  call heltable([16,16,256], n3(:,363), t3x256(:,:,15))
  call heltable([16,16,256], n3(:,364), t3x256(:,:,16))
  call heltable([16,16,256], n3(:,365), t3x256(:,:,17))
  call heltable([16,16,256], n3(:,366), t3x256(:,:,18))
  call heltable([16,16,256], n3(:,367), t3x256(:,:,19))
  call heltable([16,16,256], n3(:,368), t3x256(:,:,20))
  call heltable([8,32,256], n3(:,369), t3x256(:,:,21))
  call heltable([8,32,256], n3(:,370), t3x256(:,:,22))
  call heltable([16,16,256], n3(:,371), t3x256(:,:,23))
  call heltable([16,16,256], n3(:,372), t3x256(:,:,24))
  call heltable([8,32,256], n3(:,373), t3x256(:,:,25))
  call heltable([8,32,256], n3(:,374), t3x256(:,:,26))
  call heltable([16,16,256], n3(:,375), t3x256(:,:,27))
  call heltable([16,16,256], n3(:,376), t3x256(:,:,28))
  call heltable([8,32,256], n3(:,377), t3x256(:,:,29))
  call heltable([8,32,256], n3(:,378), t3x256(:,:,30))
  call heltable([16,16,256], n3(:,379), t3x256(:,:,31))
  call heltable([16,16,256], n3(:,380), t3x256(:,:,32))
  call heltable([32,8,256], n3(:,381), t3x256(:,:,33))
  call heltable([32,8,256], n3(:,382), t3x256(:,:,34))
  call heltable([16,16,256], n3(:,383), t3x256(:,:,35))
  call heltable([16,16,256], n3(:,384), t3x256(:,:,36))
  call heltable([32,8,256], n3(:,385), t3x256(:,:,37))
  call heltable([32,8,256], n3(:,386), t3x256(:,:,38))
  call heltable([16,16,256], n3(:,387), t3x256(:,:,39))
  call heltable([16,16,256], n3(:,388), t3x256(:,:,40))
  call heltable([16,16,256], n3(:,389), t3x256(:,:,41))
  call heltable([16,16,256], n3(:,390), t3x256(:,:,42))
  call heltable([32,8,256], n3(:,391), t3x256(:,:,43))
  call heltable([32,8,256], n3(:,392), t3x256(:,:,44))
  call heltable([16,16,256], n3(:,393), t3x256(:,:,45))
  call heltable([16,16,256], n3(:,394), t3x256(:,:,46))
  call heltable([32,8,256], n3(:,395), t3x256(:,:,47))
  call heltable([32,8,256], n3(:,396), t3x256(:,:,48))
  call heltable([16,16,256], n3(:,397), t3x256(:,:,49))
  call heltable([16,16,256], n3(:,398), t3x256(:,:,50))
  call heltable([16,16,256], n3(:,399), t3x256(:,:,51))
  call heltable([16,16,256], n3(:,400), t3x256(:,:,52))
  call heltable([16,16,256], n3(:,401), t3x256(:,:,53))
  call heltable([16,16,256], n3(:,402), t3x256(:,:,54))
  call heltable([16,16,256], n3(:,403), t3x256(:,:,55))
  call heltable([16,16,256], n3(:,404), t3x256(:,:,56))
  call heltable([16,16,256], n3(:,405), t3x256(:,:,57))
  call heltable([16,16,256], n3(:,406), t3x256(:,:,58))
  call heltable([16,16,256], n3(:,407), t3x256(:,:,59))
  call heltable([16,16,256], n3(:,408), t3x256(:,:,60))
  call heltable([16,16,256], n3(:,409), t3x256(:,:,61))
  call heltable([16,16,256], n3(:,410), t3x256(:,:,62))
  call heltable([16,16,256], n3(:,411), t3x256(:,:,63))
  call heltable([16,16,256], n3(:,412), t3x256(:,:,64))
  call heltable([8,32,256], n3(:,413), t3x256(:,:,65))
  call heltable([8,32,256], n3(:,414), t3x256(:,:,66))
  call heltable([8,32,256], n3(:,415), t3x256(:,:,67))
  call heltable([8,32,256], n3(:,416), t3x256(:,:,68))
  call heltable([16,16,256], n3(:,417), t3x256(:,:,69))
  call heltable([16,16,256], n3(:,418), t3x256(:,:,70))
  call heltable([8,32,256], n3(:,419), t3x256(:,:,71))
  call heltable([8,32,256], n3(:,420), t3x256(:,:,72))
  call heltable([16,16,256], n3(:,421), t3x256(:,:,73))
  call heltable([16,16,256], n3(:,422), t3x256(:,:,74))
  call heltable([16,16,256], n3(:,423), t3x256(:,:,75))
  call heltable([16,16,256], n3(:,424), t3x256(:,:,76))
  call heltable([16,16,256], n3(:,425), t3x256(:,:,77))
  call heltable([16,16,256], n3(:,426), t3x256(:,:,78))
  call heltable([16,16,256], n3(:,427), t3x256(:,:,79))
  call heltable([16,16,256], n3(:,428), t3x256(:,:,80))
  call heltable([16,16,256], n3(:,429), t3x256(:,:,81))
  call heltable([16,16,256], n3(:,430), t3x256(:,:,82))
  call heltable([8,32,256], n3(:,431), t3x256(:,:,83))
  call heltable([8,32,256], n3(:,432), t3x256(:,:,84))
  call heltable([8,32,256], n3(:,433), t3x256(:,:,85))
  call heltable([8,32,256], n3(:,434), t3x256(:,:,86))
  call heltable([16,16,256], n3(:,435), t3x256(:,:,87))
  call heltable([16,16,256], n3(:,436), t3x256(:,:,88))
  call heltable([8,32,256], n3(:,437), t3x256(:,:,89))
  call heltable([8,32,256], n3(:,438), t3x256(:,:,90))
  call heltable([16,16,256], n3(:,439), t3x256(:,:,91))
  call heltable([16,16,256], n3(:,440), t3x256(:,:,92))
  call heltable([32,8,256], n3(:,441), t3x256(:,:,93))
  call heltable([32,8,256], n3(:,442), t3x256(:,:,94))
  call heltable([16,16,256], n3(:,443), t3x256(:,:,95))
  call heltable([16,16,256], n3(:,444), t3x256(:,:,96))
  call heltable([32,8,256], n3(:,445), t3x256(:,:,97))
  call heltable([32,8,256], n3(:,446), t3x256(:,:,98))
  call heltable([16,16,256], n3(:,447), t3x256(:,:,99))
  call heltable([16,16,256], n3(:,448), t3x256(:,:,100))
  call heltable([16,16,256], n3(:,449), t3x256(:,:,101))
  call heltable([16,16,256], n3(:,450), t3x256(:,:,102))
  call heltable([8,32,256], n3(:,451), t3x256(:,:,103))
  call heltable([8,32,256], n3(:,452), t3x256(:,:,104))
  call heltable([16,16,256], n3(:,453), t3x256(:,:,105))
  call heltable([16,16,256], n3(:,454), t3x256(:,:,106))
  call heltable([8,32,256], n3(:,455), t3x256(:,:,107))
  call heltable([8,32,256], n3(:,456), t3x256(:,:,108))
  call heltable([16,16,256], n3(:,457), t3x256(:,:,109))
  call heltable([16,16,256], n3(:,458), t3x256(:,:,110))
  call heltable([16,16,256], n3(:,459), t3x256(:,:,111))
  call heltable([16,16,256], n3(:,460), t3x256(:,:,112))
  call heltable([16,16,256], n3(:,461), t3x256(:,:,113))
  call heltable([16,16,256], n3(:,462), t3x256(:,:,114))
  call heltable([16,16,256], n3(:,463), t3x256(:,:,115))
  call heltable([16,16,256], n3(:,464), t3x256(:,:,116))
  call heltable([16,16,256], n3(:,465), t3x256(:,:,117))
  call heltable([16,16,256], n3(:,466), t3x256(:,:,118))
  call heltable([16,16,256], n3(:,467), t3x256(:,:,119))
  call heltable([16,16,256], n3(:,468), t3x256(:,:,120))
  call heltable([16,16,256], n3(:,469), t3x256(:,:,121))
  call heltable([16,16,256], n3(:,470), t3x256(:,:,122))
  call heltable([16,16,256], n3(:,471), t3x256(:,:,123))
  call heltable([16,16,256], n3(:,472), t3x256(:,:,124))
  call heltable([8,32,256], n3(:,473), t3x256(:,:,125))
  call heltable([8,32,256], n3(:,474), t3x256(:,:,126))
  call heltable([8,32,256], n3(:,475), t3x256(:,:,127))
  call heltable([8,32,256], n3(:,476), t3x256(:,:,128))
  call heltable([16,16,256], n3(:,477), t3x256(:,:,129))
  call heltable([16,16,256], n3(:,478), t3x256(:,:,130))
  call heltable([8,32,256], n3(:,479), t3x256(:,:,131))
  call heltable([8,32,256], n3(:,480), t3x256(:,:,132))
  call heltable([16,16,256], n3(:,481), t3x256(:,:,133))
  call heltable([16,16,256], n3(:,482), t3x256(:,:,134))
  call heltable([16,16,256], n3(:,483), t3x256(:,:,135))
  call heltable([16,16,256], n3(:,484), t3x256(:,:,136))
  call heltable([16,16,256], n3(:,485), t3x256(:,:,137))
  call heltable([16,16,256], n3(:,486), t3x256(:,:,138))
  call heltable([16,16,256], n3(:,487), t3x256(:,:,139))
  call heltable([16,16,256], n3(:,488), t3x256(:,:,140))
  call heltable([8,32,256], n3(:,489), t3x256(:,:,141))
  call heltable([8,32,256], n3(:,490), t3x256(:,:,142))
  call heltable([16,16,256], n3(:,491), t3x256(:,:,143))
  call heltable([16,16,256], n3(:,492), t3x256(:,:,144))
  call heltable([8,32,256], n3(:,493), t3x256(:,:,145))
  call heltable([8,32,256], n3(:,494), t3x256(:,:,146))
  call heltable([16,16,256], n3(:,495), t3x256(:,:,147))
  call heltable([16,16,256], n3(:,496), t3x256(:,:,148))
  call heltable([8,32,256], n3(:,497), t3x256(:,:,149))
  call heltable([8,32,256], n3(:,498), t3x256(:,:,150))
  call heltable([16,16,256], n3(:,499), t3x256(:,:,151))
  call heltable([16,16,256], n3(:,500), t3x256(:,:,152))
  call heltable([8,32,256], n3(:,501), t3x256(:,:,153))
  call heltable([8,32,256], n3(:,502), t3x256(:,:,154))
  call heltable([16,16,256], n3(:,503), t3x256(:,:,155))
  call heltable([16,16,256], n3(:,504), t3x256(:,:,156))
  call heltable([8,32,256], n3(:,505), t3x256(:,:,157))
  call heltable([8,32,256], n3(:,506), t3x256(:,:,158))
  call heltable([16,16,256], n3(:,507), t3x256(:,:,159))
  call heltable([16,16,256], n3(:,508), t3x256(:,:,160))
  call heltable([16,16,256], n3(:,509), t3x256(:,:,161))
  call heltable([16,16,256], n3(:,510), t3x256(:,:,162))
  call heltable([32,8,256], n3(:,511), t3x256(:,:,163))
  call heltable([32,8,256], n3(:,512), t3x256(:,:,164))
  call heltable([16,16,256], n3(:,513), t3x256(:,:,165))
  call heltable([16,16,256], n3(:,514), t3x256(:,:,166))
  call heltable([32,8,256], n3(:,515), t3x256(:,:,167))
  call heltable([32,8,256], n3(:,516), t3x256(:,:,168))
  call heltable([16,16,256], n3(:,517), t3x256(:,:,169))
  call heltable([16,16,256], n3(:,518), t3x256(:,:,170))
  call heltable([16,16,256], n3(:,519), t3x256(:,:,171))
  call heltable([16,16,256], n3(:,520), t3x256(:,:,172))
  call heltable([16,16,256], n3(:,521), t3x256(:,:,173))
  call heltable([16,16,256], n3(:,522), t3x256(:,:,174))
  call heltable([16,16,256], n3(:,523), t3x256(:,:,175))
  call heltable([16,16,256], n3(:,524), t3x256(:,:,176))
  call heltable([16,16,256], n3(:,525), t3x256(:,:,177))
  call heltable([16,16,256], n3(:,526), t3x256(:,:,178))
  call heltable([16,16,256], n3(:,527), t3x256(:,:,179))
  call heltable([16,16,256], n3(:,528), t3x256(:,:,180))
  call heltable([16,16,256], n3(:,529), t3x256(:,:,181))
  call heltable([16,16,256], n3(:,530), t3x256(:,:,182))
  call heltable([16,16,256], n3(:,531), t3x256(:,:,183))
  call heltable([16,16,256], n3(:,532), t3x256(:,:,184))
  call heltable([8,32,256], n3(:,533), t3x256(:,:,185))
  call heltable([8,32,256], n3(:,534), t3x256(:,:,186))
  call heltable([8,32,256], n3(:,535), t3x256(:,:,187))
  call heltable([8,32,256], n3(:,536), t3x256(:,:,188))
  call heltable([16,16,256], n3(:,537), t3x256(:,:,189))
  call heltable([16,16,256], n3(:,538), t3x256(:,:,190))
  call heltable([8,32,256], n3(:,539), t3x256(:,:,191))
  call heltable([8,32,256], n3(:,540), t3x256(:,:,192))
  call heltable([16,16,256], n3(:,541), t3x256(:,:,193))
  call heltable([16,16,256], n3(:,542), t3x256(:,:,194))
  call heltable([16,16,256], n3(:,543), t3x256(:,:,195))
  call heltable([16,16,256], n3(:,544), t3x256(:,:,196))
  call heltable([16,16,256], n3(:,545), t3x256(:,:,197))
  call heltable([16,16,256], n3(:,546), t3x256(:,:,198))
  call heltable([16,16,256], n3(:,547), t3x256(:,:,199))
  call heltable([16,16,256], n3(:,548), t3x256(:,:,200))
  call heltable([16,16,256], n3(:,549), t3x256(:,:,201))
  call heltable([16,16,256], n3(:,550), t3x256(:,:,202))
  call heltable([8,32,256], n3(:,551), t3x256(:,:,203))
  call heltable([8,32,256], n3(:,552), t3x256(:,:,204))
  call heltable([8,32,256], n3(:,553), t3x256(:,:,205))
  call heltable([8,32,256], n3(:,554), t3x256(:,:,206))
  call heltable([16,16,256], n3(:,555), t3x256(:,:,207))
  call heltable([16,16,256], n3(:,556), t3x256(:,:,208))
  call heltable([8,32,256], n3(:,557), t3x256(:,:,209))
  call heltable([8,32,256], n3(:,558), t3x256(:,:,210))
  call heltable([16,16,256], n3(:,559), t3x256(:,:,211))
  call heltable([16,16,256], n3(:,560), t3x256(:,:,212))
  call heltable([8,32,256], n3(:,561), t3x256(:,:,213))
  call heltable([8,32,256], n3(:,562), t3x256(:,:,214))
  call heltable([16,16,256], n3(:,563), t3x256(:,:,215))
  call heltable([16,16,256], n3(:,564), t3x256(:,:,216))
  call heltable([8,32,256], n3(:,565), t3x256(:,:,217))
  call heltable([8,32,256], n3(:,566), t3x256(:,:,218))
  call heltable([16,16,256], n3(:,567), t3x256(:,:,219))
  call heltable([16,16,256], n3(:,568), t3x256(:,:,220))
  call heltable([16,16,256], n3(:,569), t3x256(:,:,221))
  call heltable([16,16,256], n3(:,570), t3x256(:,:,222))
  call heltable([8,32,256], n3(:,571), t3x256(:,:,223))
  call heltable([8,32,256], n3(:,572), t3x256(:,:,224))
  call heltable([16,16,256], n3(:,573), t3x256(:,:,225))
  call heltable([16,16,256], n3(:,574), t3x256(:,:,226))
  call heltable([8,32,256], n3(:,575), t3x256(:,:,227))
  call heltable([8,32,256], n3(:,576), t3x256(:,:,228))
  call heltable([16,16,256], n3(:,577), t3x256(:,:,229))
  call heltable([16,16,256], n3(:,578), t3x256(:,:,230))
  call heltable([16,16,256], n3(:,579), t3x256(:,:,231))
  call heltable([16,16,256], n3(:,580), t3x256(:,:,232))
  call heltable([16,16,256], n3(:,581), t3x256(:,:,233))
  call heltable([16,16,256], n3(:,582), t3x256(:,:,234))
  call heltable([16,16,256], n3(:,583), t3x256(:,:,235))
  call heltable([16,16,256], n3(:,584), t3x256(:,:,236))
  call heltable([16,16,256], n3(:,585), t3x256(:,:,237))
  call heltable([16,16,256], n3(:,586), t3x256(:,:,238))
  call heltable([16,16,256], n3(:,587), t3x256(:,:,239))
  call heltable([16,16,256], n3(:,588), t3x256(:,:,240))
  call heltable([8,32,256], n3(:,589), t3x256(:,:,241))
  call heltable([8,32,256], n3(:,590), t3x256(:,:,242))
  call heltable([32,8,256], n3(:,591), t3x256(:,:,243))
  call heltable([32,8,256], n3(:,592), t3x256(:,:,244))
  call heltable([8,32,256], n3(:,593), t3x256(:,:,245))
  call heltable([8,32,256], n3(:,594), t3x256(:,:,246))
  call heltable([8,32,256], n3(:,595), t3x256(:,:,247))
  call heltable([8,32,256], n3(:,596), t3x256(:,:,248))
  call heltable([16,16,256], n3(:,597), t3x256(:,:,249))
  call heltable([16,16,256], n3(:,598), t3x256(:,:,250))
  call heltable([16,16,256], n3(:,599), t3x256(:,:,251))
  call heltable([16,16,256], n3(:,600), t3x256(:,:,252))
  call heltable([16,16,256], n3(:,601), t3x256(:,:,253))
  call heltable([16,16,256], n3(:,602), t3x256(:,:,254))
  call heltable([16,16,256], n3(:,603), t3x256(:,:,255))
  call heltable([16,16,256], n3(:,604), t3x256(:,:,256))
  call heltable([8,32,256], n3(:,605), t3x256(:,:,257))
  call heltable([8,32,256], n3(:,606), t3x256(:,:,258))
  call heltable([16,16,256], n3(:,607), t3x256(:,:,259))
  call heltable([16,16,256], n3(:,608), t3x256(:,:,260))
  call heltable([8,32,256], n3(:,609), t3x256(:,:,261))
  call heltable([8,32,256], n3(:,610), t3x256(:,:,262))
  call heltable([16,16,256], n3(:,611), t3x256(:,:,263))
  call heltable([16,16,256], n3(:,612), t3x256(:,:,264))
  call heltable([16,16,256], n3(:,613), t3x256(:,:,265))
  call heltable([16,16,256], n3(:,614), t3x256(:,:,266))
  call heltable([16,16,256], n3(:,615), t3x256(:,:,267))
  call heltable([16,16,256], n3(:,616), t3x256(:,:,268))
  call heltable([4,64,256], n3(:,617), t3x256(:,:,269))
  call heltable([4,64,256], n3(:,618), t3x256(:,:,270))
  call heltable([16,16,256], n3(:,619), t3x256(:,:,271))
  call heltable([16,16,256], n3(:,620), t3x256(:,:,272))
  call heltable([16,16,256], n3(:,621), t3x256(:,:,273))
  call heltable([16,16,256], n3(:,622), t3x256(:,:,274))
  call heltable([8,32,256], n3(:,623), t3x256(:,:,275))
  call heltable([8,32,256], n3(:,624), t3x256(:,:,276))
  call heltable([16,16,256], n3(:,625), t3x256(:,:,277))
  call heltable([16,16,256], n3(:,626), t3x256(:,:,278))
  call heltable([16,16,256], n3(:,627), t3x256(:,:,279))
  call heltable([16,16,256], n3(:,628), t3x256(:,:,280))
  call heltable([16,16,256], n3(:,629), t3x256(:,:,281))
  call heltable([16,16,256], n3(:,630), t3x256(:,:,282))
  call heltable([16,16,256], n3(:,631), t3x256(:,:,283))
  call heltable([16,16,256], n3(:,632), t3x256(:,:,284))
  call heltable([16,16,256], n3(:,633), t3x256(:,:,285))
  call heltable([16,16,256], n3(:,634), t3x256(:,:,286))
  call heltable([16,16,256], n3(:,635), t3x256(:,:,287))
  call heltable([16,16,256], n3(:,636), t3x256(:,:,288))
  call heltable([16,16,256], n3(:,637), t3x256(:,:,289))
  call heltable([16,16,256], n3(:,638), t3x256(:,:,290))
  call heltable([16,16,256], n3(:,639), t3x256(:,:,291))
  call heltable([16,16,256], n3(:,640), t3x256(:,:,292))
  call heltable([16,16,256], n3(:,641), t3x256(:,:,293))
  call heltable([16,16,256], n3(:,642), t3x256(:,:,294))
  call heltable([16,16,256], n3(:,643), t3x256(:,:,295))
  call heltable([16,16,256], n3(:,644), t3x256(:,:,296))
  call heltable([16,16,256], n3(:,645), t3x256(:,:,297))
  call heltable([16,16,256], n3(:,646), t3x256(:,:,298))
  call heltable([16,16,256], n3(:,647), t3x256(:,:,299))
  call heltable([16,16,256], n3(:,648), t3x256(:,:,300))
  call heltable([16,16,256], n3(:,649), t3x256(:,:,301))
  call heltable([16,16,256], n3(:,650), t3x256(:,:,302))
  call heltable([16,16,256], n3(:,651), t3x256(:,:,303))
  call heltable([16,16,256], n3(:,652), t3x256(:,:,304))
  call heltable([32,8,256], n3(:,653), t3x256(:,:,305))
  call heltable([32,8,256], n3(:,654), t3x256(:,:,306))
  call heltable([8,32,256], n3(:,655), t3x256(:,:,307))
  call heltable([8,32,256], n3(:,656), t3x256(:,:,308))
  call heltable([8,32,256], n3(:,657), t3x256(:,:,309))
  call heltable([8,32,256], n3(:,658), t3x256(:,:,310))
  call heltable([8,32,256], n3(:,659), t3x256(:,:,311))
  call heltable([8,32,256], n3(:,660), t3x256(:,:,312))
  call heltable([16,16,256], n3(:,661), t3x256(:,:,313))
  call heltable([16,16,256], n3(:,662), t3x256(:,:,314))
  call heltable([16,16,256], n3(:,663), t3x256(:,:,315))
  call heltable([16,16,256], n3(:,664), t3x256(:,:,316))
  call heltable([16,16,256], n3(:,665), t3x256(:,:,317))
  call heltable([16,16,256], n3(:,666), t3x256(:,:,318))
  call heltable([16,16,256], n3(:,667), t3x256(:,:,319))
  call heltable([16,16,256], n3(:,668), t3x256(:,:,320))
  call heltable([16,16,256], n3(:,669), t3x256(:,:,321))
  call heltable([16,16,256], n3(:,670), t3x256(:,:,322))
  call heltable([8,32,256], n3(:,671), t3x256(:,:,323))
  call heltable([8,32,256], n3(:,672), t3x256(:,:,324))
  call heltable([8,32,256], n3(:,673), t3x256(:,:,325))
  call heltable([8,32,256], n3(:,674), t3x256(:,:,326))
  call heltable([16,16,256], n3(:,675), t3x256(:,:,327))
  call heltable([16,16,256], n3(:,676), t3x256(:,:,328))
  call heltable([16,16,256], n3(:,677), t3x256(:,:,329))
  call heltable([16,16,256], n3(:,678), t3x256(:,:,330))
  call heltable([16,16,256], n3(:,679), t3x256(:,:,331))
  call heltable([16,16,256], n3(:,680), t3x256(:,:,332))
  call heltable([16,16,256], n3(:,681), t3x256(:,:,333))
  call heltable([16,16,256], n3(:,682), t3x256(:,:,334))
  call heltable([4,64,256], n3(:,683), t3x256(:,:,335))
  call heltable([4,64,256], n3(:,684), t3x256(:,:,336))
  call heltable([16,16,256], n3(:,685), t3x256(:,:,337))
  call heltable([16,16,256], n3(:,686), t3x256(:,:,338))
  call heltable([8,32,256], n3(:,687), t3x256(:,:,339))
  call heltable([8,32,256], n3(:,688), t3x256(:,:,340))
  call heltable([16,16,256], n3(:,689), t3x256(:,:,341))
  call heltable([16,16,256], n3(:,690), t3x256(:,:,342))
  call heltable([16,16,256], n3(:,691), t3x256(:,:,343))
  call heltable([16,16,256], n3(:,692), t3x256(:,:,344))
  call heltable([16,16,256], n3(:,693), t3x256(:,:,345))
  call heltable([16,16,256], n3(:,694), t3x256(:,:,346))
  call heltable([16,16,256], n3(:,695), t3x256(:,:,347))
  call heltable([16,16,256], n3(:,696), t3x256(:,:,348))
  call heltable([16,16,256], n3(:,697), t3x256(:,:,349))
  call heltable([16,16,256], n3(:,698), t3x256(:,:,350))
  call heltable([16,16,256], n3(:,699), t3x256(:,:,351))
  call heltable([16,16,256], n3(:,700), t3x256(:,:,352))
  call heltable([16,16,256], n3(:,701), t3x256(:,:,353))
  call heltable([16,16,256], n3(:,702), t3x256(:,:,354))
  call heltable([16,16,256], n3(:,703), t3x256(:,:,355))
  call heltable([16,16,256], n3(:,704), t3x256(:,:,356))
  call heltable([16,16,256], n3(:,705), t3x256(:,:,357))
  call heltable([16,16,256], n3(:,706), t3x256(:,:,358))
  call heltable([16,16,256], n3(:,707), t3x256(:,:,359))
  call heltable([16,16,256], n3(:,708), t3x256(:,:,360))
  call heltable([16,16,256], n3(:,709), t3x256(:,:,361))
  call heltable([16,16,256], n3(:,710), t3x256(:,:,362))
  call heltable([16,16,256], n3(:,711), t3x256(:,:,363))
  call heltable([16,16,256], n3(:,712), t3x256(:,:,364))
  call heltable([16,16,256], n3(:,713), t3x256(:,:,365))
  call heltable([16,16,256], n3(:,714), t3x256(:,:,366))
  call heltable([16,16,256], n3(:,715), t3x256(:,:,367))
  call heltable([16,16,256], n3(:,716), t3x256(:,:,368))
  call heltable([32,8,256], n3(:,717), t3x256(:,:,369))
  call heltable([32,8,256], n3(:,718), t3x256(:,:,370))
  call heltable([16,16,256], n3(:,719), t3x256(:,:,371))
  call heltable([16,16,256], n3(:,720), t3x256(:,:,372))
  call heltable([8,32,256], n3(:,721), t3x256(:,:,373))
  call heltable([8,32,256], n3(:,722), t3x256(:,:,374))
  call heltable([16,16,256], n3(:,723), t3x256(:,:,375))
  call heltable([16,16,256], n3(:,724), t3x256(:,:,376))
  call heltable([16,16,256], n3(:,725), t3x256(:,:,377))
  call heltable([16,16,256], n3(:,726), t3x256(:,:,378))
  call heltable([16,16,256], n3(:,727), t3x256(:,:,379))
  call heltable([16,16,256], n3(:,728), t3x256(:,:,380))
  call heltable([16,16,256], n3(:,729), t3x256(:,:,381))
  call heltable([16,16,256], n3(:,730), t3x256(:,:,382))
  call heltable([32,8,256], n3(:,731), t3x256(:,:,383))
  call heltable([32,8,256], n3(:,732), t3x256(:,:,384))
  call heltable([8,32,256], n3(:,733), t3x256(:,:,385))
  call heltable([8,32,256], n3(:,734), t3x256(:,:,386))
  call heltable([16,16,256], n3(:,735), t3x256(:,:,387))
  call heltable([16,16,256], n3(:,736), t3x256(:,:,388))
  call heltable([16,16,256], n3(:,737), t3x256(:,:,389))
  call heltable([16,16,256], n3(:,738), t3x256(:,:,390))
  call heltable([16,16,256], n3(:,739), t3x256(:,:,391))
  call heltable([16,16,256], n3(:,740), t3x256(:,:,392))
  call heltable([4,64,256], n3(:,741), t3x256(:,:,393))
  call heltable([4,64,256], n3(:,742), t3x256(:,:,394))
  call heltable([4,64,256], n3(:,743), t3x256(:,:,395))
  call heltable([4,64,256], n3(:,744), t3x256(:,:,396))
  call heltable([4,64,256], n3(:,745), t3x256(:,:,397))
  call heltable([4,64,256], n3(:,746), t3x256(:,:,398))
  call heltable([4,64,256], n3(:,747), t3x256(:,:,399))
  call heltable([4,64,256], n3(:,748), t3x256(:,:,400))
  call heltable([16,16,256], n3(:,749), t3x256(:,:,401))
  call heltable([16,16,256], n3(:,750), t3x256(:,:,402))
  call heltable([16,16,256], n3(:,751), t3x256(:,:,403))
  call heltable([16,16,256], n3(:,752), t3x256(:,:,404))
  call heltable([16,16,256], n3(:,753), t3x256(:,:,405))
  call heltable([16,16,256], n3(:,754), t3x256(:,:,406))
  call heltable([16,16,256], n3(:,755), t3x256(:,:,407))
  call heltable([16,16,256], n3(:,756), t3x256(:,:,408))
  call heltable([16,16,256], n3(:,757), t3x256(:,:,409))
  call heltable([16,16,256], n3(:,758), t3x256(:,:,410))
  call heltable([16,16,256], n3(:,759), t3x256(:,:,411))
  call heltable([16,16,256], n3(:,760), t3x256(:,:,412))
  call heltable([16,16,256], n3(:,761), t3x256(:,:,413))
  call heltable([16,16,256], n3(:,762), t3x256(:,:,414))
  call heltable([16,16,256], n3(:,763), t3x256(:,:,415))
  call heltable([16,16,256], n3(:,764), t3x256(:,:,416))
  call heltable([16,16,256], n3(:,765), t3x256(:,:,417))
  call heltable([16,16,256], n3(:,766), t3x256(:,:,418))
  call heltable([16,16,256], n3(:,767), t3x256(:,:,419))
  call heltable([16,16,256], n3(:,768), t3x256(:,:,420))
  call heltable([16,16,256], n3(:,769), t3x256(:,:,421))
  call heltable([16,16,256], n3(:,770), t3x256(:,:,422))
  call heltable([16,16,256], n3(:,771), t3x256(:,:,423))
  call heltable([16,16,256], n3(:,772), t3x256(:,:,424))
  call heltable([16,16,256], n3(:,773), t3x256(:,:,425))
  call heltable([16,16,256], n3(:,774), t3x256(:,:,426))
  call heltable([16,16,256], n3(:,775), t3x256(:,:,427))
  call heltable([16,16,256], n3(:,776), t3x256(:,:,428))
  call heltable([16,16,256], n3(:,777), t3x256(:,:,429))
  call heltable([16,16,256], n3(:,778), t3x256(:,:,430))
  call heltable([16,16,256], n3(:,779), t3x256(:,:,431))
  call heltable([16,16,256], n3(:,780), t3x256(:,:,432))
  call heltable([4,64,256], n3(:,781), t3x256(:,:,433))
  call heltable([4,64,256], n3(:,782), t3x256(:,:,434))
  call heltable([16,16,256], n3(:,783), t3x256(:,:,435))
  call heltable([16,16,256], n3(:,784), t3x256(:,:,436))
  call heltable([16,16,256], n3(:,785), t3x256(:,:,437))
  call heltable([16,16,256], n3(:,786), t3x256(:,:,438))
  call heltable([8,32,256], n3(:,787), t3x256(:,:,439))
  call heltable([8,32,256], n3(:,788), t3x256(:,:,440))
  call heltable([16,16,256], n3(:,789), t3x256(:,:,441))
  call heltable([16,16,256], n3(:,790), t3x256(:,:,442))
  call heltable([16,16,256], n3(:,791), t3x256(:,:,443))
  call heltable([16,16,256], n3(:,792), t3x256(:,:,444))
  call heltable([16,16,256], n3(:,793), t3x256(:,:,445))
  call heltable([16,16,256], n3(:,794), t3x256(:,:,446))
  call heltable([4,64,256], n3(:,795), t3x256(:,:,447))
  call heltable([4,64,256], n3(:,796), t3x256(:,:,448))
  call heltable([16,16,256], n3(:,797), t3x256(:,:,449))
  call heltable([16,16,256], n3(:,798), t3x256(:,:,450))
  call heltable([8,32,256], n3(:,799), t3x256(:,:,451))
  call heltable([8,32,256], n3(:,800), t3x256(:,:,452))
  call heltable([16,16,256], n3(:,801), t3x256(:,:,453))
  call heltable([16,16,256], n3(:,802), t3x256(:,:,454))
  call heltable([16,16,256], n3(:,803), t3x256(:,:,455))
  call heltable([16,16,256], n3(:,804), t3x256(:,:,456))
  call heltable([4,64,256], n3(:,805), t3x256(:,:,457))
  call heltable([4,64,256], n3(:,806), t3x256(:,:,458))
  call heltable([4,64,256], n3(:,807), t3x256(:,:,459))
  call heltable([4,64,256], n3(:,808), t3x256(:,:,460))
  call heltable([4,64,256], n3(:,809), t3x256(:,:,461))
  call heltable([4,64,256], n3(:,810), t3x256(:,:,462))
  call heltable([4,64,256], n3(:,811), t3x256(:,:,463))
  call heltable([4,64,256], n3(:,812), t3x256(:,:,464))
  call heltable([16,16,256], n3(:,813), t3x256(:,:,465))
  call heltable([16,16,256], n3(:,814), t3x256(:,:,466))
  call heltable([16,16,256], n3(:,815), t3x256(:,:,467))
  call heltable([16,16,256], n3(:,816), t3x256(:,:,468))
  call heltable([16,16,256], n3(:,817), t3x256(:,:,469))
  call heltable([16,16,256], n3(:,818), t3x256(:,:,470))
  call heltable([16,16,256], n3(:,819), t3x256(:,:,471))
  call heltable([16,16,256], n3(:,820), t3x256(:,:,472))
  call heltable([16,16,256], n3(:,821), t3x256(:,:,473))
  call heltable([16,16,256], n3(:,822), t3x256(:,:,474))
  call heltable([16,16,256], n3(:,823), t3x256(:,:,475))
  call heltable([16,16,256], n3(:,824), t3x256(:,:,476))
  call heltable([16,16,256], n3(:,825), t3x256(:,:,477))
  call heltable([16,16,256], n3(:,826), t3x256(:,:,478))
  call heltable([16,16,256], n3(:,827), t3x256(:,:,479))
  call heltable([16,16,256], n3(:,828), t3x256(:,:,480))
  call heltable([16,16,256], n3(:,829), t3x256(:,:,481))
  call heltable([16,16,256], n3(:,830), t3x256(:,:,482))
  call heltable([16,16,256], n3(:,831), t3x256(:,:,483))
  call heltable([16,16,256], n3(:,832), t3x256(:,:,484))
  call heltable([16,16,256], n3(:,833), t3x256(:,:,485))
  call heltable([16,16,256], n3(:,834), t3x256(:,:,486))
  call heltable([16,16,256], n3(:,835), t3x256(:,:,487))
  call heltable([16,16,256], n3(:,836), t3x256(:,:,488))
  call heltable([16,16,256], n3(:,837), t3x256(:,:,489))
  call heltable([16,16,256], n3(:,838), t3x256(:,:,490))
  call heltable([16,16,256], n3(:,839), t3x256(:,:,491))
  call heltable([16,16,256], n3(:,840), t3x256(:,:,492))
  call heltable([16,16,256], n3(:,841), t3x256(:,:,493))
  call heltable([16,16,256], n3(:,842), t3x256(:,:,494))
  call heltable([16,16,256], n3(:,843), t3x256(:,:,495))
  call heltable([16,16,256], n3(:,844), t3x256(:,:,496))
  call heltable([16,16,256], n3(:,845), t3x256(:,:,497))
  call heltable([16,16,256], n3(:,846), t3x256(:,:,498))
  call heltable([16,16,256], n3(:,847), t3x256(:,:,499))
  call heltable([16,16,256], n3(:,848), t3x256(:,:,500))
  call heltable([16,16,256], n3(:,849), t3x256(:,:,501))
  call heltable([16,16,256], n3(:,850), t3x256(:,:,502))
  call heltable([16,16,256], n3(:,851), t3x256(:,:,503))
  call heltable([16,16,256], n3(:,852), t3x256(:,:,504))
  call heltable([16,16,256], n3(:,853), t3x256(:,:,505))
  call heltable([16,16,256], n3(:,854), t3x256(:,:,506))
  call heltable([16,16,256], n3(:,855), t3x256(:,:,507))
  call heltable([16,16,256], n3(:,856), t3x256(:,:,508))
  call heltable([16,16,256], n3(:,857), t3x256(:,:,509))
  call heltable([16,16,256], n3(:,858), t3x256(:,:,510))
  call heltable([16,16,256], n3(:,859), t3x256(:,:,511))
  call heltable([16,16,256], n3(:,860), t3x256(:,:,512))
  call heltable([16,16,256], n3(:,861), t3x256(:,:,513))
  call heltable([16,16,256], n3(:,862), t3x256(:,:,514))
  call heltable([16,16,256], n3(:,863), t3x256(:,:,515))
  call heltable([16,16,256], n3(:,864), t3x256(:,:,516))
  call heltable([16,16,256], n3(:,865), t3x256(:,:,517))
  call heltable([16,16,256], n3(:,866), t3x256(:,:,518))
  call heltable([16,16,256], n3(:,867), t3x256(:,:,519))
  call heltable([16,16,256], n3(:,868), t3x256(:,:,520))
  call heltable([16,16,256], n3(:,869), t3x256(:,:,521))
  call heltable([16,16,256], n3(:,870), t3x256(:,:,522))
  call heltable([16,16,256], n3(:,871), t3x256(:,:,523))
  call heltable([16,16,256], n3(:,872), t3x256(:,:,524))
  call heltable([16,16,256], n3(:,873), t3x256(:,:,525))
  call heltable([16,16,256], n3(:,874), t3x256(:,:,526))
  call heltable([16,16,256], n3(:,875), t3x256(:,:,527))
  call heltable([16,16,256], n3(:,876), t3x256(:,:,528))
  call heltable([16,16,256], n3(:,877), t3x256(:,:,529))
  call heltable([16,16,256], n3(:,878), t3x256(:,:,530))
  call heltable([16,16,256], n3(:,879), t3x256(:,:,531))
  call heltable([16,16,256], n3(:,880), t3x256(:,:,532))
  call heltable([16,16,256], n3(:,881), t3x256(:,:,533))
  call heltable([16,16,256], n3(:,882), t3x256(:,:,534))
  call heltable([16,16,256], n3(:,883), t3x256(:,:,535))
  call heltable([16,16,256], n3(:,884), t3x256(:,:,536))
  call heltable([16,16,256], n3(:,885), t3x256(:,:,537))
  call heltable([16,16,256], n3(:,886), t3x256(:,:,538))
  call heltable([16,16,256], n3(:,887), t3x256(:,:,539))
  call heltable([16,16,256], n3(:,888), t3x256(:,:,540))
  call heltable([16,16,256], n3(:,889), t3x256(:,:,541))
  call heltable([16,16,256], n3(:,890), t3x256(:,:,542))
  call heltable([16,16,256], n3(:,891), t3x256(:,:,543))
  call heltable([16,16,256], n3(:,892), t3x256(:,:,544))
  call heltable([16,16,256], n3(:,893), t3x256(:,:,545))
  call heltable([16,16,256], n3(:,894), t3x256(:,:,546))
  call heltable([16,16,256], n3(:,895), t3x256(:,:,547))
  call heltable([16,16,256], n3(:,896), t3x256(:,:,548))
  call heltable([16,16,256], n3(:,897), t3x256(:,:,549))
  call heltable([16,16,256], n3(:,898), t3x256(:,:,550))
  call heltable([16,16,256], n3(:,899), t3x256(:,:,551))
  call heltable([16,16,256], n3(:,900), t3x256(:,:,552))
  call heltable([16,16,256], n3(:,901), t3x256(:,:,553))
  call heltable([16,16,256], n3(:,902), t3x256(:,:,554))
  call heltable([16,16,256], n3(:,903), t3x256(:,:,555))
  call heltable([16,16,256], n3(:,904), t3x256(:,:,556))
  call heltable([16,16,256], n3(:,905), t3x256(:,:,557))
  call heltable([16,16,256], n3(:,906), t3x256(:,:,558))
  call heltable([16,16,256], n3(:,907), t3x256(:,:,559))
  call heltable([16,16,256], n3(:,908), t3x256(:,:,560))
  call heltable([16,16,256], n3(:,909), t3x256(:,:,561))
  call heltable([16,16,256], n3(:,910), t3x256(:,:,562))
  call heltable([8,32,256], n3(:,911), t3x256(:,:,563))
  call heltable([8,32,256], n3(:,912), t3x256(:,:,564))
  call heltable([16,16,256], n3(:,913), t3x256(:,:,565))
  call heltable([16,16,256], n3(:,914), t3x256(:,:,566))
  call heltable([4,64,256], n3(:,915), t3x256(:,:,567))
  call heltable([4,64,256], n3(:,916), t3x256(:,:,568))
  call heltable([16,16,256], n3(:,917), t3x256(:,:,569))
  call heltable([16,16,256], n3(:,918), t3x256(:,:,570))
  call heltable([16,16,256], n3(:,919), t3x256(:,:,571))
  call heltable([16,16,256], n3(:,920), t3x256(:,:,572))
  call heltable([16,16,256], n3(:,921), t3x256(:,:,573))
  call heltable([16,16,256], n3(:,922), t3x256(:,:,574))
  call heltable([16,16,256], n3(:,923), t3x256(:,:,575))
  call heltable([16,16,256], n3(:,924), t3x256(:,:,576))
  call heltable([16,16,256], n3(:,925), t3x256(:,:,577))
  call heltable([16,16,256], n3(:,926), t3x256(:,:,578))
  call heltable([8,32,256], n3(:,927), t3x256(:,:,579))
  call heltable([8,32,256], n3(:,928), t3x256(:,:,580))
  call heltable([16,16,256], n3(:,929), t3x256(:,:,581))
  call heltable([16,16,256], n3(:,930), t3x256(:,:,582))
  call heltable([4,64,256], n3(:,931), t3x256(:,:,583))
  call heltable([4,64,256], n3(:,932), t3x256(:,:,584))
  call heltable([16,16,256], n3(:,933), t3x256(:,:,585))
  call heltable([16,16,256], n3(:,934), t3x256(:,:,586))
  call heltable([16,16,256], n3(:,935), t3x256(:,:,587))
  call heltable([16,16,256], n3(:,936), t3x256(:,:,588))
  call heltable([16,16,256], n3(:,937), t3x256(:,:,589))
  call heltable([16,16,256], n3(:,938), t3x256(:,:,590))
  call heltable([16,16,256], n3(:,939), t3x256(:,:,591))
  call heltable([16,16,256], n3(:,940), t3x256(:,:,592))
  call heltable([16,16,256], n3(:,941), t3x256(:,:,593))
  call heltable([16,16,256], n3(:,942), t3x256(:,:,594))
  call heltable([8,32,256], n3(:,943), t3x256(:,:,595))
  call heltable([8,32,256], n3(:,944), t3x256(:,:,596))
  call heltable([16,16,256], n3(:,945), t3x256(:,:,597))
  call heltable([16,16,256], n3(:,946), t3x256(:,:,598))
  call heltable([4,64,256], n3(:,947), t3x256(:,:,599))
  call heltable([4,64,256], n3(:,948), t3x256(:,:,600))
  call heltable([16,16,256], n3(:,949), t3x256(:,:,601))
  call heltable([16,16,256], n3(:,950), t3x256(:,:,602))
  call heltable([16,16,256], n3(:,951), t3x256(:,:,603))
  call heltable([16,16,256], n3(:,952), t3x256(:,:,604))
  call heltable([16,16,256], n3(:,953), t3x256(:,:,605))
  call heltable([16,16,256], n3(:,954), t3x256(:,:,606))
  call heltable([16,16,256], n3(:,955), t3x256(:,:,607))
  call heltable([16,16,256], n3(:,956), t3x256(:,:,608))
  call heltable([16,16,256], n3(:,957), t3x256(:,:,609))
  call heltable([16,16,256], n3(:,958), t3x256(:,:,610))
  call heltable([8,32,256], n3(:,959), t3x256(:,:,611))
  call heltable([8,32,256], n3(:,960), t3x256(:,:,612))
  call heltable([16,16,256], n3(:,961), t3x256(:,:,613))
  call heltable([16,16,256], n3(:,962), t3x256(:,:,614))
  call heltable([4,64,256], n3(:,963), t3x256(:,:,615))
  call heltable([4,64,256], n3(:,964), t3x256(:,:,616))
  call heltable([16,16,256], n3(:,965), t3x256(:,:,617))
  call heltable([16,16,256], n3(:,966), t3x256(:,:,618))
  call heltable([16,16,256], n3(:,967), t3x256(:,:,619))
  call heltable([16,16,256], n3(:,968), t3x256(:,:,620))
  call heltable([16,16,256], n3(:,969), t3x256(:,:,621))
  call heltable([16,16,256], n3(:,970), t3x256(:,:,622))
  call heltable([16,16,256], n3(:,971), t3x256(:,:,623))
  call heltable([16,16,256], n3(:,972), t3x256(:,:,624))
  call heltable([16,16,256], n3(:,973), t3x256(:,:,625))
  call heltable([16,16,256], n3(:,974), t3x256(:,:,626))
  call heltable([8,32,256], n3(:,975), t3x256(:,:,627))
  call heltable([8,32,256], n3(:,976), t3x256(:,:,628))
  call heltable([16,16,256], n3(:,977), t3x256(:,:,629))
  call heltable([16,16,256], n3(:,978), t3x256(:,:,630))
  call heltable([4,64,256], n3(:,979), t3x256(:,:,631))
  call heltable([4,64,256], n3(:,980), t3x256(:,:,632))
  call heltable([16,16,256], n3(:,981), t3x256(:,:,633))
  call heltable([16,16,256], n3(:,982), t3x256(:,:,634))
  call heltable([16,16,256], n3(:,983), t3x256(:,:,635))
  call heltable([16,16,256], n3(:,984), t3x256(:,:,636))
  call heltable([16,16,256], n3(:,985), t3x256(:,:,637))
  call heltable([16,16,256], n3(:,986), t3x256(:,:,638))
  call heltable([16,16,256], n3(:,987), t3x256(:,:,639))
  call heltable([16,16,256], n3(:,988), t3x256(:,:,640))
  call heltable([16,16,256], n3(:,989), t3x256(:,:,641))
  call heltable([16,16,256], n3(:,990), t3x256(:,:,642))
  call heltable([8,32,256], n3(:,991), t3x256(:,:,643))
  call heltable([8,32,256], n3(:,992), t3x256(:,:,644))
  call heltable([16,16,256], n3(:,993), t3x256(:,:,645))
  call heltable([16,16,256], n3(:,994), t3x256(:,:,646))
  call heltable([4,64,256], n3(:,995), t3x256(:,:,647))
  call heltable([4,64,256], n3(:,996), t3x256(:,:,648))
  call heltable([16,16,256], n3(:,997), t3x256(:,:,649))
  call heltable([16,16,256], n3(:,998), t3x256(:,:,650))
  call heltable([16,16,256], n3(:,999), t3x256(:,:,651))
  call heltable([16,16,256], n3(:,1000), t3x256(:,:,652))
  call heltable([16,16,256], n3(:,1001), t3x256(:,:,653))
  call heltable([16,16,256], n3(:,1002), t3x256(:,:,654))
  call heltable([16,16,256], n3(:,1003), t3x256(:,:,655))
  call heltable([16,16,256], n3(:,1004), t3x256(:,:,656))
  call heltable([16,16,256], n3(:,1005), t3x256(:,:,657))
  call heltable([16,16,256], n3(:,1006), t3x256(:,:,658))
  call heltable([8,32,256], n3(:,1007), t3x256(:,:,659))
  call heltable([8,32,256], n3(:,1008), t3x256(:,:,660))
  call heltable([16,16,256], n3(:,1009), t3x256(:,:,661))
  call heltable([16,16,256], n3(:,1010), t3x256(:,:,662))
  call heltable([4,64,256], n3(:,1011), t3x256(:,:,663))
  call heltable([4,64,256], n3(:,1012), t3x256(:,:,664))
  call heltable([16,16,256], n3(:,1013), t3x256(:,:,665))
  call heltable([16,16,256], n3(:,1014), t3x256(:,:,666))
  call heltable([16,16,256], n3(:,1015), t3x256(:,:,667))
  call heltable([16,16,256], n3(:,1016), t3x256(:,:,668))
  call heltable([16,16,256], n3(:,1017), t3x256(:,:,669))
  call heltable([16,16,256], n3(:,1018), t3x256(:,:,670))
  call heltable([16,16,256], n3(:,1019), t3x256(:,:,671))
  call heltable([16,16,256], n3(:,1020), t3x256(:,:,672))
  call heltable([16,16,256], n3(:,1021), t3x256(:,:,673))
  call heltable([16,16,256], n3(:,1022), t3x256(:,:,674))
  call heltable([8,32,256], n3(:,1023), t3x256(:,:,675))
  call heltable([8,32,256], n3(:,1024), t3x256(:,:,676))
  call heltable([16,16,256], n3(:,1025), t3x256(:,:,677))
  call heltable([16,16,256], n3(:,1026), t3x256(:,:,678))
  call heltable([4,64,256], n3(:,1027), t3x256(:,:,679))
  call heltable([4,64,256], n3(:,1028), t3x256(:,:,680))
  call heltable([16,16,256], n3(:,1029), t3x256(:,:,681))
  call heltable([16,16,256], n3(:,1030), t3x256(:,:,682))
  call heltable([16,16,256], n3(:,1031), t3x256(:,:,683))
  call heltable([16,16,256], n3(:,1032), t3x256(:,:,684))
  call heltable([16,16,256], n3(:,1033), t3x256(:,:,685))
  call heltable([16,16,256], n3(:,1034), t3x256(:,:,686))
  call heltable([16,16,256], n3(:,1035), t3x256(:,:,687))
  call heltable([16,16,256], n3(:,1036), t3x256(:,:,688))
  call heltable([16,16,256], n3(:,1037), t3x256(:,:,689))
  call heltable([16,16,256], n3(:,1038), t3x256(:,:,690))
  call heltable([16,16,256], n3(:,1039), t3x256(:,:,691))
  call heltable([16,16,256], n3(:,1040), t3x256(:,:,692))
  call heltable([16,16,256], n3(:,1041), t3x256(:,:,693))
  call heltable([16,16,256], n3(:,1042), t3x256(:,:,694))
  call heltable([16,16,256], n3(:,1043), t3x256(:,:,695))
  call heltable([16,16,256], n3(:,1044), t3x256(:,:,696))
  call heltable([16,16,256], n3(:,1045), t3x256(:,:,697))
  call heltable([16,16,256], n3(:,1046), t3x256(:,:,698))
  call heltable([16,16,256], n3(:,1047), t3x256(:,:,699))
  call heltable([16,16,256], n3(:,1048), t3x256(:,:,700))
  call heltable([16,16,256], n3(:,1049), t3x256(:,:,701))
  call heltable([16,16,256], n3(:,1050), t3x256(:,:,702))
  call heltable([16,16,256], n3(:,1051), t3x256(:,:,703))
  call heltable([16,16,256], n3(:,1052), t3x256(:,:,704))
  call heltable([16,16,256], n3(:,1053), t3x256(:,:,705))
  call heltable([16,16,256], n3(:,1054), t3x256(:,:,706))
  call heltable([16,16,256], n3(:,1055), t3x256(:,:,707))
  call heltable([16,16,256], n3(:,1056), t3x256(:,:,708))
  call heltable([16,16,256], n3(:,1057), t3x256(:,:,709))
  call heltable([16,16,256], n3(:,1058), t3x256(:,:,710))
  call heltable([16,16,256], n3(:,1059), t3x256(:,:,711))
  call heltable([16,16,256], n3(:,1060), t3x256(:,:,712))
  call heltable([16,16,256], n3(:,1061), t3x256(:,:,713))
  call heltable([16,16,256], n3(:,1062), t3x256(:,:,714))
  call heltable([16,16,256], n3(:,1063), t3x256(:,:,715))
  call heltable([16,16,256], n3(:,1064), t3x256(:,:,716))
  call heltable([16,16,256], n3(:,1065), t3x256(:,:,717))
  call heltable([16,16,256], n3(:,1066), t3x256(:,:,718))
  call heltable([16,16,256], n3(:,1067), t3x256(:,:,719))
  call heltable([16,16,256], n3(:,1068), t3x256(:,:,720))
  call heltable([16,16,256], n3(:,1069), t3x256(:,:,721))
  call heltable([16,16,256], n3(:,1070), t3x256(:,:,722))
  call heltable([16,16,256], n3(:,1071), t3x256(:,:,723))
  call heltable([16,16,256], n3(:,1072), t3x256(:,:,724))
  call heltable([16,16,256], n3(:,1073), t3x256(:,:,725))
  call heltable([16,16,256], n3(:,1074), t3x256(:,:,726))
  call heltable([16,16,256], n3(:,1075), t3x256(:,:,727))
  call heltable([16,16,256], n3(:,1076), t3x256(:,:,728))
  call heltable([16,16,256], n3(:,1077), t3x256(:,:,729))
  call heltable([16,16,256], n3(:,1078), t3x256(:,:,730))
  call heltable([16,16,256], n3(:,1079), t3x256(:,:,731))
  call heltable([16,16,256], n3(:,1080), t3x256(:,:,732))
  call heltable([16,16,256], n3(:,1081), t3x256(:,:,733))
  call heltable([16,16,256], n3(:,1082), t3x256(:,:,734))
  call heltable([16,16,256], n3(:,1083), t3x256(:,:,735))
  call heltable([16,16,256], n3(:,1084), t3x256(:,:,736))
  call heltable([16,16,256], n3(:,1085), t3x256(:,:,737))
  call heltable([16,16,256], n3(:,1086), t3x256(:,:,738))
  call heltable([16,16,256], n3(:,1087), t3x256(:,:,739))
  call heltable([16,16,256], n3(:,1088), t3x256(:,:,740))
  call heltable([16,16,256], n3(:,1089), t3x256(:,:,741))
  call heltable([16,16,256], n3(:,1090), t3x256(:,:,742))
  call heltable([16,16,256], n3(:,1091), t3x256(:,:,743))
  call heltable([16,16,256], n3(:,1092), t3x256(:,:,744))
  call heltable([16,16,256], n3(:,1093), t3x256(:,:,745))
  call heltable([16,16,256], n3(:,1094), t3x256(:,:,746))
  call heltable([16,16,256], n3(:,1095), t3x256(:,:,747))
  call heltable([16,16,256], n3(:,1096), t3x256(:,:,748))
  call heltable([16,16,256], n3(:,1097), t3x256(:,:,749))
  call heltable([16,16,256], n3(:,1098), t3x256(:,:,750))
  call heltable([16,16,256], n3(:,1099), t3x256(:,:,751))
  call heltable([16,16,256], n3(:,1100), t3x256(:,:,752))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppllajj_eexdddxdxag_1
