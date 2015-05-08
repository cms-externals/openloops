
module ol_external_ppajjj_ddxssxagg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppajjj_ddxssxagg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_ppajjj_ddxssxagg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_ppajjj_ddxssxagg_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppajjj_ddxssxagg_1(7) = &
                     [ 1, 2, 3, 4, 5, 6, 6 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppajjj_ddxssxagg_1(7) = &
                     [ 6, 6, 6, 6, 2, 16, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppajjj_ddxssxagg_1 = &
                     72
  integer, save :: channel_number_ppajjj_ddxssxagg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,128) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(128,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppajjj_ddxssxagg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppajjj_ddxssxagg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppajjj_ddxssxagg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppajjj_ddxssxagg_1(7)
    external_perm_ppajjj_ddxssxagg_1 = perm
    do i = 1, 7
      external_perm_inv_ppajjj_ddxssxagg_1( &
        external_perm_ppajjj_ddxssxagg_1(i)) = i
      particle_types_perm_ppajjj_ddxssxagg_1(i) = &
        particle_types_ppajjj_ddxssxagg_1( &
        external_perm_ppajjj_ddxssxagg_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_ppajjj_ddxssxagg_1(i) >= &
          external_perm_ppajjj_ddxssxagg_1(j)) then
          ii = external_perm_ppajjj_ddxssxagg_1(i)
          jj = external_perm_ppajjj_ddxssxagg_1(j)
        else
          ii = external_perm_ppajjj_ddxssxagg_1(j)
          jj = external_perm_ppajjj_ddxssxagg_1(i)
        end if
        extcomb_perm_ppajjj_ddxssxagg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppajjj_ddxssxagg_1 = &
      average_factors_ppajjj_ddxssxagg_1( &
      external_perm_ppajjj_ddxssxagg_1(1)) &
      * average_factors_ppajjj_ddxssxagg_1( &
      external_perm_ppajjj_ddxssxagg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_ppajjj_ddxssxagg_1 = &
          average_factor_ppajjj_ddxssxagg_1 &
          * factorial(count(particle_types_perm_ppajjj_ddxssxagg_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppajjj_ddxssxagg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppajjj_ddxssxagg_1")
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
      m_ex(i) = m_ex_orig(external_perm_ppajjj_ddxssxagg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppajjj_ddxssxagg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppajjj_ddxssxagg_1")
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
      & bind(c,name="ol_rambo_ppajjj_ddxssxagg_1")
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

  H_HC(:,5) = [ ((((2*(binco-1)+flip)*4+binpos, flip = 0, 1), binpos = 1, 4), binco = 1, 128/4/2) ]
  H_HC(:,6) = [ ((((2*(binco-1)+flip)*2+binpos, flip = 0, 1), binpos = 1, 2), binco = 1, 128/2/2) ]
  H_HC(:,7) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 128/1/2) ]
  end subroutine hel_init

end module ol_external_ppajjj_ddxssxagg_1


module colour_basis_ppajjj_ddxssxagg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,0,2,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppajjj_ddxssxagg_1")
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
    nhel = 128
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_ppajjj_ddxssxagg_1")
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
      [2,570,1,2,557,1,2,516,1,2,510,1,3,584,1,3,586,1,3,595,1,3,600,1,3,704,1,3,706,1,3,715,1,3,720,1], &
      [3,12])
#endif
  end subroutine tree_colbasis

end module colour_basis_ppajjj_ddxssxagg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppajjj_ddxssxagg_1(perm)
  use ol_external_ppajjj_ddxssxagg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_ppajjj_ddxssxagg_1

! **********************************************************************
module ol_heltables_ppajjj_ddxssxagg_1
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
    H7(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(44), n3(3,365), n4(4,6)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,15), t3x8(2,8,48), t3x16(2,16,98), t3x128(2,128,204), t4x16(3,16,6)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(128,7)
  integer,           save :: exthel(128,7)
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
  n2(1) = 4
  call heltable([4,2,8], n3(:,3), t3x8(:,:,1))
  call heltable([4,2,2,16], n4(:,1), t4x16(:,:,1))
  call heltable([2,2,4,16], n4(:,2), t4x16(:,:,2))
  call heltable([2,4,2,16], n4(:,3), t4x16(:,:,3))
  call heltable([2,2,4], n3(:,4), t3x4(:,:,3))
  n2(2) = 4
  call heltable([2,4,8], n3(:,5), t3x8(:,:,2))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,4))
  call heltable([2,2,4], n3(:,7), t3x4(:,:,5))
  n2(3) = 4
  call heltable([4,2,8], n3(:,8), t3x8(:,:,3))
  call heltable([4,2,2,16], n4(:,4), t4x16(:,:,4))
  call heltable([2,2,4,16], n4(:,5), t4x16(:,:,5))
  call heltable([2,4,2,16], n4(:,6), t4x16(:,:,6))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,6))
  n2(4) = 4
  call heltable([2,4,8], n3(:,10), t3x8(:,:,4))
  call heltable([2,2,4], n3(:,11), t3x4(:,:,7))
  n2(5) = 4
  call heltable([4,2,8], n3(:,12), t3x8(:,:,5))
  call heltable([4,4,16], n3(:,13), t3x16(:,:,1))
  call heltable([2,4,8], n3(:,14), t3x8(:,:,6))
  call heltable([4,4,16], n3(:,15), t3x16(:,:,2))
  n2(6) = 8
  call heltable([4,2,8], n3(:,16), t3x8(:,:,7))
  call heltable([4,4,16], n3(:,17), t3x16(:,:,3))
  n2(7) = 8
  call heltable([2,2,4], n3(:,18), t3x4(:,:,8))
  n2(8) = 4
  call heltable([4,2,8], n3(:,19), t3x8(:,:,8))
  call heltable([4,4,16], n3(:,20), t3x16(:,:,4))
  call heltable([2,4,8], n3(:,21), t3x8(:,:,9))
  call heltable([4,4,16], n3(:,22), t3x16(:,:,5))
  n2(9) = 8
  call heltable([4,2,8], n3(:,23), t3x8(:,:,10))
  n2(10) = 8
  call heltable([2,2,4], n3(:,24), t3x4(:,:,9))
  call heltable([2,4,8], n3(:,25), t3x8(:,:,11))
  call heltable([4,4,16], n3(:,26), t3x16(:,:,6))
  n2(11) = 8
  call heltable([4,4,16], n3(:,27), t3x16(:,:,7))
  call heltable([2,4,8], n3(:,28), t3x8(:,:,12))
  n2(12) = 8
  call heltable([8,2,16], n3(:,29), t3x16(:,:,8))
  call heltable([8,2,16], n3(:,30), t3x16(:,:,9))
  call heltable([8,2,16], n3(:,31), t3x16(:,:,10))
  call heltable([2,8,16], n3(:,32), t3x16(:,:,11))
  call heltable([8,2,16], n3(:,33), t3x16(:,:,12))
  call heltable([8,2,16], n3(:,34), t3x16(:,:,13))
  call heltable([2,2,4], n3(:,35), t3x4(:,:,10))
  n2(13) = 4
  call heltable([4,4,16], n3(:,36), t3x16(:,:,14))
  call heltable([2,4,8], n3(:,37), t3x8(:,:,13))
  call heltable([4,4,16], n3(:,38), t3x16(:,:,15))
  n2(14) = 8
  call heltable([4,2,8], n3(:,39), t3x8(:,:,14))
  call heltable([4,4,16], n3(:,40), t3x16(:,:,16))
  n2(15) = 8
  call heltable([2,2,4], n3(:,41), t3x4(:,:,11))
  n2(16) = 4
  call heltable([4,4,16], n3(:,42), t3x16(:,:,17))
  call heltable([2,4,8], n3(:,43), t3x8(:,:,15))
  n2(17) = 8
  call heltable([4,2,8], n3(:,44), t3x8(:,:,16))
  call heltable([4,4,16], n3(:,45), t3x16(:,:,18))
  n2(18) = 8
  call heltable([4,2,8], n3(:,46), t3x8(:,:,17))
  call heltable([4,4,16], n3(:,47), t3x16(:,:,19))
  n2(19) = 8
  call heltable([4,2,8], n3(:,48), t3x8(:,:,18))
  n2(20) = 8
  call heltable([2,8,16], n3(:,49), t3x16(:,:,20))
  call heltable([2,8,16], n3(:,50), t3x16(:,:,21))
  call heltable([8,2,16], n3(:,51), t3x16(:,:,22))
  call heltable([8,2,16], n3(:,52), t3x16(:,:,23))
  call heltable([8,2,16], n3(:,53), t3x16(:,:,24))
  call heltable([2,8,16], n3(:,54), t3x16(:,:,25))
  call heltable([2,4,8], n3(:,55), t3x8(:,:,19))
  n2(21) = 8
  call heltable([4,2,8], n3(:,56), t3x8(:,:,20))
  n2(22) = 8
  call heltable([8,2,16], n3(:,57), t3x16(:,:,26))
  call heltable([8,2,16], n3(:,58), t3x16(:,:,27))
  call heltable([2,4,8], n3(:,59), t3x8(:,:,21))
  n2(23) = 8
  call heltable([4,2,8], n3(:,60), t3x8(:,:,22))
  n2(24) = 8
  call heltable([2,8,16], n3(:,61), t3x16(:,:,28))
  call heltable([2,8,16], n3(:,62), t3x16(:,:,29))
  call heltable([8,2,16], n3(:,63), t3x16(:,:,30))
  call heltable([2,8,16], n3(:,64), t3x16(:,:,31))
  call heltable([2,8,16], n3(:,65), t3x16(:,:,32))
  call heltable([2,2,4], n3(:,66), t3x4(:,:,12))
  n2(25) = 4
  call heltable([2,4,8], n3(:,67), t3x8(:,:,23))
  call heltable([4,4,16], n3(:,68), t3x16(:,:,33))
  n2(26) = 8
  call heltable([4,2,8], n3(:,69), t3x8(:,:,24))
  call heltable([4,4,16], n3(:,70), t3x16(:,:,34))
  n2(27) = 8
  call heltable([4,2,8], n3(:,71), t3x8(:,:,25))
  call heltable([4,4,16], n3(:,72), t3x16(:,:,35))
  call heltable([2,2,4], n3(:,73), t3x4(:,:,13))
  n2(28) = 4
  call heltable([2,4,8], n3(:,74), t3x8(:,:,26))
  call heltable([4,4,16], n3(:,75), t3x16(:,:,36))
  n2(29) = 8
  call heltable([4,2,8], n3(:,76), t3x8(:,:,27))
  n2(30) = 8
  call heltable([4,2,8], n3(:,77), t3x8(:,:,28))
  call heltable([4,4,16], n3(:,78), t3x16(:,:,37))
  call heltable([4,4,16], n3(:,79), t3x16(:,:,38))
  call heltable([2,4,8], n3(:,80), t3x8(:,:,29))
  call heltable([4,4,16], n3(:,81), t3x16(:,:,39))
  n2(31) = 8
  call heltable([2,4,8], n3(:,82), t3x8(:,:,30))
  n2(32) = 8
  call heltable([8,2,16], n3(:,83), t3x16(:,:,40))
  call heltable([8,2,16], n3(:,84), t3x16(:,:,41))
  call heltable([8,2,16], n3(:,85), t3x16(:,:,42))
  call heltable([8,2,16], n3(:,86), t3x16(:,:,43))
  call heltable([8,2,16], n3(:,87), t3x16(:,:,44))
  call heltable([2,8,16], n3(:,88), t3x16(:,:,45))
  call heltable([2,2,4], n3(:,89), t3x4(:,:,14))
  n2(33) = 4
  call heltable([2,4,8], n3(:,90), t3x8(:,:,31))
  call heltable([4,4,16], n3(:,91), t3x16(:,:,46))
  n2(34) = 8
  call heltable([4,2,8], n3(:,92), t3x8(:,:,32))
  call heltable([4,4,16], n3(:,93), t3x16(:,:,47))
  n2(35) = 8
  call heltable([4,4,16], n3(:,94), t3x16(:,:,48))
  call heltable([2,2,4], n3(:,95), t3x4(:,:,15))
  n2(36) = 4
  call heltable([2,4,8], n3(:,96), t3x8(:,:,33))
  n2(37) = 8
  call heltable([4,2,8], n3(:,97), t3x8(:,:,34))
  call heltable([4,4,16], n3(:,98), t3x16(:,:,49))
  n2(38) = 8
  call heltable([4,4,16], n3(:,99), t3x16(:,:,50))
  call heltable([4,2,8], n3(:,100), t3x8(:,:,35))
  call heltable([4,4,16], n3(:,101), t3x16(:,:,51))
  n2(39) = 8
  call heltable([4,2,8], n3(:,102), t3x8(:,:,36))
  n2(40) = 8
  call heltable([8,2,16], n3(:,103), t3x16(:,:,52))
  call heltable([8,2,16], n3(:,104), t3x16(:,:,53))
  call heltable([2,8,16], n3(:,105), t3x16(:,:,54))
  call heltable([2,8,16], n3(:,106), t3x16(:,:,55))
  call heltable([2,8,16], n3(:,107), t3x16(:,:,56))
  call heltable([8,2,16], n3(:,108), t3x16(:,:,57))
  call heltable([2,4,8], n3(:,109), t3x8(:,:,37))
  n2(41) = 8
  call heltable([4,2,8], n3(:,110), t3x8(:,:,38))
  n2(42) = 8
  call heltable([8,2,16], n3(:,111), t3x16(:,:,58))
  call heltable([8,2,16], n3(:,112), t3x16(:,:,59))
  call heltable([2,4,8], n3(:,113), t3x8(:,:,39))
  n2(43) = 8
  call heltable([4,2,8], n3(:,114), t3x8(:,:,40))
  n2(44) = 8
  call heltable([2,8,16], n3(:,115), t3x16(:,:,60))
  call heltable([2,8,16], n3(:,116), t3x16(:,:,61))
  call heltable([8,2,16], n3(:,117), t3x16(:,:,62))
  call heltable([2,8,16], n3(:,118), t3x16(:,:,63))
  call heltable([2,8,16], n3(:,119), t3x16(:,:,64))
  call heltable([4,2,8], n3(:,120), t3x8(:,:,41))
  call heltable([2,4,8], n3(:,121), t3x8(:,:,42))
  call heltable([4,2,8], n3(:,122), t3x8(:,:,43))
  call heltable([4,4,16], n3(:,123), t3x16(:,:,65))
  call heltable([2,8,16], n3(:,124), t3x16(:,:,66))
  call heltable([2,8,16], n3(:,125), t3x16(:,:,67))
  call heltable([2,4,8], n3(:,126), t3x8(:,:,44))
  call heltable([4,4,16], n3(:,127), t3x16(:,:,68))
  call heltable([8,2,16], n3(:,128), t3x16(:,:,69))
  call heltable([2,8,16], n3(:,129), t3x16(:,:,70))
  call heltable([2,8,16], n3(:,130), t3x16(:,:,71))
  call heltable([2,8,16], n3(:,131), t3x16(:,:,72))
  call heltable([2,8,16], n3(:,132), t3x16(:,:,73))
  call heltable([8,2,16], n3(:,133), t3x16(:,:,74))
  call heltable([8,2,16], n3(:,134), t3x16(:,:,75))
  call heltable([8,2,16], n3(:,135), t3x16(:,:,76))
  call heltable([8,2,16], n3(:,136), t3x16(:,:,77))
  call heltable([8,2,16], n3(:,137), t3x16(:,:,78))
  call heltable([4,4,16], n3(:,138), t3x16(:,:,79))
  call heltable([4,2,8], n3(:,139), t3x8(:,:,45))
  call heltable([2,8,16], n3(:,140), t3x16(:,:,80))
  call heltable([8,2,16], n3(:,141), t3x16(:,:,81))
  call heltable([2,8,16], n3(:,142), t3x16(:,:,82))
  call heltable([4,4,16], n3(:,143), t3x16(:,:,83))
  call heltable([2,4,8], n3(:,144), t3x8(:,:,46))
  call heltable([2,8,16], n3(:,145), t3x16(:,:,84))
  call heltable([8,2,16], n3(:,146), t3x16(:,:,85))
  call heltable([8,2,16], n3(:,147), t3x16(:,:,86))
  call heltable([4,2,8], n3(:,148), t3x8(:,:,47))
  call heltable([2,4,8], n3(:,149), t3x8(:,:,48))
  call heltable([2,8,16], n3(:,150), t3x16(:,:,87))
  call heltable([2,8,16], n3(:,151), t3x16(:,:,88))
  call heltable([2,8,16], n3(:,152), t3x16(:,:,89))
  call heltable([2,8,16], n3(:,153), t3x16(:,:,90))
  call heltable([2,8,16], n3(:,154), t3x16(:,:,91))
  call heltable([8,2,16], n3(:,155), t3x16(:,:,92))
  call heltable([8,2,16], n3(:,156), t3x16(:,:,93))
  call heltable([2,8,16], n3(:,157), t3x16(:,:,94))
  call heltable([8,2,16], n3(:,158), t3x16(:,:,95))
  call heltable([8,2,16], n3(:,159), t3x16(:,:,96))
  call heltable([8,2,16], n3(:,160), t3x16(:,:,97))
  call heltable([8,2,16], n3(:,161), t3x16(:,:,98))
  call heltable([8,16,128], n3(:,162), t3x128(:,:,1))
  call heltable([8,16,128], n3(:,163), t3x128(:,:,2))
  call heltable([8,16,128], n3(:,164), t3x128(:,:,3))
  call heltable([16,8,128], n3(:,165), t3x128(:,:,4))
  call heltable([16,8,128], n3(:,166), t3x128(:,:,5))
  call heltable([16,8,128], n3(:,167), t3x128(:,:,6))
  call heltable([8,16,128], n3(:,168), t3x128(:,:,7))
  call heltable([8,16,128], n3(:,169), t3x128(:,:,8))
  call heltable([8,16,128], n3(:,170), t3x128(:,:,9))
  call heltable([16,8,128], n3(:,171), t3x128(:,:,10))
  call heltable([16,8,128], n3(:,172), t3x128(:,:,11))
  call heltable([16,8,128], n3(:,173), t3x128(:,:,12))
  call heltable([8,16,128], n3(:,174), t3x128(:,:,13))
  call heltable([16,8,128], n3(:,175), t3x128(:,:,14))
  call heltable([16,8,128], n3(:,176), t3x128(:,:,15))
  call heltable([8,16,128], n3(:,177), t3x128(:,:,16))
  call heltable([16,8,128], n3(:,178), t3x128(:,:,17))
  call heltable([16,8,128], n3(:,179), t3x128(:,:,18))
  call heltable([16,8,128], n3(:,180), t3x128(:,:,19))
  call heltable([8,16,128], n3(:,181), t3x128(:,:,20))
  call heltable([16,8,128], n3(:,182), t3x128(:,:,21))
  call heltable([8,16,128], n3(:,183), t3x128(:,:,22))
  call heltable([8,16,128], n3(:,184), t3x128(:,:,23))
  call heltable([8,16,128], n3(:,185), t3x128(:,:,24))
  call heltable([8,16,128], n3(:,186), t3x128(:,:,25))
  call heltable([8,16,128], n3(:,187), t3x128(:,:,26))
  call heltable([8,16,128], n3(:,188), t3x128(:,:,27))
  call heltable([8,16,128], n3(:,189), t3x128(:,:,28))
  call heltable([16,8,128], n3(:,190), t3x128(:,:,29))
  call heltable([16,8,128], n3(:,191), t3x128(:,:,30))
  call heltable([8,16,128], n3(:,192), t3x128(:,:,31))
  call heltable([16,8,128], n3(:,193), t3x128(:,:,32))
  call heltable([16,8,128], n3(:,194), t3x128(:,:,33))
  call heltable([16,8,128], n3(:,195), t3x128(:,:,34))
  call heltable([8,16,128], n3(:,196), t3x128(:,:,35))
  call heltable([16,8,128], n3(:,197), t3x128(:,:,36))
  call heltable([8,16,128], n3(:,198), t3x128(:,:,37))
  call heltable([8,16,128], n3(:,199), t3x128(:,:,38))
  call heltable([8,16,128], n3(:,200), t3x128(:,:,39))
  call heltable([8,16,128], n3(:,201), t3x128(:,:,40))
  call heltable([8,16,128], n3(:,202), t3x128(:,:,41))
  call heltable([8,16,128], n3(:,203), t3x128(:,:,42))
  call heltable([16,8,128], n3(:,204), t3x128(:,:,43))
  call heltable([16,8,128], n3(:,205), t3x128(:,:,44))
  call heltable([16,8,128], n3(:,206), t3x128(:,:,45))
  call heltable([8,16,128], n3(:,207), t3x128(:,:,46))
  call heltable([8,16,128], n3(:,208), t3x128(:,:,47))
  call heltable([16,8,128], n3(:,209), t3x128(:,:,48))
  call heltable([16,8,128], n3(:,210), t3x128(:,:,49))
  call heltable([8,16,128], n3(:,211), t3x128(:,:,50))
  call heltable([16,8,128], n3(:,212), t3x128(:,:,51))
  call heltable([8,16,128], n3(:,213), t3x128(:,:,52))
  call heltable([16,8,128], n3(:,214), t3x128(:,:,53))
  call heltable([8,16,128], n3(:,215), t3x128(:,:,54))
  call heltable([8,16,128], n3(:,216), t3x128(:,:,55))
  call heltable([8,16,128], n3(:,217), t3x128(:,:,56))
  call heltable([16,8,128], n3(:,218), t3x128(:,:,57))
  call heltable([8,16,128], n3(:,219), t3x128(:,:,58))
  call heltable([8,16,128], n3(:,220), t3x128(:,:,59))
  call heltable([8,16,128], n3(:,221), t3x128(:,:,60))
  call heltable([16,8,128], n3(:,222), t3x128(:,:,61))
  call heltable([16,8,128], n3(:,223), t3x128(:,:,62))
  call heltable([8,16,128], n3(:,224), t3x128(:,:,63))
  call heltable([16,8,128], n3(:,225), t3x128(:,:,64))
  call heltable([16,8,128], n3(:,226), t3x128(:,:,65))
  call heltable([8,16,128], n3(:,227), t3x128(:,:,66))
  call heltable([8,16,128], n3(:,228), t3x128(:,:,67))
  call heltable([16,8,128], n3(:,229), t3x128(:,:,68))
  call heltable([16,8,128], n3(:,230), t3x128(:,:,69))
  call heltable([8,16,128], n3(:,231), t3x128(:,:,70))
  call heltable([8,16,128], n3(:,232), t3x128(:,:,71))
  call heltable([8,16,128], n3(:,233), t3x128(:,:,72))
  call heltable([8,16,128], n3(:,234), t3x128(:,:,73))
  call heltable([8,16,128], n3(:,235), t3x128(:,:,74))
  call heltable([8,16,128], n3(:,236), t3x128(:,:,75))
  call heltable([16,8,128], n3(:,237), t3x128(:,:,76))
  call heltable([16,8,128], n3(:,238), t3x128(:,:,77))
  call heltable([8,16,128], n3(:,239), t3x128(:,:,78))
  call heltable([16,8,128], n3(:,240), t3x128(:,:,79))
  call heltable([16,8,128], n3(:,241), t3x128(:,:,80))
  call heltable([8,16,128], n3(:,242), t3x128(:,:,81))
  call heltable([8,16,128], n3(:,243), t3x128(:,:,82))
  call heltable([16,8,128], n3(:,244), t3x128(:,:,83))
  call heltable([16,8,128], n3(:,245), t3x128(:,:,84))
  call heltable([8,16,128], n3(:,246), t3x128(:,:,85))
  call heltable([8,16,128], n3(:,247), t3x128(:,:,86))
  call heltable([8,16,128], n3(:,248), t3x128(:,:,87))
  call heltable([8,16,128], n3(:,249), t3x128(:,:,88))
  call heltable([8,16,128], n3(:,250), t3x128(:,:,89))
  call heltable([8,16,128], n3(:,251), t3x128(:,:,90))
  call heltable([16,8,128], n3(:,252), t3x128(:,:,91))
  call heltable([16,8,128], n3(:,253), t3x128(:,:,92))
  call heltable([16,8,128], n3(:,254), t3x128(:,:,93))
  call heltable([8,16,128], n3(:,255), t3x128(:,:,94))
  call heltable([8,16,128], n3(:,256), t3x128(:,:,95))
  call heltable([16,8,128], n3(:,257), t3x128(:,:,96))
  call heltable([16,8,128], n3(:,258), t3x128(:,:,97))
  call heltable([8,16,128], n3(:,259), t3x128(:,:,98))
  call heltable([16,8,128], n3(:,260), t3x128(:,:,99))
  call heltable([8,16,128], n3(:,261), t3x128(:,:,100))
  call heltable([16,8,128], n3(:,262), t3x128(:,:,101))
  call heltable([8,16,128], n3(:,263), t3x128(:,:,102))
  call heltable([8,16,128], n3(:,264), t3x128(:,:,103))
  call heltable([8,16,128], n3(:,265), t3x128(:,:,104))
  call heltable([16,8,128], n3(:,266), t3x128(:,:,105))
  call heltable([8,16,128], n3(:,267), t3x128(:,:,106))
  call heltable([8,16,128], n3(:,268), t3x128(:,:,107))
  call heltable([8,16,128], n3(:,269), t3x128(:,:,108))
  call heltable([16,8,128], n3(:,270), t3x128(:,:,109))
  call heltable([16,8,128], n3(:,271), t3x128(:,:,110))
  call heltable([16,8,128], n3(:,272), t3x128(:,:,111))
  call heltable([8,16,128], n3(:,273), t3x128(:,:,112))
  call heltable([8,16,128], n3(:,274), t3x128(:,:,113))
  call heltable([16,8,128], n3(:,275), t3x128(:,:,114))
  call heltable([8,16,128], n3(:,276), t3x128(:,:,115))
  call heltable([16,8,128], n3(:,277), t3x128(:,:,116))
  call heltable([8,16,128], n3(:,278), t3x128(:,:,117))
  call heltable([8,16,128], n3(:,279), t3x128(:,:,118))
  call heltable([16,8,128], n3(:,280), t3x128(:,:,119))
  call heltable([8,16,128], n3(:,281), t3x128(:,:,120))
  call heltable([8,16,128], n3(:,282), t3x128(:,:,121))
  call heltable([16,8,128], n3(:,283), t3x128(:,:,122))
  call heltable([8,16,128], n3(:,284), t3x128(:,:,123))
  call heltable([8,16,128], n3(:,285), t3x128(:,:,124))
  call heltable([16,8,128], n3(:,286), t3x128(:,:,125))
  call heltable([8,16,128], n3(:,287), t3x128(:,:,126))
  call heltable([8,16,128], n3(:,288), t3x128(:,:,127))
  call heltable([8,16,128], n3(:,289), t3x128(:,:,128))
  call heltable([16,8,128], n3(:,290), t3x128(:,:,129))
  call heltable([16,8,128], n3(:,291), t3x128(:,:,130))
  call heltable([16,8,128], n3(:,292), t3x128(:,:,131))
  call heltable([8,16,128], n3(:,293), t3x128(:,:,132))
  call heltable([8,16,128], n3(:,294), t3x128(:,:,133))
  call heltable([16,8,128], n3(:,295), t3x128(:,:,134))
  call heltable([8,16,128], n3(:,296), t3x128(:,:,135))
  call heltable([16,8,128], n3(:,297), t3x128(:,:,136))
  call heltable([8,16,128], n3(:,298), t3x128(:,:,137))
  call heltable([8,16,128], n3(:,299), t3x128(:,:,138))
  call heltable([16,8,128], n3(:,300), t3x128(:,:,139))
  call heltable([8,16,128], n3(:,301), t3x128(:,:,140))
  call heltable([8,16,128], n3(:,302), t3x128(:,:,141))
  call heltable([16,8,128], n3(:,303), t3x128(:,:,142))
  call heltable([8,16,128], n3(:,304), t3x128(:,:,143))
  call heltable([8,16,128], n3(:,305), t3x128(:,:,144))
  call heltable([16,8,128], n3(:,306), t3x128(:,:,145))
  call heltable([8,16,128], n3(:,307), t3x128(:,:,146))
  call heltable([8,16,128], n3(:,308), t3x128(:,:,147))
  call heltable([8,16,128], n3(:,309), t3x128(:,:,148))
  call heltable([8,16,128], n3(:,310), t3x128(:,:,149))
  call heltable([16,8,128], n3(:,311), t3x128(:,:,150))
  call heltable([8,16,128], n3(:,312), t3x128(:,:,151))
  call heltable([8,16,128], n3(:,313), t3x128(:,:,152))
  call heltable([8,16,128], n3(:,314), t3x128(:,:,153))
  call heltable([8,16,128], n3(:,315), t3x128(:,:,154))
  call heltable([16,8,128], n3(:,316), t3x128(:,:,155))
  call heltable([8,16,128], n3(:,317), t3x128(:,:,156))
  call heltable([8,16,128], n3(:,318), t3x128(:,:,157))
  call heltable([8,16,128], n3(:,319), t3x128(:,:,158))
  call heltable([16,8,128], n3(:,320), t3x128(:,:,159))
  call heltable([16,8,128], n3(:,321), t3x128(:,:,160))
  call heltable([8,16,128], n3(:,322), t3x128(:,:,161))
  call heltable([8,16,128], n3(:,323), t3x128(:,:,162))
  call heltable([8,16,128], n3(:,324), t3x128(:,:,163))
  call heltable([8,16,128], n3(:,325), t3x128(:,:,164))
  call heltable([8,16,128], n3(:,326), t3x128(:,:,165))
  call heltable([8,16,128], n3(:,327), t3x128(:,:,166))
  call heltable([8,16,128], n3(:,328), t3x128(:,:,167))
  call heltable([8,16,128], n3(:,329), t3x128(:,:,168))
  call heltable([8,16,128], n3(:,330), t3x128(:,:,169))
  call heltable([16,8,128], n3(:,331), t3x128(:,:,170))
  call heltable([8,16,128], n3(:,332), t3x128(:,:,171))
  call heltable([8,16,128], n3(:,333), t3x128(:,:,172))
  call heltable([8,16,128], n3(:,334), t3x128(:,:,173))
  call heltable([8,16,128], n3(:,335), t3x128(:,:,174))
  call heltable([16,8,128], n3(:,336), t3x128(:,:,175))
  call heltable([8,16,128], n3(:,337), t3x128(:,:,176))
  call heltable([8,16,128], n3(:,338), t3x128(:,:,177))
  call heltable([8,16,128], n3(:,339), t3x128(:,:,178))
  call heltable([16,8,128], n3(:,340), t3x128(:,:,179))
  call heltable([16,8,128], n3(:,341), t3x128(:,:,180))
  call heltable([8,16,128], n3(:,342), t3x128(:,:,181))
  call heltable([8,16,128], n3(:,343), t3x128(:,:,182))
  call heltable([8,16,128], n3(:,344), t3x128(:,:,183))
  call heltable([8,16,128], n3(:,345), t3x128(:,:,184))
  call heltable([8,16,128], n3(:,346), t3x128(:,:,185))
  call heltable([8,16,128], n3(:,347), t3x128(:,:,186))
  call heltable([8,16,128], n3(:,348), t3x128(:,:,187))
  call heltable([8,16,128], n3(:,349), t3x128(:,:,188))
  call heltable([8,16,128], n3(:,350), t3x128(:,:,189))
  call heltable([8,16,128], n3(:,351), t3x128(:,:,190))
  call heltable([8,16,128], n3(:,352), t3x128(:,:,191))
  call heltable([8,16,128], n3(:,353), t3x128(:,:,192))
  call heltable([8,16,128], n3(:,354), t3x128(:,:,193))
  call heltable([8,16,128], n3(:,355), t3x128(:,:,194))
  call heltable([8,16,128], n3(:,356), t3x128(:,:,195))
  call heltable([8,16,128], n3(:,357), t3x128(:,:,196))
  call heltable([8,16,128], n3(:,358), t3x128(:,:,197))
  call heltable([8,16,128], n3(:,359), t3x128(:,:,198))
  call heltable([8,16,128], n3(:,360), t3x128(:,:,199))
  call heltable([8,16,128], n3(:,361), t3x128(:,:,200))
  call heltable([8,16,128], n3(:,362), t3x128(:,:,201))
  call heltable([8,16,128], n3(:,363), t3x128(:,:,202))
  call heltable([8,16,128], n3(:,364), t3x128(:,:,203))
  call heltable([8,16,128], n3(:,365), t3x128(:,:,204))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppajjj_ddxssxagg_1
