
module ol_external_pphttj_ttxbbbxbxh_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_pphttj_ttxbbbxbxh_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_pphttj_ttxbbbxbxh_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_pphttj_ttxbbbxbxh_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_pphttj_ttxbbbxbxh_1(7) = &
                     [ 1, 2, 3, 3, 4, 4, 5 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_pphttj_ttxbbbxbxh_1(7) = &
                     [ 6, 6, 6, 6, 6, 6, 1 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_pphttj_ttxbbbxbxh_1 = &
                     144
  integer, save :: channel_number_pphttj_ttxbbbxbxh_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,64) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(64,7)
  integer, save :: POLSEL(7) = 0

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_pphttj_ttxbbbxbxh_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_pphttj_ttxbbbxbxh_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_pphttj_ttxbbbxbxh_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_pphttj_ttxbbbxbxh_1(7)
    external_perm_pphttj_ttxbbbxbxh_1 = perm
    do i = 1, 7
      external_perm_inv_pphttj_ttxbbbxbxh_1( &
        external_perm_pphttj_ttxbbbxbxh_1(i)) = i
      particle_types_perm_pphttj_ttxbbbxbxh_1(i) = &
        particle_types_pphttj_ttxbbbxbxh_1( &
        external_perm_pphttj_ttxbbbxbxh_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_pphttj_ttxbbbxbxh_1(i) >= &
          external_perm_pphttj_ttxbbbxbxh_1(j)) then
          ii = external_perm_pphttj_ttxbbbxbxh_1(i)
          jj = external_perm_pphttj_ttxbbbxbxh_1(j)
        else
          ii = external_perm_pphttj_ttxbbbxbxh_1(j)
          jj = external_perm_pphttj_ttxbbbxbxh_1(i)
        end if
        extcomb_perm_pphttj_ttxbbbxbxh_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_pphttj_ttxbbbxbxh_1 = &
      average_factors_pphttj_ttxbbbxbxh_1( &
      external_perm_pphttj_ttxbbbxbxh_1(1)) &
      * average_factors_pphttj_ttxbbbxbxh_1( &
      external_perm_pphttj_ttxbbbxbxh_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_pphttj_ttxbbbxbxh_1 = &
          average_factor_pphttj_ttxbbbxbxh_1 &
          * factorial(count(particle_types_perm_pphttj_ttxbbbxbxh_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_pphttj_ttxbbbxbxh_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_pphttj_ttxbbbxbxh_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rMT_unscaled, rMT_unscaled, rMB_unscaled, rMB_unscaled, rMB_unscaled, rMB_unscaled, rMH_unscaled ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_pphttj_ttxbbbxbxh_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_pphttj_ttxbbbxbxh_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_pphttj_ttxbbbxbxh_1")
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
      & bind(c,name="ol_rambo_pphttj_ttxbbbxbxh_1")
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
  H(:, 1) = [ -1, -1, -1, -1, -1, -1,  0 ]
  H(:, 2) = [ -1, -1, -1, -1, -1,  1,  0 ]
  H(:, 3) = [ -1, -1, -1, -1,  1, -1,  0 ]
  H(:, 4) = [ -1, -1, -1, -1,  1,  1,  0 ]
  H(:, 5) = [ -1, -1, -1,  1, -1, -1,  0 ]
  H(:, 6) = [ -1, -1, -1,  1, -1,  1,  0 ]
  H(:, 7) = [ -1, -1, -1,  1,  1, -1,  0 ]
  H(:, 8) = [ -1, -1, -1,  1,  1,  1,  0 ]
  H(:, 9) = [ -1, -1,  1, -1, -1, -1,  0 ]
  H(:,10) = [ -1, -1,  1, -1, -1,  1,  0 ]
  H(:,11) = [ -1, -1,  1, -1,  1, -1,  0 ]
  H(:,12) = [ -1, -1,  1, -1,  1,  1,  0 ]
  H(:,13) = [ -1, -1,  1,  1, -1, -1,  0 ]
  H(:,14) = [ -1, -1,  1,  1, -1,  1,  0 ]
  H(:,15) = [ -1, -1,  1,  1,  1, -1,  0 ]
  H(:,16) = [ -1, -1,  1,  1,  1,  1,  0 ]
  H(:,17) = [ -1,  1, -1, -1, -1, -1,  0 ]
  H(:,18) = [ -1,  1, -1, -1, -1,  1,  0 ]
  H(:,19) = [ -1,  1, -1, -1,  1, -1,  0 ]
  H(:,20) = [ -1,  1, -1, -1,  1,  1,  0 ]
  H(:,21) = [ -1,  1, -1,  1, -1, -1,  0 ]
  H(:,22) = [ -1,  1, -1,  1, -1,  1,  0 ]
  H(:,23) = [ -1,  1, -1,  1,  1, -1,  0 ]
  H(:,24) = [ -1,  1, -1,  1,  1,  1,  0 ]
  H(:,25) = [ -1,  1,  1, -1, -1, -1,  0 ]
  H(:,26) = [ -1,  1,  1, -1, -1,  1,  0 ]
  H(:,27) = [ -1,  1,  1, -1,  1, -1,  0 ]
  H(:,28) = [ -1,  1,  1, -1,  1,  1,  0 ]
  H(:,29) = [ -1,  1,  1,  1, -1, -1,  0 ]
  H(:,30) = [ -1,  1,  1,  1, -1,  1,  0 ]
  H(:,31) = [ -1,  1,  1,  1,  1, -1,  0 ]
  H(:,32) = [ -1,  1,  1,  1,  1,  1,  0 ]
  H(:,33) = [  1, -1, -1, -1, -1, -1,  0 ]
  H(:,34) = [  1, -1, -1, -1, -1,  1,  0 ]
  H(:,35) = [  1, -1, -1, -1,  1, -1,  0 ]
  H(:,36) = [  1, -1, -1, -1,  1,  1,  0 ]
  H(:,37) = [  1, -1, -1,  1, -1, -1,  0 ]
  H(:,38) = [  1, -1, -1,  1, -1,  1,  0 ]
  H(:,39) = [  1, -1, -1,  1,  1, -1,  0 ]
  H(:,40) = [  1, -1, -1,  1,  1,  1,  0 ]
  H(:,41) = [  1, -1,  1, -1, -1, -1,  0 ]
  H(:,42) = [  1, -1,  1, -1, -1,  1,  0 ]
  H(:,43) = [  1, -1,  1, -1,  1, -1,  0 ]
  H(:,44) = [  1, -1,  1, -1,  1,  1,  0 ]
  H(:,45) = [  1, -1,  1,  1, -1, -1,  0 ]
  H(:,46) = [  1, -1,  1,  1, -1,  1,  0 ]
  H(:,47) = [  1, -1,  1,  1,  1, -1,  0 ]
  H(:,48) = [  1, -1,  1,  1,  1,  1,  0 ]
  H(:,49) = [  1,  1, -1, -1, -1, -1,  0 ]
  H(:,50) = [  1,  1, -1, -1, -1,  1,  0 ]
  H(:,51) = [  1,  1, -1, -1,  1, -1,  0 ]
  H(:,52) = [  1,  1, -1, -1,  1,  1,  0 ]
  H(:,53) = [  1,  1, -1,  1, -1, -1,  0 ]
  H(:,54) = [  1,  1, -1,  1, -1,  1,  0 ]
  H(:,55) = [  1,  1, -1,  1,  1, -1,  0 ]
  H(:,56) = [  1,  1, -1,  1,  1,  1,  0 ]
  H(:,57) = [  1,  1,  1, -1, -1, -1,  0 ]
  H(:,58) = [  1,  1,  1, -1, -1,  1,  0 ]
  H(:,59) = [  1,  1,  1, -1,  1, -1,  0 ]
  H(:,60) = [  1,  1,  1, -1,  1,  1,  0 ]
  H(:,61) = [  1,  1,  1,  1, -1, -1,  0 ]
  H(:,62) = [  1,  1,  1,  1, -1,  1,  0 ]
  H(:,63) = [  1,  1,  1,  1,  1, -1,  0 ]
  H(:,64) = [  1,  1,  1,  1,  1,  1,  0 ]

  end subroutine hel_init


  subroutine pol_init(pol) &
      & bind(c,name="ol_f_pol_init_pphttj_ttxbbbxbxh_1")
    implicit none
    integer, intent(in) :: pol(7)
    POLSEL = pol
  end subroutine pol_init

end module ol_external_pphttj_ttxbbbxbxh_1


module colour_basis_pphttj_ttxbbbxbxh_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,1,1,0]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_pphttj_ttxbbbxbxh_1")
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
    nhel = 64
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_pphttj_ttxbbbxbxh_1")
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

end module colour_basis_pphttj_ttxbbbxbxh_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_pphttj_ttxbbbxbxh_1(perm)
  use ol_external_pphttj_ttxbbbxbxh_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_pphttj_ttxbbbxbxh_1

! **********************************************************************
module ol_heltables_pphttj_ttxbbbxbxh_1
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
    H7(1) = [0]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(26), n3(3,203)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x2(2,2,6), t3x4(2,4,15), t3x16(2,16,20), t3x8(2,8,66), t3x64(2,64,96)

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
  call heltable([2,1,2], n3(:,3), t3x2(:,:,1))
  n2(1) = 2
  call heltable([2,4,8], n3(:,4), t3x8(:,:,1))
  call heltable([4,2,8], n3(:,5), t3x8(:,:,2))
  n2(2) = 8
  call heltable([2,4,8], n3(:,6), t3x8(:,:,3))
  call heltable([4,2,8], n3(:,7), t3x8(:,:,4))
  n2(3) = 8
  call heltable([2,2,4], n3(:,8), t3x4(:,:,3))
  call heltable([4,4,16], n3(:,9), t3x16(:,:,1))
  call heltable([1,2,2], n3(:,10), t3x2(:,:,2))
  n2(4) = 2
  call heltable([4,2,8], n3(:,11), t3x8(:,:,5))
  call heltable([2,4,8], n3(:,12), t3x8(:,:,6))
  n2(5) = 8
  call heltable([4,2,8], n3(:,13), t3x8(:,:,7))
  call heltable([2,4,8], n3(:,14), t3x8(:,:,8))
  n2(6) = 8
  call heltable([2,2,4], n3(:,15), t3x4(:,:,4))
  call heltable([8,1,8], n3(:,16), t3x8(:,:,9))
  call heltable([8,1,8], n3(:,17), t3x8(:,:,10))
  call heltable([2,1,2], n3(:,18), t3x2(:,:,3))
  call heltable([2,2,4], n3(:,19), t3x4(:,:,5))
  n2(7) = 2
  call heltable([4,2,8], n3(:,20), t3x8(:,:,11))
  call heltable([2,2,4], n3(:,21), t3x4(:,:,6))
  call heltable([4,4,16], n3(:,22), t3x16(:,:,2))
  call heltable([2,4,8], n3(:,23), t3x8(:,:,12))
  call heltable([4,2,8], n3(:,24), t3x8(:,:,13))
  n2(8) = 8
  call heltable([4,2,8], n3(:,25), t3x8(:,:,14))
  call heltable([2,4,8], n3(:,26), t3x8(:,:,15))
  n2(9) = 8
  call heltable([4,2,8], n3(:,27), t3x8(:,:,16))
  n2(10) = 8
  call heltable([2,2,4], n3(:,28), t3x4(:,:,7))
  call heltable([8,1,8], n3(:,29), t3x8(:,:,17))
  call heltable([8,1,8], n3(:,30), t3x8(:,:,18))
  call heltable([2,2,4], n3(:,31), t3x4(:,:,8))
  call heltable([2,4,8], n3(:,32), t3x8(:,:,19))
  call heltable([4,2,8], n3(:,33), t3x8(:,:,20))
  n2(11) = 8
  call heltable([2,4,8], n3(:,34), t3x8(:,:,21))
  n2(12) = 8
  call heltable([2,2,4], n3(:,35), t3x4(:,:,9))
  call heltable([4,4,16], n3(:,36), t3x16(:,:,3))
  call heltable([1,2,2], n3(:,37), t3x2(:,:,4))
  n2(13) = 2
  call heltable([2,4,8], n3(:,38), t3x8(:,:,22))
  call heltable([4,2,8], n3(:,39), t3x8(:,:,23))
  call heltable([2,4,8], n3(:,40), t3x8(:,:,24))
  n2(14) = 8
  call heltable([2,2,4], n3(:,41), t3x4(:,:,10))
  call heltable([8,1,8], n3(:,42), t3x8(:,:,25))
  call heltable([2,2,4], n3(:,43), t3x4(:,:,11))
  call heltable([4,2,8], n3(:,44), t3x8(:,:,26))
  call heltable([2,2,4], n3(:,45), t3x4(:,:,12))
  call heltable([4,4,16], n3(:,46), t3x16(:,:,4))
  call heltable([2,4,8], n3(:,47), t3x8(:,:,27))
  n2(15) = 8
  call heltable([2,4,8], n3(:,48), t3x8(:,:,28))
  call heltable([4,2,8], n3(:,49), t3x8(:,:,29))
  n2(16) = 8
  call heltable([2,2,4], n3(:,50), t3x4(:,:,13))
  call heltable([8,1,8], n3(:,51), t3x8(:,:,30))
  call heltable([8,2,16], n3(:,52), t3x16(:,:,5))
  call heltable([8,2,16], n3(:,53), t3x16(:,:,6))
  call heltable([2,8,16], n3(:,54), t3x16(:,:,7))
  call heltable([4,2,8], n3(:,55), t3x8(:,:,31))
  call heltable([8,2,16], n3(:,56), t3x16(:,:,8))
  call heltable([8,2,16], n3(:,57), t3x16(:,:,9))
  call heltable([2,8,16], n3(:,58), t3x16(:,:,10))
  call heltable([4,2,8], n3(:,59), t3x8(:,:,32))
  call heltable([2,4,8], n3(:,60), t3x8(:,:,33))
  call heltable([4,2,8], n3(:,61), t3x8(:,:,34))
  call heltable([4,2,8], n3(:,62), t3x8(:,:,35))
  call heltable([2,4,8], n3(:,63), t3x8(:,:,36))
  call heltable([4,2,8], n3(:,64), t3x8(:,:,37))
  call heltable([4,2,8], n3(:,65), t3x8(:,:,38))
  call heltable([2,1,2], n3(:,66), t3x2(:,:,5))
  n2(17) = 2
  call heltable([2,2,4], n3(:,67), t3x4(:,:,14))
  call heltable([4,4,16], n3(:,68), t3x16(:,:,11))
  call heltable([2,4,8], n3(:,69), t3x8(:,:,39))
  call heltable([4,2,8], n3(:,70), t3x8(:,:,40))
  n2(18) = 8
  call heltable([2,4,8], n3(:,71), t3x8(:,:,41))
  call heltable([4,2,8], n3(:,72), t3x8(:,:,42))
  n2(19) = 8
  call heltable([1,2,2], n3(:,73), t3x2(:,:,6))
  n2(20) = 2
  call heltable([2,2,4], n3(:,74), t3x4(:,:,15))
  call heltable([4,2,8], n3(:,75), t3x8(:,:,43))
  call heltable([2,4,8], n3(:,76), t3x8(:,:,44))
  n2(21) = 8
  call heltable([4,2,8], n3(:,77), t3x8(:,:,45))
  call heltable([2,4,8], n3(:,78), t3x8(:,:,46))
  n2(22) = 8
  call heltable([8,1,8], n3(:,79), t3x8(:,:,47))
  call heltable([8,1,8], n3(:,80), t3x8(:,:,48))
  call heltable([4,2,8], n3(:,81), t3x8(:,:,49))
  call heltable([2,4,8], n3(:,82), t3x8(:,:,50))
  call heltable([4,2,8], n3(:,83), t3x8(:,:,51))
  call heltable([2,4,8], n3(:,84), t3x8(:,:,52))
  call heltable([8,2,16], n3(:,85), t3x16(:,:,12))
  call heltable([2,8,16], n3(:,86), t3x16(:,:,13))
  call heltable([4,4,16], n3(:,87), t3x16(:,:,14))
  call heltable([2,4,8], n3(:,88), t3x8(:,:,53))
  call heltable([4,2,8], n3(:,89), t3x8(:,:,54))
  n2(23) = 8
  call heltable([2,4,8], n3(:,90), t3x8(:,:,55))
  call heltable([4,2,8], n3(:,91), t3x8(:,:,56))
  n2(24) = 8
  call heltable([4,2,8], n3(:,92), t3x8(:,:,57))
  call heltable([2,4,8], n3(:,93), t3x8(:,:,58))
  n2(25) = 8
  call heltable([4,2,8], n3(:,94), t3x8(:,:,59))
  call heltable([2,4,8], n3(:,95), t3x8(:,:,60))
  n2(26) = 8
  call heltable([8,1,8], n3(:,96), t3x8(:,:,61))
  call heltable([8,1,8], n3(:,97), t3x8(:,:,62))
  call heltable([4,2,8], n3(:,98), t3x8(:,:,63))
  call heltable([4,2,8], n3(:,99), t3x8(:,:,64))
  call heltable([8,2,16], n3(:,100), t3x16(:,:,15))
  call heltable([2,8,16], n3(:,101), t3x16(:,:,16))
  call heltable([2,4,8], n3(:,102), t3x8(:,:,65))
  call heltable([2,4,8], n3(:,103), t3x8(:,:,66))
  call heltable([8,2,16], n3(:,104), t3x16(:,:,17))
  call heltable([2,8,16], n3(:,105), t3x16(:,:,18))
  call heltable([8,2,16], n3(:,106), t3x16(:,:,19))
  call heltable([2,8,16], n3(:,107), t3x16(:,:,20))
  call heltable([8,8,64], n3(:,108), t3x64(:,:,1))
  call heltable([8,8,64], n3(:,109), t3x64(:,:,2))
  call heltable([4,16,64], n3(:,110), t3x64(:,:,3))
  call heltable([8,8,64], n3(:,111), t3x64(:,:,4))
  call heltable([8,8,64], n3(:,112), t3x64(:,:,5))
  call heltable([16,4,64], n3(:,113), t3x64(:,:,6))
  call heltable([8,8,64], n3(:,114), t3x64(:,:,7))
  call heltable([8,8,64], n3(:,115), t3x64(:,:,8))
  call heltable([8,8,64], n3(:,116), t3x64(:,:,9))
  call heltable([4,16,64], n3(:,117), t3x64(:,:,10))
  call heltable([8,8,64], n3(:,118), t3x64(:,:,11))
  call heltable([8,8,64], n3(:,119), t3x64(:,:,12))
  call heltable([8,8,64], n3(:,120), t3x64(:,:,13))
  call heltable([16,4,64], n3(:,121), t3x64(:,:,14))
  call heltable([8,8,64], n3(:,122), t3x64(:,:,15))
  call heltable([8,8,64], n3(:,123), t3x64(:,:,16))
  call heltable([8,8,64], n3(:,124), t3x64(:,:,17))
  call heltable([8,8,64], n3(:,125), t3x64(:,:,18))
  call heltable([4,16,64], n3(:,126), t3x64(:,:,19))
  call heltable([8,8,64], n3(:,127), t3x64(:,:,20))
  call heltable([8,8,64], n3(:,128), t3x64(:,:,21))
  call heltable([16,4,64], n3(:,129), t3x64(:,:,22))
  call heltable([8,8,64], n3(:,130), t3x64(:,:,23))
  call heltable([8,8,64], n3(:,131), t3x64(:,:,24))
  call heltable([8,8,64], n3(:,132), t3x64(:,:,25))
  call heltable([4,16,64], n3(:,133), t3x64(:,:,26))
  call heltable([8,8,64], n3(:,134), t3x64(:,:,27))
  call heltable([8,8,64], n3(:,135), t3x64(:,:,28))
  call heltable([8,8,64], n3(:,136), t3x64(:,:,29))
  call heltable([16,4,64], n3(:,137), t3x64(:,:,30))
  call heltable([8,8,64], n3(:,138), t3x64(:,:,31))
  call heltable([8,8,64], n3(:,139), t3x64(:,:,32))
  call heltable([4,16,64], n3(:,140), t3x64(:,:,33))
  call heltable([4,16,64], n3(:,141), t3x64(:,:,34))
  call heltable([4,16,64], n3(:,142), t3x64(:,:,35))
  call heltable([8,8,64], n3(:,143), t3x64(:,:,36))
  call heltable([4,16,64], n3(:,144), t3x64(:,:,37))
  call heltable([4,16,64], n3(:,145), t3x64(:,:,38))
  call heltable([4,16,64], n3(:,146), t3x64(:,:,39))
  call heltable([8,8,64], n3(:,147), t3x64(:,:,40))
  call heltable([4,16,64], n3(:,148), t3x64(:,:,41))
  call heltable([8,8,64], n3(:,149), t3x64(:,:,42))
  call heltable([8,8,64], n3(:,150), t3x64(:,:,43))
  call heltable([8,8,64], n3(:,151), t3x64(:,:,44))
  call heltable([4,16,64], n3(:,152), t3x64(:,:,45))
  call heltable([8,8,64], n3(:,153), t3x64(:,:,46))
  call heltable([8,8,64], n3(:,154), t3x64(:,:,47))
  call heltable([8,8,64], n3(:,155), t3x64(:,:,48))
  call heltable([4,16,64], n3(:,156), t3x64(:,:,49))
  call heltable([8,8,64], n3(:,157), t3x64(:,:,50))
  call heltable([8,8,64], n3(:,158), t3x64(:,:,51))
  call heltable([16,4,64], n3(:,159), t3x64(:,:,52))
  call heltable([8,8,64], n3(:,160), t3x64(:,:,53))
  call heltable([8,8,64], n3(:,161), t3x64(:,:,54))
  call heltable([8,8,64], n3(:,162), t3x64(:,:,55))
  call heltable([8,8,64], n3(:,163), t3x64(:,:,56))
  call heltable([8,8,64], n3(:,164), t3x64(:,:,57))
  call heltable([8,8,64], n3(:,165), t3x64(:,:,58))
  call heltable([8,8,64], n3(:,166), t3x64(:,:,59))
  call heltable([8,8,64], n3(:,167), t3x64(:,:,60))
  call heltable([4,16,64], n3(:,168), t3x64(:,:,61))
  call heltable([4,16,64], n3(:,169), t3x64(:,:,62))
  call heltable([4,16,64], n3(:,170), t3x64(:,:,63))
  call heltable([4,16,64], n3(:,171), t3x64(:,:,64))
  call heltable([4,16,64], n3(:,172), t3x64(:,:,65))
  call heltable([8,8,64], n3(:,173), t3x64(:,:,66))
  call heltable([8,8,64], n3(:,174), t3x64(:,:,67))
  call heltable([4,16,64], n3(:,175), t3x64(:,:,68))
  call heltable([8,8,64], n3(:,176), t3x64(:,:,69))
  call heltable([8,8,64], n3(:,177), t3x64(:,:,70))
  call heltable([8,8,64], n3(:,178), t3x64(:,:,71))
  call heltable([8,8,64], n3(:,179), t3x64(:,:,72))
  call heltable([8,8,64], n3(:,180), t3x64(:,:,73))
  call heltable([8,8,64], n3(:,181), t3x64(:,:,74))
  call heltable([8,8,64], n3(:,182), t3x64(:,:,75))
  call heltable([8,8,64], n3(:,183), t3x64(:,:,76))
  call heltable([4,16,64], n3(:,184), t3x64(:,:,77))
  call heltable([4,16,64], n3(:,185), t3x64(:,:,78))
  call heltable([4,16,64], n3(:,186), t3x64(:,:,79))
  call heltable([4,16,64], n3(:,187), t3x64(:,:,80))
  call heltable([8,8,64], n3(:,188), t3x64(:,:,81))
  call heltable([8,8,64], n3(:,189), t3x64(:,:,82))
  call heltable([8,8,64], n3(:,190), t3x64(:,:,83))
  call heltable([8,8,64], n3(:,191), t3x64(:,:,84))
  call heltable([4,16,64], n3(:,192), t3x64(:,:,85))
  call heltable([4,16,64], n3(:,193), t3x64(:,:,86))
  call heltable([4,16,64], n3(:,194), t3x64(:,:,87))
  call heltable([4,16,64], n3(:,195), t3x64(:,:,88))
  call heltable([8,8,64], n3(:,196), t3x64(:,:,89))
  call heltable([8,8,64], n3(:,197), t3x64(:,:,90))
  call heltable([8,8,64], n3(:,198), t3x64(:,:,91))
  call heltable([8,8,64], n3(:,199), t3x64(:,:,92))
  call heltable([4,16,64], n3(:,200), t3x64(:,:,93))
  call heltable([4,16,64], n3(:,201), t3x64(:,:,94))
  call heltable([4,16,64], n3(:,202), t3x64(:,:,95))
  call heltable([4,16,64], n3(:,203), t3x64(:,:,96))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_pphttj_ttxbbbxbxh_1
