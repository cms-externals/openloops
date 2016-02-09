
module ol_external_pphzjj_bbbxbxhzg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_pphzjj_bbbxbxhzg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_pphzjj_bbbxbxhzg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_pphzjj_bbbxbxhzg_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_pphzjj_bbbxbxhzg_1(7) = &
                     [ 1, 1, 2, 2, 3, 4, 5 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_pphzjj_bbbxbxhzg_1(7) = &
                     [ 6, 6, 6, 6, 1, 3, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_pphzjj_bbbxbxhzg_1 = &
                     72
  integer, save :: channel_number_pphzjj_bbbxbxhzg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,96) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(96,7)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_pphzjj_bbbxbxhzg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_pphzjj_bbbxbxhzg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_pphzjj_bbbxbxhzg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_pphzjj_bbbxbxhzg_1(7)
    external_perm_pphzjj_bbbxbxhzg_1 = perm
    do i = 1, 7
      external_perm_inv_pphzjj_bbbxbxhzg_1( &
        external_perm_pphzjj_bbbxbxhzg_1(i)) = i
      particle_types_perm_pphzjj_bbbxbxhzg_1(i) = &
        particle_types_pphzjj_bbbxbxhzg_1( &
        external_perm_pphzjj_bbbxbxhzg_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_pphzjj_bbbxbxhzg_1(i) >= &
          external_perm_pphzjj_bbbxbxhzg_1(j)) then
          ii = external_perm_pphzjj_bbbxbxhzg_1(i)
          jj = external_perm_pphzjj_bbbxbxhzg_1(j)
        else
          ii = external_perm_pphzjj_bbbxbxhzg_1(j)
          jj = external_perm_pphzjj_bbbxbxhzg_1(i)
        end if
        extcomb_perm_pphzjj_bbbxbxhzg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_pphzjj_bbbxbxhzg_1 = &
      average_factors_pphzjj_bbbxbxhzg_1( &
      external_perm_pphzjj_bbbxbxhzg_1(1)) &
      * average_factors_pphzjj_bbbxbxhzg_1( &
      external_perm_pphzjj_bbbxbxhzg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_pphzjj_bbbxbxhzg_1 = &
          average_factor_pphzjj_bbbxbxhzg_1 &
          * factorial(count(particle_types_perm_pphzjj_bbbxbxhzg_1(3:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_pphzjj_bbbxbxhzg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_pphzjj_bbbxbxhzg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rMB_unscaled, rMB_unscaled, rMB_unscaled, rMB_unscaled, rMH_unscaled, rMZ_unscaled, rZERO ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_pphzjj_bbbxbxhzg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_pphzjj_bbbxbxhzg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_pphzjj_bbbxbxhzg_1")
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
      & bind(c,name="ol_rambo_pphzjj_bbbxbxhzg_1")
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
  H(:, 3) = [ -1, -1, -1, -1,  0,  0, -1 ]
  H(:, 4) = [ -1, -1, -1, -1,  0,  0,  1 ]
  H(:, 5) = [ -1, -1, -1, -1,  0,  1, -1 ]
  H(:, 6) = [ -1, -1, -1, -1,  0,  1,  1 ]
  H(:, 7) = [ -1, -1, -1,  1,  0, -1, -1 ]
  H(:, 8) = [ -1, -1, -1,  1,  0, -1,  1 ]
  H(:, 9) = [ -1, -1, -1,  1,  0,  0, -1 ]
  H(:,10) = [ -1, -1, -1,  1,  0,  0,  1 ]
  H(:,11) = [ -1, -1, -1,  1,  0,  1, -1 ]
  H(:,12) = [ -1, -1, -1,  1,  0,  1,  1 ]
  H(:,13) = [ -1, -1,  1, -1,  0, -1, -1 ]
  H(:,14) = [ -1, -1,  1, -1,  0, -1,  1 ]
  H(:,15) = [ -1, -1,  1, -1,  0,  0, -1 ]
  H(:,16) = [ -1, -1,  1, -1,  0,  0,  1 ]
  H(:,17) = [ -1, -1,  1, -1,  0,  1, -1 ]
  H(:,18) = [ -1, -1,  1, -1,  0,  1,  1 ]
  H(:,19) = [ -1, -1,  1,  1,  0, -1, -1 ]
  H(:,20) = [ -1, -1,  1,  1,  0, -1,  1 ]
  H(:,21) = [ -1, -1,  1,  1,  0,  0, -1 ]
  H(:,22) = [ -1, -1,  1,  1,  0,  0,  1 ]
  H(:,23) = [ -1, -1,  1,  1,  0,  1, -1 ]
  H(:,24) = [ -1, -1,  1,  1,  0,  1,  1 ]
  H(:,25) = [ -1,  1, -1, -1,  0, -1, -1 ]
  H(:,26) = [ -1,  1, -1, -1,  0, -1,  1 ]
  H(:,27) = [ -1,  1, -1, -1,  0,  0, -1 ]
  H(:,28) = [ -1,  1, -1, -1,  0,  0,  1 ]
  H(:,29) = [ -1,  1, -1, -1,  0,  1, -1 ]
  H(:,30) = [ -1,  1, -1, -1,  0,  1,  1 ]
  H(:,31) = [ -1,  1, -1,  1,  0, -1, -1 ]
  H(:,32) = [ -1,  1, -1,  1,  0, -1,  1 ]
  H(:,33) = [ -1,  1, -1,  1,  0,  0, -1 ]
  H(:,34) = [ -1,  1, -1,  1,  0,  0,  1 ]
  H(:,35) = [ -1,  1, -1,  1,  0,  1, -1 ]
  H(:,36) = [ -1,  1, -1,  1,  0,  1,  1 ]
  H(:,37) = [ -1,  1,  1, -1,  0, -1, -1 ]
  H(:,38) = [ -1,  1,  1, -1,  0, -1,  1 ]
  H(:,39) = [ -1,  1,  1, -1,  0,  0, -1 ]
  H(:,40) = [ -1,  1,  1, -1,  0,  0,  1 ]
  H(:,41) = [ -1,  1,  1, -1,  0,  1, -1 ]
  H(:,42) = [ -1,  1,  1, -1,  0,  1,  1 ]
  H(:,43) = [ -1,  1,  1,  1,  0, -1, -1 ]
  H(:,44) = [ -1,  1,  1,  1,  0, -1,  1 ]
  H(:,45) = [ -1,  1,  1,  1,  0,  0, -1 ]
  H(:,46) = [ -1,  1,  1,  1,  0,  0,  1 ]
  H(:,47) = [ -1,  1,  1,  1,  0,  1, -1 ]
  H(:,48) = [ -1,  1,  1,  1,  0,  1,  1 ]
  H(:,49) = [  1, -1, -1, -1,  0, -1, -1 ]
  H(:,50) = [  1, -1, -1, -1,  0, -1,  1 ]
  H(:,51) = [  1, -1, -1, -1,  0,  0, -1 ]
  H(:,52) = [  1, -1, -1, -1,  0,  0,  1 ]
  H(:,53) = [  1, -1, -1, -1,  0,  1, -1 ]
  H(:,54) = [  1, -1, -1, -1,  0,  1,  1 ]
  H(:,55) = [  1, -1, -1,  1,  0, -1, -1 ]
  H(:,56) = [  1, -1, -1,  1,  0, -1,  1 ]
  H(:,57) = [  1, -1, -1,  1,  0,  0, -1 ]
  H(:,58) = [  1, -1, -1,  1,  0,  0,  1 ]
  H(:,59) = [  1, -1, -1,  1,  0,  1, -1 ]
  H(:,60) = [  1, -1, -1,  1,  0,  1,  1 ]
  H(:,61) = [  1, -1,  1, -1,  0, -1, -1 ]
  H(:,62) = [  1, -1,  1, -1,  0, -1,  1 ]
  H(:,63) = [  1, -1,  1, -1,  0,  0, -1 ]
  H(:,64) = [  1, -1,  1, -1,  0,  0,  1 ]
  H(:,65) = [  1, -1,  1, -1,  0,  1, -1 ]
  H(:,66) = [  1, -1,  1, -1,  0,  1,  1 ]
  H(:,67) = [  1, -1,  1,  1,  0, -1, -1 ]
  H(:,68) = [  1, -1,  1,  1,  0, -1,  1 ]
  H(:,69) = [  1, -1,  1,  1,  0,  0, -1 ]
  H(:,70) = [  1, -1,  1,  1,  0,  0,  1 ]
  H(:,71) = [  1, -1,  1,  1,  0,  1, -1 ]
  H(:,72) = [  1, -1,  1,  1,  0,  1,  1 ]
  H(:,73) = [  1,  1, -1, -1,  0, -1, -1 ]
  H(:,74) = [  1,  1, -1, -1,  0, -1,  1 ]
  H(:,75) = [  1,  1, -1, -1,  0,  0, -1 ]
  H(:,76) = [  1,  1, -1, -1,  0,  0,  1 ]
  H(:,77) = [  1,  1, -1, -1,  0,  1, -1 ]
  H(:,78) = [  1,  1, -1, -1,  0,  1,  1 ]
  H(:,79) = [  1,  1, -1,  1,  0, -1, -1 ]
  H(:,80) = [  1,  1, -1,  1,  0, -1,  1 ]
  H(:,81) = [  1,  1, -1,  1,  0,  0, -1 ]
  H(:,82) = [  1,  1, -1,  1,  0,  0,  1 ]
  H(:,83) = [  1,  1, -1,  1,  0,  1, -1 ]
  H(:,84) = [  1,  1, -1,  1,  0,  1,  1 ]
  H(:,85) = [  1,  1,  1, -1,  0, -1, -1 ]
  H(:,86) = [  1,  1,  1, -1,  0, -1,  1 ]
  H(:,87) = [  1,  1,  1, -1,  0,  0, -1 ]
  H(:,88) = [  1,  1,  1, -1,  0,  0,  1 ]
  H(:,89) = [  1,  1,  1, -1,  0,  1, -1 ]
  H(:,90) = [  1,  1,  1, -1,  0,  1,  1 ]
  H(:,91) = [  1,  1,  1,  1,  0, -1, -1 ]
  H(:,92) = [  1,  1,  1,  1,  0, -1,  1 ]
  H(:,93) = [  1,  1,  1,  1,  0,  0, -1 ]
  H(:,94) = [  1,  1,  1,  1,  0,  0,  1 ]
  H(:,95) = [  1,  1,  1,  1,  0,  1, -1 ]
  H(:,96) = [  1,  1,  1,  1,  0,  1,  1 ]

  H_HC(:,7) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 96/1/2) ]
  end subroutine hel_init

end module ol_external_pphzjj_bbbxbxhzg_1


module colour_basis_pphzjj_bbbxbxhzg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,0,0,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_pphzjj_bbbxbxhzg_1")
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
    nhel = 96
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_pphzjj_bbbxbxhzg_1")
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

end module colour_basis_pphzjj_bbbxbxhzg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_pphzjj_bbbxbxhzg_1(perm)
  use ol_external_pphzjj_bbbxbxhzg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_pphzjj_bbbxbxhzg_1

! **********************************************************************
module ol_heltables_pphzjj_bbbxbxhzg_1
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
    H6(3) = [-1,0,1], &
    H7(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(49), n3(3,571)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x3(2,3,1), t3x2(2,2,4), t3x6(2,6,16), t3x4(2,4,24), t3x16(2,16,32), t3x24(2,24,46), t3x12(2,12,54), &
    t3x8(2,8,66), t3x96(2,96,328)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(96,7)
  integer,           save :: exthel(96,7)
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
  call heltable([2,1,2], n3(:,2), t3x2(:,:,1))
  call heltable([2,3,6], n3(:,3), t3x6(:,:,1))
  n2(1) = 2
  n2(2) = 6
  call heltable([4,2,8], n3(:,4), t3x8(:,:,1))
  call heltable([2,6,12], n3(:,5), t3x12(:,:,1))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,2))
  call heltable([6,4,24], n3(:,7), t3x24(:,:,1))
  n2(3) = 4
  call heltable([6,2,12], n3(:,8), t3x12(:,:,2))
  call heltable([4,2,8], n3(:,9), t3x8(:,:,2))
  n2(4) = 12
  call heltable([2,2,4], n3(:,10), t3x4(:,:,3))
  n2(5) = 4
  call heltable([3,2,6], n3(:,11), t3x6(:,:,2))
  call heltable([4,4,16], n3(:,12), t3x16(:,:,1))
  n2(6) = 6
  call heltable([4,3,12], n3(:,13), t3x12(:,:,3))
  n2(7) = 12
  call heltable([2,4,8], n3(:,14), t3x8(:,:,3))
  n2(8) = 8
  call heltable([8,2,16], n3(:,15), t3x16(:,:,2))
  call heltable([8,3,24], n3(:,16), t3x24(:,:,2))
  call heltable([6,2,12], n3(:,17), t3x12(:,:,4))
  call heltable([3,2,6], n3(:,18), t3x6(:,:,3))
  call heltable([1,2,2], n3(:,19), t3x2(:,:,2))
  n2(9) = 6
  n2(10) = 2
  call heltable([6,2,12], n3(:,20), t3x12(:,:,5))
  call heltable([2,6,12], n3(:,21), t3x12(:,:,6))
  call heltable([2,4,8], n3(:,22), t3x8(:,:,4))
  n2(11) = 12
  call heltable([2,2,4], n3(:,23), t3x4(:,:,4))
  call heltable([4,6,24], n3(:,24), t3x24(:,:,3))
  n2(12) = 4
  call heltable([2,2,4], n3(:,25), t3x4(:,:,5))
  n2(13) = 4
  call heltable([3,4,12], n3(:,26), t3x12(:,:,7))
  n2(14) = 12
  call heltable([2,3,6], n3(:,27), t3x6(:,:,4))
  call heltable([4,4,16], n3(:,28), t3x16(:,:,3))
  n2(15) = 6
  call heltable([4,2,8], n3(:,29), t3x8(:,:,5))
  n2(16) = 8
  call heltable([3,8,24], n3(:,30), t3x24(:,:,4))
  call heltable([2,8,16], n3(:,31), t3x16(:,:,4))
  call heltable([2,6,12], n3(:,32), t3x12(:,:,8))
  call heltable([6,1,6], n3(:,33), t3x6(:,:,5))
  n2(17) = 6
  call heltable([1,4,4], n3(:,34), t3x4(:,:,6))
  n2(18) = 4
  call heltable([1,8,8], n3(:,35), t3x8(:,:,6))
  call heltable([6,2,12], n3(:,36), t3x12(:,:,9))
  call heltable([4,1,4], n3(:,37), t3x4(:,:,7))
  n2(19) = 4
  call heltable([1,6,6], n3(:,38), t3x6(:,:,6))
  n2(20) = 6
  call heltable([8,1,8], n3(:,39), t3x8(:,:,7))
  call heltable([2,6,12], n3(:,40), t3x12(:,:,10))
  call heltable([1,3,3], n3(:,41), t3x3(:,:,1))
  n2(21) = 3
  call heltable([3,4,12], n3(:,42), t3x12(:,:,11))
  call heltable([2,3,6], n3(:,43), t3x6(:,:,7))
  n2(22) = 6
  call heltable([4,3,12], n3(:,44), t3x12(:,:,12))
  call heltable([3,2,6], n3(:,45), t3x6(:,:,8))
  n2(23) = 6
  call heltable([2,6,12], n3(:,46), t3x12(:,:,13))
  call heltable([6,2,12], n3(:,47), t3x12(:,:,14))
  call heltable([2,6,12], n3(:,48), t3x12(:,:,15))
  call heltable([2,1,2], n3(:,49), t3x2(:,:,3))
  call heltable([2,2,4], n3(:,50), t3x4(:,:,8))
  n2(24) = 2
  call heltable([2,2,4], n3(:,51), t3x4(:,:,9))
  call heltable([6,4,24], n3(:,52), t3x24(:,:,5))
  n2(25) = 4
  call heltable([4,2,8], n3(:,53), t3x8(:,:,8))
  call heltable([2,6,12], n3(:,54), t3x12(:,:,16))
  call heltable([4,2,8], n3(:,55), t3x8(:,:,9))
  call heltable([3,2,6], n3(:,56), t3x6(:,:,9))
  call heltable([4,4,16], n3(:,57), t3x16(:,:,5))
  n2(26) = 6
  call heltable([2,4,8], n3(:,58), t3x8(:,:,10))
  n2(27) = 8
  call heltable([8,2,16], n3(:,59), t3x16(:,:,6))
  call heltable([6,2,12], n3(:,60), t3x12(:,:,17))
  call heltable([8,3,24], n3(:,61), t3x24(:,:,6))
  call heltable([3,2,6], n3(:,62), t3x6(:,:,10))
  n2(28) = 6
  call heltable([2,6,12], n3(:,63), t3x12(:,:,18))
  call heltable([2,4,8], n3(:,64), t3x8(:,:,11))
  n2(29) = 12
  call heltable([6,2,12], n3(:,65), t3x12(:,:,19))
  call heltable([4,6,24], n3(:,66), t3x24(:,:,7))
  call heltable([2,2,4], n3(:,67), t3x4(:,:,10))
  n2(30) = 4
  call heltable([3,4,12], n3(:,68), t3x12(:,:,20))
  n2(31) = 12
  call heltable([4,4,16], n3(:,69), t3x16(:,:,7))
  call heltable([4,2,8], n3(:,70), t3x8(:,:,12))
  n2(32) = 8
  call heltable([3,8,24], n3(:,71), t3x24(:,:,8))
  call heltable([2,8,16], n3(:,72), t3x16(:,:,8))
  call heltable([2,6,12], n3(:,73), t3x12(:,:,21))
  call heltable([6,1,6], n3(:,74), t3x6(:,:,11))
  n2(33) = 6
  call heltable([6,2,12], n3(:,75), t3x12(:,:,22))
  call heltable([1,8,8], n3(:,76), t3x8(:,:,13))
  call heltable([4,1,4], n3(:,77), t3x4(:,:,11))
  n2(34) = 4
  call heltable([8,1,8], n3(:,78), t3x8(:,:,14))
  call heltable([2,6,12], n3(:,79), t3x12(:,:,23))
  call heltable([3,4,12], n3(:,80), t3x12(:,:,24))
  call heltable([3,2,6], n3(:,81), t3x6(:,:,12))
  n2(35) = 6
  call heltable([6,2,12], n3(:,82), t3x12(:,:,25))
  call heltable([2,6,12], n3(:,83), t3x12(:,:,26))
  call heltable([2,6,12], n3(:,84), t3x12(:,:,27))
  call heltable([2,2,4], n3(:,85), t3x4(:,:,12))
  call heltable([2,3,6], n3(:,86), t3x6(:,:,13))
  n2(36) = 6
  call heltable([4,2,8], n3(:,87), t3x8(:,:,15))
  call heltable([2,6,12], n3(:,88), t3x12(:,:,28))
  call heltable([6,4,24], n3(:,89), t3x24(:,:,9))
  call heltable([6,2,12], n3(:,90), t3x12(:,:,29))
  call heltable([4,2,8], n3(:,91), t3x8(:,:,16))
  n2(37) = 12
  call heltable([2,2,4], n3(:,92), t3x4(:,:,13))
  n2(38) = 4
  call heltable([4,4,16], n3(:,93), t3x16(:,:,9))
  call heltable([4,3,12], n3(:,94), t3x12(:,:,30))
  n2(39) = 12
  call heltable([2,4,8], n3(:,95), t3x8(:,:,17))
  n2(40) = 8
  call heltable([8,2,16], n3(:,96), t3x16(:,:,10))
  call heltable([8,3,24], n3(:,97), t3x24(:,:,10))
  call heltable([6,2,12], n3(:,98), t3x12(:,:,31))
  call heltable([1,2,2], n3(:,99), t3x2(:,:,4))
  n2(41) = 2
  call heltable([6,2,12], n3(:,100), t3x12(:,:,32))
  call heltable([2,4,8], n3(:,101), t3x8(:,:,18))
  call heltable([2,2,4], n3(:,102), t3x4(:,:,14))
  call heltable([4,6,24], n3(:,103), t3x24(:,:,11))
  n2(42) = 4
  call heltable([2,3,6], n3(:,104), t3x6(:,:,14))
  call heltable([4,4,16], n3(:,105), t3x16(:,:,11))
  n2(43) = 6
  call heltable([4,2,8], n3(:,106), t3x8(:,:,19))
  n2(44) = 8
  call heltable([3,8,24], n3(:,107), t3x24(:,:,12))
  call heltable([2,8,16], n3(:,108), t3x16(:,:,12))
  call heltable([2,6,12], n3(:,109), t3x12(:,:,33))
  call heltable([1,4,4], n3(:,110), t3x4(:,:,15))
  n2(45) = 4
  call heltable([1,8,8], n3(:,111), t3x8(:,:,20))
  call heltable([6,2,12], n3(:,112), t3x12(:,:,34))
  call heltable([1,6,6], n3(:,113), t3x6(:,:,15))
  n2(46) = 6
  call heltable([8,1,8], n3(:,114), t3x8(:,:,21))
  call heltable([2,6,12], n3(:,115), t3x12(:,:,35))
  call heltable([2,3,6], n3(:,116), t3x6(:,:,16))
  n2(47) = 6
  call heltable([4,3,12], n3(:,117), t3x12(:,:,36))
  call heltable([6,2,12], n3(:,118), t3x12(:,:,37))
  call heltable([2,6,12], n3(:,119), t3x12(:,:,38))
  call heltable([2,2,4], n3(:,120), t3x4(:,:,16))
  call heltable([6,4,24], n3(:,121), t3x24(:,:,13))
  call heltable([4,2,8], n3(:,122), t3x8(:,:,22))
  call heltable([2,6,12], n3(:,123), t3x12(:,:,39))
  call heltable([4,2,8], n3(:,124), t3x8(:,:,23))
  call heltable([4,4,16], n3(:,125), t3x16(:,:,13))
  call heltable([2,4,8], n3(:,126), t3x8(:,:,24))
  n2(48) = 8
  call heltable([8,2,16], n3(:,127), t3x16(:,:,14))
  call heltable([6,2,12], n3(:,128), t3x12(:,:,40))
  call heltable([8,3,24], n3(:,129), t3x24(:,:,14))
  call heltable([2,4,8], n3(:,130), t3x8(:,:,25))
  call heltable([6,2,12], n3(:,131), t3x12(:,:,41))
  call heltable([4,6,24], n3(:,132), t3x24(:,:,15))
  call heltable([4,4,16], n3(:,133), t3x16(:,:,15))
  call heltable([4,2,8], n3(:,134), t3x8(:,:,26))
  n2(49) = 8
  call heltable([3,8,24], n3(:,135), t3x24(:,:,16))
  call heltable([2,8,16], n3(:,136), t3x16(:,:,16))
  call heltable([2,6,12], n3(:,137), t3x12(:,:,42))
  call heltable([6,2,12], n3(:,138), t3x12(:,:,43))
  call heltable([1,8,8], n3(:,139), t3x8(:,:,27))
  call heltable([8,1,8], n3(:,140), t3x8(:,:,28))
  call heltable([2,6,12], n3(:,141), t3x12(:,:,44))
  call heltable([6,2,12], n3(:,142), t3x12(:,:,45))
  call heltable([2,6,12], n3(:,143), t3x12(:,:,46))
  call heltable([2,2,4], n3(:,144), t3x4(:,:,17))
  call heltable([6,4,24], n3(:,145), t3x24(:,:,17))
  call heltable([6,2,12], n3(:,146), t3x12(:,:,47))
  call heltable([2,4,8], n3(:,147), t3x8(:,:,29))
  call heltable([2,2,4], n3(:,148), t3x4(:,:,18))
  call heltable([6,4,24], n3(:,149), t3x24(:,:,18))
  call heltable([6,2,12], n3(:,150), t3x12(:,:,48))
  call heltable([2,4,8], n3(:,151), t3x8(:,:,30))
  call heltable([4,2,8], n3(:,152), t3x8(:,:,31))
  call heltable([2,4,8], n3(:,153), t3x8(:,:,32))
  call heltable([12,2,24], n3(:,154), t3x24(:,:,19))
  call heltable([2,4,8], n3(:,155), t3x8(:,:,33))
  call heltable([2,12,24], n3(:,156), t3x24(:,:,20))
  call heltable([2,12,24], n3(:,157), t3x24(:,:,21))
  call heltable([4,6,24], n3(:,158), t3x24(:,:,22))
  call heltable([4,2,8], n3(:,159), t3x8(:,:,34))
  call heltable([2,6,12], n3(:,160), t3x12(:,:,49))
  call heltable([2,4,8], n3(:,161), t3x8(:,:,35))
  call heltable([12,2,24], n3(:,162), t3x24(:,:,23))
  call heltable([4,2,8], n3(:,163), t3x8(:,:,36))
  call heltable([2,12,24], n3(:,164), t3x24(:,:,24))
  call heltable([4,6,24], n3(:,165), t3x24(:,:,25))
  call heltable([4,2,8], n3(:,166), t3x8(:,:,37))
  call heltable([2,4,8], n3(:,167), t3x8(:,:,38))
  call heltable([2,6,12], n3(:,168), t3x12(:,:,50))
  call heltable([4,2,8], n3(:,169), t3x8(:,:,39))
  call heltable([12,2,24], n3(:,170), t3x24(:,:,26))
  call heltable([2,12,24], n3(:,171), t3x24(:,:,27))
  call heltable([2,8,16], n3(:,172), t3x16(:,:,17))
  call heltable([2,8,16], n3(:,173), t3x16(:,:,18))
  call heltable([2,8,16], n3(:,174), t3x16(:,:,19))
  call heltable([2,8,16], n3(:,175), t3x16(:,:,20))
  call heltable([6,2,12], n3(:,176), t3x12(:,:,51))
  call heltable([2,4,8], n3(:,177), t3x8(:,:,40))
  call heltable([2,2,4], n3(:,178), t3x4(:,:,19))
  call heltable([6,4,24], n3(:,179), t3x24(:,:,28))
  call heltable([6,2,12], n3(:,180), t3x12(:,:,52))
  call heltable([2,4,8], n3(:,181), t3x8(:,:,41))
  call heltable([2,2,4], n3(:,182), t3x4(:,:,20))
  call heltable([6,4,24], n3(:,183), t3x24(:,:,29))
  call heltable([12,2,24], n3(:,184), t3x24(:,:,30))
  call heltable([2,12,24], n3(:,185), t3x24(:,:,31))
  call heltable([4,2,8], n3(:,186), t3x8(:,:,42))
  call heltable([2,12,24], n3(:,187), t3x24(:,:,32))
  call heltable([2,4,8], n3(:,188), t3x8(:,:,43))
  call heltable([2,4,8], n3(:,189), t3x8(:,:,44))
  call heltable([4,2,8], n3(:,190), t3x8(:,:,45))
  call heltable([4,6,24], n3(:,191), t3x24(:,:,33))
  call heltable([2,6,12], n3(:,192), t3x12(:,:,53))
  call heltable([2,4,8], n3(:,193), t3x8(:,:,46))
  call heltable([2,12,24], n3(:,194), t3x24(:,:,34))
  call heltable([12,2,24], n3(:,195), t3x24(:,:,35))
  call heltable([4,2,8], n3(:,196), t3x8(:,:,47))
  call heltable([4,2,8], n3(:,197), t3x8(:,:,48))
  call heltable([4,6,24], n3(:,198), t3x24(:,:,36))
  call heltable([2,4,8], n3(:,199), t3x8(:,:,49))
  call heltable([2,6,12], n3(:,200), t3x12(:,:,54))
  call heltable([2,12,24], n3(:,201), t3x24(:,:,37))
  call heltable([4,2,8], n3(:,202), t3x8(:,:,50))
  call heltable([12,2,24], n3(:,203), t3x24(:,:,38))
  call heltable([2,8,16], n3(:,204), t3x16(:,:,21))
  call heltable([2,8,16], n3(:,205), t3x16(:,:,22))
  call heltable([2,8,16], n3(:,206), t3x16(:,:,23))
  call heltable([2,8,16], n3(:,207), t3x16(:,:,24))
  call heltable([4,2,8], n3(:,208), t3x8(:,:,51))
  call heltable([2,2,4], n3(:,209), t3x4(:,:,21))
  call heltable([4,2,8], n3(:,210), t3x8(:,:,52))
  call heltable([12,2,24], n3(:,211), t3x24(:,:,39))
  call heltable([2,4,8], n3(:,212), t3x8(:,:,53))
  call heltable([4,2,8], n3(:,213), t3x8(:,:,54))
  call heltable([2,2,4], n3(:,214), t3x4(:,:,22))
  call heltable([2,4,8], n3(:,215), t3x8(:,:,55))
  call heltable([4,2,8], n3(:,216), t3x8(:,:,56))
  call heltable([12,2,24], n3(:,217), t3x24(:,:,40))
  call heltable([4,2,8], n3(:,218), t3x8(:,:,57))
  call heltable([12,2,24], n3(:,219), t3x24(:,:,41))
  call heltable([4,2,8], n3(:,220), t3x8(:,:,58))
  call heltable([12,2,24], n3(:,221), t3x24(:,:,42))
  call heltable([8,2,16], n3(:,222), t3x16(:,:,25))
  call heltable([8,2,16], n3(:,223), t3x16(:,:,26))
  call heltable([8,2,16], n3(:,224), t3x16(:,:,27))
  call heltable([8,2,16], n3(:,225), t3x16(:,:,28))
  call heltable([4,2,8], n3(:,226), t3x8(:,:,59))
  call heltable([2,2,4], n3(:,227), t3x4(:,:,23))
  call heltable([12,2,24], n3(:,228), t3x24(:,:,43))
  call heltable([4,2,8], n3(:,229), t3x8(:,:,60))
  call heltable([2,4,8], n3(:,230), t3x8(:,:,61))
  call heltable([4,2,8], n3(:,231), t3x8(:,:,62))
  call heltable([2,2,4], n3(:,232), t3x4(:,:,24))
  call heltable([2,4,8], n3(:,233), t3x8(:,:,63))
  call heltable([12,2,24], n3(:,234), t3x24(:,:,44))
  call heltable([4,2,8], n3(:,235), t3x8(:,:,64))
  call heltable([12,2,24], n3(:,236), t3x24(:,:,45))
  call heltable([4,2,8], n3(:,237), t3x8(:,:,65))
  call heltable([12,2,24], n3(:,238), t3x24(:,:,46))
  call heltable([4,2,8], n3(:,239), t3x8(:,:,66))
  call heltable([8,2,16], n3(:,240), t3x16(:,:,29))
  call heltable([8,2,16], n3(:,241), t3x16(:,:,30))
  call heltable([8,2,16], n3(:,242), t3x16(:,:,31))
  call heltable([8,2,16], n3(:,243), t3x16(:,:,32))
  call heltable([8,12,96], n3(:,244), t3x96(:,:,1))
  call heltable([24,4,96], n3(:,245), t3x96(:,:,2))
  call heltable([8,12,96], n3(:,246), t3x96(:,:,3))
  call heltable([16,6,96], n3(:,247), t3x96(:,:,4))
  call heltable([8,12,96], n3(:,248), t3x96(:,:,5))
  call heltable([6,16,96], n3(:,249), t3x96(:,:,6))
  call heltable([4,24,96], n3(:,250), t3x96(:,:,7))
  call heltable([8,12,96], n3(:,251), t3x96(:,:,8))
  call heltable([8,12,96], n3(:,252), t3x96(:,:,9))
  call heltable([8,12,96], n3(:,253), t3x96(:,:,10))
  call heltable([24,4,96], n3(:,254), t3x96(:,:,11))
  call heltable([8,12,96], n3(:,255), t3x96(:,:,12))
  call heltable([16,6,96], n3(:,256), t3x96(:,:,13))
  call heltable([4,24,96], n3(:,257), t3x96(:,:,14))
  call heltable([6,16,96], n3(:,258), t3x96(:,:,15))
  call heltable([8,12,96], n3(:,259), t3x96(:,:,16))
  call heltable([16,6,96], n3(:,260), t3x96(:,:,17))
  call heltable([24,4,96], n3(:,261), t3x96(:,:,18))
  call heltable([16,6,96], n3(:,262), t3x96(:,:,19))
  call heltable([12,8,96], n3(:,263), t3x96(:,:,20))
  call heltable([8,12,96], n3(:,264), t3x96(:,:,21))
  call heltable([24,4,96], n3(:,265), t3x96(:,:,22))
  call heltable([16,6,96], n3(:,266), t3x96(:,:,23))
  call heltable([12,8,96], n3(:,267), t3x96(:,:,24))
  call heltable([16,6,96], n3(:,268), t3x96(:,:,25))
  call heltable([8,12,96], n3(:,269), t3x96(:,:,26))
  call heltable([8,12,96], n3(:,270), t3x96(:,:,27))
  call heltable([16,6,96], n3(:,271), t3x96(:,:,28))
  call heltable([8,12,96], n3(:,272), t3x96(:,:,29))
  call heltable([16,6,96], n3(:,273), t3x96(:,:,30))
  call heltable([16,6,96], n3(:,274), t3x96(:,:,31))
  call heltable([8,12,96], n3(:,275), t3x96(:,:,32))
  call heltable([8,12,96], n3(:,276), t3x96(:,:,33))
  call heltable([8,12,96], n3(:,277), t3x96(:,:,34))
  call heltable([24,4,96], n3(:,278), t3x96(:,:,35))
  call heltable([12,8,96], n3(:,279), t3x96(:,:,36))
  call heltable([12,8,96], n3(:,280), t3x96(:,:,37))
  call heltable([24,4,96], n3(:,281), t3x96(:,:,38))
  call heltable([24,4,96], n3(:,282), t3x96(:,:,39))
  call heltable([8,12,96], n3(:,283), t3x96(:,:,40))
  call heltable([12,8,96], n3(:,284), t3x96(:,:,41))
  call heltable([16,6,96], n3(:,285), t3x96(:,:,42))
  call heltable([12,8,96], n3(:,286), t3x96(:,:,43))
  call heltable([6,16,96], n3(:,287), t3x96(:,:,44))
  call heltable([8,12,96], n3(:,288), t3x96(:,:,45))
  call heltable([4,24,96], n3(:,289), t3x96(:,:,46))
  call heltable([8,12,96], n3(:,290), t3x96(:,:,47))
  call heltable([8,12,96], n3(:,291), t3x96(:,:,48))
  call heltable([4,24,96], n3(:,292), t3x96(:,:,49))
  call heltable([8,12,96], n3(:,293), t3x96(:,:,50))
  call heltable([6,16,96], n3(:,294), t3x96(:,:,51))
  call heltable([4,24,96], n3(:,295), t3x96(:,:,52))
  call heltable([6,16,96], n3(:,296), t3x96(:,:,53))
  call heltable([8,12,96], n3(:,297), t3x96(:,:,54))
  call heltable([16,6,96], n3(:,298), t3x96(:,:,55))
  call heltable([4,24,96], n3(:,299), t3x96(:,:,56))
  call heltable([16,6,96], n3(:,300), t3x96(:,:,57))
  call heltable([8,12,96], n3(:,301), t3x96(:,:,58))
  call heltable([12,8,96], n3(:,302), t3x96(:,:,59))
  call heltable([24,4,96], n3(:,303), t3x96(:,:,60))
  call heltable([6,16,96], n3(:,304), t3x96(:,:,61))
  call heltable([12,8,96], n3(:,305), t3x96(:,:,62))
  call heltable([6,16,96], n3(:,306), t3x96(:,:,63))
  call heltable([8,12,96], n3(:,307), t3x96(:,:,64))
  call heltable([8,12,96], n3(:,308), t3x96(:,:,65))
  call heltable([6,16,96], n3(:,309), t3x96(:,:,66))
  call heltable([12,8,96], n3(:,310), t3x96(:,:,67))
  call heltable([16,6,96], n3(:,311), t3x96(:,:,68))
  call heltable([6,16,96], n3(:,312), t3x96(:,:,69))
  call heltable([8,12,96], n3(:,313), t3x96(:,:,70))
  call heltable([8,12,96], n3(:,314), t3x96(:,:,71))
  call heltable([8,12,96], n3(:,315), t3x96(:,:,72))
  call heltable([24,4,96], n3(:,316), t3x96(:,:,73))
  call heltable([12,8,96], n3(:,317), t3x96(:,:,74))
  call heltable([12,8,96], n3(:,318), t3x96(:,:,75))
  call heltable([4,24,96], n3(:,319), t3x96(:,:,76))
  call heltable([8,12,96], n3(:,320), t3x96(:,:,77))
  call heltable([4,24,96], n3(:,321), t3x96(:,:,78))
  call heltable([8,12,96], n3(:,322), t3x96(:,:,79))
  call heltable([6,16,96], n3(:,323), t3x96(:,:,80))
  call heltable([8,12,96], n3(:,324), t3x96(:,:,81))
  call heltable([6,16,96], n3(:,325), t3x96(:,:,82))
  call heltable([4,24,96], n3(:,326), t3x96(:,:,83))
  call heltable([8,12,96], n3(:,327), t3x96(:,:,84))
  call heltable([8,12,96], n3(:,328), t3x96(:,:,85))
  call heltable([12,8,96], n3(:,329), t3x96(:,:,86))
  call heltable([24,4,96], n3(:,330), t3x96(:,:,87))
  call heltable([12,8,96], n3(:,331), t3x96(:,:,88))
  call heltable([16,6,96], n3(:,332), t3x96(:,:,89))
  call heltable([4,24,96], n3(:,333), t3x96(:,:,90))
  call heltable([6,16,96], n3(:,334), t3x96(:,:,91))
  call heltable([8,12,96], n3(:,335), t3x96(:,:,92))
  call heltable([6,16,96], n3(:,336), t3x96(:,:,93))
  call heltable([24,4,96], n3(:,337), t3x96(:,:,94))
  call heltable([6,16,96], n3(:,338), t3x96(:,:,95))
  call heltable([12,8,96], n3(:,339), t3x96(:,:,96))
  call heltable([8,12,96], n3(:,340), t3x96(:,:,97))
  call heltable([4,24,96], n3(:,341), t3x96(:,:,98))
  call heltable([16,6,96], n3(:,342), t3x96(:,:,99))
  call heltable([12,8,96], n3(:,343), t3x96(:,:,100))
  call heltable([16,6,96], n3(:,344), t3x96(:,:,101))
  call heltable([8,12,96], n3(:,345), t3x96(:,:,102))
  call heltable([12,8,96], n3(:,346), t3x96(:,:,103))
  call heltable([16,6,96], n3(:,347), t3x96(:,:,104))
  call heltable([8,12,96], n3(:,348), t3x96(:,:,105))
  call heltable([6,16,96], n3(:,349), t3x96(:,:,106))
  call heltable([16,6,96], n3(:,350), t3x96(:,:,107))
  call heltable([12,8,96], n3(:,351), t3x96(:,:,108))
  call heltable([8,12,96], n3(:,352), t3x96(:,:,109))
  call heltable([8,12,96], n3(:,353), t3x96(:,:,110))
  call heltable([4,24,96], n3(:,354), t3x96(:,:,111))
  call heltable([12,8,96], n3(:,355), t3x96(:,:,112))
  call heltable([12,8,96], n3(:,356), t3x96(:,:,113))
  call heltable([24,4,96], n3(:,357), t3x96(:,:,114))
  call heltable([4,24,96], n3(:,358), t3x96(:,:,115))
  call heltable([8,12,96], n3(:,359), t3x96(:,:,116))
  call heltable([12,8,96], n3(:,360), t3x96(:,:,117))
  call heltable([6,16,96], n3(:,361), t3x96(:,:,118))
  call heltable([12,8,96], n3(:,362), t3x96(:,:,119))
  call heltable([6,16,96], n3(:,363), t3x96(:,:,120))
  call heltable([8,12,96], n3(:,364), t3x96(:,:,121))
  call heltable([4,24,96], n3(:,365), t3x96(:,:,122))
  call heltable([12,8,96], n3(:,366), t3x96(:,:,123))
  call heltable([8,12,96], n3(:,367), t3x96(:,:,124))
  call heltable([4,24,96], n3(:,368), t3x96(:,:,125))
  call heltable([12,8,96], n3(:,369), t3x96(:,:,126))
  call heltable([6,16,96], n3(:,370), t3x96(:,:,127))
  call heltable([4,24,96], n3(:,371), t3x96(:,:,128))
  call heltable([6,16,96], n3(:,372), t3x96(:,:,129))
  call heltable([8,12,96], n3(:,373), t3x96(:,:,130))
  call heltable([6,16,96], n3(:,374), t3x96(:,:,131))
  call heltable([4,24,96], n3(:,375), t3x96(:,:,132))
  call heltable([6,16,96], n3(:,376), t3x96(:,:,133))
  call heltable([8,12,96], n3(:,377), t3x96(:,:,134))
  call heltable([12,8,96], n3(:,378), t3x96(:,:,135))
  call heltable([4,24,96], n3(:,379), t3x96(:,:,136))
  call heltable([6,16,96], n3(:,380), t3x96(:,:,137))
  call heltable([12,8,96], n3(:,381), t3x96(:,:,138))
  call heltable([6,16,96], n3(:,382), t3x96(:,:,139))
  call heltable([8,12,96], n3(:,383), t3x96(:,:,140))
  call heltable([12,8,96], n3(:,384), t3x96(:,:,141))
  call heltable([6,16,96], n3(:,385), t3x96(:,:,142))
  call heltable([12,8,96], n3(:,386), t3x96(:,:,143))
  call heltable([6,16,96], n3(:,387), t3x96(:,:,144))
  call heltable([6,16,96], n3(:,388), t3x96(:,:,145))
  call heltable([8,12,96], n3(:,389), t3x96(:,:,146))
  call heltable([8,12,96], n3(:,390), t3x96(:,:,147))
  call heltable([12,8,96], n3(:,391), t3x96(:,:,148))
  call heltable([4,24,96], n3(:,392), t3x96(:,:,149))
  call heltable([12,8,96], n3(:,393), t3x96(:,:,150))
  call heltable([12,8,96], n3(:,394), t3x96(:,:,151))
  call heltable([4,24,96], n3(:,395), t3x96(:,:,152))
  call heltable([4,24,96], n3(:,396), t3x96(:,:,153))
  call heltable([12,8,96], n3(:,397), t3x96(:,:,154))
  call heltable([4,24,96], n3(:,398), t3x96(:,:,155))
  call heltable([12,8,96], n3(:,399), t3x96(:,:,156))
  call heltable([12,8,96], n3(:,400), t3x96(:,:,157))
  call heltable([12,8,96], n3(:,401), t3x96(:,:,158))
  call heltable([4,24,96], n3(:,402), t3x96(:,:,159))
  call heltable([12,8,96], n3(:,403), t3x96(:,:,160))
  call heltable([4,24,96], n3(:,404), t3x96(:,:,161))
  call heltable([4,24,96], n3(:,405), t3x96(:,:,162))
  call heltable([4,24,96], n3(:,406), t3x96(:,:,163))
  call heltable([12,8,96], n3(:,407), t3x96(:,:,164))
  call heltable([8,12,96], n3(:,408), t3x96(:,:,165))
  call heltable([12,8,96], n3(:,409), t3x96(:,:,166))
  call heltable([4,24,96], n3(:,410), t3x96(:,:,167))
  call heltable([12,8,96], n3(:,411), t3x96(:,:,168))
  call heltable([4,24,96], n3(:,412), t3x96(:,:,169))
  call heltable([4,24,96], n3(:,413), t3x96(:,:,170))
  call heltable([12,8,96], n3(:,414), t3x96(:,:,171))
  call heltable([12,8,96], n3(:,415), t3x96(:,:,172))
  call heltable([8,12,96], n3(:,416), t3x96(:,:,173))
  call heltable([12,8,96], n3(:,417), t3x96(:,:,174))
  call heltable([4,24,96], n3(:,418), t3x96(:,:,175))
  call heltable([4,24,96], n3(:,419), t3x96(:,:,176))
  call heltable([12,8,96], n3(:,420), t3x96(:,:,177))
  call heltable([12,8,96], n3(:,421), t3x96(:,:,178))
  call heltable([6,16,96], n3(:,422), t3x96(:,:,179))
  call heltable([6,16,96], n3(:,423), t3x96(:,:,180))
  call heltable([12,8,96], n3(:,424), t3x96(:,:,181))
  call heltable([6,16,96], n3(:,425), t3x96(:,:,182))
  call heltable([12,8,96], n3(:,426), t3x96(:,:,183))
  call heltable([6,16,96], n3(:,427), t3x96(:,:,184))
  call heltable([12,8,96], n3(:,428), t3x96(:,:,185))
  call heltable([4,24,96], n3(:,429), t3x96(:,:,186))
  call heltable([12,8,96], n3(:,430), t3x96(:,:,187))
  call heltable([4,24,96], n3(:,431), t3x96(:,:,188))
  call heltable([4,24,96], n3(:,432), t3x96(:,:,189))
  call heltable([4,24,96], n3(:,433), t3x96(:,:,190))
  call heltable([12,8,96], n3(:,434), t3x96(:,:,191))
  call heltable([4,24,96], n3(:,435), t3x96(:,:,192))
  call heltable([12,8,96], n3(:,436), t3x96(:,:,193))
  call heltable([12,8,96], n3(:,437), t3x96(:,:,194))
  call heltable([12,8,96], n3(:,438), t3x96(:,:,195))
  call heltable([4,24,96], n3(:,439), t3x96(:,:,196))
  call heltable([8,12,96], n3(:,440), t3x96(:,:,197))
  call heltable([12,8,96], n3(:,441), t3x96(:,:,198))
  call heltable([4,24,96], n3(:,442), t3x96(:,:,199))
  call heltable([4,24,96], n3(:,443), t3x96(:,:,200))
  call heltable([12,8,96], n3(:,444), t3x96(:,:,201))
  call heltable([12,8,96], n3(:,445), t3x96(:,:,202))
  call heltable([4,24,96], n3(:,446), t3x96(:,:,203))
  call heltable([12,8,96], n3(:,447), t3x96(:,:,204))
  call heltable([8,12,96], n3(:,448), t3x96(:,:,205))
  call heltable([4,24,96], n3(:,449), t3x96(:,:,206))
  call heltable([12,8,96], n3(:,450), t3x96(:,:,207))
  call heltable([4,24,96], n3(:,451), t3x96(:,:,208))
  call heltable([6,16,96], n3(:,452), t3x96(:,:,209))
  call heltable([6,16,96], n3(:,453), t3x96(:,:,210))
  call heltable([12,8,96], n3(:,454), t3x96(:,:,211))
  call heltable([12,8,96], n3(:,455), t3x96(:,:,212))
  call heltable([6,16,96], n3(:,456), t3x96(:,:,213))
  call heltable([12,8,96], n3(:,457), t3x96(:,:,214))
  call heltable([6,16,96], n3(:,458), t3x96(:,:,215))
  call heltable([12,8,96], n3(:,459), t3x96(:,:,216))
  call heltable([12,8,96], n3(:,460), t3x96(:,:,217))
  call heltable([12,8,96], n3(:,461), t3x96(:,:,218))
  call heltable([24,4,96], n3(:,462), t3x96(:,:,219))
  call heltable([12,8,96], n3(:,463), t3x96(:,:,220))
  call heltable([12,8,96], n3(:,464), t3x96(:,:,221))
  call heltable([4,24,96], n3(:,465), t3x96(:,:,222))
  call heltable([12,8,96], n3(:,466), t3x96(:,:,223))
  call heltable([12,8,96], n3(:,467), t3x96(:,:,224))
  call heltable([12,8,96], n3(:,468), t3x96(:,:,225))
  call heltable([24,4,96], n3(:,469), t3x96(:,:,226))
  call heltable([12,8,96], n3(:,470), t3x96(:,:,227))
  call heltable([12,8,96], n3(:,471), t3x96(:,:,228))
  call heltable([12,8,96], n3(:,472), t3x96(:,:,229))
  call heltable([4,24,96], n3(:,473), t3x96(:,:,230))
  call heltable([24,4,96], n3(:,474), t3x96(:,:,231))
  call heltable([12,8,96], n3(:,475), t3x96(:,:,232))
  call heltable([24,4,96], n3(:,476), t3x96(:,:,233))
  call heltable([12,8,96], n3(:,477), t3x96(:,:,234))
  call heltable([12,8,96], n3(:,478), t3x96(:,:,235))
  call heltable([4,24,96], n3(:,479), t3x96(:,:,236))
  call heltable([12,8,96], n3(:,480), t3x96(:,:,237))
  call heltable([4,24,96], n3(:,481), t3x96(:,:,238))
  call heltable([12,8,96], n3(:,482), t3x96(:,:,239))
  call heltable([24,4,96], n3(:,483), t3x96(:,:,240))
  call heltable([6,16,96], n3(:,484), t3x96(:,:,241))
  call heltable([12,8,96], n3(:,485), t3x96(:,:,242))
  call heltable([12,8,96], n3(:,486), t3x96(:,:,243))
  call heltable([6,16,96], n3(:,487), t3x96(:,:,244))
  call heltable([12,8,96], n3(:,488), t3x96(:,:,245))
  call heltable([6,16,96], n3(:,489), t3x96(:,:,246))
  call heltable([12,8,96], n3(:,490), t3x96(:,:,247))
  call heltable([6,16,96], n3(:,491), t3x96(:,:,248))
  call heltable([12,8,96], n3(:,492), t3x96(:,:,249))
  call heltable([12,8,96], n3(:,493), t3x96(:,:,250))
  call heltable([12,8,96], n3(:,494), t3x96(:,:,251))
  call heltable([24,4,96], n3(:,495), t3x96(:,:,252))
  call heltable([4,24,96], n3(:,496), t3x96(:,:,253))
  call heltable([12,8,96], n3(:,497), t3x96(:,:,254))
  call heltable([12,8,96], n3(:,498), t3x96(:,:,255))
  call heltable([12,8,96], n3(:,499), t3x96(:,:,256))
  call heltable([12,8,96], n3(:,500), t3x96(:,:,257))
  call heltable([12,8,96], n3(:,501), t3x96(:,:,258))
  call heltable([24,4,96], n3(:,502), t3x96(:,:,259))
  call heltable([12,8,96], n3(:,503), t3x96(:,:,260))
  call heltable([4,24,96], n3(:,504), t3x96(:,:,261))
  call heltable([12,8,96], n3(:,505), t3x96(:,:,262))
  call heltable([12,8,96], n3(:,506), t3x96(:,:,263))
  call heltable([24,4,96], n3(:,507), t3x96(:,:,264))
  call heltable([12,8,96], n3(:,508), t3x96(:,:,265))
  call heltable([24,4,96], n3(:,509), t3x96(:,:,266))
  call heltable([4,24,96], n3(:,510), t3x96(:,:,267))
  call heltable([12,8,96], n3(:,511), t3x96(:,:,268))
  call heltable([4,24,96], n3(:,512), t3x96(:,:,269))
  call heltable([12,8,96], n3(:,513), t3x96(:,:,270))
  call heltable([24,4,96], n3(:,514), t3x96(:,:,271))
  call heltable([12,8,96], n3(:,515), t3x96(:,:,272))
  call heltable([6,16,96], n3(:,516), t3x96(:,:,273))
  call heltable([12,8,96], n3(:,517), t3x96(:,:,274))
  call heltable([12,8,96], n3(:,518), t3x96(:,:,275))
  call heltable([6,16,96], n3(:,519), t3x96(:,:,276))
  call heltable([6,16,96], n3(:,520), t3x96(:,:,277))
  call heltable([12,8,96], n3(:,521), t3x96(:,:,278))
  call heltable([6,16,96], n3(:,522), t3x96(:,:,279))
  call heltable([12,8,96], n3(:,523), t3x96(:,:,280))
  call heltable([4,24,96], n3(:,524), t3x96(:,:,281))
  call heltable([4,24,96], n3(:,525), t3x96(:,:,282))
  call heltable([6,16,96], n3(:,526), t3x96(:,:,283))
  call heltable([6,16,96], n3(:,527), t3x96(:,:,284))
  call heltable([4,24,96], n3(:,528), t3x96(:,:,285))
  call heltable([6,16,96], n3(:,529), t3x96(:,:,286))
  call heltable([4,24,96], n3(:,530), t3x96(:,:,287))
  call heltable([6,16,96], n3(:,531), t3x96(:,:,288))
  call heltable([6,16,96], n3(:,532), t3x96(:,:,289))
  call heltable([6,16,96], n3(:,533), t3x96(:,:,290))
  call heltable([4,24,96], n3(:,534), t3x96(:,:,291))
  call heltable([4,24,96], n3(:,535), t3x96(:,:,292))
  call heltable([6,16,96], n3(:,536), t3x96(:,:,293))
  call heltable([4,24,96], n3(:,537), t3x96(:,:,294))
  call heltable([6,16,96], n3(:,538), t3x96(:,:,295))
  call heltable([4,24,96], n3(:,539), t3x96(:,:,296))
  call heltable([6,16,96], n3(:,540), t3x96(:,:,297))
  call heltable([4,24,96], n3(:,541), t3x96(:,:,298))
  call heltable([4,24,96], n3(:,542), t3x96(:,:,299))
  call heltable([6,16,96], n3(:,543), t3x96(:,:,300))
  call heltable([4,24,96], n3(:,544), t3x96(:,:,301))
  call heltable([6,16,96], n3(:,545), t3x96(:,:,302))
  call heltable([4,24,96], n3(:,546), t3x96(:,:,303))
  call heltable([6,16,96], n3(:,547), t3x96(:,:,304))
  call heltable([6,16,96], n3(:,548), t3x96(:,:,305))
  call heltable([4,24,96], n3(:,549), t3x96(:,:,306))
  call heltable([4,24,96], n3(:,550), t3x96(:,:,307))
  call heltable([6,16,96], n3(:,551), t3x96(:,:,308))
  call heltable([6,16,96], n3(:,552), t3x96(:,:,309))
  call heltable([4,24,96], n3(:,553), t3x96(:,:,310))
  call heltable([6,16,96], n3(:,554), t3x96(:,:,311))
  call heltable([4,24,96], n3(:,555), t3x96(:,:,312))
  call heltable([6,16,96], n3(:,556), t3x96(:,:,313))
  call heltable([12,8,96], n3(:,557), t3x96(:,:,314))
  call heltable([12,8,96], n3(:,558), t3x96(:,:,315))
  call heltable([12,8,96], n3(:,559), t3x96(:,:,316))
  call heltable([6,16,96], n3(:,560), t3x96(:,:,317))
  call heltable([12,8,96], n3(:,561), t3x96(:,:,318))
  call heltable([12,8,96], n3(:,562), t3x96(:,:,319))
  call heltable([12,8,96], n3(:,563), t3x96(:,:,320))
  call heltable([6,16,96], n3(:,564), t3x96(:,:,321))
  call heltable([6,16,96], n3(:,565), t3x96(:,:,322))
  call heltable([6,16,96], n3(:,566), t3x96(:,:,323))
  call heltable([12,8,96], n3(:,567), t3x96(:,:,324))
  call heltable([6,16,96], n3(:,568), t3x96(:,:,325))
  call heltable([6,16,96], n3(:,569), t3x96(:,:,326))
  call heltable([6,16,96], n3(:,570), t3x96(:,:,327))
  call heltable([12,8,96], n3(:,571), t3x96(:,:,328))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_pphzjj_bbbxbxhzg_1
