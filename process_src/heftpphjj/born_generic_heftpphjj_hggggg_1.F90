
module ol_external_heftpphjj_hggggg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_heftpphjj_hggggg_1(6) = &
                     [ (dummy_counter, dummy_counter = 1, 6) ]
  integer, save :: external_perm_inv_heftpphjj_hggggg_1(6) = &
                     [ (dummy_counter, dummy_counter = 1, 6) ]
  integer, save :: extcomb_perm_heftpphjj_hggggg_1(0:22) = &
                     [ (dummy_counter, dummy_counter = 0, 22) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_heftpphjj_hggggg_1(6) = &
                     [ 1, 2, 2, 2, 2, 2 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_heftpphjj_hggggg_1(6) = &
                     [ 1, 16, 16, 16, 16, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_heftpphjj_hggggg_1 = &
                     384
  integer, save :: channel_number_heftpphjj_hggggg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(6,32) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(32,6)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_heftpphjj_hggggg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 6
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_heftpphjj_hggggg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 6
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_heftpphjj_hggggg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(6)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_heftpphjj_hggggg_1(6)
    external_perm_heftpphjj_hggggg_1 = perm
    do i = 1, 6
      external_perm_inv_heftpphjj_hggggg_1( &
        external_perm_heftpphjj_hggggg_1(i)) = i
      particle_types_perm_heftpphjj_hggggg_1(i) = &
        particle_types_heftpphjj_hggggg_1( &
        external_perm_heftpphjj_hggggg_1(i))
    end do
    do i = 1, 6
      do j = 1, i
        if (external_perm_heftpphjj_hggggg_1(i) >= &
          external_perm_heftpphjj_hggggg_1(j)) then
          ii = external_perm_heftpphjj_hggggg_1(i)
          jj = external_perm_heftpphjj_hggggg_1(j)
        else
          ii = external_perm_heftpphjj_hggggg_1(j)
          jj = external_perm_heftpphjj_hggggg_1(i)
        end if
        extcomb_perm_heftpphjj_hggggg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_heftpphjj_hggggg_1 = &
      average_factors_heftpphjj_hggggg_1( &
      external_perm_heftpphjj_hggggg_1(1)) &
      * average_factors_heftpphjj_hggggg_1( &
      external_perm_heftpphjj_hggggg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 6
        average_factor_heftpphjj_hggggg_1 = &
          average_factor_heftpphjj_hggggg_1 &
          * factorial(count(particle_types_perm_heftpphjj_hggggg_1(3:6) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_heftpphjj_hggggg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(6)
    integer :: f_perm(6)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_heftpphjj_hggggg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(6)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(6)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rMH_unscaled, rZERO, rZERO, rZERO, rZERO, rZERO ]
    do i = 1, 6
      m_ex(i) = m_ex_orig(external_perm_heftpphjj_hggggg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_heftpphjj_hggggg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(6)
    real(DREALKIND) :: f_m_ex(6)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_heftpphjj_hggggg_1")
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
      & bind(c,name="ol_rambo_heftpphjj_hggggg_1")
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
  H(:, 1) = [  0, -1, -1, -1, -1, -1 ]
  H(:, 2) = [  0, -1, -1, -1, -1,  1 ]
  H(:, 3) = [  0, -1, -1, -1,  1, -1 ]
  H(:, 4) = [  0, -1, -1, -1,  1,  1 ]
  H(:, 5) = [  0, -1, -1,  1, -1, -1 ]
  H(:, 6) = [  0, -1, -1,  1, -1,  1 ]
  H(:, 7) = [  0, -1, -1,  1,  1, -1 ]
  H(:, 8) = [  0, -1, -1,  1,  1,  1 ]
  H(:, 9) = [  0, -1,  1, -1, -1, -1 ]
  H(:,10) = [  0, -1,  1, -1, -1,  1 ]
  H(:,11) = [  0, -1,  1, -1,  1, -1 ]
  H(:,12) = [  0, -1,  1, -1,  1,  1 ]
  H(:,13) = [  0, -1,  1,  1, -1, -1 ]
  H(:,14) = [  0, -1,  1,  1, -1,  1 ]
  H(:,15) = [  0, -1,  1,  1,  1, -1 ]
  H(:,16) = [  0, -1,  1,  1,  1,  1 ]
  H(:,17) = [  0,  1, -1, -1, -1, -1 ]
  H(:,18) = [  0,  1, -1, -1, -1,  1 ]
  H(:,19) = [  0,  1, -1, -1,  1, -1 ]
  H(:,20) = [  0,  1, -1, -1,  1,  1 ]
  H(:,21) = [  0,  1, -1,  1, -1, -1 ]
  H(:,22) = [  0,  1, -1,  1, -1,  1 ]
  H(:,23) = [  0,  1, -1,  1,  1, -1 ]
  H(:,24) = [  0,  1, -1,  1,  1,  1 ]
  H(:,25) = [  0,  1,  1, -1, -1, -1 ]
  H(:,26) = [  0,  1,  1, -1, -1,  1 ]
  H(:,27) = [  0,  1,  1, -1,  1, -1 ]
  H(:,28) = [  0,  1,  1, -1,  1,  1 ]
  H(:,29) = [  0,  1,  1,  1, -1, -1 ]
  H(:,30) = [  0,  1,  1,  1, -1,  1 ]
  H(:,31) = [  0,  1,  1,  1,  1, -1 ]
  H(:,32) = [  0,  1,  1,  1,  1,  1 ]

  H_HC(:,2) = [ ((((2*(binco-1)+flip)*16+binpos, flip = 0, 1), binpos = 1, 16), binco = 1, 32/16/2) ]
  H_HC(:,3) = [ ((((2*(binco-1)+flip)*8+binpos, flip = 0, 1), binpos = 1, 8), binco = 1, 32/8/2) ]
  H_HC(:,4) = [ ((((2*(binco-1)+flip)*4+binpos, flip = 0, 1), binpos = 1, 4), binco = 1, 32/4/2) ]
  H_HC(:,5) = [ ((((2*(binco-1)+flip)*2+binpos, flip = 0, 1), binpos = 1, 2), binco = 1, 32/2/2) ]
  H_HC(:,6) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 32/1/2) ]
  end subroutine hel_init

end module ol_external_heftpphjj_hggggg_1


module colour_basis_heftpphjj_hggggg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(6) = [0,2,2,2,2,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_heftpphjj_hggggg_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(6)
    ! number of tree colour basis elements; number of selected couplings, number of selected powers per coupling
    integer, intent(out) :: ncolb, ncoupl, maxpows
    ! number of helicity configurations (all, not just non-vanishing)
    integer, intent(out) :: nhel
    extcols = extcolours
    ncolb = 24
    ncoupl = 1
    maxpows = 1
    nhel = 32
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_heftpphjj_hggggg_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,24)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([2], [1,1])
#endif
#if 24 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [2,1,1,2,2,1,2,3,1,2,4,1,2,5,1,2,6,1,2,7,1,2,8,1,2,9,1,2,10,1,2,11,1,2,12,1,2,13,1,2,14,1,2,15,1,2,16,1,2,17,1,2,18,1,2 &
      ,19,1,2,20,1,2,21,1,2,22,1,2,23,1,2,24,1], &
      [3,24])
#endif
  end subroutine tree_colbasis

end module colour_basis_heftpphjj_hggggg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_heftpphjj_hggggg_1(perm)
  use ol_external_heftpphjj_hggggg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(6)
  call set_permutation(perm)
end subroutine set_permutation_heftpphjj_hggggg_1

! **********************************************************************
module ol_heltables_heftpphjj_hggggg_1
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
    H1(1) = [0], &
    H2(2) = [-1,1], &
    H3(2) = [-1,1], &
    H4(2) = [-1,1], &
    H5(2) = [-1,1], &
    H6(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n3(3,480), n4(4,145), n5(5,30)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x2(2,2,5), t3x4(2,4,40), t3x8(2,8,45), t3x32(2,32,390), t4x4(3,4,10), t4x8(3,8,135), t5x8(4,8,30)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(32,6)
  integer,           save :: exthel(32,6)
  integer,           save :: firstpol(6)

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
  call heltable([1,2,2,4], n4(:,1), t4x4(:,:,1))
  call heltable([2,2,2,8], n4(:,2), t4x8(:,:,1))
  call heltable([2,2,2,8], n4(:,3), t4x8(:,:,2))
  call heltable([2,2,2,8], n4(:,4), t4x8(:,:,3))
  call heltable([1,2,2,4], n4(:,5), t4x4(:,:,2))
  call heltable([2,2,2,8], n4(:,6), t4x8(:,:,4))
  call heltable([2,2,2,8], n4(:,7), t4x8(:,:,5))
  call heltable([2,2,2,8], n4(:,8), t4x8(:,:,6))
  call heltable([1,2,2,4], n4(:,9), t4x4(:,:,3))
  call heltable([2,2,2,8], n4(:,10), t4x8(:,:,7))
  call heltable([2,2,2,8], n4(:,11), t4x8(:,:,8))
  call heltable([2,2,2,8], n4(:,12), t4x8(:,:,9))
  call heltable([1,2,2,4], n4(:,13), t4x4(:,:,4))
  call heltable([2,2,2,8], n4(:,14), t4x8(:,:,10))
  call heltable([2,2,2,8], n4(:,15), t4x8(:,:,11))
  call heltable([2,2,2,8], n4(:,16), t4x8(:,:,12))
  call heltable([1,2,2,4], n4(:,17), t4x4(:,:,5))
  call heltable([2,2,2,8], n4(:,18), t4x8(:,:,13))
  call heltable([2,2,2,8], n4(:,19), t4x8(:,:,14))
  call heltable([2,2,2,8], n4(:,20), t4x8(:,:,15))
  call heltable([1,2,2,4], n4(:,21), t4x4(:,:,6))
  call heltable([2,2,2,8], n4(:,22), t4x8(:,:,16))
  call heltable([2,2,2,8], n4(:,23), t4x8(:,:,17))
  call heltable([2,2,2,8], n4(:,24), t4x8(:,:,18))
  call heltable([1,2,2,4], n4(:,25), t4x4(:,:,7))
  call heltable([2,2,2,8], n4(:,26), t4x8(:,:,19))
  call heltable([2,2,2,8], n4(:,27), t4x8(:,:,20))
  call heltable([2,2,2,8], n4(:,28), t4x8(:,:,21))
  call heltable([1,2,2,4], n4(:,29), t4x4(:,:,8))
  call heltable([2,2,2,8], n4(:,30), t4x8(:,:,22))
  call heltable([2,2,2,8], n4(:,31), t4x8(:,:,23))
  call heltable([2,2,2,8], n4(:,32), t4x8(:,:,24))
  call heltable([1,2,2,4], n4(:,33), t4x4(:,:,9))
  call heltable([2,2,2,8], n4(:,34), t4x8(:,:,25))
  call heltable([2,2,2,8], n4(:,35), t4x8(:,:,26))
  call heltable([2,2,2,8], n4(:,36), t4x8(:,:,27))
  call heltable([1,2,2,4], n4(:,37), t4x4(:,:,10))
  call heltable([2,2,2,8], n4(:,38), t4x8(:,:,28))
  call heltable([2,2,2,8], n4(:,39), t4x8(:,:,29))
  call heltable([2,2,2,8], n4(:,40), t4x8(:,:,30))
  call heltable([2,2,4], n3(:,1), t3x4(:,:,1))
  call heltable([1,2,2,2,8], n5(:,1), t5x8(:,:,1))
  call heltable([1,2,2,2,8], n5(:,2), t5x8(:,:,2))
  call heltable([1,2,2,2,8], n5(:,3), t5x8(:,:,3))
  call heltable([2,2,4], n3(:,2), t3x4(:,:,2))
  call heltable([1,2,2,2,8], n5(:,4), t5x8(:,:,4))
  call heltable([1,2,2,2,8], n5(:,5), t5x8(:,:,5))
  call heltable([1,2,2,2,8], n5(:,6), t5x8(:,:,6))
  call heltable([2,2,4], n3(:,3), t3x4(:,:,3))
  call heltable([1,2,2,2,8], n5(:,7), t5x8(:,:,7))
  call heltable([1,2,2,2,8], n5(:,8), t5x8(:,:,8))
  call heltable([1,2,2,2,8], n5(:,9), t5x8(:,:,9))
  call heltable([2,2,4], n3(:,4), t3x4(:,:,4))
  call heltable([1,2,2,2,8], n5(:,10), t5x8(:,:,10))
  call heltable([1,2,2,2,8], n5(:,11), t5x8(:,:,11))
  call heltable([1,2,2,2,8], n5(:,12), t5x8(:,:,12))
  call heltable([2,2,4], n3(:,5), t3x4(:,:,5))
  call heltable([1,2,2,2,8], n5(:,13), t5x8(:,:,13))
  call heltable([1,2,2,2,8], n5(:,14), t5x8(:,:,14))
  call heltable([1,2,2,2,8], n5(:,15), t5x8(:,:,15))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,6))
  call heltable([1,2,2,2,8], n5(:,16), t5x8(:,:,16))
  call heltable([1,2,2,2,8], n5(:,17), t5x8(:,:,17))
  call heltable([1,2,2,2,8], n5(:,18), t5x8(:,:,18))
  call heltable([2,2,4], n3(:,7), t3x4(:,:,7))
  call heltable([1,2,2,2,8], n5(:,19), t5x8(:,:,19))
  call heltable([1,2,2,2,8], n5(:,20), t5x8(:,:,20))
  call heltable([1,2,2,2,8], n5(:,21), t5x8(:,:,21))
  call heltable([2,2,4], n3(:,8), t3x4(:,:,8))
  call heltable([1,2,2,2,8], n5(:,22), t5x8(:,:,22))
  call heltable([1,2,2,2,8], n5(:,23), t5x8(:,:,23))
  call heltable([1,2,2,2,8], n5(:,24), t5x8(:,:,24))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,9))
  call heltable([1,2,2,2,8], n5(:,25), t5x8(:,:,25))
  call heltable([1,2,2,2,8], n5(:,26), t5x8(:,:,26))
  call heltable([1,2,2,2,8], n5(:,27), t5x8(:,:,27))
  call heltable([2,2,4], n3(:,10), t3x4(:,:,10))
  call heltable([1,2,2,2,8], n5(:,28), t5x8(:,:,28))
  call heltable([1,2,2,2,8], n5(:,29), t5x8(:,:,29))
  call heltable([1,2,2,2,8], n5(:,30), t5x8(:,:,30))
  call heltable([1,2,2], n3(:,11), t3x2(:,:,1))
  call heltable([2,2,2,8], n4(:,41), t4x8(:,:,31))
  call heltable([2,2,2,8], n4(:,42), t4x8(:,:,32))
  call heltable([2,2,2,8], n4(:,43), t4x8(:,:,33))
  call heltable([2,2,2,8], n4(:,44), t4x8(:,:,34))
  call heltable([2,2,2,8], n4(:,45), t4x8(:,:,35))
  call heltable([2,2,2,8], n4(:,46), t4x8(:,:,36))
  call heltable([2,2,2,8], n4(:,47), t4x8(:,:,37))
  call heltable([2,2,2,8], n4(:,48), t4x8(:,:,38))
  call heltable([2,2,2,8], n4(:,49), t4x8(:,:,39))
  call heltable([2,2,4], n3(:,12), t3x4(:,:,11))
  call heltable([2,2,2,8], n4(:,50), t4x8(:,:,40))
  call heltable([2,2,2,8], n4(:,51), t4x8(:,:,41))
  call heltable([2,2,2,8], n4(:,52), t4x8(:,:,42))
  call heltable([2,2,2,8], n4(:,53), t4x8(:,:,43))
  call heltable([2,2,2,8], n4(:,54), t4x8(:,:,44))
  call heltable([2,2,2,8], n4(:,55), t4x8(:,:,45))
  call heltable([2,2,4], n3(:,13), t3x4(:,:,12))
  call heltable([2,2,2,8], n4(:,56), t4x8(:,:,46))
  call heltable([2,2,2,8], n4(:,57), t4x8(:,:,47))
  call heltable([2,2,2,8], n4(:,58), t4x8(:,:,48))
  call heltable([2,2,4], n3(:,14), t3x4(:,:,13))
  call heltable([2,2,4], n3(:,15), t3x4(:,:,14))
  call heltable([1,2,2], n3(:,16), t3x2(:,:,2))
  call heltable([2,2,2,8], n4(:,59), t4x8(:,:,49))
  call heltable([2,2,2,8], n4(:,60), t4x8(:,:,50))
  call heltable([2,2,2,8], n4(:,61), t4x8(:,:,51))
  call heltable([2,2,2,8], n4(:,62), t4x8(:,:,52))
  call heltable([2,2,2,8], n4(:,63), t4x8(:,:,53))
  call heltable([2,2,2,8], n4(:,64), t4x8(:,:,54))
  call heltable([2,2,2,8], n4(:,65), t4x8(:,:,55))
  call heltable([2,2,2,8], n4(:,66), t4x8(:,:,56))
  call heltable([2,2,2,8], n4(:,67), t4x8(:,:,57))
  call heltable([2,2,4], n3(:,17), t3x4(:,:,15))
  call heltable([1,2,2], n3(:,18), t3x2(:,:,3))
  call heltable([2,2,2,8], n4(:,68), t4x8(:,:,58))
  call heltable([2,2,2,8], n4(:,69), t4x8(:,:,59))
  call heltable([2,2,2,8], n4(:,70), t4x8(:,:,60))
  call heltable([1,2,2], n3(:,19), t3x2(:,:,4))
  call heltable([2,2,2,8], n4(:,71), t4x8(:,:,61))
  call heltable([2,2,2,8], n4(:,72), t4x8(:,:,62))
  call heltable([2,2,2,8], n4(:,73), t4x8(:,:,63))
  call heltable([1,2,2], n3(:,20), t3x2(:,:,5))
  call heltable([2,2,2,8], n4(:,74), t4x8(:,:,64))
  call heltable([2,2,2,8], n4(:,75), t4x8(:,:,65))
  call heltable([2,2,2,8], n4(:,76), t4x8(:,:,66))
  call heltable([1,4,4], n3(:,21), t3x4(:,:,16))
  call heltable([2,2,2,8], n4(:,77), t4x8(:,:,67))
  call heltable([2,2,2,8], n4(:,78), t4x8(:,:,68))
  call heltable([2,2,2,8], n4(:,79), t4x8(:,:,69))
  call heltable([2,2,2,8], n4(:,80), t4x8(:,:,70))
  call heltable([2,2,2,8], n4(:,81), t4x8(:,:,71))
  call heltable([2,2,2,8], n4(:,82), t4x8(:,:,72))
  call heltable([2,2,4], n3(:,22), t3x4(:,:,17))
  call heltable([2,2,2,8], n4(:,83), t4x8(:,:,73))
  call heltable([2,2,2,8], n4(:,84), t4x8(:,:,74))
  call heltable([2,2,2,8], n4(:,85), t4x8(:,:,75))
  call heltable([2,2,2,8], n4(:,86), t4x8(:,:,76))
  call heltable([2,2,2,8], n4(:,87), t4x8(:,:,77))
  call heltable([2,2,2,8], n4(:,88), t4x8(:,:,78))
  call heltable([1,4,4], n3(:,23), t3x4(:,:,18))
  call heltable([2,2,2,8], n4(:,89), t4x8(:,:,79))
  call heltable([2,2,2,8], n4(:,90), t4x8(:,:,80))
  call heltable([2,2,2,8], n4(:,91), t4x8(:,:,81))
  call heltable([2,2,4], n3(:,24), t3x4(:,:,19))
  call heltable([2,2,2,8], n4(:,92), t4x8(:,:,82))
  call heltable([2,2,2,8], n4(:,93), t4x8(:,:,83))
  call heltable([2,2,2,8], n4(:,94), t4x8(:,:,84))
  call heltable([1,4,4], n3(:,25), t3x4(:,:,20))
  call heltable([2,2,4], n3(:,26), t3x4(:,:,21))
  call heltable([1,4,4], n3(:,27), t3x4(:,:,22))
  call heltable([2,2,2,8], n4(:,95), t4x8(:,:,85))
  call heltable([2,2,2,8], n4(:,96), t4x8(:,:,86))
  call heltable([2,2,2,8], n4(:,97), t4x8(:,:,87))
  call heltable([2,2,2,8], n4(:,98), t4x8(:,:,88))
  call heltable([2,2,2,8], n4(:,99), t4x8(:,:,89))
  call heltable([2,2,2,8], n4(:,100), t4x8(:,:,90))
  call heltable([2,2,4], n3(:,28), t3x4(:,:,23))
  call heltable([2,2,2,8], n4(:,101), t4x8(:,:,91))
  call heltable([2,2,2,8], n4(:,102), t4x8(:,:,92))
  call heltable([2,2,2,8], n4(:,103), t4x8(:,:,93))
  call heltable([2,2,4], n3(:,29), t3x4(:,:,24))
  call heltable([2,2,4], n3(:,30), t3x4(:,:,25))
  call heltable([2,2,2,8], n4(:,104), t4x8(:,:,94))
  call heltable([2,2,2,8], n4(:,105), t4x8(:,:,95))
  call heltable([2,2,2,8], n4(:,106), t4x8(:,:,96))
  call heltable([2,2,2,8], n4(:,107), t4x8(:,:,97))
  call heltable([2,2,2,8], n4(:,108), t4x8(:,:,98))
  call heltable([2,2,2,8], n4(:,109), t4x8(:,:,99))
  call heltable([2,2,4], n3(:,31), t3x4(:,:,26))
  call heltable([2,2,2,8], n4(:,110), t4x8(:,:,100))
  call heltable([2,2,2,8], n4(:,111), t4x8(:,:,101))
  call heltable([2,2,2,8], n4(:,112), t4x8(:,:,102))
  call heltable([2,2,2,8], n4(:,113), t4x8(:,:,103))
  call heltable([2,2,2,8], n4(:,114), t4x8(:,:,104))
  call heltable([2,2,2,8], n4(:,115), t4x8(:,:,105))
  call heltable([1,4,4], n3(:,32), t3x4(:,:,27))
  call heltable([2,2,2,8], n4(:,116), t4x8(:,:,106))
  call heltable([2,2,2,8], n4(:,117), t4x8(:,:,107))
  call heltable([2,2,2,8], n4(:,118), t4x8(:,:,108))
  call heltable([2,2,4], n3(:,33), t3x4(:,:,28))
  call heltable([2,2,2,8], n4(:,119), t4x8(:,:,109))
  call heltable([2,2,2,8], n4(:,120), t4x8(:,:,110))
  call heltable([2,2,2,8], n4(:,121), t4x8(:,:,111))
  call heltable([1,4,4], n3(:,34), t3x4(:,:,29))
  call heltable([2,2,4], n3(:,35), t3x4(:,:,30))
  call heltable([1,4,4], n3(:,36), t3x4(:,:,31))
  call heltable([2,2,2,8], n4(:,122), t4x8(:,:,112))
  call heltable([2,2,2,8], n4(:,123), t4x8(:,:,113))
  call heltable([2,2,2,8], n4(:,124), t4x8(:,:,114))
  call heltable([2,2,4], n3(:,37), t3x4(:,:,32))
  call heltable([2,2,4], n3(:,38), t3x4(:,:,33))
  call heltable([2,2,2,8], n4(:,125), t4x8(:,:,115))
  call heltable([2,2,2,8], n4(:,126), t4x8(:,:,116))
  call heltable([2,2,2,8], n4(:,127), t4x8(:,:,117))
  call heltable([2,2,4], n3(:,39), t3x4(:,:,34))
  call heltable([2,2,2,8], n4(:,128), t4x8(:,:,118))
  call heltable([2,2,2,8], n4(:,129), t4x8(:,:,119))
  call heltable([2,2,2,8], n4(:,130), t4x8(:,:,120))
  call heltable([1,4,4], n3(:,40), t3x4(:,:,35))
  call heltable([2,2,4], n3(:,41), t3x4(:,:,36))
  call heltable([1,4,4], n3(:,42), t3x4(:,:,37))
  call heltable([2,2,4], n3(:,43), t3x4(:,:,38))
  call heltable([2,2,4], n3(:,44), t3x4(:,:,39))
  call heltable([1,4,4], n3(:,45), t3x4(:,:,40))
  call heltable([1,4,2,8], n4(:,131), t4x8(:,:,121))
  call heltable([1,4,2,8], n4(:,132), t4x8(:,:,122))
  call heltable([4,2,8], n3(:,46), t3x8(:,:,1))
  call heltable([1,4,2,8], n4(:,133), t4x8(:,:,123))
  call heltable([4,2,8], n3(:,47), t3x8(:,:,2))
  call heltable([4,2,8], n3(:,48), t3x8(:,:,3))
  call heltable([1,4,2,8], n4(:,134), t4x8(:,:,124))
  call heltable([1,4,2,8], n4(:,135), t4x8(:,:,125))
  call heltable([2,4,8], n3(:,49), t3x8(:,:,4))
  call heltable([1,4,2,8], n4(:,136), t4x8(:,:,126))
  call heltable([1,2,4,8], n4(:,137), t4x8(:,:,127))
  call heltable([2,4,8], n3(:,50), t3x8(:,:,5))
  call heltable([1,2,4,8], n4(:,138), t4x8(:,:,128))
  call heltable([2,4,8], n3(:,51), t3x8(:,:,6))
  call heltable([1,2,4,8], n4(:,139), t4x8(:,:,129))
  call heltable([2,4,8], n3(:,52), t3x8(:,:,7))
  call heltable([2,4,8], n3(:,53), t3x8(:,:,8))
  call heltable([2,4,8], n3(:,54), t3x8(:,:,9))
  call heltable([1,2,4,8], n4(:,140), t4x8(:,:,130))
  call heltable([4,2,8], n3(:,55), t3x8(:,:,10))
  call heltable([4,2,8], n3(:,56), t3x8(:,:,11))
  call heltable([1,2,4,8], n4(:,141), t4x8(:,:,131))
  call heltable([2,4,8], n3(:,57), t3x8(:,:,12))
  call heltable([1,2,4,8], n4(:,142), t4x8(:,:,132))
  call heltable([2,4,8], n3(:,58), t3x8(:,:,13))
  call heltable([2,4,8], n3(:,59), t3x8(:,:,14))
  call heltable([2,4,8], n3(:,60), t3x8(:,:,15))
  call heltable([4,2,8], n3(:,61), t3x8(:,:,16))
  call heltable([2,4,8], n3(:,62), t3x8(:,:,17))
  call heltable([2,4,8], n3(:,63), t3x8(:,:,18))
  call heltable([1,2,4,8], n4(:,143), t4x8(:,:,133))
  call heltable([4,2,8], n3(:,64), t3x8(:,:,19))
  call heltable([4,2,8], n3(:,65), t3x8(:,:,20))
  call heltable([1,2,4,8], n4(:,144), t4x8(:,:,134))
  call heltable([2,4,8], n3(:,66), t3x8(:,:,21))
  call heltable([1,2,4,8], n4(:,145), t4x8(:,:,135))
  call heltable([2,4,8], n3(:,67), t3x8(:,:,22))
  call heltable([2,4,8], n3(:,68), t3x8(:,:,23))
  call heltable([2,4,8], n3(:,69), t3x8(:,:,24))
  call heltable([4,2,8], n3(:,70), t3x8(:,:,25))
  call heltable([2,4,8], n3(:,71), t3x8(:,:,26))
  call heltable([2,4,8], n3(:,72), t3x8(:,:,27))
  call heltable([4,2,8], n3(:,73), t3x8(:,:,28))
  call heltable([2,4,8], n3(:,74), t3x8(:,:,29))
  call heltable([2,4,8], n3(:,75), t3x8(:,:,30))
  call heltable([2,4,8], n3(:,76), t3x8(:,:,31))
  call heltable([2,4,8], n3(:,77), t3x8(:,:,32))
  call heltable([2,4,8], n3(:,78), t3x8(:,:,33))
  call heltable([2,4,8], n3(:,79), t3x8(:,:,34))
  call heltable([2,4,8], n3(:,80), t3x8(:,:,35))
  call heltable([2,4,8], n3(:,81), t3x8(:,:,36))
  call heltable([4,2,8], n3(:,82), t3x8(:,:,37))
  call heltable([4,2,8], n3(:,83), t3x8(:,:,38))
  call heltable([4,2,8], n3(:,84), t3x8(:,:,39))
  call heltable([2,4,8], n3(:,85), t3x8(:,:,40))
  call heltable([2,4,8], n3(:,86), t3x8(:,:,41))
  call heltable([4,2,8], n3(:,87), t3x8(:,:,42))
  call heltable([4,2,8], n3(:,88), t3x8(:,:,43))
  call heltable([2,4,8], n3(:,89), t3x8(:,:,44))
  call heltable([4,2,8], n3(:,90), t3x8(:,:,45))
  call heltable([4,8,32], n3(:,91), t3x32(:,:,1))
  call heltable([4,8,32], n3(:,92), t3x32(:,:,2))
  call heltable([4,8,32], n3(:,93), t3x32(:,:,3))
  call heltable([4,8,32], n3(:,94), t3x32(:,:,4))
  call heltable([4,8,32], n3(:,95), t3x32(:,:,5))
  call heltable([4,8,32], n3(:,96), t3x32(:,:,6))
  call heltable([4,8,32], n3(:,97), t3x32(:,:,7))
  call heltable([4,8,32], n3(:,98), t3x32(:,:,8))
  call heltable([4,8,32], n3(:,99), t3x32(:,:,9))
  call heltable([4,8,32], n3(:,100), t3x32(:,:,10))
  call heltable([4,8,32], n3(:,101), t3x32(:,:,11))
  call heltable([4,8,32], n3(:,102), t3x32(:,:,12))
  call heltable([4,8,32], n3(:,103), t3x32(:,:,13))
  call heltable([4,8,32], n3(:,104), t3x32(:,:,14))
  call heltable([4,8,32], n3(:,105), t3x32(:,:,15))
  call heltable([4,8,32], n3(:,106), t3x32(:,:,16))
  call heltable([4,8,32], n3(:,107), t3x32(:,:,17))
  call heltable([4,8,32], n3(:,108), t3x32(:,:,18))
  call heltable([4,8,32], n3(:,109), t3x32(:,:,19))
  call heltable([4,8,32], n3(:,110), t3x32(:,:,20))
  call heltable([4,8,32], n3(:,111), t3x32(:,:,21))
  call heltable([4,8,32], n3(:,112), t3x32(:,:,22))
  call heltable([4,8,32], n3(:,113), t3x32(:,:,23))
  call heltable([4,8,32], n3(:,114), t3x32(:,:,24))
  call heltable([4,8,32], n3(:,115), t3x32(:,:,25))
  call heltable([4,8,32], n3(:,116), t3x32(:,:,26))
  call heltable([4,8,32], n3(:,117), t3x32(:,:,27))
  call heltable([4,8,32], n3(:,118), t3x32(:,:,28))
  call heltable([4,8,32], n3(:,119), t3x32(:,:,29))
  call heltable([4,8,32], n3(:,120), t3x32(:,:,30))
  call heltable([4,8,32], n3(:,121), t3x32(:,:,31))
  call heltable([4,8,32], n3(:,122), t3x32(:,:,32))
  call heltable([4,8,32], n3(:,123), t3x32(:,:,33))
  call heltable([4,8,32], n3(:,124), t3x32(:,:,34))
  call heltable([4,8,32], n3(:,125), t3x32(:,:,35))
  call heltable([4,8,32], n3(:,126), t3x32(:,:,36))
  call heltable([4,8,32], n3(:,127), t3x32(:,:,37))
  call heltable([4,8,32], n3(:,128), t3x32(:,:,38))
  call heltable([4,8,32], n3(:,129), t3x32(:,:,39))
  call heltable([4,8,32], n3(:,130), t3x32(:,:,40))
  call heltable([4,8,32], n3(:,131), t3x32(:,:,41))
  call heltable([4,8,32], n3(:,132), t3x32(:,:,42))
  call heltable([4,8,32], n3(:,133), t3x32(:,:,43))
  call heltable([4,8,32], n3(:,134), t3x32(:,:,44))
  call heltable([4,8,32], n3(:,135), t3x32(:,:,45))
  call heltable([4,8,32], n3(:,136), t3x32(:,:,46))
  call heltable([4,8,32], n3(:,137), t3x32(:,:,47))
  call heltable([4,8,32], n3(:,138), t3x32(:,:,48))
  call heltable([4,8,32], n3(:,139), t3x32(:,:,49))
  call heltable([4,8,32], n3(:,140), t3x32(:,:,50))
  call heltable([4,8,32], n3(:,141), t3x32(:,:,51))
  call heltable([4,8,32], n3(:,142), t3x32(:,:,52))
  call heltable([4,8,32], n3(:,143), t3x32(:,:,53))
  call heltable([4,8,32], n3(:,144), t3x32(:,:,54))
  call heltable([4,8,32], n3(:,145), t3x32(:,:,55))
  call heltable([4,8,32], n3(:,146), t3x32(:,:,56))
  call heltable([4,8,32], n3(:,147), t3x32(:,:,57))
  call heltable([4,8,32], n3(:,148), t3x32(:,:,58))
  call heltable([4,8,32], n3(:,149), t3x32(:,:,59))
  call heltable([4,8,32], n3(:,150), t3x32(:,:,60))
  call heltable([4,8,32], n3(:,151), t3x32(:,:,61))
  call heltable([4,8,32], n3(:,152), t3x32(:,:,62))
  call heltable([4,8,32], n3(:,153), t3x32(:,:,63))
  call heltable([4,8,32], n3(:,154), t3x32(:,:,64))
  call heltable([4,8,32], n3(:,155), t3x32(:,:,65))
  call heltable([4,8,32], n3(:,156), t3x32(:,:,66))
  call heltable([4,8,32], n3(:,157), t3x32(:,:,67))
  call heltable([4,8,32], n3(:,158), t3x32(:,:,68))
  call heltable([4,8,32], n3(:,159), t3x32(:,:,69))
  call heltable([8,4,32], n3(:,160), t3x32(:,:,70))
  call heltable([8,4,32], n3(:,161), t3x32(:,:,71))
  call heltable([8,4,32], n3(:,162), t3x32(:,:,72))
  call heltable([4,8,32], n3(:,163), t3x32(:,:,73))
  call heltable([4,8,32], n3(:,164), t3x32(:,:,74))
  call heltable([4,8,32], n3(:,165), t3x32(:,:,75))
  call heltable([4,8,32], n3(:,166), t3x32(:,:,76))
  call heltable([4,8,32], n3(:,167), t3x32(:,:,77))
  call heltable([4,8,32], n3(:,168), t3x32(:,:,78))
  call heltable([8,4,32], n3(:,169), t3x32(:,:,79))
  call heltable([8,4,32], n3(:,170), t3x32(:,:,80))
  call heltable([8,4,32], n3(:,171), t3x32(:,:,81))
  call heltable([4,8,32], n3(:,172), t3x32(:,:,82))
  call heltable([4,8,32], n3(:,173), t3x32(:,:,83))
  call heltable([4,8,32], n3(:,174), t3x32(:,:,84))
  call heltable([8,4,32], n3(:,175), t3x32(:,:,85))
  call heltable([8,4,32], n3(:,176), t3x32(:,:,86))
  call heltable([8,4,32], n3(:,177), t3x32(:,:,87))
  call heltable([8,4,32], n3(:,178), t3x32(:,:,88))
  call heltable([8,4,32], n3(:,179), t3x32(:,:,89))
  call heltable([8,4,32], n3(:,180), t3x32(:,:,90))
  call heltable([4,8,32], n3(:,181), t3x32(:,:,91))
  call heltable([4,8,32], n3(:,182), t3x32(:,:,92))
  call heltable([4,8,32], n3(:,183), t3x32(:,:,93))
  call heltable([4,8,32], n3(:,184), t3x32(:,:,94))
  call heltable([4,8,32], n3(:,185), t3x32(:,:,95))
  call heltable([4,8,32], n3(:,186), t3x32(:,:,96))
  call heltable([4,8,32], n3(:,187), t3x32(:,:,97))
  call heltable([4,8,32], n3(:,188), t3x32(:,:,98))
  call heltable([4,8,32], n3(:,189), t3x32(:,:,99))
  call heltable([8,4,32], n3(:,190), t3x32(:,:,100))
  call heltable([8,4,32], n3(:,191), t3x32(:,:,101))
  call heltable([8,4,32], n3(:,192), t3x32(:,:,102))
  call heltable([4,8,32], n3(:,193), t3x32(:,:,103))
  call heltable([4,8,32], n3(:,194), t3x32(:,:,104))
  call heltable([4,8,32], n3(:,195), t3x32(:,:,105))
  call heltable([4,8,32], n3(:,196), t3x32(:,:,106))
  call heltable([4,8,32], n3(:,197), t3x32(:,:,107))
  call heltable([4,8,32], n3(:,198), t3x32(:,:,108))
  call heltable([4,8,32], n3(:,199), t3x32(:,:,109))
  call heltable([4,8,32], n3(:,200), t3x32(:,:,110))
  call heltable([4,8,32], n3(:,201), t3x32(:,:,111))
  call heltable([8,4,32], n3(:,202), t3x32(:,:,112))
  call heltable([8,4,32], n3(:,203), t3x32(:,:,113))
  call heltable([8,4,32], n3(:,204), t3x32(:,:,114))
  call heltable([4,8,32], n3(:,205), t3x32(:,:,115))
  call heltable([4,8,32], n3(:,206), t3x32(:,:,116))
  call heltable([4,8,32], n3(:,207), t3x32(:,:,117))
  call heltable([4,8,32], n3(:,208), t3x32(:,:,118))
  call heltable([4,8,32], n3(:,209), t3x32(:,:,119))
  call heltable([4,8,32], n3(:,210), t3x32(:,:,120))
  call heltable([8,4,32], n3(:,211), t3x32(:,:,121))
  call heltable([8,4,32], n3(:,212), t3x32(:,:,122))
  call heltable([8,4,32], n3(:,213), t3x32(:,:,123))
  call heltable([4,8,32], n3(:,214), t3x32(:,:,124))
  call heltable([4,8,32], n3(:,215), t3x32(:,:,125))
  call heltable([4,8,32], n3(:,216), t3x32(:,:,126))
  call heltable([4,8,32], n3(:,217), t3x32(:,:,127))
  call heltable([4,8,32], n3(:,218), t3x32(:,:,128))
  call heltable([4,8,32], n3(:,219), t3x32(:,:,129))
  call heltable([8,4,32], n3(:,220), t3x32(:,:,130))
  call heltable([8,4,32], n3(:,221), t3x32(:,:,131))
  call heltable([8,4,32], n3(:,222), t3x32(:,:,132))
  call heltable([4,8,32], n3(:,223), t3x32(:,:,133))
  call heltable([4,8,32], n3(:,224), t3x32(:,:,134))
  call heltable([4,8,32], n3(:,225), t3x32(:,:,135))
  call heltable([8,4,32], n3(:,226), t3x32(:,:,136))
  call heltable([8,4,32], n3(:,227), t3x32(:,:,137))
  call heltable([8,4,32], n3(:,228), t3x32(:,:,138))
  call heltable([4,8,32], n3(:,229), t3x32(:,:,139))
  call heltable([4,8,32], n3(:,230), t3x32(:,:,140))
  call heltable([4,8,32], n3(:,231), t3x32(:,:,141))
  call heltable([8,4,32], n3(:,232), t3x32(:,:,142))
  call heltable([8,4,32], n3(:,233), t3x32(:,:,143))
  call heltable([8,4,32], n3(:,234), t3x32(:,:,144))
  call heltable([8,4,32], n3(:,235), t3x32(:,:,145))
  call heltable([8,4,32], n3(:,236), t3x32(:,:,146))
  call heltable([8,4,32], n3(:,237), t3x32(:,:,147))
  call heltable([8,4,32], n3(:,238), t3x32(:,:,148))
  call heltable([8,4,32], n3(:,239), t3x32(:,:,149))
  call heltable([8,4,32], n3(:,240), t3x32(:,:,150))
  call heltable([4,8,32], n3(:,241), t3x32(:,:,151))
  call heltable([4,8,32], n3(:,242), t3x32(:,:,152))
  call heltable([4,8,32], n3(:,243), t3x32(:,:,153))
  call heltable([4,8,32], n3(:,244), t3x32(:,:,154))
  call heltable([4,8,32], n3(:,245), t3x32(:,:,155))
  call heltable([4,8,32], n3(:,246), t3x32(:,:,156))
  call heltable([8,4,32], n3(:,247), t3x32(:,:,157))
  call heltable([8,4,32], n3(:,248), t3x32(:,:,158))
  call heltable([8,4,32], n3(:,249), t3x32(:,:,159))
  call heltable([4,8,32], n3(:,250), t3x32(:,:,160))
  call heltable([4,8,32], n3(:,251), t3x32(:,:,161))
  call heltable([4,8,32], n3(:,252), t3x32(:,:,162))
  call heltable([8,4,32], n3(:,253), t3x32(:,:,163))
  call heltable([8,4,32], n3(:,254), t3x32(:,:,164))
  call heltable([8,4,32], n3(:,255), t3x32(:,:,165))
  call heltable([8,4,32], n3(:,256), t3x32(:,:,166))
  call heltable([8,4,32], n3(:,257), t3x32(:,:,167))
  call heltable([8,4,32], n3(:,258), t3x32(:,:,168))
  call heltable([4,8,32], n3(:,259), t3x32(:,:,169))
  call heltable([4,8,32], n3(:,260), t3x32(:,:,170))
  call heltable([4,8,32], n3(:,261), t3x32(:,:,171))
  call heltable([4,8,32], n3(:,262), t3x32(:,:,172))
  call heltable([4,8,32], n3(:,263), t3x32(:,:,173))
  call heltable([4,8,32], n3(:,264), t3x32(:,:,174))
  call heltable([8,4,32], n3(:,265), t3x32(:,:,175))
  call heltable([8,4,32], n3(:,266), t3x32(:,:,176))
  call heltable([8,4,32], n3(:,267), t3x32(:,:,177))
  call heltable([4,8,32], n3(:,268), t3x32(:,:,178))
  call heltable([4,8,32], n3(:,269), t3x32(:,:,179))
  call heltable([4,8,32], n3(:,270), t3x32(:,:,180))
  call heltable([4,8,32], n3(:,271), t3x32(:,:,181))
  call heltable([4,8,32], n3(:,272), t3x32(:,:,182))
  call heltable([4,8,32], n3(:,273), t3x32(:,:,183))
  call heltable([8,4,32], n3(:,274), t3x32(:,:,184))
  call heltable([8,4,32], n3(:,275), t3x32(:,:,185))
  call heltable([8,4,32], n3(:,276), t3x32(:,:,186))
  call heltable([4,8,32], n3(:,277), t3x32(:,:,187))
  call heltable([4,8,32], n3(:,278), t3x32(:,:,188))
  call heltable([4,8,32], n3(:,279), t3x32(:,:,189))
  call heltable([8,4,32], n3(:,280), t3x32(:,:,190))
  call heltable([8,4,32], n3(:,281), t3x32(:,:,191))
  call heltable([8,4,32], n3(:,282), t3x32(:,:,192))
  call heltable([4,8,32], n3(:,283), t3x32(:,:,193))
  call heltable([4,8,32], n3(:,284), t3x32(:,:,194))
  call heltable([4,8,32], n3(:,285), t3x32(:,:,195))
  call heltable([8,4,32], n3(:,286), t3x32(:,:,196))
  call heltable([8,4,32], n3(:,287), t3x32(:,:,197))
  call heltable([8,4,32], n3(:,288), t3x32(:,:,198))
  call heltable([8,4,32], n3(:,289), t3x32(:,:,199))
  call heltable([8,4,32], n3(:,290), t3x32(:,:,200))
  call heltable([8,4,32], n3(:,291), t3x32(:,:,201))
  call heltable([8,4,32], n3(:,292), t3x32(:,:,202))
  call heltable([8,4,32], n3(:,293), t3x32(:,:,203))
  call heltable([8,4,32], n3(:,294), t3x32(:,:,204))
  call heltable([4,8,32], n3(:,295), t3x32(:,:,205))
  call heltable([4,8,32], n3(:,296), t3x32(:,:,206))
  call heltable([4,8,32], n3(:,297), t3x32(:,:,207))
  call heltable([8,4,32], n3(:,298), t3x32(:,:,208))
  call heltable([8,4,32], n3(:,299), t3x32(:,:,209))
  call heltable([8,4,32], n3(:,300), t3x32(:,:,210))
  call heltable([8,4,32], n3(:,301), t3x32(:,:,211))
  call heltable([8,4,32], n3(:,302), t3x32(:,:,212))
  call heltable([8,4,32], n3(:,303), t3x32(:,:,213))
  call heltable([4,8,32], n3(:,304), t3x32(:,:,214))
  call heltable([4,8,32], n3(:,305), t3x32(:,:,215))
  call heltable([4,8,32], n3(:,306), t3x32(:,:,216))
  call heltable([8,4,32], n3(:,307), t3x32(:,:,217))
  call heltable([8,4,32], n3(:,308), t3x32(:,:,218))
  call heltable([8,4,32], n3(:,309), t3x32(:,:,219))
  call heltable([4,8,32], n3(:,310), t3x32(:,:,220))
  call heltable([4,8,32], n3(:,311), t3x32(:,:,221))
  call heltable([4,8,32], n3(:,312), t3x32(:,:,222))
  call heltable([8,4,32], n3(:,313), t3x32(:,:,223))
  call heltable([8,4,32], n3(:,314), t3x32(:,:,224))
  call heltable([8,4,32], n3(:,315), t3x32(:,:,225))
  call heltable([8,4,32], n3(:,316), t3x32(:,:,226))
  call heltable([8,4,32], n3(:,317), t3x32(:,:,227))
  call heltable([8,4,32], n3(:,318), t3x32(:,:,228))
  call heltable([8,4,32], n3(:,319), t3x32(:,:,229))
  call heltable([8,4,32], n3(:,320), t3x32(:,:,230))
  call heltable([8,4,32], n3(:,321), t3x32(:,:,231))
  call heltable([8,4,32], n3(:,322), t3x32(:,:,232))
  call heltable([8,4,32], n3(:,323), t3x32(:,:,233))
  call heltable([8,4,32], n3(:,324), t3x32(:,:,234))
  call heltable([8,4,32], n3(:,325), t3x32(:,:,235))
  call heltable([8,4,32], n3(:,326), t3x32(:,:,236))
  call heltable([8,4,32], n3(:,327), t3x32(:,:,237))
  call heltable([8,4,32], n3(:,328), t3x32(:,:,238))
  call heltable([8,4,32], n3(:,329), t3x32(:,:,239))
  call heltable([8,4,32], n3(:,330), t3x32(:,:,240))
  call heltable([4,8,32], n3(:,331), t3x32(:,:,241))
  call heltable([4,8,32], n3(:,332), t3x32(:,:,242))
  call heltable([4,8,32], n3(:,333), t3x32(:,:,243))
  call heltable([4,8,32], n3(:,334), t3x32(:,:,244))
  call heltable([4,8,32], n3(:,335), t3x32(:,:,245))
  call heltable([4,8,32], n3(:,336), t3x32(:,:,246))
  call heltable([4,8,32], n3(:,337), t3x32(:,:,247))
  call heltable([4,8,32], n3(:,338), t3x32(:,:,248))
  call heltable([4,8,32], n3(:,339), t3x32(:,:,249))
  call heltable([4,8,32], n3(:,340), t3x32(:,:,250))
  call heltable([4,8,32], n3(:,341), t3x32(:,:,251))
  call heltable([4,8,32], n3(:,342), t3x32(:,:,252))
  call heltable([4,8,32], n3(:,343), t3x32(:,:,253))
  call heltable([4,8,32], n3(:,344), t3x32(:,:,254))
  call heltable([4,8,32], n3(:,345), t3x32(:,:,255))
  call heltable([4,8,32], n3(:,346), t3x32(:,:,256))
  call heltable([4,8,32], n3(:,347), t3x32(:,:,257))
  call heltable([4,8,32], n3(:,348), t3x32(:,:,258))
  call heltable([4,8,32], n3(:,349), t3x32(:,:,259))
  call heltable([4,8,32], n3(:,350), t3x32(:,:,260))
  call heltable([4,8,32], n3(:,351), t3x32(:,:,261))
  call heltable([4,8,32], n3(:,352), t3x32(:,:,262))
  call heltable([4,8,32], n3(:,353), t3x32(:,:,263))
  call heltable([4,8,32], n3(:,354), t3x32(:,:,264))
  call heltable([4,8,32], n3(:,355), t3x32(:,:,265))
  call heltable([4,8,32], n3(:,356), t3x32(:,:,266))
  call heltable([4,8,32], n3(:,357), t3x32(:,:,267))
  call heltable([4,8,32], n3(:,358), t3x32(:,:,268))
  call heltable([4,8,32], n3(:,359), t3x32(:,:,269))
  call heltable([4,8,32], n3(:,360), t3x32(:,:,270))
  call heltable([4,8,32], n3(:,361), t3x32(:,:,271))
  call heltable([4,8,32], n3(:,362), t3x32(:,:,272))
  call heltable([4,8,32], n3(:,363), t3x32(:,:,273))
  call heltable([4,8,32], n3(:,364), t3x32(:,:,274))
  call heltable([4,8,32], n3(:,365), t3x32(:,:,275))
  call heltable([4,8,32], n3(:,366), t3x32(:,:,276))
  call heltable([4,8,32], n3(:,367), t3x32(:,:,277))
  call heltable([4,8,32], n3(:,368), t3x32(:,:,278))
  call heltable([4,8,32], n3(:,369), t3x32(:,:,279))
  call heltable([4,8,32], n3(:,370), t3x32(:,:,280))
  call heltable([4,8,32], n3(:,371), t3x32(:,:,281))
  call heltable([4,8,32], n3(:,372), t3x32(:,:,282))
  call heltable([4,8,32], n3(:,373), t3x32(:,:,283))
  call heltable([4,8,32], n3(:,374), t3x32(:,:,284))
  call heltable([4,8,32], n3(:,375), t3x32(:,:,285))
  call heltable([4,8,32], n3(:,376), t3x32(:,:,286))
  call heltable([4,8,32], n3(:,377), t3x32(:,:,287))
  call heltable([4,8,32], n3(:,378), t3x32(:,:,288))
  call heltable([4,8,32], n3(:,379), t3x32(:,:,289))
  call heltable([4,8,32], n3(:,380), t3x32(:,:,290))
  call heltable([4,8,32], n3(:,381), t3x32(:,:,291))
  call heltable([4,8,32], n3(:,382), t3x32(:,:,292))
  call heltable([4,8,32], n3(:,383), t3x32(:,:,293))
  call heltable([4,8,32], n3(:,384), t3x32(:,:,294))
  call heltable([4,8,32], n3(:,385), t3x32(:,:,295))
  call heltable([4,8,32], n3(:,386), t3x32(:,:,296))
  call heltable([4,8,32], n3(:,387), t3x32(:,:,297))
  call heltable([4,8,32], n3(:,388), t3x32(:,:,298))
  call heltable([4,8,32], n3(:,389), t3x32(:,:,299))
  call heltable([4,8,32], n3(:,390), t3x32(:,:,300))
  call heltable([4,8,32], n3(:,391), t3x32(:,:,301))
  call heltable([4,8,32], n3(:,392), t3x32(:,:,302))
  call heltable([4,8,32], n3(:,393), t3x32(:,:,303))
  call heltable([4,8,32], n3(:,394), t3x32(:,:,304))
  call heltable([4,8,32], n3(:,395), t3x32(:,:,305))
  call heltable([4,8,32], n3(:,396), t3x32(:,:,306))
  call heltable([4,8,32], n3(:,397), t3x32(:,:,307))
  call heltable([4,8,32], n3(:,398), t3x32(:,:,308))
  call heltable([4,8,32], n3(:,399), t3x32(:,:,309))
  call heltable([4,8,32], n3(:,400), t3x32(:,:,310))
  call heltable([4,8,32], n3(:,401), t3x32(:,:,311))
  call heltable([4,8,32], n3(:,402), t3x32(:,:,312))
  call heltable([4,8,32], n3(:,403), t3x32(:,:,313))
  call heltable([4,8,32], n3(:,404), t3x32(:,:,314))
  call heltable([4,8,32], n3(:,405), t3x32(:,:,315))
  call heltable([4,8,32], n3(:,406), t3x32(:,:,316))
  call heltable([4,8,32], n3(:,407), t3x32(:,:,317))
  call heltable([4,8,32], n3(:,408), t3x32(:,:,318))
  call heltable([4,8,32], n3(:,409), t3x32(:,:,319))
  call heltable([4,8,32], n3(:,410), t3x32(:,:,320))
  call heltable([4,8,32], n3(:,411), t3x32(:,:,321))
  call heltable([4,8,32], n3(:,412), t3x32(:,:,322))
  call heltable([4,8,32], n3(:,413), t3x32(:,:,323))
  call heltable([4,8,32], n3(:,414), t3x32(:,:,324))
  call heltable([4,8,32], n3(:,415), t3x32(:,:,325))
  call heltable([4,8,32], n3(:,416), t3x32(:,:,326))
  call heltable([4,8,32], n3(:,417), t3x32(:,:,327))
  call heltable([4,8,32], n3(:,418), t3x32(:,:,328))
  call heltable([4,8,32], n3(:,419), t3x32(:,:,329))
  call heltable([4,8,32], n3(:,420), t3x32(:,:,330))
  call heltable([4,8,32], n3(:,421), t3x32(:,:,331))
  call heltable([4,8,32], n3(:,422), t3x32(:,:,332))
  call heltable([4,8,32], n3(:,423), t3x32(:,:,333))
  call heltable([4,8,32], n3(:,424), t3x32(:,:,334))
  call heltable([4,8,32], n3(:,425), t3x32(:,:,335))
  call heltable([4,8,32], n3(:,426), t3x32(:,:,336))
  call heltable([4,8,32], n3(:,427), t3x32(:,:,337))
  call heltable([4,8,32], n3(:,428), t3x32(:,:,338))
  call heltable([4,8,32], n3(:,429), t3x32(:,:,339))
  call heltable([4,8,32], n3(:,430), t3x32(:,:,340))
  call heltable([4,8,32], n3(:,431), t3x32(:,:,341))
  call heltable([4,8,32], n3(:,432), t3x32(:,:,342))
  call heltable([4,8,32], n3(:,433), t3x32(:,:,343))
  call heltable([4,8,32], n3(:,434), t3x32(:,:,344))
  call heltable([4,8,32], n3(:,435), t3x32(:,:,345))
  call heltable([4,8,32], n3(:,436), t3x32(:,:,346))
  call heltable([4,8,32], n3(:,437), t3x32(:,:,347))
  call heltable([4,8,32], n3(:,438), t3x32(:,:,348))
  call heltable([4,8,32], n3(:,439), t3x32(:,:,349))
  call heltable([4,8,32], n3(:,440), t3x32(:,:,350))
  call heltable([4,8,32], n3(:,441), t3x32(:,:,351))
  call heltable([4,8,32], n3(:,442), t3x32(:,:,352))
  call heltable([4,8,32], n3(:,443), t3x32(:,:,353))
  call heltable([4,8,32], n3(:,444), t3x32(:,:,354))
  call heltable([4,8,32], n3(:,445), t3x32(:,:,355))
  call heltable([4,8,32], n3(:,446), t3x32(:,:,356))
  call heltable([4,8,32], n3(:,447), t3x32(:,:,357))
  call heltable([4,8,32], n3(:,448), t3x32(:,:,358))
  call heltable([4,8,32], n3(:,449), t3x32(:,:,359))
  call heltable([4,8,32], n3(:,450), t3x32(:,:,360))
  call heltable([4,8,32], n3(:,451), t3x32(:,:,361))
  call heltable([4,8,32], n3(:,452), t3x32(:,:,362))
  call heltable([4,8,32], n3(:,453), t3x32(:,:,363))
  call heltable([4,8,32], n3(:,454), t3x32(:,:,364))
  call heltable([4,8,32], n3(:,455), t3x32(:,:,365))
  call heltable([4,8,32], n3(:,456), t3x32(:,:,366))
  call heltable([4,8,32], n3(:,457), t3x32(:,:,367))
  call heltable([4,8,32], n3(:,458), t3x32(:,:,368))
  call heltable([4,8,32], n3(:,459), t3x32(:,:,369))
  call heltable([4,8,32], n3(:,460), t3x32(:,:,370))
  call heltable([4,8,32], n3(:,461), t3x32(:,:,371))
  call heltable([4,8,32], n3(:,462), t3x32(:,:,372))
  call heltable([4,8,32], n3(:,463), t3x32(:,:,373))
  call heltable([4,8,32], n3(:,464), t3x32(:,:,374))
  call heltable([4,8,32], n3(:,465), t3x32(:,:,375))
  call heltable([4,8,32], n3(:,466), t3x32(:,:,376))
  call heltable([4,8,32], n3(:,467), t3x32(:,:,377))
  call heltable([4,8,32], n3(:,468), t3x32(:,:,378))
  call heltable([4,8,32], n3(:,469), t3x32(:,:,379))
  call heltable([4,8,32], n3(:,470), t3x32(:,:,380))
  call heltable([4,8,32], n3(:,471), t3x32(:,:,381))
  call heltable([4,8,32], n3(:,472), t3x32(:,:,382))
  call heltable([4,8,32], n3(:,473), t3x32(:,:,383))
  call heltable([4,8,32], n3(:,474), t3x32(:,:,384))
  call heltable([4,8,32], n3(:,475), t3x32(:,:,385))
  call heltable([4,8,32], n3(:,476), t3x32(:,:,386))
  call heltable([4,8,32], n3(:,477), t3x32(:,:,387))
  call heltable([4,8,32], n3(:,478), t3x32(:,:,388))
  call heltable([4,8,32], n3(:,479), t3x32(:,:,389))
  call heltable([4,8,32], n3(:,480), t3x32(:,:,390))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_heftpphjj_hggggg_1
