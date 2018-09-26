
module ol_external_pphhjj_ddxssxhhg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_pphhjj_ddxssxhhg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: external_perm_inv_pphhjj_ddxssxhhg_1(7) = &
                     [ (dummy_counter, dummy_counter = 1, 7) ]
  integer, save :: extcomb_perm_pphhjj_ddxssxhhg_1(0:29) = &
                     [ (dummy_counter, dummy_counter = 0, 29) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_pphhjj_ddxssxhhg_1(7) = &
                     [ 1, 2, 3, 4, 5, 5, 6 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_pphhjj_ddxssxhhg_1(7) = &
                     [ 6, 6, 6, 6, 1, 1, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_pphhjj_ddxssxhhg_1 = &
                     72
  integer, save :: channel_number_pphhjj_ddxssxhhg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(7,32) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(32,7)
  integer, save :: POLSEL(7) = 0

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_pphhjj_ddxssxhhg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 7
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_pphhjj_ddxssxhhg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 7
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_pphhjj_ddxssxhhg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_external_decl_/**/DREALKIND, only: n_scatt
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(7)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_pphhjj_ddxssxhhg_1(7)
    external_perm_pphhjj_ddxssxhhg_1 = perm
    do i = 1, 7
      external_perm_inv_pphhjj_ddxssxhhg_1( &
        external_perm_pphhjj_ddxssxhhg_1(i)) = i
      particle_types_perm_pphhjj_ddxssxhhg_1(i) = &
        particle_types_pphhjj_ddxssxhhg_1( &
        external_perm_pphhjj_ddxssxhhg_1(i))
    end do
    do i = 1, 7
      do j = 1, i
        if (external_perm_pphhjj_ddxssxhhg_1(i) >= &
          external_perm_pphhjj_ddxssxhhg_1(j)) then
          ii = external_perm_pphhjj_ddxssxhhg_1(i)
          jj = external_perm_pphhjj_ddxssxhhg_1(j)
        else
          ii = external_perm_pphhjj_ddxssxhhg_1(j)
          jj = external_perm_pphhjj_ddxssxhhg_1(i)
        end if
        extcomb_perm_pphhjj_ddxssxhhg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_pphhjj_ddxssxhhg_1 = 1
    do i = 1, n_scatt
      average_factor_pphhjj_ddxssxhhg_1 = &
        average_factor_pphhjj_ddxssxhhg_1 &
        * average_factors_pphhjj_ddxssxhhg_1( &
        external_perm_pphhjj_ddxssxhhg_1(i))
    end do
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 7
        average_factor_pphhjj_ddxssxhhg_1 = &
          average_factor_pphhjj_ddxssxhhg_1 &
          * factorial(count(particle_types_perm_pphhjj_ddxssxhhg_1(n_scatt+1:7) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_pphhjj_ddxssxhhg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(7)
    integer :: f_perm(7)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_pphhjj_ddxssxhhg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(7)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(7)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rMH_unscaled, rMH_unscaled, rZERO ]
    do i = 1, 7
      m_ex(i) = m_ex_orig(external_perm_pphhjj_ddxssxhhg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_pphhjj_ddxssxhhg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(7)
    real(DREALKIND) :: f_m_ex(7)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_pphhjj_ddxssxhhg_1")
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
      & bind(c,name="ol_rambo_pphhjj_ddxssxhhg_1")
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
  H(:, 1) = [ -1, -1, -1, -1,  0,  0, -1 ]
  H(:, 2) = [ -1, -1, -1, -1,  0,  0,  1 ]
  H(:, 3) = [ -1, -1, -1,  1,  0,  0, -1 ]
  H(:, 4) = [ -1, -1, -1,  1,  0,  0,  1 ]
  H(:, 5) = [ -1, -1,  1, -1,  0,  0, -1 ]
  H(:, 6) = [ -1, -1,  1, -1,  0,  0,  1 ]
  H(:, 7) = [ -1, -1,  1,  1,  0,  0, -1 ]
  H(:, 8) = [ -1, -1,  1,  1,  0,  0,  1 ]
  H(:, 9) = [ -1,  1, -1, -1,  0,  0, -1 ]
  H(:,10) = [ -1,  1, -1, -1,  0,  0,  1 ]
  H(:,11) = [ -1,  1, -1,  1,  0,  0, -1 ]
  H(:,12) = [ -1,  1, -1,  1,  0,  0,  1 ]
  H(:,13) = [ -1,  1,  1, -1,  0,  0, -1 ]
  H(:,14) = [ -1,  1,  1, -1,  0,  0,  1 ]
  H(:,15) = [ -1,  1,  1,  1,  0,  0, -1 ]
  H(:,16) = [ -1,  1,  1,  1,  0,  0,  1 ]
  H(:,17) = [  1, -1, -1, -1,  0,  0, -1 ]
  H(:,18) = [  1, -1, -1, -1,  0,  0,  1 ]
  H(:,19) = [  1, -1, -1,  1,  0,  0, -1 ]
  H(:,20) = [  1, -1, -1,  1,  0,  0,  1 ]
  H(:,21) = [  1, -1,  1, -1,  0,  0, -1 ]
  H(:,22) = [  1, -1,  1, -1,  0,  0,  1 ]
  H(:,23) = [  1, -1,  1,  1,  0,  0, -1 ]
  H(:,24) = [  1, -1,  1,  1,  0,  0,  1 ]
  H(:,25) = [  1,  1, -1, -1,  0,  0, -1 ]
  H(:,26) = [  1,  1, -1, -1,  0,  0,  1 ]
  H(:,27) = [  1,  1, -1,  1,  0,  0, -1 ]
  H(:,28) = [  1,  1, -1,  1,  0,  0,  1 ]
  H(:,29) = [  1,  1,  1, -1,  0,  0, -1 ]
  H(:,30) = [  1,  1,  1, -1,  0,  0,  1 ]
  H(:,31) = [  1,  1,  1,  1,  0,  0, -1 ]
  H(:,32) = [  1,  1,  1,  1,  0,  0,  1 ]

  H_HC(:,7) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 32/1/2) ]
  end subroutine hel_init


  subroutine pol_init(pol) &
      & bind(c,name="ol_f_pol_init_pphhjj_ddxssxhhg_1")
    implicit none
    integer, intent(in) :: pol(7)
    POLSEL = pol
  end subroutine pol_init

end module ol_external_pphhjj_ddxssxhhg_1


module colour_basis_pphhjj_ddxssxhhg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(7) = [1,1,1,1,0,0,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_pphhjj_ddxssxhhg_1")
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
    nhel = 32
  end subroutine tree_colbasis_dim

  subroutine tree_colbasis(basis, powers) &
    & bind(c, name="ol_tree_colbasis_pphhjj_ddxssxhhg_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,2)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([8], [1,1])
#endif
#if 2 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [1,104,4,1,120,4], &
      [3,2])
#endif
  end subroutine tree_colbasis

end module colour_basis_pphhjj_ddxssxhhg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_pphhjj_ddxssxhhg_1(perm)
  use ol_external_pphhjj_ddxssxhhg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(7)
  call set_permutation(perm)
end subroutine set_permutation_pphhjj_ddxssxhhg_1

! **********************************************************************
module ol_heltables_pphhjj_ddxssxhhg_1
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
    H6(1) = [0], &
    H7(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n2(4), n3(3,61), n4(4,2)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x1(2,1,1), t3x4(2,4,16), t3x8(2,8,20), t3x32(2,32,24), t4x4(3,4,2)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(32,7)
  integer,           save :: exthel(32,7)
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
  call heltable([1,1,4,4], n4(:,1), t4x4(:,:,1))
  call heltable([2,2,4], n3(:,4), t3x4(:,:,3))
  n2(2) = 4
  call heltable([2,4,8], n3(:,5), t3x8(:,:,2))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,4))
  call heltable([2,2,4], n3(:,7), t3x4(:,:,5))
  n2(3) = 4
  call heltable([4,2,8], n3(:,8), t3x8(:,:,3))
  call heltable([1,1,4,4], n4(:,2), t4x4(:,:,2))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,6))
  n2(4) = 4
  call heltable([2,4,8], n3(:,10), t3x8(:,:,4))
  call heltable([1,1,1], n3(:,11), t3x1(:,:,1))
  call heltable([1,4,4], n3(:,12), t3x4(:,:,7))
  call heltable([1,4,4], n3(:,13), t3x4(:,:,8))
  call heltable([1,8,8], n3(:,14), t3x8(:,:,5))
  call heltable([1,4,4], n3(:,15), t3x4(:,:,9))
  call heltable([1,8,8], n3(:,16), t3x8(:,:,6))
  call heltable([1,4,4], n3(:,17), t3x4(:,:,10))
  call heltable([1,8,8], n3(:,18), t3x8(:,:,7))
  call heltable([1,4,4], n3(:,19), t3x4(:,:,11))
  call heltable([1,8,8], n3(:,20), t3x8(:,:,8))
  call heltable([1,8,8], n3(:,21), t3x8(:,:,9))
  call heltable([1,8,8], n3(:,22), t3x8(:,:,10))
  call heltable([1,8,8], n3(:,23), t3x8(:,:,11))
  call heltable([1,8,8], n3(:,24), t3x8(:,:,12))
  call heltable([1,4,4], n3(:,25), t3x4(:,:,12))
  call heltable([1,4,4], n3(:,26), t3x4(:,:,13))
  call heltable([1,8,8], n3(:,27), t3x8(:,:,13))
  call heltable([1,4,4], n3(:,28), t3x4(:,:,14))
  call heltable([1,8,8], n3(:,29), t3x8(:,:,14))
  call heltable([1,4,4], n3(:,30), t3x4(:,:,15))
  call heltable([1,8,8], n3(:,31), t3x8(:,:,15))
  call heltable([1,4,4], n3(:,32), t3x4(:,:,16))
  call heltable([1,8,8], n3(:,33), t3x8(:,:,16))
  call heltable([1,8,8], n3(:,34), t3x8(:,:,17))
  call heltable([1,8,8], n3(:,35), t3x8(:,:,18))
  call heltable([1,8,8], n3(:,36), t3x8(:,:,19))
  call heltable([1,8,8], n3(:,37), t3x8(:,:,20))
  call heltable([8,4,32], n3(:,38), t3x32(:,:,1))
  call heltable([4,8,32], n3(:,39), t3x32(:,:,2))
  call heltable([8,4,32], n3(:,40), t3x32(:,:,3))
  call heltable([4,8,32], n3(:,41), t3x32(:,:,4))
  call heltable([8,4,32], n3(:,42), t3x32(:,:,5))
  call heltable([8,4,32], n3(:,43), t3x32(:,:,6))
  call heltable([4,8,32], n3(:,44), t3x32(:,:,7))
  call heltable([4,8,32], n3(:,45), t3x32(:,:,8))
  call heltable([4,8,32], n3(:,46), t3x32(:,:,9))
  call heltable([4,8,32], n3(:,47), t3x32(:,:,10))
  call heltable([4,8,32], n3(:,48), t3x32(:,:,11))
  call heltable([4,8,32], n3(:,49), t3x32(:,:,12))
  call heltable([4,8,32], n3(:,50), t3x32(:,:,13))
  call heltable([4,8,32], n3(:,51), t3x32(:,:,14))
  call heltable([8,4,32], n3(:,52), t3x32(:,:,15))
  call heltable([8,4,32], n3(:,53), t3x32(:,:,16))
  call heltable([4,8,32], n3(:,54), t3x32(:,:,17))
  call heltable([4,8,32], n3(:,55), t3x32(:,:,18))
  call heltable([4,8,32], n3(:,56), t3x32(:,:,19))
  call heltable([4,8,32], n3(:,57), t3x32(:,:,20))
  call heltable([4,8,32], n3(:,58), t3x32(:,:,21))
  call heltable([4,8,32], n3(:,59), t3x32(:,:,22))
  call heltable([4,8,32], n3(:,60), t3x32(:,:,23))
  call heltable([4,8,32], n3(:,61), t3x32(:,:,24))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_pphhjj_ddxssxhhg_1
