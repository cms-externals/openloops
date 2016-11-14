
module ol_external_ppjj_ggggg_1
  implicit none
  integer :: dummy_counter
  ! Permutation and inverse permutation of external particles
  integer, save :: external_perm_ppjj_ggggg_1(5) = &
                     [ (dummy_counter, dummy_counter = 1, 5) ]
  integer, save :: external_perm_inv_ppjj_ggggg_1(5) = &
                     [ (dummy_counter, dummy_counter = 1, 5) ]
  integer, save :: extcomb_perm_ppjj_ggggg_1(0:16) = &
                     [ (dummy_counter, dummy_counter = 0, 16) ]
  ! Particle types (mapping of fields to integers is not fixed!)
  integer, save :: particle_types_ppjj_ggggg_1(5) = &
                     [ 1, 1, 1, 1, 1 ]
  ! Colour and helicity average factors per particle
  integer, save :: average_factors_ppjj_ggggg_1(5) = &
                     [ 16, 16, 16, 16, 16 ]
  ! Average factor; initialised to the identity permutation
  integer, save :: average_factor_ppjj_ggggg_1 = &
                     1536
  integer, save :: channel_number_ppjj_ggggg_1 = -1
  ! external particle helicities
  logical, save :: hel_not_initialised = .true.
  integer, save :: H(5,32) ! H(i,la) = helicity of particle i in configuration la
  integer, save :: H_HC(32,5)

  contains

  subroutine n_external(n) &
      & bind(c,name="ol_f_n_external_ppjj_ggggg_1")
    ! Return the number of external particles
    implicit none
    integer, intent(out) :: n
    n = 5
  end subroutine n_external


  subroutine n_external_c(n) &
      & bind(c,name="ol_n_external_ppjj_ggggg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n
    n = 5
  end subroutine n_external_c


  subroutine set_permutation(perm) &
      & bind(c,name="ol_f_set_permutation_ppjj_ggggg_1")
    use ol_parameters_decl_/**/DREALKIND, only: out_symmetry_on
    use ol_generic, only: factorial
    implicit none
    integer, intent(in) :: perm(5)
    integer :: i, j, ii, jj
    integer :: particle_types_perm_ppjj_ggggg_1(5)
    external_perm_ppjj_ggggg_1 = perm
    do i = 1, 5
      external_perm_inv_ppjj_ggggg_1( &
        external_perm_ppjj_ggggg_1(i)) = i
      particle_types_perm_ppjj_ggggg_1(i) = &
        particle_types_ppjj_ggggg_1( &
        external_perm_ppjj_ggggg_1(i))
    end do
    do i = 1, 5
      do j = 1, i
        if (external_perm_ppjj_ggggg_1(i) >= &
          external_perm_ppjj_ggggg_1(j)) then
          ii = external_perm_ppjj_ggggg_1(i)
          jj = external_perm_ppjj_ggggg_1(j)
        else
          ii = external_perm_ppjj_ggggg_1(j)
          jj = external_perm_ppjj_ggggg_1(i)
        end if
        extcomb_perm_ppjj_ggggg_1((i*(i-1))/2 + j) = (ii*(ii-1))/2 + jj
      end do
    end do
    ! Colour and helicity average factor
    average_factor_ppjj_ggggg_1 = &
      average_factors_ppjj_ggggg_1( &
      external_perm_ppjj_ggggg_1(1)) &
      * average_factors_ppjj_ggggg_1( &
      external_perm_ppjj_ggggg_1(2))
    ! Symmetry factor for outgoing particles
    if (out_symmetry_on /= 0) then
      do i = 1, 5
        average_factor_ppjj_ggggg_1 = &
          average_factor_ppjj_ggggg_1 &
          * factorial(count(particle_types_perm_ppjj_ggggg_1(3:5) == i))
      end do
    end if
  end subroutine set_permutation


  subroutine set_permutation_c(perm) &
      & bind(c,name="ol_set_permutation_ppjj_ggggg_1")
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: perm(5)
    integer :: f_perm(5)
    f_perm = perm
    call set_permutation(f_perm)
  end subroutine set_permutation_c


  subroutine get_masses(m_ex) &
      & bind(c,name="ol_f_get_masses_ppjj_ggggg_1")
    ! Return the masses of the external particles in the current permutation.
    use KIND_TYPES, only: DREALKIND
    use ol_parameters_decl_/**/DREALKIND
    implicit none
    real(DREALKIND), intent(out) :: m_ex(5)
    integer        :: i
    real(DREALKIND) :: m_ex_orig(5)
    ! External particle masses for in the identity permutation
    m_ex_orig = [ rZERO, rZERO, rZERO, rZERO, rZERO ]
    do i = 1, 5
      m_ex(i) = m_ex_orig(external_perm_ppjj_ggggg_1(i))
    end do
  end subroutine get_masses


  subroutine get_masses_c(m_ex) &
      & bind(c,name="ol_get_masses_ppjj_ggggg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: m_ex(5)
    real(DREALKIND) :: f_m_ex(5)
    call get_masses(f_m_ex)
    m_ex = f_m_ex
  end subroutine get_masses_c


  subroutine rambo(sqrt_s, p_rambo) &
      & bind(c,name="ol_f_rambo_ppjj_ggggg_1")
    use KIND_TYPES, only: DREALKIND
    use ol_kinematics_/**/DREALKIND, only: rambo_generic => rambo
    implicit none
    real(DREALKIND), intent(in) :: sqrt_s
    real(DREALKIND), intent(out) :: p_rambo(0:3,5)
    real(DREALKIND) :: m_ex(5)
    call get_masses(m_ex)
    call rambo_generic(sqrt_s, m_ex, p_rambo)
  end subroutine rambo


  subroutine rambo_c(sqrt_s, p_rambo) &
      & bind(c,name="ol_rambo_ppjj_ggggg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in) :: sqrt_s
    real(c_double), intent(out) :: p_rambo(0:3,5)
    real(DREALKIND) :: f_sqrt_s
    real(DREALKIND) :: f_p_rambo(0:3,5)
    f_sqrt_s = sqrt_s
    call rambo(f_sqrt_s, f_p_rambo)
    p_rambo = f_p_rambo
  end subroutine rambo_c


  subroutine hel_init
    implicit none
    integer :: binpos, flip, binco
    hel_not_initialised = .false.
    ! helicity configurations for this process
  H(:, 1) = [ -1, -1, -1, -1, -1 ]
  H(:, 2) = [ -1, -1, -1, -1,  1 ]
  H(:, 3) = [ -1, -1, -1,  1, -1 ]
  H(:, 4) = [ -1, -1, -1,  1,  1 ]
  H(:, 5) = [ -1, -1,  1, -1, -1 ]
  H(:, 6) = [ -1, -1,  1, -1,  1 ]
  H(:, 7) = [ -1, -1,  1,  1, -1 ]
  H(:, 8) = [ -1, -1,  1,  1,  1 ]
  H(:, 9) = [ -1,  1, -1, -1, -1 ]
  H(:,10) = [ -1,  1, -1, -1,  1 ]
  H(:,11) = [ -1,  1, -1,  1, -1 ]
  H(:,12) = [ -1,  1, -1,  1,  1 ]
  H(:,13) = [ -1,  1,  1, -1, -1 ]
  H(:,14) = [ -1,  1,  1, -1,  1 ]
  H(:,15) = [ -1,  1,  1,  1, -1 ]
  H(:,16) = [ -1,  1,  1,  1,  1 ]
  H(:,17) = [  1, -1, -1, -1, -1 ]
  H(:,18) = [  1, -1, -1, -1,  1 ]
  H(:,19) = [  1, -1, -1,  1, -1 ]
  H(:,20) = [  1, -1, -1,  1,  1 ]
  H(:,21) = [  1, -1,  1, -1, -1 ]
  H(:,22) = [  1, -1,  1, -1,  1 ]
  H(:,23) = [  1, -1,  1,  1, -1 ]
  H(:,24) = [  1, -1,  1,  1,  1 ]
  H(:,25) = [  1,  1, -1, -1, -1 ]
  H(:,26) = [  1,  1, -1, -1,  1 ]
  H(:,27) = [  1,  1, -1,  1, -1 ]
  H(:,28) = [  1,  1, -1,  1,  1 ]
  H(:,29) = [  1,  1,  1, -1, -1 ]
  H(:,30) = [  1,  1,  1, -1,  1 ]
  H(:,31) = [  1,  1,  1,  1, -1 ]
  H(:,32) = [  1,  1,  1,  1,  1 ]

  H_HC(:,1) = [ ((((2*(binco-1)+flip)*16+binpos, flip = 0, 1), binpos = 1, 16), binco = 1, 32/16/2) ]
  H_HC(:,2) = [ ((((2*(binco-1)+flip)*8+binpos, flip = 0, 1), binpos = 1, 8), binco = 1, 32/8/2) ]
  H_HC(:,3) = [ ((((2*(binco-1)+flip)*4+binpos, flip = 0, 1), binpos = 1, 4), binco = 1, 32/4/2) ]
  H_HC(:,4) = [ ((((2*(binco-1)+flip)*2+binpos, flip = 0, 1), binpos = 1, 2), binco = 1, 32/2/2) ]
  H_HC(:,5) = [ ((((2*(binco-1)+flip)*1+binpos, flip = 0, 1), binpos = 1, 1), binco = 1, 32/1/2) ]
  end subroutine hel_init

end module ol_external_ppjj_ggggg_1


module colour_basis_ppjj_ggggg_1
  implicit none
  ! tree colour basis
  integer, save :: extcolours(5) = [2,2,2,2,2]
  contains

  pure subroutine tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel) &
    & bind(c, name="ol_tree_colbasis_dim_ppjj_ggggg_1")
    implicit none
    ! colour representation of external particles: 0=neutral, 1=fundamental, 2=adjoint
    integer, intent(out) :: extcols(5)
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
    & bind(c, name="ol_tree_colbasis_ppjj_ggggg_1")
    implicit none
    integer, intent(out) :: powers(1,1)
    integer, intent(out) :: basis(3,24)
#if 1 > 0
    ! selected powers for each selected coupling
    powers = reshape([0], [1,1])
#endif
#if 24 > 0
    ! tree colour basis: [[composition_number, permutation_number, *coupling_powers], ...]
    basis = reshape( &
      [2,1,0,2,2,0,2,3,0,2,4,0,2,5,0,2,6,0,2,7,0,2,8,0,2,9,0,2,10,0,2,11,0,2,12,0,2,13,0,2,14,0,2,15,0,2,16,0,2,17,0,2,18,0,2 &
      ,19,0,2,20,0,2,21,0,2,22,0,2,23,0,2,24,0], &
      [3,24])
#endif
  end subroutine tree_colbasis

end module colour_basis_ppjj_ggggg_1


! Only for compatibility with the Sherpa interface

subroutine set_permutation_ppjj_ggggg_1(perm)
  use ol_external_ppjj_ggggg_1, only: set_permutation
  implicit none
  integer, intent(in) :: perm(5)
  call set_permutation(perm)
end subroutine set_permutation_ppjj_ggggg_1

! **********************************************************************
module ol_heltables_ppjj_ggggg_1
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
    H5(2) = [-1,1]

  ! number of helicity states for wave functions returned by a propagator call: n2(sz)
  ! number of helicity states for wave functions in a v-point vertex call (v >= 3)
  ! or a contraction (v = 3): n<v>(v,sz)
  integer(intkind2), save :: n3(3,70), n4(4,30)

  ! helicity tables used in the construction of the h helicity states of a wave function (amplitude)
  ! from an v-point vertex (contraction): t<v>x<h>(v-1,h,sz)
  integer(intkind2), save :: t3x4(2,4,10), t3x8(2,8,15), t3x32(2,32,45), t4x8(3,8,30)

  ! change of global-helicity state resulting from flip of individual-particle helicity
  integer(intkind2), save :: eflip(32,5)
  integer,           save :: exthel(32,5)
  integer,           save :: firstpol(5)

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
  call heltable([2,2,2,8], n4(:,1), t4x8(:,:,1))
  call heltable([2,2,2,8], n4(:,2), t4x8(:,:,2))
  call heltable([2,2,2,8], n4(:,3), t4x8(:,:,3))
  call heltable([2,2,4], n3(:,2), t3x4(:,:,2))
  call heltable([2,2,2,8], n4(:,4), t4x8(:,:,4))
  call heltable([2,2,2,8], n4(:,5), t4x8(:,:,5))
  call heltable([2,2,2,8], n4(:,6), t4x8(:,:,6))
  call heltable([2,2,4], n3(:,3), t3x4(:,:,3))
  call heltable([2,2,2,8], n4(:,7), t4x8(:,:,7))
  call heltable([2,2,2,8], n4(:,8), t4x8(:,:,8))
  call heltable([2,2,2,8], n4(:,9), t4x8(:,:,9))
  call heltable([2,2,4], n3(:,4), t3x4(:,:,4))
  call heltable([2,2,2,8], n4(:,10), t4x8(:,:,10))
  call heltable([2,2,2,8], n4(:,11), t4x8(:,:,11))
  call heltable([2,2,2,8], n4(:,12), t4x8(:,:,12))
  call heltable([2,2,4], n3(:,5), t3x4(:,:,5))
  call heltable([2,2,2,8], n4(:,13), t4x8(:,:,13))
  call heltable([2,2,2,8], n4(:,14), t4x8(:,:,14))
  call heltable([2,2,2,8], n4(:,15), t4x8(:,:,15))
  call heltable([2,2,4], n3(:,6), t3x4(:,:,6))
  call heltable([2,2,2,8], n4(:,16), t4x8(:,:,16))
  call heltable([2,2,2,8], n4(:,17), t4x8(:,:,17))
  call heltable([2,2,2,8], n4(:,18), t4x8(:,:,18))
  call heltable([2,2,4], n3(:,7), t3x4(:,:,7))
  call heltable([2,2,2,8], n4(:,19), t4x8(:,:,19))
  call heltable([2,2,2,8], n4(:,20), t4x8(:,:,20))
  call heltable([2,2,2,8], n4(:,21), t4x8(:,:,21))
  call heltable([2,2,4], n3(:,8), t3x4(:,:,8))
  call heltable([2,2,2,8], n4(:,22), t4x8(:,:,22))
  call heltable([2,2,2,8], n4(:,23), t4x8(:,:,23))
  call heltable([2,2,2,8], n4(:,24), t4x8(:,:,24))
  call heltable([2,2,4], n3(:,9), t3x4(:,:,9))
  call heltable([2,2,2,8], n4(:,25), t4x8(:,:,25))
  call heltable([2,2,2,8], n4(:,26), t4x8(:,:,26))
  call heltable([2,2,2,8], n4(:,27), t4x8(:,:,27))
  call heltable([2,2,4], n3(:,10), t3x4(:,:,10))
  call heltable([2,2,2,8], n4(:,28), t4x8(:,:,28))
  call heltable([2,2,2,8], n4(:,29), t4x8(:,:,29))
  call heltable([2,2,2,8], n4(:,30), t4x8(:,:,30))
  call heltable([4,2,8], n3(:,11), t3x8(:,:,1))
  call heltable([4,2,8], n3(:,12), t3x8(:,:,2))
  call heltable([4,2,8], n3(:,13), t3x8(:,:,3))
  call heltable([4,2,8], n3(:,14), t3x8(:,:,4))
  call heltable([4,2,8], n3(:,15), t3x8(:,:,5))
  call heltable([2,4,8], n3(:,16), t3x8(:,:,6))
  call heltable([4,2,8], n3(:,17), t3x8(:,:,7))
  call heltable([2,4,8], n3(:,18), t3x8(:,:,8))
  call heltable([2,4,8], n3(:,19), t3x8(:,:,9))
  call heltable([2,4,8], n3(:,20), t3x8(:,:,10))
  call heltable([2,4,8], n3(:,21), t3x8(:,:,11))
  call heltable([2,4,8], n3(:,22), t3x8(:,:,12))
  call heltable([2,4,8], n3(:,23), t3x8(:,:,13))
  call heltable([2,4,8], n3(:,24), t3x8(:,:,14))
  call heltable([2,4,8], n3(:,25), t3x8(:,:,15))
  call heltable([4,8,32], n3(:,26), t3x32(:,:,1))
  call heltable([4,8,32], n3(:,27), t3x32(:,:,2))
  call heltable([4,8,32], n3(:,28), t3x32(:,:,3))
  call heltable([4,8,32], n3(:,29), t3x32(:,:,4))
  call heltable([4,8,32], n3(:,30), t3x32(:,:,5))
  call heltable([4,8,32], n3(:,31), t3x32(:,:,6))
  call heltable([4,8,32], n3(:,32), t3x32(:,:,7))
  call heltable([4,8,32], n3(:,33), t3x32(:,:,8))
  call heltable([4,8,32], n3(:,34), t3x32(:,:,9))
  call heltable([4,8,32], n3(:,35), t3x32(:,:,10))
  call heltable([4,8,32], n3(:,36), t3x32(:,:,11))
  call heltable([4,8,32], n3(:,37), t3x32(:,:,12))
  call heltable([4,8,32], n3(:,38), t3x32(:,:,13))
  call heltable([4,8,32], n3(:,39), t3x32(:,:,14))
  call heltable([4,8,32], n3(:,40), t3x32(:,:,15))
  call heltable([4,8,32], n3(:,41), t3x32(:,:,16))
  call heltable([4,8,32], n3(:,42), t3x32(:,:,17))
  call heltable([4,8,32], n3(:,43), t3x32(:,:,18))
  call heltable([4,8,32], n3(:,44), t3x32(:,:,19))
  call heltable([4,8,32], n3(:,45), t3x32(:,:,20))
  call heltable([4,8,32], n3(:,46), t3x32(:,:,21))
  call heltable([4,8,32], n3(:,47), t3x32(:,:,22))
  call heltable([4,8,32], n3(:,48), t3x32(:,:,23))
  call heltable([4,8,32], n3(:,49), t3x32(:,:,24))
  call heltable([4,8,32], n3(:,50), t3x32(:,:,25))
  call heltable([4,8,32], n3(:,51), t3x32(:,:,26))
  call heltable([4,8,32], n3(:,52), t3x32(:,:,27))
  call heltable([4,8,32], n3(:,53), t3x32(:,:,28))
  call heltable([4,8,32], n3(:,54), t3x32(:,:,29))
  call heltable([4,8,32], n3(:,55), t3x32(:,:,30))
  call heltable([4,8,32], n3(:,56), t3x32(:,:,31))
  call heltable([4,8,32], n3(:,57), t3x32(:,:,32))
  call heltable([4,8,32], n3(:,58), t3x32(:,:,33))
  call heltable([4,8,32], n3(:,59), t3x32(:,:,34))
  call heltable([4,8,32], n3(:,60), t3x32(:,:,35))
  call heltable([4,8,32], n3(:,61), t3x32(:,:,36))
  call heltable([4,8,32], n3(:,62), t3x32(:,:,37))
  call heltable([4,8,32], n3(:,63), t3x32(:,:,38))
  call heltable([4,8,32], n3(:,64), t3x32(:,:,39))
  call heltable([4,8,32], n3(:,65), t3x32(:,:,40))
  call heltable([4,8,32], n3(:,66), t3x32(:,:,41))
  call heltable([4,8,32], n3(:,67), t3x32(:,:,42))
  call heltable([4,8,32], n3(:,68), t3x32(:,:,43))
  call heltable([4,8,32], n3(:,69), t3x32(:,:,44))
  call heltable([4,8,32], n3(:,70), t3x32(:,:,45))

  heltables_not_init = .false.

end subroutine init_heltables

end module ol_heltables_ppjj_ggggg_1
