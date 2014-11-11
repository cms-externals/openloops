
! Copyright 2014 Fabio Cascioli, Jonas Lindert, Philipp Maierhoefer, Stefano Pozzorini
!
! This file is part of OpenLoops.
!
! OpenLoops is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! OpenLoops is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.


! ol_dilog configuration
! stop series expansion when the result doesn't change anymore

! check expansion depth required to achieve full precision;
! store the number of points which required n terms in bin(n)
!#CHECK_DEPTH




module ol_generic
  ! precision independent generic routines
  implicit none
  interface to_string
    module procedure integer_to_string, integerlist_to_string
  end interface to_string

  contains

  function integer_to_string(x)
    implicit none
    integer :: x
    character(12) :: integer_to_string
    write(integer_to_string,*) x
    integer_to_string = adjustl(integer_to_string)
  end function integer_to_string


  function integerlist_to_string(x)
    implicit none
    integer :: x(:)
    character(12*size(x)+1) :: integerlist_to_string
    integer :: k
    integerlist_to_string = "["
    do k = 1, size(x)-1
      integerlist_to_string = trim(integerlist_to_string) // trim(integer_to_string(x(k))) // ","
    end do
    integerlist_to_string = trim(integerlist_to_string) // trim(integer_to_string(x(size(x)))) // "]"
  end function integerlist_to_string


  pure function factorial(k)
    implicit none
    integer, intent(in) :: k
    integer :: factorial, i
    factorial = 1
    do i = 1, k
      factorial = factorial * i
    end do
  end function factorial


  pure function binomial(n, k)
    implicit none
    integer :: binomial
    integer, intent(in) :: n, k
    integer :: i
    if (k < 0 .or. k > n) then
      binomial = 0
    else
      binomial = 1
      do i = 1, min(k, n-k)
        binomial = (binomial * (n-i+1)) / (i)
      end do
    end if
  end function binomial


  function perm_pos(perm)
    ! Unique mapping of a permutation perm(1:n) -> integer in [1..n!].
    ! In canonically ordered list of all permutations
    ! (like Mathematica's Permutations[Range[n]]),
    ! perm is at position perm_pos
    implicit none
    integer :: perm_pos
    integer, intent(in) :: perm(:)
    integer :: n, k, pos1, perm2(size(perm))
    perm_pos = 1
    perm2 = perm
    do n = size(perm)-1, 1, -1
      pos1 = perm2(1)
      do k = 1, n
        if (perm2(k+1) > pos1) then
          perm2(k) = perm2(k+1)-1
        else
          perm2(k) = perm2(k+1)
        end if
      end do
      perm_pos = perm_pos + (pos1-1)*factorial(n)
    end do
  end function perm_pos


  function compositions(n, k)
    ! compositions of n into k parts.
    ! compositions(1:k,j) for j=1:binomial(n+k-1,k-1)
    ! are the canonically ordered compositions.
    implicit none
    integer, intent(in) :: n, k
    integer :: compositions(k,binomial(n+k-1,k-1))
    integer :: compos(k-1,binomial(n+k-1,k-1))
    integer :: lowersz, pos, a, b, c, comp(k-1)
    lowersz = 1
    do a = 1, k-1
      pos = 0
      do b = 1, lowersz
        comp(1:a-1) = compositions(1:a-1,b)
        do c = 0, n-sum(comp(1:a-1))
          pos = pos + 1
          compos(1:a-1,pos) = comp(1:a-1)
          compos(a,pos) = c
        end do
      end do
      lowersz = pos
      compositions(1:a,1:lowersz) = compos(1:a,1:lowersz)
    end do
    if (k > 0) then
      do pos = 1, lowersz
        compositions(k,pos) = n - sum(compositions(1:k-1,pos))
      end do
    end if
  end function compositions


  function relative_deviation(a, b)
    use kind_types, only: dp
    implicit none
    real(dp), intent(in) :: a, b
    real(dp) :: relative_deviation
    if (a == b) then
      relative_deviation = 0
    else if ( a == 0 .and. b == 0) then
      relative_deviation = 0
    else if ( a == 0 .or. b == 0) then
      relative_deviation = huge(a)
    else
      relative_deviation = max(abs(a/b-1), abs(b/a-1))
    end if
  end function relative_deviation


  function digit_agreement(a, b)
    use kind_types, only: dp
    implicit none
    real(dp), intent(in) :: a, b
    real(dp) :: digit_agreement
    if (a == b) then
      digit_agreement = 16
    else if (a == 0 .or. b == 0) then
      digit_agreement = 0
    else
      digit_agreement = -log10(relative_deviation(a, b))
    end if
  end function digit_agreement


  function random_string(n)
    ! Return a printable random string of n 6-bit characters
    ! from the set [0-9A-Za-z$_]
    implicit none
    integer, intent(in) :: n
    integer :: j
    integer(1) :: ran(n), i
    character(len=n) :: random_string
    open(42, file='/dev/urandom', access='stream', form='unformatted')
    read(42) ran
    close(42)
    do j = 1, n
      i = ishft(ran(j),-2) + 43 ! 43:     +     37+ 8=45
      if (i > 43) i = i + 1     ! 45:     -     46+ 2=48
      if (i > 45) i = i + 2     ! 48- 57: 0-9   58+ 7=65
      if (i > 57) i = i + 7     ! 65- 90: A-Z   91+ 6=97
      if (i > 90) i = i + 6     ! 97-122: a-z
      random_string(j:j) = char(i)
    end do
  end function random_string


end module ol_generic



module ol_iso_c_utilities
  ! function c_f_string_ptr(cptr)
  !   type(c_ptr), intent(in) :: cptr
  !   character(kind=c_char), pointer :: c_f_string_ptr(:)
  !   - convert a null terminated C character array pointer to a Fortran string pointer;
  ! subroutine c_f_string_static(c_str, f_str, maxlen):
  !   character(kind=c_char), dimension(*), intent(in) :: c_str
  !   integer, intent(in) :: maxlen
  !   character(len=maxlen), intent(out) :: f_str
  !   - convert a null terminated C character array to a Fortran string;
  ! subroutine c_f_string_alloc(c_str, f_str):
  !   character(kind=c_char), dimension(*), intent(in) :: c_str
  !   character(len=:), allocatable, intent(out) :: f_str
  !   - convert a null terminated C character array to a Fortran allocatable string;
  ! function to_lowercase(instr)
  !   character(*), intent(in) :: instr
  !   character(len(instr)) :: to_lowercase
  !   - return instr converted to lower case;
  use, intrinsic :: iso_c_binding, only: c_char
  implicit none

  character(kind=c_char), save, target, private :: dummy_string(1) = "?"

  character(len=26), private, parameter :: lower_case = 'abcdefghijklmnopqrstuvwxyz'
  character(len=26), private, parameter :: upper_case = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

  interface
    function strlen(string) bind(c)
      ! int strlen(char *string)
      use, intrinsic :: iso_c_binding, only: c_int, c_ptr
      implicit none
      type(c_ptr), value :: string
      integer(c_int) :: strlen
    end function strlen
  end interface

  interface c_f_string
    module procedure c_f_string_static!, c_f_string_alloc
  end interface c_f_string

  contains

  function c_f_string_ptr(cptr)
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_associated, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: cptr
    character(kind=c_char), pointer :: c_f_string_ptr(:)
    if (c_associated(cptr)) then
      call c_f_pointer(cptr, c_f_string_ptr, shape = [strlen(cptr)])
    else
      ! if cptr is a null pointer, return 'dummy_string'
      c_f_string_ptr => dummy_string
    end if
  end function c_f_string_ptr


  subroutine c_f_string_static(c_str, f_str, maxlen)
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_loc, c_f_pointer
    implicit none
    character(kind=c_char), dimension(*), intent(in), target :: c_str
    integer, intent(in) :: maxlen
    type(c_ptr) :: c_str_ptr
    character(len=maxlen), intent(out) :: f_str
    character(kind=c_char), pointer :: f_str_ptr(:)
    integer :: slen, i
    c_str_ptr = c_loc(c_str)
    slen = strlen(c_str_ptr)
    call c_f_pointer(c_str_ptr, f_str_ptr, shape = [slen])
    f_str = ""
    do i = 1, slen
      f_str(i:i) = f_str_ptr(i)
    end do
  end subroutine c_f_string_static


! deactivate to circumvent a bug in certain gfortran versions.
! Previously used in register_process_c, olp_setparameter_c, olp_printparameter_c, olp_start_c
!   subroutine c_f_string_alloc(c_str, f_str)
!     use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_loc, c_f_pointer
!     implicit none
!     character(kind=c_char), dimension(*), intent(in), target :: c_str
!     type(c_ptr) :: c_str_ptr
!     character(len=:), allocatable, intent(out) :: f_str
!     character(kind=c_char), pointer :: f_str_ptr(:)
!     integer :: slen, i
!     c_str_ptr = c_loc(c_str)
!     slen = strlen(c_str_ptr)
!     call c_f_pointer(c_str_ptr, f_str_ptr, shape = [slen])
!     if (allocated(f_str)) deallocate(f_str)
!     allocate(character(len=size(f_str_ptr))::f_str) ! len=slen doesn't work
!     do i = 1, slen
!       f_str(i:i) = f_str_ptr(i)
!     end do
!   end subroutine c_f_string_alloc


  function to_lowercase(instr)
    ! return instr with uppercase letters converted to lowercase
    implicit none
    character(*), intent(in) :: instr
    character(len(instr)) :: to_lowercase
    integer :: i, n
    to_lowercase = instr
    do i = 1, len(to_lowercase)
      n = index(upper_case, to_lowercase(i:i))
      if (n /= 0) to_lowercase(i:i) = lower_case(n:n)
    end do
  end function to_lowercase

end module ol_iso_c_utilities



module ol_dlfcn
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_funptr, &
    & c_null_char, c_associated, c_f_procpointer
  implicit none
  private
  public :: RTLD_LAZY, RTLD_NOW, RTLD_GLOBAL, RTLD_LOCAL
  public :: dlopen, dlsym, dlclose
  ! dlopen modes:
  integer(c_int), parameter :: RTLD_LAZY = 1, RTLD_NOW = 2, RTLD_GLOBAL = 256, RTLD_LOCAL = 0

  interface
    function c_dlopen(file, mode) bind(c,name="dlopen")
      ! void *dlopen(const char *file, int mode);
      use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
      implicit none
      character(kind=c_char), dimension(*), intent(in) :: file
      integer(c_int), value :: mode
      type(c_ptr) :: c_dlopen
    end function c_dlopen
    function c_dlsym(lib, sym) bind(c,name="dlsym")
      ! void *dlsym(void *lib, const char *sym);
      use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_funptr
      implicit none
      type(c_ptr), value :: lib
      character(kind=c_char), dimension(*), intent(in) :: sym
      type(c_funptr) :: c_dlsym
    end function c_dlsym
    function c_dlclose(lib) bind(c,name="dlclose")
      ! int dlclose(void *lib);
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int
      implicit none
      type(c_ptr), value :: lib
      integer(c_int) :: c_dlclose ! status
    end function c_dlclose
    function c_dlerror() bind(c,name="dlerror")
      ! char *dlerror(void);
      use, intrinsic :: iso_c_binding, only: c_ptr
      implicit none
      type(c_ptr) :: c_dlerror
    end function c_dlerror
  end interface

  contains

  function dlerror()
    use ol_iso_c_utilities, only: c_f_string_ptr
    implicit none
    character(kind=c_char), dimension(:), pointer :: dlerror
    dlerror => c_f_string_ptr(c_dlerror())
  end function

  function dlopen(file, mode, fatal)
    ! fatal: 0=silent (default), 1=warning, 2=error
    implicit none
    character(len=*), intent(in) :: file
    integer(c_int), intent(in) :: mode
    integer, intent(in), optional :: fatal
    type(c_ptr) :: dlopen
    dlopen = c_dlopen(trim(file) // c_null_char, mode)
    if (present(fatal)) then
      if (fatal == 1 .and. .not. c_associated(dlopen)) then
        print *, "[OpenLoops] dlopen:", dlerror()
      else if (fatal == 2 .and. .not. c_associated(dlopen)) then
        print *, "[OpenLoops] error in dlopen:", dlerror()
        stop
      end if
    end if
  end function dlopen

  function dlsym(lib, sym, fatal) result(f_funp)
    ! fatal: 0=silent (default), 1=warning, 2=error
    implicit none
    type(c_ptr), intent(in) :: lib
    character(len=*), intent(in) :: sym
    integer, intent(in), optional :: fatal
    type(c_funptr) :: c_funp
    procedure(), pointer :: f_funp
    c_funp = c_dlsym(lib, trim(sym) // c_null_char)
    if (present(fatal)) then
      if (fatal == 1 .and. .not. c_associated(c_funp)) then
        print *, "[OpenLoops] dlsym:", dlerror()
      else if (fatal == 2 .and. .not. c_associated(c_funp)) then
        print *, "[OpenLoops] error in dlsym:", dlerror()
        stop
      end if
    end if
    if (c_associated(c_funp)) then
      call c_f_procpointer(c_funp, f_funp)
    else
      f_funp => null()
    end if
  end function dlsym

  subroutine dlclose(lib, fatal)
    ! fatal: 0=silent (default), 1=warning, 2=error
    implicit none
    type(c_ptr), intent(in) :: lib
    integer, intent(in), optional :: fatal
    integer(c_int) :: status
    status = c_dlclose(lib)
    if (present(fatal)) then
      if (fatal == 1 .and. status /= 0) then
        print *, "[OpenLoops] dlclose:", dlerror()
      else if (fatal == 2 .and. status /= 0) then
        print *, "[OpenLoops] error in dlclose:", dlerror()
        stop
      end if
    end if
  end subroutine dlclose

end module ol_dlfcn





module ol_dilog_dp
  use kind_types, only: dp
  implicit none
  real(dp), parameter :: pi2_6 = 8/3._dp*atan(1._dp)**2



  integer, parameter :: max_n = 29




  real(dp), parameter :: G4  = 6,         G6  = G4 * 4* 5, G8  = G6 * 6* 7, G10 = G8 * 8* 9, G12 = G10*10*11
  real(dp), parameter :: G14 = G12*12*13, G16 = G14*14*15, G18 = G16*16*17, G20 = G18*18*19, G22 = G20*20*21
  real(dp), parameter :: G24 = G22*22*23, G26 = G24*24*25, G28 = G26*26*27, G30 = G28*28*29, G32 = G30*30*31

  real(dp), parameter :: G34 = G32*32*33, G36 = G34*34*35, G38 = G36*36*37, G40 = G38*38*39, G42 = G40*40*41
  real(dp), parameter :: G44 = G42*42*43, G46 = G44*44*45, G48 = G46*46*47, G50 = G48*48*49, G52 = G50*50*51
  real(dp), parameter :: G54 = G52*52*53, G56 = G54*54*55, G58 = G56*56*57, G60 = G58*58*59, G62 = G60*60*61

  real(dp), parameter :: B2n(max_n) = [ & ! BernoulliB[2*n]/Gamma[2*n+2]
    &                                     1._dp / (      6 * G4 ) & !  1
    & ,                                  -1._dp / (     30 * G6 ) & !  2
    & ,                                   1._dp / (     42 * G8 ) & !  3
    & ,                                  -1._dp / (     30 * G10) & !  4
    & ,                                   5._dp / (     66 * G12) & !  5
    & ,                                -691._dp / (   2730 * G14) & !  6
    & ,                                   7._dp / (      6 * G16) & !  7
    & ,                               -3617._dp / (    510 * G18) & !  8
    & ,                               43867._dp / (    798 * G20) & !  9
    & ,                             -174611._dp / (    330 * G22) & ! 10
    & ,                              854513._dp / (    138 * G24) & ! 11
    & ,                          -236364091._dp / (   2730 * G26) & ! 12
    & ,                             8553103._dp / (      6 * G28) & ! 13
    & ,                        -23749461029._dp / (    870 * G30) & ! 14
    & ,                       8615841276005._dp / (  14322 * G32) & ! 15

    & ,                      -7709321041217._dp / (    510 * G34) & ! 16
    & ,                       2577687858367._dp / (      6 * G36) & ! 17
    & ,               -26315271553053477373._dp / (1919190 * G38) & ! 18
    & ,                    2929993913841559._dp / (      6 * G40) & ! 19
    & ,              -261082718496449122051._dp / (  13530 * G42) & ! 20
    & ,              1520097643918070802691._dp / (   1806 * G44) & ! 21
    & ,            -27833269579301024235023._dp / (    690 * G46) & ! 22
    & ,            596451111593912163277961._dp / (    282 * G48) & ! 23
    & ,       -5609403368997817686249127547._dp / (  46410 * G50) & ! 24
    & ,         495057205241079648212477525._dp / (     66 * G52) & ! 25
    & ,     -801165718135489957347924991853._dp / (   1590 * G54) & ! 26
    & ,    29149963634884862421418123812691._dp / (    798 * G56) & ! 27
    & , -2479392929313226753685415739663229._dp / (    870 * G58) & ! 28
    & , 84483613348880041862046775994036021._dp / (    354 * G60) & ! 29

  & ]

  contains

! **********************************************************************
function Li2conv(z)
! Complex dilogarithm Li2(z) calculated from a series expansion in terms of Bernoulli numbers.
! This is supposed to be used in the complex region |z| < 1 && Re(z) < 1/2.
! In this region the expansion (max_n = 29) is deep enough to achieve quadruple precision.
! Required expansion depths: sp:9, dp:13, ep:15, qp:24
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in) :: z
  complex(dp) :: Li2conv
  complex(dp) :: Lz2, Lz2n1, Li2conv_next
  integer           :: n
  Lz2n1   = -log(1-z)
  Lz2     = Lz2n1*Lz2n1
  Li2conv = Lz2n1 - 0.25_dp*Lz2
  do n = 1, max_n
    Lz2n1 = Lz2n1 * Lz2
    Li2conv_next = Li2conv + Lz2n1 * B2n(n)

    if (Li2conv_next == Li2conv) exit

    Li2conv = Li2conv_next
  end do



end function Li2conv


! **********************************************************************
function Li2(z)
! Complex dilogarithm Li2(z)
! Map z to the complex region |z| < 1 && Re(z) < 1/2 and call the series expansion.
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in) :: z
  complex(dp) :: Li2
  if (real(z) <= 0.5) then
    if (abs(z) <= 1) then
      Li2 =   Li2conv(z)
    else
      Li2 = - Li2conv(1/z)     -   pi2_6                   - .5_dp * log( -z)**2
    end if
  else
    if (abs(1-z) <= 1) then
      Li2 = - Li2conv(1-z)     +   pi2_6 - log(z)*log(1-z)
    else
      Li2 =   Li2conv(1/(1-z)) + 2*pi2_6 - log(z)*log(1-z) + .5_dp * log(z-1)**2
    end if
  end if
end function Li2

end module ol_dilog_dp

