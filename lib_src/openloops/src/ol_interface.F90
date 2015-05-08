
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


module openloops
  use KIND_TYPES, only: DREALKIND
  use ol_global_decl, only: MaxParticles
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_char, c_int, c_double, c_null_char
  use ol_init, only: set_init_error_fatal, get_error, &
    & set_parameter, get_parameter, parameters_flush, cleanup
  use ol_version, only: welcome
  use ol_parameters_decl_/**/DREALKIND,  only: procname_length, max_parameter_length, verbose
  use ol_external_decl_/**/DREALKIND, only: n_scatt
  implicit none
  private
  ! from module init_iu
  public :: set_init_error_fatal, get_error
  public :: set_parameter, get_parameter, parameters_flush
  ! from module use ol_version
  public :: welcome
  ! process interface
  public :: n_external, amplitudetype, phase_space_point, start, finish, tree_colbasis_dim, tree_colbasis
  public :: register_process, register_process_id
  public :: evaluate_tree, evaluate_tree_colvect, evaluate_cc, evaluate_ccmatrix, evaluate_sc, evaluate_scpowheg
  public :: evaluate_full, evaluate_loop, evaluate_loop2, evaluate_ct, evaluate_pt
  ! used in BLHA interface
  public :: rval_size, stop_invalid_id

  interface register_process
    module procedure register_process_string, register_process_id
  end interface register_process

  type process_handle
    integer :: n_particles = 0
    integer :: max_point = -1
    integer :: tensor_rank = -1
    ! allocatable length character members are not supported in gfortran (tested with 4.8.1)
    character(len=procname_length) :: process_name
    character(len=procname_length) :: library_name
    integer, allocatable :: permutation(:)
    type(c_ptr) :: library_handle = c_null_ptr
    integer :: amplitude_type ! 1=Tree, 2=ccTree, 3=scTree, 4=scTree_polvect, 11=Loop, 12=LoopInduced
    integer :: content = 0 ! bitwise: 2^0=tree, 2^1=loop, 2^2=loop2, 2^3=pt
    integer :: n_in = 2 ! Phase-space for n_in -> n-n_in
    procedure(), pointer, nopass :: set_permutation => null()
    procedure(), pointer, nopass :: tree => null()
    procedure(), pointer, nopass :: loop => null()
    procedure(), pointer, nopass :: ct => null()
    procedure(), pointer, nopass :: pt => null()
    procedure(), pointer, nopass :: rambo => null()
    procedure(), pointer, nopass :: tree_colbasis_dim => null()
    procedure(), pointer, nopass :: tree_colbasis => null()
    procedure(), pointer, nopass :: tree_colvect => null()
  end type process_handle

  ! process handle array
  integer, save :: last_process_id = 0
  type(process_handle), save, allocatable :: process_handles(:)

  type processinfos
    integer :: ID
    integer :: EWorder(0:1)
    integer :: QCDorder(0:1)
    integer :: LeadingColour
    integer :: NF
    integer :: NC
    integer :: CKMORDER
    character :: ME, MM, ML, MU, MD, MS, MC, MB
    integer :: YE, YM, YL, YU, YD, YS, YC, YB, YT
    character :: CC
    character(len=max_parameter_length) :: LIBNAME
    character(len=127) :: MODEL
    character(len=127) :: PROC
    character(len=127) :: MAP
    character(len=127) :: MAPPERM
    character(len=127) :: APPROX
    character(len=4) :: TYPE
    character(len=4) :: LTYPE
  end type processinfos
  type(processinfos), save, allocatable :: process_infos(:)
  type(processinfos), save, allocatable :: loaded_libs(:)

  ! array for shopping list
  character(len=max_parameter_length), save, allocatable :: shopped_processes(:)
  logical, save :: shopping_list_open = .false.
  integer, parameter :: fh_shopping = 998

#if __APPLE__
  character(len=5), parameter :: dynlib_extension='dylib'
#else
  character(len=2), parameter :: dynlib_extension='so'
#endif



  contains


  pure function rval_size(n_part, amp_type)
    implicit none
    integer :: rval_size
    integer, intent(in) :: n_part, amp_type
    select case (amp_type)
      case  (1, 12) ! Tree, LoopInduced
        rval_size = 1
      case (11) ! Loop
        rval_size = 4
      case (2) ! ccTreeo
        rval_size = (n_part*(n_part-1))/2
      case (3) ! scTree
        rval_size = 2*n_part*n_part
      case (4) ! scTree_polvect
        rval_size = n_part
      case (0)
        rval_size = 0
    end select
  end function rval_size


  function get_process_handle(lib, libname, proc, perm, content, amptype, n_in)
    ! [in] lib: a shared library handle
    ! [in] proc: a full process name, '<lib>_<subproc>_<id>'
    ! [in] perm: integer array with the crossing
    ! [in] content: integer with binary tags for tree, loop, loop2, pt
    ! [in] amptype: integer to specify BLHA matrix element type
    ! return process handle of type process_handle
    ! note: error handling is done in dlsym
    use ol_dlfcn, only: dlsym
    implicit none
    type(c_ptr), intent(in) :: lib
    character(len=*), intent(in) :: libname
    character(len=*), intent(in) :: proc
    integer, intent(in) :: perm(:), content, amptype, n_in
    type(process_handle) :: get_process_handle
    real(DREALKIND) :: masses(MaxParticles)
    procedure(), pointer :: tmp_fun
    get_process_handle%library_name = trim(libname)
    get_process_handle%process_name = trim(proc)
    allocate(get_process_handle%permutation(size(perm)))
    get_process_handle%permutation = perm
    get_process_handle%library_handle = lib
    get_process_handle%set_permutation => dlsym(lib, "ol_f_set_permutation_" // trim(proc))
    get_process_handle%rambo => dlsym(lib, "ol_f_rambo_" // trim(proc))
    get_process_handle%amplitude_type = amptype
    get_process_handle%tree => dlsym(lib, "ol_f_amp2_" // trim(proc))
    get_process_handle%loop => dlsym(lib, "ol_f_vamp2_" // trim(proc))
    get_process_handle%ct => dlsym(lib, "ol_f_ctamp2_" // trim(proc))
    get_process_handle%pt => dlsym(lib, "ol_f_ptamp2_" // trim(proc))
    get_process_handle%content = content
    get_process_handle%n_in = n_in
    ! number of external particles and highest tensor rank
    tmp_fun => dlsym(lib, "ol_f_n_external_" // trim(proc))
    call tmp_fun(get_process_handle%n_particles)
    if (btest(content, 1)) then
      tmp_fun => dlsym(lib, "ol_f_max_point_" // trim(proc))
      call tmp_fun(get_process_handle%max_point)
      tmp_fun => dlsym(lib, "ol_f_tensor_rank_" // trim(proc))
      call tmp_fun(get_process_handle%tensor_rank)
    end if
    ! check correct size of the permutation
    if (get_process_handle%n_particles /= size(perm)) then
      write(*,*) '[OpenLoops] error: registered process with wrong size of particle permutation'
      stop
    end if
    ! colour basis
    get_process_handle%tree_colbasis_dim => dlsym(lib, "ol_tree_colbasis_dim_" // trim(proc))
    get_process_handle%tree_colbasis => dlsym(lib, "ol_tree_colbasis_" // trim(proc))
    get_process_handle%tree_colvect => dlsym(lib, "ol_tree_colvect_" // trim(proc))
  end function get_process_handle


  function register_process_lib(libname, proc, perm, content, amptype, n_in)
    ! [in] libname: name of the process library
    ! [in] proc: a full process name, '<lib>_<subproc>_<id>'
    ! [in] perm: integer array with the crossing
    ! [in] content: integer with binary tags for tree, loop, loop2, pt
    ! [in] amptype: integer to specify BLHA matrix element type
    ! return (integer) process id to be used in OLP_EvalSubProcess
    use ol_dlfcn, only: dlopen, RTLD_LAZY
    use ol_loop_parameters_decl_/**/DREALKIND, only: maxpoint, maxrank
    implicit none
    character(len=*), intent(in) :: libname
    character(len=*), intent(in) :: proc
    integer, intent(in) :: perm(:), content, amptype, n_in
    type(c_ptr) :: lib
    integer :: register_process_lib
    integer :: k
    type(process_handle) :: prochandle
    type(process_handle), allocatable :: process_handles_bak(:)
    lib = dlopen(libname, RTLD_LAZY, 2)
    prochandle = get_process_handle(lib, libname, proc, perm, content, amptype, n_in)
    ! Check if the process was registered before with the same permutation and amptype.
    ! If yes, return the previously assigned id
    do k = 1, last_process_id
      if ((trim(proc) == trim(process_handles(k)%process_name)) .and. &
        & (trim(libname) == trim(process_handles(k)%library_name)) .and. &
        & all(perm == process_handles(k)%permutation) .and. &
        & (amptype == process_handles(k)%amplitude_type) ) then
        register_process_lib = k
        return
      end if
    end do
    if (.not. allocated(process_handles)) then
      allocate(process_handles(1))
    end if
    if (last_process_id == size(process_handles)) then
      allocate(process_handles_bak(last_process_id))
      process_handles_bak = process_handles
      deallocate(process_handles)
      allocate(process_handles(2*last_process_id))
      process_handles(1:last_process_id) = process_handles_bak
      deallocate(process_handles_bak)
    end if
    last_process_id = last_process_id + 1
    process_handles(last_process_id) = prochandle
    if (maxpoint < process_handles(last_process_id)%max_point) then
      maxpoint = process_handles(last_process_id)%max_point
    end if
    if (maxrank < process_handles(last_process_id)%tensor_rank) then
      maxrank = process_handles(last_process_id)%tensor_rank
    end if
    register_process_lib = last_process_id
  end function register_process_lib



  subroutine unregister_processes()
    ! Close all process libraries and nullify process handles.
    use ol_dlfcn, only: dlclose
    implicit none
    integer :: id
    do id = 1, last_process_id
      call dlclose(process_handles(id)%library_handle)
      process_handles(id)%n_particles = 0
      process_handles(id)%content = 0
      deallocate(process_handles(id)%permutation)
      process_handles(id)%library_handle = c_null_ptr
      process_handles(id)%set_permutation => null()
      process_handles(id)%tree => null()
      process_handles(id)%loop => null()
      process_handles(id)%ct => null()
      process_handles(id)%pt => null()
    end do
    if (allocated(process_handles)) deallocate(process_handles)
    last_process_id = 0
  end subroutine unregister_processes


  function register_process_string(process, amptype)
    ! process: string with format 2->n-2
    ! amptype: integer 1,2,3,4,11,12
    ! return (integer) process id to be used in evaluate_process
    use ol_generic, only: to_int
    implicit none
    character(len=*), intent(in) :: process
    integer, intent(in) :: amptype
    integer :: register_process_string
    character(len=max_parameter_length) :: tmp
    character(len=max_parameter_length) :: inp, outp
    integer :: tmpi
    integer :: next, n_in
    integer, allocatable :: ext(:)
    integer, allocatable :: ext_bak(:)
    logical :: decay = .false.

    ! split: process->(in,out)
    next = 0
    inp = adjustl(trim(process(1:index(process, "->")-1)))
    outp = adjustl(process(index(process, "->")+2:len(process)))

    allocate(ext(4))
    ! split initial state
    ISloop: do
      tmp = adjustl(inp(1:index(inp, " ")))
      inp = adjustl(inp(index(inp, " "):))
      if (index(tmp, " ") == 1) then
        exit ISloop
      else
        next = next + 1
        ext(next) = to_int(trim(tmp))
      end if
    end do ISloop
    n_in = next

    ! split final state
    FSloop: do
      tmp = adjustl(outp(1:index(outp, " ")))
      outp = adjustl(outp(index(outp, " "):))
      if (index(tmp, " ") == 1) then
        exit FSloop !no more final state particles
      else
        next = next + 1
        if (next > size(ext)) then
          allocate(ext_bak(size(ext)))
          ext_bak = ext
          deallocate(ext)
          allocate(ext(2*next))
          ext(1:size(ext_bak)) = ext_bak
          deallocate(ext_bak)
        end if
        ext(next) = to_int(trim(tmp))
      end if
    end do FSloop

    if (next == n_in .or. n_in == 0) then
      print *, "[OpenLoops] register_process: invalid argument: " // trim(process)
      register_process_string = -1
      return
    end if

    register_process_string = register_process_id(ext(1:next), amptype, n_in)

  end function register_process_string


  function register_process_id(ext_in, amptype, n_in_in)
    ! process: array with format [in_1, in_2, out_1, .. , out_n-2]
    ! amptype: integer 1,2,3,4,11,12
    ! return (integer) process id to be used in evaluate_process
    use ol_parameters_decl_/**/DREALKIND, only: &
      & install_path,flavour_mapping_on, coupling_QCD, coupling_EW, write_shopping_list
    use ol_generic, only: to_string, to_lowercase
#ifdef USE_IFORT
    use ifport, only: system
#endif
    implicit none
    integer, intent(in) :: ext_in(:)
    integer, intent(in) :: amptype
    integer, optional, intent(in) :: n_in_in
    integer :: register_process_id
    character(len=4) :: loops_flags
    character(len=max_parameter_length) :: proc
    character :: coupling_order(2)
    character(len=max_parameter_length) :: process
    integer :: lib_content
    integer :: ierrparam, i
    integer :: check
    integer :: libid
    integer :: next
    integer :: ext(size(ext_in))
    logical :: proclib_exists
    integer :: n_in = 2
    integer, allocatable :: perm(:)
    integer :: librarytype

    loops_flags = "tlsp" ! used in this order to set content bits
    call parameters_flush() ! make sure that pid_string is set
    register_process_id = -1

    ! check that proclib exists
#ifdef USE_GFORTRAN
    inquire(file=trim(install_path)//"/proclib/.", exist=proclib_exists)
#endif
#ifdef USE_IFORT
    inquire(directory=trim(install_path)//"/proclib", exist=proclib_exists)
#endif
    if (.not. proclib_exists) then
      print *, "[OpenLoops] register_process: proclib folder not found, check install_path or install libraries."
      return
    end if

    next =  size(ext_in)
    ! check
    if (next < 3) then
      print*, "[OpenLoops] Error: 1 -> 1 not supported!"
      return
    end if


    if (present(n_in_in)) then
      n_in = n_in_in
    end if

    ! write process string for output
    process = ""
    do i=1,n_in
      process = trim(process) // " " // trim(to_string(ext_in(i)))
    end do
    process = trim(process) //  " ->"
    do i=n_in+1,next
      process = trim(process) // " " // trim(to_string(ext_in(i)))
    end do

    ext = ext_in
    ! charge conjugate final state particles
    call charge_conj(ext,n_in)

    ! flavour mapping
    if (flavour_mapping_on == 1) then
      if (verbose > 2) then
        print*, "[OpenLoops] Flavour mapping. Original (all ingoing) process: ", ext(1:next)
      end if
      call flavour_mapping(ext)
      if (verbose > 2) then
        print*, "[OpenLoops] Flavour mapping. Mapped (all ingoing) process:   ", ext(1:next)
      end if
    end if

    ! determine normal ordering
    allocate (perm(next))
      call normal_order(ext(1:next), perm, proc)
    if (proc == "") return

    if (verbose > 3) then
      print*, "[OpenLoops] registering process: ", trim(proc) // ", ",  ext_in
    end if

    if (amptype == 99 .or. write_shopping_list ) then ! write shopping list
      ! charge conjugate back final state particles to write shopping list
      call charge_conj(ext,n_in)
      register_process_id = write_shop_list(ext(1:next), proc)
    else
      librarytype = 0
      do
        check = check_process(process, proc, perm, amptype, librarytype, n_in)
        if (check > 0) then ! found & registered
          register_process_id = check
          exit
        else if (check == 0) then ! look in next library type
          librarytype = librarytype + 1
        else
          if (check == -1) then  ! not found --> check collections
            check = check_process(process, proc, perm, 999, librarytype, n_in)
          end if
          if (check /= 1) then ! not found anywhere
            print *, "[OpenLoops] register_process: process " // trim(process) // " not found!"
          end if
          exit
        end if
      end do
    end if

    !deallocate
    deallocate(perm)

  contains

  subroutine charge_conj(x,n_in)
    ! determine charge conjugate of array x(3:)
    implicit none
    integer, intent(inout)  :: x(:)
    integer, intent(in)     :: n_in

    do i=n_in+1,size(x)
      select case(x(i))
      case(0, 21, 22, 23, 25)
        x(i)  = x(i)
      case default
        x(i) = -x(i)
      end select
    end do
  end subroutine charge_conj

  subroutine normal_order(ext, perm, proc)
      implicit none
      integer, intent(in) :: ext(:)
      integer, intent(out) :: perm(:)
      character(len=*), intent(out) :: proc
      integer :: i,j, normal(31), pos
      character(len=3) :: normalc(31)

      ! define normal ordering and corresponding characters
      normal(  1:10) = [ 12  ,-12  , 14  ,-14  , 16  ,-16  , 11  ,-11  , 13  ,-13  ]
      normalc( 1:10) = ["ne ","nex","nm ","nmx","nl ","nlx","e  ","ex ","m  ","mx "]

      normal( 11:20) = [ 15  ,-15  ,  2  , -2  ,  4  , -4  ,  6  , -6  ,  1  , -1  ]
      normalc(11:20) = ["l  ","lx ","u  ","ux ","c  ","cx ","t  ","tx ","d  ","dx "]

      normal( 21:30) = [  3  , -3  ,  5  , -5  , 25  , 22  , 23  , -24 , 24  , 21  ]
      normalc(21:30) = ["s  ","sx ","b  ","bx ","h  ","a  ","z  ","w  ","wx ","g  "]

      normal( 31:31) = [ 0 ]
      normalc(31:31) = ["g  "]

      perm = 0
      proc = ""

      ! normal order, build string and store permutation
      pos = 1
      do i = 1, size(normal)
        do j = 1, size(ext)
          if (ext(j) == normal(i)) then
            proc = trim(proc) // trim(normalc(i))
            perm(j) = pos
            pos = pos + 1
          end if
        end do
      end do

      if (pos-1 /= size(ext)) then
        print *, "[OpenLoops] normal_order: invalid process specification:", ext
        proc = ""
      end if
  end subroutine normal_order

  subroutine map_permutation(perm, mapping_str_in)
    use ol_generic, only: to_int
    implicit none
    integer, intent(inout) :: perm(:)
    character(len=127), intent(in) :: mapping_str_in
    character(len=127) :: mapping_str
    integer, allocatable :: mapping(:), perm_tmp(:)
    integer :: i, x
    allocate (mapping(size(perm)))
    allocate (perm_tmp(size(perm)))
    mapping_str=trim(mapping_str_in) // ','
    do i = 1, size(mapping)
      mapping(i) = to_int(trim(adjustl(mapping_str(1:index(mapping_str,',')-1))))
      mapping_str = mapping_str(index(mapping_str,',')+1:len_trim(mapping_str))
    end do
    !map permutation
    do i = 1, size(mapping)
      perm_tmp(i) = mapping(perm(i))
    end do
    perm = perm_tmp
    deallocate(mapping)
    deallocate(perm_tmp)
  end subroutine map_permutation

  function check_process(process, proc, perm, amptype, librarytype, n_in)
  ! 1: found, 0: not found, -1: abort
    use ol_parameters_decl_/**/DREALKIND, only: &
      & install_path, rMB, &
      & allowed_libs, pid_string, tmp_dir
    implicit none
    character(len=max_parameter_length), intent(in) :: process
    character(len=max_parameter_length), intent(inout) :: proc
    integer, intent(inout) :: perm(:)
    integer, intent(in) :: amptype, librarytype, n_in
    integer check_process
    integer, save :: info_files_read = 0
    integer :: readok, ierrg
    integer :: i, j, p, p_unmapped
    logical :: found
    logical :: is_already_loaded, only_loaded
    character(len=4) :: loops_specification
    character(len=4) :: lib_specification
    character(len=max_parameter_length) :: libfilename, libhandle, libname
    character(len=max_parameter_length) :: map_libname
    character(len=max_parameter_length) :: procunmapped
    character(len=max_parameter_length) :: perm_str, mapping_str

    check_process = 0
    map_libname = ''

    ! read all info files
    if ( info_files_read < 1) then
      call readAllInfoFiles(ierrg)
      info_files_read = 1
      if (ierrg /= 0)  then
        check_process = -1
        return
      end if
    end if

    only_loaded = .false.
    ! set loops_specification
    select case (amptype)
      case (1,2,3,4) ! tree-like
        loops_specification = "t"
        if (librarytype == 0) then
          lib_specification = "lt"
          only_loaded = .true.
        else if (librarytype == 1) then
          lib_specification = "t"
        else if (librarytype == 2) then
          lib_specification = "lt"
        else if (librarytype == 3) then
          lib_specification = "lpt"
        else if (librarytype == 4) then
          lib_specification = "lst"
        else if (librarytype == 5) then
          lib_specification = "lpst"
        else
          check_process = -1
          return
        end if
      case (11) ! loop
        loops_specification = "l"
        if (librarytype == 0) then
          lib_specification = "lt"
        else if (librarytype == 1) then
          lib_specification = "l"
        else if (librarytype == 2) then
          lib_specification = "lp"
        else if (librarytype == 3) then
          lib_specification = "lst"
        else if (librarytype == 4) then
          lib_specification = "lpt"
        else if (librarytype == 5) then
          lib_specification = "lpst"
        else
          check_process = -1
          return
        end if
      case (12) ! loop-induced
        loops_specification = "s"
        if (librarytype == 0) then
          lib_specification = "ls"
        else if (librarytype == 1) then
          lib_specification = "lst"
        else if (librarytype == 2) then
          lib_specification = "lps"
        else if (librarytype == 3) then
          lib_specification = "lpst"
        else
          check_process = -1
          return
        end if
      case (999) ! check libraries
        lib_specification = "lib"
        if (info_files_read < 2) then
          call readAllInfoFiles(ierrg, .true.)
          info_files_read = 2
            if (ierrg /= 0)  then
              check_process = -1
              print *, "[OpenLoops] Error: no process libraries installed."
              return
            end if
        end if
      case default
        print *, "[OpenLoops] register_process: amplitude type not supported:", amptype
    end select

    p = 0
    p_unmapped = 0
    InfoLoop: do
      p = p+1
      if (p > size(process_infos)) then
        if ( len_trim(map_libname) /= 0 ) then
          map_libname = ''
          proc = procunmapped
          p = p_unmapped
          cycle
        else
          exit
        end if
      end if

      if ( trim(proc) /= trim(process_infos(p)%PROC) &
        & .or. trim(lib_specification) /= trim(process_infos(p)%LTYPE) &
        & .or. index(trim(process_infos(p)%TYPE), trim(loops_specification)) == 0 &
        & ) then
        cycle InfoLoop
      end if

      libname = trim(process_infos(p)%LIBNAME)
      !check if library is "allowed" (and for correct mapping)
      if (len_trim(allowed_libs) /= 0 .and. index(allowed_libs, " " // trim(libname) // " ") == 0 &
        & .or. (len_trim(map_libname) /= 0 .and. trim(map_libname) /= libname) &
        ) then
        cycle InfoLoop
      end if

      !if required, check if library is already loaded
      if (only_loaded) then
        is_already_loaded = .false.
        if (allocated(loaded_libs)) then
          do j = 1, size(loaded_libs)
            if (trim(loaded_libs(j)%LIBNAME) == trim(libname)  &
              & .and. index(trim(process_infos(p)%TYPE), trim(loops_specification)) > 0 &
              &  ) then
                is_already_loaded = .true.
            end if
          end do
        end if
        if (.not. is_already_loaded) cycle InfoLoop
      end if

      !follow mapping
      if(trim(process_infos(p)%MAP) /= '') then
        procunmapped = proc
        proc = trim(process_infos(p)%MAP)
        !check for MB etc. Todo: check all parameters
        if ((process_infos(p)%MB == "0" .and. rMB /= 0) ) then
          if (verbose > 1) then
            print*, "[OpenLoops] Not following massless b-mapping ", trim(procunmapped), " --> ", trim(proc) , ": MB /= 0."
          end if
          proc = procunmapped
          cycle InfoLoop
        end if
        if (verbose > 1) then
          print*, "[OpenLoops] Following info-file mapping: ",trim(procunmapped), &
         &        " --> ", trim(process_infos(p)%MAP), "[", trim(process_infos(p)%MAPPERM), "]" , "."
        end if
        !map permutation
        if(len_trim(process_infos(p)%MAPPERM) /= 0) then
          call map_permutation(perm,process_infos(p)%MAPPERM)
          mapping_str = " (mapped from " // trim(procunmapped) // ")"
        end if

        map_libname = libname
        p_unmapped = p
        p = 0
        cycle
      else
        mapping_str = ''
      end if

      ! get library filename
      if (amptype == 999) then
        libfilename = "collection " // trim(process_infos(p)%LIBNAME)
      else
        libfilename = 'libopenloops_' // trim(process_infos(p)%LIBNAME) // '_' // &
                      & trim(process_infos(p)%LTYPE) // '.' // dynlib_extension
      end if

      ! check parameters
      call check_parameters(p,amptype,found)

      ! found correct library
      if (found) then

        if (verbose > 1) then
          print*, "[OpenLoops] Parameters do match info-file for process ", trim(proc), " in library ", trim(libfilename)
        end if

        if (amptype == 999) then
          print*, "[OpenLoops] Library for ", trim(process), " not installed but available in: ", trim(libname)
          print*, "[OpenLoops] Note: this library can be downloaded and installed via"
          print*, "[OpenLoops] $ cd " // trim(install_path)
          print*, "[OpenLoops] $ ./openloops libinstall ", trim(libname)
          check_process = 1
          return
        end if

        libfilename = trim(install_path) // '/proclib/' // libfilename
        libhandle = trim(to_lowercase(libname)) // "_" // trim(proc) // "_" // to_string(process_infos(p)%ID)
        lib_content = 0
        do i = 1, len(loops_flags)
          if (index(process_infos(p)%TYPE, loops_flags(i:i)) > 0) lib_content = ibset(lib_content, i-1)
        end do

        !register
        check_process = register_process_lib(libfilename, libhandle, perm, lib_content, amptype, n_in)

        !add to list of loaded libraries
        call add_loaded_library(process_infos(p))

        if (verbose > 0) then
          perm_str = "["
          do i = 1, size(perm) - 1
            perm_str = trim(perm_str) // trim(to_string(perm(i))) // ","
          end do
          perm_str = trim(perm_str) // trim(to_string(perm(i))) // "]"
          print *, "[OpenLoops] Loaded library for process " //  trim(process) // &
            & "  EW=" // trim(to_string(process_infos(p)%EWorder(0)))  // "," // trim(to_string(process_infos(p)%EWorder(1))) // &
            & " QCD=" // trim(to_string(process_infos(p)%QCDorder(0)))  // "," // trim(to_string(process_infos(p)%QCDorder(1))) // &
            & " : " // trim(libhandle) // trim(perm_str) // trim(mapping_str)
        end if
        exit
      else
        if (verbose > 1) then
          print*, "[OpenLoops] Parameters do not match info-file for process ", trim(proc), " in library ", trim(libfilename)
        end if
        check_process = 0
      end if

    end do InfoLoop

    end function check_process

  end function register_process_id


  subroutine check_parameters(p,amptype,found)
    use ol_parameters_decl_/**/DREALKIND, only: &
      & rME, rMM, rML, rMU, rMD, rMC, rMS, rMB, rMT, &
      & rYE, rYM, rYL, rYU, rYD, rYC, rYS, rYB, rYT, &
      & leadingcolour, coupling_QCD, coupling_EW, &
      & approximation, ckmorder, model, allowed_libs
    use ol_loop_parameters_decl_/**/DREALKIND , only: nf, nc
    implicit none
    integer, intent(in) :: p
    integer, intent(in) :: amptype
    logical, intent(out) :: found

    if (allocated(process_infos)) then
    found = .true.
      call check(process_infos(p)%EWorder(0) == coupling_EW(0) .or. coupling_EW(0) == -1,found, "EW tree coupling NOT ok.")
      call check(process_infos(p)%EWorder(1) == coupling_EW(1) .or. coupling_EW(1) == -1,found, "EW loop coupling NOT ok.")
      call check(process_infos(p)%QCDorder(0) == coupling_QCD(0) .or. coupling_QCD(0) == -1, found, "QCD tree coupling NOT ok.")
      call check(process_infos(p)%QCDorder(1) == coupling_QCD(1) .or. coupling_QCD(1) == -1, found, "QCD loop coupling NOT ok.")
      call check(process_infos(p)%LeadingColour == leadingcolour, found, "LeadingColour OK.")
      call check(process_infos(p)%NC == nc, found, "nc NOT ok.")
      call check(process_infos(p)%NF == nf, found, "nf NOT ok.")
      call check(process_infos(p)%CKMorder == ckmorder, found, "CKM NOT ok.")
  !    call check(index(process_infos(p)%MODEL, trim(model)) == 1, found, "model NOT ok.")
      call check((process_infos(p)%ME /= "0" .and. rME /= 0) .or. rME == 0, found, "mass ME NOT ok.")
      call check((process_infos(p)%MM /= "0" .and. rMM /= 0) .or. rMM == 0, found, "mass MM NOT ok.")
      call check((process_infos(p)%ML /= "0" .and. rML /= 0) .or. rML == 0, found, "mass ML NOT ok.")
      call check((process_infos(p)%MU /= "0" .and. rMU /= 0) .or. rMU == 0, found, "mass MU NOT ok.")
      call check((process_infos(p)%MD /= "0" .and. rMD /= 0) .or. rMD == 0, found, "mass MD NOT ok.")
      call check((process_infos(p)%MS /= "0" .and. rMS /= 0) .or. rMS == 0, found, "mass MS NOT ok.")
      call check((process_infos(p)%MC /= "0" .and. rMC /= 0) .or. rMC == 0, found, "mass MC NOT ok.")
      call check((process_infos(p)%MB /= "0" .and. rMB /= 0) .or. rMB == 0, found, "mass MB NOT ok.")
      call check(rME == rYE  .or. process_infos(p)%YE == 1, found, "YukE /= ME NOT ok.")
      call check(rMM == rYM  .or. process_infos(p)%YM == 1, found, "YukM /= MM NOT ok.")
      call check(rML == rYL  .or. process_infos(p)%YL == 1, found, "YukL /= ML NOT ok.")
      call check(rMU == rYU  .or. process_infos(p)%YU == 1, found, "YukU /= MU NOT ok.")
      call check(rMD == rYD  .or. process_infos(p)%YD == 1, found, "YukD /= MD NOT ok.")
      call check(rMS == rYS  .or. process_infos(p)%YS == 1, found, "YukS /= MS NOT ok.")
      call check(rMC == rYC  .or. process_infos(p)%YC == 1, found, "YukC /= MC NOT ok.")
      call check(rMB == rYB  .or. process_infos(p)%YB == 1, found, "YukB /= MB NOT ok.")
      call check(rMT == rYT  .or. process_infos(p)%YT == 1, found, "YukT /= YT NOT ok.")
      call check(amptype == 1 .or. amptype > 10 .or. process_infos(p)%CC /= "0", found, "CC NOT ok.")
      call check(trim(process_infos(p)%APPROX) == trim(approximation) .or. len_trim(allowed_libs) /= 0, found, "APPROX NOT ok.")
    else
      found = .false.
    end if

    contains

    subroutine check(test,found,message)
    implicit none
    logical, intent(in) :: test
    logical, intent(inout) :: found
    character(len=*), intent(in) :: message

    if (.not. test) then
      found = .false.
      if (verbose > 2) print*, "[OpenLoops] Library not suitable: " // trim(message)
    end if

    end subroutine


  end subroutine check_parameters


  subroutine readAllInfoFiles(ierr, load_channel_lib)
    use ol_parameters_decl_/**/DREALKIND, only: &
      & install_path, tmp_dir, pid_string
    use iso_fortran_env, only: iostat_end
#ifdef USE_IFORT
    use ifport, only: system
#endif
    implicit none
    logical, optional, intent(in) :: load_channel_lib
    integer, intent(out) :: ierr
    integer :: readok
    integer, parameter :: gf_list = 995, gf_info = 994
    integer :: counter, sysstate
    character(len=max_parameter_length) :: infofile_list
    character(len=500) :: sys_list_info
    character(len=500) :: infofilename
    character(len=500) :: infoline
    character(len=5) :: info_file_suffix = 'info'
    logical :: iqopen
    character(len=4) :: old_type

    type(processinfos) infos
    type(processinfos), save, allocatable :: process_infos_bak(:)

    ierr = 0

    if (present(load_channel_lib)) then
      if (load_channel_lib) then
          info_file_suffix = 'rinfo'
      end if
    end if

    infofile_list = trim(tmp_dir) // "/OL_output_list_" // trim(pid_string) // ".tmp"
    sys_list_info = 'ls -1 ' // &
                    &  trim(install_path) // '/proclib/*.' // info_file_suffix  // &
                    &  ' > ' // infofile_list  // &
                    &  ' 2>  /dev/null'

    call system(sys_list_info, sysstate)
    if (sysstate /= 0) then
      ierr = 1
      return
    end if

    inquire(gf_list, opened=iqopen)
    if(iqopen) close(unit=gf_list)
    open(gf_list, file=trim(infofile_list), status = "old", iostat=readok)
    if (readok /= 0) then
      print *, "[OpenLoops] readAllInfoFiles can't open temporary file."
      ierr = 1
      return
    end if

    InfoListLoop: do
      read (gf_list, '(A)', iostat=readok )  infofilename
      if (readok /= 0) then ! EOF -> exit
        if (readok == iostat_end) then
          exit InfoListLoop
        else
          print *, "[OpenLoops] redAllInfoFiles error reading temporary file."
          ierr = 1
          exit
        end if
      end if

      inquire(gf_info, opened=iqopen)
      if(iqopen) close(unit=gf_info)
      open(gf_info, file=trim(infofilename), status = "old", iostat=readok)
      if (readok /= 0) then
        print *, "[OpenLoops] readAllInfoFiles: can't open file " // infofilename
        ierr = 1
        exit
      end if
      counter = 0
      InfoFileLoop: do
        read (gf_info, '(A)', iostat=readok )  infoline
        if (readok /= 0) then ! EOF -> exit
          if (readok == iostat_end) then
            exit InfoFileLoop
          else
            print *, "[OpenLoops] redAllInfoFiles error reading file " // infofilename
            ierr = 1
            exit
          end if
        end if


        ! strip empty lines
        if (len_trim(infoline) == 0) then
          cycle InfoFileLoop
        end if

        infoline = adjustl(infoline)
        ! strip possible comment: start with #
        if (infoline(1:1) == "#") then
          cycle InfoFileLoop
        end if

        ! strip lines starting with OPTIONS=... (deprecated)
        if (infoline(1:8) == "options ") then
          cycle InfoFileLoop
        end if

        counter = counter+1
        ! strip first line of collection files
        if (info_file_suffix == 'rinfo' .and. counter == 1) then
          cycle
        end if

        ! read all Infos
        if (info_file_suffix == 'rinfo') then
          infos%LTYPE = "lib"
        else
          infos%LTYPE = infofilename(index(infofilename,'_',.true.)+1:index(infofilename,'.info')-1)
        end if

        call readAllInfos(infoline, infos)

        ! Add to array of infos
        if (.not. allocated(process_infos)) then
          allocate(process_infos(1))
        else
          allocate(process_infos_bak(size(process_infos)))
          process_infos_bak = process_infos
          deallocate(process_infos)
          allocate(process_infos(size(process_infos_bak)+1))
          process_infos(1:size(process_infos_bak)) = process_infos_bak
          deallocate(process_infos_bak)
        end if
        process_infos(size(process_infos)) = infos
      end do InfoFileLoop
    end do InfoListLoop

    inquire(gf_info, opened=iqopen)
    if(iqopen) close(unit=gf_info)

    inquire(gf_list, opened=iqopen)
    if(iqopen) close(unit=gf_list)

    call system("rm -f " // infofile_list )

    contains

    subroutine readAllInfos(lineinfo, infos)
      implicit none
      character(len=*), intent(in) :: lineinfo
      type(processinfos), intent(inout) :: infos
      integer :: ccount

      call readInfoCol(lineinfo, 1, infos%LIBNAME)
      call readInfoCol(lineinfo, 2, infos%PROC)

      if (index(lineinfo, 'map') > 0) then
        if (index(lineinfo, ' map') > 0) then
          call readInfo(lineinfo, 'map', infos%MAP)
        else if (index(lineinfo, ' condmap') > 0) then
          call readInfo(lineinfo, 'condmap', infos%MAP)
        else
          print*, "[OpenLoops] error: mapping not supported!"
          stop
        end if
        infos%MAPPERM =  infos%MAP(index(infos%MAP, '[')+1:index(infos%MAP, ']')-1)
        if (len_trim(infos%MAPPERM) /= 0) then
          infos%MAP = infos%MAP(1:index(infos%MAP,'[')-1)
        end if
        infos%ID = 0
      else
        call readInfoColInt(lineinfo, 3, infos%ID)
        call readInfoCoupling(lineinfo, 'QCD', infos%QCDorder)
        call readInfoCoupling(lineinfo, 'EW', infos%EWorder)
        infos%MAP = ''
        infos%MAPPERM = ''
      end if
      call readInfo(lineinfo, 'ME', infos%ME)
      call readInfo(lineinfo, 'MM', infos%MM)
      call readInfo(lineinfo, 'ML', infos%ML)
      call readInfo(lineinfo, 'MU', infos%MU)
      call readInfo(lineinfo, 'MD', infos%MD)
      call readInfo(lineinfo, 'MS', infos%MS)
      call readInfo(lineinfo, 'MC', infos%MC)
      call readInfo(lineinfo, 'MB', infos%MB)
      call readInfoInt(lineinfo, 'YukE', infos%YE)
      call readInfoInt(lineinfo, 'YukM', infos%YM)
      call readInfoInt(lineinfo, 'YukL', infos%YL)
      call readInfoInt(lineinfo, 'YukU', infos%YU)
      call readInfoInt(lineinfo, 'YukD', infos%YD)
      call readInfoInt(lineinfo, 'YukS', infos%YS)
      call readInfoInt(lineinfo, 'YukC', infos%YC)
      call readInfoInt(lineinfo, 'YukB', infos%YB)
      call readInfoInt(lineinfo, 'YukT', infos%YT)
      call readInfo(lineinfo, 'APPROX', infos%APPROX)
      call readInfoInt(lineinfo, 'CKMORDER', infos%CKMorder)
      call readInfoInt(lineinfo, 'nc', infos%NC)
      call readInfoInt(lineinfo, 'nf', infos%NF)
      call readInfoInt(lineinfo, 'LeadingColour', infos%LeadingColour)
      call readInfo(lineinfo, 'CC', infos%CC)
      call readInfo(lineinfo, 'MODEL', infos%Model)
      if (len_trim(infos%Model) == 0) then
        infos%Model = "sm"
      end if
      call readInfo(lineinfo, 'Type', infos%TYPE)
      if (trim(infos%TYPE) == "") then
        infos%TYPE = infos%LTYPE
      end if
    end subroutine readAllinfos

    subroutine readInfo(lineinfo, var, res)
      implicit none
      character(len=*), intent(in) :: lineinfo
      character(len=*), intent(in) :: var
      character(len=*), intent(out) :: res
      if (index(lineinfo, ' '//var//'=') /= 0) then
        res = lineinfo(index(lineinfo, var//'=')+len_trim(var)+1: &
              & index(lineinfo, var//'=')+index(lineinfo(index(lineinfo, var//'='):),' ')-1 )
      else
        res = ""
      end if
    end subroutine readInfo

    subroutine readInfoCol(lineinfo, col, res)
      implicit none
      character(len=*), intent(in) :: lineinfo
      integer, intent(in)  :: col
      character(len=*), intent(out) :: res
      integer sstart, send, i
      sstart = 1
      do i=1,col-1
        sstart = sstart + index(lineinfo(sstart:), " ")
      end do
      send = sstart + index(lineinfo(sstart:), " ") - 2
      res = trim(lineinfo(sstart:send))
    end subroutine readInfoCol

    subroutine readInfoColInt(lineinfo, col, res)
      use ol_generic, only: to_int
      implicit none
      character(len=*), intent(in) :: lineinfo
      integer, intent(in)  :: col
      integer, intent(out) :: res
      character(len=max_parameter_length) :: resc
      integer sstart, send
      call readInfoCol(lineinfo,col, resc)
      res = to_int(trim(resc))
      if (res == -huge(res)) then
        if (verbose > 0) then
          print*, "[OpenLoops] Warning: problem reading info line: ", trim(lineinfo)
        end if
      end if
    end subroutine readInfoColInt

    subroutine readInfoInt(lineinfo, var, res)
      use ol_generic, only: to_int
      implicit none
      character(len=*), intent(in) :: lineinfo
      character(len=*), intent(in) :: var
      character(len=10) :: restemp
      integer, intent(out) :: res
      if (index(lineinfo, var//'=') /= 0) then
        restemp = lineinfo(index(lineinfo, var//'=')+len_trim(var)+1: &
            & index(lineinfo, var//'=')+index(lineinfo(index(lineinfo, var//'='):),' ')-1)
      else
        restemp = "0"
      end if
      res = to_int(restemp)
      if (res == -huge(res)) then
        if (verbose > 0) then
          print*, "[OpenLoops] Warning: problem reading info line: ", trim(lineinfo)
        end if
      end if
    end subroutine readInfoInt

    subroutine readInfoCoupling(lineinfo, var, res)
      use ol_generic, only: to_int
      implicit none
      character(len=*), intent(in) :: lineinfo
      character(len=*), intent(in) :: var
      integer ::  stat(0:1)
      character(len=max_parameter_length) :: restempc
      integer, intent(out) :: res(0:1)
      if (index(lineinfo, var//'=') /= 0) then
        call readInfo(lineinfo,var,restempc)
        res(0) = to_int(trim(restempc(1:index(restempc,",")-1)))
        res(1) = to_int(trim(restempc(index(restempc,",")+1:)))
      else
        res(0) = 0
        res(1) = 0
      end if
      if (any(res == -huge(res))) then
        if (verbose > 0) then
          print*, "[OpenLoops] Warning: problem reading info line: ", trim(lineinfo)
        end if
      end if
    end subroutine readInfoCoupling

  end subroutine readAllInfoFiles


   subroutine add_loaded_library(infos)
   ! add infos of loaded library to the list of already loaded libraries.
    implicit none
    type(processinfos), intent(in) :: infos
    type(processinfos), allocatable :: loaded_libs_bak(:)
    integer :: i,loaded_libs_last
    if (allocated(loaded_libs)) then
      do i = 1, size(loaded_libs)
        if ( trim(infos%LIBNAME) == trim(loaded_libs(i)%LIBNAME) .and.  &
          &   trim(infos%TYPE) == trim(loaded_libs(i)%TYPE) ) return
      end do
      loaded_libs_last = size(loaded_libs)
      allocate(loaded_libs_bak(loaded_libs_last))
      loaded_libs_bak = loaded_libs
      deallocate(loaded_libs)
      allocate(loaded_libs(loaded_libs_last+1))
      loaded_libs(1:loaded_libs_last) = loaded_libs_bak
      deallocate(loaded_libs_bak)
    else
     loaded_libs_last = 0
     allocate(loaded_libs(1))
    end if
    loaded_libs(loaded_libs_last+1) = infos
   end subroutine


  subroutine flavour_mapping(ext)
!   Lepton & quark flavour mapping
!   Concept (taken from old Sherpa interface):
!   (1) Given a final state, determine the four (anti)lepton/neutrino
!       multiplicities in the given process:
!       a_nubar, a_nu, a_lbar, a_l
!   (2) Compute the discriminant N[i] as
!       N[i] = Ngen - i + Ngen*(a_nubar + a_nu*Nmax + a_lbar*Nmax^2 + a_l*Nmax^3)
!       where Nmax should be chosen such that a_...<=Nmax.
!       In practice one can safely set Nmax=10 and it will work for any
!       process with <= 20 final-state leptons.
!       It is also convenient to set Ngen=10, although i runs only from 1 to 3.
!   (3) Reassign the lepton generations with a permutation
!       p1 -> 1, p2 -> 2, p3 -> 3  such that  N[p1] > N[p2] > N[p3] */
    implicit none
    integer, intent(inout) :: ext(:)
    integer, allocatable :: new_ext(:)
    integer i, j
    integer Ngen, Nlgen, Nqgen, Nmax
    integer l_gen, nu_gen, l_gen_new, nu_gen_new
    integer d_gen, u_gen, d_gen_new, u_gen_new
    integer a_nu, a_nubar, a_l, a_lbar
    integer a_u, a_ubar, a_d, a_dbar
    integer Nl(3,2), Nq(2,2)
    real(DREALKIND) mtau, mmu, mc
    integer perm(size(ext))

    Ngen=10
    Nmax=10

    call get_parameter("tau_mass", mtau)
    call get_parameter("mu_mass", mmu)
    if (mtau == 0 .and. mmu == 0) then
      Nlgen = 3
    else if (mtau /= 0. .and. mmu == 0) then
      Nlgen = 2
    else
      Nlgen = 1
    end if

    call get_parameter("c_mass", mc)
    if (mc == 0) then
      Nqgen = 2
    else
      Nqgen = 1
    end if

    allocate(new_ext(size(ext)))
    new_ext = ext

    !lepton flavour mapping
    do i = 1, Nlgen
      l_gen=9+2*i;
      nu_gen=10+2*i;

      a_nu=count_flav(ext,nu_gen);
      a_nubar=count_flav(ext,-nu_gen);
      a_l=count_flav(ext,l_gen);
      a_lbar=count_flav(ext,-l_gen);

      Nl(i,1)=Ngen-i+Ngen*(a_nubar+a_nu*Nmax+a_lbar*Nmax*Nmax+a_l*Nmax*Nmax*Nmax)
      Nl(i,2)=i
    end do

    call sort_pair(Nl,Nlgen)

    do i = 1, Nlgen
      l_gen=9+2*Nl(i,2);
      l_gen_new=9+2*i;
      nu_gen=10+2*Nl(i,2);
      nu_gen_new=10+2*i;

      do j = 1, size(ext)
        if (abs(ext(j))==nu_gen) new_ext(j)=sign(nu_gen_new,ext(j))
        if (abs(ext(j))==l_gen)  new_ext(j)=sign(l_gen_new,ext(j))
      end do
    end do

    ext = new_ext

    !quark flavour mapping
    do i = 1, Nqgen
      d_gen=2*i-1;
      u_gen=2*i;

      a_d=count_flav(ext,d_gen);
      a_dbar=count_flav(ext,-d_gen);
      a_u=count_flav(ext,u_gen);
      a_ubar=count_flav(ext,-u_gen);

      Nq(i,1)=Ngen-(i+1) + Ngen*(a_ubar+a_u*Nmax+a_dbar*Nmax*Nmax+a_d*Nmax*Nmax*Nmax)
      Nq(i,2)=i
    end do

    call sort_pair(Nq,Nqgen)

    do i = 1, Nqgen
      d_gen=2*Nq(i,2)-1;
      d_gen_new=2*i-1;
      u_gen=2*Nq(i,2);
      u_gen_new=2*i;

      do j = 1, size(ext)
        if (abs(ext(j))==u_gen) new_ext(j)=sign(u_gen_new,ext(j))
        if (abs(ext(j))==d_gen) new_ext(j)=sign(d_gen_new,ext(j))
      end do
    end do

    ext = new_ext

    !inverse normal ordering
!    print*, perm
!    do i = 1, size(ext)
!      ext(i) = new_ext(perm(i))
!    end do
    deallocate(new_ext)

    contains

    function count_flav(ext, pid)
    ! count frequency of integer pid in array ext
      implicit none
      integer, intent(in) :: ext(:)
      integer, intent(in) :: pid
      integer :: count_flav
      integer i

      count_flav = 0
      do i = 1, size(ext)
        if (ext(i) == pid) count_flav = count_flav+1
      end do
    end function count_flav

    subroutine sort_pair(a,n)
    ! Simple insertion sort. Sorting descending on the first component of a 2-tuple of length n
      integer, intent(in) :: n
      integer, intent(inout), dimension(:,:) :: a
      integer :: temp(2)
      integer :: i, j

      do i = 2, n
        j = i - 1
        temp(1) = a(i,1)
        temp(2) = a(i,2)
        do while (j>=1 .and. a(j,1)<temp(1))
          a(j+1,1) = a(j,1)
          a(j+1,2) = a(j,2)
          j = j - 1
          if (j==0) exit
        end do
        a(j+1,1) = temp(1)
        a(j+1,2) = temp(2)
      end do
    end subroutine sort_pair

  end subroutine


  function write_shop_list(ext, proc)
    use ol_parameters_decl_/**/DREALKIND, only: shopping_list, order_ew, order_qcd, &
      & rMU, rMD, rMC, rMS, rMB, rME, rMM, rML
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: ext(:)
    character(len=max_parameter_length), intent(in) :: proc
    integer write_shop_list
    character(len=500) :: output
    character(len=max_parameter_length), allocatable :: shopped_processes_bak(:)
    integer, save :: id = 1
    integer ::  readok
    integer :: i, already_shopped
    integer :: oqcd, oew
    logical :: set_masses = .false.
    logical :: iqopen

    write_shop_list = -1

    if ( .not. (shopping_list_open) ) then
      inquire(fh_shopping, opened=iqopen)
      if(iqopen) close(unit=fh_shopping)
      !open shopping list
      open(fh_shopping, file=trim(shopping_list), status = "replace", iostat=readok)
      if (readok /= 0) then
        print*, "[OpenLoops] Error opening shopping list ", trim(shopping_list)
        return
      end if
      ! write header
      write(fh_shopping,'(A)') ""
      if (order_ew /= -1) then
        oqcd = size(ext)-2-order_ew
        write(fh_shopping,'(A)') "SelectCoupling = (Exponent[#, gQCD] == " // trim(to_string(oqcd)) // " + 2 * #2 &);"
        write(fh_shopping,'(A)') "SelectInterference = {eQED -> " // trim(to_string(order_ew*2)) // "};"
        write(fh_shopping,'(A)') "UnitaryGauge = True;"
        write(fh_shopping,'(A)') ""
      else if (order_qcd /= -1) then
        oew = size(ext)-2-order_qcd
        write(fh_shopping,'(A)') "SelectCoupling = (Exponent[#, eQED] == " // trim(to_string(oew)) // " + 2 * #2 " &
          & // " ||  Exponent[#1, eQED] == " // trim(to_string(oew+2))  // " - 2 * #2 &);"
        write(fh_shopping,'(A)') "SelectInterference = {gQCD -> " // trim(to_string(order_qcd*2)) // "};"
        write(fh_shopping,'(A)') "UnitaryGauge = False;"
        write(fh_shopping,'(A)') ""
      end if
      shopping_list_open = .true.
    end if

    if (allocated(shopped_processes)) then
      do i = 1, size(shopped_processes)
        if ( trim(proc) == trim(shopped_processes(i)) ) then
          if (verbose > 1) then
            print*, "[OpenLoops] Not written to shopping list. Already shopped as process " &
                    & // trim(to_string(i)) // ": " //trim(proc)
          end if
          write_shop_list = i
          return
        end if
      end do
      already_shopped = size(shopped_processes)
      allocate(shopped_processes_bak(already_shopped))
      shopped_processes_bak = shopped_processes
      deallocate(shopped_processes)
      allocate(shopped_processes(already_shopped+1))
      shopped_processes(1:already_shopped) = shopped_processes_bak
      deallocate(shopped_processes_bak)
    else
      already_shopped = 0
      allocate(shopped_processes(1))
    end if
    shopped_processes(already_shopped+1) = trim(proc)

    ! process name and id
    output = "(* " //trim(proc)// " *) AddProcess[FeynArtsProcess -> "
    ! inital state
    output = trim(output) // " {" // trim(PDGtoFA(ext(1))) // ", " // trim(PDGtoFA(ext(2))) //  "} -> {"
    ! final state
    do i=3,size(ext)
      output = trim(output) // trim(PDGtoFA(ext(i)))
      if (i /= size(ext)) output = trim(output) // ", "
    end do
    output = trim(output) // "}"

    ! massive fermions
    set_masses = .false.
    if (rMU /= 0 .or. rMD /= 0 .or. rMC /= 0 .or. rMS /= 0 .or. rMB /= 0 .or. &
      & rME /= 0 .or. rMM /= 0 .or. rML /= 0) then
      output = trim(output) // ", SetParameters -> JoinOptions[{"
      if (rMU /= 0) then
        output = trim(output) // "MU -> MU"
        set_masses = .true.
      end if
      if (rMD /= 0) then
        if (set_masses) output = trim(output) // ","
        output = trim(output) // "MD -> MD"
        set_masses = .true.
      end if
      if (rMC /= 0) then
        if (set_masses) output = trim(output) // ","
        output = trim(output) // "MC -> MC"
        set_masses = .true.
      end if
      if (rMS /= 0) then
        if (set_masses) output = trim(output) // ","
        output = trim(output) // "MS -> MS"
        set_masses = .true.
      end if
      if (rMB /= 0) then
        if (set_masses) output = trim(output) // ","
        output = trim(output) // "MB -> MB"
        set_masses = .true.
      end if
      if (rME /= 0) then
        if (set_masses) output = trim(output) // ","
        output = trim(output) // "ME -> ME"
        set_masses = .true.
      end if
      if (rMM /= 0) then
        if (set_masses) output = trim(output) // ","
        output = trim(output) // "MM -> MM"
        set_masses = .true.
      end if
      if (rML /= 0) then
        if (set_masses) output = trim(output) // ","
        output = trim(output) // "ML -> ML"
        set_masses = .true.
      end if
      output = trim(output) // "}]"
    end if

    output = trim(output) // "];"

    if (verbose > 0) then
      print*, "[OpenLoops] Write to shopping list "// trim(shopping_list)  //": " // trim(output)
    end if

    ! write process to shopping list
    write(fh_shopping,'(A)') trim(output)

    write_shop_list = id
    id = id+1
  end function write_shop_list



  function PDGtoFA(pdg)
  ! PDG number scheme -> FeynArts naming scheme
    implicit none
    integer, intent(in) :: pdg
    character(len=10) :: PDGtoFA

    if (pdg < 0 .and. pdg /= -24) then
      PDGtoFA = "-"
    else if (pdg == 24) then
      PDGtoFA = "-"
    else
      PDGtoFA = ""
    end if

    select case (abs(pdg))
      case  (1)
        PDGtoFA = trim(PDGtoFA) // "F[4,{1}]"
      case  (2)
        PDGtoFA = trim(PDGtoFA) // "F[3,{1}]"
      case  (3)
        PDGtoFA = trim(PDGtoFA) // "F[4,{2}]"
      case  (4)
        PDGtoFA = trim(PDGtoFA) // "F[3,{2}]"
      case  (5)
        PDGtoFA = trim(PDGtoFA) // "F[4,{3}]"
      case  (6)
        PDGtoFA = trim(PDGtoFA) // "F[3,{3}]"
      case  (11)
        PDGtoFA = trim(PDGtoFA) // "F[2,{1}]"
      case  (12)
        PDGtoFA = trim(PDGtoFA) // "F[1,{1}]"
      case  (13)
        PDGtoFA = trim(PDGtoFA) // "F[2,{2}]"
      case  (14)
        PDGtoFA = trim(PDGtoFA) // "F[1,{2}]"
      case  (15)
        PDGtoFA = trim(PDGtoFA) // "F[2,{3}]"
      case  (16)
        PDGtoFA = trim(PDGtoFA) // "F[1,{3}]"
      case  (21,9)
        PDGtoFA = "V[5]"
      case  (22)
        PDGtoFA = "V[1]"
      case  (23)
        PDGtoFA = "V[2]"
      case  (24)
        PDGtoFA = trim(PDGtoFA) // "V[3]"
      case  (25)
        PDGtoFA = "S[1]"
      case default
        print*, "[OpenLoops] Error: only SM particles are allowed!"
        PDGtoFA = "?"
    end select

  end function PDGtoFA


  function register_process_c(process, amptype) bind(c,name="ol_register_process")
    use ol_iso_c_utilities, only: c_f_string
    use ol_parameters_decl_/**/DREALKIND,  only: max_parameter_length
    implicit none
    character(kind=c_char), dimension(*), intent(in) :: process
    integer(c_int), value :: amptype
    integer(c_int) :: register_process_c
    character(len=max_parameter_length) :: f_process
    integer :: f_amptype
    f_amptype = amptype
    call c_f_string(process, f_process, max_parameter_length)
    register_process_c = register_process(f_process, f_amptype)
  end function register_process_c


  subroutine stop_invalid_id(id)
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id
    if (id <= 0 .or. id > last_process_id) then
      print *, "[OpenLoops] Error: no registered process with id " // to_string(id)
      stop
    end if
  end subroutine stop_invalid_id


  pure function amplitudetype(id)
    ! [in] id: a process id
    ! return amptype of type integer
    implicit none
    integer, intent(in) :: id
    integer amplitudetype
    ! call stop_invalid_id(id) not possible here,
    ! because 'print' and 'stop' are not allowed in pure functions.
    if (id <= 0 .or. id > last_process_id) then
      amplitudetype = 0
    else
      amplitudetype = process_handles(id)%amplitude_type
    end if
  end function amplitudetype


  function amplitudetype_c(id) bind(c,name="ol_amplitudetype")
    ! [in] id: a process id
    ! return amptype of type integer
    implicit none
    integer(c_int), intent(in) :: id
    integer(c_int) amplitudetype_c
    call stop_invalid_id(int(id))
    amplitudetype_c = process_handles(int(id))%amplitude_type
  end function amplitudetype_c


  pure function n_external(id)
    implicit none
    integer, intent(in) :: id
    integer :: n_external
    ! call stop_invalid_id(id) not possible here,
    ! because 'print' and 'stop' are not allowed in pure functions.
    if (id <= 0 .or. id > last_process_id) then
      n_external = 0
    else
      n_external = process_handles(id)%n_particles
    end if
  end function n_external


  function n_external_c(id) bind(c,name="ol_n_external")
    implicit none
    integer(c_int), value :: id
    integer(c_int) :: n_external_c
    call stop_invalid_id(int(id))
    n_external_c = process_handles(int(id))%n_particles
  end function n_external_c


  subroutine phase_space_point(id, sqrt_s, psp)
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: sqrt_s
    real(DREALKIND), intent(out) :: psp(:,:)
    type(process_handle) :: subprocess
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    call subprocess%set_permutation(subprocess%permutation)
    n_scatt = subprocess%n_in
    call subprocess%rambo(sqrt_s, psp)
  end subroutine phase_space_point


  subroutine phase_space_point_c(id, sqrt_s, pp) bind(c,name="ol_phase_space_point")
    implicit none
    integer(c_int), value :: id
    real(c_double), value :: sqrt_s
    real(c_double), intent(out) :: pp(5*n_external(id))
    integer :: f_id
    real(DREALKIND) :: f_sqrt_s
    real(DREALKIND) :: f_psp(0:3,n_external(id))
    ! call stop_invalid_id(id) not needed here
    f_id = id
    f_sqrt_s = sqrt_s
    call phase_space_point(f_id, f_sqrt_s, f_psp)
    do f_id = 1, n_external(id)
      pp(5*(f_id-1)+1:5*(f_id-1)+4) = f_psp(0:3,f_id)
      pp(5*f_id) = -1
    end do
  end subroutine phase_space_point_c


  subroutine tree_colbasis_dim(id, ncolb, colelemsz, nhel)
    ! for process with id 'id' return
    ! ncolb = number of tree colour basis elements;
    ! colelemsz = number of colour colour indices in a colour basis element
    ! nhel = number of helicity configuration, including those which vanish
    implicit none
    integer, intent(in) :: id
    integer, intent(out) :: ncolb, colelemsz, nhel
    integer :: extcols(n_external(id)), ncoupl, maxpows, ncolext
    call stop_invalid_id(id)
    if (.not. associated(process_handles(id)%tree_colbasis_dim)) then
      print *, "[OpenLoops] Error: colour basis information is not available"
      print *, "                   for process " // process_handles(id)%process_name
      stop
    end if
    call process_handles(id)%tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel)
    ncolext = count(extcols /= 0)
    colelemsz = ncolext/2 + ncolext - 1
  end subroutine tree_colbasis_dim


  subroutine tree_colbasis_dim_c(id, ncolb, colelemsz, nhel) bind(c,name="ol_tree_colbasis_dim")
    implicit none
    integer(c_int), value :: id
    integer(c_int), intent(out) :: ncolb, colelemsz, nhel
    integer :: f_ncolb, f_colelemsz, f_nhel
    ! call stop_invalid_id(id) not needed here
    call tree_colbasis_dim(int(id), f_ncolb, f_colelemsz, f_nhel)
    ncolb = f_ncolb
    colelemsz = f_colelemsz
    nhel = f_nhel
  end subroutine tree_colbasis_dim_c


  pure function get_tree_colbasis_dim(id)
    ! number of tree colour basis elements; used to declare array sizes
    implicit none
    integer, intent(in) :: id
    integer :: get_tree_colbasis_dim
    integer :: extcols(n_external(id)), ncoupl, maxpows, nhel
    ! call stop_invalid_id(id) not possible here,
    ! because 'print' and 'stop' are not allowed in pure functions.
    if (id <= 0 .or. id > last_process_id) then
      get_tree_colbasis_dim = 0
    else
      call process_handles(id)%tree_colbasis_dim(extcols, get_tree_colbasis_dim, ncoupl, maxpows, nhel)
    end if
  end function get_tree_colbasis_dim


  pure function tree_colbasis_elemsize(id)
    ! number of coloured external particles; used to declare array sizes
    implicit none
    integer, intent(in) :: id
    integer :: tree_colbasis_elemsize
    integer :: extcols(n_external(id)), ncolb, ncoupl, maxpows, nhel, ncolext
    ! call stop_invalid_id(id) not possible here,
    ! because 'print' and 'stop' are not allowed in pure functions.
    if (id <= 0 .or. id > last_process_id) then
      tree_colbasis_elemsize = 0
    else
      call process_handles(id)%tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel)
      ncolext = count(extcols /= 0)
      tree_colbasis_elemsize = ncolext/2+ncolext-1
      if (ncolext == 0) tree_colbasis_elemsize = 0
    end if
  end function tree_colbasis_elemsize


  pure function get_nhel(id)
    ! number of helicity configurations (all, not just non-vanishing); used to declare array sizes
    implicit none
    integer, intent(in) :: id
    integer :: get_nhel
    integer :: extcols(n_external(id)), ncolb, ncoupl, maxpows
    ! call stop_invalid_id(id) not possible here,
    ! because 'print' and 'stop' are not allowed in pure functions.
    if (id <= 0 .or. id > last_process_id) then
      get_nhel = 0
    else
      call process_handles(id)%tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, get_nhel)
    end if
  end function get_nhel


  subroutine tree_colbasis(id, basis, needed)
    use ol_generic, only: compositions2, nth_permutation
    implicit none
    integer, intent(in) :: id
    integer, intent(out) :: basis(:,:)
    integer, intent(out) :: needed(:,:)
    integer :: extcols(n_external(id)), ncolb, ncoupl, maxpows, nhel
    integer, allocatable :: pbasis(:,:), selected_powers(:,:), perm(:), compos(:,:), compo(:), basiselem(:)
    integer :: ncolext, i, j, k, m, needij
    integer :: ncol2ext(n_external(id)), invextperm(n_external(id))
    logical :: powok
    call stop_invalid_id(id)
    call process_handles(id)%tree_colbasis_dim(extcols, ncolb, ncoupl, maxpows, nhel)
    ! number of coloured external particles
    ncolext = 0
    do i = 1, size(extcols)
      if (extcols(i) /= 0) then
        ncolext = ncolext + 1
        ncol2ext(ncolext) = i
      end if
    end do
    do i = 1, size(invextperm)
      invextperm(process_handles(id)%permutation(i)) = i
    end do
    allocate(pbasis(ncoupl+2,ncolb))
    allocate(selected_powers(maxpows,ncoupl))
    allocate(perm(ncolext))
    call compositions2(compos, ncolext)
    allocate(compo(size(compos,1)))
    allocate(basiselem(ncolext/2+ncolext-1))
    call process_handles(id)%tree_colbasis(pbasis, selected_powers)
    do i = 1, ncolb
      do j = i, ncolb
        needij = 1
        do k = 1, ncoupl
          powok = .false.
          do m = 1, maxpows
            if (pbasis(2+k,i) + pbasis(2+k,j) == selected_powers(m,k)) then
              powok = .true.
              exit
            end if
          end do
          if (.not. powok) then
            needij = 0
            exit
          end if
        end do
        needed(i,j) = needij
        needed(j,i) = needij
      end do
      ! TODO:
      ! - apply crossing
      compo = compos(:,pbasis(1,i))
      perm = nth_permutation([(k, k=1, ncolext)], pbasis(2,i))
      basiselem = 0
      m = 1
      do j = 1, count(compo > 0)
        do k = 1, compo(j)
          basiselem(m+j-1) = invextperm(ncol2ext(perm(m)))
          m = m + 1
        end do
      end do
      basis(:,i) = basiselem
    end do
    deallocate(pbasis)
    deallocate(selected_powers)
    deallocate(perm)
    deallocate(compos)
    deallocate(compo)
    deallocate(basiselem)
  end subroutine tree_colbasis


  subroutine tree_colbasis_c(id, basis, needed) bind(c,name="ol_tree_colbasis")
    implicit none
    integer(c_int), value :: id
    integer(c_int), intent(out) :: basis(tree_colbasis_elemsize(id),get_tree_colbasis_dim(id))
    integer(c_int), intent(out) :: needed(get_tree_colbasis_dim(id),get_tree_colbasis_dim(id))
    integer :: f_basis(tree_colbasis_elemsize(id),get_tree_colbasis_dim(id))
    integer :: f_needed(get_tree_colbasis_dim(id),get_tree_colbasis_dim(id))
    ! call stop_invalid_id(id) not needed here
    call tree_colbasis(int(id), f_basis, f_needed)
    basis = f_basis
    needed = f_needed
  end subroutine tree_colbasis_c


  subroutine start() bind(c,name="ol_start")
    use ol_parameters_decl_/**/DREALKIND,  only: write_params_at_start
    use ol_parameters_init_/**/DREALKIND, only: parameters_write
    implicit none
    call parameters_flush()
    if (write_params_at_start) call parameters_write()
  end subroutine


  subroutine finish() bind(c,name="ol_finish")
    implicit none
    call cleanup()
    call unregister_processes()
    if (shopping_list_open) close(fh_shopping)
  end subroutine finish


  subroutine evaluate_tree(id, psp, res)
   ! Tree matrix element.
   ! [in] id: process id as set by register_process
   ! [in] psp: phase space point
   ! [out] res: squared tree matrix element
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: res
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1)
    real(DREALKIND) :: resmunu
    type(process_handle) :: subprocess
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: tree routine not available for process ' // trim(to_string(id))
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%tree(psp, m2cc, 0, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      & 1, [0], resmunu)
    res = m2cc(0)
  end subroutine evaluate_tree


  subroutine evaluate_tree_c(id, pp, res) bind(c,name="ol_evaluate_tree")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: res
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_res
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_tree(f_id, f_pp(0:3,:), f_res)
    res = f_res
  end subroutine evaluate_tree_c


  subroutine evaluate_tree_colvect(id, psp, amp, nhel)
    ! Tree amplitude as colour vectors for each helicity configuration.
    ! [in] id: process id as set by register_process
    ! [in] psp: phase space point
    ! [out] amp: amp(:,h) is the colour vector for helicity configuration h
    ! [out] nhel: number of non-zero helicity configurations,
    !       amp(:,nhel+1:) contains no information
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    complex(DREALKIND), intent(out) :: amp(:,:)
    integer, intent(out) :: nhel
    real(DREALKIND) :: res
    call evaluate_tree(id, psp, res) ! fill colour vector cache
    call process_handles(id)%tree_colvect(amp, nhel)
  end subroutine evaluate_tree_colvect


  subroutine evaluate_tree_colvect_c(id, pp, amp, nhel) bind(c,name="ol_evaluate_tree_colvect")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double) :: amp(2*get_tree_colbasis_dim(id),get_nhel(id))
    integer(c_int), intent(out) :: nhel
    real(c_double) :: res
    complex(DREALKIND) :: f_amp(get_tree_colbasis_dim(id),get_nhel(id))
    integer :: f_nhel, k, h
    call evaluate_tree_c(id, pp, res) ! fill colour vector cache
    call process_handles(int(id))%tree_colvect(f_amp, f_nhel)
    do h = 1, f_nhel
      do k = 1, size(f_amp,1)
        amp(2*k-1,h) = real(f_amp(k,h))
        amp(2*k,h) = aimag(f_amp(k,h))
      end do
    end do
    nhel = f_nhel
  end subroutine evaluate_tree_colvect_c


  subroutine evaluate_cc(id, psp, tree, cc, ewcc)
   ! Independent color correlated tree matrix elements.
   ! [in] id: process id as set by register_process
   ! [in] psp: phase space point
   ! [out] tree: Born matrix element
   ! [out] cc(n_external*(n_external-1)/2): array with the indepenent color correlated
   !       tree amplitudes C_ij = <M|T_iT_j|M>
   !       cc(i+j(j-1)/2+1) = C_ij with 0 <= i < j <= n_external-1
   ! [out] ewcc: charge correlation for EW i-operator
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: tree, cc(:), ewcc
    type(process_handle) :: subprocess
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1) ! keep +1 for compatibility
    real(DREALKIND) :: resmunu
    integer  :: n_cc, i, j
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: cc routine not available for process ' // trim(to_string(id))
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_cc = subprocess%n_particles*(subprocess%n_particles+1)/2
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%tree(psp, m2cc, 0, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      & n_cc, [(i, i = 0, n_cc)], resmunu)
    tree = m2cc(0)
    ewcc = m2cc(n_cc+1)
    do j = 1, subprocess%n_particles - 1
      do i = 0, j - 1
        cc(i+j*(j-1)/2+1) = m2cc((j+1)*j/2+i+1)
      end do
    end do
  end subroutine evaluate_cc


  subroutine evaluate_cc_c(id, pp, tree, cc, ewcc) bind(c,name="ol_evaluate_cc")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: tree, cc(rval_size(n_external(id),2)), ewcc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_tree, f_cc(rval_size(n_external(id),2)), f_ewcc
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_cc(f_id, f_pp(0:3,:), f_tree, f_cc, f_ewcc)
    tree = f_tree
    cc = f_cc
    ewcc = f_ewcc
  end subroutine evaluate_cc_c


  subroutine evaluate_ccmatrix(id, psp, tree, ccij, ewcc)
   ! Color correlated tree matrix elements.
   ! [in] id: process id as set by register_process
   ! [in] psp: phase space point
   ! [out] tree: squared born matrix element
   ! [out] cc(n_external:n_external): array with the color correlated
   !       tree amplitudes C_ij = <M|T_iT_j|M>
   !       cc(i,j) = C_ij with i,j = 1 <= n_external
   ! [out] ewcc: charge correlation for EW i-operator
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: tree, ccij(:,:), ewcc
    type(process_handle) :: subprocess
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1)
    integer  :: n_cc, i, j
    real(DREALKIND) :: resmunu
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: cc routine not available for process ' // trim(to_string(id))
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_cc = subprocess%n_particles*(subprocess%n_particles+1)/2+1
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%tree(psp, m2cc, 0, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      & n_cc, [(i, i = 0, n_cc)], resmunu)
    do i = 1, subprocess%n_particles
      do j = 1, i
        ccij(i,j) = m2cc(i*(i-1)/2+j)
        if (i /= j) ccij(j,i) = ccij(i,j)
      end do
    end do
    tree = m2cc(0)
    ewcc = m2cc(n_cc+1)
  end subroutine evaluate_ccmatrix


  subroutine evaluate_ccmatrix_c(id, pp, tree, ccij, ewcc) bind(c,name="ol_evaluate_ccmatrix")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: tree, ccij(n_external(id),n_external(id)), ewcc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_tree, f_ccij(n_external(id),n_external(id)), f_ewcc
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_ccmatrix(f_id, f_pp(0:3,:), f_tree, f_ccij, f_ewcc)
    tree = f_tree
    ccij = f_ccij
    ewcc = f_ewcc
  end subroutine evaluate_ccmatrix_c


  subroutine evaluate_sc(id, psp, emitter, polvect, res)
    ! Spin correlated matrix elements.
    ! [in] id: process id as set by register_process
    ! [in] psp: phase space point
    ! [in] int emitter: emitter
    ! [in] polvect: polarisation vector
    ! [out] res(n_external): array with results for each spectator j,
    !       res(j) = 1/mom^2 * <emitter,mu|mom^mu cc_ij mom^nu|j,nu>
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id, emitter
    real(DREALKIND), intent(in) :: psp(:,:), polvect(4)
    real(DREALKIND), intent(out) :: res(:)
    type(process_handle) :: subprocess
    integer :: j, extcombs(n_external(id))
    real(DREALKIND) :: m2sc(0:n_external(id)*(n_external(id)+1)/2+1)
    real(DREALKIND) :: resmunu
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: sc routine not available for process ' // trim(to_string(id))
      stop
    end if
    do j = 1, subprocess%n_particles
      if (j <= emitter) then
        extcombs(j) = emitter*(emitter-1)/2 + j
      else
        extcombs(j) = j*(j-1)/2 + emitter
      end if
    end do
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%tree(psp, m2sc, emitter, polvect, subprocess%n_particles, extcombs, resmunu)
    do j = 1, subprocess%n_particles
      res(j) = m2sc(extcombs(j))
    end do
  end subroutine evaluate_sc


  subroutine evaluate_sc_c(id, pp, emitter, polvect, res) bind(c,name="ol_evaluate_sc")
    implicit none
    integer(c_int), value :: id, emitter
    real(c_double), intent(in) :: pp(5*n_external(id)), polvect(4)
    real(c_double), intent(out) :: res(n_external(id))
    integer :: f_id, f_emitter
    real(DREALKIND) :: f_pp(0:4,n_external(id)), f_polvect(4)
    real(DREALKIND) :: f_res(n_external(id))
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    f_emitter = emitter
    f_polvect = polvect
    call evaluate_sc(f_id, f_pp(0:3,:), f_emitter, f_polvect, f_res)
    res = f_res
  end subroutine evaluate_sc_c


  subroutine evaluate_scpowheg(id, psp, emitter, res, resmunu)
   ! Spin correlated tree matrix elements in POWHEG convention
   ! [in] id: process id as set by register_process
   ! [in] psp: phase space point
   ! [out] res: squared born matrix element
   ! [out] res(4:4): array with the color correlated born matrix element B^(mu,nu)
   ! B^(mu,nu) = sum_l,k M(l)M(k) epsilon_l^mu* epsilon_k^nu
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id, emitter
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: res, resmunu(4,4)
    type(process_handle) :: subprocess
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1)
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: scpowheg routine not available for process ' // trim(to_string(id))
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%tree(psp, m2cc, -emitter, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      &  1, [0], resmunu)
    res = m2cc(0)
  end subroutine evaluate_scpowheg


  subroutine evaluate_scpowheg_c(id, pp, emitter, res, resmunu) bind(c,name="ol_evaluate_scpowheg")
    implicit none
    integer(c_int), value :: id, emitter
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: res, resmunu(4,4)
    integer :: f_id, f_emitter
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_res, f_resmunu(4,4)
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    f_emitter = emitter
    call evaluate_scpowheg(f_id, f_pp(0:3,:), f_emitter, f_res, f_resmunu)
    res = f_res
  end subroutine evaluate_scpowheg_c


  subroutine evaluate_full(id, psp, m2l0, m2l1, ir1, m2l2, ir2, acc)
    use ol_stability
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: m2l0, m2l1(0:2), ir1(0:2), m2l2(0:4), ir2(0:4)
    real(DREALKIND), intent(out) :: acc
    type(process_handle)  :: subprocess
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 1)) then
      write(*,*) '[OpenLoops] evaluate: loop routine not available for process ' // trim(to_string(id))
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%loop(psp, m2l0, m2l1, ir1, m2l2, ir2)
    acc = last_relative_deviation
  end subroutine evaluate_full


  subroutine evaluate_full_c(id, pp, m2l0, m2l1, ir1, m2l2, ir2, acc) bind(c,name="ol_evaluate_full")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: m2l0, m2l1(0:2), ir1(0:2), m2l2(0:4), ir2(0:4), acc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_m2l0, f_m2l1(0:2), f_ir1(0:2), f_m2l2(0:4), f_ir2(0:4)
    real(DREALKIND) :: f_acc
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_full(f_id, f_pp(0:3,:), f_m2l0, f_m2l1, f_ir1, f_m2l2, f_ir2, f_acc)
    m2l0 = f_m2l0
    m2l1 = f_m2l1
    ir1  = f_ir1
    m2l2 = f_m2l2
    ir2  = f_ir2
    acc  = f_acc
  end subroutine evaluate_full_c


  subroutine evaluate_loop(id, psp, m2l0, m2l1, acc)
    implicit none
    integer, intent(in)  :: id
    real(DREALKIND), intent(in)  :: psp(:,:)
    real(DREALKIND), intent(out) :: m2l0, m2l1(0:2)
    real(DREALKIND), intent(out) :: acc
    real(DREALKIND) :: ir1(0:2), m2l2(0:4), ir2(0:4)
    call evaluate_full(id, psp, m2l0, m2l1, ir1, m2l2, ir2, acc)
  end subroutine evaluate_loop


  subroutine evaluate_loop_c(id, pp, m2l0, m2l1, acc) bind(c,name="ol_evaluate_loop")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: m2l0, m2l1(0:2), acc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_m2l0, f_m2l1(0:2)
    real(DREALKIND) :: f_acc
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_loop(f_id, f_pp(0:3,:), f_m2l0, f_m2l1, f_acc)
    m2l0 = f_m2l0
    m2l1 = f_m2l1
    acc  = f_acc
  end subroutine evaluate_loop_c


  subroutine evaluate_loop2(id, psp, res, acc)
    use ol_generic, only: to_string
    implicit none
    integer, intent(in)  :: id
    real(DREALKIND), intent(in)  :: psp(:,:)
    real(DREALKIND), intent(out) :: res
    real(DREALKIND), intent(out) :: acc
    real(DREALKIND) :: m2l0, m2l1(0:2), ir1(0:2), m2l2(0:4), ir2(0:4)
    if (.not. btest(process_handles(id)%content, 2)) then
      write(*,*) '[OpenLoops] evaluate: loop^2 routine not available for process ' // trim(to_string(id))
      stop
    end if
    call evaluate_full(id, psp, m2l0, m2l1, ir1, m2l2, ir2, acc)
    res = m2l2(0)
  end subroutine evaluate_loop2


  subroutine evaluate_loop2_c(id, pp, res, acc) bind(c,name="ol_evaluate_loop2")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: res, acc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_res, f_acc
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_loop2(f_id, f_pp(0:3,:), f_res, f_acc)
    res = f_res
    acc  = f_acc
  end subroutine evaluate_loop2_c


  subroutine evaluate_ct(id, psp, m2l0, m2ct)
    use ol_stability
    use ol_generic, only: to_string
    implicit none
    integer, intent(in)  :: id
    real(DREALKIND), intent(in)  :: psp(:,:)
    real(DREALKIND), intent(out) :: m2l0, m2ct
    type(process_handle)  :: subprocess
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 1)) then
      write(*,*) '[OpenLoops] evaluate: ct routine not available for process ' // trim(to_string(id))
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%ct(psp, m2l0, m2ct)
  end subroutine evaluate_ct


  subroutine evaluate_ct_c(id, pp, m2l0, m2ct) bind(c,name="ol_evaluate_ct")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: m2l0, m2ct
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_m2l0, f_m2ct
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_ct(f_id, f_pp(0:3,:), f_m2l0, f_m2ct)
    m2l0 = f_m2l0
    m2ct = f_m2ct
  end subroutine evaluate_ct_c


  subroutine evaluate_pt(id, psp, m2l0, m2pt, m2l1)
    use ol_stability
    use ol_generic, only: to_string
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in)  :: psp(:,:)
    real(DREALKIND), intent(out) :: m2l0, m2pt, m2l1
    type(process_handle)  :: subprocess
    call stop_invalid_id(id)
    subprocess = process_handles(id)
    if (.not. btest(subprocess%content, 3)) then
      write(*,*) '[OpenLoops] evaluate: ct routine not available for process ' // trim(to_string(id))
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_scatt = subprocess%n_in
    call parameters_flush()
    call subprocess%pt(psp, m2l0, m2pt, m2l1)
  end subroutine evaluate_pt


  subroutine evaluate_pt_c(id, pp, m2l0, m2pt, m2l1) bind(c,name="ol_evaluate_pt")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*n_external(id))
    real(c_double), intent(out) :: m2l0, m2pt, m2l1
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,n_external(id))
    real(DREALKIND) :: f_m2l0, f_m2pt, f_m2l1
    f_id = id
    call stop_invalid_id(f_id) ! needed because of reshape
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_pt(f_id, f_pp(0:3,:), f_m2l0, f_m2pt, f_m2l1)
    m2l0 = f_m2l0
    m2pt = f_m2pt
    m2l1 = f_m2l1
  end subroutine evaluate_pt_c


end module openloops
