
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
  use KIND_TYPES, only: DREALKIND, MaxParticles
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_char, c_int, c_double, c_null_char
  use ol_init, only: set_init_error_fatal, get_error, &
    & set_parameter, get_parameter, parameters_flush, cleanup
  use ol_parameters_decl_/**/DREALKIND,  only: procname_length
  implicit none
  private
  ! from module init_iu
  public :: set_init_error_fatal, get_error
  public :: set_parameter, get_parameter, parameters_flush
  ! process interface
  public :: n_external, amplitudetype, phase_space_point, start, finish
  public :: register_process, register_process_id
  public :: evaluate_tree, evaluate_cc, evaluate_ccmatrix, evaluate_sc, evaluate_scpowheg
  public :: evaluate_full, evaluate_loop, evaluate_loop2, evaluate_ct, evaluate_pt
  ! used in BLHA interface
  public :: rval_size

  type process_handle
    integer :: n_particles = 0
    integer :: max_point = -1
    integer :: tensor_rank = -1
    ! allocatable length character members are not supported in gfortran (tested with 4.8.1)
    character(len=procname_length) :: process_name
    integer, allocatable :: permutation(:)
    type(c_ptr) :: library_handle = c_null_ptr
    integer :: amplitude_type ! 1=Tree, 2=ccTree, 3=scTree, 4=scTree_polvect, 11=Loop, 12=LoopInduced
    integer :: content = 0 ! bitwise: 2^0=tree, 2^1=loop, 2^2=loop2, 2^3=pt
    procedure(), pointer, nopass :: set_permutation => null()
    procedure(), pointer, nopass :: tree => null()
    procedure(), pointer, nopass :: loop => null()
    procedure(), pointer, nopass :: ct => null()
    procedure(), pointer, nopass :: pt => null()
    procedure(), pointer, nopass :: rambo => null()
  end type process_handle

  ! process handle array
  integer, save :: last_process_id = 0
  type(process_handle), save, allocatable :: process_handles(:)

  integer, parameter :: max_length = 255
  type flag
    integer :: InterfaceVersion ! 1=BLHA1, 2=BLHA2
    integer :: Model ! 1=SMdiag
    integer :: CorrectionType ! 1=QCD, 2=EW
    integer :: IRregularization ! 1=CDR
    integer :: EWrenormalisation ! 1=alpha0
    integer :: AmplitudeType ! 1=Tree, 2=ccTree, 3=scTree, 4=scTree_polvect, 11=Loop, 12=LoopInduced
    integer :: PoleCheck ! 0/1
    character(100) :: answer_file_name ! Answer file as string
  end type flag

  type(flag) flags

  type processinfos
    character :: infoME, infoMM, infoML, infoMU, infoMD, infoMS, infoMC, infoMB
    character(len=max_length) :: infoAPPROX
    integer :: infoEWorder(0:1)
    integer :: infoQCDorder(0:1)
    integer :: infoLeadingColour
    integer :: infoNF
    integer :: infoNC
    integer :: infoCKMORDER
  end type processinfos

  ! array for shopping list
  character(len=max_length), save, allocatable :: shopped_processes(:)
  logical, save :: shopping_list_open = .false.
  integer, parameter :: fh_shopping = 98

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
    end select
  end function rval_size



  function get_process_handle(lib, proc, perm, content, amptype)
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
    character(len=*), intent(in) :: proc
    integer, intent(in) :: perm(:), content, amptype
    type(process_handle) :: get_process_handle
    real(DREALKIND) :: masses(MaxParticles)
    procedure(), pointer :: tmp_fun
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
  end function get_process_handle


  function register_process_lib(libname, proc, perm, content, amptype)
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
    integer, intent(in) :: perm(:), content, amptype
    type(c_ptr) :: lib
    integer :: register_process_lib
    integer :: k
    type(process_handle) :: prochandle
    type(process_handle), allocatable :: process_handles_bak(:)
    lib = dlopen(libname, RTLD_LAZY, 2)
    prochandle = get_process_handle(lib, proc, perm, content, amptype)
    ! Check if the process was registered before with the same permutation and amptype.
    ! If yes, return the previously assigned id
    do k = 1, last_process_id
      if ((trim(proc) == trim(process_handles(k)%process_name)) .and. &
        & all(perm == process_handles(k)%permutation) .and. &
        & (amptype == process_handles(k)%amplitude_type)) then
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


  function register_process(process, amptype)
    ! process: string with format 2->n-2
    ! amptype: integer 1,2,3,4,11,12
    ! return (integer) process id to be used in evaluate_process
    implicit none
    character(len=*), intent(in) :: process
    integer, intent(in) :: amptype
    integer :: register_process
    character(len=max_length) :: tmp
    character(len=max_length) :: inp, outp
    integer :: tmpi
    integer :: next
    integer, allocatable :: ext(:)
    integer, allocatable :: ext_bak(:)

    ! split: process->(in,out)
    next = 0
    inp = adjustl(trim(process(1:index(process, "->")-1)))
    outp = adjustl(process(index(process, "->")+2:len(process)))

    allocate(ext(4))
    ! split initial state and check that we have a 2 -> n production process
    tmp = adjustl(inp(1:index(inp, " ")))
    next = next + 1
    read(tmp,*) ext(next)
    tmp = adjustl(inp(index(inp, " "):))
    if (index(tmp, " ") == 1) then
      print *, "[OpenLoops] register_process: invalid argument: " // trim(process)
      return
    else
      next = next + 1
      read(tmp,*) ext(next)
    end if

    ! split final state
    FSloop: do
      tmp = adjustl(outp(1:index(outp, " ")))
      outp = adjustl(outp(index(outp, " "):))
      if (index(tmp, " ") == 1) then
        exit FSloop !no more final state particles
      else
        next = next + 1
        read(tmp,*) tmpi
        if (next > 4) then
          allocate(ext_bak(next))
          ext_bak = ext
          deallocate(ext)
          allocate(ext(next))
          ext(1:next-1) = ext_bak
          deallocate(ext_bak)
        end if
        ext(next) = tmpi
      end if
    end do FSloop

    register_process = register_process_id(ext, amptype)

end function register_process


  function register_process_id(ext_in, amptype)
    ! process: array with format [in_1, in_2, out_1, .. , out_n-2]
    ! amptype: integer 1,2,3,4,11,12
    ! return (integer) process id to be used in evaluate_process
    use ol_parameters_decl_/**/DREALKIND, only: &
      & install_path,flavour_mapping_on, verbose
    use ol_loop_parameters_decl_/**/DREALKIND, only: nf, nc
    use iso_fortran_env, only: iostat_end
    use ol_iso_c_utilities, only: to_lowercase
#ifdef USE_IFORT
    use ifport, only: system
#endif
    implicit none
    integer, intent(in) :: ext_in(:)
    integer, intent(in) :: amptype
    integer :: register_process_id
    character(len=4) :: loops_flags
    character(len=max_length) :: proc
    character(len=max_length) :: loaded_library
    character :: coupling_order(2)
    character(len=max_length) :: process
    integer :: lib_content
    integer :: ierrparam, i
    integer :: check
    integer :: libid
    integer :: next
    integer :: ext(size(ext_in))
    logical :: proclib_exists
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
      print *, "[OpenLoops] register_process: proclib folder not found, check install_path."
      return
    end if

    next =  size(ext_in)
    ! check
    if (next < 3) then
      print*, "[OpenLoops] Error: 1 -> 1 not supported!"
      return
    else if (next < 4) then
      print*, "[OpenLoops] Error: 2 -> 1 not supported!"
      return
    end if

    ! write process string for output
    process = trim(itoc(ext_in(1))) // " " // trim(itoc(ext_in(2))) // " ->"
    do i=3,next
      process = trim(process) // " " // trim(itoc(ext_in(i)))
    end do

    ext = ext_in
    ! charge conjugate final state particles
    call charge_conj(ext)

    ! flavour mapping
    if (flavour_mapping_on == 1) then
      if (verbose > 2) then
        print*, "[OpenLoops] Flav mapping. Original (all ingoing) process: ", ext(1:next)
      end if
      call flavour_mapping(ext(1:next))
      if (verbose > 2) then
        print*, "[OpenLoops] Flav mapping. Mapped (all ingoing) process:   ", ext(1:next)
      end if
    end if

    ! determine normal ordering
    allocate (perm(next))
      call normal_order(ext(1:next), perm, proc)
    if (proc == "") return

    if (amptype == 99) then ! write shopping list
      ! charge conjugate back final state particles to write shopping list
      call charge_conj(ext)
      register_process_id = write_shopping_list(ext(1:next), proc)
    else
      librarytype = 0
      do
        check = check_process(proc, perm, amptype, librarytype, loaded_library)
        if (check > 0) then ! found & registered
          if (verbose > 0) then
            print *, "[OpenLoops] Loaded library for process " //  trim(process) // " : " // trim(loaded_library)
          end if
          register_process_id = check
          exit
        else if (check == 0) then ! look in next library type
          librarytype = librarytype + 1
        else
          if (check == -1) then  ! not found --> check collections
            check = check_process(proc, perm, 99, librarytype, loaded_library)
          end if
          if (check == 1) then ! found in collection
            print*, "[OpenLoops] Library for ", trim(process), " not installed but available in: ", trim(loaded_library)
            print*, "[OpenLoops] Note: this library can be downloaded and installed via"
            print*, "[OpenLoops] $ cd " // trim(install_path)
            print*, "[OpenLoops] $ ./scons auto=", trim(loaded_library)
          else ! not found anywhere
            print *, "[OpenLoops] register_process: process " // trim(process) // " not found!"
          end if
          exit
        end if
      end do
    end if

    !deallocate
    deallocate(perm)

  contains

  subroutine charge_conj(x)
    ! determine charge conjugate of array x(3:)
    implicit none
    integer, intent(inout)  :: x(:)

    do i=3,size(x)
      select case(x(i))
      case(0, 21, 22, 23, 25)
        x(i)  = x(i)
      case default
        x(i) = -x(i)
      end select
    end do
  end subroutine charge_conj

  subroutine map_permutation(perm, mapping_str)
    implicit none
    character(len=30), intent(inout) :: mapping_str
    integer, intent(inout) :: perm(:)
    character(len=2) :: str
    integer, allocatable :: mapping(:), perm_tmp(:)
    integer :: i, x
    allocate (mapping(size(perm)))
    allocate (perm_tmp(size(perm)))

    mapping_str=trim(mapping_str) // ','
    do i = 1, size(mapping)
      str = adjustl(mapping_str(0:index(mapping_str,',')-1))
      read(str(2:), '(i10)') mapping(i)
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


  function check_process(proc, perm, amptype, librarytype, loaded_library)
  ! 1: found, 0: not found, -1: abort
    use ol_parameters_decl_/**/DREALKIND, only: &
      & install_path, rME, rMM, rML, rMU, rMD, rMC, rMS, rMB, &
      & leadingcolour, coupling_QCD, coupling_EW, &
      & approximation, ckmorder, allowed_libs, verbose, pid_string, tmp_dir
    implicit none
    character(len=max_length), intent(inout) :: proc
    integer, intent(inout) :: perm(:)
    integer, intent(in) :: amptype, librarytype
    character(len=max_length), intent(out) :: loaded_library
    integer check_process
    integer, parameter :: gf = 96, gf2 = 97
    logical :: opend_gf2 = .false.
    integer :: readok, ierrg
    character(len=500) :: grepstring
    character(len=500) :: finalgrepstring
    character(len=4) :: loops_specification
    character(len=500) :: lineinfo
    character(len=max_length) :: infofilename
    character(len=max_length) :: libfilename, libhandle
    character(len=max_length) :: libname
    character(len=max_length) :: procunmapped
    character(len=max_length) :: perm_str, mapping_str
    character(len=max_length) :: subprocnum
    character(len=3) :: pvalstr
    type(processinfos) infos

    check_process = 0
    mapping_str = ""

    ! find corresponding .info file
    grepstring = 'grep -sH " ' // trim(proc) // ' " ' // trim(install_path) // '/proclib/'
    select case (amptype)
      case (1,2,3,4) ! tree-like
        if (librarytype == 0) then
          loops_specification = "t"
        else if (librarytype == 1) then
          loops_specification = "lt"
        else if (librarytype == 2) then
          loops_specification = "lpt"
        else if (librarytype == 3) then
          loops_specification = "lst"
        else if (librarytype == 4) then
          loops_specification = "lpst"
        else
          check_process = -1
          return
        end if
      case (11) ! loop
        if (librarytype == 0) then
          loops_specification = "lt"
        else if (librarytype == 1) then
          loops_specification = "l"
        else if (librarytype == 2) then
          loops_specification = "lp"
        else if (librarytype == 3) then
          loops_specification = "ls"
        else if (librarytype == 4) then
          loops_specification = "lst"
        else if (librarytype == 5) then
          loops_specification = "lpt"
        else if (librarytype == 6) then
          loops_specification = "lps"
        else if (librarytype == 7) then
          loops_specification = "lpst"
        else
          check_process = -1
          return
        end if
      case (12) ! loop-induced
        if (librarytype == 0) then
          loops_specification = "ls"
        else if (librarytype == 1) then
          loops_specification = "lst"
        else if (librarytype == 2) then
          loops_specification = "lps"
        else if (librarytype == 3) then
          loops_specification = "lpst"
        else
          check_process = -1
          return
        end if
      case (99) ! check libraries

      case default
        print *, "[OpenLoops] register_process: amplitude type not supported:", amptype
    end select

    if (amptype == 99) then
      finalgrepstring = trim(grepstring) // '*.rinfo ' // &
                    & '> ' // trim(tmp_dir) // '/OL_output_' // trim(pid_string) // '.tmp'
    else
      finalgrepstring = trim(grepstring) // '*_' // trim(loops_specification) // '.info ' // &
                    & '> ' // trim(tmp_dir) // '/OL_output_' // trim(pid_string) // '.tmp'
    end if

    ierrg = system(finalgrepstring)

    if (ierrg /= 0) then
      goto 52
    end if

    open(gf, file=trim(tmp_dir) // '/OL_output_' // trim(pid_string) // '.tmp', &
       & status = "old", iostat=readok)
    if (readok /= 0) then
      print *, "[OpenLoops] register_process: can't open temporary file."
      goto 52
      check_process = -2
    end if


    infofilename = "" ! set if following a mapping

    InfoReadLoop: do
      read (gf, '(A)', iostat=readok )  lineinfo

      if (readok /= 0) then ! EOF -> exit
        if (readok == iostat_end) then
          exit InfoReadLoop
        else
          print *, "[OpenLoops] register_process: error reading temporary file."
          check_process = -2
          exit
        end if
      end if

! following line necessary?
!      lineinfo = trim(lineinfo)
      if (len_trim(lineinfo) == 0) then
        cycle InfoReadLoop ! strip empty lines
      end if

      !check if library is "allowed"
      libname = lineinfo(index(lineinfo, ':')+1:index(lineinfo, ' '))
      if (len(trim(allowed_libs)) /= 0 .and. index(allowed_libs, trim(libname)) == 0) then
        cycle InfoReadLoop
      end if

      ! follow mapping
      if(index(lineinfo, 'map=') /= 0) then
        procunmapped = proc
        proc = lineinfo(index(lineinfo, 'map=')+4:index(lineinfo, 'map=')+3+len_trim(proc))
        !next line necessary?
        mapping_str = lineinfo(index(lineinfo, '[')+1:index(lineinfo, ']')-1)
        !check for MB etc.
        call readInfo(lineinfo, 'MB', infos%infoMB)
        if ((infos%infoMB == "0" .and. rMB /= 0) ) then
          if (verbose > 1) then
            print*, "[OpenLoops] Not following massless b-mapping ", trim(procunmapped), " --> ", trim(proc) , ": MB /= 0."
          end if
          proc = procunmapped
          cycle InfoReadLoop
        end if
        if (verbose > 1) then
          print*, "[OpenLoops] Following info-file mapping: ",trim(procunmapped), &
         &        " --> ", trim(lineinfo(index(lineinfo, 'map=')+4:)), "."
        end if
        !map permutation
        if(len(trim(mapping_str)) /= 0) then
          call map_permutation(perm,mapping_str)
          mapping_str = " (mapped from " // trim(procunmapped) // ")"
        end if
        if (amptype == 99) then
          infofilename= lineinfo(index(lineinfo, 'channels'):index(lineinfo, '.rinfo')+5)
        else
          infofilename= lineinfo(index(lineinfo, 'libopenloops'):index(lineinfo, '.info')+4)
        end if
        grepstring = 'grep -sH " ' // trim(proc) // ' " ' // trim(install_path) // '/proclib/'
        finalgrepstring = trim(grepstring) // trim(infofilename) // &
          & ' > ' // trim(tmp_dir) // '/OL_output_mapping_' // trim(pid_string) // '.tmp'
        ierrg = system(finalgrepstring)
        if (ierrg /= 0) then
          print *, "[OpenLoops] register_process: inconsistent mapping in " // trim(proc)
          check_process = -2
          exit
        end if
        open(gf2, file=trim(tmp_dir) // '/OL_output_mapping_' // trim(pid_string) // '.tmp', &
           & status="OLD", iostat=readok)
        if (readok /= 0) then
          print *, "[OpenLoops] register_process: can't open temporary file."
          check_process = -2
          exit
        else
          opend_gf2 = .true.
        end if
      end if

      51 continue
      if (infofilename /= "") then
        read(gf2, '(A)', iostat=readok) lineinfo
        if (readok /= 0) then ! EOF -> exit
          if (readok == iostat_end) then
            close(gf2)
            ierrg = system("rm -f " // trim(tmp_dir) // "/OL_output_mapping_" // trim(pid_string) // ".tmp")
            infofilename = ""
            proc = procunmapped
            cycle InfoReadLoop
          else
            print *, "[OpenLoops] register_process: error reading temporary file."
            check_process = -2
            exit
          end if
        end if
        if (len_trim(lineinfo) == 0) then
          go to 51
        end if
      end if

      ! get library filename
      if (amptype == 99) then
        libfilename = "collection " // lineinfo(index(lineinfo, 'channels'):index(lineinfo, '.rinfo')-1)
      else
        libfilename = lineinfo(index(lineinfo, 'libopenloops'):index(lineinfo, '.info')) // dynlib_extension
      end if
      ! remove path
      lineinfo = lineinfo(index(lineinfo, '.info')+6:)

      subprocnum = lineinfo(index(lineinfo, trim(proc))+len_trim(proc)+1:index(lineinfo, trim(proc))+len_trim(proc)+2)

      ! read all infos from .info file
      call readAllInfos(lineinfo, infos)

      if ((infos%infoEWorder(0) == coupling_EW(0) .or. coupling_EW(0) == -1) &
        & .and. (infos%infoEWorder(1) == coupling_EW(1) .or. coupling_EW(1) == -1) &
        & .and. (infos%infoQCDorder(0) == coupling_QCD(0) .or. coupling_QCD(0) == -1) &
        & .and. (infos%infoQCDorder(1) == coupling_QCD(1) .or. coupling_QCD(1) == -1) &
        & .and. infos%infoLeadingColour == leadingcolour &
        & .and. infos%infoNC == nc &
        & .and. infos%infoNF == nf &
        & .and. infos%infoCKMorder == ckmorder &
        & .and. ((infos%infoME /= "0" .and. rME /= 0) .or. (rME == 0)) &
        & .and. ((infos%infoMM /= "0" .and. rMM /= 0) .or. (rMM == 0)) &
        & .and. ((infos%infoML /= "0" .and. rML /= 0) .or. (rML == 0)) &
        & .and. ((infos%infoMU /= "0" .and. rMU /= 0) .or. (rMU == 0)) &
        & .and. ((infos%infoMD /= "0" .and. rMD /= 0) .or. (rMD == 0)) &
        & .and. ((infos%infoMS /= "0" .and. rMS /= 0) .or. (rMS == 0)) &
        & .and. ((infos%infoMC /= "0" .and. rMC /= 0) .or. (rMC == 0)) &
        & .and. ((infos%infoMB /= "0" .and. rMB /= 0) .or. (rMB == 0)) &
        & .and. (trim(infos%infoAPPROX)==trim(approximation)) &
        & ) then

        ! found correct library
        check_process = 1

        if (verbose > 1) then
          print*, "[OpenLoops] Parameters match info-file for process ", trim(proc), " in library ", trim(libfilename)
        end if

        if (amptype == 99) then
          loaded_library = libname
          exit
         end if

        libfilename = trim(install_path) // '/proclib/' // libfilename
        libhandle = trim(to_lowercase(libname)) // "_" // trim(proc) // "_" // trim(subprocnum)
        lib_content = 0
        do i = 1, len(loops_flags)
          if (index(loops_specification, loops_flags(i:i)) > 0) lib_content = ibset(lib_content, i-1)
        end do

        if (verbose > 0) then
          perm_str = "["
          do i = 1, next - 1
            perm_str = trim(perm_str) // trim(itoc(perm(i))) // ","
          end do
          perm_str = trim(perm_str) // trim(itoc(perm(i))) // "]"
          loaded_library = trim(libhandle) // trim(perm_str) //  trim(mapping_str)
        end if

        !register
        check_process = register_process_lib(libfilename, libhandle, perm, lib_content, amptype)
        exit
      else if (infofilename /= "") then
        if (verbose > 1) then
          print*, "[OpenLoops] Parameters do not match info-file for process ", trim(proc), " in library ", trim(libfilename)
        end if
        go to 51
      else
        if (verbose > 1) then
          print*, "[OpenLoops] Parameters do not match info-file for process ", trim(proc), " in library ", trim(libfilename)
        end if
        check_process = 0
      end if

    end do InfoReadLoop

    52 continue
    close(gf)
    ierrg = system("rm -f " // trim(tmp_dir) // "/OL_output_" // trim(pid_string) // ".tmp")
    if (opend_gf2) then
      close(gf2)
      ierrg = system("rm -f " // trim(tmp_dir) // "/OL_output_mapping_" // trim(pid_string) // ".tmp")
    end if

    end function check_process


    subroutine readAllInfos(lineinfo, infos)
      implicit none
      character(len=*), intent(in) :: lineinfo
      type(processinfos), intent(inout) :: infos

      call readInfoCoupling(lineinfo, 'QCD', infos%infoQCDorder)
      call readInfoCoupling(lineinfo, 'EW', infos%infoEWorder)
      call readInfo(lineinfo, 'ME', infos%infoME)
      call readInfo(lineinfo, 'MM', infos%infoMM)
      call readInfo(lineinfo, 'ML', infos%infoML)
      call readInfo(lineinfo, 'MU', infos%infoMU)
      call readInfo(lineinfo, 'MD', infos%infoMD)
      call readInfo(lineinfo, 'MS', infos%infoMS)
      call readInfo(lineinfo, 'MC', infos%infoMC)
      call readInfo(lineinfo, 'MB', infos%infoMB)
      call readInfo(lineinfo, 'APPROX', infos%infoAPPROX)
      call readInfoInt(lineinfo, 'CKMORDER', infos%infoCKMorder)
      call readInfoInt(lineinfo, 'nc', infos%infoNC)
      call readInfoInt(lineinfo, 'nf', infos%infoNF)
      call readInfoInt(lineinfo, 'LeadingColour', infos%infoLeadingColour)
    end subroutine readAllinfos


    subroutine readInfo(lineinfo, var, res)
      implicit none
      character(len=*), intent(in) :: lineinfo
      character(len=*), intent(in) :: var
      character(len=*), intent(out) :: res
      if (index(lineinfo, var//'=') /= 0) then
        res = lineinfo(index(lineinfo, var//'=')+len_trim(var)+1: &
              & index(lineinfo, var//'=')+index(lineinfo(index(lineinfo, var//'='):),' ')-1 )
      else
        res = ""
      end if
    end subroutine readInfo

    subroutine readInfoInt(lineinfo, var, res)
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
      read(restemp,'(I2)') res
    end subroutine readInfoInt

    subroutine readInfoCoupling(lineinfo, var, res)
      implicit none
      character(len=*), intent(in) :: lineinfo
      character(len=*), intent(in) :: var
      character(len=1) :: restemp(0:1)
      integer, intent(out) :: res(0:1)
      if (index(lineinfo, var//'=') /= 0) then
        restemp(0) =  lineinfo(index(lineinfo, trim(var))+len_trim(var)+1:index(lineinfo, trim(var))+len_trim(var)+2)
        restemp(1) =  lineinfo(index(lineinfo, trim(var))+len_trim(var)+3:index(lineinfo, trim(var))+len_trim(var)+4)
      else
        restemp(0) = "0"
        restemp(1) = "0"
      end if
      read(restemp(0),'(I2)') res(0)
      read(restemp(1),'(I2)') res(1)
    end subroutine readInfoCoupling

  end function register_process_id



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
    if (mtau == 0. .and. mmu == 0) then
      Nlgen = 3
    else if (mtau /= 0. .and. mmu == 0) then
      Nlgen = 2
    else
      Nlgen = 1
    end if

    call get_parameter("c_mass", mc)
    if (mc == 0.) then
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

    do i = 1, 2
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

end subroutine


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

  subroutine normal_ordering(ext, perm)
    implicit none
    integer, intent(inout) :: ext(:)
    integer, intent(inout) :: perm(:)
    integer :: exttemp(size(ext))
    character(len=max_length) :: proc
    integer :: i

    call normal_order(ext, perm, proc)
    exttemp = ext
    do i = 1,size(ext)
      ext(perm(i)) = exttemp(i)
    end do
  end subroutine normal_ordering

  function write_shopping_list(ext, proc)
    use ol_parameters_decl_/**/DREALKIND, only: shopping_list, order_ew, order_qcd, verbose, &
      & rMU, rMD, rMC, rMS, rMB, rME, rMM, rML
    implicit none
    integer, intent(in) :: ext(:)
    character(len=max_length), intent(in) :: proc
    integer write_shopping_list
    character(len=500) :: output
    character(len=max_length), allocatable :: shopped_processes_bak(:)
    integer, save :: id = 1
    integer ::  readok
    integer :: i, already_shopped
    integer :: oqcd, oew
    logical :: set_masses = .false.

    write_shopping_list = -1

    if ( .not. (shopping_list_open) ) then
      !open shopping list
      open(fh_shopping, file=trim(shopping_list), status = "replace", iostat=readok)
      if (readok /= 0) then
        print*, "[OpenLoops] Error opening shopping list ", trim(shopping_list)
        return
      end if
      ! write header
      if (order_ew /= -1) then
        oqcd = size(ext)-2-order_ew
        write(fh_shopping,'(A)') "SelectCoupling = (Exponent[#, gQCD] == " // trim(itoc(oqcd)) // " + 2 * #2 &);"
        write(fh_shopping,'(A)') "SelectInterference = {eQED -> " // trim(itoc(order_ew*2)) // "};"
        write(fh_shopping,'(A)') "UnitaryGauge = True;"
        write(fh_shopping,'(A)') ""
      else if (order_qcd /= -1) then
        oew = size(ext)-2-order_qcd
        write(fh_shopping,'(A)') "SelectCoupling = (Exponent[#, eQED] == " // trim(itoc(oew)) // " + 2 * #2 " &
          & // " ||  Exponent[#1, eQED] == " // trim(itoc(oew+2))  // " - 2 * #2 &);"
        write(fh_shopping,'(A)') "SelectInterference = {gQCD -> " // trim(itoc(order_qcd*2)) // "};"
        write(fh_shopping,'(A)') "UnitaryGauge = False;"
        write(fh_shopping,'(A)') ""
      end if
      write(fh_shopping,'(A)') "InsertFieldsOptions = {Restrictions -> {ExcludeParticles -> {}, NoQuarkMixing}};"
      write(fh_shopping,'(A)') ""
      shopping_list_open = .true.
    end if

    if (allocated(shopped_processes)) then
      do i = 1, size(shopped_processes)
        if ( trim(proc) == trim(shopped_processes(i)) ) then
          if (verbose > 1) then
            print*, "[OpenLoops] Not written to shopping list. Already shopped as process " // trim(itoc(i)) // ": " //trim(proc)
          end if
          write_shopping_list = i
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
    output = "(* " //trim(proc)// " *) SubProcess[" // trim(itoc(id)) // "] = {FeynArtsProcess -> "
    ! inital state
    output = trim(output) // " {" // trim(PDGtoFA(ext(1))) // "," // trim(PDGtoFA(ext(2))) //  "} -> {"
    ! final state
    do i=3,size(ext)
      output = trim(output) // trim(PDGtoFA(ext(i)))
      if (i /= size(ext)) output = trim(output) // ","
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

    output = trim(output) // "};"

    if (verbose > 0) then
      print*, "[OpenLoops] Write to shopping list "// trim(shopping_list)  //": " // trim(output)
    end if

    ! write process to shopping list
    write(fh_shopping,'(A)') trim(output)

    write_shopping_list = id
    id = id+1
  end function write_shopping_list


  function itoc(i)
  ! integer -> char
    implicit none
    integer, intent(in) :: i
    character(len=30) :: itoc
    write(itoc,'(I30)') i
    itoc = adjustl(itoc)
  end function itoc


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


  pure function amplitudetype(id)
    ! [in] id: a process id
    ! return amptype of type integer
    implicit none
    integer, intent(in) :: id
    integer amplitudetype
    amplitudetype =  process_handles(id)%amplitude_type
  end function amplitudetype


  pure function amplitudetype_c(id) bind(c,name="ol_amplitudetype")
    ! [in] id: a process id
    ! return amptype of type integer
    implicit none
    integer(c_int), intent(in) :: id
    integer(c_int) amplitudetype_c
    amplitudetype_c =  process_handles(id)%amplitude_type
  end function amplitudetype_c


  pure function n_external(id)
    implicit none
    integer, intent(in) :: id
    integer :: n_external
    n_external = process_handles(id)%n_particles
  end function n_external


  function n_external_c(id) bind(c,name="ol_n_external")
    implicit none
    integer(c_int), value :: id
    integer(c_int) :: n_external_c
    n_external_c = process_handles(id)%n_particles
  end function n_external_c


  subroutine phase_space_point(id, sqrt_s, psp)
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: sqrt_s
    real(DREALKIND), intent(out) :: psp(:,:)
    type(process_handle) :: subprocess
    subprocess = process_handles(id)
    call subprocess%set_permutation(subprocess%permutation)
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
    f_id = id
    f_sqrt_s = sqrt_s
    call phase_space_point(f_id, f_sqrt_s, f_psp)
    do f_id = 1, n_external(id)
      pp(5*(f_id-1)+1:5*(f_id-1)+4) = f_psp(0:3,f_id)
      pp(5*f_id) = -1
    end do
  end subroutine phase_space_point_c


  subroutine start() bind(c,name="ol_start")
    implicit none
    call parameters_flush()
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
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: res
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1)
    real(DREALKIND) :: resmunu
    type(process_handle) :: subprocess
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: tree routine not available for process', id
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    call parameters_flush()
    call subprocess%tree(psp, m2cc, 0, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      & 1, [0], resmunu)
    res = m2cc(0)
  end subroutine evaluate_tree


  subroutine evaluate_tree_c(id, pp, res) bind(c,name="ol_evaluate_tree")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: res
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_res
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_tree(f_id, f_pp(0:3,:), f_res)
    res = f_res
  end subroutine evaluate_tree_c


  subroutine evaluate_cc(id, psp, res)
   ! Independent color correlated tree matrix elements.
   ! [in] id: process id as set by register_process
   ! [in] psp: phase space point
   ! [out] res(n_external*(n_external-1)/2): array with the indepenent color correlated
   !       tree amplitudes C_ij = <M|T_iT_j|M>
   !       res(i+j(j-1)/2+1) = C_ij with 0 <= i < j <= n_external-1
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: res(:)
    type(process_handle) :: subprocess
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1)
    real(DREALKIND) :: resmunu
    integer  :: n_cc, i, j
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: cc routine not available for process', id
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_cc = subprocess%n_particles*(subprocess%n_particles+1)/2
    call parameters_flush()
    call subprocess%tree(psp, m2cc, 0, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      & n_cc, [(i, i = 0, n_cc)], resmunu)
    do j = 1, subprocess%n_particles - 1
      do i = 0, j - 1
        res(i+j*(j-1)/2+1) = m2cc((j+1)*j/2+i+1)
      end do
    end do
  end subroutine evaluate_cc


  subroutine evaluate_cc_c(id, pp, res) bind(c,name="ol_evaluate_cc")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: res(rval_size(process_handles(id)%n_particles,2))
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_res(rval_size(process_handles(id)%n_particles,2))
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_cc(f_id, f_pp(0:3,:), f_res)
    res = f_res
  end subroutine evaluate_cc_c


  subroutine evaluate_ccmatrix(id, psp, res, resij)
   ! Color correlated tree matrix elements.
   ! [in] id: process id as set by register_process
   ! [in] psp: phase space point
   ! [out] res: squared born matrix element
   ! [out] res(n_external:n_external): array with the color correlated
   !       tree amplitudes C_ij = <M|T_iT_j|M>
   !       res(i,j) = C_ij with i,j = 1 <= n_external
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: res, resij(:,:)
    type(process_handle) :: subprocess
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1)
    integer  :: n_cc, i, j
    real(DREALKIND) :: resmunu
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: cc routine not available for process', id
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    n_cc = subprocess%n_particles*(subprocess%n_particles+1)/2+1
    call parameters_flush()
    call subprocess%tree(psp, m2cc, 0, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      & n_cc, [(i, i = 0, n_cc)], resmunu)
    do i = 1, subprocess%n_particles
      do j = 1, i
        resij(i,j) = m2cc(i*(i-1)/2+j)
        if (i /= j) resij(j,i) = resij(i,j)
      end do
    end do
    res = m2cc(0)
  end subroutine evaluate_ccmatrix


  subroutine evaluate_ccmatrix_c(id, pp, res, resij) bind(c,name="ol_evaluate_ccmatrix")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: res
    real(c_double), intent(out) :: resij(0:process_handles(id)%n_particles-1,0:process_handles(id)%n_particles-1)
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_res
    real(DREALKIND) :: f_resij(process_handles(id)%n_particles-1,0:process_handles(id)%n_particles-1)
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_ccmatrix(f_id, f_pp(0:3,:), f_res, f_resij)
    res = f_res
    resij = f_resij
  end subroutine evaluate_ccmatrix_c


  subroutine evaluate_sc(id, psp, emitter, polvect, res)
    ! Spin correlated matrix elements.
    ! [in] id: process id as set by register_process
    ! [in] psp: phase space point
    ! [in] int emitter: emitter
    ! [in] polvect: polarisation vector
    ! [out] res(n_external): array with results for each spectator j,
    !       res(j) = 1/mom^2 * <emitter,mu|mom^mu cc_ij mom^nu|j,nu>
    implicit none
    integer, intent(in) :: id, emitter
    real(DREALKIND), intent(in) :: psp(:,:), polvect(4)
    real(DREALKIND), intent(out) :: res(:)
    type(process_handle) :: subprocess
    integer :: j, extcombs(n_external(id))
    real(DREALKIND) :: m2sc(0:n_external(id)*(n_external(id)+1)/2+1)
    real(DREALKIND) :: resmunu
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: sc routine not available for process', id
      stop
    end if
    do j = 1, subprocess%n_particles
      if (j <= emitter) then
        extcombs(j) = emitter*(emitter-1)/2 + j
      else
        extcombs(j) = j*(j-1)/2 + emitter
      end if
    end do
    call parameters_flush()
    call subprocess%tree(psp, m2sc, emitter, polvect, subprocess%n_particles, extcombs, resmunu)
    do j = 1, subprocess%n_particles
      res(j) = m2sc(extcombs(j))
    end do
  end subroutine evaluate_sc


  subroutine evaluate_sc_c(id, pp, emitter, polvect, res) bind(c,name="ol_evaluate_sc")
    implicit none
    integer(c_int), value :: id, emitter
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles), polvect(4)
    real(c_double), intent(out) :: res(process_handles(id)%n_particles)
    integer :: f_id, f_emitter
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles), f_polvect(4)
    real(DREALKIND) :: f_res(process_handles(id)%n_particles)
    f_id = id
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
    implicit none
    integer, intent(in) :: id, emitter
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: res, resmunu(4,4)
    type(process_handle) :: subprocess
    real(DREALKIND) :: m2cc(0:n_external(id)*(n_external(id)+1)/2+1)
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 0)) then
      write(*,*) '[OpenLoops] evaluate: scpowheg routine not available for process', id
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    call parameters_flush()
    call subprocess%tree(psp, m2cc, -emitter, &
      & [0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND, 0._/**/DREALKIND], &
      &  1, [0], resmunu)
    res = m2cc(0)
  end subroutine evaluate_scpowheg


  subroutine evaluate_scpowheg_c(id, pp, emitter, res, resmunu) bind(c,name="ol_evaluate_scpowheg")
    implicit none
    integer(c_int), value :: id, emitter
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: res, resmunu(4,4)
    integer :: f_id, f_emitter
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_res, f_resmunu(4,4)
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    f_emitter = emitter
    call evaluate_scpowheg(f_id, f_pp(0:3,:), f_emitter, f_res, f_resmunu)
    res = f_res
  end subroutine evaluate_scpowheg_c


  subroutine evaluate_full(id, psp, m2l0, m2l1, ir1, m2l2, ir2, acc)
    use ol_stability
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in) :: psp(:,:)
    real(DREALKIND), intent(out) :: m2l0, m2l1(0:2), ir1(0:2), m2l2(0:4), ir2(0:4)
    real(DREALKIND), intent(out) :: acc
    type(process_handle)  :: subprocess
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 1)) then
      write(*,*) '[OpenLoops] evaluate: loop routine not available for process', id
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    call parameters_flush()
    call subprocess%loop(psp, m2l0, m2l1, ir1, m2l2, ir2)
    acc = last_relative_deviation
  end subroutine evaluate_full


  subroutine evaluate_full_c(id, pp, m2l0, m2l1, ir1, m2l2, ir2, acc) bind(c,name="ol_evaluate_full")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: m2l0, m2l1(0:2), ir1(0:2), m2l2(0:4), ir2(0:4), acc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_m2l0, f_m2l1(0:2), f_ir1(0:2), f_m2l2(0:4), f_ir2(0:4)
    real(DREALKIND) :: f_acc
    f_id = id
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
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: m2l0, m2l1(0:2), acc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_m2l0, f_m2l1(0:2)
    real(DREALKIND) :: f_acc
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_loop(f_id, f_pp(0:3,:), f_m2l0, f_m2l1, f_acc)
    m2l0 = f_m2l0
    m2l1 = f_m2l1
    acc  = f_acc
  end subroutine evaluate_loop_c


  subroutine evaluate_loop2(id, psp, res, acc)
    implicit none
    integer, intent(in)  :: id
    real(DREALKIND), intent(in)  :: psp(:,:)
    real(DREALKIND), intent(out) :: res
    real(DREALKIND), intent(out) :: acc
    real(DREALKIND) :: m2l0, m2l1(0:2), ir1(0:2), m2l2(0:4), ir2(0:4)
    if (.not. btest(process_handles(id)%content, 2)) then
      write(*,*) '[OpenLoops] evaluate: loop routine not available for process', id
      stop
    end if
    call evaluate_full(id, psp, m2l0, m2l1, ir1, m2l2, ir2, acc)
    res = m2l2(0)
  end subroutine evaluate_loop2


  subroutine evaluate_loop2_c(id, pp, res, acc) bind(c,name="ol_evaluate_loop2")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: res, acc
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_res, f_acc
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_loop2(f_id, f_pp(0:3,:), f_res, f_acc)
    res = f_res
    acc  = f_acc
  end subroutine evaluate_loop2_c


  subroutine evaluate_ct(id, psp, m2l0, m2ct)
    use ol_stability
    implicit none
    integer, intent(in)  :: id
    real(DREALKIND), intent(in)  :: psp(:,:)
    real(DREALKIND), intent(out) :: m2l0, m2ct
    type(process_handle)  :: subprocess
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 1)) then
      write(*,*) '[OpenLoops] evaluate: ct routine not available for process', id
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    call parameters_flush()
    call subprocess%ct(psp, m2l0, m2ct)
  end subroutine evaluate_ct


  subroutine evaluate_ct_c(id, pp, m2l0, m2ct) bind(c,name="ol_evaluate_ct")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: m2l0, m2ct
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_m2l0, f_m2ct
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_ct(f_id, f_pp(0:3,:), f_m2l0, f_m2ct)
    m2l0 = f_m2l0
    m2ct = f_m2ct
  end subroutine evaluate_ct_c


  subroutine evaluate_pt(id, psp, m2l0, m2pt, m2l1)
    use ol_stability
    implicit none
    integer, intent(in) :: id
    real(DREALKIND), intent(in)  :: psp(:,:)
    real(DREALKIND), intent(out) :: m2l0, m2pt, m2l1
    type(process_handle)  :: subprocess
    subprocess = process_handles(id)
    if (id <= 0 .or. id > last_process_id) then
      write(*,*) '[OpenLoops] evaluate: no registered process with id', id
      stop
    else if (.not. btest(subprocess%content, 3)) then
      write(*,*) '[OpenLoops] evaluate: ct routine not available for process', id
      stop
    end if
    call subprocess%set_permutation(subprocess%permutation)
    call parameters_flush()
    call subprocess%pt(psp, m2l0, m2pt, m2l1)
  end subroutine evaluate_pt


  subroutine evaluate_pt_c(id, pp, m2l0, m2pt, m2l1) bind(c,name="ol_evaluate_pt")
    implicit none
    integer(c_int), value :: id
    real(c_double), intent(in) :: pp(5*process_handles(id)%n_particles)
    real(c_double), intent(out) :: m2l0, m2pt, m2l1
    integer :: f_id
    real(DREALKIND) :: f_pp(0:4,process_handles(id)%n_particles)
    real(DREALKIND) :: f_m2l0, f_m2pt, f_m2l1
    f_id = id
    f_pp = reshape(pp, [5,process_handles(id)%n_particles])
    call evaluate_pt(f_id, f_pp(0:3,:), f_m2l0, f_m2pt, f_m2l1)
    m2l0 = f_m2l0
    m2pt = f_m2pt
    m2l1 = f_m2l1
  end subroutine evaluate_pt_c


end module openloops
