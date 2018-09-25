
module bt_interface
  use BuildTensors, only: CalcTensorTNr_dp => CalcTensorTNr, CalcTNrPVco_dp => CalcTNrPVco
  use TensorReduction, only: CalcTensorFr_dp => CalcTensorFr
  implicit none
  interface CalcTensorTNr
    module procedure CalcTensorTNr_dp
#ifdef USE_qp
    module procedure CalcTensorTNr_qp
#endif
  end interface CalcTensorTNr
  interface CalcTensorFr
    module procedure CalcTensorFr_dp
#ifdef USE_qp
    module procedure CalcTensorFr_qp
#endif
  end interface CalcTensorFr
  interface CalcTNrPVco
    module procedure CalcTNrPVco_dp
#ifdef USE_qp
    module procedure CalcTNrPVco_qp
#endif
  end interface CalcTNrPVco
#ifdef USE_qp
  contains
  function CalcTensorTNr_qp(n, r, moms, m2, invs)
    use KIND_TYPES, only: QREALKIND
    implicit none
    complex(QREALKIND), allocatable :: CalcTensorTNr_qp(:,:)
    integer, intent(in) :: n, r
    complex(QREALKIND), intent(in) :: moms(:,:), m2(:), invs(:)
    allocate(CalcTensorTNr_qp(0,0))
    print *, "tensor integral reduction is only available in double precision"
    stop
  end function CalcTensorTNr_qp
  function CalcTensorFr_qp(r, moms, m2, invs)
    use KIND_TYPES, only: QREALKIND
    implicit none
    complex(QREALKIND), allocatable :: CalcTensorFr_qp(:,:)
    integer, intent(in) :: r
    complex(QREALKIND), intent(in) :: moms(:,:), m2(:), invs(:)
    allocate(CalcTensorFr_qp(0,0))
    print *, "tensor integral reduction is only available in double precision"
    stop
  end function CalcTensorFr_qp
  function CalcTNrPVco_qp(n, r, m2, invs)
    use KIND_TYPES, only: QREALKIND
    use ColiCombinatorics, only: BinomTable
    implicit none
    complex(QREALKIND), allocatable :: CalcTNrPVco_qp(:,:,:)
    integer, intent(in) :: n, r
    complex(QREALKIND), intent(in) :: m2(:), invs(:)
    allocate(CalcTNrPVco_qp(0,0,0))
    print *, "tensor integral reduction is only available in double precision"
    stop
  end function CalcTNrPVco_qp
#endif
end module bt_interface

module bt_TI_lib_switch
  use TI_lib_interface, only: TI_library
end module bt_TI_lib_switch

module bt_ColiCombinatorics
  use ColiCombinatorics, only: BinomTable
end module bt_ColiCombinatorics

module bt_TensorManipulations_dp
  use TensorManipulations, only: RankToSize
end module bt_TensorManipulations_dp

module bt_BuildTensors_dp
  use BuildTensors, only: init_tables
end module bt_BuildTensors_dp

module dd_init_dp
  interface dd_setmode
    subroutine DDsetmode(xcacc, xdacc, xmode34, xmode5, xmode6, xoutlevel)
      use KIND_TYPES, only: DREALKIND
      implicit none
      real(DREALKIND) :: xcacc, xdacc
      integer :: xmode34, xmode5, xmode6, xoutlevel
    end subroutine DDsetmode
  end interface dd_setmode
  interface dd_setparam
    subroutine DDsetparam(xdeltauv, xmuv2, xdelta2ir, xdelta1ir, xmir2, xmx2)
      use KIND_TYPES, only: DREALKIND
      implicit none
      real(DREALKIND) :: xdeltauv, xmuv2, xdelta2ir, xdelta1ir, xmir2, xmx2(10)
    end subroutine DDsetparam
  end interface dd_setparam
end module dd_init_dp

module dd_interface
  contains
  function dd_get_error_code()
    implicit none
    integer :: dd_get_error_code
    dd_get_error_code = 0
  end function dd_get_error_code
end module dd_interface

#ifdef USE_qp
module bt_TensorManipulations_qp
  use TensorManipulations, only: RankToSize
end module bt_TensorManipulations_qp

module bt_BuildTensors_qp
  use BuildTensors, only: init_tables
end module bt_BuildTensors_qp

module dd_init_qp
  implicit none
  contains
  subroutine dd_setmode(xcacc, xdacc, xmode34, xmode5, xmode6, xoutlevel)
    use KIND_TYPES, only: QREALKIND
    implicit none
    real(QREALKIND) :: xcacc, xdacc
    integer :: xmode34, xmode5, xmode6, xoutlevel
    print *, "DD qp init: ignored"
  end subroutine dd_setmode
  subroutine dd_setparam(xdeltauv, xmuv2, xdelta2ir, xdelta1ir, xmir2, xmx2)
    use KIND_TYPES, only: QREALKIND
    implicit none
    real(QREALKIND) :: xdeltauv, xmuv2, xdelta2ir, xdelta1ir, xmir2, xmx2(10)
    print *, "DD qp init: ignored"
  end subroutine dd_setparam
end module dd_init_qp
#endif
