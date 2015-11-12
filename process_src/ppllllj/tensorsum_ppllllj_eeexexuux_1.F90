
! **********************************************************************
module ol_tensor_sum_storage_ppllllj_eeexexuux_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1[[2]](4,1)
  complex(REALKIND) :: momenta_OpenDiagramHashes[[2]](4,1)

  complex(REALKIND), save :: masses2_1[[2]](2)
  complex(REALKIND), save :: masses2_OpenDiagramHashes[[2]](2)

  complex(REALKIND), save :: TOpenDiagramHashessum((1 + OpenDiagramHashes) (2 + OpenDiagramHashes) (3 + OpenDiagramHashes) (4 + OpenDiagramHashes)
-----------------------------------------------------------------------------------------------
                                              24,2) = 0
  complex(REALKIND), save :: T(#1[[2,3]] & )[[1,2,3]]sum((1 + (#1[[2,3]] & )[[1,2,3]]) (2 + (#1[[2,3]] & )[[1,2,3]]) (3 + (#1[[2,3]] & )[[1,2,3]]) (4 + (#1[[2,3]] & )[[1,2,3]])
-----------------------------------------------------------------------------------------------------------------------
                                                          24,1) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_ppllllj_eeexexuux_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 2 -> 2
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_ppllllj_eeexexuux_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = -Infinity
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none
  TOpenDiagramHashessum = 0
  T(#1[[2,3]] & )[[1,2,3]]sum = 0

end subroutine reset_tensor_sum


subroutine scale_one_tsum(tsum, spow)
  use ol_parameters_decl_/**/REALKIND, only: scalefactor
  implicit none
  complex(REALKIND), intent(inout) :: tsum(:)
  integer, intent(in) :: spow ! rank 0 scale power
  real(REALKIND) :: sfinv, sfac
  integer :: sz
  sfinv = 1/scalefactor
  sfac = scalefactor**spow
  sz = size(tsum)
  tsum(1) = sfac*tsum(1)
  if (sz > 1) then ! rank 1
    sfac = sfac*sfinv
    tsum(2:5) = sfac*tsum(2:5)
  end if
  if (sz > 5) then ! rank 2
    sfac = sfac*sfinv
    tsum(6:15) = sfac*tsum(6:15)
  end if
  if (sz > 15) then ! rank 3
    sfac = sfac*sfinv
    tsum(16:35) = sfac*tsum(16:35)
  end if
  if (sz > 35) then ! rank 4
    sfac = sfac*sfinv
    tsum(36:70) = sfac*tsum(36:70)
  end if
  if (sz > 70) then ! rank 5
    sfac = sfac*sfinv
    tsum(71:126) = sfac*tsum(71:126)
  end if
  if (sz > 126) then ! rank 6
    sfac = sfac*sfinv
    tsum(127:210) = sfac*tsum(127:210)
  end if
  if (sz > 210) then ! rank 7
    sfac = sfac*sfinv
    tsum(211:330) = sfac*tsum(211:330)
  end if
end subroutine scale_one_tsum


subroutine scale_tensor_sum()
  implicit none
  call scale_one_tsum(TOpenDiagramHashessum(:,1), -8)
  call scale_one_tsum(T(((#1[[1]] & )[Drop[#1[[2]], -1]] & ) -> -1)[[2,3]]sum(:,2), -2)
  call scale_one_tsum(T#1[[2,3]][[2,3]]sum(:,1), -2)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_ppllllj_eeexexuux_1, only: channel_number_ppllllj_eeexexuux_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_ppllllj_eeexexuux_1)
  M2 = 0

  momenta_1[[2]](:,1) = Q(1:4,Total[1])
  momenta_OpenDiagramHashes[[2]](:,1) = Q(1:4,Total[OpenDiagramHashes])

  masses2_1[[2]] = (1<>2)[[1<>2]]
  masses2_OpenDiagramHashes[[2]] = (1<>2)[[OpenDiagramHashes<>2]]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call TI_call(OpenDiagramHashes, momenta_OpenDiagramHashes, masses2_OpenDiagramHashes, TOpenDiagramHashessum(:,1), M2)
  call TI_call((((#1[[1]] & )[Drop[#1[[2]], -1]] & ) -> -1)[[2,3]], momenta_(((#OpenDiagramHashes[[OpenDiagramHashes]] & )[Drop[#OpenDiagramHashes[[2]], -1]] & ) -> -1)[[2,OpenDiagramHashes]], masses2_(((#OpenDiagramHashes[[OpenDiagramHashes]] & )[Drop[#OpenDiagramHashes[[2]], -1]] & ) -> -1)[[2,2]], T(((#1[[1]] & )[Drop[#1[[2]], -1]] & ) -> -1)[[2,3]]sum(:,2), M2)

  call TI_call(#1[[2,3]][[2,3]], momenta_#OpenDiagramHashes[[2,3]][[2,OpenDiagramHashes]], masses2_#OpenDiagramHashes[[2,3]][[2,2]], T#1[[2,3]][[2,3]]sum(:,1), M2)

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_ppllllj_eeexexuux_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_ppllllj_eeexexuux_1_/**/REALKIND
