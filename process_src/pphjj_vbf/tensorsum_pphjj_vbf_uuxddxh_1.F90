
! **********************************************************************
module ol_tensor_sum_storage_pphjj_vbf_uuxddxh_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1(4,2)
  complex(REALKIND) :: momenta_2(4,2)
  complex(REALKIND) :: momenta_3(4,2)
  complex(REALKIND) :: momenta_4(4,2)
  complex(REALKIND) :: momenta_5(4,2)
  complex(REALKIND) :: momenta_6(4,2)
  complex(REALKIND) :: momenta_7(4,2)
  complex(REALKIND) :: momenta_8(4,2)
  complex(REALKIND) :: momenta_9(4,3)
  complex(REALKIND) :: momenta_10(4,3)
  complex(REALKIND) :: momenta_11(4,4)
  complex(REALKIND) :: momenta_12(4,4)
  complex(REALKIND) :: momenta_13(4,4)
  complex(REALKIND) :: momenta_14(4,4)
  complex(REALKIND) :: momenta_15(4,4)
  complex(REALKIND) :: momenta_16(4,4)
  complex(REALKIND) :: momenta_17(4,4)
  complex(REALKIND) :: momenta_18(4,4)

  complex(REALKIND), save :: masses2_1(3)
  complex(REALKIND), save :: masses2_2(3)
  complex(REALKIND), save :: masses2_3(3)
  complex(REALKIND), save :: masses2_4(4)
  complex(REALKIND), save :: masses2_5(4)
  complex(REALKIND), save :: masses2_6(4)
  complex(REALKIND), save :: masses2_7(4)
  complex(REALKIND), save :: masses2_8(5)
  complex(REALKIND), save :: masses2_9(5)

  complex(REALKIND), save :: T1sum(5,8) = 0
  complex(REALKIND), save :: T2sum(15,16) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_pphjj_vbf_uuxddxh_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 5
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_pphjj_vbf_uuxddxh_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 2
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none
  T1sum = 0
  T2sum = 0

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
  call scale_one_tsum(T1sum(:,1), 0)
  call scale_one_tsum(T1sum(:,2), 0)
  call scale_one_tsum(T1sum(:,3), 0)
  call scale_one_tsum(T1sum(:,4), 0)
  call scale_one_tsum(T1sum(:,5), 0)
  call scale_one_tsum(T1sum(:,6), 0)
  call scale_one_tsum(T1sum(:,7), 0)
  call scale_one_tsum(T1sum(:,8), 0)
  call scale_one_tsum(T2sum(:,1), 0)
  call scale_one_tsum(T2sum(:,2), 2)
  call scale_one_tsum(T2sum(:,3), 2)
  call scale_one_tsum(T2sum(:,4), 0)
  call scale_one_tsum(T2sum(:,5), 0)
  call scale_one_tsum(T2sum(:,6), 0)
  call scale_one_tsum(T2sum(:,7), 2)
  call scale_one_tsum(T2sum(:,8), 2)
  call scale_one_tsum(T2sum(:,9), 4)
  call scale_one_tsum(T2sum(:,10), 4)
  call scale_one_tsum(T2sum(:,11), 4)
  call scale_one_tsum(T2sum(:,12), 4)
  call scale_one_tsum(T2sum(:,13), 4)
  call scale_one_tsum(T2sum(:,14), 4)
  call scale_one_tsum(T2sum(:,15), 4)
  call scale_one_tsum(T2sum(:,16), 4)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_pphjj_vbf_uuxddxh_1, only: channel_number_pphjj_vbf_uuxddxh_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_pphjj_vbf_uuxddxh_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,2)
  momenta_1(:,2) = Q(1:4,30)
  momenta_2(:,1) = Q(1:4,4)
  momenta_2(:,2) = Q(1:4,29)
  momenta_3(:,1) = Q(1:4,8)
  momenta_3(:,2) = Q(1:4,24)
  momenta_4(:,1) = Q(1:4,8)
  momenta_4(:,2) = Q(1:4,27)
  momenta_5(:,1) = Q(1:4,8)
  momenta_5(:,2) = Q(1:4,30)
  momenta_6(:,1) = Q(1:4,11)
  momenta_6(:,2) = Q(1:4,27)
  momenta_7(:,1) = Q(1:4,13)
  momenta_7(:,2) = Q(1:4,29)
  momenta_8(:,1) = Q(1:4,14)
  momenta_8(:,2) = Q(1:4,30)
  momenta_9(:,1) = Q(1:4,4)
  momenta_9(:,2) = Q(1:4,20)
  momenta_9(:,3) = Q(1:4,28)
  momenta_10(:,1) = Q(1:4,12)
  momenta_10(:,2) = Q(1:4,14)
  momenta_10(:,3) = Q(1:4,30)
  momenta_11(:,1) = Q(1:4,2)
  momenta_11(:,2) = Q(1:4,6)
  momenta_11(:,3) = Q(1:4,22)
  momenta_11(:,4) = Q(1:4,30)
  momenta_12(:,1) = Q(1:4,4)
  momenta_12(:,2) = Q(1:4,6)
  momenta_12(:,3) = Q(1:4,22)
  momenta_12(:,4) = Q(1:4,30)
  momenta_13(:,1) = Q(1:4,4)
  momenta_13(:,2) = Q(1:4,12)
  momenta_13(:,3) = Q(1:4,28)
  momenta_13(:,4) = Q(1:4,29)
  momenta_14(:,1) = Q(1:4,4)
  momenta_14(:,2) = Q(1:4,12)
  momenta_14(:,3) = Q(1:4,28)
  momenta_14(:,4) = Q(1:4,30)
  momenta_15(:,1) = Q(1:4,8)
  momenta_15(:,2) = Q(1:4,9)
  momenta_15(:,3) = Q(1:4,25)
  momenta_15(:,4) = Q(1:4,27)
  momenta_16(:,1) = Q(1:4,8)
  momenta_16(:,2) = Q(1:4,9)
  momenta_16(:,3) = Q(1:4,25)
  momenta_16(:,4) = Q(1:4,29)
  momenta_17(:,1) = Q(1:4,8)
  momenta_17(:,2) = Q(1:4,12)
  momenta_17(:,3) = Q(1:4,28)
  momenta_17(:,4) = Q(1:4,29)
  momenta_18(:,1) = Q(1:4,8)
  momenta_18(:,2) = Q(1:4,12)
  momenta_18(:,3) = Q(1:4,28)
  momenta_18(:,4) = Q(1:4,30)

  masses2_1 = [ ZERO2, MW2, MW2 ]
  masses2_2 = [ ZERO2, MZ2, MZ2 ]
  masses2_3 = [ ZERO2, ZERO2, ZERO2 ]
  masses2_4 = [ ZERO2, MW2, MW2, ZERO2 ]
  masses2_5 = [ ZERO2, MZ2, MZ2, ZERO2 ]
  masses2_6 = [ ZERO2, ZERO2, MW2, MW2 ]
  masses2_7 = [ ZERO2, ZERO2, MZ2, MZ2 ]
  masses2_8 = [ ZERO2, ZERO2, MW2, MW2, ZERO2 ]
  masses2_9 = [ ZERO2, ZERO2, MZ2, MZ2, ZERO2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call TI_call(1, momenta_3, masses2_2, T1sum(:,1), M2)
  call TI_call(1, momenta_3, masses2_1, T1sum(:,2), M2)
  call TI_call(1, momenta_6, masses2_2, T1sum(:,3), M2)
  call TI_call(1, momenta_6, masses2_1, T1sum(:,4), M2)
  call TI_call(1, momenta_7, masses2_2, T1sum(:,5), M2)
  call TI_call(1, momenta_7, masses2_1, T1sum(:,6), M2)
  call TI_call(1, momenta_8, masses2_2, T1sum(:,7), M2)
  call TI_call(1, momenta_8, masses2_1, T1sum(:,8), M2)

  call TI_call(2, momenta_4, masses2_3, T2sum(:,1), M2)
  call TI_call(2, momenta_9, masses2_5, T2sum(:,2), M2)
  call TI_call(2, momenta_9, masses2_4, T2sum(:,3), M2)
  call TI_call(2, momenta_5, masses2_3, T2sum(:,4), M2)
  call TI_call(2, momenta_2, masses2_3, T2sum(:,5), M2)
  call TI_call(2, momenta_1, masses2_3, T2sum(:,6), M2)
  call TI_call(2, momenta_10, masses2_7, T2sum(:,7), M2)
  call TI_call(2, momenta_10, masses2_6, T2sum(:,8), M2)
  call TI_call(2, momenta_14, masses2_9, T2sum(:,9), M2)
  call TI_call(2, momenta_11, masses2_8, T2sum(:,10), M2)
  call TI_call(2, momenta_18, masses2_9, T2sum(:,11), M2)
  call TI_call(2, momenta_13, masses2_9, T2sum(:,12), M2)
  call TI_call(2, momenta_17, masses2_9, T2sum(:,13), M2)
  call TI_call(2, momenta_12, masses2_8, T2sum(:,14), M2)
  call TI_call(2, momenta_16, masses2_8, T2sum(:,15), M2)
  call TI_call(2, momenta_15, masses2_8, T2sum(:,16), M2)

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_pphjj_vbf_uuxddxh_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_pphjj_vbf_uuxddxh_1_/**/REALKIND
