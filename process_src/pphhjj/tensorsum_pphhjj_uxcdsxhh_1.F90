
! **********************************************************************
module ol_tensor_sum_storage_pphhjj_uxcdsxhh_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1(4,2)
  complex(REALKIND) :: momenta_2(4,2)
  complex(REALKIND) :: momenta_3(4,4)
  complex(REALKIND) :: momenta_4(4,4)
  complex(REALKIND) :: momenta_5(4,4)
  complex(REALKIND) :: momenta_6(4,4)
  complex(REALKIND) :: momenta_7(4,5)
  complex(REALKIND) :: momenta_8(4,5)
  complex(REALKIND) :: momenta_9(4,5)
  complex(REALKIND) :: momenta_10(4,5)
  complex(REALKIND) :: momenta_11(4,5)
  complex(REALKIND) :: momenta_12(4,5)
  complex(REALKIND) :: momenta_13(4,5)
  complex(REALKIND) :: momenta_14(4,5)

  complex(REALKIND), save :: masses2_1(3)
  complex(REALKIND), save :: masses2_2(5)
  complex(REALKIND), save :: masses2_3(6)

  complex(REALKIND), save :: T2sum(15,6) = 0
  complex(REALKIND), save :: T4sum(70,8) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_pphhjj_uxcdsxhh_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 6
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_pphhjj_uxcdsxhh_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 4
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none
  T2sum = 0
  T4sum = 0

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
  call scale_one_tsum(T2sum(:,1), -2)
  call scale_one_tsum(T2sum(:,2), -2)
  call scale_one_tsum(T2sum(:,3), 2)
  call scale_one_tsum(T2sum(:,4), 2)
  call scale_one_tsum(T2sum(:,5), 2)
  call scale_one_tsum(T2sum(:,6), 2)
  call scale_one_tsum(T4sum(:,1), 4)
  call scale_one_tsum(T4sum(:,2), 4)
  call scale_one_tsum(T4sum(:,3), 4)
  call scale_one_tsum(T4sum(:,4), 4)
  call scale_one_tsum(T4sum(:,5), 4)
  call scale_one_tsum(T4sum(:,6), 4)
  call scale_one_tsum(T4sum(:,7), 4)
  call scale_one_tsum(T4sum(:,8), 4)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_pphhjj_uxcdsxhh_1, only: channel_number_pphhjj_uxcdsxhh_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_pphhjj_uxcdsxhh_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,4)
  momenta_1(:,2) = Q(1:4,62)
  momenta_2(:,1) = Q(1:4,8)
  momenta_2(:,2) = Q(1:4,61)
  momenta_3(:,1) = Q(1:4,2)
  momenta_3(:,2) = Q(1:4,10)
  momenta_3(:,3) = Q(1:4,58)
  momenta_3(:,4) = Q(1:4,62)
  momenta_4(:,1) = Q(1:4,4)
  momenta_4(:,2) = Q(1:4,5)
  momenta_4(:,3) = Q(1:4,53)
  momenta_4(:,4) = Q(1:4,61)
  momenta_5(:,1) = Q(1:4,8)
  momenta_5(:,2) = Q(1:4,10)
  momenta_5(:,3) = Q(1:4,58)
  momenta_5(:,4) = Q(1:4,59)
  momenta_6(:,1) = Q(1:4,8)
  momenta_6(:,2) = Q(1:4,10)
  momenta_6(:,3) = Q(1:4,58)
  momenta_6(:,4) = Q(1:4,62)
  momenta_7(:,1) = Q(1:4,2)
  momenta_7(:,2) = Q(1:4,10)
  momenta_7(:,3) = Q(1:4,26)
  momenta_7(:,4) = Q(1:4,58)
  momenta_7(:,5) = Q(1:4,62)
  momenta_8(:,1) = Q(1:4,2)
  momenta_8(:,2) = Q(1:4,10)
  momenta_8(:,3) = Q(1:4,42)
  momenta_8(:,4) = Q(1:4,58)
  momenta_8(:,5) = Q(1:4,62)
  momenta_9(:,1) = Q(1:4,4)
  momenta_9(:,2) = Q(1:4,5)
  momenta_9(:,3) = Q(1:4,21)
  momenta_9(:,4) = Q(1:4,53)
  momenta_9(:,5) = Q(1:4,61)
  momenta_10(:,1) = Q(1:4,4)
  momenta_10(:,2) = Q(1:4,5)
  momenta_10(:,3) = Q(1:4,37)
  momenta_10(:,4) = Q(1:4,53)
  momenta_10(:,5) = Q(1:4,61)
  momenta_11(:,1) = Q(1:4,8)
  momenta_11(:,2) = Q(1:4,10)
  momenta_11(:,3) = Q(1:4,26)
  momenta_11(:,4) = Q(1:4,58)
  momenta_11(:,5) = Q(1:4,59)
  momenta_12(:,1) = Q(1:4,8)
  momenta_12(:,2) = Q(1:4,10)
  momenta_12(:,3) = Q(1:4,26)
  momenta_12(:,4) = Q(1:4,58)
  momenta_12(:,5) = Q(1:4,62)
  momenta_13(:,1) = Q(1:4,8)
  momenta_13(:,2) = Q(1:4,10)
  momenta_13(:,3) = Q(1:4,42)
  momenta_13(:,4) = Q(1:4,58)
  momenta_13(:,5) = Q(1:4,59)
  momenta_14(:,1) = Q(1:4,8)
  momenta_14(:,2) = Q(1:4,10)
  momenta_14(:,3) = Q(1:4,42)
  momenta_14(:,4) = Q(1:4,58)
  momenta_14(:,5) = Q(1:4,62)

  masses2_1 = [ ZERO2, ZERO2, ZERO2 ]
  masses2_2 = [ ZERO2, ZERO2, MW2, MW2, ZERO2 ]
  masses2_3 = [ ZERO2, ZERO2, MW2, MW2, MW2, ZERO2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call TI_call(2, momenta_2, masses2_1, T2sum(:,1), M2)
  call TI_call(2, momenta_1, masses2_1, T2sum(:,2), M2)
  call TI_call(2, momenta_3, masses2_2, T2sum(:,3), M2)
  call TI_call(2, momenta_6, masses2_2, T2sum(:,4), M2)
  call TI_call(2, momenta_4, masses2_2, T2sum(:,5), M2)
  call TI_call(2, momenta_5, masses2_2, T2sum(:,6), M2)

  call TI_call(4, momenta_8, masses2_3, T4sum(:,1), M2)
  call TI_call(4, momenta_7, masses2_3, T4sum(:,2), M2)
  call TI_call(4, momenta_12, masses2_3, T4sum(:,3), M2)
  call TI_call(4, momenta_14, masses2_3, T4sum(:,4), M2)
  call TI_call(4, momenta_9, masses2_3, T4sum(:,5), M2)
  call TI_call(4, momenta_13, masses2_3, T4sum(:,6), M2)
  call TI_call(4, momenta_10, masses2_3, T4sum(:,7), M2)
  call TI_call(4, momenta_11, masses2_3, T4sum(:,8), M2)

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_pphhjj_uxcdsxhh_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_pphhjj_uxcdsxhh_1_/**/REALKIND
