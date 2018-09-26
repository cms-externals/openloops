
! **********************************************************************
module ol_tensor_sum_storage_ppwjj_ckm_uxsssxwx_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1(4,1)
  complex(REALKIND) :: momenta_2(4,1)
  complex(REALKIND) :: momenta_3(4,1)
  complex(REALKIND) :: momenta_4(4,1)
  complex(REALKIND) :: momenta_5(4,1)
  complex(REALKIND) :: momenta_6(4,2)
  complex(REALKIND) :: momenta_7(4,2)
  complex(REALKIND) :: momenta_8(4,2)
  complex(REALKIND) :: momenta_9(4,2)
  complex(REALKIND) :: momenta_10(4,2)
  complex(REALKIND) :: momenta_11(4,2)
  complex(REALKIND) :: momenta_12(4,2)
  complex(REALKIND) :: momenta_13(4,2)
  complex(REALKIND) :: momenta_14(4,2)
  complex(REALKIND) :: momenta_15(4,2)
  complex(REALKIND) :: momenta_16(4,2)
  complex(REALKIND) :: momenta_17(4,2)
  complex(REALKIND) :: momenta_18(4,2)
  complex(REALKIND) :: momenta_19(4,2)
  complex(REALKIND) :: momenta_20(4,2)
  complex(REALKIND) :: momenta_21(4,3)
  complex(REALKIND) :: momenta_22(4,3)
  complex(REALKIND) :: momenta_23(4,3)
  complex(REALKIND) :: momenta_24(4,3)
  complex(REALKIND) :: momenta_25(4,3)
  complex(REALKIND) :: momenta_26(4,3)
  complex(REALKIND) :: momenta_27(4,3)
  complex(REALKIND) :: momenta_28(4,3)
  complex(REALKIND) :: momenta_29(4,3)
  complex(REALKIND) :: momenta_30(4,3)
  complex(REALKIND) :: momenta_31(4,3)
  complex(REALKIND) :: momenta_32(4,3)
  complex(REALKIND) :: momenta_33(4,3)
  complex(REALKIND) :: momenta_34(4,3)
  complex(REALKIND) :: momenta_35(4,4)
  complex(REALKIND) :: momenta_36(4,4)
  complex(REALKIND) :: momenta_37(4,4)
  complex(REALKIND) :: momenta_38(4,4)

  complex(REALKIND), save :: masses2_1(2)
  complex(REALKIND), save :: masses2_2(2)
  complex(REALKIND), save :: masses2_3(2)
  complex(REALKIND), save :: masses2_4(3)
  complex(REALKIND), save :: masses2_5(4)
  complex(REALKIND), save :: masses2_6(5)

  complex(REALKIND), save :: T1sum(5,3) = 0
  complex(REALKIND), save :: T2sum(15,29) = 0
  complex(REALKIND), save :: T3sum(35,10) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_ppwjj_ckm_uxsssxwx_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 5
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_ppwjj_ckm_uxsssxwx_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 3
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none
  T1sum = 0
  T2sum = 0
  T3sum = 0

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
  call scale_one_tsum(T1sum(:,1), -2)
  call scale_one_tsum(T1sum(:,2), -2)
  call scale_one_tsum(T1sum(:,3), -2)
  call scale_one_tsum(T2sum(:,1), 0)
  call scale_one_tsum(T2sum(:,2), 0)
  call scale_one_tsum(T2sum(:,3), 0)
  call scale_one_tsum(T2sum(:,4), 0)
  call scale_one_tsum(T2sum(:,5), 0)
  call scale_one_tsum(T2sum(:,6), 0)
  call scale_one_tsum(T2sum(:,7), 0)
  call scale_one_tsum(T2sum(:,8), 0)
  call scale_one_tsum(T2sum(:,9), 0)
  call scale_one_tsum(T2sum(:,10), 0)
  call scale_one_tsum(T2sum(:,11), 0)
  call scale_one_tsum(T2sum(:,12), 0)
  call scale_one_tsum(T2sum(:,13), 0)
  call scale_one_tsum(T2sum(:,14), 0)
  call scale_one_tsum(T2sum(:,15), 0)
  call scale_one_tsum(T2sum(:,16), 2)
  call scale_one_tsum(T2sum(:,17), 2)
  call scale_one_tsum(T2sum(:,18), 2)
  call scale_one_tsum(T2sum(:,19), 2)
  call scale_one_tsum(T2sum(:,20), 2)
  call scale_one_tsum(T2sum(:,21), 2)
  call scale_one_tsum(T2sum(:,22), 2)
  call scale_one_tsum(T2sum(:,23), 2)
  call scale_one_tsum(T2sum(:,24), -2)
  call scale_one_tsum(T2sum(:,25), -2)
  call scale_one_tsum(T2sum(:,26), -2)
  call scale_one_tsum(T2sum(:,27), -2)
  call scale_one_tsum(T2sum(:,28), -2)
  call scale_one_tsum(T2sum(:,29), -2)
  call scale_one_tsum(T3sum(:,1), 2)
  call scale_one_tsum(T3sum(:,2), 2)
  call scale_one_tsum(T3sum(:,3), 2)
  call scale_one_tsum(T3sum(:,4), 2)
  call scale_one_tsum(T3sum(:,5), 2)
  call scale_one_tsum(T3sum(:,6), 2)
  call scale_one_tsum(T3sum(:,7), 4)
  call scale_one_tsum(T3sum(:,8), 4)
  call scale_one_tsum(T3sum(:,9), 4)
  call scale_one_tsum(T3sum(:,10), 4)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_ppwjj_ckm_uxsssxwx_1, only: channel_number_ppwjj_ckm_uxsssxwx_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_ppwjj_ckm_uxsssxwx_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,17)
  momenta_2(:,1) = Q(1:4,18)
  momenta_3(:,1) = Q(1:4,19)
  momenta_4(:,1) = Q(1:4,20)
  momenta_5(:,1) = Q(1:4,21)
  momenta_6(:,1) = Q(1:4,8)
  momenta_6(:,2) = Q(1:4,27)
  momenta_7(:,1) = Q(1:4,8)
  momenta_7(:,2) = Q(1:4,29)
  momenta_8(:,1) = Q(1:4,10)
  momenta_8(:,2) = Q(1:4,27)
  momenta_9(:,1) = Q(1:4,10)
  momenta_9(:,2) = Q(1:4,30)
  momenta_10(:,1) = Q(1:4,11)
  momenta_10(:,2) = Q(1:4,27)
  momenta_11(:,1) = Q(1:4,12)
  momenta_11(:,2) = Q(1:4,29)
  momenta_12(:,1) = Q(1:4,12)
  momenta_12(:,2) = Q(1:4,30)
  momenta_13(:,1) = Q(1:4,13)
  momenta_13(:,2) = Q(1:4,29)
  momenta_14(:,1) = Q(1:4,14)
  momenta_14(:,2) = Q(1:4,30)
  momenta_15(:,1) = Q(1:4,17)
  momenta_15(:,2) = Q(1:4,27)
  momenta_16(:,1) = Q(1:4,17)
  momenta_16(:,2) = Q(1:4,29)
  momenta_17(:,1) = Q(1:4,18)
  momenta_17(:,2) = Q(1:4,30)
  momenta_18(:,1) = Q(1:4,19)
  momenta_18(:,2) = Q(1:4,27)
  momenta_19(:,1) = Q(1:4,20)
  momenta_19(:,2) = Q(1:4,30)
  momenta_20(:,1) = Q(1:4,21)
  momenta_20(:,2) = Q(1:4,29)
  momenta_21(:,1) = Q(1:4,2)
  momenta_21(:,2) = Q(1:4,10)
  momenta_21(:,3) = Q(1:4,30)
  momenta_22(:,1) = Q(1:4,2)
  momenta_22(:,2) = Q(1:4,14)
  momenta_22(:,3) = Q(1:4,30)
  momenta_23(:,1) = Q(1:4,2)
  momenta_23(:,2) = Q(1:4,18)
  momenta_23(:,3) = Q(1:4,30)
  momenta_24(:,1) = Q(1:4,4)
  momenta_24(:,2) = Q(1:4,12)
  momenta_24(:,3) = Q(1:4,29)
  momenta_25(:,1) = Q(1:4,4)
  momenta_25(:,2) = Q(1:4,12)
  momenta_25(:,3) = Q(1:4,30)
  momenta_26(:,1) = Q(1:4,4)
  momenta_26(:,2) = Q(1:4,14)
  momenta_26(:,3) = Q(1:4,30)
  momenta_27(:,1) = Q(1:4,4)
  momenta_27(:,2) = Q(1:4,20)
  momenta_27(:,3) = Q(1:4,30)
  momenta_28(:,1) = Q(1:4,4)
  momenta_28(:,2) = Q(1:4,21)
  momenta_28(:,3) = Q(1:4,29)
  momenta_29(:,1) = Q(1:4,8)
  momenta_29(:,2) = Q(1:4,10)
  momenta_29(:,3) = Q(1:4,30)
  momenta_30(:,1) = Q(1:4,8)
  momenta_30(:,2) = Q(1:4,12)
  momenta_30(:,3) = Q(1:4,29)
  momenta_31(:,1) = Q(1:4,8)
  momenta_31(:,2) = Q(1:4,12)
  momenta_31(:,3) = Q(1:4,30)
  momenta_32(:,1) = Q(1:4,10)
  momenta_32(:,2) = Q(1:4,14)
  momenta_32(:,3) = Q(1:4,30)
  momenta_33(:,1) = Q(1:4,12)
  momenta_33(:,2) = Q(1:4,14)
  momenta_33(:,3) = Q(1:4,30)
  momenta_34(:,1) = Q(1:4,17)
  momenta_34(:,2) = Q(1:4,21)
  momenta_34(:,3) = Q(1:4,29)
  momenta_35(:,1) = Q(1:4,2)
  momenta_35(:,2) = Q(1:4,10)
  momenta_35(:,3) = Q(1:4,14)
  momenta_35(:,4) = Q(1:4,30)
  momenta_36(:,1) = Q(1:4,4)
  momenta_36(:,2) = Q(1:4,12)
  momenta_36(:,3) = Q(1:4,14)
  momenta_36(:,4) = Q(1:4,30)
  momenta_37(:,1) = Q(1:4,8)
  momenta_37(:,2) = Q(1:4,10)
  momenta_37(:,3) = Q(1:4,14)
  momenta_37(:,4) = Q(1:4,30)
  momenta_38(:,1) = Q(1:4,8)
  momenta_38(:,2) = Q(1:4,12)
  momenta_38(:,3) = Q(1:4,14)
  momenta_38(:,4) = Q(1:4,30)

  masses2_1 = [ MB2, MB2 ]
  masses2_2 = [ MT2, MT2 ]
  masses2_3 = [ ZERO2, ZERO2 ]
  masses2_4 = [ ZERO2, ZERO2, ZERO2 ]
  masses2_5 = [ ZERO2, ZERO2, ZERO2, ZERO2 ]
  masses2_6 = [ ZERO2, ZERO2, ZERO2, ZERO2, ZERO2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call TI_call(1, momenta_1, masses2_3, T1sum(:,1), M2)
  call TI_call(1, momenta_4, masses2_3, T1sum(:,2), M2)
  call TI_call(1, momenta_2, masses2_3, T1sum(:,3), M2)

  call TI_call(2, momenta_15, masses2_4, T2sum(:,1), M2)
  call TI_call(2, momenta_8, masses2_4, T2sum(:,2), M2)
  call TI_call(2, momenta_19, masses2_4, T2sum(:,3), M2)
  call TI_call(2, momenta_9, masses2_4, T2sum(:,4), M2)
  call TI_call(2, momenta_10, masses2_4, T2sum(:,5), M2)
  call TI_call(2, momenta_14, masses2_4, T2sum(:,6), M2)
  call TI_call(2, momenta_16, masses2_4, T2sum(:,7), M2)
  call TI_call(2, momenta_11, masses2_4, T2sum(:,8), M2)
  call TI_call(2, momenta_17, masses2_4, T2sum(:,9), M2)
  call TI_call(2, momenta_12, masses2_4, T2sum(:,10), M2)
  call TI_call(2, momenta_13, masses2_4, T2sum(:,11), M2)
  call TI_call(2, momenta_6, masses2_4, T2sum(:,12), M2)
  call TI_call(2, momenta_18, masses2_4, T2sum(:,13), M2)
  call TI_call(2, momenta_7, masses2_4, T2sum(:,14), M2)
  call TI_call(2, momenta_20, masses2_4, T2sum(:,15), M2)
  call TI_call(2, momenta_28, masses2_5, T2sum(:,16), M2)
  call TI_call(2, momenta_24, masses2_5, T2sum(:,17), M2)
  call TI_call(2, momenta_34, masses2_5, T2sum(:,18), M2)
  call TI_call(2, momenta_30, masses2_5, T2sum(:,19), M2)
  call TI_call(2, momenta_25, masses2_5, T2sum(:,20), M2)
  call TI_call(2, momenta_31, masses2_5, T2sum(:,21), M2)
  call TI_call(2, momenta_21, masses2_5, T2sum(:,22), M2)
  call TI_call(2, momenta_29, masses2_5, T2sum(:,23), M2)
  call TI_call(2, momenta_5, masses2_3, T2sum(:,24), M2)
  call TI_call(2, momenta_5, masses2_2, T2sum(:,25), M2)
  call TI_call(2, momenta_5, masses2_1, T2sum(:,26), M2)
  call TI_call(2, momenta_3, masses2_3, T2sum(:,27), M2)
  call TI_call(2, momenta_3, masses2_2, T2sum(:,28), M2)
  call TI_call(2, momenta_3, masses2_1, T2sum(:,29), M2)

  call TI_call(3, momenta_26, masses2_5, T3sum(:,1), M2)
  call TI_call(3, momenta_27, masses2_5, T3sum(:,2), M2)
  call TI_call(3, momenta_32, masses2_5, T3sum(:,3), M2)
  call TI_call(3, momenta_22, masses2_5, T3sum(:,4), M2)
  call TI_call(3, momenta_23, masses2_5, T3sum(:,5), M2)
  call TI_call(3, momenta_33, masses2_5, T3sum(:,6), M2)
  call TI_call(3, momenta_35, masses2_6, T3sum(:,7), M2)
  call TI_call(3, momenta_36, masses2_6, T3sum(:,8), M2)
  call TI_call(3, momenta_37, masses2_6, T3sum(:,9), M2)
  call TI_call(3, momenta_38, masses2_6, T3sum(:,10), M2)

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_ppwjj_ckm_uxsssxwx_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_ppwjj_ckm_uxsssxwx_1_/**/REALKIND
