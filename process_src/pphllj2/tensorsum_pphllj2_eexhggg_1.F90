
! **********************************************************************
module ol_tensor_sum_storage_pphllj2_eexhggg_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1(4,2)
  complex(REALKIND) :: momenta_2(4,2)
  complex(REALKIND) :: momenta_3(4,2)
  complex(REALKIND) :: momenta_4(4,3)
  complex(REALKIND) :: momenta_5(4,3)
  complex(REALKIND) :: momenta_6(4,3)
  complex(REALKIND) :: momenta_7(4,3)
  complex(REALKIND) :: momenta_8(4,3)
  complex(REALKIND) :: momenta_9(4,3)
  complex(REALKIND) :: momenta_10(4,3)
  complex(REALKIND) :: momenta_11(4,3)
  complex(REALKIND) :: momenta_12(4,3)
  complex(REALKIND) :: momenta_13(4,3)
  complex(REALKIND) :: momenta_14(4,3)
  complex(REALKIND) :: momenta_15(4,3)
  complex(REALKIND) :: momenta_16(4,4)
  complex(REALKIND) :: momenta_17(4,4)
  complex(REALKIND) :: momenta_18(4,4)
  complex(REALKIND) :: momenta_19(4,4)
  complex(REALKIND) :: momenta_20(4,4)
  complex(REALKIND) :: momenta_21(4,4)
  complex(REALKIND) :: momenta_22(4,4)
  complex(REALKIND) :: momenta_23(4,4)
  complex(REALKIND) :: momenta_24(4,4)
  complex(REALKIND) :: momenta_25(4,4)
  complex(REALKIND) :: momenta_26(4,4)
  complex(REALKIND) :: momenta_27(4,4)

  complex(REALKIND), save :: masses2_1(3)
  complex(REALKIND), save :: masses2_2(3)
  complex(REALKIND), save :: masses2_3(3)
  complex(REALKIND), save :: masses2_4(4)
  complex(REALKIND), save :: masses2_5(4)
  complex(REALKIND), save :: masses2_6(4)
  complex(REALKIND), save :: masses2_7(5)
  complex(REALKIND), save :: masses2_8(5)

  complex(REALKIND), save :: T3sum(35,9) = 0
  complex(REALKIND), save :: T4sum(70,27) = 0
  complex(REALKIND), save :: T5sum(126,24) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_pphllj2_eexhggg_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 5
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_pphllj2_eexhggg_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 5
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none

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
  call scale_one_tsum(T3sum(:,1), -2)
  call scale_one_tsum(T3sum(:,2), -2)
  call scale_one_tsum(T3sum(:,3), -2)
  call scale_one_tsum(T3sum(:,4), -2)
  call scale_one_tsum(T3sum(:,5), -2)
  call scale_one_tsum(T3sum(:,6), -2)
  call scale_one_tsum(T3sum(:,7), -2)
  call scale_one_tsum(T3sum(:,8), -2)
  call scale_one_tsum(T3sum(:,9), -2)
  call scale_one_tsum(T4sum(:,1), 0)
  call scale_one_tsum(T4sum(:,2), 0)
  call scale_one_tsum(T4sum(:,3), 0)
  call scale_one_tsum(T4sum(:,4), 0)
  call scale_one_tsum(T4sum(:,5), 0)
  call scale_one_tsum(T4sum(:,6), 0)
  call scale_one_tsum(T4sum(:,7), 0)
  call scale_one_tsum(T4sum(:,8), 0)
  call scale_one_tsum(T4sum(:,9), 0)
  call scale_one_tsum(T4sum(:,10), 0)
  call scale_one_tsum(T4sum(:,11), 0)
  call scale_one_tsum(T4sum(:,12), 0)
  call scale_one_tsum(T4sum(:,13), 0)
  call scale_one_tsum(T4sum(:,14), 0)
  call scale_one_tsum(T4sum(:,15), 0)
  call scale_one_tsum(T4sum(:,16), 0)
  call scale_one_tsum(T4sum(:,17), 0)
  call scale_one_tsum(T4sum(:,18), 0)
  call scale_one_tsum(T4sum(:,19), 0)
  call scale_one_tsum(T4sum(:,20), 0)
  call scale_one_tsum(T4sum(:,21), 0)
  call scale_one_tsum(T4sum(:,22), 0)
  call scale_one_tsum(T4sum(:,23), 0)
  call scale_one_tsum(T4sum(:,24), 0)
  call scale_one_tsum(T4sum(:,25), 0)
  call scale_one_tsum(T4sum(:,26), 0)
  call scale_one_tsum(T4sum(:,27), 0)
  call scale_one_tsum(T5sum(:,1), 2)
  call scale_one_tsum(T5sum(:,2), 2)
  call scale_one_tsum(T5sum(:,3), 2)
  call scale_one_tsum(T5sum(:,4), 2)
  call scale_one_tsum(T5sum(:,5), 2)
  call scale_one_tsum(T5sum(:,6), 2)
  call scale_one_tsum(T5sum(:,7), 2)
  call scale_one_tsum(T5sum(:,8), 2)
  call scale_one_tsum(T5sum(:,9), 2)
  call scale_one_tsum(T5sum(:,10), 2)
  call scale_one_tsum(T5sum(:,11), 2)
  call scale_one_tsum(T5sum(:,12), 2)
  call scale_one_tsum(T5sum(:,13), 2)
  call scale_one_tsum(T5sum(:,14), 2)
  call scale_one_tsum(T5sum(:,15), 2)
  call scale_one_tsum(T5sum(:,16), 2)
  call scale_one_tsum(T5sum(:,17), 2)
  call scale_one_tsum(T5sum(:,18), 2)
  call scale_one_tsum(T5sum(:,19), 2)
  call scale_one_tsum(T5sum(:,20), 2)
  call scale_one_tsum(T5sum(:,21), 2)
  call scale_one_tsum(T5sum(:,22), 2)
  call scale_one_tsum(T5sum(:,23), 2)
  call scale_one_tsum(T5sum(:,24), 2)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_pphllj2_eexhggg_1, only: channel_number_pphllj2_eexhggg_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_pphllj2_eexhggg_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,8)
  momenta_1(:,2) = Q(1:4,56)
  momenta_2(:,1) = Q(1:4,16)
  momenta_2(:,2) = Q(1:4,56)
  momenta_3(:,1) = Q(1:4,24)
  momenta_3(:,2) = Q(1:4,56)
  momenta_4(:,1) = Q(1:4,4)
  momenta_4(:,2) = Q(1:4,12)
  momenta_4(:,3) = Q(1:4,60)
  momenta_5(:,1) = Q(1:4,4)
  momenta_5(:,2) = Q(1:4,20)
  momenta_5(:,3) = Q(1:4,60)
  momenta_6(:,1) = Q(1:4,4)
  momenta_6(:,2) = Q(1:4,28)
  momenta_6(:,3) = Q(1:4,60)
  momenta_7(:,1) = Q(1:4,4)
  momenta_7(:,2) = Q(1:4,36)
  momenta_7(:,3) = Q(1:4,60)
  momenta_8(:,1) = Q(1:4,4)
  momenta_8(:,2) = Q(1:4,44)
  momenta_8(:,3) = Q(1:4,60)
  momenta_9(:,1) = Q(1:4,4)
  momenta_9(:,2) = Q(1:4,52)
  momenta_9(:,3) = Q(1:4,60)
  momenta_10(:,1) = Q(1:4,8)
  momenta_10(:,2) = Q(1:4,12)
  momenta_10(:,3) = Q(1:4,60)
  momenta_11(:,1) = Q(1:4,8)
  momenta_11(:,2) = Q(1:4,24)
  momenta_11(:,3) = Q(1:4,56)
  momenta_12(:,1) = Q(1:4,8)
  momenta_12(:,2) = Q(1:4,40)
  momenta_12(:,3) = Q(1:4,56)
  momenta_13(:,1) = Q(1:4,16)
  momenta_13(:,2) = Q(1:4,20)
  momenta_13(:,3) = Q(1:4,60)
  momenta_14(:,1) = Q(1:4,16)
  momenta_14(:,2) = Q(1:4,24)
  momenta_14(:,3) = Q(1:4,56)
  momenta_15(:,1) = Q(1:4,24)
  momenta_15(:,2) = Q(1:4,28)
  momenta_15(:,3) = Q(1:4,60)
  momenta_16(:,1) = Q(1:4,4)
  momenta_16(:,2) = Q(1:4,12)
  momenta_16(:,3) = Q(1:4,28)
  momenta_16(:,4) = Q(1:4,60)
  momenta_17(:,1) = Q(1:4,4)
  momenta_17(:,2) = Q(1:4,12)
  momenta_17(:,3) = Q(1:4,44)
  momenta_17(:,4) = Q(1:4,60)
  momenta_18(:,1) = Q(1:4,4)
  momenta_18(:,2) = Q(1:4,20)
  momenta_18(:,3) = Q(1:4,28)
  momenta_18(:,4) = Q(1:4,60)
  momenta_19(:,1) = Q(1:4,4)
  momenta_19(:,2) = Q(1:4,20)
  momenta_19(:,3) = Q(1:4,52)
  momenta_19(:,4) = Q(1:4,60)
  momenta_20(:,1) = Q(1:4,4)
  momenta_20(:,2) = Q(1:4,36)
  momenta_20(:,3) = Q(1:4,44)
  momenta_20(:,4) = Q(1:4,60)
  momenta_21(:,1) = Q(1:4,4)
  momenta_21(:,2) = Q(1:4,36)
  momenta_21(:,3) = Q(1:4,52)
  momenta_21(:,4) = Q(1:4,60)
  momenta_22(:,1) = Q(1:4,8)
  momenta_22(:,2) = Q(1:4,12)
  momenta_22(:,3) = Q(1:4,28)
  momenta_22(:,4) = Q(1:4,60)
  momenta_23(:,1) = Q(1:4,8)
  momenta_23(:,2) = Q(1:4,12)
  momenta_23(:,3) = Q(1:4,44)
  momenta_23(:,4) = Q(1:4,60)
  momenta_24(:,1) = Q(1:4,8)
  momenta_24(:,2) = Q(1:4,24)
  momenta_24(:,3) = Q(1:4,28)
  momenta_24(:,4) = Q(1:4,60)
  momenta_25(:,1) = Q(1:4,8)
  momenta_25(:,2) = Q(1:4,40)
  momenta_25(:,3) = Q(1:4,44)
  momenta_25(:,4) = Q(1:4,60)
  momenta_26(:,1) = Q(1:4,16)
  momenta_26(:,2) = Q(1:4,20)
  momenta_26(:,3) = Q(1:4,28)
  momenta_26(:,4) = Q(1:4,60)
  momenta_27(:,1) = Q(1:4,16)
  momenta_27(:,2) = Q(1:4,24)
  momenta_27(:,3) = Q(1:4,28)
  momenta_27(:,4) = Q(1:4,60)

  masses2_1 = [ MB2, MB2, MB2 ]
  masses2_2 = [ MT2, MT2, MT2 ]
  masses2_3 = [ ZERO2, ZERO2, ZERO2 ]
  masses2_4 = [ MB2, MB2, MB2, MB2 ]
  masses2_5 = [ MT2, MT2, MT2, MT2 ]
  masses2_6 = [ ZERO2, ZERO2, ZERO2, ZERO2 ]
  masses2_7 = [ MB2, MB2, MB2, MB2, MB2 ]
  masses2_8 = [ MT2, MT2, MT2, MT2, MT2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call tensor_integral(3, momenta_3, masses2_3, T3sum(:,1))
  call tensor_integral(3, momenta_3, masses2_2, T3sum(:,2))
  call tensor_integral(3, momenta_3, masses2_1, T3sum(:,3))
  call tensor_integral(3, momenta_2, masses2_3, T3sum(:,4))
  call tensor_integral(3, momenta_2, masses2_2, T3sum(:,5))
  call tensor_integral(3, momenta_2, masses2_1, T3sum(:,6))
  call tensor_integral(3, momenta_1, masses2_3, T3sum(:,7))
  call tensor_integral(3, momenta_1, masses2_2, T3sum(:,8))
  call tensor_integral(3, momenta_1, masses2_1, T3sum(:,9))

  call tensor_integral(4, momenta_7, masses2_5, T4sum(:,1))
  call tensor_integral(4, momenta_7, masses2_4, T4sum(:,2))
  call tensor_integral(4, momenta_15, masses2_5, T4sum(:,3))
  call tensor_integral(4, momenta_15, masses2_4, T4sum(:,4))
  call tensor_integral(4, momenta_6, masses2_5, T4sum(:,5))
  call tensor_integral(4, momenta_6, masses2_4, T4sum(:,6))
  call tensor_integral(4, momenta_5, masses2_5, T4sum(:,7))
  call tensor_integral(4, momenta_5, masses2_4, T4sum(:,8))
  call tensor_integral(4, momenta_13, masses2_5, T4sum(:,9))
  call tensor_integral(4, momenta_13, masses2_4, T4sum(:,10))
  call tensor_integral(4, momenta_8, masses2_5, T4sum(:,11))
  call tensor_integral(4, momenta_8, masses2_4, T4sum(:,12))
  call tensor_integral(4, momenta_4, masses2_5, T4sum(:,13))
  call tensor_integral(4, momenta_4, masses2_4, T4sum(:,14))
  call tensor_integral(4, momenta_10, masses2_5, T4sum(:,15))
  call tensor_integral(4, momenta_10, masses2_4, T4sum(:,16))
  call tensor_integral(4, momenta_9, masses2_5, T4sum(:,17))
  call tensor_integral(4, momenta_9, masses2_4, T4sum(:,18))
  call tensor_integral(4, momenta_14, masses2_6, T4sum(:,19))
  call tensor_integral(4, momenta_14, masses2_5, T4sum(:,20))
  call tensor_integral(4, momenta_14, masses2_4, T4sum(:,21))
  call tensor_integral(4, momenta_11, masses2_6, T4sum(:,22))
  call tensor_integral(4, momenta_11, masses2_5, T4sum(:,23))
  call tensor_integral(4, momenta_11, masses2_4, T4sum(:,24))
  call tensor_integral(4, momenta_12, masses2_6, T4sum(:,25))
  call tensor_integral(4, momenta_12, masses2_5, T4sum(:,26))
  call tensor_integral(4, momenta_12, masses2_4, T4sum(:,27))

  call tensor_integral(5, momenta_26, masses2_8, T5sum(:,1))
  call tensor_integral(5, momenta_26, masses2_7, T5sum(:,2))
  call tensor_integral(5, momenta_22, masses2_8, T5sum(:,3))
  call tensor_integral(5, momenta_22, masses2_7, T5sum(:,4))
  call tensor_integral(5, momenta_27, masses2_8, T5sum(:,5))
  call tensor_integral(5, momenta_27, masses2_7, T5sum(:,6))
  call tensor_integral(5, momenta_23, masses2_8, T5sum(:,7))
  call tensor_integral(5, momenta_23, masses2_7, T5sum(:,8))
  call tensor_integral(5, momenta_16, masses2_8, T5sum(:,9))
  call tensor_integral(5, momenta_16, masses2_7, T5sum(:,10))
  call tensor_integral(5, momenta_17, masses2_8, T5sum(:,11))
  call tensor_integral(5, momenta_17, masses2_7, T5sum(:,12))
  call tensor_integral(5, momenta_24, masses2_8, T5sum(:,13))
  call tensor_integral(5, momenta_24, masses2_7, T5sum(:,14))
  call tensor_integral(5, momenta_25, masses2_8, T5sum(:,15))
  call tensor_integral(5, momenta_25, masses2_7, T5sum(:,16))
  call tensor_integral(5, momenta_18, masses2_8, T5sum(:,17))
  call tensor_integral(5, momenta_18, masses2_7, T5sum(:,18))
  call tensor_integral(5, momenta_19, masses2_8, T5sum(:,19))
  call tensor_integral(5, momenta_19, masses2_7, T5sum(:,20))
  call tensor_integral(5, momenta_20, masses2_8, T5sum(:,21))
  call tensor_integral(5, momenta_20, masses2_7, T5sum(:,22))
  call tensor_integral(5, momenta_21, masses2_8, T5sum(:,23))
  call tensor_integral(5, momenta_21, masses2_7, T5sum(:,24))

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_pphllj2_eexhggg_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_pphllj2_eexhggg_1_/**/REALKIND
