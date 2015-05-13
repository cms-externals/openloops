
! **********************************************************************
module ol_tensor_sum_storage_pplnjj_nexeudxgg_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1(4,1)
  complex(REALKIND) :: momenta_2(4,1)
  complex(REALKIND) :: momenta_3(4,1)
  complex(REALKIND) :: momenta_4(4,1)
  complex(REALKIND) :: momenta_5(4,1)
  complex(REALKIND) :: momenta_6(4,1)
  complex(REALKIND) :: momenta_7(4,1)
  complex(REALKIND) :: momenta_8(4,1)
  complex(REALKIND) :: momenta_9(4,1)
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
  complex(REALKIND) :: momenta_21(4,2)
  complex(REALKIND) :: momenta_22(4,2)
  complex(REALKIND) :: momenta_23(4,2)
  complex(REALKIND) :: momenta_24(4,2)
  complex(REALKIND) :: momenta_25(4,2)
  complex(REALKIND) :: momenta_26(4,2)
  complex(REALKIND) :: momenta_27(4,2)
  complex(REALKIND) :: momenta_28(4,2)
  complex(REALKIND) :: momenta_29(4,2)
  complex(REALKIND) :: momenta_30(4,2)
  complex(REALKIND) :: momenta_31(4,2)
  complex(REALKIND) :: momenta_32(4,2)
  complex(REALKIND) :: momenta_33(4,2)
  complex(REALKIND) :: momenta_34(4,2)
  complex(REALKIND) :: momenta_35(4,3)
  complex(REALKIND) :: momenta_36(4,3)
  complex(REALKIND) :: momenta_37(4,3)
  complex(REALKIND) :: momenta_38(4,3)
  complex(REALKIND) :: momenta_39(4,3)
  complex(REALKIND) :: momenta_40(4,3)
  complex(REALKIND) :: momenta_41(4,3)
  complex(REALKIND) :: momenta_42(4,3)
  complex(REALKIND) :: momenta_43(4,3)
  complex(REALKIND) :: momenta_44(4,3)
  complex(REALKIND) :: momenta_45(4,3)
  complex(REALKIND) :: momenta_46(4,3)
  complex(REALKIND) :: momenta_47(4,3)
  complex(REALKIND) :: momenta_48(4,3)
  complex(REALKIND) :: momenta_49(4,3)
  complex(REALKIND) :: momenta_50(4,3)
  complex(REALKIND) :: momenta_51(4,3)
  complex(REALKIND) :: momenta_52(4,3)
  complex(REALKIND) :: momenta_53(4,3)
  complex(REALKIND) :: momenta_54(4,3)
  complex(REALKIND) :: momenta_55(4,3)
  complex(REALKIND) :: momenta_56(4,3)
  complex(REALKIND) :: momenta_57(4,3)
  complex(REALKIND) :: momenta_58(4,3)
  complex(REALKIND) :: momenta_59(4,3)
  complex(REALKIND) :: momenta_60(4,3)
  complex(REALKIND) :: momenta_61(4,3)
  complex(REALKIND) :: momenta_62(4,4)
  complex(REALKIND) :: momenta_63(4,4)
  complex(REALKIND) :: momenta_64(4,4)
  complex(REALKIND) :: momenta_65(4,4)
  complex(REALKIND) :: momenta_66(4,4)
  complex(REALKIND) :: momenta_67(4,4)
  complex(REALKIND) :: momenta_68(4,4)
  complex(REALKIND) :: momenta_69(4,4)
  complex(REALKIND) :: momenta_70(4,4)
  complex(REALKIND) :: momenta_71(4,4)
  complex(REALKIND) :: momenta_72(4,4)
  complex(REALKIND) :: momenta_73(4,4)

  complex(REALKIND), save :: masses2_1(2)
  complex(REALKIND), save :: masses2_2(2)
  complex(REALKIND), save :: masses2_3(2)
  complex(REALKIND), save :: masses2_4(3)
  complex(REALKIND), save :: masses2_5(3)
  complex(REALKIND), save :: masses2_6(3)
  complex(REALKIND), save :: masses2_7(4)
  complex(REALKIND), save :: masses2_8(5)

  complex(REALKIND), save :: T1sum(5,8) = 0
  complex(REALKIND), save :: T2sum(15,27) = 0
  complex(REALKIND), save :: T3sum(35,30) = 0
  complex(REALKIND), save :: T4sum(70,12) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_pplnjj_nexeudxgg_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 5
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_pplnjj_nexeudxgg_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 4
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none
  T1sum = 0
  T2sum = 0
  T3sum = 0
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
  call scale_one_tsum(T1sum(:,1), -4)
  call scale_one_tsum(T1sum(:,2), -4)
  call scale_one_tsum(T1sum(:,3), -4)
  call scale_one_tsum(T1sum(:,4), -4)
  call scale_one_tsum(T1sum(:,5), -4)
  call scale_one_tsum(T1sum(:,6), -4)
  call scale_one_tsum(T1sum(:,7), -4)
  call scale_one_tsum(T1sum(:,8), -4)
  call scale_one_tsum(T2sum(:,1), -2)
  call scale_one_tsum(T2sum(:,2), -2)
  call scale_one_tsum(T2sum(:,3), -2)
  call scale_one_tsum(T2sum(:,4), -2)
  call scale_one_tsum(T2sum(:,5), -2)
  call scale_one_tsum(T2sum(:,6), -2)
  call scale_one_tsum(T2sum(:,7), -2)
  call scale_one_tsum(T2sum(:,8), -2)
  call scale_one_tsum(T2sum(:,9), -2)
  call scale_one_tsum(T2sum(:,10), -2)
  call scale_one_tsum(T2sum(:,11), -2)
  call scale_one_tsum(T2sum(:,12), -2)
  call scale_one_tsum(T2sum(:,13), -2)
  call scale_one_tsum(T2sum(:,14), -2)
  call scale_one_tsum(T2sum(:,15), -2)
  call scale_one_tsum(T2sum(:,16), -2)
  call scale_one_tsum(T2sum(:,17), -2)
  call scale_one_tsum(T2sum(:,18), -2)
  call scale_one_tsum(T2sum(:,19), -2)
  call scale_one_tsum(T2sum(:,20), -2)
  call scale_one_tsum(T2sum(:,21), -2)
  call scale_one_tsum(T2sum(:,22), -2)
  call scale_one_tsum(T2sum(:,23), -2)
  call scale_one_tsum(T2sum(:,24), -2)
  call scale_one_tsum(T2sum(:,25), -4)
  call scale_one_tsum(T2sum(:,26), -4)
  call scale_one_tsum(T2sum(:,27), -4)
  call scale_one_tsum(T3sum(:,1), 0)
  call scale_one_tsum(T3sum(:,2), 0)
  call scale_one_tsum(T3sum(:,3), 0)
  call scale_one_tsum(T3sum(:,4), 0)
  call scale_one_tsum(T3sum(:,5), 0)
  call scale_one_tsum(T3sum(:,6), 0)
  call scale_one_tsum(T3sum(:,7), 0)
  call scale_one_tsum(T3sum(:,8), 0)
  call scale_one_tsum(T3sum(:,9), 0)
  call scale_one_tsum(T3sum(:,10), 0)
  call scale_one_tsum(T3sum(:,11), 0)
  call scale_one_tsum(T3sum(:,12), 0)
  call scale_one_tsum(T3sum(:,13), 0)
  call scale_one_tsum(T3sum(:,14), 0)
  call scale_one_tsum(T3sum(:,15), 0)
  call scale_one_tsum(T3sum(:,16), -2)
  call scale_one_tsum(T3sum(:,17), -2)
  call scale_one_tsum(T3sum(:,18), -2)
  call scale_one_tsum(T3sum(:,19), 0)
  call scale_one_tsum(T3sum(:,20), 0)
  call scale_one_tsum(T3sum(:,21), 0)
  call scale_one_tsum(T3sum(:,22), 0)
  call scale_one_tsum(T3sum(:,23), 0)
  call scale_one_tsum(T3sum(:,24), 0)
  call scale_one_tsum(T3sum(:,25), 0)
  call scale_one_tsum(T3sum(:,26), 0)
  call scale_one_tsum(T3sum(:,27), 0)
  call scale_one_tsum(T3sum(:,28), 0)
  call scale_one_tsum(T3sum(:,29), 0)
  call scale_one_tsum(T3sum(:,30), 0)
  call scale_one_tsum(T4sum(:,1), 2)
  call scale_one_tsum(T4sum(:,2), 2)
  call scale_one_tsum(T4sum(:,3), 2)
  call scale_one_tsum(T4sum(:,4), 2)
  call scale_one_tsum(T4sum(:,5), 2)
  call scale_one_tsum(T4sum(:,6), 2)
  call scale_one_tsum(T4sum(:,7), 2)
  call scale_one_tsum(T4sum(:,8), 2)
  call scale_one_tsum(T4sum(:,9), 2)
  call scale_one_tsum(T4sum(:,10), 2)
  call scale_one_tsum(T4sum(:,11), 2)
  call scale_one_tsum(T4sum(:,12), 2)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_pplnjj_nexeudxgg_1, only: channel_number_pplnjj_nexeudxgg_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_pplnjj_nexeudxgg_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,32)
  momenta_2(:,1) = Q(1:4,36)
  momenta_3(:,1) = Q(1:4,39)
  momenta_4(:,1) = Q(1:4,40)
  momenta_5(:,1) = Q(1:4,43)
  momenta_6(:,1) = Q(1:4,47)
  momenta_7(:,1) = Q(1:4,48)
  momenta_8(:,1) = Q(1:4,52)
  momenta_9(:,1) = Q(1:4,56)
  momenta_10(:,1) = Q(1:4,8)
  momenta_10(:,2) = Q(1:4,56)
  momenta_11(:,1) = Q(1:4,11)
  momenta_11(:,2) = Q(1:4,59)
  momenta_12(:,1) = Q(1:4,16)
  momenta_12(:,2) = Q(1:4,48)
  momenta_13(:,1) = Q(1:4,16)
  momenta_13(:,2) = Q(1:4,52)
  momenta_14(:,1) = Q(1:4,16)
  momenta_14(:,2) = Q(1:4,55)
  momenta_15(:,1) = Q(1:4,16)
  momenta_15(:,2) = Q(1:4,56)
  momenta_16(:,1) = Q(1:4,16)
  momenta_16(:,2) = Q(1:4,59)
  momenta_17(:,1) = Q(1:4,20)
  momenta_17(:,2) = Q(1:4,52)
  momenta_18(:,1) = Q(1:4,23)
  momenta_18(:,2) = Q(1:4,55)
  momenta_19(:,1) = Q(1:4,24)
  momenta_19(:,2) = Q(1:4,56)
  momenta_20(:,1) = Q(1:4,27)
  momenta_20(:,2) = Q(1:4,59)
  momenta_21(:,1) = Q(1:4,32)
  momenta_21(:,2) = Q(1:4,52)
  momenta_22(:,1) = Q(1:4,32)
  momenta_22(:,2) = Q(1:4,55)
  momenta_23(:,1) = Q(1:4,32)
  momenta_23(:,2) = Q(1:4,56)
  momenta_24(:,1) = Q(1:4,32)
  momenta_24(:,2) = Q(1:4,59)
  momenta_25(:,1) = Q(1:4,36)
  momenta_25(:,2) = Q(1:4,39)
  momenta_26(:,1) = Q(1:4,36)
  momenta_26(:,2) = Q(1:4,52)
  momenta_27(:,1) = Q(1:4,39)
  momenta_27(:,2) = Q(1:4,55)
  momenta_28(:,1) = Q(1:4,40)
  momenta_28(:,2) = Q(1:4,43)
  momenta_29(:,1) = Q(1:4,40)
  momenta_29(:,2) = Q(1:4,56)
  momenta_30(:,1) = Q(1:4,43)
  momenta_30(:,2) = Q(1:4,59)
  momenta_31(:,1) = Q(1:4,48)
  momenta_31(:,2) = Q(1:4,56)
  momenta_32(:,1) = Q(1:4,48)
  momenta_32(:,2) = Q(1:4,59)
  momenta_33(:,1) = Q(1:4,52)
  momenta_33(:,2) = Q(1:4,55)
  momenta_34(:,1) = Q(1:4,56)
  momenta_34(:,2) = Q(1:4,59)
  momenta_35(:,1) = Q(1:4,8)
  momenta_35(:,2) = Q(1:4,11)
  momenta_35(:,3) = Q(1:4,59)
  momenta_36(:,1) = Q(1:4,8)
  momenta_36(:,2) = Q(1:4,24)
  momenta_36(:,3) = Q(1:4,56)
  momenta_37(:,1) = Q(1:4,8)
  momenta_37(:,2) = Q(1:4,40)
  momenta_37(:,3) = Q(1:4,56)
  momenta_38(:,1) = Q(1:4,8)
  momenta_38(:,2) = Q(1:4,56)
  momenta_38(:,3) = Q(1:4,59)
  momenta_39(:,1) = Q(1:4,11)
  momenta_39(:,2) = Q(1:4,27)
  momenta_39(:,3) = Q(1:4,59)
  momenta_40(:,1) = Q(1:4,11)
  momenta_40(:,2) = Q(1:4,43)
  momenta_40(:,3) = Q(1:4,59)
  momenta_41(:,1) = Q(1:4,16)
  momenta_41(:,2) = Q(1:4,24)
  momenta_41(:,3) = Q(1:4,56)
  momenta_42(:,1) = Q(1:4,16)
  momenta_42(:,2) = Q(1:4,27)
  momenta_42(:,3) = Q(1:4,59)
  momenta_43(:,1) = Q(1:4,16)
  momenta_43(:,2) = Q(1:4,48)
  momenta_43(:,3) = Q(1:4,56)
  momenta_44(:,1) = Q(1:4,16)
  momenta_44(:,2) = Q(1:4,48)
  momenta_44(:,3) = Q(1:4,59)
  momenta_45(:,1) = Q(1:4,16)
  momenta_45(:,2) = Q(1:4,52)
  momenta_45(:,3) = Q(1:4,55)
  momenta_46(:,1) = Q(1:4,16)
  momenta_46(:,2) = Q(1:4,56)
  momenta_46(:,3) = Q(1:4,59)
  momenta_47(:,1) = Q(1:4,20)
  momenta_47(:,2) = Q(1:4,23)
  momenta_47(:,3) = Q(1:4,55)
  momenta_48(:,1) = Q(1:4,20)
  momenta_48(:,2) = Q(1:4,52)
  momenta_48(:,3) = Q(1:4,55)
  momenta_49(:,1) = Q(1:4,24)
  momenta_49(:,2) = Q(1:4,27)
  momenta_49(:,3) = Q(1:4,59)
  momenta_50(:,1) = Q(1:4,24)
  momenta_50(:,2) = Q(1:4,56)
  momenta_50(:,3) = Q(1:4,59)
  momenta_51(:,1) = Q(1:4,32)
  momenta_51(:,2) = Q(1:4,40)
  momenta_51(:,3) = Q(1:4,56)
  momenta_52(:,1) = Q(1:4,32)
  momenta_52(:,2) = Q(1:4,43)
  momenta_52(:,3) = Q(1:4,59)
  momenta_53(:,1) = Q(1:4,32)
  momenta_53(:,2) = Q(1:4,48)
  momenta_53(:,3) = Q(1:4,56)
  momenta_54(:,1) = Q(1:4,32)
  momenta_54(:,2) = Q(1:4,48)
  momenta_54(:,3) = Q(1:4,59)
  momenta_55(:,1) = Q(1:4,32)
  momenta_55(:,2) = Q(1:4,52)
  momenta_55(:,3) = Q(1:4,55)
  momenta_56(:,1) = Q(1:4,32)
  momenta_56(:,2) = Q(1:4,56)
  momenta_56(:,3) = Q(1:4,59)
  momenta_57(:,1) = Q(1:4,36)
  momenta_57(:,2) = Q(1:4,39)
  momenta_57(:,3) = Q(1:4,55)
  momenta_58(:,1) = Q(1:4,36)
  momenta_58(:,2) = Q(1:4,52)
  momenta_58(:,3) = Q(1:4,55)
  momenta_59(:,1) = Q(1:4,40)
  momenta_59(:,2) = Q(1:4,43)
  momenta_59(:,3) = Q(1:4,59)
  momenta_60(:,1) = Q(1:4,40)
  momenta_60(:,2) = Q(1:4,56)
  momenta_60(:,3) = Q(1:4,59)
  momenta_61(:,1) = Q(1:4,48)
  momenta_61(:,2) = Q(1:4,56)
  momenta_61(:,3) = Q(1:4,59)
  momenta_62(:,1) = Q(1:4,8)
  momenta_62(:,2) = Q(1:4,11)
  momenta_62(:,3) = Q(1:4,27)
  momenta_62(:,4) = Q(1:4,59)
  momenta_63(:,1) = Q(1:4,8)
  momenta_63(:,2) = Q(1:4,11)
  momenta_63(:,3) = Q(1:4,43)
  momenta_63(:,4) = Q(1:4,59)
  momenta_64(:,1) = Q(1:4,8)
  momenta_64(:,2) = Q(1:4,24)
  momenta_64(:,3) = Q(1:4,27)
  momenta_64(:,4) = Q(1:4,59)
  momenta_65(:,1) = Q(1:4,8)
  momenta_65(:,2) = Q(1:4,24)
  momenta_65(:,3) = Q(1:4,56)
  momenta_65(:,4) = Q(1:4,59)
  momenta_66(:,1) = Q(1:4,8)
  momenta_66(:,2) = Q(1:4,40)
  momenta_66(:,3) = Q(1:4,43)
  momenta_66(:,4) = Q(1:4,59)
  momenta_67(:,1) = Q(1:4,8)
  momenta_67(:,2) = Q(1:4,40)
  momenta_67(:,3) = Q(1:4,56)
  momenta_67(:,4) = Q(1:4,59)
  momenta_68(:,1) = Q(1:4,16)
  momenta_68(:,2) = Q(1:4,24)
  momenta_68(:,3) = Q(1:4,27)
  momenta_68(:,4) = Q(1:4,59)
  momenta_69(:,1) = Q(1:4,16)
  momenta_69(:,2) = Q(1:4,24)
  momenta_69(:,3) = Q(1:4,56)
  momenta_69(:,4) = Q(1:4,59)
  momenta_70(:,1) = Q(1:4,16)
  momenta_70(:,2) = Q(1:4,48)
  momenta_70(:,3) = Q(1:4,56)
  momenta_70(:,4) = Q(1:4,59)
  momenta_71(:,1) = Q(1:4,32)
  momenta_71(:,2) = Q(1:4,40)
  momenta_71(:,3) = Q(1:4,43)
  momenta_71(:,4) = Q(1:4,59)
  momenta_72(:,1) = Q(1:4,32)
  momenta_72(:,2) = Q(1:4,40)
  momenta_72(:,3) = Q(1:4,56)
  momenta_72(:,4) = Q(1:4,59)
  momenta_73(:,1) = Q(1:4,32)
  momenta_73(:,2) = Q(1:4,48)
  momenta_73(:,3) = Q(1:4,56)
  momenta_73(:,4) = Q(1:4,59)

  masses2_1 = [ MB2, MB2 ]
  masses2_2 = [ MT2, MT2 ]
  masses2_3 = [ ZERO2, ZERO2 ]
  masses2_4 = [ MB2, MB2, MB2 ]
  masses2_5 = [ MT2, MT2, MT2 ]
  masses2_6 = [ ZERO2, ZERO2, ZERO2 ]
  masses2_7 = [ ZERO2, ZERO2, ZERO2, ZERO2 ]
  masses2_8 = [ ZERO2, ZERO2, ZERO2, ZERO2, ZERO2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call TI_call(1, momenta_6, masses2_3, T1sum(:,1), M2)
  call TI_call(1, momenta_1, masses2_3, T1sum(:,2), M2)
  call TI_call(1, momenta_4, masses2_3, T1sum(:,3), M2)
  call TI_call(1, momenta_5, masses2_3, T1sum(:,4), M2)
  call TI_call(1, momenta_8, masses2_3, T1sum(:,5), M2)
  call TI_call(1, momenta_3, masses2_3, T1sum(:,6), M2)
  call TI_call(1, momenta_2, masses2_3, T1sum(:,7), M2)
  call TI_call(1, momenta_9, masses2_3, T1sum(:,8), M2)

  call TI_call(2, momenta_31, masses2_6, T2sum(:,1), M2)
  call TI_call(2, momenta_32, masses2_6, T2sum(:,2), M2)
  call TI_call(2, momenta_28, masses2_6, T2sum(:,3), M2)
  call TI_call(2, momenta_17, masses2_6, T2sum(:,4), M2)
  call TI_call(2, momenta_21, masses2_6, T2sum(:,5), M2)
  call TI_call(2, momenta_33, masses2_6, T2sum(:,6), M2)
  call TI_call(2, momenta_18, masses2_6, T2sum(:,7), M2)
  call TI_call(2, momenta_22, masses2_6, T2sum(:,8), M2)
  call TI_call(2, momenta_25, masses2_6, T2sum(:,9), M2)
  call TI_call(2, momenta_19, masses2_6, T2sum(:,10), M2)
  call TI_call(2, momenta_23, masses2_6, T2sum(:,11), M2)
  call TI_call(2, momenta_34, masses2_6, T2sum(:,12), M2)
  call TI_call(2, momenta_20, masses2_6, T2sum(:,13), M2)
  call TI_call(2, momenta_24, masses2_6, T2sum(:,14), M2)
  call TI_call(2, momenta_26, masses2_6, T2sum(:,15), M2)
  call TI_call(2, momenta_13, masses2_6, T2sum(:,16), M2)
  call TI_call(2, momenta_27, masses2_6, T2sum(:,17), M2)
  call TI_call(2, momenta_14, masses2_6, T2sum(:,18), M2)
  call TI_call(2, momenta_29, masses2_6, T2sum(:,19), M2)
  call TI_call(2, momenta_15, masses2_6, T2sum(:,20), M2)
  call TI_call(2, momenta_30, masses2_6, T2sum(:,21), M2)
  call TI_call(2, momenta_16, masses2_6, T2sum(:,22), M2)
  call TI_call(2, momenta_10, masses2_6, T2sum(:,23), M2)
  call TI_call(2, momenta_11, masses2_6, T2sum(:,24), M2)
  call TI_call(2, momenta_7, masses2_3, T2sum(:,25), M2)
  call TI_call(2, momenta_7, masses2_2, T2sum(:,26), M2)
  call TI_call(2, momenta_7, masses2_1, T2sum(:,27), M2)

  call TI_call(3, momenta_61, masses2_7, T3sum(:,1), M2)
  call TI_call(3, momenta_55, masses2_7, T3sum(:,2), M2)
  call TI_call(3, momenta_47, masses2_7, T3sum(:,3), M2)
  call TI_call(3, momenta_48, masses2_7, T3sum(:,4), M2)
  call TI_call(3, momenta_56, masses2_7, T3sum(:,5), M2)
  call TI_call(3, momenta_49, masses2_7, T3sum(:,6), M2)
  call TI_call(3, momenta_50, masses2_7, T3sum(:,7), M2)
  call TI_call(3, momenta_45, masses2_7, T3sum(:,8), M2)
  call TI_call(3, momenta_57, masses2_7, T3sum(:,9), M2)
  call TI_call(3, momenta_58, masses2_7, T3sum(:,10), M2)
  call TI_call(3, momenta_46, masses2_7, T3sum(:,11), M2)
  call TI_call(3, momenta_59, masses2_7, T3sum(:,12), M2)
  call TI_call(3, momenta_60, masses2_7, T3sum(:,13), M2)
  call TI_call(3, momenta_38, masses2_7, T3sum(:,14), M2)
  call TI_call(3, momenta_35, masses2_7, T3sum(:,15), M2)
  call TI_call(3, momenta_12, masses2_6, T3sum(:,16), M2)
  call TI_call(3, momenta_12, masses2_5, T3sum(:,17), M2)
  call TI_call(3, momenta_12, masses2_4, T3sum(:,18), M2)
  call TI_call(3, momenta_51, masses2_7, T3sum(:,19), M2)
  call TI_call(3, momenta_41, masses2_7, T3sum(:,20), M2)
  call TI_call(3, momenta_36, masses2_7, T3sum(:,21), M2)
  call TI_call(3, momenta_53, masses2_7, T3sum(:,22), M2)
  call TI_call(3, momenta_37, masses2_7, T3sum(:,23), M2)
  call TI_call(3, momenta_43, masses2_7, T3sum(:,24), M2)
  call TI_call(3, momenta_52, masses2_7, T3sum(:,25), M2)
  call TI_call(3, momenta_42, masses2_7, T3sum(:,26), M2)
  call TI_call(3, momenta_40, masses2_7, T3sum(:,27), M2)
  call TI_call(3, momenta_44, masses2_7, T3sum(:,28), M2)
  call TI_call(3, momenta_39, masses2_7, T3sum(:,29), M2)
  call TI_call(3, momenta_54, masses2_7, T3sum(:,30), M2)

  call TI_call(4, momenta_66, masses2_8, T4sum(:,1), M2)
  call TI_call(4, momenta_63, masses2_8, T4sum(:,2), M2)
  call TI_call(4, momenta_64, masses2_8, T4sum(:,3), M2)
  call TI_call(4, momenta_62, masses2_8, T4sum(:,4), M2)
  call TI_call(4, momenta_65, masses2_8, T4sum(:,5), M2)
  call TI_call(4, momenta_67, masses2_8, T4sum(:,6), M2)
  call TI_call(4, momenta_68, masses2_8, T4sum(:,7), M2)
  call TI_call(4, momenta_71, masses2_8, T4sum(:,8), M2)
  call TI_call(4, momenta_69, masses2_8, T4sum(:,9), M2)
  call TI_call(4, momenta_70, masses2_8, T4sum(:,10), M2)
  call TI_call(4, momenta_72, masses2_8, T4sum(:,11), M2)
  call TI_call(4, momenta_73, masses2_8, T4sum(:,12), M2)

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_pplnjj_nexeudxgg_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_pplnjj_nexeudxgg_1_/**/REALKIND
