
! **********************************************************************
module ol_tensor_sum_storage_pphbb_bbbxbxh_1_/**/REALKIND
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
  complex(REALKIND) :: momenta_35(4,2)
  complex(REALKIND) :: momenta_36(4,2)
  complex(REALKIND) :: momenta_37(4,2)
  complex(REALKIND) :: momenta_38(4,2)
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
  complex(REALKIND) :: momenta_62(4,3)
  complex(REALKIND) :: momenta_63(4,3)
  complex(REALKIND) :: momenta_64(4,3)
  complex(REALKIND) :: momenta_65(4,3)
  complex(REALKIND) :: momenta_66(4,3)
  complex(REALKIND) :: momenta_67(4,4)
  complex(REALKIND) :: momenta_68(4,4)
  complex(REALKIND) :: momenta_69(4,4)
  complex(REALKIND) :: momenta_70(4,4)
  complex(REALKIND) :: momenta_71(4,4)
  complex(REALKIND) :: momenta_72(4,4)
  complex(REALKIND) :: momenta_73(4,4)
  complex(REALKIND) :: momenta_74(4,4)

  complex(REALKIND), save :: masses2_1(2)
  complex(REALKIND), save :: masses2_2(2)
  complex(REALKIND), save :: masses2_3(2)
  complex(REALKIND), save :: masses2_4(2)
  complex(REALKIND), save :: masses2_5(3)
  complex(REALKIND), save :: masses2_6(3)
  complex(REALKIND), save :: masses2_7(3)
  complex(REALKIND), save :: masses2_8(3)
  complex(REALKIND), save :: masses2_9(3)
  complex(REALKIND), save :: masses2_10(4)
  complex(REALKIND), save :: masses2_11(4)
  complex(REALKIND), save :: masses2_12(4)
  complex(REALKIND), save :: masses2_13(5)
  complex(REALKIND), save :: masses2_14(5)

  complex(REALKIND), save :: T1sum(5,4) = 0
  complex(REALKIND), save :: T2sum(15,56) = 0
  complex(REALKIND), save :: T3sum(35,24) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_pphbb_bbbxbxh_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 5
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_pphbb_bbbxbxh_1")
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
  call scale_one_tsum(T1sum(:,4), -2)
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
  call scale_one_tsum(T2sum(:,16), 0)
  call scale_one_tsum(T2sum(:,17), 0)
  call scale_one_tsum(T2sum(:,18), 0)
  call scale_one_tsum(T2sum(:,19), 0)
  call scale_one_tsum(T2sum(:,20), 0)
  call scale_one_tsum(T2sum(:,21), 0)
  call scale_one_tsum(T2sum(:,22), 0)
  call scale_one_tsum(T2sum(:,23), 0)
  call scale_one_tsum(T2sum(:,24), 0)
  call scale_one_tsum(T2sum(:,25), 2)
  call scale_one_tsum(T2sum(:,26), 2)
  call scale_one_tsum(T2sum(:,27), 2)
  call scale_one_tsum(T2sum(:,28), 2)
  call scale_one_tsum(T2sum(:,29), 0)
  call scale_one_tsum(T2sum(:,30), 0)
  call scale_one_tsum(T2sum(:,31), 0)
  call scale_one_tsum(T2sum(:,32), 0)
  call scale_one_tsum(T2sum(:,33), 2)
  call scale_one_tsum(T2sum(:,34), 2)
  call scale_one_tsum(T2sum(:,35), 2)
  call scale_one_tsum(T2sum(:,36), 2)
  call scale_one_tsum(T2sum(:,37), 2)
  call scale_one_tsum(T2sum(:,38), 2)
  call scale_one_tsum(T2sum(:,39), 2)
  call scale_one_tsum(T2sum(:,40), 2)
  call scale_one_tsum(T2sum(:,41), 2)
  call scale_one_tsum(T2sum(:,42), 2)
  call scale_one_tsum(T2sum(:,43), 2)
  call scale_one_tsum(T2sum(:,44), 2)
  call scale_one_tsum(T2sum(:,45), -2)
  call scale_one_tsum(T2sum(:,46), -2)
  call scale_one_tsum(T2sum(:,47), -2)
  call scale_one_tsum(T2sum(:,48), -2)
  call scale_one_tsum(T2sum(:,49), -2)
  call scale_one_tsum(T2sum(:,50), -2)
  call scale_one_tsum(T2sum(:,51), -2)
  call scale_one_tsum(T2sum(:,52), -2)
  call scale_one_tsum(T2sum(:,53), -2)
  call scale_one_tsum(T2sum(:,54), -2)
  call scale_one_tsum(T2sum(:,55), -2)
  call scale_one_tsum(T2sum(:,56), -2)
  call scale_one_tsum(T3sum(:,1), 0)
  call scale_one_tsum(T3sum(:,2), 0)
  call scale_one_tsum(T3sum(:,3), 2)
  call scale_one_tsum(T3sum(:,4), 2)
  call scale_one_tsum(T3sum(:,5), 2)
  call scale_one_tsum(T3sum(:,6), 0)
  call scale_one_tsum(T3sum(:,7), 0)
  call scale_one_tsum(T3sum(:,8), 2)
  call scale_one_tsum(T3sum(:,9), 2)
  call scale_one_tsum(T3sum(:,10), 2)
  call scale_one_tsum(T3sum(:,11), 2)
  call scale_one_tsum(T3sum(:,12), 2)
  call scale_one_tsum(T3sum(:,13), 2)
  call scale_one_tsum(T3sum(:,14), 2)
  call scale_one_tsum(T3sum(:,15), 2)
  call scale_one_tsum(T3sum(:,16), 2)
  call scale_one_tsum(T3sum(:,17), 4)
  call scale_one_tsum(T3sum(:,18), 4)
  call scale_one_tsum(T3sum(:,19), 4)
  call scale_one_tsum(T3sum(:,20), 4)
  call scale_one_tsum(T3sum(:,21), 4)
  call scale_one_tsum(T3sum(:,22), 4)
  call scale_one_tsum(T3sum(:,23), 4)
  call scale_one_tsum(T3sum(:,24), 4)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_pphbb_bbbxbxh_1, only: channel_number_pphbb_bbbxbxh_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_pphbb_bbbxbxh_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,17)
  momenta_2(:,1) = Q(1:4,18)
  momenta_3(:,1) = Q(1:4,20)
  momenta_4(:,1) = Q(1:4,21)
  momenta_5(:,1) = Q(1:4,22)
  momenta_6(:,1) = Q(1:4,24)
  momenta_7(:,1) = Q(1:4,25)
  momenta_8(:,1) = Q(1:4,26)
  momenta_9(:,1) = Q(1:4,4)
  momenta_9(:,2) = Q(1:4,29)
  momenta_10(:,1) = Q(1:4,4)
  momenta_10(:,2) = Q(1:4,30)
  momenta_11(:,1) = Q(1:4,5)
  momenta_11(:,2) = Q(1:4,29)
  momenta_12(:,1) = Q(1:4,6)
  momenta_12(:,2) = Q(1:4,30)
  momenta_13(:,1) = Q(1:4,8)
  momenta_13(:,2) = Q(1:4,24)
  momenta_14(:,1) = Q(1:4,8)
  momenta_14(:,2) = Q(1:4,25)
  momenta_15(:,1) = Q(1:4,8)
  momenta_15(:,2) = Q(1:4,26)
  momenta_16(:,1) = Q(1:4,8)
  momenta_16(:,2) = Q(1:4,29)
  momenta_17(:,1) = Q(1:4,8)
  momenta_17(:,2) = Q(1:4,30)
  momenta_18(:,1) = Q(1:4,9)
  momenta_18(:,2) = Q(1:4,25)
  momenta_19(:,1) = Q(1:4,9)
  momenta_19(:,2) = Q(1:4,27)
  momenta_20(:,1) = Q(1:4,9)
  momenta_20(:,2) = Q(1:4,29)
  momenta_21(:,1) = Q(1:4,10)
  momenta_21(:,2) = Q(1:4,26)
  momenta_22(:,1) = Q(1:4,10)
  momenta_22(:,2) = Q(1:4,27)
  momenta_23(:,1) = Q(1:4,10)
  momenta_23(:,2) = Q(1:4,30)
  momenta_24(:,1) = Q(1:4,11)
  momenta_24(:,2) = Q(1:4,27)
  momenta_25(:,1) = Q(1:4,13)
  momenta_25(:,2) = Q(1:4,29)
  momenta_26(:,1) = Q(1:4,14)
  momenta_26(:,2) = Q(1:4,30)
  momenta_27(:,1) = Q(1:4,17)
  momenta_27(:,2) = Q(1:4,23)
  momenta_28(:,1) = Q(1:4,17)
  momenta_28(:,2) = Q(1:4,27)
  momenta_29(:,1) = Q(1:4,18)
  momenta_29(:,2) = Q(1:4,23)
  momenta_30(:,1) = Q(1:4,18)
  momenta_30(:,2) = Q(1:4,27)
  momenta_31(:,1) = Q(1:4,20)
  momenta_31(:,2) = Q(1:4,29)
  momenta_32(:,1) = Q(1:4,20)
  momenta_32(:,2) = Q(1:4,30)
  momenta_33(:,1) = Q(1:4,21)
  momenta_33(:,2) = Q(1:4,29)
  momenta_34(:,1) = Q(1:4,22)
  momenta_34(:,2) = Q(1:4,30)
  momenta_35(:,1) = Q(1:4,24)
  momenta_35(:,2) = Q(1:4,29)
  momenta_36(:,1) = Q(1:4,24)
  momenta_36(:,2) = Q(1:4,30)
  momenta_37(:,1) = Q(1:4,25)
  momenta_37(:,2) = Q(1:4,29)
  momenta_38(:,1) = Q(1:4,26)
  momenta_38(:,2) = Q(1:4,30)
  momenta_39(:,1) = Q(1:4,2)
  momenta_39(:,2) = Q(1:4,6)
  momenta_39(:,3) = Q(1:4,30)
  momenta_40(:,1) = Q(1:4,2)
  momenta_40(:,2) = Q(1:4,10)
  momenta_40(:,3) = Q(1:4,30)
  momenta_41(:,1) = Q(1:4,2)
  momenta_41(:,2) = Q(1:4,22)
  momenta_41(:,3) = Q(1:4,30)
  momenta_42(:,1) = Q(1:4,2)
  momenta_42(:,2) = Q(1:4,26)
  momenta_42(:,3) = Q(1:4,30)
  momenta_43(:,1) = Q(1:4,4)
  momenta_43(:,2) = Q(1:4,6)
  momenta_43(:,3) = Q(1:4,30)
  momenta_44(:,1) = Q(1:4,4)
  momenta_44(:,2) = Q(1:4,13)
  momenta_44(:,3) = Q(1:4,29)
  momenta_45(:,1) = Q(1:4,4)
  momenta_45(:,2) = Q(1:4,14)
  momenta_45(:,3) = Q(1:4,30)
  momenta_46(:,1) = Q(1:4,4)
  momenta_46(:,2) = Q(1:4,20)
  momenta_46(:,3) = Q(1:4,29)
  momenta_47(:,1) = Q(1:4,4)
  momenta_47(:,2) = Q(1:4,20)
  momenta_47(:,3) = Q(1:4,30)
  momenta_48(:,1) = Q(1:4,4)
  momenta_48(:,2) = Q(1:4,21)
  momenta_48(:,3) = Q(1:4,29)
  momenta_49(:,1) = Q(1:4,4)
  momenta_49(:,2) = Q(1:4,22)
  momenta_49(:,3) = Q(1:4,30)
  momenta_50(:,1) = Q(1:4,5)
  momenta_50(:,2) = Q(1:4,13)
  momenta_50(:,3) = Q(1:4,29)
  momenta_51(:,1) = Q(1:4,6)
  momenta_51(:,2) = Q(1:4,14)
  momenta_51(:,3) = Q(1:4,30)
  momenta_52(:,1) = Q(1:4,8)
  momenta_52(:,2) = Q(1:4,10)
  momenta_52(:,3) = Q(1:4,30)
  momenta_53(:,1) = Q(1:4,8)
  momenta_53(:,2) = Q(1:4,13)
  momenta_53(:,3) = Q(1:4,29)
  momenta_54(:,1) = Q(1:4,8)
  momenta_54(:,2) = Q(1:4,14)
  momenta_54(:,3) = Q(1:4,30)
  momenta_55(:,1) = Q(1:4,8)
  momenta_55(:,2) = Q(1:4,24)
  momenta_55(:,3) = Q(1:4,29)
  momenta_56(:,1) = Q(1:4,8)
  momenta_56(:,2) = Q(1:4,24)
  momenta_56(:,3) = Q(1:4,30)
  momenta_57(:,1) = Q(1:4,8)
  momenta_57(:,2) = Q(1:4,25)
  momenta_57(:,3) = Q(1:4,29)
  momenta_58(:,1) = Q(1:4,8)
  momenta_58(:,2) = Q(1:4,26)
  momenta_58(:,3) = Q(1:4,30)
  momenta_59(:,1) = Q(1:4,9)
  momenta_59(:,2) = Q(1:4,13)
  momenta_59(:,3) = Q(1:4,29)
  momenta_60(:,1) = Q(1:4,10)
  momenta_60(:,2) = Q(1:4,14)
  momenta_60(:,3) = Q(1:4,30)
  momenta_61(:,1) = Q(1:4,17)
  momenta_61(:,2) = Q(1:4,21)
  momenta_61(:,3) = Q(1:4,29)
  momenta_62(:,1) = Q(1:4,17)
  momenta_62(:,2) = Q(1:4,25)
  momenta_62(:,3) = Q(1:4,29)
  momenta_63(:,1) = Q(1:4,18)
  momenta_63(:,2) = Q(1:4,22)
  momenta_63(:,3) = Q(1:4,30)
  momenta_64(:,1) = Q(1:4,18)
  momenta_64(:,2) = Q(1:4,26)
  momenta_64(:,3) = Q(1:4,30)
  momenta_65(:,1) = Q(1:4,20)
  momenta_65(:,2) = Q(1:4,22)
  momenta_65(:,3) = Q(1:4,30)
  momenta_66(:,1) = Q(1:4,24)
  momenta_66(:,2) = Q(1:4,26)
  momenta_66(:,3) = Q(1:4,30)
  momenta_67(:,1) = Q(1:4,2)
  momenta_67(:,2) = Q(1:4,6)
  momenta_67(:,3) = Q(1:4,14)
  momenta_67(:,4) = Q(1:4,30)
  momenta_68(:,1) = Q(1:4,2)
  momenta_68(:,2) = Q(1:4,10)
  momenta_68(:,3) = Q(1:4,14)
  momenta_68(:,4) = Q(1:4,30)
  momenta_69(:,1) = Q(1:4,2)
  momenta_69(:,2) = Q(1:4,18)
  momenta_69(:,3) = Q(1:4,22)
  momenta_69(:,4) = Q(1:4,30)
  momenta_70(:,1) = Q(1:4,2)
  momenta_70(:,2) = Q(1:4,18)
  momenta_70(:,3) = Q(1:4,26)
  momenta_70(:,4) = Q(1:4,30)
  momenta_71(:,1) = Q(1:4,4)
  momenta_71(:,2) = Q(1:4,6)
  momenta_71(:,3) = Q(1:4,14)
  momenta_71(:,4) = Q(1:4,30)
  momenta_72(:,1) = Q(1:4,4)
  momenta_72(:,2) = Q(1:4,20)
  momenta_72(:,3) = Q(1:4,22)
  momenta_72(:,4) = Q(1:4,30)
  momenta_73(:,1) = Q(1:4,8)
  momenta_73(:,2) = Q(1:4,10)
  momenta_73(:,3) = Q(1:4,14)
  momenta_73(:,4) = Q(1:4,30)
  momenta_74(:,1) = Q(1:4,8)
  momenta_74(:,2) = Q(1:4,24)
  momenta_74(:,3) = Q(1:4,26)
  momenta_74(:,4) = Q(1:4,30)

  masses2_1 = [ MB2, MB2 ]
  masses2_2 = [ ZERO2, MB2 ]
  masses2_3 = [ MT2, MT2 ]
  masses2_4 = [ ZERO2, ZERO2 ]
  masses2_5 = [ MB2, MB2, MB2 ]
  masses2_6 = [ ZERO2, MB2, MB2 ]
  masses2_7 = [ ZERO2, MB2, ZERO2 ]
  masses2_8 = [ MT2, MT2, MT2 ]
  masses2_9 = [ ZERO2, ZERO2, MB2 ]
  masses2_10 = [ ZERO2, MB2, MB2, MB2 ]
  masses2_11 = [ ZERO2, MB2, ZERO2, MB2 ]
  masses2_12 = [ ZERO2, ZERO2, MB2, MB2 ]
  masses2_13 = [ ZERO2, MB2, MB2, ZERO2, MB2 ]
  masses2_14 = [ ZERO2, MB2, ZERO2, MB2, MB2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call TI_call(1, momenta_2, masses2_2, T1sum(:,1), M2)
  call TI_call(1, momenta_6, masses2_2, T1sum(:,2), M2)
  call TI_call(1, momenta_1, masses2_2, T1sum(:,3), M2)
  call TI_call(1, momenta_3, masses2_2, T1sum(:,4), M2)

  call TI_call(2, momenta_29, masses2_6, T2sum(:,1), M2)
  call TI_call(2, momenta_15, masses2_7, T2sum(:,2), M2)
  call TI_call(2, momenta_35, masses2_6, T2sum(:,3), M2)
  call TI_call(2, momenta_11, masses2_9, T2sum(:,4), M2)
  call TI_call(2, momenta_13, masses2_6, T2sum(:,5), M2)
  call TI_call(2, momenta_25, masses2_6, T2sum(:,6), M2)
  call TI_call(2, momenta_27, masses2_6, T2sum(:,7), M2)
  call TI_call(2, momenta_14, masses2_7, T2sum(:,8), M2)
  call TI_call(2, momenta_36, masses2_6, T2sum(:,9), M2)
  call TI_call(2, momenta_12, masses2_9, T2sum(:,10), M2)
  call TI_call(2, momenta_26, masses2_6, T2sum(:,11), M2)
  call TI_call(2, momenta_30, masses2_6, T2sum(:,12), M2)
  call TI_call(2, momenta_19, masses2_9, T2sum(:,13), M2)
  call TI_call(2, momenta_31, masses2_6, T2sum(:,14), M2)
  call TI_call(2, momenta_20, masses2_9, T2sum(:,15), M2)
  call TI_call(2, momenta_24, masses2_6, T2sum(:,16), M2)
  call TI_call(2, momenta_28, masses2_6, T2sum(:,17), M2)
  call TI_call(2, momenta_22, masses2_9, T2sum(:,18), M2)
  call TI_call(2, momenta_32, masses2_6, T2sum(:,19), M2)
  call TI_call(2, momenta_23, masses2_9, T2sum(:,20), M2)
  call TI_call(2, momenta_16, masses2_6, T2sum(:,21), M2)
  call TI_call(2, momenta_33, masses2_9, T2sum(:,22), M2)
  call TI_call(2, momenta_9, masses2_6, T2sum(:,23), M2)
  call TI_call(2, momenta_37, masses2_9, T2sum(:,24), M2)
  call TI_call(2, momenta_57, masses2_11, T2sum(:,25), M2)
  call TI_call(2, momenta_48, masses2_11, T2sum(:,26), M2)
  call TI_call(2, momenta_62, masses2_11, T2sum(:,27), M2)
  call TI_call(2, momenta_61, masses2_11, T2sum(:,28), M2)
  call TI_call(2, momenta_17, masses2_6, T2sum(:,29), M2)
  call TI_call(2, momenta_34, masses2_9, T2sum(:,30), M2)
  call TI_call(2, momenta_10, masses2_6, T2sum(:,31), M2)
  call TI_call(2, momenta_38, masses2_9, T2sum(:,32), M2)
  call TI_call(2, momenta_58, masses2_11, T2sum(:,33), M2)
  call TI_call(2, momenta_49, masses2_11, T2sum(:,34), M2)
  call TI_call(2, momenta_64, masses2_11, T2sum(:,35), M2)
  call TI_call(2, momenta_63, masses2_11, T2sum(:,36), M2)
  call TI_call(2, momenta_41, masses2_11, T2sum(:,37), M2)
  call TI_call(2, momenta_40, masses2_11, T2sum(:,38), M2)
  call TI_call(2, momenta_65, masses2_11, T2sum(:,39), M2)
  call TI_call(2, momenta_52, masses2_11, T2sum(:,40), M2)
  call TI_call(2, momenta_42, masses2_11, T2sum(:,41), M2)
  call TI_call(2, momenta_39, masses2_11, T2sum(:,42), M2)
  call TI_call(2, momenta_66, masses2_11, T2sum(:,43), M2)
  call TI_call(2, momenta_43, masses2_11, T2sum(:,44), M2)
  call TI_call(2, momenta_8, masses2_4, T2sum(:,45), M2)
  call TI_call(2, momenta_8, masses2_3, T2sum(:,46), M2)
  call TI_call(2, momenta_8, masses2_1, T2sum(:,47), M2)
  call TI_call(2, momenta_7, masses2_4, T2sum(:,48), M2)
  call TI_call(2, momenta_7, masses2_3, T2sum(:,49), M2)
  call TI_call(2, momenta_7, masses2_1, T2sum(:,50), M2)
  call TI_call(2, momenta_5, masses2_4, T2sum(:,51), M2)
  call TI_call(2, momenta_5, masses2_3, T2sum(:,52), M2)
  call TI_call(2, momenta_5, masses2_1, T2sum(:,53), M2)
  call TI_call(2, momenta_4, masses2_4, T2sum(:,54), M2)
  call TI_call(2, momenta_4, masses2_3, T2sum(:,55), M2)
  call TI_call(2, momenta_4, masses2_1, T2sum(:,56), M2)

  call TI_call(3, momenta_21, masses2_8, T3sum(:,1), M2)
  call TI_call(3, momenta_21, masses2_5, T3sum(:,2), M2)
  call TI_call(3, momenta_53, masses2_10, T3sum(:,3), M2)
  call TI_call(3, momenta_55, masses2_10, T3sum(:,4), M2)
  call TI_call(3, momenta_50, masses2_12, T3sum(:,5), M2)
  call TI_call(3, momenta_18, masses2_8, T3sum(:,6), M2)
  call TI_call(3, momenta_18, masses2_5, T3sum(:,7), M2)
  call TI_call(3, momenta_54, masses2_10, T3sum(:,8), M2)
  call TI_call(3, momenta_56, masses2_10, T3sum(:,9), M2)
  call TI_call(3, momenta_51, masses2_12, T3sum(:,10), M2)
  call TI_call(3, momenta_44, masses2_10, T3sum(:,11), M2)
  call TI_call(3, momenta_46, masses2_10, T3sum(:,12), M2)
  call TI_call(3, momenta_59, masses2_12, T3sum(:,13), M2)
  call TI_call(3, momenta_45, masses2_10, T3sum(:,14), M2)
  call TI_call(3, momenta_47, masses2_10, T3sum(:,15), M2)
  call TI_call(3, momenta_60, masses2_12, T3sum(:,16), M2)
  call TI_call(3, momenta_70, masses2_13, T3sum(:,17), M2)
  call TI_call(3, momenta_69, masses2_13, T3sum(:,18), M2)
  call TI_call(3, momenta_67, masses2_14, T3sum(:,19), M2)
  call TI_call(3, momenta_68, masses2_14, T3sum(:,20), M2)
  call TI_call(3, momenta_74, masses2_13, T3sum(:,21), M2)
  call TI_call(3, momenta_72, masses2_13, T3sum(:,22), M2)
  call TI_call(3, momenta_71, masses2_14, T3sum(:,23), M2)
  call TI_call(3, momenta_73, masses2_14, T3sum(:,24), M2)

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_pphbb_bbbxbxh_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_pphbb_bbbxbxh_1_/**/REALKIND
