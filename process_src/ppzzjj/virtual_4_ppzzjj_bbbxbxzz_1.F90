
module ol_vamp_4_ppzzjj_bbbxbxzz_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_4(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppzzjj_bbbxbxzz_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppzzjj_bbbxbxzz_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppzzjj_bbbxbxzz_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppzzjj_bbbxbxzz_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,7) :: G0
  complex(REALKIND), dimension(4,5,4,20) :: G1
  complex(REALKIND), dimension(15,16) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,70),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,37),ZERO,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,67),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),ZERO,G2tensor(:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,74),G1(:,:,:,3))
  call check_last_Q_A(l_switch,G1(:,:,:,3),Q(:,63),ZERO,G2tensor(:,2))
  call loop_QV_A(G0(:,:,:,1),wf(:,70),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,37),MT,G1(:,:,:,4))
  call loop_QV_A(G1(:,:,:,4),wf(:,67),G1(:,:,:,5))
  call check_last_Q_A(l_switch,G1(:,:,:,5),Q(:,63),MT,G2tensor(:,3))
  call loop_QV_A(G1(:,:,:,4),wf(:,74),G1(:,:,:,6))
  call check_last_Q_A(l_switch,G1(:,:,:,6),Q(:,63),MT,G2tensor(:,4))
  call loop_QV_A(G0(:,:,:,1),wf(:,70),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,37),MB,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,67),G1(:,:,:,8))
  call check_last_Q_A(l_switch,G1(:,:,:,8),Q(:,63),MB,G2tensor(:,5))
  call loop_QV_A(G1(:,:,:,7),wf(:,74),G1(:,:,:,9))
  call check_last_Q_A(l_switch,G1(:,:,:,9),Q(:,63),MB,G2tensor(:,6))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,70),Q(:,37),G1(:,:,:,10))
  call check_last_CV_D(l_switch,G1(:,:,:,10),Q(:,37),wf(:,67),Q(:,26),G2tensor(:,7))
  call check_last_CV_D(l_switch,G1(:,:,:,10),Q(:,37),wf(:,74),Q(:,26),G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,71),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,41),ZERO,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,68),G1(:,:,:,12))
  call check_last_Q_A(l_switch,G1(:,:,:,12),Q(:,63),ZERO,G2tensor(:,9))
  call loop_QV_A(G1(:,:,:,11),wf(:,72),G1(:,:,:,13))
  call check_last_Q_A(l_switch,G1(:,:,:,13),Q(:,63),ZERO,G2tensor(:,10))
  call loop_QV_A(G0(:,:,:,1),wf(:,71),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,41),MT,G1(:,:,:,14))
  call loop_QV_A(G1(:,:,:,14),wf(:,68),G1(:,:,:,15))
  call check_last_Q_A(l_switch,G1(:,:,:,15),Q(:,63),MT,G2tensor(:,11))
  call loop_QV_A(G1(:,:,:,14),wf(:,72),G1(:,:,:,16))
  call check_last_Q_A(l_switch,G1(:,:,:,16),Q(:,63),MT,G2tensor(:,12))
  call loop_QV_A(G0(:,:,:,1),wf(:,71),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,41),MB,G1(:,:,:,17))
  call loop_QV_A(G1(:,:,:,17),wf(:,68),G1(:,:,:,18))
  call check_last_Q_A(l_switch,G1(:,:,:,18),Q(:,63),MB,G2tensor(:,13))
  call loop_QV_A(G1(:,:,:,17),wf(:,72),G1(:,:,:,19))
  call check_last_Q_A(l_switch,G1(:,:,:,19),Q(:,63),MB,G2tensor(:,14))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,71),Q(:,41),G1(:,:,:,20))
  call check_last_CV_D(l_switch,G1(:,:,:,20),Q(:,41),wf(:,68),Q(:,22),G2tensor(:,15))
  call check_last_CV_D(l_switch,G1(:,:,:,20),Q(:,41),wf(:,72),Q(:,22),G2tensor(:,16))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(30)*M(1)-c(29)*M(2)) * den(353)
  T2sum(1:15,175) = T2sum(1:15,175) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(28)*M(1)-c(27)*M(2)) * den(353)
  T2sum(1:15,176) = T2sum(1:15,176) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(30)*M(1)-c(29)*M(2)) * den(353)
  T2sum(1:15,175) = T2sum(1:15,175) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(28)*M(1)-c(27)*M(2)) * den(353)
  T2sum(1:15,177) = T2sum(1:15,177) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(13)*M(1)-c(10)*M(2)) * den(353)
  T2sum(1:15,175) = T2sum(1:15,175) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(29)*M(1)-c(30)*M(2)) * den(355)
  T2sum(1:15,178) = T2sum(1:15,178) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(27)*M(1)-c(28)*M(2)) * den(355)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(29)*M(1)-c(30)*M(2)) * den(355)
  T2sum(1:15,178) = T2sum(1:15,178) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(27)*M(1)-c(28)*M(2)) * den(355)
  T2sum(1:15,180) = T2sum(1:15,180) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(10)*M(1)-c(13)*M(2)) * den(355)
  T2sum(1:15,178) = T2sum(1:15,178) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(29)*M(1)-c(30)*M(2)) * den(365)
  T2sum(1:15,178) = T2sum(1:15,178) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(27)*M(1)-c(28)*M(2)) * den(365)
  T2sum(1:15,179) = T2sum(1:15,179) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(29)*M(1)-c(30)*M(2)) * den(365)
  T2sum(1:15,178) = T2sum(1:15,178) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(27)*M(1)-c(28)*M(2)) * den(365)
  T2sum(1:15,180) = T2sum(1:15,180) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(10)*M(1)-c(13)*M(2)) * den(365)
  T2sum(1:15,178) = T2sum(1:15,178) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(30)*M(1)-c(29)*M(2)) * den(376)
  T2sum(1:15,175) = T2sum(1:15,175) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(28)*M(1)-c(27)*M(2)) * den(376)
  T2sum(1:15,176) = T2sum(1:15,176) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(30)*M(1)-c(29)*M(2)) * den(376)
  T2sum(1:15,175) = T2sum(1:15,175) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(28)*M(1)-c(27)*M(2)) * den(376)
  T2sum(1:15,177) = T2sum(1:15,177) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(13)*M(1)-c(10)*M(2)) * den(376)
  T2sum(1:15,175) = T2sum(1:15,175) + Gcoeff * G2tensor(:,8)

end subroutine vamp_4

end module ol_vamp_4_ppzzjj_bbbxbxzz_1_/**/REALKIND
