
module ol_vamp_14_pphbbj_bbxhggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_14(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphbbj_bbxhggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphbbj_bbxhggg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind2
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tables_storage_pphbbj_bbxhggg_1_/**/DREALKIND
  use ol_tensor_sum_storage_pphbbj_bbxhggg_1_/**/REALKIND
! no reduction 

  use ol_loop_storage_pphbbj_bbxhggg_1_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_storage_pphbbj_bbxhggg_1_/**/DREALKIND, only: ntry, p_switch, Hel
#endif
  use hol_initialisation_/**/REALKIND, only: G0_hol_initialisation
  use ol_h_vert_interface_/**/REALKIND
  use ol_h_prop_interface_/**/REALKIND
  use ol_h_last_step_/**/REALKIND
  use ol_merging_/**/REALKIND, only: ol_merge

  implicit none

  type(Hpolcont) :: Gcoeff(32)
  type(Hpolcont), intent(in) :: M(11,32)


#ifndef PRECISION_dp
  if (ntry==1 .OR. p_switch == 1) Gcoeff(:)%hf = Hel
#else
  if (ntry==1 .OR. p_switch == 2) Gcoeff(:)%hf = Hel
#endif

  ! =============================================
  call Hloop_QV_A(ntry,G1H8(18),wf8(:,185),G1H1(1),m3h8x1(:,231),heltab2x8(:,:,898))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,70),n2h1(749))
  call Hloop_QV_A(ntry,G1H8(41),wf8(:,185),G1H1(3),m3h8x1(:,232),heltab2x8(:,:,899))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,79),n2h1(750))
  call Hloop_QV_A(ntry,G1H8(80),wf8(:,185),G1H1(2),m3h8x1(:,233),heltab2x8(:,:,900))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,73),n2h1(751))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(22),Q(:,40),wf8(:,185),Q(:,23),G2tensor(:,74),m3h8x1(:,234),heltab2x8(:,:,901))
  call Hloop_QV_A(ntry,G1H8(40),wf8(:,192),G1H1(1),m3h8x1(:,235),heltab2x8(:,:,902))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,92),n2h1(752))
  call Hloop_QV_A(ntry,G1H8(82),wf8(:,192),G1H1(3),m3h8x1(:,236),heltab2x8(:,:,903))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,104),n2h1(753))
  call Hloop_QV_A(ntry,G1H8(44),wf8(:,192),G1H1(2),m3h8x1(:,237),heltab2x8(:,:,904))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,106),n2h1(754))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(48),Q(:,40),wf8(:,192),Q(:,23),G2tensor(:,124),m3h8x1(:,238),heltab2x8(:,:,905))
  call Hloop_QV_A(ntry,G1H8(50),wf8(:,184),G1H1(1),m3h8x1(:,239),heltab2x8(:,:,906))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,126),n2h1(755))
  call Hloop_QV_A(ntry,G1H8(108),wf8(:,184),G1H1(3),m3h8x1(:,240),heltab2x8(:,:,907))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,140),n2h1(756))
  call Hloop_QV_A(ntry,G1H8(99),wf8(:,184),G1H1(2),m3h8x1(:,241),heltab2x8(:,:,908))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,142),n2h1(757))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(100),Q(:,48),wf8(:,184),Q(:,15),G2tensor(:,93),m3h8x1(:,242),heltab2x8(:,:,909))
  call Hloop_QV_A(ntry,G1H8(103),wf8(:,187),G1H1(1),m3h8x1(:,243),heltab2x8(:,:,910))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,105),n2h1(758))
  call Hloop_QV_A(ntry,G1H8(95),wf8(:,187),G1H1(3),m3h8x1(:,244),heltab2x8(:,:,911))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,107),n2h1(759))
  call Hloop_QV_A(ntry,G1H8(96),wf8(:,187),G1H1(2),m3h8x1(:,245),heltab2x8(:,:,912))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,125),n2h1(760))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(104),Q(:,48),wf8(:,187),Q(:,15),G2tensor(:,127),m3h8x1(:,246),heltab2x8(:,:,913))
  call Hloop_VQ_A(ntry,G0H4(53),wf4(:,13),G0H1(1),m3h4x1(:,216),heltab2x4(:,:,618))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,85),n2h1(761))
  call Hloop_QV_A(ntry,G1H8(106),wf8(:,25),G1H1(1),m3h8x1(:,247),heltab2x8(:,:,914))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,141),n2h1(762))
  call Hloop_QV_A(ntry,G1H8(20),wf8(:,25),G1H1(3),m3h8x1(:,248),heltab2x8(:,:,915))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,143),n2h1(763))
  call Hloop_QV_A(ntry,G1H8(30),wf8(:,25),G1H1(2),m3h8x1(:,249),heltab2x8(:,:,916))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,96),n2h1(764))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(83),Q(:,48),wf8(:,25),Q(:,15),G2tensor(:,97),m3h8x1(:,250),heltab2x8(:,:,917))
  call Hcheck_last_QA_V(ntry,l_switch,G1H2(29),wf2(:,4),G1tensor(:,86),m3h2x1(:,483),heltab2x2(:,:,557))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H4(42),wf4(:,18),G1tensor(:,87),m3h4x1(:,217),heltab2x4(:,:,619))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(110),wf8(:,27),G1tensor(:,88),m3h8x1(:,251),heltab2x8(:,:,918))
  call Hcheck_last_QA_V(ntry,l_switch,G1H4(169),wf4(:,21),G1tensor(:,89),m3h4x1(:,218),heltab2x4(:,:,620))
  call Hloop_QV_A(ntry,G1H8(27),wf8(:,29),G1H1(1),m3h8x1(:,252),heltab2x8(:,:,919))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,98),n2h1(765))
  call Hloop_QV_A(ntry,G1H8(111),wf8(:,29),G1H1(3),m3h8x1(:,253),heltab2x8(:,:,920))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,101),n2h1(766))
  call Hloop_QV_A(ntry,G1H8(112),wf8(:,29),G1H1(2),m3h8x1(:,254),heltab2x8(:,:,921))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,112),n2h1(767))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(47),Q(:,40),wf8(:,29),Q(:,23),G2tensor(:,130),m3h8x1(:,255),heltab2x8(:,:,922))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H4(89),wf4(:,23),G1tensor(:,90),m3h4x1(:,219),heltab2x4(:,:,621))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(84),wf8(:,31),G1tensor(:,91),m3h8x1(:,256),heltab2x8(:,:,923))
  call Hcheck_last_QA_V(ntry,l_switch,G1H4(132),wf4(:,25),G1tensor(:,92),m3h4x1(:,220),heltab2x4(:,:,622))
  call Hloop_VA_Q(ntry,G0H8(88),wf8(:,174),G0H1(1),m3h8x1(:,257),heltab2x8(:,:,924))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,93),n2h1(768))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(78),wf8(:,35),G1tensor(:,94),m3h8x1(:,258),heltab2x8(:,:,925))
  call Hloop_VA_Q(ntry,G0H4(48),wf4(:,21),G0H1(1),m3h4x1(:,221),heltab2x4(:,:,623))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,95),n2h1(769))
  call Hloop_VA_Q(ntry,G0H8(89),wf8(:,209),G0H1(1),m3h8x1(:,259),heltab2x8(:,:,926))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,96),n2h1(770))
  call Hloop_VA_Q(ntry,G0H8(90),wf8(:,213),G0H1(1),m3h8x1(:,260),heltab2x8(:,:,927))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,97),n2h1(771))
  call Hloop_QV_A(ntry,G1H8(86),wf8(:,203),G1H1(1),m3h8x1(:,261),heltab2x8(:,:,928))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,122),n2h1(772))
  call Hloop_QV_A(ntry,G1H8(115),wf8(:,203),G1H1(3),m3h8x1(:,262),heltab2x8(:,:,929))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,138),n2h1(773))
  call Hloop_QV_A(ntry,G1H8(88),wf8(:,203),G1H1(2),m3h8x1(:,263),heltab2x8(:,:,930))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,1),n2h1(774))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(117),Q(:,40),wf8(:,203),Q(:,23),G2tensor(:,2),m3h8x1(:,264),heltab2x8(:,:,931))
  call Hloop_QV_A(ntry,G1H8(3),wf8(:,206),G1H1(1),m3h8x1(:,265),heltab2x8(:,:,932))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,22),n2h1(775))
  call Hloop_QV_A(ntry,G1H8(120),wf8(:,206),G1H1(3),m3h8x1(:,266),heltab2x8(:,:,933))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,7),n2h1(776))
  call Hloop_QV_A(ntry,G1H8(121),wf8(:,206),G1H1(2),m3h8x1(:,267),heltab2x8(:,:,934))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,24),n2h1(777))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(53),Q(:,40),wf8(:,206),Q(:,23),G2tensor(:,47),m3h8x1(:,268),heltab2x8(:,:,935))
  call Hloop_QV_A(ntry,G1H8(37),wf8(:,202),G1H1(1),m3h8x1(:,269),heltab2x8(:,:,936))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,49),n2h1(778))
  call Hloop_QV_A(ntry,G1H8(123),wf8(:,202),G1H1(3),m3h8x1(:,270),heltab2x8(:,:,937))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,12),n2h1(779))
  call Hloop_QV_A(ntry,G1H8(124),wf8(:,202),G1H1(2),m3h8x1(:,271),heltab2x8(:,:,938))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,13),n2h1(780))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(67),Q(:,48),wf8(:,202),Q(:,15),G2tensor(:,14),m3h8x1(:,272),heltab2x8(:,:,939))
  call Hloop_QV_A(ntry,G1H8(63),wf8(:,205),G1H1(1),m3h8x1(:,273),heltab2x8(:,:,940))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,23),n2h1(781))
  call Hloop_QV_A(ntry,G1H8(66),wf8(:,205),G1H1(3),m3h8x1(:,274),heltab2x8(:,:,941))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,19),n2h1(782))
  call Hloop_QV_A(ntry,G1H8(65),wf8(:,205),G1H1(2),m3h8x1(:,275),heltab2x8(:,:,942))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,25),n2h1(783))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(46),Q(:,48),wf8(:,205),Q(:,15),G2tensor(:,87),m3h8x1(:,276),heltab2x8(:,:,943))
  call Hloop_VA_Q(ntry,G0H4(69),wf4(:,25),G0H1(1),m3h4x1(:,222),heltab2x4(:,:,624))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,98),n2h1(784))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H4(123),wf4(:,29),G1tensor(:,99),m3h4x1(:,223),heltab2x4(:,:,625))
  call Hcheck_last_QA_V(ntry,l_switch,G1H4(60),wf4(:,31),G1tensor(:,100),m3h4x1(:,224),heltab2x4(:,:,626))
  call Hloop_VQ_A(ntry,G0H8(91),wf8(:,222),G0H1(1),m3h8x1(:,277),heltab2x8(:,:,944))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,101),n2h1(785))
  call Hloop_VQ_A(ntry,G0H8(92),wf8(:,27),G0H1(1),m3h8x1(:,278),heltab2x8(:,:,945))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,102),n2h1(786))
  call Hloop_VQ_A(ntry,G0H8(93),wf8(:,226),G0H1(1),m3h8x1(:,279),heltab2x8(:,:,946))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,103),n2h1(787))
  call Hloop_QV_A(ntry,G1H8(54),wf8(:,220),G1H1(1),m3h8x1(:,280),heltab2x8(:,:,947))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,91),n2h1(788))
  call Hloop_QV_A(ntry,G1H8(128),wf8(:,220),G1H1(3),m3h8x1(:,281),heltab2x8(:,:,948))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,21),n2h1(789))
  call Hloop_QV_A(ntry,G1H8(129),wf8(:,220),G1H1(2),m3h8x1(:,282),heltab2x8(:,:,949))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,26),n2h1(790))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(16),Q(:,48),wf8(:,220),Q(:,15),G2tensor(:,27),m3h8x1(:,283),heltab2x8(:,:,950))
  call Hloop_VQ_A(ntry,G0H4(60),wf4(:,29),G0H1(1),m3h4x1(:,225),heltab2x4(:,:,627))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,104),n2h1(791))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H4(48),wf4(:,35),G1tensor(:,105),m3h4x1(:,226),heltab2x4(:,:,628))
  call Hcheck_last_QA_V(ntry,l_switch,G1H4(65),wf4(:,37),G1tensor(:,106),m3h4x1(:,227),heltab2x4(:,:,629))
  call Hloop_VA_Q(ntry,G0H8(94),wf8(:,7),G0H1(1),m3h8x1(:,284),heltab2x8(:,:,951))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,107),n2h1(792))
  call Hloop_VA_Q(ntry,G0H8(95),wf8(:,231),G0H1(1),m3h8x1(:,285),heltab2x8(:,:,952))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,108),n2h1(793))
  call Hloop_VA_Q(ntry,G0H8(96),wf8(:,235),G0H1(1),m3h8x1(:,286),heltab2x8(:,:,953))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,109),n2h1(794))
  call Hloop_QV_A(ntry,G1H8(68),wf8(:,229),G1H1(1),m3h8x1(:,287),heltab2x8(:,:,954))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,28),n2h1(795))
  call Hloop_QV_A(ntry,G1H8(131),wf8(:,229),G1H1(3),m3h8x1(:,288),heltab2x8(:,:,955))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,118),n2h1(796))
  call Hloop_QV_A(ntry,G1H8(132),wf8(:,229),G1H1(2),m3h8x1(:,289),heltab2x8(:,:,956))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,119),n2h1(797))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(43),Q(:,48),wf8(:,229),Q(:,15),G2tensor(:,31),m3h8x1(:,290),heltab2x8(:,:,957))
  call Hloop_VA_Q(ntry,G0H4(27),wf4(:,37),G0H1(1),m3h4x1(:,228),heltab2x4(:,:,630))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,110),n2h1(798))
  call Hloop_VQ_A(ntry,G0H8(97),wf8(:,240),G0H1(1),m3h8x1(:,291),heltab2x8(:,:,958))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,111),n2h1(799))
  call Hloop_VQ_A(ntry,G0H8(98),wf8(:,31),G0H1(1),m3h8x1(:,292),heltab2x8(:,:,959))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,112),n2h1(800))
  call Hloop_VQ_A(ntry,G0H8(99),wf8(:,244),G0H1(1),m3h8x1(:,293),heltab2x8(:,:,960))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,113),n2h1(801))
  call Hloop_QV_A(ntry,G1H8(92),wf8(:,238),G1H1(1),m3h8x1(:,294),heltab2x8(:,:,961))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,32),n2h1(802))
  call Hloop_QV_A(ntry,G1H8(23),wf8(:,238),G1H1(3),m3h8x1(:,295),heltab2x8(:,:,962))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,50),n2h1(803))
  call Hloop_QV_A(ntry,G1H8(9),wf8(:,238),G1H1(2),m3h8x1(:,296),heltab2x8(:,:,963))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,51),n2h1(804))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(42),Q(:,40),wf8(:,238),Q(:,23),G2tensor(:,35),m3h8x1(:,297),heltab2x8(:,:,964))
  call Hloop_VQ_A(ntry,G0H4(26),wf4(:,35),G0H1(1),m3h4x1(:,229),heltab2x4(:,:,631))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,114),n2h1(805))
  call Hloop_VA_Q(ntry,G0H8(100),wf8(:,11),G0H1(1),m3h8x1(:,298),heltab2x8(:,:,965))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,115),n2h1(806))
  call Hloop_VA_Q(ntry,G0H8(101),wf8(:,249),G0H1(1),m3h8x1(:,299),heltab2x8(:,:,966))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,116),n2h1(807))
  call Hloop_VA_Q(ntry,G0H8(102),wf8(:,253),G0H1(1),m3h8x1(:,300),heltab2x8(:,:,967))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,117),n2h1(808))
  call Hloop_QV_A(ntry,G1H8(136),wf8(:,247),G1H1(1),m3h8x1(:,301),heltab2x8(:,:,968))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,36),n2h1(809))
  call Hloop_QV_A(ntry,G1H8(35),wf8(:,247),G1H1(3),m3h8x1(:,302),heltab2x8(:,:,969))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,37),n2h1(810))
  call Hloop_QV_A(ntry,G1H8(101),wf8(:,247),G1H1(2),m3h8x1(:,303),heltab2x8(:,:,970))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MB,G2tensor(:,38),n2h1(811))
  call Hcheck_last_CV_D(ntry,l_switch,G1H8(137),Q(:,40),wf8(:,247),Q(:,23),G2tensor(:,39),m3h8x1(:,304),heltab2x8(:,:,971))
  call Hloop_VA_Q(ntry,G0H4(54),wf4(:,31),G0H1(1),m3h4x1(:,230),heltab2x4(:,:,632))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,118),n2h1(812))
  call Hloop_VA_Q(ntry,G0H8(103),wf8(:,259),G0H1(1),m3h8x1(:,305),heltab2x8(:,:,972))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,119),n2h1(813))
  call Hloop_VQ_A(ntry,G0H8(104),wf8(:,257),G0H1(1),m3h8x1(:,306),heltab2x8(:,:,973))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,120),n2h1(814))
G2tensor(1:5,78) =G2tensor(1:5,78) + G2tensor(1:5,58) + G2tensor(1:5,5) + G1tensor(1:5,3) + G1tensor(1:5,2) + G1tensor(1:5,1)
G2tensor(6:15,78) =G2tensor(6:15,78) + G2tensor(6:15,58) + G2tensor(6:15,5)
G2tensor(1:5,39) =G2tensor(1:5,39) + G2tensor(1:5,36) + G2tensor(1:5,35) + G2tensor(1:5,32) + G2tensor(1:5,47) + G2tensor(1:5,22)  &
    + G2tensor(1:5,2) + G2tensor(1:5,122) + G2tensor(1:5,130) + G2tensor(1:5,98) + G2tensor(1:5,124) + G2tensor(1:5,92)  &
    + G2tensor(1:5,74) + G2tensor(1:5,70) + G2tensor(1:5,57) + G2tensor(1:5,133) + G2tensor(1:5,53) + G2tensor(1:5,134)  &
    + G2tensor(1:5,43) + G2tensor(1:5,34) + G2tensor(1:5,42) + G2tensor(1:5,8) + G2tensor(1:5,6) + G2tensor(1:5,17)  &
    + G1tensor(1:5,12) + G1tensor(1:5,11) + G1tensor(1:5,10) + G1tensor(1:5,6) + G1tensor(1:5,5) + G1tensor(1:5,4)
G2tensor(6:15,39) =G2tensor(6:15,39) + G2tensor(6:15,36) + G2tensor(6:15,35) + G2tensor(6:15,32) + G2tensor(6:15,47)  &
    + G2tensor(6:15,22) + G2tensor(6:15,2) + G2tensor(6:15,122) + G2tensor(6:15,130) + G2tensor(6:15,98) + G2tensor(6:15,124)  &
    + G2tensor(6:15,92) + G2tensor(6:15,74) + G2tensor(6:15,70) + G2tensor(6:15,57) + G2tensor(6:15,133) + G2tensor(6:15,53)  &
    + G2tensor(6:15,134) + G2tensor(6:15,43) + G2tensor(6:15,34) + G2tensor(6:15,42) + G2tensor(6:15,8) + G2tensor(6:15,6)  &
    + G2tensor(6:15,17)
G2tensor(1:5,31) =G2tensor(1:5,31) + G2tensor(1:5,28) + G2tensor(1:5,27) + G2tensor(1:5,91) + G2tensor(1:5,87) + G2tensor(1:5,23)  &
    + G2tensor(1:5,14) + G2tensor(1:5,49) + G2tensor(1:5,97) + G2tensor(1:5,141) + G2tensor(1:5,127) + G2tensor(1:5,105)  &
    + G2tensor(1:5,93) + G2tensor(1:5,126) + G2tensor(1:5,117) + G2tensor(1:5,65) + G2tensor(1:5,132) + G2tensor(1:5,116)  &
    + G2tensor(1:5,33) + G2tensor(1:5,30) + G2tensor(1:5,20) + G2tensor(1:5,16) + G2tensor(1:5,15) + G2tensor(1:5,3)  &
    + G1tensor(1:5,15) + G1tensor(1:5,14) + G1tensor(1:5,13) + G1tensor(1:5,9) + G1tensor(1:5,8) + G1tensor(1:5,7)
G2tensor(6:15,31) =G2tensor(6:15,31) + G2tensor(6:15,28) + G2tensor(6:15,27) + G2tensor(6:15,91) + G2tensor(6:15,87)  &
    + G2tensor(6:15,23) + G2tensor(6:15,14) + G2tensor(6:15,49) + G2tensor(6:15,97) + G2tensor(6:15,141) + G2tensor(6:15,127)  &
    + G2tensor(6:15,105) + G2tensor(6:15,93) + G2tensor(6:15,126) + G2tensor(6:15,117) + G2tensor(6:15,65) + G2tensor(6:15,132)  &
    + G2tensor(6:15,116) + G2tensor(6:15,33) + G2tensor(6:15,30) + G2tensor(6:15,20) + G2tensor(6:15,16) + G2tensor(6:15,15)  &
    + G2tensor(6:15,3)
G1tensor(1:5,73) =G1tensor(1:5,73) + G1tensor(1:5,72) + G1tensor(1:5,71) + G1tensor(1:5,70) + G1tensor(1:5,69) + G1tensor(1:5,68)  &
    + G1tensor(1:5,67) + G1tensor(1:5,66) + G1tensor(1:5,65) + G1tensor(1:5,64) + G1tensor(1:5,63) + G1tensor(1:5,62)  &
    + G1tensor(1:5,60) + G1tensor(1:5,59) + G1tensor(1:5,58) + G1tensor(1:5,57) + G1tensor(1:5,56) + G1tensor(1:5,55)  &
    + G1tensor(1:5,54) + G1tensor(1:5,53) + G1tensor(1:5,52) + G1tensor(1:5,51) + G1tensor(1:5,50) + G1tensor(1:5,49)  &
    + G1tensor(1:5,48) + G1tensor(1:5,47) + G1tensor(1:5,46) + G1tensor(1:5,45) + G1tensor(1:5,44) + G1tensor(1:5,43)  &
    + G1tensor(1:5,42) + G1tensor(1:5,41) + G1tensor(1:5,40) + G1tensor(1:5,38) + G1tensor(1:5,37) + G1tensor(1:5,36)  &
    + G1tensor(1:5,35) + G1tensor(1:5,34) + G1tensor(1:5,33) + G1tensor(1:5,32) + G1tensor(1:5,31) + G1tensor(1:5,30)  &
    + G1tensor(1:5,29) + G1tensor(1:5,28) + G1tensor(1:5,27) + G1tensor(1:5,26) + G1tensor(1:5,25) + G1tensor(1:5,24)  &
    + G1tensor(1:5,23) + G1tensor(1:5,22) + G1tensor(1:5,21) + G1tensor(1:5,19) + G1tensor(1:5,18) + G1tensor(1:5,17)
G2tensor(1:15,114) =G2tensor(1:15,114) + G2tensor(1:15,52) + G2tensor(1:15,29) + G2tensor(1:15,18) + G2tensor(1:15,9)  &
    + G2tensor(1:15,4)
G2tensor(1:15,64) =G2tensor(1:15,64) + G2tensor(1:15,10)
G2tensor(1:15,66) =G2tensor(1:15,66) + G2tensor(1:15,11)
G2tensor(1:15,118) =G2tensor(1:15,118) + G2tensor(1:15,21) + G2tensor(1:15,19) + G2tensor(1:15,12) + G2tensor(1:15,143)  &
    + G2tensor(1:15,107) + G2tensor(1:15,140) + G2tensor(1:15,67)
G2tensor(1:15,119) =G2tensor(1:15,119) + G2tensor(1:15,26) + G2tensor(1:15,25) + G2tensor(1:15,13) + G2tensor(1:15,96)  &
    + G2tensor(1:15,125) + G2tensor(1:15,142) + G2tensor(1:15,115)
G1tensor(1:5,105) =G1tensor(1:5,105) + G1tensor(1:5,75)
G1tensor(1:5,81) =G1tensor(1:5,81) + G1tensor(1:5,79) + G1tensor(1:5,76)
G2tensor(1:15,37) =G2tensor(1:15,37) + G2tensor(1:15,50) + G2tensor(1:15,7) + G2tensor(1:15,138) + G2tensor(1:15,101)  &
    + G2tensor(1:15,104) + G2tensor(1:15,79) + G2tensor(1:15,135)
G2tensor(1:15,38) =G2tensor(1:15,38) + G2tensor(1:15,51) + G2tensor(1:15,24) + G2tensor(1:15,1) + G2tensor(1:15,112)  &
    + G2tensor(1:15,106) + G2tensor(1:15,73) + G2tensor(1:15,56)
G1tensor(1:5,99) =G1tensor(1:5,99) + G1tensor(1:5,78)
G1tensor(1:5,120) =G1tensor(1:5,120) + G1tensor(1:5,113) + G1tensor(1:5,111) + G1tensor(1:5,103) + G1tensor(1:5,101)  &
    + G1tensor(1:5,84) + G1tensor(1:5,82) + G1tensor(1:5,80)
G1tensor(1:5,114) =G1tensor(1:5,114) + G1tensor(1:5,83)
G1tensor(1:5,104) =G1tensor(1:5,104) + G1tensor(1:5,85)
G1tensor(1:5,94) =G1tensor(1:5,94) + G1tensor(1:5,91) + G1tensor(1:5,88)
G1tensor(1:5,100) =G1tensor(1:5,100) + G1tensor(1:5,89)
G1tensor(1:5,106) =G1tensor(1:5,106) + G1tensor(1:5,92)
G1tensor(1:5,119) =G1tensor(1:5,119) + G1tensor(1:5,117) + G1tensor(1:5,116) + G1tensor(1:5,109) + G1tensor(1:5,108)  &
    + G1tensor(1:5,97) + G1tensor(1:5,96) + G1tensor(1:5,93)
G1tensor(1:5,118) =G1tensor(1:5,118) + G1tensor(1:5,95)
G1tensor(1:5,110) =G1tensor(1:5,110) + G1tensor(1:5,98)
G1tensor(1:5,112) =G1tensor(1:5,112) + G1tensor(1:5,102)
G1tensor(1:5,115) =G1tensor(1:5,115) + G1tensor(1:5,107)
T2sum(1:15,85) =T2sum(1:15,85) +G2tensor(:,78)
T2sum(1:15,86) =T2sum(1:15,86) +G2tensor(:,39)
T2sum(1:15,87) =T2sum(1:15,87) +G2tensor(:,31)
T1sum(1:5,1) =T1sum(1:5,1) +G1tensor(:,16)
T1sum(1:5,2) =T1sum(1:5,2) +G1tensor(:,73)
T1sum(1:5,3) =T1sum(1:5,3) +G1tensor(:,20)
T2sum(1:15,88) =T2sum(1:15,88) +G2tensor(:,114)
T2sum(1:15,89) =T2sum(1:15,89) +G2tensor(:,64)
T2sum(1:15,90) =T2sum(1:15,90) +G2tensor(:,66)
T1sum(1:5,4) =T1sum(1:5,4) +G1tensor(:,39)
T1sum(1:5,5) =T1sum(1:5,5) +G1tensor(:,61)
T2sum(1:15,91) =T2sum(1:15,91) +G2tensor(:,118)
T2sum(1:15,92) =T2sum(1:15,92) +G2tensor(:,119)
T1sum(1:5,6) =T1sum(1:5,6) +G1tensor(:,74)
T1sum(1:5,7) =T1sum(1:5,7) +G1tensor(:,105)
T1sum(1:5,8) =T1sum(1:5,8) +G1tensor(:,81)
T2sum(1:15,93) =T2sum(1:15,93) +G2tensor(:,37)
T2sum(1:15,94) =T2sum(1:15,94) +G2tensor(:,38)
T1sum(1:5,9) =T1sum(1:5,9) +G1tensor(:,77)
T1sum(1:5,10) =T1sum(1:5,10) +G1tensor(:,99)
T2sum(1:15,95) =T2sum(1:15,95) +G2tensor(:,61)
T2sum(1:15,96) =T2sum(1:15,96) +G2tensor(:,69)
T1sum(1:5,11) =T1sum(1:5,11) +G1tensor(:,120)
T1sum(1:5,12) =T1sum(1:5,12) +G1tensor(:,114)
T1sum(1:5,13) =T1sum(1:5,13) +G1tensor(:,104)
T1sum(1:5,14) =T1sum(1:5,14) +G1tensor(:,86)
T1sum(1:5,15) =T1sum(1:5,15) +G1tensor(:,87)
T1sum(1:5,16) =T1sum(1:5,16) +G1tensor(:,94)
T1sum(1:5,17) =T1sum(1:5,17) +G1tensor(:,100)
T1sum(1:5,18) =T1sum(1:5,18) +G1tensor(:,90)
T1sum(1:5,19) =T1sum(1:5,19) +G1tensor(:,106)
T1sum(1:5,20) =T1sum(1:5,20) +G1tensor(:,119)
T1sum(1:5,21) =T1sum(1:5,21) +G1tensor(:,118)
T1sum(1:5,22) =T1sum(1:5,22) +G1tensor(:,110)
T1sum(1:5,23) =T1sum(1:5,23) +G1tensor(:,112)
T1sum(1:5,24) =T1sum(1:5,24) +G1tensor(:,115)



end subroutine vamp_14

end module ol_vamp_14_pphbbj_bbxhggg_1_/**/REALKIND
