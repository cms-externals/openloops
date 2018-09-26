
module ol_vamp_9_pphbbj_bbbxbxhg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_9(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphbbj_bbbxbxhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphbbj_bbbxbxhg_1.
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
  use ol_tables_storage_pphbbj_bbbxbxhg_1_/**/DREALKIND
  use ol_tensor_sum_storage_pphbbj_bbbxbxhg_1_/**/REALKIND
! no reduction 

  use ol_loop_storage_pphbbj_bbbxbxhg_1_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_storage_pphbbj_bbbxbxhg_1_/**/DREALKIND, only: ntry, p_switch, Hel
#endif
  use hol_initialisation_/**/REALKIND, only: G0_hol_initialisation
  use ol_h_vert_interface_/**/REALKIND
  use ol_h_prop_interface_/**/REALKIND
  use ol_h_last_step_/**/REALKIND
  use ol_merging_/**/REALKIND, only: ol_merge

  implicit none

  type(Hpolcont) :: Gcoeff(32)
  type(Hpolcont), intent(in) :: M(4,32)


#ifndef PRECISION_dp
  if (ntry==1 .OR. p_switch == 1) Gcoeff(:)%hf = Hel
#else
  if (ntry==1 .OR. p_switch == 2) Gcoeff(:)%hf = Hel
#endif

  ! =============================================
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(88),Q(:,58),wf4(:,1),Q(:,5),G2tensor(:,37),m3h4x1(:,250),heltab2x4(:,:,556))
  call Hcheck_last_QA_V(ntry,l_switch,G1H8(60),wf8(:,3),G1tensor(:,27),m3h8x1(:,88),heltab2x8(:,:,512))
  call Hloop_QV_A(ntry,G1H4(279),wf4(:,5),G1H1(6),m3h4x1(:,251),heltab2x4(:,:,557))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(6),Q(:,63),ZERO,G2tensor(:,75),n2h1(587))
  call Hloop_QV_A(ntry,G1H4(278),wf4(:,5),G1H1(3),m3h4x1(:,252),heltab2x4(:,:,558))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MT,G2tensor(:,81),n2h1(588))
  call Hloop_QV_A(ntry,G1H4(280),wf4(:,5),G1H1(4),m3h4x1(:,253),heltab2x4(:,:,559))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(4),Q(:,63),MB,G2tensor(:,113),n2h1(589))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(281),Q(:,37),wf4(:,5),Q(:,26),G2tensor(:,117),m3h4x1(:,254),heltab2x4(:,:,560))
  call Hcheck_last_QA_V(ntry,l_switch,G1H2(1),wf2(:,4),G1tensor(:,28),m3h2x1(:,357),heltab2x2(:,:,386))
  call Hloop_VA_Q(ntry,G0H8(63),wf8(:,100),G0H1(1),m3h8x1(:,89),heltab2x8(:,:,513))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,29),n2h1(590))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(49),wf8(:,6),G1tensor(:,30),m3h8x1(:,90),heltab2x8(:,:,514))
  call Hloop_QV_A(ntry,G1H4(327),wf4(:,9),G1H1(5),m3h4x1(:,255),heltab2x4(:,:,561))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(5),Q(:,63),ZERO,G2tensor(:,149),n2h1(591))
  call Hloop_QV_A(ntry,G1H4(326),wf4(:,9),G1H1(10),m3h4x1(:,256),heltab2x4(:,:,562))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(10),Q(:,63),MT,G2tensor(:,30),n2h1(592))
  call Hloop_QV_A(ntry,G1H4(328),wf4(:,9),G1H1(8),m3h4x1(:,257),heltab2x4(:,:,563))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(8),Q(:,63),MB,G2tensor(:,38),n2h1(593))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(329),Q(:,37),wf4(:,9),Q(:,26),G2tensor(:,76),m3h4x1(:,258),heltab2x4(:,:,564))
  call Hloop_VA_Q(ntry,G0H2(38),wf2(:,4),G0H1(1),m3h2x1(:,358),heltab2x2(:,:,387))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,31),n2h1(594))
  call Hloop_VA_Q(ntry,G0H8(8),wf8(:,150),G0H1(1),m3h8x1(:,91),heltab2x8(:,:,515))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,32),n2h1(595))
  call Hloop_VQ_A(ntry,G0H8(12),wf8(:,148),G0H1(1),m3h8x1(:,92),heltab2x8(:,:,516))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,33),n2h1(596))
  call Hloop_VQ_A(ntry,G0H8(22),wf8(:,104),G0H1(1),m3h8x1(:,93),heltab2x8(:,:,517))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,34),n2h1(597))
  call Hloop_QV_A(ntry,G1H4(71),wf4(:,12),G1H1(11),m3h4x1(:,259),heltab2x4(:,:,565))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(11),Q(:,63),ZERO,G2tensor(:,82),n2h1(598))
  call Hloop_QV_A(ntry,G1H4(159),wf4(:,12),G1H1(13),m3h4x1(:,260),heltab2x4(:,:,566))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(13),Q(:,63),MT,G2tensor(:,114),n2h1(599))
  call Hloop_QV_A(ntry,G1H4(15),wf4(:,12),G1H1(7),m3h4x1(:,261),heltab2x4(:,:,567))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(7),Q(:,63),MB,G2tensor(:,118),n2h1(600))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(128),Q(:,57),wf4(:,12),Q(:,6),G2tensor(:,150),m3h4x1(:,262),heltab2x4(:,:,568))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H2(31),wf2(:,6),G1tensor(:,35),m3h2x1(:,359),heltab2x2(:,:,388))
  call Hloop_QV_A(ntry,G1H4(255),wf4(:,13),G1H1(2),m3h4x1(:,263),heltab2x4(:,:,569))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),ZERO,G2tensor(:,53),n2h1(601))
  call Hloop_QV_A(ntry,G1H4(254),wf4(:,13),G1H1(9),m3h4x1(:,264),heltab2x4(:,:,570))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(9),Q(:,63),MT,G2tensor(:,59),n2h1(602))
  call Hloop_QV_A(ntry,G1H4(256),wf4(:,13),G1H1(1),m3h4x1(:,265),heltab2x4(:,:,571))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),MB,G2tensor(:,95),n2h1(603))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(257),Q(:,38),wf4(:,13),Q(:,25),G2tensor(:,99),m3h4x1(:,266),heltab2x4(:,:,572))
  call Hcheck_last_QA_V(ntry,l_switch,G1H8(29),wf8(:,10),G1tensor(:,36),m3h8x1(:,94),heltab2x8(:,:,518))
  call Hloop_VA_Q(ntry,G0H8(57),wf8(:,109),G0H1(1),m3h8x1(:,95),heltab2x8(:,:,519))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,37),n2h1(604))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(62),wf8(:,12),G1tensor(:,38),m3h8x1(:,96),heltab2x8(:,:,520))
  call Hloop_QV_A(ntry,G1H4(333),wf4(:,17),G1H1(12),m3h4x1(:,267),heltab2x4(:,:,573))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(12),Q(:,63),ZERO,G2tensor(:,133),n2h1(605))
  call Hloop_QV_A(ntry,G1H4(332),wf4(:,17),G1H1(6),m3h4x1(:,268),heltab2x4(:,:,574))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(6),Q(:,63),MT,G2tensor(:,135),n2h1(606))
  call Hloop_QV_A(ntry,G1H4(334),wf4(:,17),G1H1(3),m3h4x1(:,269),heltab2x4(:,:,575))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),MB,G2tensor(:,151),n2h1(607))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(335),Q(:,38),wf4(:,17),Q(:,25),G2tensor(:,54),m3h4x1(:,270),heltab2x4(:,:,576))
  call Hloop_VA_Q(ntry,G0H8(43),wf8(:,154),G0H1(1),m3h8x1(:,97),heltab2x8(:,:,521))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,39),n2h1(608))
  call Hloop_VQ_A(ntry,G0H8(49),wf8(:,152),G0H1(1),m3h8x1(:,98),heltab2x8(:,:,522))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,40),n2h1(609))
  call Hloop_VQ_A(ntry,G0H8(38),wf8(:,114),G0H1(1),m3h8x1(:,99),heltab2x8(:,:,523))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,41),n2h1(610))
  call Hloop_QV_A(ntry,G1H4(177),wf4(:,19),G1H1(4),m3h4x1(:,271),heltab2x4(:,:,577))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(4),Q(:,63),ZERO,G2tensor(:,60),n2h1(611))
  call Hloop_QV_A(ntry,G1H4(176),wf4(:,19),G1H1(5),m3h4x1(:,272),heltab2x4(:,:,578))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(5),Q(:,63),MT,G2tensor(:,96),n2h1(612))
  call Hloop_QV_A(ntry,G1H4(178),wf4(:,19),G1H1(10),m3h4x1(:,273),heltab2x4(:,:,579))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(10),Q(:,63),MB,G2tensor(:,100),n2h1(613))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(179),Q(:,54),wf4(:,19),Q(:,9),G2tensor(:,134),m3h4x1(:,274),heltab2x4(:,:,580))
  call Hcheck_last_QA_V(ntry,l_switch,G1H8(32),wf8(:,15),G1tensor(:,42),m3h8x1(:,100),heltab2x8(:,:,524))
  call Hloop_QV_A(ntry,G1H4(285),wf4(:,22),G1H1(8),m3h4x1(:,275),heltab2x4(:,:,581))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(8),Q(:,63),ZERO,G2tensor(:,136),n2h1(614))
  call Hloop_QV_A(ntry,G1H4(284),wf4(:,22),G1H1(11),m3h4x1(:,276),heltab2x4(:,:,582))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(11),Q(:,63),MT,G2tensor(:,152),n2h1(615))
  call Hloop_QV_A(ntry,G1H4(286),wf4(:,22),G1H1(13),m3h4x1(:,277),heltab2x4(:,:,583))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(13),Q(:,63),MB,G2tensor(:,66),n2h1(616))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(287),Q(:,41),wf4(:,22),Q(:,22),G2tensor(:,79),m3h4x1(:,278),heltab2x4(:,:,584))
  call Hcheck_last_QA_V(ntry,l_switch,G1H2(4),wf2(:,8),G1tensor(:,43),m3h2x1(:,360),heltab2x2(:,:,389))
  call Hloop_VA_Q(ntry,G0H8(14),wf8(:,120),G0H1(1),m3h8x1(:,101),heltab2x8(:,:,525))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,44),n2h1(617))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(1),wf8(:,18),G1tensor(:,45),m3h8x1(:,102),heltab2x8(:,:,526))
  call Hloop_QV_A(ntry,G1H4(309),wf4(:,24),G1H1(7),m3h4x1(:,279),heltab2x4(:,:,585))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(7),Q(:,63),ZERO,G2tensor(:,88),n2h1(618))
  call Hloop_QV_A(ntry,G1H4(308),wf4(:,24),G1H1(2),m3h4x1(:,280),heltab2x4(:,:,586))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(2),Q(:,63),MT,G2tensor(:,97),n2h1(619))
  call Hloop_QV_A(ntry,G1H4(310),wf4(:,24),G1H1(9),m3h4x1(:,281),heltab2x4(:,:,587))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(9),Q(:,63),MB,G2tensor(:,115),n2h1(620))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(311),Q(:,41),wf4(:,24),Q(:,22),G2tensor(:,131),m3h4x1(:,282),heltab2x4(:,:,588))
  call Hloop_VA_Q(ntry,G0H2(13),wf2(:,8),G0H1(1),m3h2x1(:,361),heltab2x2(:,:,390))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,46),n2h1(621))
  call Hloop_VA_Q(ntry,G0H8(58),wf8(:,158),G0H1(1),m3h8x1(:,103),heltab2x8(:,:,527))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,47),n2h1(622))
  call Hloop_VQ_A(ntry,G0H8(40),wf8(:,156),G0H1(1),m3h8x1(:,104),heltab2x8(:,:,528))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,48),n2h1(623))
  call Hloop_VQ_A(ntry,G0H8(3),wf8(:,124),G0H1(1),m3h8x1(:,105),heltab2x8(:,:,529))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,49),n2h1(624))
  call Hloop_QV_A(ntry,G1H4(237),wf4(:,26),G1H1(1),m3h4x1(:,283),heltab2x4(:,:,589))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(1),Q(:,63),ZERO,G2tensor(:,161),n2h1(625))
  call Hloop_QV_A(ntry,G1H4(236),wf4(:,26),G1H1(12),m3h4x1(:,284),heltab2x4(:,:,590))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(12),Q(:,63),MT,G2tensor(:,67),n2h1(626))
  call Hloop_QV_A(ntry,G1H4(238),wf4(:,26),G1H1(6),m3h4x1(:,285),heltab2x4(:,:,591))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(6),Q(:,63),MB,G2tensor(:,80),n2h1(627))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(239),Q(:,53),wf4(:,26),Q(:,10),G2tensor(:,89),m3h4x1(:,286),heltab2x4(:,:,592))
  call Hloop_QV_A(ntry,G1H4(261),wf4(:,27),G1H1(3),m3h4x1(:,287),heltab2x4(:,:,593))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(3),Q(:,63),ZERO,G2tensor(:,98),n2h1(628))
  call Hloop_QV_A(ntry,G1H4(260),wf4(:,27),G1H1(4),m3h4x1(:,288),heltab2x4(:,:,594))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(4),Q(:,63),MT,G2tensor(:,116),n2h1(629))
  call Hloop_QV_A(ntry,G1H4(262),wf4(:,27),G1H1(5),m3h4x1(:,289),heltab2x4(:,:,595))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(5),Q(:,63),MB,G2tensor(:,132),n2h1(630))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(263),Q(:,42),wf4(:,27),Q(:,21),G2tensor(:,162),m3h4x1(:,290),heltab2x4(:,:,596))
  call Hcheck_last_QA_V(ntry,l_switch,G1H8(7),wf8(:,22),G1tensor(:,50),m3h8x1(:,106),heltab2x8(:,:,530))
  call Hloop_VA_Q(ntry,G0H8(5),wf8(:,129),G0H1(1),m3h8x1(:,107),heltab2x8(:,:,531))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,51),n2h1(631))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(43),wf8(:,24),G1tensor(:,52),m3h8x1(:,108),heltab2x8(:,:,532))
  call Hloop_QV_A(ntry,G1H4(303),wf4(:,28),G1H1(10),m3h4x1(:,291),heltab2x4(:,:,597))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(10),Q(:,63),ZERO,G2tensor(:,101),n2h1(632))
  call Hloop_QV_A(ntry,G1H4(302),wf4(:,28),G1H1(8),m3h4x1(:,292),heltab2x4(:,:,598))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(8),Q(:,63),MT,G2tensor(:,102),n2h1(633))
  call Hloop_QV_A(ntry,G1H4(304),wf4(:,28),G1H1(11),m3h4x1(:,293),heltab2x4(:,:,599))
  call Hcheck_last_Q_A(ntry,l_switch,G1H1(11),Q(:,63),MB,G2tensor(:,103),n2h1(634))
  call Hcheck_last_CV_D(ntry,l_switch,G1H4(305),Q(:,42),wf4(:,28),Q(:,21),G2tensor(:,104),m3h4x1(:,294),heltab2x4(:,:,600))
  call Hloop_VA_Q(ntry,G0H8(9),wf8(:,162),G0H1(1),m3h8x1(:,109),heltab2x8(:,:,533))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,53),n2h1(635))
  call Hloop_VQ_A(ntry,G0H8(17),wf8(:,160),G0H1(1),m3h8x1(:,110),heltab2x8(:,:,534))
  call Hcheck_last_Q_A(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,54),n2h1(636))
  call Hloop_VA_Q(ntry,G0H8(20),wf8(:,166),G0H1(1),m3h8x1(:,111),heltab2x8(:,:,535))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,55),n2h1(637))
  call Hloop_VA_Q(ntry,G0H8(18),wf8(:,171),G0H1(1),m3h8x1(:,112),heltab2x8(:,:,536))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,56),n2h1(638))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(45),wf8(:,169),G1tensor(:,57),m3h8x1(:,113),heltab2x8(:,:,537))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(63),wf8(:,164),G1tensor(:,58),m3h8x1(:,114),heltab2x8(:,:,538))
  call Hloop_VA_Q(ntry,G0H8(56),wf8(:,180),G0H1(1),m3h8x1(:,115),heltab2x8(:,:,539))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,59),n2h1(639))
  call Hloop_VA_Q(ntry,G0H8(25),wf8(:,183),G0H1(1),m3h8x1(:,116),heltab2x8(:,:,540))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,60),n2h1(640))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(52),wf8(:,178),G1tensor(:,61),m3h8x1(:,117),heltab2x8(:,:,541))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(35),wf8(:,176),G1tensor(:,62),m3h8x1(:,118),heltab2x8(:,:,542))
  call Hloop_VA_Q(ntry,G0H8(31),wf8(:,196),G0H1(1),m3h8x1(:,119),heltab2x8(:,:,543))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,63),n2h1(641))
  call Hloop_VA_Q(ntry,G0H8(35),wf8(:,192),G0H1(1),m3h8x1(:,120),heltab2x8(:,:,544))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,64),n2h1(642))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(42),wf8(:,188),G1tensor(:,65),m3h8x1(:,121),heltab2x8(:,:,545))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(18),wf8(:,190),G1tensor(:,66),m3h8x1(:,122),heltab2x8(:,:,546))
  call Hloop_VA_Q(ntry,G0H8(61),wf8(:,208),G0H1(1),m3h8x1(:,123),heltab2x8(:,:,547))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,67),n2h1(643))
  call Hloop_VA_Q(ntry,G0H8(44),wf8(:,204),G0H1(1),m3h8x1(:,124),heltab2x8(:,:,548))
  call Hcheck_last_A_Q(ntry,l_switch,G0H1(1),Q(:,63),MB,G1tensor(:,68),n2h1(644))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(44),wf8(:,200),G1tensor(:,69),m3h8x1(:,125),heltab2x8(:,:,549))
  call Hcheck_last_AQ_V(ntry,l_switch,G1H8(56),wf8(:,202),G1tensor(:,70),m3h8x1(:,126),heltab2x8(:,:,550))
G1tensor(1:5,24) =G1tensor(1:5,24) + G1tensor(1:5,23) + G1tensor(1:5,22) + G1tensor(1:5,21) + G1tensor(1:5,20) + G1tensor(1:5,19)  &
    + G1tensor(1:5,18) + G1tensor(1:5,17) + G1tensor(1:5,16) + G1tensor(1:5,15) + G1tensor(1:5,14) + G1tensor(1:5,13)  &
    + G1tensor(1:5,12) + G1tensor(1:5,11) + G1tensor(1:5,10) + G1tensor(1:5,9) + G1tensor(1:5,8) + G1tensor(1:5,7)  &
    + G1tensor(1:5,6) + G1tensor(1:5,5) + G1tensor(1:5,4) + G1tensor(1:5,3) + G1tensor(1:5,2) + G1tensor(1:5,1)
G2tensor(1:15,37) =G2tensor(1:15,37) + G2tensor(1:15,130) + G2tensor(1:15,20)
G2tensor(1:15,76) =G2tensor(1:15,76) + G2tensor(1:15,149) + G2tensor(1:15,117) + G2tensor(1:15,75) + G2tensor(1:15,46)  &
    + G2tensor(1:15,35)
G2tensor(1:15,150) =G2tensor(1:15,150) + G2tensor(1:15,82) + G2tensor(1:15,57)
G2tensor(1:15,54) =G2tensor(1:15,54) + G2tensor(1:15,133) + G2tensor(1:15,99) + G2tensor(1:15,53) + G2tensor(1:15,129)  &
    + G2tensor(1:15,111)
G2tensor(1:15,134) =G2tensor(1:15,134) + G2tensor(1:15,60) + G2tensor(1:15,145)
G2tensor(1:15,131) =G2tensor(1:15,131) + G2tensor(1:15,88) + G2tensor(1:15,79) + G2tensor(1:15,136) + G2tensor(1:15,36)  &
    + G2tensor(1:15,21)
G2tensor(1:15,89) =G2tensor(1:15,89) + G2tensor(1:15,161) + G2tensor(1:15,47)
G2tensor(1:15,104) =G2tensor(1:15,104) + G2tensor(1:15,101) + G2tensor(1:15,162) + G2tensor(1:15,98) + G2tensor(1:15,112)  &
    + G2tensor(1:15,58)
G1tensor(1:5,40) =G1tensor(1:5,40) + G1tensor(1:5,34) + G1tensor(1:5,33) + G1tensor(1:5,25)
G1tensor(1:5,42) =G1tensor(1:5,42) + G1tensor(1:5,27)
G2tensor(1:15,30) =G2tensor(1:15,30) + G2tensor(1:15,81)
G2tensor(1:15,38) =G2tensor(1:15,38) + G2tensor(1:15,113)
G1tensor(1:5,68) =G1tensor(1:5,68) + G1tensor(1:5,64) + G1tensor(1:5,56) + G1tensor(1:5,55) + G1tensor(1:5,47) + G1tensor(1:5,44)  &
    + G1tensor(1:5,32) + G1tensor(1:5,29)
G1tensor(1:5,38) =G1tensor(1:5,38) + G1tensor(1:5,30)
G2tensor(1:15,135) =G2tensor(1:15,135) + G2tensor(1:15,59)
G2tensor(1:15,151) =G2tensor(1:15,151) + G2tensor(1:15,95)
G1tensor(1:5,50) =G1tensor(1:5,50) + G1tensor(1:5,36)
G1tensor(1:5,67) =G1tensor(1:5,67) + G1tensor(1:5,63) + G1tensor(1:5,60) + G1tensor(1:5,59) + G1tensor(1:5,53) + G1tensor(1:5,51)  &
    + G1tensor(1:5,39) + G1tensor(1:5,37)
G1tensor(1:5,54) =G1tensor(1:5,54) + G1tensor(1:5,49) + G1tensor(1:5,48) + G1tensor(1:5,41)
G2tensor(1:15,97) =G2tensor(1:15,97) + G2tensor(1:15,152)
G2tensor(1:15,115) =G2tensor(1:15,115) + G2tensor(1:15,66)
G1tensor(1:5,52) =G1tensor(1:5,52) + G1tensor(1:5,45)
G2tensor(1:15,102) =G2tensor(1:15,102) + G2tensor(1:15,116)
G2tensor(1:15,103) =G2tensor(1:15,103) + G2tensor(1:15,132)
G1tensor(1:5,70) =G1tensor(1:5,70) + G1tensor(1:5,69) + G1tensor(1:5,61) + G1tensor(1:5,57)
G1tensor(1:5,66) =G1tensor(1:5,66) + G1tensor(1:5,65) + G1tensor(1:5,62) + G1tensor(1:5,58)
T1sum(1:5,1) =T1sum(1:5,1) +G1tensor(:,24)
T2sum(1:15,161) =T2sum(1:15,161) +G2tensor(:,37)
T2sum(1:15,162) =T2sum(1:15,162) +G2tensor(:,76)
T2sum(1:15,163) =T2sum(1:15,163) +G2tensor(:,150)
T2sum(1:15,164) =T2sum(1:15,164) +G2tensor(:,54)
T2sum(1:15,165) =T2sum(1:15,165) +G2tensor(:,134)
T2sum(1:15,166) =T2sum(1:15,166) +G2tensor(:,131)
T2sum(1:15,167) =T2sum(1:15,167) +G2tensor(:,89)
T2sum(1:15,168) =T2sum(1:15,168) +G2tensor(:,104)
T1sum(1:5,2) =T1sum(1:5,2) +G1tensor(:,40)
T1sum(1:5,3) =T1sum(1:5,3) +G1tensor(:,26)
T2sum(1:15,169) =T2sum(1:15,169) +G2tensor(:,146)
T2sum(1:15,170) =T2sum(1:15,170) +G2tensor(:,29)
T1sum(1:5,4) =T1sum(1:5,4) +G1tensor(:,42)
T2sum(1:15,171) =T2sum(1:15,171) +G2tensor(:,30)
T2sum(1:15,172) =T2sum(1:15,172) +G2tensor(:,38)
T1sum(1:5,5) =T1sum(1:5,5) +G1tensor(:,28)
T1sum(1:5,6) =T1sum(1:5,6) +G1tensor(:,68)
T1sum(1:5,7) =T1sum(1:5,7) +G1tensor(:,38)
T1sum(1:5,8) =T1sum(1:5,8) +G1tensor(:,31)
T2sum(1:15,173) =T2sum(1:15,173) +G2tensor(:,114)
T2sum(1:15,174) =T2sum(1:15,174) +G2tensor(:,118)
T1sum(1:5,9) =T1sum(1:5,9) +G1tensor(:,35)
T2sum(1:15,175) =T2sum(1:15,175) +G2tensor(:,135)
T2sum(1:15,176) =T2sum(1:15,176) +G2tensor(:,151)
T1sum(1:5,10) =T1sum(1:5,10) +G1tensor(:,50)
T1sum(1:5,11) =T1sum(1:5,11) +G1tensor(:,67)
T1sum(1:5,12) =T1sum(1:5,12) +G1tensor(:,54)
T2sum(1:15,177) =T2sum(1:15,177) +G2tensor(:,96)
T2sum(1:15,178) =T2sum(1:15,178) +G2tensor(:,100)
T2sum(1:15,179) =T2sum(1:15,179) +G2tensor(:,97)
T2sum(1:15,180) =T2sum(1:15,180) +G2tensor(:,115)
T1sum(1:5,13) =T1sum(1:5,13) +G1tensor(:,43)
T1sum(1:5,14) =T1sum(1:5,14) +G1tensor(:,52)
T1sum(1:5,15) =T1sum(1:5,15) +G1tensor(:,46)
T2sum(1:15,181) =T2sum(1:15,181) +G2tensor(:,67)
T2sum(1:15,182) =T2sum(1:15,182) +G2tensor(:,80)
T2sum(1:15,183) =T2sum(1:15,183) +G2tensor(:,102)
T2sum(1:15,184) =T2sum(1:15,184) +G2tensor(:,103)
T1sum(1:5,16) =T1sum(1:5,16) +G1tensor(:,70)
T1sum(1:5,17) =T1sum(1:5,17) +G1tensor(:,66)



end subroutine vamp_9

end module ol_vamp_9_pphbbj_bbbxbxhg_1_/**/REALKIND
