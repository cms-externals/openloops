
module ol_colourmatrix_ppttj_ttxbbxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(276,12)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  144,   48,   48,   18,    0,   -6,   48,    0,    0,   48,   -6,    0]
  K1(  2,:) = [   48,  144,   18,   48,   -6,    0,    0,   48,   48,    0,    0,   -6]
  K1(  3,:) = [   48,   18,  144,   48,   48,    0,    0,   -6,   -6,    0,    0,   48]
  K1(  4,:) = [   18,   48,   48,  144,    0,   48,   -6,    0,    0,   -6,   48,    0]
  K1(  5,:) = [    0,   -6,   48,    0,  144,   48,   48,   18,  -18,   -6,   -6,   18]
  K1(  6,:) = [   -6,    0,    0,   48,   48,  144,   18,   48,   -6,  -18,   18,   -6]
  K1(  7,:) = [   48,    0,    0,   -6,   48,   18,  144,   48,   -6,   18,  -18,   -6]
  K1(  8,:) = [    0,   48,   -6,    0,   18,   48,   48,  144,   18,   -6,   -6,  -18]
  K1(  9,:) = [    0,   48,   -6,    0,  -18,   -6,   -6,   18,  144,   48,   48,   18]
  K1( 10,:) = [   48,    0,    0,   -6,   -6,  -18,   18,   -6,   48,  144,   18,   48]
  K1( 11,:) = [   -6,    0,    0,   48,   -6,   18,  -18,   -6,   48,   18,  144,   48]
  K1( 12,:) = [    0,   -6,   48,    0,   18,   -6,   -6,  -18,   18,   48,   48,  144]
  K1( 13,:) = [  192,   64,   64,   24,    0,   -8,   64,    0,    0,   64,   -8,    0]
  K1( 14,:) = [   64,  192,   24,   64,   -8,    0,    0,   64,   64,    0,    0,   -8]
  K1( 15,:) = [   64,   24,  192,   64,   64,    0,    0,   -8,   -8,    0,    0,   64]
  K1( 16,:) = [   24,   64,   64,  192,    0,   64,   -8,    0,    0,   -8,   64,    0]
  K1( 17,:) = [    0,   -8,   64,    0,  192,   64,   64,   24,  -24,   -8,   -8,   24]
  K1( 18,:) = [   -8,    0,    0,   64,   64,  192,   24,   64,   -8,  -24,   24,   -8]
  K1( 19,:) = [   64,    0,    0,   -8,   64,   24,  192,   64,   -8,   24,  -24,   -8]
  K1( 20,:) = [    0,   64,   -8,    0,   24,   64,   64,  192,   24,   -8,   -8,  -24]
  K1( 21,:) = [    0,   64,   -8,    0,  -24,   -8,   -8,   24,  192,   64,   64,   24]
  K1( 22,:) = [   64,    0,    0,   -8,   -8,  -24,   24,   -8,   64,  192,   24,   64]
  K1( 23,:) = [   -8,    0,    0,   64,   -8,   24,  -24,   -8,   64,   24,  192,   64]
  K1( 24,:) = [    0,   -8,   64,    0,   24,   -8,   -8,  -24,   24,   64,   64,  192]
  K1( 25,:) = [   24,    8,    8,    3,    0,   -1,    8,    0,    0,    8,   -1,    0]
  K1( 26,:) = [    8,    0,    6,    8,  -10,  -24,  -24,  -64,   -1,    3,    3,    8]
  K1( 27,:) = [    8,    6,    0,    8,   -1,    3,    3,    8,  -10,  -24,  -24,  -64]
  K1( 28,:) = [    3,    8,    8,   24,    0,    8,   -1,    0,    0,   -1,    8,    0]
  K1( 29,:) = [    0,  -10,   -1,    0,   -3,   -1,   -1,  -24,  -30,  -10,  -10,  -24]
  K1( 30,:) = [   -1,  -24,    3,    8,   -1,    0,  -21,  -64,  -10,    0,    6,    8]
  K1( 31,:) = [    8,  -24,    3,   -1,   -1,  -21,    0,  -64,  -10,    6,    0,    8]
  K1( 32,:) = [    0,  -64,    8,    0,  -24,  -64,  -64, -192,  -24,    8,    8,   24]
  K1( 33,:) = [    0,   -1,  -10,    0,  -30,  -10,  -10,  -24,   -3,   -1,   -1,  -24]
  K1( 34,:) = [    8,    3,  -24,   -1,  -10,    0,    6,    8,   -1,    0,  -21,  -64]
  K1( 35,:) = [   -1,    3,  -24,    8,  -10,    6,    0,    8,   -1,  -21,    0,  -64]
  K1( 36,:) = [    0,    8,  -64,    0,  -24,    8,    8,   24,  -24,  -64,  -64, -192]
  K1( 37,:) = [  192,   64,   64,   24,    0,   -8,   64,    0,    0,   64,   -8,    0]
  K1( 38,:) = [   64,  192,   24,   64,   -8,    0,    0,   64,   64,    0,    0,   -8]
  K1( 39,:) = [   64,   24,  192,   64,   64,    0,    0,   -8,   -8,    0,    0,   64]
  K1( 40,:) = [   24,   64,   64,  192,    0,   64,   -8,    0,    0,   -8,   64,    0]
  K1( 41,:) = [    0,   -8,   64,    0,  192,   64,   64,   24,  -24,   -8,   -8,   24]
  K1( 42,:) = [   -8,    0,    0,   64,   64,  192,   24,   64,   -8,  -24,   24,   -8]
  K1( 43,:) = [   64,    0,    0,   -8,   64,   24,  192,   64,   -8,   24,  -24,   -8]
  K1( 44,:) = [    0,   64,   -8,    0,   24,   64,   64,  192,   24,   -8,   -8,  -24]
  K1( 45,:) = [    0,   64,   -8,    0,  -24,   -8,   -8,   24,  192,   64,   64,   24]
  K1( 46,:) = [   64,    0,    0,   -8,   -8,  -24,   24,   -8,   64,  192,   24,   64]
  K1( 47,:) = [   -8,    0,    0,   64,   -8,   24,  -24,   -8,   64,   24,  192,   64]
  K1( 48,:) = [    0,   -8,   64,    0,   24,   -8,   -8,  -24,   24,   64,   64,  192]
  K1( 49,:) = [    0,   64,    1,   21,   -3,    1,   -8,   24,   24,   -8,    1,   -3]
  K1( 50,:) = [   64,    0,   21,    1,    1,   -3,   24,   -8,   -8,   24,   -3,    1]
  K1( 51,:) = [    1,   21,    0,   64,   -8,   24,   -3,    1,    1,   -3,   24,   -8]
  K1( 52,:) = [   21,    1,   64,    0,   24,   -8,    1,   -3,   -3,    1,   -8,   24]
  K1( 53,:) = [   -3,    1,   -8,   24,    0,   64,    1,   21,    0,   -8,   10,   -6]
  K1( 54,:) = [    1,   -3,   24,   -8,   64,    0,   21,    1,   -8,    0,   -6,   10]
  K1( 55,:) = [   -8,   24,   -3,    1,    1,   21,    0,   64,   10,   -6,    0,   -8]
  K1( 56,:) = [   24,   -8,    1,   -3,   21,    1,   64,    0,   -6,   10,   -8,    0]
  K1( 57,:) = [   24,   -8,    1,   -3,    0,   -8,   10,   -6,    0,   64,    1,   21]
  K1( 58,:) = [   -8,   24,   -3,    1,   -8,    0,   -6,   10,   64,    0,   21,    1]
  K1( 59,:) = [    1,   -3,   24,   -8,   10,   -6,    0,   -8,    1,   21,    0,   64]
  K1( 60,:) = [   -3,    1,   -8,   24,   -6,   10,   -8,    0,   21,    1,   64,    0]
  K1( 61,:) = [    0,    8,    8,    6,  -24,  -10,  -64,  -24,    3,   -1,    8,    3]
  K1( 62,:) = [    8,   24,    3,    8,   -1,    0,    0,    8,    8,    0,    0,   -1]
  K1( 63,:) = [    8,    3,   24,    8,    8,    0,    0,   -1,   -1,    0,    0,    8]
  K1( 64,:) = [    6,    8,    8,    0,    3,   -1,    8,    3,  -24,  -10,  -64,  -24]
  K1( 65,:) = [  -24,   -1,    8,    3,    0,   -1,  -64,  -21,    0,  -10,    8,    6]
  K1( 66,:) = [  -10,    0,    0,   -1,   -1,   -3,  -24,   -1,  -10,  -30,  -24,  -10]
  K1( 67,:) = [  -64,    0,    0,    8,  -64,  -24, -192,  -64,    8,  -24,   24,    8]
  K1( 68,:) = [  -24,    8,   -1,    3,  -21,   -1,  -64,    0,    6,  -10,    8,    0]
  K1( 69,:) = [    3,    8,   -1,  -24,    0,  -10,    8,    6,    0,   -1,  -64,  -21]
  K1( 70,:) = [   -1,    0,    0,  -10,  -10,  -30,  -24,  -10,   -1,   -3,  -24,   -1]
  K1( 71,:) = [    8,    0,    0,  -64,    8,  -24,   24,    8,  -64,  -24, -192,  -64]
  K1( 72,:) = [    3,   -1,    8,  -24,    6,  -10,    8,    0,  -21,   -1,  -64,    0]
  K1( 73,:) = [  192,   64,   64,   24,    0,   -8,   64,    0,    0,   64,   -8,    0]
  K1( 74,:) = [   64,  192,   24,   64,   -8,    0,    0,   64,   64,    0,    0,   -8]
  K1( 75,:) = [   64,   24,  192,   64,   64,    0,    0,   -8,   -8,    0,    0,   64]
  K1( 76,:) = [   24,   64,   64,  192,    0,   64,   -8,    0,    0,   -8,   64,    0]
  K1( 77,:) = [    0,   -8,   64,    0,  192,   64,   64,   24,  -24,   -8,   -8,   24]
  K1( 78,:) = [   -8,    0,    0,   64,   64,  192,   24,   64,   -8,  -24,   24,   -8]
  K1( 79,:) = [   64,    0,    0,   -8,   64,   24,  192,   64,   -8,   24,  -24,   -8]
  K1( 80,:) = [    0,   64,   -8,    0,   24,   64,   64,  192,   24,   -8,   -8,  -24]
  K1( 81,:) = [    0,   64,   -8,    0,  -24,   -8,   -8,   24,  192,   64,   64,   24]
  K1( 82,:) = [   64,    0,    0,   -8,   -8,  -24,   24,   -8,   64,  192,   24,   64]
  K1( 83,:) = [   -8,    0,    0,   64,   -8,   24,  -24,   -8,   64,   24,  192,   64]
  K1( 84,:) = [    0,   -8,   64,    0,   24,   -8,   -8,  -24,   24,   64,   64,  192]
  K1( 85,:) = [    0,    8,    8,    6,    3,    8,   -1,    3,  -24,  -64,  -10,  -24]
  K1( 86,:) = [    8,   24,    3,    8,   -1,    0,    0,    8,    8,    0,    0,   -1]
  K1( 87,:) = [    8,    3,   24,    8,    8,    0,    0,   -1,   -1,    0,    0,    8]
  K1( 88,:) = [    6,    8,    8,    0,  -24,  -64,  -10,  -24,    3,    8,   -1,    3]
  K1( 89,:) = [    3,   -1,    8,  -24,    0,  -64,   -1,  -21,    0,    8,  -10,    6]
  K1( 90,:) = [    8,    0,    0,  -64,  -64, -192,  -24,  -64,    8,   24,  -24,    8]
  K1( 91,:) = [   -1,    0,    0,  -10,   -1,  -24,   -3,   -1,  -10,  -24,  -30,  -10]
  K1( 92,:) = [    3,    8,   -1,  -24,  -21,  -64,   -1,    0,    6,    8,  -10,    0]
  K1( 93,:) = [  -24,    8,   -1,    3,    0,    8,  -10,    6,    0,  -64,   -1,  -21]
  K1( 94,:) = [  -64,    0,    0,    8,    8,   24,  -24,    8,  -64, -192,  -24,  -64]
  K1( 95,:) = [  -10,    0,    0,   -1,  -10,  -24,  -30,  -10,   -1,  -24,   -3,   -1]
  K1( 96,:) = [  -24,   -1,    8,    3,    6,    8,  -10,    0,  -21,  -64,   -1,    0]
  K1( 97,:) = [    0,    1,   64,   21,   24,    1,   -8,   -3,   -3,   -8,    1,   24]
  K1( 98,:) = [    1,    0,   21,   64,    1,   24,   -3,   -8,   -8,   -3,   24,    1]
  K1( 99,:) = [   64,   21,    0,    1,   -8,   -3,   24,    1,    1,   24,   -3,   -8]
  K1(100,:) = [   21,   64,    1,    0,   -3,   -8,    1,   24,   24,    1,   -8,   -3]
  K1(101,:) = [   24,    1,   -8,   -3,    0,    1,   64,   21,    0,   10,   -8,   -6]
  K1(102,:) = [    1,   24,   -3,   -8,    1,    0,   21,   64,   10,    0,   -6,   -8]
  K1(103,:) = [   -8,   -3,   24,    1,   64,   21,    0,    1,   -8,   -6,    0,   10]
  K1(104,:) = [   -3,   -8,    1,   24,   21,   64,    1,    0,   -6,   -8,   10,    0]
  K1(105,:) = [   -3,   -8,    1,   24,    0,   10,   -8,   -6,    0,    1,   64,   21]
  K1(106,:) = [   -8,   -3,   24,    1,   10,    0,   -6,   -8,    1,    0,   21,   64]
  K1(107,:) = [    1,   24,   -3,   -8,   -8,   -6,    0,   10,   64,   21,    0,    1]
  K1(108,:) = [   24,    1,   -8,   -3,   -6,   -8,   10,    0,   21,   64,    1,    0]
  K1(109,:) = [   24,    8,    8,    3,    0,   -1,    8,    0,    0,    8,   -1,    0]
  K1(110,:) = [    8,    0,    6,    8,    8,    3,    3,   -1,  -64,  -24,  -24,  -10]
  K1(111,:) = [    8,    6,    0,    8,  -64,  -24,  -24,  -10,    8,    3,    3,   -1]
  K1(112,:) = [    3,    8,    8,   24,    0,    8,   -1,    0,    0,   -1,    8,    0]
  K1(113,:) = [    0,    8,  -64,    0, -192,  -64,  -64,  -24,   24,    8,    8,  -24]
  K1(114,:) = [   -1,    3,  -24,    8,  -64,    0,  -21,   -1,    8,    0,    6,  -10]
  K1(115,:) = [    8,    3,  -24,   -1,  -64,  -21,    0,   -1,    8,    6,    0,  -10]
  K1(116,:) = [    0,   -1,  -10,    0,  -24,   -1,   -1,   -3,  -24,  -10,  -10,  -30]
  K1(117,:) = [    0,  -64,    8,    0,   24,    8,    8,  -24, -192,  -64,  -64,  -24]
  K1(118,:) = [    8,  -24,    3,   -1,    8,    0,    6,  -10,  -64,    0,  -21,   -1]
  K1(119,:) = [   -1,  -24,    3,    8,    8,    6,    0,  -10,  -64,  -21,    0,   -1]
  K1(120,:) = [    0,  -10,   -1,    0,  -24,  -10,  -10,  -30,  -24,   -1,   -1,   -3]
  K1(121,:) = [  192,   64,   64,   24,    0,   -8,   64,    0,    0,   64,   -8,    0]
  K1(122,:) = [   64,  192,   24,   64,   -8,    0,    0,   64,   64,    0,    0,   -8]
  K1(123,:) = [   64,   24,  192,   64,   64,    0,    0,   -8,   -8,    0,    0,   64]
  K1(124,:) = [   24,   64,   64,  192,    0,   64,   -8,    0,    0,   -8,   64,    0]
  K1(125,:) = [    0,   -8,   64,    0,  192,   64,   64,   24,  -24,   -8,   -8,   24]
  K1(126,:) = [   -8,    0,    0,   64,   64,  192,   24,   64,   -8,  -24,   24,   -8]
  K1(127,:) = [   64,    0,    0,   -8,   64,   24,  192,   64,   -8,   24,  -24,   -8]
  K1(128,:) = [    0,   64,   -8,    0,   24,   64,   64,  192,   24,   -8,   -8,  -24]
  K1(129,:) = [    0,   64,   -8,    0,  -24,   -8,   -8,   24,  192,   64,   64,   24]
  K1(130,:) = [   64,    0,    0,   -8,   -8,  -24,   24,   -8,   64,  192,   24,   64]
  K1(131,:) = [   -8,    0,    0,   64,   -8,   24,  -24,   -8,   64,   24,  192,   64]
  K1(132,:) = [    0,   -8,   64,    0,   24,   -8,   -8,  -24,   24,   64,   64,  192]
  K1(133,:) = [    0,  -72,   -9,  -27,    0,   -9,    9,  -27,    0,   72,    9,   27]
  K1(134,:) = [  -72, -216,  -27,  -72,    9,    0,    0,  -72,  -72,    0,    0,    9]
  K1(135,:) = [   -9,  -27,    0,  -72,    9,  -27,    0,   -9,    9,   27,    0,   72]
  K1(136,:) = [  -27,  -72,  -72, -216,    0,  -72,    9,    0,    0,    9,  -72,    0]
  K1(137,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(138,:) = [   -9,    0,  -27,  -72,    9,    0,   27,   72,    9,    0,  -27,   -9]
  K1(139,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(140,:) = [  -27,  -72,   -9,    0,   27,   72,    9,    0,  -27,   -9,    9,    0]
  K1(141,:) = [    0,  -72,    9,    0,   27,    9,    9,  -27, -216,  -72,  -72,  -27]
  K1(142,:) = [   72,    0,   27,    9,    9,    0,   27,   -9,  -72,    0,  -27,   -9]
  K1(143,:) = [    9,    0,    0,  -72,    9,  -27,   27,    9,  -72,  -27, -216,  -72]
  K1(144,:) = [   27,    9,   72,    0,   27,   -9,    9,    0,  -27,   -9,  -72,    0]
  K1(145,:) = [    0,   -9,  -72,  -27,    0,    9,   72,   27,    0,    9,   -9,  -27]
  K1(146,:) = [   -9,    0,  -27,  -72,    9,    0,   27,   72,    9,    0,  -27,   -9]
  K1(147,:) = [  -72,  -27, -216,  -72,  -72,    0,    0,    9,    9,    0,    0,  -72]
  K1(148,:) = [  -27,  -72,  -72, -216,    0,  -72,    9,    0,    0,    9,  -72,    0]
  K1(149,:) = [    0,    9,  -72,    0, -216,  -72,  -72,  -27,   27,    9,    9,  -27]
  K1(150,:) = [    9,    0,    0,  -72,  -72, -216,  -27,  -72,    9,   27,  -27,    9]
  K1(151,:) = [   72,   27,    0,    9,  -72,  -27,    0,   -9,    9,   27,    0,   -9]
  K1(152,:) = [   27,   72,    9,    0,  -27,  -72,   -9,    0,   27,    9,   -9,    0]
  K1(153,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(154,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(155,:) = [   -9,  -27,    0,  -72,    9,  -27,    0,   -9,    9,   27,    0,   72]
  K1(156,:) = [  -27,   -9,  -72,    0,  -27,    9,   -9,    0,   27,    9,   72,    0]
  K1(157,:) = [ -216,  -72,  -72,  -27,    0,    9,  -72,    0,    0,  -72,    9,    0]
  K1(158,:) = [  -72,    0,  -27,   -9,   -9,    0,  -27,    9,   72,    0,   27,    9]
  K1(159,:) = [  -72,  -27, -216,  -72,  -72,    0,    0,    9,    9,    0,    0,  -72]
  K1(160,:) = [  -27,   -9,  -72,    0,  -27,    9,   -9,    0,   27,    9,   72,    0]
  K1(161,:) = [    0,   -9,  -72,  -27,    0,    9,   72,   27,    0,    9,   -9,  -27]
  K1(162,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(163,:) = [  -72,  -27,    0,   -9,   72,   27,    0,    9,   -9,  -27,    0,    9]
  K1(164,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(165,:) = [    0,   72,    9,   27,    0,    9,   -9,   27,    0,  -72,   -9,  -27]
  K1(166,:) = [  -72,    0,    0,    9,    9,   27,  -27,    9,  -72, -216,  -27,  -72]
  K1(167,:) = [    9,   27,    0,   72,   -9,   27,    0,    9,   -9,  -27,    0,  -72]
  K1(168,:) = [    0,    9,  -72,    0,  -27,    9,    9,   27,  -27,  -72,  -72, -216]
  K1(169,:) = [ -216,  -72,  -72,  -27,    0,    9,  -72,    0,    0,  -72,    9,    0]
  K1(170,:) = [  -72, -216,  -27,  -72,    9,    0,    0,  -72,  -72,    0,    0,    9]
  K1(171,:) = [  -72,  -27,    0,   -9,   72,   27,    0,    9,   -9,  -27,    0,    9]
  K1(172,:) = [  -27,  -72,   -9,    0,   27,   72,    9,    0,  -27,   -9,    9,    0]
  K1(173,:) = [    0,    9,   72,   27,    0,   -9,  -72,  -27,    0,   -9,    9,   27]
  K1(174,:) = [    9,    0,   27,   72,   -9,    0,  -27,  -72,   -9,    0,   27,    9]
  K1(175,:) = [  -72,    0,    0,    9,  -72,  -27, -216,  -72,    9,  -27,   27,    9]
  K1(176,:) = [    0,  -72,    9,    0,  -27,  -72,  -72, -216,  -27,    9,    9,   27]
  K1(177,:) = [    0,  -72,   -9,  -27,    0,   -9,    9,  -27,    0,   72,    9,   27]
  K1(178,:) = [  -72,    0,  -27,   -9,   -9,    0,  -27,    9,   72,    0,   27,    9]
  K1(179,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(180,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(181,:) = [  432,  144,  144,   54,    0,  -18,  144,    0,    0,  144,  -18,    0]
  K1(182,:) = [  144,  432,   54,  144,  -18,    0,    0,  144,  144,    0,    0,  -18]
  K1(183,:) = [  144,   54,  432,  144,  144,    0,    0,  -18,  -18,    0,    0,  144]
  K1(184,:) = [   54,  144,  144,  432,    0,  144,  -18,    0,    0,  -18,  144,    0]
  K1(185,:) = [    0,  -18,  144,    0,  432,  144,  144,   54,  -54,  -18,  -18,   54]
  K1(186,:) = [  -18,    0,    0,  144,  144,  432,   54,  144,  -18,  -54,   54,  -18]
  K1(187,:) = [  144,    0,    0,  -18,  144,   54,  432,  144,  -18,   54,  -54,  -18]
  K1(188,:) = [    0,  144,  -18,    0,   54,  144,  144,  432,   54,  -18,  -18,  -54]
  K1(189,:) = [    0,  144,  -18,    0,  -54,  -18,  -18,   54,  432,  144,  144,   54]
  K1(190,:) = [  144,    0,    0,  -18,  -18,  -54,   54,  -18,  144,  432,   54,  144]
  K1(191,:) = [  -18,    0,    0,  144,  -18,   54,  -54,  -18,  144,   54,  432,  144]
  K1(192,:) = [    0,  -18,  144,    0,   54,  -18,  -18,  -54,   54,  144,  144,  432]
  K1(193,:) = [ -216,  -72,  -72,  -27,    0,    9,  -72,    0,    0,  -72,    9,    0]
  K1(194,:) = [  -72,    0,  -27,   -9,    9,   27,    0,   72,    9,  -27,    0,   -9]
  K1(195,:) = [  -72,  -27, -216,  -72,  -72,    0,    0,    9,    9,    0,    0,  -72]
  K1(196,:) = [  -27,   -9,  -72,    0,    0,   72,    9,   27,    0,   -9,    9,  -27]
  K1(197,:) = [    0,    9,  -72,    0, -216,  -72,  -72,  -27,   27,    9,    9,  -27]
  K1(198,:) = [    9,   27,    0,   72,  -72,    0,  -27,   -9,    9,    0,   27,   -9]
  K1(199,:) = [  -72,    0,    0,    9,  -72,  -27, -216,  -72,    9,  -27,   27,    9]
  K1(200,:) = [    0,   72,    9,   27,  -27,   -9,  -72,    0,   27,   -9,    9,    0]
  K1(201,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(202,:) = [  -72,  -27,    0,   -9,    9,    0,  -27,   -9,    9,    0,   27,   72]
  K1(203,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(204,:) = [    0,   -9,  -72,  -27,  -27,   -9,    9,    0,   27,   72,    9,    0]
  K1(205,:) = [ -216,  -72,  -72,  -27,    0,    9,  -72,    0,    0,  -72,    9,    0]
  K1(206,:) = [  -72, -216,  -27,  -72,    9,    0,    0,  -72,  -72,    0,    0,    9]
  K1(207,:) = [  -72,  -27,    0,   -9,    9,    0,  -27,   -9,    9,    0,   27,   72]
  K1(208,:) = [  -27,  -72,   -9,    0,    0,    9,   -9,  -27,    0,    9,   72,   27]
  K1(209,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(210,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(211,:) = [  -72,    0,  -27,   -9,    9,   27,    0,   72,    9,  -27,    0,   -9]
  K1(212,:) = [    0,  -72,   -9,  -27,   27,    9,   72,    0,  -27,    9,   -9,    0]
  K1(213,:) = [    0,  -72,    9,    0,   27,    9,    9,  -27, -216,  -72,  -72,  -27]
  K1(214,:) = [  -72,    0,    0,    9,    9,   27,  -27,    9,  -72, -216,  -27,  -72]
  K1(215,:) = [    9,    0,   27,   72,    9,   27,    0,   -9,  -72,  -27,    0,   -9]
  K1(216,:) = [    0,    9,   72,   27,   27,    9,   -9,    0,  -27,  -72,   -9,    0]
  K1(217,:) = [    0,  -72,   -9,  -27,   27,    9,   72,    0,  -27,    9,   -9,    0]
  K1(218,:) = [  -72, -216,  -27,  -72,    9,    0,    0,  -72,  -72,    0,    0,    9]
  K1(219,:) = [   -9,  -27,    0,  -72,   72,    0,   27,    9,   -9,    0,  -27,    9]
  K1(220,:) = [  -27,  -72,  -72, -216,    0,  -72,    9,    0,    0,    9,  -72,    0]
  K1(221,:) = [   27,    9,   72,    0,    0,  -72,   -9,  -27,    0,    9,   -9,   27]
  K1(222,:) = [    9,    0,    0,  -72,  -72, -216,  -27,  -72,    9,   27,  -27,    9]
  K1(223,:) = [   72,    0,   27,    9,   -9,  -27,    0,  -72,   -9,   27,    0,    9]
  K1(224,:) = [    0,  -72,    9,    0,  -27,  -72,  -72, -216,  -27,    9,    9,   27]
  K1(225,:) = [  -27,  -72,   -9,    0,    0,    9,   -9,  -27,    0,    9,   72,   27]
  K1(226,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(227,:) = [   -9,    0,  -27,  -72,   -9,  -27,    0,    9,   72,   27,    0,    9]
  K1(228,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(229,:) = [    0,   -9,  -72,  -27,  -27,   -9,    9,    0,   27,   72,    9,    0]
  K1(230,:) = [   -9,    0,  -27,  -72,   -9,  -27,    0,    9,   72,   27,    0,    9]
  K1(231,:) = [  -72,  -27, -216,  -72,  -72,    0,    0,    9,    9,    0,    0,  -72]
  K1(232,:) = [  -27,  -72,  -72, -216,    0,  -72,    9,    0,    0,    9,  -72,    0]
  K1(233,:) = [  -27,   -9,  -72,    0,    0,   72,    9,   27,    0,   -9,    9,  -27]
  K1(234,:) = [   -9,  -27,    0,  -72,   72,    0,   27,    9,   -9,    0,  -27,    9]
  K1(235,:) = [    9,    0,    0,    9,    9,   27,   27,    9,    9,   27,   27,    9]
  K1(236,:) = [    0,    9,    9,    0,   27,    9,    9,   27,   27,    9,    9,   27]
  K1(237,:) = [   27,   72,    9,    0,    0,   -9,    9,   27,    0,   -9,  -72,  -27]
  K1(238,:) = [   72,   27,    0,    9,   -9,    0,   27,    9,   -9,    0,  -27,  -72]
  K1(239,:) = [    9,    0,    0,  -72,    9,  -27,   27,    9,  -72,  -27, -216,  -72]
  K1(240,:) = [    0,    9,  -72,    0,  -27,    9,    9,   27,  -27,  -72,  -72, -216]
  K1(241,:) = [    0,   81,   81,   54,    0,    0,  -81,    0,    0,  -81,    0,    0]
  K1(242,:) = [   81,    0,   54,   81,    0,    0,    0,  -81,  -81,    0,    0,    0]
  K1(243,:) = [   81,   54,    0,   81,  -81,    0,    0,    0,    0,    0,    0,  -81]
  K1(244,:) = [   54,   81,   81,    0,    0,  -81,    0,    0,    0,    0,  -81,    0]
  K1(245,:) = [    0,    0,  -81,    0, -243,  -81,  -81,  -54,    0,    0,    0,  -54]
  K1(246,:) = [    0,    0,    0,  -81,  -81, -243,  -54,  -81,    0,    0,  -54,    0]
  K1(247,:) = [  -81,    0,    0,    0,  -81,  -54, -243,  -81,    0,  -54,    0,    0]
  K1(248,:) = [    0,  -81,    0,    0,  -54,  -81,  -81, -243,  -54,    0,    0,    0]
  K1(249,:) = [    0,  -81,    0,    0,    0,    0,    0,  -54, -243,  -81,  -81,  -54]
  K1(250,:) = [  -81,    0,    0,    0,    0,    0,  -54,    0,  -81, -243,  -54,  -81]
  K1(251,:) = [    0,    0,    0,  -81,    0,  -54,    0,    0,  -81,  -54, -243,  -81]
  K1(252,:) = [    0,    0,  -81,    0,  -54,    0,    0,    0,  -54,  -81,  -81, -243]
  K1(253,:) = [  432,  144,  144,   54,    0,  -18,  144,    0,    0,  144,  -18,    0]
  K1(254,:) = [  144,  432,   54,  144,  -18,    0,    0,  144,  144,    0,    0,  -18]
  K1(255,:) = [  144,   54,  432,  144,  144,    0,    0,  -18,  -18,    0,    0,  144]
  K1(256,:) = [   54,  144,  144,  432,    0,  144,  -18,    0,    0,  -18,  144,    0]
  K1(257,:) = [    0,  -18,  144,    0,  432,  144,  144,   54,  -54,  -18,  -18,   54]
  K1(258,:) = [  -18,    0,    0,  144,  144,  432,   54,  144,  -18,  -54,   54,  -18]
  K1(259,:) = [  144,    0,    0,  -18,  144,   54,  432,  144,  -18,   54,  -54,  -18]
  K1(260,:) = [    0,  144,  -18,    0,   54,  144,  144,  432,   54,  -18,  -18,  -54]
  K1(261,:) = [    0,  144,  -18,    0,  -54,  -18,  -18,   54,  432,  144,  144,   54]
  K1(262,:) = [  144,    0,    0,  -18,  -18,  -54,   54,  -18,  144,  432,   54,  144]
  K1(263,:) = [  -18,    0,    0,  144,  -18,   54,  -54,  -18,  144,   54,  432,  144]
  K1(264,:) = [    0,  -18,  144,    0,   54,  -18,  -18,  -54,   54,  144,  144,  432]
  K1(265,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(266,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(267,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(268,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(269,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(270,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(271,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(272,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(273,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(274,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(275,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1(276,:) = [    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0]
  K1 = (1._/**/REALKIND / 9) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppttj_ttxbbxgg_1_/**/REALKIND



module ol_forced_parameters_ppttj_ttxbbxgg_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppttj_ttxbbxgg_1_/**/REALKIND

module ol_tree_ppttj_ttxbbxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(77)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 64 ! number of helicity configurations
  integer(intkind2), save :: nhel = 64 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(64) ! physical helicity states
  complex(DREALKIND) :: M1helarr(12,64) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*gQCD**4
    f(2) = gQCD**4

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,19))
  den(9) = 1 / (Q(5,28))
  den(12) = 1 / (Q(5,20) - MB2)
  den(13) = 1 / (Q(5,40) - MB2)
  den(16) = 1 / (Q(5,11) - MB2)
  den(21) = 1 / (Q(5,36) - MB2)
  den(22) = 1 / (Q(5,24) - MB2)
  den(25) = 1 / (Q(5,7) - MB2)
  den(31) = 1 / (Q(5,44))
  den(38) = 1 / (Q(5,52) - MB2)
  den(41) = 1 / (Q(5,17) - MT2)
  den(42) = 1 / (Q(5,34) - MT2)
  den(47) = 1 / (Q(5,14) - MT2)
  den(50) = 1 / (Q(5,33) - MT2)
  den(51) = 1 / (Q(5,18) - MT2)
  den(56) = 1 / (Q(5,13) - MT2)
  den(59) = 1 / (Q(5,35))
  den(67) = 1 / (Q(5,49) - MT2)

  ! denominators

  den(3) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(1)*den(12)
  den(15) = den(13)*den(14)
  den(17) = den(1)*den(16)
  den(18) = den(12)*den(17)
  den(19) = den(9)*den(12)
  den(20) = den(1)*den(19)
  den(23) = den(1)*den(21)
  den(24) = den(22)*den(23)
  den(26) = den(1)*den(25)
  den(27) = den(22)*den(26)
  den(28) = den(9)*den(22)
  den(29) = den(1)*den(28)
  den(30) = den(17)*den(21)
  den(32) = den(21)*den(31)
  den(33) = den(1)*den(32)
  den(34) = den(13)*den(26)
  den(35) = den(13)*den(31)
  den(36) = den(1)*den(35)
  den(37) = den(4)*den(26)
  den(39) = den(4)*den(38)
  den(40) = den(1)*den(39)
  den(43) = den(41)*den(42)
  den(44) = den(2)*den(43)
  den(45) = den(6)*den(41)
  den(46) = den(2)*den(45)
  den(48) = den(2)*den(47)
  den(49) = den(41)*den(48)
  den(52) = den(50)*den(51)
  den(53) = den(2)*den(52)
  den(54) = den(6)*den(51)
  den(55) = den(2)*den(54)
  den(57) = den(2)*den(56)
  den(58) = den(51)*den(57)
  den(60) = den(50)*den(59)
  den(61) = den(2)*den(60)
  den(62) = den(48)*den(50)
  den(63) = den(42)*den(59)
  den(64) = den(2)*den(63)
  den(65) = den(42)*den(57)
  den(66) = den(4)*den(57)
  den(68) = den(4)*den(67)
  den(69) = den(2)*den(68)
  den(70) = den(21)*den(45)
  den(71) = den(13)*den(45)
  den(72) = den(21)*den(54)
  den(73) = den(13)*den(54)
  den(74) = den(12)*den(60)
  den(75) = den(12)*den(63)
  den(76) = den(22)*den(60)
  den(77) = den(22)*den(63)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppttj_ttxbbxgg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppttj_ttxbbxgg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for top anti-top bottom anti-bottom glue glue -> 0
! I   = emitter, 0 means none (replace wave function I in the current crossing by the momentum MOM),
!       for I < 0 emitter for PowHeg's B^mu,nu
! MOM = external "polarisation vector" for gluon emitter
! nextcombs is the length of the array extcombs
! The elements of the array extcombs specify for which external particle combinations
!   the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
!   i=j=0 -> 0 means no colour insertion.
! M2munu = Spin correlated born squared amplitude in PowHeg format B^mu,nu for emitter -I
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND !, only: ci, parameters_status, ZERO, scalefactor, >masses<
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_init
  use ol_kinematics_/**/REALKIND, only: conv_mom_scatt2in, internal_momenta
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_data_types_/**/REALKIND
  use ol_helicity_bookkeeping_/**/REALKIND, only: &
    & helbookkeeping_wf, helsync, flip_phase
  use ol_helicity_init, only: helbookkeeping_flip, helsync_flip
  use ol_h_propagators_/**/REALKIND
  use ol_h_wavefunctions_/**/REALKIND
  use ol_h_vertices_/**/REALKIND
  use ol_h_contractions_/**/REALKIND
  use ol_external_ppttj_ttxbbxgg_1, only: external_perm_ppttj_ttxbbxgg_1, &
    & external_perm_inv_ppttj_ttxbbxgg_1, extcomb_perm_ppttj_ttxbbxgg_1, &
    & average_factor_ppttj_ttxbbxgg_1
  use ol_external_ppttj_ttxbbxgg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppttj_ttxbbxgg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppttj_ttxbbxgg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppttj_ttxbbxgg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, l, m, n
  real(REALKIND)    :: P(0:3,6)
  real(REALKIND)    :: extmasses2(6)
  real(REALKIND)    :: M2add(0:23-1), M2munuadd
  complex(REALKIND) :: MOM_LC(4), M1(12), M1helarray(12,64)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,19), wf8(8,34), wf16(16,8), wf64(64,38)

  type(polcont) :: A(64,38)
  complex(REALKIND) :: Aj(38)

  complex(REALKIND) :: omega(2) ! phases for helicity correlations

  call ensure_mp_init()

  if (hel_not_initialised) call hel_init()
  if (colmat_not_initialised) call colourmatrix_init()
  if (factors_status /= parameters_status) then
    ! Note: if factors_init would only be called when parameters changed which are relevant for the factors,
    ! a different 'status' would have to be used, because check_forced_parameters should be called after every parameter change.
    call check_forced_parameters()
    call factors_init()
  end if

  if (momenta_nan_check(P_scatt) /= 0) then
    M2 = 0
    return
  end if

  ! Convert 2 -> n-2 PS-point to n -> 0 (so that P(1) + ... + P(n) = 0)
  extmasses2 = [ rMT2, rMT2, rMB2, rMB2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppttj_ttxbbxgg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppttj_ttxbbxgg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppttj_ttxbbxgg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppttj_ttxbbxgg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rMT, H1, ex1)
  call wf_A(P(:,2), rMT, H2, ex2)
  call wf_Q(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_V(P(:,5), rZERO, H5, ex5)
  call wf_V(P(:,6), rZERO, H6, ex6)


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...
    call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H1, ex1, shift)
    call helbookkeeping_flip(H2, 2, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H2, ex2, shift)
    call helbookkeeping_flip(H3, 3, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H3, ex3, shift)
    call helbookkeeping_flip(H4, 4, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H4, ex4, shift)
    call helbookkeeping_flip(H5, 5, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H5, ex5, shift)
    call helbookkeeping_flip(H6, 6, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H6, ex6, shift)

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_GGG_G(ntry, wf4(:,1), ex5, ex6, wf16(:,1), n4(:,1), t4x16(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, wf4(:,1), wf16(:,2), n4(:,2), t4x16(:,:,2))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex5, wf16(:,3), n4(:,3), t4x16(:,:,3))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,3), n3(:,3), t3x4(:,:,3))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,2), Q(:,12), wf16(:,4), n3(:,4), t3x16(:,:,1))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex5, Q(:,16), wf8(:,1), n3(:,5), t3x8(:,:,1))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), ex6, Q(:,32), wf8(:,2), n3(:,6), t3x8(:,:,2))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), ex5, Q(:,16), wf8(:,3), n3(:,7), t3x8(:,:,3))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex6, Q(:,32), wf8(:,4), n3(:,8), t3x8(:,:,4))
  call vert_VQ_A(ntry, ex5, ex3, wf4(:,4), n3(:,9), t3x4(:,:,4))
  call vert_AV_Q(ntry, ex4, ex6, wf4(:,5), n3(:,10), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,4), Q(:,20), MB, 1_intkind1, wf4(:,6), n2(1))
  call prop_A_Q(ntry, wf4(:,5), Q(:,40), MB, 1_intkind1, wf4(:,7), n2(2))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,6), wf16(:,5), n3(:,11), t3x16(:,:,2))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,5), n3(:,12), t3x8(:,:,5))
  call vert_VQ_A(ntry, ex6, wf4(:,6), wf8(:,6), n3(:,13), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,5), Q(:,11), MB, 1_intkind1, wf8(:,7), n2(3))
  call vert_QA_V(ntry, wf4(:,6), ex4, wf8(:,8), n3(:,14), t3x8(:,:,7))
  call vert_VQ_A(ntry, ex6, ex3, wf4(:,8), n3(:,15), t3x4(:,:,6))
  call vert_AV_Q(ntry, ex4, ex5, wf4(:,9), n3(:,16), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,8), Q(:,36), MB, 1_intkind1, wf4(:,10), n2(4))
  call prop_A_Q(ntry, wf4(:,9), Q(:,24), MB, 1_intkind1, wf4(:,11), n2(5))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,10), wf16(:,6), n3(:,17), t3x16(:,:,3))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,9), n3(:,18), t3x8(:,:,8))
  call vert_AV_Q(ntry, wf4(:,11), ex6, wf8(:,10), n3(:,19), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,9), Q(:,7), MB, 1_intkind1, wf8(:,11), n2(6))
  call vert_QA_V(ntry, ex3, wf4(:,11), wf8(:,12), n3(:,20), t3x8(:,:,10))
  call vert_VQ_A(ntry, ex5, wf4(:,10), wf8(:,13), n3(:,21), t3x8(:,:,11))
  call vert_QA_V(ntry, wf4(:,10), ex4, wf8(:,14), n3(:,22), t3x8(:,:,12))
  call vert_AV_Q(ntry, wf4(:,7), ex5, wf8(:,15), n3(:,23), t3x8(:,:,13))
  call vert_QA_V(ntry, ex3, wf4(:,7), wf8(:,16), n3(:,24), t3x8(:,:,14))
  call vert_AV_Q(ntry, ex4, wf4(:,3), wf8(:,17), n3(:,25), t3x8(:,:,15))
  call vert_VQ_A(ntry, wf4(:,3), ex3, wf8(:,18), n3(:,26), t3x8(:,:,16))
  call prop_Q_A(ntry, wf8(:,18), Q(:,52), MB, 1_intkind1, wf8(:,19), n2(7))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,12), n3(:,27), t3x4(:,:,8))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,13), n3(:,28), t3x4(:,:,9))
  call prop_Q_A(ntry, wf4(:,12), Q(:,17), MT, 1_intkind1, wf4(:,14), n2(8))
  call prop_A_Q(ntry, wf4(:,13), Q(:,34), MT, 1_intkind1, wf4(:,15), n2(9))
  call vert_QA_V(ntry, wf4(:,14), wf4(:,15), wf16(:,7), n3(:,29), t3x16(:,:,4))
  call vert_QA_V(ntry, wf4(:,14), ex2, wf8(:,20), n3(:,30), t3x8(:,:,17))
  call vert_AV_Q(ntry, ex2, wf4(:,2), wf8(:,21), n3(:,31), t3x8(:,:,18))
  call vert_VQ_A(ntry, ex6, wf4(:,14), wf8(:,22), n3(:,32), t3x8(:,:,19))
  call prop_A_Q(ntry, wf8(:,21), Q(:,14), MT, 1_intkind1, wf8(:,23), n2(10))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,16), n3(:,33), t3x4(:,:,10))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,17), n3(:,34), t3x4(:,:,11))
  call prop_Q_A(ntry, wf4(:,16), Q(:,33), MT, 1_intkind1, wf4(:,18), n2(11))
  call prop_A_Q(ntry, wf4(:,17), Q(:,18), MT, 1_intkind1, wf4(:,19), n2(12))
  call vert_QA_V(ntry, wf4(:,18), wf4(:,19), wf16(:,8), n3(:,35), t3x16(:,:,5))
  call vert_QA_V(ntry, ex1, wf4(:,19), wf8(:,24), n3(:,36), t3x8(:,:,20))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,25), n3(:,37), t3x8(:,:,21))
  call vert_AV_Q(ntry, wf4(:,19), ex6, wf8(:,26), n3(:,38), t3x8(:,:,22))
  call prop_Q_A(ntry, wf8(:,25), Q(:,13), MT, 1_intkind1, wf8(:,27), n2(13))
  call vert_QA_V(ntry, wf4(:,18), ex2, wf8(:,28), n3(:,39), t3x8(:,:,23))
  call vert_VQ_A(ntry, ex5, wf4(:,18), wf8(:,29), n3(:,40), t3x8(:,:,24))
  call vert_QA_V(ntry, ex1, wf4(:,15), wf8(:,30), n3(:,41), t3x8(:,:,25))
  call vert_AV_Q(ntry, wf4(:,15), ex5, wf8(:,31), n3(:,42), t3x8(:,:,26))
  call vert_AV_Q(ntry, ex2, wf4(:,3), wf8(:,32), n3(:,43), t3x8(:,:,27))
  call vert_VQ_A(ntry, wf4(:,3), ex1, wf8(:,33), n3(:,44), t3x8(:,:,28))
  call prop_Q_A(ntry, wf8(:,33), Q(:,49), MT, 1_intkind1, wf8(:,34), n2(14))


  ! colour-stripped amplitudes
  do nsync = ntry+ntry-1, ntry+1  !  nsync = 1,2  for 1st point and nsync = 3 later
    call diagrams()
    if (nsync == 1) then
      call helsync(nsync, A, nhel, Hel)
      call helsync_flip(nsync, nhel, Hel, eflip, exthel)
    end if
  end do

  do k = 1, nhel
    call colourvector(A, k, M1helarray(:,k))
  end do
  M1helarray(:,nhel+1:) = 0
  M1helarr = M1helarray ! fill cache

  M2 = 0
  if (ReplacePol == 0) then ! no helicity correlation

    do k = 1, nhel
      call colint(M1helarray(:,k), M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do

  else ! helicity correlation

    call flip_phase(P(:,ReplacePol), firstpol(ReplacePol), MOM, omega)
    do k = 1, nhel
      M1 = M1helarray(:,k)
      r = eflip(k, ReplacePol) ! Flip helicity of external particle ReplacePol (gluon emitter).
      if (r <= nhel) then      ! Only add flipped helicity configuration if it doesn't vanish.
        M1 = M1 + omega(exthel(k,ReplacePol)) * M1helarray(:,r)
      end if
      call colint(M1, M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do
    M2 = 0.25_/**/REALKIND * M2

  end if

  if ( JBmunu /= 0 ) then ! Bmunu helicity correlation
    M2munu = 0
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppttj_ttxbbxgg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppttj_ttxbbxgg_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*6-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_VV(nsync, wf4(:,2), wf16(:,1), A(:,1), n3(:,45), t3x64(:,:,1), nhel, den(3))
    call cont_VV(nsync, wf4(:,2), wf16(:,2), A(:,2), n3(:,46), t3x64(:,:,2), nhel, den(3))
    call cont_VV(nsync, wf4(:,2), wf16(:,3), A(:,3), n3(:,47), t3x64(:,:,3), nhel, den(3))
    call cont_VV(nsync, wf4(:,3), wf16(:,4), A(:,4), n3(:,48), t3x64(:,:,4), nhel, den(5))
    call cont_VV(nsync, wf8(:,1), wf8(:,2), A(:,5), n3(:,49), t3x64(:,:,5), nhel, den(8))
    call cont_VV(nsync, wf8(:,3), wf8(:,4), A(:,6), n3(:,50), t3x64(:,:,6), nhel, den(11))
    call cont_QA(nsync, wf4(:,7), wf16(:,5), A(:,7), n3(:,51), t3x64(:,:,7), nhel, den(15))
    call cont_QA(nsync, wf8(:,6), wf8(:,7), A(:,8), n3(:,52), t3x64(:,:,8), nhel, den(18))
    call cont_VV(nsync, wf8(:,4), wf8(:,8), A(:,9), n3(:,53), t3x64(:,:,9), nhel, den(20))
    call cont_QA(nsync, wf4(:,11), wf16(:,6), A(:,10), n3(:,54), t3x64(:,:,10), nhel, den(24))
    call cont_QA(nsync, wf8(:,10), wf8(:,11), A(:,11), n3(:,55), t3x64(:,:,11), nhel, den(27))
    call cont_VV(nsync, wf8(:,4), wf8(:,12), A(:,12), n3(:,56), t3x64(:,:,12), nhel, den(29))
    call cont_QA(nsync, wf8(:,7), wf8(:,13), A(:,13), n3(:,57), t3x64(:,:,13), nhel, den(30))
    call cont_VV(nsync, wf8(:,1), wf8(:,14), A(:,14), n3(:,58), t3x64(:,:,14), nhel, den(33))
    call cont_QA(nsync, wf8(:,11), wf8(:,15), A(:,15), n3(:,59), t3x64(:,:,15), nhel, den(34))
    call cont_VV(nsync, wf8(:,1), wf8(:,16), A(:,16), n3(:,60), t3x64(:,:,16), nhel, den(36))
    call cont_QA(nsync, wf8(:,11), wf8(:,17), A(:,17), n3(:,61), t3x64(:,:,17), nhel, den(37))
    call cont_QA(nsync, wf8(:,5), wf8(:,19), A(:,18), n3(:,62), t3x64(:,:,18), nhel, den(40))
    call cont_VV(nsync, wf4(:,2), wf16(:,7), A(:,19), n3(:,63), t3x64(:,:,19), nhel, den(44))
    call cont_VV(nsync, wf8(:,2), wf8(:,20), A(:,20), n3(:,64), t3x64(:,:,20), nhel, den(46))
    call cont_QA(nsync, wf8(:,22), wf8(:,23), A(:,21), n3(:,65), t3x64(:,:,21), nhel, den(49))
    call cont_VV(nsync, wf4(:,2), wf16(:,8), A(:,22), n3(:,66), t3x64(:,:,22), nhel, den(53))
    call cont_VV(nsync, wf8(:,2), wf8(:,24), A(:,23), n3(:,67), t3x64(:,:,23), nhel, den(55))
    call cont_QA(nsync, wf8(:,26), wf8(:,27), A(:,24), n3(:,68), t3x64(:,:,24), nhel, den(58))
    call cont_VV(nsync, wf8(:,3), wf8(:,28), A(:,25), n3(:,69), t3x64(:,:,25), nhel, den(61))
    call cont_QA(nsync, wf8(:,23), wf8(:,29), A(:,26), n3(:,70), t3x64(:,:,26), nhel, den(62))
    call cont_VV(nsync, wf8(:,3), wf8(:,30), A(:,27), n3(:,71), t3x64(:,:,27), nhel, den(64))
    call cont_QA(nsync, wf8(:,27), wf8(:,31), A(:,28), n3(:,72), t3x64(:,:,28), nhel, den(65))
    call cont_QA(nsync, wf8(:,27), wf8(:,32), A(:,29), n3(:,73), t3x64(:,:,29), nhel, den(66))
    call cont_QA(nsync, wf8(:,21), wf8(:,34), A(:,30), n3(:,74), t3x64(:,:,30), nhel, den(69))
    call cont_VV(nsync, wf8(:,14), wf8(:,20), A(:,31), n3(:,75), t3x64(:,:,31), nhel, den(70))
    call cont_VV(nsync, wf8(:,16), wf8(:,20), A(:,32), n3(:,76), t3x64(:,:,32), nhel, den(71))
    call cont_VV(nsync, wf8(:,14), wf8(:,24), A(:,33), n3(:,77), t3x64(:,:,33), nhel, den(72))
    call cont_VV(nsync, wf8(:,16), wf8(:,24), A(:,34), n3(:,78), t3x64(:,:,34), nhel, den(73))
    call cont_VV(nsync, wf8(:,8), wf8(:,28), A(:,35), n3(:,79), t3x64(:,:,35), nhel, den(74))
    call cont_VV(nsync, wf8(:,8), wf8(:,30), A(:,36), n3(:,80), t3x64(:,:,36), nhel, den(75))
    call cont_VV(nsync, wf8(:,12), wf8(:,28), A(:,37), n3(:,81), t3x64(:,:,37), nhel, den(76))
    call cont_VV(nsync, wf8(:,12), wf8(:,30), A(:,38), n3(:,82), t3x64(:,:,38), nhel, den(77))

end subroutine diagrams


elemental function diagmap(j, n)
  implicit none
  integer, intent(in) :: j, n
  complex(REALKIND) :: diagmap
  diagmap = A(j,n)%j
end function diagmap

function diagsum(j, pos, neg)
  implicit none
  integer, intent(in) :: j, pos(:), neg(:)
  complex(REALKIND) :: diagsum
  diagsum = sum(diagmap(j, pos)) - sum(diagmap(j, neg))
end function diagsum

subroutine colourvector(A, j, M1)
  implicit none
  type(polcont) :: A(:,:)
  integer, intent(in) :: j
  complex(REALKIND), intent(out) :: M1(12) ! M1helarray(12,64)
  integer :: empty(0)

  M1( 1) = ((-A(j,35)%j-A(j,36)%j-A(j,37)%j-A(j,38)%j)*f(1))/6._/**/REALKIND
  M1( 2) = ((-A(j,1)%j+A(j,3)%j+A(j,5)%j+A(j,6)%j+A(j,10)%j+A(j,19)%j+A(j,31)%j+A(j,38)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,12)%j &
        -A(j,14)%j-A(j,20)%j+A(j,27)%j)*f(2))/2._/**/REALKIND
  M1( 3) = ((-A(j,1)%j+A(j,3)%j+A(j,5)%j+A(j,6)%j+A(j,7)%j+A(j,22)%j+A(j,34)%j+A(j,35)%j)*f(1))/2._/**/REALKIND+(CI*(-A(j,9)%j &
        +A(j,16)%j+A(j,23)%j-A(j,25)%j)*f(2))/2._/**/REALKIND
  M1( 4) = ((-A(j,31)%j-A(j,32)%j-A(j,33)%j-A(j,34)%j)*f(1))/6._/**/REALKIND
  M1( 5) = ((-A(j,22)%j-A(j,24)%j-A(j,26)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,29)%j-A(j,30)%j)*f(2))/6._/**/REALKIND
  M1( 6) = ((A(j,1)%j-A(j,2)%j-A(j,4)%j-A(j,5)%j+A(j,13)%j+A(j,24)%j+A(j,33)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,14)%j+A(j,18)%j &
        -A(j,23)%j+A(j,29)%j)*f(2))/2._/**/REALKIND
  M1( 7) = ((A(j,2)%j-A(j,3)%j+A(j,4)%j-A(j,6)%j+A(j,11)%j+A(j,26)%j+A(j,37)%j)*f(1))/2._/**/REALKIND+(CI*(-A(j,12)%j+A(j,17)%j &
        +A(j,25)%j+A(j,30)%j)*f(2))/2._/**/REALKIND
  M1( 8) = ((-A(j,10)%j-A(j,11)%j-A(j,13)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,17)%j-A(j,18)%j)*f(2))/6._/**/REALKIND
  M1( 9) = ((-A(j,19)%j-A(j,21)%j-A(j,28)%j)*f(1))/6._/**/REALKIND+(CI*(A(j,29)%j+A(j,30)%j)*f(2))/6._/**/REALKIND
  M1(10) = ((A(j,2)%j-A(j,3)%j+A(j,4)%j-A(j,6)%j+A(j,8)%j+A(j,28)%j+A(j,36)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,9)%j-A(j,18)%j &
        -A(j,27)%j-A(j,29)%j)*f(2))/2._/**/REALKIND
  M1(11) = ((A(j,1)%j-A(j,2)%j-A(j,4)%j-A(j,5)%j+A(j,15)%j+A(j,21)%j+A(j,32)%j)*f(1))/2._/**/REALKIND+(CI*(-A(j,16)%j-A(j,17)%j &
        +A(j,20)%j-A(j,30)%j)*f(2))/2._/**/REALKIND
  M1(12) = ((-A(j,7)%j-A(j,8)%j-A(j,15)%j)*f(1))/6._/**/REALKIND+(CI*(A(j,17)%j+A(j,18)%j)*f(2))/6._/**/REALKIND

end subroutine colourvector


! **********************************************************************
subroutine colint(M, M2colint, extcombs)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! The elements of the array extcombs specifies for which external particle
! combinations the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
! i=j=0 -> 0 means no colour insertion.
! **********************************************************************
  use ol_colourmatrix_ppttj_ttxbbxgg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(12)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 12*extcomb
    do i = 1, 12
      do j = 1, 12
        M2colint(extcomb) = M2colint(extcomb) + real(conjg(M(i))*K1(i+colmatpos,j)*M(j))
      end do
    end do
  end do

end subroutine colint


! **********************************************************************
subroutine colintmunu(M1, M2, M2colint)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! **********************************************************************
  use ol_colourmatrix_ppttj_ttxbbxgg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(12)
  complex(REALKIND), intent(in)  :: M2(12)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 12
    do j = 1, 12
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppttj_ttxbbxgg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(12,64)
  integer, intent(out) :: nhelout
  M1out = M1helarr
  nhelout = nhel
end subroutine colourvector
#endif


! =================================================== !
! Only interfaces for easier usage of AMP2_<procname> !
! =================================================== !

#ifdef PRECISION_dp
subroutine amp2tree(P, M2) &
    & bind(c,name="ol_f_amp2tree_ppttj_ttxbbxgg_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_ppttj_ttxbbxgg_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  if (J <= I) then
    extcomb = I*(I-1)/2 + J
  else
    extcomb = J*(J-1)/2 + I
  end if
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ extcomb ], M2munu)
  M2 = M2tmp(extcomb)
end subroutine amp2ccone


#ifdef PRECISION_dp
subroutine amp2ccall(P, M2) &
    & bind(c,name="ol_f_amp2ccall_ppttj_ttxbbxgg_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    23, [ (k, k = 0, 23-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_ppttj_ttxbbxgg_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  if (J <= I) then
    extcomb = I*(I-1)/2 + J
  else
    extcomb = J*(J-1)/2 + I
  end if
  call amp2(P, M2tmp, I, MOM, 1, [ extcomb ], M2munu)
  M2 = M2tmp(extcomb)
end subroutine amp2hcone


#ifdef PRECISION_dp
subroutine amp2hcall(P, M2, I, MOM) &
    & bind(c,name="ol_f_amp2hcall_ppttj_ttxbbxgg_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(6)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(6)
  do J = 1, 6
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 6,extcombs, M2munu)
  do J = 1, 6
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_ppttj_ttxbbxgg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_ppttj_ttxbbxgg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_ppttj_ttxbbxgg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_ppttj_ttxbbxgg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_j = j
  f_mom = mom
  call amp2hcone(f_p, f_m2, f_i, f_j, f_mom)
  m2 = f_m2
end subroutine amp2hcone_c

subroutine amp2hcall_c(p, m2, i, mom) &
    & bind(c,name="ol_amp2hcall_ppttj_ttxbbxgg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_c



! Only for compatibility with the old interface
subroutine amp2tree_legacy(p, m2) &
    & bind(c,name="amp2tree_ppttj_ttxbbxgg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_ppttj_ttxbbxgg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_ppttj_ttxbbxgg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_ppttj_ttxbbxgg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_j = j
  f_mom = mom
  call amp2hcone(f_p, f_m2, f_i, f_j, f_mom)
  m2 = f_m2
end subroutine amp2hcone_legacy

subroutine amp2hcall_legacy(p, m2, i, mom) &
    & bind(c,name="amp2hcall_ppttj_ttxbbxgg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_ppttj_ttxbbxgg_1_/**/REALKIND
