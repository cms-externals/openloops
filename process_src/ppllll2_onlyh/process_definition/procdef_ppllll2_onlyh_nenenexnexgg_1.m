
FeynArtsProcess = {F[1, {1}], F[1, {1}]} -> {F[1, {1}], F[1, {1}], V[5], V[5]};

SortExternal = True;

OpenLoopsModel = "SM_yuksel";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs, Loops[5 | 6]},
  Adjacencies -> {3, 4}
};

InsertFieldsOptions = {
  Model -> {"SMQCD", "SMQCDR2"},
  GenericModel -> "Lorentz",
  InsertionLevel -> {Particles},
  Restrictions -> {ExcludeParticles -> {S[2 | 3]}, NoQuarkMixing}
};

UnitaryGauge = True;

ColourCorrelations = False;

SubProcessName = Automatic;

SelectCoupling = Exponent[#1, gQCD] == 2 && Exponent[#1, Yuk] == 1 & ;

SelectInterference = {
  eQED -> {8},
  Yuk -> {2}
};

SelectTreeDiagrams = True & ;

SelectLoopDiagrams = True & ;

SelectCTDiagrams = True & ;

ReplaceOSw = False;

SetParameters = {
  ME -> 0,
  YE -> 0,
  nc -> 3,
  nf -> 6,
  MU -> 0,
  MD -> 0,
  MS -> 0,
  MC -> 0,
  YU -> 0,
  YD -> 0,
  YS -> 0,
  YC -> 0,
  LeadingColour -> 0
};

ChannelMap = {};

Approximation = "onlyh";

ForceLoops = Automatic;

NonZeroHels = Null;
