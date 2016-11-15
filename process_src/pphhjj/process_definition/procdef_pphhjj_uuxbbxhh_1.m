
FeynArtsProcess = {F[3, {1}], -F[3, {1}]} -> {-F[4, {3}], F[4, {3}], S[1], S[1]};

SortExternal = True;

OpenLoopsModel = "SM_yuksel";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs},
  Adjacencies -> {3, 4}
};

InsertFieldsOptions = {
  Model -> {"SMQCD", "SMQCDR2"},
  GenericModel -> "Lorentz",
  InsertionLevel -> {Particles},
  Restrictions -> {ExcludeParticles -> {}, NoQuarkMixing}
};

UnitaryGauge = False;

ColourCorrelations = Automatic;

SubProcessName = Automatic;

SelectCoupling = Exponent[#1, eQED] == 4 & ;

SelectInterference = {
  eQED -> {8}
};

SelectTreeDiagrams = True & ;

SelectLoopDiagrams = True & ;

SelectCTDiagrams = True & ;

ReplaceOSw = False;

SetParameters = {
  YukB -> 1,
  YukT -> 1,
  CKMORDER -> 0,
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
  LeadingColour -> 0,
  POLSEL -> 1
};

ChannelMap = {};

Approximation = "";

ForceLoops = Automatic;

NonZeroHels = Null;
