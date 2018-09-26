
FeynArtsProcess = {F[3, {1}], -F[3, {1}]} -> {-F[4, {3}], F[4, {3}]};

SortExternal = True;

OpenLoopsModel = "SM";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs}
};

InsertFieldsOptions = {
  Restrictions -> {ExcludeParticles -> {S[2 | 3], SV}, NoQuarkMixing},
  Model -> {"SMQCD", "SMQCDR2"},
  InsertionLevel -> {Particles}
};

UnitaryGauge = True;

ColourCorrelations = Automatic;

SubProcessName = Automatic;

SelectCoupling = Exponent[#1, gQCD] == 2 + 2*#2 & ;

SelectInterference = False;

SelectTreeDiagrams = True & ;

SelectLoopDiagrams = True & ;

SelectCTDiagrams = True & ;

ReplaceOSw = False;

SetParameters = {
  nc -> 3,
  nf -> 6,
  ME -> 0,
  MM -> 0,
  ML -> 0,
  MU -> 0,
  MD -> 0,
  MS -> 0,
  MC -> 0,
  LeadingColour -> 0
};

ChannelMap = {};

Approximation = "";

NonZeroHels = Null;
