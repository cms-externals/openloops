
FeynArtsProcess = {-F[3, {1}], F[3, {2}]} -> {F[3, {2}], -F[4, {1}], V[3]};

SortExternal = True;

OpenLoopsModel = "SM";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs}
};

InsertFieldsOptions = {
  Restrictions -> {ExcludeParticles -> {S[2 | 3]}, NoQuarkMixing},
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
  MU -> 0,
  MD -> 0,
  MS -> 0,
  MC -> 0,
  LeadingColour -> 0
};

ChannelMap = {};

Approximation = "";

NonZeroHels = Null;
