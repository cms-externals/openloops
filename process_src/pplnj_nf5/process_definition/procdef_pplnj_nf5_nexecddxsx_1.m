
FeynArtsProcess = {-F[1, {1}], F[2, {1}]} -> {-F[3, {2}], -F[4, {1}], F[4, {1}], F[4, {2}]};

SortExternal = True;

OpenLoopsModel = "SM";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs, Loops[5]},
  Adjacencies -> {3, 4}
};

InsertFieldsOptions = {
  Restrictions -> {ExcludeParticles -> {F[3, {3}]}, NoQuarkMixing},
  Model -> {"SMQCD", "SMQCDR2"},
  GenericModel -> "Lorentz",
  InsertionLevel -> {Particles}
};

UnitaryGauge = True;

ColourCorrelations = Automatic;

SubProcessName = Automatic;

SelectCoupling = MemberQ[{2}, Exponent[#1, eQED]] & ;

SelectInterference = {
  eQED -> {4}
};

SelectTreeDiagrams = True & ;

SelectLoopDiagrams = True & ;

SelectCTDiagrams = True & ;

ReplaceOSw = False;

SetParameters = {
  nf -> 5,
  ME -> 0,
  CKMORDER -> 0,
  nc -> 3,
  MU -> 0,
  MD -> 0,
  MS -> 0,
  MC -> 0,
  LeadingColour -> 0,
  POLSEL -> 1
};

ChannelMap = {
  {"nexeuccxdx"},
  {"nexeudxbbx<1,2,3,6,4,5>", "MB=0"}
};

Approximation = "";

ForceLoops = "t";

NonZeroHels = Null;

OnTheFlyMode = 0;
