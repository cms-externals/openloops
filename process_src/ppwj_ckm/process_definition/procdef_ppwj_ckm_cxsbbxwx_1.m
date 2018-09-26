
FeynArtsProcess = {-F[3, {2}], F[4, {2}]} -> {-F[4, {3}], F[4, {3}], V[3]};

SortExternal = True;

OpenLoopsModel = "SM_CKM";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs, Loops[6]},
  Adjacencies -> {3, 4}
};

InsertFieldsOptions = {
  Restrictions -> {ExcludeFieldPoints -> {}},
  Model -> {"SMQCD", "SMQCDR2"},
  GenericModel -> "Lorentz",
  InsertionLevel -> {Particles}
};

UnitaryGauge = True;

ColourCorrelations = Automatic;

SubProcessName = Automatic;

SelectCoupling = MemberQ[{1}, Exponent[#1, eQED]] & ;

SelectInterference = {
  eQED -> {2}
};

SelectTreeDiagrams = True & ;

SelectLoopDiagrams = True & ;

SelectCTDiagrams = True & ;

ReplaceOSw = False;

GenerateNpoint = {};

SetParameters = {
  CKMORDER -> 1,
  ME -> 0,
  nc -> 3,
  nf -> 6,
  MU -> 0,
  MD -> 0,
  MS -> 0,
  MC -> 0,
  LeadingColour -> 0,
  POLSEL -> 1
};

ChannelMap = {};

Approximation = "";

ForceLoops = "t";

NonZeroHels = Null;
