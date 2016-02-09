
FeynArtsProcess = {F[3, {1}], -F[3, {1}]} -> {-F[4, {1}], F[4, {1}], -F[4, {2}], F[4, {2}], V[2]};

SortExternal = True;

OpenLoopsModel = "SM";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs},
  Adjacencies -> {3, 4}
};

InsertFieldsOptions = {
  Model -> {"SMQCD", "SMQCDR2"},
  GenericModel -> "Lorentz",
  InsertionLevel -> {Particles},
  Restrictions -> {ExcludeParticles -> {S[2 | 3]}, NoQuarkMixing}
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

SetParameters = {
  nc -> 3,
  nf -> 6,
  MU -> 0,
  MD -> 0,
  MS -> 0,
  MC -> 0,
  LeadingColour -> 0
};

ChannelMap = {
  {"uuxddxbbxz", "MB=0"}
};

Approximation = "";

ForceLoops = "t";

NonZeroHels = Null;
