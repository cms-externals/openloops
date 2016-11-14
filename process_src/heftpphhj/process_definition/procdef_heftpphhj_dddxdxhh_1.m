
FeynArtsProcess = {F[4, {1}], F[4, {1}]} -> {F[4, {1}], F[4, {1}], S[1], S[1]};

SortExternal = True;

OpenLoopsModel = "HEFT";

CreateTopologiesOptions = {
  ExcludeTopologies -> {Snails, WFCorrectionCTs, TadpoleCTs},
  Adjacencies -> {3, 4, 5, 6}
};

InsertFieldsOptions = {
  Model -> {"HEFT", "HEFTR2"},
  GenericModel -> "HEFT",
  InsertionLevel -> {Particles},
  Restrictions -> {ExcludeParticles -> {S[2 | 3], F[3, {3}]}, NoQuarkMixing}
};

UnitaryGauge = True;

ColourCorrelations = Automatic;

SubProcessName = Automatic;

SelectCoupling = Exponent[#1, gQCD] == 4 && Exponent[#1, eQED] == 2 & ;

SelectInterference = {
  eQED -> {4}
};

SelectTreeDiagrams = True & ;

SelectLoopDiagrams = True & ;

SelectCTDiagrams = True & ;

ReplaceOSw = False;

SetParameters = {
  MB -> 0,
  YB -> 0,
  nc -> 3,
  nf -> 5,
  MU -> 0,
  MD -> 0,
  MS -> 0,
  MC -> 0,
  LeadingColour -> 0,
  POLSEL -> 1
};

ChannelMap = {
  {"bbbxbxhh", "MB=0"},
  {"uuuxuxhh"}
};

Approximation = "";

ForceLoops = "t";

NonZeroHels = Null;
