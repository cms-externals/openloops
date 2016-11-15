
FeynArtsProcess = {F[3, {1}], -F[3, {1}]} -> {V[1], V[2], V[5]};

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
  Restrictions -> {ExcludeParticles -> {S[2 | 3]}, NoQuarkMixing}
};

UnitaryGauge = True;

ColourCorrelations = False;

SubProcessName = Automatic;

SelectCoupling = MemberQ[{2}, Exponent[#1, eQED]] & ;

SelectInterference = {
  eQED -> {4}
};

SelectTreeDiagrams = False & ;

SelectLoopDiagrams = ContainsFermionLoop[##1] && NFieldPropagators[F[3 | 4], 0, "Type" -> Internal][##1] & ;

SelectCTDiagrams = NFieldPropagators[V[5], 1];

ReplaceOSw = False;

SetParameters = {
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

ChannelMap = {
  {"ddxazg"},
  {"bbxazg", "MB=0", "YB=0"}
};

Approximation = "";

ForceLoops = Automatic;

NonZeroHels = Null;
