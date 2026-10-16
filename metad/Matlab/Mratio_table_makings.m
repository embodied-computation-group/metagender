% table for Arthur

% extract female Mratios
femm = fitFm.Mratio.';
femv = fitFv.Mratio.';
femg = fitFg.Mratio.';
femc = fitFc.Mratio.';

% extract male Mratios
masm = fitMm.Mratio.';
masv = fitMv.Mratio.';
masg = fitMg.Mratio.';
masc = fitMc.Mratio.';

% gender specification
genderf = repmat("F", 172,1);
genderm = repmat("M", 146,1);

% bring together
Mratiosf = table(genderf, femm, femv, femg, femc);
Mratiosm = table(genderm, masm, masv, masg, masc);
Mratiosf.Properties.VariableNames = ["gender", "memory", "vision", "GDP", "calories"];
Mratiosm.Properties.VariableNames = ["gender", "memory", "vision", "GDP", "calories"];

Mratios = vertcat(Mratiosf, Mratiosm);