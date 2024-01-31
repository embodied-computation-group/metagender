% create correlation dataframe

% load original data
clear all 
clc

curdir = 'C:\Users\kelly\Documents\Universiteit\BMSN-CN2\Year 3\Internship abroad\Internship\Analysis_gender\metagen\analysis\Matlab';
model_data = readtable('Hmetad_data.csv');
model_data = model_data(:,2:8);

%% columns with subject number and gender, per gender
subgen = unique(model_data(:,1:2), 'rows');

frow = contains(subgen.gender, 'Feminin');
fsub = subgen(frow, 1:2);

mrow = contains(subgen.gender, 'Masculin');
msub = subgen(mrow, 1:2);

%% couple mratio
mratf = [fitFm.Mratio' fitFv.Mratio' fitFg.Mratio' fitFc.Mratio', fitFt.Mratio'];
mratm = [fitMm.Mratio' fitMv.Mratio' fitMg.Mratio' fitMc.Mratio', fitMt.Mratio'];

mratf = array2table(mratf);
mratm = array2table(mratm);

%% and now with the subject numbers
fem = [fsub mratf];
mas = [msub mratm];

fem.Properties.VariableNames = ["subj", "gender", "Mmem", "Mvis", "Mgdp", "Mcal", "Mtot"];
mas.Properties.VariableNames = ["subj", "gender", "Mmem", "Mvis", "Mgdp", "Mcal", "Mtot"];

mratio_data = [fem;mas];

writetable(mratio_data,  'mratio_data.csv')