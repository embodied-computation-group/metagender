% fit metamodel

%% but first convert the data into the needed files with trials2counts

% load dataset
clear all 
clc

curdir = 'C:\Users\au723615\OneDrive - Aarhus universitet\Documents\University\RA\Projects\MetaGender\Analysis\metad\Matlab';
model_data = readtable('Hmetad_data.csv');
model_data = model_data(:,2:8);

%% Loop function over subjects to format data for modelling
firstcol = unique(model_data{:,1});
countF = 1;
countM = 1;

 for sub = 1:numel(firstcol)
    i = firstcol(sub);
    % for memory modality
    rowsm = find(model_data.subj == i & contains(model_data.mod, 'memory')); % select relevant rows
    [mem1, mem2] = trials2counts(model_data{rowsm, 5}, model_data{rowsm, 6}, model_data{rowsm, 7}, 7); % perform trials2counts
    if contains(model_data{rowsm, 2}, 'Feminin') % differing file names for men and women
    veclabelm1 = sprintf('mem_F_S1_%d', i); 
    veclabelm2 = sprintf('mem_F_S2_%d', i);
    elseif contains(model_data{rowsm, 2}, 'Masculin')
    veclabelm1 = sprintf('mem_M_S1_%d', i); 
    veclabelm2 = sprintf('mem_M_S2_%d', i);
    end
    fm1 = fullfile(curdir, 'counts', veclabelm1);
    fm2 = fullfile(curdir, 'counts', veclabelm2);
    save(fm1, 'mem1'); % save as file
    save(fm2, 'mem2'); 
    % save in struct 
    if contains(model_data{rowsm, 2}, 'Feminin') % differing file names for men and women
    DATAm(1).nR_S1{countF} = mem1;
    DATAm(1).nR_S2{countF} = mem2;
    elseif contains(model_data{rowsm, 2}, 'Masculin')
    DATAm(2).nR_S1{countM} = mem1;
    DATAm(2).nR_S2{countM} = mem2;    
    end
    
    % for vision modality
    rowsv = find(model_data.subj == i & contains(model_data.mod, 'vision'));
    [vis1, vis2] = trials2counts(model_data{rowsv, 5}, model_data{rowsv, 6}, model_data{rowsv, 7}, 7);
    if contains(model_data{rowsv, 2}, 'Feminin') % differing file names for men and women
    veclabelv1 = sprintf('vis_F_S1_%d', i); 
    veclabelv2 = sprintf('vis_F_S2_%d', i);
    elseif contains(model_data{rowsv, 2}, 'Masculin')
    veclabelv1 = sprintf('vis_M_S1_%d', i); 
    veclabelv2 = sprintf('vis_M_S2_%d', i);
    end
    fv1 = fullfile(curdir, 'counts', veclabelv1);
    fv2 = fullfile(curdir, 'counts', veclabelv2);
    save(fv1, 'vis1'); % save as file
    save(fv2, 'vis2'); 
    % save in struct 
    if contains(model_data{rowsv, 2}, 'Feminin') % differing file names for men and women
    DATAv(1).nR_S1{countF} = vis1;
    DATAv(1).nR_S2{countF} = vis2;
    elseif contains(model_data{rowsv, 2}, 'Masculin')
    DATAv(2).nR_S1{countM} = vis1;
    DATAv(2).nR_S2{countM} = vis2;    
    end
    
    % for gdp modality 
    rowsg = find(model_data.subj == i & contains(model_data.mod, 'GDP')); 
    [gdp1, gdp2] = trials2counts(model_data{rowsg, 5}, model_data{rowsg, 6}, model_data{rowsg, 7}, 7);
    if contains(model_data{rowsg, 2}, 'Feminin') % differing file names for men and women
    veclabelg1 = sprintf('gdp_F_S1_%d', i); 
    veclabelg2 = sprintf('gdp_F_S2_%d', i);
    elseif contains(model_data{rowsg, 2}, 'Masculin')
    veclabelg1 = sprintf('gdp_M_S1_%d', i); 
    veclabelg2 = sprintf('gdp_M_S2_%d', i);
    end
    fg1 = fullfile(curdir, 'counts', veclabelg1);
    fg2 = fullfile(curdir, 'counts', veclabelg2);
    save(fg1, 'gdp1'); % save as file
    save(fg2, 'gdp2'); 
    % save in struct 
    if contains(model_data{rowsg, 2}, 'Feminin') % differing file names for men and women
    DATAg(1).nR_S1{countF} = gdp1;
    DATAg(1).nR_S2{countF} = gdp2;
    elseif contains(model_data{rowsg, 2}, 'Masculin')
    DATAg(2).nR_S1{countM} = gdp1;
    DATAg(2).nR_S2{countM} = gdp2;    
    end
    
    
    % for calories modality
    rowsc = find(model_data.subj == i & contains(model_data.mod, 'Calories'));
    [cal1, cal2] = trials2counts(model_data{rowsc, 5}, model_data{rowsc, 6}, model_data{rowsc, 7}, 7);
    if contains(model_data{rowsc, 2}, 'Feminin') % differing file names for men and women
    veclabelc1 = sprintf('cal_F_S1_%d', i); 
    veclabelc2 = sprintf('cal_F_S2_%d', i);
    elseif contains(model_data{rowsc, 2}, 'Masculin')
    veclabelc1 = sprintf('cal_M_S1_%d', i); 
    veclabelc2 = sprintf('cal_M_S2_%d', i);
    end
    fc1 = fullfile(curdir, 'counts', veclabelc1);
    fc2 = fullfile(curdir, 'counts', veclabelc2);
    save(fc1, 'cal1'); % save as file
    save(fc2, 'cal2'); 
    % save in struct 
    if contains(model_data{rowsc, 2}, 'Feminin') % differing file names for men and women
    DATAc(1).nR_S1{countF} = cal1;
    DATAc(1).nR_S2{countF} = cal2;
    countF = countF + 1;
    elseif contains(model_data{rowsc, 2}, 'Masculin')
    DATAc(2).nR_S1{countM} = cal1;
    DATAc(2).nR_S2{countM} = cal2;
    countM = countM + 1;
    end 
        fprintf('participant %d completed \n', i);
 end
 
 
 %% create overall data set
firstcol = unique(model_data{:,1});
countF = 1;
countM = 1;

 for sub = 1:numel(firstcol)
    i = firstcol(sub);
    % for memory modality
    rowst = find(model_data.subj == i); % select relevant rows
    [tot1, tot2] = trials2counts(model_data{rowst, 5}, model_data{rowst, 6}, model_data{rowst, 7}, 7); % perform trials2counts
    if contains(model_data{rowst, 2}, 'Feminin') % differing file names for men and women
    veclabelt1 = sprintf('tot_F_S1_%d', i); 
    veclabelt2 = sprintf('tot_F_S2_%d', i);
    elseif contains(model_data{rowst, 2}, 'Masculin')
    veclabelt1 = sprintf('tot_M_S1_%d', i); 
    veclabelt2 = sprintf('tot_M_S2_%d', i);
    end
    ft1 = fullfile(curdir, 'counts', veclabelt1);
    ft2 = fullfile(curdir, 'counts', veclabelt2);
    save(ft1, 'tot1'); % save as file
    save(ft2, 'tot2'); 
    % save in struct 
    if contains(model_data{rowst, 2}, 'Feminin') % differing file names for men and women
    DATAt(1).nR_S1{countF} = tot1;
    DATAt(1).nR_S2{countF} = tot2;
    countF = countF + 1;
    elseif contains(model_data{rowst, 2}, 'Masculin')
    DATAt(2).nR_S1{countM} = tot1;
    DATAt(2).nR_S2{countM} = tot2;    
    countM = countM + 1;
    end
            fprintf('participant %d completed \n', i);
 end
 
 % fit model
fitMt = fit_meta_d_mcmc_group(DATAt(2).nR_S1, DATAt(2).nR_S2);

% Compute HDI of difference
sampleDifft = fitFt.mcmc.samples.mu_logMratio - fitMt.mcmc.samples.mu_logMratio;
hdit = calc_HDI(sampleDifft(:));
fprintf(['\n HDI on difference in log(meta-d''/d'') total : ', num2str(hdit) '\n\n'])

% plot
plotSamples(exp(fitFt.mcmc.samples.mu_logMratio))
plotSamples(exp(fitMt.mcmc.samples.mu_logMratio))
plotSamples(sampleDifft)

save('fitFt'); 
save('fitMt'); 
 %% fit metamodel
 % takes ages to run

 %% memory
fitFm = fit_meta_d_mcmc_group(DATAm(1).nR_S1, DATAm(1).nR_S2);
fitMm = fit_meta_d_mcmc_group(DATAm(2).nR_S1, DATAm(2).nR_S2);

% Compute HDI of difference
sampleDiffm = fitFm.mcmc.samples.mu_logMratio - fitMm.mcmc.samples.mu_logMratio;
hdim = calc_HDI(sampleDiffm(:));
fprintf(['\n HDI on difference in log(meta-d''/d'') memory : ', num2str(hdim) '\n\n'])

% plot
plotSamples(exp(fitFm.mcmc.samples.mu_logMratio))
plotSamples(exp(fitMm.mcmc.samples.mu_logMratio))
plotSamples(sampleDiffm)

 %% vision
fitFv = fit_meta_d_mcmc_group(DATAv(1).nR_S1, DATAv(1).nR_S2);
fitMv = fit_meta_d_mcmc_group(DATAv(2).nR_S1, DATAv(2).nR_S2);

% Compute HDI of difference
sampleDiffv = fitFv.mcmc.samples.mu_logMratio - fitMv.mcmc.samples.mu_logMratio;
hdiv = calc_HDI(sampleDiffv(:));
fprintf(['\n HDI on difference in log(meta-d''/d'') vision : ', num2str(hdiv) '\n\n'])

% plot
plotSamples(exp(fitFv.mcmc.samples.mu_logMratio))
plotSamples(exp(fitMv.mcmc.samples.mu_logMratio))
plotSamples(sampleDiffv)

 %% gdp
fitFg = fit_meta_d_mcmc_group(DATAg(1).nR_S1, DATAg(1).nR_S2);
fitMg = fit_meta_d_mcmc_group(DATAg(2).nR_S1, DATAg(2).nR_S2);
 
% Compute HDI of difference
sampleDiffg = fitFg.mcmc.samples.mu_logMratio - fitMg.mcmc.samples.mu_logMratio;
hdig = calc_HDI(sampleDiffg(:));
fprintf(['\n HDI on difference in log(meta-d''/d'') gdp : ', num2str(hdig) '\n\n'])

% plot
plotSamples(exp(fitFg.mcmc.samples.mu_logMratio))
plotSamples(exp(fitMg.mcmc.samples.mu_logMratio))
plotSamples(sampleDiffg)

 %% calories
fitFc = fit_meta_d_mcmc_group(DATAc(1).nR_S1, DATAc(1).nR_S2);
fitMc = fit_meta_d_mcmc_group(DATAc(2).nR_S1, DATAc(2).nR_S2);

% Compute HDI of difference
sampleDiffc = fitFc.mcmc.samples.mu_logMratio - fitMc.mcmc.samples.mu_logMratio;
hdic = calc_HDI(sampleDiffc(:));
fprintf(['\n HDI on difference in log(meta-d''/d'') calories : ', num2str(hdic) '\n\n'])

% plot
plotSamples(exp(fitFc.mcmc.samples.mu_logMratio))
plotSamples(exp(fitMc.mcmc.samples.mu_logMratio))
plotSamples(sampleDiffc) 

%% make plots

% first the catapillar plots
figure;

set(gcf, 'Position', [200 100 1200 400])

subplot(3,4,1:2)
plot(fitFt.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Total F');
box off
set(gca, 'FontSize', 8)

subplot(3,4,3:4)
plot(fitMt.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Total M');
box off
set(gca, 'FontSize', 8)

subplot(3,4,5)
plot(fitMm.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Memory F');
box off
set(gca, 'FontSize', 8)

subplot(3,4,6)
plot(fitMm.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Memory M');
box off
set(gca, 'FontSize', 8)

subplot(3,4,7)
plot(fitFv.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Vision F');
box off
set(gca, 'FontSize', 8)

subplot(3,4,8)
plot(fitMv.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Vision M');
box off
set(gca, 'FontSize', 8)

subplot(3,4,9)
plot(fitFg.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('GDP F');
box off
set(gca, 'FontSize', 8)

subplot(3,4,10)
plot(fitMg.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('GDP M');
box off
set(gca, 'FontSize', 8)

subplot(3,4,11)
plot(fitFc.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Calorie F');
box off
set(gca, 'FontSize', 8)

subplot(3,4,12)
plot(fitMc.mcmc.samples.mu_logMratio');
xlabel('Sample');
ylabel('Parameter');
title('Calorie M');
box off
set(gca, 'FontSize', 8)


sgtitle('Parameter distribution');
sgt.FontSize = 40;

%% now the distributions
figure;
set(gcf, 'Position', [200 20 1000 600])

subplot(3,2, 1:2)
histogram(sampleDifft(:), 'FaceColor', "#7F7F7F")
line(hdit,[-50,-50], 'LineWidth', 1, 'Color', "black", "marker", "|")
ylim([-200 1600])
xlabel('M-ratio difference') 
ylabel('Sample count') 
title('Total');
box off
set(gca, 'FontSize', 10)
set(gca,'fontname','times')

subplot(3,2,3)
histogram(sampleDiffm(:), 'FaceColor', "#7F7F7F")
line(hdim,[-50,-50], 'LineWidth', 1, 'Color', "black", "marker", "|")
ylim([-200 1600])
xlabel('M-ratio difference') 
ylabel('Sample count') 
title('Memory');
box off
set(gca, 'FontSize', 10)
set(gca,'fontname','times')

subplot(3,2,4)
histogram(sampleDiffv(:), 'FaceColor', "#7F7F7F")
line(hdiv,[-50,-50], 'LineWidth', 1, 'Color', "black", "marker", "|")
ylim([-200 1600])
xlabel('M-ratio difference') 
ylabel('Sample count') 
title('Vision');
box off
set(gca, 'FontSize', 10)
set(gca,'fontname','times')

subplot(3,2,5)
histogram(sampleDiffg(:), 'FaceColor', "#7F7F7F")
line(hdig,[-50,-50], 'LineWidth', 1, 'Color', "black", "marker", "|")
ylim([-200 1600])
xlabel('M-ratio difference') 
ylabel('Sample count') 
title('GDP');
box off
set(gca, 'FontSize', 10)
set(gca,'fontname','times')

subplot(3,2,6)
histogram(sampleDiffc(:), 'FaceColor', "#7F7F7F")
line(hdic,[-50,-50], 'LineWidth', 1, 'Color', "black", "marker", "|")
ylim([-200 1600])
xlabel('M-ratio difference') 
ylabel('Sample count') 
title('Calories');
box off
set(gca, 'FontSize', 10)
set(gca,'fontname','times')

sgtitle('M-ratio difference distribution (F-M)', 'FontName', 'times');
sgt.FontSize = 40;

%% save all fits
% mem
save('fitFm'); 
save('fitMm'); 

% vis
save('fitFv'); 
save('fitMv'); 

% gdp
save('fitFg'); 
save('fitMg'); 

% cal 
save('fitFc'); 
save('fitMc'); 

%% stats
% means
meant = mean(sampleDifft(:));
meanm = mean(sampleDiffm(:));
meanv = mean(sampleDiffv(:));
meang = mean(sampleDiffg(:));
meanc = mean(sampleDiffc(:));
% sd's
sdt = std(sampleDifft(:));
sdm = std(sampleDiffm(:));
sdv = std(sampleDiffv(:));
sdg = std(sampleDiffg(:));
sdc = std(sampleDiffc(:));

% build data frame
mods = ["group"; "mem"; "vis"; "gdp"; "cal"];
colnames = ["mod" "mean", "sd", "lower", "upper"];
stats = [meant sdt;meanm sdm;meanv sdv;meang sdg;meanc sdc];
stats = [colnames; mods stats hdis];