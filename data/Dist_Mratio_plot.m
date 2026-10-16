% distribution plots

% load data
curdir = 'C:\Users\au723615\OneDrive - Aarhus universitet\Documents\University\RA\Projects\MetaGender\Files';
load('fitFm.mat')
load('fitFv.mat')
load('fitFg.mat')
load('fitFc.mat')
load('fitFt.mat')
load('fitMm.mat')
load('fitMv.mat')
load('fitMg.mat')
load('fitMc.mat')
load('fitMt.mat')

%% now plot
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