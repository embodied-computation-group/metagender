function results = analyzeHMMCondition(condition)
% analyzeHMMCondition - Analyzes HMM results for a given condition
%
% Usage:
%     results = analyzeHMMCondition(condition)
%
% Inputs:
%     condition - A string representing the condition (e.g., 'M', 'g', etc.)
%
% Outputs:
%     results - A struct containing analysis results
%
% Example:
%     results = analyzeHMMCondition('M')
%

% Validate the input
if nargin < 1 || isempty(condition) || ~ischar(condition)
    error('Condition must be a non-empty string.');
end

try
    % Construct the filenames
    femaleFile = sprintf('fitF%s.mat', condition);
    maleFile = sprintf('fitM%s.mat', condition);

    % Load the .mat files
    loadedDataF = load(femaleFile);
    loadedDataM = load(maleFile);

    % Extract variable names from the .mat files
    varsF = fieldnames(loadedDataF);
    varsM = fieldnames(loadedDataM);

    % Access the first (and presumably only) field from each struct
    sample_diff = loadedDataF.(varsF{1}).mcmc.samples.mu_logMratio - ...
                  loadedDataM.(varsM{1}).mcmc.samples.mu_logMratio;

    % Calculate HDI
    hdic = calc_HDI(sample_diff(:));

    % Calculate p-value and posterior probability
    pvalue = (1 - (sum(sample_diff(:) > 0) / numel(sample_diff))) * 2;
    pp = (sum(sample_diff(:) > 0) / numel(sample_diff));

    results.hdic = hdic;
    results.pvalue = pvalue;
    results.pp = pp;
    results.sample_diff = sample_diff;

    % Display HDI
    figure;
    histogram(sample_diff);
    fprintf(['\n HDI on difference in log(meta-d''/d'') calories : ', num2str(hdic) '\n\n'])

catch ME
    % Error handling
    error('An error occurred: %s', ME.message);
end

end
