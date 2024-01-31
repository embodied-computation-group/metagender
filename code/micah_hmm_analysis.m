function results = micah_hmm_analysis(condition)
% micah_hmm_analysis - Analyzes HMM results for given condition
%
% Usage:
%     results = micah_hmm_analysis(condition)
%
% Inputs:
%     condition - A string representing the condition (e.g., 'A', 'B', etc.)
%                 Default value is 'c' if not provided.
%
% Outputs:
%     results - A struct containing analysis results
%
% Example:
%     results = micah_hmm_analysis('A')
%

% Set default condition if not provided
if nargin < 1
    condition = 'c';
end

try
    femlabel = sprintf('fitF%s.mat', condition);
    mallabel = sprintf('fitM%s.mat', condition);

    fem_filename = whos('-file', femlabel);
    fem_namelabel = fem_filename.name;

    masc_filename = whos('-file', mallabel);
    masc_namelabel = masc_filename.name;

    % Load the .mat file
    loadedData1 = load(femlabel);
    loadedData2 = load(mallabel);

    % Access the struct field dynamically
    sample_diff = loadedData1.(fem_namelabel).mcmc.samples.mu_logMratio - ...
                  loadedData2.(masc_namelabel).mcmc.samples.mu_logMratio;

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
