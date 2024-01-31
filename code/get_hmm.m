data_dir = '/mnt/fast_scratch/projects/metagen/data'

% Assuming 'files' is the struct array obtained from the dir command
files = dir(fullfile(data_dir, 'fit*.mat')); % Update data_dir with your directory

% Initialize a container for the results
allResults = struct();

% Loop over the files
for i = 1:length(files)
    filename = files(i).name;
    
    % Check if the file is for a female (starts with 'fitF')
    if startsWith(filename, 'fitF')
        % Extract the condition identifier (e.g., 'M' from 'fitFM.mat')
        condition = filename(5:end-4);

        % Check if the male counterpart exists
        maleFilename = ['fitM', condition, '.mat'];
        if any(strcmp({files.name}, maleFilename))
            % Call the function for this condition
            fprintf('Analyzing condition: %s\n', condition);
            allResults.(condition) = analyzeHMMCondition(condition);
        end
    end
end

% Display or process allResults as needed
