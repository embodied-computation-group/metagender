data_dir = '/mnt/fast_scratch/projects/metagen/data';

files = dir(fullfile(data_dir, '*.mat'));

for n = 1:length(files)
   
results = micah_hmm_analysis(files(n).name)    
    
end