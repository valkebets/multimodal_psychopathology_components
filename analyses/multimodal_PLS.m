%% Multimodal PLS
%
% 
% 1. Load data
% 2. Regress out confounds
% 3. Reduce imaging data dimensionality with PCA
% 4. PLS analysis 


clear
close all
clc
 

addpath(fullfile(getenv('CBIG_CODE_DIR')
addpath(fullfile(getenv('CBIG_CODE_DIR','external_packages','matlab','non_default_packages','PLS_MIPlab')))


data_dir = '/Users/valkebets/Documents/Work/project-data/paper__ABCD_psychopathology-dimensions/Manuscript/Submissions/NDA_study/analyses_data'


%% 1. Load data

% Load subject list
[subjects, ~] = CBIG_text2cell(fullfile(data_dir,'subjects_lists','subjects_discovery.csv'));

% Load behavior data
load(fullfile(data_dir,'behavior_data.mat'));

% Load imaging data
load(fullfile(data_dir,'surface_area.mat'));
load(fullfile(data_dir,'thickness.mat'));
load(fullfile(data_dir,'volume.mat'));
load(fullfile(data_dir,'RSFC.mat'));

% Number of principal components to keep will depend on % explained variance
pca_threshold = 50;


%% 2. Regress out confounds 

% Load confounds
load(fullfile(data_dir,'confounds.mat'));

% Regress out confounds from each modality

% Behavior data
clear confounds
confounds = [age age2 sex site ethnicity];
[cbcl_reg, ~, ~, ~] = CBIG_glm_regress_matrix(cbcl, confounds, 0, []);

% Surface area data
clear confounds
confounds = [age age2 sex site ethnicity total_surface_area];
[surf_area_reg, ~, ~, ~] = CBIG_glm_regress_matrix(surface_area, confounds, 0, []);

% Thickness data
clear confounds
confounds = [age age2 sex site ethnicity ICV];
[thickness_reg, ~, ~, ~] = CBIG_glm_regress_matrix(thickness, confounds, 0, []);

% Volume data
clear confounds
confounds = [age age2 sex site ethnicity ICV];
[volume_reg, ~, ~, ~] = CBIG_glm_regress_matrix(volume, confounds, 0, []);

% RSFC data
clear confounds
confounds = [age age2 sex site ethnicity meanFD DVARS];
[rsfc_reg, ~, ~, ~] = CBIG_glm_regress_matrix(RSFC, confounds, 0, []);


%% 3. Compute PCA over imaging data modality

% PCA on surface area data
clear pca_scores pca_explained
surf_area_z = zscore(surf_area_reg);
[~, pca_scores, ~, ~, pca_explained] = pca(surf_area_z, 'Centered', false);
num_pc = (find(cumsum(pca_explained) >= pca_threshold, 1));
surf_area_pca = pca_scores(:, 1:num_pc);  

% PCA on thickness data
clear pca_scores pca_explained
thickness_z = zscore(thickness_reg);
[~, pca_scores, ~, ~, pca_explained] = pca(thickness_z, 'Centered', false);
num_pc = (find(cumsum(pca_explained) >= pca_threshold, 1));
thickness_pca = pca_scores(:, 1:num_pc);  

% PCA on volume data
clear pca_scores pca_explained
volume_z = zscore(volume_reg);
[~, pca_scores, ~, ~, pca_explained] = pca(volume_z, 'Centered', false);
num_pc = (find(cumsum(pca_explained) >= pca_threshold, 1));
volume_pca = pca_scores(:, 1:num_pc);  

% PCA on RSFC data 
clear pca_scores pca_explained
rsfc_z = zscore(rsfc_reg);
[~, pca_scores, ~, ~, pca_explained] = pca(rsfc_z, 'Centered', false);
num_pc = (find(cumsum(pca_explained) >= pca_threshold, 1));
rsfc_pca = pca_scores(:, 1:num_pc);  


%% 4. PLS analysis
    
Y = cbcl_reg;
X = [surf_area_pca thickness_pca volume_pca rsfc_pca];


[U, S, V, Lx, Ly, explCovLC, behav_loadings, pca_img_loadings] = ...
    myPLS_analysis(X_reg, Y_reg, 1, 1);

% Re-compute loadings in original space
surf_area_loadings = corr(Lx, surface_area_reg);
thickness_loadings = corr(Lx, thickness_reg);
volume_loadings = corr(Lx, volume_reg);
RSFC_loadings = corr(Lx, rsfc_reg);


% Permutation testing (while accounting for site)

pvals_LC = myPLS_permut(X, Y, U, S, 10000, site, 1, 1, 1)


