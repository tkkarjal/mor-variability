function [cluster_map,cluster_signal,k] = create_clusters(bp_image,mni_atlas,ncutoff)
% Performs binding-potential-based subdivision of anatomical ROIs, as
% described in the paper ###.
%
% Inputs:
%
%   - bp_image       = an image file representing spatial distribution of
%                      a tracer's binding potential in the brain
%   - mni_atlas      = ROI atlas in MNI-space. The subdivision will be done
%                      for every (sufficiently large, as defined by ncutoff)
%                      ROI that is defined in the atlas.
%   - ncutoff        = Determines the size of ROIs that will not be subdivided
%                      because the original ROI is already sufficiently small.
%
%
% Outputs:
% 
%   - cluster_map    = the new ROI atlas
%   - cluster_signal = mean binding potential within each of the new ROIs
%   - k              = number of produced ROIs
%
% Tomi Karjalainen, October 9th 2019

if(ischar(bp_image))
    V = spm_vol(bp_image);
    bp_image = spm_read_vols(V);
    clear V
end

if(ischar(mni_atlas))
    V = spm_vol(mni_atlas);
    mni_atlas = spm_read_vols(V);
    clear V
end

labels = setdiff(unique(mni_atlas(:)),0);
cluster_map = zeros(size(mni_atlas));
k = 0;

for i = 1:length(labels)
    original_mask = mni_atlas == labels(i);
    nvoxels = sum(original_mask(:));
    ndiv = ceil(nvoxels/ncutoff);
    splitted_masks = roi_split(original_mask,bp_image,ndiv);
    for j = 1:ndiv
        k = k + 1;
        mask = logical(splitted_masks(:,:,:,j));
        cluster_map(mask) = k;
    end
end

cluster_signal = zeros(size(cluster_map));

for i = 1:k
    idx = cluster_map == i;
    cluster_signal(idx) = mean(bp_image(idx),'omitnan');
end

end

function masks = roi_split(original_mask,mean_img,ndiv)

original_idx = find(original_mask);
X = mean_img(original_mask);

try
    Z = linkage(X,'ward','euclidean');
catch
    Z = linkage(X,'ward','euclidean','savememory','on');
end

c = cluster(Z,'maxclust',ndiv);
masks = zeros([size(mean_img) ndiv]);

for i = 1:ndiv
    idx = c == i;
    mask = zeros(size(mean_img));
    mask(original_idx(idx)) = 1;
    masks(:,:,:,i) = mask;
end

end