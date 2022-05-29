% clear screen and workspace
clear; clc;

% illustrate three different shapes
% of frequency distributions

% draw 100 random samples of 5 digits
samples = random('Discrete Uniform', 10, [100,5]) - 1;

% frequency distribution of the largest digit
dist_max = max(samples,[],2);

% this distribution has the highest frequencies at 9 and 8
% ans is called negatively skew
figure('Name','Negatively skew')
h_max = histogram(dist_max);

% frequency distribution of the average of the
% largest and the smallest digits
dist_min = min(samples,[],2);
dist_avg = (dist_min + dist_max)/2;

% this distribution is symmetrical and gives good estimates
% of the average of 4.5 of all the digits
figure('Name','Symmetrical')
h_avg = histogram(dist_avg);

% frequency distribution of the sample range
% i.e. the largest minus the smallest digits
dist_range = dist_max - dist_min;

% this distribution is also skew negative
% but less than the first one
figure('Name','Negatively skew but less than the first')
h_range = histogram(dist_range);