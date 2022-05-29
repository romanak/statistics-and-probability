% clear screen and workspace
clear; clc;

% for a large population in which 30% favor some proposal,
% draw a set of 200 random samples of size 10
samples = random('Discrete Uniform', 10, [200,10]);

% appearance of any of the digits 1,2,3 represents
% someone favoring the proposal
samples = ismember(samples, [1,2,3]);
favors = sum(samples, 2);

% plot a histogram of the distribution
h = histogram(favors);

% proportion of sample estimate are correct to within +/- 10%
correct10 = histcounts(favors,[1.5,4.5]) / 200;

% theoretical proportion is 0.7
disp(correct10)