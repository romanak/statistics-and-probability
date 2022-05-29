% clear screen and workspace
clear; clc;

% the number of random digits to encounter zero

distribution = zeros([1,50]);

for i=1:50
    j = 0;
    while random('Discrete Uniform', 10) - 1 ~= 0
        j = j+1;
    end
    distribution(i) = j;
end

h = histogram(distribution);

avg_n = mean(distribution);
disp(avg_n)