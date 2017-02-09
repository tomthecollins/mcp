function p = empiricalProbability(P, cardinality, dimension,...
    empiricalMass, tfPitch)

% 9/5/2011 Copyright Tom Collins

% This function takes a pattern P of known cardinality and an empirical
% mass function. The sum of the probabilities of all permissible
% translations of P is returned. As durational patterns are not thought to
% have any permissible translations, the last argument tfPitch = 1 if P
% includes a dimension for pitch, and tfPitch = 0 otherwise.

% Example pattern and empirical mass.
% P = [3 71 1/2; 7/2 70 1/2];
% cardinality = 2;
% dimension = 3;
% empiricalMass = [62 1/2 1 1/16; 63 1/2 1 1/16; 65 1 1 1/16;...
%     68 1/2 2 1/8; 70 1/2 2 1/8; 70 3/2 2 1/8; 71 1/2 3 3/16;...
%     73 1 2 1/16; 75 1/2 2 1/16];
% tfPitch = 1;


m = dimension;
% Strip pattern of its ontime values.
S = P(:,2:m);

if tfPitch
    % Determine permissible translations of S.
    minPatternPitch = min(S(:,1));
    maxPatternPitch = max(S(:,1));
    minDatasetPitch = empiricalMass(1,1);
    maxDatasetPitch = empiricalMass(end,1);
    k = minDatasetPitch - minPatternPitch;
    K = maxDatasetPitch - maxPatternPitch;
    p = 0;
    i = k; % Increment from k to K.
    while i <= K
        if m == 3
            x = [i 0];
        else
            x = i;
        end
        % Create translation of S.
        STrans = S + x(ones(cardinality,1),:);
        % Add its likelihood.
        p = p + exp(empiricalLogProbability(STrans, cardinality,...
            dimension-1, empiricalMass));
        i=i+1;
    end
else
    p = exp(empiricalLogProbability(S, cardinality, dimension-1,...
        empiricalMass));
end