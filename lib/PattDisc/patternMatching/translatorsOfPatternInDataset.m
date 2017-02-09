function translators = translatorsOfPatternInDataset(P, D)

% 27/3/2011 Copyright Tom Collins

% This function takes a pattern P and a dataset D as its arguments.
% Generally it will be the case that P is a subset of D, and the function
% translatorsOfPatternInDataset is being used to find other occurrences
% (translations) of P in D. This is an implemented solution to P1 as
% described by Ukkonen, Lemstrom, and Makinen (2003).

% It is assumed that P and D are in lexicographic order. Otherwise
% D = sortrows(D);
% can be used to achieve this. Alternatively,
% D = unique(D, 'rows');
% will also remove duplicate datapoints.

% INPUT
%  P is a k-dimensional set of l points.
%  D is a k-dimensional set of n points.

% EXAMPLE INPUT
% P = [0 0; 0 3; 1 2];
% D = [2 2; 2 4; 3 4; 3 5; 6 4; 6 5; 6 7; 7 6; 8 4; 9 2; 9 5; 10 3; 10 4];

% Cardinalities of P and D.
m = size(P,1);
n = size(D,1);

% Cover trivial scenarios.
if m > n % If P contains more points than D, then D cannot contain any
         % translations of P.
    translators = [];
    executeIteration = 0;
elseif m == n % If P and D are the same size, it may be that D is a
              % translation of P.
    Q = D - P;
    q = Q(1,:);
    if Q(2:m,:) == q(ones(m-1,1),:)
        translators = q;
    else
        translators = [];
    end
    executeIteration = 0;
elseif m == 1 % If P contains only one point, then there are as many
    % translators as there are points in D.
    translators = D - P(ones(n,1),:);
    executeIteration = 0;
else
    executeIteration = 1;
end

% DEBUG!
% test_var = [];

% For nontrivial scenarios.
if executeIteration
    d = size(D,2); % Dimension of space.
    translators = zeros(n,d);
    r = 1; % Increment over translators.
    j = 1; % Increment over dataset.
    while j <= n - m + 1
        f = D(j,:) - P(1,:);
        i = 2; % Increment over pattern.
        k = 1; % Increment over successfully matched points.
        J = j; % Increment over dataset, allowing for backtracking.
        while i <= m && J <= n
            q = D(J,:);
            if max(abs(q - (P(i,:) + f))) < 1e-5 % Test equality with
                                                 % tolerance for error.
                k=k+1;
                i=i+1;
                J=J+1;
            else
                if lexLess(q, P(i,:) + f)
                    J=J+1;
                else
                    i=m+1;
                end
            end
        end
        if k == m
            translators(r,:) = f;
            r=r+1;
        end
        j=j+1;
    end
    if r == 1
        translators = [];
    else
        translators = translators(1:r-1,:);
    end
end
