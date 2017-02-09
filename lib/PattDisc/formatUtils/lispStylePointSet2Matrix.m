function D = lispStylePointSet2Matrix(fname, k)

% Copyright Tom Collins 20/6/2013

% This function parses a list of the form
%
% (0 60 60 1/3 0)
% (0 61 60 2/3 1)
% ...
%
% and converts it to a matrix
% 
% [0 60 60 1/3 0; 0 61 60 2/3 1;...];

% INPUT
%  fname is the string for the list, typically stored as a text file.
%  k is the dimension of the point set that the list represents (e.g., the
%   number of elements per row).

if nargin < 2
  k = 5;
end

% Make format string according to k.
fstring = '(';
% fstring = [];
for i = 1:k
  fstring = [fstring '%s'];
end

fid = fopen(fname);
A = textscan(fid, fstring);
fclose(fid);

% Remove final brackets and convert to matrix.
n = size(A{1}, 1);
D = zeros(n, k);
for i = 1:n
  for j = 1:k - 1
    D(i, j) = str2num(A{j}{i});
  end
  if strcmp(A{k}{i}(end), ')')
    a = A{k}{i}(1:end-1);
  else
    a = A{k}{i};
  end
  D(i, k) = str2num(a);
end
