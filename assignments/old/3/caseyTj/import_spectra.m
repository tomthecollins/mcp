% Copyright Tom Collins 3/4/2017

% Analyzed signal.
fid = fopen(fullfile('.', 'supporting', 'Xfb.js'));
TT = textscan(fid, '%s');
fclose(fid);
TT = TT{1};
% TT = TT(3:end);
X = zeros(82, 500);
k = 1;
end_idx = 0;
while end_idx < size(TT, 1)
  start_idx = 84*(k - 1) + 3;
  end_idx = 84*k;
  if end_idx < size(TT, 1)
    curr_col = TT(start_idx:end_idx);
    for l = 1:82
      X(l, k) = str2num(curr_col{l});
    end
    k=k+1;
  end
end
X = X(:, 1:k);

figure
imagesc(X)
colormap(1 - gray)
