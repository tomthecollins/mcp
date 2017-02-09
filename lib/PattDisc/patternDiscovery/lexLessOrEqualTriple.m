function tf = lexLessOrEqualTriple(dj, v, di, k)

% Copyright Tom Collins 27/3/2011

% This function is a special version of the function lexLessOrEqual, and is
% useful when testing whether dj + v is lexicographically less than or
% equal to di. It is faster to check each element of dj + v and di in turn,
% rather than calculate dj + v first.

% The function returns 1 if dj + v is 'less than' di, where 'less than' is
% the lexicographic ordering. It returns 0 otherwise.

% In general, for two vectors u and w, this function finds the first index
% i such that u(i) is not equal to w(i). If u(i) is less than w(i), then u
% is 'less than' w. If w(i) is less than u(i), then w is 'less than' u. The
% other possible outcome is that u equals w.

% For example, with u and w as defined below, u is 'less than' w. But if u
% had 2 instead of 1 as its second argument, then w would be 'less than' u.

% Example vectors.
% dj = [3 1 7];
% v = [2 2 -3];
% di = [5 2 4];
% k = 3;

% Logical outcome.
tf = 'equal';
% Dimension of vector.
s = 1; % Increment over u, v.
% while s <= k
%     ej = dj(s) + v(s);
%     if ej == di(s)
%         s=s+1;
%     elseif ej < di(s)
%         tf = 1;
%         s = k + 1;
%     else
%         tf = 0;
%         s = k + 1;
%     end
% end

while s <= k
    ej = dj(s) + v(s);
    if abs(ej - di(s)) < 1e-5 % Test equality with tolerance for error.
        s=s+1;
    elseif ej > di(s)
        tf = 0;
        s = k + 1;
    else
        tf = 1;
        s = k + 1;
    end        
end

end
