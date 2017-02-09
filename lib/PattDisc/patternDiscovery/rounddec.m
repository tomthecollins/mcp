function x = rounddec(x, n)
%ROUNDDEC  Round to n decimal places
%
%   This function rounds an element x to a total of n decimal places.
%
if n >= 0
    x = round(x * 10^n) / 10^n;

end

