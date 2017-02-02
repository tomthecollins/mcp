function [palxvec, palyvec] = plompLeveltCurve

% Copyright Tom Collins 20/2/2005

% Plomp and Levelt's (1965) curve for the extent to which a pair of pure
% tones cause beating.

% REFERENCE
%  Plomp, R., and Levelt, W. J. M., Tonal consonance and critical
%   bandwidth, in Journal of the Acoustical Society of America, 38
%   (1965), 548-560.

% INPUT
%  fName is the path and name to an audio file in wav format.

% EXAMPLE INPUT
%  n = 4;
%  u = [0 .25 .3333 .5 .6667 .75 1];

x1vec = [1.2*(0:5:40)/204 1.2*42.5/204 1.2*(45:5:200)/204 1.2];
y1vec = [0 40 81 113 135 149 158 166 169 170.5 169 166 159 151 140 130 ...
  119.5 108 99 88 78.5 69 61 53 46 40 34 29 25 21 17.5 14 11 9 7 5 4 3 ...
  2 1.5 1 .5 0]/170.5;
% plot(x1vec, y1vec)

i = 1;
while i < 8
  xjvec = x1vec(i:i+2);
  yjvec = y1vec(i:i+2);
  % Further subdivide the interval. Why by this amount I have no idea!
  xivec = xjvec(1):1.2/204:xjvec(3);
  yivec = interp1(xjvec, yjvec, xivec, 'cubic');
  % Populate the variables xkvec and ykvec with these results. Why just
  % the first 10 of these? I have no idea!
  xkvec(5*(i + 1) - 9:5*(i + 1)) = xivec(1:10);
  ykvec(5*(i + 1) - 9:5*(i + 1)) = yivec(1:10);
  i=i+2;
end

i = 9;
xjvec = x1vec(i:i+2);
yjvec = y1vec(i:i+2);
% Further subdivide the interval. Why by this different amount I have no
% idea!
xivec = xjvec(1):1/170:xjvec(3);
yivec = interp1(xjvec, yjvec, xivec, 'cubic');
xkvec(41:45) = xivec(1:5);
ykvec(41:45) = yivec(1:5);
i=i+2;

while i > 10 && i < 42
  xjvec = x1vec(i:i+2);
  yjvec = y1vec(i:i+2);
  % Further subdivide the interval.
  xivec = xjvec(1):1.2/204:xjvec(3);
  yivec = interp1(xjvec, yjvec, xivec, 'cubic');
  % Populate the variables xkvec and ykvec with these results. Why just
  % the first 10 of these? I have no idea!
  xkvec(5*(i + 1) - 14:5*i) = xivec(1:10);
  ykvec(5*(i + 1) - 14:5*i) = yivec(1:10);
  i=i+2;
end

ykvec([42 45]) = .9926;
ykvec(43:44) = .994;
palxvec = xkvec;
palyvec = ykvec;

bounter = 1;
for i = 206:400
  palxvec(i) = 1.2 + 5.9e-3*bounter;
  bounter=bounter+1;
end
palyvec(206:400) = 0;
% plot(palxvec, palyvec);

end
