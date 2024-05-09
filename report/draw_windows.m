function draw_windows()

X = -30:30
Y = zeros(1, length(X));
n12 = (30 - -30 + 0) / 2
Y = 1 - ((X - n12 + 30) / n12).^2
% Y = ones(1, length(X));
% Y(ceil(length(Y)/2)) = 1;

stem(X, Y, 'color', 'red');
saveas(gcf, '/tmp/welch.png');

% img = imread('/tmp/rd.jpg');
% [height width depth] = size(img);

% al = we(1:10, 1, 10)
% al = we(1:width, 1, width)
% for a = 1:length(al)
%   img(:, a, :) = img(:, a, :) .* al(a);
% end
% img(:, 1:width/2-5, :) = img(:, 1:width/2-5, :) * 0;
% img(:, width/2+5:end, :) = img(:, width/2+5:end, :) * 0;
% imwrite(img, '/tmp/kdview.png')
% imwrite(img, '/tmp/weview.png')

end

function d = we(X, a, b)
n12 = (a - b) / 2;
d = 1 - ((X - n12) / n12).^2;
end
