% import fakeset csv from dataset/SMH
set = final{:,:};
set=sparse(set);
%c = set(1,:)
%d = set(:,4)       gets the values in 4th column

% set11var_mat = table2array(set11var)
% set11var_mat(:,1)=[]      remove 1st column
final27var(:,1)=[];
final27var_mat = table2array(final27var);
% sum( sum( sum( sum( sum( sum( sum( sum( sum( sum(abs(Y.data-X.data)))))))))))
% perfect prediction

% X.data(isnan(X.data))=0.01        they're not NaN, they're 0

%make 5 fold cv sets
load experiment\set11var_mat
[m,n] = size(set11var_mat);
p = 0.5;
idx = randperm(m);
train5 = set11var_mat(idx(1:round(p*m)),:);
test5 = set11var_mat(idx(round(p*m)+1:end),:);

% SVD
[U,S,V]=svd(set11var_mat);
new = U*S*V;
mean(abs(set11var_mat(:,11)-new(:,11))); % error is 203.4581