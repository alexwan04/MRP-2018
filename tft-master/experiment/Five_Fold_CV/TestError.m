load experiment/five_fold_cv/test5.mat
i=1;
Xhisto = [];
Yhisto = test5(:,11)';

while i<=8185
    ind = test5(i,:);
%     if train5(i,5) > 34
%         i = i + 1;
%     else
        Xhisto = [Xhisto, X.data(ind(:,1), ind(:,2), ind(:,3), ind(:,4), ind(:,5), ind(:,6), ind(:,7), ind(:,8), ind(:,9), ind(:,10))];
        i = i + 1;
%     end
end
% sum(abs(Xhisto-Yhisto))/8185;
% Yhisto(:,8184)=[];