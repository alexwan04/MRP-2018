load experiment/smhtest_mat.mat
i=1;
Xhisto = [];
Yhisto = smhtest_mat(:,11)';

while i<=4093
    ind = smhtest_mat(i,:);
    Xhisto = [Xhisto, X.data(ind(:,1), ind(:,2), ind(:,3), ind(:,4), ind(:,5), ind(:,6), ind(:,7), ind(:,8), ind(:,9), ind(:,10))];
    i = i + 1;
end

load experiment/smhtest_mat

Y = Tensor( gender_index, month_index,day_of_week_index, NON_GIM_ERConsult_index, Waittime_to_admit_index,...
    CT_HEAD_Count_index, POR_CHEST_Count_index, RAD_CHEST_Count_index, RAD_LOWER_EXT_Count_index, RAD_PELVIS_HIP_Count_index);
i=1;
while i <= 4093
    ind = set11var_mat(i,:);
    Y.data(ind(:,1), ind(:,2), ind(:,3), ind(:,4), ind(:,5), ind(:,6), ind(:,7), ind(:,8), ind(:,9), ind(:,10)) = ind(:,11);
    i = i + 1;
end

i=1;
Xhisto=[];
Yhisto=[];
while i<=4093
    ind = smhtest_mat(i,:);
    if smhtest_mat(i,5) > 34
        i = i + 1;
    else
        Xhisto = [Xhisto, Xtrain.data(ind(:,1), ind(:,2), ind(:,3), ind(:,4), ind(:,5), ind(:,6), ind(:,7), ind(:,8), ind(:,9), ind(:,10))];
        Yhisto = [Yhisto, Y.data(ind(:,1), ind(:,2), ind(:,3), ind(:,4), ind(:,5), ind(:,6), ind(:,7), ind(:,8), ind(:,9), ind(:,10))];
        i = i + 1;
    end
end

sum(abs(Xhisto-Yhisto))/4093;