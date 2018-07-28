gender_index = Index(2);
month_index = Index(12);
day_of_week_index = Index(7);
NON_GIM_ERConsult_index = Index(2);
Waittime_to_admit_index = Index(59);

CT_HEAD_Count_index = Index(2);
POR_CHEST_Count_index = Index(2);
RAD_CHEST_Count_index = Index(2);
RAD_LOWER_EXT_Count_index = Index(2);
RAD_PELVIS_HIP_Count_index = Index(2);

topic_index = Index(5);

Y = Tensor( gender_index, month_index,day_of_week_index, NON_GIM_ERConsult_index, Waittime_to_admit_index,...
    CT_HEAD_Count_index, POR_CHEST_Count_index, RAD_CHEST_Count_index, RAD_LOWER_EXT_Count_index, RAD_PELVIS_HIP_Count_index);

Z1 = Tensor( topic_index, gender_index);
Z2 = Tensor( topic_index, month_index);
Z3 = Tensor( topic_index, day_of_week_index);
Z4 = Tensor( topic_index, NON_GIM_ERConsult_index);
Z5 = Tensor( topic_index, Waittime_to_admit_index);

Z6 = Tensor( topic_index, CT_HEAD_Count_index);
Z7 = Tensor( topic_index, POR_CHEST_Count_index);
Z8 = Tensor( topic_index, RAD_CHEST_Count_index);
Z9 = Tensor( topic_index, RAD_LOWER_EXT_Count_index);
Z10 = Tensor( topic_index, RAD_PELVIS_HIP_Count_index);

load experiment/set11var_mat

i=1;
while i <= 16371
    ind = set11var_mat(i,:);
    Y.data(ind(:,1), ind(:,2), ind(:,3), ind(:,4), ind(:,5), ind(:,6), ind(:,7), ind(:,8), ind(:,9), ind(:,10)) = ind(:,11);
    i = i + 1;
end

Y.data(Y.data==0)=0.01;