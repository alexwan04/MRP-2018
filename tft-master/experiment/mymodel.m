% GTP = Generalized Tensor Product
clear all;

tft_clear();
rand('seed',0);

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

X = Tensor( gender_index, month_index,day_of_week_index, NON_GIM_ERConsult_index, Waittime_to_admit_index,...
    CT_HEAD_Count_index, POR_CHEST_Count_index, RAD_CHEST_Count_index, RAD_LOWER_EXT_Count_index, RAD_PELVIS_HIP_Count_index);
Y = X;

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
    X.data(ind(:,1), ind(:,2), ind(:,3), ind(:,4), ind(:,5), ind(:,6), ind(:,7), ind(:,8), ind(:,9), ind(:,10)) = ind(:,11);
    i = i + 1;
end
% X.data = rand (patient_index.cardinality, var_index.cardinality );

X.data(X.data==0)=0.01;
Y.data = X.data;


Z1.data = rand( topic_index.cardinality, gender_index.cardinality );
Z2.data = rand( topic_index.cardinality, month_index.cardinality );
Z3.data = rand( topic_index.cardinality, day_of_week_index.cardinality );
Z4.data = rand( topic_index.cardinality, NON_GIM_ERConsult_index.cardinality );
Z5.data = rand( topic_index.cardinality, Waittime_to_admit_index.cardinality );

Z6.data = rand( topic_index.cardinality, CT_HEAD_Count_index.cardinality );
Z7.data = rand( topic_index.cardinality, POR_CHEST_Count_index.cardinality );
Z8.data = rand( topic_index.cardinality, RAD_CHEST_Count_index.cardinality );
Z9.data = rand( topic_index.cardinality, RAD_LOWER_EXT_Count_index.cardinality );
Z10.data = rand( topic_index.cardinality, RAD_PELVIS_HIP_Count_index.cardinality );

pre_process();

p = [1];
phi = [1];

factorization_model = {X, {Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10}}; 

model = TFModel(factorization_model, p, phi);

gtp_rules = model.update_rules();

for rule_ind = 1:length(gtp_rules)
    display_rule( gtp_rules{rule_ind}, rule_ind, 'rule ' );
end

config = TFEngineConfig(model, 15);
engine = TFDefaultEngine(config, 'gtp');
engine.factorize();
plot(engine.beta_divergence'');
check_divergence(engine.beta_divergence);
