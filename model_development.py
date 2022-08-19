import torch
import random
import numpy as np
import os
def seed_everything(seed=42):
    """"
    Seed everything.
    """
    random.seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.backends.cudnn.deterministic = True
SEED = 811 #42
seed_everything(SEED)

np.seterr(divide='ignore', invalid='ignore') #Ignore warning: #RuntimeWarning: invalid value encountered in true_divide

# Read the data selected by univariate Cox regression in R language
import pandas as pd
dataset = pd.read_csv('data/data_surv.csv')
dataset.columns = [name.replace('.',' ') for name in dataset.columns]
time_column = 'Survival months'
event_column = 'Status'
features = np.setdiff1d(dataset.columns, [time_column, event_column]).tolist()

#  Scale features
from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler().fit(dataset.loc[:,features])
dataset.loc[:,features] = scaler.transform(dataset.loc[:,features])

# Split dataset to train and test dataset
from sklearn.model_selection import train_test_split
import numpy as np
index_train, index_test = train_test_split(range(dataset.shape[0]), test_size = 0.3)
data_train = dataset.loc[index_train].reset_index( drop = True )
data_test  = dataset.loc[index_test].reset_index( drop = True )
# Creating the X, T and E inputs
X_train, X_test = data_train[features], data_test[features]
T_train, T_test = data_train[time_column], data_test[time_column]
E_train, E_test = data_train[event_column], data_test[event_column]

# Some Basic Function
from pysurvival.utils.metrics import concordance_index
from sklearn.model_selection import RepeatedKFold
class BaseFun:
    def parse_param(self, param):
        for key in param:
            if isinstance(param[key], str):
                param[key] = '"{}"'.format(param[key])
        return ','.join(['{} = {}'.format(key, param[key]) for key in param])
    def get_random_param(self,space):
        param = {}
        for key in space:
            if  key == 'structure':
                items = []
                for i in range(1,random.choice(space['structure']['num_layers'])+1):
                    items.append(
                        {
                        'activation': random.choice(space['structure']['activations']),
                        'num_units':random.choice(space['structure']['num_units'])
                        }
                    )
                param['structure'] = items
            else:
                param[key] = random.choice(space[key])
        return param
    def tuning_and_construct(self,X, T, E,max_iter=100):
        self.tuning_result = self.tuning_with_space(X, T, E,self.space,max_iter=max_iter)
        self.model = self.fit_model(X, T, E,**self.tuning_result['best_param'])
        return self.model
    def tuning_with_space(self,x,t,e,space,max_iter=100):
        [x,t,e] = [item if isinstance(item, np.ndarray) else np.array(item) for item in [x,t,e]]
        scores = []
        best_score = 0
        best_param = {}
        num = 1
        while True:
            param = self.get_random_param(space)
            print('Number {} iteration'.format(num), end=' ... ')
            # split train data to 5 parts
            rkf = RepeatedKFold(n_splits=5, n_repeats=1)
            score_iter = []
            for train_index, test_index in rkf.split(x):
                x_train, x_test = x[train_index], x[test_index]
                t_train, t_test = t[train_index], t[test_index]
                e_train, e_test = e[train_index], e[test_index]
                try:
                    model = self.fit_model(x_train, t_train, e_train,**param)
                    score = concordance_index(model, x_test, t_test, e_test)
                except Exception as err:
                    print(str(err))
                    break
                score_iter.append(score)
            mean_score = np.mean(score_iter)
            print('mean_c_index: ', mean_score, end=' ')
            if mean_score > best_score:
                best_score = mean_score
                best_param = param
                print('is the best so far')
            else:
                print('')
            scores.append({'iteration':num,'mean_score': mean_score, 'param': param})
            if num == max_iter:
                break
            num += 1
        return {'best_param': best_param, 'best_score': best_score, 'scores': scores}

# Construction of NeuralMultiTaskModel
from pysurvival.models.multi_task import NeuralMultiTaskModel
class NMTLR(BaseFun):
    def __init__(self):
        self.space = {
            'structure': {'num_layers':[1,2,3,4,5],
                          'num_units': [i for i in range(8, 100)],
                          'activations': ["Atan", "BentIdentity", "BipolarSigmoid", "CosReLU",
                                           "Gaussian", "Hardtanh", "InverseSqrt", "LeakyReLU",
                                          "LeCunTanh", "LogLog", "LogSigmoid", "ReLU", "SELU", "Sigmoid",
                                          "Sinc", "SinReLU","Softmax", "Softplus", "Softsign", "Swish", "Tanh"]
                         },
            'optimizer':['adadelta','adagrad','adam','adamax','rmsprop','sgd'],
            'bins' : [i for i in range(10,100)],
            'lr': [round(1e-5 * i, 5) for i in range(1, 100 + 1)],
            'num_epochs': [i for i in range(50, 1000 + 1)],
            'dropout': [round(0.1 * i, 2) for i in range(1, 4 + 1)],
            'l2_reg': [round(0.0001 * i, 5) for i in range(1, 100 + 1)],
            'l2_smooth':[round(0.0001 * i, 5) for i in range(1, 100 + 1)],
            'batch_normalization' : [False, True]
        }
        self.model = None
    def fit_model(self,X, T, E,**kwargs):
        structure = [{'activation': 'ReLU', 'num_units': 128}]
        bins = 100
        if 'structure' in kwargs:
            structure = kwargs['structure']
            del kwargs['structure']
        if 'bins' in kwargs:
            bins = kwargs['bins']
            del kwargs['bins']
        self.model = NeuralMultiTaskModel(structure=structure,bins=bins)
        eval('self.model.fit(X, T, E,{})'.format(self.parse_param(kwargs)))
        return self.model

# Construction of NonLinearCoxPHModel (DeepSurv)
from pysurvival.models.semi_parametric import NonLinearCoxPHModel
class DeepSurv(BaseFun):
    def __init__(self):
        self.space = {
            'structure': {'num_layers':[1,2,3,4,5],
                          'num_units': [i for i in range(8, 100)],
                          'activations': ["Atan", "BentIdentity", "BipolarSigmoid", "CosReLU",
                                           "Gaussian", "Hardtanh", "InverseSqrt", "LeakyReLU",
                                          "LeCunTanh", "LogLog", "LogSigmoid", "ReLU", "SELU", "Sigmoid",
                                          "Sinc", "SinReLU","Softmax", "Softplus", "Softsign", "Swish", "Tanh"]
                         },
            'optimizer':['adadelta','adagrad','adam','adamax','rmsprop','sgd'],
            'lr': [round(1e-5 * i, 5) for i in range(1, 100 + 1)],
            'num_epochs': [i for i in range(50, 5000 + 1)],
            'dropout': [round(0.1 * i, 2) for i in range(1, 4 + 1)],
            'l2_reg': [round(0.0001 * i, 5) for i in range(1, 100 + 1)],
            'batch_normalization' : [False, True]
        }
        self.model = None
    def fit_model(self,X, T, E,**kwargs):
        structure = [{'activation': 'ReLU', 'num_units': 128}]
        bins = 100
        if 'structure' in kwargs:
            structure = kwargs['structure']
            del kwargs['structure']
        self.model = NonLinearCoxPHModel(structure=structure)
        eval('self.model.fit(X, T, E,{})'.format(self.parse_param(kwargs)))
        return self.model

# Construction of RandomSurvivalForestModel
from pysurvival.models.survival_forest import RandomSurvivalForestModel
class RSF(BaseFun):
    def __init__(self):
        self.space = {
            'num_trees': [i for i in range(20, 1000 + 1)],
            'max_features': ['sqrt', 'log2', 'all', 0.1, 0.2],
            'min_node_size': [i for i in range(5, 80 + 1)],
            'sample_size_pct': [round(0.2 * i, 2) for i in range(1, 4 + 1)],
            'importance_mode': ['impurity', 'impurity_corrected', 'permutation', 'normalized_permutation']
        }
        self.model = None
    def fit_model(self,X, T, E,**kwargs):
        if 'num_trees' in kwargs:
            self.model = RandomSurvivalForestModel(num_trees=kwargs['num_trees'])
            del kwargs['num_trees']
        else:
            self.model = ConditionalSurvivalForestModel()
        eval('self.model.fit(X, T, E,seed=SEED,{})'.format(self.parse_param(kwargs)))
        return self.model

# Construct CPH
from pysurvival.models.semi_parametric import CoxPHModel
def cph(X_train, T_train, E_train):
    model = CoxPHModel()
    model.fit(X_train, T_train, E_train, lr=0.2, l2_reg=0.01)
    return model
cph_model = cph(X_train, T_train, E_train)
c_index_train = concordance_index(cph_model, X_train, T_train, E_train)
c_index_test = concordance_index(cph_model, X_test, T_test, E_test)
print('C-index of train: {:.4f}; C-index of test: {:.4f}'.format(c_index_train,c_index_test))

max_iter = 1000
# Tuning NMTLR
nmtlr = NMTLR()
nmtlr.tuning_and_construct(X_train, T_train, E_train,max_iter=max_iter)
nmtlr_model = nmtlr.model
c_index_train = concordance_index(nmtlr.model, X_train, T_train, E_train)
c_index_test = concordance_index(nmtlr.model, X_test, T_test, E_test)
print('C-index of train: {:.4f}; C-index of test: {:.4f}'.format(c_index_train,c_index_test)) #C-index: 0.6996

# Tuning DeepSurv
deepsurv = DeepSurv()
deepsurv.tuning_and_construct(X_train, T_train, E_train,max_iter=max_iter)
deepsurv_model = deepsurv.model
c_index_train = concordance_index(deepsurv.model, X_train, T_train, E_train)
c_index_test = concordance_index(deepsurv.model, X_test, T_test, E_test)
print('C-index of train: {:.4f}; C-index of test: {:.4f}'.format(c_index_train,c_index_test)) #C-index: 0.6996

# Tuning RSF
rsf = RSF()
rsf.tuning_and_construct(X_train, T_train, E_train,max_iter=max_iter)
rsf_model = rsf.model
c_index_train = concordance_index(rsf.model, X_train, T_train, E_train)
c_index_test = concordance_index(rsf.model, X_test, T_test, E_test)
print('C-index of train: {:.4f}; C-index of test: {:.4f}'.format(c_index_train,c_index_test)) #C-index: 0.6996

# Recommend treatment
def rec_treatment(model,X,col=0):
    if isinstance(X,pd.core.series.Series):
        x_trt = np.copy(X.to_numpy())
    else:
        x_trt = np.copy(X)
    # Calculate risk of observations treatment i
    x_trt[:, col] = 0
    h_i = model.predict_risk(x_trt)
    # Risk of observations in treatment j
    x_trt[:, col] = 1
    h_j = model.predict_risk(x_trt)
    rec_ij = h_i - h_j
    rec_ij = pd.DataFrame(rec_ij, columns=["Rec"])
    return rec_ij
# model: cph, treatment:surgery
rec_ = rec_treatment(cph_model,X_test,col=7)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Surgery"], 'Yes', 'No')
data_rec.to_csv(save_path + 'cph_surgery.csv')
# model: cph, treatment:chemotherapy
rec_ = rec_treatment(cph_model,X_test,col=3)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Chemotherapy"], 'Yes', 'No')
data_rec.to_csv(save_path + 'cph_chemotherapy.csv')
# model: nmtlr, treatment:surgery
rec_ = rec_treatment(nmtlr.model,X_test,col=7)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Surgery"], 'Yes', 'No')
data_rec.to_csv(save_path + 'nmtlr_surgery.csv')
# model: nmtlr, treatment:chemotherapy
rec_ = rec_treatment(nmtlr.model,X_test,col=3)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Chemotherapy"], 'Yes', 'No')
data_rec.to_csv(save_path + 'nmtlr_chemotherapy.csv')
# model: deepsurv, treatment:surgery
rec_ = rec_treatment(deepsurv.model,X_test,col=7)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Surgery"], 'Yes', 'No')
data_rec.to_csv(save_path + 'deepsurv_surgery.csv')
# model: deepsurv, treatment:chemotherapy
rec_ = rec_treatment(deepsurv.model,X_test,col=3)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Chemotherapy"], 'Yes', 'No')
data_rec.to_csv(save_path + 'deepsurv_chemotherapy.csv')
# model: rsf, treatment:surgery
rec_ = rec_treatment(rsf.model,X_test,col=7)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Surgery"], 'Yes', 'No')
data_rec.to_csv(save_path + 'rsf_surgery.csv')
# model: rsf, treatment:chemotherapy
rec_ = rec_treatment(rsf.model,X_test,col=3)
data_rec = pd.concat([data_test,rec_],axis=1)
data_rec['Recommendation'] = np.where(data_rec['Rec'] < 0, 0, 1)
data_rec['Align'] = np.where(data_rec["Recommendation"] == data_rec["Chemotherapy"], 'Yes', 'No')
data_rec.to_csv(save_path + 'rsf_chemotherapy.csv')

# Generate data for plotting prediction error plot
from pysurvival.utils.metrics import brier_score
times_cph, brier_scores_cph = brier_score(cph_model, X_train, T_train, E_train, t_max=100, use_mean_point=True)
ibs_cph =  np.trapz(brier_scores_cph, times_cph)/100
print(ibs_cph)
times_nmtlr, brier_scores_nmtlr = brier_score(nmtlr.model, X_train, T_train, E_train, t_max=100, use_mean_point=True)
ibs_nmtlr =  np.trapz(brier_scores_nmtlr, times_nmtlr)/100

times_deepsurv, brier_scores_deepsurv = brier_score(dep_model, X_train, T_train, E_train, t_max=100, use_mean_point=True)
ibs_deepsurv =  np.trapz(brier_scores_deepsurv, times_deepsurv)/100
print(ibs_deepsurv)
times_rsf, brier_scores_rsf = brier_score(rsf.model, X_train, T_train, E_train, t_max=100, use_mean_point=True)
ibs_rsf =  np.trapz(brier_scores_rsf, times_rsf)/100

pd.DataFrame(
    {'times':times_cph+times_nmtlr+times_deepsurv+times_rsf,
    'brier_scores':brier_scores_cph+brier_scores_nmtlr+brier_scores_deepsurv+brier_scores_rsf,
    'models':['CoxPH (IBS: {:.4f})'.format(ibs_cph) for i in brier_scores_cph] + ['NMTLR (IBS: {:.4f})'.format(ibs_nmtlr) for i in brier_scores_nmtlr]+['DeepSurv (IBS: {:.4f})'.format(ibs_deepsurv) for i in brier_scores_deepsurv]+['RSF (IBS: {:.4f})'.format(ibs_rsf) for i in brier_scores_rsf]
            }).to_csv(save_path+'brier_score.csv')

# Generate data for plotting variable importance
def variableImportance(model, X, T, E):
    ctmp = concordance_index(model, X, T, E)
    imps = []
    for column in X:
        tmpX = X.copy()
        tmpX[column] = np.random.permutation(tmpX[column])
        ctmp2 = concordance_index(model, tmpX, T, E)
        imps.append((ctmp - ctmp2)/ctmp)
    return imps
imp_df = pd.DataFrame(
    {
        'DeepSurv': variableImportance(deepsurv.model, X_test, T_test, E_test),
        'NMTLR': variableImportance(nmtlr.model, X_test, T_test, E_test),
        'RSF': variableImportance(rsf.model, X_test, T_test, E_test),
    },
    index= X_test.columns
)
imp_df['Average'] = imp_df.mean(axis=1)
print(imp_df)
def plotFeatureImportance(a):
    import matplotlib.pyplot as plt
    import matplotlib as mpl
    import matplotlib.cm as cm
    import matplotlib.colors as colors
    mpl.rcParams['figure.dpi'] = 300
    # a = pd.read_csv(feature_filename)
    a = a.sort_values(by=['Average'], ascending=False)
    fig, ax = plt.subplots(figsize=(8, 10))

    def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
        new_cmap = colors.LinearSegmentedColormap.from_list(
            'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
            cmap(np.linspace(minval, maxval, n)))
        return new_cmap

    im = ax.imshow(a[['DeepSurv', 'NMTLR', 'RSF', 'Average']],
                   cmap=truncate_colormap(cm.get_cmap("hot"), minval=0.4, maxval=1), norm=colors.SymLogNorm(1e-3))
    # im.set_clim(-5, -2)
    cbar = ax.figure.colorbar(im, ax=ax)
    cbar.ax.set_ylabel("Decrease in Concordance Index, %", rotation=-90, va="bottom")
    # cbar.ax.set_aspect(50)
    cbar.set_ticks([0.1,0.01,0.001,0,-0.001])
    cbar.set_ticklabels(['10',"1","0.1",0,'-1'])

    ax.set_yticks(np.arange(len(a)))
    ax.set_xticks(np.arange(len(a.columns)))
    ax.set_xticklabels(['DeepSurv', 'NMTLR', 'RSF', 'Average'])
    ax.set_yticklabels(a.index)
    plt.setp(ax.get_xticklabels(), rotation=45, ha="right", rotation_mode="anchor")
    fig.tight_layout()
    plt.savefig("./variable.eps")
    plt.show()
plotFeatureImportance(imp_df)
