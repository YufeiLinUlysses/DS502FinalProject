import pandas as pd  

extraData = pd.read_csv("./data/extraData.csv",header=0)
# extraData = extraData.drop([extraData.shape[0]])
alias_list = []
for alias in extraData["Alias"]:
    alias = alias.lstrip()
    alias_list.append(alias)

extraData["Alias"] = alias_list
print(extraData["Alias"])

train = pd.read_csv("./data/train.csv")

sub = train[["Neighborhood","SalePrice"]]

print(sub["Neighborhood"][0] in extraData["Alias"].tolist())

# ci = []
# pw = []
# mi = []
# cr = []
# for i in range(extraData.shape[0]):
#     for j in range(sub.shape[0]):
#         if extraData["Alias"][i] == sub["Neighborhood"][j]:
#             ci.append(extraData['Crime Index (National Avg = 1.0)'][i])

# print(ci)
print(extraData["Alias"])
joinDf = sub.set_index('Neighborhood').join(extraData.set_index('Alias'))
quantitative = [f for f in joinDf.columns if joinDf.dtypes[f] != 'object']
quanDF = joinDf[quantitative]
print(quanDF.corr())

# import seaborn as sns; sns.set_theme()
# import matplotlib 
# import matplotlib.pyplot as plt 

# sns.heatmap(quanDF.corr())
# plt.show()

print(quanDF.head())

joinDf = train.set_index('Neighborhood').join(extraData.set_index('Alias'))
joinDf.drop(["Neighborhood","Comments","CrimeRate"],1,inplace=True)
joinDf.reset_index(inplace=True)
joinDf.rename(columns={"index":"Neighborhood"},inplace=True)
joinDf.sort_values(by=["Id"],inplace=True)
print(joinDf.head())
joinDf.to_csv("./data/train_new.csv",index=False)