from pandas import Series, DataFrame
import pandas as pd 
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.datasets import make_classification
from sklearn.linear_model import LogisticRegression
import matplotlib.pyplot as plt


x1 =np.random.randint(100,size=1000)
x2 =np.random.randint(100,size=1000)
x1 = sorted(x1)
x2 = sorted(x2)
step = np.random.normal(1,np.sqrt(2),1000)
y = []
for i in range(1000):
    y.append(x1[i]*3 + x2[i]*4 + step[i])

x = np.dstack((x1,x2))
x = x[0]
reg = LinearRegression().fit(x,y)

reg.coef_

# print(reg.coef_)

a1,b1 = make_classification(n_samples=1000,n_features=2, n_informative=2,
                             n_redundant=0, n_repeated=0,n_classes=2)
a_train = a1[:-200]
a_test = a1[-200:]
b_train = b1[:-200]
b_test = b1[-200:]
clf = LogisticRegression().fit(a_train,b_train)
b_prob = clf.predict_proba(a_test)[:,1]
sum=0
for i in range(200):
    if abs(b_test[i]-b_prob[i])<0.5 :
        sum += 1
accuracy = sum/200
# print(accuracy)

# f, ax = plt.subplots(figsize=(8, 6))
# xx, yy = np.mgrid[-5:5:.01, -5:5:.01]
# grid = np.c_[xx.ravel(), yy.ravel()]
# probs = clf.predict_proba(grid)[:, 1].reshape(xx.shape)
# ax.scatter(a_test[100:,0], a_test[100:, 1],c=b_test[100:], s=50,
#            cmap="RdBu", vmin=-.2, vmax=1.2,
#            edgecolor="white", linewidth=1)

# ax.set(aspect="equal",
#        xlim=(-5, 5), ylim=(-5, 5),
#        xlabel="$X_1$", ylabel="$X_2$")
# plt.show()


a2,b2 = make_classification(n_samples=1000,n_features=4, n_informative=4,
                             n_redundant=0, n_repeated=0,n_clusters_per_class=1, n_classes=10)
a_train = a1[:-200]
a_test = a1[-200:]
b_train = b1[:-200]
b_test = b1[-200:]
clf = LogisticRegression().fit(a_train,b_train)
b_prob = clf.predict(a_test)
sum=0
for i in range(200):
    if abs(b_test[i]-b_prob[i])<2 :
        sum += 1
accuracy2 = sum/200
print(accuracy2)
