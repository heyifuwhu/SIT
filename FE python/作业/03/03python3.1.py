import numpy as np

a1=np.zeros((3,4),dtype=np.float64)
print(a1)

a2=np.random.randn(100)
b=(a2 > 0.4).sum()
print(b)
print(b/100)

a3=np.empty((7,7))
for i in range(7):
    a3[i]=np.random.uniform(-10,10)
    for j in range(7):
        a3[i,j]=np.random.uniform(-10,10)
print(a3)

new_a3=a3
for i in range(7):
    for j in range(7):
        if new_a3[i,j]>0:
            new_a3[i,j]=int(new_a3[i,j]+1)
        elif new_a3[i,j]<0:
            new_a3[i,j]=int(new_a3[i,j]-1)
print(new_a3)


a4=np.random.normal(1,np.sqrt(2),1000)
mean_a4=np.mean(a4)
variance_a4=np.var(a4)
skewness_a4=(np.mean(a4**3)-3*mean_a4*variance_a4-mean_a4)/(np.sqrt(variance_a4)**3)
kurtosis_a4=np.mean((a4-mean_a4)**4)/variance_a4**2-3
print(mean_a4)
print(variance_a4)
print(skewness_a4)
print(kurtosis_a4)

a5=np.random.randint(100,size=100)
print(a5)
max_a5=max(a5)
min_a5=min(a5)
print("the max index")
for i in range(100):
    if a5[i]==max_a5:
        print(i)
print("the min index")
for i in range(100):
    if a5[i]==min_a5:
        print(i)


x=np.array([0,1,2,3])
y=np.array([-2.4,0.5,1.9,4.1])
A = np.vstack([x, np.ones(len(x))]).T
m, c = np.linalg.lstsq(A, y,rcond=None)[0]
print(m, c)
        




