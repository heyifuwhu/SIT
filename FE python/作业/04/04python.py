from pandas import Series, DataFrame
import pandas as pd 
import numpy as np
'''## 1.1
BalanceSheet = pd.read_csv('AAPL BS.csv')
Ratings = pd.read_csv('AAPL Ratings.csv')
## 1.2
BalanceSheet = BalanceSheet.drop('aqepsq', axis = 1)
BalanceSheet = BalanceSheet.drop('gdwlamq', axis = 1)
BSmean = (BalanceSheet['dlttq']).mean()
BalanceSheet['dlttq'] = BalanceSheet['dlttq'].replace(0,BSmean)
BalanceSheet['intanq'] = BalanceSheet['intanq'].interpolate()


## 1.3
BalanceSheet['niq'] = BalanceSheet['niq'].replace(BalanceSheet['niq'][np.abs(BalanceSheet['niq'])>15000],15000)
BalanceSheet['citotalq'] = BalanceSheet['citotalq'].replace(BalanceSheet['citotalq'][np.abs(BalanceSheet['citotalq'])>15000],15000)


## 1.4
df = BalanceSheet.ix[:,['acoq','actq','apq','chq','citotalq']]
def sort_value(column):
    new_column = column.sort_values(ascending = False)
    return Series([new_column.iloc[1], new_column.min(),new_column.max()], index=['second_large','min', 'max'])

print (df.apply(sort_value))

## 1.5
print(df.corr())

## 1.6
Matched = pd.merge(Ratings,BalanceSheet,on='datadate',how='inner')
print(Matched)

## 1.7
rate_no = {
    'AAA' : '0',
    'AA+' : '1',
    'AA'  : '2',
    'AA-' : '3',
    'A+'  : '4',
    'A'   : '5',
    'A-'  : '6',
    'BBB+': '7',
    'BBB' : '8',
    'BBB-': '9',
    'BB+' : '10',
    'BB ' : '11'
}
Matched['Rate'] = Matched['splticrm'].map(str).map(rate_no)
print(Matched['Rate'])

## 1.8

Matched = Matched.take(np.random.randint(0,27,size=len(Matched)*2))
print(Matched)

## 1.9
Matched.to_csv('HW4.csv')'''




## 2.1
'''def myFun1(array):
    ##array[i][j]
    for i in range(len(array)):
        for j in range(len(array[0])):
            if(array[i][j]==np.inf):
                array[i][j] = pow(2,8)
    return array

a = np.array([[1,2],[4,0]])

print(myFun1(1/a))

## 2.2
def myFun2(array):
    for i in range(len(array)):
        for j in range(len(array[0])):
            if(array[i][j] > 0):
                array[i][j] = 1
            elif(array[i][j] < 0):
                array[i][j] = -1
    return array
b = np.array([[1,2],[-4,0]])
print(myFun2(b))

## 2.3
def myFun3(matrix,oneD):
    newD = np.zeros([len(matrix[0])])
    for i in range(len(matrix)):
        for j in range(len(matrix[0])):
            newD[j] += matrix[i][j] * oneD[i]
    return newD
data_matrix = np.array([[1,2,3],[-4,0,3]])
weight = np.array([0.2,0.8])
print(myFun3(data_matrix,weight))

'''## 2.4
def myFun4():
    array1 = np.random.normal(size=100)
    array2 = np.random.normal(size=100)
    twoD1 = np.array(array1).reshape((4,25))
    twoD2 = np.array(array2).reshape((4,25))
    twoD = np.concatenate([array1,array2]).reshape((8,25))
    return twoD1,twoD2,twoD
print(myFun4())

'''##leetcode 1
class Solution:
    def arrayPairSum(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """
        return sum(sorted(nums)[::2])

##leetcode 2
class Solution:
    def transpose(self, A):
        """
        :type A: List[List[int]]
        :rtype: List[List[int]]
        """
        for i in range (len(A)):
            for j in range(len(A[0])):
                A[i][j] = A[j][i]
                A[j][i] = A[i][j]
        return A 
'''

