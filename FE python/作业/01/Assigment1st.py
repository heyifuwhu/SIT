#Q1
#*********************************************************************************
def inserttool(list1,x): # creat a function 
    list1 = [int(list1[i]) for i in range(len(list1))] #transfer character into int
    list2 = sorted(list1)
    print("oldlist",list1)
    for i in range(len(list2)): #insert i, and find right place to put it in
        a = len(list2)
        if int(x)>=list2[a-1] :
            list2.insert(a,int(x))
            break
        elif list2[i]>int(x) : 
            list2.insert(i,int(x)) 
            break
        else : 
            continue 
    print("newlist",list2) 

list1 = input ("please input your number").split( )
x = input ("insert a number")
inserttool(list1,x)

#Q2
#*********************************************************************************
def returnNo(x1,x2): ## creat a function 
    H=x1
    M=0
    for i in range(x2): #calculate the value of total distance
        M=M+H+H/2
        H=H/2
    if x2==1:
        A="once"
    elif x2==2:
        A="twice"
    elif x1>2:
        A=str(x2)+"th"
    print("after", A, "bounce, the ball bounce back to %f meters height." % (M))


x1 = int(input ("OringinalHeight"))
x2 = int(input ("NumLanding"))
returnNo(x1,x2)

#Q3
#**********************************************************************************
dict1 = {'a': 97, 't': 116, 'w': 119, 'c': 99, 'b': 98, \
'e': 101, 'd': 100, 'g': 103, 'f': 102, 'i': 105, 'h': 104,\
'k': 107, 'j': 106, 'm': 109, 'l': 108, 'o': 96, 'n': 110, \
'q': 113, 'p': 112, 's': 115, 'r': 114, 'u': 117, 'v': 118, \
'y': 121, 'x': 120, 'z': 122}
#Sort the key of ​dict from ‘a’ to ‘z’
d=dict(sorted(dict1.items(),key=lambda item:item[0]))
print(d)
#Find and correct a wrong value with corresponding key
i=97
for key,value in d.items():
    if d[key] == i:
        i+=1
    else :
        print("the wrong value corresponding" ,key, ":", value)
        d[key]=111
        print(d)
        break

#Create an upper case letter and corresponding ASCII in a new dictionary by using ​for​ loop.
new_d = dict()
a=65
for i in range(26):
    new_d[chr(a+i)] = a+i
print(new_d)
#Combine lower case and upper case letter with ASCII in a new dictionary.
com_d = dict()
com_d.update(new_d)
com_d.update(d)
print(com_d)
#Switch the value of ​dict​ to hexadecimal number
for key,value in dict1.items():
    dict1[key]=hex(value)
print("dict in hexadecimal number",dict1)

#Leetcode 1
#******************************************************************************************
class Solution:
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        for i in range(len(nums)):
            A = target - nums[i]
            for j in range(i+1,len(nums)):
                if A==nums[j]:
                    return [i,j]

#Leetcode 7
#*******************************************************************************************
class Solution:
    def reverse(self, x):
        """
        :type x: int
        :rtype: int
        """
        
        if x >= 0:
            x = int(str(x)[::-1])
            if x>2147483646:
                return 0
        elif x<0:
            
            x = int('-' + str(abs(x))[::-1])
            if x<-2147483646:
                return 0
            
        return x