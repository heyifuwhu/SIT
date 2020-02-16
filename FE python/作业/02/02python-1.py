###        1.1     #############################################################
def mysubstr(arg1):
    li=arg1.split()
    result = {}
    for i in range(len(li)):
        li[i]=li[i].replace('.','').replace(',','').replace('(','').replace(')','')
        li[i]=li[i].lower()
        result[li[i]] = li.count(li[i])
        if result[li[i]] > result[li[0]]:
            maxkey = int(result[li[i]])
    for key,value in result.items():
        if value == maxkey:
            print("the most frequent word is %s and its frequency in this string is %s" %(key,value))

    return result


myString = '''
    This course is designed for those students have no experience or
    limited experience on Python. This course will cover the basis
    syntax rules, modules, importing packages (Numpy, pandas), data
    visualization, and Intro for machine learning on Python. You will
    need to implement what you learn from this course to do a finance
    related project. This course aims to get you familiar with Python
    language, and can finish a simple project with Python.
    '''
print(mysubstr(myString))

#######  1.2   ############################################
class Rectangular:
    def __init__(self,a,b):
        self.a = a
        self.b = b
    def area(self):
        area = self.a * self.b
        return area
    def perimeter(self):
        perimeter = 2 * (self.a + self.b)
        return perimeter

class Square(Rectangular):
    def __init__(self, a):
        Rectangular.__init__(self,a,a)
    def area(self):
        area = self.a * self.a
        return area
    
    def perimeter(self):
        perimeter = self.a * 4
        return perimeter
    

myRec = Rectangular(10,20)
mySqu = Square(10)
print(myRec.area())
print(myRec.perimeter())
print(mySqu.area())

####    2.1   #################################################################
def myans(g):
    i = int(1/g)
    z=float (0)
    for j in range(0,i*4):
        
        
        import math
        y=z+g*(z+4-math.exp(z))
        if abs(y-z) < 0.0001:
            break 
        else:
            z=y
        

    return y
    

mynum = myans(0.1)
print(mynum)

#########    2.2   ######################################################
def mybisec():
    import math
    a = float(2)
    b = float(4)
    ya = math.sin(a)*math.exp(a)
    yb = math.sin(b)*math.exp(b)
    while abs(ya-yb)>0.0001:
        ya = math.sin(a)*math.exp(a)
        yb = math.sin(b)*math.exp(b)
        mid = (a+b)/2
        ymid = math.sin(mid)*math.exp(mid)
        if ymid*ya>0:
            a = mid
        elif ymid*yb>0:
            b = mid
    return mid

mys = mybisec()
print(mys)

########    2.3   #######################################################
def myans(g):
    z=float (1)
    
    dic={}  
        
    import math
 
    for y in (-4,0):   
        while abs(y-z) > 0.001:
            z=y
            y=z+g*(z*math.sin(z)-1)   
        dic["stable1"]=y
    for y in (0,4):   
        while abs(y-z) > 0.001:
            z=y
            y=z+g*(z*math.sin(z)-1)   
        dic["stable2"]=y
    for y in (0,-4):   
        while abs(y-z) > 0.001:
            z=y
            y=z-g*(z*math.sin(z)-1)   
        dic["unstable1"]=y   
    for y in (4,0):   
        while abs(y-z) > 0.001:
            z=y
            y=z-g*(z*math.sin(z)-1)   
        dic["unstable2"]=y
    

    return dic
    

mynum = myans(0.1)
print(mynum)

######### leetcode 09  #####################################################
class Solution:
    def isPalindrome(self, x):
        """
        :type x: int
        :rtype: bool
        """
        if x >= 0:
            y = int(str(x)[::-1])
            if y == x:
                return True
            else:
                return False
            
        elif x<0:
                return False
            
        return x
        
#########  leetcode 13  #########################################################
class Solution:
    def romanToInt(self, s):
        """
        :type s: str
        :rtype: int
        """
        sum = 0
        b = {'I':1,'V':5,'X':10,'L':50,'C':100,'D':500,'M':1000}
        for i in range(len(s)-1):
            if (b[s[i]] < b[s[i+1]]):
                sum -= b[s[i]]
            elif (b[s[i]] >= b[s[i+1]]):
                sum += b[s[i]]
        sum += b[s[len(s)-1]]   
            
        return sum



