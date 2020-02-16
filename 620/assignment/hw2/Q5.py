import math
# Q5 a
bond_value=100/1.03**31
for i in range(1,32):
	bond_value+=5/1.03**i
conversion_1=bond_value/100
print(conversion_1)


# Q5 b
bond_value_3month=100/1.03**42
for i in range(0,43):
	bond_value_3month+=3.5/1.03**i
I_3mons=math.sqrt(1.03)-1
present_value=bond_value_3month/(I_3mons+1)
conversion_2=(present_value-3.5/2)/100
print(conversion_2)

# Q5 c
price_1=169-118.71875*conversion_1
price_2=136-118.71875*conversion_2
print(price_1,price_2)

# Q5 d
accured_I=5*175/181
cash_price=118.71876*conversion_1+accured_I
print(cash_price)