from generator import LCG, SCG
from point import Point

print('For LCG generator: ')
generator1 = LCG(seed = 17,
                 a = 1103515245,
                 M = 2**32,
                 c = 12345)


a = iter(generator1)
fr1 = 0
for i in range(10000000):
    x=generator1.get()
    y=generator1.get()
    p = Point(x,y)
    
    if (p.distance() < 1):
        fr1 += 1
ratio1 = fr1/10000000

print('ration1: %s' % ratio1)


print('\nFor SCG generator:')


generator2 = SCG(seed = 17,
                 M = 2**32)
a = iter(generator2)
fr2 = 0
for i in range(10000000):
    x=generator2.get()
    y=generator2.get()
    p = Point(x,y)
    
    if (p.distance() < 1):
        fr2 += 1
ratio2 = fr2/10000000

print('ration2: %s' % ratio2)




