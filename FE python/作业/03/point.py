class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def distance(self):
        import math
        distance = math.sqrt(self.x*self.x+self.y*self.y)
        return distance
        # <fill your code here>
