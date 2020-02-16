class LCG:
    def __init__(self, seed, a, c, M):
        self.seed = seed
        self.a = a
        self.c = c
        self.M = M
        self.orginal = seed
        # <fill your code here>

    def get_seed(self):
        return self.seed
        # <fill your code here>

    def set_seed(self, new_seed):
        self.seed = new_seed
        self.orginal = new_seed
        # <fill your code here>

    def reset(self):
        self.seed = self.orginal
        # <fill your code here>

    def get(self): # return one uniform distributed random number
        self.seed = ((self.a * self.seed + self.c) % self.M)
        return self.seed/self.M
        # <fill your code here>

    def get_sequence(self, num): # return one sequence of distributed random numbers
        mylist = []
        for i in range(num):
            mylist.append(self.get())
        return mylist
        # <fill your code here>

    def __iter__(self):
        return self
        # <fill your code here>

    def __next__(self):
        self.seed = ((self.a * self.seed + self.c) % self.M)
        return self.seed/self.M


class SCG(LCG):
    def __init__(self, seed, M):
        self.seed = seed
        self.M = M
        self.org = seed

    def get(self): # return one uniform distributed random number
        self.seed = (self.seed * (self.seed + 1)) % self.M
        return self.seed/self.M 
    

    def __next__(self):
        self.seed = (self.seed * (self.seed + 1)) % self.M
        return self.seed/self.M
