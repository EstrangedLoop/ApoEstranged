"""
    INPUT:
    months, pairs of rabbits made after each gen

    took me a while to figure out the formula for this, which is

    Gen(N) = Gen(N-1) + Gen(N-2)*K
    where n = month, and k = babbies

"""

def fib(m, l):
    if m == 1:
        return 1
    elif m == 2:
        return 1
    else:
        return fib(m - 1, l) + fib(m - 2,l)*l



def main():
    
    with open("rosa4dat.txt", "r") as f:
        month, litter = f.read().split()
    
    numRabbits = fib(int(month), int(litter))
    
    with open("ANS.txt", "w") as f:
        print(numRabbits, file=f)
main()
