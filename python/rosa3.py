"""
Reverse Complement
This program should take in a DNA string, flip it, then return the complement
of each symbol.
Rule:
"A" and "T" are complements of each other.
"C" and "G" are complements of each other.
"""
# uses filename = rosa3dat.py


def main():
    stringList = []
    with open("rosa3dat.txt", 'r') as f: 
        for line in f:
            for token in line:
                # add stuff in order in list, ignoring newline
                if token != "\n":
                    stringList.append(token)
    #iterate list backwards
    # format of for: (start, end, increment)
    revString = "" 
    for i in range(len(stringList) -1,-1, -1):
        if stringList[i] == "A":
            revString += "T"
        elif stringList[i] == "T":
            revString += "A"
        elif stringList[i] == "C":
            revString += "G"
        elif stringList[i] == "G":
            revString += "C"
    
    with open("ANS.txt", "w") as f:
        print(revString, file=f)


main()
