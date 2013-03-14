##############"
# Apo
# 12/03/13
# My teacher tells us
# to put flowerboxes here usually.
# This probably is not the right format.
#
##############

import sys

def main():
    # usage: python3 rosa_dna.py FILENAME
    # Everything after the (guesing) python3 bit is counted into
    # some kind of list in sys.argv

    fileName = sys.argv[1]
    tags = ["A", "C", "G", "T"]

    tagA = 0
    tagC = 0
    tagG = 0
    tagT = 0
    
    # Basic input/output
    # The 'r' is for reading the file
    # This is one of the coolest ways to write it because
    # You don't have to close the file.
    with open(fileName, 'r') as f:
        for line in f:
            for token in line:
                if token == tags[0]:
                    tagA += 1
                elif token == tags[1]:
                    tagC += 1
                elif token == tags[2]:
                    tagG += 1
                elif token == tags[3]:
                    tagT += 1


    # Maybe I'm using these print statements a bit
    # too gratuitously.
    # The switch from python 2.7 -> 3.3
    # made print from a statement into a function
    # print "blah" to print("blah")
    print(tagA, tagC, tagG, tagT) 


















main()
