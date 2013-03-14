##############"
# Apo
# 13/03/13
# My teacher tells us
# to put flowerboxes here usually.
# This probably is not the right format.
#
##############

import sys

def main():
    # usage: python3 rosa_rna.py FILENAME
    # Everything after the (guesing) python3 bit is counted into
    # some kind of list in sys.argv

    fileName = sys.argv[1]
    tags = ["A", "C", "G", "T"]
    string = ""

    # Basic input/output
    # The 'r' is for reading the file
    # This is one of the coolest ways to write it because
    # You don't have to close the file.
    with open(fileName, 'r') as f:
        for line in f:
            for token in line:
                if token == tags[0]:
                    string += tags[0] 
                elif token == tags[1]:
                    string += tags[1]
                elif token == tags[2]:
                    string += tags[2] 
                elif token == tags[3]:
                    string += "U" 


    print(string) 
    out_file = open("something.txt", "w")
    out_file.write(string)
    out_file.close()

















main()
