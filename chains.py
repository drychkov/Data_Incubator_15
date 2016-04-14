
import random as rd
import numpy as np

# draw a random link from a bag
def link(N, chain=[]):         # use number of links and the current chain as arguments
    num = rd.randint(1,N)      # draw a link
    while num in chain:        # check if the link is unique. If not, do again
        num = rd.randint(1,N)  
    return num                 # returm the link

def main():
    rd.seed(0) # set the seed for random numbers

    N = 8 # set number of links

    chain = [] # set a chain
    Mseq = []  # set a sequence of sunchains

    # go through all links
    for i in range(N): 
        chain.append(link(N, chain))      # draw a link and add to the chain
        chain.sort()                      # sort the links in the chain
        M = 1                             # set number of subchains
    
        # go through all given links
        for j in range(len(chain)-1):
            dist = chain[j+1] - chain[j]  # find distance between neighbor links
            if dist != 1:                 # if it is not one (closest neighbors)
                M = M + 1                 # increase number of subchains
            
        Mseq.append(M)                    # add number of subchains to the sequence
    
    # print mean and standard deviation    
    print "For N = ", N, ":"
    print "the mean value is", np.mean(Mseq)
    print "the standard deviation is", np.std(Mseq)

if __name__ == '__main__':
  main()



