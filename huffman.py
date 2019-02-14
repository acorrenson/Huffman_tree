
# Let U the description of an random emitter
U = {
    'a': 0.5,
    'b': 0.2,
    'c': 0.2,
    'd': 0.1
}

def find_two_min(u):
    """
        Find the 2 characters emitted with the 
        lowest frequency by an emitter u
    """

    k1, m1 = ('', 1)
    k2, m2 = ('', 1)

    for k in u:
        if u[k] <= m2:
            m1 = m2
            k1 = k2
            m2 = u[k]
            k2 = k
        elif u[k] <= m1:
            m1 = u[k]
            k1 = k

    return ((k1, m1), (k2, m2))


def find_min(u):
    """
        Find the character emitted with the 
        lowest frequency by an emitter u
    """
    
    m = 1
    c = ''
    for k in u:
        if u[k] <= m:
            m = u[k]
            c = k
    return (c, m)


def huff(u, n=None):
    """
    @brief      Apply huffman algo. according to the 
        description of an emitter
    
    @param      u     Description of the emitter (<dict>)
    
    @return     The optimal code tree (freq, left, right) (<tuple)
    """

    # init the first sub-tree
    if n is None:
        c1, c2 = find_two_min(u)
        k1, p1 = c1
        k2, p2 = c2
        
        del u[k1]
        del u[k2]

        # first Node (deeper Node)
        N = (p1+p2, c1, c2)    

    # recursive part
    def rec_huff(u, N):
        if len(u) > 0:
            # find char with lowest freq.
            char = find_min(u)
            # remove this char from the rest
            del u[char[0]]
            # new node
            new_N = (char[1] + N[0], char, N)
            # expand the code tree
            return rec_huff(u, new_N)
        else:
            # no more char to add to the tree
            # return the root Node
            return N

    return rec_huff(u, N)


def pprint(t, i=0):
    """
    @brief      Display a tree in the console.
    
    @param      t     the tree (<tuple>)
    @param      i     { parameter_description }
    """

    if len(t) >= 3:
        print(i*'  ' + str(t[0]))
        pprint(t[1], i+1)
        pprint(t[2], i+1)
    elif len(t) < 3:
        print(i*'  ' + str(t))

if __name__ == '__main__':
    h = huff(U)
    pprint(h)
