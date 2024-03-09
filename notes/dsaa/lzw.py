import copy


# out = encrypt("banana_bandana", t)
# out: 103604532


def mktable(chars):
    i = 0
    t = {}
    for c in chars:
        t[c] = len(t)
    return t

def encrypt(str, table):
    s = ""
    out = ""
    table = copy.deepcopy(table)
    for c in str:
        if s + c in table:
            s += c
            continue
        else:
            out += "%s" % (table[s], )
            table[s + c] = len(table)
            s = c

    out += "%s" % (table[s])
    return out

# https://www2.cs.duke.edu/csed/curious/compression/lzw.html#references
def decrypt(inp, t):
    out = ""
    s = ""

    trev = { str(v):k for (k, v) in t.items() }

    print ("trev: %s" % trev)
    newidx = len(t)

    prevcode = inp[0]
    out += trev[prevcode]

    for curcode in str(inp[1:]):

        entry = trev[curcode]
        out += entry

        ch = entry[0]

        trev[str(newidx)] = trev[prevcode] + ch 


        print ("curcode: %s | entry: %s | out: %s || prevcode: %s |  ch: %s | trev[%s] = %s " %
               (curcode, entry, out, prevcode, ch, str(newidx), trev[str(newidx)]))
        newidx += 1


        prevcode = curcode

    return out



if __name__ == "__main__":
    t = mktable("abdn_");
    enc = encrypt("banana_bandana", t)

    print("enc: %s" % enc)
    print("dec: %s" % decrypt(enc, t))



