from sympy import *

def S(name): return Symbol(name)

# row:[v0 v1]
def Vec(name):
    return (S(name + "0"), S(name + "1"))


# row: [[v00 v01]]
#      [[v10 v11]]
def Mat(name):
     return ((S(name + "00"), S(name + "01")), (S(name + "10"), S(name + "11")))
    # return (Vec("0" + name), Vec(name + "1"))

# tensor: [[[t000 t001]] ; [[t100; t101]]]
#         [[[t010 t011]] ; [[t110; t111]]]
def Tensor(name):
    return (((S(name + "000"), S(name + "001")), (S(name + "010"), S(name + "011"))),
            ((S(name + "100"), S(name + "101")), (S(name + "110"), S(name + "111"))))
  
    #return (Mat(name + "["), Mat(name + "]"))

def trans(x): ((a, b), (c, d)) = x; return ((a, c), (b, d))

# [e f] [[a b]] = [ea+fc eb+fd]
#       [[c d]]
def mdotv(m, v):
    ((a, b), (c, d)) = m
    (e, f) = v
    return (a*e+c*f, b*e+d*f)

    

# [[n00 n01]] [[m00 m01]] = [[n00m00+n01m10 n00m01+n01m11]]
# [[n10 n11]] [[m10 m11]] = [[n10m00+n11m10 n10m01+n11m11]
def mdotm(m, n):
   (v, w) = n
   return (mdotv(m, v), mdotv(m, w))

def mdott(m, t):
   (n, o) = t
   return (mdotm(m, n), mdotm(m, o))

# [v0 v1] ([[t000 t001]]; [[t100 t101]])^T
#         ([[t010 t011]]; [[t110 t111]])
# = 
# [v0 v1] ([[t000 t001]]; [[t010 t011]])^T
#         ([[t100 t101]]; [[t110 t111]])
# = 
# [v0t00+v1t100 v0t001+v1t101]; [v0t010+v1t110 v0t001v1t111]
# =
# [[v0t00+v1t100 v0t001+v1t101]]
# [[v0t010+v1t110 v0t001v1t111]]

def tleftv(t, v):
    return trightv(trans(t), v)

def tleftm(t, m):
    return trans(trightm(trans(t), m))

# ((t000*v0 + t010*v1, t001*v0 + t011*v1), (t100*v0 + t110*v1, t101*v0 + t111*v1))
# [v0 v1] ([[t000 t001]]; [[t100 t101]] 
#         ([[t010 t011]]; [[t110 t111]]
# = ( [t00v0+t010v1 t001v0+t011v1]; [t100v0+t110v1, t101v0 +t111v1]) 
# = [[t00v0+t010v1   t001v0+t011v1]]
#   [[t100v0+t110v1, t101v0 +t111v1]]  
def trightv(t, v):
    (m, n) = t
    return (mdotv(m, v), mdotv(n, v))

def trightm(t, o):
    (m, n) = t
    return (mdotm(m, o), mdotm(n, o))


# row multiplication
def matmatmul_ref(m, n):
    out = [[0, 0], [0, 0]]
    for i in range(2):
        for j in range(2):
            for k in range(2):
                out[i][j] += m[i][k] * n[k][j]
    return ((out[0][0], out[0][1]), (out[1][0], out[1][1]))

# row multiplication
def vecmatmul_ref(v, m):
    out = [0, 0]
    for i in range(2):
        for j in range(2):
            out[i] += v[j]*m[j][i]
    return (out[0], out[1])

s = Tensor("s"); t = Tensor("t");
v = Vec("v"); w = Vec("w");
m = Mat("m"); n = Mat("n");

assert(mdotv(m, v) == vecmatmul_ref(v, m))
assert(mdotm(m, n) == matmatmul_ref(n, m))
assert(mdott(m, t) == (matmatmul_ref(t[0], m), matmatmul_ref(t[1], m)))
assert(trightv(t, v) == (vecmatmul_ref(v, t[0]), vecmatmul_ref(v, t[1])))
assert(trightm(t, m) == (matmatmul_ref(m, t[0]), matmatmul_ref(m, t[1])))

def mapPair(f, x): (x0, x1) = x; return (f(x0), f(x1))
assert(tleftv(t, v) == (vecmatmul_ref(v, trans(t)[0]), vecmatmul_ref(v, trans(t)[1])))
assert(tleftm(t, m) == trans((matmatmul_ref(m, trans(t)[0]), matmatmul_ref(m, trans(t)[1]))))

