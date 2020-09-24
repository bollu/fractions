#include <assert.h>
#include <stdio.h>
#include <deque>
#include <functional>
#include <utility>
#include <vector>

using namespace std;
using ll = long long;
using num = ll;

struct Vec;
struct Mat;
struct Tensor;
struct Expr;
struct LFT;

struct OutFile {
public:
    OutFile(const char *path) { f = fopen(path, "w"); }

    OutFile(FILE *f) : f(f) {};

    ~OutFile() { fflush(f); }

    void write(void *p) { fprintf(f, "%p", p); }

    void write(const std::string &s) { fprintf(f, "%s", s.c_str()); }

    void write(int i) { fprintf(f, "%d", i); }

    void write(ll i) { fprintf(f, "%lld", i); }

    void write(char c) {
        fputc(c, f);
        if (c == '\n') {
            for (int i = 0; i < indentLevel; ++i) {
                for (int j = 0; j < 2; ++j) { fputc(' ', f); }
                fputs("┆", f);
                // fputc('|', f);
            }
        }
    }

    void write(const char *s) { while (*s != '\0') { write(*s++); }}

    void indent() { indentLevel++; }

    void dedent() {
        indentLevel--;
        assert(indentLevel >= 0);
    }

private:
    ll indentLevel = 0;
    FILE *f;
};

OutFile &operator<<(OutFile &f, const std::string &s) {
    f.write(s.c_str());
    return f;
}

OutFile &operator<<(OutFile &f, int i) {
    f.write(i);
    return f;
}

OutFile &operator<<(OutFile &f, ll i) {
    f.write(i);
    return f;
}

OutFile &operator<<(OutFile &f, const char *s) {
    f.write(s);
    return f;
}

OutFile &operator<<(OutFile &f, char c) {
    f.write(c);
    return f;
}

OutFile &operator<<(OutFile &f, void *p) {
    f.write(p);
    return f;
}


struct ScopedIndenter {
    ScopedIndenter(OutFile &f, const char *preamble = "") : f(f) {
        f.indent();
        f << "\n" << preamble << "\n";
    }

    ~ScopedIndenter() { f.dedent(); }

    OutFile &f;
};

static OutFile cerr(stderr);
static OutFile cout(stdout);

void debugPrompt(const char *name) {
    cerr << "\n" << name << "[press key]>\n";
    getchar();
}

enum class LFTType {
    Vec, Mat, Tensor
};

struct LFT {
    const LFTType type;

    LFT(LFTType type) : type(type) {};

    template<typename T>
    bool isa() const {
        return type == T::getLFTType();
    }

    template<typename T>
    const T *cast() const {
        assert(isa<T>());
        return (T *) this;
    }

    template<typename T>
    const T *dyn_cast() const {
        if (isa<T>()) {
            return (T *) this;
        }
        return nullptr;
    }

    virtual bool refine() const = 0;

    virtual int branch() const = 0;

    // do I need cons? no idea. page 188
    virtual const Expr *cons(std::function<const Expr *(int)> f) const = 0;

    virtual const Expr *app(std::function<const Expr *(int)> f) const = 0;

    static const LFT *dot(int i, const LFT *a, const LFT *b);

};

enum class ExprType {
    Vec = 'v', Mat = 'm', Tensor = 't'
};

struct Expr {
    const ExprType exprty;

    Expr(ExprType exprty) : exprty(exprty) {};

    virtual const LFT *head() const = 0;

    virtual const Expr *tail(int i) const = 0;
};


struct Vec : public LFT, public Expr {
    int v0, v1;

    Vec(int v0, int v1) : v0(v0), v1(v1), LFT(LFTType::Vec), Expr(ExprType::Vec) {};

    int dot(Vec v) { return v0 * v.v0 + v1 * v.v1; }

    static LFTType getLFTType() { return LFTType::Vec; }

    // sign() = clamp(-1, sgn(v0) + sgn(v1), 1)
    int sign() const {
        if (v0 < 0) {
            return v1 <= 0 ? -1 : 0;
        } else if (v0 == 0) {
            if (v1 < 0) {
                return -1;
            } else if (v1 == 0) {
                return 0;
            } else {
                assert(v1 > 0);
                return 1;
            }
        } else {
            assert(v0 > 0);
            return v1 >= 0 ? 1 : 0;
        }
    }

    Vec scale() const {
        if (v0 % 2 == 0 && v1 % 2 == 0) {
            return Vec(v0 / 2, v1 / 2);
        }
        return *this;
    }

    bool refine() const override {
        // TODO: does ~= mean != ?
        return sign() != 0;
    }

    int branch() const override { return 0; }

    const Expr *app(std::function<const Expr *(int)> f) const override {
        assert(false && "unimplemented for this class");
        return nullptr;
    }

    const Expr *cons(std::function<const Expr *(int)> f) const override {
        return new Vec(*this);
    }

    const LFT *head() const override {
        return new Vec(*this);
    }

    const Expr *tail(int i) const override {
        assert(false && "vec has no tail!");
        exit(1);
    }


    bool operator<(const Vec &other) const;


    void print(OutFile &o) const {
        o << "v(" << v0 << " " << v1 << ")";
    }
};

OutFile &operator<<(OutFile &o, Vec v) {
    v.print(o);
    return o;
}


struct Mat : public LFT {
    const int mat[2][2];

    //   coordinates: (row, col)
    //     |col      col
    //-----+------------------
    // row |a:(0, 0) b:(0, 1)
    // row |c:(1, 0) d:(1, 1)
    int a() const { return mat[0][0]; }

    int b() const { return mat[0][1]; }

    int c() const { return mat[1][0]; }

    int d() const { return mat[1][1]; }

    Vec v0() const { return Vec(a(), b()); }

    Vec v1() const { return Vec(c(), d()); }

    Mat(int a, int b, int c, int d) : mat{{a, b},
                                          {c, d}}, LFT(LFTType::Mat) {}

    Mat(Vec v, Vec w) : mat{v.v0, v.v1, w.v0, w.v1}, LFT(LFTType::Mat) {}

    static LFTType getLFTType() { return LFTType::Mat; }

    static Mat identity() { return Mat(1, 0, 0, 1); }

    Mat transpose() const { return Mat(a(), c(), b(), d()); }

    int determinant() const { return a() * d() - b() * c(); }

    // tame inverse
    Mat inverse() const { return Mat(d(), -b(), -c(), a()); }

    // scaling by 2
    Mat scale() const {
        if (a() % 2 == 0 && b() % 2 == 0 && c() % 2 == 0 && d() % 2 == 0) {
            return Mat(a() / 2, b() / 2, c() / 2, d() / 2);
        }
        return *this;
    }

    // it's all row vector based.
    // [v0 v1] [m00 m01]  = [v0m00 + v1m10; v0m01 + v1m11]
    //         [m10 m11]
    Vec dot(const Vec &v) const {
        return Vec(mat[0][0] * v.v0 + mat[1][0] * v.v1, mat[0][1] * v.v0 + mat[1][1] * v.v1);
    }

    // [[t00 t01]] [[m00 m01]] =
    // [t10 t11]]  [[m10 m11]]
    // [[t00 t01] [t10 t11]] @ [m00 m01]; [[t00 t01] [t10 t11]] @ [m10 m11]]
    Mat dot(const Mat &m) const {
        return Mat(this->dot(m.v0()), this->dot(m.v1()));
    }

    Tensor dot(const Tensor &t) const;

    bool refine() const override {
        const int a = v0().sign(), b = v1().sign();
        return a == b && b != 0;
    }

    int branch() const override { return 1; }

    const Expr *app(std::function<const Expr *(int)> g) const override;

    bool operator<(const Vec &x) const {
        return (this->v0() < x) && (this->v1() < x);
    }

    bool operator<(const Mat &m) const {
        return (*this < m.v0()) && (*this < m.v1());
    }

    static bool disjoint(const Mat &m, const Mat &n) {
        return (m < n) || (n < m);
    }

    bool operator==(const Mat &other) const {
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                if (mat[i][j] != other.mat[i][j]) {
                    return false;
                }
            }
        }
        return true;
    }

    const Expr *cons(std::function<const Expr *(int)> f) const override;

    void print(OutFile &o) const {
        o << "m(" << v0() << " " << v1() << ")";
    }
};

OutFile &operator<<(OutFile &o, Mat m) {
    m.print(o);
    return o;
}

bool Vec::operator<(const Vec &other) const {
    return Mat(*this, other).determinant() < 0;
}


const Mat spos(1, 0, 0, 1);
const Mat sinf(1, -1, 1, 1);
const Mat sneg(0, 1, -1, 0);
const Mat szer(1, 1, -1, 1);

const Mat ispos(spos.inverse());
const Mat isinf(sinf.inverse());
const Mat isneg(sneg.inverse());
const Mat iszer(szer.inverse());

// see section 9.1
const Mat dneg(1, 1, 0, 2);
const Mat dzer(3, 1, 1, 3);
const Mat dpos(2, 0, 1, 1);

const Mat idneg(dneg.inverse());
const Mat idzer(dzer.inverse());
const Mat idpos(dpos.inverse());

struct Tensor : public LFT {
    const Mat ms[2];
    const int n = 0;  // for absorption

    Tensor(Mat m0, Mat m1, int n = 0)
            : ms{m0, m1}, LFT(LFTType::Tensor), n(n) {};

    Tensor bumpn() const { return Tensor(ms[0], ms[1], n + 1); }

    static LFTType getLFTType() { return LFTType::Tensor; }

    Mat m0() const { return ms[0]; }

    Mat m1() const { return ms[1]; }

    Tensor inverse() const {
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                for (int k = 0; k < 2; ++k) {
                    if (ms[i].mat[j][k] % 2 != 0) {
                        return *this;
                    }
                }
            }
        }
        return Tensor(ms[0].scale(), ms[1].scale());
    }

    Tensor transpose() const {
        Vec v00 = m0().v0();
        Vec v01 = m0().v1();
        Vec v10 = m1().v0();
        Vec v11 = m1().v1();
        return Tensor({v00, v10}, {v01, v11});
    }

    // tensor products: tleftv, tleftm, trightv, trightm
    Mat left(Vec v) const { return transpose().right(v); }

    Mat right(Vec v) const { return Mat(m0().dot(v), m1().dot(v)); }

    Tensor left(Mat m) const { return (transpose().right(m)).transpose(); }

    Tensor right(Mat m) const { return Tensor(m0().dot(m), m1().dot(m)); }

    bool refine() const override {
        const Vec v = m0().v0();
        const Vec w = m0().v1();
        const Vec x = m1().v0();
        const Vec y = m1().v1();
        const int a = v.sign(), b = w.sign(), c = x.sign(), d = y.sign();
        return a == b && b == c && c == d && d != 0;
    }

    int branch() const override { return 2; }

    const Expr *app(std::function<const Expr *(int)> g) const override;

    Tensor scale() const {
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                for (int k = 0; k < 2; ++k) {
                    if (ms[i].mat[j][k] % 2 != 0) {
                        return *this;
                    }
                }
            }
        }
        return Tensor(m0().scale(), m1().scale());
    }

    const Expr *cons(std::function<const Expr *(int)> f) const override;

    void print(OutFile &o) const {
        o << "t(" << m0() << " " << m1() << ")";
    }
};

OutFile &operator<<(OutFile &o, Tensor t) {
    t.print(o);
    return o;
}


Tensor Mat::dot(const Tensor &t) const {
    return Tensor(this->dot(t.m0()), this->dot(t.m1()));
};

const Tensor tadd(Mat(0, 0, 1, 0), Mat(1, 0, 0, 1));
const Tensor tsub(Mat(0, 0, 1, 0), Mat(-1, 0, 0, 1));
const Tensor tmul(Mat(1, 0, 0, 0), Mat(0, 0, 0, 1));
const Tensor tdiv(Mat(0, 0, 1, 0), Mat(0, 1, 0, 0));


OutFile &operator<<(OutFile &o, const LFT &lft) {
    if (lft.isa<Vec>()) {
        lft.cast<Vec>()->print(o);
    } else if (lft.isa<Mat>()) {
        lft.cast<Mat>()->print(o);
    } else {
        assert(lft.isa<Tensor>());
        lft.cast<Tensor>()->print(o);
    }
    return o;
}

OutFile &operator<<(OutFile &o, const Expr &e) {
    return o << "Expr(" << (char) e.exprty << " " << (void *) e.head() << " = " << *e.head() << ")";
}


struct ExprThunk {
    ExprThunk(std::function<const Expr *()> thunk) : thunk(thunk) {};

    const Expr *get() const {
        if (!value) {
            value = thunk();
        }
        return value;
    }

    static ExprThunk thunkify(const Expr *e) {
        return ExprThunk([e]() { return e; });
    }

private:
    std::function<const Expr *()> thunk;
    mutable const Expr *value = nullptr;
};

struct MatExpr : public Expr {
    const Mat m;
    const ExprThunk ethunk;

    // const Expr *e;
    MatExpr(Mat m, ExprThunk ethunk) : m(m), ethunk(ethunk), Expr(ExprType::Mat) {}

    const LFT *head() const override { return new Mat(m); }

    const Expr *tail(int i) const override {
        assert(i == 1);
        return ethunk.get();
    }
};

const Expr *Mat::cons(std::function<const Expr *(int)> f) const {
    return new MatExpr(*this, ExprThunk([f]() { return f(1); }));
}

// counter for each tensor, fair absorption (section 11.6)
struct TensorExpr : public Expr {
    Tensor t;
    int counter;
    const ExprThunk lthunk;
    const ExprThunk rthunk;

    TensorExpr(Tensor t, int counter, const ExprThunk lthunk,
               const ExprThunk rthunk)
            : Expr(ExprType::Tensor), t(t), counter(counter), lthunk(lthunk), rthunk(rthunk) {};

    const LFT *head() const override { return new Tensor(t); }

    const Expr *tail(int i) const override {
        assert(i == 1 || i == 2);
        return i == 1 ? lthunk.get() : rthunk.get();
    }
};

const Expr *Tensor::cons(std::function<const Expr *(int)> f) const {
    return new TensorExpr(*this, this->n,
                          ExprThunk([f]() { return f(1); }),
                          ExprThunk([f]() { return f(2); }));
}


// Page 185
const LFT *LFT::dot(int i, const LFT *l, const LFT *r) {
    assert(i == 1 || i == 2);
    if (i == 1) {
        assert(l->isa<Mat>() || l->isa<Tensor>());

        if (l->isa<Mat>()) {
            const Mat *m = l->cast<Mat>();
            if (r->isa<Vec>()) {
                const Vec *v = r->cast<Vec>();
                return new Vec(m->dot(*v).scale());
            } else if (r->isa<Mat>()) {
                const Mat *n = r->cast<Mat>();
                return new Mat(m->dot(*n).scale());
            } else {
                assert(r->isa<Tensor>());
                const Tensor *t = r->cast<Tensor>();
                return new Tensor(m->dot(*t).scale());
            }
        } else {
            assert(l->isa<Tensor>());
            const Tensor *t = l->cast<Tensor>();
            assert(r->isa<Vec>() || r->isa<Mat>());
            if (r->isa<Vec>()) {
                const Vec *v = r->cast<Vec>();
                return new Mat(t->left(*v).scale());
            } else {
                assert(r->isa<Mat>());
                const Mat *m = r->cast<Mat>();
                if (*m == Mat::identity()) {
                    return t;
                } else {
                    return new Tensor(t->left(*m).scale().bumpn());
                }
            }
        };
    } else {
        assert(i == 2);
        assert(l->isa<Tensor>());
        const Tensor *t = l->cast<Tensor>();

        // same as previous code, just left switched to right
        assert(r->isa<Vec>() || r->isa<Mat>());
        if (r->isa<Vec>()) {
            const Vec *v = r->cast<Vec>();
            return new Mat(t->right(*v).scale());
        } else {
            assert(r->isa<Mat>());
            const Mat *m = r->cast<Mat>();
            if (*m == Mat::identity()) {
                return t;
            } else {
                return new Tensor(t->right(*m).scale().bumpn());
            }
        }
    }
}

const Expr *Mat::app(std::function<const Expr *(int)> g) const {
    const LFT *l = this;
    const LFT *r = g(1)->head();
    const LFT *lr = LFT::dot(1, l, r);
    return LFT::dot(1, this, g(1)->head())->cons([g](int i) {
        return g(1)->tail(i);
    });
}

const Expr *Tensor::app(std::function<const Expr *(int)> g) const {
    ScopedIndenter indent(cerr, __PRETTY_FUNCTION__);
    cerr << "- g(1): " << *g(1) << "\n";
    cerr << "- g(2): " << *g(2) << "\n";

    const int c = g(1)->head()->branch();
    cerr << "- c " << c << "\n";
    const auto h = [g, c](int i) {
        if (i <= c) {
            return g(1)->tail(i);
        } else {
            return g(2)->tail(i - c);
        }
    };
    const LFT *dotTwo = LFT::dot(2, this, g(2)->head());
    cerr << "- dotTwo " << *dotTwo << "\n";
    const LFT *dotOne = LFT::dot(1, dotTwo, g(1)->head());
    cerr << "- dotOne " << *dotOne << "\n";
    cerr << "- dotOne->cons(h) " << *dotOne->cons(h) << "\n";
    return dotOne->cons(h);
}

// digit matrices: section 9.1
struct Digits {
    const int d0;
    const int d1;

    Digits(int d0, int d1) : d0(d0), d1(d1) {}

    // n = d0, c = d1
    Mat to_mat() const {
        const int pow2 = 1 << d0;
        return Mat(pow2 + d1 + 1, pow2 - d1 - 1, pow2 + d1 - 1, pow2 - d1 + 1);
    }
};

Expr *erec(const Expr *e) {
    return new MatExpr(Mat(0, 1, 1, 0), ExprThunk::thunkify(e));
}

struct Uefp {
    const Digits digits;
    const Expr *e;

    Uefp(Digits digits, const Expr *e) : digits(digits), e(e) {};

    Uefp urec() const { return Uefp(Digits(digits.d0, -digits.d1), erec(e)); }

    const Expr *to_expr() const {
        return new MatExpr(digits.to_mat(), ExprThunk::thunkify(e));
    }
};

// signed exact floating point
enum class SefpType {
    Positive, Negative, Inf, Zero
};

struct Sefp {
    const SefpType type;
    const Uefp uefp;

    Sefp(SefpType ty, Uefp uefp) : type(type), uefp(uefp) {}

    Sefp srec() const { return Sefp(type, uefp.urec()); }

    Mat to_mat() const {
        switch (type) {
            case SefpType::Positive:
                return spos.dot(uefp.digits.to_mat());
            case SefpType::Negative:
                return sneg.dot(uefp.digits.to_mat());
            case SefpType::Inf:
                return sinf.dot(uefp.digits.to_mat());
            case SefpType::Zero:
                return szer.dot(uefp.digits.to_mat());
        }
    }
};

// fair (11.9)
int strategyf(Tensor t, int i) { return (i % 2) + 1; }

// refine (11.10)
int strategyr(Tensor t, int i) {
    return Mat::disjoint(t.transpose().m0(), t.transpose().m1()) ? 2 : 1;
}

// information overlap (11.10)
int strategyo(Tensor t, int i) {
    if (t.refine()) {
        return strategyr(t, i);
    } else {
        return strategyf(t, i);
    }
}

// decision (11.11)
bool decision(int i, const LFT *lft) {
    if (i == 1) {
        if (lft->isa<Mat>()) {
            return true;
        } else if (const Tensor *t = lft->dyn_cast<Tensor>()) {
            return strategyo(*t, i) == 1;
        } else {
            assert(false && "must be matrix or tensor");
        }
    } else {
        assert(i == 2);
        const Tensor *t = lft->cast<Tensor>();
        return strategyo(*t, i) == 2;
    }
}

// ===Normalization functions===
// Absorption function (11.4)
const Expr *ab(const LFT *k, const Expr *e, bool b);

Sefp sem(const Expr *e, int i);

Uefp dem(Digits d, const Expr *e, int i);

template<typename T>
std::function<T(int)> one(T t) {
    return [t](int i) {
        assert(i == 1);
        return t;
    };
}

// Sign emission (11.1)
Sefp sem(const Expr *e, int i) {
    const LFT *l = e->head();

    debugPrompt(__PRETTY_FUNCTION__);
    ScopedIndenter indent(cerr, __PRETTY_FUNCTION__);
    cerr << *e << "\n";
    auto f = [e, l](int d) { 
        ScopedIndenter indent(cerr, __PRETTY_FUNCTION__);
        cerr << "- d:" << d << "\n";
        cerr << "- e->tail(d):" << *e->tail(d) << "\n";
        cerr << "- decision(d,l):" << decision(d,l) << "\n";
        const Expr *out = ab(l, e->tail(d), decision(d, l));
        cerr << "- ab(l, e->tail(d), decision(d, l)):" << *out << "\n";
        return out;
    };
    cerr << "- l: " << *l << "\n";
    cerr << "- isPos: " << ispos << "\n";
    cerr << "- LFT::dot(1, &ispos, l): " << *LFT::dot(1, &ispos, l) << "\n";
    cerr << "- LFT::dot(1, &ispos, l)->refine(): " << LFT::dot(1, &ispos, l)->refine() << "\n";
    if (LFT::dot(1, &ispos, l)->refine()) {
        return Sefp(SefpType::Positive,
                    dem(Digits(0, 0), ispos.app(one(e)), i));
    } else if (LFT::dot(1, &isneg, l)->refine()) {
        return Sefp(SefpType::Negative,
                    dem(Digits(0, 0), isneg.app(one(e)), i));
    } else if (LFT::dot(1, &iszer, l)->refine()) {
        return Sefp(SefpType::Negative,
                    dem(Digits(0, 0), iszer.app(one(e)), i));
    } else if (LFT::dot(1, &isinf, l)->refine()) {
        return Sefp(SefpType::Negative,
                    dem(Digits(0, 0), isinf.app(one(e)), i));
    } else {
        const Expr *lapp = l->app(f);
        cerr << "- l->app(f):" << *lapp << "\n";
        return sem(l->app(f), i);
    }
}

// Digit emission (11.2)
Uefp dem(Digits d, const Expr *e, int j) {
    const LFT *l = e->head();
    auto f = [e, l](int d) { return ab(l, e->tail(d), decision(d, l)); };
    if (j == 0 || l->isa<Vec>()) {
        return Uefp(d, e);
    } else if (LFT::dot(1, &idneg, l)->refine()) {
        return dem(Digits(d.d0 + 1, 2 * d.d1 - 1),
                   LFT::dot(1, &idneg, l)->app(one(e)), j - 1);
    } else if (LFT::dot(1, &idpos, l)->refine()) {
        return dem(Digits(d.d0 + 1, 2 * d.d1 + 1),
                   LFT::dot(1, &idpos, l)->app(one(e)), j - 1);
    } else if (LFT::dot(1, &idzer, l)->refine()) {
        return dem(Digits(d.d0 + 1, 2 * d.d1),
                   LFT::dot(1, &idzer, l)->app(one(e)), j - 1);
    } else {
        return dem(d, l->app(f), j);
    }
}

// Absorption function (11.4)
const Expr *ab(const LFT *k, const Expr *e, bool b) {
    if (!b) {
        return Uefp(Digits(0, 0), e).to_expr();
    } else if (k->isa<Tensor>() && e->head()->isa<Tensor>()) {
        return Uefp(dem(Digits(0, 1), e, 1)).to_expr();
    } else {
        return e;
    }
}

std::string mshow(Mat m);

std::string sshow(deque<int> numbers);

std::deque<int> scientific(Mat mat, int i);

std::deque<int> mantissa(int i, int n, Mat mat);

std::string eshow(const Expr *e, int i) { return mshow(sem(e, i).to_mat()); }

std::string mshow(Mat m) {
    const int d = m.determinant();
    Vec v = m.v0().scale();
    const int p = v.v0;
    const int q = v.v1;
    if (d == 0) {
        if (q == 1) {
            return std::to_string(v.v0);
        } else {
            return std::to_string(p) + "/" + std::to_string(q);
        }
    } else {
        return sshow(scientific(m, 0));
    }
}

int powi(int base, int exp) {
    int ans = 1;
    for (int i = 0; i < exp; ++i) {
        ans *= base;
    }
    return ans;
}

std::tuple<int, int, int> normalize(int e, int l, int v) {
    if (l > 0 && abs(v) < powi(10, l - 1)) {
        return normalize(e - 1, l - 1, v);
    } else {
        return std::make_tuple(e, l, v);
    }
}

std::string sshow(deque<int> numbers) {
    if (numbers.size() == 0) {
        return "unbounded";
    }
    const int e = numbers[0];

    int f = 0;
    for (int i = numbers.size() - 1; i >= 1; i--) {
        f = f * 10 + numbers[i];
    }

    int h, l, v;
    std::tie(h, l, v) = normalize(e, numbers.size() - 1, f);

    std::string out = "";
    if (v < 0) {
        out += "-";
    }
    if (v == 0) {
        out += "0";
    } else {
        out += "0" + std::to_string(abs(v));
    }
    out += "e";
    out += std::to_string(h);
    return out;
}

std::deque<int> scientific(Mat m, int n) {
    if (m.inverse().dot(Vec(1, 0)).refine()) {
        return {};
    } else if (szer.inverse().dot(m).refine()) {
        deque<int> ss = mantissa(-9, 9, m);
        ss.push_front(n);
        return ss;
    } else {
        return scientific(Mat(1, 0, 0, 10).dot(m), n + 1);
    }
}

std::deque<int> mantissa(int i, int n, Mat mat) {
    Mat d(i + 1, 10, i - 1, 10);
    if (d.inverse().dot(mat).refine()) {  // c i
        Mat e(10, 0, -i, 1);
        std::deque<int> ss = mantissa(-9, 9, e);
        ss.push_front(i);
        return ss;
    } else if (i < n) {
        return mantissa(i + 1, n, mat);
    } else {
        return {};
    }
}

// TODO: elementary functions.

const Expr *eiterate(std::function<Mat(int)> f, int n) {
    // this needs to be lazy, will instantly blow up
    return new MatExpr(f(n),
                       ExprThunk([f, n]() { return eiterate(f, n + 1); }));
}

const Expr *eiteratex(std::function<Tensor(int)> f, int n, const Expr *x) {
    return new TensorExpr(
            f(n), 0, ExprThunk::thunkify(x),
            ExprThunk([f, n, x]() { return eiteratex(f, n + 1, x); }));
}

const Expr *rollover(int a, int b, int c) {
    const int d = 2 * (b - a) + c;
    if (d >= 0) {
        return new MatExpr(
                dneg, ExprThunk([a, d, c]() { return rollover(4 * a, d, c); }));
    } else {
        return new MatExpr(
                dpos, ExprThunk([b, d, c]() { return rollover(-d, 4 * b, c); }));
    }
}

// p/q
const Expr *esqrtrat(int p, int q) { return rollover(p, q, p - q); }

// sqrt(x) for any x.
const Expr *esqrtspos(const Expr *e) {
    return eiteratex(
            [](int n) { return Tensor(Mat(1, 0, 2, 1), Mat(1, 2, 0, 1)); }, 0, e);
}

// elogpos(x) = log(S+(x))
const Expr *elogpos(const Expr *e) {
    return eiteratex(
            [](int n) {
                if (n == 0) {
                    return Tensor(Mat(1, 0, 1, 1), Mat(-1, 1, -1, 0));
                } else {
                    return Tensor(Mat(n, 0, 2 * n + 1, n + 1),
                                  Mat(n + 1, 2 * n + 1, 0, n));
                };
            },
            0, e);
}

// ee = natural exp
const Expr *ee() {
    return eiterate(
            [](int n) { return Mat(2 * n + 2, 2 * n + 1, 2 * n + 1, 2 * n); }, 0);
}

// Pi: section 10.2.4

const Expr *epi() {
    const Expr *eomega = eiterate(
            [](int n) {
                if (n == 0) {
                    return Mat(6795705, 213440, 6795704, 213440);
                } else {
                    const int b = (2 * n - 1) * (6 * n - 5) * (6 * n - 1);
                    const int c = b * (545140134 * n + 13591409);
                    const int d = b * (n + 1);
                    const int e = 10939058860032000 * powi(n, 4);
                    return Mat(b, c, d, e);
                }
            },
            0);
    return new TensorExpr(tdiv, 0, ExprThunk::thunkify(esqrtrat(1005, 1)),
                          ExprThunk::thunkify(eomega));
}

// tangent: Section 10.2.5
const Expr *etanszer() { assert(false && "unimplemented"); }

// arc tangent: Section 10.2.6
const Expr *earctanszer() { assert(false && "unimplemented"); }

int main() {
    for (int i = 0; i < 10; ++i) {
        cerr << "pi upto " << i << "places: " << eshow(epi(), i);
    }
    return 0;
}
