// Inspired by anime.js
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <map>

#define TAB "    "

using ll = long long;

using accessor = const char *;

enum class valtype { tfloat, tvar, tmul, tadd, tsub, tundefined };
struct val {
    valtype type;
    float f_;
    const char *var_;
    val *inner[2];

    static val binop(valtype t, val *l, val *r) { 
        val v; v.type = t; v.inner[0] = l; v.inner[1] = r; return v;
    }

    val() : f_(-42), var_("UNDEFINED"), type(valtype::tundefined) { }

    public:
    val(const val &other) = default;
    val(int i) : f_(i), var_(nullptr), type(valtype::tfloat) {}
    val(float f) : f_(f), var_(nullptr), type(valtype::tfloat) {}
    val(double d) : f_(d), var_(nullptr), type(valtype::tfloat) {}
    val(const char *var) : var_(var), type(valtype::tvar) {}
    static val add(val *l, val *r) { return binop(valtype::tadd, l, r); }
    static val sub(val *l, val *r) { return binop(valtype::tsub, l, r); }
    static val mul(val *l, val *r) { return binop(valtype::tmul, l, r); }
};

val operator +(val l, val r) { return val::add(new val(l), new val(r)); }
val operator -(val l, val r) { return val::sub(new val(l), new val(r)); }
val operator *(val l, val r) { return val::mul(new val(l), new val(r)); }

struct anim { 
    const char *name; float duration;
    anim(const char *name, float duration) : name(name), duration(duration) {}
    virtual ~anim() {}
};

struct anim_const : public anim { 
    val v; accessor acc; 
    anim_const(const char *name, accessor acc, val v): 
        anim(name, 0), v(v), acc(acc) {}
};

struct anim_slowend : public anim { 
    accessor acc;
    val end;
    anim_slowend(const char *name, accessor acc, float duration, val end) : 
        anim(name, duration), acc(acc), end(end) {};
};

struct anim_wait : public anim { 
    anim_wait(const char *name, float duration) : anim(name, duration) {}
};

struct anim_sequence : public anim {
    static const int MAX_ANIMS_SEQUENCE = 100;
    anim *anims[MAX_ANIMS_SEQUENCE];
    int len;
    anim_sequence(const char *name) : anim(name, 0.0), len(0) {}

    void seq(anim *a) { assert(a); anims[len++] = a; duration += a->duration; }

};

struct anim_parallel : public anim {
    static const int MAX_ANIMS_PARALLEL = 100;
    anim *anims[MAX_ANIMS_PARALLEL];
    int len;
    anim_parallel(const char *name) : anim(name, 0), len(0) {}
    void par(anim *a) { 
        assert(a);
        anims[len++] = a;
        duration = std::max (duration, a->duration);
    }
};


int indent(char *s, const int depth) {
    int oix = 0;
    for(int i = 0; i < depth; ++i) { oix += sprintf(s + oix, "%s", TAB); }
    return oix;
}

int compile_accessor(const accessor acc, char *os) {
    return sprintf(os, "%s", acc);
}

int compile_val(const val v, char *os) { 
    switch(v.type) {
        case valtype::tfloat: return sprintf(os, "%f", v.f_); 
        case valtype::tvar: return sprintf(os, "inp.%s", v.var_); 
        case valtype::tmul:
        case valtype::tadd:
        case valtype::tsub: {
            ll oix = 0;
            oix += sprintf(os + oix, "(");
            oix += compile_val(*v.inner[0], os + oix);
            oix += sprintf(os + oix, ")");
            oix += sprintf(os + oix, "%c", 
                    v.type == valtype::tmul ? '*' : 
                    v.type == valtype::tadd ? '+' : 
                    v.type == valtype::tsub ? '-' : '#');
            oix += sprintf(os + oix, "(");
            oix += compile_val(*v.inner[1], os + oix);
            oix += sprintf(os + oix, ")");
            return oix;
        }
    case valtype::tundefined: { 
      assert(false && "uninitialized value!");
      exit(1);
    }
            
    }
}


int write_set_accessor_to_val(char *os, const accessor acc, const val v) {
    ll oix = 0;
    // in.<accessor> = val;\n
    oix += sprintf(os + oix, TAB "inp.");
    oix += compile_accessor(acc, os + oix);
    oix += sprintf(os + oix, " = ");
    oix += compile_val(v, os + oix);
    oix += sprintf(os + oix, ";\n");
    return oix;
}

// unk = unknown
void compile_anim_cases(const float time_start, const float parent_time_end, const anim *aunk, const int depth, char *os, ll &oix, 
        std::map<accessor, val> &finalvals) {

    oix += indent(os + oix, depth);
    oix += sprintf(os + oix, "if (inp.t >= %f && inp.t <= %f) { // start %s\n", 
            time_start, time_start + aunk->duration, aunk->name);

    if (const anim_sequence *aseq = dynamic_cast<const anim_sequence*>(aunk)) {
        float seq_time_start = time_start;
        for(int i = 0; i < aseq->len; ++i) {
            const anim *acur = aseq->anims[i];
            compile_anim_cases(seq_time_start, time_start + aseq->duration, acur, depth+1, os, oix, finalvals);
            seq_time_start += acur->duration; 
        }
    } else if (const anim_parallel *apar = dynamic_cast<const anim_parallel*>(aunk)) {
        for(int i = 0; i < apar->len; ++i) {
            const anim *acur = apar->anims[i];
            compile_anim_cases(time_start, time_start + apar->duration, acur, depth+1,os, oix, finalvals);
        }
    } else if (const anim_const *aconst = dynamic_cast<const anim_const*>(aunk)) {
        // in.<accessor> = val;\n
        oix += indent(os + oix, depth);
        oix += write_set_accessor_to_val(os + oix, aconst->acc, aconst->v);
        finalvals[aconst->acc] = aconst->v;
    } else if (const anim_wait *_ = dynamic_cast<const anim_wait*>(aunk)) {
        // do nothing
    } else if (const anim_slowend *aslowend = dynamic_cast<const anim_slowend*>(aunk)) {
        // need start value which is implicitly defined.
        auto it = finalvals.find(aslowend->acc);
        if (it == finalvals.end()) {
            char accname[1024];
            (void) compile_accessor(aslowend->acc, accname);
            fprintf(stderr, "ERROR: unable to find value |%s| required by animation |%s|\n",
                    accname, aslowend->name);
            exit(1);
        } 

        const val prev_final = it->second;
        const val t = val("t") - val(time_start);
        const val t01lin = aslowend->duration == 0 ? 1 : (1.0 / aslowend->duration) * t;
        // https://easings.net/
        const val t01 = 1 - (1 - t01lin) * (1 - t01lin) * (1 - t01lin);
        const val v = prev_final * (1 - t01) + aslowend->end * t01;

        // in.<accessor> = val;\n
        oix += indent(os + oix, depth);
        oix += write_set_accessor_to_val(os + oix, aslowend->acc, v);
        // oix += sprintf(os + oix, TAB "inp.");
        // oix += compile_accessor(aslowend->acc, os + oix);
        // oix += sprintf(os + oix, " = ");
        // oix += compile_val(v, os + oix);
        // oix += sprintf(os + oix, ";\n");

        finalvals[aslowend->acc] = aslowend->end;
    } else {
        assert(false && "unhandled animation.");
    }

    // write else part.
    if (const anim_slowend *aslowend = dynamic_cast<const anim_slowend*>(aunk)) {
        oix += indent(os + oix, depth);
        oix += sprintf(os + oix, "} else if (inp.t >= %f && inp.t <= %f) { // elseif %s\n", time_start, parent_time_end, aunk->name);
        oix += indent(os + oix, depth);
        oix += write_set_accessor_to_val(os + oix, aslowend->acc, aslowend->end);

    } else if (const anim_const *aconst = dynamic_cast<const anim_const*>(aunk)) {
        oix += indent(os + oix, depth);
        oix += sprintf(os + oix, "} else if (inp.t >= %f && inp.t <= %f) { // elseif %s\n", time_start, parent_time_end, aunk->name);
        oix += indent(os + oix, depth);
        oix += write_set_accessor_to_val(os + oix, aconst->acc, aconst->v);
    }

    oix += indent(os + oix, depth);
    oix += sprintf(os + oix, "} // end %s\n", aunk->name);
}


void compile_anim(anim *a, const char *fnname, char *os) {
    ll oix = 0;
    const float totaltime = a->duration;

    oix += sprintf(os + oix, "function %s(inp, t01) {\n", fnname);
    // oix += sprintf(os + oix, TAB "let out = {}\n");
    oix += sprintf(os + oix, TAB "inp.t = t01 * %f;\n", a->duration); 
    oix += sprintf(os + oix, TAB "if (inp.t > %f) { inp.t = %f; }\n", 
            a->duration, a->duration);

    std::map<accessor, val> finalvals;
    std::map<accessor, float> lasttime;
    compile_anim_cases(0, totaltime, a, 1, os, oix, finalvals);
    oix += sprintf(os + oix, TAB "return inp;\n");
    oix += sprintf(os + oix, "}\n");
}

int main() {
    anim *cx_begin = new anim_const("begin_cx", "cx", 100);
    anim *rad_begin = new anim_const("begin_rad", "cr", 0);

    anim_sequence *toplevel = new anim_sequence("toplevel");
    toplevel->seq(cx_begin);
    toplevel->seq(rad_begin);
    toplevel->seq(new anim_slowend("grow", "cr", /*duration=*/200, 20));
    toplevel->seq(new anim_slowend("right_cx", "cx", /*duration=*/200, 300));
    toplevel->seq(new anim_wait("wait", 100));
    toplevel->seq(new anim_slowend("left_cx", "cx", /*duration=*/200, 100));

    anim_parallel *disappear = new anim_parallel("disappear");
    disappear->par(new anim_slowend("left_disappear_cx", "cx", /*duration=*/ 20, 80));
    disappear->par(new anim_slowend("left_disappear_cr", "cr", /*duration=*/ 20, 0));
    toplevel->seq(disappear);

    char buf[4096];
    compile_anim(toplevel, "anim_circle", buf);
    fprintf(stdout, "%s\n", buf);
    fflush(stdout);
    return 0;
}
