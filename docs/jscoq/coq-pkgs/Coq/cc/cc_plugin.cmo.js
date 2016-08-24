(function(a){"use strict";var
b8=125,bk=131,aT=".",P=140,I=112,bj="cc",ck="$l",b7="  [",b4=3901498,b5=" : ",cj=-431191102,cf="$n",bi=-912009552,O=250,ce=": ",W="Init",D=124,H=246,aX="congruence",cd="[",aZ=115,an=130,aW="plugins/cc/g_congruence.ml4",aS=111,b3="cc_plugin",b6=151,aU="Extension: cannot occur",b2="A",aa=113,ah=122,cc="X",ci="with",aY="]",cb=139,b1=915186972,bg=116,ca=888453194,V="Logic",b_="Congruence",b$=" and ",aV=109,b9="Exception in tactic extend ",b0=-318868643,ai=126,ch=121,cg="Heq",aA="f_equal",aR=15500,bZ=129,bh=1e3,w=a.jsoo_runtime,s=w.caml_check_bound,ay=w.caml_int_compare,aQ=w.caml_make_vect,b=w.caml_new_string,N=w.caml_obj_tag,az=w.caml_register_global,r=w.caml_wrap_exception;function
g(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
h(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
m(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
G(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
bY(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
fQ(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):w.caml_call_gen(a,[b,c,d,e,f,g])}function
fP(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}var
c=w.caml_get_global_data(),ag=b(b3),j=c.Term,l=c.Names,aj=c.Hashset,bm=c.Sorts,d=c.Util,t=c.Not_found,aB=c.Global,ar=c.Environ,k=c.Tacmach,e=c.Int,u=c.Queue,i=c.Pp,a0=c.Control,x=c.Errors,z=c.Pervasives,bl=c.Vars,bn=c.Goal,ao=c.Assert_failure,Q=c.Termops,aq=c.Feedback,ap=c.Hashtbl,L=c.CamlinternalLazy,av=c.Globnames,F=c.Tactics,q=c.Tacticals,p=c.Proofview,au=c.Typing,ba=c.Refiner,bc=c.Coqlib,bK=c.Universes,bb=c.Evd,Z=c.Closure,ax=c.Loc,be=c.Tacenv,aP=c.Constrarg,aO=c.Stdarg,am=c.Genarg,aN=c.Tacinterp,bf=c.Mltop,a1=[0,0],dQ=b("Out of depth ... "),dP=b("Out of instances ... "),dR=b("First run was incomplete, completing ... "),dO=b("Executing ... "),dN=b("Running E-matching algorithm ... "),dM=b("paf_of_patt: pattern is trivial"),dK=b("wrong incomplete class"),dE=b(" ... "),dF=b(" = "),dG=b("Checking if "),dD=b("Yes"),dH=b("No"),dA=b(aT),dB=b("Processing mark for term "),dx=b("weird error in injection subterms merge"),dy=[0,b("add_pacs")],dv=b(aT),dw=b("Updating term "),ds=b(aT),dt=b(b$),du=b("Merging "),dn=b(aT),dp=b(b$),dq=b("Linking "),dm=[0,b("plugins/cc/ccalgo.ml"),651,2],df=b(aY),dg=b(" <> "),dh=b(b5),di=b(b7),dj=b("Adding new disequality, depth="),da=b(aY),db=b(" == "),dc=b(b5),dd=b(b7),de=b("Adding new equality, depth="),c$=b("discarding redundant (dis)equality"),c7=b(aY),c8=b(cd),c4=b(aY),c5=b(":="),c6=b(cd),c3=b("incomplete matching"),cT=b("not a node"),cU=[0,b("subterms")],cK=b("not a constructor"),cL=[0,b("get_constructor")],cG=b("not a representative"),cH=[0,b("get_representative")],cw=b("signature already entered"),cx=[0,b("enter")],co=[0,b(b_),[0,b("Verbose"),0]],cp=b("Congruence Verbose"),cQ=b("Ccalgo.Discriminable"),cW=b(b2),cY=b(b2),dI=b("_eps_"),dS=b("invalid cc transitivity"),dT=b("not enough args"),dU=[0,b("nth_arg")],dV=[0,1,20],dW=b("equal_proof "),dX=[0,1,20],dY=b("edge_proof "),dZ=[0,1,20],d0=b("constr_proof "),d2=b(","),d1=b("}"),d3=b("{"),d4=[0,1,20],d5=b("path_proof "),d6=[0,1,20],d7=b("congr_proof "),d8=[0,1,20],d9=b("ind_proof "),eC=b("f"),eD=b("I don't know how to handle dependent equality"),e0=[0,0],eX=b("congruence failed."),eY=b(b_),eR=b("("),eS=b(")"),eN=b("Goal solved, generating proof ..."),eM=b("Computation completed."),eL=b("Problem built, solving ..."),eK=b("Reading subgoal ..."),eO=b("Goal is solvable by congruence but some arguments are missing."),eP=b("  replacing metavariables by arbitrary terms."),eQ=b(')",'),eT=b('"congruence with ('),eU=b("  Try "),eV=b("Incomplete"),eW=b("congruence failed"),eI=b(cc),eJ=b(cg),eH=b("H"),eF=b("e"),eG=b(cc),eE=b(cg),eA=b("t"),ee=b("CC"),ef=b(aA),eg=[0,b(W),[0,b(V),0]],eh=b("eq_rect"),ei=[0,b(W),[0,b(V),0]],ej=b("eq_refl"),ek=[0,b(W),[0,b(V),0]],el=b("eq_sym"),em=[0,b(W),[0,b(V),0]],eo=b("eq_trans"),ep=[0,b(W),[0,b(V),0]],eq=b("eq"),er=[0,b(W),[0,b(V),0]],es=b("False"),et=[0,b(W),[0,b(V),0]],eu=b("True"),ev=[0,b(W),[0,b(V),0]],ew=b("I"),ex=[0,b(W),[0,b(V),0]],fc=b(aA),fd=b(aA),fA=b(ck),fO=[0,b(aW),1,0],fB=[0,b(ci)],fC=b(cf),fN=[0,b(aW),1,0],fD=[0,b(aX)],fE=b(ck),fM=[0,b(aW),1,0],fF=[0,b(ci)],fG=[0,b(aX)],fH=b(cf),fL=[0,b(aW),1,0],fI=[0,b(aX)],fJ=[0,[0,b(aX)],0],fK=b(bj),fv=b(aU),ft=b(aU),fr=b(aU),fp=b(aU),e4=b(b3),fy=b(bj),e6=b(ce),e7=b(bj),e9=b(b9),fn=b(aA),ff=b(ce),fg=b(aA),fi=b(b9),cl=c.Typeops,cm=c.Goptions,d$=c.Inductiveops,d_=c.Type_errors,eb=c.Evarsolve,ec=c.Equality,ed=c.Context,ea=c.Evarutil,e2=c.Tacentries,e3=c.Array,ab=5;function
A(a){var
b=a1[1];if(b){var
c=g(a,0);return g(aq[15],c)}return b}function
cn(a){a1[1]=a;return 0}var
cq=[0,1,0,cp,co,function(a){return a1[1]},cn];g(cm[4],cq);var
cr=e[1],cs=[0,function(a,b){return a===b?1:0},cr],aC=g(ap[18],cs);function
ct(a,b){var
c=a[1]===b[1]?1:0,d=c?a[2]===b[2]?1:0:c;return d}var
cu=[0,ct,function(a){var
b=g(e[1],a[2]),c=g(e[1],a[1]);return h(aj[2][1],c,b)}],as=g(ap[18],cu);function
cv(a,b,c){if(h(as[10],c[1],b)){var
d=g(i[1],cw);m(x[3],0,cx,d)}else
m(as[9],c[1],b,a);return m(aC[9],c[2],a,b)}function
cy(a,b){return h(as[7],b[1],a)}function
cz(a,b){try{var
c=h(aC[7],a[2],b);h(as[6],a[1],c);var
d=h(aC[6],a[2],b);return d}catch(f){f=r(f);if(f===t)return 0;throw f}}function
cA(c,b){function
a(a){return cz(c,a)}return h(e[2][13],a,b)}var
cB=[0,function(a,b){var
c=ay(a[1],b[1]);if(0===c){var
e=ay(a[2],b[2]);return 0===e?m(d[17][45],ay,a[3],b[3]):e}return c}],cC=[0,function(a,b){var
c=ay(a[1],b[1]);return 0===c?ay(a[2],b[2]):c}],o=g(d[21][1],cB),n=g(d[21][1],cC);function
bo(a,b){var
c=0===a[0]?0===a[1]?0===b[0]?0===b[1]?1:0:0:0===b[0]?0===b[1]?0:1:0:0===b[0]?0:1;return c?1:0}function
a2(a,b){var
d=a,c=b;for(;;){switch(d[0]){case
0:if(0===c[0])return h(j[cb],d[1],c[1]);break;case
1:if(1===c[0]){var
g=bo(d[1],c[1]);return g?bo(d[2],c[2]):g}break;case
2:if(2===c[0])return h(l[1][1],d[1],c[1]);break;case
3:if(3===c[0]){var
i=a2(d[1],c[1]);if(i){var
d=d[2],c=c[2];continue}return i}break;default:if(4===c[0]){var
e=c[1],f=d[1],k=f[2]===e[2]?1:0;if(k){var
m=f[3]===e[3]?1:0;if(m)return h(l[46],f[1][1],e[1][1]);var
n=m}else
var
n=k;return n}}return 0}}function
a3(a){switch(a[0]){case
0:var
c=g(j[b6],a[1]);return h(aj[2][1],1,c);case
1:var
d=g(bm[6],a[2]),e=g(bm[6],a[1]);return m(aj[2][3],2,e,d);case
2:var
f=g(l[1][3],a[1]);return h(aj[2][1],3,f);case
3:var
i=a3(a[2]),k=a3(a[1]);return m(aj[2][3],4,k,i);default:var
b=a[1],n=b[3],o=b[2],p=g(l[50],b[1][1]);return G(aj[2][4],5,p,o,n)}}var
J=g(ap[18],[0,j[cb],j[b6]]),ac=g(ap[18],[0,a2,a3]),a4=g(ap[18],[0,l[1][1],l[1][3]]),cD=[0,g(j[I],z[8])],bp=[0,[1,z[8],[0,z[8],z[8],0]],z[8],o[1],0,cD];function
bq(a){var
b=g(ac[1],ab);return[0,ab,0,aQ(5,bp),g(J[1],ab),0,b]}function
cE(a,b){var
f=g(J[1],ab),h=g(a4[1],ab),i=e[2][1],j=g(u[2],0),k=g(u[2],0),l=e[2][1],c=g(aC[1],ab),d=[0,g(as[1],ab),c];return[0,bq(0),d,l,k,j,0,0,i,h,a,0,f,b]}function
cF(a){return a[1]}function
y(f,b){var
a=0,c=b;for(;;){var
e=s(f[3],c)[c+1][2];if(0<=e){var
a=[0,c,a],c=e;continue}var
g=function(a){s(f[3],a)[a+1][2]=c;return 0};h(d[17][11],g,a);return c}}function
R(a,b){var
c=s(a[3],b)[b+1][1];if(0===c[0])return c[1];var
d=g(i[1],cG);return m(x[3],0,cH,d)}function
a5(a,b){return s(a[3],b)[b+1][3]}function
cI(a,b,c){var
d=a5(a,b);return h(o[22],c,d)}function
cJ(a,b,c){var
d=b;for(;;)try{var
f=a5(a,d),g=h(o[22],c,f);return g}catch(f){f=r(f);if(f===t){var
e=s(a[3],d)[d+1][1];if(0===e[0])throw t;var
d=e[1];continue}throw f}}function
br(a,b){var
c=s(a[3],b)[b+1][5];if(4===c[0])return c[1];var
d=g(i[1],cK);return m(x[3],0,cL,d)}function
bs(a,b){return R(a,b)[1]}function
cM(a){return a[4]}function
cN(a){return a[5]}function
cO(a,b,c){var
d=R(a,b);d[1]=d[1]+1|0;d[2]=h(e[2][4],c,d[2]);d[3]=h(e[2][4],c,d[3]);return 0}function
cP(a,b,c){var
d=R(a,b);d[1]=d[1]+1|0;d[3]=h(e[2][4],c,d[3]);return 0}var
bt=[248,cQ,w.caml_fresh_oo_id(0)];function
cR(a){var
b=g(d[17][4],a[3]);return[0,a[1],a[2]+1|0,b]}function
cS(a,b,c){try{var
i=h(n[22],b,a[6]),d=i}catch(f){f=r(f);if(f!==t)throw f;var
d=e[2][1]}var
f=a[6],g=h(e[2][4],c,d);a[6]=m(n[4],b,g,f);return 0}function
ak(a,b){return s(a[3],b)[b+1][5]}function
a6(a,b){var
c=s(a[3],b)[b+1][4];if(c){var
d=c[1];return[0,d[1],d[2]]}var
e=g(i[1],cT);return m(x[3],0,cU,e)}function
bu(a,b){var
c=a6(a,b),d=y(a,c[2]);return[0,y(a,c[1]),d]}function
cV(a){var
b=a[2],c=b+1|0;if(c===a[1]){var
e=((a[1]*3|0)/2|0)+1|0,f=aQ(e,bp);a[1]=e;bY(d[19][10],a[3],0,f,0,b);a[3]=f}a[2]=c;return b}function
aD(a){return[0,0,e[2][1],e[2][1],0,a,n[1]]}var
cX=[0,g(l[1][5],cW)],cZ=[0,g(l[1][5],cY)],c0=g(j[I],2),c1=[0,0,g(j[I],2),c0],c2=g(j[ch],c1);function
K(a){switch(a[0]){case
0:return bv(a[1],0);case
1:var
i=a[1],d=[0,cZ,g(j[bg],a[2]),c2],e=g(j[ah],d),f=[0,cX,g(j[bg],i),e];return g(j[ah],f);case
2:return g(j[aa],a[1]);case
3:var
k=a[1],c=[0,K(a[2]),0],b=k;for(;;){if(3===b[0]){var
l=b[1],c=[0,K(b[2]),c],b=l;continue}if(0===b[0])return bv(b[1],c);var
m=K(b);return h(j[60],m,c)}default:return g(j[bk],a[1][1])}}function
bv(a,b){var
e=g(j[P],a);if(10===e[0]){var
c=e[1],f=g(aB[2],0);if(h(ar[63],c[1],f)){var
d=h(l[aV][1],c[1],0);if(b){var
i=b[2],k=g(j[ai],[0,d,b[1]]);return h(j[60],k,i)}var
m=g(aB[2],0),n=h(cl[26],m,c),o=g(aB[2],0),p=h(ar[62],d,o),q=h(j[85],p[2]+1|0,n[1])[1],r=[0,d,g(j[I],1)],s=g(j[ai],r);return h(j[69],s,q)}}return h(j[60],a,b)}function
E(a){var
b=g(j[P],a);switch(b[0]){case
6:var
n=E(b[3]),o=E(b[2]);return g(j[ch],[0,b[1],o,n]);case
7:var
p=E(b[3]),q=E(b[2]);return g(j[ah],[0,b[1],q,p]);case
8:var
r=E(b[4]),s=E(b[3]),t=E(b[2]);return g(j[123],[0,b[1],t,s,r]);case
9:var
u=h(d[19][51],E,b[2]),v=[0,E(b[1]),u];return g(j[D],v);case
10:var
c=b[1],w=g(l[aZ],c[1]),x=g(l[aS],w);return g(j[bZ],[0,x,c[2]]);case
11:var
e=b[1],f=e[1],y=g(l[an],f[1]),z=g(l[ai],y);return g(j[an],[0,[0,z,f[2]],e[2]]);case
12:var
i=b[1],k=i[1],m=k[1],A=g(l[an],m[1]),B=g(l[ai],A);return g(j[bk],[0,[0,[0,B,m[2]],k[2]],i[2]]);case
16:var
C=b[1],F=function(a){var
b=g(l[aZ],a);return g(l[aS],b)},G=h(l[aV][10],F,C),H=[0,G,E(b[2])];return g(j[ai],H);default:return a}}function
a7(c,b){if(0===b[0]){var
e=b[1],f=b[2],g=function(a,b){return[3,b,a7(c,a)]};return m(d[17][16],g,f,e)}var
a=b[1]-1|0;return s(c,a)[a+1]}function
S(a,b){var
c=g(i[1],c4),d=K(ak(a,b)),e=g(Q[5],d),f=g(i[1],c5),j=g(i[20],b),k=g(i[1],c6),l=h(i[14],k,j),m=h(i[14],l,f),n=h(i[14],m,e);return h(i[14],n,c)}function
aE(a){var
b=g(i[1],c7),c=K(a),d=g(Q[5],c),e=g(i[1],c8),f=h(i[14],e,d);return h(i[14],f,b)}function
X(a,b){var
d=a[1];try{var
f=h(ac[7],d[6],b);return f}catch(f){f=r(f);if(f===t){var
c=cV(d),p=K(b),g=E(h(k[15],a[13],p));switch(b[0]){case
2:var
x=o[1],i=[0,[0,aD(g)],-1,x,0,b];break;case
3:var
l=X(a,b[1]),n=X(a,b[2]);cO(d,y(d,l),c);cP(d,y(d,n),c);a[3]=h(e[2][4],c,a[3]);var
z=o[1],i=[0,[0,aD(g)],-1,z,[0,[0,l,n]],b];break;case
4:h(u[3],[0,c,[0,[0,c,0]]],a[5]);h(u[3],[0,c,[1,[0,c,b[1][2],0]]],a[5]);var
A=o[1],i=[0,[0,aD(g)],-1,A,0,b];break;default:h(u[3],[0,c,[0,[0,c,0]]],a[5]);var
q=o[1],i=[0,[0,aD(g)],-1,q,0,b]}s(d[3],c)[c+1]=i;m(ac[5],d[6],b,c);try{var
w=h(J[7],a[12],g),j=w}catch(f){f=r(f);if(f!==t)throw f;var
j=e[2][1]}var
v=h(e[2][4],c,j);m(J[9],a[12],g,v);return c}throw f}}function
bw(a,b,c,d){var
e=X(a,c),f=X(a,d);h(u[3],[0,e,f,[0,b,0]],a[4]);return m(J[5],a[1][4],b,[0,c,d])}function
bx(a,b,c,d){var
e=X(a,c),f=X(a,d);a[6]=[0,[0,e,f,b],a[6]];return 0}function
c9(a,b,c,d){a[7]=[0,[0,b,c,d[1],d[3],d[2],d[5],d[4]],a[7]];return 0}function
c_(e,b,c){try{var
f=e[1],a=function(a){return y(f,a)},g=h(d[19][15],a,c),i=h(a4[8],e[9],b),j=function(a){function
b(a,b){return a===y(e[1],b)?1:0}return m(d[19][31],b,g,a)},k=h(d[17][23],j,i);return k}catch(f){f=r(f);if(f===t)return 0;throw f}}function
dk(a,b,c,d){var
e=s(a[3],b)[b+1];e[1]=[1,c,d];e[2]=c;return 0}function
by(a,b,c){var
d=b,e=c;for(;;){var
f=s(a[3],d)[d+1][1];if(0===f[0])return e;var
g=f[1],h=[0,[0,[0,d,g],f[2]],e],d=g,e=h;continue}}function
dl(a,b,c){var
k=y(a,c);if(y(a,b)===k){var
l=by(a,c,0),d=[0,by(a,b,0),l];for(;;){var
e=d[1];if(e){var
f=d[2];if(f){var
h=f[1][1],i=e[1][1],g=i[1]===h[1]?1:0,j=g?i[2]===h[2]?1:0:g;if(j){var
d=[0,e[2],f[2]];continue}return d}return[0,e,0]}return[0,0,d[2]]}}throw[0,ao,dm]}function
bz(f,b,c,d){A(function(a){var
d=g(i[1],dn),e=S(f[1],c),j=g(i[1],dp),k=S(f[1],b),l=g(i[1],dq),m=h(i[14],l,k),n=h(i[14],m,j),o=h(i[14],n,e);return h(i[14],o,d)});var
k=R(f[1],b),a=R(f[1],c);dk(f[1],b,c,d);try{var
D=h(J[7],f[12],k[5]),q=D}catch(f){f=r(f);if(f!==t)throw f;var
q=e[2][1]}var
x=h(e[2][6],b,q);m(J[9],f[12],k[5],x);var
v=h(e[2][7],k[3],a[3]);a[1]=g(e[2][19],v);a[3]=v;a[2]=h(e[2][7],k[2],a[2]);cA(f[2],k[3]);f[3]=h(e[2][7],f[3],k[3]);var
y=s(f[1][3],b)[b+1][3];function
z(a,b){return h(u[3],[0,b,[1,a]],f[5])}h(o[10],z,y);var
B=k[6];function
C(b){function
a(a){return h(u[3],[0,a,[0,b]],f[5])}return g(e[2][13],a)}h(n[10],C,B);var
l=k[4],j=a[4];if(typeof
l==="number"){if(0===l)return 0;if(typeof
j==="number"){if(0===j){a[4]=1;return 0}}else
if(0===j[0]){f[8]=h(e[2][6],c,f[8]);a[4]=1;return 0}}else
if(0===l[0]){if(typeof
j==="number"){if(0===j){a[4]=[0,l[1]];f[8]=h(e[2][6],b,f[8]);f[8]=h(e[2][4],c,f[8]);return 0}var
w=0}else
var
w=0===j[0]?0:1;if(!w){f[8]=h(e[2][6],b,f[8]);return 0}}else{var
p=l[1];if(typeof
j==="number"){if(0===j){a[4]=[1,p];return 0}}else
if(0!==j[0])return h(u[3],[0,p[1],[1,p[2]]],f[5])}return 0}function
dr(c,b){A(function(a){var
d=g(i[1],ds),e=S(b[1],c[2]),f=g(i[1],dt),j=S(b[1],c[1]),k=g(i[1],du),l=h(i[14],k,j),m=h(i[14],l,f),n=h(i[14],m,e);return h(i[14],n,d)});var
d=b[1],e=y(d,c[1]),f=y(d,c[2]),j=1-(e===f?1:0);if(j){var
l=bs(d,f);if(bs(d,e)<l)return bz(b,e,f,c);var
a=c[3],k=typeof
a==="number"?0:0===a[0]?[0,a[1],1-a[2]]:[1,a[3],a[4],a[1],a[2],a[5]];return bz(b,f,e,[0,c[2],c[1],k])}return j}function
dz(d,b,c){A(function(a){var
b=g(i[1],dA),e=S(c[1],d),f=g(i[1],dB),j=h(i[14],f,e);return h(i[14],j,b)});var
q=y(c[1],d),f=R(c[1],q);if(0===b[0]){cS(f,b[1],d);c[3]=h(e[2][7],f[2],c[3]);return 0}var
a=b[1],r=s(c[1][3],q)[q+1];if(1-h(o[3],a,r[3]))r[3]=m(o[4],a,d,r[3]);var
j=f[4];if(typeof
j==="number"){if(0===j)return 0===a[2]?(f[4]=[1,[0,d,a]],0):(c[3]=h(e[2][7],f[2],c[3]),f[4]=[0,a],c[8]=h(e[2][4],q,c[8]),0)}else
if(1===j[0]){var
t=j[1],k=t[2],v=t[1];if(a[1]===k[1]){var
z=br(c[1],a[1]),p=z[3],n=k[3],l=a[3];for(;;){var
w=0<p?1:0;if(w){if(n)if(l){h(u[3],[0,n[1],l[1],[1,v,k,d,a,p]],c[4]);var
p=p-1|0,n=n[2],l=l[2];continue}var
B=g(i[1],dx);return m(x[3],0,dy,B)}return w}}throw[0,bt,v,k,d,a]}c[3]=h(e[2][7],f[2],c[3]);return 0}function
dC(c){var
d=c[1];function
e(a){if(a){var
b=a[1],l=y(d,b[2]);if(y(d,b[1])===l)var
j=g(i[1],dD),f=[0,b];else
var
m=e(a[2]),j=g(i[1],dH),f=m;A(function(a){var
d=g(i[1],dE),e=S(c[1],b[2]),f=g(i[1],dF),k=S(c[1],b[1]),l=g(i[1],dG),m=h(i[14],l,k),n=h(i[14],m,f),o=h(i[14],n,e),p=h(i[14],o,d);return h(i[14],p,j)});var
k=f}else
var
k=a;return k}return e(c[6])}var
dJ=g(l[1][5],dI);function
dL(b){var
a=b[8];function
c(a){var
f=R(b[1],a)[4];if(typeof
f!=="number"&&0===f[0]){var
c=f[1],v=K(ak(b[1],c[1])),w=h(k[15],b[13],v),y=c[3],z=function(a){return K(ak(b[1],a))},A=h(d[17][12],z,y),B=g(d[17][6],A),C=h(j[76],w,B),D=c[2],n=ak(b[1],a),p=C,l=D;for(;;){if(0<l){var
q=g(j[34],p),s=q[2],e=h(k[20],dJ,b[13]),o=b[13];b[13]=m(bn[4][12],o[2],o[1],[0,[0,e,s],0]);var
t=q[3],u=[0,g(j[aa],e),0],n=[3,n,[2,e]],p=h(bl[12],u,t),l=l-1|0;continue}b[1][5]=[0,c,b[1][5]];X(b,n);return 0}}var
r=g(i[1],dK);return m(x[3],0,0,r)}return h(e[2][13],c,a)}function
bA(a){var
c=[0,n[1]],g=a[1],b=a[1][3];function
f(d,b){var
a=d<g[2]?1:0;if(a){var
f=b[1];if(0===f[0]){var
i=f[1][6],j=function(a,b){try{var
j=h(n[22],a,c[1]),g=j}catch(f){f=r(f);if(f!==t)throw f;var
g=e[2][1]}var
f=c[1],i=h(e[2][4],d,g);c[1]=m(n[4],a,i,f);return 0};return h(n[10],j,i)}return 0}return a}h(d[19][14],f,b);return c[1]}function
bB(q,b,c){var
f=g(d[22][9],c),m=f[3];if(m){var
i=m[2],u=m[1],a=u[2],j=u[1],k=q[1];if(0===j[0]){var
l=j[2],o=j[1];if(l){var
B=l[2],C=l[1];try{var
D=h(ac[7],k[6],o),E=[0,D,g(d[17][1],l)],F=R(k,a)[6],G=h(n[22],E,F),H=function(a){var
b=bu(q[1],a),e=[0,[0,[0,o,B],b[1]],[0,[0,C,b[2]],i]],j=f[2],k=[0,g(d[19][8],f[1]),j,e];return h(d[22][3],k,c)},I=h(e[2][13],H,G);return I}catch(f){f=r(f);if(f===t)return 0;throw f}}try{var
v=y(k,h(ac[7],k[6],o))===a?1:0,J=v?h(d[22][3],[0,f[1],f[2],i],c):v;return J}catch(f){f=r(f);if(f===t)return 0;throw f}}var
p=j[1],w=p-1|0;if(0<=s(f[1],w)[w+1]){var
x=p-1|0,z=s(f[1],x)[x+1]===a?1:0;return z?h(d[22][3],[0,f[1],f[2],i],c):z}var
A=p-1|0;s(f[1],A)[A+1]=a;return h(d[22][3],[0,f[1],f[2],i],c)}b[1]=[0,[0,f[2],f[1]],b[1]];return 0}function
a8(a,b){if(0===b[0]){var
c=g(d[17][1],b[2]);return[0,h(ac[7],a,b[1]),c]}return g(z[1],dM)}function
bC(a){var
k=a[1][6],f=g(d[22][2],0),l=bA(a);function
b(b){var
i=b[5];if(typeof
i==="number")if(0===i)try{var
u=a8(k,b[4]),v=h(n[22],u,l),c=v}catch(f){f=r(f);if(f!==t)throw f;var
c=e[2][1]}else
var
c=e[2][1];else
try{var
w=h(J[7],a[12],i[1]),c=w}catch(f){f=r(f);if(f!==t)throw f;var
c=e[2][1]}function
m(a){return h(d[22][3],[0,aQ(b[3],-1),b,[0,[0,b[4],a],0]],f)}h(e[2][13],m,c);var
j=b[7];if(typeof
j==="number")if(0===j)try{var
p=a8(k,b[6]),q=h(n[22],p,l),g=q}catch(f){f=r(f);if(f!==t)throw f;var
g=e[2][1]}else
var
g=e[2][1];else
try{var
s=h(J[7],a[12],j[1]),g=s}catch(f){f=r(f);if(f!==t)throw f;var
g=e[2][1]}function
o(a){return h(d[22][3],[0,aQ(b[3],-1),b,[0,[0,b[6],a],0]],f)}return h(e[2][13],o,g)}h(d[17][11],b,a[7]);return f}function
bD(a){var
b=[0,0],c=bC(a);A(function(a){return g(i[1],dN)});try{for(;;){g(a0[2],0);bB(a,b,c);continue}}catch(f){f=r(f);if(f===d[22][1])return b[1];throw f}}function
a9(a,c){A(function(a){return g(i[1],dO)});try{for(;;){g(a0[2],0);try{dr(g(u[5],c[4]),c);var
I=1,k=I}catch(f){f=r(f);if(f!==u[1])throw f;try{var
v=g(u[5],c[5]);dz(v[1],v[2],c);var
H=1,k=H}catch(f){f=r(f);if(f!==u[1])throw f;try{var
b=g(e[2][23],c[3]);c[3]=h(e[2][6],b,c[3]);A(function(j){return function(a){var
b=g(i[1],dv),d=S(c[1],j),e=g(i[1],dw),f=h(i[14],e,d);return h(i[14],f,b)}}(b));var
l=bu(c[1],b),p=l[1],y=a6(c[1],b)[2],q=R(c[1],p),s=q[4],P=typeof
s==="number"?0:0===s[0]?(q[4]=1,c[8]=h(e[2][6],p,c[8]),1):0,z=a5(c[1],p),B=function(d,e){return function(a,b){return h(u[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,d,a[3]]]]],c[5])}}(y,b);h(o[10],B,z);var
C=q[6],E=function(d){return function(a,b){return h(u[3],[0,d,[0,[0,a[1],a[2]+1|0]]],c[5])}}(b);h(n[10],E,C);try{var
F=cy(l,c[2]);h(u[3],[0,b,F,0],c[4])}catch(f){f=r(f);if(f!==t)throw f;cv(b,l,c[2])}var
G=1,k=G}catch(f){f=r(f);if(f!==t)throw f;var
k=0}}}if(k)continue;var
w=dC(c);if(w)var
L=a?[1,w[1]]:a,f=[0,L];else
if(g(e[2][2],c[8]))if(0<c[10]){var
M=bD(c),N=function(a){var
n=a[2],b=a[1];g(a0[2],0);var
o=0<c[10]?1:0;if(o){if(c_(c,b[1],n))return A(function(a){return g(i[1],c$)});m(a4[5],c[9],b[1],n);var
s=c[1],q=function(a){try{var
c=ak(s,a);return c}catch(f){f=r(f);if(g(x[22],f)){var
b=g(i[1],c3);return m(x[3],0,0,b)}throw f}},l=h(d[19][15],q,n),t=g(j[aa],b[1]),p=h(d[19][15],K,l);g(d[19][40],p);var
e=g(j[D],[0,t,p]),f=a7(l,b[4]),k=a7(l,b[6]);c[11]=1;c[10]=c[10]-1|0;return b[2]?(A(function(a){var
b=g(i[1],da),d=aE(k),j=g(i[1],db),l=aE(f),m=g(i[1],dc),n=g(Q[5],e),o=g(i[1],dd),p=h(i[14],o,n),q=h(i[14],p,m),r=h(i[14],q,l),s=h(i[14],r,j),t=h(i[14],s,d),u=h(i[14],t,b),v=g(i[6],0),w=g(i[20],c[10]),x=g(i[1],de),y=h(i[14],x,w),z=h(i[14],y,v);return h(i[14],z,u)}),bw(c,e,f,k)):(A(function(a){var
b=g(i[1],df),d=aE(k),j=g(i[1],dg),l=aE(f),m=g(i[1],dh),n=g(Q[5],e),o=g(i[1],di),p=h(i[14],o,n),q=h(i[14],p,m),r=h(i[14],q,l),s=h(i[14],r,j),t=h(i[14],s,d),u=h(i[14],t,b),v=g(i[6],0),w=g(i[20],c[10]),x=g(i[1],dj),y=h(i[14],x,w),z=h(i[14],y,v);return h(i[14],z,u)}),bx(c,[0,e],f,k))}return o};h(d[17][11],N,M);var
O=c[11]?(c[11]=0,a9(1,c)):(A(function(a){return g(i[1],dP)}),0),f=O}else{A(function(a){return g(i[1],dQ)});var
f=0}else{A(function(a){return g(i[1],dR)});dL(c);var
f=a9(0,c)}return f}}catch(f){f=r(f);if(f[1]===bt){var
J=a?[0,[0,f[2],f[3],f[4],f[5]]]:a;return[0,J]}throw f}}var
f=[0,[0,n[1],n[2],n[3],n[4],n[5],n[6],n[7],n[8],n[9],n[10],n[11],n[12],n[13],n[14],n[15],n[16],n[17],n[18],n[19],n[20],n[21],n[22],n[23],n[24]],[0,o[1],o[2],o[3],o[4],o[5],o[6],o[7],o[8],o[9],o[10],o[11],o[12],o[13],o[14],o[15],o[16],o[17],o[18],o[19],o[20],o[21],o[22],o[23],o[24]],J,ac,a2,K,A,cF,cM,cN,cE,X,bw,bx,c9,cR,y,cI,cJ,ak,br,a6,dl,bA,bB,bC,a8,bD,a9,S,bq];az(173,f,"Cc_plugin.Ccalgo");function
at(a){return[0,a,a,[2,a]]}function
al(a,b){var
c=a[3],d=b[3];if(2===c[0])if(2===d[0])return at([3,c[1],d[1]]);return[0,[3,a[1],b[1]],[3,a[2],b[2]],[4,a,b]]}function
B(a,b){var
d=a,c=b;for(;;){var
e=d[3],j=c[3];switch(e[0]){case
2:return c;case
4:var
n=e[2],o=e[1];switch(j[0]){case
2:var
k=0;break;case
3:var
l=j[1][3];if(4===l[0]){var
r=j[2],s=B(n,l[2]),d=al(B(o,l[1]),s),c=r;continue}var
k=1;break;case
4:var
t=B(n,j[2]);return al(B(o,j[1]),t);default:var
k=1}break;default:var
k=0}if(!k){if(2===j[0])return d;if(3===e[0]){var
q=B(e[2],c),d=e[1],c=q;continue}}if(h(f[5],d[2],c[1]))return[0,d[1],c[2],[3,d,c]];var
p=g(i[1],dS);return m(x[3],0,0,p)}}function
T(a){var
b=a[3];switch(b[0]){case
0:return[0,a[2],a[1],[1,b[1]]];case
1:return[0,a[2],a[1],[0,b[1]]];case
2:return a;case
3:var
c=T(b[1]);return B(T(b[2]),c);case
4:var
d=T(b[2]);return al(T(b[1]),d);default:var
e=b[4],f=b[3],g=b[2],h=[5,T(b[1]),g,f,e];return[0,a[2],a[1],h]}}function
bE(a,b){var
c=h(f[3][7],a,b);return[0,c[1],c[2],[0,b]]}function
bF(a,b){var
c=h(f[3][7],a,b);return[0,c[2],c[1],[1,b]]}function
bG(a,b){var
c=a,d=b;for(;;){if(3===c[0]){if(0<d){var
c=c[1],d=d-1|0;continue}return c[2]}var
e=g(i[1],dT);return m(x[3],0,dU,e)}}function
bH(a,b,c,d){var
e=bG(a[2],c-d|0);return[0,bG(a[1],c-d|0),e,[5,a,b,c,d]]}function
Y(d,b,c){function
e(a){var
e=h(f[30],d,c),j=g(i[3],dV),k=h(f[30],d,b),l=g(i[1],dW),m=h(i[14],l,k),n=h(i[14],m,j);return h(i[14],n,e)}g(f[7],e);if(b===c)return at(h(f[20],d,b));var
a=m(f[23],d,b,c),j=T(aF(d,c,a[2]));return B(aF(d,b,a[1]),j)}function
bI(c,b){var
d=b[2],j=b[1],k=j[2],l=j[1];function
p(a){var
b=h(f[30],c,k),d=g(i[3],dX),e=h(f[30],c,l),j=g(i[1],dY),m=h(i[14],j,e),n=h(i[14],m,d);return h(i[14],n,b)}g(f[7],p);var
q=Y(c,l,d[1]),r=T(Y(c,k,d[2])),a=d[3];if(typeof
a==="number")var
e=bJ(c,d[1],d[2]);else
if(0===a[0])var
m=a[1],s=a[2]?bF(g(f[9],c),m):bE(g(f[9],c),m),e=s;else
var
n=a[2],t=a$(c,a[1],n,a[3],a[4]),o=h(f[21],c,n[1]),e=bH(t,o[1],o[3],a[5]);return B(B(q,e),r)}function
a_(d,b,c){function
k(a){var
c=g(i[3],dZ),e=h(f[30],d,b),j=g(i[1],d0),k=h(i[14],j,e);return h(i[14],k,c)}g(f[7],k);var
a=m(f[19],d,b,c),e=Y(d,b,a);if(0===c[3])return e;var
l=g(f[16],c),j=h(f[22],d,a),n=h(f[20],d,j[2]),o=a_(d,j[1],l);return B(e,al(o,at(n)))}function
aF(d,b,c){function
e(a){var
e=g(i[1],d1);function
j(a){return g(i[20],a[1][2])}function
k(a){return g(i[1],d2)}var
l=m(i[54],k,j,c),n=g(i[1],d3),o=g(i[3],d4),p=h(f[30],d,b),q=g(i[1],d5),r=h(i[14],q,p),s=h(i[14],r,o),t=h(i[14],s,n),u=h(i[14],t,l);return h(i[14],u,e)}g(f[7],e);if(c){var
a=c[1],j=bI(d,a);return B(aF(d,a[1][2],c[2]),j)}return at(h(f[20],d,b))}function
bJ(d,b,c){function
j(a){var
e=h(f[30],d,c),j=g(i[3],d6),k=h(f[30],d,b),l=g(i[1],d7),m=h(i[14],l,k),n=h(i[14],m,j);return h(i[14],n,e)}g(f[7],j);var
a=h(f[22],d,b),e=h(f[22],d,c),k=Y(d,a[2],e[2]);return al(Y(d,a[1],e[1]),k)}function
a$(j,b,c,d,e){function
a(a){var
c=h(f[30],j,d),e=g(i[3],d8),k=h(f[30],j,b),l=g(i[1],d9),m=h(i[14],l,k),n=h(i[14],m,e);return h(i[14],n,c)}g(f[7],a);var
k=Y(j,b,d),l=a_(j,b,c),m=B(k,a_(j,d,e));return B(T(l),m)}var
aG=[0,at,al,B,T,bE,bF,bH,Y,bI,aF,bJ,a$,function(a,b){if(b0<=b[1]){var
c=b[2];return a$(a,c[1],c[2],c[3],c[4])}var
d=b[2];return Y(a,d[1],d[2])}];az(174,aG,"Cc_plugin.Ccproof");function
U(c,b){return[H,function(a){return m(bc[5],ee,c,b)}]}var
aH=U(eg,ef),bL=U(ei,eh),bM=U(ek,ej),en=U(em,el),bN=U(ep,eo),v=U(er,eq),_=U(et,es),aI=U(ev,eu),aJ=U(ex,ew);function
bO(a){var
b=m(Z[37],0,Z[12],a);return function(a){var
c=g(Z[31],a);return h(Z[42],b,c)}}function
aK(a){var
b=m(Z[37],0,Z[11],a);return function(a){var
c=g(Z[31],a);return h(Z[42],b,c)}}function
aL(a,b,c){return m(au[4],a,[0,b],c)}function
C(e,b,c){var
x=g(bO(e),c),a=g(j[P],x);switch(a[0]){case
6:var
i=a[3],k=a[2],y=g(j[I],1);if(!h(Q[44],y,i)){var
n=g(Q[54],i),z=aL(e,b,n),A=aL(e,b,k),B=C(e,b,n);return[3,[3,[1,A,z],C(e,b,k)],B]}break;case
9:var
D=C(e,b,a[1]),E=a[2],F=function(a){return C(e,b,a)},G=h(d[19][15],F,E),H=function(a,b){return[3,a,b]};return m(d[19][17],H,D,G);case
10:var
o=a[1],J=g(l[aZ],o[1]),K=g(l[aS],J);return[0,g(j[bZ],[0,K,o[2]])];case
11:var
p=a[1],q=p[1],L=g(l[an],q[1]),M=g(l[ai],L);return[0,g(j[an],[0,[0,M,q[2]],p[2]])];case
12:var
r=a[1],s=r[1],u=s[2],v=s[1],N=g(l[an],v[1]),O=g(l[ai],N),f=[0,O,v[2]],R=g(aB[26],f),w=h(d$[44],e,[0,f,u]);return[4,[0,[0,[0,f,u],r[2]],w,w-R[1][6]|0]];case
16:var
S=function(a){var
b=g(l[aZ],a);return g(l[aS],b)},T=h(l[aV][10],S,a[1]),U=C(e,b,a[2]),V=g(l[aV][3],T);return[3,[0,g(j[b8],V)],U]}if(g(bl[2],c))return[0,c];throw t}function
bd(a,b,c){var
i=g(aK(a),c),e=g(j[P],i);if(9===e[0]){var
d=e[2],f=N(v),k=e[1],l=O===f?v[1]:H===f?g(L[2],v):v;if(h(av[11],l,k))if(3===d.length-1){var
m=C(a,b,s(d,2)[3]),n=C(a,b,s(d,1)[2]);return[0,aR,[0,s(d,0)[1],n,m]]}return[0,bi,C(a,b,c)]}return[0,bi,C(a,b,c)]}function
aw(f,b,c){var
r=g(bO(f),c),a=g(j[P],r);switch(a[0]){case
0:var
i=a[1];return[0,[1,i],g(e[2][5],i)];case
6:var
k=a[3],l=a[2],t=g(j[I],1);if(!h(Q[44],t,k)){var
n=g(Q[54],k),o=aw(f,b,l),p=aw(f,b,n),u=aL(f,b,n),v=aL(f,b,l),w=h(e[2][7],o[2],p[2]);return[0,[0,[1,v,u],[0,o[1],[0,p[1],0]]],w]}break;case
9:var
x=C(f,b,a[1]),y=a[2],z=function(a){return aw(f,b,a)},A=h(d[19][48],z,y),q=g(d[17][38],A),B=m(d[17][15],e[2][7],e[2][1],q[2]);return[0,[0,x,g(d[17][6],q[1])],B]}var
s=C(f,b,c);return[0,[0,s,0],e[2][1]]}function
bP(a){return 0===a[0]?1:0}function
bQ(a,b,c,d){try{var
u=g(aK(a),d),i=g(j[37],u)}catch(f){f=r(f);if(f===j[28])throw t;throw f}var
f=i[2],k=N(v),w=i[1],x=O===k?v[1]:H===k?g(L[2],v):v;if(h(av[11],x,w))if(3===f.length-1){var
l=aw(a,b,s(f,1)[2]),m=l[1],n=aw(a,b,s(f,2)[3]),o=n[1],p=g(e[2][19],l[2])===c?bP(m)?0:[0,s(f,0)[1]]:1,q=g(e[2][19],n[2])===c?bP(o)?0:[0,s(f,0)[1]]:1;if(1===p)if(1===q)throw t;return[0,c,p,m,q,o]}throw t}function
ey(a,b,c,d){var
e=a,f=c,k=d;for(;;){var
o=g(aK(e),k),i=g(j[P],o);if(6===i[0]){var
l=i[3],m=i[2],n=N(_),p=O===n?_[1]:H===n?g(L[2],_):_;if(h(av[11],p,l))return[0,ca,bQ(e,b,f,m)];var
e=h(ar[20],[0,i[1],m],e),f=f+1|0,k=l;continue}return[0,b1,bQ(e,b,f,k)]}}function
ez(a,b,c){var
m=g(aK(a),c),d=g(j[P],m);if(6===d[0]){var
i=d[3],k=d[2],l=N(_),n=O===l?_[1]:H===l?g(L[2],_):_;if(h(av[11],n,i)){var
e=bd(a,b,k);if(aR<=e[1]){var
f=e[2];return[0,b4,[0,f[1],f[2],f[3]]]}return[0,cj,e[2]]}try{var
o=ey(h(ar[20],[0,d[1],k],a),b,1,i);return o}catch(f){f=r(f);if(f===t)return[0,bi,C(a,b,c)];throw f}}return bd(a,b,c)}function
bR(a,b,c,d,e){var
f=b[1][2],i=g(j[I],1),m=g(k[2],e),n=g(k[8],e),o=fP(ec[38],n,m,f,i,a,c,d),p=g(l[1][5],eA),q=[0,[0,h(k[20],p,e)],a,o];return g(j[ah],q)}var
$=j[114];function
ad(a,b,c){var
d=N(a);function
e(a){return g(c,g(j[D],[0,a,b]))}var
f=O===d?a[1]:H===d?g(L[2],a):a;return h(q[67],f,e)}function
ae(a,b,c){var
d=N(a);function
e(a){return g(c,g(j[D],[0,a,b]))}var
f=O===d?a[1]:H===d?g(L[2],a):a;return h(q[70][57],f,e)}function
eB(a){var
b=g(k[45],a);return g(p[66][1],b)}function
aM(c,b){var
a=[0,function(a){function
d(a){return h(au[2],0,a)}var
e=m(k[49][1],d,a,b),f=h(F[138],c,b),i=g(ba[11],e[1]),j=g(p[66][1],i);return h(q[70][3],j,f)}];return g(p[62][10],a)}function
M(a){var
b=[0,function(o){function
c(a){return h(k[49][7],o,a)}try{var
b=a[3];switch(b[0]){case
0:var
d=g(F[45],b[1]);break;case
1:var
x=g(f[6],a[1]),J=g(f[6],a[2]),K=c(x),d=ae(en,[0,K,J,x,b[1]],F[45]);break;case
2:var
y=b[1],L=c(g(f[6],y)),N=F[45],d=ae(bM,[0,L,g(f[6],y)],N);break;case
3:var
z=b[2],s=b[1],O=g(f[6],s[1]),A=g(f[6],s[2]),P=g(f[6],z[2]),Q=c(A),R=g($,2),S=[0,Q,O,A,P,g($,1),R],T=function(a){return ae(bN,S,a)},U=[0,M(z),0],V=[0,M(s),U],W=T(eB),d=h(q[70][19],W,V);break;case
4:var
t=b[2],u=b[1],m=g(f[6],u[1]),e=g(f[6],t[1]),n=g(f[6],u[2]),B=g(f[6],t[2]),C=c(m),X=c(e),v=c(g(j[D],[0,m,[0,e]])),Y=function(a){var
b=g(l[1][5],eC);return h(k[20],b,a)},Z=h(k[49][3],Y,o),_=[0,g(j[I],1),[0,e]],aa=[0,[0,Z],C,g(j[D],_)],ab=g(j[ah],aa),ac=[0,C,v,ab,m,n,g($,1)],af=function(a){return ad(aH,ac,a)},ag=[0,X,v,n,e,B,g($,1)],ai=function(a){return ad(aH,ag,a)},aj=g($,3),ak=g($,2),al=g(j[D],[0,n,[0,B]]),am=g(j[D],[0,n,[0,e]]),an=[0,v,g(j[D],[0,m,[0,e]]),am,al,ak,aj],ao=function(a){return ad(bN,an,a)},ap=g(i[1],eD),aq=[0,h(q[70][5],0,ap),0],ar=[0,F[D],aq],as=M(t),at=ai(k[45]),au=g(p[66][1],at),av=[0,h(q[70][3],au,as),ar],aw=[0,g(q[70][23],av),0],ax=M(u),ay=af(k[45]),az=g(p[66][1],ay),aA=[0,h(q[70][3],az,ax),aw],aB=ao(k[45]),aC=g(p[66][1],aB),d=h(q[70][19],aC,aA);break;default:var
w=b[1],E=g(f[6],w[1]),aD=g(f[6],w[2]),G=g(f[6],a[1]),H=c(E),aE=c(G),aF=g(j[I],(1+b[3]|0)-b[4]|0),aG=b[2],aI=function(a){return bR(H,aG,aF,G,a)},aJ=h(k[49][3],aI,o),aK=[0,H,aE,aJ,E,aD,g($,1)],aL=function(a){return ad(aH,aK,a)},aM=M(w),aN=aL(k[45]),aO=g(p[66][1],aN),d=h(q[70][3],aO,aM)}return d}catch(f){f=r(f);if(g(p[66][10],f))return h(p[18],0,f);throw f}}];return g(p[62][9],b)}function
bS(a,b){function
c(a){return h(au[2],0,a)}var
d=m(k[23],c,b,a),e=g(F[45],a),f=g(p[66][8],e),i=g(ba[11],d[1]);return m(q[5],i,f,b)}function
bT(o,b,c,d){var
a=[0,function(a){var
m=g(f[6],b),e=g(f[6],c);function
r(a){return h(k[15],a,e)}var
i=h(k[49][3],r,a),t=g(l[1][5],eF),u=g(k[20],t),n=h(k[49][3],u,a),w=g(l[1][5],eG),x=g(k[20],w),y=h(k[49][3],x,a),z=[0,[0,y],i,g(j[I],1)],A=g(j[ah],z),E=ad(bL,[0,i,m,A,o,e,g(j[aa],n)],bS),B=[0,g(p[66][1],E),0],s=[0,i,m,e],C=[0,M(d),B],D=[0,n],F=ae(v,s,function(a){return aM(D,a)});return h(q[70][19],F,C)}];return g(p[62][9],a)}function
bU(K,b){var
a=[0,function(a){g(bc[11],bc[14]);function
P(a){return g(i[1],eK)}g(f[7],P);function
R(a){var
n=g(k[8],a),l=g(bb[69],a),c=h(f[11],K,a),m=[0,0],o=[0,0];function
p(a){var
b=C(n,l,a);h(f[12],c,b);return 0}h(d[17][11],p,b);var
q=h(bn[4][3],a[2],a[1]),r=g(ar[26],q);function
s(a){var
p=g(ed[2][1][17],a),k=p[1],e=g(j[aa],k),b=ez(n,l,p[3]),i=b[1];if(aR<=i){if(ca<=i)return b1<=i?G(f[15],c,k,1,b[2]):G(f[15],c,k,0,b[2]);if(b4<=i){var
q=b[2];return G(f[14],c,[0,e],q[2],q[3])}var
r=b[2];return G(f[13],c,e,r[2],r[3])}if(cj<=i){var
s=b[2],u=m[1],v=function(a){return G(f[14],c,[2,a[1],e],a[2],s)};h(d[17][11],v,u);o[1]=[0,[0,e,s],o[1]];return 0}var
t=b[2],w=o[1];function
x(a){return G(f[14],c,[2,e,a[1]],t,a[2])}h(d[17][11],x,w);m[1]=[0,[0,e,t],m[1]];return 0}h(d[17][11],s,r);var
t=g(k[7],a),e=bd(n,l,h(ea[31],l,t));if(aR<=e[1]){var
i=e[2];G(f[14],c,0,i[2],i[3])}else{var
u=e[2],v=m[1],w=function(a){return G(f[14],c,[1,a[1]],a[2],u)};h(d[17][11],w,v)}return c}var
x=h(k[49][3],R,a);function
S(a){return g(i[1],eL)}g(f[7],S);var
y=h(f[29],1,x);function
T(a){return g(i[1],eM)}g(f[7],T);var
c=g(f[8],x);if(y){var
o=y[1],U=function(a){return g(i[1],eN)};g(f[7],U);if(typeof
o==="number"){var
z=[0,0],V=g(p[62][5],a),W=function(a){z[1]++;return g($,z[1])},X=g(f[10],c),Y=function(a){var
b=h(f[21],c,a[1]),e=a[3];function
i(a){var
b=h(f[20],c,a);return g(f[6],b)}var
k=h(d[17][12],i,e),l=h(d[17][48],a[2],W),m=g(d[17][6],l),n=h(d[17][8],k,m),o=g(j[bk],b[1]);return h(j[60],o,n)},Z=h(d[17][12],Y,X),_=g(i[1],eO);g(aq[11],_);var
ab=g(i[1],eP),ac=g(i[1],eQ),af=g(Q[6],V),ag=function(a){var
b=g(i[1],eR),c=g(i[17],0),d=g(i[1],eS),e=h(i[14],d,c);return h(i[14],e,b)},ai=m(i[54],ag,af,Z),aj=g(i[1],eT),ak=h(i[14],aj,ai),al=h(i[14],ak,ac),am=h(i[30],8,al),an=g(i[1],eU),ao=h(i[14],an,am),ap=h(i[14],ao,ab);g(aq[11],ap);var
as=g(i[1],eV);return h(q[70][4],0,as)}else{if(0===o[0]){var
r=o[1],A=r[2],u=h(aG[13],c,[0,b0,[0,r[1],A,r[3],r[4]]]),at=h(f[21],c,A[1])[1],J=[0,function(a){var
b=g(f[6],u[1]),e=g(f[6],u[2]);function
t(a){return h(k[15],a,b)}var
c=h(k[49][3],t,a),i=g(p[62][3],a),w=g(l[1][5],eI),x=g(k[20],w),m=N(aJ),y=h(k[49][3],x,a),z=O===m?aJ[1]:H===m?g(L[2],aJ):aJ,n=N(aI),A=g(bK[48],z),B=O===n?aI[1]:H===n?g(L[2],aI):aI,o=g(bK[48],B),C=g(k[49][4],a),r=bY(bb[b8],0,0,0,bb[105],C),d=g(j[bg],r[2]),D=[0,[0,y],d,g(j[I],1)],E=g(j[ah],D),F=g(l[1][5],eJ),G=g(k[20],F),s=h(k[49][3],G,a);function
J(a){return bR(c,at,o,i,a)}var
K=h(k[49][3],J,a),P=[0,c,d,K,b,e,g(j[aa],s)],Q=[0,c,b,e],R=0,Y=ad(aH,P,function(a){return ad(bL,[0,d,o,E,A,i,a],bS)}),S=[0,g(p[66][1],Y),R],T=[0,M(u),S],U=[0,s],X=ae(v,Q,function(a){return aM(U,a)}),V=h(q[70][19],X,T),W=g(p[60][1],r[1]);return h(q[70][3],W,V)}];return g(p[62][9],J)}var
n=o[1],s=h(aG[13],c,[0,-608347012,[0,n[1],n[2]]]),w=h(f[20],c,n[1]),t=h(f[20],c,n[2]),e=n[3];if(typeof
e==="number")return M(s);else
switch(e[0]){case
0:var
au=e[1],B=[0,function(a){var
b=g(f[6],w),d=g(f[6],t);function
e(a){return h(k[15],a,b)}var
i=[0,h(k[49][3],e,a),b,d],m=g(l[1][5],eE),n=g(k[20],m),c=h(k[49][3],n,a),o=[0,au,[0,g(j[aa],c)]],p=g(j[D],o),r=[0,g(F[99],p),0],u=[0,M(s),r],x=[0,c],y=ae(v,i,function(a){return aM(x,a)});return h(q[70][19],y,u)}];return g(p[62][9],B);case
1:return bT(e[1],w,t,s);default:var
av=e[2],aw=e[1],E=[0,function(a){var
c=g(f[6],t),d=g(l[1][5],eH),e=g(k[20],d),b=h(k[49][3],e,a),i=[0,av,[0,g(j[aa],b)]],m=g(j[D],i),n=[0,g(F[99],m),0],o=[0,bT(aw,w,t,s),n],p=aM([0,b],c);return h(q[70][19],p,o)}];return g(p[62][9],E)}}}var
ax=g(i[1],eW);return h(q[70][4],0,ax)}];return g(p[62][9],a)}function
bV(a){var
b=g(i[1],eX);return h(x[7],eY,b)}function
bW(a,b){var
c=g(p[66][1],bV),d=bU(a,b),e=g(q[70][28],F[17]),f=h(q[70][3],e,d);return h(q[70][12],f,c)}function
eZ(a,b,c,d){var
e=N(a);function
f(l){var
a=[0,function(a){function
n(a){return h(au[2],0,a)}var
e=m(k[49][1],n,a,b),o=e[2],r=e[1],s=g(k[49][5],a),f=fQ(eb[4],0,0,e0,s,r,o),i=g(j[D],[0,l,[0,f[2],b,c]]),t=f[1],u=g(k[49][5],a),v=G(au[2],0,u,t,i),w=g(d,i),x=g(ba[11],v[1]),y=g(p[66][1],x);return h(q[70][3],y,w)}];return g(p[62][10],a)}var
i=O===e?a[1]:H===e?g(L[2],a):a;return h(q[70][57],i,f)}var
e1=[0,function(a){var
u=g(p[62][3],a);function
w(a,b){try{var
c=ae(bM,[0],F[85]),d=[0,g(q[70][22],c),0],e=[0,g(p[13],0),d],f=eZ(v,a,b,F[145]),i=h(q[70][19],f,e);return i}catch(f){f=r(f);if(g(p[66][10],f))return h(p[18],0,f);throw f}}function
x(a){var
b=a[1];return b[1]===d_[1]?g(p[13],0):h(p[18],[0,a[2]],b)}var
d=g(j[P],u);if(9===d[0]){var
e=d[2];if(3===e.length-1){var
k=N(v),y=e[2],z=e[3],A=d[1],B=O===k?v[1]:H===k?g(L[2],v):v;if(h(av[11],B,A)){var
l=g(j[P],y),m=g(j[P],z);if(9===l[0])if(9===m[0]){var
o=m[2],f=l[2];if(f.length-1===o.length-1)var
t=function(a){if(0<=a){var
b=t(a-1|0),c=s(o,a)[a+1],d=w(s(f,a)[a+1],c);return h(q[70][16],d,b)}var
e=bW(bh,0);return g(q[70][22],e)},n=t(f.length-1-1|0),c=1;else
var
c=0}else
var
c=0;else
var
c=0;if(!c)var
n=g(p[13],0);var
i=n,b=1}else
var
b=0}else
var
b=0}else
var
b=0;if(!b)var
i=g(p[13],0);return h(p[20],i,x)}],af=[0,M,bU,bV,bW,g(p[62][9],e1)];az(192,af,"Cc_plugin.Cctac");g(bf[12],e4);try{var
fo=0,fq=[0,function(a){if(a){var
b=a[2];if(b)if(!b[2]){var
c=a[1],d=g(am[6],aO[6]),e=h(aN[2][7],d,c),f=b[1],i=g(am[17],aP[8]),j=g(am[6],i),k=h(aN[2][7],j,f);return function(a){return h(af[4],e,k)}}}return g(z[2],fp)},fo],fs=[0,function(a){if(a)if(!a[2]){var
b=a[1],c=g(am[17],aP[8]),d=g(am[6],c),e=h(aN[2][7],d,b);return function(a){return h(af[4],bh,e)}}return g(z[2],fr)},fq],fu=[0,function(a){if(a)if(!a[2]){var
b=a[1],c=g(am[6],aO[6]),d=h(aN[2][7],c,b);return function(a){return h(af[4],d,0)}}return g(z[2],ft)},fs],fw=[0,function(a){return a?g(z[2],fv):function(a){return h(af[4],bh,0)}},fu],fx=g(e3[12],fw);m(be[9],0,[0,ag,fy],fx);var
fz=function(a){var
j=g(l[1][6],fA),b=aP[8],f=0,i=0;if(0===b[0]){var
k=[0,fB,[0,[1,ax[4],[0,[5,[0,b[1]]]],j],i]],m=g(l[1][6],fC),c=aO[6];if(0===c[0]){var
n=[0,[0,fD,[0,[1,ax[4],[5,[0,c[1]]],m],k]],f],p=g(l[1][6],fE),d=aP[8],o=0;if(0===d[0]){var
q=[0,[0,fG,[0,fF,[0,[1,ax[4],[0,[5,[0,d[1]]]],p],o]]],n],s=g(l[1][6],fH),e=aO[6],r=0;if(0===e[0])return h(e2[4],[0,ag,fK],[0,fJ,[0,[0,fI,[0,[1,ax[4],[5,[0,e[1]]],s],r]],q]]);throw[0,ao,fL]}throw[0,ao,fM]}throw[0,ao,fN]}throw[0,ao,fO]};h(bf[19],fz,ag)}catch(f){f=r(f);if(!g(x[22],f))throw f;var
e5=h(x[18],0,f),e8=h(z[16],e7,e6),e_=h(z[16],e9,e8),e$=g(i[1],e_),fa=h(i[13],e$,e5);g(aq[13],fa)}function
fb(a){var
b=[28,[0,0,[31,ax[4],[0,[0,ag,fc],0],0]]],c=g(l[1][5],fd);return G(be[4],1,0,c,b)}try{var
fm=[0,function(a,b){return af[5]}];m(be[9],0,[0,ag,fn],fm);h(bf[19],fb,ag)}catch(f){f=r(f);if(!g(x[22],f))throw f;var
fe=h(x[18],0,f),fh=h(z[16],fg,ff),fj=h(z[16],fi,fh),fk=g(i[1],fj),fl=h(i[13],fk,fe);g(aq[13],fl)}var
bX=[0,ag];az(202,bX,"Cc_plugin.G_congruence");az(203,[0,f,aG,af,bX],"Cc_plugin");return}(function(){return this}()));
