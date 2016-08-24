(function(a){"use strict";var
a2="Firstorder",al=140,am=145,ab=112,a1="$l",bD="already done",aY="firstorder_using",bQ=",",bF=250,aB=": ",aA="$t",ay="gintuition",X=148,bP="Exception in vernac extend ",N=124,aX=246,bO=115,R="Extension: cannot occur",aW=113,bN=144,bE=122,bM="with",bC="Depth",aa="firstorder",a0="ground_plugin",az="Firstorder_Print_Solver",aV=100,bL="Solver",S="plugins/firstorder/g_ground.ml4",bG="Exception in tactic extend ",bK=248,ax="Firstorder_Set_Solver",aU="using",bJ="-----",aZ="using ",W=114,bI=121,bH="reversible in 1st order mode",w=a.jsoo_runtime,M=w.caml_check_bound,bz=w.caml_fresh_oo_id,b=w.caml_new_string,bA=w.caml_obj_tag,Q=w.caml_register_global,s=w.caml_wrap_exception;function
h(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
i(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
m(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
t(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
H(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
aT(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):w.caml_call_gen(a,[b,c,d,e,f,g])}function
bB(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}function
fA(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):w.caml_call_gen(a,[b,c,d,e,f,g,h,i])}var
c=w.caml_get_global_data(),V=b(a0),bq=[0,b(a0),b("auto_with")],k=c.Term,o=c.Util,ac=c.Context,E=c.Vars,y=c.Termops,ao=c.Hipattern,aC=c.Global,an=c.Inductiveops,I=c.Closure,u=c.Tacmach,z=c.Names,ae=c.Int,K=c.Not_found,C=c.Queue,af=c.Evd,aF=c.Reductionops,J=c.Assert_failure,g=c.Pp,a7=c.Ppconstr,as=c.Printer,aH=c.Hints,q=c.Pervasives,D=c.Errors,ah=c.Globnames,ag=c.Option,aI=c.Heap,a_=c.CamlinternalLazy,n=c.Tactics,l=c.Proofview,j=c.Tacticals,bb=c.Refiner,ba=c.Sigma,A=c.Tacinterp,U=c.Feedback,bf=c.List,bg=c.Classops,bn=c.Cc_plugin,bo=c.Auto,v=c.Constrarg,G=c.Loc,aN=c.Tacentries,d=c.Genarg,aw=c.Stdarg,bi=c.Tacintern,bm=c.Egramml,av=c.Vernac_classifier,bh=c.Vernacinterp,bj=c.Locality,aM=c.Tacenv,F=c.Pcoq,aO=c.Mltop,bk=c.Goptions,aP=c.Geninterp,bl=c.Genintern,Z=c.CList,aj=c.CLexer,bp=c.Array,a3=[0,1],aD=[0,I[12]],bT=b("Formula.Is_atom"),bV=[0,0,[0,0,0]],bW=b("_"),bY=b("Unify.UFAIL"),ch=b(" : "),ci=b("| "),cj=b(bJ),ck=b(bJ),ce=b(" : No such Hint database"),cf=b("Firstorder: "),cI=b("iff"),cJ=b("not"),cG=[0,b("Init"),[0,b("Logic"),0]],cH=b("User"),cD=b(bH),ct=b("No link"),cr=b("No axiom link"),cp=[0,b("plugins/firstorder/rules.ml"),52,7],cn=b("Not the expected number of hyps"),cX=b("not implemented ... yet"),cV=b("Untypable instance, maybe higher-order non-prenex quantification"),cU=b(bD),cW=b(bD),cR=b("can't happen"),cS=b("x"),cY=[0,0],cZ=b(bH),eJ=b(aA),eM=[0,b(S),1,0],eK=[0,b(ay)],eL=b(ay),eE=b(R),eX=b("$l'"),fe=[0,b(S),1,0],eY=[0,b(bM)],eZ=b(a1),fd=[0,b(S),1,0],e0=b(aA),fc=[0,b(S),1,0],e1=[0,b(aa)],e2=b(a1),fb=[0,b(S),1,0],e3=[0,b(bM)],e4=b(aA),fa=[0,b(S),1,0],e5=[0,b(aa)],e6=b(a1),e$=[0,b(S),1,0],e7=b(aA),e_=[0,b(S),1,0],e8=[0,b(aa)],e9=b(aa),eS=b(R),eQ=b(R),eO=b(R),eb=b('Deprecated syntax; use "," as separator'),dN=b(aZ),dM=b(aZ),dL=b(aZ),dH=b(az),fp=b(az),fm=b(R),fk=b(az),fh=b("Firstorder solver tactic is "),fg=b(R),dw=b(ax),fz=b(ax),fw=b(R),fu=b(ax),fr=b(R),c6=b(a0),c8=[0,b(a2),[0,b(bC),0]],c9=b("Firstorder Depth"),da=[0,b("Congruence"),[0,b(bC),0]],db=b("Congruence Depth"),df=b("Firstorder default solver"),dh=b(aB),di=b(ax),dk=b(bP),dr=[0,b(bL)],ds=[0,b(a2)],dt=[0,b("Set")],dy=b(aB),dz=b(az),dB=b(bP),dF=[0,[0,[0,b("Print")],[0,[0,b(a2)],[0,[0,b(bL)],0]]],0],dI=b("GTauto failed"),dO=b(aY),dW=b(aY),d1=b(aU),d4=b(bQ),d7=b(bQ),d_=b(aU),ef=b(aU),ek=b(aY),eV=b(aa),em=b(aB),en=b(aa),ep=b(bG),eH=b(ay),eu=b(aB),ev=b(ay),ex=b(bG),b2=c.Constrextern,cl=c.Coqlib,cm=c.Control,cL=c.Typing,cM=c.Evarutil,cN=c.Environ,c4=c.Flags,c1=c.Tacsubst,c3=c.Libnames,c0=c.Tactic_option,c2=c.Pptactic,c5=c.Decl_mode_plugin;function
bR(a,b,c,d,e,f){var
g=i(a,c,d);return 0===g?i(b,e,f):g}function
bS(a,b,c,d,e,f,g,h){var
j=t(a,c,d,e,f);return 0===j?i(b,g,h):j}var
ad=[bK,bT,bz(0)];function
a4(a,b){var
c=a,d=b;for(;;){var
e=h(k[al],d);if(6===e[0]){var
f=e[3];if(0<c){var
c=c-1|0,d=f;continue}return 1+a4(0,f)|0}return 0}}function
bU(a,b){var
c=h(aC[26],a[1])[1][6],d=h(u[8],b),e=i(an[4],d,a);function
f(a){return a4(c,a)}return i(o[19][15],f,e)}function
ap(e,b,c,d){var
a=h(u[8],d),f=i(an[4],a,b);function
g(a){var
b=i(k[76],a,c),d=i(k[85],e,b)[2];return h(k[83],d)[1]}return i(o[19][15],g,f)}function
aE(a){var
b=h(u[8],a),c=m(I[37],0,aD[1],b);return function(a){var
b=h(I[31],a);return i(I[41],c,b)}}function
aq(a,b){var
E=aE(a),B=h(u[8],a),C=m(I[37],0,aD[1],B),D=h(I[31],b),c=i(I[42],C,D),n=h(ao[23],c);if(n){var
p=n[1],F=h(y[54],p[2]);return[0,p[1],F]}var
q=h(ao[21],c);if(q){var
r=q[1];return[5,r[2],r[3]]}var
s=h(ao[27],c);if(s){var
e=s[1],f=e[2],t=h(k[43],e[1]),g=t[2],d=t[1],v=h(aC[26],d),j=v[2],w=v[1],x=j[4].length-1;if(0===x)return[1,[0,d,g],f];var
G=0<e[3]?1:0,H=function(a){var
b=w[6];return h(y[69],a)===b?1:0},l=i(o[19][28],H,j[9]);if(!h(an[19],[0,d,w,j])){var
K=G?l?1:0:1;if(K)return 1===x?[2,[0,d,g],f,l]:[3,[0,d,g],f,l]}return[6,c]}var
z=h(ao[29],c);if(z){var
A=z[1],J=A[2];return[4,h(k[43],A[1]),J]}return[6,h(E,c)]}var
bX=[0,h(z[1][5],bW)];function
a5(n,b,c,d){var
p=[0,0],j=[0,0],l=[0,0],A=aE(n);function
g(a,e,c){var
f=a,q=c;for(;;){var
d=aq(n,q);switch(d[0]){case
0:g(f,1-e,d[1]);var
q=d[2];continue;case
1:var
s=1-e,B=s?(p[1]=1,0):s;return B;case
4:var
x=d[2],G=h(b,1),H=h(k[W],G),I=M(ap(1,d[1],x,n),0)[1],J=function(a,b,c){var
d=h(ac[1][1][3],c);return g([0,H,f],e,i(E[8],a,d))},K=2-h(o[17][1],x)|0;return t(o[17][83],J,K,0,I);case
5:var
L=h(b,1),N=h(k[W],L),f=[0,N,f],q=d[2];continue;case
6:var
r=m(E[11],f,0,d[1]),y=1-h(k[7],r);if(y){if(e){j[1]=[0,r,j[1]];return 0}l[1]=[0,r,l[1]];var
z=0}else
var
z=y;return z;default:if(d[3]){var
u=h(A,m(E[11],f,0,q));if(e)j[1]=[0,u,j[1]];else
l[1]=[0,u,l[1]]}var
v=ap(0,d[1],d[2],n),C=function(a,b,c){var
d=h(ac[1][1][3],c);return g(f,e,i(E[8],a,d))},D=function(a){var
b=1-h(o[17][1],a)|0;return t(o[17][83],C,b,0,a)};if(e)var
F=function(a){return a?0:1},w=i(o[19][28],F,v);else
var
w=e;if(w)p[1]=1;return i(o[19][13],D,v)}}}switch(c){case
0:g(0,0,d);break;case
1:g(0,1,d);break;default:var
a=h(k[79],d),e=a[1],f=function(a){var
c=h(b,1);return h(k[W],c)},q=i(o[17][14],f,e);g(q,0,a[2]);p[1]=0}return[0,p[1],[0,j[1],l[1]]]}var
p=[0,a3,aD,bR,bS,bU,ap,bX,a5,function(a,b,c,d,e){var
l=aE(d);try{var
p=h(e,0)+1|0,q=a3[1]?a5(d,e,a,c):bV,r=q[1];if(1===a){var
m=aq(d,c);switch(m[0]){case
0:var
j=0;break;case
1:var
j=3;break;case
2:var
j=1;break;case
3:var
j=2;break;case
4:var
w=M(ap(0,m[1],m[2],d),0)[1],x=h(o[17][104],w),j=[0,p,h(ac[1][1][3],x),r];break;case
5:var
j=4;break;default:throw[0,ad,m[1]]}var
t=[1,j]}else{var
g=aq(d,c);switch(g[0]){case
0:var
n=g[1],y=h(l,n),f=aq(d,n);switch(f[0]){case
0:var
i=[5,f[1],f[2],g[2]];break;case
1:var
i=[0,f[1],f[2]];break;case
2:var
i=[1,f[1],f[2]];break;case
3:var
i=[2,f[1],f[2]];break;case
4:var
i=[4,f[1],f[2]];break;case
5:var
i=[3,n];break;default:var
i=0}var
k=[4,y,i];break;case
1:var
k=0;break;case
2:if(g[3])throw[0,ad,h(l,c)];var
k=[0,g[1]];break;case
3:if(g[3])throw[0,ad,h(l,c)];var
k=[1,g[1]];break;case
4:var
k=[3,g[1]];break;case
5:var
k=[2,p,g[1],r];break;default:throw[0,ad,g[1]]}var
t=[0,k]}var
u=q[2],v=[0,[0,b,h(l,c),t,u]];return v}catch(f){f=s(f);if(f[1]===ad)return[1,f[2]];throw f}}];Q(119,p,"Ground_plugin.Formula");var
O=[bK,bY,bz(0)];function
aG(a,b){var
f=h(C[2],0),p=[0,0];function
t(c,b){var
a=p[1];function
d(a){var
d=i(y[53],[0,[0,c,b],0],a[2]);return[0,a[1],d]}p[1]=[0,[0,c,b],i(o[17][12],d,a)];return 0}function
u(a){var
b=h(k[al],a);if(2===b[0])try{var
c=u(i(ae[4][2],b[1],p[1]));return c}catch(f){f=s(f);if(f===K)return a;throw f}return a}i(C[3],[0,a,b],f);try{for(;;){var
A=h(C[5],f),g=u(i(aF[22],af[16],A[1])),j=u(i(aF[22],af[16],A[2])),e=h(k[al],g),c=h(k[al],j);switch(e[0]){case
2:var
q=e[1];if(2===c[0]){var
x=c[1];if(1-(q===x?1:0))if(q<x)t(x,g);else
t(q,j)}else{var
w=i(y[53],p[1],j),Z=h(y[43],w);if(h(ae[2][2],Z))var
_=h(k[W],q),S=i(y[52],_,w)?0:(t(q,w),1);else
var
S=0;if(!S)throw[0,O,g,j]}var
d=3;break;case
6:switch(c[0]){case
6:var
G=c[3],F=c[2],E=e[3],D=e[2],d=4;break;case
2:var
d=0;break;case
5:var
d=1;break;default:var
d=2}break;case
7:switch(c[0]){case
7:var
G=c[3],F=c[2],E=e[3],D=e[2],d=4;break;case
2:var
d=0;break;case
5:var
d=1;break;default:var
d=2}break;case
9:var
H=e[2];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
9:var
I=c[2];i(C[3],[0,e[1],c[1]],f);var
J=H.length-1;if(J!==I.length-1)throw[0,O,g,j];var
L=J-1|0,ab=0;if(!(L<0)){var
l=ab;for(;;){var
ac=M(I,l)[l+1],ad=[0,M(H,l)[l+1],ac];i(C[3],ad,f);var
ag=l+1|0;if(L!==l){var
l=ag;continue}break}}var
d=3;break;default:var
d=2}break;case
13:var
N=e[4];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
13:var
P=c[4];i(C[3],[0,e[2],c[2]],f);i(C[3],[0,e[3],c[3]],f);var
Q=N.length-1;if(Q!==P.length-1)throw[0,O,g,j];var
R=Q-1|0,ah=0;if(!(R<0)){var
m=ah;for(;;){var
ai=M(P,m)[m+1],aj=[0,M(N,m)[m+1],ai];i(C[3],aj,f);var
ak=m+1|0;if(R!==m){var
m=ak;continue}break}}var
d=3;break;default:var
d=2}break;default:var
d=0}switch(d){case
0:if(2===c[0]){var
B=c[1],v=i(y[53],p[1],g),X=h(y[43],v);if(h(ae[2][2],X)){var
Y=h(k[W],B);if(i(y[52],Y,v))var
z=1;else{t(B,v);var
n=2,z=0}}else
var
z=1;if(z)throw[0,O,g,j]}else
if(5===e[0]){var
V=[0,h(k[99],g),j];i(C[3],V,f);var
n=2}else
var
n=0;break;case
1:var
n=0;break;case
2:var
n=1;break;case
3:var
n=2;break;default:i(C[3],[0,D,F],f);var
$=h(y[54],G),aa=[0,h(y[54],E),$];i(C[3],aa,f);var
n=3}switch(n){case
0:if(5===c[0]){var
U=[0,g,h(k[99],j)];i(C[3],U,f);var
r=1}else
var
r=0;break;case
1:var
r=0;break;case
2:var
r=1;break;default:var
r=2}switch(r){case
0:if(1-i(k[139],g,j))throw[0,O,g,j];var
T=0;break;case
1:var
T=0;break;default:var
T=1}continue}}catch(f){f=s(f);if(f===C[1])return p[1];throw f}}function
bZ(c,b){function
d(a){if(h(k[7],a))if(h(k[30],a)===c)return 0;function
e(a,b){var
c=d(b);return 0<=a?0<=c?a+c|0:a:c}var
b=m(k[142],e,-1,a);return 0<=b?b+1|0:-1}return d(b)}function
b0(a){var
c=[0,1],d=[0,0];function
e(a,b){var
f=h(k[al],b);if(2===f[0]){var
g=f[1];try{var
m=a+i(ae[4][2],g,d[1])|0,n=h(k[ab],m);return n}catch(f){f=s(f);if(f===K){var
j=c[1];c[1]++;d[1]=[0,[0,g,j],d[1]];return h(k[ab],j+a|0)}throw f}}function
l(a){return a+1|0}return t(k[bN],l,e,a,b)}var
b=e(0,a);return[0,c[1]-1|0,b]}function
b1(a,b,c,d){try{var
g=aG(c,d),e=i(ae[4][2],a,g);if(h(k[7],e))var
f=[0,[1,b]];else
var
j=bZ(a,c),f=[0,[0,b0(e),j]];return f}catch(f){f=s(f);if(f[1]===O)return 0;if(f===K)return[0,[1,b]];throw f}}function
a6(d,b,c){function
a(a){return h(k[W],d+a|0)}var
e=i(o[17][48],b,a);return i(E[12],e,c)}var
ar=[0,O,aG,b1,function(a,b){var
c=a[1],d=a6(0,c,a[2]),e=a6(c,b[1],b[2]);try{var
f=aG(d,e),g=function(a){var
b=a[1]<c?1:0;return b?b:h(k[7],a[2])},j=i(o[17][22],g,f);return j}catch(f){f=s(f);if(f[1]===O)return 0;throw f}}];Q(126,ar,"Ground_plugin.Unify");function
a8(a){if(0===a[0]){var
b=a[1];if(typeof
b==="number")return 999;else
switch(b[0]){case
0:return 90;case
1:return 40;case
2:return-30;case
3:return 60;default:var
c=b[2];if(typeof
c==="number")return 0;else
switch(c[0]){case
0:return aV;case
1:return 80;case
2:return 70;case
3:return-20;case
4:return 50;default:return-10}}}var
d=a[1];if(typeof
d==="number")switch(d){case
1:return 40;case
2:return-15;case
3:return-50;default:return aV}return-29}var
b3=[0,function(a,b){var
c=a8(b[3]);return a8(a[3])-c|0}],aJ=[0,k[141]],b4=[0,function(a,b){var
c=i(ah[18][1],a[1],b[1]);if(0===c){var
d=function(a,b){var
c=w.caml_int_compare(a[1],b[1]);return 0===c?i(aJ[1],a[2],b[2]):c};return m(ag[5],d,a[2],b[2])}return c}],e=h(o[21][1],aJ),ai=h(o[20][1],b4);function
at(a,b,c){try{var
d=[0,b,i(e[22],a,c)],f=m(e[4],a,d,c);return f}catch(f){f=s(f);if(f===K)return m(e[4],a,[0,b,0],c);throw f}}function
a9(a,b,c){try{var
f=i(e[22],a,c),g=function(a){return 1-i(ah[5],a,b)},d=i(o[17][29],g,f),h=d?m(e[4],a,d,c):i(e[6],a,c);return h}catch(f){f=s(f);if(f===K)return c;throw f}}var
T=h(aI[2],b3);function
b5(a){var
b=a.slice();b[8]=a[8]-1|0;return b}function
b6(a,b){var
c=b.slice();c[7]=i(ai[4],a,b[7]);return c}function
b7(a,b){var
d=i(ai[3],a,b[7]);if(d)var
e=d;else{var
c=a[2];if(c){var
f=c[1],j=f[1],k=a[1],g=function(a){var
b=a[2];if(b){var
c=b[1],d=i(ah[5],k,a[1]);if(d){var
e=j<c[1]?1:0;if(e)return i(ar[4],c,f);var
g=e}else
var
g=d;var
h=g}else
var
h=b;return h};return i(ai[16],g,b[7])}var
e=c}return e}function
aK(a,b,c,d,e){var
h=H(p[9],a,b,c,e,d[6]);if(0===h[0]){var
f=h[1];if(1===a){var
m=d[8],n=d[7],o=d[6],q=f[2],r=d[3],s=d[2];return[0,i(T[2],f,d[1]),s,r,q,0,o,n,m]}var
j=d.slice();j[1]=i(T[2],f,d[1]);j[2]=at(f[2],b,d[2]);return j}var
g=h[1];if(1===a){var
k=d.slice();k[4]=g;k[5]=[0,g];return k}var
l=d.slice();l[2]=at(g,b,d[2]);l[3]=[0,g,d[3]];return l}function
b8(a,b){function
d(a,b){return a[1]===p[7]?b:at(a[2],a[1],b)}var
c=b.slice();c[1]=m(o[17][16],T[2],a,b[1]);c[2]=m(o[17][16],d,a,b[2]);return c}function
b9(a,b){var
c=i(e[22],a,b[2]);return h(o[17][3],c)}function
b_(a){var
b=a;for(;;){var
c=h(T[3],b[1]),f=h(T[4],b[1]);if(c[1]===p[7]){var
d=b.slice();d[1]=f;if(b[4]===c[2])return[0,c,d];var
b=d;continue}var
e=b.slice();e[1]=f;e[2]=a9(c[2],c[1],b[2]);return[0,c,e]}}function
b$(a){var
b=[0,-1],d=ai[1];function
c(a){if(a)b[1]++;return b[1]}var
f=h(k[W],1);return[0,T[1],e[1],0,f,0,c,d,a]}function
ca(a){if(2===a[0]){var
b=a[1],c=function(a){return[3,[0,b,a+1|0]]},d=h(an[21],b);return i(o[17][48],d,c)}return[0,a,0]}var
cb=h(o[17][ab],ca);function
cc(a,b,c){var
d=h(cb,a);function
e(a,b){var
e=b[2];function
f(a){return t(af[160],0,0,0,a)}var
d=m(u[24],f,e,a),c=d[1],g=i(u[15],c,d[2]);return[0,aK(0,a,g,b[1],c),c]}return m(o[17][16],e,d,[0,b,c])}function
cd(a,b,c){var
d=[0,b];function
e(a){var
b=h(aH[25],a[5]);switch(b[0]){case
1:case
4:case
5:return 0;default:var
e=b[1][1][1];try{var
f=h(ah[16],e),g=i(u[15],c,e);d[1]=aK(2,f,g,d[1],c);var
j=0;return j}catch(f){f=s(f);if(f===K)return 0;throw f}}}function
f(a,b,c){return i(o[17][11],e,c)}function
g(a){try{var
e=h(aH[10],a),b=e}catch(f){f=s(f);if(f!==K)throw f;var
c=i(q[16],a,ce),d=i(q[16],cf,c),b=h(D[6],d)}return i(aH[9][12],f,b)}i(o[17][11],g,a);return[0,d[1],c]}function
cg(a){function
b(a,b,c){var
d=af[16],e=h(aC[2],0),f=H(b2[6],0,0,e,d,a),j=h(g[18],0),k=h(a7[22],f),l=h(g[1],ch),m=i(g[52],as[42],b),n=h(g[1],ci),o=i(g[14],n,m),p=i(g[14],o,l),q=i(g[14],p,k),r=i(g[14],q,j);return i(g[14],r,c)}var
c=h(g[1],cj),d=h(g[9],0),f=m(e[11],b,a,d),j=h(g[18],0),k=h(g[1],ck),l=i(g[14],k,j),n=i(g[14],l,f),o=i(g[14],n,c);return i(g[28],0,o)}var
f=[0,aJ,[0,e[1],e[2],e[3],e[4],e[5],e[6],e[7],e[8],e[9],e[10],e[11],e[12],e[13],e[14],e[15],e[16],e[17],e[18],e[19],e[20],e[21],e[22],e[23],e[24]],ai,at,a9,T,b5,b6,b7,aK,b8,b9,b_,b$,cc,cd,cg];Q(137,f,"Ground_plugin.Sequent");function
x(a,b,c,d,e){h(cm[2],0);var
l=h(u[9],e),q=h(u[8],e);function
r(a,b,c){var
l=a,k=b,j=c;for(;;){if(0<l){if(k){var
s=k[2],n=k[1],p=h(ac[2][1][1],n),t=h(u[7],e);if(!m(y[41],q,p,t)){var
v=i(y[42],q,p);if(!i(o[17][23],v,j)){var
w=r(l-1|0,s,[0,n,j]),x=h(ac[2][1][3],n);return H(f[10],0,[0,p],x,w,e)}}var
l=l-1|0,k=s,j=[0,n,j];continue}var
z=h(g[1],cn);return m(D[3],0,0,z)}return d}}var
j=r(a,l,0);if(b)var
n=h(u[7],e),k=H(f[10],1,p[7],n,j,e);else
var
k=j;return i(c,k,e)}function
co(a){if(0===a[0])return a[1];throw[0,J,cp]}function
L(a){if(0===a[0]){var
b=h(n[74],[0,a[1],0]);return h(l[66][8],b)}return j[1]}function
cq(a,b){try{var
d=function(a){var
b=h(n[42],a);return h(l[66][8],b)},e=i(f[12],a,b),k=i(j[67],e,d);return k}catch(f){f=s(f);if(f===K){var
c=h(g[1],cr);return i(j[24],0,c)}throw f}}function
cs(a,b,c,d,e){var
p=0,q=1;function
r(a){return x(q,p,d,e,a)}try{var
u=[0,h(l[66][8],n[16]),0],v=[0,L(c),u],w=function(b){function
a(a){var
c=[0,h(k[N],[0,a,[0,b]]),0],d=h(n[X],c);return h(l[66][8],d)}return i(j[67],c,a)},y=i(f[12],a,e),z=[0,i(j[67],y,w),v],A=h(j[7],z),o=A}catch(f){f=s(f);if(f!==K)throw f;var
t=h(g[1],ct),o=i(j[24],0,t)}return m(j[33],o,r,b)}function
cu(a,b,c){var
d=1,e=0;function
f(a){return x(e,d,b,c,a)}var
g=h(l[66][8],n[bI]);return m(j[33],g,f,a)}function
cv(a,b,c){var
d=1,e=0;function
f(a){return x(e,d,b,c,a)}var
g=h(j[22],f),k=[0,h(l[66][1],g)],m=i(n[111],0,k),o=h(l[66][8],m);return i(j[4],o,a)}function
cw(a,b,c){var
d=1,e=1;function
f(a){return x(e,d,b,c,a)}var
g=h(j[22],f),k=h(l[66][8],n[17]),o=i(j[5],k,g),p=i(j[4],o,a),q=1,r=1;function
s(a){return x(r,q,b,c,a)}var
t=h(l[66][8],n[16]);return m(j[33],t,s,p)}function
cx(a,b,c,d,e,f){var
g=M(i(p[5],a,f),0)[1],k=0;function
m(a){return x(g,k,d,e,a)}var
o=h(l[66][8],n[16]),q=[0,i(j[26],g,o),0],r=[0,L(c),q],s=i(j[70][57],c,n[99]),u=[0,h(l[66][8],s),r],v=h(j[7],u);return t(j[33],v,m,b,f)}function
cy(a,b,c,d,e,f){var
g=i(p[5],a,f);function
k(b){var
a=0,f=0,g=[0,function(a){return x(b,f,d,e,a)},a],k=h(l[66][8],n[16]),m=[0,i(j[26],b,k),g],o=[0,L(c),m];return h(j[7],o)}var
m=i(o[19][15],k,g),q=i(j[70][57],c,n[99]),r=h(l[66][8],q);return t(j[35],r,m,b,f)}function
cz(a){var
b=i(j[70][57],a,n[99]);return h(l[66][8],b)}function
cA(a,b,c,d,e,f,g){var
r=a[2],s=a[1],q=t(p[6],0,a,b,g),m=q.length-1,u=h(o[19][12],b),v=0;function
w(a){return x(m,v,e,f,a)}var
z=h(l[66][8],n[16]),A=[0,i(j[26],m,z),0],B=[0,L(d),A];function
C(p){function
a(a){var
c=M(q,a)[a+1],b=h(o[17][1],c),d=[0,h(k[131],[0,[0,s,a+1|0],r]),u],e=h(k[N],d);function
f(a){return h(k[ab],b-a|0)}var
g=i(o[19][2],b,f),j=[0,i(E[8],b,e),g],l=[0,h(k[N],j)],m=[0,i(E[8],b,p),l],n=h(k[N],m);return i(y[23],n,c)}var
b=i(o[17][48],m,a),c=h(n[X],b);return h(l[66][8],c)}var
D=[0,i(j[67],d,C),B],F=h(j[7],D);return t(j[33],F,w,c,g)}function
cB(m,b,c,d,e,f,g){var
a=[0,0,m,i(E[8],1,b)],o=h(k[bI],a),p=0,q=0,r=0,s=1,t=2;function
u(a){return x(t,s,f,g,a)}var
v=[0,h(j[22],u),r],w=[0,h(l[66][8],n[17]),v],y=[0,h(l[66][8],n[17]),w],z=[0,L(e),y];function
A(a){var
c=h(k[ab],2),d=[0,0,i(E[8],1,m),c],e=[0,a,[0,h(k[bE],d)]],f=[0,0,b,h(k[N],e)],g=[0,h(k[bE],f),0],j=h(n[X],g);return h(l[66][8],j)}var
B=[0,i(j[67],e,A),z],C=[0,h(j[7],B),q];function
D(a){var
b=h(n[42],a);return h(l[66][8],b)}var
F=[0,i(j[67],e,D),C],G=h(n[am],o),H=h(l[66][8],G),I=[0,i(j[11],H,F),p],J=0,K=0,M=1,O=[0,function(a){return x(M,K,f,g,a)},J],P=[0,L(e),O],Q=[0,h(l[66][8],n[17]),P],R=[0,h(j[7],Q),I],S=h(n[am],c),T=h(l[66][8],S),U=i(j[11],T,R);return i(j[4],U,d)}function
cC(a,b,c){if(p[1][1])var
e=h(g[1],cD),d=i(j[24],0,e);else
var
d=a;var
f=1,k=0;function
o(a){return x(k,f,b,c,a)}var
q=h(j[22],o),r=h(l[66][8],n[17]),s=i(j[5],r,q),t=i(j[4],s,a),u=1,v=0;function
w(a){return x(v,u,b,c,a)}var
y=h(l[66][8],n[16]),z=m(j[33],y,w,t);return i(j[4],z,d)}function
cE(a,b,c,d,e,f){var
g=M(i(p[5],a,f),0)[1],k=0,m=0,o=g-1|0,q=[0,function(a){return x(o,m,d,e,a)},k],r=h(l[66][8],n[16]),s=[0,i(j[26],g,r),q],u=[0,L(c),s],v=h(j[7],u),w=i(j[70][57],c,n[99]),y=h(l[66][8],w);return t(j[33],y,v,b,f)}function
cF(a,b,c,d,e){var
g=0,o=h(f[7],e),p=1,q=0;function
r(a){return x(q,p,d,o,a)}var
s=[0,h(j[22],r),g],t=0,v=h(f[7],e),w=0,y=1;function
z(a){return x(y,w,d,v,a)}var
A=[0,h(j[22],z),t],B=[0,h(l[66][8],n[16]),A],C=[0,L(c),B];function
D(a,b){var
c=i(u[11],b,1),d=[0,a,[0,h(k[aW],c)]],e=h(k[N],d),f=h(n[74],[0,c,0]),g=h(l[66][8],f),o=h(n[X],[0,e,0]),p=h(l[66][8],o);return m(j[5],p,g,b)}var
E=[0,i(j[67],c,D),C],F=[0,h(l[66][8],n[16]),E],G=[0,h(j[7],F),s],H=h(n[am],a),I=h(l[66][8],H),J=i(j[11],I,G);return i(j[4],J,b)}function
a$(a){return m(cl[4],cH,cG,a)}var
P=[aX,function(a){var
b=a$(cI),c=[0,[0,0,[1,h(k[41],b)[1]]],0],d=a$(cJ);return[0,[0,0,[1,h(k[41],d)[1]]],c]}];function
cK(a){if(a){var
b=bA(P),d=[0,a[1],1],e=bF===b?P[1]:aX===b?h(a_[2],P):P,f=i(n[68],e,d);return h(l[66][8],f)}var
c=bA(P),g=bF===c?P[1]:aX===c?h(a_[2],P):P,j=h(n[67],g);return h(l[66][8],j)}var
r=[0,x,co,L,cq,cs,cu,cv,cw,cx,cy,cz,cA,cB,cC,cE,cF,h(j[56],cK)];Q(bN,r,"Ground_plugin.Rules");function
cO(a,b){if(0===a[0]){var
c=a[1],d=c[1];if(0===b[0]){var
e=b[1],g=e[2],h=c[2],j=b[2],k=a[2],l=e[1],m=f[1][1],n=function(a,b){return a-b|0},o=function(a,b){return a-b|0},q=i(p[3],o,n);return fA(p[4],q,m,l,d,k,j,h,g)}return 0===d?1:-1}return 0===b[0]?0===b[1][1]?-1:1:i(f[1][1],a[1],b[1])}function
cP(a,b){return a===b?0:a===p[7]?1:b===p[7]?-1:i(ah[18][1],a,b)}var
cQ=[0,function(a,b){return aT(p[3],cO,cP,b[1],a[1],b[2],a[2])}],au=h(o[20][1],cQ);function
bc(a,b){var
d=[0,au[1]];function
c(a){var
l=a[3];if(0===l[0]){var
c=l[1];if(typeof
c==="number")var
p=1;else
if(2===c[0])var
x=c[3],n=c[2],w=c[1],j=1,p=0;else
var
p=1;if(p)var
j=0}else{var
e=l[1];if(typeof
e==="number")var
j=0;else
var
x=e[3],n=e[2],w=e[1],j=1}if(j){var
y=a[4],q=[0,1],r=[0,x],E=a[1],s=function(a,b){function
c(a,b){var
c=t(ar[3],w,n,a,b);if(c){var
e=c[1];return 0===e[0]?(q[1]=0,d[1]=i(au[4],[0,e,E],d[1]),0):(r[1]=1,0)}return c}var
e=a[1];function
f(d){var
a=b[2];function
e(a){return c(d,a)}return i(o[17][11],e,a)}i(o[17][11],f,e);var
g=a[2];function
h(d){var
a=b[1];function
e(a){return c(d,a)}return i(o[17][11],e,a)}return i(o[17][11],h,g)},A=b[1],B=function(a){return s(y,a[4])};i(f[6][5],B,A);var
k=b[5],z=k?[0,k[1],0]:k;s(y,[0,z,b[3]]);var
u=q[1],v=u?r[1]:u,F=v?(d[1]=i(au[4],[0,[1,n],a[1]],d[1]),0):v;return F}var
C=h(g[1],cR);return m(D[3],0,0,C)}i(o[17][11],c,a);return h(au[20],d[1])}function
bd(a){try{var
c=h(f[13],a),d=c[1],b=d[3];if(0===b[0])var
e=b[1],j=typeof
e==="number"?0:2===e[0]?1:0;else
var
j=typeof
b[1]==="number"?0:1;if(j)var
i=bd(c[2]),g=[0,[0,d,i[1]],i[2]];else
var
g=[0,0,a];return g}catch(f){f=s(f);if(f===aI[1])return[0,0,a];throw f}}var
be=h(z[1][5],cS);function
cT(a,b,c,d,e){var
r=h(u[8],c),s=h(bb[2],c);if(a===p[7])var
t=be;else
var
F=i(u[15],c,b),G=m(aF[23],r,s,F),y=h(k[34],G)[1],H=y?y[1]:be,t=H;function
z(a){return h(k[ab],d-a|0)}var
A=i(o[17][48],d,z),q=d,l=0,j=r,g=s,f=0,B=i(E[12],A,e);for(;;){if(0===q)return[0,g,f,B];var
v=m(n[14],l,t,c),C=h(ba[21][2],g),w=bB(cM[7],j,C,0,0,0,0,af[105]),D=h(ba[6],w[2]),x=[0,[0,v],w[1][1]],q=q-1|0,l=[0,v,l],j=i(cN[20],x,j),g=D,f=[0,x,f];continue}}var
Y=[0,bd,bc,function(a,b,c,d,e){var
q=bc(a,d);function
v(e){if(e[2]===p[7])var
o=e[1],a=function(a,b){if(0===o[0]){var
c=o[1];if(0===c[1]){var
d=h(f[7],b),e=[0,t(r[1],0,1,a,d),0],m=h(j[20],e),p=h(n[bO],[0,[0,c[2],0]]),q=h(l[66][8],p);return i(j[5],q,m)}var
s=h(g[1],cX);return i(j[24],0,s)}var
v=h(l[66][8],n[41]),w=[0,h(j[21],v),0],x=h(f[7],b),y=[0,t(r[1],0,1,a,x),0],z=[0,h(j[20],y),0],A=[0,function(a){var
b=i(u[11],a,1),c=[0,[0,h(k[aW],b),0]],d=h(n[bO],c);return i(l[66][8],d,a)},z],B=[0,h(l[66][8],n[17]),A],C=[0,h(j[7],B),w],D=h(n[am],o[1]),E=h(l[66][8],D);return i(j[11],E,C)};else
var
a=function(a,b){var
c=e[2],o=e[1];if(0===o[0]){var
d=o[1],p=d[2],q=d[1];if(i(f[9],[0,c,[0,d]],b)){var
w=h(g[1],cU);return i(j[24],0,w)}if(0<q)var
x=function(a,b){var
d=cT(c,a,b,q,p),f=d[2],o=h(k[N],[0,a,[0,d[3]]]),e=i(y[23],o,f);try{var
x=d[1],z=h(u[8],b),A=t(cL[2],0,z,x,e),g=A}catch(f){f=s(f);if(!h(D[22],f))throw f;var
g=h(D[6],cV)}var
r=h(n[X],[0,e,0]),v=h(l[66][8],r),w=h(bb[11],g[1]);return m(j[5],w,v,b)},v=i(j[67],c,x);else
var
F=function(a){var
b=[0,h(k[N],[0,a,[0,p]]),0],c=h(n[X],b);return h(l[66][8],c)},v=i(j[67],c,F);var
z=i(f[8],[0,c,[0,d]],b),A=h(f[7],z),B=[0,t(r[1],1,0,a,A),0],C=[0,h(j[20],B),0],E=[0,v,[0,h(l[66][8],n[17]),C]];return h(j[7],E)}if(i(f[9],[0,c,0],b)){var
G=h(g[1],cW);return i(j[24],0,G)}var
H=h(l[66][8],n[41]),I=[0,h(j[21],H),0],J=i(f[8],[0,c,0],b),K=h(f[7],J),L=[0,t(r[1],1,0,a,K),0],M=[0,h(j[20],L),0],O=[0,h(l[66][8],n[17]),M];function
P(a,b){var
c=i(u[11],b,1),d=[0,a,[0,h(k[aW],c)]],e=[0,h(k[N],d),0],f=h(n[X],e);return i(l[66][8],f,b)}var
Q=[0,i(j[67],c,P),O],R=[0,h(l[66][8],n[17]),Q],S=[0,h(j[7],R),I],T=h(n[am],o[1]),U=h(l[66][8],T);return i(j[11],U,S)};return a(c,d)}var
w=i(o[17][12],v,q),x=h(j[19],w);return m(j[4],x,b,e)}];Q(150,Y,"Ground_plugin.Instances");var
aL=[0,function(a,b,c){var
d=[0,z[18][1]];function
l(a){try{var
b=h(bg[23],a),c=h(k[41],b)[1];d[1]=i(z[18][7],c,d[1]);var
e=0;return e}catch(f){f=s(f);if(f===k[28])return 0;throw f}}var
n=h(bg[26],0);i(bf[11],l,n);var
o=h(z[18][12],d[1]),v=i(I[8][11],I[12],[0,z[1][11][2],o]);p[2][1]=v;function
B(z,b,c){if(w.caml_equal(h(A[8],0),cY)){var
V=h(as[66],c);h(U[15],V)}try{var
I=h(f[13],b),l=I[2],n=I[1],e=function(a){return i(f[11],z,a)},X=0,k=function(a,b){return B(X,a,b)},d=function(a){return B([0,n,z],l,a)},C=n[3];if(0===C[0]){var
u=C[1];if(typeof
u==="number")var
v=h(r[11],n[1]);else
switch(u[0]){case
0:var
Z=e(l),v=H(r[9],u[1],d,n[1],k,Z);break;case
1:var
_=e(l),v=H(r[10],u[1],d,n[1],k,_);break;case
2:var
K=h(Y[1],b),L=K[1],$=K[2],aa=i(q[22],L,z),M=function(a){return B(aa,$,a)};if(p[1][1])if(0<b[8])var
ab=e(b),N=t(Y[3],L,M,k,ab),D=1;else
var
D=0;else
var
D=0;if(!D)var
N=M;var
v=N;break;case
3:if(p[1][1])var
ac=e(l),v=H(r[15],u[1],d,n[1],k,ac);else
var
v=d;break;default:var
o=u[2];if(typeof
o==="number")var
x=d;else
switch(o[0]){case
3:if(0<b[8])if(p[1][1])var
af=e(l),x=H(r[16],o[1],d,n[1],k,af),E=1;else
var
E=0;else
var
E=0;if(!E)var
x=d;break;case
4:if(p[1][1])var
ag=e(l),x=aT(r[12],o[1],o[2],d,n[1],k,ag);else
var
x=d;break;case
5:var
ah=e(l),x=bB(r[13],o[1],o[2],o[3],d,n[1],k,ah);break;default:var
ae=e(l),x=aT(r[12],o[1],o[2],d,n[1],k,ae)}var
ad=e(l),v=H(r[5],u[1],x,n[1],k,ad)}var
J=v}else{var
O=C[1];if(typeof
O==="number")switch(O){case
0:var
ai=e(l),y=m(r[8],d,k,ai);break;case
1:var
aj=e(l),y=m(r[6],d,k,aj);break;case
2:var
ak=e(l),y=m(r[7],d,k,ak);break;case
3:var
y=d;break;default:if(p[1][1])var
al=h(g[1],cZ),P=i(j[24],0,al);else
var
P=d;var
am=e(l),y=m(r[14],P,k,am)}else{var
Q=h(Y[1],b),R=Q[1],an=Q[2],ao=i(q[22],R,z),S=function(a){return B(ao,an,a)};if(p[1][1])if(0<b[8])var
ap=e(b),T=t(Y[3],R,S,k,ap),F=1;else
var
F=0;else
var
F=0;if(!F)var
T=S;var
y=T}var
J=y}var
G=J}catch(f){f=s(f);if(f!==aI[1])throw f;var
G=a}var
W=i(r[4],b[4],b);return m(j[4],W,G,c)}var
e=h(b,c),x=e[2],y=e[1],C=0;function
D(a,b){return B(C,a,b)}var
E=h(u[9],c),F=h(bf[1],E);return H(r[1],F,1,D,y,x)}];Q(155,aL,"Ground_plugin.Ground");h(aO[12],c6);var
ak=[0,3];function
c7(a){return a?(ak[1]=i(q[5],a[1],0),0):(ak[1]=3,0)}var
c_=[0,1,0,c9,c8,function(a){return[0,ak[1]]},c7];h(bk[3],c_);var
_=[0,aV];function
c$(a){return a?(_[1]=i(q[5],a[1],0),0):(_[1]=0,0)}var
dc=[0,1,0,db,da,function(a){return[0,_[1]]},c$];h(bk[3],dc);var
dd=[0,bq,0],de=[0,function(a,b){return t(bo[14],0,0,0,0)}];m(aM[9],0,bq,de);var
br=[31,G[4],dd,0],aQ=i(c0[1],[0,br],df),bs=aQ[3],aR=aQ[2],bt=aQ[1];try{var
fq=0,fs=[0,[0,0,function(a){if(a)if(!a[2]){var
b=a[1],c=h(d[4],v[14]),e=i(d[8],c,b);return function(a){var
b=h(bi[3],e),c=h(bj[10][2],0);return i(bt,h(bj[6],c),b)}}return h(q[2],fr)}],fq],ft=function(a,b){return m(bh[1],b[1],[0,fu,a],b[2])};i(Z[80],ft,fs);var
fv=0,fx=[0,function(a){if(a)if(!a[2])return function(a){return av[6]};return h(q[2],fw)},fv],fy=function(a,b){return i(av[3],[0,fz,a],b)};i(Z[80],fy,fx)}catch(f){f=s(f);if(!h(D[22],f))throw f;var
dg=i(D[18],0,f),dj=i(q[16],di,dh),dl=i(q[16],dk,dj),dm=h(g[1],dl),dn=i(g[13],dm,dg);h(U[13],dn)}var
dp=[6,h(F[12],v[14])],dq=h(d[4],v[14]),du=[0,[0,dt,[0,ds,[0,dr,[0,[1,G[4],dq,dp],0]]]],0];function
dv(a,b){return m(bm[1],[0,dw,a],0,b)}i(Z[80],dv,du);try{var
ff=0,fi=[0,[0,0,function(a){return a?h(q[2],fg):function(a){var
b=h(bs,0),c=h(g[1],fh),d=i(g[14],c,b);return h(U[11],d)}}],ff],fj=function(a,b){return m(bh[1],b[1],[0,fk,a],b[2])};i(Z[80],fj,fi);var
fl=0,fn=[0,function(a){return a?h(q[2],fm):function(a){return av[5]}},fl],fo=function(a,b){return i(av[3],[0,fp,a],b)};i(Z[80],fo,fn)}catch(f){f=s(f);if(!h(D[22],f))throw f;var
dx=i(D[18],0,f),dA=i(q[16],dz,dy),dC=i(q[16],dB,dA),dD=h(g[1],dC),dE=i(g[13],dD,dx);h(U[13],dE)}function
dG(a,b){return m(bm[1],[0,dH,a],0,b)}i(Z[80],dG,dF);var
dJ=h(g[1],dI),dK=i(j[24],0,dJ);function
$(a,b,c,d,e){var
g=p[1][1];try{p[1][1]=a;var
i=b?b[1]:h(aR,0)[2],j=function(a){var
e=h(f[14],ak[1]),b=m(f[15],c,e,a);return m(f[16],d,b[1],b[2])},k=h(l[66][8],i),n=m(aL[1],k,j,e);p[1][1]=g;return n}catch(f){f=s(f);p[1][1]=g;throw f}}function
bu(a,b,c,d){var
e=m(g[54],g[44],c3[41],d),f=h(g[1],dL);return i(g[14],f,e)}function
bv(a,b,c,d){function
e(a){return h(as[42],a[2])}var
f=h(a7[6],e),j=m(g[54],g[44],f,d),k=h(g[1],dM);return i(g[14],k,j)}function
bw(a,b,c,d){var
e=m(g[54],g[44],as[42],d),f=h(g[1],dN);return i(g[14],f,e)}var
B=h(d[2],dO);function
dP(a,b){var
c=h(d[17],v[18]),e=h(d[4],c),f=i(d[7],e,b),g=i(bi[10],a,f),j=h(d[17],v[18]),k=h(d[5],j);return[0,a,i(d[8],k,g)]}i(bl[5],B,dP);function
dQ(a,b){var
c=h(d[17],v[18]),e=h(d[5],c),f=i(d[7],e,b),g=i(c1[2],a,f),j=h(d[17],v[18]),k=h(d[5],j);return i(d[8],k,g)}i(bl[6],B,dQ);function
dR(a,b){var
c=h(d[17],v[18]),e=h(d[5],c),f=i(d[7],e,b);return i(A[9],a,f)}i(aP[6],B,dR);var
dS=h(d[17],v[18]),dT=h(d[6],dS),dU=[0,h(aP[2],dT)];i(aP[3],B,dU);var
dV=h(d[4],B),aS=m(F[13],F[9],dW,dV),dX=0,dY=0;function
dZ(a,b,c){return[0,a,0]}var
d0=[6,F[14][16]],d2=[0,[0,[0,[0,0,[0,h(aj[14],d1)]],d0],dZ],dY];function
d3(a,b,c,d,e){return[0,c,a]}var
d5=[0,h(aj[14],d4)],d6=[2,[6,F[14][16]],d5],d8=[0,h(aj[14],d7)],d9=[6,F[14][16]],d$=[0,[0,[0,[0,[0,[0,0,[0,h(aj[14],d_)]],d9],d8],d6],d3],d2];function
ea(a,b,c,d,e){var
f=h(g[1],eb);i(c4[52],U[13],f);return[0,c,[0,b,a]]}var
ec=[3,[6,F[14][16]]],ed=[6,F[14][16]],ee=[6,F[14][16]],eg=[0,[0,[0,[0,[0,[0,0,[0,h(aj[14],ef)]],ee],ed],ec],ea],d$],eh=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],eg]],dX]];m(F[23],aS,0,eh);t(c2[1],B,bu,bv,bw);var
ei=[0,aS,0];function
ej(a){var
b=a[2],c=h(d[4],B);return[0,i(d[7],c,b)]}m(aN[5],ek,ej,ei);try{var
eN=0,eP=[0,function(a){if(a){var
b=a[2];if(b){var
c=b[2];if(c)if(!c[2]){var
e=a[1],f=h(d[18],v[14]),g=h(d[6],f),j=i(A[2][7],g,e),k=b[1],m=h(d[6],B),n=i(A[2][7],m,k),o=c[1],p=h(d[17],aw[7]),r=h(d[6],p),s=i(A[2][7],r,o);return function(a){var
b=h(A[19],a),c=i(ag[15],b,j),d=1;function
e(a){return $(d,c,n,s,a)}return h(l[66][1],e)}}}}return h(q[2],eO)},eN],eR=[0,function(a){if(a){var
b=a[2];if(b)if(!b[2]){var
c=a[1],e=h(d[18],v[14]),f=h(d[6],e),g=i(A[2][7],f,c),j=b[1],k=h(d[17],aw[7]),m=h(d[6],k),n=i(A[2][7],m,j);return function(a){var
c=h(A[19],a),b=0,d=i(ag[15],c,g),e=1;function
f(a){return $(e,d,b,n,a)}return h(l[66][1],f)}}}return h(q[2],eQ)},eP],eT=[0,function(a){if(a){var
b=a[2];if(b)if(!b[2]){var
c=a[1],e=h(d[18],v[14]),f=h(d[6],e),g=i(A[2][7],f,c),j=b[1],k=h(d[6],B),m=i(A[2][7],k,j);return function(a){var
c=h(A[19],a),b=0,d=i(ag[15],c,g),e=1;function
f(a){return $(e,d,m,b,a)}return h(l[66][1],f)}}}return h(q[2],eS)},eR],eU=h(bp[12],eT);m(aM[9],0,[0,V,eV],eU);var
eW=function(a){var
k=h(z[1][6],eX),b=aw[7],g=0,j=0;if(0===b[0]){var
l=[0,eY,[0,[1,G[4],[0,[5,[0,b[1]]]],k],j]],m=h(z[1][6],eZ);if(0===B[0]){var
n=[0,[1,G[4],[5,[0,B[1]]],m],l],o=h(z[1][6],e0),c=v[14];if(0===c[0]){var
p=[0,[0,e1,[0,[1,G[4],[4,[5,[0,c[1]]]],o],n]],g],r=h(z[1][6],e2),d=aw[7],q=0;if(0===d[0]){var
s=[0,e3,[0,[1,G[4],[0,[5,[0,d[1]]]],r],q]],t=h(z[1][6],e4),e=v[14];if(0===e[0]){var
u=[0,[0,e5,[0,[1,G[4],[4,[5,[0,e[1]]]],t],s]],p],w=0,x=h(z[1][6],e6);if(0===B[0]){var
y=[0,[1,G[4],[5,[0,B[1]]],x],w],A=h(z[1][6],e7),f=v[14];if(0===f[0])return i(aN[4],[0,V,e9],[0,[0,e8,[0,[1,G[4],[4,[5,[0,f[1]]]],A],y]],u]);throw[0,J,e_]}throw[0,J,e$]}throw[0,J,fa]}throw[0,J,fb]}throw[0,J,fc]}throw[0,J,fd]}throw[0,J,fe]};i(aO[19],eW,V)}catch(f){f=s(f);if(!h(D[22],f))throw f;var
el=i(D[18],0,f),eo=i(q[16],en,em),eq=i(q[16],ep,eo),er=h(g[1],eq),es=i(g[13],er,el);h(U[13],es)}try{var
eD=0,eF=[0,function(a){if(a)if(!a[2]){var
b=a[1],c=h(d[18],v[14]),e=h(d[6],c),f=i(A[2][7],e,b);return function(a){var
d=h(A[19],a),b=0,c=0,e=i(ag[15],d,f),g=0;function
j(a){return $(g,e,c,b,a)}return h(l[66][1],j)}}return h(q[2],eE)},eD],eG=h(bp[12],eF);m(aM[9],0,[0,V,eH],eG);var
eI=function(a){var
e=h(z[1][6],eJ),b=v[14],c=0,d=0;if(0===b[0])return i(aN[4],[0,V,eL],[0,[0,eK,[0,[1,G[4],[4,[5,[0,b[1]]]],e],d]],c]);throw[0,J,eM]};i(aO[19],eI,V)}catch(f){f=s(f);if(!h(D[22],f))throw f;var
et=i(D[18],0,f),ew=i(q[16],ev,eu),ey=i(q[16],ex,ew),ez=h(g[1],ey),eA=i(g[13],ez,et);h(U[13],eA)}function
eB(a){var
d=i(bn[3][4],_[1],0),e=h(aR,0)[2],b=0,c=0,f=[0,i(j[70][3],e,d)],g=1;function
k(a){return $(g,f,c,b,a)}var
n=h(l[66][1],k),o=i(bn[3][4],_[1],0),p=m(bo[18],0,0,0),q=i(j[70][12],p,o);return i(j[70][12],q,n)}var
eC=h(l[13],0),bx=i(l[67][1],eC,eB);h(c5[3][3],bx);var
by=[0,V,ak,_,br,bt,aR,bs,dK,$,bu,bv,bw,B,aS,bx];Q(183,by,"Ground_plugin.G_ground");Q(184,[0,p,ar,f,r,Y,aL,by],"Ground_plugin");return}(function(){return this}()));
