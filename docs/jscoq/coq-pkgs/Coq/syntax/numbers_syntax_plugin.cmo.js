(function(a){"use strict";var
aa="zn2z",I="BigN",aK="Cyclic",o="Numbers",aJ="t_",aI="int31",aH="digits",H="BigZ",G="BigQ",$="t",aG="Int31",n="Coq",i=a.jsoo_runtime,b=i.caml_new_string,aF=i.caml_register_global,E=i.caml_wrap_exception;function
f(a,b){return a.length==1?a(b):i.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):i.caml_call_gen(a,[b,c])}function
F(a,b,c,d,e){return a.length==4?a(b,c,d,e):i.caml_call_gen(a,[b,c,d,e])}var
c=i.caml_get_global_data(),ae=b("numbers_syntax_plugin"),s=[0,b(n),[0,b(o),[0,b(aK),[0,b(aG),[0,b(aG),0]]]]],ag=b("int31_scope"),N=[0,b(n),[0,b(o),[0,b(aK),[0,b("DoubleCyclic"),[0,b("DoubleType"),0]]]]],w=[0,b(n),[0,b(o),[0,b("Natural"),[0,b(I),[0,b(I),0]]]]],aj=b("bigN_scope"),x=[0,b(n),[0,b(o),[0,b("Integer"),[0,b(H),[0,b(H),0]]]]],am=b("bigZ_scope"),A=[0,b(n),[0,b(o),[0,b("Rational"),[0,b(G),[0,b(G),0]]]]],ap=b("bigQ_scope"),h=c.Globnames,e=c.Bigint,m=c.Loc,l=c.Pervasives,ab=c.Pp,ad=c.Errors,ac=c.List,j=c.Names,p=c.Notation,aL=c.Nat_syntax_plugin,aM=c.Libnames;f(c.Mltop[12],ae);var
a_=b("bigN are only non-negative numbers."),a$=b("interp_bigN"),a8=b("int31 are only non-negative numbers."),a9=b("interp_int31"),aN=b(aI),aO=b(aI),aP=b(aH),aQ=b(aH),aR=b(aa),aT=b(aa),aU=b(aa),aV=b($),aW=[0,b(I),0],aX=b("t'"),aY=b(I),aZ=b($),a0=[0,b(H),0],a1=b(aJ),a2=b(H),a3=b($),a4=[0,b(G),0],a5=b(aJ),a6=b(G),a7=b("Numbers_syntax.Non_closed");function
q(a){var
b=d(ac[15],j[1][5],a);return f(j[5][4],b)}function
k(a,b){var
c=f(j[1][5],b),e=q(a);return d(aM[17],e,c)}function
J(a,b){var
c=f(j[6][4],b);return d(j[23][3],a,c)}function
K(a,b){return J([0,q(a)],b)}function
r(a,b,c){var
d=f(j[6][4],b);return J([2,[0,q(a)],d],c)}var
af=k(s,aN);function
t(a){return K(s,a)}var
u=[3,[0,[0,t(aO),0],1]],L=[3,[0,[0,t(aP),0],1]],M=[3,[0,[0,t(aQ),0],2]],aS=k(N,aR);function
O(a){return K(N,a)}var
P=[3,[0,[0,O(aT),0],1]],v=[3,[0,[0,O(aU),0],2]],ah=k(d(l[22],w,aW),aV),ai=r(w,aY,aX),ak=7;function
Q(a){return[3,[0,[0,ai,0],d(l[4],a,ak)+1|0]]}var
al=k(d(l[22],x,a0),aZ),R=r(x,a2,a1),y=[3,[0,[0,R,0],1]],z=[3,[0,[0,R,0],2]],an=k(d(l[22],A,a4),a3),ao=r(A,a6,a5),B=[3,[0,[0,ao,0],1]],g=[248,a7,i.caml_fresh_oo_id(0)];function
S(a,b){var
d=[0,[0,a,u,0]],g=[0,[0,a,L,0]],h=[0,[0,a,M,0]];function
c(a,b){if(0<a){var
d=f(e[8],b),i=c(a-1|0,d[1]),j=d[2]?h:g;return[0,j,i]}return 0}var
i=c(31,b);return[4,a,d,f(ac[6],i)]}function
aq(a){var
b=[0,a,a9,f(ab[1],a8)];return f(ad[8],b)}function
ar(a,b){return f(e[20],b)?S(a,b):aq(a)}function
T(a){if(4===a[0]){var
l=a[2];if(0===l[0])if(d(h[5],l[1][2],u)){var
c=a[3],b=e[5];for(;;){if(c){var
i=c[1];if(0===i[0]){var
j=c[2],k=i[1][2];if(d(h[5],k,L)){var
c=j,b=f(e[11],b);continue}if(d(h[5],k,M)){var
m=f(e[11],b),c=j,b=f(e[9],m);continue}}throw g}return b}}}throw g}function
as(a){try{var
b=[0,T(a)];return b}catch(f){f=E(f);if(f===g)return 0;throw f}}F(p[13],ag,[0,af,s],ar,[0,[0,[0,[0,m[4],u,0]],0],as,1]);var
U=d(e[23],e[7],31);function
V(a){var
c=a,b=U;for(;;){if(0<c){var
c=c-1|0,b=d(e[14],b,b);continue}return b}}function
at(a,b){var
c=V(a-1|0);return d(e[15],b,c)}function
au(a){var
c=0,b=U;for(;;){if(d(e[16],a,b))return c;var
c=c+1|0,b=d(e[14],b,b);continue}}function
av(f,b,c){var
h=[0,[0,f,P,0]],i=[0,[0,f,v,0]];function
g(a,b){if(0<a){if(d(e[17],b,e[5]))return[4,f,h,[0,[13,[0,f,0,0,0]],0]];var
c=at(a,b),j=[0,g(a-1|0,c[2]),0];return[4,f,i,[0,[13,[0,f,0,0,0]],[0,g(a-1|0,c[1]),j]]]}return S(f,b)}return g(b,c)}function
C(a,b){var
c=au(b),i=[0,[0,a,Q(c),0]],g=av(a,c,b);if(c<7)var
h=[0,g,0];else
var
j=f(e[3],c-7|0),h=[0,d(aL[1][3],a,j),[0,g,0]];return[4,a,i,h]}function
aw(a){var
b=[0,a,a$,f(ab[1],a_)];return f(ad[8],b)}function
ax(a,b){return f(e[20],b)?C(a,b):aw(a)}function
W(a){if(4===a[0]){var
e=a[2];if(0===e[0]){var
f=a[3];if(f){var
b=f[2];if(b){var
c=b[2];if(c)if(!c[2])if(d(h[5],e[1][2],v)){var
g=W(c[1]),i=W(b[1]);return 1+d(l[5],i,g)|0}}}}}return 0}function
X(a,b){if(4===b[0]){var
i=b[2];if(0===i[0]){var
j=b[3],k=i[1][2];if(d(h[5],k,P))return e[5];if(j){var
c=j[2];if(c){var
f=c[2];if(f)if(!f[2])if(d(h[5],k,v)){var
g=a-1|0,l=X(g,f[1]),m=X(g,c[1]),n=V(g),o=d(e[14],n,m);return d(e[12],o,l)}}}}}return T(b)}function
Y(a){return X(W(a),a)}function
D(a){if(4===a[0]){var
b=a[3];if(b){var
c=b[2];if(!c)return Y(b[1]);if(!c[2])return Y(c[1])}}throw g}function
ay(a){try{var
b=[0,D(a)];return b}catch(f){f=E(f);if(f===g)return 0;throw f}}function
az(a){var
b=a<8?1:0;if(b)var
d=az(a+1|0),e=Q(a),c=[0,[0,[0,m[4],e,0]],d];else
var
c=b;return c}var
aA=az(0);F(p[13],aj,[0,ah,w],ax,[0,aA,ay,1]);function
Z(a,b){var
c=[0,[0,a,y,0]],d=[0,[0,a,z,0]];return f(e[20],b)?[4,a,c,[0,C(a,b),0]]:[4,a,d,[0,C(a,f(e[22],b)),0]]}function
_(a){if(4===a[0]){var
c=a[2];if(0===c[0]){var
b=a[3];if(b)if(!b[2]){var
i=b[1],j=c[1][2];if(d(h[5],j,y))return D(i);if(d(h[5],j,z)){var
k=D(i);if(d(e[17],k,e[5]))throw g;return f(e[22],k)}}}}throw g}function
aB(a){try{var
b=[0,_(a)];return b}catch(f){f=E(f);if(f===g)return 0;throw f}}F(p[13],am,[0,al,x],Z,[0,[0,[0,[0,m[4],y,0]],[0,[0,[0,m[4],z,0]],0]],aB,1]);function
aC(a,b){return[4,a,[0,[0,a,B,0]],[0,Z(a,b),0]]}function
aD(a){try{if(4===a[0]){var
f=a[2];if(0===f[0]){var
c=a[3];if(c)if(c[2])var
b=0;else
if(d(h[5],f[1][2],B))var
e=[0,_(c[1])],b=1;else
var
b=0;else
var
b=0}else
var
b=0}else
var
b=0;if(!b)var
e=0;return e}catch(f){f=E(f);if(f===g)return 0;throw f}}F(p[13],ap,[0,an,A],aC,[0,[0,[0,[0,m[4],B,0]],0],aD,1]);var
aE=[0,ae,q,k,J,K,r,s,af,t,ag,u,L,M,N,aS,O,P,v,w,ah,ai,aj,ak,Q,x,al,R,am,y,z,A,an,ao,ap,B,g,S,aq,ar,T,as,U,V,at,au,av,C,aw,ax,Y,D,ay,aA,Z,_,aB,aC,aD];aF(46,aE,"Numbers_syntax_plugin.Numbers_syntax");aF(47,[0,aE],"Numbers_syntax_plugin");return}(function(){return this}()));
