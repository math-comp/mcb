(function(a){"use strict";var
M="Rdefinitions",K="R",L="Reals",J="Coq",k=a.jsoo_runtime,b=k.caml_new_string,I=k.caml_register_global,ab=k.caml_wrap_exception;function
f(a,b){return a.length==1?a(b):k.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):k.caml_call_gen(a,[b,c])}function
ac(a,b,c,d,e){return a.length==4?a(b,c,d,e):k.caml_call_gen(a,[b,c,d,e])}var
j=k.caml_get_global_data(),x=b("r_syntax_plugin"),d=j.Bigint,e=j.Globnames,q=j.Names,w=j.Util,O=j.Loc,P=j.Libnames,N=j.Notation;f(j.Mltop[12],x);var
n=[248,b("R_syntax.Non_closed_number"),k.caml_fresh_oo_id(0)],Q=[0,b(J),[0,b(L),[0,b(M),0]]],S=b(K),T=b(K),V=b("R1"),W=b("R0"),X=b("Ropp"),Y=b("Rplus"),Z=b("Rmult"),$=[0,b(J),[0,b(L),[0,b(M),0]]],aa=b("R_scope");function
y(a){var
b=c(w[17][14],q[1][5],a);return f(q[5][4],b)}var
h=y(Q),R=f(q[1][5],S),z=c(P[17],h,R);function
l(a,b){var
d=f(q[1][5],b);return c(e[27],a,d)}var
A=l(h,T),g=[1,l(h,V)],r=[1,l(h,W)],s=[1,l(h,X)],i=[1,l(h,Y)],o=[1,l(h,Z)],m=f(d[11],d[6]),B=f(d[9],m),C=f(d[11],m),U=[1,A];function
t(a,b){return c(d[17],d[6],b)?[0,[0,a,g,0]]:[4,a,[0,[0,a,i,0]],[0,[0,[0,a,g,0]],[0,t(a,f(d[10],b)),0]]]}function
u(e,b){var
j=[0,[0,e,g,0]],k=t(e,m);function
h(a){if(c(d[16],a,C))return t(e,a);var
b=f(d[8],a),g=[4,e,[0,[0,e,o,0]],[0,k,[0,h(b[1]),0]]];return b[2]?[4,e,[0,[0,e,i,0]],[0,j,[0,g,0]]]:g}return c(d[17],b,d[5])?[0,[0,e,r,0]]:h(b)}function
D(a,b){return f(d[19],b)?[4,a,[0,[0,a,s,0]],[0,u(a,f(d[22],b)),0]]:u(a,b)}function
p(a){if(4===a[0]){var
x=a[2];if(0===x[0]){var
h=a[3];if(h){var
j=h[1],k=x[1][2];if(0===j[0]){var
l=h[2];if(l){var
q=l[1],y=j[1][2];switch(q[0]){case
0:if(l[2])var
b=1;else{if(c(e[5],k,i))if(c(e[5],y,g))if(c(e[5],q[1][2],g))return m;var
b=0}break;case
4:var
A=q[2];if(0===A[0]){var
v=q[3];if(v){var
C=v[1];if(0===C[0]){var
w=v[2];if(w){var
D=w[1];if(0===D[0])if(w[2])var
b=0;else
if(l[2])var
b=1;else{if(c(e[5],k,i))if(c(e[5],A[1][2],i))if(c(e[5],y,g))if(c(e[5],C[1][2],g))if(c(e[5],D[1][2],g))return B;var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0;break;default:var
b=0}}else
var
b=1}else
var
b=0;if(!b){var
r=h[2];if(r)if(!r[2]){if(c(e[5],k,o)){var
E=p(j);if(1-c(d[17],E,m))throw n;var
F=p(r[1]);return f(d[11],F)}if(0===j[0]){var
s=h[2][1];if(4===s[0]){var
z=s[2];if(0===z[0]){var
t=s[3];if(t){var
u=t[2];if(u)if(!u[2])if(c(e[5],k,i))if(c(e[5],z[1][2],o))if(c(e[5],j[1][2],g)){var
G=p(t[1]);if(1-c(d[17],G,m))throw n;var
H=p(u[1]),I=f(d[11],H);return f(d[9],I)}}}}}}}}}}throw n}function
v(a){if(0===a[0]){var
b=a[1][2];if(c(e[5],b,r))return d[5];if(c(e[5],b,g))return d[6]}return p(a)}function
E(a){if(4===a[0]){var
g=a[2];if(0===g[0]){var
b=a[3];if(b)if(!b[2])if(c(e[5],g[1][2],s)){var
h=v(b[1]);if(c(d[17],h,d[5]))throw n;return f(d[22],h)}}}return v(a)}function
F(a){try{var
b=[0,E(a)];return b}catch(f){f=ab(f);if(f===n)return 0;throw f}}function
G(a){return[0,[0,O[4],a,0]]}var
_=[0,c(w[17][12],G,[0,s,[0,r,[0,i,[0,o,[0,g,0]]]]]),F,0];ac(N[13],aa,[0,z,$],D,_);var
H=[0,x,n,y,h,z,l,A,U,g,r,s,i,o,m,B,C,t,u,D,v,E,F,G];I(20,H,"R_syntax_plugin.R_syntax");I(21,[0,H],"R_syntax_plugin");return}(function(){return this}()));
