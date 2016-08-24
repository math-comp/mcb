(function(a){"use strict";var
ag="Z",af="N",ae="positive",j=a.jsoo_runtime,b=j.caml_new_string,ad=j.caml_register_global,D=j.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):j.caml_call_gen(a,[b])}function
e(a,b,c){return a.length==2?a(b,c):j.caml_call_gen(a,[b,c])}function
E(a,b,c,d,e){return a.length==4?a(b,c,d,e):j.caml_call_gen(a,[b,c,d,e])}var
g=j.caml_get_global_data(),H=b("z_syntax_plugin"),f=[0,b("Coq"),[0,b("Numbers"),[0,b("BinNums"),0]]],h=g.Globnames,d=g.Bigint,F=g.Pp,G=g.Errors,m=g.Names,C=g.Notation,i=g.Loc,ai=g.Libnames,ah=g.Util;c(g.Mltop[12],H);var
k=[248,b("Z_syntax.Non_closed_number"),j.caml_fresh_oo_id(0)],au=b('No negative numbers in type "N".'),av=b("interp_N"),an=b('Only strictly positive numbers in type "positive".'),ao=b("interp_positive"),aj=b(ae),ak=b(ae),ap=b("positive_scope"),aq=b(af),at=b(af),aw=b("N_scope"),ax=b(ag),ay=b(ag),aB=b("Z_scope");function
n(a){var
b=e(ah[17][14],m[1][5],a);return c(m[5][4],b)}function
q(a,b){var
d=c(m[1][5],b),f=n(a);return e(ai[17],f,d)}var
I=q(f,aj);function
r(a,b){return e(h[25],a,b)}var
al=c(m[1][5],ak),o=r(n(f),al),J=[0,[0,o,0],1],K=[0,[0,o,0],2],L=[0,[0,o,0],3],s=[3,J],t=[3,K],u=[3,L],am=[2,[0,o,0]];function
v(f,b){var
i=[0,[0,f,s,0]],j=[0,[0,f,u,0]],k=[0,[0,f,t,0]];function
g(a){var
h=c(d[8],a),b=h[1];return 0===h[2]?[4,f,k,[0,g(b),0]]:e(d[17],b,d[5])?j:[4,f,i,[0,g(b),0]]}return g(b)}function
M(a){var
b=[0,a,ao,c(F[1],an)];return c(G[8],b)}function
N(a,b){return c(d[18],b)?v(a,b):M(a)}function
l(a){switch(a[0]){case
0:if(e(h[5],a[1][2],u))return d[6];break;case
4:var
f=a[2];if(0===f[0]){var
b=a[3];if(b)if(!b[2]){var
g=b[1],i=f[1][2];if(e(h[5],i,t)){var
j=l(g);return c(d[11],j)}if(e(h[5],i,s)){var
m=l(g),n=c(d[11],m);return c(d[9],n)}}}break}throw k}function
O(a){try{var
b=[0,l(a)];return b}catch(f){f=D(f);if(f===k)return 0;throw f}}E(C[13],ap,[0,I,f],N,[0,[0,[0,[0,i[4],s,0]],[0,[0,[0,i[4],t,0]],[0,[0,[0,i[4],u,0]],0]]],O,1]);var
ar=c(m[1][5],aq),w=r(n(f),ar),P=[0,[0,w,0],1],Q=[0,[0,w,0],2],x=[3,P],y=[3,Q],R=q(f,at),as=[2,[0,w,0]];function
S(a,b,c){return e(d[17],c,d[5])?[0,[0,a,x,0]]:[4,a,[0,[0,a,y,0]],[0,v(a,c),0]]}function
T(a){var
b=[0,a,av,c(F[1],au)];return c(G[8],b)}function
U(a,b){return c(d[20],b)?S(a,1,b):T(a)}function
V(a){switch(a[0]){case
0:if(e(h[5],a[1][2],x))return d[5];break;case
4:var
c=a[2];if(0===c[0]){var
b=a[3];if(b)if(!b[2])if(e(h[5],c[1][2],y))return l(b[1])}break}throw k}function
W(a){try{var
b=[0,V(a)];return b}catch(f){f=D(f);if(f===k)return 0;throw f}}E(C[13],aw,[0,R,f],U,[0,[0,[0,[0,i[4],x,0]],[0,[0,[0,i[4],y,0]],0]],W,1]);var
X=q(f,ax),az=c(m[1][5],ay),p=r(n(f),az),Y=[0,[0,p,0],1],Z=[0,[0,p,0],2],_=[0,[0,p,0],3],z=[3,Y],A=[3,Z],B=[3,_],aA=[2,[0,p,0]];function
$(a,b){if(e(d[17],b,d[5]))return[0,[0,a,z,0]];if(c(d[20],b))var
g=A,f=b;else
var
g=B,f=c(d[22],b);return[4,a,[0,[0,a,g,0]],[0,v(a,f),0]]}function
aa(a){switch(a[0]){case
0:if(e(h[5],a[1][2],z))return d[5];break;case
4:var
f=a[2];if(0===f[0]){var
b=a[3];if(b)if(!b[2]){var
g=b[1],i=f[1][2];if(e(h[5],i,A))return l(g);if(e(h[5],i,B)){var
j=l(g);return c(d[22],j)}}}break}throw k}function
ab(a){try{var
b=[0,aa(a)];return b}catch(f){f=D(f);if(f===k)return 0;throw f}}E(C[13],aB,[0,X,f],$,[0,[0,[0,[0,i[4],z,0]],[0,[0,[0,i[4],A,0]],[0,[0,[0,i[4],B,0]],0]]],ab,1]);var
ac=[0,H,k,f,n,q,I,r,o,am,J,K,L,s,t,u,v,M,N,l,O,w,as,P,Q,x,y,R,S,T,U,V,W,X,p,aA,Y,Z,_,z,A,B,$,aa,ab];ad(26,ac,"Z_syntax_plugin.Z_syntax");ad(27,[0,ac],"Z_syntax_plugin");return}(function(){return this}()));
