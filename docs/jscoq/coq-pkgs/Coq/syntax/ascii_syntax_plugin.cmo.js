(function(a){"use strict";var
K=250,J="Ascii",p=246,I="ascii",f=a.jsoo_runtime,G=f.caml_ml_string_length,c=f.caml_new_string,F=f.caml_obj_tag,H=f.caml_register_global,n=f.caml_string_get,aa=f.caml_wrap_exception;function
d(a,b){return a.length==1?a(b):f.caml_call_gen(a,[b])}function
g(a,b,c){return a.length==2?a(b,c):f.caml_call_gen(a,[b,c])}function
$(a,b,c,d){return a.length==3?a(b,c,d):f.caml_call_gen(a,[b,c,d])}function
ab(a,b,c,d,e){return a.length==4?a(b,c,d,e):f.caml_call_gen(a,[b,c,d,e])}var
b=f.caml_get_global_data(),r=c("ascii_syntax_plugin"),j=[0,c("Coq"),[0,c("Strings"),[0,c(J),0]]],h=b.Util,i=b.Coqlib,l=b.Globnames,q=b.CamlinternalLazy,k=b.Names,O=b.Option,N=b.Pervasives,L=b.Printf,M=b.Pp,S=b.Errors,R=b.Libnames,P=b.Notation,Q=b.Loc;d(b.Mltop[12],r);var
m=[248,c("Ascii_syntax.Non_closed_ascii"),f.caml_fresh_oo_id(0)],Z=[0,[4,0,[0,2,3],0,0],c("%03d")],X=c("Expects a single character or a three-digits ascii code."),Y=c("interp_ascii_string"),W=c(J),V=c("Ascii interpretation"),T=c(I),U=c(I),_=c("char_scope");function
o(a){var
b=g(h[17][14],k[1][5],a);return d(k[5][4],b)}function
s(a,b){var
c=d(k[1][5],b),e=o(a);return g(l[25],e,c)}function
t(a,b){var
c=d(k[1][5],b),e=o(a);return g(R[17],e,c)}var
u=t(j,T),v=s(j,U),w=[0,[0,v,0],1],x=[3,w];function
y(a){return $(i[1],V,j,a)}var
e=[p,function(a){return y(W)}];function
z(c,b){function
f(a,b){if(0===a)return 0;var
d=f(a-1|0,b/2|0),e=0,g=0===(b%2|0)?i[29]:i[28];return[0,[0,[0,c,g,e]],d]}var
a=F(e),g=f(8,b),h=0,j=K===a?e[1]:p===a?d(q[2],e):e;return[4,c,[0,[0,c,j,h]],g]}function
A(a,b){if(1===G(b))var
e=n(b,0);else{if(3===G(b))if(d(h[11],n(b,0)))if(d(h[11],n(b,1)))if(d(h[11],n(b,2)))var
e=f.caml_int_of_string(b),c=1;else
var
c=0;else
var
c=0;else
var
c=0;else
var
c=0;if(!c)var
g=[0,a,Y,d(M[1],X)],e=d(S[8],g)}return z(a,e)}function
B(a){function
c(a,b){if(b){var
d=b[1];if(0===d[0]){var
e=b[2],f=d[1][2];if(g(l[5],f,i[28]))return 1+(2*c(a-1|0,e)|0)|0;if(g(l[5],f,i[29]))return 2*c(a-1|0,e)|0}}else
if(0===a)return 0;throw m}try{var
b=[0,function(a){if(4===a[0]){var
b=a[2];if(0===b[0]){var
f=F(e),h=K===f?e[1]:p===f?d(q[2],e):e;if(g(l[5],b[1][2],h))return c(8,a[3])}}throw m}(a)];return b}catch(f){f=aa(f);if(f===m)return 0;throw f}}function
C(a){if(32<=a)if(!(126<a)){var
b=d(N[17],a);return g(h[15][1],1,b)}return g(L[4],Z,a)}function
D(a){var
b=B(a);return g(O[15],C,b)}ab(P[14],_,[0,u,j],A,[0,[0,[0,[0,Q[4],x,0]],0],D,1]);var
E=[0,r,m,o,s,t,j,u,v,w,x,y,e,z,A,B,C,D];H(25,E,"Ascii_syntax_plugin.Ascii_syntax");H(26,[0,E],"Ascii_syntax_plugin");return}(function(){return this}()));
