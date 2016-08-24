(function(a){"use strict";var
s="String",o=250,i=246,F="string",E="Strings",D="Coq",f=a.jsoo_runtime,b=f.caml_new_string,n=f.caml_obj_tag,C=f.caml_register_global,P=f.caml_wrap_exception;function
g(a,b){return a.length==1?a(b):f.caml_call_gen(a,[b])}function
h(a,b,c){return a.length==2?a(b,c):f.caml_call_gen(a,[b,c])}function
O(a,b,c,d){return a.length==3?a(b,c,d):f.caml_call_gen(a,[b,c,d])}function
Q(a,b,c,d,e){return a.length==4?a(b,c,d,e):f.caml_call_gen(a,[b,c,d,e])}var
e=f.caml_get_global_data(),v=b("string_syntax_plugin"),m=[0,b(D),[0,b(E),[0,b(s),0]]],j=e.CamlinternalLazy,u=e.Globnames,p=e.Buffer,k=e.Ascii_syntax_plugin,t=e.Loc,I=e.Char,H=e.Coqlib,G=e.Notation;g(e.Mltop[12],v);var
l=[248,b("String_syntax.Non_closed_string"),f.caml_fresh_oo_id(0)],w=h(k[1][5],m,b(F)),q=h(k[1][4],m,b(F)),x=[3,[0,[0,q,0],1]],y=[3,[0,[0,q,0],2]],L=b("EmptyString"),K=b(s),J=b("String interpretation"),M=[0,b(D),[0,b(E),[0,b(s),0]]],N=b("string_scope");function
r(a){return O(H[1],J,m,a)}var
c=[i,function(a){return r(K)}],d=[i,function(a){return r(L)}];function
z(e,b){var
q=f.caml_ml_string_length(b);function
l(a){if(a===q){var
m=n(d),r=0,s=o===m?d[1]:i===m?g(j[2],d):d;return[0,[0,e,s,r]]}var
t=[0,l(a+1|0),0],p=n(c),u=[0,h(k[1][13],e,f.caml_string_get(b,a)),t],v=0,w=o===p?c[1]:i===p?g(j[2],c):c;return[4,e,[0,[0,e,w,v]],u]}return l(0)}function
A(a){try{var
m=g(p[1],16),b=function(a){var
b=a;for(;;){switch(b[0]){case
0:var
q=n(d),v=o===q?d[1]:i===q?g(j[2],d):d;if(h(u[5],b[1][2],v))return[0,g(p[2],m)];break;case
4:var
r=b[2];if(0===r[0]){var
e=b[3];if(e){var
f=e[2];if(f)if(!f[2]){var
s=n(c),w=o===s?c[1]:i===s?g(j[2],c):c;if(h(u[5],r[1][2],w)){var
t=g(k[1][15],e[1]);if(t){var
x=g(I[1],t[1]);h(p[10],m,x);var
b=f[1];continue}throw l}}}}break}throw l}}(a);return b}catch(f){f=P(f);if(f===l)return 0;throw f}}Q(G[14],N,[0,w,M],z,[0,[0,[0,[0,t[4],y,0]],[0,[0,[0,t[4],x,0]],0]],A,1]);var
B=[0,v,l,m,w,q,x,y,r,c,d,z,A];C(19,B,"String_syntax_plugin.String_syntax");C(20,[0,B],"String_syntax_plugin");return}(function(){return this}()));
