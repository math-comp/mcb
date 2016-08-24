(function(a){"use strict";var
f=a.jsoo_runtime,d=f.caml_new_string,s=f.caml_register_global,E=f.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):f.caml_call_gen(a,[b])}function
i(a,b,c){return a.length==2?a(b,c):f.caml_call_gen(a,[b,c])}function
F(a,b,c,d,e){return a.length==4?a(b,c,d,e):f.caml_call_gen(a,[b,c,d,e])}var
b=f.caml_get_global_data(),n=d("nat_syntax_plugin"),g=b.Coqlib,m=b.Globnames,h=b.Bigint,e=b.Pp,l=b.Loc,u=b.Feedback,v=b.Errors,t=b.Notation;c(b.Mltop[12],n);var
o=c(h[3],5e3),w=d("limits and on the command executed)."),x=d("may vary from 5000 to 70000 depending on your system "),y=d("working with large numbers in nat (observed threshold "),z=d("Stack overflow or segmentation fault happens when "),A=d("Cannot interpret a negative number as a number of type nat"),B=d("nat_of_int"),C=d("Nat_syntax.Non_closed_number"),D=d("nat_scope");function
p(a,b){if(c(h[20],b)){if(i(h[16],o,b)){var
k=c(e[26],w),l=c(e[26],x),m=c(e[26],y),n=c(e[26],z),p=i(e[14],n,m),q=i(e[14],p,l),r=i(e[14],q,k);c(u[13],r)}var
j=[0,[0,a,g[23],0]],d=b,s=[0,[0,a,g[24],0]];for(;;){if(f.caml_notequal(d,h[5])){var
j=[4,a,s,[0,j,0]],d=c(h[10],d);continue}return j}}var
t=[0,a,B,c(e[1],A)];return c(v[8],t)}var
j=[248,C,f.caml_fresh_oo_id(0)];function
k(a){switch(a[0]){case
0:if(i(m[5],a[1][2],g[23]))return h[5];break;case
4:var
d=a[2];if(0===d[0]){var
b=a[3];if(b)if(!b[2])if(i(m[5],d[1][2],g[24])){var
e=k(b[1]);return c(h[9],e)}}break}throw j}function
q(a){try{var
b=[0,k(a)];return b}catch(f){f=E(f);if(f===j)return 0;throw f}}F(t[13],D,[0,g[19],g[18]],p,[0,[0,[0,[0,l[4],g[24],0]],[0,[0,[0,l[4],g[23],0]],0]],q,1]);var
r=[0,n,o,p,j,k,q];s(18,r,"Nat_syntax_plugin.Nat_syntax");s(19,[0,r],"Nat_syntax_plugin");return}(function(){return this}()));
