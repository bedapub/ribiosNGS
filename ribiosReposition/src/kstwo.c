#include <math.h>
#include <R.h>
#include <Rinternals.h>

// ks: x and y MUST BE PRESORTED
void ks(double data1[], unsigned long n1,
	double data2[], unsigned long n2,
	double *d) {
  unsigned long j1=1, j2=1;
  float d1, d2, dt, en1, en2, fn1=0.0, fn2=0.0;
  en1=n1;
  en2=n2;
  *d=0;
  while(j1<=n1 && j2<=n2) {
    if ((d1=data1[j1]) <= (d2=data2[j2])) fn1=j1++/en1;
    if(d2 <= d1) fn2=j2++/en2;
    if ((dt=fabs(fn2 - fn1)) > *d) *d=dt;
  }
}

SEXP kssorted(SEXP x, SEXP y) {
  R_len_t n1=length(x), n2=length(y);

  SEXP ans;
  PROTECT(ans=allocVector(REALSXP, 1));
  double *pans=REAL(ans);
  
  // sort input
  double *px=REAL(x), *py=REAL(y);
  px--; py--;
  ks(px, n1, py, n2, pans);

  UNPROTECT(1);
  return(ans); 
}

SEXP ckscore(SEXP n, SEXP vec) {
  R_len_t vect=length(vec);
  SEXP ans;
  PROTECT(ans=allocVector(REALSXP, 1));
  int *pv = INTEGER(vec);
  int pn = INTEGER(n)[0];
  int i=0;
  float da=0.0, db=0.0, a=0.0, b=0.0;
  
  R_isort(pv, vect);
  pv--;
  
  for(i=1;i<=vect;i++) {
    if((a=((float)i)/vect-((float)pv[i])/pn) > da) da=a;
    if((b=((float)pv[i])/pn-((float)(i-1))/vect) > db) db=b;
  }
  
  double *pans=REAL(ans);
  *pans= (da >= db) ? da : -db;

  UNPROTECT(1);
  return(ans);
}
