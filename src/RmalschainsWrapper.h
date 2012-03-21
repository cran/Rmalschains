
#ifndef _Rmalschains_RmalschainsWrapper_H
#define _Rmalschains_RmalschainsWrapper_H

#include <Rcpp.h>

RcppExport SEXP RmalschainsWrapper(SEXP p_fcall, SEXP p_dim, SEXP p_lower, SEXP p_upper,
		SEXP p_rho, SEXP p_popsize, SEXP p_relMaxEval, SEXP p_argls,
		SEXP p_debugMA, SEXP p_istep, SEXP p_effort, SEXP p_alpha, SEXP p_targetValue, 
                SEXP p_threshold, SEXP p_optMin, SEXP p_initialpop, SEXP p_seed);


#endif
