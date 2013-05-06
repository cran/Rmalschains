#include <R_ext/Print.h>
#include <R_ext/Error.h>

#include <sstream>

#include <Rcpp.h>

#include "problem.h"
#include "RmalschainsEvaluate.h"

#include "debug.h"
#include "ilocalsearch.h"
#include "cross.h"
#include "ea.h"
#include "ssga.h"
#include "cmaeshan.h"
#include "srandom.h"
#include "malschains.h"
#include "get_util.h"
#include <iostream>
#include <cstdio>
#include <cassert>

using namespace realea;
using namespace std;
using std::auto_ptr;

EvalBase *ev = NULL;

tFitness rFitnessFunc(const tGen *x, int n) {
	Rcpp::NumericVector par(n);

	for (int i = 0; i < n; i++) {

		par[i] = x[i];
		//	Rprintf("%Le\n", par[i]);
	}

	double res = ev->eval(par);

	//Rprintf("%Le\n", res);

	return res;
}

RcppExport SEXP RmalschainsWrapper(SEXP p_fcall, SEXP p_dim, SEXP p_lower, SEXP p_upper,
		SEXP p_rho, SEXP p_popsize, SEXP p_maxEval, SEXP p_argls,
		SEXP p_debugMA, SEXP p_istep, SEXP p_effort, SEXP p_alpha, SEXP p_targetValue, SEXP p_threshold, 
		SEXP p_optMin, SEXP p_initialpop, SEXP p_seed, SEXP p_lsOnly) {

	BEGIN_RCPP;

	unsigned int dim = Rcpp::as<unsigned int>(p_dim);
	unsigned int popsize = Rcpp::as<unsigned int>(p_popsize);
	unsigned int maxEval = Rcpp::as<unsigned int>(p_maxEval);
	unsigned int istep = Rcpp::as<unsigned int>(p_istep);

	Rcpp::NumericVector upper(p_upper);
	Rcpp::NumericVector lower(p_lower);

	bool debugMA = Rcpp::as<bool>(p_debugMA);

	if(debugMA) {
		enable_print_info();
		enable_print_debug();
		set_InitVerbose();
	}
	else {
		disable_print_info();
		disable_print_debug();
	}

	double effort = Rcpp::as<double>(p_effort);
	double alpha = Rcpp::as<double>(p_alpha);

	double targetValue = Rcpp::as<double>(p_targetValue);
	double threshold = Rcpp::as<double>(p_threshold);
	bool optMin = Rcpp::as<bool>(p_optMin);

	//unsigned int maxEval = relMaxEval + istep;

	std::string arg_ls = Rcpp::as<std::string>(p_argls);

	unsigned long seed = Rcpp::as<unsigned long>(p_seed);

	bool lsOnly = Rcpp::as<bool>(p_lsOnly);

	Random random(new SRandom(seed));

	if (TYPEOF(p_fcall) == EXTPTRSXP) {
		ev = new EvalCompiled(p_fcall, p_rho);
	} else {
		ev = new EvalStandard(p_fcall, p_rho);
	}

	ProblemPtr prob(new Problem());
	prob->setDimension(dim);

	for(unsigned int i=0; i < dim; i++) {

		prob->setDomainValues(i, lower(i), upper(i), true);
	}

	prob->setOptimize(targetValue, threshold);
	prob->setMaxEval(maxEval);

	if(optMin)
		prob->setMinimize();
	else
		prob->setMaximize();

	//Rprintf("targetValue: %Le\n", targetValue);

	prob->setEval(rFitnessFunc);

	tChromosomeReal sol(dim);
	tFitness fitness = 0;

	DomainReal *domain = prob->getDomain();

	ILocalSearch *ls = get_LS(arg_ls, domain, &random);

	PopulationReal *pop;
	MALSChains *ma = NULL;
	SSGA *ssga = NULL;
	Hybrid *hybrid = NULL;
    EA *alg;

	if(lsOnly) {

		//the local search needs a population (at least cmaes and sw need it)
		pop = new PopulationReal(&random, popsize, popsize);
		pop->reset(domain);

	} else {

		ssga = new SSGA(&random);
		ssga->setCross(new CrossBLX(alpha));
		ssga->setMutation(new MutationBGA());
		ssga->setSelect(new SelectNAM(3));
		ssga->setReplacement(new ReplaceWorst());

		ma = new MALSChains(ssga, ls);

		if (debugMA) {
			ma->setDebug();
		}

		ma->setRestart(new RestartBest());
		hybrid = ma;
		hybrid->setEffortRatio(effort);

		//Rprintf("RatioLS: %f\nIstep: %d\n", effort, istep);

		std::stringstream effortStr;
		effortStr << effort;

		std::stringstream maxEvalStr;
		maxEvalStr << maxEval;

		set_Effort(hybrid, effortStr.str());
		hybrid->setIntensity(istep);
		set_MaxEval(hybrid, maxEvalStr.str());

		if (popsize > 0) {
			//Rprintf("Popsize: %u\n", popsize);
			hybrid->setPopsize(popsize);
		}

		//EA alg(hybrid, prob);
		alg = new EA(hybrid, prob);

		//Rprintf("sol%Le\n", sol[0]);

		ma->init();

		pop = ma->getPop();
	}

	if(p_initialpop != NULL && p_initialpop != R_NilValue) {

		Rcpp::NumericMatrix initialpop(p_initialpop);
		unsigned indsize = initialpop.ncol();

		if(indsize != dim) {

			Rf_warning("Problem with your initial population: not right number of dimensions. Not using it.\n");

		} else {

			//PopulationReal *pop = ma->getPop();

			for(int s=0; s < initialpop.nrow() && s < popsize; s++) {

				tChromosomeReal ind(dim);

				for(unsigned int i=0;i<dim;i++) ind[i] = initialpop(s, i);

				tFitness fitness = prob->eval(ind);
				pop->change(s, ind, fitness);
			}

			if(lsOnly) {

				//if initialpop is given, use the best individual
				//as starting point of the local search

				unsigned pos = pop->getBest();
				tIndividualRealPtr best= pop->getInd(pos);

				tChromosomeReal bestsol= best->sol();
				copy(bestsol.begin(), bestsol.end(), sol.begin());
				fitness = best->perf();
			}

		}
	}

	if(lsOnly) {

		//run the local search

		Running* running = new Running(prob->getFinishCriterion());
		running->setMaxEval(prob->getMaxEval());

		ls->setPopulation(pop);
		ls->setProblem(prob.get());
		ls->setRandom(&random);
		ls->setRunning(running);
		ls->setEval(prob.get());

		ILSParameters *params = ls->getInitOptions(sol);

		tFitness fitness_old = 0;
		unsigned eval = 0;

		//run the local search in steps of 100 and break if no more improvement is present
		while(eval < maxEval) {

			ls->apply(params, sol, fitness, 100);

			if(abs(fitness - fitness_old) < threshold) break;

			eval += 100;
			fitness_old = fitness;
		}
	} else {

		ma->realApply(sol, fitness);
	}

	//Rprintf("%Le\n", fitness);

	return Rcpp::List::create(Rcpp::Named("fitness") = fitness, Rcpp::Named("sol") = sol);

	//To suppress the initialized but not used compiler warning
	(void)alg;

	END_RCPP;
}
