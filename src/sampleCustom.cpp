#include <Rcpp.h>
using namespace Rcpp;

inline NumericVector quantile(NumericVector x, NumericVector pq) {
  Environment stats("package:stats");
  Function qntl = stats["quantile"];
  int npr = pq.size();
  NumericVector q(npr);
  q = qntl(x, pq);
  return q;
}

class rct {
  public:
    List probs;
    List binomWt;
    List cauchyWt;
    NumericVector train;
    NumericVector target;
    NumericVector cumTarget;
    Environment e;
    const static NumericVector pq;
    
    void useCauchy(bool val) { probs = val ? cauchyWt : binomWt; }
    
    void setTarget(NumericVector _target) {
      this->target = as<NumericVector>(_target);
      NumericVector _cumTarget = cumsum(_target);
      this->cumTarget = as<NumericVector>(_cumTarget);
    }
    
    rct() { }
    
    rct(Environment _e) {
      this->e = _e;
      train = e["train"];
      List binom = e["binomWt"];
      List cauchy = e["cauchyWt"];
      for (int i = 0; i < binom.size(); i++) {
        binom[i] = as<sugar::probs_t>(binom[i]);
        cauchy[i] = as<sugar::probs_t>(cauchy[i]);
      }
      this->binomWt = binom;
      this->cauchyWt = cauchy;
      useCauchy(false);
    }
    
    List weeks2Nsubjects(int nSim, int nSubjects) {
      NumericVector y(nSim);
      NumericVector p(52);
      for (int i = 0; i < nSim; i++) {
        double n = 0; 
        int k = 0;
        while (n <= nSubjects) {
          p = clone(probs(k % 52).get());
          n += sugar::SampleNoReplace(p, 1, train)(0);
          k++;
        }
        y(i) = k;
      }
      List L = List::create(_["weeks"] = y, _["CI"] = quantile(y, pq));
      return L;
    }
    
    NumericMatrix PredCIbyWk(int nSim) {
      NumericVector y(nSim);
      NumericMatrix out(52, 3);
      colnames(out) = as<CharacterVector>(quantile(y, pq).names());
      for (int i = 0; i < 52; i++) {
        NumericVector p = clone(probs(i).get());
        y = y + sugar::SampleReplace(p, nSim, train);
        out.row(i) = quantile(y, pq);
      }
      return out;
    }
    
    NumericVector getDistance(int nSim) {
      NumericVector out(nSim);
      for (int k = 0; k < nSim; k++) {
        NumericVector pred(52);
        NumericVector p = clone(probs(0).get());
        pred(0) = sugar::SampleNoReplace(p, 1, train)(0);
        for (int i = 1; i < 52; i++) {
          p = clone(probs(i).get());
          pred(i) = pred(i - 1) + sugar::SampleNoReplace(p, 1, train)(0);
        }
        out(k) = sqrt(sum(pow(pred - cumTarget, 2)));
      }
      return out;
    }
};

const NumericVector rct::pq = {.025, .5, .975};

RCPP_MODULE(mod) {
  class_<rct>("rct")
  .default_constructor()
  .constructor<Environment>()
  .method("setTarget", &rct::setTarget)
  .method("useCauchy", &rct::useCauchy)
  .method("PredCIbyWk", &rct::PredCIbyWk, "Predictive CI by week")
  .method("getDistance", &rct::getDistance)
  .method("weeks2Nsubjects", &rct::weeks2Nsubjects)
  .field("e", &rct::e)
  .field("probs", &rct::probs)
  .field("train", &rct::train, "The train vector")
  .field_readonly("target", &rct::target)
  .field("cumTarget", &rct::cumTarget)
  ;
}






