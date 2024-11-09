#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int sim1(IntegerVector x, List probs, int nSubjects, int startWeek = 1L) {
  int n = 0;
  int i = startWeek - 1L;
  int ss = x.size();
  while (n <= nSubjects) {
    sugar::probs_t prob = probs(i % 52);
    NumericVector p = clone(prob.get());
    int id = sugar::SampleNoReplace(p, ss, 1, false)(0);
    n += x[id];
    i++;
  }
  return i;
}

// [[Rcpp::export]]
IntegerVector getPredVec(IntegerVector x, List probs) {
  int ss = x.size();
  IntegerVector pred(52);
  for (int i = 0; i < 52; i++) {
    sugar::probs_t prob = probs[i];
    NumericVector p = clone(prob.get());
    int id = sugar::SampleNoReplace(p, ss, 1, false)[0];
    pred[i] = x[id];
  }
  return pred;
}

// [[Rcpp::export]]
double getDistance(IntegerVector x, IntegerVector y, List probs) {
  IntegerVector pred = cumsum(getPredVec(x, probs));
  IntegerVector pred1 = pred - y; 
  NumericVector pred2 = pow(pred1, 2);
  double pred3 = sum(pred2);
  return sqrt(pred3);
}

// [[Rcpp::export]]
List PredCIbyWk(IntegerVector x, List probs, int nSim, NumericVector pq) {
  IntegerVector y(nSim);
  List out(52);
  Environment stats("package:stats");
  Function quantile = stats["quantile"];
  int npr = pq.size();
  NumericVector q(npr);
  for (int i = 0; i < 52; i++) {
    sugar::probs_t prob = probs[i];
    NumericVector p = clone(prob.get());
    IntegerVector idx = sugar::SampleReplace(p, 52, nSim, false);
    IntegerVector y1 = x[idx];
    y = y + y1;
    q = quantile(y, pq);
    out[i] = q;
  }
  return out;
}


inline NumericVector quantile(IntegerVector x, NumericVector pq) {
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
  IntegerVector train;
  IntegerVector target;
  IntegerVector cumTarget;
  Environment e;

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
  rct() { }
  
  
  const static NumericVector pq;
  
  void setTarget(NumericVector _target) {
    this->target = as<IntegerVector>(_target);
    NumericVector _cumTarget = cumsum(_target);
    this->cumTarget = as<IntegerVector>(_cumTarget);
  }
  
  void useCauchy(bool val) { probs = val ? cauchyWt : binomWt; }
  
  IntegerVector weeks2Nsubjects(int nSim, int nSubjects) {
    IntegerVector y(nSim);
    NumericVector p(52);
    for (int i = 0; i < nSim; i++) {
      int n = 0, k = 0;
      while (n <= nSubjects) {
        p = clone(probs(k % 52).get());
        n += sugar::SampleNoReplace(p, 1, train)(0);
        k++;
      }
      y(i) = k;
    }
    return y;
  }

  NumericMatrix PredCIbyWk(int nSim) {
    IntegerVector y(nSim);
    NumericMatrix out(52, 3);
    colnames(out) = as<CharacterVector>(quantile(y, pq).names());
    for (int i = 0; i < 52; i++) {
      NumericVector p = clone(probs(i).get());
      y = y + sugar::SampleReplace(p, nSim, train);
      out.row(i) = quantile(y, pq);
    }
    return out;
  }
  
  IntegerVector getPredVec() {
    IntegerVector pred(52);
    for (int i = 0; i < 52; i++) {
      NumericVector p = clone(probs(i).get());
      pred(i) = sugar::SampleNoReplace(p, 1, train)(0);
    }
    return pred;
  }
  
  NumericVector getDistance(int nSim) {
    NumericVector out(nSim);
    for (int k = 0; k < nSim; k++) {
      IntegerVector pred(52);
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






