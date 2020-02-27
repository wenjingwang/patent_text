#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include<map>
#include<vector>
#include<iostream>
#include<string>
#include <cmath>
#include<set>
using namespace std;
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
  //
  //   http://www.rcpp.org/
  //   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
  //
  
  class TfidfCalc{
    public:
      TfidfCalc(){};
    void Init(DataFrame input,int doc_nums);
    arma::sp_mat calc(List docs);
    void clear();
    private:
      vector<int> df;
    vector<int> count;
    double doc_nums;
    map<string,int> hash;
  };

void TfidfCalc::clear(){
  df.clear();
  hash.clear();
  count.clear();
}
void TfidfCalc::Init(DataFrame input,int doc_nums){
  this->doc_nums = doc_nums;
  StringVector keywords = input[0];
  IntegerVector df = input[1];
  IntegerVector count = input[2];
  Rcout << "Init data  keywords length: "<< keywords.size()<< endl;
  for(int i=0;i<keywords.size();i++){
    string word = Rcpp::as<string>(keywords[i]);
    this->hash[word]=i;
    this->df.push_back(df[i]);
    this->count.push_back(count[i]);
  }
  return;
}

arma::sp_mat TfidfCalc::calc(List docs){
  Rcout<< "input doc number: "<< docs.size() << endl;
  int start_idx = 0;
  arma::sp_mat res = arma::zeros<arma::sp_mat>(docs.size(),this->df.size());
  int nulldoc = 0;
  for(int docidx=start_idx;docidx<docs.size();docidx++){
    Rcout<<"processing " << docidx+1 << " document" <<endl;
    if(Rf_isNull(docs[docidx])){
      Rcout<<"doc "<< docidx << " is null"<<endl;
      nulldoc++;
      continue;
    }
    Rcpp::CharacterVector cv = docs[docidx];
    for(int j=0;j<cv.size();j++){
      if(Rf_isNull(cv[j])){
        Rcout << "doc "<< docidx << " word "<< j << " is null"<<endl;
        continue;
      }
      string cstr = Rcpp::as<string>(cv[j]);
      auto it = this->hash.find(cstr);
      if(it!=this->hash.end()){
        res(docidx,it->second) = res(docidx,it->second) +1;
      }else{
      }
    }
    for(unsigned int i=0;i<this->df.size();i++){
      double val =  res(docidx,i);
      if(val==0){
        continue;
      }
      res(docidx,i) = val / this->count[i] * log(this->doc_nums/this->df[i]);
    }
  }
  Rcout<<"found "<< nulldoc <<" empty docs"<<endl;
  return res;
}

RCPP_MODULE(locationmodule){
  Rcpp::class_<TfidfCalc>( "TfidfCalc" )
  .constructor("documentation for default constructor")
  .method( "Init", &TfidfCalc::Init, "documentation for Init")
  .method("calc",&TfidfCalc::calc,"calc tfidf")
  .method("clear",&TfidfCalc::clear,"clear tfidf")
  ;
}