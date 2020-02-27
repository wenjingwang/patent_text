#include<Rcpp.h>
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
    vector<vector<double> > calc(List docs);
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
  Rcout << "Init data "<<  input.size() <<" keywords length: "<< keywords.size()<< endl;
  for(int i=0;i<keywords.size();i++){
    string word = Rcpp::as<string>(keywords[i]);
    this->hash[word]=i;
    this->df.push_back(df[i]);
    this->count.push_back(count[i]);
  }
  return;
}

vector<vector<double> > TfidfCalc::calc(List docs){
  Rcout<< docs.size() << endl;
  int start_idx = 0;
  vector<vector<double>> res;
  int nulldoc = 0;
  for(int docidx=start_idx;docidx<docs.size();docidx++){
    Rcout<<"processing " << docidx+1 << " document" <<endl;
    if(Rf_isNull(docs[docidx])){
      Rcout<<"doc "<< docidx << " is null"<<endl;
      nulldoc++;
      continue;
    }
    vector<double> tf(this->df.size());
    Rcpp::CharacterVector cv = docs[docidx];
    for(int j=0;j<cv.size();j++){
      if(Rf_isNull(cv[j])){
        Rcout << "doc "<< docidx << " word "<< j << " is null"<<endl;
        continue;
      }
      string cstr = Rcpp::as<string>(cv[j]);
      auto it = this->hash.find(cstr);
      if(it!=this->hash.end()){
        tf[it->second] = tf[it->second]+1;
      }else{
        // Rcout<<"cannot find word "<< cstr << " index" <<endl;
      }
    }
    for(unsigned int i=0;i<tf.size();i++){
      tf[i] = tf[i]/this->count[i]* log(this->doc_nums/this->df[i]);
    }
    res.push_back(tf);
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