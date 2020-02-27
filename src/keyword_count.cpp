#include<Rcpp.h>
#include<map>
#include<vector>
#include<iostream>
#include<string>
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
  
  //using namespace Rcpp;


class KeywordCount{
  public:
    KeywordCount();
  void addDocs(List docs);
  void print();
  DataFrame getKeywords();
  List getStats();
  private:
    int current_doc_idx;
  int processed_doc_num;
  vector<string> strs;
  vector<int> df;
  vector<int> counts;
  map<string,int> hash;
};


//constructors
KeywordCount::KeywordCount(){ 
  current_doc_idx = -1;
  processed_doc_num = 0;
}
List KeywordCount::getStats(){
  int word_count = this->strs.size();
  List list = List::create(Named("current_doc_idx") = this->current_doc_idx , 
                           Named("processed_doc_num") = this->processed_doc_num,
                           Named("word_count") =  word_count);
  return list;
}
void KeywordCount::print(){
  Rcout << "-----------------------------current state-----------------------------" <<endl;
  Rcout<< "keywords count "<< this->current_doc_idx<<endl;
  Rcout << "process " << this->processed_doc_num << " documents"<<endl;
  Rcout << "-----------------------------current state-----------------------------" <<endl;
}

DataFrame KeywordCount::getKeywords(){
  Rcpp::StringVector sv2(this->strs.size());
  for(unsigned int i=0;i<this->strs.size();i++){
    sv2[i] = Rcpp::String(this->strs[i], CE_UTF8);
  }
  return Rcpp::DataFrame::create(Rcpp::Named("key") = sv2,
                                 Rcpp::Named("df") = this->df,
                                 Rcpp::Named("count") = this->counts);
}


//print function
void KeywordCount::addDocs(List docs){
  int nulldoc = 0;
  this->print();
  Rcout<< docs.size() << endl;
  int start_idx = 0;
  for(int docidx=start_idx;docidx<docs.size();docidx++){
    Rcout<<"processing " << docidx+1 << " document, current word count "<< this->current_doc_idx <<endl;
    if(Rf_isNull(docs[docidx])){
      Rcout<<"doc "<< docidx << " is null"<<endl;
      nulldoc++;
      continue;
    }
    this->processed_doc_num++;
    Rcpp::CharacterVector cv = docs[docidx];
    map<string,int> current_doc_set;
    for(int j=0;j<cv.size();j++){
      if(Rf_isNull(cv[j])){
        Rcout << "doc "<< docidx << " word "<< j << " is null"<<endl;
        continue;
      }
      string cstr = Rcpp::as<string>(cv[j]);
      auto curr_it = current_doc_set.find(cstr);
      if(curr_it!=current_doc_set.end()){
        int wordidx = curr_it->second;
        this->counts[wordidx]++;
        continue;
      }
      auto it = hash.find(cstr);
      if(it==hash.end()){
        this->current_doc_idx++;
        hash[cstr]=this->current_doc_idx;
        current_doc_set[cstr] = this->current_doc_idx;
        this->strs.push_back(cstr);
        this->df.push_back(1);
        this->counts.push_back(1);
        //Rcout<<"visit key " << cstr<<" "<<this->current_doc_idx<<endl;
      }else{
        int wordidx = it->second;
        //Rcout<<"visit key " << cstr<< " "<<wordidx<<endl;
        current_doc_set[cstr] = wordidx;
        this->df[wordidx]++;
        this->counts[wordidx]++;
      }
    }
  }
  Rcout<<"process documents done"  <<endl;
  Rcout <<"null doc count: " << nulldoc <<endl;
  this->print();
}

RCPP_MODULE(locationmodule){
  Rcpp::class_<KeywordCount>( "KeywordCount" )
  .constructor("documentation for default constructor")
  .method( "addDocs", &KeywordCount::addDocs, "documentation for print")
  .method( "print", &KeywordCount::print, "documentation for print")
  .method("getKeywords",&KeywordCount::getKeywords,"")
  .method("getStats",&KeywordCount::getStats,"")
  ;
  
}



void printCurrentPath(){
  Function getwd("getwd");
  string current_path = Rcpp::as<string>(getwd());
  cout<< "current path is: "<<current_path<<endl;
}
