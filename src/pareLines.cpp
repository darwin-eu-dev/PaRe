# include <Rcpp.h>
# include <iostream>
# include <fstream>
# include <vector>
using std::string;
using std::cout;

// [[Rcpp::export]]
std::vector<std::string> pareLines(std::string fileName) {
  std::vector<std::string> lines;
  string line;
  std::ifstream myReadFile(fileName);

  while (getline(myReadFile, line)) {
    lines.push_back(line);
  }
  return lines;
}
