package main;
import "fmt";

// Résultat attendu 512
func main() {
  var a, n, acc int;
  a = 2;
  n = 9;
  acc = 1;
  for (n != 0) {
    if (n%2 != 0) { acc = a*acc }; 
    a = a*a;
    n = n/2
  }
  fmt.Print(acc);
};
