package main;
import "fmt";

// Résultat attendu : 21
func main() {
    var x = 1;
    var y = 2;
    x, y = y, x;
    
    fmt.Print(x);
    fmt.Print(y)
}