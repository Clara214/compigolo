package main;
import "fmt";

func f() (int, int) {
    return 40, 2
};

// Résultat attendu : 42
func main() {
    var a int;
    var res int;
    _, a = f();
    res, _ = f();
    
    fmt.Print(res + a)
}