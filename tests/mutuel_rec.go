package main;
import "fmt";

// Résultat attendu : 10
func even(n int) bool {
    if (n == 0) { return true } else { return odd(n-1) }
};

func odd(n int) bool {
    if (n == 0) { return false } else { return even(n-1) }
};

func main() {
    if (even(10)) { fmt.Print(1) } else { fmt.Print(0) };
    if (even(11)) { fmt.Print(0) } else { fmt.Print(1) }
}