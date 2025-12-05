package main;
import "fmt";

func f() { return 1, 2; }

func main() {
    var a, b, c, d = f(), f()
}