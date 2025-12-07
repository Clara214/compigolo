package main;
import "fmt";

type point struct { x int }
func f() (*point) { 
    var p = new (point)
    p.x = 5
    return p
}

func main() {
	fmt.Print(f())
}