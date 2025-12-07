package main;
import "fmt";

type S struct { x int; y int; z int }

func main() {
	_ = new(S)
	_ = 0
	_ = "caca"
	_ = true
	fmt.Print("ca marche")
};
