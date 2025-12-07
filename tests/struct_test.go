package main;
import "fmt";

type S struct { x int; y int; z int }

func main() {
	s := new(S)
	s.x = 0; s.y = 1; s.z = 2;
	fmt.Print(s)
	fmt.Print(s.x)
	fmt.Print(s.y)
	fmt.Print(s.z)
};
