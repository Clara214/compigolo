package main;
import "fmt";

func f() (int, int) {
	return 2, 3
}

func id(x int, y int) (int, int) {
	return x, y
}

func main() {
	fmt.Print(id(f()))
};
