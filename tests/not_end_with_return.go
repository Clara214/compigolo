package main;
import "fmt";

func hello() int {
	return 0
	fmt.Print("Ce message ne devrait pas s'afficher")
}

func main() {
	var a = hello()
};

