package main

import "fmt"

// Résultat attendu : 42
func main() {
	var a, b, c, d int;
	if true {
		a = 1
		fmt.Print(a)
		a = 0
	} else {
		b = 1
		fmt.Print(b)
		b = 0
	}
	a = 1
	for (a == 1) {
		a = 0
		c = 1
		fmt.Print(c)
		c = 0 
	}
	{
		d = 1
		fmt.Print(d)
		d = 0
	}
}
