package main

import (
	"fmt"
	"math"
)

func Sqrt(x float64) float64 {
	for {
		var z = float64(1)
		x = (z - ((z*z)-x)/2*z)
		if x > 0 {
			break
		}
	}
	return x
}
func main() {
	var v int = int(Sqrt(2))
	fmt.Println(v)
	fmt.Println(math.Sqrt(2))
}
