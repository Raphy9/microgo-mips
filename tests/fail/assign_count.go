package main
func foo() (int, int) { return 1, 2 }
func main() {
    x, y, z := foo() // Error: Mismatch count (3 variables, 2 values)
}