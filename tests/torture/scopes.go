package main;
import "fmt";

func main() {
    x := 1;
    if true {
        x := 2;
        x++;
    } else {};
    fmt.Print(x); 
};