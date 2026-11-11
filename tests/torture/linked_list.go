// (Structures récursives et pointeurs)
package main
import "fmt"

type Node struct {
    val int
    next *Node
}

func main() {
    head := new(Node)
    head.val = 1
    
    head.next = new(Node)
    head.next.val = 2
    head.next.next = nil // nil assignable à un pointeur *Node

    fmt.Print(head.val, head.next.val)
}