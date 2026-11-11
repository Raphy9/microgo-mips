package main
import "fmt"

type Node struct {
    val  int
    next *Node
}

func fib(n int) int {
    if n <= 1 {
        return n
    }
    return fib(n-1) + fib(n-2)
}

func test_shadow() int {
    x := 100
    if true {
        x := 200
        x = x + 1
    }
    return x
}

func list_sum(head *Node) int {
    s := 0
    curr := head
    for curr != nil {
        s = s + curr.val
        curr = curr.next
    }
    return s
}

func main() {
    f := fib(6)
    fmt.Print("Fib(6):")
    fmt.Print(f)
    fmt.Print("\n")

    sh := test_shadow()
    if sh == 100 {
        fmt.Print("Shadowing:OK")
    } else {
        fmt.Print("Shadowing:FAIL(Got ")
        fmt.Print(sh)
        fmt.Print(")")
    }
    fmt.Print("\n")

    n1 := new(Node)
    n2 := new(Node)
    n3 := new(Node)

    n1.val = 10
    n2.val = 20
    n3.val = 30

    n1.next = n2
    n2.next = n3
    n3.next = nil

    total := list_sum(n1)
    if total == 60 {
        fmt.Print("ListSum:OK")
    } else {
        fmt.Print("ListSum:FAIL")
    }
    fmt.Print("\n")

    i := 0
    if (i == 0) || (i == 1/0) {
        fmt.Print("Logic:OK")
    } else {
        fmt.Print("Logic:FAIL")
    }
    fmt.Print("\n")
}