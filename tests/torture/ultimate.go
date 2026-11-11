package main;
import "fmt";

/* Structure pour tester le tas (Heap) et les pointeurs */
type Node struct {
    val int;
    next *Node
};

func triangle(n, p1, p2 int) int {
    if (n <= 0) { return 0 };
    return n + triangle(n-1, p1, p2)
};

func swap_calc(a, b int) (int, int) {
    return b + 10, a + 10
};

func main() {
    res := 10 * 2 + 10 / 2; 
    if (res == 25 && (1==0 || 1==1)) {
        res = res + 5 // res = 30
    };
    if (res > 0) {
        res := 1000;
        res = res + 1
    };
    list := new(Node);
    list.val = 10;
    list.next = new(Node);
    list.next.val = 20;
    list.next.next = new(Node);
    list.next.next.val = 30;

    sum_list := 0;
    iter := list;
    
    for (iter != nil) {
        sum_list = sum_list + iter.val;
        iter = iter.next
    };
    valA := 1;
    valB := 2;
    valA, valB = swap_calc(valA, valB);
    sum_tuple := valA + valB; 
    rec_val := triangle(10, 0, 0);

    // res (30) + sum_list (60) + sum_tuple (23) + rec_val (55) = 168
    total := res + sum_list + sum_tuple + rec_val;

    fmt.Print(total);
    fmt.Print("\n")
};