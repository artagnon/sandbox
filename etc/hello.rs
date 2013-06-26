extern mod extra;

fn printer (x: &int) -> int {
    // x (borrowed pointer) is frozen
    println(fmt!("hello? %d", *x));
    *x + 1
}

fn map_closure<T, R>(vector: &[T], mapper: &fn(&T) -> R) -> ~[R] {
    // stack closure
    vector.map(|el: &T| mapper(el))
}

fn each_closure<T>(vector: &[T], function: &fn(&T) -> bool) -> bool {
    let mut n = ~0;
    while *n < vector.len() {
        if (!function(&vector[*n])) {
            return false;
        }
        *n += 1;
    }
    return true;
}

struct Point {
    x : int,
    y : int
}

fn vec_head<T: Copy>(vector: &[T]) -> T {
    copy(vector[0])
}

impl Point {
    fn new(this_x: int, this_y: int) -> Point {
        Point { x: this_x, y: this_y }
    }
    fn print(&self) {
        println(fmt!("%d, %d", self.x, self.y));
    }
}

fn destructive_change(x: &mut int) {
    *x *= 2;
}

fn main () {
    let mut count: ~int = ~0;
    println(fmt!("printer function::"));
    while *count < 3 {
        *count = printer(count);
    }

    let mut stack: ~[int] = ~[1, 2, 3];
    let mapper = |el: &int| *el * 2;
    stack = map_closure(stack, mapper);
    println(fmt!("map closure::"));
    for stack.iter().advance |el: &int| {
        println(fmt!("%d", *el));
    }

    println(fmt!("each_closure without for::"));
    each_closure([2, 4, 8, 5, 16], |n| {
        if *n % 2 != 0 {
            println(fmt!("found odd number! %d", *n));
            false
        } else { true }
    });


    println(fmt!("each_closure with for::"));
    // for can be used only with stack closures
    for each_closure([2, 4, 8, 17, 32]) |n| {
        if (*n % 2 != 0) {
            println(fmt!("odd number %d", *n));
            break;
        }
    }
    let location = Point::new(10, 20);
    location.print();

    println(fmt!("Vector HEAD:: %d", vec_head(stack)));

    let mut map = std::hashmap::HashMap::new();
    map.find_or_insert("moo", 3);
    map.find_or_insert("foo", 4);
    println("HashMap::");
    for map.iter().advance |(k, v)| {
        println(fmt!("%s => %d", *k, *v));
    }

    let stack2: ~[int] = ~[4, 5, 6];
    let zstack2 = std::vec::zip(stack, stack2);
    println("Zipped vector::");
    for zstack2.iter().advance |el| {
        println(fmt!("%?", *el));
    }

    let mut x = ~4;
    destructive_change(x);
    println("Destructive change::");
    println(fmt!("x = %d", *x));

    let mut pq = extra::priority_queue::PriorityQueue::new();
    pq.push(5);
    pq.push(6);
    pq.push(3);
    println("Priority queue::");
    for pq.iter().advance |el| {
        println(fmt!("%?", *el));
    }
}
