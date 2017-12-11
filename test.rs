
struct Point {
    x : i8,
    y : i8
}

fn f (x: i8) -> i8 {
    return x;
}

fn main() {
    let mut m = 12;
    m = 1;

    let mut out = f (12);
    let hoge = out + 1;
    if out == 11 {
        println!("yes");
    } else {
        println!("no")
    }

    let arr = [0, 1, 2, 3, 4];
    for v in arr.iter() {
        println!("{}", v);
    }

    let vector = vec![0,1,2,3];
    // println!( out );

    let fuga = Some (10);
    let foo = match fuga {
        None     => false,
        Some (_) => true
    };
    let xx = (|x| {x + 1}) (3);
}
