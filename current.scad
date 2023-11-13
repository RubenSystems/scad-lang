
fn do_math(x: i32, y: i32) i32 {
	x + y * x - y
};

fn main() i32 {
	let x : i32 = 2 + 4; 
	if x == 6 {
		let m : i32 = do_math(x: x, y: x + 2);
		print_int(num: m);
	} else if x == 7 {
		print_int(num: 7);
	};
	
	0
};