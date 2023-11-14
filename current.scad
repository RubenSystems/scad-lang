
fn do_math(x: i32, y: i32) i32 {
	x + y * x - y
};

fn main() i32 {
	let x : i32 = {
		let j : i32 = 100; 
		j + 2
	}; 

	if x == 1024 {
		print_int(num: 10000);
	} else if x == 102 {
		print_int(num: 30000);
	};

	print_int(num: x);
	
	0
};