#include <stdio.h>


// fn add(x: i32, y: i32) i32 {
// 	x + y
// };

// int main() {
// 	let x : i32 = add(3, 4);
// 	if x == 5 {
// 		print_int(num: 100);
// 	};

// 	0
// };

int add(int x, int y) {
	return x + y; 
}

int main () {
	int x = add(3, 4); 
	if (x == 7) {
		printf("100\n");
	}

	return 0;
}