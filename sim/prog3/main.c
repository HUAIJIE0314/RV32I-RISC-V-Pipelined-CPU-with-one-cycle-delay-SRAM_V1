int GCD(unsigned int a, unsigned int b) {
	if(b == 0) {
		return a;
	}
	return GCD(b, a % b);
}

int main() {
	extern unsigned int div1, div2;
	extern int _test_start;

	*(&_test_start) = GCD(div1, div2);

	return 0;
}
