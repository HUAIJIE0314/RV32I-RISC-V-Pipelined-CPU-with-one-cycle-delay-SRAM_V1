int main() {
	extern int array_size;
	extern int array_addr; //sort addr
	extern int _test_start; //save addr

	int temp;
	
	for(int i = 0; i < array_size-1; i++) {
		for(int j = i + 1; j < array_size; j++) {
			if(*(&array_addr + i) > *(&array_addr + j)) {
				temp = *(&array_addr + j);
				*(&array_addr + j) = *(&array_addr + i);
				*(&array_addr + i) = temp;
			}
		}
	}

	//save result
	for(int i = 0; i < array_size; i++) {
		*(&_test_start + i) = *(&array_addr + i);
	}

	return 0;
}
