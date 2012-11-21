#include <iostream>
#include <vector>

using namespace std;

int main() {
	int n_numbers, this_number;
	vector<int> numbers;
	cin >> n_numbers;
	for (int i = 0; i < n_numbers; i++) {
		cin >> this_number;
		numbers.push_back(this_number);
	}
	int zeros_barrier = numbers.size() - 1;
	for (int i = numbers.size() - 1; i > 0; i--)
		if (numbers[i] == 0)
			zeros_barrier --;
		else
			break;
	for (int i = 0; i < zeros_barrier;) {
		if (numbers[i] == 0) {
			for (int j = i; j < zeros_barrier; j++) {
				if (numbers[j + 1] == 0)
					continue;
				int temp = numbers[j];
				numbers[j] = numbers[j + 1];
				numbers[j + 1] = temp;
			}
			zeros_barrier--;
		}
		else
			i++;
	}
	for (vector<int>::iterator i = numbers.begin(); i != numbers.end(); i++)
		cout << *i << " ";
	cout << endl;
	return 0;
}
