#include <stdio.h>

#define left(i) (2 * i + 1)
#define right(i) (2 * i + 2)
#define parent(i) ((i - 1) / 2)

void swap(int *ar, int idx1, int idx2)
{
	int tmp = ar[idx1];
	ar[idx1] = ar[idx2];
	ar[idx2] = tmp;
}	

void heapify(int *ar, int nr, int i)
{
	int largest = i;

	if (left(i) < nr && ar[largest] < ar[left(i)])
		largest = left(i);
	if (right(i) < nr && ar[largest] < ar[right(i)])
		largest = right(i);
	if (i != largest) {
		swap(ar, i, largest);
		heapify(ar, nr, largest);
	}
}

void print_ar(int *ar, int nr)
{
	printf("[");
	for(int i = 0; i < nr; i++)
		printf("%d;", ar[i]);
	printf("]\n");
}

#define MAX 10

int main()
{
	int ar[MAX] = { 45, 12, 19, 87, 72, 13, 10, 9, 21, 74 };

	print_ar(ar, MAX);
	for (int i = parent(MAX); i >= 0; i--)
		heapify(ar, MAX, i);
	print_ar(ar, MAX);
	return 0;
}
