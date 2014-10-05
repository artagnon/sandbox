#include <stdio.h>

#define MAX 10

void swap(int *ar, int idx1, int idx2)
{
	int tmp = ar[idx1];
	ar[idx1] = ar[idx2];
	ar[idx2] = tmp;
}	

int partition(int *ar, int start, int end)
{
	int i = start;

	for (int j = start; j < end; j ++)
		if (ar[j] < ar[end - 1])
			swap(ar, i++, j);
	swap(ar, i, end - 1);
	return i;
}

void qsort(int *ar, int start, int end)
{
	int pivot;

	if (start == end)
		return;
	pivot = partition(ar, start, end);
	qsort(ar, start, pivot);
	qsort(ar, pivot + 1, end);
}

void print_ar(int *ar, int nr)
{
	printf("[");
	for(int i = 0; i < nr; i++)
		printf("%d;", ar[i]);
	printf("]\n");
}

int main()
{
	int ar[MAX] = { 14, 18, 92, 13, 12, 6, 18, 37, 4, 10 };
	print_ar(ar, MAX);
	qsort(ar, 0, MAX);
	print_ar(ar, MAX);
}
