#include <stdio.h>
#include <limits.h>

#define max(x, y) ((x > y) ? x : y)
#define min(x, y) ((x > y) ? y : x)

int minimal_sum(int *ar, int start, int end, int k)
{
	int oldmax, newmax, this_partition, min_sum_others;

	newmax = INT_MAX;
	if (start > end)	
		return INT_MIN;
	if (!k) {
		this_partition = 0;
		for (int j = start; j <= end; j++)
			this_partition += ar[j];
		return this_partition;
	}
	for (int i = 0; start + i <= end; i++) {
		this_partition = 0;
		oldmax = newmax;
		for (int j = start; j <= start + i; j++)
			this_partition += ar[j];
		min_sum_others = minimal_sum(ar, start + i + 1, end, k - 1);
		newmax = min(oldmax, max(this_partition, min_sum_others));
	}
	return newmax;
}

#define NR 9

int main()
{
	int ar[NR] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	printf("%d\n", minimal_sum(ar, 0, NR - 1, 2));
	return 0;
}
