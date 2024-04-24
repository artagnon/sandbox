# mesosched

The scheduling algorithm is simple: find the job with the highest timestamp requirements, and
allocate it to the node that has just enough availability. Maintain a queue of jobs that weren't
scheduled so they can be scheduled when resources become free. Keep stepping through the allocations
until some resources become free, and allot these resources to jobs in the queue using the same
algorithm.

The output format:

    1 >> r#1 -> j#0 || r#2 -> j#1 || r#7 -> j#4 || r#8 -> j#2 ||
    5 >> r#1 -> j#5 || r#2 -> j#3 ||

The first number before the '>>' specifies the global timestamp at which jobs are being scheduled
and resources are being allocated. r# is followed by the resource ID and j# is followed by the job
ID. In the first time stamp, resource 1 is allocated to job 0, resource 2 is allocated to job 1,
resource 7 is allocated to job 4 and so on.

Not much work has been done in attempting to show that this is a good scheduling algorithm.

## Building
```
# In the mesosched directory
$ mkdir mesosched-build
$ cd mesosched-build
$ cmake -GNinja ..
$ ninja check
```
