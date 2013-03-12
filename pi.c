#include <stdio.h>    // printf, fprint
#include <sys/time.h> // timeval, gettimeofday
#include <stdlib.h>   // srand48, brand48

// argc holds the number of command-line arguments +1
// argv holds those arguments starting at argv[1]

main(int argc, char * argv[]) {

    // checking to make sure there's at least one argument

    if(argc < 2) {

        fprintf(stderr, "Please specify an integer on the command line\n");
        exit(-1);

        }

    /* a timeval structure allows you to get the current
       time in order to seed a random number generator
       */

    struct timeval time;
    gettimeofday(&time, NULL);
    srand48(time.tv_usec);
    int counter = 0, iterations = atoi(argv[1]), i;

    /* count up to the number of iterations specified
       on the command-line and then generate a point
       (x,y) in [0,1] x [0,1] (the unit square). If
       x*x + y*y < 1, then this point is also in the
       unit circle. But the area of the unit circle
       is pi/4 times that of the unit square. So taking
       the ratio of points in the circle to all points
       gives us roughly pi/4.
       */

    for(i = 0; i < iterations; ++i) {

        float x = drand48(), y = drand48();
        counter += (x*x + y*y < 1);

        }

    // counter/iterations ~ pi/4

    float pi = (float) (counter*4)/iterations;
    printf("%f\n", pi);

    }

