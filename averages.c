#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "avgFun.h"
extern double dsimp_(int *N,double F[]);

int main(){
    // test these functions
    // y = cos(x);

    // initialize
    int testnpoints = 10000;
    int p;
    double y[testnpoints], x[testnpoints];
    double avgx,varx,stdx;
    double avgy,vary,stdy;
    
    // x is equally spaced points between 0 and pi
    // y is cos(x)
    for (p=0;p<=testnpoints;p++){
       // x[p] = p*PI/(2*testnpoints);
       // y[p] = cos(x[p])*sin(x[p]);

        x[p] = p*PI/(testnpoints);
        y[p] = pow(cos(x[p]),4)*sin(x[p]);
        printf("%f %f\n",x[p], y[p]);
    }
    printf("pi = %lf\n",PI);

    testnpoints = testnpoints + 1;
    double integral = dsimp_(&testnpoints,y)*PI/(testnpoints);
    printf("integral = %f\n",integral);
    
    
    return 0;
}

