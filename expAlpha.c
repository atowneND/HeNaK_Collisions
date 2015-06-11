/*********************************************************/
// expAlpha.c
// expected value of alpha
// Ashley Towne
// 06/09/2015
// extracts B(alpha) from look.pcc12
// calculates expected value of alpha
/*********************************************************/

/*********************************************************/
// includes
#include <stdio.h>
#include <stdlib.h>

#include "avgFun.h"
/*********************************************************/

/*********************************************************/
// main
int main(){
    // open file
    char *datadir = "./outdata_dq18-16"; // from Prof. Hickman's data
    FILE *fd = fopen(datadir,"r");
    char indata[BUFSIZE];

    // initialize temporary pointers and flags and head of arrays
    int typeflag = 1;
    int ctr = 0;

    double *theta = malloc(sizeof(double *));
    double *B_theta = malloc(sizeof(double *));

    double *thetavec = (double *)malloc((ctr + 1)*sizeof(double));
    double *Bvec = (double *)malloc((ctr + 1)*sizeof(double));

    // read in data
    if (fd==NULL){
        // error checking
        printf("ERROR: fopen (6)\n");
        exit(1);
    }
    else{
        // read in one line of data
        while (fgets(indata,BUFSIZE,fd)!=NULL){
            // check if it's a comment
            typeflag = checkdatatype(indata, theta, B_theta);
            
            if (!typeflag){
                // resize array of dat
                thetavec = realloc(thetavec,(ctr + 1)*sizeof(double));
                Bvec = realloc(Bvec,(ctr + 1)*sizeof(double));

                // assign values to arrays
                thetavec[ctr] = *theta;
                Bvec[ctr] = *B_theta;

                // increment to prepare for next array value
                ctr = ctr + 1;
            }
        }
    }

    fclose(fd);

    // get statistics
    struct stats thetaStats = expvals(thetavec,Bvec);

    printf("Stats:\n");
    printf("\tavg = %f\n\tvar = %f\n\tstd = %f\n",thetaStats.avg,thetaStats.var,thetaStats.std);

    // clean up
    free(theta);
    free(B_theta);
    free(thetavec);
    free(Bvec);


    // calculate expectation value of alpha

    return 0;
}
/*********************************************************/
