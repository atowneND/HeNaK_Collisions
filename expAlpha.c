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

#include "parsedata.h"
/*********************************************************/

/*********************************************************/
// main
int main(){
    char *datadir = "./outdata_dq18-16"; // from Prof. Hickman's data
    FILE *fd = fopen(datadir,"r");
    char indata[BUFSIZE];

    int *theta = malloc(sizeof(int*));
    float *B_theta = malloc(sizeof(float));

    int *thetavec = malloc(sizeof(int));
    float *Bvec = malloc(sizeof(float));
    /*float *theta = malloc(sizeof(float*));
    float *B_theta = malloc(sizeof(float*));

    float *thetavec = malloc(sizeof(float));
    float *Bvec = malloc(sizeof(float));*/
    int ctr = 0;

    // read in data
    if (fd==NULL){
        // error checking
        printf("ERROR: fopen (6)\n");
        exit(1);
    }
    else{
        // read in one line of data
        while (fgets(indata,BUFSIZE,fd)!=NULL){
            checkdatatype(indata, theta, B_theta);
//            thetavec[0] = *theta;
//            Bvec[0] = *B_theta;
//            printf("theta = %f\tB = %f\n",*theta,*B_theta);
        }
    }

    fclose(fd);

    // clean up
    free(theta);
    free(B_theta);
    free(thetavec);
    free(Bvec);


    // calculate expectation value of alpha

    return 0;
}
/*********************************************************/
