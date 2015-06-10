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

    float *theta = malloc(sizeof(float *));
    float *B_theta = malloc(sizeof(float *));

    float *thetavec = malloc(sizeof(float)*91);
    float *Bvec = malloc(sizeof(float)*91);

    int typeflag = 1;
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
            typeflag = checkdatatype(indata, theta, B_theta);
            if (!typeflag){
                thetavec[ctr] = *theta;
                Bvec[ctr] = *B_theta;
                ctr = ctr + 1;
            }
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
