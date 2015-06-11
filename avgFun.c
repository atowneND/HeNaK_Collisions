/*********************************************************/
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "avgFun.h"
/*********************************************************/

/*********************************************************/
// FORTRAN function declaration
extern double dsimp_(int *N,double F[]);
/*********************************************************/

/*********************************************************/
// calculate the mean value
double fmean(double B[]){
    int npoints = MAXDATA;

    int i;
    double sum = 0;

    // calculate sum of all B
    for (i = 0; i < npoints; i++){
        sum = B[i] + sum;
    }

    // divide by number of points
    double avgB = sum/((double)npoints);

    return avgB;
}
/*********************************************************/

/*********************************************************/
// calculate statistical values
struct stats expvals(double theta[],double B[]){
    int numpoints = MAXDATA;

    // assuming theta is from 0 to pi
    int i;
    double numeratorvec[numpoints];
    double numeratorvec2[numpoints];
    double denominatorvec[numpoints];

    // calculate integrands
    for (i=0;i<numpoints;i++){
        numeratorvec[i] = theta[i]*B[i]*sin(theta[i]);
        numeratorvec2[i] = numeratorvec[i]*theta[i];
        denominatorvec[i] = B[i]*sin(theta[i]);
    }

    // calculate integrals
    double numerator = dsimp_(&numpoints,numeratorvec);
    double numerator2 = dsimp_(&numpoints,numeratorvec2);
    double denominator = dsimp_(&numpoints,denominatorvec);

    // combine for average value of tipping angle
    struct stats thetaStats;
    thetaStats.avg = numerator/denominator;
    thetaStats.var = numerator2/denominator;
    thetaStats.std = thetaStats.var - pow(thetaStats.avg,2);

    return thetaStats;
}
/*********************************************************/

/*********************************************************/
// parse data
int checkdatatype(char indata[BUFSIZE],double *theta, double *B_theta){
    int ctr = 0;
    char firstchar;
    while (isspace(indata[ctr])){
        ctr = ctr + 1;
    }

    firstchar = indata[ctr];
    switch (firstchar){
        case '#':
            return 1;
        default: 
            // parse data line
            sscanf(indata,"%lf %lf",theta,B_theta);
            return 0;
    }
}
/*********************************************************/
