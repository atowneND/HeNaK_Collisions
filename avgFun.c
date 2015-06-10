#include <math.h>
#include <stdio.h>
#include "avgFun.h"

extern double dsimp_(int *N,double F[]);

double fmean(double B[]){
    int npoints = MAXDATA;

    int i;
    double sum = 0;

    // calculate sum of all B
    for (i = 0; i < npoints; i++){
        sum = B[i] + sum;
    }

    // divide by number of points
    double avgB = sum/npoints;

    return avgB;
}

double expvals(double theta[],double B[],int retval){
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
    double avgTheta = numerator/denominator;
    double avgTheta2 = numerator2/denominator;
    double varTheta = avgTheta2 - avgTheta*avgTheta;

    switch (retval){
        case 0: // average theta
            return avgTheta;
        case 1: // variance theta
            return varTheta;
        case 2: // standard deviation
            return sqrt(varTheta);
        default:
            return -1;
    }
}

