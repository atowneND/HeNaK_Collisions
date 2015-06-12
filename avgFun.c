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
// compute new x axis for B(alpha) -> B(lambda)
void alpha2lambda(double theta[], double lambda[], int j, int jp, int numpoints){
    // l*(l+1) = j*(j+1) + jp*(jp+1) -2*sqrt(j*(j+1)*jp*(jp+1))*cos(alpha)
    // l^2 + l - (RHS) = 0
    // a = 1, b = 1, c = -RHS
    double a = 1;
    double b = 1;
    double c;
    double x[numpoints][2];

    int i;
    for (i=0;i<numpoints;i++){
        c = -(j*(j+1) + jp*(jp+1) -2*sqrt(j*(j+1)*jp*(jp+1))*cos(theta[i]));
        quadformula(a,b,c,&x[i][0],&x[i][1]);
//        printf("t: %f\tc = %f\tlambda = %f, %f\n",theta[i],c,x[i][0],x[i][1]);

        if ((x[i][0]>0)&&(x[i][1]<0)){
            lambda[i] = x[i][0];
        }
        else{
            printf("check lambda[%i]\n",i);
            lambda[i] = x[i][0];
        }
    }
}
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
// quaddratic formula: x = (-b +- sqrt(b^2 - 4ac))/2a
void quadformula(double a, double b, double c, double *x1, double *x2){
    double determinant;
    determinant = pow(b,2)-4*a*c;
    if (determinant<0){
        printf("quadformula: answer is imaginary\nexiting program\n");
        exit(1);
    }
    else{
        *x1 = (-b + sqrt(determinant))/(2*a);
        *x2 = (-b - sqrt(determinant))/(2*a);
    }
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
