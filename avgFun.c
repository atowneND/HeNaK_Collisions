/*********************************************************/
#include <ctype.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
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
// compute new x axis for B(alpha) -> B(lambda)
double lambda2alpha(double lambda, int j, int jp){
    // l*(l+1) = j*(j+1) + jp*(jp+1) -2*sqrt(j*(j+1)*jp*(jp+1))*cos(alpha)
    // alpha=acos((l*(l+1)-j*(j+1)-jp*(jp+1))/(-2*sqrt(j*(j+1)*jp*(jp+1))))

    double numerator = lambda*(lambda+1) - j*(j+1) - jp*(jp+1);
    double denominator = -2*sqrt(j*(j+1)*jp*(jp+1));
    double alpha = acos(numerator/denominator)*180/PI;
    return alpha;
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
struct stats expvalsQM(double lambda[],double theta_l[],double B[], int numpoints, int j, int jp){
    int i;
    double numeratorvec[numpoints];
    double numeratorvec2[numpoints];
    double denominatorvec[numpoints];

    double num=0;
    double num2=0;
    double denom=0;

    for (i=0;i<numpoints;i++){
        //numeratorvec[i] = theta_l[i]*B[i]*(2*lambda[i]+1);
        numeratorvec[i] = lambda[i]*B[i]*(2*lambda[i]+1);
        numeratorvec2[i] = numeratorvec[i]*lambda[i];
        denominatorvec[i] = B[i]*(2*lambda[i]+1);

        num = num + numeratorvec[i];
        num2 = num2 + numeratorvec2[i];
        denom = denom + denominatorvec[i];
    }
    struct stats thetaStats;
    thetaStats.avg = num/denom;
    thetaStats.var = num2/denom - pow(thetaStats.avg,2);
    thetaStats.std = sqrt(thetaStats.var);
    //printf("avg=%lf\t<a^2>=%lf\tstd=%lf\tvar=%lf\n",thetaStats.avg,num2/denom,thetaStats.std,thetaStats.var);

    // convert to an angle
    thetaStats.avg = lambda2alpha(thetaStats.avg,j,jp);
    thetaStats.std = lambda2alpha(thetaStats.std,j,jp);
    thetaStats.var = pow(thetaStats.std,2);

    return thetaStats;
}
/*********************************************************/

/*********************************************************/
// calculate statistical values
struct stats expvals(double theta[],double B[],int numpoints,int j,int jp){

    // assuming theta is from 0 to pi
    int i;
    double numeratorvec[numpoints];
    double numeratorvec2[numpoints];
    double denominatorvec[numpoints];

    // calculate integrands
    for (i=0;i<numpoints;i++){
        numeratorvec[i] = theta[i]*B[i]*sin(theta[i]*PI/180);
        numeratorvec2[i] = numeratorvec[i]*theta[i];
        denominatorvec[i] = B[i]*sin(theta[i]*PI/180);
    }

    // calculate integrals
    double numerator = dsimp_(&numpoints,numeratorvec);
    double numerator2 = dsimp_(&numpoints,numeratorvec2);
    double denominator = dsimp_(&numpoints,denominatorvec);

    // combine for average value of tipping angle
    struct stats thetaStats;
    thetaStats.avg = numerator/denominator;
    thetaStats.var = numerator2/denominator - pow(thetaStats.avg,2);
    thetaStats.std = sqrt(thetaStats.var);

    return thetaStats;
}
/*********************************************************/

/*********************************************************/
// normalize B's
void normBtheta(double theta[],double B[],double Bnorm[],int numpoints){
    int i;
    double integrand[numpoints];

    // calculate integrands
    for (i=0;i<numpoints;i++){
        integrand[i] = B[i]*sin(theta[i]*PI/180);
    }

    // calculate integrals
    double NormFactor = dsimp_(&numpoints,integrand);

    // normalize B
    for (i=0;i<numpoints;i++){
        Bnorm[i] = B[i]/NormFactor;
    }
}
void normBlambda(double lambda[],double B[],double Bnorm[],int numpoints){
    int i;
    //    double integrand[numpoints];

    // calculate integrands
    //    for (i=0;i<numpoints;i++){
    //    }

    // calculate integrals
    // CURRENTLY NOT NORMALIZED
    double NormFactor = 1;

    // normalize B
    for (i=0;i<numpoints;i++){
        Bnorm[i] = B[i]/NormFactor;
    }
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
int checkdatatype(char indata[BUFSIZE],double *xval, double *B_val, double *xaltval, int Btype){
    // at some point, a data struct should probably be passed in... or there should be two functions
    int ctr = 0;
    static int linectr;
    char firstchar;
    while (isspace(indata[ctr])){
        ctr = ctr + 1;
    }

    firstchar = indata[ctr];
    switch (firstchar){
        case '#':
            if (Btype==0){ // B(theta)
                return 1;
            }
            else if (Btype==1){ // B(lambda)
                linectr = linectr + 1;
                if (linectr<=8){
                    return 1;
                }
                if (linectr>8){
                    sscanf(indata,"%c %lf %lf %lf",&firstchar,xval,B_val,xaltval);
                    return 0;
                }
            }
        default: 
            if (Btype==0){ // B(theta)
                sscanf(indata,"%lf %lf",xval,B_val);
            }
            else if(Btype==1){ // B(lambda)
                return 1;
            }
            return 0; // should only do this for B(theta)
    }
    // return 1 = no data in this line
    // return 0 = data in this line
}
/*********************************************************/
