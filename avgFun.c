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
struct stats expvals(double theta[],double B[],int numpoints,int j,int jp){

    // assuming theta is from 0 to pi
    int i;
    double numeratorvec[numpoints];
    double numeratorvec2[numpoints];
    double denominatorvec[numpoints];

    // write data to file - redirect stdout to file
    int bak, new;
    char datfile[BUFSIZE];
    sprintf(datfile,"Run2Results/Stats/check/checkstats_%i_%i.dat",j,jp);
    fflush(stdout);
    bak = dup(1);
    new = open(datfile,O_RDWR|O_CREAT|O_APPEND,0666);
    dup2(new,1);
    close(new);
    printf("# alpha\talpha^2\tB*sin(alpha)\n");

    double sums[3];
    sums[0] = 0;
    sums[1] = 0;
    sums[2] = 0;
    // calculate integrands and write to file
    for (i=0;i<numpoints;i++){
        numeratorvec[i] = theta[i]*B[i]*sin(theta[i]);
        numeratorvec2[i] = numeratorvec[i]*theta[i];
        denominatorvec[i] = B[i]*sin(theta[i]);
        printf("%lf\t%lf\t%lf\n",theta[i],pow(theta[i],2),denominatorvec[i]);
        sums[0] = sums[0] + theta[i];
        sums[1] = sums[1] + pow(theta[i],2);
        sums[2] = sums[2] + denominatorvec[i];
    }
    printf("#Sums: %lf\t%lf\t%lf\n",sums[0],sums[1],sums[2]);

    // finish stdout redirection
    fflush(stdout);
    dup2(bak,1);
    close(bak);

    // calculate integrals
    double numerator = dsimp_(&numpoints,numeratorvec);
    double numerator2 = dsimp_(&numpoints,numeratorvec2);
    double denominator = dsimp_(&numpoints,denominatorvec);

    // combine for average value of tipping angle
    struct stats thetaStats;
    thetaStats.avg = numerator/denominator;
    thetaStats.std = numerator2/denominator - pow(thetaStats.avg,2);
    thetaStats.var = sqrt(thetaStats.std);

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
        integrand[i] = B[i]*sin(theta[i]);
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
            if (Btype==0){
                return 1;
            }
            else if (Btype==1){
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
            // parse data line
            if (Btype==0){ // B(theta)
                sscanf(indata,"%lf %lf",xval,B_val);
            }
            else if(Btype==1){ // B(lambda)
                //sscanf(indata,"%lf %lf %lf",xval,B_val,xaltval);
                return 1;
            }
            return 0;
    }
}
/*********************************************************/
