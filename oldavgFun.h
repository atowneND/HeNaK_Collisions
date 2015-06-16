/*********************************************************/
#ifndef AVGFUN_H_
#define AVGFUN_H_

#define MAXDATA 100
#define PI 3.14159265358979323
#define BUFSIZE 1000
/*********************************************************/

/*********************************************************/
double fmean(double B[]);
/*********************************************************/

/*********************************************************/
// compute new x axis for B(alpha) -> B(lambda)
void alpha2lambda(double theta[], double lambda[], int j, int jp, int numpoints);
/*********************************************************/

/*********************************************************/
// parses data
// returns 0 if no new data
// returns 1 for new data chunk
// returns 2 for new element
int checkdatatype(char indata[BUFSIZE],double *xval, double *B_val, double *xaltval, int Btype);
/*********************************************************/

/*********************************************************/
// calculates average, variance, and standard deviation for B(theta)
struct stats expvals(double theta[],double B[],int numpoints);
/*********************************************************/

/*********************************************************/
// normalize B's
void normBtheta(double theta[],double B[],double Bnorm[],int numpoints);
void normBlambda(double lambda[],double B[],double Bnorm[],int numpoints);
/*********************************************************/

/*********************************************************/
// quadratic formula
void quadformula(double a, double b, double c, double *x1, double *x2);
/*********************************************************/

/*********************************************************/
struct stats{
    double avg;
    double var;
    double std;
};
/*********************************************************/

/*********************************************************/
#endif
/*********************************************************/
