/*********************************************************/
// main.c
// expected value of alpha
// Ashley Towne
// 06/09/2015
// calculates expected value of alpha given B(alpha)
// TO DO:
/*********************************************************/

/*********************************************************/
// includes
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "avgFun.h"
/*********************************************************/

/*********************************************************/
// main
int main(int argc, char *argv[]){
    /*********************************************************/
    // get inputs
    char inBtheta[100];
    char inBlambda[100];
    char *outfiledir;
    int j,jp;
    if (argc<2){
        printf("Enter j and jp\n");
        scanf("%i %i",&j,&jp);
    }
    else if(argc==3){
        j = atoi(argv[1]);
        jp = atoi(argv[2]);
        if (j < jp){
            int jtmp = j;
            j = jp;
            jp = jtmp;
        }
    }
    printf("j = %i;\tjp = %i;\n",j,jp);
    sprintf(inBtheta,"Btheta_%i_%i.dat",j,jp);
    printf("inBtheta = %s\n",inBtheta);
    sprintf(inBlambda,"Blambda_%i_%i.dat",j,jp);
    printf("inBlambda = %s\n",inBlambda);

    /*********************************************************/
    // initialize temporary pointers and flags and head of arrays
    int typeflag = 1;
    int ctr = 0;

    double *xtmp = malloc(sizeof(double *));
    double *Btmp = malloc(sizeof(double *));
    double *thetatmp = malloc(sizeof(double *));

    double *thetavec = (double *)malloc((ctr + 1)*sizeof(double));
    double *Bthetavec = (double *)malloc((ctr + 1)*sizeof(double));

    double *lambdavec = (double *)malloc((ctr + 1)*sizeof(double));
    double *Blambdavec = (double *)malloc((ctr + 1)*sizeof(double));
    double *theta_lvec = (double *)malloc((ctr + 1)*sizeof(double));

    /*********************************************************/
    // read Btheta file
    FILE *fdBtheta = fopen(inBtheta,"r");
    char indata[BUFSIZE];
    int Btype = 0;

    if (fdBtheta==NULL){
        // error checking
        printf("ERROR: unable to open %s\n",inBtheta);
        exit(1);
    }
    else{
        // read in one line of data
        printf("Reading %s\n",inBtheta);
        while (fgets(indata,BUFSIZE,fdBtheta)!=NULL){
            // check if it's a comment
            typeflag = checkdatatype(indata, xtmp, Btmp, thetatmp, Btype);
            
            if (!typeflag){
                // resize array of dat
                thetavec = realloc(thetavec,(ctr + 1)*sizeof(double));
                Bthetavec = realloc(Bthetavec,(ctr + 1)*sizeof(double));

                // assign values to arrays
                thetavec[ctr] = (*xtmp)*PI/180;
                Bthetavec[ctr] = *Btmp;

                // increment to prepare for next array value
                ctr = ctr + 1;
            }
        }
    }
    int numpoints = ctr;

    fclose(fdBtheta);

    /*********************************************************/
    FILE *fdBlambda = fopen(inBlambda,"r");
    Btype = 1;
    ctr = 0;

    if (fdBlambda==NULL){
        // error checking
        printf("ERROR: unable to open %s\n",inBlambda);
        exit(1);
    }
    else{
        // read in one line of data
        printf("Reading %s\n",inBlambda);
        while (fgets(indata,BUFSIZE,fdBlambda)!=NULL){
            // check if it's a comment
            typeflag = checkdatatype(indata, xtmp, Btmp, thetatmp, Btype);
            
            if (!typeflag){
                // resize array of dat
                lambdavec = realloc(lambdavec,(ctr + 1)*sizeof(double));
                Blambdavec = realloc(Blambdavec,(ctr + 1)*sizeof(double));
                theta_lvec = realloc(Blambdavec,(ctr + 1)*sizeof(double));

                // assign values to arrays
                lambdavec[ctr] = (*xtmp);
                Blambdavec[ctr] = *Btmp;
                theta_lvec[ctr] = *thetatmp;

                // increment to prepare for next array value
                ctr = ctr + 1;
            }
        }
    }

    fclose(fdBlambda);

    /*********************************************************/
    // get statistics
    struct stats thetaStats = expvals(thetavec,Bthetavec);

    printf("Stats: (deg)\n");
    printf("\tavg = %f\n\tvar = %f\n\tstd = %f\n",thetaStats.avg*180/PI,thetaStats.var*180/PI,thetaStats.std*180/PI);

    /*********************************************************/
    // convert alpha to lambda
    double lambda[numpoints];
    alpha2lambda(thetavec,lambda,j,jp,numpoints);
    
    // redirect stdout to file
    // from http://stackoverflow.com/questions/4832603/how-could-i-temporary-redirect-stdout-to-a-file-in-a-c-program
    int bak,new;
    fflush(stdout);
    bak = dup(1);
    new = open(outfiledir,O_RDWR|O_CREAT,0666);
    dup2(new,1);
    close(new);

    printf("# lambda, B(lambda)/B(1)\n");
    printf("# j=%i\tjp=%i\n",j,jp);
    // write to file lambda and B
    for (ctr=0;ctr<numpoints;ctr++){
        printf("%f\t%f\n",lambda[ctr],(Bthetavec[ctr]*sin(thetavec[ctr]))/(Bthetavec[1])*sin(thetavec[ctr]));
    }

    // finish stdout redirection
    fflush(stdout);
    dup2(bak,1);
    close(bak);

    /*********************************************************/
    // clean up
    free(xtmp);
    free(Btmp);
    free(thetavec);
    free(Bthetavec);

    return 0;
}
/*********************************************************/
