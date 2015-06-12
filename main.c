/*********************************************************/
// main.c
// expected value of alpha
// Ashley Towne
// 06/09/2015
// calculates expected value of alpha
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
    // get inputs
    char *indatadir;
    char *outfiledir;
    if (argc<2){
        printf("usage: ./main <filename>\n");
        printf("Please enter the name of the file that contains the data\n");
        printf("Exiting program\n");
        exit(1);
    }
    else{
        indatadir = argv[1];
        if (argc>2){
            outfiledir = argv[2];
        }
        else{
            outfiledir = "renameThisFile_Output.dat";
        }
    }

    // open file
    FILE *fd = fopen(indatadir,"r");
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
        printf("Reading %s\n",indatadir);
        while (fgets(indata,BUFSIZE,fd)!=NULL){
            // check if it's a comment
            typeflag = checkdatatype(indata, theta, B_theta);
            
            if (!typeflag){
                // resize array of dat
                thetavec = realloc(thetavec,(ctr + 1)*sizeof(double));
                Bvec = realloc(Bvec,(ctr + 1)*sizeof(double));

                // assign values to arrays
                thetavec[ctr] = (*theta)*PI/180;
                Bvec[ctr] = *B_theta;

//                printf("theta[%i] = %f\n",ctr,thetavec[ctr]);
                // increment to prepare for next array value
                ctr = ctr + 1;
            }
        }
    }
    int numpoints = ctr;

    fclose(fd);

    // get statistics
    struct stats thetaStats = expvals(thetavec,Bvec);

    printf("#Stats:\n");
    printf("#\tavg = %f\n\tvar = %f\n\tstd = %f\n",thetaStats.avg,thetaStats.var,thetaStats.std);

    // convert alpha to lambda
    double lambda[numpoints];
    int j,jp;
    j = 18;
    jp = 16;
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
        printf("%f\t%f\n",lambda[ctr],(Bvec[ctr]*sin(thetavec[ctr]))/(Bvec[1])*sin(thetavec[ctr]));
    }

    // finish stdout redirection
    fflush(stdout);
    dup2(bak,1);
    close(bak);

    // clean up
    free(theta);
    free(B_theta);
    free(thetavec);
    free(Bvec);

    return 0;
}
/*********************************************************/
