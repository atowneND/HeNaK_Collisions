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
    }
        if (j > jp){
            int jtmp = j;
            j = jp;
            jp = jtmp;
        }
//    printf("j = %i;\tjp = %i;\n",j,jp);
    sprintf(inBtheta,"Btheta_%i_%i.dat",j,jp);
    sprintf(inBlambda,"Blambda_%i_%i.dat",j,jp);

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
    int Btype = 0;
    int numAngles = readBfile(inBtheta,thetavec,Bthetavec,NULL,Btype);

    /*********************************************************/
    Btype = 1;
    int numLambdas = readBfile(inBlambda,lambdavec,Blambdavec,theta_lvec,Btype);

    /*********************************************************/
    // get statistics
    struct stats thetaStats = expvals(thetavec,Bthetavec,numAngles);

    int bak,new;
    char statFile[100];
    sprintf(statFile,"stats_%i_%i.dat",j,jp);

    // redirect stdout to file
    // from http://stackoverflow.com/questions/4832603/how-could-i-temporary-redirect-stdout-to-a-file-in-a-c-program
    fflush(stdout);
    bak = dup(1);
    new = open(statFile,O_RDWR|O_CREAT|O_TRUNC,0666);
    dup2(new,1);
    close(new);
    
    // write to file
    printf("# j=%i\tjp=%i\n",j,jp);
    printf("Stats: (deg)\n");
    printf("\tavg = %f\n\tvar = %f\n\tstd = %f\n",thetaStats.avg*180/PI,thetaStats.var*180/PI,thetaStats.std*180/PI);

    // finish stdout redirection
    fflush(stdout);
    dup2(bak,1);
    close(bak);

    /*********************************************************/
    // normalize B's
    double Bthetanorm[numAngles];
    normBtheta(thetavec,Bthetavec,Bthetanorm,numAngles);
    double Blambdanorm[numLambdas];
    normBlambda(lambdavec,Blambdavec,Blambdanorm,numLambdas);

    /*********************************************************/
    // write B(theta) to file
    // need to delete old file or overwrite new file
    char thetaOutput[100];
    sprintf(thetaOutput,"normBtheta_%i_%i.dat",j,jp);
    printf("writing B(theta) to %s\n",thetaOutput);

    // redirect stdout to file
    // from http://stackoverflow.com/questions/4832603/how-could-i-temporary-redirect-stdout-to-a-file-in-a-c-program
    fflush(stdout);
    bak = dup(1);
    new = open(thetaOutput,O_RDWR|O_CREAT|O_TRUNC,0666);
    dup2(new,1);
    close(new);

    // print header
    printf("# theta, B(theta)/B(1)\n");
    printf("# j=%i\tjp=%i\n",j,jp);
    printf("# theta (rad)\tB(theta)\n");

    // write to file theta and B
    for (ctr=0;ctr<numAngles;ctr++){
        printf("%lf\t%lf\n",thetavec[ctr],Bthetanorm[ctr]);
    }

    // finish stdout redirection
    fflush(stdout);
    dup2(bak,1);
    close(bak);

    /*********************************************************/
    // write B(lambda) to file
    // need to delete old file or overwrite new file
    char lambdaOutput[100];
    sprintf(lambdaOutput,"normBlambda_%i_%i.dat",j,jp);
    printf("writing B(lambda) to %s\n",lambdaOutput);

    // redirect stdout to file
    // from http://stackoverflow.com/questions/4832603/how-could-i-temporary-redirect-stdout-to-a-file-in-a-c-program
    fflush(stdout);
    bak = dup(1);
    new = open(lambdaOutput,O_RDWR|O_CREAT|O_TRUNC,0666);
    dup2(new,1);
    close(new);

    // print header
    printf("# lambda, B(lambda)/B(1)\n");
    printf("# j=%i\tjp=%i\n",j,jp);
    printf("# lambda\tB(lambda)\ttheta (deg)\n");

    // write to file lambda, B, theta
    for (ctr=0;ctr<numLambdas;ctr++){
        printf("%lf\t%lf\t%lf\n",lambdavec[ctr],Blambdanorm[ctr],theta_lvec[ctr]);
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
