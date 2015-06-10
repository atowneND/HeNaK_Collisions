/*********************************************************/
#include "parsedata.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <gsl_integration.h>
/*********************************************************/

/*********************************************************/
// parse data
//int checkdatatype(char indata[BUFSIZE],float *theta, float *B_theta){
int checkdatatype(char indata[BUFSIZE],int *theta, float *B_theta){
    int ctr = 0;
    char firstchar;
    while (isspace(indata[ctr])){
        ctr = ctr + 1;
    }

    float foo;
    firstchar = indata[ctr];
    switch (firstchar){
        case '#':
//            printf("Comment line\n");
            break;
        default: 
            // parse data line
            sscanf(indata,"%i %f %f",theta,B_theta,foo);
            printf("1theta = %i\tB = %f\telse = %f\n",*theta,*B_theta,foo);
            break;
    }

    return 0;
}
/*********************************************************/

/*********************************************************/
// calculate expectation value
float expectationvalue(struct alldata *datastruct){
    // for one data chunk
    struct alldata *foostruct = malloc(sizeof(struct alldata));
    foostruct->jmin = 3;
    foostruct->jmax = 5;
    foostruct->maxlamb = 2;
    foostruct->djcount = 1;

    foostruct->jset[foostruct->djcount] = malloc(foostruct->djcount*sizeof(struct datachunk *)); // pointer to struct datachunk
    (foostruct->jset[0])->recordcount = 1;
    printf("number of records = %i\n",(foostruct->jset[0])->recordcount);
    (foostruct->jset[0])->rec = malloc(sizeof(struct arecord *)); // pointer to a record structure

    ((foostruct->jset[0])->rec)->lambda = 2;
    ((foostruct->jset[0])->rec)->blam = 1.25;
    ((foostruct->jset[0])->rec)->angle = 90;

    free(foostruct);
    return 0;
}
/*********************************************************/
