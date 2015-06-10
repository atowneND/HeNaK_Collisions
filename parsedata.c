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
int globalfoo=0;
int checkdatatype(char indata[BUFSIZE], struct alldata *datastruct){
    globalfoo = globalfoo + 1;
    if(globalfoo >= 48184){
        return 3;
    }
    int ctr = 0;
    char firstchar;
    while (isspace(indata[ctr])){
        ctr = ctr + 1;
    }

    firstchar = indata[ctr];
    switch (firstchar){
        case '#':
//            printf("Comment line\n");
            break;
        case 'j':
//            printf("delta j line\n");
            if (!(indata[ctr+1]==' ')){
                // GENERAL DATA

                // declare tmp variables
                char *foo = malloc(100);
                char *bar = malloc(100);
                int jmin,jmax,lambdamax;
                
                // parse data line
                sscanf(indata,"%s %s %i %i %i",foo,bar,&jmin,&jmax,&lambdamax);

                // save into data structure
                datastruct->jmin = jmin;
                datastruct->jmax = jmax;
                datastruct->maxlamb = lambdamax;
                datastruct->djcount = 0;

                // clean up
                free(foo);
                free(bar);

                // general data, no new elements or j values
                return 0;
            }
            else{
                // create new module of data
                // find the two integers and save to this module's data
            }
            break;
        case 'l':
//            printf("header line\n");
            break;
        default:
//            printf("dataline\n");
            // add new record to data module
            // scan three numbers (%i,%f,%f)
            // save to record of data module
            break;
    }

    return 1;
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
