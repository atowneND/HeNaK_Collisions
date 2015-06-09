#include "parsedata.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>

int checkdatatype(char indata[BUFSIZE]){
    int ctr = 0;
    char firstchar;
    while (isspace(indata[ctr])){
        ctr = ctr + 1;
    }

    firstchar = indata[ctr];
    switch (firstchar){
        case '#':
            printf("Comment line\n");
            break;
        case 'j':
            printf("delta j line\n");
            if (!(indata[ctr+1]==' ')){
                printf("\tgeneral data\n");
                // save overall data for the whole file
                char foo[100];
                char bar[100];
                int jmin,jmax,lambdamax;
                // sscanf doesn't work
            //    sscanf(indata,"%s %s %i %i %i",foo,bar,jmin,jmax,lambdamax);
            }
            else{
                // create new module of data
                // find the two integers and save to this module's data
            }
            break;
        case 'l':
            printf("header line\n");
            break;
        default:
            printf("dataline\n");
            // add new record to data module
            // scan three numbers (%i,%f,%f)
            // save to record of data module
            break;
    }

    return 0;
}
