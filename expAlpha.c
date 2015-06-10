/*********************************************************/
// expAlpha.c
// expected value of alpha
// Ashley Towne
// 06/09/2015
// extracts B(alpha) from look.pcc12
// calculates expected value of alpha
/*********************************************************/

/*********************************************************/
// includes
#include <stdio.h>
#include <stdlib.h>

#include "parsedata.h"
/*********************************************************/

/*********************************************************/
// main
int main(){
    char *datadir = "./look.pcc12"; // from Prof. Hickman's data
    FILE *fd = fopen(datadir,"r");
    char indata[BUFSIZE];

    // read in data
    if (fd==NULL){
        // error checking
        printf("ERROR: fopen (6)\n");
        exit(1);
    }
    else{
        // declare data structure for all data
        struct alldata test;
        test.jmin = 0;
        test.jmax = 0;
        test.maxlamb = 0;
        test.djcount = 0;
        struct alldata *datastruct = &test;

        // read in one line of data
        while (fgets(indata,BUFSIZE,fd)!=NULL){
            // check if comment or metadata
            checkdatatype(indata,datastruct);
        }
        float foo = expectationvalue(datastruct);
    }

    fclose(fd);

    // calculate expectation value of alpha

    return 0;
}
/*********************************************************/
