/*********************************************************/
// expAlpha.c
// expected value of alpha
// Ashley Towne
// 06/09/2015
// extracts B(alpha) from look.pcc12
// calculates expected value of alpha
/*********************************************************/

// includes
#include <stdio.h>
#include <stdlib.h>

#include "parsedata.h"

// main
int main(){
    char *datadir = "./look.pcc12"; // from Prof. Hickman's data
    FILE *fd = fopen(datadir,"r");
    char indata[BUFSIZE];

    int ctr = 0;
    
    // error checking
    if (fd==NULL){
        printf("ERROR: fopen (6)\n");
        exit(1);
    }
    else{
        // read in one line of data
        while (fgets(indata,BUFSIZE,fd)!=NULL){
            // check if comment or metadata
            checkdatatype(indata);
            ctr = ctr + 1;
            if (ctr>10)
                break;
        }
    }

    fclose(fd);
    return 0;
}
