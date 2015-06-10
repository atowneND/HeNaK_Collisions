/*********************************************************/
#ifndef PARSEDATA_H_
#define PARSEDATA_H_

#define BUFSIZE 1000

/*********************************************************/
// structure for all data
struct alldata{
    int jmin;
    int jmax;
    int maxlamb;
    int djcount; // number of data chunks (number of delta j's)
    //struct datachunk **jset; // points to an array of j/jp cominations
    struct datachunk *jset[]; // points to a single j/jp comination
};
/*********************************************************/

/*********************************************************/
// structure for each combination of j an jp
struct datachunk{
    int recordcount;
    //struct arecord **rec; // points to an array of records 
    struct arecord *rec; // points to a single record
};
/*********************************************************/

/*********************************************************/
// structure for each record (for each lambda)
struct arecord{
    int lambda;
    float blam;
    float angle;
};
/*********************************************************/

/*********************************************************/
// parses data
// returns 0 if no new data
// returns 1 for new data chunk
// returns 2 for new element
//int checkdatatype(char indata[BUFSIZE],float *theta, float *B_theta);
int checkdatatype(char indata[BUFSIZE],int *theta, float *B_theta);

// calculates expectation value of alpha
float expectationvalue(struct alldata *datastruct);
/*********************************************************/

/*********************************************************/
#endif
/*********************************************************/
