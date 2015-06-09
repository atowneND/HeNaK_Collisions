#ifndef PARSEDATA_H_
#define PARSEDATA_H_

#define BUFSIZE 1000
// structure for all data
struct alldata{
    int jmin;
    int jmax;
    int maxlamb;
    int djcount;
    struct datachunk **jset; // points to an array of j/jp cominations
};

// structure for each combination of j an jp
struct datachunk{
    int recordcount;
    struct arecord **rec; // points to an array of records 
};

// structure for each record (for each lambda)
struct arecord{
    int lambda;
    float blam;
    float angle;
};

int checkdatatype(char indata[BUFSIZE]);

#endif
