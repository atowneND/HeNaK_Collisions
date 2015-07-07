#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// gcc test.c -lm

int getmin(int x, int y);
int getmax(int x, int y);

int main(){
    int N=10;
    int a, b, j, jp, jmin;
    float jtest,j2test,m,mp;

    printf("#j jp m mp\n");
    for (a=1;a<N;a++){
        for (b=1;b<N;b++){
            jtest = -0.5+0.5*sqrt(1-4*(1-pow(a,2))/pow(b,2));
            j2test = -0.5-0.5*sqrt(1-4*(1-pow(a,2))/pow(b,2));
            jp = pow(a,2) - 1;

            if ((jtest-(int)jtest == 0)&&(jtest>0)){
                j = (int)jtest;

                for (m=-getmax(j,jp);m<=getmax(j,jp);m++){
                    mp=m/(double)(a*b);
                    if (mp-(int)mp==0){
                        printf("%i %i %i %i\n",j,jp,(int)mp,(int)m);
                    }
                }
            }
            else if((j2test-(int)j2test ==0)&&(j2test>0)){
                j = (int)jtest;

                for (m=-getmax(j,jp);m<=getmax(j,jp);m++){
                    mp=m/(double)(a*b);
                    if (mp-(int)mp==0){
                        printf("%i %i %i %i\n",j,jp,(int)mp,(int)m);
                    }
                }
            }
        }
    }

    return 0;
}

int getmin(int x, int y){
    int minval;
    if (x<y){
        minval = x;
    }
    else{
        minval = y;
    }
    return minval;
}

int getmax(int x, int y){
    int maxval;
    if (x>y){
        maxval = x;
    }
    else{
        maxval = y;
    }
    return maxval;
}
