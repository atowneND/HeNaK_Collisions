#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// gcc test.c -lm

int getmin(int x, int y);
int getmax(int x, int y);
void find_zero_delta_theta_states(int N);
void powerpoint_timing(void);

int main(){
    int N=10;
    find_zero_delta_theta_states(N);
    powerpoint_timing();
    return 0;
}

void powerpoint_timing(void){
    double ppt_structure[] = {9, 2, 1, 2, 2, 1};
    char *key[] = {"intro","data","results","theory","data","results"};

    int N = sizeof(ppt_structure)/sizeof(double);
    double num_slides;
    double time_per_slide;
    double total_time = 12; // total time in minutes
    int i;

    for (i=0;i<N;i++){
        num_slides = num_slides + ppt_structure[i];
    }
    printf("num_slides=%f\n",num_slides);

    for (i=0;i<N;i++){
        time_per_slide = ppt_structure[i]*total_time/num_slides;
        printf("%s:\t%f\n",key[i],time_per_slide);
    }
}

void find_zero_delta_theta_states(int N){
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
