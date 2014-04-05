#include <stdio.h>
#include <float.h>
#include <math.h>
#define MAX_STEPS 10000000

float func (float x)
{
    return log(x);
}

float dich (float start, float end, float (*func) (float), int *step)
{
    float start2, end2;
    float f1 = func (start);
    float f2 = func (end);

    float middle = (end-start)/2 + start;
    float fm = func (middle);

    if ((*step == MAX_STEPS) || (fabs(fm) <= FLT_EPSILON)) return middle;

    if (f1*fm > 0)
    {
        start2 = middle;
        end2 = end;
    }
    else
    {
        start2 = start;
        end2 = middle;
    }
    (*step)++;
    return dich (start2, end2, func, step);
}

int main()
{
    int step = 0;
    int i;
    float res;
    for (i=0; i<100000; i++)
    {
        res = dich (0.001, i+1000.0, func, &step);
    }
    printf ("%f, %i\n", res, step);
    return 0;
}
