#include <stdio.h>
#include <math.h>
#include "lib.h"

struct FixedPoint
{
  uint8 digits[9];
  uint8 base;
};

double convert_fixed_to_float(struct FixedPoint* fp)
{
  double result = 0;

  for(int i = 8; i > 0; i--)
  {
    result += (double)fp->digits[i] * pow((double)fp->base, -i);
  }
  return result;
}

struct FixedPoint convert_fixed_point_base(struct FixedPoint* b_number, uint8 B)
{
  struct FixedPoint B_number = {0};
  B_number.base = B;

  int k = 0;
  double R = convert_fixed_to_float(b_number);
  double M = pow(b_number->base, -8.0) / 2.0;
  uint8 U;

  do
  {
    k++;
    U = (uint8)(R * B);
    R = (R * B) - U;
    M = M * B;
    B_number.digits[k] = U;
  }
  while(R >= M && R <= (1 - M));

  if(R <= 0.5)
    B_number.digits[k] = U;
  else
    B_number.digits[k] = U + 1;

  return B_number;
}

int main()
{
  struct FixedPoint b_number = {0};
  b_number.base = 2;
  b_number.digits[1] = 1;
  b_number.digits[2] = 0;
  b_number.digits[3] = 1;
  b_number.digits[4] = 0;
  b_number.digits[5] = 1;
  b_number.digits[6] = 1;
  b_number.digits[7] = 1;
  b_number.digits[8] = 1;

  printf("b format : %.8f\n", convert_fixed_to_float(&b_number));

  struct FixedPoint B_number = convert_fixed_point_base(&b_number, 10);
  printf("B format : %.8f\n", convert_fixed_to_float(&B_number));

  /*
  float float_pt = convert_fixed_to_float(&b_number);
  printf("%.8f", float_pt);
  */

  return 0;
}

