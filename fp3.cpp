#include <stdio.h>
#include <math.h>
#include "lib.h"

struct FixedPoint
{
  uint8 digits[9];
  uint8 base;
};

struct Decimal
{
  uint8 significand[9]; // 0-th place is not used
  int8 exponent;
};

double
convert_fixed_to_float(struct FixedPoint* fp)
{
  double result = 0;

  for(int i = 8; i > 0; i--)
  {
    result += (double)fp->digits[i] * pow((double)fp->base, -i);
  }
  return result;
}

struct FixedPoint
convert_fixed_point_base(struct FixedPoint* b_number, uint8 B)
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

double
log_b(int base, double x)
{
  return log(x)/log(base);
}

char
digit_to_ascii(int digit)
{
  switch(digit)
  {
    case 0 : return '0';
    case 1 : return '1';
    case 2 : return '2';
    case 3 : return '3';
    case 4 : return '4';
    case 5 : return '5';
    case 6 : return '6';
    case 7 : return '7';
    case 8 : return '8';
    case 9 : return '9';
  }
  return '?';
}

void
print_dec(struct Decimal* dec, char* buf)
{
  int pos = 0;
  buf[pos++] = '0';
  buf[pos++] = '.';
  for(int i = 1; i <= 8; i++)
  {
    buf[pos++] = digit_to_ascii(dec->significand[i]);
  }
  buf[pos++] = 'E';
  pos += sprintf(&buf[pos], "%d", dec->exponent);
  buf[pos] = '\0';
}

struct Decimal
fp_bin_to_dec(float v)
{
  struct Decimal dec_number = {0};

  int8 x = (int8)(floor(log_b(10, v)) + 1);
  dec_number.exponent = x;

  float B_power_x = (float)pow(10, -x);
  float v_prime = v * B_power_x;
  //assert(1/B <= v_prime && v_prime < 1);

  int k = 0;
  double R = v_prime;
  double M = pow(2.0, -23) / 2.0; // -23 is the width of the significand of single precision binary FP
  uint8 U;

  do
  {
    k++;
    U = (uint8)(R * 10);
    R = (R * 10) - U;
    M = M * 10;
    dec_number.significand[k] = U;
  }
  while(R >= M && R <= (1 - M));

  if(R <= 0.5)
    dec_number.significand[k] = U;
  else
    dec_number.significand[k] = U + 1;

  return dec_number;
}

int
main()
{
  /*
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
  */

  /*
  float float_pt = convert_fixed_to_float(&b_number);
  printf("%.8f", float_pt);
  */

  struct Decimal dec = fp_bin_to_dec(300933.0f);

  char buf[100] = {0};
  print_dec(&dec, buf);
  puts(buf);
  return 0;
}

