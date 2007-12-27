#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"

int WIDTH = 1;
int XA = 0;
int XB = 16;
int YA = 0;
int YB = 16;
int Z1 = 0;
int Z2 = 0;
int Z3 = 0;
int Z4 = 0;
int XADD = 10; // move x axis left on screen
int YADD = -8; // lower y axis view
int LEVEL_UNDEFINED = 99999;
int LEVEL_WATER = -1;
float steep = 0;
float sealevel = 0;
char buf2[MAX_STRING_LENGTH];
char pic0;
char *picType;
int *pic;
/** generate a map - mord**/
//srand( (unsigned int)time( NULL ) );
float randomer(void)
{
  return (float)number(0, 10000)/10000.0;
}






char *ZColor( int z0)
{
  int t1;
  const char *inv = "";
  static char buf[20];
  if ( z0 == LEVEL_WATER )
  {
    snprintf(buf, sizeof(buf), "{cCS{c0");
    return buf;
  }
  if ( z0 == LEVEL_UNDEFINED )
  {
    buf[0] = '\0';
    buf[1] = '\0';
    return buf;
  }
  t1 = z0 % 10;
  
  if (t1 < 0) {
  inv = "{cV";
  t1 *= -1;
  }
  
  switch (t1)
  {
  case 0:
    snprintf(buf, sizeof(buf), "%s{cc%d{c0",inv, t1);
    break;
  case 1:
    snprintf(buf, sizeof(buf), "%s{cb%d{c0",inv, t1);
    break;
  case 2:
    snprintf(buf, sizeof(buf), "%s{cB%d{c0",inv, t1);;
    break;
  case 3:
    snprintf(buf, sizeof(buf), "%s{cg%d{c0",inv, t1);
    break;
  case 4:
    snprintf(buf, sizeof(buf), "%s{cG%d{c0",inv, t1);
    break;
  case 5:
    snprintf(buf, sizeof(buf), "%s{cY%d{c0",inv, t1);
    break;
  case 6:
    snprintf(buf, sizeof(buf), "%s{cy%d{c0",inv, t1);
    break;
  case 7:
    snprintf(buf, sizeof(buf), "%s{cL%d{c0",inv, t1);
    break;
  case 8:
    snprintf(buf, sizeof(buf), "%s{cw%d{c0",inv, t1);
    break;
  case 9:
    snprintf(buf, sizeof(buf), "%s{cW%d{c0",inv, t1);
    break;
  default:
    snprintf(buf, sizeof(buf), "%s{cP%d{c0",inv, t1);
    break;
  }

  //buf[0] = t1;
  //buf[1] = '\0';
  return buf;
}

int round_it(float num)
{
  return (int)(num + 0.5);
}

void frac(int x0,int y0,int x2,int y2,int z0,int z1,int z2,int z3)
{
  int xmid, ymid, z01, z12, z23, z30, newz;
  //  50% chance rise or descend
  if ( randomer() < 0.5 )
  {
    newz = round_it( (z0+z1+z2+z3) / 4) + round_it( randomer() * (y2-y0) * steep );
  }
  else
  {
    newz = round_it( (z0+z1+z2+z3) / 4) - round_it( randomer() * (y2-y0) * steep );
  }

  xmid = ( x0 + x2) >> 1;
  ymid = ( y0 + y2) >> 1;
  z12 =  ( z1 + z2) >> 1;
  z30 =  ( z3 + z0) >> 1;
  z01 =  ( z0 + z1) >> 1;
  z23 =  ( z2 + z3) >> 1;

  if ( (( x2 - x0 ) > WIDTH ) && (( y2 - y0 ) > WIDTH ))
  {
    frac( x0, y0, xmid, ymid, z0, z01, newz, z30);
    frac( xmid, y0, x2, ymid, z01, z1, z12, newz);
    frac( x0, ymid, xmid, y2, z30, newz, z23, z3);
    frac( xmid, ymid, x2, y2, newz, z12, z2, z23);
  }
  else
  {
    if ( newz <= sealevel )
    {
      // above sea level
      picType[ymid*YB+xmid] = 'l';
      pic[ymid*YB+xmid] = newz;
    }
    else
    {
      //  below "sea level"
      picType[ymid*YB+xmid] = 's';
      pic[ymid*YB+xmid] = LEVEL_WATER;
    }
  }
}


char * landscape(int height, int width)
{
  int i,j;
  for (i = 0; i < XB; i++)
  {
    for (j = 0; j < YB; j++)
    {
      picType[j*YB+i] = 'u';
    }
  }
  for (i = 0; i < XB; i++)
  {
    for (j = 0; j < YB; j++)
    {
      pic[j*YB+i] = LEVEL_UNDEFINED;
    }
  }

  steep = ( randomer() / 2.5 ) + 0.5;
  sealevel = round_it( 17 * randomer() ) - 8;

  Z1 = round_it( 15 * randomer() ) - 7;
  Z2 = round_it( 15 * randomer() ) - 7;
  Z3 = round_it( 15 * randomer() ) - 7;
  Z4 = round_it( 15 * randomer() ) - 7;

  frac( XA, YA, XB, YB, Z1, Z2, Z3, Z4);

  for (i = 0; i < XB; i++)
  {
    for (j = 0; j < YB; j++)
    {
      pic0 = LEVEL_UNDEFINED;
      if ( picType[j*YB+i] == 'l' )
      {
        pic0 = abs((int)(pic[j*YB+i] - sealevel));
      }
      if ( picType[j*YB+i] == 's' )
      {
        pic0 = LEVEL_WATER;
      }

      strlcat(buf2, ZColor( pic0 ), sizeof(buf2));
    }
    strlcat(buf2, "\n", sizeof(buf2));
  }
  return buf2;
}


ACMD(do_landscape)
{
  picType = (char *) alloca((unsigned int)((XB*YB+YB)*sizeof(char)));
  pic = (int *) alloca((unsigned int)((XB*YB+YB)*sizeof(int)));
  memset(pic, 0, (XB*YB+YB)*sizeof(int));
  memset(picType, 0, (XB*YB+YB)*sizeof(char));
  *buf2 ='\0';

  ch->Send( "%s", landscape(XB, YB));
}



