int main(argc, argv)
     int argc;
     char **argv;
{
  char /**/a,b;
  a = 0x0a + 1;
  b = 0;
 L1: b++;
  if (b < a) goto L1;
  if (1 < 2) goto L2;
  return a + b;
 L2: return 0;
}
