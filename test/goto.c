int main(argc, argv) {
  int argc;
  char **argv;
  char/**/a,b;
  a = 1;
  b = 2;
  if (a < b) goto L1;
  return 2;
 L1: // Welp
  return 3;
}
