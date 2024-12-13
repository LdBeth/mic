char foo = 1;
char a() {
  char a, b;
  a = 1;
  b = 2;
  if (a < b) goto L1;
  return 2;
 L1:
  return 3;
}
