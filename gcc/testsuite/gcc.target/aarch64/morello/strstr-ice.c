/* { dg-do compile } */
inline char *strstr(const char *__haystack, const char *__needle) {
  return __builtin_strstr(__haystack, __needle);
}
char *f() {
  const char *p = "a";
  return strstr("", p);
}
