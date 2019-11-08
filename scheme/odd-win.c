#ifdef WIN32
#define EXPORT extern __declspec (dllexport)
#else
#define EXPORT extern
#endif

EXPORT int odd(n) int n; { return n != 0 && even(n - 1); }
