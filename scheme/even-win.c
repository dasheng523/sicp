#ifdef WIN32
#define EXPORT extern __declspec (dllexport)
#else
#define EXPORT extern
#endif

EXPORT int even(n) int n; { return n == 0 || odd(n - 1); }
