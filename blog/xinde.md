# scheme 宏语法
## #' #` #, #,@ 的区别
这几个句法都用在宏定义里。其中 #' 和#`用法差不多的，都是用于定义宏展开之后的样子。他们的区别在于前者先解析，然后才轮到后者。前者可以出现在后者的子句里，而后者不能出现在前者的子句了。#, 和 #,@也同样只能出现在#'的子句里。前者会求值表达式，并把结果替换回去，后者用于替换list或vec。
