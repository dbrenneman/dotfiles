(clazz
  (classBody) @class.inside) @class.around

(typeAlias) @class.around

(classMethod
  body: (_) @function.inside) @function.around

(objectMethod
  body: (_) @function.inside) @function.around

(parameterList
  (typedIdentifier) @parameter.inside) @parameter.around

[
  (lineComment)
  (blockComment)
  (docComment)
] @comment.inside

(lineComment)+ @comment.around
(blockComment) @comment.around
(docComment) @comment.around
