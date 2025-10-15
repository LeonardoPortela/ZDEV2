FUNCTION-POOL zhcmt_py_0005              MESSAGE-ID sv.

* INCLUDE LZHCMT_PY_0005D...                 " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzhcmt_py_0005t00                       . "view rel. data dcl.

DATA: t_py0005 TYPE TABLE OF zhcmt_py_0005,
      w_py0005 TYPE zhcmt_py_0005.
