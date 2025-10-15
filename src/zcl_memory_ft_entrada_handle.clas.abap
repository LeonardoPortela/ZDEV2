class ZCL_MEMORY_FT_ENTRADA_HANDLE definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  data AT_ZLEST0108 type ZLEST0108 .
  data AT_LIKP type TAB_LIKP .
  data AT_LIPS type TAB_LIPS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MEMORY_FT_ENTRADA_HANDLE IMPLEMENTATION.


  method IF_SHM_BUILD_INSTANCE~BUILD.
  endmethod.
ENDCLASS.
