class ZCL_MEMORY_NFE_INBOUND_HANDLE definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  data AT_NFE_INBOUND type ZIB_NFE_DIST_TER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MEMORY_NFE_INBOUND_HANDLE IMPLEMENTATION.


  METHOD IF_SHM_BUILD_INSTANCE~BUILD.
  ENDMETHOD.
ENDCLASS.
