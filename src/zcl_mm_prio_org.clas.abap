class ZCL_MM_PRIO_ORG definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_MM_PRIO_ORG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_PRIO_ORG IMPLEMENTATION.


  method IF_EX_BADI_MM_PRIO_ORG~MRP_PRIO_ORG_DETERMINE.
  endmethod.


  method IF_EX_BADI_MM_PRIO_ORG~PM_PRIO_ORG_DETERMINE.
  endmethod.


  method IF_EX_BADI_MM_PRIO_ORG~PO_PRIO_ORG_DETERMINE.
  endmethod.


  method IF_EX_BADI_MM_PRIO_ORG~PR_PRIO_ORG_DETERMINE.
  endmethod.


  method IF_EX_BADI_MM_PRIO_ORG~RES_PRIO_ORG_DETERMINE.
  endmethod.
ENDCLASS.
