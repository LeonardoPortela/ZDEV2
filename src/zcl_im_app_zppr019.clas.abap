class ZCL_IM_APP_ZPPR019 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BOM_UPDATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_APP_ZPPR019 IMPLEMENTATION.


  METHOD IF_EX_BOM_UPDATE~CHANGE_AT_SAVE.
* Ampliação para preencher a tabela ZPPT0025 - Transação ZPP0022
      INCLUDE ZIPP_ZPPR019_EXIT if FOUND.
  ENDMETHOD.


  METHOD IF_EX_BOM_UPDATE~CHANGE_BEFORE_UPDATE.

  ENDMETHOD.


  METHOD IF_EX_BOM_UPDATE~CHANGE_IN_UPDATE.
  ENDMETHOD.


  method IF_EX_BOM_UPDATE~CREATE_TREX_CPOINTER.
  endmethod.
ENDCLASS.
