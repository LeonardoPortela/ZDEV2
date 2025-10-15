class ZCL_IM_TESTE12 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BUPA_GENERAL_UPDATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_TESTE12 IMPLEMENTATION.


  METHOD if_ex_bupa_general_update~change_before_update.



    CONSTANTS: c_resbd(32) TYPE c VALUE '(SAPLBUS_LOCATOR)GT_BUT0BK-BANKL'.

    FIELD-SYMBOLS: <fs_resbd> TYPE BANKL.

    ASSIGN (c_resbd) TO <fs_resbd>.


  ENDMETHOD.
ENDCLASS.
