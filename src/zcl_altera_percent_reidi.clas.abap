class ZCL_ALTERA_PERCENT_REIDI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_BAPI_PO_CREATE_02 .
  interfaces IF_EX_ME_BAPI_PO_CREATE_01 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ALTERA_PERCENT_REIDI IMPLEMENTATION.


  METHOD if_ex_me_bapi_po_create_01~extensionin.

  ENDMETHOD.


  method IF_EX_ME_BAPI_PO_CREATE_01~EXTENSIONOUT.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_01~INBOUND.

  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_01~OUTBOUND.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_01~PARTNERS_ON_ITEM_ACTIVE.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_01~RENUMBERING.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_01~TEXT_OUTPUT.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_01~TEXT_SELECTION.
  endmethod.


  METHOD if_ex_me_bapi_po_create_02~extensionin.

    IF extensionin IS NOT INITIAL.
      DATA(lt_extensionin) = extensionin.

      SORT lt_extensionin BY valuepart2.

      READ TABLE lt_extensionin ASSIGNING FIELD-SYMBOL(<fs_extensionin>)
      WITH KEY valuepart2 = poitem-ebelp
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF <fs_extensionin>-structure = 'EKPO' AND <fs_extensionin>-valuepart1 EQ 'PERCENTUAL-REIDI'.
          poitem-reidi = abap_true.
          REPLACE FIRST OCCURRENCE OF ',' IN <fs_extensionin>-valuepart3(14) WITH '.'.
          poitem-aliquota = <fs_extensionin>-valuepart3(14).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method IF_EX_ME_BAPI_PO_CREATE_02~EXTENSIONOUT.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_02~INBOUND.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_02~MAP2E_EXTENSIONOUT.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_02~MAP2I_EXTENSIONIN.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_02~OUTBOUND.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_02~PARTNERS_ON_ITEM_ACTIVE.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_02~TEXT_OUTPUT.
  endmethod.


  method IF_EX_ME_BAPI_PO_CREATE_02~TOGGLE_ORDER_UNIT.
  endmethod.
ENDCLASS.
