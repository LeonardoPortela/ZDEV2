class ZCL_IM_BADI_SCD_CREATE_CHC definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BADI_SCD_CREATE_CHC
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BADI_SCD_CREATE_CHCK .
protected section.
*"* protected components of class ZCL_IM_BADI_SCD_CREATE_CHC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_BADI_SCD_CREATE_CHC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_BADI_SCD_CREATE_CHC IMPLEMENTATION.


METHOD IF_EX_BADI_SCD_CREATE_CHCK~CHECK_DOCUMENT_CREATE.

  IF I_REFOBJ-VTTKF-TKNUM IS NOT INITIAL.

    TRY .

        ZCL_DOC_CUSTO=>ZIF_DOC_CUSTO~GET_INSTANCE(
          )->GET_CK_VALIDA_VI_VT( I_VTTKF = I_REFOBJ-VTTKF
          ).

      CATCH ZCX_DOC_CUSTO INTO DATA(EX_DOC_CUSTO).

        MESSAGE ID EX_DOC_CUSTO->MSGID TYPE 'E' NUMBER EX_DOC_CUSTO->MSGNO
           WITH EX_DOC_CUSTO->MSGV1 EX_DOC_CUSTO->MSGV2 EX_DOC_CUSTO->MSGV3 EX_DOC_CUSTO->MSGV4
        RAISING CREATE_NO_DOCUMENT.

    ENDTRY.

  ENDIF.

ENDMETHOD.
ENDCLASS.
