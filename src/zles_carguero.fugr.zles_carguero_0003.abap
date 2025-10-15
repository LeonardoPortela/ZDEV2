FUNCTION ZLES_CARGUERO_0003.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VIAGEM_ID) TYPE  ZDE_VIAGEM_ID OPTIONAL
*"----------------------------------------------------------------------

  IF I_VIAGEM_ID IS INITIAL.
    SUBMIT ZLESR0135 AND RETURN VIA SELECTION-SCREEN.
  ELSE.
    PERFORM ABRIR_VIAGEM(ZLESR0135) USING I_VIAGEM_ID ABAP_FALSE.
  ENDIF.

ENDFUNCTION.
