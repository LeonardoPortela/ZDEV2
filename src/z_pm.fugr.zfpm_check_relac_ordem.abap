FUNCTION zfpm_check_relac_ordem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AUART) TYPE  AUFART OPTIONAL
*"     VALUE(I_QMART) TYPE  QMART OPTIONAL
*"     VALUE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"  EXPORTING
*"     VALUE(RETURNLOG) TYPE  BAPIRETURN1
*"----------------------------------------------------------------------

  "Check relacion table zpmt0073.
  DATA: ws_zpmt0073 TYPE zpmt0073.

  CLEAR ws_zpmt0073.
  SELECT SINGLE * FROM zpmt0073
    INTO ws_zpmt0073
     WHERE auart EQ i_auart
     AND qmart EQ i_qmart
     AND werks EQ i_werks.

    IF SY-SUBRC NE 0.
    RETURNLOG = VALUE #(
    TYPE = 'E'
    MESSAGE = |Não é permitido esse tipo de nota { i_qmart } Para o tipo de ordem { i_auart }|
    ).
    ELSE.
    RETURNLOG = VALUE #(
    TYPE = 'S'
    MESSAGE = |Tipo de nota permitido com sucesso.|
    ).
    ENDIF.





ENDFUNCTION.
