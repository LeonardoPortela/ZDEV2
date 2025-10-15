FUNCTION z_xml_versao_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(XML_VERSAO) TYPE  J_1BNFEXMLVERSION
*"----------------------------------------------------------------------


  DATA: wa_setleaf    TYPE setleaf,
        vg_data_corte TYPE sy-datum,
        it_setleaf  LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR: it_setleaf[].

  "Opter Área de contabilidade de custos
  SELECT * INTO TABLE it_setleaf
    FROM setleaf
   WHERE setname EQ 'DATA_INI_EM_XML_2'.

  IF sy-subrc IS INITIAL.
    vg_data_corte = wa_setleaf-valfrom.
    IF vg_data_corte LE sy-datum.
      xml_versao = 2.
    ELSE.
      xml_versao = 110 / 100.
    ENDIF.
  ELSE.
    xml_versao = 110 / 100.
  ENDIF.


ENDFUNCTION.
