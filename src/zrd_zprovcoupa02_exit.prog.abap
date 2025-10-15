REPORT zrd_zprovcoupa02_exit.

FORM f_exit_zprovcoupa02_0002 USING p_registro_manter TYPE zprovcoupa02
                              CHANGING p_error.

  CHECK p_registro_manter-werks IS NOT INITIAL.

  SELECT SINGLE gtext
    INTO p_registro_manter-werksn
    FROM tgsbt
    WHERE spras = sy-langu
    AND   gsber = p_registro_manter-werks.

ENDFORM.

FORM f_exit_zprovcoupa02_0005 CHANGING p_registro_manter TYPE zprovcoupa02.

  DATA: pernr TYPE pa0001-pernr.

  DATA: cname TYPE zhcmt0007-cname.

  "Valor exportado na função Z_AJUDA_PESQUISA_APROVADORES..
  IMPORT pernr TO pernr FROM MEMORY ID 'NUMERO_PESSOAL_APROVADOR'.
  FREE MEMORY ID 'NUMERO_PESSOAL_APROVADOR'.

  IF pernr IS NOT INITIAL.

    p_registro_manter-pernr_aprovador = pernr.

    SELECT SINGLE cname
      INTO cname
      FROM zhcmt0007
      WHERE pernr = pernr.
    IF sy-subrc IS INITIAL.
      p_registro_manter-nome_aprovador = cname.
    ENDIF.
  ENDIF.

  CHECK p_registro_manter-werks IS NOT INITIAL.

  SELECT SINGLE gtext
    INTO p_registro_manter-werksn
    FROM tgsbt
    WHERE spras = sy-langu
    AND   gsber = p_registro_manter-werks.

ENDFORM.
