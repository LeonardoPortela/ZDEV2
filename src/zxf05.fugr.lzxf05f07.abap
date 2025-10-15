*----------------------------------------------------------------------*
***INCLUDE LZXF05F07.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_grupo_fornec
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_LFB1_BUKRS
*&      --> I_LFA1_KTOKK
*&---------------------------------------------------------------------*
FORM f_grupo_fornec  USING    p_bukrs
                              p_ktokk.


  SELECT SINGLE land1
    FROM t001
    INTO @DATA(lv_land1)
    WHERE bukrs EQ @p_bukrs.
  IF sy-subrc EQ 0.

    SELECT COUNT(*)
      FROM zbpt0001
      WHERE land1 = lv_land1
      AND   ktokk = p_ktokk
      AND   mitkz = 'K'.
    IF sy-subrc NE 0.
      MESSAGE e024(sd) WITH 'Grupo de Fornecedor' p_ktokk 'não previsto para empresa' p_bukrs RAISING error.
    ENDIF.

  ENDIF.



ENDFORM.
