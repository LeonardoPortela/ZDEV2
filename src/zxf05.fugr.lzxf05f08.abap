*----------------------------------------------------------------------*
***INCLUDE LZXF05F08.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_grupo_cliente
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_KNB1_BUKRS
*&      --> I_KNA1_KTOKD
*&---------------------------------------------------------------------*
FORM f_grupo_cliente  USING    p_bukrs
                               p_ktokd.

  SELECT SINGLE land1
    FROM t001
    INTO @DATA(lv_land1)
    WHERE bukrs EQ @p_bukrs.
  IF sy-subrc EQ 0.

    SELECT COUNT(*)
      FROM zbpt0001
      WHERE land1 = lv_land1
      AND   ktokd = p_ktokd
      AND   mitkz = 'D'.
    IF sy-subrc NE 0.
      MESSAGE e024(sd) WITH 'Grupo de Cliente' p_ktokd 'não previsto para empresa' p_bukrs RAISING error.
    ENDIF.

  ENDIF.

ENDFORM.
