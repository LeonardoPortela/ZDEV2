*&---------------------------------------------------------------------*
*& Report  ZRD_ZBPT0001_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zbpt0001_exit.

FORM f_exit_zbpt0001_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zbpt0001 TYPE zbpt0001.

  CLEAR: wl_zbpt0001.

  wl_zbpt0001-date_create = sy-datum.
  wl_zbpt0001-time_create = sy-uzeit.
  wl_zbpt0001-user_create = sy-uname.

  MOVE-CORRESPONDING wl_zbpt0001 TO p_registro_manter.

ENDFORM.

FORM f_exit_zbpt0001_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zbpt0001 TYPE zbpt0001.

  CLEAR: wl_zbpt0001.

  MOVE-CORRESPONDING p_registro_manter TO wl_zbpt0001.

  CLEAR: p_error.

  IF wl_zbpt0001-mitkz NE 'D' AND wl_zbpt0001-mitkz NE 'K'.
    p_error = abap_true.
    MESSAGE 'Obrigatório informar  tipo: D - Cliente  ou K - Fornecedor' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wl_zbpt0001-mitkz = 'D' AND wl_zbpt0001-ktokd = ''.
    p_error = abap_true.
    MESSAGE 'Para tipo de conta D (Cliente) é preciso informar um grupo de cliente' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wl_zbpt0001-mitkz = 'K' AND wl_zbpt0001-ktokk = ''.
    p_error = abap_true.
    MESSAGE 'Para tipo de conta K - (Fornecedor) é preciso informar um grupo de fornecedor' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  SELECT SINGLE COUNT(*)
    FROM t001
    WHERE land1 EQ wl_zbpt0001-land1.
  IF sy-subrc NE 0.
    p_error = abap_true.
    MESSAGE 'Chave país inválida' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE COUNT(*)
    FROM t001b
    WHERE mkoar EQ wl_zbpt0001-mitkz.
  IF sy-subrc NE 0.
    p_error = abap_true.
    MESSAGE 'Tipo de conta inválido' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF  wl_zbpt0001-mitkz = 'F'.

    SELECT SINGLE COUNT(*)
     FROM t077k
     WHERE ktokk EQ wl_zbpt0001-ktokk.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Grupo de conta fornecedor inválido' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  IF  wl_zbpt0001-mitkz = 'D'.

    SELECT SINGLE COUNT(*)
      FROM T077d
      WHERE ktokd EQ wl_zbpt0001-ktokd.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Grupo de conta cliente inválido' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.

*FORM F_EXIT_ZBPT0001_0003 CHANGING P_REGISTRO_MANTER TYPE ANY.
*
*  DATA: WL_ZBPT0001 TYPE ZBPT0001.
*
*  CLEAR: WL_ZBPT0001.
*
*  MOVE-CORRESPONDING P_REGISTRO_MANTER TO WL_ZBPT0001.
*
*  WL_ZBPT0001-DT_REGISTRO = SY-DATUM.
*  WL_ZBPT0001-HR_REGISTRO = SY-UZEIT.
*  WL_ZBPT0001-US_REGISTRO = SY-UNAME.
*
*  MOVE-CORRESPONDING WL_ZBPT0001 TO P_REGISTRO_MANTER.
*
*ENDFORM.

*FORM F_EXIT_ZBPT0001_0004 CHANGING P_SAIDA TYPE ANY.
*
*  DATA: WL_ZBPT0001_OUT TYPE ZBPT0001_OUT.
*
*  CLEAR: WL_ZBPT0001_OUT.
*
*  MOVE-CORRESPONDING P_SAIDA TO WL_ZBPT0001_OUT.
*
*  "WL_ZBPT0001_OUT-BUTXT = ''.
*
*  MOVE-CORRESPONDING WL_ZBPT0001_OUT TO P_SAIDA.
*
*ENDFORM.
