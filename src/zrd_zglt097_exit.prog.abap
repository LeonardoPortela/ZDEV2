*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT097_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt097_exit.

FORM f_exit_zglt097_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt097 TYPE zglt097.

  CLEAR: wl_zglt097.

  wl_zglt097-usnam     = sy-uname.
  wl_zglt097-zdt_atual = sy-datum.
  wl_zglt097-zhr_atual = sy-uzeit.

  MOVE-CORRESPONDING wl_zglt097 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt097_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt097 TYPE zglt097.

  CLEAR: wl_zglt097.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt097.

  CLEAR: p_error.

  IF wl_zglt097-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Empresa é campo obritório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zglt097-livro_caixa IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Livro Caixa é campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zglt097-gjahr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Exercício é campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zglt097-monat IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Mês/Exercício é campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.
  IF wl_zglt097-data_lim IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Data Final é campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.



ENDFORM.

FORM f_exit_zglt097_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt097 TYPE zglt097.

  CLEAR: wl_zglt097.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt097.

  wl_zglt097-usnam = sy-uname.
  wl_zglt097-zdt_atual = sy-datum.
  wl_zglt097-zhr_atual = sy-uzeit.

  MOVE-CORRESPONDING wl_zglt097 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt097_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt097_out TYPE zglt097_out.

  CLEAR: wl_zglt097_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt097_out.

  MOVE-CORRESPONDING wl_zglt097_out TO p_saida.

ENDFORM.
FORM f_exit_zglt097_0013 TABLES p_table.

  DATA: t_zglt097_out   TYPE TABLE OF zglt097_out,
        w_t_zglt097_out TYPE zglt097_out,
        w_t_zglt097     TYPE zglt097_out,
        t_zglt097       TYPE TABLE OF zglt097_out,
        lva_data_lim    TYPE zglt097_out-data_lim,
        lva_gjahr       TYPE zglt097_out-gjahr,
        lva_monat       TYPE zglt097_out-monat,
        lva_answer      TYPE c.

  DATA: lwa_fields LIKE sval,
        lit_fields TYPE STANDARD TABLE OF sval.

  t_zglt097_out[] = p_table[] .

  SORT t_zglt097_out BY bukrs livro_caixa gjahr monat.

  MOVE 'ZGLT097'    TO lwa_fields-tabname.
  MOVE 'MONAT'   TO lwa_fields-fieldname.
  APPEND lwa_fields TO lit_fields.

  MOVE 'ZGLT097'    TO lwa_fields-tabname.
  MOVE 'GJAHR'   TO lwa_fields-fieldname.
  APPEND lwa_fields TO lit_fields.

  MOVE 'ZGLT097'    TO lwa_fields-tabname.
  MOVE 'DATA_LIM'   TO lwa_fields-fieldname.
  APPEND lwa_fields TO lit_fields.


  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = 'Informe Data Limite'
    IMPORTING
      returncode  = lva_answer
    TABLES
      fields      = lit_fields.

  IF lva_answer IS INITIAL.

    LOOP AT lit_fields INTO lwa_fields .
      IF lwa_fields-fieldname = 'MONAT'.
        lva_monat =  lwa_fields-value.
      ELSE.
        IF  lwa_fields-fieldname = 'GJAHR'.
          lva_gjahr = lwa_fields-value.
        ELSE.
          lva_data_lim = lwa_fields-value.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lva_data_lim IS NOT INITIAL AND
       lva_gjahr IS NOT INITIAL AND
       lva_monat IS NOT INITIAL.

      LOOP AT t_zglt097_out INTO  w_t_zglt097_out.
        MOVE-CORRESPONDING w_t_zglt097_out TO w_t_zglt097.

        w_t_zglt097-gjahr     = lva_gjahr.
        w_t_zglt097-monat     = lva_monat.
        w_t_zglt097-data_lim  = lva_data_lim.
        w_t_zglt097-usnam     = sy-uname.
        w_t_zglt097-zdt_atual = sy-datum.
        w_t_zglt097-zhr_atual = sy-uzeit.

*        IF w_t_zglt097-monat = '12'.
*          w_t_zglt097-monat = '01'.
*          w_t_zglt097-gjahr = w_t_zglt097_out-gjahr + 1.
*        ELSE.
*          w_t_zglt097-monat = w_t_zglt097_out-monat + 1.
*          w_t_zglt097-gjahr = w_t_zglt097_out-gjahr.
*        ENDIF.
*
*        CONCATENATE lva_data_lim+0(4) w_t_zglt097-monat w_t_zglt097-data_lim+6(2) INTO  w_t_zglt097-data_lim .

        SELECT *
          FROM zglt097
          INTO TABLE @DATA(lit_zglt097)
         WHERE bukrs  = @w_t_zglt097-bukrs
           AND livro_caixa = @w_t_zglt097-livro_caixa
           AND gjahr = @w_t_zglt097-gjahr
           AND monat = @w_t_zglt097-monat.

        IF  lit_zglt097 IS INITIAL.

          APPEND w_t_zglt097 TO t_zglt097.
          MODIFY zglt097 FROM w_t_zglt097.
        ENDIF.
        CLEAR: w_t_zglt097, lit_zglt097.
      ENDLOOP.
    ENDIF.

    p_table[] = t_zglt097[].

  ENDIF.
ENDFORM.
