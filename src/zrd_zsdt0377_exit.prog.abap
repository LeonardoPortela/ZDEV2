*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0377_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0377_exit.

FORM f_exit_zsdt0377_0001 CHANGING p_registro_manter TYPE any.

  DATA: w_zsdt0377 TYPE zsdt0377.

  CLEAR: w_zsdt0377.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0377.
  w_zsdt0377-waerk = 'USD'.
  w_zsdt0377-user_create    =  sy-uname.
  w_zsdt0377-date_create    = sy-datum.
  w_zsdt0377-time_create    = sy-uzeit.

  MOVE-CORRESPONDING w_zsdt0377 TO p_registro_manter.

ENDFORM.
" delete
FORM f_exit_zsdt0377_0014 USING p_saida TYPE zsdt0377 CHANGING cv_break TYPE c.


  p_saida-deleted = abap_true.
  cv_break = abap_true.

  p_saida-user_change    = sy-uname.
  p_saida-date_change    = sy-datum.
  p_saida-time_change    = sy-uzeit.

  MODIFY zsdt0377 FROM p_saida.

  COMMIT WORK AND WAIT.


ENDFORM.
" pós INSERT ANTES DO SAVE
FORM f_exit_zsdt0377_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0377 TYPE zsdt0377.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0377.

  IF w_zsdt0377-bukrs IS INITIAL.

    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Empresa' DISPLAY LIKE 'E'.
    EXIT.

  ELSE.

    SELECT COUNT(*) FROM t001
      WHERE bukrs = w_zsdt0377-bukrs.

    IF sy-dbcnt = 0.

      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Empresa inválida' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

  ENDIF.

  IF w_zsdt0377-cultura IS INITIAL.

    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Cultura' DISPLAY LIKE 'E'.
    EXIT.

  ELSE.

    SELECT COUNT(*) FROM zsdt0038 WHERE cultura = w_zsdt0377-cultura.

    IF sy-dbcnt = 0.

      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Cultura inválida' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

  ENDIF.

  IF w_zsdt0377-safra IS INITIAL.

    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Safra' DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.
  IF w_zsdt0377-valor IS INITIAL.
    p_erro = abap_true.

    MESSAGE s024(sd) WITH 'Informar Valor'
                 DISPLAY LIKE 'E'.

    EXIT.

  ENDIF.

  SELECT COUNT(*) FROM zsdt0377
    WHERE bukrs = w_zsdt0377-bukrs
      AND cultura = w_zsdt0377-cultura
      AND safra = w_zsdt0377-safra
      AND deleted = abap_false.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Existe registro ativo para essa chave' DISPLAY LIKE 'E'.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  SELECT COUNT(*) FROM zsdt0377
    WHERE bukrs = w_zsdt0377-bukrs
      AND cultura = w_zsdt0377-cultura
      AND safra = w_zsdt0377-safra.

  w_zsdt0377-sequencial = sy-dbcnt + 1.

  MOVE-CORRESPONDING w_zsdt0377  TO  p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0377_0003 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0377 TYPE zsdt0377.
  DATA lv_matnr TYPE matnr18.

  CLEAR: w_zsdt0377.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0377.
  w_zsdt0377-user_change    =  sy-uname.
  w_zsdt0377-date_change    = sy-datum.
  w_zsdt0377-time_change    = sy-uzeit.

*  IF w_zsdt0377-matkl IS NOT INITIAL.
*
*    SELECT SINGLE wgbez FROM t023t
*      INTO w_zsdt0377-wgbez
*        WHERE spras = sy-langu
*          AND matkl = w_zsdt0377-matkl.
*
*  ENDIF.
*
*  IF w_zsdt0377-matkl IS NOT INITIAL.
*
*    SELECT SINGLE msehl FROM t006a
*      INTO w_zsdt0377-msehl
*        WHERE spras = sy-langu
*          AND msehi = w_zsdt0377-mseh3.
*
*  ENDIF.

  MOVE-CORRESPONDING w_zsdt0377 TO p_registro_manter.

ENDFORM.
" processa linha
FORM f_exit_zsdt0377_0004 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0377 TYPE zsdt0377.

  CLEAR w_zsdt0377.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0377.

  IF w_zsdt0377-deleted = abap_true.
    CLEAR w_zsdt0377.
  ENDIF.

  MOVE-CORRESPONDING w_zsdt0377 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0377_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0377_out TYPE zsdt0377.

  CLEAR: wl_zsdt0377_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0377_out.

  MOVE-CORRESPONDING wl_zsdt0377_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0377_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zsdt0377_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

  IF p_field = 'SEQUENCIAL'.

    p_scrtext_l = 'Sequencial'.

  ENDIF.

  IF p_field = 'VALOR'.

    p_scrtext_l = 'Valor.Limite'.

  ENDIF.

  IF p_field = 'DELETED'.

    p_scrtext_l = 'Excluido'.
    p_check = abap_true.

  ENDIF.

ENDFORM.

FORM f_exit_zsdt0377_0009  TABLES it_excl_toolbar
                            USING p_db_tab.

  IF p_db_tab = 'ZSDT0377'.
    APPEND 'Modificar' TO it_excl_toolbar.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0377_0017 USING p_tipo.


  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.

*====>  Tabelas internas
  DATA: BEGIN OF t_val OCCURS 0,
          valida TYPE ilm_para_value,
          descr  TYPE eaml_descr,
        END OF t_val.

  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
  DATA lv_field TYPE dynfnam.

  GET CURSOR FIELD lv_field.

  IF p_tipo = '0001'. "   CULTURA

    SELECT cultura AS domvalue_l descricao AS ddtext FROM zsdt0038
      INTO CORRESPONDING FIELDS OF TABLE t_dd07v.

  ELSEIF p_tipo = '0002'. " SAFRA

    DATA lv_safra TYPE c LENGTH 4.

    lv_safra = sy-datum(4).

    SUBTRACT 2 FROM lv_safra.

    DO 4 TIMES.

      APPEND INITIAL LINE TO t_dd07v ASSIGNING FIELD-SYMBOL(<fs_dd07v>).

      <fs_dd07v>-domvalue_l = lv_safra.
      <fs_dd07v>-ddtext = `Safra: ` && lv_safra.

      ADD 1 TO lv_safra.

    ENDDO.

  ENDIF.

  CHECK t_dd07v[] IS NOT INITIAL.

  LOOP AT t_dd07v INTO s_dd07v.
    t_val-valida     = s_dd07v-domvalue_l.
    t_val-descr      = s_dd07v-ddtext.
    APPEND t_val.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VALIDA'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = lv_field
      window_title    = 'Pesquisa'
      value_org       = 'S'
    TABLES
      value_tab       = t_val
      return_tab      = t_return
      dynpfld_mapping = t_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.




ENDFORM.
FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.

*====>  Tabelas internas
  DATA: BEGIN OF t_val OCCURS 0,
          valida TYPE zlest0230-valida,
          descr  TYPE eaml_descr,
        END OF t_val.

  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT cultura AS domvalue_l descricao AS ddtext FROM zsdt0038
    INTO CORRESPONDING FIELDS OF TABLE t_dd07v.

  CHECK t_dd07v[] IS NOT INITIAL.

  LOOP AT t_dd07v INTO s_dd07v.
    t_val-valida     = s_dd07v-domvalue_l.
    t_val-descr      = s_dd07v-ddtext.
    APPEND t_val.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VALIDA'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Valida Regra'
      value_org       = 'S'
    TABLES
      value_tab       = t_val
      return_tab      = t_return
      dynpfld_mapping = t_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
