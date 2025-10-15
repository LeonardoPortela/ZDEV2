*&--------------------------------------------------------------------&*
*&                     Relatório Módulo - SD                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 04/07/2024                                              &*
*& Descrição: Cadastro de valores UPF e Alíquotas Retenção nas Vendas &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0343_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0343_t1_exit.

FORM f_exit_zsdt0343_t1_0002 USING p_registro_manter TYPE any
                          CHANGING p_error.

  DATA: wl_zsdt0343_t1 TYPE zsdt0343_t1.
  DATA: lv_num(10).

  CLEAR: wl_zsdt0343_t1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0343_t1.

  IF wl_zsdt0343_t1-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a Empresa!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zsdt0343_t1-uf IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha o Estado!' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM zsdt0343_t1
    UP TO 1 ROWS
    INTO @DATA(lv_found)
    WHERE bukrs  EQ @wl_zsdt0343_t1-bukrs
      AND uf     EQ @wl_zsdt0343_t1-uf
      AND cancel EQ @abap_off.
  ENDSELECT.

  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'NOVO'.
    p_error = abap_true.
    MESSAGE 'Cadastro já existe!' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM zsdt0343_t1
    UP TO 1 ROWS
    INTO @lv_found
    WHERE bukrs  EQ @wl_zsdt0343_t1-bukrs
      AND uf     EQ @wl_zsdt0343_t1-uf
      AND cancel EQ @abap_on.
  ENDSELECT.

  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'CHANGE' AND lv_found-cancel IS NOT INITIAL.
    p_error = abap_true.
    MESSAGE 'Registro cancelado, valor não pode ser modificado!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'CHANGE' AND wl_zsdt0343_t1-vl_upf NE lv_found-vl_upf.
    p_error = abap_true.
    MESSAGE 'Valor não pode ser modificado!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF sy-ucomm EQ 'NOVO'.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSDT0343T1'
      IMPORTING
        number                  = lv_num
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE i836(sd) WITH 'O intervalo de numeração,'
                            'não foi encontrado!'.
      EXIT.
    ENDIF.

    wl_zsdt0343_t1-id = lv_num.

  ENDIF.

  wl_zsdt0343_t1-c_user = sy-uname.
  wl_zsdt0343_t1-hora = sy-uzeit.
  wl_zsdt0343_t1-data = sy-datum.

  MOVE-CORRESPONDING wl_zsdt0343_t1 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0343_t1_0005 CHANGING p_registro_manter TYPE any.


  DATA: wl_zsdt0343_t1 TYPE zsdt0343_t1.
  DATA: lv_num(10).

  CLEAR: wl_zsdt0343_t1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0343_t1.

  IF sy-ucomm EQ 'CHANGE'.

    LOOP AT SCREEN.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-VL_UPF'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    SELECT * FROM zsdt0343_t1
      UP TO 1 ROWS
      INTO @DATA(lv_found)
      WHERE bukrs  EQ @wl_zsdt0343_t1-bukrs
        AND uf     EQ @wl_zsdt0343_t1-uf
        AND cancel EQ @abap_on.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND sy-ucomm EQ 'CHANGE' AND lv_found-cancel IS NOT INITIAL.
*    p_error = abap_true.
      MESSAGE 'Registro cancelado, valor não pode ser modificado!' TYPE 'I'.

      LOOP AT SCREEN.
        IF screen-name(23) EQ '<FS_WA_REGISTRO_MANTER>'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

      EXIT.

    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0343_t1 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0343_t1_0019 USING p_registro_search TYPE any
                       CHANGING p_error
                                p_cond TYPE rsds_where.

  DATA: lwa_cond_line  TYPE rsdswhere.

  DATA: wl_zsdt0343_t1 TYPE zsdt0343_t1.

  CLEAR: wl_zsdt0343_t1, p_cond.

  MOVE-CORRESPONDING p_registro_search TO wl_zsdt0343_t1.

*------------------------------------------------------------
  SELECT * FROM zsdt0343_range
    INTO TABLE @DATA(it_zsdt0343_range)
    WHERE campo EQ 'BUKRS'
      AND c_option EQ 'EQ'.

  IF sy-subrc IS INITIAL.
    SORT it_zsdt0343_range BY c_option low.

    DELETE ADJACENT DUPLICATES FROM it_zsdt0343_range COMPARING c_option low.

    DATA(lv_line) = lines( it_zsdt0343_range ).

    IF lv_line GE '3'.
      LOOP AT it_zsdt0343_range ASSIGNING FIELD-SYMBOL(<fs_range>).

        AT FIRST.
          DATA(lv_str) = '( ' && | '{ <fs_range>-low }'| && ','.
          CONTINUE.
        ENDAT.
        AT NEW low.
          IF sy-tabix EQ lv_line.
            lv_str = lv_str && | '{ <fs_range>-low }' | && ')'.
            CONTINUE.
          ENDIF.
          lv_str = lv_str && | '{ <fs_range>-low }'| && ','.
        ENDAT.
*      AT LAST.
*        lv_str = lv_str && | '{ <fs_range>-low }' | && ')'.
*      ENDAT.

      ENDLOOP.
    ELSEIF lv_line EQ '2'.
      LOOP AT it_zsdt0343_range ASSIGNING <fs_range>.
        AT FIRST.
          lv_str = '( ' && | '{ <fs_range>-low }'| && ','.
        ENDAT.
        AT LAST.
          lv_str = lv_str && | '{ <fs_range>-low }' | && ')'.
        ENDAT.

      ENDLOOP.
    ELSEIF lv_line EQ '1'.
      LOOP AT it_zsdt0343_range ASSIGNING <fs_range>.
        AT LAST.
          lv_str = '( ' && | '{ <fs_range>-low }' | && ')'.
        ENDAT.
      ENDLOOP.
    ENDIF.

    IF lv_str IS NOT INITIAL.
      APPEND VALUE #( line = |     ( BUKRS IN { lv_str } ) | ) TO  p_cond-where_tab.
    ENDIF.

    IF lv_str IS NOT INITIAL.
      APPEND VALUE #( line = |  AND  ( CANCEL NE '{ abap_true }' ) | ) TO  p_cond-where_tab.
    ENDIF.

*    DELETE FROM ZSDT0343_range WHERE campo EQ 'BUKRS'.
*    COMMIT WORK.
  ENDIF.

*------------------------------------------------------------
  SELECT * FROM zsdt0343_range
    INTO TABLE @it_zsdt0343_range
    WHERE campo    EQ 'BUKRS'
      AND c_option NE 'EQ'.

  IF sy-subrc IS INITIAL.

    LOOP AT it_zsdt0343_range ASSIGNING <fs_range>.
      APPEND VALUE #( line = | AND ( BUKRS | && <fs_range>-c_option && | '{ <fs_range>-low }' ) | ) TO  p_cond-where_tab.
    ENDLOOP.

*    DELETE FROM ZSDT0343_range WHERE campo EQ 'BUKRS' AND c_option NE 'EQ'.
*    COMMIT WORK.

  ENDIF.

ENDFORM.

FORM f_exit_zsdt0343_t1_0013  TABLES p_tables.

  DATA: wl_zsdt0343_t1 TYPE zsdt0343_t1,
        it_zsdt0343_t1 TYPE TABLE OF zsdt0343_t1.

  LOOP AT p_tables ASSIGNING FIELD-SYMBOL(<fs_p_table>).

    wl_zsdt0343_t1 = CORRESPONDING #( <fs_p_table> ).

    IF wl_zsdt0343_t1-cancel IS INITIAL.
      wl_zsdt0343_t1-cancel = abap_on.
      wl_zsdt0343_t1-user_cancel = sy-uname.
      wl_zsdt0343_t1-data_cancel = sy-datum.
      wl_zsdt0343_t1-hora_cancel = sy-uzeit.

      MODIFY zsdt0343_t1 FROM wl_zsdt0343_t1.
      IF sy-subrc IS INITIAL.
        <fs_p_table> = CORRESPONDING #( wl_zsdt0343_t1 ).
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
FORM f_exit_zsdt0343_t1_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0343_T1_OUT'.
*     p_field       = 'VTEXT'.
    p_outputlen = 15.
  ENDIF.

ENDFORM.
FORM  f_exit_zsdt0343_t1_0009 TABLES it_excl_toolbar
                              USING p_db_tab.

  IF p_db_tab = 'ZSDT0343_T1'.
    APPEND 'Deletar'      TO it_excl_toolbar.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
FORM f_exit_zsdt0343_t1_0020 CHANGING p_refresh_selecao.

  p_refresh_selecao = abap_true.

ENDFORM.
