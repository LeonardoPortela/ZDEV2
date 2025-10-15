*&--------------------------------------------------------------------&*
*&                     Relatório Módulo - MM                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 16/09/2024                                              &*
*& Descrição: Automatizar info obrigatórias notas de fertilizantes    &*
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
*& Report  zrd_zmmt0089_exit
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0089_exit.

FORM f_exit_zmmt0089_0002 USING p_registro_manter TYPE any
                             CHANGING p_error.

  DATA: wl_zmmt0089 TYPE zmmt0089.
  DATA: lv_num(10).

  CLEAR: wl_zmmt0089.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0089.

  IF wl_zmmt0089-tipo_tag IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha o Tipo de Tag!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zmmt0089-tag IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a Tag!' TYPE 'I'.
    EXIT.
  ENDIF.

*  IF wl_zmmt0089-tag_fim IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Preencha a Tag Fim!' TYPE 'I'.
*    EXIT.
*  ENDIF.

*  IF wl_zmmt0089-matkl IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Preencha o Grupo de Mercadoria!' TYPE 'I'.
*    EXIT.
*  ENDIF.

**  IF wl_zmmt0089-vtext IS INITIAL.
*  SELECT vtext UP TO 1 ROWS
*    FROM t685t
*    INTO @DATA(lv_desc)
*    WHERE spras EQ @sy-langu
*      AND kschl EQ @wl_zmmt0089-kschl.
*  ENDSELECT.
**  ENDIF.
*
**  IF wl_zmmt0089-vtextg IS INITIAL.
*  SELECT wgbez60 UP TO 1 ROWS
*    FROM t023t
*    INTO @DATA(lv_descg)
*    WHERE spras EQ @sy-langu
*      AND matkl EQ @wl_zmmt0089-matkl.
*  ENDSELECT.
**  ENDIF.
*
*  SELECT * FROM zmmt0089
*    UP TO 1 ROWS
*    INTO @DATA(lv_found)
*    WHERE bukrs    EQ @wl_zmmt0089-bukrs
*      AND branch   EQ @wl_zmmt0089-branch
*      AND kschl    EQ @wl_zmmt0089-kschl
*      AND cancel   EQ @abap_off.
*  ENDSELECT.
*
*  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'NOVO'.
*    p_error = abap_true.
*    MESSAGE 'Cadastro já existe!' TYPE 'I'.
*    EXIT.
*  ENDIF.

*  IF sy-ucomm EQ 'CHANGE'.
*    p_error = abap_true.
*    MESSAGE 'Não é possível modificar grupo de mercadoria!' TYPE 'I'.
*    EXIT.
*  ENDIF.

*  SELECT * FROM zmmt0089
*    UP TO 1 ROWS
*    INTO @lv_found
*    WHERE bukrs    EQ @wl_zmmt0089-bukrs
*      AND branch   EQ @wl_zmmt0089-branch
*      AND kschl    EQ @wl_zmmt0089-kschl
*      AND cancel   EQ @wl_zmmt0089-cancel.
*  ENDSELECT.
*
*  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'CHANGE' AND lv_found-cancel IS NOT INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Registro cancelado, valor não pode ser modificado!' TYPE 'I'.
*    EXIT.
*  ENDIF.

*  SELECT * FROM zsdt0343_t1
*    UP TO 1 ROWS
*    INTO @DATA(wa_zsdt0343_t1)
*    WHERE bukrs    EQ @wl_zmmt0089-bukrs
*      AND cancel   EQ @abap_off.
*  ENDSELECT.
*  IF sy-subrc IS NOT INITIAL OR wa_zsdt0343_t1-vl_upf IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Não existe UPF cadastrada para a empresa e Estado informado!' TYPE 'I'.
*    EXIT.
*  ENDIF.

*  SELECT * FROM ZSDT0343_t2
*    UP TO 1 ROWS
*    INTO @DATA(wa_ZSDT0343_t2)
*    WHERE bukrs    EQ @wl_zmmt0089-bukrs
*      AND branch   EQ @wl_zmmt0089-branch
*      AND kschl    EQ @wl_zmmt0089-kschl
*      AND cancel   EQ @abap_off.
*  ENDSELECT.
*  IF sy-subrc IS NOT INITIAL OR wa_ZSDT0343_t1-vl_upf IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Não existe Alíquota cadastrada para a empresa!' TYPE 'I'.
*    EXIT.
*  ENDIF.

*  IF sy-ucomm EQ 'NOVO'.
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr             = '01'
*        object                  = 'ZSDT0343T3'
*      IMPORTING
*        number                  = lv_num
*      EXCEPTIONS
*        interval_not_found      = 1
*        number_range_not_intern = 2
*        object_not_found        = 3
*        quantity_is_0           = 4
*        quantity_is_not_1       = 5
*        interval_overflow       = 6
*        buffer_overflow         = 7
*        OTHERS                  = 8.
*    IF sy-subrc NE 0.
*      p_error = abap_true.
*      MESSAGE i836(sd) WITH 'O intervalo de numeração,'
*                            'não foi encontrado!'.
*      EXIT.
*    ENDIF.
*    wl_zmmt0089-id = lv_num.
*  ENDIF.

**  wl_zmmt0089-c_user = sy-uname.
**  wl_zmmt0089-hora   = sy-uzeit.
**  wl_zmmt0089-data   = sy-datum.
*  IF wl_zmmt0089-vtext IS NOT INITIAL.
*    wl_zmmt0089-vtext = wl_zmmt0089-vtext.
*  ELSE.
*    wl_zmmt0089-vtext  = lv_desc.
*  ENDIF.
*  IF wl_zmmt0089-vtextg IS NOT INITIAL.
*    wl_zmmt0089-vtextg = wl_zmmt0089-vtextg.
*  ELSE.
*    wl_zmmt0089-vtextg  = lv_descg.
*  ENDIF.

  MOVE-CORRESPONDING wl_zmmt0089 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0089_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0089 TYPE zmmt0089.

  CLEAR: wl_zmmt0089.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0089.

*  SELECT * FROM zmmt0089
*    UP TO 1 ROWS
*    INTO @DATA(lv_found)
*    WHERE bukrs    EQ @wl_zmmt0089-bukrs
*      AND branch   EQ @wl_zmmt0089-branch
*      AND kschl    EQ @wl_zmmt0089-kschl
*      AND cancel   EQ @wl_zmmt0089-cancel.
*  ENDSELECT.
*
*  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'CHANGE' AND lv_found-cancel IS NOT INITIAL.
*
*    MESSAGE 'Registro cancelado, valor não pode ser modificado!' TYPE 'I'.
*    LOOP AT SCREEN.
*      IF screen-name(23) EQ '<FS_WA_REGISTRO_MANTER>'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*    EXIT.
*
*  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = '<FS_WA_REGISTRO_MANTER>-FATOR'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF sy-ucomm EQ 'CHANGE'.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-VTEXT'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-VTEXTG'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  MOVE-CORRESPONDING wl_zmmt0089 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0089_0010 TABLES it_zsdt0343.

*  DATA: it_zmmt0089 TYPE STANDARD TABLE OF zmmt0089.

*  it_zmmt0089[] = CORRESPONDING #( it_zsdt0343[] ).
*
*  IF it_zmmt0089[] IS NOT INITIAL.
*
*    SELECT *
*      FROM t685t
*      INTO TABLE @DATA(it_desc)
*      FOR ALL ENTRIES IN @it_zmmt0089
*      WHERE spras EQ @sy-langu
*        AND kschl EQ @it_zmmt0089-kschl.
*
*    SELECT * "wgbez60 UP TO 1 ROWS
*      FROM t023t
*      INTO TABLE @DATA(it_descg)
*      FOR ALL ENTRIES IN @it_zmmt0089
*      WHERE spras EQ @sy-langu
*        AND matkl EQ @it_zmmt0089-matkl.
*
**    LOOP AT it_zmmt0089 ASSIGNING FIELD-SYMBOL(<fs_zsdt0343>).
**
**      READ TABLE it_desc INTO DATA(wa_desc) WITH KEY kschl = <fs_zsdt0343>-kschl.
**      IF sy-subrc IS INITIAL.
**        <fs_zsdt0343>-vtext = wa_desc-vtext.
**      ENDIF.
**
**      READ TABLE it_descg INTO DATA(wa_descg) WITH KEY matkl = <fs_zsdt0343>-matkl.
**      IF sy-subrc IS INITIAL.
**        <fs_zsdt0343>-vtextg = wa_descg-wgbez60.
**      ENDIF.
**
**    ENDLOOP.
*
*    it_zsdt0343[] = CORRESPONDING #( it_zmmt0089[] ).
*  ENDIF.
ENDFORM.

FORM f_exit_zmmt0089_0019 USING p_registro_search TYPE any
                       CHANGING p_error
                                p_cond TYPE rsds_where.

  DATA: lwa_cond_line  TYPE rsdswhere.

  DATA: wl_zmmt0089 TYPE zmmt0089.

  CLEAR: wl_zmmt0089, p_cond.

  MOVE-CORRESPONDING p_registro_search TO wl_zmmt0089.

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
      APPEND VALUE #( line = |( BUKRS IN { lv_str } ) | ) TO  p_cond-where_tab.
    ENDIF.

    IF lv_str IS NOT INITIAL.
      APPEND VALUE #( line = |AND ( CANCEL NE '{ abap_true }' ) | ) TO  p_cond-where_tab.
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
      APPEND VALUE #( line = |AND ( BUKRS | && <fs_range>-c_option && | '{ <fs_range>-low }' ) | ) TO  p_cond-where_tab.
    ENDLOOP.

*    DELETE FROM ZSDT0343_range WHERE campo EQ 'BUKRS' AND c_option NE 'EQ'.
*    COMMIT WORK.
  ENDIF.
*--------------------------------------------------------------------

  SELECT * FROM zsdt0343_range
    INTO TABLE @it_zsdt0343_range
    WHERE campo EQ 'WERKS'
      AND c_option EQ 'EQ'.

  IF sy-subrc IS INITIAL.
    SORT it_zsdt0343_range BY c_option low.

    DELETE ADJACENT DUPLICATES FROM it_zsdt0343_range COMPARING c_option low.

    lv_line = lines( it_zsdt0343_range ).

    IF lv_line GE '3'.
      LOOP AT it_zsdt0343_range ASSIGNING <fs_range>.

        AT FIRST.
          lv_str = '( ' && | '{ <fs_range>-low }'| && ','.
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
          lv_str = '( ' && |' { <fs_range>-low }' | && ')'.
        ENDAT.
      ENDLOOP.
    ENDIF.

    CLEAR lv_str.

    IF lv_str IS NOT INITIAL.
      APPEND VALUE #( line = |AND ( BRANCH IN { lv_str } ) | ) TO  p_cond-where_tab.
    ENDIF.

*    DELETE FROM ZSDT0343_range WHERE campo EQ 'WERKS' AND c_option EQ 'EQ'.
*    COMMIT WORK.
  ENDIF.
*-------------------------------------------------------------------------------------
  SELECT * FROM zsdt0343_range
    INTO TABLE @it_zsdt0343_range
    WHERE campo EQ 'WERKS'
      AND c_option EQ 'BT'.

  IF sy-subrc IS INITIAL.

    LOOP AT it_zsdt0343_range ASSIGNING <fs_range>.
      APPEND VALUE #( line = |AND ( BRANCH BETWEEN '{ <fs_range>-low  }' AND '{ <fs_range>-high  }' ) | ) TO  p_cond-where_tab.
    ENDLOOP.

*    DELETE FROM ZSDT0343_range WHERE campo EQ 'WERKS' AND c_option EQ 'BT'.
*    COMMIT WORK.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0089_0013  TABLES p_tables.

*  DATA: wl_zmmt0089 TYPE zmmt0089,
*        it_zmmt0089 TYPE TABLE OF zmmt0089.

*  LOOP AT p_tables ASSIGNING FIELD-SYMBOL(<fs_p_table>).
*
*    wl_zmmt0089 = CORRESPONDING #( <fs_p_table> ).
*
*    IF wl_zmmt0089-cancel IS INITIAL.
*      wl_zmmt0089-cancel = abap_on.
*      wl_zmmt0089-user_cancel = sy-uname.
*      wl_zmmt0089-data_cancel = sy-datum.
*      wl_zmmt0089-hora_cancel = sy-uzeit.
*
*      MODIFY zmmt0089 FROM wl_zmmt0089.
*      IF sy-subrc IS INITIAL.
*        COMMIT WORK.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.

*  CALL METHOD obj_alv->refresh_table_display
*    EXPORTING
*      is_stable = wa_stable.

ENDFORM.
FORM f_exit_zmmt0089_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'zmmt0089'. "= 'ZSDT0343 T2'.
*     p_field       = 'STEXT'.
    p_outputlen = 15.
  ENDIF.

ENDFORM.
FORM  f_exit_zmmt0089_0009 TABLES it_excl_toolbar
                              USING p_db_tab.

*  IF p_db_tab = 'zmmt0089'.
*    APPEND 'Deletar'      TO it_excl_toolbar.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
*FORM f_exit_zmmt0089_0020 CHANGING p_refresh_selecao.
*
*  p_refresh_selecao = abap_true.
*
*ENDFORM.
FORM f_exit_zmmt0089_0017 USING p_tipo.

*  IF p_tipo = '0001'.
*    PERFORM f4_val_kschl USING '<FS_WA_REGISTRO_MANTER>-KSCHL'
*                               '<FS_WA_REGISTRO_MANTER>-VTEXT'.
*  ENDIF.
*
*  IF p_tipo = '0002'.
*    PERFORM f4_val_matkl USING '<FS_WA_REGISTRO_MANTER>-MATKL'
*                               '<FS_WA_REGISTRO_MANTER>-VTEXTG'.
*  ENDIF.

ENDFORM.

*FORM f4_val_kschl USING p_cod TYPE help_info-dynprofld
*                        p_desc TYPE help_info-dynprofld.
*
*
*  FIELD-SYMBOLS: <fs_campo> TYPE any.
*  FIELD-SYMBOLS: <fs_campo2> TYPE any.
*
*  ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>-KSCHL') TO <fs_campo>.
*  ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>-VTEXT') TO <fs_campo2>.
*
*  IF <fs_campo> IS ASSIGNED.
*
*    SELECT kschl, vtext UP TO 1 ROWS
*      FROM t685t
*      INTO @DATA(t_desc)
*      WHERE spras EQ @sy-langu
*        AND kschl EQ @<fs_campo>.
*    ENDSELECT.
*
*    IF sy-subrc IS INITIAL AND t_desc-vtext IS NOT INITIAL.
*      IF <fs_campo2> IS ASSIGNED.
*        <fs_campo2>  = t_desc-vtext.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*
***====>  Tabelas internas
**  DATA: BEGIN OF t_skat OCCURS 0,
**          hkont  TYPE hkont,
**          txt50  TYPE txt50,
**        END OF t_skat.
**
**  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
**  DATA: t_mapping TYPE STANDARD TABLE OF dselc.
**
***====>  Work Area
**  DATA: s_return  TYPE ddshretval.
**  DATA: s_mapping TYPE dselc.
***  DATA: c_pt    TYPE langu VALUE 'PT',
***        c_ktopl TYPE ktopl VALUE '0050'.
**
***  READ TABLE t_return ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY fieldname = 'F0001'.
***  IF sy-subrc = 0.
**
***  SELECT saknr txt50
***    FROM  skat INTO TABLE t_skat
***    WHERE spras = c_pt
***      AND ktopl = c_ktopl.
**    SELECT kschl, vtext "UP TO 1 ROWS
**      FROM t685t
**      INTO table @DATA(t_desc)
**      WHERE spras EQ @sy-langu
**        AND kschl EQ @p_cod.
***    ENDSELECT.
**
***  ENDIF.
**
**  IF sy-subrc = 0.
**
**    s_mapping-fldname     = 'F0001'.
**    s_mapping-dyfldname   = p_cod.
**    APPEND s_mapping TO t_mapping.
**    CLEAR s_mapping.
**
**    s_mapping-fldname     = 'F0002'.
**    s_mapping-dyfldname   = p_desc.
**    APPEND s_mapping TO t_mapping.
**    CLEAR s_mapping.
**
**    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
**      EXPORTING
**        retfield        = 'KSCHL'
**        dynpprog        = sy-cprog
**        dynpnr          = sy-dynnr
**        dynprofield     = p_cod
**        window_title    = 'Tipos de Condição'
**        value_org       = 'S'
**      TABLES
**        value_tab       = t_desc
**        return_tab      = t_return
**        dynpfld_mapping = t_mapping
**      EXCEPTIONS
**        parameter_error = 1
**        no_values_found = 2
**        OTHERS          = 3.
**
**    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**    ENDIF.
**  ENDIF.
*
*ENDFORM.

*FORM f4_val_matkl USING p_cod TYPE help_info-dynprofld
*                        p_desc TYPE help_info-dynprofld.
*
*
*  FIELD-SYMBOLS: <fs_campo3> TYPE any.
*  FIELD-SYMBOLS: <fs_campo4> TYPE any.
*
*  ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>-MATKL') TO <fs_campo3>.
*  ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>-VTEXTG') TO <fs_campo4>.
*
*  IF <fs_campo3> IS ASSIGNED.
*
*    SELECT matkl, wgbez60 UP TO 1 ROWS
*      FROM t023t
*      INTO @DATA(t_desc)
*      WHERE spras EQ @sy-langu
*        AND matkl EQ @<fs_campo3>.
*    ENDSELECT.
*
*    IF sy-subrc IS INITIAL AND t_desc-wgbez60 IS NOT INITIAL.
*      IF <fs_campo4> IS ASSIGNED.
*        <fs_campo4>  = t_desc-wgbez60.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.

*ENDFORM.
