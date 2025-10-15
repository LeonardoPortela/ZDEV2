*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0003_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zglt0003_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0003 TYPE zglt0003.

  CLEAR: wl_zglt0003.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0003.

  IF wl_zglt0003-criador IS INITIAL.
    wl_zglt0003-criador   = sy-uname.
  ENDIF.
  wl_zglt0003-modificador   = sy-uname.
  wl_zglt0003-data   = sy-datum.
  wl_zglt0003-hora   = sy-uzeit.

  MOVE-CORRESPONDING wl_zglt0003 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0003_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0003        TYPE zglt0003,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0003.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0003.


  MOVE-CORRESPONDING wl_zglt0003 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0003_0003 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0003        TYPE zglt0003,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0003.
  MOVE-CORRESPONDING p_saida TO wl_zglt0003.

  MOVE-CORRESPONDING wl_zglt0003 TO p_saida.

ENDFORM.

FORM f_exit_zglt0003_0004 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0003_out    TYPE zglt0003_out,
        vl_low             TYPE tvarvc-low,
        ln_conta(10)       TYPE n,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0003_out.
  MOVE-CORRESPONDING p_saida TO wl_zglt0003_out.

  IF wl_zglt0003_out-tipo IS NOT INITIAL.

    SELECT ddtext
      UP TO 1 ROWS
           FROM dd07t
           INTO wl_zglt0003_out-descr_tip
           WHERE domname EQ 'ZD_TIPO'
             AND domvalue_l EQ wl_zglt0003_out-tipo
             AND ddlanguage EQ 'P'.
    ENDSELECT.

    ln_conta = wl_zglt0003_out-conta_ori.
    IF wl_zglt0003_out-tipo EQ '4'.

      SELECT SINGLE wgbez FROM t023t
        INTO wl_zglt0003_out-descr_ori
        WHERE matkl EQ wl_zglt0003_out-conta_ori
          AND spras EQ 'P'.

    ELSE.

      SELECT txt50
        UP TO 1 ROWS
        FROM skat
        INTO wl_zglt0003_out-descr_ori
        WHERE saknr EQ ln_conta
          AND ktopl EQ '0050'
          AND spras EQ 'P'.
      ENDSELECT.

    ENDIF.

    ln_conta = wl_zglt0003_out-conta_cut.

    SELECT txt50
      UP TO 1 ROWS
      FROM skat
      INTO wl_zglt0003_out-descr_cut
      WHERE saknr EQ ln_conta
        AND ktopl EQ '0050'
        AND spras EQ 'P'.
    ENDSELECT.

  ENDIF.

  MOVE-CORRESPONDING wl_zglt0003_out TO p_saida.

ENDFORM.

FORM f_exit_zglt0003_0005 CHANGING p_registro_manter TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0003_out    TYPE zglt0003_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  DATA: ln_conta(10) TYPE n.

  CLEAR: wl_zglt0003_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0003_out.

  IF wl_zglt0003_out-tipo IS NOT INITIAL.

    SELECT ddtext
      UP TO 1 ROWS
           FROM dd07t
           INTO wl_zglt0003_out-descr_tip
           WHERE domname EQ 'ZD_TIPO'
             AND domvalue_l EQ wl_zglt0003_out-tipo
             AND ddlanguage EQ 'P'.
    ENDSELECT.

    ln_conta = wl_zglt0003_out-conta_ori.
    IF wl_zglt0003_out-tipo EQ '4'.

      SELECT SINGLE wgbez FROM t023t
        INTO wl_zglt0003_out-descr_ori
        WHERE matkl EQ wl_zglt0003_out-conta_ori
          AND spras EQ 'P'.

    ELSE.

      SELECT txt50
        UP TO 1 ROWS
        FROM skat
        INTO wl_zglt0003_out-descr_ori
        WHERE saknr EQ ln_conta
          AND ktopl EQ '0050'
          AND spras EQ 'P'.
      ENDSELECT.

    ENDIF.

    ln_conta = wl_zglt0003_out-conta_cut.

    SELECT txt50
      UP TO 1 ROWS
      FROM skat
      INTO wl_zglt0003_out-descr_cut
      WHERE saknr EQ ln_conta
        AND spras EQ 'P'.
    ENDSELECT.

  ENDIF.

  MOVE-CORRESPONDING wl_zglt0003_out TO p_registro_manter.

ENDFORM.
FORM f_exit_zglt0003_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZGLT0003_OUT'. "= 'ZSDT0343 T2'.
    p_outputlen = 15.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0003_OUT'
   AND   p_field       = 'DESCR_TIP'.
    p_outputlen = 20.
    p_scrtext_l = 'Descrição Tipo'.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0003_OUT'
   AND   p_field       = 'CONTA_ORI'.
    p_outputlen = 15.
    p_scrtext_l = 'Conta Origem'.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0003_OUT'
   AND   p_field       = 'DESCR_ORI'.
    p_outputlen = 50.
    p_scrtext_l = 'Descrição Conta Origem'.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0003_OUT'
   AND   p_field       = 'CONTA_CUT'.
    p_outputlen = 15.
    p_scrtext_l = 'Conta CutOff'.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0003_OUT'
   AND   p_field       = 'DESCR_CUT'.
    p_outputlen = 50.
    p_scrtext_l = 'Descrição Conta CutOff'.
  ENDIF.

ENDFORM.
FORM  f_exit_zglt0003_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zglt0003'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.

FORM f_exit_zglt0003_0017 USING p_tipo.

  FIELD-SYMBOLS: <fs_tipo> TYPE any.

  IF p_tipo = '0001'.

    ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>-TIPO') TO <fs_tipo>.

    IF <fs_tipo> IS ASSIGNED AND <fs_tipo> EQ '4'.

      PERFORM f4_val_conta_orim USING '<FS_WA_REGISTRO_MANTER>-CONTA_ORI'
                                      '<FS_WA_REGISTRO_MANTER>-DESCR_ORI'.

    ELSE.

      PERFORM f4_val_conta_ori USING '<FS_WA_REGISTRO_MANTER>-CONTA_ORI'
                                     '<FS_WA_REGISTRO_MANTER>-DESCR_ORI'.
    ENDIF.
  ENDIF.

  IF p_tipo = '0002'.
    PERFORM f4_val_conta_cut USING '<FS_WA_REGISTRO_MANTER>-CONTA_CUT'
                                   '<FS_WA_REGISTRO_MANTER>-DESCR_CUT'.
  ENDIF.

  IF p_tipo = '0003'.
    PERFORM f4_val_tipo USING      '<FS_WA_REGISTRO_MANTER>-TIPO'
                                   '<FS_WA_REGISTRO_MANTER>-DESCR_TIP'.
  ENDIF.

ENDFORM.

FORM f4_val_conta_ori USING p_cod TYPE help_info-dynprofld
                            p_desc TYPE help_info-dynprofld.
  DATA: BEGIN OF t_skat OCCURS 0,
          hkont	TYPE hkont,
          txt50	TYPE txt50,
        END OF t_skat.

  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
  DATA: c_pt    TYPE langu VALUE 'PT',
        c_ktopl TYPE ktopl VALUE '0050'.

  SELECT saknr txt50
    FROM  skat INTO TABLE t_skat
    WHERE spras = c_pt
      AND ktopl = c_ktopl.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CONTA_ORI'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Contas Razão'
        value_org       = 'S'
      TABLES
        value_tab       = t_skat
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
  ENDIF.

ENDFORM.
FORM f4_val_conta_orim USING p_cod TYPE help_info-dynprofld
                             p_desc TYPE help_info-dynprofld.

  DATA: BEGIN OF t_t023t OCCURS 0,
          matkl	TYPE t023t-matkl,
          wgbez	TYPE t023t-wgbez,
        END OF t_t023t.

  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT matkl wgbez FROM t023t INTO TABLE t_t023t
    WHERE spras EQ 'P'.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CONTA_ORI'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Conta Grupo(Estoque)'
        value_org       = 'S'
      TABLES
        value_tab       = t_t023t
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
  ENDIF.

ENDFORM.
FORM f4_val_conta_cut USING p_cod TYPE help_info-dynprofld
                            p_desc TYPE help_info-dynprofld.

  DATA: BEGIN OF t_skat OCCURS 0,
          hkont	TYPE hkont,
          txt50	TYPE txt50,
        END OF t_skat.

  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
  DATA: c_pt    TYPE langu VALUE 'PT',
        c_ktopl TYPE ktopl VALUE '0050'.

  SELECT saknr txt50
    FROM  skat INTO TABLE t_skat
    WHERE spras = c_pt
      AND ktopl = c_ktopl.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CONTA_ORI'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Contas Razão'
        value_org       = 'S'
      TABLES
        value_tab       = t_skat
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
  ENDIF.

ENDFORM.
FORM f4_val_tipo USING p_cod TYPE help_info-dynprofld
                            p_desc TYPE help_info-dynprofld.

  DATA: BEGIN OF t_dd07t OCCURS 0,
          domvalue_l TYPE domvalue_l,
          ddtext     TYPE val_text,
        END OF t_dd07t.

  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT domvalue_l ddtext
         FROM dd07t INTO TABLE t_dd07t
         WHERE domname EQ 'ZD_TIPO'
           AND ddlanguage EQ 'P'.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TIPO'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Tipo'
        value_org       = 'S'
      TABLES
        value_tab       = t_dd07t
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
  ENDIF.

ENDFORM.
