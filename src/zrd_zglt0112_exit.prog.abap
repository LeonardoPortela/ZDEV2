*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0112_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0112_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zglt0112_0001 CHANGING p_registro_manter TYPE any.


ENDFORM.

FORM f_exit_zglt0112_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt0112 TYPE zglt0112.

  CLEAR: wl_zglt0112.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0112.

  CLEAR: p_error.

  IF  wl_zglt0112-investidora IS INITIAL OR
      wl_zglt0112-investida   IS INITIAL OR
      wl_zglt0112-hkont       IS INITIAL OR
      wl_zglt0112-tipo_reflexa_pl IS INITIAL.

    p_error = abap_true.
    MESSAGE s032(15) DISPLAY LIKE 'E'. "Dados incompletos; preencher todos os campos
    EXIT.

  ENDIF.


*------------------------------
*---- valida empresa
*------------------------------
  IF wl_zglt0112-investidora IS NOT INITIAL.
    SELECT bukrs
      INTO @DATA(l_bukrs)
      FROM t001
        UP TO 1 ROWS
     WHERE bukrs = @wl_zglt0112-investidora.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE 'Empresa Informada está Incorreta.' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.

  ENDIF.

  IF wl_zglt0112-investida IS NOT INITIAL.
    SELECT  bukrs
      INTO l_bukrs
      FROM t001
        UP TO 1 ROWS
     WHERE bukrs = wl_zglt0112-investida.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE 'Empresa Informada está Incorreta.' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.

  ENDIF.

*------------------------------
*---- valida Conta
*------------------------------
  IF wl_zglt0112-hkont IS NOT INITIAL.

    SELECT saknr UP TO 1 ROWS
      INTO @DATA(l_saknr)
      FROM skat
      WHERE saknr = @wl_zglt0112-hkont.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE 'Conta Informada está Incorreta.' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.

  ENDIF.

*------------------------------
*---- valida Tipo reflexa PL
*------------------------------

  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.


**  Begin of CS2023000082  #103662 FF   21.02.2023
  IF wl_zglt0112-tipo_reflexa_pl IS NOT INITIAL.

    CLEAR t_dd07v[].
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZDTP_REFLEXA_PL'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zglt0112-tipo_reflexa_pl.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc <> 0.
      MESSAGE 'Tipo Reflexa inválido.' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.

  ENDIF.
**  End of FF  21.02.2023

ENDFORM.

FORM f_exit_zglt0112_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0112 TYPE zglt0112.

  CLEAR: wl_zglt0112.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0112.

  IF wl_zglt0112-investidora IS NOT INITIAL.

    UNPACK wl_zglt0112-investidora TO wl_zglt0112-investidora.

    SELECT SINGLE butxt
      FROM t001
      INTO wl_zglt0112-investidora_nome
      WHERE bukrs = wl_zglt0112-investidora.

    IF sy-subrc <> 0.
      CLEAR wl_zglt0112-investidora_nome.
    ENDIF.

  ENDIF.

  IF wl_zglt0112-investida IS NOT INITIAL.

    UNPACK wl_zglt0112-investida TO wl_zglt0112-investida.

    SELECT SINGLE butxt
      FROM t001
      INTO wl_zglt0112-investida_nome
      WHERE bukrs = wl_zglt0112-investida.

    IF sy-subrc <> 0.
      CLEAR wl_zglt0112-investida_nome.
    ENDIF.

  ENDIF.

  IF wl_zglt0112-hkont IS NOT INITIAL.

    DATA: c_pt    TYPE langu VALUE 'PT',
          c_ktopl TYPE ktopl VALUE '0050'.

    UNPACK wl_zglt0112-hkont TO wl_zglt0112-hkont.

    SELECT SINGLE saknr txt50
      FROM skat
      INTO (wl_zglt0112-hkont, wl_zglt0112-hkont_nome)
      WHERE saknr = wl_zglt0112-hkont
        AND spras = c_pt
        AND ktopl = c_ktopl.

    IF sy-subrc <> 0.
      CLEAR wl_zglt0112-hkont_nome.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zglt0112 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt0112_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0112_out TYPE zglt0112_out.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zglt0112_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0112_out.

************************************************ RJF

  IF wl_zglt0112_out-tipo_reflexa_pl IS NOT INITIAL.

    CLEAR t_dd07v[].
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZDTP_REFLEXA_PL'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zglt0112_out-tipo_reflexa_pl.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      CONCATENATE wl_zglt0112_out-tipo_reflexa_pl '-' s_dd07v-ddtext INTO wl_zglt0112_out-tipo_reflexa_pl SEPARATED BY space.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0112_out TO p_saida.

********************************************** RJF

ENDFORM.

FORM f_exit_zglt0112_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zglt0112'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.

ENDFORM.

FORM f_exit_zglt0112_0008_v2 CHANGING  p_fcat_out TYPE lvc_s_fcat.

  IF  p_fcat_out-ref_table ='ZGLT0112_OUT' AND
   p_fcat_out-ref_field      = 'TIPO_REFLEXA_PL'.
    p_fcat_out-scrtext_l = 'Tipo Reflexa PL'.
    p_fcat_out-col_pos = 8.
    p_fcat_out-outputlen = 40.
  ENDIF.

ENDFORM.

FORM f_exit_zglt0112_0016 USING p_ucomm TYPE sy-ucomm
                          CHANGING p_registro_manter TYPE any
                                             p_saida TYPE any.

  DATA: wl_zglt0112 TYPE zglt0112_out.

  CLEAR: wl_zglt0112.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0112.


  IF wl_zglt0112-investidora IS NOT INITIAL.

    UNPACK wl_zglt0112-investidora TO wl_zglt0112-investidora.

    SELECT SINGLE butxt
      FROM t001
      INTO wl_zglt0112-investidora_nome
      WHERE bukrs = wl_zglt0112-investidora.

    IF sy-subrc <> 0.
      CLEAR wl_zglt0112-investidora_nome.
    ENDIF.

  ENDIF.

  IF wl_zglt0112-investida IS NOT INITIAL.

    UNPACK wl_zglt0112-investida TO wl_zglt0112-investida.

    SELECT SINGLE butxt
      FROM t001
      INTO wl_zglt0112-investida_nome
      WHERE bukrs = wl_zglt0112-investida.

    IF sy-subrc <> 0.
      CLEAR wl_zglt0112-investida_nome.
    ENDIF.

  ENDIF.

  IF wl_zglt0112-hkont IS NOT INITIAL.

    DATA: c_pt    TYPE langu VALUE 'PT',
          c_ktopl TYPE ktopl VALUE '0050'.

    UNPACK wl_zglt0112-hkont TO wl_zglt0112-hkont.

    SELECT SINGLE saknr txt50
      FROM skat
      INTO (wl_zglt0112-hkont, wl_zglt0112-hkont_nome)
      WHERE saknr = wl_zglt0112-hkont
        AND spras = c_pt
        AND ktopl = c_ktopl.

    IF sy-subrc <> 0.
      CLEAR wl_zglt0112-hkont_nome.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zglt0112 TO p_saida.
  MOVE-CORRESPONDING wl_zglt0112 TO p_registro_manter.

ENDFORM.
FORM f_exit_zglt0112_0017 USING p_tipo.


  IF p_tipo = '0001'.
    PERFORM f4_val_bukrs USING '<FS_WA_REGISTRO_MANTER>-INVESTIDORA'
                                '<FS_WA_REGISTRO_MANTER>-INVESTIDORA_NOME'.
  ENDIF.

  IF p_tipo = '0002'.
    PERFORM f4_val_bukrs USING '<FS_WA_REGISTRO_MANTER>-INVESTIDA'
                               '<FS_WA_REGISTRO_MANTER>-INVESTIDA_NOME'.
  ENDIF.

  IF p_tipo = '0003'.
    PERFORM f4_val_hkont USING '<FS_WA_REGISTRO_MANTER>-HKONT'
                               '<FS_WA_REGISTRO_MANTER>-HKONT_NOME'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_VAL
*&---------------------------------------------------------------------*
FORM f4_val_bukrs USING p_cod TYPE help_info-dynprofld
                         p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: BEGIN OF t_emp OCCURS 0,
          bukrs TYPE t001-bukrs,
          name  TYPE t001-butxt,
        END OF t_emp.

  CLEAR t_return.
*  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT bukrs butxt
    FROM  t001 INTO TABLE t_emp.

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
        retfield        = 'INVESTIDORA'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Empresa'
        value_org       = 'S'
      TABLES
        value_tab       = t_emp
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
FORM f4_val_hkont USING p_cod TYPE help_info-dynprofld
                        p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: BEGIN OF t_skat OCCURS 0,
          hkont	TYPE hkont,
          txt50	TYPE txt50,
        END OF t_skat.

*  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
  DATA: c_pt    TYPE langu VALUE 'PT',
        c_ktopl TYPE ktopl VALUE '0050'.

*  READ TABLE t_return ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY fieldname = 'F0001'.
*  IF sy-subrc = 0.

  SELECT saknr txt50
    FROM  skat INTO TABLE t_skat
    WHERE spras = c_pt
      AND ktopl = c_ktopl.

*  ENDIF.

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
        retfield        = 'HKONT'
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
