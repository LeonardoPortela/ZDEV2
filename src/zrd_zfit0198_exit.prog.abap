*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0198_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0198_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zfit0198_0001 CHANGING p_registro_manter TYPE any.


ENDFORM.

FORM f_exit_zfit0198_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0198 TYPE zfit0198.

  CLEAR: wl_zfit0198.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0198.

  CLEAR: p_error.

  IF  wl_zfit0198-hkont       IS INITIAL.

    p_error = abap_true.
    MESSAGE s032(15) DISPLAY LIKE 'E'. "Dados incompletos; preencher todos os campos
    EXIT.

  ENDIF.

*------------------------------
*---- valida Conta
*------------------------------
  IF wl_zfit0198-hkont IS NOT INITIAL.

    SELECT saknr UP TO 1 ROWS
      INTO @DATA(l_saknr)
      FROM skat
      WHERE saknr = @wl_zfit0198-hkont.
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

ENDFORM.

FORM f_exit_zfit0198_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0198 TYPE zfit0198.
  DATA: wl_zfit0198_out TYPE zfit0198_out.

  CLEAR: wl_zfit0198.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0198.

  IF wl_zfit0198-hkont IS NOT INITIAL.

    DATA: c_pt    TYPE langu VALUE 'PT',
          c_ktopl TYPE ktopl VALUE '0050'.

    UNPACK wl_zfit0198-hkont TO wl_zfit0198-hkont.

    SELECT SINGLE saknr txt50
      FROM skat
      INTO (wl_zfit0198-hkont, wl_zfit0198-hkont_nome)
      WHERE saknr = wl_zfit0198-hkont
        AND spras = c_pt
        AND ktopl = c_ktopl.

    IF sy-subrc <> 0.
      CLEAR wl_zfit0198_out-hkont_nome.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zfit0198 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0198_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0198_out TYPE zfit0198_out.
*  DATA: t_dd07v TYPE TABLE OF dd07v,
*        s_dd07v TYPE dd07v.
*  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zfit0198_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0198_out.

************************************************ RJF

*  IF wl_zfit0198_out-tipo_reflexa_pl IS NOT INITIAL.
*
*    CLEAR t_dd07v[].
*    CALL FUNCTION 'GET_DOMAIN_VALUES'
*      EXPORTING
*        domname         = 'ZDTP_REFLEXA_PL'
*      TABLES
*        values_tab      = t_dd07v
*      EXCEPTIONS
*        no_values_found = 1
*        OTHERS          = 2.
*
*    gv_domvalue_l = wl_zfit0198_out-tipo_reflexa_pl.
*
*    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
*    IF sy-subrc EQ 0.
*      CONCATENATE wl_zfit0198_out-tipo_reflexa_pl '-' s_dd07v-ddtext INTO wl_zfit0198_out-tipo_reflexa_pl SEPARATED BY space.
*    ENDIF.
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0198_out TO p_saida.

********************************************** RJF

ENDFORM.

FORM f_exit_zfit0198_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zfit0198'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.

ENDFORM.

FORM f_exit_zfit0198_0008_v2 CHANGING  p_fcat_out TYPE lvc_s_fcat.

*  IF  p_fcat_out-ref_table ='zfit0198_OUT' AND
*   p_fcat_out-ref_field      = 'TIPO_REFLEXA_PL'.
*    p_fcat_out-scrtext_l = 'Tipo Reflexa PL'.
*    p_fcat_out-col_pos = 8.
*    p_fcat_out-outputlen = 40.
*  ENDIF.

ENDFORM.

FORM f_exit_zfit0198_0016 USING p_ucomm TYPE sy-ucomm
                          CHANGING p_registro_manter TYPE any
                                             p_saida TYPE any.

  DATA: wl_zfit0198 TYPE zfit0198.
  DATA: wl_zfit0198_out TYPE zfit0198_out.

  CLEAR: wl_zfit0198.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0198.

  IF wl_zfit0198-hkont IS NOT INITIAL.

    DATA: c_pt    TYPE langu VALUE 'PT',
          c_ktopl TYPE ktopl VALUE '0050'.

    UNPACK wl_zfit0198-hkont TO wl_zfit0198-hkont.

    SELECT SINGLE saknr txt50
      FROM skat
      INTO (wl_zfit0198-hkont, wl_zfit0198-hkont_nome)
      WHERE saknr = wl_zfit0198-hkont
        AND spras = c_pt
        AND ktopl = c_ktopl.

    IF sy-subrc <> 0.
      CLEAR wl_zfit0198_out-hkont_nome.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zfit0198 TO p_saida.
  MOVE-CORRESPONDING wl_zfit0198 TO p_registro_manter.

ENDFORM.
FORM f_exit_zfit0198_0017 USING p_tipo.

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

**====>  Tabelas internas
*  DATA: BEGIN OF t_emp OCCURS 0,
*          bukrs TYPE t001-bukrs,
*          name  TYPE t001-butxt,
*        END OF t_emp.
*
*  CLEAR t_return.
**  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
*  DATA: t_mapping TYPE STANDARD TABLE OF dselc.
*
**====>  Work Area
*  DATA: s_return  TYPE ddshretval.
*  DATA: s_mapping TYPE dselc.
*
*  SELECT bukrs butxt
*    FROM  t001 INTO TABLE t_emp.
*
*  IF sy-subrc = 0.
*
*    s_mapping-fldname     = 'F0001'.
*    s_mapping-dyfldname   = p_cod.
*    APPEND s_mapping TO t_mapping.
*    CLEAR s_mapping.
*
*    s_mapping-fldname     = 'F0002'.
*    s_mapping-dyfldname   = p_desc.
*    APPEND s_mapping TO t_mapping.
*    CLEAR s_mapping.
*
*    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*      EXPORTING
*        retfield        = 'INVESTIDORA'
*        dynpprog        = sy-cprog
*        dynpnr          = sy-dynnr
*        dynprofield     = p_cod
*        window_title    = 'Empresa'
*        value_org       = 'S'
*      TABLES
*        value_tab       = t_emp
*        return_tab      = t_return
*        dynpfld_mapping = t_mapping
*      EXCEPTIONS
*        parameter_error = 1
*        no_values_found = 2
*        OTHERS          = 3.
*
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDIF.



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
