*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0199_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0199_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

  " SHDB
  DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
        wa_bdcdata LIKE LINE OF ti_bdcdata.
  DATA: BEGIN OF it_msg OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF it_msg.
  DATA: wl_mode(1).


FORM f_exit_zfit0199_0001 CHANGING p_registro_manter TYPE any.


ENDFORM.

FORM f_exit_zfit0199_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0199 TYPE zfit0199.

  CLEAR: wl_zfit0199.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0199.

  CLEAR: p_error.

  IF  wl_zfit0199-tp_lcto       IS INITIAL.

    p_error = abap_true.
    MESSAGE s032(15) DISPLAY LIKE 'E'. "Dados incompletos; preencher todos os campos
    EXIT.

  ENDIF.

*------------------------------
*---- valida Conta
*------------------------------
  IF wl_zfit0199-tp_lcto IS NOT INITIAL.

    SELECT tp_lcto UP TO 1 ROWS
      INTO @DATA(l_tp_lcto)
      FROM zglt031
      WHERE tp_lcto = @wl_zfit0199-tp_lcto.
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

FORM f_exit_zfit0199_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0199 TYPE zfit0199.
  DATA: wl_zfit0199_out TYPE zfit0199_out.

  CLEAR: wl_zfit0199.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0199.

  IF wl_zfit0199-tp_lcto IS NOT INITIAL.

*    DATA: c_pt    TYPE langu VALUE 'PT',
*          c_ktopl TYPE ktopl VALUE '0050'.

    UNPACK wl_zfit0199-tp_lcto TO wl_zfit0199-tp_lcto.

    SELECT SINGLE tp_lcto descricao
      FROM zglt031
      INTO (wl_zfit0199-tp_lcto, wl_zfit0199-descricao)
      WHERE tp_lcto = wl_zfit0199-tp_lcto.

    IF sy-subrc <> 0.
      CLEAR wl_zfit0199-descricao.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zfit0199 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0199_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0199_out TYPE zfit0199_out.
*  DATA: t_dd07v TYPE TABLE OF dd07v,
*        s_dd07v TYPE dd07v.
*  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zfit0199_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0199_out.

************************************************ RJF

*  IF wl_zfit0199_out-tipo_reflexa_pl IS NOT INITIAL.
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
*    gv_domvalue_l = wl_zfit0199_out-tipo_reflexa_pl.
*
*    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
*    IF sy-subrc EQ 0.
*      CONCATENATE wl_zfit0199_out-tipo_reflexa_pl '-' s_dd07v-ddtext INTO wl_zfit0199_out-tipo_reflexa_pl SEPARATED BY space.
*    ENDIF.
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0199_out TO p_saida.

********************************************** RJF

ENDFORM.

FORM f_exit_zfit0199_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zfit0199'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.

ENDFORM.

FORM f_exit_zfit0199_0008_v2 CHANGING  p_fcat_out TYPE lvc_s_fcat.

  IF  p_fcat_out-ref_table EQ 'ZFIT0199_OUT' AND
      p_fcat_out-ref_field EQ 'TP_LCTO'.
      p_fcat_out-hotspot    = abap_true.
*    p_fcat_out-scrtext_l  = 'Codigo de Tipo de Lançamento'.
*    p_fcat_out-col_pos    = 8.
*    p_fcat_out-outputlen  = 40.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0199_0016 USING p_ucomm TYPE sy-ucomm
                          CHANGING p_registro_manter TYPE any
                                             p_saida TYPE any.

  DATA: wl_zfit0199 TYPE zfit0199.
  DATA: wl_zfit0199_out TYPE zfit0199_out.

  CLEAR: wl_zfit0199.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0199.

  IF wl_zfit0199-tp_lcto IS NOT INITIAL.

    DATA: c_pt    TYPE langu VALUE 'PT',
          c_ktopl TYPE ktopl VALUE '0050'.

    UNPACK wl_zfit0199-tp_lcto TO wl_zfit0199-tp_lcto.

    SELECT SINGLE tp_lcto descricao
      FROM zglt031
      INTO (wl_zfit0199-tp_lcto, wl_zfit0199-descricao)
      WHERE tp_lcto = wl_zfit0199-tp_lcto
.

    IF sy-subrc <> 0.
      CLEAR wl_zfit0199_out-descricao.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zfit0199 TO p_saida.
  MOVE-CORRESPONDING wl_zfit0199 TO p_registro_manter.

ENDFORM.
FORM f_exit_zfit0199_0017 USING p_tipo.

  IF p_tipo = '0003'.
    PERFORM f4_val_hkont USING '<FS_WA_REGISTRO_MANTER>-TP_LCTO'
                               '<FS_WA_REGISTRO_MANTER>-DESCRICAO'.
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
  DATA: BEGIN OF t_zglt031 OCCURS 0,
          tp_lcto	  TYPE zglt031-tp_lcto,
          descricao	TYPE zglt031-descricao,
        END OF t_zglt031.

*  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
*  DATA: c_pt    TYPE langu VALUE 'PT',
*        c_ktopl TYPE ktopl VALUE '0050'.

*  READ TABLE t_return ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY fieldname = 'F0001'.
*  IF sy-subrc = 0.

  SELECT tp_lcto descricao
    FROM  zglt031 INTO TABLE t_zglt031.
*    WHERE spras = c_pt
*      AND ktopl = c_ktopl.

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
        retfield        = 'TP_LCTO'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Modelo'
        value_org       = 'S'
      TABLES
        value_tab       = t_zglt031
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
FORM f_exit_zfit0199_0018   USING  p_saida TYPE any
                                   p_column_id TYPE lvc_fname
                                   p_row_id TYPE lvc_index.
  DATA: wl_zfit0199_out TYPE zfit0199_out.
  IF  p_saida IS NOT INITIAL.
    wl_zfit0199_out = CORRESPONDING #( p_saida ).
    IF p_column_id EQ 'TP_LCTO'.
      PERFORM f_bdc_data
                          USING:
            'ZGL013'  '0100'  'X'  ''               ' ',
            ''          ''      ''   'BDC_OKCODE'	    '=DISPLA'.
      PERFORM f_bdc_data
                          USING:
            'ZGL013'  '0100'  'X'  ''               ' ',
            ''          ''      ''   'BDC_CURSOR'	    'WG_ZGLT031-TP_LCTO',
            ''          ''      ''   'BDC_OKCODE'	    '=SEARCH',
            ''          ''      ''   'WG_ZGLT031-TP_LCTO'    wl_zfit0199_out-tp_lcto.

      REFRESH it_msg.
      wl_mode = 'E'.
      CALL TRANSACTION 'ZGL014' USING ti_bdcdata
           MODE wl_mode
           MESSAGES INTO it_msg.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data USING p_program p_dynpro p_start p_fnam p_fval.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.
ENDFORM.                    " F_BDC_DATA
