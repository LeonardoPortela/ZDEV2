*&--------------------------------------------------------------------&*
*&                     Relatório Módulo - SD                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 11/02/2025                                              &*
*& Descrição: Cadastro Parâmetro % PIS/COFINS                         &*
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
REPORT zrd_zfit0015_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zfit0015_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0015 TYPE zfit0015.

  CLEAR: wl_zfit0015.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0015.

  wl_zfit0015-c_user = sy-uname.
  wl_zfit0015-hora = sy-uzeit.
  wl_zfit0015-data = sy-datum.

  MOVE-CORRESPONDING wl_zfit0015 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0015_0002 USING p_registro_manter TYPE any
                             CHANGING p_error.

  DATA: wl_zfit0015 TYPE zfit0015_out.
  DATA: lv_num(10).

  CLEAR: wl_zfit0015.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0015.

  IF wl_zfit0015-saknr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Nº conta do Razão Campo obrigatório!' TYPE 'I'.
    EXIT.
  ELSE.

    SELECT txt50
      UP TO 1 ROWS
      FROM skat "ska1
      INTO wl_zfit0015-desc_saknr
      WHERE saknr EQ wl_zfit0015-saknr
        AND spras EQ sy-langu
        AND ktopl EQ '0050'.
    ENDSELECT.

  ENDIF.

  SELECT * FROM zfit0015
    INTO TABLE @DATA(it_found)
    WHERE saknr    EQ @wl_zfit0015-saknr.

  SELECT * FROM zfit0015
    INTO TABLE @DATA(it_foundx)
    WHERE saknr    EQ @wl_zfit0015-saknr
      AND bukrs    EQ @space.

  IF sy-subrc IS INITIAL." AND sy-ucomm EQ 'NOVO'.

    IF wl_zfit0015-bukrs IS NOT INITIAL AND wl_zfit0015-werks IS INITIAL AND wl_zfit0015-kostl IS INITIAL.
* Não permitir pois não faz sentido existir conta e empresa(todas) com conta e empresa específica.
      p_error = abap_true.
      MESSAGE 'Empresa não pode ser criada com anterior todas existente!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wl_zfit0015-bukrs IS NOT INITIAL AND wl_zfit0015-werks IS NOT INITIAL AND wl_zfit0015-kostl IS NOT INITIAL.
      READ TABLE it_found ASSIGNING FIELD-SYMBOL(<fs_found>) WITH KEY bukrs = wl_zfit0015-bukrs
                                                                      werks = wl_zfit0015-werks
                                                                      kostl = wl_zfit0015-kostl. "<<<------"189660 - NMS ------->>>
      IF sy-subrc IS NOT INITIAL.
*        p_error = abap_true.
*        MESSAGE 'Empresa e Filiais são obrigatórias a criação!' TYPE 'I'.
*        EXIT.
      ELSE.
        p_error = abap_true.
        MESSAGE 'Empresa e filial não pode ser criada com anterior todas existente!' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDIF.

    IF wl_zfit0015-bukrs IS NOT INITIAL AND wl_zfit0015-werks IS NOT INITIAL.
      IF wl_zfit0015-kostl IS INITIAL.
        READ TABLE it_found ASSIGNING <fs_found> WITH KEY bukrs = wl_zfit0015-bukrs
                                                          werks = wl_zfit0015-werks. "<<<------"189660 - NMS ------->>>
        IF sy-subrc IS INITIAL.
          p_error = abap_true.
          MESSAGE 'Empresa e Filiais já criadas!' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
**<<<------"189660 - NMS - INI------>>>
*    IF wl_zfit0015-bukrs IS NOT INITIAL AND wl_zfit0015-werks IS INITIAL.
    IF wl_zfit0015-bukrs IS NOT INITIAL AND wl_zfit0015-werks IS INITIAL AND wl_zfit0015-kostl IS NOT INITIAL.
**<<<------"189660 - NMS - FIM------>>>
      READ TABLE it_found ASSIGNING <fs_found> WITH KEY bukrs = wl_zfit0015-bukrs.
      IF sy-subrc IS INITIAL.
        p_error = abap_true.
        MESSAGE 'Filial é obrigatória para a criação!' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDIF.

  ELSE.

    SELECT * FROM zfit0015
      INTO TABLE @it_foundx
      WHERE saknr    EQ @wl_zfit0015-saknr
        AND bukrs    NE @space
        AND werks    EQ @space.

    IF sy-subrc IS INITIAL AND wl_zfit0015-bukrs IS INITIAL AND wl_zfit0015-werks IS INITIAL AND wl_zfit0015-kostl IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Conta contábil não pode ser criada com anterior empresa específica criada!' TYPE 'I'.
      EXIT.
    ENDIF.

*** BUG - 178383 - Inicio - CBRAND
*    IF wl_zfit0015-bukrs IS NOT INITIAL AND wl_zfit0015-werks IS INITIAL AND wl_zfit0015-kostl IS INITIAL.
*
*    ELSE.
*
*      IF wl_zfit0015-saknr IS NOT INITIAL
*        AND wl_zfit0015-bukrs IS INITIAL
*        AND wl_zfit0015-werks IS INITIAL
*        AND wl_zfit0015-kostl IS INITIAL.
*      ELSE.
*        p_error = abap_true.
*        MESSAGE 'Conta Contábil ainda não criada!' TYPE 'I'.
*        EXIT.
*      ENDIF.
*    ENDIF.
*** BUG - 178383 - Fim - CBRAND
  ENDIF.

*  IF wl_zfit0015-bukrs IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Empresa campo obrigatório!' TYPE 'I'.
*    EXIT.
*  ELSE.
*
*    SELECT butxt FROM t001
*      INTO wl_zfit0015-desc_bukrs
*      WHERE bukrs EQ wl_zfit0015-bukrs.
*    ENDSELECT.
*
*  ENDIF.
*
*  IF wl_zfit0015-werks IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Centro campo obrigatório!' TYPE 'I'.
*    EXIT.
*  ELSE.
*
*    SELECT name1 FROM t001w
*      INTO wl_zfit0015-desc_werks
*      WHERE werks EQ wl_zfit0015-werks.
*    ENDSELECT.
*
*  ENDIF.
*
*  IF wl_zfit0015-kostl IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Centro de custo campo obrigatório!' TYPE 'I'.
*    EXIT.
*  ELSE.

  SELECT ltext FROM cskt
    INTO wl_zfit0015-desc_kostl
    WHERE kostl EQ wl_zfit0015-kostl
      AND spras EQ sy-langu
      AND datbi GE sy-datum.
  ENDSELECT.
*  ENDIF.

  wl_zfit0015-c_user = sy-uname.
  wl_zfit0015-hora = sy-uzeit.
  wl_zfit0015-data = sy-datum.

  MOVE-CORRESPONDING wl_zfit0015 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0015_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0015 TYPE zfit0015.

  CLEAR: wl_zfit0015.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0015.

  wl_zfit0015-c_user = sy-uname.
  wl_zfit0015-hora = sy-uzeit.
  wl_zfit0015-data = sy-datum.

  MOVE-CORRESPONDING wl_zfit0015 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0015_0004 CHANGING p_saida     TYPE zfit0015_out.

  DATA: lc_saida TYPE zfit0015_out.

  MOVE-CORRESPONDING p_saida TO lc_saida.

  MOVE-CORRESPONDING lc_saida TO p_saida.

ENDFORM.

FORM f_exit_zfit0015_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0015 TYPE zfit0015_out.

  CLEAR: wl_zfit0015.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0015.

  MOVE-CORRESPONDING wl_zfit0015 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0015_0010 TABLES it_zsdt0343.

  DATA: it_zfit0015 TYPE STANDARD TABLE OF zfit0015_out.

  it_zfit0015[] = CORRESPONDING #( it_zsdt0343[] ).

  IF it_zfit0015[] IS NOT INITIAL.

    SELECT saknr, txt50
      FROM skat "ska1
      INTO TABLE @DATA(it_skat)
      FOR ALL ENTRIES IN @it_zfit0015
      WHERE saknr EQ @it_zfit0015-saknr
        AND spras EQ @sy-langu
        AND ktopl EQ '0050'.

    SELECT bukrs, butxt
      FROM t001
      INTO TABLE @DATA(it_t001)
      FOR ALL ENTRIES IN @it_zfit0015
      WHERE bukrs EQ @it_zfit0015-bukrs.

    SELECT werks, name1 FROM t001w
      INTO TABLE @DATA(it_t001w)
      FOR ALL ENTRIES IN @it_zfit0015
      WHERE werks EQ @it_zfit0015-werks.

    SELECT kostl, ltext FROM cskt
      INTO TABLE @DATA(it_cskt)
      FOR ALL ENTRIES IN @it_zfit0015
      WHERE kostl EQ @it_zfit0015-kostl
        AND spras EQ @sy-langu
        AND datbi GE @sy-datum.

    LOOP AT it_zfit0015 ASSIGNING FIELD-SYMBOL(<fs_zsdt0343>).

      READ TABLE it_skat INTO DATA(wa_skat) WITH KEY saknr = <fs_zsdt0343>-saknr.
      IF sy-subrc IS INITIAL.
        <fs_zsdt0343>-desc_saknr = wa_skat-txt50.
      ENDIF.

      READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = <fs_zsdt0343>-bukrs.
      IF sy-subrc IS INITIAL.
        <fs_zsdt0343>-desc_bukrs = wa_t001-butxt.
      ENDIF.

      READ TABLE it_t001w INTO DATA(wa_t001w) WITH KEY werks = <fs_zsdt0343>-werks.
      IF sy-subrc IS INITIAL.
        <fs_zsdt0343>-desc_werks = wa_t001w-name1.
      ENDIF.

      READ TABLE it_cskt INTO DATA(wa_cskt) WITH KEY kostl = <fs_zsdt0343>-kostl.
      IF sy-subrc IS INITIAL.
        <fs_zsdt0343>-desc_kostl = wa_cskt-ltext.
      ENDIF.
    ENDLOOP.
    it_zsdt0343[] = CORRESPONDING #( it_zfit0015[] ).
  ENDIF.

ENDFORM.

FORM f_exit_zfit0015_0019 USING p_registro_search TYPE any
                       CHANGING p_error
                                p_cond TYPE rsds_where.

  DATA: wl_zfit0015 TYPE zfit0015_out.

  MOVE-CORRESPONDING p_registro_search TO wl_zfit0015.
  MOVE-CORRESPONDING wl_zfit0015 TO p_registro_search.

ENDFORM.

FORM f_exit_zfit0015_0013  TABLES p_tables.

ENDFORM.
FORM f_exit_zfit0015_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZFIT0015_OUT'.
    p_outputlen = 12.
  ENDIF.

ENDFORM.
FORM  f_exit_zfit0015_0009 TABLES it_excl_toolbar
                              USING p_db_tab.

ENDFORM.
FORM f_exit_zfit0015_0016 USING p_ucomm TYPE sy-ucomm
                             CHANGING p_registro_manter TYPE any
                                      p_saida TYPE any.

  DATA: wl_zfit0015 TYPE zfit0015_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0015.


  MOVE-CORRESPONDING wl_zfit0015 TO p_registro_manter.
  MOVE-CORRESPONDING wl_zfit0015 TO p_saida.

ENDFORM.
*FORM f_exit_zfit0015_0017 USING p_tipo.
*
*ENDFORM.

FORM f4_val_kschl USING p_cod TYPE help_info-dynprofld
                        p_desc TYPE help_info-dynprofld.


  FIELD-SYMBOLS: <fs_campo> TYPE any.
  FIELD-SYMBOLS: <fs_campo2> TYPE any.

  ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>-KSCHL') TO <fs_campo>.
  ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>-VTEXT') TO <fs_campo2>.

  IF <fs_campo> IS ASSIGNED.

    SELECT kschl, vtext UP TO 1 ROWS
      FROM t685t
      INTO @DATA(t_desc)
      WHERE spras EQ @sy-langu
        AND kschl EQ @<fs_campo>.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND t_desc-vtext IS NOT INITIAL.
      IF <fs_campo2> IS ASSIGNED.
        <fs_campo2>  = t_desc-vtext.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
FORM f_exit_zfit0015_0017 USING p_tipo.

  IF p_tipo = '0003'.
    PERFORM f4_val_hkont USING '<FS_WA_REGISTRO_MANTER>-SAKNR'
                               '<FS_WA_REGISTRO_MANTER>-SAKNR_NOME'.
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
        retfield        = 'SAKNR'
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
