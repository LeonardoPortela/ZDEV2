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
REPORT zrd_zfit0014_exit.

FORM f_exit_zfit0014_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0014 TYPE zfit0014.

  CLEAR: wl_zfit0014.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0014.

  wl_zfit0014-c_user = sy-uname.
  wl_zfit0014-hora = sy-uzeit.
  wl_zfit0014-data = sy-datum.

  MOVE-CORRESPONDING wl_zfit0014 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0014_0002 USING p_registro_manter TYPE any
                             CHANGING p_error.

  DATA: wl_zfit0014 TYPE zfit0014_out.
  DATA: lv_num(10).

  CLEAR: wl_zfit0014.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0014.

  IF wl_zfit0014-tipo_i IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Tipo de Imposto Campo obrigatório!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zfit0014-data_ini IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Data Início campo obrigatório!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zfit0014-data_fim IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Data Fim campo obrigatório!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zfit0014-data_ini GT wl_zfit0014-data_fim AND ( wl_zfit0014-data_fim NE '00000000' ).
    p_error = abap_true.
    MESSAGE 'Data (De) não pode ser maior que (Até)!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zfit0014-desc_tipo_i IS INITIAL.
    SELECT ddtext UP TO 1 ROWS
      FROM dd07t
      INTO @wl_zfit0014-desc_tipo_i
      WHERE ddlanguage EQ @sy-langu
        AND domname    EQ 'ZDTIPO_I'
        AND domvalue_l EQ @wl_zfit0014-tipo_i.
    ENDSELECT.
  ENDIF.

  SELECT * FROM zfit0014
    INTO TABLE @DATA(it_found)
    WHERE tipo_i    EQ @wl_zfit0014-tipo_i.
*      AND data_ini  EQ @wl_zfit0014-data_ini.

  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'NOVO'.

    LOOP AT it_found ASSIGNING FIELD-SYMBOL(<fs_found>).
      IF ( wl_zfit0014-data_ini GE <fs_found>-data_ini AND wl_zfit0014-data_ini LE <fs_found>-data_fim )
      OR ( wl_zfit0014-data_fim GE <fs_found>-data_ini AND wl_zfit0014-data_fim LE <fs_found>-data_fim )
      OR ( wl_zfit0014-data_ini GE <fs_found>-data_ini AND wl_zfit0014-data_fim LE <fs_found>-data_fim )
      OR ( wl_zfit0014-data_ini LE <fs_found>-data_ini AND wl_zfit0014-data_fim GE <fs_found>-data_fim ).
        p_error = abap_true.
        MESSAGE 'Já existe cadastro ativo para o período total/parcial informado!' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  wl_zfit0014-c_user = sy-uname.
  wl_zfit0014-hora = sy-uzeit.
  wl_zfit0014-data = sy-datum.

  MOVE-CORRESPONDING wl_zfit0014 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0014_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0014 TYPE zfit0014.

  CLEAR: wl_zfit0014.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0014.

  wl_zfit0014-c_user = sy-uname.
  wl_zfit0014-hora = sy-uzeit.
  wl_zfit0014-data = sy-datum.

  MOVE-CORRESPONDING wl_zfit0014 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0014_0004 CHANGING p_saida     TYPE zfit0014_out.

  DATA: lc_saida TYPE zfit0014_out.

  MOVE-CORRESPONDING p_saida TO lc_saida.

  MOVE-CORRESPONDING lc_saida TO p_saida.

ENDFORM.

FORM f_exit_zfit0014_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0014 TYPE zfit0014_out.

  CLEAR: wl_zfit0014.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0014.

  MOVE-CORRESPONDING wl_zfit0014 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0014_0010 TABLES it_zsdt0343.

  DATA: it_zfit0014 TYPE STANDARD TABLE OF zfit0014_out.

  it_zfit0014[] = CORRESPONDING #( it_zsdt0343[] ).

  IF it_zfit0014[] IS NOT INITIAL.

    SELECT domvalue_l, ddtext
      FROM dd07t
      INTO TABLE @DATA(it_desc)
      WHERE ddlanguage EQ @sy-langu
        AND domname    EQ 'ZDTIPO_I'.

    LOOP AT it_zfit0014 ASSIGNING FIELD-SYMBOL(<fs_zsdt0343>).

      READ TABLE it_desc INTO DATA(wa_desc) WITH KEY domvalue_l = <fs_zsdt0343>-tipo_i.
      IF sy-subrc IS INITIAL.
        <fs_zsdt0343>-desc_tipo_i = wa_desc-ddtext.
      ENDIF.
    ENDLOOP.

    it_zsdt0343[] = CORRESPONDING #( it_zfit0014[] ).
  ENDIF.

ENDFORM.

FORM f_exit_zfit0014_0019 USING p_registro_search TYPE any
                       CHANGING p_error
                                p_cond TYPE rsds_where.

  DATA: wl_zfit0014 TYPE zfit0014_out.

  MOVE-CORRESPONDING p_registro_search TO wl_zfit0014.
  MOVE-CORRESPONDING wl_zfit0014 TO p_registro_search.

ENDFORM.

FORM f_exit_zfit0014_0013  TABLES p_tables.

ENDFORM.
FORM f_exit_zfit0014_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZFIT0014_OUT'.
    p_outputlen = 12.
  ENDIF.

ENDFORM.
FORM  f_exit_zfit0014_0009 TABLES it_excl_toolbar
                              USING p_db_tab.

ENDFORM.
FORM f_exit_zfit0014_0016 USING p_ucomm TYPE sy-ucomm
                             CHANGING p_registro_manter TYPE any
                                      p_saida TYPE any.

  DATA: wl_zfit0014 TYPE zfit0014_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0014.

  MOVE-CORRESPONDING wl_zfit0014 TO p_registro_manter.
  MOVE-CORRESPONDING wl_zfit0014 TO p_saida.


ENDFORM.
FORM f_exit_zfit0014_0017 USING p_tipo.
*  IF p_tipo = '0001'.
*    PERFORM f4_val_kschl USING '<FS_WA_REGISTRO_MANTER>-KSCHL'
*                               '<FS_WA_REGISTRO_MANTER>-VTEXT'.
*  ENDIF.
ENDFORM.

*FORM f_exit_zfit0014_0020 CHANGING p_refresh_selecao.
*
*  p_refresh_selecao = abap_true.
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
