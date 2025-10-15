*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0032_EXIT - TELA 341
*& Tabela ZSDT0032 Param Exceção Multa/Juro-ZSDT0062
*& transação ZSDT0038-Param Exceção Multa/Juro-ZSDT0062
*&---------------------------------------------------------------------*
*&Autor: Samuel da Mata Cabana - USER STORY 192298
*&Data: 02.10.2025
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0032_exit.

FORM f_exit_zsdt0032_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0032 TYPE zsdt0032.

  CLEAR: wl_zsdt0032.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0032.

  wl_zsdt0032-dt_criacao = sy-datum.
  wl_zsdt0032-hr_criacao = sy-uzeit.
  wl_zsdt0032-us_criacao = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0032 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0032_0002 USING p_registro_manter TYPE zsdt0032
                        CHANGING p_error.

  DATA: g_domain TYPE dd07l-domname,
        g_value  TYPE dd07l-domvalue_l,
        g_subrc  TYPE sy-subrc,
        gwa_tab  TYPE dd07v.

  DATA: wl_zsdt0032 TYPE zsdt0032.

  CLEAR: wl_zsdt0032.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0032.

*  IF wl_zsdt0032-dt_criacao IS NOT INITIAL.
*    wl_zsdt0032-dt_criacao = sy-datum.
*    wl_zsdt0032-hr_criacao = sy-uzeit.
*    wl_zsdt0032-us_criacao = sy-uname.
*  ELSE.
*    wl_zsdt0032-dt_modif   = sy-datum.
*    wl_zsdt0032-hr_modif   = sy-uzeit.
*    wl_zsdt0032-us_modif   = sy-uname.
*  ENDIF.

*  IF wl_zsdt0032-inativo = 'X'.
*    MESSAGE 'Não é permitido edição!' TYPE 'S' DISPLAY LIKE 'E'.
*    p_error = 'X'.
*    EXIT.
*  ENDIF.

  IF p_registro_manter-cliente IS INITIAL.
    MESSAGE 'Campo cliente é obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.


  IF p_registro_manter-cliente IS NOT INITIAL.
    SELECT SINGLE kunnr
      INTO p_registro_manter-cliente
      FROM kna1
     WHERE kunnr = p_registro_manter-cliente.
    IF sy-subrc NE 0.
      MESSAGE 'Cliente não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0032 TO p_registro_manter.

ENDFORM.

FORM  f_exit_zsdt0032_0009 TABLES pt_excl_toolbar

                           USING p_db_tab.

*

  TYPES: BEGIN OF ty_excl_toolbar,

           code TYPE ui_func.

  TYPES: END OF ty_excl_toolbar.



  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,

        wa_excl_toolbar TYPE ty_excl_toolbar.



  FREE: it_excl_toolbar.





  wa_excl_toolbar-code = 'Deletar'.

  APPEND wa_excl_toolbar  TO it_excl_toolbar.



  pt_excl_toolbar[] = it_excl_toolbar[].

ENDFORM.


FORM f_exit_zsdt0032_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0032 TYPE zsdt0032.

  CLEAR: wl_zsdt0032.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0032.

*  IF wl_zsdt0032-dt_criacao IS INITIAL.
*    wl_zsdt0032-dt_criacao = sy-datum.
*    wl_zsdt0032-hr_criacao = sy-uzeit.
*    wl_zsdt0032-us_criacao = sy-uname.
*  ELSE.
*    wl_zsdt0032-dt_modif   = sy-datum.
*    wl_zsdt0032-hr_modif   = sy-uzeit.
*    wl_zsdt0032-us_modif   = sy-uname.
*  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0032 TO p_registro_manter.

ENDFORM.

*FORM f_exit_zsdt0032_0004 CHANGING p_registro_manter TYPE any.
*
*  DATA w_zsdt0032 TYPE zsdt0032.
*
*  CLEAR w_zsdt0032.
*
*  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0032.
**
**  CLEAR: lc_saida-descr_operacao, lc_saida-descr_modal.
**
**  g_domain = 'ZMODAL'.
**  g_value  = lc_saida-cd_modal.
**
**  CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
**    EXPORTING
**      domname  = g_domain
**      value    = g_value
**    IMPORTING
**      dd07v_wa = gwa_tab
**      rc       = g_subrc.
**  IF g_subrc =  0.
**    lc_saida-descr_modal = gwa_tab-ddtext.
**  ENDIF.
**
**  g_domain = 'ZDM_OPER_AQUAV'.
**  g_value  = lc_saida-operacao.
**
**  CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
**    EXPORTING
**      domname  = g_domain
**      value    = g_value
**    IMPORTING
**      dd07v_wa = gwa_tab
**      rc       = g_subrc.
**  IF g_subrc =  0.
**    lc_saida-descr_operacao = gwa_tab-ddtext.
**  ENDIF.
*
*  MOVE-CORRESPONDING w_zsdt0032 TO p_registro_manter.

*ENDFORM.

FORM f_exit_zsdt0032_0005 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0032 TYPE zsdt0032.
  CLEAR: wl_zsdt0032.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0032.

  SELECT SINGLE dt_criacao FROM  zsdt0032 INTO @DATA(zvar_dtcria) WHERE cliente = @wl_zsdt0032-cliente.

  IF zvar_dtcria IS INITIAL.
    wl_zsdt0032-dt_criacao = sy-datum.
    wl_zsdt0032-hr_criacao = sy-uzeit.
    wl_zsdt0032-us_criacao = sy-uname.
  ELSE.
    wl_zsdt0032-dt_modif   = sy-datum.
    wl_zsdt0032-hr_modif   = sy-uzeit.
    wl_zsdt0032-us_modif   = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0032 TO p_saida.


ENDFORM.


*FORM f_exit_zsdt0032_0006 USING p_registro_manter TYPE any
*                           CHANGING p_error.
*
*ENDFORM.

*-Proj.EUDR-10.10.2024-#154904-JT-inicio
*FORM f_exit_zsdt0032_0017 USING p_tipo.
*
**  CASE p_tipo.
**    WHEN '0001'.
**      PERFORM f4_val_param_cd_modal USING '<FS_WA_REGISTRO_MANTER>-CD_MODAL'.
**    WHEN '0002'.
**      PERFORM f4_val_param_operacao USING '<FS_WA_REGISTRO_MANTER>-OPERACAO'.
**  ENDCASE.

*ENDFORM.


*FORM f_exit_zsdt0032_0008 CHANGING p_col_pos
*                                    p_ref_tabname
*                                    p_ref_fieldname
*                                    p_tabname
*                                    p_field
*                                    p_scrtext_l
*                                    p_outputlen
*                                    p_edit
*                                    p_sum
*                                    p_emphasize
*                                    p_just
*                                    p_hotspot
*                                    p_f4
*                                    p_check.

*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'CLIENTE'.
*    p_outputlen    = 18.
*  ENDIF.
*
*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'DESCR_CLIEN'.
*    p_scrtext_l    = 'Descrição Cliente'.
*    p_outputlen    = 25.
*  ENDIF.
*
*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'DT_CRIACAO'.
*    p_scrtext_l    = 'Data Criação'.
*    p_outputlen    = 12.
*  ENDIF.
*
*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'HR_CRIACAO'.
*    p_scrtext_l    = 'Hora Criação'.
*    p_outputlen    = 12.
*  ENDIF.
*
*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'US_CRIACAO'.
*    p_scrtext_l    = 'Usuário Criação'.
*    p_outputlen    = 20.
*  ENDIF.
*
*  IF p_ref_tabname = 'ZLEST0037_OUT' AND
*     p_field       = 'INATIVO'.
*    p_check        = abap_true.
*  ENDIF.
*
*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'DT_MODIF'.
*    p_scrtext_l    = 'Data Modif'.
*    p_outputlen    = 12.
*  ENDIF.
*
*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'HR_MODIF'.
*    p_scrtext_l    = 'Hora Modif'.
*    p_outputlen    = 12.
*  ENDIF.
*
*  IF p_ref_tabname = 'zsdt0032_OUT' AND
*     p_field       = 'US_MODIF'.
*    p_scrtext_l    = 'Usuário Modif'.
*    p_outputlen    = 20.
*  ENDIF.

*ENDFORM.

*FORM f_exit_zsdt0032_0016 USING p_ucomm           TYPE sy-ucomm
*                        CHANGING p_registro_manter TYPE any
*                                 p_saida           TYPE any.
*
**  DATA: w_zsdt0032      TYPE zsdt0032.
**
**  CLEAR: w_zsdt0032.
***
**  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0032.
**
**  IF w_zsdt0032-cd_modal <> '03'.
**    CLEAR w_zsdt0032-operacao.
**  ENDIF.
**
**  MOVE-CORRESPONDING w_zsdt0032       TO p_registro_manter.
*
*ENDFORM.

FORM f_exit_zsdt0032_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'zsdt0032'
      tabfirst = 'X'.

ENDFORM.
