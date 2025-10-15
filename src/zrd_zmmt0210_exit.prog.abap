*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0210_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0210_exit.

DATA: lv_cd_departamento TYPE zde_departamento,
      lv_ck_consulta     TYPE char01.

FORM f_exit_zmmt0210_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0210        TYPE zmmt0210.

  CLEAR: wl_zmmt0210, lv_cd_departamento.

  IMPORT lv_cd_departamento FROM MEMORY ID 'CD_DEPARTAMENTO'.
  IF sy-subrc = 0.
    wl_zmmt0210-cd_departamento = lv_cd_departamento.
  ENDIF.

  wl_zmmt0210-user_create  = sy-uname.
  wl_zmmt0210-date_create  = sy-datum.
  wl_zmmt0210-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wl_zmmt0210 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0210_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zmmt0210 TYPE zmmt0210.
  CLEAR:  wa_zmmt0210.

  DATA: var_answer TYPE c.

  DATA: lit_zmmt0210 TYPE TABLE OF zmmt0210.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0210.

  CLEAR: p_error.

  SELECT SINGLE cd_departamento
    INTO @DATA(_cd_departamento)
    FROM zmmt0072
   WHERE cd_departamento = @wa_zmmt0210-cd_departamento.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Cod.Depatamento Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE bukrs
    INTO @DATA(_bukrs)
    FROM t001
   WHERE bukrs = @wa_zmmt0210-bukrs.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Empresa Incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE bsart
    INTO @DATA(_bsart)
    FROM t161
   WHERE bsart = @wa_zmmt0210-bsart.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Tipo Doc.Compra Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0210_0003 CHANGING p_registro_manter TYPE any.

  DATA: lc_obj_departamento TYPE REF TO zcl_mm_departamento,
        t_zmmt0210          TYPE TABLE OF zmmt0210,
        wa_zmmt0210         TYPE zmmt0210.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0210.

  wa_zmmt0210-date_create = sy-datum.
  wa_zmmt0210-time_create = sy-uzeit.
  wa_zmmt0210-user_create = sy-uname.

  MOVE-CORRESPONDING wa_zmmt0210 TO p_registro_manter.
  APPEND wa_zmmt0210             TO t_zmmt0210.

*----------------------------
* gravar registro
*----------------------------
  CREATE OBJECT lc_obj_departamento
    EXPORTING
      i_cd_departamento = wa_zmmt0210-cd_departamento.

  TRY.
      lc_obj_departamento->set_empresa_tipo_pedido( i_empresa_tipo_pedido = t_zmmt0210
                                                    i_acao_gravar         = 'I' ).
      lc_obj_departamento->gravar_registro( ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).
  ENDTRY.

ENDFORM.

FORM f_exit_zmmt0210_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0210 TYPE zmmt0210.

  IF lv_cd_departamento IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-CD_DEPARTAMENTO'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0210.
  MOVE-CORRESPONDING wl_zmmt0210       TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0210_0006 USING p_saida TYPE any
                       CHANGING p_error.

  DATA: lc_obj_departamento TYPE REF TO zcl_mm_departamento,
        wl_zmmt0210         TYPE zmmt0210,
        t_zmmt0210          TYPE TABLE OF zmmt0210.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0210.
  MOVE sy-mandt              TO wl_zmmt0210-mandt.
  APPEND wl_zmmt0210         TO t_zmmt0210.

*----------------------------
* deletar registro
*----------------------------
  CREATE OBJECT lc_obj_departamento
    EXPORTING
      i_cd_departamento = wl_zmmt0210-cd_departamento.

  TRY.
      lc_obj_departamento->set_empresa_tipo_pedido( i_empresa_tipo_pedido = t_zmmt0210
                                                    i_acao_gravar         = 'D' ).
      lc_obj_departamento->gravar_registro( ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).
  ENDTRY.

ENDFORM.

FORM f_exit_zmmt0210_0008  CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZMMT0210_OUT' AND
       p_field       = 'CD_DEPARTAMENTO'.
    p_scrtext_l    = 'Cd.Departamento'.
    p_outputlen    = 16.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0210_OUT' AND
     p_field       = 'BUKRS'.
    p_scrtext_l    = 'Empresa'.
    p_outputlen    = 16.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0210_OUT' AND
     p_field       = 'BSART'.
    p_scrtext_l    = 'Tipo Doc.Compra'.
    p_outputlen    = 16.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0210_OUT' AND
     p_field       = 'GERA_MIRO_POS_MIGO'.
    p_scrtext_l    = 'Gerar MIRO após MIGO'.
    p_check        = abap_true.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0210_OUT' AND
     p_field       = 'USER_CREATE'.
    p_scrtext_l    = 'Usuário'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0210_OUT' AND
     p_field       = 'DATE_CREATE'.
    p_scrtext_l    = 'Data'.
    p_outputlen    = 12.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0210_OUT' AND
     p_field       = 'TIME_CREATE'.
    p_scrtext_l    = 'Hora'.
    p_outputlen    = 12.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0210_0009  TABLES it_excl_toolbar
                            USING p_db_tab.

  FREE: lv_ck_consulta.

  IMPORT lv_ck_consulta FROM MEMORY ID 'CK_CONSULTA'.

  IF sy-subrc = 0 AND lv_ck_consulta = abap_true.
    APPEND 'Novo'         TO it_excl_toolbar.
    APPEND 'Modificar'    TO it_excl_toolbar.
    APPEND 'Deletar'      TO it_excl_toolbar.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0210_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZMMT0210'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zmmt0210_0020.

  SUBMIT zregister_data WITH p_db_tab = 'ZMMT0211'
                        WITH p_stcnam = 'ZMMT0211_OUT'
                        WITH p_scmant = '0296'
                        WITH p_nocwid = abap_true
                        WITH p_nosave = abap_true
                        WITH p_act_01 = 'Visualizar Log de Alteracoes'
                        WITH p_title  = 'Parametros CFOP exceção Miro automatica ZMM0110'
                    AND RETURN.

ENDFORM.
