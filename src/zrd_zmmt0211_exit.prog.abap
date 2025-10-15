*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0211_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0211_exit.

DATA: lv_cd_departamento TYPE zde_departamento,
      lv_ck_consulta     TYPE char01.

FORM f_exit_zmmt0211_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0211 TYPE zmmt0211.

  CLEAR: wl_zmmt0211, lv_cd_departamento.

  IMPORT lv_cd_departamento FROM MEMORY ID 'CD_DEPARTAMENTO'.
  IF sy-subrc = 0.
    wl_zmmt0211-cd_departamento = lv_cd_departamento.
  ENDIF.

  wl_zmmt0211-user_create  = sy-uname.
  wl_zmmt0211-date_create  = sy-datum.
  wl_zmmt0211-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wl_zmmt0211 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0211_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zmmt0211 TYPE zmmt0211.
  CLEAR:  wa_zmmt0211.

  DATA: var_answer TYPE c.

  DATA: lit_zmmt0211 TYPE TABLE OF zmmt0211,
        lr_cfop      TYPE RANGE OF j_1bcfop,
        lw_cfop      LIKE LINE OF lr_cfop.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0211.

  CLEAR: p_error.

  SELECT SINGLE cd_departamento
    INTO @DATA(_cd_departamento)
    FROM zmmt0072
   WHERE cd_departamento = @wa_zmmt0211-cd_departamento.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Cod.Depatamento Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  FREE: lr_cfop, lw_cfop.
  lw_cfop-sign    = 'I'.
  lw_cfop-option  = 'CP'.
  lw_cfop-low     = wa_zmmt0211-cfop && '*'.
  APPEND lw_cfop TO lr_cfop.

  SELECT SINGLE cfop
    INTO @DATA(_cfop)
    FROM j_1bagn
   WHERE cfop IN @lr_cfop.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'CFOP Informado Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0211_0003 CHANGING p_registro_manter TYPE any.

  DATA: lc_obj_departamento TYPE REF TO zcl_mm_departamento,
        t_zmmt0211          TYPE TABLE OF zmmt0211,
        wa_zmmt0211         TYPE zmmt0211.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0211.

  wa_zmmt0211-date_create = sy-datum.
  wa_zmmt0211-time_create = sy-uzeit.
  wa_zmmt0211-user_create = sy-uname.

  MOVE-CORRESPONDING wa_zmmt0211 TO p_registro_manter.
  APPEND wa_zmmt0211             TO t_zmmt0211.

*----------------------------
* gravar registro
*----------------------------
  CREATE OBJECT lc_obj_departamento
    EXPORTING
      i_cd_departamento = wa_zmmt0211-cd_departamento.

  TRY.
      lc_obj_departamento->set_cfop_miro( i_cfop_miro   = t_zmmt0211
                                          i_acao_gravar = 'I' ).
      lc_obj_departamento->gravar_registro( ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).
  ENDTRY.

ENDFORM.

FORM f_exit_zmmt0211_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0211 TYPE zmmt0211.

  IF lv_cd_departamento IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-CD_DEPARTAMENTO'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0211.
  MOVE-CORRESPONDING wl_zmmt0211       TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0211_0006 USING p_saida TYPE any
                       CHANGING p_error.

  DATA: lc_obj_departamento TYPE REF TO zcl_mm_departamento,
        wl_zmmt0211         TYPE zmmt0211,
        t_zmmt0211          TYPE TABLE OF zmmt0211.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0211.
  MOVE sy-mandt              TO wl_zmmt0211-mandt.
  APPEND wl_zmmt0211         TO t_zmmt0211.

*----------------------------
* deletar registro
*----------------------------
  CREATE OBJECT lc_obj_departamento
    EXPORTING
      i_cd_departamento = wl_zmmt0211-cd_departamento.

  TRY.
      lc_obj_departamento->set_cfop_miro( i_cfop_miro   = t_zmmt0211
                                          i_acao_gravar = 'D' ).
      lc_obj_departamento->gravar_registro( ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).
  ENDTRY.

ENDFORM.

FORM f_exit_zmmt0211_0008  CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZMMT0211_OUT' AND
     p_field       = 'CD_DEPARTAMENTO'.
    p_scrtext_l    = 'Cd.Departamento'.
    p_outputlen    = 16.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0211_OUT' AND
     p_field       = 'CFOP'.
    p_scrtext_l    = 'CFOP'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0211_OUT' AND
     p_field       = 'USER_CREATE'.
    p_scrtext_l    = 'Usuário'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0211_OUT' AND
     p_field       = 'DATE_CREATE'.
    p_scrtext_l    = 'Data'.
    p_outputlen    = 12.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0211_OUT' AND
     p_field       = 'TIME_CREATE'.
    p_scrtext_l    = 'Hora'.
    p_outputlen    = 12.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0211_0009  TABLES it_excl_toolbar
                            USING p_db_tab.

  FREE: lv_ck_consulta.

  IMPORT lv_ck_consulta FROM MEMORY ID 'CK_CONSULTA'.

  APPEND 'Modificar'      TO it_excl_toolbar.

  IF sy-subrc = 0 AND lv_ck_consulta = abap_true.
    APPEND 'Novo'         TO it_excl_toolbar.
    APPEND 'Deletar'      TO it_excl_toolbar.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0211_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZMMT0211'
      tabfirst = 'X'.

ENDFORM.
