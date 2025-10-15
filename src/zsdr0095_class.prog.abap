*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_CLASS
*&---------------------------------------------------------------------*



CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: tl_parametros TYPE ustyp_t_parameters,
          vl_txt_button TYPE text40,
          v_time        TYPE t.

    REFRESH: tl_parametros.

    CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
      EXPORTING
        user_name           = sy-uname
      TABLES
        user_parameters     = tl_parametros
      EXCEPTIONS
        user_name_not_exist = 1
        OTHERS              = 2.

    IF vg_st_logon = c_disconnected.
      ty_toolbar-icon      = icon_disconnect.
      ty_toolbar-function  = c_login.
      ty_toolbar-text      = 'Conectar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ELSE.
      ty_toolbar-icon      = icon_connect.
      ty_toolbar-function  = c_logout.

      IF vg_time_lim(2) = '23'.
        vg_time_lim = 1.
      ENDIF.

      CONCATENATE vg_time_lim(2) ':' vg_time_lim+2(2) ':' vg_time_lim+4(2) INTO DATA(_time_desc).
      CONCATENATE 'Desconectar em ' _time_desc 'seg.'
             INTO vl_txt_button SEPARATED BY space.

      ty_toolbar-text      = vl_txt_button.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    IF sy-tcode EQ 'ZMEMO00'.
      IF ( lines( tg_t001[] ) EQ 1 ). "Se selecionou somente uma empresa, habilita envio Registro DU-e para Portal
        ty_toolbar-icon      = icon_transfer.
        ty_toolbar-function  = c_transmitir.
        ty_toolbar-text      = 'Transmitir DU-e'.
        ty_toolbar-butn_type = 0.
        APPEND ty_toolbar TO e_object->mt_toolbar.
        CLEAR ty_toolbar.
      ENDIF.
    ENDIF.

    "READ TABLE TL_PARAMETROS INTO DATA(WL_PARAMETROS) WITH KEY PARID = 'ZDUE_LCTO_AVULSO'.
    "IF SY-SUBRC EQ 0.
    ty_toolbar-icon      = icon_create.
    ty_toolbar-function  = c_novo.
    ty_toolbar-text      = 'Lançar DU-e'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_change.
    ty_toolbar-function  = c_change.
    ty_toolbar-text      = 'Modificar'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
    "ENDIF.

    ty_toolbar-icon      = icon_display.
    ty_toolbar-function  = c_view.
    ty_toolbar-text      = 'Visualizar'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = c_excluir.
    ty_toolbar-text      = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_workflow_external_event.
    ty_toolbar-function  = c_cons_status.
    ty_toolbar-text      = 'Consultar Status'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


*      TY_TOOLBAR-ICON      = ICON_GENERATE.
*      TY_TOOLBAR-FUNCTION  = 'RECEPCIONAR_CARGA'.
*      TY_TOOLBAR-TEXT      = 'Recepcionar Carga'.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.
*
*      TY_TOOLBAR-ICON      = ICON_SYSTEM_UNDO.
*      TY_TOOLBAR-FUNCTION  = 'DISP_NFE_AJUSTE'.
*      TY_TOOLBAR-TEXT      = 'Lib. NF-e p/ Ajuste'.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.
*
*      TY_TOOLBAR-ICON      = ICON_SET_STATE.
*      TY_TOOLBAR-FUNCTION  = 'DEF_CNPJ_CPF_TRANSP'.
*      TY_TOOLBAR-TEXT      = 'Definir CNPJ/CPF Transportador'.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.


  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

* Inicio - falheiros - 25.11.2022
    vg_ucomm_drawn = e_ucomm.

    EXPORT: vg_ucomm_drawn FROM vg_ucomm_drawn
   TO MEMORY ID 'M_DRAWN'.

* Fim - falheiros - 25.11.2022

    CASE e_ucomm.
      WHEN c_novo.
        PERFORM f_lancar_due.
      WHEN c_transmitir.
        PERFORM f_transmitir_due.
      WHEN c_view.
        PERFORM f_view_due.
      WHEN c_change.
        PERFORM f_change_due.
      WHEN c_login.
        PERFORM f_autenticar.
      WHEN c_logout.
        PERFORM f_logout.
      WHEN c_cons_status.
        PERFORM f_consultar_status.
      WHEN c_excluir.
        PERFORM f_excluir_due.
    ENDCASE.

    CASE e_ucomm.
      WHEN c_transmitir   OR
           c_novo         OR
           c_change       OR
           c_excluir      OR
           c_cons_status.

        PERFORM: f_selecionar_dados,
                 f_processa_dados.

        LEAVE TO SCREEN 0100.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0100 IMPLEMENTATION.

  METHOD catch_hotspot.

    CASE e_column_id.
      WHEN 'X'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: on_finished.

    PERFORM f_atualiza_time.

    IF ( vg_tst_lim > vg_tst_atual ).
      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = 'CLOCK'.

      go_clock->interval = vg_time_interval.
      CALL METHOD go_clock->run.
    ELSE.
      PERFORM f_logout.
    ENDIF.

  ENDMETHOD.                           " ON_FINISHED
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

FORM f_atrib_ds_dominio USING p_domname    TYPE dd07t-domname
                              p_domvalue_l
                     CHANGING c_ddtext     TYPE dd07t-ddtext.

  DATA: values   TYPE vrm_values WITH HEADER LINE,
        tg_dd07t TYPE TABLE OF dd07t WITH HEADER LINE.

  DATA: v_value TYPE dd07t-domvalue_l.

  CLEAR: values[], values, tg_dd07t[], c_ddtext.

  CHECK ( p_domname     IS NOT INITIAL ) AND
        ( p_domvalue_l  IS NOT INITIAL ).

  v_value = CONV #( p_domvalue_l ).

  SELECT SINGLE ddtext
    FROM dd07t INTO c_ddtext
   WHERE domname    = p_domname
     AND ddlanguage = sy-langu
     AND domvalue_l = v_value.


ENDFORM.
