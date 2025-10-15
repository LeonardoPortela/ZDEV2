*&---------------------------------------------------------------------*
*&  Include           MZPMR0066I01
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE okcode.
    WHEN
        'BACK' OR
        'EEND' OR
        'CANC' OR
        'EXIT'.
      CLEAR okcode.
      LEAVE TO SCREEN 0.
    WHEN 'TREN'.
      REFRESH gt_outtab.
      CLEAR:  gv_werks,
              gv_budat,
              gv_pernr,
              gv_name1.
      CALL SCREEN '0200'.
    WHEN 'MASS'.
      CALL TRANSACTION 'ZPM0076'.
    WHEN 'DAVUL'.
      CALL SCREEN '0300'.
    WHEN 'BAIXAR'.
      CALL TRANSACTION 'ZPM0077'.
    WHEN 'RELAT'.
      CALL TRANSACTION 'ZPM0078'.
    WHEN 'PDIAS'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action                       = 'U'
          view_name                    = 'ZPMT0042'
        EXCEPTIONS
          client_reference             = 1
          foreign_lock                 = 2
          invalid_action               = 3
          no_clientindependent_auth    = 4
          no_database_function         = 5
          no_editor_function           = 6
          no_show_auth                 = 7
          no_tvdir_entry               = 8
          no_upd_auth                  = 9
          only_show_allowed            = 10
          system_failure               = 11
          unknown_field_in_dba_sellist = 12
          view_not_found               = 13
          maintenance_prohibited       = 14
          OTHERS                       = 15.

    WHEN 'PSUB'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action                       = 'U'
          view_name                    = 'ZPMT0043'
        EXCEPTIONS
          client_reference             = 1
          foreign_lock                 = 2
          invalid_action               = 3
          no_clientindependent_auth    = 4
          no_database_function         = 5
          no_editor_function           = 6
          no_show_auth                 = 7
          no_tvdir_entry               = 8
          no_upd_auth                  = 9
          only_show_allowed            = 10
          system_failure               = 11
          unknown_field_in_dba_sellist = 12
          view_not_found               = 13
          maintenance_prohibited       = 14
          OTHERS                       = 15.

*** Inicio - Rubenilson Pereira - 13.10.22 - US 77010
    WHEN 'CAIXA'.

      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action                       = 'U'
          view_name                    = 'ZPMT0065'
        EXCEPTIONS
          client_reference             = 1
          foreign_lock                 = 2
          invalid_action               = 3
          no_clientindependent_auth    = 4
          no_database_function         = 5
          no_editor_function           = 6
          no_show_auth                 = 7
          no_tvdir_entry               = 8
          no_upd_auth                  = 9
          only_show_allowed            = 10
          system_failure               = 11
          unknown_field_in_dba_sellist = 12
          view_not_found               = 13
          maintenance_prohibited       = 14
          OTHERS                       = 15.
*** Fim - Rubenilson Pereira - 13.10.22 - US 77010

  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           MZPMR0066I01
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter "trigger event after ENTER is pressed
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CASE okcode.
    WHEN
        'BACK' OR
        'EEND' OR
        'CANC' OR
        'EXIT'.
      CLEAR okcode.
      LEAVE TO SCREEN 0.
    WHEN
        'NEW'.
      PERFORM z_new.
      CLEAR okcode.
  ENDCASE.

  PERFORM z_valida_usuario.

ENDMODULE.

MODULE f4_date INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = gv_budat
    IMPORTING
      select_date                  = gv_budat
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
ENDMODULE.

MODULE f4_pnr INPUT.
  "146631 CS2024000618 MEL. EMPRESTIMO FERRAMENTA PSA
*  SELECT pernr,
*         ename INTO TABLE @DATA(lt_pnr)
*    FROM pa0001
*     WHERE endda >= @sy-datum.

  "IF gv_pernr IS NOT INITIAL.
    SELECT DISTINCT
    a~pernr,a~cname,a~cpf_nr,
    CASE WHEN b~matricula IS NOT INITIAL THEN 'X' ELSE ' ' END AS biometria,
    CASE WHEN c~matricula IS NOT INITIAL THEN 'X' ELSE ' ' END AS senha

     FROM zhcmt0007 AS a
    LEFT JOIN zmmt0088 AS b ON a~pernr = b~matricula
    LEFT JOIN zmmt0120 AS c ON a~pernr = c~matricula
    WHERE a~fdate = '00000000'
    "AND a~werks = @gv_pernr
    INTO TABLE @DATA(lt_pnr).
    SORT lt_pnr BY cname.
 " ELSE.
   " MESSAGE 'Centro é obrigatório!' TYPE 'I'.
 " ENDIF.



  "DELETE ADJACENT DUPLICATES FROM lt_pnr COMPARING cname.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PERNR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GV_PERNR'
      value_org       = 'S'
    TABLES
      value_tab       = lt_pnr
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.

MODULE f4_date2 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = eg_zpmt0044-budat
    IMPORTING
      select_date                  = eg_zpmt0044-budat
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
ENDMODULE.
*&---------------------------------------------------------------------*
*&  Include           MZPMR0066I01
*&---------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  DATA: it_msg TYPE TABLE OF bdcmsgcoll.

  CASE okcode.
    WHEN
        'BACK' OR
        'EEND' OR
        'CANC' OR
        'EXIT'.
      CLEAR okcode.
      LEAVE TO SCREEN 0.
    WHEN
        'EDESC'.
      PERFORM z_descarte.
      CLEAR okcode.
    WHEN
        'NEW'.
      PERFORM z_new.
      CLEAR okcode.
  ENDCASE.
*
  PERFORM z_valida_material.

ENDMODULE.
