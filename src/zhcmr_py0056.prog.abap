*&---------------------------------------------------------------------*
*& Report ZHCMR_PY0056
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcmr_py0056.

TYPES: BEGIN OF ty_saida,
         pernr TYPE pcalac-pernr,
         runid TYPE pcalac-runid,
         cname TYPE pa0002-cname,
       END OF ty_saida.

DATA: it_saida TYPE TABLE OF ty_saida,
      wa_saida TYPE ty_saida.

CLASS lcl_fx DEFINITION DEFERRED.
DATA: lo_fx TYPE REF TO lcl_fx.
CLASS lcl_fx DEFINITION.
  PUBLIC SECTION .
    CLASS-METHODS: fb_pesquisar,
      get_dados,
      action_process,
      del_data.
ENDCLASS.
CLASS lcl_fx IMPLEMENTATION.
  METHOD get_dados.
    SELECT SINGLE a~pernr,a~runid ,b~cname FROM pcalac AS a
INNER JOIN pa0002 AS b ON a~pernr = b~pernr
WHERE a~pernr = @wa_saida-pernr
AND a~runid = @wa_saida-runid
INTO @DATA(ls_saida).

    IF sy-subrc = 0.
      IF ls_saida-cname IS NOT INITIAL.
        MOVE-CORRESPONDING ls_saida TO wa_saida.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD del_data.

    IF wa_saida-cname IS NOT INITIAL AND wa_saida-pernr IS NOT INITIAL AND wa_saida-runid IS NOT INITIAL.
      SELECT SINGLE * FROM pcalac WHERE pernr = @wa_saida-pernr AND runid = @wa_saida-runid INTO @DATA(ls_pcalac).
      IF sy-subrc = 0.

        DATA: it_pernrs TYPE  pernr_tab.

        APPEND wa_saida-pernr TO it_pernrs.

        CALL FUNCTION 'HR_PAYROLL_RESULTS_UNREGISTER'
          EXPORTING
            type   = ls_pcalac-type
            runid  = ls_pcalac-runid
          TABLES
            pernrs = it_pernrs.

        IF sy-subrc = 0.

          DATA: it_salv_log_del TYPE STANDARD TABLE OF zpcalac_del,
                wa_salv_log_del TYPE zpcalac_del.

          MOVE-CORRESPONDING wa_saida TO wa_salv_log_del.

          wa_salv_log_del-usuario = sy-uname.
          wa_salv_log_del-data = sy-datum.
          wa_salv_log_del-hora = sy-uzeit.

          MODIFY zpcalac_del FROM wa_salv_log_del.
          COMMIT WORK.
          CLEAR: wa_saida,wa_salv_log_del.
          MESSAGE 'Registro deletado!' TYPE 'S'.

        ENDIF.
      ENDIF.
    ELSE.
      IF wa_saida-pernr IS INITIAL.
        MESSAGE 'Nº pessoal é Obrigatório!' TYPE 'I'.

      ELSEIF wa_saida-runid IS INITIAL.
        MESSAGE 'Nº execução é Obrigatório!' TYPE 'I'.
      ELSE.
        IF wa_saida-cname IS INITIAL.
          MESSAGE 'Nome completo é Obrigatório!' TYPE 'I'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD fb_pesquisar.
    DATA(lo_dados) = NEW lcl_fx( ).
    lo_dados->get_dados( ).
  ENDMETHOD.

  METHOD action_process.
    CASE sy-ucomm.
      WHEN 'BACK'.
        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN 'CANCEL'.
        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN 'EXIT'.
        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN 'ONLI'.
        DATA(lo_delete) = NEW lcl_fx( ).
        lo_delete->del_data( ).
      WHEN 'FB_PESQUISAR'.
        DATA(lo_search) = NEW lcl_fx( ).
        lo_search->fb_pesquisar( ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  CALL SCREEN 100.

END-OF-SELECTION.

MODULE user_command_0100 INPUT.
  DATA(lo_start) = NEW lcl_fx( ).
  lo_start->action_process( ).
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.
