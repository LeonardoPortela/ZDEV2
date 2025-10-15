*----------------------------------------------------------------------*
***INCLUDE MZENTREMESSA1000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  CLEAR: it_fcode.

  IF vg_pesquisou IS INITIAL.
    SET TITLEBAR 'TLCONS'.
    wa_fcode = 'NOVACON'.
    APPEND wa_fcode TO it_fcode.
    wa_fcode = 'PROCESSAR'.
    APPEND wa_fcode TO it_fcode.
  ELSE.
    wa_fcode = 'EXECCONS'.
    APPEND wa_fcode TO it_fcode.
    SET TITLEBAR 'TLVINC'.
    IF NOT vg_alterou IS INITIAL.
      wa_fcode = 'PROCESSAR'.
      APPEND wa_fcode TO it_fcode.
    ELSE.
      wa_fcode = 'SAVE'.
      APPEND wa_fcode TO it_fcode.
    ENDIF.
  ENDIF.

  SET PF-STATUS 'PFCONS' EXCLUDING it_fcode.

  IF vg_dynnr_1001 IS INITIAL.
    vg_dynnr_1001 = '1001'.
  ENDIF.

ENDMODULE.                 " STATUS_1000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      IF vg_alterou IS INITIAL.
        LEAVE PROGRAM.
      ELSE.
        PERFORM sair USING vg_alterou.
      ENDIF.
    WHEN 'EXECCONS'.
      PERFORM pesquisar_notas_remessa.
      CLEAR: ok_code.
      vg_pesquisou = 'X'.
    WHEN 'SAVE'.
      PERFORM salvar_notas_remessa.
      CLEAR: ok_code.
    WHEN 'NOVACON'.
      IF vg_alterou IS INITIAL.
        CLEAR: vg_pesquisou, it_notas[], it_itens[], it_disp[], it_vinc[], it_desvin[].
      ELSE.
        PERFORM sair USING vg_alterou.
      ENDIF.
      CLEAR: ok_code.
    WHEN 'PROCESSAR'.
      PERFORM gera_remessas_entrada.
      PERFORM pesquisar_notas_remessa.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1000  INPUT

*&---------------------------------------------------------------------*
*&      Form  SALVAR_NOTAS_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM salvar_notas_remessa .

  IF vg_alterou IS NOT INITIAL.

    IF vg_alterou EQ 'X'.

      LOOP AT it_vinc INTO wa_notas.
        IF wa_notas-tp_status IS INITIAL.
          wa_entrada-vbeln        = wa_notas-vbeln.
          wa_entrada-docnum       = wa_notas-docnum.
          wa_entrada-nfenum       = wa_notas-nfenum.
          wa_entrada-serie        = wa_notas-series.
          wa_entrada-bukrs        = wa_notas-bukrs.
          wa_entrada-branch       = wa_notas-branch.
          wa_entrada-vbeln_s      = wa_notas-vbeln_s.
          wa_entrada-dt_chegada   = wa_notas-dt_chegada.
          wa_entrada-qt_chegada   = wa_notas-qt_chegada.
          wa_entrada-werks_v      = wa_notas-werks_v.
          wa_entrada-lgort        = wa_notas-lgort.
          wa_entrada-vkorg        = wa_notas-vkorg.
          wa_entrada-vtweg        = wa_notas-vtweg.
          wa_entrada-spart        = wa_notas-spart.
          wa_entrada-vbeln_ord_e  = ''.
          wa_entrada-vbeln_e      = ''.
          wa_entrada-tp_status    = ''.
          MOVE-CORRESPONDING wa_entrada TO zsdt_entrada_rem.
          MODIFY zsdt_entrada_rem.
        ENDIF.
      ENDLOOP.

      LOOP AT it_desvin INTO wa_notas.
        DELETE FROM zsdt_entrada_rem WHERE docnum EQ wa_notas-docnum.
      ENDLOOP.

      COMMIT WORK.

      MESSAGE 'Alterações foram salvas!' TYPE 'S'.

      CLEAR: vg_alterou, it_desvin[].

    ELSE.
      MESSAGE 'Favor informar data/peso de chegada' TYPE 'S' DISPLAY LIKE 'E'.
      vg_alterou = 'X'.
    ENDIF.

  ENDIF.

ENDFORM.                    " SALVAR_NOTAS_REMESSA

*&---------------------------------------------------------------------*
*&      Form  SAIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_ALTEROU  text
*----------------------------------------------------------------------*
FORM sair  USING    p_vg_alterou.

  IF p_vg_alterou IS NOT INITIAL.

    DATA: answer TYPE c LENGTH 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = 'Atenção!'
        textline1 = 'Dados foram alterados.'
        textline2 = 'Deseja salvar?'
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN 'J'.
        PERFORM salvar_notas_remessa.
      WHEN 'N'.
        CLEAR vg_alterou.
        CASE ok_code.
          WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
            LEAVE PROGRAM.
          WHEN 'NOVACON'.
            CLEAR: vg_pesquisou, it_notas[], it_itens[], it_disp[], it_vinc[], it_desvin[].
        ENDCASE.
      WHEN 'A'.
        EXIT.
    ENDCASE.

  ENDIF.

ENDFORM.                    " SAIR

*&---------------------------------------------------------------------*
*&      Form  GERA_REMESSAS_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gera_remessas_entrada .

  DATA vg_status LIKE zsdt_entrada_rem-tp_status.

  IF vg_alterou IS INITIAL.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_saidas
      FROM zsdt_entrada_rem
      FOR ALL ENTRIES IN it_vinc
     WHERE docnum EQ it_vinc-docnum
       AND tp_status EQ vg_status.

    IF NOT it_saidas[] IS INITIAL.

      CALL FUNCTION 'Z_PROC_ENT_REMESSA'
        EXPORTING
          p_manual       = 'X'
        TABLES
          it_notas_saida = it_saidas
        EXCEPTIONS
          sem_saidas     = 1
          com_saidas     = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.
      MESSAGE 'Sem entradas de remessa a processar' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ELSE.
    MESSAGE 'Favor salvar alterações primeiro' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " GERA_REMESSAS_ENTRADA
