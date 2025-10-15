*&---------------------------------------------------------------------*
*& Report  ZSDR0137
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0137.

TABLES: vbak.

DATA: l_title  TYPE cua_tit_tx.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_contra RADIOBUTTON GROUP g1 USER-COMMAND c1  DEFAULT 'X'.
SELECTION-SCREEN COMMENT (60) text-011 FOR FIELD p_contra.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_cadban RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (60) text-010 FOR FIELD p_cadban.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_vkorg   LIKE vbak-vkorg  MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK b3.

****************************************************************
* output
****************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      screen-required = '2'.

      IF p_contra = abap_on.
        screen-input = '1'.
        screen-active = '1'.
      ELSE.
        screen-input = '0'.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

****************************************************************
* output
****************************************************************
AT SELECTION-SCREEN.

  IF p_contra = abap_true.
    IF p_vkorg IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar Organização de Vendas!' DISPLAY LIKE 'E'.
      STOP.
    ELSE.
      SELECT SINGLE *
        FROM tvko
        INTO @DATA(w_tvko)
       WHERE vkorg = @p_vkorg.

      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH 'Organização de Vendas incorreta!' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

****************************************************************
* start
****************************************************************
START-OF-SELECTION.

  l_title = 'Parametr.Exceção Gerador Docs.SIGAM - Org.Vendas:' && p_vkorg.

  CASE abap_true.

    WHEN p_cadban.
      CALL TRANSACTION 'ZSDT0192'.

    WHEN p_contra.
      AUTHORITY-CHECK OBJECT 'ZSDT0193VK'
                          ID 'VKORG' FIELD p_vkorg.
      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH 'Seu usuário não tem acesso Liberado para '
                              'a Organização de Vendas' p_vkorg
                         DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      SUBMIT zsdr0148 WITH p_vkorg  = p_vkorg
                      WITH p_db_tab = 'ZSDT0309'
                      WITH p_stcnam = 'ZSDT0309_OUT'
                      WITH p_scmant = '0002'
                      WITH p_title  = l_title
                       AND RETURN.
  ENDCASE.

****************************************************************
****************************************************************
