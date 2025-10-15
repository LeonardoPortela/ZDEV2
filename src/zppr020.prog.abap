REPORT zppr020.

TABLES zppt0002.


SELECTION-SCREEN: BEGIN OF SCREEN 0002 AS SUBSCREEN.
SELECT-OPTIONS: cd_sai FOR zppt0002-cd_sai  NO INTERVALS.
SELECTION-SCREEN: END OF SCREEN 0002.


DATA: wa_0002  TYPE zppt0002,
      r_cdsai  TYPE REF TO zppt0002-cd_sai,
      obj_alv  TYPE REF TO cl_gui_alv_grid,
      obj_cont TYPE REF TO cl_gui_custom_container,
      it_saida TYPE TABLE OF zppe0001,
      wa_saida TYPE zppe0001,
      t_mseg   TYPE TABLE OF mseg.

START-OF-SELECTION.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.

      CHECK cd_sai IS NOT INITIAL.

      PERFORM pf_seleciona_dados.
      PERFORM pf_exibe_dados.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  PF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_seleciona_dados .

  SELECT id_cotton
    FROM zppt0002
    INTO TABLE @DATA(it_id)
    WHERE cd_sai IN @cd_sai.

  CHECK it_id IS NOT INITIAL.

  SELECT *
    FROM zppt0002
    INTO TABLE @DATA(it_0002)
    FOR ALL ENTRIES IN @it_id
    WHERE id_cotton EQ @it_id-id_cotton.

  SELECT *
    FROM zppt0006
    INTO TABLE @DATA(it_0006)
    FOR ALL ENTRIES IN @it_id
    WHERE id_cotton EQ @it_id-id_cotton.

  SORT it_0006 BY id_cotton data hora.

  PERFORM fm_remove_estorno TABLES it_0006.

  CHECK it_0002 IS NOT INITIAL.

  SELECT *
    FROM mchb
    INTO TABLE @DATA(it_mchb)
    FOR ALL ENTRIES IN @it_0002
    WHERE charg EQ @it_0002-acharg.


  SELECT *
     FROM mseg
     INTO TABLE @DATA(it_mseg)
    FOR ALL ENTRIES IN @it_0002
  WHERE charg = @it_0002-acharg
     AND werks = @it_0002-werks.

  LOOP AT it_0002 INTO DATA(wa_0002).

    wa_saida-cd_sai         = wa_0002-cd_sai.
    wa_saida-id_cotton      = wa_0002-id_cotton.
    wa_saida-rolo           = wa_0002-charg.
    wa_saida-doc_rolo       = wa_0002-mblnr02.
    wa_saida-rolinho        = wa_0002-acharg.
    wa_saida-doc_rolinho    = wa_0002-mblnr.
    wa_saida-status_regitro = wa_0002-status_registro.
    wa_saida-qtd_trace      = wa_0002-menge.

    READ TABLE it_mchb INTO DATA(wa_mchb) WITH KEY charg = wa_0002-acharg lfgja = wa_0002-cd_safra.
    IF sy-subrc IS INITIAL.
      wa_saida-qtd_sap = wa_mchb-clabs.
    ENDIF.

    SORT it_0006 BY id_cotton data hora.

    LOOP AT it_0006 INTO DATA(wa_0006) WHERE id_cotton = wa_0002-id_cotton
                                         AND acharg EQ wa_0002-acharg
                                         AND status_msg = 'S'.

      IF wa_saida-doc_log_01 IS INITIAL.
        wa_saida-doc_log_01 = wa_0006-cd_mensagem+30(10).
      ELSEIF wa_saida-doc_log_02 IS INITIAL.
        wa_saida-doc_log_02 = wa_0006-cd_mensagem+30(10).
      ELSEIF wa_saida-doc_log_03 IS INITIAL.
        wa_saida-doc_log_03 = wa_0006-cd_mensagem+30(10).
      ELSEIF wa_saida-doc_log_04 IS INITIAL.
        wa_saida-doc_log_04 = wa_0006-cd_mensagem+30(10).
      ENDIF.

    ENDLOOP.

    SELECT SINGLE status_registro
      FROM zppt0006
      INTO @wa_saida-ultimo_status
    WHERE id_cotton EQ @wa_0002-id_cotton
      AND acharg    EQ @wa_0002-acharg
      AND id IN ( SELECT MAX( id )
                   FROM zppt0006
                  WHERE id_cotton EQ @wa_0002-id_cotton
                    AND acharg    EQ @wa_0002-acharg ).

    t_mseg = it_mseg.

    DELETE t_mseg WHERE charg NE wa_0002-acharg.
    DELETE t_mseg WHERE werks NE wa_0002-werks.

    SORT t_mseg BY cpudt_mkpf cputm_mkpf.

    DATA(qtd) = lines( t_mseg ).

    IF t_mseg IS NOT INITIAL.
      wa_saida-ultimo_doc_sap = t_mseg[ qtd ]-ebeln.
      wa_saida-ultimo_tp_movimento = t_mseg[ qtd ]-bwart.
    ENDIF.

    APPEND wa_saida TO it_saida.

    CLEAR wa_saida .

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXIBE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_exibe_dados .

  CREATE OBJECT obj_cont
    EXPORTING
      container_name = 'CC'.

  CREATE OBJECT obj_alv
    EXPORTING
      i_shellstyle    = 0
      i_parent        = obj_cont
      i_appl_events   = abap_false
      i_fcat_complete = abap_false.

  CALL METHOD obj_alv->set_table_for_first_display
    EXPORTING
      i_structure_name              = 'ZPPE0001'
    CHANGING
      it_outtab                     = it_saida
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_REMOVE_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_0006  text
*----------------------------------------------------------------------*
FORM fm_remove_estorno  TABLES p_0006 STRUCTURE zppt0006.

  LOOP AT p_0006 ASSIGNING FIELD-SYMBOL(<f_006>).

    IF <f_006>-status_registro EQ '04'.
      <f_006>-flag_envio = 'W'.
    ENDIF.

    IF <f_006>-status_registro EQ '05'.
      <f_006>-flag_envio = 'Y'.
      MODIFY p_0006 FROM <f_006> TRANSPORTING flag_envio WHERE flag_envio EQ 'W'.
    ENDIF.

  ENDLOOP.

  DELETE p_0006 WHERE flag_envio EQ 'Y'.
  SORT p_0006 BY acharg.
  DELETE ADJACENT DUPLICATES FROM p_0006 COMPARING acharg.

ENDFORM.
