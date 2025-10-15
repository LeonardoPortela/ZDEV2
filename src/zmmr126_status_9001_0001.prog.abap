*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9001.
*----------------------------------------------------------------------*
TABLES: zsdt0001us.

DATA: lc_name TYPE name1.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.

  IF zsdt0001us-tp_carga IS INITIAL.
    zsdt0001us-tp_carga = zif_carga=>st_tp_carga_entrada_fob.
  ENDIF.

  IF lc_name IS INITIAL AND pfilia IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
      FROM j_1bbranch
     WHERE branch EQ @pfilia.
    pempre  = wa_j_1bbranch-bukrs.
    lc_name = wa_j_1bbranch-name.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE ok_code.
    WHEN 'SELECIONAR'.
      CHECK lc_name IS NOT INITIAL.
      ck_selecionou = abap_true.

      SELECT SINGLE * INTO @DATA(wa_zsdt0001us)
        FROM zsdt0001us
       WHERE us_name EQ @sy-uname.

      wa_zsdt0001us-us_name   = sy-uname.
      wa_zsdt0001us-nr_safra  = psafra.
      wa_zsdt0001us-id_bukrs  = pempre.
      wa_zsdt0001us-id_branch = pfilia.
      wa_zsdt0001us-tp_carga  = zsdt0001us-tp_carga.
      ptipca = zsdt0001us-tp_carga.
      MODIFY zsdt0001us FROM wa_zsdt0001us.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_PSAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validar_psafra INPUT.

  DATA: pno_ini TYPE i,
        pno_fim TYPE i.

  pno_ini = sy-datum(4).
  pno_fim = sy-datum(4).
  ADD -1 TO pno_ini.
  ADD  1 TO pno_fim.

*-CS2022000332-#78064-07.06.2022-JT-inicio
*-validacao sera feita em outro ponto
*  IF PSAFRA LT PNO_INI OR
*     PSAFRA GT PNO_FIM.
*    MESSAGE E002 WITH PSAFRA.
*  ENDIF.
*-CS2022000332-#78064-07.06.2022-JT-fim

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_PFILIA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validar_pfilia INPUT.

  CLEAR: lc_name.

  SELECT SINGLE * INTO wa_j_1bbranch
    FROM j_1bbranch
   WHERE branch EQ pfilia.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e003 WITH pfilia.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
    ID 'WERKS' FIELD  pfilia
    ID 'ACTVT' FIELD '03'.    "Alteração

  CASE sy-subrc.
    WHEN 4.
      MESSAGE e004.
    WHEN 12.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    WHEN OTHERS.
  ENDCASE.

  pempre  = wa_j_1bbranch-bukrs.

  CLEAR: wa_j_1bbranch.

ENDMODULE.
