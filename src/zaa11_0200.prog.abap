*&---------------------------------------------------------------------*
*&  Include           ZAA11_0200
*&---------------------------------------------------------------------*
DATA: p_01,         "Radio Button Status 01
      p_02,         "Radio Button Status 02
      p_03,         "Radio Button Status 03
      p_04,         "Radio Button Status 04
      p_05.         "Radio Button Status 05

DATA: wa_zaa005_mod TYPE zaa005.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TL0200'.

  IF vg_1vtela IS NOT INITIAL.

    READ TABLE it_dados_imob INTO wa_dados_imob INDEX vg_row.
    CLEAR: p_01, p_02, p_03, p_04, p_05.

    IF wa_dados_imob-zimob_v EQ 0.
      MOVE abap_true TO p_01.
    ELSEIF wa_dados_imob-zimob_v EQ 1.
      MOVE abap_true TO p_02.
    ELSEIF wa_dados_imob-zimob_v EQ 2.
      MOVE abap_true TO p_03.
    ELSEIF wa_dados_imob-zimob_v EQ 3.
      MOVE abap_true TO p_04.
    ELSE.
      MOVE abap_true TO p_05.
    ENDIF.

    CLEAR: wa_dados_imob, vg_1vtela.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200_exit INPUT.
  CLEAR ok_code.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: vl_obj_key TYPE sibflporb-instid,
        vl_lines   TYPE i,
        vl_bloq    TYPE char1,                "Bloqueio para salvar novo status
        anexos     TYPE TABLE OF bdn_con.

  DATA: wl_name  TYPE thead-tdname.

  DATA: seltab    TYPE TABLE OF rsparams,
        seltab_wa LIKE LINE OF seltab.

  CASE sy-ucomm.
    WHEN 'SALVAR'.

      READ TABLE it_dados_imob INTO wa_dados_imob INDEX vg_row.

      wa_zaa005_mod-anln1 = wa_dados_imob-anln1.
      wa_zaa005_mod-anln2 = wa_dados_imob-anln2.
      wa_zaa005_mod-bukrs = wa_dados_imob-bukrs.
      wa_zaa005_mod-gsber = wa_dados_imob-gsber.
      wa_zaa005_mod-kostl = wa_dados_imob-kostl.
      wa_zaa005_mod-gjahr = p_gjahr-low.
      wa_zaa005_mod-zimob_a = wa_dados_imob-zimob_a.
      wa_zaa005_mod-zimob_p = wa_dados_imob-zimob_p.

      IF p_02 EQ abap_true.
        wa_zaa005_mod-zimob_v = '1'.
        wa_dados_imob-zimob_v = '1'.
      ELSEIF p_04 EQ abap_true.
        wa_zaa005_mod-zimob_v = '3'.
        wa_dados_imob-zimob_v = '3'.


        "Abrir a transação ZAA18

        CLEAR: seltab[], seltab_wa.

        seltab_wa-selname = 'P_BUKRS'.
        seltab_wa-sign    = 'I'.
        seltab_wa-option  = 'EQ'.
        seltab_wa-low     = wa_zaa005_mod-bukrs.
        APPEND seltab_wa TO seltab.

        seltab_wa-selname = 'P_WERKS'.
        seltab_wa-sign    = 'I'.
        seltab_wa-option  = 'EQ'.
        seltab_wa-low     = wa_zaa005_mod-gsber.
        APPEND seltab_wa TO seltab.

        seltab_wa-selname = 'P_ANLN1'.
        seltab_wa-sign    = 'I'.
        seltab_wa-option  = 'EQ'.
        seltab_wa-low     = wa_zaa005_mod-anln1.
        APPEND seltab_wa TO seltab.

        MODIFY zaa005 FROM wa_zaa005_mod.
        MODIFY it_dados_imob FROM wa_dados_imob INDEX vg_row.

*        SUBMIT zaa13 WITH SELECTION-TABLE seltab AND RETURN. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

        CLEAR: wa_zaa005_mod, vg_row, wa_dados_imob, vl_bloq.

*        "Busca anexos
*        CONCATENATE P_GJAHR-LOW WA_DADOS_IMOB-ANLN1 WA_DADOS_IMOB-ANLN2
*                    WA_DADOS_IMOB-GSBER WA_DADOS_IMOB-KOSTL INTO VL_OBJ_KEY.
*
*        CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*          EXPORTING
*            CLASSNAME          = 'ZAA11'
*            OBJKEY             = VL_OBJ_KEY
*            CLIENT             = SY-MANDT
*          TABLES
*            GOS_CONNECTIONS    = ANEXOS
*          EXCEPTIONS
*            NO_OBJECTS_FOUND   = 1
*            INTERNAL_ERROR     = 2
*            INTERNAL_GOS_ERROR = 3
*            OTHERS             = 4.
*
*        DESCRIBE TABLE ANEXOS LINES VL_LINES.
*
*        "Busca Texto
*        REFRESH: IT_TEXTO.
*        CLEAR: WL_NAME.
*        CONCATENATE P_GJAHR-LOW WA_DADOS_IMOB-ANLN1 WA_DADOS_IMOB-ANLN2
*                    WA_DADOS_IMOB-GSBER WA_DADOS_IMOB-KOSTL INTO WL_NAME.
*
*        CALL FUNCTION 'READ_TEXT'
*          EXPORTING
*            ID                      = 'ZAA1'
*            LANGUAGE                = SY-LANGU
*            NAME                    = WL_NAME
*            OBJECT                  = 'ZAA02'
*          TABLES
*            LINES                   = IT_TEXTO
*          EXCEPTIONS
*            ID                      = 1
*            LANGUAGE                = 2
*            NAME                    = 3
*            NOT_FOUND               = 4
*            OBJECT                  = 5
*            REFERENCE_CHECK         = 6
*            WRONG_ACCESS_TO_ARCHIVE = 7
*            OTHERS                  = 8.
*
**        "Bloqueia salvar novo status
**        IF SY-SUBRC IS NOT INITIAL." OR VL_LINES EQ 0.
**          VL_BLOQ = ABAP_TRUE.
***          MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
**          MESSAGE 'Inserir Texto
**        ENDIF.
*
*        CLEAR: IT_TEXTO.

      ELSEIF p_01 EQ abap_true.
        wa_zaa005_mod-zimob_v = '0'.
        wa_dados_imob-zimob_v = '0'.
      ELSEIF p_03 EQ abap_true.
        wa_zaa005_mod-zimob_v = '2'.
        wa_dados_imob-zimob_v = '2'.

*-CS2019000323 - 02.09.2021 - JT - inicio
        "Busca Texto
        REFRESH: it_texto.
*        CLEAR: WL_NAME.
*        CONCATENATE P_GJAHR-LOW WA_DADOS_IMOB-ANLN1 WA_DADOS_IMOB-ANLN2
*                    WA_DADOS_IMOB-GSBER WA_DADOS_IMOB-KOSTL INTO WL_NAME.
*
*        CALL FUNCTION 'READ_TEXT'
*          EXPORTING
*            ID                      = 'ZAA1'
*            LANGUAGE                = SY-LANGU
*            NAME                    = WL_NAME
*            OBJECT                  = 'ZAA02'
*          TABLES
*            LINES                   = IT_TEXTO
*          EXCEPTIONS
*            ID                      = 1
*            LANGUAGE                = 2
*            NAME                    = 3
*            NOT_FOUND               = 4
*            OBJECT                  = 5
*            REFERENCE_CHECK         = 6
*            WRONG_ACCESS_TO_ARCHIVE = 7
*            OTHERS                  = 8.

        "Bloqueia salvar novo status
*       IF SY-SUBRC IS NOT INITIAL.
        IF wa_dados_imob-observ IS INITIAL.
          vl_bloq = abap_true.
          MESSAGE text-009 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

        CLEAR: it_texto.
      ELSE.
        wa_zaa005_mod-zimob_v = '4'.
        wa_dados_imob-zimob_v = '4'.
      ENDIF.
      "Bloqueia salvar caso Imob não esteja no nível 0 ou 1 de aprovação
      IF NOT ( ( wa_dados_imob-zimob_a EQ 0 OR wa_dados_imob-zimob_a EQ 1 ) AND ( wa_dados_imob-usuar_aa EQ sy-uname ) ). "( WA_DADOS_IMOB-ZIMOB_A NE 0 OR WA_DADOS_IMOB-ZIMOB_A NE 1 ) OR WA_DADOS_IMOB-USUAR_AA NE SY-UNAME.
        vl_bloq = abap_true.
      ENDIF.

      IF vl_bloq IS INITIAL.
        MODIFY zaa005 FROM wa_zaa005_mod.
        MODIFY it_dados_imob FROM wa_dados_imob INDEX vg_row.
        "MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'S'.
        CLEAR: wa_zaa005_mod, vg_row, wa_dados_imob, vl_bloq.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: wa_zaa005_mod, vg_row, wa_dados_imob, vl_bloq.
        LEAVE TO SCREEN 0.
      ENDIF.

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'PF0400'.
  SET TITLEBAR 'TL0400'.
ENDMODULE.
