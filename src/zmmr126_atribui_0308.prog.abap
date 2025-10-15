*----------------------------------------------------------------------*
***INCLUDE ZMMR126_ATRIBUI_0308.
*----------------------------------------------------------------------*

DATA: LC_0308_NR_PERC_AVA TYPE ZDE_NR_PERC_AVA_ARQ,
      CK_0308_NR_PERC_AVA TYPE CHAR01.

DATA: VL_NR_PERC_AVA_ARQ TYPE C LENGTH 6,
      VL_NR_PERC_AVA_QUE TYPE C LENGTH 6,
      VL_NR_PERC_AVA_MOF TYPE C LENGTH 6,
      VL_NR_PERC_AVA_PIC TYPE C LENGTH 6,
      VL_NR_PERC_AVA_FER TYPE C LENGTH 6,
      VL_NR_PERC_AVA_GER TYPE C LENGTH 6,
      VL_NR_PERC_AVA_ARD TYPE C LENGTH 6,
      VL_NR_PERC_AVA_GES TYPE C LENGTH 6,
      LC_FIELD_SET_0308  TYPE C LENGTH 50.

*&---------------------------------------------------------------------*
*&      Form  ATRIBUI_PERC_SUB_AVARIADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ATRIBUI_PERC_SUB_AVARIADO  USING P_NR_PERC_AVA TYPE ZDE_NR_PERC_AVA_ARQ CHANGING OK TYPE CHAR01.

  OBJETO->CK_DIGITADO_AVA_ARQ = ABAP_FALSE.
  OBJETO->CK_DIGITADO_AVA_QUE = ABAP_FALSE.
  OBJETO->CK_DIGITADO_AVA_MOF = ABAP_FALSE.
  OBJETO->CK_DIGITADO_AVA_PIC = ABAP_FALSE.
  OBJETO->CK_DIGITADO_AVA_FER = ABAP_FALSE.
  OBJETO->CK_DIGITADO_AVA_GER = ABAP_FALSE.
  OBJETO->CK_DIGITADO_AVA_ARD = ABAP_FALSE.
  OBJETO->CK_DIGITADO_AVA_GES = ABAP_FALSE.

  LC_0308_NR_PERC_AVA = P_NR_PERC_AVA.
  CK_0308_NR_PERC_AVA = ABAP_FALSE.
  CLEAR: LC_FIELD_SET_0308.

  CALL SCREEN 0308 STARTING AT 30 15.
  OK = CK_0308_NR_PERC_AVA.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0308  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0308 INPUT.

  CASE OK_CODE.
    WHEN 'CONFIRMAR'.
      PERFORM VERIFICA_SUBTOTAL_AVARIADO CHANGING SY-SUBRC.
      IF SY-SUBRC IS INITIAL.
        CK_0308_NR_PERC_AVA = ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0308  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0308 OUTPUT.

  SET PF-STATUS 'PF0308'.
  SET TITLEBAR 'TL0308'.

  LOOP AT SCREEN.

    TRY .
        CASE SCREEN-NAME.
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARQ'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ IMPORTING E_CK_CARAC = DATA(E_CK_C) E_CK_OBRIGATORIO = DATA(E_CK_O) ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_QUE'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_QUE IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_MOF'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_MOF IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_PIC'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_PIC IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_FER'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_FER IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GER'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_GER IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARD'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_ARD IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GES'.
            OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_GES IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
        ENDCASE.
      CATCH ZCX_CARGA.    "
    ENDTRY.

    IF E_CK_C EQ ABAP_TRUE.
      SCREEN-INPUT = '1'.
    ELSE.
      SCREEN-INPUT = '0'.
    ENDIF.

    IF E_CK_O EQ ABAP_TRUE.
      SCREEN-REQUIRED = '1'.
    ELSE.
      SCREEN-REQUIRED = '0'.
    ENDIF.
    MODIFY SCREEN.

  ENDLOOP.

  IF LC_FIELD_SET_0308 IS NOT INITIAL.
    SET CURSOR FIELD LC_FIELD_SET_0308 OFFSET POS.
    CLEAR: LC_FIELD_SET_0308.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SUBTOTAL_AVARIADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM VERIFICA_SUBTOTAL_AVARIADO CHANGING P_SUBRC TYPE SY-SUBRC.

  "Verifica Obrigatoriedade de Digitar

  DATA(LC_SUBRC) = P_SUBRC.

  LC_SUBRC = 1.

  TRY .

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ IMPORTING E_CK_CARAC = DATA(E_CK_C) E_CK_OBRIGATORIO = DATA(E_CK_O) ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_ARQ EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARQ'.
        MESSAGE S238 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_QUE IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_QUE EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_QUE'.
        MESSAGE S239 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_MOF IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_MOF EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_MOF'.
        MESSAGE S240 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_PIC IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_PIC EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_PIC'.
        MESSAGE S241 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_FER IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_FER EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_FER'.
        MESSAGE S242 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_GER IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_GER EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GER'.
        MESSAGE S243 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_ARD IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_ARD EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARD'.
        MESSAGE S244 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

      OBJETO->GET_CK_SUB_CARAC_AVA_MATERIAL( EXPORTING I_SUB_CARAC_AVA  = ZIF_CARGA=>ST_TP_CARACT_SUB_GES IMPORTING E_CK_CARAC = E_CK_C E_CK_OBRIGATORIO = E_CK_O ).
      IF E_CK_O EQ ABAP_TRUE AND OBJETO->CK_DIGITADO_AVA_GES EQ ABAP_FALSE.
        LC_FIELD_SET_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GES'.
        MESSAGE S245 DISPLAY LIKE 'E'.
        P_SUBRC = LC_SUBRC.
        EXIT.
      ENDIF.

    CATCH ZCX_CARGA INTO EX_CARGA.
      EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
      P_SUBRC = LC_SUBRC.
      EXIT.
  ENDTRY.

  DATA: LC_TOTAL TYPE ZDE_NR_PERC_AVA_ARQ.

  LC_TOTAL = ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARQ + ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_QUE +
             ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_MOF + ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_PIC +
             ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_FER + ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GER +
             ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARD + ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GES.

  IF LC_TOTAL NE LC_0308_NR_PERC_AVA.
    MESSAGE S236 DISPLAY LIKE 'E'.
  ELSE.
    LC_SUBRC = 0.
  ENDIF.

  P_SUBRC = LC_SUBRC.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0308_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0308_EXIT INPUT.

  CK_0308_NR_PERC_AVA = ABAP_FALSE.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_01  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_01 INPUT.
  OBJETO->CK_DIGITADO_AVA_ARQ = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_02  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_02 INPUT.
  OBJETO->CK_DIGITADO_AVA_QUE = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_03  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_03 INPUT.
  OBJETO->CK_DIGITADO_AVA_MOF = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_04  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_04 INPUT.
  OBJETO->CK_DIGITADO_AVA_PIC = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_05  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_05 INPUT.
  OBJETO->CK_DIGITADO_AVA_FER = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_06  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_06 INPUT.
  OBJETO->CK_DIGITADO_AVA_GER = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_07  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_07 INPUT.
  OBJETO->CK_DIGITADO_AVA_ARD = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_08  INPUT
*&---------------------------------------------------------------------*
MODULE SUB_PERC_08 INPUT.
  OBJETO->CK_DIGITADO_AVA_GES = ABAP_TRUE.
ENDMODULE.
