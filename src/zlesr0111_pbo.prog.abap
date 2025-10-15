*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_PBO
*&---------------------------------------------------------------------*

MODULE PBO_0100 OUTPUT.

  IF OBJ_ALV_0100 IS INITIAL.

    PERFORM F_REFRESH_OBJETOS.
    PERFORM F_CRIAR_CATALOG USING '0100'.

    IF OBJ_CONTAINER_0100 IS INITIAL.
      CREATE OBJECT OBJ_CONTAINER_0100
        EXPORTING
          CONTAINER_NAME = 'CC_ALV_0100'.
    ENDIF.

    CREATE OBJECT OBJ_ALV_0100
      EXPORTING
        I_PARENT = OBJ_CONTAINER_0100.

    CREATE OBJECT OBJ_TOOLBAR_0100
      EXPORTING
        IO_ALV_GRID = OBJ_ALV_0100.

    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-INFO_FNAME = 'ROWCOLOR'.
    GS_VARIANT-REPORT  = SY-REPID.
    WA_STABLE-ROW         = 'X'.
    WA_STABLE-COL         = 'X'.

    SET HANDLER: OBJ_TOOLBAR_0100->ON_TOOLBAR          FOR OBJ_ALV_0100,
                 OBJ_TOOLBAR_0100->HANDLE_USER_COMMAND FOR OBJ_ALV_0100,
                 LCL_EVENT_HANDLER_0100=>CATCH_HOTSPOT FOR OBJ_ALV_0100.

    PERFORM F_EXCLUDE_FCODE USING '0100'.

    CASE ABAP_TRUE.
      WHEN P_TP_RCC. "Recepcionar Carga

        CASE ABAP_TRUE.
          WHEN P_VW_NF. "Notas Fiscais

            CALL METHOD OBJ_ALV_0100->SET_TABLE_FOR_FIRST_DISPLAY
              EXPORTING
                IS_LAYOUT            = GS_LAYOUT
                I_SAVE               = 'A'
                IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
                IS_VARIANT           = GS_VARIANT
              CHANGING
                IT_FIELDCATALOG      = IT_FCAT
                IT_OUTTAB            = IT_SAIDA_0100_01.

          WHEN P_VW_DUE. "DU-e's
          WHEN P_RC_NF. "Recepções de Carga por NF

            CALL METHOD OBJ_ALV_0100->SET_TABLE_FOR_FIRST_DISPLAY
              EXPORTING
                IS_LAYOUT            = GS_LAYOUT
                I_SAVE               = 'A'
                IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
                IS_VARIANT           = GS_VARIANT
              CHANGING
                IT_FIELDCATALOG      = IT_FCAT
                IT_OUTTAB            = IT_SAIDA_0100_02.

          WHEN P_RC_DUE. "Recepções de Carga por DU-e
          WHEN P_EC_DUE. "Entregas de Carga por DU-e
        ENDCASE.

      WHEN P_TP_ECG. "Entregar Carga

        CASE ABAP_TRUE.
          WHEN P_VW_NF. "Notas Fiscais
          WHEN P_VW_DUE. "DU-e's

            CALL METHOD OBJ_ALV_0100->SET_TABLE_FOR_FIRST_DISPLAY
              EXPORTING
                IS_LAYOUT            = GS_LAYOUT
                I_SAVE               = 'A'
                IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
                IS_VARIANT           = GS_VARIANT
              CHANGING
                IT_FIELDCATALOG      = IT_FCAT
                IT_OUTTAB            = IT_SAIDA_0100_03.

          WHEN P_RC_NF. "Recepções de Carga por NF
          WHEN P_RC_DUE. "Recepções de Carga por DU-e
          WHEN P_EC_DUE. "Entregas de Carga por DU-e

            CALL METHOD OBJ_ALV_0100->SET_TABLE_FOR_FIRST_DISPLAY
              EXPORTING
                IS_LAYOUT            = GS_LAYOUT
                I_SAVE               = 'A'
                IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
                IS_VARIANT           = GS_VARIANT
              CHANGING
                IT_FIELDCATALOG      = IT_FCAT
                IT_OUTTAB            = IT_SAIDA_0100_04.


        ENDCASE.

    ENDCASE.


    CALL METHOD OBJ_ALV_0100->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD OBJ_ALV_0100->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.
    CALL METHOD OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
       EXPORTING
         IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.

  CASE ABAP_TRUE.
    WHEN P_TP_RCC. "Recepcionar Carga
      CASE ABAP_TRUE.
        WHEN P_VW_NF. "Notas Fiscais
          SET TITLEBAR 'T0100_RCC_01'.
        WHEN P_VW_DUE. "DU-e's
          SET TITLEBAR 'T0100_RCC_02'.
        WHEN P_RC_NF. "Recepções de Carga por NF
          SET TITLEBAR 'T0100_RCC_03'.
        WHEN P_RC_DUE. "Recepções de Carga por DU-e
          SET TITLEBAR 'T0100_RCC_04'.
      ENDCASE.

    WHEN P_TP_ECG. "Entregar Carga

      CASE ABAP_TRUE.
        WHEN P_VW_NF. "Notas Fiscais
        WHEN P_VW_DUE. "DU-e's
          SET TITLEBAR 'T0100_ECG_02'.
        WHEN P_RC_NF.  "Recepções de Carga por NF
        WHEN P_RC_DUE. "Recepções de Carga por DU-e
        WHEN P_EC_DUE. "Entregas de Carga por DU-e
          SET TITLEBAR 'T0100_ECG_03'.
      ENDCASE.

  ENDCASE.


ENDMODULE.

MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'T0101'.
ENDMODULE.

MODULE STATUS_0102 OUTPUT.
  SET PF-STATUS 'PF0102'.
  SET TITLEBAR 'T0102'.
ENDMODULE.
