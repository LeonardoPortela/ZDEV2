*&---------------------------------------------------------------------*
*&  Include           ZMMR106_PAI_PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TI0100'.

  PERFORM MONTAR_ALV.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: OB_SELECT TYPE REF TO ZCL_RELATORIO_CCTM,
        DIRECAO   TYPE C VALUE 'C'.

  CREATE OBJECT OB_SELECT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  IF SY-DYNNR EQ '0100'.
    CASE SY-UCOMM.
      WHEN 'ENTER'.
        OB_SELECT->M_SEL_DADOS( ).
*            OB_SELECT->M_SEL_DADOS_( ).
        OB_SELECT->M_AGRUPA_( ).
*            OB_SELECT->M_AGRUPA( ).
        CLEAR SC_TELA.
        MOVE SC_ALV TO SC_TELA.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETO OUTPUT.

  DATA:
    OBG_TOOLBAR TYPE REF TO ZCL_ALV_TOOLBAR,
    TL_FUNCTION TYPE UI_FUNCTIONS,
    WL_FUNCTION LIKE TL_FUNCTION  WITH HEADER LINE.

  CREATE OBJECT OB_SELECT.

*  OB_SELECT->M_SELECTS( ).
  OB_SELECT->M_LAYOUT( ).

  IF WA_CONT IS INITIAL.

    CREATE OBJECT WA_CONT
      EXPORTING
        CONTAINER_NAME = 'T_PRINCIPAL'.

    CREATE OBJECT WA_ALV
      EXPORTING
        I_PARENT = WA_CONT.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = WA_ALV.

    SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR      FOR WA_ALV,
                 OBG_TOOLBAR->HANDLE_ESTORNO  FOR WA_ALV,
                 OBG_TOOLBAR->ON_DOUBLE_CLICK FOR WA_ALV,
                 OBG_TOOLBAR->ON_CLICK        FOR WA_ALV.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
        IS_VARIANT                    = WA_VARIANTE
        IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        I_SAVE                        = 'X'
        I_DEFAULT                     = 'X'
*       I_SAVE                        = 'X'
*       I_STRUCTURE_NAME              = 'SFLIGHT'
      CHANGING
        IT_OUTTAB                     = IT_SAIDA[]
*       IT_OUTTAB                     = IT_SS
        IT_FIELDCATALOG               = IT_FCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC NE 0 .
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETO  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_ALV.

  PERFORM MONTAR_ESTRUTURA USING:

      'EMPRESA'    'Empresa'       '05'  ' '  ' ',"  '',
      'PEDIDO'     'Pedido'        '12'  'X'  ' ',"  '',
      'BSART'      'Tipo'          '06'  ' '  ' ',"  '',
      'NR_MIGO'    'Nr. MIGO'      '12'  'X'  ' ',"  '',
      'DT_MIGO'    'Dt. MIGO'      '10'  ' '  ' ',"  '',
      'TO_MIGO'    'Total MIGO'    '20'  ''   'X',"  '',
      'NR_MIRO'    'Nr. MIRO'      '12'  'X'  ' ',"  '',
      'DT_MIRO'    'Dt. MIRO'      '10'  ' '  ' ',"  '',
      'GSBER'      'Divisão'       '10'  ' '  ' ',"  '',
      'TO_MIRO'    'Total MIRO'    '20'  ' '  'X',"  '',
      'DIFERENCA'  'Diferença'     '20'  ' '  'X',"  ''.
      'ID_PROCESS' 'WorkFlow SE'   '50'  ' '  ' '."  ''.

ENDFORM.                    " MONTAR_ALV


*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_OPT      text
*      -->P_SUM      text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING   P_CAMPO TYPE C
                              P_DESC  TYPE C
                              P_TAM   TYPE C
                              P_HOT   TYPE C
*                              P_OPT   TYPE C
                              P_SUM   TYPE C
                   .
  WA_FCAT-FIELDNAME = P_CAMPO.
  WA_FCAT-SCRTEXT_S = P_DESC.
  WA_FCAT-SCRTEXT_M = P_DESC.
  WA_FCAT-SCRTEXT_L = P_DESC.
  WA_FCAT-OUTPUTLEN = P_TAM.
  WA_FCAT-HOTSPOT   = P_HOT.
*  WA_FCAT-COL_OPT   = P_OPT.
  WA_FCAT-DO_SUM    = P_SUM.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    "MONTAR_POPUP

FORM BATCH_INPUT  USING     VALUE(P_FLAG)
                            VALUE(P_FNAM)
                            VALUE(P_FVAL).

  CLEAR IT_BDC.

  IF NOT P_FLAG IS INITIAL.
    IT_BDC-PROGRAM  = P_FNAM.
    IT_BDC-DYNPRO   = P_FVAL.
    IT_BDC-DYNBEGIN = 'X'.
  ELSE.
    IT_BDC-FNAM = P_FNAM.
    IT_BDC-FVAL = P_FVAL.
  ENDIF.

  APPEND IT_BDC.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0104 OUTPUT.
  SET PF-STATUS 'S0104'.
  SET TITLEBAR 'T0104'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0104 INPUT.

  CASE SY-UCOMM.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
