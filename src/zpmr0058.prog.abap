*&---------------------------------------------------------------------*
*& Report  ZPMR0058
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&                 AMAGGI
*&---------------------------------------------------------------------*
*& ABAP:  Anderson Oenning ( AO ) - Amaggi
*& Data      : 25/10/2019
*
*& Chamado/Descrição : CS2019001167 - Relação de lista de tarefas por centro de planejamento.
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
*&
*&--------------------------------------------------------------------
REPORT ZPMR0058.

**Definições de tabelas.
TABLES: PLKO, PLAS, PLPO.

***Declaração tabelas.
DATA: GT_PLKO TYPE TABLE OF PLKO.

DATA: CLICKS TYPE SY-TABIX.

**Declaração estutura ALV.
DATA: OBJ_ALV_0110    TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CUSTOM_0110 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_LAYOUT       TYPE LVC_S_LAYO,
      WA_STABLE       TYPE LVC_S_STBL,
      GT_EXC_BUTTON   TYPE UI_FUNCTIONS,
      IT_FCAT         TYPE TABLE OF LVC_S_FCAT.



*---------------------------------------------------------------------*
*       CLASS lcl_events_handler DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_EVENTS_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS
      HANDLE_DOUBLE_CLICK
                  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.
ENDCLASS.                    "lcl_events_handler DEFINITION


*---------------------------------------------------------------------*
*       CLASS lcl_events_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENTS_HANDLER IMPLEMENTATION.
*---Metodo para double_click.

  METHOD HANDLE_DOUBLE_CLICK.

    CHECK E_ROW-ROWTYPE(1) EQ SPACE.
    PERFORM SEL_LISTA  USING E_ROW E_COLUMN-FIELDNAME.

  ENDMETHOD. " HANDLE_DOUBLE_CLICK
ENDCLASS.                    "lcl_events_handler IMPLEMENTATION



***Definições parametros pesquisa.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_IWERK FOR PLKO-IWERK OBLIGATORY,
                P_PLNNR FOR PLKO-PLNNR,
                P_PLNAL FOR PLKO-PLNAL.
SELECTION-SCREEN END OF BLOCK B1.



***Estrutura do relatório.
* ( PLNRR )  (PLNAL)   (KTEXT)       (IWERK)
*GrupListTar | Ngr | TxBrev roteiro | Centro |
*            |     |                |        |
*            |     |                |        |
*            |     |                |        |
*            |     |                |        |
*            |     |                |        |
*            |     |                |        |
*            |     |                |        |
*            |     |                |        |


START-OF-SELECTION.

  PERFORM SELEC_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELEC_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELEC_DADOS .

**  Seleção lista de tarefas.
  SELECT *
  FROM PLKO
  INTO TABLE GT_PLKO
    WHERE PLNNR IN P_PLNNR
      AND PLNAL IN P_PLNAL
      AND IWERK IN P_IWERK.

  IF GT_PLKO IS INITIAL.
    MESSAGE TEXT-002 TYPE 'E' DISPLAY LIKE 'I'.
  ELSE.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  SET TITLEBAR 'TIT0100'.

  REFRESH IT_FCAT.
  PERFORM ALV_PREENCHE_CAT USING:
   'PLNNR ' 'GrupListTar      ' '10' ''  ''  ''  '' ' '  '' '' '' ' ',
   'PLNAL ' 'Ngr              ' '06' ''  ''  ''  '' ' '  '' '' '' ' ',
   'KTEXT ' 'TxBrev roteiro   ' '40' ''  ''  ''  '' ' '  '' '' '' ' ',
   'IWERK ' 'Centro           ' '06' ''  ''  ''  '' ' '  '' '' '' ' '.

  IF ( OBJ_CUSTOM_0110 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0110
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_0110
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0110
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
  ENDIF.

  SET HANDLER: LCL_EVENTS_HANDLER=>HANDLE_DOUBLE_CLICK FOR OBJ_ALV_0110.

  CALL METHOD OBJ_ALV_0110->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IT_TOOLBAR_EXCLUDING          = GT_EXC_BUTTON
      I_SAVE                        = 'A'
    CHANGING
      IT_FIELDCATALOG               = IT_FCAT
      IT_OUTTAB                     = GT_PLKO
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0123   text
*      -->P_0124   text
*      -->P_0125   text
*      -->P_0126   text
*      -->P_0127   text
*      -->P_0128   text
*      -->P_0129   text
*      -->P_0130   text
*      -->P_0131   text
*      -->P_0132   text
*      -->P_0133   text
*      -->P_0134   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING: P_CAMPO         TYPE C
                                P_DESC          TYPE C
                                P_TAM           TYPE C
                                P_HOT           TYPE C
                                P_ZERO          TYPE C
                                P_SUM           TYPE C
                                P_EDIT          TYPE C
                                P_CHECK         TYPE C
                                P_REF_TABNAME   LIKE DD02D-TABNAME
                                P_REF_FIELDNAME LIKE DD03D-FIELDNAME
                                P_TABNAME       LIKE DD02D-TABNAME
                                P_NO_OUT        TYPE C.

  DATA: WL_FCAT TYPE LVC_S_FCAT.
  CLEAR: WA_LAYOUT, WL_FCAT.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-EDIT      = P_EDIT.
  WL_FCAT-CHECKBOX  = P_CHECK.
  WL_FCAT-REF_TABLE = P_REF_TABNAME.
  WL_FCAT-REF_FIELD = P_REF_FIELDNAME.
  WL_FCAT-TABNAME   = P_REF_TABNAME.
  WL_FCAT-NO_OUT    = P_NO_OUT.
  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_LISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM SEL_LISTA  USING    P_ROW P_COLUMN_FIELDNAME.

  FREE CLICKS.
  ADD 1 TO CLICKS.
  TRY .
      DATA(WA_LIST) = GT_PLKO[ P_ROW ].
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
  ENDTRY.

  CASE P_COLUMN_FIELDNAME.
    WHEN 'PLNNR'.
      SET PARAMETER ID 'PLN' FIELD WA_LIST-PLNNR.
      SET PARAMETER ID 'PAL' FIELD WA_LIST-PLNAL.
      SET PARAMETER ID 'WRK' FIELD WA_LIST-IWERK.
      CALL TRANSACTION 'IA06' AND SKIP FIRST SCREEN.

  ENDCASE.

ENDFORM.
