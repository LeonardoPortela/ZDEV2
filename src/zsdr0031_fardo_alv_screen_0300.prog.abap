*&---------------------------------------------------------------------*
*&  Include   ZSDR0031_FARDO_ALV_SCREEN_0300
*&---------------------------------------------------------------------*
*&TITULO  : Include para REL. VENDA ALGODÃO (ZSDR0031)
*&AUTOR   : Sara Trincha Oikawa
*&DATA.   : 12.05.2020
*&OBJETIVO: CS2018001961
*           O Objetivo deste novo relátório é exibir e comparar a qtde
*           de fardos e o peso que foram formados lote com o que foi
*           exportado. E nos casos onde há diferença exibir de forma
*           clara qual a instrução foi "formado o lote" e posteriormente
*           quando exportado, em qual instrução foi realizada a exportação.
*----------------------------------------------------------------------*
* INCLUDE  ZSDR0031_FARDO_ALV_SCREEN_0300
*----------------------------------------------------------------------*
* Description / Include functions
************************************************************************
* This include contains PBO and PAI events for the screen of report
************************************************************************

* DATA FOR TABSTRIP 'MAIN_TAB'
  CONTROLS: MAIN_TAB TYPE TABSTRIP.
  DATA: BEGIN OF I_MAIN_TAB,
          SUBSCREEN   LIKE SY-DYNNR,
          PROG        LIKE SY-REPID VALUE  'ZSDR0031',
          PRESSED_TAB LIKE SY-UCOMM VALUE  C_MAIN_TAB-TAB1,
        END OF I_MAIN_TAB.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
  MODULE STATUS_0300 OUTPUT.

    IF CUSTOM_CONTAINER1 IS INITIAL.
      SET PF-STATUS 'STATUS_0300'.
      SET TITLEBAR  'TIT_0300'.

* Creating Object
      PERFORM F_FARDO_OBJECTS_CREATE.
* Building the field catalog  (TAB1)
      PERFORM F_FARDO_FIELDCATALOG TABLES I_FIELDCAT USING '1'.
* Modifying the field catalog
*   PERFORM f_modify_field_cat. " TABLES i_fieldcat.
* Building the field catalog  (TAB2)
      PERFORM F_FARDO_FIELDCATALOG TABLES I_FIELDCAT1 USING '2'.
* Modifying the field catalog
*   PERFORM f_modify_field_cat1. " TABLES i_fieldcat.

* For Layout
      PERFORM F_FARDO_LAYOUT USING SY-TITLE 'X' 'B' 'X'.
* For Sort
      PERFORM F_FARDO_SORT   TABLES I_SORT.
    ENDIF.
  ENDMODULE. " STATUS_0300 OUTPUT

*&---------------------------------------------------------------------*
*& Module MAIN_TAB_ACTIVE_TAB_SET OUTPUT
*&---------------------------------------------------------------------*
* Call method to display in the output grid
*----------------------------------------------------------------------*
  MODULE MAIN_TAB_ACTIVE_TAB_SET OUTPUT.

    MAIN_TAB-ACTIVETAB = I_MAIN_TAB-PRESSED_TAB.
    CASE I_MAIN_TAB-PRESSED_TAB.
      WHEN C_MAIN_TAB-TAB1.
* To display report TAB1
        I_MAIN_TAB-SUBSCREEN = '0310'.
        CALL METHOD O_ALVGRID1->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING
            IS_VARIANT                    = W_VARIANT
            I_SAVE                        = C_LAY
            IS_LAYOUT                     = W_LAYOUT
          CHANGING
            IT_OUTTAB                     = IT_FARDO1[]
            IT_FIELDCATALOG               = I_FIELDCAT[]
            IT_SORT                       = I_SORT[]
          EXCEPTIONS
            INVALID_PARAMETER_COMBINATION = 1
            PROGRAM_ERROR                 = 2
            TOO_MANY_LINES                = 3
            OTHERS                        = 4.

        IF SY-SUBRC <> 0.
          MESSAGE 'Erro ALV report display'  TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      WHEN C_MAIN_TAB-TAB2.
* To display report TAB2
        I_MAIN_TAB-SUBSCREEN = '0320'.
        CALL METHOD O_ALVGRID2->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING
            IS_VARIANT                    = W_VARIANT
            I_SAVE                        = C_LAY
            IS_LAYOUT                     = W_LAYOUT
            I_BYPASSING_BUFFER            = 'X'
          CHANGING
            IT_OUTTAB                     = IT_FARDO2[]
            IT_FIELDCATALOG               = I_FIELDCAT1[]
            IT_SORT                       = I_SORT[]
          EXCEPTIONS
            INVALID_PARAMETER_COMBINATION = 1
            PROGRAM_ERROR                 = 2
            TOO_MANY_LINES                = 3
            OTHERS                        = 4.
        IF SY-SUBRC <> 0.
          MESSAGE 'Erro ALV report display' TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      WHEN OTHERS.
* DO NOTHING
    ENDCASE.
  ENDMODULE. "MAIN_TAB_ACTIVE_TAB_SET OUTPUT

*&---------------------------------------------------------------------*
*& Module MAIN_TAB_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
* Check & Process the selected Tab
*----------------------------------------------------------------------*
  MODULE MAIN_TAB_ACTIVE_TAB_GET INPUT.

    CASE SY-UCOMM.
      WHEN C_MAIN_TAB-TAB1.
        I_MAIN_TAB-PRESSED_TAB = C_MAIN_TAB-TAB1.
      WHEN C_MAIN_TAB-TAB2.
        I_MAIN_TAB-PRESSED_TAB = C_MAIN_TAB-TAB2.
      WHEN OTHERS.
* DO NOTHING
    ENDCASE.
  ENDMODULE. "MAIN_TAB_ACTIVE_TAB_GET INPUT

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0300 INPUT
*&---------------------------------------------------------------------*
* User Command
*----------------------------------------------------------------------*
  MODULE USER_COMMAND_0300 INPUT.

    CASE SY-UCOMM.
      WHEN 'BACK'.
        PERFORM F_FARDO_EXIT_PROGRAM.
        SET SCREEN '0'.
      WHEN 'EXIT' OR 'CANC'.
        PERFORM F_FARDO_EXIT_PROGRAM.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMODULE. "USER_COMMAND_9000 INPUT

*&---------------------------------------------------------------------*
*& Module MAIN_TAB_ACTIVE_TAB_SET INPUT
*&---------------------------------------------------------------------*
* Set sunscreen
*----------------------------------------------------------------------*
  MODULE MAIN_TAB_ACTIVE_TAB_SET INPUT.

    MAIN_TAB-ACTIVETAB = I_MAIN_TAB-PRESSED_TAB.
    CASE I_MAIN_TAB-PRESSED_TAB.
      WHEN C_MAIN_TAB-TAB1.
        I_MAIN_TAB-SUBSCREEN = '0310'.
      WHEN C_MAIN_TAB-TAB2.
        I_MAIN_TAB-SUBSCREEN = '0320'.
      WHEN OTHERS.
* DO NOTHING
    ENDCASE.
  ENDMODULE.  "MAIN_TAB_ACTIVE_TAB_SET INPUT
