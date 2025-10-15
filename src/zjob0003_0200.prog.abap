*----------------------------------------------------------------------*
***INCLUDE ZJOB0003_0200.
*----------------------------------------------------------------------*

DATA: CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR       TYPE REF TO CL_GUI_TEXTEDIT,
      CK_READ_ONLY TYPE CHAR01,
      LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB,
      WL_HEADER    TYPE THEAD.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  CLEAR: TL_TLINES[], WL_HEADER.
  FREE:  TL_TLINES[].

  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TL0200'.

  IF ( EDITOR IS INITIAL ).

    CREATE OBJECT CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'LONGTEXT'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CHECK SY-SUBRC IS INITIAL.

    CREATE OBJECT EDITOR
      EXPORTING
        PARENT                 = CONTAINER
        WORDWRAP_MODE          = '2'
        WORDWRAP_POSITION      = '72'
      EXCEPTIONS
        ERROR_CNTL_CREATE      = 1
        ERROR_CNTL_INIT        = 2
        ERROR_CNTL_LINK        = 3
        ERROR_DP_CREATE        = 4
        GUI_TYPE_NOT_SUPPORTED = 5
        OTHERS                 = 6.

    CHECK SY-SUBRC IS INITIAL.

    WL_HEADER-TDOBJECT = 'ZTEXTOJOB'.
    WL_HEADER-TDID     = 'JCAN'.
    WL_HEADER-TDSPRAS  = SY-LANGU.
    CONCATENATE ZDE_ZJOB0002_ALV-JOBNAME ZDE_ZJOB0002_ALV-JOBCOUNT INTO WL_HEADER-TDNAME.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = WL_HEADER-TDID
        LANGUAGE                = WL_HEADER-TDSPRAS
        NAME                    = WL_HEADER-TDNAME
        OBJECT                  = WL_HEADER-TDOBJECT
      TABLES
        LINES                   = TL_TLINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    CASE SY-SUBRC.
      WHEN 0.
        CK_READ_ONLY = ABAP_TRUE.
      WHEN 1.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      WHEN 2.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      WHEN 3.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      WHEN 4.
        CK_READ_ONLY = ABAP_FALSE.
      WHEN 5.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      WHEN 6.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      WHEN 7.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      WHEN 8.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDCASE.

    IF CK_READ_ONLY EQ ABAP_TRUE.

      CALL METHOD EDITOR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR->TRUE.

      CLEAR: LONGTEXT_TAB.

      LOOP AT TL_TLINES.
        APPEND TL_TLINES-TDLINE TO LONGTEXT_TAB.
      ENDLOOP.

      CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE           = LONGTEXT_TAB
        EXCEPTIONS
          ERROR_DP        = 1
          ERROR_DP_CREATE = 2
          OTHERS          = 3.
    ELSE.
      CALL METHOD EDITOR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR->FALSE.
    ENDIF.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200_EXIT INPUT.
  PERFORM LIMPAR_TELA.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE OK_CODE.
    WHEN OK_CONFIRMAR.

      CLEAR: TL_TLINES[].

      IF EDITOR IS NOT INITIAL.
        CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
          IMPORTING
            TABLE           = LONGTEXT_TAB
          EXCEPTIONS
            ERROR_DP        = 1
            ERROR_DP_CREATE = 2
            OTHERS          = 3.
        LOOP AT LONGTEXT_TAB INTO DATA(WA_LINE).
          TL_TLINES-TDLINE = WA_LINE.
          APPEND TL_TLINES.
        ENDLOOP.
      ENDIF.
      "BREAK-POINT.

      IF ( TL_TLINES[] IS NOT INITIAL ).
        WL_HEADER-TDOBJECT = 'ZTEXTOJOB'.
        WL_HEADER-TDID     = 'JCAN'.
        WL_HEADER-TDSPRAS  = SY-LANGU.

        IF VG_ACAO EQ 'JUSTIFICAR'.
          CLEAR WA_JUSTIFICA.

          LOOP AT IT_JUSTIFICA INTO WA_JUSTIFICA.
            MOVE-CORRESPONDING WA_JUSTIFICA TO ZDE_ZJOB0002_ALV.

            CONCATENATE ZDE_ZJOB0002_ALV-JOBNAME ZDE_ZJOB0002_ALV-JOBCOUNT INTO WL_HEADER-TDNAME.

            CALL FUNCTION 'SAVE_TEXT'
              EXPORTING
                HEADER          = WL_HEADER
                INSERT          = ABAP_TRUE
                SAVEMODE_DIRECT = ABAP_TRUE
              TABLES
                LINES           = TL_TLINES
              EXCEPTIONS
                ID              = 1
                LANGUAGE        = 2
                NAME            = 3
                OBJECT          = 4
                OTHERS          = 5.

          ENDLOOP.

          CK_CONFIRMADO = ABAP_TRUE.
          PERFORM LIMPAR_TELA.
          LEAVE TO SCREEN 0.

        ELSE.

          CONCATENATE ZDE_ZJOB0002_ALV-JOBNAME ZDE_ZJOB0002_ALV-JOBCOUNT INTO WL_HEADER-TDNAME.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              HEADER          = WL_HEADER
              INSERT          = ABAP_TRUE
              SAVEMODE_DIRECT = ABAP_TRUE
            TABLES
              LINES           = TL_TLINES
            EXCEPTIONS
              ID              = 1
              LANGUAGE        = 2
              NAME            = 3
              OBJECT          = 4
              OTHERS          = 5.

          CK_CONFIRMADO = ABAP_TRUE.
          PERFORM LIMPAR_TELA.
          LEAVE TO SCREEN 0.

        ENDIF.

      ENDIF.

      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA .
  IF EDITOR IS NOT INITIAL.
    EDITOR->FREE( ).
  ENDIF.
  CLEAR: EDITOR.

  IF CONTAINER IS NOT INITIAL.
    CONTAINER->FREE( ).
  ENDIF.
  CLEAR: CONTAINER.

ENDFORM.
