*----------------------------------------------------------------------*
***INCLUDE LZGF_DESTINACAO_MERCADORIAF02.
*----------------------------------------------------------------------*

DATA: EDITOR       TYPE REF TO CL_GUI_TEXTEDIT,
      CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      LONGTEXT_TAB TYPE ZDE_TDLINE_T,
      LONGTEXT     TYPE ZDE_TDLINE.

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_TLINES  text
*----------------------------------------------------------------------*
FORM VISUALIZAR_TEXTO.

  CALL SCREEN 0101 STARTING AT 5 5.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.

  PERFORM LIMPAR_TELA_0101.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.

  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'TL0101' WITH GB_TEXTO_OBS.

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
        WORDWRAP_POSITION      = '132'
      EXCEPTIONS
        ERROR_CNTL_CREATE      = 1
        ERROR_CNTL_INIT        = 2
        ERROR_CNTL_LINK        = 3
        ERROR_DP_CREATE        = 4
        GUI_TYPE_NOT_SUPPORTED = 5
        OTHERS                 = 6.

    CHECK SY-SUBRC IS INITIAL.

    OB_DESTINACAO->GET_REGISTRO( IMPORTING E_ZMMT0114 = DATA(E_ZMMT0114) ).

    DATA(LC_DOCNUM_) = COND STRING( WHEN E_ZMMT0114-DOCNUM IS NOT INITIAL THEN E_ZMMT0114-DOCNUM ELSE E_ZMMT0114-DOCNUM_DEV ).

    TRY .

        ZCL_NFE=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = CONV #( LC_DOCNUM_ )
          )->SET_REGISTRO( EXPORTING I_DOCNUM = CONV #( LC_DOCNUM_ ) I_SEM_BLOQUEIO = ABAP_TRUE
          )->GET_REGISTRO( IMPORTING E_INFO_DOC_ELETRONICO = DATA(E_INFO_DOC_ELETRONICO)
          )->GET_CK_AUTORIZADO_USO(
          ).

        DATA(READONLY_MODE) = EDITOR->TRUE.

      CATCH ZCX_DOC_ELETRONICO.    "

        IF E_INFO_DOC_ELETRONICO-CANCEL EQ ABAP_TRUE.
          READONLY_MODE = EDITOR->TRUE.
        ELSE.
          READONLY_MODE = EDITOR->FALSE.
        ENDIF.

    ENDTRY.

    CALL METHOD EDITOR->SET_READONLY_MODE
      EXPORTING
        READONLY_MODE = READONLY_MODE.

    CLEAR: LONGTEXT_TAB.

    LOOP AT TL_TLINES.
      LONGTEXT = TL_TLINES-TDLINE.
      APPEND LONGTEXT TO LONGTEXT_TAB.
    ENDLOOP.

    CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
      EXPORTING
        TABLE           = LONGTEXT_TAB
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  CASE OK_CODE.
    WHEN 'CONFIRMAR'.

      CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
        IMPORTING
          TABLE           = LONGTEXT_TAB
        EXCEPTIONS
          ERROR_DP        = 1
          ERROR_DP_CREATE = 2
          OTHERS          = 3.

      DESCRIBE TABLE LONGTEXT_TAB LINES DATA(QTD_LINHAS).

      IF QTD_LINHAS IS INITIAL.

        CALL FUNCTION 'DELETE_TEXT'
          EXPORTING
            ID              = I_TDID
            LANGUAGE        = SY-LANGU
            NAME            = LC_NAME
            OBJECT          = LC_OBJECT
            SAVEMODE_DIRECT = ABAP_TRUE
          EXCEPTIONS
            NOT_FOUND       = 1
            OTHERS          = 2.

        "015  Texto da Nota Fiscal Foi Removido!
        "016  Texto da Nota Fiscal Foi Saldo!

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          MESSAGE S015.
          PERFORM LIMPAR_TELA_0101.
          LEAVE TO SCREEN 0.
        ENDIF.

      ELSE.

        CLEAR: TL_TLINES[].

        LOOP AT LONGTEXT_TAB INTO DATA(WA_LINE).
          TL_TLINES-TDLINE = WA_LINE.
          APPEND TL_TLINES.
        ENDLOOP.

        DATA: WL_HEADER TYPE THEAD.
        WL_HEADER-TDOBJECT = LC_OBJECT.
        WL_HEADER-TDID     = I_TDID.
        WL_HEADER-TDSPRAS  = SY-LANGU.
        WL_HEADER-TDNAME   = LC_NAME.

        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
            HEADER          = WL_HEADER
            SAVEMODE_DIRECT = ABAP_TRUE
          TABLES
            LINES           = TL_TLINES
          EXCEPTIONS
            ID              = 1
            LANGUAGE        = 2
            NAME            = 3
            OBJECT          = 4
            OTHERS          = 5.

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          MESSAGE S016.
        ENDIF.

        PERFORM LIMPAR_TELA_0101.
        LEAVE TO SCREEN 0.

      ENDIF.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA_0101 .

  IF EDITOR IS NOT INITIAL.
    EDITOR->FREE( ).
  ENDIF.
  CLEAR: EDITOR.

  IF CONTAINER IS NOT INITIAL.
    CONTAINER->FREE( ).
  ENDIF.
  CLEAR: CONTAINER.

ENDFORM.

FORM RETORNA_TEXTO USING P_DOCNUM TYPE J_1BDOCNUM CHANGING P_TEXTO TYPE STRING.

  TRY .
      P_TEXTO = ZCL_MATERIAL_DESTINACAO=>ZIF_MATERIAL_DESTINACAO~GET_TEXTO_NOTA_FISCAL_DOCNUM( I_DOCNUM = P_DOCNUM ).
    CATCH ZCX_MATERIAL_DESTINACAO.    "
  ENDTRY.

ENDFORM.
