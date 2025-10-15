*----------------------------------------------------------------------*
***INCLUDE ZFIMU04_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.
  SET PF-STATUS '0100' EXCLUDING FCODE.
  SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.

  IF WG_ACAO IS INITIAL.
    " Usuários que podem implantar saldo
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS         = '0000'
        SETNR         = 'ZFI0083_IMPLANT'
      TABLES
        SET_VALUES    = T_USERMD
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    SORT T_USERMD BY FROM.
    READ TABLE T_USERMD WITH KEY FROM = SY-UNAME.
    VMUDAR = 'S'.
    IF SY-SUBRC NE 0.
      VMUDAR = 'N'.
    ENDIF.

    WG_ACAO = C_DISPLA.
    REFRESH: TG_FIELDS.
    PERFORM F_TRATA_CAMPOS USING  SPACE
                              'GR1'
                              C_0       "INPUT 1     NO INPUT 0
                              C_0.      "INVISIBLE 1 VISIBLE 0

    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR4'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0

    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR3'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0



  ENDIF.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF    SCREEN-NAME   EQ TG_FIELDS-CAMPO
        OR  SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.

        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDMODULE.
